/*
 *  kernel/sched.c
 *  (C) 1995 Chad Page
 *
 *  This is the main scheduler - hopefully simpler than Linux's at present.
 */

#include <linuxmt/kernel.h>
#include <linuxmt/sched.h>
#include <linuxmt/init.h>
#include <linuxmt/timer.h>
#include <linuxmt/string.h>
#include <linuxmt/trace.h>
#include <linuxmt/debug.h>

#include <arch/irq.h>

#define idle_task task[0]

struct task_struct *task;           /* dynamically allocated task array */
struct task_struct *current;
struct task_struct *previous;
int max_tasks = MAX_TASKS;

extern int intr_count;

void add_to_runqueue(register struct task_struct *p)
{
    (p->prev_run = idle_task.prev_run)->next_run = p;
    p->next_run = &idle_task;
    idle_task.prev_run = p;
}

void del_from_runqueue(register struct task_struct *p)
{
#if CHECK_SCHED       /* sanity tests */
    if (!p->next_run || !p->prev_run) {
	printk("del_fr_runq: %d not found (%d)\n", p->pid, p->state);
	return;
    }
    if (p == &idle_task) {
	printk("idle task cannot sleep\n");
	return;
    }
#endif
    (p->next_run->prev_run = p->prev_run)->next_run = p->next_run;
    p->next_run = p->prev_run = NULL;

}

static void process_timeout(int __data)
{
    register struct task_struct *p = (struct task_struct *) __data;

    debug_sched("sched: timeout %d\n", p->pid);
    p->timeout = 0UL;
    wake_up_process(p);
}

/*
 *  Schedule a task. On entry current is the task, which will
 *  vanish quietly for a while and someone elses thread will return
 *  from here.
 */

void schedule(void)
{
    register struct task_struct *prev;
    register struct task_struct *next;
    struct timer_list timer;
    jiff_t timeout = 0UL;

    prev = current;

#ifdef CHECK_SCHED
    if (_gint_count > 1) {      /* neither user nor idle task was running */
    /* Taking a timer IRQ during another IRQ or while in kernel space is
     * quite legal. We just dont switch then */
	panic("schedule from int");
    }
#endif

    /* We have to let a task exit! */
    if (prev->state == TASK_EXITING)
	return;

    if (last_pid <= 0)	/* don't try to reschedule the idle_task [0] */
	return;

    clr_irq();
    if (prev->state == TASK_INTERRUPTIBLE) {
        if (prev->signal || (prev->timeout && (prev->timeout <= jiffies))) {
            prev->timeout = 0UL;
            prev->state = TASK_RUNNING;
        } else {
	    timeout = prev->timeout;
	}
    }

    /* Choose a task to run next */
    next = prev->next_run;
    if (prev->state != TASK_RUNNING)
	del_from_runqueue(prev);
    if (next == &idle_task)
        next = next->next_run;
    set_irq();

    if (next != prev) {

        if (timeout) {
            timer.tl_expires = timeout;
            timer.tl_data = (int) prev;
            timer.tl_function = process_timeout;
	    debug_sched("sched: addtimer %d\n", current->pid);
            add_timer(&timer);
        }

        previous = prev;
        current = next;
	debug_sched("sched: %d %d\n", prev->pid, next->pid);
        tswitch();  /* Won't return for a new task */

        if (timeout) {
	    debug_sched("sched: deltimer %d\n", current->pid);
            del_timer(&timer);
        }
    } else if (current->pid)
	debug_sched("resched: %d prevstate %d\n", current->pid, prev->state);
}

static struct timer_list *next_timer;

void add_timer(struct timer_list * timer)
{
    struct timer_list **p;
    flag_t flags;

    timer->tl_next = NULL;
    p = &next_timer;
    save_flags(flags);
    clr_irq();
    while (*p) {
        if ((*p)->tl_expires > timer->tl_expires) {
            timer->tl_next = *p;
            break;
        }
        p = &(*p)->tl_next;
    }
    *p = timer;
    restore_flags(flags);
}

int del_timer(struct timer_list * timer)
{
    struct timer_list **p;
    flag_t flags;
    int ret = 0;

    p = &next_timer;
    save_flags(flags);
    clr_irq();
    while (*p) {
        if (*p == timer) {
            *p = timer->tl_next;
            ret = 1;
	    break;
        }
        p = &(*p)->tl_next;
    }
    //if (timer == next_timer) *next_timer = NULL;
    restore_flags(flags);
    return ret;
}

static void run_timer_list(void)
{
    struct timer_list *timer;

    clr_irq();
    while ((timer = next_timer) && timer->tl_expires <= jiffies) {
        del_timer(timer);
        set_irq();
        timer->tl_function(timer->tl_data);
        clr_irq();
    }
    set_irq();
}

/* maybe someday I'll implement these profiling things -PL */

#if 0

static void do_it_prof(struct task_struct *p, jiff_t ticks)
{
    jiff_t it_prof = p->it_prof_value;

    if (it_prof) {
    if (it_prof <= ticks) {
        it_prof = ticks + p->it_prof_incr;
        send_sig(SIGPROF, p, 1);
    }
    p->it_prof_value = it_prof - ticks;
    }
}

static void update_one_process(struct taks_struct *p,
                   jiff_t ticks, jiff_t user, jiff_t system)
{
    do_process_times(p, user, system);
    do_it_virt(p, user);
    do_it_prof(p, ticks);
}

#endif

void do_timer(struct pt_regs *regs)
{
    jiffies++;

#ifdef NEED_RESCHED		/* need_resched is not checked anywhere */
    if (!((int) jiffies & 7))
	need_resched = 1;	/* how primitive can you get? */
#endif

    run_timer_list();

}

void INITPROC sched_init(void)
{
    register struct task_struct *t = &task[max_tasks];

/*
 *	Mark tasks 0-(max_tasks-1) as not in use.
 */
    do {
	(--t)->state = TASK_UNUSED;
    } while (t > task);

    current = task;
    next_task_slot = task;
    task_slots_unused = max_tasks;
/*
 *	Now create task 0 to be ourself.
 */
    kfork_proc(NULL);

    t->state = TASK_RUNNING;
    t->next_run = t->prev_run = t;
}

