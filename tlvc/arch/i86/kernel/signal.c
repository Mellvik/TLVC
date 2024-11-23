/*
 *  linux/arch/i386/kernel/signal.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 *  Modified for ELKS 1998-1999 Al Riddoch
 */

#include <linuxmt/types.h>
#include <linuxmt/config.h>
#include <linuxmt/sched.h>
#include <linuxmt/mm.h>
#include <linuxmt/kernel.h>
#include <linuxmt/signal.h>
#include <linuxmt/errno.h>
#include <linuxmt/wait.h>
#include <linuxmt/debug.h>
#include <linuxmt/memory.h>

#include <arch/segment.h>

int do_signal(void)
{
    register __sigdisposition_t *sd;
    register __kern_sighandler_t sah;
    unsigned int signr;
    sigset_t mask;

    signr = 1;
    mask = (sigset_t)1;
    while (current->signal) {
	while(!(current->signal & mask)) {
	    signr++;
	    mask <<= 1;
	}
	current->signal ^= mask;

	debug_sig("SIGNAL process signal %d pid %d\n", signr, current->pid);
	sah = current->sig.handler;
	sd = &current->sig.action[signr - 1].sa_dispose;
	if (*sd == SIGDISP_DFL) {			/* Default */
	    if ((mask &					/* Default Ignore */
			(SM_SIGCONT | SM_SIGCHLD | SM_SIGWINCH | SM_SIGURG))
		|| (current->pid == 1 && signr != SIGKILL))
		continue;
	    else if (mask &				/* Default Stop */
			(SM_SIGSTOP | SM_SIGTSTP | SM_SIGTTIN | SM_SIGTTOU)) {
		debug_sig("SIGNAL pid %d stopped\n", current->pid);
		current->state = TASK_STOPPED;
		/* Let the parent know */
		current->exit_status = signr;
		schedule();
	    }
	    else {					/* Default Core or Terminate */
#if 0
		if (mask &				/* Default Core */
		      (SM_SIGQUIT|SM_SIGILL|SM_SIGABRT|SM_SIGFPE|SM_SIGSEGV|SM_SIGTRAP))
		    dump_core();
#endif
		debug_sig("SIGNAL terminating pid %d\n", current->pid);
		do_exit(signr);				/* Default Terminate */
	    }
	}
	else if (*sd != SIGDISP_IGN) {			/* Set handler */
	    debug_sig("SIGNAL setup return stack for handler %x:%x\n",
		      _FP_SEG(sah), _FP_OFF(sah));
	    //debug_sig("Stack at %x\n", current->t_regs.sp);
	    arch_setup_sighandler_stack(current, sah, signr);
	    //debug_sig("Stack at %x\n", current->t_regs.sp);
	    *sd = SIGDISP_DFL;
	    debug_sig("SIGNAL reset pending signals\n");
	    if (current->signal)
		printk("SIGNAL(%d) ignoring signal (mask=%s)\n",
		    current->pid, current->signal);
	    current->signal = 0;

	    return 1;
	}
	else /* else (*sd == SIGDISP_IGN) Ignore */
	    debug_sig("SIGNAL signal %d ignored pid %d\n", signr, current->pid);
    }
    return 0;
}

