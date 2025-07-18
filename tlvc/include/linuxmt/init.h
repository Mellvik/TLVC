#ifndef __LINUXMT_INIT_H
#define __LINUXMT_INIT_H

/* Assorted initializers */

#include <linuxmt/config.h>
#include <linuxmt/types.h>

#if defined(CONFIG_FARTEXT_KERNEL) && !defined(__STRICT_ANSI__)
#define INITPROC __far __attribute__ ((far_section, noinline, section (".fartext.init")))
#define FARPROC  __far __attribute__ ((far_section, noinline, section (".fartext.far")))

/* these symbols defined in elks-small.ld linker script */
extern void INITPROC __start_fartext_init(void);
extern void INITPROC __end_fartext_init(void);
#else
#define INITPROC
#define FARPROC
#endif

struct task_struct;
struct gendisk;
struct drive_infot;

/* kernel init routines*/
extern int  INITPROC buffer_init(void);
extern void INITPROC console_init(void);
extern void INITPROC inode_init(void);
extern void INITPROC irq_init(void);
extern void save_timer_irq(void);
extern void INITPROC mm_init(seg_t,seg_t);
extern void INITPROC seg_add(seg_t, seg_t);
extern void INITPROC sched_init(void);
extern void INITPROC serial_init(void);
extern void INITPROC rs_setbaud(dev_t, unsigned long);
extern void INITPROC sock_init(void);
extern void INITPROC tty_init(void);

extern void INITPROC device_init(void);
extern void INITPROC setup_dev(register struct gendisk *);

extern void INITPROC tz_init(const char *tzstr);

/* block device init routines*/
struct file_operations;
extern int INITPROC register_blkdev(unsigned int,const char *,struct file_operations *);
extern void INITPROC blk_dev_init(void);
extern int  INITPROC bioshd_init(void);
extern int  INITPROC get_ide_data(int, struct drive_infot *);
extern int  INITPROC directhd_init(void);
extern void INITPROC floppy_init(void);
extern void INITPROC rd_init(void);
extern void INITPROC ssd_init(void);
extern void INITPROC romflash_init(void);
extern void INITPROC xd_init(void);

/* char device init routines*/
extern int INITPROC register_chrdev(unsigned int,const char *,struct file_operations *);
extern void INITPROC chr_dev_init(void);
extern void INITPROC cgatext_init(void);
extern void INITPROC eth_init(void);
extern void INITPROC ne2k_drv_init(void);
extern void INITPROC el3_drv_init(void);
extern void INITPROC wd_drv_init(void);
extern void INITPROC ee16_drv_init(void);
extern void INITPROC lp_init(void);
extern void INITPROC mem_dev_init(void);
extern void INITPROC meta_init(void);
extern void INITPROC pty_init(void);
extern void INITPROC tcpdev_init(void);
extern void INITPROC rhd_init(void);
extern void INITPROC rdf_init(void);
extern void INITPROC rxd_init(void);
extern void INITPROC rbioshd_init(void);

extern void kfork_proc(void (*addr)());
extern void arch_setup_user_stack(struct task_struct *, word_t entry);

#endif
