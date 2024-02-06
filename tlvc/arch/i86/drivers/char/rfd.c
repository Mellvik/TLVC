/*
 * TLVC implementation of raw access to direct (non BIOS) floppy devices.
 *
 * Helge Skrivervik 10/23
 */

/* 
 * /dev/rdf[0..4]
 * IO in blocks only (sector size, currently fixed at 512 b/s).
 * Physical IO is shared with the block driver.
 * Data are copied directly to/from user process space.
 */

#include <linuxmt/config.h>

#ifdef CONFIG_BLK_DEV_FD

#include <linuxmt/kernel.h>
#include <linuxmt/major.h>
#include <linuxmt/fs.h>
#include <linuxmt/errno.h>
#include <linuxmt/mm.h>
#include <linuxmt/sched.h>
#include <linuxmt/debug.h>
#include <linuxmt/mem.h>
#include <linuxmt/heap.h>
#include <linuxmt/timer.h>
#include <linuxmt/init.h>

#include <arch/io.h>
#include <arch/segment.h>

#define	SECSIZ		512	/* Fixed sector size for now */
#define FLOPPY		/* required for blk.h definitions */
#include "../block/blk.h"
#define DEVICE_NAME "r" DEVICE_NAME

int floppy_open(struct inode *, struct file *);
int fd_ioctl(struct inode *, struct file *, unsigned int, unsigned int);
void floppy_release(struct inode *, struct file *);
size_t block_wr(struct inode *, struct file *, char *, size_t);
size_t block_rd(struct inode *, struct file *, char *, size_t);

/*
 * Let the blocl device open function do whatever is required - it will
 * recognize char device access and act accordingly.
 * There is no keeping track of opens/closes when doing raw access.
 */

int rfd_open(register struct inode *inode, struct file *filp)
{
    //unsigned int minor;

    //minor = MINOR(inode->i_rdev);
    //printk("rdf open\n");
    return(floppy_open(inode, filp));
}

void rfd_close(struct inode *inode, struct file *filp)
{
    //printk("rdf close\n");
    return(floppy_release(inode, filp));
}

/* FIXME change ops struct to point directly to the block driver entries when appropriate */
static struct file_operations rfd_fops = {
    NULL,			/* lseek */
    block_rd,			/* read */
    block_wr,			/* write */
    NULL,			/* readdir */
    NULL,			/* select */
    fd_ioctl,		/* ioctl - FIXME: possibly useful when formatting */	
    rfd_open,			/* open */
    rfd_close			/* release */
};

/*@+type@*/

void INITPROC rfd_init(void)
{
    /* Device initialization done by the block driver, nothing required */

    if (register_chrdev(RAW_FD_MAJOR, "rfd", &rfd_fops))
	printk("RFD: Unable to get major %d for raw floppy devices\n", RAW_FD_MAJOR);
    //printk("rfd: Raw access to floppy devices configured\n");
}

#endif
