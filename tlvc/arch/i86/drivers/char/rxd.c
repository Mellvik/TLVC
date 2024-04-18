/*
 * Raw/char driver for the TLVC XD (MFM) disk driver - copy of the raw HD driver
 *
 * Helge Skrivervik 04/24
 */

/* 
 * /dev/rxd[a..c][0..4]
 * Physical IO via the block driver.
 * Data are copied directly to/from user process space.
 *
 * NOTE: The major device numbers must be the same for the 
 * block and corresponding raw device
 */

#include <linuxmt/config.h>

#ifdef CONFIG_BLK_DEV_XD

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

int xd_open(struct inode *, struct file *);
int xd_ioctl(struct inode *, struct file *, unsigned int, unsigned int);
void xd_release(struct inode *, struct file *);
size_t block_wr(struct inode *, struct file *, char *, size_t);
size_t block_rd(struct inode *, struct file *, char *, size_t);

/*
 * The open routine can be shared with the block driver which only looks 
 * at/uses the minor device.
 */

int rxd_open(register struct inode *inode, struct file *filp)
{
    //printk("rxd open\n");
    return(xd_open(inode, filp));
}

void rxd_close(struct inode *inode, struct file *filp)
{
    //printk("rxd close\n");
    //return(x_release(inode, filp));
    return; 	/* Nothing to do */
}

/* FIXME change ops struct to point directly to the block driver entries when appropriate */
static struct file_operations rxd_fops = {
    NULL,			/* lseek */
    block_rd,			/* read */
    block_wr,			/* write */
    NULL,			/* readdir */
    NULL,			/* select */
    xd_ioctl,			/* ioctl - FIXME: Useful or dangerous? */	
    rxd_open,			/* open */
    rxd_close			/* release */
};

/*@+type@*/

void INITPROC rxd_init(void)
{
    /* Device initialization done by the block driver, nothing required */
    if (register_chrdev(RAW_XD_MAJOR, "rxd", &rxd_fops))
	printk("RXD: Unable to get major %d for raw disk devices\n", RAW_XD_MAJOR);
    printk("rxd: Raw access to block devices configured\n");
}

#endif
