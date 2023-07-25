/*
 * TLVC implementation of raw access to direct (non BIOS) hard disk devices.
 *
 * Helge Skrivervik 06/23
 */

/* 
 * /dev/rhd[a..c][0..4]
 * IO in blocks only (sector size, currently fixed at 512 b/s).
 * Physical IO is shared with the block driver.
 * Data are copied directly to/from user process space.
 */

#include <linuxmt/config.h>

#ifdef CONFIG_BLK_DEV_CHAR

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
#define ATDISK		/* required for blk.h definitions */
#include "../block/blk.h"
#define DEVICE_NAME "r" DEVICE_NAME

int directhd_open(struct inode *, struct file *);
int directhd_ioctl(struct inode *, struct file *, unsigned int, unsigned int);
void directhd_release(struct inode *, struct file *);
size_t block_wr(struct inode *, struct file *, char *, size_t);
size_t block_rd(struct inode *, struct file *, char *, size_t);

/*
 * The open routine can be shared with the block driver which only looks 
 * at/uses the minor device.
 */

int rhd_open(register struct inode *inode, struct file *filp)
{
    //unsigned int minor;

    //minor = MINOR(inode->i_rdev);
    //printk("rdh open\n");
    return(directhd_open(inode, filp));
}

void rhd_close(struct inode *inode, struct file *filp)
{
    //printk("rdh close\n");
    return(directhd_release(inode, filp));
}

/* FIXME change ops struct to point directly to the block driver entries when appropriate */
static struct file_operations rhd_fops = {
    NULL,			/* lseek */
    block_rd,			/* read */
    block_wr,			/* write */
    NULL,			/* readdir */
    NULL,			/* select */
    directhd_ioctl,		/* ioctl */
    rhd_open,			/* open */
    rhd_close			/* release */
};

/*@+type@*/

void rhd_init(void)
{
    /* Device initialization done by the block driver, nothing required */

    if (register_chrdev(RAW_HD_MAJOR, "rhd", &rhd_fops))
	printk("RHD: Unable to get major %d for raw disk devices\n", RAW_HD_MAJOR);
    printk("rhd: Raw access to block devices configured\n");
}

#endif
