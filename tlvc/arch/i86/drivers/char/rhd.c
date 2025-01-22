/*
 * TLVC implementation of raw access to direct (IDE) hard disk devices.
 *
 * Helge Skrivervik 06/23
 */

/* 
 * /dev/rhd[a..c][0..4]
 * Physical IO via the block driver.
 * Data are copied directly to/from user process space.
 *
 * NOTE: The major device numbers must be the same for the 
 * block and corresponding raw device
 */

#include <linuxmt/config.h>

#ifdef CONFIG_BLK_DEV_HD

#include <linuxmt/kernel.h>
#include <linuxmt/sched.h>

#define	SECSIZ		512	/* Fixed sector size for now */

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
    //printk("rhd open\n");
    return(directhd_open(inode, filp));
}

void rhd_close(struct inode *inode, struct file *filp)
{
    //printk("rhd close\n");
    //return(directhd_release(inode, filp));
    return; 	/* Nothing to do */
}

/* FIXME change ops struct to point directly to the block driver entries when appropriate */
static struct file_operations rhd_fops = {
    NULL,			/* lseek */
    block_rd,			/* read */
    block_wr,			/* write */
    NULL,			/* readdir */
    NULL,			/* select */
    directhd_ioctl,		/* ioctl - FIXME: Useful or dangerous? */	
    rhd_open,			/* open */
    rhd_close			/* release */
};

/*@+type@*/

void INITPROC rhd_init(void)
{
    /* Device initialization done by the block driver, nothing required */

    if (register_chrdev(RAW_HD_MAJOR, "rhd", &rhd_fops))
	printk("RHD: Unable to get major %d for raw disk devices\n", RAW_HD_MAJOR);
    //printk("rhd: Raw access to ATHD block devices configured\n");
}

#endif
