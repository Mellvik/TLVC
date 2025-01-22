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
#include <linuxmt/sched.h>

int floppy_open(struct inode *, struct file *);
int fd_ioctl(struct inode *, struct file *, unsigned int, unsigned int);
void floppy_release(struct inode *, struct file *);
size_t block_wr(struct inode *, struct file *, char *, size_t);
size_t block_rd(struct inode *, struct file *, char *, size_t);

/*
 * Let the block device open function do whatever is required - it will
 * recognize char device access and act accordingly.
 * There is (currently) no keeping track of opens/closes when doing raw access.
 */

int rdf_open(register struct inode *inode, struct file *filp)
{
    //printk("rdf open\n");
    return(floppy_open(inode, filp));
}

void rdf_close(struct inode *inode, struct file *filp)
{
    //printk("rdf close\n");
    return(floppy_release(inode, filp));
}

static struct file_operations rdf_fops = {
    NULL,			/* lseek */
    block_rd,			/* read */
    block_wr,			/* write */
    NULL,			/* readdir */
    NULL,			/* select */
    fd_ioctl,		/* ioctl - FIXME: possibly useful when formatting */	
    rdf_open,			/* open */
    rdf_close			/* release */
};

/*@+type@*/

void INITPROC rdf_init(void)
{
    /* Device initialization done by the block driver, nothing required */

    if (register_chrdev(RAW_FD_MAJOR, "rdf", &rdf_fops))
	printk("rdf: Unable to get major %d for raw floppy devices\n", RAW_FD_MAJOR);
    //printk("rdf: Raw access to floppy devices configured\n");
}

#endif
