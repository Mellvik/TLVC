/*
 * Raw/char access to BIOS hard/floppy drives
 *
 * Helge Skrivervik 01/25
 */

/* 
 * /dev/rbd[...], /dev/rfd0, /dev/rfd1
 * Physical IO via the block driver.
 * Data are copied directly to/from user process space.
 *
 * NOTE: The major device numbers must be the same for the 
 * block and corresponding raw device
 */

#include <linuxmt/config.h>

#ifdef CONFIG_BLK_DEV_BIOS

#include <linuxmt/kernel.h>
#include <linuxmt/sched.h>

int bioshd_open(struct inode *, struct file *);
int bioshd_ioctl(struct inode *, struct file *, unsigned int, unsigned int);
void bioshd_release(struct inode *, struct file *);
size_t block_wr(struct inode *, struct file *, char *, size_t);
size_t block_rd(struct inode *, struct file *, char *, size_t);

/*
 * Since the major device number is the same, the open and release routines
 * are shared with the block driver. The block driver will recognize type
 * type of device and act accordingly.
 */

int rbioshd_open(register struct inode *inode, struct file *filp)
{
    //printk("rbioshd open\n");
    return(bioshd_open(inode, filp));
}

void rbioshd_close(struct inode *inode, struct file *filp)
{
    //printk("rbioshd close\n");
    bioshd_release(inode, filp);
    return; 
}

/* FIXME change ops struct to point directly to the block driver entries when appropriate */
static struct file_operations rhd_fops = {
    NULL,			/* lseek */
    block_rd,			/* read */
    block_wr,			/* write */
    NULL,			/* readdir */
    NULL,			/* select */
    bioshd_ioctl,		/* ioctl */
    rbioshd_open,		/* open */
    rbioshd_close		/* release */
};

void INITPROC rbioshd_init(void)
{
    /* Device initialization done by the block driver, nothing required */

    if (register_chrdev(RAW_BIOS_MAJOR, "rbioshd", &rhd_fops))
	printk("RHD: Unable to get major %d for raw disk devices\n", RAW_HD_MAJOR);
    //printk("bioshd: Raw access configured\n");
}

#endif
