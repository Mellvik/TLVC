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

#if 0
size_t rhd_read(struct inode *inode, struct file *filp,
	       char *data, size_t len)
{
	return(rhd_rw(inode, filp, data, len, READ));
}

size_t rhd_write(struct inode *inode, struct file *filp,
	       char *data, size_t len)
{
	return(rhd_rw(inode, filp, data, len, WRITE));
}

/*
 * Set up a new request for the raw IO transaction passing along the buffer pointer
 * and the data size from the calling process (adjusted to whole sectors).
 * return the req pointer so we can wait for it in the caller (via end_request()).
 */
struct request *do_raw_blkio(struct inode *inode, struct file *filp,
	       char *data, size_t len, int rw)
{
    struct request *req;
    clr_irq();
    /* May need ot use __get_request_wait() instead */
    req = get_request(NR_REQUEST -1, inode->i_rdev);
    set_irq();
    if (!req) {
	printk("RAW IO: Cannot get request\n");
	req->rq_error = -EIO;
	return NULL;
    }

    debug_raw("raw_blkio[%04x], len %l, pos %l req %x\n", inode->i_rdev, len, 
		filp->f_pos, req);
    //req->rq_dev = inode->i_rdev;
    req->rq_cmd = rw;
    req->rq_buffer = data;
    req->rq_seg = current->t_regs.ds;
    req->rq_bh = NULL;
    req->rq_next = NULL;
    req->rq_blocknr = filp->f_pos/BLOCK_SIZE; /* must use 1K blocksize even here
    					      * to get the phys sector calculation 
					      * at the lower level right */

    req->rq_nr_sectors = len/512;
    add_request(&blk_dev[MAJOR(req->rq_dev)], req);
    return req;
}
#endif

int rhd_ioctl(struct inode *inode, struct file *file, int cmd, char *arg)
{
    /* placeholder for now */
    return -EINVAL;
}


int rhd_open(register struct inode *inode, struct file *filp)
{
    //unsigned int minor;

    //minor = MINOR(inode->i_rdev);
    printk("rdh open\n");
    return(directhd_open(inode, filp));
}

void rhd_close(struct inode *inode, struct file *filp)
{
    printk("rdh close\n");
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

    printk("rhd: Raw access to block devices configured\n");
    if (register_chrdev(RAW_HD_MAJOR, "rhd", &rhd_fops))
	printk("RHD: Unable to get major %d for raw disk devices\n", RAW_HD_MAJOR);
}

#endif
