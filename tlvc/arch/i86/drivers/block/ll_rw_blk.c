/*
 *  linux/drivers/block/ll_rw_blk.c
 *
 * Copyright (C) 1991, 1992 Linus Torvalds
 * Copyright (C) 1994,      Karl Keyte: Added support for disk statistics
 */

/*
 * This handles all read/write requests to block devices
 * and (since 2023) their raw/char relatives.
 */

#include <linuxmt/config.h>
#include <linuxmt/limits.h>
#include <linuxmt/types.h>
#include <linuxmt/sched.h>
#include <linuxmt/kernel.h>
#include <linuxmt/fs.h>
#include <linuxmt/errno.h>
#include <linuxmt/string.h>
#include <linuxmt/init.h>
#include <linuxmt/mm.h>
#include <linuxmt/debug.h>
#include <linuxmt/ioctl.h>

#include <arch/system.h>
#include <arch/io.h>
#include <arch/segment.h>
#include <arch/irq.h>

#if defined(CONFIG_BLK_DEV_FD) || defined(CONFIG_BLK_DEV_XD)
#define FIXED_SECTOR_SIZE 512
#define ASYNC_IO
struct wait_queue wait_for_request;
static struct request *__get_request_wait(int, kdev_t);
#endif
#include "blk.h"

/*
 * The request-struct contains all necessary data
 * to load a number of sectors into memory
 *
 * NR_REQUEST is the number of entries in the request-queue.
 * NOTE that writes may use only the low 2/3 of these: reads
 * take precedence.
 */

//#define NR_REQUEST	20 /* Moved to limits.h */

static struct request request_list[NR_REQUEST];

struct blk_dev_struct blk_dev[MAX_BLKDEV];	/* initialized by blk_dev_init() */

#ifdef BDEV_SIZE_CHK
int *blk_size[MAX_BLKDEV] = { NULL, NULL, };
#endif

/*
 * blksize_size contains the size of all block-devices:
 *
 * blksize_size[MAJOR]
 *
 */

/* int blksize_size[MAX_BLKDEV] = {0,}; */

/*
 * hardsect_size contains the size of the hardware sector of a device.
 *
 * hardsect_size[MAJOR][MINOR]
 *
 * if (!hardsect_size[MAJOR])
 *		then 512 bytes is assumed.
 * else
 *		sector_size is hardsect_size[MAJOR][MINOR]
 *
 * This is currently set by some scsi device and read by the msdos fs driver
 * This might be a some uses later.
 */

/* int * hardsect_size[MAX_BLKDEV] = { NULL, NULL, }; */

/*
 * look for a free request in the first N entries.
 * NOTE: interrupts must be disabled on the way in, and will still
 *       be disabled on the way out.
 */
struct request *get_request(int n, kdev_t dev)
{
    static struct request *prev_found = NULL;
    static struct request *prev_limit = NULL;
    register struct request *req;
    register struct request *limit;

    limit = request_list + n;
    if (limit != prev_limit) {
	prev_limit = limit;
	prev_found = request_list;
    }
    req = prev_found;
    do {
	req = ((req > request_list) ? req : limit) - 1;
	if (req->rq_status == RQ_INACTIVE) {
	    prev_found = req;
	    req->rq_status = RQ_ACTIVE;
	    req->rq_dev = dev;
	    req->rq_nr_sectors = 0;
	    return req;
	}
    } while (req != prev_found);
    return NULL;
}

#if 0
static struct request *get_request_wait(int n, kdev_t dev)
{
    register struct request *req;

    clr_irq();
    req = get_request(n, dev);
    set_irq();
    if (req)
	return req;
    return __get_request_wait(n, dev);
}
#endif

/*
 * add_request passes a request on to the low level driver if the device 
 * request queue is empty, otherwise adds a request to the linked list.
 * It disables interrupts so that it can muck with the
 * request-lists in peace.
 */

static void add_request(struct blk_dev_struct *dev, struct request *req)
{
    register struct request *tmp;

    clr_irq();
    //mark_buffer_clean(req->rq_bh);	/* EXPERIMENTAL: removed, too early to mark clean */
    if (!(tmp = dev->current_request)) {	/* if queue empty, process ... */
	dev->current_request = req;
	set_irq();
#if DEBUG_ASYNC
	if (debug_level > 2) printk("AD%04x|", req);
#endif
	(dev->request_fn) ();
    } else {				/* otherwise just add to queue */
	/* FIXME: remove this for solid state devices */
	for (; tmp->rq_next; tmp = tmp->rq_next) {
	    if ((IN_ORDER(tmp, req) ||
		!IN_ORDER(tmp, tmp->rq_next)) && IN_ORDER(req, tmp->rq_next))
		break;
	}
	req->rq_next = tmp->rq_next;
	tmp->rq_next = req;
	set_irq();
#if DEBUG_ASYNC
	if (debug_level > 2) printk("AQ%04x|", req);
#endif
    }
    //mark_buffer_clean(req->rq_bh);	/* EXPERIMENTAL: moved to end_request, blk.h */
}

static void make_request(unsigned short major, int rw, struct buffer_head *bh)
{
    struct request *req;
    int max_req;
    ext_buffer_head *ebh = EBH(bh);

#if DEBUG_ASYNC
    if (debug_level)
	printk("BLK (%d) %lu %s %lx:%x\n", major, buffer_blocknr(bh), rw==READ? "read": "write",
	(unsigned long)buffer_seg(bh), buffer_data(bh));
#endif

#ifdef BDEV_SIZE_CHK	/* NOTE: not updated for raw IO */
    sector_t count = BLOCK_SIZE / FIXED_SECTOR_SIZE;	/* FIXME must move to lower level*/
    sector_t sector = buffer_blocknr(bh) * count;
    if (blk_size[major])
	if (blk_size[major][MINOR(buffer_dev(bh))] < (sector + count) >> 1) {
	    printk("Attempt to access beyond end of device %d %d %d\n", blk_size[major][MINOR(buffer_dev(bh))], sector, count);
	    return;
	}
#endif

    /* Uhhuh.. Nasty dead-lock possible here.. */
    if (ebh->b_locked)
	return;
    /* Maybe the above fixes it, and maybe it doesn't boot. Life is interesting */
    lock_buffer(bh);

    switch (rw) {

    case READ:
	max_req = NR_REQUEST;	/* reads take precedence */
	break;

    case WRITE:
	/* We don't allow the write-requests to fill up the
	 * queue completely:  we want some room for reads,
	 * as they take precedence. The last third of the
	 * requests are only for reads.
	 */
	max_req = (NR_REQUEST * 2) / 3;
	break;

    default:
	printk("make_request: bad block dev cmd, must be R/W\n");
	unlock_buffer(bh);
	return;
    }

    /* find an unused request. */
    clr_irq();
    req = get_request(max_req, buffer_dev(bh)); 
    set_irq();

	// Try again blocking if no request available

    if (!req) {

	/* I suspect we may need to call get_request_wait() but not at the moment
	 * For now I will wait until we start getting panics, and then work out
	 * what we have to do - Al <ajr@ecs.soton.ac.uk>
	 * Indeed, when using direct floppy (async) IO, we immediately run out of requests
	 * on write, and need the wait. (HS)
	 */

#if defined(MULTI_BH) || defined(ASYNC_IO)
	req = __get_request_wait(max_req, buffer_dev(bh));
#else
	panic("Can't get request.");
#endif
    }

    /* fill up the request-info, and add it to the queue */
    req->rq_cmd = (__u8) rw;
    req->rq_bh = bh;
    if ((req->rq_nr_sectors = ebh->b_nr_sectors)) { /* raw device access, blocks = sectors */
	req->rq_buffer = bh->b_data;	/* pointing to process space */
	req->rq_seg = ebh->b_L2seg;
	req->rq_blocknr = ebh->b_blocknr;
    } else {
	req->rq_blocknr = buffer_blocknr(bh) * BLOCK_SIZE / 512;
	req->rq_seg = buffer_seg(bh);
	req->rq_buffer = buffer_data(bh);
    }
#if DEBUG_ASYNC
    if (debug_level) printk("|seg%lx:%04x|%cblk%d|BH%04x|dev%04x;", (unsigned long)buffer_seg(bh),
		buffer_data(bh), rw == READ ? 'R' : 'W',
		(word_t)ebh->b_blocknr, (word_t)bh, (word_t)buffer_dev(bh));
#endif

#ifdef BLOAT_FS
    req->rq_nr_sectors = count;
    req->rq_current_nr_sectors = count;
#endif

    req->rq_next = NULL;
    add_request(&blk_dev[major], req);
}


#if defined(MULTI_BH) || defined(ASYNC_IO)

/*
 * wait until a free request in the first N entries is available.
 */
static struct request *__get_request_wait(int n, kdev_t dev)
{
    register struct request *req;
    //printk("[%u] Waiting for request ...", (int)jiffies);

    //prepare_to_wait(&wait_for_request);	// maybe set interruptible ...
    wait_set(&wait_for_request);
    current->state = TASK_UNINTERRUPTIBLE;
    //sleep_on(&wait_for_request);	// or interruptible_sleep_on()
    goto startgrw;
    do {
	schedule();
  startgrw:
	//unplug_device(MAJOR(dev) + blk_dev);	/* Device can't be plugged */
	clr_irq();
	req = get_request(n, dev);
	set_irq();
    } while (req == NULL);
    current->state = TASK_RUNNING;
    wait_clear(&wait_for_request);

    return req;
}
#endif
#ifdef MULTI_BH

/*
 * "plug" the device if there are no outstanding requests: this will
 * force the transfer to start only after we have put all the requests
 * on the list.
 */

static void plug_device(register struct blk_dev_struct *dev,
			struct request *plug)
{
    flag_t flags;

    plug->rq_status = RQ_INACTIVE;
    plug->cmd = -1;
    plug->next = NULL;
    save_flags(flags);
    clr_irq();
    if (!dev->current_request)
	dev->current_request = plug;
    restore_flags(flags);
}

/*
 * remove the plug and let it rip..
 */

static void unplug_device(struct blk_dev_struct *dev)
{
    register struct request *req;
    unsigned int flags;

    save_flags(flags);
    clr_irq();
    req = dev->current_request;
    if (req && req->rq_status == RQ_INACTIVE && req->cmd == -1) {
	dev->current_request = req->next;
	(dev->request_fn) ();
    }
    restore_flags(flags);
}

/* This function can be used to request a number of buffers from a block
 * device. Currently the only restriction is that all buffers must belong
 * to the same device.
 */
void ll_rw_block(int rw, int nr, register struct buffer_head **bh)
{
    struct blk_dev_struct *dev;
    struct request plug;
    unsigned short int major;
    int i;

    /* Make sure the first block contains something reasonable */
    while (!*bh) {
	bh++;
	if (--nr <= 0)
	    return;
    }
    dev = NULL;
    if ((major = MAJOR(buffer_dev(bh[0])) < MAX_BLKDEV)
	dev = blk_dev + major;
    if (!dev || !dev->request_fn) {
	printk("ll_rw_block: Trying to read nonexistent block-device %s (%lx)\n",
	     kdevname(buffer_dev(bh[0])), buffer_blocknr(bh[0]));
	goto sorry;
    }

    /* If there are no pending requests for this device, then we insert
     * a dummy request for that device.  This will prevent the request
     * from starting until we have shoved all of the blocks into the
     * queue, and then we let it rip.  */
    if (nr > 1)
	plug_device(dev, &plug);
    for (i = 0; i < nr; i++)
	if (bh[i])
	    make_request(major, rw, bh[i]);
    unplug_device(dev);
    return;

  sorry:
    for (i = 0; i < nr; i++)
	if (bh[i]) {
		mark_buffer_clean(bh[i]);
		mark_buffer_uptodate(bh[i], 0);
	}
}
#endif /* MULTI_BH */

/* This function is used to request one or more physical sectors from a device.
 * (aka raw device access).
 * When called from a char/raw device driver, the buffer header
 * points to process data space, not to a buffer in the buffer-system.
 */

void ll_rw_blk(int rw, register struct buffer_head *bh)
{
    register struct blk_dev_struct *dev;
    unsigned int major;

    dev = NULL;
    if ((major = MAJOR(buffer_dev(bh))) < MAX_BLKDEV)
	dev = blk_dev + major;
    if (!dev || !dev->request_fn)
	panic("ll_rw_blk: unknown device %D", buffer_dev(bh));
    make_request(major, rw, bh);
}

void INITPROC blk_dev_init(void)
{
    register struct request *req;
    register struct blk_dev_struct *dev;

    dev = blk_dev;
    do {
	dev->request_fn = NULL;
	dev->current_request = NULL;
    } while (++dev < &blk_dev[MAX_BLKDEV]);

    req = request_list;
    do {
	req->rq_status = RQ_INACTIVE;
	req->rq_next = NULL;
    } while (++req < &request_list[NR_REQUEST]);

#ifdef CONFIG_BLK_DEV_RAM
    rd_init();		/* RAMDISK block device*/
#endif

#if defined(CONFIG_BLK_DEV_SSD_TEST) || defined(CONFIG_BLK_DEV_SSD_SD8018X)
    ssd_init();		/* SSD block device*/
#endif

#ifdef CONFIG_BLK_DEV_HD
    directhd_init();
#endif

#ifdef CONFIG_BLK_DEV_XD
    xd_init();
#endif

#ifdef CONFIG_BLK_DEV_FD
    floppy_init();
#endif

#ifdef CONFIG_BLK_DEV_BIOS
    bioshd_init();
#endif

#ifdef CONFIG_ROMFS_FS
    romflash_init();
#endif

}
