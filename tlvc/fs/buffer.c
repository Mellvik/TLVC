#include <linuxmt/types.h>
#include <linuxmt/init.h>
#include <linuxmt/sched.h>
#include <linuxmt/kernel.h>
#include <linuxmt/major.h>
#include <linuxmt/string.h>
#include <linuxmt/mm.h>
#include <linuxmt/heap.h>
#include <linuxmt/config.h>
#include <linuxmt/limits.h>
#include <linuxmt/errno.h>
#include <linuxmt/debug.h>
#include <linuxmt/trace.h>

#include <arch/system.h>
#include <arch/segment.h>
#include <arch/io.h>
#include <arch/irq.h>

/*
 * Kernel buffer management.
 */

/* Number of internal L1 buffers/cache blocks,
 * either used as the system buffer pool (no L2) or
 * to map/copy external L2 buffers to/from kernel data segment,
 * default is 12, may be overridden by 'cache=' in /bootopts. */
int nr_mapbufs = 12;
#define MAX_NR_MAPBUFS 24

/* The L1 buffers/cache is allocated from heap later. */
static unsigned char *L1buf;

int L2_bufs;		/* /bootopts # buffers override */

/* Buffer heads: local heap allocated */
static struct buffer_head *buffer_heads;

#ifdef CONFIG_FAR_BUFHEADS
static ext_buffer_head *ext_buffer_heads;

/* convert a buffer_head ptr to ext_buffer_head ptr */
ext_buffer_head *EBH(struct buffer_head *bh)
{
	int idx = bh - buffer_heads;
	return ext_buffer_heads + idx;
}

/* functions for buffer_head points called outside of buffer.c */
void mark_buffer_dirty(struct buffer_head *bh)     { EBH(bh)->b_dirty = 1; }
void mark_buffer_clean(struct buffer_head *bh)     { EBH(bh)->b_dirty = 0; }
unsigned char buffer_count(struct buffer_head *bh) { return EBH(bh)->b_count; }
block32_t buffer_blocknr(struct buffer_head *bh)   { return EBH(bh)->b_blocknr; }
kdev_t buffer_dev(struct buffer_head *bh)          { return EBH(bh)->b_dev; }

#endif /* CONFIG_FAR_BUFHEADS */


/* Buffer cache */
static struct buffer_head *bh_lru;
static struct buffer_head *bh_llru;
static struct buffer_head *bh_next;

/*
 * External L2 buffers are allocated within main or xms memory segments.
 * If CONFIG_FS_XMS_BUFFER is set and unreal mode and A20 gate can be enabled,
 *   extended/xms memory will be used and CONFIG_FS_NR_EXT_BUFFERS ignored.
 * Total extended/xms memory would be (BLOCK_SIZE * CONFIG_FS_NR_XMS_BUFFERS).
 * Otherwise, total main memory used is (BLOCK_SIZE * CONFIG_FS_NR_EXT_BUFFERS),
 * each main memory segment being up to 64K in size for segment register addressing
 *
 * Number of external/main (L2) buffers specified in config by CONFIG_FS_NR_EXT_BUFFERS
 * Number of extended/xms  (L2) buffers specified in config by CONFIG_FS_NR_XMS_BUFFERS
 */
#if defined(CONFIG_FS_EXTERNAL_BUFFER) || defined(CONFIG_FS_XMS_BUFFER)
static struct buffer_head *L1map[MAX_NR_MAPBUFS]; /* L1 indexed pointer to L2 buffer */
static struct wait_queue L1wait;		/* Wait for a free L1 buffer area */
static int lastL1map;
#endif
static struct wait_queue bufwait;		/* Wait for buffers to become available */

static int xms_enabled;
static unsigned int map_count, remap_count, unmap_count;

#ifdef CHECK_FREECNTS
static int nr_free_bh, nr_bh;

#define DCR_COUNT(bh) if(!(--bh->b_count))nr_free_bh++
#define INR_COUNT(bh) if(!(bh->b_count++))nr_free_bh--
#define CLR_COUNT(bh) if(bh->b_count)nr_free_bh++
#define SET_COUNT(bh) if(--nr_free_bh < 0) { panic("VFS: get_free_buffer: bad free buffer head count.\n"); }
#else

#define DCR_COUNT(bh) (bh->b_count--)
#define INR_COUNT(bh) (bh->b_count++)
#define CLR_COUNT(bh)
#define SET_COUNT(bh)
#endif

#define buf_num(bh)	((bh) - buffer_heads)	/* buffer number, for debugging */

#if defined(CHECK_FREECNTS) && DEBUG_EVENT
void list_buffer_status(void)
{
    int i = 1;
    int inuse = 0;
    int isinuse, j;
    struct buffer_head *bh = bh_llru;
    ext_buffer_head *ebh;

    printk("\n  #    buf/bh   blk/dev  flgs L1map Mcnt Bcnt\n");
    do {
        ebh = EBH(bh);
        isinuse = ebh->b_count || ebh->b_dirty || ebh->b_locked 
#ifdef CONFIG_FS_EXTERNAL_BUFFER
		|| ebh->b_mapcount
#endif
		;
        if (isinuse || bh->b_data) {
            j = 0;
#ifdef CONFIG_FS_EXTERNAL_BUFFER
            if (bh->b_data) {
                for (; j<nr_mapbufs; j++) {
                    if (L1map[j] == bh) {
                        j++;
                        break;
                    }
                }
            }
            printk("%3d: %3d/%04x %5ld/%04x %c%c%c   L%02d    %d   %d\n",
                i, buf_num(bh), bh, ebh->b_blocknr, ebh->b_dev,
                ebh->b_locked ? 'L':' ', ebh->b_dirty ? 'D':' ', ebh->b_uptodate ? 'U' : ' ',
                j, ebh->b_mapcount , ebh->b_count);
	    //if (ebh->b_blocknr == 1) printk("b_data %04x b_L2data %04x\n", bh->b_data, ebh->b_L2data);
#else
            printk("%3d: %3d/%04x %5ld/%04x %c%c%c         %d   %d\n",
                i, buf_num(bh), bh, ebh->b_blocknr, ebh->b_dev,
                ebh->b_locked ? 'L':' ', ebh->b_dirty ? 'D':' ', ebh->b_uptodate ? 'U' : ' ',
                j, ebh->b_count);
#endif
        }
        i++;
        if (isinuse) inuse++;
    } while ((bh = ebh->b_prev_lru) != NULL);
#ifdef CONFIG_FS_EXTERNAL_BUFFER
    printk("Total L2 buffers inuse %d/%d (%d free), %dk L1 (map %u unmap %u remap %u)\n",
	inuse, nr_bh, nr_free_bh, nr_mapbufs, map_count, unmap_count, remap_count);
#else
    printk("Total buffers inuse %d/%d\n",
	inuse, nr_bh);
#endif
}

#if UNUSED
static void dump_buffer(struct buffer_head *bh, int count) {
	int i;

	if (bh) {
		for (i = 0; i < count; i += 2) {
			if (!i%16) printk("\n");
			printk("%04x ",  *(unsigned int *)(buffer_data(bh) + i));
		}
	} else printk("dump_buf: bh zero\n");
	printk("\n");
}
#endif
#endif

/*
 * Put buffer at the end of the list. 
 */
static void put_last_lru(struct buffer_head *bh)
{
    ext_buffer_head *ebh = EBH(bh);
    flag_t flags;

    save_flags(flags);	/* EXPERIMENTAL */
    clr_irq();
    if (bh_llru != bh) {
	struct buffer_head *bhn = ebh->b_next_lru;
	ext_buffer_head *ebhn = EBH(bhn);

	if ((ebhn->b_prev_lru = ebh->b_prev_lru))	/* Either unhook */
	    EBH(ebh->b_prev_lru)->b_next_lru = bhn;
	else					/* .. or alter head */
	    bh_lru = bhn;
	/*
	 *      Put on lru end
	 */
	ebh->b_next_lru = NULL;
	EBH(ebh->b_prev_lru = bh_llru)->b_next_lru = bh;
	bh_llru = bh;
    }
    restore_flags(flags);
}

static void INITPROC add_buffers(int nbufs, unsigned char *buf, ramdesc_t seg)
{
    struct buffer_head *bh;
    int n = 0;
    size_t offset;

    for (bh = bh_next; n < nbufs; n++, bh = ++bh_next) {
	ext_buffer_head *ebh = EBH(bh);

	if (bh != buffer_heads) {
	    ebh->b_next_lru = ebh->b_prev_lru = bh;
	    put_last_lru(bh);
	}

#if defined(CONFIG_FS_EXTERNAL_BUFFER) || defined(CONFIG_FS_XMS_BUFFER)
	/* segment adjusted to require no offset to buffer */
	offset = xms_enabled? ((n & 63) << BLOCK_SIZE_BITS) :
			      ((n & 63) << (BLOCK_SIZE_BITS - 4));
	ebh->b_L2seg = seg + offset;
#else
	bh->b_data = buf;
	buf += BLOCK_SIZE;
#endif
    }
}

int INITPROC buffer_init(void)
{
    /* XMS buffers override EXT buffers override internal buffers*/
#if defined(CONFIG_FS_EXTERNAL_BUFFER) || defined(CONFIG_FS_XMS_BUFFER)
    int bufs_to_alloc = CONFIG_FS_NR_EXT_BUFFERS;

#ifdef CONFIG_FS_XMS_BUFFER
    xms_enabled = xms_init();	/* try to enable unreal mode and A20 gate*/
    if (xms_enabled)
	bufs_to_alloc = CONFIG_FS_NR_XMS_BUFFERS;
#endif
    if (L2_bufs)
	bufs_to_alloc = L2_bufs;
#ifdef CONFIG_FAR_BUFHEADS
    /* FIXME: update max # after reducing the buffer_head struct size */
    if (bufs_to_alloc > 2975) bufs_to_alloc = 2975; /* max 64K far bufheads @22 bytes*/
#else
    if (bufs_to_alloc > 256) bufs_to_alloc = 64; /* high value likely set for XMS buffers,
						  * set to something that's likely to work */
#endif

    printk("%d %s buffers (%ldB ram), ", bufs_to_alloc, xms_enabled? "xms": "ext",
		(long)bufs_to_alloc << 10);
#else
    int bufs_to_alloc = nr_mapbufs;
#endif
    printk("%d L1 buffers, %uB ram\n", nr_mapbufs, nr_mapbufs<<10);
    if (nr_mapbufs > bufs_to_alloc)
	printk("WARNING: # of L2 buffers is lower than the L1 cache!\n");

    nr_bh = nr_free_bh = bufs_to_alloc;
#ifdef CHECK_FREECNTS
    debug_setcallback(1, list_buffer_status);   /* ^O will generate buffer list */
#endif

    if (!(L1buf = heap_alloc(nr_mapbufs * BLOCK_SIZE, HEAP_TAG_BUFHEAD|HEAP_TAG_CLEAR)))
        return 1;

    buffer_heads = heap_alloc(bufs_to_alloc * sizeof(struct buffer_head),
	HEAP_TAG_BUFHEAD|HEAP_TAG_CLEAR);
    if (!buffer_heads) return 1;
#ifdef CONFIG_FAR_BUFHEADS
    size_t size = bufs_to_alloc * sizeof(ext_buffer_head);
    segment_s *seg = seg_alloc((size + 15) >> 4, SEG_FLAG_EXTBUF);
    if (!seg) return 1;
    fmemsetw(0, seg->base, 0, size >> 1);
    ext_buffer_heads = _MK_FP(seg->base, 0);
#endif
    bh_next = bh_lru = bh_llru = buffer_heads;

#if defined(CONFIG_FS_EXTERNAL_BUFFER) || defined(CONFIG_FS_XMS_BUFFER)
    do {
	int nbufs;

	/* allocate buffers in 64k chunks so addressable with segment/offset*/
	if ((nbufs = bufs_to_alloc) > 64)
	    nbufs = 64;
	bufs_to_alloc -= nbufs;
#ifdef CONFIG_FS_XMS_BUFFER
	if (xms_enabled) {
	    ramdesc_t xmsseg = xms_alloc((long_t)nbufs << BLOCK_SIZE_BITS);
	    add_buffers(nbufs, 0, xmsseg);
	} else
#endif
	{
	    segment_s *extseg = seg_alloc (nbufs << (BLOCK_SIZE_BITS - 4),
		SEG_FLAG_EXTBUF|SEG_FLAG_ALIGN1K);
	    if (!extseg) return 2;
	    add_buffers(nbufs, 0, extseg->base);
	}
    } while (bufs_to_alloc > 0);
#else
    /* no EXT or XMS buffers, internal L1 only */
    add_buffers(nr_mapbufs, L1buf, kernel_ds);
#endif
    return 0;
}

/*
 *	Wait on a buffer
 */

void wait_on_buffer(struct buffer_head *bh)
{
    ext_buffer_head *ebh = EBH(bh);

    ebh->b_count++;
    wait_set((struct wait_queue *)bh);       /* use bh as wait address */
    for (;;) {

        current->state = TASK_UNINTERRUPTIBLE;
        if (!ebh->b_locked)
            break;
        schedule();
    }

    wait_clear((struct wait_queue *)bh);
    current->state = TASK_RUNNING;
    ebh->b_count--;

#ifdef CHECK_BLOCKIO
    if (EBH(bh)->b_locked) panic("wait_on_buffer");
#endif
}

void lock_buffer(struct buffer_head *bh)
{
    wait_on_buffer(bh);
    EBH(bh)->b_locked = 1;
}

void unlock_buffer(struct buffer_head *bh)
{
    EBH(bh)->b_locked = 0;
    wake_up((struct wait_queue *)bh);	/* use bh as wait address*/
	//FIXME: Check if this one is useful at all or if the one in brelse is enough.
	// ALSO, if we want to use schedule() instead of sleep/wait on this one.
    wake_up(&bufwait);	/* If we ran out of buffers, get_free_buffers is waiting */
}

void invalidate_buffers(kdev_t dev)
{
    struct buffer_head *bh = bh_llru;
    ext_buffer_head *ebh;

    do {
	ebh = EBH(bh);

	if (ebh->b_dev != dev) continue;
	wait_on_buffer(bh);
        if (ebh->b_count) {
            printk("invalidate_buffers: skipping active block %ld\n", ebh->b_blocknr); /*DEBUG*/
            continue;
        }
	debug_blk("invalidating blk %ld\n", ebh->b_blocknr);
	ebh->b_uptodate = 0;
	mark_buffer_clean(bh);
	brelseL1(bh, 0);        /* release buffer from L1 if present */
	unlock_buffer(bh);
    } while ((bh = ebh->b_prev_lru) != NULL);
}

static void sync_buffers(kdev_t dev, int wait)
{
    struct buffer_head *bh = bh_lru;
    ext_buffer_head *ebh;

#ifdef CHECK_FREECNTS
    list_buffer_status();
#endif
    debug_blk("sync_buffers dev %p wait %d\n", dev, wait);

    do {
	ebh = EBH(bh);

	/*
	 *      Skip clean buffers.
	 */
	if ((dev && (ebh->b_dev != dev)) || !ebh->b_dirty)
	   continue;

	/*
	 *      If buffer is locked; skip it unless wait is requested
	 *      AND pass > 0.
	 */

	if (ebh->b_locked) {
            debug_blk("SYNC: dev %x buf %d block %ld LOCKED mapped %d skipped %d data %04x\n",
                ebh->b_dev, buf_num(bh), ebh->b_blocknr, ebh->b_mapcount, !wait,
                bh->b_data);
	    if (!wait) continue;
	    wait_on_buffer(bh);
	}

	/*
	 *      Do the stuff
	 */
	ebh->b_count++;
	ll_rw_blk(WRITE, bh);
	ebh->b_count--;
    } while ((bh = ebh->b_next_lru) != NULL);
}

/*
 * find an available buffer
 */
struct buffer_head *get_free_buffer(void)
{
    struct buffer_head *bh = bh_lru;
    ext_buffer_head *ebh = EBH(bh);
    int i, sync_loop = 0;

	//printk("lru:%04x/%d|", bh, ebh->b_dirty);
    while (ebh->b_count || ebh->b_dirty || ebh->b_locked
#if defined(CONFIG_FS_EXTERNAL_BUFFER) || defined(CONFIG_FS_XMS_BUFFER)
		|| bh->b_data
#endif
								) {
	if ((bh = ebh->b_next_lru) == NULL) {
	    unsigned long jif = jiffies;
	    switch (sync_loop) {
	    case 0:
		printk("DEBUG: no free bufs, syncing\n"); /* FIXME delete */
		sync_buffers(0,0);
		break;

#if defined(CONFIG_FS_EXTERNAL_BUFFER) || defined(CONFIG_FS_XMS_BUFFER)
	    case 1:
	    /*
	     * Skip if we're running with L1 buffers only.
	     * (in which case brelseL1_index() is a dummy anyway)
	     */ 
		printk("RelL1:");
		for (i=0; i<nr_mapbufs; i++)
			brelseL1_index(i, 1);
		printk("\n");
		break;
#endif

	    case 2:	/* EXPERIMENTAL: This may not make much sense */
		printk("SyncWait;");
		sync_buffers(0, 1);	/* sync w/ wait */
		break;

	    default:
		/* we're out of buffers - which happens quite a bit when using floppy
		 * based file systems: Sleep instead of looping.
		 */
		sleep_on(&bufwait);
		printk("Bufwait %d [%lu] jiffies;", sync_loop, jiffies-jif);
	    }
	    bh = bh_lru;	/* start over */
	    sync_loop++;
	}	
	ebh = EBH(bh);
    }
#ifdef CHECK_BLOCKIO
    if (ebh->b_mapcount) panic("get_free_buffer"); /* mapped buffer reallocated */
#endif
    put_last_lru(bh);
    ebh->b_uptodate = 0;
    ebh->b_count = 1;
    SET_COUNT(ebh);
    //if (sync_loop)
	//printk("GFB:%04x/%04x\n", bh, *(unsigned int *)bh->b_data);
    return bh;
}

/*
 * Release a buffer head
 */

void brelse(struct buffer_head *bh)
{
    ext_buffer_head *ebh;

    if (!bh) return;
    wait_on_buffer(bh);
    ebh = EBH(bh);
#ifdef CHECK_BLOCKIO
    if (ebh->b_count == 0) panic("brelse");
#endif
    DCR_COUNT(ebh);
//#ifdef BLOAT_FS
    if (!ebh->b_count)
	wake_up(&bufwait);	/* EXPERIMENTAL, probably superfluous and expensive */
//#endif
}

/*
 * bforget() is like brelse(), except it removes the buffer
 * data validity.
 */
#if 0
void __bforget(struct buffer_head *bh)
{
    ext_buffer_head *ebh = EBH(bh);

    wait_on_buffer(bh);
    mark_buffer_clean(bh);
    DCR_COUNT(ebh);
    ebh->b_dev = NODEV;
    wake_up(&bufwait);
}
#endif

/* Turns out both minix_bread and bread do this, so I made this a function
 * of it's own... */

struct buffer_head *readbuf(struct buffer_head *bh)
{
    ext_buffer_head *ebh = EBH(bh);

	//printk("readbuf %04x/%04x;", bh, bh->b_dev);
    if (!ebh->b_uptodate) {
	ll_rw_blk(READ, bh);
	wait_on_buffer(bh);
	if (!ebh->b_uptodate) {
	    brelse(bh);
	    bh = NULL;
	}
    }
    //dump_buffer(bh, 64);
    return bh;
}

static struct buffer_head *find_buffer(kdev_t dev, block32_t block)
{
    struct buffer_head *bh = bh_llru;
    ext_buffer_head *ebh;

    do {
	ebh = EBH(bh);

	if (ebh->b_blocknr == block && ebh->b_dev == dev) break;
    } while ((bh = ebh->b_prev_lru) != NULL);
    return bh;
}

struct buffer_head *get_hash_table(kdev_t dev, block_t block)
{
    struct buffer_head *bh;

    if ((bh = find_buffer(dev, (block32_t)block)) != NULL) {
	ext_buffer_head *ebh = EBH(bh);

	INR_COUNT(ebh);
	wait_on_buffer(bh);
    }
    return bh;
}

/*
 * Ok, this is getblk, and it isn't very clear, again to hinder
 * race-conditions. Most of the code is seldom used, (ie repeating),
 * so it should be much more efficient than it looks.
 *
 * The algorithm is changed: hopefully better, and an elusive bug removed.
 *
 * 14.02.92: changed it to sync dirty buffers a bit: better performance
 * when the filesystem starts to get full of dirty blocks (I hope).
 */

/* 16 bit block numbers for super blocks and MINIX filesystem driver*/
struct buffer_head *getblk(kdev_t dev, block_t block)
{
	return getblk32(dev, (block32_t)block);
}

/* 32 bit block numbers for FAT filesystem driver*/
struct buffer_head *getblk32(kdev_t dev, block32_t block)
{
    struct buffer_head *bh;
    struct buffer_head *n_bh;
    ext_buffer_head *ebh;

    /* If there are too many dirty buffers, we wake up the update process
     * now so as to ensure that there are still clean buffers available
     * for user processes to use (and dirty) */

    n_bh = NULL;
    goto start;
    do {
	/*
	 * Block not found. Create a buffer for this job.
	 */
	n_bh = get_free_buffer();	/* This function may sleep and someone else */
      start:				/* can create the block */
	if ((bh = find_buffer(dev, block)) != NULL) goto found_it;
    } while (n_bh == NULL);
    bh = n_bh;				/* Block not found, use the new buffer */

/* OK, FINALLY we know that this buffer is the only one of its kind,
 * and that it's unused (b_count=0), unlocked (b_locked=0), and clean
 */
    ebh = EBH(bh);
    ebh->b_dev = dev;
    ebh->b_blocknr = block;
    goto return_it;

  found_it:
    if (n_bh != NULL) {
	ext_buffer_head *en_bh = EBH(n_bh);

	CLR_COUNT(en_bh);
	en_bh->b_count = 0;	/* Release previously created buffer head */
    }
    ebh = EBH(bh);
    INR_COUNT(ebh);
    wait_on_buffer(bh);
    if (!ebh->b_dirty && ebh->b_uptodate)
	put_last_lru(bh);

  return_it:
    //printk("getblk %04x %lu\n", dev, block);
    return bh;
}

/*
 * bread() reads a specified block and returns the buffer that contains
 * it. It returns NULL if the block was unreadable.
 */

/* 16 bit block numbers for super blocks and MINIX filesystem driver*/
struct buffer_head *bread(kdev_t dev, block_t block)
{
    return readbuf(getblk(dev, block));
}

/* 32 bit block numbers for FAT filesystem driver*/
struct buffer_head *bread32(kdev_t dev, block32_t block)
{
    return readbuf(getblk32(dev, block));
}

#if 0

/* NOTHING is using breada at this point, so I can pull it out... Chad */
struct buffer_head *breada(kdev_t dev, block_t block, int bufsize,
			   unsigned int pos, unsigned int filesize)
{
    struct buffer_head *bh, *bha;
    int i, j;

    if (pos >= filesize)
	return NULL;

    if (block < 0)
	return NULL;
    bh = getblk(dev, block);

    if (bh->b_uptodate)
	return bh;

    bha = getblk(dev, block + 1);
    if (bha->b_uptodate) {
	brelse(bha);
	bha = NULL;
    } else {
	/* Request the read for these buffers, and then release them */
	ll_rw_blk(READ, bha);
	brelse(bha);
    }
    /* Wait for this buffer, and then continue on */
    wait_on_buffer(bh);
    if (bh->b_uptodate)
	return bh;
    brelse(bh);
    return NULL;
}

#endif

void mark_buffer_uptodate(struct buffer_head *bh, int on)
{
    ext_buffer_head *ebh = EBH(bh);
    flag_t flags;

    save_flags(flags);
    clr_irq();
    ebh->b_uptodate = on;
    restore_flags(flags);
}

void fsync_dev(kdev_t dev)
{
    sync_buffers(dev, 0);
    sync_supers(dev);
    sync_inodes(dev);
    sync_buffers(dev, 1);
}

void sync_dev(kdev_t dev)
{
    sync_buffers(dev, 0);
    sync_supers(dev);
    sync_inodes(dev);
    sync_buffers(dev, 0);
}

int sys_sync(void)
{
    fsync_dev(0);
    return 0;
}


/* clear a buffer area to zeros, used to avoid slow map to L1 if possible */
void zero_buffer(struct buffer_head *bh, size_t offset, int count)
{
#if defined(CONFIG_FS_XMS_INT15) || (!defined(CONFIG_FS_EXTERNAL_BUFFER) && !defined(CONFIG_FS_XMS_BUFFER))
#define FORCEMAP 1
#else
#define FORCEMAP 0
#endif
    /* xms int15 doesn't support a memset function, so map into L1 */
    if (FORCEMAP || bh->b_data) {
        map_buffer(bh);
        memset(bh->b_data + offset, 0, count);
        unmap_buffer(bh);
    }
#if !FORCEMAP
    else {
        ext_buffer_head *ebh = EBH(bh);
        xms_fmemset((char *)offset, ebh->b_L2seg, 0, count);
    }
#endif
}

#if defined(CONFIG_FS_EXTERNAL_BUFFER) || defined(CONFIG_FS_XMS_BUFFER)

/* map_buffer copies a buffer into L1 buffer space. It will freeze forever
 * before failing, so it can return void.  This is mostly 8086 dependant,
 * although the interface is not.
 */
void map_buffer(struct buffer_head *bh)
{
    ext_buffer_head *ebh = EBH(bh);
    int i;

    /* Just in case we're in a middle of some IO */
    wait_on_buffer(bh);

    /* If buffer is already mapped, just increase the refcount and return */
    if (bh->b_data) {
	if (!ebh->b_mapcount)
	    debug("REMAP: %d\n", buf_num(bh));
	//printk("RM%d;", buf_num(bh));
	remap_count++;
	goto end_map_buffer;
    }

    //printk("MB:%x/", buf_num(bh));
    i = lastL1map;
    /* search for free L1 buffer or wait until one is available*/
    for (;;) {
	struct buffer_head *bmap;
	ext_buffer_head *ebmap;

	if (++i >= nr_mapbufs) i = 0;
	debug("map:   %d try %d\n", buf_num(bh), i);

	/* First check for the trivial case, to avoid dereferencing a null pointer */
	if (!(bmap = L1map[i]))
	    break;
	ebmap = EBH(bmap);

	/* L1 with zero count can be unmapped and reused for this request*/
#ifdef CHECK_BLOCKIO
	if (ebmap->b_mapcount < 0)
		printk("map_buffer: %d BAD mapcount %d\n", buf_num(bmap), ebmap->b_mapcount);
#endif
	/* don't remap if I/O in progress to prevent bh/req buffer unpairing */
	if (!ebmap->b_mapcount && !ebmap->b_locked) {
	    debug("UNMAP: %d <- %d\n", buf_num(bmap), i);
	    //printk("F_UNMAP %x;", buf_num(bmap));
	    brelseL1_index(i, 1);       /* Unmap/copy L1 to L2 */
	    break;		/* success */
	}
	if (i == lastL1map) {
	    /* no free L1 buffers, must wait for L1 unmap_buffer*/
	    debug("MAPWAIT: %d\n", buf_num(bh));
	    sleep_on(&L1wait);
	}
    }

    /* Map/copy L2 to L1 */
    lastL1map = i;
    L1map[i] = bh;
    bh->b_data = L1buf + (i << BLOCK_SIZE_BITS);
    if (ebh->b_uptodate)
	xms_fmemcpyw(bh->b_data, kernel_ds, 0, ebh->b_L2seg, BLOCK_SIZE/2);
    debug("MAP:   %d -> %d\n", buf_num(bh), i);
    //printk("MAP:   %d -> %d\n", buf_num(bh), i);
    //printk("%x/%04x;", bh->b_data, (word_t)*bh->b_data);
    map_count++;
  end_map_buffer:
    ebh->b_mapcount++;
}

/* unmap_buffer decreases bh->b_mapcount, and wakes up anyone waiting over
 * in map_buffer if it's decremented to 0... this is a bit of a misnomer,
 * since the unmapping is actually done in map_buffer to prevent frivoulous
 * unmaps if possible.
 */
void unmap_buffer(struct buffer_head *bh)
{
    if (bh) {
	ext_buffer_head *ebh = EBH(bh);

	//printk("UB:%d/%d;", buf_num(bh), ebh->b_mapcount);
#ifdef CHECK_BLOCKIO
	if (ebh->b_mapcount <= 0) {
	    printk("unmap_buffer: %d BAD mapcount %d\n", buf_num(bh), ebh->b_mapcount);
	    ebh->b_mapcount = 0;
	} else
#endif
	if (--ebh->b_mapcount == 0) {
	    debug("unmap: %d\n", buf_num(bh));
	    wake_up(&L1wait);
	} else
	    debug("unmap_buffer: %d mapcount %d\n", buf_num(bh), ebh->b_mapcount+1);
    }
}

void unmap_brelse(struct buffer_head *bh)
{
    if (bh) {
	unmap_buffer(bh);
	brelse(bh);
    }
}

/* release L1 buffer by index with optional copyout */
void brelseL1_index(int i, int copyout)
{
    struct buffer_head *bh = L1map[i];
    ext_buffer_head *ebh;

    if (!bh) return;
    ebh = EBH(bh);
    if (ebh->b_mapcount || ebh->b_locked)
        return;
    if (copyout && ebh->b_uptodate && bh->b_data) {
        xms_fmemcpyw(0, ebh->b_L2seg, bh->b_data, kernel_ds, BLOCK_SIZE/2);
	unmap_count++;
    }
    bh->b_data = 0;
    //printk("RelL1:%d;", i);
    L1map[i] = 0;
}

/* release L1 buffer by buffer head with optional copyout */
void brelseL1(struct buffer_head *bh, int copyout)
{
    int i;

    if (!bh) return;
    for (i = 0; i < nr_mapbufs; i++) {
        if (L1map[i] == bh) {
            brelseL1_index(i, copyout);
            break;
        }
    }
}

ramdesc_t buffer_seg(struct buffer_head *bh)
{
    return (bh->b_data? kernel_ds: EBH(bh)->b_L2seg);
}

unsigned char *buffer_data(struct buffer_head *bh)
{
    return (bh->b_data? bh->b_data: 0);	/* L2 addresses now always @ offset 0 */
}

#endif /* CONFIG_FS_EXTERNAL_BUFFER || CONFIG_FS_XMS_BUFFER */
