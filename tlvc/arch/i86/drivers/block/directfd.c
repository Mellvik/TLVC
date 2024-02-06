/*
 * Copyright (C) 1991, 1992  Linus Torvalds
 *
 * 02.12.91 - Changed to static variables to indicate need for reset
 * and recalibrate. This makes some things easier (output_byte reset
 * checking etc), and means less interrupt jumping in case of errors,
 * so the code is hopefully easier to understand.
 *
 * This file is certainly a mess. I've tried my best to get it working,
 * but I don't like programming floppies, and I have only one anyway.
 * Urgel. I should check for more errors, and do more graceful error
 * recovery. Seems there are problems with several drives. I've tried to
 * correct them. No promises.
 *
 * As with hd.c, all routines within this file can (and will) be called
 * by interrupts, so extreme caution is needed. A hardware interrupt
 * handler may not sleep, or a kernel panic will happen. Thus I cannot
 * call "floppy-on" directly, but have to set a special timer interrupt
 * etc.
 *
 * 28.02.92 - made track-buffering routines, based on the routines written
 * by entropy@wintermute.wpi.edu (Lawrence Foard). Linus.
 *
 * Automatic floppy-detection and formatting written by Werner Almesberger
 * (almesber@nessie.cs.id.ethz.ch), who also corrected some problems with
 * the floppy-change signal detection.
 *
 * 1992/7/22 -- Hennus Bergman: Added better error reporting, fixed
 * FDC data overrun bug, added some preliminary stuff for vertical
 * recording support.
 *
 * 1992/9/17: Added DMA allocation & DMA functions. -- hhb.
 *
 * TODO: Errors are still not counted properly.
 *
 * 1992/9/20
 * Modifications for ``Sector Shifting'' by Rob Hooft (hooft@chem.ruu.nl)
 * modelled after the freeware MS/DOS program fdformat/88 V1.8 by
 * Christoph H. Hochstatter.
 *
 * I have fixed the shift values to the ones I always use. Maybe a new
 * ioctl() should be created to be able to modify them.
 * There is a bug in the driver that makes it impossible to format a
 * floppy as the first thing after bootup.
 *
 * 1993/4/29 -- Linus -- cleaned up the timer handling in the kernel, and
 * this helped the floppy driver as well. Much cleaner, and still seems to
 * work.
 *
 * 1996/3/27 -- trp@cyberoptics.com
 * began port to linux-8086
 * removed volatile and fixed function definitions
 *
 * 2023/3/10 -- helge@skrivervik.com
 * Rewritten for TLVC to replace BIOS based IO from ELKS. 
 * DMA setup from Minix for simplicity and compactness. 
 */

/*
 * TODO (HS 2023):
 * - When XMS buffers are active, the BIOS hd driver will use DMASEG as a bounce buffer
 *   thus colliding with the usage here. This is a problem only in the odd case 
 *   that we're using BIOS HD + DIRECT FD + XMS buffers + TRACK cache, 
 *   which really should not happen. IOW - use either BIOS block IO or DIRECT block IO,
 *   don't mix!!
 * - Update DMA code
 * - Clean up debug output
 * - The driver has many provisions for 4 floppy drives, but except for the oldest PCs
 *   (PC and XT) there is no physical support more than 2 drives except via a 2nd controller.
 *   Also, this driver uses CMOS settings to set the drive type which applies to drives 0 
 *   and 1 only. 2 and 3 will have base_type[] 0 and ultimately fail.
 * - Add verbose flag to reduce 'normal' verbosity (replace the former per-drive
 *   flag in ftd_msg[].
 */

#include <linuxmt/config.h>
#include <linuxmt/sched.h>
#include <linuxmt/fs.h>
#include <linuxmt/kernel.h>
#include <linuxmt/timer.h>
#include <linuxmt/mm.h>
#include <linuxmt/signal.h>
#include <linuxmt/fdreg.h>
#include <linuxmt/fd.h>
#include <linuxmt/errno.h>
#include <linuxmt/string.h>
#include <linuxmt/debug.h>
#include <linuxmt/memory.h>	/* for peek/poke */
#include <linuxmt/stat.h>	/* for S_ISCHR() */

#include <arch/dma.h>
#include <arch/system.h>
#include <arch/io.h>
#include <arch/segment.h>
#include <arch/ports.h>
#include <arch/hdreg.h>		/* for ioctl GETGEO */

#define FLOPPYDISK
#define MAJOR_NR FLOPPY_MAJOR
#define MINOR_SHIFT	5	/* shift to get drive num */
#define FLOPPY_DMA 2		/* hardwired on old PCs */

#include "blk.h"
#ifdef CONFIG_BLK_DEV_BHD	/* Kludge - FIXME */
int running_qemu = 0;
#else
extern int running_qemu;
#endif

/* This is confusing. DEVICE_INTR is the do_floppy variable.
 * The code is sometimes using the macro, some times the variable.
 * It may seem this is a trick to get GCC to shut up ...
 */
#ifdef DEVICE_INTR	/* from blk.h */
void (*DEVICE_INTR) () = NULL;


#define SET_INTR(x) (DEVICE_INTR = (x))
#define CLEAR_INTR SET_INTR(NULL)
#else
#define CLEAR_INTR
#endif		/* DEVICE_INTR */

#define _MK_LINADDR(seg, offs) ((unsigned long)((((unsigned long)(seg)) << 4) + (unsigned)(offs)))

/* Driver configuration */

//#define dfd_debug	/* lots of debug output */
//#define INCLUDE_FD_FORMATTING	/* Formatting code untested, don't waste the space */

#ifndef CONFIG_HW_PCXT	/* some options are not meaningful on pre-AT systems */
#define ENABLE_FDC_82077	/* Enable 82077 extras if present, REQUIRED to use 360k media */
			/* in 1.2M drive on QEMU!! */
#define USE_DIR_REG	/* Use the DIR register if available for media change detection */
			/* Default is on, off is experimental */
			/* Use off for PC/XT/8086/8088 8bit ISA systems (save RAM), 
			 * on otherwise - to let the driver make wise (!) choices -
			 * see developer notes for details. */
#else
#undef CHECK_MEDIA_CHANGE /* Now defined in linuxmt/fs.h */
#endif

//#define ENABLE_CYLBUF	/* Allow small capacity floppies to read a full cylinder  */
			/* into the (18 sector) track cache - NOT GOOD for root file systems */

#ifdef dfd_debug
#define DEBUG printk
#else
#define DEBUG(...)
#endif

#ifdef CHECK_MEDIA_CHANGE
#ifndef USE_DIR_REG
#define USE_DIR_REG	/* Required to check media change */
#endif
#endif

#ifdef USE_DIR_REG
#define MEDIA_CHANGED	((sys_caps & CAP_PC_AT)? (inb(FD_DIR) & 0x80) : 0)
#else
#define MEDIA_CHANGED	0
#endif


static int initial_reset_flag = 0;
static int need_configure = 1;	/* for 82077 */
static int reset = 0;		/* something went wrong, reset FDC, start over */
static int recalibrate = 0;	/* less dramatic than reset, like changed drive
				 * parameters or seek errors. Will (eventually)
				 * trigger a call to recalibrate_floppy() */
static int recover = 0;		/* signal that we're recovering from a hang,
				 * awakened by the watchdog timer */
static int seek = 0;		/* set if the current operation needs a track
				 * change (seek) */
static int nr_sectors;		/* # of sectors to r/w, 2 if block IO */
static int raw;			/* set if raw/char IO	*/

static unsigned char current_DOR = 0x0C; /* default: DMA on, RESET off */

static unsigned char running = 0; /* keep track of motors already running */
/* NOTE: current_DOR tells which motor(s) have been commanded to run,
 * 'running' tells which ones are actually running. The difference is subtle - 
 * the spinup time, typical .5 secs */

/*
 * Note that MAX_ERRORS=X doesn't imply that we retry every bad read
 * max X times - some types of errors increase the errorcount by 2 or
 * even 3, so we might actually retry only X/2 times before giving up.
 */
#define MAX_ERRORS 6

/*
 * Maximum disk size (in kilobytes). This default is used whenever the
 * current disk size is unknown.
 */
#define MAX_DISK_SIZE 1440	/* change to add support for 2880K */

/*
 * Maximum number of sectors in a track cache. Track caching is disabled
 * if tracks are bigger.
 */
#define MAX_BUFFER_SECTORS 18

/*
 * All floppy drives have 2 heads!
 */
#define FLOPPY_HEADS	2


/*
 * The 8237 DMA controller cannot access data above 1MB on the origianl PC 
 * (4 bit page register). The AT is different, the page reg is 8 bits while the
 * 2nd DMA controller transfers words only, not bytes and thus up to 128k bytes
 * in one 'swoop'.
 */
#define LAST_DMA_ADDR	(0x100000L - BLOCK_SIZE) /* Enforce the 1M limit */

/*
 * globals used by 'result()'
 */
#define MAX_REPLIES 7
static unsigned char reply_buffer[MAX_REPLIES];
#define ST0 (reply_buffer[0])
#define ST1 (reply_buffer[1])
#define ST2 (reply_buffer[2])
#define ST3 (reply_buffer[3])

/* CMOS drive types, from CMOS location 0x10 */
/* NOTE: Pre-AT machines have no CMOS, not DIR - should be configured from 
 * /bootopts, will currently default to one type 1 drive */
#define CMOS_NONE   0
#define CMOS_360k   1
#define CMOS_1200k  2
#define CMOS_720k   3
#define CMOS_1400k  4
#define CMOS_2880k  5
#define CMOS_MAX    5

/* indices into minor_types[], used for floppy format probes */
#define FT_360k_PC  1           /* 360kB PC diskettes */
#define FT_1200k    2           /* 1.2 MB AT-diskettes */
#define FT_720k     3           /* 3.5" 720kB diskette */
#define FT_360k_AT  4           /* 360kB in 1.2MB drive */
#define FT_720k_AT  5           /* 720kB in 1.44MB drive */
#define FT_1440k    6           /* 3.5" 1.44MB diskette */

/*
 * The 'stretch' tells if the tracks need to be doubled for some
 * types (ie 360kB diskette in 1.2MB drive). 
 */
static struct floppy_struct floppy_type[] = {
	    {   0,  0, 0, 0, 0x00, 0x00, 0x00, 0x00, NULL},	  /* no testing */
    /* 1 */ { 720,  9, 40, 0, 0x2A, 0x02, 0xDF, 0x50, "360k/PC"}, /* 360kB PC diskettes */
    /* 2 */ {2400, 15, 80, 0, 0x1B, 0x00, 0xDF, 0x54, "1.2M"},	  /* 1.2 MB AT-diskettes */
    /* 3 */ {1440,  9, 80, 0, 0x2A, 0x02, 0xDF, 0x50, "720k"},	  /* 3.5" 720kB diskette */
    /* 4 */ { 720,  9, 40, 1, 0x23, 0x01, 0xDF, 0x50, "360k/AT"}, /* 360kB in 1.2MB drive */
    /* 5 */ {1440,  9, 80, 0, 0x23, 0x01, 0xDF, 0x50, "720k/AT"}, /* 720kB in 1.2MB drive */
    /* 6 */ {2880, 18, 80, 0, 0x1B, 0x00, 0xCF, 0x6C, "1.44M"},	  /* 1.44MB diskette */
	 /* totSectors/secPtrack/tracks/stretch/gap/Drate/S&Hrates/fmtGap/name/  */
};

/* floppy probes to try per CMOS floppy type */
static unsigned char p360k[] =  { FT_360k_PC, FT_360k_PC, 0 };
static unsigned char p1200k[] = { FT_1200k,   FT_360k_AT, 0 };
static unsigned char p720k[] =  { FT_720k,    FT_720k,    0 };
static unsigned char p1440k[] = { FT_1440k,   FT_720k,    0 };

/*
 * Auto-detection. Each drive type has a zero-terminated list of formats which
 * are used in succession to try to read the disk. If the FDC cannot lock onto
 * the disk, the next format is tried. This uses the variable 'probing'.
 */
static unsigned char *probe_list[CMOS_MAX] = { p360k, p1200k, p720k, p1440k, NULL };

/* Auto-detection: Disk type used until the next media change occurs. */
struct floppy_struct *current_type[4] = { NULL, NULL, NULL, NULL };

/* This type is tried first. */
static unsigned char *base_type[4] = { NULL, NULL, NULL, NULL };

/*
 * The driver is trying to determine the correct media format
 * while probing is set. rw_interrupt() clears it after a
 * successful access.
 */
static int probing = 0;

/* Prevent "aliased" accesses. */

static int fd_ref[4] = { 0, 0, 0, 0 };	  /* device reference counter */
static int fd_device[4] = { 0, 0, 0, 0 }; /* has the i_rdev used in the last access,
					   * used to detect multiple opens 
					   * via different devices (minor numbers) */

/* Synchronization of FDC access. */
static int format_status = FORMAT_NONE, fdc_busy = 0;
static struct wait_queue fdc_wait;

/* bit vector set when media changed - causes I/O to be discarded until unset */
static unsigned int changed_floppies;
static unsigned int just_opened;

#ifdef INCLUDE_FD_FORMATTING
static struct wait_queue format_done;

/* Errors during formatting are counted here. */
static int format_errors;

/* Format request descriptor. */
static struct format_descr format_req;
#endif

/* Current error count. */
#ifdef INCLUDE_FD_FORMATTING
#define CURRENT_ERRORS (format_status == FORMAT_BUSY ? format_errors : \
    (CURRENT->rq_errors))
#else
#define CURRENT_ERRORS (CURRENT->rq_errors)
#endif

/*
 * Threshold for reporting FDC errors to the console.
 * Setting this to zero may flood your screen when using
 * ultra cheap floppies ;-)
 */
#define ERROR_THRESHOLD	3

/*
 * Rate is 0 for 500kb/s, 1 for 300kbps, 2 for 250kbps
 * Spec1 is 0xSH, where S is stepping rate (F=1ms, E=2ms, D=3ms etc),
 * H is head unload time, time the FDC will wait before unloading
 * the head after a command that accesses the disk (1=16ms, 2=32ms, etc)
 *
 * Spec2 is (HLD<<1 | ND), where HLD is head load time (1=2ms, 2=4 ms etc)
 * and ND is set means no DMA. Hardcoded to 6 (HLD=6ms, use DMA).
 */

/*
 * The block buffer is used for all writes, for formatting and for reads
 * in case track buffering doesn't work or has been turned off.
 */
#define WORD_ALIGNED    __attribute__((aligned(2)))
static char tmp_floppy_area[BLOCK_SIZE] WORD_ALIGNED;

#ifdef CHECK_MEDIA_CHANGE
#define buffer_dirty(b)	((b)->b_dirty)
#endif

// Externals
void finvalidate_inodes(kdev_t, int);

// Internals
static void redo_fd_request(void);
static void recal_interrupt(void);
static void floppy_shutdown(void);
static void motor_off_callback(int);

/*
 * These are global variables, as that's the easiest way to give
 * information to interrupts. They are the data used for the current
 * request.
 */
#define NO_TRACK 255

static int read_track = 0;	/* set to read entire track 
				 * (or cylinder, cache space allowing) */
static int cache_track = -1;
static int cache_drive = -1;
static int cur_spec1 = -1;
static int cur_rate = -1;
static struct floppy_struct *floppy = floppy_type;
static unsigned char current_drive = 255;
static unsigned char sector = 0;
static unsigned char head = 0;
static unsigned char track = 0;
static unsigned char seek_track = 0;
static unsigned char current_track = NO_TRACK;
static unsigned char command = 0;
static unsigned char fdc_version = FDC_TYPE_STD;	/* FDC version code */
#ifdef ENABLE_FDC_82077
static int cur_config = -1;	/* 82077 only, last used configuration byte */
#endif

void ll_rw_block(int, int, struct buffer_head **);
static void floppy_ready(void);
void floppy_release(struct inode *, struct file *);

static void delay_loop(int cnt)
{
    while (cnt-- > 0) asm("nop");
}

static void select_callback(int unused)
{
    DEBUG("SC;");
    floppy_ready();
}

static struct timer_list select = { NULL, 0, 0, select_callback };
/*
 * Select drive for this op - always one at a time, leave motor_on alone,
 * several drives can have the motor running at the same time. A drive cannot
 * be selected unless the motor is on, they can be set concurrently.
 */
/*
 * NOTE: The argument (nr) is silently ignored, current_drive being used instead.
 * is this OK?
  */
static void floppy_select(unsigned int nr)
{
    DEBUG("sel0x%x-", current_DOR);
    if (current_drive == (current_DOR & 3)) {
	/* Drive already selected, we're ready to go */
	floppy_ready();
	return;
    }
    /* activate a different drive */
    seek = 1;
    current_track = NO_TRACK;
    /* 
     * NOTE: This is drive select, not motor on/off
     * Unless the motor is turned on, select will fail 
     * Setting them concurrently is OK
     */
    current_DOR &= 0xFC;
    current_DOR |= current_drive;
    outb(current_DOR, FD_DOR);

    /* It is not obvious why select should take any time at all ... HS */
    /* The select_callback calls floppy_ready() */
    /* The callback is NEVER called unless several drives are active concurrently */
    del_timer(&select);
    select.tl_expires = jiffies + 2;
    add_timer(&select);
}

static void motor_on_callback(int nr)
{
    DEBUG("mON,");
    clr_irq();	// Experimental
    running |= 0x10 << nr;
    set_irq();
    floppy_select(nr);
}

static struct timer_list motor_on_timer[4] = {
    {NULL, 0, 0, motor_on_callback},
    {NULL, 0, 1, motor_on_callback},
    {NULL, 0, 2, motor_on_callback},
    {NULL, 0, 3, motor_on_callback}
};
static struct timer_list motor_off_timer[4] = {
    {NULL, 0, 0, motor_off_callback},
    {NULL, 0, 1, motor_off_callback},
    {NULL, 0, 2, motor_off_callback},
    {NULL, 0, 3, motor_off_callback}
};
static struct timer_list fd_timeout = {NULL, 0, 0, floppy_shutdown};

static void motor_off_callback(int nr)
{
    unsigned char mask = ~(0x10 << nr);

    DEBUG("[%u]MOF%d", (unsigned int)jiffies, nr);
    clr_irq();
    running &= mask;
    current_DOR &= mask;
    outb(current_DOR, FD_DOR);
    set_irq();
}

/*
 * floppy_on: turn on motor. If already running, call floppy_select
 * otherwise start motor and set timer and return.
 * In the latter case, motor_on_callback will call floppy_select();
 *
 * DOR (Data Output Register) is |MOTD|MOTC|MOTB|MOTA|DMA|/RST|DR1|DR0|
 */
static void floppy_on(int nr)
{
    unsigned char mask = 0x10 << nr;	/* motor on select */

    DEBUG("flpON");
    del_timer(&motor_off_timer[nr]);

    if (mask & running) {
	floppy_select(nr);
	DEBUG("#%x;",current_DOR);
	return;	
    }

    if (!(mask & current_DOR)) {	/* motor not running yet */
	del_timer(&motor_on_timer[nr]);
	motor_on_timer[nr].tl_expires = jiffies + HZ/2;	/* TEAC 1.44M says 'waiting time' 505ms,
							 * may be too little for 5.25in drives. */
	add_timer(&motor_on_timer[nr]);

	current_DOR &= 0xFC;	/* remove drive select */
	current_DOR |= mask;	/* set motor select */
	current_DOR |= nr;	/* set drive select */
	outb(current_DOR, FD_DOR);
    }

    DEBUG("flpon0x%x;", current_DOR);
}

/* floppy_off (and floppy_on) are called from upper layer routines when a
 * block request is started or ended respectively. It's extremely important
 * that the motor off timer is slow enough not to affect performance. 3 
 * seconds is a fair compromise.
 */
static void floppy_off(unsigned int nr)
{
    del_timer(&motor_off_timer[nr]);
    motor_off_timer[nr].tl_expires = jiffies + 3 * HZ;
    add_timer(&motor_off_timer[nr]);
    DEBUG("flpOFF%d\n", nr);
}

void request_done(int uptodate)
{
    del_timer(&fd_timeout);

#ifdef INCLUDE_FD_FORMATTING
    if (format_status != FORMAT_BUSY)
	end_request(uptodate);
    else {
	format_status = uptodate ? FORMAT_OKAY : FORMAT_ERROR;
	wake_up(&format_done);
    }
#else
    end_request(uptodate);
#endif
}

#ifdef CHECK_MEDIA_CHANGE
/*
 * This routine checks whether a removable media has been removed/changed,
 * and invalidates all inode and buffer-cache-entries in that case.
 * It is called from device open, mount and file read/write code.
 * Because the FDC is interrupt driven and can't sleep, this routine
 * must be called by a non-interrupt routine, as invalidate_buffers
 * may sleep, and otherwise all accessed kernel variables would need
 * protection via interrupt disabling.
 *
 * Since this driver is the only driver implementing media change,
 * the entire routine has been moved here for simplicity.
 */

static int media_check = 0;	/* FIXME: temporary, avoid reentry */
int check_disk_change(kdev_t dev)
{
    unsigned int mask;
    struct super_block *s;
    struct inode *inodep;

    printk("check_disk_change dev %04x/%d: ", dev, media_check);
    if (!changed_floppies || media_check) return 0;	/* fast exit */
    if (!dev) {
        dev = (changed_floppies & 1)? MKDEV(MAJOR_NR, 0): MKDEV(MAJOR_NR, 1);
    } else {
        if (MAJOR(dev) != MAJOR_NR)
            return 0;
    }
    media_check++; 	/* prevent reentry */
    DEBUG("Ch%d", dev);
    mask = 1 << DEVICE_NR(dev);
    if (!(changed_floppies & mask)) {
        DEBUG("N;");
	media_check = 0;
        return 0;
    }
    DEBUG("Y;");

    if (dev == ROOT_DEV) panic("Root media gone");

    /* I/O is ignored but inuse, locked, or dirty inodes may not be cleared... */
    s = get_super(dev);
    int retval = 0;
    printk("fsync;");
    fsync_dev(dev);	/* do_umount also does fsync, but it doesn't hurt ... */
    if (s && s->s_mounted) {
        do_umount(dev);
	finvalidate_inodes(dev, 1);	/* force purging all inodes, 
					 * otherwise do_umount will not work */
        printk("VFS: Media removed, forced unmount of %s\n", s->s_mntonname);
	retval = 1;
    }
        /* fake up inode to enable device release */
        inodep = new_inode(NULL, S_IFBLK);
        inodep->i_rdev = dev;
	printk("fake inode: %04x\n", inodep);
        floppy_release(inodep, NULL);
        iput(inodep);
#if 0
    } else {
	printk("inv_i;");
	invalidate_inodes(dev);
	printk("inv_bufs\n");
	invalidate_buffers(dev);
    }
#endif

    //changed_floppies &= ~mask;	/* clear media change flag */
    //recalibrate = 1;
    DEBUG("VFS: Media removal cleanup completed on dev %D\n", dev);

    media_check = 0;
    return retval;
}
#endif /* CHECK_MEDIA_CHANGE */

#ifdef CHECK_MEDIA_CHANGE_XXXX
/*
 * The check_media_change entry in struct file_operations (fs.h) is not
 * part of the 'normal' setup (only BLOAT_FS), so we're ignoring it for now,
 * assuming the user is smart enough to umount before media changes - or
 * ready for the consequences.
 */ 

/*
 * floppy-change is never called from an interrupt, so we can relax a bit
 * here, sleep etc. Note that floppy_on tries to set current_DOR to point
 * to the desired drive, but it will probably not survive the sleep if
 * several floppies are used at the same time: thus the loop.
 */
static unsigned int changed_floppies = 0, fake_change = 0;

int floppy_change(struct buffer_head *bh)
{
    unsigned int mask = 1 << ((bh->b_dev & 0x03) >> MINOR_SHIFT;

    if (MAJOR(bh->b_dev) != MAJOR_NR) {
	printk("floppy_change: not a floppy\n");
	return 0;
    }
    if (fake_change & mask) {
	cache_track = -1;
	fake_change &= ~mask;
/* omitting the next line breaks formatting in a horrible way ... */
	changed_floppies &= ~mask;
	return 1;
    }
    if (changed_floppies & mask) {
	cache_track = -1;
	changed_floppies &= ~mask;
	recalibrate = 1;
	return 1;
    }
    if (!bh)
	return 0;
    if (buffer_dirty(bh))
	ll_rw_block(WRITE, 1, &bh);
    else {
	cache_track = -1;
	mark_buffer_uptodate(bh, 0);
	ll_rw_block(READ, 1, &bh);
    }
    wait_on_buffer(bh);
    if (changed_floppies & mask) {
	changed_floppies &= ~mask;
	recalibrate = 1;
	return 1;
    }
    return 0;
}
#endif

static void setup_DMA(void)
{
    unsigned long dma_addr;
    unsigned int count, physaddr;
    int use_xms;
    struct request *req = CURRENT;

#pragma GCC diagnostic ignored "-Wshift-count-overflow"
    use_xms = req->rq_seg >> 16;
    physaddr = (req->rq_seg << 4) + (unsigned int)req->rq_buffer;

    count = nr_sectors<<9;
    if (use_xms || (physaddr + (unsigned int)count) < physaddr)
	dma_addr = LAST_DMA_ADDR + 1;	/* force use of bounce buffer */
    else
	dma_addr = _MK_LINADDR(req->rq_seg, req->rq_buffer);

    DEBUG("setupDMA ");

#ifdef INCLUDE_FD_FORMATTING
    if (command == FD_FORMAT) {
	dma_addr = _MK_LINADDR(kernel_ds, tmp_floppy_area);
	count = floppy->sect * 4;
    }
#endif
    if (read_track) {	/* mark track cache bad, in case all this fails.. */
	cache_drive = cache_track = -1;
#ifdef ENABLE_CYLBUF
	if (floppy->sect <= MAX_BUFFER_SECTORS/2)
		count = floppy->sect << 10;	/* cache cylinder, track * 2 */
        else {
#else
	if (floppy->sect <= MAX_BUFFER_SECTORS) {
#endif
		count = floppy->sect << 9;	/* sects/trk (one side) times 512 */
		if (floppy->sect & 1 && !head)
	    		count += 512; /* add one if head=0 && sector count is odd */
	}

	dma_addr = _MK_LINADDR(DMASEG, 0);
    } else if (dma_addr >= LAST_DMA_ADDR) {
	dma_addr = _MK_LINADDR(kernel_ds, tmp_floppy_area); /* use bounce buffer */
	if (command == FD_WRITE)
	    xms_fmemcpyw(tmp_floppy_area, kernel_ds, req->rq_buffer, req->rq_seg, BLOCK_SIZE/2);
    }
    DEBUG("%d/%lx;", count, dma_addr);
    clr_irq();
    disable_dma(FLOPPY_DMA);
    clear_dma_ff(FLOPPY_DMA);
    set_dma_mode(FLOPPY_DMA,
		 (command == FD_READ) ? DMA_MODE_READ : DMA_MODE_WRITE);
    set_dma_addr(FLOPPY_DMA, dma_addr);
    set_dma_count(FLOPPY_DMA, count);
    enable_dma(FLOPPY_DMA);
    set_irq();
}

static void output_byte(char byte)
{
    int counter;
    unsigned char status;

    if (reset)
	return;
    for (counter = 0; counter < 10000; counter++) {
	status = inb_p(FD_STATUS) & (STATUS_READY | STATUS_DIR);
	if (status == STATUS_READY) {
	    outb(byte, FD_DATA);
	    return;
	}
    }
    current_track = NO_TRACK;
    reset = 1;
    printk("Unable to send byte to FDC\n");
}

static int result(void)
{
    int i = 0, counter, status;

    if (reset)
	return -1;
    for (counter = 0; counter < 10000; counter++) {
	status = inb_p(FD_STATUS) & (STATUS_DIR | STATUS_READY | STATUS_BUSY);
	if (status == STATUS_READY) {	/* done, no more result bytes */
	    return i;
	}
	if (status == (STATUS_DIR | STATUS_READY | STATUS_BUSY)) {
	    if (i >= MAX_REPLIES) {
		printk("floppy_stat reply overrun\n");
		break;
	    }
	    reply_buffer[i++] = inb_p(FD_DATA);
	}
    }
    reset = 1;
    current_track = NO_TRACK;
    printk("df: Result phase timeout\n");
    return -1;
}

static void bad_flp_intr(void)
{
    int errors;

    DEBUG("bad_flpI-");
    current_track = NO_TRACK;
#ifdef INCLUDE_FD_FORMATTING
    if (format_status == FORMAT_BUSY)
	errors = ++format_errors;
    else 
#endif
    if (!CURRENT) {
	printk("%s: no current request\n", DEVICE_NAME);
	reset = recalibrate = 1;
	return;
    } else
	errors = ++CURRENT->rq_errors;
    if (errors > MAX_ERRORS) {
	request_done(0);
    }
    if (errors > MAX_ERRORS / 2)
	reset = 1;
    else
	recalibrate = 1;
}

#ifdef SUPPORT_2880K

/* Set perpendicular mode as required, based on data rate, if supported.
 * 82077 Untested! 1Mbps data rate only possible with 82077-1.
 * TODO: increase MAX_BUFFER_SECTORS, add floppy_type entries.
 */
static void perpendicular_mode(unsigned char rate)
{
    if (fdc_version == FDC_TYPE_82077) {
	output_byte(FD_PERPENDICULAR);
	if (rate & 0x40) {
	    unsigned char r = rate & 0x03;
	    if (r == 0)
		output_byte(2);	/* perpendicular, 500 kbps */
	    else if (r == 3)
		output_byte(3);	/* perpendicular, 1Mbps */
	    else {
		printk("%s: Invalid data rate for perpendicular mode!\n",
		       DEVICE_NAME);
		reset = 1;
	    }
	} else
	    output_byte(0);	/* conventional mode */
    } else {
	if (rate & 0x40) {
	    printk("%s: perpendicular mode not supported by this FDC.\n",
		   DEVICE_NAME);
	    reset = 1;
	}
    }
}				/* perpendicular_mode */
#endif

/*
 * In case you have a 82077 and want to test it, you'll have to compile
 * with `ENABLE_FDC_82077' defined. You may also want to add support for
 * recognizing drives with vertical recording support. Use #define SUPPORT_2880K
 * for that and add data for the 2880k format in the type tables.
 *
 * Notice: Polling is left on for backward compatibility (it is always on with 
 *	   the NEC765/i8272).
 * Notice: Implied seek is now enabled if the 82077 is detected, eliminating 
 *	   seek/seek interrupt ahead of read/writes. Using implied seek, the
 *	   QEMU problem with 360 drives and seeks > logical track 40 goes away.
 *	   On real systems it's the other way: Implied seeks must be TURNED OFF
 *	   for 360k in 1.2M drive.
 */
static void configure_fdc_mode(void)
{
#ifdef ENABLE_FDC_82077
    unsigned char cfg;

    //DEBUG("Conf;");
    if (floppy->stretch && !running_qemu) cfg = 0x0A;
    else cfg = 0x4A;
    if (cfg != cur_config) need_configure = 1;
    if (need_configure && (fdc_version == FDC_TYPE_82077)) {
	/* Enhanced version with FIFO, vertical recording & more. */
	output_byte(FD_CONFIGURE);
	output_byte(0);
	output_byte(cfg);
				/* 0x0A FIFO on, polling on, 10 byte threshold,
				 * 0x4A add EIS - Enable Implied Seek,
				 * EIS must be off for 360k in 1.2M drive unless
				 * QEMU, then it's mandatory!  */
	output_byte(0);		/* precompensation from track 0 upwards */
	need_configure = 0;
	cur_config = cfg;
    }
#endif
    if (cur_spec1 != floppy->spec1) {
	cur_spec1 = floppy->spec1;
	output_byte(FD_SPECIFY);
	output_byte(cur_spec1);	/* hut etc */
	output_byte(6);		/* Head load time =6ms, DMA */
    }
    if (cur_rate != floppy->rate) {
	/* use bit 6 of floppy->rate to indicate perpendicular mode */
#ifdef SUPPORT_2880K
	perpendicular_mode(floppy->rate);
#endif
	outb_p((cur_rate = (floppy->rate)) & ~0x40, FD_DCR);
    }
}				/* configure_fdc_mode */

static void tell_sector(int nr)
{
    if (nr != 7) {
	printk(" -- FDC reply error");
	reset = 1;
    } else
	printk(": track %d, head %d, sector %d", reply_buffer[3],
	       reply_buffer[4], reply_buffer[5]);
}				/* tell_sector */

/*
 * Ok, this interrupt is called after a DMA read/write has succeeded
 * or failed, so we check the results, and copy any buffers.
 * hhb: Added better error reporting.
 */
static void rw_interrupt(void)
{
    unsigned char *buffer_area;
    int nr;
    char bad;

    nr = result();
    /* NOTE: If read_track is active and sector count is uneven, ST0 will
     * always show HD1 as selected at this point. */
    DEBUG("rwI%x|%x|%x-",ST0,ST1,ST2);

    /* check IC to find cause of interrupt */
    switch ((ST0 & ST0_INTR) >> 6) {
    case 1:			/* error occured during command execution */
	bad = 1;
	if (ST1 & ST1_WP) {
	    printk("%s: Drive %d is write protected", DEVICE_NAME,
		   current_drive);
	    request_done(0);
	    bad = 0;
	} else if (ST1 & ST1_OR) {
	    if (ST0 & ST0_DS)
		printk("%s: Over/Under-run - retrying", DEVICE_NAME);
	    /* could continue from where we stopped, but ... */
	    bad = 0;
	} else if (CURRENT_ERRORS > ERROR_THRESHOLD) {
	    printk("%s%d: ", DEVICE_NAME, ST0 & ST0_DS);
	    if (ST0 & ST0_ECE) {
		printk("Recalibrate failed!");
	    } else if (ST2 & ST2_CRC) {
		printk("data CRC error");
		tell_sector(nr);
	    } else if (ST1 & ST1_CRC) {
		printk("CRC error");
		tell_sector(nr);
	    } else if ((ST1 & (ST1_MAM | ST1_ND)) || (ST2 & ST2_MAM)) {
		if (!probing) {
		    printk("sector not found");
		    tell_sector(nr);
		} else
		    printk("probe failed...");
	    } else if (ST2 & ST2_WC) {	/* seek error */
		printk("wrong cylinder");
	    } else if (ST2 & ST2_BC) {	/* cylinder marked as bad */
		printk("bad cylinder");
	    } else {
		printk("unknown error. ST[0-3]: 0x%x 0x%x 0x%x 0x%x",
		       ST0, ST1, ST2, ST3);
	    }
	    printk("\n");
	    CURRENT->rq_errors *= 2;
	    //CURRENT->rq_errors++; /* may want to increase this even more, doesn't make */ 
	    		/* sense to re-try most of these conditions more 
			 * than the reporting threshold. */
			/* FIXME: Need smarter retry/error reporting scheme */
	}
	if (bad)
	    bad_flp_intr();
	redo_fd_request();
	return;
    case 2:			/* invalid command given */
	printk("%s: Invalid FDC command given!\n", DEVICE_NAME);
	request_done(0);
	return;
    case 3:
	printk("%s: Abnormal termination caused by polling\n", DEVICE_NAME);
	bad_flp_intr();
	redo_fd_request();
	return;
    default:			/* (0) Normal command termination */
	break;
    }

    if (probing) {
	int drive = (MINOR(CURRENT->rq_dev) >> MINOR_SHIFT) & 3;

	printk("df%d: Auto-detected floppy type %s\n", drive, floppy->name);
	current_type[drive] = floppy;
#ifdef BDEV_SIZE_CHK
	floppy_sizes[drive] = floppy->size >> 1;
#endif
	probing = 0;
    }
    if (read_track) {
	cache_drive = current_drive;
#ifdef ENABLE_CYLBUF
	if (floppy->sect <= MAX_BUFFER_SECTORS/2) {
	    buffer_area = (unsigned char *)(((floppy->sect * head) + sector) << 9);
	    cache_track = (seek_track << 1); 	/* full cylinder cache */
	} else
#endif
	{
	    buffer_area = (unsigned char *)(sector << 9);
	    cache_track = (seek_track << 1) + head; /* track cache */
	}
	DEBUG("rd:%04x:%04x->%04lx:%04x;", DMASEG, buffer_area,
		(unsigned long)CURRENT->rq_seg, CURRENT->rq_buffer);
	xms_fmemcpyw(CURRENT->rq_buffer, CURRENT->rq_seg, buffer_area, DMASEG, BLOCK_SIZE/2);
    } else if (command == FD_READ 
#ifndef CONFIG_FS_XMS_BUFFER
	   && _MK_LINADDR(CURRENT->rq_seg, CURRENT->rq_buffer) >= LAST_DMA_ADDR
#endif
	) {
	/* if the dest buffer is out of reach for DMA (always the case if using
	 * XMS buffers) we need to read/write via the bounce buffer */
	xms_fmemcpyw(CURRENT->rq_buffer, CURRENT->rq_seg, tmp_floppy_area, kernel_ds, BLOCK_SIZE/2);
	printk("directfd: illegal buffer usage, rq_buffer %04x:%04x\n", 
		CURRENT->rq_seg, CURRENT->rq_buffer);
    }
    request_done(1);
    //printk("RQOK;");
    redo_fd_request();	/* Continue with the next request - if any */
}

/*
 * We read tracks - or  a full cylinder if possible (and configured on). 
 * If we get too many errors, we go back to
 * reading just one sector at a time. This means we should be able to
 * read a sector even if there are other bad sectors on this track.
 *
 * The FDC will start read/write at the specified sector, then continues
 * until the DMA controller tells it to stop ... as long as we're on the same cyl.
 * Notably: If the # of sectors per track is odd, we read sectors + 1 if head = 0
 * to ensure we have full blocks in the cache.
 * (with full cylinder cache active on low dens drives, this applies to 1.2M only.)
 *
 * From the Intel 8272A app note: "The 8272A always operates in a multi-sector 
 * transfer mode. It continues to transfer data until the TC input is active."
 * IOW: We tell the FDC where to start, and the DMA controller where to stop.
 */
void setup_rw_floppy(void)
{
    unsigned char hd;

    DEBUG("setup_rw%d-",track);
    setup_DMA();
#ifdef ENABLE_CYLBUF
    hd = (read_track && (floppy->sect <= MAX_BUFFER_SECTORS/2)) ? 0 : head;
#else
    hd = head;
#endif
    do_floppy = rw_interrupt;
    output_byte(command);
    output_byte(hd << 2 | current_drive);

    if (command != FD_FORMAT) {
	output_byte(track);
	output_byte(hd);
	if (read_track)
	    output_byte(1);	/* always start at 1 */
	else
	    output_byte(sector+1); 

	output_byte(2);		/* sector size = 512 */
	output_byte(floppy->sect);
	output_byte(floppy->gap);
	output_byte(0xFF);	/* sector size, 0xff unless sector size==0 (128b) */
    } else {
	output_byte(2);		/* sector size = 512 */
	output_byte(floppy->sect * 2); /* sectors per cyl */
	output_byte(floppy->fmt_gap);
	output_byte(FD_FILL_BYTE);
    }
    if (reset)		/* If output_byte timed out */
	redo_fd_request();
}

static void seek_interrupt(void)
{
    /* get interrupt status */
    output_byte(FD_SENSEI);
    DEBUG("seekI%d-",ST1);
    if (result() != 2 || (ST0 & 0xF8) != 0x20 || ST1 != seek_track) {
	printk("%s%d: seek failed - %d/%d\n", DEVICE_NAME, current_drive, seek_track, ST1);
	recalibrate = 1;
	bad_flp_intr();
	redo_fd_request();
	return;
    }
    current_track = seek_track;
    setup_rw_floppy();
}

/*
 * This routine is called when everything should be correctly set up
 * for the transfer (ie floppy motor is on and the correct floppy is
 * selected, error conditions cleared).
 */
static void transfer(void)
{
    read_track = (command == FD_READ) && (CURRENT_ERRORS < 4) &&
	(floppy->sect <= MAX_BUFFER_SECTORS);
    DEBUG("trns%d-", read_track);

    configure_fdc_mode();	/* Make sure the controller is in the right mode,
				 * may change between every transaction when 
				 * doing floppy-floppy copying */

    if (reset) {		/* if there was an output_byte timeout in */
	redo_fd_request();	/* configure_fdc_mode */
	return;
    }
    if (!seek) {
	setup_rw_floppy();
	return;
    }

    if (fdc_version != FDC_TYPE_82077 || (floppy->stretch && !running_qemu)) {
    	/* OK; need to change tracks 'manually' ... */
    	do_floppy = seek_interrupt;
    	DEBUG("sk%d;",seek_track);
    	output_byte(FD_SEEK);
    	output_byte((head << 2) | current_drive);
    	output_byte(seek_track);
    	if (reset)		/* If something happened in output_byte() */
		redo_fd_request();
    } else { 			/* implied seek */
	current_track = seek_track;
	setup_rw_floppy();
    }
}

static void recalibrate_floppy(void)
{
    DEBUG("recal-");
    recalibrate = 0;
    current_track = 0;
    do_floppy = recal_interrupt;
    output_byte(FD_RECALIBRATE);
    output_byte(current_drive);
#if 0
    /* this may not make sense: We're waiting for recal_interrupt
     * why redo_fd_request here when recal_interrupt is doing it ??? */
    /* 'reset' gets set in recal_interrupt, maybe that's it ??? */
    if (reset)
	redo_fd_request();
#endif
}

/*
 * Special case - used after a unexpected interrupt (or reset)
 */

static void recal_interrupt(void)
{
    output_byte(FD_SENSEI);
    current_track = NO_TRACK;
    if (result() != 2 || (ST0 & 0xD8)) /* look for any error bit */
	reset = 1;
    DEBUG("recalI-%x", ST0);	/* Should be 0x2X, Seek End */
    /* Recalibrate until track 0 is reached. Might help on some errors. */
    if (ST0 & 0x10)		/* Look for UnitCheck, which will happen regularly
    				 * on 80 track drives because RECAL only steps 77 times */
	recalibrate_floppy();	/* FIXME: may loop, should limit nr of recalibrates */
    else
	redo_fd_request();
}

static void unexpected_floppy_interrupt(void)
{
    current_track = NO_TRACK;
    output_byte(FD_SENSEI);
    printk("%s: unexpected interrupt\n", DEVICE_NAME);
    if (result() != 2 || (ST0 & 0xE0) == 0x60)
	reset = 1;
    else
	recalibrate = 1;
}

/*
 * Must do 4 FD_SENSEI's after reset - one per drive - because of ``drive polling''.
 */
static void reset_interrupt(void)
{
    short i;

    DEBUG("rstI-");
    for (i = 0; i < 4; i++) {
	output_byte(FD_SENSEI);
	(void) result();
    }
    //DEBUG("1-");
    output_byte(FD_SPECIFY);
    output_byte(cur_spec1);	/* hut etc */
    output_byte(6);		/* Head load time =6ms, DMA */
    //DEBUG("2-");
    configure_fdc_mode();	/* reprogram fdc */
    if (initial_reset_flag) {
	initial_reset_flag = 0;
	recalibrate = 1;
	reset = 0;
	return;
    }
    if (!recover)
	redo_fd_request();
    else {
	recalibrate_floppy();
	recover = 0;
    }
}

/*
 * reset is done by pulling bit 2 of DOR low for a while.
 * As the FDC recovers conciousness, it pulls the IRQ, and
 * reset_interrupt (above) gets called.
 */
static void reset_floppy(void)
{

    DEBUG("[%u]rst-", (unsigned int)jiffies);
    do_floppy = reset_interrupt;
    reset = 0;
    current_track = NO_TRACK;
    cur_spec1 = -1;
    cur_rate = -1;
    recalibrate = 1;
    need_configure = 1;	/* FIXME - not required if LOCK is set on 82077 */
    //if (!initial_reset_flag)
	//printk("Reset-floppy called\n");
    clr_irq();
    outb_p(current_DOR & ~0x04, FD_DOR);
    delay_loop(1000);
    outb(current_DOR, FD_DOR);
    set_irq();
}

/* 
 * shutdown is called by the 'main' watchdog timer (typically 6 secs of idle time)
 * and sets the 'recover' flag to enable a full restart of the adapter: Reset FDC,
 * re-configure, recalibrate, essentially start from scratch.
 */
static void floppy_shutdown(void)
{
    printk("[%u]shtdwn0x%x|%x-", (unsigned int)jiffies, current_DOR, running);
    do_floppy = NULL;
    request_done(0);
    recover = 1;
    reset_floppy();
    redo_fd_request();
}

#ifdef USE_DIR_REG
static void shake_done(void)
{
    output_byte(FD_SENSEI);	/* clear interrupt */
    result();
    DEBUG("shD0x%x-", ST0);
    current_track = NO_TRACK;
    if (MEDIA_CHANGED || ST0 & 0xD8)
	request_done(0);	/* still errors: proably no media in drive,
				 * just exit */
    //else
	redo_fd_request();	// let redo_fd_request clean up
}

static int retry_recal(void (*proc)())
{
    output_byte(FD_SENSEI);
    DEBUG("rrecal-");
    //if (result() == 2 && (ST0 & 0x10) != 0x10) /* track 0 test */
    /* Check all error bits just in case */
    if (result() == 2 && !(ST0 & 0xD8)) { /* Check IC1, IC0, UC, NR */
	DEBUG("|%02x|", ST0);
	return 0;
    }
    DEBUG("/%02x/", ST0);
    do_floppy = proc;		/* otherwise repeat recal */
    output_byte(FD_RECALIBRATE);
    output_byte(current_drive);
    return 1;
}

static void shake_zero(void)
{
    DEBUG("sh0-");
    if (!retry_recal(shake_zero))
	shake_done();
}

static void shake_one(void)
{
    DEBUG("sh1-");
    if (retry_recal(shake_one))
	return;	/* just return if retry_recal initiated a RECAL */
    do_floppy = shake_done;
    output_byte(FD_SEEK);
    output_byte(head << 2 | current_drive);
    output_byte(1);	/* Resetting the media change bit requires head movement */
}
#endif		/* USE_DIR_REG */

static void floppy_ready(void)
{
    unsigned int mask = (1 << current_drive);

    DEBUG("RDY0x%x,%d,%d-", MEDIA_CHANGED, reset, recalibrate);
#ifdef USE_DIR_REG
    if (MEDIA_CHANGED) {
	cache_track = -1;	/* MAYBE NOT */
        if (current_type[current_drive] && !(just_opened & mask)) {
            changed_floppies |= (1 << current_drive); /* this will discard any queued I/O */
            printk("df%d: Drive was opened, purge pending I/O\n", current_drive);
            current_type[current_drive] = NULL;     /* comment out to keep last media format */
#ifdef BDEV_SIZE_CHK
	    floppy_sizes[current_drive] = MAX_DISK_SIZE;
#endif
	}

	if (!reset && !recalibrate) {
	    if (current_track && current_track != NO_TRACK)
		do_floppy = shake_zero;
	    else
		do_floppy = shake_one;
	    output_byte(FD_RECALIBRATE);
	    output_byte(current_drive);
	    return;
	}
    }
#endif
    just_opened &= ~mask;

    if (reset) {
	reset_floppy();
	return;
    }
    if (recalibrate) {
	recalibrate_floppy();
	return;
    }
    transfer();
}

#ifdef INCLUDE_FD_FORMATTING
static void setup_format_params(void)
{
    unsigned char *here = (unsigned char *) tmp_floppy_area;
    int count, head_shift, track_shift, total_shift;

    /* allow for about 30ms for data transport per track */
    head_shift = floppy->sect / 6;
    /* a ``cylinder'' is two tracks plus a little stepping time */
    track_shift = 2 * head_shift + 1;
    /* count backwards */
    total_shift = floppy->sect -
	((track_shift * track + head_shift * head) % floppy->sect);

    /* XXX: should do a check to see this fits in tmp_floppy_area!! */
    for (count = 0; count < floppy->sect; count++) {
	*here++ = track;
	*here++ = head;
	*here++ = 1 + ((count + total_shift) % floppy->sect);
	*here++ = 2;		/* 512 bytes */
    }
}
#endif

static void redo_fd_request(void)
{
    unsigned int start;
    struct request *req;
    int device, drive;

  repeat:
    req = CURRENT;
    DEBUG("REDO:%04x;", req);
#ifdef INCLUDE_FD_FORMATTING
    if (format_status == FORMAT_WAIT)
	format_status = FORMAT_BUSY;
    if (format_status != FORMAT_BUSY)
#endif
    {
	if (!req) {
	    if (fdc_busy) {
		fdc_busy = 0;
		wake_up(&fdc_wait);
	    }
	    CLEAR_INTR;
	    return;
	}
	CHECK_REQUEST(req);
    }
    seek = 0;
    device = MINOR(req->rq_dev) >> MINOR_SHIFT;
    probing = 0;
    drive = device & 3;

#ifdef CHECK_MEDIA_CHANGE
    if (changed_floppies & (1 << drive)) {
        req->rq_errors = -1;    /* stop error display */
        request_done(0);
        goto repeat;
    }
#endif

    if (device > 3)		/* get type from minor device # */
	floppy = &floppy_type[device >> 2];
    else {			/* Auto-detection */
	floppy = current_type[drive];
	if (!floppy) {
	    if (!base_type[drive]) {	/* Access to non-existent drive */
		request_done(0);
		goto repeat;
	    }
	    probing = 1;
	    need_configure = 1;	/* Maybe 'probing' can replace 'need_configure' */
	    floppy = &floppy_type[base_type[drive][CURRENT_ERRORS & 1]];
	}
    }

    DEBUG("[%u]redo-%c %d(%s) bl %u;", (unsigned int)jiffies, 
		req->rq_cmd == WRITE? 'W':'R', device, floppy->name, req->rq_blocknr);
    debug_blkdrv("df[%04x]: %c blk %ld\n", req->rq_dev, req->rq_cmd==WRITE? 'W' : 'R',
		req->rq_blocknr);
    if (format_status != FORMAT_BUSY) {
    	unsigned int tmp;
	if (current_drive != drive) {
	    current_track = NO_TRACK;
	    current_drive = drive;
	}

	raw = !!req->rq_nr_sectors;	/* raw IO if rq_nr_sectors > 0 */
	nr_sectors = 2;			/* Default: Block IO - 1k blocks */
	if (raw)
		nr_sectors = req->rq_nr_sectors;;
	start = (unsigned int) req->rq_blocknr;	/* rq_blocknr is ALWAYS sectors */
	if (start + nr_sectors > floppy->size) {
	    if (!raw || start >= floppy->size) {
		request_done(0);
		goto repeat;
	    } else
		nr_sectors = floppy->size - start;
	}
	sector = start % floppy->sect;
	tmp = start / floppy->sect;
	head = tmp % FLOPPY_HEADS;
	track = tmp / FLOPPY_HEADS;
	/* Ensure raw IO requests have valid # of sectors: Don't span track boundaries */
	/* If running a 82077 you have implied seek, and this exercise is not required */
	if (raw && ((sector + (floppy->sect*head) + nr_sectors)) 
			> (floppy->sect*FLOPPY_HEADS)) {
		nr_sectors = (floppy->sect * FLOPPY_HEADS) - (sector + (floppy->sect * head));
		req->rq_nr_sectors = nr_sectors;	/* return updated # of sectors */
	}
	seek_track = track << floppy->stretch;
	DEBUG("%d:%d:%d:%d; ", start, sector, head, track);
	if (req->rq_cmd == READ)
	    command = FD_READ;
	else if (req->rq_cmd == WRITE)
	    command = FD_WRITE;
	else {
	    printk("redo_fd_request: unknown command\n");
	    request_done(0);
	    goto repeat;
	}
    }
#ifdef INCLUDE_FD_FORMATTING
    else {	/* Format drive */
	if (current_drive != (format_req.device & 3))
	    current_track = NO_TRACK;
	current_drive = format_req.device & 3;
	if (((unsigned) format_req.track) >= floppy->track ||
	    (format_req.head & 0xfffe) || probing) {
	    request_done(0);
	    goto repeat;
	}
	head = format_req.head;
	track = format_req.track;
	seek_track = track << floppy->stretch;
	if ((seek_track << 1) + head == cache_track)
	    cache_track = -1;
	command = FD_FORMAT;
	setup_format_params();
    }
#endif

    /* restart timer for hung operations, 6 secs probably too long ... */
    del_timer(&fd_timeout);
    fd_timeout.tl_expires = jiffies + 6 * HZ;
    add_timer(&fd_timeout);
#ifdef ENABLE_CYLBUF
    int cyl_mask = (floppy->sect <= MAX_BUFFER_SECTORS/2) ? 0 : head; /* full cyl cache */
#else
#define cyl_mask head
#endif

    DEBUG("prep %d|%d,%d|%d-", cache_track, seek_track, cache_drive, current_drive);

    if ((((seek_track << 1) + cyl_mask) == cache_track) && (current_drive == cache_drive)) {
	/* Requested block is in the cache. If reading, go get it.
	 * If the sector count is odd, we cache sectors+1 when head=0 to get 
	 * full blocks. When head=1 we read the entire track
	 * and ignore the first sector.
	 */
	DEBUG("bufrd chs %d/%d/%d\n", seek_track, head, sector);
	unsigned char *buf_ptr;
#ifdef ENABLE_CYLBUF
	if (floppy->sect <= MAX_BUFFER_SECTORS/2) 
		buf_ptr = (unsigned char *)(((floppy->sect * head) + sector) << 9);
	else
#endif
		buf_ptr = (unsigned char *)(sector << 9);

	if (command == FD_READ) {	/* requested data is in cache */
	    xms_fmemcpyw(req->rq_buffer, req->rq_seg, buf_ptr, DMASEG, BLOCK_SIZE/2);
	    request_done(1);
	    goto repeat;
    	} else if (command == FD_WRITE)	/* update track cache */
	    xms_fmemcpyw(buf_ptr, DMASEG, req->rq_buffer, req->rq_seg, BLOCK_SIZE/2);
    } 

    if (seek_track != current_track)
	seek = 1;

    floppy_on(current_drive);
}

void do_fd_request(void)
{
    DEBUG("fdrq:");
    //if (CURRENT) CURRENT->rq_errors = 0;	// EXPERIMENTAL
    while (fdc_busy) {
	printk("df: request while fdc busy\n");
	sleep_on(&fdc_wait);
    }
    fdc_busy = 1;
    redo_fd_request();
}

int fd_ioctl(struct inode *inode,
		    struct file *filp, unsigned int cmd, unsigned int param)
{
    struct floppy_struct *this_floppy;
    int device, drive, err = -EINVAL;
    struct hd_geometry *loc = (struct hd_geometry *) param;

    if (!suser())	/* For now */
	return -EPERM;
    if (!inode || !inode->i_rdev)
	return -EINVAL;
    device = MINOR(inode->i_rdev) >> MINOR_SHIFT;
    drive = device & 3;
    if (device > 3)
	this_floppy = &floppy_type[drive >> 2];
    else if ((this_floppy = current_type[drive]) == NULL)
	return -ENODEV;

    switch (cmd) {
    case HDIO_GETGEO:	/* need this for the sys/makeboot command */
    case FDGETPRM:
	err = verify_area(VERIFY_WRITE, (void *) param, sizeof(struct hd_geometry));
	if (!err) {
	    put_user_char(FLOPPY_HEADS, &loc->heads);
	    put_user_char(this_floppy->sect, &loc->sectors);
	    put_user(this_floppy->track, &loc->cylinders);
	    put_user_long(0L, &loc->start);
	}
	return err;

#ifdef INCLUDE_FD_FORMATTING
    case FDFMTBEG:
	if (!suser())
	    return -EPERM;
	return 0;

    case FDFMTEND:
	if (!suser())
	    return -EPERM;
	clr_irq();
	fake_change |= 1 << drive;
	set_irq();
	cmd = FDCLRPRM;
	break;

    case FDFMTTRK:
	if (fd_ref[drive] != 1)
	    return -EBUSY;
	clr_irq();
	while (format_status != FORMAT_NONE)
	    sleep_on(&format_done);
	memcpy_fromfs((char *)(&format_req),
		    (char *)param, sizeof(struct format_descr));
	format_req.device = device;
	format_status = FORMAT_WAIT;
	format_errors = 0;
	while (format_status != FORMAT_OKAY && format_status != FORMAT_ERROR) {
	    if (fdc_busy)
		sleep_on(&fdc_wait);
	    else {
		fdc_busy = 1;
		redo_fd_request();
	    }
	}
	while (format_status != FORMAT_OKAY && format_status != FORMAT_ERROR)
	    sleep_on(&format_done);
	set_irq();
	err = format_status == FORMAT_OKAY;
	format_status = FORMAT_NONE;
	floppy_off(drive);
	wake_up(&format_done);
	return err ? 0 : -EIO;
#endif	/* INCLUDE_FD_FORMATTING */

    default:
	return -EINVAL;
    }
    return err;
}

static int INITPROC CMOS_READ(int addr)
{
    outb_p(addr, 0x70);
    return inb_p(0x71);
}

static unsigned char * INITPROC find_base(int drive, int type)
{
    unsigned char *base;

    if (type > 0 && type <= CMOS_MAX) {
	if (type == CMOS_2880k) type--;	/* force 2.88 to look like 1.44 */
	base = probe_list[type - 1];
	printk("df%d is %s (%d)%d", drive, floppy_type[*base].name, type, *base);
	return base;
    }
    printk("df%d is unknown type %d", drive, type);
    return NULL;
}

/* CMOS 0x10 bits 7-4: Floppy 1 drive type,
 *           bits 3-0: Floppy 2 drive type, no config for drives 3-4
 *      0x14 bits 7-6: # of drives - 1 
 */
static void INITPROC config_types(void)
{
    int at = sys_caps & CAP_PC_AT;
    printk("Floppy drive(s) [%sCMOS]: ", at ? "" : "no ");
    if (at) {
	base_type[0] = find_base(0, (CMOS_READ(0x10) >> 4) & 0xF);
	if ((CMOS_READ(0x14) >> 6) & 1) {
	    printk(", ");
	    base_type[1] = find_base(1, CMOS_READ(0x10) & 0xF);
	}
    } else {	/* No CMOS, force type 1 & just one drive */
	base_type[0] = probe_list[FT_360k_PC - 1];
	printk("df0 is 360k/PC (1)");
    }
    printk("\n");
}

/*
 * floppy_open check for aliasing (/dev/fd0 can be the same as
 * /dev/PS0 etc), and disallows simultaneous access to the same
 * drive with different device numbers.
 *
 * NOTE: floppy_open is called by the raw floppy driver.
 * FIXME: May want to reject raw access on an open block device.
 */
int floppy_open(struct inode *inode, struct file *filp)
{
    int drive, old_dev, dev;
    int israw = S_ISCHR(inode->i_mode);

    drive = MINOR(inode->i_rdev) >> MINOR_SHIFT;
    dev = drive & 3;
    fd_device[dev] = inode->i_rdev;

    if (!israw) {			/* For block device access only */
	old_dev = fd_device[dev];
    	if (old_dev && old_dev != inode->i_rdev)
	    return -EBUSY;	/* no reopens using different minor */
	fd_ref[dev]++;
	//cache_drive = cache_track = -1;	

	//if (fd_ref[dev] == 1) invalidate_buffers(inode->i_rdev);	/* EXPERIMENTAL */
					/* probably superfluous, done on prev close */
	just_opened |= (1 << dev);
#ifdef CHECK_MEDIA_CHANGE_XXX
	if (filp && filp->f_mode) {
	    if (check_disk_change(inode->i_rdev))
		return -ENXIO;
	}
#endif
    } 

    if (drive > 3)		/* forced floppy type */
	floppy = &floppy_type[drive >> 2];
    else {			/* Auto-detection */
	floppy = current_type[dev];
	if (!floppy) {
	    if (!base_type[drive])
		return -ENXIO;
	    probing = 1;
	    floppy = &floppy_type[base_type[drive][0]];
	}
    }
    inode->i_size = ((sector_t)(floppy->size)) << 9;	/* NOTE: assumes sector size 512 */
    DEBUG("df%d: open %s dev %04x, sz %lu, %s\n", drive, israw?"(raw)":"",
		inode->i_rdev, inode->i_size, floppy->name);

    return 0;
}

void floppy_release(struct inode *inode, struct file *filp)
{
    kdev_t dev = inode->i_rdev;
    int drive = (MINOR(dev) >> MINOR_SHIFT) & 3;

#if 0
    /*DEBUG*/printk("df%d release (i=%04x)", drive, inode);
    DEBUG("\n");
#endif
    if (S_ISCHR(inode->i_mode)) return;/*goto clear_raw;*/ /* change if allowing simlutaneous raw
						 * and block access! FIXME possibly superfluous */
    if (--fd_ref[drive] == 0) {
	fsync_dev(dev);
	invalidate_buffers(dev);
	invalidate_inodes(dev);
//clear_raw:
	changed_floppies &= ~(1 << drive);	/* clear media change flag */
	current_type[drive] = NULL;	/* prevent next open from flagging media change */
    }
}

static struct file_operations floppy_fops = {
    NULL,			/* lseek - default */
    block_read,			/* read - general block-dev read */
    block_write,		/* write - general block-dev write */
    NULL,			/* readdir - bad */
    NULL,			/* select */
    fd_ioctl,			/* ioctl */
    floppy_open,		/* open */
    floppy_release,		/* release */
};

/*
 * The version command is not supposed to generate an interrupt, but
 * my FDC does, except when booting in SVGA screen mode.
 * When it does generate an interrupt, it doesn't return any status bytes.
 * It appears to have something to do with the version command...
 *
 * This should never be called, because of the reset after the version check.
 */
#ifdef ENABLE_FDC_82077
static void ignore_interrupt(void)
{
    printk("%s: weird interrupt ignored (%d)\n", DEVICE_NAME, result());
    reset = 1;
    CLEAR_INTR;			/* ignore only once */
}
#endif

static void floppy_interrupt(int unused, struct pt_regs *unused1)
{
    void (*handler) () = DEVICE_INTR;

    DEVICE_INTR = NULL;
    if (!handler)
	handler = unexpected_floppy_interrupt;
    //printk("$");
    handler();
}

void INITPROC floppy_init(void)
{
#ifdef BDEV_SIZE_CHK
    extern int *blk_size[];
#endif
    int err;

    outb(current_DOR, FD_DOR);	/* all motors off, DMA, /RST  (0x0c) */
    if (register_blkdev(MAJOR_NR, DEVICE_NAME, &floppy_fops)) {
	printk("Unable to get major %d for floppy\n", MAJOR_NR);
	return;
    }
#ifdef BDEV_SIZE_CHK
    blk_size[MAJOR_NR] = floppy_sizes;
#endif
    blk_dev[MAJOR_NR].request_fn = DEVICE_REQUEST;

    config_types();
    err = request_irq(FLOPPY_IRQ, floppy_interrupt, INT_GENERIC);
    if (err) {
	printk("Unable to grab IRQ%d for the floppy driver\n", FLOPPY_IRQ);
	return;	/* should be able to signal failure back to the caller */
    }

    if (request_dma(FLOPPY_DMA, (void *)DEVICE_NAME))
	printk("Unable to grab DMA%d for the floppy driver\n", FLOPPY_DMA);

#ifdef ENABLE_FDC_82077
    /* Try to determine the floppy controller type */
    DEVICE_INTR = ignore_interrupt;	/* don't ask ... */
    output_byte(FD_VERSION);	/* get FDC version code */
    if (result() != 1) {
	printk("%s: FDC failed to return version byte\n", DEVICE_NAME);
	fdc_version = FDC_TYPE_STD;
    } else
	fdc_version = reply_buffer[0];

#else
    fdc_version = FDC_TYPE_STD;	/* force std fdc type; can't test other. */
#endif
    printk("%s: Direct floppy driver, FDC (%s) @ irq %d, DMA %d", DEVICE_NAME, 
	    fdc_version == 0x80 ? "8272A" : "82077", FLOPPY_IRQ, FLOPPY_DMA);
    configure_fdc_mode();
#ifdef USE_DIR_REG
    printk(", DIR active\n");
#else
    printk("\n");
#endif

#ifdef ENABLE_FDC_82077
    /* Not all FDCs seem to be able to handle the version command
     * properly, so force a reset for the standard FDC clones,
     * to avoid interrupt garbage.
     */
    if (fdc_version == FDC_TYPE_STD) {
	initial_reset_flag = 1;
	reset_floppy();
    }
#endif
}

#if 0
/* replace separate DMA handler later - this is much more compact and efficient */

/*===========================================================================*
 *				dma_setup (from minix driver)		     *
 *===========================================================================*/
static void dma_setup(int opcode)
{
/* The IBM PC can perform DMA operations by using the DMA chip.  To use it,
 * the DMA (Direct Memory Access) chip is loaded with the 20-bit memory address
 * to be read from or written to, the byte count minus 1, and a read or write
 * opcode.  This routine sets up the DMA chip.  Note that the chip is not
 * capable of doing a DMA across a 64K boundary (e.g., you can't read a
 * 512-byte block starting at physical address 65520).
 */

  /* Set up the DMA registers.  (The comment on the reset is a bit strong,
   * it probably only resets the floppy channel.)
   */
  outb(DMA_INIT, DMA_RESET_VAL);	/* reset the dma controller */
  outb(DMA_FLIPFLOP, 0);		/* write anything to reset it */
  outb(DMA_MODE, opcode == DEV_SCATTER ? DMA_WRITE : DMA_READ);
  outb(DMA_ADDR, (unsigned) tmp_phys >>  0);
  outb(DMA_ADDR, (unsigned) tmp_phys >>  8);
  outb(DMA_TOP, (unsigned) (tmp_phys >> 16));
  outb(DMA_COUNT, (SECTOR_SIZE - 1) >> 0);
  outb(DMA_COUNT, (SECTOR_SIZE - 1) >> 8);
  outb(DMA_INIT, 2);			/* some sort of enable */
}
#endif
