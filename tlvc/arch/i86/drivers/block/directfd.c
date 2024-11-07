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
 * For timing references, check this: https://wiki.osdev.org/Floppy_Disk_Controller#Motor_Delays
 */

/*
 * TODO (HS 2023):
 * - The driver has many provisions for 4 floppy drives, but except for the oldest PCs
 *   (PC and XT) there is no physical support more than 2 drives (needs a 2nd controller).
 *   Also, this driver uses CMOS settings to set the drive type which applies to drives 0 
 *   and 1 only. 2 and 3 will have base_type[] 0 and ultimately fail. Finally, the 
 *   bootopts 'xtflpy' parameter (currently) accepts only 2 drives.
 * - Are errors counted properly? 
 * - Repeated errors will cause an error message to appear twice: once when the MAXERRORS
 *   threshold is passed, once when giving up and exiting.
 * - Many errors (like sector not found) would benefit from a 'shake', step one track and
 *   back, to ensure we don't have a positioning problem (old drives primarily).
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
#define MAX_FLOPPIES	2	/* number of drives supported, 2 or 4 */

#include "blk.h"

/* This is confusing. DEVICE_INTR is the do_floppy pointer.
 * The code is sometimes using the macro, some times the variable.
 * FIXME: Use one or the other.
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

//#define INCLUDE_FD_FORMATTING	/* Formatting code untested, don't waste the space */

#ifndef CONFIG_HW_PCXT	/* some options are not meaningful on pre-AT systems */
//#define ENABLE_FDC_82077	/* Enable 82077 extras if present, REQUIRED to use 360k media */
			/* in 1.2M drive on QEMU!! */
			/* Unless you need 2880 floppy support there is minimal benefit in enabling
			 * 82077 support. The FIFO is good but doesnt matter (we're not experiencing
			 * over- or under-runs ever), the autoseek will not be used etc.
			 */
//#define USE_DIR_REG	/* Use the DIR register if available for media change detection */
			/* Default is off as the benefits of using it, even for triggering
			 * autoprobes, is questionable. E.g. when I accidentally open the
			 * drive door on a mounted floppy, I'd rather be able to just put
			 * it back than pretending it's possible to save anything - detecting,
			 * fluching, umounting etc. */
			/* Always use off for PC/XT/8086/8088 8bit ISA systems */
#else
#undef CHECK_MEDIA_CHANGE /* Now defined in linuxmt/fs.h */
#endif

#define DEBUG_DIRECTFD 0
#if DEBUG_DIRECTFD
#define DEBUG if (debug_level > 0) printk
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
 * All floppy drives have 2 heads!
 */
#define FLOPPY_HEADS	2

/*
 * Head Load Time [2ms], apparently not critical, we use the same for all drives.
 */
#define HEAD_LOAD_TIME	6

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
/* NOTE: Pre-AT machines have no CMOS, no DIR - should be configured from 
 * /bootopts, will default to 2 type 1 drives */
/* Sep-24: Configured via bootopts (hs) */
#define CMOS_NONE   0
#define CMOS_360k   1
#define CMOS_1200k  2
#define CMOS_720k   3
#define CMOS_1400k  4
#define CMOS_2880k  5
#define CMOS_MAX    5

/* Indices into floppy_type[], used for floppy format probes. Must match the table below */
/* Must also match the forced floppy type by minor dev num - see image/Make.devices */
/* NOTICE that MINOR_SHIFT is now 0 (and gone)! for direct floppies */
#define FT_360k_PC  1		/* 360kB PC diskettes */
#define FT_1200k    2		/* 1.2 MB AT-diskettes */
#define FT_720k     3		/* 3.5" 720kB diskette */
#define FT_360k_AT  4		/* 360kB in 1.2MB drive */
#define FT_720k_AT  5		/* 720kB in 1.44MB drive */
#define FT_1440k    6		/* 3.5" 1.44MB diskette */

/* Data rate codes */
#define R250	2
#define R300	1
#define R500	0
#define R1M	3

/*
 * The 'stretch' tells if the tracks need to be doubled for some
 * types (ie 360kB diskette in 1.2MB drive). 
 * The HUT (head unload times) seems uncritical. The SRT is data-rate dependent and
 * counting down from 16 - very confusing.
 * Changing the 360k drive's SRT to F will make reading unstable. 
 */
static struct floppy_struct floppy_type[] = {
	    {   0,  0, 0, 0, 0x00, 0x00, 0x00, 0x00, NULL},	  /* no testing */
    /* 1 */ { 720,  9, 40, 0, 0x2A, R250, 0xDF, 0x50, "360k/PC"}, /* 360kB PC, SRT 6ms */
    /* 2 */ {2400, 15, 80, 0, 0x1B, R500, 0xDF, 0x54, "1.2M"},	  /* 1.2MB AT, SRT 5ms */
    /* 3 */ {1440,  9, 80, 0, 0x2A, R250, 0xEF, 0x50, "720k"},	  /* 3.5" 720kB */
    /* 4 */ { 720,  9, 40, 1, 0x23, R300, 0xDF, 0x50, "360k/AT"}, /* 360kB in 1.2MB drive */
    /* 5 */ {1440,  9, 80, 0, 0x23, R300, 0xDF, 0x50, "720k/AT"}, /* 720kB in 1.2MB drive */
    /* 6 */ {2880, 18, 80, 0, 0x1B, R500, 0xDF, 0x6C, "1.44M"},	  /* 1.44MB, SRT 3ms */
	 /* totSectors/secPtrack/tracks/stretch/gap/Drate/S&Hrates/fmtGap/name/  */
};

/* floppy probes to try per CMOS floppy type */
static unsigned char p360k[] =  { FT_360k_PC, FT_360k_PC, 0 };	/* DO NOT change */
static unsigned char p1200k[] = { FT_1200k,   FT_360k_AT, 0 };
static unsigned char p720k[] =  { FT_720k,    FT_720k,    0 };
static unsigned char p1440k[] = { FT_1440k,   FT_720k,    0 };	/* add 2880 here if needed */

/*
 * Auto-detection. Each drive type has a zero-terminated list of formats which
 * are used in succession to try to read the disk. If the FDC cannot lock onto
 * the disk, the next format is tried. This uses the variable 'probing'.
 */
static unsigned char *probe_list[CMOS_MAX] = { p360k, p1200k, p720k, p1440k, NULL };

/* This type is tried first. */
static unsigned char *base_type[MAX_FLOPPIES];

/*
 * The driver is trying to determine the correct media format
 * while probing is set. rw_interrupt() clears it after a
 * successful access. This works when the formats are distinct,
 * not when the format is the same and the # of sectors differ.
 */
static int probing;

/*
 * Keep per drive state and status to avoid race conditions in globals.
 * Even though only one IO op may be active at any one time, _open, _release and
 * _ioctl may operate on a different device and cannot touch globals.
 */
static struct fdev_s fdevice[MAX_FLOPPIES];

/* On an XT with a 720k drive, probing will not work because the first 9 sectors
 * read correctly anyway, we need to fake the CMOS types via bootopts */
extern int xt_floppy[];
extern int fdcache;	/* size of sector cache from bootopts */

/* Synchronization of FDC access. */
#ifdef INCLUDE_FD_FORMATTING
static int format_status = FORMAT_NONE
static int fdc_busy;
static struct wait_queue fdc_wait;
#endif

/* bit vector set when media changed - causes I/O to be discarded until unset */
//static unsigned int changed_floppies;
//static unsigned int just_opened;

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

#define NO_TRACK 255

/*
 * These are global variables, as that's the easiest way to give
 * information to interrupts. They are the data used for the current
 * request.
 */

static int initial_reset_flag;
static int need_configure = 1;	/* for 82077 */
static int reset;		/* something went wrong, reset FDC, start over */
static int recalibrate;		/* less dramatic than reset, like changed drive
				 * parameters or seek errors. Will (eventually)
				 * trigger a call to recalibrate_floppy() */
static int recover;		/* signal that we're recovering from a hang,
				 * awakened by the watchdog timer */
static int seek;		/* set if the current operation needs a cylinder
				 * change (seek) */
static int nr_sectors;		/* # of sectors to r/w, 2 if block IO */
static unsigned char raw;	/* set if raw/char IO	*/
static unsigned char use_bounce;/* this transaction uses the bounce buffer */

#if CONFIG_FLOPPY_CACHE
static unsigned char use_cache;	
static int cache_drive = -1;
static int cache_start;		/* first cached sector */
static int cache_len;		/* valid length of cache in sectors */
static int cache_size;		/* # of sectors - min=0 (off), max=FD_CACHE_SEGSZ>>9 */
//static unsigned int cache_hits;	/* For stats */
#endif

static int cur_spec1 = -1;	/* HUT | SRT */
static int cur_rate = -1;	/* datarate */
static struct floppy_struct *floppy;
static unsigned char current_drive = 255;
static unsigned char sector;
static unsigned char head;
static unsigned char track;
static unsigned char seek_track;
static unsigned char current_track = NO_TRACK;
static unsigned char command;
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
    clr_irq();
    running |= 0x10 << nr;
    set_irq();
    floppy_select(nr);
}

static struct timer_list motor_on_timer[MAX_FLOPPIES] = {
    {NULL, 0, 0, motor_on_callback},
    {NULL, 0, 1, motor_on_callback},
#if MAX_FLOPPIES == 4
    {NULL, 0, 2, motor_on_callback},
    {NULL, 0, 3, motor_on_callback}
#endif
};
static struct timer_list motor_off_timer[MAX_FLOPPIES] = {
    {NULL, 0, 0, motor_off_callback},
    {NULL, 0, 1, motor_off_callback},
#if MAX_FLOPPIES == 4
    {NULL, 0, 2, motor_off_callback},
    {NULL, 0, 3, motor_off_callback}
#endif
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
    unsigned int mask = 1 << ((bh->b_dev & 0x03);

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
    struct request *req = CURRENT;

#pragma GCC diagnostic ignored "-Wshift-count-overflow"
    use_bounce = req->rq_seg >> 16;		/* XMS buffers active, always bounce */ 
    physaddr = (req->rq_seg << 4) + (unsigned int)req->rq_buffer;
    dma_addr = _MK_LINADDR(req->rq_seg, req->rq_buffer);

    count = nr_sectors<<9;
    if (use_bounce || (physaddr + (unsigned int)count) < physaddr) { /* 64k phys wrap ? */
	use_bounce++;
	dma_addr = _MK_LINADDR(FD_BOUNCE_SEG, 0);
	if (raw) {	/* The application buffer spans a 64k boundary, split it into
			 * 2 or three parts, using the first k of the sector cache
			 * as a bounce buffer */
	    int sec_cnt = (0xffff - physaddr) >> 9;
	    if (sec_cnt <= 1) {	/* the single block with wrap problem */
		nr_sectors = BLOCK_SIZE >> 9;	
	    } else {		/* get the sectors before the wrap */
		nr_sectors = sec_cnt-1;
		use_bounce = 0;
		dma_addr = _MK_LINADDR(req->rq_seg, req->rq_buffer);
	    }
	    //count = nr_sectors<<9;
	}
    } else {
#if CONFIG_FLOPPY_CACHE
	if (use_cache) 
	    dma_addr = _MK_LINADDR(FD_CACHE_SEG, 0);
#endif
    }
    if (raw) {	/* ensure raw access doesn't span cylinders */
	int rest = (floppy->sect<<1) - sector - head*floppy->sect;
	if (rest < nr_sectors) {
	    printk("tr %d, spt %d, s %d, h %d, rest %d, req %d; ", current_track,
		floppy->sect, sector, head, rest, nr_sectors);
	    nr_sectors = rest;
	}
    }
    count = nr_sectors<<9;
    DEBUG("setupDMA:-%x:%x-", req->rq_seg, req->rq_buffer);

#ifdef INCLUDE_FD_FORMATTING
    if (command == FD_FORMAT) {
	dma_addr = _MK_LINADDR(FD_BOUNCE, 0);
	count = floppy->sect << 2;	/* formatting data, 4 bytes per sector */
    }
#endif
#if CONFIG_FLOPPY_CACHE
    if (use_cache) {
	cache_drive = -1;	/* mark cache bad, in case all this fails.. */
	cache_len = (floppy->sect<<1) - (sector + head*floppy->sect);
	if (cache_len > cache_size) cache_len = cache_size;
	count = cache_len << 9;
    } else
#endif
    if (use_bounce && command == FD_WRITE)
	xms_fmemcpyw(0, FD_BOUNCE_SEG, req->rq_buffer, req->rq_seg, BLOCK_SIZE/2);

    DEBUG("%d/%lx/%x;", count, dma_addr, physaddr);
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
    if (!CURRENT) return;
    if (probing) probing++;
#ifdef INCLUDE_FD_FORMATTING
    if (format_status == FORMAT_BUSY)
	errors = ++format_errors;
    else 
#endif
    errors = ++CURRENT->rq_errors;
    if (errors > MAX_ERRORS) {
        printk("df%d: Max retries (%d) exceeded\n", CURRENT->rq_dev&3, errors);
	request_done(0);
	return;
    }
    if (probing) return;
    if (errors > MAX_ERRORS / 2)
	reset = 1;
    else
	recalibrate = 1;
}

#ifdef SUPPORT_2880K

/*
 * Set perpendicular mode as required, based on data rate, if supported.
 * 82077 Untested! 1Mbps data rate only possible with 82072 and later FDCs.
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
	output_byte(HEAD_LOAD_TIME << 1);  /* Head load time, unit 2ms, LSB disables DMA */
    }
    if (cur_rate != floppy->rate) {
	/* use bit 6 of floppy->rate to indicate perpendicular mode */
#ifdef SUPPORT_2880K
	perpendicular_mode(floppy->rate);
#endif
	outb_p((cur_rate = (floppy->rate)) & ~0x40, FD_DCR);
    }
}

static void tell_sector(int nr)
{
    if (nr != 7) {
	printk(" -- FDC reply error");
	reset = 1;
    } else
	printk(": track %d, head %d, sector %d", reply_buffer[3],
	       reply_buffer[4], reply_buffer[5]);
}

/*
 * Ok, this interrupt is called after a DMA read/write has succeeded
 * or failed, so we check the results, and copy any buffers.
 * hhb: Added better error reporting.
 *
 * TODO: Cleanup the last part, where the actual data transfer is going on
 */
static void rw_interrupt(void)
{
    int nr;
    char bad;

    nr = result();
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
	    CURRENT->rq_errors++; /* may want to increase this even more, doesn't make */ 
	    		/* sense to re-try most of these conditions more 
			 * than the reporting threshold. NOTE: rq_errors is incremented in 
			 * bad_flp_intr too!! */
			/* FIXME: Need smarter retry/error reporting scheme */
			/* FIXME: A track change may fix some of these errors
			 * on old and worn drives */
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

    struct request *req = CURRENT;
    int drive = MINOR(req->rq_dev) & 3;

    if (probing) {
	if (fdevice[drive].current_type != floppy) {
	    fdevice[drive].current_type = floppy;
	    printk("df%d: Auto-detected floppy type %s\n", drive, floppy->name);
	}
	fdevice[drive].inode->i_size = (sector_t)floppy->size << 9;
	probing = 0;
    }
    if (raw) CURRENT->rq_nr_sectors = nr_sectors;
    if (use_bounce && command == FD_READ) {
	xms_fmemcpyw(req->rq_buffer, req->rq_seg, NULL, FD_BOUNCE_SEG, nr_sectors << (9-1));
									       /* words ^ */
    } else {
#if CONFIG_FLOPPY_CACHE
	if (use_cache) {
	    cache_drive = current_drive;
	    cache_start = CURRENT->rq_blocknr;
	    DEBUG("rd:%04x:0->%04lx:%04x;", FD_CACHE_SEG,
		(unsigned long)req->rq_seg, req->rq_buffer);
	    xms_fmemcpyw(req->rq_buffer, req->rq_seg, 0, FD_CACHE_SEG, BLOCK_SIZE/2);
	}
#endif
    }
    request_done(1);
    //DEBUG("RQOK;");
    redo_fd_request();	/* Continue with next request if any */
}

/*
 * We read multiple sectors if possible.  If we get too many errors, we go back to
 * reading just one sector at a time. This means we should be able to
 * read a sector even if there are other bad sectors on this track.
 *
 * The FDC will start read/write at the specified sector and continue
 * until the DMA controller tells it to stop ... as long as we're on the same cyl.
 * Notably: We always read (or write) at least a full block (2 sectors) except in raw mode.
 * This means that if the sector count is odd, an IO op will sometimes span tracks to
 * fill the block: Last sector on side 0, fist sector on side 1.
 * In raw mode, we use the requested IO size - if possible. 
 *
 * From the Intel 8272A app note: "The 8272A always operates in a multi-sector 
 * transfer mode. It continues to transfer data until the TC input is active."
 * IOW: We tell the FDC where to start, and the DMA controller where to stop.
 */
void setup_rw_floppy(void)
{
    DEBUG("setup_rw%d-",track);
    setup_DMA();
    do_floppy = rw_interrupt;
    output_byte(command);
    output_byte(head << 2 | current_drive);

#ifdef INCLUDE_FD_FORMATTING
    if (command != FD_FORMAT) {
#endif
	output_byte(track);
	output_byte(head);
	output_byte(sector+1); 

	output_byte(2);		/* sector size = 512 */
	output_byte(floppy->sect);
	output_byte(floppy->gap);
	output_byte(0xFF);	/* sector size, 0xff unless sector size==0 (128b) */

#ifdef INCLUDE_FD_FORMATTING
    } else {
	output_byte(2);		/* sector size = 512 */
	output_byte(floppy->sect * 2); /* sectors per cyl */
	output_byte(floppy->fmt_gap);
	output_byte(FD_FILL_BYTE);
    }
#endif
    DEBUG("S_OK;");
    if (reset)		/* If output_byte timed out */
	redo_fd_request();
}

static void seek_interrupt(void)
{
    /* get interrupt status */
    output_byte(FD_SENSEI);
    DEBUG("seekI-");
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
#if CONFIG_FLOPPY_CACHE
    use_cache = cache_size && !raw && (command == FD_READ) && (CURRENT_ERRORS < 4);

    DEBUG("trns%d-", use_cache);
#else
    DEBUG("trns-");
#endif

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
    //output_byte(FD_SENSEI);	/* Never use the SENSEI command except after seek and recal */
    printk("%s: Unexpected interrupt\n", DEVICE_NAME);
    //if (result() != 2 || (ST0 & 0xE0) == 0x60)
//	reset = 1;
    //else
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
    output_byte(FD_SPECIFY);
    output_byte(cur_spec1);	/* hut etc */
    output_byte(HEAD_LOAD_TIME << 1);		/* Head load time, DMA */
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
 * HS: 6 secs is excessive, Hermann Sieb says 1,8s, 2 or 3 seems like a reasonable
 *	 compromise ...
 */
static void floppy_shutdown(void)
{
    DEBUG("[%u]shtdwn0x%x|%x-", (unsigned int)jiffies, current_DOR, running);
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
    if (result() == 2 && (ST0 & 0x10))	/* track 0 test */
	return 0;			/* failure */
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
    //unsigned int mask = (1 << current_drive);

    DEBUG("RDY0x%x,%d,%d-", MEDIA_CHANGED, reset, recalibrate);
#ifdef USE_DIR_REG
    if (MEDIA_CHANGED) {
        if (fdevice[current_drive].current_type && !(just_opened & mask)) {
            changed_floppies |= (1 << current_drive); /* this will discard any queued I/O */
            printk("df%d: Drive was opened, purge pending I/O\n", current_drive);
            fdevice[current_drive].current_type = NULL;     /* comment out to keep last media format */
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
    //just_opened &= ~mask;

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
#ifdef INCLUDE_FD_FORMATTING
    if (format_status == FORMAT_WAIT)
	format_status = FORMAT_BUSY;
    if (format_status != FORMAT_BUSY)
#endif
    {
	if (!req) {
#ifdef INCLUDE_FD_FORMATTING
	    if (fdc_busy) {
		fdc_busy = 0;
		wake_up(&fdc_wait);
	    }
#endif
	    CLEAR_INTR;
	    return;
	}
	CHECK_REQUEST(req);
    }
    DEBUG("\nREDO:%04x;", req);
    seek = 0;
    device = MINOR(req->rq_dev);
    drive = device & 3;
    if (fdevice[drive].fd_probe) {
	probing = 1;
	fdevice[drive].fd_probe = 0;
    }

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
	floppy = fdevice[drive].current_type;
	if (probing > 1) {	/* the previous attempt (using current_type) failed, start
				 * auto-detection */
	    if (!base_type[drive][probing-1])	/* end of table? */
		probing = 1;
	    floppy = &floppy_type[base_type[drive][probing-1]];
	    //printk("df%d: auto-probe #%d %s\n", drive, probing, floppy->name);
	}
#if 0
	if (!floppy) {
	    if (!base_type[drive]) {	/* Access to non-existent drive */
		request_done(0);
		goto repeat;
	    }
	    probing = 1;
	    need_configure = 1;	/* Maybe 'probing' can replace 'need_configure' */
	    floppy = &floppy_type[base_type[drive][CURRENT_ERRORS & 1]];
	}
#endif
    }

#if DEBUG_DIRECTFD
    if (debug_level == 1) printk("[%u]redo-%c %d(%s) bl %u;", (unsigned int)jiffies, 
		req->rq_cmd == WRITE? 'W':'R', device, floppy->name, req->rq_blocknr);
    if (debug_level > 2) printk("df[%04x]: %c blk %ld(%d)\n", req->rq_dev, req->rq_cmd==WRITE? 'W' : 'R',
		req->rq_blocknr, req->rq_nr_sectors);
#endif
#ifdef INCLUDE_FD_FORMATTING
    if (format_status != FORMAT_BUSY)
#endif
    {
    	unsigned int tmp;
	if (current_drive != drive) {
	    current_track = NO_TRACK;
	    current_drive = drive;
	}

	raw = !!req->rq_nr_sectors;	/* raw IO if rq_nr_sectors > 0 */
	nr_sectors = 2;			/* Default: Block IO - 1k blocks */
	if (raw)
		nr_sectors = req->rq_nr_sectors;;
	start = (unsigned int) req->rq_blocknr;	/* rq_blocknr is ALWAYS sector # */
	if (start + nr_sectors > floppy->size) {
	    if (!raw || start >= floppy->size) {
		request_done(0);
		goto repeat;
	    } else {
		nr_sectors = floppy->size - start;
		req->rq_nr_sectors = nr_sectors;	/* tell requester we truncated */
	    }
	}
	sector = start % floppy->sect;
	tmp = start / floppy->sect;
	head = tmp % FLOPPY_HEADS;
	track = tmp / FLOPPY_HEADS;

	/* Ensure raw IO requests have valid # of sectors: Don't span cyl boundaries */
	/* If running a 82077 which has implied seek, enable that instead! */
#if 0
	if (raw && ((sector + (floppy->sect*head) + nr_sectors)) 
			> (floppy->sect<<1)) {		/* sect * 2 = cyl */
		nr_sectors = (floppy->sect<<1) - (sector + (floppy->sect * head));
		req->rq_nr_sectors = nr_sectors;	/* return updated # of sectors */
	}
#endif
	seek_track = track << floppy->stretch;
	DEBUG("%d:%d:%d:%d; ", start, sector, head, track);
	if (req->rq_cmd == READ)
	    command = FD_READ;
	else if (req->rq_cmd == WRITE)
	    command = FD_WRITE;
	else {
	    printk("df%d: unknown command (%x)\n", drive, command);
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

    DEBUG("prep %d,%d|%d-", seek_track, cache_drive, current_drive);

#if CONFIG_FLOPPY_CACHE 
    if (!raw && (current_drive == cache_drive) && start >= cache_start
			&& start <= (cache_start + cache_len - nr_sectors + 1)) {

	unsigned char *buf_ptr;

	/* Requested block is in the cache. If we're reading, go get it.
	 * Otherwise, update it. */
	buf_ptr = (unsigned char *)((start - cache_start) << 9);
	DEBUG("bufrd chs %d/%d/%d-%x\n", seek_track, head, sector, buf_ptr);

	if (command == FD_READ) {	/* requested data is in cache */
//	    cache_hits++;
	    xms_fmemcpyw(req->rq_buffer, req->rq_seg, buf_ptr, FD_CACHE_SEG, BLOCK_SIZE/2);
	    request_done(1);
	    goto repeat;
    	} else if (command == FD_WRITE)	/* update track cache */
	    xms_fmemcpyw(buf_ptr, FD_CACHE_SEG, req->rq_buffer, req->rq_seg, BLOCK_SIZE/2);
    } 
#endif

    /* restart timer for hung operations, 4 secs seems about right ... */
    del_timer(&fd_timeout);
    fd_timeout.tl_expires = jiffies + 4 * HZ;
    add_timer(&fd_timeout);

    if (seek_track != current_track)
	seek = 1;

    floppy_on(current_drive);
}

void do_fd_request(void)
{
    DEBUG("fdrq:");
    if (CURRENT) CURRENT->rq_errors = 0;	// EXPERIMENTAL
#ifdef INCLUDE_FD_FORMATTING
    while (fdc_busy) {
	printk("df: request while fdc busy\n");
	sleep_on(&fdc_wait);
    }
    fdc_busy = 1;
#endif
    redo_fd_request();
}

int fd_ioctl(struct inode *inode,
		    struct file *filp, unsigned int cmd, unsigned int param)
{
    struct floppy_struct *this_floppy;
    int device, drive, err = -EINVAL;
    struct hd_geometry *loc = (struct hd_geometry *)param;

    if (!suser())	/* For now */
	return -EPERM;
    if (!inode || !inode->i_rdev)
	return -EINVAL;
    device = MINOR(inode->i_rdev);
    drive = device & 3;
    if (device > 3)
	this_floppy = &floppy_type[drive >> 2];
    else if ((this_floppy = fdevice[drive].current_type) == NULL)
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
	printk("df%d: %s (type %d)", drive, floppy_type[*base].name, type);
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
    if (sys_caps & CAP_PC_AT) {
	base_type[0] = find_base(0, (CMOS_READ(0x10) >> 4) & 0xF);
	if ((CMOS_READ(0x14) >> 6) & 1) {
	    printk(", ");
	    base_type[1] = find_base(1, CMOS_READ(0x10) & 0xF);
	}
	printk(" [CMOS]");
    } else {
	if (xt_floppy[0]) {	/* floppy types from bootopts */
	    base_type[0] = find_base(0, xt_floppy[0]);
	    if (xt_floppy[1]) {
		printk(", ");
		base_type[1] = find_base(1, xt_floppy[1]);
	    }
	    printk(" [bootopts]");
	} else {
		/* No CMOS or bootopts, force 2 type 1 drives - for convenience.
		 * Neither may exist, we have no way to find out. */
	    base_type[0] = base_type[1] = probe_list[FT_360k_PC - 1];
	    printk("df0, df1 set to 360k/PC (type 1)");
	}
    }
    printk("\n");
}

/*
 * floppy_open checks for aliasing (/dev/fd0 can be the same as
 * /dev/PS0 etc), and disallows simultaneous access to the same
 * drive with different device numbers.
 *
 * FIXME: May want to reject raw access on an open block device.
 */
int floppy_open(struct inode *inode, struct file *filp)
{
    int drive, old_dev, dev;
    int israw = S_ISCHR(inode->i_mode);
    struct fdev_s *fdev;
    struct floppy_struct *this_floppy;

    dev = MINOR(inode->i_rdev);
    drive = dev & 3;
    fdev = &fdevice[drive];
    old_dev = fdev->fd_device;
    //cache_drive = -1;	

    if (!israw) {			/* For block device access only */
	if (fdev->fd_ref)
    	    if (old_dev != inode->i_rdev)
		return -EBUSY;		/* no reopens using different minor */
	fdev->fd_ref++;
	fdev->fd_device = inode->i_rdev;

#ifdef CHECK_MEDIA_CHANGE_XXX
	if (filp && filp->f_mode) {
	    if (check_disk_change(inode->i_rdev))
		return -ENXIO;
	}
#endif
    } 

    if (dev > 3)		/* forced floppy type */
	this_floppy = &floppy_type[dev >> 2];
    else {			/* Auto-detection */
	this_floppy = fdev->current_type;
	if (!this_floppy) {
	    if (!base_type[drive])
		return -ENXIO;
	    this_floppy = &floppy_type[base_type[drive][0]];
	    fdev->current_type = this_floppy;
	}
	if (sys_caps & CAP_PC_AT) fdev->fd_probe++;
    }
    fdev->inode = inode;
    inode->i_size = ((sector_t)(this_floppy->size)) << 9;
    DEBUG("df%d: open %s dev %04x, sz %lu, %s\n", drive, israw?"(raw)":"",
		inode->i_rdev, inode->i_size, this_floppy->name);

    return 0;
}

void floppy_release(struct inode *inode, struct file *filp)
{
    kdev_t rdev = inode->i_rdev;
    int drive = MINOR(rdev) & 3;

    if (S_ISCHR(inode->i_mode)) return;/*goto clear_raw;*/ /* change if allowing simlutaneous raw
						 * and block access! FIXME possibly superfluous */
    if (--(fdevice[drive].fd_ref) == 0) {
	fsync_dev(rdev);
	invalidate_buffers(rdev);
	invalidate_inodes(rdev);
#if 0
clear_raw:
	changed_floppies &= ~(1 << drive);	/* clear media change flag */
	fdevice[drive].current_type = NULL;	/* prevent next open from flagging media change */
	printk("Cache hits: %u\n", cache_hits);
#endif
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
    int err;

    outb(current_DOR, FD_DOR);	/* all motors off, DMA, /RST  (0x0c) */
    if (register_blkdev(MAJOR_NR, DEVICE_NAME, &floppy_fops)) {
	printk("Unable to get major %d for floppy\n", MAJOR_NR);
	return;
    }
    blk_dev[MAJOR_NR].request_fn = DEVICE_REQUEST;

    err = request_irq(FLOPPY_IRQ, floppy_interrupt, INT_GENERIC);
    if (err) {
	printk("Unable to grab IRQ%d for the floppy driver\n", FLOPPY_IRQ);
	return;	/* should be able to signal failure back to the caller */
    }

    if (request_dma(FLOPPY_DMA, (void *)DEVICE_NAME)) {
	printk("Unable to grab DMA%d for the floppy driver\n", FLOPPY_DMA);
	return;
    }

#ifdef ENABLE_FDC_82077
    /* Try to determine the floppy controller type */
    DEVICE_INTR = ignore_interrupt;	/* don't ask ... */
    output_byte(FD_VERSION);	/* get FDC version code */
    if (result() != 1) {
	printk("%s: FDC failed to return version byte\n", DEVICE_NAME);
	fdc_version = FDC_TYPE_STD;
    } else
	fdc_version = ST0;

#else
    fdc_version = FDC_TYPE_STD;	/* force std fdc type; can't test other. */
#endif
    printk("%s: Direct floppy driver, FDC %s @ irq %d, DMA %d", DEVICE_NAME, 
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
    config_types();

#if CONFIG_FLOPPY_CACHE
    /* sector cache setup - /bootopts fdcache= has preference, otherwise autoconfig */
    if (fdcache != -1)			/* allow fdcache=0 in bootopts */
	cache_size = fdcache<<1;	/* cache size is sectors, fdcache is k bytes */
    else if (SETUP_CPU_TYPE == 7)
	cache_size = 0; 		/* sector cache is slowing down fast systems */
    else cache_size = FD_CACHE_SEGSZ>>9;	/* use menuconfig value */

    if (cache_size > (FD_CACHE_SEGSZ>>9)) cache_size = FD_CACHE_SEGSZ>>9;
    printk("Floppy cache %dk, available %dk\n", cache_size>>1, FD_CACHE_SEGSZ>>10);
	
#endif
}
