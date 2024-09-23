/*
 * MFM/RLL (XT type) disk driver for TLVC.
 *
 * Written by Helge Skrivervik (@Mellvik) Feb24.
 * Inspired by the xt.c mfm driver in Venix86 2.1
 * and the mfm driver in early Linux (xd.c).
 */

/* DEVELOPER NOTES:
 *
 * - We don't check for extreme transfer sizes (like > 32k) which will crash 
 *   the system because the RAM refresh is disturbed. This is safe because 
 *   TLVC cannot - without special programming - transfer more than 20k in a raw
 *   read/write operation anyway. Block mode operations are always 1k.
 *
 * - Sectors - controller level - start at 0, not 1!!
 *
 * - The 1st MFM controller is always at 0x320, IRQ5, DMA3. This is a commonly used 
 *   address on network interfaces and others. Collisions have interesting consequences,
 *   not necessarily easy to decipher. 
 *
 * - Using a MFM controller on a AT or later machine may or may not work. 1st gen
 *   AT machines emulate MFM controllers @ 0x320 for compatibility, masking the fact that
 *   the disk(s) are IDE/ATA. Adding a physical mfm controller at that address
 *   may or may not work.
 *
 * - There is no 'standard' MFM controller. All have the same basic command set, most
 *   have extensions and the behaviour of the 'state machine' governing the command
 *   phase varies. E.g. there is no standard way to determine disk drive data (CHS etc.).
 *   This is why the 'big' Linux MFM driver pokes into the controller ROM to find the 
 *   type and details. We don't have space for that, and assume the default 10MB CHS
 *   values (304/4/17/128) unless /bootopts is used to define the size.
 *   The last value in the bootopts parameter quadruple is the Write Precompensation
 *   track, aka WPCOM, which seems to  be important only for the oldest drives.
 *   A good thing n this mess is that since all drives in this category have (or emulate) 17
 *   sectors, there is no problem booting even if the real size of the drive is unknown
 *   at that point. After booting, TLVC can make its own assumptions about size.
 *   Note however, that the controller needs to be told the CHS values we plan to use
 *   in order to provide access to the entire drive w/o errors (see the xd_setparam()
 *   function).
 *
 * - A drive 'works' only with the type of controller with which it was formatted. 
 *   IOW moving drives around is a no-go. Newer controllers (actually most except the 
 *   very first generation, pre 1985) have a formatter included in the firmware, to be
 *   started from MSDOS DEBUG, typically at C800:5. For the older controllers, other
 *   tricks are required.
 *
 * - Pending the availability of a MFM formatting utility for TLVC, a crude formatter is
 *   included in the driver. If compiled in (#define ALLOW_FORMATTING), set the /bootopts
 *   sector count for the drive to 0, and a drive format will be attempted.
 *   [Obviously, the sector count needs to be reset to normal before the next boot.]
 *   There is no safe way (at this point) to detect the completion of the format process
 *   other than keeping an eye on the activity LED. No LED activity means formatting has 
 *   stopped - completed or failed. Check the status code returned to find out which. 
 *   It's important to set the correct CHSW parameters before the formatting. Also notice
 *   that the interleave is preset to 5, which is good for <8MHz machines, otherwise use 4.
 *   It is also worth noticing that neither the set drive parameter nor the format command 
 *   take a sector count parameter, it is apparently assumed that the sector count is always 17. 
 *
 * - INTERLEAVE: The Hard Drive Bible disagrees with the above (which comes from a WD document).
 *   Their recipe is 4:1 @4.77MHz or less, 3:1 @ 5-10MHz, 2:1 @ 10-16 MHz, 1:1 for higher.
 *   Remember - this is all about old MFM/RLL drives.
 *
 * - Beware that the sluggishness of 4.77MHz PCs may deliver surprisese, in 
 *   particular related to interrupts. Like the interrupt occuring before the request call
 *   has completed (see also comments in the code).
 *
 * - If drive 0 is missing and drive 1 present, some BIOSes will seem to hang at boot time.
 *   Be patient, 15-20 seconds, and it continues - TLVC will boot fine. Such configuration 
 *   is not hard-disk bootable though (floppy OK).
 *
 * - RESET: On some controllers, the drive parameters must be re-programmed after a reset.
 *
 * - SOME CONTROLLERS CAN NEVER WORK: Some 3rd gen MFM controllers, like the Longshine LCS6210D,
 *   work only via the embedded BIOS. No API, no IOports, no IRQ lines, no DMA ACK lines etc.
 *   The driver will just not find such controllers. Use BIOS HD instead.
 */

/*
 * TODO
 * - Add support for raw devices
 * - Add ioctl to manipulate drive parameters, such as automatic retries -
 *   to be able to continue after hard errors and to support real performance 
 *   testing.
 * - Add format utility.
 */

#include <linuxmt/config.h>
#include <linuxmt/debug.h>
#include <linuxmt/kernel.h>
#include <linuxmt/mm.h>
#include <linuxmt/fs.h>
#include <linuxmt/timer.h>
#include <linuxmt/biosparm.h>
#include <linuxmt/genhd.h>
#include <linuxmt/major.h>
#include <linuxmt/errno.h>
#include <linuxmt/stat.h>	/* for S_ISCHR() */
#include <linuxmt/heap.h>	/* for heap_alloc */
#include <linuxmt/string.h>	/* for memset */

#include <arch/dma.h>
#include <arch/system.h>
#include <arch/io.h>
#include <arch/segment.h>
#include <arch/ports.h>
#include <arch/hdreg.h>		/* for ioctl defines */

#define BIOSHD_DRIVE_PARMS      0x0800	/* from linuxmt/bioshd.h */
#define MFMDISK		/* for blk.h */
#define ASYNC_IO
#define MINOR_SHIFT 5
#define MAJOR_NR XD_MAJOR

#include "blk.h"

/* Default drive params if all else fails */
#define DEF_HEADS	4
#define DEF_CYLS	304
#define DEF_SECTS	17
#define DEF_WPCOMP	128

#define	XD_CNTF	0x05		/* stepping speed, 5=70us, 0=3ms, the code is controller
				 * dependent, but these two seem pretty much std. Note:
				 * this is not the drive's physical stepping speed. Even
				 * back in 82/83, the drives could handle buffered seeks,
				 * thus the uSec timings (they're not real). 70uS seems
				 * to work in most cases. */
/*
 * The 8237 DMA controller cannot access data above 1MB on the origianl PC
 * (4 bit page register) (the AT is different, the page reg is 8 bits while the
 * 2nd DMA controller transfers words only, not bytes and thus up to 128k bytes
 * in one 'swoop').
 */
#define LAST_DMA_ADDR   (0x100000L - BLOCK_SIZE) /* Enforce the 1M limit */

#define _MK_LINADDR(seg, offs) ((unsigned long)((((unsigned long)(seg)) << 4) + (unsigned)(offs)))

/*
 * XT winchester controller commands and formats.
 */
#define CMD_TESTREADY	0x00		/* test drive ready */
#define CMD_RECAL	0x01
#define CMD_SENSE	0x03		/* request status information  */
#define CMD_FORMATDRV	0x04		/* Format drive */
#define	CMD_READ	0x08		/* read data */
#define	CMD_WRITE	0x0a		/* write data */
#define CMD_SEEK	0x0b
#define CMD_SETPARAM	0x0c		/* Set drive params, most ctrlrs */

/* XT hard disk controller registers */
#define XD_DATA		(MHD_PORT + 0x00)	/* data RW register */
#define XD_RESET	(MHD_PORT + 0x01)	/* reset WO register */
#define XD_STATUS	(MHD_PORT + 0x01)	/* status RO register */
#define XD_SELECT	(MHD_PORT + 0x02)	/* select WO register */
#define XD_JUMPER	(MHD_PORT + 0x02)	/* jumper RO register */
#define XD_CONTROL	(MHD_PORT + 0x03)	/* DMAE/INTE WO register */

/* Bits for command status byte */
#define CSB_ERROR	0x02	/* error */
#define CSB_LUN		0x20	/* logical Unit Number */

/* XT hard disk controller status bits */
#define STAT_READY	0x01	/* controller is ready */
#define STAT_INPUT	0x02	/* data flowing from controller to host */
#define STAT_COMMAND	0x04	/* controller in command phase */
#define STAT_SELECT	0x08	/* controller is selected */
#define STAT_REQUEST	0x10	/* controller requesting data */
#define STAT_INTERRUPT	0x20	/* controller requesting interrupt */

/* XT hard disk controller control bits */
#define PIO_MODE	0x00	/* control bits to set for PIO */
#define DMA_MODE	0x03	/* control bits to set for DMA & interrupt */

#define XD_TIMEOUT	HZ	/* Really 1sec in jiffies, we're not there yet ... */

static struct	xdmsg {	/* convert error numbers to messages */
	const char	num;
	const char	*msg;
} xdmsg[] = {
	{0xFF,	"Controller timeout"},
	{0x30,	"Hardware Failure"},	/* maps several 3x error codes */
	{0x21,	"Illegal disk address"},
	{0x20,	"Invalid command"},
	{0x1A,	"Format/alt.track error"}, /* maps 1a-1f errors */
	{0x19,	"Bad track"},
	{0x18,	"Correctable data error"},
	{0x15,	"Seek error"},
	{0x12,	"No address mark"},
	{0x11,	"Data error"},
	{0x06,	"No track 0"},
	{0x04,	"Drive not ready"},
	{0x03,	"Write fault"},
	/*{0x14,	"Sector not found"},*/	/* 1,2,10,14 -> equals seek err (15) */
					/* mapped automatically by many controllers */
	/*{0x10,	"ID error"},*/
	/*{0x02,	"No seek complete"},*/
	/*{0x01,	"No index signal"},*/
	{0x00,	"Unknown error"}
};


extern int	hdparms[];	/* Get CHS values from /bootopts */

static int	xd_busy;
static struct	wait_queue xd_wait;
static byte_t	xtnoerr;	/* set during tests to supress error messages */
static byte_t	usebounce;	/* set when bounce buffer is in use */


static int access_count[MAX_XD_DRIVES] = {0, };
static struct drive_infot drive_info[MAX_XD_DRIVES] = {0, };
static struct hd_struct hd[MAX_XD_DRIVES << MINOR_SHIFT]; /* partition ptr {start_sect, num_sects} */
static int xd_sizes[MAX_XD_DRIVES] = {0, };

static char *bounce_buffer;

/* local function prototypes */
int xd_ioctl(struct inode *, struct file *, unsigned int, unsigned int);
int xd_open(struct inode *, struct file *);
void xd_release(struct inode *, struct file *);
static void xd_geninit();
static void mdelay(int);
static void setup_DMA(int);
static void deverror(int, byte_t *);
//static int get_drive_type(int); 
static void do_xdintr(int, struct pt_regs *);
static int xd_waitport(byte_t, byte_t, int);
static void redo_xd_request(void);
static void xd_build (byte_t *, byte_t, byte_t, byte_t, word_t, byte_t, byte_t, byte_t);
static int xd_recal(int);
static word_t xd_command(byte_t *, byte_t, byte_t *, byte_t *, byte_t *, int);

//#define ALLOW_FORMATTING	/* comment out to avoid accidental disasters */

#ifdef ALLOW_FORMATTING
static void xd_format(int);
#endif

//#define DEBUG
#ifdef DEBUG
#define debug_xd printk
#else
#define debug_xd(...)
#endif


static struct file_operations xd_ops = {
    NULL,			/* lseek - default */
    block_read,			/* read - general block-dev read */
    block_write,		/* write - general block-dev write */
    NULL,			/* readdir - bad */
    NULL,			/* select */
    xd_ioctl,			/* ioctl */
    xd_open,			/* open */
    xd_release,			/* release */
};

static struct gendisk xd_gendisk = {
    MAJOR_NR,			/* major: major number */
    DEVICE_NAME,		/* major_name: device name */
    MINOR_SHIFT,		/* minor_shift: # rightshifts to get real minor  */
    1 << MINOR_SHIFT,		/* max_p: Max partitons, FIXME change to 4 */
    MAX_XD_DRIVES,
    xd_geninit,			/* init */
    hd,				/* hd struct */
    xd_sizes,			/* drive sizes */
    0,
    (void *) drive_info,
    NULL
};


static void xd_geninit(void)
{
    struct drive_infot *drivep;
    struct hd_struct *hdp = hd;
    int i;

    drivep = drive_info;
    for (i = 0; i < MAX_XD_DRIVES << MINOR_SHIFT; i++) {
	hdp->start_sect = -1;
	if ((i & ((1 << MINOR_SHIFT) - 1)) == 0) {
	    if ((hdp->nr_sects = (sector_t)drivep->sectors *
				drivep->heads * drivep->cylinders))
	    	hdp->start_sect = 0;	/* enable drive */
	    //printk("xd%d: %ld##", i, hdp->nr_sects);
	    drivep++;
	} else
	    hdp->nr_sects = 0;
	hdp++;
    }
    return;
}

static void do_xd_request(void)
{
	debug_xd("XDRQ:");
	while (xd_busy) {
		printk("request while XD busy\n");
		sleep_on(&xd_wait);
	}
	xd_busy = 1;
	redo_xd_request();
	debug_xd("exit xdrq;");
	//mdelay(100);
}

/*
 * Start a R/W request, then return and wait for the completion interrupt. Unless
 * there is an error before we even get started, in which case we signal back
 * failure and continue with the next request. 
 */
static void redo_xd_request(void)
{
    sector_t start;
    //unsigned raw_mode;
    unsigned count, track, cyl;
    byte_t head, sec, sense[4], cmd[6];
    struct drive_infot *dp;
    struct request *req;
    int minor, drive;


  repeat:
    req = CURRENT;
    debug_xd("redo_xd_request %x %i;", req, xd_busy);

    if (!req) {
	if (xd_busy) {
	    xd_busy = 0;
	    wake_up(&xd_wait);
	}
	//kputchar('=');
	return;
    }
    CHECK_REQUEST(req);	/* must set CHECK_BLOCKIO to activate, see blk.h */
    minor = MINOR(req->rq_dev);
    drive = minor >> MINOR_SHIFT;

    if (drive >= 2 || drive < 0) {
    	printk("Non-existent drive\n");
	end_request(0);
	goto repeat;
    }

    dp = &drive_info[drive]; 
    if (req->rq_nr_sectors) {
    	count = req->rq_nr_sectors;
	//raw_mode = 1;
    } else {
	count = BLOCK_SIZE / 512;
    	//raw_mode = 0;
    }
    start = req->rq_blocknr;

    if (hd[minor].start_sect == -1 || hd[minor].nr_sects < start) {
    	printk("Bad partition start or block out of bounds: %lu\n", start);
	end_request(0);
    	goto repeat;
    }

    /* Sector count starts at 0, not the (now) normal 1 */
    start += hd[minor].start_sect;
    track = start / dp->sectors;
    head  = track % dp->heads;
    cyl   = track / dp->heads;
    sec   = start % dp->sectors;
    debug_xd("xd%d: CHS %d/%d/%u st: %lu cnt: %d buf: %04x seg: %lx %c\n",
		drive, cyl, head, sec, start, count, req->rq_buffer,
		(unsigned long)req->rq_seg, req->rq_cmd == READ? 'R' :'W');
    xd_build(cmd, req->rq_cmd == READ ? CMD_READ : CMD_WRITE, drive, head, 
				cyl, sec, count&0xFF, XD_CNTF);
    //printk("xd%d: Command %x|%x|%x|%x|%x|%x\n", drive, cmd[0], cmd[1],
    //				cmd[2], cmd[3], cmd[4], cmd[5]);
    setup_DMA(count);
    xd_command(cmd, DMA_MODE, 0, 0, sense, XD_TIMEOUT);
    /* Don't attempt to do error processing when having requested DMA_MODE */
}


int xd_open(struct inode *inode, struct file *filp)
{
    unsigned int minor = MINOR(inode->i_rdev);
    int target = DEVICE_NR(inode->i_rdev);

    //printk("XD open: target %d, minor %d, start_sect %ld\n", target, minor, hd[minor].start_sect);
    if (target >= MAX_XD_DRIVES)
	return -ENXIO;

    if (((int) hd[minor].start_sect) == -1)	/* FIXME is this initialized properly? */
	return -ENXIO;

    if (!S_ISCHR(inode->i_mode)) 	/* Don't count raw opens */
	access_count[target]++;

    inode->i_size = (hd[minor].nr_sects) << 9;

    /* limit inode size to max filesize for CHS >= 4MB (2^22)*/
    if (hd[minor].nr_sects >= 0x00400000L)	/* 2^22*/
        inode->i_size = 0x7ffffffL;		/* 2^31 - 1*/
    debug_xd("%cxd[%04x] open, size %ld\n", S_ISCHR(inode->i_mode)? 'r': ' ', inode->i_rdev, inode->i_size);
    if (!bounce_buffer)		/* allocate bounce buffer space */
	if (!(bounce_buffer = heap_alloc(BLOCK_SIZE, HEAP_TAG_DRVR))) {
	    printk("xd: cannot allocate buffer space\n");
	    return -EBUSY;
	}
    return 0;
}

/* Much of the DMA setup is superfluous on an XT type machine, 
 * but we try to make this work even on ATs
 */
static void setup_DMA(int nr_sectors)
{
    unsigned long dma_addr;
    unsigned int count, physaddr;
    int use_xms;
    struct request *req = CURRENT;

#pragma GCC diagnostic ignored "-Wshift-count-overflow"
    use_xms = req->rq_seg >> 16;
    physaddr = (req->rq_seg << 4) + (unsigned int)req->rq_buffer;

    count = nr_sectors<<9;
    if (use_xms || (physaddr + (unsigned int)count) < physaddr) /* 64k wrap test */
	dma_addr = LAST_DMA_ADDR + 1;	/* force use of bounce buffer */
    else
	dma_addr = _MK_LINADDR(req->rq_seg, req->rq_buffer);

    debug_xd("setupDMA ");

    if (dma_addr >= LAST_DMA_ADDR) {	/* xms or 64k wrap */
	dma_addr = _MK_LINADDR(kernel_ds, bounce_buffer);
	usebounce = 1;
	if (req->rq_cmd == WRITE)
	    xms_fmemcpyw(bounce_buffer, kernel_ds, req->rq_buffer,
	    				req->rq_seg, BLOCK_SIZE/2);
    }
    debug_xd("%d/%lx;", count, dma_addr);
    clr_irq();
    disable_dma(MHD_DMA);
    clear_dma_ff(MHD_DMA);
    set_dma_mode(MHD_DMA,
		 (req->rq_cmd == READ) ? DMA_MODE_READ : DMA_MODE_WRITE);
    set_dma_addr(MHD_DMA, dma_addr);
    set_dma_count(MHD_DMA, count);
    enable_dma(MHD_DMA);
    set_irq();
}

int xd_ioctl(struct inode *inode,
	struct file *filp, unsigned int cmd, unsigned int param)
{
    struct hd_geometry *loc = (struct hd_geometry *) param;
    int drive, err;

    if ((!inode) || !(inode->i_rdev))
	return -EINVAL;

    drive = DEVICE_NR(inode->i_rdev);
    if (drive >= MAX_XD_DRIVES)
	return -ENODEV;

    if (cmd == HDIO_GETGEO) {
	/* safety check .. i presume :) */
	err = verify_area(VERIFY_WRITE, (void *)param, sizeof(struct hd_geometry));
	if (err)
	    return err;
	put_user_char(drive_info[drive].heads, &loc->heads);
	put_user_char(drive_info[drive].sectors, &loc->sectors);
	put_user(drive_info[drive].cylinders, &loc->cylinders);
	put_user_long(hd[MINOR(inode->i_rdev)].start_sect, &loc->start);
	return 0;
    }
    return -EINVAL;
}

static void do_xdintr(int irq, struct pt_regs *regs)
{
	register int i;
	byte_t j = 0xFF;
	int drive = 0;

	i = inb_p(XD_DATA); 
	outb_p(PIO_MODE, XD_CONTROL);	/* turn off interrupts/DMA */
	//kputchar('Z');
	//debug_xd("xd INTstat %x %x; ", inb(XD_STATUS), i);
	drive = i>>5;			/* get drive # from status byte */
	if (xd_busy) {
		if (i & CSB_ERROR) {
			byte_t cmd[6], sense[4];

			memset(&cmd[2], 0, 4); /* skip this? the controller doesn't care ... */
			cmd[0] = CMD_SENSE;
			cmd[1] = drive << 5;
			//if (xdcmd(CMD_SENSE, PIO_MODE, drive))
			//if (!xdcmd(cmd, PIO_MODE) && !xd_waitport(STAT_READY, STAT_READY, 20))
			    //j = inb_p(XD_DATA) & 0x3f;	/* get 1st byte of SENSE data */

			/* get error code */
			if (!xd_command(cmd, PIO_MODE, sense, NULL, NULL, XD_TIMEOUT)) {
			    //printk("Sense %x|%x|%x|%x;", sense[0], sense[1], sense[2], sense[3]);
			    j = sense[0]&0x3f;	/* some ctrlrs set the high bit(s) */
			} /* else unknown error */

			if (xtnoerr == 0)
			    deverror(drive, sense);

			if (j != 0x18) {	/* check for correctable error */
			        //xd_recal(drive); 	/* LATER */
				i = 0;		/* indicate error */
				goto done;
			}
		}
		/*
		 * Finished with this transfer ?
		 */
		if (CURRENT->rq_cmd == READ && usebounce)
			xms_fmemcpyw(CURRENT->rq_buffer, CURRENT->rq_seg, bounce_buffer,
					kernel_ds, BLOCK_SIZE/2);
		i = 1;	/* iodone */
			
done:
		end_request(i);
	} else
		printk("Spurious xd interrupt\n");
	usebounce = 0;
	redo_xd_request();
}

void xd_release(struct inode *inode, struct file *filp)
{
    int target = DEVICE_NR(inode->i_rdev);
    kdev_t dev = inode->i_rdev;

    access_count[target]--;
    fsync_dev(dev);
    if (!access_count[target]) {
	invalidate_buffers(dev);
	invalidate_inodes(dev);
	heap_free(bounce_buffer);
	bounce_buffer = NULL;	/* need to fix this when raw is added */
    }
    return;
}

/* controller reset & probe: check if anything there */
/* At power on, status is usually 0c3 (or 0xc3, the upper 2 bits always set on some
 * controllers).
 * After reset, the status is 0 (or 0xc0). In order to use for probing, select the 
 * interface and check that the SELECTED bit is set.
 * Need experience with more controllers to verify. */

static int INITPROC xd_reset(void)
{
	int in, i = 100;

#if 1
	int prev;
	outb_p(0, XD_RESET);
	mdelay(100);
	if ((prev = inb(XD_STATUS)) & STAT_SELECT) { /* should not be selected after reset */
		//printk(" reset: unexpected status 0x%x;", prev);
		//printk("(%x);", inb(XD_STATUS + 4));	/* DEBUG: poke for controller @ 0x324 */
		return 1;	/* controller not found */
	}
	//printk(" reset: %x;", prev);
#else
	printk(" DEBUG: not resetting controller;");
#endif
	/* FIXME: Use xd_waitport() */
	do {
		mdelay(100);		/* may not be needed */
		outb(0, XD_SELECT);
		in = inb_p(XD_STATUS);
		//if (in != prev) {
			//printk("%x;", in);
			//prev = in;
		//}
	} while (!(in & STAT_SELECT) && i--);

	/* Compaq controller returns 0xc8, 86box emulating 'IBM Fixed Disk Controller'
	 * returns 0x0d (SELECT, COMMAND, READY) */
	//printk(" xd reset returned %x;", in);
	return(!i);
}

/* INITPROC for now. Eventually, this function will be called from ioctl too. */
static void INITPROC xd_setparam(byte_t drive, byte_t heads, word_t cyls, 
						word_t wprecomp)
{
	byte_t cmdblk[14];

	xd_build(cmdblk, CMD_SETPARAM, drive, 0, 0, 0, 0, 0);
	cmdblk[6] = (byte_t) (cyls >> 8) & 0x03;
	cmdblk[7] = (byte_t) (cyls & 0xff);
	cmdblk[8] = heads & 0x0f;
	cmdblk[9] = 0;		/* Reduced current - not used */
	cmdblk[10] = 0;
	cmdblk[11] = (byte_t) (wprecomp >> 8) & 0x03;
	cmdblk[12] = (byte_t) (wprecomp & 0xff);
	cmdblk[13] = 0x0b;	/* 'Maximum length of an error burst
				 * to be corrected' - 11 is max and seemingly the default */

	/* Some controllers require geometry info as data, others as an extended cmd block. */
	/* This setup takes care of both */

	if (xd_command(cmdblk, PIO_MODE, NULL, &cmdblk[6], NULL, XD_TIMEOUT * 2))
		printk("xd: error setting characteristics for xd%d\n", drive);
}

void INITPROC xd_init(void)
{
    int i, err, drive, hdcount = 0;
    struct gendisk *ptr;

    //bounce_buffer = NULL;	/* not allocated yet */
    //xt_busy = 0;		/* probably not required */
    if (register_blkdev(MAJOR_NR, DEVICE_NAME, &xd_ops)) {
	printk("Unable to get major %d for xd-disk\n", MAJOR_NR);
	return;
    }
    blk_dev[MAJOR_NR].request_fn = DEVICE_REQUEST;

    err = request_irq(MHD_IRQ, do_xdintr, INT_GENERIC);
    if (err) {
    	printk("Unable to grab IRQ%d for XD driver\n", MHD_IRQ);
	return;
    }
    if (request_dma(MHD_DMA, (void *)DEVICE_NAME)) {
    	printk("md: Unable to get DMA%d for XD driver\n", MHD_DMA);
	return;
    }

    printk("xd: MFM disk controller @ 0x%x, irq %d, DMA %d", MHD_PORT, MHD_IRQ, MHD_DMA);

    if (xd_reset()) {	/* works as a probe */
	printk(" not found\n");
	return;
    }
    printk("\n");

    for (drive = 0; drive < MAX_XD_DRIVES; drive++) { /* initialize 1 until probe works */
    	struct drive_infot *dp = &drive_info[drive];
	int chs_src;

	/* Check if drive is present */
	if (xd_recal(drive)) {
		//printk("xd%d: no drive\n", drive); 	/* DEBUG - remove */
		continue;
	}

	/* Three (4) ways to determine drive geometry:
	 * 1) Use BIOS call - will fail if the BIOS is really old, or may
	 *    deliver wrong data (like 305/4/17 regardless).
	 * 2) Read from /bootopts - may not be present
	 * 3) Use defaults set above - inflexible (10M)
	 * 4) Read controller switches to get the drive type
	 *    (see the get_drive_type() function, may or may not work with
	 *     your controller and the coding is different from one to the next).
	 * It may be possible to peek into the BIOS code on the card too,
	 * but it gets very complicated and RAM intensive, so /bootopts seems like a 
	 * smart choice.
	 * Note: Old BIOSes will happily respond to CHS queries for non-existent drives!
	 */
#ifdef NOTYET
	struct biosparms bdt;

	bdt.ax = BIOSHD_DRIVE_PARMS;
	bdt.dx = drive + 0x80;
	bdt.es = bdt.di = bdt.si = 0;
	if (call_bios(&bdt) == 0) {
	    chs_src = 0;
	    dp->heads = (bdt.dx >>8) + 1;
	    dp->sectors = bdt.cx & 0x3f;
	    dp->cylinders = (((bdt.cx & 0xc0) << 2) | (bdt.cx >> 8)) + 1;
	} else
#endif
	i = drive*4;
	if (hdparms[i]) {
	    chs_src = 1;
	    dp->cylinders = hdparms[i];
	    dp->heads = hdparms[i+1];
	    dp->sectors = hdparms[i+2];
	    dp->wpcomp = hdparms[i+3] < 0 ? 0 : hdparms[i+3]; /* -1 means 0 */
	} else {
	    chs_src = 2;
	    dp->cylinders = DEF_CYLS;
	    dp->heads = DEF_HEADS;
	    dp->sectors = DEF_SECTS;
	    dp->wpcomp = DEF_WPCOMP;
	}

	hdcount++;
	printk("xd%d: CHS %d/%d/%d/%d %s\n", 
		drive, dp->cylinders, dp->heads, dp->sectors, dp->wpcomp,
		chs_src == 1 ? "(from /bootopts)" : "(preset)");

#ifdef ALLOW_FORMATTING
	if (dp->sectors < 0) {
		xd_setparam(drive, dp->heads, dp->cylinders, dp->wpcomp);
		printk("xd%d: Formatting drive. Change bootopts and reboot when done.\n", drive);
		xd_format(drive);
		printk("Looping ");
		while (1) {
			mdelay(5000);
			if (inb(XD_STATUS) & 1) {
				printk("Format done (%x);", inb(XD_STATUS));
				while (1);
			}
		}
	}
#endif
	xd_setparam(drive, dp->heads, dp->cylinders, dp->wpcomp);
    }
    if (!hdcount) {
    	printk("xd: No drives found\n");
	return;
    }
    xd_gendisk.nr_real = hdcount;
    blk_dev[MAJOR_NR].request_fn = DEVICE_REQUEST;
    if (gendisk_head == NULL) {
    	xd_gendisk.next = gendisk_head;
	gendisk_head = &xd_gendisk;
    } else {
    	for (ptr = gendisk_head; ptr->next != NULL; ptr = ptr->next);
	ptr->next = &xd_gendisk;
	xd_gendisk.next = NULL;
    }
    printk("xd: Found %d hard drive%c\n", hdcount, hdcount > 1? 's' : ' ');

    /* print drive info */
    for (i = 0; i < hdcount; i++)
	/* sanity check */
	if (drive_info[i].heads != 0) {
	    printk("xd%d: %d heads, %d cylinders, %d sectors (~%luMB)\n",
		   i, drive_info[i].heads, drive_info[i].cylinders, drive_info[i].sectors,
		   (((__u32)drive_info[i].heads*(__u32)drive_info[i].cylinders*
		     (__u32)drive_info[i].sectors)>>1)/1000);
	}

}

static void mdelay(int x)
{
	while (x--) outb_p(x, 0x80);
}

#ifdef NOTINUSE
static int get_drive_type(int drive) 	/* works for some, not all cards */
{
	int t = inb_p(XD_SELECT);	/* read jumper settings */

	return(drive ? t : (t>>2)&0x33);
}
#endif

static void deverror(int drive, byte_t *sense)
{
	register int i, j = sense[0]&0x3f;

	if ((j & 0x30) == 0x30 ) j = 0x30;
	if (j > 0x29 && j < 0x30) j = 0x2A;
	for (i = 0; xdmsg[i].num != 0; i++) {
	    if (xdmsg[i].num == j)
		break;
	}
	printk("xd%d: %s (%02X)", drive, xdmsg[i].msg, j);
	if ((j & 0x30) == 0x10) /* drive error, add blocknr */
				/* possibly get sector # from sense[] */
	    printk(" @ block %lu", CURRENT->rq_blocknr);
	printk("\n");
}

/* crude status wait loop */
static int xd_waitport(byte_t flags, byte_t mask, int timeout)
{
	unsigned long expiry = jiffies + (unsigned long)timeout;

	while (((inb_p(XD_STATUS) & mask) != flags) && (jiffies < expiry))
		;

	return (jiffies >= expiry);
}

/* xd_command: handle all data IO necessary for a single command */

/* NOTE: Be careful with printk's inside the switch. Commands initiating
 * DMA transfers will commence immediately after the last command byte,
 * and the interrupt may kick in before we're finished here, delivering
 * some times surprising results.
 * NOTE II: This function must be reentrant, it calls itself.
 */

static word_t xd_command(byte_t *command, byte_t mode, byte_t *indata,
			byte_t *outdata, byte_t *sense, int timeout)
{
	word_t csb;
	byte_t complete = 0;

	debug_xd("xd_command: cmd = 0x%X, mode = 0x%X, indata = 0x%X, outdata = 0x%X, sense = 0x%X\n",
			*command, mode, indata, outdata, sense);

	//printk("Cm0x%x;", command[0]);
	outb(0, XD_SELECT);
	outb(mode, XD_CONTROL);

	if (xd_waitport(STAT_SELECT, STAT_SELECT, timeout))
		return 1;

	while (!complete) {
		if (xd_waitport(STAT_READY, STAT_READY, timeout))
			return 10;

		switch (inb(XD_STATUS) & (STAT_COMMAND | STAT_INPUT)) {
			case 0:
				if (mode == DMA_MODE) {
					/* write via DMA command issued */
					complete++;
					break;
				} else {
					//printk("%02x#", *outdata);
					outb_p(outdata ? *outdata++ : 0, XD_DATA);
				}
				break;

			case STAT_INPUT:
				if (mode == DMA_MODE) {
					/* read via DMA command issued */
					complete++;
					break;
				} else {
					if (indata)
						*indata++ = inb_p(XD_DATA);
					else
						inb_p(XD_DATA);
				}
				break;

			case STAT_COMMAND:
				//printk("%02x$", *command);
				outb(command ? *command++ : 0, XD_DATA);
				break;

			case STAT_COMMAND | STAT_INPUT:
				complete++;
				break;
		}
	}
	if (mode == DMA_MODE) return 0;

	csb = inb_p(XD_DATA);
	//printk("csb %x;", csb);
	
	//if (csb == 0xff) csb = 0;	/* DEBUG FIXME (for 86Box) */

	if (xd_waitport(0, STAT_SELECT, timeout))	/* wait until deselected */
		return 3;

	/* Error processing for non-DMA commands */
	if (csb & CSB_ERROR) {
		byte_t cmd[6];
		xd_build(cmd, CMD_SENSE, (csb & CSB_LUN) >> 5,0,0,0,0,0);
		xd_command(cmd, PIO_MODE, sense, NULL, NULL, XD_TIMEOUT);
		//printk("sense %x|%x|%x|%x;", sense[0], sense[1], sense[2], sense[3]);
	}

	debug_xd("xd_command: completed with csb = 0x%X\n", csb);

	return (csb & CSB_ERROR);	/* returns either 0 or 2 */
}

/* xd_build: put stuff into an array in a format suitable for the controller */
static void xd_build(byte_t *cmdblk, byte_t command, byte_t drive, byte_t head,
		word_t cylinder, byte_t sector, byte_t count, byte_t control)
{
	cmdblk[0] = command;
	cmdblk[1] = ((drive & 0x07) << 5) | (head & 0x1F);
	cmdblk[2] = ((cylinder & 0x300) >> 2) | (sector & 0x3F);
	cmdblk[3] = cylinder & 0xFF;
	cmdblk[4] = count;
	cmdblk[5] = control;
}

/* xd_recal: recalibrate a given drive and reset controller if necessary */
static int xd_recal(int drive)
{
	byte_t cmd[6];

	inb(XD_DATA); /* empty data reg just in case */

	/* RECAL simply tells the drive to return to track zero */
	xd_build(cmd, CMD_RECAL, drive, 0, 0, 0, 0, XD_CNTF);
	return(xd_command(cmd, PIO_MODE, NULL, NULL, NULL, XD_TIMEOUT * 8));
}

#ifdef ALLOW_FORMATTING

#define XD_INTERLEAVE	5	/* use 5 for 4.77MHz, 4 if >= 8MHz */

/* Format the entire drive - a real kludge pending adding this to ioctl and
 * write a utility to use it. Useful because a drive will have to be formatted
 * by the controller in use, there is no swapping drives around.
 */
static void xd_format(int drive)
{
	byte_t cmd[6], sense[4];

	xd_build(cmd, CMD_FORMATDRV, drive, 0, 0, 0, XD_INTERLEAVE, XD_CNTF);
	if (xd_command(cmd, PIO_MODE, 0, 0, sense, XD_TIMEOUT*10))
		printk("xd%d: Format command returned 0x%x\n", drive, sense[0]);
}
#endif
