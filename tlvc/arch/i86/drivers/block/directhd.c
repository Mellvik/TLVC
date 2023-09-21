/*
 * directhd driver for ELKS kernel
 * Copyright (C) 1998 Blaz Antonic
 * 14.04.1998 Bugfixes by Alastair Bridgewater nyef@sudval.org
 * 17.04.2023 Rewritten for TLVC by helge@skrivervik.com (hs)
 * 01.07.2023 modified to handle any request size, support raw io & and multisector transfers (hs)
 */
/*
 * NOTE:
 * - This driver may or may not on 8088 and 8 bit bus systems,
 *   I don't think it will work with pre-IDE drives. HS
 */
/*
 * TODO (HS 04/23):
 * - create kernel library routines for insw & outsw, make sure they're used
 *   where local variants are used now (check all drivers).
 * - Create a library routine for delays/waits, many drivers have their own variant, wastes space.
 * - use interrupts - it will simplify the logic and improve reliability (and speed)
 * - test with 2 controllers, 4 drives
 * - add LBA support (will LBA work on a CHS initialized drive - or vise versa?)
 * - recognize the presence of solid state devices (to remove request sorting)
 */
/*
 * A note about IDE/ATA:
 * The IDE read/write sector commands initiate multisector I/O but each sector needs its own read
 * or write operation: One command, many response-iterations - 
 * like waiting for a new DRQ (or interrupt) per sector.
 *
 * The Read/Write Multiple cmds are different, one command, one response, but requires the # of
 * sectors in the ReadM or WriteM command to match that specified in the preceding
 * Set Multiple command. If device ID word 47 bits 7:0 are zero, multisector 
 * read/writes are not supported. Otherwise, the field holds the max # of sectors per
 * transaction.
 * This driver uses 2 for block access, whatever is allowed for raw IO. 
 *
 * NOTE2: In addition to ID word 47, the SET_MULTIPLE command is used to doublecheck the availability
 * multisector transfers. If the command fails (as issued in the _init routine), it's not.
 * More about multisector transfers in the code comments below.
 * The read/write multiple support code is encapsulated by ifdefs and may be left out w/o
 * loss of functionality.
 * DMA transfers are possible with most modern IDE devices but not supported by this 
 * driver and not really meaningful because it may be slower than PIO.
 *
 * NOTE3: Old disk drives (1980s) are very slow and require (among other things) delays
 * between polls of the status and result registers. When tuning these delays the 
 * advantages of using interrupts instead, become obvious.
 * And attempt has been made to minimize the (negative) effects of the delays for modern drives.
 */

#include <linuxmt/major.h>
#include <linuxmt/genhd.h>
#include <linuxmt/fs.h>
#include <linuxmt/string.h>
#include <linuxmt/mm.h>
#include <linuxmt/heap.h>
#include <linuxmt/directhd.h>
#include <linuxmt/debug.h>
#include <linuxmt/errno.h>

#include <arch/hdreg.h>
#include <arch/ports.h>
#include <arch/io.h>
#include <arch/segment.h>

#define STATUS(port) inb_p((port) + ATA_STATUS)
#define ERROR(port) inb_p((port) + ATA_ERROR)
#define SECTOR(port) inb_p((port) + ATA_SECTOR)
#define WAITING(port) ((STATUS(port) & BUSY_STAT) == BUSY_STAT)
#define DRQ_WAIT(port) (STATUS(port) & DRQ_STAT) /* set when ready to transfer */

/* #define USE_ASM */
/* use asm insw/outsw instead of C version */
/* asm versions should work on 8088/86, but only with CONFIG_PC_XT */

/* We've instructed GCC to generate 8086 code, this does not fit */
/* FIXME: Use #pragmas */
#define port_write(port,buf,nr) \
__asm__("cld;rep;outsw"::"d" (port),"S" (buf),"c" (nr))


#define USE_MULTISECT_IO	/* Enable/disable multisector R/W - for debugging */
//#define USE_INTERRUPTS		/* EXPERIMENTAL - test interupts */
#define OLD_IDE_DELAY 1000	/* delay required for old drives, system dependent */
#define DEBUG

//#define USE_LOCALBUF		/* For debugging: use local bounce buffer instead of */
				/* direct buffer io via far pointers.
				 * When XMS buffers are active, the same 1k buffer
				 * is used for bouncing. Price: 1k bytes */
#define MAJOR_NR ATHD_MAJOR
#define MINOR_SHIFT	5
#define ATDISK
#include "blk.h"

int directhd_ioctl();
int directhd_open();
void directhd_release();
static void mdelay(int);

static struct file_operations directhd_fops = {
    NULL,			/* lseek */
    block_read,			/* read */
    block_write,		/* write */
    NULL,			/* readdir */
    NULL,			/* select */
    directhd_ioctl,		/* ioctl */
    directhd_open,		/* open */
    directhd_release		/* release */
};

/* MAX_ATA_DRIVES is set in directhd.h - to save RAM, reduce to 2 */
static int access_count[MAX_ATA_DRIVES] = { 0, };

static int io_ports[2] = { HD1_PORT, HD2_PORT };
static int cmd_ports[2] = { HD1_CMD, HD2_CMD };

#if defined(USE_LOCALBUF) || defined(CONFIG_FS_XMS_BUFFER)
static char localbuf[BLOCK_SIZE];	/* bounce buffer for debugging and
					 * XMS buffer bouncing */
#endif

#define PORT_IO	port_io
void (*PORT_IO)() = NULL;

static int directhd_initialized = 0;
static struct drive_infot drive_info[MAX_ATA_DRIVES] = { 0, };

/* NOTE: This is wasting a lot of memory, allocating 32 entries times MAX_ATA_DRIVES,
 * -> 128 entries while we may need 16, 32 at the most. Each entry = 8 bytes,
 * save potential > 768 bytes */
static struct hd_struct hd[MAX_ATA_DRIVES << MINOR_SHIFT]; /* partition pointer {start_sect, num_sects} */
static int  directhd_sizes[MAX_ATA_DRIVES << MINOR_SHIFT] = { 0, };

static void directhd_geninit();
static void reset_controller(int);
static int drive_busy(int);
static void directhd_int(int, struct pt_regs *);

static struct gendisk directhd_gendisk = {
    MAJOR_NR,			/* major: major number */
    DEVICE_NAME,		/* major_name: device name */
    MINOR_SHIFT,		/* minor_shift: # rightshifts to get real minor  */
    1 << MINOR_SHIFT,		/* max_p: Max partitons, FIXME change to 4 */
    MAX_ATA_DRIVES,
    directhd_geninit,		/* init */
    hd,				/* hd struct */
    directhd_sizes,		/* drive sizes */
    0,
    (void *) drive_info,
    NULL
};

void directhd_geninit(void)
{
    struct drive_infot *drivep;
    struct hd_struct *hdp = hd;
    int i;

    drivep = drive_info;
    for (i = 0; i < MAX_ATA_DRIVES << MINOR_SHIFT; i++) {
	if ((i & ((1 << MINOR_SHIFT) - 1)) == 0) {
	    hdp->start_sect = 0;
	    hdp->nr_sects = (sector_t)drivep->sectors *
		drivep->heads * drivep->cylinders;
	    //printk("at%d: %ld$", i, hdp->nr_sects);
	    drivep++;
	} else {
	    hdp->start_sect = -1;
	    hdp->nr_sects = 0;
	}
	hdp++;
    }
    return;
}

/* assumes current data segment - which is kernel_ds */
void insw(unsigned int port, word_t *buffer, int count)
{
    count >>= 1;
    //printk("insw %x,%x,%d;", port, buffer, count);
    do {
	*buffer++ = inw(port);
    } while (--count);
}


void read_data(unsigned int port, ramdesc_t seg, word_t *buffer, int count, int raw)
{

#if defined(CONFIG_FS_XMS_BUFFER) || defined(USE_LOCALBUF) /* use bounce buffer */
    if (!raw) {	
	insw(port, (word_t *)localbuf, count);
	//printk("insw %d %04x %04x %04x;", count, localbuf, buffer, *(word_t *)localbuf);
	xms_fmemcpyw(buffer, seg, localbuf, kernel_ds, count/2);
    } else
#endif
    {

	unsigned int __far *locbuf = _MK_FP(seg, (unsigned)buffer);

	count >>= 1;	/* bytes -> words */
	//printk("%x,%x,%x,%lx,%d;", port, buffer, seg, locbuf, count);
	do {
	    *locbuf++ = inw(port);
	} while (--count);
    }
}

#if defined(USE_LOCALBUF) || defined(CONFIG_FS_XMS_BUFFER)

void outsw(unsigned int port, word_t *buffer, int count)
{
    int i;

    count >>= 1;
    //printk("%04x:", buffer);
    for (i = 0; i < count; i++) {
	outw(buffer[i], port);
    }
    return;
}
#endif

void write_data(unsigned int port, ramdesc_t seg, word_t *buffer, int count, int raw)
{
#if defined(CONFIG_FS_XMS_BUFFER) || defined(USE_LOCALBUF)

    if (!raw) {
	xms_fmemcpyw(localbuf, kernel_ds, buffer, seg, count/2);
	outsw(port, (word_t *)localbuf, count);
    } else 
#endif
    {
	unsigned int __far *locbuf = _MK_FP(seg, (unsigned)buffer);

	count >>= 1;
	//printk("%x,%x,%x,%lx,%d;", port, buffer, seg, locbuf, count);
	do {
	    outw(*locbuf++, port);
	} while (--count);
    }
}

#if 0				/* not used */

void swap_order(unsigned char *buffer,int count)
{
    int i;
    char tmp;

    for (i = 0; i < count; i++)
	if ((i % 2) == 0) {
	    tmp = *(buffer + i + 1);
	    *(buffer + i + 1) = *(buffer + i);
	    *(buffer + i) = tmp;
	}
    return;
}

#endif

void out_hd(unsigned int drive, unsigned int nsect, unsigned int sect,
	    unsigned int head, unsigned int cyl, unsigned int cmd)
{
    word_t port;

    port = io_ports[drive >> 1];

    /* setting WPCOM to 0 is not good. this change uses the last value input to
     * the drive. (my BIOS sets this correctly, so it works for now but we should
     * fix this to work properly)  -- Alastair Bridgewater */
    /* this doesn't matter on newer (IDE) drives, but is important for MFM/RLLs
     * I'll add support for those later and we'll need it then - Blaz Antonic */
    /* meanwhile, I found some documentation that says that for IDE drives
     * the correct WPCOM value is 0xff. so I changed it. - Alastair Bridewater */

    //outb_p(0x20, ++port);		/* means 128 (x4), test value for conner 40M */
    outb_p(0xff, ++port);		/* the supposedly correct value for WPCOM on IDE */
    outb_p(nsect, ++port);
    outb_p(sect, ++port);
    outb_p(cyl, ++port);
    outb_p(cyl >> 8, ++port);
    outb_p(0xA0 | ((drive & 1) << 4) | head, ++port); /* setup for 2 drives, fix for 4 (&2 instead) */
    outb(cmd, ++port);

    return;
}
#ifdef DEBUG
static void dump_ide(word_t *buffer, int size) {
        int counter = 0;

        do {
                printk("%04X ", *buffer++);
                if (++counter == 16) {
                        printk("\n");
                        counter = 0;
                }
        } while (--size);
        if (counter) printk("\n");
}
#endif

int INITPROC directhd_init(void)
{
    word_t *ide_buffer = (word_t *)heap_alloc(512, 0);
    struct gendisk *ptr;
    int i, hdcount = 0, drive;
    unsigned int port;

    /* .. once for each drive */
    /* note, however, that this breaks (hangs) if you don't have two IDE interfaces
     * in your computer. If you only have one, change the 4 to a 2.
     * (this explains why your computer was locking up after mentioning the
     */
    /* "If Drive 1 is not detected as being present, Drive 0 clears the Drive
     * 1 Status Register to 00h." From the spec. Making ST=0 a safe indication of
     * non presence.
     * Also, we should do a CMOS check for the number of drives, which would make 
     * this logic faster and more reliable FIXME */ 

    /* FIXME: AMI board hangs when trying to access 2nd controller, disable for now */
    /* Maybe just try polling the address to see if there is anything */

    for (drive = 0; drive < 2/*MAX_ATA_DRIVES*/; drive++) {
	if (!drive&1) reset_controller(drive/2);
	/* send drive_ID command to drive */
	out_hd(drive, 0, 0, 0, 0, ATA_DRIVE_ID);

	port = io_ports[drive / 2];

	/* wait -- if status is 0xff, there is no drive with this number */
	mdelay(OLD_IDE_DELAY);
	i = STATUS(port);
	if (!i || (i & 1) == 1) { /* this one may not be safe FIXME */
	    /* error - drive not found or non-ide */

	    //printk("athd%d (on port 0x%x) not found\n", drive, port);
	    continue;	/* Proceed with next drive.
			 * Always do this, even if the master drive
			 * is missing.  */
	}

	/* get drive info */
	while (WAITING(port)) mdelay(OLD_IDE_DELAY);

	insw(port, ide_buffer, 512);
#if 0
	swap_order(buffer, 512);
#endif
	/* Gather useful drive info - note that text bytes are swapped.
	 *
	 * Safety check - check for heads returned and assume CD
	 * if we get typical CD response .. this is a good place for
	 * ATAPI ID, but it'd only enlarge the kernel and no HD can
	 * have 60416 heads :-))) .. 4096 heads should cover it,
	 * IDE will be dead by that time
	 *
	 * Maybe we can assume some specific number here ? i always
	 * get 60416 with ATAPI (Mitsumi) CD - Blaz Antonic
	 *
	 * Safety check - head, cyl and sector values must be other than
	 * 0 and buffer has to contain valid data (first entry in buffer
	 * must be other than 0)
	 *
	 * FIXME: This will cause problems on many new drives .. some
	 * of them even have more than 16.384 cylinders
	 *
	 * This is some sort of bugfix, we will use same method as real Linux -
	 * work with disk geometry set in current translation mode (54-56) if valid (53)
	 * rather than physical drive info. Also, old drives have only physical, which may
	 * even be misleading: The configured BIOS drive type's CHS mapping may be different.
	 * E.g the Compaq drive type 17 (conner 42MB) reports 806/4/26 while the BIOS
	 * values are 980/5/17. Both work, but aren't interchangeable, so the BIOS value 
	 * wins for compatibility with other OSes. How do we get those values (not using
	 * a BIOS call)? Probably a bootopts setting: Like CHS0=960,5,17,128 - the latter 
	 * being the WPCOM - write precompensation, which may be significant for 
	 * drives that old. (HS)
	 */

	struct drive_infot *dp = &drive_info[drive];
	dp->ctl = 0;
#ifdef DEBUG
	dump_ide(ide_buffer, 64);
	//ide_buffer[53] = 0; /* force old ide behaviour for debug */
#endif
	ide_buffer[20] = 0; /* String termination */
	if ((ide_buffer[54] < 34096) && (*ide_buffer != 0)) {
	    /* Physical CHS data @ (word) offsets: cyl@1, heads@3, sectors@6 */
	    /* Actual CHS data @ (word) offsets: cyl@54, heads@55, sectors@56 */

	    dp->multio_max = ide_buffer[47] & 0xff; /* max sectors per multi io op */
	    					    /* zero if unsupported */
	    if (ide_buffer[53]&1) {	/* check the 'validity bit'. If set, use 
	    				 * 'current' values, otherwise defaults.
					 * Usually indicates old vs new tech. */
		dp->cylinders = ide_buffer[54];
		dp->heads = ide_buffer[55];
		dp->sectors = ide_buffer[56];
	    } else {		/* old drive, limited ID, limited cmd set */
/* !!!!!!!!!!!!!!!!!!!  FIXME: hardcoded values for debugging, remove later */
		dp->cylinders = 980/*ide_buffer[1]*/;
		dp->heads = 5/*ide_buffer[3]*/;
		dp->sectors = 17/*ide_buffer[6]*/;
		dp->ctl |= ATA_CFG_OLDIDE;
	    }

	    hdcount++;
	    printk("IDE CHS: %d/%d/%d serial# %s\n", dp->cylinders, dp->heads, dp->sectors,
		&ide_buffer[10]);

	    /* Initialize settings. Some (old in particular) drives need this
	     * and will default to some odd default values otherwise */
	    /* NOTE: In older docs this cmd is known as 'Initialize Drive Parameters' */
	    out_hd(drive, dp->sectors, 0, dp->heads - 1, 0, ATA_SPECIFY);
	    while(WAITING(port)) mdelay(1000);
	    if (STATUS(port) & ERR_STAT) printk("err %x;", ERROR(port)); /* DEBUG */

#ifdef USE_MULTISECT_IO	
	    if (dp->multio_max) {
		/* Set multiple IO mode, default to 2 sectors per op - if available */
		/* EDIT: set to max always, experimental */
		out_hd(drive, dp->multio_max, 0, 0, 0, ATA_SET_MULT);
		while (WAITING(port));
		if (!(STATUS(port) & ERR_STAT)) {
		    printk("athd%d: Multisector I/O, max %d sects\n", drive, dp->multio_max);
		} else {
		    printk("athd%d: Multisector I/O failed (%x), turned off\n", ERROR(port));
		    dp->multio_max = 0;
		}
	    }
#endif	/* USE_MULTISECT_IO */

	    //printk("athd%d: IDE data 47-49: %04x, %04x, %04x\n", drive, 
		//ide_buffer[47], ide_buffer[48], ide_buffer[49]);
	} else
	    printk("athd%d: No valid drive ID\n", drive);
    }

    heap_free(ide_buffer);
    if (!hdcount) {
	printk("athd: no drives found\n");
	return 0;
    }

    directhd_gendisk.nr_real = hdcount;
 
    if (register_blkdev(MAJOR_NR, DEVICE_NAME, &directhd_fops)) {
	printk("athd: unable to register\n");
	return -1;
    }

#ifdef USE_INTERRUPTS	/* Experimental */
    /* TEST this on 8 bit bus machines! (irq 5) */
    /* On AT and higher, add irq reg for 2nd card if present - irq 15/HD2_AT_IRQ */
    int got_irq = HD1_AT_IRQ;
    printk("athd: Interrupt registration: ");
    if (request_irq(got_irq, directhd_int, INT_GENERIC)) {
	got_irq = HD_IRQ;
	if (request_irq(got_irq, directhd_int, INT_GENERIC))
	    got_irq = 0;
    }
    if (got_irq)
    	printk("%d\n", got_irq);
    else
    	printk("failed\n");
#endif

    blk_dev[MAJOR_NR].request_fn = DEVICE_REQUEST;
    if (gendisk_head == NULL) {
	directhd_gendisk.next = gendisk_head;
	gendisk_head = &directhd_gendisk;
    } else {
	for (ptr = gendisk_head; ptr->next != NULL; ptr = ptr->next);
	ptr->next = &directhd_gendisk;
	directhd_gendisk.next = NULL;
    }

    printk("athd: found %d hard drive%c\n", hdcount, hdcount == 1 ? ' ' : 's');

    /* print drive info */
    for (i = 0; i < 4; i++)
	/* sanity check */
	if (drive_info[i].heads != 0) {
	    printk("athd: /dev/dhd%c: %d heads, %d cylinders, %d sectors (~%luMB)\n",
		   (i + 'a'),
		   drive_info[i].heads,
		   drive_info[i].cylinders, drive_info[i].sectors,
		   (((__u32)drive_info[i].heads*(__u32)drive_info[i].cylinders*
		     (__u32)drive_info[i].sectors)>>1)/1000);
	}

    directhd_initialized = 1;
    return 0;
}

int directhd_ioctl(struct inode *inode, struct file *filp,
		unsigned int cmd, unsigned int arg)
{
    struct hd_geometry *loc = (struct hd_geometry *) arg;
    int dev, err;

    if ((!inode) || !(inode->i_rdev))
	return -EINVAL;

    dev = DEVICE_NR(inode->i_rdev);
    if (dev >= MAX_ATA_DRIVES)
	return -ENODEV;

    switch (cmd) {
    case HDIO_GETGEO:
	/* safety check .. i presume :) */
	err = verify_area(VERIFY_WRITE, (void *)arg, sizeof(struct hd_geometry));
	if (err)
	    return err;
	put_user_char(drive_info[dev].heads, &loc->heads);
	put_user_char(drive_info[dev].sectors, &loc->sectors);
	put_user(drive_info[dev].cylinders, &loc->cylinders);
	put_user_long(hd[MINOR(inode->i_rdev)].start_sect, &loc->start);
	return 0;
	break;
    }
    return -EINVAL;
}

/*
 * NOTE: open is used by the char driver too!
 */

int directhd_open(struct inode *inode, struct file *filp)
{
    unsigned int minor = MINOR(inode->i_rdev);
    int target = DEVICE_NR(inode->i_rdev);

    //printk("ATH open: target %d, minor %d, start_sect %ld\n", target, minor, hd[minor].start_sect);
    if (target >= 4 || !directhd_initialized)
	return -ENXIO;

    if (((int) hd[minor].start_sect) == -1)	/* FIXME is this initialized */
	return -ENXIO;

    access_count[target]++;

    /* something should be here, but can't remember what :)
     * it really isn't important until probe code works
     *
     * probe code works now but i still can't remember what is missing.
     * any clues ?
     */

    inode->i_size = (hd[minor].nr_sects) << 9;
    /* limit inode size to max filesize for CHS >= 4MB (2^22)*/
    if (hd[minor].nr_sects >= 0x00400000L)	/* 2^22*/
        inode->i_size = 0x7ffffffL;		/* 2^31 - 1*/
    debug_blkdrv("dhd[%04x] open, size %ld\n", inode->i_rdev, inode->i_size);
    return 0;
}

void directhd_release(struct inode *inode, struct file *filp)
{
    int target = DEVICE_NR(inode->i_rdev);
    kdev_t dev = inode->i_rdev;

    access_count[target]--;
    fsync_dev(dev);			/* cannot trust umount to do this */
    if (!access_count[target]) {	/* don't do this if raw device */
	invalidate_buffers(dev);
	invalidate_inodes(dev);
    }

    return;
}

/*
 * 06/23 HS: Added buffer header manipulation to handle raw IO
 * 07/23 HS: Added delays to accommodate old (early) drives 
 */
void do_directhd_request(void)
{
    unsigned int count;		/* # of sectors to read/write */
    sector_t start;		/* first sector */
    unsigned char *buff;	
    short sector;		/* 1 .. 63 ? */
    short cylinder;		/* 0 .. 1024 and maybe more */
    short head;			/* 0 .. 16 */
    unsigned int tmp;
    int minor;
    int drive;			/* 0 .. 3 */
    int port;
    int cmd, delay;
    struct drive_infot *dp;
    struct request *req;

    while (1) {			/* process HD requests */
	req = CURRENT;

	INIT_REQUEST(req);

	if (directhd_initialized != 1) {
	    end_request(0);
	    //continue;		/* no reason to continue */
	    return;
	}

	minor = MINOR(req->rq_dev);
	drive = minor >> MINOR_SHIFT;
	dp = &drive_info[drive];
	delay = (dp->ctl&ATA_CFG_OLDIDE) ? OLD_IDE_DELAY : 0;

	/* check if drive exists */
	if (drive > 3 || drive < 0 || dp->heads == 0) {
	    printk("Non-existent drive\n");
	    end_request(0);
	    continue;
	}

	/* count is now the # of sectors
	 * to read, not the FS (or system) 
	 * block size.
	 */
#ifdef CONFIG_BLK_DEV_CHAR
	count = req->rq_nr_sectors ? req->rq_nr_sectors : BLOCK_SIZE / 512;
#else
	count = BLOCK_SIZE / 512;
#endif

	start = req->rq_blocknr;
	buff = req->rq_buffer;
	/* safety check should be here */
	//debug_blkdrv("dhd[%04x]: start: %lu nscts: %lu\n", req->rq_dev,
			//hd[minor].start_sect, hd[minor].nr_sects);
#ifdef DEBUG
	//printk("BF: %lx:%04x;", (unsigned long)req->rq_seg, buff);
#endif

	if (hd[minor].start_sect == -1 || hd[minor].nr_sects < start) {
	    printk("Bad partition start or block out of bounds: %lu\n", start);
	    end_request(0);
	    break;
	}
	/* may want to check for count = 1 - some drives don't like 1 sector in multi_mode */
	if (req->rq_cmd == READ) {
	    cmd = (!dp->multio_max)? ATA_READ : ATA_READM; 
	    PORT_IO = read_data;
	} else {
	    cmd = (!dp->multio_max)? ATA_WRITE : ATA_WRITEM;
	    PORT_IO = write_data;
	}

	start += hd[minor].start_sect;

	sector = (start % dp->sectors) + 1;
	tmp = start / dp->sectors;
	head = tmp % dp->heads;
	cylinder = tmp / dp->heads;
	port = io_ports[drive / 2];

#ifdef DEBUG
	//printk("IOPa %x;", STATUS(port));
	debug_blkdrv("athd%d: CHS %d/%d/%u st: %lu cnt: %d buf: %04x seg: %lx %04x/%c\n",
		 drive, cylinder, head, sector, start, count, buff, 
		(unsigned long)req->rq_seg, *(int *)buff, req->rq_cmd == READ? 'R' :'W');
#endif

	/* Send drive parameters */

	/* NOTE: Ideally we should check that the drive is ready to take commands
	 * before issuing one. However, if the previous transaction (command) was
	 * with a different drive, that's the source we're getting status from.
	 * So we issue the command and do the extra error checking instead. */

	/* BTW - on old drives, where this is an issue, there is no NOOP cmd
	 * and thus no reasonable way to switch drive selection to the other unit
	 * except issuing a regular command. A non existing command would do, 
	 * but that's time consuming - and not neccessarily predictable. */

	out_hd(drive, count, sector, head, cylinder, cmd);

	//printk("IOP1 %x;", STATUS(port));
	while (WAITING(port)) mdelay(delay);

	if ((STATUS(port) & ERR_STAT) == ERR_STAT) { /* something went wrong */
		printk("athd: IO status: 0x%x error: 0x%x CHS[%u/%u/%u]\n",
			STATUS(port), ERROR(port), cylinder, head, SECTOR(port));
		end_request(0);
		return;
	}
	/* Wait for DRQ to trigger the actual IO */
	tmp = 0x00;
	while ((tmp & DRQ_STAT) != DRQ_STAT) {
		if ((tmp & ERR_STAT) == ERR_STAT) {
		    printk("athd: RD DRQ status: 0x%x error: 0x%x\n",
			       STATUS(port), ERROR(port));
		    end_request(0);
		    return;
		} else {
		    tmp = STATUS(port);
		    //debug_blkdrv("athd%d: statusb 0x%x\n", drive, tmp);
		}
	}
#define raw_mode tmp
#ifdef CONFIG_BLK_DEV_CHAR
	if (req->rq_nr_sectors)
		raw_mode = 1; /* flag raw IO */
	else
#endif
		raw_mode = 0;

	/*
	 * Do the I/O. IDE will accept sector count up to 256, and will interrupt
	 * per sector in 'normal' mode, per block in multi_io mode. The block size is 
	 * set via the SET_MULTIPLE command, we use the max number allowed by the drive.
	 * Requesting fewer the the block size is OK.
	 */

	int blksize = dp->multio_max ? dp->multio_max : 1;	/* 1 or max */
	int requested = count, l;

	do {
	    	int step = requested < blksize ? requested : blksize;

	    	//printk("doIO %d;", step*512);
		while (!DRQ_WAIT(port));
		PORT_IO(port, req->rq_seg, (word_t *)buff, step*512, raw_mode);
		while(WAITING(port)) l++;	/* dummy increment */
	    	//printk("gotIO %x/%04x;", STATUS(port), *(int *)buff);
		if (STATUS(port) & ERR_STAT) {
		    /* May want the full CHS here */
		    printk("athd%d multisector R/W error %x sector %d\n", drive,
		    	ERROR(port), SECTOR(port));
		    /* Older drives may develop bad sectors,
		     * and other problems - that's a hard error:
		     * The drive has already retried a number of times. */
		    end_request(0);
		    return;
		}
		buff += step<<9;
		requested -= step;
	} while (requested);

	//printk("ATHD: bl %lu/%04x/%d/%04x/%c\n", start/2, *(int *)buff, count, 
		//req->rq_bh, req->rq_cmd == READ? 'R' :'W');
	end_request(1);
    }
    return;
}

/*
 * NOTE: Toggle the soft reset bit for the controller
 */
static void reset_controller(int controller)
{
	int	i;
	int	cport = cmd_ports[controller];

	outb_p(4, cport);		/* reset controller */
	mdelay(3000);
	//outb(hd_info[0].ctl & 0x0f, HD1_CMD);
#ifdef USE_INTERRUPTS
	outb_p(0, cport);		/* enable interrupts */
#else
	outb_p(2, cport);		/* Remove reset signal, disable interrupts */
#endif
	if ((i = drive_busy(io_ports[controller])))
		printk("athd%i: still busy (%x)\n", controller, i);
	if ((i = ERROR(io_ports[controller])) != 1)
		printk("athd%i: Reset failed: %02x\n", controller, i);
}

static void directhd_int(int unused, struct pt_regs *unused1)
{
	printk("X");
}

/*
 * FIXME:
 * Delay loop and status test, should do this with a micro_delay timer a la minix 
 */
static int drive_busy(int port)
{
	unsigned int i;
	unsigned char c;

	for (i = 0; i < 50000; i++) {
		// FIXME: More bits to test here? Check other drivers.
		c = STATUS(port) & (BUSY_STAT | READY_STAT);
		if (c == READY_STAT)
			return 0;
	}
	return(c);
}

/* these delays are required for the oldest of drives only */
static void mdelay(int x) 
{
	while (x--) outb_p(x, 0x80);
}
