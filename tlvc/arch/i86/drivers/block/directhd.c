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
#define WAITING(port) ((STATUS(port) & BUSY_STAT) == BUSY_STAT)
#define DRQ_WAIT(port) (STATUS(port) & DRQ_STAT) /* set when ready to transfer */

/* #define USE_ASM */
/* use asm insw/outsw instead of C version */
/* asm versions should work on 8088/86, but only with CONFIG_PC_XT */

/* We've instructed GCC to generate 8086 code, this does not fit */
/* FIXME: Use #pragmas */
#define port_write(port,buf,nr) \
__asm__("cld;rep;outsw"::"d" (port),"S" (buf),"c" (nr))


#define USE_MULTISECT_IO	/* Enable multisector R/W */
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

/*
 * FIXME Maybe we can merge all this stuff into one piece of code thet handles 
 * all conditions? With the possible exception of moving data into XMS buffers...
 */

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
    if (!raw) {				// FIXME: Fix indentation
    while (count) {
	int chars = (count > BLOCK_SIZE)? BLOCK_SIZE : count;

	insw(port, (word_t *)localbuf, chars);
	printk("insw %d %04x %04x %04x;", chars, localbuf, buffer, *(word_t *)localbuf);
#ifdef CONFIG_FS_XMS_BUFFER
	xms_fmemcpyw(buffer, seg, localbuf, kernel_ds, chars/2);
#else
	fmemcpyw(buffer, seg, localbuf, kernel_ds, chars/2);
#endif
	count -= chars;
	buffer += chars/2; 
    }
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

#if 0

/* this _should_ work on an 8088/86 now, but I haven't tested it yet */

void insw(unsigned int port,unsigned int *buffer,int count)
{
#asm
	push	bp
	mov	bp, sp
	push	cx
	push	di
	push	dx
	mov	dx, [bp + 04]
	mov	di, [bp + 06]
	mov	cx, [bp + 08]
	shr	cx, 1
	cld
#ifndef CONFIG_PC_XT
	repz
	insw
#else
			/* this should work on an 8088 */
dhd_insw_loop:
	in	ax, dx
	stosw
	loop	dhd_insw_loop
#endif
	pop	dx
	pop	di
	pop	cx
	pop	bp
#endasm
}

#endif

#if defined(USE_LOCALBUF) || defined(CONFIG_FS_XMS_BUFFER)

void outsw(unsigned int port, word_t *buffer, int count)
{
    int i;

    count >>= 1;
    //printk("%04x:", buffer);
    for (i = 0; i < count; i++) {
	//if (i < 16) printk("%04x|", buffer[i]); else printk("%d!",i);
	outw(buffer[i], port);
    }
    return;
}
#endif

void write_data(unsigned int port, ramdesc_t seg, word_t *buffer, int count, int raw)
{
#if defined(CONFIG_FS_XMS_BUFFER) || defined(USE_LOCALBUF)

    int chars;

    if (!raw) {
    while (count) {
	chars = (count > BLOCK_SIZE) ? BLOCK_SIZE : count;
#ifdef CONFIG_FS_XMS_BUFFER
	xms_fmemcpyw(localbuf, kernel_ds, buffer, seg, chars/2);
#else
	fmemcpyw(localbuf, kernel_ds, buffer, seg, chars/2);
#endif
	outsw(port, (word_t *)localbuf, chars);
	count -= chars;
	buffer += chars/2;
    }
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


#if 0

/* the assembler version. Again, this should work,
 * but I haven't had a chance to test it yet.
 */

void outsw(unsigned int port,unsigned int *buffer,int count)
{
#asm
	push	bp
	mov	bp, sp
	push	cx
	push	si
	push	dx
	mov	dx, [bp + 04]
	mov	si, [bp + 06]
	mov	cx, [bp + 08]
	shr	cx, 1
	cld
#ifndef CONFIG_PC_XT
	repz
	outsw
#else
dhd_outsw_loop:		/* this should work on an 8088 */
	lodsw
	out	dx, ax
	loop	dhd_outsw_loop
#endif
	pop	dx
	pop	si
	pop	cx
	pop	bp
#endasm
}

#endif

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
     * serial port, doesn't it? :-) -- Alastair Bridgewater */
    /* this should work now, IMO - Blaz Antonic */
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
	//printk("st%x;", i);
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
	/* Gather useful info - note that text bytes are swapped.
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
	 * rather than physical drive info. Old drives have only physical, which may
	 * be misleading: The copnmfigured BIOS drive type's CHS mapping may be different.
	 * E.g the Compaq drive type 17 (conner 42MB) reports 806/4/26 while the BIOS
	 * values are 980/5/17. Both work, but aren't interchangeable, so the BIOS value 
	 * wins for compatibility with other OSes. How do we get those values (not using
	 * a BIOS call)? The most flexible way seems to be a bootopts setting: Like
	 * CHS0=960,5,17,128 - the latter being the WPCOM, which is significant for 
	 * drives that old. (HS)
	 */

	struct drive_infot *dp = &drive_info[drive];
	dp->ctl = 0;
#ifdef DEBUG
	dump_ide(ide_buffer, 64);
	ide_buffer[20] = 0; /* String termination */
	//ide_buffer[53] = 0; /* force old ide behaviour for debug */
#endif
	if ((ide_buffer[54] < 34096) && (*ide_buffer != 0)) {
	    /* Physical CHS data @ (word) offsets: cyl=1, heads=3, sectors=6 */
	    /* Actual CHS data @ (word) offsets: cyl=54, heads=55, sectors=56 */

	    if (ide_buffer[53]&1) {	/* check the 'validity bit'. If set, use 
	    				 * 'current' values, otherwise defaults.
					 * Usually indicates old vs new tech. */
		dp->cylinders = ide_buffer[54];
		dp->heads = ide_buffer[55];
		dp->sectors = ide_buffer[56];
		dp->MAX_ATA_SPIO = ide_buffer[47] & 0xff; /* max sectors per multi io op */
	    } else {		/* old drive, limited ID, limited cmd set */
		dp->cylinders = 980/*ide_buffer[1]*/;
		dp->heads = 5/*ide_buffer[3]*/;
		dp->sectors = 17/*ide_buffer[6]*/;
		dp->MAX_ATA_SPIO = 1;
		dp->ctl |= ATA_CFG_OLDIDE|ATA_CFG_NMULT; /* old implies no-multi */
	    }

	    hdcount++;
	    printk("IDE CHS: %d/%d/%d serial# %s\n", dp->cylinders, dp->heads, dp->sectors,
		&ide_buffer[10]);

	    /* Initialize settings, some (old in particular) drives need this
	     * and will default to some odd default values otherwise */
	    /* NOTE: In older docs this cmd is known as 'Initialize Drive Parameters' */
	    out_hd(drive, dp->sectors, 0, dp->heads - 1, 0, ATA_SPECIFY);
	    while(WAITING(port)) mdelay(1000);
	    if (STATUS(port) & ERR_STAT) printk("err %x;", ERROR(port)); /* DEBUG */

#ifdef USE_MULTISECT_IO
	    if (dp->MAX_ATA_SPIO > 1) {
		/* Set multiple IO mode, default to 2 sectors per op - if available */
		out_hd(drive, 2, 0, 0, 0, ATA_SET_MULT);
		while (WAITING(port));
		if (!(STATUS(port) & ERR_STAT)) 
		    printk("athd%d: Multisector I/O, max %d sects\n", drive, dp->MAX_ATA_SPIO);
		else
		    dp->ctl |= ATA_CFG_NMULT;
	    } else
#endif	/* USE_MULTISECT_IO */
	    dp->ctl |= ATA_CFG_NMULT;

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
	    printk("athd: /dev/dhd%c: %d heads, %d cylinders, %d sectors\n",
		   (i + 'a'),
		   drive_info[i].heads,
		   drive_info[i].cylinders, drive_info[i].sectors);
	}

    directhd_initialized = 1;

    return 0;
}

/* why is arg unsigned int here if it's used as hd_geometry later ?
 * one of joys of K&R ? Someone please answer ...
 */
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
	err = verify_area(VERIFY_WRITE, arg, sizeof(struct hd_geometry));
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

    access_count[target]--;

    /* nothing really to do */

    return;
}

/*
 * 06/23 HS: Added buffer header manipulation to handle raw IO
 * 07/23 HS: Added delays to accommodate old (early) drives 
 */
void do_directhd_request(void)
{
    unsigned int count;		/* # of sectors to read/write */
    unsigned int this_pass;		/* # of sectors read/written */
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
	    continue;
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
#ifdef CONFIG_FS_XMS_BUFFER
	//printk("BF: %lx:%04x;", req->rq_seg, buff);
#else
	//printk("BF: %x:%04x;", req->rq_seg, buff);
#endif
#endif

	if (hd[minor].start_sect == -1 || hd[minor].nr_sects < start) {
	    printk("Bad partition start\n");
	    end_request(0);
	    break;
	}
	if (req->rq_cmd == READ) {
#ifdef USE_MULTISECT_IO
	    cmd = (dp->ctl&ATA_CFG_NMULT)? ATA_READ : ATA_READM; 
#else
	    cmd = ATA_READ;
#endif
	    port_io = read_data;
	} else {
#ifdef USE_MULTISECT_IO
	    cmd = (dp->ctl&ATA_CFG_NMULT)? ATA_WRITE : ATA_WRITEM;
#else
	    cmd = ATA_WRITE;
#endif
	    port_io = write_data;
	}

	start += hd[minor].start_sect;

	while (count > 0) {
	    sector = (start % dp->sectors) + 1;
	    tmp = start / dp->sectors;
	    head = tmp % dp->heads;
	    cylinder = tmp / dp->heads;
	    port = io_ports[drive / 2];

	    /* Handle requests spanning tracks, ie. in need of head change.
	     * Surprisingly, IDE READ and WRITE commands don't automatically 
	     * change head or cylinder when multiple sectors are requested.
	     */

	    if (count <= (dp->sectors - sector + 1))
		this_pass = count;
	    else
		this_pass = dp->sectors - sector + 1;

	    //printk("IOPa %x;", STATUS(port));
#ifdef USE_MULTISECT_IO
	    if (!(dp->ctl & ATA_CFG_NMULT)) {	/* We're running multisector IO */
		this_pass = (this_pass > dp->MAX_ATA_SPIO) ? dp->MAX_ATA_SPIO : this_pass;

	    	//printk("IOPb %x;", STATUS(port));
		while (WAITING(port));
		out_hd(drive, this_pass, 0, 0, 0, ATA_SET_MULT);
		while (WAITING(port));
		if (ERROR(port) & ATA_ERR_ABRT) {
			/* The drive doesn't support the nmbr requested
			 * in this_pass, do a single sector and try again. Acceptable
			 * numbers are drive dependent (usually powers of 2), so we'll 
			 * just try and fail which is faster and smaller than some
			 * smart logic. Enable the printk below
			 * and do a 'dd bs=9b ... ' (or 12b) to see how this
			 * works. It's interesting. */
		    //printk("athd%d: multi-IO err %d\n", drive, this_pass);
		    this_pass = 1;
	    	    out_hd(drive, this_pass, 0, 0, 0, ATA_SET_MULT);
		}
	    }
#endif

#ifdef DEBUG
#ifdef CONFIG_FS_XMS_BUFFER
	    debug_blkdrv("athd%d: cyl: %d hd: %d sec: %u st: %lu cnt: %d buf: %04x seg: %08lx\n",
		 drive, cylinder, head, sector, start, this_pass, buff, req->rq_seg);
#else
	    debug_blkdrv("athd%d: cyl: %d hd: %d sec: %u st: %lu cnt: %d buf: %04x seg: %04x\n",
		 drive, cylinder, head, sector, start, this_pass, buff, req->rq_seg);
#endif
#endif

	    /* Send drive parameters */
	    /* NOTE: Ideally we should check that the drive is ready to take commands
	     * before issuing one. However, if the previous transaction (command) was
	     * with the other drive, that's the source we're getting status from.
	     * So we issue the command and do the extra error checking instead. */
	    /* BTW - on old drives, where this is an issue, there is no NOOP cmd
	     * and thus no reasonable way to switch drive selection to the other unit
	     * except issuing a regular command. A non existing command would do, 
	     * but that's time consuming. */
	    out_hd(drive, this_pass, sector, head, cylinder, cmd);

	    //printk("IOP1 %x;", STATUS(port));
	    while (WAITING(port)) mdelay(delay);
	    if ((STATUS(port) & ERR_STAT) == ERR_STAT) { /* something went wrong */
		printk("athd: RD status: 0x%x error: 0x%x\n", STATUS(port), ERROR(port));
 		end_request(0);
		break;
	    }
	    //printk("IOP2;");
	    tmp = 0x00;
	    while ((tmp & DRQ_STAT) != DRQ_STAT) {
		if ((tmp & ERR_STAT) == ERR_STAT) {
		    printk("athd: RD DRQ status: 0x%x error: 0x%x\n",
			       STATUS(port), ERROR(port));
		    end_request(0);
		    break;
		} else {
		    tmp = STATUS(port);
		    //debug_blkdrv("athd%d: statusb 0x%x\n", drive, tmp);
		}
	    }
#define raw_flag tmp
#ifdef CONFIG_BLK_DEV_CHAR
	    if (req->rq_nr_sectors) raw_flag = 1; /* flag raw IO */
	    else
#endif
	    raw_flag = 0;
	    /* Do the I/O, either individual sectors or the whole shebang.
	     * FIXME: Add error checking below. How do we report errors back to the caller?
	     */
	    if (dp->ctl & ATA_CFG_NMULT) {
		int k = 0, l;
		while (1) {
	    		//printk("IOP3;");
			port_io(port, req->rq_seg, (word_t *)(buff+(k<<9)), 512, raw_flag);
			while(WAITING(port)) l++;	/* dummy increment */
			if (STATUS(port) & ERR_STAT) {
				/* Older drives may develop bad sectors,
				 * and other problems - that's a hard error */
				printk("athd%d: hard IO error %x\n", drive, ERROR(port));
				end_request(0);
				return;
			}
			if (++k == this_pass) break;
			while (!DRQ_WAIT(port));
		}
	    } else {
		port_io(port, req->rq_seg, (word_t *)buff, this_pass*512, raw_flag);
		if (STATUS(port)&ERR_STAT) 
			printk("athd%d multisector R/W error %x\n", drive, ERROR(port));
	    }

	    count -= this_pass;
	    start += this_pass;
	    buff += this_pass * 512;
	}
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
	outb_p(2, cport);		/* Change this to enable interrupts */
	if ((i = drive_busy(io_ports[controller])))
		printk("athd%i: still busy (%x)\n", controller, i);
	if ((i = ERROR(io_ports[controller])) != 1)
		printk("athd%i: Reset failed: %02x\n", controller, i);
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
