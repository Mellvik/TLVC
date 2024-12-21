/*
 * directhd driver for ELKS kernel
 * Copyright (C) 1998 Blaz Antonic
 * 14.04.1998 Bugfixes by Alastair Bridgewater nyef@sudval.org
 * 17.04.2023 Rewritten for TLVC by helge@skrivervik.com (hs)
 * 01.07.2023 modified to handle any request size, support raw io & and multisector transfers (hs)
 * 31.12.2023 Support for 8 bit ISA and (some) XTIDE cards (hs)
 * 02.10.2024 Full XTIDE support, configure via the xtide= setting in bootopts
 */

/*
 * TODO (HS 04/23):
 * - Create a library routine for delays/waits, many drivers have their own variant, wastes space.
 * - use interrupts - it will simplify the logic and improve reliability (and speed)
 * - test with 2 controllers, 4 drives
 * - add LBA support (will LBA work on a CHS initialized drive - or vise versa?)
 * - recognize the presence of solid state devices (to remove request sorting)
 * - start_sect is sometimes used as signed long (as in '-1'), not good
 */
/*
 * A note about IDE/ATA:
 * The IDE read/write Sector commands initiate multisector I/O but each sector needs its own read
 * or write operation: One command, many response-iterations - 
 * like waiting for a new DRQ (or interrupt) per sector.
 *
 * The Read/Write Multiple cmds are different, one command, one response. The # of
 * sectors in the ReadM or WriteM command miust eb lower than or equal to 
 * that specified in the preceding Set Multiple command. 
 * If device ID word 47 bits 7:0 are zero, multisector read/writes are not supported. 
 * Otherwise, the field holds the max # of sectors per transaction.
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
#include <linuxmt/debug.h>
#include <linuxmt/errno.h>
#include <linuxmt/config.h>
#include <linuxmt/stat.h>	/* for S_ISCHR() */

#include <arch/directhd.h>
#include <arch/ports.h>
#include <arch/io.h>
#include <arch/segment.h>

#ifdef CONFIG_IDE_XT
static unsigned char STATUS_REG[3] = { 0x7, 0xE, 0xE};
static unsigned char ERROR_REG[3]  = { 0x1, 0x2, 0x8};
static unsigned char SECTOR_REG[3] = { 0x3, 0x6, 0xA};

#define STATUS(p) inb_p((p) + STATUS_REG[cf_shift])
#define ERROR(p)  inb_p((p) + ERROR_REG[cf_shift])
#define SECTOR(p) inb_p((p) + SECTOR_REG[cf_shift])

#else 		/* No IDE_XT */

#define STATUS(p) inb_p((p) + ATA_STATUS)
#define ERROR(p)  inb_p((p) + ATA_ERROR)
#define SECTOR(p) inb_p((p) + ATA_SECTOR)

#endif		/* CONFIG_IDE_XT */

#define WAITING(p) ((STATUS(p) & BUSY_STAT) == BUSY_STAT)
#define DRQ_WAIT(p) (STATUS(p) & DRQ_STAT) /* set when ready to transfer */

#define DEBUG_DIRECTHD 0
#if DEBUG_DIRECTHD
static void dump_ide(word_t *, int);
#endif

/* #define USE_ASM */
/* use asm insw/outsw instead of C version */
/* asm versions should work on 8088/86, but only with CONFIG_HW_PCXT */

/* We've instructed GCC to generate 8086 code, this does not fit */
/* FIXME: Use #pragmas */
#define port_write(port,buf,nr) \
__asm__("cld;rep;outsw"::"d" (port),"S" (buf),"c" (nr))


#define USE_MULTISECT_IO	/* Enable/disable multisector R/W - for debugging */
//#define USE_INTERRUPTS		/* EXPERIMENTAL - test interrupts */
				/* cannot work with XT/IDE, XT/CF */
#define OLD_IDE_DELAY 1000	/* delay required for old drives, system dependent */
//#define USE_LOCALBUF		/* For debugging: use local bounce buffer instead of */
				/* direct buffer io via far pointers.
				 * When XMS buffers are active, the same 1k buffer
				 * is used for bouncing. Price: 1k bytes from heap */
#define MAJOR_NR ATHD_MAJOR
#define MINOR_SHIFT	5
#define ATDISK

extern int hdparms[];		/* Geometry data from /bootopts */

#ifdef CONFIG_IDE_XT
extern int xtideparms[];	/* config data for xtide controllers */
static int cur_type;		/* per request XT/IDE type (for speed)  */
#define is_xtide xtideparms[0]	/* flags the (expected) presence of an XT/IDE card */
#define xtide_port  0		/* indexes into the xtideparms array */
#define xtide_irq   1
#define xtide_flags 2
#else
#define is_xtide 0
#endif

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
static int access_count[MAX_ATA_DRIVES];

/* NEW (01/24): Support for XT/CF-Lite cards with A0 disconnected (registers at
 * even addresses only) and control port at base+1c. IO address at XTIDE_PORT,
 * usually 0x300, watch out for collissions with a NIC. The on-board BIOS may be
 * disabled. If so, boot from floppy. */
/* UPDATE Oct24: Initital CF-Lite suppoort replaced by generic XT-IDE support, 
 * enabled and configured via bootopts setting 'xtide=' - available in ideparms[].
 */

static struct ide_controller ide_ct[2] = {{HD1_PORT, HD1_CMD, HD1_AT_IRQ, 0},
			 		  {HD2_PORT, HD2_CMD, HD2_AT_IRQ, 0}};

#if defined(USE_LOCALBUF) || defined(CONFIG_FS_XMS_BUFFER)
static byte_t *localbuf;	/* bounce buffer for debugging and XMS buffers */
#endif

#define PORT_IO	port_io
void (*PORT_IO)() = NULL;

int directhd_initialized;	/* Used by block/init.c */
static struct drive_infot drive_info[MAX_ATA_DRIVES];

/* NOTE (FIXME): This is wasting a lot of memory, allocating 32 entries times MAX_ATA_DRIVES,
 * -> 128 entries while we may need 16, 32 at the most. Each entry = 8 bytes,
 * save potential > 768 bytes */
static struct hd_struct hd[MAX_ATA_DRIVES << MINOR_SHIFT]; /* partition pointer {start_sect, num_sects} */
static int directhd_sizes[MAX_ATA_DRIVES << MINOR_SHIFT];

static void directhd_geninit();
static int reset_controller(int);
static int drive_busy(int, int);
#ifdef USE_INTERRUPTS
static void do_directhd(int, struct pt_regs *);
#endif

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

static void directhd_geninit(void)
{
    struct drive_infot *drivep;
    struct hd_struct *hdp = hd;
    int i;

    drivep = drive_info;
    for (i = 0; i < MAX_ATA_DRIVES << MINOR_SHIFT; i++) {
	hdp->start_sect = -1;
	if ((i & ((1 << MINOR_SHIFT) - 1)) == 0) {
	    if ((hdp->nr_sects = (sector_t)drivep->sectors *
				drivep->heads * drivep->cylinders))
	    	hdp->start_sect = 0;	/* enable drive */
	    //printk("at%d: %ld$", i, hdp->nr_sects);
	    drivep++;
	} else
	    hdp->nr_sects = 0;
	hdp++;
    }
    return;
}

/* assumes buffer in current data segment - kernel_ds */

void insw(unsigned int port, word_t *buffer, int count)
{
    count = (count+1)>>1;
#ifdef CONFIG_IDE_XT
    if (is_xtide && !cur_type) {	/* for type 0 only */
	char *buf = (char *)buffer;	/* handle the XT-IDE rev 1 method */
					/* everything else ends up being word xfers */
	do {
	    *buf++ = inb(port);		/* low byte */
	    *buf++ = inb(port+8);	/* hi byte */
	} while (--count);
    } else
#endif
    {
	do {
	    *buffer++ = inw(port);
	} while (--count);
    }
}


void read_data(unsigned int port, ramdesc_t seg, word_t *buffer, int count, int raw)
{

#if defined(CONFIG_FS_XMS_BUFFER) || defined(USE_LOCALBUF) /* use bounce buffer */
    if (!raw) {	
	insw(port, (word_t *)localbuf, count);
	debug_blk("insw %d %x:%04x %lx:%04x %04x;", count, kernel_ds, localbuf,
		(unsigned long)seg, buffer, *(word_t *)localbuf);
	xms_fmemcpyw(buffer, seg, localbuf, kernel_ds, count/2);
    } else
#endif
    {

	//printk("%x,%x,%x,%lx,%d;", port, buffer, seg, locbuf, count);
   	count = (count+1)>>1;
#ifdef CONFIG_IDE_XT
	if (is_xtide && !cur_type) {	/* type 0: use byte IO */
	    byte_t __far *locbuf8 = _MK_FP(seg, (unsigned)buffer);
	    do {
		*locbuf8++ = inb(port);
		*locbuf8++ = inb(port+8);
	    } while (--count);

	} else
#endif
	{
	    word_t __far *locbuf = _MK_FP(seg, (unsigned)buffer);
	    do {
		*locbuf++ = inw(port);
	    } while (--count);
	}
    }
}

#if defined(USE_LOCALBUF) || defined(CONFIG_FS_XMS_BUFFER)

void outsw(unsigned int port, word_t *buffer, int count)
{
    count = (count+1) >> 1;
#ifdef CONFIG_IDE_XT
    if (is_xtide && !cur_type) {	/* type 0 (compat) only */
	byte_t *buf = (byte_t *)buffer;
	do {
	    outb(*(buf+1), port+8);
	    outb(*buf, port);
	    buf += 2;
	} while (--count);
    } else
#endif
    {
	do {
	    outw(*buffer++, port);
	} while (--count);
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
	count = (count+1) >> 1;
#ifdef CONFIG_IDE_XT
	/* for some reason, 'hispeed'-mode word-writes don't work (reads are fine), have to
	 * use byte writes for now. Compat mode always need byte writes, CFlite is fine with
	 * word IO */
	//printk("%x,%x,%x,%x,%d;", port, buffer, seg, cur_type, count);
	if (is_xtide && cur_type != 1) {	/* unless CF Lite */
	    byte_t __far *locbuf8 = _MK_FP(seg, (unsigned)buffer);
	    int offset = 8;
	    if (cur_type == 2) offset = 1;
	    do {
		outb(*(locbuf8+1), port+offset);
		outb(*locbuf8, port);
		locbuf8 += 2;
	    } while (--count);
	} else
#endif
	{
	    word_t __far *locbuf = _MK_FP(seg, (unsigned)buffer);
	    do {
		outw(*locbuf++, port);
	    } while (--count);
	}
    }
}

/*
 * Send a commmand packet to the drive, adapt to the various controller addressing schemes
 * (XT-IDE etc.)
 */
void send_cmd(unsigned int drive, unsigned int nsect, unsigned int sect,
	    unsigned int head, unsigned int cyl, unsigned int cmd)
{
    word_t port = ide_ct[drive >> 1].io_port;
    struct drive_infot *dp = &drive_info[drive];
    int increment = 1 + !!(ide_ct[drive>>1].reg_type); 

    /* setting WPCOM to 0 is not good. this change uses the last value input to
     * the drive. (my BIOS sets this correctly, so it works for now but we should
     * fix this to work properly)  -- Alastair Bridgewater */
    /* this doesn't matter on newer (IDE) drives, but is important for MFM/RLLs
     * I'll add support for those later and we'll need it then - Blaz Antonic */
    /* meanwhile, I found some documentation that says that for IDE drives
     * the correct WPCOM value is 0xff. so I changed it. - Alastair Bridewater */
    /* ATA2 redefined this register to be the features register. We may have to
     * distinguish between 'very old' and 'newer' drives here. Helge Skrivervik/2024 */

    /* FIXME - simplify this!! */
    /* FIXME - may need to disable LBA mode just in case the BIOS has set it */

    port += increment;			/* skip data register(s) */
    //outb_p(0x20, ++port);		/* means 128 (x4), test value for conner 40M */
    //++port;
#ifdef CONFIG_IDE_XT
    if (cur_type & XTIDE_CFG_HISPEED) { /* register adressing modified by swapping
    					 * address lines A3 and A0 (what a mess) */
	outb_p(nsect, port);
	outb_p(cyl, (port += increment));
	outb_p(0xA0 | ((drive & 1) << 4) | head, (port += increment)); 
	port += increment;		/* no features to set */
	outb_p(sect, (port += increment));
	outb_p(cyl >> 8, (port += increment));
    } else
#endif
    {
	if (dp->ctl & ATA_CFG_OLDIDE)	/* Features reg was WPCOM reg back in the day */
	    outb_p(0xff, port);		/* the supposedly correct value for WPCOM on IDE */
	outb_p(nsect, (port += increment));
	outb_p(sect, (port += increment));
	outb_p(cyl, (port += increment));
	outb_p(cyl >> 8, (port += increment));
	outb_p(0xA0 | ((drive & 1) << 4) | head, (port += increment)); 
    }
    outb(cmd, (port += increment));
    return;
}
#if DEBUG_DIRECTHD
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

#ifdef CONFIG_IDE_XT
/* Only called when when using XT/CFlite - will fail if used with XT-IDE */ 
static int ata_set_feature(unsigned int drive, unsigned int cmd)
{
	word_t port = ide_ct[drive >> 1].io_port;
	int err = 0, cf_shift = ide_ct[drive>>1].reg_type;

	if (cf_shift > 1) {	/* insurance */
	    printk("ath%d: driver error\n", drive>>1);
	    return 2;
	}
	outb_p(drive<<4, port + (ATA_DH<<cf_shift));
	outb_p(cmd, port + (ATA_FEATURES<<cf_shift));
	outb_p(ATA_SET_FEAT, port + (ATA_COMMAND<<cf_shift));

	while(WAITING(port)) mdelay(1000);
	if (STATUS(port) & ERR_STAT) {
		printk("ath%dd%d: feature not available: %x, %x\n", 
			drive/2, drive, cmd, ERROR(port));
		err++;	/* 'Abort' is the only possible error condition here, */
			/* which means 'command not implemented'. */
	}
	return err;
}
#endif

int INITPROC directhd_init(void)
{
    struct gendisk *ptr;
    word_t *ide_buffer;
    int i, hdcount = 0, drive;
    unsigned int port;
    char athd_msg[] = "athd%d: AT/IDE controller at 0x%x%s\n";

    /* .. once for each drive */
    /* By default, MAX_ATA_DRIVES is 4. On some systems, this may break (hang)
     * if there is only one IDE interface (the normal).
     * If so, change the MAX_ATA_DRIVES to 2 (which saves memory too).
     *
     * "If Drive 1 is not detected as being present, Drive 0 clears the Drive
     * 1 Status Register to 00h." From the spec. Making ST=0 a safe indication of
     * non presence.
     * Also, we should do a CMOS check for the number of drives, which would make 
     * this logic faster and more reliable FIXME */ 

    for (drive = 0; drive < MAX_ATA_DRIVES; drive++) {
	struct drive_infot *dp = &drive_info[drive];
	struct ide_controller *ct = &ide_ct[drive>>1];
#ifdef CONFIG_IDE_XT
	int offset = (drive>>1)*3;
	int cf_shift = 0;
#endif

	dp->ctl = 0;
	port = ct->io_port;

	if ((drive&1) == 0) {
#ifdef CONFIG_IDE_XT
	    if (is_xtide) {	/* fails in the unlikely setting that first
				 * controller is IDE, 2nd is XTIDE. FIXME */
		ct->io_port = port = xtideparms[xtide_port+offset];
		athd_msg[8] = 'X';
		ct->ctl_port = port + 8 + 6; /* 8 is the ctrl reg block offset, 6
				 	      * is the register in that block */
		cur_type = xtideparms[xtide_flags+offset];
		if (cur_type & XTIDE_CFG_CFLITE) {
			ct->reg_type = 1;
			ct->ctl_port += (8 + 6); /* A0 gone, double the offset */
		}
		if (cur_type & XTIDE_CFG_HISPEED) {
			ct->reg_type = 2;
			ct->ctl_port -= 7;	/* Swap A0 & A3 and 0xE becomes 0x7 */
		}
	    }
	    cf_shift = ct->reg_type;
#endif
	    if ((i = reset_controller(drive/2))) {
		printk("athd%d: Controller not found at 0x%x (%x)\n", drive/2, port, ct->ctl_port);
		drive++; /* don't check for slave drive if controller not found */
		continue;
	    }
	    printk(athd_msg, drive/2, port, is_xtide? " (8bit)":"");
	}

#ifdef CONFIG_IDE_XT
	if (is_xtide && cur_type&XTIDE_CFG_CFLITE) {	/* Put the drive (CF card) into
							 * 8bit mode, not really required since
							 * the BIOS will have done that already */
	    /* NOTE: CF cards will retain the 8bit mode setting until power cycled - or being
	     * reprogrammed explicitly to 16bit. If this is run on a 16bit ISA machine,
	     * BIOS boot will fail unless power cycled.
	     */
	     i = 0;
	     i += ata_set_feature(drive, ATA_FEAT_8BIT);
	     i += ata_set_feature(drive, ATA_FEAT_NO_WCACHE);	/* disable write cache */

	     if (i) {
		 printk("athd%dd%d: Failed to set 8bit mode, drive disabled\n", drive/2, drive&1);
		 continue;
	     }
	}
#endif

#if defined(CONFIG_FS_XMS_BUFFER) || defined(USE_LOCALBUF)
	localbuf = heap_alloc(BLOCK_SIZE, HEAP_TAG_DRVR);	/* permanent bounce buffer */
	if (!localbuf) {
	    printk("athd: cannot allocate bounce buffer\n");
	    return -ENOMEM;
	}
	ide_buffer = (word_t *)localbuf;
#else
	ide_buffer = (word_t *)heap_alloc(512, HEAP_TAG_DRVR);	/* temporary buffer */
#endif
	/* Get drive specs */
	send_cmd(drive, 0, 0, 0, 0, ATA_DRIVE_ID);

	/* wait -- if status is 0xff, there is no drive with this number */
	mdelay(OLD_IDE_DELAY);
	i = STATUS(port);
	if (!i || (i & 1) == 1) { /* error - drive not found or non-ide */
	    //printk("athd%d (port 0x%x) not found (%x)\n", drive, port, i);
	    continue;	/* Proceed with next drive.
			 * Always do this, even if the master drive
			 * is missing.  */
	}

	/* get drive info */
	while (WAITING(port)) mdelay(OLD_IDE_DELAY);
	insw(port, ide_buffer, 512);

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
	 * This is some sort of bugfix, we will use same method as 'big' Linux -
	 * work with disk geometry set in current translation mode (54-56) if valid (53)
	 * rather than physical drive info. Also, old drives have only physical, which may
	 * even be misleading: The configured BIOS drive type's CHS mapping may be different.
	 * E.g the Compaq drive type 17 (conner 42MB) reports 806/4/26 while the BIOS
	 * values are 980/5/17. Both work, but aren't interchangeable, so the BIOS value 
	 * wins for compatibility with other OSes. How do we get those values (not using
	 * a BIOS call)? Via bootopts: hdparms=960,5,17,-1 (HS/2023)
	 */

#if DEBUG_DIRECTHD
	dump_ide(ide_buffer, 64);
	//ide_buffer[53] = 0; /* force old ide behaviour for debugging */
#endif
	ide_buffer[20] = 0; /* String termination */
	if (ide_buffer[10] == 0x4551)	/* Crude QEMU detection */
		running_qemu = 1;
	i = drive*4;
	if ((ide_buffer[54] < 34096) && (*ide_buffer != 0)) {
	    /* Physical CHS data @ (word) offsets: cyl@1, heads@3, sectors@6 */
	    /* Actual CHS data @ (word) offsets: cyl@54, heads@55, sectors@56 */

	    if ((dp->multio_max = ide_buffer[47] & 0xff) == 1)  /* max sectors per multi io op */
		dp->multio_max = 0;	/* zero if unsupported, 1 is useless */
	    if (ide_buffer[53]&1) {	/* check the 'validity bit'. If set, use 
	    				 * 'current' values, otherwise defaults.
					 * Usually indicates old vs new tech. */
		dp->cylinders = ide_buffer[54];
		dp->heads = ide_buffer[55];
		dp->sectors = ide_buffer[56];
		hdparms[i] = 0; /* IDE takes presedence over bootopts - IS THIS OK? */

	    } else {		/* old drive, limited ID, limited cmd set, allow
				 * bootopts to override the geometry in ide_data */
		if (drive < 2 && hdparms[i]) {	/* works with first 2 drives only */
			dp->cylinders = hdparms[i];
			dp->heads = hdparms[i+1];
			dp->sectors = hdparms[i+2];
		} else {
			dp->cylinders = ide_buffer[1];
			dp->heads = ide_buffer[3];
			dp->sectors = ide_buffer[6];
		}
		dp->ctl |= ATA_CFG_OLDIDE;
	    }
	    hdcount++;
	    printk("athd%dd%d: IDE CHS: %d/%d/%d%s", drive/2, drive&1, dp->cylinders,
		dp->heads, dp->sectors, (drive < 2 && hdparms[i]) ? " (from /bootopts)" : "");

	    /* Initialize settings. Some (old in particular) drives need this
	     * and will default to some odd default values otherwise */
	    /* NOTE: In older docs this cmd is known as 'Initialize Drive Parameters' */
	    send_cmd(drive, dp->sectors, 0, dp->heads - 1, 0, ATA_SPECIFY);
	    while(WAITING(port)) mdelay(1000);
	    if (STATUS(port) & ERR_STAT) printk("\nath%dd%d err in specify: %x;", 
			drive/2, drive, ERROR(port)); /* DEBUG */

#ifdef USE_MULTISECT_IO	
	    if (dp->multio_max) {
		/* Set multiple IO mode, set to max always, experimental */
		send_cmd(drive, dp->multio_max, 0, 0, 0, ATA_SET_MULT);
		while (WAITING(port));
		if (!(STATUS(port) & ERR_STAT)) {
		    printk(", Multisector I/O, max %d sects", dp->multio_max);
		} else {
		    printk(", Multisector I/O failed (%x), turned off", ERROR(port));
		    dp->multio_max = 0;
		}
	    }
#endif	/* USE_MULTISECT_IO */
	    printk("\n");
	    //printk("athd%d: IDE data 47-49: %04x, %04x, %04x\n", drive, 
		//ide_buffer[47], ide_buffer[48], ide_buffer[49]);
	} else
	    printk("ath%dd%d: No valid drive ID\n", drive/2, drive);

#if !defined(CONFIG_FS_XMS_BUFFER) && !defined(USE_LOCALBUF)
	heap_free(ide_buffer);
#endif
    }
    if (!hdcount) {
	printk("ath: no drives found\n");
	return 0;
    }

    directhd_gendisk.nr_real = hdcount;
 
    if (register_blkdev(MAJOR_NR, DEVICE_NAME, &directhd_fops)) {
	printk("ath: unable to register\n");
	return -1;
    }

#ifdef USE_INTERRUPTS	/* Experimental */
    /* FIXME: Need to move this into the main loop to accomodate the use of different IRQs
     * for primary and 2ndary controller. AS is, the same IRQ is assigned to both controllers
     * which will not work. Possibly configure only one controller if machine is XT */

    /* NOTE: Many modern day XT_IDE controllers do not even have an IRQ line - set 
	     xtide_irq bit in /bootopts to zero to tell the driver */
    /* TEST this on 8 bit bus machines! (irq 5) */
    /* TODO: On AT and higher, add irq reg for 2nd card if present - irq 15/HD2_AT_IRQ */
    int got_irq;
#ifdef CONFIG_IDE_XT
    if (is_xtide)
	got_irq = xtideparms[xtide_irq + offset];
    else
#endif
	got_irq = HD1_AT_IRQ;
    if (got_irq) {
	printk("athd: Interrupt registration: ");
	if (request_irq(got_irq, do_directhd, INT_GENERIC))
	    got_irq = 0;
	if (got_irq)
    	   printk("%d\n", got_irq);
	else
    	   printk("failed\n");
    } else
	printk("No IRQ; PIO only\n")
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

#if NOTNEEDED
    /* print drive info */
    for (i = 0; i < MAX_ATA_DRIVES; i++)
	/* sanity check */
	if (drive_info[i].heads != 0) {
	    printk("athd%dd%d: /dev/dhd%c: %d heads, %d cylinders, %d sectors (~%luMB)\n",
		   i/2, i&1, (i + 'a'),
		   drive_info[i].heads,
		   drive_info[i].cylinders, drive_info[i].sectors,
		   (((__u32)drive_info[i].heads*(__u32)drive_info[i].cylinders*
		     (__u32)drive_info[i].sectors)>>1)/1000);
	}
#endif
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
 * NOTE: open is used by the raw driver too!
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

    if (!S_ISCHR(inode->i_mode)) { 	/* Don't count raw opens */
	access_count[target]++;
    }

    inode->i_size = (hd[minor].nr_sects) << 9;
    /* limit inode size to max filesize for CHS >= 4MB (2^22)*/
    if (hd[minor].nr_sects >= 0x00400000L)	/* 2^22*/
        inode->i_size = 0x7ffffffL;		/* 2^31 - 1*/
    debug_blk("%cdhd[%04x] open, size %ld\n", S_ISCHR(inode->i_mode)? 'r': ' ',
						inode->i_rdev, inode->i_size);
    return 0;
}

void directhd_release(struct inode *inode, struct file *filp)
{
    int target = DEVICE_NR(inode->i_rdev);
    kdev_t dev = inode->i_rdev;

    /* The raw driver never calls directhd_release, so we're fine with the counters */
    access_count[target]--;
    fsync_dev(dev);			/* cannot trust umount to do this */
    if (!access_count[target]) {
	invalidate_buffers(dev);
	invalidate_inodes(dev);
    }

    return;
}

/*
 * 06/23 HS: Added buffer header manipulation to handle raw IO
 * 07/23 HS: Added delays to accommodate early IDE drives 
 */
void do_directhd_request(void)
{
    unsigned int count;		/* # of sectors to read/write */
    sector_t start;		/* first sector */
    char *buff;	
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
    unsigned int raw_mode;
#ifdef CONFIG_IDE_XT
    int cf_shift = 0;
#endif

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
#ifdef CONFIG_IDE_XT
	if (is_xtide) {
	    cf_shift = ide_ct[drive>>1].reg_type;
	    cur_type = cf_shift;	/* we don't need both, turn cf_dhift into a global */
	}
#endif
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
	if (req->rq_nr_sectors) {
		count = req->rq_nr_sectors;
		raw_mode = 1; /* flag raw IO */
	} else {
		raw_mode = 0;
		count = BLOCK_SIZE / 512;
	}

	start = req->rq_blocknr;
	buff = req->rq_buffer;
	/* safety check should be here */
	debug_blk("dhd[%04x]: start: %lu nscts: %lu\n", req->rq_dev,
			hd[minor].start_sect, hd[minor].nr_sects);

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
	port = ide_ct[drive>>1].io_port;

#if DEBUG_DIRECTHD
	if (debug_level > 2) printk("athd%d%d: CHS %d/%d/%u st: %lu cnt: %d buf: %04x seg: %lx %04x/%c\n",
		 drive/2, drive&1, cylinder, head, sector, start, count, buff, 
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

	send_cmd(drive, count, sector, head, cylinder, cmd);

	while (WAITING(port)) mdelay(delay);

	if ((STATUS(port) & ERR_STAT) == ERR_STAT) { /* something went wrong */
		printk("athd%dd%d: IO status: 0x%x error: 0x%x CHS[%u/%u/%u]\n",
			drive/2, drive&1, STATUS(port), ERROR(port), cylinder, head, SECTOR(port));
		end_request(0);
		return;
	}
	/* Wait for DRQ to trigger the actual IO */
	tmp = 0x00;
	while ((tmp & DRQ_STAT) != DRQ_STAT) {
		if ((tmp & ERR_STAT) == ERR_STAT) {
		    printk("athd%dd%d: RD DRQ status: 0x%x error: 0x%x\n",
			       drive/2, drive&1, STATUS(port), ERROR(port));
		    end_request(0);
		    return;
		} else {
		    tmp = STATUS(port);
		    debug_blk("athd%d: status 0x%x\n", drive, tmp);
		}
	}

	/*
	 * Do the I/O. IDE will accept sector count up to 256, and will interrupt
	 * per sector in 'normal' mode, per block in multi_io mode. The block size is 
	 * set via the SET_MULTIPLE command, we use the max number allowed by the drive.
	 * Requesting fewer sectors than the block size is OK.
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
		    printk("athd%dd%d multisector R/W error %x sector %d\n", drive/2,
		    	drive&1, ERROR(port), SECTOR(port));
		    /* Older drives may develop bad sectors,
		     * and other problems - that's a hard error:
		     * The drive has already retried a number of time internally. */
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
 * Probe for controller existence and 
 * toggle the soft reset bit for the controller, return 0 if all good.
 */
static int reset_controller(int controller)
{
	int	i, err;
	int	cf_shift = ide_ct[controller].reg_type;
	int	cport = ide_ct[controller].ctl_port;
	int	port = ide_ct[controller].io_port;

	outb_p(0xC, cport);		/* reset controller */
	err = STATUS(port);		/* if 0xFF -> nothing there */

	/* If the busy bit doesn't get immediately set, there is nothing there */
	/* Some controllers will only respond if a drive was detected during POST */
	/* QEMU sets, then clears the busy bit real fast, leaving 0x50 as the status */
	/* This is a problem only when the XT-IDE support is active */

	if (err == 0xff || !(err & 0xd0)) {	/* should be !(err & BUSY_STAT) */
	    err = 1;
	} else {
	    err = 0;
	    mdelay(3000);
#ifdef USE_INTERRUPTS
	    outb_p(0x8, cport);		/* Clr reset, enable interrupts */
#else
	    outb_p(0xA, cport);		/* Clr reset, disable interrupts */
#endif
	    if ((i = drive_busy(port, cf_shift))) {	/* probably no controller or no drive */
						/* don't clutter the console with these messages */
		debug_blk("athd%d: still busy (%x)\n", controller, i);
		err = 1;
	    } else if ((i = ERROR(port)) && i != 1) { /* i == 0 if controller found, but no drives */
		printk("athd%d: Reset failed: %02x\n", controller, i);
		err = 1;
	    }
	}
	return err;
}

#ifdef USE_INTERRUPTS
/* test interrupt enable/disable for now */
static void do_directhd(int unused, struct pt_regs *unused1)
{
	printk("X");
}
#endif

/*
 * FIXME:
 * Delay loop and status test, should do this with a micro_delay timer a la minix 
 */
static int drive_busy(int port, int cf_shift)
{
	unsigned int i;
	unsigned char c;

	for (i = 0; i < 50000; i++) {
		// FIXME: More bits to test here? Check other drivers.
		c = STATUS(port); 
		if ((c&(BUSY_STAT|READY_STAT)) == READY_STAT)
			return 0;
	}
	return(c);
}

/* these delays are required for the oldest of drives only */
static void mdelay(int x) 
{
	while (x--) outb_p(x, 0x80);
}
