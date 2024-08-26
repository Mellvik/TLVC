/*
 *  Code extracted from
 *  linux/kernel/hd.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 *
 *  Thanks to Branko Lankester, lankeste@fwi.uva.nl, who found a bug
 *  in the early extended-partition checks and added DM partitions
 *
 *  Support for DiskManager v6.0x added by Mark Lord (mlord@bnr.ca)
 *  with information provided by OnTrack.  This now works for linux fdisk
 *  and LILO, as well as loadlin and bootln.  Note that disks other than
 *  /dev/hda *must* have a "DOS" type 0x51 partition in the first slot (hda1).
 *
 *  More flexible handling of extended partitions - aeb, 950831
 */

#include <linuxmt/boot.h>
#include <linuxmt/config.h>
#include <linuxmt/fs.h>
#include <linuxmt/genhd.h>
#include <linuxmt/kernel.h>
#include <linuxmt/major.h>
#include <linuxmt/init.h>
#include <linuxmt/string.h>
#include <linuxmt/memory.h>

#include <arch/system.h>
#include <arch/segment.h>

#define MINOR_SHIFT	5	/* FIXME move to include file, this is bad */
				/* or create a conv_drive function in directhd.c */

#ifdef CONFIG_BLK_DEV_XD	/* choose MFM or IDE controller, */
#define HD_MAJOR XD_MAJOR	/* they're mutually exclusive */
#elif CONFIG_BLK_DEV_HD
#define HD_MAJOR ATHD_MAJOR
#else
#define HD_MAJOR BIOSHD_MAJOR	/* Enables BIOSHD + DIRECTFD */
#endif

int boot_rootdev;	/* set by /bootopts options if configured*/
extern int boot_partition;
char running_qemu;	/* for directhd/fd */

void INITPROC device_init(void)
{
    register struct gendisk *p;
    kdev_t rootdev;

    chr_dev_init();
    blk_dev_init();

    set_irq();

    for (p = gendisk_head; p; p = p->next)
	setup_dev(p);
    //printk("boot_rootdev 0x%x, ", boot_rootdev);

    /*
     * The bootloader may have passed us a ROOT_DEV which is actually a BIOS
     * drive number.  If so, convert it into a proper <major, minor> block
     * device number.  -- tkchia 20200308
     */
    if (!boot_rootdev && (SETUP_ELKS_FLAGS & EF_BIOS_DEV_NUM) != 0) {

#if defined(CONFIG_BLK_DEV_BIOS) && !defined(CONFIG_BLK_DEV_FD)
	extern kdev_t INITPROC bioshd_conv_bios_drive(unsigned int biosdrive);

	rootdev = bioshd_conv_bios_drive((unsigned)ROOT_DEV);

#else 	/* Direct HD/XD/FD */
	/* Should work with BIOS hd + direct FD too */
	/* Will NOT work with PC98 */

	if (ROOT_DEV & 0x80) 	/* hard drive: HD, XD, BIOSHD */
		rootdev = MKDEV(HD_MAJOR, ((ROOT_DEV & 0x03) << MINOR_SHIFT) 
						+ boot_partition);
	else			/* floppy */
		rootdev = MKDEV(FLOPPY_MAJOR, (ROOT_DEV & 0x03));
#endif

	//printk("device_setup: BIOS drive 0x%x, root device 0x%x\n",
		//ROOT_DEV, rootdev);

    } else
	/* use boot_rootdev from /bootopts */
	rootdev = boot_rootdev;

    //printk("device_setup: root device 0x%x\n", rootdev);
    ROOT_DEV = rootdev;
}
