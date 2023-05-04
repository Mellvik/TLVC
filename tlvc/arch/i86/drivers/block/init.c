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

int boot_rootdev;	/* set by /bootopts options if configured*/

void INITPROC device_init(void)
{
    register struct gendisk *p;
    kdev_t rootdev;

    chr_dev_init();
    blk_dev_init();

    set_irq();

    for (p = gendisk_head; p; p = p->next)
	setup_dev(p);
    printk("boot_rootdev 0x%x\n", boot_rootdev);

//#ifdef CONFIG_BLK_DEV_BIOS
    /*
     * The bootloader may have passed us a ROOT_DEV which is actually a BIOS
     * drive number.  If so, convert it into a proper <major, minor> block
     * device number.  -- tkchia 20200308
     */
    /*
     * The bootloader ALWAYS uses BIOS, regardless of the CONFIG_BLK_DEV_BIOS
     * setting. So biosdrive is what we got for now. mellvik/20230421 for TLVC
     */ 
    if (!boot_rootdev && (SETUP_ELKS_FLAGS & EF_BIOS_DEV_NUM) != 0) {
#ifdef CONFIG_BLK_DEV_BIOS
	extern kdev_t INITPROC bioshd_conv_bios_drive(unsigned int biosdrive);

	rootdev = bioshd_conv_bios_drive((unsigned)ROOT_DEV);
	printk("device_setup: BIOS drive 0x%x, root device 0x%x\n",
		ROOT_DEV, rootdev);
#ifdef CONFIG_BLK_DEV_FD
	/* Needed only if we have BIOS HD and non-BIOS fd */
	if (rootdev == 0x380) rootdev = 0x200; 
#endif
#else   
	/* find the device (maj/min) we booted from */
	int minor, partition = 0;
	extern int boot_partition;
#define MINOR_SHIFT	5	/* FIXME move to include file, this is bad */
				/* or create a conv_drive function in directhd.c */

	if (ROOT_DEV & 0x80) {		/* hard drive*/
		minor = ROOT_DEV & 0x03;
		partition = boot_partition;	/* saved from add_partition()*/
		rootdev = MKDEV(ATHD_MAJOR, (minor >> MINOR_SHIFT) + partition);
	} else
		rootdev = MKDEV(FLOPPY_MAJOR, 0);
#endif

    } else
	/* use boot_rootdev from /bootopts */
	rootdev = boot_rootdev;

    printk("device_setup: root device 0x%x\n", rootdev);
    ROOT_DEV = rootdev;
}
