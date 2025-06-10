
#include <linuxmt/config.h>
#include <linuxmt/major.h>
#include <linuxmt/fs.h>
#include <linuxmt/kernel.h>
#include "romflash.h"


static int romflash_open(struct inode *i, struct file *f)
{
	i->i_size = 0x10000;	/* 8086 address space */
	return 0;
}


static struct file_operations romflash_fops =
{
	NULL,           /* lseek */
	NULL,           /* read */
	NULL,           /* write */
	NULL,           /* readdir */
	NULL,           /* select */
	NULL,           /* ioctl */
	romflash_open,  /* open */
	NULL            /* release */
};


void INITPROC romflash_init(void)
{
	if (register_blkdev(ROMFLASH_MAJOR, "rom", &romflash_fops))
		printk("Unable to register romfs\n");
}
