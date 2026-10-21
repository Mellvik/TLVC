/*
 *  linux/fs/minix/dir.c
 *
 *  Copyright (C) 1991, 1992 Linus Torvalds
 *
 *  minix directory handling functions
 */

#include <linuxmt/types.h>
#include <linuxmt/string.h>
#include <linuxmt/errno.h>
#include <linuxmt/fs.h>
#include <linuxmt/minix_fs.h>
#include <linuxmt/stat.h>
#include <linuxmt/sched.h>
#include <linuxmt/config.h>

#include <arch/segment.h>

static int minix_readdir(struct inode *inode,
			 register struct file *filp,
			 char *dirent, filldir_t filldir);

/*
 * While Linux applications use opendir/readdir etc. library calls to access
 * directory entries, V7 (Venix) applications need to read 'raw' directories.
 * Fortunately, the V7 file system has the exact same directory layout as
 * the Minix v1 filesystem, so all we need to do is to deliver the data. 
 * make, sh and csh are some of the applications that depend on this.
 */

static size_t minix_dir_read(struct inode *inode,struct file *filp,
			     char *buf,size_t count)
{
//printk("minix_dir_read: %x %ld %x %d, dev %x rdev %x\n", inode, 
	//filp->f_pos, buf, count, inode->i_dev, inode->i_rdev);
#ifdef CONFIG_COMPAT_V7
    //if (current->task_is_V7) {	/* If the V7 flag is turned off in sys_read, this won't work */
	struct buffer_head *bh;
	int offset = (unsigned short)filp->f_pos & (BLOCK_SIZE - 1);
	bh = minix_bread(inode, (block_t) ((filp->f_pos) >> BLOCK_SIZE_BITS), 0);
	if (bh) {
		if ((filp->f_pos + count) > inode->i_size)
			count = inode->i_size - filp->f_pos;
#if 1
		xms_fmemcpyb(buf, current->t_regs.ds,
			buffer_data(bh) + offset, buffer_seg(bh), count);
		brelse(bh);
#else
		map_buffer(bh);	
		memcpy_tofs(buf, bh->b_data+offset, count);
		unmap_brelse(bh);
#endif
		filp->f_pos += count;
		return count;
	}
	return -EIO;
    //}
#else
    return -EISDIR;
#endif
}

/*@-type@*/

static struct file_operations minix_dir_operations = {
    NULL,			/* lseek - default */
    minix_dir_read,		/* read */
    NULL,			/* write - bad */
    minix_readdir,		/* readdir */
    NULL,			/* select - default */
    NULL,			/* ioctl - default */
    NULL,			/* no special open code */
    NULL			/* no special release code */
};

/*
 * directories can handle most operations...
 */
struct inode_operations minix_dir_inode_operations = {
    &minix_dir_operations,	/* default directory file-ops */
    minix_create,		/* create */
    minix_lookup,		/* lookup */
    minix_link,			/* link */
    minix_unlink,		/* unlink */
    minix_symlink,		/* symlink */
    minix_mkdir,		/* mkdir */
    minix_rmdir,		/* rmdir */
    minix_mknod,		/* mknod */
    NULL,			/* readlink */
    NULL,			/* follow_link */
    NULL,			/* getblk */
    minix_truncate		/* truncate */
};

/*@+type@*/

static int minix_readdir(struct inode *inode,
			 register struct file *filp,
			 char *dirent, filldir_t filldir)
{
    register struct buffer_head *bh;
    struct minix_dir_entry *de;
    struct minix_sb_info *info;
    unsigned short offset;

    if (!inode || !inode->i_sb || !S_ISDIR(inode->i_mode)) return -EBADF;
    info = &inode->i_sb->u.minix_sb;
    if (((unsigned short)filp->f_pos) & (info->s_dirsize - 1)) return -EBADF;
    while (filp->f_pos < (loff_t) inode->i_size) {
	offset = (unsigned short)filp->f_pos & (BLOCK_SIZE - 1);
	bh = minix_bread(inode, (block_t) ((filp->f_pos) >> BLOCK_SIZE_BITS), 0);
	if (!bh) {
	    filp->f_pos += (BLOCK_SIZE - offset);
	    continue;
	} else map_buffer(bh);
	do {
	    de = (struct minix_dir_entry *) (offset + bh->b_data);
	    if (de->inode) {
			filldir(dirent, de->name, strnlen(de->name, info->s_namelen),
				filp->f_pos, de->inode);
			filp->f_pos += info->s_dirsize;
		    unmap_brelse(bh);
		    return 0;
	    }
	    offset += info->s_dirsize;
	    filp->f_pos += info->s_dirsize;
	} while (offset < BLOCK_SIZE && filp->f_pos < (loff_t)inode->i_size);
	unmap_brelse(bh);
    }
    return 0;
}
