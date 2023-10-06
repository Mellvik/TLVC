/*
 *  linux/fs/block_dev.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

#include <linuxmt/config.h>

#include <linuxmt/errno.h>
#include <linuxmt/sched.h>
#include <linuxmt/kernel.h>
#include <linuxmt/fcntl.h>
#include <linuxmt/fs.h>
#include <linuxmt/mm.h>
#include <linuxmt/debug.h>
#include <linuxmt/stat.h>

#include <arch/segment.h>
#include <arch/system.h>

#define SECT_SIZE 512
#define SECT_SIZE_BITS 9

size_t block_read(struct inode *inode, struct file *filp, char *buf, size_t count)
{
    loff_t pos;
    size_t chars;
    size_t read = 0;

    /* Amount we can do I/O over */
    pos = ((loff_t)inode->i_size) - filp->f_pos;
    if (pos <= 0) {
	debug("blockread: EOF reached size %ld pos %ld.\n", inode->i_size, filp->f_pos);
	return 0;		/* EOF */
    }
    if ((loff_t)count > pos) count = (size_t)pos;

    while (count > 0) {
	register struct buffer_head *bh;

	/*
	 *      Read the block in
	 */
	chars = (filp->f_pos >> BLOCK_SIZE_BITS);
	if (inode->i_op->getblk)
	    bh = inode->i_op->getblk(inode, (block_t)chars, 0);
	else
	    bh = getblk(inode->i_rdev, (block_t)chars);
	/* Offset to block/offset */
	chars = BLOCK_SIZE - (((size_t)(filp->f_pos)) & (BLOCK_SIZE - 1));
	if (chars > count) chars = count;
	if (bh) {
	    if (!readbuf(bh)) {
		if (!read) read = -EIO;
		break;
	    }
	    xms_fmemcpyb(buf, current->t_regs.ds,
		buffer_data(bh) + (((size_t)(filp->f_pos)) & (BLOCK_SIZE - 1)),
		buffer_seg(bh), chars);
	    brelse(bh);
	} else fmemsetb(buf, current->t_regs.ds, 0, chars);
	buf += chars;
	filp->f_pos += chars;
	read += chars;
	count -= chars;
    }

#ifdef BLOAT_FS
    filp->f_reada = 1;
#endif
#ifdef FIXME
    if (!IS_RDONLY(inode)) inode->i_atime = CURRENT_TIME;
#endif
    return read;
}

size_t block_write(struct inode *inode, struct file *filp, char *buf, size_t count)
{
    size_t chars, offset;
    size_t written = 0;

    if (filp->f_flags & O_APPEND) filp->f_pos = (loff_t)inode->i_size;

    while (count > 0) {
	register struct buffer_head *bh;

	chars = (filp->f_pos >> BLOCK_SIZE_BITS);
	if (inode->i_op->getblk)
	    bh = inode->i_op->getblk(inode, (block_t)chars, 1);
	else
	    bh = getblk(inode->i_rdev, (block_t)chars);
	if (!bh) {
	    if (!written) written = -ENOSPC;
	    break;
	}
	/* Offset to block/offset */
	offset = ((size_t)filp->f_pos) & (BLOCK_SIZE - 1);
	chars = BLOCK_SIZE - offset;
	if (chars > count) chars = count;
	/*
	 *      Read the block in, unless we
	 *      are writing a whole block.
	 */
	if (chars != BLOCK_SIZE) {
	    if (!readbuf(bh)) {
		if (!written) written = -EIO;
		break;
	    }
	}
	/*
	 *      Alter buffer, mark dirty
	 */
	xms_fmemcpyb(buffer_data(bh) + offset, buffer_seg(bh), buf,
		current->t_regs.ds, chars);
	mark_buffer_uptodate(bh, 1);
	mark_buffer_dirty(bh);
	brelse(bh);
	buf += chars;
	filp->f_pos += chars;
	written += chars;
	count -= chars;
    }
    {
	register struct inode *pinode = inode;
	if ((loff_t)pinode->i_size < filp->f_pos)
	    pinode->i_size = (__u32) filp->f_pos;
	pinode->i_mtime = pinode->i_ctime = CURRENT_TIME;
	pinode->i_dirt = 1;
    }
    return written;
}

#ifdef CONFIG_BLK_DEV_CHAR
/*
 * For raw block device access only. 
 * NOTE: This code assumes that the major device #s are the same for char and block devices.
 * DO NOT CHANGE THAT.
 *
 * A regular buffer acts as a bounce buffer for small and odd sized * (< SECTSIZE) transfers.
 * For all other transfers we use the buffer header to pass metadata back and forth,
 * data transfers go directly to/from the process' memory space.
 */
struct buffer_head * get_free_buffer(void);

static int raw_blk_rw(struct inode *inode, register struct file *filp,
		  char *buf, size_t count, int wr)
{
    struct buffer_head *bh;
    ext_buffer_head *ebh;
    size_t chars, offset;
    int written = 0;

    bh = get_free_buffer();
    ebh = EBH(bh);
    ebh->b_dev = inode->i_rdev;
					
    while (count > 0) {
    /*
     *      Partial block processing: At the beginning of the transfer if the starting
     *	    position is not on a block boundary, and at the end unless the transfer 
     *	    size matches a block boundary.
     *	    As much as I'd like to not use the buffer system at all, this is the easy 
     *	    way to temporarily acquire a bounce buffer for the odd cases.
     */
	offset = ((size_t)filp->f_pos) & (SECT_SIZE - 1);
	chars = 0;
	if (offset) {	/* process partial first sector */
		chars = SECT_SIZE - offset;
		if (chars > count)
	    		chars = count;
	} else if (count < SECT_SIZE) /* partial trailing sector */
		chars = count;
	printk("RAW %u/%u bl %lu:", (unsigned int) chars, (unsigned int) count,
					filp->f_pos >> SECT_SIZE_BITS);
	if (chars) {
	/*
	 *      Get a (bounce) buffer for the partial block.
	 *	Cannot use getblk() since that would send us right into the
	 * 	buffer cache looking for a match.
	 */
		ebh->b_blocknr = filp->f_pos >> SECT_SIZE_BITS;
		ll_rw_blk(READ, bh);
		wait_on_buffer(bh);
		if (!ebh->b_uptodate) {
			written = -EIO;
			break;
		}
		/* 
		 * Got the data, now process partial block
		 */
		if (wr == BLOCK_WRITE) {
			/*
			 * Alter buffer, mark dirty
		 	 */
	    		xms_fmemcpyb(buffer_data(bh) + offset, buffer_seg(bh), buf,
				current->t_regs.ds, chars);
	    		/*
	     		 * Writing: queue physical I/O
	     		 */
	    		ll_rw_blk(WRITE, bh);
	    		wait_on_buffer(bh);
	    		if (!ebh->b_uptodate) { /* Write error. */
				if (!written) written = -EIO;
				break;
	    		}
		} else {
			/*
			 * Move the data into the requesting process' dataspace 
	 		 */
	    		xms_fmemcpyb(buf, current->t_regs.ds, buffer_data(bh) + offset,
				buffer_seg(bh), chars);
		}
	} else {	/* we're moving full sectors */
		unsigned char *o_data;
		seg_t o_seg;

		chars = (count & 0xffff); /* try to transfer the whole thing -
					 * up to 64k, which is more than the 
					 * system can handle anyway. */
				/* FIXME: How do we get info about 
				 * partial IO back from the lower levels? */

		ebh->b_blocknr = filp->f_pos >> SECT_SIZE_BITS;
		o_data = bh->b_data;		/* save the 'real' values */
		o_seg = ebh->b_L2seg;
		ebh->b_L2seg = current->t_regs.ds;
		bh->b_data = (unsigned char *)buf;
		ebh->b_nr_sectors = (chars >> SECT_SIZE_BITS);

	    	ll_rw_blk(wr, bh);
	    	wait_on_buffer(bh);
		ebh->b_L2seg = o_seg;		/* restore */
		bh->b_data = o_data;
    		if (!ebh->b_uptodate) {		/* Write error. */
			if (!written) written = -EIO;
			break;
	    	}
	}
	
	buf += chars;
	filp->f_pos += chars;
	written += chars;
	count -= chars;
	//printk("raw: chars %d, pos %ld, bh %04x\n", chars, filp->f_pos >> SECT_SIZE_BITS, bh);
    }
    ebh->b_dev = NODEV;	/* Invalidate buffer */
    brelse(bh);
    return written;
}

size_t block_rd(struct inode *inode, struct file *filp,
	       char *buf, size_t count)
{
    return raw_blk_rw(inode, filp, buf, count, BLOCK_READ);
}

size_t block_wr(struct inode *inode, struct file *filp,
		char *buf, size_t count)
{
    return raw_blk_rw(inode, filp, buf, count, BLOCK_WRITE);
}

#endif /* CONFIG_BLK_DEV_CHAR */
