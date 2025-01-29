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
	return 0;		/* EOF */
    }

    //if (check_disk_change(inode->i_rdev))
        //return -ENXIO;

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

    //if (check_disk_change(inode->i_rdev))
        //return -ENXIO;

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
        if (/*bh->b_dev == 0x200 &&*/ EBH(bh)->b_blocknr >= 5)
                debug_blk("block_write: have block %ld\n", EBH(bh)->b_blocknr);
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
    if ((loff_t)inode->i_size < filp->f_pos)
	inode->i_size = (__u32) filp->f_pos;
    inode->i_mtime = inode->i_ctime = current_time();
    inode->i_dirt = 1;
    return written;
}

/*
 * For raw block device access only. 
 * NOTE: This code assumes that the major device #s are the same for char and block devices.
 * DO NOT CHANGE THAT.
 *
 * A regular buffer acts as a bounce buffer for small and odd sized (< SECTSIZE) transfers.
 * For other transfers, the buffer header is used to pass metadata back and forth while
 * data transfers go directly to/from the process' memory space.
 */
struct buffer_head *get_free_buffer(void);

static int raw_blk_rw(struct inode *inode, register struct file *filp,
		  char *buf, size_t count, int wr)
{
    struct buffer_head *bh;
    ext_buffer_head *ebh;
    size_t chars, offset;
    int io_count = 0;

    bh = get_free_buffer();
    ebh = EBH(bh);
    ebh->b_dev = inode->i_rdev;
					
    while (count > 0) {
    /*
     *      Partial sector processing.
     *
     *	    As much as I'd like to stay off the buffer system, this is a convenient
     *	    way to acquire a tmp bounce buffer for the odd cases.
     */
	/* check for end of medium */
	if ((inode->i_size - filp->f_pos) <= 0) {
		if (wr == BLOCK_READ || io_count)
			return io_count;	/* Truncated r/w or read EOF */
		return -ENOSPC;			/* Nothing written, No space */
	}
	offset = ((size_t)filp->f_pos) & (SECT_SIZE - 1);
	chars = 0;
	if (offset) {	/* process partial first sector */
		chars = SECT_SIZE - offset;
		if (chars > count)
	    		chars = count;
	} else if (count < SECT_SIZE) /* partial trailing sector */
		chars = count;

#if 0
	printk("RAW pos %u, cnt %u/%u bl %lu:", (unsigned int) offset,
		(unsigned int) chars, (unsigned int) count,
		filp->f_pos >> SECT_SIZE_BITS);
#endif

	if (chars) {
		ebh->b_blocknr = filp->f_pos >> SECT_SIZE_BITS;
		ebh->b_nr_sectors = 1;	/* tell low level this is raw */
		ll_rw_blk(READ, bh);
		wait_on_buffer(bh);
		if (!ebh->b_uptodate) {
			if (!io_count) io_count = -EIO;
			break;
		}
		/* 
		 * Got the data, now process partial block
		 */
		if (wr == BLOCK_WRITE) {
	    		xms_fmemcpyb(buffer_data(bh) + offset, buffer_seg(bh), buf,
				current->t_regs.ds, chars);
	    		/*
	     		 * Writing: queue physical I/O
	     		 */
	    		ll_rw_blk(WRITE, bh);
	    		wait_on_buffer(bh);
	    		if (!ebh->b_uptodate) { /* Write error */
				if (!io_count) io_count = -EIO;
				break;
	    		}
		} else {
			/*
			 * Move the data into the requesting process' dataspace 
	 		 */
	    		xms_fmemcpyb(buf, current->t_regs.ds, buffer_data(bh) + offset,
				buffer_seg(bh), chars);
		}
	} else {	/* moving full sectors */
		char *o_data;
		ramdesc_t o_seg;
		unsigned char sec_cnt;

		chars = (count & 0xffff); /* try to transfer the whole thing -
					 * up to 64k, which is more than the 
					 * system can handle anyway. */

		ebh->b_blocknr = filp->f_pos >> SECT_SIZE_BITS;
		o_data = bh->b_data;		/* save the 'original' values */
		o_seg = ebh->b_L2seg;		/* may be long (xms active) or int */
		ebh->b_L2seg = current->t_regs.ds;
		bh->b_data = buf;
		ebh->b_nr_sectors = (chars >> SECT_SIZE_BITS);
		chars &= ~(SECT_SIZE - 1);
		sec_cnt = ebh->b_nr_sectors;
		debug_raw("IO: blk %lu cnt %d addr %x:%x\n", ebh->b_blocknr,
			(int)ebh->b_nr_sectors, ebh->b_L2seg, bh->b_data);

	    	ll_rw_blk(wr, bh);
	    	wait_on_buffer(bh);
		ebh->b_L2seg = o_seg;		/* restore */
		bh->b_data = o_data;
		if (sec_cnt != ebh->b_nr_sectors) {
			debug_raw("partial raw IO, requested %d, got %d\n",
						sec_cnt, ebh->b_nr_sectors);
			chars = (ebh->b_nr_sectors << SECT_SIZE_BITS);
		}
    		if (!ebh->b_uptodate) {		/* IO error */
			if (!io_count) io_count = -EIO;
			break;
	    	}
	}
	
	buf += chars;
	filp->f_pos += chars;
	io_count += chars;
	count -= chars;
	debug_raw("raw: chars %d, nxt blk %ld, bh %04x;", 
		chars, filp->f_pos >> SECT_SIZE_BITS, bh);
    }
done:
    ebh->b_dev = NODEV;	/* Invalidate buffer */
    brelse(bh);
    debug_raw("raw io returns %d\n", io_count);
    return io_count;
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

