/*
 * Extended (> 1M) memory management support for buffers.
 *
 * Nov 2021 Greg Haerr
 */

#include <linuxmt/config.h>
#include <linuxmt/memory.h>
#include <linuxmt/kernel.h>
#include <linuxmt/init.h>
#include <linuxmt/string.h>
#include <linuxmt/debug.h>
#include <arch/segment.h>

#define KB_TO_LINADDR(x)	((long_t)(x)<<10)
#define LINADDR_TO_KB(x)	((word_t)((x)>>10))

#ifdef CONFIG_FS_XMS

extern int xms_size;	/* Total XMS size, may be less than reported by BIOS, k bytes */
extern int xms_start;	/* Offset from 1M where xms memory starts, normally 0, k bytes */
extern int xms_avail;	/* XMS mem available, i.e. currently not in use */
extern int hma_avail;	/* Set if HMA memory is available */

word_t INITPROC find_xms_start(void);
word_t INITPROC find_xms_end(word_t, word_t);

/* used when running XMS_INT15 */
extern void int15_fmemcpyw(void *dst_off, addr_t dst_seg, void *src_off, addr_t src_seg,
		size_t count);

/*
 * ramdesc_t: if CONFIG_FS_XMS_BUFFER not set, then it's a normal seg_t segment descriptor.
 * Otherwise, it's a physical RAM descriptor (32 bits), used for possible XMS access.
 * When <= 65535, low 16 bits are used as the (seg_t) physical segment.
 * When > 65535, all 32 bits are used as a linear address in unreal mode.
 *
 * addr_t: linear 32-bit RAM address, used directly by CPU using 32-bit
 *     address operand prefix and unreal mode for indexing off of DS:ESI or ES:EDI
 *     in linear32_fmemcypw.
 */

static long_t xms_alloc_ptr;

/* A15 has already been enabled at this point */
void xms_init(void)
{
	if (hma_avail)
		xms_start = 64;		/* set this for meminfo */
	xms_avail = xms_size - xms_start;

	printk("xms: ");
	if (!xms_mode) {
		printk("disabled");
		return;
	}
	if (!xms_size) {
		printk("not available");
		return;
	}
#ifndef CONFIG_FS_XMS_INT15
	if (xms_mode == XMS_INT15) {
	    printk("XMS_INT15 not available, ");
	    xms_mode = XMS_UNREAL;
	}
#endif

	if (arch_cpu == 6) {
		if (xms_mode == XMS_UNREAL) {
#ifdef CONFIG_FS_XMS_LOADALL
		    xms_mode = XMS_LOADALL;
#else
		    xms_mode = XMS_DISABLED;
		    printk("not available");
		    return;
#endif
		}
	}			/* INT15 is ok if present */

	if (xms_mode == XMS_UNREAL) {
		if (!check_unreal_mode()) {
		    printk("disabled, unreal mode requires 386");
		    xms_mode = XMS_DISABLED;
		    return;
		} else
		    enable_unreal_mode();
	}
#ifdef XMS_SIZEUP		/* this is getting complicated */
	if (!hma_avail) {
		/* XMS does not start at 1M, size it up. It may consist of
		 * more than one segment and start anywhere above 1M. We're
		 * using the first assuming it's the largest. 
		 * Useful in unreal mode only
		 */
		if (xms_mode == XMS_UNREAL 
#ifdef CONFIG_FS_XMS_LOADALL
			|| xms_mode == XMS_LOADALL
#endif
						) {
		    xms_start = find_xms_start();	/* kbytes from 1M */
		    xms_avail = find_xms_end(xms_start, xms_size) - xms_start;
		} else
#ifdef CONFIG_FS_XMS_INT15
		    xms_mode = XMS_INT15;	/* Force INT15 mode since HMA not found */
#else
		    xms_mode = XMS_DISABLED;
#endif
	}
#endif

	xms_alloc_ptr = XMS_START_ADDR + KB_TO_LINADDR(xms_start);
	printk("%uk available, ", xms_avail);

	if (xms_mode == XMS_INT15)
		printk("using int 15/1F");
	else if (xms_mode == XMS_LOADALL)
		printk("using 286/LOADALL");
	else 
		printk("using unreal mode");
	if (kernel_cs == 0xffff) printk(", HMA kernel");
	return;
}

/* Allocate from XMS memory - very simple for now, no free,
 * just checking against over-allocation.
 * Unit is K bytes.
 */
ramdesc_t xms_alloc(int size)
{
	long_t mem = xms_alloc_ptr;

	if (size < xms_avail) {
	    xms_alloc_ptr += KB_TO_LINADDR(size);
	    xms_avail -= size;
	    //printk("xms_alloc %lx size %u\n", mem, size);
	    return mem;
	}
	printk("xms: Alloc failed @ %lx\n", xms_alloc_ptr);
	return 0;
}

/* copy bytes between XMS and far memory */
void xms_fmemcpyb(void *dst_off, ramdesc_t dst_seg, void *src_off, ramdesc_t src_seg,
		size_t count)
{
	int	need_xms_src = src_seg >> 16;
	int	need_xms_dst = dst_seg >> 16;

	if (need_xms_src || need_xms_dst) {
		if (!xms_size) panic("xms_fmemcpyb");
		if (!need_xms_src) src_seg <<= 4;
		if (!need_xms_dst) dst_seg <<= 4;

		if (xms_mode == XMS_UNREAL)
		    linear32_fmemcpyb(dst_off, dst_seg, src_off, src_seg, count);
#ifdef CONFIG_FS_XMS_LOADALL
		else if (xms_mode == XMS_LOADALL)
		    loadall_block_move(src_seg+(word_t)src_off, dst_seg+(word_t)dst_off, count);
#endif
#ifdef CONFIG_FS_XMS_INT15
		else {
		/* lots of extra work on odd transfers because INT 15 block moves words only */
		    size_t wc = count >> 1;
		    if (count & 1) {
			static char buf[2];

			if (wc)
			    int15_fmemcpyw(dst_off, dst_seg, src_off, src_seg, wc);
			dst_off += count-1;
			src_off += count-1;

			if (need_xms_src) {
			    /* move from XMS to kernel data segment */
			    int15_fmemcpyw(buf, (addr_t)kernel_ds<<4, src_off, src_seg, 1);
			    pokeb((word_t)dst_off, (seg_t)(dst_seg>>4), buf[0]);
			} else {
			    /* move from kernel data segment to XMS, very infrequent for odd count */
			    addr_t kernel_ds_32 = (addr_t)kernel_ds << 4;
			    int15_fmemcpyw(buf, kernel_ds_32, dst_off, dst_seg, 1);
			    buf[0] = peekb((word_t)src_off, (seg_t)(src_seg>>4));
			    int15_fmemcpyw(dst_off, dst_seg, buf, kernel_ds_32, 1);
			}
			return;
		    }
		    int15_fmemcpyw(dst_off, dst_seg, src_off, src_seg, wc);
		}
#endif
#if !(defined(CONFIG_FS_XMS_INT15) || defined(CONFIG_FS_XMS_LOADALL))
		else panic("xms_memcpyb: config error");
#endif
		return;
	}
	fmemcpyb(dst_off, (seg_t)dst_seg, src_off, (seg_t)src_seg, count);
}

/* memset XMS of far memory, INT 15 not supported */
void xms_fmemset(void *dst_off, ramdesc_t dst_seg, byte_t val, size_t count)
{
	int	need_xms_dst = dst_seg >> 16;

	if (need_xms_dst) {
		if (!xms_size || xms_mode == XMS_INT15) panic("xms_fmemset");

		if (xms_mode == XMS_UNREAL)
		    linear32_fmemset(dst_off, dst_seg, val, count);
#ifdef CONFIG_FS_XMS_LOADALL
		else {
		    //printk("memset %lx,%x,%d; ", dst_seg+(word_t)dst_off, val&0xff, count);
		    loadall_block_move(0xffff0000L, dst_seg+(word_t)dst_off, count);
		}
#endif
		return;
	}
	fmemsetb(dst_off, (seg_t)dst_seg, val, count);
}

#ifdef XMS_SIZEUP
word_t INITPROC find_xms_start(void)
{
	long_t lin_base = XMS_START_ADDR;
	word_t val = 0x79BA;

	while (lin_base < (KB_TO_LINADDR(xms_size) + XMS_START_ADDR)) {
	    printk("start xms @ %lx: %04x\n", lin_base, linear32_peekw((void *)20, lin_base));
	    linear32_pokew((void *)0, lin_base, val);
	    if (linear32_peekw((void *)0, lin_base) == val)
		break;
	    lin_base += 0x10000;
	}
	return LINADDR_TO_KB(lin_base - 0x10000);
}

/* Find the end of the current XMS 'segment', searching up, since there may be holes
 * in it. Stop at 'end' which is normally the systemreported xms size (kb)
 */
word_t INITPROC find_xms_end(word_t start, word_t top)
{
	long_t lin_base = KB_TO_LINADDR(start) + XMS_START_ADDR;
	word_t val = 0x76AB;

	top += LINADDR_TO_KB(XMS_START_ADDR);
	if (top > 0x7fffU) top = 0x7fffU;	/* cap at 32M for now */

	while (lin_base < KB_TO_LINADDR(top)) {
	    linear32_pokew((void *)0x8010, lin_base, val);
	    //printk("find_xms_end %lx: %04x\n", lin_base,
				//linear32_peekw((void *)0x8010, lin_base));
	    if (linear32_peekw((void *)0x8010, lin_base) != val)
		break;
	    lin_base += 0x10000;
	    start += 64;
	}
	return(start);
}
#endif

#ifdef CONFIG_FS_XMS_INT15
struct gdt_table {
	word_t	limit_15_0;
	word_t	base_15_0;
	byte_t	base_23_16;
	byte_t	access_byte;
	byte_t	flags_limit_19_16;
	byte_t	base_31_24;
};

static struct gdt_table gdt_table[8];

/* move words between XMS and main memory using BIOS INT 15h AH=87h block move */
void int15_fmemcpyw(void *dst_off, addr_t dst_seg, void *src_off, addr_t src_seg,
		size_t count)
{
	struct gdt_table *gp;
	memset(gdt_table, 0, sizeof(gdt_table));

	src_seg += (word_t)src_off;
	dst_seg += (word_t)dst_off;

	gp = &gdt_table[2];		/* source descriptor*/
	clr_irq();			/* protect gdt_tabe against reentry */
	gp->limit_15_0 = 0xffff;
	gp->base_15_0 = (word_t)src_seg;
	gp->base_23_16 = src_seg >> 16;
	gp->access_byte = 0x93;		/* present, rignt 0, data, expand-up, writable, accessed */
	//gp->flags_limit_19_16 = 0;	/* byte-granular, 16-bit, limit=64K */
	//gp->flags_limit_19_16 = 0xCF;	/* page-granular, 32-bit, limit=4GB */
	gp->base_31_24 = src_seg >> 24;

	gp = &gdt_table[3];		/* dest descriptor*/
	gp->limit_15_0 = 0xffff;
	gp->base_15_0 = (word_t)dst_seg;
	gp->base_23_16 = dst_seg >> 16;
	gp->access_byte = 0x93;		/* present, rignt 0, data, expand-up, writable, accessed */
	//gp->flags_limit_19_16 = 0;	/* byte-granular, 16-bit, limit=64K */
	//gp->flags_limit_19_16 = 0xCF;	/* page-granular, 32-bit, limit=4GB */
	gp->base_31_24 = dst_seg >> 24;
	block_move(gdt_table, count);
	set_irq();
}
#endif /* CONFIG_FS_XMS_INT15 */
#endif /* CONFIG_FS_XMS */
