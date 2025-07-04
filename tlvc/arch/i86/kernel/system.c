#include <linuxmt/config.h>
#include <linuxmt/types.h>
#include <linuxmt/kernel.h>
#include <linuxmt/wait.h>
#include <linuxmt/sched.h>
#include <linuxmt/fs.h> /* for ROOT_DEV */
#include <linuxmt/heap.h>
#include <linuxmt/memory.h>

#include <arch/segment.h>
#include <arch/system.h>
#include <arch/io.h>

seg_t membase, memend;  /* start and end segment of available main memory */
unsigned int heapsize;	/* max size of kernel near heap */
byte_t sys_caps;	/* system capabilities bits */
unsigned char arch_cpu; /* CPU type from cputype.S */
void INITPROC val_heapsize(void); /* validate heapsize from config or bootopts */

unsigned int INITPROC setup_arch(void)
{
	unsigned int endbss;

#ifdef CONFIG_HW_COMPAQFAST
	outb_p(1,0xcf);	/* Switch COMPAQ Deskpro to high speed */
#endif

	/*
	 * Extend kernel data segment to maximum of 64K to make room for local heap.
	 *
	 * Set membase to beginning of available main memory, which
	 * is directly after end of the kernel data segment.
	 *
	 * Set memend to end of available main memory - possibly overridden by 
	 * a 'memsize=' setting in bootopts.
	 * If ramdisk configured, subtract space for it from end of memory.
	 *
	 * Calculate heapsize for near heap allocator.
	 * Return start address for near heap allocator.
	 */

	/* Heap allocations at even addresses */
	endbss = (unsigned int)(_endbss + 1) & ~1;

	/* Calculate or set size of heap which - unless set specifically,
	 * extends to the end of end of the kernel DS */

#ifdef SETUP_HEAPSIZE		/* MK88 only */
	if (!heapsize)
	    heapsize = SETUP_HEAPSIZE;
#endif
#ifndef CONFIG_OPTSEG_HIGH
	if (heapsize) {			/* may be set via heap= in /bootopts */
		val_heapsize();
	} else
#endif
	{
           membase = kernel_ds + 0x1000;
           heapsize = 1 + ~endbss;
	}
	//debug("endbss %x heap %x kdata size %lx\n", endbss, heapsize, (long_t)(membase-kernel_ds)<<4);

	if (!memend) 				/* bootopts setting overrides */
		memend = SETUP_MEM_KBYTES << 6;
	//if (memend > 0xa000) memend = 0xa000;	/* sanity check */

#if defined(CONFIG_RAMDISK_SEGMENT) && (CONFIG_RAMDISK_SEGMENT > 0)
	if (CONFIG_RAMDISK_SEGMENT <= memend) {
		/* reduce top of memory by size of ram disk */
		memend -= CONFIG_RAMDISK_SECTORS << 5;
	}
#endif

	arch_cpu = SETUP_CPU_TYPE;
#ifdef SYS_CAPS
	sys_caps = SYS_CAPS;	/* custom system capabilities */
#else
	if (arch_cpu > 5)       /* 80286+ IBM PC/AT capabilities or Unknown CPU */
		sys_caps = CAP_ALL;
#endif
	return endbss;		/* used as start address in near heap init */

}
void INITPROC val_heapsize(void) {
	unsigned int heapsegs;
	unsigned int endbss = (unsigned int)(_endbss + 1) & ~1;

	heapsegs = (1 + ~endbss) >> 4;  /* max possible heap in segments */
	if ((heapsize >> 4) < heapsegs) /* allow if less than max */
		heapsegs = heapsize >> 4;
	membase = kernel_ds + heapsegs + (((unsigned int) (_endbss+15)) >> 4);
	heapsize = heapsegs << 4;
}

/*
 * The following routines may need porting on non-IBM PC architectures
 */

/*
 * This function gets called by the keyboard interrupt handler.
 * As it's called within an interrupt, it may NOT sync.
 */
void ctrl_alt_del(void)
{
    hard_reset_now();
}

void hard_reset_now(void)
{
#ifdef CONFIG_ARCH_IBMPC
    asm("mov $0x40,%ax\n\t"
	"mov %ax,%ds\n\t"
	"movw $0x1234,0x72\n\t"
	"ljmp $0xFFFF,$0\n\t"
	);
#endif
}

/*
 * Use Advanced Power Management to power off system
 * For details on how this code works, see
 * http://wiki.osdev.org/APM
 */
void apm_shutdown_now(void)
{
#if defined(CONFIG_APM) && defined(CONFIG_ARCH_IBMPC)
    asm("movw $0x5301,%ax\n\t"
	"xorw %bx,%bx\n\t"
	"int $0x15\n\t"
	"jc apm_error\n\t"
	"movw $0x5308,%ax\n\t"
	"movw $1,%bx\n\t"
	"movw $1,%cx\n\t"
	"int $0x15\n\t"
	"jc apm_error\n\t"
	"movw $0x5307,%ax\n\t"
	"movw $1,%bx\n\t"
	"movw $3,%cx\n\t"
	"int $0x15\n\t"
	"apm_error:\n\t"
	);
#endif
}
