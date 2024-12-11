#ifndef __LINUXMT_CONFIG_H
#define __LINUXMT_CONFIG_H

#include <autoconf.h>
#include <linuxmt/major.h>

/*
 * Compile-time configuration
 */

#ifdef CONFIG_ARCH_IBMPC
#define MAX_SERIAL		4		/* max number of serial tty devices*/

/* If SETUP_MEM_KBYTES_ASM is unset, the BIOS will be queried for the himem value */
/* (in setup.S) */
//#define SETUP_MEM_KBYTES_ASM    640		/* EXPERIMENTAL - make use of the top */
						/* 1k reserved by the BIOS */
/*
 * Setup data - normally queried by startup setup.S code, but can
 * be overridden for embedded systems with less overhead.
 * See setup.S for more details.
 */
#define SETUP_VID_COLS		setupb(7)	/* BIOS video # columns */
#define SETUP_VID_LINES		setupb(14)	/* BIOS video # lines */
#define SETUP_CPU_TYPE		setupb(0x20)	/* processor type */
#define SETUP_MEM_KBYTES	setupw(0x2a)	/* base memory in 1K bytes */
#define SETUP_ROOT_DEV		setupw(0x1fc)	/* root device, kdev_t or BIOS dev */
#define SETUP_ELKS_FLAGS	setupb(0x1f6)	/* flags for root device type */
#define SETUP_PART_OFFSETLO	setupw(0x1e2)	/* partition offset low word */
#define SETUP_PART_OFFSETHI	setupw(0x1e4)	/* partition offset high word */
#ifdef CONFIG_ROMCODE
#define SYS_CAPS	(CAP_PC_AT|CAP_DRIVE_PARMS)
#endif
#define UTS_MACHINE		"ibmpc i8086"

/* The following can be set for minimal systems or for QEMU emulation testing:
 * 10 buffers (@20 = 200), 2 ttyq (@80 = 160), 4k L1 cache, 512 heap free,
 * 10 tasks (@876 = 8760) = ~13728.
 * Use bufs=10 cache=4 tasks=10 in /bootopts
 */
#if defined(CONFIG_HW_MK88)
#define SETUP_HEAPSIZE            13728         /* force kernel heap size */
#endif
//#undef SETUP_MEM_KBYTES
//#define SETUP_MEM_KBYTES        256     /* force available memory in 1K bytes */
//#define SETUP_MEM_KBYTES_ASM    SETUP_MEM_KBYTES

#endif /* CONFIG_ARCH_IBMPC */

#ifdef CONFIG_ARCH_PC98
#define MAX_SERIAL		1	/* max number of serial tty devices*/
#define SETUP_VID_COLS		80	/* video # columns */
#define SETUP_VID_LINES		25	/* video # lines */
#define SETUP_CPU_TYPE		1	/* processor type = 8086 */
#define SETUP_MEM_KBYTES	640	/* base memory in 1K bytes */
#define SETUP_ROOT_DEV		setupw(0x1fc)	/* root device, kdev_t or BIOS dev */
#define SETUP_ELKS_FLAGS	setupb(0x1f6)	/* flags for root device type */
#define SETUP_PART_OFFSETLO	setupw(0x1e2)	/* partition offset low word */
#define SETUP_PART_OFFSETHI	setupw(0x1e4)	/* partition offset high word */
#define SYS_CAPS		CAP_IRQ8TO15	/* enable capabilities */
#define UTS_MACHINE		"pc-98 i8086"

#define CONFIG_VAR_SECTOR_SIZE	/* sector size may vary across disks */
#endif

#ifdef CONFIG_ARCH_8018X
#define MAX_SERIAL		2	/* max number of serial tty devices*/
#define SETUP_VID_COLS		80	/* video # columns */
#define SETUP_VID_LINES		25	/* video # lines */
#define SETUP_CPU_TYPE		5	/* processor type 80186 */
#define SETUP_MEM_KBYTES	512	/* base memory in 1K bytes */
#define SETUP_ROOT_DEV		0x0600	/* root device ROMFS */
#define SETUP_ELKS_FLAGS	0	/* flags for root device type */
#define SETUP_PART_OFFSETLO	0	/* partition offset low word */
#define SETUP_PART_OFFSETHI	0	/* partition offset high word */
#define SYS_CAPS		0	/* no XT/AT capabilities */
#define UTS_MACHINE		"8018x"

#define CONFIG_8018X_FCPU	16
#define CONFIG_8018X_EB
#endif

/*
 * System capabilities - configurable for ROM or custom installations.
 * Normally, all capabilities will be set if arch_cpu > 5 (PC/AT),
 * except when SYS_CAPS is defined for custom installations or emulations.
 */
#define CAP_PC_AT	(CAP_IRQ8TO15|CAP_IRQ2MAP9)      /* PC/AT capabilities */
#define CAP_DRIVE_PARMS	0x02		/* has INT 13h fn 8 drive parms */
#define CAP_KBD_LEDS	0x04		/* has keyboard LEDs */
#define CAP_HD_IDE	0x08		/* can do hard drive IDE probes */
#define CAP_IRQ8TO15	0x10		/* has IRQ 8 through 15 */
#define CAP_IRQ2MAP9	0x20		/* map IRQ 2 to 9 */
#define CAP_ALL		0xFF		/* all capabilities if PC/AT only */

/* Don't touch these, unless you really know what you are doing. */
#define DEF_INITSEG	0x0100		/* initial Image load address by boot code */
#define DEF_SYSSEG	0x1300		/* kernel copied here by setup.S code */
#define DEF_SETUPSEG	(DEF_INITSEG + 0x20)
#define DEF_SYSSIZE	0x2F00

#ifdef CONFIG_ROMCODE
#if defined(CONFIG_BLK_DEV_BHD) || defined(CONFIG_BLK_DEV_BFD)
#define DMASEG		0x80	/* 0x400 bytes floppy sector buffer */
#define DMASEGSZ	0x2400	/* SECTOR_SIZE * 18 (9216) */
#define KERNEL_DATA	0x2C0	/* kernel data segment */
#else
#define KERNEL_DATA     0x80	/* kernel data segment */
#endif	/* CONFIG_BLK_DEV_BIOS */
#define SETUP_DATA	CONFIG_ROM_SETUP_DATA
#endif /* CONFIG_ROMCODE */


#if (defined(CONFIG_ARCH_IBMPC) || defined(CONFIG_ARCH_8018X)) && !defined(CONFIG_ROMCODE)

/* Sanitize floppy cache size config - change to test larger caches */
#if CONFIG_FLOPPY_CACHE > 7
#undef CONFIG_FLOPPY_CACHE
#define CONFIG_FLOPPY_CACHE 7
#endif

/* Define segment locations of low memory, must not overlap.

   Reorganized dec 2024 (HS): setup data + bootopts buffer +
   floppy cache (if present) are now contiguous. If the
   floppy cache is allocated but not used (bootopts fdcache=0),
   the entire block may be released and made available as general memory.
   The structure is: Fixed segs first, then variable/configurable, 
   then the kernel REL_SYSSEG.
   NOTE: The meminfo program makes assumptions about the order of these blocks!

 | kernel text     |
 +-----------------+ Kernel CS = REL_SYSSEG = (XD_BOUNCESEG + (XD_BOUNCESEGSZ>>4))
 | Other bounce    |  Optional, 1k
 +-----------------+ XD_BOUNCESEG = (FD_BOUNCESEG + (0x400>>4))
 | Floppy bounce   |  1k
 +-----------------+ FD_BOUNCESEG = (FD_CACHESEG + (CONFIG_FLOPPY_CACHE<<10))
 | Floppy cache    |  Optional, max 7k
 +-----------------+ FD_CACHESEG = (DEF_OPTSEG + (OPTSEGSZ>>4)) = DMASEG
 | bootopts buffer |  Optional 1k
 +-----------------+ 0x70 DEF_OPTSEG
 | setup data      |  512B
 +-----------------+ 0x50 REL_INITSEG 
 | IRQ vectors/BDA |
 +-----------------+ 000

*/
#ifdef CONFIG_BOOTOPTS
#define OPTSEGSZ        0x400	/* 0x400 bytes boot options*/
#else
#define OPTSEGSZ        0
#endif

#if defined(CONFIG_BLK_DEV_XD) || defined(CONFIG_ETH_LANCE)
#define XD_BOUNCESEGSZ		0x400
#else
#define XD_BOUNCESEGSZ		0
#endif

#define REL_INITSEG	0x50	/* IRQ vectors/ BDA below this */
#define SETUP_DATA	REL_INITSEG			/* 0x200 bytes setup data */
#define DEF_OPTSEG	0x70
#define FD_CACHESEG	(DEF_OPTSEG + (OPTSEGSZ>>4))
#define DMASEG		FD_CACHESEG
#define FD_CACHESEGSZ	(CONFIG_FLOPPY_CACHE<<10)	/* Bytes, may be zero */
#define FD_BOUNCESEG	(FD_CACHESEG + (FD_CACHESEGSZ>>4))
#define XD_BOUNCESEG	(FD_BOUNCESEG + (0x400>>4))
#define REL_SYSSEG	(XD_BOUNCESEG + (XD_BOUNCESEGSZ>>4))

#endif 	/* ARCH_PC, ARCH_8016X, !ROM */

#if defined(CONFIG_ARCH_PC98) && !defined(CONFIG_ROMCODE)
/* Define segment locations of low memory, must not overlap */
#define DEF_OPTSEG	0x60		/* 0x200 bytes boot options*/
#define OPTSEGSZ	0x200		/* max size of /bootopts file */
#define REL_INITSEG	0x80		/* 0x200 bytes setup data */
#define DMASEG		0xA0		/* 0x400 bytes floppy sector buffer */

#define DMASEGSZ	0x2400		/* SECTOR_SIZE * 18 (9216) */
#define REL_SYSSEG	0x2E0		/* kernel code segment */
#define SETUP_DATA	REL_INITSEG
#endif /* CONFIG_ARCH_PC98 && !CONFIG_ROMCODE */


/*
 * Defines for what uname() should return.
 * The definitions for UTS_RELEASE and UTS_VERSION are now passed as
 * kernel compilation parameters, and should only be used by elks/kernel/version.c
 */
#define UTS_SYSNAME "TLVC"
#define UTS_NODENAME "tlvc"		/* someday set by sethostname() */

#endif
