#ifndef __LINUXMT_CONFIG_H
#define __LINUXMT_CONFIG_H

#include <autoconf.h>
#include <linuxmt/major.h>

/*
 * Compile-time configuration
 */

#define CONFIG_OPTSEG_HIGH	/* Load /bootopts (of any size) into high memory */
				/* Not currently working on FAT boot devices */
#ifdef CONFIG_ARCH_IBMPC
#define MAX_SERIAL		4		/* max number of serial tty devices */
#define MAX_XMS_SIZE		0x7fff		/* Caps XMS-size to 32M (mask - setup.S) */

/* If SETUP_MEM_KBYTES_ASM is unset, the BIOS will be queried for the himem value */
/* (in setup.S) */
//#define SETUP_MEM_KBYTES_ASM    640		/* EXPERIMENTAL - make use of the top */
						/* 1k reserved by the BIOS */

/* linear address to start XMS buffer allocations from */
#define XMS_START_ADDR	  0x00100000L	/* 1M */
//#define XMS_START_ADDR  0x00FA0000L	/* 15.6M (Compaq with only 1M ram) */

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
#define SETUP_XMS_SIZE		setupw(0x1ea)	/* xms size in kbytes */
#ifdef CONFIG_ROMCODE
#define SYS_CAPS	(CAP_PC_AT|CAP_DRIVE_PARMS)
#endif
#define UTS_MACHINE		"ibmpc i8086"

/* Fast serial input handler for arrow keys or fast SLIP on very slow systems */
#define CONFIG_FAST_IRQ4                      /* com1/ttyS0 */
#define CONFIG_FAST_IRQ3                      /* com2/ttyS1 */

/* The following can be set for minimal systems or for QEMU emulation testing:
 * 10 buffers (@20 = 200), 2 ttyq (@80 = 160), 4k L1 cache, 512 heap free,
 * 10 tasks (@876 = 8760), 64 inodes (@80 = 5120), 64 files (@14 = 896) = ~19744.
 * Use buf=10 cache=4 task=10 inode=64 file=64  in /bootopts
 */
#if defined(CONFIG_HW_MK88)
#define SETUP_HEAPSIZE		19744	/* force kernel heap size */
#endif
//#undef SETUP_MEM_KBYTES
//#define SETUP_MEM_KBYTES	256	/* force available memory in 1K bytes */
//#define SETUP_MEM_KBYTES_ASM	SETUP_MEM_KBYTES

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
#define DEF_SYSSEG	0x1400		/* kernel copied here by setup.S code */
#define DEF_SETUPSEG	(DEF_INITSEG + 0x20)
#define DEF_SYSMAX	0x2F00
#define DEF_MINHEAP	0x8000		/* minimal kernel heap */

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

   Reorganized Dec 2024 (HS): The bootopts buffer and the FD bounce buffer
   are now the same (they're used at different stages), so there is no
   OPTSEG to release after boot. Also, the setup-data seg (REL_INITSEG) now
   overlaps with the FD_CACHE if present. If there is a FD_CACHE allocated
   and not used, it will be released - including the setup-data segment.
   If there is no FD_CACHE, the setup-data-seg may be released but its small
   size may make it less than useful.

   NOTE: The meminfo(1) program makes assumptions about the order of these blocks!

 | kernel text     |
 +-----------------+ Kernel CS = REL_SYSSEG = (XD_BOUNCESEG + (XD_BOUNCESEGSZ>>4))
 | Other bounce    |  Optional, 1k-1.6k
 | (MFM, LANCE)    |
 +-----------------+ XD_BOUNCESEG = REL_INITSEG + (HAS_FDCACHE ? (FD_CACHESEGSZ>>4):0x10)
 | Floppy cache    |  Optional, variable, max 6.5 + 0.5k
 | setup data      |  Mandatory, 512B
 +-----------------+ 0x90 REL_INITSEG = FD_CACHESEG = DMASEG
 | Floppy bounce   |  1k (mandatory)
 | bootopts buffer |  (deprecated)
 +-----------------+ 0x50 DEF_OPTSEG FD_BOUNCESEG
 | IRQ vectors/BDA |  NOTE:  If XMS_LOADALL, this segment is expanded to 0x90
 +-----------------+ 000

*/

#if defined(CONFIG_BLK_DEV_XD) || defined(CONFIG_ETH_LANCE)
#define XD_BOUNCESEGSZ		0x400
#ifdef CONFIG_ETH_LANCE
#undef XD_BOUNCESEGSZ
#define XD_BOUNCESEGSZ		0x610	/* must hold a complete packet */
#endif
#else
#define XD_BOUNCESEGSZ		0
#endif

#define FD_CACHESEGSZ	(CONFIG_FLOPPY_CACHE<<10)	/* Bytes, may be zero */
#if CONFIG_FLOPPY_CACHE
#define HAS_FDCACHE 1
#else
#define HAS_FDCACHE 0
#endif

#define BDA_IAC_SEG		0x4F	/* BDA 'Intra-Applications Communications Area' */
					/* 16 bytes */
#define BDA_IAC_OPTSEG		0x4F0	/* Seg where boot loads /bootopts */
#define BDA_IAC_OPTOFFS		0x4F2	/* Offset */
#define BDA_IAC_OPTSIZE		0x4F4	/* Size */

// NOTE: for accomodating the LOADALL 0x80:0 seg (0x80:0-0x87:0),
// leave OPTSEG in place (its use is over before LOADALL needs it)
// and move REL_INITSEG above LOADALL (0x87 or 0x90)

#define LOADALL_SEG	0x80		/* 286 only, 0x80-0x87 */
#ifdef CONFIG_FS_XMS_LOADALL
#define DEF_OPTSEG	0x90		/* release 0x50-0x7f in init */
#define UNUSED_SEG	0x50		/* too small to release */
					/* maybe use for SETUP_DATA */
#else
#define DEF_OPTSEG	0x50
#endif
#define OPTSEGSZ	0x400		/* max size of /bootopts file */
#define FD_BOUNCESEG	DEF_OPTSEG	/* IRQ vectors/ BDA below this */
#define DMASEG		FD_BOUNCESEG	/* used by bioshd driver only */
#define REL_INITSEG	(FD_BOUNCESEG+0x40)
#define SETUP_DATA	REL_INITSEG	/* 0x200 bytes setup data */
#define FD_CACHESEG	SETUP_DATA	/* overlapping if present */
#define XD_BOUNCESEG	(FD_CACHESEG + 0x20 + \
			(FD_CACHESEGSZ>>4) - (HAS_FDCACHE*0x20))
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
