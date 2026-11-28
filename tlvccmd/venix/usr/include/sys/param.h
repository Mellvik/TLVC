/*
 *		VENIX/86	Version 86/2.0
 *			Edited: 8/24/84
 *
 * User tunable variables.
 * Requires recompiling just conf/c.c if system is parameterized.
 */
#ifndef	SMALL
#define	NRBUF	5		/* Number of raw/async buf headers */
#define	NBUF	20		/* Size of buffer cache in blocks */
#define	NINODE	75		/* Number of in core inodes */
#define	NFILE	70		/* Number of in core file structures */
#define	NMOUNT	6		/* Number of mountable file systems */
#define	NCLIST	20		/* Max total clist size (*CSIZE bytes) */
#define	NCALL	25		/* Max simultaneous time callouts */
#define	NPROC	25		/* Max number of processes */
#define	NSEG	25		/* Max number of shared segments */
#define	NFLOCKS	30		/* Max number of region locks */
#else
#define	NRBUF	2		/* numbers for small (single user) system */
#define	NBUF	15
#define	NINODE	43
#define	NFILE	40
#define	NMOUNT	4
#define	NCLIST	15
#define	NCALL	15
#define	NPROC	15
#define	NSEG	15
#define	NFLOCKS	20
#endif
#define	CMAPSIZ	(4*NPROC)	/* Size of core allocation area */
#define	SMAPSIZ	(2*(NPROC+NSEG))/* Size of swap allocation area */
#define	MAXUPRC	20		/* Max number of processes per user */
#define	TIMEZONE (8*60)		/* Default minutes west of Greenwich */
#define	DSTFLAG	1		/* Default daylight savings time applys here */
#define	HZ	60		/* Default effective tick/second of the clock */

/*
 * Tunable variables.
 * Requires recompiling sources.
 */
#define	NOFILE	20		/* Max open files per process */
#define	NCARGS	2048		/* Max no. of characters on exec */

/*
 * Defines for different flavors of VENIX.
 * Requires recompiling sources.
 */
#define PARAM   		/* Parameterized system (user tunable params) */

/*
 * Priorities.
 */
#define	PFAST	(-110)
#define	PSWP	(-100)
#define	PINOD	(-90)
#define	PRIBIO	(-50)
#define	PZERO	0
#define	PPIPE	1
#define	PWAIT	40
#define	PSLEP	90
#define	PUSER	100

/*
 * Signals.
 */
#define		NSIG	(32+1)
#define		SIGHUP	1	/* hang up */
#define		SIGINT	2	/* interrupt */
#define		SIGQUIT	3	/* quit */
#define		SIGILL	4	/* illegal instruction */
#define		SIGTRAP	5	/* trace or breakpoint */
#define		SIGIOT	6	/* iot -or- async error */
#define		SIGEMT	7	/* emt */
#define		SIGFPE	8	/* floating exception */
#define		SIGKILL	9	/* kill */
#define		SIGBUS	10	/* bus error */
#define		SIGSEGV	11	/* segmentation violation */
#define		SIGSYS	12	/* sys */
#define		SIGPIPE	13	/* end of pipe */
#define		SIGALRM	14	/* alarm clock */
#define		SIGTERM	15	/* software termination */
#define		SIGAIO	16	/* async i/o finished */

/*
 * Fundamental constants.
 */
#define	NULL	0
#define	NODEV	(-1)
#define	ROOTINO	1		/* i number of all roots */
#define	DIRSIZ	14		/* bytes per directory name */
#define BSIZE	512		/* bytes per block */
#define BSHIFT	9		/* bit shift per block */
#define BROUND	0777		/* bytes/block wrap-around */
#define	BSLOP	2		/* bytes of padding between blocks */
#define	NICFREE	100		/* number of in-core free blocks */
#define	NICINOD	100		/* number of in-core free inodes */

/*
 * Several system types definitions.
 */
typedef	unsigned int	daddr_t;
typedef	unsigned int	dev_t;
typedef	char *		caddr_t;
typedef	unsigned int	ino_t;
typedef	long		time_t;
typedef	long		off_t;

/*
 * Some macros amd constants for memory managment.
 *	  "core clicks"	== internal VENIX memory granularity	(memory sizes)
 *	  "physical"	== physical memory granularity		(memory addr)
 *	  "byte"	== virtual address in bytes
 *	  "disk block"	== disk blocks
 */
#define	USIZE	2		/* size of user block (core clicks) */
#define	SSIZE	8		/* initial stack size (core clicks) */
#define	CMAX	128		/* maximum size of segment (core clicks) */

#define	ctop(x)	(((unsigned int)(x))<<5)	   /* core to phys */
#define ctob(x) (caddr_t)(((unsigned int)(x))<<9)  /* core to byte addr */
#define	ctohb(x) (caddr_t)(((unsigned int)(x))>>7) /* core to high byte addr */
#define	ctod(x)	(daddr_t)(x)			   /* core to disk */

#define	ptoc_r(x) (((unsigned int)(x)+31)>>5)	   /* phys to core w rnd */
#define	ptoc(x)	(((unsigned int)(x))>>5)	   /* phys to core */
#define	ptob(x)	(caddr_t)(((unsigned int)(x))<<4)  /* phys to byte addr */
#define	ptohb(x) (caddr_t)(((unsigned int)(x))>>12)/* phys to hi byte addr */
#define	ptod(x)	(daddr_t)ptoc(x)		   /* phys to disk */

#define	lbtoc(x) ((x)>>9)			   /* long byte addr to core */

#define	btoc_r(x) (((unsigned int)(x)+511)>>9)	   /* byte addr to core/rnd */
#define	btoc(x)	(((unsigned int)(x))>>9)	   /* byte addr to core */
#define	hbtoc(x) (((unsigned int)(x))<<7)	   /* high byte addr to core */
#define	btop_r(x) (((unsigned int)(x)+15)>>4)	   /* byte addr to phys/rnd */
#define	btop(x)	(((unsigned int)(x))>>4)	   /* byte addr to phys */
#define	btod(x)	(daddr_t)btoc(x)		   /* byte addr to disk */
#define	bcrnd(x) (((unsigned int)(x))&0777)	   /* byte to core rnd gran */
#define	bprnd(x) (((unsigned int)(x))&017)	   /* byte to phys rnd gran */

#define	dtoc(x)	(unsigned int)(x)		   /* disk to core */
#define	dtop(x)	(((unsigned int)(x))<<5)	   /* disk to phys */
#define	dtob(x)	(caddr_t)(((unsigned int)(x))<<9)  /* disk to byte addr */
#define dtohb(x) (caddr_t)(((unsigned int)(x))>>7) /* disk to high byte addr */

/*
 * Other macros for various units conversions.
 */
#define	itod(x) (daddr_t)(((unsigned)(x)+31)>>4)   /* inum to disk addr */
#define	itoo(x)	(unsigned int)(32*((x+15)&017))    /* inum to disk addr off */
#define	major(x) (((unsigned int)(x))>>8)	   /* major part of device */
#define	minor(x) (((unsigned int)(x))&0377)	   /* minor part of device */
#define	makedev(x,y) (dev_t)((x)<<8 | (y))	   /* make a device number */

#define	PSHIGH(x)	((x&0101000)==0)	   /* PSW high ? */
#define	USER(x)		(x!=0)			   /* From USER mode ? */
#ifndef KERNEL
#define	KERNEL(x)	(x==0)			   /* From KERNEL mode ? */
#endif
