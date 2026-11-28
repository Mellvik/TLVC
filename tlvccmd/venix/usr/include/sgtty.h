/*
 * Define the structure for stty & gtty system calls
 * (none of the delay algorithims are implemented on VENIX)
 */

struct sgttyb {
	char sg_ispeed;		/* input speed */
	char sg_ospeed;		/* output speed */
	char sg_erase;		/* erase character */
	char sg_kill;		/* kill character */
	int  sg_flags;		/* mode flags */
};

#define	B9600	13		/* different speeds */
#define	B4800	12
#define	B2400	11
#define	B1800	10
#define	B1200	9
#define	B600	8
#define	B300	7
#define	B200	6
#define	B150	5
#define	B134	4
#define	B110	3
#define	B75	2
#define	B50	1
#define	B0	0
#define EXTA	14		/* External A--non-functional on VENIX  */
#define EXTB	15		/* External B--non-functional on VENIX  */

#define	TANDEM	0001		/* different flags */
#define	CBREAK	0002
#define	LCASE	0004
#define	ECHO	0010
#define	CRMOD	0020
#define	RAW	0040
#define	ODDP	0100
#define	EVENP	0200
#define	ANYP	0300
#define ALLDELAY	0177400	/* non-functional on VENIX */
#define BSDELAY		0100000	/* non-functional on VENIX */
#define BS0		0	/* non-functional on VENIX */
#define BS1		0100000	/* non-functional on VENIX */
#define	NLDELAY	01400
#define	NL3	01400
#define	NL2	01000
#define	NL1	00400
#define	NL0	00000
#define	XTABS	06000
#define	TBDELAY	06000
#define	TAB2	04000
#define	TAB1	02000
#define	TAB0	00000
#define	CRDELAY	030000
#define	CR3	030000
#define	CR2	020000
#define	CR1	010000
#define	CR0	000000
#define	VTDELAY	040000
#define	FF1	040000
#define	FF0	000000
#define	CRT	0100000

/*
 * IOCTL commands
 */
#define	TIOCGETD	(('t'<<8)|0)	/** % get line disipline */
#define	TIOCSETD	(('t'<<8)|1)	/** % set line disipline */
#define	TIOCHPCL	(('t'<<8)|2)	/* hangup on close */
#define	TIOCGETP	(('t'<<8)|8)	/* get current parameter */
#define	TIOCSETP	(('t'<<8)|9)	/* set parameters */
#define	TIOCSETN	(('t'<<8)|10)	/* set parameters without flush */
#define	TIOCEXCL	(('t'<<8)|13)	/* set exclusive use */
#define	TIOCNXCL	(('t'<<8)|14)	/* clear exclusive use */
#define	TIOCFLUSH	(('t'<<8)|16)	/* flush i/o */
#define	TIOCSETC	(('t'<<8)|17)	/** % set special characters */
#define TIOCGETC	(('t'<<8)|18)	/** %get special characters */
#define	TIOCQCNT	(('t'<<8)|30)	/* get char counts on i/o queues */
/*
 * DIO and FIO features not supported
 *
 * AIO (asynchronous i/o controls)
 */
#define	AIOCWAIT	(('a'<<8)|0)	/* wait/test outstanding requests */
