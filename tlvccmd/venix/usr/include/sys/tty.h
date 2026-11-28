/*
 *		VENIX/86	Version 86/1.2
 *			Edited: 12/12/83
 *
 * A clist structure is the head of a linked list queue of characters.
 * The actual data structure is defined below in "cblock".
 */
struct clist {
	int	       c_cc;	/* character count */
	unsigned char *c_cf;	/* pointer to first character */
	unsigned char *c_cl;	/* pointer to last character */
};

/*
 * A tty structure is needed for each VENIX character device that is used
 * for normal terminal IO.  The routines in tty.c handle the common code
 * associated with these structures.  The definition and device dependent
 * code is in each driver.
 */
struct tty {
	struct	clist t_rawq;	/* input chars from device */
	struct	clist t_outq;	/* output list to device */
	unsigned int  t_flags;	/* mode, settable by ioctl call */
	unsigned int  t_state;	/* internal state, not visible externally */
	caddr_t	      t_addr;	/* startup function address */
	unsigned int  t_count;	/* character count */
	unsigned char t_col;	/* printing column of device */
	unsigned char t_line;	/* printing line of device */
	char	      t_erase;	/* erase character */
	char	      t_kill;	/* kill character */
	unsigned int  t_speeds;	/* output + input line speed */
	dev_t	      t_dev;	/* device name */
	unsigned int  t_sem;	/* local semaphores for this process group */
};

/*
 * Structure of arg for ioctl.
 */
struct	ttiocb {
	unsigned int	ioc_speeds;
	char		ioc_erase;
	char		ioc_kill;
	unsigned int	ioc_flags;
};

#define	TTIPRI	10
#define	TTOPRI	20

#define CERASE	('\b')		/* <delete> previous charater */
#define	CEOT	('D'& 037)	/* <^D> send EOF to reader (logoff) */
#define	CKILL	('U'& 037)	/* <^U> kill this line */
#define	CEBUF	('E'& 037)	/* <^E> erase entire type-ahead buffer */
#define	CRETYPE	('R'& 037)	/* <^R> retype the type-ahead buffer */
#define CXON	('Q'& 037)	/* <^Q> restart transmission */
#define CXOFF	('S'& 037)	/* <^S> stop transmission */
#define	CQUIT	('Z'& 037)	/* <^Z> send QUIT signal */
#define	CINTR	('C'& 037)	/* <^C> send INTR signal */

#define	CSIZE	62		/* characters per cblock */
#define	CROUND	077		/* wrap-around mask */

/*
 * Actual structure of the data manipulated by getc/putc.
 */
struct	cblock {
	struct	cblock	*c_next;
	unsigned char	 c_info[CSIZE];
};

/*
 * Limits.
 */
#define	TTHIWAT	100
#define	TTLOWAT	40
#define	TTYHOG	256
#define	TTYSCRL	20

/*
 * Modes - note that character delays are not supported on VENIX
 */
#define	TANDEM	01		/* ^S ^Q processing on i/o */
#define	CBREAK	02		/* c-by-c wakeup on read */
#define	LCASE	04		/* upper case only terminal */
#define	ECHO	010		/* echo charaters when typed */
#define	CRMOD	020		/* map <cr> - <nl> */
#define	RAW	040		/* raw (no processing of characters) */
#define	ODDP	0100		/* odd parity */
#define	EVENP	0200		/* even parity */
#define	NBREAD	0400		/* non-blocking read */
#define	NLDELAY	01400		/* new line delays (not supported on VENIX) */
#define	TBDELAY	06000		/* tab delays (not supported on VENIX) */
#define	XTABS	06000		/* expand tabs on output */
#define	CRDELAY	030000		/* return delays (not supported on VENIX) */
#define	SCROLL	040000		/* Stop output every NSCRL lines */
#define CRT	0100000		/* CRT terminal */

/*
 * Internal state bits.
 */
#define	TIMEOUT	01		/* Delay timeout (not supported on VENIX) */
#define	WOPEN	02		/* waiting for open to complete */
#define	ISOPEN	04		/* device is open */
#define	FLUSH	010		/* outq flushed during DMA */
#define	CARR_ON	020		/* software copy of carrier-present */
#define	BUSY	040		/* output in progress */
#define	ASLEEP	0100		/* wakeup when output done */
#define	XCLUDE	0200		/* exclusive-use flag */
#define	XOFF	0400		/* output was stopped */
#define	HUPCLS	01000		/* hang up on last close */
#define	TBLOCK	02000		/* tandem queue blocked */
#define	ESCAP	010000		/* escape character was typed */
#define	ISLEEP	040000		/* wakeup on input */

/*
 * TTY ioctl commands.
 */
#define TIOCGETD	(('t'<<8)|0)	/* get line disipline */
#define TIOCSETD	(('t'<<8)|1)	/* set line disipline */
#define	TIOCHPCL	(('t'<<8)|2)	/* hangup on close */
#define	TIOCGETP	(('t'<<8)|8)	/* get current parameter */
#define	TIOCSETP	(('t'<<8)|9)	/* set parameters */
#define	TIOCSETN	(('t'<<8)|10)	/* set parameters without flush */
#define	TIOCEXCL	(('t'<<8)|13)	/* set exclusive use */
#define	TIOCNXCL	(('t'<<8)|14)	/* clear exclusive use */
#define	TIOCFLUSH	(('t'<<8)|16)	/* flush i/o */
#define TIOCSETC	(('t'<<8)|17)	/* set special characters */
#define TIOCGETC	(('t'<<8)|18)	/* get special characters */
#define	TIOCQCNT	(('t'<<8)|30)	/* get char counts on i/o queues */

/*
 * DIO and FIO features not supported.
 *
 * AIO (asynchronous i/o controls)
 */

#define	AIOCWAIT	(('a'<<8)|0)	/* wait/test outstanding requests */
