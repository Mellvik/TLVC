/*
 * Each buffer in the pool is usually doubly linked into 2 lists:
 *  - the device with which it is currently associated (always),
 *  - and also on a list of blocks available for allocation for
 *    other use (usually).
 * The latter list is kept in last-used order, and the two lists
 * are doubly linked to make it easy to remove a buffer from one
 * list when it was found by looking through the other.  A buffer
 * is on the available list, and is liable to be reassigned to
 * another disk block, if and only if it is not marked BUSY.  When
 * a buffer is busy, the available-list pointers can be used for
 * other purposes.  Most drivers use the forward ptr as a link in
 * their I/O  active queues.  A buffer header contains all the
 * information required to perform the I/O.  Most of the routines
 * which manipulate these things are in bio.c.
 */
struct buf {
	int	b_flags;		/* see defines below */
	struct	buf   *b_forw;		/* headed by devtab of b_dev */
	struct	buf   *b_back;		/*   " */
	struct	buf   *av_forw;		/* position on free list, */
	struct	buf   *av_back;		/*     if not BUSY */
	int	b_dev;			/* major+minor device name */
	int	b_wcount;		/* transfer count (neg. words) */
	caddr_t	b_addr;			/* low order core address */
	int	b_xmem;			/* high order core address */
	daddr_t b_blkno;		/* block no. on device */
	int	b_resid;		/* words not transferred after error */
	struct	user  *b_procp;		/* requesting process (A_AIO) */
} buf[];

/*
 * Each block device has a devtab, which contains private state stuff
 * and 2 list heads: the b_forw/b_back list, which is doubly linked
 * and has all the buffers currently associated with that major device;
 * and the d_actf/d_actl list, which is private to the device but in
 * fact is always used for the head and tail of the I/O queue for the
 * device.  Various routines in bio.c look at b_forw/b_back (notice
 * they are the same as in the buf structure) but the rest is private
 * to each device driver.
 */
struct devtab {
	char	d_active;		/* busy flag */
	char	d_errcnt;		/* error count (for recovery) */
	struct	buf *b_forw;		/* first buffer for this dev */
	struct	buf *b_back;		/* last buffer for this dev */
	struct	buf *d_actf;		/* head of I/O queue */
	struct 	buf *d_actl;		/* tail of I/O queue */
};

/*
 * This is the head of the queue of available buffers-- all unused
 * except for the 2 list heads.  Also headers for swapping and raw.
 */
struct	buf bfreelist;
struct	buf swbuf0;
struct	buf swbuf1;
struct	buf rawbuf[];

/*
 * These flags are kept in b_flags.
 */
#define	B_WRITE	     0	/* Non-read pseudo-flag */
#define	B_READ	    01	/* Read when I/O occurs */
#define	B_DONE	    02	/* Transaction finished */
#define	B_ERROR	    04	/* Transaction aborted */
#define	B_BUSY	   010	/* Not on av_forw/back list */
#define	B_PHYS	   020	/* Physical IO potentially using UNIBUS map */
#define	B_MAP	   040	/* This block has the UNIBUS map allocated */
#define	B_WANTED  0100	/* Issue wakeup when BUSY goes off */
#define	B_AGE	  0200	/* Delayed write for correct aging */
#define	B_ASYNC	  0400	/* Don't wait for I/O completion */
#define	B_DELWRI 01000	/* Don't write till block leaves available list */

/*
 * These bits decode the minor device number.
 */
#define	A_AIO	0200	/* Raw I/O is to asynchronous */
#define	A_SSIG	0100	/* Send signal SIGAIO when async i/o finished */

#define	D_CLOSE	0040	/* Call driver close routine on all closes */
