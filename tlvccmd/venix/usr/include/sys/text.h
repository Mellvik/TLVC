/*
 * Text and data structure.
 * One allocated per shared structrure.
 */
struct text {
	unsigned char x_flag;
	unsigned char x_ccount;	/* number of loaded references */
	unsigned int  x_count;	/* reference count */
	daddr_t	      x_daddr;	/* disk address of segment (if established) */
	caddr_t	      x_caddr;	/* core address, if loaded */
	unsigned int  x_size;	/* size in core clicks */
	struct inode *x_iptr;	/* inode of prototype */
} text[];

/*
 * flag bit definitions
 */
#define	XLOCK	0100		/* shared segment is locked */
#define	XWANT	0200		/* shared segment wanted */

