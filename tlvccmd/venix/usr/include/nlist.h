/*
 * Format of the old symbol table entry. This is here for compatability.
 * The nlist subroutine takes an old symbol table format as its argument
 * and it knows how to read the format actually stored in the file.
 */

#define	SYMNMLEN	8

struct	nlist {
	char	n_name[SYMNMLEN];	/* symbol name */
	short	n_type;			/* type  */
	long	n_value;		/* value */
};

/*
 * Simple values for n_type.
 */
#define	N_UNDF	0x0		/* undefined */
#define	N_ABS	0x2		/* absolute */
#define	N_TEXT	0x4		/* text */
#define	N_DATA	0x6		/* data */
#define	N_BSS	0x8		/* bss */
#define	N_COMM	0x12		/* common (internal to ld) */
#define	N_FN	0x1f		/* file name symbol */

#define	N_EXT	01		/* external bit, or'ed in */
#define	N_TYPE	0x1e		/* mask for all the type bits */

#define	N_STAB	0xe0

/*
 * Format for namelist values.
 */
#define	N_FORMAT	"%08lx"

