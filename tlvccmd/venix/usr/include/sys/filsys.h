/*
 * Definition of the unix super block.
 * The root super block is allocated and
 * read in iinit/alloc.c. Subsequently
 * a super block is allocated and read
 * with each mount (smount/sys3.c) and
 * released with unmount (sumount/sys3.c).
 * A disk block is ripped off for storage.
 * See alloc.c for general alloc/free
 * routines for free list and I list.
 */
struct	filsys {
	int	s_isize;	/* size in blocks of I list */
	daddr_t	s_fsize;	/* size in blocks of entire volume */
	int	s_nfree;	/* number of in core free blocks */
	daddr_t	s_free[NICFREE];/* in core free blocks */
	int	s_ninode;	/* number of in core I nodes */
	ino_t	s_inode[NICINOD];  /* in core free I nodes */
	char	s_flock;	/* lock during free list manipulation */
	char	s_ilock;	/* lock during I list manipulation */
	char	s_fmod;		/* super block modified flag */
	char	s_ronly;	/* mounted read-only flag */
	long	s_time;		/* current date of last update */
	int	s_pad[49];	/* padding to make it a full block */
};
