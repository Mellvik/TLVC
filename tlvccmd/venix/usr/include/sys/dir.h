#ifndef	DIRSIZ
#define	DIRSIZ	14
#endif

struct	direct {		/* structure of directory entry */
	ino_t	d_ino;		/* inode number */
	char	d_name[DIRSIZ];	/* file name */
};
