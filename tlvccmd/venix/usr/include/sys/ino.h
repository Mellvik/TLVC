/*
 * Inode structure as it appears on
 * the disk. Not used by the system,
 * but by things like check, df, dump.
 */
struct	inode {
	int		i_mode;
	unsigned char	i_nlink;
	unsigned char	i_uid;
	unsigned char	i_gid;
	unsigned char	i_size0;
	unsigned int	i_size1;
	unsigned int	i_addr[8];
	int		i_atime[2];
	int		i_mtime[2];
};

/* modes */
#define		IALLOC	0100000
#define		IFMT	 060000
#define		IFDIR	 040000
#define		IFCHR	 020000
#define		IFREG	 000000
#define		IFBLK	 060000
#define		ILARG	 010000
#define		ISUID	  04000
#define		ISGID	  02000
#define		ISVTX	  01000
#define		IREAD	   0400
#define		IWRITE	   0200
#define		IEXEC	   0100

#define	INOPB	16
