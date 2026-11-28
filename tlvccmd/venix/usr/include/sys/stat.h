struct stat
{
	dev_t	st_dev;
	ino_t	st_ino;
	unsigned short st_mode;
	short	st_nlink;
	short	st_uid;
	short	st_gid;
	dev_t	st_rdev;
	off_t	st_size;
	time_t	st_atime;
	time_t	st_mtime;
	time_t	st_ctime;
};

#define	S_IFMT	0160000		/* type of file */
#define		S_IFDIR	0140000	/* directory */
#define		S_IFCHR	0120000	/* character special */
#define		S_IFBLK	0160000	/* block special */
#define		S_IFREG	0100000	/* regular file */
#define	S_ILRG		010000	/* large file */
#define	S_ISUID		004000	/* set user id on execution */
#define	S_ISGID		002000	/* set group id on execution */
#define	S_ISVTX		001000	/* save shared segment, even after use */
#define	S_IREAD		000400	/* read permission, owner */
#define	S_IWRITE	000200	/* write permission, owner */
#define	S_IEXEC		000100	/* execute permission, owner */
