/*
 * The I node is the focus of all file activity in unix.
 * There is a unique inode allocated for each active file,
 * each current directory, each mounted-on file, shared
 * segment file, and the root.  An inode is `named' by
 * its dev/inumber pair. (iget/iget.c) Data, from mode on,
 * is read in from permanent inode on volume.
 */
struct	inode {
	unsigned char	 i_flag;
	unsigned char	 i_count;	/* reference count */
	dev_t		 i_dev;		/* device where inode resides */
	ino_t		 i_number;	/* 1-to-1 with device address */
	int		 i_mode;	/* mode falgs */
	unsigned char	 i_nlink;	/* directory entries */
	unsigned char	 i_uid;		/* owner */
	unsigned char	 i_gid;		/* group of owner */
	unsigned char	 i_size0;	/* most significant of size */
	unsigned int	 i_size1;	/* least sig */
	daddr_t		 i_addr[8];	/* block numbers constituting file */
	daddr_t		 i_lastr;	/* last log blk read (read-ahead) */
#ifndef	NO_RLOCK
	struct locklist	*i_locklist;	/* locked region list */
#endif
} inode[];

#ifndef	NO_RLOCK
struct locklist {
	struct locklist	*ll_link;	/* link to next lock region */
	int		 ll_flags;	/* misc flags */
	struct proc	*ll_proc;	/* process which owns region */
	short		 ll_start[2];	/* starting offset */
	short		 ll_end[2];	/* ending offset */
} locklist[];
#endif

/*
 * Flags.
 */
#define	ILOCK	01		/* inode is locked */
#define	IUPD	02		/* inode has been modified */
#define	IACC	04		/* inode access time to be updated */
#define	IMOUNT	010		/* inode is mounted on */
#define	IWANT	020		/* some process waiting on lock */
#define	ITEXT	040		/* inode is shared segment prototype */
#define	IPIPE	0100		/* inode is a pipe */

/*
 * Inode structure as it appears on the disk.
 */
struct	dinode {
	int		di_mode;
	unsigned char	di_nlink;
	unsigned char	di_uid;
	unsigned char	di_gid;
	unsigned char	di_size0;
	unsigned int	di_size1;
	daddr_t		di_addr[8];
	long		di_atime;
	long		di_mtime;
};

/*
 * Modes.
 */
#define		IALLOC	0100000	/* file is used */
#define		IFMT	 060000	/* type of file */
#define		IFDIR	 040000	/* directory */
#define		IFCHR	 020000	/* character special */
#define		IFBLK	 060000	/* block special, 0 is regular */
#define		ILARG	 010000	/* large addressing algorithm */
#define		ISUID	  04000	/* set user id on execution */
#define		ISGID	  02000	/* set group id on execution */
#define		ISVTX	  01000	/* save swapped text after use if on root */
#define		IREAD	   0400	/* read, write, execute permissions */
#define		IWRITE	   0200
#define		IEXEC	   0100
