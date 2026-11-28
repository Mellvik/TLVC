/*
 *		VENIX/86	Version 86/1.2
 *			Edited:	12/12/83
 *
 * The user structure.
 * One allocated per process.  Contains all per process data
 * that doesn't need to be referenced while the process is swapped.
 * The user structure contains the system stack per user and is
 * cross referenced with the proc structure for the same process.
 * Do not change the order of structure members up to u_error
 * unless mch.s is modified.
 */
struct user {
	unsigned int  u_rsav[2];/* save info when exchanging stacks */
	unsigned int  u_cs;	/* code segment */
	unsigned int  u_ds;	/* data & stack segment */
	unsigned int  u_es;	/* extra segment */
	unsigned int  u_cso;	/* code segment offset */
	unsigned int  u_dso;	/* data segment offset */
	unsigned int  u_eso;	/* extra segment offset/prototype */
	unsigned int  u_fpsaved;/* flt pt saved */
	struct {		/* 8087 context storage (see 8087 FSAVE) */
		int   u_fpcw;
		int   u_fpsw;
		int   u_fptag;
		int   u_fpip[2];
		int   u_fpop[2];
		struct {
			int u_fpman[4];
			int u_fpexp;
		} u_fpregs[8];
	} u_fps;
	char	      u_segflg;	/* flag for IO; user or kernel space */
	char	      u_error;	/* return error code */
	unsigned char u_uid;	/* effective user id */
	unsigned char u_gid;	/* effective group id */
	unsigned char u_ruid;	/* real user id */
	unsigned char u_rgid;	/* real group id */
	char	     *u_procp;	/* pointer to proc structure */
	char	     *u_base;	/* base address for IO */
	unsigned int  u_count;	/* bytes remaining for IO */
	unsigned int  u_offset[2];/* offset in file for IO */
	char	     *u_cdir;	/* pointer to inode of current directory */
	char	     *u_rdir;	/* pointer to inode of root directory */
	char	      u_dbuf[DIRSIZ];/* current pathname component */
	char	     *u_dirp;	/* current pointer to inode */
	struct	{		/* current directory entry */
		ino_t u_ino;
		char  u_name[DIRSIZ];
	} u_dent;
	char	     *u_pdir;	/* inode of parent directory of dirp */
	char	     *u_ofile[NOFILE];/* pntrs to file struct of open files */
	caddr_t	      u_qsav[2];/* label variable for quits and interrupts */
	caddr_t	      u_ssav[2];/* label variable for swapping */
	caddr_t	      u_signal[NSIG];/* disposition of signals */
	time_t	      u_utime;	/* this process user time */
	time_t	      u_stime;	/* this process system time */
	time_t	      u_cutime;	/* sum of childs' utimes */
	time_t	      u_cstime;	/* sum of childs' stimes */
	int	     *u_areg;	/* address of users saved register(s) */
	caddr_t	      u_prof[4];/* profile arguments */
	unsigned int  u_mask;	/* mask for creat & mknod */
	char	      u_intflg;	/* catch intr from sys */
	int	      u_stack[1]/* kernel stack extends from here up */
} ;

extern struct user u;

/*
 * u_error codes
 */
#define	EPERM	1
#define	ENOENT	2
#define	ESRCH	3
#define	EINTR	4
#define	EIO	5
#define	ENXIO	6
#define	E2BIG	7
#define	ENOEXEC	8
#define	EBADF	9
#define	ECHILD	10
#define	EAGAIN	11
#define	ENOMEM	12
#define	EACCES	13
#define	EFAULT	14
#define	ENOTBLK	15
#define	EBUSY	16
#define	EEXIST	17
#define	EXDEV	18
#define	ENODEV	19
#define	ENOTDIR	20
#define	EISDIR	21
#define	EINVAL	22
#define	ENFILE	23
#define	EMFILE	24
#define	ENOTTY	25
#define	ETXTBSY	26
#define	EFBIG	27
#define	ENOSPC	28
#define	ESPIPE	29
#define	EROFS	30
#define	EMLINK	31
#define	EPIPE	32

#define	EDEADLOCK	40
