/*
 *		VENIX/86	Version 86/1.2
 *			Edited: 12/12/83
 *
 * The proc structures.
 * One structure allocated per active process.  It
 * contains all data needed about the process while
 * the process may be swapped out.  Other per process
 * data (user.h) is swapped with the process.
 */
struct	proc {
	unsigned int  p_flag;
	unsigned char p_stat;
	char	      p_pri;	/* priority, negative is high */
	unsigned char p_uid;	/* user id */
	unsigned char p_time;	/* resident time for scheduling */
	unsigned char p_cpu;	/* cpu usage for scheduling */
	char	      p_nice;	/* nice for scheduling */
	int	      p_pid;	/* unique process id */
	int	      p_ppid;	/* process id of parent */
	daddr_t	      p_daddr;	/* address of swappable data (core or disk) */
	unsigned int  p_dsize;	/* size of memory resident data */
	unsigned int  p_tod;	/* size of swappable data (top of data) */
	unsigned int  p_ssize;	/* size of swappable stack (0==stack below) */
	char	     *p_textp;	/* pointer to code structure */
	char	     *p_datap;	/* pointer to data structure */
	unsigned int  p_wchan;	/* event process is awaiting */
	char	     *p_ttyp;	/* control tty, for directing tty signals */
	unsigned long p_sig;	/* signals to this process */
	unsigned int  p_alarm;	/* time until alarm clock signal */
} proc[];

/*
 * Above structure when used for a ZOMB'ed process.
 */
struct zproc {
	unsigned int  p_flag;
	unsigned char p_stat;
	char	      p_pri;
	unsigned char p_uid;
	unsigned char p_time;
	unsigned char p_cpu;
	char	      p_nice;
	int	      p_pid;
	int	      p_ppid;
	daddr_t	      p_daddr;
	unsigned int  p_dsize;
	long	      z_cutime;	/* hold child usr time during ZOMB state */
	long	      z_cstime;	/* hold child sys time during ZOMB state */
	unsigned int  p_wchan;
	char	     *p_ttyp;
	int	      z_exit;	/* hold exit status during ZOMB state */
	int	      z_junk;
	unsigned int  p_alarm;
};

	/* Stat codes */
#define	SSLEEP	1		/* sleeping on high priority */
#define	SWAIT	2		/* sleeping on low priority */
#define	SRUN	3		/* running */
#define	SIDL	4		/* intermediate state in process creation */
#define	SZOMB	5		/* intermediate state in process termination */
#define	SSTOP	6		/* process being traced */
#define	SSUSP	0100		/* process suspended (or'd in) */

	/* Flag codes */
#define	SLOAD	040000		/* in core */
#define	SSYS	020000		/* scheduling process */
#define	SSWAP	010000		/* process is being swapped out */
#define	STRC	004000		/* process is being traced */
#define	SWTED	002000		/* another tracing flag */
#define	SWAIO	001000		/* waiting for async i/o */
#define	SFAST	000400		/* process needs fast response (realtime) */
#define SSLOCK	000100		/* lock by system call */
#define	SLOCK	000177		/* locked for raw i/o */
