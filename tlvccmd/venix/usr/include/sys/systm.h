/*
 * Random set of variables.
 */
struct inode *rootdir;		/* pointer to inode of root directory */
char	execnt;			/* number of processes in exec */
char	rawbusy;		/* waiting for a raw i/o buffer */
int	lbolt;			/* time of day in HZ */
long	time;			/* time in sec from 1970 */
int	semaphore;		/* globol semaphores (16) */

/*
 * The callout structure is for a routine arranging to
 * be called by the clock interrupt (clock.c) with a
 * specified argument, in a specified amount of time.
 */
struct	callo {
	int	c_time;		/* incremental time */
	int	c_arg;		/* argument to routine */
	int	(*c_func)();	/* routine */
} callout[];

/*
 * Mount structure.
 * One allocated on every mount.
 * Used to find the super block.
 */
struct	mount {
	int	m_dev;		/* device mounted */
	int	*m_bufp;	/* pointer to superblock */
	int	*m_inodp;	/* pointer to mounted on inode */
} mount[];

int	mpid;			/* generic for unique process id's */
char	runin;			/* scheduling flag */
char	runout;			/* scheduling flag */
char	runrun;			/* scheduling flag */
char	curpri;			/* more scheduling */
int	maxmem;			/* actual max memory per process */
int	updlock;		/* lock for sync */
int	rablock;		/* block to be read ahead */
char	regloc[];		/* locs. of saved user registers (trap.c) */
