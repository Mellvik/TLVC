#define NSIG 17

#define	SIGHUP	1	/* hangup    (not used by terminal driver) */
#define	SIGINT	2	/* interrupt (^C or BREAK) */
#define	SIGQUIT	3	/* quit      (^Z) */
#define	SIGILL	4	/* illegal instruction (not reset when caught) */
#define	SIGTRAP	5	/* trace trap (not reset when caught) */
#define	SIGIOT	6	/* IOT instruction */
#define	SIGEMT	7	/* EMT instruction */
#define	SIGFPE	8	/* floating point exception */
#define	SIGKILL	9	/* kill (cannot be caught or ignored) */
#define	SIGBUS	10	/* bus error */
#define	SIGSEGV	11	/* segmentation violation */
#define	SIGSYS	12	/* bad argument to system call */
#define	SIGPIPE	13	/* write on a pipe with no one to read it */
#define	SIGALRM	14	/* alarm clock */
#define	SIGTERM	15	/* software termination signal from kill */
#define	SIGAIO	16	/* Async I/O finished */

int	(*signal())();
#define	SIG_DFL	(int (*)())0
#define	SIG_IGN	(int (*)())1
