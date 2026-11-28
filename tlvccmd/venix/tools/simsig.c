
/*
 * simsig  -  Send and catch signal to oneself.
 *
 *
 */
#define NLOOP 10000L

long loop = NLOOP;

func() {
	signal(1, func);
}

main{} {
	register int pid = getpid();

	signal(1, func);
	while(loop--) 
		kill(pid, 1);

	display("simple kill-catch-signal", NLOOP);
}
