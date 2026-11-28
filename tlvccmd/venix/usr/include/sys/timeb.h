/*
 * Structure returned by ftime system call
 */
struct timeb {
	long	time;
	unsigned int millitm;
	int	timezone;
	int	dstflag;
};
