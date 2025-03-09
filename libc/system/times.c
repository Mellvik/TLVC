#ifdef L_times

#include <sys/times.h>
#include <linuxmt/mem.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#if 0	/* system call not implemented */

clock_t times(struct tms *tp)
{
   long rv;
   __times(tp, &rv);
   return rv;
}

#else

/* library version of 'times' since TLVC doesn't implement it */
clock_t times(struct tms *tp)
{

    unsigned long jif;
    int fd = open("/dev/kmem", O_RDONLY);

    if (fd < 0) {
	perror("/dev/kmem");
	return -1;
    }
    if (ioctl(fd, MEM_GETJIFFIES, &jif) < 0 )  {
	perror("times: ioctl error in /dev/kmem");
	return -1;
    }
    close(fd);
    tp->tms_utime = jif;
    tp->tms_stime = jif;
    tp->tms_cutime = jif;
    tp->tms_cstime = jif;
    
    return jif;
}

#endif
#endif	/* L_times*/
