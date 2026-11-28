#include <signal.h>
#undef NSIG                     /* to prevent redefines in <param.h> */
#include <sgtty.h>
#include <a.out.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/filsys.h>
#include <sys/devparm.h>
#include <sys/xtblk0.h>

int serno;
int nusr;
struct exec header;
struct  sgttyb sgbuf;

char    clear[] = "\033H\033J\033\016"; /* clear and reset screen */
char    hlon[]  = "\033\006";           /* highlight */
char    hloff[] = "\033\005";   
char    rvon[]  = "\033\010";           /* reverse video */
char    rvoff[] = "\033\007";

#define DEL             ('H' & 037)     /* delete key */
#define NUSR_BOOT       508L    /* loc. of #users variable in floppy boot */
#define SN_BOOT         510L    /* loc. of serial no. in floppy boot */
#define NUSR_INIT       4L      /* offset to #users variabl in /etc/init */
#define SN_INIT         (508L + 512L)
#define pf printf

fatal(s)
char *s;
{
        printf("%s\n", s);
        exit(1);
}

main(argc, argv)
int argc;
char **argv;
{
        getserial();
        printf("Floppy: Serial %d, nusers %d\n", serno, nusr);
        putserial(1);
        putnusers(1);
        printf("Disk: serial %d, users %d\n", serno, nusr);
}
/*
 * Insert serial number
 */     
putserial(rw)
int rw;
{       
int     fd;

        if ((fd = open("/dev/w0.sys",2)) < 0)
                fatal("Cannot open /dev/w0.sys\n");
        lseek(fd, SN_INIT, 0);
        if (rw)
                read(fd, &serno, sizeof(serno));
        else
                write(fd, &serno, sizeof(serno));
        close(fd);
        return (0);
}

/*
 * find out number of users and system serial number,
 * stored in floppy boot block
 */
getserial()
{
int     fd;

        if ((fd = open("/dev/f0",0)) < 0)
                fatal("Can't open /dev/f0\n");
        lseek(fd, NUSR_BOOT, 0);
        read(fd, &nusr, sizeof(nusr));
        lseek(fd, SN_BOOT, 0);
        read(fd, &serno, sizeof(serno));
        close(fd);
        return (0);
}


/*
 * patch /etc/init with # of users
 */
putnusers(rw)
int rw;
{
int     i, fd;

        if (!rw) {
                pf("\nNumber of users this system will have (1 to 16) [%d] : ",n
usr);
                nusr = getnum(1,16,nusr);
        }
        if ((fd = open("/etc/init",2)) < 0)
                fatal("Cannot open /etc/init\n");
        /* NUSR_INIT tells us positition of
         * symbol determining # of users,
         * offset from end of code area
         */
        read(fd, &header, sizeof(struct exec));
        lseek(fd, header.a_text+NUSR_INIT+sizeof(struct exec), 0);
        read(fd, &i, sizeof(i));
        if ((i < 1) || (i > 16)) /* check we're in right spot */
                nusr = 0; 
        else {       
                lseek(fd, header.a_text+NUSR_INIT+sizeof(header), 0);
                if (rw) {
                        nusr = i;
                        return;
                }
                write(fd, &nusr, sizeof(nusr));
        }       
        close(fd);
        return (0);
}

getnum(min,max,def)
int     min, max, def;  
{
register char *cp;      
char     c, cbuf[10];   
int      r, flags;      

        if (min > max)
                min = max;
        if (def > max)
                def = max;
        else
                if (def < min)
                        def = min;  
        ioctl(0,TIOCGETP,&sgbuf);
        flags = sgbuf.sg_flags;
        sgbuf.sg_flags = RAW;   
        ioctl(0,TIOCSETP,&sgbuf);
        while (1) 
        {
                pf("%s",rvon);  
                for (cp = cbuf; cp < cbuf + 10;)
                {
                        read(0,&c,1);
                        if      (isdigit(c) ||
                                 (
                                  (cp == cbuf) &&       /* first digit? */
                                  (
                                   (c == '+' && max >= 0) ||    /* '+' okay? */
                                   (c == '-' && min < 0)        /* '-' okay? */
                                  )
                                 )
                                )
                        {
                                *cp++ = c;
                                pf("%c",c);
                        }
                        else
                        {
                                if ((c == '\r' || c == '\n') && (cp > cbuf))
                                {
                                        pf("\r\n");
                                        break;
                                }
                                else
                                {
                                        if (c == DEL)
                                        {
                                                if (cp > cbuf)
                                                {
                                                        pf("%s\b \b%s",
                                                                rvoff,rvon);
                                                        --cp;
                                                }
                                                else
                                                {
                                                        pf("\007");
                                                }
                                        }
                                        else
                                        {
                                                /* use the default */
                                                if (    (c == '\r' ||
                                                        c == '\n') &&
                                                        (cp == cbuf)
                                                   )
                                                {
                                                        pf("%d\r\n",(r = def));
                                                        goto numout;
                                                }
                                                else
                                                {
                                                        pf("\007"); /* beep */
                                                }
                                        }
                                }
                        }
                }
                *cp = 0;
                r = atoi(cbuf);
numout:
                pf("%s",rvoff);
                /*
                 * check number bewteen bounds,
                 * allowing also for unsigned values
                 */
                if (min <= r && r <= max)
                {
                        sgbuf.sg_flags = flags;
                        ioctl(0,TIOCSETP,&sgbuf);
                        return(r);
                }
                pf("\r\n%sInvalid number%s: please type again: ",
                        hlon,hloff);
        }
}
