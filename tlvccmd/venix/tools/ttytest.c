#include <stdio.h>
#include <sgtty.h>

#define END 0x8

extern errno;

main() {
        unsigned char c = 0;
        struct sgttyb stbuf;
        int fd;
        char buf[10];
        unsigned int std_flags, new_flags;

        fd = 0;  /* Use std io */
        /*gtty(fd, stbuf); virker */
        ioctl(fd, TIOCGETP, &stbuf);
        std_flags = stbuf.sg_flags;
        new_flags = std_flags;
        printf("sg_flags: 0%06o\n", std_flags);
        /* In RAW mode, the modes CRT, SCROLL, XTABS, CRMOD, LCASE 
         * and CBREAK are all ignored. Also, chars are 8 bits regardless.
         */
        /*new_flags &= ~(ODD|EVEN|TANDEM|LCASE|ECHO|XTABS|CRMOD);*/
        new_flags &= ~ECHO;
        new_flags |= RAW;
        stbuf.sg_flags = new_flags;

        printf("changed sg_flags: 0%06o\n", stbuf.sg_flags);
        ioctl(fd, TIOCSETP, &stbuf);
        ioctl(fd, TIOCGETP, &stbuf);
        /*stty(fd, &stbuf);
        gtty(fd, &stbuf);*/
        printf("new sg_flags: 0%06o\n", stbuf.sg_flags);

        while (c != END) {
                int st;
                /*c = getchar();*/
                /* len in the read call must be 1 or read waits */
                st = read(0, buf, 1);
                if (st > 0) {
                        c = (unsigned char)buf[0];
                        printf("0x%02x ", c);
                } else {
                        printf("\nRead error: %d\n", st);
                        break;
                }
        }
        ioctl(fd, TIOCGETP, &stbuf);
        /*gtty(1, &stbuf);*/
        stbuf.sg_flags = std_flags;
        ioctl(fd, TIOCSETP, &stbuf);
        /*stty(1, &stbuf);*/
        printf("Tty settings returned to normal.\n");
}

