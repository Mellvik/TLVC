#include <stdio.h>

void memset(buffer, val, len)
int *buffer; int val; int len;
{
        if (len) {
          int *d = buffer;

                do {
                        *d++ = val;
                } while (--len);
        }
     return;
}

main(argc, argv) 
int argc; char **argv;
{
        int blocks=15*80*2;
        int cnt = 0;
        int fd = 1; /* stdout */
        int buf[512/sizeof(int)];

        printf("Sizeof buf: %d\n", sizeof(buf));
        while (cnt < blocks) {
                memset(buf, cnt, sizeof(buf));
                if (write(fd, buf, sizeof(buf)) != sizeof(buf)) {
                        printf("write error\n");
                        exit(1);
                }
                cnt++;
        }
        close(fd);
}

