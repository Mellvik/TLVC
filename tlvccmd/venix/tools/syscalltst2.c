#include <stdio.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

unsigned maxmem();

main(argc, argv)
int argc; char **argv;
{
        char buf[1024];
        int fd[2], fd2[2], fd3[2];
        unsigned int p, pp, curend;
        int fp, f2, l, l2;
        unsigned mem, initbrk = sbrk(0);
        struct stat st;
        time_t t, tt, time();
        char *newfile = "/tmp/abcd";
        long sk, lseek();
        FILE *ff;

        printf("pipe calls returned %d/%d/%d ", 
                        pipe(fd), pipe(fd2), pipe(fd3));
        printf("(fd %d/%d, fd2 %d/%d, fd3 %d/%d)\n", fd[0], fd[1],
                        fd2[0], fd2[1], fd3[0], fd3[1]);
        fp = dup(fd[0]);
        f2 = dup2(fd[1],fd[0]);
        printf("dup %d:%d, dup2(%d,%d) returned %d\n", fp, 
                fd[0], fd[1], fd[0], f2);
        close(fp);
        close(fd[0]);
        close(fd[1]);
        tt = time(&t);
        printf("time: ret %lu, parm %lu, %s %s", tt, t, ctime(&tt), 
                                ctime(&t));
        if (argc > 1) { /* either an option or a file name */
                if (argv[1][0] == '/') {        /* it's a file ... */
                        fp = open(argv[1], O_RDWR);
                        if (fp < 0) err(argv[1]);
                        printf("open %s ok\n", argv[1]);
                        if (stat(argv[1], &st) < 0) err(argv[1]);
                        printf("%s: mode %o, size %lu, ino %d\n",
                                argv[1], st.st_mode, st.st_size, st.st_ino);
                        printf("read %d bytes from %s\n", 
                                (l=read(fp, buf, 1024)), argv[1]);
                        sk = lseek(fp, 70000L, 0);
                        printf("return from lseek to 70000: %lu\n", sk);
                        if ((f2=creat(newfile, 0664)) < 0) err(newfile);
                        printf("Created %s\n", newfile);
                        if ((l2=write(f2, buf, l)) < 0) err(newfile);
                        printf("wrote %d bytes to %s\n", l2, newfile);
                        close(f2);
                        if (stat(newfile, &st) < 0) err("stat-2");
                        printf("%s now has %ld bytes\n", newfile, st.st_size);
                        close(fp);
                } else {
                        printf("wrong option: %s\n", argv[1]);
                        exit(2);
                }
        }
        ff = fopen("/tmp/tmp/xxx", "a");
        printf("fopen /tmp/tmp/xxx returned %x\n", ff);
        if (ff > 0)
                fprintf(ff, "This is coming through the pipe!\n");
        else perror("fopen");
        fclose(ff);
        ff = popen("cat", "w");
        printf("popen returned %x\n", ff);
        if (ff <= 0) perror("Popen failed: ");
        else fprintf(ff, "This is coming through the pipe!\n");
        pclose(ff);
        ff = popen("cat /etc/motd", "r");
        printf("2nd popen returned %x\n", ff);
        if (ff <= 0) perror("2nd popen failed: ");
        else {
                char buf[30];
                printf("Pipe read: ");
                while (fgets(buf, sizeof(buf), ff) != NULL) {
                        printf("%s", buf);
                } 
        }
        pclose(ff);

        printf("initbrk: %d, curend: %d\n", initbrk, curend=sbrk(0));

        brk(curend+1024);
        printf("new curend: %d\n", curend=sbrk(0));
        mem = 2048;
        while ((pp = (unsigned)malloc(mem)) != 0) {
                printf("sbrk: 0x%x, malloc: %u, ptr 0x%x\n", sbrk(0), mem, pp);
                mem += 2048;
                free(pp);
        }
        printf("Last malloc %u/0x%x, total alloc %u (curbrk-initbrk %u)\n", pp, 
pp,
                mem, sbrk(0) - initbrk);
        pp = sbrk(0);
        brk(curend);

        pp = maxmem(pp);
        printf("heap start: %u (0x%x), heap end: %u (0x%x), size %u\n", 
                        initbrk, initbrk, pp, pp, pp-initbrk);

}

err(c) 
char *c;
{
        perror(c);
        exit(2);
}

unsigned maxmem(top)
unsigned top;
{
        unsigned cbrk = sbrk(0);
        unsigned max = 0;

#ifdef NOTDEF
        while (sbrk(1024) != -1) max += 1024;
        printf("Allocation test: Start %u, end %u, got %u\n", cbrk, sbrk(0), max
);
        brk(cbrk);
        if (brk(top)) printf("brk to top (%u) returned %u\n", top, sbrk(0));
        else printf("brk(%u) failed\n", top);
        brk(cbrk);
#endif
        printf("brk test, start: %u/0x%x\n", cbrk, cbrk);
        while (sbrk(512) != -1) {
		cbrk = sbrk(0);
                /*printf("%u;", cbrk);*/
                /* don't allow wrap around, it will crash the system  */
                if (cbrk > (cbrk + 512)) break;
        }
        printf("\n");
        return(cbrk);
}
