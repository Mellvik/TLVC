/*
 * syscall_test.c - Comprehensive V7 syscall test suite
 * K&R C, runs on Venix/86 and TLVC with V7 shim layer.
 *
 * Build on Venix:  cc -o syscall_test syscall_test.c
 * Build on TLVC:   (after vrewrite conversion and link with V7 shim)
 *
 * PURPOSE: Run on Venix first to establish ground truth, then run the
 * converted binary on TLVC to verify the V7 shim layer is correct.
 * Results should be identical on both systems.
 *
 * Each test prints:  TEST name: PASS  or  TEST name: FAIL (reason) errno=N
 * Final summary:     Passed: N  /  Failed: M
 *
 * NOTE: struct stat, TIOCGETP, and SIGUSR1 are Venix-specific values.
 *       Verify TIOCGETP against /usr/include/sgtty.h before building.
 *       SIGUSR1=16, check /usr/include/signal.h on your Venix system.
 *
 * Tests:
 *   1  read/write          11 time/ftime
 *   2  open/close/creat    12 brk/sbrk
 *   3  lseek               13 getpid/getuid/getgid/getppid
 *   4  stat/fstat          14 umask
 *   5  link/unlink         15 access
 *   6  chdir               16 signal (SIGUSR1)
 *   7  chmod               17 fork/wait (exit status)
 *   8  chown               18 ioctl (TIOCGETP)
 *   9  dup                 19 lseek-then-read (seek to 100, read 100 bytes)
 *  10  pipe                20 sequential-read (2 reads, verify 2nd = bytes 100-
199)
 */

/* K&R C: no function prototypes, no void, no const */
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <errno.h>
#include <signal.h>
#include <sgtty.h>
extern int errno;

/* V7 tty ioctl constants */
/* TIOCGETP value: check /usr/include/sgtty.h on your system */
#ifdef NOTDEF
#define TIOCGETP  ((int)0x7408)
#define TIOCSETP  ((int)0x7409)   /* TIOCSETP - set tty params */
#define SIGKILL   9
#define SIGUSR1   16    /* may vary by system */
#define SIGALRM   14
#endif
#define SIGUSR1   16    /* may vary by system, TLVC = SIGURG */
/* ---------- forward declarations (K&R style) ---------- */
int t_read_write();
int t_open_close_creat();
int t_lseek();
int t_stat_fstat();
int t_link_unlink();
int t_chdir();
int t_chmod();
int t_chown();
int t_dup();
int t_pipe();
int t_time_ftime();
int t_brk_sbrk();
int t_getpid_getuid();
int t_umask();
int t_access();
int t_signal();
int t_fork_wait();
int t_ioctl();

/* ---------- test infrastructure ---------- */

int n_pass = 0;
int n_fail = 0;

/* Write a string to fd without using stdio */
writeln(fd, s)
int fd;
char *s;
{
    char *p;
    int len;
    for (p = s; *p; p++)
        ;
    len = p - s;
    write(fd, s, len);
    write(fd, "\n", 1);
}

/* Simple integer to decimal string */
char *
itoa(n, buf)
int n;
char *buf;
{
    char tmp[12];
    char *p = tmp + 11;
    char *b = buf;
    int neg = 0;
    *p = '\0';
    if (n < 0) { neg = 1; n = -n; }
    if (n == 0) *--p = '0';
    while (n > 0) { *--p = '0' + (n % 10); n /= 10; }
    if (neg) *--p = '-';
    while (*p) *b++ = *p++;
    *b = '\0';
    return buf;
}

pass(name)
char *name;
{
    write(1, "TEST ", 5);
    write(1, name, strlen(name));
    write(1, ": PASS\n", 8);
    n_pass++;
}

fail(name, reason)
char *name, *reason;
{
    char ebuf[8];
    write(1, "TEST ", 5);
    write(1, name, strlen(name));
    write(1, ": FAIL (", 8);
    write(1, reason, strlen(reason));
    write(1, ") errno=", 8);
    itoa(errno, ebuf);
    write(1, ebuf, strlen(ebuf));
    write(1, "\n", 1);
    n_fail++;
}

/* strcmp without libc */
xstrcmp(a, b)
char *a, *b;
{
    while (*a && *a == *b) { a++; b++; }
    return *a - *b;
}

/* ---------- 1. read / write ---------- */

int
t_read_write()
{
    char buf[32];
    int fd, n;
    int fds[2];

    /* write to stdout (fd 1) b basic smoke test */
    if (write(1, "  [read/write] ", 15) < 0) {
        fail("write-stdout", "write failed");
        return;
    }
    write(1, "\n", 1);

    /* pipe: write then read */
    if (pipe(fds) < 0) { fail("pipe-for-rw", "pipe failed"); return; }
    if (write(fds[1], "hello", 5) != 5) {
        fail("write-pipe", "short write");
        close(fds[0]); close(fds[1]); return;
    }
    n = read(fds[0], buf, 5);
    close(fds[0]); close(fds[1]);
    if (n != 5) { fail("read-pipe", "short read"); return; }
    buf[5] = '\0';
    if (xstrcmp(buf, "hello") != 0) { fail("read-pipe-data", "wrong data"); return; }
    pass("read-write");
}

/* ---------- 2. open / close / creat ---------- */

int
t_open_close_creat()
{
    int fd;
    char buf[8];

    /* creat */
    fd = creat("/tmp/sctest_oc", 0600);
    if (fd < 0) { fail("creat", "creat failed"); return; }
    if (write(fd, "abc", 3) != 3) { fail("creat-write", "write failed"); close(fd); return; }
    close(fd);

    /* open read-only */
    fd = open("/tmp/sctest_oc", 0);
    if (fd < 0) { fail("open-ro", "open failed"); return; }
    if (read(fd, buf, 3) != 3) { fail("open-ro-read", "read failed"); close(fd);
 return; }
    close(fd);
    buf[3] = '\0';
    if (xstrcmp(buf, "abc") != 0) { fail("open-ro-data", "wrong data"); return; 
}

    /* open nonexistent b ENOENT */
    fd = open("/tmp/sctest_nosuchfile_xyz", 0);
    if (fd >= 0) { fail("open-noent", "should have failed"); close(fd); return; 
}
    if (errno != 2) { fail("open-noent-errno", "wrong errno (expect 2=ENOENT)");
 return; }

    unlink("/tmp/sctest_oc");
    pass("open-close-creat");
}

/* ---------- 3. lseek ---------- */

int
t_lseek()
{
    int fd;
    char buf[4];
    long pos;

    fd = creat("/tmp/sctest_ls", 0600);
    if (fd < 0) { fail("lseek-creat", "creat failed"); return; }
    write(fd, "0123456789", 10);
    close(fd);

    fd = open("/tmp/sctest_ls", 0);
    if (fd < 0) { fail("lseek-open", "open failed"); return; }

    /* seek from start */
    pos = lseek(fd, 5L, 0);
    if (pos != 5L) { fail("lseek-set", "wrong position"); close(fd); return; }
    if (read(fd, buf, 1) != 1 || buf[0] != '5') {
        fail("lseek-set-read", "wrong data"); close(fd); return;
    }

    /* seek from current */
    pos = lseek(fd, 2L, 1);
    if (pos != 8L) { fail("lseek-cur", "wrong position"); close(fd); return; }
    if (read(fd, buf, 1) != 1 || buf[0] != '8') {
        fail("lseek-cur-read", "wrong data"); close(fd); return;
    }

    /* seek from end */
    pos = lseek(fd, -3L, 2);
    if (pos != 7L) { fail("lseek-end", "wrong position"); close(fd); return; }
    if (read(fd, buf, 1) != 1 || buf[0] != '7') {
        printf("lseek: read returned %c (0x%x)\n", buf[0], buf[0]);
        fail("lseek-end-read", "wrong data"); close(fd); return;
    }

    close(fd);
    unlink("/tmp/sctest_ls");
    pass("lseek");
}

/* ---------- 4. stat / fstat ---------- */

#ifdef NOTDEF
typedef short          dev_t;
typedef unsigned short ino_t;
typedef long          off_t;   /* V7: file size, short (files < 32KB) */
typedef long           time_t;
#endif

/* Venix/86 stat structure.
 * NOTE: This must match the kernel's struct stat exactly.
 *       Fields: dev(2) ino(2) mode(2) nlink(2) uid(2) gid(2) rdev(2) size(2) ti
mes(4*3)
 *       Total: 30 bytes on Venix/86.
 */
struct stat {
    dev_t          st_dev;      /*  0: device */
    ino_t          st_ino;      /*  2: inode */
    unsigned short st_mode;     /*  4: file mode */
    short          st_nlink;    /*  6: link count */
    short          st_uid;      /*  8: owner uid */
    short          st_gid;      /* 10: owner gid */
    dev_t          st_rdev;     /* 12: device for special files */
    off_t          st_size;     /* 14: file size in bytes */
    time_t         st_atime;    /* 16: last access time */
    time_t         st_mtime;    /* 20: last modification time */
    time_t         st_ctime;    /* 24: last status change time */
};  /* 28 bytes total on Venix/86 */

int
t_stat_fstat()
{
    struct stat sb;
    int fd;

    /* stat a known file */
    if (stat("/tmp", &sb) < 0) { fail("stat-tmp", "stat failed"); return; }
    /* /tmp should be a directory: mode bits 040xxx */
    if ((sb.st_mode & 0170000) != 0140000) {
        fail("stat-tmp-mode", "not a directory");
        return;
    }

    /* fstat on a creat'd file */
    fd = creat("/tmp/sctest_st", 0644);
    if (fd < 0) { fail("fstat-creat", "creat failed"); return; }
    write(fd, "test", 4);
    if (fstat(fd, &sb) < 0) { fail("fstat", "fstat failed"); close(fd); return; 
}
    if (sb.st_size != (off_t)4) { fail("fstat-size", "wrong size"); close(fd); return; }
    /* regular file: mode 010xxxx or 0100xxx */
    if ((sb.st_mode & 0170000) != 0100000) {
        fail("fstat-mode", "not a regular file"); close(fd); return;
    }
    close(fd);
    unlink("/tmp/sctest_st");
    pass("stat-fstat");
}

/* ---------- 5. link / unlink ---------- */

int
t_link_unlink()
{
    struct stat s1, s2;
    int fd;

    fd = creat("/tmp/sctest_ln1", 0600);
    if (fd < 0) { fail("link-creat", "creat failed"); return; }
    close(fd);

    if (link("/tmp/sctest_ln1", "/tmp/sctest_ln2") < 0) {
        fail("link", "link failed"); unlink("/tmp/sctest_ln1"); return;
    }

    stat("/tmp/sctest_ln1", &s1);
    stat("/tmp/sctest_ln2", &s2);
    if (s1.st_ino != s2.st_ino) {
        fail("link-ino", "inodes differ"); 
        unlink("/tmp/sctest_ln1"); unlink("/tmp/sctest_ln2"); return;
    }
    if (s1.st_nlink < 2) {
        fail("link-nlink", "nlink < 2");
        unlink("/tmp/sctest_ln1"); unlink("/tmp/sctest_ln2"); return;
    }

    if (unlink("/tmp/sctest_ln2") < 0) { fail("unlink", "unlink failed"); return
; }
    /* ln2 should be gone */
    if (open("/tmp/sctest_ln2", 0) >= 0) { fail("unlink-gone", "still exists"); 
return; }

    unlink("/tmp/sctest_ln1");
    pass("link-unlink");
}

/* ---------- 6. chdir ---------- */

int
t_chdir()
{
    int fd;
    if (chdir("/tmp") < 0) { fail("chdir", "chdir failed"); return; }
    /* creat relative path b should land in /tmp */
    fd = creat("sctest_cd", 0600);
    if (fd < 0) { fail("chdir-creat-rel", "relative creat failed"); return; }
    close(fd);
    unlink("/tmp/sctest_cd");
    /* restore */
    chdir("/");
    pass("chdir");
}

/* ---------- 7. chmod ---------- */

int
t_chmod()
{
    struct stat sb;
    int fd;

    fd = creat("/tmp/sctest_cm", 0600);
    if (fd < 0) { fail("chmod-creat", "creat failed"); return; }
    close(fd);

    if (chmod("/tmp/sctest_cm", 0444) < 0) { fail("chmod", "chmod failed"); return; }
    stat("/tmp/sctest_cm", &sb);
    if ((sb.st_mode & 0777) != 0444) {
        fail("chmod-verify", "mode not set"); unlink("/tmp/sctest_cm"); return;
    }
    /* restore so we can delete it */
    chmod("/tmp/sctest_cm", 0600);
    unlink("/tmp/sctest_cm");
    pass("chmod");
}

/* ---------- 8. chown (may need root) ---------- */

int
t_chown()
{
    struct stat sb;
    int fd, uid;

    fd = creat("/tmp/sctest_co", 0600);
    if (fd < 0) { fail("chown-creat", "creat failed"); return; }
    close(fd);

    stat("/tmp/sctest_co", &sb);
    uid = sb.st_uid;

    /* chown to self b always should work */
    if (chown("/tmp/sctest_co", uid, sb.st_gid) < 0) {
        fail("chown-self", "chown to self failed");
        unlink("/tmp/sctest_co"); return;
    }
    unlink("/tmp/sctest_co");
    pass("chown");
}

/* ---------- 9. dup ---------- */

int
t_dup()
{
    int fds[2], fd2;
    char buf[8];

    if (pipe(fds) < 0) { fail("dup-pipe", "pipe failed"); return; }
    fd2 = dup(fds[1]);
    if (fd2 < 0) { fail("dup", "dup failed"); close(fds[0]); close(fds[1]); return; }
    if (write(fd2, "X", 1) != 1) {
        fail("dup-write", "write on dup'd fd failed");
        close(fds[0]); close(fds[1]); close(fd2); return;
    }
    close(fd2); close(fds[1]);
    if (read(fds[0], buf, 1) != 1 || buf[0] != 'X') {
        fail("dup-read", "wrong data"); close(fds[0]); return;
    }
    close(fds[0]);
    pass("dup");
}

/* ---------- 10. pipe ---------- */

int
t_pipe()
{
    int fds[2];
    char buf[32];
    int n;

    if (pipe(fds) < 0) { fail("pipe", "pipe failed"); return; }
    write(fds[1], "pipedata", 8);
    close(fds[1]);
    n = read(fds[0], buf, 32);
    close(fds[0]);
    if (n != 8) { fail("pipe-len", "wrong read length"); return; }
    buf[8] = '\0';
    if (xstrcmp(buf, "pipedata") != 0) { fail("pipe-data", "wrong data"); return
; }
    pass("pipe");
}

/* ---------- 11. time / ftime ---------- */

struct timeb {
    long   time;
    short  millitm;
    short  timezone;
    short  dstflag;
};
time_t time();
int
t_time_ftime()
{
    time_t t1, t2;
    struct timeb tb;

    t1 = time(0);
    if (t1 <= 0L) { fail("time", "time returned 0 or negative"); return; }

    /* time should return a sensible value (after 1980-01-01 = 315532800) */
    if (t1 < 315532800L) { printf("got time %lu\n", t1); fail("time-sanity", "time before 1980"); return; }

    /* ftime */
    if (ftime(&tb) < 0) { fail("ftime", "ftime failed"); return; }
    if (tb.time <= 0L) { fail("ftime-time", "ftime returned bad time"); return; 
}
    if (tb.millitm < 0 || tb.millitm >= 1000) {
        fail("ftime-millitm", "millitm out of range"); return;
    }

    /* Two successive time() calls: t2 >= t1 */
    t2 = time((long *)0);
    if (t2 < t1) { fail("time-monotone", "time went backwards"); return; }

    pass("time-ftime");
}

/* ---------- 12. brk / sbrk ---------- */

int
t_brk_sbrk()
{
    char *p1, *p2, *p3;
    int i;

    /* sbrk(0) returns current break */
    p1 = (char *)sbrk(0);
    if (p1 == (char *)-1) { fail("sbrk-0", "sbrk(0) failed"); return; }

    /* allocate 256 bytes */
    p2 = (char *)sbrk(256);
    if (p2 == (char *)-1) { fail("sbrk-alloc", "sbrk(256) failed"); return; }
    if (p2 != p1) { fail("sbrk-addr", "sbrk didn't return old break"); return; }

    /* write and verify */
    for (i = 0; i < 256; i++) p2[i] = (char)(i & 0xff);
    for (i = 0; i < 256; i++) {
        if (p2[i] != (char)(i & 0xff)) {
            fail("sbrk-memory", "memory verify failed"); return;
        }
    }

    /* current break is p1+256 */
    p3 = (char *)sbrk(0);
    if (p3 != p1 + 256) { fail("sbrk-break", "break not advanced"); return; }

    /* shrink back */
    if (brk(p1) < 0) { fail("brk-shrink", "brk failed"); return; }
    p3 = (char *)sbrk(0);
    if (p3 != p1) { fail("brk-shrink-verify", "break not restored"); return; }

    pass("brk-sbrk");
}

/* ---------- 13. getpid / getuid ---------- */

int
t_getpid_getuid()
{
    int pid, uid;

    pid = getpid();
    if (pid <= 0) { fail("getpid", "getpid returned <= 0"); return; }

    uid = getuid();
    if (uid < 0) { fail("getuid", "getuid returned < 0"); return; }

    /* getgid */
    if (getgid() < 0) { fail("getgid", "getgid returned < 0"); return; }

#ifdef NOTDEF
    /* getppid does not exist on V7 */
    if (getppid() <= 0) { fail("getppid", "getppid returned <= 0"); return; }
#endif

    pass("getpid-getuid");
}

/* ---------- 14. umask ---------- */

int
t_umask()
{
    struct stat sb;
    int fd;
    int old;

    old = umask(022);
    fd = creat("/tmp/sctest_um", 0777);
    if (fd < 0) { fail("umask-creat", "creat failed"); umask(old); return; }
    close(fd);
    stat("/tmp/sctest_um", &sb);
    if ((sb.st_mode & 0777) != 0755) {
        fail("umask-mode", "umask not applied correctly");
        unlink("/tmp/sctest_um"); umask(old); return;
    }
    unlink("/tmp/sctest_um");
    umask(old);
    pass("umask");
}

/* ---------- 15. access ---------- */

int
t_access()
{
    int fd;

    fd = creat("/tmp/sctest_ac", 0644);
    if (fd < 0) { fail("access-creat", "creat failed"); return; }
    close(fd);

    /* F_OK = 0: exists */
    if (access("/tmp/sctest_ac", 0) < 0) { fail("access-exists", "F_OK failed"); return; }

    /* R_OK = 4 */
    if (access("/tmp/sctest_ac", 4) < 0) { fail("access-read", "R_OK failed"); return; }

    /* nonexistent */
    if (access("/tmp/sctest_nosuch_xyz", 0) >= 0) {
        fail("access-noent", "should fail for nonexistent"); return;
    }

    unlink("/tmp/sctest_ac");
    pass("access");
}

/* ---------- 16. signal ---------- */

int sig_caught = 0;

catch_sig(n)
int n;
{
    sig_caught = n;
    printf("Signal %d caught\n", n);
    signal(SIGALRM, catch_sig);

}

int
t_signal()
{
    int old, i;

    sig_caught = 0;
    old = signal(SIGALRM, catch_sig);
    if (old == -1) { fail("signal", "signal() failed"); return; }
#ifdef NOTDEF
    printf("waiting for signal\n");
    read(0,&i,2);
    printf("Read terminated, read %x\n", i);
#endif

    /* send signal to self */
    if (kill(getpid(), SIGALRM) < 0) { fail("signal-kill", "kill failed"); return; }
    /* give it a moment to arrive */
    if (sig_caught != SIGALRM) { 
	printf("signal: got wrong signal %d\n", sig_caught);
	fail("signal-caught", "signal not caught"); 
	return;
    }

    if ((i = signal(SIGALRM, old)) != catch_sig)
	printf("signal: wrong return. Got %x, expected %x\n", i, catch_sig);
    pass("signal");
}

/* ---------- 17. fork / wait ---------- */

int
t_fork_wait()
{
    int pid, status;
    int fds[2];
    char buf[4];

    if (pipe(fds) < 0) { fail("fork-pipe", "pipe failed"); return; }
    pid = fork();
    if (pid < 0) { fail("fork", "fork failed"); close(fds[0]); close(fds[1]); return; }

    if (pid == 0) {
        /* child: write a byte and exit with status 42 */
        close(fds[0]);
        write(fds[1], "C", 1);
        close(fds[1]);
        exit(42);
    }

    /* parent */
    close(fds[1]);
    if (read(fds[0], buf, 1) != 1 || buf[0] != 'C') {
        fail("fork-pipe-read", "didn't get child byte");
        close(fds[0]); return;
    }
    close(fds[0]);

    if (wait(&status) != pid) { fail("wait-pid", "wrong pid from wait"); return;
 }
    /* V7: exit status in high byte */
    if (((status >> 8) & 0xff) != 42) {
        printf("exit status %x\n", status);
        fail("wait-status", "wrong exit status"); return;
    }
    pass("fork-wait");
}

/* ---------- 18. ioctl (basic) ---------- */

#ifdef NOTDEF
struct sgttyb {
    char sg_ispeed;
    char sg_ospeed;
    char sg_erase;
    char sg_kill;
    short sg_flags;
};
#endif

int
t_ioctl()
{
    struct sgttyb tty;
    int fd;

    /* Try TIOCGETP b succeeds on tty, fails (ENOTTY=25) on non-tty */
    if (ioctl(0, TIOCGETP, &tty) < 0) {
        if (errno == 25) {
            pass("ioctl-notty");    /* correctly returned ENOTTY */
        } else {
            fail("ioctl", "unexpected errno");
        }
    } else {
        pass("ioctl");
    }
}


/* ---------- 19/20. lseek vs sequential read verification ---------- */

#define TESTFILE  "/tmp/sctest_seek"
#define FILESIZE  1024     /* bytes b content: byte[i] = i & 0xff */
#define SEEK_POS  100      /* seek to this position */
#define READ_LEN  100      /* read this many bytes */

/* Create the reference file with known content */
int
make_seek_file()
{
    int fd, i;
    char buf[256];

    /* Fill buffer: buf[i] = i & 0xff */
    for (i = 0; i < 256; i++)
        buf[i] = (char)(i & 0xff);

    fd = creat(TESTFILE, 0600);
    if (fd < 0) return -1;

    /* Write 256 bytes * 4 = 1024 bytes */
    for (i = 0; i < 4; i++) {
        if (write(fd, buf, 256) != 256) {
            close(fd);
            unlink(TESTFILE);
            return -1;
        }
    }
    close(fd);
    return 0;
}

/* Verify that buf[0..len-1] == file content at file_offset */
int
verify_bytes(buf, len, file_offset)
char *buf;
int len, file_offset;
{
    int i;
    for (i = 0; i < len; i++) {
        if (buf[i] != (char)((file_offset + i) & 0xff))
            return i;   /* return index of first mismatch */
    }
    return -1;  /* -1 = all match */
}

/*
 * Test 19: lseek to position 100, read 100 bytes,
 *          verify they are bytes 100..199 from the file.
 */
int
t_lsr()
{
    int fd;
    long pos;
    char buf[READ_LEN];
    int bad;

    if (make_seek_file() < 0) {
        fail("lseek-read-setup", "could not create test file");
        return;
    }

    fd = open(TESTFILE, 0);
    if (fd < 0) { fail("lseek-read-open", "open failed"); return; }

    pos = lseek(fd, (long)SEEK_POS, 0);
    if (pos != (long)SEEK_POS) {
        fail("lseek-read-seek", "lseek returned wrong position");
        close(fd); return;
    }

    if (read(fd, buf, READ_LEN) != READ_LEN) {
        fail("lseek-read-read", "short read after lseek");
        close(fd); return;
    }

    bad = verify_bytes(buf, READ_LEN, SEEK_POS);
    if (bad >= 0) {
        fail("lseek-read-data", "data mismatch after lseek");
        close(fd); return;
    }

    close(fd);
    pass("lseek-then-read");
}

/*
 * Test 20: sequential reads b read 100 bytes (discard),
 *          read next 100 bytes, verify they are bytes 100..199.
 *          This confirms the file offset advances correctly.
 */
int
t_sequential_read()
{
    int fd;
    char discard[READ_LEN];
    char buf[READ_LEN];
    int bad;

    /* File already created by test_lseek_read */
    fd = open(TESTFILE, 0);
    if (fd < 0) { fail("seq-read-open", "open failed"); return; }

    /* First read: consume bytes 0..99 */
    if (read(fd, discard, READ_LEN) != READ_LEN) {
        fail("seq-read-first", "first read failed or short");
        close(fd); return;
    }

    /* Second read: should give bytes 100..199 */
    if (read(fd, buf, READ_LEN) != READ_LEN) {
        fail("seq-read-second", "second read failed or short");
        close(fd); return;
    }

    bad = verify_bytes(buf, READ_LEN, SEEK_POS);
    if (bad >= 0) {
        fail("seq-read-data", "data mismatch on second sequential read");
        close(fd); return;
    }

    close(fd);
    unlink(TESTFILE);
    pass("sequential-read");
}

/* ---------- main ---------- */

main()
{
    char nbuf[8];

    writeln(1, "=== V7 Syscall Test Suite ===");
    writeln(1, "");

    t_read_write();
    t_open_close_creat();
    t_lseek();
    t_stat_fstat();
    t_link_unlink();
    t_chdir();
    t_chmod();
    t_chown();
    t_dup();
    t_pipe();
    t_time_ftime();
    t_brk_sbrk();
    t_getpid_getuid();
    t_umask();
    t_access();
    t_signal();
    t_fork_wait();
    t_ioctl();
    t_lsr();
    t_sequential_read();

    writeln(1, "");
    writeln(1, "=== Results ===");
    write(1, "Passed: ", 8); itoa(n_pass, nbuf); writeln(1, nbuf);
    write(1, "Failed: ", 8); itoa(n_fail, nbuf); writeln(1, nbuf);

    exit(n_fail == 0 ? 0 : 1);
}
