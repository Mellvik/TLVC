#include <linuxmt/config.h>
#include <linuxmt/init.h>
#include <linuxmt/mm.h>
#include <linuxmt/sched.h>
#include <linuxmt/types.h>
#include <linuxmt/fcntl.h>
#include <linuxmt/ntty.h>
#include <linuxmt/memory.h>
#include <linuxmt/kernel.h>
#include <linuxmt/string.h>
#include <linuxmt/fs.h>
#include <linuxmt/utsname.h>
#include <linuxmt/netstat.h>
#include <linuxmt/trace.h>
#include <linuxmt/debug.h>
#include <linuxmt/devnum.h>
#include <linuxmt/heap.h>
#include <linuxmt/prectimer.h>
#include <arch/system.h>
#include <arch/segment.h>
#include <arch/ports.h>
#include <arch/io.h>

/*
 *	System variable setups
 */
#define ENV		1	/* allow environ variables as bootopts */
#define DEBUG		0	/* display parsing at boot. NOTE: Most debug
				 * messages will go to the physical console */

#define MAX_INIT_ARGS	6	/* max # arguments to /bin/init or init= program */
#define MAX_INIT_ENVS	12	/* max # environ variables passed to /bin/init */
#define MAX_INIT_SLEN	80	/* max # words of args + environ passed to /bin/init */

#define	__STRING(x)	#x	/* TODO: move these to header file */
#define STR(x)          __STRING(x)

/* bootopts error message are duplicated below so static here for space savings */
char errmsg_initargs[] = "bootopts: init args > " STR(MAX_INIT_ARGS);
char errmsg_initenvs[] = "bootopts: env vars > " STR(MAX_INIT_ENVS);
char errmsg_initslen[] = "bootopts: init args+env > " STR(MAX_INIT_SLEN) " words";

#if defined(CONFIG_FS_RO) || defined(CONFIG_ROOT_READONLY)
int root_mountflags = MS_RDONLY;
#else
int root_mountflags = 0;
#endif

struct netif_parms netif_parms[MAX_ETHS] = {
    /* NOTE:  The order must match the defines in netstat.h */
    { NE2K_IRQ, NE2K_PORT, 0, NE2K_FLAGS },
    { WD_IRQ, WD_PORT, WD_RAM, WD_FLAGS },
    { EL3_IRQ, EL3_PORT, 0, EL3_FLAGS },
    { EE16_IRQ, EE16_PORT, EE16_RAM, EE16_FLAGS },
    { LANCE_IRQ, LANCE_PORT, 0, LANCE_FLAGS },
};
seg_t kernel_cs, kernel_ds;
int tracing;
int nr_map_bufs, nr_ext_bufs, nr_xms_bufs;

#ifdef CONFIG_CALIBRATE_DELAY
void calibrate_delay(void);
#endif

//#define BOOT_TIMER	/* display jiffies at system startup - for benchmarking */

/* this needs fixing, hdparms[] should be the size of MAX_ATA_DRIVES */
/* directhd.c depends on this size when initializing drive geometry */
#define CHS_DRIVES 2	/* # of drives to config in bootopts */
#define CHS_ARR_SIZE	CHS_DRIVES * 4
int hdparms[CHS_ARR_SIZE];	/* cover 2 drives */

int netbufs[2] = {-1,-1};	/* # of network buffers to allocate by the driver */
int xt_floppy[2];		/* XT floppy types, needed if XT has 720k drive(s) */
int xtideparms[6];		/* config data for xtide controller if present */
int fdcache = -1;		/* floppy sector cache size(KB), -1: not configured */
static int boot_console;
static char bininit[] = "/bin/init";
static char binshell[] = "/bin/sh";
#ifdef CONFIG_SYS_NO_BININIT
static char *init_command = binshell;
#else
static char *init_command = bininit;
#endif

#ifdef CONFIG_BOOTOPTS
/*
 * Parse /bootopts startup options
 */
static char hasopts;
static int args = 2;	/* room for argc and av[0] */
static int envs;
static int argv_slen;
/* argv_init doubles as sptr data for sys_execv later */
#ifdef CONFIG_SYS_NO_BININIT
static char *argv_init[MAX_INIT_SLEN] = { NULL, binshell, NULL };
#else
static char *argv_init[MAX_INIT_SLEN] = { NULL, bininit, NULL };
#endif
#if ENV
static char *envp_init[MAX_INIT_ENVS];
#endif
static unsigned char options[OPTSEGSZ];

extern int boot_rootdev;
extern int dprintk_on;
static char * INITPROC root_dev_name(int dev);
static int INITPROC parse_options(void);
static void INITPROC finalize_options(void);
static char * INITPROC option(char *s);

#endif

static void INITPROC kernel_init(void);
static void INITPROC kernel_banner(seg_t start, seg_t end, seg_t init, seg_t extra);
static void INITPROC early_kernel_init(void);
static void init_task(void);

/* this procedure is called using temp stack then switched, no local vars allowed */
void start_kernel(void)
{
    //printk("START\n");
    early_kernel_init();        /* read bootopts using kernel interrupt stack */
    task = heap_alloc(max_tasks * sizeof(struct task_struct),
        HEAP_TAG_TASK|HEAP_TAG_CLEAR);
    if (!task) panic("No task mem");

    sched_init();               /* set us (the current stack) to be idle task #0*/
    setsp(&task->t_regs.ax);    /* change to idle task stack */
    kernel_init();              /* continue init running on idle task stack */

    /* fork and run procedure init_task() as task #1 */
    kfork_proc(init_task);
    wake_up_process(&task[1]);

    /*
     * We are now the idle task. We won't run unless no other process can run.
     * The idle task always runs with _gint_count == 1 (switched from user mode syscall)
     */
    while (1) {
        schedule();
#ifdef CONFIG_TIMER_INT0F
        int0F();        /* simulate timer interrupt hooked on IRQ 7 */
#else
        idle_halt();    /* halt until interrupt to save power */
#endif
    }
}

static void INITPROC early_kernel_init(void)
{
    unsigned int endbss;

    /* Note: no memory allocation available until after heap_init */
    tty_init();                     /* parse_options may call rs_setbaud */
#ifdef CONFIG_TIME_TZ
    tz_init(CONFIG_TIME_TZ);        /* parse_options may call tz_init */
#endif
    ROOT_DEV = SETUP_ROOT_DEV;      /* default root device from boot loader */

#ifdef CONFIG_BOOTOPTS
    hasopts = parse_options();
#endif

    /* create near heap at end of kernel bss */
    heap_init();                    /* init near memory allocator */
    endbss = setup_arch();          /* sets membase and memend globals, cpu type */
    heap_add((void *)endbss, heapsize);
    mm_init(membase, memend);       /* init far/main memory allocator */

    if (fdcache < 0 ||			/* not set in bootopts */
	fdcache > CONFIG_FLOPPY_CACHE)	/* or too big */
	fdcache = CONFIG_FLOPPY_CACHE; 	/* then default to CONFIG */

    if (arch_cpu == 7)
	fdcache = 0;			/* disable fdcache for 386+ */

/* Add UMB support here */

/*** Experimental: Test the upper 1k of memory for BIOS modifications ****/
#if 0
    byte_t __far *upper = _MK_FP(0x9fc0, 0); 
    int i = 0;
    while (i++ < 1024) upper[i] = i;
#endif

}

void INITPROC kernel_init(void)
{
    irq_init();		/* Install timer and DIV fault handlers */

    /* set console from /bootopts console= or 0=default */
    set_console(boot_console);
    console_init();	/* init direct, bios or headless console */

#ifdef CONFIG_CHAR_DEV_RS
    serial_init();
#endif

    inode_init();
    if (buffer_init())	/* also enables xms and unreal mode if configured and possible */
	panic("No buf mem");

#ifdef CONFIG_ARCH_IBMPC
    outw(0, 0x510);
    if (inb(0x511) == 'Q' && inb(0x511) == 'E')
        running_qemu = 1;
#endif

#ifdef CONFIG_SOCKET
    sock_init();
#endif

    device_init();

#ifdef CONFIG_BOOTOPTS
    finalize_options();
    if (!hasopts) printk("/bootopts not found or bad format/size\n");
#endif

#ifdef CONFIG_FARTEXT_KERNEL
    /* add .farinit.init section to main memory free list */
    seg_t     init_seg = ((unsigned long)(void __far *)__start_fartext_init) >> 16;
    seg_t s = init_seg + (((word_t)(void *)__start_fartext_init + 15) >> 4);
    seg_t e = init_seg + (((word_t)(void *)  __end_fartext_init + 15) >> 4);
    debug("init: seg %04x to %04x size %04x (%d)\n", s, e, (e - s) << 4, (e - s) << 4);
    seg_add(s, e);
#else
    seg_t s = 0, e = 0;
#endif

    kernel_banner(membase, memend, s, e - s);
#ifdef CONFIG_CALIBRATE_DELAY
    calibrate_delay();
#endif
}

static void INITPROC kernel_banner(seg_t start, seg_t end, seg_t init, seg_t extra)
{
#ifdef CONFIG_ARCH_IBMPC
    printk("PC/%cT class, cpu %d, ", (sys_caps & CAP_PC_AT) ? 'A' : 'X',
					       (int) arch_cpu);
#endif

#ifdef CONFIG_ARCH_PC98
    printk("PC-9801 machine, ");
#endif

#ifdef CONFIG_ARCH_8018X
    printk("8018X machine, ");
#endif

    printk("syscaps 0x%x, %uK base ram, %d tasks, %d files, %d inodes\n",
	    sys_caps, SETUP_MEM_KBYTES, max_tasks, nr_files, nr_inodes);
    printk("TLVC %s (%u text, %u ftext, %u data, %u bss, %u heap)\n",
           system_utsname.release,
           (unsigned)_endtext, (unsigned)_endftext, (unsigned)_enddata,
           (unsigned)_endbss - (unsigned)_enddata, heapsize);
    printk("Kernel text %x:0, ", kernel_cs);
#ifdef CONFIG_FARTEXT_KERNEL
    printk("ftext %x:0, init %x:0, ", (unsigned)((long)kernel_init >> 16), init);
#endif
    printk("data %x:0, top %x:0, %uK free\n",
           kernel_ds, end, (int) ((end - start + extra) >> 6));
}

static void INITPROC try_exec_process(const char *path)
{
    int num;

    num = run_init_process(path);
    if (num) printk("Can't run %s, errno %d\n", path, num);
}

static void INITPROC do_init_task(void)
{
    int num;
    const char *s;

    mount_root();

#ifdef BOOT_TIMER	/* temporary, works with similar printout in getty */
    printk("[%lu]", jiffies); 	/* for measuring startup time */
#endif
#ifdef CONFIG_SYS_NO_BININIT
    /* when no /bin/init, force initial process group on console to make signals work */
    current->session = current->pgrp = 1;
#endif

    /* Don't open /dev/console for /bin/init, 0-2 closed immediately and fragments heap */
    //if (strcmp(init_command, bininit) != 0) {
	/* Set stdin/stdout/stderr to /dev/console if not running /bin/init */
	num = sys_open(s="/dev/console", O_RDWR, 0);
	if (num < 0)
	    printk("Unable to open %s (error %d)\n", s, num);
	sys_dup(num);		/* open stdout*/
	sys_dup(num);		/* open stderr*/
    //}

    /* release the setup data segment and unused parts of the FDcache */
    if (!fdcache)
	seg_add(REL_INITSEG, XD_BOUNCESEG);
    else if (fdcache < CONFIG_FLOPPY_CACHE)
	seg_add(REL_INITSEG + (fdcache<<6), XD_BOUNCESEG);

#ifdef CONFIG_BOOTOPTS
    /* Release /bootopts parsing buffers */
    heap_add(options, OPTSEGSZ);

    /* pass argc/argv/env array to init_command */

    /* unset special sys_wait4() processing if pid 1 not /bin/init */
    if (strcmp(init_command, bininit) != 0)
        current->ppid = 1;      /* turns off auto-child reaping */

    /* run /bin/init or init= command, normally no return */
    run_init_process_sptr(init_command, (char *)argv_init, argv_slen);
#else
    try_exec_process(init_command);
#endif /* CONFIG_BOOTOPTS */

    printk("No init - running %s\n", binshell);
    current->ppid = 1;			/* turns off auto-child reaping */
    try_exec_process(binshell);
    try_exec_process("/bin/sash");
    panic("No init or sh found");
}

/* this procedure runs in user mode as task 1*/
static void init_task(void)
{
    do_init_task();
}


#ifdef CONFIG_BOOTOPTS
static struct dev_name_struct {
	const char *name;
	int num;
} devices[] = {
	/* the 4 primary partitionable drives must be first */
#ifdef CONFIG_BLK_DEV_HD
	{ "dhda",    DEV_DHDA},
	{ "dhdb",    DEV_DHDB },
	{ "dhdc",    DEV_DHDC },
	{ "dhdd",    DEV_DHDD },
#elif CONFIG_BLK_DEV_XD
	{ "xda",     DEV_XDA},
	{ "xdb",     DEV_XDB },
	{ "hda",     DEV_HDA },
	{ "hdb",     DEV_HDB },
#else
	{ "hda",     DEV_HDA },
	{ "hdb",     DEV_HDB },
	{ "hdc",     DEV_HDC },
	{ "hdd",     DEV_HDD },
#endif
#ifdef CONFIG_BLK_DEV_FD
	{ "df0",     DEV_DF0 },
	{ "df1",     DEV_DF1 },
#else
	{ "fd0",     DEV_FD0 },
	{ "fd1",     DEV_FD1 },
#endif
	{ "ttyS0",   DEV_TTYS0 },
	{ "ttyS1",   DEV_TTYS1 },
	{ "tty1",    DEV_TTY1 },
	{ "tty2",    DEV_TTY2 },
	{ "tty3",    DEV_TTY3 },
	{ NULL,           0 }
};

/*
 * Convert a root device number to name.
 * Device number could be bios device, not kdev_t.
 * TODO: Expand the number of devices available as root devices.
 * Since the introduction of the 'root=' directive in bootopts, making any
 * device the root device has been easy and at times attractive.
 * The cost of expansion is the size of the table above.
 */
static char * INITPROC root_dev_name(int dev)
{
	int i, offs;
#define NAMEOFF	13
	static char name[18] = "ROOTDEV=/dev/";

#if DEBUG
	printk("ROOTDEV 0x%x; ", dev);
#endif
	for (i=0; i<6; i++) {
		if (devices[i].num == (dev & 0xfff0)) {
			strcpy(&name[NAMEOFF], devices[i].name);
			offs = strlen(devices[i].name);
			if (i < 4) {
				if (dev & 0x07) {
					name[NAMEOFF+offs] = '0' + (dev & 7);
					name[NAMEOFF+offs+1] = 0;
				}
			}
			return name;
		}
	}
	return NULL;
}

/*
 * Convert a /dev/ name to device number.
 */
static int INITPROC parse_dev(char *line)
{
	int base = 0;
	struct dev_name_struct *dev = devices;

	if (strncmp(line,"/dev/",5) == 0)
		line += 5;
	do {
		int len = strlen(dev->name);
		if (strncmp(line,dev->name,len) == 0) {
			line += len;
			base = dev->num;
			break;
		}
		dev++;
	} while (dev->name);
	return (base + atoi(line));
}

/* Parse comma-separated list of numbers. Missing and empty fields (adjacent
 * commas) mean zero */
static void INITPROC parse_parms(int cnt, char *line, int *nums, int base)
{
	int i;
	char *l, *m, c;

	l = line;
	for (i = 0; i < cnt; i++) {
		m = l;
		while ((*l) && (*l != ',')) l++;
		c = *l;		/* ensure robust eol handling */
		if (l > m) {
			*l = '\0';
			nums[i] = (int)simple_strtol(m, base);
		} else nums[i] = 0;	/* item missing or negative */
		if (!c) break;
		l++;
	}
}

static void INITPROC comirq(char *line)
{
	int irq[MAX_SERIAL], i;

	parse_parms(MAX_SERIAL, line, irq, 0);
	for (i = 0; i < MAX_SERIAL; i++) 
		if (irq[i]) set_serial_irq(i, irq[i]);
}

static void INITPROC parse_nic(char *line, struct netif_parms *parms)
{
    char *p;

    parms->irq = (int)simple_strtol(line, 0);
    if ((p = strchr(line, ','))) {
        parms->port = (int)simple_strtol(p+1, 16);
        if ((p = strchr(p+1, ','))) {
            parms->ram = (int)simple_strtol(p+1, 16);
            if ((p = strchr(p+1, ',')))
                parms->flags = (int)simple_strtol(p+1, 0);
        }
    }
}

/*
 * This is a simple kernel command line parsing function: it parses
 * the command line from /bootopts, and fills in the arguments/environment
 * to init as appropriate. Any cmd-line option is taken to be an environment
 * variable if it contains the character '='.
 *
 * This routine also checks for options meant for the kernel.
 * These options are not given to init - they are for internal kernel use only.
 */
static int INITPROC parse_options(void)
{
	char *line = (char *)options;
	char *next;

	/* copy /bootops loaded by boot loader at 0050:0000 */
	fmemcpyb(options, kernel_ds, 0, DEF_OPTSEG, sizeof(options));

#pragma GCC diagnostic ignored "-Wstrict-aliasing"
	/* check file starts with ##, one or two sectors, max 1023 bytes */
	if (*(unsigned short *)options != 0x2323 || (options[511] && options[OPTSEGSZ-1]))
		return 0;

#if DEBUG
	printk("/bootopts: %s", &options[3]);
#endif
	next = line;
	while ((line = next) != NULL && *line) {
		if ((next = option(line)) != NULL) {
			if (*line == '#') {	/* skip line after comment char */
				next = line;
				while (*next != '\n' && *next != '\0')
					next++;
				continue;
			} else *next++ = 0;
		}
		if (*line == 0)		/* skip spaces and linefeeds */
			continue;
		/*
		 * check for kernel options first..
		 */
		if (!strncmp(line,"root=",5)) {
			int dev = parse_dev(line+5);
#if DEBUG
			printk("root %s=0x%04x\n", line+5, dev);
#endif
			ROOT_DEV = (kdev_t)dev;
			boot_rootdev = dev;    /* stop translation in device_setup */
			continue;
		}
		if (!strncmp(line,"console=", 8)) {
			int dev = parse_dev(line+8);
			char *p = strchr(line+8, ',');
			if (p) {
				*p++ = 0;
#ifdef CONFIG_CHAR_DEV_RS
				/* set serial console baud rate*/
				rs_setbaud(dev, simple_strtol(p, 10));
#endif
			}


#if DEBUG
			printk("console %s=0x%04x\n", line+8, dev);
#endif
			boot_console = dev;
			continue;
		}
		if (!strcmp(line,"ro")) {
			root_mountflags |= MS_RDONLY;
			continue;
		}
		if (!strcmp(line,"rw")) {
			root_mountflags &= ~MS_RDONLY;
			continue;
		}
		if (!strncmp(line,"debug=", 6)) {
			debug_level = (int)simple_strtol(line+6, 10);
			continue;
		}
		if (!strncmp(line,"init=", 5)) {
			line += 5;
			init_command = argv_init[1] = line;
			continue;
		}
		if (!strncmp(line,"ne0=", 4)) {
			parse_nic(line+4, &netif_parms[ETH_NE2K]);
			continue;
		}
		if (!strncmp(line,"wd0=", 4)) {
			parse_nic(line+4, &netif_parms[ETH_WD]);
			continue;
		}
		if (!strncmp(line,"3c0=", 4)) {
			parse_nic(line+4, &netif_parms[ETH_EL3]);
			continue;
		}
		if (!strncmp(line,"ee0=", 4)) {
			parse_nic(line+4, &netif_parms[ETH_EE16]);
			continue;
		}
		if (!strncmp(line,"le0=", 4)) {
			parse_nic(line+4, &netif_parms[ETH_LANCE]);
			continue;
		}
		if (!strncmp(line,"bufs=", 5)) {
			nr_ext_bufs = (int)simple_strtol(line+5, 10);
			continue;
		}
		if (!strncmp(line,"cache=", 6)) {
			nr_map_bufs = (int)simple_strtol(line+6, 10);
			continue;
		}
		if (!strncmp(line,"xmsbufs=", 8)) {
			nr_xms_bufs = (int)simple_strtol(line+8, 10);
			continue;
		}
		if (!strncmp(line,"tasks=", 6)) {
			max_tasks = (int)simple_strtol(line+6, 10);
			continue;
		}
		if (!strncmp(line,"inodes=",7)) {
			nr_inodes = (int)simple_strtol(line+7, 10);
			continue;
		}
		if (!strncmp(line,"files=",6)) {
			nr_files = (int)simple_strtol(line+6, 10);
			continue;
		}
		if (!strncmp(line,"comirq=", 7)) {
			comirq(line+7);
			continue;
		}
		if (!strcmp(line,"strace")) {
			tracing |= TRACE_STRACE;
			continue;
		}
		if (!strcmp(line,"kstack")) {
			tracing |= TRACE_KSTACK;
			continue;
		}
		if (!strncmp(line,"xtide=", 6)) {
			parse_parms(6, line+6, xtideparms, 0);
			continue;
		}
		if (!strncmp(line,"hdparms=", 8)) {
			parse_parms(CHS_ARR_SIZE, line+8, hdparms, 10);
			continue;
		}
		if (!strncmp(line, "netbufs=", 8)) {
			parse_parms(2, line+8, netbufs, 10);
			continue;
		}
		if (!strncmp(line, "xtflpy=", 7)) {
			parse_parms(2, line+7, xt_floppy, 10);
			continue;
		}
		if (!strncmp(line, "fdcache=", 8)) {
			fdcache = (int)simple_strtol(line+8, 10);
			continue;
		}
		if (!strncmp(line,"TZ=",3)) {
			tz_init(line+3);
			/* fall through and add line to environment */
		}

		/*
		 * Then check if it's an environment variable or an init argument.
		 */
		if (!strchr(line,'=')) {    /* no '=' means init argument */
			if (args >= MAX_INIT_ARGS)
				panic(errmsg_initargs);
			argv_init[args++] = line;
		}
#if ENV
		else {
			if (envs >= MAX_INIT_ENVS)
				panic(errmsg_initenvs);
			envp_init[envs++] = line;
		}
#endif
	}
	return 1;	/* success*/
}

static void INITPROC finalize_options(void)
{
	int i;

	/* set ROOTDEV environment variable for fsck in /etc/mount.cfg */
	if (envs + running_qemu >= MAX_INIT_ENVS)
		panic(errmsg_initenvs);
	envp_init[envs++] = root_dev_name(ROOT_DEV);
	if (running_qemu)
		envp_init[envs++] = (char *)"QEMU=1";

#if DEBUG
	printk("args: ");
	for (i=1; i<args; i++)
		printk("'%s'", argv_init[i]);
	printk("\n");

#if ENV
	printk("envp: ");
	for (i=0; i<envs-1; i++)
		printk("'%s'", envp_init[i]);
	printk("\n");
#endif
#endif

	/* convert argv array to stack array for sys_execv*/
	args--;
	argv_init[0] = (char *)args;		/* 0 = argc*/
	char *q = (char *)&argv_init[args+2+envs+1];
	for (i=1; i<=args; i++) {		/* 1..argc = av*/
		char *p = argv_init[i];
		char *savq = q;
		while ((*q++ = *p++) != 0)
			;
		argv_init[i] = (char *)(savq - (char *)argv_init);
	}
	/*argv_init[args+1] = NULL;*/               /* argc+1 = 0*/
#if ENV
	if (envs) {
		for (i=0; i<envs; i++) {
			char *p = envp_init[i];
			char *savq = q;
			while ((*q++ = *p++) != 0)
				;
			argv_init[args+2+i] = (char *)(savq - (char *)argv_init);
		}

	}
#endif
	/*argv_init[args+2+envs] = NULL;*/
	argv_slen = q - (char *)argv_init;
	if (argv_slen > sizeof(argv_init))
		panic(errmsg_initslen);
}

/* return whitespace-delimited string*/
static char * INITPROC option(char *s)
{
	char *t = s;
	if (*s == '#')
		return s;
	for(; *s != ' ' && *s != '\t' && *s != '\n'; ++s, ++t) {
		if (*s == '\0')
			return NULL;
		if (*s == '"') {
			s++;
			while (*s != '"') {
				if (*s == '\0')
					return NULL;
				*t++ = *s++;
			}
			*t++ = 0;
			break;
		}
	}
	return s;
}
#endif /* CONFIG_BOOTOPTS*/

