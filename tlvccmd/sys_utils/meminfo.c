/*
 * meminfo.c
 *
 * Copyright (c) 2002 Harry Kalogirou
 * harkal@gmx.net
 *
 * Enhanced by Greg Haerr 24 Apr 2020
 * Partial rewrite by Helge Skrivervik, TLVC, 2024/2025 
 *
 * This file may be distributed under the terms of the GNU General Public
 * License v2, or at your option any later version.
 *
 * TODO: Rewrite and simplify mem_map with a big loop through arenas instead
 *	 of linear coding which includes a lot of fixed expectations that keep
 *	 changing all the time.
 */

#define __LIBC__            /* get all typedefs */
#include <linuxmt/types.h>
#include <linuxmt/mm.h>
#include <linuxmt/mem.h>
#include <linuxmt/heap.h>
#include <linuxmt/sched.h>
#include <linuxmt/config.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/ioctl.h>

#define LINEARADDRESS(off, seg)		((off_t) (((off_t)seg << 4) + off))

int aflag;		/* show application memory*/
int fflag;		/* show free memory*/
int tflag;		/* show tty and driver memory*/
int bflag;		/* show buffer memory*/
int mflag;		/* show application memory allocation */
int Mflag;		/* show RAM allocation of heap and app memory */
int Pflag;		/* Like M, but show processes too */
int sflag;		/* show system memory */
int allflag;		/* show all memory*/

int fd;
unsigned int ds, cs, ftext;
unsigned int heap_all;
unsigned int seg_all;
unsigned int taskoff;
unsigned int bss_size;
int maxtasks;
int segptr;
int has_umb;
struct task_struct task_table;
struct mem_usage mu;
struct proc_list_s {
	seg_t seg;
	int flag;
	char proc[12];
	unsigned int size;
} *proc_list;

#define MAX_BLOCKS 5

static long_t total_segsize = 0;
static char *segtype[] =
    { "free", "CSEG", "DSEG", "DDAT", "FDAT", "BUF ", "RDSK", "BUFH" };
struct pseg_s { seg_t base, end; } heap[MAX_BLOCKS], segs[MAX_BLOCKS], extbuf;
unsigned int free_cnt[MAX_BLOCKS];

int memread(word_t off, word_t seg, void *buf, int size)
{
	if (lseek(fd, LINEARADDRESS(off, seg), SEEK_SET) == -1)
		return 0;

	if (read(fd, buf, size) != size)
		return 0;

	return 1;
}

word_t getword(word_t off, word_t seg)
{
	word_t word;

	if (!memread(off, seg, &word, sizeof(word)))
		return 0;
	return word;
}

void p_divider_l(unsigned long seg, char *suffix)
{
	printf("%05lx:0+--------------------+ %s\n", seg, suffix);
}

void p_divider(unsigned int seg, char *suffix)
{
	printf("%04x:0 +--------------------+ %s\n", seg, suffix);
}

void p_subdiv(unsigned int seg, char *suffix, int flag)
{
	char *s = "%04x:0";

	if (flag) 	/* value is offset */
	    s = " +%04x";
	printf(s, seg);
	printf(" +- - - - - - - - - - + %s\n", suffix);
}

char *make_kb(char *n, long_t size)
{
	unsigned int r = 1024;
	unsigned int t = __divmod(size, &r);

	if (t > 9)
		sprintf(n, "%-uK", t);
	else {
		if (r > 1019) r = 1019;		/* avoid confusing rounding errors */
		sprintf(n, "%-u.%1uK", t, r/102);
	}
	return n;
}

void p_empty(void)
{
	printf("       | %-19s|\n", "");
}

void pp_block(seg_t start, seg_t end)		/* box content with process names etc. */
{
	int i, count = 0;
	char p[16];

	for (i = segptr - 1; i; i--) {
	    if ((proc_list[i].seg >= start) && (proc_list[i].seg < end)) {
		printf("  %04x | %c %-17s| %s\n", proc_list[i].seg,
	           *segtype[proc_list[i].flag], proc_list[i].proc,
		   make_kb(p, (long_t)proc_list[i].size<<4));
		count++;
	    }
	}
	if (count > 4) p_empty();
}

void p_block(int count, long_t size, char *txt, char *suffix)
{
	int i, mid;
	char n[8];

	mid = count/2;
	for (i = 0; i < count; i++) {
	    if (i == mid) 
		printf("       | %-19s| %3s %s\n", txt, make_kb(n, size), suffix);
	    else
	    	p_empty();
	}
}

char buf[80];
void process_name(unsigned int off, unsigned int seg)
{
    word_t argc, argv;

    argc = getword(off, seg);

    while (argc-- > 0) {
        off += 2;
        argv = getword(off, seg);
        if (!memread(argv, seg, buf, sizeof(buf)))
            return;
        if (!Pflag) printf("%s ",buf);
        break;      /* display only executable name for now */
    }
}

struct task_struct *find_process(unsigned int seg)
{
    int i;
    int off = taskoff;

    for (i = 0; i < maxtasks; i++) {
        if (!memread(off, ds, &task_table, sizeof(task_table))) {
            perror("taskinfo");
            exit(1);
        }
        if ((unsigned)task_table.mm[SEG_CODE] == seg ||
            (unsigned)task_table.mm[SEG_DATA] == seg) {
            return &task_table;
        }
        off += sizeof(struct task_struct);
    }
    return NULL;
}

void display_seg(word_t mem)
{
    seg_t segbase = getword(mem + offsetof(segment_s, base), ds);
    segext_t segsize = getword(mem + offsetof(segment_s, size), ds);
    word_t segflags = getword(mem + offsetof(segment_s, flags), ds) & SEG_FLAG_TYPE;
    byte_t ref_count = getword(mem + offsetof(segment_s, ref_count), ds);
    struct task_struct *t;

    if (!Pflag)
	printf("%04x   %s %7ld %4d  ",
	    segbase, segtype[segflags], (long_t)segsize << 4, ref_count);
    if (segflags == SEG_FLAG_CSEG || segflags == SEG_FLAG_DSEG) {
	if ((t = find_process(mem)) != NULL) {
	    process_name(t->t_begstack, t->t_regs.ss);
	}
	if (Pflag) {
	    proc_list[segptr].seg = segbase;
	    proc_list[segptr].flag = segflags;
	    strncpy(proc_list[segptr].proc, buf,  11);
	    proc_list[segptr].size = segsize;
	    //printf("proc_list: %x %x %s %u\n", segbase, segflags, buf, segsize);
	    segptr++;
	}
    }
    total_segsize += (long_t)segsize << 4;
}

/*
 * This is where most of the dirty work happens.
 * First scan the heap and map every entry into the heap-struct.
 * Then, using the pointers in the heap, map every main memory allocation
 * into the segs struct. 
 * The rest of the code pretty much assumes that the sequence of arenas in the
 * segs array is: main mem - umbs - freed fartext - freed lower mem. FIX!
 */ 
void blk_scan(void)
{
	/*
	 * REMINDER: The size of a heap block is in bytes (heap.size),
	 * while size of a main memory segment is in paragraphs
	 */
        word_t segflags, mem, n = getword(heap_all + offsetof(list_s, next), ds);
	seg_t segbase, oldbase = 0, oldend, curend;
	int i = 0;
	unsigned int size;
	long_t s;
	char nn[8];

	curend  = n + sizeof(heap_s) + getword(n + offsetof(heap_s, size), ds);
	while (n != heap_all) {
	    if (!oldbase) heap[i].base = n;	/* initial */
	    if (n < oldbase) {
		heap[i].end = curend-1;
		heap[++i].base = n;
	    }
	    oldbase = n;
	    curend = n + sizeof(heap_s) + getword(n + offsetof(heap_s, size), ds);
            n = getword(n + offsetof(list_s, next), ds);
	} 
	heap[i].end = curend;

	i = oldbase = 0;
	oldend = 0xffff;
	n = getword(seg_all + offsetof(list_s, next), ds);
	while (n != seg_all) {
	    mem = n - offsetof(segment_s, all);
	    segbase = getword(mem + offsetof(segment_s, base), ds);
	    segflags = getword(mem + offsetof(segment_s, flags), ds) & SEG_FLAG_TYPE;
	    size = getword(mem + offsetof(segment_s, size), ds);
	    curend = segbase + size;
	    if (!oldbase) segs[i].base = segbase; 	/* initial */
	    if (segflags == SEG_FLAG_EXTBUF) {
		extbuf.base = segbase;
		extbuf.end  = curend;
	    }
	    //printf("segbase=%x oldbase=%x curend=%x\n", segbase, oldbase, curend);
	    if ((segbase < oldbase) || (segbase > oldend)) { /* catch UMBs too */
		//printf("new arena, oldend %x, new base %x\n", oldend, segbase);
		segs[i].end  = oldend;
		segs[++i].base = segbase;	
		if (segbase >= 0xa000) has_umb++;	/* system has UMBs */
	    }
	    if (segflags == SEG_FLAG_FREE) free_cnt[i] += size;
	    //if (segflags == SEG_FLAG_FREE) printf("FREE: arena %d, free %ld |", i, ((long)size<<4));
	    oldend = curend;
	    oldbase = segbase;
	    n = getword(n + offsetof(list_s, next), ds);
	}
	segs[i].end = curend;
	if (Pflag) return;

	printf("Kernel heap:\n\t   SEG   OFFS   SIZE\n");
	int tot = 0;
	for (i = 0; heap[i].base; i++) {
	    printf(" Block %d: %04x   %04x  %5u bytes\n", i+1, ds, heap[i].base,
	    	     heap[i].end - heap[i].base);
	    tot += heap[i].end - heap[i].base;
	}
	printf(" Kernel heap total:    %u bytes\n", tot);
	printf("\nMain memory:\n");

	for (i = 0; segs[i].base; i++) {
	    s = (long_t)(segs[i].end - segs[i].base) << 4;
	    printf(" Arena %d: %04x - %04x (%6lu bytes, %s)\n", i+1, segs[i].base,
			segs[i].end, s, make_kb(nn, s));
	}
	if (extbuf.base) {
	    s = (long_t)(extbuf.end - extbuf.base) << 4;
	    printf(" Ext buf: %04x - %04x (%6lu bytes, %s)\n", extbuf.base, extbuf.end,
		     s, make_kb(nn, s));
	}
}

void map_umb(char *msg, int n) {
	msg[strlen(msg)-1] = n + '1';
	p_divider((long_t)(segs[n].end), "");
	if (Pflag) {
	    p_block(1, (long_t)(segs[n].end - segs[n].base)<<4, "UMB block,    size", msg);
	    p_block(1, (long_t)(free_cnt[n])<<4, "         available", "");
	    p_empty();
	    pp_block(segs[n].base, segs[n].end);
	} else
	    p_block(5, (long_t)(segs[n].end - segs[n].base)<<4, "Upper memory block", msg);
	p_divider((long_t)(segs[n].base), "UMB block base");
	printf("\n");
}

/*
 * Paint a graphic representation of the memory layout. 
 * Uses data generated by blk_scan() above.
 */
void mem_map(void)
{
	int i, start;
	seg_t s_size;
	char main_msg[] = "Main memory arena x";
	unsigned int membase = ds + (seg_t)(((long_t)heap[0].end + 0x8L)>>4);
	int hma = (mu.xms_start == 64);

	printf("\n");
	/* The XMS block, if present, will 'merge' with the HMA block - if present.
	 * The XMS block has one or two sub-blocks depending on usage.
	 * XMS never includes HMA.
	 * mu.xms_start is where XMS memory actually starts, in kbytes above 1M.
	 * IOW - XMS memory may not start at 1M and x_top may not indicate the
	 * amount of memory in the system. Also, when displaying XMS usage, we 
	 * need to count down, not up.
	 */
	if (mu.xms_free + mu.xms_used) {	/* XMS memory available */
	    long x_start = (long)(mu.xms_start<<6) + 0x10000;	/* Above HMA */
	    long x_top = (((long)(mu.xms_free + mu.xms_used))<<6) + x_start;
	    int sizer = mu.xms_free/1000 ? mu.xms_free/1000: -2;

	    p_divider_l(x_top, "XMS top");
	    if (mu.xms_free) {		/* we have a 'free' sub-block on top */
		p_block(5+sizer, (long)mu.xms_free<<10, "   Free XMS", "");
		if (mu.xms_used || !hma)
		    p_divider_l(x_top - ((long)mu.xms_free<<6), mu.xms_used ? "" : "XMS start");
	    }
	    if (mu.xms_used) {		/* The 'used' sub-block may be the only block. */
	    				/* Buffers is currently the only use,
					 * fix when more uses are added */
	        sizer = mu.xms_used/1000 ? mu.xms_used/1000: -2;
		p_block(5 + sizer, (long)mu.xms_used<<10, "   XMS buffers", "");
		if (!hma) p_divider_l(x_start, "XMS start");
	    }
	}
	if (hma) {	/* HMA available */
	    p_divider_l(0x11000L, "HMA top/XMS start");
	    p_block(3, (long_t)0x10000, (cs == 0xffff)? "   Kernel text":"   Available", "");
	    p_divider_l((long)cs+1, "HMA start");
	    printf("\n");
	}
	if (has_umb) {		/* map UMB blocks */
	    /* since the segs array is structured the way it is, we're accessing it backwards */
	    i = MAX_BLOCKS - 1;
	    while (i) {
		if (segs[i].base > 0x9fff) 	/* UMB is > 640k, HMA is separate */
	   	    map_umb(main_msg, i);
		i--;
	    }
	}
	p_divider(segs[0].end, "Top of conv. memory");
	i = 8;
	if (Pflag) i = 1;
	if (extbuf.base) { 
	    p_block(3, (long_t)(segs[0].end - extbuf.base)<<4, "Ext buffers", "");
	    p_subdiv(extbuf.base, "", 1);
	    i -= 2;
	}
	p_block(i, (long_t)(segs[0].end - membase)<<4, "Main memory", "Arena 1");
	if (Pflag)
	    pp_block(segs[0].base, segs[0].end); /* display processes populating the arena */
	p_divider(membase, "Kernel DS end");
	p_block(3, (long_t)(heap[0].end-heap[0].base+1), "Main kernel heap", "");
	p_subdiv(heap[0].base, "", 1);

	if (heap[1].base) {		/* More heap found: the released bootopts buffer */
	    p_block(1, (long_t)(heap[0].base-heap[1].end), "Kernel bss", "");
	    p_subdiv(heap[1].end, "BSS continues", 1);
	    p_block(1, (long_t)(heap[1].end-heap[1].base), "[bootopts buffer]",
			"Heap block 2");
	    p_subdiv(heap[1].base, "", 1);
	    p_block(1, (long_t)(heap[1].base - (heap[0].base - bss_size)), "Kernel bss", "");
	} else
	    p_block(1, (long_t)bss_size, "Kernel bss", "");

	p_subdiv(heap[0].base - bss_size, "BSS start", 1);
	p_block(1, (long_t)heap[0].base - bss_size, "Kernel data", "");
	p_divider(ds, "Kernel DS start");
	i = 1; 					/* index into the segs[] array */
	while (segs[i].base > segs[0].end) i++; /* skip umbs - assumes predetermined sequence
						 * of memory arenas in the segs array */

	if (ftext) {		/* we have FARTEXT, then we also have INITPROC */
	    main_msg[strlen(main_msg)-1] = i + '1';
	    p_block(1, (long_t)(segs[i].end-segs[i].base)<<4, "[INITPROC code]",
			main_msg);
	    if (Pflag) {
		p_empty();
		pp_block(segs[i].base, segs[i].end);
	    }
	    if (segs[i].base > ftext)	/* if fartext > INITPROC, sometimes it's not */
		p_subdiv(segs[i].base, "", 0);
	    p_block(1, (long_t)(segs[i].base - ftext)<<4, "Kernel fartext", "");
	    p_divider(ftext, "");
	    i++;
	    while (segs[i].base > segs[0].end) i++; 			/* skip umbs */
	    s_size = ftext - cs;
	} else
	    s_size = ds - cs;
	if (cs != 0xffff) {				/* kernel text in conv memory */
		p_block(5, (long_t)s_size<<4, "Kernel text", "");
		p_divider(cs, "");
	} else cs = ftext;
	if (XD_BOUNCESEGSZ) {	/* bounce buffer for MFM/Lance configured, 1k */
	    p_block(1, (long_t)XD_BOUNCESEGSZ, "XD/Lance bounce", "");
	    start = cs - (XD_BOUNCESEGSZ>>4);
	    p_divider(start, "");
	} else
	    start = cs;

	main_msg[strlen(main_msg)-1] = i + '1';
	if (segs[i].end == 0) {		/* FDcache is filling the area */
	    p_block(1, (long_t)(start - REL_INITSEG)<<4, "Floppy cache", "");
	} else {	/* we have a released segment, figure out size and show */
	    char *type;
	    if (start - segs[i].base <= 0x40)
		type = "[setup-data]";
	    else
		type = "[Unused FDcache]";
	    p_block(1, (long_t)(segs[i].end-segs[i].base)<<4, type, main_msg);
	    if (Pflag)
		pp_block(segs[i].base, segs[i].end);
	    if (segs[i].base > REL_INITSEG) {
		p_divider(segs[i].base, "");
	        p_block(1, (long_t)(segs[i].base - REL_INITSEG)<<4, "Floppy cache", "in use");
	    }
	}
	p_divider(REL_INITSEG, "");
	p_block(1, (long_t)OPTSEGSZ, "OPTSEG/FD_BOUNCE", "");
	p_divider(DEF_OPTSEG, "");
	if (DEF_OPTSEG > 0x50)
	    p_subdiv(0x80, "Reserved, LOADALL", 1);
	p_block(1, (long_t)DEF_OPTSEG<<4, "IRQ vectors/BDA", "");
	p_divider(0, "\n");
	
	return;
}


void dump_segs(void)
{
    word_t n, mem, area = 1;
    seg_t segbase, oldbase = 0;

    if (!Pflag) printf("\t SEG   TYPE    SIZE  CNT  NAME\n");

    n = getword(seg_all + offsetof(list_s, next), ds);

    while (n != seg_all) {
	mem = n - offsetof(segment_s, all);
	segbase = getword(mem + offsetof(segment_s, base), ds);
	oldbase = segs[area-1].base;

	if (!Pflag) {
	    if (segbase == oldbase)
        	printf("Arena %d ", area++);
	    else
	    	printf("        ");
	}
	display_seg(mem);
	if (!Pflag) printf("\n");
	/* next in list */
	n = getword(n + offsetof(list_s, next), ds);
    }
}

void dump_heap(void)
{
	word_t total_size = 0;
	word_t total_free = 0;
	static char *heaptype[] = { "free", "MEM ", "DRVR", "TTY ", "TASK", "BUFH", "PIPE",
				    "INOD", "FILE", "CACH", "NETB", "OPTS" };

	printf("  HEAP   TYPE  SIZE    SEG   STYPE   SSIZE CNT  NAME\n");

	word_t n = getword(heap_all + offsetof(list_s, next), ds);
	while (n != heap_all) {
		word_t h = n - offsetof(heap_s, all);
		word_t size = getword(h + offsetof(heap_s, size), ds);
		byte_t tag = getword(h + offsetof(heap_s, tag), ds) & HEAP_TAG_TYPE;
		word_t mem = h + sizeof(heap_s);
		word_t segflags;
		int free, app, tty, buffer, system;

		if (tag == HEAP_TAG_SEG)
			segflags = getword(mem + offsetof(segment_s, flags), ds) & SEG_FLAG_TYPE;
		else segflags = -1;
		free = (tag == HEAP_TAG_FREE || segflags == SEG_FLAG_FREE);
		app = ((tag == HEAP_TAG_SEG)
		     && (segflags == SEG_FLAG_CSEG || segflags == SEG_FLAG_DSEG ||
			 segflags == SEG_FLAG_DDAT || segflags == SEG_FLAG_FDAT));
		tty = (tag == HEAP_TAG_TTY || tag == HEAP_TAG_DRVR);
		buffer = ((tag == HEAP_TAG_SEG) && (segflags == SEG_FLAG_EXTBUF))
			 || tag == HEAP_TAG_BUFHEAD || tag == HEAP_TAG_CACHE
			 || tag == HEAP_TAG_PIPE || tag == HEAP_TAG_NETWORK;
		system = (tag == HEAP_TAG_TASK || tag == HEAP_TAG_INODE || tag == HEAP_TAG_FILE);

		if (allflag ||
		   (fflag && free) || (aflag && app) || (tflag && tty) || (bflag && buffer) 
				   || (sflag && system)) {
			printf("  %04x   %s %5d   ", mem, heaptype[tag], size);
			total_size += size + sizeof(heap_s);
			if (tag == HEAP_TAG_FREE)
				total_free += size;

			switch (tag) {
			case HEAP_TAG_SEG:
				display_seg(mem);
				break;
			}
			printf("\n");
		}

		/* next in heap */
		n = getword(n + offsetof(list_s, next), ds);
	}
	if (allflag)
		printf("Heap/free   %5u/%u, Total mem %lu\n", total_size, total_free, total_segsize);
	else
		printf("Total size: %5u\n", total_size);
}

void usage(void)
{
	printf("usage: meminfo [-amMPftbsh]\n");
}

int main(int argc, char **argv)
{
	int c;

	if (argc < 2)
		allflag = 1;
	else while ((c = getopt(argc, argv, "aftbsmMPh")) != -1) {
		switch (c) {
			case 'a':
				aflag = 1;
				break;
			case 'f':
				fflag = 1;
				break;
			case 't':
				tflag = 1;
				break;
			case 'b':
				bflag = 1;
				break;
			case 's':
				sflag = 1;
				break;
			case 'm':
				mflag = 1;
				break;
			case 'P':
				Pflag = 1;
				/* fall thru */
			case 'M':
				Mflag = 1;
				break;
			case 'h':
				usage();
				return 0;
			default:
				usage();
				return 1;
		}
	}

	if ((fd = open("/dev/kmem", O_RDONLY)) < 0) {
		perror(argv[0]);
		return 1;
	}
    if (ioctl(fd, MEM_GETDS, &ds) ||
        ioctl(fd, MEM_GETBSS_SZ, &bss_size) ||
        ioctl(fd, MEM_GETHEAP, &heap_all) ||
	ioctl(fd, MEM_GETSEGALL, &seg_all) ||
        ioctl(fd, MEM_GETTASK, &taskoff) ||
        ioctl(fd, MEM_GETFARTEXT, &ftext) ||
        ioctl(fd, MEM_GETCS, &cs) ||
        ioctl(fd, MEM_GETMAXTASKS, &maxtasks)) {
	    perror(argv[0]);
	    return 1;
    }
    ioctl(fd, MEM_GETUSAGE, &mu); /* note MEM_GETUSAGE amounts are floors, 
	     			   * so total may display 1k less than actual */
    if (!memread(taskoff, ds, &task_table, sizeof(task_table)))
        perror("taskinfo");

    blk_scan();	
    if (Mflag || Pflag) {
	if (Pflag)  {
	    int sz = sizeof(struct proc_list_s) * (maxtasks<<1);
	    if (!(proc_list = (struct proc_list_s *)malloc(sz))) {
		printf("%s: malloc failed\n", argv[0]);
		return 2;
	    }
	    memset(proc_list, 0, sz);	/* mandatory */
	    dump_segs(); 	/* silent dump to fill the proc_list array */
	}
	mem_map();
    }
    else if (mflag) dump_segs();
    else dump_heap();

    printf("Main memory: %d/%dK used, %dK free, ",
    		mu.main_used, mu.main_used + mu.main_free, mu.main_free);
    if (mu.xms_free + mu.xms_used) 
	printf("XMS: %d/%dK used, %dK free\n",
    		mu.xms_used, mu.xms_used + mu.xms_free, mu.xms_free);
    else printf("\n");

    return 0;
}
