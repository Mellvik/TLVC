/* chmem - set total memory size for execution	Author: Andy Tanenbaum
 * from original Minix 1 source in "Operating Systems: Design and Implemtation", 1st ed.
 *
 * Completely rewritten for ELKS by Greg Haerr
 * Handle Venix binaries (TLVC) June 2026 by Helge Skrivervik
 *
 * The 8088 architecture does not make it possible to catch stacks that grow big.
 * One way to deal with this problem is to let the stack grow down towards the data
 * segment and the data segment grow up towards the stack. Default stack & heap size
 * on TLVC is 4K for each, but if the programmer knows that a smaller amount is
 * sufficient (or larger is required), s/he can change it using chmem.
 *
 * Usage: chmem [-h heap] [-s stack] files...
 *	Heap or stack values are specified in decimal by default, or hex with leading 0x.
 *	Maximum heap can be specified using -h 0xffff, which is treated specially by the kernel.
 *	When specifying heap or stack options, chmem will always write out a v1 header.
 *	Functions as 'size' program when run with no options.
 *
 * Explanations of chmem (size) output:
 *	TEXT	size of code
 *	FTEXT	size of far segment code (for medium memory model programs)
 *	DATA	size of initialized data
 *	BSS	size of uninitialized data
 * 	HEAP	header heap size (0 = default 4K, 0xFFFF = allocate maximum heap)
 *	STACK	header minimum stack size (0 = default 4K)
 *	TOTDATA	size of DATA+BSS+heap+stack
 *	TOTAL	size of TOTDATA+TEXT+FTEXT (in-memory size less environment)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <linuxmt/minix.h>
#include <linuxmt/v7.h>

#define MAGIC       0x0301	/* magic number for executable progs */
#define MAX         65520L	/* maximum allocation size */

int first;

char *progname;

/* Print an error message and die */
int msg(const char *s, ...)
{
	va_list		p;
	va_start(p, s);
	vfprintf(stderr, s, p);
	va_end(p);
	return 1;
}

void usage(void)
{
	msg("Usage: %s [-h heap] [-s stack] files...\n", progname);
	exit(1);
}

void hdr(void) {
	printf(" TEXT  FTEXT   DATA    BSS   HEAP  STACK  TOTDATA  TOTAL\n");
	first++;
}

int do_chmem(char *filename, int changeheap, int changestack,
	 unsigned long heap, unsigned long stack)
{
	int fd;
	int rdonly = !changeheap && !changestack;
	unsigned int magic, oldheap, oldstack, dsegsize, displayheap, newheap;
	unsigned long newstack, totdata, total;
	struct minix_exec_hdr header;
	struct elks_supl_hdr suplhdr;
	struct v7_exec *v7hdr = (struct v7_exec *)&header;
	size_t dseg, bseg, ftseg;
	unsigned long tseg;
	int len;

	if ((fd = open(filename, rdonly? O_RDONLY: O_RDWR)) < 0) {
		perror(filename);
		return 1;
	}
	if (read(fd, &header, sizeof(header)) != sizeof(header))
		return msg("%s: bad header\n", filename);
	len = header.hlen;
	magic = v7hdr->a_magic;

	if (magic != MAGIC && magic != OMAGIC && magic != NMAGIC)
		return msg("%s: not an executable\n", filename);

	if (magic == MAGIC && header.version > 1)
		return msg("%s: unsupported a.out version %u\n", filename,
			   (unsigned)header.version);
	if (magic != MAGIC) len = 32;		/* no length in V7 header */

	if (len >= sizeof(header) + sizeof(suplhdr)) {
		if (read(fd, &suplhdr, sizeof(suplhdr)) != sizeof(suplhdr))
			return msg("%s: bad header\n", filename);
	} else
		memset(&suplhdr, 0, sizeof(suplhdr));
	if (!first) hdr();

	if (magic != MAGIC) {
	    tseg = v7hdr->a_text;
	    ftseg = 0;
	    dseg = (size_t)v7hdr->a_data;
	    bseg = (size_t)v7hdr->a_bss;
	    dsegsize = dseg + bseg;
	    oldheap = (size_t)v7hdr->a_heap;
	    if (!oldheap) oldheap = 0xFFF0;
	    oldstack = v7hdr->a_stack? v7hdr->a_stack:v7hdr->a_hstack;
	    if (oldstack == 0) oldstack = 0x2000; /* stack high, unmodified */
	    if (magic == OMAGIC) {
		dsegsize += (size_t)tseg + oldstack;
		if (oldheap >= 0xFFF0) oldheap -= dsegsize;
		totdata = total = (unsigned long)(oldheap + dsegsize);
	    } else {
		if (oldheap >= 0xFFF0) totdata = 0xFFF0;
		else totdata = (unsigned long)dsegsize + oldheap + oldstack;
		total = totdata + tseg;
	    }

	    printf("%5lu  %5u  %5u  %5u  %5u  %5u   %6lu %6lu %s\n", tseg,
		ftseg, dseg, bseg, oldheap, oldstack, totdata, total, filename);
	} else {
	    tseg = header.tseg;
	    ftseg = (size_t)suplhdr.esh_ftseg;
	    dseg = (size_t)header.dseg;
	    bseg = (size_t)header.bseg;
	    dsegsize = dseg + bseg;

	    if (header.chmem == 0) {				/* default heap*/
		oldheap = INIT_HEAP;
		displayheap = 0;
	    } else if (header.version == 1)
		displayheap = oldheap = header.chmem;
	    else
		displayheap = oldheap = header.chmem - dsegsize;	/* v0 displays effective heap, not header value*/

	    if (header.version == 1)
		oldstack = header.minstack? header.minstack: INIT_STACK;
	    else oldstack = 0;			/* v0 doesn't display seperate stack*/

	    if (header.chmem >= 0xFFF0) {
		totdata = 0xFFF0;
		total = totdata + tseg + ftseg;
	    } else {
		totdata = (unsigned long)oldheap+oldstack+dsegsize;
		total = (unsigned long)tseg+ftseg+dsegsize+oldheap+oldstack;
	    }
	    printf("%5lu  %5u  %5u  %5u  %5u  %5u   %6lu %6lu %s", tseg, ftseg, dseg, bseg,
		displayheap, header.minstack, totdata, total, filename);

	    if (header.version == 0)
		printf(" (v0 header)");
	    if (suplhdr.esh_compr_tseg || suplhdr.esh_compr_dseg || suplhdr.esh_compr_ftseg)
		printf(" (%u %u %u)",
			suplhdr.esh_compr_tseg, suplhdr.esh_compr_ftseg, suplhdr.esh_compr_dseg);
	}

	if (magic != MAGIC) {
		char *type;
		if (magic == OMAGIC) type = "V7 tiny";
		else type = "V7 split I&D";
	 	printf("%56s %s, stack %s ", "", type, (size_t)v7hdr->a_stack?"low":"high");
	}
	printf("\n");

	if (rdonly) {
		close(fd);
		return 0;
	}

	if (!changeheap)
		heap = oldheap;
	if (!changestack)
		stack = oldstack;
	if (stack == oldstack && heap == oldheap) {
		printf("No changes made\n");
		return 0;
	}
	if (heap > 0xFFFF)
		return msg("%s: heap too large: %ld\n", filename, heap);
	if (stack > MAX)
		return msg("%s: stack too large: %ld\n", filename, stack);

	if (magic != MAGIC && v7hdr->a_stack && changestack) {
		printf("Warning: stack located low, cannot change\n"); 
		stack = v7hdr->a_stack;
	}
	newstack = stack? stack: INIT_STACK;
	newheap = heap? heap: INIT_HEAP;

	if (magic == OMAGIC) dsegsize -= oldstack;	/* avoid counting both old and new stack */
	if (newheap < 0xFFF0 && (unsigned long)dsegsize+newheap+newstack > MAX) {
		heap = MAX - dsegsize - newstack - 128;		/* reserve 128 bytes for environment*/
		if (heap < newheap)
			return msg("%s: heap+stack too large: %ld\n", filename, heap + newstack);
		msg("Warning: heap truncated to %ld\n", heap);
		newheap = heap;
	}

	if (newheap >= 0xFFF0)
		totdata = 0xFFF0;
	else
		totdata = (unsigned long)newheap + newstack + dsegsize;

	total = totdata;
	if (magic != OMAGIC)
		total += tseg;

	printf("%5lu  %5u  %5u  %5u  %5lu  %5lu   %6lu %6lu %s\n",
			tseg, ftseg, dseg, bseg, heap, stack, totdata, total, filename);
	if (magic != MAGIC) {
	    v7hdr->a_heap = heap;
	    /* Never change a_stack, it's either actual & fixed (low) or 0 (high) */ 
	    if (v7hdr->a_stack == 0) v7hdr->a_hstack = stack;
	} else {
	    header.chmem = (unsigned)heap;
	    header.minstack = (unsigned)stack;
	    header.version = 1;
	}

	lseek(fd, 0L, SEEK_SET);
	if (write(fd, &header, sizeof(header)) != sizeof(header)) {
		perror(filename);
		close(fd);
		return 1;
	}
	close(fd);

	return 0;
}

int
main(int argc, char **argv)
{
	int ch, err;
	int changeheap = 0, changestack = 0;
	unsigned long heap = 0, stack = 0;

	progname = argv[0];
	while ((ch = getopt(argc, argv, "h:s:")) != -1) {
		switch (ch) {
		case 'h':
			heap = strtoul(optarg, NULL, 0);
			changeheap = 1;
			break;
		case 's':
			stack = strtoul(optarg, NULL, 0);
			changestack = 1;
			break;
		default:
			usage();
			break;
		}
	}
	if (optind >= argc)
		usage();

	while (optind < argc) {
		if (do_chmem(argv[optind], changeheap, changestack,
					   heap, stack))
			err = 1;
		argc--;
		argv++;
	}

	return err;
}
