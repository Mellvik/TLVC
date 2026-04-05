#ifndef _NET_NETBUF_H
#define _NET_NETBUF_H
/*
 * I/O buffers for TLVC ethernet interface IO.
 *
 * Feb 2023 Helge Skrivervik
 */

/* Buffer configuration for TLVC Ethernet NICs
 *
 * There are 3 buffer strategies:
 * - No buffers: The driver is moving data directly to/from the requester
 *	(via far mem move).
 * - Static buffers: The number of send and receive buffers specified in the
 *	NET_OBUFCNT and NET_IBUFCNT defines are allocated statically at
 *	compile time. The BUFCNT numbers may be 0, in which case the driver
 *	will run as if NO_BUFS was set (except the extra code is compiled in).
 * - Heap allocation: Buffer space is allocated from the kernel heap, which 
 * 	is slightly different from static allocation in that memory is 
 * 	allocated only if the network is running. Memory consumption will be 
 * 	slightly higher because of the headers in the heap allocation.
 *
 * When HEAP allocation is used, the # of buffers may be set in /bootopts via
 * the 'netbufs=' directive. 'netbufs=2,1' means 2 receive buffers, 1 transmit
 * buffer. The kernel will not do sanity checking on the netbufs= numbers,
 * it's entirely possible to make the system unbootable by requesting too many
 * buffers. 2,1 or 2,2 are reasonable choices for regular usage. Again, zero is
 * a valid selection and will turn off buffers entirely.
 * When using heap allocation, a header strucure per buffer is also 
 * allocated from the heap, 6 bytes per buffer.
 *
 * AGAIN: zero buffers is always a valid choice - which complicates the driver 
 * somewhat but makes benchmarking much more convenient.
 */
/*
 * March 2026 (HS): Added pagebuffers, 256 byte buffers modeled after the ne2k
 * NIC buffers, a very simple scheme in which a packet is stored in as many pages
 * as it takes and no inherent structure other than a 2 word header holding the 
 * size of the packet (i.e. how many pages it takes) and a status word. For
 * convenience, the 2 word header is kept while only the first word (size) is
 * used.
 * Memory allocation is thusly in pages and for coding efficiency the number of
 * pages must be a power of 2, conveniently 32, 8k bytes. For now, this buffer
 * strategy applies to incoming packets only. When selected, there is no output
 * buffering.
 * Initially, when page buffers are used, the buffer setting in bootopts is ignored.
 */
/* March 2026 (HS): Removed the non-buffered option and the static buffer option.
 * IOW the driver always uses buffers, kernel reads and writes never touch
 * the device */

#define NET_RXBUFS  0 	/* indexes */
#define NET_TXBUFS  1

#define NO_BUFS     0	/* buffer support not compiled in */
#define STATIC_BUFS 1	/* use static buffer allocation, ignore netbufs= in bootopts */
#define HEAP_BUFS   2	/* allocate buffers from kernel heap */
#define PAGE_BUFS   3	/* Use page buffers for receving, no buffers for output */

/* Set the default strategy */
//#define NET_BUF_STRAT	PAGE_BUFS
#define NET_BUF_STRAT	HEAP_BUFS

/* Default buffer allocations (1 or more), in case
 * netbufs= is missing from /bootopts */
#define NET_OBUFCNT 2
#define NET_IBUFCNT 2

#ifndef __ASSEMBLER__

#include <linuxmt/limits.h>

struct netbuf *netbuf_init(struct netbuf *, int);
extern int netbufs[];

struct netbuf {
	int len;			/* ZERO if available */
	struct netbuf *next;
	char *data;	/* allocate from heap */
};

struct page {
	char c[256];
};

struct pbuf {		/* packet buffers split into pages */
	struct page   *top;	/* MUST be first! */
	struct page   *base;
	unsigned char head;	/* always 1 ahead */
	unsigned char tail;
};

#define PBUF_COUNT	32	/* Always power of 2 */

void netbuf_release(struct netbuf *);

struct netbuf *net_ibuf;
struct netbuf *rnext;

struct netbuf *net_obuf;
struct netbuf *tnext, *pnext;

#endif /* ASSEMBLER */

#endif
