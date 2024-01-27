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

#define NET_RXBUFS  0 	/* indexes */
#define NET_TXBUFS  1

#define NO_BUFS     0	/* buffer support not compiled in */
#define STATIC_BUFS 1	/* use static buffer allocation, ignore netbufs= in bootopts */
#define HEAP_BUFS   2	/* allocate buffers from kernel heap */

/* Set the current strategy */
#define NET_BUF_STRAT	HEAP_BUFS

/* Use when strategy is STATIC_BUFS or HEAP_BUFS - in the latter case only 
 * if netbufs= is missing from /bootopts */
#define NET_OBUFCNT 2
#define NET_IBUFCNT 2

#ifndef __ASSEMBLER__

#include <linuxmt/limits.h>

extern int netbufs[];

struct netbuf {
	int len;			/* ZERO if available */
	struct netbuf *next;
#if NET_BUF_STRAT == HEAP_BUFS 
	char *data;	/* allocate from heap */
#else
	char data[MAX_PACKET_ETH];
#endif
};

void netbuf_release(struct netbuf *);

#if NET_BUF_STRAT == HEAP_BUFS
struct netbuf *net_ibuf;
struct netbuf *rnext;

struct netbuf *net_obuf;
struct netbuf *tnext;
#endif

#if NET_BUF_STRAT == STATIC_BUFS
struct netbuf net_ibuf[NET_IBUFCNT];
struct netbuf *rnext;

struct netbuf net_obuf[NET_OBUFCNT];
struct netbuf *tnext;
#endif

#endif /* ASSEMBLER */

#endif
