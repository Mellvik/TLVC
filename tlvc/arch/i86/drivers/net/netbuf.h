#ifndef _NET_NETBUF_H
#define _NET_NETBUF_H
/*
 * I/O buffers for TLVC ethernet interface IO.
 *
 * Feb 2023 Helge Skrivervik
 */

#define NET_OBUFCNT 2
#define NET_IBUFCNT 2

#ifndef __ASSEMBLER__

#include <linuxmt/limits.h>

struct netbuf {
	int len;			/* ZERO if available */
	struct netbuf *next;
	char data[MAX_PACKET_ETH];	/* full size for now */
};

#if (NET_IBUFCNT > 0)
struct netbuf net_ibuf[NET_IBUFCNT];
struct netbuf *rnext;
#endif

#if (NET_OBUFCNT > 0)
struct netbuf net_obuf[NET_OBUFCNT];
struct netbuf *tnext;
#endif

#endif /* ASSEMBLER */

#endif
