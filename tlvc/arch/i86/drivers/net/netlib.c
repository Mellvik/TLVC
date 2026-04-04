/*
 * Initialize IO buffers 
 */

#include "netlib.h"
#include <linuxmt/kernel.h>
#include <linuxmt/heap.h>

struct netbuf *netbuf_init(struct netbuf *buf, int cnt) {
	int i;

	if (!cnt) return(NULL);
	for (i = 0; i < cnt; i++) {
#if NET_BUF_STRAT == HEAP_BUFS 
		if (!(buf[i].data = heap_alloc(MAX_PACKET_ETH, HEAP_TAG_NETWORK))) {
			printk("eth: Buffer alloc failed\n");
			return(NULL);
		}
		//printk("netbuf got %x\n", buf[i].data);
#endif
		buf[i].len = 0;
		buf[i].next = &buf[i+1];
	}
	buf[--i].next = &buf[0];
	return(&buf[0]);
}

#if NET_BUF_STRAT == HEAP_BUFS 
void netbuf_release(struct netbuf *buf) {
	struct netbuf *n = buf;

	//if (buf == NULL) return;
	do {
		heap_free(n->data);
		//printk("netbuf rel %x\n", n->data);
		n = n->next;
	} while (n != buf);
}
#endif

#if NET_BUF_STRAT == PAGE_BUFS

struct page *pbuf_init(struct pbuf *p)
{
	if (!(p->base = heap_alloc(PBUF_COUNT<<8, HEAP_TAG_NETWORK))) {
		printk("eth: Page buffer alloc failed\n");
		return NULL;
	}
	p->head = 1;
	p->tail = 0;
	p->top = p->base + (PBUF_COUNT<<8);
	return p->base;
}
#endif
