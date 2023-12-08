/*
 * Initialize IO buffers 
 */

#include "netbuf.h"
#include <linuxmt/kernel.h>
#include <linuxmt/heap.h>

struct netbuf *netbuf_init(struct netbuf *buf, int cnt) {
	int i;
	for (i = 0; i < cnt; i++) {
#ifdef USE_HEAP_BUFFER
		buf[i].data = NULL;
		if (!(buf[i].data = heap_alloc(MAX_PACKET_ETH, HEAP_TAG_BUFHEAD))) {
			printk("eth: Buffer alloc failed\n");
			netbuf_release(&buf[0]);
			return(NULL);
		}
		printk("netbuf got %x\n", buf[i].data);
#endif
		buf[i].len = 0;
		buf[i].next = &buf[i+1];
	}
	buf[--i].next = &buf[0];
	return(&buf[0]);
}

#ifdef USE_HEAP_BUFFER
void netbuf_release(struct netbuf *buf) {
	struct netbuf *n = buf;
	do {
		heap_free(n->data);
		printk("netbuf rel %x\n", n->data);
		n = n->next;
	} while (n != buf);
}
#endif
