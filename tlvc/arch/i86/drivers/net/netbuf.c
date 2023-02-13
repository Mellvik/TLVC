/*
 * Initialize IO buffers 
 */

#include "netbuf.h"

struct netbuf * netbuf_init(struct netbuf *buf, int cnt) {
	int i;
	for (i = 0; i < cnt; i++) {
		buf[i].len = 0;
		buf[i].next = &buf[i+1];
	}
	buf[--i].next = &buf[0];
	return(&buf[0]);
}
