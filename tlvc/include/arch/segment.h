#ifndef __ARCH_8086_SEGMENT_H
#define __ARCH_8086_SEGMENT_H

#include <linuxmt/types.h>

extern __u16 kernel_cs, kernel_ds;

extern short *_endtext, *_endftext, *_enddata, *_endbss;
extern short endistack[], istack[];
extern unsigned int heapsize;

#endif /* !__ARCH_8086_SEGMENT_H */
