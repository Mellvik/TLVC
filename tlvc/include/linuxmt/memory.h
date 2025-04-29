#ifndef __LINUXMT_MEMORY_H
#define __LINUXMT_MEMORY_H

/* memory primitives */

#include <linuxmt/types.h>
#include <linuxmt/init.h>

byte_t peekb (word_t off, seg_t seg);
word_t peekw (word_t off, seg_t seg);
long_t peekl (word_t off, seg_t seg);

byte_t setupb (word_t);		/* Get data from setup segment */
word_t setupw (word_t);

void pokeb (word_t off, seg_t seg, byte_t val);
void pokew (word_t off, seg_t seg, word_t val);
void pokel (word_t off, seg_t seg, long_t val);

void fmemsetb (void * off, seg_t seg, byte_t val, size_t count);
void fmemsetw (void * off, seg_t seg, word_t val, size_t count);

void fmemcpyb (void * dst_off, seg_t dst_seg, void * src_off, seg_t src_seg, size_t count);
void fmemcpyw (void * dst_off, seg_t dst_seg, void * src_off, seg_t src_seg, size_t count);

word_t fmemcmpb (void * dst_off, seg_t dst_seg, void * src_off, seg_t src_seg, size_t count);
word_t fmemcmpw (void * dst_off, seg_t dst_seg, void * src_off, seg_t src_seg, size_t count);

/* macros for far pointers (a la Turbo C++ and Open Watcom) */
#define _FP_SEG(fp)     ((unsigned)((unsigned long)(void __far *)(fp) >> 16))
#define _FP_OFF(fp)     ((unsigned)(unsigned long)(void __far *)(fp))
#define _MK_FP(seg,off)	((void __far *)((((unsigned long)(seg)) << 16) | (off)))
#define _MK_LINADDR(seg, offs) ((unsigned long)((((unsigned long)(seg)) << 4) + (unsigned)(offs)))

/* unreal mode, A20 gate management */
int check_unreal_mode(void);	/* check if unreal mode capable, returns > 0 on success */
void enable_unreal_mode(void);	/* requires 386+ CPU to call */
/* enable_a20_gate/block_move must be FARPROC INT since 15/1F disables A20 w/HMA kernel */
int FARPROC enable_a20_gate(void);	/* returns 0 on fail */
struct gdt_table;
int FARPROC block_move(struct gdt_table *gdtp, size_t words); /* use INT 15/1F */

/* XMS memory management */
/* possible values for xms_mode */
#define XMS_DISABLED	0	/* Off */
#define XMS_UNREAL	1	/* using unreal mode and linear32_fmemcpy for block moves */
#define XMS_INT15	2	/* using BIOS INT 15 block move */
extern int xms_mode;

#ifdef CONFIG_FS_XMS_BUFFER
typedef __u32 ramdesc_t;	/* special physical ram descriptor */

/* allocate from XMS memory */
void xms_init(void);		/* enables xms and A20 gate if present */
ramdesc_t xms_alloc(int size);

/* copy to/from XMS or far memory - XMS requires unreal mode and A20 gate enabled */
void xms_fmemcpyw(void *dst_off, ramdesc_t dst_seg, void *src_off, ramdesc_t src_seg,
		size_t count);
void xms_fmemcpyb(void *dst_off, ramdesc_t dst_seg, void *src_off, ramdesc_t src_seg,
		size_t count);
void xms_fmemset(void *dst_off, ramdesc_t dst_seg, byte_t val, size_t count);

/* low level copy - must have 386 CPU and xms_enabled before calling! */
void linear32_fmemcpyw(void *dst_off, addr_t dst_seg, void *src_off, addr_t src_seg,
		size_t count);
void linear32_fmemcpyb(void *dst_off, addr_t dst_seg, void *src_off, addr_t src_seg,
		size_t count);
void linear32_fmemset(void *dst_off, addr_t dst_seg, byte_t val, size_t count);
unsigned int linear32_peekw(void *, addr_t);
void linear32_pokew(void *, addr_t, unsigned);
#else

typedef seg_t ramdesc_t;	/* ramdesc_t is just a regular segment descriptor */
#define xms_fmemcpyw	fmemcpyw
#define xms_fmemcpyb	fmemcpyb
#define xms_fmemset     fmemsetb

#endif /* CONFIG_FS_XMS_BUFFER */

#endif
