/*
 * v7.h - constants and more related to running V7 (Venix) binaries
 * on TLVC.
 * Helge Skrivervik, 2026
 */

#define OMAGIC	0x107	/* tiny model binary */
#define NMAGIC	0x109	/* small model, stack usually @ bottom of DS */
			/* may include code mapping for large text */

struct v7_exec {
short		a_magic;	/* magic number */
unsigned short	a_stack;	/* size of stack if Z type, 0 otherwise */
long		a_text;		/* size of text segment */
long		a_data;		/* size of initialized data */
long		a_bss;		/* size of uninitialized data */
long		a_syms;		/* size of symbol table */
long		a_entry;	/* entry point */
long		a_trsize;	/* size of text relocation */
long		a_heap;		/* heap size unless default (max) */
				/* was a_drsize, size of data relocation */
};

