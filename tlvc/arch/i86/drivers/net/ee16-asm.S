//
// 
// Low level routines for TLVC EtherExpress16 driver. 
// Copied from the EtherLink III driver.
// FIXME: merge these into a net-lib asm file.
//
// Helge Skrivervik (@mellvik) april 2024
//

#include <linuxmt/config.h>
#include "arch/ports.h"
#include <arch/asm-offsets.h>

	.code16

	.data
	.extern current


	.text

//-----------------------------------------------------------------------------
// Read data into process memory from port
// Handles words only - for now. Count is bytes
//-----------------------------------------------------------------------------
// void ee16_insw(int port, char *data, int count)
//

	.global ee16_insw

ee16_insw:

	push    %di
	mov	%sp,%di
	push	%es

	mov	4(%di),%dx	// Port
	mov	8(%di),%cx	// byte count
	mov	6(%di),%di	// Buffer pointer

	inc	%cx
	shr	%cx		// make word count
	cld
        mov	current,%bx
        mov	TASK_USER_DS(%bx),%es	// destination segment = user process
	//cli

word_loop:
        in	%dx,%ax
        stosw
        loop	word_loop

	//sti
        pop     %es
        pop     %di

        ret


//-----------------------------------------------------------------------------
// Write data buffer to port
// Words only - will not work bit 8bit bus for now
//-----------------------------------------------------------------------------
// void ee16_outsw(int port, char *data, seg_t seg, int count)
//

	.global ee16_outsw

ee16_outsw:
        push    %si
	mov	%sp,%si
	push	%ds

	mov	4(%si),%dx	// Port
        mov     10(%si),%cx	// byte count
	lds	6(%si),%si	// buf -> %si, seg -> %ds

	inc	%cx
	shr	%cx		// make word count
	cld
	//cli

wr_loop:
	lodsw
	out     %ax,%dx
	loop	wr_loop

	//sti
	pop     %ds
	pop	%si

	ret

//-------------------------------------------------
	.global ee16_udelay

ee16_udelay:

	push	%bp
	mov	%sp,%bp
	mov	4(%bp),%cx	// count
udly_loop:
	outb	$0x80		// presumably approx 1us
	loop	udly_loop

	pop	%bp
	ret

