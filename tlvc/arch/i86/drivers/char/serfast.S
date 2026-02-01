// Fast custom serial COM interrupt routine for ELKS/TLVC
//
// runs on any stack and skips all OS overhead
// must run with interrupts disabled as could interrupt user, kernel or interrupt stack
// reads character into ring buffer
// timer interrupt runs serial_bh() which checks for non-zero queues and calls wake_up
//
// 25 June 2020 Greg Haerr
//
	.code16
	.text

//
// fast interrupt routine for serial ports, uses "jmp _irq_comX" from interrupt vector
//
	.extern	fast_com_irq
	.global	_irq_com1
	.global	_irq_com2
	.global	_irq_com3
	.global	_irq_com4

_irq_com1:
	push	%ax			// save regs, uses 18 bytes of current stack
	mov	$0,%ax			// pass port numer to the next level
irq_common:
	push	%bx
	push	%cx
	push	%dx
	push	%ds

	// Recover kernel data segment
	// Was pushed by the CALLF of the dynamic handler
	mov	%sp,%bx
	mov	%ss:12(%bx),%ds
	call	fast_com_irq		// call special C interrupt routine
					// which doesn't use any SS/SP/BP addressing
	mov	$0x20,%al		// EOI on primary controller
	out	%al,$0x20

	pop	%ds			// restore regs
	pop	%dx
	pop	%cx
	pop	%bx
	pop	%ax
	add	$4,%sp			// skip the trampoline DS:*irq
	iret

_irq_com2:
	push	%ax			// save regs, uses 18 bytes of current stack
	mov	$1,%ax
	jmp	irq_common

_irq_com3:
	push	%ax			// save regs, uses 18 bytes of current stack
	mov	$2,%ax
	jmp	irq_common

_irq_com4:
	push	%ax			// save regs, uses 18 bytes of current stack
	mov	$3,%ax
	jmp	irq_common

