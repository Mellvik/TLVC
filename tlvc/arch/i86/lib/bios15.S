####################################################################################
# BIOS INT 15H Block Move
# Nov 2021 Greg Haerr
#
# int block_move(struct gdt_table *gdtp, size_t words)
#
#	Move words to/from anywhere in physical memory using BIOS INT 15h AH=87h.
#	Normally used to move data to/from extended memory (> 1M) because
#	real mode addressing can only address the first 1M of RAM, unless
#	using unreal mode. Protected mode is used for the copy, and interrupts
#	are disabled the whole time. Returns 0 on success.
#
#	This is a substitute function for linear32_fmemcpyw which requires unreal mode,
#	and uses a int15_fmemcpy wrapper above it to prefill the passed GDT table.
#

#include <linuxmt/config.h>

	.arch	i286, nojumps
	.code16
	.section .fartext.far

	.global	block_move
	.global enable_a20_gate

# IBM PC A20 gate functions shared with setup.S
#include "a20.inc"

#
# int FARPROC block_move(struct gdt_table *gdtp, size_t words)
# Uses BIOS INT 15h AH=87h Block Move
# Enables A20 gate on return in case kernel in HMA. COmpletely 
# disables all interrupts by masking of the interrupt controllers
# and turning off the NMI for the duration of the transfer.
# This is likely to create havoc of the transfers are 'big' - as
# in 'many k bytes'. 1K (one disk block) per transfer seems to work 
# fine. May also be system speed dependent. Tested on 12.5MHz 286
#
block_move:
	push	%es
	push	%si
	push	%bp
	mov	%sp,%bp

	mov	12(%bp),%cx	# word count -> CX
	mov	10(%bp),%si	# gdtp -> ES:SI
	push	%ds
	pop	%es

#ifdef CONFIG_FS_XMS_INT15DI
	//mov	$0x80,%al	// disable NMI, may not be required
	//out	%al,$0x70
	in	$0x21,%al	// disable interrupt controller
	xchg	%al,%ah
	in	$0xA1,%al
	push	%ax
	mov	0xff,%al
	out	%al,$0x21
	out	%al,$0xA1	//--------
#endif
	xor	%al,%al		// doesn't matter, but this is 'clean'
	mov	$0x87,%ah	# BIOS Block Move
	int	$0x15
	jnc	1f
	mov	$-1,%ax		# fail return AX < 0
	jmp	2f
1:	xor	%ax,%ax		# success return AX = 0
2:
	push	%ax
	push	%cs
	call	enable_a20_gate
	pop	%bx

#ifdef CONFIG_FS_XMS_INT15DI
	//xor	%al,%al
	//out	%al,$0x70	# re-enable NMI
	pop	%ax		# get int mask back
	out	%al,$0xA1	# re-enable interrupt controllers
	xchg	%al,%ah
	out	%al,$0x21	//---------------
#endif
	mov	%bx,%ax		# get return code from INT15
	pop	%bp
	pop	%si
	pop	%es
	lret

