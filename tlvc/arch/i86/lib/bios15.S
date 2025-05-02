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
	.global loadall_block_move

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


#ifdef CONFIG_FS_XMS_LOADALL
#  EXPERIMENTAL -- based on code from @drachen6jp for ELKS
# int FARPROC loadall_block_move(addr_t src, addr_t dst, size_t bytes)
#
# Prepare a loadall register block which changes the DS cache entry to
# point to the source address of the move, while the ES cache entry 
# points to the destination address. Either can be anywhere in the 16M
# address space. Filling the other two segment register caches (CS, SS)
# is also required since this is what the processor will be running on
# after the LOADALL instruction has been executed. 
# The caches in the processor will be individually reset to 'normal' real
# mode values (high 4 bits clear) whenever the corresponding register is
# changed in the code. 
# Since this metnhod copies the current SS and CS values into the corresponding
# caches, resetting them after is not critical. What's needed is
# that the DS and ES registers get updated (restored) before we exit.
#
# The return value is usually ignored, but may be useful for debugging.

loadall_block_move:

	push	%es
	push	%si
	push	%di
	push	%bp
	mov	%sp,%bp

	//xor	%si,%si
	xor	%di,%di

	cld
	mov	$0x80,%cx
	mov	%cx,%es		#ES=0080h segment
	//mov	$0x80,%cx
	xor	%ax,%ax
	rep
	stosw			#800h-880h clear

	//xor	%di,%di
	dec	%ax		# limit is always 0xffff
	mov	%ax,%es:(0x3a)	# ES limit
	mov	%ax,%es:(0x4c)	# DS limit
	mov	%ax,%es:(0x40)	# CS limit
	mov	%ax,%es:(0x46)	# SS limit
	mov	%al,%es:(0x5f)	# IDT - important

#src DS
	mov	12(%bp),%ax	#src addr low16
	mov	%ax,%es:(0x48)	#new DS low16
	mov	14(%bp),%ax	#src addr high8
	mov	$0x93,%ah	# & access byte
	mov	%ax,%es:(0x4a)	#new DS high8 & access byte
	
#dst ES
	mov	16(%bp),%ax	#dst addr low16
	mov	%ax,%es:(0x36)	#new ES low16
	mov	18(%bp),%ax	#dst addr high8 
	mov	$0x93,%ah	# & access byte
	mov	%ax,%es:(0x38)	#new ES high8 & access byte

# code segment cache entry
	cli
	mov	%cs,%ax
	mov	$12,%cl
	shr	%cl,%ax
	mov	%al,%es:(0x3e)	#CS high8
	mov	%cs,%ax
	mov	$4,%cl
	shl	%cl,%ax
	mov	%ax,%es:(0x3c)	#CS low16
	mov	$0x9a93,%ax
	mov	%ah,%es:(0x3f)	#cs access byte
	mov	%al,%es:(0x45)	#ss access byte

# stack segment cache entry
	mov	%ss,%ax
	mov	$12,%cl
	shr	%cl,%ax
	mov	%al,%es:(0x44)	#SS high8
	mov	%ss,%ax
	mov	$4,%cl
	shl	%cl,%ax
	mov	%ax,%es:(0x42)	#SS low16

	mov	20(%bp),%cx	# byte count

	push	%ds
	push	%ss		# needed for seg reg cache reset
	push	%cs		# after the transfer

	//push	%cs
	//call	modechange2
	//mov	%sp,%bp
	//mov	-6(%bp),%ax
	//add	$2,%ax
	//mov	%ax,%es:(0x1a)	#new IP
	movw	$modechange,%es:(0x1a)	# where to start after loadall
	//mov	%ax,%bx
	//sub	$7,%bx
	//mov	$0x050f,%ax		#loadall
	//mov	%ax,%cs:(%bx)
	//pushf			# no flags needed, zero is OK (IF == off)
	//pop	%ax
	//mov	%ax,%es:(0x18)
	//smsw	%es:(0x06)	# not useful

	mov	%ds,%es:(0x1e)
	mov	%ss,%es:(0x20)
	mov	%cs,%es:(0x22)
	mov	%es,%es:(0x24)
	//mov	%di,%es:(0x26)	# conveniently left at zero
	//mov	%si,%es:(0x28)	# conveniently left at zero
	//mov	%bp,%es:(0x2a)
	mov	%sp,%es:(0x2c)
	mov	%bx,%es:(0x2e)
	mov	%dx,%es:(0x30)
	mov	%cx,%es:(0x32)
	//mov	%ax,%es:(0x34)
exe:
	.byte 0x0f	#loadall
	.byte 0x05
modechange2:
	//call	modechange3
	retf		# use the push %cs above
modechange3:
	//ret
modechange:
	shr	$1,%cx	# make words
	rep
	movsw		# do the block move
	rcl	$1,%cx
	rep
	movsb		# get the final byte if any

	call	modechange2	#reset CS
	pop	%ax		#reset SS
	mov	%ax,%ss
	pop	%ax		#reset DS
	mov	%ax,%ds
	//xor	%ax,%ax
	//mov	%ax,%es		#reset %es
	//push	%cs	# use the %cs push from above
	//call	enable_a20_gate	# required

	sti

	pop	%bp
	pop	%di
	pop	%si
	pop	%es
	xor	%ax,%ax		#all success
	lret
#endif

#if UNUSED
#
# Test functions
#
int0Dh:	push	%bp
	push	%ax
	mov	%sp,%bp
	mov	4(%bp),%ax
	inc	%ax
	//inc	%ax
	mov	%ax,4(%bp)
	pop	%ax
	pop	%bp
	iret

setgpf:	push	%ds
	xor	%ax,%ax
	mov	%ax,%ds
	mov	$0x0D*4,%bx
	mov	$int0Dh,%ax
	mov	%ax,(%bx)
	add	$2,%bx
	push	%cs
	pop	%ax
	mov	%ax,(%bx)
	pop	%ds
	ret

	.global	get_extmem
get_extmem:
	mov	$0x88,%ah
	int	$0x15
	jc	1f
	ret
1:	mov	$-1,%ax
	ret
#endif
