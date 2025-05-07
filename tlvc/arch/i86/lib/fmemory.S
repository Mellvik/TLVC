// fmemcpy[bw], fmemset[bw], fmemcmp[bw] routines
//
// segment parameter after offset to allow LDS/LES from the stack
// assume DS=SS, save ES, for GCC-IA16

#include <linuxmt/config.h>

#define ARG0	2
#define ARG1	4
#define ARG2	6
#define ARG3	8
#define ARG4	10

	.arch	i8086, nojumps
	.code16
	.text

	.global fmemcpyb
	.global fmemcpyw
	.global fmemsetb
	.global fmemsetw
	.global fmemcmpb
	.global fmemcmpw

// void fmemcpyb (void * dst_off, seg_t dst_seg, void * src_off, seg_t src_seg,
//		size_t count)

fmemcpyb:
	mov    %si,%ax
	mov    %di,%dx
	mov    %sp,%si
	mov    %es,%bx
	mov    ARG4(%si),%cx  // byte count
	les    ARG0(%si),%di  // far destination pointer
	lds    ARG2(%si),%si  // far source pointer
	cld
	shr    $1,%cx         // copy words
	rep
	movsw
	rcl    $1,%cx         // then possibly final byte
	rep
	movsb
	mov    %bx,%es
	mov    %ax,%si
	mov    %dx,%di
	mov    %ss,%ax
	mov    %ax,%ds
	ret

// void fmemcpyw (void * dst_off, seg_t dst_seg, void * src_off, seg_t src_seg,
//		size_t count)

fmemcpyw:
	mov    %es,%bx
	mov    %si,%ax
	mov    %di,%dx
	mov    %sp,%si
	mov    ARG4(%si),%cx  // word count
	les    ARG0(%si),%di  // far destination pointer
	lds    ARG2(%si),%si  // far source pointer
	cld
	rep
	movsw
	mov    %ax,%si
	mov    %dx,%di
	mov    %ss,%ax
	mov    %ax,%ds
	mov    %bx,%es
	ret

// void fmemsetb (void * off, seg_t seg, byte_t val, size_t count)
// compiler pushes byte_t as word_t

fmemsetb:
	mov    %di,%dx
	mov    %sp,%di
	mov    %es,%bx
	mov    ARG2(%di),%ax  // value
	mov    ARG3(%di),%cx  // byte count
	les    ARG0(%di),%di  // far pointer
	cld
	shr    $1,%cx         // store words
	mov    %al,%ah
	rep
	stosw
	rcl    $1,%cx         // then possibly final byte
	rep
	stosb
	mov    %bx,%es
	mov    %dx,%di
	ret

// void fmemsetw (void * off, seg_t seg, word_t val, size_t count)

fmemsetw:
	mov    %es,%bx
	mov    %di,%dx
	mov    %sp,%di
	mov    ARG2(%di),%ax  // value
	mov    ARG3(%di),%cx  // byte count
	les    ARG0(%di),%di  // far pointer
	cld
	rep
	stosw
	mov    %dx,%di
	mov    %bx,%es
	ret

// int fmemcmpb (void * dst_off, seg_t dst_seg, void * src_off, seg_t src_seg,
//		size_t count)

fmemcmpb:
	mov    %es,%bx
	mov    %si,%ax
	mov    %di,%dx
	mov    %sp,%si
	mov    ARG4(%si),%cx  // byte count
	les    ARG0(%si),%di  // far destination pointer
	lds    ARG2(%si),%si  // far source pointer
	cld
	repz
	cmpsb
	mov    %ax,%si
	mov    %dx,%di
	jz     fmemcmpb_same
	mov    $1,%ax
	jmp    fmemcmpb_exit

fmemcmpb_same:
	xor    %ax,%ax
fmemcmpb_exit:
	mov    %ss,%dx
	mov    %dx,%ds
	mov    %bx,%es
	ret

// int fmemcmpw (void * dst_off, seg_t dst_seg, void * src_off, seg_t src_seg,
//		size_t count)

fmemcmpw:
	mov    %es,%bx
	mov    %si,%ax
	mov    %di,%dx
	mov    %sp,%si
	mov    ARG4(%si),%cx  // byte count
	les    ARG0(%si),%di  // far destination pointer
	lds    ARG2(%si),%si  // far source pointer
	cld
	repz
	cmpsw
	mov    %ax,%si
	mov    %dx,%di
	jz     fmemcmpw_same
	mov    $1,%ax
	jmp    fmemcmpw_exit

fmemcmpw_same:
	xor    %ax,%ax
fmemcmpw_exit:
	mov    %ss,%dx
	mov    %dx,%ds
	mov    %bx,%es
	ret

#ifdef CONFIG_FS_XMS_LOADALL

	.arch	i286, nojumps

	.section .fartext.far
	.global loadall_block_move

# XMS block move for 286 processors using the LOADALL instruction.
# Based on code from @drachen6jp for ELKS
#
# int FARPROC loadall_block_move(addr_t src, addr_t dst, size_t bytes)
#
# Prepare a loadall register block which changes the DS cache entry to
# point to the source address of the move, while the ES cache entry 
# points to the destination address. Either can be anywhere in the 16M
# address space. Filling the other two segment register caches (CS, SS)
# is also required since this is what the processor will be running on
# after the LOADALL instruction has been executed. 
# The segment caches in the processor will be individually reset to 
# 'normal' real mode values (high 4 bits clear) whenever the 
# corresponding register is changed by the running code. 
# Since this method copies the current SS and CS values into the corresponding
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

	xor	%di,%di

	cld
	mov	$0x80,%cx
	mov	%cx,%es		# ES=0080h segment
	mov	$0x33,%cx	# 102 bytes
	xor	%ax,%ax
	cli
	rep
	stosw

	xor	%di,%di
	dec	%ax		# limit is always 0xffff
	mov	%ax,%es:(0x3a)	# ES limit
	mov	%ax,%es:(0x4c)	# DS limit
	mov	%ax,%es:(0x40)	# CS limit
	mov	%ax,%es:(0x46)	# SS limit
	mov	%al,%es:(0x5f)	# IDT - important

	# if highword(src) = 0xffff, do memset 
# Prepare src (DS)
	xor	%bx,%bx
	mov	12(%bp),%ax	#src addr low16
	mov	%ax,%es:(0x48)	#new DS low16
	mov	14(%bp),%ax	#src addr high8
	cmp	$0xffff,%ax
	jnz	1f
	mov	$1,%bl		# memset flag
	mov	%bl,%al		# make the high byte inoccuous
				# not used when doing a memset
1:	mov	$0x93,%ah	# access byte
	mov	%ax,%es:(0x4a)	# new DS high8 & access byte
	
# Prepare dst (ES)
	mov	16(%bp),%ax	#dst addr low16
	mov	%ax,%es:(0x36)	#new ES low16
	mov	18(%bp),%ax	#dst addr high8 
	mov	$0x93,%ah	# & access byte
	mov	%ax,%es:(0x38)	#new ES high8 & access byte

# code segment cache entry
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

	cmp	$1,%bl		# check if memset
	jnz	1f
	mov	12(%bp),%ax		# get filler value
	movw	$memset,%es:(0x1a)	# run memset
	jmp	2f
1:	movw	$copyblk,%es:(0x1a)	# run block transfer

2:	mov	%ds,%es:(0x1e)
	mov	%ss,%es:(0x20)
	mov	%cs,%es:(0x22)
	mov	%es,%es:(0x24)
	mov	%sp,%es:(0x2c)
	//mov	%di,%es:(0x26)	# conveniently left at zero
	//mov	%si,%es:(0x28)	# conveniently left at zero
	//mov	%bp,%es:(0x2a)
	//mov	%bx,%es:(0x2e)
	//mov	%dx,%es:(0x30)
	mov	%cx,%es:(0x32)	# counter
	mov	%ax,%es:(0x34)	# needed with memset
exe:
	.byte 0x0f,0x05		#loadall

modechange2:
	retf		# use the push %cs above
memset:
	shr	$1,%cx
	rep
	stosw
	rcl	$1,%cx
	rep
	stosb
	jmp	done
copyblk:
	shr	$1,%cx	# make words
	rep
	movsw		# do the block move
	rcl	$1,%cx
	rep
	movsb		# get the final byte if any

done:	call	modechange2	#reset CS
	pop	%ax		#reset SS
	mov	%ax,%ss
	pop	%ax		#reset DS
	mov	%ax,%ds
	sti

	pop	%bp
	pop	%di
	pop	%si
	pop	%es
	xor	%ax,%ax		#all success
	lret
#endif
