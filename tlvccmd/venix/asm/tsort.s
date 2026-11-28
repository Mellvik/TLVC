/* tsort.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 tsort.s -o tsort.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: tsort  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x000021ce  (8654 bytes)
//   a_data:   0x0000033c  (828 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6a,%sp
L_0045:
	movw   $D_0292,-0x6(%bp)
	cmpw   $0x1,0x4(%bp)
	jle   L_0077
	mov    $D_000f,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	call   L_03b2
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_0077
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	mov    $D_0011,%ax
	push   %ax
	call   L_027b
	add    $0x4,%sp
L_0077:
	lea    -0x6e(%bp),%ax
	push   %ax
	lea    -0x3c(%bp),%ax
	push   %ax
	mov    $D_001e,%ax
	push   %ax
	pushw  -0x6(%bp)
	call   L_050a
	add    $0x8,%sp
	mov    %ax,-0xa(%bp)
	cmp    $0xffff,%ax
	jne   L_0097
	jmp   L_0136
L_0097:
	cmpw   $0x2,-0xa(%bp)
	je   L_00ab
	pushw  D_000c
	mov    $D_0023,%ax
	push   %ax
	call   L_027b
	add    $0x4,%sp
L_00ab:
	lea    -0x3c(%bp),%ax
	push   %ax
	call   L_019a
	pop    %cx
	mov    %ax,%si
	lea    -0x6e(%bp),%ax
	push   %ax
	call   L_019a
	pop    %cx
	mov    %ax,-0x8(%bp)
	cmp    %ax,%si
	je   L_0077
	pushw  -0x8(%bp)
	push   %si
	call   L_013f
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0077
	mov    $0x4,%ax
	push   %ax
	call   L_1e0e
	pop    %cx
	mov    %ax,%di
	mov    -0x8(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,(%di)
	mov    %si,0x2(%di)
	mov    -0x8(%bp),%bx
	mov    %di,0x2(%bx)
	jmp   L_0077
L_00ef:
	movw   $0x0,-0xa(%bp)
	mov    $D_0004,%si
L_00f7:
	cmpw   $0x0,(%si)
	je   L_0114
	cmpw   $0x1,0x6(%si)
	jne   L_0110
	movw   $0x1,-0xa(%bp)
	push   %si
	call   L_016c
	pop    %cx
	or     %ax,%ax
	je   L_0114
L_0110:
	mov    (%si),%si
	jmp   L_00f7
L_0114:
	cmpw   $0x0,-0xa(%bp)
	je   L_0138
	cmpw   $0x0,(%si)
	jne   L_0124
	call   L_02c3
	mov    %ax,%si
L_0124:
	pushw  0x4(%si)
	mov    $D_002c,%ax
	push   %ax
	call   L_04c6
	add    $0x4,%sp
	movw   $0x0,0x6(%si)
L_0136:
	jmp   L_00ef
L_0138:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_013f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_014b:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%di
L_0151:
	or     %di,%di
	je   L_0166
	mov    0x2(%di),%ax
	cmp    0x4(%bp),%ax
	jne   L_0162
	mov    $0x1,%ax
	jmp   L_0168
L_0162:
	mov    (%di),%di
	jmp   L_0151
L_0166:
	xor    %ax,%ax
L_0168:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_016c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0178:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%di
L_017e:
	or     %di,%di
	je   L_0194
	mov    0x2(%di),%bx
	cmpw   $0x1,0x6(%bx)
	jne   L_0190
	mov    $0x1,%ax
	jmp   L_0196
L_0190:
	mov    (%di),%di
	jmp   L_017e
L_0194:
	xor    %ax,%ax
L_0196:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_019a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_01a7:
	mov    0x4(%bp),%di
	mov    $D_0004,%si
L_01ae:
	cmpw   $0x0,(%si)
	je   L_01c9
	pushw  0x4(%si)
	push   %di
	call   L_0243
	add    $0x4,%sp
	or     %ax,%ax
	je   L_01c5
	mov    %si,%ax
	jmp   L_023e
L_01c5:
	mov    (%si),%si
	jmp   L_01ae
L_01c9:
	mov    %di,-0x6(%bp)
L_01cc:
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_01d9
	incw   -0x6(%bp)
	jmp   L_01cc
L_01d9:
	mov    -0x6(%bp),%ax
	inc    %ax
	sub    %di,%ax
	push   %ax
	call   L_1e0e
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    $0x8,%ax
	push   %ax
	call   L_1e0e
	pop    %cx
	mov    %ax,(%si)
	or     %ax,%ax
	je   L_01fb
	cmpw   $0x0,-0x6(%bp)
	jne   L_0209
L_01fb:
	pushw  D_000c
	mov    $D_0030,%ax
	push   %ax
	call   L_027b
	add    $0x4,%sp
L_0209:
	mov    -0x6(%bp),%ax
	mov    %ax,0x4(%si)
	movw   $0x1,0x6(%si)
	mov    (%si),%bx
	movw   $0x0,(%bx)
	mov    (%si),%bx
	movw   $0x0,0x2(%bx)
	mov    (%si),%bx
	movw   $0x0,0x6(%bx)
L_0228:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0228
	mov    %si,%ax
L_023e:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0243:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_024f:
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_0257:
	mov    (%di),%al
	cbtw
	mov    (%si),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_0275
	cmpb   $0x0,(%di)
	jne   L_0271
	mov    $0x1,%ax
	jmp   L_0277
L_0271:
	inc    %di
	inc    %si
	jmp   L_0257
L_0275:
	xor    %ax,%ax
L_0277:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_027b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0287:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_029f
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_029f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_02ab:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	mov    $D_003f,%ax
	push   %ax
	mov    $D_02a2,%ax
	push   %ax
	call   L_0386
	add    $0x8,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_02c3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_02d0:
	mov    $D_0004,%di
L_02d3:
	cmpw   $0x0,(%di)
	je   L_02e2
	cmpw   $0x1,0x6(%di)
	je   L_02e2
	mov    (%di),%di
	jmp   L_02d3
L_02e2:
	pushw  D_000c
	mov    $D_004c,%ax
	push   %ax
	call   L_029f
	add    $0x4,%sp
L_02f0:
	cmpw   $0x1,0x6(%di)
	jne   L_0325
	movw   $0x2,0x6(%di)
	mov    0x2(%di),%ax
	mov    %ax,-0x6(%bp)
L_0301:
	cmpw   $0x0,-0x6(%bp)
	jne   L_030f
	mov    $D_0063,%ax
	push   %ax
	call   L_027b
	pop    %cx
L_030f:
	mov    -0x6(%bp),%bx
	mov    0x2(%bx),%di
	cmpw   $0x0,0x6(%di)
	jne   L_02f0
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x6(%bp)
	jmp   L_0301
L_0325:
	cmpw   $0x2,0x6(%di)
	jne   L_0367
	movw   $0x3,0x6(%di)
	pushw  D_000c
	pushw  0x4(%di)
	call   L_029f
	add    $0x4,%sp
	mov    0x2(%di),%ax
	mov    %ax,-0x6(%bp)
L_0343:
	cmpw   $0x0,-0x6(%bp)
	jne   L_0351
	mov    $D_006b,%ax
	push   %ax
	call   L_027b
	pop    %cx
L_0351:
	mov    -0x6(%bp),%bx
	mov    0x2(%bx),%di
	cmpw   $0x0,0x6(%di)
	jne   L_0325
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x6(%bp)
	jmp   L_0343
L_0367:
	mov    $D_0004,%si
L_036a:
	cmpw   $0x0,(%si)
	je   L_037e
	cmpw   $0x0,0x6(%si)
	je   L_037a
	movw   $0x1,0x6(%si)
L_037a:
	mov    (%si),%si
	jmp   L_036a
L_037e:
	mov    %di,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0386:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_0f70
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_03ac
	mov    $0xffff,%ax
	jmp   L_03ae
L_03ac:
	xor    %ax,%ax
L_03ae:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_03b2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	cmpb   $0x0,D_0074
	jne   L_03ef
	movw   $D_02aa,-0x8(%bp)
	/* suppressed: movw $D_0332,D_0332 (self-ref in .data) */
	movw   $0x88,-0xa(%bp)
L_03d5:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_03ea
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_03d5
L_03ea:
	movb   $0x1,D_0074
L_03ef:
	movw   $D_0292,-0x6(%bp)
L_03f4:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_0413
	mov    -0x6(%bp),%ax
	cmp    D_0332,%ax
	jb   L_040d
	xor    %ax,%ax
	jmp   L_047e
L_040d:
	addw   $0x8,-0x6(%bp)
	jmp   L_03f4
L_0413:
	cmpb   $0x77,(%di)
	jne   L_0429
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_04b0
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_0468
L_0429:
	cmpb   $0x61,(%di)
	jne   L_046a
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_20a0
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_0457
	cmpw   $0x2,D_033e
	jne   L_0457
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_04b0
	add    $0x4,%sp
	mov    %ax,%si
L_0457:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_0f52
	add    $0x8,%sp
L_0468:
	jmp   L_0478
L_046a:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_20a0
	add    $0x4,%sp
	mov    %ax,%si
L_0478:
	or     %si,%si
	jge   L_0480
	xor    %ax,%ax
L_047e:
	jmp   L_04a8
L_0480:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_049e
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_04a5
L_049e:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_04a5:
	mov    -0x6(%bp),%ax
L_04a8:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_04b0:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall creat(8) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* creat -> __NR_creat */
	xchg	%ax,%bx
	mov	%dx,%cx
	mov	$__NR_creat,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_04c4
	neg	%ax
	mov	%ax,D_033e	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_04c4:
	pop    %bp
	ret


L_04c6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_029a,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0f70
	add    $0x6,%sp
	mov    D_02a0,%al
	cbtw
	test   $0x20,%ax
	je   L_04ea
	mov    $0xffff,%ax
	jmp   L_04ec
L_04ea:
	xor    %ax,%ax
L_04ec:
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	mov    $D_0292,%ax
	push   %ax
	call   L_0564
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_050a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_0564
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%di
	movb   $0x41,-0x6(%bp)
	mov    %di,-0x8(%bp)
	mov    %di,-0xc(%bp)
	movw   $0x0,-0xa(%bp)
L_053e:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_054b
	incw   -0xa(%bp)
	jmp   L_053e
L_054b:
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_0564
	add    $0x6,%sp
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0564:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xe,%sp
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x10(%bp)
L_057e:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	jmp   L_0728
L_058a:
	mov    -0x8(%bp),%ax
	jmp   L_0658
L_0590:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	cmp    $0x25,%ax
	jne   L_05a1
	jmp   L_0746
L_05a1:
	movw   $0x0,-0xe(%bp)
	cmpw   $0x2a,-0x6(%bp)
	je   L_05b5
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xe(%bp)
	jmp   L_05be
L_05b5:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
L_05be:
	movw   $0x0,-0xa(%bp)
	movw   $0x1,-0x12(%bp)
L_05c8:
	cmpw   $0x30,-0x6(%bp)
	jl   L_05f0
	cmpw   $0x39,-0x6(%bp)
	jg   L_05f0
	mov    -0xa(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0x6(%bp),%ax
	sub    $0x30,%ax
	mov    %ax,-0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	jmp   L_05c8
L_05f0:
	cmpw   $0x0,-0xa(%bp)
	jne   L_05fb
	movw   $0x7530,-0xa(%bp)
L_05fb:
	cmpw   $0x6c,-0x6(%bp)
	jne   L_0611
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	movw   $0x2,-0x12(%bp)
	jmp   L_0634
L_0611:
	cmpw   $0x68,-0x6(%bp)
	jne   L_0627
	movw   $0x0,-0x12(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	jmp   L_0634
L_0627:
	cmpw   $0x5b,-0x6(%bp)
	jne   L_0634
	push   %di
	call   L_0bfc
	pop    %cx
	mov    %ax,%di
L_0634:
	mov    -0x6(%bp),%bx
	mov    D_014d(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_064f
	mov    -0x6(%bp),%ax
	add    $0x20,%ax
	mov    %ax,-0x6(%bp)
	movw   $0x2,-0x12(%bp)
L_064f:
	cmpw   $0x0,-0x6(%bp)
	jne   L_065a
	mov    $0xffff,%ax
L_0658:
	jmp   L_0697
L_065a:
	lea    -0x10(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	pushw  -0x12(%bp)
	pushw  -0xa(%bp)
	pushw  -0x6(%bp)
	pushw  -0xe(%bp)
	call   L_074f
	add    $0xc,%sp
	or     %ax,%ax
	je   L_0680
	cmpw   $0x0,-0xe(%bp)
	je   L_0680
	incw   -0x8(%bp)
L_0680:
	cmpw   $0x0,-0x10(%bp)
L_0684:
	jne   L_0689
	jmp   L_057e
L_0689:
	cmpw   $0x0,-0x8(%bp)
	je   L_0694
	mov    -0x8(%bp),%ax
	jmp   L_0697
L_0694:
	mov    $0xffff,%ax
L_0697:
	jmp   L_0715
L_0699:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_06b3
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_06ba
L_06b3:
	pushw  0x4(%bp)
	call   L_0ea4
	pop    %cx
L_06ba:
	mov    %ax,-0xc(%bp)
	cmp    $0x20,%ax
	je   L_0699
	cmpw   $0x9,-0xc(%bp)
	je   L_0699
	cmpw   $0xa,-0xc(%bp)
L_06cc:
	je   L_0699
	cmpw   $0xffff,-0xc(%bp)
L_06d2:
	je   L_0684
	pushw  0x4(%bp)
	pushw  -0xc(%bp)
	call   L_211c
	add    $0x4,%sp
	jmp   L_057e
L_06e3:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_06fd
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0704
L_06fd:
	pushw  0x4(%bp)
	call   L_0ea4
	pop    %cx
L_0704:
	mov    %ax,-0xc(%bp)
	cmp    -0x6(%bp),%ax
	je   L_06d2
	cmpw   $0xffff,-0xc(%bp)
	jne   L_0717
	mov    $0xffff,%ax
L_0715:
	jmp   L_0748
L_0717:
	pushw  0x4(%bp)
	pushw  -0xc(%bp)
	call   L_211c
	add    $0x4,%sp
	mov    -0x8(%bp),%ax
	jmp   L_0748
L_0728:
	or     %ax,%ax
	jne   L_072f
	jmp   L_058a
L_072f:
	cmp    $0x9,%ax
L_0732:
	je   L_06cc
	cmp    $0xa,%ax
	je   L_0732
	cmp    $0x20,%ax
	je   L_0732
	cmp    $0x25,%ax
	jne   L_0746
	jmp   L_0590
L_0746:
	jmp   L_06e3
L_0748:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_074f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x63,0x6(%bp)
	je   L_0769
	cmpw   $0x73,0x6(%bp)
	je   L_0769
	cmpw   $0x5b,0x6(%bp)
	jne   L_078e
L_0769:
	pushw  0xe(%bp)
	pushw  0xc(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	cmpw   $0x0,0x4(%bp)
	je   L_0782
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	jmp   L_0784
L_0782:
	xor    %ax,%ax
L_0784:
	push   %ax
	call   L_0aee
	add    $0xa,%sp
	jmp   L_0a5c
L_078e:
	movw   $0x0,-0x58(%bp)
	movw   $0x0,-0x56(%bp)
	movw   $0x0,-0x52(%bp)
	movw   $0x0,-0x4c(%bp)
	cmpw   $0x65,0x6(%bp)
	je   L_07ae
	cmpw   $0x66,0x6(%bp)
	jne   L_07b3
L_07ae:
	movw   $0x1,-0x4c(%bp)
L_07b3:
	movw   $0xa,-0x48(%bp)
	cmpw   $0x6f,0x6(%bp)
	jne   L_07c5
	movw   $0x8,-0x48(%bp)
	jmp   L_07d0
L_07c5:
	cmpw   $0x78,0x6(%bp)
	jne   L_07d0
	movw   $0x10,-0x48(%bp)
L_07d0:
	lea    -0x44(%bp),%ax
	mov    %ax,%di
	movw   $0x0,-0x4a(%bp)
	movw   $0x0,-0x4e(%bp)
L_07df:
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_07f9
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0800
L_07f9:
	pushw  0xc(%bp)
	call   L_0ea4
	pop    %cx
L_0800:
	mov    %ax,-0x46(%bp)
	cmp    $0x20,%ax
	je   L_07df
	cmpw   $0x9,-0x46(%bp)
	je   L_07df
	cmpw   $0xa,-0x46(%bp)
	je   L_07df
	cmpw   $0x2d,-0x46(%bp)
	jne   L_0850
	incw   -0x4e(%bp)
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_0841
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0848
L_0841:
	pushw  0xc(%bp)
	call   L_0ea4
	pop    %cx
L_0848:
	mov    %ax,-0x46(%bp)
	decw   0x8(%bp)
	jmp   L_087d
L_0850:
	cmpw   $0x2b,-0x46(%bp)
	jne   L_087d
	decw   0x8(%bp)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_0873
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_087a
L_0873:
	pushw  0xc(%bp)
	call   L_0ea4
	pop    %cx
L_087a:
	mov    %ax,-0x46(%bp)
L_087d:
	decw   0x8(%bp)
	jge   L_0885
	jmp   L_09d9
L_0885:
	movw   $0x0,-0x54(%bp)
	cmpw   $0x30,-0x46(%bp)
	jl   L_089b
	cmpw   $0x39,-0x46(%bp)
	jg   L_089b
	movw   $0x1,-0x54(%bp)
L_089b:
	cmpw   $0x61,-0x46(%bp)
	jl   L_08a7
	cmpw   $0x7a,-0x46(%bp)
	jle   L_08b3
L_08a7:
	cmpw   $0x41,-0x46(%bp)
	jl   L_08be
	cmpw   $0x5a,-0x46(%bp)
	jg   L_08be
L_08b3:
	cmpw   $0x10,-0x48(%bp)
	jne   L_08be
	movw   $0x1,-0x54(%bp)
L_08be:
	cmpw   $0x0,-0x54(%bp)
	jne   L_08c7
	jmp   L_095b
L_08c7:
	incw   -0x52(%bp)
	cmpw   $0x8,-0x48(%bp)
	jne   L_08e1
	mov    $0x3,%cx
	or     %cx,%cx
	je   L_091b
L_08d7:
	shlw   -0x58(%bp)
	rclw   -0x56(%bp)
	loop   L_08d7
	jmp   L_091b
L_08e1:
	cmpw   $0xa,-0x48(%bp)
	jne   L_090c
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	mov    $0x2,%cx
	or     %cx,%cx
	je   L_08fa
L_08f4:
	shl    %ax
	rcl    %dx
	loop   L_08f4
L_08fa:
	add    -0x58(%bp),%ax
	adc    -0x56(%bp),%dx
	shl    %ax
	rcl    %dx
	mov    %ax,-0x58(%bp)
	mov    %dx,-0x56(%bp)
	jmp   L_091b
L_090c:
	mov    $0x4,%cx
	or     %cx,%cx
	je   L_091b
L_0913:
	shlw   -0x58(%bp)
	rclw   -0x56(%bp)
	loop   L_0913
L_091b:
	mov    -0x46(%bp),%ax
	mov    %ax,-0x50(%bp)
	cmpw   $0x30,-0x46(%bp)
	jl   L_0933
	cmpw   $0x39,-0x46(%bp)
	jg   L_0933
	subw   $0x30,-0x46(%bp)
	jmp   L_0949
L_0933:
	cmpw   $0x61,-0x46(%bp)
	jl   L_0945
	cmpw   $0x66,-0x46(%bp)
	jg   L_0945
	subw   $0x57,-0x46(%bp)
	jmp   L_0949
L_0945:
	subw   $0x37,-0x46(%bp)
L_0949:
	mov    -0x46(%bp),%ax
	cwtd
	add    %ax,-0x58(%bp)
	adc    %dx,-0x56(%bp)
	mov    -0x50(%bp),%ax
	mov    %ax,-0x46(%bp)
	jmp   L_0970
L_095b:
	cmpw   $0x2e,-0x46(%bp)
	jne   L_0972
	cmpw   $0xa,-0x48(%bp)
	jne   L_0988
	cmpw   $0x0,-0x4c(%bp)
	je   L_0997
	incw   -0x52(%bp)
L_0970:
	jmp   L_09e1
L_0972:
	cmpw   $0x65,-0x46(%bp)
	je   L_097e
	cmpw   $0x45,-0x46(%bp)
	jne   L_0988
L_097e:
	cmpw   $0x0,-0x4a(%bp)
	jne   L_0988
	cmpw   $0xa,-0x48(%bp)
L_0988:
	je   L_098d
	jmp   L_0a12
L_098d:
	cmpw   $0x0,-0x4c(%bp)
	je   L_0997
	cmpw   $0x0,-0x52(%bp)
L_0997:
	je   L_0a12
	incw   -0x4a(%bp)
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_09c0
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_09c7
L_09c0:
	pushw  0xc(%bp)
	call   L_0ea4
	pop    %cx
L_09c7:
	mov    %ax,-0x46(%bp)
	cmp    $0x2b,%ax
	je   L_09e1
	cmpw   $0x2d,-0x46(%bp)
	je   L_09e1
	cmpw   $0x30,-0x46(%bp)
L_09d9:
	jl   L_0a12
	cmpw   $0x39,-0x46(%bp)
	jg   L_0a12
L_09e1:
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_0a05
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0a0c
L_0a05:
	pushw  0xc(%bp)
	call   L_0ea4
	pop    %cx
L_0a0c:
	mov    %ax,-0x46(%bp)
	jmp   L_087d
L_0a12:
	cmpw   $0x0,-0x4e(%bp)
	je   L_0a2b
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	mov    %ax,-0x58(%bp)
	mov    %dx,-0x56(%bp)
L_0a2b:
	cmpw   $0xffff,-0x46(%bp)
	je   L_0a46
	pushw  0xc(%bp)
	pushw  -0x46(%bp)
	call   L_211c
	add    $0x4,%sp
	mov    0xe(%bp),%bx
	movw   $0x0,(%bx)
	jmp   L_0a4d
L_0a46:
	mov    0xe(%bp),%bx
	movw   $0x1,(%bx)
L_0a4d:
	cmpw   $0x0,0x4(%bp)
	je   L_0a5a
	lea    -0x44(%bp),%ax
	cmp    %ax,%di
	jne   L_0a5f
L_0a5a:
	xor    %ax,%ax
L_0a5c:
	jmp   L_0ae7
L_0a5f:
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	mov    -0x4c(%bp),%ax
	mov    $0x4,%cx
	shl    %cl,%ax
	or     0xa(%bp),%ax
	jmp   L_0ad8
	lea    -0x44(%bp),%ax
	push   %ax
	call   L_0c66
	fstpl  -0x60(%bp)
	pop    %cx
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	fldl   -0x60(%bp)
	fstps  (%bx)
	jmp   L_0ad6
	lea    -0x44(%bp),%ax
	push   %ax
	call   L_0c66
	fstpl  -0x60(%bp)
	pop    %cx
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	fldl   -0x60(%bp)
	fstpl  (%bx)
	jmp   L_0ad6
	mov    -0x58(%bp),%ax
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    %ax,(%bx)
	jmp   L_0ad6
	mov    -0x58(%bp),%ax
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    %ax,(%bx)
	jmp   L_0ad6
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_0ad6:
	jmp   L_0ae4
L_0ad8:
	cmp    $0x12,%ax
	ja   L_0ae4
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x20f6(%bx)
L_0ae4:
	mov    $0x1,%ax
L_0ae7:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0aee:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	mov    0xc(%bp),%bx
	movw   $0x0,(%bx)
	mov    %di,-0x8(%bp)
	cmpw   $0x63,0x6(%bp)
	jne   L_0b1a
	cmpw   $0x7530,0x8(%bp)
	jne   L_0b1a
	movw   $0x1,0x8(%bp)
L_0b1a:
	movw   $0x0,-0xa(%bp)
	cmpw   $0x73,0x6(%bp)
	jne   L_0b2a
	movw   $0x1,-0xa(%bp)
L_0b2a:
	decw   0x2(%si)
	jl   L_0b3b
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0b40
L_0b3b:
	push   %si
	call   L_0ea4
	pop    %cx
L_0b40:
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	mov    D_0076(%bx),%al
	cbtw
	test   %ax,-0xa(%bp)
	je   L_0b55
	cmpw   $0xffff,-0x6(%bp)
	jne   L_0b2a
L_0b55:
	movw   $0x1,-0xa(%bp)
	cmpw   $0x63,0x6(%bp)
	jne   L_0b67
	movw   $0x0,-0xa(%bp)
	jmp   L_0b72
L_0b67:
	cmpw   $0x5b,0x6(%bp)
	jne   L_0b72
	movw   $0x2,-0xa(%bp)
L_0b72:
	cmpw   $0xffff,-0x6(%bp)
	je   L_0bb3
	mov    -0x6(%bp),%bx
	mov    D_0076(%bx),%al
	cbtw
	test   %ax,-0xa(%bp)
	jne   L_0bb3
	or     %di,%di
	je   L_0b93
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
L_0b93:
	decw   0x8(%bp)
	jle   L_0bb3
	decw   0x2(%si)
	jl   L_0ba9
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0bae
L_0ba9:
	push   %si
	call   L_0ea4
	pop    %cx
L_0bae:
	mov    %ax,-0x6(%bp)
	jmp   L_0b72
L_0bb3:
	cmpw   $0xffff,-0x6(%bp)
	je   L_0bd2
	cmpw   $0x0,0x8(%bp)
	jle   L_0bc9
	push   %si
	pushw  -0x6(%bp)
	call   L_211c
	add    $0x4,%sp
L_0bc9:
	mov    0xc(%bp),%bx
	movw   $0x0,(%bx)
	jmp   L_0bd9
L_0bd2:
	mov    0xc(%bp),%bx
	movw   $0x1,(%bx)
L_0bd9:
	or     %di,%di
	je   L_0bf3
	cmp    -0x8(%bp),%di
	je   L_0bf3
	cmpw   $0x63,0x6(%bp)
	je   L_0bee
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
L_0bee:
	mov    $0x1,%ax
	jmp   L_0bf5
L_0bf3:
	xor    %ax,%ax
L_0bf5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0bfc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	movw   $0x0,-0x6(%bp)
	cmpb   $0x5e,(%di)
	jne   L_0c14
	incw   -0x6(%bp)
	inc    %di
L_0c14:
	xor    %si,%si
L_0c16:
	cmp    $0x80,%si
	jge   L_0c31
	cmpw   $0x0,-0x6(%bp)
	je   L_0c29
	andb   $0xfd,D_0076(%si)
	jmp   L_0c2e
L_0c29:
	orb    $0x2,D_0076(%si)
L_0c2e:
	inc    %si
	jmp   L_0c16
L_0c31:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	and    $0x7f,%ax
	cmp    $0x5d,%ax
	je   L_0c5e
	cmpw   $0x0,-0x6(%bp)
	je   L_0c4f
	orb    $0x2,D_0076(%si)
	inc    %si
	jmp   L_0c55
L_0c4f:
	andb   $0xfd,D_0076(%si)
	inc    %si
L_0c55:
	or     %si,%si
	jne   L_0c31
	dec    %di
	mov    %di,%ax
	jmp   L_0c60
L_0c5e:
	mov    %di,%ax
L_0c60:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0c66:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x3c,%sp
	mov    0x4(%bp),%di
	movw   $0x1,-0x24(%bp)
L_0c77:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    $0x20,%ax
	je   L_0c77
	cmp    $0x2d,%si
	jne   L_0c90
	movw   $0xffff,-0x24(%bp)
	jmp   L_0c96
L_0c90:
	cmp    $0x2b,%si
	je   L_0c96
	dec    %di
L_0c96:
	movw   $0x0,-0x22(%bp)
	fldl   D_011c
	fstpl  -0xc(%bp)
	movw   $0x0,-0x1e(%bp)
L_0cab:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    D_014d(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_0cf8
	cmpw   $0x10,-0x1e(%bp)
	jge   L_0cf0
	fldl   D_0124
	fmull  -0xc(%bp)
	fstpl  -0x30(%bp)
	lea    -0x30(%si),%ax
	mov    %ax,-0x38(%bp)
	filds  -0x38(%bp)
	fstl   -0x38(%bp)
	faddl  -0x30(%bp)
	fstl   -0x40(%bp)
	fstpl  -0xc(%bp)
	jmp   L_0cf3
L_0cf0:
	incw   -0x22(%bp)
L_0cf3:
	incw   -0x1e(%bp)
	jmp   L_0cab
L_0cf8:
	cmp    $0x2e,%si
	jne   L_0d48
L_0cfd:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    D_014d(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_0d48
	cmpw   $0x10,-0x1e(%bp)
	jge   L_0d43
	fldl   D_012c
	fmull  -0xc(%bp)
	fstpl  -0x30(%bp)
	lea    -0x30(%si),%ax
	mov    %ax,-0x38(%bp)
	filds  -0x38(%bp)
	fstl   -0x38(%bp)
	faddl  -0x30(%bp)
	fstl   -0x40(%bp)
	fstpl  -0xc(%bp)
	decw   -0x22(%bp)
L_0d43:
	incw   -0x1e(%bp)
	jmp   L_0cfd
L_0d48:
	movw   $0x1,-0x26(%bp)
	movw   $0x0,-0x20(%bp)
	cmp    $0x45,%si
	je   L_0d5c
	cmp    $0x65,%si
	jne   L_0db1
L_0d5c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    $0x2b,%ax
	je   L_0d76
	cmp    $0x2d,%si
	jne   L_0d75
	movw   $0xffff,-0x26(%bp)
	jmp   L_0d76
L_0d75:
	dec    %di
L_0d76:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    D_014d(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_0d9a
	mov    -0x20(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	lea    -0x30(%si),%dx
	add    %dx,%ax
	mov    %ax,-0x20(%bp)
	jmp   L_0d76
L_0d9a:
	cmpw   $0x0,-0x26(%bp)
	jge   L_0da8
	mov    -0x20(%bp),%ax
	neg    %ax
	mov    %ax,-0x20(%bp)
L_0da8:
	mov    -0x22(%bp),%ax
	add    -0x20(%bp),%ax
	mov    %ax,-0x22(%bp)
L_0db1:
	movw   $0x1,-0x26(%bp)
	cmpw   $0x0,-0x22(%bp)
	jge   L_0dc9
	movw   $0xffff,-0x26(%bp)
	mov    -0x22(%bp),%ax
	neg    %ax
	mov    %ax,-0x22(%bp)
L_0dc9:
	mov    -0x22(%bp),%ax
	imulw  -0x26(%bp)
	add    -0x1e(%bp),%ax
	cmp    $0xffd9,%ax
	jge   L_0de7
	fldl   D_0134
	fstpl  -0xc(%bp)
	movw   $0x0,-0x22(%bp)
L_0de7:
	fldl   D_013c
	fstpl  -0x14(%bp)
	fldl   D_0144
	fstpl  -0x1c(%bp)
	mov    -0x22(%bp),%ax
	mov    %ax,-0x28(%bp)
L_0e01:
	testw  $0x1,-0x22(%bp)
	je   L_0e16
	fldl   -0x14(%bp)
	fmull  -0x1c(%bp)
	fstpl  -0x14(%bp)
L_0e16:
	sarw   -0x22(%bp)
	cmpw   $0x0,-0x22(%bp)
	je   L_0e2f
	fldl   -0x1c(%bp)
	fmull  -0x1c(%bp)
	fstpl  -0x1c(%bp)
	jmp   L_0e01
L_0e2f:
	cmpw   $0x0,-0x26(%bp)
	jge   L_0e45
	fldl   -0xc(%bp)
	fdivl  -0x14(%bp)
	fstpl  -0xc(%bp)
	jmp   L_0e53
L_0e45:
	fldl   -0xc(%bp)
	fmull  -0x14(%bp)
	fstpl  -0xc(%bp)
L_0e53:
	mov    -0x26(%bp),%ax
	imulw  -0x28(%bp)
	push   %ax
	fldl   -0xc(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2184
	fstpl  -0x30(%bp)
	add    $0xa,%sp
	fldl   -0x30(%bp)
	fstpl  -0xc(%bp)
	cmpw   $0x0,-0x24(%bp)
	jge   L_0e97
	fldl   -0xc(%bp)
	fchs
	fstl   -0x30(%bp)
	fstpl  -0xc(%bp)
L_0e97:
	fldl   -0xc(%bp)
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ea4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_0ebb
	mov    $0xffff,%ax
	jmp   L_0ec7
L_0ebb:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_0ec9
	mov    $0xffff,%ax
L_0ec7:
	jmp   L_0f41
L_0ec9:
	cmpw   $0x0,0x4(%di)
	jne   L_0efd
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0ee4
	mov    0x7(%di),%al
	cbtw
	add    $D_01ce,%ax
	mov    %ax,0x4(%di)
	jmp   L_0ec9
L_0ee4:
	mov    $0x200,%ax
	push   %ax
	call   L_1e0e
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_0ef9
	orb    $0x4,0x6(%di)
	jmp   L_0ec9
L_0ef9:
	orb    $0x8,0x6(%di)
L_0efd:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0f10
	mov    $0x1,%ax
	jmp   L_0f13
L_0f10:
	mov    $0x200,%ax
L_0f13:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_20b6
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_0f43
	cmpw   $0xffff,0x2(%di)
	jne   L_0f35
	orb    $0x10,0x6(%di)
	jmp   L_0f39
L_0f35:
	orb    $0x20,0x6(%di)
L_0f39:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_0f41:
	jmp   L_0f4d
L_0f43:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_0f4d:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0f52:
	push   %bp
	push   %si
	mov    %sp,%bp
	/* Venix syscall lseek(19) */
	/* Venix syscall lseek(19) -> TLVC patched _lseek (V7 mode) */
	mov	0x6(%bp),%bx	/* fd */
	mov	0x8(%bp),%cx	/* offset low word */
	mov	0xa(%bp),%dx	/* offset high word */
	shl	$2,%dx		/* shift up to make room for whence */
	or	$0x8000,%dx	/* set high bit: V7 lseek convention */
	add	0xc(%bp),%dx	/* fold in whence (0/1/2) */
	mov	$__NR_lseek,%ax
	int	$0x80
	/* result in %ax; kernel handles errno same as standard */
L_0f6d:
	pop    %si
	pop    %bp
	ret


L_0f70:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0f88:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_0f9a
	jmp   L_1537
L_0f9a:
	cmpb   $0x25,-0x5(%bp)
	je   L_0fad
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_0fab:
	jmp   L_0f88
L_0fad:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0fd8
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1b48
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0fd8:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_0ff1
	incb   -0xa6(%bp)
	inc    %di
L_0ff1:
	cmpb   $0x30,(%di)
	jne   L_0ffc
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_0ffc:
	cmpb   $0x2a,(%di)
	je   L_102c
	movw   $0x0,-0xe(%bp)
L_1006:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_1042
	cmpw   $0x9,-0xc(%bp)
	jg   L_1042
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_1006
L_102c:
	inc    %di
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xe(%bp)
L_1042:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_1089
	cmpb   $0x2a,(%di)
	je   L_108b
	movw   $0x0,-0x10(%bp)
L_105b:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_1085
	cmpw   $0x9,-0xc(%bp)
	jg   L_1085
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_105b
L_1085:
	addw   $0x30,-0xc(%bp)
L_1089:
	jmp   L_10a2
L_108b:
	inc    %di
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0xc(%bp)
	incb   -0xa7(%bp)
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0x10(%bp)
L_10a2:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_14ac
L_10ad:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_10be
L_10b5:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_10ee
	jmp   L_10e7
L_10be:
	cmp    $0x64,%ax
	je   L_10f0
	cmp    $0x6f,%ax
	je   L_10d4
	cmp    $0x75,%ax
	je   L_10e7
	cmp    $0x78,%ax
	je   L_10e2
	jmp   L_10b5
L_10d4:
	movw   $0x8,-0x12(%bp)
	jmp   L_10e7
L_10db:
	movw   $0x8,-0x12(%bp)
	jmp   L_10ee
L_10e2:
	movw   $0x10,-0x12(%bp)
L_10e7:
	jmp   L_1139
L_10e9:
	movw   $0x10,-0x12(%bp)
L_10ee:
	jmp   L_1149
L_10f0:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_110d
L_1100:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_110d:
	cmpw   $0x0,-0x14(%bp)
	jg   L_1157
	jl   L_111b
	cmpw   $0x0,-0x16(%bp)
	jae   L_1157
L_111b:
	mov    -0x16(%bp),%ax
	mov    -0x14(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	jmp   L_1157
L_1139:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_1157
L_1149:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_1157:
	cmpw   $0x0,-0x14(%bp)
	jne   L_1161
	cmpw   $0x0,-0x16(%bp)
L_1161:
	jne   L_1172
	cmpw   $0x0,-0x10(%bp)
	jne   L_1172
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_1172:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_1179:
	cmpw   $0x0,-0x14(%bp)
	jne   L_1183
	cmpw   $0x0,-0x16(%bp)
L_1183:
	je   L_11ce
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_1696
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_11a4
	mov    $0x30,%ax
	jmp   L_11a7
L_11a4:
	mov    $0x57,%ax
L_11a7:
	add    -0xc(%bp),%ax
	mov    -0xa(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_158e
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_1179
L_11ce:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_121d
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_11ce
L_11ec:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_11f8:
	cmpw   $0x2,-0xc(%bp)
	jge   L_121d
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_1218
	decw   -0x8(%bp)
L_1218:
	incw   -0xc(%bp)
	jmp   L_11f8
L_121d:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_1224:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_123b
	movw   $0x0,-0xc(%bp)
L_123b:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_124a
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_124a:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_1b48
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_126d:
	jmp   L_0fab
L_1270:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_1282
	movw   $D_01e2,-0xa(%bp)
L_1282:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_1291
	movw   $0x7fff,-0xc(%bp)
L_1291:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_1297:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1224
	decw   -0xc(%bp)
	jl   L_1224
	incw   -0x8(%bp)
	jmp   L_1297
L_12a9:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_12b1:
	jmp   L_126d
L_12b3:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_12bf
	movw   $0x6,-0x10(%bp)
L_12bf:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1894
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_12f4
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_12f4:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_1309
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_1321
L_1309:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_1309
L_1321:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_1334
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_1334:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_1353
L_1340:
	decw   -0xc(%bp)
	jl   L_1353
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_1340
L_1353:
	cmpw   $0x0,-0xc(%bp)
	jg   L_135c
	jmp   L_121d
L_135c:
	decw   -0xc(%bp)
	jge   L_1364
	jmp   L_121d
L_1364:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_135c
L_1379:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1387
	movw   $0x6,-0x10(%bp)
	jmp   L_138a
L_1387:
	incw   -0x10(%bp)
L_138a:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1868
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_13bf
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_13bf:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_13ca
	incw   -0x1a(%bp)
L_13ca:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
L_13ec:
	decw   -0xc(%bp)
	jle   L_1406
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_13ec
L_1406:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_1423
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_1434
L_1423:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_1434:
	mov    -0x1a(%bp),%ax
	xor    %dx,%dx
	mov    $0xa,%cx
	div    %cx
	add    $0x30,%ax
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	xor    %dx,%dx
	mov    $0xa,%cx
	div    %cx
	add    $0x30,%dx
	mov    -0x8(%bp),%bx
	mov    %dx,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_121d
L_1465:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1471
	movw   $0x6,-0x10(%bp)
L_1471:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_16ec
	add    $0xc,%sp
	add    $0x8,%si
L_148f:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_148f
	decw   -0x8(%bp)
	jmp   L_121d
L_14a0:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_14a9:
	jmp   L_12b1
L_14ac:
	cmp    $0x65,%ax
	jne   L_14b4
	jmp   L_1379
L_14b4:
	jg   L_14f5
	cmp    $0x4f,%ax
	jne   L_14be
	jmp   L_10d4
L_14be:
	jg   L_14d5
	cmp    $0x25,%ax
	je   L_14a0
	cmp    $0x44,%ax
	jne   L_14cd
	jmp   L_10f0
L_14cd:
	cmp    $0x4c,%ax
L_14d0:
	jne   L_14d5
	jmp   L_10ad
L_14d5:
	cmp    $0x55,%ax
	jne   L_14dd
	jmp   L_1139
L_14dd:
	cmp    $0x58,%ax
	jne   L_14e5
	jmp   L_10e2
L_14e5:
	cmp    $0x63,%ax
	jne   L_14ed
	jmp   L_11ec
L_14ed:
	cmp    $0x64,%ax
	jne   L_14f5
	jmp   L_1100
L_14f5:
	cmp    $0x6f,%ax
	jne   L_14fd
	jmp   L_10db
L_14fd:
	jg   L_1514
	cmp    $0x66,%ax
	jne   L_1507
	jmp   L_12b3
L_1507:
	cmp    $0x67,%ax
	jne   L_150f
	jmp   L_1465
L_150f:
	cmp    $0x6c,%ax
	je   L_14d0
L_1514:
	cmp    $0x72,%ax
	jne   L_151c
	jmp   L_12a9
L_151c:
	cmp    $0x73,%ax
	jne   L_1524
	jmp   L_1270
L_1524:
	cmp    $0x75,%ax
	jne   L_152c
	jmp   L_1149
L_152c:
	cmp    $0x78,%ax
	jne   L_1534
	jmp   L_10e9
L_1534:
	jmp   L_14a9
L_1537:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_155b
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1b48
	add    $0x8,%sp
L_155b:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_16ca


L_1565:
	mov    %sp,%bx
	mov    0x2(%bx),%ax
	mulw   0x6(%bx)
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mulw   0x8(%bx)
	push   %ax
	mov    0x4(%bx),%ax
	mulw   0x6(%bx)
	add    -0x2(%bx),%ax
	mov    %ax,%dx
	mov    -0x4(%bx),%ax
	add    -0x6(%bx),%dx
	add    $0x6,%sp
	ret
	call   L_16ca


L_158e:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_15ac
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_15ac:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_15b5:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_15b5
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_15c5:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_15c5
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1565
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_1606
	jne   L_15fb
	or     $0x0,%ax
	je   L_1606
L_15fb:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_1606:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_16ca


L_160e:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_162a
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_162a:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_1641
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_1641:
	push   %dx
	push   %ax
	call   L_158e
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_165b
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_165b:
	ret
	call   L_16ca
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_160e
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1565
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_16ca


L_1696:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_158e
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1565
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_16ca:
	pop    %ax
	mov    %sp,%bx
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	mov    0x2(%bx),%bx
	pushw  0x2(%bx)
	pushw  (%bx)
	/* TODO: indirect call - verify */
	call   *%ax
	add    $0x8,%sp
	mov    %sp,%bx
	mov    0x2(%bx),%bx
	mov    %dx,0x2(%bx)
	mov    %ax,(%bx)
	ret
	/* .byte 0 (alignment) */


L_16ec:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0x6(%bp),%ax
	push   %ax
	lea    -0x8(%bp),%ax
	push   %ax
	pushw  0xc(%bp)
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1868
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_1726
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_1726:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_172d:
	cmpw   $0x0,-0xa(%bp)
	jle   L_1747
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_1747
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_172d
L_1747:
	cmpw   $0x0,-0x8(%bp)
	jl   L_1758
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_1764
L_1758:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1762
	cmpw   $0xfffd,-0x8(%bp)
L_1762:
	jge   L_17e3
L_1764:
	decw   -0x8(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
	movw   $0x1,-0xa(%bp)
L_177f:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_1799
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_177f
L_1799:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_17b5
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_17bb
L_17b5:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_17bb:
	mov    -0x8(%bp),%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	add    $0x30,%ax
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    -0x8(%bp),%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	add    $0x30,%dx
	mov    %si,%bx
	inc    %si
	mov    %dx,%cx
	mov    %cl,(%bx)
	jmp   L_1853
L_17e3:
	cmpw   $0x0,-0x8(%bp)
	jg   L_1805
	cmpb   $0x30,(%di)
	je   L_17f4
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_17f4:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1805
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_17f4
L_1805:
	movw   $0x1,-0xa(%bp)
L_180a:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_1832
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    -0xa(%bp),%ax
	cmp    -0x8(%bp),%ax
	jne   L_182d
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_182d:
	incw   -0xa(%bp)
	jmp   L_180a
L_1832:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_1853
L_183a:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_184d
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_183a
L_184d:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1853:
	cmpb   $0x2e,-0x1(%si)
	jne   L_185a
	dec    %si
L_185a:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1868:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x1,%ax
	push   %ax
	pushw  0x10(%bp)
	pushw  0xe(%bp)
	pushw  0xc(%bp)
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_18bf
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1894:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %ax,%ax
	push   %ax
	pushw  0x10(%bp)
	pushw  0xe(%bp)
	pushw  0xc(%bp)
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_18bf
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_18bf:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_18d2
	movw   $0x0,0xc(%bp)
L_18d2:
	cmpw   $0x4f,0xc(%bp)
	jl   L_18dd
	movw   $0x4e,0xc(%bp)
L_18dd:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_01ea,%si
	fldl   0x4(%bp)
	fcompl D_023a
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_191a
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_191a:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_219e
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $D_023a,-0x16(%bp)
	fldl   -0xc(%bp)
	fcompl D_0242
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_1965
	jmp   L_1a12
L_1965:
	movw   $D_023a,-0x16(%bp)
L_196a:
	fldl   -0xc(%bp)
	fcompl D_024a
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_1986
	jmp   L_1a10
L_1986:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	fdivl  D_0252
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_219e
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   D_025a
	fstl   -0x1e(%bp)
	fdivl  D_0262
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  D_026a
	fstl   -0x2e(%bp)
	fistps -0x30(%bp)
	fwait
	mov    -0x30(%bp),%ax
	add    $0x30,%ax
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	inc    %di
	jmp   L_196a
L_19f9:
	cmpw   $D_023a,-0x16(%bp)
	jae   L_1a53
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_1a10:
	jmp   L_19f9
L_1a12:
	fldl   0x4(%bp)
	fcompl D_0272
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_1a62
L_1a2b:
	fldl   0x4(%bp)
	fmull  D_027a
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl D_0282
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_1a53:
	jae   L_1a62
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_1a2b
L_1a62:
	mov    0xc(%bp),%ax
	add    $D_01ea,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_1a74
	add    %di,-0x16(%bp)
L_1a74:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_01ea,-0x16(%bp)
	jae   L_1a8a
	movb   $0x0,D_01ea
	mov    $D_01ea,%ax
	jmp   L_1af5
L_1a8a:
	cmp    -0x16(%bp),%si
	ja   L_1ae6
	cmp    $D_023a,%si
	jae   L_1ae6
	fldl   0x4(%bp)
	fmull  D_028a
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_219e
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	fldl   -0x14(%bp)
	fistps -0x18(%bp)
	fwait
	mov    -0x18(%bp),%ax
	add    $0x30,%ax
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_1a8a
L_1ae6:
	cmpw   $D_023a,-0x16(%bp)
	jb   L_1af7
	movb   $0x0,D_0239
	mov    $D_01ea,%ax
L_1af5:
	jmp   L_1b41
L_1af7:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_1aff:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_1b3b
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_01ea,-0x16(%bp)
	jbe   L_1b1e
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_1aff
L_1b1e:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_1aff
	cmp    $D_01ea,%si
	jbe   L_1b38
	movb   $0x30,(%si)
L_1b38:
	inc    %si
	jmp   L_1aff
L_1b3b:
	movb   $0x0,(%si)
	mov    $D_01ea,%ax
L_1b41:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b48:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1b55:
	cmpw   $0x0,0x8(%bp)
	jl   L_1b5e
	jmp   L_1bec
L_1b5e:
	cmpb   $0x2d,(%di)
	jne   L_1b9b
	cmpw   $0x30,0xc(%bp)
	jne   L_1b9b
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1b8a
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    0xa(%bp),%bx
	incw   (%bx)
	mov    (%bx),%dx
	dec    %dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_1b9a
L_1b8a:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1c3a
	add    $0x4,%sp
L_1b9a:
	dec    %si
L_1b9b:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1bb9
	mov    0xa(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    0xc(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_1bc5
L_1bb9:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_1c3a
	add    $0x4,%sp
L_1bc5:
	incw   0x8(%bp)
	jmp   L_1b55
L_1bca:
	dec    %si
	jl   L_1c00
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1bee
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    0xa(%bp),%bx
	incw   (%bx)
	mov    (%bx),%dx
	dec    %dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
L_1bec:
	jmp   L_1bca
L_1bee:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1c3a
	add    $0x4,%sp
	jmp   L_1bca
L_1c00:
	cmpw   $0x0,0x8(%bp)
	je   L_1c35
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1c24
	mov    0xa(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    0xc(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_1c30
L_1c24:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_1c3a
	add    $0x4,%sp
L_1c30:
	decw   0x8(%bp)
	jmp   L_1c00
L_1c35:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1c3a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_1c55
	mov    $0xffff,%ax
	jmp   L_1d1c
L_1c55:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1c88
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,-0x9(%bp)
	movw   $0x1,-0x8(%bp)
	pushw  -0x8(%bp)
	lea    -0x9(%bp),%ax
	push   %ax
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_216a
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_1d0d
L_1c88:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1cda
	cmp    $D_029a,%di
	jne   L_1cb3
	mov    D_02a1,%al
	cbtw
	push   %ax
	call   L_2078
	pop    %cx
	or     %ax,%ax
	je   L_1ca8
	orb    $0x4,0x6(%di)
	jmp   L_1c55
L_1ca8:
	movw   $D_0344,0x4(%di)
	movw   $D_0344,(%di)
	jmp   L_1c55
L_1cb3:
	mov    $0x200,%ax
	push   %ax
	call   L_1e0e
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1cca
	orb    $0x4,0x6(%di)
	jmp   L_1c55
L_1cca:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_1cfc
L_1cda:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_1cfc
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_216a
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_1cfc:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_1d0d:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_1d1e
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_1d1c:
	jmp   L_1d21
L_1d1e:
	mov    0x4(%bp),%ax
L_1d21:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d28:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	and    $0x6,%ax
	cmp    $0x2,%ax
	jne   L_1d74
	mov    0x4(%di),%si
	or     %si,%si
	je   L_1d74
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_1d74
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_216a
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_1d74
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_1d76
L_1d74:
	xor    %ax,%ax
L_1d76:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d7b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0292,%di
L_1d83:
	cmp    D_0332,%di
	jae   L_1d93
	push   %di
	call   L_1d97
	pop    %cx
	add    $0x8,%di
	jmp   L_1d83
L_1d93:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d97:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_1dea
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_1dea
	push   %di
	call   L_1d28
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_2064
	pop    %cx
	or     %ax,%ax
	jge   L_1dcc
	mov    $0xffff,%si
L_1dcc:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_1ddc
	pushw  0x4(%di)
	call   L_1f5a
	pop    %cx
L_1ddc:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_1dea
	movw   $0x0,0x4(%di)
L_1dea:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_1e0e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_0334
	jne   L_1e39
	mov    $D_0336,%ax
	or     $0x1,%ax
	mov    %ax,D_0334
	mov    $D_0334,%ax
	or     $0x1,%ax
	mov    %ax,D_0336
	/* dual-reloc split: movw $D_0336,D_0746 */
	mov	$D_0336,%ax
	mov	%ax,D_0746
	/* dual-reloc split: movw $D_0334,D_0744 */
	mov	$D_0334,%ax
	mov	%ax,D_0744
L_1e39:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_0744,%di
L_1e4d:
	movw   $0x0,D_0338
L_1e53:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_1e82
L_1e5a:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_1e69
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_1e5a
L_1e69:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_1e82
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_1e82
	jmp   L_1f2b
L_1e82:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	jbe   L_1e91
L_1e8f:
	jmp   L_1e53
L_1e91:
	cmp    D_0746,%si
	jne   L_1e9d
	cmp    $D_0334,%di
	je   L_1ea1
L_1e9d:
	xor    %ax,%ax
	jmp   L_1ed9
L_1ea1:
	incw   D_0338
	cmpw   $0x1,D_0338
	jle   L_1e8f
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_0338
	xor    %ax,%ax
	push   %ax
	call   L_203e
	pop    %cx
	mov    %ax,%si
	mov    D_0338,%ax
	shl    %ax
	add    %si,%ax
	add    $0x0,%ax
	cmp    %si,%ax
	jae   L_1edb
	xor    %ax,%ax
L_1ed9:
	jmp   L_1eee
L_1edb:
	mov    D_0338,%ax
	shl    %ax
	push   %ax
	call   L_203e
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_1ef0
	xor    %ax,%ax
L_1eee:
	jmp   L_1f55
L_1ef0:
	mov    D_0746,%bx
	mov    %si,(%bx)
	mov    D_0746,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_1f0e
	mov    D_0746,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_0746,%bx
	mov    %ax,(%bx)
L_1f0e:
	mov    D_0338,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_0746
	mov    $D_0334,%ax
	or     $0x1,%ax
	mov    D_0746,%bx
	mov    %ax,(%bx)
	jmp   L_1e4d
L_1f2b:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_0744
	cmp    %ax,%si
	jbe   L_1f4a
	mov    D_0744,%bx
	mov    (%bx),%ax
	mov    %ax,D_0748
	mov    D_0744,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_1f4a:
	mov    D_0744,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_1f55:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1f5a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_0744
	mov    (%si),%ax
	and    $0xfffe,%ax
	mov    %ax,(%si)
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%di
	mov    -0x2(%di),%ax
	test   $0x1,%ax
	je   L_1f8f
	push   %di
	call   L_1f5a
	pop    %cx
L_1f8f:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_1e0e
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_1fae
	cmp    %di,%si
	jne   L_1fb2
L_1fae:
	mov    %si,%ax
	jmp   L_201a
L_1fb2:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_1fd1
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_1fd1:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_1fef
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_1fd1
L_1fef:
	cmp    %di,%si
	jae   L_2018
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_2018
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	shl    %ax
	add    %si,%ax
	mov    %ax,%bx
	mov    D_0748,%ax
	mov    %ax,(%bx)
L_2018:
	mov    %si,%ax
L_201a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2035
	neg	%ax
	mov	%ax,D_033e	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_203c
L_2035:
	mov    0x4(%bp),%bx
	mov    %bx,D_033a
L_203c:
	pop    %bp
	ret


L_203e:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_033a,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2055
	neg	%ax
	mov	%ax,D_033e	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_2062
L_2055:
	mov    D_033a,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_033a
L_2062:
	pop    %bp
	ret


L_2064:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall close(6) */
	mov    0x4(%bp),%ax
	/* close -> __NR_close */
	xchg	%ax,%bx
	mov	$__NR_close,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2075
	neg	%ax
	mov	%ax,D_033e	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2075:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2078:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_20e9
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_2095
	xor    %ax,%ax
	jmp   L_2098
L_2095:
	mov    $0x1,%ax
L_2098:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_20a0:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall open(5) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* open: Venix has no mode arg; supply mode=0666 for TLVC */
	xchg	%ax,%bx
	mov	%dx,%cx
	xor	%dx,%dx
	test	$0x3,%cx
	je	open_wronly_5_7190
	and	$0xfffc,%cx
	or	$0x241,%cx
	mov	$0x1b6,%dx
open_wronly_5_7190:
	mov	$__NR_open,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_20b4
	neg	%ax
	mov	%ax,D_033e	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_20b4:
	pop    %bp
	ret


L_20b6:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall read(3) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* read -> __NR_read */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_read,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_20cd
	neg	%ax
	mov	%ax,D_033e	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_20cd:
	pop    %bp
	ret
	/* .byte 0 (alignment) */
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7409,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2102
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_20e9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2102
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2102:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall ioctl(54) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* ioctl -> __NR_ioctl */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_ioctl,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2119
	neg	%ax
	mov	%ax,D_033e	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2119:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_211c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x6(%bp),%di
	cmpw   $0xffff,0x4(%bp)
	jne   L_2130
	mov    $0xffff,%ax
	jmp   L_2166
L_2130:
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	je   L_2140
	mov    (%di),%ax
	cmp    0x4(%di),%ax
	ja   L_2156
L_2140:
	mov    (%di),%ax
	cmp    0x4(%di),%ax
	jne   L_2151
	cmpw   $0x0,0x2(%di)
	jne   L_2151
	incw   (%di)
	jmp   L_2156
L_2151:
	mov    $0xffff,%ax
	jmp   L_2166
L_2156:
	incw   0x2(%di)
	decw   (%di)
	mov    (%di),%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	xor    %ax,%ax
L_2166:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_216a:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall write(4) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* write -> __NR_write */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_write,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2181
	neg	%ax
	mov	%ax,D_033e	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2181:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2184:
	push   %bp
	mov    %sp,%bp
	filds  0xc(%bp)
	fldl   0x4(%bp)
	fscale
	fxch   %st(1)
	fistps 0xc(%bp)
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_219e:
	push   %bp
	mov    %sp,%bp
	mov    0xc(%bp),%bx
	fldl   0x4(%bp)
	frndint
	fstl   (%bx)
	fsubl  0x4(%bp)
	fchs
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_21ba:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall exit(1) */
	mov    0x4(%bp),%ax
	/* exit -> __NR_exit */
	xchg	%ax,%bx
	mov	$__NR_exit,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_21cb
	neg	%ax
	mov	%ax,D_033e	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_21cb:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


	/* synthesized: fstat() - translates TLVC stat (32-bit ino) to Venix stat (16-bit ino)
	   TLVC: st_dev+0 st_ino+2(long) st_mode+6 st_nlink+8 st_uid+10 st_gid+12
	          st_rdev+14 st_size+16(long) st_atime+20 st_mtime+24 st_ctime+28
	   Venix: st_dev+0  st_ino+2(short) st_mode+4  st_nlink+6  st_uid+8  st_gid+10
	          st_rdev+12 st_size+14(long) st_atime+18 st_mtime+22 st_ctime+26 */
fstat:
	push	%bp
	mov	%sp,%bp
	push	%si
	push	%di
	sub	$0x20,%sp		/* 32-byte TLVC stat buffer on stack */
	/* call fstat syscall into our TLVC buffer */
	mov	0x4(%bp),%bx	/* fd */
	lea	-0x20(%bp),%cx	/* &tlvc_stat */
	mov	$__NR_fstat,%ax
	int	$0x80
	test	%ax,%ax
	jns	fstat_ok
	neg	%ax
	mov	%ax,D_033e	/* errno */
	mov	$0xffff,%ax
	jmp	fstat_ret
fstat_ok:
	/* translate TLVC stat -> Venix stat in caller buffer */
	mov	0x6(%bp),%di	/* di = venix_stat_buf */
	lea	-0x20(%bp),%si	/* si = tlvc_stat_buf */
	/* st_dev: TLVC +0 -> Venix +0 (same) */
	mov	(%si),%ax
	mov	%ax,(%di)
	/* st_ino: TLVC +2..+5 (long) -> Venix +2 (short, low word only) */
	mov	0x2(%si),%ax
	mov	%ax,0x2(%di)
	/* Remaining fields: TLVC offsets +6..+31 -> Venix offsets +4..+29 */
	/* memcpy(venix+4, tlvc+6, 26): st_mode..st_ctime */
	lea	0x4(%di),%di
	lea	0x6(%si),%si
	mov	$26,%cx
	cld
	rep	movsb
	xor	%ax,%ax	/* return 0 */
fstat_ret:
	add	$0x20,%sp
	pop	%di
	pop	%si
	pop	%bp
	ret


	/* synthesized by vrewrite: _exit(status) - raw exit syscall */
	.global _exit
_exit:
	push	%bp
	mov	%sp,%bp
	mov	$__NR_exit,%ax
	mov	0x4(%bp),%bx	/* arg1: exit status */
	int	$0x80
	/* no return */


	.data

	.byte	0x00, 0x00	/* |..| */
D_0002:
	.byte	0xbf, 0x0f	/* |..| */
D_0004:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_000c:
	.byte	0x0e, 0x20, 0x00	/* |. .| */
D_000f:
	.byte	0x72, 0x00	/* |r.| */
D_0011:
	.ascii	"cannot open \0"
D_001e:
	.byte	0x25, 0x73	/* |%s| */
D_0020:
	.byte	0x25, 0x73, 0x00	/* |%s.| */
D_0023:
	.ascii	"odd data\0"
D_002c:
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_0030:
	.ascii	"too many items\0"
D_003f:
	.byte	0x74, 0x73, 0x6f, 0x72, 0x74, 0x3a, 0x20, 0x25	/* |tsort: %| */
	.byte	0x73, 0x25, 0x73, 0x0a, 0x00	/* |s%s..| */
D_004c:
	.ascii	"cycle in reverse order\0"
D_0063:
	.ascii	"error 1\0"
D_006b:
	.ascii	"error 2\0"
	.byte	0x00	/* |.| */
D_0074:
	.byte	0x00, 0x00	/* |..| */
D_0076:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0xae, 0x0a, 0xba, 0x0a, 0xc6, 0x0a, 0xe4, 0x0a	/* |........| */
	.byte	0xe4, 0x0a, 0xe4, 0x0a, 0xe4, 0x0a, 0xe4, 0x0a	/* |........| */
	.byte	0xe4, 0x0a, 0xe4, 0x0a, 0xe4, 0x0a, 0xe4, 0x0a	/* |........| */
	.byte	0xe4, 0x0a, 0xe4, 0x0a, 0xe4, 0x0a, 0xe4, 0x0a	/* |........| */
	.byte	0x72, 0x0a, 0x72, 0x0a, 0x90, 0x0a	/* |r.r...| */
D_011c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0124:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_012c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0134:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_013c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_0144:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x14, 0x40	/* |.......@| */
	.byte	0x00	/* |.| */
D_014d:
	.byte	0x20	/* | | */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.byte	0x08, 0x08, 0x08, 0x08, 0x08, 0x20	/* |..... | */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.byte	0x20, 0x08, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* | .......| */
	.byte	0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.word	D_0410	/* ptr: 0x0410 */
	.word	D_0404	/* ptr: 0x0404 */
	.word	D_0404	/* ptr: 0x0404 */
	.word	D_0404	/* ptr: 0x0404 */
	.word	D_0404	/* ptr: 0x0404 */
	.byte	0x04, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x01, 0x01	/* |AAAAAA..| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x02, 0x02	/* |BBBBBB..| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x10, 0x10, 0x10, 0x10	/* |......| */
	.word	D_0020	/* ptr: 0x0020 */
D_01ce:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_01e2:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_01ea:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |.......| */
D_0239:
	.byte	0x00	/* |.| */
D_023a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0242:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_024a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0252:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_025a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_0262:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_026a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0272:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_027a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0282:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_028a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0292:
	.byte	0x44, 0x25, 0x00, 0x00	/* |D%..| */
	.word	D_0544	/* ptr: 0x2544 */
	.byte	0x01, 0x00	/* |..| */
D_029a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_02a0:
	.byte	0x02	/* |.| */
D_02a1:
	.byte	0x01	/* |.| */
D_02a2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
D_02aa:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0332:
	.word	D_0332	/* ptr: 0x2332 */
D_0334:
	.byte	0x00, 0x00	/* |..| */
D_0336:
	.byte	0x00, 0x00	/* |..| */
D_0338:
	.byte	0x00, 0x00	/* |..| */
D_033a:
	.word	D_074a	/* ptr: 0x274a */

	.bss

	.space	2
D_033e:	.space	2	/* bss word @ 0x033e */
	.space	4
D_0344:	.space	2	/* bss word @ 0x0344 */
	.space	190
D_0404:	.space	2	/* bss word @ 0x0404 */
	.space	10
D_0410:	.space	2	/* bss word @ 0x0410 */
	.space	306
D_0544:	.space	2	/* bss word @ 0x0544 */
	.space	510
D_0744:	.space	2	/* bss word @ 0x0744 */
D_0746:	.space	2	/* bss word @ 0x0746 */
D_0748:	.space	2	/* bss word @ 0x0748 */
D_074a:	/* endcore: initial break */

/* end of file */
