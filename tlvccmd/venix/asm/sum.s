/* sum.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 sum.s -o sum.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: sum  [linked executable]
//   Magic:    0x0107  OMAGIC (object/impure)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x000015b4  (5556 bytes)
//   a_data:   0x000001b2  (434 bytes)
//   Load:  cs:0x0000 (text)   ds:0x35b4 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xa,%sp
L_0045:
	movw   $0x0,-0xe(%bp)
	mov    $0x1,%si
L_004d:
	cmp    0x4(%bp),%si
	jge   L_008e
	/* TODO: data ref: mov    $0x35b8,%ax */
	mov    $D_35b8,%ax
	push   %ax
	mov    %si,%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_01b4
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jne   L_0093
	mov    %si,%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x35ba,%ax */
	mov    $D_35ba,%ax
	push   %ax
	/* TODO: data ref: mov    $0x36cc,%ax */
	mov    $D_36cc,%ax
	push   %ax
	call   L_0188
	add    $0x6,%sp
	addw   $0xa,-0xe(%bp)
	jmp   L_0170
L_008e:
	/* TODO: data ref: movw   $0x36bc,-0x8(%bp) */
	movw   $D_36bc,-0x8(%bp)
L_0093:
	xor    %di,%di
	mov    %di,-0xc(%bp)
	movw   $0x0,-0xa(%bp)
L_009d:
	mov    -0x8(%bp),%bx
	decw   0x2(%bx)
	jl   L_00b7
	mov    -0x8(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_00be
L_00b7:
	pushw  -0x8(%bp)
	call   L_02f2
	pop    %cx
L_00be:
	mov    %ax,-0x6(%bp)
	cmp    $0xffff,%ax
	je   L_00ea
	addw   $0x1,-0xc(%bp)
	adcw   $0x0,-0xa(%bp)
	test   $0x1,%di
	je   L_00df
	mov    %di,%ax
	shr    %ax
	add    $0x8000,%ax
	mov    %ax,%di
	jmp   L_00e1
L_00df:
	shr    %di
L_00e1:
	add    -0x6(%bp),%di
	and    $0xffff,%di
	jmp   L_009d
L_00ea:
	mov    -0x8(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_011e
	incw   -0xe(%bp)
	cmpw   $0x1,0x4(%bp)
	jle   L_010c
	mov    %si,%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	jmp   L_010f
L_010c:
	/* TODO: data ref: mov    $0x35e5,%ax */
	mov    $D_35e5,%ax
L_010f:
	push   %ax
	/* TODO: data ref: mov    $0x35ce,%ax */
	mov    $D_35ce,%ax
	push   %ax
	/* TODO: data ref: mov    $0x36cc,%ax */
	mov    $D_36cc,%ax
	push   %ax
	call   L_0188
	add    $0x6,%sp
L_011e:
	mov    $0x200,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    -0xc(%bp),%ax
	mov    -0xa(%bp),%dx
	add    $0x1ff,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	call   L_0a5c
	add    $0x8,%sp
	push   %dx
	push   %ax
	push   %di
	/* TODO: data ref: mov    $0x35e7,%ax */
	mov    $D_35e7,%ax
	push   %ax
	call   L_02c8
	add    $0x8,%sp
	cmpw   $0x2,0x4(%bp)
	jle   L_0161
	mov    %si,%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x35f0,%ax */
	mov    $D_35f0,%ax
	push   %ax
	call   L_02c8
	add    $0x4,%sp
L_0161:
	/* TODO: data ref: mov    $0x35f4,%ax */
	mov    $D_35f4,%ax
	push   %ax
	call   L_02c8
	pop    %cx
	pushw  -0x8(%bp)
	call   L_11e5
	pop    %cx
L_0170:
	inc    %si
	cmp    0x4(%bp),%si
	jge   L_0179
	jmp   L_004d
L_0179:
	pushw  -0xe(%bp)
	call   exit
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0188:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_03be
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_01ae
	mov    $0xffff,%ax
	jmp   L_01b0
L_01ae:
	xor    %ax,%ax
L_01b0:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_01b4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	/* TODO: data ref: cmpb   $0x0,0x35f6 */
	cmpb   $0x0,D_35f6
	jne   L_01f1
	/* TODO: data ref: movw   $0x36d4,-0x8(%bp) */
	movw   $D_36d4,-0x8(%bp)
	/* TODO: data ref: movw   $0x375c,0x375c */
	/* suppressed: movw $D_375c,D_375c (self-ref in .data) */
	movw   $0x88,-0xa(%bp)
L_01d7:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_01ec
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_01d7
L_01ec:
	/* TODO: data ref: movb   $0x1,0x35f6 */
	movb   $0x1,D_35f6
L_01f1:
	/* TODO: data ref: movw   $0x36bc,-0x6(%bp) */
	movw   $D_36bc,-0x6(%bp)
L_01f6:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_0215
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x375c,%ax */
	cmp    D_375c,%ax
	jb   L_020f
	xor    %ax,%ax
	jmp   L_0280
L_020f:
	addw   $0x8,-0x6(%bp)
	jmp   L_01f6
L_0215:
	cmpb   $0x77,(%di)
	jne   L_022b
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_02b2
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_026a
L_022b:
	cmpb   $0x61,(%di)
	jne   L_026c
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_14ee
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_0259
	/* TODO: data ref: cmpw   $0x2,0x3768 */
	cmpw   $0x2,D_3768
	jne   L_0259
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_02b2
	add    $0x4,%sp
	mov    %ax,%si
L_0259:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_03a0
	add    $0x8,%sp
L_026a:
	jmp   L_027a
L_026c:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_14ee
	add    $0x4,%sp
	mov    %ax,%si
L_027a:
	or     %si,%si
	jge   L_0282
	xor    %ax,%ax
L_0280:
	jmp   L_02aa
L_0282:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_02a0
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_02a7
L_02a0:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_02a7:
	mov    -0x6(%bp),%ax
L_02aa:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_02b2:
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
	jns    L_02c6
	neg	%ax
	mov	%ax,D_3768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_02c6:
	pop    %bp
	ret


L_02c8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x36c4,%ax */
	mov    $D_36c4,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_03be
	add    $0x6,%sp
	/* TODO: data ref: mov    0x36ca,%al */
	mov    D_36ca,%al
	cbtw
	test   $0x20,%ax
	je   L_02ec
	mov    $0xffff,%ax
	jmp   L_02ee
L_02ec:
	xor    %ax,%ax
L_02ee:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_02f2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_0309
	mov    $0xffff,%ax
	jmp   L_0315
L_0309:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_0317
	mov    $0xffff,%ax
L_0315:
	jmp   L_038f
L_0317:
	cmpw   $0x0,0x4(%di)
	jne   L_034b
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0332
	mov    0x7(%di),%al
	cbtw
	/* TODO: data ref: add    $0x35f8,%ax */
	add    $D_35f8,%ax
	mov    %ax,0x4(%di)
	jmp   L_0317
L_0332:
	mov    $0x200,%ax
	push   %ax
	call   L_125c
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_0347
	orb    $0x4,0x6(%di)
	jmp   L_0317
L_0347:
	orb    $0x8,0x6(%di)
L_034b:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_035e
	mov    $0x1,%ax
	jmp   L_0361
L_035e:
	mov    $0x200,%ax
L_0361:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1504
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_0391
	cmpw   $0xffff,0x2(%di)
	jne   L_0383
	orb    $0x10,0x6(%di)
	jmp   L_0387
L_0383:
	orb    $0x20,0x6(%di)
L_0387:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_038f:
	jmp   L_039b
L_0391:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_039b:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_03a0:
	push   %bp
	push   %si
	mov    %sp,%bp
	/* Venix syscall lseek(19) */
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    0xa(%bp),%cx
	mov    0xc(%bp),%si
	/* lseek -> __NR_lseek */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_lseek,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_03bb
	neg	%ax
	mov	%ax,D_3768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_03bb:
	pop    %si
	pop    %bp
	ret


L_03be:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_03d6:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_03e8
	jmp   L_0985
L_03e8:
	cmpb   $0x25,-0x5(%bp)
	je   L_03fb
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_03f9:
	jmp   L_03d6
L_03fb:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0426
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_0f96
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0426:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_043f
	incb   -0xa6(%bp)
	inc    %di
L_043f:
	cmpb   $0x30,(%di)
	jne   L_044a
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_044a:
	cmpb   $0x2a,(%di)
	je   L_047a
	movw   $0x0,-0xe(%bp)
L_0454:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0490
	cmpw   $0x9,-0xc(%bp)
	jg   L_0490
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_0454
L_047a:
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
L_0490:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_04d7
	cmpb   $0x2a,(%di)
	je   L_04d9
	movw   $0x0,-0x10(%bp)
L_04a9:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_04d3
	cmpw   $0x9,-0xc(%bp)
	jg   L_04d3
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_04a9
L_04d3:
	addw   $0x30,-0xc(%bp)
L_04d7:
	jmp   L_04f0
L_04d9:
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
L_04f0:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_08fa
L_04fb:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_050c
L_0503:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_053c
	jmp   L_0535
L_050c:
	cmp    $0x64,%ax
	je   L_053e
	cmp    $0x6f,%ax
	je   L_0522
	cmp    $0x75,%ax
	je   L_0535
	cmp    $0x78,%ax
	je   L_0530
	jmp   L_0503
L_0522:
	movw   $0x8,-0x12(%bp)
	jmp   L_0535
L_0529:
	movw   $0x8,-0x12(%bp)
	jmp   L_053c
L_0530:
	movw   $0x10,-0x12(%bp)
L_0535:
	jmp   L_0587
L_0537:
	movw   $0x10,-0x12(%bp)
L_053c:
	jmp   L_0597
L_053e:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_055b
L_054e:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_055b:
	cmpw   $0x0,-0x14(%bp)
	jg   L_05a5
	jl   L_0569
	cmpw   $0x0,-0x16(%bp)
	jae   L_05a5
L_0569:
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
	jmp   L_05a5
L_0587:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_05a5
L_0597:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_05a5:
	cmpw   $0x0,-0x14(%bp)
	jne   L_05af
	cmpw   $0x0,-0x16(%bp)
L_05af:
	jne   L_05c0
	cmpw   $0x0,-0x10(%bp)
	jne   L_05c0
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_05c0:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_05c7:
	cmpw   $0x0,-0x14(%bp)
	jne   L_05d1
	cmpw   $0x0,-0x16(%bp)
L_05d1:
	je   L_061c
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_0ae4
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_05f2
	mov    $0x30,%ax
	jmp   L_05f5
L_05f2:
	mov    $0x57,%ax
L_05f5:
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
	call   L_09dc
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_05c7
L_061c:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_066b
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_061c
L_063a:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_0646:
	cmpw   $0x2,-0xc(%bp)
	jge   L_066b
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0666
	decw   -0x8(%bp)
L_0666:
	incw   -0xc(%bp)
	jmp   L_0646
L_066b:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0672:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_0689
	movw   $0x0,-0xc(%bp)
L_0689:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_0698
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_0698:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_0f96
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_06bb:
	jmp   L_03f9
L_06be:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_06d0
	/* TODO: data ref: movw   $0x360c,-0xa(%bp) */
	movw   $D_360c,-0xa(%bp)
L_06d0:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_06df
	movw   $0x7fff,-0xc(%bp)
L_06df:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_06e5:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0672
	decw   -0xc(%bp)
	jl   L_0672
	incw   -0x8(%bp)
	jmp   L_06e5
L_06f7:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_06ff:
	jmp   L_06bb
L_0701:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_070d
	movw   $0x6,-0x10(%bp)
L_070d:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0ce2
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0742
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0742:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_0757
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_076f
L_0757:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_0757
L_076f:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_0782
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_0782:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_07a1
L_078e:
	decw   -0xc(%bp)
	jl   L_07a1
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_078e
L_07a1:
	cmpw   $0x0,-0xc(%bp)
	jg   L_07aa
	jmp   L_066b
L_07aa:
	decw   -0xc(%bp)
	jge   L_07b2
	jmp   L_066b
L_07b2:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_07aa
L_07c7:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_07d5
	movw   $0x6,-0x10(%bp)
	jmp   L_07d8
L_07d5:
	incw   -0x10(%bp)
L_07d8:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0cb6
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_080d
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_080d:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_0818
	incw   -0x1a(%bp)
L_0818:
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
L_083a:
	decw   -0xc(%bp)
	jle   L_0854
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_083a
L_0854:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_0871
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_0882
L_0871:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_0882:
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
	jmp   L_066b
L_08b3:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_08bf
	movw   $0x6,-0x10(%bp)
L_08bf:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0b3a
	add    $0xc,%sp
	add    $0x8,%si
L_08dd:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_08dd
	decw   -0x8(%bp)
	jmp   L_066b
L_08ee:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_08f7:
	jmp   L_06ff
L_08fa:
	cmp    $0x65,%ax
	jne   L_0902
	jmp   L_07c7
L_0902:
	jg   L_0943
	cmp    $0x4f,%ax
	jne   L_090c
	jmp   L_0522
L_090c:
	jg   L_0923
	cmp    $0x25,%ax
	je   L_08ee
	cmp    $0x44,%ax
	jne   L_091b
	jmp   L_053e
L_091b:
	cmp    $0x4c,%ax
L_091e:
	jne   L_0923
	jmp   L_04fb
L_0923:
	cmp    $0x55,%ax
	jne   L_092b
	jmp   L_0587
L_092b:
	cmp    $0x58,%ax
	jne   L_0933
	jmp   L_0530
L_0933:
	cmp    $0x63,%ax
	jne   L_093b
	jmp   L_063a
L_093b:
	cmp    $0x64,%ax
	jne   L_0943
	jmp   L_054e
L_0943:
	cmp    $0x6f,%ax
	jne   L_094b
	jmp   L_0529
L_094b:
	jg   L_0962
	cmp    $0x66,%ax
	jne   L_0955
	jmp   L_0701
L_0955:
	cmp    $0x67,%ax
	jne   L_095d
	jmp   L_08b3
L_095d:
	cmp    $0x6c,%ax
	je   L_091e
L_0962:
	cmp    $0x72,%ax
	jne   L_096a
	jmp   L_06f7
L_096a:
	cmp    $0x73,%ax
	jne   L_0972
	jmp   L_06be
L_0972:
	cmp    $0x75,%ax
	jne   L_097a
	jmp   L_0597
L_097a:
	cmp    $0x78,%ax
	jne   L_0982
	jmp   L_0537
L_0982:
	jmp   L_08f7
L_0985:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_09a9
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_0f96
	add    $0x8,%sp
L_09a9:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_0b18


L_09b3:
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
	call   L_0b18


L_09dc:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_09fa
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_09fa:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_0a03:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_0a03
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_0a13:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_0a13
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_09b3
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_0a54
	jne   L_0a49
	or     $0x0,%ax
	je   L_0a54
L_0a49:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_0a54:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_0b18


L_0a5c:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0a78
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0a78:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0a8f
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0a8f:
	push   %dx
	push   %ax
	call   L_09dc
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_0aa9
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_0aa9:
	ret
	call   L_0b18
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0a5c
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_09b3
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_0b18


L_0ae4:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_09dc
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_09b3
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_0b18:
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


L_0b3a:
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
	call   L_0cb6
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_0b74
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_0b74:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_0b7b:
	cmpw   $0x0,-0xa(%bp)
	jle   L_0b95
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_0b95
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_0b7b
L_0b95:
	cmpw   $0x0,-0x8(%bp)
	jl   L_0ba6
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_0bb2
L_0ba6:
	cmpw   $0x0,-0x8(%bp)
	jge   L_0bb0
	cmpw   $0xfffd,-0x8(%bp)
L_0bb0:
	jge   L_0c31
L_0bb2:
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
L_0bcd:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_0be7
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_0bcd
L_0be7:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_0c03
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_0c09
L_0c03:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_0c09:
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
	jmp   L_0ca1
L_0c31:
	cmpw   $0x0,-0x8(%bp)
	jg   L_0c53
	cmpb   $0x30,(%di)
	je   L_0c42
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0c42:
	cmpw   $0x0,-0x8(%bp)
	jge   L_0c53
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_0c42
L_0c53:
	movw   $0x1,-0xa(%bp)
L_0c58:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_0c80
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
	jne   L_0c7b
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0c7b:
	incw   -0xa(%bp)
	jmp   L_0c58
L_0c80:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_0ca1
L_0c88:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_0c9b
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_0c88
L_0c9b:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0ca1:
	cmpb   $0x2e,-0x1(%si)
	jne   L_0ca8
	dec    %si
L_0ca8:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0cb6:
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
	call   L_0d0d
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ce2:
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
	call   L_0d0d
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0d0d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_0d20
	movw   $0x0,0xc(%bp)
L_0d20:
	cmpw   $0x4f,0xc(%bp)
	jl   L_0d2b
	movw   $0x4e,0xc(%bp)
L_0d2b:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	/* TODO: data ref: mov    $0x3614,%si */
	mov    $D_3614,%si
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x3664 */
	fcompl D_3664
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_0d68
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_0d68:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1584
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	/* TODO: data ref: movw   $0x3664,-0x16(%bp) */
	movw   $D_3664,-0x16(%bp)
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x366c */
	fcompl D_366c
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_0db3
	jmp   L_0e60
L_0db3:
	/* TODO: data ref: movw   $0x3664,-0x16(%bp) */
	movw   $D_3664,-0x16(%bp)
L_0db8:
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x3674 */
	fcompl D_3674
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_0dd4
	jmp   L_0e5e
L_0dd4:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	/* TODO: data ref: fdivl  0x367c */
	fdivl  D_367c
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1584
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	/* TODO: data ref: fldl   0x3684 */
	fldl   D_3684
	fstl   -0x1e(%bp)
	/* TODO: data ref: fdivl  0x368c */
	fdivl  D_368c
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	/* TODO: data ref: fmull  0x3694 */
	fmull  D_3694
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
	jmp   L_0db8
L_0e47:
	/* TODO: data ref: cmpw   $0x3664,-0x16(%bp) */
	cmpw   $D_3664,-0x16(%bp)
	jae   L_0ea1
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_0e5e:
	jmp   L_0e47
L_0e60:
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x369c */
	fcompl D_369c
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_0eb0
L_0e79:
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x36a4 */
	fmull  D_36a4
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	/* TODO: data ref: fcompl 0x36ac */
	fcompl D_36ac
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_0ea1:
	jae   L_0eb0
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_0e79
L_0eb0:
	mov    0xc(%bp),%ax
	/* TODO: data ref: add    $0x3614,%ax */
	add    $D_3614,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_0ec2
	add    %di,-0x16(%bp)
L_0ec2:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	/* TODO: data ref: cmpw   $0x3614,-0x16(%bp) */
	cmpw   $D_3614,-0x16(%bp)
	jae   L_0ed8
	/* TODO: data ref: movb   $0x0,0x3614 */
	movb   $0x0,D_3614
	/* TODO: data ref: mov    $0x3614,%ax */
	mov    $D_3614,%ax
	jmp   L_0f43
L_0ed8:
	cmp    -0x16(%bp),%si
	ja   L_0f34
	/* TODO: data ref: cmp    $0x3664,%si */
	cmp    $D_3664,%si
	jae   L_0f34
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x36b4 */
	fmull  D_36b4
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1584
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
	jmp   L_0ed8
L_0f34:
	/* TODO: data ref: cmpw   $0x3664,-0x16(%bp) */
	cmpw   $D_3664,-0x16(%bp)
	jb   L_0f45
	/* TODO: data ref: movb   $0x0,0x3663 */
	movb   $0x0,D_3663
	/* TODO: data ref: mov    $0x3614,%ax */
	mov    $D_3614,%ax
L_0f43:
	jmp   L_0f8f
L_0f45:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_0f4d:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_0f89
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	/* TODO: data ref: cmpw   $0x3614,-0x16(%bp) */
	cmpw   $D_3614,-0x16(%bp)
	jbe   L_0f6c
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_0f4d
L_0f6c:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_0f4d
	/* TODO: data ref: cmp    $0x3614,%si */
	cmp    $D_3614,%si
	jbe   L_0f86
	movb   $0x30,(%si)
L_0f86:
	inc    %si
	jmp   L_0f4d
L_0f89:
	movb   $0x0,(%si)
	/* TODO: data ref: mov    $0x3614,%ax */
	mov    $D_3614,%ax
L_0f8f:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f96:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_0fa3:
	cmpw   $0x0,0x8(%bp)
	jl   L_0fac
	jmp   L_103a
L_0fac:
	cmpb   $0x2d,(%di)
	jne   L_0fe9
	cmpw   $0x30,0xc(%bp)
	jne   L_0fe9
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_0fd8
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
	jmp   L_0fe8
L_0fd8:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1088
	add    $0x4,%sp
L_0fe8:
	dec    %si
L_0fe9:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1007
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
	jmp   L_1013
L_1007:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_1088
	add    $0x4,%sp
L_1013:
	incw   0x8(%bp)
	jmp   L_0fa3
L_1018:
	dec    %si
	jl   L_104e
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_103c
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
L_103a:
	jmp   L_1018
L_103c:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1088
	add    $0x4,%sp
	jmp   L_1018
L_104e:
	cmpw   $0x0,0x8(%bp)
	je   L_1083
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1072
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
	jmp   L_107e
L_1072:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_1088
	add    $0x4,%sp
L_107e:
	decw   0x8(%bp)
	jmp   L_104e
L_1083:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1088:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_10a3
	mov    $0xffff,%ax
	jmp   L_116a
L_10a3:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_10d6
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
	call   L_156a
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_115b
L_10d6:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1128
	/* TODO: data ref: cmp    $0x36c4,%di */
	cmp    $D_36c4,%di
	jne   L_1101
	/* TODO: data ref: mov    0x36cb,%al */
	mov    D_36cb,%al
	cbtw
	push   %ax
	call   L_14c6
	pop    %cx
	or     %ax,%ax
	je   L_10f6
	orb    $0x4,0x6(%di)
	jmp   L_10a3
L_10f6:
	/* TODO: data ref: movw   $0x376e,0x4(%di) */
	movw   $D_376e,0x4(%di)
	/* TODO: data ref: movw   $0x376e,(%di) */
	movw   $D_376e,(%di)
	jmp   L_10a3
L_1101:
	mov    $0x200,%ax
	push   %ax
	call   L_125c
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1118
	orb    $0x4,0x6(%di)
	jmp   L_10a3
L_1118:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_114a
L_1128:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_114a
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_156a
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_114a:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_115b:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_116c
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_116a:
	jmp   L_116f
L_116c:
	mov    0x4(%bp),%ax
L_116f:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1176:
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
	jne   L_11c2
	mov    0x4(%di),%si
	or     %si,%si
	je   L_11c2
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_11c2
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_156a
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_11c2
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_11c4
L_11c2:
	xor    %ax,%ax
L_11c4:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_11c9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x36bc,%di */
	mov    $D_36bc,%di
L_11d1:
	/* TODO: data ref: cmp    0x375c,%di */
	cmp    D_375c,%di
	jae   L_11e1
	push   %di
	call   L_11e5
	pop    %cx
	add    $0x8,%di
	jmp   L_11d1
L_11e1:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_11e5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_1238
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_1238
	push   %di
	call   L_1176
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_14b2
	pop    %cx
	or     %ax,%ax
	jge   L_121a
	mov    $0xffff,%si
L_121a:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_122a
	pushw  0x4(%di)
	call   L_13a8
	pop    %cx
L_122a:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_1238
	movw   $0x0,0x4(%di)
L_1238:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* exit: provided by TLVC libc — elided */

L_125c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: cmpw   $0x0,0x375e */
	cmpw   $0x0,D_375e
	jne   L_1287
	/* TODO: data ref: mov    $0x3760,%ax */
	mov    $D_3760,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x375e */
	mov    %ax,D_375e
	/* TODO: data ref: mov    $0x375e,%ax */
	mov    $D_375e,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x3760 */
	mov    %ax,D_3760
	/* TODO: data ref: movw   $0x3760,0x3b70 */
	/* dual-reloc split: movw $D_3760,D_3b70 */
	mov	$D_3760,%ax
	mov	%ax,D_3b70
	/* TODO: data ref: movw   $0x375e,0x3b6e */
	/* dual-reloc split: movw $D_375e,D_3b6e */
	mov	$D_375e,%ax
	mov	%ax,D_3b6e
L_1287:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    0x3b6e,%di */
	mov    D_3b6e,%di
L_129b:
	/* TODO: data ref: movw   $0x0,0x3762 */
	movw   $0x0,D_3762
L_12a1:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_12d0
L_12a8:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_12b7
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_12a8
L_12b7:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_12d0
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_12d0
	jmp   L_1379
L_12d0:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	jbe   L_12df
L_12dd:
	jmp   L_12a1
L_12df:
	/* TODO: data ref: cmp    0x3b70,%si */
	cmp    D_3b70,%si
	jne   L_12eb
	/* TODO: data ref: cmp    $0x375e,%di */
	cmp    $D_375e,%di
	je   L_12ef
L_12eb:
	xor    %ax,%ax
	jmp   L_1327
L_12ef:
	/* TODO: data ref: incw   0x3762 */
	incw   D_3762
	/* TODO: data ref: cmpw   $0x1,0x3762 */
	cmpw   $0x1,D_3762
	jle   L_12dd
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	/* TODO: data ref: mov    %ax,0x3762 */
	mov    %ax,D_3762
	xor    %ax,%ax
	push   %ax
	call   L_148c
	pop    %cx
	mov    %ax,%si
	/* TODO: data ref: mov    0x3762,%ax */
	mov    D_3762,%ax
	shl    %ax
	add    %si,%ax
	add    $0x0,%ax
	cmp    %si,%ax
	jae   L_1329
	xor    %ax,%ax
L_1327:
	jmp   L_133c
L_1329:
	/* TODO: data ref: mov    0x3762,%ax */
	mov    D_3762,%ax
	shl    %ax
	push   %ax
	call   L_148c
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_133e
	xor    %ax,%ax
L_133c:
	jmp   L_13a3
L_133e:
	/* TODO: data ref: mov    0x3b70,%bx */
	mov    D_3b70,%bx
	mov    %si,(%bx)
	/* TODO: data ref: mov    0x3b70,%ax */
	mov    D_3b70,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_135c
	/* TODO: data ref: mov    0x3b70,%bx */
	mov    D_3b70,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x3b70,%bx */
	mov    D_3b70,%bx
	mov    %ax,(%bx)
L_135c:
	/* TODO: data ref: mov    0x3762,%ax */
	mov    D_3762,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	/* TODO: data ref: mov    %ax,0x3b70 */
	mov    %ax,D_3b70
	/* TODO: data ref: mov    $0x375e,%ax */
	mov    $D_375e,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x3b70,%bx */
	mov    D_3b70,%bx
	mov    %ax,(%bx)
	jmp   L_129b
L_1379:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	/* TODO: data ref: mov    %ax,0x3b6e */
	mov    %ax,D_3b6e
	cmp    %ax,%si
	jbe   L_1398
	/* TODO: data ref: mov    0x3b6e,%bx */
	mov    D_3b6e,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x3b72 */
	mov    %ax,D_3b72
	/* TODO: data ref: mov    0x3b6e,%bx */
	mov    D_3b6e,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_1398:
	/* TODO: data ref: mov    0x3b6e,%ax */
	mov    D_3b6e,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_13a3:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_13a8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	/* TODO: data ref: mov    %si,0x3b6e */
	mov    %si,D_3b6e
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
	je   L_13dd
	push   %di
	call   L_13a8
	pop    %cx
L_13dd:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_125c
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_13fc
	cmp    %di,%si
	jne   L_1400
L_13fc:
	mov    %si,%ax
	jmp   L_1468
L_1400:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_141f
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_141f:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_143d
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_141f
L_143d:
	cmp    %di,%si
	jae   L_1466
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_1466
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
	/* TODO: data ref: mov    0x3b72,%ax */
	mov    D_3b72,%ax
	mov    %ax,(%bx)
L_1466:
	mov    %si,%ax
L_1468:
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
	jns    L_1483
	neg	%ax
	mov	%ax,D_3768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_148a
L_1483:
	mov    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x3764 */
	mov    %bx,D_3764
L_148a:
	pop    %bp
	ret


L_148c:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	/* TODO: data ref: add    0x3764,%ax */
	add    D_3764,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_14a3
	neg	%ax
	mov	%ax,D_3768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_14b0
L_14a3:
	/* TODO: data ref: mov    0x3764,%bx */
	mov    D_3764,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x3764 */
	mov    %bx,D_3764
L_14b0:
	pop    %bp
	ret


L_14b2:
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
	jns    L_14c3
	neg	%ax
	mov	%ax,D_3768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_14c3:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_14c6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1537
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_14e3
	xor    %ax,%ax
	jmp   L_14e6
L_14e3:
	mov    $0x1,%ax
L_14e6:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_14ee:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall open(5) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* open: Venix has no mode arg; supply mode=0 for TLVC */
	xchg	%ax,%bx
	mov	%dx,%cx
	xor	%dx,%dx
	mov	$__NR_open,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1502
	neg	%ax
	mov	%ax,D_3768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1502:
	pop    %bp
	ret


L_1504:
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
	jns    L_151b
	neg	%ax
	mov	%ax,D_3768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_151b:
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
	call   L_1550
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1537:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1550
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1550:
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
	jns    L_1567
	neg	%ax
	mov	%ax,D_3768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1567:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_156a:
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
	jns    L_1581
	neg	%ax
	mov	%ax,D_3768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1581:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1584:
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


L_15a0:
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
	jns    L_15b1
	neg	%ax
	mov	%ax,D_3768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_15b1:
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
	mov	%ax,D_3768	/* errno */
	mov	$0xffff,%ax
	jmp	fstat_ret
fstat_ok:
	/* translate TLVC stat -> Venix stat in caller buffer */
	mov	0x6(%bp),%di	/* di = venix_stat_buf */
	lea	-0x20(%bp),%si	/* si = tlvc_stat_buf */
	/* st_dev: same offset +0 */
	mov	(%si),%ax
	mov	%ax,(%di)
	/* st_ino: TLVC +2 (long->short, take low word) -> Venix +2 */
	mov	0x2(%si),%ax
	mov	%ax,0x2(%di)
	/* st_mode: TLVC +6 -> Venix +4 */
	mov	0x6(%si),%ax
	mov	%ax,0x4(%di)
	/* st_nlink: TLVC +8 -> Venix +6 */
	mov	0x8(%si),%ax
	mov	%ax,0x6(%di)
	/* st_uid: TLVC +10 -> Venix +8 */
	mov	0xa(%si),%ax
	mov	%ax,0x8(%di)
	/* st_gid: TLVC +12 -> Venix +10 */
	mov	0xc(%si),%ax
	mov	%ax,0xa(%di)
	/* st_rdev: TLVC +14 -> Venix +12 */
	mov	0xe(%si),%ax
	mov	%ax,0xc(%di)
	/* st_size: TLVC +16 (long) -> Venix +14 (long) */
	mov	0x10(%si),%ax
	mov	%ax,0xe(%di)
	mov	0x12(%si),%ax
	mov	%ax,0x10(%di)
	/* st_atime: TLVC +20 -> Venix +18 */
	mov	0x14(%si),%ax
	mov	%ax,0x12(%di)
	mov	0x16(%si),%ax
	mov	%ax,0x14(%di)
	/* st_mtime: TLVC +24 -> Venix +22 */
	mov	0x18(%si),%ax
	mov	%ax,0x16(%di)
	mov	0x1a(%si),%ax
	mov	%ax,0x18(%di)
	/* st_ctime: TLVC +28 -> Venix +26 */
	mov	0x1c(%si),%ax
	mov	%ax,0x1a(%di)
	mov	0x1e(%si),%ax
	mov	%ax,0x1c(%di)
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
D_35b6:
	.byte	0xbf, 0x0f	/* |..| */
D_35b8:
	.byte	0x72, 0x00	/* |r.| */
D_35ba:
	.byte	0x73, 0x75, 0x6d, 0x3a, 0x20, 0x43, 0x61, 0x6e	/* |sum: Can| */
	.byte	0x27, 0x74, 0x20, 0x6f, 0x70, 0x65, 0x6e, 0x20	/* |'t open | */
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_35ce:
	.byte	0x73, 0x75, 0x6d, 0x3a, 0x20, 0x72, 0x65, 0x61	/* |sum: rea| */
	.byte	0x64, 0x20, 0x65, 0x72, 0x72, 0x6f, 0x72, 0x20	/* |d error | */
	.byte	0x6f, 0x6e, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |on %s..| */
D_35e5:
	.byte	0x2d, 0x00	/* |-.| */
D_35e7:
	.ascii	"%05u%6ld\0"
D_35f0:
	.byte	0x20, 0x25, 0x73, 0x00	/* | %s.| */
D_35f4:
	.byte	0x0a, 0x00	/* |..| */
D_35f6:
	.byte	0x00, 0x00	/* |..| */
D_35f8:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_360c:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_3614:
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
D_3663:
	.byte	0x00	/* |.| */
D_3664:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_366c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_3674:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_367c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_3684:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_368c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_3694:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_369c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_36a4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_36ac:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_36b4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_36bc:
	.byte	0x6e, 0x39, 0x00, 0x00	/* |n9..| */
	.word	D_396e	/* ptr: 0x396e */
	.byte	0x01, 0x00	/* |..| */
D_36c4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_36ca:
	.byte	0x02	/* |.| */
D_36cb:
	.byte	0x01	/* |.| */
D_36cc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
D_36d4:
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
D_375c:
	.word	D_375c	/* ptr: 0x375c */
D_375e:
	.byte	0x00, 0x00	/* |..| */
D_3760:
	.byte	0x00, 0x00	/* |..| */
D_3762:
	.byte	0x00, 0x00	/* |..| */
D_3764:
	.word	D_3b74	/* ptr: 0x3b74 */

	.bss

	.space	2
D_3768:	.space	2	/* bss word @ 0x3768 */
	.space	4
D_376e:	.space	2	/* bss word @ 0x376e */
	.space	510
D_396e:	.space	2	/* bss word @ 0x396e */
	.space	510
D_3b6e:	.space	2	/* bss word @ 0x3b6e */
D_3b70:	.space	2	/* bss word @ 0x3b70 */
D_3b72:	.space	2	/* bss word @ 0x3b72 */
D_3b74:	/* endcore: initial break */

/* end of file */
