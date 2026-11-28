/* cat.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 cat.s -o cat.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: cat  [linked executable]
//   Magic:    0x0107  OMAGIC (object/impure)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x0000162a  (5674 bytes)
//   a_data:   0x000001a6  (422 bytes)
//   Load:  cs:0x0000 (text)   ds:0x362a (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x24,%sp
L_0045:
	movw   $0x0,-0x6(%bp)
	movw   $0xffff,-0xa(%bp)
	/* TODO: data ref: mov    $0x37d8,%ax */
	mov    $D_37d8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x372e,%ax */
	mov    $D_372e,%ax
	push   %ax
	call   L_03be
	add    $0x4,%sp
L_005d:
	cmpw   $0x1,0x4(%bp)
	jle   L_009d
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x2d,(%bx)
	jne   L_009d
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	mov    0x1(%bx),%al
	cbtw
	jmp   L_0089
L_007a:
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x372e,%ax */
	mov    $D_372e,%ax
	push   %ax
	call   L_03be
	add    $0x4,%sp
	jmp   L_0094
L_0089:
	or     %ax,%ax
	je   L_009d
	cmp    $0x75,%ax
	je   L_007a
	jmp   L_009d
L_0094:
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
	jmp   L_005d
L_009d:
	lea    -0x28(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    0x3735,%al */
	mov    D_3735,%al
	cbtw
	push   %ax
	call   L_1526
	add    $0x4,%sp
	andw   $0xe000,-0x24(%bp)
	cmpw   $0xa000,-0x24(%bp)
	je   L_00cb
	cmpw   $0xe000,-0x24(%bp)
	je   L_00cb
	mov    -0x28(%bp),%ax
	mov    %ax,-0x8(%bp)
	mov    -0x26(%bp),%ax
	mov    %ax,-0xa(%bp)
L_00cb:
	cmpw   $0x2,0x4(%bp)
	jge   L_00d9
	movw   $0x2,0x4(%bp)
	incw   -0x6(%bp)
L_00d9:
	decw   0x4(%bp)
	jg   L_00e1
	jmp   L_01c7
L_00e1:
	cmpw   $0x0,-0x6(%bp)
	jne   L_0100
	addw   $0x2,0x6(%bp)
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	cmpb   $0x2d,(%bx)
	jne   L_0105
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	cmpb   $0x0,0x1(%bx)
	jne   L_0105
L_0100:
	/* TODO: data ref: mov    $0x3726,%di */
	mov    $D_3726,%di
	jmp   L_012f
L_0105:
	/* TODO: data ref: mov    $0x362e,%ax */
	mov    $D_362e,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_01fc
	add    $0x4,%sp
	mov    %ax,%di
	or     %ax,%ax
	jne   L_012f
	mov    0x6(%bp),%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x3630,%ax */
	mov    $D_3630,%ax
	push   %ax
	/* TODO: data ref: mov    $0x3736,%ax */
	mov    $D_3736,%ax
	push   %ax
	call   L_01d0
	add    $0x6,%sp
	jmp   L_00d9
L_012f:
	lea    -0x28(%bp),%ax
	push   %ax
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1526
	add    $0x4,%sp
	mov    -0x28(%bp),%ax
	cmp    -0x8(%bp),%ax
	jne   L_0175
	mov    -0x26(%bp),%ax
	cmp    -0xa(%bp),%ax
	jne   L_0175
	cmpw   $0x0,-0x6(%bp)
	je   L_0159
	/* TODO: data ref: mov    $0x365d,%ax */
	mov    $D_365d,%ax
	jmp   L_015e
L_0159:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
L_015e:
	push   %ax
	/* TODO: data ref: mov    $0x3644,%ax */
	mov    $D_3644,%ax
	push   %ax
	/* TODO: data ref: mov    $0x3736,%ax */
	mov    $D_3736,%ax
	push   %ax
	call   L_01d0
	add    $0x6,%sp
	push   %di
	call   L_1245
	pop    %cx
	jmp   L_00d9
L_0175:
	decw   0x2(%di)
	jl   L_0186
	mov    (%di),%bx
	incw   (%di)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_018b
L_0186:
	push   %di
	call   L_0310
	pop    %cx
L_018b:
	mov    %ax,%si
	cmp    $0xffff,%ax
	je   L_01b6
	/* TODO: data ref: decw   0x3730 */
	decw   D_3730
	jl   L_01a9
	/* TODO: data ref: mov    0x372e,%bx */
	mov    D_372e,%bx
	/* TODO: data ref: incw   0x372e */
	incw   D_372e
	mov    %si,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_0175
L_01a9:
	/* TODO: data ref: mov    $0x372e,%ax */
	mov    $D_372e,%ax
	push   %ax
	push   %si
	call   L_10e8
	add    $0x4,%sp
	jmp   L_0175
L_01b6:
	/* TODO: data ref: cmp    $0x3726,%di */
	cmp    $D_3726,%di
	jne   L_01bf
	jmp   L_00d9
L_01bf:
	push   %di
	call   L_1245
	pop    %cx
	jmp   L_00d9
L_01c7:
	xor    %ax,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_01d0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_041e
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_01f6
	mov    $0xffff,%ax
	jmp   L_01f8
L_01f6:
	xor    %ax,%ax
L_01f8:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_01fc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	/* TODO: data ref: cmpb   $0x0,0x3660 */
	cmpb   $0x0,D_3660
	jne   L_0239
	/* TODO: data ref: movw   $0x373e,-0x8(%bp) */
	movw   $D_373e,-0x8(%bp)
	/* TODO: data ref: movw   $0x37c6,0x37c6 */
	/* suppressed: movw $D_37c6,D_37c6 (self-ref in .data) */
	movw   $0x88,-0xa(%bp)
L_021f:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_0234
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_021f
L_0234:
	/* TODO: data ref: movb   $0x1,0x3660 */
	movb   $0x1,D_3660
L_0239:
	/* TODO: data ref: movw   $0x3726,-0x6(%bp) */
	movw   $D_3726,-0x6(%bp)
L_023e:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_025d
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x37c6,%ax */
	cmp    D_37c6,%ax
	jb   L_0257
	xor    %ax,%ax
	jmp   L_02c8
L_0257:
	addw   $0x8,-0x6(%bp)
	jmp   L_023e
L_025d:
	cmpb   $0x77,(%di)
	jne   L_0273
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_02fa
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_02b2
L_0273:
	cmpb   $0x61,(%di)
	jne   L_02b4
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1564
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_02a1
	/* TODO: data ref: cmpw   $0x2,0x39d8 */
	cmpw   $0x2,D_39d8
	jne   L_02a1
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_02fa
	add    $0x4,%sp
	mov    %ax,%si
L_02a1:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_0400
	add    $0x8,%sp
L_02b2:
	jmp   L_02c2
L_02b4:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1564
	add    $0x4,%sp
	mov    %ax,%si
L_02c2:
	or     %si,%si
	jge   L_02ca
	xor    %ax,%ax
L_02c8:
	jmp   L_02f2
L_02ca:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_02e8
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_02ef
L_02e8:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_02ef:
	mov    -0x6(%bp),%ax
L_02f2:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_02fa:
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
	jns    L_030e
	neg	%ax
	mov	%ax,D_39d8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_030e:
	pop    %bp
	ret


L_0310:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_0327
	mov    $0xffff,%ax
	jmp   L_0333
L_0327:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_0335
	mov    $0xffff,%ax
L_0333:
	jmp   L_03ad
L_0335:
	cmpw   $0x0,0x4(%di)
	jne   L_0369
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0350
	mov    0x7(%di),%al
	cbtw
	/* TODO: data ref: add    $0x3662,%ax */
	add    $D_3662,%ax
	mov    %ax,0x4(%di)
	jmp   L_0335
L_0350:
	mov    $0x200,%ax
	push   %ax
	call   L_12bc
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_0365
	orb    $0x4,0x6(%di)
	jmp   L_0335
L_0365:
	orb    $0x8,0x6(%di)
L_0369:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_037c
	mov    $0x1,%ax
	jmp   L_037f
L_037c:
	mov    $0x200,%ax
L_037f:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_157a
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_03af
	cmpw   $0xffff,0x2(%di)
	jne   L_03a1
	orb    $0x10,0x6(%di)
	jmp   L_03a5
L_03a1:
	orb    $0x20,0x6(%di)
L_03a5:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_03ad:
	jmp   L_03b9
L_03af:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_03b9:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_03be:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	cmpw   $0x0,0x4(%di)
	je   L_03dd
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_03dd
	pushw  0x4(%di)
	call   L_1408
	pop    %cx
L_03dd:
	andb   $0xf3,0x6(%di)
	mov    0x6(%bp),%ax
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_03f1
	orb    $0x4,0x6(%di)
	jmp   L_03f6
L_03f1:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
L_03f6:
	movw   $0x0,0x2(%di)
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0400:
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
	jns    L_041b
	neg	%ax
	mov	%ax,D_39d8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_041b:
	pop    %si
	pop    %bp
	ret


L_041e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0436:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_0448
	jmp   L_09e5
L_0448:
	cmpb   $0x25,-0x5(%bp)
	je   L_045b
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_0459:
	jmp   L_0436
L_045b:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0486
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_0ff6
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0486:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_049f
	incb   -0xa6(%bp)
	inc    %di
L_049f:
	cmpb   $0x30,(%di)
	jne   L_04aa
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_04aa:
	cmpb   $0x2a,(%di)
	je   L_04da
	movw   $0x0,-0xe(%bp)
L_04b4:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_04f0
	cmpw   $0x9,-0xc(%bp)
	jg   L_04f0
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_04b4
L_04da:
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
L_04f0:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_0537
	cmpb   $0x2a,(%di)
	je   L_0539
	movw   $0x0,-0x10(%bp)
L_0509:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0533
	cmpw   $0x9,-0xc(%bp)
	jg   L_0533
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_0509
L_0533:
	addw   $0x30,-0xc(%bp)
L_0537:
	jmp   L_0550
L_0539:
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
L_0550:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_095a
L_055b:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_056c
L_0563:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_059c
	jmp   L_0595
L_056c:
	cmp    $0x64,%ax
	je   L_059e
	cmp    $0x6f,%ax
	je   L_0582
	cmp    $0x75,%ax
	je   L_0595
	cmp    $0x78,%ax
	je   L_0590
	jmp   L_0563
L_0582:
	movw   $0x8,-0x12(%bp)
	jmp   L_0595
L_0589:
	movw   $0x8,-0x12(%bp)
	jmp   L_059c
L_0590:
	movw   $0x10,-0x12(%bp)
L_0595:
	jmp   L_05e7
L_0597:
	movw   $0x10,-0x12(%bp)
L_059c:
	jmp   L_05f7
L_059e:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_05bb
L_05ae:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_05bb:
	cmpw   $0x0,-0x14(%bp)
	jg   L_0605
	jl   L_05c9
	cmpw   $0x0,-0x16(%bp)
	jae   L_0605
L_05c9:
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
	jmp   L_0605
L_05e7:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0605
L_05f7:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0605:
	cmpw   $0x0,-0x14(%bp)
	jne   L_060f
	cmpw   $0x0,-0x16(%bp)
L_060f:
	jne   L_0620
	cmpw   $0x0,-0x10(%bp)
	jne   L_0620
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_0620:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0627:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0631
	cmpw   $0x0,-0x16(%bp)
L_0631:
	je   L_067c
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_0b44
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_0652
	mov    $0x30,%ax
	jmp   L_0655
L_0652:
	mov    $0x57,%ax
L_0655:
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
	call   L_0a3c
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_0627
L_067c:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_06cb
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_067c
L_069a:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_06a6:
	cmpw   $0x2,-0xc(%bp)
	jge   L_06cb
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_06c6
	decw   -0x8(%bp)
L_06c6:
	incw   -0xc(%bp)
	jmp   L_06a6
L_06cb:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_06d2:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_06e9
	movw   $0x0,-0xc(%bp)
L_06e9:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_06f8
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_06f8:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_0ff6
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_071b:
	jmp   L_0459
L_071e:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_0730
	/* TODO: data ref: movw   $0x3676,-0xa(%bp) */
	movw   $D_3676,-0xa(%bp)
L_0730:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_073f
	movw   $0x7fff,-0xc(%bp)
L_073f:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0745:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_06d2
	decw   -0xc(%bp)
	jl   L_06d2
	incw   -0x8(%bp)
	jmp   L_0745
L_0757:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_075f:
	jmp   L_071b
L_0761:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_076d
	movw   $0x6,-0x10(%bp)
L_076d:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0d42
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_07a2
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_07a2:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_07b7
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_07cf
L_07b7:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_07b7
L_07cf:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_07e2
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_07e2:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_0801
L_07ee:
	decw   -0xc(%bp)
	jl   L_0801
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_07ee
L_0801:
	cmpw   $0x0,-0xc(%bp)
	jg   L_080a
	jmp   L_06cb
L_080a:
	decw   -0xc(%bp)
	jge   L_0812
	jmp   L_06cb
L_0812:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_080a
L_0827:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0835
	movw   $0x6,-0x10(%bp)
	jmp   L_0838
L_0835:
	incw   -0x10(%bp)
L_0838:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0d16
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_086d
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_086d:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_0878
	incw   -0x1a(%bp)
L_0878:
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
L_089a:
	decw   -0xc(%bp)
	jle   L_08b4
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_089a
L_08b4:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_08d1
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_08e2
L_08d1:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_08e2:
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
	jmp   L_06cb
L_0913:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_091f
	movw   $0x6,-0x10(%bp)
L_091f:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0b9a
	add    $0xc,%sp
	add    $0x8,%si
L_093d:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_093d
	decw   -0x8(%bp)
	jmp   L_06cb
L_094e:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_0957:
	jmp   L_075f
L_095a:
	cmp    $0x65,%ax
	jne   L_0962
	jmp   L_0827
L_0962:
	jg   L_09a3
	cmp    $0x4f,%ax
	jne   L_096c
	jmp   L_0582
L_096c:
	jg   L_0983
	cmp    $0x25,%ax
	je   L_094e
	cmp    $0x44,%ax
	jne   L_097b
	jmp   L_059e
L_097b:
	cmp    $0x4c,%ax
L_097e:
	jne   L_0983
	jmp   L_055b
L_0983:
	cmp    $0x55,%ax
	jne   L_098b
	jmp   L_05e7
L_098b:
	cmp    $0x58,%ax
	jne   L_0993
	jmp   L_0590
L_0993:
	cmp    $0x63,%ax
	jne   L_099b
	jmp   L_069a
L_099b:
	cmp    $0x64,%ax
	jne   L_09a3
	jmp   L_05ae
L_09a3:
	cmp    $0x6f,%ax
	jne   L_09ab
	jmp   L_0589
L_09ab:
	jg   L_09c2
	cmp    $0x66,%ax
	jne   L_09b5
	jmp   L_0761
L_09b5:
	cmp    $0x67,%ax
	jne   L_09bd
	jmp   L_0913
L_09bd:
	cmp    $0x6c,%ax
	je   L_097e
L_09c2:
	cmp    $0x72,%ax
	jne   L_09ca
	jmp   L_0757
L_09ca:
	cmp    $0x73,%ax
	jne   L_09d2
	jmp   L_071e
L_09d2:
	cmp    $0x75,%ax
	jne   L_09da
	jmp   L_05f7
L_09da:
	cmp    $0x78,%ax
	jne   L_09e2
	jmp   L_0597
L_09e2:
	jmp   L_0957
L_09e5:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0a09
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_0ff6
	add    $0x8,%sp
L_0a09:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_0b78


L_0a13:
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
	call   L_0b78


L_0a3c:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_0a5a
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_0a5a:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_0a63:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_0a63
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_0a73:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_0a73
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0a13
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_0ab4
	jne   L_0aa9
	or     $0x0,%ax
	je   L_0ab4
L_0aa9:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_0ab4:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_0b78


L_0abc:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0ad8
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0ad8:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0aef
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0aef:
	push   %dx
	push   %ax
	call   L_0a3c
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_0b09
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_0b09:
	ret
	call   L_0b78
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0abc
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0a13
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_0b78


L_0b44:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0a3c
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0a13
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_0b78:
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


L_0b9a:
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
	call   L_0d16
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_0bd4
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_0bd4:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_0bdb:
	cmpw   $0x0,-0xa(%bp)
	jle   L_0bf5
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_0bf5
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_0bdb
L_0bf5:
	cmpw   $0x0,-0x8(%bp)
	jl   L_0c06
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_0c12
L_0c06:
	cmpw   $0x0,-0x8(%bp)
	jge   L_0c10
	cmpw   $0xfffd,-0x8(%bp)
L_0c10:
	jge   L_0c91
L_0c12:
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
L_0c2d:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_0c47
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_0c2d
L_0c47:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_0c63
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_0c69
L_0c63:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_0c69:
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
	jmp   L_0d01
L_0c91:
	cmpw   $0x0,-0x8(%bp)
	jg   L_0cb3
	cmpb   $0x30,(%di)
	je   L_0ca2
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0ca2:
	cmpw   $0x0,-0x8(%bp)
	jge   L_0cb3
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_0ca2
L_0cb3:
	movw   $0x1,-0xa(%bp)
L_0cb8:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_0ce0
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
	jne   L_0cdb
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0cdb:
	incw   -0xa(%bp)
	jmp   L_0cb8
L_0ce0:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_0d01
L_0ce8:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_0cfb
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_0ce8
L_0cfb:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0d01:
	cmpb   $0x2e,-0x1(%si)
	jne   L_0d08
	dec    %si
L_0d08:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0d16:
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
	call   L_0d6d
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0d42:
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
	call   L_0d6d
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0d6d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_0d80
	movw   $0x0,0xc(%bp)
L_0d80:
	cmpw   $0x4f,0xc(%bp)
	jl   L_0d8b
	movw   $0x4e,0xc(%bp)
L_0d8b:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	/* TODO: data ref: mov    $0x367e,%si */
	mov    $D_367e,%si
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x36ce */
	fcompl D_36ce
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_0dc8
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_0dc8:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_15fa
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	/* TODO: data ref: movw   $0x36ce,-0x16(%bp) */
	movw   $D_36ce,-0x16(%bp)
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x36d6 */
	fcompl D_36d6
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_0e13
	jmp   L_0ec0
L_0e13:
	/* TODO: data ref: movw   $0x36ce,-0x16(%bp) */
	movw   $D_36ce,-0x16(%bp)
L_0e18:
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x36de */
	fcompl D_36de
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_0e34
	jmp   L_0ebe
L_0e34:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	/* TODO: data ref: fdivl  0x36e6 */
	fdivl  D_36e6
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_15fa
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	/* TODO: data ref: fldl   0x36ee */
	fldl   D_36ee
	fstl   -0x1e(%bp)
	/* TODO: data ref: fdivl  0x36f6 */
	fdivl  D_36f6
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	/* TODO: data ref: fmull  0x36fe */
	fmull  D_36fe
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
	jmp   L_0e18
L_0ea7:
	/* TODO: data ref: cmpw   $0x36ce,-0x16(%bp) */
	cmpw   $D_36ce,-0x16(%bp)
	jae   L_0f01
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_0ebe:
	jmp   L_0ea7
L_0ec0:
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x3706 */
	fcompl D_3706
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_0f10
L_0ed9:
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x370e */
	fmull  D_370e
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	/* TODO: data ref: fcompl 0x3716 */
	fcompl D_3716
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_0f01:
	jae   L_0f10
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_0ed9
L_0f10:
	mov    0xc(%bp),%ax
	/* TODO: data ref: add    $0x367e,%ax */
	add    $D_367e,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_0f22
	add    %di,-0x16(%bp)
L_0f22:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	/* TODO: data ref: cmpw   $0x367e,-0x16(%bp) */
	cmpw   $D_367e,-0x16(%bp)
	jae   L_0f38
	/* TODO: data ref: movb   $0x0,0x367e */
	movb   $0x0,D_367e
	/* TODO: data ref: mov    $0x367e,%ax */
	mov    $D_367e,%ax
	jmp   L_0fa3
L_0f38:
	cmp    -0x16(%bp),%si
	ja   L_0f94
	/* TODO: data ref: cmp    $0x36ce,%si */
	cmp    $D_36ce,%si
	jae   L_0f94
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x371e */
	fmull  D_371e
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_15fa
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
	jmp   L_0f38
L_0f94:
	/* TODO: data ref: cmpw   $0x36ce,-0x16(%bp) */
	cmpw   $D_36ce,-0x16(%bp)
	jb   L_0fa5
	/* TODO: data ref: movb   $0x0,0x36cd */
	movb   $0x0,D_36cd
	/* TODO: data ref: mov    $0x367e,%ax */
	mov    $D_367e,%ax
L_0fa3:
	jmp   L_0fef
L_0fa5:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_0fad:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_0fe9
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	/* TODO: data ref: cmpw   $0x367e,-0x16(%bp) */
	cmpw   $D_367e,-0x16(%bp)
	jbe   L_0fcc
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_0fad
L_0fcc:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_0fad
	/* TODO: data ref: cmp    $0x367e,%si */
	cmp    $D_367e,%si
	jbe   L_0fe6
	movb   $0x30,(%si)
L_0fe6:
	inc    %si
	jmp   L_0fad
L_0fe9:
	movb   $0x0,(%si)
	/* TODO: data ref: mov    $0x367e,%ax */
	mov    $D_367e,%ax
L_0fef:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ff6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1003:
	cmpw   $0x0,0x8(%bp)
	jl   L_100c
	jmp   L_109a
L_100c:
	cmpb   $0x2d,(%di)
	jne   L_1049
	cmpw   $0x30,0xc(%bp)
	jne   L_1049
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1038
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
	jmp   L_1048
L_1038:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_10e8
	add    $0x4,%sp
L_1048:
	dec    %si
L_1049:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1067
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
	jmp   L_1073
L_1067:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_10e8
	add    $0x4,%sp
L_1073:
	incw   0x8(%bp)
	jmp   L_1003
L_1078:
	dec    %si
	jl   L_10ae
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_109c
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
L_109a:
	jmp   L_1078
L_109c:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_10e8
	add    $0x4,%sp
	jmp   L_1078
L_10ae:
	cmpw   $0x0,0x8(%bp)
	je   L_10e3
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_10d2
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
	jmp   L_10de
L_10d2:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_10e8
	add    $0x4,%sp
L_10de:
	decw   0x8(%bp)
	jmp   L_10ae
L_10e3:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_10e8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_1103
	mov    $0xffff,%ax
	jmp   L_11ca
L_1103:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1136
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
	call   L_15e0
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_11bb
L_1136:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1188
	/* TODO: data ref: cmp    $0x372e,%di */
	cmp    $D_372e,%di
	jne   L_1161
	/* TODO: data ref: mov    0x3735,%al */
	mov    D_3735,%al
	cbtw
	push   %ax
	call   L_153c
	pop    %cx
	or     %ax,%ax
	je   L_1156
	orb    $0x4,0x6(%di)
	jmp   L_1103
L_1156:
	/* TODO: data ref: movw   $0x39e0,0x4(%di) */
	movw   $D_39e0,0x4(%di)
	/* TODO: data ref: movw   $0x39e0,(%di) */
	movw   $D_39e0,(%di)
	jmp   L_1103
L_1161:
	mov    $0x200,%ax
	push   %ax
	call   L_12bc
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1178
	orb    $0x4,0x6(%di)
	jmp   L_1103
L_1178:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_11aa
L_1188:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_11aa
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_15e0
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_11aa:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_11bb:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_11cc
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_11ca:
	jmp   L_11cf
L_11cc:
	mov    0x4(%bp),%ax
L_11cf:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_11d6:
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
	jne   L_1222
	mov    0x4(%di),%si
	or     %si,%si
	je   L_1222
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_1222
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_15e0
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_1222
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_1224
L_1222:
	xor    %ax,%ax
L_1224:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1229:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x3726,%di */
	mov    $D_3726,%di
L_1231:
	/* TODO: data ref: cmp    0x37c6,%di */
	cmp    D_37c6,%di
	jae   L_1241
	push   %di
	call   L_1245
	pop    %cx
	add    $0x8,%di
	jmp   L_1231
L_1241:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1245:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_1298
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_1298
	push   %di
	call   L_11d6
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1512
	pop    %cx
	or     %ax,%ax
	jge   L_127a
	mov    $0xffff,%si
L_127a:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_128a
	pushw  0x4(%di)
	call   L_1408
	pop    %cx
L_128a:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_1298
	movw   $0x0,0x4(%di)
L_1298:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* exit: provided by TLVC libc — elided */

L_12bc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: cmpw   $0x0,0x37c8 */
	cmpw   $0x0,D_37c8
	jne   L_12e7
	/* TODO: data ref: mov    $0x37ca,%ax */
	mov    $D_37ca,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x37c8 */
	mov    %ax,D_37c8
	/* TODO: data ref: mov    $0x37c8,%ax */
	mov    $D_37c8,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x37ca */
	mov    %ax,D_37ca
	/* TODO: data ref: movw   $0x37ca,0x3de2 */
	/* dual-reloc split: movw $D_37ca,D_3de2 */
	mov	$D_37ca,%ax
	mov	%ax,D_3de2
	/* TODO: data ref: movw   $0x37c8,0x3de0 */
	/* dual-reloc split: movw $D_37c8,D_3de0 */
	mov	$D_37c8,%ax
	mov	%ax,D_3de0
L_12e7:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    0x3de0,%di */
	mov    D_3de0,%di
L_12fb:
	/* TODO: data ref: movw   $0x0,0x37cc */
	movw   $0x0,D_37cc
L_1301:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_1330
L_1308:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_1317
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_1308
L_1317:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_1330
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_1330
	jmp   L_13d9
L_1330:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	jbe   L_133f
L_133d:
	jmp   L_1301
L_133f:
	/* TODO: data ref: cmp    0x3de2,%si */
	cmp    D_3de2,%si
	jne   L_134b
	/* TODO: data ref: cmp    $0x37c8,%di */
	cmp    $D_37c8,%di
	je   L_134f
L_134b:
	xor    %ax,%ax
	jmp   L_1387
L_134f:
	/* TODO: data ref: incw   0x37cc */
	incw   D_37cc
	/* TODO: data ref: cmpw   $0x1,0x37cc */
	cmpw   $0x1,D_37cc
	jle   L_133d
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	/* TODO: data ref: mov    %ax,0x37cc */
	mov    %ax,D_37cc
	xor    %ax,%ax
	push   %ax
	call   L_14ec
	pop    %cx
	mov    %ax,%si
	/* TODO: data ref: mov    0x37cc,%ax */
	mov    D_37cc,%ax
	shl    %ax
	add    %si,%ax
	add    $0x0,%ax
	cmp    %si,%ax
	jae   L_1389
	xor    %ax,%ax
L_1387:
	jmp   L_139c
L_1389:
	/* TODO: data ref: mov    0x37cc,%ax */
	mov    D_37cc,%ax
	shl    %ax
	push   %ax
	call   L_14ec
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_139e
	xor    %ax,%ax
L_139c:
	jmp   L_1403
L_139e:
	/* TODO: data ref: mov    0x3de2,%bx */
	mov    D_3de2,%bx
	mov    %si,(%bx)
	/* TODO: data ref: mov    0x3de2,%ax */
	mov    D_3de2,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_13bc
	/* TODO: data ref: mov    0x3de2,%bx */
	mov    D_3de2,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x3de2,%bx */
	mov    D_3de2,%bx
	mov    %ax,(%bx)
L_13bc:
	/* TODO: data ref: mov    0x37cc,%ax */
	mov    D_37cc,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	/* TODO: data ref: mov    %ax,0x3de2 */
	mov    %ax,D_3de2
	/* TODO: data ref: mov    $0x37c8,%ax */
	mov    $D_37c8,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x3de2,%bx */
	mov    D_3de2,%bx
	mov    %ax,(%bx)
	jmp   L_12fb
L_13d9:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	/* TODO: data ref: mov    %ax,0x3de0 */
	mov    %ax,D_3de0
	cmp    %ax,%si
	jbe   L_13f8
	/* TODO: data ref: mov    0x3de0,%bx */
	mov    D_3de0,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x3de4 */
	mov    %ax,D_3de4
	/* TODO: data ref: mov    0x3de0,%bx */
	mov    D_3de0,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_13f8:
	/* TODO: data ref: mov    0x3de0,%ax */
	mov    D_3de0,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_1403:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1408:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	/* TODO: data ref: mov    %si,0x3de0 */
	mov    %si,D_3de0
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
	je   L_143d
	push   %di
	call   L_1408
	pop    %cx
L_143d:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_12bc
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_145c
	cmp    %di,%si
	jne   L_1460
L_145c:
	mov    %si,%ax
	jmp   L_14c8
L_1460:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_147f
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_147f:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_149d
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_147f
L_149d:
	cmp    %di,%si
	jae   L_14c6
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_14c6
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
	/* TODO: data ref: mov    0x3de4,%ax */
	mov    D_3de4,%ax
	mov    %ax,(%bx)
L_14c6:
	mov    %si,%ax
L_14c8:
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
	jns    L_14e3
	neg	%ax
	mov	%ax,D_39d8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_14ea
L_14e3:
	mov    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x37ce */
	mov    %bx,D_37ce
L_14ea:
	pop    %bp
	ret


L_14ec:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	/* TODO: data ref: add    0x37ce,%ax */
	add    D_37ce,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1503
	neg	%ax
	mov	%ax,D_39d8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1510
L_1503:
	/* TODO: data ref: mov    0x37ce,%bx */
	mov    D_37ce,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x37ce */
	mov    %bx,D_37ce
L_1510:
	pop    %bp
	ret


L_1512:
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
	jns    L_1523
	neg	%ax
	mov	%ax,D_39d8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1523:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1526:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall fstat(28) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* fstat -> __NR_fstat */
	xchg	%ax,%bx
	mov	%dx,%cx
	mov	$__NR_fstat,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_153a
	neg	%ax
	mov	%ax,D_39d8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_153a:
	pop    %bp
	ret


L_153c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_15ad
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_1559
	xor    %ax,%ax
	jmp   L_155c
L_1559:
	mov    $0x1,%ax
L_155c:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1564:
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
	jns    L_1578
	neg	%ax
	mov	%ax,D_39d8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1578:
	pop    %bp
	ret


L_157a:
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
	jns    L_1591
	neg	%ax
	mov	%ax,D_39d8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1591:
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
	call   L_15c6
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_15ad:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_15c6
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_15c6:
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
	jns    L_15dd
	neg	%ax
	mov	%ax,D_39d8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_15dd:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_15e0:
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
	jns    L_15f7
	neg	%ax
	mov	%ax,D_39d8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_15f7:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_15fa:
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


L_1616:
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
	jns    L_1627
	neg	%ax
	mov	%ax,D_39d8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1627:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


	/* synthesized: stty() stub - TIOCSETP not available in TLVC */
stty:
	push	%bp
	mov	%sp,%bp
	xor	%ax,%ax	/* return 0 (pretend success) */
	pop	%bp
	ret

	/* synthesized: gtty() -> return -1 (not a tty) for TLVC */
gtty:
	push	%bp
	mov	%sp,%bp
	mov	$0xffff,%ax	/* return -1: not a tty */
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
D_362c:
	.byte	0xbf, 0x0f	/* |..| */
D_362e:
	.byte	0x72, 0x00	/* |r.| */
D_3630:
	.byte	0x63, 0x61, 0x74, 0x3a, 0x20, 0x63, 0x61, 0x6e	/* |cat: can| */
	.byte	0x27, 0x74, 0x20, 0x6f, 0x70, 0x65, 0x6e, 0x20	/* |'t open | */
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_3644:
	.byte	0x63, 0x61, 0x74, 0x3a, 0x20, 0x69, 0x6e, 0x70	/* |cat: inp| */
	.byte	0x75, 0x74, 0x20, 0x25, 0x73, 0x20, 0x69, 0x73	/* |ut %s is| */
	.byte	0x20, 0x6f, 0x75, 0x74, 0x70, 0x75, 0x74, 0x0a	/* | output.| */
	.byte	0x00	/* |.| */
D_365d:
	.byte	0x2d, 0x00, 0x00	/* |-..| */
D_3660:
	.byte	0x00, 0x00	/* |..| */
D_3662:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_3676:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_367e:
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
D_36cd:
	.byte	0x00	/* |.| */
D_36ce:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_36d6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_36de:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_36e6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_36ee:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_36f6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_36fe:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_3706:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_370e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_3716:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_371e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_3726:
	.word	D_3be0	/* ptr: 0x3be0 */
	.byte	0x00, 0x00	/* |..| */
	.word	D_3be0	/* ptr: 0x3be0 */
	.byte	0x01, 0x00	/* |..| */
D_372e:
	.byte	0x00, 0x00	/* |..| */
D_3730:
	.byte	0x00, 0x00, 0x00, 0x00, 0x02	/* |.....| */
D_3735:
	.byte	0x01	/* |.| */
D_3736:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
D_373e:
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
D_37c6:
	.word	D_37c6	/* ptr: 0x37c6 */
D_37c8:
	.byte	0x00, 0x00	/* |..| */
D_37ca:
	.byte	0x00, 0x00	/* |..| */
D_37cc:
	.byte	0x00, 0x00	/* |..| */
D_37ce:
	.word	D_3de6	/* ptr: 0x3de6 */

	.bss

	.space	8
D_37d8:	.space	2	/* bss word @ 0x37d8 */
	.space	510
D_39d8:	.space	2	/* bss word @ 0x39d8 */
	.space	6
D_39e0:	.space	2	/* bss word @ 0x39e0 */
	.space	510
D_3be0:	.space	2	/* bss word @ 0x3be0 */
	.space	510
D_3de0:	.space	2	/* bss word @ 0x3de0 */
D_3de2:	.space	2	/* bss word @ 0x3de2 */
D_3de4:	.space	2	/* bss word @ 0x3de4 */
D_3de6:	/* endcore: initial break */

/* end of file */
