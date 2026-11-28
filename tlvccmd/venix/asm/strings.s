/* strings.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 strings.s -o strings.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: strings  [linked executable]
//   Magic:    0x0107  OMAGIC (object/impure)
//   a_stack:  0x0000  (0 bytes)
//   a_text:   0x00001bec  (7148 bytes)
//   a_data:   0x00000704  (1796 bytes)
//   Load:  cs:0x0000 (text)   ds:0x1bec (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
L_0042:
	cmpw   $0x0,0x4(%bp)
	jg   L_004b
	jmp   L_013b
L_004b:
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	cmpb   $0x2d,(%bx)
	je   L_0058
	jmp   L_013b
L_0058:
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	cmpb   $0x0,0x1(%bx)
	jne   L_006a
	/* TODO: data ref: incw   0x22f2 */
	incw   D_22f2
	jmp   L_0131
L_006a:
	mov    $0x1,%di
L_006d:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	add    %di,%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	jne   L_007e
	jmp   L_0131
L_007e:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	add    %di,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	jmp   L_011a
L_008d:
	/* TODO: data ref: incw   0x22f4 */
	incw   D_22f4
	jmp   L_0097
L_0093:
	/* TODO: data ref: incw   0x22f2 */
	incw   D_22f2
L_0097:
	jmp   L_0118
L_0099:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	add    %di,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	/* TODO: data ref: mov    0x20f1(%bx),%al */
	mov    D_20f1(%bx),%al
	cbtw
	test   $0x4,%ax
	jne   L_00c7
	/* TODO: data ref: mov    $0x1e03,%ax */
	mov    $D_1e03,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2256,%ax */
	mov    $D_2256,%ax
	push   %ax
	call   L_04e8
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_00c7:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	add    %di,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	/* TODO: data ref: mov    %ax,0x1bf2 */
	mov    %ax,D_1bf2
	inc    %di
L_00da:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	add    %di,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	/* TODO: data ref: mov    0x20f1(%bx),%al */
	mov    D_20f1(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0117
	/* TODO: data ref: mov    0x1bf2,%ax */
	mov    D_1bf2,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    0x6(%bp),%bx
	mov    (%bx),%dx
	add    %di,%dx
	mov    %dx,%bx
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	add    %dx,%ax
	sub    $0x30,%ax
	/* TODO: data ref: mov    %ax,0x1bf2 */
	mov    %ax,D_1bf2
	inc    %di
	jmp   L_00da
L_0117:
	dec    %di
L_0118:
	jmp   L_012d
L_011a:
	cmp    $0x61,%ax
	jne   L_0122
	jmp   L_0093
L_0122:
	cmp    $0x6f,%ax
	jne   L_012a
	jmp   L_008d
L_012a:
	jmp   L_0099
L_012d:
	inc    %di
	jmp   L_006d
L_0131:
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
	jmp   L_0042
L_013b:
	cmpw   $0x0,0x4(%bp)
	jle   L_0178
	/* TODO: data ref: mov    $0x2246,%ax */
	mov    $D_2246,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1e35,%ax */
	mov    $D_1e35,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_0394
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_0169
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_06de
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0169:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x1bf0 */
	mov    %ax,D_1bf0
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
L_0178:
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: mov    $0x2246,%ax */
	mov    $D_2246,%ax
	push   %ax
	call   L_052a
	add    $0x8,%sp
	/* TODO: data ref: cmpw   $0x0,0x22f2 */
	cmpw   $0x0,D_22f2
	jne   L_01cd
	/* TODO: data ref: mov    $0x2246,%ax */
	mov    $D_2246,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x20,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2300,%ax */
	mov    $D_2300,%ax
	push   %ax
	call   L_0778
	add    $0x8,%sp
	cmp    $0x1,%ax
	jne   L_01cd
	/* TODO: data ref: cmpw   $0x107,0x2300 */
	cmpw   $0x107,D_2300
	je   L_01c2
	/* TODO: data ref: cmpw   $0x109,0x2300 */
	cmpw   $0x109,D_2300
	je   L_01c2
	mov    $0x1,%ax
	jmp   L_01c4
L_01c2:
	xor    %ax,%ax
L_01c4:
	cwtd
	or     %dx,%dx
	jne   L_01cb
	or     %ax,%ax
L_01cb:
	je   L_01f0
L_01cd:
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: mov    $0x2246,%ax */
	mov    $D_2246,%ax
	push   %ax
	call   L_052a
	add    $0x8,%sp
	mov    $0xe100,%ax
	mov    $0x5f5,%dx
	push   %dx
	push   %ax
	call   L_0228
	add    $0x4,%sp
	jmp   L_021b
L_01f0:
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    0x2304,%ax */
	mov    D_2304,%ax
	/* TODO: data ref: mov    0x2306,%dx */
	mov    D_2306,%dx
	add    $0x20,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: mov    $0x2246,%ax */
	mov    $D_2246,%ax
	push   %ax
	call   L_052a
	add    $0x8,%sp
	/* TODO: data ref: pushw  0x230a */
	pushw  D_230a
	/* TODO: data ref: pushw  0x2308 */
	pushw  D_2308
	call   L_0228
	add    $0x4,%sp
L_021b:
	cmpw   $0x0,0x4(%bp)
	jle   L_0224
	jmp   L_013b
L_0224:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0228:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x12,%sp
	/* TODO: data ref: mov    $0x1bf4,%di */
	mov    $D_1bf4,%di
	movw   $0x0,-0x6(%bp)
L_0238:
	cmpw   $0x0,0x6(%bp)
	jne   L_0247
	cmpw   $0x0,0x4(%bp)
	jne   L_0247
	jmp   L_034f
L_0247:
	/* TODO: data ref: decw   0x2248 */
	decw   D_2248
	jl   L_025d
	/* TODO: data ref: mov    0x2246,%bx */
	mov    D_2246,%bx
	/* TODO: data ref: incw   0x2246 */
	incw   D_2246
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0265
L_025d:
	/* TODO: data ref: mov    $0x2246,%ax */
	mov    $D_2246,%ax
	push   %ax
	call   L_0850
	pop    %cx
L_0265:
	mov    %ax,%si
	cmp    $0xa,%si
	je   L_0284
	push   %si
	call   L_0356
	pop    %cx
	or     %ax,%ax
	jne   L_0284
	cmpw   $0x0,0x6(%bp)
	jne   L_027f
	cmpw   $0x0,0x4(%bp)
L_027f:
	je   L_0284
	jmp   L_0322
L_0284:
	/* TODO: data ref: cmp    $0x1bf4,%di */
	cmp    $D_1bf4,%di
	jbe   L_0291
	cmpb   $0xa,-0x1(%di)
	jne   L_0291
	dec    %di
L_0291:
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	/* TODO: data ref: mov    0x1bf2,%ax */
	mov    D_1bf2,%ax
	/* TODO: data ref: add    $0x1bf4,%ax */
	add    $D_1bf4,%ax
	cmp    %ax,%di
	jbe   L_0318
	/* TODO: data ref: cmpw   $0x0,0x22f4 */
	cmpw   $0x0,D_22f4
	je   L_030a
	/* TODO: data ref: mov    $0x2246,%ax */
	mov    $D_2246,%ax
	push   %ax
	call   L_0434
	pop    %cx
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
	mov    -0x6(%bp),%ax
	cwtd
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	/* TODO: data ref: mov    $0x2246,%ax */
	mov    $D_2246,%ax
	push   %ax
	call   L_0434
	pop    %cx
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    -0x6(%bp),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	mov    -0xa(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    -0xe(%bp),%ax
	sbb    -0xc(%bp),%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	push   %dx
	push   %ax
	mov    -0x12(%bp),%ax
	mov    -0x10(%bp),%dx
	sub    -0x16(%bp),%ax
	sbb    -0x14(%bp),%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: mov    $0x1e37,%ax */
	mov    $D_1e37,%ax
	push   %ax
	call   L_074e
	add    $0xa,%sp
L_030a:
	/* TODO: data ref: mov    $0x1bf4,%ax */
	mov    $D_1bf4,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1e42,%ax */
	mov    $D_1e42,%ax
	push   %ax
	call   L_074e
	add    $0x4,%sp
L_0318:
	/* TODO: data ref: mov    $0x1bf4,%di */
	mov    $D_1bf4,%di
	movw   $0x0,-0x6(%bp)
	jmp   L_0332
L_0322:
	/* TODO: data ref: cmp    $0x1df2,%di */
	cmp    $D_1df2,%di
	jae   L_032f
	mov    %di,%bx
	inc    %di
	mov    %si,%cx
	mov    %cl,(%bx)
L_032f:
	incw   -0x6(%bp)
L_0332:
	/* TODO: data ref: mov    0x224c,%al */
	mov    D_224c,%al
	cbtw
	test   $0x20,%ax
	jne   L_034f
	/* TODO: data ref: mov    0x224c,%al */
	mov    D_224c,%al
	cbtw
	test   $0x10,%ax
	jne   L_034f
	subw   $0x1,0x4(%bp)
	sbbw   $0x0,0x6(%bp)
	jmp   L_0238
L_034f:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0356:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	jmp   L_037f
L_0360:
	xor    %ax,%ax
	jmp   L_0390
L_0364:
	mov    $0x1,%ax
	jmp   L_0390
L_0369:
	cmpw   $0x80,0x4(%bp)
	jg   L_0376
	cmpw   $0x20,0x4(%bp)
	jge   L_037b
L_0376:
	mov    $0x1,%ax
	jmp   L_0390
L_037b:
	xor    %ax,%ax
	jmp   L_0390
L_037f:
	cmp    $0xa,%ax
	je   L_0360
	cmp    $0xc,%ax
	je   L_0360
	cmp    $0x7f,%ax
	je   L_0364
	jmp   L_0369
L_0390:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0394:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	push   %si
	call   L_181f
	pop    %cx
	cmpb   $0x77,(%di)
	jne   L_03be
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0514
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	jmp   L_03fa
L_03be:
	cmpb   $0x61,(%di)
	jne   L_03fc
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1b26
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jge   L_03e7
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0514
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_03e7:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	call   L_091a
	add    $0x8,%sp
L_03fa:
	jmp   L_040b
L_03fc:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1b26
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_040b:
	cmpw   $0x0,-0x6(%bp)
	jge   L_0415
	xor    %ax,%ax
	jmp   L_042e
L_0415:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,0x7(%si)
	cmpb   $0x72,(%di)
	je   L_0428
	orb    $0x2,0x6(%si)
	jmp   L_042c
L_0428:
	orb    $0x1,0x6(%si)
L_042c:
	mov    %si,%ax
L_042e:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0434:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x2(%bx)
	jge   L_044d
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
L_044d:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_0465
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%ax
	neg    %ax
	mov    %ax,%di
	jmp   L_049e
L_0465:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
	je   L_0497
	xor    %di,%di
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	je   L_049e
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	jne   L_049e
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	mov    0x4(%bp),%bx
	sub    0x4(%bx),%ax
	mov    %ax,%di
	jmp   L_049e
L_0497:
	mov    $0xffff,%ax
	mov    %ax,%dx
	jmp   L_04e0
L_049e:
	mov    $0x1,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_091a
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmp    $0x0,%dx
	jg   L_04d1
	jl   L_04c9
	cmpw   $0x0,-0x8(%bp)
	jae   L_04d1
L_04c9:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	jmp   L_04e0
L_04d1:
	mov    %di,%ax
	cwtd
	add    %ax,-0x8(%bp)
	adc    %dx,-0x6(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
L_04e0:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_04e8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_0938
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_050e
	mov    $0xffff,%ax
	jmp   L_0510
L_050e:
	xor    %ax,%ax
L_0510:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0514:
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
	jns    L_0528
	neg	%ax
	mov	%ax,D_2320	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_0528:
	pop    %bp
	ret


L_052a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	mov    0x4(%bp),%bx
	andb   $0xef,0x6(%bx)
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x1,%ax
	jne   L_0548
	jmp   L_0691
L_0548:
	cmpw   $0x2,0xa(%bp)
	jl   L_0551
	jmp   L_0632
L_0551:
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_055d
	jmp   L_0632
L_055d:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_056c
	jmp   L_0632
L_056c:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%si
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,0xa(%bp)
	jne   L_05c5
	mov    $0x1,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_091a
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    %si,%ax
	cwtd
	sub    -0xc(%bp),%ax
	sbb    -0xa(%bp),%dx
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    -0x10(%bp),%ax
	adc    -0xe(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	jmp   L_05e0
L_05c5:
	mov    %si,%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	sub    -0xc(%bp),%ax
	sbb    -0xa(%bp),%dx
	mov    %ax,0x6(%bp)
	mov    %dx,0x8(%bp)
L_05e0:
	or     %si,%si
	jle   L_0621
	mov    %si,%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jg   L_0621
	jl   L_05f3
	cmp    %ax,-0x8(%bp)
	ja   L_0621
L_05f3:
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x4(%bp),%bx
	sub    (%bx),%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jl   L_0621
	jg   L_060b
	cmp    %ax,-0x8(%bp)
	jb   L_0621
L_060b:
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	add    %ax,(%bx)
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	sub    %ax,0x2(%bx)
	xor    %ax,%ax
	jmp   L_06d2
L_0621:
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x1,%ax
	and    $0x0,%dx
	mov    %ax,%di
	jmp   L_0634
L_0632:
	xor    %di,%di
L_0634:
	mov    %di,%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	pushw  0xa(%bp)
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	sub    -0xc(%bp),%ax
	sbb    -0xa(%bp),%dx
	push   %dx
	push   %ax
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_091a
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
	or     %di,%di
	je   L_069b
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_0688
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_06c1
L_0688:
	pushw  0x4(%bp)
	call   L_0850
	pop    %cx
	jmp   L_06c1
L_0691:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
L_069b:
	je   L_06c1
	pushw  0x4(%bp)
	call   L_17b0
	pop    %cx
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_091a
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_06c1:
	cmpw   $0xffff,-0x6(%bp)
	jne   L_06d4
	cmpw   $0xffff,-0x8(%bp)
	jne   L_06d4
	mov    $0xffff,%ax
L_06d2:
	jmp   L_06d6
L_06d4:
	xor    %ax,%ax
L_06d6:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_06de:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x1e46,%di */
	mov    $D_1e46,%di
	/* TODO: data ref: mov    0x2320,%ax */
	mov    D_2320,%ax
	/* TODO: data ref: cmp    0x1ea0,%ax */
	cmp    D_1ea0,%ax
	jge   L_06f9
	/* TODO: data ref: mov    0x2320,%bx */
	mov    D_2320,%bx
	shl    %bx
	/* TODO: data ref: mov    0x1e5a(%bx),%di */
	mov    D_1e5a(%bx),%di
L_06f9:
	pushw  0x4(%bp)
	call   L_08fe
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_0726
	push   %si
	pushw  0x4(%bp)
	mov    $0x2,%ax
	push   %ax
	call   L_1ba2
	add    $0x6,%sp
	mov    $0x2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1e54,%ax */
	mov    $D_1e54,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_1ba2
	add    $0x6,%sp
L_0726:
	push   %di
	call   L_08fe
	pop    %cx
	push   %ax
	push   %di
	mov    $0x2,%ax
	push   %ax
	call   L_1ba2
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1e57,%ax */
	mov    $D_1e57,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_1ba2
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_074e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x224e,%ax */
	mov    $D_224e,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0938
	add    $0x6,%sp
	/* TODO: data ref: mov    0x2254,%al */
	mov    D_2254,%al
	cbtw
	test   $0x20,%ax
	je   L_0772
	mov    $0xffff,%ax
	jmp   L_0774
L_0772:
	xor    %ax,%ax
L_0774:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0778:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_07d9
L_0793:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_07d9
	mov    0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
L_07a1:
	decw   0x2(%si)
	jl   L_07b2
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_07b7
L_07b2:
	push   %si
	call   L_0850
	pop    %cx
L_07b7:
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jl   L_07ca
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_07cf
L_07ca:
	mov    -0x8(%bp),%ax
	jmp   L_07dc
L_07cf:
	decw   -0xa(%bp)
	jne   L_07a1
	incw   -0x8(%bp)
	jmp   L_0793
L_07d9:
	mov    -0x8(%bp),%ax
L_07dc:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_0845
L_07fe:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_0845
	mov    0x6(%bp),%ax
	mov    %ax,-0x6(%bp)
L_080c:
	decw   0x2(%si)
	jl   L_0824
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    (%si),%bx
	incw   (%si)
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_0832
L_0824:
	push   %si
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_16c2
	add    $0x4,%sp
L_0832:
	decw   -0x6(%bp)
	jne   L_080c
	mov    0x6(%si),%al
	cbtw
	test   $0x20,%ax
	jne   L_0845
	incw   -0x8(%bp)
	jmp   L_07fe
L_0845:
	mov    -0x8(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0850:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_0867
	mov    $0xffff,%ax
	jmp   L_0873
L_0867:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_0875
	mov    $0xffff,%ax
L_0873:
	jmp   L_08ed
L_0875:
	cmpw   $0x0,0x4(%di)
	jne   L_08a9
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0890
	mov    0x7(%di),%al
	cbtw
	/* TODO: data ref: add    $0x2172,%ax */
	add    $D_2172,%ax
	mov    %ax,0x4(%di)
	jmp   L_0875
L_0890:
	mov    $0x200,%ax
	push   %ax
	call   L_1896
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_08a5
	orb    $0x4,0x6(%di)
	jmp   L_0875
L_08a5:
	orb    $0x8,0x6(%di)
L_08a9:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_08bc
	mov    $0x1,%ax
	jmp   L_08bf
L_08bc:
	mov    $0x200,%ax
L_08bf:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1b3c
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_08ef
	cmpw   $0xffff,0x2(%di)
	jne   L_08e1
	orb    $0x10,0x6(%di)
	jmp   L_08e5
L_08e1:
	orb    $0x20,0x6(%di)
L_08e5:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_08ed:
	jmp   L_08f9
L_08ef:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_08f9:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_08fe:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_0909:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_0914
	inc    %si
	jmp   L_0909
L_0914:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_091a:
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
L_0935:
	pop    %si
	pop    %bp
	ret


L_0938:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0950:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_0962
	jmp   L_0eff
L_0962:
	cmpb   $0x25,-0x5(%bp)
	je   L_0975
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_0973:
	jmp   L_0950
L_0975:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_09a0
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_15d0
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_09a0:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_09b9
	incb   -0xa6(%bp)
	inc    %di
L_09b9:
	cmpb   $0x30,(%di)
	jne   L_09c4
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_09c4:
	cmpb   $0x2a,(%di)
	je   L_09f4
	movw   $0x0,-0xe(%bp)
L_09ce:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0a0a
	cmpw   $0x9,-0xc(%bp)
	jg   L_0a0a
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_09ce
L_09f4:
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
L_0a0a:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_0a51
	cmpb   $0x2a,(%di)
	je   L_0a53
	movw   $0x0,-0x10(%bp)
L_0a23:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0a4d
	cmpw   $0x9,-0xc(%bp)
	jg   L_0a4d
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_0a23
L_0a4d:
	addw   $0x30,-0xc(%bp)
L_0a51:
	jmp   L_0a6a
L_0a53:
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
L_0a6a:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_0e74
L_0a75:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_0a86
L_0a7d:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_0ab6
	jmp   L_0aaf
L_0a86:
	cmp    $0x64,%ax
	je   L_0ab8
	cmp    $0x6f,%ax
	je   L_0a9c
	cmp    $0x75,%ax
	je   L_0aaf
	cmp    $0x78,%ax
	je   L_0aaa
	jmp   L_0a7d
L_0a9c:
	movw   $0x8,-0x12(%bp)
	jmp   L_0aaf
L_0aa3:
	movw   $0x8,-0x12(%bp)
	jmp   L_0ab6
L_0aaa:
	movw   $0x10,-0x12(%bp)
L_0aaf:
	jmp   L_0b01
L_0ab1:
	movw   $0x10,-0x12(%bp)
L_0ab6:
	jmp   L_0b11
L_0ab8:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0ad5
L_0ac8:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0ad5:
	cmpw   $0x0,-0x14(%bp)
	jg   L_0b1f
	jl   L_0ae3
	cmpw   $0x0,-0x16(%bp)
	jae   L_0b1f
L_0ae3:
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
	jmp   L_0b1f
L_0b01:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0b1f
L_0b11:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0b1f:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0b29
	cmpw   $0x0,-0x16(%bp)
L_0b29:
	jne   L_0b3a
	cmpw   $0x0,-0x10(%bp)
	jne   L_0b3a
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_0b3a:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0b41:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0b4b
	cmpw   $0x0,-0x16(%bp)
L_0b4b:
	je   L_0b96
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_105e
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_0b6c
	mov    $0x30,%ax
	jmp   L_0b6f
L_0b6c:
	mov    $0x57,%ax
L_0b6f:
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
	call   L_0f56
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_0b41
L_0b96:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_0be5
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_0b96
L_0bb4:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_0bc0:
	cmpw   $0x2,-0xc(%bp)
	jge   L_0be5
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0be0
	decw   -0x8(%bp)
L_0be0:
	incw   -0xc(%bp)
	jmp   L_0bc0
L_0be5:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0bec:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_0c03
	movw   $0x0,-0xc(%bp)
L_0c03:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_0c12
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_0c12:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_15d0
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0c35:
	jmp   L_0973
L_0c38:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_0c4a
	/* TODO: data ref: movw   $0x2186,-0xa(%bp) */
	movw   $D_2186,-0xa(%bp)
L_0c4a:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_0c59
	movw   $0x7fff,-0xc(%bp)
L_0c59:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0c5f:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0bec
	decw   -0xc(%bp)
	jl   L_0bec
	incw   -0x8(%bp)
	jmp   L_0c5f
L_0c71:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_0c79:
	jmp   L_0c35
L_0c7b:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0c87
	movw   $0x6,-0x10(%bp)
L_0c87:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_125c
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0cbc
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0cbc:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_0cd1
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_0ce9
L_0cd1:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_0cd1
L_0ce9:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_0cfc
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_0cfc:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_0d1b
L_0d08:
	decw   -0xc(%bp)
	jl   L_0d1b
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_0d08
L_0d1b:
	cmpw   $0x0,-0xc(%bp)
	jg   L_0d24
	jmp   L_0be5
L_0d24:
	decw   -0xc(%bp)
	jge   L_0d2c
	jmp   L_0be5
L_0d2c:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_0d24
L_0d41:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0d4f
	movw   $0x6,-0x10(%bp)
	jmp   L_0d52
L_0d4f:
	incw   -0x10(%bp)
L_0d52:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1230
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0d87
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0d87:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_0d92
	incw   -0x1a(%bp)
L_0d92:
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
L_0db4:
	decw   -0xc(%bp)
	jle   L_0dce
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_0db4
L_0dce:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_0deb
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_0dfc
L_0deb:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_0dfc:
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
	jmp   L_0be5
L_0e2d:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0e39
	movw   $0x6,-0x10(%bp)
L_0e39:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_10b4
	add    $0xc,%sp
	add    $0x8,%si
L_0e57:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_0e57
	decw   -0x8(%bp)
	jmp   L_0be5
L_0e68:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_0e71:
	jmp   L_0c79
L_0e74:
	cmp    $0x65,%ax
	jne   L_0e7c
	jmp   L_0d41
L_0e7c:
	jg   L_0ebd
	cmp    $0x4f,%ax
	jne   L_0e86
	jmp   L_0a9c
L_0e86:
	jg   L_0e9d
	cmp    $0x25,%ax
	je   L_0e68
	cmp    $0x44,%ax
	jne   L_0e95
	jmp   L_0ab8
L_0e95:
	cmp    $0x4c,%ax
L_0e98:
	jne   L_0e9d
	jmp   L_0a75
L_0e9d:
	cmp    $0x55,%ax
	jne   L_0ea5
	jmp   L_0b01
L_0ea5:
	cmp    $0x58,%ax
	jne   L_0ead
	jmp   L_0aaa
L_0ead:
	cmp    $0x63,%ax
	jne   L_0eb5
	jmp   L_0bb4
L_0eb5:
	cmp    $0x64,%ax
	jne   L_0ebd
	jmp   L_0ac8
L_0ebd:
	cmp    $0x6f,%ax
	jne   L_0ec5
	jmp   L_0aa3
L_0ec5:
	jg   L_0edc
	cmp    $0x66,%ax
	jne   L_0ecf
	jmp   L_0c7b
L_0ecf:
	cmp    $0x67,%ax
	jne   L_0ed7
	jmp   L_0e2d
L_0ed7:
	cmp    $0x6c,%ax
	je   L_0e98
L_0edc:
	cmp    $0x72,%ax
	jne   L_0ee4
	jmp   L_0c71
L_0ee4:
	cmp    $0x73,%ax
	jne   L_0eec
	jmp   L_0c38
L_0eec:
	cmp    $0x75,%ax
	jne   L_0ef4
	jmp   L_0b11
L_0ef4:
	cmp    $0x78,%ax
	jne   L_0efc
	jmp   L_0ab1
L_0efc:
	jmp   L_0e71
L_0eff:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0f23
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_15d0
	add    $0x8,%sp
L_0f23:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_1092


L_0f2d:
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
	call   L_1092


L_0f56:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_0f74
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_0f74:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_0f7d:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_0f7d
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_0f8d:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_0f8d
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0f2d
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_0fce
	jne   L_0fc3
	or     $0x0,%ax
	je   L_0fce
L_0fc3:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_0fce:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_1092


L_0fd6:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0ff2
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0ff2:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_1009
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_1009:
	push   %dx
	push   %ax
	call   L_0f56
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_1023
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_1023:
	ret
	call   L_1092
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0fd6
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0f2d
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_1092


L_105e:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0f56
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0f2d
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_1092:
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


L_10b4:
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
	call   L_1230
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_10ee
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_10ee:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_10f5:
	cmpw   $0x0,-0xa(%bp)
	jle   L_110f
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_110f
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_10f5
L_110f:
	cmpw   $0x0,-0x8(%bp)
	jl   L_1120
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_112c
L_1120:
	cmpw   $0x0,-0x8(%bp)
	jge   L_112a
	cmpw   $0xfffd,-0x8(%bp)
L_112a:
	jge   L_11ab
L_112c:
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
L_1147:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_1161
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_1147
L_1161:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_117d
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_1183
L_117d:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_1183:
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
	jmp   L_121b
L_11ab:
	cmpw   $0x0,-0x8(%bp)
	jg   L_11cd
	cmpb   $0x30,(%di)
	je   L_11bc
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_11bc:
	cmpw   $0x0,-0x8(%bp)
	jge   L_11cd
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_11bc
L_11cd:
	movw   $0x1,-0xa(%bp)
L_11d2:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_11fa
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
	jne   L_11f5
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_11f5:
	incw   -0xa(%bp)
	jmp   L_11d2
L_11fa:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_121b
L_1202:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_1215
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_1202
L_1215:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_121b:
	cmpb   $0x2e,-0x1(%si)
	jne   L_1222
	dec    %si
L_1222:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1230:
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
	call   L_1287
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_125c:
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
	call   L_1287
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1287:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_129a
	movw   $0x0,0xc(%bp)
L_129a:
	cmpw   $0x4f,0xc(%bp)
	jl   L_12a5
	movw   $0x4e,0xc(%bp)
L_12a5:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	/* TODO: data ref: mov    $0x218e,%si */
	mov    $D_218e,%si
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x21de */
	fcompl D_21de
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jae   L_12df
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x46(%bp)
	fstpl  0x4(%bp)
L_12df:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1bbc
	fstpl  -0x46(%bp)
	add    $0xa,%sp
	fldl   -0x46(%bp)
	fstpl  0x4(%bp)
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x21e6 */
	fcompl D_21e6
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jne   L_1320
	jmp   L_13f6
L_1320:
	/* TODO: data ref: mov    $0x218e,%si */
	mov    $D_218e,%si
L_1323:
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x21ee */
	fcompl D_21ee
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jb   L_13af
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	/* TODO: data ref: fmull  0x21f6 */
	fmull  D_21f6
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1bbc
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	fldl   -0x4e(%bp)
	/* TODO: data ref: fmull  0x21fe */
	fmull  D_21fe
	/* TODO: data ref: faddl  0x2206 */
	faddl  D_2206
	fistps -0x60(%bp)
	fwait
	mov    -0x60(%bp),%ax
	lea    -0x3e(%bp),%dx
	mov    %di,%bx
	inc    %di
	shl    %bx
	add    %bx,%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	/* TODO: data ref: cmp    $0x2710,%ax */
	cmp    $D_2710,%ax
	jne   L_1323
	fldl   -0xc(%bp)
	/* TODO: data ref: faddl  0x220e */
	faddl  D_220e
	fstpl  -0xc(%bp)
	lea    -0x3e(%bp),%ax
	lea    -0x1(%di),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	jmp   L_1323
L_13af:
	mov    $0x1,%ax
	push   %ax
	push   %si
	fldl   -0xc(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   L_155a
	add    $0x6,%sp
	mov    %ax,%si
L_13ca:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_13ec
	xor    %ax,%ax
	push   %ax
	push   %si
	lea    -0x3e(%bp),%ax
	mov    %di,%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_155a
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_13ca
L_13ec:
	mov    %si,%ax
	/* TODO: data ref: sub    $0x218e,%ax */
	sub    $D_218e,%ax
	mov    %ax,%di
	jmp   L_1476
L_13f6:
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x2216 */
	fcompl D_2216
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jbe   L_1476
L_140c:
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x221e */
	fmull  D_221e
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x2226 */
	fcompl D_2226
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_1442
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	sub    $0x3,%di
	jmp   L_140c
L_1442:
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x222e */
	fmull  D_222e
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x2236 */
	fcompl D_2236
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_1476
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_1442
L_1476:
	mov    0xc(%bp),%ax
	/* TODO: data ref: add    $0x218e,%ax */
	add    $D_218e,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_1488
	add    %di,-0x16(%bp)
L_1488:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	/* TODO: data ref: cmpw   $0x218e,-0x16(%bp) */
	cmpw   $D_218e,-0x16(%bp)
	jae   L_149e
	/* TODO: data ref: movb   $0x0,0x218e */
	movb   $0x0,D_218e
	/* TODO: data ref: mov    $0x218e,%ax */
	mov    $D_218e,%ax
	jmp   L_1507
L_149e:
	cmp    -0x16(%bp),%si
	ja   L_14f8
	/* TODO: data ref: cmp    $0x21de,%si */
	cmp    $D_21de,%si
	jae   L_14f8
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x223e */
	fmull  D_223e
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1bbc
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	fldl   -0x4e(%bp)
	fstpl  0x4(%bp)
	xor    %ax,%ax
	push   %ax
	push   %si
	fldl   -0x14(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   L_155a
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_149e
L_14f8:
	/* TODO: data ref: cmpw   $0x21de,-0x16(%bp) */
	cmpw   $D_21de,-0x16(%bp)
	jb   L_1509
	/* TODO: data ref: movb   $0x0,0x21dd */
	movb   $0x0,D_21dd
	/* TODO: data ref: mov    $0x218e,%ax */
	mov    $D_218e,%ax
L_1507:
	jmp   L_1553
L_1509:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_1511:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_154d
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	/* TODO: data ref: cmpw   $0x218e,-0x16(%bp) */
	cmpw   $D_218e,-0x16(%bp)
	jbe   L_1530
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_1511
L_1530:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_1511
	/* TODO: data ref: cmp    $0x218e,%si */
	cmp    $D_218e,%si
	jbe   L_154a
	movb   $0x30,(%si)
L_154a:
	inc    %si
	jmp   L_1511
L_154d:
	movb   $0x0,(%si)
	/* TODO: data ref: mov    $0x218e,%ax */
	mov    $D_218e,%ax
L_1553:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_155a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%si
	xor    %di,%di
L_1565:
	or     %si,%si
	je   L_1576
	mov    %si,%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	mov    %ax,%si
	inc    %di
	jmp   L_1565
L_1576:
	cmpw   $0x0,0x8(%bp)
	jne   L_158f
	mov    %di,%si
L_157e:
	cmp    $0x4,%si
	jge   L_158f
	mov    0x6(%bp),%bx
	movb   $0x30,(%bx)
	incw   0x6(%bp)
	inc    %si
	jmp   L_157e
L_158f:
	mov    0x6(%bp),%ax
	add    %di,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%si
L_159f:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_15c8
	mov    %si,%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	add    $0x30,%dx
	mov    0x6(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	mov    %dx,%cx
	mov    %cl,(%bx)
	mov    %si,%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	mov    %ax,%si
	jmp   L_159f
L_15c8:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_15d0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_15dd:
	cmpw   $0x0,0x8(%bp)
	jl   L_15e6
	jmp   L_1674
L_15e6:
	cmpb   $0x2d,(%di)
	jne   L_1623
	cmpw   $0x30,0xc(%bp)
	jne   L_1623
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1612
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
	jmp   L_1622
L_1612:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_16c2
	add    $0x4,%sp
L_1622:
	dec    %si
L_1623:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1641
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
	jmp   L_164d
L_1641:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_16c2
	add    $0x4,%sp
L_164d:
	incw   0x8(%bp)
	jmp   L_15dd
L_1652:
	dec    %si
	jl   L_1688
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1676
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
L_1674:
	jmp   L_1652
L_1676:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_16c2
	add    $0x4,%sp
	jmp   L_1652
L_1688:
	cmpw   $0x0,0x8(%bp)
	je   L_16bd
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_16ac
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
	jmp   L_16b8
L_16ac:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_16c2
	add    $0x4,%sp
L_16b8:
	decw   0x8(%bp)
	jmp   L_1688
L_16bd:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_16c2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_16dd
	mov    $0xffff,%ax
	jmp   L_17a4
L_16dd:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1710
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
	call   L_1ba2
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_1795
L_1710:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1762
	/* TODO: data ref: cmp    $0x224e,%di */
	cmp    $D_224e,%di
	jne   L_173b
	/* TODO: data ref: mov    0x2255,%al */
	mov    D_2255,%al
	cbtw
	push   %ax
	call   L_1afe
	pop    %cx
	or     %ax,%ax
	je   L_1730
	orb    $0x4,0x6(%di)
	jmp   L_16dd
L_1730:
	/* TODO: data ref: movw   $0x2328,0x4(%di) */
	movw   $D_2328,0x4(%di)
	/* TODO: data ref: movw   $0x2328,(%di) */
	movw   $D_2328,(%di)
	jmp   L_16dd
L_173b:
	mov    $0x200,%ax
	push   %ax
	call   L_1896
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1752
	orb    $0x4,0x6(%di)
	jmp   L_16dd
L_1752:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_1784
L_1762:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_1784
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1ba2
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_1784:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_1795:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_17a6
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_17a4:
	jmp   L_17a9
L_17a6:
	mov    0x4(%bp),%ax
L_17a9:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_17b0:
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
	jne   L_17fc
	mov    0x4(%di),%si
	or     %si,%si
	je   L_17fc
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_17fc
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1ba2
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_17fc
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_17fe
L_17fc:
	xor    %ax,%ax
L_17fe:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1803:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x2246,%di */
	mov    $D_2246,%di
L_180b:
	/* TODO: data ref: cmp    0x22e6,%di */
	cmp    D_22e6,%di
	jae   L_181b
	push   %di
	call   L_181f
	pop    %cx
	add    $0x8,%di
	jmp   L_180b
L_181b:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_181f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_1872
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_1872
	push   %di
	call   L_17b0
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1aea
	pop    %cx
	or     %ax,%ax
	jge   L_1854
	mov    $0xffff,%si
L_1854:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_1864
	pushw  0x4(%di)
	call   L_19e0
	pop    %cx
L_1864:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_1872
	movw   $0x0,0x4(%di)
L_1872:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_1896:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: cmpw   $0x0,0x22e8 */
	cmpw   $0x0,D_22e8
	jne   L_18c1
	/* TODO: data ref: mov    $0x22ea,%ax */
	mov    $D_22ea,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x22e8 */
	mov    %ax,D_22e8
	/* TODO: data ref: mov    $0x22e8,%ax */
	mov    $D_22e8,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x22ea */
	mov    %ax,D_22ea
	/* TODO: data ref: movw   $0x22ea,0x272a */
	/* dual-reloc split: movw $D_22ea,D_272a */
	mov	$D_22ea,%ax
	mov	%ax,D_272a
	/* TODO: data ref: movw   $0x22e8,0x2728 */
	/* dual-reloc split: movw $D_22e8,D_2728 */
	mov	$D_22e8,%ax
	mov	%ax,D_2728
L_18c1:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    0x2728,%di */
	mov    D_2728,%di
L_18d5:
	/* TODO: data ref: movw   $0x0,0x22ec */
	movw   $0x0,D_22ec
L_18db:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_190a
L_18e2:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_18f1
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_18e2
L_18f1:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_190a
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_190a
	jmp   L_19b1
L_190a:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_18db
	/* TODO: data ref: cmp    0x272a,%si */
	cmp    D_272a,%si
	jne   L_1923
	/* TODO: data ref: cmp    $0x22e8,%di */
	cmp    $D_22e8,%di
	je   L_1927
L_1923:
	xor    %ax,%ax
	jmp   L_195f
L_1927:
	/* TODO: data ref: incw   0x22ec */
	incw   D_22ec
	/* TODO: data ref: cmpw   $0x1,0x22ec */
	cmpw   $0x1,D_22ec
	jle   L_18db
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	/* TODO: data ref: mov    %ax,0x22ec */
	mov    %ax,D_22ec
	xor    %ax,%ax
	push   %ax
	call   L_1ac4
	pop    %cx
	mov    %ax,%si
	/* TODO: data ref: mov    0x22ec,%ax */
	mov    D_22ec,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_1961
	xor    %ax,%ax
L_195f:
	jmp   L_1974
L_1961:
	/* TODO: data ref: mov    0x22ec,%ax */
	mov    D_22ec,%ax
	shl    %ax
	push   %ax
	call   L_1ac4
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_1976
	xor    %ax,%ax
L_1974:
	jmp   L_19db
L_1976:
	/* TODO: data ref: mov    0x272a,%bx */
	mov    D_272a,%bx
	mov    %si,(%bx)
	/* TODO: data ref: mov    0x272a,%ax */
	mov    D_272a,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_1994
	/* TODO: data ref: mov    0x272a,%bx */
	mov    D_272a,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x272a,%bx */
	mov    D_272a,%bx
	mov    %ax,(%bx)
L_1994:
	/* TODO: data ref: mov    0x22ec,%ax */
	mov    D_22ec,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	/* TODO: data ref: mov    %ax,0x272a */
	mov    %ax,D_272a
	/* TODO: data ref: mov    $0x22e8,%ax */
	mov    $D_22e8,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x272a,%bx */
	mov    D_272a,%bx
	mov    %ax,(%bx)
	jmp   L_18d5
L_19b1:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	/* TODO: data ref: mov    %ax,0x2728 */
	mov    %ax,D_2728
	cmp    %ax,%si
	jbe   L_19d0
	/* TODO: data ref: mov    0x2728,%bx */
	mov    D_2728,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x272c */
	mov    %ax,D_272c
	/* TODO: data ref: mov    0x2728,%bx */
	mov    D_2728,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_19d0:
	/* TODO: data ref: mov    0x2728,%ax */
	mov    D_2728,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_19db:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_19e0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	/* TODO: data ref: mov    %si,0x2728 */
	mov    %si,D_2728
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
	je   L_1a15
	push   %di
	call   L_19e0
	pop    %cx
L_1a15:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_1896
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_1a34
	cmp    %di,%si
	jne   L_1a38
L_1a34:
	mov    %si,%ax
	jmp   L_1aa0
L_1a38:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_1a57
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_1a57:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_1a75
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_1a57
L_1a75:
	cmp    %di,%si
	jae   L_1a9e
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_1a9e
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
	/* TODO: data ref: mov    0x272c,%ax */
	mov    D_272c,%ax
	mov    %ax,(%bx)
L_1a9e:
	mov    %si,%ax
L_1aa0:
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
	jns    L_1abb
	neg	%ax
	mov	%ax,D_2320	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1ac2
L_1abb:
	mov    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x22ee */
	mov    %bx,D_22ee
L_1ac2:
	pop    %bp
	ret


L_1ac4:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	/* TODO: data ref: add    0x22ee,%ax */
	add    D_22ee,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1adb
	neg	%ax
	mov	%ax,D_2320	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1ae8
L_1adb:
	/* TODO: data ref: mov    0x22ee,%bx */
	mov    D_22ee,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x22ee */
	mov    %bx,D_22ee
L_1ae8:
	pop    %bp
	ret


L_1aea:
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
	jns    L_1afb
	neg	%ax
	mov	%ax,D_2320	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1afb:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1afe:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1b6f
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_1b1b
	xor    %ax,%ax
	jmp   L_1b1e
L_1b1b:
	mov    $0x1,%ax
L_1b1e:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1b26:
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
	je	open_wronly_5_3190
	and	$0xfffc,%cx
	or	$0x241,%cx
	mov	$0x1b6,%dx
open_wronly_5_3190:
	mov	$__NR_open,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1b3a
	neg	%ax
	mov	%ax,D_2320	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1b3a:
	pop    %bp
	ret


L_1b3c:
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
	jns    L_1b53
	neg	%ax
	mov	%ax,D_2320	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1b53:
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
	call   L_1b88
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b6f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1b88
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b88:
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
	jns    L_1b9f
	neg	%ax
	mov	%ax,D_2320	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1b9f:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1ba2:
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
	jns    L_1bb9
	neg	%ax
	mov	%ax,D_2320	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1bb9:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1bbc:
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


L_1bd8:
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
	jns    L_1be9
	neg	%ax
	mov	%ax,D_2320	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1be9:
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
	mov	%ax,D_2320	/* errno */
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
D_1bee:
	.byte	0xbf, 0x0f	/* |..| */
D_1bf0:
	.byte	0xf4, 0x1d	/* |..| */
D_1bf2:
	.byte	0x04, 0x00	/* |..| */
D_1bf4:
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
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_1df2:
	.byte	0x00, 0x00	/* |..| */
D_1df4:
	.ascii	"Standard input\0"
D_1e03:
	.byte	0x55, 0x73, 0x61, 0x67, 0x65, 0x3a, 0x20, 0x73	/* |Usage: s| */
	.byte	0x74, 0x72, 0x69, 0x6e, 0x67, 0x73, 0x20, 0x5b	/* |trings [| */
	.byte	0x20, 0x2d, 0x61, 0x20, 0x5d, 0x20, 0x5b, 0x20	/* | -a ] [ | */
	.byte	0x2d, 0x6f, 0x20, 0x5d, 0x20, 0x5b, 0x20, 0x2d	/* |-o ] [ -| */
	.byte	0x23, 0x20, 0x5d, 0x20, 0x5b, 0x20, 0x66, 0x69	/* |# ] [ fi| */
	.byte	0x6c, 0x65, 0x20, 0x2e, 0x2e, 0x2e, 0x20, 0x5d	/* |le ... ]| */
	.byte	0x0a, 0x00	/* |..| */
D_1e35:
	.byte	0x72, 0x00	/* |r.| */
D_1e37:
	.ascii	"0x%X  %7D \0"
D_1e42:
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_1e46:
	.ascii	"Unknown error\0"
D_1e54:
	.byte	0x3a, 0x20, 0x00	/* |: .| */
D_1e57:
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_1e5a:
	.byte	0xa2, 0x1e, 0xaa, 0x1e, 0xb4, 0x1e, 0xce, 0x1e	/* |........| */
	.byte	0xde, 0x1e	/* |..| */
	.word	D_1ef6	/* ptr: 0x1ef6 */
	.byte	0x00, 0x1f, 0x1a, 0x1f, 0x2c, 0x1f, 0x3e, 0x1f	/* |....,.>.| */
	.byte	0x4e, 0x1f, 0x5a, 0x1f, 0x6c, 0x1f	/* |N.Z.l.| */
	.word	D_1f7c	/* ptr: 0x1f7c */
	.byte	0x8e, 0x1f, 0x9a, 0x1f, 0xb0, 0x1f, 0xc2, 0x1f	/* |........| */
	.byte	0xce, 0x1f, 0xe0, 0x1f, 0xef, 0x1f, 0xff, 0x1f	/* |........| */
	.byte	0x0e, 0x20, 0x1f, 0x20, 0x33, 0x20, 0x47, 0x20	/* |. . 3 G | */
	.byte	0x58, 0x20, 0x67, 0x20, 0x76, 0x20, 0x8e, 0x20	/* |X g v . | */
	.byte	0x9b, 0x20, 0xb1, 0x20, 0xc0, 0x20	/* |. . . | */
	.word	D_20cc	/* ptr: 0x20cc */
	.byte	0xdf, 0x20	/* |. | */
D_1ea0:
	.byte	0x23, 0x00	/* |#.| */
D_1ea2:
	.ascii	"Error 0\0"
D_1eaa:
	.ascii	"Not owner\0"
D_1eb4:
	.ascii	"No such file or directory\0"
D_1ece:
	.ascii	"No such process\0"
D_1ede:
	.ascii	"Interrupted system call\0"
D_1ef6:
	.ascii	"I/O error\0"
D_1f00:
	.ascii	"No such device or address\0"
D_1f1a:
	.ascii	"Arg list too long\0"
D_1f2c:
	.ascii	"Exec format error\0"
D_1f3e:
	.ascii	"Bad file number\0"
D_1f4e:
	.ascii	"No children\0"
D_1f5a:
	.ascii	"No more processes\0"
D_1f6c:
	.ascii	"Not enough core\0"
D_1f7c:
	.ascii	"Permission denied\0"
D_1f8e:
	.ascii	"Bad address\0"
D_1f9a:
	.ascii	"Block device required\0"
D_1fb0:
	.ascii	"Mount device busy\0"
D_1fc2:
	.ascii	"File exists\0"
D_1fce:
	.ascii	"Cross-device link\0"
D_1fe0:
	.ascii	"No such device\0"
D_1fef:
	.ascii	"Not a directory\0"
D_1fff:
	.byte	0x49	/* |I| */
D_2000:
	.byte	0x73, 0x20, 0x61, 0x20, 0x64, 0x69, 0x72, 0x65	/* |s a dire| */
D_2008:
	.ascii	"ctory\0"
D_200e:
	.ascii	"Invalid argument\0"
D_201f:
	.byte	0x46	/* |F| */
D_2020:
	.ascii	"ile table overflow\0"
D_2033:
	.byte	0x54, 0x6f, 0x6f, 0x20, 0x6d, 0x61, 0x6e	/* |Too man| */
D_203a:
	.ascii	"y open files\0"
D_2047:
	.ascii	"Not a typewriter\0"
D_2058:
	.ascii	"Text file busy\0"
D_2067:
	.ascii	"File too large\0"
D_2076:
	.ascii	"No space left on device\0"
D_208e:
	.ascii	"Illegal seek\0"
D_209b:
	.ascii	"Read-only file system\0"
D_20b1:
	.ascii	"Too many links\0"
D_20c0:
	.ascii	"Broken pipe\0"
D_20cc:
	.ascii	"Argument too large\0"
D_20df:
	.ascii	"Result too large\0"
	.byte	0x00	/* |.| */
D_20f1:
	.byte	0x20	/* | | */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.byte	0x08, 0x08, 0x08, 0x08, 0x08, 0x20	/* |..... | */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.byte	0x20, 0x08, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* | .......| */
	.byte	0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04	/* |........| */
	.byte	0x04, 0x04, 0x04, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x10, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41	/* |..AAAAAA| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x10, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42	/* |..BBBBBB| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x20, 0x00	/* | .| */
D_2172:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_2186:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_218e:
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
D_21dd:
	.byte	0x00	/* |.| */
D_21de:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_21e6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_21ee:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_21f6:
	.byte	0x2c, 0x43, 0x1c, 0xeb, 0xe2, 0x36, 0x1a, 0x3f	/* |,C...6.?| */
D_21fe:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_2206:
	.byte	0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0xb9, 0x3f	/* |.......?| */
D_220e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_2216:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_221e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x8f, 0x40	/* |.....@.@| */
D_2226:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_222e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_2236:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_223e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_2246:
	.byte	0x28, 0x25	/* |(%| */
D_2248:
	.byte	0x00, 0x00, 0x28, 0x25	/* |..(%| */
D_224c:
	.byte	0x01, 0x00	/* |..| */
D_224e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_2254:
	.byte	0x02	/* |.| */
D_2255:
	.byte	0x01	/* |.| */
D_2256:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
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
D_22e6:
	.byte	0xe6, 0x22	/* |."| */
D_22e8:
	.byte	0x00, 0x00	/* |..| */
D_22ea:
	.byte	0x00, 0x00	/* |..| */
D_22ec:
	.byte	0x00, 0x00	/* |..| */
D_22ee:
	.byte	0x2e, 0x27	/* |.'| */

	.bss

	.space	2
D_22f2:	.space	2	/* bss word @ 0x22f2 */
D_22f4:	.space	2	/* bss word @ 0x22f4 */
	.space	10
D_2300:	.space	2	/* bss word @ 0x2300 */
	.space	2
D_2304:	.space	2	/* bss word @ 0x2304 */
D_2306:	.space	2	/* bss word @ 0x2306 */
D_2308:	.space	2	/* bss word @ 0x2308 */
D_230a:	.space	2	/* bss word @ 0x230a */
	.space	20
D_2320:	.space	2	/* bss word @ 0x2320 */
	.space	6
D_2328:	.space	2	/* bss word @ 0x2328 */
	.space	998
D_2710:	.space	2	/* bss word @ 0x2710 */
	.space	22
D_2728:	.space	2	/* bss word @ 0x2728 */
D_272a:	.space	2	/* bss word @ 0x272a */
D_272c:	.space	2	/* bss word @ 0x272c */
D_272e:	/* endcore: initial break */

/* end of file */
