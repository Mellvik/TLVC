/* strings_ucb.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 strings_ucb.s -o strings_ucb.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: strings_ucb  [linked executable]
//   Magic:    0x0107  OMAGIC (object/impure)
//   a_stack:  0x0000  (0 bytes)
//   a_text:   0x00001bbe  (7102 bytes)
//   a_data:   0x00000726  (1830 bytes)
//   Load:  cs:0x0000 (text)   ds:0x1bbe (data/bss)

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
	/* TODO: data ref: incw   0x22e6 */
	incw   D_22e6
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
	/* TODO: data ref: incw   0x22e8 */
	incw   D_22e8
	jmp   L_0097
L_0093:
	/* TODO: data ref: incw   0x22e6 */
	incw   D_22e6
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
	/* TODO: data ref: mov    0x20e5(%bx),%al */
	mov    D_20e5(%bx),%al
	cbtw
	test   $0x4,%ax
	jne   L_00c7
	/* TODO: data ref: mov    $0x1dfc,%ax */
	mov    $D_1dfc,%ax
	push   %ax
	/* TODO: data ref: mov    $0x224a,%ax */
	mov    $D_224a,%ax
	push   %ax
	call   L_04ba
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
	/* TODO: data ref: mov    %ax,0x1bc6 */
	mov    %ax,D_1bc6
	inc    %di
L_00da:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	add    %di,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	/* TODO: data ref: mov    0x20e5(%bx),%al */
	mov    D_20e5(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0117
	/* TODO: data ref: mov    0x1bc6,%ax */
	mov    D_1bc6,%ax
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
	/* TODO: data ref: mov    %ax,0x1bc6 */
	mov    %ax,D_1bc6
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
	/* TODO: data ref: mov    $0x223a,%ax */
	mov    $D_223a,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1e2e,%ax */
	mov    $D_1e2e,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_0366
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_0169
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_06b0
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0169:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x1bc4 */
	mov    %ax,D_1bc4
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
L_0178:
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: mov    $0x223a,%ax */
	mov    $D_223a,%ax
	push   %ax
	call   L_04fc
	add    $0x8,%sp
	/* TODO: data ref: cmpw   $0x0,0x22e6 */
	cmpw   $0x0,D_22e6
	jne   L_01cd
	/* TODO: data ref: mov    $0x223a,%ax */
	mov    $D_223a,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x20,%ax
	push   %ax
	/* TODO: data ref: mov    $0x22f4,%ax */
	mov    $D_22f4,%ax
	push   %ax
	call   L_074a
	add    $0x8,%sp
	cmp    $0x1,%ax
	jne   L_01cd
	/* TODO: data ref: cmpw   $0x107,0x22f4 */
	cmpw   $0x107,D_22f4
	je   L_01c2
	/* TODO: data ref: cmpw   $0x109,0x22f4 */
	cmpw   $0x109,D_22f4
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
	/* TODO: data ref: mov    $0x223a,%ax */
	mov    $D_223a,%ax
	push   %ax
	call   L_04fc
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
	/* TODO: data ref: mov    0x22f8,%ax */
	mov    D_22f8,%ax
	/* TODO: data ref: mov    0x22fa,%dx */
	mov    D_22fa,%dx
	add    $0x20,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: mov    $0x223a,%ax */
	mov    $D_223a,%ax
	push   %ax
	call   L_04fc
	add    $0x8,%sp
	/* TODO: data ref: pushw  0x22fe */
	pushw  D_22fe
	/* TODO: data ref: pushw  0x22fc */
	pushw  D_22fc
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
	sub    $0xa,%sp
	/* TODO: data ref: mov    $0x1bc8,%di */
	mov    $D_1bc8,%di
	movw   $0x0,-0x6(%bp)
L_0238:
	cmpw   $0x0,0x6(%bp)
	jne   L_0247
	cmpw   $0x0,0x4(%bp)
	jne   L_0247
	jmp   L_0320
L_0247:
	/* TODO: data ref: decw   0x223c */
	decw   D_223c
	jl   L_025d
	/* TODO: data ref: mov    0x223a,%bx */
	mov    D_223a,%bx
	/* TODO: data ref: incw   0x223a */
	incw   D_223a
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0265
L_025d:
	/* TODO: data ref: mov    $0x223a,%ax */
	mov    $D_223a,%ax
	push   %ax
	call   L_0822
	pop    %cx
L_0265:
	mov    %ax,%si
	cmp    $0xa,%si
	je   L_0281
	push   %si
	call   L_0327
	pop    %cx
	or     %ax,%ax
	jne   L_0281
	cmpw   $0x0,0x6(%bp)
	jne   L_027f
	cmpw   $0x0,0x4(%bp)
L_027f:
	jne   L_02f3
L_0281:
	/* TODO: data ref: cmp    $0x1bc8,%di */
	cmp    $D_1bc8,%di
	jbe   L_028e
	cmpb   $0xa,-0x1(%di)
	jne   L_028e
	dec    %di
L_028e:
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	/* TODO: data ref: mov    0x1bc6,%ax */
	mov    D_1bc6,%ax
	/* TODO: data ref: add    $0x1bc8,%ax */
	add    $D_1bc8,%ax
	cmp    %ax,%di
	jbe   L_02e9
	/* TODO: data ref: cmpw   $0x0,0x22e8 */
	cmpw   $0x0,D_22e8
	je   L_02db
	/* TODO: data ref: mov    $0x223a,%ax */
	mov    $D_223a,%ax
	push   %ax
	call   L_0406
	pop    %cx
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
	mov    -0x6(%bp),%ax
	cwtd
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    -0xa(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    -0xe(%bp),%ax
	sbb    -0xc(%bp),%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: mov    $0x1e30,%ax */
	mov    $D_1e30,%ax
	push   %ax
	call   L_0720
	add    $0x6,%sp
L_02db:
	/* TODO: data ref: mov    $0x1bc8,%ax */
	mov    $D_1bc8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1e35,%ax */
	mov    $D_1e35,%ax
	push   %ax
	call   L_0720
	add    $0x4,%sp
L_02e9:
	/* TODO: data ref: mov    $0x1bc8,%di */
	mov    $D_1bc8,%di
	movw   $0x0,-0x6(%bp)
	jmp   L_0303
L_02f3:
	/* TODO: data ref: cmp    $0x1dc6,%di */
	cmp    $D_1dc6,%di
	jae   L_0300
	mov    %di,%bx
	inc    %di
	mov    %si,%cx
	mov    %cl,(%bx)
L_0300:
	incw   -0x6(%bp)
L_0303:
	/* TODO: data ref: mov    0x2240,%al */
	mov    D_2240,%al
	cbtw
	test   $0x20,%ax
	jne   L_0320
	/* TODO: data ref: mov    0x2240,%al */
	mov    D_2240,%al
	cbtw
	test   $0x10,%ax
	jne   L_0320
	subw   $0x1,0x4(%bp)
	sbbw   $0x0,0x6(%bp)
	jmp   L_0238
L_0320:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0327:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	jmp   L_0350
L_0331:
	xor    %ax,%ax
	jmp   L_0361
L_0335:
	mov    $0x1,%ax
	jmp   L_0361
L_033a:
	cmpw   $0x80,0x4(%bp)
	jg   L_0347
	cmpw   $0x20,0x4(%bp)
	jge   L_034c
L_0347:
	mov    $0x1,%ax
	jmp   L_0361
L_034c:
	xor    %ax,%ax
	jmp   L_0361
L_0350:
	cmp    $0xa,%ax
	je   L_0331
	cmp    $0xc,%ax
	je   L_0331
	cmp    $0x7f,%ax
	je   L_0335
	jmp   L_033a
L_0361:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0366:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	push   %si
	call   L_17f1
	pop    %cx
	cmpb   $0x77,(%di)
	jne   L_0390
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_04e6
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	jmp   L_03cc
L_0390:
	cmpb   $0x61,(%di)
	jne   L_03ce
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1af8
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jge   L_03b9
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_04e6
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_03b9:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	call   L_08ec
	add    $0x8,%sp
L_03cc:
	jmp   L_03dd
L_03ce:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1af8
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_03dd:
	cmpw   $0x0,-0x6(%bp)
	jge   L_03e7
	xor    %ax,%ax
	jmp   L_0400
L_03e7:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,0x7(%si)
	cmpb   $0x72,(%di)
	je   L_03fa
	orb    $0x2,0x6(%si)
	jmp   L_03fe
L_03fa:
	orb    $0x1,0x6(%si)
L_03fe:
	mov    %si,%ax
L_0400:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0406:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x2(%bx)
	jge   L_041f
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
L_041f:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_0437
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%ax
	neg    %ax
	mov    %ax,%di
	jmp   L_0470
L_0437:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
	je   L_0469
	xor    %di,%di
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	je   L_0470
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	jne   L_0470
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	mov    0x4(%bp),%bx
	sub    0x4(%bx),%ax
	mov    %ax,%di
	jmp   L_0470
L_0469:
	mov    $0xffff,%ax
	mov    %ax,%dx
	jmp   L_04b2
L_0470:
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
	call   L_08ec
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmp    $0x0,%dx
	jg   L_04a3
	jl   L_049b
	cmpw   $0x0,-0x8(%bp)
	jae   L_04a3
L_049b:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	jmp   L_04b2
L_04a3:
	mov    %di,%ax
	cwtd
	add    %ax,-0x8(%bp)
	adc    %dx,-0x6(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
L_04b2:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_04ba:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_090a
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_04e0
	mov    $0xffff,%ax
	jmp   L_04e2
L_04e0:
	xor    %ax,%ax
L_04e2:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_04e6:
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
	jns    L_04fa
	neg	%ax
	mov	%ax,D_2314	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_04fa:
	pop    %bp
	ret


L_04fc:
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
	jne   L_051a
	jmp   L_0663
L_051a:
	cmpw   $0x2,0xa(%bp)
	jl   L_0523
	jmp   L_0604
L_0523:
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_052f
	jmp   L_0604
L_052f:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_053e
	jmp   L_0604
L_053e:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%si
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,0xa(%bp)
	jne   L_0597
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
	call   L_08ec
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
	jmp   L_05b2
L_0597:
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
L_05b2:
	or     %si,%si
	jle   L_05f3
	mov    %si,%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jg   L_05f3
	jl   L_05c5
	cmp    %ax,-0x8(%bp)
	ja   L_05f3
L_05c5:
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x4(%bp),%bx
	sub    (%bx),%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jl   L_05f3
	jg   L_05dd
	cmp    %ax,-0x8(%bp)
	jb   L_05f3
L_05dd:
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	add    %ax,(%bx)
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	sub    %ax,0x2(%bx)
	xor    %ax,%ax
	jmp   L_06a4
L_05f3:
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x1,%ax
	and    $0x0,%dx
	mov    %ax,%di
	jmp   L_0606
L_0604:
	xor    %di,%di
L_0606:
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
	call   L_08ec
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
	or     %di,%di
	je   L_066d
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_065a
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0693
L_065a:
	pushw  0x4(%bp)
	call   L_0822
	pop    %cx
	jmp   L_0693
L_0663:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
L_066d:
	je   L_0693
	pushw  0x4(%bp)
	call   L_1782
	pop    %cx
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_08ec
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_0693:
	cmpw   $0xffff,-0x6(%bp)
	jne   L_06a6
	cmpw   $0xffff,-0x8(%bp)
	jne   L_06a6
	mov    $0xffff,%ax
L_06a4:
	jmp   L_06a8
L_06a6:
	xor    %ax,%ax
L_06a8:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_06b0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x1e3a,%di */
	mov    $D_1e3a,%di
	/* TODO: data ref: mov    0x2314,%ax */
	mov    D_2314,%ax
	/* TODO: data ref: cmp    0x1e94,%ax */
	cmp    D_1e94,%ax
	jge   L_06cb
	/* TODO: data ref: mov    0x2314,%bx */
	mov    D_2314,%bx
	shl    %bx
	/* TODO: data ref: mov    0x1e4e(%bx),%di */
	mov    D_1e4e(%bx),%di
L_06cb:
	pushw  0x4(%bp)
	call   L_08d0
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_06f8
	push   %si
	pushw  0x4(%bp)
	mov    $0x2,%ax
	push   %ax
	call   L_1b74
	add    $0x6,%sp
	mov    $0x2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1e48,%ax */
	mov    $D_1e48,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_1b74
	add    $0x6,%sp
L_06f8:
	push   %di
	call   L_08d0
	pop    %cx
	push   %ax
	push   %di
	mov    $0x2,%ax
	push   %ax
	call   L_1b74
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1e4b,%ax */
	mov    $D_1e4b,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_1b74
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0720:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x2242,%ax */
	mov    $D_2242,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_090a
	add    $0x6,%sp
	/* TODO: data ref: mov    0x2248,%al */
	mov    D_2248,%al
	cbtw
	test   $0x20,%ax
	je   L_0744
	mov    $0xffff,%ax
	jmp   L_0746
L_0744:
	xor    %ax,%ax
L_0746:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_074a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_07ab
L_0765:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_07ab
	mov    0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0773:
	decw   0x2(%si)
	jl   L_0784
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0789
L_0784:
	push   %si
	call   L_0822
	pop    %cx
L_0789:
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jl   L_079c
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_07a1
L_079c:
	mov    -0x8(%bp),%ax
	jmp   L_07ae
L_07a1:
	decw   -0xa(%bp)
	jne   L_0773
	incw   -0x8(%bp)
	jmp   L_0765
L_07ab:
	mov    -0x8(%bp),%ax
L_07ae:
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
	je   L_0817
L_07d0:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_0817
	mov    0x6(%bp),%ax
	mov    %ax,-0x6(%bp)
L_07de:
	decw   0x2(%si)
	jl   L_07f6
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
	jmp   L_0804
L_07f6:
	push   %si
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1694
	add    $0x4,%sp
L_0804:
	decw   -0x6(%bp)
	jne   L_07de
	mov    0x6(%si),%al
	cbtw
	test   $0x20,%ax
	jne   L_0817
	incw   -0x8(%bp)
	jmp   L_07d0
L_0817:
	mov    -0x8(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0822:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_0839
	mov    $0xffff,%ax
	jmp   L_0845
L_0839:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_0847
	mov    $0xffff,%ax
L_0845:
	jmp   L_08bf
L_0847:
	cmpw   $0x0,0x4(%di)
	jne   L_087b
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0862
	mov    0x7(%di),%al
	cbtw
	/* TODO: data ref: add    $0x2166,%ax */
	add    $D_2166,%ax
	mov    %ax,0x4(%di)
	jmp   L_0847
L_0862:
	mov    $0x200,%ax
	push   %ax
	call   L_1868
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_0877
	orb    $0x4,0x6(%di)
	jmp   L_0847
L_0877:
	orb    $0x8,0x6(%di)
L_087b:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_088e
	mov    $0x1,%ax
	jmp   L_0891
L_088e:
	mov    $0x200,%ax
L_0891:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1b0e
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_08c1
	cmpw   $0xffff,0x2(%di)
	jne   L_08b3
	orb    $0x10,0x6(%di)
	jmp   L_08b7
L_08b3:
	orb    $0x20,0x6(%di)
L_08b7:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_08bf:
	jmp   L_08cb
L_08c1:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_08cb:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_08d0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_08db:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_08e6
	inc    %si
	jmp   L_08db
L_08e6:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_08ec:
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
L_0907:
	pop    %si
	pop    %bp
	ret


L_090a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0922:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_0934
	jmp   L_0ed1
L_0934:
	cmpb   $0x25,-0x5(%bp)
	je   L_0947
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_0945:
	jmp   L_0922
L_0947:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0972
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_15a2
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0972:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_098b
	incb   -0xa6(%bp)
	inc    %di
L_098b:
	cmpb   $0x30,(%di)
	jne   L_0996
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_0996:
	cmpb   $0x2a,(%di)
	je   L_09c6
	movw   $0x0,-0xe(%bp)
L_09a0:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_09dc
	cmpw   $0x9,-0xc(%bp)
	jg   L_09dc
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_09a0
L_09c6:
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
L_09dc:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_0a23
	cmpb   $0x2a,(%di)
	je   L_0a25
	movw   $0x0,-0x10(%bp)
L_09f5:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0a1f
	cmpw   $0x9,-0xc(%bp)
	jg   L_0a1f
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_09f5
L_0a1f:
	addw   $0x30,-0xc(%bp)
L_0a23:
	jmp   L_0a3c
L_0a25:
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
L_0a3c:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_0e46
L_0a47:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_0a58
L_0a4f:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_0a88
	jmp   L_0a81
L_0a58:
	cmp    $0x64,%ax
	je   L_0a8a
	cmp    $0x6f,%ax
	je   L_0a6e
	cmp    $0x75,%ax
	je   L_0a81
	cmp    $0x78,%ax
	je   L_0a7c
	jmp   L_0a4f
L_0a6e:
	movw   $0x8,-0x12(%bp)
	jmp   L_0a81
L_0a75:
	movw   $0x8,-0x12(%bp)
	jmp   L_0a88
L_0a7c:
	movw   $0x10,-0x12(%bp)
L_0a81:
	jmp   L_0ad3
L_0a83:
	movw   $0x10,-0x12(%bp)
L_0a88:
	jmp   L_0ae3
L_0a8a:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0aa7
L_0a9a:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0aa7:
	cmpw   $0x0,-0x14(%bp)
	jg   L_0af1
	jl   L_0ab5
	cmpw   $0x0,-0x16(%bp)
	jae   L_0af1
L_0ab5:
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
	jmp   L_0af1
L_0ad3:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0af1
L_0ae3:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0af1:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0afb
	cmpw   $0x0,-0x16(%bp)
L_0afb:
	jne   L_0b0c
	cmpw   $0x0,-0x10(%bp)
	jne   L_0b0c
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_0b0c:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0b13:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0b1d
	cmpw   $0x0,-0x16(%bp)
L_0b1d:
	je   L_0b68
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_1030
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_0b3e
	mov    $0x30,%ax
	jmp   L_0b41
L_0b3e:
	mov    $0x57,%ax
L_0b41:
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
	call   L_0f28
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_0b13
L_0b68:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_0bb7
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_0b68
L_0b86:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_0b92:
	cmpw   $0x2,-0xc(%bp)
	jge   L_0bb7
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0bb2
	decw   -0x8(%bp)
L_0bb2:
	incw   -0xc(%bp)
	jmp   L_0b92
L_0bb7:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0bbe:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_0bd5
	movw   $0x0,-0xc(%bp)
L_0bd5:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_0be4
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_0be4:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_15a2
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0c07:
	jmp   L_0945
L_0c0a:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_0c1c
	/* TODO: data ref: movw   $0x217a,-0xa(%bp) */
	movw   $D_217a,-0xa(%bp)
L_0c1c:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_0c2b
	movw   $0x7fff,-0xc(%bp)
L_0c2b:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0c31:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0bbe
	decw   -0xc(%bp)
	jl   L_0bbe
	incw   -0x8(%bp)
	jmp   L_0c31
L_0c43:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_0c4b:
	jmp   L_0c07
L_0c4d:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0c59
	movw   $0x6,-0x10(%bp)
L_0c59:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_122e
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0c8e
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0c8e:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_0ca3
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_0cbb
L_0ca3:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_0ca3
L_0cbb:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_0cce
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_0cce:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_0ced
L_0cda:
	decw   -0xc(%bp)
	jl   L_0ced
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_0cda
L_0ced:
	cmpw   $0x0,-0xc(%bp)
	jg   L_0cf6
	jmp   L_0bb7
L_0cf6:
	decw   -0xc(%bp)
	jge   L_0cfe
	jmp   L_0bb7
L_0cfe:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_0cf6
L_0d13:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0d21
	movw   $0x6,-0x10(%bp)
	jmp   L_0d24
L_0d21:
	incw   -0x10(%bp)
L_0d24:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1202
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0d59
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0d59:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_0d64
	incw   -0x1a(%bp)
L_0d64:
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
L_0d86:
	decw   -0xc(%bp)
	jle   L_0da0
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_0d86
L_0da0:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_0dbd
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_0dce
L_0dbd:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_0dce:
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
	jmp   L_0bb7
L_0dff:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0e0b
	movw   $0x6,-0x10(%bp)
L_0e0b:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1086
	add    $0xc,%sp
	add    $0x8,%si
L_0e29:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_0e29
	decw   -0x8(%bp)
	jmp   L_0bb7
L_0e3a:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_0e43:
	jmp   L_0c4b
L_0e46:
	cmp    $0x65,%ax
	jne   L_0e4e
	jmp   L_0d13
L_0e4e:
	jg   L_0e8f
	cmp    $0x4f,%ax
	jne   L_0e58
	jmp   L_0a6e
L_0e58:
	jg   L_0e6f
	cmp    $0x25,%ax
	je   L_0e3a
	cmp    $0x44,%ax
	jne   L_0e67
	jmp   L_0a8a
L_0e67:
	cmp    $0x4c,%ax
L_0e6a:
	jne   L_0e6f
	jmp   L_0a47
L_0e6f:
	cmp    $0x55,%ax
	jne   L_0e77
	jmp   L_0ad3
L_0e77:
	cmp    $0x58,%ax
	jne   L_0e7f
	jmp   L_0a7c
L_0e7f:
	cmp    $0x63,%ax
	jne   L_0e87
	jmp   L_0b86
L_0e87:
	cmp    $0x64,%ax
	jne   L_0e8f
	jmp   L_0a9a
L_0e8f:
	cmp    $0x6f,%ax
	jne   L_0e97
	jmp   L_0a75
L_0e97:
	jg   L_0eae
	cmp    $0x66,%ax
	jne   L_0ea1
	jmp   L_0c4d
L_0ea1:
	cmp    $0x67,%ax
	jne   L_0ea9
	jmp   L_0dff
L_0ea9:
	cmp    $0x6c,%ax
	je   L_0e6a
L_0eae:
	cmp    $0x72,%ax
	jne   L_0eb6
	jmp   L_0c43
L_0eb6:
	cmp    $0x73,%ax
	jne   L_0ebe
	jmp   L_0c0a
L_0ebe:
	cmp    $0x75,%ax
	jne   L_0ec6
	jmp   L_0ae3
L_0ec6:
	cmp    $0x78,%ax
	jne   L_0ece
	jmp   L_0a83
L_0ece:
	jmp   L_0e43
L_0ed1:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0ef5
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_15a2
	add    $0x8,%sp
L_0ef5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_1064


L_0eff:
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
	call   L_1064


L_0f28:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_0f46
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_0f46:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_0f4f:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_0f4f
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_0f5f:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_0f5f
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0eff
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_0fa0
	jne   L_0f95
	or     $0x0,%ax
	je   L_0fa0
L_0f95:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_0fa0:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_1064


L_0fa8:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0fc4
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0fc4:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0fdb
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0fdb:
	push   %dx
	push   %ax
	call   L_0f28
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_0ff5
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_0ff5:
	ret
	call   L_1064
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0fa8
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0eff
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_1064


L_1030:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0f28
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0eff
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_1064:
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


L_1086:
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
	call   L_1202
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_10c0
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_10c0:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_10c7:
	cmpw   $0x0,-0xa(%bp)
	jle   L_10e1
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_10e1
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_10c7
L_10e1:
	cmpw   $0x0,-0x8(%bp)
	jl   L_10f2
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_10fe
L_10f2:
	cmpw   $0x0,-0x8(%bp)
	jge   L_10fc
	cmpw   $0xfffd,-0x8(%bp)
L_10fc:
	jge   L_117d
L_10fe:
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
L_1119:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_1133
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_1119
L_1133:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_114f
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_1155
L_114f:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_1155:
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
	jmp   L_11ed
L_117d:
	cmpw   $0x0,-0x8(%bp)
	jg   L_119f
	cmpb   $0x30,(%di)
	je   L_118e
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_118e:
	cmpw   $0x0,-0x8(%bp)
	jge   L_119f
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_118e
L_119f:
	movw   $0x1,-0xa(%bp)
L_11a4:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_11cc
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
	jne   L_11c7
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_11c7:
	incw   -0xa(%bp)
	jmp   L_11a4
L_11cc:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_11ed
L_11d4:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_11e7
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_11d4
L_11e7:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_11ed:
	cmpb   $0x2e,-0x1(%si)
	jne   L_11f4
	dec    %si
L_11f4:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1202:
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
	call   L_1259
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_122e:
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
	call   L_1259
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1259:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_126c
	movw   $0x0,0xc(%bp)
L_126c:
	cmpw   $0x4f,0xc(%bp)
	jl   L_1277
	movw   $0x4e,0xc(%bp)
L_1277:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	/* TODO: data ref: mov    $0x2182,%si */
	mov    $D_2182,%si
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x21d2 */
	fcompl D_21d2
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jae   L_12b1
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x46(%bp)
	fstpl  0x4(%bp)
L_12b1:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1b8e
	fstpl  -0x46(%bp)
	add    $0xa,%sp
	fldl   -0x46(%bp)
	fstpl  0x4(%bp)
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x21da */
	fcompl D_21da
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jne   L_12f2
	jmp   L_13c8
L_12f2:
	/* TODO: data ref: mov    $0x2182,%si */
	mov    $D_2182,%si
L_12f5:
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x21e2 */
	fcompl D_21e2
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jb   L_1381
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	/* TODO: data ref: fmull  0x21ea */
	fmull  D_21ea
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1b8e
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	fldl   -0x4e(%bp)
	/* TODO: data ref: fmull  0x21f2 */
	fmull  D_21f2
	/* TODO: data ref: faddl  0x21fa */
	faddl  D_21fa
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
	jne   L_12f5
	fldl   -0xc(%bp)
	/* TODO: data ref: faddl  0x2202 */
	faddl  D_2202
	fstpl  -0xc(%bp)
	lea    -0x3e(%bp),%ax
	lea    -0x1(%di),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	jmp   L_12f5
L_1381:
	mov    $0x1,%ax
	push   %ax
	push   %si
	fldl   -0xc(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   L_152c
	add    $0x6,%sp
	mov    %ax,%si
L_139c:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_13be
	xor    %ax,%ax
	push   %ax
	push   %si
	lea    -0x3e(%bp),%ax
	mov    %di,%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_152c
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_139c
L_13be:
	mov    %si,%ax
	/* TODO: data ref: sub    $0x2182,%ax */
	sub    $D_2182,%ax
	mov    %ax,%di
	jmp   L_1448
L_13c8:
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x220a */
	fcompl D_220a
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jbe   L_1448
L_13de:
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x2212 */
	fmull  D_2212
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x221a */
	fcompl D_221a
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_1414
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	sub    $0x3,%di
	jmp   L_13de
L_1414:
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x2222 */
	fmull  D_2222
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x222a */
	fcompl D_222a
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_1448
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_1414
L_1448:
	mov    0xc(%bp),%ax
	/* TODO: data ref: add    $0x2182,%ax */
	add    $D_2182,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_145a
	add    %di,-0x16(%bp)
L_145a:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	/* TODO: data ref: cmpw   $0x2182,-0x16(%bp) */
	cmpw   $D_2182,-0x16(%bp)
	jae   L_1470
	/* TODO: data ref: movb   $0x0,0x2182 */
	movb   $0x0,D_2182
	/* TODO: data ref: mov    $0x2182,%ax */
	mov    $D_2182,%ax
	jmp   L_14d9
L_1470:
	cmp    -0x16(%bp),%si
	ja   L_14ca
	/* TODO: data ref: cmp    $0x21d2,%si */
	cmp    $D_21d2,%si
	jae   L_14ca
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x2232 */
	fmull  D_2232
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1b8e
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
	call   L_152c
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_1470
L_14ca:
	/* TODO: data ref: cmpw   $0x21d2,-0x16(%bp) */
	cmpw   $D_21d2,-0x16(%bp)
	jb   L_14db
	/* TODO: data ref: movb   $0x0,0x21d1 */
	movb   $0x0,D_21d1
	/* TODO: data ref: mov    $0x2182,%ax */
	mov    $D_2182,%ax
L_14d9:
	jmp   L_1525
L_14db:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_14e3:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_151f
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	/* TODO: data ref: cmpw   $0x2182,-0x16(%bp) */
	cmpw   $D_2182,-0x16(%bp)
	jbe   L_1502
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_14e3
L_1502:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_14e3
	/* TODO: data ref: cmp    $0x2182,%si */
	cmp    $D_2182,%si
	jbe   L_151c
	movb   $0x30,(%si)
L_151c:
	inc    %si
	jmp   L_14e3
L_151f:
	movb   $0x0,(%si)
	/* TODO: data ref: mov    $0x2182,%ax */
	mov    $D_2182,%ax
L_1525:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_152c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%si
	xor    %di,%di
L_1537:
	or     %si,%si
	je   L_1548
	mov    %si,%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	mov    %ax,%si
	inc    %di
	jmp   L_1537
L_1548:
	cmpw   $0x0,0x8(%bp)
	jne   L_1561
	mov    %di,%si
L_1550:
	cmp    $0x4,%si
	jge   L_1561
	mov    0x6(%bp),%bx
	movb   $0x30,(%bx)
	incw   0x6(%bp)
	inc    %si
	jmp   L_1550
L_1561:
	mov    0x6(%bp),%ax
	add    %di,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%si
L_1571:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_159a
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
	jmp   L_1571
L_159a:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_15a2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_15af:
	cmpw   $0x0,0x8(%bp)
	jl   L_15b8
	jmp   L_1646
L_15b8:
	cmpb   $0x2d,(%di)
	jne   L_15f5
	cmpw   $0x30,0xc(%bp)
	jne   L_15f5
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_15e4
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
	jmp   L_15f4
L_15e4:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1694
	add    $0x4,%sp
L_15f4:
	dec    %si
L_15f5:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1613
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
	jmp   L_161f
L_1613:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_1694
	add    $0x4,%sp
L_161f:
	incw   0x8(%bp)
	jmp   L_15af
L_1624:
	dec    %si
	jl   L_165a
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1648
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
L_1646:
	jmp   L_1624
L_1648:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1694
	add    $0x4,%sp
	jmp   L_1624
L_165a:
	cmpw   $0x0,0x8(%bp)
	je   L_168f
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_167e
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
	jmp   L_168a
L_167e:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_1694
	add    $0x4,%sp
L_168a:
	decw   0x8(%bp)
	jmp   L_165a
L_168f:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1694:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_16af
	mov    $0xffff,%ax
	jmp   L_1776
L_16af:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_16e2
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
	call   L_1b74
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_1767
L_16e2:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1734
	/* TODO: data ref: cmp    $0x2242,%di */
	cmp    $D_2242,%di
	jne   L_170d
	/* TODO: data ref: mov    0x2249,%al */
	mov    D_2249,%al
	cbtw
	push   %ax
	call   L_1ad0
	pop    %cx
	or     %ax,%ax
	je   L_1702
	orb    $0x4,0x6(%di)
	jmp   L_16af
L_1702:
	/* TODO: data ref: movw   $0x231c,0x4(%di) */
	movw   $D_231c,0x4(%di)
	/* TODO: data ref: movw   $0x231c,(%di) */
	movw   $D_231c,(%di)
	jmp   L_16af
L_170d:
	mov    $0x200,%ax
	push   %ax
	call   L_1868
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1724
	orb    $0x4,0x6(%di)
	jmp   L_16af
L_1724:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_1756
L_1734:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_1756
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1b74
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_1756:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_1767:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_1778
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_1776:
	jmp   L_177b
L_1778:
	mov    0x4(%bp),%ax
L_177b:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1782:
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
	jne   L_17ce
	mov    0x4(%di),%si
	or     %si,%si
	je   L_17ce
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_17ce
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1b74
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_17ce
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_17d0
L_17ce:
	xor    %ax,%ax
L_17d0:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_17d5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x223a,%di */
	mov    $D_223a,%di
L_17dd:
	/* TODO: data ref: cmp    0x22da,%di */
	cmp    D_22da,%di
	jae   L_17ed
	push   %di
	call   L_17f1
	pop    %cx
	add    $0x8,%di
	jmp   L_17dd
L_17ed:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_17f1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_1844
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_1844
	push   %di
	call   L_1782
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1abc
	pop    %cx
	or     %ax,%ax
	jge   L_1826
	mov    $0xffff,%si
L_1826:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_1836
	pushw  0x4(%di)
	call   L_19b2
	pop    %cx
L_1836:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_1844
	movw   $0x0,0x4(%di)
L_1844:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_1868:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: cmpw   $0x0,0x22dc */
	cmpw   $0x0,D_22dc
	jne   L_1893
	/* TODO: data ref: mov    $0x22de,%ax */
	mov    $D_22de,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x22dc */
	mov    %ax,D_22dc
	/* TODO: data ref: mov    $0x22dc,%ax */
	mov    $D_22dc,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x22de */
	mov    %ax,D_22de
	/* TODO: data ref: movw   $0x22de,0x271e */
	/* dual-reloc split: movw $D_22de,D_271e */
	mov	$D_22de,%ax
	mov	%ax,D_271e
	/* TODO: data ref: movw   $0x22dc,0x271c */
	/* dual-reloc split: movw $D_22dc,D_271c */
	mov	$D_22dc,%ax
	mov	%ax,D_271c
L_1893:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    0x271c,%di */
	mov    D_271c,%di
L_18a7:
	/* TODO: data ref: movw   $0x0,0x22e0 */
	movw   $0x0,D_22e0
L_18ad:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_18dc
L_18b4:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_18c3
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_18b4
L_18c3:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_18dc
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_18dc
	jmp   L_1983
L_18dc:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_18ad
	/* TODO: data ref: cmp    0x271e,%si */
	cmp    D_271e,%si
	jne   L_18f5
	/* TODO: data ref: cmp    $0x22dc,%di */
	cmp    $D_22dc,%di
	je   L_18f9
L_18f5:
	xor    %ax,%ax
	jmp   L_1931
L_18f9:
	/* TODO: data ref: incw   0x22e0 */
	incw   D_22e0
	/* TODO: data ref: cmpw   $0x1,0x22e0 */
	cmpw   $0x1,D_22e0
	jle   L_18ad
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	/* TODO: data ref: mov    %ax,0x22e0 */
	mov    %ax,D_22e0
	xor    %ax,%ax
	push   %ax
	call   L_1a96
	pop    %cx
	mov    %ax,%si
	/* TODO: data ref: mov    0x22e0,%ax */
	mov    D_22e0,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_1933
	xor    %ax,%ax
L_1931:
	jmp   L_1946
L_1933:
	/* TODO: data ref: mov    0x22e0,%ax */
	mov    D_22e0,%ax
	shl    %ax
	push   %ax
	call   L_1a96
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_1948
	xor    %ax,%ax
L_1946:
	jmp   L_19ad
L_1948:
	/* TODO: data ref: mov    0x271e,%bx */
	mov    D_271e,%bx
	mov    %si,(%bx)
	/* TODO: data ref: mov    0x271e,%ax */
	mov    D_271e,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_1966
	/* TODO: data ref: mov    0x271e,%bx */
	mov    D_271e,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x271e,%bx */
	mov    D_271e,%bx
	mov    %ax,(%bx)
L_1966:
	/* TODO: data ref: mov    0x22e0,%ax */
	mov    D_22e0,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	/* TODO: data ref: mov    %ax,0x271e */
	mov    %ax,D_271e
	/* TODO: data ref: mov    $0x22dc,%ax */
	mov    $D_22dc,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x271e,%bx */
	mov    D_271e,%bx
	mov    %ax,(%bx)
	jmp   L_18a7
L_1983:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	/* TODO: data ref: mov    %ax,0x271c */
	mov    %ax,D_271c
	cmp    %ax,%si
	jbe   L_19a2
	/* TODO: data ref: mov    0x271c,%bx */
	mov    D_271c,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x2720 */
	mov    %ax,D_2720
	/* TODO: data ref: mov    0x271c,%bx */
	mov    D_271c,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_19a2:
	/* TODO: data ref: mov    0x271c,%ax */
	mov    D_271c,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_19ad:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_19b2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	/* TODO: data ref: mov    %si,0x271c */
	mov    %si,D_271c
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
	je   L_19e7
	push   %di
	call   L_19b2
	pop    %cx
L_19e7:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_1868
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_1a06
	cmp    %di,%si
	jne   L_1a0a
L_1a06:
	mov    %si,%ax
	jmp   L_1a72
L_1a0a:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_1a29
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_1a29:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_1a47
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_1a29
L_1a47:
	cmp    %di,%si
	jae   L_1a70
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_1a70
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
	/* TODO: data ref: mov    0x2720,%ax */
	mov    D_2720,%ax
	mov    %ax,(%bx)
L_1a70:
	mov    %si,%ax
L_1a72:
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
	jns    L_1a8d
	neg	%ax
	mov	%ax,D_2314	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1a94
L_1a8d:
	mov    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x22e2 */
	mov    %bx,D_22e2
L_1a94:
	pop    %bp
	ret


L_1a96:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	/* TODO: data ref: add    0x22e2,%ax */
	add    D_22e2,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1aad
	neg	%ax
	mov	%ax,D_2314	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1aba
L_1aad:
	/* TODO: data ref: mov    0x22e2,%bx */
	mov    D_22e2,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x22e2 */
	mov    %bx,D_22e2
L_1aba:
	pop    %bp
	ret


L_1abc:
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
	jns    L_1acd
	neg	%ax
	mov	%ax,D_2314	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1acd:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1ad0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1b41
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_1aed
	xor    %ax,%ax
	jmp   L_1af0
L_1aed:
	mov    $0x1,%ax
L_1af0:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1af8:
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
	jns    L_1b0c
	neg	%ax
	mov	%ax,D_2314	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1b0c:
	pop    %bp
	ret


L_1b0e:
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
	jns    L_1b25
	neg	%ax
	mov	%ax,D_2314	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1b25:
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
	call   L_1b5a
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b41:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1b5a
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b5a:
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
	jns    L_1b71
	neg	%ax
	mov	%ax,D_2314	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1b71:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1b74:
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
	jns    L_1b8b
	neg	%ax
	mov	%ax,D_2314	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1b8b:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1b8e:
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


L_1baa:
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
	jns    L_1bbb
	neg	%ax
	mov	%ax,D_2314	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1bbb:
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
	mov	%ax,D_2314	/* errno */
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
D_1bc0:
	.byte	0xbf, 0x0f, 0xc8, 0x1d	/* |....| */
D_1bc4:
	.byte	0xed, 0x1d	/* |..| */
D_1bc6:
	.byte	0x04, 0x00	/* |..| */
D_1bc8:
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
D_1dc6:
	.byte	0x00, 0x00	/* |..| */
D_1dc8:
	.byte	0x40, 0x28, 0x23, 0x29, 0x73, 0x74, 0x72, 0x69	/* |@(#)stri| */
	.byte	0x6e, 0x67, 0x73, 0x2e, 0x63, 0x09	/* |ngs.c.| */
	.ascii	"4.1 (Berkeley) 10/1/80\0"
D_1ded:
	.ascii	"Standard input\0"
D_1dfc:
	.byte	0x55, 0x73, 0x61, 0x67, 0x65, 0x3a, 0x20, 0x73	/* |Usage: s| */
	.byte	0x74, 0x72, 0x69, 0x6e, 0x67, 0x73, 0x20, 0x5b	/* |trings [| */
	.byte	0x20, 0x2d, 0x61, 0x20, 0x5d, 0x20, 0x5b, 0x20	/* | -a ] [ | */
	.byte	0x2d, 0x6f, 0x20, 0x5d, 0x20, 0x5b, 0x20, 0x2d	/* |-o ] [ -| */
	.byte	0x23, 0x20, 0x5d, 0x20, 0x5b, 0x20, 0x66, 0x69	/* |# ] [ fi| */
	.byte	0x6c, 0x65, 0x20, 0x2e, 0x2e, 0x2e, 0x20, 0x5d	/* |le ... ]| */
	.byte	0x0a, 0x00	/* |..| */
D_1e2e:
	.byte	0x72, 0x00	/* |r.| */
D_1e30:
	.ascii	"%7D \0"
D_1e35:
	.byte	0x25, 0x73, 0x0a, 0x00, 0x00	/* |%s...| */
D_1e3a:
	.ascii	"Unknown error\0"
D_1e48:
	.byte	0x3a, 0x20, 0x00	/* |: .| */
D_1e4b:
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_1e4e:
	.byte	0x96, 0x1e, 0x9e, 0x1e, 0xa8, 0x1e, 0xc2, 0x1e	/* |........| */
	.byte	0xd2, 0x1e, 0xea, 0x1e, 0xf4, 0x1e	/* |......| */
	.word	D_1f0e	/* ptr: 0x1f0e */
	.byte	0x20, 0x1f, 0x32, 0x1f, 0x42, 0x1f	/* | .2.B.| */
	.word	D_1f4e	/* ptr: 0x1f4e */
	.byte	0x60, 0x1f, 0x70, 0x1f, 0x82, 0x1f, 0x8e, 0x1f	/* |`.p.....| */
	.byte	0xa4, 0x1f, 0xb6, 0x1f, 0xc2, 0x1f, 0xd4, 0x1f	/* |........| */
	.byte	0xe3, 0x1f, 0xf3, 0x1f, 0x02, 0x20, 0x13, 0x20	/* |..... . | */
	.word	D_2027	/* ptr: 0x2027 */
	.byte	0x3b, 0x20, 0x4c, 0x20, 0x5b, 0x20, 0x6a, 0x20	/* |; L [ j | */
	.byte	0x82, 0x20, 0x8f, 0x20, 0xa5, 0x20, 0xb4, 0x20	/* |. . . . | */
	.byte	0xc0, 0x20, 0xd3, 0x20	/* |. . | */
D_1e94:
	.byte	0x23, 0x00	/* |#.| */
D_1e96:
	.ascii	"Error 0\0"
D_1e9e:
	.ascii	"Not owner\0"
D_1ea8:
	.ascii	"No such file or directory\0"
D_1ec2:
	.ascii	"No such process\0"
D_1ed2:
	.ascii	"Interrupted system call\0"
D_1eea:
	.ascii	"I/O error\0"
D_1ef4:
	.ascii	"No such device or address\0"
D_1f0e:
	.ascii	"Arg list too long\0"
D_1f20:
	.ascii	"Exec format error\0"
D_1f32:
	.ascii	"Bad file number\0"
D_1f42:
	.ascii	"No children\0"
D_1f4e:
	.ascii	"No more processes\0"
D_1f60:
	.ascii	"Not enough core\0"
D_1f70:
	.ascii	"Permission denied\0"
D_1f82:
	.ascii	"Bad address\0"
D_1f8e:
	.ascii	"Block device required\0"
D_1fa4:
	.ascii	"Mount device busy\0"
D_1fb6:
	.ascii	"File exists\0"
D_1fc2:
	.ascii	"Cross-device link\0"
D_1fd4:
	.ascii	"No such device\0"
D_1fe3:
	.ascii	"Not a directory\0"
D_1ff3:
	.byte	0x49, 0x73, 0x20, 0x61, 0x20, 0x64, 0x69, 0x72	/* |Is a dir| */
	.byte	0x65, 0x63, 0x74, 0x6f, 0x72	/* |ector| */
D_2000:
	.byte	0x79, 0x00	/* |y.| */
D_2002:
	.byte	0x49, 0x6e, 0x76, 0x61, 0x6c, 0x69	/* |Invali| */
D_2008:
	.ascii	"d argument\0"
D_2013:
	.byte	0x46, 0x69, 0x6c, 0x65, 0x20, 0x74, 0x61, 0x62	/* |File tab| */
	.byte	0x6c, 0x65, 0x20, 0x6f, 0x76	/* |le ov| */
D_2020:
	.ascii	"erflow\0"
D_2027:
	.byte	0x54, 0x6f, 0x6f, 0x20, 0x6d, 0x61, 0x6e, 0x79	/* |Too many| */
	.byte	0x20, 0x6f, 0x70, 0x65, 0x6e, 0x20, 0x66, 0x69	/* | open fi| */
	.byte	0x6c, 0x65, 0x73	/* |les| */
D_203a:
	.byte	0x00	/* |.| */
D_203b:
	.ascii	"Not a typewriter\0"
D_204c:
	.ascii	"Text file busy\0"
D_205b:
	.ascii	"File too large\0"
D_206a:
	.ascii	"No space left on device\0"
D_2082:
	.ascii	"Illegal seek\0"
D_208f:
	.ascii	"Read-only file system\0"
D_20a5:
	.ascii	"Too many links\0"
D_20b4:
	.ascii	"Broken pipe\0"
D_20c0:
	.ascii	"Argument too large\0"
D_20d3:
	.ascii	"Result too large\0"
	.byte	0x00	/* |.| */
D_20e5:
	.byte	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |        | */
	.byte	0x20, 0x08, 0x08, 0x08, 0x08, 0x08, 0x20, 0x20	/* | .....  | */
	.byte	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |        | */
	.byte	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |        | */
	.byte	0x08, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04	/* |........| */
	.byte	0x04, 0x04, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x01	/* |.AAAAAA.| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x02	/* |.BBBBBB.| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x02, 0x10, 0x10, 0x10, 0x10, 0x20	/* |....... | */
	.byte	0x00	/* |.| */
D_2166:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_217a:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_2182:
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
D_21d1:
	.byte	0x00	/* |.| */
D_21d2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_21da:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_21e2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_21ea:
	.byte	0x2c, 0x43, 0x1c, 0xeb, 0xe2, 0x36, 0x1a, 0x3f	/* |,C...6.?| */
D_21f2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_21fa:
	.byte	0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0xb9, 0x3f	/* |.......?| */
D_2202:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_220a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_2212:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x8f, 0x40	/* |.....@.@| */
D_221a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_2222:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_222a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_2232:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_223a:
	.byte	0x1c, 0x25	/* |.%| */
D_223c:
	.byte	0x00, 0x00, 0x1c, 0x25	/* |...%| */
D_2240:
	.byte	0x01, 0x00	/* |..| */
D_2242:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_2248:
	.byte	0x02	/* |.| */
D_2249:
	.byte	0x01	/* |.| */
D_224a:
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
D_22da:
	.byte	0xda, 0x22	/* |."| */
D_22dc:
	.byte	0x00, 0x00	/* |..| */
D_22de:
	.byte	0x00, 0x00	/* |..| */
D_22e0:
	.byte	0x00, 0x00	/* |..| */
D_22e2:
	.byte	0x22, 0x27	/* |"'| */

	.bss

	.space	2
D_22e6:	.space	2	/* bss word @ 0x22e6 */
D_22e8:	.space	2	/* bss word @ 0x22e8 */
	.space	10
D_22f4:	.space	2	/* bss word @ 0x22f4 */
	.space	2
D_22f8:	.space	2	/* bss word @ 0x22f8 */
D_22fa:	.space	2	/* bss word @ 0x22fa */
D_22fc:	.space	2	/* bss word @ 0x22fc */
D_22fe:	.space	2	/* bss word @ 0x22fe */
	.space	20
D_2314:	.space	2	/* bss word @ 0x2314 */
	.space	6
D_231c:	.space	2	/* bss word @ 0x231c */
	.space	1010
D_2710:	.space	2	/* bss word @ 0x2710 */
	.space	10
D_271c:	.space	2	/* bss word @ 0x271c */
D_271e:	.space	2	/* bss word @ 0x271e */
D_2720:	.space	2	/* bss word @ 0x2720 */
D_2722:	/* endcore: initial break */

/* end of file */
