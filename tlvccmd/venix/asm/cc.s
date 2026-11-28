/* cc.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 cc.s -o cc.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: cc  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00001c4c  (7244 bytes)
//   a_data:   0x00000300  (768 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

L_0036:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_0734
	je   L_004a
	pushw  D_0734
	call   L_1bd2
	pop    %cx
L_004a:
	cmpw   $0x0,D_0736
	je   L_0059
	pushw  D_0736
	call   L_1bd2
	pop    %cx
L_0059:
	cmpw   $0x0,D_0738
	je   L_0068
	pushw  D_0738
	call   L_1bd2
	pop    %cx
L_0068:
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	cmp    $0x1,%di
	jg   L_0098
	mov    $D_0032,%ax
	push   %ax
	call   L_0822
	pop    %cx
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0098:
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_085c
	add    $0x4,%sp
	cmp    $0x1,%ax
	je   L_00b9
	mov    $0x36,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_085c
	add    $0x4,%sp
L_00b9:
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_085c
	add    $0x4,%sp
	cmp    $0x1,%ax
	je   L_00da
	mov    $0x36,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_085c
	add    $0x4,%sp
L_00da:
	mov    $0x1,%ax
	push   %ax
	mov    $0xf,%ax
	push   %ax
	call   L_085c
	add    $0x4,%sp
	cmp    $0x1,%ax
	je   L_00fb
	mov    $0x36,%ax
	push   %ax
	mov    $0xf,%ax
	push   %ax
	call   L_085c
	add    $0x4,%sp
L_00fb:
	mov    %si,-0x8(%bp)
	mov    %di,-0xc(%bp)
	mov    D_0004,%bx
	movw   $D_0066,(%bx)
	addw   $0x2,D_0004
	mov    D_0006,%bx
	movw   $D_006a,(%bx)
	addw   $0x2,D_0006
	mov    D_0008,%bx
	movw   $D_006d,(%bx)
	addw   $0x2,D_0008
	mov    D_000a,%bx
	movw   $D_0070,(%bx)
	addw   $0x2,D_000a
	mov    D_000c,%bx
	movw   $D_0075,(%bx)
	addw   $0x2,D_000c
	mov    D_000c,%bx
	movw   $D_0078,(%bx)
	addw   $0x2,D_000c
	inc    %si
	inc    %si
L_0151:
	dec    %di
	jg   L_0157
	jmp   L_02ac
L_0157:
	mov    (%si),%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	incw   -0x6(%bp)
	cmpb   $0x2d,(%bx)
	je   L_0169
	jmp   L_0281
L_0169:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	jmp   L_0203
L_0172:
	incw   D_0014
	jmp   L_01ce
L_0178:
	mov    D_0004,%bx
	mov    (%si),%ax
	mov    %ax,(%bx)
	addw   $0x2,D_0004
	jmp   L_01ce
L_0187:
	incw   D_0010
	jmp   L_01ce
L_018d:
	incw   D_0012
	jmp   L_01ea
L_0193:
	incw   D_001c
	jmp   L_01ea
L_0199:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    D_000c,%bx
	mov    %ax,(%bx)
	addw   $0x2,D_000c
	dec    %di
	mov    D_000c,%bx
	mov    (%si),%ax
	mov    %ax,(%bx)
	mov    %ax,D_0022
	addw   $0x2,D_000c
	jmp   L_0200
L_01bd:
	incw   D_0018
L_01c1:
	mov    D_0006,%bx
	mov    (%si),%ax
	mov    %ax,(%bx)
	addw   $0x2,D_0006
L_01ce:
	jmp   L_0200
L_01d0:
	mov    D_0006,%bx
	mov    (%si),%ax
	mov    %ax,(%bx)
	addw   $0x2,D_0006
L_01dd:
	mov    D_000c,%bx
	mov    (%si),%ax
	mov    %ax,(%bx)
	addw   $0x2,D_000c
L_01ea:
	jmp   L_0200
L_01ec:
	pushw  (%si)
	mov    $D_007b,%ax
	push   %ax
	call   L_0822
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0200:
	jmp   L_02a7
L_0203:
	cmp    $0x69,%ax
L_0206:
	je   L_01dd
	jg   L_024a
	cmp    $0x50,%ax
	jne   L_0212
	jmp   L_0187
L_0212:
	jg   L_022b
	cmp    $0x44,%ax
L_0217:
	jne   L_021c
	jmp   L_0178
L_021c:
	cmp    $0x49,%ax
	je   L_0217
	cmp    $0x4f,%ax
	jne   L_0229
	jmp   L_0193
L_0229:
	jmp   L_01ec
L_022b:
	cmp    $0x53,%ax
	jne   L_0233
	jmp   L_0172
L_0233:
	cmp    $0x55,%ax
	je   L_0217
	cmp    $0x63,%ax
	jne   L_0240
	jmp   L_018d
L_0240:
	cmp    $0x66,%ax
L_0243:
	jne   L_0248
	jmp   L_01c1
L_0248:
	jmp   L_01ec
L_024a:
	cmp    $0x6f,%ax
	jne   L_0252
	jmp   L_0199
L_0252:
	jg   L_0265
	cmp    $0x6c,%ax
	je   L_02a7
	cmp    $0x6d,%ax
L_025c:
	je   L_0206
	cmp    $0x6e,%ax
	je   L_0206
	jmp   L_0229
L_0265:
	cmp    $0x70,%ax
	jne   L_026d
	jmp   L_01bd
L_026d:
	cmp    $0x73,%ax
	je   L_025c
	cmp    $0x77,%ax
	je   L_0243
	cmp    $0x7a,%ax
	jne   L_027f
	jmp   L_01d0
L_027f:
	jmp   L_0229
L_0281:
	mov    (%si),%ax
	mov    %ax,-0x6(%bp)
L_0286:
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	cmpb   $0x0,(%bx)
	jne   L_0286
	mov    -0x6(%bp),%bx
	cmpb   $0x63,-0x2(%bx)
	jne   L_02a7
	mov    -0x6(%bp),%bx
	cmpb   $0x2e,-0x3(%bx)
	jne   L_02a7
	incw   D_000e
L_02a7:
	inc    %si
	inc    %si
	jmp   L_0151
L_02ac:
	cmpw   $0x0,D_0018
	je   L_02b8
	mov    $D_0094,%ax
	jmp   L_02bb
L_02b8:
	mov    $D_00a1,%ax
L_02bb:
	mov    D_000c,%bx
	mov    %ax,(%bx)
	addw   $0x2,D_000c
	mov    -0x8(%bp),%si
	mov    -0xc(%bp),%di
L_02cc:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	jne   L_02d6
	jmp   L_05c8
L_02d6:
	mov    (%si),%bx
	cmpb   $0x2d,(%bx)
	jne   L_02f5
	mov    (%si),%bx
	cmpb   $0x6c,0x1(%bx)
	jne   L_0310
	mov    D_000c,%bx
	mov    (%si),%ax
	mov    %ax,(%bx)
	addw   $0x2,D_000c
	jmp   L_05c3
L_02f5:
	mov    (%si),%ax
	mov    %ax,-0x6(%bp)
L_02fa:
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	cmpb   $0x0,(%bx)
	jne   L_02fa
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	cmpb   $0x2e,-0x1(%bx)
L_0310:
	je   L_0315
	jmp   L_05c3
L_0315:
	movw   $0x0,-0xa(%bp)
	mov    -0x6(%bp),%bx
	cmpb   $0x63,(%bx)
	jne   L_039f
	mov    $0x4,%ax
	push   %ax
	pushw  (%si)
	call   L_07e6
	add    $0x4,%sp
	or     %ax,%ax
	je   L_034a
L_0332:
	pushw  (%si)
	mov    $D_00ad,%ax
	push   %ax
	mov    $D_0266,%ax
	push   %ax
	call   L_0734
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_034a:
	cmpw   $0x1,D_000e
	jle   L_0361
	pushw  (%si)
	mov    $D_00c6,%ax
	push   %ax
	mov    $D_0266,%ax
	push   %ax
	call   L_0734
	add    $0x6,%sp
L_0361:
	mov    D_0004,%bx
	movw   $D_07a8,(%bx)
	mov    D_0004,%bx
	movw   $0x0,0x2(%bx)
	pushw  (%si)
	mov    $D_07a8,%ax
	push   %ax
	call   L_1b5c
	add    $0x4,%sp
	mov    -0x6(%bp),%bx
	movb   $0x69,(%bx)
	mov    (%si),%ax
	mov    %ax,D_0734
	pushw  (%si)
	xor    %ax,%ax
	push   %ax
	mov    $D_0740,%ax
	push   %ax
	mov    $D_00ce,%ax
	push   %ax
	call   L_063a
	add    $0x8,%sp
	incw   -0xa(%bp)
L_039f:
	cmpw   $0x0,D_0010
	jne   L_03ac
	mov    -0x6(%bp),%bx
	cmpb   $0x69,(%bx)
L_03ac:
	jne   L_0417
	cmpw   $0x0,-0xa(%bp)
	jne   L_03c7
	mov    $0x4,%ax
	push   %ax
	pushw  (%si)
	call   L_07e6
	add    $0x4,%sp
	or     %ax,%ax
L_03c2:
	je   L_03c7
	jmp   L_0332
L_03c7:
	mov    D_0006,%bx
	movw   $0x0,(%bx)
	pushw  (%si)
	mov    $D_07a8,%ax
	push   %ax
	call   L_1b5c
	add    $0x4,%sp
	cmpw   $0x0,-0xa(%bp)
	je   L_03e7
	/* dual-reloc split: movw $D_07a8,D_0734 */
	mov	$D_07a8,%ax
	mov	%ax,D_0734
L_03e7:
	mov    -0x6(%bp),%bx
	movb   $0x73,(%bx)
	mov    (%si),%ax
	mov    %ax,D_0736
	pushw  (%si)
	mov    $D_07a8,%ax
	push   %ax
	mov    $D_0bb0,%ax
	push   %ax
	pushw  D_001e
	call   L_063a
	add    $0x8,%sp
	cmpw   $0x0,-0xa(%bp)
	je   L_0414
	mov    $D_07a8,%ax
	push   %ax
	call   L_1bd2
	pop    %cx
L_0414:
	incw   -0xa(%bp)
L_0417:
	cmpw   $0x0,D_001c
	jne   L_0421
	jmp   L_0517
L_0421:
	mov    -0x6(%bp),%bx
	cmpb   $0x73,(%bx)
	je   L_042c
	jmp   L_0517
L_042c:
	cmpw   $0x0,-0xa(%bp)
	jne   L_0442
	mov    $0x4,%ax
	push   %ax
	pushw  (%si)
	call   L_07e6
	add    $0x4,%sp
	or     %ax,%ax
L_0440:
	jne   L_03c2
L_0442:
	mov    $D_00d7,%ax
	push   %ax
	call   L_0776
	pop    %cx
	mov    %ax,D_0ba8
	pushw  D_0ba8
	pushw  (%si)
	call   L_0760
	add    $0x4,%sp
	or     %ax,%ax
	je   L_0479
	pushw  (%si)
	pushw  D_0ba8
	mov    $D_00e2,%ax
	push   %ax
	mov    $D_0266,%ax
	push   %ax
	call   L_0734
	add    $0x8,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0479:
	pushw  (%si)
	call   L_1bd2
	pop    %cx
	mov    D_0ba8,%ax
	mov    %ax,D_0738
	mov    D_000a,%bx
	mov    %ax,(%bx)
	addw   $0x2,D_000a
	mov    D_000a,%bx
	movw   $0x0,(%bx)
	pushw  (%si)
	xor    %ax,%ax
	push   %ax
	mov    $D_0308,%ax
	push   %ax
	mov    $D_00fb,%ax
	push   %ax
	call   L_063a
	add    $0x8,%sp
	cmp    $0x3c,%ax
	jne   L_050f
	mov    $0x4,%ax
	push   %ax
	pushw  (%si)
	call   L_07e6
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_04e3
	pushw  (%si)
	call   L_1bd2
	pop    %cx
	cmp    $0xffff,%ax
	jne   L_04e3
	pushw  (%si)
	mov    $D_0105,%ax
	push   %ax
	mov    $D_0266,%ax
	push   %ax
	call   L_0734
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_04e3:
	pushw  (%si)
	pushw  D_0ba8
	call   L_0760
	add    $0x4,%sp
	or     %ax,%ax
	je   L_050f
	pushw  (%si)
	pushw  D_0ba8
	mov    $D_0116,%ax
	push   %ax
	mov    $D_0266,%ax
	push   %ax
	call   L_0734
	add    $0x8,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_050f:
	pushw  D_0ba8
	call   L_1bd2
	pop    %cx
L_0517:
	cmpw   $0x0,D_0014
	jne   L_0524
	mov    -0x6(%bp),%bx
	cmpb   $0x73,(%bx)
L_0524:
	jne   L_05a4
	cmpw   $0x0,-0xa(%bp)
	jne   L_053f
	mov    $0x4,%ax
	push   %ax
	pushw  (%si)
	call   L_07e6
	add    $0x4,%sp
	or     %ax,%ax
	je   L_053f
	jmp   L_0440
L_053f:
	pushw  (%si)
	mov    $D_07a8,%ax
	push   %ax
	call   L_1b5c
	add    $0x4,%sp
	cmpw   $0x0,-0xa(%bp)
	je   L_0557
	/* dual-reloc split: movw $D_07a8,D_0736 */
	mov	$D_07a8,%ax
	mov	%ax,D_0736
L_0557:
	mov    -0x6(%bp),%bx
	movb   $0x6f,(%bx)
	mov    D_0008,%bx
	movw   $D_07a8,(%bx)
	mov    D_0008,%bx
	movw   $0x0,0x2(%bx)
	mov    (%si),%ax
	mov    %ax,D_0734
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $D_0720,%ax
	push   %ax
	mov    $D_012f,%ax
	push   %ax
	call   L_063a
	add    $0x8,%sp
	cmpw   $0x0,-0xa(%bp)
	je   L_0595
	mov    $D_07a8,%ax
	push   %ax
	call   L_1bd2
	pop    %cx
L_0595:
	mov    -0x6(%bp),%bx
	movb   $0x6f,(%bx)
	incw   D_001a
	mov    (%si),%ax
	mov    %ax,D_0020
L_05a4:
	mov    D_000c,%bx
	mov    (%si),%ax
	mov    %ax,(%bx)
	addw   $0x2,D_000c
	movw   $0x0,D_0738
	movw   $0x0,D_0736
	movw   $0x0,D_0734
L_05c3:
	inc    %si
	inc    %si
	jmp   L_02cc
L_05c8:
	mov    D_0010,%ax
	or     D_0014,%ax
	or     D_0012,%ax
	je   L_05dc
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
L_05dc:
	mov    D_000c,%bx
	movw   $D_0137,(%bx)
	addw   $0x2,D_000c
	mov    D_000c,%bx
	movw   $0x0,(%bx)
	addw   $0x2,D_000c
	cmpw   $0x1,D_001a
	jne   L_0603
	mov    D_0020,%ax
	mov    %ax,D_0734
L_0603:
	mov    D_0022,%ax
	mov    %ax,D_0738
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $D_0320,%ax
	push   %ax
	mov    $D_013b,%ax
	push   %ax
	call   L_063a
	add    $0x8,%sp
	cmpw   $0x1,D_001a
	jne   L_062c
	pushw  D_0020
	call   L_1bd2
	pop    %cx
L_062c:
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_063a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	call   L_084c
	or     %ax,%ax
	jne   L_06a8
	cmpw   $0x0,0x8(%bp)
	je   L_066c
	xor    %ax,%ax
	push   %ax
	call   L_1b0a
	pop    %cx
	xor    %ax,%ax
	push   %ax
	pushw  0x8(%bp)
	call   L_1b46
	add    $0x4,%sp
	or     %ax,%ax
	je   L_066c
	mov    $0x14,%ax
	push   %ax
	call   _exit
	pop    %cx
L_066c:
	cmpw   $0x0,0xa(%bp)
	je   L_0694
	mov    $0x1,%ax
	push   %ax
	call   L_1b0a
	pop    %cx
	mov    $0x1b4,%ax
	push   %ax
	pushw  0xa(%bp)
	call   L_080c
	add    $0x4,%sp
	cmp    $0x1,%ax
	je   L_0694
	mov    $0x15,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0694:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_071a
	add    $0x4,%sp
	mov    $0x16,%ax
	push   %ax
	call   _exit
	pop    %cx
L_06a8:
	lea    -0x6(%bp),%ax
	push   %ax
	call   L_1be6
	pop    %cx
	mov    -0x6(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    %ax,%di
	jmp   L_06f9
L_06bc:
	mov    %di,%ax
	jmp   L_0714
L_06c0:
	pushw  0x8(%bp)
	mov    $D_0143,%ax
	push   %ax
	mov    $D_0266,%ax
	push   %ax
	call   L_0734
	add    $0x6,%sp
	jmp   L_0711
L_06d3:
	pushw  0xa(%bp)
	mov    $D_0160,%ax
	push   %ax
	mov    $D_0266,%ax
	push   %ax
	call   L_0734
	add    $0x6,%sp
	jmp   L_0711
L_06e6:
	pushw  0x4(%bp)
	mov    $D_017f,%ax
	push   %ax
	mov    $D_0266,%ax
	push   %ax
	call   L_0734
	add    $0x6,%sp
	jmp   L_0711
L_06f9:
	or     %ax,%ax
L_06fb:
	je   L_06bc
	cmp    $0x14,%ax
	je   L_06c0
	cmp    $0x15,%ax
	je   L_06d3
	cmp    $0x16,%ax
	je   L_06e6
	cmp    $0x3c,%ax
	je   L_06fb
L_0711:
	call   L_0036
L_0714:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_071a:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall exece(59) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    D_0300,%cx
	/* exece(59) -> execve(11) [TLVC# changed] */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_execve,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_0732
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_0732:
	pop    %bp
	ret


L_0734:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_097c
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_075a
	mov    $0xffff,%ax
	jmp   L_075c
L_075a:
	xor    %ax,%ax
L_075c:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0760:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall link(9) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* link -> __NR_link */
	xchg	%ax,%bx
	mov	%dx,%cx
	mov	$__NR_link,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_0774
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_0774:
	pop    %bp
	ret


L_0776:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	call   L_07fc
	mov    %ax,%si
	mov    0x4(%bp),%di
L_0784:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_0784
	dec    %di
L_078d:
	dec    %di
	cmpb   $0x58,(%di)
	jne   L_07b0
	mov    %si,%ax
	xor    %dx,%dx
	mov    $0xa,%cx
	div    %cx
	add    $0x30,%dx
	mov    %dx,%cx
	mov    %cl,(%di)
	mov    %si,%ax
	xor    %dx,%dx
	mov    $0xa,%cx
	div    %cx
	mov    %ax,%si
	jmp   L_078d
L_07b0:
	inc    %di
	movw   $0x61,-0x6(%bp)
L_07b6:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_07e6
	add    $0x4,%sp
	cmp    $0xffff,%ax
	je   L_07de
	cmpw   $0x7a,-0x6(%bp)
	jne   L_07d2
	mov    $D_019c,%ax
	jmp   L_07e1
L_07d2:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%di)
	incw   -0x6(%bp)
	jmp   L_07b6
L_07de:
	mov    0x4(%bp),%ax
L_07e1:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_07e6:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall access(33) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* access -> __NR_access */
	xchg	%ax,%bx
	mov	%dx,%cx
	mov	$__NR_access,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_07fa
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_07fa:
	pop    %bp
	ret


L_07fc:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getpid(20) */
	/* getpid -> __NR_getpid */
	mov	$__NR_getpid,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_080a
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_080a:
	pop    %bp
	ret


L_080c:
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
	jns    L_0820
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_0820:
	pop    %bp
	ret


L_0822:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_025e,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_097c
	add    $0x6,%sp
	mov    D_0264,%al
	cbtw
	test   $0x20,%ax
	je   L_0846
	mov    $0xffff,%ax
	jmp   L_0848
L_0846:
	xor    %ax,%ax
L_0848:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_084c:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall fork(2) */
	/* fork -> __NR_fork */
	mov	$__NR_fork,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_085a
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_085a:
	pop    %bp
	ret


L_085c:
	push   %bp
	mov    %sp,%bp
	mov    $0x16,%cx
	mov    0x4(%bp),%ax
	or     %ax,%ax
	jle   L_089f
	cmp    $0x20,%ax
	jg   L_089f
	mov    %ax,%cx
	shl    %ax
	add    $D_0be0,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    0x6(%bp),%ax
	mov    %ax,(%bx)
	cmp    $0x2,%ax
	jl   L_088b
	mov    $0x6,%ax
	mul    %cx
	add    $(L_08a8-6),%ax
L_088b:
	mov    %ax,%dx
	mov    %cx,%ax
	/* Venix syscall signal(48) */
	/* using SIG_DFL: Venix trampoline incompatible with TLVC far-ptr convention */
	xchg	%ax,%bx
	xor	%cx,%cx
	xor	%dx,%dx
	mov	$__NR_signal,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	cmp    $0x2,%ax
	jl   L_089c
	pop    %ax
	jmp   L_089d
L_089c:
	pop    %bx
L_089d:
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_08a6
L_089f:
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	mov    $0xffff,%ax
L_08a6:
	pop    %bp
	ret
L_08a8:
	push   %ax
	mov    $D_0be2,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0be4,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0be6,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0be8,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0bea,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0bec,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0bee,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0bf0,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0bf2,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0bf4,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0bf6,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0bf8,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0bfa,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0bfc,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0bfe,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0c00,%ax
	jmp   L_090c
	push   %ax
	mov    $D_0c02,%ax
L_090c:
	jmp   L_0966
	push   %ax
	mov    $D_0c04,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c06,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c08,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c0a,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c0c,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c0e,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c10,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c12,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c14,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c16,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c18,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c1a,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c1c,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c1e,%ax
	jmp   L_0966
	push   %ax
	mov    $D_0c20,%ax
L_0966:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	sub    $D_0be0,%ax
	shr    %ax
	push   %ax
	mov    (%bx),%ax
	/* TODO: indirect call - verify */
	call   *%ax
	pop    %bx
	pop    %dx
	pop    %cx
	pop    %bx
	pop    %ax
	popf
	ret


L_097c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0994:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_09a6
	jmp   L_0f43
L_09a6:
	cmpb   $0x25,-0x5(%bp)
	je   L_09b9
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_09b7:
	jmp   L_0994
L_09b9:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_09e4
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_15f0
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_09e4:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_09fd
	incb   -0xa6(%bp)
	inc    %di
L_09fd:
	cmpb   $0x30,(%di)
	jne   L_0a08
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_0a08:
	cmpb   $0x2a,(%di)
	je   L_0a38
	movw   $0x0,-0xe(%bp)
L_0a12:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0a4e
	cmpw   $0x9,-0xc(%bp)
	jg   L_0a4e
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_0a12
L_0a38:
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
L_0a4e:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_0a95
	cmpb   $0x2a,(%di)
	je   L_0a97
	movw   $0x0,-0x10(%bp)
L_0a67:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0a91
	cmpw   $0x9,-0xc(%bp)
	jg   L_0a91
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_0a67
L_0a91:
	addw   $0x30,-0xc(%bp)
L_0a95:
	jmp   L_0aae
L_0a97:
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
L_0aae:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_0eb8
L_0ab9:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_0aca
L_0ac1:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_0afa
	jmp   L_0af3
L_0aca:
	cmp    $0x64,%ax
	je   L_0afc
	cmp    $0x6f,%ax
	je   L_0ae0
	cmp    $0x75,%ax
	je   L_0af3
	cmp    $0x78,%ax
	je   L_0aee
	jmp   L_0ac1
L_0ae0:
	movw   $0x8,-0x12(%bp)
	jmp   L_0af3
L_0ae7:
	movw   $0x8,-0x12(%bp)
	jmp   L_0afa
L_0aee:
	movw   $0x10,-0x12(%bp)
L_0af3:
	jmp   L_0b45
L_0af5:
	movw   $0x10,-0x12(%bp)
L_0afa:
	jmp   L_0b55
L_0afc:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0b19
L_0b0c:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0b19:
	cmpw   $0x0,-0x14(%bp)
	jg   L_0b63
	jl   L_0b27
	cmpw   $0x0,-0x16(%bp)
	jae   L_0b63
L_0b27:
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
	jmp   L_0b63
L_0b45:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0b63
L_0b55:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0b63:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0b6d
	cmpw   $0x0,-0x16(%bp)
L_0b6d:
	jne   L_0b7e
	cmpw   $0x0,-0x10(%bp)
	jne   L_0b7e
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_0b7e:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0b85:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0b8f
	cmpw   $0x0,-0x16(%bp)
L_0b8f:
	je   L_0bda
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_10a2
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_0bb0
	mov    $0x30,%ax
	jmp   L_0bb3
L_0bb0:
	mov    $0x57,%ax
L_0bb3:
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
	call   L_0f9a
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_0b85
L_0bda:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_0c29
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_0bda
L_0bf8:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_0c04:
	cmpw   $0x2,-0xc(%bp)
	jge   L_0c29
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0c24
	decw   -0x8(%bp)
L_0c24:
	incw   -0xc(%bp)
	jmp   L_0c04
L_0c29:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0c30:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_0c47
	movw   $0x0,-0xc(%bp)
L_0c47:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_0c56
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_0c56:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_15f0
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0c79:
	jmp   L_09b7
L_0c7c:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_0c8e
	movw   $D_019e,-0xa(%bp)
L_0c8e:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_0c9d
	movw   $0x7fff,-0xc(%bp)
L_0c9d:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0ca3:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0c30
	decw   -0xc(%bp)
	jl   L_0c30
	incw   -0x8(%bp)
	jmp   L_0ca3
L_0cb5:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_0cbd:
	jmp   L_0c79
L_0cbf:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0ccb
	movw   $0x6,-0x10(%bp)
L_0ccb:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_12a0
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0d00
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0d00:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_0d15
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_0d2d
L_0d15:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_0d15
L_0d2d:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_0d40
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_0d40:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_0d5f
L_0d4c:
	decw   -0xc(%bp)
	jl   L_0d5f
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_0d4c
L_0d5f:
	cmpw   $0x0,-0xc(%bp)
	jg   L_0d68
	jmp   L_0c29
L_0d68:
	decw   -0xc(%bp)
	jge   L_0d70
	jmp   L_0c29
L_0d70:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_0d68
L_0d85:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0d93
	movw   $0x6,-0x10(%bp)
	jmp   L_0d96
L_0d93:
	incw   -0x10(%bp)
L_0d96:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1274
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0dcb
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0dcb:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_0dd6
	incw   -0x1a(%bp)
L_0dd6:
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
L_0df8:
	decw   -0xc(%bp)
	jle   L_0e12
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_0df8
L_0e12:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_0e2f
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_0e40
L_0e2f:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_0e40:
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
	jmp   L_0c29
L_0e71:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0e7d
	movw   $0x6,-0x10(%bp)
L_0e7d:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_10f8
	add    $0xc,%sp
	add    $0x8,%si
L_0e9b:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_0e9b
	decw   -0x8(%bp)
	jmp   L_0c29
L_0eac:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_0eb5:
	jmp   L_0cbd
L_0eb8:
	cmp    $0x65,%ax
	jne   L_0ec0
	jmp   L_0d85
L_0ec0:
	jg   L_0f01
	cmp    $0x4f,%ax
	jne   L_0eca
	jmp   L_0ae0
L_0eca:
	jg   L_0ee1
	cmp    $0x25,%ax
	je   L_0eac
	cmp    $0x44,%ax
	jne   L_0ed9
	jmp   L_0afc
L_0ed9:
	cmp    $0x4c,%ax
L_0edc:
	jne   L_0ee1
	jmp   L_0ab9
L_0ee1:
	cmp    $0x55,%ax
	jne   L_0ee9
	jmp   L_0b45
L_0ee9:
	cmp    $0x58,%ax
	jne   L_0ef1
	jmp   L_0aee
L_0ef1:
	cmp    $0x63,%ax
	jne   L_0ef9
	jmp   L_0bf8
L_0ef9:
	cmp    $0x64,%ax
	jne   L_0f01
	jmp   L_0b0c
L_0f01:
	cmp    $0x6f,%ax
	jne   L_0f09
	jmp   L_0ae7
L_0f09:
	jg   L_0f20
	cmp    $0x66,%ax
	jne   L_0f13
	jmp   L_0cbf
L_0f13:
	cmp    $0x67,%ax
	jne   L_0f1b
	jmp   L_0e71
L_0f1b:
	cmp    $0x6c,%ax
	je   L_0edc
L_0f20:
	cmp    $0x72,%ax
	jne   L_0f28
	jmp   L_0cb5
L_0f28:
	cmp    $0x73,%ax
	jne   L_0f30
	jmp   L_0c7c
L_0f30:
	cmp    $0x75,%ax
	jne   L_0f38
	jmp   L_0b55
L_0f38:
	cmp    $0x78,%ax
	jne   L_0f40
	jmp   L_0af5
L_0f40:
	jmp   L_0eb5
L_0f43:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0f67
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_15f0
	add    $0x8,%sp
L_0f67:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_10d6


L_0f71:
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
	call   L_10d6


L_0f9a:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_0fb8
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_0fb8:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_0fc1:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_0fc1
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_0fd1:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_0fd1
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0f71
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_1012
	jne   L_1007
	or     $0x0,%ax
	je   L_1012
L_1007:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_1012:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_10d6


L_101a:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_1036
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_1036:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_104d
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_104d:
	push   %dx
	push   %ax
	call   L_0f9a
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_1067
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_1067:
	ret
	call   L_10d6
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_101a
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0f71
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_10d6


L_10a2:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0f9a
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0f71
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_10d6:
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


L_10f8:
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
	call   L_1274
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_1132
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_1132:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_1139:
	cmpw   $0x0,-0xa(%bp)
	jle   L_1153
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_1153
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_1139
L_1153:
	cmpw   $0x0,-0x8(%bp)
	jl   L_1164
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_1170
L_1164:
	cmpw   $0x0,-0x8(%bp)
	jge   L_116e
	cmpw   $0xfffd,-0x8(%bp)
L_116e:
	jge   L_11ef
L_1170:
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
L_118b:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_11a5
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_118b
L_11a5:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_11c1
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_11c7
L_11c1:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_11c7:
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
	jmp   L_125f
L_11ef:
	cmpw   $0x0,-0x8(%bp)
	jg   L_1211
	cmpb   $0x30,(%di)
	je   L_1200
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1200:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1211
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_1200
L_1211:
	movw   $0x1,-0xa(%bp)
L_1216:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_123e
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
	jne   L_1239
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1239:
	incw   -0xa(%bp)
	jmp   L_1216
L_123e:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_125f
L_1246:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_1259
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_1246
L_1259:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_125f:
	cmpb   $0x2e,-0x1(%si)
	jne   L_1266
	dec    %si
L_1266:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1274:
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
	call   L_12cb
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_12a0:
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
	call   L_12cb
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_12cb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_12de
	movw   $0x0,0xc(%bp)
L_12de:
	cmpw   $0x4f,0xc(%bp)
	jl   L_12e9
	movw   $0x4e,0xc(%bp)
L_12e9:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_01a6,%si
	fldl   0x4(%bp)
	fcompl D_01f6
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jae   L_1323
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x46(%bp)
	fstpl  0x4(%bp)
L_1323:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1c1c
	fstpl  -0x46(%bp)
	add    $0xa,%sp
	fldl   -0x46(%bp)
	fstpl  0x4(%bp)
	fldl   -0xc(%bp)
	fcompl D_01fe
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jne   L_1364
	jmp   L_1415
L_1364:
	mov    $D_01a6,%si
L_1367:
	fldl   -0xc(%bp)
	fcompl D_0206
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jb   L_13ce
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	fmull  D_020e
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1c1c
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	fldl   -0x4e(%bp)
	fmull  D_0216
	faddl  D_021e
	fistps -0x60(%bp)
	fwait
	mov    -0x60(%bp),%ax
	lea    -0x3e(%bp),%dx
	mov    %di,%bx
	shl    %bx
	add    %bx,%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	inc    %di
	jmp   L_1367
L_13ce:
	mov    $0x1,%ax
	push   %ax
	push   %si
	fldl   -0xc(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   L_1579
	add    $0x6,%sp
	mov    %ax,%si
L_13e9:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_140b
	xor    %ax,%ax
	push   %ax
	push   %si
	lea    -0x3e(%bp),%ax
	mov    %di,%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_1579
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_13e9
L_140b:
	mov    %si,%ax
	sub    $D_01a6,%ax
	mov    %ax,%di
	jmp   L_1495
L_1415:
	fldl   0x4(%bp)
	fcompl D_0226
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jbe   L_1495
L_142b:
	fldl   0x4(%bp)
	fmull  D_022e
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	fcompl D_0236
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_1461
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	sub    $0x3,%di
	jmp   L_142b
L_1461:
	fldl   0x4(%bp)
	fmull  D_023e
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	fcompl D_0246
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_1495
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_1461
L_1495:
	mov    0xc(%bp),%ax
	add    $D_01a6,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_14a7
	add    %di,-0x16(%bp)
L_14a7:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_01a6,-0x16(%bp)
	jae   L_14bd
	movb   $0x0,D_01a6
	mov    $D_01a6,%ax
	jmp   L_1526
L_14bd:
	cmp    -0x16(%bp),%si
	ja   L_1517
	cmp    $D_01f6,%si
	jae   L_1517
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	fmull  D_024e
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1c1c
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
	call   L_1579
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_14bd
L_1517:
	cmpw   $D_01f6,-0x16(%bp)
	jb   L_1528
	movb   $0x0,D_01f5
	mov    $D_01a6,%ax
L_1526:
	jmp   L_1572
L_1528:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_1530:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_156c
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_01a6,-0x16(%bp)
	jbe   L_154f
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_1530
L_154f:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_1530
	cmp    $D_01a6,%si
	jbe   L_1569
	movb   $0x30,(%si)
L_1569:
	inc    %si
	jmp   L_1530
L_156c:
	movb   $0x0,(%si)
	mov    $D_01a6,%ax
L_1572:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1579:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%si
	xor    %di,%di
L_1584:
	or     %si,%si
	je   L_1595
	mov    %si,%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	mov    %ax,%si
	inc    %di
	jmp   L_1584
L_1595:
	cmpw   $0x0,0x8(%bp)
	jne   L_15ae
	mov    %di,%si
L_159d:
	cmp    $0x4,%si
	jge   L_15ae
	mov    0x6(%bp),%bx
	movb   $0x30,(%bx)
	incw   0x6(%bp)
	inc    %si
	jmp   L_159d
L_15ae:
	mov    0x6(%bp),%ax
	add    %di,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%si
L_15be:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_15e7
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
	jmp   L_15be
L_15e7:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_15f0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_15fd:
	cmpw   $0x0,0x8(%bp)
	jl   L_1606
	jmp   L_1694
L_1606:
	cmpb   $0x2d,(%di)
	jne   L_1643
	cmpw   $0x30,0xc(%bp)
	jne   L_1643
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1632
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
	jmp   L_1642
L_1632:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_16e2
	add    $0x4,%sp
L_1642:
	dec    %si
L_1643:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1661
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
	jmp   L_166d
L_1661:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_16e2
	add    $0x4,%sp
L_166d:
	incw   0x8(%bp)
	jmp   L_15fd
L_1672:
	dec    %si
	jl   L_16a8
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1696
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
L_1694:
	jmp   L_1672
L_1696:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_16e2
	add    $0x4,%sp
	jmp   L_1672
L_16a8:
	cmpw   $0x0,0x8(%bp)
	je   L_16dd
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_16cc
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
	jmp   L_16d8
L_16cc:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_16e2
	add    $0x4,%sp
L_16d8:
	decw   0x8(%bp)
	jmp   L_16a8
L_16dd:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_16e2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_16fd
	mov    $0xffff,%ax
	jmp   L_17c4
L_16fd:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1730
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
	call   L_1c02
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_17b5
L_1730:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1782
	cmp    $D_025e,%di
	jne   L_175b
	mov    D_0265,%al
	cbtw
	push   %ax
	call   L_1b1e
	pop    %cx
	or     %ax,%ax
	je   L_1750
	orb    $0x4,0x6(%di)
	jmp   L_16fd
L_1750:
	movw   $D_0c28,0x4(%di)
	movw   $D_0c28,(%di)
	jmp   L_16fd
L_175b:
	mov    $0x200,%ax
	push   %ax
	call   L_18b6
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1772
	orb    $0x4,0x6(%di)
	jmp   L_16fd
L_1772:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_17a4
L_1782:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_17a4
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1c02
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_17a4:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_17b5:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_17c6
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_17c4:
	jmp   L_17c9
L_17c6:
	mov    0x4(%bp),%ax
L_17c9:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_17d0:
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
	jne   L_181c
	mov    0x4(%di),%si
	or     %si,%si
	je   L_181c
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_181c
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1c02
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_181c
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_181e
L_181c:
	xor    %ax,%ax
L_181e:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1823:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0256,%di
L_182b:
	cmp    D_02f6,%di
	jae   L_183b
	push   %di
	call   L_183f
	pop    %cx
	add    $0x8,%di
	jmp   L_182b
L_183b:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_183f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_1892
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_1892
	push   %di
	call   L_17d0
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1b0a
	pop    %cx
	or     %ax,%ax
	jge   L_1874
	mov    $0xffff,%si
L_1874:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_1884
	pushw  0x4(%di)
	call   L_1a00
	pop    %cx
L_1884:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_1892
	movw   $0x0,0x4(%di)
L_1892:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_18b6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_02f8
	jne   L_18e1
	mov    $D_02fa,%ax
	or     $0x1,%ax
	mov    %ax,D_02f8
	mov    $D_02f8,%ax
	or     $0x1,%ax
	mov    %ax,D_02fa
	/* dual-reloc split: movw $D_02fa,D_102a */
	mov	$D_02fa,%ax
	mov	%ax,D_102a
	/* dual-reloc split: movw $D_02f8,D_1028 */
	mov	$D_02f8,%ax
	mov	%ax,D_1028
L_18e1:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_1028,%di
L_18f5:
	movw   $0x0,D_02fc
L_18fb:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_192a
L_1902:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_1911
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_1902
L_1911:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_192a
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_192a
	jmp   L_19d1
L_192a:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_18fb
	cmp    D_102a,%si
	jne   L_1943
	cmp    $D_02f8,%di
	je   L_1947
L_1943:
	xor    %ax,%ax
	jmp   L_197f
L_1947:
	incw   D_02fc
	cmpw   $0x1,D_02fc
	jle   L_18fb
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_02fc
	xor    %ax,%ax
	push   %ax
	call   L_1ae4
	pop    %cx
	mov    %ax,%si
	mov    D_02fc,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_1981
	xor    %ax,%ax
L_197f:
	jmp   L_1994
L_1981:
	mov    D_02fc,%ax
	shl    %ax
	push   %ax
	call   L_1ae4
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_1996
	xor    %ax,%ax
L_1994:
	jmp   L_19fb
L_1996:
	mov    D_102a,%bx
	mov    %si,(%bx)
	mov    D_102a,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_19b4
	mov    D_102a,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_102a,%bx
	mov    %ax,(%bx)
L_19b4:
	mov    D_02fc,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_102a
	mov    $D_02f8,%ax
	or     $0x1,%ax
	mov    D_102a,%bx
	mov    %ax,(%bx)
	jmp   L_18f5
L_19d1:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_1028
	cmp    %ax,%si
	jbe   L_19f0
	mov    D_1028,%bx
	mov    (%bx),%ax
	mov    %ax,D_102c
	mov    D_1028,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_19f0:
	mov    D_1028,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_19fb:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1a00:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_1028
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
	je   L_1a35
	push   %di
	call   L_1a00
	pop    %cx
L_1a35:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_18b6
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_1a54
	cmp    %di,%si
	jne   L_1a58
L_1a54:
	mov    %si,%ax
	jmp   L_1ac0
L_1a58:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_1a77
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_1a77:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_1a95
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_1a77
L_1a95:
	cmp    %di,%si
	jae   L_1abe
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_1abe
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
	mov    D_102c,%ax
	mov    %ax,(%bx)
L_1abe:
	mov    %si,%ax
L_1ac0:
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
	jns    L_1adb
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1ae2
L_1adb:
	mov    0x4(%bp),%bx
	mov    %bx,D_02fe
L_1ae2:
	pop    %bp
	ret


L_1ae4:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_02fe,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1afb
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1b08
L_1afb:
	mov    D_02fe,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_02fe
L_1b08:
	pop    %bp
	ret


L_1b0a:
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
	jns    L_1b1b
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1b1b:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1b1e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1b9f
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_1b3b
	xor    %ax,%ax
	jmp   L_1b3e
L_1b3b:
	mov    $0x1,%ax
L_1b3e:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1b46:
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
	je	open_wronly_5_f190
	and	$0xfffc,%cx
	or	$0x241,%cx
	mov	$0x1b6,%dx
open_wronly_5_f190:
	mov	$__NR_open,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1b5a
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1b5a:
	pop    %bp
	ret


L_1b5c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x6(%bp)
L_1b6d:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_1b6d
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7409,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1bb8
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b9f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1bb8
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1bb8:
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
	jns    L_1bcf
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1bcf:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1bd2:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall unlink(10) */
	mov    0x4(%bp),%ax
	/* unlink -> __NR_unlink */
	xchg	%ax,%bx
	mov	$__NR_unlink,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1be3
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1be3:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1be6:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall wait(7) */
	/* wait -> __NR_wait4 */
	xchg	%ax,%bx
	mov	%dx,%cx
	mov	$__NR_wait4,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1bf6
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1bf4:
	pop    %bp
	ret
L_1bf6:
	mov    0x4(%bp),%bx
	cmp    $0x0,%bx
	je   L_1bf4
	mov    %dx,(%bx)
	jmp   L_1bf4


L_1c02:
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
	jns    L_1c19
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1c19:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1c1c:
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


L_1c38:
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
	jns    L_1c49
	neg	%ax
	mov	%ax,D_0bd8	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1c49:
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
	mov	%ax,D_0bd8	/* errno */
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
	.word	D_0fbf	/* ptr: 0x0fbf */
D_0004:
	.word	D_0740	/* ptr: 0x2740 */
D_0006:
	.word	D_0bb0	/* ptr: 0x2bb0 */
D_0008:
	.word	D_0720	/* ptr: 0x2720 */
D_000a:
	.word	D_0308	/* ptr: 0x2308 */
D_000c:
	.word	D_0320	/* ptr: 0x2320 */
D_000e:
	.byte	0x00, 0x00	/* |..| */
D_0010:
	.byte	0x00, 0x00	/* |..| */
D_0012:
	.byte	0x00, 0x00	/* |..| */
D_0014:
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_0018:
	.byte	0x00, 0x00	/* |..| */
D_001a:
	.byte	0x00, 0x00	/* |..| */
D_001c:
	.byte	0x00, 0x00	/* |..| */
D_001e:
	.word	D_0024	/* ptr: 0x2024 */
D_0020:
	.byte	0x00, 0x00	/* |..| */
D_0022:
	.byte	0x2c, 0x20	/* |, | */
D_0024:
	.ascii	"/lib/c0\0"
	.ascii	"a.out\0"
D_0032:
	.byte	0x55, 0x73, 0x61, 0x67, 0x65, 0x3a, 0x20, 0x63	/* |Usage: c| */
	.byte	0x63, 0x20, 0x2d, 0x5b, 0x44, 0x49, 0x4f, 0x50	/* |c -[DIOP| */
	.byte	0x53, 0x55, 0x63, 0x66, 0x69, 0x6c, 0x6e, 0x6f	/* |SUcfilno| */
	.byte	0x70, 0x6d, 0x73, 0x7a, 0x77, 0x5d, 0x20, 0x2e	/* |pmszw] .| */
	.byte	0x2e, 0x2e, 0x20, 0x66, 0x69, 0x6c, 0x65, 0x2e	/* |.. file.| */
	.byte	0x5b, 0x63, 0x69, 0x73, 0x6f, 0x5d, 0x20, 0x2e	/* |[ciso] .| */
	.byte	0x2e, 0x2e, 0x0a, 0x00	/* |....| */
D_0066:
	.byte	0x63, 0x70	/* |cp| */
	.word	D_0070	/* ptr: 0x0070 */
D_006a:
	.byte	0x63, 0x30, 0x00	/* |c0.| */
D_006d:
	.byte	0x61, 0x73, 0x00	/* |as.| */
D_0070:
	.ascii	"copt\0"
D_0075:
	.byte	0x6c, 0x64, 0x00	/* |ld.| */
D_0078:
	.byte	0x2d, 0x58, 0x00	/* |-X.| */
D_007b:
	.byte	0x63, 0x63, 0x3a, 0x20, 0x69, 0x6e, 0x76, 0x61	/* |cc: inva| */
	.byte	0x6c, 0x69, 0x64, 0x20, 0x6f, 0x70, 0x74, 0x69	/* |lid opti| */
	.byte	0x6f, 0x6e, 0x20, 0x60, 0x25, 0x73, 0x27, 0x0a	/* |on `%s'.| */
	.byte	0x00	/* |.| */
D_0094:
	.ascii	"/lib/mcrt0.o\0"
D_00a1:
	.ascii	"/lib/crt0.o\0"
D_00ad:
	.byte	0x63, 0x63, 0x3a, 0x20, 0x6e, 0x6f, 0x20, 0x73	/* |cc: no s| */
	.byte	0x6f, 0x75, 0x72, 0x63, 0x65, 0x20, 0x66, 0x69	/* |ource fi| */
	.byte	0x6c, 0x65, 0x20, 0x60, 0x25, 0x73, 0x27, 0x0a	/* |le `%s'.| */
	.byte	0x00	/* |.| */
D_00c6:
	.byte	0x63, 0x63, 0x3a, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |cc: %s..| */
D_00ce:
	.ascii	"/lib/cpp\0"
D_00d7:
	.ascii	"coptXXXXXX\0"
D_00e2:
	.byte	0x63, 0x63, 0x3a, 0x20, 0x63, 0x61, 0x6e, 0x27	/* |cc: can'| */
	.byte	0x74, 0x20, 0x6c, 0x69, 0x6e, 0x6b, 0x20, 0x25	/* |t link %| */
	.byte	0x73, 0x20, 0x74, 0x6f, 0x20, 0x25, 0x73, 0x0a	/* |s to %s.| */
	.byte	0x00	/* |.| */
D_00fb:
	.ascii	"/lib/copt\0"
D_0105:
	.byte	0x43, 0x61, 0x6e, 0x27, 0x74, 0x20, 0x75, 0x6e	/* |Can't un| */
	.byte	0x6c, 0x69, 0x6e, 0x6b, 0x20, 0x25, 0x73, 0x0a	/* |link %s.| */
	.byte	0x00	/* |.| */
D_0116:
	.byte	0x63, 0x63, 0x3a, 0x20, 0x63, 0x61, 0x6e, 0x27	/* |cc: can'| */
	.byte	0x74, 0x20, 0x6c, 0x69, 0x6e, 0x6b, 0x20, 0x25	/* |t link %| */
	.byte	0x73, 0x20, 0x74, 0x6f, 0x20, 0x25, 0x73, 0x0a	/* |s to %s.| */
	.byte	0x00	/* |.| */
D_012f:
	.ascii	"/bin/as\0"
D_0137:
	.byte	0x2d, 0x6c, 0x63, 0x00	/* |-lc.| */
D_013b:
	.ascii	"/bin/ld\0"
D_0143:
	.byte	0x63, 0x63, 0x3a, 0x20, 0x6f, 0x70, 0x65, 0x6e	/* |cc: open| */
	.byte	0x20, 0x65, 0x72, 0x72, 0x6f, 0x72, 0x20, 0x6f	/* | error o| */
	.byte	0x6e, 0x20, 0x66, 0x69, 0x6c, 0x65, 0x20, 0x60	/* |n file `| */
	.byte	0x25, 0x73, 0x27, 0x0a, 0x00	/* |%s'..| */
D_0160:
	.byte	0x63, 0x63, 0x3a, 0x20, 0x63, 0x72, 0x65, 0x61	/* |cc: crea| */
	.byte	0x74, 0x65, 0x20, 0x65, 0x72, 0x72, 0x6f, 0x72	/* |te error| */
	.byte	0x20, 0x6f, 0x6e, 0x20, 0x66, 0x69, 0x6c, 0x65	/* | on file| */
	.byte	0x20, 0x60, 0x25, 0x73, 0x27, 0x0a, 0x00	/* | `%s'..| */
D_017f:
	.byte	0x63, 0x63, 0x3a, 0x20, 0x63, 0x6f, 0x75, 0x6c	/* |cc: coul| */
	.byte	0x64, 0x20, 0x6e, 0x6f, 0x74, 0x20, 0x65, 0x78	/* |d not ex| */
	.byte	0x65, 0x63, 0x75, 0x74, 0x65, 0x20, 0x60, 0x25	/* |ecute `%| */
	.byte	0x73, 0x27, 0x0a, 0x00, 0x00	/* |s'...| */
D_019c:
	.byte	0x2f, 0x00	/* |/.| */
D_019e:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_01a6:
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
D_01f5:
	.byte	0x00	/* |.| */
D_01f6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_01fe:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0206:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_020e:
	.byte	0x2c, 0x43, 0x1c, 0xeb, 0xe2, 0x36, 0x1a, 0x3f	/* |,C...6.?| */
D_0216:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_021e:
	.byte	0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0xb9, 0x3f	/* |.......?| */
D_0226:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_022e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x8f, 0x40	/* |.....@.@| */
D_0236:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_023e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0246:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_024e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_0256:
	.word	D_0e28	/* ptr: 0x2e28 */
	.byte	0x00, 0x00	/* |..| */
	.word	D_0e28	/* ptr: 0x2e28 */
	.byte	0x01, 0x00	/* |..| */
D_025e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_0264:
	.byte	0x02	/* |.| */
D_0265:
	.byte	0x01	/* |.| */
D_0266:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.word	D_0206	/* ptr: 0x0206 */
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
D_02f6:
	.byte	0xf6, 0x22	/* |."| */
D_02f8:
	.byte	0x00, 0x00	/* |..| */
D_02fa:
	.byte	0x00, 0x00	/* |..| */
D_02fc:
	.byte	0x00, 0x00	/* |..| */
D_02fe:
	.word	D_102e	/* ptr: 0x302e */

	.bss

D_0300:	.space	2	/* bss word @ 0x0300 */
	.space	6
D_0308:	.space	2	/* bss word @ 0x0308 */
	.space	22
D_0320:	.space	2	/* bss word @ 0x0320 */
	.space	1022
D_0720:	.space	2	/* bss word @ 0x0720 */
	.space	18
D_0734:	.space	2	/* bss word @ 0x0734 */
D_0736:	.space	2	/* bss word @ 0x0736 */
D_0738:	.space	2	/* bss word @ 0x0738 */
	.space	6
D_0740:	.space	2	/* bss word @ 0x0740 */
	.space	102
D_07a8:	.space	2	/* bss word @ 0x07a8 */
	.space	1022
D_0ba8:	.space	2	/* bss word @ 0x0ba8 */
	.space	6
D_0bb0:	.space	2	/* bss word @ 0x0bb0 */
	.space	38
D_0bd8:	.space	2	/* bss word @ 0x0bd8 */
	.space	6
D_0be0:	.space	2	/* bss word @ 0x0be0 */
D_0be2:	.space	2	/* bss word @ 0x0be2 */
D_0be4:	.space	2	/* bss word @ 0x0be4 */
D_0be6:	.space	2	/* bss word @ 0x0be6 */
D_0be8:	.space	2	/* bss word @ 0x0be8 */
D_0bea:	.space	2	/* bss word @ 0x0bea */
D_0bec:	.space	2	/* bss word @ 0x0bec */
D_0bee:	.space	2	/* bss word @ 0x0bee */
D_0bf0:	.space	2	/* bss word @ 0x0bf0 */
D_0bf2:	.space	2	/* bss word @ 0x0bf2 */
D_0bf4:	.space	2	/* bss word @ 0x0bf4 */
D_0bf6:	.space	2	/* bss word @ 0x0bf6 */
D_0bf8:	.space	2	/* bss word @ 0x0bf8 */
D_0bfa:	.space	2	/* bss word @ 0x0bfa */
D_0bfc:	.space	2	/* bss word @ 0x0bfc */
D_0bfe:	.space	2	/* bss word @ 0x0bfe */
D_0c00:	.space	2	/* bss word @ 0x0c00 */
D_0c02:	.space	2	/* bss word @ 0x0c02 */
D_0c04:	.space	2	/* bss word @ 0x0c04 */
D_0c06:	.space	2	/* bss word @ 0x0c06 */
D_0c08:	.space	2	/* bss word @ 0x0c08 */
D_0c0a:	.space	2	/* bss word @ 0x0c0a */
D_0c0c:	.space	2	/* bss word @ 0x0c0c */
D_0c0e:	.space	2	/* bss word @ 0x0c0e */
D_0c10:	.space	2	/* bss word @ 0x0c10 */
D_0c12:	.space	2	/* bss word @ 0x0c12 */
D_0c14:	.space	2	/* bss word @ 0x0c14 */
D_0c16:	.space	2	/* bss word @ 0x0c16 */
D_0c18:	.space	2	/* bss word @ 0x0c18 */
D_0c1a:	.space	2	/* bss word @ 0x0c1a */
D_0c1c:	.space	2	/* bss word @ 0x0c1c */
D_0c1e:	.space	2	/* bss word @ 0x0c1e */
D_0c20:	.space	2	/* bss word @ 0x0c20 */
	.space	6
D_0c28:	.space	2	/* bss word @ 0x0c28 */
	.space	510
D_0e28:	.space	2	/* bss word @ 0x0e28 */
	.space	405
D_0fbf:	.space	2	/* bss word @ 0x0fbf */
	.space	103
D_1028:	.space	2	/* bss word @ 0x1028 */
D_102a:	.space	2	/* bss word @ 0x102a */
D_102c:	.space	2	/* bss word @ 0x102c */
D_102e:	/* endcore: initial break */

/* end of file */
