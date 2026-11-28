/* make.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 make.s -o make.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* Wrapping near-call/jmp targets (16-bit address space) */
	.set L_a812, 0xa812	/* wrapping branch target */
	.set L_b5c3, 0xb5c3	/* wrapping branch target */
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: make  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00004d7c  (19836 bytes)
//   a_data:   0x000021c8  (8648 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x10,%sp
	/* TODO: data ref: mov    $0x2097,%ax */
	mov    $D_0097,%ax
	push   %ax
	call   L_1967
	pop    %cx
	movw   $0x0,-0xa(%bp)
	movb   $0x3,D_21f8
	/* TODO: data ref: movw   $0x20ab,-0x14(%bp) */
	movw   $D_00ab,-0x14(%bp)
L_0055:
	mov    -0x14(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_006f
	mov    -0x14(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	orb    $0x1,D_21f8(%bx)
	incw   -0x14(%bp)
	jmp   L_0055
L_006f:
	/* TODO: data ref: movw   $0x20c0,-0x14(%bp) */
	movw   $D_00c0,-0x14(%bp)
L_0074:
	mov    -0x14(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_008e
	mov    -0x14(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	orb    $0x2,D_21f8(%bx)
	incw   -0x14(%bp)
	jmp   L_0074
L_008e:
	movw   $0x1,D_227a
	movw   $0x1,-0x6(%bp)
L_0099:
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jge   L_00e8
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	je   L_00e3
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%bx
	cmpb   $0x2d,(%bx)
	je   L_00e3
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_1356
	pop    %cx
	or     %ax,%ax
	je   L_00e3
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
L_00e3:
	incw   -0x6(%bp)
	jmp   L_0099
L_00e8:
	/* TODO: data ref: mov    $0x20cb,%ax */
	mov    $D_00cb,%ax
	push   %ax
	/* TODO: data ref: mov    $0x20c9,%ax */
	mov    $D_00c9,%ax
	push   %ax
	call   L_12b4
	add    $0x4,%sp
	movw   $0x0,D_227a
	movw   $0x1,-0x6(%bp)
L_0101:
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jl   L_010c
	jmp   L_023e
L_010c:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	jne   L_011e
	jmp   L_0238
L_011e:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%bx
	cmpb   $0x2d,(%bx)
	je   L_0132
L_012f:
	jmp   L_0238
L_0132:
	movw   $0x1,-0x8(%bp)
L_0137:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	add    -0x8(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x11(%bp)
	or     %cl,%cl
	jne   L_0157
	jmp   L_022a
L_0157:
	mov    -0x11(%bp),%al
	cbtw
	jmp   L_0215
	/* TODO: data ref: movw   $0x1,0x2034 */
	movw   $0x1,D_0034
	jmp   L_01ac
	/* TODO: data ref: movw   $0x1,0x2036 */
	movw   $0x1,D_0036
	jmp   L_01ac
	/* TODO: data ref: movw   $0x1,0x2038 */
	movw   $0x1,D_0038
	jmp   L_01ac
	/* TODO: data ref: movw   $0x1,0x2046 */
	movw   $0x1,D_0046
	jmp   L_01ac
	/* TODO: data ref: movw   $0x0,0x203c */
	movw   $0x0,D_003c
	jmp   L_01ac
	/* TODO: data ref: movw   $0x1,0x203c */
	movw   $0x1,D_003c
	jmp   L_01ac
	/* TODO: data ref: movw   $0x1,0x203a */
	movw   $0x1,D_003a
	jmp   L_01ac
	/* TODO: data ref: movw   $0x1,0x203e */
	movw   $0x1,D_003e
	jmp   L_01ac
	/* TODO: data ref: movw   $0x1,0x2040 */
	movw   $0x1,D_0040
	jmp   L_01ac
	/* TODO: data ref: movw   $0x1,0x2042 */
	movw   $0x1,D_0042
L_01ac:
	jmp   L_01fd
	mov    0x4(%bp),%ax
	dec    %ax
	cmp    %ax,-0x6(%bp)
	jl   L_01bf
	/* TODO: data ref: mov    $0x20cd,%ax */
	mov    $D_00cd,%ax
	push   %ax
	call   L_142e
	pop    %cx
L_01bf:
	mov    -0x6(%bp),%ax
	inc    %ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_0493
	pop    %cx
	or     %ax,%ax
	je   L_01eb
	mov    -0x6(%bp),%ax
	inc    %ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x20f3,%ax */
	mov    $D_00f3,%ax
	push   %ax
	call   L_140a
	add    $0x4,%sp
L_01eb:
	mov    -0x6(%bp),%ax
	inc    %ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	incw   -0xa(%bp)
L_01fd:
	jmp   L_0213
L_01ff:
	mov    -0x11(%bp),%al
	/* TODO: data ref: mov    %al,0x204c */
	mov    %al,D_004c
	/* TODO: data ref: mov    $0x204c,%ax */
	mov    $D_004c,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2102,%ax */
	mov    $D_0102,%ax
	push   %ax
	call   L_140a
	add    $0x4,%sp
L_0213:
	jmp   L_0224
L_0215:
	sub    $0x53,%ax
	cmp    $0x21,%ax
	ja   L_01ff
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x204e(%bx)
L_0224:
	incw   -0x8(%bp)
	jmp   L_0137
L_022a:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
L_0238:
	incw   -0x6(%bp)
	jmp   L_0101
L_023e:
	cmpw   $0x0,-0xa(%bp)
	jne   L_0258
	/* TODO: data ref: mov    $0x211b,%ax */
	mov    $D_011b,%ax
	push   %ax
	call   L_0493
	pop    %cx
	or     %ax,%ax
	je   L_0258
	/* TODO: data ref: mov    $0x2124,%ax */
	mov    $D_0124,%ax
	push   %ax
	call   L_0493
	pop    %cx
L_0258:
	/* TODO: data ref: cmpw   $0x0,0x2036 */
	cmpw   $0x0,D_0036
	je   L_0266
	xor    %ax,%ax
	push   %ax
	call   L_052d
	pop    %cx
L_0266:
	/* TODO: data ref: mov    $0x212d,%ax */
	mov    $D_012d,%ax
	push   %ax
	call   L_0f7e
	pop    %cx
	or     %ax,%ax
	je   L_0276
	/* TODO: data ref: incw   0x2046 */
	incw   D_0046
L_0276:
	/* TODO: data ref: mov    $0x2135,%ax */
	mov    $D_0135,%ax
	push   %ax
	call   L_0f7e
	pop    %cx
	or     %ax,%ax
	je   L_0288
	/* TODO: data ref: movw   $0x1,0x2038 */
	movw   $0x1,D_0038
L_0288:
	/* TODO: data ref: mov    $0x213d,%ax */
	mov    $D_013d,%ax
	push   %ax
	call   L_0f7e
	pop    %cx
	mov    %ax,%di
	or     %ax,%ax
	je   L_029c
	mov    0x4(%di),%ax
	mov    %ax,D_21ca
L_029c:
	cmpw   $0x0,D_21ca
	jne   L_02b1
	/* TODO: data ref: mov    $0x2147,%ax */
	mov    $D_0147,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x4,%sp
L_02b1:
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3928
	add    $0x4,%sp
	and    $0x1,%ax
	/* TODO: data ref: mov    %ax,0x202e */
	mov    %ax,D_002e
	mov    $0x1,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_3928
	add    $0x4,%sp
	and    $0x1,%ax
	/* TODO: data ref: mov    %ax,0x2030 */
	mov    %ax,D_0030
	mov    $0x386,%ax
	push   %ax
	call   L_0462
	pop    %cx
	movw   $0x0,-0xc(%bp)
	movw   $0x1,-0x6(%bp)
L_02eb:
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jge   L_0341
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,-0x14(%bp)
	or     %ax,%ax
	je   L_033c
	pushw  -0x14(%bp)
	call   L_0f7e
	pop    %cx
	mov    %ax,%di
	or     %ax,%ax
	jne   L_031c
	pushw  -0x14(%bp)
	call   L_0f96
	pop    %cx
	mov    %ax,%di
L_031c:
	incw   -0xc(%bp)
	lea    -0x10(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	push   %di
	call   L_06c0
	add    $0x6,%sp
	/* TODO: data ref: cmpw   $0x0,0x2034 */
	cmpw   $0x0,D_0034
	je   L_033c
	mov    $0x1,%ax
	push   %ax
	call   L_052d
	pop    %cx
L_033c:
	incw   -0x6(%bp)
	jmp   L_02eb
L_0341:
	cmpw   $0x0,-0xc(%bp)
	jne   L_0378
	cmpw   $0x0,D_227c
	jne   L_0358
	/* TODO: data ref: mov    $0x2158,%ax */
	mov    $D_0158,%ax
	push   %ax
	call   L_142e
	pop    %cx
	jmp   L_0378
L_0358:
	lea    -0x10(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	pushw  D_227c
	call   L_06c0
	add    $0x6,%sp
	/* TODO: data ref: cmpw   $0x0,0x2034 */
	cmpw   $0x0,D_0034
	je   L_0378
	mov    $0x1,%ax
	push   %ax
	call   L_052d
	pop    %cx
L_0378:
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: cmpw   $0x0,0x2048 */
	cmpw   $0x0,D_0048
	je   L_03b3
	/* TODO: data ref: cmpw   $0x0,0x203a */
	cmpw   $0x0,D_003a
	jne   L_03d1
	/* TODO: data ref: cmpw   $0x0,0x2040 */
	cmpw   $0x0,D_0040
	jne   L_03d1
	/* TODO: data ref: mov    $0x2179,%ax */
	mov    $D_0179,%ax
	push   %ax
	call   L_13bd
	pop    %cx
	mov    %ax,%bx
	mov    0x4(%bx),%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
L_03b3:
	je   L_03eb
	pushw  -0x6(%bp)
	call   L_1530
	pop    %cx
	cmp    $0x0,%dx
	jl   L_03eb
	jg   L_03c8
	cmp    $0x0,%ax
	jbe   L_03eb
L_03c8:
	pushw  -0x6(%bp)
	call   L_0415
	pop    %cx
	or     %ax,%ax
L_03d1:
	jne   L_03eb
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x217b,%ax */
	mov    $D_017b,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x6,%sp
	pushw  -0x6(%bp)
	call   L_4d02
	pop    %cx
L_03eb:
	cmpb   $0x0,D_21d8
	je   L_03fa
	mov    $D_21d8,%ax
	push   %ax
	call   L_4d02
	pop    %cx
L_03fa:
	/* TODO: data ref: mov    $0x218d,%ax */
	mov    $D_018d,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x4,%sp
	mov    $0x2,%ax
	push   %ax
	call   _exit
	pop    %cx
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0415:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: mov    $0x218f,%ax */
	mov    $D_018f,%ax
	push   %ax
	call   L_0f7e
	pop    %cx
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_045b
	mov    -0x6(%bp),%bx
	mov    0x4(%bx),%di
L_0430:
	or     %di,%di
	je   L_045b
	mov    0x2(%di),%si
L_0437:
	or     %si,%si
	je   L_0457
	mov    0x2(%si),%bx
	pushw  0x2(%bx)
	pushw  0x4(%bp)
	call   L_4bc8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0453
	mov    $0x1,%ax
	jmp   L_045d
L_0453:
	mov    (%si),%si
	jmp   L_0437
L_0457:
	mov    (%di),%di
	jmp   L_0430
L_045b:
	xor    %ax,%ax
L_045d:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0462:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: cmpw   $0x0,0x202e */
	cmpw   $0x0,D_002e
	jne   L_047b
	pushw  0x4(%bp)
	mov    $0x2,%ax
	push   %ax
	call   L_3928
	add    $0x4,%sp
L_047b:
	/* TODO: data ref: cmpw   $0x0,0x2030 */
	cmpw   $0x0,D_0030
	jne   L_048f
	pushw  0x4(%bp)
	mov    $0x3,%ax
	push   %ax
	call   L_3928
	add    $0x4,%sp
L_048f:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0493:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: mov    0x2094,%ax */
	mov    D_0094,%ax
	/* TODO: data ref: incw   0x2094 */
	incw   D_0094
	or     %ax,%ax
	jne   L_04b2
	/* TODO: data ref: cmpw   $0x0,0x203e */
	cmpw   $0x0,D_003e
	jne   L_04b2
	xor    %ax,%ax
	push   %ax
	call   L_04f2
	pop    %cx
L_04b2:
	/* TODO: data ref: mov    $0x2199,%ax */
	mov    $D_0199,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4bc8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_04cd
	mov    $D_211e,%ax
	push   %ax
	call   L_04f2
	pop    %cx
	jmp   L_04ed
L_04cd:
	/* TODO: data ref: mov    $0x219b,%ax */
	mov    $D_019b,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_32f4
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_04ea
	pushw  -0x6(%bp)
	call   L_04f2
	pop    %cx
	jmp   L_04ed
L_04ea:
	mov    $0x1,%ax
L_04ed:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_04f2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	mov    %ax,D_21f0
	movw   $0x0,D_2278
	movw   $0x0,D_21ee
	call   L_2384
	or     %ax,%ax
	je   L_0518
	/* TODO: data ref: mov    $0x219d,%ax */
	mov    $D_019d,%ax
	push   %ax
	call   L_142e
	pop    %cx
L_0518:
	cmpw   $0x0,D_21f0
	je   L_0527
	pushw  D_21f0
	call   L_48fd
	pop    %cx
L_0527:
	xor    %ax,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_052d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	cmpw   $0x0,0x4(%bp)
	je   L_0574
	/* TODO: data ref: mov    $0x21b4,%ax */
	mov    $D_01b4,%ax
	push   %ax
	call   L_35bc
	pop    %cx
	mov    D_21ce,%ax
	mov    %ax,-0xc(%bp)
L_0549:
	cmpw   $0x0,-0xc(%bp)
	je   L_0574
	mov    -0xc(%bp),%bx
	pushw  0x4(%bx)
	mov    -0xc(%bp),%bx
	mov    0x2(%bx),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	/* TODO: data ref: mov    $0x21c7,%ax */
	mov    $D_01c7,%ax
	push   %ax
	call   L_35bc
	add    $0x6,%sp
	mov    -0xc(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0xc(%bp)
	jmp   L_0549
L_0574:
	cmpw   $0x0,D_21ec
	je   L_0583
	/* TODO: data ref: mov    $0x21d0,%ax */
	mov    $D_01d0,%ax
	push   %ax
	call   L_35bc
	pop    %cx
L_0583:
	mov    D_21ec,%ax
	mov    %ax,-0xa(%bp)
L_0589:
	cmpw   $0x0,-0xa(%bp)
	je   L_05af
	mov    -0xa(%bp),%bx
	pushw  0x4(%bx)
	mov    -0xa(%bp),%bx
	pushw  0x2(%bx)
	/* TODO: data ref: mov    $0x21d9,%ax */
	mov    $D_01d9,%ax
	push   %ax
	call   L_35bc
	add    $0x6,%sp
	mov    -0xa(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	jmp   L_0589
L_05af:
	mov    D_21cc,%ax
	mov    %ax,-0x6(%bp)
L_05b5:
	cmpw   $0x0,-0x6(%bp)
	jne   L_05be
	jmp   L_06a8
L_05be:
	mov    -0x6(%bp),%bx
	pushw  0x2(%bx)
	/* TODO: data ref: mov    $0x21e3,%ax */
	mov    $D_01e3,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
	mov    -0x6(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	je   L_05df
	/* TODO: data ref: mov    $0x21e8,%ax */
	mov    $D_01e8,%ax
	push   %ax
	call   L_35bc
	pop    %cx
L_05df:
	cmpw   $0x0,0x4(%bp)
	je   L_05f9
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	and    $0x7,%ax
	push   %ax
	/* TODO: data ref: mov    $0x21ea,%ax */
	mov    $D_01ea,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
L_05f9:
	mov    -0x6(%bp),%ax
	cmp    D_227c,%ax
	jne   L_060a
	/* TODO: data ref: mov    $0x21f4,%ax */
	mov    $D_01f4,%ax
	push   %ax
	call   L_35bc
	pop    %cx
L_060a:
	mov    -0x6(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,-0x10(%bp)
L_0613:
	cmpw   $0x0,-0x10(%bp)
	jne   L_061c
	jmp   L_069d
L_061c:
	mov    -0x10(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	je   L_065d
	/* TODO: data ref: mov    $0x2202,%ax */
	mov    $D_0202,%ax
	push   %ax
	call   L_35bc
	pop    %cx
L_0631:
	cmpw   $0x0,-0x8(%bp)
	je   L_065d
	mov    -0x8(%bp),%bx
	cmpw   $0x0,0x2(%bx)
	je   L_0653
	mov    -0x8(%bp),%bx
	mov    0x2(%bx),%bx
	pushw  0x2(%bx)
	/* TODO: data ref: mov    $0x2210,%ax */
	mov    $D_0210,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
L_0653:
	mov    -0x8(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x8(%bp)
	jmp   L_0631
L_065d:
	mov    -0x10(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,-0xe(%bp)
	or     %ax,%ax
	je   L_0692
	/* TODO: data ref: mov    $0x2215,%ax */
	mov    $D_0215,%ax
	push   %ax
	call   L_35bc
	pop    %cx
L_0672:
	cmpw   $0x0,-0xe(%bp)
	je   L_0692
	mov    -0xe(%bp),%bx
	pushw  0x2(%bx)
	/* TODO: data ref: mov    $0x2222,%ax */
	mov    $D_0222,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
	mov    -0xe(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_0672
L_0692:
	mov    -0x10(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_0613
L_069d:
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x6(%bp)
	jmp   L_05b5
L_06a8:
	/* TODO: data ref: mov    $0x2227,%ax */
	mov    $D_0227,%ax
	push   %ax
	call   L_35bc
	pop    %cx
	mov    $D_2126,%ax
	push   %ax
	call   L_488e
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_06c0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x174,%sp
	mov    0x4(%bp),%di
	or     %di,%di
	jne   L_06e1
	mov    0x8(%bp),%bx
	movw   $0x0,(%bx)
	movw   $0x0,0x2(%bx)
	xor    %ax,%ax
	jmp   L_0727
L_06e1:
	/* TODO: data ref: cmpw   $0x0,0x2034 */
	cmpw   $0x0,D_0034
	je   L_0700
	pushw  0x6(%bp)
	pushw  0x2(%di)
	/* TODO: data ref: mov    $0x222a,%ax */
	mov    $D_022a,%ax
	push   %ax
	call   L_35bc
	add    $0x6,%sp
	mov    $D_2126,%ax
	push   %ax
	call   L_488e
	pop    %cx
L_0700:
	testw  $0x7,0x6(%di)
	jle   L_072a
	mov    0x8(%bp),%bx
	mov    0x8(%di),%ax
	mov    0xa(%di),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
	mov    0x6(%di),%ax
	and    $0x7,%ax
	cmp    $0x3,%ax
	jne   L_0725
	mov    $0x1,%ax
	jmp   L_0727
L_0725:
	xor    %ax,%ax
L_0727:
	jmp   L_0d10
L_072a:
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x16(%bp)
	movw   $0x0,-0x14(%bp)
	movw   $0x0,-0x2a(%bp)
	movw   $0x0,-0x2c(%bp)
	pushw  0x2(%di)
	call   L_1530
	pop    %cx
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
	movw   $0x0,-0x1e(%bp)
	movw   $0x0,-0x1c(%bp)
	movw   $0x0,-0xa(%bp)
	mov    $0x1,%ax
	mov    $0x0,%cl
	shl    %cl,%ax
	and    $0x7,%ax
	andw   $0xfff8,0x6(%di)
	or     %ax,0x6(%di)
	movw   $0x0,-0x178(%bp)
	mov    0x4(%di),%ax
	mov    %ax,-0x2e(%bp)
L_077d:
	cmpw   $0x0,-0x2e(%bp)
	je   L_07a6
	mov    -0x2e(%bp),%bx
	mov    0x2(%bx),%si
L_0789:
	or     %si,%si
	je   L_079c
	mov    (%si),%ax
	mov    %ax,-0x20(%bp)
	push   %si
	call   L_0ebb
	pop    %cx
	mov    -0x20(%bp),%si
	jmp   L_0789
L_079c:
	mov    -0x2e(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x2e(%bp)
	jmp   L_077d
L_07a6:
	mov    0x4(%di),%ax
	mov    %ax,-0x2e(%bp)
L_07ac:
	cmpw   $0x0,-0x2e(%bp)
	jne   L_07b5
	jmp   L_0952
L_07b5:
	movw   $0x0,-0xe(%bp)
	movw   $0x0,-0xc(%bp)
	mov    -0x2e(%bp),%bx
	mov    0x2(%bx),%si
L_07c5:
	or     %si,%si
	je   L_0843
	lea    -0x12(%bp),%ax
	push   %ax
	mov    0x6(%bp),%ax
	inc    %ax
	push   %ax
	pushw  0x2(%si)
	call   L_06c0
	add    $0x6,%sp
	add    %ax,-0x6(%bp)
	/* TODO: data ref: cmpw   $0x0,0x2034 */
	cmpw   $0x0,D_0034
	je   L_07fb
	pushw  -0x10(%bp)
	pushw  -0x12(%bp)
	mov    0x2(%si),%bx
	pushw  0x2(%bx)
	/* TODO: data ref: mov    $0x2239,%ax */
	mov    $D_0239,%ax
	push   %ax
	call   L_35bc
	add    $0x8,%sp
L_07fb:
	mov    -0x12(%bp),%ax
	mov    -0x10(%bp),%dx
	cmp    -0xc(%bp),%dx
	jl   L_0819
	jg   L_080d
	cmp    -0xe(%bp),%ax
	jbe   L_0819
L_080d:
	mov    -0x12(%bp),%ax
	mov    -0x10(%bp),%dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
L_0819:
	mov    -0x1a(%bp),%ax
	mov    -0x18(%bp),%dx
	cmp    -0x10(%bp),%dx
	jg   L_083f
	jl   L_082b
	cmp    -0x12(%bp),%ax
	jae   L_083f
L_082b:
	mov    0x2(%si),%bx
	pushw  0x2(%bx)
	pushw  -0x178(%bp)
	call   L_148f
	add    $0x4,%sp
	mov    %ax,-0x178(%bp)
L_083f:
	mov    (%si),%si
	jmp   L_07c5
L_0843:
	mov    0x6(%di),%ax
	mov    $0x3,%cx
	sar    %cl,%ax
	and    $0x7,%ax
	cmp    $0x2,%ax
	je   L_0856
	jmp   L_08fe
L_0856:
	mov    -0x2e(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_0862
	jmp   L_08fc
L_0862:
	mov    -0x1a(%bp),%ax
	mov    -0x18(%bp),%dx
	cmp    -0xc(%bp),%dx
	jl   L_0895
	jg   L_0874
	cmp    -0xe(%bp),%ax
	jb   L_0895
L_0874:
	cmpw   $0x0,-0x18(%bp)
	jne   L_088c
	cmpw   $0x0,-0x1a(%bp)
	jne   L_088c
	cmpw   $0x0,-0xc(%bp)
	jne   L_088c
	cmpw   $0x0,-0xe(%bp)
	je   L_0895
L_088c:
	mov    -0x2e(%bp),%bx
	cmpw   $0x0,0x2(%bx)
	jne   L_08fc
L_0895:
	/* TODO: data ref: mov    0x2048,%ax */
	mov    D_0048,%ax
	mov    %ax,-0x8(%bp)
	/* TODO: data ref: movw   $0x0,0x2048 */
	movw   $0x0,D_0048
	pushw  0x2(%di)
	/* TODO: data ref: mov    $0x2247,%ax */
	mov    $D_0247,%ax
	push   %ax
	call   L_12b4
	add    $0x4,%sp
	pushw  -0x178(%bp)
	call   L_14c3
	pop    %cx
	push   %ax
	/* TODO: data ref: mov    $0x2249,%ax */
	mov    $D_0249,%ax
	push   %ax
	call   L_12b4
	add    $0x4,%sp
	movw   $0x0,-0x178(%bp)
	/* TODO: data ref: cmpw   $0x0,0x2042 */
	cmpw   $0x0,D_0042
	jne   L_08db
	mov    -0x2e(%bp),%bx
	pushw  0x4(%bx)
	call   L_0d17
	pop    %cx
	add    %ax,-0x6(%bp)
L_08db:
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x224b,%ax */
	mov    $D_024b,%ax
	push   %ax
	call   L_12b4
	add    $0x4,%sp
	mov    -0x8(%bp),%ax
	/* TODO: data ref: mov    %ax,0x2048 */
	mov    %ax,D_0048
	call   L_157a
	mov    %ax,-0x1e(%bp)
	mov    %dx,-0x1c(%bp)
	movw   $0x1,-0xa(%bp)
L_08fc:
	jmp   L_0947
L_08fe:
	mov    -0x2e(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	je   L_0929
	cmpw   $0x0,-0x2c(%bp)
	je   L_0920
	pushw  0x2(%di)
	/* TODO: data ref: mov    $0x224d,%ax */
	mov    $D_024d,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x6,%sp
	jmp   L_0929
L_0920:
	mov    -0x2e(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,-0x2c(%bp)
L_0929:
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	cmp    -0x14(%bp),%dx
	jl   L_0947
	jg   L_093b
	cmp    -0x16(%bp),%ax
	jbe   L_0947
L_093b:
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0947:
	mov    -0x2e(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x2e(%bp)
	jmp   L_07ac
L_0952:
	mov    D_21ca,%ax
	mov    %ax,-0x2e(%bp)
L_0958:
	cmpw   $0x0,-0x2e(%bp)
	jne   L_0961
	jmp   L_0acd
L_0961:
	mov    -0x2e(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x22(%bp)
L_096a:
	cmpw   $0x0,-0x22(%bp)
	jne   L_0973
	jmp   L_0b07
L_0973:
	mov    -0x22(%bp),%bx
	mov    0x2(%bx),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x174(%bp)
	lea    -0xfa(%bp),%ax
	push   %ax
	pushw  -0x174(%bp)
	pushw  0x2(%di)
	call   L_10c8
	add    $0x6,%sp
	or     %ax,%ax
	je   L_09c1
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	lea    -0x15e(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x226e,%ax */
	mov    $D_026e,%ax
	push   %ax
	lea    -0xfa(%bp),%ax
	push   %ax
	call   L_1082
	add    $0x6,%sp
	push   %ax
	call   L_1597
	add    $0x6,%sp
	mov    D_21ca,%ax
	mov    %ax,-0x30(%bp)
L_09bd:
	cmpw   $0x0,-0x30(%bp)
L_09c1:
	jne   L_09c6
	jmp   L_0afc
L_09c6:
	mov    -0x30(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x24(%bp)
L_09cf:
	cmpw   $0x0,-0x24(%bp)
	jne   L_09d8
	jmp   L_0af1
L_09d8:
	mov    -0x24(%bp),%bx
	mov    0x2(%bx),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x176(%bp)
	lea    -0x172(%bp),%ax
	push   %ax
	pushw  -0x174(%bp)
	pushw  -0x176(%bp)
	call   L_1082
	add    $0x6,%sp
	push   %ax
	call   L_0f7e
	pop    %cx
	mov    %ax,-0x26(%bp)
	or     %ax,%ax
	je   L_0a22
	lea    -0x96(%bp),%ax
	push   %ax
	pushw  -0x176(%bp)
	lea    -0xfa(%bp),%ax
	push   %ax
	call   L_1082
	add    $0x6,%sp
	push   %ax
	call   L_0f7e
	pop    %cx
	mov    %ax,-0x28(%bp)
	or     %ax,%ax
L_0a22:
	jne   L_0a27
	jmp   L_0ae6
L_0a27:
	lea    -0xe(%bp),%ax
	push   %ax
	mov    0x6(%bp),%ax
	inc    %ax
	push   %ax
	pushw  -0x28(%bp)
	call   L_06c0
	add    $0x6,%sp
	add    %ax,-0x6(%bp)
	mov    -0x1a(%bp),%ax
	mov    -0x18(%bp),%dx
	cmp    -0xc(%bp),%dx
	jg   L_0a62
	jl   L_0a4e
	cmp    -0xe(%bp),%ax
	jae   L_0a62
L_0a4e:
	mov    -0x28(%bp),%bx
	pushw  0x2(%bx)
	pushw  -0x178(%bp)
	call   L_148f
	add    $0x4,%sp
	mov    %ax,-0x178(%bp)
L_0a62:
	/* TODO: data ref: cmpw   $0x0,0x2034 */
	cmpw   $0x0,D_0034
	je   L_0a7f
	pushw  -0xc(%bp)
	pushw  -0xe(%bp)
	mov    -0x28(%bp),%bx
	pushw  0x2(%bx)
	/* TODO: data ref: mov    $0x2270,%ax */
	mov    $D_0270,%ax
	push   %ax
	call   L_35bc
	add    $0x8,%sp
L_0a7f:
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	cmp    -0x14(%bp),%dx
	jl   L_0a9d
	jg   L_0a91
	cmp    -0x16(%bp),%ax
	jbe   L_0a9d
L_0a91:
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0a9d:
	lea    -0xfa(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x227e,%ax */
	mov    $D_027e,%ax
	push   %ax
	call   L_12b4
	add    $0x4,%sp
	lea    -0x96(%bp),%ax
	push   %ax
	call   L_103d
	pop    %cx
	push   %ax
	/* TODO: data ref: mov    $0x2280,%ax */
	mov    $D_0280,%ax
	push   %ax
	call   L_12b4
	add    $0x4,%sp
	mov    -0x26(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,-0x32(%bp)
L_0ac9:
	cmpw   $0x0,-0x32(%bp)
L_0acd:
	je   L_0b12
	mov    -0x32(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,-0x2a(%bp)
	or     %ax,%ax
	jne   L_0b12
	mov    -0x32(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x32(%bp)
	jmp   L_0ac9
L_0ae6:
	mov    -0x24(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x24(%bp)
	jmp   L_09cf
L_0af1:
	mov    -0x30(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x30(%bp)
	jmp   L_09bd
L_0afc:
	mov    -0x22(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x22(%bp)
	jmp   L_096a
L_0b07:
	mov    -0x2e(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x2e(%bp)
	jmp   L_0958
L_0b12:
	cmpw   $0x0,-0x6(%bp)
	jne   L_0b40
	mov    -0x1a(%bp),%ax
	mov    -0x18(%bp),%dx
	cmp    -0x14(%bp),%dx
	jl   L_0b45
	jg   L_0b2a
	cmp    -0x16(%bp),%ax
	jb   L_0b45
L_0b2a:
	cmpw   $0x0,-0x18(%bp)
	jne   L_0b40
	cmpw   $0x0,-0x1a(%bp)
	jne   L_0b40
	cmpw   $0x0,-0x14(%bp)
	jne   L_0b40
	cmpw   $0x0,-0x16(%bp)
L_0b40:
	je   L_0b45
	jmp   L_0c5d
L_0b45:
	cmpw   $0x0,-0x14(%bp)
	jl   L_0b5b
	jg   L_0b53
	cmpw   $0x0,-0x16(%bp)
	jbe   L_0b5b
L_0b53:
	mov    -0x16(%bp),%ax
	mov    -0x14(%bp),%dx
	jmp   L_0b5e
L_0b5b:
	call   L_157a
L_0b5e:
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
	pushw  0x2(%di)
	/* TODO: data ref: mov    $0x2282,%ax */
	mov    $D_0282,%ax
	push   %ax
	call   L_12b4
	add    $0x4,%sp
	pushw  -0x178(%bp)
	call   L_14c3
	pop    %cx
	push   %ax
	/* TODO: data ref: mov    $0x2284,%ax */
	mov    $D_0284,%ax
	push   %ax
	call   L_12b4
	add    $0x4,%sp
	cmpw   $0x0,-0x2c(%bp)
	je   L_0b96
	pushw  -0x2c(%bp)
	call   L_0d17
	pop    %cx
	add    %ax,-0x6(%bp)
	jmp   L_0ba6
L_0b96:
	cmpw   $0x0,-0x2a(%bp)
	je   L_0ba8
	pushw  -0x2a(%bp)
	call   L_0d17
	pop    %cx
	add    %ax,-0x6(%bp)
L_0ba6:
	jmp   L_0bf7
L_0ba8:
	mov    0x6(%di),%ax
	mov    $0x3,%cx
	sar    %cl,%ax
	test   $0x7,%ax
	jne   L_0bf7
	/* TODO: data ref: mov    $0x2286,%ax */
	mov    $D_0286,%ax
	push   %ax
	call   L_0f7e
	pop    %cx
	mov    %ax,-0x26(%bp)
	or     %ax,%ax
	je   L_0c03
	pushw  0x2(%di)
	/* TODO: data ref: mov    $0x228f,%ax */
	mov    $D_028f,%ax
	push   %ax
	call   L_12b4
	add    $0x4,%sp
	mov    -0x26(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,-0x32(%bp)
L_0bda:
	cmpw   $0x0,-0x32(%bp)
	je   L_0c29
	mov    -0x32(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,-0x2a(%bp)
	or     %ax,%ax
	je   L_0bf9
	pushw  -0x2a(%bp)
	call   L_0d17
	pop    %cx
	add    %ax,-0x6(%bp)
L_0bf7:
	jmp   L_0c29
L_0bf9:
	mov    -0x32(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x32(%bp)
	jmp   L_0bda
L_0c03:
	/* TODO: data ref: cmpw   $0x0,0x203c */
	cmpw   $0x0,D_003c
	je   L_0c1c
	pushw  0x2(%di)
	/* TODO: data ref: mov    $0x2291,%ax */
	mov    $D_0291,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
	incw   -0x6(%bp)
	jmp   L_0c29
L_0c1c:
	pushw  0x2(%di)
	/* TODO: data ref: mov    $0x22ac,%ax */
	mov    $D_02ac,%ax
	push   %ax
	call   L_140a
	add    $0x4,%sp
L_0c29:
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x22c7,%ax */
	mov    $D_02c7,%ax
	push   %ax
	call   L_12b4
	add    $0x4,%sp
	/* TODO: data ref: cmpw   $0x0,0x203a */
	cmpw   $0x0,D_003a
	jne   L_0c52
	pushw  0x2(%di)
	call   L_1530
	pop    %cx
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
	or     %dx,%dx
	jne   L_0c98
	or     %ax,%ax
	jne   L_0c98
L_0c52:
	call   L_157a
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
	jmp   L_0c98
L_0c5d:
	cmpw   $0x0,-0x6(%bp)
	je   L_0c78
	cmpw   $0x0,0x6(%bp)
	jne   L_0c78
	pushw  0x2(%di)
	/* TODO: data ref: mov    $0x22c9,%ax */
	mov    $D_02c9,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
	jmp   L_0c98
L_0c78:
	/* TODO: data ref: cmpw   $0x0,0x2042 */
	cmpw   $0x0,D_0042
	jne   L_0c98
	cmpw   $0x0,0x6(%bp)
	jne   L_0c98
	cmpw   $0x0,-0xa(%bp)
	jne   L_0c98
	pushw  0x2(%di)
	/* TODO: data ref: mov    $0x22ec,%ax */
	mov    $D_02ec,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
L_0c98:
	/* TODO: data ref: cmpw   $0x0,0x2042 */
	cmpw   $0x0,D_0042
	je   L_0cb8
	cmpw   $0x0,0x6(%bp)
	jne   L_0cb8
	/* TODO: data ref: cmpw   $0x0,0x2044 */
	cmpw   $0x0,D_0044
	jle   L_0cb1
	mov    $0xffff,%ax
	jmp   L_0cb3
L_0cb1:
	xor    %ax,%ax
L_0cb3:
	push   %ax
	call   _exit
	pop    %cx
L_0cb8:
	cmpw   $0x0,-0x6(%bp)
	je   L_0cc3
	mov    $0x3,%ax
	jmp   L_0cc6
L_0cc3:
	mov    $0x2,%ax
L_0cc6:
	mov    $0x0,%cl
	shl    %cl,%ax
	and    $0x7,%ax
	andw   $0xfff8,0x6(%di)
	or     %ax,0x6(%di)
	mov    -0x1e(%bp),%ax
	mov    -0x1c(%bp),%dx
	cmp    -0x18(%bp),%dx
	jl   L_0cf3
	jg   L_0ce7
	cmp    -0x1a(%bp),%ax
	jbe   L_0cf3
L_0ce7:
	mov    -0x1e(%bp),%ax
	mov    -0x1c(%bp),%dx
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
L_0cf3:
	mov    -0x1a(%bp),%ax
	mov    -0x18(%bp),%dx
	mov    %ax,0x8(%di)
	mov    %dx,0xa(%di)
	mov    0x8(%bp),%bx
	mov    -0x1a(%bp),%ax
	mov    -0x18(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
	mov    -0x6(%bp),%ax
L_0d10:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0d17:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x9ca,%sp
	/* TODO: data ref: incw   0x2044 */
	incw   D_0044
	/* TODO: data ref: cmpw   $0x0,0x2042 */
	cmpw   $0x0,D_0042
	je   L_0d30
	xor    %ax,%ax
	jmp   L_0dea
L_0d30:
	/* TODO: data ref: cmpw   $0x0,0x2040 */
	cmpw   $0x0,D_0040
	je   L_0d72
	/* TODO: data ref: mov    $0x2301,%ax */
	mov    $D_0301,%ax
	push   %ax
	call   L_13bd
	pop    %cx
	mov    %ax,%bx
	mov    0x4(%bx),%ax
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: cmpw   $0x0,0x2038 */
	cmpw   $0x0,D_0038
	jne   L_0d5b
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x2303,%ax */
	mov    $D_0303,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
L_0d5b:
	/* TODO: data ref: cmpw   $0x0,0x203a */
	cmpw   $0x0,D_003a
	jne   L_0d6f
	pushw  -0x6(%bp)
	mov    $0x1,%ax
	push   %ax
	call   L_1ef2
	add    $0x4,%sp
L_0d6f:
	jmp   L_0dfe
L_0d72:
	cmpw   $0x0,0x4(%bp)
	jne   L_0d7b
	jmp   L_0dfe
L_0d7b:
	lea    -0x9ce(%bp),%ax
	push   %ax
	mov    0x4(%bp),%bx
	pushw  0x2(%bx)
	call   L_116f
	add    $0x4,%sp
	/* TODO: data ref: mov    0x2046,%ax */
	mov    D_0046,%ax
	mov    %ax,-0x8(%bp)
	movw   $0x0,-0xa(%bp)
	lea    -0x9ce(%bp),%ax
	mov    %ax,-0x6(%bp)
L_0d9e:
	mov    -0x6(%bp),%bx
	cmpb   $0x2d,(%bx)
	je   L_0dae
	mov    -0x6(%bp),%bx
	cmpb   $0x40,(%bx)
	jne   L_0dc7
L_0dae:
	mov    -0x6(%bp),%bx
	cmpb   $0x2d,(%bx)
	jne   L_0dbd
	movw   $0x1,-0x8(%bp)
	jmp   L_0dc2
L_0dbd:
	movw   $0x1,-0xa(%bp)
L_0dc2:
	incw   -0x6(%bp)
	jmp   L_0d9e
L_0dc7:
	pushw  -0xa(%bp)
	pushw  -0x8(%bp)
	pushw  -0x6(%bp)
	call   L_0e07
	add    $0x6,%sp
	or     %ax,%ax
	je   L_0df3
	cmpw   $0x0,-0x8(%bp)
	jne   L_0df3
	/* TODO: data ref: cmpw   $0x0,0x203c */
	cmpw   $0x0,D_003c
	je   L_0dec
	mov    $0x1,%ax
L_0dea:
	jmp   L_0e00
L_0dec:
	xor    %ax,%ax
	push   %ax
	call   L_142e
	pop    %cx
L_0df3:
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,0x4(%bp)
	jmp   L_0d72
L_0dfe:
	xor    %ax,%ax
L_0e00:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e07:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	cmpb   $0x0,(%di)
	jne   L_0e19
	xor    %ax,%ax
	jmp   L_0e59
L_0e19:
	/* TODO: data ref: cmpw   $0x0,0x2038 */
	cmpw   $0x0,D_0038
	jne   L_0e50
	cmpw   $0x0,0x8(%bp)
	je   L_0e2d
	/* TODO: data ref: cmpw   $0x0,0x203a */
	cmpw   $0x0,D_003a
	je   L_0e50
L_0e2d:
	push   %di
	/* TODO: data ref: cmpw   $0x0,0x203a */
	cmpw   $0x0,D_003a
	je   L_0e3a
	/* TODO: data ref: mov    $0x2314,%ax */
	mov    $D_0314,%ax
	jmp   L_0e3d
L_0e3a:
	/* TODO: data ref: mov    0x204a,%ax */
	mov    D_004a,%ax
L_0e3d:
	push   %ax
	/* TODO: data ref: mov    $0x230e,%ax */
	mov    $D_030e,%ax
	push   %ax
	call   L_35bc
	add    $0x6,%sp
	mov    $D_2126,%ax
	push   %ax
	call   L_488e
	pop    %cx
L_0e50:
	/* TODO: data ref: cmpw   $0x0,0x203a */
	cmpw   $0x0,D_003a
	je   L_0e5b
	xor    %ax,%ax
L_0e59:
	jmp   L_0eb7
L_0e5b:
	pushw  0x6(%bp)
	push   %di
	call   L_1d3a
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	je   L_0eb5
	mov    %si,%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	or     %ax,%ax
	je   L_0e8a
	mov    %si,%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2315,%ax */
	mov    $D_0315,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
	jmp   L_0e95
L_0e8a:
	push   %si
	/* TODO: data ref: mov    $0x2327,%ax */
	mov    $D_0327,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
L_0e95:
	cmpw   $0x0,0x6(%bp)
	je   L_0ea5
	/* TODO: data ref: mov    $0x233f,%ax */
	mov    $D_033f,%ax
	push   %ax
	call   L_35bc
	pop    %cx
	jmp   L_0ead
L_0ea5:
	/* TODO: data ref: mov    $0x234b,%ax */
	mov    $D_034b,%ax
	push   %ax
	call   L_35bc
	pop    %cx
L_0ead:
	mov    $D_2126,%ax
	push   %ax
	call   L_488e
	pop    %cx
L_0eb5:
	mov    %si,%ax
L_0eb7:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ebb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0x2(%di),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%si
L_0ed2:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	jmp   L_0efc
L_0eda:
	pushw  (%di)
	mov    $0x1,%ax
	push   %ax
	pushw  -0x6(%bp)
	call   L_1597
	add    $0x6,%sp
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	je   L_0f11
	mov    -0x8(%bp),%ax
	mov    %ax,(%di)
	movw   $0x0,0x2(%di)
	jmp   L_0f11
L_0efc:
	or     %ax,%ax
	je   L_0f11
	cmp    $0x2a,%ax
	je   L_0eda
	cmp    $0x3f,%ax
	je   L_0eda
	cmp    $0x5b,%ax
	je   L_0eda
	jmp   L_0ed2
L_0f11:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f18:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	xor    %si,%si
	mov    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
L_0f26:
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0f3b
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	add    %ax,%si
	incw   -0x6(%bp)
	jmp   L_0f26
L_0f3b:
	mov    %si,%ax
	cwtd
	mov    $0x1fd,%cx
	idiv   %cx
	mov    %dx,%si
	mov    %si,%di
L_0f47:
	mov    %di,%bx
	shl    %bx
	/* TODO: data ref: cmpw   $0x0,0x234e(%bx) */
	cmpw   $0x0,D_034e(%bx)
	je   L_0f77
	mov    %di,%bx
	shl    %bx
	/* TODO: data ref: mov    0x234e(%bx),%bx */
	mov    D_034e(%bx),%bx
	pushw  0x2(%bx)
	pushw  0x4(%bp)
	call   L_4bc8
	add    $0x4,%sp
	or     %ax,%ax
	je   L_0f77
	lea    0x1(%di),%ax
	cwtd
	mov    $0x1fd,%cx
	idiv   %cx
	mov    %dx,%di
	jmp   L_0f47
L_0f77:
	mov    %di,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f7e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_0f18
	pop    %cx
	mov    %ax,%bx
	shl    %bx
	/* TODO: data ref: mov    0x234e(%bx),%ax */
	mov    D_034e(%bx),%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f96:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    0x2748,%ax */
	mov    D_0748,%ax
	/* TODO: data ref: incw   0x2748 */
	incw   D_0748
	cmp    $0x1fa,%ax
	jle   L_0faf
	mov    $D_0d28,%ax
	push   %ax
	call   L_142e
	pop    %cx
L_0faf:
	mov    $0xc,%ax
	push   %ax
	call   L_1145
	pop    %cx
	mov    %ax,%di
	mov    D_21cc,%ax
	mov    %ax,(%di)
	pushw  0x4(%bp)
	call   L_103d
	pop    %cx
	mov    %ax,0x2(%di)
	movw   $0x0,0x4(%di)
	andw   $0xfff8,0x6(%di)
	andw   $0xffc7,0x6(%di)
	movw   $0x0,0x8(%di)
	movw   $0x0,0xa(%di)
	mov    %di,D_21cc
	cmpw   $0x0,D_227c
	jne   L_1003
	mov    0x4(%bp),%bx
	cmpb   $0x2e,(%bx)
	jne   L_0fff
	pushw  0x4(%bp)
	call   L_1018
	pop    %cx
	or     %ax,%ax
	je   L_1003
L_0fff:
	mov    %di,D_227c
L_1003:
	pushw  0x4(%bp)
	call   L_0f18
	pop    %cx
	mov    %ax,%bx
	shl    %bx
	/* TODO: data ref: mov    %di,0x234e(%bx) */
	mov    %di,D_034e(%bx)
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1018:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_101d:
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1037
	mov    0x4(%bp),%bx
	cmpb   $0x2f,(%bx)
	jne   L_1032
	mov    $0x1,%ax
	jmp   L_1039
L_1032:
	incw   0x4(%bp)
	jmp   L_101d
L_1037:
	xor    %ax,%ax
L_1039:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_103d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    $0x1,%ax
	push   %ax
	push   %di
	call   L_38de
	pop    %cx
	inc    %ax
	push   %ax
	call   L_2978
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	mov    %ax,%si
	or     %ax,%ax
	jne   L_1069
	mov    $D_0d3c,%ax
	push   %ax
	call   L_142e
	pop    %cx
L_1069:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_1069
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1082:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    0x8(%bp),%ax
	mov    %ax,-0x6(%bp)
L_1096:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	je   L_10ac
	incw   -0x6(%bp)
	jmp   L_1096
L_10ac:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_10ac
	mov    0x8(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_10c8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
L_10de:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_10de
L_10e6:
	mov    %si,%bx
	inc    %si
	cmpb   $0x0,(%bx)
	jne   L_10e6
	mov    %di,%ax
	sub    -0x6(%bp),%ax
	mov    %si,%dx
	sub    -0x8(%bp),%dx
	cmp    %dx,%ax
	jge   L_1100
	xor    %ax,%ax
	jmp   L_113e
L_1100:
	cmp    -0x8(%bp),%si
	jbe   L_111b
	dec    %di
	mov    (%di),%al
	cbtw
	dec    %si
	mov    (%si),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_1100
	xor    %ax,%ax
	jmp   L_113e
L_111b:
	cmp    %di,-0x6(%bp)
	jae   L_1135
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	incw   0x8(%bp)
	jmp   L_111b
L_1135:
	mov    0x8(%bp),%bx
	movb   $0x0,(%bx)
	mov    $0x1,%ax
L_113e:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1145:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	push   %di
	mov    $0x1,%ax
	push   %ax
	call   L_2978
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	je   L_1163
	mov    %si,%ax
	jmp   L_116b
L_1163:
	mov    $D_0d4a,%ax
	push   %ax
	call   L_142e
	pop    %cx
L_116b:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_116f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6a,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	/* TODO: data ref: incw   0x274a */
	incw   D_074a
	/* TODO: data ref: cmpw   $0x64,0x274a */
	cmpw   $0x64,D_074a
	jle   L_1192
	mov    $D_0d58,%ax
	push   %ax
	call   L_142e
	pop    %cx
L_1192:
	or     %di,%di
	je   L_1199
L_1196:
	cmpb   $0x0,(%di)
L_1199:
	jne   L_119e
	jmp   L_12a4
L_119e:
	cmpb   $0x24,(%di)
	je   L_11b2
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_1196
L_11b2:
	inc    %di
	cmpb   $0x0,(%di)
	je   L_11bd
	cmpb   $0x24,(%di)
	jne   L_11cc
L_11bd:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
L_11ca:
	jmp   L_1196
L_11cc:
	lea    -0x6a(%bp),%ax
	mov    %ax,-0x6(%bp)
	cmpb   $0x28,(%di)
	je   L_11dc
	cmpb   $0x7b,(%di)
	jne   L_1251
L_11dc:
	cmpb   $0x28,(%di)
	jne   L_11e6
	mov    $0x29,%ax
	jmp   L_11e9
L_11e6:
	mov    $0x7d,%ax
L_11e9:
	mov    %ax,%cx
	mov    %cl,-0x6d(%bp)
	inc    %di
L_11ef:
	cmpb   $0x20,(%di)
	jne   L_11f7
	inc    %di
	jmp   L_11ef
L_11f7:
	cmpb   $0x20,(%di)
	je   L_1224
	mov    (%di),%al
	cbtw
	mov    -0x6d(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_1224
	cmpb   $0x0,(%di)
	je   L_1224
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	jmp   L_11f7
L_1224:
	mov    (%di),%al
	cbtw
	mov    -0x6d(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_123d
	cmpb   $0x0,(%di)
	je   L_123d
	inc    %di
	jmp   L_1224
L_123d:
	mov    (%di),%al
	cbtw
	mov    -0x6d(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_1261
	inc    %di
	jmp   L_1261
L_1251:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
L_1261:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	lea    -0x6a(%bp),%ax
	push   %ax
	call   L_13bd
	pop    %cx
	mov    %ax,-0x6c(%bp)
	mov    %ax,%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_127d
	jmp   L_11ca
L_127d:
	push   %si
	mov    -0x6c(%bp),%bx
	pushw  0x4(%bx)
	call   L_116f
	add    $0x4,%sp
	mov    %ax,%si
	mov    -0x6c(%bp),%bx
	mov    $0x1,%ax
	mov    %al,%cl
	shl    %cl,%ax
	and    $0x2,%ax
	andw   $0xfffd,0x6(%bx)
	or     %ax,0x6(%bx)
	jmp   L_11ca
L_12a4:
	movb   $0x0,(%si)
	/* TODO: data ref: decw   0x274a */
	decw   D_074a
	mov    %si,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_12b4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	pushw  0x4(%bp)
	call   L_13bd
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	mov    0x6(%bx),%ax
	test   $0x1,%ax
	je   L_12d1
	jmp   L_1351
L_12d1:
	mov    -0x6(%bp),%bx
	mov    0x6(%bp),%ax
	mov    %ax,0x4(%bx)
	mov    -0x6(%bp),%bx
	mov    D_227a,%ax
	mov    $0x0,%cl
	shl    %cl,%ax
	and    $0x1,%ax
	andw   $0xfffe,0x6(%bx)
	or     %ax,0x6(%bx)
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	sar    %ax
	test   $0x1,%ax
	je   L_133e
	mov    $D_0d74,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4bc8
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1351
	mov    $D_0d76,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4bc8
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1351
	mov    $D_0d78,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4bc8
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1351
	mov    $D_0d7a,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4bc8
	add    $0x4,%sp
	or     %ax,%ax
L_133e:
	je   L_1351
	pushw  0x4(%bp)
	mov    $D_0d7c,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x6,%sp
L_1351:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1356:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_135b:
	mov    0x4(%bp),%bx
	cmpb   $0x20,(%bx)
	jne   L_1368
	incw   0x4(%bp)
	jmp   L_135b
L_1368:
	mov    0x4(%bp),%di
L_136b:
	cmpb   $0x0,(%di)
	je   L_1373
	cmpb   $0x3a,(%di)
L_1373:
	je   L_13b7
	cmpb   $0x3d,(%di)
	jne   L_13b4
	mov    0x4(%bp),%si
L_137d:
	cmpb   $0x3d,(%si)
	je   L_138f
	cmpb   $0x20,(%si)
	je   L_138f
	cmpb   $0x9,(%si)
	je   L_138f
	inc    %si
	jmp   L_137d
L_138f:
	movb   $0x0,(%si)
	inc    %di
L_1393:
	cmpb   $0x20,(%di)
	je   L_139d
	cmpb   $0x9,(%di)
	jne   L_13a0
L_139d:
	inc    %di
	jmp   L_1393
L_13a0:
	push   %di
	call   L_103d
	pop    %cx
	push   %ax
	pushw  0x4(%bp)
	call   L_12b4
	add    $0x4,%sp
	mov    $0x1,%ax
	jmp   L_13b9
L_13b4:
	inc    %di
	jmp   L_136b
L_13b7:
	xor    %ax,%ax
L_13b9:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_13bd:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_21ec,%di
L_13c6:
	or     %di,%di
	je   L_13e2
	pushw  0x2(%di)
	pushw  0x4(%bp)
	call   L_4bc8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_13de
	mov    %di,%ax
	jmp   L_1406
L_13de:
	mov    (%di),%di
	jmp   L_13c6
L_13e2:
	mov    $0x8,%ax
	push   %ax
	call   L_1145
	pop    %cx
	mov    %ax,%di
	mov    D_21ec,%ax
	mov    %ax,(%di)
	mov    %di,D_21ec
	pushw  0x4(%bp)
	call   L_103d
	pop    %cx
	mov    %ax,0x2(%di)
	movw   $0x0,0x4(%di)
	mov    %di,%ax
L_1406:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_140a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x64,%sp
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	lea    -0x68(%bp),%ax
	push   %ax
	call   L_3a84
	add    $0x6,%sp
	push   %ax
	call   L_142e
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_142e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,0x4(%bp)
	je   L_144c
	pushw  0x4(%bp)
	mov    $D_0da2,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x6,%sp
	jmp   L_145a
L_144c:
	mov    $D_0db4,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x4,%sp
L_145a:
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1466:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x32,%sp
	pushw  0x4(%bp)
	pushw  D_2278
	mov    $D_0dbc,%ax
	push   %ax
	lea    -0x36(%bp),%ax
	push   %ax
	call   L_3a84
	add    $0x8,%sp
	push   %ax
	call   L_142e
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_148f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x4,%ax
	push   %ax
	call   L_1145
	pop    %cx
	mov    %ax,%di
	mov    0x6(%bp),%ax
	mov    %ax,0x2(%di)
	cmpw   $0x0,0x4(%bp)
	je   L_14bd
	mov    0x4(%bp),%si
L_14ad:
	cmpw   $0x0,(%si)
	je   L_14b6
	mov    (%si),%si
	jmp   L_14ad
L_14b6:
	mov    %di,(%si)
	mov    0x4(%bp),%ax
	jmp   L_14bf
L_14bd:
	mov    %di,%ax
L_14bf:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_14c3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,0x4(%bp)
	jne   L_14d5
	/* TODO: data ref: movb   $0x0,0x274c */
	movb   $0x0,D_074c
	jmp   L_152b
L_14d5:
	/* TODO: data ref: mov    $0x274c,%di */
	mov    $D_074c,%di
L_14d8:
	cmpw   $0x0,0x4(%bp)
	je   L_14fe
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%si
	push   %si
	call   L_38de
	pop    %cx
	add    %di,%ax
	cmp    $D_0d25,%ax
	jbe   L_1500
	mov    $D_0dc8,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x4,%sp
L_14fe:
	jmp   L_1524
L_1500:
	cmpb   $0x0,(%si)
	je   L_1514
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_1500
L_1514:
	mov    %di,%bx
	inc    %di
	movb   $0x20,(%bx)
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,0x4(%bp)
	jmp   L_14d8
L_1524:
	dec    %di
	movb   $0x0,(%di)
	/* TODO: data ref: mov    $0x274c,%ax */
	mov    $D_074c,%ax
L_152b:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1530:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x1e,%sp
	mov    0x4(%bp),%di
L_153b:
	cmpb   $0x0,(%di)
	je   L_1548
	cmpb   $0x28,(%di)
	je   L_1548
	inc    %di
	jmp   L_153b
L_1548:
	cmpb   $0x28,(%di)
	jne   L_1556
	pushw  0x4(%bp)
	call   L_19db
	pop    %cx
	jmp   L_1573
L_1556:
	lea    -0x22(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c76
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_156d
	xor    %ax,%ax
	mov    %ax,%dx
	jmp   L_1573
L_156d:
	mov    -0xc(%bp),%ax
	mov    -0xa(%bp),%dx
L_1573:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_157a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_3a66
	pop    %cx
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1597:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2e4,%sp
	mov    0x4(%bp),%di
	movw   $0x0,-0xe4(%bp)
	cmpw   $0x0,0x6(%bp)
	jne   L_15e1
	mov    D_21d0,%ax
	mov    %ax,-0xe8(%bp)
L_15b7:
	cmpw   $0x0,-0xe8(%bp)
	je   L_15e1
	mov    -0xe8(%bp),%bx
	pushw  0x2(%bx)
	push   %di
	call   L_4bc8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_15d5
	xor    %ax,%ax
	jmp   L_182b
L_15d5:
	mov    -0xe8(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0xe8(%bp)
	jmp   L_15b7
L_15e1:
	mov    $0x4,%ax
	push   %ax
	call   L_1145
	pop    %cx
	mov    %ax,-0xe8(%bp)
	mov    %ax,%bx
	mov    D_21d0,%ax
	mov    %ax,(%bx)
	mov    -0xe8(%bp),%ax
	mov    %ax,D_21d0
	push   %di
	call   L_103d
	pop    %cx
	mov    -0xe8(%bp),%bx
	mov    %ax,0x2(%bx)
	movw   $0x0,-0x10(%bp)
	mov    %di,-0x14(%bp)
L_160f:
	mov    -0x14(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_162a
	mov    -0x14(%bp),%bx
	cmpb   $0x2f,(%bx)
	jne   L_1625
	mov    -0x14(%bp),%ax
	mov    %ax,-0x10(%bp)
L_1625:
	incw   -0x14(%bp)
	jmp   L_160f
L_162a:
	cmpw   $0x0,-0x10(%bp)
	jne   L_163f
	movw   $D_1348,-0xc(%bp)
	movw   $D_134a,-0xe(%bp)
	mov    %di,-0x12(%bp)
	jmp   L_1663
L_163f:
	mov    %di,-0xc(%bp)
	mov    -0x10(%bp),%bx
	movb   $0x0,(%bx)
	lea    -0x78(%bp),%ax
	push   %ax
	mov    $D_134b,%ax
	push   %ax
	pushw  -0xc(%bp)
	call   L_1082
	add    $0x6,%sp
	mov    %ax,-0xe(%bp)
	mov    -0x10(%bp),%ax
	inc    %ax
	mov    %ax,-0x12(%bp)
L_1663:
	movw   $0x0,-0x6(%bp)
	mov    D_21ce,%ax
	mov    %ax,-0xe6(%bp)
L_166f:
	cmpw   $0x0,-0xe6(%bp)
	je   L_16b4
	mov    -0xe6(%bp),%bx
	pushw  0x4(%bx)
	pushw  -0xc(%bp)
	call   L_4bc8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_16a8
	mov    -0xe6(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x6(%bp)
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	call   L_3408
	add    $0x8,%sp
	jmp   L_16b4
L_16a8:
	mov    -0xe6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0xe6(%bp)
	jmp   L_166f
L_16b4:
	cmpw   $0x0,-0x6(%bp)
	jne   L_16fc
	mov    $D_134d,%ax
	push   %ax
	pushw  -0xc(%bp)
	call   L_32f4
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	mov    $0x6,%ax
	push   %ax
	call   L_1145
	pop    %cx
	mov    %ax,-0xe6(%bp)
	mov    %ax,%bx
	mov    D_21ce,%ax
	mov    %ax,(%bx)
	mov    -0xe6(%bp),%ax
	mov    %ax,D_21ce
	mov    -0xe6(%bp),%bx
	mov    -0x6(%bp),%ax
	mov    %ax,0x2(%bx)
	pushw  -0xc(%bp)
	call   L_103d
	pop    %cx
	mov    -0xe6(%bp),%bx
	mov    %ax,0x4(%bx)
L_16fc:
	cmpw   $0x0,-0x6(%bp)
	jne   L_171e
	pushw  -0xc(%bp)
	mov    $D_134f,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x6,%sp
	mov    $D_135e,%ax
	push   %ax
	call   L_142e
	pop    %cx
	jmp   L_181b
L_171e:
	pushw  -0x6(%bp)
	mov    $0x20,%ax
	push   %ax
	mov    $0x10,%ax
	push   %ax
	lea    -0x2e8(%bp),%ax
	push   %ax
	call   L_35e6
	add    $0x8,%sp
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0x8(%bp)
L_173c:
	mov    -0x8(%bp),%ax
	cmp    -0xa(%bp),%ax
	jl   L_1747
	jmp   L_a812
L_1747:
	lea    -0x2e8(%bp),%ax
	mov    -0x8(%bp),%dx
	mov    $0x4,%cx
	shl    %cl,%dx
	add    %dx,%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	je   L_17aa
	lea    -0x2e8(%bp),%ax
	mov    -0x8(%bp),%dx
	mov    $0x4,%cx
	shl    %cl,%dx
	add    %dx,%ax
	inc    %ax
	inc    %ax
	mov    %ax,-0xde(%bp)
	movw   $D_0e62,-0xe0(%bp)
L_1776:
	mov    -0xe0(%bp),%ax
	cmp    D_0e72,%ax
	jae   L_179b
	mov    -0xde(%bp),%bx
	incw   -0xde(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0xe0(%bp),%bx
	incw   -0xe0(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_1776
L_179b:
	pushw  -0x12(%bp)
	mov    $D_0e62,%ax
	push   %ax
	call   L_1832
	add    $0x4,%sp
	or     %ax,%ax
L_17aa:
	je   L_17e5
	lea    -0xdc(%bp),%ax
	push   %ax
	mov    $D_0e62,%ax
	push   %ax
	pushw  -0xe(%bp)
	call   L_1082
	add    $0x6,%sp
	lea    -0xdc(%bp),%ax
	push   %ax
	call   L_0f7e
	pop    %cx
	mov    %ax,-0xe2(%bp)
	or     %ax,%ax
	jne   L_17e1
	lea    -0xdc(%bp),%ax
	push   %ax
	call   L_103d
	pop    %cx
	push   %ax
	call   L_0f96
	pop    %cx
	mov    %ax,-0xe2(%bp)
L_17e1:
	cmpw   $0x0,0x6(%bp)
L_17e5:
	je   L_180c
	mov    $0x4,%ax
	push   %ax
	call   L_1145
	pop    %cx
	mov    %ax,-0xe4(%bp)
	mov    %ax,%bx
	mov    0x8(%bp),%ax
	mov    %ax,(%bx)
	mov    -0xe4(%bp),%bx
	mov    -0xe2(%bp),%ax
	mov    %ax,0x2(%bx)
	mov    -0xe4(%bp),%ax
	mov    %ax,0x8(%bp)
L_180c:
	incw   -0x8(%bp)
	jmp   L_173c
	cmpw   $0x20,-0xa(%bp)
	jne   L_181b
	jmp   L_171e
L_181b:
	cmpw   $0x0,-0x10(%bp)
	je   L_1827
	mov    -0x10(%bp),%bx
	movb   $0x2f,(%bx)
L_1827:
	mov    -0xe4(%bp),%ax
L_182b:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1832:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	movw   $0x7fff,-0xa(%bp)
	mov    0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x8(%bp)
	jmp   L_190a
L_1853:
	movw   $0x0,-0x6(%bp)
L_1858:
	incw   0x6(%bp)
	mov    0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%di
	or     %ax,%ax
	je   L_18cc
	mov    %di,%ax
	jmp   L_18b6
L_186b:
	cmpw   $0x0,-0x6(%bp)
	je   L_1885
	incw   0x6(%bp)
	pushw  0x6(%bp)
	incw   0x4(%bp)
	pushw  0x4(%bp)
	call   L_1832
	add    $0x4,%sp
	jmp   L_1887
L_1885:
	xor    %ax,%ax
L_1887:
	jmp   L_18ce
L_1889:
	cmp    %si,-0xa(%bp)
	jg   L_1893
	mov    $0x1,%ax
	jmp   L_1895
L_1893:
	xor    %ax,%ax
L_1895:
	mov    0x6(%bp),%bx
	mov    0x1(%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	mov    %dx,%di
	cmp    %dx,%si
	jg   L_18ad
	mov    $0x1,%dx
	jmp   L_18af
L_18ad:
	xor    %dx,%dx
L_18af:
	and    %dx,%ax
	or     %ax,-0x6(%bp)
	jmp   L_18c0
L_18b6:
	cmp    $0x2d,%ax
	je   L_1889
	cmp    $0x5d,%ax
	je   L_186b
L_18c0:
	mov    %di,-0xa(%bp)
	cmp    %di,%si
	jne   L_1858
	incw   -0x6(%bp)
	jmp   L_1858
L_18cc:
	xor    %ax,%ax
L_18ce:
	jmp   L_1908
L_18d0:
	or     %si,%si
	je   L_18e8
	incw   0x6(%bp)
	pushw  0x6(%bp)
	incw   0x4(%bp)
	pushw  0x4(%bp)
	call   L_1832
	add    $0x4,%sp
	jmp   L_1927
L_18e8:
	xor    %ax,%ax
	jmp   L_1927
L_18ec:
	incw   0x6(%bp)
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_192e
	add    $0x4,%sp
	jmp   L_1927
L_18fd:
	or     %si,%si
	jne   L_1906
	mov    $0x1,%ax
	jmp   L_1927
L_1906:
	xor    %ax,%ax
L_1908:
	jmp   L_1927
L_190a:
	or     %ax,%ax
	je   L_18fd
	cmp    $0x2a,%ax
	je   L_18ec
	cmp    $0x3f,%ax
L_1916:
	je   L_18d0
	cmp    $0x5b,%ax
	jne   L_1920
	jmp   L_1853
L_1920:
	cmp    %si,-0x8(%bp)
	je   L_1916
	xor    %ax,%ax
L_1927:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_192e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	jne   L_1940
	mov    $0x1,%ax
	jmp   L_1963
L_1940:
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1961
	pushw  0x6(%bp)
	mov    0x4(%bp),%ax
	incw   0x4(%bp)
	push   %ax
	call   L_1832
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1940
	mov    $0x1,%ax
	jmp   L_1963
L_1961:
	xor    %ax,%ax
L_1963:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1967:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xa,%sp
	cmpw   $0x0,0x4(%bp)
	je   L_19a1
	cmpw   $0x0,D_0e74
	je   L_19a1
	call   L_32b8
	push   %ax
	call   L_30a4
	pop    %cx
	mov    %ax,-0xe(%bp)
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_3a66
	pop    %cx
	mov    $D_136a,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_32f4
	add    $0x4,%sp
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
L_19a1:
	je   L_19d4
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_29ce
	pop    %cx
	mov    %ax,-0xa(%bp)
	mov    %ax,%bx
	movb   $0x0,0x10(%bx)
	mov    -0xa(%bp),%ax
	add    $0x4,%ax
	push   %ax
	mov    -0xe(%bp),%bx
	pushw  (%bx)
	mov    $D_136c,%ax
	push   %ax
	pushw  -0xc(%bp)
	call   L_3078
	add    $0x8,%sp
	pushw  -0xc(%bp)
	call   L_48fd
	pop    %cx
L_19d4:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_19db:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x1e,%sp
	mov    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
L_19e9:
	mov    -0x6(%bp),%bx
	cmpb   $0x28,(%bx)
	je   L_19f6
	incw   -0x6(%bp)
	jmp   L_19e9
L_19f6:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	pushw  0x4(%bp)
	call   L_1b54
	pop    %cx
	mov    -0x6(%bp),%bx
	movb   $0x28,(%bx)
	incw   -0x6(%bp)
	mov    -0x6(%bp),%bx
	cmpb   $0x28,(%bx)
	jne   L_1a23
	movw   $0x1,-0x22(%bp)
	movw   $0x8,-0x1e(%bp)
	incw   -0x6(%bp)
	jmp   L_1a2d
L_1a23:
	movw   $0x0,-0x22(%bp)
	movw   $0xe,-0x1e(%bp)
L_1a2d:
	lea    -0x19(%bp),%ax
	add    -0x1e(%bp),%ax
	mov    %ax,-0xa(%bp)
	lea    -0x19(%bp),%ax
	mov    %ax,-0x8(%bp)
L_1a3c:
	mov    -0x8(%bp),%ax
	cmp    -0xa(%bp),%ax
	jae   L_1a69
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1a69
	mov    -0x6(%bp),%bx
	cmpb   $0x29,(%bx)
	je   L_1a69
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	incw   -0x8(%bp)
	jmp   L_1a3c
L_1a69:
	mov    -0x8(%bp),%ax
	cmp    -0xa(%bp),%ax
	jae   L_1a7c
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_1a69
L_1a7c:
	call   L_1bf5
	or     %ax,%ax
	jne   L_1a86
	jmp   L_1b35
L_1a86:
	cmpw   $0x0,-0x22(%bp)
	jne   L_1a8f
	jmp   L_1b12
L_1a8f:
	call   L_1c6c
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  D_0ec4
	pushw  D_0ec2
	call   L_4174
	add    $0x8,%sp
	mov    %ax,-0x20(%bp)
	movw   $0x0,-0x1c(%bp)
L_1aaf:
	mov    -0x1c(%bp),%ax
	cmp    -0x20(%bp),%ax
	jge   L_1a7c
	pushw  D_2288
	mov    $0x1,%ax
	push   %ax
	mov    $0xa,%ax
	push   %ax
	mov    $D_0ed2,%ax
	push   %ax
	call   L_35e6
	add    $0x8,%sp
	mov    D_0ed4,%al
	cbtw
	test   $0x1,%ax
	je   L_1b0d
	mov    D_0ed4,%al
	cbtw
	test   $0xfffe,%ax
	jne   L_1aed
	cmpw   $0x0,D_0eda
	jne   L_1aeb
	cmpw   $0x0,D_0ed8
L_1aeb:
	je   L_1b0d
L_1aed:
	pushw  -0x1e(%bp)
	lea    -0x19(%bp),%ax
	push   %ax
	pushw  D_0ed2
	call   L_1cf7
	add    $0x6,%sp
	or     %ax,%ax
	je   L_1b0d
	call   L_1b43
	mov    $D_0e86,%ax
	mov    $D_0e76,%dx
	jmp   L_1b3c
L_1b0d:
	incw   -0x1c(%bp)
	jmp   L_1aaf
L_1b12:
	pushw  -0x1e(%bp)
	lea    -0x19(%bp),%ax
	push   %ax
	mov    $D_0e76,%ax
	push   %ax
	call   L_1cf7
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_1b2a
	jmp   L_1a7c
L_1b2a:
	call   L_1b43
	mov    $D_0e86,%ax
	mov    $D_0e76,%dx
	jmp   L_1b3c
L_1b35:
	call   L_1b43
	xor    %ax,%ax
	mov    %ax,%dx
L_1b3c:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b43:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  D_2288
	call   L_48fd
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b54:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	mov    0x4(%bp),%di
	lea    -0x30(%bp),%ax
	push   %ax
	push   %di
	call   L_4c76
	add    $0x4,%sp
	mov    -0x22(%bp),%ax
	mov    -0x20(%bp),%dx
	mov    %ax,D_2284
	mov    %dx,D_2286
	mov    $D_1379,%ax
	push   %ax
	push   %di
	call   L_32f4
	add    $0x4,%sp
	mov    %ax,D_2288
	cmp    $0x0,%ax
	jne   L_1b96
	push   %di
	mov    $D_137b,%ax
	push   %ax
	call   L_140a
	add    $0x4,%sp
L_1b96:
	pushw  D_2288
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	lea    -0x12(%bp),%ax
	push   %ax
	call   L_35e6
	add    $0x8,%sp
	cmpw   $D_138a,-0x12(%bp)
	je   L_1bbe
	push   %di
	mov    $D_1393,%ax
	push   %ax
	call   L_140a
	add    $0x4,%sp
L_1bbe:
	movw   $0x0,D_2280
	movw   $0x0,D_2282
	mov    $0xffc6,%ax
	push   %ax
	mov    $D_13a8,%ax
	push   %ax
	lea    -0xf(%bp),%ax
	push   %ax
	call   L_3a84
	add    $0x6,%sp
	mov    $0xa,%ax
	push   %ax
	lea    -0xf(%bp),%ax
	push   %ax
	mov    $D_0ea6,%ax
	push   %ax
	call   L_4c8c
	add    $0x6,%sp
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1bf5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	addw   $0x3c,D_2280
	adcw   $0x0,D_2282
	mov    $D_0ea6,%ax
	push   %ax
	call   L_28d4
	pop    %cx
	add    $0x1,%ax
	adc    $0x0,%dx
	and    $0xfffe,%ax
	and    $0xffff,%dx
	add    %ax,D_2280
	adc    %dx,D_2282
	mov    D_2280,%ax
	mov    D_2282,%dx
	cmp    D_2286,%dx
	jl   L_1c3a
	jg   L_1c36
	cmp    D_2284,%ax
	jb   L_1c3a
L_1c36:
	xor    %ax,%ax
	jmp   L_1c68
L_1c3a:
	xor    %ax,%ax
	push   %ax
	pushw  D_2282
	pushw  D_2280
	pushw  D_2288
	call   L_3408
	add    $0x8,%sp
	pushw  D_2288
	mov    $0x1,%ax
	push   %ax
	mov    $0x3c,%ax
	push   %ax
	mov    $D_0e76,%ax
	push   %ax
	call   L_35e6
	add    $0x8,%sp
	mov    $0x1,%ax
L_1c68:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1c6c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	pushw  D_2288
	mov    $0x1,%ax
	push   %ax
	mov    $0x20,%ax
	push   %ax
	mov    $D_0eb2,%ax
	push   %ax
	call   L_35e6
	add    $0x8,%sp
	cmpw   $0x107,D_0eb2
	je   L_1ca8
	cmpw   $0x109,D_0eb2
	je   L_1ca8
	mov    $D_0e76,%ax
	push   %ax
	mov    $D_13af,%ax
	push   %ax
	call   L_140a
	add    $0x4,%sp
L_1ca8:
	mov    D_0eb6,%ax
	mov    D_0eb8,%dx
	add    D_0eba,%ax
	adc    D_0ebc,%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,D_0ecc
	jne   L_1cc9
	cmpw   $0x0,D_0eca
L_1cc9:
	je   L_1cdc
	mov    $0x2,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_40c8
	add    $0x6,%sp
L_1cdc:
	mov    $0x1,%ax
	push   %ax
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	pushw  D_2288
	call   L_3408
	add    $0x8,%sp
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1cf7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	movw   $0x0,-0x6(%bp)
L_1d0a:
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_1d31
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_1d2c
	xor    %ax,%ax
	jmp   L_1d34
L_1d2c:
	incw   -0x6(%bp)
	jmp   L_1d0a
L_1d31:
	mov    $0x1,%ax
L_1d34:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1d3a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	push   %di
	call   L_1d67
	pop    %cx
	or     %ax,%ax
	je   L_1d5a
	pushw  0x6(%bp)
	push   %di
	call   L_1d94
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_1d61
L_1d5a:
	push   %di
	call   L_1e44
	pop    %cx
	mov    %ax,%si
L_1d61:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d67:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
L_1d71:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	mov    %cl,%al
	cbtw
	mov    %ax,%bx
	mov    D_21f8(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_1d71
	mov    -0x5(%bp),%al
	cbtw
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d94:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_3918
	/* TODO: data ref: mov    %ax,0x2032 */
	mov    %ax,D_0032
	or     %ax,%ax
	jne   L_1dd8
	xor    %ax,%ax
	push   %ax
	call   L_0462
	pop    %cx
	call   L_1e22
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_1dbe
	mov    $D_13d5,%ax
	jmp   L_1dc1
L_1dbe:
	mov    $D_13d8,%ax
L_1dc1:
	push   %ax
	mov    $D_13d2,%ax
	push   %ax
	mov    $D_13ca,%ax
	push   %ax
	call   L_38fa
	add    $0xa,%sp
	mov    $D_13dc,%ax
	push   %ax
	call   L_142e
	pop    %cx
L_1dd8:
	call   L_1ddf
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ddf:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    $0x1,%ax
	push   %ax
	call   L_0462
	pop    %cx
L_1ded:
	lea    -0x6(%bp),%ax
	push   %ax
	call   L_4d16
	pop    %cx
	mov    %ax,%di
	/* TODO: data ref: cmp    0x2032,%ax */
	cmp    D_0032,%ax
	je   L_1e0c
	cmp    $0xffff,%di
	jne   L_1ded
	mov    $D_13f0,%ax
	push   %ax
	call   L_142e
	pop    %cx
	jmp   L_1ded
L_1e0c:
	/* TODO: data ref: movw   $0x0,0x2032 */
	movw   $0x0,D_0032
	mov    $0x386,%ax
	push   %ax
	call   L_0462
	pop    %cx
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e22:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_21ce,%di
L_1e2b:
	or     %di,%di
	je   L_1e40
	cmpw   $0x0,0x2(%di)
	je   L_1e3c
	pushw  0x2(%di)
	call   L_48fd
	pop    %cx
L_1e3c:
	mov    (%di),%di
	jmp   L_1e2b
L_1e40:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e44:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x192,%sp
	mov    0x4(%bp),%di
L_1e51:
	cmpb   $0x20,(%di)
	je   L_1e5b
	cmpb   $0x9,(%di)
	jne   L_1e5e
L_1e5b:
	inc    %di
	jmp   L_1e51
L_1e5e:
	cmpb   $0x0,(%di)
	jne   L_1e69
	mov    $0xffff,%ax
	jmp   L_1eeb
L_1e69:
	lea    -0x194(%bp),%ax
	mov    %ax,-0x196(%bp)
	mov    %di,%si
L_1e73:
	cmpb   $0x0,(%si)
	je   L_1ead
	mov    -0x196(%bp),%bx
	mov    %si,(%bx)
	addw   $0x2,-0x196(%bp)
L_1e83:
	cmpb   $0x20,(%si)
	je   L_1e95
	cmpb   $0x9,(%si)
	je   L_1e95
	cmpb   $0x0,(%si)
	je   L_1e95
	inc    %si
	jmp   L_1e83
L_1e95:
	cmpb   $0x0,(%si)
	je   L_1e73
	mov    %si,%bx
	inc    %si
	movb   $0x0,(%bx)
L_1ea0:
	cmpb   $0x20,(%si)
	je   L_1eaa
	cmpb   $0x9,(%si)
	jne   L_1e73
L_1eaa:
	inc    %si
	jmp   L_1ea0
L_1ead:
	mov    -0x196(%bp),%bx
	movw   $0x0,(%bx)
	call   L_3918
	/* TODO: data ref: mov    %ax,0x2032 */
	mov    %ax,D_0032
	or     %ax,%ax
	jne   L_1ee8
	xor    %ax,%ax
	push   %ax
	call   L_0462
	pop    %cx
	call   L_1e22
	mov    $0x386,%ax
	push   %ax
	call   L_0462
	pop    %cx
	lea    -0x194(%bp),%ax
	push   %ax
	push   %di
	call   L_2e38
	add    $0x4,%sp
	push   %di
	mov    $D_13fe,%ax
	push   %ax
	call   L_140a
	add    $0x4,%sp
L_1ee8:
	call   L_1ddf
L_1eeb:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ef2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x22,%sp
	lea    -0x22(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_4c76
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_1f27
	cmpw   $0x0,0x4(%bp)
	je   L_1f14
	jmp   L_1fb6
L_1f14:
	pushw  0x6(%bp)
	mov    $D_140d,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x6,%sp
	jmp   L_1fa1
L_1f27:
	cmpw   $0x0,-0x12(%bp)
	jne   L_1f36
	cmpw   $0x0,-0x14(%bp)
	jne   L_1f36
	jmp   L_1fb6
L_1f36:
	mov    $0x2,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_4c46
	add    $0x4,%sp
	mov    %ax,-0x26(%bp)
	or     %ax,%ax
	jl   L_1f67
	mov    $0x1,%ax
	push   %ax
	lea    -0x23(%bp),%ax
	push   %ax
	pushw  -0x26(%bp)
	call   L_4c5c
	add    $0x6,%sp
	cmp    $0x1,%ax
	jge   L_1f69
	pushw  -0x26(%bp)
	call   L_4c0a
	pop    %cx
L_1f67:
	jmp   L_1fa3
L_1f69:
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  -0x26(%bp)
	call   L_3a48
	add    $0x8,%sp
	mov    $0x1,%ax
	push   %ax
	lea    -0x23(%bp),%ax
	push   %ax
	pushw  -0x26(%bp)
	call   L_4d32
	add    $0x6,%sp
	cmp    $0x1,%ax
	jge   L_1f9a
	pushw  -0x26(%bp)
	call   L_4c0a
	pop    %cx
	jmp   L_1fa3
L_1f9a:
	pushw  -0x26(%bp)
	call   L_4c0a
	pop    %cx
L_1fa1:
	jmp   L_1fd1
L_1fa3:
	pushw  0x6(%bp)
	mov    $D_142d,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x6,%sp
	jmp   L_1fd1
L_1fb6:
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_33f2
	add    $0x4,%sp
	mov    %ax,-0x26(%bp)
	or     %ax,%ax
	jl   L_1fa3
	pushw  -0x26(%bp)
	call   L_4c0a
	pop    %cx
L_1fd1:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1fd8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5dc,%sp
	cmpw   $0x0,D_21ee
	jne   L_1fed
	call   L_2139
	jmp   L_2036
L_1fed:
	mov    D_21ee,%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	mov    D_1fcf(%bx),%al
	cbtw
	test   $0x8,%ax
	je   L_2006
	incw   D_21ee
	jmp   L_1fed
L_2006:
	mov    D_21ee,%bx
	cmpb   $0x0,(%bx)
	jne   L_2014
	call   L_2139
	jmp   L_205b
L_2014:
	mov    D_21ee,%bx
	cmpb   $0x3a,(%bx)
	jne   L_2038
	incw   D_21ee
	mov    D_21ee,%bx
	cmpb   $0x3a,(%bx)
	jne   L_2033
	incw   D_21ee
	mov    $0x106,%ax
	jmp   L_205b
L_2033:
	mov    $0x105,%ax
L_2036:
	jmp   L_205b
L_2038:
	mov    D_21ee,%bx
	cmpb   $0x3e,(%bx)
	jne   L_204a
	incw   D_21ee
	mov    $0x107,%ax
	jmp   L_205b
L_204a:
	mov    D_21ee,%bx
	cmpb   $0x3b,(%bx)
	jne   L_205d
	pushw  D_21ee
	call   L_20e5
	pop    %cx
L_205b:
	jmp   L_20b1
L_205d:
	mov    D_21ee,%di
	lea    -0x5e0(%bp),%ax
	mov    %ax,%si
L_2067:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	mov    D_21f8(%bx),%al
	cbtw
	test   $0x2,%ax
	jne   L_2085
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_2067
L_2085:
	cmp    D_21ee,%di
	je   L_20b3
	movb   $0x0,(%si)
	lea    -0x5e0(%bp),%ax
	push   %ax
	call   L_0f7e
	pop    %cx
	mov    %ax,D_2290
	or     %ax,%ax
	jne   L_20aa
	lea    -0x5e0(%bp),%ax
	push   %ax
	call   L_0f96
	pop    %cx
	mov    %ax,D_2290
L_20aa:
	mov    %di,D_21ee
	mov    $0x101,%ax
L_20b1:
	jmp   L_20de
L_20b3:
	pushw  D_2278
	mov    D_21ee,%bx
	mov    (%bx),%al
	cbtw
	push   %ax
	mov    D_21ee,%bx
	mov    (%bx),%al
	cbtw
	push   %ax
	mov    $D_1ba0,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0xa,%sp
	xor    %ax,%ax
	push   %ax
	call   L_142e
	pop    %cx
	xor    %ax,%ax
L_20de:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_20e5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%ax
	inc    %ax
	mov    %ax,%di
L_20f1:
	cmpb   $0x20,(%di)
	je   L_20fb
	cmpb   $0x9,(%di)
	jne   L_20fe
L_20fb:
	inc    %di
	jmp   L_20f1
L_20fe:
	mov    $0x4,%ax
	push   %ax
	call   L_1145
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	movw   $0x0,(%bx)
	cmpw   $0x0,D_21f0
	jne   L_211a
	mov    %di,%ax
	jmp   L_211f
L_211a:
	push   %di
	call   L_103d
	pop    %cx
L_211f:
	mov    -0x6(%bp),%bx
	mov    %ax,0x2(%bx)
	mov    -0x6(%bp),%ax
	mov    %ax,D_2290
	movw   $0x0,D_21ee
	mov    $0x102,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2139:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5e6,%sp
L_2142:
	movw   $0x0,-0x5e8(%bp)
	movw   $0x0,D_21ee
	cmpw   $0x0,D_21f0
	jne   L_2172
	/* TODO: data ref: mov    0x2092,%bx */
	mov    D_0092,%bx
	/* TODO: data ref: addw   $0x2,0x2092 */
	addw   $0x2,D_0092
	mov    (%bx),%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_216c
	xor    %ax,%ax
	jmp   L_223a
L_216c:
	incw   D_2278
	jmp   L_21df
L_2172:
	movw   $D_1496,-0x6(%bp)
	mov    $D_1496,%di
L_217a:
	cmp    D_1a72,%di
	jb   L_2183
	jmp   L_226f
L_2183:
	mov    D_21f0,%bx
	decw   0x2(%bx)
	jl   L_219f
	mov    D_21f0,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_21a7
L_219f:
	pushw  D_21f0
	call   L_36f2
	pop    %cx
L_21a7:
	mov    %ax,-0x5ea(%bp)
	jmp   L_223c
L_21ae:
	cmp    $D_1496,%di
	jne   L_21c9
L_21b4:
	movw   $0x1,-0x5e8(%bp)
	jmp   L_21c9
L_21bc:
	cmpw   $0x0,-0x5e8(%bp)
	jne   L_21c9
	movw   $0x0,-0x5ea(%bp)
L_21c9:
	jmp   L_2261
L_21cc:
	incw   D_2278
	cmp    $D_1496,%di
	je   L_21dc
	cmpb   $0x5c,-0x1(%di)
	je   L_21e2
L_21dc:
	movb   $0x0,(%di)
L_21df:
	jmp   L_2277
L_21e2:
	movb   $0x20,-0x1(%di)
L_21e6:
	mov    D_21f0,%bx
	decw   0x2(%bx)
	jl   L_2202
	mov    D_21f0,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_220a
L_2202:
	pushw  D_21f0
	call   L_36f2
	pop    %cx
L_220a:
	mov    %ax,-0x5ea(%bp)
	cmp    $0x9,%ax
	je   L_2221
	cmpw   $0x20,-0x5ea(%bp)
	je   L_2221
	cmpw   $0xa,-0x5ea(%bp)
	jne   L_222e
L_2221:
	cmpw   $0xa,-0x5ea(%bp)
	jne   L_21e6
	incw   D_2278
	jmp   L_21e6
L_222e:
	cmpw   $0xffff,-0x5ea(%bp)
	jne   L_2261
L_2235:
	movb   $0x0,(%di)
	xor    %ax,%ax
L_223a:
	jmp   L_228f
L_223c:
	cmp    $0xffff,%ax
	je   L_2235
	cmp    $0x9,%ax
	jne   L_2249
	jmp   L_21ae
L_2249:
	cmp    $0xa,%ax
	jne   L_2251
	jmp   L_21cc
L_2251:
	cmp    $0x23,%ax
	jne   L_2259
	jmp   L_21bc
L_2259:
	cmp    $0x3b,%ax
	jne   L_2261
	jmp   L_21b4
L_2261:
	mov    %di,%bx
	inc    %di
	mov    -0x5ea(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_217a
L_226f:
	mov    $D_1bc5,%ax
	push   %ax
	call   L_142e
	pop    %cx
L_2277:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5e3(%bp)
	cmp    $0x9,%cl
	jne   L_2291
	pushw  -0x6(%bp)
	call   L_20e5
	pop    %cx
L_228f:
	jmp   L_22e3
L_2291:
	mov    -0x5e3(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_1fcf(%bx),%al
	cbtw
	test   $0x3,%ax
	jne   L_22c1
	mov    -0x5e3(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_1fcf(%bx),%al
	cbtw
	test   $0x4,%ax
	jne   L_22c1
	cmpb   $0x20,-0x5e3(%bp)
	je   L_22c1
	cmpb   $0x2e,-0x5e3(%bp)
	jne   L_22e6
L_22c1:
	mov    -0x6(%bp),%ax
	inc    %ax
	mov    %ax,%di
L_22c7:
	cmpb   $0x0,(%di)
	je   L_22e6
	cmpb   $0x3a,(%di)
	je   L_22e6
	mov    %di,%bx
	inc    %di
	cmpb   $0x3d,(%bx)
	jne   L_22c7
	pushw  -0x6(%bp)
	call   L_1356
	pop    %cx
	mov    $0x104,%ax
L_22e3:
	jmp   L_2378
L_22e6:
	mov    $D_1496,%si
L_22e9:
	cmpb   $0x0,(%si)
	je   L_22f6
	cmpb   $0x3b,(%si)
	je   L_22f6
	inc    %si
	jmp   L_22e9
L_22f6:
	mov    %si,-0x5e6(%bp)
	mov    (%si),%al
	mov    %al,-0x5e4(%bp)
	movb   $0x0,(%si)
	lea    -0x5e2(%bp),%ax
	push   %ax
	mov    $D_1496,%ax
	push   %ax
	call   L_116f
	add    $0x4,%sp
	cmpb   $0x0,-0x5e4(%bp)
	je   L_2341
	lea    -0x5e2(%bp),%ax
	mov    %ax,%si
L_231f:
	cmpb   $0x0,(%si)
	je   L_2327
	inc    %si
	jmp   L_231f
L_2327:
	mov    -0x5e4(%bp),%al
	mov    %al,(%si)
L_232d:
	incw   -0x5e6(%bp)
	mov    -0x5e6(%bp),%bx
	mov    (%bx),%al
	cbtw
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%si)
	or     %cl,%cl
	jne   L_232d
L_2341:
	lea    -0x5e2(%bp),%ax
	mov    %ax,%di
	mov    $D_1496,%si
L_234a:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_234a
	mov    -0x6(%bp),%ax
	mov    %ax,D_21ee
	mov    %ax,%di
L_2363:
	cmpb   $0x0,(%di)
	jne   L_236b
	jmp   L_2142
L_236b:
	cmpb   $0x20,(%di)
	je   L_237a
	cmpb   $0x9,(%di)
	je   L_237a
	mov    $0x103,%ax
L_2378:
	jmp   L_237d
L_237a:
	inc    %di
	jmp   L_2363
L_237d:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2384:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x142,%sp
	xor    %si,%si
	/* movw constant -> D_1b76 */
	mov	$0xffff,%ax
	mov	%ax,D_1b76
	movw   $0x0,D_1b78
	movw   $0x0,D_1b7a
	lea    -0x132(%bp),%ax
	mov    %ax,-0x136(%bp)
	movw   $D_2296,-0x13a(%bp)
L_23af:
	cmpw   $0x0,D_1b74
	je   L_23c5
	pushw  D_1b76
	push   %si
	mov    $D_1bd3,%ax
	push   %ax
	call   L_35bc
	add    $0x6,%sp
L_23c5:
	addw   $0x2,-0x136(%bp)
	lea    -0x4(%bp),%ax
	cmp    %ax,-0x136(%bp)
	jbe   L_23e1
	mov    $D_1be7,%ax
	push   %ax
	call   L_1466
	pop    %cx
	mov    $0x1,%ax
	jmp   L_24e5
L_23e1:
	mov    -0x136(%bp),%bx
	mov    %si,(%bx)
	addw   $0x2,-0x13a(%bp)
	push   %si
	mov    -0x13a(%bp),%bx
	mov    $D_228a,%si
	lods   %ds:(%si),%ax
	mov    %ax,(%bx)
	pop    %si
L_23f8:
	mov    %si,%bx
	shl    %bx
	mov    D_1aa4(%bx),%ax
	mov    %ax,-0x138(%bp)
	cmp    $0xfc18,%ax
	jle   L_2473
	cmpw   $0x0,D_1b76
	jge   L_2420
	call   L_1fd8
	mov    %ax,D_1b76
	or     %ax,%ax
	jge   L_2420
	movw   $0x0,D_1b76
L_2420:
	mov    D_1b76,%ax
	add    %ax,-0x138(%bp)
	jl   L_2473
	cmpw   $0x12,-0x138(%bp)
	jge   L_2473
	mov    -0x138(%bp),%bx
	shl    %bx
	mov    D_1a80(%bx),%ax
	mov    %ax,-0x138(%bp)
	mov    %ax,%bx
	shl    %bx
	mov    D_1b28(%bx),%ax
	cmp    D_1b76,%ax
	jne   L_2473
	/* movw constant -> D_1b76 */
	mov	$0xffff,%ax
	mov	%ax,D_1b76
	push   %si
	lea    D_228a,%bx
	mov    $D_2290,%si
	lods   %ds:(%si),%ax
	mov    %ax,(%bx)
	pop    %si
	mov    -0x138(%bp),%si
	cmpw   $0x0,D_1b7a
	jg   L_246c
	jmp   L_23af
L_246c:
	decw   D_1b7a
L_2470:
	jmp   L_23af
L_2473:
	mov    %si,%bx
	shl    %bx
	mov    D_1b4e(%bx),%ax
	mov    %ax,-0x138(%bp)
	cmp    $0xfffe,%ax
	jne   L_24e8
	cmpw   $0x0,D_1b76
	jge   L_249b
	call   L_1fd8
	mov    %ax,D_1b76
	or     %ax,%ax
	jge   L_249b
	movw   $0x0,D_1b76
L_249b:
	movw   $D_1a74,-0x13c(%bp)
L_24a1:
	mov    -0x13c(%bp),%bx
	cmpw   $0xffff,(%bx)
	jne   L_24b3
	mov    -0x13c(%bp),%bx
	cmp    %si,0x2(%bx)
	je   L_24ba
L_24b3:
	addw   $0x4,-0x13c(%bp)
	jmp   L_24a1
L_24ba:
	addw   $0x4,-0x13c(%bp)
	mov    -0x13c(%bp),%bx
	cmpw   $0x0,(%bx)
	jl   L_24d4
	mov    -0x13c(%bp),%bx
	mov    (%bx),%ax
	cmp    D_1b76,%ax
	jne   L_24ba
L_24d4:
	mov    -0x13c(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x138(%bp)
	or     %ax,%ax
	jge   L_24e8
	xor    %ax,%ax
L_24e5:
	jmp   L_259b
L_24e8:
	cmpw   $0x0,-0x138(%bp)
	je   L_24f2
	jmp   L_25cf
L_24f2:
	mov    D_1b7a,%ax
	jmp   L_b5c3
	mov    $D_1bfb,%ax
	push   %ax
	call   L_1466
	pop    %cx
	incw   D_1b78
	movw   $0x3,D_1b7a
L_250a:
	lea    -0x130(%bp),%ax
	cmp    %ax,-0x136(%bp)
	jae   L_2517
	jmp   L_2598
L_2517:
	mov    -0x136(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_1aa4(%bx),%ax
	add    $0x100,%ax
	mov    %ax,-0x138(%bp)
	cmp    $0x0,%ax
	jl   L_255b
	cmpw   $0x12,-0x138(%bp)
	jge   L_255b
	mov    -0x138(%bp),%bx
	shl    %bx
	mov    D_1a80(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x100,D_1b28(%bx)
	jne   L_255b
	mov    -0x138(%bp),%bx
	shl    %bx
	mov    D_1a80(%bx),%si
L_2558:
	jmp   L_2470
L_255b:
	mov    -0x136(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_1aa4(%bx),%ax
	mov    %ax,-0x138(%bp)
	cmpw   $0x0,D_1b74
	je   L_258b
	mov    -0x136(%bp),%bx
	pushw  -0x2(%bx)
	mov    -0x136(%bp),%bx
	pushw  (%bx)
	mov    $D_1c08,%ax
	push   %ax
	call   L_35bc
	add    $0x6,%sp
L_258b:
	subw   $0x2,-0x136(%bp)
	subw   $0x2,-0x13a(%bp)
	jmp   L_250a
L_2598:
	mov    $0x1,%ax
L_259b:
	jmp   L_28cc
	cmpw   $0x0,D_1b74
	je   L_25b3
	pushw  D_1b76
	mov    $D_1c33,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
L_25b3:
	cmpw   $0x0,D_1b76
	je   L_2598
	/* movw constant -> D_1b76 */
	mov	$0xffff,%ax
	mov	%ax,D_1b76
	jmp   L_23f8
	cmp    $0x3,%ax
	ja   L_25cf
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x3b7c(%bx)
L_25cf:
	cmpw   $0x0,D_1b74
	je   L_25e4
	pushw  -0x138(%bp)
	mov    $D_1c54,%ax
	push   %ax
	call   L_35bc
	add    $0x4,%sp
L_25e4:
	mov    -0x138(%bp),%bx
	shl    %bx
	mov    D_1b02(%bx),%ax
	shl    %ax
	sub    %ax,-0x136(%bp)
	mov    -0x13a(%bp),%di
	mov    -0x138(%bp),%bx
	shl    %bx
	mov    D_1b02(%bx),%ax
	shl    %ax
	sub    %ax,-0x13a(%bp)
	mov    -0x13a(%bp),%ax
	inc    %ax
	inc    %ax
	push   %si
	lea    D_228a,%bx
	mov    %ax,%si
	lods   %ds:(%si),%ax
	mov    %ax,(%bx)
	pop    %si
	mov    -0x138(%bp),%ax
	mov    %ax,-0x134(%bp)
	mov    -0x138(%bp),%bx
	shl    %bx
	mov    D_1adc(%bx),%ax
	mov    %ax,-0x138(%bp)
	mov    %ax,%bx
	shl    %bx
	mov    D_1aca(%bx),%ax
	mov    -0x136(%bp),%bx
	add    (%bx),%ax
	inc    %ax
	mov    %ax,-0x132(%bp)
	cmp    $0x12,%ax
	jge   L_2663
	mov    -0x132(%bp),%bx
	shl    %bx
	mov    D_1a80(%bx),%si
	mov    %si,%bx
	shl    %bx
	mov    D_1b28(%bx),%ax
	mov    -0x138(%bp),%dx
	neg    %dx
	cmp    %dx,%ax
	je   L_2675
L_2663:
	mov    -0x138(%bp),%bx
	shl    %bx
	mov    D_1aca(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_1a80(%bx),%si
L_2675:
	mov    -0x134(%bp),%ax
	jmp   L_28bb
L_267c:
	decw   D_1490
	jge   L_2685
	jmp   L_2558
L_2685:
	mov    D_1490,%bx
	shl    %bx
	mov    D_1440(%bx),%ax
	mov    %ax,D_2292
	mov    %ax,%bx
	mov    0x6(%bx),%ax
	mov    $0x3,%cx
	sar    %cl,%ax
	test   $0x7,%ax
	jne   L_26b9
	mov    D_2292,%bx
	mov    D_1494,%ax
	mov    $0x3,%cl
	shl    %cl,%ax
	and    $0x38,%ax
	andw   $0xffc7,0x6(%bx)
	or     %ax,0x6(%bx)
	jmp   L_26e3
L_26b9:
	mov    D_2292,%bx
	mov    0x6(%bx),%ax
	mov    $0x3,%cx
	sar    %cl,%ax
	and    $0x7,%ax
	cmp    D_1494,%ax
	je   L_26e5
	mov    D_2292,%bx
	pushw  0x2(%bx)
	mov    $D_1c5f,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x6,%sp
L_26e3:
	jmp   L_273a
L_26e5:
	cmpw   $0x1,D_1494
	jne   L_273a
	mov    D_2292,%bx
	mov    0x2(%bx),%bx
	cmpb   $0x2e,(%bx)
	je   L_273a
	cmpw   $0x0,(%di)
	je   L_273a
	mov    D_2292,%bx
	mov    0x4(%bx),%ax
	mov    %ax,D_23c4
L_2707:
	mov    D_23c4,%bx
	cmpw   $0x0,(%bx)
	je   L_273a
	mov    D_23c4,%bx
	cmpw   $0x0,0x4(%bx)
	je   L_272f
	mov    D_2292,%bx
	pushw  0x2(%bx)
	mov    $D_1c82,%ax
	push   %ax
	mov    $D_212e,%ax
	push   %ax
	call   L_3078
	add    $0x6,%sp
L_272f:
	mov    D_23c4,%bx
	mov    (%bx),%ax
	mov    %ax,D_23c4
	jmp   L_2707
L_273a:
	mov    $0x6,%ax
	push   %ax
	call   L_1145
	pop    %cx
	mov    %ax,D_23c4
	mov    %ax,%bx
	movw   $0x0,(%bx)
	mov    D_23c4,%bx
	mov    -0x2(%di),%ax
	mov    %ax,0x2(%bx)
	mov    D_23c4,%bx
	mov    (%di),%ax
	mov    %ax,0x4(%bx)
	mov    $D_1ca1,%ax
	push   %ax
	mov    D_2292,%bx
	pushw  0x2(%bx)
	call   L_4bc8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_2785
	cmpw   $0x0,-0x2(%di)
	jne   L_2785
	mov    D_2292,%bx
	movw   $0x0,0x4(%bx)
L_2782:
	jmp   L_267c
L_2785:
	mov    D_2292,%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_279b
	mov    D_2292,%bx
	mov    D_23c4,%ax
	mov    %ax,0x4(%bx)
	jmp   L_2782
L_279b:
	mov    D_2292,%bx
	mov    0x4(%bx),%ax
	mov    %ax,D_228e
L_27a5:
	mov    D_228e,%bx
	cmpw   $0x0,(%bx)
	je   L_27b9
	mov    D_228e,%bx
	mov    (%bx),%ax
	mov    %ax,D_228e
	jmp   L_27a5
L_27b9:
	cmpw   $0x1,D_1494
	jne   L_27d5
	mov    D_2292,%bx
	mov    0x2(%bx),%bx
	cmpb   $0x2e,(%bx)
	jne   L_27d5
	mov    D_228e,%bx
	movw   $0x0,0x4(%bx)
L_27d5:
	mov    D_228e,%bx
	mov    D_23c4,%ax
	mov    %ax,(%bx)
	jmp   L_2782
	mov    (%di),%ax
	mov    %ax,D_1440
	movw   $0x1,D_1490
L_27eb:
	jmp   L_2558
	mov    D_1490,%bx
	shl    %bx
	mov    (%di),%ax
	mov    %ax,D_1440(%bx)
	incw   D_1490
	cmpw   $0x28,D_1490
	jle   L_27eb
	mov    $D_1cab,%ax
	push   %ax
	call   L_142e
	pop    %cx
	jmp   L_27eb
	pushw  D_2278
	mov    $D_1cba,%ax
	push   %ax
	lea    -0x146(%bp),%ax
	push   %ax
	call   L_3a84
	add    $0x6,%sp
	lea    -0x146(%bp),%ax
	push   %ax
	mov    $D_1cbd,%ax
	push   %ax
	call   L_140a
	add    $0x4,%sp
L_2831:
	jmp   L_27eb
	movw   $0x0,D_1492
	movw   $0x0,D_228a
L_283f:
	jmp   L_27eb
	mov    $0x4,%ax
	push   %ax
	call   L_1145
	pop    %cx
	mov    %ax,D_228c
	mov    %ax,%bx
	movw   $0x0,(%bx)
	mov    D_228c,%bx
	mov    (%di),%ax
	mov    %ax,0x2(%bx)
	cmpw   $0x0,D_1492
	jne   L_286a
	mov    D_228c,%ax
	mov    %ax,D_228a
	jmp   L_2873
L_286a:
	mov    D_1492,%bx
	mov    D_228c,%ax
	mov    %ax,(%bx)
L_2873:
	mov    D_228c,%ax
	mov    %ax,D_1492
L_2879:
	jmp   L_2831
	movw   $0x1,D_1494
	jmp   L_2831
	movw   $0x2,D_1494
	jmp   L_2831
	movw   $0x0,D_228a
	jmp   L_2831
	mov    (%di),%ax
	mov    %ax,D_228a
	jmp   L_283f
	mov    (%di),%ax
	mov    %ax,D_228a
	mov    (%di),%ax
	mov    %ax,D_143e
	jmp   L_2879
	mov    -0x2(%di),%ax
	mov    %ax,D_228a
	mov    D_143e,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
	mov    (%di),%ax
	mov    %ax,D_143e
L_28b9:
	jmp   L_2879
L_28bb:
	sub    $0x5,%ax
	cmp    $0xd,%ax
	ja   L_2879
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x3b84(%bx)
	jmp   L_28b9
L_28cc:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_28d4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	mov    0x4(%bp),%di
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
	xor    %si,%si
L_28ec:
	mov    (%di),%al
	cbtw
	jmp   L_28f5
L_28f1:
	inc    %si
L_28f2:
	inc    %di
	jmp   L_290e
L_28f5:
	cmp    $0x9,%ax
	je   L_290b
	cmp    $0x20,%ax
	je   L_290b
	cmp    $0x2b,%ax
	je   L_28f2
	cmp    $0x2d,%ax
	je   L_28f1
	jmp   L_290e
L_290b:
	inc    %di
	jmp   L_28ec
L_290e:
	cmpb   $0x30,(%di)
	jl   L_2958
	cmpb   $0x39,(%di)
	jg   L_2958
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	call   L_40cb
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	cwtd
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
	mov    -0xc(%bp),%ax
	mov    -0xa(%bp),%dx
	add    -0x10(%bp),%ax
	adc    -0xe(%bp),%dx
	sub    $0x30,%ax
	sbb    $0x0,%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	jmp   L_290e
L_2958:
	or     %si,%si
	je   L_296b
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	jmp   L_2971
L_296b:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
L_2971:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2978:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%ax
	mulw   0x6(%bp)
	mov    %ax,0x4(%bp)
	pushw  0x4(%bp)
	call   L_4974
	pop    %cx
	mov    %ax,%di
	or     %di,%di
	jne   L_2998
	xor    %ax,%ax
	jmp   L_29b9
L_2998:
	mov    %di,%si
	mov    0x4(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
L_29a8:
	decw   -0x6(%bp)
	jl   L_29b7
	mov    %si,%bx
	inc    %si
	inc    %si
	movw   $0x0,(%bx)
	jmp   L_29a8
L_29b7:
	mov    %di,%ax
L_29b9:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_4abe
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_29ce:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_29e3
	pop    %cx
	push   %ax
	call   L_2ca1
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_29e3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x16,%sp
	lea    -0x16(%bp),%ax
	push   %ax
	call   L_2ff0
	pop    %cx
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    -0x10(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	call   L_40cb
	add    $0x8,%sp
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	mov    0x2(%bx),%dx
	sub    -0x1a(%bp),%ax
	sbb    -0x18(%bp),%dx
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_2b01
	pop    %cx
	mov    %ax,%si
	mov    0xe(%si),%di
	movw   $0x77,-0x6(%bp)
	movw   $L_012f,-0x8(%bp)
	cmpw   $0x4a,0xa(%si)
	je   L_2a43
	cmpw   $0x4b,0xa(%si)
	jne   L_2a6b
L_2a43:
	mov    0xa(%si),%ax
	sub    $0x4a,%ax
	mov    %ax,%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	mov    D_1d14(%bx),%ax
	mov    %ax,-0x6(%bp)
	mov    0xa(%si),%ax
	sub    $0x4a,%ax
	mov    %ax,%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	mov    D_1d16(%bx),%ax
	mov    %ax,-0x8(%bp)
L_2a6b:
	pushw  -0x6(%bp)
	push   %si
	call   L_2aca
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	pushw  -0x8(%bp)
	push   %si
	call   L_2aca
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	cmpw   $0x0,-0xe(%bp)
	je   L_2ac1
	cmp    -0x6(%bp),%di
	jg   L_2a9b
	cmp    -0x6(%bp),%di
	jne   L_2ac1
	cmpw   $0x2,0x4(%si)
	jl   L_2ac1
L_2a9b:
	cmp    -0x8(%bp),%di
	jl   L_2aab
	cmp    -0x8(%bp),%di
	jne   L_2ac1
	cmpw   $0x1,0x4(%si)
	jge   L_2ac1
L_2aab:
	addw   $0xe10,-0xc(%bp)
	adcw   $0x0,-0xa(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_2b01
	pop    %cx
	mov    %ax,%si
	incw   0x10(%si)
L_2ac1:
	mov    %si,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2aca:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	cmp    $0x3a,%si
	jl   L_2ae8
	pushw  0xa(%di)
	call   L_2db9
	pop    %cx
	sub    $0x16d,%ax
	add    %ax,%si
L_2ae8:
	mov    %si,%ax
	sub    0xe(%di),%ax
	add    0xc(%di),%ax
	add    $0x2bc,%ax
	cwtd
	mov    $0x7,%cx
	idiv   %cx
	mov    %si,%ax
	sub    %dx,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2b01:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	mov    $0x5180,%ax
	mov    $0x1,%dx
	push   %dx
	push   %ax
	mov    0x4(%bp),%bx
	pushw  0x2(%bx)
	pushw  (%bx)
	call   L_41c5
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    $0x5180,%ax
	mov    $0x1,%dx
	push   %dx
	push   %ax
	mov    0x4(%bp),%bx
	pushw  0x2(%bx)
	pushw  (%bx)
	call   L_4174
	add    $0x8,%sp
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
	cmpw   $0x0,-0xa(%bp)
	jg   L_2b60
	jl   L_2b4f
	cmpw   $0x0,-0xc(%bp)
	jae   L_2b60
L_2b4f:
	addw   $0x5180,-0xc(%bp)
	adcw   $0x1,-0xa(%bp)
	subw   $0x1,-0x10(%bp)
	sbbw   $0x0,-0xe(%bp)
L_2b60:
	mov    $D_1d1c,%di
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0xa(%bp)
	pushw  -0xc(%bp)
	call   L_41c5
	add    $0x8,%sp
	mov    %di,%bx
	inc    %di
	inc    %di
	mov    %ax,(%bx)
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0xa(%bp)
	pushw  -0xc(%bp)
	call   L_4174
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	cwtd
	mov    $0x3c,%cx
	idiv   %cx
	mov    %di,%bx
	inc    %di
	inc    %di
	mov    %dx,(%bx)
	mov    -0x8(%bp),%ax
	cwtd
	mov    $0x3c,%cx
	idiv   %cx
	mov    %ax,-0x8(%bp)
	mov    %di,%bx
	inc    %di
	inc    %di
	mov    -0x8(%bp),%ax
	mov    %ax,(%bx)
	mov    $0x7,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    -0x10(%bp),%ax
	mov    -0xe(%bp),%dx
	add    $0x4,%ax
	adc    $0x70,%dx
	push   %dx
	push   %ax
	call   L_41c5
	add    $0x8,%sp
	mov    %ax,D_1d28
	cmpw   $0x0,-0xe(%bp)
	jl   L_2c0b
	jg   L_2bdf
	cmpw   $0x0,-0x10(%bp)
	jb   L_2c0b
L_2bdf:
	movw   $0x46,-0x8(%bp)
L_2be4:
	pushw  -0x8(%bp)
	call   L_2db9
	pop    %cx
	cwtd
	cmp    %dx,-0xe(%bp)
	jl   L_2c33
	jg   L_2bf8
	cmp    %ax,-0x10(%bp)
	jb   L_2c33
L_2bf8:
	pushw  -0x8(%bp)
	call   L_2db9
	pop    %cx
	cwtd
	sub    %ax,-0x10(%bp)
	sbb    %dx,-0xe(%bp)
	incw   -0x8(%bp)
	jmp   L_2be4
L_2c0b:
	movw   $0x46,-0x8(%bp)
L_2c10:
	cmpw   $0x0,-0xe(%bp)
	jg   L_2c33
	jl   L_2c1e
	cmpw   $0x0,-0x10(%bp)
	jae   L_2c33
L_2c1e:
	mov    -0x8(%bp),%ax
	dec    %ax
	push   %ax
	call   L_2db9
	pop    %cx
	cwtd
	add    %ax,-0x10(%bp)
	adc    %dx,-0xe(%bp)
	decw   -0x8(%bp)
	jmp   L_2c10
L_2c33:
	mov    -0x8(%bp),%ax
	mov    %ax,D_1d26
	mov    -0x10(%bp),%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,D_1d2a
	pushw  -0x8(%bp)
	call   L_2db9
	pop    %cx
	cmp    $0x16e,%ax
	jne   L_2c54
	movw   $0x1d,D_1cfe
L_2c54:
	movw   $0x0,-0x8(%bp)
L_2c59:
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    -0x6(%bp),%ax
	cmp    D_1cfc(%bx),%ax
	jl   L_2c78
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    D_1cfc(%bx),%ax
	sub    %ax,-0x6(%bp)
	incw   -0x8(%bp)
	jmp   L_2c59
L_2c78:
	movw   $0x1c,D_1cfe
	mov    -0x6(%bp),%ax
	inc    %ax
	mov    %di,%bx
	inc    %di
	inc    %di
	mov    %ax,(%bx)
	mov    %di,%bx
	inc    %di
	inc    %di
	mov    -0x8(%bp),%ax
	mov    %ax,(%bx)
	movw   $0x0,D_1d2c
	mov    $D_1d1c,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2ca1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    $D_1ce2,%di
	mov    $D_1d2e,%si
L_2cad:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_2cad
	mov    0x4(%bp),%bx
	mov    0xc(%bx),%ax
	mov    $0x3,%dx
	imul   %dx
	add    $D_1d48,%ax
	mov    %ax,%si
	mov    $D_1ce2,%di
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	inc    %di
	mov    0x4(%bp),%ax
	add    $0x8,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    $0x3,%dx
	imul   %dx
	add    $D_1d5e,%ax
	mov    %ax,%si
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	pushw  (%bx)
	push   %di
	call   L_2dd7
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_2dd7
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_2dd7
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_2dd7
	add    $0x4,%sp
	mov    %ax,%di
	mov    0x4(%bp),%bx
	cmpw   $0x64,0xa(%bx)
	jl   L_2d9c
	movb   $0x32,0x1(%di)
	movb   $0x30,0x2(%di)
L_2d9c:
	inc    %di
	inc    %di
	mov    0x4(%bp),%bx
	mov    0xa(%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_2dd7
	add    $0x4,%sp
	mov    %ax,%di
	mov    $D_1ce2,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2db9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	cwtd
	mov    $0x4,%cx
	idiv   %cx
	or     %dx,%dx
	jne   L_2dd0
	mov    $0x16e,%ax
	jmp   L_2dd3
L_2dd0:
	mov    $0x16d,%ax
L_2dd3:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2dd7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	inc    %di
	cmpw   $0xa,0x6(%bp)
	jl   L_2e02
	mov    0x6(%bp),%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	add    $0x30,%dx
	mov    %di,%bx
	inc    %di
	mov    %dx,%cx
	mov    %cl,(%bx)
	jmp   L_2e08
L_2e02:
	mov    %di,%bx
	inc    %di
	movb   $0x20,(%bx)
L_2e08:
	mov    0x6(%bp),%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	add    $0x30,%dx
	mov    %di,%bx
	inc    %di
	mov    %dx,%cx
	mov    %cl,(%bx)
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2e38
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2e38:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x288,%sp
	mov    $0x1,%si
	movw   $0x0,-0x28c(%bp)
	mov    $D_1d8c,%ax
	push   %ax
	call   L_3004
	pop    %cx
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_2e5e
	movw   $D_1d91,-0x6(%bp)
L_2e5e:
	mov    $0x2f,%ax
	push   %ax
L_2e62:
	pushw  0x4(%bp)
	call   L_32c8
	add    $0x4,%sp
	or     %ax,%ax
	je   L_2e74
	mov    $D_1da0,%ax
	jmp   L_2e77
L_2e74:
	mov    -0x6(%bp),%ax
L_2e77:
	mov    %ax,%di
L_2e79:
	lea    -0x86(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	push   %di
	call   L_2f61
	add    $0x6,%sp
	mov    %ax,%di
L_2e8a:
	pushw  0x6(%bp)
	lea    -0x86(%bp),%ax
	push   %ax
	call   L_2fd6
	add    $0x4,%sp
	mov    D_23c6,%ax
	jmp   L_2f27
L_2e9e:
	movw   $D_1da1,-0x286(%bp)
	lea    -0x86(%bp),%ax
	mov    %ax,-0x284(%bp)
	movw   $0x1,-0x28a(%bp)
L_2eb2:
	mov    -0x28a(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,-0x288(%bp)
	lea    -0x286(%bp),%ax
	mov    -0x28a(%bp),%dx
	inc    %dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	mov    -0x288(%bp),%ax
	mov    %ax,(%bx)
	or     %ax,%ax
	je   L_2ef5
	cmpw   $0xfe,-0x28a(%bp)
	jl   L_2eef
	movw   $0x7,D_23c6
	mov    $0xffff,%ax
	jmp   L_2f25
L_2eef:
	incw   -0x28a(%bp)
	jmp   L_2eb2
L_2ef5:
	lea    -0x286(%bp),%ax
	push   %ax
	mov    $D_1d84,%ax
	push   %ax
	call   L_2fd6
	add    $0x4,%sp
	mov    $0xffff,%ax
	jmp   L_2f5a
L_2f09:
	inc    %si
	cmp    $0x5,%si
	jbe   L_2f14
	mov    $0xffff,%ax
	jmp   L_2f5a
L_2f14:
	push   %si
	call   L_37a0
	pop    %cx
	jmp   L_2e8a
L_2f1c:
	incw   -0x28c(%bp)
	jmp   L_2f43
L_2f22:
	mov    $0xffff,%ax
L_2f25:
	jmp   L_2f5a
L_2f27:
	cmp    $0x7,%ax
	je   L_2f22
	cmp    $0x8,%ax
	jne   L_2f34
	jmp   L_2e9e
L_2f34:
	cmp    $0xc,%ax
	je   L_2f22
	cmp    $0x900d,%ax
	je   L_2f1c
	cmp    $0x1a,%ax
	je   L_2f09
L_2f43:
	or     %di,%di
	je   L_2f4a
	jmp   L_2e79
L_2f4a:
	cmpw   $0x0,-0x28c(%bp)
	je   L_2f57
	movw   $0xd,D_23c6
L_2f57:
	mov    $0xffff,%ax
L_2f5a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2f61:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    0x8(%bp),%ax
	mov    %ax,-0x6(%bp)
L_2f75:
	cmpb   $0x0,(%di)
	je   L_2f96
	cmpb   $0x3a,(%di)
	je   L_2f96
	cmpb   $0x2d,(%di)
	je   L_2f96
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	jmp   L_2f75
L_2f96:
	mov    0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_2fa7
	mov    -0x6(%bp),%bx
	movb   $0x2f,(%bx)
	incw   -0x6(%bp)
L_2fa7:
	cmpb   $0x0,(%si)
	je   L_2fbe
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	jmp   L_2fa7
L_2fbe:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	cmpb   $0x0,(%di)
	je   L_2fce
	inc    %di
	mov    %di,%ax
	jmp   L_2fd0
L_2fce:
	xor    %ax,%ax
L_2fd0:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2fd6:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall exece(59) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    D_21c8,%cx
	/* exece(59) -> execve(11) [TLVC# changed] */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_execve,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2fee
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2fee:
	pop    %bp
	ret


L_2ff0:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall ftime(35) */
	mov    0x4(%bp),%ax
	/* TODO: Venix-only ftime(35): no TLVC equivalent */
	xor	%ax,%ax	/* stub */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3001
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3001:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3004:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    D_21c8,%si
L_3012:
	cmpw   $0x0,(%si)
	je   L_3030
	mov    %si,%bx
	inc    %si
	inc    %si
	pushw  (%bx)
	push   %di
	call   L_3037
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_3012
	mov    -0x6(%bp),%ax
	jmp   L_3032
L_3030:
	xor    %ax,%ax
L_3032:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3037:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_3044:
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_3063
	mov    %di,%bx
	inc    %di
	cmpb   $0x3d,(%bx)
	jne   L_3044
	mov    %si,%ax
	jmp   L_3074
L_3063:
	cmpb   $0x0,(%di)
	jne   L_3072
	cmpb   $0x3d,-0x1(%si)
	jne   L_3072
	mov    %si,%ax
	jmp   L_3074
L_3072:
	xor    %ax,%ax
L_3074:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3078:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_3ad6
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_309e
	mov    $0xffff,%ax
	jmp   L_30a0
L_309e:
	xor    %ax,%ax
L_30a0:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_30a4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	call   L_30c8
L_30b0:
	call   L_3138
	mov    %ax,%si
	or     %ax,%ax
	je   L_30be
	cmp    %di,0x4(%si)
	jne   L_30b0
L_30be:
	call   L_30f3
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_30c8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_1db2
	jne   L_30e7
	mov    $D_1fc8,%ax
	push   %ax
	mov    $D_1da4,%ax
	push   %ax
	call   L_32f4
	add    $0x4,%sp
	mov    %ax,D_1db2
	jmp   L_30ef
L_30e7:
	pushw  D_1db2
	call   L_36be
	pop    %cx
L_30ef:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_30f3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_1db2
	je   L_310d
	pushw  D_1db2
	call   L_48fd
	pop    %cx
	movw   $0x0,D_1db2
L_310d:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3111:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
L_311a:
	cmpb   $0x0,(%di)
	je   L_3127
	cmpb   $0x3a,(%di)
	je   L_3127
	inc    %di
	jmp   L_311a
L_3127:
	cmpb   $0x0,(%di)
	je   L_3132
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
L_3132:
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3138:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_1db2
	jne   L_315d
	mov    $D_1fca,%ax
	push   %ax
	mov    $D_1da4,%ax
	push   %ax
	call   L_32f4
	add    $0x4,%sp
	mov    %ax,D_1db2
	or     %ax,%ax
	jne   L_315d
	xor    %ax,%ax
	jmp   L_3177
L_315d:
	pushw  D_1db2
	mov    $0x200,%ax
	push   %ax
	mov    $D_1db4,%ax
	push   %ax
	call   L_3256
	add    $0x6,%sp
	mov    %ax,%di
	or     %di,%di
	jne   L_3179
	xor    %ax,%ax
L_3177:
	jmp   L_31e6
L_3179:
	mov    %di,D_1fb6
	push   %di
	call   L_3111
	pop    %cx
	mov    %ax,%di
	mov    %di,D_1fb8
	push   %di
	call   L_3111
	pop    %cx
	mov    %ax,%di
	push   %di
	call   L_31ea
	pop    %cx
	mov    %ax,D_1fba
	push   %di
	call   L_3111
	pop    %cx
	mov    %ax,%di
	push   %di
	call   L_31ea
	pop    %cx
	mov    %ax,D_1fbc
	movw   $0x0,D_1fbe
	/* dual-reloc split: movw $D_1db0,D_1fc0 */
	mov	$D_1db0,%ax
	mov	%ax,D_1fc0
	push   %di
	call   L_3111
	pop    %cx
	mov    %ax,%di
	mov    %di,D_1fc2
	push   %di
	call   L_3111
	pop    %cx
	mov    %ax,%di
	mov    %di,D_1fc4
	push   %di
	call   L_3111
	pop    %cx
	mov    %ax,%di
	mov    %di,D_1fc6
L_31d3:
	cmpb   $0x0,(%di)
	je   L_31e0
	cmpb   $0xa,(%di)
	je   L_31e0
	inc    %di
	jmp   L_31d3
L_31e0:
	movb   $0x0,(%di)
	mov    $D_1fb6,%ax
L_31e6:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_31ea:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	xor    %si,%si
	mov    %si,-0x6(%bp)
L_31f9:
	mov    (%di),%al
	cbtw
	jmp   L_3204
L_31fe:
	incw   -0x6(%bp)
L_3201:
	inc    %di
	jmp   L_321d
L_3204:
	cmp    $0x9,%ax
	je   L_321a
	cmp    $0x20,%ax
	je   L_321a
	cmp    $0x2b,%ax
	je   L_3201
	cmp    $0x2d,%ax
	je   L_31fe
	jmp   L_321d
L_321a:
	inc    %di
	jmp   L_31f9
L_321d:
	cmpb   $0x30,(%di)
	jl   L_3243
	cmpb   $0x39,(%di)
	jg   L_3243
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    %di,%bx
	inc    %di
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	add    %dx,%ax
	sub    $0x30,%ax
	mov    %ax,%si
	jmp   L_321d
L_3243:
	cmpw   $0x0,-0x6(%bp)
	je   L_324f
	mov    %si,%ax
	neg    %ax
	jmp   L_3251
L_324f:
	mov    %si,%ax
L_3251:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3256:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x8(%bp),%di
	mov    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
L_3266:
	decw   0x6(%bp)
	jle   L_3296
	decw   0x2(%di)
	jl   L_327c
	mov    (%di),%bx
	incw   (%di)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_3281
L_327c:
	push   %di
	call   L_36f2
	pop    %cx
L_3281:
	mov    %ax,%si
	or     %ax,%ax
	jl   L_3296
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	cmp    $0xa,%si
	jne   L_3266
L_3296:
	or     %si,%si
	jge   L_32a6
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jne   L_32a6
	xor    %ax,%ax
	jmp   L_32b2
L_32a6:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x6(%bp)
	mov    0x4(%bp),%ax
L_32b2:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_32b8:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getuid(24) */
	/* getuid -> __NR_getuid */
	mov	$__NR_getuid,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_32c6
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_32c6:
	pop    %bp
	ret


L_32c8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
L_32d1:
	mov    (%di),%al
	cbtw
	mov    0x6(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_32e6
	mov    %di,%ax
	jmp   L_32f0
L_32e6:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_32d1
	xor    %ax,%ax
L_32f0:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_32f4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	cmpb   $0x0,D_1fcc
	jne   L_3331
	movw   $D_2136,-0x8(%bp)
	/* suppressed: movw $D_21be,D_21be (self-ref in .data) */
	movw   $0x88,-0xa(%bp)
L_3317:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_332c
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_3317
L_332c:
	movb   $0x1,D_1fcc
L_3331:
	movw   $D_211e,-0x6(%bp)
L_3336:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_3355
	mov    -0x6(%bp),%ax
	cmp    D_21be,%ax
	jb   L_334f
	xor    %ax,%ax
	jmp   L_33c0
L_334f:
	addw   $0x8,-0x6(%bp)
	jmp   L_3336
L_3355:
	cmpb   $0x77,(%di)
	jne   L_336b
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_33f2
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_33aa
L_336b:
	cmpb   $0x61,(%di)
	jne   L_33ac
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c46
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_3399
	cmpw   $0x2,D_23c6
	jne   L_3399
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_33f2
	add    $0x4,%sp
	mov    %ax,%si
L_3399:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_3a48
	add    $0x8,%sp
L_33aa:
	jmp   L_33ba
L_33ac:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c46
	add    $0x4,%sp
	mov    %ax,%si
L_33ba:
	or     %si,%si
	jge   L_33c2
	xor    %ax,%ax
L_33c0:
	jmp   L_33ea
L_33c2:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_33e0
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_33e7
L_33e0:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_33e7:
	mov    -0x6(%bp),%ax
L_33ea:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_33f2:
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
	jns    L_3406
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3406:
	pop    %bp
	ret


L_3408:
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
	jne   L_3426
	jmp   L_356f
L_3426:
	cmpw   $0x2,0xa(%bp)
	jl   L_342f
	jmp   L_3510
L_342f:
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_343b
	jmp   L_3510
L_343b:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_344a
	jmp   L_3510
L_344a:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%si
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,0xa(%bp)
	jne   L_34a3
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
	call   L_3a48
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
	jmp   L_34be
L_34a3:
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
L_34be:
	or     %si,%si
	jle   L_34ff
	mov    %si,%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jg   L_34ff
	jl   L_34d1
	cmp    %ax,-0x8(%bp)
	ja   L_34ff
L_34d1:
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x4(%bp),%bx
	sub    (%bx),%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jl   L_34ff
	jg   L_34e9
	cmp    %ax,-0x8(%bp)
	jb   L_34ff
L_34e9:
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	add    %ax,(%bx)
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	sub    %ax,0x2(%bx)
	xor    %ax,%ax
	jmp   L_35b0
L_34ff:
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x1,%ax
	and    $0x0,%dx
	mov    %ax,%di
	jmp   L_3512
L_3510:
	xor    %di,%di
L_3512:
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
	call   L_3a48
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
	or     %di,%di
	je   L_3579
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_3566
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_359f
L_3566:
	pushw  0x4(%bp)
	call   L_36f2
	pop    %cx
	jmp   L_359f
L_356f:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
L_3579:
	je   L_359f
	pushw  0x4(%bp)
	call   L_488e
	pop    %cx
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_3a48
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_359f:
	cmpw   $0xffff,-0x6(%bp)
	jne   L_35b2
	cmpw   $0xffff,-0x8(%bp)
	jne   L_35b2
	mov    $0xffff,%ax
L_35b0:
	jmp   L_35b4
L_35b2:
	xor    %ax,%ax
L_35b4:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_35bc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_2126,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_3ad6
	add    $0x6,%sp
	mov    D_212c,%al
	cbtw
	test   $0x20,%ax
	je   L_35e0
	mov    $0xffff,%ax
	jmp   L_35e2
L_35e0:
	xor    %ax,%ax
L_35e2:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_35e6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_3647
L_3601:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_3647
	mov    0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
L_360f:
	decw   0x2(%si)
	jl   L_3620
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_3625
L_3620:
	push   %si
	call   L_36f2
	pop    %cx
L_3625:
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jl   L_3638
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_363d
L_3638:
	mov    -0x8(%bp),%ax
	jmp   L_364a
L_363d:
	decw   -0xa(%bp)
	jne   L_360f
	incw   -0x8(%bp)
	jmp   L_3601
L_3647:
	mov    -0x8(%bp),%ax
L_364a:
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
	je   L_36b3
L_366c:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_36b3
	mov    0x6(%bp),%ax
	mov    %ax,-0x6(%bp)
L_367a:
	decw   0x2(%si)
	jl   L_3692
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
	jmp   L_36a0
L_3692:
	push   %si
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_47a0
	add    $0x4,%sp
L_36a0:
	decw   -0x6(%bp)
	jne   L_367a
	mov    0x6(%si),%al
	cbtw
	test   $0x20,%ax
	jne   L_36b3
	incw   -0x8(%bp)
	jmp   L_366c
L_36b3:
	mov    -0x8(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_36be:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	push   %di
	call   L_488e
	pop    %cx
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_3a48
	add    $0x8,%sp
	movw   $0x0,0x2(%di)
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	andb   $0xcf,0x6(%di)
	pop    %di
	pop    %si
	pop    %bp
	ret


L_36f2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_3709
	mov    $0xffff,%ax
	jmp   L_3715
L_3709:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_3717
	mov    $0xffff,%ax
L_3715:
	jmp   L_378f
L_3717:
	cmpw   $0x0,0x4(%di)
	jne   L_374b
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_3732
	mov    0x7(%di),%al
	cbtw
	add    $D_2050,%ax
	mov    %ax,0x4(%di)
	jmp   L_3717
L_3732:
	mov    $0x200,%ax
	push   %ax
	call   L_4974
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_3747
	orb    $0x4,0x6(%di)
	jmp   L_3717
L_3747:
	orb    $0x8,0x6(%di)
L_374b:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_375e
	mov    $0x1,%ax
	jmp   L_3761
L_375e:
	mov    $0x200,%ax
L_3761:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4c5c
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_3791
	cmpw   $0xffff,0x2(%di)
	jne   L_3783
	orb    $0x10,0x6(%di)
	jmp   L_3787
L_3783:
	orb    $0x20,0x6(%di)
L_3787:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_378f:
	jmp   L_379b
L_3791:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_379b:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_37a0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x4(%bp)
	je   L_37de
	mov    $0x3e8,%ax
	push   %ax
	call   L_383c
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    $D_2064,%ax
	push   %ax
	call   L_3860
	pop    %cx
	or     %ax,%ax
	je   L_37e0
	pushw  -0x8(%bp)
	mov    $0xe,%ax
	push   %ax
	call   L_3928
	add    $0x4,%sp
	pushw  -0x6(%bp)
	call   L_383c
	pop    %cx
L_37de:
	jmp   L_381e
L_37e0:
	cmpw   $0x0,-0x6(%bp)
	je   L_3801
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jbe   L_37f6
	mov    0x4(%bp),%ax
	sub    %ax,-0x6(%bp)
	jmp   L_3801
L_37f6:
	mov    -0x6(%bp),%ax
	mov    %ax,0x4(%bp)
	movw   $0x1,-0x6(%bp)
L_3801:
	mov    $D_1825,%ax
	push   %ax
	mov    $0xe,%ax
	push   %ax
	call   L_3928
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	pushw  0x4(%bp)
	call   L_383c
	pop    %cx
L_3819:
	call   L_3850
	jmp   L_3819
L_381e:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x1,%ax
	push   %ax
	mov    $D_2064,%ax
	push   %ax
	call   L_388e
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_383c:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall alarm(27) */
	mov    0x4(%bp),%ax
	/* alarm -> __NR_alarm */
	xchg	%ax,%bx
	mov	$__NR_alarm,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_384d
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_384d:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3850:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall pause(29) */
	/* pause -> __NR_pause */
	mov	$__NR_pause,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_385e
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_385e:
	pop    %bp
	ret


L_3860:
	push   %bp
	mov    %sp,%bp
	mov    0x4(%bp),%dx
	mov    0x2(%bp),%cx
	mov    0x0(%bp),%bx
	mov    %dx,%bp
	mov    %bx,0x0(%bp)
	mov    %sp,0x2(%bp)
	mov    %cx,0x4(%bp)
	mov    0x2,%bx
	mov    %bx,0x6(%bp)
	mov    (%bx),%cx
	mov    %cx,0x8(%bp)
	mov    0x2(%bx),%cx
	mov    %cx,0xa(%bp)
	mov    $0x0,%ax
	pop    %bp
	ret


L_388e:
	push   %bp
	mov    %sp,%bp
	mov    0x4(%bp),%bx
	mov    0x6(%bp),%ax
	or     %ax,%ax
	jne   L_389c
	inc    %ax
L_389c:
	mov    (%bx),%cx
	jmp   L_38a7
L_38a0:
	mov    0x0(%bp),%bp
	or     %bp,%bp
	je   L_38b2
L_38a7:
	cmp    %cx,0x0(%bp)
	jne   L_38a0
	mov    -0x2(%bp),%si
	mov    -0x4(%bp),%di
L_38b2:
	mov    %cs,%cx
	mov    %ds,%dx
	cmp    %dx,%cx
	je   L_38d1
	mov    0x6(%bx),%bp
	or     %bp,%bp
	je   L_38d1
	mov    %bp,0x2
	mov    0x8(%bx),%cx
	mov    %cx,0x0(%bp)
	mov    0xa(%bx),%cx
	mov    %cx,0x2(%bp)
L_38d1:
	mov    (%bx),%bp
	mov    0x2(%bx),%sp
	pop    %cx
	pop    %cx
	mov    0x4(%bx),%cx
	push   %cx
	ret
	/* .byte 0 (alignment) */


L_38de:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_38e9:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_38f4
	inc    %si
	jmp   L_38e9
L_38f4:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_38fa:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall exece(59) */
	mov    0x4(%bp),%ax
	mov    D_21c8,%cx
	mov    %bp,%dx
	add    $0x6,%dx
	/* exece(59) -> execve(11) [TLVC# changed] */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_execve,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3915
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3915:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3918:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall fork(2) */
	/* fork -> __NR_fork */
	mov	$__NR_fork,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3926
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3926:
	pop    %bp
	ret


L_3928:
	push   %bp
	mov    %sp,%bp
	mov    $0x16,%cx
	mov    0x4(%bp),%ax
	or     %ax,%ax
	jle   L_396b
	cmp    $0x20,%ax
	jg   L_396b
	mov    %ax,%cx
	shl    %ax
	add    $D_23c8,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    0x6(%bp),%ax
	mov    %ax,(%bx)
	cmp    $0x2,%ax
	jl   L_3957
	mov    $0x6,%ax
	mul    %cx
	add    $D_196e,%ax
L_3957:
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
	jl   L_3968
	pop    %ax
	jmp   L_3969
L_3968:
	pop    %bx
L_3969:
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3972
L_396b:
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	mov    $0xffff,%ax
L_3972:
	pop    %bp
	ret
L_3974:
	push   %ax
	mov    $D_23ca,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23cc,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23ce,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23d0,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23d2,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23d4,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23d6,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23d8,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23da,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23dc,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23de,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23e0,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23e2,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23e4,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23e6,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23e8,%ax
	jmp   L_39d8
	push   %ax
	mov    $D_23ea,%ax
L_39d8:
	jmp   L_3a32
	push   %ax
	mov    $D_23ec,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_23ee,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_23f0,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_23f2,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_23f4,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_23f6,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_23f8,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_23fa,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_23fc,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_23fe,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_2400,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_2402,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_2404,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_2406,%ax
	jmp   L_3a32
	push   %ax
	mov    $D_2408,%ax
L_3a32:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	sub    $D_23c8,%ax
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


L_3a48:
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
L_3a63:
	pop    %si
	pop    %bp
	ret


L_3a66:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall time(13) */
	/* time -> __NR_time */
	xchg	%ax,%bx
	mov	$__NR_time,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3a76
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3a74:
	jmp   L_3a82
L_3a76:
	mov    0x4(%bp),%bx
	or     %bx,%bx
	je   L_3a82
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_3a82:
	pop    %bp
	ret


L_3a84:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	movb   $0x42,-0x6(%bp)
	mov    0x4(%bp),%ax
	mov    %ax,-0xc(%bp)
	movw   $0x7fff,-0xa(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_3ad6
	add    $0x6,%sp
	decw   -0xa(%bp)
	jl   L_3abe
	mov    -0xc(%bp),%bx
	incw   -0xc(%bp)
	movb   $0x0,(%bx)
	xor    %ax,%ax
	jmp   L_3acb
L_3abe:
	lea    -0xc(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_47a0
	add    $0x4,%sp
L_3acb:
	mov    0x4(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3ad6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3aee:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_3b00
	jmp   L_409d
L_3b00:
	cmpb   $0x25,-0x5(%bp)
	je   L_3b13
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_3b11:
	jmp   L_3aee
L_3b13:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_3b3e
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_46ae
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3b3e:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_3b57
	incb   -0xa6(%bp)
	inc    %di
L_3b57:
	cmpb   $0x30,(%di)
	jne   L_3b62
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_3b62:
	cmpb   $0x2a,(%di)
	je   L_3b92
	movw   $0x0,-0xe(%bp)
L_3b6c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_3ba8
	cmpw   $0x9,-0xc(%bp)
	jg   L_3ba8
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_3b6c
L_3b92:
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
L_3ba8:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_3bef
	cmpb   $0x2a,(%di)
	je   L_3bf1
	movw   $0x0,-0x10(%bp)
L_3bc1:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_3beb
	cmpw   $0x9,-0xc(%bp)
	jg   L_3beb
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_3bc1
L_3beb:
	addw   $0x30,-0xc(%bp)
L_3bef:
	jmp   L_3c08
L_3bf1:
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
L_3c08:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_4012
L_3c13:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_3c24
L_3c1b:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_3c54
	jmp   L_3c4d
L_3c24:
	cmp    $0x64,%ax
	je   L_3c56
	cmp    $0x6f,%ax
	je   L_3c3a
	cmp    $0x75,%ax
	je   L_3c4d
	cmp    $0x78,%ax
	je   L_3c48
	jmp   L_3c1b
L_3c3a:
	movw   $0x8,-0x12(%bp)
	jmp   L_3c4d
L_3c41:
	movw   $0x8,-0x12(%bp)
	jmp   L_3c54
L_3c48:
	movw   $0x10,-0x12(%bp)
L_3c4d:
	jmp   L_3c9f
L_3c4f:
	movw   $0x10,-0x12(%bp)
L_3c54:
	jmp   L_3caf
L_3c56:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_3c73
L_3c66:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_3c73:
	cmpw   $0x0,-0x14(%bp)
	jg   L_3cbd
	jl   L_3c81
	cmpw   $0x0,-0x16(%bp)
	jae   L_3cbd
L_3c81:
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
	jmp   L_3cbd
L_3c9f:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_3cbd
L_3caf:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_3cbd:
	cmpw   $0x0,-0x14(%bp)
	jne   L_3cc7
	cmpw   $0x0,-0x16(%bp)
L_3cc7:
	jne   L_3cd8
	cmpw   $0x0,-0x10(%bp)
	jne   L_3cd8
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_3cd8:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_3cdf:
	cmpw   $0x0,-0x14(%bp)
	jne   L_3ce9
	cmpw   $0x0,-0x16(%bp)
L_3ce9:
	je   L_3d34
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_41fc
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_3d0a
	mov    $0x30,%ax
	jmp   L_3d0d
L_3d0a:
	mov    $0x57,%ax
L_3d0d:
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
	call   L_40f4
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_3cdf
L_3d34:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_3d83
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_3d34
L_3d52:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_3d5e:
	cmpw   $0x2,-0xc(%bp)
	jge   L_3d83
L_3d64:
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_3d7e
	decw   -0x8(%bp)
L_3d7e:
	incw   -0xc(%bp)
	jmp   L_3d5e
L_3d83:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_3d8a:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_3da1
	movw   $0x0,-0xc(%bp)
L_3da1:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_3db0
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_3db0:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_46ae
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3dd3:
	jmp   L_3b11
L_3dd6:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_3de8
	movw   $D_206e,-0xa(%bp)
L_3de8:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_3df7
	movw   $0x7fff,-0xc(%bp)
L_3df7:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3dfd:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_3d8a
	decw   -0xc(%bp)
	jl   L_3d8a
	incw   -0x8(%bp)
	jmp   L_3dfd
L_3e0f:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_3e17:
	jmp   L_3dd3
L_3e19:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3e25
	movw   $0x6,-0x10(%bp)
L_3e25:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_43fa
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_3e5a
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_3e5a:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_3e6f
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_3e87
L_3e6f:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_3e6f
L_3e87:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_3e9a
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_3e9a:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_3eb9
L_3ea6:
	decw   -0xc(%bp)
	jl   L_3eb9
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_3ea6
L_3eb9:
	cmpw   $0x0,-0xc(%bp)
	jg   L_3ec2
	jmp   L_3d83
L_3ec2:
	decw   -0xc(%bp)
	jge   L_3eca
	jmp   L_3d83
L_3eca:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_3ec2
L_3edf:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3eed
	movw   $0x6,-0x10(%bp)
	jmp   L_3ef0
L_3eed:
	incw   -0x10(%bp)
L_3ef0:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_43ce
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_3f25
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_3f25:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_3f30
	incw   -0x1a(%bp)
L_3f30:
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
L_3f52:
	decw   -0xc(%bp)
	jle   L_3f6c
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_3f52
L_3f6c:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_3f89
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_3f9a
L_3f89:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_3f9a:
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
	jmp   L_3d83
L_3fcb:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3fd7
	movw   $0x6,-0x10(%bp)
L_3fd7:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4252
	add    $0xc,%sp
	add    $0x8,%si
L_3ff5:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_3ff5
	decw   -0x8(%bp)
	jmp   L_3d83
L_4006:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_400f:
	jmp   L_3e17
L_4012:
	cmp    $0x65,%ax
	jne   L_401a
	jmp   L_3edf
L_401a:
	jg   L_405b
	cmp    $0x4f,%ax
	jne   L_4024
	jmp   L_3c3a
L_4024:
	jg   L_403b
	cmp    $0x25,%ax
	je   L_4006
	cmp    $0x44,%ax
	jne   L_4033
	jmp   L_3c56
L_4033:
	cmp    $0x4c,%ax
L_4036:
	jne   L_403b
	jmp   L_3c13
L_403b:
	cmp    $0x55,%ax
	jne   L_4043
	jmp   L_3c9f
L_4043:
	cmp    $0x58,%ax
	jne   L_404b
	jmp   L_3c48
L_404b:
	cmp    $0x63,%ax
	jne   L_4053
L_4050:
	jmp   L_3d52
L_4053:
	cmp    $0x64,%ax
L_4056:
	jne   L_405b
	jmp   L_3c66
L_405b:
	cmp    $0x6f,%ax
	jne   L_4063
	jmp   L_3c41
L_4063:
	jg   L_407a
	cmp    $0x66,%ax
	jne   L_406d
	jmp   L_3e19
L_406d:
	cmp    $0x67,%ax
	jne   L_4075
	jmp   L_3fcb
L_4075:
	cmp    $0x6c,%ax
	je   L_4036
L_407a:
	cmp    $0x72,%ax
	jne   L_4082
	jmp   L_3e0f
L_4082:
	cmp    $0x73,%ax
	jne   L_408a
	jmp   L_3dd6
L_408a:
	cmp    $0x75,%ax
	jne   L_4092
	jmp   L_3caf
L_4092:
	cmp    $0x78,%ax
	jne   L_409a
	jmp   L_3c4f
L_409a:
	jmp   L_400f
L_409d:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_40c1
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_46ae
	add    $0x8,%sp
L_40c1:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
L_40c6:
	pop    %bp
	ret


L_40c8:
	call   L_4230


L_40cb:
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
	call   L_4230


L_40f4:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_4112
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_4112:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_411b:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_411b
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_412b:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_412b
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_40cb
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_416c
	jne   L_4161
	or     $0x0,%ax
	je   L_416c
L_4161:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_416c:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_4230


L_4174:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_4190
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_4190:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_41a7
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_41a7:
	push   %dx
	push   %ax
	call   L_40f4
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_41c1
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_41c1:
	ret
	call   L_4230


L_41c5:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_4174
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_40cb
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_4230


L_41fc:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_40f4
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_40cb
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_4230:
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


L_4252:
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
	call   L_43ce
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_428c
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_428c:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_4293:
	cmpw   $0x0,-0xa(%bp)
	jle   L_42ad
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_42ad
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_4293
L_42ad:
	cmpw   $0x0,-0x8(%bp)
	jl   L_42be
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_42ca
L_42be:
	cmpw   $0x0,-0x8(%bp)
	jge   L_42c8
	cmpw   $0xfffd,-0x8(%bp)
L_42c8:
	jge   L_4349
L_42ca:
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
L_42e5:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_42ff
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_42e5
L_42ff:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_431b
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_4321
L_431b:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_4321:
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
	jmp   L_43b9
L_4349:
	cmpw   $0x0,-0x8(%bp)
	jg   L_436b
	cmpb   $0x30,(%di)
	je   L_435a
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_435a:
	cmpw   $0x0,-0x8(%bp)
	jge   L_436b
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_435a
L_436b:
	movw   $0x1,-0xa(%bp)
L_4370:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_4398
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
	jne   L_4393
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_4393:
	incw   -0xa(%bp)
	jmp   L_4370
L_4398:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_43b9
L_43a0:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_43b3
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_43a0
L_43b3:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_43b9:
	cmpb   $0x2e,-0x1(%si)
	jne   L_43c0
	dec    %si
L_43c0:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_43ce:
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
	call   L_4425
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_43fa:
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
	call   L_4425
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4425:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_4438
	movw   $0x0,0xc(%bp)
L_4438:
	cmpw   $0x4f,0xc(%bp)
	jl   L_4443
	movw   $0x4e,0xc(%bp)
L_4443:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_2076,%si
	fldl   0x4(%bp)
	fcompl D_20c6
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_4480
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_4480:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4d4c
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $D_20c6,-0x16(%bp)
	fldl   -0xc(%bp)
	fcompl D_20ce
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_44cb
	jmp   L_4578
L_44cb:
	movw   $D_20c6,-0x16(%bp)
L_44d0:
	fldl   -0xc(%bp)
	fcompl D_20d6
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_44ec
	jmp   L_4576
L_44ec:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	fdivl  D_20de
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4d4c
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   D_20e6
	fstl   -0x1e(%bp)
	fdivl  D_20ee
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  D_20f6
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
	jmp   L_44d0
L_455f:
	cmpw   $D_20c6,-0x16(%bp)
	jae   L_45b9
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_4576:
	jmp   L_455f
L_4578:
	fldl   0x4(%bp)
	fcompl D_20fe
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_45c8
L_4591:
	fldl   0x4(%bp)
	fmull  D_2106
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl D_210e
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_45b9:
	jae   L_45c8
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_4591
L_45c8:
	mov    0xc(%bp),%ax
	add    $D_2076,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_45da
	add    %di,-0x16(%bp)
L_45da:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_2076,-0x16(%bp)
	jae   L_45f0
	movb   $0x0,D_2076
	mov    $D_2076,%ax
	jmp   L_465b
L_45f0:
	cmp    -0x16(%bp),%si
	ja   L_464c
	cmp    $D_20c6,%si
	jae   L_464c
	fldl   0x4(%bp)
	fmull  D_2116
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4d4c
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
	jmp   L_45f0
L_464c:
	cmpw   $D_20c6,-0x16(%bp)
	jb   L_465d
	movb   $0x0,D_20c5
	mov    $D_2076,%ax
L_465b:
	jmp   L_46a7
L_465d:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_4665:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_46a1
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_2076,-0x16(%bp)
	jbe   L_4684
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_4665
L_4684:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_4665
	cmp    $D_2076,%si
	jbe   L_469e
	movb   $0x30,(%si)
L_469e:
	inc    %si
	jmp   L_4665
L_46a1:
	movb   $0x0,(%si)
	mov    $D_2076,%ax
L_46a7:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_46ae:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_46bb:
	cmpw   $0x0,0x8(%bp)
	jl   L_46c4
	jmp   L_4752
L_46c4:
	cmpb   $0x2d,(%di)
	jne   L_4701
	cmpw   $0x30,0xc(%bp)
	jne   L_4701
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_46f0
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
	jmp   L_4700
L_46f0:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_47a0
	add    $0x4,%sp
L_4700:
	dec    %si
L_4701:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_471f
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
	jmp   L_472b
L_471f:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_47a0
	add    $0x4,%sp
L_472b:
	incw   0x8(%bp)
	jmp   L_46bb
L_4730:
	dec    %si
	jl   L_4766
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_4754
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
L_4752:
	jmp   L_4730
L_4754:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_47a0
	add    $0x4,%sp
	jmp   L_4730
L_4766:
	cmpw   $0x0,0x8(%bp)
	je   L_479b
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_478a
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
	jmp   L_4796
L_478a:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_47a0
	add    $0x4,%sp
L_4796:
	decw   0x8(%bp)
	jmp   L_4766
L_479b:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_47a0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_47bb
	mov    $0xffff,%ax
	jmp   L_4882
L_47bb:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_47ee
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
	call   L_4d32
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_4873
L_47ee:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_4840
	cmp    $D_2126,%di
	jne   L_4819
	mov    D_212d,%al
	cbtw
	push   %ax
	call   L_4c1e
	pop    %cx
	or     %ax,%ax
	je   L_480e
	orb    $0x4,0x6(%di)
	jmp   L_47bb
L_480e:
	movw   $D_2410,0x4(%di)
	movw   $D_2410,(%di)
	jmp   L_47bb
L_4819:
	mov    $0x200,%ax
	push   %ax
	call   L_4974
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_4830
	orb    $0x4,0x6(%di)
	jmp   L_47bb
L_4830:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_4862
L_4840:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_4862
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4d32
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_4862:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_4873:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_4884
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_4882:
	jmp   L_4887
L_4884:
	mov    0x4(%bp),%ax
L_4887:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_488e:
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
	jne   L_48da
	mov    0x4(%di),%si
	or     %si,%si
	je   L_48da
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_48da
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4d32
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_48da
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_48dc
L_48da:
	xor    %ax,%ax
L_48dc:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_48e1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_211e,%di
L_48e9:
	cmp    D_21be,%di
	jae   L_48f9
	push   %di
	call   L_48fd
	pop    %cx
	add    $0x8,%di
	jmp   L_48e9
L_48f9:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_48fd:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_4950
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_4950
	push   %di
	call   L_488e
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4c0a
	pop    %cx
	or     %ax,%ax
	jge   L_4932
	mov    $0xffff,%si
L_4932:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_4942
	pushw  0x4(%di)
	call   L_4abe
	pop    %cx
L_4942:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_4950
	movw   $0x0,0x4(%di)
L_4950:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_4974:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_21c0
	jne   L_499f
	mov    $D_21c2,%ax
	or     $0x1,%ax
	mov    %ax,D_21c0
	mov    $D_21c0,%ax
	or     $0x1,%ax
	mov    %ax,D_21c2
	/* dual-reloc split: movw $D_21c2,D_2812 */
	mov	$D_21c2,%ax
	mov	%ax,D_2812
	/* dual-reloc split: movw $D_21c0,D_2810 */
	mov	$D_21c0,%ax
	mov	%ax,D_2810
L_499f:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_2810,%di
L_49b3:
	movw   $0x0,D_21c4
L_49b9:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_49e8
L_49c0:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_49cf
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_49c0
L_49cf:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_49e8
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_49e8
	jmp   L_4a8f
L_49e8:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_49b9
	cmp    D_2812,%si
	jne   L_4a01
	cmp    $D_21c0,%di
	je   L_4a05
L_4a01:
	xor    %ax,%ax
	jmp   L_4a3d
L_4a05:
	incw   D_21c4
	cmpw   $0x1,D_21c4
	jle   L_49b9
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_21c4
	xor    %ax,%ax
	push   %ax
	call   L_4ba2
	pop    %cx
	mov    %ax,%si
	mov    D_21c4,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_4a3f
	xor    %ax,%ax
L_4a3d:
	jmp   L_4a52
L_4a3f:
	mov    D_21c4,%ax
	shl    %ax
	push   %ax
	call   L_4ba2
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_4a54
	xor    %ax,%ax
L_4a52:
	jmp   L_4ab9
L_4a54:
	mov    D_2812,%bx
	mov    %si,(%bx)
	mov    D_2812,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_4a72
	mov    D_2812,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_2812,%bx
	mov    %ax,(%bx)
L_4a72:
	mov    D_21c4,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_2812
	mov    $D_21c0,%ax
	or     $0x1,%ax
	mov    D_2812,%bx
	mov    %ax,(%bx)
	jmp   L_49b3
L_4a8f:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_2810
	cmp    %ax,%si
	jbe   L_4aae
	mov    D_2810,%bx
	mov    (%bx),%ax
	mov    %ax,D_2814
	mov    D_2810,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_4aae:
	mov    D_2810,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_4ab9:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4abe:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_2810
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
	je   L_4af3
	push   %di
	call   L_4abe
	pop    %cx
L_4af3:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_4974
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_4b12
	cmp    %di,%si
	jne   L_4b16
L_4b12:
	mov    %si,%ax
	jmp   L_4b7e
L_4b16:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_4b35
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_4b35:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_4b53
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_4b35
L_4b53:
	cmp    %di,%si
	jae   L_4b7c
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_4b7c
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
	mov    D_2814,%ax
	mov    %ax,(%bx)
L_4b7c:
	mov    %si,%ax
L_4b7e:
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
	jns    L_4b99
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_4ba0
L_4b99:
	mov    0x4(%bp),%bx
	mov    %bx,D_21c6
L_4ba0:
	pop    %bp
	ret


L_4ba2:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_21c6,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_4bb9
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_4bc6
L_4bb9:
	mov    D_21c6,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_21c6
L_4bc6:
	pop    %bp
	ret


L_4bc8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_4bd5:
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_4bf4
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_4bd5
	xor    %ax,%ax
	jmp   L_4c05
L_4bf4:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_4c05:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4c0a:
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
	jns    L_4c1b
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4c1b:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4c1e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4ccf
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_4c3b
	xor    %ax,%ax
	jmp   L_4c3e
L_4c3b:
	mov    $0x1,%ax
L_4c3e:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4c46:
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
	jns    L_4c5a
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4c5a:
	pop    %bp
	ret


L_4c5c:
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
	jns    L_4c73
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4c73:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4c76:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall stat(18) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* stat -> __NR_stat */
	xchg	%ax,%bx
	mov	%dx,%cx
	mov	$__NR_stat,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_4c8a
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4c8a:
	pop    %bp
	ret


L_4c8c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x6(%bp)
L_4c9d:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_4c9d
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
	call   L_4ce8
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4ccf:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4ce8
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4ce8:
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
	jns    L_4cff
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4cff:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4d02:
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
	jns    L_4d13
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4d13:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4d16:
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
	jns    L_4d26
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4d24:
	pop    %bp
	ret
L_4d26:
	mov    0x4(%bp),%bx
	cmp    $0x0,%bx
	je   L_4d24
	mov    %dx,(%bx)
	jmp   L_4d24


L_4d32:
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
	jns    L_4d49
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4d49:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4d4c:
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


L_4d68:
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
	jns    L_4d79
	neg	%ax
	mov	%ax,D_23c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4d79:
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
	mov	%ax,D_23c6	/* errno */
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
	.byte	0xbf, 0x0f, 0x06, 0x20, 0x0a, 0x4d	/* |... .M| */
D_0008:
	.byte	0x41, 0x4b, 0x45, 0x2e, 0x20, 0x20, 0x56, 0x45	/* |AKE.  VE| */
	.byte	0x52, 0x53, 0x49, 0x4f, 0x4e, 0x20, 0x32, 0x2e	/* |RSION 2.| */
	.byte	0x35, 0x38, 0x20, 0x20, 0x20, 0x20, 0x20, 0x31	/* |58     1| */
D_0020:
	.byte	0x34, 0x20, 0x4d, 0x41, 0x52, 0x43, 0x48, 0x20	/* |4 MARCH | */
	.byte	0x31, 0x39, 0x37, 0x39, 0x0a, 0x00	/* |1979..| */
D_002e:
	.byte	0x00, 0x00	/* |..| */
D_0030:
	.byte	0x00, 0x00	/* |..| */
D_0032:
	.byte	0x00, 0x00	/* |..| */
D_0034:
	.byte	0x00, 0x00	/* |..| */
D_0036:
	.byte	0x00, 0x00	/* |..| */
D_0038:
	.byte	0x00, 0x00	/* |..| */
D_003a:
	.byte	0x00, 0x00	/* |..| */
D_003c:
	.byte	0x00, 0x00	/* |..| */
D_003e:
	.byte	0x00, 0x00	/* |..| */
D_0040:
	.byte	0x00, 0x00	/* |..| */
D_0042:
	.byte	0x00, 0x00	/* |..| */
D_0044:
	.byte	0x00, 0x00	/* |..| */
D_0046:
	.byte	0x00, 0x00	/* |..| */
D_0048:
	.byte	0x01, 0x00	/* |..| */
D_004a:
	.byte	0x96, 0x20	/* |. | */
D_004c:
	.byte	0x58, 0x00, 0x7e, 0x01, 0xff, 0x01, 0xff, 0x01	/* |X.~.....| */
	.byte	0xff, 0x01, 0xff, 0x01, 0xff, 0x01, 0xff, 0x01	/* |........| */
	.byte	0xff, 0x01, 0xff, 0x01, 0xff, 0x01, 0xff, 0x01	/* |........| */
	.byte	0xff, 0x01, 0xff, 0x01, 0xff, 0x01, 0xff, 0x01	/* |........| */
	.byte	0xff, 0x01, 0xff, 0x01, 0x5e, 0x01, 0xff, 0x01	/* |....^...| */
	.byte	0xae, 0x01, 0xff, 0x01, 0xff, 0x01, 0x76, 0x01	/* |......v.| */
	.byte	0xff, 0x01, 0x86, 0x01, 0xff, 0x01, 0xff, 0x01	/* |........| */
	.byte	0x8e, 0x01, 0xff, 0x01, 0x66, 0x01, 0xa6, 0x01	/* |....f...| */
	.byte	0x96, 0x01, 0x6e, 0x01, 0x9e, 0x01	/* |..n...| */
D_0092:
	.byte	0xda, 0x2d	/* |.-| */
D_0094:
	.byte	0x00, 0x00, 0x00	/* |...| */
D_0097:
	.ascii	"/usr/sif/make/Meter\0"
D_00ab:
	.byte	0x3d, 0x7c, 0x5e, 0x28, 0x29, 0x3b, 0x26, 0x3c	/* |=|^();&<| */
	.byte	0x3e, 0x2a, 0x3f, 0x5b, 0x5d, 0x3a, 0x24, 0x60	/* |>*?[]:$`| */
	.byte	0x27, 0x22, 0x5c, 0x0a, 0x00	/* |'"\..| */
D_00c0:
	.byte	0x0a, 0x09	/* |..| */
	.ascii	" :;&>|\0"
D_00c9:
	.byte	0x24, 0x00	/* |$.| */
D_00cb:
	.byte	0x24, 0x00	/* |$.| */
D_00cd:
	.ascii	"No description argument after -f flag\0"
D_00f3:
	.ascii	"Cannot open %s\0"
D_0102:
	.ascii	"Unknown flag argument %s\0"
D_011b:
	.ascii	"makefile\0"
D_0124:
	.ascii	"Makefile\0"
D_012d:
	.ascii	".IGNORE\0"
D_0135:
	.ascii	".SILENT\0"
D_013d:
	.ascii	".SUFFIXES\0"
D_0147:
	.byte	0x4e, 0x6f, 0x20, 0x73, 0x75, 0x66, 0x66, 0x69	/* |No suffi| */
	.byte	0x78, 0x20, 0x6c, 0x69, 0x73, 0x74, 0x2e, 0x0a	/* |x list..| */
	.byte	0x00	/* |.| */
D_0158:
	.ascii	"No arguments or description file\0"
D_0179:
	.byte	0x40, 0x00	/* |@.| */
D_017b:
	.byte	0x0a	/* |.| */
	.ascii	"***  %s removed.\0"
D_018d:
	.byte	0x0a, 0x00	/* |..| */
D_018f:
	.ascii	".PRECIOUS\0"
D_0199:
	.byte	0x2d, 0x00	/* |-.| */
D_019b:
	.byte	0x72, 0x00	/* |r.| */
D_019d:
	.ascii	"Description file error\0"
D_01b4:
	.byte	0x4f, 0x70, 0x65, 0x6e, 0x20, 0x64, 0x69, 0x72	/* |Open dir| */
	.byte	0x65, 0x63, 0x74, 0x6f, 0x72, 0x69, 0x65, 0x73	/* |ectories| */
	.byte	0x3a, 0x0a, 0x00	/* |:..| */
D_01c7:
	.byte	0x09, 0x25, 0x64, 0x3a, 0x20, 0x25, 0x73, 0x0a	/* |.%d: %s.| */
	.byte	0x00	/* |.| */
D_01d0:
	.byte	0x4d, 0x61, 0x63, 0x72, 0x6f, 0x73, 0x3a, 0x0a	/* |Macros:.| */
	.byte	0x00	/* |.| */
D_01d9:
	.byte	0x09, 0x25, 0x73, 0x20, 0x3d, 0x20, 0x25, 0x73	/* |.%s = %s| */
	.byte	0x0a, 0x00	/* |..| */
D_01e3:
	.byte	0x0a, 0x0a, 0x25, 0x73, 0x00	/* |..%s.| */
D_01e8:
	.word	D_003a	/* ptr: 0x003a */
D_01ea:
	.ascii	"  done=%d\0"
D_01f4:
	.ascii	"  (MAIN NAME)\0"
D_0202:
	.byte	0x0a	/* |.| */
	.ascii	" depends on:\0"
D_0210:
	.ascii	" %s \0"
D_0215:
	.byte	0x0a, 0x20, 0x63, 0x6f, 0x6d, 0x6d, 0x61, 0x6e	/* |. comman| */
	.byte	0x64, 0x73, 0x3a, 0x0a, 0x00	/* |ds:..| */
D_0222:
	.byte	0x09, 0x25, 0x73, 0x0a, 0x00	/* |.%s..| */
D_0227:
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_022a:
	.byte	0x64, 0x6f, 0x6e, 0x61, 0x6d, 0x65, 0x28, 0x25	/* |doname(%| */
	.byte	0x73, 0x2c, 0x25, 0x64, 0x29, 0x0a, 0x00	/* |s,%d)..| */
D_0239:
	.byte	0x54, 0x49, 0x4d, 0x45, 0x28, 0x25, 0x73, 0x29	/* |TIME(%s)| */
	.byte	0x3d, 0x25, 0x6c, 0x64, 0x0a, 0x00	/* |=%ld..| */
D_0247:
	.byte	0x40, 0x00	/* |@.| */
D_0249:
	.byte	0x3f, 0x00	/* |?.| */
D_024b:
	.byte	0x40, 0x00	/* |@.| */
D_024d:
	.byte	0x54, 0x6f, 0x6f, 0x20, 0x6d, 0x61, 0x6e, 0x79	/* |Too many| */
	.byte	0x20, 0x63, 0x6f, 0x6d, 0x6d, 0x61, 0x6e, 0x64	/* | command| */
	.byte	0x20, 0x6c, 0x69, 0x6e, 0x65, 0x73, 0x20, 0x66	/* | lines f| */
	.byte	0x6f, 0x72, 0x20, 0x60, 0x25, 0x73, 0x27, 0x0a	/* |or `%s'.| */
	.byte	0x00	/* |.| */
D_026e:
	.byte	0x2a, 0x00	/* |*.| */
D_0270:
	.byte	0x54, 0x49, 0x4d, 0x45, 0x28, 0x25, 0x73, 0x29	/* |TIME(%s)| */
	.byte	0x3d, 0x25, 0x6c, 0x64, 0x0a, 0x00	/* |=%ld..| */
D_027e:
	.byte	0x2a, 0x00	/* |*.| */
D_0280:
	.word	D_003c	/* ptr: 0x003c */
D_0282:
	.word	D_0040	/* ptr: 0x0040 */
D_0284:
	.byte	0x3f, 0x00	/* |?.| */
D_0286:
	.ascii	".DEFAULT\0"
D_028f:
	.byte	0x3c, 0x00	/* |<.| */
D_0291:
	.byte	0x44, 0x6f, 0x6e, 0x27, 0x74, 0x20, 0x6b, 0x6e	/* |Don't kn| */
	.byte	0x6f, 0x77, 0x20, 0x68, 0x6f, 0x77, 0x20, 0x74	/* |ow how t| */
	.byte	0x6f, 0x20, 0x6d, 0x61, 0x6b, 0x65, 0x20, 0x25	/* |o make %| */
	.byte	0x73, 0x0a, 0x00	/* |s..| */
D_02ac:
	.ascii	" Don't know how to make %s\0"
D_02c7:
	.byte	0x40, 0x00	/* |@.| */
D_02c9:
	.byte	0x60, 0x25, 0x73, 0x27, 0x20, 0x6e, 0x6f, 0x74	/* |`%s' not| */
	.byte	0x20, 0x72, 0x65, 0x6d, 0x61, 0x64, 0x65, 0x20	/* | remade | */
	.byte	0x62, 0x65, 0x63, 0x61, 0x75, 0x73, 0x65, 0x20	/* |because | */
	.byte	0x6f, 0x66, 0x20, 0x65, 0x72, 0x72, 0x6f, 0x72	/* |of error| */
	.byte	0x73, 0x0a, 0x00	/* |s..| */
D_02ec:
	.byte	0x60, 0x25, 0x73, 0x27, 0x20, 0x69, 0x73, 0x20	/* |`%s' is | */
	.byte	0x75, 0x70, 0x20, 0x74, 0x6f, 0x20, 0x64, 0x61	/* |up to da| */
	.byte	0x74, 0x65, 0x2e, 0x0a, 0x00	/* |te...| */
D_0301:
	.byte	0x40, 0x00	/* |@.| */
D_0303:
	.byte	0x74, 0x6f, 0x75, 0x63, 0x68, 0x28, 0x25, 0x73	/* |touch(%s| */
	.byte	0x29, 0x0a, 0x00	/* |)..| */
D_030e:
	.byte	0x25, 0x73, 0x25, 0x73, 0x0a, 0x00	/* |%s%s..| */
D_0314:
	.byte	0x00	/* |.| */
D_0315:
	.ascii	"*** Error code %d\0"
D_0327:
	.ascii	"*** Termination code %d\0"
D_033f:
	.byte	0x20, 0x28, 0x69, 0x67, 0x6e, 0x6f, 0x72, 0x65	/* | (ignore| */
	.byte	0x64, 0x29, 0x0a, 0x00	/* |d)..| */
D_034b:
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_034e:
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
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
D_0748:
	.byte	0x00, 0x00	/* |..| */
D_074a:
	.byte	0x00, 0x00	/* |..| */
D_074c:
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
	.byte	0x00	/* |.| */
D_0d25:
	.byte	0x00, 0x00, 0x00	/* |...| */
D_0d28:
	.ascii	"Hash table overflow\0"
D_0d3c:
	.ascii	"out of memory\0"
D_0d4a:
	.ascii	"out of memory\0"
D_0d58:
	.ascii	"infinitely recursive macro?\0"
D_0d74:
	.word	D_0040	/* ptr: 0x0040 */
D_0d76:
	.byte	0x2a, 0x00	/* |*.| */
D_0d78:
	.word	D_003c	/* ptr: 0x003c */
D_0d7a:
	.byte	0x3f, 0x00	/* |?.| */
D_0d7c:
	.byte	0x57, 0x61, 0x72, 0x6e, 0x69, 0x6e, 0x67, 0x3a	/* |Warning:| */
	.byte	0x20, 0x25, 0x73, 0x20, 0x63, 0x68, 0x61, 0x6e	/* | %s chan| */
	.byte	0x67, 0x65, 0x64, 0x20, 0x61, 0x66, 0x74, 0x65	/* |ged afte| */
	.byte	0x72, 0x20, 0x62, 0x65, 0x69, 0x6e, 0x67, 0x20	/* |r being | */
	.byte	0x75, 0x73, 0x65, 0x64, 0x0a, 0x00	/* |used..| */
D_0da2:
	.byte	0x4d, 0x61, 0x6b, 0x65, 0x3a, 0x20, 0x25, 0x73	/* |Make: %s| */
	.byte	0x2e, 0x20, 0x20, 0x53, 0x74, 0x6f, 0x70, 0x2e	/* |.  Stop.| */
	.byte	0x0a, 0x00	/* |..| */
D_0db4:
	.byte	0x0a, 0x53, 0x74, 0x6f, 0x70, 0x2e, 0x0a, 0x00	/* |.Stop...| */
D_0dbc:
	.ascii	"line %d: %s\0"
D_0dc8:
	.byte	0x24, 0x3f, 0x20, 0x6c, 0x69, 0x73, 0x74, 0x20	/* |$? list | */
	.byte	0x74, 0x6f, 0x6f, 0x20, 0x6c, 0x6f, 0x6e, 0x67	/* |too long| */
	.byte	0x0a, 0x00, 0xdc, 0x2e	/* |....| */
	.word	D_0f0d	/* ptr: 0x2f0d */
	.word	D_0f17	/* ptr: 0x2f17 */
	.word	D_0f25	/* ptr: 0x2f25 */
	.word	D_0f33	/* ptr: 0x2f33 */
	.word	D_0f3b	/* ptr: 0x2f3b */
	.word	D_0f43	/* ptr: 0x2f43 */
	.word	D_0f4b	/* ptr: 0x2f4b */
	.word	D_0f51	/* ptr: 0x2f51 */
	.word	D_0f59	/* ptr: 0x2f59 */
	.word	D_0f61	/* ptr: 0x2f61 */
	.word	D_0f68	/* ptr: 0x2f68 */
	.word	D_0f70	/* ptr: 0x2f70 */
	.word	D_0f77	/* ptr: 0x2f77 */
	.word	D_0f7f	/* ptr: 0x2f7f */
	.word	D_0f87	/* ptr: 0x2f87 */
	.word	D_0f92	/* ptr: 0x2f92 */
	.byte	0x99, 0x2f	/* |./| */
	.word	D_0fb0	/* ptr: 0x2fb0 */
	.byte	0xc1, 0x2f	/* |./| */
	.word	D_0fec	/* ptr: 0x2fec */
	.byte	0xf3, 0x2f	/* |./| */
	.word	D_1003	/* ptr: 0x3003 */
	.byte	0x0a, 0x30, 0x20, 0x30, 0x3c, 0x30, 0x48, 0x30	/* |.0 0<0H0| */
	.word	D_1057	/* ptr: 0x3057 */
	.byte	0x5e, 0x30, 0x75, 0x30, 0x91, 0x30, 0x9d, 0x30	/* |^0u0.0.0| */
	.word	D_10ac	/* ptr: 0x30ac */
	.byte	0xb4, 0x30, 0xcb, 0x30, 0xe7, 0x30, 0xf3, 0x30	/* |.0.0.0.0| */
	.word	D_1102	/* ptr: 0x3102 */
	.byte	0x09, 0x31, 0x1e, 0x31, 0x3b, 0x31, 0x48, 0x31	/* |.1.1;1H1| */
	.word	D_1158	/* ptr: 0x3158 */
	.byte	0x5f, 0x31, 0x75, 0x31	/* |_1u1| */
	.word	D_1184	/* ptr: 0x3184 */
	.byte	0x8b, 0x31, 0x96, 0x31	/* |.1.1| */
	.word	D_11a6	/* ptr: 0x31a6 */
	.byte	0xad, 0x31, 0xc4, 0x31	/* |.1.1| */
	.word	D_11d3	/* ptr: 0x31d3 */
	.byte	0xdb, 0x31, 0xf2, 0x31	/* |.1.1| */
	.word	D_1201	/* ptr: 0x3201 */
	.byte	0x18, 0x32	/* |.2| */
	.word	D_123f	/* ptr: 0x323f */
	.byte	0x56, 0x32, 0x91, 0x32	/* |V2.2| */
	.word	D_129b	/* ptr: 0x329b */
	.byte	0xa4, 0x32, 0xba, 0x32, 0xea, 0x32	/* |.2.2.2| */
	.word	D_12f6	/* ptr: 0x32f6 */
	.byte	0xff, 0x32, 0x0a, 0x33, 0x3b, 0x33, 0x00, 0x00	/* |.2.3;3..| */
D_0e62:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0e72:
	.byte	0x70, 0x2e	/* |p.| */
D_0e74:
	.byte	0x00, 0x00	/* |..| */
D_0e76:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0e86:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0ea6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_0eb2:
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_0eb6:
	.byte	0x00, 0x00	/* |..| */
D_0eb8:
	.byte	0x00, 0x00	/* |..| */
D_0eba:
	.byte	0x00, 0x00	/* |..| */
D_0ebc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_0ec2:
	.byte	0x00, 0x00	/* |..| */
D_0ec4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_0eca:
	.byte	0x00, 0x00	/* |..| */
D_0ecc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_0ed2:
	.byte	0x00, 0x00	/* |..| */
D_0ed4:
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_0ed8:
	.byte	0x00, 0x00	/* |..| */
D_0eda:
	.byte	0x00, 0x00, 0x2e, 0x53, 0x55, 0x46, 0x46, 0x49	/* |...SUFFI| */
	.byte	0x58, 0x45, 0x53, 0x20, 0x3a, 0x20, 0x2e, 0x6f	/* |XES : .o| */
	.byte	0x75, 0x74, 0x20, 0x2e, 0x6f, 0x20, 0x2e, 0x63	/* |ut .o .c| */
	.byte	0x20, 0x2e, 0x66, 0x20, 0x2e, 0x65, 0x20, 0x2e	/* | .f .e .| */
	.byte	0x72, 0x20, 0x2e, 0x79, 0x20, 0x2e	/* |r .y .| */
D_0f00:
	.ascii	"yr .ye .l .s\0"
D_0f0d:
	.ascii	"YACC=yacc\0"
D_0f17:
	.ascii	"YACCR=yacc -r\0"
D_0f25:
	.ascii	"YACCE=yacc -e\0"
D_0f33:
	.ascii	"YFLAGS=\0"
D_0f3b:
	.ascii	"LEX=lex\0"
D_0f43:
	.ascii	"LFLAGS=\0"
D_0f4b:
	.ascii	"CC=cc\0"
D_0f51:
	.ascii	"AS=as -\0"
D_0f59:
	.ascii	"CFLAGS=\0"
D_0f61:
	.ascii	"RC=f77\0"
D_0f68:
	.ascii	"RFLAGS=\0"
D_0f70:
	.ascii	"EC=f77\0"
D_0f77:
	.ascii	"EFLAGS=\0"
D_0f7f:
	.ascii	"FFLAGS=\0"
D_0f87:
	.ascii	"LOADLIBES=\0"
D_0f92:
	.ascii	".c.o :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(CC) $(CFLAGS) -c $<\0"
D_0fb0:
	.ascii	".e.o .r.o .f.o :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(EC) $(RFLAGS) $(EFLAGS) $(FFLAGS) -c $<\0"
D_0fec:
	.ascii	".s.o :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(AS) -o $@ $<\0"
D_1003:
	.ascii	".y.o :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(YACC) $(YFLAGS) $<\0"
	.byte	0x09	/* |.| */
	.ascii	"$(CC) $(CFLAGS) -c y.tab.c\0"
	.byte	0x09	/* |.| */
	.ascii	"rm y.tab.c\0"
	.byte	0x09	/* |.| */
	.ascii	"mv y.tab.o $@\0"
D_1057:
	.ascii	".yr.o:\0"
	.byte	0x09	/* |.| */
	.ascii	"$(YACCR) $(YFLAGS) $<\0"
	.byte	0x09	/* |.| */
	.ascii	"$(RC) $(RFLAGS) -c y.tab.r\0"
	.byte	0x09	/* |.| */
	.ascii	"rm y.tab.r\0"
	.byte	0x09	/* |.| */
	.ascii	"mv y.tab.o $@\0"
D_10ac:
	.ascii	".ye.o :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(YACCE) $(YFLAGS) $<\0"
	.byte	0x09	/* |.| */
	.ascii	"$(EC) $(RFLAGS) -c y.tab.e\0"
	.byte	0x09	/* |.| */
	.ascii	"rm y.tab.e\0"
	.byte	0x09	/* |.| */
	.ascii	"mv y.tab.o $@\0"
D_1102:
	.ascii	".l.o :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(LEX) $(LFLAGS) $<\0"
	.byte	0x09	/* |.| */
	.ascii	"$(CC) $(CFLAGS) -c lex.yy.c\0"
	.byte	0x09	/* |.| */
	.ascii	"rm lex.yy.c\0"
	.byte	0x09	/* |.| */
	.ascii	"mv lex.yy.o $@\0"
D_1158:
	.ascii	".y.c :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(YACC) $(YFLAGS) $<\0"
	.byte	0x09	/* |.| */
	.ascii	"mv y.tab.c $@\0"
D_1184:
	.ascii	".l.c :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(LEX) $<\0"
	.byte	0x09	/* |.| */
	.ascii	"mv lex.yy.c $@\0"
D_11a6:
	.ascii	".yr.r:\0"
	.byte	0x09	/* |.| */
	.ascii	"$(YACCR) $(YFLAGS) $<\0"
	.byte	0x09	/* |.| */
	.ascii	"mv y.tab.r $@\0"
D_11d3:
	.ascii	".ye.e :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(YACCE) $(YFLAGS) $<\0"
	.byte	0x09	/* |.| */
	.ascii	"mv y.tab.e $@\0"
D_1201:
	.ascii	".s.out .c.out .o.out :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(CC) $(CFLAGS) $< $(LOADLIBES) -o $@\0"
D_123f:
	.ascii	".f.out .r.out .e.out :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(EC) $(EFLAGS) $(RFLAGS) $(FFLAGS) $< $(LOADLIBES) -o $@\0"
	.byte	0x09	/* |.| */
	.ascii	"-rm $*.o\0"
D_129b:
	.ascii	".y.out :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(YACC) $(YFLAGS) $<\0"
	.byte	0x09	/* |.| */
	.ascii	"$(CC) $(CFLAGS) y.tab.c $(LOADLIBES) -ly -o $@\0"
	.byte	0x09	/* |.| */
	.ascii	"rm y.tab.c\0"
D_12f6:
	.ascii	".l.out :\0"
	.byte	0x09	/* |.| */
	.ascii	"$(LEX) $<\0"
	.byte	0x09	/* |.| */
	.ascii	"$(CC) $(CFLAGS) lex.yy.c $(LOADLIBES) -ll -o $@\0"
	.byte	0x09	/* |.| */
	.ascii	"rm lex.yy.c\0"
D_1348:
	.word	D_002e	/* ptr: 0x002e */
D_134a:
	.byte	0x00	/* |.| */
D_134b:
	.byte	0x2f, 0x00	/* |/.| */
D_134d:
	.byte	0x72, 0x00	/* |r.| */
D_134f:
	.ascii	"Directory %s: \0"
D_135e:
	.ascii	"Cannot open\0"
D_136a:
	.byte	0x61, 0x00	/* |a.| */
D_136c:
	.byte	0x55, 0x73, 0x65, 0x72, 0x20, 0x25, 0x73, 0x2c	/* |User %s,| */
	.byte	0x20, 0x25, 0x73, 0x0a, 0x00	/* | %s..| */
D_1379:
	.byte	0x72, 0x00	/* |r.| */
D_137b:
	.ascii	"cannot open %s\0"
D_138a:
	.byte	0x21, 0x3c, 0x61, 0x72, 0x63, 0x68, 0x3e, 0x0a	/* |!<arch>.| */
	.byte	0x00	/* |.| */
D_1393:
	.ascii	"%s is not an archive\0"
D_13a8:
	.ascii	"%-10ld\0"
D_13af:
	.ascii	"%s is not an object module\0"
D_13ca:
	.ascii	"/bin/sh\0"
D_13d2:
	.byte	0x73, 0x68, 0x00	/* |sh.| */
D_13d5:
	.byte	0x2d, 0x63, 0x00	/* |-c.| */
D_13d8:
	.byte	0x2d, 0x63, 0x65, 0x00	/* |-ce.| */
D_13dc:
	.ascii	"Couldn't load Shell\0"
D_13f0:
	.ascii	"bad wait code\0"
D_13fe:
	.ascii	"Cannot load %s\0"
D_140d:
	.byte	0x74, 0x6f, 0x75, 0x63, 0x68, 0x3a, 0x20, 0x66	/* |touch: f| */
	.byte	0x69, 0x6c, 0x65, 0x20, 0x25, 0x73, 0x20, 0x64	/* |ile %s d| */
	.byte	0x6f, 0x65, 0x73, 0x20, 0x6e, 0x6f, 0x74, 0x20	/* |oes not | */
	.byte	0x65, 0x78, 0x69, 0x73, 0x74, 0x2e, 0x0a, 0x00	/* |exist...| */
D_142d:
	.byte	0x43, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x74	/* |Cannot t| */
	.byte	0x6f, 0x75, 0x63, 0x68, 0x20, 0x25, 0x73, 0x0a	/* |ouch %s.| */
	.byte	0x00	/* |.| */
D_143e:
	.byte	0x00, 0x00	/* |..| */
D_1440:
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
D_1490:
	.byte	0x00, 0x00	/* |..| */
D_1492:
	.byte	0x00, 0x00	/* |..| */
D_1494:
	.byte	0x00, 0x00	/* |..| */
D_1496:
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
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |.......| */
D_1825:
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
	.byte	0x00	/* |.| */
D_196e:
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
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_1a72:
	.byte	0x72, 0x3a	/* |r:| */
D_1a74:
	.byte	0xff, 0xff, 0x01, 0x00, 0x00, 0x00, 0xff, 0xff	/* |........| */
	.byte	0xfe, 0xff, 0x00, 0x00	/* |....| */
D_1a80:
	.byte	0x09, 0x00, 0x0b, 0x00, 0x12, 0x00, 0x05, 0x00	/* |........| */
	.byte	0x0c, 0x00, 0x0d, 0x00, 0x03, 0x00, 0x04, 0x00	/* |........| */
	.byte	0x10, 0x00, 0x11, 0x00, 0x07, 0x00	/* |......| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x01, 0x00, 0x0a, 0x00	/* |....| */
	.word	D_0008	/* ptr: 0x0008 */
	.byte	0x06, 0x00, 0x0e, 0x00, 0x0f, 0x00	/* |......| */
D_1aa4:
	.byte	0x18, 0xfc, 0x03, 0xff, 0x18, 0xfc, 0x09, 0xff	/* |........| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0xff, 0xfe, 0x18, 0xfc	/* |........| */
	.byte	0x06, 0xff, 0x18, 0xfc, 0x08, 0xff, 0x18, 0xfc	/* |........| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc, 0x00, 0xff	/* |........| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc	/* |......| */
D_1aca:
	.byte	0x00, 0x00, 0x11, 0x00, 0x10, 0x00, 0x0f, 0x00	/* |........| */
	.byte	0x0e, 0x00, 0x0d, 0x00, 0x0c, 0x00, 0x0b, 0x00	/* |........| */
	.byte	0x01, 0x00	/* |..| */
D_1adc:
	.byte	0x00, 0x00, 0x06, 0x00, 0x06, 0x00, 0x07, 0x00	/* |........| */
	.byte	0x07, 0x00, 0x07, 0x00, 0x07, 0x00, 0x03, 0x00	/* |........| */
	.byte	0x03, 0x00, 0x04, 0x00, 0x04, 0x00, 0x05, 0x00	/* |........| */
	.byte	0x05, 0x00	/* |..| */
	.word	D_0008	/* ptr: 0x0008 */
	.word	D_0008	/* ptr: 0x0008 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x01, 0x00, 0x01, 0x00	/* |....| */
D_1b02:
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x01, 0x00, 0x01, 0x00, 0x04, 0x00, 0x01, 0x00	/* |........| */
	.byte	0x01, 0x00	/* |..| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x00, 0x00, 0x01, 0x00, 0x01, 0x00	/* |......| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00	/* |........| */
	.byte	0x01, 0x00	/* |..| */
	.word	D_0002	/* ptr: 0x0002 */
D_1b28:
	.byte	0x18, 0xfc, 0xfa, 0xff, 0xf9, 0xff, 0x03, 0x01	/* |........| */
	.byte	0x04, 0x01, 0x00, 0x01, 0xfd, 0xff, 0x01, 0x01	/* |........| */
	.byte	0xfc, 0xff, 0x01, 0x01, 0xfb, 0xff, 0xf8, 0xff	/* |........| */
	.byte	0x05, 0x01, 0x06, 0x01, 0xfe, 0xff, 0xff, 0xff	/* |........| */
	.word	D_0102	/* ptr: 0x0102 */
	.byte	0x01, 0x01	/* |..| */
	.word	D_0102	/* ptr: 0x0102 */
D_1b4e:
	.byte	0x01, 0x00, 0xfe, 0xff	/* |....| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x03, 0x00, 0x04, 0x00, 0x06, 0x00, 0x09, 0x00	/* |........| */
	.byte	0x07, 0x00, 0x0f, 0x00	/* |....| */
	.word	D_0008	/* ptr: 0x0008 */
	.byte	0x0a, 0x00, 0x0b, 0x00, 0x0d, 0x00, 0x0e, 0x00	/* |........| */
	.byte	0x05, 0x00, 0x10, 0x00, 0x11, 0x00, 0x0c, 0x00	/* |........| */
	.byte	0x12, 0x00	/* |..| */
D_1b74:
	.byte	0x00, 0x00	/* |..| */
D_1b76:
	.byte	0xff, 0xff	/* |..| */
D_1b78:
	.byte	0x00, 0x00	/* |..| */
D_1b7a:
	.byte	0x00, 0x00, 0xf8, 0x24, 0x04, 0x25, 0x04, 0x25	/* |...$.%.%| */
	.byte	0x9e, 0x25, 0x7c, 0x26, 0xca, 0x28, 0xe0, 0x27	/* |.%|&.(.'| */
	.byte	0xee, 0x27, 0x0f, 0x28, 0xca, 0x28, 0x33, 0x28	/* |.'.(.(3(| */
	.byte	0x41, 0x28, 0x7b, 0x28, 0x83, 0x28, 0x8b, 0x28	/* |A({(.(.(| */
	.byte	0x93, 0x28, 0x9a, 0x28, 0xa6, 0x28	/* |.(.(.(| */
D_1ba0:
	.ascii	"Bad character %c (octal %o), line %d\0"
D_1bc5:
	.ascii	"line too long\0"
D_1bd3:
	.byte	0x73, 0x74, 0x61, 0x74, 0x65, 0x20, 0x25, 0x64	/* |state %d| */
	.byte	0x2c, 0x20, 0x63, 0x68, 0x61, 0x72, 0x20, 0x30	/* |, char 0| */
	.byte	0x25, 0x6f, 0x0a, 0x00	/* |%o..| */
D_1be7:
	.ascii	"yacc stack overflow\0"
D_1bfb:
	.ascii	"syntax error\0"
D_1c08:
	.byte	0x65, 0x72, 0x72, 0x6f, 0x72, 0x20, 0x72, 0x65	/* |error re| */
	.byte	0x63, 0x6f, 0x76, 0x65, 0x72, 0x79, 0x20, 0x70	/* |covery p| */
	.byte	0x6f, 0x70, 0x73, 0x20, 0x73, 0x74, 0x61, 0x74	/* |ops stat| */
	.byte	0x65, 0x20, 0x25, 0x64, 0x2c, 0x20, 0x75, 0x6e	/* |e %d, un| */
	.byte	0x63, 0x6f, 0x76, 0x65, 0x72, 0x73, 0x20, 0x25	/* |covers %| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_1c33:
	.byte	0x65, 0x72, 0x72, 0x6f, 0x72, 0x20, 0x72, 0x65	/* |error re| */
	.byte	0x63, 0x6f, 0x76, 0x65, 0x72, 0x79, 0x20, 0x64	/* |covery d| */
	.byte	0x69, 0x73, 0x63, 0x61, 0x72, 0x64, 0x73, 0x20	/* |iscards | */
	.byte	0x63, 0x68, 0x61, 0x72, 0x20, 0x25, 0x64, 0x0a	/* |char %d.| */
	.byte	0x00	/* |.| */
D_1c54:
	.byte	0x72, 0x65, 0x64, 0x75, 0x63, 0x65, 0x20, 0x25	/* |reduce %| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_1c5f:
	.byte	0x49, 0x6e, 0x63, 0x6f, 0x6e, 0x73, 0x69, 0x73	/* |Inconsis| */
	.byte	0x74, 0x65, 0x6e, 0x74, 0x20, 0x72, 0x75, 0x6c	/* |tent rul| */
	.byte	0x65, 0x73, 0x20, 0x6c, 0x69, 0x6e, 0x65, 0x73	/* |es lines| */
	.byte	0x20, 0x66, 0x6f, 0x72, 0x20, 0x60, 0x25, 0x73	/* | for `%s| */
	.byte	0x27, 0x0a, 0x00	/* |'..| */
D_1c82:
	.byte	0x4d, 0x75, 0x6c, 0x74, 0x69, 0x70, 0x6c, 0x65	/* |Multiple| */
	.byte	0x20, 0x72, 0x75, 0x6c, 0x65, 0x73, 0x20, 0x6c	/* | rules l| */
	.byte	0x69, 0x6e, 0x65, 0x73, 0x20, 0x66, 0x6f, 0x72	/* |ines for| */
	.byte	0x20, 0x60, 0x25, 0x73, 0x27, 0x0a, 0x00	/* | `%s'..| */
D_1ca1:
	.ascii	".SUFFIXES\0"
D_1cab:
	.ascii	"Too many lefts\0"
D_1cba:
	.byte	0x25, 0x64, 0x00	/* |%d.| */
D_1cbd:
	.ascii	"Must be a separator on rules line %s\0"
D_1ce2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
D_1cfc:
	.byte	0x1f, 0x00	/* |..| */
D_1cfe:
	.byte	0x1c, 0x00, 0x1f, 0x00, 0x1e, 0x00, 0x1f, 0x00	/* |........| */
	.byte	0x1e, 0x00, 0x1f, 0x00, 0x1f, 0x00, 0x1e, 0x00	/* |........| */
	.byte	0x1f, 0x00, 0x1e, 0x00, 0x1f, 0x00	/* |......| */
D_1d14:
	.byte	0x05, 0x00	/* |..| */
D_1d16:
	.byte	0x4d, 0x01	/* |M.| */
	.word	D_003a	/* ptr: 0x003a */
	.byte	0x2f, 0x01	/* |/.| */
D_1d1c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
D_1d26:
	.byte	0x00, 0x00	/* |..| */
D_1d28:
	.byte	0x00, 0x00	/* |..| */
D_1d2a:
	.byte	0x00, 0x00	/* |..| */
D_1d2c:
	.byte	0x00, 0x00	/* |..| */
D_1d2e:
	.byte	0x44, 0x61, 0x79, 0x20, 0x4d, 0x6f, 0x6e, 0x20	/* |Day Mon | */
	.byte	0x30, 0x30, 0x20, 0x30, 0x30, 0x3a, 0x30, 0x30	/* |00 00:00| */
	.byte	0x3a, 0x30, 0x30, 0x20, 0x31, 0x39, 0x30, 0x30	/* |:00 1900| */
	.byte	0x0a, 0x00	/* |..| */
D_1d48:
	.ascii	"SunMonTueWedThuFriSat\0"
D_1d5e:
	.ascii	"JanFebMarAprMayJunJulAugSepOctNovDec\0"
	.byte	0x00	/* |.| */
D_1d84:
	.ascii	"/bin/sh\0"
D_1d8c:
	.ascii	"PATH\0"
D_1d91:
	.ascii	":/bin:/usr/bin\0"
D_1da0:
	.byte	0x00	/* |.| */
D_1da1:
	.byte	0x73, 0x68, 0x00	/* |sh.| */
D_1da4:
	.ascii	"/etc/passwd\0"
D_1db0:
	.byte	0x00, 0x00	/* |..| */
D_1db2:
	.byte	0x00, 0x00	/* |..| */
D_1db4:
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
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
D_1fb6:
	.byte	0x00, 0x00	/* |..| */
D_1fb8:
	.byte	0x00, 0x00	/* |..| */
D_1fba:
	.byte	0x00, 0x00	/* |..| */
D_1fbc:
	.byte	0x00, 0x00	/* |..| */
D_1fbe:
	.byte	0x00, 0x00	/* |..| */
D_1fc0:
	.byte	0x00, 0x00	/* |..| */
D_1fc2:
	.byte	0x00, 0x00	/* |..| */
D_1fc4:
	.byte	0x00, 0x00	/* |..| */
D_1fc6:
	.byte	0x00, 0x00	/* |..| */
D_1fc8:
	.byte	0x72, 0x00	/* |r.| */
D_1fca:
	.byte	0x72, 0x00	/* |r.| */
D_1fcc:
	.byte	0x00, 0x00, 0x00	/* |...| */
D_1fcf:
	.byte	0x20	/* | | */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.byte	0x08, 0x08, 0x08, 0x08	/* |....| */
	.word	D_0008	/* ptr: 0x2008 */
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
	.byte	0x10, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04	/* |........| */
	.byte	0x04, 0x04, 0x04, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x10, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41	/* |..AAAAAA| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x10	/* |..| */
	.word	D_2242	/* ptr: 0x4242 */
	.word	D_2242	/* ptr: 0x4242 */
	.word	D_2242	/* ptr: 0x4242 */
	.word	D_0202	/* ptr: 0x0202 */
	.word	D_0202	/* ptr: 0x0202 */
	.word	D_0202	/* ptr: 0x0202 */
	.word	D_0202	/* ptr: 0x0202 */
	.word	D_0202	/* ptr: 0x0202 */
	.word	D_0202	/* ptr: 0x0202 */
	.word	D_0202	/* ptr: 0x0202 */
	.word	D_0202	/* ptr: 0x0202 */
	.word	D_0202	/* ptr: 0x0202 */
	.word	D_0202	/* ptr: 0x0202 */
	.byte	0x10, 0x10, 0x10, 0x10	/* |....| */
	.word	D_0020	/* ptr: 0x0020 */
D_2050:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_2064:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
D_206e:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_2076:
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
D_20c5:
	.byte	0x00	/* |.| */
D_20c6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_20ce:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_20d6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_20de:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_20e6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_20ee:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_20f6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_20fe:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_2106:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_210e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_2116:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_211e:
	.word	D_2610	/* ptr: 0x4610 */
	.byte	0x00, 0x00	/* |..| */
	.word	D_2610	/* ptr: 0x4610 */
	.byte	0x01, 0x00	/* |..| */
D_2126:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_212c:
	.byte	0x02	/* |.| */
D_212d:
	.byte	0x01	/* |.| */
D_212e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
D_2136:
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
D_21be:
	.byte	0xbe, 0x41	/* |.A| */
D_21c0:
	.byte	0x00, 0x00	/* |..| */
D_21c2:
	.byte	0x00, 0x00	/* |..| */
D_21c4:
	.byte	0x00, 0x00	/* |..| */
D_21c6:
	.word	D_2816	/* ptr: 0x4816 */

	.bss

D_21c8:	.space	2	/* bss word @ 0x21c8 */
D_21ca:	.space	2	/* bss word @ 0x21ca */
D_21cc:	.space	2	/* bss word @ 0x21cc */
D_21ce:	.space	2	/* bss word @ 0x21ce */
D_21d0:	.space	2	/* bss word @ 0x21d0 */
	.space	6
D_21d8:	.space	2	/* bss word @ 0x21d8 */
	.space	18
D_21ec:	.space	2	/* bss word @ 0x21ec */
D_21ee:	.space	2	/* bss word @ 0x21ee */
D_21f0:	.space	2	/* bss word @ 0x21f0 */
	.space	6
D_21f8:	.space	2	/* bss word @ 0x21f8 */
	.space	72
D_2242:	.space	2	/* bss word @ 0x2242 */
	.space	52
D_2278:	.space	2	/* bss word @ 0x2278 */
D_227a:	.space	2	/* bss word @ 0x227a */
D_227c:	.space	2	/* bss word @ 0x227c */
	.space	2
D_2280:	.space	2	/* bss word @ 0x2280 */
D_2282:	.space	2	/* bss word @ 0x2282 */
D_2284:	.space	2	/* bss word @ 0x2284 */
D_2286:	.space	2	/* bss word @ 0x2286 */
D_2288:	.space	2	/* bss word @ 0x2288 */
D_228a:	.space	2	/* bss word @ 0x228a */
D_228c:	.space	2	/* bss word @ 0x228c */
D_228e:	.space	2	/* bss word @ 0x228e */
D_2290:	.space	2	/* bss word @ 0x2290 */
D_2292:	.space	2	/* bss word @ 0x2292 */
	.space	2
D_2296:	.space	2	/* bss word @ 0x2296 */
	.space	300
D_23c4:	.space	2	/* bss word @ 0x23c4 */
D_23c6:	.space	2	/* bss word @ 0x23c6 */
D_23c8:	.space	2	/* bss word @ 0x23c8 */
D_23ca:	.space	2	/* bss word @ 0x23ca */
D_23cc:	.space	2	/* bss word @ 0x23cc */
D_23ce:	.space	2	/* bss word @ 0x23ce */
D_23d0:	.space	2	/* bss word @ 0x23d0 */
D_23d2:	.space	2	/* bss word @ 0x23d2 */
D_23d4:	.space	2	/* bss word @ 0x23d4 */
D_23d6:	.space	2	/* bss word @ 0x23d6 */
D_23d8:	.space	2	/* bss word @ 0x23d8 */
D_23da:	.space	2	/* bss word @ 0x23da */
D_23dc:	.space	2	/* bss word @ 0x23dc */
D_23de:	.space	2	/* bss word @ 0x23de */
D_23e0:	.space	2	/* bss word @ 0x23e0 */
D_23e2:	.space	2	/* bss word @ 0x23e2 */
D_23e4:	.space	2	/* bss word @ 0x23e4 */
D_23e6:	.space	2	/* bss word @ 0x23e6 */
D_23e8:	.space	2	/* bss word @ 0x23e8 */
D_23ea:	.space	2	/* bss word @ 0x23ea */
D_23ec:	.space	2	/* bss word @ 0x23ec */
D_23ee:	.space	2	/* bss word @ 0x23ee */
D_23f0:	.space	2	/* bss word @ 0x23f0 */
D_23f2:	.space	2	/* bss word @ 0x23f2 */
D_23f4:	.space	2	/* bss word @ 0x23f4 */
D_23f6:	.space	2	/* bss word @ 0x23f6 */
D_23f8:	.space	2	/* bss word @ 0x23f8 */
D_23fa:	.space	2	/* bss word @ 0x23fa */
D_23fc:	.space	2	/* bss word @ 0x23fc */
D_23fe:	.space	2	/* bss word @ 0x23fe */
D_2400:	.space	2	/* bss word @ 0x2400 */
D_2402:	.space	2	/* bss word @ 0x2402 */
D_2404:	.space	2	/* bss word @ 0x2404 */
D_2406:	.space	2	/* bss word @ 0x2406 */
D_2408:	.space	2	/* bss word @ 0x2408 */
	.space	6
D_2410:	.space	2	/* bss word @ 0x2410 */
	.space	510
D_2610:	.space	2	/* bss word @ 0x2610 */
	.space	510
D_2810:	.space	2	/* bss word @ 0x2810 */
D_2812:	.space	2	/* bss word @ 0x2812 */
D_2814:	.space	2	/* bss word @ 0x2814 */
D_2816:	/* endcore: initial break */

/* end of file */
