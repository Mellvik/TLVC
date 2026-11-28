/* neqn.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 neqn.s -o neqn.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* Wrapping near-call/jmp targets (16-bit address space) */
	.set L_000b, 0x000b	/* wrapping branch target */
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: neqn  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00004d22  (19746 bytes)
//   a_data:   0x00002280  (8832 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

L_0036:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
L_0045:
	call   L_1487
	mov    %ax,-0x6(%bp)
	call   L_1487
	mov    %ax,-0x8(%bp)
	pushw  0x4(%bp)
	pushw  D_2282
	pushw  0x4(%bp)
	call   L_1515
	add    $0x6,%sp
	xor    %ax,%ax
	push   %ax
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    0x4(%bp),%bx
	shl    %bx
	sub    D_2350(%bx),%ax
	sub    $0x28,%ax
	push   %ax
	call   L_1467
	add    $0x4,%sp
	push   %ax
	/* TODO: data ref: mov    $0x2014,%ax */
	mov    $D_0014,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	mov    0x6(%bp),%ax
	jmp   L_00ff
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x2020,%ax */
	mov    $D_0020,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	jmp   L_00eb
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x2035,%ax */
	mov    $D_0035,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	jmp   L_00eb
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x203f,%ax */
	mov    $D_003f,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	jmp   L_00fd
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x2049,%ax */
	mov    $D_0049,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	jmp   L_00fd
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x205e,%ax */
	mov    $D_005e,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	jmp   L_00fd
	pushw  0x4(%bp)
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x2074,%ax */
	mov    $D_0074,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_00eb:
	jmp   L_010e
	pushw  0x4(%bp)
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x2092,%ax */
	mov    $D_0092,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_00fd:
	jmp   L_010e
L_00ff:
	sub    $0x130,%ax
	cmp    $0x7,%ax
	ja   L_010e
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2004(%bx)
L_010e:
	pushw  -0x6(%bp)
	pushw  D_2282
	pushw  -0x6(%bp)
	call   L_1515
	add    $0x6,%sp
	pushw  -0x6(%bp)
	pushw  -0x6(%bp)
	pushw  0x4(%bp)
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x20a5,%ax */
	mov    $D_00a5,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	pushw  0x4(%bp)
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x20d3,%ax */
	mov    $D_00d3,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	cmpw   $0x135,0x6(%bp)
	je   L_0155
	mov    0x4(%bp),%bx
	shl    %bx
	addw   $0x14,D_2288(%bx)
L_0155:
	cmpw   $0x0,D_2418
	je   L_017e
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_2350(%bx)
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_2288(%bx)
	pushw  0x4(%bp)
	pushw  0x6(%bp)
	/* TODO: data ref: mov    $0x20f4,%ax */
	mov    $D_00f4,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
L_017e:
	pushw  -0x6(%bp)
	call   L_14d1
	pop    %cx
	pushw  -0x8(%bp)
	call   L_14d1
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0194:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x138,%sp
L_01a4:
	xor    %si,%si
	/* TODO: data ref: movw   $0xffff,0x2ade */
	/* movw constant -> D_0ade */
	mov	$0xffff,%ax
	mov	%ax,D_0ade
	/* TODO: data ref: movw   $0x0,0x2ae0 */
	movw   $0x0,D_0ae0
	/* TODO: data ref: movw   $0x0,0x2ae2 */
	movw   $0x0,D_0ae2
	lea    -0x132(%bp),%ax
	mov    %ax,-0x136(%bp)
	movw   $D_2426,-0x13a(%bp)
L_01c6:
	/* TODO: data ref: cmpw   $0x0,0x2adc */
	cmpw   $0x0,D_0adc
	je   L_01dc
	/* TODO: data ref: pushw  0x2ade */
	pushw  D_0ade
	push   %si
	/* TODO: data ref: mov    $0x2ba4,%ax */
	mov    $D_0ba4,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_01dc:
	addw   $0x2,-0x136(%bp)
	lea    -0x4(%bp),%ax
	cmp    %ax,-0x136(%bp)
	jbe   L_01f8
	/* TODO: data ref: mov    $0x2bb8,%ax */
	mov    $D_0bb8,%ax
	push   %ax
	call   L_1658
	pop    %cx
	mov    $0x1,%ax
	jmp   L_02f4
L_01f8:
	mov    -0x136(%bp),%bx
	mov    %si,(%bx)
	addw   $0x2,-0x13a(%bp)
	mov    -0x13a(%bp),%bx
	mov    D_241a,%ax
	mov    %ax,(%bx)
L_020c:
	mov    %si,%bx
	shl    %bx
	/* TODO: data ref: mov    0x2584(%bx),%ax */
	mov    D_0584(%bx),%ax
	mov    %ax,-0x138(%bp)
	cmp    $0xfc18,%ax
	jle   L_0282
	/* TODO: data ref: cmpw   $0x0,0x2ade */
	cmpw   $0x0,D_0ade
	jge   L_0234
	call   L_1857
	/* TODO: data ref: mov    %ax,0x2ade */
	mov    %ax,D_0ade
	or     %ax,%ax
	jge   L_0234
	/* TODO: data ref: movw   $0x0,0x2ade */
	movw   $0x0,D_0ade
L_0234:
	/* TODO: data ref: mov    0x2ade,%ax */
	mov    D_0ade,%ax
	add    %ax,-0x138(%bp)
	jl   L_0282
	cmpw   $0x230,-0x138(%bp)
	jge   L_0282
	mov    -0x138(%bp),%bx
	shl    %bx
	/* TODO: data ref: mov    0x2124(%bx),%ax */
	mov    D_0124(%bx),%ax
	mov    %ax,-0x138(%bp)
	mov    %ax,%bx
	shl    %bx
	/* TODO: data ref: mov    0x2878(%bx),%ax */
	mov    D_0878(%bx),%ax
	/* TODO: data ref: cmp    0x2ade,%ax */
	cmp    D_0ade,%ax
	jne   L_0282
	/* TODO: data ref: movw   $0xffff,0x2ade */
	/* movw constant -> D_0ade */
	mov	$0xffff,%ax
	mov	%ax,D_0ade
	mov    D_241c,%ax
	mov    %ax,D_241a
	mov    -0x138(%bp),%si
	/* TODO: data ref: cmpw   $0x0,0x2ae2 */
	cmpw   $0x0,D_0ae2
	jg   L_027b
	jmp   L_01c6
L_027b:
	/* TODO: data ref: decw   0x2ae2 */
	decw   D_0ae2
L_027f:
	jmp   L_01c6
L_0282:
	mov    %si,%bx
	shl    %bx
	/* TODO: data ref: mov    0x29aa(%bx),%ax */
	mov    D_09aa(%bx),%ax
	mov    %ax,-0x138(%bp)
	cmp    $0xfffe,%ax
	jne   L_02f7
	/* TODO: data ref: cmpw   $0x0,0x2ade */
	cmpw   $0x0,D_0ade
	jge   L_02aa
	call   L_1857
	/* TODO: data ref: mov    %ax,0x2ade */
	mov    %ax,D_0ade
	or     %ax,%ax
	jge   L_02aa
	/* TODO: data ref: movw   $0x0,0x2ade */
	movw   $0x0,D_0ade
L_02aa:
	/* TODO: data ref: movw   $0x2118,-0x13c(%bp) */
	movw   $D_0118,-0x13c(%bp)
L_02b0:
	mov    -0x13c(%bp),%bx
	cmpw   $0xffff,(%bx)
	jne   L_02c2
	mov    -0x13c(%bp),%bx
	cmp    %si,0x2(%bx)
	je   L_02c9
L_02c2:
	addw   $0x4,-0x13c(%bp)
	jmp   L_02b0
L_02c9:
	addw   $0x4,-0x13c(%bp)
	mov    -0x13c(%bp),%bx
	cmpw   $0x0,(%bx)
	jl   L_02e3
	mov    -0x13c(%bp),%bx
	mov    (%bx),%ax
	/* TODO: data ref: cmp    0x2ade,%ax */
	cmp    D_0ade,%ax
	jne   L_02c9
L_02e3:
	mov    -0x13c(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x138(%bp)
	or     %ax,%ax
	jge   L_02f7
	xor    %ax,%ax
L_02f4:
	jmp   L_03ab
L_02f7:
	cmpw   $0x0,-0x138(%bp)
	je   L_0301
	jmp   L_03df
L_0301:
	/* TODO: data ref: mov    0x2ae2,%ax */
	mov    D_0ae2,%ax
	jmp   L_03d3
	/* TODO: data ref: mov    $0x2bcc,%ax */
	mov    $D_0bcc,%ax
	push   %ax
	call   L_1658
	pop    %cx
	/* TODO: data ref: incw   0x2ae0 */
	incw   D_0ae0
	/* TODO: data ref: movw   $0x3,0x2ae2 */
	movw   $0x3,D_0ae2
L_0319:
	lea    -0x130(%bp),%ax
	cmp    %ax,-0x136(%bp)
	jae   L_0326
	jmp   L_03a8
L_0326:
	mov    -0x136(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,%bx
	shl    %bx
	/* TODO: data ref: mov    0x2584(%bx),%ax */
	mov    D_0584(%bx),%ax
	add    $0x100,%ax
	mov    %ax,-0x138(%bp)
	cmp    $0x0,%ax
	jl   L_036b
	cmpw   $0x230,-0x138(%bp)
	jge   L_036b
	mov    -0x138(%bp),%bx
	shl    %bx
	/* TODO: data ref: mov    0x2124(%bx),%ax */
	mov    D_0124(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	/* TODO: data ref: cmpw   $0x100,0x2878(%bx) */
	cmpw   $0x100,D_0878(%bx)
	jne   L_036b
	mov    -0x138(%bp),%bx
	shl    %bx
	/* TODO: data ref: mov    0x2124(%bx),%si */
	mov    D_0124(%bx),%si
L_0368:
	jmp   L_027f
L_036b:
	mov    -0x136(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,%bx
	shl    %bx
	/* TODO: data ref: mov    0x2584(%bx),%ax */
	mov    D_0584(%bx),%ax
	mov    %ax,-0x138(%bp)
	/* TODO: data ref: cmpw   $0x0,0x2adc */
	cmpw   $0x0,D_0adc
	je   L_039b
	mov    -0x136(%bp),%bx
	pushw  -0x2(%bx)
	mov    -0x136(%bp),%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x2bd9,%ax */
	mov    $D_0bd9,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_039b:
	subw   $0x2,-0x136(%bp)
	subw   $0x2,-0x13a(%bp)
	jmp   L_0319
L_03a8:
	mov    $0x1,%ax
L_03ab:
	jmp   L_0921
	/* TODO: data ref: cmpw   $0x0,0x2adc */
	cmpw   $0x0,D_0adc
	je   L_03c3
	/* TODO: data ref: pushw  0x2ade */
	pushw  D_0ade
	/* TODO: data ref: mov    $0x2c04,%ax */
	mov    $D_0c04,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_03c3:
	/* TODO: data ref: cmpw   $0x0,0x2ade */
	cmpw   $0x0,D_0ade
	je   L_03a8
	/* TODO: data ref: movw   $0xffff,0x2ade */
	/* movw constant -> D_0ade */
	mov	$0xffff,%ax
	mov	%ax,D_0ade
	jmp   L_020c
L_03d3:
	cmp    $0x3,%ax
	ja   L_03df
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2ae4(%bx)
L_03df:
	/* TODO: data ref: cmpw   $0x0,0x2adc */
	cmpw   $0x0,D_0adc
	je   L_03f4
	pushw  -0x138(%bp)
	/* TODO: data ref: mov    $0x2c25,%ax */
	mov    $D_0c25,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_03f4:
	mov    -0x138(%bp),%bx
	shl    %bx
	/* TODO: data ref: mov    0x27ba(%bx),%ax */
	mov    D_07ba(%bx),%ax
	shl    %ax
	sub    %ax,-0x136(%bp)
	mov    -0x13a(%bp),%di
	mov    -0x138(%bp),%bx
	shl    %bx
	/* TODO: data ref: mov    0x27ba(%bx),%ax */
	mov    D_07ba(%bx),%ax
	shl    %ax
	sub    %ax,-0x13a(%bp)
	mov    -0x13a(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,D_241a
	mov    -0x138(%bp),%ax
	mov    %ax,-0x134(%bp)
	mov    -0x138(%bp),%bx
	shl    %bx
	/* TODO: data ref: mov    0x26fc(%bx),%ax */
	mov    D_06fc(%bx),%ax
	mov    %ax,-0x138(%bp)
	mov    %ax,%bx
	shl    %bx
	/* TODO: data ref: mov    0x26b6(%bx),%ax */
	mov    D_06b6(%bx),%ax
	mov    -0x136(%bp),%bx
	add    (%bx),%ax
	inc    %ax
	mov    %ax,-0x132(%bp)
	cmp    $0x230,%ax
	jge   L_046c
	mov    -0x132(%bp),%bx
	shl    %bx
	/* TODO: data ref: mov    0x2124(%bx),%si */
	mov    D_0124(%bx),%si
	mov    %si,%bx
	shl    %bx
	/* TODO: data ref: mov    0x2878(%bx),%ax */
	mov    D_0878(%bx),%ax
	mov    -0x138(%bp),%dx
	neg    %dx
	cmp    %dx,%ax
	je   L_047e
L_046c:
	mov    -0x138(%bp),%bx
	shl    %bx
	/* TODO: data ref: mov    0x26b6(%bx),%ax */
	mov    D_06b6(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	/* TODO: data ref: mov    0x2124(%bx),%si */
	mov    D_0124(%bx),%si
L_047e:
	mov    -0x134(%bp),%ax
	jmp   L_0912
	pushw  (%di)
	call   L_1373
	pop    %cx
L_048b:
	jmp   L_0368
	/* TODO: data ref: mov    $0x2c30,%ax */
	mov    $D_0c30,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_16a0
	add    $0x4,%sp
L_049b:
	jmp   L_048b
	movw   $0x0,D_2422
	jmp   L_048b
	xor    %ax,%ax
	push   %ax
	pushw  (%di)
	pushw  -0x2(%di)
	call   L_0928
	add    $0x6,%sp
L_04b3:
	jmp   L_048b
	mov    $0x1,%ax
	push   %ax
	pushw  (%di)
	pushw  -0x2(%di)
	call   L_0928
	add    $0x6,%sp
L_04c4:
	jmp   L_048b
	xor    %ax,%ax
	push   %ax
	call   L_1f34
	pop    %cx
L_04cd:
	jmp   L_048b
	mov    (%di),%ax
	mov    %ax,D_241a
	mov    $0x1,%ax
	push   %ax
	call   L_1f34
	pop    %cx
L_04dc:
	jmp   L_049b
	mov    D_241e,%ax
	mov    %ax,D_241a
	jmp   L_049b
	pushw  -0x6(%di)
	mov    $0x4c,%ax
	push   %ax
	call   L_1f78
	add    $0x4,%sp
L_04f3:
	jmp   L_04b3
	pushw  -0x6(%di)
	mov    $0x43,%ax
	push   %ax
	call   L_1f78
	add    $0x4,%sp
L_0502:
	jmp   L_04c4
	pushw  -0x6(%di)
	mov    $0x52,%ax
	push   %ax
	call   L_1f78
	add    $0x4,%sp
L_0511:
	jmp   L_04cd
	pushw  -0x6(%di)
	mov    $0x2d,%ax
	push   %ax
	call   L_1f78
	add    $0x4,%sp
L_0520:
	jmp   L_04dc
	mov    D_241e,%ax
	mov    %ax,D_241a
	incw   D_241e
L_052c:
	jmp   L_04dc
	mov    D_241e,%ax
	mov    %ax,D_241a
	incw   D_241e
L_0538:
	jmp   L_04f3
	mov    D_241e,%ax
	mov    %ax,D_241a
	incw   D_241e
L_0544:
	jmp   L_04f3
	mov    D_241e,%ax
	mov    %ax,D_241a
	incw   D_241e
L_0550:
	jmp   L_0502
	mov    (%di),%ax
	mov    %ax,D_241a
	jmp   L_0502
	mov    (%di),%ax
	mov    %ax,D_241a
L_055e:
	jmp   L_0511
	movw   $0x0,D_241a
	jmp   L_0511
	pushw  (%di)
	pushw  -0x4(%di)
	call   L_236c
	add    $0x4,%sp
L_0573:
	jmp   L_0520
	pushw  (%di)
	call   L_1ef4
	pop    %cx
	jmp   L_052c
	pushw  (%di)
	pushw  -0x2(%di)
	call   L_3068
	add    $0x4,%sp
L_0588:
	jmp   L_0538
	pushw  (%di)
	pushw  -0x2(%di)
	call   L_0ac6
	add    $0x4,%sp
	jmp   L_0544
	pushw  (%di)
	call   L_0b72
	pop    %cx
L_059d:
	jmp   L_0550
	pushw  (%di)
	call   L_318a
	pop    %cx
	jmp   L_055e
	pushw  D_241e
	pushw  -0x6(%di)
	mov    $0x4c,%ax
	push   %ax
	call   L_2906
	add    $0x6,%sp
	mov    -0x6(%di),%ax
	mov    %ax,D_241e
L_05be:
	jmp   L_0573
	pushw  D_241e
	pushw  -0x6(%di)
	mov    $0x43,%ax
	push   %ax
	call   L_2906
	add    $0x6,%sp
	mov    -0x6(%di),%ax
	mov    %ax,D_241e
L_05d7:
	jmp   L_0588
	pushw  D_241e
	pushw  -0x6(%di)
	mov    $0x52,%ax
	push   %ax
	call   L_2906
	add    $0x6,%sp
	mov    -0x6(%di),%ax
	mov    %ax,D_241e
L_05f0:
	jmp   L_059d
	pushw  D_241e
	pushw  -0x6(%di)
	mov    $0x2d,%ax
	push   %ax
	call   L_2906
	add    $0x6,%sp
	mov    -0x6(%di),%ax
	mov    %ax,D_241e
L_0609:
	jmp   L_05be
	pushw  (%di)
	pushw  -0x2(%di)
	pushw  -0x6(%di)
	call   L_2e1b
	add    $0x6,%sp
	jmp   L_05d7
	pushw  (%di)
	pushw  -0x2(%di)
	pushw  -0x4(%di)
	call   L_2c74
	add    $0x6,%sp
L_0629:
	jmp   L_05d7
	pushw  (%di)
	pushw  -0x2(%di)
	pushw  -0x4(%di)
	call   L_2c74
	add    $0x6,%sp
L_0639:
	jmp   L_05f0
	pushw  (%di)
	pushw  -0x2(%di)
	pushw  -0x6(%di)
	call   L_0f98
	add    $0x6,%sp
L_0649:
	jmp   L_0609
	xor    %ax,%ax
	push   %ax
	pushw  (%di)
	pushw  -0x4(%di)
	call   L_0f98
	add    $0x6,%sp
L_0659:
	jmp   L_0609
	pushw  (%di)
	xor    %ax,%ax
	push   %ax
	pushw  -0x4(%di)
	call   L_0f98
	add    $0x6,%sp
L_0669:
	jmp   L_0629
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	pushw  (%di)
	call   L_0f98
	add    $0x6,%sp
L_0679:
	jmp   L_0639
	pushw  (%di)
	pushw  -0x2(%di)
	pushw  -0x4(%di)
	call   L_24dc
	add    $0x6,%sp
L_0689:
	jmp   L_0649
	pushw  (%di)
	pushw  -0x2(%di)
	pushw  -0x6(%di)
	call   L_0c5c
	add    $0x6,%sp
	movw   $0x0,D_2420
L_069f:
	jmp   L_0659
	pushw  (%di)
	xor    %ax,%ax
	push   %ax
	pushw  -0x4(%di)
	call   L_0c5c
	add    $0x6,%sp
L_06af:
	jmp   L_0669
	pushw  (%di)
	pushw  -0x2(%di)
	call   L_0036
	add    $0x4,%sp
L_06bc:
	jmp   L_0679
	pushw  (%di)
	pushw  -0x2(%di)
	mov    $0x12d,%ax
	push   %ax
	call   L_229e
	add    $0x6,%sp
L_06cd:
	jmp   L_0689
	pushw  (%di)
	pushw  -0x2(%di)
	mov    $0x12a,%ax
	push   %ax
	call   L_229e
	add    $0x6,%sp
L_06de:
	jmp   L_069f
	pushw  (%di)
	pushw  -0x2(%di)
	mov    $0x12c,%ax
	push   %ax
	call   L_229e
	add    $0x6,%sp
L_06ef:
	jmp   L_06af
	pushw  (%di)
	pushw  -0x2(%di)
	mov    $0x12b,%ax
	push   %ax
	call   L_229e
	add    $0x6,%sp
L_0700:
	jmp   L_06bc
	pushw  -0x6(%di)
	call   L_1ffa
	pop    %cx
L_0709:
	jmp   L_06cd
	call   L_1024
L_070e:
	jmp   L_06cd
	pushw  -0x2(%di)
	call   L_382e
	pop    %cx
	mov    %ax,D_241a
L_071a:
	jmp   L_06de
	pushw  -0x2(%di)
	call   L_382e
	pop    %cx
	mov    %ax,D_241a
	jmp   L_06de
	pushw  -0x2(%di)
	call   L_382e
	pop    %cx
	mov    %ax,D_241a
L_0732:
	jmp   L_06ef
	pushw  -0x2(%di)
	call   L_382e
	pop    %cx
	mov    %ax,D_241a
L_073e:
	jmp   L_0700
	movw   $0x132,D_241a
	jmp   L_0700
	movw   $0x136,D_241a
	jmp   L_0700
	movw   $0x137,D_241a
L_0756:
	jmp   L_0709
	movw   $0x134,D_241a
	jmp   L_0709
	movw   $0x135,D_241a
	jmp   L_070e
	movw   $0x130,D_241a
	jmp   L_071a
	movw   $0x133,D_241a
L_0776:
	jmp   L_071a
	movw   $0x131,D_241a
	jmp   L_071a
	mov    D_2282,%ax
	mov    %ax,D_241a
	subw   $0x3,D_2282
	movw   $0x1,D_2420
	cmpw   $0x0,D_2418
	je   L_0732
	pushw  D_2420
	pushw  D_2282
	pushw  D_241a
	/* TODO: data ref: mov    $0x2c3d,%ax */
	mov    $D_0c3d,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
L_07ae:
	jmp   L_073e
	mov    D_2282,%ax
	mov    %ax,D_241a
	cmpw   $0x0,D_2420
	jne   L_07c2
	subw   $0x3,D_2282
L_07c2:
	cmpw   $0x0,D_2418
	je   L_0756
	pushw  D_2282
	pushw  D_241a
	/* TODO: data ref: mov    $0x2c64,%ax */
	mov    $D_0c64,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_07db:
	jmp   L_0776
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,D_241a
L_07e5:
	jmp   L_07ae
	movw   $0x7b,D_241a
L_07ed:
	jmp   L_07ae
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,D_241a
L_07f7:
	jmp   L_07ae
	movw   $0x7d,D_241a
	jmp   L_07ae
	movw   $0x0,D_241a
L_0807:
	jmp   L_07ae
	mov    D_241e,%bx
	shl    %bx
	mov    (%di),%ax
	mov    %ax,D_2558(%bx)
	incw   D_241e
L_0819:
	jmp   L_07db
	mov    D_241e,%bx
	shl    %bx
	mov    (%di),%ax
	mov    %ax,D_2558(%bx)
	incw   D_241e
L_082b:
	jmp   L_07db
	mov    D_241e,%ax
	mov    %ax,D_241a
L_0833:
	jmp   L_07db
	mov    D_241e,%ax
	mov    %ax,D_241a
	jmp   L_07e5
	mov    D_241e,%ax
	mov    %ax,D_241a
L_0843:
	jmp   L_07ed
	mov    D_241e,%ax
	mov    %ax,D_241a
	jmp   L_07f7
	mov    D_2282,%ax
	mov    %ax,D_241a
	pushw  (%di)
	call   L_3008
	pop    %cx
L_0859:
	jmp   L_0807
	mov    $0x31,%ax
	push   %ax
	call   L_0a4a
	pop    %cx
L_0863:
	jmp   L_0819
	mov    $0x31,%ax
	push   %ax
	call   L_0a4a
	pop    %cx
L_086d:
	jmp   L_082b
	mov    $0x31,%ax
	push   %ax
	call   L_0a4a
	pop    %cx
	jmp   L_0833
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_0a4a
	pop    %cx
L_0883:
	jmp   L_0843
	mov    $0x123,%ax
	push   %ax
	call   L_2de5
	pop    %cx
L_088d:
	jmp   L_0859
	mov    $0x122,%ax
	push   %ax
	call   L_2de5
	pop    %cx
L_0897:
	jmp   L_0863
	mov    -0x2(%di),%ax
	mov    %ax,D_241a
	jmp   L_0863
	pushw  (%di)
	mov    $0x102,%ax
	push   %ax
	call   L_322c
	add    $0x4,%sp
L_08ad:
	jmp   L_086d
	pushw  (%di)
	mov    $0x101,%ax
	push   %ax
	call   L_322c
	add    $0x4,%sp
L_08bb:
	jmp   L_0883
	xor    %ax,%ax
	push   %ax
	mov    $0x103,%ax
	push   %ax
	call   L_322c
	add    $0x4,%sp
L_08ca:
	jmp   L_088d
	xor    %ax,%ax
	push   %ax
	mov    $0x104,%ax
	push   %ax
	call   L_322c
	add    $0x4,%sp
L_08d9:
	jmp   L_0897
	xor    %ax,%ax
	push   %ax
	mov    $0x105,%ax
	push   %ax
	call   L_322c
	add    $0x4,%sp
	jmp   L_08ad
	mov    $0x10d,%ax
	push   %ax
	call   L_0ed0
	pop    %cx
	jmp   L_08bb
	mov    $0x10f,%ax
	push   %ax
	call   L_0ed0
	pop    %cx
	jmp   L_08bb
	mov    $0x110,%ax
	push   %ax
	call   L_0ed0
	pop    %cx
	jmp   L_08ca
	mov    $0x111,%ax
	push   %ax
	call   L_0ed0
	pop    %cx
L_0910:
	jmp   L_08d9
L_0912:
	dec    %ax
	cmp    $0x5b,%ax
	ja   L_08d9
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2aec(%bx)
	jmp   L_0910
L_0921:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0928:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
L_0937:
	mov    0x4(%bp),%ax
	mov    %ax,D_241a
	mov    0x6(%bp),%bx
	shl    %bx
	pushw  D_2350(%bx)
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_2350(%bx)
	call   L_1467
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    0x6(%bp),%bx
	shl    %bx
	sub    D_2350(%bx),%ax
	push   %ax
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    0x4(%bp),%bx
	shl    %bx
	sub    D_2350(%bx),%ax
	push   %ax
	call   L_1467
	add    $0x4,%sp
	add    -0x6(%bp),%ax
	mov    %ax,-0x8(%bp)
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2288(%bx)
	mov    D_241a,%bx
	shl    %bx
	mov    -0x6(%bp),%ax
	mov    %ax,D_2350(%bx)
	cmpw   $0x0,D_2418
	je   L_09c2
	pushw  -0x8(%bp)
	pushw  -0x6(%bp)
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	pushw  D_241a
	/* TODO: data ref: mov    $0x2c80,%ax */
	mov    $D_0c80,%ax
	push   %ax
	call   L_39ae
	add    $0xc,%sp
L_09c2:
	mov    0x4(%bp),%bx
	shl    %bx
	cmpw   $0x31,D_25f8(%bx)
	jne   L_09e1
	mov    0x6(%bp),%bx
	shl    %bx
	cmpw   $0x31,D_26c0(%bx)
	jne   L_09e1
	/* TODO: data ref: movw   $0x2ca4,-0xa(%bp) */
	movw   $D_0ca4,-0xa(%bp)
	jmp   L_09e6
L_09e1:
	/* TODO: data ref: movw   $0x2ca7,-0xa(%bp) */
	movw   $D_0ca7,-0xa(%bp)
L_09e6:
	cmpw   $0x0,0x8(%bp)
	je   L_0a16
	pushw  -0xa(%bp)
	pushw  0x4(%bp)
	pushw  D_2282
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x2ca8,%ax */
	mov    $D_0ca8,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	pushw  0x4(%bp)
	pushw  0x4(%bp)
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x2cc0,%ax */
	mov    $D_0cc0,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
L_0a16:
	pushw  0x6(%bp)
	pushw  -0xa(%bp)
	pushw  D_241a
	/* TODO: data ref: mov    $0x2ce0,%ax */
	mov    $D_0ce0,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_25f8(%bx),%ax
	mov    0x4(%bp),%bx
	shl    %bx
	mov    %ax,D_25f8(%bx)
	pushw  0x6(%bp)
	call   L_14d1
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a4a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0a56:
	mov    D_2788,%ax
	mov    %ax,D_241a
	cmpb   $0x72,0x4(%bp)
	je   L_0a68
	cmpb   $0x52,0x4(%bp)
	jne   L_0a70
L_0a68:
	movw   $0x31,D_2788
	jmp   L_0a9f
L_0a70:
	cmpb   $0x69,0x4(%bp)
	je   L_0a7c
	cmpb   $0x49,0x4(%bp)
	jne   L_0a84
L_0a7c:
	movw   $0x31,D_2788
	jmp   L_0a9f
L_0a84:
	cmpb   $0x62,0x4(%bp)
	je   L_0a90
	cmpb   $0x42,0x4(%bp)
	jne   L_0a98
L_0a90:
	movw   $0x31,D_2788
	jmp   L_0a9f
L_0a98:
	mov    0x4(%bp),%al
	cbtw
	mov    %ax,D_2788
L_0a9f:
	pushw  D_2788
	/* TODO: data ref: mov    $0x2cf2,%ax */
	mov    $D_0cf2,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	cmpw   $0x0,D_2418
	je   L_0ac2
	pushw  D_2788
	/* TODO: data ref: mov    $0x2cfa,%ax */
	mov    $D_0cfa,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_0ac2:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ac6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0ad2:
	mov    0x6(%bp),%ax
	mov    %ax,D_241a
	cmpw   $0x31,D_2788
	jne   L_0ae4
	mov    $0x31,%ax
	jmp   L_0ae7
L_0ae4:
	mov    $0x31,%ax
L_0ae7:
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_25f8(%bx)
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_26c0(%bx)
	cmpw   $0x0,D_2418
	je   L_0b42
	mov    D_241a,%bx
	shl    %bx
	pushw  D_25f8(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_26c0(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2288(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2350(%bx)
	pushw  0x4(%bp)
	pushw  0x6(%bp)
	pushw  D_2788
	pushw  D_241a
	/* TODO: data ref: mov    $0x2d08,%ax */
	mov    $D_0d08,%ax
	push   %ax
	call   L_39ae
	add    $0x12,%sp
L_0b42:
	pushw  0x4(%bp)
	pushw  0x6(%bp)
	pushw  D_2788
	pushw  D_241a
	/* TODO: data ref: mov    $0x2d3c,%ax */
	mov    $D_0d3c,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	mov    0x4(%bp),%ax
	mov    %ax,D_2788
	pushw  D_2788
	/* TODO: data ref: mov    $0x2d52,%ax */
	mov    $D_0d52,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0b72:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_0b7f:
	mov    0x4(%bp),%ax
	mov    %ax,D_241a
	mov    D_2282,%ax
	cwtd
	mov    $0x4,%cx
	idiv   %cx
	mov    %ax,-0x6(%bp)
	pushw  0x4(%bp)
	pushw  D_2282
	pushw  0x4(%bp)
	call   L_1515
	add    $0x6,%sp
	pushw  0x4(%bp)
	pushw  -0x6(%bp)
	pushw  0x4(%bp)
	pushw  0x4(%bp)
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x2d5a,%ax */
	mov    $D_0d5a,%ax
	push   %ax
	call   L_39ae
	add    $0xc,%sp
	cmpw   $0x0,D_2418
	je   L_0bd1
	pushw  -0x6(%bp)
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x2d7c,%ax */
	mov    $D_0d7c,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_0bd1:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0bd6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x14,%sp
L_0be5:
	mov    $0x14,%ax
	push   %ax
	lea    -0x18(%bp),%ax
	push   %ax
	call   L_1a91
	add    $0x4,%sp
	movw   $0x0,D_2422
	movw   $0x0,D_241a
	mov    -0x18(%bp),%al
	cbtw
	/* TODO: data ref: mov    %ax,0x2f3c */
	mov    %ax,D_0f3c
	jmp   L_0c20
L_0c08:
	/* TODO: data ref: movw   $0x31,0x2f3c */
	movw   $0x31,D_0f3c
	jmp   L_0c40
L_0c10:
	/* TODO: data ref: movw   $0x32,0x2f3c */
	movw   $0x32,D_0f3c
	jmp   L_0c40
L_0c18:
	/* TODO: data ref: movw   $0x33,0x2f3c */
	movw   $0x33,D_0f3c
	jmp   L_0c40
L_0c20:
	cmp    $0x52,%ax
	je   L_0c08
	jg   L_0c31
	cmp    $0x42,%ax
	je   L_0c18
	cmp    $0x49,%ax
	je   L_0c10
L_0c31:
	cmp    $0x62,%ax
	je   L_0c18
	cmp    $0x69,%ax
	je   L_0c10
	cmp    $0x72,%ax
	je   L_0c08
L_0c40:
	/* TODO: data ref: pushw  0x2f3c */
	pushw  D_0f3c
	/* TODO: data ref: mov    $0x2d8d,%ax */
	mov    $D_0d8d,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	/* TODO: data ref: mov    0x2f3c,%ax */
	mov    D_0f3c,%ax
	mov    %ax,D_2788
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0c5c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
L_0c6b:
	call   L_1487
	mov    %ax,D_241a
	mov    %ax,%bx
	shl    %bx
	movw   $0x0,D_25f8(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x0,D_26c0(%bx)
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2288(%bx)
	mov    %ax,-0x8(%bp)
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2350(%bx),%ax
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0x6(%bp)
	cmpw   $0x6,D_2282
	jl   L_0cba
	mov    D_2282,%ax
	jmp   L_0cbd
L_0cba:
	mov    $0x6,%ax
L_0cbd:
	mov    %ax,-0xc(%bp)
	addw   $0x3,D_2282
	pushw  0x4(%bp)
	pushw  D_2282
	pushw  0x4(%bp)
	call   L_1515
	add    $0x6,%sp
	pushw  0x4(%bp)
	pushw  D_241a
	/* TODO: data ref: mov    $0x2d96,%ax */
	mov    $D_0d96,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	cmpw   $0x0,0x6(%bp)
	jle   L_0d32
	pushw  0x6(%bp)
	pushw  -0xc(%bp)
	pushw  0x6(%bp)
	call   L_1515
	add    $0x6,%sp
	pushw  0x6(%bp)
	pushw  D_241a
	pushw  D_241a
	pushw  0x6(%bp)
	/* TODO: data ref: mov    $0x2da4,%ax */
	mov    $D_0da4,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    D_241a,%bx
	shl    %bx
	add    %ax,D_2288(%bx)
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    %ax,-0x6(%bp)
L_0d32:
	cmpw   $0x0,0x8(%bp)
	jle   L_0d72
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	pushw  0x8(%bp)
	call   L_1515
	add    $0x6,%sp
	pushw  0x8(%bp)
	pushw  D_241a
	pushw  D_241a
	pushw  0x8(%bp)
	/* TODO: data ref: mov    $0x2dc2,%ax */
	mov    $D_0dc2,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	mov    0x8(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    D_241a,%bx
	shl    %bx
	add    %ax,D_2288(%bx)
L_0d72:
	pushw  D_241a
	/* TODO: data ref: mov    $0x2de0,%ax */
	mov    $D_0de0,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	cmpw   $0x0,0x6(%bp)
	jle   L_0dec
	cmpw   $0x6,D_2282
	jl   L_0d92
	mov    D_2282,%ax
	jmp   L_0d95
L_0d92:
	mov    $0x6,%ax
L_0d95:
	push   %ax
	pushw  0x6(%bp)
	pushw  -0xc(%bp)
	pushw  0x6(%bp)
	pushw  D_241a
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    0x6(%bp),%bx
	shl    %bx
	sub    D_2350(%bx),%ax
	add    -0xa(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x2de8,%ax */
	mov    $D_0de8,%ax
	push   %ax
	call   L_39ae
	add    $0xe,%sp
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    0x6(%bp),%bx
	shl    %bx
	sub    D_2350(%bx),%ax
	add    -0xa(%bp),%ax
	neg    %ax
	push   %ax
	pushw  0x6(%bp)
	pushw  D_241a
	/* TODO: data ref: mov    $0x2e11,%ax */
	mov    $D_0e11,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
L_0dec:
	pushw  0x4(%bp)
	pushw  D_241a
	pushw  0x4(%bp)
	pushw  0x4(%bp)
	pushw  D_241a
	/* TODO: data ref: mov    $0x2e30,%ax */
	mov    $D_0e30,%ax
	push   %ax
	call   L_39ae
	add    $0xc,%sp
	cmpw   $0x0,0x8(%bp)
	jle   L_0e5d
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    0x8(%bp),%bx
	shl    %bx
	add    D_2350(%bx),%ax
	push   %ax
	pushw  0x8(%bp)
	pushw  D_241a
	cmpw   $0x6,D_2282
	jl   L_0e30
	mov    D_2282,%ax
	jmp   L_0e33
L_0e30:
	mov    $0x6,%ax
L_0e33:
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	pushw  0x8(%bp)
	pushw  D_241a
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    0x8(%bp),%bx
	shl    %bx
	add    D_2350(%bx),%ax
	neg    %ax
	push   %ax
	/* TODO: data ref: mov    $0x2e63,%ax */
	mov    $D_0e63,%ax
	push   %ax
	call   L_39ae
	add    $0x14,%sp
L_0e5d:
	/* TODO: data ref: mov    $0x2eaa,%ax */
	mov    $D_0eaa,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	mov    -0x6(%bp),%ax
	add    -0xa(%bp),%ax
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2350(%bx)
	cmpw   $0x0,D_2418
	je   L_0ea7
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2350(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2288(%bx)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	pushw  D_241a
	/* TODO: data ref: mov    $0x2eac,%ax */
	mov    $D_0eac,%ax
	push   %ax
	call   L_39ae
	add    $0xe,%sp
L_0ea7:
	pushw  0x4(%bp)
	call   L_14d1
	pop    %cx
	cmpw   $0x0,0x6(%bp)
	jle   L_0ebb
	pushw  0x6(%bp)
	call   L_14d1
	pop    %cx
L_0ebb:
	cmpw   $0x0,0x8(%bp)
	jle   L_0ec8
	pushw  0x8(%bp)
	call   L_14d1
	pop    %cx
L_0ec8:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0ed0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_0edd:
	call   L_1487
	mov    %ax,D_241a
	mov    0x4(%bp),%ax
	jmp   L_0f17
	/* TODO: data ref: movw   $0x2ee2,-0x6(%bp) */
	movw   $D_0ee2,-0x6(%bp)
	jmp   L_0f26
	/* TODO: data ref: movw   $0x2ee7,-0x6(%bp) */
	movw   $D_0ee7,-0x6(%bp)
	jmp   L_0f26
	/* TODO: data ref: movw   $0x2eec,-0x6(%bp) */
	movw   $D_0eec,-0x6(%bp)
	jmp   L_0f26
	/* TODO: data ref: movw   $0x2ef1,-0x6(%bp) */
	movw   $D_0ef1,-0x6(%bp)
	jmp   L_0f26
L_0f04:
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x2ef6,%ax */
	mov    $D_0ef6,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
	jmp   L_0f26
L_0f17:
	sub    $0x10d,%ax
	cmp    $0x4,%ax
	ja   L_0f04
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2ed8(%bx)
L_0f26:
	pushw  -0x6(%bp)
	pushw  D_241a
	/* TODO: data ref: mov    $0x2f0d,%ax */
	mov    $D_0f0d,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	mov    D_241a,%bx
	shl    %bx
	movw   $0x28,D_2288(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x0,D_2350(%bx)
	cmpw   $0x0,D_2418
	je   L_0f7b
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2350(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2288(%bx)
	pushw  -0x6(%bp)
	pushw  D_241a
	/* TODO: data ref: mov    $0x2f18,%ax */
	mov    $D_0f18,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
L_0f7b:
	mov    D_241a,%bx
	shl    %bx
	movw   $0x31,D_25f8(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x31,D_26c0(%bx)
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f98:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0fa4:
	cmpw   $0x0,0x6(%bp)
	je   L_0fc1
	cmpw   $0x0,0x8(%bp)
	je   L_0fc1
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_2e1b
	add    $0x6,%sp
	jmp   L_0fef
L_0fc1:
	cmpw   $0x0,0x6(%bp)
	je   L_0fd9
	pushw  0x6(%bp)
	mov    $0x123,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2c74
	add    $0x6,%sp
	jmp   L_0fef
L_0fd9:
	cmpw   $0x0,0x8(%bp)
	je   L_0fef
	pushw  0x8(%bp)
	mov    $0x122,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2c74
	add    $0x6,%sp
L_0fef:
	cmpw   $0x0,D_2418
	je   L_1015
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_2350(%bx)
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_2288(%bx)
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x2f44,%ax */
	mov    $D_0f44,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
L_1015:
	mov    0x4(%bp),%bx
	shl    %bx
	movw   $0x31,D_26c0(%bx)
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1024:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_1031:
	call   L_1487
	mov    %ax,D_241a
	/* TODO: data ref: movw   $0x2f60,-0x6(%bp) */
	movw   $D_0f60,-0x6(%bp)
	pushw  -0x6(%bp)
	pushw  D_241a
	/* TODO: data ref: mov    $0x2f65,%ax */
	mov    $D_0f65,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	mov    D_241a,%bx
	shl    %bx
	movw   $0x28,D_2288(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x0,D_2350(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x31,D_25f8(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x31,D_26c0(%bx)
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
L_108e:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_10ba
	add    $0x4,%sp
	push   %ax
	call   L_10a3
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_10a3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_10af:
	pushw  0x4(%bp)
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_10ba:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
L_10c9:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_1544
	add    $0x4,%sp
	call   L_1e6c
L_10d8:
	mov    $D_2868,%ax
	push   %ax
	call   L_1260
	pop    %cx
	mov    %ax,-0x8(%bp)
	cmp    $0xffff,%ax
	jne   L_10eb
	jmp   L_11c7
L_10eb:
	mov    D_2858,%ax
	mov    %ax,D_285a
	cmpb   $0x2e,D_2868
	jne   L_1104
	cmpb   $0x45,D_2869
	jne   L_1104
	cmpb   $0x51,D_286a
L_1104:
	je   L_1109
	jmp   L_1238
L_1109:
	movw   $L_000b,-0x6(%bp)
L_110e:
	cmpw   $0x64,-0x6(%bp)
	jge   L_1124
	mov    -0x6(%bp),%bx
	shl    %bx
	movw   $0x0,D_2790(%bx)
	incw   -0x6(%bp)
	jmp   L_110e
L_1124:
	mov    $D_2868,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2f92,%ax */
	mov    $D_0f92,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	/* TODO: data ref: mov    $0x2f95,%ax */
	mov    $D_0f95,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	/* TODO: data ref: movw   $0x0,0x2f42 */
	movw   $0x0,D_0f42
	call   L_1668
	call   L_0194
	cmpw   $0x0,D_2422
	jle   L_119c
	pushw  D_2422
	pushw  D_2422
	/* TODO: data ref: mov    $0x2fb0,%ax */
	mov    $D_0fb0,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	/* TODO: data ref: pushw  0x2f42 */
	pushw  D_0f42
	/* TODO: data ref: mov    $0x2fc2,%ax */
	mov    $D_0fc2,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	pushw  D_2860
	pushw  D_2860
	/* TODO: data ref: mov    $0x2fcd,%ax */
	mov    $D_0fcd,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	pushw  D_2422
	/* TODO: data ref: mov    $0x2fe3,%ax */
	mov    $D_0fe3,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	cmpw   $0x0,D_2864
	jne   L_119c
	/* TODO: data ref: mov    $0x2fee,%ax */
	mov    $D_0fee,%ax
	push   %ax
	call   L_39ae
	pop    %cx
L_119c:
	/* TODO: data ref: mov    $0x2ff5,%ax */
	mov    $D_0ff5,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	/* TODO: data ref: mov    $0x300a,%ax */
	mov    $D_100a,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	cmpw   $0xffff,D_285c
	jne   L_11d9
	decw   D_21e0
	jl   L_11c9
	mov    D_21de,%bx
	incw   D_21de
	movb   $0xa,(%bx)
	mov    $0xa,%ax
L_11c7:
	jmp   L_11d7
L_11c9:
	mov    $D_21de,%ax
	push   %ax
	mov    $0xa,%ax
	push   %ax
	call   L_478a
	add    $0x4,%sp
L_11d7:
	jmp   L_1257
L_11d9:
	decw   D_21e0
	jl   L_11f3
	mov    D_21de,%bx
	incw   D_21de
	mov    D_285c,%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_1201
L_11f3:
	mov    $D_21de,%ax
	push   %ax
	pushw  D_285c
	call   L_478a
	add    $0x4,%sp
L_1201:
	cmp    $0xa,%ax
L_1204:
	jne   L_1209
	jmp   L_10d8
L_1209:
	decw   D_21e0
	jl   L_1223
	call   L_174c
	mov    D_21de,%bx
	incw   D_21de
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_1231
L_1223:
	mov    $D_21de,%ax
	push   %ax
	call   L_174c
	push   %ax
	call   L_478a
	add    $0x4,%sp
L_1231:
	cmp    $0xa,%ax
	je   L_1204
	jmp   L_1209
L_1238:
	mov    -0x8(%bp),%ax
	/* TODO: data ref: cmp    0x2f3e,%ax */
	cmp    D_0f3e,%ax
	jne   L_1247
	call   L_12be
L_1244:
	jmp   L_10d8
L_1247:
	mov    $D_2868,%ax
	push   %ax
	/* TODO: data ref: mov    $0x300e,%ax */
	mov    $D_100e,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	jmp   L_1244
L_1257:
	xor    %ax,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1260:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_126c:
	mov    0x4(%bp),%di
L_1270:
	call   L_174c
	mov    %ax,%si
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	cmp    $0xa,%ax
	je   L_12ab
	cmp    $0xffff,%si
	je   L_12ab
	/* TODO: data ref: cmp    0x2f3e,%si */
	cmp    D_0f3e,%si
	je   L_12ab
	cmp    $D_2d18,%di
	jb   L_1270
	mov    $D_2868,%ax
	push   %ax
	/* TODO: data ref: mov    $0x3011,%ax */
	mov    $D_1011,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
	movb   $0x0,D_2d18
L_12ab:
	/* TODO: data ref: cmp    0x2f3e,%si */
	cmp    D_0f3e,%si
	jne   L_12b2
	dec    %di
L_12b2:
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_12be:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_12cb:
	/* TODO: data ref: mov    $0x302d,%ax */
	mov    $D_102d,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	call   L_1487
	mov    %ax,-0x6(%bp)
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x3048,%ax */
	mov    $D_1048,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_12e6:
	cmpb   $0x0,D_2868
	je   L_12fe
	mov    $D_2868,%ax
	push   %ax
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x3051,%ax */
	mov    $D_1051,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_12fe:
	call   L_1668
	call   L_0194
	cmpw   $0x0,D_2422
	jle   L_1324
	pushw  D_2422
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x305d,%ax */
	mov    $D_105d,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	pushw  D_2422
	call   L_14d1
	pop    %cx
L_1324:
	/* TODO: data ref: mov    $0x306b,%ax */
	mov    $D_106b,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	mov    $D_2868,%ax
	push   %ax
	call   L_1260
	pop    %cx
	/* TODO: data ref: cmp    0x2f3e,%ax */
	cmp    D_0f3e,%ax
	je   L_12e6
	cmpb   $0x0,D_2868
	je   L_1352
	mov    $D_2868,%ax
	push   %ax
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x3080,%ax */
	mov    $D_1080,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_1352:
	/* TODO: data ref: mov    $0x308b,%ax */
	mov    $D_108b,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x30a0,%ax */
	mov    $D_10a0,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	pushw  -0x6(%bp)
	call   L_14d1
	pop    %cx
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1373:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
L_1382:
	cmpw   $0x0,D_2418
	je   L_13a8
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_2350(%bx)
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_2288(%bx)
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x30a7,%ax */
	mov    $D_10a7,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
L_13a8:
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    %ax,D_2860
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x30c3,%ax */
	mov    $D_10c3,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    0x4(%bp),%bx
	shl    %bx
	sub    D_2350(%bx),%ax
	sub    $0x3c,%ax
	mov    %ax,-0x6(%bp)
	cmp    $0x0,%ax
	jle   L_13eb
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x30d0,%ax */
	mov    $D_10d0,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_13eb:
	mov    0x4(%bp),%bx
	shl    %bx
	cmpw   $0x31,D_25f8(%bx)
	jne   L_13fc
	/* TODO: data ref: mov    $0x30f8,%ax */
	mov    $D_10f8,%ax
	jmp   L_13ff
L_13fc:
	/* TODO: data ref: mov    $0x30fb,%ax */
	mov    $D_10fb,%ax
L_13ff:
	push   %ax
	pushw  0x4(%bp)
	/* TODO: data ref: pushw  0x2f3a */
	pushw  D_0f3a
	/* TODO: data ref: pushw  0x2f3c */
	pushw  D_0f3c
	/* TODO: data ref: mov    $0x30da,%ax */
	mov    $D_10da,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2350(%bx),%ax
	sub    $0x14,%ax
	mov    %ax,-0x8(%bp)
	cmp    $0x0,%ax
	jle   L_1436
	pushw  -0x8(%bp)
	/* TODO: data ref: mov    $0x30fc,%ax */
	mov    $D_10fc,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_1436:
	decw   D_21e0
	jl   L_144c
	mov    D_21de,%bx
	incw   D_21de
	movb   $0xa,(%bx)
	mov    $0xa,%ax
	jmp   L_145a
L_144c:
	mov    $D_21de,%ax
	push   %ax
	mov    $0xa,%ax
	push   %ax
	call   L_478a
	add    $0x4,%sp
L_145a:
	mov    0x4(%bp),%ax
	mov    %ax,D_2422
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1467:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1473:
	mov    0x4(%bp),%ax
	cmp    0x6(%bp),%ax
	jle   L_1480
	mov    0x4(%bp),%ax
	jmp   L_1483
L_1480:
	mov    0x6(%bp),%ax
L_1483:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1487:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_1494:
	movw   $L_000b,-0x6(%bp)
L_1499:
	cmpw   $0x64,-0x6(%bp)
	jge   L_14b9
	mov    -0x6(%bp),%bx
	shl    %bx
	incw   D_2790(%bx)
	mov    D_2790(%bx),%ax
	dec    %ax
	jne   L_14b4
	mov    -0x6(%bp),%ax
	jmp   L_14cc
L_14b4:
	incw   -0x6(%bp)
	jmp   L_1499
L_14b9:
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x3104,%ax */
	mov    $D_1104,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
	xor    %ax,%ax
L_14cc:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_14d1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_14dd:
	mov    0x4(%bp),%bx
	shl    %bx
	movw   $0x0,D_2790(%bx)
	pop    %di
	pop    %si
	pop    %bp
	ret


L_14ec:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_14f8:
	cmpw   $0x6,0x4(%bp)
	jl   L_1503
	mov    0x4(%bp),%ax
	jmp   L_1506
L_1503:
	mov    $0x6,%ax
L_1506:
	push   %ax
	/* TODO: data ref: mov    $0x3118,%ax */
	mov    $D_1118,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1515:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1521:
	pushw  0x8(%bp)
	cmpw   $0x6,0x6(%bp)
	jl   L_152f
	mov    0x6(%bp),%ax
	jmp   L_1532
L_152f:
	mov    $0x6,%ax
L_1532:
	push   %ax
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x3120,%ax */
	mov    $D_1120,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1544:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1550:
	decw   0x4(%bp)
	mov    0x4(%bp),%ax
	mov    %ax,D_2862
	mov    0x6(%bp),%ax
	mov    %ax,D_278a
L_155f:
	cmpw   $0x0,D_2862
	jg   L_1569
	jmp   L_1602
L_1569:
	mov    D_278a,%bx
	mov    0x2(%bx),%bx
	cmpb   $0x2d,(%bx)
	je   L_1578
	jmp   L_1602
L_1578:
	mov    D_278a,%bx
	mov    0x2(%bx),%bx
	mov    0x1(%bx),%al
	cbtw
	jmp   L_15e7
	mov    D_278a,%bx
	mov    0x2(%bx),%bx
	mov    0x2(%bx),%al
	cbtw
	/* TODO: data ref: mov    %ax,0x2f3e */
	mov    %ax,D_0f3e
	mov    D_278a,%bx
	mov    0x2(%bx),%bx
	mov    0x3(%bx),%al
	cbtw
	/* TODO: data ref: mov    %ax,0x2f40 */
	mov    %ax,D_0f40
	jmp   L_15e5
	mov    D_278a,%bx
	mov    0x2(%bx),%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_382e
	pop    %cx
	/* TODO: data ref: mov    %ax,0x2f3a */
	mov    %ax,D_0f3a
	jmp   L_15e5
	mov    D_278a,%bx
	mov    0x2(%bx),%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_382e
	pop    %cx
	/* TODO: data ref: mov    %ax,0x2f38 */
	mov    %ax,D_0f38
	jmp   L_15e5
	mov    D_278a,%bx
	mov    0x2(%bx),%bx
	mov    0x2(%bx),%al
	cbtw
	/* TODO: data ref: mov    %ax,0x2f3c */
	mov    %ax,D_0f3c
	jmp   L_15f6
	incw   D_2864
	jmp   L_15f6
L_15df:
	movw   $0x1,D_2418
L_15e5:
	jmp   L_15f6
L_15e7:
	sub    $0x64,%ax
	cmp    $0xf,%ax
	ja   L_15df
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2f72(%bx)
L_15f6:
	decw   D_2862
	addw   $0x2,D_278a
	jmp   L_155f
L_1602:
	movw   $0x1,D_278c
	movw   $0x1,D_2858
	cmpw   $0x0,D_2862
	jg   L_1627
	/* dual-reloc split: movw $D_21d6,D_285e */
	mov	$D_21d6,%ax
	mov	%ax,D_285e
	mov    D_278a,%bx
	/* TODO: data ref: mov    0x2f70,%ax */
	mov    D_0f70,%ax
	mov    %ax,0x2(%bx)
	jmp   L_1654
L_1627:
	/* TODO: data ref: mov    $0x3138,%ax */
	mov    $D_1138,%ax
	push   %ax
	mov    D_278a,%bx
	pushw  0x2(%bx)
	call   L_389a
	add    $0x4,%sp
	mov    %ax,D_285e
	or     %ax,%ax
	jne   L_1654
	mov    D_278a,%bx
	pushw  0x2(%bx)
	/* TODO: data ref: mov    $0x313a,%ax */
	mov    $D_113a,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
L_1654:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1658:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1664:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1668:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1674:
	movw   $0x0,D_241e
	/* TODO: data ref: mov    0x2f3a,%ax */
	mov    D_0f3a,%ax
	mov    %ax,D_2282
	/* TODO: data ref: mov    0x2f3c,%ax */
	mov    D_0f3c,%ax
	mov    %ax,D_2788
	pushw  D_2282
	call   L_14ec
	pop    %cx
	pushw  D_2788
	/* TODO: data ref: mov    $0x314d,%ax */
	mov    $D_114d,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_16a0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_16ac:
	cmpw   $0x0,0x4(%bp)
	jle   L_16ba
	/* TODO: data ref: mov    $0x3155,%ax */
	mov    $D_1155,%ax
	push   %ax
	call   L_39ae
	pop    %cx
L_16ba:
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	call   L_39ae
	add    $0x4,%sp
	pushw  D_2858
	pushw  D_285a
	mov    D_278c,%ax
	shl    %ax
	add    D_278a,%ax
	mov    %ax,%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x3167,%ax */
	mov    $D_1167,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
	/* TODO: data ref: mov    $0x318a,%ax */
	mov    $D_118a,%ax
	push   %ax
	mov    $D_21e6,%ax
	push   %ax
	call   L_3802
	add    $0x4,%sp
	cmpw   $0x0,0x4(%bp)
	jle   L_1707
	/* TODO: data ref: mov    $0x3190,%ax */
	mov    $D_1190,%ax
	push   %ax
	mov    $D_21e6,%ax
	push   %ax
	call   L_3802
	add    $0x4,%sp
L_1707:
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    $D_21e6,%ax
	push   %ax
	call   L_3802
	add    $0x6,%sp
	pushw  D_2858
	pushw  D_285a
	mov    D_278c,%ax
	shl    %ax
	add    D_278a,%ax
	mov    %ax,%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x319e,%ax */
	mov    $D_119e,%ax
	push   %ax
	mov    $D_21e6,%ax
	push   %ax
	call   L_3802
	add    $0xa,%sp
	cmpw   $0x0,0x4(%bp)
	jle   L_1748
	mov    $0x1,%ax
	push   %ax
	call   L_10a3
	pop    %cx
L_1748:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_174c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1758:
	/* TODO: data ref: cmpw   $0x4de8,0x31c2 */
	cmpw   $D_2de8,D_11c2
	jbe   L_176d
	/* TODO: data ref: decw   0x31c2 */
	decw   D_11c2
	/* TODO: data ref: mov    0x31c2,%bx */
	mov    D_11c2,%bx
	mov    (%bx),%al
	cbtw
	jmp   L_17b9
L_176d:
	mov    D_285e,%bx
	decw   0x2(%bx)
	jl   L_1789
	mov    D_285e,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_1791
L_1789:
	pushw  D_285e
	call   L_39d8
	pop    %cx
L_1791:
	mov    %ax,D_285c
	cmp    $0xa,%ax
	jne   L_179d
	incw   D_2858
L_179d:
	cmpw   $0xffff,D_285c
	je   L_17a9
	mov    D_285c,%ax
	jmp   L_17b9
L_17a9:
	incw   D_278c
	mov    D_278c,%ax
	cmp    D_2862,%ax
	jle   L_17bb
	mov    $0xffff,%ax
L_17b9:
	jmp   L_1808
L_17bb:
	pushw  D_285e
	call   L_48e7
	pop    %cx
	movw   $0x1,D_2858
	/* TODO: data ref: mov    $0x31c4,%ax */
	mov    $D_11c4,%ax
	push   %ax
	mov    D_278c,%ax
	shl    %ax
	add    D_278a,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_389a
	add    $0x4,%sp
	mov    %ax,D_285e
	or     %ax,%ax
	je   L_17ea
	jmp   L_1758
L_17ea:
	mov    D_278c,%ax
	shl    %ax
	add    D_278a,%ax
	mov    %ax,%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x31c6,%ax */
	mov    $D_11c6,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
	mov    $0xffff,%ax
L_1808:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_180c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1818:
	mov    0x4(%bp),%di
	mov    %di,%si
L_181e:
	mov    %si,%bx
	inc    %si
	cmpb   $0x0,(%bx)
	jne   L_181e
	dec    %si
	/* TODO: data ref: cmpw   $0x4f14,0x31c2 */
	cmpw   $D_2f14,D_11c2
	jb   L_183d
	/* TODO: data ref: mov    $0x31d9,%ax */
	mov    $D_11d9,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x4,%sp
L_183d:
	cmp    %di,%si
	jbe   L_1853
	dec    %si
	mov    (%si),%al
	cbtw
	/* TODO: data ref: mov    0x31c2,%bx */
	mov    D_11c2,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	/* TODO: data ref: incw   0x31c2 */
	incw   D_11c2
	jmp   L_183d
L_1853:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1857:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_1864:
	call   L_174c
	mov    %ax,%di
	cmp    $0x20,%ax
	je   L_1864
	cmp    $0xa,%di
L_1871:
	je   L_1864
	mov    %di,D_241c
	mov    %di,%ax
	jmp   L_1920
L_187c:
	mov    $0xffff,%ax
	jmp   L_1898
L_1881:
	mov    $0x103,%ax
	jmp   L_1898
L_1886:
	mov    $0x104,%ax
	jmp   L_1898
L_188b:
	mov    $0x105,%ax
	jmp   L_1898
L_1890:
	mov    $0x7b,%ax
	jmp   L_1898
L_1895:
	mov    $0x7d,%ax
L_1898:
	jmp   L_191e
L_189b:
	movw   $0x0,D_2d18
L_18a1:
	call   L_174c
	mov    %ax,%di
	cmp    $0x22,%ax
	je   L_18f6
	cmp    $0xa,%di
	je   L_18f6
	cmp    $0x5c,%di
	jne   L_18cc
	call   L_174c
	mov    %ax,%di
	cmp    $0x22,%ax
	je   L_18cc
	mov    D_2d18,%bx
	movb   $0x5c,D_30a8(%bx)
	incw   D_2d18
L_18cc:
	mov    D_2d18,%bx
	mov    %di,%cx
	mov    %cl,D_30a8(%bx)
	incw   D_2d18
	cmpw   $0x190,D_2d18
	jl   L_18a1
	mov    $D_30a8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x31eb,%ax */
	mov    $D_11eb,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
	jmp   L_18a1
L_18f6:
	mov    D_2d18,%bx
	movb   $0x0,D_30a8(%bx)
	/* dual-reloc split: movw $D_30a8,D_241c */
	mov	$D_30a8,%ax
	mov	%ax,D_241c
	cmp    $0xa,%di
	jne   L_191b
	mov    $D_30a8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x320b,%ax */
	mov    $D_120b,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
L_191b:
	mov    $0x102,%ax
L_191e:
	jmp   L_1963
L_1920:
	cmp    $0x5e,%ax
	jne   L_1928
	jmp   L_1886
L_1928:
	jg   L_1942
	cmp    $0xffff,%ax
	jne   L_1932
	jmp   L_187c
L_1932:
	cmp    $0x9,%ax
	jne   L_193a
	jmp   L_188b
L_193a:
	cmp    $0x22,%ax
	jne   L_1942
	jmp   L_189b
L_1942:
	cmp    $0x7b,%ax
	jne   L_194a
	jmp   L_1890
L_194a:
	cmp    $0x7d,%ax
	jne   L_1952
	jmp   L_1895
L_1952:
	cmp    $0x7e,%ax
	jne   L_195a
	jmp   L_1881
L_195a:
	/* TODO: data ref: cmp    0x2f40,%di */
	cmp    D_0f40,%di
	jne   L_1966
	mov    $0xffff,%ax
L_1963:
	jmp   L_1a1e
L_1966:
	/* TODO: data ref: mov    0x31c2,%bx */
	mov    D_11c2,%bx
	mov    %di,%cx
	mov    %cl,(%bx)
	/* TODO: data ref: incw   0x31c2 */
	incw   D_11c2
	mov    $0x190,%ax
	push   %ax
	mov    $D_30a8,%ax
	push   %ax
	call   L_1a91
	add    $0x4,%sp
	cmpw   $0x0,D_2418
	je   L_1995
	mov    $D_30a8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x321e,%ax */
	mov    $D_121e,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_1995:
	xor    %ax,%ax
	push   %ax
	mov    $D_30a8,%ax
	push   %ax
	mov    $D_3238,%ax
	push   %ax
	call   L_1db4
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_19ee
	/* TODO: data ref: mov    0x31c2,%bx */
	mov    D_11c2,%bx
	movb   $0x20,(%bx)
	/* TODO: data ref: incw   0x31c2 */
	incw   D_11c2
	mov    -0x6(%bp),%bx
	pushw  0x2(%bx)
	call   L_180c
	pop    %cx
	/* TODO: data ref: mov    0x31c2,%bx */
	mov    D_11c2,%bx
	movb   $0x20,(%bx)
	/* TODO: data ref: incw   0x31c2 */
	incw   D_11c2
	cmpw   $0x0,D_2418
	jne   L_19d7
	jmp   L_1871
L_19d7:
	mov    -0x6(%bp),%bx
	pushw  0x2(%bx)
	mov    $D_30a8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x3232,%ax */
	mov    $D_1232,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_19eb:
	jmp   L_1864
L_19ee:
	xor    %ax,%ax
	push   %ax
	mov    $D_30a8,%ax
	push   %ax
	mov    $D_2d20,%ax
	push   %ax
	call   L_1db4
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_1a20
	cmpw   $0x0,D_2418
	je   L_1a1b
	mov    $D_30a8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x3243,%ax */
	mov    $D_1243,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_1a1b:
	mov    $0x101,%ax
L_1a1e:
	jmp   L_1a8c
L_1a20:
	mov    -0x6(%bp),%bx
	cmpw   $0x117,0x2(%bx)
	je   L_1a3e
	mov    -0x6(%bp),%bx
	cmpw   $0x119,0x2(%bx)
	je   L_1a3e
	mov    -0x6(%bp),%bx
	cmpw   $0x118,0x2(%bx)
	jne   L_1a4a
L_1a3e:
	mov    -0x6(%bp),%bx
	pushw  0x2(%bx)
	call   L_1c48
	pop    %cx
L_1a48:
	jmp   L_19eb
L_1a4a:
	mov    -0x6(%bp),%bx
	cmpw   $0x11a,0x2(%bx)
	jne   L_1a59
	call   L_1d49
	jmp   L_1a48
L_1a59:
	mov    -0x6(%bp),%bx
	cmpw   $0x11b,0x2(%bx)
	jne   L_1a68
	call   L_30f7
	jmp   L_1a48
L_1a68:
	mov    -0x6(%bp),%bx
	cmpw   $0x11c,0x2(%bx)
	jne   L_1a77
	call   L_0bd6
	jmp   L_1a48
L_1a77:
	mov    -0x6(%bp),%bx
	cmpw   $0x11d,0x2(%bx)
	jne   L_1a86
	call   L_1d2c
	jmp   L_1a48
L_1a86:
	mov    -0x6(%bp),%bx
	mov    0x2(%bx),%ax
L_1a8c:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1a91:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_1a9e:
	mov    0x6(%bp),%di
	mov    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
L_1aa8:
	call   L_174c
	mov    %ax,%si
	cmp    $0x20,%ax
	je   L_1aa8
	cmp    $0xa,%si
	je   L_1aa8
	cmp    $0xffff,%si
	jne   L_1ac5
	mov    0x4(%bp),%bx
	movb   $0x0,(%bx)
	jmp   L_1b6c
L_1ac5:
	cmp    $0x20,%si
	je   L_1af1
	cmp    $0x9,%si
	je   L_1af1
	cmp    $0xa,%si
	je   L_1af1
	cmp    $0x7b,%si
	je   L_1af1
	cmp    $0x7d,%si
	je   L_1af1
	cmp    $0x22,%si
	je   L_1af1
	cmp    $0x7e,%si
	je   L_1b30
	cmp    $0x5e,%si
	je   L_1b30
	/* TODO: data ref: cmp    0x2f40,%si */
	cmp    D_0f40,%si
L_1af1:
	je   L_1b30
	cmp    $0x5c,%si
	jne   L_1b0b
	call   L_174c
	mov    %ax,%si
	cmp    $0x22,%ax
	je   L_1b0b
	mov    -0x6(%bp),%bx
	movb   $0x5c,(%bx)
	incw   -0x6(%bp)
L_1b0b:
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	dec    %di
	jg   L_1b29
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x325a,%ax */
	mov    $D_125a,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
L_1b29:
	call   L_174c
	mov    %ax,%si
	jmp   L_1ac5
L_1b30:
	cmp    $0x7b,%si
	je   L_1b54
	cmp    $0x7d,%si
	je   L_1b54
	cmp    $0x22,%si
	je   L_1b54
	cmp    $0x7e,%si
	je   L_1b54
	cmp    $0x5e,%si
	je   L_1b54
	cmp    $0x9,%si
	je   L_1b54
	/* TODO: data ref: cmp    0x2f40,%si */
	cmp    D_0f40,%si
	jne   L_1b60
L_1b54:
	/* TODO: data ref: mov    0x31c2,%bx */
	mov    D_11c2,%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	/* TODO: data ref: incw   0x31c2 */
	incw   D_11c2
L_1b60:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%ax
	mov    %ax,D_241c
L_1b6c:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b71:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
L_1b80:
	call   L_174c
	mov    %ax,-0x6(%bp)
	cmp    $0x20,%ax
	je   L_1b80
	cmpw   $0x9,-0x6(%bp)
	je   L_1b80
	cmpw   $0xa,-0x6(%bp)
	je   L_1b80
	cmpw   $0x0,0x6(%bp)
	je   L_1bd2
	movw   $0x0,-0xa(%bp)
L_1ba2:
	call   L_174c
	mov    %ax,-0x8(%bp)
	cmp    -0x6(%bp),%ax
	je   L_1bfc
	cmpw   $0xffff,-0x8(%bp)
	je   L_1bfc
	mov    -0xa(%bp),%ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    -0x8(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	mov    -0xa(%bp),%ax
	cmp    0x8(%bp),%ax
	jl   L_1ba2
	mov    $0x1,%ax
	jmp   L_1c1b
L_1bd2:
	mov    0x4(%bp),%bx
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	movw   $0x1,-0xa(%bp)
L_1be1:
	call   L_174c
	mov    %ax,-0x8(%bp)
	cmp    $0x20,%ax
	je   L_1c1d
	cmpw   $0x9,-0x8(%bp)
	je   L_1c1d
	cmpw   $0xa,-0x8(%bp)
	je   L_1c1d
	cmpw   $0xffff,-0x8(%bp)
L_1bfc:
	je   L_1c1d
	mov    -0xa(%bp),%ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    -0x8(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	mov    -0xa(%bp),%ax
	cmp    0x8(%bp),%ax
	jl   L_1be1
	mov    $0x1,%ax
L_1c1b:
	jmp   L_1c41
L_1c1d:
	mov    0x4(%bp),%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	movb   $0x0,(%bx)
	cmpw   $0xffff,-0x8(%bp)
	jne   L_1c3f
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x3272,%ax */
	mov    $D_1272,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
L_1c3f:
	xor    %ax,%ax
L_1c41:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1c48:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
L_1c57:
	mov    $0x190,%ax
	push   %ax
	mov    $D_30a8,%ax
	push   %ax
	call   L_1a91
	add    $0x4,%sp
	cmpw   $0x117,0x4(%bp)
	je   L_1c80
	mov    $0x190,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $D_30a8,%ax
	push   %ax
	call   L_1b71
	add    $0x6,%sp
	jmp   L_1ce5
L_1c80:
	mov    $D_30a8,%ax
	push   %ax
	call   L_1cec
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    $0x190,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $D_30a8,%ax
	push   %ax
	call   L_1b71
	add    $0x6,%sp
	or     %ax,%ax
	je   L_1cb3
	mov    $D_30a8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x3293,%ax */
	mov    $D_1293,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
L_1cb3:
	mov    $D_30a8,%ax
	push   %ax
	call   L_1cec
	pop    %cx
	mov    %ax,-0x8(%bp)
	pushw  -0x8(%bp)
	pushw  -0x6(%bp)
	mov    $D_3238,%ax
	push   %ax
	call   L_1db4
	add    $0x6,%sp
	cmpw   $0x0,D_2418
	je   L_1ce5
	pushw  -0x8(%bp)
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x32b4,%ax */
	mov    $D_12b4,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_1ce5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1cec:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1cf8:
	pushw  0x4(%bp)
	call   L_3a86
	pop    %cx
	inc    %ax
	push   %ax
	call   L_495e
	pop    %cx
	mov    %ax,%di
	or     %di,%di
	jne   L_1d1c
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x32cd,%ax */
	mov    $D_12cd,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
L_1d1c:
	pushw  0x4(%bp)
	push   %di
	call   L_4c62
	add    $0x4,%sp
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d2c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1d38:
	/* TODO: data ref: mov    $0x32eb,%ax */
	mov    $D_12eb,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_16a0
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d49:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1d55:
	movw   $0x0,D_2422
	movw   $0x0,D_241a
	mov    $0x190,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $D_30a8,%ax
	push   %ax
	call   L_1b71
	add    $0x6,%sp
	or     %ax,%ax
	je   L_1d88
	mov    $D_30a8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x3307,%ax */
	mov    $D_1307,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
L_1d88:
	mov    D_30a8,%al
	cbtw
	/* TODO: data ref: mov    %ax,0x2f3e */
	mov    %ax,D_0f3e
	mov    D_30a9,%al
	cbtw
	/* TODO: data ref: mov    %ax,0x2f40 */
	mov    %ax,D_0f40
	/* TODO: data ref: cmpw   $0x6f,0x2f3e */
	cmpw   $0x6f,D_0f3e
	jne   L_1db0
	/* TODO: data ref: cmpw   $0x66,0x2f40 */
	cmpw   $0x66,D_0f40
	jne   L_1db0
	/* TODO: data ref: movw   $0x0,0x2f40 */
	movw   $0x0,D_0f40
	/* TODO: data ref: movw   $0x0,0x2f3e */
	movw   $0x0,D_0f3e
L_1db0:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1db4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_1dc1:
	mov    0x6(%bp),%ax
	mov    %ax,-0x6(%bp)
	xor    %si,%si
L_1dc9:
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1dde
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	add    %ax,%si
	incw   -0x6(%bp)
	jmp   L_1dc9
L_1dde:
	mov    %si,%ax
	cwtd
	mov    $0x64,%cx
	idiv   %cx
	mov    %dx,%si
	mov    %si,%ax
	shl    %ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%di
L_1df3:
	or     %di,%di
	je   L_1e1b
	pushw  (%di)
	pushw  0x6(%bp)
	call   L_4bb4
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_1e16
	cmpw   $0x0,0x8(%bp)
	je   L_1e12
	mov    0x8(%bp),%ax
	mov    %ax,0x2(%di)
L_1e12:
	mov    %di,%ax
	jmp   L_1e23
L_1e16:
	mov    0x4(%di),%di
	jmp   L_1df3
L_1e1b:
	cmpw   $0x0,0x8(%bp)
	jne   L_1e25
	xor    %ax,%ax
L_1e23:
	jmp   L_1e67
L_1e25:
	mov    $0x6,%ax
	push   %ax
	call   L_495e
	pop    %cx
	mov    %ax,%di
	or     %di,%di
	jne   L_1e41
	mov    $D_19df,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x4,%sp
L_1e41:
	mov    0x6(%bp),%ax
	mov    %ax,(%di)
	mov    0x8(%bp),%ax
	mov    %ax,0x2(%di)
	mov    %si,%ax
	shl    %ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,0x4(%di)
	mov    %si,%ax
	shl    %ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    %di,(%bx)
	mov    %di,%ax
L_1e67:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e6c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_1e79:
	movw   $0x0,-0x6(%bp)
L_1e7e:
	mov    -0x6(%bp),%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	/* TODO: data ref: cmpw   $0x0,0x3324(%bx) */
	cmpw   $0x0,D_1324(%bx)
	je   L_1eb4
	mov    -0x6(%bp),%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	/* TODO: data ref: pushw  0x3326(%bx) */
	pushw  D_1326(%bx)
	mov    -0x6(%bp),%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	/* TODO: data ref: pushw  0x3324(%bx) */
	pushw  D_1324(%bx)
	mov    $D_2d20,%ax
	push   %ax
	call   L_1db4
	add    $0x6,%sp
	incw   -0x6(%bp)
	jmp   L_1e7e
L_1eb4:
	movw   $0x0,-0x6(%bp)
L_1eb9:
	mov    -0x6(%bp),%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	/* TODO: data ref: cmpw   $0x0,0x33fc(%bx) */
	cmpw   $0x0,D_13fc(%bx)
	je   L_1eef
	mov    -0x6(%bp),%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	/* TODO: data ref: pushw  0x33fe(%bx) */
	pushw  D_13fe(%bx)
	mov    -0x6(%bp),%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	/* TODO: data ref: pushw  0x33fc(%bx) */
	pushw  D_13fc(%bx)
	mov    $D_3300,%ax
	push   %ax
	call   L_1db4
	add    $0x6,%sp
	incw   -0x6(%bp)
	jmp   L_1eb9
L_1eef:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ef4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1f00:
	/* TODO: data ref: movw   $0x1,0x2f42 */
	movw   $0x1,D_0f42
	pushw  0x4(%bp)
	pushw  0x4(%bp)
	mov    $D_19f6,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	mov    0x4(%bp),%ax
	mov    %ax,D_241a
	cmpw   $0x0,D_2418
	je   L_1f30
	pushw  0x4(%bp)
	mov    $D_1a09,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_1f30:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1f34:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1f40:
	/* TODO: data ref: movw   $0x1,0x2f42 */
	movw   $0x1,D_0f42
	cmpw   $0x0,0x4(%bp)
	jne   L_1f60
	call   L_1487
	mov    %ax,D_241a
	pushw  D_241a
	mov    $D_1a14,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_1f60:
	cmpw   $0x0,D_2418
	je   L_1f74
	pushw  0x4(%bp)
	mov    $D_1a28,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_1f74:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1f78:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_1f85:
	mov    D_241e,%ax
	sub    0x6(%bp),%ax
	dec    %ax
	mov    0x6(%bp),%bx
	shl    %bx
	mov    %ax,D_2558(%bx)
	cmpw   $0x0,D_2418
	je   L_1fe4
	pushw  0x4(%bp)
	mov    $D_1a36,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	mov    0x6(%bp),%ax
	inc    %ax
	mov    %ax,-0x6(%bp)
L_1fb0:
	mov    -0x6(%bp),%ax
	cmp    D_241e,%ax
	jge   L_1fd1
	mov    -0x6(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	mov    $D_1a45,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	incw   -0x6(%bp)
	jmp   L_1fb0
L_1fd1:
	mov    0x6(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	mov    $D_1a4a,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_1fe4:
	mov    D_241e,%bx
	shl    %bx
	mov    0x4(%bp),%ax
	mov    %ax,D_2558(%bx)
	incw   D_241e
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ffa:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xd8,%sp
L_200a:
	movw   $D_1a55,-0xdc(%bp)
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	mov    %ax,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	mov    0x4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_2027:
	mov    -0xa(%bp),%ax
	cmp    D_241e,%ax
	jge   L_205d
	incw   -0x8(%bp)
	cmpw   $0x0,D_2418
	je   L_204d
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	mov    $D_1a5a,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_204d:
	mov    -0xa(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	inc    %ax
	inc    %ax
	add    %ax,-0xa(%bp)
	jmp   L_2027
L_205d:
	movw   $0x1,-0xe(%bp)
L_2062:
	mov    -0xe(%bp),%ax
	cmp    -0x6(%bp),%ax
	jle   L_206d
	jmp   L_2154
L_206d:
	movw   $0x0,-0x12(%bp)
	movw   $0x0,-0x10(%bp)
	mov    0x4(%bp),%ax
	add    -0xe(%bp),%ax
	mov    %ax,-0xc(%bp)
	movw   $0x0,-0xa(%bp)
L_2085:
	mov    -0xa(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_20e6
	mov    -0xc(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    -0xc(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%dx
	mov    %dx,%bx
	shl    %bx
	sub    D_2350(%bx),%ax
	push   %ax
	pushw  -0x10(%bp)
	call   L_1467
	add    $0x4,%sp
	mov    %ax,-0x10(%bp)
	mov    -0xc(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	pushw  D_2350(%bx)
	pushw  -0x12(%bp)
	call   L_1467
	add    $0x4,%sp
	mov    %ax,-0x12(%bp)
	mov    -0x6(%bp),%ax
	inc    %ax
	inc    %ax
	add    %ax,-0xc(%bp)
	incw   -0xa(%bp)
	jmp   L_2085
L_20e6:
	cmpw   $0x0,D_2418
	je   L_2100
	pushw  -0x10(%bp)
	pushw  -0x12(%bp)
	pushw  -0xe(%bp)
	mov    $D_1a66,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
L_2100:
	mov    0x4(%bp),%ax
	add    -0xe(%bp),%ax
	mov    %ax,-0xc(%bp)
	movw   $0x0,-0xa(%bp)
L_210e:
	mov    -0xa(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_214e
	mov    -0xc(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	mov    -0x12(%bp),%ax
	mov    %ax,D_2350(%bx)
	mov    -0x12(%bp),%ax
	add    -0x10(%bp),%ax
	mov    -0xc(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%dx
	mov    %dx,%bx
	shl    %bx
	mov    %ax,D_2288(%bx)
	mov    -0x6(%bp),%ax
	inc    %ax
	inc    %ax
	add    %ax,-0xc(%bp)
	incw   -0xa(%bp)
	jmp   L_210e
L_214e:
	incw   -0xe(%bp)
	jmp   L_2062
L_2154:
	mov    0x4(%bp),%ax
	mov    %ax,-0xc(%bp)
	movw   $0x0,-0xa(%bp)
L_215f:
	mov    -0xa(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_21b4
	mov    -0xc(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	add    -0xc(%bp),%ax
	inc    %ax
	push   %ax
	mov    -0xc(%bp),%ax
	inc    %ax
	push   %ax
	mov    -0xc(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	add    -0xc(%bp),%ax
	inc    %ax
	mov    %ax,%bx
	shl    %bx
	pushw  D_2558(%bx)
	call   L_2906
	add    $0x6,%sp
	lea    -0xda(%bp),%ax
	mov    -0xa(%bp),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	mov    D_241a,%ax
	mov    %ax,(%bx)
	mov    -0x6(%bp),%ax
	inc    %ax
	inc    %ax
	add    %ax,-0xc(%bp)
	incw   -0xa(%bp)
	jmp   L_215f
L_21b4:
	call   L_1487
	mov    %ax,D_241a
	mov    -0xda(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2288(%bx)
	mov    -0xda(%bp),%bx
	shl    %bx
	mov    D_2350(%bx),%ax
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2350(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x0,D_25f8(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x0,D_26c0(%bx)
	cmpw   $0x0,D_2418
	je   L_2229
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2350(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2288(%bx)
	pushw  -0x8(%bp)
	pushw  -0x6(%bp)
	pushw  D_241a
	mov    $D_1a7d,%ax
	push   %ax
	call   L_39ae
	add    $0xc,%sp
L_2229:
	pushw  D_241a
	mov    $D_1aa3,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	movw   $0x0,-0xa(%bp)
L_223c:
	mov    -0xa(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_2288
	mov    -0x8(%bp),%ax
	dec    %ax
	cmp    %ax,-0xa(%bp)
	jne   L_2252
	mov    $D_1ab4,%ax
	jmp   L_2256
L_2252:
	mov    -0xdc(%bp),%ax
L_2256:
	push   %ax
	lea    -0xda(%bp),%ax
	mov    -0xa(%bp),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    $D_1aac,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	lea    -0xda(%bp),%ax
	mov    -0xa(%bp),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_14d1
	pop    %cx
	incw   -0xa(%bp)
	jmp   L_223c
L_2288:
	mov    $D_1ab5,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	mov    0x4(%bp),%ax
	mov    %ax,D_241e
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_229e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_22ab:
	mov    0x8(%bp),%ax
	mov    %ax,D_241a
	mov    0x6(%bp),%ax
	add    $0x31,%ax
	cwtd
	mov    $0x32,%cx
	idiv   %cx
	mov    $0x14,%dx
	imul   %dx
	mov    %ax,-0x6(%bp)
	pushw  D_241a
	mov    $D_1ab8,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	cmpw   $0x12d,0x4(%bp)
	je   L_22e1
	cmpw   $0x12c,0x4(%bp)
	jne   L_2303
L_22e1:
	pushw  0x8(%bp)
	pushw  -0x6(%bp)
	cmpw   $0x12c,0x4(%bp)
	jne   L_22f3
	mov    $D_1ad0,%ax
	jmp   L_22f6
L_22f3:
	mov    $D_1ad2,%ax
L_22f6:
	push   %ax
	mov    $D_1ac0,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
	jmp   L_2339
L_2303:
	cmpw   $0x12a,0x4(%bp)
	jne   L_231f
	pushw  -0x6(%bp)
	pushw  0x8(%bp)
	pushw  -0x6(%bp)
	mov    $D_1ad3,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
	jmp   L_2339
L_231f:
	cmpw   $0x12b,0x4(%bp)
	jne   L_2339
	pushw  -0x6(%bp)
	pushw  0x8(%bp)
	pushw  -0x6(%bp)
	mov    $D_1ae9,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
L_2339:
	cmpw   $0x0,D_2418
	je   L_2367
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2350(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2288(%bx)
	pushw  -0x6(%bp)
	pushw  0x4(%bp)
	pushw  0x8(%bp)
	mov    $D_1aff,%ax
	push   %ax
	call   L_39ae
	add    $0xc,%sp
L_2367:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_236c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
L_237b:
	call   L_1487
	mov    %ax,-0xa(%bp)
	mov    0x4(%bp),%ax
	mov    %ax,D_241a
	movw   $0x14,-0xc(%bp)
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    0x6(%bp),%bx
	shl    %bx
	add    D_2288(%bx),%ax
	mov    %ax,-0x6(%bp)
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	sub    -0xc(%bp),%ax
	mov    %ax,-0x8(%bp)
	cmpw   $0x0,D_2418
	je   L_23d1
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	pushw  D_241a
	mov    $D_1b24,%ax
	push   %ax
	call   L_39ae
	add    $0xc,%sp
L_23d1:
	pushw  0x4(%bp)
	pushw  D_2282
	pushw  0x4(%bp)
	call   L_1515
	add    $0x6,%sp
	pushw  0x6(%bp)
	pushw  D_2282
	pushw  0x6(%bp)
	call   L_1515
	add    $0x6,%sp
	pushw  0x4(%bp)
	pushw  -0xa(%bp)
	mov    $D_1b4e,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	pushw  0x6(%bp)
	pushw  -0xa(%bp)
	pushw  -0xa(%bp)
	pushw  0x6(%bp)
	mov    $D_1b5c,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	pushw  0x6(%bp)
	pushw  0x6(%bp)
	pushw  -0xa(%bp)
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    0x6(%bp),%bx
	shl    %bx
	sub    D_2350(%bx),%ax
	sub    -0xc(%bp),%ax
	push   %ax
	pushw  D_241a
	mov    $D_1b7a,%ax
	push   %ax
	call   L_39ae
	add    $0xc,%sp
	pushw  0x4(%bp)
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	neg    %ax
	mov    0x6(%bp),%bx
	shl    %bx
	add    D_2350(%bx),%ax
	mov    0x4(%bp),%bx
	shl    %bx
	sub    D_2350(%bx),%ax
	push   %ax
	pushw  0x4(%bp)
	pushw  0x6(%bp)
	mov    $D_1ba4,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	pushw  -0xc(%bp)
	pushw  -0xa(%bp)
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_2350(%bx)
	pushw  0x4(%bp)
	pushw  -0xa(%bp)
	mov    $D_1bc8,%ax
	push   %ax
	call   L_39ae
	add    $0xc,%sp
	mov    D_241a,%bx
	shl    %bx
	mov    -0x8(%bp),%ax
	mov    %ax,D_2350(%bx)
	mov    D_241a,%bx
	shl    %bx
	mov    -0x6(%bp),%ax
	mov    %ax,D_2288(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x0,D_25f8(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x0,D_26c0(%bx)
	pushw  0x6(%bp)
	call   L_14d1
	pop    %cx
	pushw  -0xa(%bp)
	call   L_14d1
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_24dc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
L_24eb:
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    %ax,-0xa(%bp)
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2350(%bx),%ax
	mov    %ax,-0xe(%bp)
	mov    0x6(%bp),%ax
	mov    %ax,D_241a
	mov    -0xa(%bp),%ax
	sub    -0xe(%bp),%ax
	sub    $0x14,%ax
	push   %ax
	mov    -0xe(%bp),%ax
	add    $0x14,%ax
	push   %ax
	call   L_1467
	add    $0x4,%sp
	cwtd
	mov    $0x14,%cx
	idiv   %cx
	mov    %ax,-0x6(%bp)
	cmp    $0x2,%ax
	jge   L_2533
	movw   $0x1,-0x6(%bp)
L_2533:
	mov    -0x6(%bp),%ax
	dec    %ax
	dec    %ax
	mov    %ax,-0x8(%bp)
	cmpw   $0x7b,0x4(%bp)
	je   L_2547
	cmpw   $0x7d,0x8(%bp)
	jne   L_2575
L_2547:
	mov    -0x6(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	or     %dx,%dx
	je   L_2559
	mov    -0x6(%bp),%ax
	jmp   L_255f
L_2559:
	incw   -0x6(%bp)
	mov    -0x6(%bp),%ax
L_255f:
	mov    %ax,-0x6(%bp)
	cmp    $0x3,%ax
	jge   L_256c
	movw   $0x3,-0x6(%bp)
L_256c:
	mov    -0x6(%bp),%ax
	sub    $0x3,%ax
	mov    %ax,-0x8(%bp)
L_2575:
	mov    -0x6(%bp),%ax
	shl    %ax
	mov    $0x14,%dx
	imul   %dx
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2288(%bx)
	mov    -0x6(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    $0x28,%dx
	imul   %dx
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2350(%bx)
	mov    -0x6(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	or     %dx,%dx
	jne   L_25b9
	mov    D_241a,%bx
	shl    %bx
	subw   $0x14,D_2350(%bx)
L_25b9:
	mov    -0xa(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	add    $0x14,%dx
	mov    %dx,-0x10(%bp)
	pushw  -0x10(%bp)
	pushw  D_241a
	mov    $D_1bfa,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	mov    0x4(%bp),%ax
	jmp   L_26de
L_25e4:
	cmpw   $0x1,-0x6(%bp)
	jg   L_25f4
	mov    $D_1c0b,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	jmp   L_2630
L_25f4:
	mov    $D_1c1a,%ax
	push   %ax
	mov    $D_1c15,%ax
	push   %ax
	mov    $D_1c10,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
	jmp   L_2630
L_260b:
	cmpw   $0x1,-0x6(%bp)
	jg   L_261b
	mov    $D_1c1f,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	jmp   L_2630
L_261b:
	mov    $D_1c2e,%ax
	push   %ax
	mov    $D_1c29,%ax
	push   %ax
	mov    $D_1c24,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
L_2630:
	jmp   L_2680
L_2632:
	mov    $D_1c33,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	movw   $0x0,-0xc(%bp)
L_263f:
	mov    -0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_2655
	mov    $D_1c3b,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	addw   $0x2,-0xc(%bp)
	jmp   L_263f
L_2655:
	mov    $D_1c40,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	movw   $0x0,-0xc(%bp)
L_2662:
	mov    -0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_2678
	mov    $D_1c45,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	addw   $0x2,-0xc(%bp)
	jmp   L_2662
L_2678:
	mov    $D_1c4a,%ax
	push   %ax
	call   L_39ae
	pop    %cx
L_2680:
	jmp   L_26c5
L_2682:
	mov    $D_1c5a,%ax
	push   %ax
	mov    $D_1c55,%ax
	push   %ax
	mov    $D_1c50,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
	jmp   L_26dc
L_2699:
	mov    $D_1c69,%ax
	push   %ax
	mov    $D_1c64,%ax
	push   %ax
	mov    $D_1c5f,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
	jmp   L_26dc
L_26b0:
	mov    $D_1c72,%ax
	push   %ax
	mov    $D_1c70,%ax
	push   %ax
	mov    $D_1c6e,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
L_26c5:
	jmp   L_2714
L_26c7:
	lea    0x4(%bp),%ax
	push   %ax
	lea    0x4(%bp),%ax
	push   %ax
	lea    0x4(%bp),%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
L_26dc:
	jmp   L_2714
L_26de:
	cmp    $0x63,%ax
	jne   L_26e6
	jmp   L_260b
L_26e6:
	jg   L_26f8
	or     %ax,%ax
	je   L_2714
	cmp    $0x28,%ax
	je   L_2682
	cmp    $0x5b,%ax
	je   L_2699
	jmp   L_26c7
L_26f8:
	cmp    $0x66,%ax
	jne   L_2700
	jmp   L_25e4
L_2700:
	cmp    $0x6e,%ax
	je   L_2714
	cmp    $0x7b,%ax
	jne   L_270d
	jmp   L_2632
L_270d:
	cmp    $0x7c,%ax
	je   L_26b0
	jmp   L_26c7
L_2714:
	pushw  0x6(%bp)
	mov    -0x10(%bp),%ax
	neg    %ax
	push   %ax
	mov    $D_1c74,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	cmpw   $0x0,0x8(%bp)
	jne   L_2730
	jmp   L_287a
L_2730:
	pushw  -0x10(%bp)
	mov    $D_1c81,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	mov    0x8(%bp),%ax
	jmp   L_283d
L_2743:
	cmpw   $0x1,-0x6(%bp)
	jg   L_2753
	mov    $D_1c8b,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	jmp   L_278f
L_2753:
	mov    $D_1c9a,%ax
	push   %ax
	mov    $D_1c95,%ax
	push   %ax
	mov    $D_1c90,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
	jmp   L_278f
L_276a:
	cmpw   $0x1,-0x6(%bp)
	jg   L_277a
	mov    $D_1c9f,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	jmp   L_278f
L_277a:
	mov    $D_1cae,%ax
	push   %ax
	mov    $D_1ca9,%ax
	push   %ax
	mov    $D_1ca4,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
L_278f:
	jmp   L_27df
L_2791:
	mov    $D_1cb3,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	movw   $0x0,-0xc(%bp)
L_279e:
	mov    -0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_27b4
	mov    $D_1cbb,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	addw   $0x2,-0xc(%bp)
	jmp   L_279e
L_27b4:
	mov    $D_1cc0,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	movw   $0x0,-0xc(%bp)
L_27c1:
	mov    -0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_27d7
	mov    $D_1cc5,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	addw   $0x2,-0xc(%bp)
	jmp   L_27c1
L_27d7:
	mov    $D_1cca,%ax
	push   %ax
	call   L_39ae
	pop    %cx
L_27df:
	jmp   L_2824
L_27e1:
	mov    $D_1cda,%ax
	push   %ax
	mov    $D_1cd5,%ax
	push   %ax
	mov    $D_1cd0,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
	jmp   L_283b
L_27f8:
	mov    $D_1ce9,%ax
	push   %ax
	mov    $D_1ce4,%ax
	push   %ax
	mov    $D_1cdf,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
	jmp   L_283b
L_280f:
	mov    $D_1cf2,%ax
	push   %ax
	mov    $D_1cf0,%ax
	push   %ax
	mov    $D_1cee,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
L_2824:
	jmp   L_286a
L_2826:
	lea    0x8(%bp),%ax
	push   %ax
	lea    0x8(%bp),%ax
	push   %ax
	lea    0x8(%bp),%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_28ba
	add    $0x8,%sp
L_283b:
	jmp   L_286a
L_283d:
	cmp    $0x63,%ax
	jne   L_2845
	jmp   L_276a
L_2845:
	jg   L_2853
	cmp    $0x29,%ax
	je   L_27f8
	cmp    $0x5d,%ax
	je   L_27e1
	jmp   L_2826
L_2853:
	cmp    $0x66,%ax
	jne   L_285b
	jmp   L_2743
L_285b:
	cmp    $0x7c,%ax
	je   L_280f
	cmp    $0x7d,%ax
	jne   L_2868
	jmp   L_2791
L_2868:
	jmp   L_2826
L_286a:
	mov    -0x10(%bp),%ax
	neg    %ax
	push   %ax
	mov    $D_1cf4,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
L_287a:
	mov    $D_1cfc,%ax
	push   %ax
	call   L_39ae
	pop    %cx
	cmpw   $0x0,D_2418
	je   L_28b3
	pushw  0x8(%bp)
	pushw  0x4(%bp)
	pushw  -0x10(%bp)
	pushw  -0x6(%bp)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2350(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2288(%bx)
	mov    $D_1cfe,%ax
	push   %ax
	call   L_39ae
	add    $0xe,%sp
L_28b3:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_28ba:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_28c7:
	pushw  0x6(%bp)
	mov    $D_1d27,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	movw   $0x0,-0x6(%bp)
L_28d9:
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jge   L_28f3
	pushw  0x8(%bp)
	mov    $D_1d2d,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	incw   -0x6(%bp)
	jmp   L_28d9
L_28f3:
	pushw  0xa(%bp)
	mov    $D_1d30,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2906:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x12,%sp
L_2915:
	call   L_1487
	mov    %ax,D_241a
	movw   $0x14,-0xc(%bp)
	cmpw   $0x2d,0x4(%bp)
	jne   L_292b
	movw   $0x0,-0xc(%bp)
L_292b:
	mov    0x8(%bp),%ax
	sub    0x6(%bp),%ax
	mov    %ax,-0x12(%bp)
	inc    %ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0x14(%bp)
	mov    0x6(%bp),%ax
	add    -0x14(%bp),%ax
	dec    %ax
	mov    %ax,-0x16(%bp)
	movw   $0x0,-0xe(%bp)
	mov    0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
L_2953:
	mov    -0xa(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_2974
	mov    -0xa(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	add    %ax,-0xe(%bp)
	incw   -0xa(%bp)
	jmp   L_2953
L_2974:
	mov    -0x12(%bp),%ax
	dec    %ax
	imulw  -0xc(%bp)
	add    -0xe(%bp),%ax
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2288(%bx)
	movw   $0x0,-0x10(%bp)
	mov    0x8(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_2994:
	mov    -0xa(%bp),%ax
	cmp    -0x16(%bp),%ax
	jle   L_29b8
	mov    -0xa(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	add    -0xc(%bp),%ax
	add    %ax,-0x10(%bp)
	decw   -0xa(%bp)
	jmp   L_2994
L_29b8:
	mov    -0x12(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	or     %dx,%dx
	je   L_29db
	mov    -0x16(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_2350(%bx),%ax
	add    -0x10(%bp),%ax
	jmp   L_29e4
L_29db:
	mov    -0x10(%bp),%ax
	sub    $0x14,%ax
	sub    -0xc(%bp),%ax
L_29e4:
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2350(%bx)
	cmpw   $0x0,D_2418
	je   L_2a4a
	pushw  0x4(%bp)
	pushw  D_241a
	mov    $D_1d34,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	mov    0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
L_2a0c:
	mov    -0xa(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_2a2c
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	mov    $D_1d49,%ax
	push   %ax
	call   L_39ae
	add    $0x4,%sp
	incw   -0xa(%bp)
	jmp   L_2a0c
L_2a2c:
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2350(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2288(%bx)
	mov    $D_1d4e,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_2a4a:
	mov    0x6(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	pushw  D_2282
	mov    0x6(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	call   L_1515
	add    $0x6,%sp
	mov    0x6(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	pushw  D_241a
	mov    $D_1d5a,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	mov    0x6(%bp),%ax
	inc    %ax
	mov    %ax,-0xa(%bp)
L_2a84:
	mov    -0xa(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_2ad1
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	pushw  D_2282
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	call   L_1515
	add    $0x6,%sp
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	pushw  D_241a
	pushw  D_241a
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	mov    $D_1d68,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	incw   -0xa(%bp)
	jmp   L_2a84
L_2ad1:
	pushw  D_241a
	cmpw   $0x52,0x4(%bp)
	jne   L_2ae0
	mov    $0x1,%ax
	jmp   L_2ae2
L_2ae0:
	xor    %ax,%ax
L_2ae2:
	push   %ax
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2350(%bx)
	pushw  D_241a
	mov    $D_1d86,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	mov    0x8(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_2b02:
	mov    -0xa(%bp),%ax
	cmp    0x6(%bp),%ax
	jge   L_2b0d
	jmp   L_2c00
L_2b0d:
	mov    -0xa(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    %ax,-0x8(%bp)
	mov    -0xa(%bp),%bx
	shl    %bx
	mov    D_2558(%bx),%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_2350(%bx),%ax
	mov    %ax,-0x6(%bp)
	mov    0x4(%bp),%ax
	jmp   L_2be0
L_2b3b:
	mov    -0x8(%bp),%ax
	sub    -0x6(%bp),%ax
	add    -0xc(%bp),%ax
	push   %ax
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	mov    -0x6(%bp),%ax
	neg    %ax
	push   %ax
	mov    $D_1da5,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	jmp   L_2b95
L_2b69:
	mov    -0x8(%bp),%ax
	sub    -0x6(%bp),%ax
	add    -0xc(%bp),%ax
	push   %ax
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	mov    -0x6(%bp),%ax
	neg    %ax
	push   %ax
	mov    $D_1dc8,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
L_2b95:
	jmp   L_2bde
L_2b97:
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	pushw  D_241a
	mov    -0x6(%bp),%ax
	neg    %ax
	push   %ax
	mov    $D_1deb,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	mov    -0x8(%bp),%ax
	sub    -0x6(%bp),%ax
	add    -0xc(%bp),%ax
	push   %ax
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	pushw  D_241a
	mov    $D_1e0c,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
L_2bde:
	jmp   L_2bfa
L_2be0:
	cmp    $0x2d,%ax
	je   L_2b97
	cmp    $0x43,%ax
	je   L_2b97
	cmp    $0x4c,%ax
	jne   L_2bf2
	jmp   L_2b3b
L_2bf2:
	cmp    $0x52,%ax
	jne   L_2bfa
	jmp   L_2b69
L_2bfa:
	decw   -0xa(%bp)
	jmp   L_2b02
L_2c00:
	pushw  D_241a
	cmpw   $0x52,0x4(%bp)
	je   L_2c0f
	mov    $0x1,%ax
	jmp   L_2c11
L_2c0f:
	xor    %ax,%ax
L_2c11:
	push   %ax
	mov    D_241a,%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    D_241a,%bx
	shl    %bx
	sub    D_2350(%bx),%ax
	add    -0xc(%bp),%ax
	push   %ax
	mov    $D_1e2d,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
	mov    0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
L_2c3a:
	mov    -0xa(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_2c54
	mov    -0xa(%bp),%bx
	shl    %bx
	pushw  D_2558(%bx)
	call   L_14d1
	pop    %cx
	incw   -0xa(%bp)
	jmp   L_2c3a
L_2c54:
	mov    D_241a,%bx
	shl    %bx
	movw   $0x0,D_25f8(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x0,D_26c0(%bx)
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2c74:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
L_2c83:
	mov    0x4(%bp),%ax
	mov    %ax,D_241a
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    %ax,-0xa(%bp)
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2350(%bx),%ax
	mov    %ax,-0xc(%bp)
	mov    0x8(%bp),%bx
L_2ca4:
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    %ax,-0xe(%bp)
	mov    0x8(%bp),%bx
	shl    %bx
	mov    D_2350(%bx),%ax
	mov    %ax,-0x10(%bp)
	cmpw   $0x123,0x6(%bp)
	jne   L_2d29
	movw   $0x14,-0x8(%bp)
	mov    -0x8(%bp),%ax
	neg    %ax
	add    -0xe(%bp),%ax
	sub    -0x10(%bp),%ax
	mov    %ax,-0x6(%bp)
	mov    -0x8(%bp),%ax
	add    -0xc(%bp),%ax
	cmp    -0xe(%bp),%ax
	jle   L_2ce7
	mov    -0xc(%bp),%ax
	sub    -0x10(%bp),%ax
	mov    %ax,-0x6(%bp)
L_2ce7:
	mov    -0xe(%bp),%ax
	sub    -0xc(%bp),%ax
	sub    -0x8(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_1467
	add    $0x4,%sp
	add    -0xc(%bp),%ax
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2350(%bx)
	mov    -0xe(%bp),%ax
	sub    -0xc(%bp),%ax
	sub    -0x8(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_1467
	add    $0x4,%sp
	add    -0xa(%bp),%ax
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2288(%bx)
	jmp   L_2d85
L_2d29:
	movw   $0x14,-0x8(%bp)
	mov    D_241a,%bx
	shl    %bx
	mov    -0xc(%bp),%ax
	mov    %ax,D_2350(%bx)
	mov    $0xffec,%ax
	sub    -0x10(%bp),%ax
	mov    %ax,-0x6(%bp)
	mov    -0xe(%bp),%ax
	add    $0x14,%ax
	mov    -0xa(%bp),%dx
	sub    -0xc(%bp),%dx
	cmp    %dx,%ax
	jge   L_2d68
	mov    -0xa(%bp),%ax
	sub    -0xc(%bp),%ax
	neg    %ax
	add    -0xe(%bp),%ax
	sub    -0x10(%bp),%ax
	sub    -0x8(%bp),%ax
	mov    %ax,-0x6(%bp)
L_2d68:
	mov    -0xe(%bp),%ax
	sub    $0x14,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_1467
	add    $0x4,%sp
	add    -0xa(%bp),%ax
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2288(%bx)
L_2d85:
	cmpw   $0x0,D_2418
	je   L_2dbd
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2288(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2350(%bx)
	mov    -0x6(%bp),%ax
	neg    %ax
	push   %ax
	pushw  0x8(%bp)
	pushw  -0x6(%bp)
	pushw  0x4(%bp)
	pushw  D_241a
	mov    $D_1e44,%ax
	push   %ax
	call   L_39ae
	add    $0x10,%sp
L_2dbd:
	mov    -0x6(%bp),%ax
	neg    %ax
	push   %ax
	pushw  0x8(%bp)
	pushw  -0x6(%bp)
	pushw  D_241a
	mov    $D_1e7f,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	pushw  0x8(%bp)
	call   L_14d1
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2de5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_2df1:
	/* TODO: data ref: mov    0x2f38,%ax */
	mov    D_0f38,%ax
	sub    %ax,D_2282
	mov    0x4(%bp),%ax
	mov    %ax,D_241a
	cmpw   $0x0,D_2418
	je   L_2e17
	pushw  D_2282
	pushw  D_241a
	mov    $D_1e9b,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_2e17:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2e1b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x1c,%sp
L_2e2a:
	call   L_1487
	mov    %ax,-0x1c(%bp)
	mov    0x4(%bp),%ax
	mov    %ax,D_241a
	cmpw   $0x0,D_2418
	je   L_2e54
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	pushw  D_241a
	mov    $D_1eae,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
L_2e54:
	mov    D_2282,%ax
	/* TODO: data ref: add    0x2f38,%ax */
	add    D_0f38,%ax
	cmp    $0x6,%ax
	jl   L_2e69
	mov    D_2282,%ax
	/* TODO: data ref: add    0x2f38,%ax */
	add    D_0f38,%ax
	jmp   L_2e6c
L_2e69:
	mov    $0x6,%ax
L_2e6c:
	mov    %ax,-0x6(%bp)
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    %ax,-0x8(%bp)
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2350(%bx),%ax
	mov    %ax,-0xe(%bp)
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    %ax,-0xa(%bp)
	mov    0x6(%bp),%bx
	shl    %bx
	mov    D_2350(%bx),%ax
	mov    %ax,-0x10(%bp)
	mov    0x8(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    %ax,-0xc(%bp)
	mov    0x8(%bp),%bx
	shl    %bx
	mov    D_2350(%bx),%ax
	mov    %ax,-0x12(%bp)
	movw   $0x14,-0x16(%bp)
	mov    -0x16(%bp),%ax
	neg    %ax
	add    -0xa(%bp),%ax
	sub    -0x10(%bp),%ax
	mov    %ax,-0x14(%bp)
	mov    -0x16(%bp),%ax
	add    -0xe(%bp),%ax
	cmp    -0xa(%bp),%ax
	jle   L_2ede
	mov    -0xe(%bp),%ax
	sub    -0x10(%bp),%ax
	mov    %ax,-0x14(%bp)
L_2ede:
	mov    $0xffec,%ax
	sub    -0x12(%bp),%ax
	mov    %ax,-0x1a(%bp)
L_2ee7:
	movw   $0x14,-0x18(%bp)
L_2eec:
	mov    -0xc(%bp),%ax
	add    $0x14,%ax
	mov    -0x8(%bp),%dx
	sub    -0xe(%bp),%dx
	cmp    %dx,%ax
	jge   L_2f12
	mov    -0x8(%bp),%ax
	sub    -0xe(%bp),%ax
	neg    %ax
	mov    -0xc(%bp),%dx
	sub    -0x12(%bp),%dx
	add    %dx,%ax
	sub    -0x18(%bp),%ax
	mov    %ax,-0x1a(%bp)
L_2f12:
	mov    -0xa(%bp),%ax
	sub    -0xe(%bp),%ax
	sub    -0x16(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_1467
	add    $0x4,%sp
	mov    %ax,-0x20(%bp)
	mov    -0xc(%bp),%ax
	sub    $0x14,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_1467
	add    $0x4,%sp
	add    -0x8(%bp),%ax
	add    -0x20(%bp),%ax
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2288(%bx)
	mov    -0xa(%bp),%ax
	sub    -0xe(%bp),%ax
	sub    -0x16(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_1467
	add    $0x4,%sp
	add    -0xe(%bp),%ax
	mov    D_241a,%bx
	shl    %bx
	mov    %ax,D_2350(%bx)
	pushw  0x6(%bp)
	pushw  -0x6(%bp)
	pushw  0x6(%bp)
	call   L_1515
	add    $0x6,%sp
	pushw  0x8(%bp)
	pushw  -0x6(%bp)
	pushw  0x8(%bp)
	call   L_1515
	add    $0x6,%sp
	pushw  0x8(%bp)
	pushw  -0x1c(%bp)
	mov    $D_1ec8,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	pushw  0x6(%bp)
	pushw  -0x1c(%bp)
	pushw  -0x1c(%bp)
	pushw  0x6(%bp)
	mov    $D_1ed6,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	mov    -0x14(%bp),%ax
	neg    %ax
	add    -0x1a(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	pushw  0x6(%bp)
	pushw  -0x14(%bp)
	pushw  0x4(%bp)
	mov    $D_1ef4,%ax
	push   %ax
	call   L_39ae
	add    $0xc,%sp
	mov    -0x1a(%bp),%ax
	neg    %ax
	push   %ax
	pushw  -0x1c(%bp)
	pushw  0x8(%bp)
	pushw  0x8(%bp)
	mov    $D_1f1c,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	/* TODO: data ref: mov    0x2f38,%ax */
	mov    D_0f38,%ax
	add    %ax,D_2282
	pushw  0x6(%bp)
	call   L_14d1
	pop    %cx
	pushw  0x8(%bp)
	call   L_14d1
	pop    %cx
	pushw  -0x1c(%bp)
	call   L_14d1
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3008:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_3014:
	mov    0x4(%bp),%bx
	cmpb   $0x2b,(%bx)
	jne   L_302b
	mov    0x4(%bp),%ax
	inc    %ax
	push   %ax
	call   L_382e
	pop    %cx
	add    %ax,D_2282
	jmp   L_304c
L_302b:
	mov    0x4(%bp),%bx
	cmpb   $0x2d,(%bx)
	jne   L_3042
	mov    0x4(%bp),%ax
	inc    %ax
	push   %ax
	call   L_382e
	pop    %cx
	sub    %ax,D_2282
	jmp   L_304c
L_3042:
	pushw  0x4(%bp)
	call   L_382e
	pop    %cx
	mov    %ax,D_2282
L_304c:
	cmpw   $0x0,D_2418
	je   L_3064
	pushw  D_2282
	pushw  0x4(%bp)
	mov    $D_1f3c,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
L_3064:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3068:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
L_3077:
	mov    0x6(%bp),%ax
	mov    %ax,D_241a
	cmpw   $0x0,D_2418
	je   L_30b0
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2288(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2350(%bx)
	pushw  0x4(%bp)
	pushw  0x6(%bp)
	pushw  D_2282
	pushw  D_241a
	mov    $D_1f53,%ax
	push   %ax
	call   L_39ae
	add    $0xe,%sp
L_30b0:
	cmpw   $0x6,D_2282
	jl   L_30bc
	mov    D_2282,%ax
	jmp   L_30bf
L_30bc:
	mov    $0x6,%ax
L_30bf:
	mov    %ax,-0x6(%bp)
	cmpw   $0x6,0x4(%bp)
	jl   L_30cd
	mov    0x4(%bp),%ax
	jmp   L_30d0
L_30cd:
	mov    $0x6,%ax
L_30d0:
	mov    %ax,-0x8(%bp)
	pushw  -0x8(%bp)
	pushw  0x6(%bp)
	pushw  -0x6(%bp)
	pushw  D_241a
	mov    $D_1f7d,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	mov    0x4(%bp),%ax
	mov    %ax,D_2282
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_30f7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x14,%sp
L_3106:
	mov    $0x14,%ax
	push   %ax
	lea    -0x18(%bp),%ax
	push   %ax
	call   L_1a91
	add    $0x4,%sp
	cmpb   $0x2b,-0x18(%bp)
	jne   L_3128
	lea    -0x17(%bp),%ax
	push   %ax
	call   L_382e
	pop    %cx
	/* TODO: data ref: add    %ax,0x2f3a */
	add    %ax,D_0f3a
	jmp   L_3147
L_3128:
	cmpb   $0x2d,-0x18(%bp)
	jne   L_313c
	lea    -0x17(%bp),%ax
	push   %ax
	call   L_382e
	pop    %cx
	/* TODO: data ref: sub    %ax,0x2f3a */
	sub    %ax,D_0f3a
	jmp   L_3147
L_313c:
	lea    -0x18(%bp),%ax
	push   %ax
	call   L_382e
	pop    %cx
	/* TODO: data ref: mov    %ax,0x2f3a */
	mov    %ax,D_0f3a
L_3147:
	movw   $0x0,D_2422
	movw   $0x0,D_241a
	/* TODO: data ref: pushw  0x2f3a */
	pushw  D_0f3a
	call   L_14ec
	pop    %cx
	/* TODO: data ref: mov    0x2f3a,%ax */
	mov    D_0f3a,%ax
	mov    %ax,D_2282
	/* TODO: data ref: cmpw   $0xc,0x2f3a */
	cmpw   $0xc,D_0f3a
	jl   L_3176
	/* TODO: data ref: mov    0x2f3a,%ax */
	mov    D_0f3a,%ax
	cwtd
	mov    $0x4,%cx
	idiv   %cx
	/* TODO: data ref: mov    %ax,0x2f38 */
	mov    %ax,D_0f38
	jmp   L_3182
L_3176:
	/* TODO: data ref: mov    0x2f3a,%ax */
	mov    D_0f3a,%ax
	cwtd
	mov    $0x3,%cx
	idiv   %cx
	/* TODO: data ref: mov    %ax,0x2f38 */
	mov    %ax,D_0f38
L_3182:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_318a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_3196:
	mov    0x4(%bp),%ax
	mov    %ax,D_241a
	pushw  0x4(%bp)
	pushw  D_2282
	pushw  0x4(%bp)
	call   L_1515
	add    $0x6,%sp
	pushw  0x4(%bp)
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	neg    %ax
	push   %ax
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_2350(%bx)
	pushw  0x4(%bp)
	mov    $D_1f94,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
	pushw  0x4(%bp)
	pushw  0x4(%bp)
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_2288(%bx),%ax
	mov    0x4(%bp),%bx
	shl    %bx
	sub    D_2350(%bx),%ax
	push   %ax
	mov    $D_1fb6,%ax
	push   %ax
	call   L_39ae
	add    $0x8,%sp
	mov    0x4(%bp),%bx
	shl    %bx
	addw   $0x14,D_2288(%bx)
	cmpw   $0x0,D_2418
	je   L_3227
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_2288(%bx)
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_2350(%bx)
	pushw  0x4(%bp)
	pushw  0x4(%bp)
	mov    $D_1fcf,%ax
	push   %ax
	call   L_39ae
	add    $0xa,%sp
L_3227:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_322c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
L_323b:
	call   L_1487
	mov    %ax,D_241a
	mov    %ax,%bx
	shl    %bx
	movw   $0x0,D_2350(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x28,D_2288(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x31,D_25f8(%bx)
	mov    D_241a,%bx
	shl    %bx
	movw   $0x31,D_26c0(%bx)
	cmpw   $0x102,0x4(%bp)
	jne   L_327e
	mov    0x6(%bp),%ax
	mov    %ax,-0x8(%bp)
	jmp   L_32c8
L_327e:
	cmpw   $0x103,0x4(%bp)
	jne   L_328c
	movw   $D_20ac,-0x8(%bp)
	jmp   L_32c8
L_328c:
	cmpw   $0x104,0x4(%bp)
	jne   L_329a
	movw   $D_20af,-0x8(%bp)
	jmp   L_32c8
L_329a:
	cmpw   $0x105,0x4(%bp)
	jne   L_32a8
	movw   $D_20b2,-0x8(%bp)
	jmp   L_32c8
L_32a8:
	xor    %ax,%ax
	push   %ax
	pushw  0x6(%bp)
	mov    $D_3300,%ax
	push   %ax
	call   L_1db4
	add    $0x6,%sp
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	je   L_32cb
	mov    -0xa(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x8(%bp)
L_32c8:
	jmp   L_3358
L_32cb:
	movw   $0x0,D_33c8
	movw   $0x0,D_3576
	movw   $0x0,D_3578
	movw   $0x0,D_3574
L_32e3:
	mov    D_3578,%ax
	incw   D_3578
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_3330
	pushw  0x6(%bp)
	pushw  -0x6(%bp)
	call   L_33b3
	add    $0x4,%sp
	mov    %ax,D_33c8
	cmpw   $0x0,D_3576
	jne   L_3315
	mov    D_33c8,%ax
	mov    %ax,D_3576
L_3315:
	cmpw   $0x190,D_3574
	jle   L_32e3
	pushw  0x6(%bp)
	mov    $D_20b5,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_16a0
	add    $0x6,%sp
	jmp   L_32e3
L_3330:
	mov    D_3574,%bx
	movb   $0x0,D_33d0(%bx)
	movw   $D_33d0,-0x8(%bp)
	mov    D_241a,%bx
	shl    %bx
	mov    D_3576,%ax
	mov    %ax,D_26c0(%bx)
	mov    D_241a,%bx
	shl    %bx
	mov    D_33c8,%ax
	mov    %ax,D_25f8(%bx)
L_3358:
	cmpw   $0x0,D_2418
	je   L_339b
	mov    D_241a,%bx
	shl    %bx
	pushw  D_25f8(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_26c0(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2288(%bx)
	mov    D_241a,%bx
	shl    %bx
	pushw  D_2350(%bx)
	pushw  -0x8(%bp)
	pushw  D_241a
	pushw  0x4(%bp)
	mov    $D_20d7,%ax
	push   %ax
	call   L_39ae
	add    $0x10,%sp
L_339b:
	pushw  -0x8(%bp)
	pushw  D_241a
	mov    $D_2103,%ax
	push   %ax
	call   L_39ae
	add    $0x6,%sp
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_33b3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_33c0:
	movw   $0x31,-0x6(%bp)
	mov    0x4(%bp),%ax
	jmp   L_3705
	cmpw   $0x31,D_33c8
	jne   L_33d5
	call   L_371c
L_33d5:
	pushw  0x4(%bp)
	call   L_3746
	pop    %cx
	jmp   L_341e
	cmpw   $0x31,D_33c8
	jne   L_33ee
	pushw  0x4(%bp)
	call   L_3746
	pop    %cx
	jmp   L_33ff
L_33ee:
	mov    D_3574,%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3574
L_33ff:
	mov    D_33c8,%ax
	mov    %ax,-0x6(%bp)
	jmp   L_3452
	cmpw   $0x31,D_33c8
	jne   L_3411
	call   L_371c
L_3411:
	call   L_371c
	pushw  0x4(%bp)
	call   L_3746
	pop    %cx
	call   L_371c
L_341e:
	jmp   L_3452
	cmpw   $0x31,D_33c8
	jne   L_342a
	call   L_371c
L_342a:
	mov    $0x71,%ax
	push   %ax
	mov    $0x65,%ax
	push   %ax
	call   L_37b5
	add    $0x4,%sp
	jmp   L_3490
	cmpw   $0x31,D_33c8
	jne   L_3444
	call   L_371c
L_3444:
	mov    $0x6c,%ax
	push   %ax
	mov    $0x70,%ax
	push   %ax
	call   L_37b5
	add    $0x4,%sp
L_3452:
	jmp   L_3490
	cmpw   $0x31,D_33c8
	jne   L_345e
	call   L_371c
L_345e:
	mov    0x6(%bp),%ax
	add    D_3578,%ax
	mov    %ax,%bx
	cmpb   $0x3d,(%bx)
	jne   L_347f
	mov    $0x3d,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_37b5
	add    $0x4,%sp
	incw   D_3578
	jmp   L_34cc
L_347f:
	mov    D_3574,%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3574
L_3490:
	jmp   L_34cc
	cmpw   $0x31,D_33c8
	jne   L_349c
	call   L_371c
L_349c:
	mov    0x6(%bp),%ax
	add    D_3578,%ax
	mov    %ax,%bx
	cmpb   $0x3e,(%bx)
	jne   L_34be
	mov    $0x3e,%ax
	push   %ax
	mov    $0x2d,%ax
	push   %ax
	call   L_37b5
	add    $0x4,%sp
	incw   D_3578
	jmp   L_34f3
L_34be:
	mov    $0x69,%ax
	push   %ax
	mov    $0x6d,%ax
	push   %ax
	call   L_37b5
	add    $0x4,%sp
L_34cc:
	jmp   L_34f3
	cmpw   $0x31,D_33c8
	jne   L_34d8
	call   L_371c
L_34d8:
	mov    $0x6c,%ax
	push   %ax
	mov    $0x73,%ax
	push   %ax
	call   L_37b5
	add    $0x4,%sp
	jmp   L_34f3
	call   L_371c
	call   L_371c
	jmp   L_34f3
	call   L_371c
L_34f3:
	jmp   L_35bc
	cmpw   $0x31,D_33c8
	jne   L_3500
	call   L_371c
L_3500:
	mov    D_3574,%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3574
	mov    D_3578,%ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,0x4(%bp)
	mov    D_3574,%bx
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3578
	incw   D_3574
	mov    D_3578,%ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    D_3574,%bx
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3578
	incw   D_3574
	cmpw   $0x28,0x4(%bp)
	jne   L_3571
	mov    D_3578,%ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    D_3574,%bx
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3578
	incw   D_3574
L_3571:
	cmpw   $0x2a,0x4(%bp)
	jne   L_35bc
	mov    D_3574,%bx
	cmpb   $0x28,D_33cf(%bx)
	jne   L_35bc
	mov    D_3578,%ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    D_3574,%bx
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3578
	incw   D_3574
	mov    D_3578,%ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    D_3574,%bx
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3578
	incw   D_3574
L_35bc:
	jmp   L_363c
	mov    D_3574,%bx
	movb   $0x5c,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x66,D_33d0(%bx)
	incw   D_3574
	cmpw   $0x31,D_33c8
	jne   L_35e4
	mov    $0x31,%ax
	jmp   L_35e7
L_35e4:
	mov    $0x31,%ax
L_35e7:
	mov    D_3574,%bx
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3574
	mov    $0x6d,%ax
	push   %ax
	mov    $0x66,%ax
	push   %ax
	call   L_37b5
	add    $0x4,%sp
	mov    D_3574,%bx
	movb   $0x5c,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x66,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x50,D_33d0(%bx)
	incw   D_3574
	cmpw   $0x31,D_33c8
	jne   L_3636
	mov    $0x31,%ax
	jmp   L_3639
L_3636:
	mov    $0x31,%ax
L_3639:
	mov    %ax,-0x6(%bp)
L_363c:
	jmp   L_369a
	cmpw   $0x31,D_2788
	jne   L_368d
	mov    D_3574,%bx
	movb   $0x5c,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x5e,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x66,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x5c,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x7c,D_33d0(%bx)
	incw   D_3574
	movw   $0x31,-0x6(%bp)
	jmp   L_36de
L_368d:
	mov    D_3574,%bx
	movb   $0x66,D_33d0(%bx)
	incw   D_3574
L_369a:
	jmp   L_3703
	cmpw   $0x31,D_2788
	jne   L_36d1
	mov    D_3574,%bx
	movb   $0x5c,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x5e,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x6a,D_33d0(%bx)
	incw   D_3574
	movw   $0x31,-0x6(%bp)
	jmp   L_3703
L_36d1:
	mov    D_3574,%bx
	movb   $0x6a,D_33d0(%bx)
	incw   D_3574
L_36de:
	jmp   L_3703
L_36e0:
	mov    D_3574,%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3574
	cmpw   $0x31,D_2788
	jne   L_36fd
	mov    $0x31,%ax
	jmp   L_3700
L_36fd:
	mov    $0x31,%ax
L_3700:
	mov    %ax,-0x6(%bp)
L_3703:
	jmp   L_3714
L_3705:
	sub    $0x20,%ax
	cmp    $0x5e,%ax
	ja   L_36e0
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x3fee(%bx)
L_3714:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_371c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_3728:
	mov    D_3574,%bx
	movb   $0x5c,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x7c,D_33d0(%bx)
	incw   D_3574
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3746:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_3752:
	mov    D_3574,%bx
	movb   $0x5c,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x66,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x31,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x5c,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x66,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x50,D_33d0(%bx)
	incw   D_3574
	pop    %di
	pop    %si
	pop    %bp
	ret


L_37b5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_37c1:
	mov    D_3574,%bx
	movb   $0x5c,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	movb   $0x28,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3574
	mov    D_3574,%bx
	mov    0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_33d0(%bx)
	incw   D_3574
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3802:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_3ac0
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_3828
	mov    $0xffff,%ax
	jmp   L_382a
L_3828:
	xor    %ax,%ax
L_382a:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_382e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	xor    %si,%si
	mov    %si,-0x6(%bp)
L_383d:
	mov    (%di),%al
	cbtw
	jmp   L_3848
L_3842:
	incw   -0x6(%bp)
L_3845:
	inc    %di
	jmp   L_3861
L_3848:
	cmp    $0x9,%ax
	je   L_385e
	cmp    $0x20,%ax
	je   L_385e
	cmp    $0x2b,%ax
	je   L_3845
	cmp    $0x2d,%ax
	je   L_3842
	jmp   L_3861
L_385e:
	inc    %di
	jmp   L_383d
L_3861:
	cmpb   $0x30,(%di)
	jl   L_3887
	cmpb   $0x39,(%di)
	jg   L_3887
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
	jmp   L_3861
L_3887:
	cmpw   $0x0,-0x6(%bp)
	je   L_3893
	mov    %si,%ax
	neg    %ax
	jmp   L_3895
L_3893:
	mov    %si,%ax
L_3895:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_389a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	cmpb   $0x0,D_2110
	jne   L_38d7
	movw   $D_21ee,-0x8(%bp)
	/* suppressed: movw $D_2276,D_2276 (self-ref in .data) */
	movw   $0x88,-0xa(%bp)
L_38bd:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_38d2
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_38bd
L_38d2:
	movb   $0x1,D_2110
L_38d7:
	movw   $D_21d6,-0x6(%bp)
L_38dc:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_38fb
	mov    -0x6(%bp),%ax
	cmp    D_2276,%ax
	jb   L_38f5
	xor    %ax,%ax
	jmp   L_3966
L_38f5:
	addw   $0x8,-0x6(%bp)
	jmp   L_38dc
L_38fb:
	cmpb   $0x77,(%di)
	jne   L_3911
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_3998
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_3950
L_3911:
	cmpb   $0x61,(%di)
	jne   L_3952
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c32
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_393f
	cmpw   $0x2,D_357a
	jne   L_393f
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_3998
	add    $0x4,%sp
	mov    %ax,%si
L_393f:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_3aa2
	add    $0x8,%sp
L_3950:
	jmp   L_3960
L_3952:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c32
	add    $0x4,%sp
	mov    %ax,%si
L_3960:
	or     %si,%si
	jge   L_3968
	xor    %ax,%ax
L_3966:
	jmp   L_3990
L_3968:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_3986
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_398d
L_3986:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_398d:
	mov    -0x6(%bp),%ax
L_3990:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3998:
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
	jns    L_39ac
	neg	%ax
	mov	%ax,D_357a	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_39ac:
	pop    %bp
	ret


L_39ae:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_21de,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_3ac0
	add    $0x6,%sp
	mov    D_21e4,%al
	cbtw
	test   $0x20,%ax
	je   L_39d2
	mov    $0xffff,%ax
	jmp   L_39d4
L_39d2:
	xor    %ax,%ax
L_39d4:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_39d8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_39ef
	mov    $0xffff,%ax
	jmp   L_39fb
L_39ef:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_39fd
	mov    $0xffff,%ax
L_39fb:
	jmp   L_3a75
L_39fd:
	cmpw   $0x0,0x4(%di)
	jne   L_3a31
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_3a18
	mov    0x7(%di),%al
	cbtw
	add    $D_2112,%ax
	mov    %ax,0x4(%di)
	jmp   L_39fd
L_3a18:
	mov    $0x200,%ax
	push   %ax
	call   L_495e
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_3a2d
	orb    $0x4,0x6(%di)
	jmp   L_39fd
L_3a2d:
	orb    $0x8,0x6(%di)
L_3a31:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_3a44
	mov    $0x1,%ax
	jmp   L_3a47
L_3a44:
	mov    $0x200,%ax
L_3a47:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4c48
	add    $0x6,%sp
L_3a55:
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_3a77
	cmpw   $0xffff,0x2(%di)
	jne   L_3a69
	orb    $0x10,0x6(%di)
	jmp   L_3a6d
L_3a69:
	orb    $0x20,0x6(%di)
L_3a6d:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_3a75:
	jmp   L_3a81
L_3a77:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_3a81:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3a86:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_3a91:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_3a9c
	inc    %si
	jmp   L_3a91
L_3a9c:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3aa2:
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
L_3abd:
	pop    %si
	pop    %bp
	ret


L_3ac0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3ad8:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_3aea
	jmp   L_4087
L_3aea:
	cmpb   $0x25,-0x5(%bp)
	je   L_3afd
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_3afb:
	jmp   L_3ad8
L_3afd:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_3b28
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_4698
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3b28:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_3b41
	incb   -0xa6(%bp)
	inc    %di
L_3b41:
	cmpb   $0x30,(%di)
	jne   L_3b4c
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_3b4c:
	cmpb   $0x2a,(%di)
	je   L_3b7c
	movw   $0x0,-0xe(%bp)
L_3b56:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_3b92
	cmpw   $0x9,-0xc(%bp)
	jg   L_3b92
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_3b56
L_3b7c:
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
L_3b92:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_3bd9
	cmpb   $0x2a,(%di)
	je   L_3bdb
	movw   $0x0,-0x10(%bp)
L_3bab:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_3bd5
	cmpw   $0x9,-0xc(%bp)
	jg   L_3bd5
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_3bab
L_3bd5:
	addw   $0x30,-0xc(%bp)
L_3bd9:
	jmp   L_3bf2
L_3bdb:
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
L_3bf2:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_3ffc
L_3bfd:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_3c0e
L_3c05:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_3c3e
	jmp   L_3c37
L_3c0e:
	cmp    $0x64,%ax
	je   L_3c40
	cmp    $0x6f,%ax
	je   L_3c24
	cmp    $0x75,%ax
	je   L_3c37
	cmp    $0x78,%ax
	je   L_3c32
	jmp   L_3c05
L_3c24:
	movw   $0x8,-0x12(%bp)
	jmp   L_3c37
L_3c2b:
	movw   $0x8,-0x12(%bp)
	jmp   L_3c3e
L_3c32:
	movw   $0x10,-0x12(%bp)
L_3c37:
	jmp   L_3c89
L_3c39:
	movw   $0x10,-0x12(%bp)
L_3c3e:
	jmp   L_3c99
L_3c40:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_3c5d
L_3c50:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_3c5d:
	cmpw   $0x0,-0x14(%bp)
	jg   L_3ca7
	jl   L_3c6b
	cmpw   $0x0,-0x16(%bp)
	jae   L_3ca7
L_3c6b:
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
	jmp   L_3ca7
L_3c89:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_3ca7
L_3c99:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_3ca7:
	cmpw   $0x0,-0x14(%bp)
	jne   L_3cb1
	cmpw   $0x0,-0x16(%bp)
L_3cb1:
	jne   L_3cc2
	cmpw   $0x0,-0x10(%bp)
	jne   L_3cc2
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_3cc2:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_3cc9:
	cmpw   $0x0,-0x14(%bp)
	jne   L_3cd3
	cmpw   $0x0,-0x16(%bp)
L_3cd3:
	je   L_3d1e
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_41e6
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_3cf4
	mov    $0x30,%ax
	jmp   L_3cf7
L_3cf4:
	mov    $0x57,%ax
L_3cf7:
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
	call   L_40de
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_3cc9
L_3d1e:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_3d6d
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_3d1e
L_3d3c:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_3d48:
	cmpw   $0x2,-0xc(%bp)
	jge   L_3d6d
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_3d68
	decw   -0x8(%bp)
L_3d68:
	incw   -0xc(%bp)
	jmp   L_3d48
L_3d6d:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_3d74:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_3d8b
	movw   $0x0,-0xc(%bp)
L_3d8b:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_3d9a
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_3d9a:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_4698
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3dbd:
	jmp   L_3afb
L_3dc0:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_3dd2
	movw   $D_2126,-0xa(%bp)
L_3dd2:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_3de1
	movw   $0x7fff,-0xc(%bp)
L_3de1:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3de7:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_3d74
	decw   -0xc(%bp)
	jl   L_3d74
	incw   -0x8(%bp)
	jmp   L_3de7
L_3df9:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_3e01:
	jmp   L_3dbd
L_3e03:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3e0f
	movw   $0x6,-0x10(%bp)
L_3e0f:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_43e4
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_3e44
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_3e44:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_3e59
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_3e71
L_3e59:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_3e59
L_3e71:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_3e84
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_3e84:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_3ea3
L_3e90:
	decw   -0xc(%bp)
	jl   L_3ea3
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_3e90
L_3ea3:
	cmpw   $0x0,-0xc(%bp)
	jg   L_3eac
	jmp   L_3d6d
L_3eac:
	decw   -0xc(%bp)
	jge   L_3eb4
	jmp   L_3d6d
L_3eb4:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_3eac
L_3ec9:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3ed7
	movw   $0x6,-0x10(%bp)
	jmp   L_3eda
L_3ed7:
	incw   -0x10(%bp)
L_3eda:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_43b8
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_3f0f
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_3f0f:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_3f1a
	incw   -0x1a(%bp)
L_3f1a:
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
L_3f3c:
	decw   -0xc(%bp)
	jle   L_3f56
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_3f3c
L_3f56:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_3f73
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_3f84
L_3f73:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_3f84:
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
	jmp   L_3d6d
L_3fb5:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3fc1
	movw   $0x6,-0x10(%bp)
L_3fc1:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_423c
	add    $0xc,%sp
	add    $0x8,%si
L_3fdf:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_3fdf
	decw   -0x8(%bp)
	jmp   L_3d6d
L_3ff0:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_3ff9:
	jmp   L_3e01
L_3ffc:
	cmp    $0x65,%ax
	jne   L_4004
	jmp   L_3ec9
L_4004:
	jg   L_4045
	cmp    $0x4f,%ax
	jne   L_400e
	jmp   L_3c24
L_400e:
	jg   L_4025
	cmp    $0x25,%ax
	je   L_3ff0
	cmp    $0x44,%ax
	jne   L_401d
	jmp   L_3c40
L_401d:
	cmp    $0x4c,%ax
L_4020:
	jne   L_4025
	jmp   L_3bfd
L_4025:
	cmp    $0x55,%ax
	jne   L_402d
	jmp   L_3c89
L_402d:
	cmp    $0x58,%ax
	jne   L_4035
	jmp   L_3c32
L_4035:
	cmp    $0x63,%ax
	jne   L_403d
	jmp   L_3d3c
L_403d:
	cmp    $0x64,%ax
	jne   L_4045
	jmp   L_3c50
L_4045:
	cmp    $0x6f,%ax
	jne   L_404d
	jmp   L_3c2b
L_404d:
	jg   L_4064
	cmp    $0x66,%ax
	jne   L_4057
	jmp   L_3e03
L_4057:
	cmp    $0x67,%ax
	jne   L_405f
	jmp   L_3fb5
L_405f:
	cmp    $0x6c,%ax
	je   L_4020
L_4064:
	cmp    $0x72,%ax
	jne   L_406c
	jmp   L_3df9
L_406c:
	cmp    $0x73,%ax
	jne   L_4074
	jmp   L_3dc0
L_4074:
	cmp    $0x75,%ax
	jne   L_407c
	jmp   L_3c99
L_407c:
	cmp    $0x78,%ax
	jne   L_4084
	jmp   L_3c39
L_4084:
	jmp   L_3ff9
L_4087:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_40ab
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_4698
	add    $0x8,%sp
L_40ab:
	lea    -0x4(%bp),%sp
	pop    %di
L_40af:
	pop    %si
	pop    %bp
	ret
L_40b2:
	call   L_421a


L_40b5:
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
	call   L_421a


L_40de:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_40fc
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_40fc:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_4105:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_4105
	push   %ax
	mov    0x2(%bx),%ax
L_4112:
	mov    0x4(%bx),%dx
L_4115:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_4115
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
L_4126:
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
L_412e:
	pushw  0x6(%bx)
	call   L_40b5
L_4134:
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_4156
	jne   L_414b
	or     $0x0,%ax
	je   L_4156
L_414b:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_4156:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_421a


L_415e:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_417a
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_417a:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_4191
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_4191:
	push   %dx
	push   %ax
	call   L_40de
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_41ab
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_41ab:
	ret
	call   L_421a
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_415e
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_40b5
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_421a


L_41e6:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_40de
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_40b5
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_421a:
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


L_423c:
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
	call   L_43b8
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_4276
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_4276:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_427d:
	cmpw   $0x0,-0xa(%bp)
	jle   L_4297
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_4297
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_427d
L_4297:
	cmpw   $0x0,-0x8(%bp)
	jl   L_42a8
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_42b4
L_42a8:
	cmpw   $0x0,-0x8(%bp)
	jge   L_42b2
	cmpw   $0xfffd,-0x8(%bp)
L_42b2:
	jge   L_4333
L_42b4:
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
L_42cf:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_42e9
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_42cf
L_42e9:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_4305
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_430b
L_4305:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_430b:
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
	jmp   L_43a3
L_4333:
	cmpw   $0x0,-0x8(%bp)
	jg   L_4355
	cmpb   $0x30,(%di)
	je   L_4344
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_4344:
	cmpw   $0x0,-0x8(%bp)
	jge   L_4355
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_4344
L_4355:
	movw   $0x1,-0xa(%bp)
L_435a:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_4382
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
	jne   L_437d
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_437d:
	incw   -0xa(%bp)
	jmp   L_435a
L_4382:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_43a3
L_438a:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_439d
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_438a
L_439d:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_43a3:
	cmpb   $0x2e,-0x1(%si)
	jne   L_43aa
	dec    %si
L_43aa:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_43b8:
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
	call   L_440f
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_43e4:
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
	call   L_440f
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_440f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_4422
	movw   $0x0,0xc(%bp)
L_4422:
	cmpw   $0x4f,0xc(%bp)
L_4426:
	jl   L_442d
	movw   $0x4e,0xc(%bp)
L_442d:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_212e,%si
	fldl   0x4(%bp)
	fcompl D_217e
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_446a
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_446a:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4cf2
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $D_217e,-0x16(%bp)
	fldl   -0xc(%bp)
	fcompl D_2186
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_44b5
	jmp   L_4562
L_44b5:
	movw   $D_217e,-0x16(%bp)
L_44ba:
	fldl   -0xc(%bp)
	fcompl D_218e
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_44d6
	jmp   L_4560
L_44d6:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	fdivl  D_2196
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4cf2
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   D_219e
	fstl   -0x1e(%bp)
	fdivl  D_21a6
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  D_21ae
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
	jmp   L_44ba
L_4549:
	cmpw   $D_217e,-0x16(%bp)
	jae   L_45a3
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_4560:
	jmp   L_4549
L_4562:
	fldl   0x4(%bp)
	fcompl D_21b6
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_45b2
L_457b:
	fldl   0x4(%bp)
	fmull  D_21be
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl D_21c6
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_45a3:
	jae   L_45b2
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_457b
L_45b2:
	mov    0xc(%bp),%ax
	add    $D_212e,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_45c4
	add    %di,-0x16(%bp)
L_45c4:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_212e,-0x16(%bp)
	jae   L_45da
	movb   $0x0,D_212e
	mov    $D_212e,%ax
	jmp   L_4645
L_45da:
	cmp    -0x16(%bp),%si
	ja   L_4636
	cmp    $D_217e,%si
	jae   L_4636
	fldl   0x4(%bp)
	fmull  D_21ce
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4cf2
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
	jmp   L_45da
L_4636:
	cmpw   $D_217e,-0x16(%bp)
	jb   L_4647
	movb   $0x0,D_217d
	mov    $D_212e,%ax
L_4645:
	jmp   L_4691
L_4647:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_464f:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_468b
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_212e,-0x16(%bp)
	jbe   L_466e
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_464f
L_466e:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_464f
	cmp    $D_212e,%si
	jbe   L_4688
	movb   $0x30,(%si)
L_4688:
	inc    %si
	jmp   L_464f
L_468b:
	movb   $0x0,(%si)
	mov    $D_212e,%ax
L_4691:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4698:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_46a5:
	cmpw   $0x0,0x8(%bp)
	jl   L_46ae
	jmp   L_473c
L_46ae:
	cmpb   $0x2d,(%di)
	jne   L_46eb
	cmpw   $0x30,0xc(%bp)
	jne   L_46eb
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_46da
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
	jmp   L_46ea
L_46da:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_478a
	add    $0x4,%sp
L_46ea:
	dec    %si
L_46eb:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_4709
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
	jmp   L_4715
L_4709:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_478a
	add    $0x4,%sp
L_4715:
	incw   0x8(%bp)
	jmp   L_46a5
L_471a:
	dec    %si
	jl   L_4750
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_473e
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
L_473c:
	jmp   L_471a
L_473e:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_478a
	add    $0x4,%sp
	jmp   L_471a
L_4750:
	cmpw   $0x0,0x8(%bp)
	je   L_4785
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_4774
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
	jmp   L_4780
L_4774:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_478a
	add    $0x4,%sp
L_4780:
	decw   0x8(%bp)
	jmp   L_4750
L_4785:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_478a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_47a5
	mov    $0xffff,%ax
	jmp   L_486c
L_47a5:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_47d8
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
	call   L_4cd8
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_485d
L_47d8:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_482a
	cmp    $D_21de,%di
	jne   L_4803
	mov    D_21e5,%al
	cbtw
	push   %ax
	call   L_4c0a
	pop    %cx
	or     %ax,%ax
	je   L_47f8
	orb    $0x4,0x6(%di)
	jmp   L_47a5
L_47f8:
	movw   $D_3580,0x4(%di)
	movw   $D_3580,(%di)
	jmp   L_47a5
L_4803:
	mov    $0x200,%ax
	push   %ax
	call   L_495e
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_481a
	orb    $0x4,0x6(%di)
	jmp   L_47a5
L_481a:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_484c
L_482a:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_484c
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4cd8
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_484c:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_485d:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_486e
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_486c:
	jmp   L_4871
L_486e:
	mov    0x4(%bp),%ax
L_4871:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4878:
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
	jne   L_48c4
	mov    0x4(%di),%si
	or     %si,%si
	je   L_48c4
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_48c4
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4cd8
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_48c4
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_48c6
L_48c4:
	xor    %ax,%ax
L_48c6:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_48cb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_21d6,%di
L_48d3:
	cmp    D_2276,%di
	jae   L_48e3
	push   %di
	call   L_48e7
	pop    %cx
	add    $0x8,%di
	jmp   L_48d3
L_48e3:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_48e7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_493a
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_493a
	push   %di
	call   L_4878
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4bf6
	pop    %cx
	or     %ax,%ax
	jge   L_491c
	mov    $0xffff,%si
L_491c:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_492c
	pushw  0x4(%di)
	call   L_4aaa
	pop    %cx
L_492c:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_493a
	movw   $0x0,0x4(%di)
L_493a:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_495e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_2278
	jne   L_4989
	mov    $D_227a,%ax
	or     $0x1,%ax
	mov    %ax,D_2278
	mov    $D_2278,%ax
	or     $0x1,%ax
	mov    %ax,D_227a
	/* dual-reloc split: movw $D_227a,D_3982 */
	mov	$D_227a,%ax
	mov	%ax,D_3982
	/* dual-reloc split: movw $D_2278,D_3980 */
	mov	$D_2278,%ax
	mov	%ax,D_3980
L_4989:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_3980,%di
L_499d:
	movw   $0x0,D_227c
L_49a3:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_49d2
L_49aa:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_49b9
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_49aa
L_49b9:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_49d2
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_49d2
	jmp   L_4a7b
L_49d2:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	jbe   L_49e1
L_49df:
	jmp   L_49a3
L_49e1:
	cmp    D_3982,%si
	jne   L_49ed
	cmp    $D_2278,%di
	je   L_49f1
L_49ed:
	xor    %ax,%ax
	jmp   L_4a29
L_49f1:
	incw   D_227c
	cmpw   $0x1,D_227c
	jle   L_49df
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_227c
	xor    %ax,%ax
	push   %ax
	call   L_4b8e
	pop    %cx
	mov    %ax,%si
	mov    D_227c,%ax
	shl    %ax
	add    %si,%ax
	add    $0x0,%ax
	cmp    %si,%ax
	jae   L_4a2b
	xor    %ax,%ax
L_4a29:
	jmp   L_4a3e
L_4a2b:
	mov    D_227c,%ax
	shl    %ax
	push   %ax
	call   L_4b8e
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_4a40
	xor    %ax,%ax
L_4a3e:
	jmp   L_4aa5
L_4a40:
	mov    D_3982,%bx
	mov    %si,(%bx)
	mov    D_3982,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_4a5e
	mov    D_3982,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_3982,%bx
	mov    %ax,(%bx)
L_4a5e:
	mov    D_227c,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_3982
	mov    $D_2278,%ax
	or     $0x1,%ax
	mov    D_3982,%bx
	mov    %ax,(%bx)
	jmp   L_499d
L_4a7b:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_3980
	cmp    %ax,%si
	jbe   L_4a9a
	mov    D_3980,%bx
	mov    (%bx),%ax
	mov    %ax,D_3984
	mov    D_3980,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_4a9a:
	mov    D_3980,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_4aa5:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4aaa:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_3980
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
	je   L_4adf
	push   %di
	call   L_4aaa
	pop    %cx
L_4adf:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_495e
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_4afe
	cmp    %di,%si
	jne   L_4b02
L_4afe:
	mov    %si,%ax
	jmp   L_4b6a
L_4b02:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_4b21
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_4b21:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_4b3f
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_4b21
L_4b3f:
	cmp    %di,%si
	jae   L_4b68
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_4b68
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
	mov    D_3984,%ax
	mov    %ax,(%bx)
L_4b68:
	mov    %si,%ax
L_4b6a:
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
	jns    L_4b85
	neg	%ax
	mov	%ax,D_357a	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_4b8c
L_4b85:
	mov    0x4(%bp),%bx
	mov    %bx,D_227e
L_4b8c:
	pop    %bp
	ret


L_4b8e:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_227e,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_4ba5
	neg	%ax
	mov	%ax,D_357a	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_4bb2
L_4ba5:
	mov    D_227e,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_227e
L_4bb2:
	pop    %bp
	ret


L_4bb4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_4bc1:
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_4be0
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_4bc1
	xor    %ax,%ax
	jmp   L_4bf1
L_4be0:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_4bf1:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4bf6:
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
	jns    L_4c07
	neg	%ax
	mov	%ax,D_357a	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4c07:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4c0a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4ca5
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_4c27
	xor    %ax,%ax
	jmp   L_4c2a
L_4c27:
	mov    $0x1,%ax
L_4c2a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4c32:
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
	jns    L_4c46
	neg	%ax
	mov	%ax,D_357a	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4c46:
	pop    %bp
	ret


L_4c48:
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
	jns    L_4c5f
	neg	%ax
	mov	%ax,D_357a	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4c5f:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4c62:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x6(%bp)
L_4c73:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_4c73
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
	call   L_4cbe
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4ca5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4cbe
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4cbe:
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
	jns    L_4cd5
	neg	%ax
	mov	%ax,D_357a	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4cd5:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4cd8:
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
	jns    L_4cef
	neg	%ax
	mov	%ax,D_357a	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4cef:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4cf2:
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


L_4d0e:
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
	jns    L_4d1f
	neg	%ax
	mov	%ax,D_357a	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4d1f:
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
	mov	%ax,D_357a	/* errno */
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
	.byte	0xbf, 0x0f, 0xbd, 0x00, 0xcc, 0x00, 0x9f, 0x00	/* |........| */
	.byte	0xae, 0x00, 0xdb, 0x00, 0xed, 0x00, 0x90, 0x00	/* |........| */
	.byte	0x90, 0x00	/* |..| */
D_0014:
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x31, 0x30, 0x20, 0x25	/* |.nr 10 %| */
	.byte	0x64, 0x75, 0x0a, 0x00	/* |du..| */
D_0020:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x76, 0x27, 0x2d, 0x31, 0x27, 0x5f, 0x5c, 0x76	/* |v'-1'_\v| */
	.byte	0x27, 0x31, 0x27, 0x0a, 0x00	/* |'1'..| */
D_0035:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5e	/* |.ds %d ^| */
	.byte	0x0a, 0x00	/* |..| */
D_003f:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x7e	/* |.ds %d ~| */
	.byte	0x0a, 0x00	/* |..| */
D_0049:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x76, 0x27, 0x2d, 0x31, 0x27, 0x2e, 0x5c, 0x76	/* |v'-1'.\v| */
	.byte	0x27, 0x31, 0x27, 0x0a, 0x00	/* |'1'..| */
D_005e:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x76, 0x27, 0x2d, 0x31, 0x27, 0x2e, 0x2e, 0x5c	/* |v'-1'..\| */
	.byte	0x76, 0x27, 0x31, 0x27, 0x0a, 0x00	/* |v'1'..| */
D_0074:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x76, 0x27, 0x2d, 0x31, 0x27, 0x5c, 0x6c, 0x27	/* |v'-1'\l'| */
	.byte	0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\n(%du'\| */
	.byte	0x76, 0x27, 0x31, 0x27, 0x0a, 0x00	/* |v'1'..| */
D_0092:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x6c, 0x27, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75	/* |l'\n(%du| */
	.byte	0x27, 0x0a, 0x00	/* |'..| */
D_00a5:
	.ascii	".as %d \h'-\n(%du-\n(%du/2u'\v'0-\n(10u'\*(%d\0"
D_00d3:
	.byte	0x5c, 0x76, 0x27, 0x5c, 0x6e, 0x28, 0x31, 0x30	/* |\v'\n(10| */
	.byte	0x75, 0x27, 0x5c, 0x68, 0x27, 0x2d, 0x5c, 0x6e	/* |u'\h'-\n| */
	.byte	0x28, 0x25, 0x64, 0x75, 0x2b, 0x5c, 0x6e, 0x28	/* |(%du+\n(| */
	.byte	0x25, 0x64, 0x75, 0x2f, 0x32, 0x75, 0x27, 0x0a	/* |%du/2u'.| */
	.byte	0x00	/* |.| */
D_00f4:
	.byte	0x2e, 0x09, 0x64, 0x69, 0x61, 0x63, 0x72, 0x69	/* |..diacri| */
	.byte	0x74, 0x3a, 0x20, 0x25, 0x63, 0x20, 0x6f, 0x76	/* |t: %c ov| */
	.byte	0x65, 0x72, 0x20, 0x53, 0x25, 0x64, 0x2c, 0x20	/* |er S%d, | */
	.byte	0x68, 0x3d, 0x25, 0x64, 0x2c, 0x20, 0x62, 0x3d	/* |h=%d, b=| */
	.byte	0x25, 0x64, 0x0a, 0x00	/* |%d..| */
D_0118:
	.byte	0xff, 0xff, 0x01, 0x00, 0x00, 0x00, 0xff, 0xff	/* |........| */
	.byte	0xfe, 0xff, 0x00, 0x00	/* |....| */
D_0124:
	.byte	0x22, 0x00, 0x3c, 0x00, 0x3d, 0x00, 0x34, 0x00	/* |".<.=.4.| */
	.byte	0x98, 0x00, 0x3b, 0x00, 0x3a, 0x00, 0x3b, 0x00	/* |..;.:.;.| */
	.byte	0x3a, 0x00, 0x97, 0x00, 0x96, 0x00, 0x95, 0x00	/* |:.......| */
	.byte	0x84, 0x00, 0x3b, 0x00, 0x3a, 0x00, 0x83, 0x00	/* |..;.:...| */
	.byte	0x58, 0x00, 0x59, 0x00, 0x8d, 0x00, 0x43, 0x00	/* |X.Y...C.| */
	.byte	0x45, 0x00, 0x3e, 0x00, 0x44, 0x00, 0x41, 0x00	/* |E.>.D.A.| */
	.byte	0x42, 0x00	/* |B.| */
	.word	D_003f	/* ptr: 0x003f */
	.byte	0x40, 0x00, 0x43, 0x00, 0x45, 0x00, 0x3e, 0x00	/* |@.C.E.>.| */
	.byte	0x44, 0x00, 0x41, 0x00, 0x42, 0x00	/* |D.A.B.| */
	.word	D_003f	/* ptr: 0x003f */
	.byte	0x40, 0x00, 0x43, 0x00, 0x45, 0x00, 0x3e, 0x00	/* |@.C.E.>.| */
	.byte	0x44, 0x00, 0x41, 0x00, 0x42, 0x00	/* |D.A.B.| */
	.word	D_003f	/* ptr: 0x003f */
	.byte	0x40, 0x00, 0x77, 0x00, 0x78, 0x00, 0x79, 0x00	/* |@.w.x.y.| */
	.byte	0x7a, 0x00, 0x22, 0x00	/* |z.".| */
	.word	D_0035	/* ptr: 0x0035 */
	.byte	0x7b, 0x00, 0x82, 0x00, 0x80, 0x00, 0x8c, 0x00	/* |{.......| */
	.byte	0x8b, 0x00, 0x57, 0x00, 0x8a, 0x00, 0x87, 0x00	/* |..W.....| */
	.byte	0x56, 0x00, 0x4e, 0x00, 0x4d, 0x00, 0x69, 0x00	/* |V.N.M.i.| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x4c, 0x00, 0x4b, 0x00, 0x4f, 0x00, 0x15, 0x00	/* |L.K.O...| */
	.word	D_0014	/* ptr: 0x0014 */
	.byte	0x89, 0x00, 0x13, 0x00, 0x12, 0x00, 0x39, 0x00	/* |......9.| */
	.byte	0x37, 0x00, 0x11, 0x00, 0x6f, 0x00, 0x10, 0x00	/* |7...o...| */
	.byte	0x5c, 0x00, 0x0f, 0x00, 0x51, 0x00, 0x0e, 0x00	/* |\...Q...| */
	.byte	0x0d, 0x00, 0x0c, 0x00, 0x0b, 0x00, 0x5a, 0x00	/* |......Z.| */
	.byte	0x08, 0x00, 0x07, 0x00, 0x7e, 0x00, 0x76, 0x00	/* |....~.v.| */
	.byte	0x75, 0x00, 0x5b, 0x00	/* |u.[.| */
	.word	D_0074	/* ptr: 0x0074 */
	.byte	0x73, 0x00, 0x71, 0x00, 0x7c, 0x00, 0x16, 0x00	/* |s.q.|...| */
	.byte	0x22, 0x00, 0x5d, 0x00, 0x32, 0x00, 0x72, 0x00	/* |".].2.r.| */
	.byte	0x01, 0x00	/* |..| */
	.word	D_005e	/* ptr: 0x005e */
	.byte	0x5f, 0x00, 0x60, 0x00, 0x61, 0x00, 0x00, 0x00	/* |_.`.a...| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0x00, 0x00	/* |..8.....| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x24, 0x00, 0x23, 0x00	/* |....$.#.| */
	.byte	0x25, 0x00, 0x26, 0x00, 0x27, 0x00, 0x30, 0x00	/* |%.&.'.0.| */
	.byte	0x22, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |".......| */
	.byte	0x06, 0x00, 0x33, 0x00, 0x28, 0x00	/* |..3.(.| */
	.word	D_0020	/* ptr: 0x0020 */
	.byte	0x29, 0x00, 0x2a, 0x00, 0x2b, 0x00, 0x1c, 0x00	/* |).*.+...| */
	.byte	0x1f, 0x00, 0x1d, 0x00, 0x1e, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x81, 0x00, 0x68, 0x00, 0x00, 0x00	/* |....h...| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x81, 0x00, 0x81, 0x00	/* |........| */
	.byte	0x81, 0x00, 0x81, 0x00, 0x0a, 0x00, 0x86, 0x00	/* |........| */
	.byte	0x81, 0x00, 0x17, 0x00, 0x1b, 0x00, 0x18, 0x00	/* |........| */
	.byte	0x19, 0x00, 0x1a, 0x00, 0x09, 0x00, 0x2d, 0x00	/* |......-.| */
	.byte	0x2f, 0x00, 0x2e, 0x00, 0x2c, 0x00, 0x21, 0x00	/* |/...,.!.| */
	.byte	0x70, 0x00, 0x24, 0x00, 0x23, 0x00, 0x25, 0x00	/* |p.$.#.%.| */
	.byte	0x26, 0x00, 0x27, 0x00, 0x30, 0x00, 0x22, 0x00	/* |&.'.0.".| */
	.byte	0x58, 0x00, 0x59, 0x00, 0x90, 0x00, 0x06, 0x00	/* |X.Y.....| */
	.byte	0x33, 0x00, 0x28, 0x00	/* |3.(.| */
	.word	D_0020	/* ptr: 0x0020 */
	.byte	0x29, 0x00, 0x2a, 0x00, 0x2b, 0x00, 0x1c, 0x00	/* |).*.+...| */
	.byte	0x1f, 0x00, 0x1d, 0x00, 0x1e, 0x00, 0x85, 0x00	/* |........| */
	.byte	0x81, 0x00, 0x81, 0x00, 0x77, 0x00, 0x78, 0x00	/* |....w.x.| */
	.byte	0x79, 0x00, 0x7a, 0x00, 0x58, 0x00, 0x59, 0x00	/* |y.z.X.Y.| */
	.byte	0x88, 0x00, 0x7f, 0x00, 0x0a, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x17, 0x00, 0x1b, 0x00, 0x18, 0x00	/* |........| */
	.byte	0x19, 0x00, 0x1a, 0x00, 0x09, 0x00, 0x2d, 0x00	/* |......-.| */
	.byte	0x2f, 0x00, 0x2e, 0x00, 0x2c, 0x00, 0x21, 0x00	/* |/...,.!.| */
	.byte	0x03, 0x00, 0x24, 0x00, 0x23, 0x00, 0x25, 0x00	/* |..$.#.%.| */
	.byte	0x26, 0x00, 0x27, 0x00, 0x30, 0x00, 0x22, 0x00	/* |&.'.0.".| */
	.byte	0x6a, 0x00, 0x6b, 0x00, 0x6c, 0x00, 0x06, 0x00	/* |j.k.l...| */
	.byte	0x05, 0x00, 0x28, 0x00	/* |..(.| */
	.word	D_0020	/* ptr: 0x0020 */
	.byte	0x29, 0x00, 0x2a, 0x00, 0x2b, 0x00, 0x1c, 0x00	/* |).*.+...| */
	.byte	0x1f, 0x00, 0x1d, 0x00, 0x1e, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x0a, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x17, 0x00, 0x1b, 0x00, 0x18, 0x00	/* |........| */
	.byte	0x19, 0x00, 0x1a, 0x00, 0x09, 0x00, 0x2d, 0x00	/* |......-.| */
	.byte	0x2f, 0x00, 0x2e, 0x00, 0x2c, 0x00, 0x21, 0x00	/* |/...,.!.| */
	.byte	0x24, 0x00, 0x23, 0x00, 0x25, 0x00, 0x26, 0x00	/* |$.#.%.&.| */
	.byte	0x27, 0x00, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00	/* |'.0.....| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x33, 0x00	/* |......3.| */
	.byte	0x28, 0x00	/* |(.| */
	.word	D_0020	/* ptr: 0x0020 */
	.byte	0x29, 0x00, 0x2a, 0x00, 0x2b, 0x00, 0x1c, 0x00	/* |).*.+...| */
	.byte	0x1f, 0x00, 0x1d, 0x00, 0x1e, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x91, 0x00	/* |....| */
	.word	D_0092	/* ptr: 0x0092 */
	.byte	0x93, 0x00, 0x94, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x17, 0x00, 0x1b, 0x00	/* |........| */
	.byte	0x18, 0x00, 0x19, 0x00, 0x1a, 0x00, 0x09, 0x00	/* |........| */
	.byte	0x2d, 0x00, 0x2f, 0x00, 0x2e, 0x00, 0x2c, 0x00	/* |-./...,.| */
	.byte	0x21, 0x00, 0x00, 0x00, 0x24, 0x00, 0x23, 0x00	/* |!...$.#.| */
	.byte	0x25, 0x00, 0x26, 0x00, 0x27, 0x00, 0x30, 0x00	/* |%.&.'.0.| */
	.byte	0x36, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |6.......| */
	.byte	0x06, 0x00, 0x05, 0x00, 0x28, 0x00	/* |....(.| */
	.word	D_0020	/* ptr: 0x0020 */
	.byte	0x29, 0x00, 0x2a, 0x00, 0x2b, 0x00, 0x1c, 0x00	/* |).*.+...| */
	.byte	0x1f, 0x00, 0x1d, 0x00, 0x1e, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |P.......| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x0a, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x17, 0x00, 0x1b, 0x00, 0x18, 0x00	/* |........| */
	.byte	0x19, 0x00, 0x1a, 0x00, 0x09, 0x00, 0x2d, 0x00	/* |......-.| */
	.byte	0x2f, 0x00, 0x2e, 0x00, 0x2c, 0x00, 0x21, 0x00	/* |/...,.!.| */
	.byte	0x00, 0x00, 0x24, 0x00, 0x23, 0x00, 0x25, 0x00	/* |..$.#.%.| */
	.byte	0x26, 0x00, 0x27, 0x00, 0x30, 0x00, 0x00, 0x00	/* |&.'.0...| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x28, 0x00	/* |..(.| */
	.word	D_0020	/* ptr: 0x0020 */
	.byte	0x29, 0x00, 0x2a, 0x00, 0x2b, 0x00, 0x1c, 0x00	/* |).*.+...| */
	.byte	0x1f, 0x00, 0x1d, 0x00, 0x1e, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x0a, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x17, 0x00, 0x1b, 0x00, 0x18, 0x00	/* |........| */
	.byte	0x19, 0x00, 0x1a, 0x00, 0x09, 0x00, 0x2d, 0x00	/* |......-.| */
	.byte	0x2f, 0x00, 0x2e, 0x00, 0x2c, 0x00, 0x21, 0x00	/* |/...,.!.| */
	.byte	0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x31, 0x00	/* |......1.| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46, 0x00	/* |......F.| */
	.byte	0x47, 0x00, 0x48, 0x00	/* |G.H.| */
	.word	D_0049	/* ptr: 0x0049 */
	.byte	0x4a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7d, 0x00	/* |J.....}.| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x52, 0x00, 0x53, 0x00, 0x54, 0x00, 0x55, 0x00	/* |R.S.T.U.| */
	.byte	0x7d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |}.......| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x62, 0x00, 0x63, 0x00, 0x64, 0x00	/* |..b.c.d.| */
	.byte	0x65, 0x00, 0x66, 0x00, 0x67, 0x00, 0x00, 0x00	/* |e.f.g...| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x6d, 0x00, 0x6e, 0x00, 0x31, 0x00	/* |..m.n.1.| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x31, 0x00	/* |......1.| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x31, 0x00	/* |......1.| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x8e, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x8f, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x31, 0x00	/* |....1.| */
D_0584:
	.byte	0xe3, 0xff, 0x18, 0xfc, 0x11, 0x00, 0x18, 0xfc	/* |........| */
	.byte	0xe3, 0xfe, 0x18, 0xfc, 0x6f, 0x00, 0x6f, 0x00	/* |....o.o.| */
	.byte	0x6f, 0x00, 0x6f, 0x00, 0x6f, 0x00, 0xc4, 0xff	/* |o.o.o...| */
	.byte	0xc3, 0xff, 0xc0, 0xff, 0xbf, 0xff, 0xe5, 0xfe	/* |........| */
	.byte	0x40, 0x00, 0x18, 0xfc, 0x6f, 0x00, 0x6f, 0x00	/* |@...o.o.| */
	.byte	0x6f, 0x00, 0x6f, 0x00, 0xbe, 0xff, 0x0f, 0xff	/* |o.o.....| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc, 0x0f, 0xff	/* |........| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc	/* |........| */
	.byte	0x18, 0xfc, 0xd0, 0xff, 0x40, 0x00, 0x18, 0xfc	/* |....@...| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc	/* |........| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc	/* |........| */
	.byte	0x0f, 0xff, 0x0f, 0xff, 0x0f, 0xff, 0x0f, 0xff	/* |........| */
	.byte	0x18, 0xfc, 0xe3, 0xfe, 0x18, 0xfc, 0x6f, 0x00	/* |......o.| */
	.byte	0x6f, 0x00, 0x6f, 0x00, 0x6f, 0x00, 0x6f, 0x00	/* |o.o.o.o.| */
	.byte	0x6f, 0x00, 0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc	/* |o.......| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc	/* |........| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc	/* |........| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0xe3, 0xfe, 0xf3, 0xfe	/* |........| */
	.byte	0xf3, 0xfe, 0xf3, 0xfe, 0xeb, 0xfe, 0x40, 0x00	/* |......@.| */
	.byte	0x40, 0x00, 0x40, 0x00, 0x40, 0x00, 0x6f, 0x00	/* |@.@.@.o.| */
	.byte	0x6f, 0x00, 0x85, 0xff, 0xf3, 0xfe, 0xf3, 0xfe	/* |o.......| */
	.byte	0xf3, 0xfe, 0xf3, 0xfe, 0x24, 0xff, 0x18, 0xfc	/* |....$...| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc	/* |........| */
	.byte	0x18, 0xfc, 0xb4, 0xff, 0x18, 0xfc, 0x18, 0xfc	/* |........| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0xe3, 0xfe, 0xeb, 0xfe	/* |........| */
	.byte	0xeb, 0xfe, 0xeb, 0xfe, 0xe3, 0xfe, 0xe3, 0xfe	/* |........| */
	.byte	0xb6, 0xff, 0x11, 0x00, 0xb5, 0xff, 0x92, 0xff	/* |........| */
	.byte	0x8f, 0xff, 0xeb, 0xfe, 0xeb, 0xfe, 0x18, 0xfc	/* |........| */
	.byte	0xbb, 0xff, 0xc6, 0xff, 0x18, 0xfc, 0xbc, 0xff	/* |........| */
	.byte	0xba, 0xff, 0xb9, 0xff, 0x97, 0xff, 0x18, 0xfc	/* |........| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc	/* |........| */
	.byte	0x18, 0xfc, 0x6f, 0x00, 0x18, 0xfc, 0x6f, 0x00	/* |..o...o.| */
	.byte	0x18, 0xfc, 0x40, 0x00, 0x18, 0xfc, 0x18, 0xfc	/* |..@.....| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc	/* |........| */
	.byte	0x18, 0xfc, 0x18, 0xfc, 0x40, 0x00, 0x40, 0x00	/* |....@.@.| */
	.byte	0x40, 0x00, 0x40, 0x00, 0xeb, 0xfe, 0xe3, 0xfe	/* |@.@.....| */
	.byte	0x11, 0x00, 0x8e, 0xff, 0x8d, 0xff, 0x8c, 0xff	/* |........| */
	.byte	0x87, 0xff, 0x18, 0xfc, 0x18, 0xfc, 0x18, 0xfc	/* |........| */
	.byte	0x18, 0xfc	/* |..| */
D_06b6:
	.byte	0x00, 0x00, 0x62, 0x00, 0x3c, 0x00, 0x9e, 0x01	/* |..b.<...| */
	.byte	0x60, 0x00, 0x5d, 0x00, 0x5b, 0x00, 0x61, 0x00	/* |`.].[.a.| */
	.byte	0x5a, 0x00, 0x9e, 0x00, 0x59, 0x00, 0x57, 0x00	/* |Z...Y.W.| */
	.byte	0x56, 0x00, 0x5c, 0x00, 0x47, 0x01, 0x55, 0x00	/* |V.\.G.U.| */
	.byte	0x6d, 0x00, 0x54, 0x00, 0x53, 0x00, 0x51, 0x00	/* |m.T.S.Q.| */
	.byte	0x50, 0x00, 0x4f, 0x00, 0x4e, 0x00, 0x30, 0x00	/* |P.O.N.0.| */
	.byte	0x4c, 0x00, 0x4a, 0x00	/* |L.J.| */
	.word	D_0049	/* ptr: 0x0049 */
	.byte	0x48, 0x00, 0x47, 0x00, 0x46, 0x00, 0x45, 0x00	/* |H.G.F.E.| */
	.byte	0x44, 0x00, 0x42, 0x00, 0x41, 0x00, 0x36, 0x00	/* |D.B.A.6.| */
D_06fc:
	.byte	0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00	/* |........| */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x04, 0x00, 0x05, 0x00, 0x06, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x07, 0x00, 0x07, 0x00, 0x07, 0x00, 0x07, 0x00	/* |........| */
	.byte	0x08, 0x00, 0x0a, 0x00, 0x0b, 0x00, 0x0c, 0x00	/* |........| */
	.byte	0x0d, 0x00, 0x0f, 0x00, 0x0f, 0x00, 0x03, 0x00	/* |........| */
	.byte	0x03, 0x00, 0x03, 0x00, 0x03, 0x00, 0x03, 0x00	/* |........| */
	.byte	0x03, 0x00, 0x03, 0x00, 0x03, 0x00, 0x03, 0x00	/* |........| */
	.byte	0x03, 0x00, 0x03, 0x00, 0x03, 0x00, 0x03, 0x00	/* |........| */
	.byte	0x03, 0x00, 0x03, 0x00, 0x03, 0x00, 0x03, 0x00	/* |........| */
	.byte	0x03, 0x00, 0x03, 0x00, 0x03, 0x00, 0x03, 0x00	/* |........| */
	.byte	0x03, 0x00, 0x03, 0x00, 0x03, 0x00, 0x03, 0x00	/* |........| */
	.byte	0x03, 0x00, 0x03, 0x00, 0x18, 0x00, 0x1e, 0x00	/* |........| */
	.byte	0x1f, 0x00	/* |..| */
	.word	D_0020	/* ptr: 0x0020 */
	.byte	0x21, 0x00, 0x1d, 0x00, 0x1d, 0x00, 0x1d, 0x00	/* |!.......| */
	.byte	0x1d, 0x00, 0x1d, 0x00, 0x1d, 0x00, 0x1d, 0x00	/* |........| */
	.byte	0x1d, 0x00, 0x1c, 0x00, 0x10, 0x00, 0x19, 0x00	/* |........| */
	.byte	0x19, 0x00, 0x1a, 0x00, 0x1a, 0x00, 0x1a, 0x00	/* |........| */
	.byte	0x09, 0x00, 0x09, 0x00, 0x13, 0x00	/* |......| */
	.word	D_0014	/* ptr: 0x0014 */
	.byte	0x16, 0x00, 0x15, 0x00, 0x11, 0x00, 0x12, 0x00	/* |........| */
	.byte	0x12, 0x00, 0x12, 0x00, 0x12, 0x00, 0x17, 0x00	/* |........| */
	.byte	0x0e, 0x00, 0x1b, 0x00, 0x1b, 0x00, 0x1b, 0x00	/* |........| */
	.byte	0x1b, 0x00, 0x1b, 0x00, 0x1b, 0x00, 0x1b, 0x00	/* |........| */
	.byte	0x1b, 0x00, 0x1b, 0x00, 0x1b, 0x00, 0x22, 0x00	/* |......".| */
	.byte	0x22, 0x00	/* |".| */
D_07ba:
	.byte	0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x01, 0x00	/* |..| */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x01, 0x00	/* |..| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x01, 0x00, 0x01, 0x00	/* |....| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x04, 0x00, 0x04, 0x00, 0x04, 0x00, 0x04, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00	/* |........| */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x00, 0x00, 0x03, 0x00	/* |....| */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x04, 0x00, 0x04, 0x00, 0x04, 0x00, 0x04, 0x00	/* |........| */
	.byte	0x04, 0x00, 0x03, 0x00, 0x03, 0x00, 0x04, 0x00	/* |........| */
	.byte	0x03, 0x00, 0x03, 0x00, 0x01, 0x00, 0x03, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x04, 0x00, 0x03, 0x00	/* |......| */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x04, 0x00, 0x01, 0x00	/* |....| */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x01, 0x00	/* |....| */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x00, 0x00, 0x01, 0x00, 0x03, 0x00, 0x01, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x01, 0x00, 0x01, 0x00	/* |......| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x01, 0x00, 0x01, 0x00, 0x01, 0x00	/* |......| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x01, 0x00, 0x01, 0x00, 0x03, 0x00, 0x01, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x01, 0x00	/* |....| */
D_0878:
	.byte	0x18, 0xfc, 0xff, 0xff, 0xfe, 0xff, 0x00, 0x01	/* |........| */
	.byte	0xfd, 0xff, 0x0c, 0x01, 0x0b, 0x01, 0xef, 0xff	/* |........| */
	.byte	0xee, 0xff, 0x29, 0x01, 0x21, 0x01, 0xed, 0xff	/* |..).!...| */
	.byte	0xec, 0xff, 0xeb, 0xff, 0xea, 0xff, 0xe8, 0xff	/* |........| */
	.byte	0xe7, 0xff, 0xe5, 0xff, 0xe2, 0xff, 0xe1, 0xff	/* |........| */
	.byte	0xe0, 0xff, 0xdf, 0xff, 0xfb, 0xff	/* |......| */
	.word	D_0124	/* ptr: 0x0124 */
	.byte	0x26, 0x01, 0x27, 0x01, 0x28, 0x01, 0x25, 0x01	/* |&.'.(.%.| */
	.byte	0x12, 0x01, 0x14, 0x01, 0x15, 0x01, 0x13, 0x01	/* |........| */
	.byte	0x0e, 0x01, 0x2e, 0x01, 0x7b, 0x00, 0x02, 0x01	/* |....{...| */
	.byte	0x01, 0x01, 0x03, 0x01, 0x04, 0x01, 0x05, 0x01	/* |........| */
	.byte	0x0d, 0x01, 0x0f, 0x01, 0x10, 0x01, 0x11, 0x01	/* |........| */
	.byte	0x2d, 0x01, 0x2a, 0x01, 0x2c, 0x01, 0x2b, 0x01	/* |-.*.,.+.| */
	.byte	0x06, 0x01, 0xfd, 0xff, 0xfc, 0xff, 0x0c, 0x01	/* |........| */
	.byte	0x20, 0x01, 0xe9, 0xff, 0xf2, 0xff, 0xe4, 0xff	/* | .......| */
	.byte	0xf0, 0xff, 0xe3, 0xff, 0x23, 0x01, 0x22, 0x01	/* |....#.".| */
	.byte	0x1e, 0x01, 0x1f, 0x01, 0x32, 0x01, 0x36, 0x01	/* |....2.6.| */
	.byte	0x37, 0x01, 0x34, 0x01, 0x35, 0x01, 0x30, 0x01	/* |7.4.5.0.| */
	.byte	0x33, 0x01, 0x31, 0x01, 0xfd, 0xff, 0xfd, 0xff	/* |3.1.....| */
	.byte	0xfd, 0xff, 0xfd, 0xff, 0xfd, 0xff, 0x7b, 0x00	/* |......{.| */
	.byte	0x7b, 0x00, 0x7b, 0x00, 0x7b, 0x00, 0xe9, 0xff	/* |{.{.{...| */
	.byte	0xf2, 0xff, 0xfe, 0xff, 0xfd, 0xff, 0xfd, 0xff	/* |........| */
	.byte	0xfd, 0xff, 0xfd, 0xff, 0x7b, 0x00, 0xde, 0xff	/* |....{...| */
	.byte	0x01, 0x01, 0x02, 0x01, 0xde, 0xff, 0xde, 0xff	/* |........| */
	.byte	0x7b, 0x00, 0xfe, 0xff, 0xde, 0xff, 0xde, 0xff	/* |{.......| */
	.byte	0xde, 0xff, 0xde, 0xff, 0xfd, 0xff, 0xfd, 0xff	/* |........| */
	.byte	0xfd, 0xff, 0xfd, 0xff, 0xfd, 0xff, 0xfd, 0xff	/* |........| */
	.byte	0xf7, 0xff, 0xfe, 0xff, 0xf7, 0xff, 0xf7, 0xff	/* |........| */
	.byte	0xf7, 0xff, 0xfd, 0xff, 0xfd, 0xff, 0xe6, 0xff	/* |........| */
	.byte	0x2f, 0x01, 0xfa, 0xff, 0xf9, 0xff, 0xf8, 0xff	/* |/.......| */
	.byte	0xf6, 0xff, 0xf5, 0xff, 0xf4, 0xff, 0x07, 0x01	/* |........| */
	.byte	0x08, 0x01, 0x09, 0x01, 0x0a, 0x01, 0x7d, 0x00	/* |......}.| */
	.byte	0xf3, 0xff, 0xf2, 0xff, 0xf1, 0xff, 0xf0, 0xff	/* |........| */
	.byte	0x7d, 0x00, 0x16, 0x01, 0x7d, 0x00, 0x7d, 0x00	/* |}...}.}.| */
	.byte	0x7d, 0x00, 0xf3, 0xff, 0xde, 0xff, 0x7d, 0x00	/* |}.....}.| */
	.byte	0xf9, 0xff, 0x7d, 0x00, 0x7b, 0x00, 0x7b, 0x00	/* |..}.{.{.| */
	.byte	0x7b, 0x00, 0x7b, 0x00, 0xfd, 0xff, 0xfd, 0xff	/* |{.{.....| */
	.byte	0xfe, 0xff, 0xf7, 0xff, 0xf7, 0xff, 0xf7, 0xff	/* |........| */
	.byte	0xf7, 0xff, 0x7d, 0x00, 0x7d, 0x00, 0x7d, 0x00	/* |..}.}.}.| */
	.byte	0x7d, 0x00	/* |}.| */
D_09aa:
	.byte	0x03, 0x00, 0xfe, 0xff, 0x01, 0x00	/* |......| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x04, 0x00, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x27, 0x00	/* |......'.| */
	.byte	0x00, 0x00, 0x29, 0x00, 0x00, 0x00, 0x00, 0x00	/* |..).....| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x4d, 0x00, 0x4e, 0x00, 0x4f, 0x00, 0x00, 0x00	/* |M.N.O...| */
	.byte	0x48, 0x00	/* |H.| */
	.word	D_0049	/* ptr: 0x0049 */
	.byte	0x4b, 0x00, 0x4a, 0x00, 0x32, 0x00, 0x00, 0x00	/* |K.J.2...| */
	.byte	0x00, 0x00, 0x54, 0x00, 0x55, 0x00, 0x56, 0x00	/* |..T.U.V.| */
	.byte	0x57, 0x00, 0x58, 0x00, 0x59, 0x00, 0x5a, 0x00	/* |W.X.Y.Z.| */
	.byte	0x5b, 0x00, 0x5c, 0x00, 0x00, 0x00, 0x00, 0x00	/* |[.\.....| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x09, 0x00, 0x05, 0x00	/* |........| */
	.byte	0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2c, 0x00	/* |......,.| */
	.byte	0x51, 0x00, 0x52, 0x00	/* |Q.R.| */
	.word	D_003f	/* ptr: 0x003f */
	.byte	0x40, 0x00, 0x37, 0x00, 0x38, 0x00, 0x39, 0x00	/* |@.7.8.9.| */
	.byte	0x3a, 0x00, 0x3b, 0x00, 0x3c, 0x00, 0x3d, 0x00	/* |:.;.<.=.| */
	.byte	0x3e, 0x00, 0x18, 0x00, 0x19, 0x00, 0x1a, 0x00	/* |>.......| */
	.byte	0x1b, 0x00, 0x1c, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x45, 0x00, 0x2d, 0x00, 0x2e, 0x00, 0x2f, 0x00	/* |E.-.../.| */
	.byte	0x30, 0x00, 0x00, 0x00, 0x4c, 0x00, 0x5d, 0x00	/* |0...L.].| */
	.word	D_005e	/* ptr: 0x005e */
	.byte	0x50, 0x00, 0x41, 0x00, 0x42, 0x00, 0x00, 0x00	/* |P.A.B...| */
	.byte	0x33, 0x00, 0x34, 0x00	/* |3.4.| */
	.word	D_0035	/* ptr: 0x0035 */
	.byte	0x36, 0x00, 0x08, 0x00, 0x17, 0x00, 0x22, 0x00	/* |6.....".| */
	.byte	0x23, 0x00, 0x16, 0x00, 0x2b, 0x00, 0x00, 0x00	/* |#...+...| */
	.byte	0x46, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |F.......| */
	.byte	0x25, 0x00, 0x26, 0x00, 0x28, 0x00, 0x00, 0x00	/* |%.&.(...| */
	.byte	0x00, 0x00, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x11, 0x00	/* |........| */
	.byte	0x12, 0x00, 0x13, 0x00, 0x53, 0x00, 0x21, 0x00	/* |....S.!.| */
	.byte	0x00, 0x00, 0x2a, 0x00, 0x00, 0x00, 0x1d, 0x00	/* |..*.....| */
	.byte	0x00, 0x00, 0x1e, 0x00, 0x1f, 0x00	/* |......| */
	.word	D_0020	/* ptr: 0x0020 */
	.byte	0x24, 0x00, 0x43, 0x00, 0x44, 0x00, 0x0b, 0x00	/* |$.C.D...| */
	.byte	0x31, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |1.......| */
	.byte	0x00, 0x00	/* |..| */
	.word	D_0014	/* ptr: 0x0014 */
	.byte	0x15, 0x00, 0x47, 0x00, 0x00, 0x00, 0x00, 0x00	/* |..G.....| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x0d, 0x00	/* |........| */
	.byte	0x0e, 0x00, 0x0f, 0x00	/* |....| */
D_0adc:
	.byte	0x00, 0x00	/* |..| */
D_0ade:
	.byte	0xff, 0xff	/* |..| */
D_0ae0:
	.byte	0x00, 0x00	/* |..| */
D_0ae2:
	.byte	0x00, 0x00, 0x07, 0x03, 0x13, 0x03, 0x13, 0x03	/* |........| */
	.byte	0xae, 0x03, 0x85, 0x04, 0x8e, 0x04, 0x9d, 0x04	/* |........| */
	.byte	0x1f, 0x09, 0xa5, 0x04, 0xb5, 0x04, 0xc6, 0x04	/* |........| */
	.byte	0xcf, 0x04, 0xde, 0x04, 0x1f, 0x09, 0x1f, 0x09	/* |........| */
	.byte	0xe6, 0x04, 0xf5, 0x04, 0x04, 0x05, 0x13, 0x05	/* |........| */
	.byte	0x22, 0x05, 0x2e, 0x05, 0x3a, 0x05, 0x46, 0x05	/* |"...:.F.| */
	.byte	0x52, 0x05, 0x59, 0x05, 0x60, 0x05, 0x68, 0x05	/* |R.Y.`.h.| */
	.byte	0x75, 0x05, 0x7d, 0x05, 0x8a, 0x05, 0x97, 0x05	/* |u.}.....| */
	.byte	0x9f, 0x05, 0xa7, 0x05, 0xc0, 0x05, 0xd9, 0x05	/* |........| */
	.byte	0xf2, 0x05, 0x0b, 0x06, 0x1b, 0x06, 0x2b, 0x06	/* |......+.| */
	.byte	0x3b, 0x06, 0x4b, 0x06, 0x5b, 0x06, 0x6b, 0x06	/* |;.K.[.k.| */
	.byte	0x7b, 0x06, 0x1f, 0x09, 0x8b, 0x06, 0xa1, 0x06	/* |{.......| */
	.byte	0xb1, 0x06, 0xbe, 0x06, 0xcf, 0x06, 0xe0, 0x06	/* |........| */
	.byte	0xf1, 0x06, 0x02, 0x07, 0x0b, 0x07, 0x10, 0x07	/* |........| */
	.byte	0x1c, 0x07, 0x28, 0x07, 0x34, 0x07, 0x40, 0x07	/* |..(.4.@.| */
	.byte	0x48, 0x07, 0x50, 0x07, 0x58, 0x07, 0x60, 0x07	/* |H.P.X.`.| */
	.byte	0x68, 0x07, 0x70, 0x07, 0x78, 0x07, 0x80, 0x07	/* |h.p.x...| */
	.byte	0xb0, 0x07, 0xdd, 0x07, 0xe7, 0x07, 0xef, 0x07	/* |........| */
	.byte	0xf9, 0x07, 0x01, 0x08, 0x09, 0x08, 0x1b, 0x08	/* |........| */
	.byte	0x2d, 0x08, 0x35, 0x08, 0x3d, 0x08, 0x45, 0x08	/* |-.5.=.E.| */
	.byte	0x4d, 0x08, 0x5b, 0x08, 0x65, 0x08, 0x6f, 0x08	/* |M.[.e.o.| */
	.byte	0x79, 0x08, 0x85, 0x08, 0x8f, 0x08, 0x99, 0x08	/* |y.......| */
	.byte	0xa1, 0x08, 0xaf, 0x08, 0xbd, 0x08, 0xcc, 0x08	/* |........| */
	.byte	0xdb, 0x08, 0xea, 0x08, 0xf4, 0x08, 0xfe, 0x08	/* |........| */
	.byte	0x08, 0x09	/* |..| */
D_0ba4:
	.byte	0x73, 0x74, 0x61, 0x74, 0x65, 0x20, 0x25, 0x64	/* |state %d| */
	.byte	0x2c, 0x20, 0x63, 0x68, 0x61, 0x72, 0x20, 0x30	/* |, char 0| */
	.byte	0x25, 0x6f, 0x0a, 0x00	/* |%o..| */
D_0bb8:
	.ascii	"yacc stack overflow\0"
D_0bcc:
	.ascii	"syntax error\0"
D_0bd9:
	.byte	0x65, 0x72, 0x72, 0x6f, 0x72, 0x20, 0x72, 0x65	/* |error re| */
	.byte	0x63, 0x6f, 0x76, 0x65, 0x72, 0x79, 0x20, 0x70	/* |covery p| */
	.byte	0x6f, 0x70, 0x73, 0x20, 0x73, 0x74, 0x61, 0x74	/* |ops stat| */
	.byte	0x65, 0x20, 0x25, 0x64, 0x2c, 0x20, 0x75, 0x6e	/* |e %d, un| */
	.byte	0x63, 0x6f, 0x76, 0x65, 0x72, 0x73, 0x20, 0x25	/* |covers %| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_0c04:
	.byte	0x65, 0x72, 0x72, 0x6f, 0x72, 0x20, 0x72, 0x65	/* |error re| */
	.byte	0x63, 0x6f, 0x76, 0x65, 0x72, 0x79, 0x20, 0x64	/* |covery d| */
	.byte	0x69, 0x73, 0x63, 0x61, 0x72, 0x64, 0x73, 0x20	/* |iscards | */
	.byte	0x63, 0x68, 0x61, 0x72, 0x20, 0x25, 0x64, 0x0a	/* |char %d.| */
	.byte	0x00	/* |.| */
D_0c25:
	.byte	0x72, 0x65, 0x64, 0x75, 0x63, 0x65, 0x20, 0x25	/* |reduce %| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_0c30:
	.ascii	"syntax error\0"
D_0c3d:
	.byte	0x2e, 0x09, 0x66, 0x72, 0x6f, 0x6d, 0x3a, 0x20	/* |..from: | */
	.byte	0x6f, 0x6c, 0x64, 0x20, 0x70, 0x73, 0x20, 0x25	/* |old ps %| */
	.byte	0x64, 0x2c, 0x20, 0x6e, 0x65, 0x77, 0x20, 0x70	/* |d, new p| */
	.byte	0x73, 0x20, 0x25, 0x64, 0x2c, 0x20, 0x66, 0x66	/* |s %d, ff| */
	.byte	0x6c, 0x67, 0x20, 0x25, 0x64, 0x0a, 0x00	/* |lg %d..| */
D_0c64:
	.byte	0x2e, 0x09, 0x74, 0x6f, 0x3a, 0x20, 0x6f, 0x6c	/* |..to: ol| */
	.byte	0x64, 0x20, 0x70, 0x73, 0x20, 0x25, 0x64, 0x2c	/* |d ps %d,| */
	.byte	0x20, 0x6e, 0x65, 0x77, 0x20, 0x70, 0x73, 0x20	/* | new ps | */
	.byte	0x25, 0x64, 0x0a, 0x00	/* |%d..| */
D_0c80:
	.byte	0x2e, 0x09, 0x65, 0x3a, 0x65, 0x62, 0x3a, 0x20	/* |..e:eb: | */
	.byte	0x53, 0x25, 0x64, 0x20, 0x3c, 0x2d, 0x20, 0x53	/* |S%d <- S| */
	.byte	0x25, 0x64, 0x20, 0x53, 0x25, 0x64, 0x3b, 0x20	/* |%d S%d; | */
	.byte	0x62, 0x3d, 0x25, 0x64, 0x2c, 0x20, 0x68, 0x3d	/* |b=%d, h=| */
	.byte	0x25, 0x64, 0x0a, 0x00	/* |%d..| */
D_0ca4:
	.byte	0x5c, 0x7c, 0x00	/* |\|.| */
D_0ca7:
	.byte	0x00	/* |.| */
D_0ca8:
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x77, 0x27, 0x5c, 0x73, 0x25, 0x64, 0x5c, 0x2a	/* |w'\s%d\*| */
	.byte	0x28, 0x25, 0x64, 0x25, 0x73, 0x27, 0x0a, 0x00	/* |(%d%s'..| */
D_0cc0:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x68, 0x27, 0x7c, 0x5c, 0x6e, 0x28, 0x39, 0x37	/* |h'|\n(97| */
	.byte	0x75, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75	/* |u-\n(%du| */
	.byte	0x27, 0x5c, 0x2a, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |'\*(%d..| */
D_0ce0:
	.byte	0x2e, 0x61, 0x73, 0x20, 0x25, 0x64, 0x20, 0x22	/* |.as %d "| */
	.byte	0x25, 0x73, 0x5c, 0x2a, 0x28, 0x25, 0x64, 0x0a	/* |%s\*(%d.| */
	.byte	0x00, 0x00	/* |..| */
D_0cf2:
	.byte	0x2e, 0x66, 0x74, 0x20, 0x25, 0x63, 0x0a, 0x00	/* |.ft %c..| */
D_0cfa:
	.byte	0x2e, 0x09, 0x73, 0x65, 0x74, 0x66, 0x6f, 0x6e	/* |..setfon| */
	.byte	0x74, 0x20, 0x25, 0x63, 0x0a, 0x00	/* |t %c..| */
D_0d08:
	.byte	0x2e, 0x09, 0x62, 0x3a, 0x66, 0x62, 0x3a, 0x20	/* |..b:fb: | */
	.byte	0x53, 0x25, 0x64, 0x20, 0x3c, 0x2d, 0x20, 0x5c	/* |S%d <- \| */
	.byte	0x66, 0x25, 0x63, 0x20, 0x53, 0x25, 0x64, 0x20	/* |f%c S%d | */
	.byte	0x5c, 0x66, 0x25, 0x63, 0x20, 0x62, 0x3d, 0x25	/* |\f%c b=%| */
	.byte	0x64, 0x2c, 0x68, 0x3d, 0x25, 0x64, 0x2c, 0x6c	/* |d,h=%d,l| */
	.byte	0x66, 0x3d, 0x25, 0x63, 0x2c, 0x72, 0x66, 0x3d	/* |f=%c,rf=| */
	.byte	0x25, 0x63, 0x0a, 0x00	/* |%c..| */
D_0d3c:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x66, 0x25, 0x63, 0x5c, 0x2a, 0x28, 0x25, 0x64	/* |f%c\*(%d| */
	.byte	0x5c, 0x66, 0x25, 0x63, 0x0a, 0x00	/* |\f%c..| */
D_0d52:
	.byte	0x2e, 0x66, 0x74, 0x20, 0x25, 0x63, 0x0a, 0x00	/* |.ft %c..| */
D_0d5a:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x2a, 0x28, 0x25, 0x64, 0x5c, 0x68, 0x27, 0x2d	/* |*(%d\h'-| */
	.byte	0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75, 0x2b, 0x25	/* |\n(%du+%| */
	.byte	0x64, 0x75, 0x27, 0x5c, 0x2a, 0x28, 0x25, 0x64	/* |du'\*(%d| */
	.byte	0x0a, 0x00	/* |..| */
D_0d7c:
	.byte	0x2e, 0x09, 0x66, 0x61, 0x74, 0x20, 0x25, 0x64	/* |..fat %d| */
	.byte	0x2c, 0x20, 0x73, 0x68, 0x3d, 0x25, 0x64, 0x0a	/* |, sh=%d.| */
	.byte	0x00	/* |.| */
D_0d8d:
	.byte	0x2e, 0x66, 0x74, 0x20, 0x25, 0x63, 0x0a, 0x00	/* |.ft %c..| */
	.byte	0x00	/* |.| */
D_0d96:
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x6e, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |n(%d..| */
D_0da4:
	.byte	0x2e, 0x69, 0x66, 0x20, 0x5c, 0x6e, 0x28, 0x25	/* |.if \n(%| */
	.byte	0x64, 0x3e, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x20	/* |d>\n(%d | */
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x6e, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |n(%d..| */
D_0dc2:
	.byte	0x2e, 0x69, 0x66, 0x20, 0x5c, 0x6e, 0x28, 0x25	/* |.if \n(%| */
	.byte	0x64, 0x3e, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x20	/* |d>\n(%d | */
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x6e, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |n(%d..| */
D_0de0:
	.ascii	".ds %d \0"
D_0de8:
	.byte	0x5c, 0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\v'%du'\| */
	.byte	0x68, 0x27, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75	/* |h'\n(%du| */
	.byte	0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75, 0x2f	/* |-\n(%du/| */
D_0e00:
	.ascii	"2u'\s%d\*(%d\s%d\0"
D_0e11:
	.byte	0x5c, 0x68, 0x27, 0x2d, 0x5c, 0x6e, 0x28, 0x25	/* |\h'-\n(%| */
	.byte	0x64, 0x75, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64	/* |du-\n(%d| */
	.byte	0x75, 0x2f, 0x32, 0x75, 0x27, 0x5c, 0x76, 0x27	/* |u/2u'\v'| */
	.byte	0x25, 0x64, 0x75, 0x27, 0x5c, 0x0a, 0x00	/* |%du'\..| */
D_0e30:
	.byte	0x5c, 0x68, 0x27, 0x5c, 0x6e, 0x28, 0x25, 0x64	/* |\h'\n(%d| */
	.byte	0x75, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75	/* |u-\n(%du| */
	.byte	0x2f, 0x32, 0x75, 0x27, 0x5c, 0x2a, 0x28, 0x25	/* |/2u'\*(%| */
	.byte	0x64, 0x5c, 0x68, 0x27, 0x5c, 0x6e, 0x28, 0x25	/* |d\h'\n(%| */
	.byte	0x64, 0x75, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64	/* |du-\n(%d| */
	.byte	0x75, 0x2b, 0x32, 0x75, 0x2f, 0x32, 0x75, 0x27	/* |u+2u/2u'| */
	.byte	0x5c, 0x0a, 0x00	/* |\..| */
D_0e63:
	.byte	0x5c, 0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\v'%du'\| */
	.byte	0x68, 0x27, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64	/* |h'-\n(%d| */
	.byte	0x75, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75	/* |u-\n(%du| */
	.byte	0x2f, 0x32, 0x75, 0x27, 0x5c, 0x73, 0x25, 0x64	/* |/2u'\s%d| */
	.byte	0x5c, 0x2a, 0x28, 0x25, 0x64, 0x5c, 0x73, 0x25	/* |\*(%d\s%| */
	.byte	0x64, 0x5c, 0x68, 0x27, 0x5c, 0x6e, 0x28, 0x25	/* |d\h'\n(%| */
	.byte	0x64, 0x75, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64	/* |du-\n(%d| */
	.byte	0x75, 0x2f, 0x32, 0x75, 0x27, 0x5c, 0x76, 0x27	/* |u/2u'\v'| */
	.byte	0x25, 0x64, 0x75, 0x27, 0x5c, 0x0a, 0x00	/* |%du'\..| */
D_0eaa:
	.byte	0x0a, 0x00	/* |..| */
D_0eac:
	.byte	0x2e, 0x09, 0x66, 0x72, 0x6f, 0x6d, 0x20, 0x74	/* |..from t| */
	.byte	0x6f, 0x3a, 0x20, 0x53, 0x25, 0x64, 0x20, 0x3c	/* |o: S%d <| */
	.byte	0x2d, 0x20, 0x25, 0x64, 0x20, 0x66, 0x20, 0x25	/* |- %d f %| */
	.byte	0x64, 0x20, 0x74, 0x20, 0x25, 0x64, 0x3b, 0x20	/* |d t %d; | */
	.byte	0x68, 0x3d, 0x25, 0x64, 0x20, 0x62, 0x3d, 0x25	/* |h=%d b=%| */
	.byte	0x64, 0x0a, 0x00, 0x00, 0xe8, 0x0e, 0x04, 0x0f	/* |d.......| */
	.byte	0xfd, 0x0e, 0xef, 0x0e	/* |....| */
	.word	D_0ef6	/* ptr: 0x0ef6 */
D_0ee2:
	.ascii	"\(*S\0"
D_0ee7:
	.ascii	"\(cu\0"
D_0eec:
	.ascii	"\(ca\0"
D_0ef1:
	.ascii	"\(*P\0"
D_0ef6:
	.ascii	"funny type %d in funny\0"
D_0f0d:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x25	/* |.ds %d %| */
	.byte	0x73, 0x0a, 0x00	/* |s..| */
D_0f18:
	.byte	0x2e, 0x09, 0x66, 0x75, 0x6e, 0x6e, 0x79, 0x3a	/* |..funny:| */
	.byte	0x20, 0x53, 0x25, 0x64, 0x20, 0x3c, 0x2d, 0x20	/* | S%d <- | */
	.byte	0x25, 0x73, 0x3b, 0x20, 0x68, 0x3d, 0x25, 0x64	/* |%s; h=%d| */
	.byte	0x20, 0x62, 0x3d, 0x25, 0x64, 0x0a, 0x00, 0x00	/* | b=%d...| */
D_0f38:
	.byte	0x03, 0x00	/* |..| */
D_0f3a:
	.byte	0x0a, 0x00	/* |..| */
D_0f3c:
	.byte	0x31, 0x00	/* |1.| */
D_0f3e:
	.byte	0x00, 0x00	/* |..| */
D_0f40:
	.byte	0x00, 0x00	/* |..| */
D_0f42:
	.byte	0x00, 0x00	/* |..| */
D_0f44:
	.byte	0x2e, 0x09, 0x69, 0x6e, 0x74, 0x65, 0x67, 0x72	/* |..integr| */
	.byte	0x61, 0x6c, 0x3a, 0x20, 0x53, 0x25, 0x64, 0x3b	/* |al: S%d;| */
	.byte	0x20, 0x68, 0x3d, 0x25, 0x64, 0x20, 0x62, 0x3d	/* | h=%d b=| */
	.byte	0x25, 0x64, 0x0a, 0x00	/* |%d..| */
D_0f60:
	.ascii	"\(is\0"
D_0f65:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x25	/* |.ds %d %| */
	.byte	0x73, 0x0a, 0x00	/* |s..| */
D_0f70:
	.word	D_1136	/* ptr: 0x3136 */
	.word	D_1585	/* ptr: 0x1585 */
	.byte	0xd9, 0x15, 0xc9, 0x15, 0xdf, 0x15, 0xdf, 0x15	/* |........| */
	.byte	0xdf, 0x15, 0xdf, 0x15, 0xdf, 0x15, 0xdf, 0x15	/* |........| */
	.byte	0xdf, 0x15, 0xdf, 0x15, 0xdf, 0x15	/* |......| */
	.word	D_15b6	/* ptr: 0x15b6 */
	.byte	0xdf, 0x15, 0xdf, 0x15, 0xa3, 0x15	/* |......| */
D_0f92:
	.byte	0x25, 0x73, 0x00	/* |%s.| */
D_0f95:
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x39, 0x39, 0x20, 0x5c	/* |.nr 99 \| */
	.byte	0x6e, 0x28, 0x2e, 0x73, 0x0a, 0x2e, 0x6e, 0x72	/* |n(.s..nr| */
	.byte	0x20, 0x39, 0x38, 0x20, 0x5c, 0x6e, 0x28, 0x2e	/* | 98 \n(.| */
	.byte	0x66, 0x0a, 0x00	/* |f..| */
D_0fb0:
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x77, 0x27, 0x5c, 0x2a, 0x28, 0x25, 0x64, 0x27	/* |w'\*(%d'| */
	.byte	0x0a, 0x00	/* |..| */
D_0fc2:
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x4d, 0x4b, 0x20, 0x25	/* |.nr MK %| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_0fcd:
	.byte	0x2e, 0x69, 0x66, 0x20, 0x25, 0x64, 0x3e, 0x5c	/* |.if %d>\| */
	.byte	0x6e, 0x28, 0x2e, 0x76, 0x20, 0x2e, 0x6e, 0x65	/* |n(.v .ne| */
	.byte	0x20, 0x25, 0x64, 0x75, 0x0a, 0x00	/* | %du..| */
D_0fe3:
	.byte	0x2e, 0x72, 0x6e, 0x20, 0x25, 0x64, 0x20, 0x31	/* |.rn %d 1| */
	.byte	0x30, 0x0a, 0x00	/* |0..| */
D_0fee:
	.byte	0x5c, 0x2a, 0x28, 0x31, 0x30, 0x0a, 0x00	/* |\*(10..| */
D_0ff5:
	.byte	0x2e, 0x70, 0x73, 0x20, 0x5c, 0x6e, 0x28, 0x39	/* |.ps \n(9| */
	.byte	0x39, 0x0a, 0x2e, 0x66, 0x74, 0x20, 0x5c, 0x6e	/* |9..ft \n| */
	.byte	0x28, 0x39, 0x38, 0x0a, 0x00	/* |(98..| */
D_100a:
	.word	D_252e	/* ptr: 0x452e */
	.byte	0x4e, 0x00	/* |N.| */
D_100e:
	.byte	0x25, 0x73, 0x00	/* |%s.| */
D_1011:
	.byte	0x69, 0x6e, 0x70, 0x75, 0x74, 0x20, 0x6c, 0x69	/* |input li| */
	.byte	0x6e, 0x65, 0x20, 0x74, 0x6f, 0x6f, 0x20, 0x6c	/* |ne too l| */
	.byte	0x6f, 0x6e, 0x67, 0x3a, 0x20, 0x25, 0x2e, 0x32	/* |ong: %.2| */
	.byte	0x30, 0x73, 0x0a, 0x00	/* |0s..| */
D_102d:
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x39, 0x39, 0x20, 0x5c	/* |.nr 99 \| */
	.byte	0x6e, 0x28, 0x2e, 0x73, 0x0a, 0x2e, 0x6e, 0x72	/* |n(.s..nr| */
	.byte	0x20, 0x39, 0x38, 0x20, 0x5c, 0x6e, 0x28, 0x2e	/* | 98 \n(.| */
	.byte	0x66, 0x0a, 0x00	/* |f..| */
D_1048:
	.byte	0x2e, 0x72, 0x6d, 0x20, 0x25, 0x64, 0x20, 0x0a	/* |.rm %d .| */
	.byte	0x00	/* |.| */
D_1051:
	.byte	0x2e, 0x61, 0x73, 0x20, 0x25, 0x64, 0x20, 0x22	/* |.as %d "| */
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_105d:
	.byte	0x2e, 0x61, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.as %d \| */
	.byte	0x2a, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |*(%d..| */
D_106b:
	.byte	0x2e, 0x70, 0x73, 0x20, 0x5c, 0x6e, 0x28, 0x39	/* |.ps \n(9| */
	.byte	0x39, 0x0a, 0x2e, 0x66, 0x74, 0x20, 0x5c, 0x6e	/* |9..ft \n| */
	.byte	0x28, 0x39, 0x38, 0x0a, 0x00	/* |(98..| */
D_1080:
	.ascii	".as %d \"%s\0"
D_108b:
	.byte	0x2e, 0x70, 0x73, 0x20, 0x5c, 0x6e, 0x28, 0x39	/* |.ps \n(9| */
	.byte	0x39, 0x0a, 0x2e, 0x66, 0x74, 0x20, 0x5c, 0x6e	/* |9..ft \n| */
	.byte	0x28, 0x39, 0x38, 0x0a, 0x00	/* |(98..| */
D_10a0:
	.byte	0x5c, 0x2a, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |\*(%d..| */
D_10a7:
	.byte	0x2e, 0x09, 0x61, 0x6e, 0x73, 0x77, 0x65, 0x72	/* |..answer| */
	.byte	0x20, 0x3c, 0x2d, 0x20, 0x53, 0x25, 0x64, 0x2c	/* | <- S%d,| */
	.byte	0x20, 0x68, 0x3d, 0x25, 0x64, 0x2c, 0x62, 0x3d	/* | h=%d,b=| */
	.byte	0x25, 0x64, 0x0a, 0x00	/* |%d..| */
D_10c3:
	.ascii	".ds %d \x'0'\0"
D_10d0:
	.ascii	"\x'0-%du'\0"
D_10da:
	.ascii	"\f%c\s%d\*(%d%s\s\n(99\f\n(98\0"
D_10f8:
	.byte	0x5c, 0x7c, 0x00	/* |\|.| */
D_10fb:
	.byte	0x00	/* |.| */
D_10fc:
	.ascii	"\x'%du'\0"
D_1104:
	.ascii	"no eqn strings left\0"
D_1118:
	.byte	0x2e, 0x70, 0x73, 0x20, 0x25, 0x64, 0x0a, 0x00	/* |.ps %d..| */
D_1120:
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x77, 0x27, 0x5c, 0x73, 0x25, 0x64, 0x5c, 0x2a	/* |w'\s%d\*| */
	.byte	0x28, 0x25, 0x64, 0x27, 0x0a, 0x00	/* |(%d'..| */
D_1136:
	.byte	0x2d, 0x00	/* |-.| */
D_1138:
	.byte	0x72, 0x00	/* |r.| */
D_113a:
	.ascii	"can't open file %s\0"
D_114d:
	.byte	0x2e, 0x66, 0x74, 0x20, 0x25, 0x63, 0x0a, 0x00	/* |.ft %c..| */
D_1155:
	.ascii	"eqn fatal error: \0"
D_1167:
	.byte	0x0a, 0x66, 0x69, 0x6c, 0x65, 0x20, 0x25, 0x73	/* |.file %s| */
	.byte	0x2c, 0x20, 0x62, 0x65, 0x74, 0x77, 0x65, 0x65	/* |, betwee| */
	.byte	0x6e, 0x20, 0x6c, 0x69, 0x6e, 0x65, 0x73, 0x20	/* |n lines | */
	.byte	0x25, 0x64, 0x20, 0x61, 0x6e, 0x64, 0x20, 0x25	/* |%d and %| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_118a:
	.ascii	"eqn: \0"
D_1190:
	.ascii	"fatal error: \0"
D_119e:
	.byte	0x0a, 0x66, 0x69, 0x6c, 0x65, 0x20, 0x25, 0x73	/* |.file %s| */
	.byte	0x2c, 0x20, 0x62, 0x65, 0x74, 0x77, 0x65, 0x65	/* |, betwee| */
	.byte	0x6e, 0x20, 0x6c, 0x69, 0x6e, 0x65, 0x73, 0x20	/* |n lines | */
	.byte	0x25, 0x64, 0x20, 0x61, 0x6e, 0x64, 0x20, 0x25	/* |%d and %| */
	.byte	0x64, 0x0a, 0x00, 0x00	/* |d...| */
D_11c2:
	.word	D_2de8	/* ptr: 0x4de8 */
D_11c4:
	.byte	0x72, 0x00	/* |r.| */
D_11c6:
	.ascii	"can't open file %s\0"
D_11d9:
	.ascii	"pushback overflow\0"
D_11eb:
	.ascii	"quoted string %.20s... too long\0"
D_120b:
	.ascii	"missing \" in %.20s\0"
D_121e:
	.byte	0x2e, 0x09, 0x6c, 0x65, 0x78, 0x20, 0x74, 0x6f	/* |..lex to| */
	.byte	0x6b, 0x65, 0x6e, 0x20, 0x3d, 0x20, 0x7c, 0x25	/* |ken = |%| */
	.byte	0x73, 0x7c, 0x0a, 0x00	/* |s|..| */
D_1232:
	.byte	0x2e, 0x09, 0x66, 0x6f, 0x75, 0x6e, 0x64, 0x20	/* |..found | */
	.byte	0x25, 0x73, 0x7c, 0x3d, 0x25, 0x73, 0x7c, 0x0a	/* |%s|=%s|.| */
	.byte	0x00	/* |.| */
D_1243:
	.byte	0x2e, 0x09, 0x25, 0x73, 0x20, 0x69, 0x73, 0x20	/* |..%s is | */
	.byte	0x6e, 0x6f, 0x74, 0x20, 0x61, 0x20, 0x6b, 0x65	/* |not a ke| */
	.byte	0x79, 0x77, 0x6f, 0x72, 0x64, 0x0a, 0x00	/* |yword..| */
D_125a:
	.ascii	"token %.20s... too long\0"
D_1272:
	.ascii	"Unexpected end of input at %.20s\0"
D_1293:
	.ascii	"Unterminated definition at %.20s\0"
D_12b4:
	.byte	0x2e, 0x09, 0x6e, 0x61, 0x6d, 0x65, 0x20, 0x25	/* |..name %| */
	.byte	0x73, 0x20, 0x64, 0x65, 0x66, 0x69, 0x6e, 0x65	/* |s define| */
	.byte	0x64, 0x20, 0x61, 0x73, 0x20, 0x25, 0x73, 0x0a	/* |d as %s.| */
	.byte	0x00	/* |.| */
D_12cd:
	.ascii	"out of space in strsave on %s\0"
D_12eb:
	.ascii	"Include not yet implemented\0"
D_1307:
	.ascii	"Bizarre delimiters at %.20s\0"
	.byte	0x00	/* |.| */
D_1324:
	.word	D_153c	/* ptr: 0x353c */
D_1326:
	.byte	0x23, 0x01	/* |#.| */
	.word	D_1540	/* ptr: 0x3540 */
	.byte	0x22, 0x01	/* |".| */
	.word	D_1544	/* ptr: 0x3544 */
	.byte	0xff, 0xff	/* |..| */
	.word	D_1548	/* ptr: 0x3548 */
	.byte	0x1e, 0x01	/* |..| */
	.word	D_154d	/* ptr: 0x354d */
	.byte	0x1f, 0x01	/* |..| */
	.word	D_1550	/* ptr: 0x3550 */
	.byte	0x0d, 0x01	/* |..| */
	.word	D_1554	/* ptr: 0x3554 */
	.byte	0x32, 0x01	/* |2.| */
	.word	D_1558	/* ptr: 0x3558 */
	.byte	0x36, 0x01	/* |6.| */
	.word	D_155c	/* ptr: 0x355c */
	.byte	0x37, 0x01	/* |7.| */
	.word	D_1561	/* ptr: 0x3561 */
	.byte	0x30, 0x01	/* |0.| */
	.word	D_1565	/* ptr: 0x3565 */
	.byte	0x31, 0x01	/* |1.| */
	.word	D_156c	/* ptr: 0x356c */
	.byte	0x34, 0x01	/* |4.| */
	.word	D_1570	/* ptr: 0x3570 */
	.byte	0x33, 0x01	/* |3.| */
	.word	D_1576	/* ptr: 0x3576 */
	.byte	0x35, 0x01	/* |5.| */
	.word	D_157c	/* ptr: 0x357c */
	.byte	0x0f, 0x01	/* |..| */
	.word	D_1581	/* ptr: 0x3581 */
	.byte	0x0e, 0x01	/* |..| */
	.word	D_1585	/* ptr: 0x3585 */
	.byte	0x0e, 0x01	/* |..| */
	.word	D_158e	/* ptr: 0x358e */
	.byte	0x10, 0x01	/* |..| */
	.word	D_1594	/* ptr: 0x3594 */
	.byte	0x11, 0x01	/* |..| */
	.word	D_159a	/* ptr: 0x359a */
	.byte	0x13, 0x01	/* |..| */
	.word	D_159f	/* ptr: 0x359f */
	.byte	0x12, 0x01	/* |..| */
	.word	D_15a5	/* ptr: 0x35a5 */
	.byte	0x14, 0x01	/* |..| */
	.word	D_15ab	/* ptr: 0x35ab */
	.byte	0x15, 0x01	/* |..| */
	.word	D_15b1	/* ptr: 0x35b1 */
	.byte	0x20, 0x01	/* | .| */
	.word	D_15b6	/* ptr: 0x35b6 */
	.byte	0x21, 0x01	/* |!.| */
	.word	D_15bb	/* ptr: 0x35bb */
	.byte	0x16, 0x01	/* |..| */
	.word	D_15c1	/* ptr: 0x35c1 */
	.word	D_0124	/* ptr: 0x0124 */
	.word	D_15c6	/* ptr: 0x35c6 */
	.byte	0x25, 0x01	/* |%.| */
	.word	D_15cb	/* ptr: 0x35cb */
	.byte	0x29, 0x01	/* |).| */
	.word	D_15cf	/* ptr: 0x35cf */
	.byte	0x26, 0x01	/* |&.| */
	.word	D_15d5	/* ptr: 0x35d5 */
	.byte	0x27, 0x01	/* |'.| */
	.word	D_15dc	/* ptr: 0x35dc */
	.byte	0x28, 0x01	/* |(.| */
	.word	D_15e1	/* ptr: 0x35e1 */
	.byte	0x2e, 0x01	/* |..| */
	.word	D_15e6	/* ptr: 0x35e6 */
	.byte	0x2f, 0x01	/* |/.| */
	.word	D_15ec	/* ptr: 0x35ec */
	.byte	0x1a, 0x01	/* |..| */
	.word	D_15f2	/* ptr: 0x35f2 */
	.byte	0x17, 0x01	/* |..| */
	.word	D_15f9	/* ptr: 0x35f9 */
	.word	D_0118	/* ptr: 0x0118 */
	.word	D_1601	/* ptr: 0x3601 */
	.byte	0x17, 0x01	/* |..| */
	.word	D_1609	/* ptr: 0x3609 */
	.byte	0x1b, 0x01	/* |..| */
	.word	D_160f	/* ptr: 0x360f */
	.byte	0x1b, 0x01	/* |..| */
	.word	D_1616	/* ptr: 0x3616 */
	.byte	0x1c, 0x01	/* |..| */
	.word	D_161c	/* ptr: 0x361c */
	.byte	0x1d, 0x01	/* |..| */
	.word	D_1624	/* ptr: 0x3624 */
	.byte	0x2a, 0x01	/* |*.| */
	.word	D_1627	/* ptr: 0x3627 */
	.byte	0x2b, 0x01	/* |+.| */
	.word	D_162c	/* ptr: 0x362c */
	.byte	0x2d, 0x01	/* |-.| */
	.word	D_1630	/* ptr: 0x3630 */
	.byte	0x2c, 0x01	/* |,.| */
	.word	D_1635	/* ptr: 0x3635 */
	.byte	0x0b, 0x01	/* |..| */
	.word	D_163a	/* ptr: 0x363a */
	.byte	0x0c, 0x01	/* |..| */
	.word	D_1641	/* ptr: 0x3641 */
	.byte	0x06, 0x01	/* |..| */
	.word	D_1648	/* ptr: 0x3648 */
	.byte	0x0a, 0x01	/* |..| */
	.word	D_164c	/* ptr: 0x364c */
	.byte	0x07, 0x01	/* |..| */
	.word	D_1651	/* ptr: 0x3651 */
	.byte	0x08, 0x01	/* |..| */
	.word	D_1656	/* ptr: 0x3656 */
	.byte	0x09, 0x01, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_13fc:
	.word	D_165b	/* ptr: 0x365b */
D_13fe:
	.word	D_165e	/* ptr: 0x365e */
	.word	D_1663	/* ptr: 0x3663 */
	.word	D_1666	/* ptr: 0x3666 */
	.word	D_166b	/* ptr: 0x366b */
	.word	D_166e	/* ptr: 0x366e */
	.word	D_1673	/* ptr: 0x3673 */
	.word	D_1676	/* ptr: 0x3676 */
	.word	D_167b	/* ptr: 0x367b */
	.word	D_167e	/* ptr: 0x367e */
	.word	D_1683	/* ptr: 0x3683 */
	.word	D_1686	/* ptr: 0x3686 */
	.word	D_168b	/* ptr: 0x368b */
	.word	D_168e	/* ptr: 0x368e */
	.word	D_1693	/* ptr: 0x3693 */
	.word	D_1697	/* ptr: 0x3697 */
	.word	D_169c	/* ptr: 0x369c */
	.word	D_16a5	/* ptr: 0x36a5 */
	.word	D_16aa	/* ptr: 0x36aa */
	.word	D_16b2	/* ptr: 0x36b2 */
	.word	D_16b7	/* ptr: 0x36b7 */
	.word	D_16bc	/* ptr: 0x36bc */
	.word	D_16c7	/* ptr: 0x36c7 */
	.word	D_16cd	/* ptr: 0x36cd */
	.word	D_16d8	/* ptr: 0x36d8 */
	.word	D_16df	/* ptr: 0x36df */
	.word	D_16e7	/* ptr: 0x36e7 */
	.byte	0xef, 0x36	/* |.6| */
	.word	D_16f0	/* ptr: 0x36f0 */
	.word	D_16f6	/* ptr: 0x36f6 */
	.word	D_16fb	/* ptr: 0x36fb */
	.word	D_16ff	/* ptr: 0x36ff */
	.word	D_1704	/* ptr: 0x3704 */
	.word	D_1709	/* ptr: 0x3709 */
	.word	D_170e	/* ptr: 0x370e */
	.word	D_1711	/* ptr: 0x3711 */
	.word	D_1714	/* ptr: 0x3714 */
	.word	D_1717	/* ptr: 0x3717 */
	.word	D_171a	/* ptr: 0x371a */
	.byte	0x21, 0x37	/* |!7| */
	.word	D_1729	/* ptr: 0x3729 */
	.word	D_172e	/* ptr: 0x372e */
	.word	D_173d	/* ptr: 0x373d */
	.word	D_1741	/* ptr: 0x3741 */
	.word	D_1745	/* ptr: 0x3745 */
	.word	D_174b	/* ptr: 0x374b */
	.word	D_1751	/* ptr: 0x3751 */
	.word	D_1757	/* ptr: 0x3757 */
	.word	D_175c	/* ptr: 0x375c */
	.word	D_1761	/* ptr: 0x3761 */
	.word	D_1766	/* ptr: 0x3766 */
	.word	D_176c	/* ptr: 0x376c */
	.word	D_1771	/* ptr: 0x3771 */
	.word	D_1777	/* ptr: 0x3777 */
	.word	D_177c	/* ptr: 0x377c */
	.word	D_1782	/* ptr: 0x3782 */
	.word	D_1787	/* ptr: 0x3787 */
	.word	D_178d	/* ptr: 0x378d */
	.word	D_1792	/* ptr: 0x3792 */
	.word	D_179a	/* ptr: 0x379a */
	.word	D_179f	/* ptr: 0x379f */
	.word	D_17a7	/* ptr: 0x37a7 */
	.word	D_17af	/* ptr: 0x37af */
	.word	D_17b5	/* ptr: 0x37b5 */
	.word	D_17ba	/* ptr: 0x37ba */
	.word	D_17c0	/* ptr: 0x37c0 */
	.word	D_17c5	/* ptr: 0x37c5 */
	.word	D_17cc	/* ptr: 0x37cc */
	.word	D_17d1	/* ptr: 0x37d1 */
	.word	D_17d8	/* ptr: 0x37d8 */
	.word	D_17dd	/* ptr: 0x37dd */
	.word	D_17e0	/* ptr: 0x37e0 */
	.word	D_17e5	/* ptr: 0x37e5 */
	.word	D_17e8	/* ptr: 0x37e8 */
	.word	D_17ed	/* ptr: 0x37ed */
	.word	D_17f3	/* ptr: 0x37f3 */
	.word	D_17f8	/* ptr: 0x37f8 */
	.word	D_17fe	/* ptr: 0x37fe */
	.word	D_1803	/* ptr: 0x3803 */
	.word	D_1807	/* ptr: 0x3807 */
	.word	D_180c	/* ptr: 0x380c */
	.word	D_1810	/* ptr: 0x3810 */
	.word	D_1815	/* ptr: 0x3815 */
	.word	D_1818	/* ptr: 0x3818 */
	.word	D_181d	/* ptr: 0x381d */
	.word	D_1820	/* ptr: 0x3820 */
	.word	D_1825	/* ptr: 0x3825 */
	.word	D_182b	/* ptr: 0x382b */
	.word	D_1830	/* ptr: 0x3830 */
	.word	D_1836	/* ptr: 0x3836 */
	.word	D_183b	/* ptr: 0x383b */
	.word	D_183e	/* ptr: 0x383e */
	.word	D_1843	/* ptr: 0x3843 */
	.word	D_1846	/* ptr: 0x3846 */
	.word	D_184b	/* ptr: 0x384b */
	.word	D_1850	/* ptr: 0x3850 */
	.word	D_1855	/* ptr: 0x3855 */
	.word	D_185a	/* ptr: 0x385a */
	.word	D_185f	/* ptr: 0x385f */
	.word	D_1863	/* ptr: 0x3863 */
	.word	D_1868	/* ptr: 0x3868 */
	.word	D_186e	/* ptr: 0x386e */
	.word	D_1873	/* ptr: 0x3873 */
	.word	D_1877	/* ptr: 0x3877 */
	.word	D_187c	/* ptr: 0x387c */
	.word	D_1880	/* ptr: 0x3880 */
	.word	D_1885	/* ptr: 0x3885 */
	.word	D_188d	/* ptr: 0x388d */
	.word	D_1892	/* ptr: 0x3892 */
	.word	D_189a	/* ptr: 0x389a */
	.word	D_189f	/* ptr: 0x389f */
	.word	D_18a7	/* ptr: 0x38a7 */
	.word	D_18ac	/* ptr: 0x38ac */
	.word	D_18b0	/* ptr: 0x38b0 */
	.word	D_18b5	/* ptr: 0x38b5 */
	.word	D_18b9	/* ptr: 0x38b9 */
	.word	D_18be	/* ptr: 0x38be */
	.word	D_18c2	/* ptr: 0x38c2 */
	.word	D_18c7	/* ptr: 0x38c7 */
	.word	D_18cb	/* ptr: 0x38cb */
	.word	D_18d5	/* ptr: 0x38d5 */
	.word	D_18d9	/* ptr: 0x38d9 */
	.word	D_18e3	/* ptr: 0x38e3 */
	.word	D_18e6	/* ptr: 0x38e6 */
	.word	D_18ef	/* ptr: 0x38ef */
	.word	D_18f2	/* ptr: 0x38f2 */
	.word	D_18fb	/* ptr: 0x38fb */
	.word	D_18fe	/* ptr: 0x38fe */
	.word	D_1907	/* ptr: 0x3907 */
	.word	D_190b	/* ptr: 0x390b */
	.word	D_1915	/* ptr: 0x3915 */
	.word	D_1919	/* ptr: 0x3919 */
	.word	D_1923	/* ptr: 0x3923 */
	.word	D_1927	/* ptr: 0x3927 */
	.word	D_1931	/* ptr: 0x3931 */
	.word	D_1935	/* ptr: 0x3935 */
	.word	D_193f	/* ptr: 0x393f */
	.word	D_1944	/* ptr: 0x3944 */
	.word	D_194f	/* ptr: 0x394f */
	.word	D_1954	/* ptr: 0x3954 */
	.word	D_195f	/* ptr: 0x395f */
	.word	D_1964	/* ptr: 0x3964 */
	.word	D_196f	/* ptr: 0x396f */
	.word	D_1974	/* ptr: 0x3974 */
	.word	D_197f	/* ptr: 0x397f */
	.word	D_1983	/* ptr: 0x3983 */
	.word	D_198d	/* ptr: 0x398d */
	.word	D_1991	/* ptr: 0x3991 */
	.word	D_199b	/* ptr: 0x399b */
	.word	D_199f	/* ptr: 0x399f */
	.word	D_19a9	/* ptr: 0x39a9 */
	.word	D_19ad	/* ptr: 0x39ad */
	.word	D_19b7	/* ptr: 0x39b7 */
	.word	D_19ba	/* ptr: 0x39ba */
	.word	D_19c3	/* ptr: 0x39c3 */
	.word	D_19c7	/* ptr: 0x39c7 */
	.word	D_19d1	/* ptr: 0x39d1 */
	.word	D_19d5	/* ptr: 0x39d5 */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_153c:
	.byte	0x73, 0x75, 0x62, 0x00	/* |sub.| */
D_1540:
	.byte	0x73, 0x75, 0x70, 0x00	/* |sup.| */
D_1544:
	.word	D_252e	/* ptr: 0x452e */
	.byte	0x4e, 0x00	/* |N.| */
D_1548:
	.ascii	"from\0"
D_154d:
	.byte	0x74, 0x6f, 0x00	/* |to.| */
D_1550:
	.byte	0x73, 0x75, 0x6d, 0x00	/* |sum.| */
D_1554:
	.byte	0x68, 0x61	/* |ha| */
	.word	D_0074	/* ptr: 0x0074 */
D_1558:
	.byte	0x76, 0x65, 0x63, 0x00	/* |vec.| */
D_155c:
	.ascii	"dyad\0"
D_1561:
	.byte	0x64, 0x6f, 0x74, 0x00	/* |dot.| */
D_1565:
	.ascii	"dotdot\0"
D_156c:
	.byte	0x62, 0x61, 0x72, 0x00	/* |bar.| */
D_1570:
	.ascii	"tilde\0"
D_1576:
	.ascii	"under\0"
D_157c:
	.ascii	"prod\0"
D_1581:
	.byte	0x69, 0x6e, 0x74, 0x00	/* |int.| */
D_1585:
	.ascii	"integral\0"
D_158e:
	.ascii	"union\0"
D_1594:
	.ascii	"inter\0"
D_159a:
	.ascii	"pile\0"
D_159f:
	.ascii	"lpile\0"
D_15a5:
	.ascii	"cpile\0"
D_15ab:
	.ascii	"rpile\0"
D_15b1:
	.ascii	"over\0"
D_15b6:
	.ascii	"sqrt\0"
D_15bb:
	.byte	0x61, 0x62, 0x6f	/* |abo| */
D_15be:
	.byte	0x76, 0x65, 0x00	/* |ve.| */
D_15c1:
	.ascii	"size\0"
D_15c6:
	.ascii	"font\0"
D_15cb:
	.byte	0x66, 0x61, 0x74, 0x00	/* |fat.| */
D_15cf:
	.ascii	"roman\0"
D_15d5:
	.ascii	"italic\0"
D_15dc:
	.ascii	"bold\0"
D_15e1:
	.ascii	"left\0"
D_15e6:
	.ascii	"right\0"
D_15ec:
	.ascii	"delim\0"
D_15f2:
	.ascii	"define\0"
D_15f9:
	.ascii	"tdefine\0"
D_1601:
	.ascii	"ndefine\0"
D_1609:
	.ascii	"gsize\0"
D_160f:
	.ascii	".gsize\0"
D_1616:
	.ascii	"gfont\0"
D_161c:
	.ascii	"include\0"
D_1624:
	.byte	0x75, 0x70, 0x00	/* |up.| */
D_1627:
	.ascii	"down\0"
D_162c:
	.byte	0x66, 0x77, 0x64, 0x00	/* |fwd.| */
D_1630:
	.ascii	"back\0"
D_1635:
	.ascii	"mark\0"
D_163a:
	.byte	0x6c, 0x69, 0x6e, 0x65	/* |line| */
D_163e:
	.byte	0x75, 0x70, 0x00	/* |up.| */
D_1641:
	.ascii	"matrix\0"
D_1648:
	.byte	0x63, 0x6f, 0x6c, 0x00	/* |col.| */
D_164c:
	.ascii	"lcol\0"
D_1651:
	.ascii	"ccol\0"
D_1656:
	.ascii	"rcol\0"
D_165b:
	.byte	0x3e, 0x3d, 0x00	/* |>=.| */
D_165e:
	.ascii	"\(>=\0"
D_1663:
	.byte	0x3c, 0x3d, 0x00	/* |<=.| */
D_1666:
	.ascii	"\(<=\0"
D_166b:
	.byte	0x3d, 0x3d, 0x00	/* |==.| */
D_166e:
	.ascii	"\(==\0"
D_1673:
	.byte	0x21, 0x3d, 0x00	/* |!=.| */
D_1676:
	.ascii	"\(!=\0"
D_167b:
	.byte	0x2b, 0x2d, 0x00	/* |+-.| */
D_167e:
	.ascii	"\(+-\0"
D_1683:
	.byte	0x2d, 0x3e, 0x00	/* |->.| */
D_1686:
	.ascii	"\(->\0"
D_168b:
	.byte	0x3c, 0x2d, 0x00	/* |<-.| */
D_168e:
	.ascii	"\(<-\0"
D_1693:
	.byte	0x69, 0x6e, 0x66, 0x00	/* |inf.| */
D_1697:
	.ascii	"\(if\0"
D_169c:
	.ascii	"infinity\0"
D_16a5:
	.ascii	"\(if\0"
D_16aa:
	.ascii	"partial\0"
D_16b2:
	.ascii	"\(pd\0"
D_16b7:
	.ascii	"half\0"
D_16bc:
	.ascii	"\f1\(12\fP\0"
D_16c7:
	.ascii	"prime\0"
D_16cd:
	.ascii	"\f1\(fm\fP\0"
D_16d8:
	.ascii	"dollar\0"
D_16df:
	.byte	0x5c	/* |\| */
D_16e0:
	.ascii	"f1$\fP\0"
D_16e7:
	.ascii	"nothing\0"
	.byte	0x00	/* |.| */
D_16f0:
	.ascii	"times\0"
D_16f6:
	.ascii	"\(mu\0"
D_16fb:
	.byte	0x64, 0x65, 0x6c, 0x00	/* |del.| */
D_16ff:
	.ascii	"\(gr\0"
D_1704:
	.ascii	"grad\0"
D_1709:
	.ascii	"\(gr\0"
D_170e:
	.word	D_1c3c	/* ptr: 0x3c3c */
	.byte	0x00	/* |.| */
D_1711:
	.byte	0x3c, 0x3c, 0x00	/* |<<.| */
D_1714:
	.word	D_1e3e	/* ptr: 0x3e3e */
	.byte	0x00	/* |.| */
D_1717:
	.byte	0x3e, 0x3e, 0x00	/* |>>.| */
D_171a:
	.ascii	"approx\0"
	.byte	0x7e, 0x08	/* |~.| */
	.ascii	"\d~\u\0"
D_1729:
	.ascii	"cdot\0"
D_172e:
	.ascii	"\v'-.5'.\v'.5'\0"
D_173d:
	.byte	0x2e, 0x2e, 0x2e	/* |...| */
	.byte	0x00	/* |.| */
D_1741:
	.byte	0x2e, 0x2e, 0x2e, 0x00	/* |....| */
D_1745:
	.ascii	",...,\0"
D_174b:
	.ascii	",...,\0"
D_1751:
	.ascii	"alpha\0"
D_1757:
	.ascii	"\(*a\0"
D_175c:
	.ascii	"beta\0"
D_1761:
	.ascii	"\(*b\0"
D_1766:
	.ascii	"gamma\0"
D_176c:
	.ascii	"\(*g\0"
D_1771:
	.ascii	"GAMMA\0"
D_1777:
	.ascii	"\(*G\0"
D_177c:
	.ascii	"delta\0"
D_1782:
	.ascii	"\(*d\0"
D_1787:
	.ascii	"DELTA\0"
D_178d:
	.ascii	"\(*D\0"
D_1792:
	.ascii	"epsilon\0"
D_179a:
	.ascii	"\(*e\0"
D_179f:
	.ascii	"EPSILON\0"
D_17a7:
	.ascii	"\f1E\fP\0"
D_17af:
	.ascii	"omega\0"
D_17b5:
	.ascii	"\(*w\0"
D_17ba:
	.ascii	"OMEGA\0"
D_17c0:
	.ascii	"\(*W\0"
D_17c5:
	.ascii	"lambda\0"
D_17cc:
	.ascii	"\(*l\0"
D_17d1:
	.ascii	"LAMBDA\0"
D_17d8:
	.ascii	"\(*L\0"
D_17dd:
	.byte	0x6d, 0x75, 0x00	/* |mu.| */
D_17e0:
	.ascii	"\(*m\0"
D_17e5:
	.byte	0x6e, 0x75, 0x00	/* |nu.| */
D_17e8:
	.ascii	"\(*n\0"
D_17ed:
	.ascii	"theta\0"
D_17f3:
	.ascii	"\(*h\0"
D_17f8:
	.ascii	"THETA\0"
D_17fe:
	.ascii	"\(*H\0"
D_1803:
	.byte	0x70, 0x68, 0x69, 0x00	/* |phi.| */
D_1807:
	.ascii	"\(*f\0"
D_180c:
	.word	D_2850	/* ptr: 0x4850 */
	.word	D_0049	/* ptr: 0x0049 */
D_1810:
	.ascii	"\(*F\0"
D_1815:
	.byte	0x70, 0x69, 0x00	/* |pi.| */
D_1818:
	.ascii	"\(*p\0"
D_181d:
	.byte	0x50	/* |P| */
	.word	D_0049	/* ptr: 0x0049 */
D_1820:
	.ascii	"\(*P\0"
D_1825:
	.ascii	"sigma\0"
D_182b:
	.ascii	"\(*s\0"
D_1830:
	.ascii	"SIGMA\0"
D_1836:
	.ascii	"\(*S\0"
D_183b:
	.byte	0x78, 0x69, 0x00	/* |xi.| */
D_183e:
	.ascii	"\(*c\0"
D_1843:
	.byte	0x58	/* |X| */
	.word	D_0049	/* ptr: 0x0049 */
D_1846:
	.ascii	"\(*C\0"
D_184b:
	.ascii	"zeta\0"
D_1850:
	.ascii	"\(*z\0"
D_1855:
	.ascii	"iota\0"
D_185a:
	.ascii	"\(*i\0"
D_185f:
	.byte	0x65, 0x74, 0x61, 0x00	/* |eta.| */
D_1863:
	.ascii	"\(*y\0"
D_1868:
	.ascii	"kappa\0"
D_186e:
	.ascii	"\(*k\0"
D_1873:
	.byte	0x72, 0x68, 0x6f, 0x00	/* |rho.| */
D_1877:
	.ascii	"\(*r\0"
D_187c:
	.byte	0x74, 0x61, 0x75, 0x00	/* |tau.| */
D_1880:
	.ascii	"\(*t\0"
D_1885:
	.ascii	"omicron\0"
D_188d:
	.ascii	"\(*o\0"
D_1892:
	.ascii	"upsilon\0"
D_189a:
	.ascii	"\(*u\0"
D_189f:
	.ascii	"UPSILON\0"
D_18a7:
	.ascii	"\(*U\0"
D_18ac:
	.byte	0x70, 0x73, 0x69, 0x00	/* |psi.| */
D_18b0:
	.ascii	"\(*q\0"
D_18b5:
	.byte	0x50	/* |P| */
	.word	D_2953	/* ptr: 0x4953 */
	.byte	0x00	/* |.| */
D_18b9:
	.ascii	"\(*Q\0"
D_18be:
	.byte	0x63, 0x68, 0x69, 0x00	/* |chi.| */
D_18c2:
	.ascii	"\(*x\0"
D_18c7:
	.byte	0x61, 0x6e, 0x64, 0x00	/* |and.| */
D_18cb:
	.ascii	"\f1and\fP\0"
D_18d5:
	.byte	0x66, 0x6f, 0x72, 0x00	/* |for.| */
D_18d9:
	.ascii	"\f1for\fP\0"
D_18e3:
	.byte	0x69, 0x66, 0x00	/* |if.| */
D_18e6:
	.ascii	"\f1if\fP\0"
D_18ef:
	.byte	0x52, 0x65, 0x00	/* |Re.| */
D_18f2:
	.ascii	"\f1Re\fP\0"
D_18fb:
	.byte	0x49, 0x6d, 0x00	/* |Im.| */
D_18fe:
	.ascii	"\f1Im\fP\0"
D_1907:
	.byte	0x73, 0x69, 0x6e, 0x00	/* |sin.| */
D_190b:
	.ascii	"\f1sin\fP\0"
D_1915:
	.byte	0x63, 0x6f, 0x73, 0x00	/* |cos.| */
D_1919:
	.ascii	"\f1cos\fP\0"
D_1923:
	.byte	0x74, 0x61, 0x6e, 0x00	/* |tan.| */
D_1927:
	.ascii	"\f1tan\fP\0"
D_1931:
	.byte	0x61, 0x72, 0x63, 0x00	/* |arc.| */
D_1935:
	.ascii	"\f1arc\fP\0"
D_193f:
	.ascii	"sinh\0"
D_1944:
	.ascii	"\f1sinh\fP\0"
D_194f:
	.ascii	"coth\0"
D_1954:
	.ascii	"\f1coth\fP\0"
D_195f:
	.ascii	"tanh\0"
D_1964:
	.ascii	"\f1tanh\fP\0"
D_196f:
	.ascii	"cosh\0"
D_1974:
	.ascii	"\f1cosh\fP\0"
D_197f:
	.byte	0x6c, 0x69, 0x6d, 0x00	/* |lim.| */
D_1983:
	.ascii	"\f1lim\fP\0"
D_198d:
	.byte	0x6c, 0x6f, 0x67, 0x00	/* |log.| */
D_1991:
	.ascii	"\f1log\fP\0"
D_199b:
	.byte	0x6d, 0x61, 0x78, 0x00	/* |max.| */
D_199f:
	.ascii	"\f1max\fP\0"
D_19a9:
	.byte	0x6d, 0x69, 0x6e, 0x00	/* |min.| */
D_19ad:
	.ascii	"\f1min\fP\0"
D_19b7:
	.byte	0x6c, 0x6e, 0x00	/* |ln.| */
D_19ba:
	.ascii	"\f1ln\fP\0"
D_19c3:
	.byte	0x65, 0x78, 0x70, 0x00	/* |exp.| */
D_19c7:
	.ascii	"\f1exp\fP\0"
D_19d1:
	.byte	0x64, 0x65, 0x74, 0x00	/* |det.| */
D_19d5:
	.ascii	"\f1det\fP\0"
D_19df:
	.ascii	"out of space in lookup\0"
D_19f6:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x6b, 0x28, 0x39, 0x37, 0x5c, 0x2a, 0x28, 0x25	/* |k(97\*(%| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_1a09:
	.byte	0x2e, 0x09, 0x6d, 0x61, 0x72, 0x6b, 0x20, 0x25	/* |..mark %| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_1a14:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x68, 0x27, 0x7c, 0x5c, 0x6e, 0x28, 0x39, 0x37	/* |h'|\n(97| */
	.byte	0x75, 0x27, 0x0a, 0x00	/* |u'..| */
D_1a28:
	.byte	0x2e, 0x09, 0x6c, 0x69, 0x6e, 0x65, 0x75, 0x70	/* |..lineup| */
	.byte	0x20, 0x25, 0x64, 0x0a, 0x00, 0x00	/* | %d...| */
D_1a36:
	.byte	0x2e, 0x09	/* |..| */
	.ascii	"%d column of\0"
D_1a45:
	.ascii	" S%d\0"
D_1a4a:
	.byte	0x2c, 0x20, 0x72, 0x6f, 0x77, 0x73, 0x3d, 0x25	/* |, rows=%| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_1a55:
	.ascii	"\ \ \0"
D_1a5a:
	.byte	0x2e, 0x09, 0x63, 0x6f, 0x6c, 0x63, 0x74, 0x3d	/* |..colct=| */
	.byte	0x25, 0x64, 0x0a, 0x00	/* |%d..| */
D_1a66:
	.byte	0x2e, 0x09, 0x72, 0x6f, 0x77, 0x20, 0x25, 0x64	/* |..row %d| */
	.byte	0x3a, 0x20, 0x62, 0x3d, 0x25, 0x64, 0x2c, 0x20	/* |: b=%d, | */
	.byte	0x68, 0x62, 0x3d, 0x25, 0x64, 0x0a, 0x00	/* |hb=%d..| */
D_1a7d:
	.byte	0x2e, 0x09, 0x6d, 0x61, 0x74, 0x72, 0x69, 0x78	/* |..matrix| */
	.byte	0x20, 0x53, 0x25, 0x64, 0x3a, 0x20, 0x72, 0x3d	/* | S%d: r=| */
	.byte	0x25, 0x64, 0x2c, 0x20, 0x63, 0x3d, 0x25, 0x64	/* |%d, c=%d| */
	.byte	0x2c, 0x20, 0x68, 0x3d, 0x25, 0x64, 0x2c, 0x20	/* |, h=%d, | */
	.byte	0x62, 0x3d, 0x25, 0x64, 0x0a, 0x00	/* |b=%d..| */
D_1aa3:
	.ascii	".ds %d \"\0"
D_1aac:
	.ascii	"\*(%d%s\0"
D_1ab4:
	.byte	0x00	/* |.| */
D_1ab5:
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_1ab8:
	.ascii	".ds %d \0"
D_1ac0:
	.byte	0x5c, 0x68, 0x27, 0x25, 0x73, 0x25, 0x64, 0x75	/* |\h'%s%du| */
	.byte	0x27, 0x5c, 0x2a, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |'\*(%d..| */
D_1ad0:
	.byte	0x2d, 0x00	/* |-.| */
D_1ad2:
	.byte	0x00	/* |.| */
D_1ad3:
	.byte	0x5c, 0x76, 0x27, 0x2d, 0x25, 0x64, 0x75, 0x27	/* |\v'-%du'| */
	.byte	0x5c, 0x2a, 0x28, 0x25, 0x64, 0x5c, 0x76, 0x27	/* |\*(%d\v'| */
	.byte	0x25, 0x64, 0x75, 0x27, 0x0a, 0x00	/* |%du'..| */
D_1ae9:
	.byte	0x5c, 0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\v'%du'\| */
	.byte	0x2a, 0x28, 0x25, 0x64, 0x5c, 0x76, 0x27, 0x2d	/* |*(%d\v'-| */
	.byte	0x25, 0x64, 0x75, 0x27, 0x0a, 0x00	/* |%du'..| */
D_1aff:
	.byte	0x2e, 0x09, 0x6d, 0x6f, 0x76, 0x65, 0x20, 0x25	/* |..move %| */
	.byte	0x64, 0x20, 0x64, 0x69, 0x72, 0x20, 0x25, 0x64	/* |d dir %d| */
	.byte	0x20, 0x61, 0x6d, 0x74, 0x20, 0x25, 0x64, 0x3b	/* | amt %d;| */
	.byte	0x20, 0x68, 0x3d, 0x25, 0x64, 0x20, 0x62, 0x3d	/* | h=%d b=| */
	.byte	0x25, 0x64, 0x0a, 0x00, 0x00	/* |%d...| */
D_1b24:
	.byte	0x2e, 0x09, 0x62, 0x3a, 0x62, 0x6f, 0x62, 0x3a	/* |..b:bob:| */
	.byte	0x20, 0x53, 0x25, 0x64, 0x20, 0x3c, 0x2d, 0x20	/* | S%d <- | */
	.byte	0x53, 0x25, 0x64, 0x20, 0x6f, 0x76, 0x65, 0x72	/* |S%d over| */
	.byte	0x20, 0x53, 0x25, 0x64, 0x3b, 0x20, 0x62, 0x3d	/* | S%d; b=| */
	.byte	0x25, 0x64, 0x2c, 0x20, 0x68, 0x3d, 0x25, 0x64	/* |%d, h=%d| */
	.byte	0x0a, 0x00	/* |..| */
D_1b4e:
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x6e, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |n(%d..| */
D_1b5c:
	.byte	0x2e, 0x69, 0x66, 0x20, 0x5c, 0x6e, 0x28, 0x25	/* |.if \n(%| */
	.byte	0x64, 0x3e, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x20	/* |d>\n(%d | */
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x6e, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |n(%d..| */
D_1b7a:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c, 0x68	/* |v'%du'\h| */
	.byte	0x27, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75, 0x2d	/* |'\n(%du-| */
	.byte	0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75, 0x2f, 0x32	/* |\n(%du/2| */
	.byte	0x75, 0x27, 0x5c, 0x2a, 0x28, 0x25, 0x64, 0x5c	/* |u'\*(%d\| */
	.byte	0x0a, 0x00	/* |..| */
D_1ba4:
	.byte	0x5c, 0x68, 0x27, 0x2d, 0x5c, 0x6e, 0x28, 0x25	/* |\h'-\n(%| */
	.byte	0x64, 0x75, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64	/* |du-\n(%d| */
	.byte	0x75, 0x2f, 0x32, 0x75, 0x27, 0x5c, 0x76, 0x27	/* |u/2u'\v'| */
	.byte	0x25, 0x64, 0x75, 0x27, 0x5c, 0x2a, 0x28, 0x25	/* |%du'\*(%| */
	.byte	0x64, 0x5c, 0x0a, 0x00	/* |d\..| */
D_1bc8:
	.byte	0x5c, 0x68, 0x27, 0x2d, 0x5c, 0x6e, 0x28, 0x25	/* |\h'-\n(%| */
	.byte	0x64, 0x75, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64	/* |du-\n(%d| */
	.byte	0x75, 0x2d, 0x32, 0x75, 0x2f, 0x32, 0x75, 0x27	/* |u-2u/2u'| */
	.byte	0x5c, 0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\v'%du'\| */
	.byte	0x6c, 0x27, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75	/* |l'\n(%du| */
	.byte	0x27, 0x5c, 0x76, 0x27, 0x25, 0x64, 0x75, 0x27	/* |'\v'%du'| */
	.byte	0x0a, 0x00	/* |..| */
D_1bfa:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64	/* |.ds %d| */
D_1c00:
	.ascii	" \|\v'%du'\0"
D_1c0b:
	.ascii	"\(lf\0"
D_1c10:
	.ascii	"\(bv\0"
D_1c15:
	.ascii	"\(bv\0"
D_1c1a:
	.ascii	"\(lf\0"
D_1c1f:
	.ascii	"\(lc\0"
D_1c24:
	.ascii	"\(lc\0"
D_1c29:
	.ascii	"\(bv\0"
D_1c2e:
	.ascii	"\(bv\0"
D_1c33:
	.ascii	"\b'\(lt\0"
D_1c3b:
	.byte	0x5c	/* |\| */
D_1c3c:
	.byte	0x28, 0x62, 0x76, 0x00	/* |(bv.| */
D_1c40:
	.ascii	"\(lk\0"
D_1c45:
	.ascii	"\(bv\0"
D_1c4a:
	.ascii	"\(lb'\0"
D_1c50:
	.ascii	"\(lt\0"
D_1c55:
	.ascii	"\(bv\0"
D_1c5a:
	.ascii	"\(lb\0"
D_1c5f:
	.ascii	"\(lc\0"
D_1c64:
	.ascii	"\(bv\0"
D_1c69:
	.ascii	"\(lf\0"
D_1c6e:
	.byte	0x7c, 0x00	/* ||.| */
D_1c70:
	.byte	0x7c, 0x00	/* ||.| */
D_1c72:
	.byte	0x7c, 0x00	/* ||.| */
D_1c74:
	.ascii	"\v'%du'\*(%d\0"
D_1c81:
	.ascii	"\|\v'%du'\0"
D_1c8b:
	.ascii	"\(rf\0"
D_1c90:
	.ascii	"\(bv\0"
D_1c95:
	.ascii	"\(bv\0"
D_1c9a:
	.ascii	"\(rf\0"
D_1c9f:
	.ascii	"\(rc\0"
D_1ca4:
	.ascii	"\(rc\0"
D_1ca9:
	.ascii	"\(bv\0"
D_1cae:
	.ascii	"\(bv\0"
D_1cb3:
	.ascii	"\b'\(rt\0"
D_1cbb:
	.ascii	"\(bv\0"
D_1cc0:
	.ascii	"\(rk\0"
D_1cc5:
	.ascii	"\(bv\0"
D_1cca:
	.ascii	"\(rb'\0"
D_1cd0:
	.ascii	"\(rc\0"
D_1cd5:
	.ascii	"\(bv\0"
D_1cda:
	.ascii	"\(rf\0"
D_1cdf:
	.ascii	"\(rt\0"
D_1ce4:
	.ascii	"\(bv\0"
D_1ce9:
	.ascii	"\(rb\0"
D_1cee:
	.byte	0x7c, 0x00	/* ||.| */
D_1cf0:
	.byte	0x7c, 0x00	/* ||.| */
D_1cf2:
	.byte	0x7c, 0x00	/* ||.| */
D_1cf4:
	.ascii	"\v'%du'\0"
D_1cfc:
	.byte	0x0a, 0x00	/* |..| */
D_1cfe:
	.byte	0x2e, 0x09, 0x63, 0x75, 0x72, 0x6c, 0x79, 0x3a	/* |..curly:| */
	.byte	0x20, 0x68, 0x3d, 0x25, 0x64, 0x20, 0x62, 0x3d	/* | h=%d b=| */
	.byte	0x25, 0x64, 0x20, 0x6e, 0x3d, 0x25, 0x64, 0x20	/* |%d n=%d | */
	.byte	0x76, 0x3d, 0x25, 0x64, 0x20, 0x6c, 0x3d, 0x25	/* |v=%d l=%| */
	.byte	0x63, 0x2c, 0x20, 0x72, 0x3d, 0x25, 0x63, 0x0a	/* |c, r=%c.| */
	.byte	0x00	/* |.| */
D_1d27:
	.ascii	"\b'%s\0"
D_1d2d:
	.byte	0x25, 0x73, 0x00	/* |%s.| */
D_1d30:
	.byte	0x25, 0x73, 0x27, 0x00	/* |%s'.| */
D_1d34:
	.byte	0x2e, 0x09	/* |..| */
	.ascii	"S%d <- %c pile of:\0"
D_1d49:
	.ascii	" S%d\0"
D_1d4e:
	.byte	0x3b, 0x68, 0x3d, 0x25, 0x64, 0x20, 0x62, 0x3d	/* |;h=%d b=| */
	.byte	0x25, 0x64, 0x0a, 0x00	/* |%d..| */
D_1d5a:
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x6e, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |n(%d..| */
D_1d68:
	.byte	0x2e, 0x69, 0x66, 0x20, 0x5c, 0x6e, 0x28, 0x25	/* |.if \n(%| */
	.byte	0x64, 0x3e, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x20	/* |d>\n(%d | */
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x6e, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |n(%d..| */
D_1d86:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c, 0x68	/* |v'%du'\h| */
	.byte	0x27, 0x25, 0x64, 0x75, 0x2a, 0x5c, 0x6e, 0x28	/* |'%du*\n(| */
	.byte	0x25, 0x64, 0x75, 0x27, 0x5c, 0x0a, 0x00	/* |%du'\..| */
D_1da5:
	.byte	0x5c, 0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\v'%du'\| */
	.byte	0x2a, 0x28, 0x25, 0x64, 0x5c, 0x68, 0x27, 0x2d	/* |*(%d\h'-| */
	.byte	0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\n(%du'\| */
	.byte	0x76, 0x27, 0x30, 0x2d, 0x25, 0x64, 0x75, 0x27	/* |v'0-%du'| */
	.byte	0x5c, 0x0a, 0x00	/* |\..| */
D_1dc8:
	.byte	0x5c, 0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\v'%du'\| */
	.byte	0x68, 0x27, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64	/* |h'-\n(%d| */
	.byte	0x75, 0x27, 0x5c, 0x2a, 0x28, 0x25, 0x64, 0x5c	/* |u'\*(%d\| */
	.byte	0x76, 0x27, 0x30, 0x2d, 0x25, 0x64, 0x75, 0x27	/* |v'0-%du'| */
	.byte	0x5c, 0x0a, 0x00	/* |\..| */
D_1deb:
	.byte	0x5c, 0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\v'%du'\| */
	.byte	0x68, 0x27, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75	/* |h'\n(%du| */
	.byte	0x2d, 0x5c, 0x6e, 0x28, 0x25	/* |-\n(%| */
D_1e00:
	.ascii	"du/2u'\*(%d\0"
D_1e0c:
	.byte	0x5c, 0x68, 0x27, 0x2d, 0x5c, 0x6e, 0x28, 0x25	/* |\h'-\n(%| */
	.byte	0x64, 0x75, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64	/* |du-\n(%d| */
	.byte	0x75, 0x2f, 0x32, 0x75, 0x27, 0x5c, 0x76, 0x27	/* |u/2u'\v'| */
	.byte	0x30, 0x2d, 0x25, 0x64, 0x75, 0x27, 0x5c, 0x0a	/* |0-%du'\.| */
	.byte	0x00	/* |.| */
D_1e2d:
	.byte	0x5c, 0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\v'%du'\| */
	.byte	0x68, 0x27, 0x25, 0x64, 0x75, 0x2a, 0x5c, 0x6e	/* |h'%du*\n| */
	.byte	0x28	/* |(| */
D_1e3e:
	.byte	0x25, 0x64, 0x75, 0x27, 0x0a, 0x00	/* |%du'..| */
D_1e44:
	.byte	0x2e, 0x09, 0x62, 0x3a, 0x62, 0x20, 0x73, 0x68	/* |..b:b sh| */
	.byte	0x69, 0x66, 0x74, 0x20, 0x62, 0x3a, 0x20, 0x53	/* |ift b: S| */
	.byte	0x25, 0x64, 0x20, 0x3c, 0x2d, 0x20, 0x53, 0x25	/* |%d <- S%| */
	.byte	0x64, 0x20, 0x76, 0x65, 0x72, 0x74, 0x20, 0x25	/* |d vert %| */
	.byte	0x64, 0x20, 0x53, 0x25, 0x64, 0x20, 0x76, 0x65	/* |d S%d ve| */
	.byte	0x72, 0x74, 0x20, 0x25, 0x64, 0x3b, 0x20, 0x62	/* |rt %d; b| */
	.byte	0x3d, 0x25, 0x64, 0x2c, 0x20, 0x68, 0x3d, 0x25	/* |=%d, h=%| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_1e7f:
	.byte	0x2e, 0x61, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.as %d \| */
	.byte	0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c, 0x2a	/* |v'%du'\*| */
	.byte	0x28, 0x25, 0x64, 0x5c, 0x76, 0x27, 0x25, 0x64	/* |(%d\v'%d| */
	.byte	0x75, 0x27, 0x0a, 0x00	/* |u'..| */
D_1e9b:
	.byte	0x2e, 0x09, 0x73, 0x68, 0x69, 0x66, 0x74, 0x3a	/* |..shift:| */
	.byte	0x20, 0x25, 0x64, 0x3b, 0x70, 0x73, 0x3d, 0x25	/* | %d;ps=%| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_1eae:
	.byte	0x2e, 0x09, 0x73, 0x68, 0x69, 0x66, 0x74, 0x32	/* |..shift2| */
	.byte	0x20, 0x73, 0x25, 0x64, 0x20, 0x3c, 0x2d, 0x20	/* | s%d <- | */
	.byte	0x25, 0x64, 0x20, 0x25, 0x64, 0x20, 0x25, 0x64	/* |%d %d %d| */
	.byte	0x0a, 0x00	/* |..| */
D_1ec8:
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x6e, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |n(%d..| */
D_1ed6:
	.byte	0x2e, 0x69, 0x66, 0x20, 0x5c, 0x6e, 0x28, 0x25	/* |.if \n(%| */
	.byte	0x64, 0x3e, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x20	/* |d>\n(%d | */
	.byte	0x2e, 0x6e, 0x72, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.nr %d \| */
	.byte	0x6e, 0x28, 0x25, 0x64, 0x0a, 0x00	/* |n(%d..| */
D_1ef4:
	.byte	0x2e, 0x61, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.as %d \| */
	.byte	0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c, 0x2a	/* |v'%du'\*| */
	.byte	0x28, 0x25, 0x64, 0x5c, 0x68, 0x27, 0x2d, 0x5c	/* |(%d\h'-\| */
	.byte	0x6e, 0x28, 0x25, 0x64, 0x75, 0x27, 0x5c, 0x76	/* |n(%du'\v| */
	.byte	0x27, 0x25, 0x64, 0x75, 0x27, 0x5c, 0x0a, 0x00	/* |'%du'\..| */
D_1f1c:
	.byte	0x5c, 0x2a, 0x28, 0x25, 0x64, 0x5c, 0x68, 0x27	/* |\*(%d\h'| */
	.byte	0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75, 0x2b	/* |-\n(%du+| */
	.byte	0x5c, 0x6e, 0x28, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\n(%du'\| */
	.byte	0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x0a, 0x00	/* |v'%du'..| */
D_1f3c:
	.byte	0x2e, 0x09, 0x73, 0x65, 0x74, 0x73, 0x69, 0x7a	/* |..setsiz| */
	.byte	0x65, 0x20, 0x25, 0x73, 0x3b, 0x20, 0x70, 0x73	/* |e %s; ps| */
	.byte	0x20, 0x3d, 0x20, 0x25, 0x64, 0x0a, 0x00	/* | = %d..| */
D_1f53:
	.byte	0x2e, 0x09, 0x62, 0x3a, 0x73, 0x62, 0x3a, 0x20	/* |..b:sb: | */
	.byte	0x53, 0x25, 0x64, 0x20, 0x3c, 0x2d, 0x20, 0x5c	/* |S%d <- \| */
	.byte	0x73, 0x25, 0x64, 0x20, 0x53, 0x25, 0x64, 0x20	/* |s%d S%d | */
	.byte	0x5c, 0x73, 0x25, 0x64, 0x3b, 0x20, 0x62, 0x3d	/* |\s%d; b=| */
	.byte	0x25, 0x64, 0x2c, 0x20, 0x68, 0x3d, 0x25, 0x64	/* |%d, h=%d| */
	.byte	0x0a, 0x00	/* |..| */
D_1f7d:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x5c	/* |.ds %d \| */
	.byte	0x73, 0x25, 0x64, 0x5c, 0x2a, 0x28, 0x25, 0x64	/* |s%d\*(%d| */
	.byte	0x5c, 0x73, 0x25, 0x64, 0x0a, 0x00, 0x00	/* |\s%d...| */
D_1f94:
	.ascii	".ds %d \v'%du'\e\L'%du'\l'\n(%du'\0"
D_1fb6:
	.byte	0x5c, 0x76, 0x27, 0x25, 0x64, 0x75, 0x27, 0x5c	/* |\v'%du'\| */
	.byte	0x68, 0x27, 0x2d, 0x5c, 0x6e, 0x28, 0x25, 0x64	/* |h'-\n(%d| */
	.byte	0x75, 0x27, 0x5c, 0x2a, 0x28, 0x25, 0x64, 0x0a	/* |u'\*(%d.| */
	.byte	0x00	/* |.| */
D_1fcf:
	.byte	0x2e, 0x09, 0x73, 0x71, 0x72, 0x74, 0x3a, 0x20	/* |..sqrt: | */
	.byte	0x53, 0x25, 0x64, 0x20, 0x3c, 0x2d, 0x20, 0x53	/* |S%d <- S| */
	.byte	0x25, 0x64, 0x3b, 0x62, 0x3d, 0x25, 0x64, 0x2c	/* |%d;b=%d,| */
	.byte	0x20, 0x68, 0x3d, 0x25, 0x64, 0x0a, 0x00, 0xe8	/* | h=%d...| */
	.byte	0x34, 0xcb, 0x33	/* |4.3| */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.byte	0xcb, 0x33	/* |.3| */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_15be	/* ptr: 0x35be */
	.byte	0xcb, 0x33, 0xcb, 0x33	/* |.3.3| */
	.word	D_16e0	/* ptr: 0x36e0 */
	.byte	0x3a, 0x34, 0xcb, 0x33, 0x92, 0x34, 0xde, 0x33	/* |:4.3.4.3| */
	.byte	0xce, 0x34, 0xcb, 0x33, 0xcb, 0x33, 0xcb, 0x33	/* |.4.3.3.3| */
	.byte	0xcb, 0x33, 0xcb, 0x33, 0xcb, 0x33, 0xcb, 0x33	/* |.3.3.3.3| */
	.byte	0xcb, 0x33, 0xcb, 0x33, 0xcb, 0x33, 0xcb, 0x33	/* |.3.3.3.3| */
	.byte	0xcb, 0x33, 0x54, 0x34, 0x20, 0x34, 0x54, 0x34	/* |.3T4 4T4| */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.byte	0xcb, 0x33, 0xf6, 0x34, 0xcb, 0x33, 0xf0, 0x34	/* |.3.4.3.4| */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_163e	/* ptr: 0x363e */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_169c	/* ptr: 0x369c */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.word	D_16e0	/* ptr: 0x36e0 */
	.byte	0x07, 0x34	/* |.4| */
	.word	D_16e0	/* ptr: 0x36e0 */
	.byte	0xe8, 0x34	/* |.4| */
D_20ac:
	.byte	0x5c, 0x20, 0x00	/* |\ .| */
D_20af:
	.byte	0x5c, 0x7c, 0x00	/* |\|.| */
D_20b2:
	.byte	0x5c, 0x74, 0x00	/* |\t.| */
D_20b5:
	.ascii	"converted token %.25s... too long\0"
D_20d7:
	.byte	0x2e, 0x09, 0x25, 0x64, 0x74, 0x65, 0x78, 0x74	/* |..%dtext| */
	.byte	0x3a, 0x20, 0x53, 0x25, 0x64, 0x20, 0x3c, 0x2d	/* |: S%d <-| */
	.byte	0x20, 0x25, 0x73, 0x3b, 0x20, 0x62, 0x3d, 0x25	/* | %s; b=%| */
	.byte	0x64, 0x2c, 0x68, 0x3d, 0x25, 0x64, 0x2c, 0x6c	/* |d,h=%d,l| */
	.byte	0x66, 0x3d, 0x25, 0x63, 0x2c, 0x72, 0x66, 0x3d	/* |f=%c,rf=| */
	.byte	0x25, 0x63, 0x0a, 0x00	/* |%c..| */
D_2103:
	.byte	0x2e, 0x64, 0x73, 0x20, 0x25, 0x64, 0x20, 0x22	/* |.ds %d "| */
	.byte	0x25, 0x73, 0x0a, 0x00, 0x00	/* |%s...| */
D_2110:
	.byte	0x00, 0x00	/* |..| */
D_2112:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_2126:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_212e:
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
D_217d:
	.byte	0x00	/* |.| */
D_217e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_2186:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_218e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_2196:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_219e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_21a6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_21ae:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_21b6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_21be:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_21c6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_21ce:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_21d6:
	.word	D_3780	/* ptr: 0x5780 */
	.byte	0x00, 0x00	/* |..| */
	.word	D_3780	/* ptr: 0x5780 */
	.byte	0x01, 0x00	/* |..| */
D_21de:
	.byte	0x00, 0x00	/* |..| */
D_21e0:
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_21e4:
	.byte	0x02	/* |.| */
D_21e5:
	.byte	0x01	/* |.| */
D_21e6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
D_21ee:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_2276:
	.word	D_2276	/* ptr: 0x4276 */
D_2278:
	.byte	0x00, 0x00	/* |..| */
D_227a:
	.byte	0x00, 0x00	/* |..| */
D_227c:
	.byte	0x00, 0x00	/* |..| */
D_227e:
	.word	D_3986	/* ptr: 0x5986 */

	.bss

	.space	2
D_2282:	.space	2	/* bss word @ 0x2282 */
	.space	4
D_2288:	.space	2	/* bss word @ 0x2288 */
	.space	198
D_2350:	.space	2	/* bss word @ 0x2350 */
	.space	198
D_2418:	.space	2	/* bss word @ 0x2418 */
D_241a:	.space	2	/* bss word @ 0x241a */
D_241c:	.space	2	/* bss word @ 0x241c */
D_241e:	.space	2	/* bss word @ 0x241e */
D_2420:	.space	2	/* bss word @ 0x2420 */
D_2422:	.space	2	/* bss word @ 0x2422 */
	.space	2
D_2426:	.space	2	/* bss word @ 0x2426 */
	.space	262
D_252e:	.space	2	/* bss word @ 0x252e */
	.space	40
D_2558:	.space	2	/* bss word @ 0x2558 */
	.space	158
D_25f8:	.space	2	/* bss word @ 0x25f8 */
	.space	198
D_26c0:	.space	2	/* bss word @ 0x26c0 */
	.space	198
D_2788:	.space	2	/* bss word @ 0x2788 */
D_278a:	.space	2	/* bss word @ 0x278a */
D_278c:	.space	2	/* bss word @ 0x278c */
	.space	2
D_2790:	.space	2	/* bss word @ 0x2790 */
	.space	190
D_2850:	.space	2	/* bss word @ 0x2850 */
	.space	6
D_2858:	.space	2	/* bss word @ 0x2858 */
D_285a:	.space	2	/* bss word @ 0x285a */
D_285c:	.space	2	/* bss word @ 0x285c */
D_285e:	.space	2	/* bss word @ 0x285e */
D_2860:	.space	2	/* bss word @ 0x2860 */
D_2862:	.space	2	/* bss word @ 0x2862 */
D_2864:	.space	2	/* bss word @ 0x2864 */
	.space	2
D_2868:	.space	2	/* bss word @ 0x2868 */
D_2869:	.space	2	/* bss word @ 0x2869 */
D_286a:	.space	2	/* bss word @ 0x286a */
	.space	231
D_2953:	.space	2	/* bss word @ 0x2953 */
	.space	963
D_2d18:	.space	2	/* bss word @ 0x2d18 */
	.space	6
D_2d20:	.space	2	/* bss word @ 0x2d20 */
	.space	198
D_2de8:	.space	2	/* bss word @ 0x2de8 */
	.space	298
D_2f14:	.space	2	/* bss word @ 0x2f14 */
	.space	402
D_30a8:	.space	2	/* bss word @ 0x30a8 */
D_30a9:	.space	2	/* bss word @ 0x30a9 */
	.space	397
D_3238:	.space	2	/* bss word @ 0x3238 */
	.space	198
D_3300:	.space	2	/* bss word @ 0x3300 */
	.space	198
D_33c8:	.space	2	/* bss word @ 0x33c8 */
	.space	5
D_33cf:	.space	2	/* bss word @ 0x33cf */
D_33d0:	.space	2	/* bss word @ 0x33d0 */
	.space	418
D_3574:	.space	2	/* bss word @ 0x3574 */
D_3576:	.space	2	/* bss word @ 0x3576 */
D_3578:	.space	2	/* bss word @ 0x3578 */
D_357a:	.space	2	/* bss word @ 0x357a */
	.space	4
D_3580:	.space	2	/* bss word @ 0x3580 */
	.space	510
D_3780:	.space	2	/* bss word @ 0x3780 */
	.space	510
D_3980:	.space	2	/* bss word @ 0x3980 */
D_3982:	.space	2	/* bss word @ 0x3982 */
D_3984:	.space	2	/* bss word @ 0x3984 */
D_3986:	/* endcore: initial break */

/* end of file */
