/* ed.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 ed.s -o ed.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* Wrapping near-call/jmp targets (16-bit address space) */
	.set L_a240, 0xa240	/* wrapping branch target */
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: ed  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00002d16  (11542 bytes)
//   a_data:   0x000000de  (222 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    $0x1,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	mov    %ax,D_0ce8
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	mov    %ax,D_0504
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	mov    $0x1,%ax
	push   %ax
	mov    $0xf,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_008f
	mov    $0xeda,%ax
	push   %ax
	mov    $0xf,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
L_008f:
	addw   $0x2,0x6(%bp)
L_0093:
	cmpw   $0x1,0x4(%bp)
	jle   L_00ea
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	cmpb   $0x2d,(%bx)
	jne   L_00ea
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	mov    0x1(%bx),%al
	cbtw
	jmp   L_00d3
L_00ae:
	movw   $0x0,D_000a
	jmp   L_00e1
L_00b6:
	xor    %ax,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	movw   $0x1,D_000a
	jmp   L_00e1
L_00cb:
	movw   $0x1,D_0cee
	jmp   L_00e1
L_00d3:
	or     %ax,%ax
	je   L_00ae
	cmp    $0x71,%ax
	je   L_00b6
	cmp    $0x78,%ax
	je   L_00cb
L_00e1:
	addw   $0x2,0x6(%bp)
	decw   0x4(%bp)
	jmp   L_0093
L_00ea:
	cmpw   $0x0,D_0cee
	je   L_0105
	call   L_2100
	mov    $D_0cf6,%ax
	push   %ax
	mov    $D_0766,%ax
	push   %ax
	call   L_21ac
	add    $0x4,%sp
	mov    %ax,D_00e0
L_0105:
	cmpw   $0x1,0x4(%bp)
	jle   L_012a
	mov    0x6(%bp),%bx
	mov    (%bx),%di
	mov    $D_0716,%si
L_0113:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0113
	/* dual-reloc split: movw $D_0040,D_0770 */
	mov	$D_0040,%ax
	mov	%ax,D_0770
L_012a:
	mov    D_001e,%ax
	shl    %ax
	push   %ax
	call   L_29d4
	pop    %cx
	mov    %ax,D_1406
	mov    $D_0042,%ax
	push   %ax
	call   L_2506
	pop    %cx
	mov    %ax,D_1408
	call   L_1287
	mov    -0x6(%bp),%ax
	test   $0x1,%ax
	jne   L_015b
	mov    $0x97b,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
L_015b:
	mov    D_0504,%ax
	test   $0x1,%ax
	jne   L_0171
	mov    $0x9a8,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
L_0171:
	mov    $D_00e6,%ax
	push   %ax
	call   L_25d4
	pop    %cx
	call   L_0184
	call   L_0eda
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0184:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0189:
	cmpw   $0x0,D_0506
	je   L_01a2
	movw   $0x0,D_0506
	mov    D_00f4,%ax
	mov    %ax,D_0ff8
	mov    %ax,D_0ff6
	jmp   L_030d
L_01a2:
	movw   $0x0,D_0ff6
	movw   $0x0,D_0ff8
L_01ae:
	mov    D_0ff8,%ax
	mov    %ax,D_0ff6
	call   L_05e8
	mov    %ax,%di
	or     %ax,%ax
	jne   L_01c4
	call   L_0aa1
	mov    %ax,%si
	jmp   L_01de
L_01c4:
	mov    %di,D_0ff8
	call   L_0aa1
	mov    %ax,%si
	cmp    $0x3b,%ax
	jne   L_01d9
	mov    $0x2c,%si
	mov    %di,D_00f4
L_01d9:
	cmp    $0x2c,%si
	je   L_01ae
L_01de:
	cmpw   $0x0,D_0ff6
	jne   L_01eb
	mov    D_0ff8,%ax
	mov    %ax,D_0ff6
L_01eb:
	mov    %si,%ax
	jmp   L_04f4
L_01f0:
	call   L_07a9
	call   L_083e
	pushw  D_0ff8
	mov    $0xb14,%ax
	push   %ax
	call   L_0d74
	add    $0x4,%sp
L_0204:
	jmp   L_0189
L_0206:
	call   L_0f17
	mov    D_0ff6,%ax
	dec    %ax
	dec    %ax
	push   %ax
	mov    $0xb14,%ax
	push   %ax
	call   L_0d74
	add    $0x4,%sp
L_0219:
	jmp   L_0204
L_021b:
	call   L_0f17
	jmp   L_0204
L_0220:
	movw   $0x0,D_0cea
	mov    $0x65,%si
L_0229:
	call   L_0803
	cmpw   $0x0,D_000a
	je   L_0248
	cmpw   $0x0,D_0cea
	je   L_0248
	movw   $0x0,D_0cea
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0248:
	push   %si
	call   L_0878
	pop    %cx
	call   L_1287
	mov    D_1406,%ax
	mov    %ax,D_0ff8
	jmp   L_0357
L_0259:
	call   L_0803
	push   %si
	call   L_0878
	pop    %cx
	mov    $D_0716,%ax
	push   %ax
	call   L_1f5e
	pop    %cx
L_0269:
	jmp   L_0219
L_026b:
	mov    $0x1,%ax
	push   %ax
	call   L_1317
	pop    %cx
	jmp   L_0269
L_0275:
	call   L_07a9
	call   L_081b
	call   L_083e
	mov    D_0ff8,%ax
	dec    %ax
	dec    %ax
	push   %ax
	mov    $0xb14,%ax
	push   %ax
	call   L_0d74
	add    $0x4,%sp
	jmp   L_0269
L_0290:
	cmpw   $0x0,D_0ff8
	jne   L_02a5
	mov    D_00f4,%ax
	mov    %ax,D_0ff6
	mov    D_00f4,%ax
	inc    %ax
	inc    %ax
	mov    %ax,D_0ff8
L_02a5:
	call   L_07a9
	call   L_083e
	call   L_081b
	call   L_1445
L_02b1:
	jmp   L_0269
L_02b3:
	call   L_0aa1
	mov    %ax,%si
	cmp    $0x61,%ax
	jl   L_02c2
	cmp    $0x7a,%si
	jle   L_02ca
L_02c2:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_02ca:
	call   L_083e
	call   L_07a9
	call   L_081b
	mov    D_0ff8,%bx
	mov    (%bx),%ax
	and    $0xfffe,%ax
	lea    -0x61(%si),%dx
	mov    %dx,%bx
	shl    %bx
	mov    %ax,D_04ce(%bx)
	orw    $0x1,D_0ce0
	jmp   L_02b1
L_02ef:
	xor    %ax,%ax
	push   %ax
	call   L_17b7
	pop    %cx
L_02f6:
	jmp   L_02b1
L_02f8:
	cmpw   $0x0,D_0ff8
	jne   L_0307
	mov    D_00f4,%ax
	inc    %ax
	inc    %ax
	mov    %ax,D_0ff8
L_0307:
	mov    D_0ff8,%ax
	mov    %ax,D_0ff6
L_030d:
	jmp   L_0316
L_030f:
	incw   D_0502
L_0313:
	call   L_083e
L_0316:
	call   L_07a9
	call   L_081b
	mov    D_0ff6,%di
L_0320:
	mov    %di,%bx
	inc    %di
	inc    %di
	pushw  (%bx)
	call   L_0ff2
	pop    %cx
	push   %ax
	call   L_1f5e
	pop    %cx
	cmp    D_0ff8,%di
	jbe   L_0320
	mov    D_0ff8,%ax
	mov    %ax,D_00f4
	movw   $0x0,D_0502
L_0341:
	jmp   L_02f6
L_0343:
	movw   $0x0,D_0cea
L_0349:
	call   L_0803
	call   L_083e
	call   L_0eda
L_0352:
	push   %si
	call   L_0878
	pop    %cx
L_0357:
	xor    %ax,%ax
	push   %ax
	mov    $D_01fe,%ax
	push   %ax
	call   L_2c3c
	add    $0x4,%sp
	mov    %ax,D_0ce6
	or     %ax,%ax
	jge   L_0379
	movw   $0xa,D_140a
	mov    $D_01fe,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0379:
	call   L_07d3
	movw   $0x0,D_01f6
	mov    D_1406,%ax
	cmp    D_00e2,%ax
	je   L_0390
	mov    $0x1,%ax
	jmp   L_0392
L_0390:
	xor    %ax,%ax
L_0392:
	mov    %ax,%si
	pushw  D_0ff8
	mov    $0xb82,%ax
	push   %ax
	call   L_0d74
	add    $0x4,%sp
	call   L_0952
	mov    %si,D_0cea
L_03a9:
	jmp   L_0341
L_03ab:
	call   L_07a9
	call   L_081b
	cmpw   $0x0,D_0770
	je   L_03bd
	mov    $0x1,%ax
	jmp   L_03bf
L_03bd:
	xor    %ax,%ax
L_03bf:
	push   %ax
	call   L_14d2
	pop    %cx
L_03c4:
	jmp   L_03a9
L_03c6:
	mov    $0x1,%ax
	push   %ax
	call   L_17b7
	pop    %cx
	jmp   L_03a9
L_03d0:
	call   L_07a9
	call   L_081b
	call   L_083e
	mov    D_0ff8,%bx
	mov    (%bx),%ax
	and    $0xfffe,%ax
	cmp    D_0cec,%ax
	je   L_03f0
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_03f0:
	mov    D_0ff8,%bx
	mov    D_0cce,%ax
	mov    %ax,(%bx)
	mov    D_0ff8,%ax
	mov    %ax,D_00f4
	jmp   L_03c4
L_0401:
	xor    %ax,%ax
	push   %ax
	call   L_1317
	pop    %cx
L_0408:
	jmp   L_03c4
L_040a:
	incw   D_1200
L_040e:
	call   L_07d3
	call   L_081b
	push   %si
	call   L_0878
	pop    %cx
	cmpw   $0x0,D_1200
	je   L_0454
	mov    $0x1,%ax
	push   %ax
	mov    $D_01fe,%ax
	push   %ax
	call   L_2c3c
	add    $0x4,%sp
	mov    %ax,D_0ce6
	cmp    $0xffff,%ax
	je   L_0454
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  D_0ce6
	call   L_27f8
	add    $0x8,%sp
	cmp    $0xffff,%dx
	jne   L_0471
	cmp    $0xffff,%ax
	jne   L_0471
L_0454:
	mov    $0x1b6,%ax
	push   %ax
	mov    $D_01fe,%ax
	push   %ax
	call   L_259c
	add    $0x4,%sp
	mov    %ax,D_0ce6
	or     %ax,%ax
	jge   L_0471
	mov    $D_01fe,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0471:
	movw   $0x0,D_1200
	call   L_0c4f
	call   L_0952
	mov    D_1406,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,D_0ff6
L_0486:
	jne   L_0408
	mov    D_0ff8,%ax
	cmp    D_00e2,%ax
	jne   L_0486
	movw   $0x0,D_0cea
L_0497:
	jmp   L_0408
L_049a:
	call   L_0803
	call   L_083e
	movw   $0x1,D_0cee
	mov    $D_004e,%ax
	push   %ax
	call   L_1f5e
	pop    %cx
	call   L_2100
	mov    $D_0cf6,%ax
	push   %ax
	mov    $D_0766,%ax
	push   %ax
	call   L_21ac
	add    $0x4,%sp
	mov    %ax,D_00e0
L_04c2:
	jmp   L_0497
L_04c4:
	call   L_07d3
	call   L_083e
	mov    D_0ff8,%ax
	sub    D_1406,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	and    $0x7fff,%ax
	cwtd
	mov    %ax,D_0ce2
	mov    %dx,D_0ce4
	call   L_1f14
	mov    $0xa,%ax
	push   %ax
	call   L_1f8b
	pop    %cx
	jmp   L_04c2
L_04ef:
	call   L_0e4e
L_04f2:
	jmp   L_04c2
L_04f4:
	cmp    $0x67,%ax
	jne   L_04fc
	jmp   L_026b
L_04fc:
	jg   L_0566
	cmp    $0x51,%ax
	jne   L_0506
	jmp   L_0343
L_0506:
	jg   L_0534
	cmp    $0x21,%ax
	je   L_04ef
	jg   L_051f
	cmp    $0xffff,%ax
	jne   L_0517
	jmp   L_05e4
L_0517:
	cmp    $0xa,%ax
	jne   L_051f
	jmp   L_02f8
L_051f:
	cmp    $0x3d,%ax
	je   L_04c4
	cmp    $0x45,%ax
	jne   L_052c
	jmp   L_0220
L_052c:
	cmp    $0x50,%ax
L_052f:
	jne   L_0534
	jmp   L_0313
L_0534:
	cmp    $0x63,%ax
	jne   L_053c
	jmp   L_0206
L_053c:
	jg   L_054e
	cmp    $0x57,%ax
	jne   L_0546
	jmp   L_040a
L_0546:
	cmp    $0x61,%ax
	jne   L_054e
	jmp   L_01f0
L_054e:
	cmp    $0x64,%ax
	jne   L_0556
	jmp   L_021b
L_0556:
	cmp    $0x65,%ax
	jne   L_055e
	jmp   L_0229
L_055e:
	cmp    $0x66,%ax
	jne   L_0566
	jmp   L_0259
L_0566:
	cmp    $0x71,%ax
	jne   L_056e
	jmp   L_0349
L_056e:
	jg   L_059f
	cmp    $0x6b,%ax
	jne   L_0578
	jmp   L_02b3
L_0578:
	jg   L_058a
	cmp    $0x69,%ax
	jne   L_0582
	jmp   L_0275
L_0582:
	cmp    $0x6a,%ax
	jne   L_058a
	jmp   L_0290
L_058a:
	cmp    $0x6c,%ax
	jne   L_0592
	jmp   L_030f
L_0592:
	cmp    $0x6d,%ax
	jne   L_059a
	jmp   L_02ef
L_059a:
	cmp    $0x70,%ax
	je   L_052f
L_059f:
	cmp    $0x75,%ax
	jne   L_05a7
	jmp   L_03d0
L_05a7:
	jg   L_05c1
	cmp    $0x72,%ax
	jne   L_05b1
	jmp   L_0352
L_05b1:
	cmp    $0x73,%ax
	jne   L_05b9
	jmp   L_03ab
L_05b9:
	cmp    $0x74,%ax
	jne   L_05c1
	jmp   L_03c6
L_05c1:
	cmp    $0x76,%ax
	jne   L_05c9
	jmp   L_0401
L_05c9:
	cmp    $0x77,%ax
	jne   L_05d1
	jmp   L_040e
L_05d1:
	cmp    $0x78,%ax
	jne   L_05d9
	jmp   L_049a
L_05d9:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_04f2
L_05e4:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_05e8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	xor    %si,%si
	mov    %si,%di
L_05f4:
	call   L_0aa1
	mov    %ax,-0x6(%bp)
	cmp    $0x30,%ax
	jl   L_0654
	cmpw   $0x39,-0x6(%bp)
	jg   L_0654
	movw   $0x0,-0x8(%bp)
L_060a:
	mov    -0x8(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,-0x8(%bp)
	mov    -0x6(%bp),%ax
	sub    $0x30,%ax
	add    %ax,-0x8(%bp)
	call   L_0aa1
	mov    %ax,-0x6(%bp)
	cmp    $0x30,%ax
	jl   L_062f
	cmpw   $0x39,-0x6(%bp)
	jle   L_060a
L_062f:
	mov    -0x6(%bp),%ax
	mov    %ax,D_00f2
	or     %di,%di
	jne   L_063d
	mov    D_1406,%di
L_063d:
	or     %si,%si
	jge   L_0649
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
L_0649:
	mov    -0x8(%bp),%ax
	shl    %ax
	add    %ax,%di
	xor    %si,%si
L_0652:
	jmp   L_05f4
L_0654:
	movw   $0x0,-0xa(%bp)
	or     %di,%di
	jne   L_0661
	or     %si,%si
	je   L_0664
L_0661:
	incw   -0xa(%bp)
L_0664:
	mov    -0x6(%bp),%ax
	jmp   L_0744
L_066a:
	inc    %si
	or     %di,%di
	jne   L_0652
	mov    D_00f4,%di
	jmp   L_0652
L_0675:
	dec    %si
	or     %di,%di
	jne   L_0652
	mov    D_00f4,%di
L_067e:
	jmp   L_0652
L_0680:
	pushw  -0x6(%bp)
	call   L_1909
	pop    %cx
	mov    D_00f4,%di
L_068b:
	cmpw   $0x2f,-0x6(%bp)
	jne   L_069f
	inc    %di
	inc    %di
	cmp    D_00e2,%di
	jbe   L_06ab
	mov    D_1406,%di
	jmp   L_06ab
L_069f:
	dec    %di
	dec    %di
	cmp    D_1406,%di
	jae   L_06ab
	mov    D_00e2,%di
L_06ab:
	push   %di
	xor    %ax,%ax
	push   %ax
	call   L_1b94
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_06d3
	cmp    D_00f4,%di
	jne   L_068b
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_068b
L_06c9:
	mov    D_00e2,%di
	jmp   L_06d3
L_06cf:
	mov    D_00f4,%di
L_06d3:
	jmp   L_0791
L_06d6:
	call   L_0aa1
	mov    %ax,-0x6(%bp)
	cmp    $0x61,%ax
	jl   L_06e7
	cmpw   $0x7a,-0x6(%bp)
	jle   L_06ef
L_06e7:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_06ef:
	mov    D_1406,%di
L_06f3:
	cmp    D_00e2,%di
	jbe   L_06fc
	jmp   L_0791
L_06fc:
	mov    -0x6(%bp),%ax
	sub    $0x61,%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_04ce(%bx),%ax
	mov    (%di),%dx
	and    $0xfffe,%dx
	cmp    %dx,%ax
	je   L_0791
	inc    %di
	inc    %di
	jmp   L_06f3
L_0718:
	mov    -0x6(%bp),%ax
	mov    %ax,D_00f2
	or     %di,%di
	jne   L_0726
	xor    %ax,%ax
	jmp   L_0742
L_0726:
	mov    %si,%ax
	shl    %ax
	add    %ax,%di
	cmp    D_1406,%di
	jb   L_0738
	cmp    D_00e2,%di
	jbe   L_0740
L_0738:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0740:
	mov    %di,%ax
L_0742:
	jmp   L_07a2
L_0744:
	cmp    $0x2b,%ax
	jne   L_074c
	jmp   L_066a
L_074c:
	jg   L_076d
	cmp    $0x9,%ax
L_0751:
	jne   L_0756
	jmp   L_067e
L_0756:
	cmp    $0x20,%ax
	je   L_0751
	cmp    $0x24,%ax
	jne   L_0763
	jmp   L_06c9
L_0763:
	cmp    $0x27,%ax
	jne   L_076b
	jmp   L_06d6
L_076b:
	jmp   L_0718
L_076d:
	cmp    $0x2d,%ax
L_0770:
	jne   L_0775
	jmp   L_0675
L_0775:
	cmp    $0x2e,%ax
	jne   L_077d
	jmp   L_06cf
L_077d:
	cmp    $0x2f,%ax
L_0780:
	jne   L_0785
	jmp   L_0680
L_0785:
	cmp    $0x3f,%ax
	je   L_0780
	cmp    $0x5e,%ax
	je   L_0770
	jmp   L_076b
L_0791:
	cmpw   $0x0,-0xa(%bp)
	je   L_0751
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_067e
L_07a2:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_07a9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_0ff8
	jne   L_07be
	mov    D_00f4,%ax
	mov    %ax,D_0ff8
	mov    %ax,D_0ff6
L_07be:
	mov    D_0ff6,%ax
	cmp    D_0ff8,%ax
	jbe   L_07cf
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_07cf:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_07d3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_0ff8
	jne   L_07fc
	mov    D_1406,%ax
	inc    %ax
	inc    %ax
	mov    %ax,D_0ff6
	mov    D_00e2,%ax
	mov    %ax,D_0ff8
	mov    D_00e2,%ax
	cmp    D_1406,%ax
	jne   L_07fc
	mov    D_1406,%ax
	mov    %ax,D_0ff6
L_07fc:
	call   L_07a9
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0803:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_0ff8
	je   L_0817
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0817:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_081b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_0ff6,%ax
	cmp    D_1406,%ax
	jbe   L_0832
	mov    D_0ff8,%ax
	cmp    D_00e2,%ax
	jbe   L_083a
L_0832:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_083a:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_083e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_0aa1
	mov    %ax,%di
	cmp    $0xa,%ax
	je   L_0874
	cmp    $0x70,%di
	je   L_0857
	cmp    $0x6c,%di
	jne   L_086c
L_0857:
	incw   D_0506
	cmp    $0x6c,%di
	jne   L_0864
	incw   D_0502
L_0864:
	call   L_0aa1
	cmp    $0xa,%ax
	je   L_0874
L_086c:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0874:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0878:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	movw   $0x0,D_0ce2
	movw   $0x0,D_0ce4
	call   L_0aa1
	mov    %ax,-0x6(%bp)
	cmp    $0xa,%ax
	je   L_089b
	cmpw   $0xffff,-0x6(%bp)
	jne   L_08ca
L_089b:
	mov    $D_0716,%di
	cmpb   $0x0,(%di)
	jne   L_08b1
	cmpw   $0x66,0x4(%bp)
	je   L_08b1
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_08b1:
	mov    $D_01fe,%si
L_08b4:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_08c8
	jmp   L_094d
L_08c8:
	jmp   L_08b4
L_08ca:
	cmpw   $0x20,-0x6(%bp)
	je   L_08d8
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_08d8:
	call   L_0aa1
	mov    %ax,-0x6(%bp)
	cmp    $0x20,%ax
	je   L_08d8
	cmpw   $0xa,-0x6(%bp)
	jne   L_08f1
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_08f1:
	mov    $D_01fe,%di
L_08f4:
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	cmpw   $0x20,-0x6(%bp)
	je   L_090a
	cmpw   $0xffff,-0x6(%bp)
	jne   L_0912
L_090a:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0912:
	call   L_0aa1
	mov    %ax,-0x6(%bp)
	cmp    $0xa,%ax
	jne   L_08f4
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	cmpb   $0x0,D_0716
	je   L_0936
	cmpw   $0x65,0x4(%bp)
	je   L_0936
	cmpw   $0x66,0x4(%bp)
	jne   L_094d
L_0936:
	mov    $D_0716,%di
	mov    $D_01fe,%si
L_093c:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_093c
L_094d:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0952:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  D_0ce6
	call   L_2c28
	pop    %cx
	/* movw constant -> D_0ce6 */
	mov	$0xffff,%ax
	mov	%ax,D_0ce6
	cmpw   $0x0,D_000a
	je   L_0977
	call   L_1f14
	mov    $0xa,%ax
	push   %ax
	call   L_1f8b
	pop    %cx
L_0977:
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x97b,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	mov    $0xa,%ax
	push   %ax
	call   L_1f8b
	pop    %cx
	movw   $0xa,D_140a
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
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
	mov    $0x2,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	mov    D_00e2,%ax
	cmp    D_1406,%ax
	jbe   L_09f9
	mov    D_1406,%ax
	inc    %ax
	inc    %ax
	mov    %ax,D_0ff6
	mov    D_00e2,%ax
	mov    %ax,D_0ff8
	mov    $0x1b6,%ax
	push   %ax
	mov    $D_0068,%ax
	push   %ax
	call   L_259c
	add    $0x4,%sp
	mov    %ax,D_0ce6
	cmp    $0x0,%ax
	jle   L_09f9
	call   L_0c4f
L_09f9:
	movw   $0x0,D_0cea
	call   L_0eda
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a06:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	movw   $0x0,D_1200
	movw   $0x0,D_0502
	mov    $0x3f,%ax
	push   %ax
	call   L_1f8b
	pop    %cx
	pushw  0x4(%bp)
	call   L_1f5e
	pop    %cx
	movw   $0x0,D_0ce2
	movw   $0x0,D_0ce4
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_27f8
	add    $0x8,%sp
	movw   $0x0,D_0506
	cmpw   $0x0,D_0770
	je   L_0a58
	movw   $0xa,D_140a
L_0a58:
	movw   $0x0,D_0770
	mov    D_140a,%ax
	mov    %ax,D_00f2
	cmpw   $0x0,D_140a
	je   L_0a7a
L_0a6b:
	call   L_0aa1
	mov    %ax,%di
	cmp    $0xa,%ax
	je   L_0a7a
	cmp    $0xffff,%di
	jne   L_0a6b
L_0a7a:
	cmpw   $0x0,D_0ce6
	jle   L_0a8f
	pushw  D_0ce6
	call   L_2c28
	pop    %cx
	/* movw constant -> D_0ce6 */
	mov	$0xffff,%ax
	mov	%ax,D_0ce6
L_0a8f:
	mov    $0x1,%ax
	push   %ax
	mov    $D_00e6,%ax
	push   %ax
	call   L_2602
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0aa1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    D_00f2,%ax
	mov    %ax,D_140a
	or     %ax,%ax
	je   L_0abc
	movw   $0x0,D_00f2
	mov    D_140a,%ax
	jmp   L_0b03
L_0abc:
	cmpw   $0x0,D_0770
	je   L_0ae5
	mov    D_0770,%bx
	incw   D_0770
	mov    (%bx),%al
	cbtw
	mov    %ax,D_140a
	or     %ax,%ax
	je   L_0ada
	mov    D_140a,%ax
	jmp   L_0b0f
L_0ada:
	movw   $0x0,D_0770
	mov    $0xffff,%ax
	jmp   L_0b0f
L_0ae5:
	mov    $0x1,%ax
	push   %ax
	lea    -0x5(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_2c52
	add    $0x6,%sp
	or     %ax,%ax
	jg   L_0b05
	/* movw constant -> D_140a */
	mov	$0xffff,%ax
	mov	%ax,D_140a
	mov    $0xffff,%ax
L_0b03:
	jmp   L_0b0f
L_0b05:
	mov    -0x5(%bp),%al
	cbtw
	and    $0x7f,%ax
	mov    %ax,D_140a
L_0b0f:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	movw   $D_0ffe,-0x6(%bp)
	mov    D_0770,%si
L_0b23:
	call   L_0aa1
	mov    %ax,%di
	cmp    $0xa,%ax
	je   L_0b5f
	cmp    $0xffff,%di
	jne   L_0b3e
	or     %si,%si
	je   L_0b3a
	mov    %di,D_00f2
L_0b3a:
	mov    %di,%ax
	jmp   L_0b7d
L_0b3e:
	and    $0x7f,%di
	je   L_0b23
	mov    -0x6(%bp),%bx
	mov    %di,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	cmpw   $D_11fc,-0x6(%bp)
	jb   L_0b23
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_0b23
L_0b5f:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x6(%bp)
	cmpb   $0x2e,D_0ffe
	jne   L_0b7b
	cmpb   $0x0,D_0fff
	jne   L_0b7b
	mov    $0xffff,%ax
	jmp   L_0b7d
L_0b7b:
	xor    %ax,%ax
L_0b7d:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    $D_0ffe,%si
	mov    D_0976,%ax
	mov    %ax,-0x6(%bp)
L_0b91:
	decw   D_01f6
	jge   L_0bfc
	mov    $0x200,%ax
	push   %ax
	mov    $D_0776,%ax
	push   %ax
	pushw  D_0ce6
	call   L_2c52
	add    $0x6,%sp
	dec    %ax
	mov    %ax,D_01f6
	or     %ax,%ax
	jge   L_0bb7
	mov    $0xffff,%ax
	jmp   L_0c4a
L_0bb7:
	movw   $D_0776,-0x6(%bp)
L_0bbc:
	mov    D_01f6,%ax
	add    $D_0776,%ax
	cmp    %ax,-0x6(%bp)
	jae   L_0bf7
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	mov    (%bx),%al
	cbtw
	test   $0x80,%ax
	je   L_0bbc
	cmpw   $0x0,D_00e0
	je   L_0bf7
	pushw  D_0ce4
	pushw  D_0ce2
	mov    D_01f6,%ax
	inc    %ax
	push   %ax
	mov    $D_0776,%ax
	push   %ax
	mov    $D_0cf6,%ax
	push   %ax
	call   L_204f
	add    $0xa,%sp
L_0bf7:
	movw   $D_0776,-0x6(%bp)
L_0bfc:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%di
	incw   -0x6(%bp)
	or     %di,%di
	je   L_0c36
	test   $0x80,%di
	jne   L_0c17
	cmp    $D_11fe,%si
	jb   L_0c25
L_0c17:
	movw   $0xa,D_140a
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0c25:
	mov    %si,%bx
	inc    %si
	mov    %di,%cx
	mov    %cl,(%bx)
	addw   $0x1,D_0ce2
	adcw   $0x0,D_0ce4
L_0c36:
	cmp    $0xa,%di
	je   L_0c3e
	jmp   L_0b91
L_0c3e:
	dec    %si
	movb   $0x0,(%si)
	mov    -0x6(%bp),%ax
	mov    %ax,D_0976
	xor    %ax,%ax
L_0c4a:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0c4f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xa,%sp
	movw   $0x200,-0xa(%bp)
	mov    $D_0776,%di
	mov    D_0ff6,%ax
	mov    %ax,-0x6(%bp)
L_0c65:
	mov    -0x6(%bp),%bx
	addw   $0x2,-0x6(%bp)
	pushw  (%bx)
	call   L_0ff2
	pop    %cx
	mov    %ax,%si
L_0c74:
	decw   -0xa(%bp)
	jge   L_0ce0
	mov    %di,%ax
	sub    $D_0776,%ax
	mov    %ax,-0x8(%bp)
	cmpw   $0x0,D_00e0
	je   L_0cb2
	mov    -0x8(%bp),%ax
	cwtd
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    D_0ce2,%ax
	mov    D_0ce4,%dx
	sub    -0xe(%bp),%ax
	sbb    -0xc(%bp),%dx
	push   %dx
	push   %ax
	pushw  -0x8(%bp)
	mov    $D_0776,%ax
	push   %ax
	mov    $D_0cf6,%ax
	push   %ax
	call   L_204f
	add    $0xa,%sp
L_0cb2:
	pushw  -0x8(%bp)
	mov    $D_0776,%ax
	push   %ax
	pushw  D_0ce6
	call   L_2ce8
	add    $0x6,%sp
	cmp    -0x8(%bp),%ax
	je   L_0cd8
	mov    $D_0012,%ax
	push   %ax
	call   L_1f5e
	pop    %cx
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0cd8:
	movw   $0x1ff,-0xa(%bp)
	mov    $D_0776,%di
L_0ce0:
	addw   $0x1,D_0ce2
	adcw   $0x0,D_0ce4
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	je   L_0cfe
	jmp   L_0c74
L_0cfe:
	movb   $0xa,-0x1(%di)
	mov    -0x6(%bp),%ax
	cmp    D_0ff8,%ax
	ja   L_0d0e
	jmp   L_0c65
L_0d0e:
	mov    %di,%ax
	sub    $D_0776,%ax
	mov    %ax,-0x8(%bp)
	cmpw   $0x0,D_00e0
	je   L_0d47
	mov    -0x8(%bp),%ax
	cwtd
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    D_0ce2,%ax
	mov    D_0ce4,%dx
	sub    -0xe(%bp),%ax
	sbb    -0xc(%bp),%dx
	push   %dx
	push   %ax
	pushw  -0x8(%bp)
	mov    $D_0776,%ax
	push   %ax
	mov    $D_0cf6,%ax
	push   %ax
	call   L_204f
	add    $0xa,%sp
L_0d47:
	pushw  -0x8(%bp)
	mov    $D_0776,%ax
	push   %ax
	pushw  D_0ce6
	call   L_2ce8
	add    $0x6,%sp
	cmp    -0x8(%bp),%ax
	je   L_0d6d
	mov    $D_0012,%ax
	push   %ax
	call   L_1f5e
	pop    %cx
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0d6d:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0d74:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	movw   $0x0,-0x8(%bp)
	mov    0x6(%bp),%ax
	mov    %ax,D_00f4
L_0d87:
	/* TODO: indirect call - verify */
	call   *0x4(%bp)
	or     %ax,%ax
	je   L_0d91
	jmp   L_0e44
L_0d91:
	mov    D_00e2,%ax
	sub    D_1406,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	inc    %ax
	cmp    D_001e,%ax
	jb   L_0e08
	mov    D_1406,%ax
	mov    %ax,-0xc(%bp)
	addw   $0x200,D_001e
	pushw  D_1406
	call   L_2b1e
	pop    %cx
	mov    D_001e,%ax
	shl    %ax
	push   %ax
	pushw  D_1406
	call   L_2b3a
	add    $0x4,%sp
	mov    %ax,D_1406
	or     %ax,%ax
	jne   L_0de4
	movw   $0xa,D_140a
	mov    -0xc(%bp),%ax
	mov    %ax,D_1406
	mov    $D_006f,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0de4:
	mov    D_1406,%ax
	sub    -0xc(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	shl    %ax
	add    %ax,D_00f4
	mov    D_1406,%ax
	sub    -0xc(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	shl    %ax
	add    %ax,D_00e2
L_0e08:
	call   L_104d
	mov    %ax,-0xa(%bp)
	incw   -0x8(%bp)
	addw   $0x2,D_00e2
	mov    D_00e2,%di
	lea    0x2(%di),%ax
	mov    %ax,%si
	addw   $0x2,D_00f4
	mov    D_00f4,%ax
	mov    %ax,-0x6(%bp)
L_0e2a:
	cmp    -0x6(%bp),%di
	jbe   L_0e39
	dec    %di
	dec    %di
	mov    (%di),%ax
	dec    %si
	dec    %si
	mov    %ax,(%si)
	jmp   L_0e2a
L_0e39:
	mov    -0x6(%bp),%bx
	mov    -0xa(%bp),%ax
	mov    %ax,(%bx)
	jmp   L_0d87
L_0e44:
	mov    -0x8(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e4e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	call   L_0803
	call   L_26c8
	mov    %ax,%si
	or     %ax,%ax
	jne   L_0e9b
	pushw  D_0504
	mov    $0x1,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	pushw  D_0ce8
	mov    $0x3,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	xor    %ax,%ax
	push   %ax
	mov    $D_007f,%ax
	push   %ax
	mov    $D_007c,%ax
	push   %ax
	mov    $D_0074,%ax
	push   %ax
	call   L_26aa
	add    $0x8,%sp
	mov    $0x40,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0e9b:
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	mov    %ax,%di
L_0eab:
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_2ccc
	pop    %cx
	mov    %ax,-0x6(%bp)
	cmp    %si,%ax
	je   L_0ec0
	cmpw   $0xffff,-0x6(%bp)
	jne   L_0eab
L_0ec0:
	push   %di
	mov    $0x2,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	mov    $D_0082,%ax
	push   %ax
	call   L_1f5e
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0eda:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_000a
	je   L_0f04
	cmpw   $0x0,D_0cea
	je   L_0f04
	mov    D_00e2,%ax
	cmp    D_1406,%ax
	je   L_0f04
	movw   $0x0,D_0cea
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_0f04:
	pushw  D_1408
	call   L_2cb8
	pop    %cx
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f17:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_07a9
	call   L_083e
	call   L_081b
	pushw  D_0ff8
	pushw  D_0ff6
	call   L_0f37
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f37:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%ax
	inc    %ax
	inc    %ax
	mov    %ax,%si
	mov    D_00e2,%ax
	mov    %ax,-0x6(%bp)
	mov    %si,%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	shl    %ax
	sub    %ax,D_00e2
L_0f5d:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %di,%bx
	inc    %di
	inc    %di
	mov    %ax,(%bx)
	cmp    -0x6(%bp),%si
	jbe   L_0f5d
	mov    0x4(%bp),%di
	cmp    D_00e2,%di
	jbe   L_0f7b
	mov    D_00e2,%di
L_0f7b:
	mov    %di,D_00f4
	movw   $0x1,D_0cea
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f8a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    D_00e2,%ax
	mov    %ax,-0x6(%bp)
	mov    D_1406,%ax
	inc    %ax
	inc    %ax
	mov    %ax,%di
L_0f9d:
	testw  $0x1,(%di)
	jne   L_0fac
	cmp    -0x6(%bp),%di
	jae   L_0fed
	inc    %di
	inc    %di
	jmp   L_0f9d
L_0fac:
	lea    0x2(%di),%ax
	mov    %ax,%si
L_0fb1:
	cmp    -0x6(%bp),%si
	ja   L_0fd2
	testw  $0x1,(%si)
	je   L_0fc4
	inc    %si
	inc    %si
	mov    %di,D_00f4
	jmp   L_0fb1
L_0fc4:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %di,%bx
	inc    %di
	inc    %di
	mov    %ax,(%bx)
	jmp   L_0fb1
L_0fd2:
	lea    -0x2(%di),%ax
	mov    %ax,D_00e2
	mov    D_00f4,%ax
	cmp    D_00e2,%ax
	jbe   L_0fe7
	mov    D_00e2,%ax
	mov    %ax,D_00f4
L_0fe7:
	movw   $0x1,D_0cea
L_0fed:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ff2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    $D_0ffe,%si
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_10e0
	add    $0x4,%sp
	mov    %ax,%di
	mov    D_023e,%ax
	mov    %ax,-0x6(%bp)
	andw   $0xff00,0x4(%bp)
L_1014:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	je   L_1045
	decw   -0x6(%bp)
	jne   L_1014
	xor    %ax,%ax
	push   %ax
	addw   $0x100,0x4(%bp)
	pushw  0x4(%bp)
	call   L_10e0
	add    $0x4,%sp
	mov    %ax,%di
	mov    D_023e,%ax
	mov    %ax,-0x6(%bp)
	jmp   L_1014
L_1045:
	mov    $D_0ffe,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_104d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	movw   $0x1,D_0cea
	mov    $D_0ffe,%si
	mov    D_0508,%ax
	mov    %ax,-0x8(%bp)
	mov    $0x1,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_10e0
	add    $0x4,%sp
	mov    %ax,%di
	mov    D_023e,%ax
	mov    %ax,-0x6(%bp)
	andw   $0xff00,-0x8(%bp)
L_107e:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,(%di)
	or     %cl,%cl
	je   L_10bf
	mov    %di,%bx
	inc    %di
	cmpb   $0xa,(%bx)
	jne   L_109e
	dec    %di
	movb   $0x0,(%di)
	mov    %si,D_070e
	jmp   L_10bf
L_109e:
	decw   -0x6(%bp)
	jne   L_107e
	mov    $0x1,%ax
	push   %ax
	addw   $0x100,-0x8(%bp)
	pushw  -0x8(%bp)
	call   L_10e0
	add    $0x4,%sp
	mov    %ax,%di
	mov    D_023e,%ax
	mov    %ax,-0x6(%bp)
	jmp   L_107e
L_10bf:
	mov    D_0508,%ax
	mov    %ax,-0x6(%bp)
	mov    %si,%ax
	sub    $D_0ffe,%ax
	add    $0x3,%ax
	sar    %ax
	and    $0x7ffe,%ax
	add    %ax,D_0508
	mov    -0x6(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_10e0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	and    $0xff,%ax
	mov    %ax,%di
	mov    0x4(%bp),%ax
	shl    %ax
	and    $0x1fc,%ax
	mov    %ax,%si
	cmp    $0xff,%di
	jl   L_1113
	movw   $0xa,D_140a
	mov    $D_0006,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_1113:
	mov    $0x200,%ax
	sub    %si,%ax
	mov    %ax,D_023e
	cmp    D_000e,%di
	jne   L_112e
	mov    0x6(%bp),%ax
	or     %ax,D_0978
	lea    D_1206(%si),%ax
	jmp   L_1138
L_112e:
	cmp    D_0010,%di
	jne   L_113a
	lea    D_050e(%si),%ax
L_1138:
	jmp   L_11b4
L_113a:
	cmpw   $0x0,0x6(%bp)
	jne   L_11b7
	cmpw   $0x0,D_0978
	je   L_1178
	cmpw   $0x0,D_11fe
	je   L_1166
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    $0x200,%ax
	push   %ax
	mov    $D_1206,%ax
	push   %ax
	mov    $D_097e,%ax
	push   %ax
	call   L_204f
	add    $0xa,%sp
L_1166:
	mov    $D_0ce8,%ax
	push   %ax
	mov    $D_1206,%ax
	push   %ax
	pushw  D_000e
	call   L_a240
	add    $0x6,%sp
L_1178:
	movw   $0x0,D_0978
	mov    %di,D_000e
	mov    $D_0c52,%ax
	push   %ax
	mov    $D_1206,%ax
	push   %ax
	push   %di
	call   L_1240
	add    $0x6,%sp
	cmpw   $0x0,D_11fe
	je   L_11b0
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    $0x200,%ax
	push   %ax
	mov    $D_1206,%ax
	push   %ax
	mov    $D_097e,%ax
	push   %ax
	call   L_204f
	add    $0xa,%sp
L_11b0:
	lea    D_1206(%si),%ax
L_11b4:
	jmp   L_1239
L_11b7:
	cmpw   $0x0,D_0010
	jl   L_121d
	cmpw   $0x0,D_11fe
	je   L_121f
	movw   $D_050e,-0x6(%bp)
	movw   $D_0246,-0x8(%bp)
	movw   $0x200,-0xa(%bp)
L_11d4:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_11f3
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	incw   -0x8(%bp)
	jmp   L_11d4
L_11f3:
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    $0x200,%ax
	push   %ax
	mov    $D_0246,%ax
	push   %ax
	mov    $D_097e,%ax
	push   %ax
	call   L_204f
	add    $0xa,%sp
	mov    $D_0ce8,%ax
	push   %ax
	mov    $D_0246,%ax
	push   %ax
	pushw  D_0010
	call   L_1240
	add    $0x6,%sp
L_121d:
	jmp   L_1231
L_121f:
	mov    $D_0ce8,%ax
	push   %ax
	mov    $D_050e,%ax
	push   %ax
	pushw  D_0010
	call   L_1240
	add    $0x6,%sp
L_1231:
	mov    %di,D_0010
	lea    D_050e(%si),%ax
L_1239:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1240:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %ax,%ax
	push   %ax
	mov    0x4(%bp),%ax
	cwtd
	mov    $0x9,%cx
	or     %cx,%cx
	je   L_1259
L_1253:
	shl    %ax
	rcl    %dx
	loop   L_1253
L_1259:
	push   %dx
	push   %ax
	pushw  D_000c
	call   L_27f8
	add    $0x8,%sp
	mov    $0x200,%ax
	push   %ax
	pushw  0x6(%bp)
	pushw  D_000c
	/* TODO: indirect call - verify */
	call   *0x8(%bp)
	add    $0x6,%sp
	cmp    $0x200,%ax
	je   L_1283
	mov    $D_0006,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_1283:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1287:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  D_000c
	call   L_2c28
	pop    %cx
	movw   $0x2,D_0508
	mov    $D_04ce,%di
L_129d:
	cmp    $D_0502,%di
	jae   L_12ad
	mov    %di,%bx
	inc    %di
	inc    %di
	movw   $0x0,(%bx)
	jmp   L_129d
L_12ad:
	movw   $0x0,D_0cec
	movw   $0x0,D_0ce0
	/* movw constant -> D_000e */
	mov	$0xffff,%ax
	mov	%ax,D_000e
	/* movw constant -> D_0010 */
	mov	$0xffff,%ax
	mov	%ax,D_0010
	movw   $0x0,D_0978
	mov    $0x180,%ax
	push   %ax
	pushw  D_1408
	call   L_259c
	add    $0x4,%sp
	push   %ax
	call   L_2c28
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	pushw  D_1408
	call   L_2c3c
	add    $0x4,%sp
	mov    %ax,D_000c
	cmpw   $0x0,D_0cee
	je   L_130a
	movw   $0x1,D_11fe
	mov    $D_097e,%ax
	push   %ax
	mov    $D_0766,%ax
	push   %ax
	call   L_246c
	add    $0x4,%sp
L_130a:
	mov    D_1406,%ax
	mov    %ax,D_00e2
	mov    %ax,D_00f4
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1317:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x102,%sp
	cmpw   $0x0,D_0770
	je   L_132f
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_132f:
	call   L_07d3
	call   L_081b
	call   L_0aa1
	mov    %ax,%si
	cmp    $0xa,%ax
	jne   L_1347
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_1347:
	push   %si
	call   L_1909
	pop    %cx
	lea    -0x106(%bp),%ax
	mov    %ax,%di
L_1352:
	call   L_0aa1
	mov    %ax,%si
	cmp    $0xa,%ax
	je   L_1396
	cmp    $0xffff,%si
	jne   L_1369
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_1369:
	cmp    $0x5c,%si
	jne   L_137e
	call   L_0aa1
	mov    %ax,%si
	cmp    $0xa,%si
	je   L_137e
	mov    %di,%bx
	inc    %di
	movb   $0x5c,(%bx)
L_137e:
	mov    %di,%bx
	inc    %di
	mov    %si,%cx
	mov    %cl,(%bx)
	lea    -0x8(%bp),%ax
	cmp    %ax,%di
	jb   L_1352
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_1352
L_1396:
	mov    %di,%bx
	inc    %di
	movb   $0xa,(%bx)
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	mov    D_1406,%ax
	mov    %ax,-0x6(%bp)
L_13a8:
	mov    -0x6(%bp),%ax
	cmp    D_00e2,%ax
	ja   L_13e8
	mov    -0x6(%bp),%bx
	andw   $0xfffe,(%bx)
	mov    -0x6(%bp),%ax
	cmp    D_0ff6,%ax
	jb   L_13e2
	mov    -0x6(%bp),%ax
	cmp    D_0ff8,%ax
	ja   L_13e2
	pushw  -0x6(%bp)
	xor    %ax,%ax
	push   %ax
	call   L_1b94
	add    $0x4,%sp
	cmp    0x4(%bp),%ax
	jne   L_13e2
	mov    -0x6(%bp),%bx
	orw    $0x1,(%bx)
L_13e2:
	addw   $0x2,-0x6(%bp)
	jmp   L_13a8
L_13e8:
	cmpb   $0x64,-0x106(%bp)
	jne   L_1402
	cmpb   $0xa,-0x105(%bp)
	jne   L_1402
	cmpb   $0x0,-0x104(%bp)
	jne   L_1402
	call   L_0f8a
	jmp   L_143e
L_1402:
	mov    D_1406,%ax
	mov    %ax,-0x6(%bp)
L_1408:
	mov    -0x6(%bp),%ax
	cmp    D_00e2,%ax
	ja   L_143e
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	test   $0x1,%ax
	je   L_1438
	mov    -0x6(%bp),%bx
	andw   $0xfffe,(%bx)
	mov    -0x6(%bp),%ax
	mov    %ax,D_00f4
	lea    -0x106(%bp),%ax
	mov    %ax,D_0770
	call   L_0184
	mov    D_1406,%ax
	mov    %ax,-0x6(%bp)
L_1438:
	addw   $0x2,-0x6(%bp)
	jmp   L_1408
L_143e:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1445:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    $D_0776,%di
	mov    D_0ff6,%ax
	mov    %ax,-0x6(%bp)
L_1454:
	mov    -0x6(%bp),%ax
	cmp    D_0ff8,%ax
	ja   L_148e
	mov    -0x6(%bp),%bx
	pushw  (%bx)
	call   L_0ff2
	pop    %cx
	mov    %ax,%si
L_1468:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,(%di)
	or     %cl,%cl
	je   L_1488
	mov    %di,%ax
	inc    %di
	cmp    $D_0974,%ax
	jb   L_1468
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_1468
L_1488:
	addw   $0x2,-0x6(%bp)
	jmp   L_1454
L_148e:
	mov    $D_0ffe,%si
	mov    $D_0776,%di
L_1494:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_1494
	call   L_104d
	mov    D_0ff6,%bx
	mov    %ax,(%bx)
	mov    D_0ff6,%ax
	cmp    D_0ff8,%ax
	jae   L_14c7
	pushw  D_0ff8
	mov    D_0ff6,%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_0f37
	add    $0x4,%sp
L_14c7:
	mov    D_0ff6,%ax
	mov    %ax,D_00f4
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_14d2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	call   L_15ab
	mov    %ax,-0x8(%bp)
	mov    D_0ff6,%si
L_14e4:
	cmp    D_0ff8,%si
	jbe   L_14ed
	jmp   L_1596
L_14ed:
	push   %si
	xor    %ax,%ax
	push   %ax
	call   L_1b94
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_14fe
	jmp   L_1591
L_14fe:
	orw    $0x1,0x4(%bp)
	call   L_167b
	cmpw   $0x0,-0x8(%bp)
	je   L_152b
L_150c:
	mov    D_0712,%bx
	cmpb   $0x0,(%bx)
	je   L_152b
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_1b94
	add    $0x4,%sp
	or     %ax,%ax
	je   L_152b
	call   L_167b
	jmp   L_150c
L_152b:
	call   L_104d
	mov    %ax,D_0cec
	andw   $0xfffe,(%si)
	cmpw   $0x0,D_0ce0
	je   L_1554
	mov    $D_04ce,%di
L_153f:
	cmp    $D_0502,%di
	jae   L_1554
	mov    (%di),%ax
	cmp    (%si),%ax
	jne   L_1550
	mov    D_0cec,%ax
	mov    %ax,(%di)
L_1550:
	inc    %di
	inc    %di
	jmp   L_153f
L_1554:
	mov    (%si),%ax
	mov    %ax,D_0cce
	mov    D_0cec,%ax
	mov    %ax,(%si)
	mov    D_1406,%ax
	mov    %ax,-0xa(%bp)
	push   %si
	mov    $0x1649,%ax
	push   %ax
	call   L_0d74
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	mov    D_1406,%ax
	sub    -0xa(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	add    %ax,-0x6(%bp)
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %ax,%si
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %ax,D_0ff8
L_1591:
	inc    %si
	inc    %si
	jmp   L_14e4
L_1596:
	cmpw   $0x0,0x4(%bp)
	jne   L_15a4
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_15a4:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_15ab:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	call   L_0aa1
	mov    %ax,%di
	cmp    $0xa,%ax
	je   L_15c0
	cmp    $0x20,%di
	jne   L_15c8
L_15c0:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_15c8:
	push   %di
	call   L_1909
	pop    %cx
	movw   $D_00f6,-0x6(%bp)
L_15d2:
	call   L_0aa1
	mov    %ax,%si
	cmp    $0x5c,%si
	jne   L_15e4
	call   L_0aa1
	or     $0x80,%ax
	mov    %ax,%si
L_15e4:
	cmp    $0xa,%si
	jne   L_15fe
	cmpw   $0x0,D_0770
	je   L_15f6
	or     $0x80,%si
	jmp   L_15fe
L_15f6:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_15fe:
	cmp    %di,%si
	je   L_161d
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	cmpw   $D_01f6,-0x6(%bp)
	jb   L_15d2
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_15d2
L_161d:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x6(%bp)
	call   L_0aa1
	mov    %ax,D_00f2
	cmp    $0x67,%ax
	jne   L_163f
	movw   $0x0,D_00f2
	call   L_083e
	mov    $0x1,%ax
	jmp   L_1644
L_163f:
	call   L_083e
	xor    %ax,%ax
L_1644:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0ffe,%di
	mov    D_070e,%si
	or     %si,%si
	jne   L_165e
	mov    $0xffff,%ax
	jmp   L_1677
L_165e:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_165e
	movw   $0x0,D_070e
	xor    %ax,%ax
L_1677:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_167b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    $D_0ffe,%di
	mov    $D_0776,%si
	movw   $D_00f6,-0x6(%bp)
L_168e:
	cmp    D_0710,%di
	jae   L_16a3
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_168e
L_16a3:
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	je   L_1734
	cmpw   $0x26,-0x8(%bp)
	jne   L_16cf
	pushw  D_0712
	pushw  D_0710
	push   %si
	call   L_1782
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_16a3
L_16cf:
	testw  $0x80,-0x8(%bp)
	je   L_1713
	andw   $0x7f,-0x8(%bp)
	cmpw   $0x31,-0x8(%bp)
	jl   L_1713
	mov    D_0cf0,%ax
	add    $0x31,%ax
	cmp    %ax,-0x8(%bp)
	jge   L_1713
	mov    -0x8(%bp),%ax
	sub    $0x31,%ax
	mov    %ax,%bx
	shl    %bx
	pushw  D_0cd6(%bx)
	mov    -0x8(%bp),%ax
	sub    $0x31,%ax
	mov    %ax,%bx
	shl    %bx
	pushw  D_0756(%bx)
	push   %si
	call   L_1782
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_16a3
L_1713:
	mov    -0x8(%bp),%ax
	and    $0x7f,%ax
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	cmp    $D_0976,%si
	jae   L_1729
	jmp   L_16a3
L_1729:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_16a3
L_1734:
	mov    D_0712,%di
	mov    %si,%ax
	sub    $D_0776,%ax
	add    $D_0ffe,%ax
	mov    %ax,D_0712
L_1743:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	je   L_1764
	cmp    $D_0976,%si
	jb   L_1743
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_1743
L_1764:
	mov    $D_0ffe,%di
	mov    $D_0776,%si
L_176a:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_176a
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
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_178f:
	cmp    0x8(%bp),%si
	jae   L_17b1
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	cmp    $D_0976,%di
	jb   L_178f
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_178f
L_17b1:
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_17b7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	call   L_07a9
	call   L_081b
	call   L_05e8
	mov    %ax,%di
	or     %ax,%ax
	jne   L_17d6
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_17d6:
	call   L_083e
	cmpw   $0x0,0x4(%bp)
	je   L_181a
	mov    D_00e2,%si
	mov    D_1406,%ax
	mov    %ax,-0x8(%bp)
	mov    %si,%ax
	inc    %si
	inc    %si
	push   %ax
	mov    $0x18e1,%ax
	push   %ax
	call   L_0d74
	add    $0x4,%sp
	mov    D_00e2,%ax
	mov    %ax,-0x6(%bp)
	mov    D_1406,%ax
	sub    -0x8(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xa(%bp)
	shl    %ax
	add    %ax,%si
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %ax,%di
	jmp   L_1837
L_181a:
	mov    D_0ff8,%ax
	mov    %ax,-0x6(%bp)
	mov    D_0ff6,%si
L_1824:
	cmp    -0x6(%bp),%si
	ja   L_1833
	mov    %si,%bx
	inc    %si
	inc    %si
	andw   $0xfffe,(%bx)
	jmp   L_1824
L_1833:
	mov    D_0ff6,%si
L_1837:
	addw   $0x2,-0x6(%bp)
	cmp    %si,%di
	jae   L_1875
	mov    -0x6(%bp),%ax
	sub    %si,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	shl    %ax
	add    %di,%ax
	mov    %ax,D_00f4
	inc    %di
	inc    %di
	cmp    %si,%di
	je   L_18ad
	push   %si
	push   %di
	call   L_18b4
	add    $0x4,%sp
	pushw  -0x6(%bp)
	push   %si
	call   L_18b4
	add    $0x4,%sp
	pushw  -0x6(%bp)
	push   %di
	call   L_18b4
	add    $0x4,%sp
	jmp   L_18a7
L_1875:
	cmp    -0x6(%bp),%di
	jb   L_189f
	mov    %di,%ax
	inc    %di
	inc    %di
	mov    %ax,D_00f4
	pushw  -0x6(%bp)
	push   %si
	call   L_18b4
	add    $0x4,%sp
	push   %di
	pushw  -0x6(%bp)
	call   L_18b4
	add    $0x4,%sp
	push   %di
	push   %si
	call   L_18b4
	add    $0x4,%sp
	jmp   L_18a7
L_189f:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_18a7:
	movw   $0x1,D_0cea
L_18ad:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_18b4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_18c2:
	dec    %si
	dec    %si
	mov    (%si),%ax
	mov    %ax,-0x6(%bp)
	cmp    %di,%si
	jbe   L_18dc
	mov    (%di),%ax
	mov    %ax,(%si)
	mov    %di,%bx
	inc    %di
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,(%bx)
	jmp   L_18c2
L_18dc:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_0ff6,%ax
	cmp    D_0ff8,%ax
	jbe   L_18f4
	mov    $0xffff,%ax
	jmp   L_1905
L_18f4:
	mov    D_0ff6,%bx
	addw   $0x2,D_0ff6
	pushw  (%bx)
	call   L_0ff2
	pop    %cx
	xor    %ax,%ax
L_1905:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1909:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xe,%sp
	movw   $D_0446,-0x6(%bp)
	mov    0x4(%bp),%di
	lea    -0xd(%bp),%ax
	mov    %ax,-0x10(%bp)
	call   L_0aa1
	mov    %ax,%si
	cmp    %di,%ax
	jne   L_193a
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	jne   L_1984
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_1984
L_193a:
	movw   $0x0,D_0cd0
	movw   $0x0,D_0cf0
	cmp    $0x5e,%si
	jne   L_1954
	call   L_0aa1
	mov    %ax,%si
	incw   D_0cd0
L_1954:
	mov    %si,D_00f2
	movw   $0x0,-0x8(%bp)
L_195d:
	cmpw   $D_04c6,-0x6(%bp)
	jb   L_1967
	jmp   L_1b04
L_1967:
	call   L_0aa1
	mov    %ax,%si
	cmp    %di,%si
	jne   L_1987
	lea    -0xd(%bp),%ax
	cmp    %ax,-0x10(%bp)
	je   L_197b
	jmp   L_1b04
L_197b:
	mov    -0x6(%bp),%bx
	movb   $0xb,(%bx)
	incw   -0x6(%bp)
L_1984:
	jmp   L_1b8d
L_1987:
	cmp    $0x2a,%si
	je   L_1992
	mov    -0x6(%bp),%ax
	mov    %ax,-0x8(%bp)
L_1992:
	mov    %si,%ax
	jmp   L_1b47
L_1997:
	call   L_0aa1
	mov    %ax,%si
	cmp    $0x28,%ax
	jne   L_19d4
	cmpw   $0x5,D_0cf0
	jl   L_19ab
	jmp   L_1b04
L_19ab:
	mov    -0x10(%bp),%bx
	mov    D_0cf0,%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x10(%bp)
	mov    -0x6(%bp),%bx
	movb   $0x1,(%bx)
	incw   -0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    D_0cf0,%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   D_0cf0
	incw   -0x6(%bp)
L_19d2:
	jmp   L_195d
L_19d4:
	cmp    $0x29,%si
	jne   L_1a02
	lea    -0xd(%bp),%ax
	cmp    %ax,-0x10(%bp)
	ja   L_19e4
	jmp   L_1b04
L_19e4:
	mov    -0x6(%bp),%bx
	movb   $0xc,(%bx)
	incw   -0x6(%bp)
	decw   -0x10(%bp)
	mov    -0x10(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
L_1a00:
	jmp   L_19d2
L_1a02:
	cmp    $0x31,%si
	jl   L_1a24
	cmp    $0x36,%si
	jge   L_1a24
	mov    -0x6(%bp),%bx
	movb   $0xe,(%bx)
	incw   -0x6(%bp)
	lea    -0x31(%si),%ax
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
L_1a22:
	jmp   L_19d2
L_1a24:
	mov    -0x6(%bp),%bx
	movb   $0x2,(%bx)
	incw   -0x6(%bp)
	cmp    $0xa,%si
	jne   L_1a35
	jmp   L_1ab5
L_1a35:
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
L_1a3f:
	jmp   L_1a00
L_1a41:
	mov    -0x6(%bp),%bx
	movb   $0x4,(%bx)
	incw   -0x6(%bp)
	jmp   L_1a00
L_1a4c:
	cmpw   $0x0,-0x8(%bp)
	je   L_1a60
	mov    -0x8(%bp),%bx
	cmpb   $0x1,(%bx)
	je   L_1a60
	mov    -0x8(%bp),%bx
	cmpb   $0xc,(%bx)
L_1a60:
	jne   L_1a65
	jmp   L_1b5e
L_1a65:
	mov    -0x8(%bp),%bx
	orb    $0x1,(%bx)
	jmp   L_1a22
L_1a6d:
	call   L_0aa1
	mov    %ax,D_00f2
	cmp    %di,%ax
	je   L_1a7a
	jmp   L_1b5e
L_1a7a:
	mov    -0x6(%bp),%bx
	movb   $0xa,(%bx)
	incw   -0x6(%bp)
L_1a83:
	jmp   L_1a3f
L_1a85:
	mov    -0x6(%bp),%bx
	movb   $0x6,(%bx)
	incw   -0x6(%bp)
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x6(%bp)
	movw   $0x1,-0x12(%bp)
	call   L_0aa1
	mov    %ax,%si
	cmp    $0x5e,%ax
	jne   L_1ab2
	call   L_0aa1
	mov    %ax,%si
	mov    -0x6(%bp),%bx
	movb   $0x8,-0x2(%bx)
L_1ab2:
	cmp    $0xa,%si
L_1ab5:
	je   L_1b04
	cmp    $0x2d,%si
	jne   L_1b06
	mov    -0x6(%bp),%bx
	cmpb   $0x0,-0x1(%bx)
	je   L_1b06
	call   L_0aa1
	mov    %ax,%si
	cmp    $0x5d,%ax
	jne   L_1add
	mov    -0x6(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x6(%bp)
	incw   -0x12(%bp)
	jmp   L_1b24
L_1add:
	mov    -0x6(%bp),%bx
	mov    -0x1(%bx),%al
	cbtw
	cmp    %si,%ax
	jge   L_1b06
	mov    -0x6(%bp),%bx
	mov    -0x1(%bx),%al
	cbtw
	inc    %ax
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	incw   -0x12(%bp)
	cmpw   $D_04c6,-0x6(%bp)
	jb   L_1add
L_1b04:
	jmp   L_1b7a
L_1b06:
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	incw   -0x12(%bp)
	cmpw   $D_04c6,-0x6(%bp)
	jae   L_1b7a
	call   L_0aa1
	mov    %ax,%si
	cmp    $0x5d,%ax
	jne   L_1ab2
L_1b24:
	mov    -0x8(%bp),%bx
	mov    -0x12(%bp),%ax
	mov    %ax,%cx
	mov    %cl,0x1(%bx)
L_1b2f:
	jmp   L_1a83
L_1b32:
	mov    -0x6(%bp),%bx
	movb   $0x2,(%bx)
	incw   -0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	jmp   L_1b2f
L_1b47:
	cmp    $0x2a,%ax
	jne   L_1b4f
	jmp   L_1a4c
L_1b4f:
	jg   L_1b60
	cmp    $0xa,%ax
	je   L_1b7a
	cmp    $0x24,%ax
	jne   L_1b5e
	jmp   L_1a6d
L_1b5e:
	jmp   L_1b32
L_1b60:
	cmp    $0x2e,%ax
	jne   L_1b68
	jmp   L_1a41
L_1b68:
	cmp    $0x5b,%ax
	jne   L_1b70
	jmp   L_1a85
L_1b70:
	cmp    $0x5c,%ax
	jne   L_1b78
	jmp   L_1997
L_1b78:
	jmp   L_1b32
L_1b7a:
	movb   $0x0,D_0446
	movw   $0x0,D_0cf0
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_1b8d:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b94:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	movb   $0x0,-0x5(%bp)
L_1b9e:
	cmpb   $0x5,-0x5(%bp)
	jge   L_1bc5
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	movw   $0x0,D_0756(%bx)
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	movw   $0x0,D_0cd6(%bx)
	incb   -0x5(%bp)
	jmp   L_1b9e
L_1bc5:
	cmpw   $0x0,0x4(%bp)
	je   L_1bf7
	cmpw   $0x0,D_0cd0
	je   L_1bd6
	xor    %ax,%ax
	jmp   L_1c02
L_1bd6:
	mov    $D_0ffe,%di
	mov    $D_0776,%si
L_1bdc:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_1bdc
	mov    D_0712,%di
	mov    %di,D_0c7e
	jmp   L_1c15
L_1bf7:
	mov    0x6(%bp),%ax
	cmp    D_1406,%ax
	jne   L_1c04
	xor    %ax,%ax
L_1c02:
	jmp   L_1c2b
L_1c04:
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_0ff2
	pop    %cx
	mov    %ax,%di
	movw   $0x0,D_0c7e
L_1c15:
	mov    $D_0446,%si
	cmpw   $0x0,D_0cd0
	je   L_1c2d
	mov    %di,D_0710
	push   %si
	push   %di
	call   L_1c8e
	add    $0x4,%sp
L_1c2b:
	jmp   L_1c68
L_1c2d:
	cmpb   $0x2,(%si)
	jne   L_1c6a
	mov    0x1(%si),%al
	mov    %al,-0x5(%bp)
L_1c38:
	mov    (%di),%al
	cbtw
	mov    -0x5(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_1c5e
	push   %si
	push   %di
	call   L_1c8e
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1c5e
	mov    %di,D_0710
	mov    $0x1,%ax
	jmp   L_1c89
L_1c5e:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_1c38
	xor    %ax,%ax
L_1c68:
	jmp   L_1c89
L_1c6a:
	push   %si
	push   %di
	call   L_1c8e
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1c7f
	mov    %di,D_0710
	mov    $0x1,%ax
	jmp   L_1c89
L_1c7f:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_1c6a
	xor    %ax,%ax
L_1c89:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1c8e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%si
	mov    0x6(%bp),%di
L_1c9e:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_1e6d
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
	je   L_1c9e
	xor    %ax,%ax
	jmp   L_1cfe
	mov    %si,%bx
	inc    %si
	cmpb   $0x0,(%bx)
	jne   L_1c9e
	xor    %ax,%ax
	jmp   L_1cfe
	cmpb   $0x0,(%si)
	je   L_1c9e
	xor    %ax,%ax
	jmp   L_1cfe
	mov    %si,D_0712
	mov    $0x1,%ax
	jmp   L_1cfe
	mov    $0x1,%ax
	push   %ax
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	push   %ax
	push   %di
	call   L_1ecc
	add    $0x6,%sp
	or     %ax,%ax
	je   L_1cfc
	mov    (%di),%al
	cbtw
	add    %ax,%di
L_1cfa:
	jmp   L_1c9e
L_1cfc:
	xor    %ax,%ax
L_1cfe:
	jmp   L_1d1e
	xor    %ax,%ax
	push   %ax
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	push   %ax
	push   %di
	call   L_1ecc
	add    $0x6,%sp
	or     %ax,%ax
	je   L_1d1c
	mov    (%di),%al
	cbtw
	add    %ax,%di
L_1d1a:
	jmp   L_1cfa
L_1d1c:
	xor    %ax,%ax
L_1d1e:
	jmp   L_1d82
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    %si,D_0756(%bx)
	jmp   L_1cfa
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    %si,D_0cd6(%bx)
L_1d3e:
	jmp   L_1d1a
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x8(%bp)
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x0,D_0cd6(%bx)
	jne   L_1d5c
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_1d5c:
	push   %si
	pushw  -0x8(%bp)
	call   L_1e81
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1d80
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    D_0cd6(%bx),%ax
	mov    -0x8(%bp),%bx
	shl    %bx
	sub    D_0756(%bx),%ax
	add    %ax,%si
L_1d7e:
	jmp   L_1d3e
L_1d80:
	xor    %ax,%ax
L_1d82:
	jmp   L_1ddb
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x8(%bp)
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x0,D_0cd6(%bx)
	jne   L_1da0
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_1da0:
	mov    %si,-0x6(%bp)
L_1da3:
	push   %si
	pushw  -0x8(%bp)
	call   L_1e81
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1dc7
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    D_0cd6(%bx),%ax
	mov    -0x8(%bp),%bx
	shl    %bx
	sub    D_0756(%bx),%ax
	add    %ax,%si
	jmp   L_1da3
L_1dc7:
	cmp    -0x6(%bp),%si
	jb   L_1d7e
	push   %di
	push   %si
	call   L_1c8e
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1ddd
	mov    $0x1,%ax
L_1ddb:
	jmp   L_1e57
L_1ddd:
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    D_0cd6(%bx),%ax
	mov    -0x8(%bp),%bx
	shl    %bx
	sub    D_0756(%bx),%ax
	sub    %ax,%si
	jmp   L_1dc7
	mov    %si,-0x6(%bp)
L_1df6:
	mov    %si,%bx
	inc    %si
	cmpb   $0x0,(%bx)
	je   L_1e17
	jmp   L_1df6
	mov    %si,-0x6(%bp)
L_1e03:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_1e03
	inc    %di
L_1e17:
	jmp   L_1e41
	mov    %si,-0x6(%bp)
L_1e1c:
	cmpb   $0x7,-0x1(%di)
	jne   L_1e27
	mov    $0x1,%ax
	jmp   L_1e29
L_1e27:
	xor    %ax,%ax
L_1e29:
	push   %ax
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	push   %ax
	push   %di
	call   L_1ecc
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_1e1c
	mov    (%di),%al
	cbtw
	add    %ax,%di
L_1e41:
	dec    %si
	cmp    D_0c7e,%si
	je   L_1e5e
	push   %di
	push   %si
	call   L_1c8e
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1e59
	mov    $0x1,%ax
L_1e57:
	jmp   L_1e60
L_1e59:
	cmp    -0x6(%bp),%si
	ja   L_1e41
L_1e5e:
	xor    %ax,%ax
L_1e60:
	jmp   L_1e7a
L_1e62:
	mov    $D_0004,%ax
	push   %ax
	call   L_0a06
	pop    %cx
	jmp   L_1d7e
L_1e6d:
	dec    %ax
	cmp    $0xe,%ax
	ja   L_1e62
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2020(%bx)
L_1e7a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e81:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,%bx
	shl    %bx
	mov    D_0756(%bx),%ax
	mov    %ax,-0x6(%bp)
L_1e9a:
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_1ec5
	mov    %di,%bx
	shl    %bx
	mov    -0x6(%bp),%ax
	cmp    D_0cd6(%bx),%ax
	jb   L_1e9a
	mov    $0x1,%ax
	jmp   L_1ec7
L_1ec5:
	xor    %ax,%ax
L_1ec7:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ecc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	cmpb   $0x0,0x6(%bp)
	jne   L_1edf
	xor    %ax,%ax
	jmp   L_1f10
L_1edf:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
L_1ee7:
	dec    %si
	je   L_1f03
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    0x6(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_1ee7
	mov    0x8(%bp),%ax
	jmp   L_1f10
L_1f03:
	cmpw   $0x0,0x8(%bp)
	jne   L_1f0e
	mov    $0x1,%ax
	jmp   L_1f10
L_1f0e:
	xor    %ax,%ax
L_1f10:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1f14:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  D_0ce4
	pushw  D_0ce2
	call   L_2931
	add    $0x8,%sp
	mov    %ax,%di
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    $D_0ce2,%ax
	push   %ax
	call   L_28dd
	add    $0x6,%sp
	cmpw   $0x0,D_0ce4
	jne   L_1f4d
	cmpw   $0x0,D_0ce2
L_1f4d:
	je   L_1f52
	call   L_1f14
L_1f52:
	lea    0x30(%di),%ax
	push   %ax
	call   L_1f8b
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1f5e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	movw   $0x0,D_0ccc
L_1f6d:
	cmpb   $0x0,(%di)
	je   L_1f7f
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1f8b
	pop    %cx
	jmp   L_1f6d
L_1f7f:
	mov    $0xa,%ax
	push   %ax
	call   L_1f8b
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1f8b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_003e,%di
	mov    0x4(%bp),%si
	cmpw   $0x0,D_0502
	je   L_1fea
	incw   D_0ccc
	cmpw   $0x48,D_0ccc
	jl   L_1fbb
	movw   $0x0,D_0ccc
	mov    %di,%bx
	inc    %di
	movb   $0x5c,(%bx)
	mov    %di,%bx
	inc    %di
	movb   $0xa,(%bx)
L_1fbb:
	cmp    $0x9,%si
	jne   L_1fc5
	mov    $0x3e,%si
	jmp   L_1fcd
L_1fc5:
	cmp    $0x8,%si
	jne   L_1fe2
	mov    $0x3c,%si
L_1fcd:
	mov    %di,%bx
	inc    %di
	movb   $0x2d,(%bx)
	mov    %di,%bx
	inc    %di
	movb   $0x8,(%bx)
	mov    %di,%bx
	inc    %di
	mov    %si,%cx
	mov    %cl,(%bx)
	jmp   L_2020
L_1fe2:
	cmp    $0x20,%si
	jge   L_2019
	cmp    $0xa,%si
L_1fea:
	je   L_2019
	mov    %di,%bx
	inc    %di
	movb   $0x5c,(%bx)
	mov    %si,%ax
	mov    $0x3,%cx
	sar    %cl,%ax
	add    $0x30,%ax
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,%ax
	and    $0x7,%ax
	add    $0x30,%ax
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	addw   $0x2,D_0ccc
	jmp   L_2020
L_2019:
	mov    %di,%bx
	inc    %di
	mov    %si,%cx
	mov    %cl,(%bx)
L_2020:
	cmp    $0xa,%si
	je   L_202b
	cmp    $D_0cc6,%di
	jb   L_2047
L_202b:
	/* dual-reloc split: movw $D_0c86,D_003e */
	mov	$D_0c86,%ax
	mov	%ax,D_003e
	mov    %di,%ax
	sub    $D_0c86,%ax
	push   %ax
	mov    $D_0c86,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_2ce8
	add    $0x6,%sp
	jmp   L_204b
L_2047:
	mov    %di,D_003e
L_204b:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_204f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%si
	mov    %si,%ax
	add    $0x100,%ax
	mov    %ax,-0xa(%bp)
	mov    0x4(%bp),%ax
	add    $0x200,%ax
	mov    %ax,-0xc(%bp)
	mov    0xa(%bp),%ax
	mov    0xc(%bp),%dx
	and    $0xff,%ax
	and    $0x0,%dx
	mov    %ax,-0x6(%bp)
	mov    0xa(%bp),%ax
	mov    0xc(%bp),%dx
	mov    $0x8,%cx
	or     %cx,%cx
	je   L_208e
L_2088:
	sar    %dx
	rcr    %ax
	loop   L_2088
L_208e:
	and    $0xff,%ax
	and    $0x0,%dx
	mov    %ax,-0x8(%bp)
	mov    0x6(%bp),%di
L_209b:
	mov    0x8(%bp),%ax
	decw   0x8(%bp)
	or     %ax,%ax
	je   L_20f9
	mov    (%di),%al
	cbtw
	add    -0x6(%bp),%ax
	and    $0xff,%ax
	add    %si,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	add    -0x8(%bp),%ax
	and    $0xff,%ax
	add    -0xc(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	sub    -0x8(%bp),%ax
	and    $0xff,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	sub    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%di)
	incw   -0x6(%bp)
	cmpw   $0x100,-0x6(%bp)
	jne   L_20f6
	movw   $0x0,-0x6(%bp)
	incw   -0x8(%bp)
	cmpw   $0x100,-0x8(%bp)
	jne   L_20f6
	movw   $0x0,-0x8(%bp)
L_20f6:
	inc    %di
	jmp   L_209b
L_20f9:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2100:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xa,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	mov    %ax,-0xe(%bp)
	lea    -0xa(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_2c85
	add    $0x4,%sp
	cmp    $0xffff,%ax
	jne   L_2133
	mov    $D_0084,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_2133:
	mov    -0x6(%bp),%ax
	mov    %ax,-0xc(%bp)
	andw   $0xfff7,-0x6(%bp)
	lea    -0xa(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_2c6c
	add    $0x4,%sp
	mov    $D_0092,%ax
	push   %ax
	call   L_1f5e
	pop    %cx
	mov    $D_0766,%di
L_2156:
	call   L_0aa1
	mov    %ax,%si
	cmp    $0xffff,%ax
	je   L_2174
	cmp    $0xa,%si
	je   L_2174
	cmp    $D_076f,%di
	jae   L_2156
	mov    %di,%bx
	inc    %di
	mov    %si,%cx
	mov    %cl,(%bx)
	jmp   L_2156
L_2174:
	movb   $0x0,(%di)
	mov    -0xc(%bp),%ax
	mov    %ax,-0x6(%bp)
	lea    -0xa(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_2c6c
	add    $0x4,%sp
	pushw  -0xe(%bp)
	mov    $0x2,%ax
	push   %ax
	call   L_26d8
	add    $0x4,%sp
	cmpb   $0x0,D_0766
	je   L_21a3
	mov    $0x1,%ax
	jmp   L_21a5
L_21a3:
	xor    %ax,%ax
L_21a5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_21ac:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2a,%sp
	mov    0x6(%bp),%di
	mov    %di,%ax
	add    $0x100,%ax
	mov    %ax,%si
	mov    0x6(%bp),%ax
	add    $0x200,%ax
	mov    %ax,-0x6(%bp)
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	jne   L_21d4
	xor    %ax,%ax
	jmp   L_2465
L_21d4:
	mov    $0x8,%ax
	push   %ax
	pushw  0x4(%bp)
	lea    -0x21(%bp),%ax
	push   %ax
	call   L_2652
	add    $0x6,%sp
L_21e5:
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_21f8
	mov    0x4(%bp),%bx
	movb   $0x0,(%bx)
	incw   0x4(%bp)
	jmp   L_21e5
L_21f8:
	mov    -0x21(%bp),%al
	mov    %al,-0x19(%bp)
	mov    -0x20(%bp),%al
	mov    %al,-0x18(%bp)
	lea    -0x12(%bp),%ax
	push   %ax
	call   L_25b2
	pop    %cx
	or     %ax,%ax
	jge   L_221a
	movw   $0xffff,-0x10(%bp)
	movw   $0xffff,-0x12(%bp)
L_221a:
	call   L_26c8
	or     %ax,%ax
	jne   L_2268
	xor    %ax,%ax
	push   %ax
	call   L_2c28
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	call   L_2c28
	pop    %cx
	pushw  -0x12(%bp)
	call   L_24f0
	pop    %cx
	pushw  -0x10(%bp)
	call   L_24f0
	pop    %cx
	xor    %ax,%ax
	push   %ax
	mov    $D_00a8,%ax
	push   %ax
	mov    $D_0097,%ax
	push   %ax
	call   L_26aa
	add    $0x6,%sp
	xor    %ax,%ax
	push   %ax
	mov    $D_00b7,%ax
	push   %ax
	mov    $D_00aa,%ax
	push   %ax
	call   L_26aa
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_2268:
	mov    $0xa,%ax
	push   %ax
	lea    -0x21(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	call   L_2ce8
	add    $0x6,%sp
	xor    %ax,%ax
	push   %ax
	call   L_2ccc
	pop    %cx
	cmp    $0xffff,%ax
	je   L_229b
	mov    $0xd,%ax
	push   %ax
	lea    -0x21(%bp),%ax
	push   %ax
	pushw  -0x12(%bp)
	call   L_2c52
	add    $0x6,%sp
	cmp    $0xd,%ax
	je   L_22a3
L_229b:
	mov    $D_00b9,%ax
	push   %ax
	call   L_0a06
	pop    %cx
L_22a3:
	pushw  -0x12(%bp)
	call   L_2c28
	pop    %cx
	pushw  -0x10(%bp)
	call   L_2c28
	pop    %cx
	movw   $0x7b,-0x26(%bp)
	movw   $0x0,-0x24(%bp)
	movw   $0x0,-0x8(%bp)
L_22c0:
	cmpw   $0xd,-0x8(%bp)
	jge   L_2307
	lea    -0x21(%bp),%ax
	add    -0x8(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	cwtd
	push   %dx
	push   %ax
	pushw  -0x24(%bp)
	pushw  -0x26(%bp)
	call   L_2837
	add    $0x8,%sp
	mov    %ax,-0x2a(%bp)
	mov    %dx,-0x28(%bp)
	mov    -0x8(%bp),%ax
	cwtd
	mov    %ax,-0x2e(%bp)
	mov    %dx,-0x2c(%bp)
	mov    -0x2a(%bp),%ax
	mov    -0x28(%bp),%dx
	add    -0x2e(%bp),%ax
	adc    -0x2c(%bp),%dx
	mov    %ax,-0x26(%bp)
	mov    %dx,-0x24(%bp)
	incw   -0x8(%bp)
	jmp   L_22c0
L_2307:
	movw   $0x0,-0x8(%bp)
L_230c:
	cmpw   $0x100,-0x8(%bp)
	jge   L_2331
	mov    %di,%ax
	add    -0x8(%bp),%ax
	mov    %ax,%bx
	mov    -0x8(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    -0x6(%bp),%ax
	add    -0x8(%bp),%ax
	mov    %ax,%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_230c
L_2331:
	movw   $0x0,-0x8(%bp)
L_2336:
	cmpw   $0x100,-0x8(%bp)
	jl   L_2340
	jmp   L_2439
L_2340:
	mov    -0x8(%bp),%ax
	cwtd
	mov    $0xd,%cx
	idiv   %cx
	lea    -0x21(%bp),%ax
	add    %dx,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	cwtd
	mov    %ax,-0x2a(%bp)
	mov    %dx,-0x28(%bp)
	mov    $0x5,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x24(%bp)
	pushw  -0x26(%bp)
	call   L_2837
	add    $0x8,%sp
	add    -0x2a(%bp),%ax
	adc    -0x28(%bp),%dx
	mov    %ax,-0x26(%bp)
	mov    %dx,-0x24(%bp)
	mov    $0xfff1,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x24(%bp)
	pushw  -0x26(%bp)
	call   L_2931
	add    $0x8,%sp
	mov    %ax,-0x14(%bp)
	mov    $0xff,%ax
	sub    -0x8(%bp),%ax
	mov    %ax,-0xc(%bp)
	inc    %ax
	mov    %ax,-0x2a(%bp)
	mov    -0x14(%bp),%ax
	and    $0xff,%ax
	xor    %dx,%dx
	divw   -0x2a(%bp)
	mov    %dx,-0xa(%bp)
	mov    $0x8,%cx
	shrw   %cl,-0x14(%bp)
	mov    %di,%ax
	add    -0xc(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,-0xe(%bp)
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %di,%dx
	add    -0xc(%bp),%dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	mov    -0xe(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    -0x6(%bp),%ax
	add    -0xc(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	jne   L_2433
	mov    -0x14(%bp),%ax
	and    $0xff,%ax
	xor    %dx,%dx
	divw   -0xc(%bp)
	mov    %dx,-0xa(%bp)
L_23fb:
	mov    -0x6(%bp),%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	je   L_2415
	mov    -0xa(%bp),%ax
	inc    %ax
	cwtd
	idivw  -0xc(%bp)
	mov    %dx,-0xa(%bp)
	jmp   L_23fb
L_2415:
	mov    -0x6(%bp),%ax
	add    -0xc(%bp),%ax
	mov    %ax,%bx
	mov    -0xa(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    -0x6(%bp),%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	mov    -0xc(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
L_2433:
	incw   -0x8(%bp)
	jmp   L_2336
L_2439:
	movw   $0x0,-0x8(%bp)
L_243e:
	cmpw   $0x100,-0x8(%bp)
	jge   L_2462
	mov    %di,%ax
	add    -0x8(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	add    %si,%ax
	mov    %ax,%bx
	mov    -0x8(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_243e
L_2462:
	mov    $0x1,%ax
L_2465:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_246c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xe,%sp
	xor    %di,%di
L_2476:
	cmp    $0x9,%di
	jge   L_2492
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	cbtw
	lea    -0x12(%bp),%dx
	add    %di,%dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   0x4(%bp)
	inc    %di
	jmp   L_2476
L_2492:
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_2816
	pop    %cx
	call   L_258c
	cwtd
	add    %ax,-0x8(%bp)
	adc    %dx,-0x6(%bp)
	xor    %di,%di
L_24a6:
	cmp    $0x4,%di
	jge   L_24db
	mov    %di,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	cwtd
	mov    -0x8(%bp),%bx
	mov    -0x6(%bp),%si
	mov    %ax,%cx
	or     %cx,%cx
	je   L_24c5
L_24bf:
	sar    %si
	rcr    %bx
	loop   L_24bf
L_24c5:
	and    $0xff,%bx
	and    $0x0,%si
	lea    -0x12(%bp),%ax
	add    %di,%ax
	mov    %ax,%si
	mov    %bx,%cx
	xor    %cl,(%si)
	inc    %di
	jmp   L_24a6
L_24db:
	pushw  0x6(%bp)
	lea    -0x12(%bp),%ax
	push   %ax
	call   L_21ac
	add    $0x4,%sp
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_24f0:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall dup(41) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* dup -> __NR_dup */
	xchg	%ax,%bx
	mov	$__NR_dup,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2504
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2504:
	pop    %bp
	ret


L_2506:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	call   L_258c
	mov    %ax,%si
	mov    0x4(%bp),%di
L_2514:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_2514
	dec    %di
L_251d:
	dec    %di
	cmpb   $0x58,(%di)
	jne   L_2540
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
	jmp   L_251d
L_2540:
	inc    %di
	movw   $0x61,-0x6(%bp)
L_2546:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2576
	add    $0x4,%sp
	cmp    $0xffff,%ax
	je   L_256e
	cmpw   $0x7a,-0x6(%bp)
	jne   L_2562
	mov    $D_00d4,%ax
	jmp   L_2571
L_2562:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%di)
	incw   -0x6(%bp)
	jmp   L_2546
L_256e:
	mov    0x4(%bp),%ax
L_2571:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2576:
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
	jns    L_258a
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_258a:
	pop    %bp
	ret


L_258c:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getpid(20) */
	/* getpid -> __NR_getpid */
	mov	$__NR_getpid,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_259a
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_259a:
	pop    %bp
	ret


L_259c:
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
	jns    L_25b0
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_25b0:
	pop    %bp
	ret


L_25b2:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall pipe(42) */
	/* pipe -> __NR_pipe */
	xchg	%ax,%bx
	mov	$__NR_pipe,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_25c2
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_25c0:
	pop    %bp
	ret
L_25c2:
	mov    0x4(%bp),%cx
	mov    %cx,%bp
	mov    %ax,-0x70(%bp)
	mov    %dx,0x2(%bp)
	mov    $0x0,%ax
	jmp   L_25c0
	/* .byte 0 (alignment) */


L_25d4:
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


L_2602:
	push   %bp
	mov    %sp,%bp
	mov    0x4(%bp),%bx
	mov    0x6(%bp),%ax
	or     %ax,%ax
	jne   L_2610
	inc    %ax
L_2610:
	mov    (%bx),%cx
	jmp   L_261b
L_2614:
	mov    0x0(%bp),%bp
	or     %bp,%bp
	je   L_2626
L_261b:
	cmp    %cx,0x0(%bp)
	jne   L_2614
	mov    -0x2(%bp),%si
	mov    -0x4(%bp),%di
L_2626:
	mov    %cs,%cx
	mov    %ds,%dx
	cmp    %dx,%cx
	je   L_2645
	mov    0x6(%bx),%bp
	or     %bp,%bp
	je   L_2645
	mov    %bp,0x2
	mov    0x8(%bx),%cx
	mov    %cx,0x0(%bp)
	mov    0xa(%bx),%cx
	mov    %cx,0x2(%bp)
L_2645:
	mov    (%bx),%bp
	mov    0x2(%bx),%sp
	pop    %cx
	pop    %cx
	mov    0x4(%bx),%cx
	push   %cx
	ret
	/* .byte 0 (alignment) */


L_2652:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
L_266a:
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_26a0
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_269b
L_2683:
	incw   -0x6(%bp)
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_2696
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	jmp   L_2683
L_2696:
	mov    -0x8(%bp),%ax
	jmp   L_26a3
L_269b:
	incw   -0x6(%bp)
	jmp   L_266a
L_26a0:
	mov    -0x8(%bp),%ax
L_26a3:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_26aa:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall exece(59) */
	mov    0x4(%bp),%ax
	mov    D_00de,%cx
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
	jns    L_26c5
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_26c5:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_26c8:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall fork(2) */
	/* fork -> __NR_fork */
	mov	$__NR_fork,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_26d6
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_26d6:
	pop    %bp
	ret


L_26d8:
	push   %bp
	mov    %sp,%bp
	mov    $0x16,%cx
	mov    0x4(%bp),%ax
	or     %ax,%ax
	jle   L_271b
	cmp    $0x20,%ax
	jg   L_271b
	mov    %ax,%cx
	shl    %ax
	add    $D_140e,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    0x6(%bp),%ax
	mov    %ax,(%bx)
	cmp    $0x2,%ax
	jl   L_2707
	mov    $0x6,%ax
	mul    %cx
	add    $D_071e,%ax
L_2707:
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
	jl   L_2718
	pop    %ax
	jmp   L_2719
L_2718:
	pop    %bx
L_2719:
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2722
L_271b:
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	mov    $0xffff,%ax
L_2722:
	pop    %bp
	ret
L_2724:
	push   %ax
	mov    $D_1410,%ax
	jmp   L_2788
	push   %ax
	mov    $D_1412,%ax
	jmp   L_2788
	push   %ax
	mov    $D_1414,%ax
	jmp   L_2788
	push   %ax
	mov    $D_1416,%ax
	jmp   L_2788
	push   %ax
	mov    $D_1418,%ax
	jmp   L_2788
	push   %ax
	mov    $D_141a,%ax
	jmp   L_2788
	push   %ax
	mov    $D_141c,%ax
	jmp   L_2788
	push   %ax
	mov    $D_141e,%ax
	jmp   L_2788
	push   %ax
	mov    $D_1420,%ax
	jmp   L_2788
	push   %ax
	mov    $D_1422,%ax
	jmp   L_2788
	push   %ax
	mov    $D_1424,%ax
	jmp   L_2788
	push   %ax
	mov    $D_1426,%ax
	jmp   L_2788
	push   %ax
	mov    $D_1428,%ax
	jmp   L_2788
	push   %ax
	mov    $D_142a,%ax
L_2776:
	jmp   L_2788
	push   %ax
	mov    $D_142c,%ax
L_277c:
	jmp   L_2788
	push   %ax
	mov    $D_142e,%ax
	jmp   L_2788
	push   %ax
	mov    $D_1430,%ax
L_2788:
	jmp   L_27e2
	push   %ax
	mov    $D_1432,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_1434,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_1436,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_1438,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_143a,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_143c,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_143e,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_1440,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_1442,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_1444,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_1446,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_1448,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_144a,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_144c,%ax
	jmp   L_27e2
	push   %ax
	mov    $D_144e,%ax
L_27e2:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	sub    $D_140e,%ax
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


L_27f8:
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
L_2813:
	pop    %si
	pop    %bp
	ret


L_2816:
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
	jns    L_2826
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_2832
L_2826:
	mov    0x4(%bp),%bx
	or     %bx,%bx
	je   L_2832
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_2832:
	pop    %bp
	ret
	call   L_299c


L_2837:
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
	call   L_299c


L_2860:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_287e
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_287e:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_2887:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_2887
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_2897:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_2897
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_2837
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_28d8
	jne   L_28cd
	or     $0x0,%ax
	je   L_28d8
L_28cd:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_28d8:
	pop    %ax
	mov    $0x0,%dx
	ret


L_28dd:
	call   L_299c


L_28e0:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_28fc
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_28fc:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_2913
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_2913:
	push   %dx
	push   %ax
	call   L_2860
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_292d
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_292d:
	ret
	call   L_299c


L_2931:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_28e0
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_2837
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_299c
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_2860
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_2837
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_299c:
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

	/* _exit: provided by TLVC libc — elided */

L_29d2:
	ret
	/* .byte 0 (alignment) */


L_29d4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_00d6
	jne   L_29ff
	mov    $D_00d8,%ax
	or     $0x1,%ax
	mov    %ax,D_00d6
	mov    $D_00d6,%ax
	or     $0x1,%ax
	mov    %ax,D_00d8
	/* dual-reloc split: movw $D_00d8,D_1452 */
	mov	$D_00d8,%ax
	mov	%ax,D_1452
	/* dual-reloc split: movw $D_00d6,D_1450 */
	mov	$D_00d6,%ax
	mov	%ax,D_1450
L_29ff:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_1450,%di
L_2a13:
	movw   $0x0,D_00da
L_2a19:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_2a48
L_2a20:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_2a2f
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_2a20
L_2a2f:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_2a48
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_2a48
	jmp   L_2aef
L_2a48:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_2a19
	cmp    D_1452,%si
	jne   L_2a61
	cmp    $D_00d6,%di
	je   L_2a65
L_2a61:
	xor    %ax,%ax
	jmp   L_2a9d
L_2a65:
	incw   D_00da
	cmpw   $0x1,D_00da
	jle   L_2a19
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_00da
	xor    %ax,%ax
	push   %ax
	call   L_2c02
	pop    %cx
	mov    %ax,%si
	mov    D_00da,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_2a9f
	xor    %ax,%ax
L_2a9d:
	jmp   L_2ab2
L_2a9f:
	mov    D_00da,%ax
	shl    %ax
	push   %ax
	call   L_2c02
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_2ab4
	xor    %ax,%ax
L_2ab2:
	jmp   L_2b19
L_2ab4:
	mov    D_1452,%bx
	mov    %si,(%bx)
	mov    D_1452,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_2ad2
	mov    D_1452,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_1452,%bx
	mov    %ax,(%bx)
L_2ad2:
	mov    D_00da,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_1452
	mov    $D_00d6,%ax
	or     $0x1,%ax
	mov    D_1452,%bx
	mov    %ax,(%bx)
	jmp   L_2a13
L_2aef:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_1450
	cmp    %ax,%si
	jbe   L_2b0e
	mov    D_1450,%bx
	mov    (%bx),%ax
	mov    %ax,D_1454
	mov    D_1450,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_2b0e:
	mov    D_1450,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_2b19:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2b1e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_1450
	mov    (%si),%ax
	and    $0xfffe,%ax
	mov    %ax,(%si)
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2b3a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%di
	mov    -0x2(%di),%ax
	test   $0x1,%ax
	je   L_2b53
	push   %di
	call   L_2b1e
	pop    %cx
L_2b53:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_29d4
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_2b72
	cmp    %di,%si
	jne   L_2b76
L_2b72:
	mov    %si,%ax
	jmp   L_2bde
L_2b76:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_2b95
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_2b95:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_2bb3
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_2b95
L_2bb3:
	cmp    %di,%si
	jae   L_2bdc
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_2bdc
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
	mov    D_1454,%ax
	mov    %ax,(%bx)
L_2bdc:
	mov    %si,%ax
L_2bde:
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
	jns    L_2bf9
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_2c00
L_2bf9:
	mov    0x4(%bp),%bx
	mov    %bx,D_00dc
L_2c00:
	pop    %bp
	ret


L_2c02:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_00dc,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2c19
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_2c26
L_2c19:
	mov    D_00dc,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_00dc
L_2c26:
	pop    %bp
	ret


L_2c28:
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
	jns    L_2c39
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2c39:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2c3c:
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
	jns    L_2c50
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2c50:
	pop    %bp
	ret


L_2c52:
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
	jns    L_2c69
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2c69:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2c6c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7409,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2c9e
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2c85:
	push   %bp
L_2c86:
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2c9e
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2c9e:
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
	jns    L_2cb5
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2cb5:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2cb8:
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
	jns    L_2cc9
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2cc9:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2ccc:
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
	jns    L_2cdc
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2cda:
	pop    %bp
	ret
L_2cdc:
	mov    0x4(%bp),%bx
	cmp    $0x0,%bx
	je   L_2cda
	mov    %dx,(%bx)
	jmp   L_2cda


L_2ce8:
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
	jns    L_2cff
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2cff:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2d02:
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
	jns    L_2d13
	neg	%ax
	mov	%ax,D_140c	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2d13:
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
	mov	%ax,D_140c	/* errno */
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
	.byte	0x00, 0x00	/* |..| */
D_0006:
	.byte	0x54, 0x4d, 0x50, 0x00	/* |TMP.| */
D_000a:
	.byte	0x01, 0x00	/* |..| */
D_000c:
	.byte	0xff, 0xff	/* |..| */
D_000e:
	.byte	0xff, 0xff	/* |..| */
D_0010:
	.byte	0xff, 0xff	/* |..| */
D_0012:
	.ascii	"WRITE ERROR\0"
D_001e:
	.byte	0x80, 0x00, 0x20, 0x1d, 0xa7, 0x1c, 0x00, 0x1e	/* |.. .....| */
	.byte	0xc1, 0x1c, 0xf3, 0x1d, 0xdf, 0x1c, 0x19, 0x1e	/* |........| */
	.byte	0x00, 0x1d, 0x19, 0x1e, 0xcd, 0x1c, 0xd6, 0x1c	/* |........| */
	.byte	0x30, 0x1d, 0x62, 0x1e, 0x40, 0x1d, 0x84, 0x1d	/* |0.b.@...| */
D_003e:
	.word	D_0c86	/* ptr: 0x2c86 */
D_0040:
	.byte	0x72, 0x00	/* |r.| */
D_0042:
	.ascii	"/tmp/eXXXXX\0"
D_004e:
	.ascii	"Entering encrypting mode!\0"
D_0068:
	.ascii	"ed.hup\0"
D_006f:
	.ascii	"MEM?\0"
D_0074:
	.ascii	"/bin/sh\0"
D_007c:
	.byte	0x73, 0x68	/* |sh| */
	.byte	0x00	/* |.| */
D_007f:
	.byte	0x2d	/* |-| */
	.word	D_0074	/* ptr: 0x0074 */
D_0082:
	.byte	0x21, 0x00	/* |!.| */
D_0084:
	.ascii	"Input not tty\0"
D_0092:
	.ascii	"Key:\0"
D_0097:
	.ascii	"/usr/lib/makekey\0"
D_00a8:
	.byte	0x2d, 0x00	/* |-.| */
D_00aa:
	.ascii	"/lib/makekey\0"
D_00b7:
	.byte	0x2d, 0x00	/* |-.| */
D_00b9:
	.ascii	"crypt: cannot generate key\0"
D_00d4:
	.byte	0x2f, 0x00	/* |/.| */
D_00d6:
	.byte	0x00, 0x00	/* |..| */
D_00d8:
	.byte	0x00, 0x00	/* |..| */
D_00da:
	.byte	0x00, 0x00	/* |..| */
D_00dc:
	.word	D_1456	/* ptr: 0x3456 */

	.bss

D_00de:	.space	2	/* bss word @ 0x00de */
D_00e0:	.space	2	/* bss word @ 0x00e0 */
D_00e2:	.space	2	/* bss word @ 0x00e2 */
	.space	2
D_00e6:	.space	2	/* bss word @ 0x00e6 */
	.space	10
D_00f2:	.space	2	/* bss word @ 0x00f2 */
D_00f4:	.space	2	/* bss word @ 0x00f4 */
D_00f6:	.space	2	/* bss word @ 0x00f6 */
	.space	254
D_01f6:	.space	2	/* bss word @ 0x01f6 */
	.space	6
D_01fe:	.space	2	/* bss word @ 0x01fe */
	.space	62
D_023e:	.space	2	/* bss word @ 0x023e */
	.space	6
D_0246:	.space	2	/* bss word @ 0x0246 */
	.space	510
D_0446:	.space	2	/* bss word @ 0x0446 */
	.space	126
D_04c6:	.space	2	/* bss word @ 0x04c6 */
	.space	6
D_04ce:	.space	2	/* bss word @ 0x04ce */
	.space	50
D_0502:	.space	2	/* bss word @ 0x0502 */
D_0504:	.space	2	/* bss word @ 0x0504 */
D_0506:	.space	2	/* bss word @ 0x0506 */
D_0508:	.space	2	/* bss word @ 0x0508 */
	.space	4
D_050e:	.space	2	/* bss word @ 0x050e */
	.space	510
D_070e:	.space	2	/* bss word @ 0x070e */
D_0710:	.space	2	/* bss word @ 0x0710 */
D_0712:	.space	2	/* bss word @ 0x0712 */
	.space	2
D_0716:	.space	2	/* bss word @ 0x0716 */
	.space	6
D_071e:	.space	2	/* bss word @ 0x071e */
	.space	54
D_0756:	.space	2	/* bss word @ 0x0756 */
	.space	14
D_0766:	.space	2	/* bss word @ 0x0766 */
	.space	7
D_076f:	.space	2	/* bss word @ 0x076f */
D_0770:	.space	2	/* bss word @ 0x0770 */
	.space	4
D_0776:	.space	2	/* bss word @ 0x0776 */
	.space	508
D_0974:	.space	2	/* bss word @ 0x0974 */
D_0976:	.space	2	/* bss word @ 0x0976 */
D_0978:	.space	2	/* bss word @ 0x0978 */
	.space	4
D_097e:	.space	2	/* bss word @ 0x097e */
	.space	722
D_0c52:	.space	2	/* bss word @ 0x0c52 */
	.space	42
D_0c7e:	.space	2	/* bss word @ 0x0c7e */
	.space	6
D_0c86:	.space	2	/* bss word @ 0x0c86 */
	.space	62
D_0cc6:	.space	2	/* bss word @ 0x0cc6 */
	.space	4
D_0ccc:	.space	2	/* bss word @ 0x0ccc */
D_0cce:	.space	2	/* bss word @ 0x0cce */
D_0cd0:	.space	2	/* bss word @ 0x0cd0 */
	.space	4
D_0cd6:	.space	2	/* bss word @ 0x0cd6 */
	.space	8
D_0ce0:	.space	2	/* bss word @ 0x0ce0 */
D_0ce2:	.space	2	/* bss word @ 0x0ce2 */
D_0ce4:	.space	2	/* bss word @ 0x0ce4 */
D_0ce6:	.space	2	/* bss word @ 0x0ce6 */
D_0ce8:	.space	2	/* bss word @ 0x0ce8 */
D_0cea:	.space	2	/* bss word @ 0x0cea */
D_0cec:	.space	2	/* bss word @ 0x0cec */
D_0cee:	.space	2	/* bss word @ 0x0cee */
D_0cf0:	.space	2	/* bss word @ 0x0cf0 */
	.space	4
D_0cf6:	.space	2	/* bss word @ 0x0cf6 */
	.space	711
D_0fbf:	.space	2	/* bss word @ 0x0fbf */
	.space	53
D_0ff6:	.space	2	/* bss word @ 0x0ff6 */
D_0ff8:	.space	2	/* bss word @ 0x0ff8 */
	.space	4
D_0ffe:	.space	2	/* bss word @ 0x0ffe */
D_0fff:	.space	2	/* bss word @ 0x0fff */
	.space	507
D_11fc:	.space	2	/* bss word @ 0x11fc */
D_11fe:	.space	2	/* bss word @ 0x11fe */
D_1200:	.space	2	/* bss word @ 0x1200 */
	.space	4
D_1206:	.space	2	/* bss word @ 0x1206 */
	.space	510
D_1406:	.space	2	/* bss word @ 0x1406 */
D_1408:	.space	2	/* bss word @ 0x1408 */
D_140a:	.space	2	/* bss word @ 0x140a */
D_140c:	.space	2	/* bss word @ 0x140c */
D_140e:	.space	2	/* bss word @ 0x140e */
D_1410:	.space	2	/* bss word @ 0x1410 */
D_1412:	.space	2	/* bss word @ 0x1412 */
D_1414:	.space	2	/* bss word @ 0x1414 */
D_1416:	.space	2	/* bss word @ 0x1416 */
D_1418:	.space	2	/* bss word @ 0x1418 */
D_141a:	.space	2	/* bss word @ 0x141a */
D_141c:	.space	2	/* bss word @ 0x141c */
D_141e:	.space	2	/* bss word @ 0x141e */
D_1420:	.space	2	/* bss word @ 0x1420 */
D_1422:	.space	2	/* bss word @ 0x1422 */
D_1424:	.space	2	/* bss word @ 0x1424 */
D_1426:	.space	2	/* bss word @ 0x1426 */
D_1428:	.space	2	/* bss word @ 0x1428 */
D_142a:	.space	2	/* bss word @ 0x142a */
D_142c:	.space	2	/* bss word @ 0x142c */
D_142e:	.space	2	/* bss word @ 0x142e */
D_1430:	.space	2	/* bss word @ 0x1430 */
D_1432:	.space	2	/* bss word @ 0x1432 */
D_1434:	.space	2	/* bss word @ 0x1434 */
D_1436:	.space	2	/* bss word @ 0x1436 */
D_1438:	.space	2	/* bss word @ 0x1438 */
D_143a:	.space	2	/* bss word @ 0x143a */
D_143c:	.space	2	/* bss word @ 0x143c */
D_143e:	.space	2	/* bss word @ 0x143e */
D_1440:	.space	2	/* bss word @ 0x1440 */
D_1442:	.space	2	/* bss word @ 0x1442 */
D_1444:	.space	2	/* bss word @ 0x1444 */
D_1446:	.space	2	/* bss word @ 0x1446 */
D_1448:	.space	2	/* bss word @ 0x1448 */
D_144a:	.space	2	/* bss word @ 0x144a */
D_144c:	.space	2	/* bss word @ 0x144c */
D_144e:	.space	2	/* bss word @ 0x144e */
D_1450:	.space	2	/* bss word @ 0x1450 */
D_1452:	.space	2	/* bss word @ 0x1452 */
D_1454:	.space	2	/* bss word @ 0x1454 */
D_1456:	/* endcore: initial break */

/* end of file */
