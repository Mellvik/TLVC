/* tar.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 tar.s -o tar.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: tar  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x0000  (0 bytes)
//   a_text:   0x00004d4e  (19790 bytes)
//   a_data:   0x00000d60  (3424 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	cmpw   $0x2,0x4(%bp)
	jge   L_0047
	call   L_0652
L_0047:
	/* TODO: data ref: movw   $0x0,0x37b8 */
	movw   $0x0,D_37b8
	movw   $0x26,0xf86
	mov    0x4(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	addw   $0x2,0x6(%bp)
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x6(%bp)
	addw   $0x2,0x6(%bp)
L_0071:
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	jne   L_007c
	jmp   L_030e
L_007c:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	jmp   L_0251
L_0085:
	/* TODO: data ref: incw   0x37ae */
	incw   D_37ae
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,0xf86
	addw   $0x2,0x6(%bp)
	cmpw   $0x1,0x12
	jne   L_00ac
	movw   $0x0,0x12
	jmp   L_00ac
L_00a4:
	/* TODO: data ref: incw   0x37a4 */
	incw   D_37a4
	/* TODO: data ref: incw   0x37a0 */
	incw   D_37a0
L_00ac:
	jmp   L_011b
L_00ae:
	mov    $0x16,%ax
	push   %ax
	call   L_27d4
	pop    %cx
	mov    $0x94,%ax
	push   %ax
	mov    $0x16,%ax
	push   %ax
	call   L_286a
	add    $0x4,%sp
	/* TODO: data ref: mov    %ax,0x37b8 */
	mov    %ax,D_37b8
	or     %ax,%ax
	jne   L_00e5
	mov    $0x16,%ax
	push   %ax
	mov    $0x96,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_00e5:
	mov    $0xbe,%ax
	push   %ax
	/* TODO: data ref: pushw  0x37b8 */
	pushw  D_37b8
	call   L_270c
	add    $0x4,%sp
L_00f3:
	/* TODO: data ref: incw   0x37a0 */
	incw   D_37a0
	cmpw   $0x1,0x12
	je   L_0148
	/* TODO: data ref: cmpw   $0x0,0x37a4 */
	cmpw   $0x0,D_37a4
	jne   L_0148
L_0105:
	mov    $0xd7,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_011b:
	jmp   L_016d
L_011d:
	/* TODO: data ref: incw   0x37b0 */
	incw   D_37b0
	jmp   L_017d
L_0123:
	/* TODO: data ref: incw   0x37b2 */
	incw   D_37b2
	jmp   L_017d
L_0129:
	/* TODO: data ref: incw   0x37ba */
	incw   D_37ba
	jmp   L_017d
L_012f:
	/* TODO: data ref: incw   0x37ac */
	incw   D_37ac
	jmp   L_017d
L_0135:
	incw   0xf88
	jmp   L_017d
L_013b:
	/* TODO: data ref: cmpw   $0x0,0x37ae */
	cmpw   $0x0,D_37ae
	je   L_017d
	/* TODO: data ref: movw   $0xffff,0x37ae */
	/* movw constant -> D_37ae */
	mov	$0xffff,%ax
	mov	%ax,D_37ae
L_0148:
	jmp   L_017d
L_014a:
	/* TODO: data ref: cmpw   $0x0,0x37ae */
	cmpw   $0x0,D_37ae
	je   L_016f
	/* TODO: data ref: mov    0x37c0,%ax */
	mov    D_37c0,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    -0x6(%bp),%bx
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    $0x30,%dx
	add    %dx,%ax
	/* TODO: data ref: mov    %ax,0x37c0 */
	mov    %ax,D_37c0
L_016d:
	jmp   L_017d
L_016f:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	mov    %al,0x2c
	movw   $0x26,0xf86
L_017d:
	jmp   L_01fe
L_017f:
	mov    0x6(%bp),%bx
	addw   $0x2,0x6(%bp)
	pushw  (%bx)
	call   L_2738
	pop    %cx
	mov    %ax,0x12
	cmp    $0x14,%ax
	jg   L_019b
	cmpw   $0x0,0x12
	jg   L_01b5
L_019b:
	mov    $0x14,%ax
	push   %ax
	mov    $0x103,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_01b5:
	/* TODO: data ref: cmpw   $0x0,0x37a0 */
	cmpw   $0x0,D_37a0
	je   L_0204
	/* TODO: data ref: cmpw   $0x0,0x37a4 */
	cmpw   $0x0,D_37a4
	jne   L_0204
	jmp   L_0105
L_01c6:
	mov    0x6(%bp),%bx
	addw   $0x2,0x6(%bp)
	pushw  (%bx)
	call   L_20f4
	pop    %cx
	mov    %ax,0x4
	mov    %dx,0x6
	cmp    $0x0,%dx
	jg   L_0204
	jl   L_01e8
	cmpw   $0x3,0x4
	ja   L_0204
L_01e8:
	mov    $0x125,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_01fe:
	jmp   L_0234
L_0200:
	/* TODO: data ref: incw   0x37a2 */
	incw   D_37a2
L_0204:
	jmp   L_0234
L_0206:
	mov    $0xf8c,%ax
	push   %ax
	call   L_3984
	pop    %cx
	mov    $0x5180,%ax
	mov    $0x1,%dx
	push   %dx
	push   %ax
	mov    0x6(%bp),%bx
	addw   $0x2,0x6(%bp)
	pushw  (%bx)
	call   L_2738
	pop    %cx
	cwtd
	push   %dx
	push   %ax
	call   L_3fe9
	add    $0x8,%sp
	sub    %ax,0xf8c
	sbb    %dx,0xf8e
L_0234:
	jmp   L_024e
L_0236:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	push   %ax
	mov    $0x13f,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	call   L_0652
L_024e:
	jmp   L_0308
L_0251:
	cmp    $0x62,%ax
	jne   L_0259
	jmp   L_017f
L_0259:
	jg   L_029e
	cmp    $0x34,%ax
L_025e:
	jne   L_0263
	jmp   L_014a
L_0263:
	jg   L_0283
	cmp    $0x2d,%ax
	jne   L_026d
	jmp   L_013b
L_026d:
	cmp    $0x30,%ax
	je   L_025e
	cmp    $0x31,%ax
	je   L_025e
	cmp    $0x32,%ax
	je   L_025e
	cmp    $0x33,%ax
	je   L_025e
L_0281:
	jmp   L_0236
L_0283:
	cmp    $0x35,%ax
	je   L_025e
	cmp    $0x36,%ax
	je   L_025e
	cmp    $0x37,%ax
	je   L_025e
	cmp    $0x38,%ax
	je   L_025e
	cmp    $0x39,%ax
	je   L_025e
L_029c:
	jmp   L_0281
L_029e:
	cmp    $0x72,%ax
	jne   L_02a6
	jmp   L_00f3
L_02a6:
	jg   L_02d2
	cmp    $0x63,%ax
	jne   L_02b0
	jmp   L_00a4
L_02b0:
	cmp    $0x64,%ax
	jne   L_02b8
	jmp   L_0206
L_02b8:
	cmp    $0x66,%ax
	jne   L_02c0
	jmp   L_0085
L_02c0:
	cmp    $0x6c,%ax
	jne   L_02c8
	jmp   L_0200
L_02c8:
	cmp    $0x6d,%ax
	jne   L_02d0
	jmp   L_0135
L_02d0:
	jmp   L_0281
L_02d2:
	cmp    $0x75,%ax
	jne   L_02da
	jmp   L_00ae
L_02da:
	jg   L_02ee
	cmp    $0x73,%ax
	jne   L_02e4
	jmp   L_01c6
L_02e4:
	cmp    $0x74,%ax
	jne   L_02ec
	jmp   L_012f
L_02ec:
	jmp   L_029c
L_02ee:
	cmp    $0x76,%ax
	jne   L_02f6
	jmp   L_011d
L_02f6:
	cmp    $0x77,%ax
	jne   L_02fe
	jmp   L_0123
L_02fe:
	cmp    $0x78,%ax
	jne   L_0306
	jmp   L_0129
L_0306:
	jmp   L_029c
L_0308:
	incw   -0x6(%bp)
	jmp   L_0071
L_030e:
	/* TODO: data ref: cmpw   $0x0,0x37a0 */
	cmpw   $0x0,D_37a0
	jne   L_0318
	jmp   L_048d
L_0318:
	/* TODO: data ref: cmpw   $0x0,0x37a4 */
	cmpw   $0x0,D_37a4
	je   L_0331
	/* TODO: data ref: cmpw   $0x0,0x37b8 */
	cmpw   $0x0,D_37b8
	je   L_0331
	call   L_0652
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_0331:
	cmpw   $0x0,0x12
	jne   L_033e
	movw   $0x1,0x12
L_033e:
	mov    $0x14,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  0x6
	pushw  0x4
	call   L_40e3
	add    $0x8,%sp
	add    $0x2,%ax
	adc    $0x0,%dx
	sub    %ax,0x4
	sbb    %dx,0x6
	cmpw   $0x0,0x6
	jg   L_0387
	jl   L_0371
	cmpw   $0x0,0x4
	ja   L_0387
L_0371:
	mov    $0x158,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_0387:
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	cmp    $0x1,%ax
	je   L_03a8
	/* TODO: data ref: mov    $0x1609,%ax */
	mov    $D_1609,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
L_03a8:
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	cmp    $0x1,%ax
	je   L_03c9
	/* TODO: data ref: mov    $0x163f,%ax */
	mov    $D_163f,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
L_03c9:
	mov    $0x1,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	cmp    $0x1,%ax
	je   L_03ea
	/* TODO: data ref: mov    $0x1624,%ax */
	mov    $D_1624,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
L_03ea:
	mov    $0x174,%ax
	push   %ax
	pushw  0xf86
	call   L_4ae6
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0438
	/* TODO: data ref: cmpw   $0x0,0x37a4 */
	cmpw   $0x0,D_37a4
	jne   L_0419
	mov    $0x176,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_0419:
	mov    $0x1,%ax
	push   %ax
	call   L_2630
	pop    %cx
	/* TODO: data ref: mov    %ax,0x37c2 */
	mov    %ax,D_37c2
	movw   $0x1,0x12
	movw   $0xe100,0x4
	movw   $0x5f5,0x6
	jmp   L_0483
L_0438:
	mov    $0x2,%ax
	push   %ax
	pushw  0xf86
	call   L_4b7a
	add    $0x4,%sp
	/* TODO: data ref: mov    %ax,0x37c2 */
	mov    %ax,D_37c2
	or     %ax,%ax
	jge   L_0483
	/* TODO: data ref: cmpw   $0x0,0x37a4 */
	cmpw   $0x0,D_37a4
	je   L_0469
	mov    $0x1b6,%ax
	push   %ax
	pushw  0xf86
	call   L_2968
	add    $0x4,%sp
	/* TODO: data ref: mov    %ax,0x37c2 */
	mov    %ax,D_37c2
	or     %ax,%ax
	jge   L_0483
L_0469:
	pushw  0xf86
	mov    $0x1a0,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_0483:
	pushw  0x6(%bp)
	call   L_0671
	pop    %cx
	jmp   L_05d4
L_048d:
	/* TODO: data ref: cmpw   $0x0,0x37ba */
	cmpw   $0x0,D_37ba
	jne   L_0497
	jmp   L_05df
L_0497:
	mov    $0x1b5,%ax
	push   %ax
	pushw  0xf86
	call   L_4ae6
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_04bb
	xor    %ax,%ax
	push   %ax
	call   L_2630
	pop    %cx
	/* TODO: data ref: mov    %ax,0x37c2 */
	mov    %ax,D_37c2
	movw   $0x1,0x12
	jmp   L_04e9
L_04bb:
	xor    %ax,%ax
	push   %ax
	pushw  0xf86
	call   L_4b7a
	add    $0x4,%sp
	/* TODO: data ref: mov    %ax,0x37c2 */
	mov    %ax,D_37c2
	or     %ax,%ax
	jge   L_04e9
	pushw  0xf86
	mov    $0x1b7,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_04e9:
	/* TODO: data ref: cmpw   $0x0,0x37c0 */
	cmpw   $0x0,D_37c0
	jne   L_04f3
	jmp   L_05d6
L_04f3:
	/* TODO: data ref: cmpw   $0x0,0x37c0 */
	cmpw   $0x0,D_37c0
	jle   L_0501
	/* TODO: data ref: cmpw   $0x63,0x37c0 */
	cmpw   $0x63,D_37c0
	jle   L_0507
L_0501:
	/* TODO: data ref: movw   $0x1,0x37c0 */
	movw   $0x1,D_37c0
L_0507:
	call   L_3760
	/* TODO: data ref: cmpw   $0x0,0x37ae */
	cmpw   $0x0,D_37ae
	jge   L_051b
	mov    $0x1cc,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	jmp   L_0523
L_051b:
	mov    $0x20b,%ax
	push   %ax
	call   L_2b54
	pop    %cx
L_0523:
	mov    $0x248,%ax
	push   %ax
	call   L_2b54
	pop    %cx
L_052b:
	call   L_1f1f
	cmp    $0xa,%ax
	jne   L_052b
	mov    $0x284,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	movw   $0x1,-0x8(%bp)
L_0540:
	mov    -0x8(%bp),%ax
	/* TODO: data ref: cmp    0x37c0,%ax */
	cmp    D_37c0,%ax
	jle   L_054c
	jmp   L_05cc
L_054c:
	cmpw   $0x2,-0x8(%bp)
	jl   L_058c
	mov    -0x8(%bp),%ax
	dec    %ax
	/* TODO: data ref: cmpw   $0x0,0x37ae */
	cmpw   $0x0,D_37ae
	jge   L_0562
	mov    $0x41,%dx
	jmp   L_0565
L_0562:
	mov    $0x31,%dx
L_0565:
	add    %dx,%ax
	push   %ax
	mov    $0x295,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
L_0576:
	call   L_1f1f
	cmp    $0xa,%ax
	jne   L_0576
	mov    $0x2bb,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
L_058c:
	movw   $0x0,0xc
	movw   $0x0,0xe
	movw   $0x0,0x8
	movw   $0x0,0xa
	mov    0x12,%ax
	/* TODO: data ref: mov    %ax,0x37aa */
	mov    %ax,D_37aa
	xor    %ax,%ax
	push   %ax
	pushw  0xe
	pushw  0xc
	/* TODO: data ref: pushw  0x37c2 */
	pushw  D_37c2
	call   L_3966
	add    $0x8,%sp
	pushw  0x6(%bp)
	call   L_102f
	pop    %cx
	incw   -0x8(%bp)
	jmp   L_0540
L_05cc:
	mov    $0x2ce,%ax
	push   %ax
	call   L_2b54
	pop    %cx
L_05d4:
	jmp   L_05dd
L_05d6:
	pushw  0x6(%bp)
	call   L_102f
	pop    %cx
L_05dd:
	jmp   L_063f
L_05df:
	/* TODO: data ref: cmpw   $0x0,0x37ac */
	cmpw   $0x0,D_37ac
	je   L_0641
	mov    $0x2d0,%ax
	push   %ax
	pushw  0xf86
	call   L_4ae6
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_060a
	xor    %ax,%ax
	push   %ax
	call   L_2630
	pop    %cx
	/* TODO: data ref: mov    %ax,0x37c2 */
	mov    %ax,D_37c2
	movw   $0x1,0x12
	jmp   L_0638
L_060a:
	xor    %ax,%ax
	push   %ax
	pushw  0xf86
	call   L_4b7a
	add    $0x4,%sp
	/* TODO: data ref: mov    %ax,0x37c2 */
	mov    %ax,D_37c2
	or     %ax,%ax
	jge   L_0638
	pushw  0xf86
	mov    $0x2d2,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_0638:
	pushw  0x6(%bp)
	call   L_13a2
	pop    %cx
L_063f:
	jmp   L_0644
L_0641:
	call   L_0652
L_0644:
	xor    %ax,%ax
	push   %ax
	call   L_1811
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0652:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x2e7,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0671:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x104,%sp
	cmpw   $0x0,0xf8e
	jne   L_0686
	cmpw   $0x0,0xf8c
L_0686:
	je   L_069f
	mov    $0xf8c,%ax
	push   %ax
	call   L_21dc
	pop    %cx
	push   %ax
	mov    $0x31a,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
L_069f:
	/* TODO: data ref: cmpw   $0x0,0x37a4 */
	cmpw   $0x0,D_37a4
	je   L_06a9
	jmp   L_0739
L_06a9:
	call   L_082d
L_06ac:
	call   L_099c
	/* TODO: data ref: cmpw   $0x0,0x37a8 */
	cmpw   $0x0,D_37a8
	je   L_06bd
	xor    %ax,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_06bd:
	call   L_082d
	call   L_0813
	or     %ax,%ax
	je   L_06ac
	/* TODO: data ref: cmpw   $0x0,0x37b8 */
	cmpw   $0x0,D_37b8
	je   L_0739
	mov    $0x16,%ax
	push   %ax
	mov    $0x16,%ax
	push   %ax
	mov    $0x16,%ax
	push   %ax
	mov    $0x16,%ax
	push   %ax
	mov    $0x16,%ax
	push   %ax
	mov    $0x16,%ax
	push   %ax
	mov    $0x33e,%ax
	push   %ax
	lea    -0x108(%bp),%ax
	push   %ax
	call   L_39a2
	add    $0x10,%sp
	/* TODO: data ref: pushw  0x37b8 */
	pushw  D_37b8
	call   L_47ac
	pop    %cx
	lea    -0x108(%bp),%ax
	push   %ax
	call   L_3770
	pop    %cx
	/* TODO: data ref: pushw  0x37b8 */
	pushw  D_37b8
	mov    $0x38c,%ax
	push   %ax
	mov    $0x16,%ax
	push   %ax
	call   L_2646
	add    $0x6,%sp
	mov    $0xf68,%ax
	push   %ax
	/* TODO: data ref: mov    0x37b8,%bx */
	mov    D_37b8,%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_4b3c
	add    $0x4,%sp
	mov    0xf76,%ax
	mov    0xf78,%dx
	/* TODO: data ref: mov    %ax,0x37b4 */
	mov    %ax,D_37b4
	/* TODO: data ref: mov    %dx,0x37b6 */
	mov    %dx,D_37b6
L_0739:
	lea    -0x40(%bp),%ax
	push   %ax
	call   L_1890
	pop    %cx
L_0741:
	mov    0x4(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_07b1
	/* TODO: data ref: cmpw   $0x0,0x37a8 */
	cmpw   $0x0,D_37a8
	jne   L_07b1
	mov    0x4(%bp),%bx
	mov    (%bx),%si
	mov    0x4(%bp),%bx
	mov    (%bx),%di
L_075a:
	cmpb   $0x0,(%di)
	je   L_0769
	cmpb   $0x2f,(%di)
	jne   L_0766
	mov    %di,%si
L_0766:
	inc    %di
	jmp   L_075a
L_0769:
	mov    0x4(%bp),%bx
	cmp    (%bx),%si
	je   L_0794
	movb   $0x0,(%si)
	mov    0x4(%bp),%bx
	pushw  (%bx)
	call   L_2198
	pop    %cx
	or     %ax,%ax
	jge   L_0790
	mov    $0x38e,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
L_078e:
	jmp   L_0741
L_0790:
	movb   $0x2f,(%si)
	inc    %si
L_0794:
	xor    %ax,%ax
	push   %ax
	push   %si
	mov    0x4(%bp),%bx
	addw   $0x2,0x4(%bp)
	pushw  (%bx)
	call   L_09ff
	add    $0x6,%sp
	lea    -0x40(%bp),%ax
	push   %ax
	call   L_2198
	pop    %cx
	jmp   L_078e
L_07b1:
	call   L_1443
	call   L_1443
	/* TODO: data ref: cmpw   $0x1,0x37a2 */
	cmpw   $0x1,D_37a2
	jne   L_07f0
L_07be:
	/* TODO: data ref: cmpw   $0x0,0x37c4 */
	cmpw   $0x0,D_37c4
	je   L_07f0
	/* TODO: data ref: mov    0x37c4,%bx */
	mov    D_37c4,%bx
	cmpw   $0x0,0x4(%bx)
	je   L_07e4
	/* TODO: data ref: mov    0x37c4,%ax */
	mov    D_37c4,%ax
	add    $0x6,%ax
	push   %ax
	mov    $0x3a5,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
L_07e4:
	/* TODO: data ref: mov    0x37c4,%bx */
	mov    D_37c4,%bx
	mov    0x6a(%bx),%ax
	/* TODO: data ref: mov    %ax,0x37c4 */
	mov    %ax,D_37c4
	jmp   L_07be
L_07f0:
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	je   L_0809
	pushw  0xa
	pushw  0x8
	mov    $0x3ba,%ax
	push   %ax
	call   L_2b54
	add    $0x6,%sp
L_0809:
	call   L_1e1c
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0813:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpb   $0x0,0xd68
	jne   L_0827
	call   L_1d9c
	mov    $0x1,%ax
	jmp   L_0829
L_0827:
	xor    %ax,%ax
L_0829:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_082d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	movw   $0x0,-0xa(%bp)
	movw   $0x0,-0x8(%bp)
L_083f:
	cmpw   $0x64,-0x8(%bp)
	jl   L_0848
	jmp   L_095c
L_0848:
	mov    $0xd68,%ax
	push   %ax
	call   L_1b90
	pop    %cx
	cmpb   $0x0,0xd68
	jne   L_085a
	jmp   L_097d
L_085a:
	mov    $0xf68,%di
	lea    -0x6(%bp),%ax
	push   %ax
	mov    $0x3d4,%ax
	push   %ax
	mov    $0xdcc,%ax
	push   %ax
	call   L_2cbd
	add    $0x6,%sp
	mov    -0x6(%bp),%ax
	mov    %ax,0x4(%di)
	lea    -0x6(%bp),%ax
	push   %ax
	mov    $0x3d7,%ax
	push   %ax
	mov    $0xdd4,%ax
	push   %ax
	call   L_2cbd
	add    $0x6,%sp
	mov    -0x6(%bp),%ax
	mov    %ax,0x8(%di)
	lea    -0x6(%bp),%ax
	push   %ax
	mov    $0x3da,%ax
	push   %ax
	mov    $0xddc,%ax
	push   %ax
	call   L_2cbd
	add    $0x6,%sp
	mov    -0x6(%bp),%ax
	mov    %ax,0xa(%di)
	lea    0xe(%di),%ax
	push   %ax
	mov    $0x3dd,%ax
	push   %ax
	mov    $0xde4,%ax
	push   %ax
	call   L_2cbd
	add    $0x6,%sp
	lea    0x16(%di),%ax
	push   %ax
	mov    $0x3e1,%ax
	push   %ax
	mov    $0xdf0,%ax
	push   %ax
	call   L_2cbd
	add    $0x6,%sp
	mov    $0xf8a,%ax
	push   %ax
	mov    $0x3e5,%ax
	push   %ax
	mov    $0xdfc,%ax
	push   %ax
	call   L_2cbd
	add    $0x6,%sp
	call   L_16f0
	cmp    %ax,0xf8a
	je   L_095c
	mov    $0x3e8,%ax
	push   %ax
	call   L_1e76
	pop    %cx
	mov    %ax,-0xc(%bp)
	cmp    $0x0,%ax
	jge   L_08fc
	mov    $0x2,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_08fc:
	cmpw   $0x0,-0xc(%bp)
	jne   L_0956
	movw   $0x0,-0x8(%bp)
	/* TODO: data ref: mov    0x37aa,%ax */
	mov    D_37aa,%ax
	mov    %ax,-0xa(%bp)
	mov    0x12,%ax
	/* TODO: data ref: mov    %ax,0x37aa */
	mov    %ax,D_37aa
	mov    0xc,%ax
	mov    0xe,%dx
	/* TODO: data ref: sub    0x37bc,%ax */
	sub    D_37bc,%ax
	/* TODO: data ref: sbb    0x37be,%dx */
	sbb    D_37be,%dx
	mov    %ax,0xc
	mov    %dx,0xe
	xor    %ax,%ax
	push   %ax
	pushw  0xe
	pushw  0xc
	/* TODO: data ref: pushw  0x37c2 */
	pushw  D_37c2
	call   L_3966
	add    $0x8,%sp
	mov    $0xd68,%ax
	push   %ax
	call   L_1b90
	pop    %cx
	mov    -0xa(%bp),%ax
	/* TODO: data ref: mov    %ax,0x37aa */
	mov    %ax,D_37aa
	subw   $0x2,0x8
	sbbw   $0x0,0xa
L_0956:
	incw   -0x8(%bp)
	jmp   L_083f
L_095c:
	cmpw   $0x64,-0x8(%bp)
	jne   L_0978
	mov    $0x3ed,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x2,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_0978:
	/* TODO: data ref: cmpw   $0x0,0x37b8 */
	cmpw   $0x0,D_37b8
L_097d:
	je   L_0995
	mov    $0xdf0,%ax
	push   %ax
	mov    $0xd68,%ax
	push   %ax
	mov    $0x415,%ax
	push   %ax
	/* TODO: data ref: pushw  0x37b8 */
	pushw  D_37b8
	call   L_270c
	add    $0x8,%sp
L_0995:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_099c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x204,%sp
	cmpb   $0x31,0xe04
	je   L_09f8
	mov    0xf76,%ax
	mov    0xf78,%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	addw   $0x1ff,-0x8(%bp)
	adcw   $0x0,-0x6(%bp)
	mov    $0x200,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_408f
	add    $0x6,%sp
L_09d3:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	subw   $0x1,-0x8(%bp)
	sbbw   $0x0,-0x6(%bp)
	cmp    $0x0,%dx
	jl   L_09f8
	jg   L_09ed
	cmp    $0x0,%ax
	jbe   L_09f8
L_09ed:
	lea    -0x208(%bp),%ax
	push   %ax
	call   L_1b90
	pop    %cx
	jmp   L_09d3
L_09f8:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_09ff:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x222,%sp
	mov    $0xf68,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_4baa
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_0a2c
	pushw  0x4(%bp)
	mov    $0x41c,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	jmp   L_0a66
L_0a2c:
	mov    0xf6c,%ax
	and    $0xe000,%ax
	mov    %ax,-0x8(%bp)
	cmp    $0xe000,%ax
	je   L_0a68
	cmpw   $0xa000,-0x8(%bp)
	je   L_0a68
	xor    %ax,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_4b7a
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	cmp    $0x0,%ax
	jge   L_0a6d
	pushw  0x4(%bp)
	mov    $0x437,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
L_0a66:
	jmp   L_0a8b
L_0a68:
	movw   $0xffff,-0x6(%bp)
L_0a6d:
	pushw  0x4(%bp)
	mov    $0x72,%ax
	push   %ax
	call   L_171d
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0a8d
	cmpw   $0x0,-0x6(%bp)
	jl   L_0a8b
	pushw  -0x6(%bp)
	call   L_4b28
	pop    %cx
L_0a8b:
	jmp   L_0aec
L_0a8d:
	cmpw   $0x0,0xf8e
	jne   L_0a9b
	cmpw   $0x0,0xf8c
	je   L_0ab0
L_0a9b:
	mov    0xf7e,%ax
	mov    0xf80,%dx
	cmp    0xf8e,%dx
	jl   L_0ac7
	jg   L_0ab0
	cmp    0xf8c,%ax
	jbe   L_0ac7
L_0ab0:
	/* TODO: data ref: cmpw   $0x0,0x37b8 */
	cmpw   $0x0,D_37b8
	je   L_0ac2
	pushw  0x4(%bp)
	call   L_1799
	pop    %cx
	or     %ax,%ax
	jne   L_0ac7
L_0ac2:
	mov    $0x1,%ax
	jmp   L_0ac9
L_0ac7:
	xor    %ax,%ax
L_0ac9:
	mov    %ax,-0x226(%bp)
	mov    %ax,-0x222(%bp)
	cmpw   $0xc000,-0x8(%bp)
	je   L_0aef
	cmpw   $0x0,-0x222(%bp)
	jne   L_0aef
	cmpw   $0x0,-0x6(%bp)
	jl   L_0aec
	pushw  -0x6(%bp)
	call   L_4b28
	pop    %cx
L_0aec:
	jmp   L_0c56
L_0aef:
	cmpw   $0xc000,-0x8(%bp)
	jne   L_0afa
	cmpw   $0x0,0x8(%bp)
L_0afa:
	je   L_0aff
	jmp   L_0c59
L_0aff:
	movw   $0x0,-0x21e(%bp)
	lea    -0x20c(%bp),%ax
	mov    %ax,%di
L_0b0b:
	mov    -0x21e(%bp),%ax
	incw   -0x21e(%bp)
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0b0b
	dec    %di
	cmpb   $0x2f,-0x1(%di)
	je   L_0b36
	mov    %di,%bx
	inc    %di
	movb   $0x2f,(%bx)
	movb   $0x0,(%di)
L_0b36:
	movw   $0x0,-0x21e(%bp)
	pushw  -0x6(%bp)
	call   L_4b28
	pop    %cx
	pushw  0x6(%bp)
	call   L_2198
	pop    %cx
	or     %ax,%ax
	jge   L_0b51
	jmp   L_0c56
L_0b51:
	cmpw   $0x0,-0x222(%bp)
	je   L_0b6b
	mov    $0x1,%ax
	push   %ax
	mov    $0x452,%ax
	push   %ax
	lea    -0x20c(%bp),%ax
	push   %ax
	call   L_09ff
	add    $0x6,%sp
L_0b6b:
	xor    %ax,%ax
	push   %ax
	mov    $0x454,%ax
	push   %ax
	call   L_4b7a
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_0b7b:
	mov    $0x10,%ax
	push   %ax
	lea    -0x21c(%bp),%ax
	push   %ax
	pushw  -0x6(%bp)
	call   L_4b90
	add    $0x6,%sp
	or     %ax,%ax
	jg   L_0b94
	jmp   L_0c47
L_0b94:
	/* TODO: data ref: cmpw   $0x0,0x37a8 */
	cmpw   $0x0,D_37a8
	je   L_0b9e
	jmp   L_0c47
L_0b9e:
	cmpw   $0x0,-0x21c(%bp)
	jne   L_0bab
	incw   -0x21e(%bp)
L_0ba9:
	jmp   L_0b7b
L_0bab:
	lea    -0x21a(%bp),%ax
	push   %ax
	mov    $0x456,%ax
	push   %ax
	call   L_4ae6
	add    $0x4,%sp
	or     %ax,%ax
	je   L_0bd1
	lea    -0x21a(%bp),%ax
	push   %ax
	mov    $0x458,%ax
	push   %ax
	call   L_4ae6
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0bd7
L_0bd1:
	incw   -0x21e(%bp)
L_0bd5:
	jmp   L_0ba9
L_0bd7:
	mov    %di,%si
	movw   $0x0,-0x220(%bp)
L_0bdf:
	cmpw   $0xe,-0x220(%bp)
L_0be4:
	jge   L_0c00
	lea    -0x21a(%bp),%ax
L_0bea:
	add    -0x220(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x220(%bp)
	jmp   L_0bdf
L_0c00:
	movb   $0x0,(%si)
	pushw  -0x6(%bp)
L_0c06:
	call   L_4b28
	pop    %cx
	xor    %ax,%ax
	push   %ax
	push   %di
L_0c0e:
	lea    -0x20c(%bp),%ax
	push   %ax
	call   L_09ff
	add    $0x6,%sp
	xor    %ax,%ax
	push   %ax
	mov    $0x45b,%ax
	push   %ax
	call   L_4b7a
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	incw   -0x21e(%bp)
	xor    %ax,%ax
	push   %ax
	mov    -0x21e(%bp),%ax
	mov    $0x4,%cx
	shl    %cl,%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	call   L_3966
	add    $0x8,%sp
	jmp   L_0bd5
L_0c47:
	pushw  -0x6(%bp)
	call   L_4b28
	pop    %cx
	mov    $0x45d,%ax
	push   %ax
	call   L_2198
	pop    %cx
L_0c56:
	jmp   L_0cea
L_0c59:
	cmpw   $0x8000,-0x8(%bp)
L_0c5e:
	je   L_0c6c
	movw   $0x0,0xf76
	movw   $0x0,0xf78
L_0c6c:
	mov    $0xf68,%ax
	push   %ax
	call   L_1675
	pop    %cx
	mov    0x4(%bp),%si
	mov    $0xd68,%di
	movw   $0x0,-0x21e(%bp)
L_0c80:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	je   L_0c9e
	cmpw   $0x64,-0x21e(%bp)
	jge   L_0c9e
	incw   -0x21e(%bp)
	jmp   L_0c80
L_0c9e:
	cmpw   $0xa000,-0x8(%bp)
	je   L_0cac
	cmpw   $0xe000,-0x8(%bp)
	jne   L_0cc5
L_0cac:
	cmpw   $0x5b,-0x21e(%bp)
	jge   L_0ccc
	pushw  0xf74
	mov    $0x460,%ax
	push   %ax
	mov    $0xdc4,%ax
	push   %ax
	call   L_39a2
	add    $0x6,%sp
L_0cc5:
	cmpw   $0x64,-0x21e(%bp)
	jl   L_0ced
L_0ccc:
	pushw  0x4(%bp)
	mov    $0x464,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	cmpw   $0x0,-0x6(%bp)
	jl   L_0cea
	pushw  -0x6(%bp)
	call   L_4b28
	pop    %cx
L_0cea:
	jmp   L_0da8
L_0ced:
	cmpw   $0x1,0xf6e
	jg   L_0cf7
	jmp   L_0dd6
L_0cf7:
	cmpw   $0x0,0x8(%bp)
	je   L_0d00
	jmp   L_0dd6
L_0d00:
	movw   $0x0,-0x226(%bp)
	/* TODO: data ref: mov    0x37c4,%ax */
	mov    D_37c4,%ax
	mov    %ax,-0x224(%bp)
L_0d0d:
	cmpw   $0x0,-0x224(%bp)
	je   L_0d40
	mov    -0x224(%bp),%bx
	mov    (%bx),%ax
	cmp    0xf6a,%ax
	jne   L_0d33
	mov    -0x224(%bp),%bx
	mov    0x2(%bx),%ax
	cmp    0xf68,%ax
	jne   L_0d33
	incw   -0x226(%bp)
	jmp   L_0d40
L_0d33:
	mov    -0x224(%bp),%bx
	mov    0x6a(%bx),%ax
	mov    %ax,-0x224(%bp)
	jmp   L_0d0d
L_0d40:
	cmpw   $0x0,-0x226(%bp)
	je   L_0dab
	mov    -0x224(%bp),%ax
	add    $0x6,%ax
	push   %ax
	mov    $0xe05,%ax
	push   %ax
	call   L_4bf4
	add    $0x4,%sp
	movb   $0x31,0xe04
	call   L_16f0
	push   %ax
	mov    $0x481,%ax
	push   %ax
	mov    $0xdfc,%ax
	push   %ax
	call   L_39a2
	add    $0x6,%sp
	mov    $0xd68,%ax
	push   %ax
	call   L_1cda
	pop    %cx
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	je   L_0d94
	mov    -0x224(%bp),%ax
	add    $0x6,%ax
	push   %ax
	pushw  0x4(%bp)
	mov    $0x485,%ax
	push   %ax
	call   L_2b54
	add    $0x6,%sp
L_0d94:
	mov    -0x224(%bp),%bx
	decw   0x4(%bx)
	cmpw   $0x0,-0x6(%bp)
	jl   L_0da8
	pushw  -0x6(%bp)
	call   L_4b28
	pop    %cx
L_0da8:
	jmp   L_0e84
L_0dab:
	mov    $0x6c,%ax
	push   %ax
	call   L_4892
	pop    %cx
	mov    %ax,-0x224(%bp)
	or     %ax,%ax
	jne   L_0dd8
	cmpw   $0x0,0x10
	je   L_0e18
	mov    $0x496,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	movw   $0x0,0x10
L_0dd6:
	jmp   L_0e18
L_0dd8:
	mov    -0x224(%bp),%bx
	/* TODO: data ref: mov    0x37c4,%ax */
	mov    D_37c4,%ax
	mov    %ax,0x6a(%bx)
	mov    -0x224(%bp),%ax
	/* TODO: data ref: mov    %ax,0x37c4 */
	mov    %ax,D_37c4
	mov    -0x224(%bp),%bx
	mov    0xf6a,%ax
	mov    %ax,(%bx)
	mov    -0x224(%bp),%bx
	mov    0xf68,%ax
	mov    %ax,0x2(%bx)
	mov    0xf6e,%ax
	dec    %ax
	mov    -0x224(%bp),%bx
	mov    %ax,0x4(%bx)
	pushw  0x4(%bp)
	mov    -0x224(%bp),%ax
	add    $0x6,%ax
	push   %ax
	call   L_4bf4
	add    $0x4,%sp
L_0e18:
	mov    $0x200,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    0xf76,%ax
	mov    0xf78,%dx
	add    $0x1ff,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	call   L_4092
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	call   L_16f0
	push   %ax
	mov    $0x4c1,%ax
	push   %ax
	mov    $0xdfc,%ax
	push   %ax
	call   L_39a2
	add    $0x6,%sp
	mov    -0xc(%bp),%ax
	mov    -0xa(%bp),%dx
	add    $0x1,%ax
	adc    $0x0,%dx
	cmp    0x6,%dx
	jl   L_0e87
	jg   L_0e66
	cmp    0x4,%ax
	jbe   L_0e87
L_0e66:
	pushw  0x4(%bp)
	mov    $0x4c5,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	cmpw   $0x0,-0x6(%bp)
	jl   L_0e84
	pushw  -0x6(%bp)
	call   L_4b28
	pop    %cx
L_0e84:
	jmp   L_1028
L_0e87:
	mov    0x8,%ax
	mov    0xa,%dx
	add    -0xc(%bp),%ax
	adc    -0xa(%bp),%dx
	add    $0x1,%ax
	adc    $0x0,%dx
	cmp    0x6,%dx
	jge   L_0ea3
	jmp   L_0f69
L_0ea3:
	jg   L_0eae
	cmp    0x4,%ax
	ja   L_0eae
	jmp   L_0f69
L_0eae:
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	je   L_0ec7
	pushw  0xa
	pushw  0x8
	mov    $0x4da,%ax
	push   %ax
	call   L_2b54
	add    $0x6,%sp
L_0ec7:
	call   L_1443
	call   L_1443
	call   L_1e1c
	/* TODO: data ref: pushw  0x37c2 */
	pushw  D_37c2
	call   L_4b28
	pop    %cx
	call   L_3760
	incw   0x14
	pushw  0x14
	mov    $0x4f4,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	mov    %ax,-0x220(%bp)
	cmp    $0x1,%ax
	je   L_0f15
	xor    %ax,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
L_0f15:
	call   L_1f1f
	cmp    $0xa,%ax
	jne   L_0f15
	pushw  -0x220(%bp)
	mov    $0x2,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	mov    $0x2,%ax
	push   %ax
	pushw  0xf86
	call   L_4b7a
	add    $0x4,%sp
	/* TODO: data ref: mov    %ax,0x37c2 */
	mov    %ax,D_37c2
	or     %ax,%ax
	jge   L_0f5a
	pushw  0xf86
	mov    $0x520,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_0f5a:
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	je   L_0f69
	mov    $0x532,%ax
	push   %ax
	call   L_2b54
	pop    %cx
L_0f69:
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	je   L_0f83
	pushw  -0xa(%bp)
	pushw  -0xc(%bp)
	pushw  0x4(%bp)
	mov    $0x53f,%ax
	push   %ax
	call   L_2b54
	add    $0x8,%sp
L_0f83:
	mov    $0xd68,%ax
	push   %ax
	call   L_1cda
	pop    %cx
	movw   $0x0,-0x21e(%bp)
	cmpw   $0x0,-0xa(%bp)
	jne   L_0f9d
	cmpw   $0x0,-0xc(%bp)
	je   L_0fd8
L_0f9d:
	mov    $0x200,%ax
L_0fa0:
	push   %ax
	lea    -0x20c(%bp),%ax
	push   %ax
L_0fa6:
	pushw  -0x6(%bp)
	call   L_4b90
	add    $0x6,%sp
	mov    %ax,-0x21e(%bp)
	or     %ax,%ax
	jle   L_0fd8
	cmpw   $0x0,-0xa(%bp)
	jl   L_0fd8
	jg   L_0fc5
	cmpw   $0x0,-0xc(%bp)
	jbe   L_0fd8
L_0fc5:
	lea    -0x20c(%bp),%ax
	push   %ax
	call   L_1cda
	pop    %cx
	subw   $0x1,-0xc(%bp)
	sbbw   $0x0,-0xa(%bp)
	jmp   L_0f9d
L_0fd8:
	cmpw   $0x0,-0x6(%bp)
	jl   L_0fe5
	pushw  -0x6(%bp)
	call   L_4b28
	pop    %cx
L_0fe5:
	cmpw   $0x0,-0xa(%bp)
	jne   L_0ff8
	cmpw   $0x0,-0xc(%bp)
	jne   L_0ff8
	cmpw   $0x0,-0x21e(%bp)
	je   L_1009
L_0ff8:
	pushw  0x4(%bp)
	mov    $0x550,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
L_1009:
	mov    -0xc(%bp),%ax
	mov    -0xa(%bp),%dx
	subw   $0x1,-0xc(%bp)
	sbbw   $0x0,-0xa(%bp)
	cmp    $0x0,%dx
	jl   L_1028
	jg   L_1023
	cmp    $0x0,%ax
	jbe   L_1028
L_1023:
	call   L_1443
	jmp   L_1009
L_1028:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_102f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x21a,%sp
	movw   $0x0,-0x212(%bp)
	call   L_26fa
	or     %ax,%ax
	jne   L_104a
	mov    $0x1,%ax
	jmp   L_104c
L_104a:
	xor    %ax,%ax
L_104c:
	mov    %ax,-0x212(%bp)
	cmpw   $0x0,0xf8e
	jne   L_105c
	cmpw   $0x0,0xf8c
L_105c:
	je   L_1075
	mov    $0xf8c,%ax
	push   %ax
	call   L_21dc
	pop    %cx
	push   %ax
	mov    $0x56c,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
L_1075:
	call   L_082d
	call   L_0813
	or     %ax,%ax
	je   L_1082
	jmp   L_139b
L_1082:
	mov    0x4(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_10ba
	mov    0x4(%bp),%ax
	mov    %ax,-0x20e(%bp)
L_1091:
	mov    -0x20e(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_10b5
	mov    $0xd68,%ax
	push   %ax
	mov    -0x20e(%bp),%bx
	pushw  (%bx)
	call   L_184a
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_10ba
	addw   $0x2,-0x20e(%bp)
	jmp   L_1091
L_10b5:
	call   L_099c
	jmp   L_1075
L_10ba:
	cmpw   $0x0,0xf8e
	jne   L_10c6
	cmpw   $0x0,0xf8c
L_10c6:
	je   L_10dd
	mov    0xf7e,%ax
	mov    0xf80,%dx
	cmp    0xf8e,%dx
	jl   L_10ef
	jg   L_10dd
	cmp    0xf8c,%ax
	jbe   L_10ef
L_10dd:
	mov    $0xd68,%ax
	push   %ax
	mov    $0x78,%ax
	push   %ax
	call   L_171d
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_10f4
L_10ef:
	call   L_099c
	jmp   L_1075
L_10f4:
	mov    $0xd68,%ax
	push   %ax
	call   L_15aa
	pop    %cx
	or     %ax,%ax
	je   L_112f
	mov    0xf6c,%ax
	and    $0xfff,%ax
	or     $0x4000,%ax
	push   %ax
	mov    $0xd68,%ax
	push   %ax
	call   L_21ac
	add    $0x4,%sp
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	jne   L_111e
	jmp   L_1075
L_111e:
	mov    $0xd68,%ax
	push   %ax
	mov    $0x595,%ax
	push   %ax
	call   L_2b54
	add    $0x4,%sp
	jmp   L_1075
L_112f:
	cmpb   $0x31,0xe04
	jne   L_1180
	mov    $0xd68,%ax
	push   %ax
	call   L_4cb8
	pop    %cx
	mov    $0xd68,%ax
	push   %ax
	mov    $0xe05,%ax
	push   %ax
	call   L_27a4
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_1165
	mov    $0xd68,%ax
	push   %ax
	mov    $0x59b,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	jmp   L_1075
L_1165:
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	je   L_117e
	mov    $0xe05,%ax
	push   %ax
	mov    $0xd68,%ax
	push   %ax
	mov    $0x5b1,%ax
	push   %ax
	call   L_2b54
	add    $0x6,%sp
L_117e:
	jmp   L_11ee
L_1180:
	mov    0xf6c,%ax
	and    $0x6000,%ax
	mov    %ax,-0x216(%bp)
	cmp    $0x6000,%ax
	je   L_1197
	/* TODO: data ref: cmpw   $0x2000,-0x216(%bp) */
	cmpw   $D_2000,-0x216(%bp)
	jne   L_1214
L_1197:
	cmpw   $0x0,-0x212(%bp)
	jne   L_11b3
	mov    $0xd68,%ax
	push   %ax
	mov    $0x5c2,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	jmp   L_1075
L_11b3:
	lea    -0x214(%bp),%ax
	push   %ax
	mov    $0x5f4,%ax
	push   %ax
	mov    $0xdc4,%ax
	push   %ax
	call   L_2cbd
	add    $0x6,%sp
	pushw  -0x214(%bp)
	pushw  0xf6c
	mov    $0xd68,%ax
	push   %ax
	call   L_27ba
	add    $0x6,%sp
	or     %ax,%ax
	je   L_11f0
	mov    $0xd68,%ax
	push   %ax
	mov    $0x5f8,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
L_11ee:
	jmp   L_1211
L_11f0:
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	je   L_1211
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	pushw  -0xa(%bp)
	pushw  -0xc(%bp)
	mov    $0xd68,%ax
	push   %ax
	mov    $0x610,%ax
	push   %ax
	call   L_2b54
	add    $0xc,%sp
L_1211:
	jmp   L_133d
L_1214:
	mov    0xf6c,%ax
	and    $0xfff,%ax
	push   %ax
	mov    $0xd68,%ax
	push   %ax
	call   L_2968
	add    $0x4,%sp
	mov    %ax,-0x210(%bp)
	or     %ax,%ax
	jge   L_1245
	mov    $0xd68,%ax
	push   %ax
	mov    $0x616,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	call   L_099c
	jmp   L_1075
L_1245:
	mov    $0x200,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    0xf76,%ax
	mov    0xf78,%dx
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	add    $0x1ff,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	call   L_4092
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	je   L_128e
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	pushw  -0xa(%bp)
	pushw  -0xc(%bp)
	mov    $0xd68,%ax
	push   %ax
	mov    $0x62f,%ax
	push   %ax
	call   L_2b54
	add    $0xc,%sp
L_128e:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	subw   $0x1,-0x8(%bp)
	sbbw   $0x0,-0x6(%bp)
	cmp    $0x0,%dx
	jge   L_12a4
	jmp   L_1335
L_12a4:
	jg   L_12ae
	cmp    $0x0,%ax
	ja   L_12ae
	jmp   L_1335
L_12ae:
	lea    -0x20c(%bp),%ax
	push   %ax
	call   L_1b90
	pop    %cx
	cmpw   $0x0,-0xa(%bp)
	jl   L_12f9
	jg   L_12c6
	cmpw   $0x200,-0xc(%bp)
	jbe   L_12f9
L_12c6:
	mov    $0x200,%ax
	push   %ax
	lea    -0x20c(%bp),%ax
	push   %ax
	pushw  -0x210(%bp)
	call   L_4cfe
	add    $0x6,%sp
	or     %ax,%ax
	jge   L_130d
	mov    $0xd68,%ax
	push   %ax
	mov    $0x64c,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	mov    $0x2,%ax
	push   %ax
	call   L_1811
	pop    %cx
	jmp   L_1329
L_12f9:
	pushw  -0xc(%bp)
	lea    -0x20c(%bp),%ax
	push   %ax
	pushw  -0x210(%bp)
	call   L_4cfe
	add    $0x6,%sp
	or     %ax,%ax
L_130d:
	jge   L_1329
	mov    $0xd68,%ax
	push   %ax
	mov    $0x66a,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	mov    $0x2,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_1329:
	subw   $0x200,-0xc(%bp)
	sbbw   $0x0,-0xa(%bp)
	jmp   L_128e
L_1335:
	pushw  -0x210(%bp)
	call   L_4b28
	pop    %cx
L_133d:
	mov    0xf6c,%ax
	and    $0xfff,%ax
	push   %ax
	mov    $0xd68,%ax
	push   %ax
	call   L_21ac
	add    $0x4,%sp
	pushw  0xf72
	pushw  0xf70
	mov    $0xd68,%ax
	push   %ax
	call   L_21c2
	add    $0x6,%sp
	cmpw   $0x0,0xf88
	je   L_136a
	jmp   L_1075
L_136a:
	xor    %ax,%ax
	push   %ax
	call   L_3984
	pop    %cx
	cwtd
	mov    %ax,-0x21e(%bp)
	mov    %dx,-0x21c(%bp)
	mov    0xf7e,%ax
	mov    0xf80,%dx
	mov    %ax,-0x21a(%bp)
	mov    %dx,-0x218(%bp)
	lea    -0x21e(%bp),%ax
	push   %ax
	mov    $0xd68,%ax
	push   %ax
	call   L_4ccc
	add    $0x4,%sp
	jmp   L_1075
L_139b:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_13a2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_13a7:
	call   L_082d
	call   L_0813
	or     %ax,%ax
	jne   L_141f
	mov    0x4(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_13da
	mov    0x4(%bp),%di
L_13bc:
	cmpw   $0x0,(%di)
	je   L_13d5
	mov    $0xd68,%ax
	push   %ax
	pushw  (%di)
	call   L_184a
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_13d5
	inc    %di
	inc    %di
	jmp   L_13bc
L_13d5:
	cmpw   $0x0,(%di)
	je   L_141a
L_13da:
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	je   L_13ef
	mov    $0xd68,%ax
	push   %ax
	mov    $0xf68,%ax
	push   %ax
	call   L_147a
	add    $0x4,%sp
L_13ef:
	mov    $0xd68,%ax
	push   %ax
	mov    $0x688,%ax
	push   %ax
	call   L_2b54
	add    $0x4,%sp
	cmpb   $0x31,0xe04
	jne   L_1412
	mov    $0xe05,%ax
	push   %ax
	mov    $0x68b,%ax
	push   %ax
	call   L_2b54
	add    $0x4,%sp
L_1412:
	mov    $0x699,%ax
	push   %ax
	call   L_2b54
	pop    %cx
L_141a:
	call   L_099c
	jmp   L_13a7
L_141f:
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	je   L_143f
	mov    0x8,%ax
	mov    0xa,%dx
	add    $0x2,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	mov    $0x69b,%ax
	push   %ax
	call   L_2b54
	add    $0x6,%sp
L_143f:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1443:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x202,%sp
	lea    -0x204(%bp),%ax
	mov    %ax,-0x206(%bp)
L_1454:
	lea    -0x4(%bp),%ax
	cmp    %ax,-0x206(%bp)
	jae   L_146a
	mov    -0x206(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x206(%bp)
	jmp   L_1454
L_146a:
	lea    -0x204(%bp),%ax
	push   %ax
	call   L_1cda
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_147a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	push   %di
	call   L_1510
	pop    %cx
	pushw  0xa(%di)
	pushw  0x8(%di)
	mov    $0x6b5,%ax
	push   %ax
	call   L_2b54
	add    $0x6,%sp
	mov    0x4(%di),%ax
	and    $0x6000,%ax
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: cmp    $0x2000,%ax */
	cmp    $D_2000,%ax
	je   L_14ae
	cmpw   $0x6000,-0x6(%bp)
	jne   L_14df
L_14ae:
	lea    -0x6(%bp),%ax
	push   %ax
	mov    $0x6bd,%ax
	push   %ax
	mov    0x6(%bp),%ax
	add    $0x5c,%ax
	push   %ax
	call   L_2cbd
	add    $0x6,%sp
	mov    -0x6(%bp),%ax
	and    $0xff,%ax
	push   %ax
	mov    -0x6(%bp),%ax
	mov    $0x8,%cx
	shr    %cl,%ax
	push   %ax
	mov    $0x6c0,%ax
	push   %ax
	call   L_2b54
	add    $0x6,%sp
	jmp   L_14ef
L_14df:
	pushw  0x10(%di)
	pushw  0xe(%di)
	mov    $0x6c8,%ax
	push   %ax
	call   L_2b54
	add    $0x6,%sp
L_14ef:
	lea    0x16(%di),%ax
	push   %ax
	call   L_21dc
	pop    %cx
	mov    %ax,%si
	lea    0x14(%si),%ax
	push   %ax
	lea    0x4(%si),%ax
	push   %ax
	mov    $0x6cc,%ax
	push   %ax
	call   L_2b54
	add    $0x6,%sp
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1510:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x4(%di),%ax
	and    $0x6000,%ax
	jmp   L_153a
L_1522:
	movb   $0x64,-0x5(%bp)
	jmp   L_154b
L_1528:
	movb   $0x62,-0x5(%bp)
	jmp   L_154b
L_152e:
	movb   $0x63,-0x5(%bp)
	jmp   L_154b
L_1534:
	movb   $0x2d,-0x5(%bp)
	jmp   L_154b
L_153a:
	/* TODO: data ref: cmp    $0x2000,%ax */
	cmp    $D_2000,%ax
	je   L_152e
	cmp    $0x4000,%ax
	je   L_1522
	cmp    $0x6000,%ax
	je   L_1528
	jmp   L_1534
L_154b:
	mov    -0x5(%bp),%al
	cbtw
	push   %ax
	mov    $0x6de,%ax
	push   %ax
	call   L_2b54
	add    $0x4,%sp
	mov    $0x82,%si
L_155d:
	cmp    $0x94,%si
	jae   L_1572
	push   %di
	mov    %si,%bx
	inc    %si
	inc    %si
	pushw  (%bx)
	call   L_1577
	add    $0x4,%sp
	jmp   L_155d
L_1572:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1577:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_1585:
	dec    %di
	jl   L_159a
	mov    0x6(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %si,%bx
	inc    %si
	inc    %si
	test   %ax,(%bx)
	jne   L_159a
	inc    %si
	inc    %si
	jmp   L_1585
L_159a:
	pushw  (%si)
	mov    $0x6e1,%ax
	push   %ax
	call   L_2b54
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_15aa:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    %di,%si
L_15b6:
	cmpb   $0x0,(%si)
	je   L_1602
	cmpb   $0x2f,(%si)
	jne   L_15ff
	movb   $0x0,(%si)
	mov    $0x1,%ax
	push   %ax
	push   %di
	call   L_2844
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_15f1
	push   %di
	call   L_1f93
	pop    %cx
	or     %ax,%ax
	je   L_15e2
	xor    %ax,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_15e2:
	pushw  0xf72
	pushw  0xf70
	push   %di
	call   L_21c2
	add    $0x6,%sp
L_15f1:
	movb   $0x2f,(%si)
	cmpb   $0x0,0x1(%si)
	jne   L_15ff
	mov    $0x1,%ax
	jmp   L_1604
L_15ff:
	inc    %si
	jmp   L_15b6
L_1602:
	xor    %ax,%ax
L_1604:
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
	call   L_3846
	add    $0x4,%sp
	/* TODO: data ref: incw   0x37a8 */
	incw   D_37a8
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
	mov    $0x3,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	/* TODO: data ref: incw   0x37a8 */
	incw   D_37a8
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
	mov    $0x1,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	/* TODO: data ref: incw   0x37a8 */
	incw   D_37a8
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
	mov    $0xf,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	/* TODO: data ref: incw   0x37a8 */
	incw   D_37a8
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1675:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xd68,%si
L_1681:
	cmp    $0xf68,%si
	jae   L_168d
	movb   $0x0,(%si)
	inc    %si
	jmp   L_1681
L_168d:
	mov    0x4(%di),%ax
	and    $0x7fff,%ax
	push   %ax
	mov    $0x6e4,%ax
	push   %ax
	mov    $0xdcc,%ax
	push   %ax
	call   L_39a2
	add    $0x6,%sp
	pushw  0x8(%di)
	mov    $0x6e9,%ax
	push   %ax
	mov    $0xdd4,%ax
	push   %ax
	call   L_39a2
	add    $0x6,%sp
	pushw  0xa(%di)
	mov    $0x6ee,%ax
	push   %ax
	mov    $0xddc,%ax
	push   %ax
	call   L_39a2
	add    $0x6,%sp
	pushw  0x10(%di)
	pushw  0xe(%di)
	mov    $0x6f3,%ax
	push   %ax
	mov    $0xde4,%ax
	push   %ax
	call   L_39a2
	add    $0x8,%sp
	pushw  0x18(%di)
	pushw  0x16(%di)
	mov    $0x6fa,%ax
	push   %ax
	mov    $0xdf0,%ax
	push   %ax
	call   L_39a2
	add    $0x8,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_16f0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0xdfc,%si
L_16f8:
	cmp    $0xe04,%si
	jae   L_1704
	movb   $0x20,(%si)
	inc    %si
	jmp   L_16f8
L_1704:
	xor    %di,%di
	mov    $0xd68,%si
L_1709:
	cmp    $0xf68,%si
	jae   L_1717
	mov    (%si),%al
	cbtw
	add    %ax,%di
	inc    %si
	jmp   L_1709
L_1717:
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_171d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: cmpw   $0x0,0x37b2 */
	cmpw   $0x0,D_37b2
	je   L_1768
	pushw  0x4(%bp)
	mov    $0x701,%ax
	push   %ax
	call   L_2b54
	add    $0x4,%sp
	/* TODO: data ref: cmpw   $0x0,0x37b0 */
	cmpw   $0x0,D_37b0
	je   L_174a
	pushw  0x6(%bp)
	mov    $0xf68,%ax
	push   %ax
	call   L_147a
	add    $0x4,%sp
L_174a:
	pushw  0x6(%bp)
	mov    $0x705,%ax
	push   %ax
	call   L_2b54
	add    $0x4,%sp
	call   L_176f
	cmp    $0x79,%ax
	jne   L_1764
	mov    $0x1,%ax
	jmp   L_176b
L_1764:
	xor    %ax,%ax
	jmp   L_176b
L_1768:
	mov    $0x1,%ax
L_176b:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_176f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	call   L_1f1f
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	cmp    $0xa,%cl
	je   L_178c
L_1782:
	call   L_1f1f
	cmp    $0xa,%ax
	je   L_1790
	jmp   L_1782
L_178c:
	movb   $0x6e,-0x5(%bp)
L_1790:
	mov    -0x5(%bp),%al
	cbtw
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1799:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6c,%sp
	/* TODO: data ref: pushw  0x37b8 */
	pushw  D_37b8
	call   L_2c56
	pop    %cx
	pushw  0x4(%bp)
	call   L_1948
	pop    %cx
	mov    %ax,-0x70(%bp)
	mov    %dx,-0x6e(%bp)
	cmp    $0x0,%dx
	jg   L_17c7
	jl   L_17c2
	cmp    $0x0,%ax
	jae   L_17c7
L_17c2:
	mov    $0x1,%ax
	jmp   L_180a
L_17c7:
	xor    %ax,%ax
	push   %ax
	pushw  -0x6e(%bp)
	pushw  -0x70(%bp)
	/* TODO: data ref: pushw  0x37b8 */
	pushw  D_37b8
	call   L_297e
	add    $0x8,%sp
	lea    -0x6c(%bp),%ax
	push   %ax
	lea    -0x68(%bp),%ax
	push   %ax
	mov    $0x70a,%ax
	push   %ax
	/* TODO: data ref: pushw  0x37b8 */
	pushw  D_37b8
	call   L_2ca4
	add    $0x8,%sp
	mov    0xf7e,%ax
	mov    0xf80,%dx
	cmp    -0x6a(%bp),%dx
	jl   L_1808
	jg   L_1803
	cmp    -0x6c(%bp),%ax
	jbe   L_1808
L_1803:
	mov    $0x1,%ax
	jmp   L_180a
L_1808:
	xor    %ax,%ax
L_180a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1811:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: cmpw   $0x0,0x37ae */
	cmpw   $0x0,D_37ae
	jge   L_1827
	mov    $0x711,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	jmp   L_182f
L_1827:
	mov    $0x754,%ax
	push   %ax
	call   L_2b54
	pop    %cx
L_182f:
	mov    $0x795,%ax
	push   %ax
	call   L_2b54
	pop    %cx
L_1837:
	call   L_1f1f
	cmp    $0xa,%ax
	jne   L_1837
	pushw  0x4(%bp)
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_184a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1857:
	cmpb   $0x0,(%di)
	je   L_1876
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
	je   L_1857
	xor    %ax,%ax
	jmp   L_188c
L_1876:
	cmpb   $0x0,(%si)
	je   L_1889
	cmpb   $0x2f,(%si)
	jne   L_1885
	mov    $0x1,%ax
	jmp   L_188c
L_1885:
	xor    %ax,%ax
	jmp   L_188c
L_1889:
	mov    $0x1,%ax
L_188c:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1890:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	call   L_2b32
	pop    %cx
	call   L_3836
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_1903
	mov    $0x1,%ax
	push   %ax
	call   L_4b28
	pop    %cx
	pushw  -0x8(%bp)
	call   L_2630
	pop    %cx
	xor    %ax,%ax
	push   %ax
	mov    $0x7bb,%ax
	push   %ax
	mov    $0x7b2,%ax
	push   %ax
	call   L_3804
	add    $0x6,%sp
	xor    %ax,%ax
	push   %ax
	mov    $0x7cc,%ax
	push   %ax
	mov    $0x7bf,%ax
	push   %ax
	call   L_3804
	add    $0x6,%sp
	mov    $0x7d0,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x7dd,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_18f9:
	xor    %ax,%ax
	push   %ax
	call   L_4ce2
	pop    %cx
	cmp    $0xffff,%ax
L_1903:
	jne   L_18f9
	/* TODO: data ref: cmpw   $0x0,0x37a8 */
	cmpw   $0x0,D_37a8
	jne   L_1933
	mov    $0x32,%ax
	push   %ax
	pushw  0x4(%bp)
	pushw  -0xa(%bp)
	call   L_4b90
	add    $0x6,%sp
	or     %ax,%ax
	jle   L_192d
L_1920:
	mov    0x4(%bp),%bx
	cmpb   $0xa,(%bx)
	je   L_192d
	incw   0x4(%bp)
	jmp   L_1920
L_192d:
	mov    0x4(%bp),%bx
	movb   $0x0,(%bx)
L_1933:
	pushw  -0xa(%bp)
	call   L_4b28
	pop    %cx
	pushw  -0x8(%bp)
	call   L_4b28
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1948:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	xor    %di,%di
L_1952:
	mov    0x4(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	je   L_196d
	mov    0x4(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	cmpb   $0x20,(%bx)
	je   L_196d
	inc    %di
	jmp   L_1952
L_196d:
	/* TODO: data ref: pushw  0x37b6 */
	pushw  D_37b6
	/* TODO: data ref: pushw  0x37b4 */
	pushw  D_37b4
	pushw  0xf92
	pushw  0xf90
	push   %di
	pushw  0x4(%bp)
	call   L_199a
	add    $0xc,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_199a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xd4,%sp
	movw   $0x0,0xf9a
L_19a9:
	mov    0x8(%bp),%ax
	mov    0xa(%bp),%dx
	cmp    0xe(%bp),%dx
	jl   L_19c3
	jg   L_19bb
	cmp    0xc(%bp),%ax
	jb   L_19c3
L_19bb:
	mov    $0xffff,%ax
	mov    %ax,%dx
	jmp   L_1a8f
L_19c3:
	mov    $0x2,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    0xc(%bp),%ax
	mov    0xe(%bp),%dx
	sub    0x8(%bp),%ax
	sbb    0xa(%bp),%dx
	push   %dx
	push   %ax
	call   L_4092
	add    $0x8,%sp
	mov    %ax,-0xd8(%bp)
	mov    %dx,-0xd6(%bp)
	mov    0x8(%bp),%ax
	mov    0xa(%bp),%dx
	add    -0xd8(%bp),%ax
	adc    -0xd6(%bp),%dx
	sub    $0x64,%ax
	sbb    $0x0,%dx
	mov    %ax,-0xd0(%bp)
	mov    %dx,-0xce(%bp)
	mov    -0xd0(%bp),%ax
	mov    -0xce(%bp),%dx
	cmp    0xa(%bp),%dx
	jg   L_1a24
	jl   L_1a16
	cmp    0x8(%bp),%ax
	jae   L_1a24
L_1a16:
	mov    0x8(%bp),%ax
	mov    0xa(%bp),%dx
	mov    %ax,-0xd0(%bp)
	mov    %dx,-0xce(%bp)
L_1a24:
	xor    %ax,%ax
	push   %ax
	pushw  -0xce(%bp)
	pushw  -0xd0(%bp)
	/* TODO: data ref: pushw  0x37b8 */
	pushw  D_37b8
	call   L_297e
	add    $0x8,%sp
	/* TODO: data ref: pushw  0x37b8 */
	pushw  D_37b8
	mov    $0xc8,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	lea    -0xcc(%bp),%ax
	push   %ax
	call   L_2b7e
	add    $0x8,%sp
	incw   0xf9a
	xor    %di,%di
L_1a56:
	cmp    $0xc8,%di
	jge   L_1a76
	lea    -0xcc(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	cmpb   $0xa,(%bx)
	je   L_1a76
	addw   $0x1,-0xd0(%bp)
	adcw   $0x0,-0xce(%bp)
	inc    %di
	jmp   L_1a56
L_1a76:
	mov    -0xd0(%bp),%ax
	mov    -0xce(%bp),%dx
	cmp    0xe(%bp),%dx
	jl   L_1a91
	jg   L_1a8a
	cmp    0xc(%bp),%ax
	jb   L_1a91
L_1a8a:
	mov    $0xffff,%ax
	mov    %ax,%dx
L_1a8f:
	jmp   L_1b0a
L_1a91:
	mov    -0xd0(%bp),%ax
	mov    -0xce(%bp),%dx
	mov    %ax,-0xd4(%bp)
	mov    %dx,-0xd2(%bp)
	mov    %di,%si
	inc    %di
L_1aa4:
	cmp    $0xc8,%di
	jge   L_1ac4
	addw   $0x1,-0xd4(%bp)
	adcw   $0x0,-0xd2(%bp)
	lea    -0xcc(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	cmpb   $0xa,(%bx)
	je   L_1ac4
	inc    %di
	jmp   L_1aa4
L_1ac4:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	lea    -0xcc(%bp),%ax
	add    %si,%ax
	push   %ax
	call   L_1b11
	add    $0x6,%sp
	mov    %ax,%di
	or     %di,%di
	jge   L_1aee
	mov    -0xd0(%bp),%ax
	mov    -0xce(%bp),%dx
	mov    %ax,0xc(%bp)
	mov    %dx,0xe(%bp)
L_1aeb:
	jmp   L_19a9
L_1aee:
	or     %di,%di
	jle   L_1b02
	mov    -0xd4(%bp),%ax
	mov    -0xd2(%bp),%dx
	mov    %ax,0x8(%bp)
	mov    %dx,0xa(%bp)
	jmp   L_1aeb
L_1b02:
	mov    -0xd0(%bp),%ax
	mov    -0xce(%bp),%dx
L_1b0a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b11:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%bx
	cmpb   $0xa,(%bx)
	je   L_1b26
	mov    $0x2,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_1b26:
	xor    %di,%di
L_1b28:
	cmp    0x8(%bp),%di
	jge   L_1b78
	mov    0x4(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	mov    0x1(%bx),%al
	cbtw
	mov    0x6(%bp),%dx
	add    %di,%dx
	mov    %dx,%bx
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jle   L_1b51
	mov    $0xffff,%ax
	jmp   L_1b8c
L_1b51:
	mov    0x4(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	mov    0x1(%bx),%al
	cbtw
	mov    0x6(%bp),%dx
	add    %di,%dx
	mov    %dx,%bx
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jge   L_1b75
	mov    $0x1,%ax
	jmp   L_1b8c
L_1b75:
	inc    %di
	jmp   L_1b28
L_1b78:
	mov    0x4(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	cmpb   $0x20,0x1(%bx)
	jne   L_1b89
	xor    %ax,%ax
	jmp   L_1b8c
L_1b89:
	mov    $0xffff,%ax
L_1b8c:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b90:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	/* TODO: data ref: mov    0x37aa,%ax */
	mov    D_37aa,%ax
	cmp    0x12,%ax
	jge   L_1bab
	/* TODO: data ref: cmpw   $0x0,0x37a6 */
	cmpw   $0x0,D_37a6
	je   L_1bab
	jmp   L_1ca7
L_1bab:
	/* TODO: data ref: cmpw   $0x0,0x37a6 */
	cmpw   $0x0,D_37a6
	jne   L_1bc0
	cmpw   $0x0,0x12
	jne   L_1bc0
	movw   $0x14,-0x8(%bp)
	jmp   L_1bc6
L_1bc0:
	mov    0x12,%ax
	mov    %ax,-0x8(%bp)
L_1bc6:
	mov    -0x8(%bp),%ax
	mov    $0x9,%cx
	shl    %cl,%ax
	push   %ax
	mov    $0xfa0,%ax
	push   %ax
	/* TODO: data ref: pushw  0x37c2 */
	pushw  D_37c2
	call   L_4b90
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	cwtd
	/* TODO: data ref: mov    %ax,0x37bc */
	mov    %ax,D_37bc
	/* TODO: data ref: mov    %dx,0x37be */
	mov    %dx,D_37be
	cmp    $0x0,%dx
	jg   L_1c2c
	jl   L_1bf4
	cmp    $0x0,%ax
	jae   L_1c2c
L_1bf4:
	mov    $0x7e0,%ax
	push   %ax
	call   L_1e76
	pop    %cx
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jge   L_1c0b
	mov    $0x3,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_1c0b:
	cmpw   $0x0,-0xa(%bp)
	jne   L_1c26
	xor    %ax,%ax
	push   %ax
	pushw  0xe
	pushw  0xc
	/* TODO: data ref: pushw  0x37c2 */
	pushw  D_37c2
	call   L_3966
	add    $0x8,%sp
L_1c26:
	cmpw   $0x0,-0xa(%bp)
	jle   L_1bc6
L_1c2c:
	mov    0xc,%ax
	mov    0xe,%dx
	/* TODO: data ref: add    0x37bc,%ax */
	add    D_37bc,%ax
	/* TODO: data ref: adc    0x37be,%dx */
	adc    D_37be,%dx
	mov    %ax,0xc
	mov    %dx,0xe
	/* TODO: data ref: cmpw   $0x0,0x37a6 */
	cmpw   $0x0,D_37a6
	jne   L_1ca1
	mov    -0x6(%bp),%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	or     %dx,%dx
	je   L_1c6c
	mov    $0x7e5,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x3,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_1c6c:
	mov    -0x6(%bp),%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: cmpw   $0x0,0x37a0 */
	cmpw   $0x0,D_37a0
	je   L_1c9b
	cmpw   $0x1,-0x6(%bp)
	je   L_1c9b
	mov    $0x802,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x4,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_1c9b:
	mov    -0x6(%bp),%ax
	mov    %ax,0x12
L_1ca1:
	/* TODO: data ref: movw   $0x0,0x37aa */
	movw   $0x0,D_37aa
L_1ca7:
	/* TODO: data ref: movw   $0x1,0x37a6 */
	movw   $0x1,D_37a6
	/* TODO: data ref: mov    0x37aa,%ax */
	mov    D_37aa,%ax
	/* TODO: data ref: incw   0x37aa */
	incw   D_37aa
	mov    $0x9,%cx
	shl    %cl,%ax
	add    $(L_0fa6-6),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1e4c
	add    $0x4,%sp
	addw   $0x1,0x8
	adcw   $0x0,0xa
	mov    $0x200,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1cda:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: movw   $0x1,0x37a6 */
	movw   $0x1,D_37a6
	cmpw   $0x0,0x12
	jne   L_1cf2
	movw   $0x1,0x12
L_1cf2:
	/* TODO: data ref: mov    0x37aa,%ax */
	mov    D_37aa,%ax
	cmp    0x12,%ax
	jl   L_1d32
	mov    0x12,%ax
	mov    $0x9,%cx
	shl    %cl,%ax
	push   %ax
	mov    $0xfa0,%ax
	push   %ax
	/* TODO: data ref: pushw  0x37c2 */
	pushw  D_37c2
	call   L_4cfe
	add    $0x6,%sp
	or     %ax,%ax
	jge   L_1d2c
	mov    $0x82a,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x2,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_1d2c:
	/* TODO: data ref: movw   $0x0,0x37aa */
	movw   $0x0,D_37aa
L_1d32:
	pushw  0x4(%bp)
	/* TODO: data ref: mov    0x37aa,%ax */
	mov    D_37aa,%ax
	/* TODO: data ref: incw   0x37aa */
	incw   D_37aa
	mov    $0x9,%cx
	shl    %cl,%ax
	add    $(L_0fa6-6),%ax
	push   %ax
	call   L_1e4c
	add    $0x4,%sp
	/* TODO: data ref: mov    0x37aa,%ax */
	mov    D_37aa,%ax
	cmp    0x12,%ax
	jl   L_1d8b
	mov    0x12,%ax
	mov    $0x9,%cx
	shl    %cl,%ax
	push   %ax
	mov    $0xfa0,%ax
	push   %ax
	/* TODO: data ref: pushw  0x37c2 */
	pushw  D_37c2
	call   L_4cfe
	add    $0x6,%sp
	or     %ax,%ax
	jge   L_1d85
	mov    $0x843,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x2,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_1d85:
	/* TODO: data ref: movw   $0x0,0x37aa */
	movw   $0x0,D_37aa
L_1d8b:
	addw   $0x1,0x8
	adcw   $0x0,0xa
	mov    $0x200,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d9c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x1,%ax
	push   %ax
	mov    $0xfe00,%ax
	mov    $0xffff,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: pushw  0x37c2 */
	pushw  D_37c2
	call   L_3966
	add    $0x8,%sp
	/* TODO: data ref: mov    0x37aa,%ax */
	mov    D_37aa,%ax
	cmp    0x12,%ax
	jl   L_1e0e
	mov    0x12,%ax
	dec    %ax
	/* TODO: data ref: mov    %ax,0x37aa */
	mov    %ax,D_37aa
	mov    0x12,%ax
	mov    $0x9,%cx
	shl    %cl,%ax
	push   %ax
	mov    $0xfa0,%ax
	push   %ax
	/* TODO: data ref: pushw  0x37c2 */
	pushw  D_37c2
	call   L_4b90
	add    $0x6,%sp
	or     %ax,%ax
	jge   L_1df8
	mov    $0x85c,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x4,%sp
	mov    $0x4,%ax
	push   %ax
	call   L_1811
	pop    %cx
L_1df8:
	mov    $0x1,%ax
	push   %ax
	mov    $0xfe00,%ax
	mov    $0xffff,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: pushw  0x37c2 */
	pushw  D_37c2
	call   L_3966
	add    $0x8,%sp
L_1e0e:
	subw   $0x1,0x8
	sbbw   $0x0,0xa
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e1c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x12,%ax
	mov    $0x9,%cx
	shl    %cl,%ax
	push   %ax
	mov    $0xfa0,%ax
	push   %ax
	/* TODO: data ref: pushw  0x37c2 */
	pushw  D_37c2
	call   L_4cfe
	add    $0x6,%sp
	/* TODO: data ref: movw   $0x0,0x37aa */
	movw   $0x0,D_37aa
	xor    %ax,%ax
	cwtd
	mov    %ax,0x8
	mov    %dx,0xa
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e4c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	movw   $L_0200,-0x6(%bp)
L_1e5f:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	decw   -0x6(%bp)
	jne   L_1e5f
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e76:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	pushw  0x4(%bp)
	mov    $0x87f,%ax
	push   %ax
	call   L_2b54
	add    $0x4,%sp
	mov    $0x8a1,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	mov    $0x8b5,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	mov    $0x8cb,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	mov    $0x8ea,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	mov    $0x8fd,%ax
	push   %ax
	call   L_2b54
	pop    %cx
L_1eb1:
	call   L_1f1f
	or     $0x20,%ax
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	mov    %cl,%al
	cbtw
	jmp   L_1f09
L_1ec1:
	mov    $0x916,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	mov    $0xffff,%ax
	jmp   L_1efd
L_1ece:
	mov    $0x91d,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	mov    $0x924,%ax
	push   %ax
	call   L_2b54
	pop    %cx
L_1ede:
	call   L_1f1f
	cmp    $0xa,%ax
	jne   L_1ede
	mov    $0x953,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	xor    %ax,%ax
	jmp   L_1f1a
L_1ef2:
	mov    $0x955,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	mov    $0x1,%ax
L_1efd:
	jmp   L_1f1a
L_1eff:
	mov    $0x95d,%ax
	push   %ax
	call   L_2b54
	pop    %cx
	jmp   L_1eb1
L_1f09:
	cmp    $0x61,%ax
	je   L_1ec1
	cmp    $0x69,%ax
	je   L_1ef2
	cmp    $0x72,%ax
	je   L_1ece
	jmp   L_1eff
L_1f1a:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1f1f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    $0xf94,%ax
	push   %ax
	mov    $0x7408,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_4c50
	add    $0x6,%sp
	mov    0xf98,%ax
	mov    %ax,-0x6(%bp)
	movw   $0x20,0xf98
	mov    $0xf94,%ax
	push   %ax
	mov    $0x7409,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_4c50
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	lea    -0x7(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_4b90
	add    $0x6,%sp
	mov    -0x6(%bp),%ax
	mov    %ax,0xf98
	mov    $0xf94,%ax
	push   %ax
	mov    $0x7409,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_4c50
	add    $0x6,%sp
	cmpb   $0xd,-0x7(%bp)
	jne   L_1f88
	mov    $0xa,%ax
	jmp   L_1f8c
L_1f88:
	mov    -0x7(%bp),%al
	cbtw
L_1f8c:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1f93:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x100,%sp
	xor    %si,%si
	movb   $0x0,-0x84(%bp)
	xor    %di,%di
L_1fa5:
	mov    0x4(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	je   L_1fc5
	mov    0x4(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	cmpb   $0x2f,(%bx)
	jne   L_1fc2
	lea    0x1(%di),%ax
	mov    %ax,%si
L_1fc2:
	inc    %di
	jmp   L_1fa5
L_1fc5:
	or     %si,%si
	je   L_1fd8
	push   %si
	pushw  0x4(%bp)
	lea    -0x84(%bp),%ax
	push   %ax
	call   L_3708
	add    $0x6,%sp
L_1fd8:
	mov    $0x95f,%ax
	push   %ax
	lea    -0x84(%bp),%ax
	add    %si,%ax
	push   %ax
	call   L_4bf4
	add    $0x4,%sp
	mov    $0x2,%ax
	push   %ax
	lea    -0x84(%bp),%ax
	push   %ax
	call   L_2844
	add    $0x4,%sp
	or     %ax,%ax
	je   L_2014
	lea    -0x84(%bp),%ax
	push   %ax
	mov    $0x961,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	mov    $0x1,%ax
	jmp   L_203c
L_2014:
	xor    %ax,%ax
	push   %ax
	mov    $0x41ff,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_27ba
	add    $0x6,%sp
	or     %ax,%ax
	jge   L_203e
	pushw  0x4(%bp)
	mov    $0x980,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	mov    $0x1,%ax
L_203c:
	jmp   L_208a
L_203e:
	pushw  0x4(%bp)
	lea    -0x104(%bp),%ax
	push   %ax
	call   L_4bf4
	add    $0x4,%sp
	mov    $0x9a7,%ax
	push   %ax
	lea    -0x104(%bp),%ax
	push   %ax
	call   L_4bc0
	add    $0x4,%sp
	lea    -0x104(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_27a4
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_208c
	lea    -0x104(%bp),%ax
	push   %ax
	mov    $0x9aa,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	pushw  0x4(%bp)
	call   L_4cb8
	pop    %cx
	mov    $0x1,%ax
L_208a:
	jmp   L_20e9
L_208c:
	mov    $0x9c7,%ax
	push   %ax
	lea    -0x104(%bp),%ax
	push   %ax
	call   L_4bc0
	add    $0x4,%sp
	lea    -0x104(%bp),%ax
	push   %ax
	lea    -0x84(%bp),%ax
	push   %ax
	call   L_27a4
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_20eb
	lea    -0x104(%bp),%ax
	push   %ax
	mov    $0x9c9,%ax
	push   %ax
	mov    $0xcc6,%ax
	push   %ax
	call   L_270c
	add    $0x6,%sp
	lea    -0x104(%bp),%ax
	push   %ax
	call   L_36ec
	pop    %cx
	lea    -0x104(%bp),%dx
	add    %ax,%dx
	mov    %dx,%bx
	movb   $0x0,(%bx)
	lea    -0x104(%bp),%ax
	push   %ax
	call   L_4cb8
	pop    %cx
	pushw  0x4(%bp)
	call   L_4cb8
	pop    %cx
	mov    $0x1,%ax
L_20e9:
	jmp   L_20ed
L_20eb:
	xor    %ax,%ax
L_20ed:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_20f4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	mov    0x4(%bp),%di
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
	xor    %si,%si
L_210c:
	mov    (%di),%al
	cbtw
	jmp   L_2115
L_2111:
	inc    %si
L_2112:
	inc    %di
	jmp   L_212e
L_2115:
	cmp    $0x9,%ax
	je   L_212b
	cmp    $0x20,%ax
	je   L_212b
	cmp    $0x2b,%ax
	je   L_2112
	cmp    $0x2d,%ax
	je   L_2111
	jmp   L_212e
L_212b:
	inc    %di
	jmp   L_210c
L_212e:
	cmpb   $0x30,(%di)
	jl   L_2178
	cmpb   $0x39,(%di)
	jg   L_2178
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	call   L_3fe9
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
	jmp   L_212e
L_2178:
	or     %si,%si
	je   L_218b
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	jmp   L_2191
L_218b:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
L_2191:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2198:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall chdir(12) */
	mov    0x4(%bp),%ax
	/* chdir -> __NR_chdir */
	xchg	%ax,%bx
	mov	$__NR_chdir,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_21a9
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_21a9:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_21ac:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall chmod(15) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* chmod -> __NR_chmod */
	xchg	%ax,%bx
	mov	%dx,%cx
	mov	$__NR_chmod,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_21c0
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_21c0:
	pop    %bp
	ret


L_21c2:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall chown(16) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* chown -> __NR_chown */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_chown,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_21d9
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_21d9:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_21dc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_21f1
	pop    %cx
	push   %ax
	call   L_24af
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_21f1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x16,%sp
	lea    -0x16(%bp),%ax
	push   %ax
	call   L_26e6
	pop    %cx
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    -0x10(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	call   L_3fe9
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
	call   L_230f
	pop    %cx
	mov    %ax,%si
	mov    0xe(%si),%di
	movw   $0x77,-0x6(%bp)
	movw   $L_012f,-0x8(%bp)
	cmpw   $0x4a,0xa(%si)
	je   L_2251
	cmpw   $0x4b,0xa(%si)
	jne   L_2279
L_2251:
	mov    0xa(%si),%ax
	sub    $0x4a,%ax
	mov    %ax,%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	mov    0xa18(%bx),%ax
	mov    %ax,-0x6(%bp)
	mov    0xa(%si),%ax
	sub    $0x4a,%ax
	mov    %ax,%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	mov    0xa1a(%bx),%ax
	mov    %ax,-0x8(%bp)
L_2279:
	pushw  -0x6(%bp)
	push   %si
	call   L_22d8
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	pushw  -0x8(%bp)
	push   %si
	call   L_22d8
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	cmpw   $0x0,-0xe(%bp)
	je   L_22cf
	cmp    -0x6(%bp),%di
	jg   L_22a9
	cmp    -0x6(%bp),%di
	jne   L_22cf
	cmpw   $0x2,0x4(%si)
	jl   L_22cf
L_22a9:
	cmp    -0x8(%bp),%di
	jl   L_22b9
	cmp    -0x8(%bp),%di
	jne   L_22cf
	cmpw   $0x1,0x4(%si)
	jge   L_22cf
L_22b9:
	addw   $0xe10,-0xc(%bp)
	adcw   $0x0,-0xa(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_230f
	pop    %cx
	mov    %ax,%si
	incw   0x10(%si)
L_22cf:
	mov    %si,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_22d8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	cmp    $0x3a,%si
	jl   L_22f6
	pushw  0xa(%di)
	call   L_25c7
	pop    %cx
	sub    $0x16d,%ax
	add    %ax,%si
L_22f6:
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


L_230f:
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
	call   L_40e3
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
	call   L_4092
	add    $0x8,%sp
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
	cmpw   $0x0,-0xa(%bp)
	jg   L_236e
	jl   L_235d
	cmpw   $0x0,-0xc(%bp)
	jae   L_236e
L_235d:
	addw   $0x5180,-0xc(%bp)
	adcw   $0x1,-0xa(%bp)
	subw   $0x1,-0x10(%bp)
	sbbw   $0x0,-0xe(%bp)
L_236e:
	mov    $0xa20,%di
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0xa(%bp)
	pushw  -0xc(%bp)
	call   L_40e3
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
	call   L_4092
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
	call   L_40e3
	add    $0x8,%sp
	mov    %ax,0xa2c
	cmpw   $0x0,-0xe(%bp)
	jl   L_2419
	jg   L_23ed
	cmpw   $0x0,-0x10(%bp)
	jb   L_2419
L_23ed:
	movw   $0x46,-0x8(%bp)
L_23f2:
	pushw  -0x8(%bp)
	call   L_25c7
	pop    %cx
	cwtd
	cmp    %dx,-0xe(%bp)
	jl   L_2441
	jg   L_2406
	cmp    %ax,-0x10(%bp)
	jb   L_2441
L_2406:
	pushw  -0x8(%bp)
	call   L_25c7
	pop    %cx
	cwtd
	sub    %ax,-0x10(%bp)
	sbb    %dx,-0xe(%bp)
	incw   -0x8(%bp)
	jmp   L_23f2
L_2419:
	movw   $0x46,-0x8(%bp)
L_241e:
	cmpw   $0x0,-0xe(%bp)
	jg   L_2441
	jl   L_242c
	cmpw   $0x0,-0x10(%bp)
	jae   L_2441
L_242c:
	mov    -0x8(%bp),%ax
	dec    %ax
	push   %ax
	call   L_25c7
	pop    %cx
	cwtd
	add    %ax,-0x10(%bp)
	adc    %dx,-0xe(%bp)
	decw   -0x8(%bp)
	jmp   L_241e
L_2441:
	mov    -0x8(%bp),%ax
	mov    %ax,0xa2a
	mov    -0x10(%bp),%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,0xa2e
	pushw  -0x8(%bp)
	call   L_25c7
	pop    %cx
	cmp    $0x16e,%ax
	jne   L_2462
	movw   $0x1d,0xa02
L_2462:
	movw   $0x0,-0x8(%bp)
L_2467:
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    -0x6(%bp),%ax
	cmp    0xa00(%bx),%ax
	jl   L_2486
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    0xa00(%bx),%ax
	sub    %ax,-0x6(%bp)
	incw   -0x8(%bp)
	jmp   L_2467
L_2486:
	movw   $0x1c,0xa02
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
	movw   $0x0,0xa30
	mov    $0xa20,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_24af:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    $0x9e6,%di
	mov    $0xa32,%si
L_24bb:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_24bb
	mov    0x4(%bp),%bx
	mov    0xc(%bx),%ax
	mov    $0x3,%dx
	imul   %dx
	add    $0xa4c,%ax
	mov    %ax,%si
	mov    $0x9e6,%di
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
	add    $(L_0a68-6),%ax
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
	call   L_25e5
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_25e5
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_25e5
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_25e5
	add    $0x4,%sp
	mov    %ax,%di
	mov    0x4(%bp),%bx
	cmpw   $0x64,0xa(%bx)
	jl   L_25aa
	movb   $0x32,0x1(%di)
	movb   $0x30,0x2(%di)
L_25aa:
	inc    %di
	inc    %di
	mov    0x4(%bp),%bx
	mov    0xa(%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_25e5
	add    $0x4,%sp
	mov    %ax,%di
	mov    $0x9e6,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_25c7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	cwtd
	mov    $0x4,%cx
	idiv   %cx
	or     %dx,%dx
	jne   L_25de
	mov    $0x16e,%ax
	jmp   L_25e1
L_25de:
	mov    $0x16d,%ax
L_25e1:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_25e5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	inc    %di
	cmpw   $0xa,0x6(%bp)
	jl   L_2610
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
	jmp   L_2616
L_2610:
	mov    %di,%bx
	inc    %di
	movb   $0x20,(%bx)
L_2616:
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


L_2630:
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
	jns    L_2644
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2644:
	pop    %bp
	ret


L_2646:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	push   %si
	call   L_481b
	pop    %cx
	cmpb   $0x77,(%di)
	jne   L_2670
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2968
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	jmp   L_26ac
L_2670:
	cmpb   $0x61,(%di)
	jne   L_26ae
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4b7a
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jge   L_2699
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2968
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_2699:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	call   L_3966
	add    $0x8,%sp
L_26ac:
	jmp   L_26bd
L_26ae:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4b7a
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_26bd:
	cmpw   $0x0,-0x6(%bp)
	jge   L_26c7
	xor    %ax,%ax
	jmp   L_26e0
L_26c7:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,0x7(%si)
	cmpb   $0x72,(%di)
	je   L_26da
	orb    $0x2,0x6(%si)
	jmp   L_26de
L_26da:
	orb    $0x1,0x6(%si)
L_26de:
	mov    %si,%ax
L_26e0:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_26e6:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall ftime(35) */
	mov    0x4(%bp),%ax
	/* TODO: Venix-only ftime(35): no TLVC equivalent */
	xor	%ax,%ax	/* stub */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_26f7
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_26f7:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_26fa:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getuid(24) */
	/* getuid -> __NR_getuid */
	mov	$__NR_getuid,%ax
	int	$0x80
	mov    %dx,%ax
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_270a
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_270a:
	pop    %bp
	ret


L_270c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_39f4
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_2732
	mov    $0xffff,%ax
	jmp   L_2734
L_2732:
	xor    %ax,%ax
L_2734:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2738:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	xor    %si,%si
	mov    %si,-0x6(%bp)
L_2747:
	mov    (%di),%al
	cbtw
	jmp   L_2752
L_274c:
	incw   -0x6(%bp)
L_274f:
	inc    %di
	jmp   L_276b
L_2752:
	cmp    $0x9,%ax
	je   L_2768
	cmp    $0x20,%ax
	je   L_2768
	cmp    $0x2b,%ax
	je   L_274f
	cmp    $0x2d,%ax
	je   L_274c
	jmp   L_276b
L_2768:
	inc    %di
	jmp   L_2747
L_276b:
	cmpb   $0x30,(%di)
	jl   L_2791
	cmpb   $0x39,(%di)
	jg   L_2791
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
	jmp   L_276b
L_2791:
	cmpw   $0x0,-0x6(%bp)
	je   L_279d
	mov    %si,%ax
	neg    %ax
	jmp   L_279f
L_279d:
	mov    %si,%ax
L_279f:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_27a4:
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
	jns    L_27b8
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_27b8:
	pop    %bp
	ret


L_27ba:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall mknod(14) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* mknod -> __NR_mknod */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_mknod,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_27d1
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_27d1:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_27d4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	call   L_285a
	mov    %ax,%si
	mov    0x4(%bp),%di
L_27e2:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_27e2
	dec    %di
L_27eb:
	dec    %di
	cmpb   $0x58,(%di)
	jne   L_280e
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
	jmp   L_27eb
L_280e:
	inc    %di
	movw   $0x61,-0x6(%bp)
L_2814:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2844
	add    $0x4,%sp
	cmp    $0xffff,%ax
	je   L_283c
	cmpw   $0x7a,-0x6(%bp)
	jne   L_2830
	mov    $0xa88,%ax
	jmp   L_283f
L_2830:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%di)
	incw   -0x6(%bp)
	jmp   L_2814
L_283c:
	mov    0x4(%bp),%ax
L_283f:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2844:
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
	jns    L_2858
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2858:
	pop    %bp
	ret


L_285a:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getpid(20) */
	/* getpid -> __NR_getpid */
	mov	$__NR_getpid,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2868
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2868:
	pop    %bp
	ret


L_286a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	cmpb   $0x0,0xa8a
	jne   L_28a7
	movw   $0xcce,-0x8(%bp)
	movw   $0xd56,0xd56
	movw   $0x88,-0xa(%bp)
L_288d:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_28a2
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_288d
L_28a2:
	movb   $0x1,0xa8a
L_28a7:
	movw   $0xcb6,-0x6(%bp)
L_28ac:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_28cb
	mov    -0x6(%bp),%ax
	cmp    0xd56,%ax
	jb   L_28c5
	xor    %ax,%ax
	jmp   L_2936
L_28c5:
	addw   $0x8,-0x6(%bp)
	jmp   L_28ac
L_28cb:
	cmpb   $0x77,(%di)
	jne   L_28e1
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2968
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_2920
L_28e1:
	cmpb   $0x61,(%di)
	jne   L_2922
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4b7a
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_290f
	/* TODO: data ref: cmpw   $0x2,0x37c6 */
	cmpw   $0x2,D_37c6
	jne   L_290f
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2968
	add    $0x4,%sp
	mov    %ax,%si
L_290f:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_3966
	add    $0x8,%sp
L_2920:
	jmp   L_2930
L_2922:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4b7a
	add    $0x4,%sp
	mov    %ax,%si
L_2930:
	or     %si,%si
	jge   L_2938
	xor    %ax,%ax
L_2936:
	jmp   L_2960
L_2938:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_2956
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_295d
L_2956:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_295d:
	mov    -0x6(%bp),%ax
L_2960:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2968:
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
	jns    L_297c
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_297c:
	pop    %bp
	ret


L_297e:
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
	jne   L_299c
	jmp   L_2ae5
L_299c:
	cmpw   $0x2,0xa(%bp)
	jl   L_29a5
	jmp   L_2a86
L_29a5:
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_29b1
	jmp   L_2a86
L_29b1:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_29c0
	jmp   L_2a86
L_29c0:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%si
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,0xa(%bp)
	jne   L_2a19
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
	call   L_3966
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
	jmp   L_2a34
L_2a19:
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
L_2a34:
	or     %si,%si
	jle   L_2a75
	mov    %si,%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jg   L_2a75
	jl   L_2a47
	cmp    %ax,-0x8(%bp)
	ja   L_2a75
L_2a47:
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x4(%bp),%bx
	sub    (%bx),%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jl   L_2a75
	jg   L_2a5f
	cmp    %ax,-0x8(%bp)
	jb   L_2a75
L_2a5f:
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	add    %ax,(%bx)
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	sub    %ax,0x2(%bx)
	xor    %ax,%ax
	jmp   L_2b26
L_2a75:
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x1,%ax
	and    $0x0,%dx
	mov    %ax,%di
	jmp   L_2a88
L_2a86:
	xor    %di,%di
L_2a88:
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
	call   L_3966
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
	or     %di,%di
	je   L_2aef
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_2adc
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2b15
L_2adc:
	pushw  0x4(%bp)
	call   L_363e
	pop    %cx
	jmp   L_2b15
L_2ae5:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
L_2aef:
	je   L_2b15
	pushw  0x4(%bp)
	call   L_47ac
	pop    %cx
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_3966
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_2b15:
	cmpw   $0xffff,-0x6(%bp)
	jne   L_2b28
	cmpw   $0xffff,-0x8(%bp)
	jne   L_2b28
	mov    $0xffff,%ax
L_2b26:
	jmp   L_2b2a
L_2b28:
	xor    %ax,%ax
L_2b2a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2b32:
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
	jns    L_2b42
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2b40:
	pop    %bp
	ret
L_2b42:
	mov    0x4(%bp),%cx
	mov    %cx,%bp
	mov    %ax,-0x70(%bp)
	mov    %dx,0x2(%bp)
	mov    $0x0,%ax
	jmp   L_2b40
	/* .byte 0 (alignment) */


L_2b54:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0xcbe,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_39f4
	add    $0x6,%sp
	mov    0xcc4,%al
	cbtw
	test   $0x20,%ax
	je   L_2b78
	mov    $0xffff,%ax
	jmp   L_2b7a
L_2b78:
	xor    %ax,%ax
L_2b7a:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2b7e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_2bdf
L_2b99:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_2bdf
	mov    0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
L_2ba7:
	decw   0x2(%si)
	jl   L_2bb8
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2bbd
L_2bb8:
	push   %si
	call   L_363e
	pop    %cx
L_2bbd:
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jl   L_2bd0
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_2bd5
L_2bd0:
	mov    -0x8(%bp),%ax
	jmp   L_2be2
L_2bd5:
	decw   -0xa(%bp)
	jne   L_2ba7
	incw   -0x8(%bp)
	jmp   L_2b99
L_2bdf:
	mov    -0x8(%bp),%ax
L_2be2:
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
	je   L_2c4b
L_2c04:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_2c4b
	mov    0x6(%bp),%ax
	mov    %ax,-0x6(%bp)
L_2c12:
	decw   0x2(%si)
	jl   L_2c2a
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
	jmp   L_2c38
L_2c2a:
	push   %si
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_46be
	add    $0x4,%sp
L_2c38:
	decw   -0x6(%bp)
	jne   L_2c12
	mov    0x6(%si),%al
	cbtw
	test   $0x20,%ax
	jne   L_2c4b
	incw   -0x8(%bp)
	jmp   L_2c04
L_2c4b:
	mov    -0x8(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2c56:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	push   %di
	call   L_47ac
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
	call   L_3966
	add    $0x8,%sp
	movw   $0x0,0x2(%di)
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	andb   $0xcf,0x6(%di)
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
	mov    $0xcb6,%ax
	push   %ax
	call   L_2cfe
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2ca4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_2cfe
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2cbd:
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
L_2cd8:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_2ce5
	incw   -0xa(%bp)
	jmp   L_2cd8
L_2ce5:
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_2cfe
	add    $0x6,%sp
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2cfe:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xe,%sp
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x10(%bp)
L_2d18:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	jmp   L_2ec2
L_2d24:
	mov    -0x8(%bp),%ax
	jmp   L_2df2
L_2d2a:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	cmp    $0x25,%ax
	jne   L_2d3b
	jmp   L_2ee0
L_2d3b:
	movw   $0x0,-0xe(%bp)
	cmpw   $0x2a,-0x6(%bp)
	je   L_2d4f
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xe(%bp)
	jmp   L_2d58
L_2d4f:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
L_2d58:
	movw   $0x0,-0xa(%bp)
	movw   $0x1,-0x12(%bp)
L_2d62:
	cmpw   $0x30,-0x6(%bp)
	jl   L_2d8a
	cmpw   $0x39,-0x6(%bp)
	jg   L_2d8a
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
	jmp   L_2d62
L_2d8a:
	cmpw   $0x0,-0xa(%bp)
	jne   L_2d95
	movw   $0x7530,-0xa(%bp)
L_2d95:
	cmpw   $0x6c,-0x6(%bp)
	jne   L_2dab
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	movw   $0x2,-0x12(%bp)
	jmp   L_2dce
L_2dab:
	cmpw   $0x68,-0x6(%bp)
	jne   L_2dc1
	movw   $0x0,-0x12(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	jmp   L_2dce
L_2dc1:
	cmpw   $0x5b,-0x6(%bp)
	jne   L_2dce
	push   %di
	call   L_3396
	pop    %cx
	mov    %ax,%di
L_2dce:
	mov    -0x6(%bp),%bx
	mov    0xb63(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_2de9
	mov    -0x6(%bp),%ax
	add    $0x20,%ax
	mov    %ax,-0x6(%bp)
	movw   $0x2,-0x12(%bp)
L_2de9:
	cmpw   $0x0,-0x6(%bp)
	jne   L_2df4
	mov    $0xffff,%ax
L_2df2:
	jmp   L_2e31
L_2df4:
	lea    -0x10(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	pushw  -0x12(%bp)
	pushw  -0xa(%bp)
	pushw  -0x6(%bp)
	pushw  -0xe(%bp)
	call   L_2ee9
	add    $0xc,%sp
	or     %ax,%ax
	je   L_2e1a
	cmpw   $0x0,-0xe(%bp)
	je   L_2e1a
	incw   -0x8(%bp)
L_2e1a:
	cmpw   $0x0,-0x10(%bp)
L_2e1e:
	jne   L_2e23
	jmp   L_2d18
L_2e23:
	cmpw   $0x0,-0x8(%bp)
	je   L_2e2e
	mov    -0x8(%bp),%ax
	jmp   L_2e31
L_2e2e:
	mov    $0xffff,%ax
L_2e31:
	jmp   L_2eaf
L_2e33:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_2e4d
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2e54
L_2e4d:
	pushw  0x4(%bp)
	call   L_363e
	pop    %cx
L_2e54:
	mov    %ax,-0xc(%bp)
	cmp    $0x20,%ax
	je   L_2e33
	cmpw   $0x9,-0xc(%bp)
	je   L_2e33
	cmpw   $0xa,-0xc(%bp)
L_2e66:
	je   L_2e33
	cmpw   $0xffff,-0xc(%bp)
L_2e6c:
	je   L_2e1e
	pushw  0x4(%bp)
	pushw  -0xc(%bp)
	call   L_4c6a
	add    $0x4,%sp
	jmp   L_2d18
L_2e7d:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_2e97
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2e9e
L_2e97:
	pushw  0x4(%bp)
	call   L_363e
	pop    %cx
L_2e9e:
	mov    %ax,-0xc(%bp)
	cmp    -0x6(%bp),%ax
	je   L_2e6c
	cmpw   $0xffff,-0xc(%bp)
	jne   L_2eb1
	mov    $0xffff,%ax
L_2eaf:
	jmp   L_2ee2
L_2eb1:
	pushw  0x4(%bp)
	pushw  -0xc(%bp)
	call   L_4c6a
	add    $0x4,%sp
	mov    -0x8(%bp),%ax
	jmp   L_2ee2
L_2ec2:
	or     %ax,%ax
	jne   L_2ec9
	jmp   L_2d24
L_2ec9:
	cmp    $0x9,%ax
L_2ecc:
	je   L_2e66
	cmp    $0xa,%ax
	je   L_2ecc
	cmp    $0x20,%ax
	je   L_2ecc
	cmp    $0x25,%ax
	jne   L_2ee0
	jmp   L_2d2a
L_2ee0:
	jmp   L_2e7d
L_2ee2:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2ee9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x63,0x6(%bp)
	je   L_2f03
	cmpw   $0x73,0x6(%bp)
	je   L_2f03
	cmpw   $0x5b,0x6(%bp)
	jne   L_2f28
L_2f03:
	pushw  0xe(%bp)
	pushw  0xc(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	cmpw   $0x0,0x4(%bp)
	je   L_2f1c
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	jmp   L_2f1e
L_2f1c:
	xor    %ax,%ax
L_2f1e:
	push   %ax
	call   L_3288
	add    $0xa,%sp
	jmp   L_31f6
L_2f28:
	movw   $0x0,-0x58(%bp)
	movw   $0x0,-0x56(%bp)
	movw   $0x0,-0x52(%bp)
	movw   $0x0,-0x4c(%bp)
	cmpw   $0x65,0x6(%bp)
	je   L_2f48
	cmpw   $0x66,0x6(%bp)
	jne   L_2f4d
L_2f48:
	movw   $0x1,-0x4c(%bp)
L_2f4d:
	movw   $0xa,-0x48(%bp)
	cmpw   $0x6f,0x6(%bp)
	jne   L_2f5f
	movw   $0x8,-0x48(%bp)
	jmp   L_2f6a
L_2f5f:
	cmpw   $0x78,0x6(%bp)
	jne   L_2f6a
	movw   $0x10,-0x48(%bp)
L_2f6a:
	lea    -0x44(%bp),%ax
	mov    %ax,%di
	movw   $0x0,-0x4a(%bp)
	movw   $0x0,-0x4e(%bp)
L_2f79:
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_2f93
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2f9a
L_2f93:
	pushw  0xc(%bp)
	call   L_363e
	pop    %cx
L_2f9a:
	mov    %ax,-0x46(%bp)
	cmp    $0x20,%ax
	je   L_2f79
	cmpw   $0x9,-0x46(%bp)
	je   L_2f79
	cmpw   $0xa,-0x46(%bp)
	je   L_2f79
	cmpw   $0x2d,-0x46(%bp)
	jne   L_2fea
	incw   -0x4e(%bp)
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_2fdb
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2fe2
L_2fdb:
	pushw  0xc(%bp)
	call   L_363e
	pop    %cx
L_2fe2:
	mov    %ax,-0x46(%bp)
	decw   0x8(%bp)
	jmp   L_3017
L_2fea:
	cmpw   $0x2b,-0x46(%bp)
	jne   L_3017
	decw   0x8(%bp)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_300d
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_3014
L_300d:
	pushw  0xc(%bp)
	call   L_363e
	pop    %cx
L_3014:
	mov    %ax,-0x46(%bp)
L_3017:
	decw   0x8(%bp)
	jge   L_301f
	jmp   L_3173
L_301f:
	movw   $0x0,-0x54(%bp)
	cmpw   $0x30,-0x46(%bp)
	jl   L_3035
	cmpw   $0x39,-0x46(%bp)
	jg   L_3035
	movw   $0x1,-0x54(%bp)
L_3035:
	cmpw   $0x61,-0x46(%bp)
	jl   L_3041
	cmpw   $0x7a,-0x46(%bp)
	jle   L_304d
L_3041:
	cmpw   $0x41,-0x46(%bp)
	jl   L_3058
	cmpw   $0x5a,-0x46(%bp)
	jg   L_3058
L_304d:
	cmpw   $0x10,-0x48(%bp)
	jne   L_3058
	movw   $0x1,-0x54(%bp)
L_3058:
	cmpw   $0x0,-0x54(%bp)
	jne   L_3061
	jmp   L_30f5
L_3061:
	incw   -0x52(%bp)
	cmpw   $0x8,-0x48(%bp)
	jne   L_307b
	mov    $0x3,%cx
	or     %cx,%cx
	je   L_30b5
L_3071:
	shlw   -0x58(%bp)
	rclw   -0x56(%bp)
	loop   L_3071
	jmp   L_30b5
L_307b:
	cmpw   $0xa,-0x48(%bp)
	jne   L_30a6
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	mov    $0x2,%cx
	or     %cx,%cx
	je   L_3094
L_308e:
	shl    %ax
	rcl    %dx
	loop   L_308e
L_3094:
	add    -0x58(%bp),%ax
	adc    -0x56(%bp),%dx
	shl    %ax
	rcl    %dx
	mov    %ax,-0x58(%bp)
	mov    %dx,-0x56(%bp)
	jmp   L_30b5
L_30a6:
	mov    $0x4,%cx
	or     %cx,%cx
	je   L_30b5
L_30ad:
	shlw   -0x58(%bp)
	rclw   -0x56(%bp)
	loop   L_30ad
L_30b5:
	mov    -0x46(%bp),%ax
	mov    %ax,-0x50(%bp)
	cmpw   $0x30,-0x46(%bp)
	jl   L_30cd
	cmpw   $0x39,-0x46(%bp)
	jg   L_30cd
	subw   $0x30,-0x46(%bp)
	jmp   L_30e3
L_30cd:
	cmpw   $0x61,-0x46(%bp)
	jl   L_30df
	cmpw   $0x66,-0x46(%bp)
	jg   L_30df
	subw   $0x57,-0x46(%bp)
	jmp   L_30e3
L_30df:
	subw   $0x37,-0x46(%bp)
L_30e3:
	mov    -0x46(%bp),%ax
	cwtd
	add    %ax,-0x58(%bp)
	adc    %dx,-0x56(%bp)
	mov    -0x50(%bp),%ax
	mov    %ax,-0x46(%bp)
	jmp   L_310a
L_30f5:
	cmpw   $0x2e,-0x46(%bp)
	jne   L_310c
	cmpw   $0xa,-0x48(%bp)
	jne   L_3122
	cmpw   $0x0,-0x4c(%bp)
	je   L_3131
	incw   -0x52(%bp)
L_310a:
	jmp   L_317b
L_310c:
	cmpw   $0x65,-0x46(%bp)
	je   L_3118
	cmpw   $0x45,-0x46(%bp)
	jne   L_3122
L_3118:
	cmpw   $0x0,-0x4a(%bp)
	jne   L_3122
	cmpw   $0xa,-0x48(%bp)
L_3122:
	je   L_3127
	jmp   L_31ac
L_3127:
	cmpw   $0x0,-0x4c(%bp)
	je   L_3131
	cmpw   $0x0,-0x52(%bp)
L_3131:
	je   L_31ac
	incw   -0x4a(%bp)
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_315a
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_3161
L_315a:
	pushw  0xc(%bp)
	call   L_363e
	pop    %cx
L_3161:
	mov    %ax,-0x46(%bp)
	cmp    $0x2b,%ax
	je   L_317b
	cmpw   $0x2d,-0x46(%bp)
	je   L_317b
	cmpw   $0x30,-0x46(%bp)
L_3173:
	jl   L_31ac
	cmpw   $0x39,-0x46(%bp)
	jg   L_31ac
L_317b:
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_319f
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_31a6
L_319f:
	pushw  0xc(%bp)
	call   L_363e
	pop    %cx
L_31a6:
	mov    %ax,-0x46(%bp)
	jmp   L_3017
L_31ac:
	cmpw   $0x0,-0x4e(%bp)
	je   L_31c5
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	mov    %ax,-0x58(%bp)
	mov    %dx,-0x56(%bp)
L_31c5:
	cmpw   $0xffff,-0x46(%bp)
	je   L_31e0
	pushw  0xc(%bp)
	pushw  -0x46(%bp)
	call   L_4c6a
	add    $0x4,%sp
	mov    0xe(%bp),%bx
	movw   $0x0,(%bx)
	jmp   L_31e7
L_31e0:
	mov    0xe(%bp),%bx
	movw   $0x1,(%bx)
L_31e7:
	cmpw   $0x0,0x4(%bp)
	je   L_31f4
	lea    -0x44(%bp),%ax
	cmp    %ax,%di
	jne   L_31f9
L_31f4:
	xor    %ax,%ax
L_31f6:
	jmp   L_3281
L_31f9:
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	mov    -0x4c(%bp),%ax
	mov    $0x4,%cx
	shl    %cl,%ax
	or     0xa(%bp),%ax
	jmp   L_3272
	lea    -0x44(%bp),%ax
	push   %ax
	call   L_3400
	fstpl  -0x60(%bp)
	pop    %cx
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	fldl   -0x60(%bp)
	fstps  (%bx)
	jmp   L_3270
	lea    -0x44(%bp),%ax
	push   %ax
	call   L_3400
	fstpl  -0x60(%bp)
	pop    %cx
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	fldl   -0x60(%bp)
	fstpl  (%bx)
	jmp   L_3270
	mov    -0x58(%bp),%ax
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    %ax,(%bx)
	jmp   L_3270
	mov    -0x58(%bp),%ax
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    %ax,(%bx)
	jmp   L_3270
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_3270:
	jmp   L_327e
L_3272:
	cmp    $0x12,%ax
	ja   L_327e
	shl    %ax
	xchg   %ax,%bx
	jmp    *0xb0c(%bx)
L_327e:
	mov    $0x1,%ax
L_3281:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3288:
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
	jne   L_32b4
	cmpw   $0x7530,0x8(%bp)
	jne   L_32b4
	movw   $0x1,0x8(%bp)
L_32b4:
	movw   $0x0,-0xa(%bp)
	cmpw   $0x73,0x6(%bp)
	jne   L_32c4
	movw   $0x1,-0xa(%bp)
L_32c4:
	decw   0x2(%si)
	jl   L_32d5
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_32da
L_32d5:
	push   %si
	call   L_363e
	pop    %cx
L_32da:
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	mov    0xa8c(%bx),%al
	cbtw
	test   %ax,-0xa(%bp)
	je   L_32ef
	cmpw   $0xffff,-0x6(%bp)
	jne   L_32c4
L_32ef:
	movw   $0x1,-0xa(%bp)
	cmpw   $0x63,0x6(%bp)
	jne   L_3301
	movw   $0x0,-0xa(%bp)
	jmp   L_330c
L_3301:
	cmpw   $0x5b,0x6(%bp)
	jne   L_330c
	movw   $0x2,-0xa(%bp)
L_330c:
	cmpw   $0xffff,-0x6(%bp)
	je   L_334d
	mov    -0x6(%bp),%bx
	mov    0xa8c(%bx),%al
	cbtw
	test   %ax,-0xa(%bp)
	jne   L_334d
	or     %di,%di
	je   L_332d
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
L_332d:
	decw   0x8(%bp)
	jle   L_334d
	decw   0x2(%si)
	jl   L_3343
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_3348
L_3343:
	push   %si
	call   L_363e
	pop    %cx
L_3348:
	mov    %ax,-0x6(%bp)
	jmp   L_330c
L_334d:
	cmpw   $0xffff,-0x6(%bp)
	je   L_336c
	cmpw   $0x0,0x8(%bp)
	jle   L_3363
	push   %si
	pushw  -0x6(%bp)
	call   L_4c6a
	add    $0x4,%sp
L_3363:
	mov    0xc(%bp),%bx
	movw   $0x0,(%bx)
	jmp   L_3373
L_336c:
	mov    0xc(%bp),%bx
	movw   $0x1,(%bx)
L_3373:
	or     %di,%di
	je   L_338d
	cmp    -0x8(%bp),%di
	je   L_338d
	cmpw   $0x63,0x6(%bp)
	je   L_3388
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
L_3388:
	mov    $0x1,%ax
	jmp   L_338f
L_338d:
	xor    %ax,%ax
L_338f:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3396:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	movw   $0x0,-0x6(%bp)
	cmpb   $0x5e,(%di)
	jne   L_33ae
	incw   -0x6(%bp)
	inc    %di
L_33ae:
	xor    %si,%si
L_33b0:
	cmp    $0x80,%si
	jge   L_33cb
	cmpw   $0x0,-0x6(%bp)
	je   L_33c3
	andb   $0xfd,0xa8c(%si)
	jmp   L_33c8
L_33c3:
	orb    $0x2,0xa8c(%si)
L_33c8:
	inc    %si
	jmp   L_33b0
L_33cb:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	and    $0x7f,%ax
	cmp    $0x5d,%ax
	je   L_33f8
	cmpw   $0x0,-0x6(%bp)
	je   L_33e9
	orb    $0x2,0xa8c(%si)
	inc    %si
	jmp   L_33ef
L_33e9:
	andb   $0xfd,0xa8c(%si)
	inc    %si
L_33ef:
	or     %si,%si
	jne   L_33cb
	dec    %di
	mov    %di,%ax
	jmp   L_33fa
L_33f8:
	mov    %di,%ax
L_33fa:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3400:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x3c,%sp
	mov    0x4(%bp),%di
	movw   $0x1,-0x24(%bp)
L_3411:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    $0x20,%ax
	je   L_3411
	cmp    $0x2d,%si
	jne   L_342a
	movw   $0xffff,-0x24(%bp)
	jmp   L_3430
L_342a:
	cmp    $0x2b,%si
	je   L_3430
	dec    %di
L_3430:
	movw   $0x0,-0x22(%bp)
	fldl   0xb32
	fstpl  -0xc(%bp)
	movw   $0x0,-0x1e(%bp)
L_3445:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    0xb63(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_3492
	cmpw   $0x10,-0x1e(%bp)
	jge   L_348a
	fldl   0xb3a
	fmull  -0xc(%bp)
	fstpl  -0x30(%bp)
	lea    -0x30(%si),%ax
	mov    %ax,-0x38(%bp)
	filds  -0x38(%bp)
	fstl   -0x38(%bp)
	faddl  -0x30(%bp)
	fstl   -0x40(%bp)
	fstpl  -0xc(%bp)
	jmp   L_348d
L_348a:
	incw   -0x22(%bp)
L_348d:
	incw   -0x1e(%bp)
	jmp   L_3445
L_3492:
	cmp    $0x2e,%si
	jne   L_34e2
L_3497:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    0xb63(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_34e2
	cmpw   $0x10,-0x1e(%bp)
	jge   L_34dd
	fldl   0xb42
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
L_34dd:
	incw   -0x1e(%bp)
	jmp   L_3497
L_34e2:
	movw   $0x1,-0x26(%bp)
	movw   $0x0,-0x20(%bp)
	cmp    $0x45,%si
	je   L_34f6
	cmp    $0x65,%si
	jne   L_354b
L_34f6:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    $0x2b,%ax
	je   L_3510
	cmp    $0x2d,%si
	jne   L_350f
	movw   $0xffff,-0x26(%bp)
	jmp   L_3510
L_350f:
	dec    %di
L_3510:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    0xb63(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_3534
	mov    -0x20(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	lea    -0x30(%si),%dx
	add    %dx,%ax
	mov    %ax,-0x20(%bp)
	jmp   L_3510
L_3534:
	cmpw   $0x0,-0x26(%bp)
	jge   L_3542
	mov    -0x20(%bp),%ax
	neg    %ax
	mov    %ax,-0x20(%bp)
L_3542:
	mov    -0x22(%bp),%ax
	add    -0x20(%bp),%ax
	mov    %ax,-0x22(%bp)
L_354b:
	movw   $0x1,-0x26(%bp)
	cmpw   $0x0,-0x22(%bp)
	jge   L_3563
	movw   $0xffff,-0x26(%bp)
	mov    -0x22(%bp),%ax
	neg    %ax
	mov    %ax,-0x22(%bp)
L_3563:
	mov    -0x22(%bp),%ax
	imulw  -0x26(%bp)
	add    -0x1e(%bp),%ax
	cmp    $0xffd9,%ax
	jge   L_3581
	fldl   0xb4a
	fstpl  -0xc(%bp)
	movw   $0x0,-0x22(%bp)
L_3581:
	fldl   0xb52
	fstpl  -0x14(%bp)
	fldl   0xb5a
	fstpl  -0x1c(%bp)
	mov    -0x22(%bp),%ax
	mov    %ax,-0x28(%bp)
L_359b:
	testw  $0x1,-0x22(%bp)
	je   L_35b0
	fldl   -0x14(%bp)
	fmull  -0x1c(%bp)
	fstpl  -0x14(%bp)
L_35b0:
	sarw   -0x22(%bp)
	cmpw   $0x0,-0x22(%bp)
	je   L_35c9
	fldl   -0x1c(%bp)
	fmull  -0x1c(%bp)
	fstpl  -0x1c(%bp)
	jmp   L_359b
L_35c9:
	cmpw   $0x0,-0x26(%bp)
	jge   L_35df
	fldl   -0xc(%bp)
	fdivl  -0x14(%bp)
	fstpl  -0xc(%bp)
	jmp   L_35ed
L_35df:
	fldl   -0xc(%bp)
	fmull  -0x14(%bp)
	fstpl  -0xc(%bp)
L_35ed:
	mov    -0x26(%bp),%ax
	imulw  -0x28(%bp)
	push   %ax
	fldl   -0xc(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4d18
	fstpl  -0x30(%bp)
	add    $0xa,%sp
	fldl   -0x30(%bp)
	fstpl  -0xc(%bp)
	cmpw   $0x0,-0x24(%bp)
	jge   L_3631
	fldl   -0xc(%bp)
	fchs
	fstl   -0x30(%bp)
	fstpl  -0xc(%bp)
L_3631:
	fldl   -0xc(%bp)
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_363e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_3655
	mov    $0xffff,%ax
	jmp   L_3661
L_3655:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_3663
	mov    $0xffff,%ax
L_3661:
	jmp   L_36db
L_3663:
	cmpw   $0x0,0x4(%di)
	jne   L_3697
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_367e
	mov    0x7(%di),%al
	cbtw
	add    $(L_0bea-6),%ax
	mov    %ax,0x4(%di)
	jmp   L_3663
L_367e:
	mov    $0x200,%ax
	push   %ax
	call   L_4892
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_3693
	orb    $0x4,0x6(%di)
	jmp   L_3663
L_3693:
	orb    $0x8,0x6(%di)
L_3697:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_36aa
	mov    $0x1,%ax
	jmp   L_36ad
L_36aa:
	mov    $0x200,%ax
L_36ad:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4b90
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_36dd
	cmpw   $0xffff,0x2(%di)
	jne   L_36cf
	orb    $0x10,0x6(%di)
	jmp   L_36d3
L_36cf:
	orb    $0x20,0x6(%di)
L_36d3:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_36db:
	jmp   L_36e7
L_36dd:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_36e7:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_36ec:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_36f7:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_3702
	inc    %si
	jmp   L_36f7
L_3702:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3708:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
L_3720:
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_3756
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_3751
L_3739:
	incw   -0x6(%bp)
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_374c
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	jmp   L_3739
L_374c:
	mov    -0x8(%bp),%ax
	jmp   L_3759
L_3751:
	incw   -0x6(%bp)
	jmp   L_3720
L_3756:
	mov    -0x8(%bp),%ax
L_3759:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3760:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall sync(36) */
	/* sync -> __NR_sync */
	mov	$__NR_sync,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_376e
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_376e:
	pop    %bp
	ret


L_3770:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	call   L_3836
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jne   L_37a2
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	mov    $0xc03,%ax
	push   %ax
	mov    $0xc00,%ax
	push   %ax
	mov    $0xbf8,%ax
	push   %ax
	call   L_3804
	add    $0xa,%sp
	mov    $0x7f,%ax
	push   %ax
	call   L_3822
	pop    %cx
L_37a2:
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	mov    %ax,%di
	mov    $0x1,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	mov    %ax,%si
L_37c2:
	lea    -0x6(%bp),%ax
	push   %ax
	call   L_4ce2
	pop    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0x8(%bp),%ax
	je   L_37d8
	cmpw   $0xffff,-0xa(%bp)
	jne   L_37c2
L_37d8:
	cmpw   $0xffff,-0xa(%bp)
	jne   L_37e3
	movw   $0xffff,-0x6(%bp)
L_37e3:
	push   %di
	mov    $0x2,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	push   %si
	mov    $0x3,%ax
	push   %ax
	call   L_3846
	add    $0x4,%sp
	mov    -0x6(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3804:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall exece(59) */
	mov    0x4(%bp),%ax
	mov    0xd60,%cx
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
	jns    L_381f
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_381f:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3822:
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
	jns    L_3833
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3833:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3836:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall fork(2) */
	/* fork -> __NR_fork */
	mov	$__NR_fork,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3844
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3844:
	pop    %bp
	ret


L_3846:
	push   %bp
	mov    %sp,%bp
	mov    $0x16,%cx
	mov    0x4(%bp),%ax
	or     %ax,%ax
	jle   L_3889
	cmp    $0x20,%ax
	jg   L_3889
	mov    %ax,%cx
	shl    %ax
	/* TODO: data ref: add    $0x37c8,%ax */
	add    $D_37c8,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    0x6(%bp),%ax
	mov    %ax,(%bx)
	cmp    $0x2,%ax
	jl   L_3875
	mov    $0x6,%ax
	mul    %cx
	/* TODO: data ref: add    $0x388c,%ax */
	add    $D_388c,%ax
L_3875:
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
	jl   L_3886
	pop    %ax
	jmp   L_3887
L_3886:
	pop    %bx
L_3887:
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3890
L_3889:
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	mov    $0xffff,%ax
L_3890:
	pop    %bp
	ret
L_3892:
	push   %ax
	/* TODO: data ref: mov    $0x37ca,%ax */
	mov    $D_37ca,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37cc,%ax */
	mov    $D_37cc,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37ce,%ax */
	mov    $D_37ce,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37d0,%ax */
	mov    $D_37d0,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37d2,%ax */
	mov    $D_37d2,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37d4,%ax */
	mov    $D_37d4,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37d6,%ax */
	mov    $D_37d6,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37d8,%ax */
	mov    $D_37d8,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37da,%ax */
	mov    $D_37da,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37dc,%ax */
	mov    $D_37dc,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37de,%ax */
	mov    $D_37de,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37e0,%ax */
	mov    $D_37e0,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37e2,%ax */
	mov    $D_37e2,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37e4,%ax */
	mov    $D_37e4,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37e6,%ax */
	mov    $D_37e6,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37e8,%ax */
	mov    $D_37e8,%ax
	jmp   L_38f6
	push   %ax
	/* TODO: data ref: mov    $0x37ea,%ax */
	mov    $D_37ea,%ax
L_38f6:
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x37ec,%ax */
	mov    $D_37ec,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x37ee,%ax */
	mov    $D_37ee,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x37f0,%ax */
	mov    $D_37f0,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x37f2,%ax */
	mov    $D_37f2,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x37f4,%ax */
	mov    $D_37f4,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x37f6,%ax */
	mov    $D_37f6,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x37f8,%ax */
	mov    $D_37f8,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x37fa,%ax */
	mov    $D_37fa,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x37fc,%ax */
	mov    $D_37fc,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x37fe,%ax */
	mov    $D_37fe,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x3800,%ax */
	mov    $D_3800,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x3802,%ax */
	mov    $D_3802,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x3804,%ax */
	mov    $D_3804,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x3806,%ax */
	mov    $D_3806,%ax
	jmp   L_3950
	push   %ax
	/* TODO: data ref: mov    $0x3808,%ax */
	mov    $D_3808,%ax
L_3950:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	/* TODO: data ref: sub    $0x37c8,%ax */
	sub    $D_37c8,%ax
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


L_3966:
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
L_3981:
	pop    %si
	pop    %bp
	ret


L_3984:
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
	jns    L_3994
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_39a0
L_3994:
	mov    0x4(%bp),%bx
	or     %bx,%bx
	je   L_39a0
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_39a0:
	pop    %bp
	ret


L_39a2:
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
	call   L_39f4
	add    $0x6,%sp
	decw   -0xa(%bp)
	jl   L_39dc
	mov    -0xc(%bp),%bx
	incw   -0xc(%bp)
	movb   $0x0,(%bx)
	xor    %ax,%ax
	jmp   L_39e9
L_39dc:
	lea    -0xc(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_46be
	add    $0x4,%sp
L_39e9:
	mov    0x4(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_39f4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3a0c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_3a1e
	jmp   L_3fbb
L_3a1e:
	cmpb   $0x25,-0x5(%bp)
	je   L_3a31
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_3a2f:
	jmp   L_3a0c
L_3a31:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_3a5c
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_45cc
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3a5c:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_3a75
	incb   -0xa6(%bp)
	inc    %di
L_3a75:
	cmpb   $0x30,(%di)
	jne   L_3a80
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_3a80:
	cmpb   $0x2a,(%di)
	je   L_3ab0
	movw   $0x0,-0xe(%bp)
L_3a8a:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_3ac6
	cmpw   $0x9,-0xc(%bp)
	jg   L_3ac6
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_3a8a
L_3ab0:
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
L_3ac6:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_3b0d
	cmpb   $0x2a,(%di)
	je   L_3b0f
	movw   $0x0,-0x10(%bp)
L_3adf:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_3b09
	cmpw   $0x9,-0xc(%bp)
	jg   L_3b09
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_3adf
L_3b09:
	addw   $0x30,-0xc(%bp)
L_3b0d:
	jmp   L_3b26
L_3b0f:
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
L_3b26:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_3f30
L_3b31:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_3b42
L_3b39:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_3b72
	jmp   L_3b6b
L_3b42:
	cmp    $0x64,%ax
	je   L_3b74
	cmp    $0x6f,%ax
	je   L_3b58
	cmp    $0x75,%ax
	je   L_3b6b
	cmp    $0x78,%ax
	je   L_3b66
	jmp   L_3b39
L_3b58:
	movw   $0x8,-0x12(%bp)
	jmp   L_3b6b
L_3b5f:
	movw   $0x8,-0x12(%bp)
	jmp   L_3b72
L_3b66:
	movw   $0x10,-0x12(%bp)
L_3b6b:
	jmp   L_3bbd
L_3b6d:
	movw   $0x10,-0x12(%bp)
L_3b72:
	jmp   L_3bcd
L_3b74:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_3b91
L_3b84:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_3b91:
	cmpw   $0x0,-0x14(%bp)
	jg   L_3bdb
	jl   L_3b9f
	cmpw   $0x0,-0x16(%bp)
	jae   L_3bdb
L_3b9f:
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
	jmp   L_3bdb
L_3bbd:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_3bdb
L_3bcd:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_3bdb:
	cmpw   $0x0,-0x14(%bp)
	jne   L_3be5
	cmpw   $0x0,-0x16(%bp)
L_3be5:
	jne   L_3bf6
	cmpw   $0x0,-0x10(%bp)
	jne   L_3bf6
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_3bf6:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_3bfd:
	cmpw   $0x0,-0x14(%bp)
	jne   L_3c07
	cmpw   $0x0,-0x16(%bp)
L_3c07:
	je   L_3c52
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_411a
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_3c28
	mov    $0x30,%ax
	jmp   L_3c2b
L_3c28:
	mov    $0x57,%ax
L_3c2b:
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
	call   L_4012
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_3bfd
L_3c52:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_3ca1
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_3c52
L_3c70:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_3c7c:
	cmpw   $0x2,-0xc(%bp)
	jge   L_3ca1
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_3c9c
	decw   -0x8(%bp)
L_3c9c:
	incw   -0xc(%bp)
	jmp   L_3c7c
L_3ca1:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_3ca8:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_3cbf
	movw   $0x0,-0xc(%bp)
L_3cbf:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_3cce
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_3cce:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_45cc
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3cf1:
	jmp   L_3a2f
L_3cf4:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_3d06
	movw   $L_0c06,-0xa(%bp)
L_3d06:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_3d15
	movw   $0x7fff,-0xc(%bp)
L_3d15:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3d1b:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_3ca8
	decw   -0xc(%bp)
	jl   L_3ca8
	incw   -0x8(%bp)
	jmp   L_3d1b
L_3d2d:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_3d35:
	jmp   L_3cf1
L_3d37:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3d43
	movw   $0x6,-0x10(%bp)
L_3d43:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4318
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_3d78
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_3d78:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_3d8d
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_3da5
L_3d8d:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_3d8d
L_3da5:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_3db8
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_3db8:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_3dd7
L_3dc4:
	decw   -0xc(%bp)
	jl   L_3dd7
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_3dc4
L_3dd7:
	cmpw   $0x0,-0xc(%bp)
	jg   L_3de0
	jmp   L_3ca1
L_3de0:
	decw   -0xc(%bp)
	jge   L_3de8
	jmp   L_3ca1
L_3de8:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_3de0
L_3dfd:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3e0b
	movw   $0x6,-0x10(%bp)
	jmp   L_3e0e
L_3e0b:
	incw   -0x10(%bp)
L_3e0e:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_42ec
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_3e43
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_3e43:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_3e4e
	incw   -0x1a(%bp)
L_3e4e:
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
L_3e70:
	decw   -0xc(%bp)
	jle   L_3e8a
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_3e70
L_3e8a:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_3ea7
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_3eb8
L_3ea7:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_3eb8:
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
	jmp   L_3ca1
L_3ee9:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3ef5
	movw   $0x6,-0x10(%bp)
L_3ef5:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4170
	add    $0xc,%sp
	add    $0x8,%si
L_3f13:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_3f13
	decw   -0x8(%bp)
	jmp   L_3ca1
L_3f24:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_3f2d:
	jmp   L_3d35
L_3f30:
	cmp    $0x65,%ax
	jne   L_3f38
	jmp   L_3dfd
L_3f38:
	jg   L_3f79
	cmp    $0x4f,%ax
	jne   L_3f42
	jmp   L_3b58
L_3f42:
	jg   L_3f59
	cmp    $0x25,%ax
	je   L_3f24
	cmp    $0x44,%ax
	jne   L_3f51
	jmp   L_3b74
L_3f51:
	cmp    $0x4c,%ax
L_3f54:
	jne   L_3f59
	jmp   L_3b31
L_3f59:
	cmp    $0x55,%ax
	jne   L_3f61
	jmp   L_3bbd
L_3f61:
	cmp    $0x58,%ax
	jne   L_3f69
	jmp   L_3b66
L_3f69:
	cmp    $0x63,%ax
	jne   L_3f71
	jmp   L_3c70
L_3f71:
	cmp    $0x64,%ax
	jne   L_3f79
	jmp   L_3b84
L_3f79:
	cmp    $0x6f,%ax
	jne   L_3f81
	jmp   L_3b5f
L_3f81:
	jg   L_3f98
	cmp    $0x66,%ax
	jne   L_3f8b
	jmp   L_3d37
L_3f8b:
	cmp    $0x67,%ax
	jne   L_3f93
	jmp   L_3ee9
L_3f93:
	cmp    $0x6c,%ax
	je   L_3f54
L_3f98:
	cmp    $0x72,%ax
	jne   L_3fa0
	jmp   L_3d2d
L_3fa0:
	cmp    $0x73,%ax
	jne   L_3fa8
	jmp   L_3cf4
L_3fa8:
	cmp    $0x75,%ax
	jne   L_3fb0
	jmp   L_3bcd
L_3fb0:
	cmp    $0x78,%ax
	jne   L_3fb8
	jmp   L_3b6d
L_3fb8:
	jmp   L_3f2d
L_3fbb:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_3fdf
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_45cc
	add    $0x8,%sp
L_3fdf:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_414e


L_3fe9:
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
	call   L_414e


L_4012:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_4030
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_4030:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_4039:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_4039
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_4049:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_4049
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_3fe9
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_408a
	jne   L_407f
	or     $0x0,%ax
	je   L_408a
L_407f:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_408a:
	pop    %ax
	mov    $0x0,%dx
	ret


L_408f:
	call   L_414e


L_4092:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_40ae
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_40ae:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_40c5
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_40c5:
	push   %dx
	push   %ax
	call   L_4012
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_40df
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_40df:
	ret
	call   L_414e


L_40e3:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_4092
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_3fe9
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_414e


L_411a:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_4012
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_3fe9
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_414e:
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


L_4170:
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
	call   L_42ec
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_41aa
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_41aa:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_41b1:
	cmpw   $0x0,-0xa(%bp)
	jle   L_41cb
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_41cb
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_41b1
L_41cb:
	cmpw   $0x0,-0x8(%bp)
	jl   L_41dc
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_41e8
L_41dc:
	cmpw   $0x0,-0x8(%bp)
	jge   L_41e6
	cmpw   $0xfffd,-0x8(%bp)
L_41e6:
	jge   L_4267
L_41e8:
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
L_4203:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_421d
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_4203
L_421d:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_4239
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_423f
L_4239:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_423f:
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
	jmp   L_42d7
L_4267:
	cmpw   $0x0,-0x8(%bp)
	jg   L_4289
	cmpb   $0x30,(%di)
	je   L_4278
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_4278:
	cmpw   $0x0,-0x8(%bp)
	jge   L_4289
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_4278
L_4289:
	movw   $0x1,-0xa(%bp)
L_428e:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_42b6
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
	jne   L_42b1
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_42b1:
	incw   -0xa(%bp)
	jmp   L_428e
L_42b6:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_42d7
L_42be:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_42d1
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_42be
L_42d1:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_42d7:
	cmpb   $0x2e,-0x1(%si)
	jne   L_42de
	dec    %si
L_42de:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_42ec:
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
	call   L_4343
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4318:
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
	call   L_4343
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4343:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_4356
	movw   $0x0,0xc(%bp)
L_4356:
	cmpw   $0x4f,0xc(%bp)
	jl   L_4361
	movw   $0x4e,0xc(%bp)
L_4361:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $0xc0e,%si
	fldl   0x4(%bp)
	fcompl 0xc5e
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_439e
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_439e:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4d32
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $L_0c5e,-0x16(%bp)
	fldl   -0xc(%bp)
	fcompl 0xc66
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_43e9
	jmp   L_4496
L_43e9:
	movw   $L_0c5e,-0x16(%bp)
L_43ee:
	fldl   -0xc(%bp)
	fcompl 0xc6e
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_440a
	jmp   L_4494
L_440a:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	fdivl  0xc76
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4d32
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   0xc7e
	fstl   -0x1e(%bp)
	fdivl  0xc86
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  0xc8e
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
	jmp   L_43ee
L_447d:
	cmpw   $L_0c5e,-0x16(%bp)
	jae   L_44d7
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_4494:
	jmp   L_447d
L_4496:
	fldl   0x4(%bp)
	fcompl 0xc96
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_44e6
L_44af:
	fldl   0x4(%bp)
	fmull  0xc9e
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl 0xca6
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_44d7:
	jae   L_44e6
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_44af
L_44e6:
	mov    0xc(%bp),%ax
	add    $0xc0e,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_44f8
	add    %di,-0x16(%bp)
L_44f8:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $L_0c0e,-0x16(%bp)
	jae   L_450e
	movb   $0x0,0xc0e
	mov    $0xc0e,%ax
	jmp   L_4579
L_450e:
	cmp    -0x16(%bp),%si
	ja   L_456a
	cmp    $0xc5e,%si
	jae   L_456a
	fldl   0x4(%bp)
	fmull  0xcae
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4d32
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
	jmp   L_450e
L_456a:
	cmpw   $L_0c5e,-0x16(%bp)
	jb   L_457b
	movb   $0x0,0xc5d
	mov    $0xc0e,%ax
L_4579:
	jmp   L_45c5
L_457b:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_4583:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_45bf
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $L_0c0e,-0x16(%bp)
	jbe   L_45a2
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_4583
L_45a2:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_4583
	cmp    $0xc0e,%si
	jbe   L_45bc
	movb   $0x30,(%si)
L_45bc:
	inc    %si
	jmp   L_4583
L_45bf:
	movb   $0x0,(%si)
	mov    $0xc0e,%ax
L_45c5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_45cc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_45d9:
	cmpw   $0x0,0x8(%bp)
	jl   L_45e2
	jmp   L_4670
L_45e2:
	cmpb   $0x2d,(%di)
	jne   L_461f
	cmpw   $0x30,0xc(%bp)
	jne   L_461f
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_460e
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
	jmp   L_461e
L_460e:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_46be
	add    $0x4,%sp
L_461e:
	dec    %si
L_461f:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_463d
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
	jmp   L_4649
L_463d:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_46be
	add    $0x4,%sp
L_4649:
	incw   0x8(%bp)
	jmp   L_45d9
L_464e:
	dec    %si
	jl   L_4684
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_4672
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
L_4670:
	jmp   L_464e
L_4672:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_46be
	add    $0x4,%sp
	jmp   L_464e
L_4684:
	cmpw   $0x0,0x8(%bp)
	je   L_46b9
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_46a8
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
	jmp   L_46b4
L_46a8:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_46be
	add    $0x4,%sp
L_46b4:
	decw   0x8(%bp)
	jmp   L_4684
L_46b9:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_46be:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_46d9
	mov    $0xffff,%ax
	jmp   L_47a0
L_46d9:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_470c
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
	call   L_4cfe
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_4791
L_470c:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_475e
	cmp    $0xcbe,%di
	jne   L_4737
	mov    0xcc5,%al
	cbtw
	push   %ax
	call   L_4b52
	pop    %cx
	or     %ax,%ax
	je   L_472c
	orb    $0x4,0x6(%di)
	jmp   L_46d9
L_472c:
	/* TODO: data ref: movw   $0x3810,0x4(%di) */
	movw   $D_3810,0x4(%di)
	/* TODO: data ref: movw   $0x3810,(%di) */
	movw   $D_3810,(%di)
	jmp   L_46d9
L_4737:
	mov    $0x200,%ax
	push   %ax
	call   L_4892
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_474e
	orb    $0x4,0x6(%di)
	jmp   L_46d9
L_474e:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_4780
L_475e:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_4780
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4cfe
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_4780:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_4791:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_47a2
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_47a0:
	jmp   L_47a5
L_47a2:
	mov    0x4(%bp),%ax
L_47a5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_47ac:
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
	jne   L_47f8
	mov    0x4(%di),%si
	or     %si,%si
	je   L_47f8
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_47f8
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4cfe
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_47f8
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_47fa
L_47f8:
	xor    %ax,%ax
L_47fa:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_47ff:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0xcb6,%di
L_4807:
	cmp    0xd56,%di
	jae   L_4817
	push   %di
	call   L_481b
	pop    %cx
	add    $0x8,%di
	jmp   L_4807
L_4817:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_481b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_486e
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_486e
	push   %di
	call   L_47ac
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4b28
	pop    %cx
	or     %ax,%ax
	jge   L_4850
	mov    $0xffff,%si
L_4850:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_4860
	pushw  0x4(%di)
	call   L_49dc
	pop    %cx
L_4860:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_486e
	movw   $0x0,0x4(%di)
L_486e:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_4892:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,0xd58
	jne   L_48bd
	mov    $0xd5a,%ax
	or     $0x1,%ax
	mov    %ax,0xd58
	mov    $0xd58,%ax
	or     $0x1,%ax
	mov    %ax,0xd5a
	/* TODO: data ref: movw   $0xd5a,0x3c12 */
	movw   $0xd5a,D_3c12
	/* TODO: data ref: movw   $0xd58,0x3c10 */
	movw   $0xd58,D_3c10
L_48bd:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    0x3c10,%di */
	mov    D_3c10,%di
L_48d1:
	movw   $0x0,0xd5c
L_48d7:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_4906
L_48de:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_48ed
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_48de
L_48ed:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_4906
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_4906
	jmp   L_49ad
L_4906:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_48d7
	/* TODO: data ref: cmp    0x3c12,%si */
	cmp    D_3c12,%si
	jne   L_491f
	cmp    $0xd58,%di
	je   L_4923
L_491f:
	xor    %ax,%ax
	jmp   L_495b
L_4923:
	incw   0xd5c
	cmpw   $0x1,0xd5c
	jle   L_48d7
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,0xd5c
	xor    %ax,%ax
	push   %ax
	call   L_4ac0
	pop    %cx
	mov    %ax,%si
	mov    0xd5c,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_495d
	xor    %ax,%ax
L_495b:
	jmp   L_4970
L_495d:
	mov    0xd5c,%ax
	shl    %ax
	push   %ax
	call   L_4ac0
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_4972
	xor    %ax,%ax
L_4970:
	jmp   L_49d7
L_4972:
	/* TODO: data ref: mov    0x3c12,%bx */
	mov    D_3c12,%bx
	mov    %si,(%bx)
	/* TODO: data ref: mov    0x3c12,%ax */
	mov    D_3c12,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_4990
	/* TODO: data ref: mov    0x3c12,%bx */
	mov    D_3c12,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x3c12,%bx */
	mov    D_3c12,%bx
	mov    %ax,(%bx)
L_4990:
	mov    0xd5c,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	/* TODO: data ref: mov    %ax,0x3c12 */
	mov    %ax,D_3c12
	mov    $0xd58,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x3c12,%bx */
	mov    D_3c12,%bx
	mov    %ax,(%bx)
	jmp   L_48d1
L_49ad:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	/* TODO: data ref: mov    %ax,0x3c10 */
	mov    %ax,D_3c10
	cmp    %ax,%si
	jbe   L_49cc
	/* TODO: data ref: mov    0x3c10,%bx */
	mov    D_3c10,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x3c14 */
	mov    %ax,D_3c14
	/* TODO: data ref: mov    0x3c10,%bx */
	mov    D_3c10,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_49cc:
	/* TODO: data ref: mov    0x3c10,%ax */
	mov    D_3c10,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_49d7:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_49dc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	/* TODO: data ref: mov    %si,0x3c10 */
	mov    %si,D_3c10
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
	je   L_4a11
	push   %di
	call   L_49dc
	pop    %cx
L_4a11:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_4892
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_4a30
	cmp    %di,%si
	jne   L_4a34
L_4a30:
	mov    %si,%ax
	jmp   L_4a9c
L_4a34:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_4a53
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_4a53:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_4a71
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_4a53
L_4a71:
	cmp    %di,%si
	jae   L_4a9a
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_4a9a
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
	/* TODO: data ref: mov    0x3c14,%ax */
	mov    D_3c14,%ax
	mov    %ax,(%bx)
L_4a9a:
	mov    %si,%ax
L_4a9c:
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
	jns    L_4ab7
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_4abe
L_4ab7:
	mov    0x4(%bp),%bx
	mov    %bx,0xd5e
L_4abe:
	pop    %bp
	ret


L_4ac0:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    0xd5e,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_4ad7
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_4ae4
L_4ad7:
	mov    0xd5e,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,0xd5e
L_4ae4:
	pop    %bp
	ret


L_4ae6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_4af3:
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_4b12
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_4af3
	xor    %ax,%ax
	jmp   L_4b23
L_4b12:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_4b23:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4b28:
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
	jns    L_4b39
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4b39:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4b3c:
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
	jns    L_4b50
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4b50:
	pop    %bp
	ret


L_4b52:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c37
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_4b6f
	xor    %ax,%ax
	jmp   L_4b72
L_4b6f:
	mov    $0x1,%ax
L_4b72:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4b7a:
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
	je	open_wronly_5_b190
	and	$0xfffc,%cx
	or	$0x241,%cx
	mov	$0x1b6,%dx
open_wronly_5_b190:
	mov	$__NR_open,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_4b8e
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4b8e:
	pop    %bp
	ret


L_4b90:
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
	jns    L_4ba7
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4ba7:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4baa:
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
	jns    L_4bbe
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4bbe:
	pop    %bp
	ret


L_4bc0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x6(%bp)
L_4bd1:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_4bd1
	dec    %di
L_4bda:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_4bda
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4bf4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x6(%bp)
L_4c05:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_4c05
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
	call   L_4c50
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4c37:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c50
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4c50:
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
	jns    L_4c67
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4c67:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4c6a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x6(%bp),%di
	cmpw   $0xffff,0x4(%bp)
	jne   L_4c7e
	mov    $0xffff,%ax
	jmp   L_4cb4
L_4c7e:
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	je   L_4c8e
	mov    (%di),%ax
	cmp    0x4(%di),%ax
	ja   L_4ca4
L_4c8e:
	mov    (%di),%ax
	cmp    0x4(%di),%ax
	jne   L_4c9f
	cmpw   $0x0,0x2(%di)
	jne   L_4c9f
	incw   (%di)
	jmp   L_4ca4
L_4c9f:
	mov    $0xffff,%ax
	jmp   L_4cb4
L_4ca4:
	incw   0x2(%di)
	decw   (%di)
	mov    (%di),%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	xor    %ax,%ax
L_4cb4:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4cb8:
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
	jns    L_4cc9
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4cc9:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4ccc:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall utime(30) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* utime -> __NR_utime */
	xchg	%ax,%bx
	mov	%dx,%cx
	mov	$__NR_utime,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_4ce0
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4ce0:
	pop    %bp
	ret


L_4ce2:
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
	jns    L_4cf2
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4cf0:
	pop    %bp
	ret
L_4cf2:
	mov    0x4(%bp),%bx
	cmp    $0x0,%bx
	je   L_4cf0
	mov    %dx,(%bx)
	jmp   L_4cf0


L_4cfe:
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
	jns    L_4d15
	neg	%ax
	mov	%ax,D_37c6	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4d15:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4d18:
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


L_4d32:
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
	mov	%ax,D_37c6	/* errno */
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
	.word	D_0fbf	/* ptr: 0x0fbf */
	.byte	0x80, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00	/* |........| */
	.byte	0x01, 0x00	/* |..| */
	.ascii	"/tmp/tarXXXXXX\0"
	.byte	0x00	/* |.| */
	.ascii	"/dev/f0\0"
	.byte	0x01, 0x00, 0x00, 0x01, 0x72, 0x00, 0x2d, 0x00	/* |....r.-.| */
	.byte	0x01, 0x00, 0x80, 0x00, 0x77, 0x00, 0x2d, 0x00	/* |....w.-.| */
	.byte	0x02, 0x00, 0x00, 0x08, 0x73, 0x00, 0x40, 0x00	/* |....s.@.| */
	.byte	0x78, 0x00, 0x2d, 0x00, 0x01, 0x00, 0x20, 0x00	/* |x.-... .| */
	.byte	0x72, 0x00, 0x2d, 0x00, 0x01, 0x00, 0x10, 0x00	/* |r.-.....| */
	.byte	0x77, 0x00, 0x2d, 0x00, 0x02, 0x00, 0x00, 0x04	/* |w.-.....| */
	.byte	0x73, 0x00, 0x08, 0x00, 0x78, 0x00, 0x2d, 0x00	/* |s...x.-.| */
	.byte	0x01, 0x00, 0x04, 0x00, 0x72, 0x00, 0x2d, 0x00	/* |....r.-.| */
	.byte	0x01, 0x00, 0x02, 0x00, 0x77, 0x00, 0x2d, 0x00	/* |....w.-.| */
	.byte	0x02, 0x00, 0x00, 0x02, 0x74, 0x00, 0x01, 0x00	/* |....t...| */
	.byte	0x78, 0x00, 0x2d, 0x00, 0x2e, 0x00, 0x36, 0x00	/* |x.-...6.| */
	.byte	0x3e, 0x00, 0x4a, 0x00, 0x52, 0x00, 0x5a, 0x00	/* |>.J.R.Z.| */
	.byte	0x66, 0x00, 0x6e, 0x00, 0x76, 0x00, 0x77, 0x00	/* |f.n.v.w.| */
	.byte	0x54, 0x61, 0x72, 0x3a, 0x20, 0x63, 0x61, 0x6e	/* |Tar: can| */
	.byte	0x6e, 0x6f, 0x74, 0x20, 0x63, 0x72, 0x65, 0x61	/* |not crea| */
	.byte	0x74, 0x65, 0x20, 0x74, 0x65, 0x6d, 0x70, 0x6f	/* |te tempo| */
	.byte	0x72, 0x61, 0x72, 0x79, 0x20, 0x66, 0x69, 0x6c	/* |rary fil| */
	.byte	0x65, 0x20, 0x28, 0x25, 0x73, 0x29, 0x0a, 0x00	/* |e (%s)..| */
	.byte	0x21, 0x21, 0x21, 0x21, 0x21, 0x2f, 0x21, 0x2f	/* |!!!!!/!/| */
	.byte	0x21, 0x2f, 0x21, 0x2f, 0x21, 0x2f, 0x21, 0x2f	/* |!/!/!/!/| */
	.byte	0x21, 0x2f, 0x21, 0x20, 0x30, 0x30, 0x30, 0x0a	/* |!/! 000.| */
	.byte	0x00, 0x54, 0x61, 0x72, 0x3a, 0x20, 0x42, 0x6c	/* |.Tar: Bl| */
	.byte	0x6f, 0x63, 0x6b, 0x65, 0x64, 0x20, 0x6d, 0x65	/* |ocked me| */
	.byte	0x64, 0x69, 0x61, 0x20, 0x63, 0x61, 0x6e, 0x6e	/* |dia cann| */
	.byte	0x6f, 0x74, 0x20, 0x62, 0x65, 0x20, 0x75, 0x70	/* |ot be up| */
	.byte	0x64, 0x61, 0x74, 0x65, 0x64, 0x20, 0x28, 0x79	/* |dated (y| */
	.byte	0x65, 0x74, 0x29, 0x0a, 0x00, 0x74, 0x61, 0x72	/* |et)..tar| */
	.byte	0x3a, 0x20, 0x69, 0x6e, 0x76, 0x61, 0x6c, 0x69	/* |: invali| */
	.byte	0x64, 0x20, 0x62, 0x6c, 0x6f, 0x63, 0x6b, 0x73	/* |d blocks| */
	.byte	0x69, 0x7a, 0x65, 0x2e, 0x20, 0x28, 0x4d, 0x61	/* |ize. (Ma| */
	.byte	0x78, 0x20, 0x25, 0x64, 0x29, 0x0a, 0x00, 0x74	/* |x %d)..t| */
	.byte	0x61, 0x72, 0x3a, 0x20, 0x69, 0x6e, 0x76, 0x61	/* |ar: inva| */
	.byte	0x6c, 0x69, 0x64, 0x20, 0x64, 0x65, 0x76, 0x69	/* |lid devi| */
	.byte	0x63, 0x65, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x0a	/* |ce size.| */
	.byte	0x00, 0x74, 0x61, 0x72, 0x3a, 0x20, 0x25, 0x63	/* |.tar: %c| */
	.byte	0x3a, 0x20, 0x75, 0x6e, 0x6b, 0x6e, 0x6f, 0x77	/* |: unknow| */
	.byte	0x6e, 0x20, 0x6f, 0x70, 0x74, 0x69, 0x6f, 0x6e	/* |n option| */
	.byte	0x0a, 0x00, 0x74, 0x61, 0x72, 0x3a, 0x20, 0x64	/* |..tar: d| */
	.byte	0x65, 0x76, 0x69, 0x63, 0x65, 0x20, 0x73, 0x69	/* |evice si| */
	.byte	0x7a, 0x65, 0x20, 0x74, 0x6f, 0x6f, 0x20, 0x73	/* |ze too s| */
	.byte	0x6d, 0x61, 0x6c, 0x6c, 0x0a, 0x00, 0x2d, 0x00	/* |mall..-.| */
	.byte	0x43, 0x61, 0x6e, 0x20, 0x6f, 0x6e, 0x6c, 0x79	/* |Can only| */
	.byte	0x20, 0x63, 0x72, 0x65, 0x61, 0x74, 0x65, 0x20	/* | create | */
	.byte	0x73, 0x74, 0x61, 0x6e, 0x64, 0x61, 0x72, 0x64	/* |standard| */
	.byte	0x20, 0x6f, 0x75, 0x74, 0x70, 0x75, 0x74, 0x20	/* | output | */
	.byte	0x61, 0x72, 0x63, 0x68, 0x69, 0x76, 0x65, 0x73	/* |archives| */
	.byte	0x0a, 0x00, 0x74, 0x61, 0x72, 0x3a, 0x20, 0x63	/* |..tar: c| */
	.byte	0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6f, 0x70	/* |annot op| */
	.byte	0x65, 0x6e, 0x20, 0x25, 0x73, 0x0a, 0x00, 0x2d	/* |en %s..-| */
	.byte	0x00, 0x74, 0x61, 0x72, 0x3a, 0x20, 0x63, 0x61	/* |.tar: ca| */
	.byte	0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6f, 0x70, 0x65	/* |nnot ope| */
	.byte	0x6e, 0x20, 0x25, 0x73, 0x0a, 0x00, 0x52, 0x65	/* |n %s..Re| */
	.byte	0x6d, 0x6f, 0x76, 0x65, 0x20, 0x74, 0x68, 0x65	/* |move the| */
	.byte	0x20, 0x58, 0x46, 0x45, 0x52, 0x20, 0x66, 0x6c	/* | XFER fl| */
	.byte	0x6f, 0x70, 0x70, 0x79, 0x20, 0x61, 0x6e, 0x64	/* |oppy and| */
	.byte	0x20, 0x69, 0x6e, 0x73, 0x65, 0x72, 0x74, 0x20	/* | insert | */
	.byte	0x53, 0x59, 0x53, 0x54, 0x45, 0x4d, 0x20, 0x41	/* |SYSTEM A| */
	.byte	0x20, 0x66, 0x6c, 0x6f, 0x70, 0x70, 0x79, 0x20	/* | floppy | */
	.byte	0x69, 0x6e, 0x20, 0x64, 0x72, 0x69, 0x76, 0x65	/* |in drive| */
	.byte	0x20, 0x30, 0x2e, 0x0a, 0x00, 0x52, 0x65, 0x6d	/* | 0...Rem| */
	.byte	0x6f, 0x76, 0x65, 0x20, 0x74, 0x68, 0x65, 0x20	/* |ove the | */
	.byte	0x58, 0x46, 0x45, 0x52, 0x20, 0x66, 0x6c, 0x6f	/* |XFER flo| */
	.byte	0x70, 0x70, 0x79, 0x20, 0x61, 0x6e, 0x64, 0x20	/* |ppy and | */
	.byte	0x69, 0x6e, 0x73, 0x65, 0x72, 0x74, 0x20, 0x55	/* |insert U| */
	.byte	0x53, 0x45, 0x52, 0x20, 0x31, 0x20, 0x66, 0x6c	/* |SER 1 fl| */
	.byte	0x6f, 0x70, 0x70, 0x79, 0x20, 0x69, 0x6e, 0x20	/* |oppy in | */
	.byte	0x64, 0x72, 0x69, 0x76, 0x65, 0x20, 0x30, 0x2e	/* |drive 0.| */
	.byte	0x0a, 0x00	/* |..| */
	.ascii	"Press `return' to begin the tar extract to the winchester. \0"
	.byte	0x0a	/* |.| */
	.ascii	"Starting ...   \0"
	.byte	0x49, 0x6e, 0x73, 0x65, 0x72	/* |Inser| */
	.word	D_2074	/* ptr: 0x2074 */
	.byte	0x66, 0x6c, 0x6f, 0x70, 0x70, 0x79	/* |floppy| */
	.word	D_2520	/* ptr: 0x2520 */
	.word	D_2063	/* ptr: 0x2063 */
	.byte	0x61, 0x6e	/* |an| */
	.word	D_2064	/* ptr: 0x2064 */
	.byte	0x70, 0x72, 0x65, 0x73	/* |pres| */
	.word	D_2073	/* ptr: 0x2073 */
	.byte	0x60, 0x72, 0x65, 0x74, 0x75, 0x72	/* |`retur| */
	.word	D_276e	/* ptr: 0x276e */
	.byte	0x07, 0x07, 0x00, 0x0a	/* |....| */
	.ascii	"Continuing ...   \0"
	.byte	0x0a, 0x00, 0x2d, 0x00, 0x74, 0x61, 0x72, 0x3a	/* |..-.tar:| */
	.byte	0x20, 0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20	/* | cannot | */
	.byte	0x6f, 0x70, 0x65, 0x6e, 0x20, 0x25, 0x73, 0x0a	/* |open %s.| */
	.byte	0x00, 0x55, 0x73, 0x61, 0x67, 0x65, 0x3a, 0x20	/* |.Usage: | */
	.byte	0x74, 0x61, 0x72, 0x20, 0x5b, 0x63, 0x72, 0x74	/* |tar [crt| */
	.byte	0x75, 0x78, 0x20, 0x5b, 0x62, 0x64, 0x66, 0x6c	/* |ux [bdfl| */
	.byte	0x6d, 0x73, 0x76, 0x77, 0x5d, 0x5d, 0x20, 0x5b	/* |msvw]] [| */
	.byte	0x61, 0x72, 0x67, 0x75, 0x6d, 0x65, 0x6e, 0x74	/* |argument| */
	.byte	0x73, 0x5d, 0x20, 0x5b, 0x66, 0x69, 0x6c, 0x65	/* |s] [file| */
	.byte	0x73, 0x5d, 0x0a, 0x00	/* |s]..| */
	.ascii	"tar: saving files modified since %s\0"
	.ascii	"sort +0 -1 +1nr %s -o %s; awk '$1 != prev {print; prev=$1}' %s >%sX;mv %sX %s\0"
	.byte	0x72, 0x00, 0x74, 0x61, 0x72, 0x3a, 0x20, 0x63	/* |r.tar: c| */
	.byte	0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x61, 0x63	/* |annot ac| */
	.byte	0x63, 0x65, 0x73, 0x73, 0x20, 0x25, 0x73, 0x0a	/* |cess %s.| */
	.byte	0x00, 0x4d, 0x69, 0x73, 0x73, 0x69, 0x6e, 0x67	/* |.Missing| */
	.byte	0x20, 0x6c, 0x69, 0x6e, 0x6b, 0x73, 0x20, 0x74	/* | links t| */
	.byte	0x6f, 0x20, 0x25, 0x73, 0x0a, 0x00, 0x25, 0x44	/* |o %s..%D| */
	.byte	0x20, 0x62, 0x6c, 0x6f, 0x63, 0x6b, 0x73, 0x20	/* | blocks | */
	.byte	0x75, 0x73, 0x65, 0x64, 0x20, 0x6f, 0x6e, 0x20	/* |used on | */
	.byte	0x6d, 0x65, 0x64, 0x69, 0x75, 0x6d, 0x0a, 0x00	/* |medium..| */
	.byte	0x25, 0x6f	/* |%o| */
	.word	D_2500	/* ptr: 0x2500 */
	.byte	0x6f, 0x00, 0x25, 0x6f	/* |o.%o| */
	.word	D_2500	/* ptr: 0x2500 */
	.byte	0x6c, 0x6f	/* |lo| */
	.word	D_2500	/* ptr: 0x2500 */
	.byte	0x6c, 0x6f	/* |lo| */
	.word	D_2500	/* ptr: 0x2500 */
	.byte	0x6f, 0x00	/* |o.| */
	.ascii	"SOFT\0"
	.byte	0x75, 0x6e, 0x72, 0x65, 0x63, 0x6f, 0x76, 0x65	/* |unrecove| */
	.byte	0x72, 0x61, 0x62, 0x6c, 0x65, 0x20, 0x64, 0x69	/* |rable di| */
	.byte	0x72, 0x65, 0x63, 0x74, 0x6f, 0x72, 0x79, 0x20	/* |rectory | */
	.byte	0x63, 0x68, 0x65, 0x63, 0x6b, 0x73, 0x75, 0x6d	/* |checksum| */
	.byte	0x20, 0x65, 0x72, 0x72, 0x6f, 0x72, 0x0a, 0x00	/* | error..| */
	.byte	0x25, 0x73, 0x20, 0x25, 0x73, 0x0a, 0x00, 0x74	/* |%s %s..t| */
	.byte	0x61, 0x72, 0x3a, 0x20, 0x25, 0x73, 0x3a, 0x20	/* |ar: %s: | */
	.byte	0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6f	/* |cannot o| */
	.byte	0x70, 0x65, 0x6e, 0x20, 0x66, 0x69, 0x6c, 0x65	/* |pen file| */
	.byte	0x0a, 0x00, 0x74, 0x61, 0x72, 0x3a, 0x20, 0x25	/* |..tar: %| */
	.byte	0x73, 0x3a, 0x20, 0x63, 0x61, 0x6e, 0x6e, 0x6f	/* |s: canno| */
	.byte	0x74, 0x20, 0x6f, 0x70, 0x65, 0x6e, 0x20, 0x66	/* |t open f| */
	.byte	0x69, 0x6c, 0x65, 0x0a, 0x00, 0x2e, 0x00, 0x2e	/* |ile.....| */
	.byte	0x00, 0x2e, 0x00	/* |...| */
	.word	D_2e2e	/* ptr: 0x2e2e */
	.word	D_2e00	/* ptr: 0x2e00 */
	.word	D_2e00	/* ptr: 0x2e00 */
	.byte	0x2e, 0x00	/* |..| */
	.word	D_3625	/* ptr: 0x3625 */
	.byte	0x6f, 0x00, 0x74, 0x61, 0x72, 0x3a, 0x20, 0x25	/* |o.tar: %| */
	.byte	0x73, 0x3a, 0x20, 0x66, 0x69, 0x6c, 0x65, 0x20	/* |s: file | */
	.byte	0x6e, 0x61, 0x6d, 0x65, 0x20, 0x74, 0x6f, 0x6f	/* |name too| */
	.byte	0x20, 0x6c, 0x6f, 0x6e, 0x67, 0x0a, 0x00, 0x25	/* | long..%| */
	.byte	0x36, 0x6f, 0x00, 0x61, 0x20, 0x25, 0x73, 0x20	/* |6o.a %s | */
	.byte	0x6c, 0x69, 0x6e, 0x6b, 0x20, 0x74, 0x6f, 0x20	/* |link to | */
	.byte	0x25, 0x73, 0x0a, 0x00, 0x54, 0x61, 0x72, 0x3a	/* |%s..Tar:| */
	.byte	0x20, 0x6f, 0x75, 0x74, 0x20, 0x6f, 0x66, 0x20	/* | out of | */
	.byte	0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x2e, 0x20	/* |memory. | */
	.byte	0x4c, 0x69, 0x6e, 0x6b, 0x20, 0x69, 0x6e, 0x66	/* |Link inf| */
	.byte	0x6f, 0x72, 0x6d, 0x61, 0x74, 0x69, 0x6f, 0x6e	/* |ormation| */
	.byte	0x20, 0x6c, 0x6f, 0x73, 0x74, 0x0a, 0x00, 0x25	/* | lost..%| */
	.byte	0x36, 0x6f, 0x00, 0x25, 0x73, 0x3a, 0x20, 0x74	/* |6o.%s: t| */
	.byte	0x6f, 0x6f, 0x20, 0x62, 0x69, 0x67, 0x20, 0x74	/* |oo big t| */
	.byte	0x6f, 0x20, 0x64, 0x75, 0x6d, 0x70, 0x0a, 0x00	/* |o dump..| */
	.byte	0x25, 0x44, 0x20, 0x62, 0x6c, 0x6f, 0x63, 0x6b	/* |%D block| */
	.byte	0x73, 0x20, 0x75, 0x73, 0x65, 0x64, 0x20, 0x6f	/* |s used o| */
	.byte	0x6e, 0x20, 0x6d, 0x65, 0x64, 0x69, 0x75, 0x6d	/* |n medium| */
	.byte	0x0a, 0x00, 0x49, 0x6e, 0x73, 0x65, 0x72, 0x74	/* |..Insert| */
	.byte	0x20, 0x6d, 0x65, 0x64, 0x69, 0x75	/* | mediu| */
	.word	D_206d	/* ptr: 0x206d */
	.word	D_2523	/* ptr: 0x2523 */
	.word	D_2064	/* ptr: 0x2064 */
	.byte	0x61, 0x6e	/* |an| */
	.word	D_2064	/* ptr: 0x2064 */
	.byte	0x70, 0x72, 0x65, 0x73	/* |pres| */
	.word	D_2073	/* ptr: 0x2073 */
	.byte	0x60, 0x72, 0x65, 0x74, 0x75, 0x72	/* |`retur| */
	.word	D_276e	/* ptr: 0x276e */
	.word	D_2e20	/* ptr: 0x2e20 */
	.word	D_2e2e	/* ptr: 0x2e2e */
	.byte	0x07, 0x07, 0x0a, 0x00, 0x43, 0x61, 0x6e, 0x6e	/* |....Cann| */
	.byte	0x6f, 0x74, 0x20, 0x72, 0x65, 0x6f, 0x70, 0x65	/* |ot reope| */
	.byte	0x6e, 0x20, 0x25, 0x73, 0x0a, 0x00, 0x0a, 0x4e	/* |n %s...N| */
	.byte	0x65, 0x77, 0x20, 0x6d, 0x65, 0x64, 0x69, 0x75	/* |ew mediu| */
	.byte	0x6d, 0x0a, 0x00, 0x61, 0x20, 0x25, 0x73, 0x20	/* |m..a %s | */
	.byte	0x25, 0x6c, 0x64, 0x20, 0x62, 0x6c, 0x6f, 0x63	/* |%ld bloc| */
	.byte	0x6b, 0x73, 0x0a, 0x00, 0x74, 0x61, 0x72, 0x3a	/* |ks..tar:| */
	.byte	0x20, 0x25, 0x73, 0x3a, 0x20, 0x66, 0x69, 0x6c	/* | %s: fil| */
	.byte	0x65, 0x20, 0x63, 0x68, 0x61, 0x6e, 0x67, 0x65	/* |e change| */
	.byte	0x64, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x0a, 0x00	/* |d size..| */
	.byte	0x74, 0x61, 0x72, 0x3a, 0x20, 0x65, 0x78, 0x74	/* |tar: ext| */
	.byte	0x72, 0x61, 0x63, 0x74, 0x69, 0x6e, 0x67, 0x20	/* |racting | */
	.byte	0x66, 0x69, 0x6c, 0x65, 0x73, 0x20, 0x6d, 0x6f	/* |files mo| */
	.byte	0x64, 0x69, 0x66, 0x69, 0x65, 0x64, 0x20, 0x73	/* |dified s| */
	.byte	0x69, 0x6e, 0x63, 0x65, 0x20, 0x25, 0x73, 0x0a	/* |ince %s.| */
	.byte	0x00, 0x78, 0x20, 0x25, 0x73, 0x0a, 0x00, 0x74	/* |.x %s..t| */
	.byte	0x61, 0x72, 0x3a, 0x20, 0x25, 0x73, 0x3a, 0x20	/* |ar: %s: | */
	.byte	0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6c	/* |cannot l| */
	.byte	0x69, 0x6e, 0x6b, 0x0a, 0x00, 0x25, 0x73, 0x20	/* |ink..%s | */
	.byte	0x6c, 0x69, 0x6e, 0x6b, 0x65, 0x64, 0x20, 0x74	/* |linked t| */
	.byte	0x6f, 0x20, 0x25, 0x73, 0x0a, 0x00, 0x74, 0x61	/* |o %s..ta| */
	.byte	0x72, 0x3a, 0x20, 0x6d, 0x75, 0x73, 0x74, 0x20	/* |r: must | */
	.byte	0x62, 0x65, 0x20, 0x73, 0x75, 0x70, 0x65, 0x72	/* |be super| */
	.byte	0x2d, 0x75, 0x73, 0x65, 0x72, 0x20, 0x74, 0x6f	/* |-user to| */
	.byte	0x20, 0x63, 0x72, 0x65, 0x61, 0x74, 0x65, 0x20	/* | create | */
	.byte	0x64, 0x65, 0x76, 0x69, 0x63, 0x65, 0x20, 0x6e	/* |device n| */
	.byte	0x6f, 0x64, 0x65, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |ode %s..| */
	.word	D_3625	/* ptr: 0x3625 */
	.byte	0x6f, 0x00, 0x74, 0x61, 0x72, 0x3a, 0x20, 0x25	/* |o.tar: %| */
	.byte	0x73, 0x20, 0x61, 0x6c, 0x72, 0x65, 0x61, 0x64	/* |s alread| */
	.byte	0x79, 0x20, 0x65, 0x78, 0x69, 0x73, 0x74, 0x73	/* |y exists| */
	.byte	0x0a, 0x00, 0x78, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |..x %s..| */
	.byte	0x74, 0x61, 0x72, 0x3a, 0x20, 0x25, 0x73, 0x20	/* |tar: %s | */
	.byte	0x2d, 0x20, 0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74	/* |- cannot| */
	.byte	0x20, 0x63, 0x72, 0x65, 0x61, 0x74, 0x65, 0x0a	/* | create.| */
	.byte	0x00, 0x78, 0x20, 0x25, 0x73, 0x2c, 0x20, 0x25	/* |.x %s, %| */
	.byte	0x6c, 0x64, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73	/* |ld bytes| */
	.byte	0x2c, 0x20, 0x25, 0x6c, 0x64, 0x20, 0x62, 0x6c	/* |, %ld bl| */
	.byte	0x6f, 0x63, 0x6b, 0x73, 0x0a, 0x00, 0x74, 0x61	/* |ocks..ta| */
	.byte	0x72, 0x3a, 0x20, 0x25, 0x73, 0x3a, 0x20, 0x65	/* |r: %s: e| */
	.byte	0x78, 0x74, 0x72, 0x61, 0x63, 0x74, 0x20, 0x77	/* |xtract w| */
	.byte	0x72, 0x69, 0x74, 0x65, 0x20, 0x65, 0x72, 0x72	/* |rite err| */
	.byte	0x6f, 0x72, 0x0a, 0x00, 0x74, 0x61, 0x72, 0x3a	/* |or..tar:| */
	.byte	0x20, 0x25, 0x73, 0x3a, 0x20, 0x65, 0x78, 0x74	/* | %s: ext| */
	.byte	0x72, 0x61, 0x63, 0x74, 0x20, 0x77, 0x72, 0x69	/* |ract wri| */
	.byte	0x74, 0x65, 0x20, 0x65, 0x72, 0x72, 0x6f, 0x72	/* |te error| */
	.byte	0x0a, 0x00, 0x25, 0x73, 0x00	/* |..%s.| */
	.ascii	" linked to %s\0"
	.byte	0x0a, 0x00, 0x25, 0x44, 0x20, 0x62, 0x6c, 0x6f	/* |..%D blo| */
	.byte	0x63, 0x6b, 0x73, 0x20, 0x75, 0x73, 0x65, 0x64	/* |cks used| */
	.byte	0x20, 0x6f, 0x6e, 0x20, 0x6d, 0x65, 0x64, 0x69	/* | on medi| */
	.byte	0x75, 0x6d, 0x0a, 0x00	/* |um..| */
	.ascii	"%3d/%1d\0"
	.byte	0x25, 0x6f, 0x00	/* |%o.| */
	.ascii	"%3d,%3d\0"
	.word	D_3725	/* ptr: 0x3725 */
	.byte	0x44, 0x00	/* |D.| */
	.ascii	" %-12.12s %-4.4s \0"
	.byte	0x25, 0x63	/* |%c| */
	.word	D_2500	/* ptr: 0x2500 */
	.byte	0x63, 0x00	/* |c.| */
	.ascii	"%6o \0"
	.ascii	"%6o \0"
	.ascii	"%6o \0"
	.ascii	"%11lo \0"
	.ascii	"%11lo \0"
	.byte	0x25	/* |%| */
	.word	D_2063	/* ptr: 0x2063 */
	.byte	0x00	/* |.| */
	.ascii	"%s: \0"
	.ascii	"%s %lo\0"
	.byte	0x52, 0x65, 0x6d, 0x6f, 0x76, 0x65, 0x20, 0x74	/* |Remove t| */
	.byte	0x68, 0x65, 0x20, 0x53, 0x59, 0x53, 0x54, 0x45	/* |he SYSTE| */
	.byte	0x4d, 0x20, 0x66, 0x6c, 0x6f, 0x70, 0x70, 0x79	/* |M floppy| */
	.byte	0x20, 0x61, 0x6e, 0x64, 0x20, 0x72, 0x65, 0x69	/* | and rei| */
	.byte	0x6e, 0x73, 0x65, 0x72, 0x74, 0x20, 0x74, 0x68	/* |nsert th| */
	.byte	0x65, 0x20, 0x58, 0x46, 0x45, 0x52, 0x20, 0x66	/* |e XFER f| */
	.byte	0x6c, 0x6f, 0x70, 0x70, 0x79, 0x20, 0x69, 0x6e	/* |loppy in| */
	.byte	0x20, 0x64, 0x72, 0x69, 0x76, 0x65, 0x20, 0x30	/* | drive 0| */
	.byte	0x2e, 0x0a, 0x00, 0x52, 0x65, 0x6d, 0x6f, 0x76	/* |...Remov| */
	.byte	0x65, 0x20, 0x74, 0x68, 0x65, 0x20, 0x55, 0x53	/* |e the US| */
	.byte	0x45, 0x52, 0x20, 0x66, 0x6c, 0x6f, 0x70, 0x70	/* |ER flopp| */
	.byte	0x79, 0x20, 0x61, 0x6e, 0x64, 0x20, 0x72, 0x65	/* |y and re| */
	.byte	0x69, 0x6e, 0x73, 0x65, 0x72, 0x74, 0x20, 0x74	/* |insert t| */
	.byte	0x68, 0x65, 0x20, 0x58, 0x46, 0x45, 0x52, 0x20	/* |he XFER | */
	.byte	0x66, 0x6c, 0x6f, 0x70, 0x70, 0x79, 0x20, 0x69	/* |floppy i| */
	.byte	0x6e, 0x20, 0x64, 0x72, 0x69, 0x76, 0x65, 0x20	/* |n drive | */
	.byte	0x30, 0x2e, 0x0a, 0x00, 0x50, 0x72, 0x65, 0x73	/* |0...Pres| */
	.byte	0x73, 0x20, 0x60, 0x72, 0x65, 0x74, 0x75, 0x72	/* |s `retur| */
	.byte	0x6e	/* |n| */
	.word	D_2027	/* ptr: 0x2027 */
	.byte	0x77, 0x68, 0x65, 0x6e, 0x20, 0x72, 0x65, 0x61	/* |when rea| */
	.byte	0x64, 0x79, 0x2e, 0x07, 0x07, 0x00	/* |dy....| */
	.ascii	"/bin/pwd\0"
	.byte	0x70, 0x77, 0x64, 0x00	/* |pwd.| */
	.ascii	"/usr/bin/pwd\0"
	.byte	0x70, 0x77, 0x64, 0x00, 0x70, 0x77, 0x64, 0x20	/* |pwd.pwd | */
	.byte	0x66, 0x61, 0x69, 0x6c, 0x65, 0x64, 0x21, 0x0a	/* |failed!.| */
	.byte	0x00, 0x2f, 0x0a, 0x00	/* |./..| */
	.ascii	"HARD\0"
	.byte	0x54, 0x61, 0x72, 0x3a, 0x20, 0x6d, 0x65, 0x64	/* |Tar: med| */
	.byte	0x69, 0x75, 0x6d, 0x20, 0x62, 0x6c, 0x6f, 0x63	/* |ium bloc| */
	.byte	0x6b, 0x73, 0x69, 0x7a, 0x65, 0x20, 0x65, 0x72	/* |ksize er| */
	.byte	0x72, 0x6f, 0x72, 0x0a, 0x00, 0x54, 0x61, 0x72	/* |ror..Tar| */
	.byte	0x3a, 0x20, 0x43, 0x61, 0x6e, 0x6e, 0x6f, 0x74	/* |: Cannot| */
	.byte	0x20, 0x75, 0x70, 0x64, 0x61, 0x74, 0x65, 0x20	/* | update | */
	.byte	0x62, 0x6c, 0x6f, 0x63, 0x6b, 0x65, 0x64, 0x20	/* |blocked | */
	.byte	0x6d, 0x65, 0x64, 0x69, 0x61, 0x20, 0x28, 0x79	/* |media (y| */
	.byte	0x65, 0x74, 0x29, 0x0a, 0x00, 0x54, 0x61, 0x72	/* |et)..Tar| */
	.byte	0x3a, 0x20, 0x6d, 0x65, 0x64, 0x69, 0x75, 0x6d	/* |: medium| */
	.byte	0x20, 0x77, 0x72, 0x69, 0x74, 0x65, 0x20, 0x65	/* | write e| */
	.byte	0x72, 0x72, 0x6f, 0x72, 0x0a, 0x00, 0x54, 0x61	/* |rror..Ta| */
	.byte	0x72, 0x3a, 0x20, 0x6d, 0x65, 0x64, 0x69, 0x75	/* |r: mediu| */
	.byte	0x6d, 0x20, 0x77, 0x72, 0x69, 0x74, 0x65, 0x20	/* |m write | */
	.byte	0x65, 0x72, 0x72, 0x6f, 0x72, 0x0a, 0x00, 0x54	/* |error..T| */
	.byte	0x61, 0x72, 0x3a, 0x20, 0x6d, 0x65, 0x64, 0x69	/* |ar: medi| */
	.byte	0x75, 0x6d, 0x20, 0x72, 0x65, 0x61, 0x64, 0x20	/* |um read | */
	.byte	0x65, 0x72, 0x72, 0x6f, 0x72, 0x20, 0x61, 0x66	/* |error af| */
	.byte	0x74, 0x65, 0x72, 0x20, 0x73, 0x65, 0x65, 0x6b	/* |ter seek| */
	.byte	0x0a, 0x00, 0x0a, 0x25, 0x73, 0x20, 0x45, 0x52	/* |...%s ER| */
	.byte	0x52, 0x4f, 0x52, 0x20, 0x69, 0x6e, 0x20, 0x72	/* |ROR in r| */
	.byte	0x65, 0x61, 0x64, 0x69, 0x6e, 0x67, 0x20, 0x74	/* |eading t| */
	.byte	0x68, 0x69, 0x73, 0x20, 0x66, 0x6c, 0x6f, 0x70	/* |his flop| */
	.byte	0x70, 0x79, 0x0a, 0x00, 0x57, 0x6f, 0x75, 0x6c	/* |py..Woul| */
	.byte	0x64, 0x20, 0x79, 0x6f, 0x75, 0x20, 0x6c, 0x69	/* |d you li| */
	.byte	0x6b, 0x65, 0x20, 0x74, 0x6f, 0x0a, 0x0a, 0x00	/* |ke to...| */
	.byte	0x09, 0x41, 0x62, 0x6f, 0x72, 0x74, 0x20, 0x74	/* |.Abort t| */
	.byte	0x68, 0x65, 0x20, 0x74, 0x72, 0x61, 0x6e, 0x73	/* |he trans| */
	.byte	0x66, 0x65, 0x72, 0x2c, 0x0a, 0x00, 0x09, 0x52	/* |fer,...R| */
	.byte	0x65, 0x74, 0x72, 0x79, 0x20, 0x72, 0x65, 0x61	/* |etry rea| */
	.byte	0x64, 0x69, 0x6e, 0x67, 0x20, 0x74, 0x68, 0x65	/* |ding the| */
	.byte	0x20, 0x66, 0x6c, 0x6f, 0x70, 0x70, 0x79, 0x2c	/* | floppy,| */
	.byte	0x20, 0x6f, 0x72, 0x0a, 0x00, 0x09, 0x49, 0x67	/* | or...Ig| */
	.byte	0x6e, 0x6f, 0x72, 0x65, 0x20, 0x74, 0x68, 0x65	/* |nore the| */
	.byte	0x20, 0x65, 0x72, 0x72, 0x6f, 0x72, 0x0a, 0x00	/* | error..| */
	.byte	0x0a	/* |.| */
	.ascii	"Type `a', `r', or `i'? \0"
	.byte	0x41, 0x62, 0x6f, 0x72, 0x74, 0x0a, 0x00, 0x52	/* |Abort..R| */
	.byte	0x65, 0x74, 0x72, 0x79, 0x0a, 0x00	/* |etry..| */
	.ascii	"Check floppy and press `return' when ready ...\0"
	.byte	0x0a, 0x00, 0x49, 0x67, 0x6e, 0x6f, 0x72, 0x65	/* |..Ignore| */
	.byte	0x0a, 0x00, 0x07	/* |...| */
	.word	D_2e00	/* ptr: 0x2e00 */
	.byte	0x00, 0x74, 0x61, 0x72, 0x3a, 0x20, 0x6d, 0x6b	/* |.tar: mk| */
	.byte	0x64, 0x69, 0x72, 0x20, 0x2d, 0x20, 0x63, 0x61	/* |dir - ca| */
	.byte	0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x61, 0x63, 0x63	/* |nnot acc| */
	.byte	0x65, 0x73, 0x73, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |ess %s..| */
	.byte	0x74, 0x61, 0x72, 0x3a, 0x20, 0x6d, 0x6b, 0x64	/* |tar: mkd| */
	.byte	0x69, 0x72, 0x20, 0x2d, 0x20, 0x63, 0x61, 0x6e	/* |ir - can| */
	.byte	0x6e, 0x6f, 0x74, 0x20, 0x6d, 0x61, 0x6b, 0x65	/* |not make| */
	.byte	0x20, 0x64, 0x69, 0x72, 0x65, 0x63, 0x74, 0x6f	/* | directo| */
	.byte	0x72, 0x79, 0x20, 0x25, 0x73, 0x0a, 0x00, 0x2f	/* |ry %s../| */
	.byte	0x2e, 0x00, 0x74, 0x61, 0x72, 0x3a, 0x20, 0x6d	/* |..tar: m| */
	.byte	0x6b, 0x64, 0x69, 0x72, 0x20, 0x2d, 0x20, 0x63	/* |kdir - c| */
	.byte	0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6c, 0x69	/* |annot li| */
	.byte	0x6e, 0x6b, 0x20, 0x25, 0x73, 0x0a, 0x00, 0x2e	/* |nk %s...| */
	.byte	0x00, 0x74, 0x61, 0x72, 0x3a, 0x20, 0x6d, 0x6b	/* |.tar: mk| */
	.byte	0x64, 0x69, 0x72, 0x20, 0x2d, 0x20, 0x63, 0x61	/* |dir - ca| */
	.byte	0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6c, 0x69, 0x6e	/* |nnot lin| */
	.byte	0x6b, 0x20, 0x25, 0x73, 0x0a, 0x00, 0x00, 0x00	/* |k %s....| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x1f, 0x00, 0x1c, 0x00, 0x1f, 0x00, 0x1e, 0x00	/* |........| */
	.byte	0x1f, 0x00, 0x1e, 0x00, 0x1f, 0x00, 0x1f, 0x00	/* |........| */
	.byte	0x1e, 0x00, 0x1f, 0x00, 0x1e, 0x00, 0x1f, 0x00	/* |........| */
	.byte	0x05, 0x00, 0x4d, 0x01, 0x3a, 0x00, 0x2f, 0x01	/* |..M.:./.| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x44, 0x61, 0x79, 0x20, 0x4d, 0x6f	/* |..Day Mo| */
	.byte	0x6e, 0x20, 0x30, 0x30, 0x20, 0x30, 0x30, 0x3a	/* |n 00 00:| */
	.byte	0x30, 0x30, 0x3a, 0x30, 0x30, 0x20, 0x31, 0x39	/* |00:00 19| */
	.byte	0x30, 0x30, 0x0a, 0x00	/* |00..| */
	.ascii	"SunMonTueWedThuFriSat\0"
	.ascii	"JanFebMarAprMayJunJulAugSepOctNovDec\0"
	.byte	0x00, 0x2f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |./......| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00	/* |.....| */
	.word	D_3248	/* ptr: 0x3248 */
	.word	D_3254	/* ptr: 0x3254 */
	.word	D_3260	/* ptr: 0x3260 */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_327e	/* ptr: 0x327e */
	.word	D_320c	/* ptr: 0x320c */
	.word	D_320c	/* ptr: 0x320c */
	.word	D_322a	/* ptr: 0x322a */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x14, 0x40	/* |.......@| */
	.word	D_2000	/* ptr: 0x2000 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.byte	0x08, 0x08, 0x08, 0x08	/* |....| */
	.word	D_2008	/* ptr: 0x2008 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.word	D_2020	/* ptr: 0x2020 */
	.byte	0x20, 0x08	/* | .| */
	.word	D_1010	/* ptr: 0x1010 */
	.word	D_1010	/* ptr: 0x1010 */
	.word	D_1010	/* ptr: 0x1010 */
	.word	D_1010	/* ptr: 0x1010 */
	.word	D_1010	/* ptr: 0x1010 */
	.word	D_1010	/* ptr: 0x1010 */
	.word	D_1010	/* ptr: 0x1010 */
	.byte	0x10, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04	/* |........| */
	.byte	0x04, 0x04	/* |..| */
	.word	D_1004	/* ptr: 0x1004 */
	.word	D_1010	/* ptr: 0x1010 */
	.word	D_1010	/* ptr: 0x1010 */
	.word	D_1010	/* ptr: 0x1010 */
	.byte	0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x01, 0x01	/* |AAAAAA..| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01	/* |..| */
	.word	D_1010	/* ptr: 0x1010 */
	.word	D_1010	/* ptr: 0x1010 */
	.word	D_1010	/* ptr: 0x1010 */
	.byte	0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x02, 0x02	/* |BBBBBB..| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02	/* |..| */
	.word	D_1010	/* ptr: 0x1010 */
	.word	D_1010	/* ptr: 0x1010 */
	.byte	0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* | .......| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.ascii	"/bin/sh\0"
	.byte	0x73, 0x68	/* |sh| */
	.word	D_2d00	/* ptr: 0x2d00 */
	.byte	0x63, 0x00	/* |c.| */
	.ascii	"(null)\0"
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24	/* |.......$| */
	.byte	0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08	/* |@.......| */
	.byte	0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59	/* |@......Y| */
	.byte	0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24	/* |@......$| */
	.byte	0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |@.......| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24	/* |.......$| */
	.byte	0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0	/* |@.......| */
	.byte	0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24	/* |?......$| */
	.byte	0x40	/* |@| */
	.word	D_3a10	/* ptr: 0x3a10 */
	.byte	0x00, 0x00	/* |..| */
	.word	D_3a10	/* ptr: 0x3a10 */
	.byte	0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x02, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x06, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x56, 0x0d, 0x00, 0x00, 0x00, 0x00	/* |..V.....| */
	.byte	0x00, 0x00	/* |..| */
	.word	D_3c16	/* ptr: 0x3c16 */

	.bss

	.space	607
D_0fbf:	.space	2	/* bss word @ 0x0fbf */
	.space	67
D_1004:	.space	2	/* bss word @ 0x1004 */
	.space	10
D_1010:	.space	2	/* bss word @ 0x1010 */
	.space	1527
D_1609:	.space	2	/* bss word @ 0x1609 */
	.space	25
D_1624:	.space	2	/* bss word @ 0x1624 */
	.space	25
D_163f:	.space	2	/* bss word @ 0x163f */
	.space	2495
D_2000:	.space	2	/* bss word @ 0x2000 */
	.space	6
D_2008:	.space	2	/* bss word @ 0x2008 */
	.space	22
D_2020:	.space	2	/* bss word @ 0x2020 */
	.space	5
D_2027:	.space	2	/* bss word @ 0x2027 */
	.space	58
D_2063:	.space	2	/* bss word @ 0x2063 */
D_2064:	.space	2	/* bss word @ 0x2064 */
	.space	7
D_206d:	.space	2	/* bss word @ 0x206d */
	.space	4
D_2073:	.space	2	/* bss word @ 0x2073 */
D_2074:	.space	2	/* bss word @ 0x2074 */
	.space	1162
D_2500:	.space	2	/* bss word @ 0x2500 */
	.space	30
D_2520:	.space	2	/* bss word @ 0x2520 */
	.space	1
D_2523:	.space	2	/* bss word @ 0x2523 */
	.space	585
D_276e:	.space	2	/* bss word @ 0x276e */
	.space	1424
D_2d00:	.space	2	/* bss word @ 0x2d00 */
	.space	254
D_2e00:	.space	2	/* bss word @ 0x2e00 */
	.space	30
D_2e20:	.space	2	/* bss word @ 0x2e20 */
	.space	12
D_2e2e:	.space	2	/* bss word @ 0x2e2e */
	.space	988
D_320c:	.space	2	/* bss word @ 0x320c */
	.space	28
D_322a:	.space	2	/* bss word @ 0x322a */
	.space	28
D_3248:	.space	2	/* bss word @ 0x3248 */
	.space	10
D_3254:	.space	2	/* bss word @ 0x3254 */
	.space	10
D_3260:	.space	2	/* bss word @ 0x3260 */
	.space	28
D_327e:	.space	2	/* bss word @ 0x327e */
	.space	933
D_3625:	.space	2	/* bss word @ 0x3625 */
	.space	254
D_3725:	.space	2	/* bss word @ 0x3725 */
	.space	121
D_37a0:	.space	2	/* bss word @ 0x37a0 */
D_37a2:	.space	2	/* bss word @ 0x37a2 */
D_37a4:	.space	2	/* bss word @ 0x37a4 */
D_37a6:	.space	2	/* bss word @ 0x37a6 */
D_37a8:	.space	2	/* bss word @ 0x37a8 */
D_37aa:	.space	2	/* bss word @ 0x37aa */
D_37ac:	.space	2	/* bss word @ 0x37ac */
D_37ae:	.space	2	/* bss word @ 0x37ae */
D_37b0:	.space	2	/* bss word @ 0x37b0 */
D_37b2:	.space	2	/* bss word @ 0x37b2 */
D_37b4:	.space	2	/* bss word @ 0x37b4 */
D_37b6:	.space	2	/* bss word @ 0x37b6 */
D_37b8:	.space	2	/* bss word @ 0x37b8 */
D_37ba:	.space	2	/* bss word @ 0x37ba */
D_37bc:	.space	2	/* bss word @ 0x37bc */
D_37be:	.space	2	/* bss word @ 0x37be */
D_37c0:	.space	2	/* bss word @ 0x37c0 */
D_37c2:	.space	2	/* bss word @ 0x37c2 */
D_37c4:	.space	2	/* bss word @ 0x37c4 */
D_37c6:	.space	2	/* bss word @ 0x37c6 */
D_37c8:	.space	2	/* bss word @ 0x37c8 */
D_37ca:	.space	2	/* bss word @ 0x37ca */
D_37cc:	.space	2	/* bss word @ 0x37cc */
D_37ce:	.space	2	/* bss word @ 0x37ce */
D_37d0:	.space	2	/* bss word @ 0x37d0 */
D_37d2:	.space	2	/* bss word @ 0x37d2 */
D_37d4:	.space	2	/* bss word @ 0x37d4 */
D_37d6:	.space	2	/* bss word @ 0x37d6 */
D_37d8:	.space	2	/* bss word @ 0x37d8 */
D_37da:	.space	2	/* bss word @ 0x37da */
D_37dc:	.space	2	/* bss word @ 0x37dc */
D_37de:	.space	2	/* bss word @ 0x37de */
D_37e0:	.space	2	/* bss word @ 0x37e0 */
D_37e2:	.space	2	/* bss word @ 0x37e2 */
D_37e4:	.space	2	/* bss word @ 0x37e4 */
D_37e6:	.space	2	/* bss word @ 0x37e6 */
D_37e8:	.space	2	/* bss word @ 0x37e8 */
D_37ea:	.space	2	/* bss word @ 0x37ea */
D_37ec:	.space	2	/* bss word @ 0x37ec */
D_37ee:	.space	2	/* bss word @ 0x37ee */
D_37f0:	.space	2	/* bss word @ 0x37f0 */
D_37f2:	.space	2	/* bss word @ 0x37f2 */
D_37f4:	.space	2	/* bss word @ 0x37f4 */
D_37f6:	.space	2	/* bss word @ 0x37f6 */
D_37f8:	.space	2	/* bss word @ 0x37f8 */
D_37fa:	.space	2	/* bss word @ 0x37fa */
D_37fc:	.space	2	/* bss word @ 0x37fc */
D_37fe:	.space	2	/* bss word @ 0x37fe */
D_3800:	.space	2	/* bss word @ 0x3800 */
D_3802:	.space	2	/* bss word @ 0x3802 */
D_3804:	.space	2	/* bss word @ 0x3804 */
D_3806:	.space	2	/* bss word @ 0x3806 */
D_3808:	.space	2	/* bss word @ 0x3808 */
	.space	6
D_3810:	.space	2	/* bss word @ 0x3810 */
	.space	122
D_388c:	.space	2	/* bss word @ 0x388c */
	.space	386
D_3a10:	.space	2	/* bss word @ 0x3a10 */
	.space	510
D_3c10:	.space	2	/* bss word @ 0x3c10 */
D_3c12:	.space	2	/* bss word @ 0x3c12 */
D_3c14:	.space	2	/* bss word @ 0x3c14 */
D_3c16:	/* endcore: initial break */

/* end of file */
