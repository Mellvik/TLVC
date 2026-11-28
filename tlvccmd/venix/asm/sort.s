/* sort.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 sort.s -o sort.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* Wrapping near-call/jmp targets (16-bit address space) */
	.set L_94e4, 0x94e4	/* wrapping branch target */
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: sort  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x000028b8  (10424 bytes)
//   a_data:   0x000006bc  (1724 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xa,%sp
	call   L_0f70
	mov    0x6(%bp),%ax
	mov    %ax,D_09c6
L_0047:
	decw   0x4(%bp)
	jg   L_004f
	jmp   L_013e
L_004f:
	addw   $0x2,0x6(%bp)
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	cmpb   $0x2d,(%bx)
	je   L_0060
	jmp   L_00e5
L_0060:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x8(%bp)
L_0068:
	incw   -0x8(%bp)
	mov    -0x8(%bp),%bx
	mov    (%bx),%al
	cbtw
	jmp   L_00d5
L_0073:
	mov    -0x8(%bp),%bx
	cmpb   $0x2d,-0x1(%bx)
	jne   L_0047
	mov    D_09c4,%ax
	shl    %ax
	add    D_09c6,%ax
	mov    %ax,%bx
	movw   $D_0432,(%bx)
	incw   D_09c4
L_008f:
	jmp   L_0047
L_0091:
	decw   0x4(%bp)
	jle   L_0068
	addw   $0x2,0x6(%bp)
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,D_06c0
	jmp   L_0068
L_00a4:
	decw   0x4(%bp)
	jle   L_0068
	addw   $0x2,0x6(%bp)
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,D_0004
	jmp   L_0068
L_00b7:
	cmpw   $0x0,D_09bc
	jle   L_00c3
	mov    $0x1,%ax
	jmp   L_00c5
L_00c3:
	xor    %ax,%ax
L_00c5:
	push   %ax
	mov    0x6(%bp),%bx
	incw   (%bx)
	pushw  (%bx)
	call   L_0fa5
	add    $0x4,%sp
L_00d3:
	jmp   L_008f
L_00d5:
	or     %ax,%ax
	je   L_0073
	cmp    $0x54,%ax
	je   L_00a4
	cmp    $0x6f,%ax
	je   L_0091
	jmp   L_00b7
L_00e5:
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	cmpb   $0x2b,(%bx)
	jne   L_0125
	incw   D_09bc
	cmpw   $0xa,D_09bc
	jl   L_0110
	mov    $D_0442,%ax
	push   %ax
	mov    $D_0434,%ax
	push   %ax
	call   L_0a88
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0110:
	call   L_0f70
	xor    %ax,%ax
	push   %ax
	mov    0x6(%bp),%bx
	incw   (%bx)
	pushw  (%bx)
	call   L_0fa5
	add    $0x4,%sp
L_0123:
	jmp   L_00d3
L_0125:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	mov    D_09c4,%dx
	shl    %dx
	add    D_09c6,%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	incw   D_09c4
	jmp   L_0123
L_013e:
	movw   $D_06cc,-0xc(%bp)
	mov    $0x1,%di
L_0146:
	cmp    D_09bc,%di
	jle   L_014f
	jmp   L_01e6
L_014f:
	mov    %di,%ax
	mov    $0x14,%dx
	imul   %dx
	add    $D_06cc,%ax
	mov    %ax,-0xa(%bp)
	mov    %ax,%bx
	mov    (%bx),%ax
	cmp    D_040e,%ax
	jne   L_01a0
	mov    -0xa(%bp),%bx
	mov    0x2(%bx),%ax
	cmp    D_0410,%ax
	jne   L_01a0
	mov    -0xa(%bp),%bx
	mov    0x4(%bx),%ax
	cmp    D_0412,%ax
	jne   L_01a0
	mov    -0xa(%bp),%bx
	mov    0x6(%bx),%ax
	cmp    D_0414,%ax
	jne   L_01e2
	mov    -0xa(%bp),%bx
	mov    0x8(%bx),%ax
	cmp    D_0416,%ax
	jne   L_01e2
	mov    -0xa(%bp),%bx
	mov    0xa(%bx),%ax
	cmp    D_0418,%ax
L_01a0:
	jne   L_01e2
	mov    -0xc(%bp),%bx
	mov    (%bx),%ax
	mov    -0xa(%bp),%bx
	mov    %ax,(%bx)
	mov    -0xc(%bp),%bx
	mov    0x2(%bx),%ax
	mov    -0xa(%bp),%bx
	mov    %ax,0x2(%bx)
	mov    -0xc(%bp),%bx
	mov    0x4(%bx),%ax
	mov    -0xa(%bp),%bx
	mov    %ax,0x4(%bx)
	mov    -0xc(%bp),%bx
	mov    0x6(%bx),%ax
	mov    -0xa(%bp),%bx
	mov    %ax,0x6(%bx)
	mov    -0xc(%bp),%bx
	mov    0x8(%bx),%ax
	mov    -0xa(%bp),%bx
	mov    %ax,0xa(%bx)
	mov    -0xa(%bp),%bx
	mov    %ax,0x8(%bx)
L_01e2:
	inc    %di
	jmp   L_0146
L_01e6:
	cmpw   $0x0,D_09c4
	jne   L_0200
	mov    D_09c4,%ax
	shl    %ax
	add    D_09c6,%ax
	mov    %ax,%bx
	movw   $D_0443,(%bx)
	incw   D_09c4
L_0200:
	cmpw   $0x0,D_06c8
	je   L_0224
	cmpw   $0x1,D_09c4
	jle   L_0224
	mov    $D_045b,%ax
	push   %ax
	mov    $D_0445,%ax
	push   %ax
	call   L_0a88
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0224:
	call   L_09ff
	movw   $0xaf2a,-0x6(%bp)
	xor    %ax,%ax
	push   %ax
	call   L_2766
	pop    %cx
	mov    %ax,D_06c6
L_0236:
	pushw  -0x6(%bp)
	call   L_274a
	pop    %cx
	cmp    $0xffff,%ax
	jne   L_0249
	subw   $0x200,-0x6(%bp)
	jmp   L_0236
L_0249:
	subw   $0x200,-0x6(%bp)
	pushw  -0x6(%bp)
	call   L_274a
	pop    %cx
	mov    -0x6(%bp),%ax
	sub    D_06c6,%ax
	mov    %ax,%di
	lea    -0x200(%di),%ax
	mov    %ax,D_09cc
	xor    %dx,%dx
	mov    $0xa,%cx
	div    %cx
	mov    %ax,D_09cc
	mov    $0x3,%cx
	shl    %cl,%ax
	mov    %ax,D_06c2
	mov    D_09cc,%ax
	shl    %ax
	add    D_06c6,%ax
	mov    %ax,D_06c4
	mov    $0xffff,%di
	/* dual-reloc split: movw $D_0004,D_09c2 */
	mov	$D_0004,%ax
	mov	%ax,D_09c2
L_028c:
	mov    D_09c2,%bx
	cmpw   $0x0,(%bx)
	je   L_02e2
	call   L_1338
	push   %ax
	mov    D_09c2,%bx
	pushw  (%bx)
	mov    $D_045c,%ax
	push   %ax
	/* dual-reloc split: movw $D_079c,D_09c0 */
	mov	$D_079c,%ax
	mov	%ax,D_09c0
	mov    $D_079c,%ax
	push   %ax
	call   L_1648
	add    $0x8,%sp
L_02b3:
	mov    D_09c0,%bx
	cmpb   $0x0,(%bx)
	je   L_02c2
	incw   D_09c0
	jmp   L_02b3
L_02c2:
	subw   $0x2,D_09c0
	mov    $0x180,%ax
	push   %ax
	pushw  D_000a
	call   L_1446
	add    $0x4,%sp
	mov    %ax,%di
	or     %ax,%ax
	jge   L_02e2
	addw   $0x2,D_09c2
	jmp   L_028c
L_02e2:
	or     %di,%di
	jge   L_02fc
	mov    $D_047b,%ax
	push   %ax
	mov    $D_0469,%ax
	push   %ax
	call   L_0a88
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_02fc:
	push   %di
	call   L_278c
	pop    %cx
	mov    $0xac7,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_150a
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_150a
	add    $0x4,%sp
	cmp    $0x1,%ax
	je   L_0330
	mov    $0xac7,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_150a
	add    $0x4,%sp
L_0330:
	mov    $0xac7,%ax
	push   %ax
	mov    $0xd,%ax
	push   %ax
	call   L_150a
	add    $0x4,%sp
	mov    $0xac7,%ax
	push   %ax
	mov    $0xf,%ax
	push   %ax
	call   L_150a
	add    $0x4,%sp
	mov    D_09c4,%ax
	mov    %ax,D_09c8
	cmpw   $0x0,D_0794
	jne   L_036b
	cmpw   $0x0,D_06c8
	jne   L_036b
	call   L_03db
	mov    $D_0612,%ax
	push   %ax
	call   L_24c1
	pop    %cx
L_036b:
	mov    D_0794,%ax
	or     D_06c8,%ax
	je   L_0378
	xor    %ax,%ax
	jmp   L_037b
L_0378:
	mov    D_09c4,%ax
L_037b:
	mov    %ax,%di
L_037d:
	lea    0x7(%di),%ax
	cmp    D_09c8,%ax
	jl   L_0393
	cmpw   $0x0,D_09ce
	je   L_03b7
	cmp    D_09c4,%di
	jge   L_03b7
L_0393:
	lea    0x7(%di),%ax
	mov    %ax,-0xe(%bp)
	cmp    D_09c8,%ax
	jl   L_03a5
	mov    D_09c8,%ax
	mov    %ax,-0xe(%bp)
L_03a5:
	call   L_090e
	pushw  -0xe(%bp)
	push   %di
	call   L_0591
	add    $0x4,%sp
	mov    -0xe(%bp),%di
	jmp   L_037d
L_03b7:
	cmp    D_09c8,%di
	je   L_03cb
	call   L_09c1
	pushw  D_09c8
	push   %di
	call   L_0591
	add    $0x4,%sp
L_03cb:
	movw   $0x0,D_0422
	call   L_0ac7
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_03db:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0xa(%bp)
	movw   $0xffff,-0x6(%bp)
L_03f2:
	mov    D_06c4,%di
	mov    D_06c6,%si
L_03fa:
	mov    D_09cc,%ax
	shl    %ax
	add    D_06c6,%ax
	cmp    %ax,%si
	jae   L_0410
	mov    D_06c4,%ax
	add    D_06c2,%ax
	cmp    %ax,%di
L_0410:
	jb   L_0415
	jmp   L_94e4
L_0415:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    %di,(%bx)
L_041b:
	cmpw   $0xa,-0x6(%bp)
	jne   L_0424
	jmp   L_04d3
L_0424:
	cmpw   $0xffff,-0x6(%bp)
	je   L_045d
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    D_09ca,%bx
	decw   0x2(%bx)
	jl   L_0450
	mov    D_09ca,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0458
L_0450:
	pushw  D_09ca
	call   L_145c
	pop    %cx
L_0458:
	mov    %ax,-0x6(%bp)
L_045b:
	jmp   L_041b
L_045d:
	cmpw   $0x0,D_09ca
	je   L_046c
	pushw  D_09ca
	call   L_24c1
	pop    %cx
L_046c:
	mov    -0xa(%bp),%ax
	cmp    D_09c4,%ax
	jge   L_04d3
	mov    -0xa(%bp),%ax
	incw   -0xa(%bp)
	push   %ax
	call   L_0945
	pop    %cx
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_048f
	/* dual-reloc split: movw $D_0612,D_09ca */
	mov	$D_0612,%ax
	mov	%ax,D_09ca
	jmp   L_04aa
L_048f:
	mov    $D_047c,%ax
	push   %ax
	pushw  -0xc(%bp)
	call   L_1348
	add    $0x4,%sp
	mov    %ax,D_09ca
	or     %ax,%ax
	jne   L_04aa
	pushw  -0xc(%bp)
	call   L_0a6f
	pop    %cx
L_04aa:
	mov    D_09ca,%bx
	decw   0x2(%bx)
	jl   L_04c6
	mov    D_09ca,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_04ce
L_04c6:
	pushw  D_09ca
	call   L_145c
	pop    %cx
L_04ce:
	mov    %ax,-0x6(%bp)
	jmp   L_045b
L_04d3:
	mov    %di,%bx
	inc    %di
	movb   $0xa,(%bx)
	cmpw   $0xffff,-0x6(%bp)
	jne   L_04e6
	incw   -0x8(%bp)
	dec    %si
	dec    %si
	jmp   L_0510
L_04e6:
	mov    D_09ca,%bx
	decw   0x2(%bx)
	jl   L_0502
	mov    D_09ca,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_050a
L_0502:
	pushw  D_09ca
	call   L_145c
	pop    %cx
L_050a:
	mov    %ax,-0x6(%bp)
	jmp   L_03fa
L_0510:
	push   %si
	pushw  D_06c6
	call   L_114c
	add    $0x4,%sp
	cmpw   $0x0,-0x8(%bp)
	je   L_052a
	mov    D_09c8,%ax
	cmp    D_09c4,%ax
	je   L_052f
L_052a:
	call   L_090e
	jmp   L_0532
L_052f:
	call   L_09c1
L_0532:
	cmp    D_06c6,%si
	jbe   L_0579
	dec    %si
	dec    %si
	mov    (%si),%di
	cmpb   $0x0,(%di)
	je   L_0532
L_0541:
	mov    D_06be,%bx
	decw   0x2(%bx)
	jl   L_0561
	mov    (%di),%al
	cbtw
	mov    D_06be,%bx
	incw   (%bx)
	mov    (%bx),%dx
	dec    %dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_056f
L_0561:
	pushw  D_06be
	mov    (%di),%al
	cbtw
	push   %ax
	call   L_2364
	add    $0x4,%sp
L_056f:
	mov    %di,%bx
	inc    %di
	cmpb   $0xa,(%bx)
	jne   L_0541
	jmp   L_0532
L_0579:
	pushw  D_06be
	call   L_24c1
	pop    %cx
	cmpw   $0x0,-0x8(%bp)
	jne   L_058a
	jmp   L_03f2
L_058a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0591:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x12,%sp
	mov    D_06c6,%ax
	mov    %ax,-0x6(%bp)
	movw   $0x0,-0x10(%bp)
	mov    0x4(%bp),%ax
	mov    %ax,-0x8(%bp)
L_05aa:
	mov    -0x8(%bp),%ax
	cmp    0x6(%bp),%ax
	jge   L_060e
	pushw  -0x8(%bp)
	call   L_0945
	pop    %cx
	mov    %ax,-0xe(%bp)
	or     %ax,%ax
	jne   L_05cb
	mov    -0x6(%bp),%bx
	movw   $D_0612,0x200(%bx)
	jmp   L_05ea
L_05cb:
	mov    $D_047e,%ax
	push   %ax
	pushw  -0xe(%bp)
	call   L_1348
	add    $0x4,%sp
	mov    -0x6(%bp),%bx
	mov    %ax,0x200(%bx)
	or     %ax,%ax
	jne   L_05ea
	pushw  -0xe(%bp)
	call   L_0a6f
	pop    %cx
L_05ea:
	mov    -0x10(%bp),%bx
	shl    %bx
	mov    -0x6(%bp),%ax
	mov    %ax,D_07bc(%bx)
	pushw  -0x6(%bp)
	call   L_0881
	pop    %cx
	or     %ax,%ax
	jne   L_0604
	incw   -0x10(%bp)
L_0604:
	addw   $0x202,-0x6(%bp)
	incw   -0x8(%bp)
	jmp   L_05aa
L_060e:
	mov    -0x10(%bp),%ax
	mov    %ax,-0x8(%bp)
	shl    %ax
	add    $D_07bc,%ax
	push   %ax
	mov    $D_07bc,%ax
	push   %ax
	call   L_114c
	add    $0x4,%sp
	movw   $0x0,-0x14(%bp)
L_0629:
	mov    -0x8(%bp),%ax
	decw   -0x8(%bp)
	or     %ax,%ax
	je   L_0684
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    D_07bc(%bx),%di
	cmpb   $0x0,(%di)
	jne   L_0629
	movw   $0x1,-0x14(%bp)
	mov    -0x8(%bp),%bx
	shl    %bx
	pushw  D_07bc(%bx)
	call   L_0881
	pop    %cx
	or     %ax,%ax
	je   L_0629
	mov    -0x8(%bp),%ax
	mov    %ax,-0x12(%bp)
L_065d:
	incw   -0x12(%bp)
	mov    -0x12(%bp),%ax
	cmp    -0x10(%bp),%ax
	jge   L_067f
	mov    -0x12(%bp),%bx
	shl    %bx
	mov    D_07bc(%bx),%ax
	mov    -0x12(%bp),%dx
	dec    %dx
	mov    %dx,%bx
	shl    %bx
	mov    %ax,D_07bc(%bx)
	jmp   L_065d
L_067f:
	decw   -0x10(%bp)
	jmp   L_0629
L_0684:
	cmpw   $0x0,-0x14(%bp)
	jne   L_060e
	mov    D_0794,%ax
	and    D_09be,%ax
	or     D_06c8,%ax
	mov    %ax,-0x16(%bp)
	mov    -0x10(%bp),%ax
	mov    %ax,-0x8(%bp)
L_069e:
	cmpw   $0x0,-0x8(%bp)
	jg   L_06a7
	jmp   L_0834
L_06a7:
	mov    -0x8(%bp),%ax
	dec    %ax
	mov    %ax,%bx
	shl    %bx
	mov    D_07bc(%bx),%di
	cmpw   $0x0,D_06c8
	jne   L_0721
	cmpw   $0x0,D_09be
	je   L_06eb
	cmpw   $0x0,-0x16(%bp)
	jne   L_06eb
	mov    -0x8(%bp),%ax
	dec    %ax
	dec    %ax
	mov    %ax,%bx
	shl    %bx
	pushw  D_07bc(%bx)
	mov    -0x8(%bp),%ax
	dec    %ax
	mov    %ax,%bx
	shl    %bx
	pushw  D_07bc(%bx)
	/* TODO: indirect call - verify */
	call   *0x200c
	add    $0x4,%sp
	or     %ax,%ax
	je   L_0721
L_06eb:
	mov    D_06be,%bx
	decw   0x2(%bx)
	jl   L_070b
	mov    (%di),%al
	cbtw
	mov    D_06be,%bx
	incw   (%bx)
	mov    (%bx),%dx
	dec    %dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_0719
L_070b:
	pushw  D_06be
	mov    (%di),%al
	cbtw
	push   %ax
	call   L_2364
	add    $0x4,%sp
L_0719:
	mov    %di,%bx
	inc    %di
	cmpb   $0xa,(%bx)
	jne   L_06eb
L_0721:
	cmpw   $0x0,-0x16(%bp)
	je   L_074b
	mov    -0x8(%bp),%ax
	dec    %ax
	mov    %ax,%bx
	shl    %bx
	mov    D_07bc(%bx),%di
	mov    -0x6(%bp),%si
L_0736:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	cmp    $0xa,%ax
	jne   L_0736
L_074b:
	mov    -0x8(%bp),%ax
	dec    %ax
	mov    %ax,%bx
	shl    %bx
	pushw  D_07bc(%bx)
	call   L_0881
	pop    %cx
	or     %ax,%ax
	je   L_0777
	decw   -0x8(%bp)
	cmpw   $0x0,-0x8(%bp)
L_0766:
	jne   L_076b
	jmp   L_069e
L_076b:
	cmpw   $0x1,-0x8(%bp)
	jne   L_0777
	mov    D_09be,%ax
	mov    %ax,-0x16(%bp)
L_0777:
	mov    -0x8(%bp),%ax
	shl    %ax
	add    $D_07bc,%ax
	mov    %ax,-0xa(%bp)
L_0782:
	subw   $0x2,-0xa(%bp)
	cmpw   $D_07bc,-0xa(%bp)
	jbe   L_07c1
	mov    -0xa(%bp),%bx
	pushw  -0x2(%bx)
	mov    -0xa(%bp),%bx
	pushw  (%bx)
	/* TODO: indirect call - verify */
	call   *0x200c
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_07c1
	mov    -0xa(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0xc(%bp)
	mov    -0xa(%bp),%bx
	mov    -0x2(%bx),%ax
	mov    -0xa(%bp),%bx
	mov    %ax,(%bx)
	mov    -0xa(%bp),%bx
	mov    -0xc(%bp),%ax
	mov    %ax,-0x2(%bx)
	jmp   L_0782
L_07c1:
	cmpw   $0x0,-0x16(%bp)
L_07c5:
	je   L_0766
	pushw  -0x6(%bp)
	mov    -0x8(%bp),%ax
	dec    %ax
	mov    %ax,%bx
	shl    %bx
	pushw  D_07bc(%bx)
	/* TODO: indirect call - verify */
	call   *0x200c
	add    $0x4,%sp
	mov    %ax,-0x10(%bp)
	cmpw   $0x0,D_06c8
	je   L_082b
	cmpw   $0x0,-0x10(%bp)
	jle   L_0806
	mov    -0x8(%bp),%ax
	dec    %ax
	mov    %ax,%bx
	shl    %bx
	pushw  D_07bc(%bx)
	mov    $D_0480,%ax
	push   %ax
	call   L_08e8
	add    $0x4,%sp
L_0803:
	jmp   L_069e
L_0806:
	cmpw   $0x0,D_09be
	je   L_07c5
	cmpw   $0x0,-0x10(%bp)
	jne   L_0803
	mov    -0x8(%bp),%ax
	dec    %ax
	mov    %ax,%bx
	shl    %bx
	pushw  D_07bc(%bx)
	mov    $D_048a,%ax
	push   %ax
	call   L_08e8
	add    $0x4,%sp
	jmp   L_0803
L_082b:
	cmpw   $0x0,-0x10(%bp)
	jne   L_0803
	jmp   L_074b
L_0834:
	mov    D_06c6,%ax
	mov    %ax,-0x6(%bp)
	mov    0x4(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0840:
	mov    -0x8(%bp),%ax
	cmp    0x6(%bp),%ax
	jge   L_0872
	mov    -0x6(%bp),%bx
	pushw  0x200(%bx)
	call   L_24c1
	pop    %cx
	addw   $0x202,-0x6(%bp)
	mov    -0x8(%bp),%ax
	cmp    D_09c4,%ax
	jl   L_086d
	pushw  -0x8(%bp)
	call   L_0945
	pop    %cx
	push   %ax
	call   L_285a
	pop    %cx
L_086d:
	incw   -0x8(%bp)
	jmp   L_0840
L_0872:
	pushw  D_06be
	call   L_24c1
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0881:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%bx
	mov    0x200(%bx),%ax
	mov    %ax,-0x6(%bp)
	mov    0x4(%bp),%di
	lea    0x200(%di),%ax
	mov    %ax,%si
L_089c:
	mov    -0x6(%bp),%bx
	decw   0x2(%bx)
	jl   L_08b6
	mov    -0x6(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_08bd
L_08b6:
	pushw  -0x6(%bp)
	call   L_145c
	pop    %cx
L_08bd:
	mov    %ax,-0x8(%bp)
	cmp    $0xffff,%ax
	jne   L_08ca
	mov    $0x1,%ax
	jmp   L_08e1
L_08ca:
	cmp    %si,%di
	jb   L_08cf
	dec    %di
L_08cf:
	mov    %di,%bx
	inc    %di
	mov    -0x8(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	cmpw   $0xa,-0x8(%bp)
	jne   L_089c
	xor    %ax,%ax
L_08e1:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_08e8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x6(%bp),%di
L_08f0:
	cmpb   $0xa,(%di)
	je   L_08f8
	inc    %di
	jmp   L_08f0
L_08f8:
	movb   $0x0,(%di)
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_0a88
	add    $0x4,%sp
	call   L_0ac7
	pop    %di
	pop    %si
	pop    %bp
	ret


L_090e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  D_09c8
	call   L_0945
	pop    %cx
	mov    %ax,%di
	mov    $D_0495,%ax
	push   %ax
	push   %di
	call   L_1348
	add    $0x4,%sp
	mov    %ax,D_06be
	or     %ax,%ax
	jne   L_093d
	push   %di
	mov    $D_0497,%ax
	push   %ax
	call   L_0a88
	add    $0x4,%sp
	call   L_0ac7
L_093d:
	incw   D_09c8
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0945:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	cmp    D_09c4,%ax
	jge   L_098b
	mov    0x4(%bp),%ax
	shl    %ax
	add    D_09c6,%ax
	mov    %ax,%bx
	mov    (%bx),%bx
	cmpb   $0x2d,(%bx)
	jne   L_097c
	mov    0x4(%bp),%ax
	shl    %ax
	add    D_09c6,%ax
	mov    %ax,%bx
	mov    (%bx),%bx
	cmpb   $0x0,0x1(%bx)
	jne   L_097c
	xor    %ax,%ax
	jmp   L_09bd
L_097c:
	mov    0x4(%bp),%ax
	shl    %ax
	add    D_09c6,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	jmp   L_09bd
L_098b:
	mov    D_09c4,%ax
	sub    %ax,0x4(%bp)
	mov    0x4(%bp),%ax
	cwtd
	mov    $0x1a,%cx
	idiv   %cx
	add    $0x61,%ax
	mov    D_09c0,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0x4(%bp),%ax
	cwtd
	mov    $0x1a,%cx
	idiv   %cx
	add    $0x61,%dx
	mov    D_09c0,%bx
	mov    %dx,%cx
	mov    %cl,0x1(%bx)
	mov    D_000a,%ax
L_09bd:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_09c1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_06c0
	je   L_09f5
	mov    $D_04a5,%ax
	push   %ax
	pushw  D_06c0
	call   L_1348
	add    $0x4,%sp
	mov    %ax,D_06be
	or     %ax,%ax
	jne   L_09fb
	pushw  D_06c0
	mov    $D_04a7,%ax
	push   %ax
	call   L_0a88
	add    $0x4,%sp
	call   L_0ac7
	jmp   L_09fb
L_09f5:
	/* dual-reloc split: movw $D_061a,D_06be */
	mov	$D_061a,%ax
	mov	%ax,D_06be
L_09fb:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_09ff:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x3c,%sp
	cmpw   $0x0,D_0794
	je   L_0a26
	cmpw   $0x0,D_06c0
	je   L_0a26
	lea    -0x22(%bp),%ax
	push   %ax
	pushw  D_06c0
	call   L_27f8
	add    $0x4,%sp
	cmp    $0xffff,%ax
L_0a26:
	je   L_0a68
	mov    D_09c4,%ax
	sub    $0x7,%ax
	mov    %ax,%di
L_0a30:
	cmp    D_09c4,%di
	jge   L_0a68
	lea    -0x40(%bp),%ax
	push   %ax
	mov    %di,%ax
	shl    %ax
	add    D_09c6,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_27f8
	add    $0x4,%sp
	cmp    $0xffff,%ax
	je   L_0a65
	mov    -0x22(%bp),%ax
	cmp    -0x40(%bp),%ax
	jne   L_0a65
	mov    -0x20(%bp),%ax
	cmp    -0x3e(%bp),%ax
	jne   L_0a65
	incw   D_09ce
L_0a65:
	inc    %di
	jmp   L_0a30
L_0a68:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a6f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	mov    $D_04b5,%ax
	push   %ax
	call   L_0a88
	add    $0x4,%sp
	call   L_0ac7
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a88:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0622,%ax
	push   %ax
	mov    $D_04c1,%ax
	push   %ax
	call   L_12ec
	add    $0x4,%sp
	mov    $D_0622,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_12ec
	add    $0x4,%sp
	mov    $D_0622,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_12ec
	add    $0x4,%sp
	mov    $D_0622,%ax
	push   %ax
	mov    $D_04c8,%ax
	push   %ax
	call   L_12ec
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ac7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_150a
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_150a
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0xf,%ax
	push   %ax
	call   L_150a
	add    $0x4,%sp
	mov    D_09c8,%ax
	cmp    D_09c4,%ax
	jne   L_0b03
	incw   D_09c8
L_0b03:
	mov    D_09c4,%di
L_0b07:
	cmp    D_09c8,%di
	jg   L_0b1a
	push   %di
	call   L_0945
	pop    %cx
	push   %ax
	call   L_285a
	pop    %cx
	inc    %di
	jmp   L_0b07
L_0b1a:
	pushw  D_0422
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x1c,%sp
	cmpw   $0x0,D_09bc
	jle   L_0b3a
	mov    $0x1,%ax
	jmp   L_0b3c
L_0b3a:
	xor    %ax,%ax
L_0b3c:
	mov    %ax,-0xe(%bp)
L_0b3f:
	mov    -0xe(%bp),%ax
	cmp    D_09bc,%ax
	jle   L_0b4b
	jmp   L_0e1f
L_0b4b:
	mov    -0xe(%bp),%ax
	mov    $0x14,%dx
	imul   %dx
	add    $D_06cc,%ax
	mov    %ax,-0x20(%bp)
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	cmpw   $0x0,-0xe(%bp)
	je   L_0ba7
	mov    $0x1,%ax
	push   %ax
	pushw  -0x20(%bp)
	push   %di
	call   L_0e9d
	add    $0x6,%sp
	mov    %ax,-0x10(%bp)
	xor    %ax,%ax
	push   %ax
	pushw  -0x20(%bp)
	push   %di
	call   L_0e9d
	add    $0x6,%sp
	mov    %ax,%di
	mov    $0x1,%ax
	push   %ax
	pushw  -0x20(%bp)
	push   %si
	call   L_0e9d
	add    $0x6,%sp
	mov    %ax,-0x12(%bp)
	xor    %ax,%ax
	push   %ax
	pushw  -0x20(%bp)
	push   %si
	call   L_0e9d
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_0bb7
L_0ba7:
	push   %di
	call   L_0f59
	pop    %cx
	mov    %ax,-0x10(%bp)
	push   %si
	call   L_0f59
	pop    %cx
	mov    %ax,-0x12(%bp)
L_0bb7:
	mov    -0x20(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_0bc3
	jmp   L_0d7c
L_0bc3:
	mov    (%di),%al
	cbtw
	push   %ax
	call   L_1130
	pop    %cx
	or     %ax,%ax
	je   L_0bd2
	inc    %di
	jmp   L_0bc3
L_0bd2:
	mov    (%si),%al
	cbtw
	push   %ax
	call   L_1130
	pop    %cx
	or     %ax,%ax
	je   L_0be1
	inc    %si
	jmp   L_0bd2
L_0be1:
	mov    -0x20(%bp),%bx
	mov    0x6(%bx),%ax
	mov    %ax,-0x16(%bp)
	mov    %ax,-0x14(%bp)
	cmpb   $0x2d,(%di)
	jne   L_0bfb
	inc    %di
	mov    -0x14(%bp),%ax
	neg    %ax
	mov    %ax,-0x14(%bp)
L_0bfb:
	cmpb   $0x2d,(%si)
	jne   L_0c09
	inc    %si
	mov    -0x16(%bp),%ax
	neg    %ax
	mov    %ax,-0x16(%bp)
L_0c09:
	mov    %di,-0x18(%bp)
L_0c0c:
	mov    -0x18(%bp),%ax
	cmp    -0x10(%bp),%ax
	jae   L_0c2b
	mov    -0x18(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	mov    D_04cd(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0c2b
	incw   -0x18(%bp)
	jmp   L_0c0c
L_0c2b:
	mov    %si,-0x1a(%bp)
L_0c2e:
	mov    -0x1a(%bp),%ax
	cmp    -0x12(%bp),%ax
	jae   L_0c4d
	mov    -0x1a(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	mov    D_04cd(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0c4d
	incw   -0x1a(%bp)
	jmp   L_0c2e
L_0c4d:
	mov    -0x18(%bp),%ax
	mov    %ax,-0x1c(%bp)
	mov    -0x1a(%bp),%ax
	mov    %ax,-0x1e(%bp)
	movw   $0x0,-0xa(%bp)
	mov    -0x14(%bp),%ax
	cmp    -0x16(%bp),%ax
	jne   L_0c99
L_0c66:
	cmp    %di,-0x18(%bp)
	jbe   L_0c99
	cmp    %si,-0x1a(%bp)
	jbe   L_0c99
	decw   -0x1a(%bp)
	mov    -0x1a(%bp),%bx
	mov    (%bx),%al
	cbtw
	decw   -0x18(%bp)
	mov    -0x18(%bp),%bx
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %dx,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_0c66
	mov    -0xc(%bp),%ax
	mov    %ax,-0xa(%bp)
	jmp   L_0c66
L_0c99:
	cmp    %di,-0x18(%bp)
	jbe   L_0cb0
	decw   -0x18(%bp)
	mov    -0x18(%bp),%bx
	cmpb   $0x30,(%bx)
	je   L_0c99
	mov    -0x14(%bp),%ax
	neg    %ax
	jmp   L_0cd1
L_0cb0:
	cmp    %si,-0x1a(%bp)
	jbe   L_0cc5
	decw   -0x1a(%bp)
	mov    -0x1a(%bp),%bx
	cmpb   $0x30,(%bx)
	je   L_0cb0
	mov    -0x16(%bp),%ax
	jmp   L_0cd1
L_0cc5:
	cmpw   $0x0,-0xa(%bp)
	je   L_0cd3
	mov    -0xa(%bp),%ax
	imulw  -0x14(%bp)
L_0cd1:
	jmp   L_0d36
L_0cd3:
	mov    -0x1c(%bp),%di
	cmpb   $0x2e,(%di)
	jne   L_0cdc
	inc    %di
L_0cdc:
	mov    -0x1e(%bp),%si
	cmpb   $0x2e,(%si)
	jne   L_0ce5
	inc    %si
L_0ce5:
	mov    -0x14(%bp),%ax
	cmp    -0x16(%bp),%ax
	jne   L_0d38
L_0ced:
	cmp    -0x10(%bp),%di
	jae   L_0d04
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	mov    D_04cd(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0d38
	cmp    -0x12(%bp),%si
L_0d04:
	jae   L_0d38
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	mov    D_04cd(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0d38
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %dx,%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	je   L_0ced
	mov    -0xa(%bp),%ax
	imulw  -0x14(%bp)
L_0d36:
	jmp   L_0d7a
L_0d38:
	cmp    -0x10(%bp),%di
	jae   L_0d5b
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	mov    D_04cd(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0d5b
	mov    %di,%bx
	inc    %di
	cmpb   $0x30,(%bx)
	je   L_0d38
	mov    -0x14(%bp),%ax
	neg    %ax
	jmp   L_0d7a
L_0d5b:
	cmp    -0x12(%bp),%si
	jae   L_0dba
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	mov    D_04cd(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0dbf
	mov    %si,%bx
	inc    %si
	cmpb   $0x30,(%bx)
	je   L_0d5b
	mov    -0x16(%bp),%ax
L_0d7a:
	jmp   L_0dc7
L_0d7c:
	mov    -0x20(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x6(%bp)
	mov    -0x20(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x8(%bp)
L_0d8d:
	mov    (%di),%al
	cbtw
	add    -0x8(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	je   L_0d9d
	inc    %di
L_0d9b:
	jmp   L_0d8d
L_0d9d:
	mov    (%si),%al
	cbtw
	add    -0x8(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	je   L_0dad
	inc    %si
	jmp   L_0d9d
L_0dad:
	cmp    -0x10(%bp),%di
	jae   L_0db7
	cmpb   $0xa,(%di)
	jne   L_0dc9
L_0db7:
	cmp    -0x12(%bp),%si
L_0dba:
	jae   L_0e19
	cmpb   $0xa,(%si)
L_0dbf:
	je   L_0e19
	mov    -0x20(%bp),%bx
	mov    0x6(%bx),%ax
L_0dc7:
	jmp   L_0ddb
L_0dc9:
	cmp    -0x12(%bp),%si
	jae   L_0dd3
	cmpb   $0xa,(%si)
	jne   L_0ddd
L_0dd3:
	mov    -0x20(%bp),%bx
	mov    0x6(%bx),%ax
	neg    %ax
L_0ddb:
	jmp   L_0e17
L_0ddd:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	add    -0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	add    -0x6(%bp),%dx
	mov    %dx,%bx
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %dx,%ax
	mov    %ax,-0x14(%bp)
	or     %ax,%ax
	je   L_0d9b
	mov    -0x20(%bp),%bx
	mov    0x6(%bx),%ax
	imulw  -0x14(%bp)
L_0e17:
	jmp   L_0e36
L_0e19:
	incw   -0xe(%bp)
	jmp   L_0b3f
L_0e1f:
	cmpw   $0x0,D_09be
	je   L_0e2a
	xor    %ax,%ax
	jmp   L_0e36
L_0e2a:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_0e3d
	add    $0x4,%sp
L_0e36:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e3d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_0e4a:
	mov    (%di),%al
	cbtw
	mov    (%si),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_0e69
	mov    %di,%bx
	inc    %di
	cmpb   $0xa,(%bx)
	jne   L_0e66
	xor    %ax,%ax
	jmp   L_0e99
L_0e66:
	inc    %si
	jmp   L_0e4a
L_0e69:
	cmpb   $0xa,(%di)
	jne   L_0e73
	mov    D_06d2,%ax
	jmp   L_0e99
L_0e73:
	cmpb   $0xa,(%si)
	jne   L_0e7f
	mov    D_06d2,%ax
	neg    %ax
	jmp   L_0e99
L_0e7f:
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jle   L_0e94
	mov    D_06d2,%ax
	jmp   L_0e99
L_0e94:
	mov    D_06d2,%ax
	neg    %ax
L_0e99:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e9d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%si
	mov    0x6(%bp),%ax
	add    $0xc,%ax
	mov    0x8(%bp),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	mov    (%bx),%di
	or     %di,%di
	jge   L_0ec2
	push   %si
	call   L_0f59
	pop    %cx
	jmp   L_0f55
L_0ec2:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	jle   L_0f10
	cmpb   $0x0,D_09d0
	je   L_0eed
L_0ed0:
	mov    (%si),%al
	cbtw
	mov    D_09d0,%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_0eea
	cmpb   $0xa,(%si)
	je   L_0f0b
	inc    %si
	jmp   L_0ed0
L_0eea:
	inc    %si
	jmp   L_0ec2
L_0eed:
	mov    (%si),%al
	cbtw
	push   %ax
	call   L_1130
	pop    %cx
	or     %ax,%ax
	je   L_0efc
	inc    %si
	jmp   L_0eed
L_0efc:
	mov    (%si),%al
	cbtw
	push   %ax
	call   L_1130
	pop    %cx
	or     %ax,%ax
	jne   L_0ec2
	cmpb   $0xa,(%si)
L_0f0b:
	je   L_0f4e
	inc    %si
	jmp   L_0efc
L_0f10:
	mov    0x6(%bp),%ax
	add    $0x8,%ax
	mov    0x8(%bp),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	je   L_0f33
L_0f24:
	mov    (%si),%al
	cbtw
	push   %ax
	call   L_1130
	pop    %cx
	or     %ax,%ax
	je   L_0f33
	inc    %si
	jmp   L_0f24
L_0f33:
	mov    0x6(%bp),%ax
	add    $0x10,%ax
	mov    0x8(%bp),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	mov    (%bx),%di
L_0f44:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	jle   L_0f53
	cmpb   $0xa,(%si)
L_0f4e:
	je   L_0f53
	inc    %si
	jmp   L_0f44
L_0f53:
	mov    %si,%ax
L_0f55:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f59:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
L_0f62:
	cmpb   $0xa,(%di)
	je   L_0f6a
	inc    %di
	jmp   L_0f62
L_0f6a:
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f70:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    $D_040e,%si
	mov    D_09bc,%ax
	mov    $0x14,%dx
	imul   %dx
	add    $D_06cc,%ax
	mov    %ax,-0x6(%bp)
	xor    %di,%di
L_0f89:
	cmp    $0xa,%di
	jge   L_0fa0
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    -0x6(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	inc    %di
	jmp   L_0f89
L_0fa0:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0fa5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_09bc,%ax
	mov    $0x14,%dx
	imul   %dx
	add    $D_06cc,%ax
	mov    %ax,%di
	xor    %si,%si
L_0fb9:
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	jne   L_0fc4
	jmp   L_107f
L_0fc4:
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	cbtw
	jmp   L_1073
L_0fcd:
	lea    0x8(%di),%ax
	mov    0x6(%bp),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	incw   (%bx)
	jmp   L_1028
L_0fdd:
	movw   $D_038e,0x2(%di)
	jmp   L_1028
L_0fe4:
	movw   $D_008e,(%di)
	jmp   L_1028
L_0fea:
	movw   $D_028e,0x2(%di)
	jmp   L_1028
L_0ff1:
	movw   $0x1,D_06c8
	jmp   L_101f
L_0ff9:
	movw   $0x1,D_0794
	jmp   L_101f
L_1001:
	incw   0x4(%di)
	jmp   L_1028
L_1006:
	incw   0x4(%bp)
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	mov    %al,D_09d0
	or     %al,%al
	jne   L_101f
	decw   0x4(%bp)
	jmp   L_101f
L_101a:
	movw   $0xffff,0x6(%di)
L_101f:
	jmp   L_10dc
L_1022:
	movw   $0x1,D_09be
L_1028:
	jmp   L_1071
L_102a:
	lea    0xc(%di),%ax
	mov    0x6(%bp),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	cmpw   $0xffff,(%bx)
	jne   L_104b
	lea    0xc(%di),%ax
	mov    0x6(%bp),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
L_104b:
	mov    $D_06dc,%ax
	sub    $D_06d8,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,%si
L_1059:
	lea    0x4(%bp),%ax
	push   %ax
	call   L_10e6
	pop    %cx
	lea    0xc(%di),%dx
	mov    0x6(%bp),%bx
	add    %si,%bx
	shl    %bx
	add    %bx,%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
L_1071:
	jmp   L_10d6
L_1073:
	cmp    $0x66,%ax
	jne   L_107b
	jmp   L_0fe4
L_107b:
	jg   L_10a0
	or     %ax,%ax
L_107f:
	je   L_10e2
	cmp    $0x2e,%ax
	je   L_102a
	cmp    $0x62,%ax
	jne   L_108e
	jmp   L_0fcd
L_108e:
	cmp    $0x63,%ax
	jne   L_1096
	jmp   L_0ff1
L_1096:
	cmp    $0x64,%ax
	jne   L_109e
	jmp   L_0fdd
L_109e:
	jmp   L_1059
L_10a0:
	cmp    $0x6e,%ax
	jne   L_10a8
	jmp   L_1001
L_10a8:
	jg   L_10bc
	cmp    $0x69,%ax
	jne   L_10b2
	jmp   L_0fea
L_10b2:
	cmp    $0x6d,%ax
	jne   L_10ba
	jmp   L_0ff9
L_10ba:
	jmp   L_109e
L_10bc:
	cmp    $0x72,%ax
	jne   L_10c4
	jmp   L_101a
L_10c4:
	cmp    $0x74,%ax
	jne   L_10cc
	jmp   L_1006
L_10cc:
	cmp    $0x75,%ax
	jne   L_10d4
	jmp   L_1022
L_10d4:
	jmp   L_109e
L_10d6:
	movw   $0xb26,D_000c
L_10dc:
	incw   0x4(%bp)
	jmp   L_0fb9
L_10e2:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_10e6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%bx
	mov    (%bx),%di
	movw   $0x0,-0x6(%bp)
L_10f6:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	mov    D_04cd(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_1128
	mov    -0x6(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	add    %dx,%ax
	sub    $0x30,%ax
	mov    %ax,-0x6(%bp)
	mov    %di,%ax
	inc    %di
	mov    0x4(%bp),%bx
	mov    %ax,(%bx)
	jmp   L_10f6
L_1128:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1130:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x20,0x4(%bp)
	je   L_1141
	cmpw   $0x9,0x4(%bp)
	jne   L_1146
L_1141:
	mov    $0x1,%ax
	jmp   L_1148
L_1146:
	xor    %ax,%ax
L_1148:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_114c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x10,%sp
L_1154:
	mov    0x6(%bp),%ax
	sub    0x4(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0x10(%bp)
	cmp    $0x1,%ax
	ja   L_116b
	jmp   L_12e5
L_116b:
	mov    -0x10(%bp),%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x10(%bp)
	shl    %ax
	add    0x4(%bp),%ax
	mov    %ax,-0x8(%bp)
	mov    %ax,-0xa(%bp)
	mov    0x4(%bp),%di
	mov    0x6(%bp),%ax
	dec    %ax
	dec    %ax
	mov    %ax,%si
L_118d:
	cmp    -0x8(%bp),%di
	jae   L_11cb
	mov    -0x8(%bp),%bx
	pushw  (%bx)
	pushw  (%di)
	/* TODO: indirect call - verify */
	call   *0x200c
	add    $0x4,%sp
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_11c1
	subw   $0x2,-0x8(%bp)
	mov    (%di),%ax
	mov    %ax,-0xe(%bp)
	mov    -0x8(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,(%di)
	mov    -0x8(%bp),%bx
	mov    -0xe(%bp),%ax
	mov    %ax,(%bx)
	jmp   L_118d
L_11c1:
	cmpw   $0x0,-0xc(%bp)
	jge   L_11cb
	inc    %di
	inc    %di
L_11c9:
	jmp   L_118d
L_11cb:
	cmp    -0xa(%bp),%si
	jbe   L_1247
	pushw  (%si)
	mov    -0xa(%bp),%bx
	pushw  (%bx)
	/* TODO: indirect call - verify */
	call   *0x200c
	add    $0x4,%sp
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_11ff
	addw   $0x2,-0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0xe(%bp)
	mov    -0xa(%bp),%bx
	mov    (%si),%ax
	mov    %ax,(%bx)
	mov    -0xe(%bp),%ax
	mov    %ax,(%si)
	jmp   L_11cb
L_11ff:
	cmpw   $0x0,-0xc(%bp)
	jle   L_1243
	cmp    -0x8(%bp),%di
	jne   L_122f
	addw   $0x2,-0xa(%bp)
	mov    (%di),%ax
	mov    %ax,-0xe(%bp)
	mov    (%si),%ax
	mov    %ax,(%di)
	mov    -0xa(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,(%si)
	mov    -0xa(%bp),%bx
	mov    -0xe(%bp),%ax
	mov    %ax,(%bx)
	addw   $0x2,-0x8(%bp)
	mov    -0x8(%bp),%di
	jmp   L_11cb
L_122f:
	mov    (%di),%ax
	mov    %ax,-0xe(%bp)
	mov    (%si),%ax
	mov    %ax,(%di)
	mov    -0xe(%bp),%ax
	mov    %ax,(%si)
	dec    %si
	dec    %si
	inc    %di
	inc    %di
L_1241:
	jmp   L_11c9
L_1243:
	dec    %si
	dec    %si
	jmp   L_11cb
L_1247:
	cmp    -0x8(%bp),%di
	jne   L_12bf
	cmpw   $0x0,D_09be
	je   L_1271
	mov    -0x8(%bp),%ax
	inc    %ax
	inc    %ax
	mov    %ax,-0x6(%bp)
L_125b:
	mov    -0x6(%bp),%ax
	cmp    -0xa(%bp),%ax
	ja   L_1271
	mov    -0x6(%bp),%bx
	mov    (%bx),%bx
	movb   $0x0,(%bx)
	addw   $0x2,-0x6(%bp)
	jmp   L_125b
L_1271:
	mov    0x6(%bp),%ax
	sub    -0xa(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0x14(%bp)
	mov    -0x8(%bp),%ax
	sub    0x4(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	cmp    -0x14(%bp),%ax
	jl   L_12a9
	pushw  0x6(%bp)
	mov    -0xa(%bp),%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_114c
	add    $0x4,%sp
	mov    -0x8(%bp),%ax
	mov    %ax,0x6(%bp)
L_12a6:
	jmp   L_1154
L_12a9:
	pushw  -0x8(%bp)
	pushw  0x4(%bp)
	call   L_114c
	add    $0x4,%sp
	mov    -0xa(%bp),%ax
	inc    %ax
	inc    %ax
	mov    %ax,0x4(%bp)
	jmp   L_12a6
L_12bf:
	subw   $0x2,-0x8(%bp)
	mov    (%si),%ax
	mov    %ax,-0xe(%bp)
	mov    (%di),%ax
	mov    %ax,(%si)
	mov    -0x8(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,(%di)
	mov    -0x8(%bp),%bx
	mov    -0xe(%bp),%ax
	mov    %ax,(%bx)
	subw   $0x2,-0xa(%bp)
	mov    -0xa(%bp),%si
	jmp   L_1241
L_12e5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_12ec:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_12fc:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	je   L_132d
	decw   0x2(%si)
	jl   L_131e
	mov    (%si),%bx
	incw   (%si)
	mov    -0x8(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_1328
L_131e:
	push   %si
	pushw  -0x8(%bp)
	call   L_2364
	add    $0x4,%sp
L_1328:
	mov    %ax,-0x6(%bp)
	jmp   L_12fc
L_132d:
	mov    -0x6(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1338:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getpid(20) */
	/* getpid -> __NR_getpid */
	mov	$__NR_getpid,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1346
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1346:
	pop    %bp
	ret


L_1348:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	cmpb   $0x0,D_04ca
	jne   L_1385
	movw   $D_062a,-0x8(%bp)
	/* suppressed: movw $D_06b2,D_06b2 (self-ref in .data) */
	movw   $0x88,-0xa(%bp)
L_136b:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_1380
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_136b
L_1380:
	movb   $0x1,D_04ca
L_1385:
	movw   $D_0612,-0x6(%bp)
L_138a:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_13a9
	mov    -0x6(%bp),%ax
	cmp    D_06b2,%ax
	jb   L_13a3
	xor    %ax,%ax
	jmp   L_1414
L_13a3:
	addw   $0x8,-0x6(%bp)
	jmp   L_138a
L_13a9:
	cmpb   $0x77,(%di)
	jne   L_13bf
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1446
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_13fe
L_13bf:
	cmpb   $0x61,(%di)
	jne   L_1400
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_27c8
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_13ed
	cmpw   $0x2,D_0ad4
	jne   L_13ed
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1446
	add    $0x4,%sp
	mov    %ax,%si
L_13ed:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_162a
	add    $0x8,%sp
L_13fe:
	jmp   L_140e
L_1400:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_27c8
	add    $0x4,%sp
	mov    %ax,%si
L_140e:
	or     %si,%si
	jge   L_1416
	xor    %ax,%ax
L_1414:
	jmp   L_143e
L_1416:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_1434
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_143b
L_1434:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_143b:
	mov    -0x6(%bp),%ax
L_143e:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1446:
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
	jns    L_145a
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_145a:
	pop    %bp
	ret


L_145c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_1473
	mov    $0xffff,%ax
	jmp   L_147f
L_1473:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_1481
	mov    $0xffff,%ax
L_147f:
	jmp   L_14f9
L_1481:
	cmpw   $0x0,0x4(%di)
	jne   L_14b5
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_149c
	mov    0x7(%di),%al
	cbtw
	add    $D_054e,%ax
	mov    %ax,0x4(%di)
	jmp   L_1481
L_149c:
	mov    $0x200,%ax
	push   %ax
	call   L_2538
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_14b1
	orb    $0x4,0x6(%di)
	jmp   L_1481
L_14b1:
	orb    $0x8,0x6(%di)
L_14b5:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_14c8
	mov    $0x1,%ax
	jmp   L_14cb
L_14c8:
	mov    $0x200,%ax
L_14cb:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_27de
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_14fb
	cmpw   $0xffff,0x2(%di)
	jne   L_14ed
	orb    $0x10,0x6(%di)
	jmp   L_14f1
L_14ed:
	orb    $0x20,0x6(%di)
L_14f1:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_14f9:
	jmp   L_1505
L_14fb:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_1505:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_150a:
	push   %bp
	mov    %sp,%bp
	mov    $0x16,%cx
	mov    0x4(%bp),%ax
	or     %ax,%ax
	jle   L_154d
	cmp    $0x20,%ax
	jg   L_154d
	mov    %ax,%cx
	shl    %ax
	add    $D_0adc,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    0x6(%bp),%ax
	mov    %ax,(%bx)
	cmp    $0x2,%ax
	jl   L_1539
	mov    $0x6,%ax
	mul    %cx
	add    $(L_1556-6),%ax
L_1539:
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
	jl   L_154a
	pop    %ax
	jmp   L_154b
L_154a:
	pop    %bx
L_154b:
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1554
L_154d:
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	mov    $0xffff,%ax
L_1554:
	pop    %bp
	ret
L_1556:
	push   %ax
	mov    $D_0ade,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0ae0,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0ae2,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0ae4,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0ae6,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0ae8,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0aea,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0aec,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0aee,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0af0,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0af2,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0af4,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0af6,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0af8,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0afa,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0afc,%ax
	jmp   L_15ba
	push   %ax
	mov    $D_0afe,%ax
L_15ba:
	jmp   L_1614
	push   %ax
	mov    $D_0b00,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b02,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b04,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b06,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b08,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b0a,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b0c,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b0e,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b10,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b12,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b14,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b16,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b18,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b1a,%ax
	jmp   L_1614
	push   %ax
	mov    $D_0b1c,%ax
L_1614:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	sub    $D_0adc,%ax
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


L_162a:
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
L_1645:
	pop    %si
	pop    %bp
	ret


L_1648:
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
	call   L_169a
	add    $0x6,%sp
	decw   -0xa(%bp)
	jl   L_1682
	mov    -0xc(%bp),%bx
	incw   -0xc(%bp)
	movb   $0x0,(%bx)
	xor    %ax,%ax
	jmp   L_168f
L_1682:
	lea    -0xc(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_2364
	add    $0x4,%sp
L_168f:
	mov    0x4(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_169a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_16b2:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_16c4
	jmp   L_1c61
L_16c4:
	cmpb   $0x25,-0x5(%bp)
	je   L_16d7
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_16d5:
	jmp   L_16b2
L_16d7:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_1702
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_2272
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_1702:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_171b
	incb   -0xa6(%bp)
	inc    %di
L_171b:
	cmpb   $0x30,(%di)
	jne   L_1726
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_1726:
	cmpb   $0x2a,(%di)
	je   L_1756
	movw   $0x0,-0xe(%bp)
L_1730:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_176c
	cmpw   $0x9,-0xc(%bp)
	jg   L_176c
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_1730
L_1756:
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
L_176c:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_17b3
	cmpb   $0x2a,(%di)
	je   L_17b5
	movw   $0x0,-0x10(%bp)
L_1785:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_17af
	cmpw   $0x9,-0xc(%bp)
	jg   L_17af
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_1785
L_17af:
	addw   $0x30,-0xc(%bp)
L_17b3:
	jmp   L_17cc
L_17b5:
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
L_17cc:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_1bd6
L_17d7:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_17e8
L_17df:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_1818
	jmp   L_1811
L_17e8:
	cmp    $0x64,%ax
	je   L_181a
	cmp    $0x6f,%ax
	je   L_17fe
	cmp    $0x75,%ax
	je   L_1811
	cmp    $0x78,%ax
	je   L_180c
	jmp   L_17df
L_17fe:
	movw   $0x8,-0x12(%bp)
	jmp   L_1811
L_1805:
	movw   $0x8,-0x12(%bp)
	jmp   L_1818
L_180c:
	movw   $0x10,-0x12(%bp)
L_1811:
	jmp   L_1863
L_1813:
	movw   $0x10,-0x12(%bp)
L_1818:
	jmp   L_1873
L_181a:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_1837
L_182a:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_1837:
	cmpw   $0x0,-0x14(%bp)
	jg   L_1881
	jl   L_1845
	cmpw   $0x0,-0x16(%bp)
	jae   L_1881
L_1845:
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
	jmp   L_1881
L_1863:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_1881
L_1873:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_1881:
	cmpw   $0x0,-0x14(%bp)
	jne   L_188b
	cmpw   $0x0,-0x16(%bp)
L_188b:
	jne   L_189c
	cmpw   $0x0,-0x10(%bp)
	jne   L_189c
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_189c:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_18a3:
	cmpw   $0x0,-0x14(%bp)
	jne   L_18ad
	cmpw   $0x0,-0x16(%bp)
L_18ad:
	je   L_18f8
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_1dc0
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_18ce
	mov    $0x30,%ax
	jmp   L_18d1
L_18ce:
	mov    $0x57,%ax
L_18d1:
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
	call   L_1cb8
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_18a3
L_18f8:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_1947
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_18f8
L_1916:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_1922:
	cmpw   $0x2,-0xc(%bp)
	jge   L_1947
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_1942
	decw   -0x8(%bp)
L_1942:
	incw   -0xc(%bp)
	jmp   L_1922
L_1947:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_194e:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_1965
	movw   $0x0,-0xc(%bp)
L_1965:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_1974
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_1974:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_2272
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_1997:
	jmp   L_16d5
L_199a:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_19ac
	movw   $D_0562,-0xa(%bp)
L_19ac:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_19bb
	movw   $0x7fff,-0xc(%bp)
L_19bb:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_19c1:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_194e
	decw   -0xc(%bp)
	jl   L_194e
	incw   -0x8(%bp)
	jmp   L_19c1
L_19d3:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_19db:
	jmp   L_1997
L_19dd:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_19e9
	movw   $0x6,-0x10(%bp)
L_19e9:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1fbe
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_1a1e
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_1a1e:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_1a33
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_1a4b
L_1a33:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_1a33
L_1a4b:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_1a5e
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_1a5e:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_1a7d
L_1a6a:
	decw   -0xc(%bp)
	jl   L_1a7d
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_1a6a
L_1a7d:
	cmpw   $0x0,-0xc(%bp)
	jg   L_1a86
	jmp   L_1947
L_1a86:
	decw   -0xc(%bp)
	jge   L_1a8e
	jmp   L_1947
L_1a8e:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_1a86
L_1aa3:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1ab1
	movw   $0x6,-0x10(%bp)
	jmp   L_1ab4
L_1ab1:
	incw   -0x10(%bp)
L_1ab4:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1f92
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_1ae9
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_1ae9:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_1af4
	incw   -0x1a(%bp)
L_1af4:
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
L_1b16:
	decw   -0xc(%bp)
	jle   L_1b30
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_1b16
L_1b30:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_1b4d
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_1b5e
L_1b4d:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_1b5e:
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
	jmp   L_1947
L_1b8f:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1b9b
	movw   $0x6,-0x10(%bp)
L_1b9b:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1e16
	add    $0xc,%sp
	add    $0x8,%si
L_1bb9:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_1bb9
	decw   -0x8(%bp)
	jmp   L_1947
L_1bca:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_1bd3:
	jmp   L_19db
L_1bd6:
	cmp    $0x65,%ax
	jne   L_1bde
	jmp   L_1aa3
L_1bde:
	jg   L_1c1f
	cmp    $0x4f,%ax
	jne   L_1be8
	jmp   L_17fe
L_1be8:
	jg   L_1bff
	cmp    $0x25,%ax
	je   L_1bca
	cmp    $0x44,%ax
	jne   L_1bf7
	jmp   L_181a
L_1bf7:
	cmp    $0x4c,%ax
L_1bfa:
	jne   L_1bff
	jmp   L_17d7
L_1bff:
	cmp    $0x55,%ax
	jne   L_1c07
	jmp   L_1863
L_1c07:
	cmp    $0x58,%ax
	jne   L_1c0f
	jmp   L_180c
L_1c0f:
	cmp    $0x63,%ax
	jne   L_1c17
	jmp   L_1916
L_1c17:
	cmp    $0x64,%ax
	jne   L_1c1f
	jmp   L_182a
L_1c1f:
	cmp    $0x6f,%ax
	jne   L_1c27
	jmp   L_1805
L_1c27:
	jg   L_1c3e
	cmp    $0x66,%ax
	jne   L_1c31
	jmp   L_19dd
L_1c31:
	cmp    $0x67,%ax
	jne   L_1c39
	jmp   L_1b8f
L_1c39:
	cmp    $0x6c,%ax
	je   L_1bfa
L_1c3e:
	cmp    $0x72,%ax
	jne   L_1c46
	jmp   L_19d3
L_1c46:
	cmp    $0x73,%ax
	jne   L_1c4e
	jmp   L_199a
L_1c4e:
	cmp    $0x75,%ax
	jne   L_1c56
	jmp   L_1873
L_1c56:
	cmp    $0x78,%ax
	jne   L_1c5e
	jmp   L_1813
L_1c5e:
	jmp   L_1bd3
L_1c61:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_1c85
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_2272
	add    $0x8,%sp
L_1c85:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_1df4


L_1c8f:
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
	call   L_1df4


L_1cb8:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_1cd6
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_1cd6:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_1cdf:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_1cdf
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_1cef:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_1cef
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1c8f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_1d30
	jne   L_1d25
	or     $0x0,%ax
	je   L_1d30
L_1d25:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_1d30:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_1df4


L_1d38:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_1d54
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_1d54:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_1d6b
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_1d6b:
	push   %dx
	push   %ax
	call   L_1cb8
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_1d85
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_1d85:
	ret
	call   L_1df4
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_1d38
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1c8f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_1df4


L_1dc0:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_1cb8
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1c8f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_1df4:
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


L_1e16:
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
	call   L_1f92
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_1e50
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_1e50:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_1e57:
	cmpw   $0x0,-0xa(%bp)
	jle   L_1e71
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_1e71
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_1e57
L_1e71:
	cmpw   $0x0,-0x8(%bp)
	jl   L_1e82
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_1e8e
L_1e82:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1e8c
	cmpw   $0xfffd,-0x8(%bp)
L_1e8c:
	jge   L_1f0d
L_1e8e:
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
L_1ea9:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_1ec3
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_1ea9
L_1ec3:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_1edf
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_1ee5
L_1edf:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_1ee5:
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
	jmp   L_1f7d
L_1f0d:
	cmpw   $0x0,-0x8(%bp)
	jg   L_1f2f
	cmpb   $0x30,(%di)
	je   L_1f1e
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1f1e:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1f2f
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_1f1e
L_1f2f:
	movw   $0x1,-0xa(%bp)
L_1f34:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_1f5c
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
	jne   L_1f57
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1f57:
	incw   -0xa(%bp)
	jmp   L_1f34
L_1f5c:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_1f7d
L_1f64:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_1f77
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_1f64
L_1f77:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1f7d:
	cmpb   $0x2e,-0x1(%si)
	jne   L_1f84
	dec    %si
L_1f84:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1f92:
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
	call   L_1fe9
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1fbe:
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
	call   L_1fe9
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1fe9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_1ffc
	movw   $0x0,0xc(%bp)
L_1ffc:
	cmpw   $0x4f,0xc(%bp)
	jl   L_2007
	movw   $0x4e,0xc(%bp)
L_2007:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_056a,%si
	fldl   0x4(%bp)
	fcompl D_05ba
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_2044
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_2044:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2888
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $D_05ba,-0x16(%bp)
	fldl   -0xc(%bp)
	fcompl D_05c2
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_208f
	jmp   L_213c
L_208f:
	movw   $D_05ba,-0x16(%bp)
L_2094:
	fldl   -0xc(%bp)
	fcompl D_05ca
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_20b0
	jmp   L_213a
L_20b0:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	fdivl  D_05d2
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2888
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   D_05da
	fstl   -0x1e(%bp)
	fdivl  D_05e2
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  D_05ea
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
	jmp   L_2094
L_2123:
	cmpw   $D_05ba,-0x16(%bp)
	jae   L_217d
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_213a:
	jmp   L_2123
L_213c:
	fldl   0x4(%bp)
	fcompl D_05f2
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_218c
L_2155:
	fldl   0x4(%bp)
	fmull  D_05fa
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl D_0602
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_217d:
	jae   L_218c
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_2155
L_218c:
	mov    0xc(%bp),%ax
	add    $D_056a,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_219e
	add    %di,-0x16(%bp)
L_219e:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_056a,-0x16(%bp)
	jae   L_21b4
	movb   $0x0,D_056a
	mov    $D_056a,%ax
	jmp   L_221f
L_21b4:
	cmp    -0x16(%bp),%si
	ja   L_2210
	cmp    $D_05ba,%si
	jae   L_2210
	fldl   0x4(%bp)
	fmull  D_060a
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2888
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
	jmp   L_21b4
L_2210:
	cmpw   $D_05ba,-0x16(%bp)
	jb   L_2221
	movb   $0x0,D_05b9
	mov    $D_056a,%ax
L_221f:
	jmp   L_226b
L_2221:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_2229:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_2265
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_056a,-0x16(%bp)
	jbe   L_2248
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_2229
L_2248:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_2229
	cmp    $D_056a,%si
	jbe   L_2262
	movb   $0x30,(%si)
L_2262:
	inc    %si
	jmp   L_2229
L_2265:
	movb   $0x0,(%si)
	mov    $D_056a,%ax
L_226b:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2272:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_227f:
	cmpw   $0x0,0x8(%bp)
	jl   L_2288
	jmp   L_2316
L_2288:
	cmpb   $0x2d,(%di)
	jne   L_22c5
	cmpw   $0x30,0xc(%bp)
	jne   L_22c5
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_22b4
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
	jmp   L_22c4
L_22b4:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_2364
	add    $0x4,%sp
L_22c4:
	dec    %si
L_22c5:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_22e3
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
	jmp   L_22ef
L_22e3:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_2364
	add    $0x4,%sp
L_22ef:
	incw   0x8(%bp)
	jmp   L_227f
L_22f4:
	dec    %si
	jl   L_232a
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_2318
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
L_2316:
	jmp   L_22f4
L_2318:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_2364
	add    $0x4,%sp
	jmp   L_22f4
L_232a:
	cmpw   $0x0,0x8(%bp)
	je   L_235f
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_234e
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
	jmp   L_235a
L_234e:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_2364
	add    $0x4,%sp
L_235a:
	decw   0x8(%bp)
	jmp   L_232a
L_235f:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2364:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_237f
	mov    $0xffff,%ax
	jmp   L_2446
L_237f:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_23b2
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
	call   L_286e
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_2437
L_23b2:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_2404
	cmp    $D_061a,%di
	jne   L_23dd
	mov    D_0621,%al
	cbtw
	push   %ax
	call   L_27a0
	pop    %cx
	or     %ax,%ax
	je   L_23d2
	orb    $0x4,0x6(%di)
	jmp   L_237f
L_23d2:
	movw   $D_0b24,0x4(%di)
	movw   $D_0b24,(%di)
	jmp   L_237f
L_23dd:
	mov    $0x200,%ax
	push   %ax
	call   L_2538
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_23f4
	orb    $0x4,0x6(%di)
	jmp   L_237f
L_23f4:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_2426
L_2404:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_2426
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_286e
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_2426:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_2437:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_2448
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_2446:
	jmp   L_244b
L_2448:
	mov    0x4(%bp),%ax
L_244b:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2452:
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
	jne   L_249e
	mov    0x4(%di),%si
	or     %si,%si
	je   L_249e
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_249e
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_286e
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_249e
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_24a0
L_249e:
	xor    %ax,%ax
L_24a0:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_24a5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0612,%di
L_24ad:
	cmp    D_06b2,%di
	jae   L_24bd
	push   %di
	call   L_24c1
	pop    %cx
	add    $0x8,%di
	jmp   L_24ad
L_24bd:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_24c1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_2514
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_2514
	push   %di
	call   L_2452
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_278c
	pop    %cx
	or     %ax,%ax
	jge   L_24f6
	mov    $0xffff,%si
L_24f6:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_2506
	pushw  0x4(%di)
	call   L_2682
	pop    %cx
L_2506:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_2514
	movw   $0x0,0x4(%di)
L_2514:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_2538:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_06b4
	jne   L_2563
	mov    $D_06b6,%ax
	or     $0x1,%ax
	mov    %ax,D_06b4
L_254e:
	mov    $D_06b4,%ax
	or     $0x1,%ax
L_2554:
	mov    %ax,D_06b6
	/* dual-reloc split: movw $D_06b6,D_0f26 */
	mov	$D_06b6,%ax
	mov	%ax,D_0f26
	/* dual-reloc split: movw $D_06b4,D_0f24 */
	mov	$D_06b4,%ax
	mov	%ax,D_0f24
L_2563:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
L_2570:
	mov    %ax,-0x6(%bp)
	mov    D_0f24,%di
L_2577:
	movw   $0x0,D_06b8
L_257d:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_25ac
L_2584:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_2593
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_2584
L_2593:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_25ac
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_25ac
	jmp   L_2653
L_25ac:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_257d
	cmp    D_0f26,%si
	jne   L_25c5
	cmp    $D_06b4,%di
	je   L_25c9
L_25c5:
	xor    %ax,%ax
	jmp   L_2601
L_25c9:
	incw   D_06b8
	cmpw   $0x1,D_06b8
	jle   L_257d
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_06b8
	xor    %ax,%ax
	push   %ax
	call   L_2766
	pop    %cx
	mov    %ax,%si
	mov    D_06b8,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_2603
	xor    %ax,%ax
L_2601:
	jmp   L_2616
L_2603:
	mov    D_06b8,%ax
	shl    %ax
	push   %ax
	call   L_2766
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
L_2612:
	jne   L_2618
	xor    %ax,%ax
L_2616:
	jmp   L_267d
L_2618:
	mov    D_0f26,%bx
	mov    %si,(%bx)
	mov    D_0f26,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_2636
	mov    D_0f26,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_0f26,%bx
	mov    %ax,(%bx)
L_2636:
	mov    D_06b8,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_0f26
	mov    $D_06b4,%ax
	or     $0x1,%ax
	mov    D_0f26,%bx
	mov    %ax,(%bx)
	jmp   L_2577
L_2653:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_0f24
	cmp    %ax,%si
	jbe   L_2672
	mov    D_0f24,%bx
	mov    (%bx),%ax
	mov    %ax,D_0f28
	mov    D_0f24,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_2672:
	mov    D_0f24,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_267d:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2682:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_0f24
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
	je   L_26b7
	push   %di
	call   L_2682
	pop    %cx
L_26b7:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_2538
	pop    %cx
L_26cc:
	mov    %ax,%si
	or     %si,%si
	je   L_26d6
L_26d2:
	cmp    %di,%si
	jne   L_26da
L_26d6:
	mov    %si,%ax
L_26d8:
	jmp   L_2742
L_26da:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_26f9
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_26f9:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_2717
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_26f9
L_2717:
	cmp    %di,%si
	jae   L_2740
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_2740
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
	mov    D_0f28,%ax
	mov    %ax,(%bx)
L_2740:
	mov    %si,%ax
L_2742:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_274a:
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
	jns    L_275d
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_2764
L_275d:
	mov    0x4(%bp),%bx
	mov    %bx,D_06ba
L_2764:
	pop    %bp
	ret


L_2766:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_06ba,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_277d
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_278a
L_277d:
	mov    D_06ba,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_06ba
L_278a:
	pop    %bp
	ret


L_278c:
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
	jns    L_279d
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_279d:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_27a0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2827
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_27bd
	xor    %ax,%ax
	jmp   L_27c0
L_27bd:
	mov    $0x1,%ax
L_27c0:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_27c8:
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
	jns    L_27dc
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_27dc:
	pop    %bp
	ret


L_27de:
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
	jns    L_27f5
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_27f5:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_27f8:
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
	jns    L_280c
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_280c:
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
	call   L_2840
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2827:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2840
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2840:
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
	jns    L_2857
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2857:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_285a:
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
	jns    L_286b
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_286b:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_286e:
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
	jns    L_2885
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2885:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2888:
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


L_28a4:
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
	jns    L_28b5
	neg	%ax
	mov	%ax,D_0ad4	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_28b5:
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
	mov	%ax,D_0ad4	/* errno */
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
	.ascii	"$$-$\0"
	.byte	0x00	/* |.| */
D_000a:
	.byte	0x9c, 0x27	/* |.'| */
D_000c:
	.byte	0x3d, 0x0e, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85	/* |=.......| */
	.byte	0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d	/* |........| */
	.byte	0x8e, 0x8f, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95	/* |........| */
	.byte	0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d	/* |........| */
	.byte	0x9e, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5	/* |........| */
	.byte	0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad	/* |........| */
	.byte	0xae, 0xaf, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5	/* |........| */
	.byte	0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd	/* |........| */
	.byte	0xbe, 0xbf, 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5	/* |........| */
	.byte	0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd	/* |........| */
	.byte	0xce, 0xcf, 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5	/* |........| */
	.byte	0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd	/* |........| */
	.byte	0xde, 0xdf, 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5	/* |........| */
	.byte	0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed	/* |........| */
	.byte	0xee, 0xef, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5	/* |........| */
	.byte	0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd	/* |........| */
	.byte	0xfe, 0xff	/* |..| */
D_008e:
	.byte	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07	/* |........| */
	.byte	0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f	/* |........| */
	.byte	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17	/* |........| */
	.byte	0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f	/* |........| */
	.byte	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27	/* | !"#$%&'| */
	.byte	0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f	/* |()*+,-./| */
	.byte	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37	/* |01234567| */
	.byte	0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f	/* |89:;<=>?| */
	.byte	0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47	/* |@ABCDEFG| */
	.byte	0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f	/* |HIJKLMNO| */
	.byte	0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57	/* |PQRSTUVW| */
	.byte	0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5c, 0x5e, 0x5f	/* |XYZ[\\^_| */
	.byte	0x60, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47	/* |`ABCDEFG| */
	.byte	0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f	/* |HIJKLMNO| */
	.byte	0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57	/* |PQRSTUVW| */
	.byte	0x58, 0x59, 0x5a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f	/* |XYZ{|}~.| */
	.byte	0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87	/* |........| */
	.byte	0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f	/* |........| */
	.byte	0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97	/* |........| */
	.byte	0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f	/* |........| */
	.byte	0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7	/* |........| */
	.byte	0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf	/* |........| */
	.byte	0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7	/* |........| */
	.byte	0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf	/* |........| */
	.byte	0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7	/* |........| */
	.byte	0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf	/* |........| */
	.byte	0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7	/* |........| */
	.byte	0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf	/* |........| */
	.byte	0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7	/* |........| */
	.byte	0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef	/* |........| */
	.byte	0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7	/* |........| */
	.byte	0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff	/* |........| */
	.byte	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07	/* |........| */
	.byte	0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f	/* |........| */
	.byte	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17	/* |........| */
	.byte	0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f	/* |........| */
	.byte	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27	/* | !"#$%&'| */
	.byte	0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f	/* |()*+,-./| */
	.byte	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37	/* |01234567| */
	.byte	0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f	/* |89:;<=>?| */
	.byte	0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47	/* |@ABCDEFG| */
	.byte	0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f	/* |HIJKLMNO| */
	.byte	0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57	/* |PQRSTUVW| */
	.byte	0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f	/* |XYZ[\]^_| */
	.byte	0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67	/* |`abcdefg| */
	.byte	0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f	/* |hijklmno| */
	.byte	0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77	/* |pqrstuvw| */
	.byte	0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f	/* |xyz{|}~.| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
D_028e:
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x00, 0x00, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
D_038e:
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x00, 0x00, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x00, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
D_040e:
	.byte	0x8e, 0x21	/* |.!| */
D_0410:
	.word	D_0a54	/* ptr: 0x2a54 */
D_0412:
	.byte	0x00, 0x00	/* |..| */
D_0414:
	.byte	0x01, 0x00	/* |..| */
D_0416:
	.byte	0x00, 0x00	/* |..| */
D_0418:
	.byte	0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
D_0422:
	.byte	0x01, 0x00	/* |..| */
	.ascii	"/usr/tmp\0"
	.ascii	"/tmp\0"
D_0432:
	.byte	0x2d, 0x00	/* |-.| */
D_0434:
	.ascii	"too many keys\0"
D_0442:
	.byte	0x00	/* |.| */
D_0443:
	.byte	0x2d, 0x00	/* |-.| */
D_0445:
	.ascii	"can check only 1 file\0"
D_045b:
	.byte	0x00	/* |.| */
D_045c:
	.ascii	"%s/stm%05uaa\0"
D_0469:
	.ascii	"can't locate temp\0"
D_047b:
	.byte	0x00	/* |.| */
D_047c:
	.byte	0x72, 0x00	/* |r.| */
D_047e:
	.byte	0x72, 0x00	/* |r.| */
D_0480:
	.ascii	"disorder:\0"
D_048a:
	.ascii	"nonunique:\0"
D_0495:
	.byte	0x77, 0x00	/* |w.| */
D_0497:
	.ascii	"can't create \0"
D_04a5:
	.byte	0x77, 0x00	/* |w.| */
D_04a7:
	.ascii	"can't create \0"
D_04b5:
	.ascii	"can't open \0"
D_04c1:
	.ascii	"sort: \0"
D_04c8:
	.word	D_000a	/* ptr: 0x000a */
D_04ca:
	.byte	0x00, 0x00, 0x00	/* |...| */
D_04cd:
	.byte	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |        | */
	.byte	0x20	/* | | */
	.word	D_0808	/* ptr: 0x0808 */
	.word	D_0808	/* ptr: 0x0808 */
	.byte	0x08, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |.       | */
	.byte	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |        | */
	.byte	0x20, 0x20	/* |  | */
	.word	D_0820	/* ptr: 0x0820 */
	.byte	0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |......| */
	.word	D_0410	/* ptr: 0x0410 */
	.byte	0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04	/* |........| */
	.byte	0x04, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x01, 0x01	/* |AAAAAA..| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x02, 0x02	/* |BBBBBB..| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x10, 0x10, 0x10, 0x10, 0x20, 0x00	/* |...... .| */
D_054e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_0562:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_056a:
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
D_05b9:
	.byte	0x00	/* |.| */
D_05ba:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_05c2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_05ca:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_05d2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_05da:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_05e2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_05ea:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_05f2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_05fa:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0602:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_060a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0612:
	.byte	0x24, 0x2d, 0x00, 0x00	/* |$-..| */
	.word	D_0d24	/* ptr: 0x2d24 */
	.byte	0x01, 0x00	/* |..| */
D_061a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02	/* |.......| */
D_0621:
	.byte	0x01	/* |.| */
D_0622:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
D_062a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_06b2:
	.byte	0xb2, 0x26	/* |.&| */
D_06b4:
	.byte	0x00, 0x00	/* |..| */
D_06b6:
	.byte	0x00, 0x00	/* |..| */
D_06b8:
	.byte	0x00, 0x00	/* |..| */
D_06ba:
	.word	D_0f2a	/* ptr: 0x2f2a */

	.bss

	.space	2
D_06be:	.space	2	/* bss word @ 0x06be */
D_06c0:	.space	2	/* bss word @ 0x06c0 */
D_06c2:	.space	2	/* bss word @ 0x06c2 */
D_06c4:	.space	2	/* bss word @ 0x06c4 */
D_06c6:	.space	2	/* bss word @ 0x06c6 */
D_06c8:	.space	2	/* bss word @ 0x06c8 */
	.space	2
D_06cc:	.space	2	/* bss word @ 0x06cc */
	.space	4
D_06d2:	.space	2	/* bss word @ 0x06d2 */
	.space	4
D_06d8:	.space	2	/* bss word @ 0x06d8 */
	.space	2
D_06dc:	.space	2	/* bss word @ 0x06dc */
	.space	182
D_0794:	.space	2	/* bss word @ 0x0794 */
	.space	6
D_079c:	.space	2	/* bss word @ 0x079c */
	.space	30
D_07bc:	.space	2	/* bss word @ 0x07bc */
	.space	74
D_0808:	.space	2	/* bss word @ 0x0808 */
	.space	22
D_0820:	.space	2	/* bss word @ 0x0820 */
	.space	410
D_09bc:	.space	2	/* bss word @ 0x09bc */
D_09be:	.space	2	/* bss word @ 0x09be */
D_09c0:	.space	2	/* bss word @ 0x09c0 */
D_09c2:	.space	2	/* bss word @ 0x09c2 */
D_09c4:	.space	2	/* bss word @ 0x09c4 */
D_09c6:	.space	2	/* bss word @ 0x09c6 */
D_09c8:	.space	2	/* bss word @ 0x09c8 */
D_09ca:	.space	2	/* bss word @ 0x09ca */
D_09cc:	.space	2	/* bss word @ 0x09cc */
D_09ce:	.space	2	/* bss word @ 0x09ce */
D_09d0:	.space	2	/* bss word @ 0x09d0 */
	.space	130
D_0a54:	.space	2	/* bss word @ 0x0a54 */
	.space	126
D_0ad4:	.space	2	/* bss word @ 0x0ad4 */
	.space	6
D_0adc:	.space	2	/* bss word @ 0x0adc */
D_0ade:	.space	2	/* bss word @ 0x0ade */
D_0ae0:	.space	2	/* bss word @ 0x0ae0 */
D_0ae2:	.space	2	/* bss word @ 0x0ae2 */
D_0ae4:	.space	2	/* bss word @ 0x0ae4 */
D_0ae6:	.space	2	/* bss word @ 0x0ae6 */
D_0ae8:	.space	2	/* bss word @ 0x0ae8 */
D_0aea:	.space	2	/* bss word @ 0x0aea */
D_0aec:	.space	2	/* bss word @ 0x0aec */
D_0aee:	.space	2	/* bss word @ 0x0aee */
D_0af0:	.space	2	/* bss word @ 0x0af0 */
D_0af2:	.space	2	/* bss word @ 0x0af2 */
D_0af4:	.space	2	/* bss word @ 0x0af4 */
D_0af6:	.space	2	/* bss word @ 0x0af6 */
D_0af8:	.space	2	/* bss word @ 0x0af8 */
D_0afa:	.space	2	/* bss word @ 0x0afa */
D_0afc:	.space	2	/* bss word @ 0x0afc */
D_0afe:	.space	2	/* bss word @ 0x0afe */
D_0b00:	.space	2	/* bss word @ 0x0b00 */
D_0b02:	.space	2	/* bss word @ 0x0b02 */
D_0b04:	.space	2	/* bss word @ 0x0b04 */
D_0b06:	.space	2	/* bss word @ 0x0b06 */
D_0b08:	.space	2	/* bss word @ 0x0b08 */
D_0b0a:	.space	2	/* bss word @ 0x0b0a */
D_0b0c:	.space	2	/* bss word @ 0x0b0c */
D_0b0e:	.space	2	/* bss word @ 0x0b0e */
D_0b10:	.space	2	/* bss word @ 0x0b10 */
D_0b12:	.space	2	/* bss word @ 0x0b12 */
D_0b14:	.space	2	/* bss word @ 0x0b14 */
D_0b16:	.space	2	/* bss word @ 0x0b16 */
D_0b18:	.space	2	/* bss word @ 0x0b18 */
D_0b1a:	.space	2	/* bss word @ 0x0b1a */
D_0b1c:	.space	2	/* bss word @ 0x0b1c */
	.space	6
D_0b24:	.space	2	/* bss word @ 0x0b24 */
	.space	510
D_0d24:	.space	2	/* bss word @ 0x0d24 */
	.space	510
D_0f24:	.space	2	/* bss word @ 0x0f24 */
D_0f26:	.space	2	/* bss word @ 0x0f26 */
D_0f28:	.space	2	/* bss word @ 0x0f28 */
D_0f2a:	/* endcore: initial break */

/* end of file */
