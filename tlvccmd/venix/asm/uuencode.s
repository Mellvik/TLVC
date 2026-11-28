/* uuencode.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 uuencode.s -o uuencode.o
 * Syscalls used:
 *   1:exit            3:read            4:write           5:open            6:close           8:creat
 *   17:brk            19:lseek          28:fstat          54:ioctl
 * INT instructions: $0xf1  $0xf4
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: uuencode  [linked executable]
//   Magic:    0x0107  OMAGIC (object/impure)
//   a_stack:  0x0000  (0 bytes)
//   a_text:   0x000018aa  (6314 bytes)
//   a_data:   0x0000048e  (1166 bytes)
//   Load:  cs:0x0000 (text)   ds:0x18aa (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	/* vrewrite: initialize _environ from envp (TLVC crt0 3rd arg) */
	mov	0x8(%bp),%ax
	mov	%ax,D_048e
	push   %si
	push   %di
	sub    $0x22,%sp
	cmpw   $0x2,0x4(%bp)
	jle   L_0076
	/* TODO: data ref: mov    $0x18d4,%ax */
	mov    $D_002a,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	call   L_034a
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_006d
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	call   L_045e
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_006d:
	addw   $0x2,0x6(%bp)
	decw   0x4(%bp)
	jmp   L_007b
L_0076:
	/* TODO: data ref: movw   $0x1c8e,-0x6(%bp) */
	movw   $D_03e4,-0x6(%bp)
L_007b:
	cmpw   $0x2,0x4(%bp)
	je   L_0091
	/* TODO: data ref: mov    $0x18d6,%ax */
	mov    $D_002c,%ax
	push   %ax
	call   L_04ce
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0091:
	lea    -0x24(%bp),%ax
	push   %ax
	mov    -0x6(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_17a6
	add    $0x4,%sp
	mov    -0x20(%bp),%ax
	and    $0x1ff,%ax
	mov    %ax,-0x26(%bp)
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	pushw  -0x26(%bp)
	/* TODO: data ref: mov    $0x18fb,%ax */
	mov    $D_0051,%ax
	push   %ax
	call   L_04ce
	add    $0x6,%sp
	/* TODO: data ref: mov    $0x1c96,%ax */
	mov    $D_03ec,%ax
	push   %ax
	pushw  -0x6(%bp)
	call   L_00e2
	add    $0x4,%sp
	/* TODO: data ref: mov    $0x1908,%ax */
	mov    $D_005e,%ax
	push   %ax
	call   L_04ce
	pop    %cx
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_00e2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x54,%sp
L_00ea:
	mov    $0x2d,%ax
	push   %ax
	lea    -0x54(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_02e8
	add    $0x6,%sp
	mov    %ax,-0x58(%bp)
	mov    0x6(%bp),%bx
	decw   0x2(%bx)
	jl   L_0122
	mov    -0x58(%bp),%ax
	and    $0x3f,%ax
	add    $0x20,%ax
	mov    0x6(%bp),%bx
	incw   (%bx)
	mov    (%bx),%dx
	dec    %dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_0135
L_0122:
	pushw  0x6(%bp)
	mov    -0x58(%bp),%ax
	and    $0x3f,%ax
	add    $0x20,%ax
	push   %ax
	call   L_136a
	add    $0x4,%sp
L_0135:
	movw   $0x0,-0x56(%bp)
L_013a:
	mov    -0x56(%bp),%ax
	cmp    -0x58(%bp),%ax
	jge   L_0158
	pushw  0x6(%bp)
	lea    -0x54(%bp),%ax
	add    -0x56(%bp),%ax
	push   %ax
	call   L_018f
	add    $0x4,%sp
	addw   $0x3,-0x56(%bp)
	jmp   L_013a
L_0158:
	mov    0x6(%bp),%bx
	decw   0x2(%bx)
	jl   L_0172
	mov    0x6(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	movb   $0xa,(%bx)
	mov    $0xa,%ax
	jmp   L_017f
L_0172:
	pushw  0x6(%bp)
	mov    $0xa,%ax
	push   %ax
	call   L_136a
	add    $0x4,%sp
L_017f:
	cmpw   $0x0,-0x58(%bp)
	jle   L_0188
	jmp   L_00ea
L_0188:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_018f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    $0x2,%cx
	sar    %cl,%ax
	mov    %ax,-0x6(%bp)
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    $0x4,%cx
	shl    %cl,%ax
	and    $0x30,%ax
	mov    0x4(%bp),%bx
	mov    0x1(%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	mov    $0x4,%cx
	sar    %cl,%dx
	and    $0xf,%dx
	or     %dx,%ax
	mov    %ax,-0x8(%bp)
	mov    0x4(%bp),%bx
	mov    0x1(%bx),%al
	cbtw
	mov    $0x2,%cx
	shl    %cl,%ax
	and    $0x3c,%ax
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	mov    $0x6,%cx
	sar    %cl,%dx
	and    $0x3,%dx
	or     %dx,%ax
	mov    %ax,-0xa(%bp)
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%al
	cbtw
	and    $0x3f,%ax
	mov    %ax,-0xc(%bp)
	mov    0x6(%bp),%bx
	decw   0x2(%bx)
	jl   L_0229
	mov    -0x6(%bp),%ax
	and    $0x3f,%ax
	add    $0x20,%ax
	mov    0x6(%bp),%bx
	incw   (%bx)
	mov    (%bx),%dx
	dec    %dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_023c
L_0229:
	pushw  0x6(%bp)
	mov    -0x6(%bp),%ax
	and    $0x3f,%ax
	add    $0x20,%ax
	push   %ax
	call   L_136a
	add    $0x4,%sp
L_023c:
	mov    0x6(%bp),%bx
	decw   0x2(%bx)
	jl   L_0260
	mov    -0x8(%bp),%ax
	and    $0x3f,%ax
	add    $0x20,%ax
	mov    0x6(%bp),%bx
	incw   (%bx)
	mov    (%bx),%dx
	dec    %dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_0273
L_0260:
	pushw  0x6(%bp)
	mov    -0x8(%bp),%ax
	and    $0x3f,%ax
	add    $0x20,%ax
	push   %ax
	call   L_136a
	add    $0x4,%sp
L_0273:
	mov    0x6(%bp),%bx
	decw   0x2(%bx)
	jl   L_0297
	mov    -0xa(%bp),%ax
	and    $0x3f,%ax
	add    $0x20,%ax
	mov    0x6(%bp),%bx
	incw   (%bx)
	mov    (%bx),%dx
	dec    %dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_02aa
L_0297:
	pushw  0x6(%bp)
	mov    -0xa(%bp),%ax
	and    $0x3f,%ax
	add    $0x20,%ax
	push   %ax
	call   L_136a
	add    $0x4,%sp
L_02aa:
	mov    0x6(%bp),%bx
	decw   0x2(%bx)
	jl   L_02ce
	mov    -0xc(%bp),%ax
	and    $0x3f,%ax
	add    $0x20,%ax
	mov    0x6(%bp),%bx
	incw   (%bx)
	mov    (%bx),%dx
	dec    %dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_02e1
L_02ce:
	pushw  0x6(%bp)
	mov    -0xc(%bp),%ax
	and    $0x3f,%ax
	add    $0x20,%ax
	push   %ax
	call   L_136a
	add    $0x4,%sp
L_02e1:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_02e8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	movw   $0x0,-0x8(%bp)
L_02f5:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_033f
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_0317
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_031e
L_0317:
	pushw  0x4(%bp)
	call   L_04f8
	pop    %cx
L_031e:
	mov    %ax,-0x6(%bp)
	cmp    $0xffff,%ax
	jne   L_032b
	mov    -0x8(%bp),%ax
	jmp   L_0342
L_032b:
	mov    0x6(%bp),%ax
	add    -0x8(%bp),%ax
	mov    %ax,%bx
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_02f5
L_033f:
	mov    0x8(%bp),%ax
L_0342:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_034a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	/* TODO: data ref: cmpb   $0x0,0x190e */
	cmpb   $0x0,D_0064
	jne   L_0387
	/* TODO: data ref: movw   $0x1ca6,-0x8(%bp) */
	movw   $D_03fc,-0x8(%bp)
	/* TODO: data ref: movw   $0x1d2e,0x1d2e */
	/* dual-reloc split: movw $D_0484,D_0484 */
	mov	$D_0484,%ax
	mov	%ax,D_0484
	movw   $0x88,-0xa(%bp)
L_036d:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_0382
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_036d
L_0382:
	/* TODO: data ref: movb   $0x1,0x190e */
	movb   $0x1,D_0064
L_0387:
	/* TODO: data ref: movw   $0x1c8e,-0x6(%bp) */
	movw   $D_03e4,-0x6(%bp)
L_038c:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_03ab
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x1d2e,%ax */
	cmp    D_0484,%ax
	jb   L_03a5
	xor    %ax,%ax
	jmp   L_0416
L_03a5:
	addw   $0x8,-0x6(%bp)
	jmp   L_038c
L_03ab:
	cmpb   $0x77,(%di)
	jne   L_03c1
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0448
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_0400
L_03c1:
	cmpb   $0x61,(%di)
	jne   L_0402
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_17e4
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_03ef
	/* TODO: data ref: cmpw   $0x2,0x1d3a */
	cmpw   $0x2,D_0490
	jne   L_03ef
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0448
	add    $0x4,%sp
	mov    %ax,%si
L_03ef:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_05c2
	add    $0x8,%sp
L_0400:
	jmp   L_0410
L_0402:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_17e4
	add    $0x4,%sp
	mov    %ax,%si
L_0410:
	or     %si,%si
	jge   L_0418
	xor    %ax,%ax
L_0416:
	jmp   L_0440
L_0418:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_0436
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_043d
L_0436:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_043d:
	mov    -0x6(%bp),%ax
L_0440:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0448:
	push   %bp
	mov    %sp,%bp
	mov	$8,%bx	/* sc: creat */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 8:creat */
	int	$0xf1
	jcxz   L_045c
	/* TODO: data ref: mov    %cx,0x1d3a */
	mov    %cx,D_0490
L_045c:
	pop    %bp
	ret


L_045e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x1910,%di */
	mov    $D_0066,%di
	/* TODO: data ref: mov    0x1d3a,%ax */
	mov    D_0490,%ax
	/* TODO: data ref: cmp    0x196a,%ax */
	cmp    D_00c0,%ax
	jge   L_0479
	/* TODO: data ref: mov    0x1d3a,%bx */
	mov    D_0490,%bx
	shl    %bx
	/* TODO: data ref: mov    0x1924(%bx),%di */
	mov    D_007a(%bx),%di
L_0479:
	pushw  0x4(%bp)
	call   L_05a6
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_04a6
	push   %si
	pushw  0x4(%bp)
	mov    $0x2,%ax
	push   %ax
	call   L_1860
	add    $0x6,%sp
	mov    $0x2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x191e,%ax */
	mov    $D_0074,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_1860
	add    $0x6,%sp
L_04a6:
	push   %di
	call   L_05a6
	pop    %cx
	push   %ax
	push   %di
	mov    $0x2,%ax
	push   %ax
	call   L_1860
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1921,%ax */
	mov    $D_0077,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_1860
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_04ce:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x1c96,%ax */
	mov    $D_03ec,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_05e0
	add    $0x6,%sp
	/* TODO: data ref: mov    0x1c9c,%al */
	mov    D_03f2,%al
	cbtw
	test   $0x20,%ax
	je   L_04f2
	mov    $0xffff,%ax
	jmp   L_04f4
L_04f2:
	xor    %ax,%ax
L_04f4:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_04f8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_050f
	mov    $0xffff,%ax
	jmp   L_051b
L_050f:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_051d
	mov    $0xffff,%ax
L_051b:
	jmp   L_0595
L_051d:
	cmpw   $0x0,0x4(%di)
	jne   L_0551
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0538
	mov    0x7(%di),%al
	cbtw
	/* TODO: data ref: add    $0x1bba,%ax */
	add    $D_0310,%ax
	mov    %ax,0x4(%di)
	jmp   L_051d
L_0538:
	mov    $0x200,%ax
	push   %ax
	call   L_153e
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_054d
	orb    $0x4,0x6(%di)
	jmp   L_051d
L_054d:
	orb    $0x8,0x6(%di)
L_0551:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0564
	mov    $0x1,%ax
	jmp   L_0567
L_0564:
	mov    $0x200,%ax
L_0567:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_17fa
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_0597
	cmpw   $0xffff,0x2(%di)
	jne   L_0589
	orb    $0x10,0x6(%di)
	jmp   L_058d
L_0589:
	orb    $0x20,0x6(%di)
L_058d:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_0595:
	jmp   L_05a1
L_0597:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_05a1:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_05a6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_05b1:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_05bc
	inc    %si
	jmp   L_05b1
L_05bc:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_05c2:
	push   %bp
	push   %si
	mov    %sp,%bp
	mov	$19,%bx	/* sc: lseek */
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    0xa(%bp),%cx
	mov    0xc(%bp),%si
	/* v7: 19:lseek */
	int	$0xf1
	jcxz   L_05dd
	/* TODO: data ref: mov    %cx,0x1d3a */
	mov    %cx,D_0490
L_05dd:
	pop    %si
	pop    %bp
	ret


L_05e0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_05f8:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_060a
	jmp   L_0ba7
L_060a:
	cmpb   $0x25,-0x5(%bp)
	je   L_061d
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_061b:
	jmp   L_05f8
L_061d:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0648
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1278
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0648:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_0661
	incb   -0xa6(%bp)
	inc    %di
L_0661:
	cmpb   $0x30,(%di)
	jne   L_066c
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_066c:
	cmpb   $0x2a,(%di)
	je   L_069c
	movw   $0x0,-0xe(%bp)
L_0676:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_06b2
	cmpw   $0x9,-0xc(%bp)
	jg   L_06b2
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_0676
L_069c:
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
L_06b2:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_06f9
	cmpb   $0x2a,(%di)
	je   L_06fb
	movw   $0x0,-0x10(%bp)
L_06cb:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_06f5
	cmpw   $0x9,-0xc(%bp)
	jg   L_06f5
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_06cb
L_06f5:
	addw   $0x30,-0xc(%bp)
L_06f9:
	jmp   L_0712
L_06fb:
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
L_0712:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_0b1c
L_071d:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_072e
L_0725:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_075e
	jmp   L_0757
L_072e:
	cmp    $0x64,%ax
	je   L_0760
	cmp    $0x6f,%ax
	je   L_0744
	cmp    $0x75,%ax
	je   L_0757
	cmp    $0x78,%ax
	je   L_0752
	jmp   L_0725
L_0744:
	movw   $0x8,-0x12(%bp)
	jmp   L_0757
L_074b:
	movw   $0x8,-0x12(%bp)
	jmp   L_075e
L_0752:
	movw   $0x10,-0x12(%bp)
L_0757:
	jmp   L_07a9
L_0759:
	movw   $0x10,-0x12(%bp)
L_075e:
	jmp   L_07b9
L_0760:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_077d
L_0770:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_077d:
	cmpw   $0x0,-0x14(%bp)
	jg   L_07c7
	jl   L_078b
	cmpw   $0x0,-0x16(%bp)
	jae   L_07c7
L_078b:
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
	jmp   L_07c7
L_07a9:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_07c7
L_07b9:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_07c7:
	cmpw   $0x0,-0x14(%bp)
	jne   L_07d1
	cmpw   $0x0,-0x16(%bp)
L_07d1:
	jne   L_07e2
	cmpw   $0x0,-0x10(%bp)
	jne   L_07e2
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_07e2:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_07e9:
	cmpw   $0x0,-0x14(%bp)
	jne   L_07f3
	cmpw   $0x0,-0x16(%bp)
L_07f3:
	je   L_083e
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_0d06
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_0814
	mov    $0x30,%ax
	jmp   L_0817
L_0814:
	mov    $0x57,%ax
L_0817:
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
	call   L_0bfe
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_07e9
L_083e:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_088d
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_083e
L_085c:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_0868:
	cmpw   $0x2,-0xc(%bp)
	jge   L_088d
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0888
	decw   -0x8(%bp)
L_0888:
	incw   -0xc(%bp)
	jmp   L_0868
L_088d:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0894:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_08ab
	movw   $0x0,-0xc(%bp)
L_08ab:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_08ba
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_08ba:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_1278
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_08dd:
	jmp   L_061b
L_08e0:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_08f2
	/* TODO: data ref: movw   $0x1bce,-0xa(%bp) */
	movw   $D_0324,-0xa(%bp)
L_08f2:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_0901
	movw   $0x7fff,-0xc(%bp)
L_0901:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0907:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0894
	decw   -0xc(%bp)
	jl   L_0894
	incw   -0x8(%bp)
	jmp   L_0907
L_0919:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_0921:
	jmp   L_08dd
L_0923:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_092f
	movw   $0x6,-0x10(%bp)
L_092f:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_0f04
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0964
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0964:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_0979
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_0991
L_0979:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_0979
L_0991:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_09a4
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_09a4:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_09c3
L_09b0:
	decw   -0xc(%bp)
	jl   L_09c3
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_09b0
L_09c3:
	cmpw   $0x0,-0xc(%bp)
	jg   L_09cc
	jmp   L_088d
L_09cc:
	decw   -0xc(%bp)
	jge   L_09d4
	jmp   L_088d
L_09d4:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_09cc
L_09e9:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_09f7
	movw   $0x6,-0x10(%bp)
	jmp   L_09fa
L_09f7:
	incw   -0x10(%bp)
L_09fa:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_0ed8
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0a2f
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0a2f:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_0a3a
	incw   -0x1a(%bp)
L_0a3a:
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
L_0a5c:
	decw   -0xc(%bp)
	jle   L_0a76
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_0a5c
L_0a76:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_0a93
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_0aa4
L_0a93:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_0aa4:
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
	jmp   L_088d
L_0ad5:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0ae1
	movw   $0x6,-0x10(%bp)
L_0ae1:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_0d5c
	add    $0xc,%sp
	add    $0x8,%si
L_0aff:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_0aff
	decw   -0x8(%bp)
	jmp   L_088d
L_0b10:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_0b19:
	jmp   L_0921
L_0b1c:
	cmp    $0x65,%ax
	jne   L_0b24
	jmp   L_09e9
L_0b24:
	jg   L_0b65
	cmp    $0x4f,%ax
	jne   L_0b2e
	jmp   L_0744
L_0b2e:
	jg   L_0b45
	cmp    $0x25,%ax
	je   L_0b10
	cmp    $0x44,%ax
	jne   L_0b3d
	jmp   L_0760
L_0b3d:
	cmp    $0x4c,%ax
L_0b40:
	jne   L_0b45
	jmp   L_071d
L_0b45:
	cmp    $0x55,%ax
	jne   L_0b4d
	jmp   L_07a9
L_0b4d:
	cmp    $0x58,%ax
	jne   L_0b55
	jmp   L_0752
L_0b55:
	cmp    $0x63,%ax
	jne   L_0b5d
	jmp   L_085c
L_0b5d:
	cmp    $0x64,%ax
	jne   L_0b65
	jmp   L_0770
L_0b65:
	cmp    $0x6f,%ax
	jne   L_0b6d
	jmp   L_074b
L_0b6d:
	jg   L_0b84
	cmp    $0x66,%ax
	jne   L_0b77
	jmp   L_0923
L_0b77:
	cmp    $0x67,%ax
	jne   L_0b7f
	jmp   L_0ad5
L_0b7f:
	cmp    $0x6c,%ax
	je   L_0b40
L_0b84:
	cmp    $0x72,%ax
	jne   L_0b8c
	jmp   L_0919
L_0b8c:
	cmp    $0x73,%ax
	jne   L_0b94
	jmp   L_08e0
L_0b94:
	cmp    $0x75,%ax
	jne   L_0b9c
	jmp   L_07b9
L_0b9c:
	cmp    $0x78,%ax
	jne   L_0ba4
	jmp   L_0759
L_0ba4:
	jmp   L_0b19
L_0ba7:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0bcb
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1278
	add    $0x8,%sp
L_0bcb:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_0d3a


L_0bd5:
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
	call   L_0d3a


L_0bfe:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_0c1c
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_0c1c:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_0c25:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_0c25
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_0c35:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_0c35
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0bd5
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_0c76
	jne   L_0c6b
	or     $0x0,%ax
	je   L_0c76
L_0c6b:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_0c76:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_0d3a


L_0c7e:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0c9a
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0c9a:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0cb1
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0cb1:
	push   %dx
	push   %ax
	call   L_0bfe
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_0ccb
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_0ccb:
	ret
	call   L_0d3a
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0c7e
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0bd5
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_0d3a


L_0d06:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0bfe
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0bd5
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_0d3a:
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


L_0d5c:
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
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_0ed8
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_0d96
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_0d96:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_0d9d:
	cmpw   $0x0,-0xa(%bp)
	jle   L_0db7
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_0db7
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_0d9d
L_0db7:
	cmpw   $0x0,-0x8(%bp)
	jl   L_0dc8
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_0dd4
L_0dc8:
	cmpw   $0x0,-0x8(%bp)
	jge   L_0dd2
	cmpw   $0xfffd,-0x8(%bp)
L_0dd2:
	jge   L_0e53
L_0dd4:
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
L_0def:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_0e09
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_0def
L_0e09:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_0e25
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_0e2b
L_0e25:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_0e2b:
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
	jmp   L_0ec3
L_0e53:
	cmpw   $0x0,-0x8(%bp)
	jg   L_0e75
	cmpb   $0x30,(%di)
	je   L_0e64
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0e64:
	cmpw   $0x0,-0x8(%bp)
	jge   L_0e75
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_0e64
L_0e75:
	movw   $0x1,-0xa(%bp)
L_0e7a:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_0ea2
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
	jne   L_0e9d
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0e9d:
	incw   -0xa(%bp)
	jmp   L_0e7a
L_0ea2:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_0ec3
L_0eaa:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_0ebd
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_0eaa
L_0ebd:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0ec3:
	cmpb   $0x2e,-0x1(%si)
	jne   L_0eca
	dec    %si
L_0eca:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0ed8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x1,%ax
	push   %ax
	pushw  0x10(%bp)
	pushw  0xe(%bp)
	pushw  0xc(%bp)
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_0f2f
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f04:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %ax,%ax
	push   %ax
	pushw  0x10(%bp)
	pushw  0xe(%bp)
	pushw  0xc(%bp)
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_0f2f
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f2f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_0f42
	movw   $0x0,0xc(%bp)
L_0f42:
	cmpw   $0x4f,0xc(%bp)
	jl   L_0f4d
	movw   $0x4e,0xc(%bp)
L_0f4d:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	/* TODO: data ref: mov    $0x1bd6,%si */
	mov    $D_032c,%si
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x1c26 */
	fcompl D_037c
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jae   L_0f87
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	int    $0xf4
	fldl   0x4(%bp)
	fchs
	fstl   -0x46(%bp)
	fstpl  0x4(%bp)
L_0f87:
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_187a
	int    $0xf4
	fstpl  -0x46(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x46(%bp)
	fstpl  0x4(%bp)
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x1c2e */
	fcompl D_0384
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jne   L_0fc8
	jmp   L_109e
L_0fc8:
	/* TODO: data ref: mov    $0x1bd6,%si */
	mov    $D_032c,%si
L_0fcb:
	int    $0xf4
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x1c36 */
	fcompl D_038c
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jb   L_1057
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   -0xc(%bp)
	/* TODO: data ref: fmull  0x1c3e */
	fmull  D_0394
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_187a
	int    $0xf4
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x4e(%bp)
	/* TODO: data ref: fmull  0x1c46 */
	fmull  D_039c
	/* TODO: data ref: faddl  0x1c4e */
	faddl  D_03a4
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
	cmp    $0x2710,%ax
	jne   L_0fcb
	int    $0xf4
	fldl   -0xc(%bp)
	/* TODO: data ref: faddl  0x1c56 */
	faddl  D_03ac
	fstpl  -0xc(%bp)
	lea    -0x3e(%bp),%ax
	lea    -0x1(%di),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	jmp   L_0fcb
L_1057:
	mov    $0x1,%ax
	push   %ax
	push   %si
	int    $0xf4
	fldl   -0xc(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   L_1202
	add    $0x6,%sp
	mov    %ax,%si
L_1072:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_1094
	xor    %ax,%ax
	push   %ax
	push   %si
	lea    -0x3e(%bp),%ax
	mov    %di,%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_1202
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_1072
L_1094:
	mov    %si,%ax
	/* TODO: data ref: sub    $0x1bd6,%ax */
	sub    $D_032c,%ax
	mov    %ax,%di
	jmp   L_111e
L_109e:
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x1c5e */
	fcompl D_03b4
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jbe   L_111e
L_10b4:
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x1c66 */
	fmull  D_03bc
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x1c6e */
	fcompl D_03c4
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_10ea
	int    $0xf4
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	sub    $0x3,%di
	jmp   L_10b4
L_10ea:
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x1c76 */
	fmull  D_03cc
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x1c7e */
	fcompl D_03d4
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_111e
	int    $0xf4
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_10ea
L_111e:
	mov    0xc(%bp),%ax
	/* TODO: data ref: add    $0x1bd6,%ax */
	add    $D_032c,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_1130
	add    %di,-0x16(%bp)
L_1130:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	/* TODO: data ref: cmpw   $0x1bd6,-0x16(%bp) */
	cmpw   $D_032c,-0x16(%bp)
	jae   L_1146
	/* TODO: data ref: movb   $0x0,0x1bd6 */
	movb   $0x0,D_032c
	/* TODO: data ref: mov    $0x1bd6,%ax */
	mov    $D_032c,%ax
	jmp   L_11af
L_1146:
	cmp    -0x16(%bp),%si
	ja   L_11a0
	/* TODO: data ref: cmp    $0x1c26,%si */
	cmp    $D_037c,%si
	jae   L_11a0
	lea    -0x14(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x1c86 */
	fmull  D_03dc
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_187a
	int    $0xf4
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x4e(%bp)
	fstpl  0x4(%bp)
	xor    %ax,%ax
	push   %ax
	push   %si
	int    $0xf4
	fldl   -0x14(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   L_1202
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_1146
L_11a0:
	/* TODO: data ref: cmpw   $0x1c26,-0x16(%bp) */
	cmpw   $D_037c,-0x16(%bp)
	jb   L_11b1
	/* TODO: data ref: movb   $0x0,0x1c25 */
	movb   $0x0,D_037b
	/* TODO: data ref: mov    $0x1bd6,%ax */
	mov    $D_032c,%ax
L_11af:
	jmp   L_11fb
L_11b1:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_11b9:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_11f5
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	/* TODO: data ref: cmpw   $0x1bd6,-0x16(%bp) */
	cmpw   $D_032c,-0x16(%bp)
	jbe   L_11d8
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_11b9
L_11d8:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_11b9
	/* TODO: data ref: cmp    $0x1bd6,%si */
	cmp    $D_032c,%si
	jbe   L_11f2
	movb   $0x30,(%si)
L_11f2:
	inc    %si
	jmp   L_11b9
L_11f5:
	movb   $0x0,(%si)
	/* TODO: data ref: mov    $0x1bd6,%ax */
	mov    $D_032c,%ax
L_11fb:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1202:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%si
	xor    %di,%di
L_120d:
	or     %si,%si
	je   L_121e
	mov    %si,%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	mov    %ax,%si
	inc    %di
	jmp   L_120d
L_121e:
	cmpw   $0x0,0x8(%bp)
	jne   L_1237
	mov    %di,%si
L_1226:
	cmp    $0x4,%si
	jge   L_1237
	mov    0x6(%bp),%bx
	movb   $0x30,(%bx)
	incw   0x6(%bp)
	inc    %si
	jmp   L_1226
L_1237:
	mov    0x6(%bp),%ax
	add    %di,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%si
L_1247:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_1270
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
	jmp   L_1247
L_1270:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1278:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1285:
	cmpw   $0x0,0x8(%bp)
	jl   L_128e
	jmp   L_131c
L_128e:
	cmpb   $0x2d,(%di)
	jne   L_12cb
	cmpw   $0x30,0xc(%bp)
	jne   L_12cb
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_12ba
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
	jmp   L_12ca
L_12ba:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_136a
	add    $0x4,%sp
L_12ca:
	dec    %si
L_12cb:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_12e9
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
	jmp   L_12f5
L_12e9:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_136a
	add    $0x4,%sp
L_12f5:
	incw   0x8(%bp)
	jmp   L_1285
L_12fa:
	dec    %si
	jl   L_1330
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_131e
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
L_131c:
	jmp   L_12fa
L_131e:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_136a
	add    $0x4,%sp
	jmp   L_12fa
L_1330:
	cmpw   $0x0,0x8(%bp)
	je   L_1365
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1354
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
	jmp   L_1360
L_1354:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_136a
	add    $0x4,%sp
L_1360:
	decw   0x8(%bp)
	jmp   L_1330
L_1365:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_136a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_1385
	mov    $0xffff,%ax
	jmp   L_144c
L_1385:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_13b8
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
	call   L_1860
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_143d
L_13b8:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_140a
	/* TODO: data ref: cmp    $0x1c96,%di */
	cmp    $D_03ec,%di
	jne   L_13e3
	/* TODO: data ref: mov    0x1c9d,%al */
	mov    D_03f3,%al
	cbtw
	push   %ax
	call   L_17bc
	pop    %cx
	or     %ax,%ax
	je   L_13d8
	orb    $0x4,0x6(%di)
	jmp   L_1385
L_13d8:
	/* TODO: data ref: movw   $0x1d40,0x4(%di) */
	movw   $D_0496,0x4(%di)
	/* TODO: data ref: movw   $0x1d40,(%di) */
	movw   $D_0496,(%di)
	jmp   L_1385
L_13e3:
	mov    $0x200,%ax
	push   %ax
	call   L_153e
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_13fa
	orb    $0x4,0x6(%di)
	jmp   L_1385
L_13fa:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_142c
L_140a:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_142c
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1860
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_142c:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_143d:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_144e
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_144c:
	jmp   L_1451
L_144e:
	mov    0x4(%bp),%ax
L_1451:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1458:
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
	jne   L_14a4
	mov    0x4(%di),%si
	or     %si,%si
	je   L_14a4
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_14a4
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1860
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_14a4
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_14a6
L_14a4:
	xor    %ax,%ax
L_14a6:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_14ab:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x1c8e,%di */
	mov    $D_03e4,%di
L_14b3:
	/* TODO: data ref: cmp    0x1d2e,%di */
	cmp    D_0484,%di
	jae   L_14c3
	push   %di
	call   L_14c7
	pop    %cx
	add    $0x8,%di
	jmp   L_14b3
L_14c3:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_14c7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_151a
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_151a
	push   %di
	call   L_1458
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1792
	pop    %cx
	or     %ax,%ax
	jge   L_14fc
	mov    $0xffff,%si
L_14fc:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_150c
	pushw  0x4(%di)
	call   L_1688
	pop    %cx
L_150c:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_151a
	movw   $0x0,0x4(%di)
L_151a:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


	.global	_exit
_exit:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_14ab
	pushw  0x4(%bp)
	call   L_1896
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_153e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: cmpw   $0x0,0x1d30 */
	cmpw   $0x0,D_0486
	jne   L_1569
	/* TODO: data ref: mov    $0x1d32,%ax */
	mov    $D_0488,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x1d30 */
	mov    %ax,D_0486
	/* TODO: data ref: mov    $0x1d30,%ax */
	mov    $D_0486,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x1d32 */
	mov    %ax,D_0488
	/* TODO: data ref: movw   $0x1d32,0x2142 */
	/* dual-reloc split: movw $D_0488,D_0898 */
	mov	$D_0488,%ax
	mov	%ax,D_0898
	/* TODO: data ref: movw   $0x1d30,0x2140 */
	/* dual-reloc split: movw $D_0486,D_0896 */
	mov	$D_0486,%ax
	mov	%ax,D_0896
L_1569:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    0x2140,%di */
	mov    D_0896,%di
L_157d:
	/* TODO: data ref: movw   $0x0,0x1d34 */
	movw   $0x0,D_048a
L_1583:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_15b2
L_158a:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_1599
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_158a
L_1599:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_15b2
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_15b2
	jmp   L_1659
L_15b2:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_1583
	/* TODO: data ref: cmp    0x2142,%si */
	cmp    D_0898,%si
	jne   L_15cb
	/* TODO: data ref: cmp    $0x1d30,%di */
	cmp    $D_0486,%di
	je   L_15cf
L_15cb:
	xor    %ax,%ax
	jmp   L_1607
L_15cf:
	/* TODO: data ref: incw   0x1d34 */
	incw   D_048a
	/* TODO: data ref: cmpw   $0x1,0x1d34 */
	cmpw   $0x1,D_048a
	jle   L_1583
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	/* TODO: data ref: mov    %ax,0x1d34 */
	mov    %ax,D_048a
	xor    %ax,%ax
	push   %ax
	call   L_176c
	pop    %cx
	mov    %ax,%si
	/* TODO: data ref: mov    0x1d34,%ax */
	mov    D_048a,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_1609
	xor    %ax,%ax
L_1607:
	jmp   L_161c
L_1609:
	/* TODO: data ref: mov    0x1d34,%ax */
	mov    D_048a,%ax
	shl    %ax
	push   %ax
	call   L_176c
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_161e
	xor    %ax,%ax
L_161c:
	jmp   L_1683
L_161e:
	/* TODO: data ref: mov    0x2142,%bx */
	mov    D_0898,%bx
	mov    %si,(%bx)
	/* TODO: data ref: mov    0x2142,%ax */
	mov    D_0898,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_163c
	/* TODO: data ref: mov    0x2142,%bx */
	mov    D_0898,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x2142,%bx */
	mov    D_0898,%bx
	mov    %ax,(%bx)
L_163c:
	/* TODO: data ref: mov    0x1d34,%ax */
	mov    D_048a,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	/* TODO: data ref: mov    %ax,0x2142 */
	mov    %ax,D_0898
	/* TODO: data ref: mov    $0x1d30,%ax */
	mov    $D_0486,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x2142,%bx */
	mov    D_0898,%bx
	mov    %ax,(%bx)
	jmp   L_157d
L_1659:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	/* TODO: data ref: mov    %ax,0x2140 */
	mov    %ax,D_0896
	cmp    %ax,%si
	jbe   L_1678
	/* TODO: data ref: mov    0x2140,%bx */
	mov    D_0896,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x2144 */
	mov    %ax,D_089a
	/* TODO: data ref: mov    0x2140,%bx */
	mov    D_0896,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_1678:
	/* TODO: data ref: mov    0x2140,%ax */
	mov    D_0896,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_1683:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1688:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	/* TODO: data ref: mov    %si,0x2140 */
	mov    %si,D_0896
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
	je   L_16bd
	push   %di
	call   L_1688
	pop    %cx
L_16bd:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_153e
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_16dc
	cmp    %di,%si
	jne   L_16e0
L_16dc:
	mov    %si,%ax
	jmp   L_1748
L_16e0:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_16ff
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_16ff:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_171d
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_16ff
L_171d:
	cmp    %di,%si
	jae   L_1746
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_1746
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
	/* TODO: data ref: mov    0x2144,%ax */
	mov    D_089a,%ax
	mov    %ax,(%bx)
L_1746:
	mov    %si,%ax
L_1748:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */
	push   %bp
	mov    %sp,%bp
	mov	$17,%bx	/* sc: brk */
	mov    0x4(%bp),%ax
	/* v7: 17:brk */
	int	$0xf1
	jcxz   L_1763
	/* TODO: data ref: mov    %cx,0x1d3a */
	mov    %cx,D_0490
	jmp   L_176a
L_1763:
	mov    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x1d36 */
	mov    %bx,D_048c
L_176a:
	pop    %bp
	ret


L_176c:
	push   %bp
	mov    %sp,%bp
	mov	$17,%bx	/* sc: brk */
	mov    0x4(%bp),%ax
	/* TODO: data ref: add    0x1d36,%ax */
	add    D_048c,%ax
	/* v7: 17:brk */
	int	$0xf1
	jcxz   L_1783
	/* TODO: data ref: mov    %cx,0x1d3a */
	mov    %cx,D_0490
	jmp   L_1790
L_1783:
	/* TODO: data ref: mov    0x1d36,%bx */
	mov    D_048c,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x1d36 */
	mov    %bx,D_048c
L_1790:
	pop    %bp
	ret


L_1792:
	push   %bp
	mov    %sp,%bp
	mov	$6,%bx	/* sc: close */
	mov    0x4(%bp),%ax
	/* v7: 6:close */
	int	$0xf1
	jcxz   L_17a3
	/* TODO: data ref: mov    %cx,0x1d3a */
	mov    %cx,D_0490
L_17a3:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_17a6:
	push   %bp
	mov    %sp,%bp
	mov	$28,%bx	/* sc: fstat */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 28:fstat */
	int	$0xf1
	jcxz   L_17ba
	/* TODO: data ref: mov    %cx,0x1d3a */
	mov    %cx,D_0490
L_17ba:
	pop    %bp
	ret


L_17bc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_182d
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_17d9
	xor    %ax,%ax
	jmp   L_17dc
L_17d9:
	mov    $0x1,%ax
L_17dc:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_17e4:
	push   %bp
	mov    %sp,%bp
	mov	$5,%bx	/* sc: open */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 5:open */
	int	$0xf1
	jcxz   L_17f8
	/* TODO: data ref: mov    %cx,0x1d3a */
	mov    %cx,D_0490
L_17f8:
	pop    %bp
	ret


L_17fa:
	push   %bp
	mov    %sp,%bp
	mov	$3,%bx	/* sc: read */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 3:read */
	int	$0xf1
	jcxz   L_1811
	/* TODO: data ref: mov    %cx,0x1d3a */
	mov    %cx,D_0490
L_1811:
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
	call   L_1846
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_182d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1846
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1846:
	push   %bp
	mov    %sp,%bp
	mov	$54,%bx	/* sc: ioctl */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 54:ioctl */
	int	$0xf1
	jcxz   L_185d
	/* TODO: data ref: mov    %cx,0x1d3a */
	mov    %cx,D_0490
L_185d:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1860:
	push   %bp
	mov    %sp,%bp
	mov	$4,%bx	/* sc: write */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 4:write */
	int	$0xf1
	jcxz   L_1877
	/* TODO: data ref: mov    %cx,0x1d3a */
	mov    %cx,D_0490
L_1877:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_187a:
	push   %bp
	mov    %sp,%bp
	mov    0xc(%bp),%bx
	int    $0xf4
	fldl   0x4(%bp)
	frndint
	fstl   (%bx)
	fsubl  0x4(%bp)
	fchs
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1896:
	push   %bp
	mov    %sp,%bp
	mov	$1,%bx	/* sc: exit */
	mov    0x4(%bp),%ax
	/* v7: 1:exit */
	int	$0xf1
	jcxz   L_18a7
	/* TODO: data ref: mov    %cx,0x1d3a */
	mov    %cx,D_0490
L_18a7:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


	.data

	.byte	0x00, 0x00	/* |..| */
D_0002:
	.byte	0xbf, 0x0f, 0x40, 0x28, 0x23, 0x29, 0x75, 0x75	/* |..@(#)uu| */
	.byte	0x65, 0x6e, 0x63, 0x6f, 0x64, 0x65, 0x2e, 0x63	/* |encode.c| */
	.byte	0x09	/* |.| */
	.ascii	"5.1 (Berkeley) 7/2/83\0"
	.byte	0x00	/* |.| */
D_002a:
	.byte	0x72, 0x00	/* |r.| */
D_002c:
	.byte	0x55, 0x73, 0x61, 0x67, 0x65, 0x3a, 0x20, 0x75	/* |Usage: u| */
	.byte	0x75, 0x65, 0x6e, 0x63, 0x6f, 0x64, 0x65, 0x20	/* |uencode | */
	.byte	0x5b, 0x69, 0x6e, 0x66, 0x69, 0x6c, 0x65, 0x5d	/* |[infile]| */
	.byte	0x20, 0x72, 0x65, 0x6d, 0x6f, 0x74, 0x65, 0x66	/* | remotef| */
	.byte	0x69, 0x6c, 0x65, 0x0a, 0x00	/* |ile..| */
D_0051:
	.byte	0x62, 0x65, 0x67, 0x69, 0x6e, 0x20, 0x25, 0x6f	/* |begin %o| */
	.byte	0x20, 0x25, 0x73, 0x0a, 0x00	/* | %s..| */
D_005e:
	.byte	0x65, 0x6e, 0x64, 0x0a, 0x00, 0x00	/* |end...| */
D_0064:
	.byte	0x00, 0x00	/* |..| */
D_0066:
	.ascii	"Unknown error\0"
D_0074:
	.word	D_0790	/* ptr: 0x203a */
	.byte	0x00	/* |.| */
D_0077:
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_007a:
	.word	D_00c2	/* ptr: 0x196c */
	.word	D_00ca	/* ptr: 0x1974 */
	.word	D_00d4	/* ptr: 0x197e */
	.word	D_00ee	/* ptr: 0x1998 */
	.word	D_00fe	/* ptr: 0x19a8 */
	.word	D_0116	/* ptr: 0x19c0 */
	.word	D_0120	/* ptr: 0x19ca */
	.word	D_013a	/* ptr: 0x19e4 */
	.word	D_014c	/* ptr: 0x19f6 */
	.word	D_015e	/* ptr: 0x1a08 */
	.word	D_016e	/* ptr: 0x1a18 */
	.word	D_017a	/* ptr: 0x1a24 */
	.word	D_018c	/* ptr: 0x1a36 */
	.word	D_019c	/* ptr: 0x1a46 */
	.word	D_01ae	/* ptr: 0x1a58 */
	.word	D_01ba	/* ptr: 0x1a64 */
	.word	D_01d0	/* ptr: 0x1a7a */
	.word	D_01e2	/* ptr: 0x1a8c */
	.word	D_01ee	/* ptr: 0x1a98 */
	.word	D_0200	/* ptr: 0x1aaa */
	.word	D_020f	/* ptr: 0x1ab9 */
	.word	D_021f	/* ptr: 0x1ac9 */
	.word	D_022e	/* ptr: 0x1ad8 */
	.word	D_023f	/* ptr: 0x1ae9 */
	.word	D_0253	/* ptr: 0x1afd */
	.word	D_0267	/* ptr: 0x1b11 */
	.word	D_0278	/* ptr: 0x1b22 */
	.word	D_0287	/* ptr: 0x1b31 */
	.word	D_0296	/* ptr: 0x1b40 */
	.word	D_02ae	/* ptr: 0x1b58 */
	.word	D_02bb	/* ptr: 0x1b65 */
	.word	D_02d1	/* ptr: 0x1b7b */
	.word	D_02e0	/* ptr: 0x1b8a */
	.word	D_02ec	/* ptr: 0x1b96 */
	.word	D_02ff	/* ptr: 0x1ba9 */
D_00c0:
	.byte	0x23, 0x00	/* |#.| */
D_00c2:
	.ascii	"Error 0\0"
D_00ca:
	.ascii	"Not owner\0"
D_00d4:
	.ascii	"No such file or directory\0"
D_00ee:
	.ascii	"No such process\0"
D_00fe:
	.ascii	"Interrupted system call\0"
D_0116:
	.ascii	"I/O error\0"
D_0120:
	.ascii	"No such device or address\0"
D_013a:
	.ascii	"Arg list too long\0"
D_014c:
	.ascii	"Exec format error\0"
D_015e:
	.ascii	"Bad file number\0"
D_016e:
	.ascii	"No children\0"
D_017a:
	.ascii	"No more processes\0"
D_018c:
	.ascii	"Not enough core\0"
D_019c:
	.ascii	"Permission denied\0"
D_01ae:
	.ascii	"Bad address\0"
D_01ba:
	.ascii	"Block device required\0"
D_01d0:
	.ascii	"Mount device busy\0"
D_01e2:
	.ascii	"File exists\0"
D_01ee:
	.ascii	"Cross-device link\0"
D_0200:
	.ascii	"No such device\0"
D_020f:
	.ascii	"Not a directory\0"
D_021f:
	.ascii	"Is a directory\0"
D_022e:
	.ascii	"Invalid argument\0"
D_023f:
	.ascii	"File table overflow\0"
D_0253:
	.ascii	"Too many open files\0"
D_0267:
	.ascii	"Not a typewriter\0"
D_0278:
	.ascii	"Text file busy\0"
D_0287:
	.ascii	"File too large\0"
D_0296:
	.ascii	"No space left on device\0"
D_02ae:
	.ascii	"Illegal seek\0"
D_02bb:
	.ascii	"Read-only file system\0"
D_02d1:
	.ascii	"Too many links\0"
D_02e0:
	.ascii	"Broken pipe\0"
D_02ec:
	.ascii	"Argument too large\0"
D_02ff:
	.ascii	"Result too large\0"
D_0310:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_0324:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_032c:
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
D_037b:
	.byte	0x00	/* |.| */
D_037c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0384:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_038c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_0394:
	.byte	0x2c, 0x43, 0x1c, 0xeb, 0xe2, 0x36, 0x1a, 0x3f	/* |,C...6.?| */
D_039c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_03a4:
	.byte	0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0xb9, 0x3f	/* |.......?| */
D_03ac:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_03b4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_03bc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x8f, 0x40	/* |.....@.@| */
D_03c4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_03cc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_03d4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_03dc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_03e4:
	.word	D_0696	/* ptr: 0x1f40 */
	.byte	0x00, 0x00	/* |..| */
	.word	D_0696	/* ptr: 0x1f40 */
	.byte	0x01, 0x00	/* |..| */
D_03ec:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_03f2:
	.byte	0x02	/* |.| */
D_03f3:
	.byte	0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06	/* |........| */
	.byte	0x02	/* |.| */
D_03fc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0484:
	.word	D_0484	/* ptr: 0x1d2e */
D_0486:
	.byte	0x00, 0x00	/* |..| */
D_0488:
	.byte	0x00, 0x00	/* |..| */
D_048a:
	.byte	0x00, 0x00	/* |..| */
D_048c:
	.word	0x08a0	/* _curbrk: bss_end=0x089c rounded to paragraph */

	.bss

D_048e:	.space	2	/* bss word @ 0x048e */
D_0490:	.space	2	/* bss word @ 0x0490 */
	.space	4
D_0496:	.space	2	/* bss word @ 0x0496 */
	.space	510
D_0696:	.space	2	/* bss word @ 0x0696 */
	.space	248
D_0790:	.space	2	/* bss word @ 0x0790 */
	.space	260
D_0896:	.space	2	/* bss word @ 0x0896 */
D_0898:	.space	2	/* bss word @ 0x0898 */
D_089a:	.space	2	/* bss word @ 0x089a */

/* end of file */
