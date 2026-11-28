/* ld.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 ld.s -o ld.o
 * Syscalls used:
 *   1:exit            3:read            4:write           5:open            6:close           8:creat
 *   9:link            10:unlink         15:chmod          17:brk            19:lseek          28:fstat
 *   48:signal         54:ioctl          60:umask          512:?512          1024:?1024
 * INT instructions: $0xf1  $0xf4
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* Wrapping near-call/jmp targets (16-bit address space) */
	.set L_0400, 0x0400	/* wrapping branch target */
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: ld  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00005954  (22868 bytes)
//   a_data:   0x00000e84  (3716 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

L_0036:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0f04,%ax
	push   %ax
	mov    $0x3c,%ax
	push   %ax
	mov    $D_0ea4,%ax
	push   %ax
	call   L_0066
	add    $0x6,%sp
	mov    $D_0ea4,%di
L_0050:
	cmp    $D_0eb4,%di
	jae   L_0062
	mov    %di,%bx
	inc    %di
	cmpb   $0x20,(%bx)
	jne   L_0050
	movb   $0x0,-0x1(%di)
L_0062:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0066:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%si
	mov    0x8(%bp),%di
L_0076:
	cmpw   $0x0,0x6(%bp)
	jne   L_007f
	jmp   L_020b
L_007f:
	cmpw   $0x0,0x8(%di)
	jne   L_0089
	cmpw   $0x0,0x6(%di)
L_0089:
	je   L_008f
	cmpw   $0x0,0x4(%di)
L_008f:
	jne   L_0094
	jmp   L_0119
L_0094:
	mov    (%di),%ax
	mov    %ax,-0x6(%bp)
	mov    0x6(%di),%ax
	mov    %ax,-0x8(%bp)
	cmp    0x4(%di),%ax
	jle   L_00aa
	mov    0x4(%di),%ax
	mov    %ax,-0x8(%bp)
L_00aa:
	mov    -0x8(%bp),%ax
	cmp    0x6(%bp),%ax
	jbe   L_00b8
	mov    0x6(%bp),%ax
	mov    %ax,-0x8(%bp)
L_00b8:
	mov    -0x8(%bp),%ax
	sub    %ax,0x6(%bp)
	mov    -0x8(%bp),%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    0x6(%di),%ax
	mov    0x8(%di),%dx
	sub    -0xc(%bp),%ax
	sbb    -0xa(%bp),%dx
	mov    %ax,0x6(%di)
	mov    %dx,0x8(%di)
	mov    -0x8(%bp),%ax
	sub    %ax,0x4(%di)
	mov    -0x8(%bp),%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    0xa(%di),%ax
	mov    0xc(%di),%dx
	add    -0xc(%bp),%ax
	adc    -0xa(%bp),%dx
	mov    %ax,0xa(%di)
	mov    %dx,0xc(%di)
L_00fc:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	decw   -0x8(%bp)
	jg   L_00fc
	mov    -0x6(%bp),%ax
	mov    %ax,(%di)
L_0116:
	jmp   L_0076
L_0119:
	cmpw   $L_0400,0x6(%bp)
	ja   L_0123
	jmp   L_01fa
L_0123:
	mov    0x6(%bp),%ax
	xor    %dx,%dx
	mov    $0x400,%cx
	div    %cx
	mov    0x6(%bp),%ax
	sub    %dx,%ax
	mov    %ax,-0x8(%bp)
	xor    %ax,%ax
	push   %ax
	mov    0x2(%di),%ax
	inc    %ax
	cwtd
	mov    $0xa,%cx
	or     %cx,%cx
	je   L_014a
L_0144:
	shl    %ax
	rcl    %dx
	loop   L_0144
L_014a:
	push   %dx
	push   %ax
	pushw  D_0ea0
	call   lseek
	add    $0x8,%sp
	mov    -0x8(%bp),%ax
	cwtd
	cmp    0x8(%di),%dx
	jg   L_0179
	jl   L_0166
	cmp    0x6(%di),%ax
	ja   L_0179
L_0166:
	pushw  -0x8(%bp)
	push   %si
	pushw  D_0ea0
	call   read
	add    $0x6,%sp
	cmp    -0x8(%bp),%ax
	je   L_0187
L_0179:
	/* TODO: data ref: mov    $0x2010,%ax */
	mov    $D_0010,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_0187:
	add    -0x8(%bp),%si
	mov    -0x8(%bp),%ax
	sub    %ax,0x6(%bp)
	mov    -0x8(%bp),%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    0x6(%di),%ax
	mov    0x8(%di),%dx
	sub    -0xc(%bp),%ax
	sbb    -0xa(%bp),%dx
	mov    %ax,0x6(%di)
	mov    %dx,0x8(%di)
	mov    -0x8(%bp),%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    0xa(%di),%ax
	mov    0xc(%di),%dx
	add    -0xc(%bp),%ax
	adc    -0xa(%bp),%dx
	mov    %ax,0xa(%di)
	mov    %dx,0xc(%di)
	mov    $0xffff,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    -0x8(%bp),%ax
	cwtd
	mov    $0x400,%cx
	idiv   %cx
	mov    0x2(%di),%dx
	inc    %dx
	add    %ax,%dx
	mov    %dx,%ax
	cwtd
	mov    $0xa,%cx
	or     %cx,%cx
	je   L_01ee
L_01e8:
	shl    %ax
	rcl    %dx
	loop   L_01e8
L_01ee:
	push   %dx
	push   %ax
	push   %di
	call   L_0278
	add    $0xa,%sp
L_01f7:
	jmp   L_0116
L_01fa:
	push   %di
	call   L_04ff
	pop    %cx
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	decw   0x6(%bp)
	jmp   L_01f7
L_020b:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0212:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%bx
	mov    (%bx),%si
	or     %si,%si
	je   L_025c
	mov    0x4(%bp),%bx
	/* TODO: data ref: mov    0x2004,%ax */
	mov    D_0004,%ax
	mov    %ax,(%bx)
	push   %si
	call   L_44de
	pop    %cx
	inc    %ax
	mov    %ax,%di
	pushw  D_0efc
	push   %di
	push   %si
	call   L_0800
	add    $0x6,%sp
	mov    %di,%ax
	cwtd
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	/* TODO: data ref: mov    0x2004,%ax */
	mov    D_0004,%ax
	/* TODO: data ref: mov    0x2006,%dx */
	mov    D_0006,%dx
	add    -0x8(%bp),%ax
	adc    -0x6(%bp),%dx
	/* TODO: data ref: mov    %ax,0x2004 */
	mov    %ax,D_0004
	/* TODO: data ref: mov    %dx,0x2006 */
	mov    %dx,D_0006
L_025c:
	pushw  0x6(%bp)
	mov    $0xa,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0800
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    %si,(%bx)
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0278:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    $0xa,%cx
	or     %cx,%cx
	je   L_0297
L_0291:
	sar    %dx
	rcr    %ax
	loop   L_0291
L_0297:
	mov    %ax,-0x6(%bp)
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x3ff,%ax
	and    $0x0,%dx
	mov    %ax,-0x8(%bp)
	testw  $0x1,-0x8(%bp)
	je   L_02bf
	/* TODO: data ref: mov    $0x201e,%ax */
	mov    $D_001e,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_02bf:
	mov    0xe(%di),%bx
	decw   0x6(%bx)
	mov    $D_0f14,%si
	mov    %si,%bx
	mov    0x2(%bx),%ax
	cmp    -0x6(%bp),%ax
	je   L_02dd
	mov    $D_1330,%si
	mov    %si,%bx
	mov    0x2(%bx),%ax
	cmp    -0x6(%bp),%ax
L_02dd:
	je   L_035e
	cmpw   $0x0,0x6(%si)
	je   L_02f0
	mov    $D_0f14,%si
	mov    %si,%bx
	cmpw   $0x0,0x6(%bx)
	jne   L_0360
L_02f0:
	cmpw   $0x0,D_0f1a
	jne   L_030a
	cmpw   $0x0,D_1336
	jne   L_030a
	mov    D_0f16,%ax
	cmp    D_1332,%ax
	jge   L_030a
	mov    $D_0f14,%si
L_030a:
	mov    -0x6(%bp),%ax
	mov    %ax,0x2(%si)
	mov    $0x3ff,%ax
	xor    %dx,%dx
	not    %ax
	not    %dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	xor    %ax,%ax
	push   %ax
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    -0xe(%bp),%ax
	and    -0xc(%bp),%dx
	push   %dx
	push   %ax
	pushw  D_0ea0
	call   lseek
	add    $0x8,%sp
	mov    $0x400,%ax
	push   %ax
	lea    0x1c(%si),%ax
	push   %ax
	pushw  D_0ea0
	call   read
	add    $0x6,%sp
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jge   L_0358
	movw   $0x0,-0xa(%bp)
L_0358:
	mov    -0xa(%bp),%ax
	mov    %ax,0x4(%si)
L_035e:
	jmp   L_036e
L_0360:
	/* TODO: data ref: mov    $0x2037,%ax */
	mov    $D_0037,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_036e:
	incw   0x6(%si)
	mov    -0x6(%bp),%ax
	mov    %ax,0x2(%di)
	mov    %si,0xe(%di)
	cmpw   $0xffff,0xc(%bp)
	jne   L_0386
	cmpw   $0xffff,0xa(%bp)
	je   L_039c
L_0386:
	mov    0xa(%bp),%ax
	mov    0xc(%bp),%dx
	mov    %ax,0x6(%di)
	mov    %dx,0x8(%di)
	movw   $0x0,0xa(%di)
	movw   $0x0,0xc(%di)
L_039c:
	lea    0x1c(%si),%ax
	add    -0x8(%bp),%ax
	mov    %ax,(%di)
	mov    0x4(%si),%ax
	sub    -0x8(%bp),%ax
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jg   L_03bb
	movw   $0x0,0x6(%di)
	movw   $0x0,0x8(%di)
L_03bb:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_03c2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    $0xa,%cx
	or     %cx,%cx
	je   L_03dd
L_03d7:
	sar    %dx
	rcr    %ax
	loop   L_03d7
L_03dd:
	mov    %ax,-0x8(%bp)
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x3ff,%ax
	and    $0x0,%dx
	mov    %ax,-0xa(%bp)
	testw  $0x1,-0xa(%bp)
	je   L_0405
	/* TODO: data ref: mov    $0x2047,%ax */
	mov    $D_0047,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_0405:
	mov    0xe(%bp),%ax
	mov    $0x41c,%dx
	imul   %dx
	add    $D_0f14,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	mov    -0x8(%bp),%ax
	mov    %ax,0x2(%bx)
	xor    %ax,%ax
	push   %ax
	mov    $0x3ff,%ax
	xor    %dx,%dx
	not    %ax
	not    %dx
	mov    0x6(%bp),%bx
	mov    0x8(%bp),%si
	and    %ax,%bx
	and    %dx,%si
	push   %si
	push   %bx
	pushw  D_0ea0
	call   lseek
	add    $0x8,%sp
	mov    $0x400,%ax
	push   %ax
	mov    -0x6(%bp),%ax
	add    $0x1c,%ax
	push   %ax
	pushw  D_0ea0
	call   read
	add    $0x6,%sp
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jge   L_045e
	movw   $0x0,-0xc(%bp)
L_045e:
	mov    -0x6(%bp),%bx
	mov    -0xc(%bp),%ax
	mov    %ax,0x4(%bx)
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	mov    %ax,0x2(%bx)
	mov    0x4(%bp),%bx
	mov    -0x6(%bp),%ax
	mov    %ax,0xe(%bx)
	mov    0x4(%bp),%bx
	mov    0xa(%bp),%ax
	mov    0xc(%bp),%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	mov    0x4(%bp),%bx
	movw   $0x0,0xa(%bx)
	movw   $0x0,0xc(%bx)
	mov    -0x6(%bp),%ax
	add    $0x1c,%ax
	add    -0xa(%bp),%ax
	mov    0x4(%bp),%bx
	mov    %ax,(%bx)
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    -0x6(%bp),%bx
	mov    0x4(%bx),%bx
	sub    -0xa(%bp),%bx
	xor    %si,%si
	or     %bx,%bx
	jge   L_04bd
	not    %si
L_04bd:
	cmp    %dx,%si
	jl   L_04d2
	jg   L_04c7
	cmp    %ax,%bx
	jbe   L_04d2
L_04c7:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	jmp   L_04dc
L_04d2:
	mov    -0x6(%bp),%bx
	mov    0x4(%bx),%ax
	sub    -0xa(%bp),%ax
	cwtd
L_04dc:
	mov    0x4(%bp),%bx
	mov    %ax,0x4(%bx)
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jge   L_04f8
	mov    0x4(%bp),%bx
	movw   $0x0,0x6(%bx)
	movw   $0x0,0x8(%bx)
L_04f8:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_04ff:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	decw   0x4(%di)
	jge   L_0531
	mov    $0xffff,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    0x2(%di),%ax
	inc    %ax
	cwtd
	mov    $0xa,%cx
	or     %cx,%cx
	je   L_0525
L_051f:
	shl    %ax
	rcl    %dx
	loop   L_051f
L_0525:
	push   %dx
	push   %ax
	push   %di
	call   L_0278
	add    $0xa,%sp
	decw   0x4(%di)
L_0531:
	subw   $0x1,0x6(%di)
	sbbw   $0x0,0x8(%di)
	cmpw   $0x0,0x8(%di)
	jg   L_0572
	jl   L_0547
	cmpw   $0x0,0x6(%di)
	ja   L_0572
L_0547:
	cmpw   $0x0,0x8(%di)
	jg   L_0563
	jl   L_0555
	cmpw   $0x0,0x6(%di)
	jae   L_0563
L_0555:
	/* TODO: data ref: mov    $0x205f,%ax */
	mov    $D_005f,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_0563:
	incw   D_0efa
	mov    0xe(%di),%bx
	decw   0x6(%bx)
	movw   $D_0ef4,0xe(%di)
L_0572:
	addw   $0x1,0xa(%di)
	adcw   $0x0,0xc(%di)
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	incw   (%di)
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0585:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2a,%sp
	mov    0x4(%bp),%di
	/* movw constant -> D_0ea0 */
	mov	$0xffff,%ax
	mov	%ax,D_0ea0
	movb   $0x0,D_0ea4
	mov    %di,D_0f00
	cmpb   $0x2d,(%di)
	jne   L_05a8
	cmpb   $0x6c,0x1(%di)
L_05a8:
	je   L_05ad
	jmp   L_065d
L_05ad:
	/* TODO: data ref: movw   $0x206d,-0x2e(%bp) */
	movw   $D_006d,-0x2e(%bp)
	cmpb   $0x0,0x2(%di)
	jne   L_05bb
	/* TODO: data ref: mov    $0x208f,%di */
	mov    $D_008f,%di
L_05bb:
	/* TODO: data ref: movw   $0x2093,0x2f00 */
	/* dual-reloc split: movw $D_0093,D_0f00 */
	mov	$D_0093,%ax
	mov	%ax,D_0f00
	xor    %si,%si
L_05c3:
	mov    %di,%ax
	add    %si,%ax
	mov    %ax,%bx
	cmpb   $0x0,0x2(%bx)
	je   L_05ff
	mov    %di,%ax
	add    %si,%ax
	mov    %ax,%bx
	mov    0x2(%bx),%al
	cbtw
	mov    D_0f00,%dx
	add    %si,%dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,0xc(%bx)
	mov    %di,%ax
	add    %si,%ax
	mov    %ax,%bx
	mov    0x2(%bx),%al
	cbtw
	mov    -0x2e(%bp),%dx
	add    %si,%dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,0x12(%bx)
	inc    %si
	jmp   L_05c3
L_05ff:
	mov    -0x2e(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	movb   $0x2e,0x12(%bx)
	mov    D_0f00,%ax
	add    %si,%ax
	mov    %ax,%bx
	movb   $0x2e,0xc(%bx)
	mov    -0x2e(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	movb   $0x61,0x13(%bx)
	mov    D_0f00,%ax
	add    %si,%ax
	mov    %ax,%bx
	movb   $0x61,0xd(%bx)
	mov    -0x2e(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	movb   $0x0,0x14(%bx)
	mov    D_0f00,%ax
	add    %si,%ax
	mov    %ax,%bx
	movb   $0x0,0xe(%bx)
	xor    %ax,%ax
	push   %ax
	mov    D_0f00,%ax
	add    $0x4,%ax
	push   %ax
	call   open
	add    $0x4,%sp
	mov    %ax,D_0ea0
	or     %ax,%ax
	jl   L_065f
	addw   $0x4,D_0f00
L_065d:
	jmp   L_0679
L_065f:
	xor    %ax,%ax
	push   %ax
	pushw  D_0f00
	call   open
	add    $0x4,%sp
	mov    %ax,D_0ea0
	or     %ax,%ax
	jge   L_0679
	mov    -0x2e(%bp),%ax
	mov    %ax,D_0f00
L_0679:
	cmpw   $0xffff,D_0ea0
	jne   L_06a2
	xor    %ax,%ax
	push   %ax
	pushw  D_0f00
	call   open
	add    $0x4,%sp
	mov    %ax,D_0ea0
	or     %ax,%ax
	jge   L_06a2
	/* TODO: data ref: mov    $0x20af,%ax */
	mov    $D_00af,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_06a2:
	/* movw constant -> D_1332 */
	mov	$0xffff,%ax
	mov	%ax,D_1332
	/* movw constant -> D_0f16 */
	mov	$0xffff,%ax
	mov	%ax,D_0f16
	movw   $0x0,D_1336
	movw   $0x0,D_0f1a
	/* dual-reloc split: movw $D_0ef4,D_0e9a */
	mov	$D_0ef4,%ax
	mov	%ax,D_0e9a
	/* dual-reloc split: movw $D_0ef4,D_0f12 */
	mov	$D_0ef4,%ax
	mov	%ax,D_0f12
	movw   $0x2,D_0efa
	mov    $0x8,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	cmpw   $0x0,D_0f0c
	jg   L_0701
	jl   L_06f3
	cmpw   $0x0,D_0f0a
	ja   L_0701
L_06f3:
	/* TODO: data ref: mov    $0x20bb,%ax */
	mov    $D_00bb,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_0701:
	mov    $D_0f04,%ax
	push   %ax
	mov    $0x8,%ax
	push   %ax
	lea    -0xd(%bp),%ax
	push   %ax
	call   L_0066
	add    $0x6,%sp
	movb   $0x0,-0x5(%bp)
	/* TODO: data ref: mov    $0x20c9,%ax */
	mov    $D_00c9,%ax
	push   %ax
	lea    -0xd(%bp),%ax
	push   %ax
	call   L_57d2
	add    $0x4,%sp
	or     %ax,%ax
	je   L_072d
	xor    %ax,%ax
	jmp   L_0758
L_072d:
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    $0x8,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	cmpw   $0x0,D_0f0c
	jg   L_075a
	jl   L_0755
	cmpw   $0x0,D_0f0a
	ja   L_075a
L_0755:
	mov    $0x1,%ax
L_0758:
	jmp   L_07a2
L_075a:
	call   L_0036
	mov    $0x10,%ax
	push   %ax
	/* TODO: data ref: mov    $0x20d2,%ax */
	mov    $D_00d2,%ax
	push   %ax
	mov    $D_0ea4,%ax
	push   %ax
	call   L_44fa
	add    $0x6,%sp
	or     %ax,%ax
	je   L_0778
	mov    $0x1,%ax
	jmp   L_07a2
L_0778:
	lea    -0x2c(%bp),%ax
	push   %ax
	pushw  D_0ea0
	call   L_5828
	add    $0x4,%sp
	mov    $D_0eb4,%ax
	push   %ax
	call   L_430c
	pop    %cx
	cmp    %dx,-0x14(%bp)
	jl   L_079f
	jg   L_079a
	cmp    %ax,-0x16(%bp)
	jbe   L_079f
L_079a:
	mov    $0x3,%ax
	jmp   L_07a2
L_079f:
	mov    $0x2,%ax
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
	mov    0x4(%bp),%ax
	add    $0x4,%ax
	mov    0x4(%bp),%bx
	mov    %ax,0x2(%bx)
	mov    $0x200,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	call   L_4dcf
	add    $0x8,%sp
	mov	$512,%bx	/* sc: ?512 */
	xor    %si,%si
	sub    %ax,%bx
	sbb    %dx,%si
	mov    0x4(%bp),%si
	mov    %bx,(%si)
	mov    0x4(%bp),%bx
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,0x204(%bx)
	mov    %dx,0x206(%bx)
	mov    0x4(%bp),%bx
	mov    D_0ef0,%ax
	mov    %ax,0x208(%bx)
	mov    0x4(%bp),%ax
	mov    %ax,D_0ef0
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0800:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_0810:
	or     %si,%si
	jne   L_0817
	jmp   L_0932
L_0817:
	mov    0x8(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_0867
	mov    0x8(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x6(%bp)
	cmp    %si,%ax
	jle   L_082e
	mov    %si,-0x6(%bp)
L_082e:
	mov    0x8(%bp),%bx
	mov    -0x6(%bp),%ax
	sub    %ax,(%bx)
	mov    0x8(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x8(%bp)
	mov    0x8(%bp),%bx
	mov    -0x6(%bp),%ax
	add    %ax,0x2(%bx)
	sub    -0x6(%bp),%si
L_084b:
	mov    -0x6(%bp),%ax
	decw   -0x6(%bp)
	or     %ax,%ax
	je   L_0810
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_084b
L_0867:
	cmp    $0x200,%si
	jge   L_0870
	jmp   L_0929
L_0870:
	mov    0x8(%bp),%bx
	mov    0x2(%bx),%ax
	mov    0x8(%bp),%dx
	add    $0x4,%dx
	cmp    %dx,%ax
	je   L_0887
	pushw  0x8(%bp)
	call   L_095c
	pop    %cx
L_0887:
	mov    %si,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    %si,%ax
	sub    %dx,%ax
	mov    %ax,-0x6(%bp)
	mov    0x8(%bp),%bx
	mov    D_0eec,%ax
	mov    D_0eee,%dx
	cmp    0x206(%bx),%dx
	jne   L_08ac
	cmp    0x204(%bx),%ax
	je   L_08c4
L_08ac:
	xor    %ax,%ax
	push   %ax
	mov    0x8(%bp),%bx
	pushw  0x206(%bx)
	pushw  0x204(%bx)
	pushw  D_0ee4
	call   lseek
	add    $0x8,%sp
L_08c4:
	pushw  -0x6(%bp)
	push   %di
	pushw  D_0ee4
	call   L_590a
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_08eb
	movw   $0x1,D_0ee8
	/* TODO: data ref: mov    $0x20dc,%ax */
	mov    $D_00dc,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_08eb:
	mov    -0x6(%bp),%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    0x8(%bp),%bx
	mov    0x204(%bx),%ax
	mov    0x206(%bx),%dx
	add    -0xc(%bp),%ax
	adc    -0xa(%bp),%dx
	mov    %ax,0x204(%bx)
	mov    %dx,0x206(%bx)
	mov    0x8(%bp),%bx
	mov    0x204(%bx),%ax
	mov    0x206(%bx),%dx
	mov    %ax,D_0eec
	mov    %dx,D_0eee
	add    -0x6(%bp),%di
	sub    -0x6(%bp),%si
L_0926:
	jmp   L_0810
L_0929:
	pushw  0x8(%bp)
	call   L_095c
	pop    %cx
	jmp   L_0926
L_0932:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0939:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_0ee8
	jne   L_0958
	mov    D_0ef0,%di
L_0949:
	or     %di,%di
	je   L_0958
	push   %di
	call   L_095c
	pop    %cx
	mov    0x208(%di),%di
	jmp   L_0949
L_0958:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_095c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	lea    0x4(%di),%ax
	mov    0x2(%di),%dx
	sub    %ax,%dx
	mov    %dx,%si
	or     %si,%si
	jne   L_0979
	jmp   L_0a00
L_0979:
	mov    D_0eec,%ax
	mov    D_0eee,%dx
	cmp    0x206(%di),%dx
	jne   L_098c
	cmp    0x204(%di),%ax
	je   L_09a1
L_098c:
	xor    %ax,%ax
	push   %ax
	pushw  0x206(%di)
	pushw  0x204(%di)
	pushw  D_0ee4
	call   lseek
	add    $0x8,%sp
L_09a1:
	push   %si
	lea    0x4(%di),%ax
	push   %ax
	pushw  D_0ee4
	call   L_590a
	add    $0x6,%sp
	cmp    %si,%ax
	je   L_09c8
	movw   $0x1,D_0ee8
	/* TODO: data ref: mov    $0x20f0,%ax */
	mov    $D_00f0,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_09c8:
	mov    %si,%ax
	cwtd
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x204(%di),%ax
	mov    0x206(%di),%dx
	add    -0x8(%bp),%ax
	adc    -0x6(%bp),%dx
	mov    %ax,0x204(%di)
	mov    %dx,0x206(%di)
	mov    0x204(%di),%ax
	mov    0x206(%di),%dx
	mov    %ax,D_0eec
	mov    %dx,D_0eee
	lea    0x4(%di),%ax
	mov    %ax,0x2(%di)
	movw   $0x200,(%di)
L_0a00:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a07:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	push   %di
	call   L_095c
	pop    %cx
	push   %di
	pushw  0x6(%bp)
	call   L_0a23
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a23:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x6(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_0a48
	mov    0x6(%bp),%bx
	decw   (%bx)
	mov    0x6(%bp),%bx
	incw   0x2(%bx)
	mov    0x2(%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    0x4(%bp),%al
	mov    %al,(%bx)
	jmp   L_0a56
L_0a48:
	mov    0x4(%bp),%al
	cbtw
	push   %ax
	pushw  0x6(%bp)
	call   L_0a07
	add    $0x4,%sp
L_0a56:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a5a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x6(%bp),%di
	cmpw   $0x1,(%di)
	jle   L_0a79
	subw   $0x2,(%di)
	mov    0x2(%di),%bx
	mov    0x4(%bp),%ax
	mov    %ax,(%bx)
	addw   $0x2,0x2(%di)
	jmp   L_0aa7
L_0a79:
	cmpw   $0x1,(%di)
	jne   L_0a9d
	mov    0x4(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    0x2(%di),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	decw   (%di)
	mov    0x4(%bp),%ax
	cbtw
	push   %ax
	push   %di
	call   L_0a07
	add    $0x4,%sp
	jmp   L_0aa7
L_0a9d:
	pushw  0x4(%bp)
	push   %di
	call   L_0aab
	add    $0x4,%sp
L_0aa7:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0aab:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	push   %di
	call   L_095c
	pop    %cx
	push   %di
	pushw  0x6(%bp)
	call   L_0a5a
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
	sub    $0x6,%sp
	cmpw   $0x0,D_0e94
	jne   L_0aea
	cmpw   $0x0,D_0e92
	jne   L_0aea
	cmpw   $0x0,D_0e90
	jne   L_0aea
	mov    $0xffff,%ax
	jmp   L_0bc3
L_0aea:
	cmpw   $0x8,D_0e90
	jl   L_0af4
	jmp   L_0be5
L_0af4:
	cmpw   $0x0,0x6(%bp)
	je   L_0b10
	pushw  0x4(%bp)
	mov    D_0e96,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	push   %ax
	pushw  D_0e8c
	call   L_0800
	add    $0x6,%sp
L_0b10:
	movw   $0x0,-0x8(%bp)
L_0b15:
	mov    -0x8(%bp),%ax
	cmp    D_0e90,%ax
	jge   L_0b47
	mov    D_0e96,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	add    D_0e8c,%ax
	add    -0x8(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    D_0e9a,%dx
	add    $0x8,%dx
	add    -0x8(%bp),%dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_0b15
L_0b47:
	incw   D_0e8e
	xor    %ax,%ax
	push   %ax
	mov    D_0e8e,%ax
	cwtd
	mov    $0xa,%cx
	or     %cx,%cx
	je   L_0b5f
L_0b59:
	shl    %ax
	rcl    %dx
	loop   L_0b59
L_0b5f:
	push   %dx
	push   %ax
	pushw  D_0ea0
	call   lseek
	add    $0x8,%sp
	mov    D_0e90,%ax
	cwtd
	mov    D_0e92,%bx
	mov    D_0e94,%si
	sub    %ax,%bx
	sbb    %dx,%si
	cmp    $0x0,%si
	jg   L_0b9a
	jl   L_0b88
	cmp    $0x400,%bx
	jae   L_0b9a
L_0b88:
	mov    D_0e90,%ax
	cwtd
	mov    D_0e92,%bx
	mov    D_0e94,%si
	sub    %ax,%bx
	sbb    %dx,%si
	jmp   L_0b9f
L_0b9a:
	mov	$1024,%bx	/* sc: ?1024 */
	xor    %si,%si
L_0b9f:
	mov    %bx,-0xa(%bp)
	pushw  -0xa(%bp)
	mov    D_0e9a,%ax
	add    $0x8,%ax
	add    D_0e90,%ax
	push   %ax
	pushw  D_0ea0
	call   read
	add    $0x6,%sp
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jg   L_0bc5
	xor    %ax,%ax
L_0bc3:
	jmp   L_0c0a
L_0bc5:
	mov    D_0e9a,%bx
	mov    -0x8(%bp),%ax
	mov    %ax,0x4(%bx)
	mov    -0x8(%bp),%ax
	add    %ax,D_0e90
	mov    D_0e9a,%ax
	add    $0x8,%ax
	mov    %ax,D_0e8c
	movw   $0x0,D_0e96
L_0be5:
	mov    D_0e96,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	add    D_0e8c,%ax
	mov    %ax,-0x6(%bp)
	incw   D_0e96
	subw   $0x8,D_0e90
	subw   $0x8,D_0e92
	sbbw   $0x0,D_0e94
	mov    -0x6(%bp),%ax
L_0c0a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0c11:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xe,%sp
L_0c19:
	cmpw   $0x0,D_0f0c
	jne   L_0c25
	cmpw   $0x0,D_0f0a
L_0c25:
	jne   L_0c2c
	mov    $0xffff,%ax
	jmp   L_0ca3
L_0c2c:
	cmpw   $0x4,D_0f08
	jge   L_0c38
	mov    D_0f08,%ax
	jmp   L_0c3b
L_0c38:
	mov    $0x4,%ax
L_0c3b:
	mov    %ax,-0x8(%bp)
	mov    D_0f08,%ax
	cwtd
	cmp    D_0f0c,%dx
	jne   L_0c53
	cmp    D_0f0a,%ax
	jne   L_0c53
	mov    D_0f08,%ax
	jmp   L_0c59
L_0c53:
	mov    D_0f08,%ax
	sub    -0x8(%bp),%ax
L_0c59:
	cwtd
	mov    D_0f0e,%bx
	mov    D_0f10,%si
	add    %ax,%bx
	adc    %dx,%si
	mov    %bx,-0xe(%bp)
	mov    %si,-0xc(%bp)
	mov    D_0f0e,%ax
	mov    D_0f10,%dx
	cmp    0x6(%bp),%dx
	jg   L_0ca5
	jl   L_0c7f
	cmp    0x4(%bp),%ax
	ja   L_0ca5
L_0c7f:
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	cmp    -0xc(%bp),%dx
	jg   L_0ca5
	jl   L_0c91
	cmp    -0xe(%bp),%ax
	jae   L_0ca5
L_0c91:
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	sub    D_0f0e,%ax
	sbb    D_0f10,%dx
	add    D_0f04,%ax
L_0ca3:
	jmp   L_0cbb
L_0ca5:
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	cmp    D_0f10,%dx
	jg   L_0cbe
	jl   L_0cb9
	cmp    D_0f0e,%ax
	jae   L_0cbe
L_0cb9:
	xor    %ax,%ax
L_0cbb:
	jmp   L_0e5a
L_0cbe:
	mov    D_0f08,%ax
	cwtd
	cmp    D_0f0c,%dx
	jne   L_0cd1
	cmp    D_0f0a,%ax
	jne   L_0cd1
	jmp   L_0e05
L_0cd1:
	mov    D_0f08,%ax
	sub    -0x8(%bp),%ax
	jle   L_0d22
	cmpw   $0x0,0x8(%bp)
	jne   L_0d0e
	mov    D_0f08,%ax
	sub    -0x8(%bp),%ax
	mov    %ax,-0x10(%bp)
	mov    D_0f04,%ax
	mov    %ax,-0x12(%bp)
L_0cee:
	cmpw   $0x0,-0x10(%bp)
	je   L_0d22
	mov    -0x12(%bp),%bx
	mov    (%bx),%al
	cbtw
	/* TODO: data ref: mov    0x2200,%bx */
	mov    D_0200,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x12(%bp)
	/* TODO: data ref: incw   0x2200 */
	incw   D_0200
	decw   -0x10(%bp)
	jmp   L_0cee
L_0d0e:
	pushw  0x8(%bp)
	mov    D_0f08,%ax
	sub    -0x8(%bp),%ax
	push   %ax
	pushw  D_0f04
	call   L_0800
	add    $0x6,%sp
L_0d22:
	movw   $0x0,-0x6(%bp)
L_0d27:
	mov    -0x6(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_0d56
	mov    D_0f08,%ax
	sub    -0x8(%bp),%ax
	add    D_0f04,%ax
	add    -0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    D_0f12,%dx
	add    $0x8,%dx
	add    -0x6(%bp),%dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	jmp   L_0d27
L_0d56:
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	mov    %ax,D_0f0e
	mov    %dx,D_0f10
	mov    D_0f08,%ax
	sub    -0x8(%bp),%ax
	cwtd
	sub    %ax,D_0f0a
	sbb    %dx,D_0f0c
	incw   D_0f06
	xor    %ax,%ax
	push   %ax
	mov    D_0f06,%ax
	cwtd
	mov    $0xa,%cx
	or     %cx,%cx
	je   L_0d8a
L_0d84:
	shl    %ax
	rcl    %dx
	loop   L_0d84
L_0d8a:
	push   %dx
	push   %ax
	pushw  D_0ea0
	call   lseek
	add    $0x8,%sp
	mov    -0x8(%bp),%ax
	cwtd
	mov    D_0f0a,%bx
	mov    D_0f0c,%si
	sub    %ax,%bx
	sbb    %dx,%si
	cmp    $0x0,%si
	jg   L_0dc5
	jl   L_0db3
	cmp    $0x400,%bx
	jae   L_0dc5
L_0db3:
	mov    -0x8(%bp),%ax
	cwtd
	mov    D_0f0a,%bx
	mov    D_0f0c,%si
	sub    %ax,%bx
	sbb    %dx,%si
	jmp   L_0dca
L_0dc5:
	mov	$1024,%bx	/* sc: ?1024 */
	xor    %si,%si
L_0dca:
	mov    %bx,-0xa(%bp)
	pushw  -0xa(%bp)
	mov    D_0f12,%ax
	add    $0x8,%ax
	add    -0x8(%bp),%ax
	push   %ax
	pushw  D_0ea0
	call   read
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jg   L_0df0
	movw   $0x0,-0x6(%bp)
L_0df0:
	mov    -0x8(%bp),%ax
	add    -0x6(%bp),%ax
	mov    %ax,D_0f08
	mov    D_0f12,%ax
	add    $0x8,%ax
	mov    %ax,D_0f04
L_0e02:
	jmp   L_0c19
L_0e05:
	cmpw   $0x0,0x8(%bp)
	jne   L_0e37
	mov    D_0f08,%ax
	mov    %ax,-0x10(%bp)
	mov    D_0f04,%ax
	mov    %ax,-0x12(%bp)
L_0e17:
	cmpw   $0x0,-0x10(%bp)
	je   L_0e48
	mov    -0x12(%bp),%bx
	mov    (%bx),%al
	cbtw
	/* TODO: data ref: mov    0x2200,%bx */
	mov    D_0200,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	decw   -0x10(%bp)
	/* TODO: data ref: incw   0x2200 */
	incw   D_0200
	incw   -0x12(%bp)
	jmp   L_0e17
L_0e37:
	pushw  0x8(%bp)
	pushw  D_0f08
	pushw  D_0f04
	call   L_0800
	add    $0x6,%sp
L_0e48:
	movw   $0x0,D_0f08
	xor    %ax,%ax
	cwtd
	mov    %ax,D_0f0a
	mov    %dx,D_0f0c
	jmp   L_0e02
L_0e5a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	/* vrewrite: initialize _environ from envp (TLVC crt0 3rd arg) */
	mov	0x8(%bp),%ax
	mov	%ax,D_0e84
	push   %si
	push   %di
	sub    $0x10,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	//call   signal
	add    $0x4,%sp
	jmp	L_0e99 ///////////////////
	cmp    $0x1,%ax
	je   L_0e99
	/* TODO: data ref: mov    $0x201e,%ax */
	mov    $D_001e,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   signal
	add    $0x4,%sp
	/* TODO: data ref: mov    $0x201e,%ax */
	mov    $D_001e,%ax
	push   %ax
	mov    $0xf,%ax
	push   %ax
	call   signal
	add    $0x4,%sp
L_0e99:
	cmpw   $0x1,0x4(%bp)	// If argc == 1, exit
	jne   L_0ea7
	mov    $0x4,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0ea7:
	mov    0x6(%bp),%ax
	inc    %ax
	inc    %ax
	mov    %ax,-0x12(%bp)
	mov    $0x1,%di
L_0eb2:
	cmp    0x4(%bp),%di
	jl   L_0eba
	jmp   L_116e
L_0eba:
	movw   $0x0,D_0f00
	mov    -0x12(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x10(%bp)
	addw   $0x2,-0x12(%bp)
	mov    -0x10(%bp),%bx
	cmpb   $0x2d,(%bx)
	je   L_0ede
	pushw  -0x10(%bp)
	call   L_1188
	pop    %cx
	jmp   L_1016
L_0ede:
	mov    $0x1,%si
L_0ee1:
	mov    -0x10(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	jne   L_0ef0
	jmp   L_1016
L_0ef0:
	mov    -0x10(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	jmp   L_10df
L_0efd:
	inc    %di
	cmp    0x4(%bp),%di
	jl   L_0f11
	/* TODO: data ref: mov    $0x2253,%ax */
	mov    $D_0253,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_0f11:
	mov    -0x12(%bp),%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x2008 */
	mov    %ax,D_0008
	addw   $0x2,-0x12(%bp)
	incw   D_0e9e
	jmp   L_0f5b
L_0f23:
	inc    %di
	cmp    0x4(%bp),%di
	jl   L_0f37
	/* TODO: data ref: mov    $0x225d,%ax */
	mov    $D_025d,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_0f37:
	mov    -0x12(%bp),%bx
	addw   $0x2,-0x12(%bp)
	pushw  (%bx)
	call   L_1e89
	pop    %cx
	push   %ax
	call   L_1eac
	pop    %cx
	mov    -0x10(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	cmpb   $0x65,(%bx)
	jne   L_0f5b
	mov    D_1a1a,%ax
	mov    %ax,D_184a
L_0f5b:
	jmp   L_0fb4
L_0f5d:
	inc    %di
	cmp    0x4(%bp),%di
	jl   L_0f71
	/* TODO: data ref: mov    $0x2273,%ax */
	mov    $D_0273,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_0f71:
	mov    -0x12(%bp),%bx
	addw   $0x2,-0x12(%bp)
	pushw  (%bx)
	call   L_430c
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    -0x10(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	jmp   L_0fd8
L_0f8d:
	cmpw   $0x0,D_1836
	jne   L_0f9b
	cmpw   $0x0,D_1834
	je   L_0fa9
L_0f9b:
	/* TODO: data ref: mov    $0x228d,%ax */
	mov    $D_028d,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_0fa9:
	mov    -0x6(%bp),%ax
	cwtd
	mov    %ax,D_1e28
	mov    %dx,D_1e2a
L_0fb4:
	jmp   L_0fe7
L_0fb6:
	mov    -0x6(%bp),%ax
	cwtd
	mov    %ax,D_1e38
	mov    %dx,D_1e3a
	incw   D_1e24
	jmp   L_0fe7
L_0fc7:
	mov    -0x6(%bp),%ax
	cwtd
	mov    %ax,D_1a28
	mov    %dx,D_1a2a
	incw   D_1e40
	jmp   L_101d
L_0fd8:
	cmp    $0x42,%ax
	je   L_0fc7
	cmp    $0x44,%ax
	je   L_0fb6
	cmp    $0x54,%ax
	je   L_0f8d
L_0fe7:
	jmp   L_1037
L_0fe9:
	dec    %si
	mov    %si,%ax
	add    -0x10(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	mov    %al,-0x13(%bp)
	mov    -0x10(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	movb   $0x2d,(%bx)
	mov    -0x10(%bp),%ax
	add    %si,%ax
	push   %ax
	call   L_1188
	pop    %cx
	mov    -0x10(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	mov    -0x13(%bp),%al
	mov    %al,(%bx)
L_1016:
	jmp   L_116a
L_1019:
	incw   D_1a30
L_101d:
	jmp   L_1049
L_101f:
	incw   D_1a2e
	jmp   L_1049
L_1025:
	incw   D_19f2
	incw   D_19f0
	jmp   L_1049
L_102f:
	incw   D_1a06
	incw   D_1a30
L_1037:
	jmp   L_1049
L_1039:
	incw   D_183c
	jmp   L_1049
L_103f:
	incw   D_1a18
	jmp   L_1090
L_1045:
	incw   D_182c
L_1049:
	jmp   L_1090
L_104b:
	lea    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
L_1051:
	mov    -0x10(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	cmpb   $0x39,0x1(%bx)
	jg   L_1082
	mov    -0x10(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	cmpb   $0x30,0x1(%bx)
	jl   L_1082
	inc    %si
	mov    %si,%ax
	add    -0x10(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    -0xe(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xe(%bp)
	jmp   L_1051
L_1082:
	lea    -0xc(%bp),%ax
	cmp    %ax,-0xe(%bp)
	jne   L_1092
	/* TODO: data ref: movw   $0x2000,0x3e3c */
	/* movw constant -> D_1e3c */
	mov	$0x0000,%ax
	mov	%ax,D_1e3c
L_1090:
	jmp   L_10dc
L_1092:
	mov    -0xe(%bp),%bx
	movb   $0x0,(%bx)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_441c
	pop    %cx
	mov    %ax,D_1e3c
	jmp   L_10dc
L_10a5:
	incw   D_19dc
	jmp   L_10dc
L_10ab:
	/* TODO: data ref: mov    $0x22af,%ax */
	mov    $D_02af,%ax
	push   %ax
	call   L_1c3e
	pop    %cx
	mov    %ax,D_0f00
	mov    -0x10(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    D_0f00,%bx
	mov    %ax,%cx
	mov    %cl,0x1(%bx)
	movb   $0x0,D_0ea4
	/* TODO: data ref: mov    $0x22b2,%ax */
	mov    $D_02b2,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_10dc:
	jmp   L_1166
L_10df:
	cmp    $0x6c,%ax
	jne   L_10e7
	jmp   L_0fe9
L_10e7:
	jg   L_1126
	cmp    $0x54,%ax
L_10ec:
	jne   L_10f1
	jmp   L_0f5d
L_10f1:
	jg   L_1104
	cmp    $0x41,%ax
	je   L_10a5
	cmp    $0x42,%ax
	je   L_10ec
	cmp    $0x44,%ax
	je   L_10ec
L_1102:
	jmp   L_10ab
L_1104:
	cmp    $0x58,%ax
	jne   L_110c
	jmp   L_101f
L_110c:
	cmp    $0x64,%ax
	jne   L_1114
	jmp   L_103f
L_1114:
	cmp    $0x65,%ax
L_1117:
	jne   L_111c
	jmp   L_0f23
L_111c:
	cmp    $0x69,%ax
L_111f:
	jne   L_1124
	jmp   L_1039
L_1124:
	jmp   L_1102
L_1126:
	cmp    $0x72,%ax
	jne   L_112e
	jmp   L_1025
L_112e:
	jg   L_1147
	cmp    $0x6d,%ax
	jne   L_1138
	jmp   L_1045
L_1138:
	cmp    $0x6e,%ax
	je   L_111f
	cmp    $0x6f,%ax
	jne   L_1145
L_1142:
	jmp   L_0efd
L_1145:
	jmp   L_1102
L_1147:
	cmp    $0x73,%ax
	jne   L_114f
	jmp   L_102f
L_114f:
	cmp    $0x75,%ax
	je   L_1117
	cmp    $0x78,%ax
	jne   L_115c
	jmp   L_1019
L_115c:
	cmp    $0x7a,%ax
	jne   L_1164
	jmp   L_104b
L_1164:
	jmp   L_1102
L_1166:
	inc    %si
	jmp   L_0ee1
L_116a:
	inc    %di
	jmp   L_0eb2
L_116e:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_2073
	add    $0x4,%sp
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1188:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xa,%sp
	mov    0x4(%bp),%di
	push   %di
	call   L_0585
	pop    %cx
	mov    %ax,-0xa(%bp)
	jmp   L_1400
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_15ab
	add    $0x6,%sp
	jmp   L_13a2
	/* TODO: data ref: mov    $0x22bb,%ax */
	mov    $D_02bb,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
	movw   $0x8,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
L_11c9:
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	call   L_141b
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_11dc
	jmp   L_13a2
L_11dc:
	mov    $0x2,%ax
	push   %ax
	mov    $D_0ed4,%ax
	push   %ax
	call   L_430c
	pop    %cx
	push   %dx
	push   %ax
	call   L_32b3
	add    $0x6,%sp
	add    $0x3c,%ax
	adc    $0x0,%dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    -0xe(%bp),%ax
	adc    -0xc(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	jmp   L_11c9
	movw   $0x44,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
	mov    $0x2,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	mov    $D_0f04,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	mov    $D_183a,%ax
	push   %ax
	call   L_0066
	add    $0x6,%sp
	addw   $0x2,-0x8(%bp)
	adcw   $0x0,-0x6(%bp)
	pushw  D_183a
	call   L_557e
	pop    %cx
	mov    %ax,D_1a1c
	or     %ax,%ax
	jne   L_1268
	/* TODO: data ref: mov    $0x22fe,%ax */
	mov    $D_02fe,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_1268:
	mov    D_183a,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	mov    $D_0f04,%ax
	push   %ax
	pushw  D_183a
	pushw  D_1a1c
	call   L_0066
	add    $0x6,%sp
	mov    D_183a,%ax
	xor    %dx,%dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    -0xe(%bp),%ax
	adc    -0xc(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    D_183a,%ax
	xor    %dx,%dx
	mov    $0x8,%cx
	div    %cx
	mov    %ax,D_183a
	mov    $0x2,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	mov    $D_0f04,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	mov    $D_1a2c,%ax
	push   %ax
	call   L_0066
	add    $0x6,%sp
	addw   $0x2,-0x8(%bp)
	adcw   $0x0,-0x6(%bp)
	pushw  D_1a2c
	call   L_557e
	pop    %cx
	mov    %ax,D_1838
	or     %ax,%ax
	jne   L_1309
	/* TODO: data ref: mov    $0x2316,%ax */
	mov    $D_0316,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_1309:
	mov    D_1a2c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	mov    $D_0f04,%ax
	push   %ax
	pushw  D_1a2c
	pushw  D_1838
	call   L_0066
	add    $0x6,%sp
	mov    D_183a,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	add    D_1a1c,%ax
	mov    %ax,%si
L_1340:
	sub    $0x8,%si
	cmp    D_1a1c,%si
	jb   L_137e
	cmpw   $0x0,0x2(%si)
	jl   L_1366
	jg   L_1356
	cmpw   $0x0,(%si)
	jb   L_1366
L_1356:
	mov    D_1a2c,%ax
	xor    %dx,%dx
	cmp    %dx,0x2(%si)
	jb   L_1374
	ja   L_1366
	cmp    %ax,(%si)
	jb   L_1374
L_1366:
	/* TODO: data ref: mov    $0x2331,%ax */
	mov    $D_0331,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_1374:
	mov    (%si),%ax
	add    D_1838,%ax
	mov    %ax,(%si)
	jmp   L_1340
L_137e:
	call   L_1503
	or     %ax,%ax
	jne   L_137e
	pushw  D_1a1c
	call   L_43f6
	pop    %cx
	pushw  D_1838
	call   L_43f6
	pop    %cx
	mov    $0xffff,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	call   L_148a
	add    $0x4,%sp
L_13a2:
	jmp   L_13fe
	/* TODO: data ref: mov    $0x2353,%ax */
	mov    $D_0353,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
	movw   $0x8,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
L_13bc:
	mov    $0x2,%ax
	push   %ax
	mov    $D_0ed4,%ax
	push   %ax
	call   L_430c
	pop    %cx
	push   %dx
	push   %ax
	call   L_32b3
	add    $0x6,%sp
	add    $0x3c,%ax
	adc    $0x0,%dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    -0xe(%bp),%ax
	adc    -0xc(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	call   L_141b
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_13bc
L_13fe:
	jmp   L_140c
L_1400:
	cmp    $0x3,%ax
	ja   L_140c
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2218(%bx)
L_140c:
	pushw  D_0ea0
	call   L_5814
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_141b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	cmpw   $0x0,D_0f0c
	jg   L_1458
	jl   L_1447
	cmpw   $0x0,D_0f0a
	ja   L_1458
L_1447:
	mov    $0xffff,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	call   L_148a
	add    $0x4,%sp
	xor    %ax,%ax
	jmp   L_1486
L_1458:
	call   L_0036
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	add    $0x3c,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_15ab
	add    $0x6,%sp
	or     %ax,%ax
	je   L_1483
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_148a
	add    $0x4,%sp
L_1483:
	mov    $0x1,%ax
L_1486:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_148a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    0x21f4,%bx */
	mov    D_01f4,%bx
	cmpw   $0xfa,0x2(%bx)
	jne   L_14da
	/* TODO: data ref: addw   $0x6,0x21f4 */
	addw   $0x6,D_01f4
	/* TODO: data ref: cmpw   $0x21f4,0x21f4 */
	cmpw   $D_01f4,D_01f4
	jne   L_14b5
	/* TODO: data ref: mov    $0x239a,%ax */
	mov    $D_039a,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_14b5:
	mov    $0x3e8,%ax
	push   %ax
	call   L_557e
	pop    %cx
	/* TODO: data ref: mov    0x21f4,%bx */
	mov    D_01f4,%bx
	mov    %ax,(%bx)
	/* TODO: data ref: mov    0x21f4,%bx */
	mov    D_01f4,%bx
	cmpw   $0x0,(%bx)
	jne   L_14da
	/* TODO: data ref: mov    $0x23bf,%ax */
	mov    $D_03bf,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_14da:
	/* TODO: data ref: mov    0x21f4,%bx */
	mov    D_01f4,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    0x21f4,%bx */
	mov    D_01f4,%bx
	incw   0x2(%bx)
	mov    0x2(%bx),%dx
	dec    %dx
	mov    $0x2,%cx
	shl    %cl,%dx
	add    %dx,%ax
	mov    %ax,%bx
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1503:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xa,%sp
	pushw  D_1e3e
	call   L_1f49
	pop    %cx
	mov    %ax,-0xe(%bp)
	mov    D_183a,%ax
	dec    %ax
	mov    $0x3,%cx
	shl    %cl,%ax
	add    D_1a1c,%ax
	mov    %ax,-0x8(%bp)
	mov    D_1a1c,%ax
	mov    %ax,-0x6(%bp)
L_152c:
	mov    -0x6(%bp),%ax
	cmp    -0x8(%bp),%ax
	ja   L_1590
	mov    -0x6(%bp),%bx
	pushw  (%bx)
	call   L_1e89
	pop    %cx
	mov    %ax,%si
	or     %ax,%ax
	je   L_158a
	mov    (%si),%di
	cmpb   $0x1,0x2(%di)
	jne   L_158a
	mov    -0x6(%bp),%bx
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	call   L_141b
	add    $0x4,%sp
	mov    -0x6(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x6(%bx),%dx
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
L_1569:
	mov    -0x6(%bp),%ax
	cmp    -0x8(%bp),%ax
	jae   L_158a
	mov    -0x6(%bp),%bx
	mov    0xc(%bx),%ax
	mov    0xe(%bx),%dx
	cmp    -0xa(%bp),%dx
	jne   L_158a
	cmp    -0xc(%bp),%ax
	jne   L_158a
	addw   $0x8,-0x6(%bp)
	jmp   L_1569
L_158a:
	addw   $0x8,-0x6(%bp)
	jmp   L_152c
L_1590:
	pushw  D_1e3e
	call   L_1f49
	pop    %cx
	cmp    -0xe(%bp),%ax
	je   L_15a2
	mov    $0x1,%ax
	jmp   L_15a4
L_15a2:
	xor    %ax,%ax
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
	sub    $0x3e,%sp
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	call   L_3331
	add    $0x4,%sp
	cmpw   $0x0,D_180e
	jne   L_15f7
	cmpw   $0x0,D_180c
	jne   L_15f7
	mov    D_1800,%ax
	mov    D_1802,%dx
	add    D_1804,%ax
	adc    D_1806,%dx
	or     %dx,%dx
	jne   L_15e9
	or     %ax,%ax
	jne   L_15e9
	xor    %ax,%ax
	jmp   L_1bc5
L_15e9:
	/* TODO: data ref: mov    $0x23dc,%ax */
	mov    $D_03dc,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_15f7:
	cmpw   $0x0,0x4(%bp)
	je   L_1612
	mov    $D_0ed4,%ax
	push   %ax
	call   L_430c
	pop    %cx
	mov    %ax,-0x1c(%bp)
	mov    %dx,-0x1a(%bp)
	movw   $D_0ea4,-0x16(%bp)
	jmp   L_1632
L_1612:
	lea    -0x3a(%bp),%ax
	push   %ax
	pushw  D_0ea0
	call   L_5828
	add    $0x4,%sp
	mov    D_0f00,%ax
	mov    %ax,-0x16(%bp)
	mov    -0x2c(%bp),%ax
	mov    -0x2a(%bp),%dx
	mov    %ax,-0x1c(%bp)
	mov    %dx,-0x1a(%bp)
L_1632:
	mov    D_1800,%ax
	mov    D_1802,%dx
	add    $0x20,%ax
	adc    $0x0,%dx
	add    D_1804,%ax
	adc    D_1806,%dx
	add    D_1814,%ax
	adc    D_1816,%dx
	add    D_1818,%ax
	adc    D_181a,%dx
	add    D_180c,%ax
	adc    D_180e,%dx
	add    $0x4,%ax
	adc    $0x0,%dx
	cmp    -0x1a(%bp),%dx
	jl   L_167f
	jg   L_1671
	cmp    -0x1c(%bp),%ax
	jb   L_167f
L_1671:
	/* TODO: data ref: mov    $0x23e8,%ax */
	mov    $D_03e8,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_167f:
	mov    D_19e4,%ax
	mov    %ax,-0x14(%bp)
	xor    %si,%si
L_1687:
	cmp    $0x2,%si
	jl   L_168f
	jmp   L_17b5
L_168f:
	pushw  -0x16(%bp)
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: pushw  0x21fa(%bx) */
	pushw  D_01fa(%bx)
	call   L_57d2
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_16be
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: cmpw   $0x0,0x2204(%bx) */
	cmpw   $0x0,D_0204(%bx)
	jne   L_16be
	/* TODO: data ref: cmpw   $0x0,0x2202(%bx) */
	cmpw   $0x0,D_0202(%bx)
L_16be:
	je   L_16c3
	jmp   L_17b1
L_16c3:
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	mov    -0x1c(%bp),%ax
	mov    -0x1a(%bp),%dx
	/* TODO: data ref: mov    %ax,0x2202(%bx) */
	mov    %ax,D_0202(%bx)
	/* TODO: data ref: mov    %dx,0x2204(%bx) */
	mov    %dx,D_0204(%bx)
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: mov    0x2202(%bx),%ax */
	mov    D_0202(%bx),%ax
	/* TODO: data ref: mov    0x2204(%bx),%dx */
	mov    D_0204(%bx),%dx
	mov    %ax,-0x18(%bp)
	pushw  -0x18(%bp)
	call   L_557e
	pop    %cx
	mov    %ax,-0x3e(%bp)
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	mov    -0x3e(%bp),%ax
	/* TODO: data ref: mov    %ax,0x21fe(%bx) */
	mov    %ax,D_01fe(%bx)
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: cmpw   $0x0,0x21fe(%bx) */
	cmpw   $0x0,D_01fe(%bx)
	jne   L_1726
	/* TODO: data ref: mov    $0x2403,%ax */
	mov    $D_0403,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_1726:
	pushw  -0x1a(%bp)
	pushw  -0x1c(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	mov    $D_0f04,%ax
	push   %ax
	pushw  -0x1c(%bp)
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: pushw  0x21fe(%bx) */
	pushw  D_01fe(%bx)
	call   L_0066
	add    $0x6,%sp
	mov    D_1846,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_174c(%bx),%ax
	mov    D_174e(%bx),%dx
	mov    %ax,-0x3e(%bp)
	mov    %dx,-0x3c(%bp)
	mov    D_1834,%ax
	mov    D_1836,%dx
	add    D_1800,%ax
	adc    D_1802,%dx
	sub    -0x3e(%bp),%ax
	sbb    -0x3c(%bp),%dx
	/* TODO: data ref: cmp    0x21f8,%dx */
	cmp    D_01f8,%dx
	jl   L_1796
	jg   L_1790
	/* TODO: data ref: cmp    0x21f6,%ax */
	cmp    D_01f6,%ax
	jbe   L_1796
L_1790:
	mov    D_1846,%ax
	inc    %ax
	jmp   L_1799
L_1796:
	mov    D_1846,%ax
L_1799:
	mov    %ax,-0x42(%bp)
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	mov    -0x42(%bp),%ax
	/* TODO: data ref: mov    %ax,0x2206(%bx) */
	mov    %ax,D_0206(%bx)
	movw   $0xffff,-0x14(%bp)
L_17b1:
	inc    %si
	jmp   L_1687
L_17b5:
	mov    D_1a20,%ax
	mov    D_1a22,%dx
	add    D_1834,%ax
	adc    D_1836,%dx
	mov    %ax,D_1a20
	mov    %dx,D_1a22
	mov    D_1840,%ax
	mov    D_1842,%dx
	add    D_1830,%ax
	adc    D_1832,%dx
	mov    %ax,D_1840
	mov    %dx,D_1842
	mov    D_19e0,%ax
	mov    D_19e2,%dx
	add    D_17f0,%ax
	adc    D_17f2,%dx
	mov    %ax,D_19e0
	mov    %dx,D_19e2
	movw   $0x0,-0xa(%bp)
	movw   $0x0,-0x12(%bp)
	movw   $0xa,-0xc(%bp)
	mov    D_1e3e,%ax
	mov    %ax,-0x6(%bp)
	mov    D_1800,%ax
	mov    D_1802,%dx
	add    $0x20,%ax
	adc    $0x0,%dx
	add    D_1804,%ax
	adc    D_1806,%dx
	add    D_1814,%ax
	adc    D_1816,%dx
	add    D_1818,%ax
	adc    D_181a,%dx
	mov    %ax,-0x3e(%bp)
	mov    %dx,-0x3c(%bp)
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	add    -0x3e(%bp),%ax
	adc    -0x3c(%bp),%dx
	mov    %ax,0x6(%bp)
	mov    %dx,0x8(%bp)
	pushw  D_180e
	pushw  D_180c
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	mov    $0x2,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	add    D_180c,%ax
	adc    D_180e,%dx
	push   %dx
	push   %ax
	mov    $D_0e8c,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	mov    $D_0e8c,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_0066
	add    $0x6,%sp
	mov    -0x8(%bp),%ax
	dec    %ax
	dec    %ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	add    D_180c,%ax
	adc    D_180e,%dx
	add    $0x2,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	mov    $D_0e8c,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	pushw  -0x8(%bp)
	call   L_557e
	pop    %cx
	mov    %ax,D_1848
	or     %ax,%ax
	jne   L_18d9
	/* TODO: data ref: mov    $0x2418,%ax */
	mov    $D_0418,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_18d9:
	mov    $D_0e8c,%ax
	push   %ax
	mov    -0x8(%bp),%ax
	dec    %ax
	dec    %ax
	push   %ax
	mov    D_1848,%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_0066
	add    $0x6,%sp
L_18ef:
	cmpw   $0x0,D_0f0c
	jge   L_18f9
	jmp   L_19fa
L_18f9:
	jg   L_1905
	cmpw   $0x0,D_0f0a
	ja   L_1905
	jmp   L_19fa
L_1905:
	mov    $D_0f04,%ax
	push   %ax
	mov    $0xa,%ax
	push   %ax
	mov    $D_19fc,%ax
	push   %ax
	call   L_0066
	add    $0x6,%sp
	cmpw   $0x0,D_19fc
	je   L_1945
	cmpw   $0x2,D_19fc
	jb   L_192d
	mov    D_19fc,%ax
	cmp    -0x8(%bp),%ax
	jb   L_193b
L_192d:
	/* TODO: data ref: mov    $0x2437,%ax */
	mov    $D_0437,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_193b:
	mov    D_1848,%ax
	add    D_19fc,%ax
	mov    %ax,D_19fc
L_1945:
	mov    -0x14(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_19ff
	mov    D_19fe,%al
	cbtw
	mov    %ax,-0xe(%bp)
	testw  $0x1,-0xe(%bp)
	jne   L_197d
	cmpw   $0x0,D_1a2e
	je   L_1976
	mov    D_19fc,%bx
	cmpb   $0x4c,(%bx)
	jne   L_1976
	testw  $0xe0,-0xe(%bp)
	jne   L_1976
	jmp   L_18ef
L_1976:
	addw   $0xa,-0xc(%bp)
L_197a:
	jmp   L_18ef
L_197d:
	cmpb   $0x5,D_19fe
	jne   L_1987
	incw   -0x12(%bp)
L_1987:
	call   L_1fa5
	call   L_1ca0
	push   %ax
	call   L_1eac
	pop    %cx
	or     %ax,%ax
	jne   L_197a
	mov    D_1a1a,%di
	cmpb   $0x1,0x2(%di)
	jne   L_197a
	cmpb   $0x1,D_19fe
	jne   L_19c9
	mov    D_1a02,%ax
	mov    D_1a04,%dx
	cmp    0x8(%di),%dx
	jl   L_197a
	jg   L_19ba
	cmp    0x6(%di),%ax
	jbe   L_197a
L_19ba:
	mov    D_1a02,%ax
	mov    D_1a04,%dx
	mov    %ax,0x6(%di)
	mov    %dx,0x8(%di)
L_19c7:
	jmp   L_197a
L_19c9:
	cmpw   $0x0,0x8(%di)
	jne   L_19d5
	cmpw   $0x0,0x6(%di)
	je   L_19dc
L_19d5:
	cmpb   $0x5,D_19fe
	je   L_19c7
L_19dc:
	incw   -0xa(%bp)
	mov    D_19fe,%al
	mov    %al,0x2(%di)
	mov    D_1a02,%ax
	mov    D_1a04,%dx
	mov    %ax,0x6(%di)
	mov    %dx,0x8(%di)
	mov    D_19ff,%al
	mov    %al,0x3(%di)
	jmp   L_19c7
L_19fa:
	cmpw   $0x0,0x4(%bp)
	je   L_1a09
	cmpw   $0x0,-0xa(%bp)
	jne   L_1a09
	jmp   L_1bc7
L_1a09:
	mov    D_1800,%ax
	mov    D_1802,%dx
	/* TODO: data ref: cmp    0x21f8,%dx */
	cmp    D_01f8,%dx
	jl   L_1a22
	jg   L_1a1e
	/* TODO: data ref: cmp    0x21f6,%ax */
	cmp    D_01f6,%ax
	jbe   L_1a22
L_1a1e:
	incw   D_1844
L_1a22:
	mov    D_1834,%ax
	mov    D_1836,%dx
	add    D_1800,%ax
	adc    D_1802,%dx
	mov    %ax,D_1834
	mov    %dx,D_1836
	cmpw   $0x0,D_1844
	jne   L_1aa1
	mov    D_1846,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_174c(%bx),%ax
	mov    D_174e(%bx),%dx
	mov    %ax,-0x3e(%bp)
	mov    %dx,-0x3c(%bp)
	mov    D_1834,%ax
	mov    D_1836,%dx
	sub    -0x3e(%bp),%ax
	sbb    -0x3c(%bp),%dx
	/* TODO: data ref: cmp    0x21f8,%dx */
	cmp    D_01f8,%dx
	jl   L_1aa1
	jg   L_1a71
	/* TODO: data ref: cmp    0x21f6,%ax */
	cmp    D_01f6,%ax
	jbe   L_1aa1
L_1a71:
	mov    D_1834,%ax
	mov    D_1836,%dx
	sub    D_1800,%ax
	sbb    D_1802,%dx
	mov    %ax,-0x3e(%bp)
	mov    %dx,-0x3c(%bp)
	incw   D_1846
	mov    D_1846,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    -0x3e(%bp),%ax
	mov    -0x3c(%bp),%dx
	mov    %ax,D_174c(%bx)
	mov    %dx,D_174e(%bx)
L_1aa1:
	mov    $0x2,%ax
	push   %ax
	pushw  D_1806
	pushw  D_1804
	call   L_32b3
	add    $0x6,%sp
	mov    %ax,-0x3e(%bp)
	mov    %dx,-0x3c(%bp)
	mov    D_1830,%ax
	mov    D_1832,%dx
	add    -0x3e(%bp),%ax
	adc    -0x3c(%bp),%dx
	mov    %ax,D_1830
	mov    %dx,D_1832
	mov    $0x2,%ax
	push   %ax
	pushw  D_180a
	pushw  D_1808
	call   L_32b3
	add    $0x6,%sp
	mov    %ax,-0x3e(%bp)
	mov    %dx,-0x3c(%bp)
	mov    D_17f0,%ax
	mov    D_17f2,%dx
	add    -0x3e(%bp),%ax
	adc    -0x3c(%bp),%dx
	mov    %ax,D_17f0
	mov    %dx,D_17f2
	mov    -0xc(%bp),%ax
	cwtd
	mov    %ax,-0x3e(%bp)
	mov    %dx,-0x3c(%bp)
	mov    D_1828,%ax
	mov    D_182a,%dx
	add    -0x3e(%bp),%ax
	adc    -0x3c(%bp),%dx
	mov    %ax,D_1828
	mov    %dx,D_182a
	mov    D_1e30,%ax
	mov    D_1e32,%dx
	add    D_1814,%ax
	adc    D_1816,%dx
	mov    %ax,D_1e30
	mov    %dx,D_1e32
	mov    D_1e2c,%ax
	mov    D_1e2e,%dx
	add    D_1818,%ax
	adc    D_181a,%dx
	mov    %ax,D_1e2c
	mov    %dx,D_1e2e
	mov    -0x12(%bp),%ax
	add    %ax,D_17ec
	pushw  D_1e3e
	call   L_1f49
	pop    %cx
	mov    %ax,-0x10(%bp)
	pushw  -0x6(%bp)
	call   L_1f49
	pop    %cx
	mov    %ax,%si
L_1b5e:
	cmp    -0x10(%bp),%si
	jge   L_1b98
	mov    %si,%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    %dx,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,-0x3e(%bp)
	mov    %si,%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,%bx
	mov    D_184c(%bx),%ax
	add    -0x3e(%bp),%ax
	mov    %ax,%di
	pushw  (%di)
	call   L_1c3e
	pop    %cx
	mov    %ax,(%di)
	inc    %si
	jmp   L_1b5e
L_1b98:
	pushw  D_1848
	call   L_56c8
	pop    %cx
	cmpw   $0xffff,-0x14(%bp)
	jne   L_1bc2
	mov    D_19e4,%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	mov    D_1800,%ax
	mov    D_1802,%dx
	mov    %ax,D_1a0c(%bx)
	mov    %dx,D_1a0e(%bx)
	incw   D_19e4
L_1bc2:
	mov    $0x1,%ax
L_1bc5:
	jmp   L_1c37
L_1bc7:
	pushw  -0x6(%bp)
	call   L_1e19
	pop    %cx
	pushw  D_1848
	call   L_56c8
	pop    %cx
	cmpw   $0xffff,-0x14(%bp)
	jne   L_1c35
	xor    %si,%si
L_1bde:
	cmp    $0x2,%si
	jge   L_1c35
	pushw  -0x16(%bp)
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: pushw  0x21fa(%bx) */
	pushw  D_01fa(%bx)
	call   L_57d2
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_1c32
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: movw   $0x0,0x2202(%bx) */
	movw   $0x0,D_0202(%bx)
	/* TODO: data ref: movw   $0x0,0x2204(%bx) */
	movw   $0x0,D_0204(%bx)
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: pushw  0x21fe(%bx) */
	pushw  D_01fe(%bx)
	call   L_56c8
	pop    %cx
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: movw   $0x0,0x2206(%bx) */
	movw   $0x0,D_0206(%bx)
L_1c32:
	inc    %si
	jmp   L_1bde
L_1c35:
	xor    %ax,%ax
L_1c37:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1c3e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	push   %di
	call   L_44de
	pop    %cx
	inc    %ax
	mov    %ax,%si
	cmp    D_19ec,%si
	jle   L_1c82
	/* TODO: data ref: movw   $0x2000,0x39ec */
	/* movw constant -> D_19ec */
	mov	$0x0000,%ax
	mov	%ax,D_19ec
	cmp    D_19ec,%si
	jle   L_1c65
	mov    %si,D_19ec
L_1c65:
	pushw  D_19ec
	call   L_557e
	pop    %cx
	mov    %ax,D_19de
	or     %ax,%ax
	jne   L_1c82
	/* TODO: data ref: mov    $0x2457,%ax */
	mov    $D_0457,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_1c82:
	push   %si
	push   %di
	pushw  D_19de
	call   L_454a
	add    $0x6,%sp
	mov    D_19de,%di
	add    %si,D_19de
	sub    %si,D_19ec
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ca0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	xor    %di,%di
	mov    D_19fc,%ax
	mov    %ax,-0x6(%bp)
L_1cb0:
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1ccb
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %di,%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%di
	incw   -0x6(%bp)
	jmp   L_1cb0
L_1ccb:
	mov    $0x89e,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    %di,%ax
	cwtd
	and    $0xffff,%ax
	and    $0x7fff,%dx
	push   %dx
	push   %ax
	call   L_4dcf
	add    $0x8,%sp
	mov    %ax,%di
	movw   $D_184c,-0xa(%bp)
L_1ceb:
	cmpw   $D_19dc,-0xa(%bp)
	jb   L_1cf5
	jmp   L_1e04
L_1cf5:
	mov    -0xa(%bp),%bx
	cmpw   $0x0,(%bx)
	jne   L_1d60
	mov    $0xa,%ax
	push   %ax
	mov    $0x44f,%ax
	push   %ax
	call   L_43b0
	add    $0x4,%sp
	mov    -0xa(%bp),%bx
	mov    %ax,(%bx)
	mov    $0x2,%ax
	push   %ax
	mov    $0x89e,%ax
	push   %ax
	call   L_43b0
	add    $0x4,%sp
	mov    -0xa(%bp),%bx
	mov    %ax,0x6(%bx)
	mov    -0xa(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_1d35
	mov    -0xa(%bp),%bx
	cmpw   $0x0,0x6(%bx)
	jne   L_1d43
L_1d35:
	/* TODO: data ref: mov    $0x2473,%ax */
	mov    $D_0473,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_1d43:
	mov    -0xa(%bp),%bx
	mov    (%bx),%ax
	add    $D_0b16,%ax
	mov    -0xa(%bp),%bx
	mov    %ax,0x2(%bx)
	mov    -0xa(%bp),%bx
	mov    0x6(%bx),%ax
	/* TODO: data ref: add    $0x113c,%ax */
	add    $(L_1142-6),%ax
	mov    -0xa(%bp),%bx
	mov    %ax,0x8(%bx)
L_1d60:
	mov    -0xa(%bp),%ax
	cmp    D_1a34,%ax
	jbe   L_1d6f
	mov    -0xa(%bp),%ax
	mov    %ax,D_1a34
L_1d6f:
	mov    -0xa(%bp),%bx
	mov    0x6(%bx),%ax
	mov    %di,%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%si
	movw   $0x1,-0xc(%bp)
L_1d82:
	cmpw   $0x0,(%si)
	jne   L_1d95
	mov    -0xa(%bp),%bx
	cmpw   $0x44f,0x4(%bx)
	je   L_1de8
	mov    %si,%ax
	jmp   L_1dc8
L_1d95:
	mov    (%si),%bx
	mov    (%bx),%ax
	mov    %ax,-0x8(%bp)
	mov    D_19fc,%ax
	mov    %ax,-0x6(%bp)
L_1da2:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_1dca
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	cmpb   $0x0,(%bx)
	jne   L_1da2
	mov    %si,%ax
L_1dc8:
	jmp   L_1e12
L_1dca:
	mov    -0xc(%bp),%ax
	shl    %ax
	add    %ax,%si
	addw   $0x2,-0xc(%bp)
	mov    -0xa(%bp),%bx
	cmp    0x8(%bx),%si
	jb   L_1de1
	/* TODO: data ref: sub    $0x113c,%si */
	sub    $0x113c,%si
L_1de1:
	cmpw   $0x89e,-0xc(%bp)
	jl   L_1d82
L_1de8:
	cmpw   $0x89e,-0xc(%bp)
	jle   L_1dfd
	/* TODO: data ref: mov    $0x2492,%ax */
	mov    $D_0492,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_1dfd:
	addw   $0xa,-0xa(%bp)
	jmp   L_1ceb
L_1e04:
	/* TODO: data ref: mov    $0x24a3,%ax */
	mov    $D_04a3,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_1e12:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e19:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_1a34,%di
L_1e22:
	cmp    $D_184c,%di
	jb   L_1e71
	mov    0x4(%di),%ax
	mov    $0xa,%dx
	imul   %dx
	add    (%di),%ax
	mov    %ax,%si
	cmp    0x4(%bp),%si
	jne   L_1e3f
	mov    %si,D_1e3e
	jmp   L_1e85
L_1e3f:
	sub    $0xa,%si
L_1e42:
	cmp    (%di),%si
	jb   L_1e67
	mov    0x4(%si),%ax
	shl    %ax
	add    0x6(%di),%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	decw   0x4(%di)
	cmp    0x4(%bp),%si
	jne   L_1e62
	mov    %si,D_1e3e
	jmp   L_1e85
L_1e62:
	sub    $0xa,%si
	jmp   L_1e42
L_1e67:
	sub    $0xa,%di
	subw   $0xa,D_1a34
	jmp   L_1e22
L_1e71:
	cmpw   $0x0,0x4(%bp)
	je   L_1e85
	/* TODO: data ref: mov    $0x24b9,%ax */
	mov    $D_04b9,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_1e85:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e89:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	mov    %ax,D_19fc
	movb   $0x1,D_19fe
	movw   $0x0,D_1a02
	movw   $0x0,D_1a04
	call   L_1ca0
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1eac:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	cmpw   $0x0,(%di)
	je   L_1ebd
	jmp   L_1f3e
L_1ebd:
	mov    D_1a34,%bx
	cmp    0x6(%bx),%di
	jb   L_1ecf
	mov    D_1a34,%bx
	cmp    0x8(%bx),%di
	jb   L_1edd
L_1ecf:
	/* TODO: data ref: mov    $0x24c7,%ax */
	mov    $D_04c7,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_1edd:
	mov    D_1a34,%bx
	mov    0x4(%bx),%ax
	mov    $0xa,%dx
	imul   %dx
	mov    D_1a34,%bx
	mov    (%bx),%dx
	add    %ax,%dx
	mov    %dx,%si
	mov    %dx,D_1a1a
	mov    %dx,(%di)
	mov    D_1a34,%bx
	incw   0x4(%bx)
	mov    D_19fc,%ax
	mov    %ax,(%si)
	mov    D_19fe,%al
	mov    %al,0x2(%si)
	mov    D_1a34,%bx
	mov    %di,%ax
	sub    0x6(%bx),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,0x4(%si)
	mov    D_1a02,%ax
	mov    D_1a04,%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
	mov    D_19ff,%al
	mov    %al,0x3(%si)
	mov    D_1a1a,%ax
	add    $0xa,%ax
	mov    %ax,D_1e3e
	mov    $0x1,%ax
	jmp   L_1f45
L_1f3e:
	mov    (%di),%ax
	mov    %ax,D_1a1a
	xor    %ax,%ax
L_1f45:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1f49:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,0x4(%bp)
	jne   L_1f58
	xor    %ax,%ax
	jmp   L_1fa1
L_1f58:
	mov    D_1a34,%di
L_1f5c:
	cmp    $D_184c,%di
	jb   L_1f93
	mov    0x4(%bp),%ax
	cmp    (%di),%ax
	jb   L_1f8e
	mov    0x4(%bp),%ax
	cmp    0x2(%di),%ax
	ja   L_1f8e
	mov    %di,%ax
	sub    $D_184c,%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	mov    $D_0b16,%dx
	imul   %dx
	add    0x4(%bp),%ax
	sub    (%di),%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	jmp   L_1fa1
L_1f8e:
	sub    $0xa,%di
	jmp   L_1f5c
L_1f93:
	/* TODO: data ref: mov    $0x24d3,%ax */
	mov    $D_04d3,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_1fa1:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1fa5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_19fe,%al
	cbtw
	and    $0xf,%ax
	jmp   L_200d
	mov    D_1a02,%ax
	mov    D_1a04,%dx
	add    D_1a20,%ax
	adc    D_1a22,%dx
	mov    %ax,D_1a02
	mov    %dx,D_1a04
	jmp   L_200b
	mov    D_1a02,%ax
	mov    D_1a04,%dx
	add    D_1840,%ax
	adc    D_1842,%dx
	mov    %ax,D_1a02
	mov    %dx,D_1a04
	jmp   L_200b
	mov    D_1a02,%ax
	mov    D_1a04,%dx
	add    D_19e0,%ax
	adc    D_19e2,%dx
	mov    %ax,D_1a02
	mov    %dx,D_1a04
	jmp   L_201a
	jmp   L_201a
L_1ffd:
	mov    D_19fe,%al
	cbtw
	test   $0x1,%ax
	je   L_201a
	movb   $0x3,D_19fe
L_200b:
	jmp   L_201a
L_200d:
	dec    %ax
	cmp    $0x8,%ax
	ja   L_1ffd
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2220(%bx)
L_201a:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_201e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	call   L_0939
	/* TODO: data ref: mov    $0x2506,%ax */
	mov    $D_0506,%ax
	push   %ax
	call   unlink
	pop    %cx
	movw   $0x0,-0x6(%bp)
L_2034:
	cmpw   $0x2,-0x6(%bp)
	jge   L_2051
	mov    -0x6(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: pushw  0x21fc(%bx) */
	pushw  D_01fc(%bx)
	call   unlink
	pop    %cx
	incw   -0x6(%bp)
	jmp   L_2034
L_2051:
	/* TODO: data ref: cmpw   $0x0,0x24de */
	cmpw   $0x0,D_04de
	jne   L_2066
	pushw  D_0efe
	/* TODO: data ref: pushw  0x2008 */
	pushw  D_0008
	call   L_4406
	add    $0x4,%sp
L_2066:
	/* TODO: data ref: pushw  0x24de */
	pushw  D_04de
	call   _exit
L_206d:
	pop    %cx
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2073:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	/* TODO: data ref: movw   $0x2104,0x21f4 */
	/* dual-reloc split: movw $D_0104,D_01f4 */
	mov	$D_0104,%ax
	mov	%ax,D_01f4
	movw   $0x0,D_0f00
	call   L_212e
	call   L_2d24
	mov    0x6(%bp),%ax
	inc    %ax
	inc    %ax
	mov    %ax,-0xc(%bp)
	mov    $0x1,%di
L_2098:
	cmp    0x4(%bp),%di
	jl   L_20a0
	jmp   L_2124
L_20a0:
	mov    -0xc(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	addw   $0x2,-0xc(%bp)
	mov    -0xa(%bp),%bx
	cmpb   $0x2d,(%bx)
	je   L_20bd
	pushw  -0xa(%bp)
	call   L_3466
	pop    %cx
	jmp   L_20f4
L_20bd:
	mov    $0x1,%si
L_20c0:
	mov    -0xa(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	je   L_20f4
	mov    -0xa(%bp),%ax
	add    %si,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	jmp   L_20f6
L_20d8:
	inc    %di
	addw   $0x2,-0xc(%bp)
	jmp   L_211d
L_20df:
	dec    %si
	mov    %si,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	movb   $0x2d,(%bx)
	mov    -0xa(%bp),%ax
	add    %si,%ax
	push   %ax
	call   L_3466
	pop    %cx
L_20f4:
	jmp   L_2120
L_20f6:
	cmp    $0x65,%ax
	je   L_20d8
	jg   L_210e
	cmp    $0x42,%ax
	je   L_20d8
	cmp    $0x44,%ax
	je   L_20d8
	cmp    $0x54,%ax
	je   L_20d8
	jmp   L_211d
L_210e:
	cmp    $0x6c,%ax
	je   L_20df
	cmp    $0x6f,%ax
	je   L_20d8
	cmp    $0x75,%ax
	je   L_20d8
L_211d:
	inc    %si
	jmp   L_20c0
L_2120:
	inc    %di
	jmp   L_2098
L_2124:
	call   L_30be
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_212e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x3a,%sp
	movw   $0x0,D_1824
	movw   $0x0,D_1826
	movw   $0x0,D_1820
	movw   $0x0,D_1822
	movw   $0x0,D_1a14
	movw   $0x0,D_1a16
	incw   D_1846
	mov    D_1846,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_1834,%ax
	mov    D_1836,%dx
	mov    %ax,D_174c(%bx)
	mov    %dx,D_174e(%bx)
	cmpw   $0x0,D_19dc
	je   L_218b
	pushw  D_1846
	/* TODO: data ref: mov    $0x250c,%ax */
	mov    $D_050c,%ax
	push   %ax
	call   L_44b4
	add    $0x4,%sp
L_218b:
	cmpw   $0x0,D_1832
	jl   L_21aa
	jg   L_219c
	cmpw   $0xfe70,D_1830
	jb   L_21aa
L_219c:
	/* TODO: data ref: mov    $0x2516,%ax */
	mov    $D_0516,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_21aa:
	cmpw   $0x0,D_1836
	jg   L_21d3
	jl   L_21bb
	cmpw   $0xfe70,D_1834
	ja   L_21d3
L_21bb:
	cmpw   $0x0,D_182c
	je   L_21d0
	/* TODO: data ref: mov    $0x252b,%ax */
	mov    $D_052b,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_21d0:
	jmp   L_283f
L_21d3:
	cmpw   $0x0,D_182c
	jne   L_21e8
	/* TODO: data ref: mov    $0x254e,%ax */
	mov    $D_054e,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_21e8:
	cmpw   $0x0,D_1844
	je   L_21fd
	/* TODO: data ref: mov    $0x2563,%ax */
	mov    $D_0563,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_21fd:
	movw   $0x1,D_19ee
	cmpw   $0x0,D_183c
	jne   L_221c
	/* TODO: data ref: mov    $0x2585,%ax */
	mov    $D_0585,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
	incw   D_183c
L_221c:
	cmpw   $0x0,D_1e3c
	jne   L_2237
	/* TODO: data ref: mov    $0x25ae,%ax */
	mov    $D_05ae,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
	/* TODO: data ref: movw   $0x2000,0x3e3c */
	/* movw constant -> D_1e3c */
	mov	$0x0000,%ax
	mov	%ax,D_1e3c
L_2237:
	cmpw   $0x0,D_19f0
	je   L_2258
	/* TODO: data ref: mov    $0x25e3,%ax */
	mov    $D_05e3,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
	movw   $0x0,D_19f2
	movw   $0x0,D_19f0
L_2258:
	movw   $0x0,D_1a20
	movw   $0x0,D_1a22
	movw   $0x0,-0x2e(%bp)
L_2269:
	cmpw   $0x2,-0x2e(%bp)
	jl   L_2272
	jmp   L_2494
L_2272:
	mov    -0x2e(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: cmpw   $0x0,0x2204(%bx) */
	cmpw   $0x0,D_0204(%bx)
	jne   L_2288
	/* TODO: data ref: cmpw   $0x0,0x2202(%bx) */
	cmpw   $0x0,D_0202(%bx)
L_2288:
	jne   L_228d
	jmp   L_248e
L_228d:
	/* TODO: data ref: pushw  0x2216 */
	pushw  D_0216
	mov    -0x2e(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: pushw  0x21fc(%bx) */
	pushw  D_01fc(%bx)
	call   L_449e
	add    $0x4,%sp
	mov    %ax,D_1e34
	cmp    $0xffff,%ax
	jne   L_22bb
	/* TODO: data ref: mov    $0x2619,%ax */
	mov    $D_0619,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_22bb:
	mov    -0x2e(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: mov    0x2202(%bx),%ax */
	mov    D_0202(%bx),%ax
	/* TODO: data ref: mov    0x2204(%bx),%dx */
	mov    D_0204(%bx),%dx
	mov    %ax,-0x3e(%bp)
	mov    %dx,-0x3c(%bp)
	mov    -0x2e(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: mov    0x2202(%bx),%ax */
	mov    D_0202(%bx),%ax
	/* TODO: data ref: mov    0x2204(%bx),%dx */
	mov    D_0204(%bx),%dx
	push   %ax
	mov    -0x2e(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: pushw  0x21fe(%bx) */
	pushw  D_01fe(%bx)
	pushw  D_1e34
	call   L_590a
	add    $0x6,%sp
	cwtd
	cmp    -0x3c(%bp),%dx
	jne   L_2309
	cmp    -0x3e(%bp),%ax
	je   L_2317
L_2309:
	/* TODO: data ref: mov    $0x2636,%ax */
	mov    $D_0636,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_2317:
	pushw  D_1e34
	call   L_5814
	pop    %cx
	cmp    $0xffff,%ax
	jne   L_2332
	/* TODO: data ref: mov    $0x264e,%ax */
	mov    $D_064e,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_2332:
	mov    -0x2e(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: mov    0x21fe(%bx),%ax */
	mov    D_01fe(%bx),%ax
	mov    %ax,-0x30(%bp)
	mov    -0x2e(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: mov    0x21fe(%bx),%ax */
	mov    D_01fe(%bx),%ax
	mov    %ax,-0x34(%bp)
	mov    -0x30(%bp),%bx
	mov    0x8(%bx),%ax
	mov    0xa(%bx),%dx
	mov    -0x30(%bp),%bx
	mov    0x4(%bx),%bx
	mov    0x6(%bx),%si
	add    $0x20,%bx
	adc    $0x0,%si
	add    %bx,%ax
	adc    %si,%dx
	mov    -0x30(%bp),%bx
	add    0x18(%bx),%ax
	adc    0x1a(%bx),%dx
	mov    -0x30(%bp),%bx
	add    0x1c(%bx),%ax
	adc    0x1e(%bx),%dx
	add    %ax,-0x34(%bp)
	mov    -0x34(%bp),%ax
	mov    %ax,-0x32(%bp)
	mov    -0x2e(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: mov    0x21fe(%bx),%ax */
	mov    D_01fe(%bx),%ax
	mov    %ax,-0x34(%bp)
	mov    -0x30(%bp),%bx
	mov    0x8(%bx),%ax
	mov    0xa(%bx),%dx
	mov    -0x30(%bp),%bx
	mov    0x4(%bx),%bx
	mov    0x6(%bx),%si
	add    $0x20,%bx
	adc    $0x0,%si
	add    %bx,%ax
	adc    %si,%dx
	mov    -0x30(%bp),%bx
	add    0x18(%bx),%ax
	adc    0x1a(%bx),%dx
	mov    -0x30(%bp),%bx
	add    0x1c(%bx),%ax
	adc    0x1e(%bx),%dx
	mov    -0x30(%bp),%bx
	add    0x10(%bx),%ax
	adc    0x12(%bx),%dx
	add    %ax,-0x34(%bp)
	mov    -0x34(%bp),%ax
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    -0x30(%bp),%bx
	pushw  0x12(%bx)
	pushw  0x10(%bx)
	call   L_4d7e
	add    $0x8,%sp
	mov    %ax,-0x2c(%bp)
	movw   $0x0,-0x1e(%bp)
L_23f7:
	mov    -0x1e(%bp),%ax
	cmp    -0x2c(%bp),%ax
	jge   L_243e
	mov    -0x32(%bp),%bx
	cmpb   $0x5,0x2(%bx)
	jne   L_2435
	mov    -0x32(%bp),%bx
	mov    (%bx),%ax
	add    -0x34(%bp),%ax
	push   %ax
	call   L_1e89
	pop    %cx
	push   %ax
	call   L_1eac
	pop    %cx
	mov    -0x32(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	add    D_1a20,%ax
	adc    D_1a22,%dx
	mov    D_1a1a,%bx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
L_2435:
	incw   -0x1e(%bp)
	addw   $0xa,-0x32(%bp)
	jmp   L_23f7
L_243e:
	mov    -0x2e(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: mov    0x2206(%bx),%ax */
	mov    D_0206(%bx),%ax
	inc    %ax
	mov    %ax,-0x1e(%bp)
L_2450:
	mov    -0x1e(%bp),%ax
	cmp    D_1846,%ax
	jg   L_2477
	mov    -0x30(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x6(%bx),%dx
	mov    -0x1e(%bp),%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	sub    %ax,D_174c(%bx)
	sbb    %dx,D_174e(%bx)
	incw   -0x1e(%bp)
	jmp   L_2450
L_2477:
	mov    -0x30(%bp),%bx
	mov    D_1a20,%ax
	mov    D_1a22,%dx
	add    0x4(%bx),%ax
	adc    0x6(%bx),%dx
	mov    %ax,D_1a20
	mov    %dx,D_1a22
L_248e:
	incw   -0x2e(%bp)
	jmp   L_2269
L_2494:
	mov    D_17ec,%ax
	mov    $0xc,%dx
	imul   %dx
	mov    %ax,-0x26(%bp)
	pushw  -0x26(%bp)
	call   L_557e
	pop    %cx
	mov    %ax,D_1e36
	or     %ax,%ax
	jne   L_24bb
	/* TODO: data ref: mov    $0x2665,%ax */
	mov    $D_0665,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_24bb:
	pushw  D_1e3e
	call   L_1f49
	pop    %cx
	mov    %ax,-0x20(%bp)
	mov    D_1e36,%ax
	mov    %ax,-0x22(%bp)
	movw   $0x0,-0x1e(%bp)
L_24d1:
	mov    -0x1e(%bp),%ax
	cmp    -0x20(%bp),%ax
	jl   L_24dc
	jmp   L_2605
L_24dc:
	mov    -0x1e(%bp),%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    %dx,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,-0x3e(%bp)
	mov    -0x1e(%bp),%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,%bx
	mov    D_184c(%bx),%ax
	add    -0x3e(%bp),%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	cmpb   $0x5,0x2(%bx)
	je   L_2514
	jmp   L_25ff
L_2514:
	mov    -0x22(%bp),%bx
	movw   $0x0,(%bx)
	mov    -0x6(%bp),%bx
	cmpb   $0xff,0x3(%bx)
	jne   L_252b
	mov    -0x22(%bp),%bx
	movw   $0xffff,(%bx)
L_252b:
	mov    -0x6(%bp),%bx
	mov    0x3(%bx),%al
	cbtw
	mov    %ax,-0x2e(%bp)
L_2535:
	cmpw   $0x0,-0x2e(%bp)
	jle   L_255c
	mov    -0x2e(%bp),%ax
	dec    %ax
	mov    %ax,%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	mov    D_1a0c(%bx),%ax
	mov    D_1a0e(%bx),%dx
	mov    -0x6(%bp),%bx
	sub    %ax,0x6(%bx)
	sbb    %dx,0x8(%bx)
	decw   -0x2e(%bp)
	jmp   L_2535
L_255c:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    -0x22(%bp),%bx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	mov    -0x22(%bp),%ax
	xor    %dx,%dx
	mov    -0x6(%bp),%bx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	mov    -0x22(%bp),%bx
	cmpw   $0x0,(%bx)
	jne   L_25eb
	movw   $0x0,D_17f4
L_258a:
	mov    D_17f4,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_174c(%bx),%ax
	mov    D_174e(%bx),%dx
	mov    -0x22(%bp),%bx
	cmp    0x8(%bx),%dx
	jg   L_25d7
	jl   L_25aa
	cmp    0x6(%bx),%ax
	ja   L_25d7
L_25aa:
	mov    -0x22(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    D_17f4,%bx
	lea    0x1(%bx),%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	cmp    D_174e(%bx),%dx
	jg   L_25d7
	jl   L_25cd
	cmp    D_174c(%bx),%ax
	jae   L_25d7
L_25cd:
	mov    -0x22(%bp),%bx
	mov    D_17f4,%ax
	mov    %ax,(%bx)
	jmp   L_25e4
L_25d7:
	incw   D_17f4
	mov    D_17f4,%ax
	cmp    D_1846,%ax
	jl   L_258a
L_25e4:
	mov    D_17f4,%ax
	cmp    D_1846,%ax
L_25eb:
	jne   L_25fb
	/* TODO: data ref: mov    $0x2684,%ax */
	mov    $D_0684,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_25fb:
	addw   $0xc,-0x22(%bp)
L_25ff:
	incw   -0x1e(%bp)
	jmp   L_24d1
L_2605:
	movw   $0x0,-0x2e(%bp)
L_260a:
	cmpw   $0x2,-0x2e(%bp)
	jge   L_265d
	mov    -0x2e(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: cmpw   $0x0,0x2204(%bx) */
	cmpw   $0x0,D_0204(%bx)
	jne   L_2626
	/* TODO: data ref: cmpw   $0x0,0x2202(%bx) */
	cmpw   $0x0,D_0202(%bx)
L_2626:
	je   L_263a
	mov    -0x2e(%bp),%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: pushw  0x21fe(%bx) */
	pushw  D_01fe(%bx)
	call   L_56c8
	pop    %cx
L_263a:
	mov    -0x2e(%bp),%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	mov    D_19e8,%ax
	mov    D_19ea,%dx
	add    D_1a0c(%bx),%ax
	adc    D_1a0e(%bx),%dx
	mov    %ax,D_19e8
	mov    %dx,D_19ea
	incw   -0x2e(%bp)
	jmp   L_260a
L_265d:
	movw   $0x2,D_181c
	movw   $0x0,D_1a08
	movw   $0x0,-0x1e(%bp)
L_266e:
	mov    -0x1e(%bp),%ax
	cmp    D_1846,%ax
	jl   L_267a
	jmp   L_2736
L_267a:
	movw   $0x19,-0x2a(%bp)
	movw   $0x0,-0x2c(%bp)
L_2684:
	mov    -0x2c(%bp),%ax
	cmp    -0x20(%bp),%ax
	jl   L_268f
	jmp   L_2730
L_268f:
	mov    -0x2c(%bp),%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    %dx,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,-0x3e(%bp)
	mov    -0x2c(%bp),%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,%bx
	mov    D_184c(%bx),%ax
	add    -0x3e(%bp),%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	cmpb   $0x5,0x2(%bx)
	jne   L_271b
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,-0x22(%bp)
	mov    %ax,%bx
	mov    (%bx),%ax
	cmp    -0x1e(%bp),%ax
	jne   L_272a
	cmpw   $0x0,D_1a08
	je   L_26eb
	mov    -0x24(%bp),%bx
	mov    -0x22(%bp),%ax
	mov    %ax,0xa(%bx)
	jmp   L_26f1
L_26eb:
	mov    -0x22(%bp),%ax
	mov    %ax,D_1a08
L_26f1:
	mov    -0x22(%bp),%ax
	mov    %ax,-0x24(%bp)
	mov    -0x22(%bp),%bx
	mov    D_181c,%ax
	mov    %ax,0x2(%bx)
	incw   -0x2a(%bp)
	cmpw   $0x18,-0x2a(%bp)
	jle   L_271d
	mov    -0x22(%bp),%bx
	movw   $0x1,0x4(%bx)
	addw   $0x8,D_181c
	movw   $0x0,-0x2a(%bp)
L_271b:
	jmp   L_272a
L_271d:
	addw   $0x5,D_181c
	mov    -0x22(%bp),%bx
	movw   $0x0,0x4(%bx)
L_272a:
	incw   -0x2c(%bp)
	jmp   L_2684
L_2730:
	incw   -0x1e(%bp)
	jmp   L_266e
L_2736:
	mov    -0x24(%bp),%bx
	movw   $0x0,0xa(%bx)
	movw   $0x0,D_1750
	movw   $0x0,D_1752
	movw   $0x1,-0x1e(%bp)
L_274f:
	mov    -0x1e(%bp),%ax
	cmp    D_1846,%ax
	jle   L_275b
	jmp   L_283f
L_275b:
	mov    -0x1e(%bp),%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_174c(%bx),%ax
	mov    D_174e(%bx),%dx
	mov    -0x1e(%bp),%bx
	lea    -0x1(%bx),%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	sub    D_174c(%bx),%ax
	sbb    D_174e(%bx),%dx
	mov    D_181c,%bx
	xor    %si,%si
	or     %bx,%bx
	jge   L_278a
	not    %si
L_278a:
	add    D_19e8,%bx
	adc    D_19ea,%si
	add    %ax,%bx
	adc    %dx,%si
	mov    -0x1e(%bp),%di
	mov    $0x3,%cx
	shl    %cl,%di
	mov    %bx,D_1750(%di)
	mov    %si,D_1752(%di)
	mov    $0x200,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    -0x1e(%bp),%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	pushw  D_1752(%bx)
	pushw  D_1750(%bx)
	call   L_4dcf
	add    $0x8,%sp
	mov	$512,%bx	/* sc: ?512 */
	xor    %si,%si
	sub    %ax,%bx
	sbb    %dx,%si
	mov    -0x1e(%bp),%ax
	dec    %ax
	mov    %ax,%di
	mov    $0x3,%cx
	shl    %cl,%di
	add    D_1750(%di),%bx
	adc    D_1752(%di),%si
	mov    -0x1e(%bp),%di
	mov    $0x3,%cx
	shl    %cl,%di
	add    %bx,D_1750(%di)
	adc    %si,D_1752(%di)
	cmpw   $0x0,D_19dc
	je   L_2839
	mov    -0x1e(%bp),%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	pushw  D_174e(%bx)
	pushw  D_174c(%bx)
	mov    -0x1e(%bp),%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	pushw  D_1752(%bx)
	pushw  D_1750(%bx)
	pushw  -0x1e(%bp)
	/* TODO: data ref: mov    $0x2695,%ax */
	mov    $D_0695,%ax
	push   %ax
	call   L_44b4
	add    $0xc,%sp
	pushw  D_19ea
	pushw  D_19e8
	pushw  D_181c
	/* TODO: data ref: mov    $0x26b1,%ax */
	mov    $D_06b1,%ax
	push   %ax
	call   L_44b4
	add    $0x8,%sp
L_2839:
	incw   -0x1e(%bp)
	jmp   L_274f
L_283f:
	/* TODO: data ref: mov    $0x26cc,%ax */
	mov    $D_06cc,%ax
	push   %ax
	call   L_1e89
	pop    %cx
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,D_17f6
	/* TODO: data ref: mov    $0x26d3,%ax */
	mov    $D_06d3,%ax
	push   %ax
	call   L_1e89
	pop    %cx
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,D_1a26
	/* TODO: data ref: mov    $0x26da,%ax */
	mov    $D_06da,%ax
	push   %ax
	call   L_1e89
	pop    %cx
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,D_1a24
	pushw  D_1e3e
	call   L_1f49
	pop    %cx
	mov    %ax,-0x20(%bp)
	cmpw   $0x0,D_19f2
	jne   L_28f4
	movw   $0x0,-0x1e(%bp)
L_2883:
	mov    -0x1e(%bp),%ax
	cmp    -0x20(%bp),%ax
	jge   L_28f4
	mov    -0x1e(%bp),%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    %dx,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,-0x3e(%bp)
	mov    -0x1e(%bp),%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,%bx
	mov    D_184c(%bx),%ax
	add    -0x3e(%bp),%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	cmpb   $0x1,0x2(%bx)
	jne   L_28f6
	mov    -0x6(%bp),%bx
	cmpw   $0x0,0x8(%bx)
	jne   L_28f6
	cmpw   $0x0,0x6(%bx)
	jne   L_28f6
	mov    -0x6(%bp),%ax
	cmp    D_1a24,%ax
	je   L_28f6
	mov    -0x6(%bp),%ax
	cmp    D_1a26,%ax
	je   L_28f6
	mov    -0x6(%bp),%ax
	cmp    D_17f6,%ax
	je   L_28f6
	incw   D_19f2
	movw   $0x0,D_1a18
L_28f4:
	jmp   L_28fb
L_28f6:
	incw   -0x1e(%bp)
	jmp   L_2883
L_28fb:
	cmpw   $0x0,D_19f2
	je   L_2908
	movw   $0x0,D_1a06
L_2908:
	movw   $0x0,-0xa(%bp)
	movw   $0x0,-0x8(%bp)
	mov    D_184c,%ax
	mov    %ax,D_19f4
	cmpw   $0x0,D_1a18
	jne   L_2929
	cmpw   $0x0,D_19f2
	je   L_2929
	jmp   L_2a92
L_2929:
	mov    $0x5,%ax
	push   %ax
	pushw  D_1836
	pushw  D_1834
	pushw  D_17f6
	call   L_32df
	add    $0x8,%sp
	mov    $0x7,%ax
	push   %ax
	pushw  D_1832
	pushw  D_1830
	pushw  D_1a26
	call   L_32df
	add    $0x8,%sp
	mov    $0x9,%ax
	push   %ax
	pushw  D_17f2
	pushw  D_17f0
	pushw  D_1a24
	call   L_32df
	add    $0x8,%sp
	pushw  D_19f4
	call   L_1f49
	pop    %cx
	mov    %ax,-0x1e(%bp)
L_2976:
	mov    -0x1e(%bp),%ax
	cmp    -0x20(%bp),%ax
	jl   L_2981
	jmp   L_2a92
L_2981:
	mov    -0x1e(%bp),%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    %dx,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,-0x3e(%bp)
	mov    -0x1e(%bp),%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,%bx
	mov    D_184c(%bx),%ax
	add    -0x3e(%bp),%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	mov    0x2(%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x1b(%bp)
	cmp    $0x1,%cl
	je   L_29c1
	jmp   L_2a50
L_29c1:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jne   L_29d8
	or     %ax,%ax
	je   L_2a50
L_29d8:
	cmpw   $0x0,-0xc(%bp)
	jl   L_29ed
	jg   L_29e6
	cmpw   $0x8,-0xe(%bp)
	jb   L_29ed
L_29e6:
	movw   $0x8,-0x1a(%bp)
	jmp   L_2a07
L_29ed:
	cmpw   $0x0,-0xc(%bp)
	jl   L_2a02
	jg   L_29fb
	cmpw   $0x4,-0xe(%bp)
	jb   L_2a02
L_29fb:
	movw   $0x4,-0x1a(%bp)
	jmp   L_2a07
L_2a02:
	movw   $0x2,-0x1a(%bp)
L_2a07:
	pushw  -0x1a(%bp)
	pushw  -0x8(%bp)
	pushw  -0xa(%bp)
	call   L_32b3
	add    $0x6,%sp
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
	mov    -0x6(%bp),%bx
	mov    -0xa(%bp),%ax
	mov    -0x8(%bp),%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	mov    -0x6(%bp),%bx
	movb   $0x13,0x2(%bx)
	mov    -0xa(%bp),%ax
	mov    -0x8(%bp),%dx
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    -0xa(%bp),%ax
	mov    -0x8(%bp),%dx
	add    -0xe(%bp),%ax
	adc    -0xc(%bp),%dx
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
L_2a50:
	mov    -0x1b(%bp),%al
	cbtw
	test   $0x1,%ax
	je   L_2a8c
	mov    -0x1b(%bp),%al
	cbtw
	test   $0x1e,%ax
	jne   L_2a8c
	mov    -0x1b(%bp),%al
	cbtw
	test   $0xe0,%ax
	je   L_2a8c
	mov    -0x6(%bp),%bx
	mov    -0x12(%bp),%ax
	mov    -0x10(%bp),%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	mov    -0x1b(%bp),%al
	cbtw
	and    $0xe0,%ax
	or     $0x13,%ax
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,0x2(%bx)
L_2a8c:
	incw   -0x1e(%bp)
	jmp   L_2976
L_2a92:
	cmpw   $0x0,D_1e24
	jne   L_2aea
	cmpw   $0x0,D_183c
	jne   L_2ade
	cmpw   $0x0,D_1e2a
	jne   L_2aba
	cmpw   $0x0,D_1e28
	jne   L_2aba
	mov    D_1e3c,%ax
	xor    %dx,%dx
	mov    %ax,D_1e28
	mov    %dx,D_1e2a
L_2aba:
	mov    $0x2,%ax
	push   %ax
	mov    D_1834,%ax
	mov    D_1836,%dx
	add    D_1e28,%ax
	adc    D_1e2a,%dx
	push   %dx
	push   %ax
	call   L_32b3
	add    $0x6,%sp
	mov    %ax,D_1e38
	mov    %dx,D_1e3a
	jmp   L_2aea
L_2ade:
	mov    D_1e3c,%ax
	xor    %dx,%dx
	mov    %ax,D_1e38
	mov    %dx,D_1e3a
L_2aea:
	mov    $0x2,%ax
	push   %ax
	pushw  -0x8(%bp)
	pushw  -0xa(%bp)
	call   L_32b3
	add    $0x6,%sp
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
	mov    D_1e28,%ax
	mov    D_1e2a,%dx
	mov    %ax,D_1824
	mov    %dx,D_1826
	mov    D_1e38,%ax
	mov    D_1e3a,%dx
	mov    %ax,D_1820
	mov    %dx,D_1822
L_2b1c:
	cmpw   $0x0,D_1e40
	jne   L_2b3a
	mov    D_1820,%ax
	mov    D_1822,%dx
	add    D_1830,%ax
	adc    D_1832,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_2b47
L_2b3a:
	mov    D_1a28,%ax
	mov    D_1a2a,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_2b47:
	mov    -0x16(%bp),%ax
	mov    -0x14(%bp),%dx
	add    -0xa(%bp),%ax
	adc    -0x8(%bp),%dx
	mov    %ax,D_1a14
	mov    %dx,D_1a16
	movw   $0x0,-0x18(%bp)
	pushw  D_1e3e
	call   L_1f49
	pop    %cx
	mov    %ax,-0x20(%bp)
	pushw  D_19f4
	call   L_1f49
	pop    %cx
	mov    %ax,-0x1e(%bp)
L_2b75:
	mov    -0x1e(%bp),%ax
	cmp    -0x20(%bp),%ax
	jl   L_2b80
	jmp   L_2cd7
L_2b80:
	mov    -0x1e(%bp),%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    %dx,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,-0x3e(%bp)
	mov    -0x1e(%bp),%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,%bx
	mov    D_184c(%bx),%ax
	add    -0x3e(%bp),%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	mov    0x2(%bx),%al
	cbtw
	and    $0x1f,%ax
	jmp   L_2cc4
	cmpw   $0x0,D_19f0
	jne   L_2bc6
	orw    $0x1,D_1e42
L_2bc6:
	cmpw   $0x0,D_19f0
	je   L_2bd4
	cmpw   $0x0,D_1a18
	je   L_2c20
L_2bd4:
	mov    -0x6(%bp),%bx
	cmpw   $0x0,0x8(%bx)
	jne   L_2c20
	cmpw   $0x0,0x6(%bx)
	jne   L_2c20
	mov    -0x6(%bp),%ax
	cmp    D_1a24,%ax
	je   L_2c20
	mov    -0x6(%bp),%ax
	cmp    D_17f6,%ax
	je   L_2c20
	mov    -0x6(%bp),%ax
	cmp    D_1a26,%ax
	je   L_2c4b
	cmpw   $0x0,-0x18(%bp)
	jne   L_2c0c
	/* TODO: data ref: mov    $0x26df,%ax */
	mov    $D_06df,%ax
	push   %ax
	call   L_44b4
	pop    %cx
L_2c0c:
	incw   -0x18(%bp)
	mov    -0x6(%bp),%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x26eb,%ax */
	mov    $D_06eb,%ax
	push   %ax
	call   L_44b4
	add    $0x4,%sp
	jmp   L_2c7d
L_2c20:
	jmp   L_2c7d
	cmpw   $0x0,D_19ee
	je   L_2c4d
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,-0x22(%bp)
	mov    %ax,%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	add    D_1824,%ax
	adc    D_1826,%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
L_2c4b:
	jmp   L_2c96
L_2c4d:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	add    D_1824,%ax
	adc    D_1826,%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	jmp   L_2c96
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	add    D_1820,%ax
	adc    D_1822,%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
L_2c7d:
	jmp   L_2cc2
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	add    D_1a14,%ax
	adc    D_1a16,%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
L_2c96:
	jmp   L_2cc2
	mov    -0x6(%bp),%bx
	mov    0x2(%bx),%al
	cbtw
	and    $0xe0,%ax
	or     $0x9,%ax
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	add    -0x16(%bp),%ax
	adc    -0x14(%bp),%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
L_2cc2:
	jmp   L_2cd1
L_2cc4:
	dec    %ax
	cmp    $0x12,%ax
	ja   L_2cd1
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x24e0(%bx)
L_2cd1:
	incw   -0x1e(%bp)
	jmp   L_2b75
L_2cd7:
	cmpw   $0x0,D_1a06
	jne   L_2ce5
	cmpw   $0x0,D_1a30
	je   L_2cf1
L_2ce5:
	movw   $0x0,D_1828
	movw   $0x0,D_182a
L_2cf1:
	mov    D_17f0,%ax
	mov    D_17f2,%dx
	add    -0xa(%bp),%ax
	adc    -0x8(%bp),%dx
	mov    %ax,D_17f0
	mov    %dx,D_17f2
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  D_182a
	pushw  D_1828
	call   L_4d7e
	add    $0x8,%sp
	mov    %ax,D_1a32
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2d24:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x20,%sp
	xor    %ax,%ax
	push   %ax
	call   L_58e2
L_2d32:
	pop    %cx
	not    %ax
	and    $0x1ff,%ax
L_2d38:
	mov    %ax,D_0efe
	and    $0x1b6,%ax
	push   %ax
	/* TODO: data ref: pushw  0x2008 */
	pushw  D_0008
	call   L_449e
	add    $0x4,%sp
	mov    %ax,D_0ee4
	cmp    $0x0,%ax
	jge   L_2d72
	/* TODO: data ref: mov    0x2008,%ax */
	mov    D_0008,%ax
	mov    %ax,D_0f00
	movb   $0x0,D_0ea4
	mov    D_1e44,%bx
	shl    %bx
	pushw  D_0a94(%bx)
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
	jmp   L_2d9e
L_2d72:
	lea    -0x24(%bp),%ax
	push   %ax
	pushw  D_0ee4
	call   L_5828
	add    $0x4,%sp
	testw  $0x49,-0x20(%bp)
	je   L_2d9e
	mov    -0x20(%bp),%ax
	and    $0x1b6,%ax
	push   %ax
	/* TODO: data ref: pushw  0x2008 */
	pushw  D_0008
	call   L_4406
	add    $0x4,%sp
	mov    -0x20(%bp),%ax
	mov    %ax,D_0efe
L_2d9e:
	/* dual-reloc split: movw $D_1e4c,D_0ee6 */
	mov	$D_1e4c,%ax
	mov	%ax,D_0ee6
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  D_0ee6
	call   L_07a9
	add    $0x6,%sp
	cmpw   $0x0,D_183c
	je   L_2dc0
	mov    $0x109,%ax
	jmp   L_2dc3
L_2dc0:
	mov    $0x107,%ax
L_2dc3:
	mov    %ax,D_17fc
	mov    D_1e3c,%ax
	mov    %ax,D_17fe
	cmpw   $0x0,D_19ee
	je   L_2ded
	mov    D_1846,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_1750(%bx),%ax
	mov    D_1752(%bx),%dx
	mov    %ax,D_1800
	mov    %dx,D_1802
	jmp   L_2e16
L_2ded:
	cmpw   $0x0,D_183c
	je   L_2dfd
	mov    D_1834,%ax
	mov    D_1836,%dx
	jmp   L_2e0f
L_2dfd:
	mov    $0x2,%ax
	push   %ax
	pushw  D_1836
	pushw  D_1834
	call   L_32b3
	add    $0x6,%sp
L_2e0f:
	mov    %ax,D_1800
	mov    %dx,D_1802
L_2e16:
	mov    D_1830,%ax
	mov    D_1832,%dx
	mov    %ax,D_1804
	mov    %dx,D_1806
	mov    D_17f0,%ax
	mov    D_17f2,%dx
	mov    %ax,D_1808
	mov    %dx,D_180a
	mov    D_1e30,%ax
	mov    D_1e32,%dx
	mov    %ax,D_1814
	mov    %dx,D_1816
	mov    D_1e2c,%ax
	mov    D_1e2e,%dx
	mov    %ax,D_1818
	mov    %dx,D_181a
	cmpw   $0x0,D_1a06
	je   L_2e5b
	xor    %ax,%ax
	mov    %ax,%dx
	jmp   L_2e77
L_2e5b:
	pushw  D_1e3e
	call   L_1f49
	pop    %cx
	mov    $0xa,%dx
	imul   %dx
	cwtd
	mov    D_1828,%bx
	mov    D_182a,%si
	add    %ax,%bx
	adc    %dx,%si
	mov    %bx,%ax
L_2e77:
	mov    %ax,D_180c
	mov    %dx,D_180e
	cmpw   $0x0,D_184a
	je   L_2eb1
	mov    D_184a,%bx
	cmpb   $0x5,0x2(%bx)
	je   L_2e9e
	/* TODO: data ref: mov    $0x26ef,%ax */
	mov    $D_06ef,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
	jmp   L_2edf
L_2e9e:
	mov    D_184a,%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,D_1810
	mov    %dx,D_1812
	jmp   L_2edf
L_2eb1:
	cmpw   $0x0,D_183c
	je   L_2ed3
	cmpw   $0x0,D_19ee
	je   L_2ecd
	mov    D_181c,%ax
	cwtd
	add    D_19e8,%ax
	adc    D_19ea,%dx
	jmp   L_2ed8
L_2ecd:
	xor    %ax,%ax
	mov    %ax,%dx
	jmp   L_2ed8
L_2ed3:
	mov    D_1e3c,%ax
	xor    %dx,%dx
L_2ed8:
	mov    %ax,D_1810
	mov    %dx,D_1812
L_2edf:
	cmpw   $0x0,D_19f2
	je   L_2eef
	mov    D_1e30,%ax
	mov    D_1e32,%dx
	jmp   L_2ef3
L_2eef:
	xor    %ax,%ax
	mov    %ax,%dx
L_2ef3:
	mov    %ax,D_1814
	mov    %dx,D_1816
	cmpw   $0x0,D_19f2
	je   L_2f0a
	mov    D_1e2c,%ax
	mov    D_1e2e,%dx
	jmp   L_2f0e
L_2f0a:
	xor    %ax,%ax
	mov    %ax,%dx
L_2f0e:
	mov    %ax,D_1818
	mov    %dx,D_181a
	pushw  D_0ee6
	mov    $0x20,%ax
	push   %ax
	mov    $D_17fc,%ax
	push   %ax
	call   L_0800
	add    $0x6,%sp
	mov    D_1800,%ax
	mov    D_1802,%dx
	add    $0x20,%ax
	adc    $0x0,%dx
	mov    %ax,D_2058
	mov    %dx,D_205a
	pushw  D_1806
	pushw  D_1804
	mov    $D_0e9c,%ax
	push   %ax
	call   L_3070
	add    $0x6,%sp
	cmpw   $0x0,D_19f2
	je   L_2f78
	pushw  D_1816
	pushw  D_1814
	mov    $D_0ee2,%ax
	push   %ax
	call   L_3070
	add    $0x6,%sp
	pushw  D_181a
	pushw  D_1818
	mov    $D_0ee0,%ax
	push   %ax
	call   L_3070
	add    $0x6,%sp
L_2f78:
	cmpw   $0x0,D_1a06
	je   L_2f86
	cmpw   $0x0,D_1a30
	jne   L_2fb2
L_2f86:
	pushw  D_180e
	pushw  D_180c
	mov    $D_0ef2,%ax
	push   %ax
	call   L_3070
	add    $0x6,%sp
	addw   $0x2,D_2058
	adcw   $0x0,D_205a
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    $D_0efc,%ax
	push   %ax
	call   L_3070
	add    $0x6,%sp
L_2fb2:
	cmpw   $0x0,D_19ee
	jne   L_2fbc
	jmp   L_3069
L_2fbc:
	cmpw   $0x0,D_19ea
	jne   L_2fc8
	cmpw   $0x0,D_19e8
L_2fc8:
	jne   L_2fcd
	jmp   L_3057
L_2fcd:
	pushw  D_19e8
	call   L_557e
	pop    %cx
	/* TODO: data ref: mov    %ax,0x21fe */
	mov    %ax,D_01fe
	or     %ax,%ax
	jne   L_2fea
	/* TODO: data ref: mov    $0x2707,%ax */
	mov    $D_0707,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_2fea:
	/* TODO: data ref: mov    0x21fe,%ax */
	mov    D_01fe,%ax
	/* TODO: data ref: mov    %ax,0x2200 */
	mov    %ax,D_0200
	mov    D_181c,%ax
	cwtd
	mov    %ax,D_1824
	mov    %dx,D_1826
	xor    %di,%di
L_2ffd:
	cmp    $0x2,%di
	jge   L_3057
	mov    %di,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: cmpw   $0x0,0x2204(%bx) */
	cmpw   $0x0,D_0204(%bx)
	jne   L_3017
	/* TODO: data ref: cmpw   $0x0,0x2202(%bx) */
	cmpw   $0x0,D_0202(%bx)
L_3017:
	je   L_3054
	mov    %di,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: pushw  0x21fc(%bx) */
	pushw  D_01fc(%bx)
	call   L_0585
	pop    %cx
	or     %ax,%ax
	je   L_303c
	/* TODO: data ref: mov    $0x270e,%ax */
	mov    $D_070e,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_303c:
	mov    $0x1,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	call   L_358b
	add    $0x6,%sp
	pushw  D_0ea0
	call   L_5814
	pop    %cx
L_3054:
	inc    %di
	jmp   L_2ffd
L_3057:
	movw   $0x0,D_1834
	movw   $0x0,D_1836
	/* movw constant -> D_17f4 */
	mov	$0xffff,%ax
	mov	%ax,D_17f4
L_3069:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3070:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0x20a,%ax
	push   %ax
	call   L_557e
	pop    %cx
	mov    %ax,(%di)
	cmp    $0x0,%ax
	jne   L_3096
	/* TODO: data ref: mov    $0x271d,%ax */
	mov    $D_071d,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_3096:
	pushw  D_205a
	pushw  D_2058
	pushw  (%di)
	call   L_07a9
	add    $0x6,%sp
	mov    D_2058,%ax
	mov    D_205a,%dx
	add    0x6(%bp),%ax
	adc    0x8(%bp),%dx
	mov    %ax,D_2058
	mov    %dx,D_205a
	pop    %di
	pop    %si
	pop    %bp
	ret


L_30be:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x1a,%sp
	cmpw   $0x0,D_1a06
	je   L_30d0
	jmp   L_31fc
L_30d0:
	pushw  D_1e3e
	call   L_1f49
	pop    %cx
	mov    %ax,-0x6(%bp)
	xor    %di,%di
L_30dd:
	cmp    -0x6(%bp),%di
	jl   L_30e5
	jmp   L_31e4
L_30e5:
	mov    %di,%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    %dx,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,-0xe(%bp)
	mov    %di,%ax
	cwtd
	mov    $0x44f,%cx
	idiv   %cx
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,%bx
	mov    D_184c(%bx),%ax
	add    -0xe(%bp),%ax
	mov    %ax,%si
	cmpw   $0x0,D_19ee
	je   L_3154
	cmpb   $0x5,0x2(%si)
	jne   L_3154
	mov    0x6(%si),%ax
	mov    %ax,-0xa(%bp)
	mov    %ax,%bx
	cmpw   $0xffff,(%bx)
	jne   L_3156
	mov    -0xa(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    D_181c,%ax
	cwtd
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	add    -0x12(%bp),%ax
	adc    -0x10(%bp),%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
L_3154:
	jmp   L_31d5
L_3156:
	mov    -0xa(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    D_181c,%ax
	cwtd
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_174c(%bx),%ax
	mov    D_174e(%bx),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_1750(%bx),%ax
	mov    D_1752(%bx),%dx
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	add    -0x12(%bp),%ax
	adc    -0x10(%bp),%dx
	add    D_19e8,%ax
	adc    D_19ea,%dx
	sub    -0x16(%bp),%ax
	sbb    -0x14(%bp),%dx
	mov    %ax,-0x1e(%bp)
	mov    %dx,-0x1c(%bp)
	mov    -0x1a(%bp),%ax
	mov    -0x18(%bp),%dx
	add    -0x1e(%bp),%ax
	adc    -0x1c(%bp),%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
L_31d5:
	pushw  D_0ef2
	push   %si
	call   L_0212
	add    $0x4,%sp
	inc    %di
	jmp   L_30dd
L_31e4:
	/* TODO: data ref: mov    0x2004,%ax */
	mov    D_0004,%ax
	mov    %ax,-0x8(%bp)
	pushw  D_0ef2
	mov    $0x2,%ax
	push   %ax
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_0800
	add    $0x6,%sp
L_31fc:
	cmpw   $0x0,D_0e9e
	jne   L_3231
	/* TODO: data ref: mov    $0x2736,%ax */
	mov    $D_0736,%ax
	push   %ax
	call   unlink
	pop    %cx
	/* TODO: data ref: mov    $0x2742,%ax */
	mov    $D_0742,%ax
	push   %ax
	/* TODO: data ref: mov    $0x273c,%ax */
	mov    $D_073c,%ax
	push   %ax
	call   L_4488
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_322b
	/* TODO: data ref: mov    $0x2748,%ax */
	mov    $D_0748,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_322b:
	/* TODO: data ref: movw   $0x2763,0x2008 */
	/* dual-reloc split: movw $D_0763,D_0008 */
	mov	$D_0763,%ax
	mov	%ax,D_0008
L_3231:
	mov    D_1e42,%ax
	/* TODO: data ref: mov    %ax,0x24de */
	mov    %ax,D_04de
	call   L_201e
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3241:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_1e42
	jne   L_3255
	mov    $D_0769,%ax
	push   %ax
	call   L_44b4
	pop    %cx
L_3255:
	cmpw   $0x0,D_0f00
	je   L_328d
	pushw  D_0f00
	mov    $D_076d,%ax
	push   %ax
	call   L_44b4
	add    $0x4,%sp
	cmpw   $0xffff,0x4(%bp)
	je   L_3285
	cmpb   $0x0,D_0ea4
	je   L_3285
	mov    $D_0ea4,%ax
	push   %ax
	mov    $D_0770,%ax
	push   %ax
	call   L_44b4
	add    $0x4,%sp
L_3285:
	mov    $D_0775,%ax
	push   %ax
	call   L_44b4
	pop    %cx
L_328d:
	pushw  0x6(%bp)
	mov    $D_0778,%ax
	push   %ax
	call   L_44b4
	add    $0x4,%sp
	cmpw   $0xffff,0x4(%bp)
	je   L_32af
	cmpw   $0x0,0x4(%bp)
	je   L_32a9
	call   L_201e
L_32a9:
	movw   $0x2,D_1e42
L_32af:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_32b3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	decw   0x8(%bp)
	mov    0x8(%bp),%ax
	xor    %dx,%dx
	add    %ax,0x4(%bp)
	adc    %dx,0x6(%bp)
	mov    0x8(%bp),%ax
	xor    %dx,%dx
	not    %ax
	not    %dx
	and    %ax,0x4(%bp)
	and    %dx,0x6(%bp)
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_32df:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	or     %di,%di
	je   L_332d
	cmpb   $0x1,0x2(%di)
	jne   L_32fe
	cmpw   $0x0,0x8(%di)
	jne   L_32fc
	cmpw   $0x0,0x6(%di)
L_32fc:
	je   L_3319
L_32fe:
	pushw  (%di)
	mov    $D_077c,%ax
	push   %ax
	call   L_44b4
	add    $0x4,%sp
	mov    $D_0781,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
	jmp   L_332d
L_3319:
	mov    0xa(%bp),%ax
	mov    %ax,%cx
	mov    %cl,0x2(%di)
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,0x6(%di)
	mov    %dx,0x8(%di)
L_332d:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3331:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x20,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	mov    $D_0f04,%ax
	push   %ax
	mov    $0x20,%ax
	push   %ax
	mov    $D_17fc,%ax
	push   %ax
	call   L_0066
	add    $0x6,%sp
	cmpw   $0x107,D_17fc
	je   L_3374
	cmpw   $0x109,D_17fc
	je   L_3374
	mov    $0x1,%ax
	jmp   L_3376
L_3374:
	xor    %ax,%ax
L_3376:
	cwtd
	or     %dx,%dx
	jne   L_337d
	or     %ax,%ax
L_337d:
	je   L_33a3
	cmpw   $0xff65,D_17fc
	jne   L_3395
	mov    $D_07af,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_3395:
	mov    $D_07bb,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_33a3:
	mov    D_1800,%ax
	mov    D_1802,%dx
	and    $0x1,%ax
	and    $0x0,%dx
	or     %dx,%dx
	jne   L_33b7
	or     %ax,%ax
L_33b7:
	jne   L_33cf
	mov    D_1804,%ax
	mov    D_1806,%dx
	and    $0x1,%ax
	and    $0x0,%dx
	or     %dx,%dx
	jne   L_33cd
	or     %ax,%ax
L_33cd:
	je   L_33dd
L_33cf:
	mov    $D_07cc,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_33dd:
	cmpw   $0x109,D_17fc
	jne   L_33ff
	movw   $0x0,D_1a20
	movw   $0x0,D_1a22
	movw   $0x0,D_1840
	movw   $0x0,D_1842
	jmp   L_343f
L_33ff:
	cmpw   $0x107,D_17fc
	jne   L_3431
	mov    D_17fe,%ax
	xor    %dx,%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	mov    %ax,D_1a20
	mov    %dx,D_1a22
	mov    D_1800,%ax
	mov    D_1802,%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	mov    %ax,D_1840
	mov    %dx,D_1842
	jmp   L_343f
L_3431:
	mov    $D_07df,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_343f:
	mov    D_17fe,%ax
	xor    %dx,%dx
	sub    %ax,D_1840
	sbb    %dx,D_1842
	mov    D_1840,%ax
	mov    D_1842,%dx
	sub    D_1804,%ax
	sbb    D_1806,%dx
	mov    %ax,D_19e0
	mov    %dx,D_19e2
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3466:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%di
	push   %di
	call   L_0585
	pop    %cx
	or     %ax,%ax
	jne   L_34a5
L_347a:
	cmpb   $0x0,(%di)
	je   L_3482
	inc    %di
	jmp   L_347a
L_3482:
	cmp    0x4(%bp),%di
	jb   L_348d
	dec    %di
	cmpb   $0x2f,(%di)
	jne   L_3482
L_348d:
	inc    %di
	push   %di
	call   L_42ce
	pop    %cx
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	call   L_358b
	add    $0x6,%sp
	jmp   L_357c
L_34a5:
	/* TODO: data ref: mov    0x21f4,%bx */
	mov    D_01f4,%bx
	mov    0x4(%bx),%ax
	/* TODO: data ref: mov    0x21f4,%bx */
	mov    D_01f4,%bx
	cmp    0x2(%bx),%ax
	jne   L_34d3
	/* TODO: data ref: mov    0x21f4,%bx */
	mov    D_01f4,%bx
	cmpw   $0xfa,0x2(%bx)
	jge   L_34ce
	mov    $D_08b0,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_34ce:
	/* TODO: data ref: addw   $0x6,0x21f4 */
	addw   $0x6,D_01f4
L_34d3:
	/* TODO: data ref: mov    0x21f4,%bx */
	mov    D_01f4,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    0x21f4,%bx */
	mov    D_01f4,%bx
	incw   0x4(%bx)
	mov    0x4(%bx),%dx
	dec    %dx
	mov    $0x2,%cx
	shl    %cl,%dx
	add    %dx,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    0x2(%bx),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmp    $0xffff,%dx
	jne   L_3503
	cmpw   $0xffff,-0x8(%bp)
	je   L_357c
L_3503:
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	call   L_0036
	movw   $0x0,-0xa(%bp)
	cmpw   $0x0,D_19ea
	jne   L_352e
	cmpw   $0x0,D_19e8
L_352e:
	je   L_355a
	xor    %si,%si
L_3532:
	cmp    $0x2,%si
	jge   L_355a
	mov    %si,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	/* TODO: data ref: pushw  0x21fa(%bx) */
	pushw  D_01fa(%bx)
	mov    $D_0ea4,%ax
	push   %ax
	call   L_57d2
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_3557
	movw   $0x2,-0xa(%bp)
L_3557:
	inc    %si
	jmp   L_3532
L_355a:
	mov    $D_0ea4,%ax
	push   %ax
	call   L_42ce
	pop    %cx
	pushw  -0xa(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    $0x3c,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	call   L_358b
	add    $0x6,%sp
	jmp   L_34a5
L_357c:
	pushw  D_0ea0
	call   L_5814
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_358b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x12,%sp
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_3331
	add    $0x4,%sp
	cmpw   $0x0,0x8(%bp)
	je   L_35a8
	jmp   L_36c5
L_35a8:
	cmpw   $0x0,D_19ee
	jne   L_35b2
	jmp   L_36c5
L_35b2:
	mov    D_1834,%ax
	mov    D_1836,%dx
	add    D_1800,%ax
	adc    D_1802,%dx
	mov    %ax,D_1834
	mov    %dx,D_1836
	mov    D_17f4,%ax
	inc    %ax
	mov    %ax,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_1834,%ax
	mov    D_1836,%dx
	cmp    D_174e(%bx),%dx
	jge   L_35e3
	jmp   L_36c5
L_35e3:
	jg   L_35ee
	cmp    D_174c(%bx),%ax
	ja   L_35ee
	jmp   L_36c5
L_35ee:
	cmpw   $0xffff,D_17f4
	je   L_365a
	movw   $0x0,-0xa(%bp)
L_35fa:
	mov    D_17f4,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_1750(%bx),%ax
	mov    D_1752(%bx),%dx
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    D_17f4,%ax
	inc    %ax
	mov    %ax,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_1750(%bx),%ax
	mov    D_1752(%bx),%dx
	sub    -0x12(%bp),%ax
	sbb    -0x10(%bp),%dx
	sub    D_1824,%ax
	sbb    D_1826,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	mov    -0xa(%bp),%ax
	cwtd
	cmp    -0x14(%bp),%dx
	jg   L_365a
	jl   L_3648
	cmp    -0x16(%bp),%ax
	jae   L_365a
L_3648:
	pushw  D_0ee6
	xor    %ax,%ax
	push   %ax
	call   L_0a23
	add    $0x4,%sp
	incw   -0xa(%bp)
	jmp   L_35fa
L_365a:
	pushw  D_0ee6
	incw   D_17f4
	pushw  D_17f4
	call   L_418d
	add    $0x4,%sp
	cmpw   $0x0,D_19ea
	jne   L_3678
	cmpw   $0x0,D_19e8
L_3678:
	je   L_36b2
	movw   $0x0,-0xa(%bp)
	/* TODO: data ref: mov    0x21fe,%ax */
	mov    D_01fe,%ax
	/* TODO: data ref: mov    %ax,0x2200 */
	mov    %ax,D_0200
L_3685:
	mov    -0xa(%bp),%ax
	cwtd
	cmp    D_19ea,%dx
	jg   L_36b2
	jl   L_3697
	cmp    D_19e8,%ax
	jae   L_36b2
L_3697:
	pushw  D_0ee6
	/* TODO: data ref: mov    0x2200,%bx */
	mov    D_0200,%bx
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_0a23
	add    $0x4,%sp
	incw   -0xa(%bp)
	/* TODO: data ref: incw   0x2200 */
	incw   D_0200
	jmp   L_3685
L_36b2:
	mov    D_181c,%ax
	cwtd
	add    D_19e8,%ax
	adc    D_19ea,%dx
	mov    %ax,D_1824
	mov    %dx,D_1826
L_36c5:
	cmpw   $0x0,0x8(%bp)
	je   L_36d1
	cmpw   $0x1,0x8(%bp)
	jne   L_36e7
L_36d1:
	mov    D_1a20,%ax
	mov    D_1a22,%dx
	add    D_1824,%ax
	adc    D_1826,%dx
	mov    %ax,D_1a20
	mov    %dx,D_1a22
L_36e7:
	cmpw   $0x0,0x8(%bp)
	je   L_36f3
	cmpw   $0x2,0x8(%bp)
	jne   L_371f
L_36f3:
	mov    D_1840,%ax
	mov    D_1842,%dx
	add    D_1820,%ax
	adc    D_1822,%dx
	mov    %ax,D_1840
	mov    %dx,D_1842
	mov    D_19e0,%ax
	mov    D_19e2,%dx
	add    D_1a14,%ax
	adc    D_1a16,%dx
	mov    %ax,D_19e0
	mov    %dx,D_19e2
L_371f:
	movw   $0x0,-0xa(%bp)
L_3724:
	cmpw   $0x1f,-0xa(%bp)
	jge   L_373a
	mov    -0xa(%bp),%bx
	shl    %bx
	movw   $0x0,D_205c(%bx)
	incw   -0xa(%bp)
	jmp   L_3724
L_373a:
	/* dual-reloc split: movw $D_07f0,D_22f4 */
	mov	$D_07f0,%ax
	mov	%ax,D_22f4
	mov    D_22f4,%bx
	movw   $0x0,0x2(%bx)
	movw   $0xffff,-0x8(%bp)
	addw   $0x20,0x4(%bp)
	adcw   $0x0,0x6(%bp)
	mov    $0x2,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	add    D_1800,%ax
	adc    D_1802,%dx
	add    D_1804,%ax
	adc    D_1806,%dx
	add    D_1814,%ax
	adc    D_1816,%dx
	add    D_1818,%ax
	adc    D_181a,%dx
	add    D_180c,%ax
	adc    D_180e,%dx
	push   %dx
	push   %ax
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	mov    $D_0f04,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	lea    -0x6(%bp),%ax
	push   %ax
	call   L_0066
	add    $0x6,%sp
	mov    -0x6(%bp),%ax
	dec    %ax
	dec    %ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	add    D_1800,%ax
	adc    D_1802,%dx
	add    D_1804,%ax
	adc    D_1806,%dx
	add    D_1814,%ax
	adc    D_1816,%dx
	add    D_1818,%ax
	adc    D_181a,%dx
	add    D_180c,%ax
	adc    D_180e,%dx
	add    $0x2,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
	pushw  -0x6(%bp)
	call   L_557e
	pop    %cx
	mov    %ax,D_1848
	or     %ax,%ax
	jne   L_380e
	mov    $D_08bd,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_380e:
	mov    $D_0f04,%ax
	push   %ax
	mov    -0x6(%bp),%ax
	dec    %ax
	dec    %ax
	push   %ax
	mov    D_1848,%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_0066
	add    $0x6,%sp
	pushw  D_180e
	pushw  D_180c
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	add    D_1800,%ax
	adc    D_1802,%dx
	add    D_1804,%ax
	adc    D_1806,%dx
	add    D_1814,%ax
	adc    D_1816,%dx
	add    D_1818,%ax
	adc    D_181a,%dx
L_3852:
	push   %dx
	push   %ax
	mov    $D_0f04,%ax
	push   %ax
	call   L_0278
	add    $0xa,%sp
L_385e:
	cmpw   $0x0,D_0f0c
	jge   L_3868
	jmp   L_3abf
L_3868:
	jg   L_3874
	cmpw   $0x0,D_0f0a
	ja   L_3874
	jmp   L_3abf
L_3874:
	incw   -0x8(%bp)
	mov    $D_0f04,%ax
	push   %ax
	mov    $0xa,%ax
	push   %ax
	mov    $D_19fc,%ax
	push   %ax
	call   L_0066
	add    $0x6,%sp
	cmpw   $0x0,D_19fc
	je   L_38b7
	cmpw   $0x2,D_19fc
	jb   L_389f
	mov    D_19fc,%ax
	cmp    -0x6(%bp),%ax
	jb   L_38ad
L_389f:
	mov    $D_08e9,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_38ad:
	mov    D_1848,%ax
	add    D_19fc,%ax
	mov    %ax,D_19fc
L_38b7:
	mov    D_19fe,%al
	cbtw
	and    $0xf,%ax
	jmp   L_391a
	mov    D_1a02,%ax
	mov    D_1a04,%dx
	add    D_1a20,%ax
	adc    D_1a22,%dx
	mov    %ax,D_1a02
	mov    %dx,D_1a04
	jmp   L_3918
	mov    D_1a02,%ax
	mov    D_1a04,%dx
	add    D_1840,%ax
	adc    D_1842,%dx
	mov    %ax,D_1a02
	mov    %dx,D_1a04
	jmp   L_3918
	mov    D_1a02,%ax
	mov    D_1a04,%dx
	add    D_19e0,%ax
	adc    D_19e2,%dx
	mov    %ax,D_1a02
	mov    %dx,D_1a04
	jmp   L_3927
	jmp   L_3927
L_390a:
	mov    D_19fe,%al
	cbtw
	test   $0x1,%ax
	je   L_3927
	movb   $0x3,D_19fe
L_3918:
	jmp   L_3927
L_391a:
	dec    %ax
	cmp    $0x8,%ax
	ja   L_390a
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2890(%bx)
L_3927:
	mov    D_19fe,%al
	cbtw
	mov    %ax,-0xc(%bp)
	testw  $0x1,-0xc(%bp)
	jne   L_3977
	cmpw   $0x1,0x8(%bp)
L_3939:
	jne   L_393e
	jmp   L_385e
L_393e:
	cmpw   $0x0,D_1a06
L_3943:
	je   L_3948
	jmp   L_385e
L_3948:
	cmpw   $0x0,D_1a30
	jne   L_3943
	cmpw   $0x0,D_1a2e
	je   L_3966
	mov    D_19fc,%bx
	cmpb   $0x4c,(%bx)
	jne   L_3966
	testw  $0xe0,-0xc(%bp)
	je   L_3939
L_3966:
	pushw  D_0ef2
	mov    $D_19fc,%ax
	push   %ax
	call   L_0212
	add    $0x4,%sp
L_3974:
	jmp   L_385e
L_3977:
	call   L_1ca0
	mov    %ax,%bx
	mov    (%bx),%di
	or     %di,%di
	jne   L_3990
	mov    $D_0909,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_3990:
	cmpb   $0x1,D_19fe
	je   L_399a
	jmp   L_3a40
L_399a:
	mov    D_22f4,%bx
	cmpw   $0x64,0x2(%bx)
	jne   L_39c8
	addw   $0x4,D_22f4
	cmpw   $D_0890,D_22f4
	jne   L_39bf
	mov    $D_092a,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_39bf:
	mov    D_22f4,%bx
	movw   $0x0,0x2(%bx)
L_39c8:
	mov    D_22f4,%bx
	cmpw   $0x0,(%bx)
	jne   L_39f6
	mov    $0x258,%ax
	push   %ax
	call   L_557e
	pop    %cx
	mov    D_22f4,%bx
	mov    %ax,(%bx)
	mov    D_22f4,%bx
	cmpw   $0x0,(%bx)
	jne   L_39f6
	mov    $D_0940,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_39f6:
	mov    D_22f4,%bx
	incw   0x2(%bx)
	mov    0x2(%bx),%ax
	dec    %ax
	mov    $0x6,%dx
	imul   %dx
	mov    D_22f4,%bx
	mov    (%bx),%dx
	add    %ax,%dx
	mov    %dx,%si
	mov    -0x8(%bp),%ax
	mov    %ax,(%si)
	mov    %di,0x2(%si)
	mov    -0x8(%bp),%ax
	cwtd
	mov    $0x1f,%cx
	idiv   %cx
	mov    %dx,%bx
	shl    %bx
	mov    D_205c(%bx),%ax
	mov    %ax,0x4(%si)
	mov    -0x8(%bp),%ax
	cwtd
	mov    $0x1f,%cx
	idiv   %cx
	mov    %dx,%bx
	shl    %bx
	mov    %si,D_205c(%bx)
L_3a3d:
	jmp   L_3974
L_3a40:
	mov    D_19fe,%al
	cbtw
	test   $0xe0,%ax
	jne   L_3a3d
	cmpw   $0x0,D_19ee
	je   L_3a7f
	mov    D_19fe,%al
	cbtw
	mov    0x2(%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
L_3a60:
	je   L_3a3d
	pushw  D_19fc
	mov    $D_0958,%ax
	push   %ax
	call   L_44b4
	add    $0x4,%sp
	mov    $D_095d,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_3a7d:
	jmp   L_3a3d
L_3a7f:
	mov    D_19fe,%al
	cbtw
	mov    0x2(%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_3aa2
	mov    D_1a02,%ax
	mov    D_1a04,%dx
	cmp    0x8(%di),%dx
	jne   L_3aa2
	cmp    0x6(%di),%ax
	je   L_3a60
L_3aa2:
	pushw  D_19fc
	mov    $D_096e,%ax
	push   %ax
	call   L_44b4
	add    $0x4,%sp
	mov    $D_0973,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
	jmp   L_3a7d
L_3abf:
	cmpw   $0x0,0x8(%bp)
	jne   L_3b35
	xor    %ax,%ax
	push   %ax
	pushw  D_1802
	pushw  D_1800
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	mov    $D_0f04,%ax
	push   %ax
	call   L_03c2
	add    $0xc,%sp
	mov    $0x1,%ax
	push   %ax
	pushw  D_1816
	pushw  D_1814
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	add    D_1800,%ax
	adc    D_1802,%dx
	add    D_1804,%ax
	adc    D_1806,%dx
	push   %dx
	push   %ax
	mov    $D_0e8c,%ax
	push   %ax
	call   L_03c2
	add    $0xc,%sp
	pushw  D_0ee2
	pushw  D_0ee6
	mov    D_1824,%ax
	mov    D_1826,%dx
	sub    D_1e28,%ax
	sbb    D_1e2a,%dx
	push   %dx
	push   %ax
	pushw  D_1a22
	pushw  D_1a20
	call   L_3d34
	add    $0xc,%sp
L_3b35:
	cmpw   $0x1,0x8(%bp)
	jne   L_3ba9
	xor    %ax,%ax
	push   %ax
	pushw  D_1802
	pushw  D_1800
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	mov    $D_0f04,%ax
	push   %ax
	call   L_03c2
	add    $0xc,%sp
	mov    $0x1,%ax
	push   %ax
	pushw  D_1816
	pushw  D_1814
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	add    D_1800,%ax
	adc    D_1802,%dx
	add    D_1804,%ax
	adc    D_1806,%dx
	push   %dx
	push   %ax
	mov    $D_0e8c,%ax
	push   %ax
	call   L_03c2
	add    $0xc,%sp
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    D_1824,%ax
	mov    D_1826,%dx
	sub    D_1e28,%ax
	sbb    D_1e2a,%dx
	push   %dx
	push   %ax
	pushw  D_1a22
	pushw  D_1a20
	call   L_3d34
	add    $0xc,%sp
L_3ba9:
	cmpw   $0x0,0x8(%bp)
	je   L_3bb8
	cmpw   $0x2,0x8(%bp)
	je   L_3bb8
	jmp   L_3c3a
L_3bb8:
	xor    %ax,%ax
	push   %ax
	pushw  D_1806
	pushw  D_1804
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	add    D_1800,%ax
	adc    D_1802,%dx
	push   %dx
	push   %ax
	mov    $D_0f04,%ax
	push   %ax
	call   L_03c2
	add    $0xc,%sp
	mov    $0x1,%ax
	push   %ax
	pushw  D_181a
	pushw  D_1818
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	add    D_1800,%ax
	adc    D_1802,%dx
	add    D_1804,%ax
	adc    D_1806,%dx
	add    D_1814,%ax
	adc    D_1816,%dx
	push   %dx
	push   %ax
	mov    $D_0e8c,%ax
	push   %ax
	call   L_03c2
	add    $0xc,%sp
	pushw  D_0ee0
	pushw  D_0e9c
	mov    D_1820,%ax
	mov    D_1822,%dx
	sub    D_1e38,%ax
	sbb    D_1e3a,%dx
	push   %dx
	push   %ax
	pushw  D_1842
	pushw  D_1840
	call   L_3d34
	add    $0xc,%sp
L_3c3a:
	/* movw constant -> D_1332 */
	mov	$0xffff,%ax
	mov	%ax,D_1332
	/* movw constant -> D_0f16 */
	mov	$0xffff,%ax
	mov	%ax,D_0f16
	movw   $0x0,D_1336
	movw   $0x0,D_0f1a
	/* dual-reloc split: movw $D_0ef4,D_0e9a */
	mov	$D_0ef4,%ax
	mov	%ax,D_0e9a
	/* dual-reloc split: movw $D_0ef4,D_0f12 */
	mov	$D_0ef4,%ax
	mov	%ax,D_0f12
	movw   $0x2,D_0efa
	cmpw   $0x0,0x8(%bp)
	je   L_3c70
	cmpw   $0x2,0x8(%bp)
	jne   L_3c9f
L_3c70:
	mov    D_1804,%ax
	mov    D_1806,%dx
	and    $0x1,%ax
	and    $0x0,%dx
	or     %dx,%dx
	jne   L_3c84
	or     %ax,%ax
L_3c84:
	je   L_3c9f
	pushw  D_0e9c
	xor    %ax,%ax
	push   %ax
	call   L_0a23
	add    $0x4,%sp
	addw   $0x1,D_1804
	adcw   $0x0,D_1806
	jmp   L_3c70
L_3c9f:
	cmpw   $0x0,0x8(%bp)
	je   L_3cab
	cmpw   $0x1,0x8(%bp)
	jne   L_3cc1
L_3cab:
	mov    D_1824,%ax
	mov    D_1826,%dx
	add    D_1800,%ax
	adc    D_1802,%dx
	mov    %ax,D_1824
	mov    %dx,D_1826
L_3cc1:
	cmpw   $0x0,0x8(%bp)
	je   L_3ccd
	cmpw   $0x2,0x8(%bp)
	jne   L_3d25
L_3ccd:
	mov    $0x2,%ax
	push   %ax
	pushw  D_1806
	pushw  D_1804
	call   L_32b3
	add    $0x6,%sp
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    D_1820,%ax
	mov    D_1822,%dx
	add    -0x12(%bp),%ax
	adc    -0x10(%bp),%dx
	mov    %ax,D_1820
	mov    %dx,D_1822
	mov    $0x2,%ax
	push   %ax
	pushw  D_180a
	pushw  D_1808
	call   L_32b3
	add    $0x6,%sp
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    D_1a14,%ax
	mov    D_1a16,%dx
	add    -0x12(%bp),%ax
	adc    -0x10(%bp),%dx
	mov    %ax,D_1a14
	mov    %dx,D_1a16
L_3d25:
	pushw  D_1848
	call   L_56c8
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3d34:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x1a,%sp
L_3d3c:
	pushw  D_19f2
	pushw  0xe(%bp)
	call   L_0ac7
	add    $0x4,%sp
	mov    %ax,-0xa(%bp)
	cmp    $0xffff,%ax
	jne   L_3d54
	jmp   L_4146
L_3d54:
	cmpw   $0x0,-0xa(%bp)
	jne   L_3d68
	mov    $D_0984,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_3d68:
	pushw  0xc(%bp)
	mov    -0xa(%bp),%bx
	pushw  0x2(%bx)
	pushw  (%bx)
	call   L_0c11
	add    $0x6,%sp
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_3d86
	cmpw   $0xffff,-0xc(%bp)
	jne   L_3d94
L_3d86:
	mov    $D_09a7,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_3d94:
	mov    -0xa(%bp),%bx
	mov    0x6(%bx),%ax
	sar    %ax
	and    $0x3,%ax
	jmp   L_3dde
L_3da1:
	mov    -0xc(%bp),%bx
	mov    (%bx),%al
	cbtw
	cwtd
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	jmp   L_3dee
L_3db0:
	mov    -0xc(%bp),%bx
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	jmp   L_3dee
L_3dbe:
	mov    -0xc(%bp),%bx
	mov    (%bx),%ax
	mov    0x2(%bx),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	jmp   L_3dee
L_3dce:
	mov    $D_09d1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
	jmp   L_3dee
L_3dde:
	or     %ax,%ax
	je   L_3da1
	cmp    $0x1,%ax
	je   L_3db0
	cmp    $0x2,%ax
	je   L_3dbe
	jmp   L_3dce
L_3dee:
	mov    -0xa(%bp),%bx
	mov    0x6(%bx),%ax
	mov    $0x3,%cx
	sar    %cl,%ax
	test   $0x1,%ax
	jne   L_3e01
	jmp   L_3f75
L_3e01:
	mov    -0xa(%bp),%bx
	mov    0x4(%bx),%ax
	cwtd
	mov    $0x1f,%cx
	idiv   %cx
	mov    %dx,%bx
	shl    %bx
	mov    D_205c(%bx),%si
L_3e15:
	mov    -0xa(%bp),%bx
	mov    (%si),%ax
	cmp    0x4(%bx),%ax
	je   L_3e36
	mov    0x4(%si),%si
	or     %si,%si
	jne   L_3e15
	mov    $D_09eb,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
	jmp   L_3e15
L_3e36:
	mov    0x2(%si),%di
	cmpb   $0x1,0x2(%di)
	jne   L_3e51
	push   %di
	call   L_1f49
	pop    %cx
	add    D_1a32,%ax
	mov    -0xa(%bp),%bx
	mov    %ax,0x4(%bx)
	jmp   L_3f72
L_3e51:
	mov    0x2(%di),%al
	cbtw
	and    $0x1e,%ax
	mov    -0xa(%bp),%bx
	mov    %ax,0x4(%bx)
	cmpw   $0x0,D_19ee
	jne   L_3e68
	jmp   L_3f58
L_3e68:
	cmpb   $0x5,0x2(%di)
	je   L_3e71
	jmp   L_3f58
L_3e71:
	mov    0x6(%di),%ax
	mov    %ax,-0xe(%bp)
	mov    %ax,%bx
	cmpw   $0xffff,(%bx)
	jne   L_3ebd
	mov    -0xe(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    D_181c,%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	mov    -0x12(%bp),%ax
	mov    -0x10(%bp),%dx
	add    -0x16(%bp),%ax
	adc    -0x14(%bp),%dx
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    -0x1a(%bp),%ax
	adc    -0x18(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	jmp   L_3ef2
L_3ebd:
	mov    -0xa(%bp),%bx
	mov    0x6(%bx),%ax
	test   $0x1,%ax
	je   L_3ed3
	mov    -0xe(%bp),%bx
	mov    (%bx),%ax
	cmp    D_17f4,%ax
	je   L_3ef4
L_3ed3:
	mov    -0xe(%bp),%bx
	mov    0x2(%bx),%ax
	cwtd
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    -0x12(%bp),%ax
	adc    -0x10(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_3ef2:
	jmp   L_3f56
L_3ef4:
	mov    -0xe(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    D_181c,%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	mov    D_17f4,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_174c(%bx),%ax
	mov    D_174e(%bx),%dx
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
	mov    -0x12(%bp),%ax
	mov    -0x10(%bp),%dx
	add    -0x16(%bp),%ax
	adc    -0x14(%bp),%dx
	add    D_19e8,%ax
	adc    D_19ea,%dx
	sub    -0x1a(%bp),%ax
	sbb    -0x18(%bp),%dx
	mov    %ax,-0x1e(%bp)
	mov    %dx,-0x1c(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    -0x1e(%bp),%ax
	adc    -0x1c(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_3f56:
	jmp   L_3f6a
L_3f58:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    0x6(%di),%ax
	adc    0x8(%di),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_3f6a:
	mov    -0xa(%bp),%bx
	andw   $0xfff7,0x6(%bx)
L_3f72:
	jmp   L_4019
L_3f75:
	mov    -0xa(%bp),%bx
	mov    0x4(%bx),%ax
	and    $0x1e,%ax
	jmp   L_4064
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    D_1a20,%ax
	adc    D_1a22,%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,D_19ee
	je   L_3fb1
	mov    -0xa(%bp),%bx
	mov    0x6(%bx),%ax
	test   $0x1,%ax
	jne   L_4019
	mov    D_1a08,%ax
	mov    %ax,-0xe(%bp)
L_3fad:
	cmpw   $0x0,-0xe(%bp)
L_3fb1:
	je   L_4019
	mov    -0xe(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    D_181c,%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	mov    D_17f4,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_174c(%bx),%ax
	mov    D_174e(%bx),%dx
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
	mov    -0x12(%bp),%ax
	mov    -0x10(%bp),%dx
	add    -0x16(%bp),%ax
	adc    -0x14(%bp),%dx
	add    D_19e8,%ax
	adc    D_19ea,%dx
	sub    -0x1a(%bp),%ax
	sbb    -0x18(%bp),%dx
	cmp    %ax,-0x8(%bp)
	jne   L_401b
	mov    -0xe(%bp),%bx
	mov    D_17f4,%ax
	cmp    (%bx),%ax
	jne   L_401b
	mov    -0xe(%bp),%bx
	mov    0x2(%bx),%ax
	cwtd
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_4019:
	jmp   L_4062
L_401b:
	mov    -0xe(%bp),%bx
	mov    0xa(%bx),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_3fad
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    D_1840,%ax
	adc    D_1842,%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	jmp   L_4062
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    D_19e0,%ax
	adc    D_19e2,%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	jmp   L_4072
	jmp   L_4072
L_4054:
	mov    $D_09fe,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_4062:
	jmp   L_4072
L_4064:
	dec    %ax
	dec    %ax
	cmp    $0x6,%ax
	ja   L_4054
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x28a2(%bx)
L_4072:
	mov    -0xa(%bp),%bx
	mov    0x6(%bx),%ax
	test   $0x1,%ax
	je   L_408f
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	sub    0x4(%bp),%ax
	sbb    0x6(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_408f:
	mov    -0xa(%bp),%bx
	mov    0x6(%bx),%ax
	sar    %ax
	and    $0x3,%ax
	jmp   L_4118
L_409c:
	cmpw   $0xffff,-0x6(%bp)
	jl   L_40b8
	jg   L_40aa
	cmpw   $0xff80,-0x8(%bp)
	jb   L_40b8
L_40aa:
	cmpw   $0x0,-0x6(%bp)
	jl   L_40c5
	jg   L_40b8
	cmpw   $0x7f,-0x8(%bp)
	jbe   L_40c5
L_40b8:
	mov    $D_0a25,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_40c5:
	mov    -0x8(%bp),%al
	cbtw
	mov    -0xc(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_4116
L_40d2:
	cmpw   $0x1,-0x6(%bp)
	jl   L_40fe
	jg   L_40e0
	cmpw   $0x0,-0x8(%bp)
	jbe   L_40fe
L_40e0:
	cmpw   $0x0,D_1802
	jg   L_40fe
	jl   L_40f1
	cmpw   $0x7fff,D_1800
	jae   L_40fe
L_40f1:
	mov    $D_0a40,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_40fe:
	mov    -0x8(%bp),%ax
	mov    -0xc(%bp),%bx
	mov    %ax,(%bx)
	jmp   L_4126
L_4108:
	mov    -0xc(%bp),%bx
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_4116:
	jmp   L_4126
L_4118:
	or     %ax,%ax
	je   L_409c
	cmp    $0x1,%ax
	je   L_40d2
	cmp    $0x2,%ax
	je   L_4108
L_4126:
	cmpw   $0x0,D_19f2
	jne   L_4130
	jmp   L_3d3c
L_4130:
	mov    -0xa(%bp),%bx
	mov    (%bx),%ax
	mov    0x2(%bx),%dx
	add    0x8(%bp),%ax
	adc    0xa(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
	jmp   L_3d3c
L_4146:
	cmpw   $0x0,D_19f2
	je   L_4163
	pushw  0xe(%bp)
	mov    D_0e96,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	push   %ax
	pushw  D_0e8c
	call   L_0800
	add    $0x6,%sp
L_4163:
	pushw  0xc(%bp)
	mov    $0xf618,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	call   L_0c11
	add    $0x6,%sp
	cmp    $0xffff,%ax
	je   L_4186
	mov    $D_0a5b,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_4186:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_418d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	pushw  0x6(%bp)
	mov    $0xf6cd,%ax
	push   %ax
	call   L_0a5a
	add    $0x4,%sp
	movw   $0x2,-0x8(%bp)
	mov    D_1a08,%ax
	mov    %ax,-0x6(%bp)
L_41ad:
	cmpw   $0x0,-0x6(%bp)
	jne   L_41b6
	jmp   L_42b0
L_41b6:
	addw   $0x3,-0x8(%bp)
	pushw  0x6(%bp)
	mov    $0xb8,%ax
	push   %ax
	call   L_0a23
	add    $0x4,%sp
	pushw  0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    D_181c,%bx
	xor    %si,%si
	or     %bx,%bx
	jge   L_41df
	not    %si
L_41df:
	add    %bx,%ax
	adc    %si,%dx
	add    D_19e8,%ax
	adc    D_19ea,%dx
	mov    -0x6(%bp),%bx
	mov    (%bx),%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	sub    D_174c(%bx),%ax
	sbb    D_174e(%bx),%dx
	push   %ax
	call   L_0a5a
	add    $0x4,%sp
	mov    -0x6(%bp),%bx
	cmpw   $0x1,0x4(%bx)
	je   L_4237
	addw   $0x2,-0x8(%bp)
	pushw  0x6(%bp)
	mov    $0xeb,%ax
	push   %ax
	call   L_0a23
	add    $0x4,%sp
	mov    -0xc(%bp),%ax
	sub    -0x8(%bp),%ax
	mov    %ax,-0xa(%bp)
	pushw  0x6(%bp)
	mov    -0xa(%bp),%ax
	cbtw
	push   %ax
	call   L_0a23
	add    $0x4,%sp
	jmp   L_42a4
L_4237:
	mov    -0x8(%bp),%ax
	mov    %ax,-0xc(%bp)
	addw   $0x3,-0x8(%bp)
	pushw  0x6(%bp)
	mov    $0xba,%ax
	push   %ax
	call   L_0a23
	add    $0x4,%sp
	mov    $0x10,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	mov    D_1750(%bx),%ax
	mov    D_1752(%bx),%dx
	mov    0x4(%bp),%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	sub    D_1750(%bx),%ax
	sbb    D_1752(%bx),%dx
	push   %dx
	push   %ax
	call   L_4d7e
	add    $0x8,%sp
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
	pushw  0x6(%bp)
	pushw  -0x10(%bp)
	call   L_0a5a
	add    $0x4,%sp
	addw   $0x2,-0x8(%bp)
	pushw  0x6(%bp)
	mov    $0xf5cd,%ax
	push   %ax
	call   L_0a5a
	add    $0x4,%sp
L_42a4:
	mov    -0x6(%bp),%bx
	mov    0xa(%bx),%ax
	mov    %ax,-0x6(%bp)
	jmp   L_41ad
L_42b0:
	mov    -0x8(%bp),%ax
	cmp    D_181c,%ax
	je   L_42c7
	mov    $D_0a84,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3241
	add    $0x4,%sp
L_42c7:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_42ce:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_1a06
	jne   L_4308
	cmpw   $0x0,D_1a30
	jne   L_4308
	mov    0x4(%bp),%ax
	mov    %ax,D_19fc
	movb   $0x4,D_19fe
	mov    D_1824,%ax
	mov    D_1826,%dx
	mov    %ax,D_1a02
	mov    %dx,D_1a04
	pushw  D_0ef2
	mov    $D_19fc,%ax
	push   %ax
L_4302:
	call   L_0212
	add    $0x4,%sp
L_4308:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_430c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	mov    0x4(%bp),%di
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
	xor    %si,%si
L_4324:
	mov    (%di),%al
	cbtw
	jmp   L_432d
L_4329:
	inc    %si
L_432a:
	inc    %di
	jmp   L_4346
L_432d:
	cmp    $0x9,%ax
	je   L_4343
	cmp    $0x20,%ax
	je   L_4343
	cmp    $0x2b,%ax
	je   L_432a
	cmp    $0x2d,%ax
	je   L_4329
	jmp   L_4346
L_4343:
	inc    %di
	jmp   L_4324
L_4346:
	cmpb   $0x30,(%di)
	jl   L_4390
	cmpb   $0x39,(%di)
	jg   L_4390
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	call   L_4cd5
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
	jmp   L_4346
L_4390:
	or     %si,%si
	je   L_43a3
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	jmp   L_43a9
L_43a3:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
L_43a9:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_43b0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%ax
	mulw   0x6(%bp)
	mov    %ax,0x4(%bp)
	pushw  0x4(%bp)
	call   L_557e
	pop    %cx
	mov    %ax,%di
	or     %di,%di
	jne   L_43d0
	xor    %ax,%ax
	jmp   L_43f1
L_43d0:
	mov    %di,%si
	mov    0x4(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
L_43e0:
	decw   -0x6(%bp)
	jl   L_43ef
	mov    %si,%bx
	inc    %si
	inc    %si
	movw   $0x0,(%bx)
	jmp   L_43e0
L_43ef:
	mov    %di,%ax
L_43f1:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_43f6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_56c8
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4406:
	push   %bp
	mov    %sp,%bp
	mov	$15,%bx	/* sc: chmod */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 15:chmod */
	int	$0xf1
	jcxz   L_441a
	mov    %cx,D_1e44
L_441a:
	pop    %bp
	ret


L_441c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	xor    %si,%si
	mov    %si,-0x6(%bp)
L_442b:
	mov    (%di),%al
	cbtw
	jmp   L_4436
L_4430:
	incw   -0x6(%bp)
L_4433:
	inc    %di
	jmp   L_444f
L_4436:
	cmp    $0x9,%ax
	je   L_444c
	cmp    $0x20,%ax
	je   L_444c
	cmp    $0x2b,%ax
	je   L_4433
	cmp    $0x2d,%ax
	je   L_4430
	jmp   L_444f
L_444c:
	inc    %di
	jmp   L_442b
L_444f:
	cmpb   $0x30,(%di)
	jl   L_4475
	cmpb   $0x39,(%di)
	jg   L_4475
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
	jmp   L_444f
L_4475:
	cmpw   $0x0,-0x6(%bp)
	je   L_4481
	mov    %si,%ax
	neg    %ax
	jmp   L_4483
L_4481:
	mov    %si,%ax
L_4483:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4488:
	push   %bp
	mov    %sp,%bp
	mov	$9,%bx	/* sc: link */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 9:link */
	int	$0xf1
	jcxz   L_449c
	mov    %cx,D_1e44
L_449c:
	pop    %bp
	ret


L_449e:
	push   %bp
	mov    %sp,%bp
	mov	$8,%bx	/* sc: creat */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 8:creat */
	int	$0xf1
	jcxz   L_44b2
	mov    %cx,D_1e44
L_44b2:
	pop    %bp
	ret


L_44b4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0de2,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_46e0
	add    $0x6,%sp
	mov    D_0de8,%al
	cbtw
	test   $0x20,%ax
	je   L_44d8
	mov    $0xffff,%ax
	jmp   L_44da
L_44d8:
	xor    %ax,%ax
L_44da:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_44de:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_44e9:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_44f4
	inc    %si
	jmp   L_44e9
L_44f4:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_44fa:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_4507:
	decw   0x8(%bp)
	jl   L_452b
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_452b
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_4507
	xor    %ax,%ax
	jmp   L_4546
L_452b:
	cmpw   $0x0,0x8(%bp)
	jge   L_4535
	xor    %ax,%ax
	jmp   L_4546
L_4535:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_4546:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_454a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
L_4562:
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_4598
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_4593
L_457b:
	incw   -0x6(%bp)
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_458e
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	jmp   L_457b
L_458e:
	mov    -0x8(%bp),%ax
	jmp   L_459b
L_4593:
	incw   -0x6(%bp)
	jmp   L_4562
L_4598:
	mov    -0x8(%bp),%ax
L_459b:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


signal:
	push   %bp
	mov    %sp,%bp
	mov    $0x16,%cx
	mov    0x4(%bp),%ax
	or     %ax,%ax
	jle   L_45e5
	cmp    $0x20,%ax
	jg   L_45e5
	mov    %ax,%cx
	shl    %ax
	add    $D_22fc,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    0x6(%bp),%ax
	mov    %ax,(%bx)
	cmp    $0x2,%ax
	jl     sys_signal
	mov    $0x6,%ax
	mul    %cx
	add    $D_25e8,%ax
sys_signal:
	mov    %ax,%dx
	mov    %cx,%ax
	mov	$48,%bx	/* sc: signal */
	/* v7: 48:signal */
	int	$0xf1
	cmp    $0x2,%ax
	jl   L_45e2
	pop    %ax
	jmp   L_45e3
L_45e2:
	pop    %bx
L_45e3:
	jcxz   L_45ec
L_45e5:
	mov    %cx,D_1e44
	mov    $0xffff,%ax
L_45ec:
	pop    %bp
	ret
L_45ee:
	push   %ax
	mov    $D_22fe,%ax
	jmp   L_4652
	push   %ax
	mov    $D_2300,%ax
	jmp   L_4652
	push   %ax
	mov    $D_2302,%ax
	jmp   L_4652
	push   %ax
	mov    $D_2304,%ax
	jmp   L_4652
	push   %ax
	mov    $D_2306,%ax
	jmp   L_4652
	push   %ax
	mov    $D_2308,%ax
	jmp   L_4652
	push   %ax
	mov    $D_230a,%ax
	jmp   L_4652
	push   %ax
	mov    $D_230c,%ax
	jmp   L_4652
	push   %ax
	mov    $D_230e,%ax
	jmp   L_4652
	push   %ax
	mov    $D_2310,%ax
	jmp   L_4652
	push   %ax
	mov    $D_2312,%ax
	jmp   L_4652
	push   %ax
	mov    $D_2314,%ax
	jmp   L_4652
	push   %ax
	mov    $D_2316,%ax
	jmp   L_4652
	push   %ax
	mov    $D_2318,%ax
	jmp   L_4652
	push   %ax
	mov    $D_231a,%ax
	jmp   L_4652
	push   %ax
	mov    $D_231c,%ax
	jmp   L_4652
	push   %ax
	mov    $D_231e,%ax
L_4652:
	jmp   L_46ac
	push   %ax
	mov    $D_2320,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_2322,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_2324,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_2326,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_2328,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_232a,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_232c,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_232e,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_2330,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_2332,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_2334,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_2336,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_2338,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_233a,%ax
	jmp   L_46ac
	push   %ax
	mov    $D_233c,%ax
L_46ac:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	sub    $D_22fc,%ax
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


lseek:
	push   %bp
	push   %si
	push	%di
	mov    %sp,%bp
	mov	$19,%bx	/* sc: lseek */
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    0xa(%bp),%cx
	mov    0xc(%bp),%si
	/* v7: 19:lseek */
	int	$0xf1
	jcxz   L_46dd
	mov    %cx,D_1e44
L_46dd:
	pop	%di
	pop    %si
	pop    %bp
	ret


L_46e0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_46f8:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_470a
	jmp   L_4ca7
L_470a:
	cmpb   $0x25,-0x5(%bp)
	je   L_471d
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_471b:
	jmp   L_46f8
L_471d:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_4748
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_52b8
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_4748:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_4761
	incb   -0xa6(%bp)
	inc    %di
L_4761:
	cmpb   $0x30,(%di)
	jne   L_476c
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_476c:
	cmpb   $0x2a,(%di)
	je   L_479c
	movw   $0x0,-0xe(%bp)
L_4776:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_47b2
	cmpw   $0x9,-0xc(%bp)
	jg   L_47b2
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_4776
L_479c:
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
L_47b2:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_47f9
	cmpb   $0x2a,(%di)
	je   L_47fb
	movw   $0x0,-0x10(%bp)
L_47cb:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_47f5
	cmpw   $0x9,-0xc(%bp)
	jg   L_47f5
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_47cb
L_47f5:
	addw   $0x30,-0xc(%bp)
L_47f9:
	jmp   L_4812
L_47fb:
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
L_4812:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_4c1c
L_481d:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_482e
L_4825:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_485e
	jmp   L_4857
L_482e:
	cmp    $0x64,%ax
	je   L_4860
	cmp    $0x6f,%ax
	je   L_4844
	cmp    $0x75,%ax
	je   L_4857
	cmp    $0x78,%ax
	je   L_4852
	jmp   L_4825
L_4844:
	movw   $0x8,-0x12(%bp)
	jmp   L_4857
L_484b:
	movw   $0x8,-0x12(%bp)
	jmp   L_485e
L_4852:
	movw   $0x10,-0x12(%bp)
L_4857:
	jmp   L_48a9
L_4859:
	movw   $0x10,-0x12(%bp)
L_485e:
	jmp   L_48b9
L_4860:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_487d
L_4870:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_487d:
	cmpw   $0x0,-0x14(%bp)
	jg   L_48c7
	jl   L_488b
	cmpw   $0x0,-0x16(%bp)
	jae   L_48c7
L_488b:
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
	jmp   L_48c7
L_48a9:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_48c7
L_48b9:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_48c7:
	cmpw   $0x0,-0x14(%bp)
	jne   L_48d1
	cmpw   $0x0,-0x16(%bp)
L_48d1:
	jne   L_48e2
	cmpw   $0x0,-0x10(%bp)
	jne   L_48e2
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_48e2:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_48e9:
	cmpw   $0x0,-0x14(%bp)
	jne   L_48f3
	cmpw   $0x0,-0x16(%bp)
L_48f3:
	je   L_493e
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_4e06
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_4914
	mov    $0x30,%ax
	jmp   L_4917
L_4914:
	mov    $0x57,%ax
L_4917:
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
	call   L_4cfe
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_48e9
L_493e:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_498d
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_493e
L_495c:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_4968:
	cmpw   $0x2,-0xc(%bp)
	jge   L_498d
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_4988
	decw   -0x8(%bp)
L_4988:
	incw   -0xc(%bp)
	jmp   L_4968
L_498d:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_4994:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_49ab
	movw   $0x0,-0xc(%bp)
L_49ab:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_49ba
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_49ba:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_52b8
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_49dd:
	jmp   L_471b
L_49e0:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_49f2
	movw   $D_0d2a,-0xa(%bp)
L_49f2:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_4a01
	movw   $0x7fff,-0xc(%bp)
L_4a01:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_4a07:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_4994
	decw   -0xc(%bp)
	jl   L_4994
	incw   -0x8(%bp)
	jmp   L_4a07
L_4a19:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_4a21:
	jmp   L_49dd
L_4a23:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_4a2f
	movw   $0x6,-0x10(%bp)
L_4a2f:
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
	call   L_5004
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_4a64
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_4a64:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_4a79
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_4a91
L_4a79:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_4a79
L_4a91:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_4aa4
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_4aa4:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_4ac3
L_4ab0:
	decw   -0xc(%bp)
	jl   L_4ac3
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_4ab0
L_4ac3:
	cmpw   $0x0,-0xc(%bp)
	jg   L_4acc
	jmp   L_498d
L_4acc:
	decw   -0xc(%bp)
	jge   L_4ad4
	jmp   L_498d
L_4ad4:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_4acc
L_4ae9:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_4af7
	movw   $0x6,-0x10(%bp)
	jmp   L_4afa
L_4af7:
	incw   -0x10(%bp)
L_4afa:
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
	call   L_4fd8
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_4b2f
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_4b2f:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_4b3a
	incw   -0x1a(%bp)
L_4b3a:
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
L_4b5c:
	decw   -0xc(%bp)
	jle   L_4b76
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_4b5c
L_4b76:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_4b93
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_4ba4
L_4b93:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_4ba4:
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
	jmp   L_498d
L_4bd5:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_4be1
	movw   $0x6,-0x10(%bp)
L_4be1:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_4e5c
	add    $0xc,%sp
	add    $0x8,%si
L_4bff:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_4bff
	decw   -0x8(%bp)
	jmp   L_498d
L_4c10:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_4c19:
	jmp   L_4a21
L_4c1c:
	cmp    $0x65,%ax
	jne   L_4c24
	jmp   L_4ae9
L_4c24:
	jg   L_4c65
	cmp    $0x4f,%ax
	jne   L_4c2e
	jmp   L_4844
L_4c2e:
	jg   L_4c45
	cmp    $0x25,%ax
	je   L_4c10
	cmp    $0x44,%ax
	jne   L_4c3d
	jmp   L_4860
L_4c3d:
	cmp    $0x4c,%ax
L_4c40:
	jne   L_4c45
	jmp   L_481d
L_4c45:
	cmp    $0x55,%ax
	jne   L_4c4d
	jmp   L_48a9
L_4c4d:
	cmp    $0x58,%ax
	jne   L_4c55
	jmp   L_4852
L_4c55:
	cmp    $0x63,%ax
	jne   L_4c5d
	jmp   L_495c
L_4c5d:
	cmp    $0x64,%ax
	jne   L_4c65
	jmp   L_4870
L_4c65:
	cmp    $0x6f,%ax
	jne   L_4c6d
	jmp   L_484b
L_4c6d:
	jg   L_4c84
	cmp    $0x66,%ax
	jne   L_4c77
	jmp   L_4a23
L_4c77:
	cmp    $0x67,%ax
	jne   L_4c7f
	jmp   L_4bd5
L_4c7f:
	cmp    $0x6c,%ax
	je   L_4c40
L_4c84:
	cmp    $0x72,%ax
	jne   L_4c8c
	jmp   L_4a19
L_4c8c:
	cmp    $0x73,%ax
	jne   L_4c94
	jmp   L_49e0
L_4c94:
	cmp    $0x75,%ax
	jne   L_4c9c
	jmp   L_48b9
L_4c9c:
	cmp    $0x78,%ax
	jne   L_4ca4
	jmp   L_4859
L_4ca4:
	jmp   L_4c19
L_4ca7:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_4ccb
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_52b8
	add    $0x8,%sp
L_4ccb:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_4e3a


L_4cd5:
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
	call   L_4e3a


L_4cfe:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_4d1c
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_4d1c:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_4d25:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_4d25
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_4d35:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_4d35
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_4cd5
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_4d76
	jne   L_4d6b
	or     $0x0,%ax
	je   L_4d76
L_4d6b:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_4d76:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_4e3a


L_4d7e:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_4d9a
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_4d9a:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_4db1
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_4db1:
	push   %dx
	push   %ax
	call   L_4cfe
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_4dcb
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_4dcb:
	ret
	call   L_4e3a


L_4dcf:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_4d7e
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_4cd5
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_4e3a


L_4e06:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_4cfe
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_4cd5
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_4e3a:
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


L_4e5c:
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
	call   L_4fd8
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_4e96
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_4e96:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_4e9d:
	cmpw   $0x0,-0xa(%bp)
	jle   L_4eb7
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_4eb7
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_4e9d
L_4eb7:
	cmpw   $0x0,-0x8(%bp)
	jl   L_4ec8
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_4ed4
L_4ec8:
	cmpw   $0x0,-0x8(%bp)
	jge   L_4ed2
	cmpw   $0xfffd,-0x8(%bp)
L_4ed2:
	jge   L_4f53
L_4ed4:
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
L_4eef:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_4f09
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_4eef
L_4f09:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_4f25
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_4f2b
L_4f25:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_4f2b:
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
	jmp   L_4fc3
L_4f53:
	cmpw   $0x0,-0x8(%bp)
	jg   L_4f75
	cmpb   $0x30,(%di)
	je   L_4f64
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_4f64:
	cmpw   $0x0,-0x8(%bp)
	jge   L_4f75
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_4f64
L_4f75:
	movw   $0x1,-0xa(%bp)
L_4f7a:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_4fa2
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
	jne   L_4f9d
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_4f9d:
	incw   -0xa(%bp)
	jmp   L_4f7a
L_4fa2:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_4fc3
L_4faa:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_4fbd
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_4faa
L_4fbd:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_4fc3:
	cmpb   $0x2e,-0x1(%si)
	jne   L_4fca
	dec    %si
L_4fca:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4fd8:
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
	call   L_502f
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_5004:
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
	call   L_502f
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_502f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_5042
	movw   $0x0,0xc(%bp)
L_5042:
	cmpw   $0x4f,0xc(%bp)
	jl   L_504d
	movw   $0x4e,0xc(%bp)
L_504d:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_0d32,%si
	int    $0xf4
	fldl   0x4(%bp)
	fcompl D_0d82
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_508a
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	int    $0xf4
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_508a:
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_5924
	int    $0xf4
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $D_0d82,-0x16(%bp)
	int    $0xf4
	fldl   -0xc(%bp)
	fcompl D_0d8a
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_50d5
	jmp   L_5182
L_50d5:
	movw   $D_0d82,-0x16(%bp)
L_50da:
	int    $0xf4
	fldl   -0xc(%bp)
	fcompl D_0d92
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_50f6
	jmp   L_5180
L_50f6:
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   -0xc(%bp)
	fdivl  D_0d9a
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_5924
	int    $0xf4
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   D_0da2
	fstl   -0x1e(%bp)
	fdivl  D_0daa
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  D_0db2
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
	jmp   L_50da
L_5169:
	cmpw   $D_0d82,-0x16(%bp)
	jae   L_51c3
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_5180:
	jmp   L_5169
L_5182:
	int    $0xf4
	fldl   0x4(%bp)
	fcompl D_0dba
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_51d2
L_519b:
	int    $0xf4
	fldl   0x4(%bp)
	fmull  D_0dc2
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl D_0dca
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_51c3:
	jae   L_51d2
	int    $0xf4
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_519b
L_51d2:
	mov    0xc(%bp),%ax
	add    $D_0d32,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_51e4
	add    %di,-0x16(%bp)
L_51e4:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_0d32,-0x16(%bp)
	jae   L_51fa
	movb   $0x0,D_0d32
	mov    $D_0d32,%ax
	jmp   L_5265
L_51fa:
	cmp    -0x16(%bp),%si
	ja   L_5256
	cmp    $D_0d82,%si
	jae   L_5256
	int    $0xf4
	fldl   0x4(%bp)
	fmull  D_0dd2
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_5924
	int    $0xf4
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	int    $0xf4
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
	jmp   L_51fa
L_5256:
	cmpw   $D_0d82,-0x16(%bp)
	jb   L_5267
	movb   $0x0,D_0d81
	mov    $D_0d32,%ax
L_5265:
	jmp   L_52b1
L_5267:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_526f:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_52ab
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_0d32,-0x16(%bp)
	jbe   L_528e
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_526f
L_528e:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_526f
	cmp    $D_0d32,%si
	jbe   L_52a8
	movb   $0x30,(%si)
L_52a8:
	inc    %si
	jmp   L_526f
L_52ab:
	movb   $0x0,(%si)
	mov    $D_0d32,%ax
L_52b1:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_52b8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_52c5:
	cmpw   $0x0,0x8(%bp)
	jl   L_52ce
	jmp   L_535c
L_52ce:
	cmpb   $0x2d,(%di)
	jne   L_530b
	cmpw   $0x30,0xc(%bp)
	jne   L_530b
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_52fa
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
	jmp   L_530a
L_52fa:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_53aa
	add    $0x4,%sp
L_530a:
	dec    %si
L_530b:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_5329
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
	jmp   L_5335
L_5329:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_53aa
	add    $0x4,%sp
L_5335:
	incw   0x8(%bp)
	jmp   L_52c5
L_533a:
	dec    %si
	jl   L_5370
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_535e
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
L_535c:
	jmp   L_533a
L_535e:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_53aa
	add    $0x4,%sp
	jmp   L_533a
L_5370:
	cmpw   $0x0,0x8(%bp)
	je   L_53a5
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_5394
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
	jmp   L_53a0
L_5394:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_53aa
	add    $0x4,%sp
L_53a0:
	decw   0x8(%bp)
	jmp   L_5370
L_53a5:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_53aa:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_53c5
	mov    $0xffff,%ax
	jmp   L_548c
L_53c5:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_53f8
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
	call   L_590a
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_547d
L_53f8:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_544a
	cmp    $D_0de2,%di
	jne   L_5423
	mov    D_0de9,%al
	cbtw
	push   %ax
	call   L_583e
	pop    %cx
	or     %ax,%ax
	je   L_5418
	orb    $0x4,0x6(%di)
	jmp   L_53c5
L_5418:
	movw   $D_2344,0x4(%di)
	movw   $D_2344,(%di)
	jmp   L_53c5
L_5423:
	mov    $0x200,%ax
	push   %ax
	call   L_557e
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_543a
	orb    $0x4,0x6(%di)
	jmp   L_53c5
L_543a:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_546c
L_544a:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_546c
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_590a
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_546c:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_547d:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_548e
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_548c:
	jmp   L_5491
L_548e:
	mov    0x4(%bp),%ax
L_5491:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_5498:
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
	jne   L_54e4
	mov    0x4(%di),%si
	or     %si,%si
	je   L_54e4
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_54e4
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_590a
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_54e4
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_54e6
L_54e4:
	xor    %ax,%ax
L_54e6:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_54eb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0dda,%di
L_54f3:
	cmp    D_0e7a,%di
	jae   L_5503
	push   %di
	call   L_5507
	pop    %cx
	add    $0x8,%di
	jmp   L_54f3
L_5503:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_5507:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_555a
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_555a
	push   %di
	call   L_5498
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_5814
	pop    %cx
	or     %ax,%ax
	jge   L_553c
	mov    $0xffff,%si
L_553c:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_554c
	pushw  0x4(%di)
	call   L_56c8
	pop    %cx
L_554c:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_555a
	movw   $0x0,0x4(%di)
L_555a:
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
	call   L_54eb
	pushw  0x4(%bp)
	call   L_5940
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_557e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_0e7c
	jne   L_55a9
	mov    $D_0e7e,%ax
	or     $0x1,%ax
	mov    %ax,D_0e7c
	mov    $D_0e7c,%ax
	or     $0x1,%ax
	mov    %ax,D_0e7e
	/* dual-reloc split: movw $D_0e7e,D_2746 */
	mov	$D_0e7e,%ax
	mov	%ax,D_2746
	/* dual-reloc split: movw $D_0e7c,D_2744 */
	mov	$D_0e7c,%ax
	mov	%ax,D_2744
L_55a9:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_2744,%di
L_55bd:
	movw   $0x0,D_0e80
L_55c3:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_55f2
L_55ca:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_55d9
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_55ca
L_55d9:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_55f2
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_55f2
	jmp   L_5699
L_55f2:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_55c3
	cmp    D_2746,%si
	jne   L_560b
	cmp    $D_0e7c,%di
	je   L_560f
L_560b:
	xor    %ax,%ax
	jmp   L_5647
L_560f:
	incw   D_0e80
	cmpw   $0x1,D_0e80
	jle   L_55c3
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_0e80
	xor    %ax,%ax
	push   %ax
	call   L_57ac
	pop    %cx
	mov    %ax,%si
	mov    D_0e80,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_5649
	xor    %ax,%ax
L_5647:
	jmp   L_565c
L_5649:
	mov    D_0e80,%ax
	shl    %ax
	push   %ax
	call   L_57ac
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_565e
	xor    %ax,%ax
L_565c:
	jmp   L_56c3
L_565e:
	mov    D_2746,%bx
	mov    %si,(%bx)
	mov    D_2746,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_567c
	mov    D_2746,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_2746,%bx
	mov    %ax,(%bx)
L_567c:
	mov    D_0e80,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_2746
	mov    $D_0e7c,%ax
	or     $0x1,%ax
	mov    D_2746,%bx
	mov    %ax,(%bx)
	jmp   L_55bd
L_5699:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_2744
	cmp    %ax,%si
	jbe   L_56b8
	mov    D_2744,%bx
	mov    (%bx),%ax
	mov    %ax,D_2748
	mov    D_2744,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_56b8:
	mov    D_2744,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_56c3:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_56c8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_2744
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
	je   L_56fd
	push   %di
	call   L_56c8
	pop    %cx
L_56fd:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_557e
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_571c
	cmp    %di,%si
	jne   L_5720
L_571c:
	mov    %si,%ax
	jmp   L_5788
L_5720:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_573f
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_573f:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_575d
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_573f
L_575d:
	cmp    %di,%si
	jae   L_5786
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_5786
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
	mov    D_2748,%ax
	mov    %ax,(%bx)
L_5786:
	mov    %si,%ax
L_5788:
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
	jcxz   L_57a3
	mov    %cx,D_1e44
	jmp   L_57aa
L_57a3:
	mov    0x4(%bp),%bx
	mov    %bx,D_0e82
L_57aa:
	pop    %bp
	ret


L_57ac:
	push   %bp
	mov    %sp,%bp
	mov	$17,%bx	/* sc: brk */
	mov    0x4(%bp),%ax
	add    D_0e82,%ax
	/* v7: 17:brk */
	int	$0xf1
	jcxz   L_57c3
	mov    %cx,D_1e44
	jmp   L_57d0
L_57c3:
	mov    D_0e82,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_0e82
L_57d0:
	pop    %bp
	ret


L_57d2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_57df:
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_57fe
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_57df
	xor    %ax,%ax
	jmp   L_580f
L_57fe:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_580f:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_5814:
	push   %bp
	mov    %sp,%bp
	mov	$6,%bx	/* sc: close */
	mov    0x4(%bp),%ax
	/* v7: 6:close */
	int	$0xf1
	jcxz   L_5825
	mov    %cx,D_1e44
L_5825:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_5828:
	push   %bp
	mov    %sp,%bp
	mov	$28,%bx	/* sc: fstat */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 28:fstat */
	int	$0xf1
	jcxz   L_583c
	mov    %cx,D_1e44
L_583c:
	pop    %bp
	ret


L_583e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_58af
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_585b
	xor    %ax,%ax
	jmp   L_585e
L_585b:
	mov    $0x1,%ax
L_585e:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


open:
	push   %bp
	mov    %sp,%bp
	mov	$5,%bx	/* sc: open */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 5:open */
	int	$0xf1
	jcxz   L_587a
	mov    %cx,D_1e44
L_587a:
	pop    %bp
	ret


read:
	push   %bp
	mov    %sp,%bp
	mov	$3,%bx	/* sc: read */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 3:read */
	int	$0xf1
	jcxz   L_5893
	mov    %cx,D_1e44
L_5893:
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
	call   L_58c8
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_58af:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_58c8
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_58c8:
	push   %bp
	mov    %sp,%bp
	mov	$54,%bx	/* sc: ioctl */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 54:ioctl */
	int	$0xf1
	jcxz   L_58df
	mov    %cx,D_1e44
L_58df:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_58e2:
	push   %bp
	mov    %sp,%bp
	mov	$60,%bx	/* sc: umask */
	mov    0x4(%bp),%ax
	/* v7: 60:umask */
	int	$0xf1
	jcxz   L_58f3
	mov    %cx,D_1e44
L_58f3:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


unlink:
	push   %bp
	mov    %sp,%bp
	mov	$10,%bx	/* sc: unlink */
	mov    0x4(%bp),%ax
	/* v7: 10:unlink */
	int	$0xf1
	jcxz   L_5907
	mov    %cx,D_1e44
L_5907:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_590a:
	push   %bp
	mov    %sp,%bp
	mov	$4,%bx	/* sc: write */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 4:write */
	int	$0xf1
	jcxz   L_5921
	mov    %cx,D_1e44
L_5921:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_5924:
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


L_5940:
	push   %bp
	mov    %sp,%bp
	mov	$1,%bx	/* sc: exit */
	mov    0x4(%bp),%ax
	/* v7: 1:exit */
	int	$0xf1
	jcxz   L_5951
	mov    %cx,D_1e44
L_5951:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


	.data

D_0000:
	.byte	0x00, 0x00	/* |..| */
D_0002:
	.byte	0xbf, 0x0f	/* |..| */
D_0004:
	.byte	0x02, 0x00	/* |..| */
D_0006:
	.byte	0x00, 0x00	/* |..| */
D_0008:
	.byte	0x0a	/* |.| */
	.ascii	" l.out\0"
D_0010:
	.ascii	"premature EOF\0"
D_001e:
	.ascii	"loader error; odd offset\0"
D_0037:
	.ascii	"botch: no pages\0"
D_0047:
	.ascii	"loader error odd offset\0"
D_005f:
	.ascii	"premature EOF\0"
D_006d:
	.ascii	"/usr/local/lib/libxxxxxxxxxxxxxxx\0"
D_008f:
	.byte	0x2d, 0x6c, 0x61, 0x00	/* |-la.| */
D_0093:
	.ascii	"/usr/lib/libxxxxxxxxxxxxxxx\0"
D_00af:
	.ascii	"cannot open\0"
D_00bb:
	.ascii	"premature EOF\0"
D_00c9:
	.byte	0x21, 0x3c, 0x61, 0x72, 0x63, 0x68, 0x3e, 0x0a	/* |!<arch>.| */
	.byte	0x00	/* |.| */
D_00d2:
	.ascii	"__.SYMDEF\0"
D_00dc:
	.ascii	"1output write error\0"
D_00f0:
	.ascii	"output write error\0"
	.byte	0x00	/* |.| */
D_0104:
	.byte	0x3c, 0x3a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |<:......| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_01f4:
	.byte	0x04, 0x21	/* |.!| */
D_01f6:
	.byte	0xd8, 0xd6	/* |..| */
D_01f8:
	.byte	0x00, 0x00	/* |..| */
D_01fa:
	.byte	0x32, 0x22	/* |2"| */
D_01fc:
	.byte	0x3a, 0x22	/* |:"| */
D_01fe:
	.byte	0x00, 0x00	/* |..| */
D_0200:
	.byte	0x00, 0x00	/* |..| */
D_0202:
	.byte	0x00, 0x00	/* |..| */
D_0204:
	.byte	0x00, 0x00	/* |..| */
D_0206:
	.byte	0x00, 0x00	/* |..| */
	.ascii	"B\"K\"\0"
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00	/* |.| */
D_0216:
	.byte	0x24, 0x01, 0x9f, 0x11, 0xb1, 0x11, 0x10, 0x12	/* |$.......| */
	.byte	0xa4, 0x13, 0xfb, 0x1f, 0xfd, 0x1f, 0xfd, 0x1f	/* |........| */
	.byte	0xb3, 0x1f, 0xb3, 0x1f, 0xcb, 0x1f, 0xcb, 0x1f	/* |........| */
	.byte	0xe3, 0x1f, 0xe3, 0x1f	/* |....| */
	.ascii	"arith.o\0"
	.ascii	"ld.tmp1\0"
	.ascii	"signal.o\0"
	.ascii	"ld.tmp2\0"
D_0253:
	.ascii	"-o where?\0"
D_025d:
	.ascii	"-u or -e: arg missing\0"
D_0273:
	.ascii	"-T, -D or -B: arg missing\0"
D_028d:
	.ascii	"-T: too late, text already loaded\0"
D_02af:
	.byte	0x2d, 0x78, 0x00	/* |-x.| */
D_02b2:
	.ascii	"bad flag\0"
D_02bb:
	.ascii	"warning: archive has no table of contents; add one using ranlib(1)\0"
D_02fe:
	.ascii	"ran out of memory (toc)\0"
D_0316:
	.ascii	"ran out of memory (tocstr)\0"
D_0331:
	.ascii	"mangled archive table of contents\0"
D_0353:
	.ascii	"warning: table of contents for archive is out of date; rerun ranlib(1)\0"
D_039a:
	.ascii	"too many files loaded from libraries\0"
D_03bf:
	.ascii	"ran out of memory (nextlibp)\0"
D_03dc:
	.ascii	"no namelist\0"
D_03e8:
	.ascii	"too small (old format .o?)\0"
D_0403:
	.ascii	"out of memory, pass1\0"
D_0418:
	.ascii	"out of memory for string table\0"
D_0437:
	.ascii	"bad string table index (pass 1)\0"
D_0457:
	.ascii	"ran out of memory (savestr)\0"
D_0473:
	.ascii	"out of memory for symbol table\0"
D_0492:
	.ascii	"hash table botch\0"
D_04a3:
	.ascii	"symbol table overflow\0"
D_04b9:
	.ascii	"symfree botch\0"
D_04c7:
	.ascii	"enter botch\0"
D_04d3:
	.ascii	"symx botch\0"
D_04de:
	.byte	0x04, 0x00, 0xb9, 0x2b, 0x20, 0x2c, 0x20, 0x2c	/* |...+ , ,| */
	.byte	0x20, 0x2c, 0x22, 0x2c, 0x20, 0x2c, 0x66, 0x2c	/* | ,", ,f,| */
	.byte	0x20, 0x2c, 0x7f, 0x2c, 0x20, 0x2c, 0x20, 0x2c	/* | ,., , ,| */
	.byte	0x20, 0x2c, 0x20, 0x2c, 0x20, 0x2c, 0x20, 0x2c	/* | , , , ,| */
	.byte	0x20, 0x2c, 0x20, 0x2c, 0x20, 0x2c, 0x98, 0x2c	/* | , , ,.,| */
D_0506:
	.ascii	"l.out\0"
D_050c:
	.byte	0x70, 0x67, 0x63, 0x6e, 0x74, 0x3d, 0x25, 0x64	/* |pgcnt=%d| */
	.byte	0x0a, 0x00	/* |..| */
D_0516:
	.ascii	"warning: data > 64kb\0"
D_052b:
	.ascii	"warning: code < 64kb -m not needed\0"
D_054e:
	.ascii	"warning: code > 64kb\0"
D_0563:
	.ascii	"Big code object module size error\0"
D_0585:
	.ascii	"warning: code > 64kb, default to -i flag\0"
D_05ae:
	.ascii	"warning: code > 64kb,using -z with 8192kb stack size\0"
D_05e3:
	.ascii	"warning: code > 64kb,relocation info lost, no -r flag\0"
D_0619:
	.ascii	"cannot create temporary file\0"
D_0636:
	.ascii	"error writing temp file\0"
D_064e:
	.ascii	"cannot close temp file\0"
D_0665:
	.ascii	"out of memory for symbol table\0"
D_0684:
	.ascii	"Bad symbol value\0"
D_0695:
	.byte	0x70, 0x67, 0x3d, 0x25, 0x64, 0x20, 0x70, 0x67	/* |pg=%d pg| */
	.byte	0x73, 0x69, 0x7a, 0x65, 0x3d, 0x25, 0x44, 0x20	/* |size=%D | */
	.byte	0x70, 0x67, 0x62, 0x72, 0x65, 0x61, 0x6b, 0x3d	/* |pgbreak=| */
	.byte	0x25, 0x44, 0x0a, 0x00	/* |%D..| */
D_06b1:
	.byte	0x78, 0x66, 0x65, 0x72, 0x20, 0x3d, 0x20, 0x25	/* |xfer = %| */
	.byte	0x78, 0x20, 0x20, 0x65, 0x78, 0x74, 0x72, 0x61	/* |x  extra| */
	.byte	0x74, 0x65, 0x78, 0x74, 0x20, 0x3d, 0x20, 0x25	/* |text = %| */
	.byte	0x58, 0x0a, 0x00	/* |X..| */
D_06cc:
	.ascii	"_etext\0"
D_06d3:
	.ascii	"_edata\0"
D_06da:
	.ascii	"_end\0"
D_06df:
	.byte	0x55, 0x6e, 0x64, 0x65, 0x66, 0x69, 0x6e, 0x65	/* |Undefine| */
	.byte	0x64, 0x3a, 0x0a, 0x00	/* |d:..| */
D_06eb:
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_06ef:
	.ascii	"entry point not in text\0"
D_0707:
	.ascii	"no mem\0"
D_070e:
	.ascii	"tmp file botch\0"
D_071d:
	.ascii	"ran out of memory (outb)\0"
D_0736:
	.ascii	"a.out\0"
D_073c:
	.ascii	"l.out\0"
D_0742:
	.ascii	"a.out\0"
D_0748:
	.ascii	"cannot move l.out to a.out\0"
D_0763:
	.ascii	"a.out\0"
D_0769:
	.byte	0x6c, 0x64, 0x3a, 0x00	/* |ld:.| */
D_076d:
	.byte	0x25, 0x73, 0x00	/* |%s.| */
D_0770:
	.ascii	"(%s)\0"
D_0775:
	.byte	0x3a, 0x20, 0x00	/* |: .| */
D_0778:
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_077c:
	.ascii	"%s: \0"
D_0781:
	.ascii	"user attempt to redfine loader-defined symbol\0"
D_07af:
	.ascii	"old archive\0"
D_07bb:
	.ascii	"bad magic number\0"
D_07cc:
	.ascii	"text/data size odd\0"
D_07df:
	.ascii	"bad magic number\0"
D_07f0:
	.byte	0x9c, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |.@......| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0890:
	.byte	0x08, 0x39, 0x0a, 0x39, 0x0a, 0x39, 0xc0, 0x38	/* |.9.9.9.8| */
	.byte	0xc0, 0x38, 0xd8, 0x38, 0xd8, 0x38, 0xf0, 0x38	/* |.8.8.8.8| */
	.byte	0xf0, 0x38, 0x52, 0x40, 0x54, 0x40, 0x81, 0x3f	/* |.8R@T@.?| */
	.byte	0x54, 0x40, 0x26, 0x40, 0x54, 0x40, 0x3c, 0x40	/* |T@&@T@<@| */
D_08b0:
	.ascii	"libseg botch\0"
D_08bd:
	.ascii	"out of memory reading string table (pass 2)\0"
D_08e9:
	.ascii	"bad string table index (pass 2)\0"
D_0909:
	.ascii	"internal error: symbol not found\0"
D_092a:
	.ascii	"local symbol overflow\0"
D_0940:
	.ascii	"out of memory (clocseg)\0"
D_0958:
	.ascii	"%s: \0"
D_095d:
	.ascii	"multiply defined\0"
D_096e:
	.ascii	"%s: \0"
D_0973:
	.ascii	"multiply defined\0"
D_0984:
	.ascii	"error reading reloc_info (load2td)\0"
D_09a7:
	.ascii	"error reading code or data segs (load2td)\0"
D_09d1:
	.ascii	"load2td botch: bad length\0"
D_09eb:
	.ascii	"local symbol botch\0"
D_09fe:
	.ascii	"relocation format botch (symbol type))\0"
D_0a25:
	.ascii	"byte displacement overflow\0"
D_0a40:
	.ascii	"word displacement overflow\0"
D_0a5b:
	.ascii	"error reading code or data seg (load2td)\0"
D_0a84:
	.ascii	"big code error\0"
	.byte	0x00	/* |.| */
D_0a94:
	.byte	0xdc, 0x2a, 0xe4, 0x2a, 0xee, 0x2a, 0x08, 0x2b	/* |.*.*.*.+| */
	.byte	0x18, 0x2b, 0x30, 0x2b, 0x3a, 0x2b, 0x54, 0x2b	/* |.+0+:+T+| */
	.byte	0x66, 0x2b, 0x78, 0x2b, 0x88, 0x2b, 0x94, 0x2b	/* |f+x+.+.+| */
	.byte	0xa6, 0x2b, 0xb6, 0x2b, 0xc8, 0x2b, 0xd4, 0x2b	/* |.+.+.+.+| */
	.byte	0xea, 0x2b, 0xfc, 0x2b, 0x08, 0x2c, 0x1a, 0x2c	/* |.+.+.,.,| */
	.byte	0x29, 0x2c, 0x39, 0x2c, 0x48, 0x2c, 0x59, 0x2c	/* |),9,H,Y,| */
	.byte	0x6d, 0x2c, 0x81, 0x2c, 0x92, 0x2c, 0xa1, 0x2c	/* |m,.,.,.,| */
	.byte	0xb0, 0x2c, 0xc8, 0x2c, 0xd5, 0x2c, 0xeb, 0x2c	/* |.,.,.,.,| */
	.byte	0xfa, 0x2c, 0x06, 0x2d, 0x19, 0x2d, 0x23, 0x00	/* |.,.-.-#.| */
	.ascii	"Error 0\0"
	.ascii	"Not owner\0"
	.ascii	"No such file or directory\0"
	.byte	0x4e, 0x6f, 0x20, 0x73, 0x75, 0x63, 0x68, 0x20	/* |No such | */
	.byte	0x70, 0x72, 0x6f, 0x63, 0x65, 0x73	/* |proces| */
D_0b16:
	.byte	0x73, 0x00	/* |s.| */
	.ascii	"Interrupted system call\0"
	.ascii	"I/O error\0"
	.ascii	"No such device or address\0"
	.ascii	"Arg list too long\0"
	.ascii	"Exec format error\0"
	.ascii	"Bad file number\0"
	.ascii	"No children\0"
	.ascii	"No more processes\0"
	.ascii	"Not enough core\0"
	.ascii	"Permission denied\0"
	.ascii	"Bad address\0"
	.ascii	"Block device required\0"
	.ascii	"Mount device busy\0"
	.ascii	"File exists\0"
	.ascii	"Cross-device link\0"
	.ascii	"No such device\0"
	.ascii	"Not a directory\0"
	.ascii	"Is a directory\0"
	.ascii	"Invalid argument\0"
	.ascii	"File table overflow\0"
	.ascii	"Too many open files\0"
	.ascii	"Not a typewriter\0"
	.ascii	"Text file busy\0"
	.ascii	"File too large\0"
	.ascii	"No space left on device\0"
	.ascii	"Illegal seek\0"
	.ascii	"Read-only file system\0"
	.ascii	"Too many links\0"
	.ascii	"Broken pipe\0"
	.ascii	"Argument too large\0"
	.ascii	"Result too large\0"
D_0d2a:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_0d32:
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
D_0d81:
	.byte	0x00	/* |.| */
D_0d82:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0d8a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0d92:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0d9a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0da2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_0daa:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_0db2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0dba:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0dc2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0dca:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_0dd2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0dda:
	.byte	0x44, 0x45, 0x00, 0x00, 0x44, 0x45, 0x01, 0x00	/* |DE..DE..| */
D_0de2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_0de8:
	.byte	0x02	/* |.| */
D_0de9:
	.byte	0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |.......| */
	.byte	0x06, 0x02	/* |..| possible ptr: DS:0x0206 */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0e7a:
	.word	D_0e7a	/* self-ref ptr: 0x2e7a */
D_0e7c:
	.byte	0x00, 0x00	/* |..| */
D_0e7e:
	.byte	0x00, 0x00	/* |..| */
D_0e80:
	.byte	0x00, 0x00	/* |..| */
D_0e82:
	.word	0x2750	/* _curbrk: bss_end=0x274a rounded to paragraph */

	.bss

D_0e84:	.space	2	/* bss word @ 0x0e84 */
	.space	6
D_0e8c:	.space	2	/* bss word @ 0x0e8c */
D_0e8e:	.space	2	/* bss word @ 0x0e8e */
D_0e90:	.space	2	/* bss word @ 0x0e90 */
D_0e92:	.space	2	/* bss word @ 0x0e92 */
D_0e94:	.space	2	/* bss word @ 0x0e94 */
D_0e96:	.space	2	/* bss word @ 0x0e96 */
	.space	2
D_0e9a:	.space	2	/* bss word @ 0x0e9a */
D_0e9c:	.space	2	/* bss word @ 0x0e9c */
D_0e9e:	.space	2	/* bss word @ 0x0e9e */
D_0ea0:	.space	2	/* bss word @ 0x0ea0 */
	.space	2
D_0ea4:	.space	2	/* bss word @ 0x0ea4 */
	.space	14
D_0eb4:	.space	2	/* bss word @ 0x0eb4 */
	.space	30
D_0ed4:	.space	2	/* bss word @ 0x0ed4 */
	.space	10
D_0ee0:	.space	2	/* bss word @ 0x0ee0 */
D_0ee2:	.space	2	/* bss word @ 0x0ee2 */
D_0ee4:	.space	2	/* bss word @ 0x0ee4 */
D_0ee6:	.space	2	/* bss word @ 0x0ee6 */
D_0ee8:	.space	2	/* bss word @ 0x0ee8 */
	.space	2
D_0eec:	.space	2	/* bss word @ 0x0eec */
D_0eee:	.space	2	/* bss word @ 0x0eee */
D_0ef0:	.space	2	/* bss word @ 0x0ef0 */
D_0ef2:	.space	2	/* bss word @ 0x0ef2 */
D_0ef4:	.space	2	/* bss word @ 0x0ef4 */
	.space	4
D_0efa:	.space	2	/* bss word @ 0x0efa */
D_0efc:	.space	2	/* bss word @ 0x0efc */
D_0efe:	.space	2	/* bss word @ 0x0efe */
D_0f00:	.space	2	/* bss word @ 0x0f00 */
	.space	2
D_0f04:	.space	2	/* bss word @ 0x0f04 */
D_0f06:	.space	2	/* bss word @ 0x0f06 */
D_0f08:	.space	2	/* bss word @ 0x0f08 */
D_0f0a:	.space	2	/* bss word @ 0x0f0a */
D_0f0c:	.space	2	/* bss word @ 0x0f0c */
D_0f0e:	.space	2	/* bss word @ 0x0f0e */
D_0f10:	.space	2	/* bss word @ 0x0f10 */
D_0f12:	.space	2	/* bss word @ 0x0f12 */
D_0f14:	.space	2	/* bss word @ 0x0f14 */
D_0f16:	.space	2	/* bss word @ 0x0f16 */
	.space	2
D_0f1a:	.space	2	/* bss word @ 0x0f1a */
	.space	1044
D_1330:	.space	2	/* bss word @ 0x1330 */
D_1332:	.space	2	/* bss word @ 0x1332 */
	.space	2
D_1336:	.space	2	/* bss word @ 0x1336 */
	.space	1044
D_174c:	.space	2	/* bss word @ 0x174c */
D_174e:	.space	2	/* bss word @ 0x174e */
D_1750:	.space	2	/* bss word @ 0x1750 */
D_1752:	.space	2	/* bss word @ 0x1752 */
	.space	152
D_17ec:	.space	2	/* bss word @ 0x17ec */
	.space	2
D_17f0:	.space	2	/* bss word @ 0x17f0 */
D_17f2:	.space	2	/* bss word @ 0x17f2 */
D_17f4:	.space	2	/* bss word @ 0x17f4 */
D_17f6:	.space	2	/* bss word @ 0x17f6 */
	.space	4
D_17fc:	.space	2	/* bss word @ 0x17fc */
D_17fe:	.space	2	/* bss word @ 0x17fe */
D_1800:	.space	2	/* bss word @ 0x1800 */
D_1802:	.space	2	/* bss word @ 0x1802 */
D_1804:	.space	2	/* bss word @ 0x1804 */
D_1806:	.space	2	/* bss word @ 0x1806 */
D_1808:	.space	2	/* bss word @ 0x1808 */
D_180a:	.space	2	/* bss word @ 0x180a */
D_180c:	.space	2	/* bss word @ 0x180c */
D_180e:	.space	2	/* bss word @ 0x180e */
D_1810:	.space	2	/* bss word @ 0x1810 */
D_1812:	.space	2	/* bss word @ 0x1812 */
D_1814:	.space	2	/* bss word @ 0x1814 */
D_1816:	.space	2	/* bss word @ 0x1816 */
D_1818:	.space	2	/* bss word @ 0x1818 */
D_181a:	.space	2	/* bss word @ 0x181a */
D_181c:	.space	2	/* bss word @ 0x181c */
	.space	2
D_1820:	.space	2	/* bss word @ 0x1820 */
D_1822:	.space	2	/* bss word @ 0x1822 */
D_1824:	.space	2	/* bss word @ 0x1824 */
D_1826:	.space	2	/* bss word @ 0x1826 */
D_1828:	.space	2	/* bss word @ 0x1828 */
D_182a:	.space	2	/* bss word @ 0x182a */
D_182c:	.space	2	/* bss word @ 0x182c */
	.space	2
D_1830:	.space	2	/* bss word @ 0x1830 */
D_1832:	.space	2	/* bss word @ 0x1832 */
D_1834:	.space	2	/* bss word @ 0x1834 */
D_1836:	.space	2	/* bss word @ 0x1836 */
D_1838:	.space	2	/* bss word @ 0x1838 */
D_183a:	.space	2	/* bss word @ 0x183a */
D_183c:	.space	2	/* bss word @ 0x183c */
	.space	2
D_1840:	.space	2	/* bss word @ 0x1840 */
D_1842:	.space	2	/* bss word @ 0x1842 */
D_1844:	.space	2	/* bss word @ 0x1844 */
D_1846:	.space	2	/* bss word @ 0x1846 */
D_1848:	.space	2	/* bss word @ 0x1848 */
D_184a:	.space	2	/* bss word @ 0x184a */
D_184c:	.space	2	/* bss word @ 0x184c */
	.space	398
D_19dc:	.space	2	/* bss word @ 0x19dc */
D_19de:	.space	2	/* bss word @ 0x19de */
D_19e0:	.space	2	/* bss word @ 0x19e0 */
D_19e2:	.space	2	/* bss word @ 0x19e2 */
D_19e4:	.space	2	/* bss word @ 0x19e4 */
	.space	2
D_19e8:	.space	2	/* bss word @ 0x19e8 */
D_19ea:	.space	2	/* bss word @ 0x19ea */
D_19ec:	.space	2	/* bss word @ 0x19ec */
D_19ee:	.space	2	/* bss word @ 0x19ee */
D_19f0:	.space	2	/* bss word @ 0x19f0 */
D_19f2:	.space	2	/* bss word @ 0x19f2 */
D_19f4:	.space	2	/* bss word @ 0x19f4 */
	.space	6
D_19fc:	.space	2	/* bss word @ 0x19fc */
D_19fe:	.space	2	/* bss word @ 0x19fe */
D_19ff:	.space	2	/* bss word @ 0x19ff */
	.space	1
D_1a02:	.space	2	/* bss word @ 0x1a02 */
D_1a04:	.space	2	/* bss word @ 0x1a04 */
D_1a06:	.space	2	/* bss word @ 0x1a06 */
D_1a08:	.space	2	/* bss word @ 0x1a08 */
	.space	2
D_1a0c:	.space	2	/* bss word @ 0x1a0c */
D_1a0e:	.space	2	/* bss word @ 0x1a0e */
	.space	4
D_1a14:	.space	2	/* bss word @ 0x1a14 */
D_1a16:	.space	2	/* bss word @ 0x1a16 */
D_1a18:	.space	2	/* bss word @ 0x1a18 */
D_1a1a:	.space	2	/* bss word @ 0x1a1a */
D_1a1c:	.space	2	/* bss word @ 0x1a1c */
	.space	2
D_1a20:	.space	2	/* bss word @ 0x1a20 */
D_1a22:	.space	2	/* bss word @ 0x1a22 */
D_1a24:	.space	2	/* bss word @ 0x1a24 */
D_1a26:	.space	2	/* bss word @ 0x1a26 */
D_1a28:	.space	2	/* bss word @ 0x1a28 */
D_1a2a:	.space	2	/* bss word @ 0x1a2a */
D_1a2c:	.space	2	/* bss word @ 0x1a2c */
D_1a2e:	.space	2	/* bss word @ 0x1a2e */
D_1a30:	.space	2	/* bss word @ 0x1a30 */
D_1a32:	.space	2	/* bss word @ 0x1a32 */
D_1a34:	.space	2	/* bss word @ 0x1a34 */
	.space	1006
D_1e24:	.space	2	/* bss word @ 0x1e24 */
	.space	2
D_1e28:	.space	2	/* bss word @ 0x1e28 */
D_1e2a:	.space	2	/* bss word @ 0x1e2a */
D_1e2c:	.space	2	/* bss word @ 0x1e2c */
D_1e2e:	.space	2	/* bss word @ 0x1e2e */
D_1e30:	.space	2	/* bss word @ 0x1e30 */
D_1e32:	.space	2	/* bss word @ 0x1e32 */
D_1e34:	.space	2	/* bss word @ 0x1e34 */
D_1e36:	.space	2	/* bss word @ 0x1e36 */
D_1e38:	.space	2	/* bss word @ 0x1e38 */
D_1e3a:	.space	2	/* bss word @ 0x1e3a */
D_1e3c:	.space	2	/* bss word @ 0x1e3c */
D_1e3e:	.space	2	/* bss word @ 0x1e3e */
D_1e40:	.space	2	/* bss word @ 0x1e40 */
D_1e42:	.space	2	/* bss word @ 0x1e42 */
D_1e44:	.space	2	/* bss word @ 0x1e44 */
	.space	6
D_1e4c:	.space	2	/* bss word @ 0x1e4c */
	.space	522
D_2058:	.space	2	/* bss word @ 0x2058 */
D_205a:	.space	2	/* bss word @ 0x205a */
D_205c:	.space	2	/* bss word @ 0x205c */
	.space	662
D_22f4:	.space	2	/* bss word @ 0x22f4 */
	.space	6
D_22fc:	.space	2	/* bss word @ 0x22fc */
D_22fe:	.space	2	/* bss word @ 0x22fe */
D_2300:	.space	2	/* bss word @ 0x2300 */
D_2302:	.space	2	/* bss word @ 0x2302 */
D_2304:	.space	2	/* bss word @ 0x2304 */
D_2306:	.space	2	/* bss word @ 0x2306 */
D_2308:	.space	2	/* bss word @ 0x2308 */
D_230a:	.space	2	/* bss word @ 0x230a */
D_230c:	.space	2	/* bss word @ 0x230c */
D_230e:	.space	2	/* bss word @ 0x230e */
D_2310:	.space	2	/* bss word @ 0x2310 */
D_2312:	.space	2	/* bss word @ 0x2312 */
D_2314:	.space	2	/* bss word @ 0x2314 */
D_2316:	.space	2	/* bss word @ 0x2316 */
D_2318:	.space	2	/* bss word @ 0x2318 */
D_231a:	.space	2	/* bss word @ 0x231a */
D_231c:	.space	2	/* bss word @ 0x231c */
D_231e:	.space	2	/* bss word @ 0x231e */
D_2320:	.space	2	/* bss word @ 0x2320 */
D_2322:	.space	2	/* bss word @ 0x2322 */
D_2324:	.space	2	/* bss word @ 0x2324 */
D_2326:	.space	2	/* bss word @ 0x2326 */
D_2328:	.space	2	/* bss word @ 0x2328 */
D_232a:	.space	2	/* bss word @ 0x232a */
D_232c:	.space	2	/* bss word @ 0x232c */
D_232e:	.space	2	/* bss word @ 0x232e */
D_2330:	.space	2	/* bss word @ 0x2330 */
D_2332:	.space	2	/* bss word @ 0x2332 */
D_2334:	.space	2	/* bss word @ 0x2334 */
D_2336:	.space	2	/* bss word @ 0x2336 */
D_2338:	.space	2	/* bss word @ 0x2338 */
D_233a:	.space	2	/* bss word @ 0x233a */
D_233c:	.space	2	/* bss word @ 0x233c */
	.space	6
D_2344:	.space	2	/* bss word @ 0x2344 */
	.space	674
D_25e8:	.space	2	/* bss word @ 0x25e8 */
	.space	346
D_2744:	.space	2	/* bss word @ 0x2744 */
D_2746:	.space	2	/* bss word @ 0x2746 */
D_2748:	.space	2	/* bss word @ 0x2748 */

/* end of file */
