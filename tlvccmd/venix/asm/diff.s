/* diff.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 diff.s -o diff.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* Wrapping near-call/jmp targets (16-bit address space) */
	.set L_a375, 0xa375	/* wrapping branch target */
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: diff  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00002d68  (11624 bytes)
//   a_data:   0x000002c2  (706 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

L_0036:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0042:
	pushw  D_02de
	call   L_2d0a
	pop    %cx
	pushw  D_0006
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0056:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0062:
	pushw  0x4(%bp)
	call   L_29e6
	pop    %cx
	mov    %ax,%di
	or     %di,%di
	je   L_0073
	mov    %di,%ax
	jmp   L_0076
L_0073:
	call   L_00bb
L_0076:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_007a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0086:
	pushw  0x4(%bp)
	call   L_2b32
	pop    %cx
	pushw  D_02c6
	call   L_2b32
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	call   L_29e6
	pop    %cx
	mov    %ax,D_02c6
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_2b4e
	add    $0x4,%sp
	mov    %ax,%di
	or     %di,%di
	jne   L_00b5
	call   L_00bb
L_00b5:
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_00bb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_00c7:
	pushw  D_000a
	mov    $D_001b,%ax
	push   %ax
	call   L_151d
	add    $0x4,%sp
	call   L_0036
	pop    %di
	pop    %si
	pop    %bp
	ret


L_00dc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xa,%sp
L_00eb:
	mov    $0x1,%di
L_00ee:
	cmp    0x6(%bp),%di
	jg   L_0105
	mov    %di,%ax
	shl    %ax
	dec    %ax
	mov    %ax,%si
	mov    %di,%ax
	mov    $0x2,%dx
	imul   %dx
	mov    %ax,%di
	jmp   L_00ee
L_0105:
	mov    %si,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,%si
L_010f:
	or     %si,%si
	jne   L_0116
	jmp   L_01d0
L_0116:
	mov    0x6(%bp),%ax
	sub    %si,%ax
	mov    %ax,-0xe(%bp)
	mov    $0x1,%di
L_0121:
	cmp    -0xe(%bp),%di
	jle   L_0129
	jmp   L_01c3
L_0129:
	mov    %di,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0136:
	mov    -0xa(%bp),%ax
	cmp    0x4(%bp),%ax
	jbe   L_0150
	mov    %si,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
	cmp    -0xa(%bp),%ax
	jae   L_0152
L_0150:
	jmp   L_01bf
L_0152:
	mov    -0xc(%bp),%bx
	mov    0x2(%bx),%ax
	mov    -0xa(%bp),%bx
	cmp    0x2(%bx),%ax
	jg   L_0178
	mov    -0xc(%bp),%bx
	mov    0x2(%bx),%ax
	mov    -0xa(%bp),%bx
	cmp    0x2(%bx),%ax
	jne   L_017a
	mov    -0xc(%bp),%bx
	mov    (%bx),%ax
	mov    -0xa(%bp),%bx
	cmp    (%bx),%ax
L_0178:
	jg   L_01bf
L_017a:
	mov    -0xa(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x6(%bp)
	mov    -0xc(%bp),%bx
	mov    0x2(%bx),%ax
	mov    -0xa(%bp),%bx
	mov    %ax,0x2(%bx)
	mov    -0xc(%bp),%bx
	mov    -0x6(%bp),%ax
	mov    %ax,0x2(%bx)
	mov    -0xa(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x8(%bp)
	mov    -0xc(%bp),%bx
	mov    (%bx),%ax
	mov    -0xa(%bp),%bx
	mov    %ax,(%bx)
	mov    -0xc(%bp),%bx
	mov    -0x8(%bp),%ax
	mov    %ax,(%bx)
	mov    %si,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	sub    %ax,-0xa(%bp)
	jmp   L_0136
L_01bf:
	inc    %di
	jmp   L_0121
L_01c3:
	mov    %si,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,%si
	jmp   L_010f
L_01d0:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_01d7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_01e3:
	mov    0x6(%bp),%ax
	inc    %ax
	shl    %ax
	push   %ax
	call   L_0056
	pop    %cx
	mov    %ax,%di
	mov    $0x1,%si
L_01f3:
	cmp    0x6(%bp),%si
	jg   L_0220
	mov    %si,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    0x2(%bx),%ax
	mov    %si,%dx
	mov    $0x2,%cx
	shl    %cl,%dx
	add    0x4(%bp),%dx
	mov    %dx,%bx
	mov    (%bx),%dx
	shl    %dx
	add    %di,%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	inc    %si
	jmp   L_01f3
L_0220:
	mov    $0x1,%si
L_0223:
	cmp    0x6(%bp),%si
	jg   L_0240
	mov    %si,%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %si,%dx
	shl    %dx
	add    0x8(%bp),%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	inc    %si
	jmp   L_0223
L_0240:
	push   %di
	call   L_2b32
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0249:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x224,%sp
L_0259:
	mov    0x4(%bp),%bx
	mov    (%bx),%di
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x6(%bp)
	lea    -0x224(%bp),%ax
	push   %ax
	push   %di
	call   L_2ca8
	add    $0x4,%sp
	cmp    $0xffff,%ax
	je   L_02dd
	mov    -0x220(%bp),%ax
	and    $0xe000,%ax
	cmp    $0xc000,%ax
	jne   L_02dd
	mov    $0x64,%ax
	push   %ax
	call   L_29e6
	pop    %cx
	mov    0x4(%bp),%bx
	mov    %ax,(%bx)
	mov    %ax,%si
L_0292:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0292
	movb   $0x2f,-0x1(%si)
	mov    %si,%di
L_02a9:
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_02c0
	jmp   L_0398
L_02c0:
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_02a9
	mov    -0x6(%bp),%bx
	cmpb   $0x2f,(%bx)
	je   L_02a9
	mov    -0x6(%bp),%bx
	cmpb   $0x2f,-0x1(%bx)
	jne   L_02a9
	mov    %si,%di
	jmp   L_02a9
L_02dd:
	cmpb   $0x2d,(%di)
	jne   L_02ed
	cmpb   $0x0,0x1(%di)
	jne   L_02ed
	cmpw   $0x0,D_02de
L_02ed:
	je   L_02f2
	jmp   L_0398
L_02f2:
	mov    $0x36,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_1a0a
	add    $0x4,%sp
	mov    $0x36,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_1a0a
	add    $0x4,%sp
	mov    $0x36,%ax
	push   %ax
	mov    $0xd,%ax
	push   %ax
	call   L_1a0a
	add    $0x4,%sp
	mov    $0x36,%ax
	push   %ax
	mov    $0xf,%ax
	push   %ax
	call   L_1a0a
	add    $0x4,%sp
	mov    $D_0032,%ax
	push   %ax
	call   L_15d4
	pop    %cx
	mov    %ax,D_02de
	mov    0x4(%bp),%bx
	mov    %ax,(%bx)
	mov    $0x180,%ax
	push   %ax
	pushw  D_02de
	call   L_1768
	add    $0x4,%sp
	mov    %ax,-0x228(%bp)
	or     %ax,%ax
	jge   L_0361
	pushw  D_02de
	mov    $D_003e,%ax
	push   %ax
	call   L_151d
	add    $0x4,%sp
	call   L_0036
L_0361:
	mov    $0x200,%ax
	push   %ax
	lea    -0x206(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_2c8e
	add    $0x6,%sp
	mov    %ax,-0x226(%bp)
	or     %ax,%ax
	jle   L_0390
	pushw  -0x226(%bp)
	lea    -0x206(%bp),%ax
	push   %ax
	pushw  -0x228(%bp)
	call   L_2d1e
	add    $0x6,%sp
	jmp   L_0361
L_0390:
	pushw  -0x228(%bp)
	call   L_2c3c
	pop    %cx
L_0398:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_039f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_03ac:
	mov    $D_004d,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_166a
	add    $0x4,%sp
	mov    0x4(%bp),%bx
	shl    %bx
	mov    %ax,D_02e6(%bx)
	or     %ax,%ax
	jne   L_03d6
	pushw  0x6(%bp)
	mov    $D_004f,%ax
	push   %ax
	call   L_151d
	add    $0x4,%sp
	call   L_0036
L_03d6:
	mov    $0xc,%ax
	push   %ax
	call   L_0056
	pop    %cx
	mov    %ax,%di
	xor    %si,%si
L_03e2:
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_02e6(%bx)
	call   L_137c
	pop    %cx
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_041c
	inc    %si
	lea    0x3(%si),%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	push   %ax
	push   %di
	call   L_007a
	add    $0x4,%sp
	mov    %ax,%di
	mov    %si,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    %di,%ax
	mov    %ax,%bx
	mov    -0x6(%bp),%ax
	mov    %ax,0x2(%bx)
	jmp   L_03e2
L_041c:
	mov    0x4(%bp),%bx
	shl    %bx
	mov    %si,D_02fa(%bx)
	mov    0x4(%bp),%bx
	shl    %bx
	mov    %di,D_02ca(%bx)
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_02e6(%bx)
	call   L_296f
	pop    %cx
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0440:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_044c:
	movw   $0x0,D_02e0
L_0452:
	mov    D_02e0,%ax
	cmp    D_02fa,%ax
	jge   L_0491
	mov    D_02e0,%ax
	cmp    D_02fc,%ax
	jge   L_0491
	mov    D_02e0,%ax
	inc    %ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    D_02ca,%ax
	mov    %ax,%bx
	mov    0x2(%bx),%ax
	mov    D_02e0,%dx
	inc    %dx
	mov    $0x2,%cx
	shl    %cl,%dx
	add    D_02cc,%dx
	mov    %dx,%bx
	cmp    0x2(%bx),%ax
	jne   L_0491
	incw   D_02e0
	jmp   L_0452
L_0491:
	movw   $0x0,D_02d8
L_0497:
	mov    D_02fa,%ax
	sub    D_02e0,%ax
	cmp    %ax,D_02d8
	jge   L_04e4
	mov    D_02fc,%ax
	sub    D_02e0,%ax
	cmp    %ax,D_02d8
	jge   L_04e4
	mov    D_02fa,%ax
	sub    D_02d8,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    D_02ca,%ax
	mov    %ax,%bx
	mov    0x2(%bx),%ax
	mov    D_02fc,%dx
	sub    D_02d8,%dx
	mov    $0x2,%cx
	shl    %cl,%dx
	add    D_02cc,%dx
	mov    %dx,%bx
	cmp    0x2(%bx),%ax
	jne   L_04e4
	incw   D_02d8
	jmp   L_0497
L_04e4:
	xor    %si,%si
L_04e6:
	cmp    $0x2,%si
	jge   L_0545
	mov    %si,%bx
	shl    %bx
	mov    D_02ca(%bx),%ax
	mov    D_02e0,%dx
	mov    $0x2,%cx
	shl    %cl,%dx
	add    %dx,%ax
	mov    %si,%bx
	shl    %bx
	mov    %ax,D_02f2(%bx)
	mov    %si,%bx
	shl    %bx
	mov    D_02fa(%bx),%ax
	sub    D_02e0,%ax
	sub    D_02d8,%ax
	mov    %si,%bx
	shl    %bx
	mov    %ax,D_02fe(%bx)
	xor    %di,%di
L_0520:
	mov    %si,%bx
	shl    %bx
	cmp    D_02fe(%bx),%di
	jg   L_0542
	mov    %si,%bx
	shl    %bx
	mov    D_02f2(%bx),%ax
	mov    %di,%dx
	mov    $0x2,%cx
	shl    %cl,%dx
	add    %dx,%ax
	mov    %ax,%bx
	mov    %di,(%bx)
	inc    %di
	jmp   L_0520
L_0542:
	inc    %si
	jmp   L_04e6
L_0545:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0549:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0555:
	mov    $0x1,%si
	mov    %si,%di
L_055a:
	cmp    0x6(%bp),%di
	jg   L_0562
	cmp    0xa(%bp),%si
L_0562:
	jle   L_0567
	jmp   L_05e7
L_0567:
	mov    %di,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    0x2(%bx),%ax
	mov    %si,%dx
	mov    $0x2,%cx
	shl    %cl,%dx
	add    0x8(%bp),%dx
	mov    %dx,%bx
	cmp    0x2(%bx),%ax
	jge   L_059b
	mov    %di,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	movw   $0x0,0x2(%bx)
	inc    %di
L_0599:
	jmp   L_055a
L_059b:
	mov    %di,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    0x2(%bx),%ax
	mov    %si,%dx
	mov    $0x2,%cx
	shl    %cl,%dx
	add    0x8(%bp),%dx
	mov    %dx,%bx
	cmp    0x2(%bx),%ax
	jne   L_05cd
	mov    %di,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    %si,0x2(%bx)
	inc    %di
	jmp   L_0599
L_05cd:
	inc    %si
	jmp   L_0599
L_05d0:
	cmp    0x6(%bp),%di
	jg   L_05e9
	mov    %di,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	movw   $0x0,0x2(%bx)
	inc    %di
L_05e7:
	jmp   L_05d0
L_05e9:
	mov    0xa(%bp),%ax
	inc    %ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x8(%bp),%ax
	mov    %ax,%bx
	movw   $0x0,0x2(%bx)
	xor    %si,%si
L_05fe:
	inc    %si
	cmp    0xa(%bp),%si
	jg   L_065c
	mov    %si,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x8(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	neg    %ax
	mov    %si,%dx
	shl    %dx
	add    0xc(%bp),%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
L_061f:
	lea    0x1(%si),%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x8(%bp),%ax
	mov    %ax,%bx
	mov    0x2(%bx),%ax
	mov    %si,%dx
	mov    $0x2,%cx
	shl    %cl,%dx
	add    0x8(%bp),%dx
	mov    %dx,%bx
	cmp    0x2(%bx),%ax
	jne   L_05fe
	inc    %si
	mov    %si,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x8(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %si,%dx
	shl    %dx
	add    0xc(%bp),%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	jmp   L_061f
L_065c:
	mov    %si,%ax
	shl    %ax
	add    0xc(%bp),%ax
	mov    %ax,%bx
	movw   $0xffff,(%bx)
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
	push   %cx
L_067a:
	mov    0x6(%bp),%ax
	mov    %ax,-0x6(%bp)
	cmpw   $0x3,0x4(%bp)
	jle   L_0701
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x2d,(%bx)
	jne   L_0701
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
	mov    $0x1,%di
L_069b:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	add    %di,%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	je   L_0701
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	add    %di,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	jmp   L_06ef
	/* movw constant -> D_02dc */
	mov	$0xffff,%ax
	mov	%ax,D_02dc
	jmp   L_06ed
	movw   $0x1,D_02dc
	jmp   L_06fe
	movw   $0x1,D_02e2
	jmp   L_06fe
	pushw  -0x6(%bp)
	mov    $D_005c,%ax
	push   %ax
	call   L_1542
	add    $0x4,%sp
	pushw  D_000a
	mov    $D_006b,%ax
	push   %ax
	call   L_151d
	add    $0x4,%sp
	call   L_0036
L_06ed:
	jmp   L_06fe
L_06ef:
	sub    $0x62,%ax
	cmp    $0x6,%ax
	ja   L_06fe
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x200c(%bx)
L_06fe:
	inc    %di
	jmp   L_069b
L_0701:
	cmpw   $0x3,0x4(%bp)
	je   L_0718
	mov    $D_007d,%ax
	push   %ax
	mov    $D_0228,%ax
	push   %ax
	call   L_15a8
	add    $0x4,%sp
	call   L_0036
L_0718:
	mov    $0x1,%ax
	push   %ax
	call   L_29e6
	pop    %cx
	mov    %ax,D_02c6
	mov    0x6(%bp),%ax
	add    $0x4,%ax
	push   %ax
	mov    0x6(%bp),%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_0249
	add    $0x4,%sp
	mov    0x6(%bp),%ax
	inc    %ax
	inc    %ax
	push   %ax
	mov    0x6(%bp),%ax
	add    $0x4,%ax
	push   %ax
	call   L_0249
	add    $0x4,%sp
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	xor    %ax,%ax
	push   %ax
	call   L_039f
	add    $0x4,%sp
	mov    0x6(%bp),%bx
	pushw  0x4(%bx)
	mov    $0x1,%ax
	push   %ax
	call   L_039f
	add    $0x4,%sp
	call   L_0440
	pushw  D_02fe
	pushw  D_02f2
	call   L_00dc
	add    $0x4,%sp
	pushw  D_0300
	pushw  D_02f4
	call   L_00dc
	add    $0x4,%sp
	mov    D_02cc,%ax
	mov    %ax,D_02f6
	pushw  D_02f6
	pushw  D_0300
	pushw  D_02f4
	pushw  D_02fe
	pushw  D_02f2
	call   L_0549
	add    $0xa,%sp
	mov    D_0300,%ax
	inc    %ax
	inc    %ax
	shl    %ax
	push   %ax
	pushw  D_02f6
	call   L_007a
	add    $0x4,%sp
	mov    %ax,D_02f6
	mov    D_02ca,%ax
	mov    %ax,D_02da
	pushw  D_02da
	pushw  D_02fe
	pushw  D_02f2
	call   L_01d7
	add    $0x6,%sp
	mov    D_02fe,%ax
	inc    %ax
	inc    %ax
	shl    %ax
	push   %ax
	pushw  D_02da
	call   L_007a
	add    $0x4,%sp
	mov    %ax,D_02da
	mov    D_02fe,%ax
	inc    %ax
	inc    %ax
	shl    %ax
	push   %ax
	call   L_0056
	pop    %cx
	mov    %ax,D_02c4
	mov    $0x6,%ax
	push   %ax
	call   L_0056
	pop    %cx
	mov    %ax,D_02e4
	pushw  D_02c4
	pushw  D_02f6
	pushw  D_02fe
	pushw  D_02da
	call   L_089a
	add    $0x8,%sp
	mov    %ax,%di
	pushw  D_02f6
	call   L_2b32
	pop    %cx
	pushw  D_02da
	call   L_2b32
	pop    %cx
	mov    D_02fa,%ax
	inc    %ax
	inc    %ax
	shl    %ax
	push   %ax
	call   L_0056
	pop    %cx
	mov    %ax,D_02ea
	mov    %di,%ax
	shl    %ax
	add    D_02c4,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_0ab9
	pop    %cx
	pushw  D_02e4
	call   L_2b32
	pop    %cx
	pushw  D_02c4
	call   L_2b32
	pop    %cx
	mov    D_02fa,%ax
	inc    %ax
	inc    %ax
	mov    $0x2,%cx
	shl    %cl,%ax
	push   %ax
	call   L_0056
	pop    %cx
	mov    %ax,D_02f8
	mov    D_02fc,%ax
	inc    %ax
	inc    %ax
	mov    $0x2,%cx
	shl    %cl,%ax
	push   %ax
	call   L_0056
	pop    %cx
	mov    %ax,D_02ce
	pushw  0x6(%bp)
	call   L_0b3e
	pop    %cx
	pushw  0x6(%bp)
	call   L_0ec9
	pop    %cx
	mov    D_0008,%ax
	mov    %ax,D_0006
	call   L_0036
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_089a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
L_08a9:
	xor    %si,%si
	mov    %si,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_09d8
	add    $0x6,%sp
	mov    0xa(%bp),%bx
	mov    %ax,(%bx)
	mov    $0x1,%di
L_08c2:
	cmp    0x6(%bp),%di
	jle   L_08ca
	jmp   L_09cf
L_08ca:
	mov    %di,%ax
	shl    %ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jne   L_08df
	jmp   L_09b0
L_08df:
	mov    -0x8(%bp),%ax
	shl    %ax
	add    0x8(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	neg    %ax
	mov    %ax,-0x6(%bp)
	movw   $0x0,-0x10(%bp)
	mov    0xa(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0xc(%bp)
L_08fd:
	mov    -0xc(%bp),%ax
	mov    $0x6,%dx
	imul   %dx
	add    D_02e4,%ax
	mov    %ax,%bx
	mov    -0x6(%bp),%ax
	cmp    0x2(%bx),%ax
	jle   L_095e
	pushw  -0x6(%bp)
	push   %si
	pushw  0xa(%bp)
	call   L_0a28
	add    $0x6,%sp
	mov    %ax,-0xa(%bp)
	mov    -0x10(%bp),%ax
	inc    %ax
	cmp    %ax,-0xa(%bp)
	je   L_093c
	mov    -0xa(%bp),%ax
	dec    %ax
	shl    %ax
	add    0xa(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,-0xc(%bp)
L_093c:
	cmp    %si,-0xa(%bp)
	jg   L_0996
	mov    -0xa(%bp),%ax
	shl    %ax
	add    0xa(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    $0x6,%dx
	imul   %dx
	add    D_02e4,%ax
	mov    %ax,%bx
	mov    0x2(%bx),%ax
	cmp    -0x6(%bp),%ax
L_095e:
	jle   L_0994
	mov    -0xa(%bp),%ax
	shl    %ax
	add    0xa(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,-0xe(%bp)
	pushw  -0xc(%bp)
	pushw  -0x6(%bp)
	push   %di
	call   L_09d8
	add    $0x6,%sp
	mov    -0xa(%bp),%dx
	shl    %dx
	add    0xa(%bp),%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	mov    -0xe(%bp),%ax
	mov    %ax,-0xc(%bp)
	mov    -0xa(%bp),%ax
	mov    %ax,-0x10(%bp)
L_0994:
	jmp   L_09b2
L_0996:
	pushw  -0xc(%bp)
	pushw  -0x6(%bp)
	push   %di
	call   L_09d8
	add    $0x6,%sp
	mov    -0xa(%bp),%dx
	shl    %dx
	add    0xa(%bp),%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	inc    %si
L_09b0:
	jmp   L_09cb
L_09b2:
	incw   -0x8(%bp)
	mov    -0x8(%bp),%ax
	shl    %ax
	add    0x8(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_09cb
	jmp   L_08fd
L_09cb:
	inc    %di
	jmp   L_08c2
L_09cf:
	mov    %si,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_09d8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_09e4:
	incw   D_0004
	mov    D_0004,%ax
	mov    $0x6,%dx
	imul   %dx
	push   %ax
	pushw  D_02e4
	call   L_007a
	add    $0x4,%sp
	mov    %ax,D_02e4
	mov    D_0004,%ax
	mov    $0x6,%dx
	imul   %dx
	add    D_02e4,%ax
	sub    $0x6,%ax
	mov    %ax,%di
	mov    0x4(%bp),%ax
	mov    %ax,(%di)
	mov    0x6(%bp),%ax
	mov    %ax,0x2(%di)
	mov    0x8(%bp),%ax
	mov    %ax,0x4(%di)
	mov    D_0004,%ax
	dec    %ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a28:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
L_0a37:
	mov    0x6(%bp),%ax
	shl    %ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    $0x6,%dx
	imul   %dx
	add    D_02e4,%ax
	mov    %ax,%bx
	mov    0x2(%bx),%ax
	cmp    0x8(%bp),%ax
	jge   L_0a5c
	mov    0x6(%bp),%ax
	inc    %ax
	jmp   L_0aac
L_0a5c:
	xor    %di,%di
	mov    0x6(%bp),%ax
	inc    %ax
	mov    %ax,%si
L_0a64:
	mov    %di,%ax
	add    %si,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0x6(%bp)
	cmp    %di,%ax
	jle   L_0aae
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    $0x6,%dx
	imul   %dx
	add    D_02e4,%ax
	mov    %ax,%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0x8(%bp)
	cmp    0x8(%bp),%ax
	jle   L_0a9c
	mov    -0x6(%bp),%si
	jmp   L_0a64
L_0a9c:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_0aa9
	mov    -0x6(%bp),%di
	jmp   L_0a64
L_0aa9:
	mov    -0x6(%bp),%ax
L_0aac:
	jmp   L_0ab2
L_0aae:
	mov    -0x6(%bp),%ax
	inc    %ax
L_0ab2:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ab9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0ac5:
	xor    %di,%di
L_0ac7:
	cmp    D_02fa,%di
	jg   L_0aff
	cmp    D_02e0,%di
	jg   L_0ad7
	mov    %di,%ax
	jmp   L_0af0
L_0ad7:
	mov    D_02fa,%ax
	sub    D_02d8,%ax
	cmp    %ax,%di
	jle   L_0aee
	mov    %di,%ax
	add    D_02fc,%ax
	sub    D_02fa,%ax
	jmp   L_0af0
L_0aee:
	xor    %ax,%ax
L_0af0:
	mov    %di,%dx
	shl    %dx
	add    D_02ea,%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	inc    %di
	jmp   L_0ac7
L_0aff:
	mov    0x4(%bp),%ax
	mov    $0x6,%dx
	imul   %dx
	add    D_02e4,%ax
	mov    %ax,%si
L_0b0d:
	cmpw   $0x0,0x2(%si)
	je   L_0b3a
	mov    0x2(%si),%ax
	add    D_02e0,%ax
	mov    (%si),%dx
	add    D_02e0,%dx
	shl    %dx
	add    D_02ea,%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	mov    0x4(%si),%ax
	mov    $0x6,%dx
	imul   %dx
	add    D_02e4,%ax
	mov    %ax,%si
	jmp   L_0b0d
L_0b3a:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0b3e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x14,%sp
L_0b4d:
	mov    $D_009e,%ax
	push   %ax
	mov    0x4(%bp),%bx
	pushw  0x2(%bx)
	call   L_166a
	add    $0x4,%sp
	mov    %ax,D_02e6
	mov    $D_00a0,%ax
	push   %ax
	mov    0x4(%bp),%bx
	pushw  0x4(%bx)
	call   L_166a
	add    $0x4,%sp
	mov    %ax,D_02e8
	mov    $0x1,%si
	mov    D_02ce,%bx
	movw   $0x0,(%bx)
	movw   $0x0,0x2(%bx)
	mov    D_02f8,%bx
	movw   $0x0,(%bx)
	movw   $0x0,0x2(%bx)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0xe(%bp)
	movw   $0x0,-0xc(%bp)
	movw   $0x0,-0xa(%bp)
	movw   $0x0,-0x8(%bp)
	mov    $0x1,%di
L_0bac:
	cmp    D_02fa,%di
	jle   L_0bb5
	jmp   L_0e60
L_0bb5:
	mov    %di,%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	jne   L_0c05
	xor    %ax,%ax
	push   %ax
	call   L_0e79
	pop    %cx
	cwtd
	mov    %ax,-0x14(%bp)
	mov    %dx,-0x12(%bp)
	mov    -0xa(%bp),%ax
	mov    -0x8(%bp),%dx
	add    -0x14(%bp),%ax
	adc    -0x12(%bp),%dx
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
	mov    %ax,-0x18(%bp)
	mov    %dx,-0x16(%bp)
	mov    %di,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    D_02f8,%ax
	mov    %ax,%bx
	mov    -0x18(%bp),%ax
	mov    -0x16(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
	jmp   L_0e16
L_0c05:
	mov    %di,%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	cmp    (%bx),%si
	jge   L_0c55
	mov    $0x1,%ax
	push   %ax
	call   L_0e79
	pop    %cx
	cwtd
	mov    %ax,-0x14(%bp)
	mov    %dx,-0x12(%bp)
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	add    -0x14(%bp),%ax
	adc    -0x12(%bp),%dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    %ax,-0x18(%bp)
	mov    %dx,-0x16(%bp)
	mov    %si,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    D_02ce,%ax
	mov    %ax,%bx
	mov    -0x18(%bp),%ax
	mov    -0x16(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
	inc    %si
	jmp   L_0c05
L_0c55:
	mov    D_02e6,%bx
	decw   0x2(%bx)
	jl   L_0c71
	mov    D_02e6,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0c79
L_0c71:
	pushw  D_02e6
	call   L_195c
	pop    %cx
L_0c79:
	mov    %ax,%cx
	mov    %cl,-0xf(%bp)
	mov    D_02e8,%bx
	decw   0x2(%bx)
	jl   L_0c9a
	mov    D_02e8,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0ca2
L_0c9a:
	pushw  D_02e8
	call   L_195c
	pop    %cx
L_0ca2:
	mov    %ax,%cx
	mov    %cl,-0x10(%bp)
	addw   $0x1,-0xa(%bp)
	adcw   $0x0,-0x8(%bp)
	addw   $0x1,-0xe(%bp)
	adcw   $0x0,-0xc(%bp)
	cmpw   $0x0,D_02e2
	je   L_0cdc
	mov    -0xf(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_00d3(%bx),%al
	cbtw
	test   $0x8,%ax
	je   L_0cdc
	mov    -0x10(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_00d3(%bx),%al
	cbtw
	test   $0x8,%ax
L_0cdc:
	je   L_0d28
L_0cde:
	cmpb   $0xa,-0xf(%bp)
	je   L_0d24
	addw   $0x1,-0xa(%bp)
	adcw   $0x0,-0x8(%bp)
	mov    D_02e6,%bx
	decw   0x2(%bx)
	jl   L_0d08
	mov    D_02e6,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0d10
L_0d08:
	pushw  D_02e6
	call   L_195c
	pop    %cx
L_0d10:
	mov    %ax,%cx
	mov    %cl,-0xf(%bp)
	mov    %cl,%al
	cbtw
	mov    %ax,%bx
	mov    D_00d3(%bx),%al
	cbtw
	test   $0x8,%ax
	jne   L_0cde
L_0d24:
	cmpb   $0xa,-0x10(%bp)
L_0d28:
	je   L_0d6a
	addw   $0x1,-0xe(%bp)
	adcw   $0x0,-0xc(%bp)
	mov    D_02e8,%bx
	decw   0x2(%bx)
	jl   L_0d4e
	mov    D_02e8,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0d56
L_0d4e:
	pushw  D_02e8
	call   L_195c
	pop    %cx
L_0d56:
	mov    %ax,%cx
	mov    %cl,-0x10(%bp)
	mov    %cl,%al
	cbtw
	mov    %ax,%bx
	mov    D_00d3(%bx),%al
	cbtw
	test   $0x8,%ax
	jne   L_0d24
L_0d6a:
	mov    -0xf(%bp),%al
	cbtw
	mov    -0x10(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_0ddc
	incw   -0x6(%bp)
	mov    %di,%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	cmpb   $0xa,-0xf(%bp)
	je   L_0db3
	xor    %ax,%ax
	push   %ax
	call   L_0e79
	pop    %cx
	cwtd
	mov    %ax,-0x14(%bp)
	mov    %dx,-0x12(%bp)
	mov    -0xa(%bp),%ax
	mov    -0x8(%bp),%dx
	add    -0x14(%bp),%ax
	adc    -0x12(%bp),%dx
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
L_0db3:
	cmpb   $0xa,-0x10(%bp)
	je   L_0de5
	mov    $0x1,%ax
	push   %ax
	call   L_0e79
	pop    %cx
	cwtd
	mov    %ax,-0x14(%bp)
	mov    %dx,-0x12(%bp)
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	add    -0x14(%bp),%ax
	adc    -0x12(%bp),%dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	jmp   L_0de5
L_0ddc:
	cmpb   $0xa,-0xf(%bp)
	je   L_0de5
	jmp   L_0c55
L_0de5:
	mov    %di,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    D_02f8,%ax
	mov    %ax,%bx
	mov    -0xa(%bp),%ax
	mov    -0x8(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
	mov    %si,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    D_02ce,%ax
	mov    %ax,%bx
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
	inc    %si
L_0e16:
	inc    %di
	jmp   L_0bac
L_0e1a:
	cmp    D_02fc,%si
	jg   L_0e62
	mov    $0x1,%ax
	push   %ax
	call   L_0e79
	pop    %cx
	cwtd
	mov    %ax,-0x14(%bp)
	mov    %dx,-0x12(%bp)
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	add    -0x14(%bp),%ax
	adc    -0x12(%bp),%dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    %ax,-0x18(%bp)
	mov    %dx,-0x16(%bp)
	mov    %si,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    D_02ce,%ax
	mov    %ax,%bx
	mov    -0x18(%bp),%ax
	mov    -0x16(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
	inc    %si
L_0e60:
	jmp   L_0e1a
L_0e62:
	pushw  D_02e6
	call   L_296f
	pop    %cx
	pushw  D_02e8
	call   L_296f
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e79:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0e85:
	mov    $0x1,%di
L_0e88:
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_02e6(%bx),%bx
	decw   0x2(%bx)
	jl   L_0eae
	mov    0x4(%bp),%bx
	shl    %bx
	mov    D_02e6(%bx),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0ebb
L_0eae:
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_02e6(%bx)
	call   L_195c
	pop    %cx
L_0ebb:
	cmp    $0xa,%ax
	je   L_0ec3
	inc    %di
	jmp   L_0e88
L_0ec3:
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ec9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
L_0ed8:
	mov    $D_00a2,%ax
	push   %ax
	mov    0x4(%bp),%bx
	pushw  0x2(%bx)
	call   L_166a
	add    $0x4,%sp
	mov    %ax,D_02e6
	mov    $D_00a4,%ax
	push   %ax
	mov    0x4(%bp),%bx
	pushw  0x4(%bx)
	call   L_166a
	add    $0x4,%sp
	mov    %ax,D_02e8
	mov    D_02fa,%ax
	mov    %ax,-0x6(%bp)
	mov    D_02ea,%bx
	movw   $0x0,(%bx)
	mov    D_02fc,%ax
	inc    %ax
	mov    -0x6(%bp),%dx
	inc    %dx
	shl    %dx
	add    D_02ea,%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	cmpw   $0xffff,D_02dc
	jne   L_0f28
	jmp   L_0fbd
L_0f28:
	mov    $0x1,%di
L_0f2b:
	cmp    -0x6(%bp),%di
	jle   L_0f33
	jmp   L_1061
L_0f33:
	cmp    -0x6(%bp),%di
	jg   L_0f59
	mov    %di,%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	lea    -0x1(%di),%dx
	shl    %dx
	add    D_02ea,%dx
	mov    %dx,%bx
	mov    (%bx),%dx
	inc    %dx
	cmp    %dx,%ax
	jne   L_0f59
	inc    %di
	jmp   L_0f33
L_0f59:
	lea    -0x1(%di),%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	inc    %ax
	mov    %ax,-0xa(%bp)
	lea    -0x1(%di),%ax
	mov    %ax,%si
L_0f6f:
	cmp    -0x6(%bp),%si
	jge   L_0f87
	lea    0x1(%si),%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	jne   L_0f87
	inc    %si
	jmp   L_0f6f
L_0f87:
	lea    0x1(%si),%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	mov    -0x8(%bp),%ax
	mov    %ax,(%bx)
	pushw  -0x8(%bp)
	pushw  -0xa(%bp)
	push   %si
	push   %di
	call   L_1083
	add    $0x8,%sp
	lea    0x1(%si),%ax
	mov    %ax,%di
	jmp   L_0f2b
L_0fbd:
	mov    -0x6(%bp),%di
L_0fc0:
	cmp    $0x1,%di
	jge   L_0fc8
	jmp   L_1061
L_0fc8:
	cmp    $0x1,%di
	jl   L_0ffd
	mov    %di,%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	lea    0x1(%di),%dx
	shl    %dx
	add    D_02ea,%dx
	mov    %dx,%bx
	mov    (%bx),%dx
	dec    %dx
	cmp    %dx,%ax
	jne   L_0ffd
	mov    %di,%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	je   L_0ffd
	dec    %di
	jmp   L_0fc8
L_0ffd:
	lea    0x1(%di),%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
	lea    0x1(%di),%ax
	mov    %ax,%si
L_1013:
	cmp    $0x1,%si
	jle   L_102b
	lea    -0x1(%si),%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	jne   L_102b
	dec    %si
	jmp   L_1013
L_102b:
	lea    -0x1(%si),%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	inc    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%ax
	shl    %ax
	add    D_02ea,%ax
	mov    %ax,%bx
	mov    -0x8(%bp),%ax
	mov    %ax,(%bx)
	pushw  -0xa(%bp)
	pushw  -0x8(%bp)
	push   %di
	push   %si
	call   L_1083
	add    $0x8,%sp
	lea    -0x1(%si),%ax
	mov    %ax,%di
	jmp   L_0fc0
L_1061:
	cmpw   $0x0,-0x6(%bp)
	jne   L_107c
	pushw  D_02fc
	mov    $0x1,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_1083
	add    $0x8,%sp
L_107c:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1083:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_108f:
	mov    0x4(%bp),%ax
	cmp    0x6(%bp),%ax
	jle   L_10a2
	mov    0x8(%bp),%ax
	cmp    0xa(%bp),%ax
	jle   L_10a2
	jmp   L_1235
L_10a2:
	movw   $0x1,D_0008
	cmpw   $0x1,D_02dc
	jne   L_10b2
	jmp   L_1137
L_10b2:
	mov    $D_00a6,%ax
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_1249
	add    $0x6,%sp
	decw   D_0222
	jl   L_10f6
	mov    0x4(%bp),%ax
	cmp    0x6(%bp),%ax
	jle   L_10d5
	mov    $0x61,%ax
	jmp   L_10e5
L_10d5:
	mov    0x8(%bp),%ax
	cmp    0xa(%bp),%ax
	jle   L_10e2
	mov    $0x64,%ax
	jmp   L_10e5
L_10e2:
	mov    $0x63,%ax
L_10e5:
	mov    D_0220,%bx
	incw   D_0220
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_111e
L_10f6:
	mov    $D_0220,%ax
	push   %ax
	mov    0x4(%bp),%ax
	cmp    0x6(%bp),%ax
	jle   L_1107
	mov    $0x61,%ax
	jmp   L_1117
L_1107:
	mov    0x8(%bp),%ax
	cmp    0xa(%bp),%ax
	jle   L_1114
	mov    $0x64,%ax
	jmp   L_1117
L_1114:
	mov    $0x63,%ax
L_1117:
	push   %ax
	call   L_2812
	add    $0x4,%sp
L_111e:
	cmpw   $0xffff,D_02dc
	je   L_1135
	mov    $D_00a8,%ax
	push   %ax
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	call   L_1249
	add    $0x6,%sp
L_1135:
	jmp   L_11a3
L_1137:
	decw   D_0222
	jl   L_116b
	mov    0x4(%bp),%ax
	cmp    0x6(%bp),%ax
	jle   L_114a
	mov    $0x61,%ax
	jmp   L_115a
L_114a:
	mov    0x8(%bp),%ax
	cmp    0xa(%bp),%ax
	jle   L_1157
	mov    $0x64,%ax
	jmp   L_115a
L_1157:
	mov    $0x63,%ax
L_115a:
	mov    D_0220,%bx
	incw   D_0220
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_1193
L_116b:
	mov    $D_0220,%ax
	push   %ax
	mov    0x4(%bp),%ax
	cmp    0x6(%bp),%ax
	jle   L_117c
	mov    $0x61,%ax
	jmp   L_118c
L_117c:
	mov    0x8(%bp),%ax
	cmp    0xa(%bp),%ax
	jle   L_1189
	mov    $0x64,%ax
	jmp   L_118c
L_1189:
	mov    $0x63,%ax
L_118c:
	push   %ax
	call   L_2812
	add    $0x4,%sp
L_1193:
	mov    $D_00aa,%ax
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_1249
	add    $0x6,%sp
L_11a3:
	decw   D_0222
	jl   L_11b9
	mov    D_0220,%bx
	incw   D_0220
	movb   $0xa,(%bx)
	mov    $0xa,%ax
	jmp   L_11c7
L_11b9:
	mov    $D_0220,%ax
	push   %ax
	mov    $0xa,%ax
	push   %ax
	call   L_2812
	add    $0x4,%sp
L_11c7:
	cmpw   $0x0,D_02dc
	jne   L_1204
	mov    $D_00ac,%ax
	push   %ax
	pushw  D_02e6
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	pushw  D_02f8
	call   L_128c
	add    $0xa,%sp
	mov    0x4(%bp),%ax
	cmp    0x6(%bp),%ax
	jg   L_1204
	mov    0x8(%bp),%ax
	cmp    0xa(%bp),%ax
	jg   L_1204
	mov    $D_0220,%ax
	push   %ax
	mov    $D_00af,%ax
	push   %ax
	call   L_155c
	add    $0x4,%sp
L_1204:
	cmpw   $0x0,D_02dc
	jne   L_1210
	mov    $D_00b4,%ax
	jmp   L_1213
L_1210:
	mov    D_000a,%ax
L_1213:
	push   %ax
	pushw  D_02e8
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  D_02ce
	call   L_128c
	add    $0xa,%sp
	cmpw   $0x0,D_02dc
	je   L_1245
	mov    0x8(%bp),%ax
	cmp    0xa(%bp),%ax
L_1235:
	jg   L_1245
	mov    $D_0220,%ax
	push   %ax
	mov    $D_00b7,%ax
	push   %ax
	call   L_155c
	add    $0x4,%sp
L_1245:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1249:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1255:
	mov    0x4(%bp),%ax
	cmp    0x6(%bp),%ax
	jle   L_1262
	mov    0x6(%bp),%ax
	jmp   L_1265
L_1262:
	mov    0x4(%bp),%ax
L_1265:
	push   %ax
	mov    $D_00ba,%ax
	push   %ax
	call   L_1932
	add    $0x4,%sp
	mov    0x4(%bp),%ax
	cmp    0x6(%bp),%ax
	jge   L_1288
	pushw  0x6(%bp)
	pushw  0x8(%bp)
	mov    $D_00bd,%ax
	push   %ax
	call   L_1932
	add    $0x6,%sp
L_1288:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_128c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
L_129b:
	mov    0x6(%bp),%di
L_129e:
	cmp    0x8(%bp),%di
	jle   L_12a6
	jmp   L_a375
L_12a6:
	xor    %ax,%ax
	push   %ax
	lea    -0x1(%di),%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	pushw  0x2(%bx)
	pushw  (%bx)
	pushw  0xa(%bp)
	call   L_177e
	add    $0x8,%sp
	lea    -0x1(%di),%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    0x2(%bx),%dx
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
	mov    %di,%ax
	mov    $0x2,%cx
	shl    %cl,%ax
	add    0x4(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    0x2(%bx),%dx
	sub    -0xa(%bp),%ax
	sbb    -0x8(%bp),%dx
	mov    %ax,-0x6(%bp)
	mov    $D_0220,%ax
	push   %ax
	pushw  0xc(%bp)
	call   L_155c
	add    $0x4,%sp
	xor    %si,%si
L_1305:
	cmp    -0x6(%bp),%si
	jge   L_1371
	decw   D_0222
	jl   L_1342
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_132a
	mov    0xa(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_1331
L_132a:
	pushw  0xa(%bp)
	call   L_195c
	pop    %cx
L_1331:
	mov    D_0220,%bx
	incw   D_0220
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_136e
L_1342:
	mov    $D_0220,%ax
	push   %ax
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1360
	mov    0xa(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_1367
L_1360:
	pushw  0xa(%bp)
	call   L_195c
	pop    %cx
L_1367:
	push   %ax
	call   L_2812
	add    $0x4,%sp
L_136e:
	inc    %si
	jmp   L_1305
L_1371:
	inc    %di
	jmp   L_129e
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_137c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x16,%sp
L_138b:
	movw   $0x1,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
	xor    %si,%si
	cmp    %si,D_02e2
	jne   L_1415
	xor    %di,%di
L_139f:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_13b9
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_13c0
L_13b9:
	pushw  0x4(%bp)
	call   L_195c
	pop    %cx
L_13c0:
	mov    %ax,-0x12(%bp)
	cmp    $0xa,%ax
	jne   L_13cb
	jmp   L_149b
L_13cb:
	cmpw   $0xffff,-0x12(%bp)
	jne   L_13d5
	xor    %ax,%ax
	jmp   L_143f
L_13d5:
	mov    %di,%ax
	xor    %dx,%dx
	mov    $0x10,%cx
	div    %cx
	mov    %dx,%di
	mov    %dx,%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    -0x12(%bp),%ax
	cwtd
	mov    -0x16(%bp),%cx
	or     %cx,%cx
	je   L_13f8
L_13f2:
	shl    %ax
	rcl    %dx
	loop   L_13f2
L_13f8:
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    -0x1a(%bp),%ax
	adc    -0x18(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	add    $0x7,%di
	jmp   L_139f
L_1415:
	xor    %di,%di
L_1417:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_1431
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_1438
L_1431:
	pushw  0x4(%bp)
	call   L_195c
	pop    %cx
L_1438:
	mov    %ax,-0x12(%bp)
	jmp   L_148e
L_143d:
	xor    %ax,%ax
L_143f:
	jmp   L_1516
L_1442:
	inc    %si
L_1443:
	jmp   L_1417
L_1445:
	or     %si,%si
	je   L_144e
	add    $0x7,%di
	xor    %si,%si
L_144e:
	mov    %di,%ax
	xor    %dx,%dx
	mov    $0x10,%cx
	div    %cx
	mov    %dx,%di
	mov    %dx,%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    -0x12(%bp),%ax
	cwtd
	mov    -0x16(%bp),%cx
	or     %cx,%cx
	je   L_1471
L_146b:
	shl    %ax
	rcl    %dx
	loop   L_146b
L_1471:
	mov    %ax,-0x1a(%bp)
	mov    %dx,-0x18(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	add    -0x1a(%bp),%ax
	adc    -0x18(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	add    $0x7,%di
	jmp   L_1443
L_148e:
	cmp    $0xffff,%ax
	je   L_143d
	cmp    $0x9,%ax
L_1496:
	je   L_1442
	cmp    $0xa,%ax
L_149b:
	je   L_14a4
	cmp    $0x20,%ax
	je   L_1496
	jmp   L_1445
L_14a4:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	and    $0xffff,%ax
	and    $0x0,%dx
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	mov    $0x10,%cx
	or     %cx,%cx
	je   L_14ca
L_14c4:
	sar    %dx
	rcr    %ax
	loop   L_14c4
L_14ca:
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
	mov    -0xc(%bp),%ax
	mov    -0xa(%bp),%dx
	add    -0x10(%bp),%ax
	adc    -0xe(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	and    $0xffff,%ax
	and    $0x0,%dx
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	mov    $0x10,%cx
	or     %cx,%cx
	je   L_1509
L_1503:
	sar    %dx
	rcr    %ax
	loop   L_1503
L_1509:
	cwtd
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
	mov    -0x10(%bp),%ax
	add    -0xc(%bp),%ax
L_1516:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_151d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1529:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	mov    $D_00c2,%ax
	push   %ax
	mov    $D_0228,%ax
	push   %ax
	call   L_15a8
	add    $0x8,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1542:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall exece(59) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    D_02c2,%cx
	/* exece(59) -> execve(11) [TLVC# changed] */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_execve,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_155a
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_155a:
	pop    %bp
	ret


L_155c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_156c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	je   L_159d
	decw   0x2(%si)
	jl   L_158e
	mov    (%si),%bx
	incw   (%si)
	mov    -0x8(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_1598
L_158e:
	push   %si
	pushw  -0x8(%bp)
	call   L_2812
	add    $0x4,%sp
L_1598:
	mov    %ax,-0x6(%bp)
	jmp   L_156c
L_159d:
	mov    -0x6(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_15a8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_1b48
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_15ce
	mov    $0xffff,%ax
	jmp   L_15d0
L_15ce:
	xor    %ax,%ax
L_15d0:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_15d4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	call   L_165a
	mov    %ax,%si
	mov    0x4(%bp),%di
L_15e2:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_15e2
	dec    %di
L_15eb:
	dec    %di
	cmpb   $0x58,(%di)
	jne   L_160e
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
	jmp   L_15eb
L_160e:
	inc    %di
	movw   $0x61,-0x6(%bp)
L_1614:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1644
	add    $0x4,%sp
	cmp    $0xffff,%ax
	je   L_163c
	cmpw   $0x7a,-0x6(%bp)
	jne   L_1630
	mov    $D_00ce,%ax
	jmp   L_163f
L_1630:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%di)
	incw   -0x6(%bp)
	jmp   L_1614
L_163c:
	mov    0x4(%bp),%ax
L_163f:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1644:
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
	jns    L_1658
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1658:
	pop    %bp
	ret


L_165a:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getpid(20) */
	/* getpid -> __NR_getpid */
	mov	$__NR_getpid,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1668
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1668:
	pop    %bp
	ret


L_166a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	cmpb   $0x0,D_00d0
	jne   L_16a7
	movw   $D_0230,-0x8(%bp)
	/* suppressed: movw $D_02b8,D_02b8 (self-ref in .data) */
	movw   $0x88,-0xa(%bp)
L_168d:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_16a2
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_168d
L_16a2:
	movb   $0x1,D_00d0
L_16a7:
	movw   $D_0218,-0x6(%bp)
L_16ac:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_16cb
	mov    -0x6(%bp),%ax
	cmp    D_02b8,%ax
	jb   L_16c5
	xor    %ax,%ax
	jmp   L_1736
L_16c5:
	addw   $0x8,-0x6(%bp)
	jmp   L_16ac
L_16cb:
	cmpb   $0x77,(%di)
	jne   L_16e1
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1768
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_1720
L_16e1:
	cmpb   $0x61,(%di)
	jne   L_1722
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2c78
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_170f
	cmpw   $0x2,D_0302
	jne   L_170f
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1768
	add    $0x4,%sp
	mov    %ax,%si
L_170f:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_1b2a
	add    $0x8,%sp
L_1720:
	jmp   L_1730
L_1722:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2c78
	add    $0x4,%sp
	mov    %ax,%si
L_1730:
	or     %si,%si
	jge   L_1738
	xor    %ax,%ax
L_1736:
	jmp   L_1760
L_1738:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_1756
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_175d
L_1756:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_175d:
	mov    -0x6(%bp),%ax
L_1760:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1768:
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
	jns    L_177c
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_177c:
	pop    %bp
	ret


L_177e:
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
	jne   L_179c
	jmp   L_18e5
L_179c:
	cmpw   $0x2,0xa(%bp)
	jl   L_17a5
	jmp   L_1886
L_17a5:
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_17b1
	jmp   L_1886
L_17b1:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_17c0
	jmp   L_1886
L_17c0:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%si
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,0xa(%bp)
	jne   L_1819
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
	call   L_1b2a
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
	jmp   L_1834
L_1819:
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
L_1834:
	or     %si,%si
	jle   L_1875
	mov    %si,%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jg   L_1875
	jl   L_1847
	cmp    %ax,-0x8(%bp)
	ja   L_1875
L_1847:
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x4(%bp),%bx
	sub    (%bx),%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jl   L_1875
	jg   L_185f
	cmp    %ax,-0x8(%bp)
	jb   L_1875
L_185f:
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	add    %ax,(%bx)
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	sub    %ax,0x2(%bx)
	xor    %ax,%ax
	jmp   L_1926
L_1875:
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x1,%ax
	and    $0x0,%dx
	mov    %ax,%di
	jmp   L_1888
L_1886:
	xor    %di,%di
L_1888:
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
	call   L_1b2a
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
	or     %di,%di
	je   L_18ef
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_18dc
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_1915
L_18dc:
	pushw  0x4(%bp)
	call   L_195c
	pop    %cx
	jmp   L_1915
L_18e5:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
L_18ef:
	je   L_1915
	pushw  0x4(%bp)
	call   L_2900
	pop    %cx
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_1b2a
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_1915:
	cmpw   $0xffff,-0x6(%bp)
	jne   L_1928
	cmpw   $0xffff,-0x8(%bp)
	jne   L_1928
	mov    $0xffff,%ax
L_1926:
	jmp   L_192a
L_1928:
	xor    %ax,%ax
L_192a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1932:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0220,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1b48
	add    $0x6,%sp
	mov    D_0226,%al
	cbtw
	test   $0x20,%ax
	je   L_1956
	mov    $0xffff,%ax
	jmp   L_1958
L_1956:
	xor    %ax,%ax
L_1958:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_195c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_1973
	mov    $0xffff,%ax
	jmp   L_197f
L_1973:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_1981
	mov    $0xffff,%ax
L_197f:
	jmp   L_19f9
L_1981:
	cmpw   $0x0,0x4(%di)
	jne   L_19b5
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_199c
	mov    0x7(%di),%al
	cbtw
	add    $D_0154,%ax
	mov    %ax,0x4(%di)
	jmp   L_1981
L_199c:
	mov    $0x200,%ax
	push   %ax
	call   L_29e6
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_19b1
	orb    $0x4,0x6(%di)
	jmp   L_1981
L_19b1:
	orb    $0x8,0x6(%di)
L_19b5:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_19c8
	mov    $0x1,%ax
	jmp   L_19cb
L_19c8:
	mov    $0x200,%ax
L_19cb:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_2c8e
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_19fb
	cmpw   $0xffff,0x2(%di)
	jne   L_19ed
	orb    $0x10,0x6(%di)
	jmp   L_19f1
L_19ed:
	orb    $0x20,0x6(%di)
L_19f1:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_19f9:
	jmp   L_1a05
L_19fb:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_1a05:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1a0a:
	push   %bp
	mov    %sp,%bp
	mov    0x4(%bp),%cx
	or     %cx,%cx
	jle   L_1a19
	cmp    $0x20,%cx
	jle   L_1a24
L_1a19:
	movw   $0x16,D_0302
	mov    $0xffff,%ax
	jmp   L_1a53
L_1a24:
	mov    0x6(%bp),%dx
	mov    %cx,%bx
	shl    %bx
	add    $D_030a,%bx
	pushw  (%bx)
	mov    %dx,(%bx)
	cmp    $0x2,%dx
	jl   L_1a42
	mov    $0x6,%ax
	mul    %cx
	add    $(L_1a55-6),%ax
	mov    %ax,%dx
L_1a42:
	mov    %cx,%ax
	/* Venix syscall signal(48) */
	/* using SIG_DFL: Venix trampoline incompatible with TLVC far-ptr convention */
	xchg	%ax,%bx
	xor	%cx,%cx
	xor	%dx,%dx
	mov	$__NR_signal,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	pop    %ax
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1a53
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	mov    $0xffff,%ax
L_1a53:
	pop    %bp
	ret
L_1a55:
	push   %ax
	mov    $D_030c,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_030e,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_0310,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_0312,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_0314,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_0316,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_0318,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_031a,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_031c,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_031e,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_0320,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_0322,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_0324,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_0326,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_0328,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_032a,%ax
	jmp   L_1ab9
	push   %ax
	mov    $D_032c,%ax
L_1ab9:
	jmp   L_1b13
	push   %ax
	mov    $D_032e,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_0330,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_0332,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_0334,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_0336,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_0338,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_033a,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_033c,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_033e,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_0340,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_0342,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_0344,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_0346,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_0348,%ax
	jmp   L_1b13
	push   %ax
	mov    $D_034a,%ax
L_1b13:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	sub    $D_030a,%ax
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
	/* .byte 0 (alignment) */


L_1b2a:
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
L_1b45:
	pop    %si
	pop    %bp
	ret


L_1b48:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_1b60:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_1b72
	jmp   L_210f
L_1b72:
	cmpb   $0x25,-0x5(%bp)
	je   L_1b85
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_1b83:
	jmp   L_1b60
L_1b85:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_1bb0
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_2720
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_1bb0:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_1bc9
	incb   -0xa6(%bp)
	inc    %di
L_1bc9:
	cmpb   $0x30,(%di)
	jne   L_1bd4
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_1bd4:
	cmpb   $0x2a,(%di)
	je   L_1c04
	movw   $0x0,-0xe(%bp)
L_1bde:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_1c1a
	cmpw   $0x9,-0xc(%bp)
	jg   L_1c1a
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_1bde
L_1c04:
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
L_1c1a:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_1c61
	cmpb   $0x2a,(%di)
	je   L_1c63
	movw   $0x0,-0x10(%bp)
L_1c33:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_1c5d
	cmpw   $0x9,-0xc(%bp)
	jg   L_1c5d
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_1c33
L_1c5d:
	addw   $0x30,-0xc(%bp)
L_1c61:
	jmp   L_1c7a
L_1c63:
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
L_1c7a:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_2084
L_1c85:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_1c96
L_1c8d:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_1cc6
	jmp   L_1cbf
L_1c96:
	cmp    $0x64,%ax
	je   L_1cc8
	cmp    $0x6f,%ax
	je   L_1cac
	cmp    $0x75,%ax
	je   L_1cbf
	cmp    $0x78,%ax
	je   L_1cba
	jmp   L_1c8d
L_1cac:
	movw   $0x8,-0x12(%bp)
	jmp   L_1cbf
L_1cb3:
	movw   $0x8,-0x12(%bp)
	jmp   L_1cc6
L_1cba:
	movw   $0x10,-0x12(%bp)
L_1cbf:
	jmp   L_1d11
L_1cc1:
	movw   $0x10,-0x12(%bp)
L_1cc6:
	jmp   L_1d21
L_1cc8:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_1ce5
L_1cd8:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_1ce5:
	cmpw   $0x0,-0x14(%bp)
	jg   L_1d2f
	jl   L_1cf3
	cmpw   $0x0,-0x16(%bp)
	jae   L_1d2f
L_1cf3:
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
	jmp   L_1d2f
L_1d11:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_1d2f
L_1d21:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_1d2f:
	cmpw   $0x0,-0x14(%bp)
	jne   L_1d39
	cmpw   $0x0,-0x16(%bp)
L_1d39:
	jne   L_1d4a
	cmpw   $0x0,-0x10(%bp)
	jne   L_1d4a
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_1d4a:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_1d51:
	cmpw   $0x0,-0x14(%bp)
	jne   L_1d5b
	cmpw   $0x0,-0x16(%bp)
L_1d5b:
	je   L_1da6
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_226e
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_1d7c
	mov    $0x30,%ax
	jmp   L_1d7f
L_1d7c:
	mov    $0x57,%ax
L_1d7f:
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
	call   L_2166
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_1d51
L_1da6:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_1df5
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_1da6
L_1dc4:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_1dd0:
	cmpw   $0x2,-0xc(%bp)
	jge   L_1df5
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_1df0
	decw   -0x8(%bp)
L_1df0:
	incw   -0xc(%bp)
	jmp   L_1dd0
L_1df5:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_1dfc:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_1e13
	movw   $0x0,-0xc(%bp)
L_1e13:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_1e22
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_1e22:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_2720
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_1e45:
	jmp   L_1b83
L_1e48:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_1e5a
	movw   $D_0168,-0xa(%bp)
L_1e5a:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_1e69
	movw   $0x7fff,-0xc(%bp)
L_1e69:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_1e6f:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1dfc
	decw   -0xc(%bp)
	jl   L_1dfc
	incw   -0x8(%bp)
	jmp   L_1e6f
L_1e81:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_1e89:
	jmp   L_1e45
L_1e8b:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1e97
	movw   $0x6,-0x10(%bp)
L_1e97:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_246c
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_1ecc
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_1ecc:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_1ee1
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_1ef9
L_1ee1:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_1ee1
L_1ef9:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_1f0c
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_1f0c:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_1f2b
L_1f18:
	decw   -0xc(%bp)
	jl   L_1f2b
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_1f18
L_1f2b:
	cmpw   $0x0,-0xc(%bp)
	jg   L_1f34
	jmp   L_1df5
L_1f34:
	decw   -0xc(%bp)
	jge   L_1f3c
	jmp   L_1df5
L_1f3c:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_1f34
L_1f51:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1f5f
	movw   $0x6,-0x10(%bp)
	jmp   L_1f62
L_1f5f:
	incw   -0x10(%bp)
L_1f62:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2440
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_1f97
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_1f97:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_1fa2
	incw   -0x1a(%bp)
L_1fa2:
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
L_1fc4:
	decw   -0xc(%bp)
	jle   L_1fde
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_1fc4
L_1fde:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_1ffb
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_200c
L_1ffb:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_200c:
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
	jmp   L_1df5
L_203d:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_2049
	movw   $0x6,-0x10(%bp)
L_2049:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_22c4
	add    $0xc,%sp
	add    $0x8,%si
L_2067:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_2067
	decw   -0x8(%bp)
	jmp   L_1df5
L_2078:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_2081:
	jmp   L_1e89
L_2084:
	cmp    $0x65,%ax
	jne   L_208c
	jmp   L_1f51
L_208c:
	jg   L_20cd
	cmp    $0x4f,%ax
	jne   L_2096
	jmp   L_1cac
L_2096:
	jg   L_20ad
	cmp    $0x25,%ax
	je   L_2078
	cmp    $0x44,%ax
	jne   L_20a5
	jmp   L_1cc8
L_20a5:
	cmp    $0x4c,%ax
L_20a8:
	jne   L_20ad
	jmp   L_1c85
L_20ad:
	cmp    $0x55,%ax
	jne   L_20b5
	jmp   L_1d11
L_20b5:
	cmp    $0x58,%ax
	jne   L_20bd
	jmp   L_1cba
L_20bd:
	cmp    $0x63,%ax
	jne   L_20c5
	jmp   L_1dc4
L_20c5:
	cmp    $0x64,%ax
	jne   L_20cd
	jmp   L_1cd8
L_20cd:
	cmp    $0x6f,%ax
	jne   L_20d5
	jmp   L_1cb3
L_20d5:
	jg   L_20ec
	cmp    $0x66,%ax
	jne   L_20df
	jmp   L_1e8b
L_20df:
	cmp    $0x67,%ax
	jne   L_20e7
	jmp   L_203d
L_20e7:
	cmp    $0x6c,%ax
	je   L_20a8
L_20ec:
	cmp    $0x72,%ax
	jne   L_20f4
	jmp   L_1e81
L_20f4:
	cmp    $0x73,%ax
	jne   L_20fc
	jmp   L_1e48
L_20fc:
	cmp    $0x75,%ax
	jne   L_2104
	jmp   L_1d21
L_2104:
	cmp    $0x78,%ax
	jne   L_210c
	jmp   L_1cc1
L_210c:
	jmp   L_2081
L_210f:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_2133
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_2720
	add    $0x8,%sp
L_2133:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_22a2


L_213d:
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
L_2154:
	add    -0x2(%bx),%ax
	mov    %ax,%dx
	mov    -0x4(%bx),%ax
	add    -0x6(%bx),%dx
	add    $0x6,%sp
	ret
	call   L_22a2


L_2166:
	mov    %sp,%bx
L_2168:
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_2184
L_2170:
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
L_2176:
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_2184:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_218d:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_218d
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_219d:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_219d
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_213d
	add    $0x8,%sp
	pop    %bx
L_21c0:
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_21de
	jne   L_21d3
	or     $0x0,%ax
	je   L_21de
L_21d3:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_21de:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_22a2


L_21e6:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_2202
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_2202:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_2219
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_2219:
	push   %dx
	push   %ax
	call   L_2166
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_2233
	not    %ax
	not    %dx
	add    $0x1,%ax
L_2230:
	adc    $0x0,%dx
L_2233:
	ret
	call   L_22a2
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_21e6
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_213d
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_22a2


L_226e:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_2166
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_213d
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_22a2:
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


L_22c4:
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
	call   L_2440
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_22fe
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_22fe:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_2305:
	cmpw   $0x0,-0xa(%bp)
	jle   L_231f
	mov    %di,%ax
	add    -0xa(%bp),%ax
L_2310:
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_231f
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_2305
L_231f:
	cmpw   $0x0,-0x8(%bp)
	jl   L_2330
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_233c
L_2330:
	cmpw   $0x0,-0x8(%bp)
	jge   L_233a
	cmpw   $0xfffd,-0x8(%bp)
L_233a:
	jge   L_23bb
L_233c:
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
L_2357:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_2371
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_2357
L_2371:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_238d
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_2393
L_238d:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_2393:
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
	jmp   L_242b
L_23bb:
	cmpw   $0x0,-0x8(%bp)
	jg   L_23dd
	cmpb   $0x30,(%di)
	je   L_23cc
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_23cc:
	cmpw   $0x0,-0x8(%bp)
	jge   L_23dd
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_23cc
L_23dd:
	movw   $0x1,-0xa(%bp)
L_23e2:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_240a
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
	jne   L_2405
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_2405:
	incw   -0xa(%bp)
	jmp   L_23e2
L_240a:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_242b
L_2412:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_2425
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_2412
L_2425:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_242b:
	cmpb   $0x2e,-0x1(%si)
	jne   L_2432
	dec    %si
L_2432:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2440:
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
	call   L_2497
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_246c:
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
	call   L_2497
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2497:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_24aa
	movw   $0x0,0xc(%bp)
L_24aa:
	cmpw   $0x4f,0xc(%bp)
	jl   L_24b5
	movw   $0x4e,0xc(%bp)
L_24b5:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_0170,%si
	fldl   0x4(%bp)
	fcompl D_01c0
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_24f2
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_24f2:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2d38
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $D_01c0,-0x16(%bp)
	fldl   -0xc(%bp)
	fcompl D_01c8
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_253d
	jmp   L_25ea
L_253d:
	movw   $D_01c0,-0x16(%bp)
L_2542:
	fldl   -0xc(%bp)
	fcompl D_01d0
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_255e
	jmp   L_25e8
L_255e:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	fdivl  D_01d8
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2d38
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   D_01e0
	fstl   -0x1e(%bp)
	fdivl  D_01e8
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  D_01f0
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
	jmp   L_2542
L_25d1:
	cmpw   $D_01c0,-0x16(%bp)
	jae   L_262b
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_25e8:
	jmp   L_25d1
L_25ea:
	fldl   0x4(%bp)
	fcompl D_01f8
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_263a
L_2603:
	fldl   0x4(%bp)
	fmull  D_0200
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl D_0208
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_262b:
	jae   L_263a
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_2603
L_263a:
	mov    0xc(%bp),%ax
	add    $D_0170,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_264c
	add    %di,-0x16(%bp)
L_264c:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_0170,-0x16(%bp)
	jae   L_2662
	movb   $0x0,D_0170
	mov    $D_0170,%ax
	jmp   L_26cd
L_2662:
	cmp    -0x16(%bp),%si
	ja   L_26be
	cmp    $D_01c0,%si
	jae   L_26be
	fldl   0x4(%bp)
	fmull  D_0210
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2d38
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
	jmp   L_2662
L_26be:
	cmpw   $D_01c0,-0x16(%bp)
	jb   L_26cf
	movb   $0x0,D_01bf
	mov    $D_0170,%ax
L_26cd:
	jmp   L_2719
L_26cf:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_26d7:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_2713
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_0170,-0x16(%bp)
	jbe   L_26f6
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_26d7
L_26f6:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_26d7
	cmp    $D_0170,%si
	jbe   L_2710
	movb   $0x30,(%si)
L_2710:
	inc    %si
	jmp   L_26d7
L_2713:
	movb   $0x0,(%si)
	mov    $D_0170,%ax
L_2719:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2720:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_272d:
	cmpw   $0x0,0x8(%bp)
	jl   L_2736
	jmp   L_27c4
L_2736:
	cmpb   $0x2d,(%di)
	jne   L_2773
	cmpw   $0x30,0xc(%bp)
	jne   L_2773
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_2762
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
	jmp   L_2772
L_2762:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_2812
	add    $0x4,%sp
L_2772:
	dec    %si
L_2773:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_2791
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
	jmp   L_279d
L_2791:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_2812
	add    $0x4,%sp
L_279d:
	incw   0x8(%bp)
	jmp   L_272d
L_27a2:
	dec    %si
	jl   L_27d8
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_27c6
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
L_27c4:
	jmp   L_27a2
L_27c6:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_2812
	add    $0x4,%sp
	jmp   L_27a2
L_27d8:
	cmpw   $0x0,0x8(%bp)
	je   L_280d
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_27fc
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
	jmp   L_2808
L_27fc:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_2812
	add    $0x4,%sp
L_2808:
	decw   0x8(%bp)
	jmp   L_27d8
L_280d:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2812:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_282d
	mov    $0xffff,%ax
	jmp   L_28f4
L_282d:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_2860
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
	call   L_2d1e
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_28e5
L_2860:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_28b2
	cmp    $D_0220,%di
	jne   L_288b
	mov    D_0227,%al
	cbtw
	push   %ax
	call   L_2c50
	pop    %cx
	or     %ax,%ax
	je   L_2880
	orb    $0x4,0x6(%di)
	jmp   L_282d
L_2880:
	movw   $D_0352,0x4(%di)
	movw   $D_0352,(%di)
	jmp   L_282d
L_288b:
	mov    $0x200,%ax
	push   %ax
	call   L_29e6
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_28a2
	orb    $0x4,0x6(%di)
	jmp   L_282d
L_28a2:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_28d4
L_28b2:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_28d4
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_2d1e
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_28d4:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_28e5:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_28f6
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_28f4:
	jmp   L_28f9
L_28f6:
	mov    0x4(%bp),%ax
L_28f9:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2900:
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
	jne   L_294c
	mov    0x4(%di),%si
	or     %si,%si
	je   L_294c
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_294c
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_2d1e
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_294c
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_294e
L_294c:
	xor    %ax,%ax
L_294e:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2953:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0218,%di
L_295b:
	cmp    D_02b8,%di
	jae   L_296b
	push   %di
	call   L_296f
	pop    %cx
	add    $0x8,%di
	jmp   L_295b
L_296b:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_296f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_29c2
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_29c2
	push   %di
	call   L_2900
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_2c3c
	pop    %cx
	or     %ax,%ax
	jge   L_29a4
	mov    $0xffff,%si
L_29a4:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_29b4
	pushw  0x4(%di)
	call   L_2b32
	pop    %cx
L_29b4:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_29c2
	movw   $0x0,0x4(%di)
L_29c2:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_29e6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_02ba
	jne   L_2a11
	mov    $D_02bc,%ax
	or     $0x1,%ax
	mov    %ax,D_02ba
	mov    $D_02ba,%ax
	or     $0x1,%ax
	mov    %ax,D_02bc
	/* dual-reloc split: movw $D_02bc,D_0754 */
	mov	$D_02bc,%ax
	mov	%ax,D_0754
	/* dual-reloc split: movw $D_02ba,D_0752 */
	mov	$D_02ba,%ax
	mov	%ax,D_0752
L_2a11:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_0752,%di
L_2a25:
	movw   $0x0,D_02be
L_2a2b:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_2a5a
L_2a32:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_2a41
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_2a32
L_2a41:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_2a5a
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_2a5a
	jmp   L_2b03
L_2a5a:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	jbe   L_2a69
L_2a67:
	jmp   L_2a2b
L_2a69:
	cmp    D_0754,%si
	jne   L_2a75
	cmp    $D_02ba,%di
	je   L_2a79
L_2a75:
	xor    %ax,%ax
	jmp   L_2ab1
L_2a79:
	incw   D_02be
	cmpw   $0x1,D_02be
	jle   L_2a67
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_02be
	xor    %ax,%ax
	push   %ax
	call   L_2c16
	pop    %cx
	mov    %ax,%si
	mov    D_02be,%ax
	shl    %ax
	add    %si,%ax
	add    $0x0,%ax
	cmp    %si,%ax
	jae   L_2ab3
	xor    %ax,%ax
L_2ab1:
	jmp   L_2ac6
L_2ab3:
	mov    D_02be,%ax
	shl    %ax
	push   %ax
	call   L_2c16
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_2ac8
	xor    %ax,%ax
L_2ac6:
	jmp   L_2b2d
L_2ac8:
	mov    D_0754,%bx
	mov    %si,(%bx)
	mov    D_0754,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_2ae6
	mov    D_0754,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_0754,%bx
	mov    %ax,(%bx)
L_2ae6:
	mov    D_02be,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_0754
	mov    $D_02ba,%ax
	or     $0x1,%ax
	mov    D_0754,%bx
	mov    %ax,(%bx)
	jmp   L_2a25
L_2b03:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_0752
	cmp    %ax,%si
	jbe   L_2b22
	mov    D_0752,%bx
	mov    (%bx),%ax
	mov    %ax,D_0756
	mov    D_0752,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_2b22:
	mov    D_0752,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_2b2d:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2b32:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_0752
	mov    (%si),%ax
	and    $0xfffe,%ax
	mov    %ax,(%si)
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2b4e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%di
	mov    -0x2(%di),%ax
	test   $0x1,%ax
	je   L_2b67
	push   %di
	call   L_2b32
	pop    %cx
L_2b67:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_29e6
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_2b86
	cmp    %di,%si
	jne   L_2b8a
L_2b86:
	mov    %si,%ax
	jmp   L_2bf2
L_2b8a:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_2ba9
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_2ba9:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_2bc7
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_2ba9
L_2bc7:
	cmp    %di,%si
	jae   L_2bf0
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_2bf0
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
	mov    D_0756,%ax
	mov    %ax,(%bx)
L_2bf0:
	mov    %si,%ax
L_2bf2:
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
	jns    L_2c0d
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_2c14
L_2c0d:
	mov    0x4(%bp),%bx
	mov    %bx,D_02c0
L_2c14:
	pop    %bp
	ret


L_2c16:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_02c0,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2c2d
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_2c3a
L_2c2d:
	mov    D_02c0,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_02c0
L_2c3a:
	pop    %bp
	ret


L_2c3c:
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
	jns    L_2c4d
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2c4d:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2c50:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2cd7
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_2c6d
	xor    %ax,%ax
	jmp   L_2c70
L_2c6d:
	mov    $0x1,%ax
L_2c70:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2c78:
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
	jns    L_2c8c
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2c8c:
	pop    %bp
	ret


L_2c8e:
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
	jns    L_2ca5
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2ca5:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2ca8:
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
	jns    L_2cbc
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2cbc:
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
	call   L_2cf0
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2cd7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2cf0
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2cf0:
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
	jns    L_2d07
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2d07:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2d0a:
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
	jns    L_2d1b
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2d1b:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2d1e:
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
	jns    L_2d35
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2d35:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2d38:
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


L_2d54:
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
	jns    L_2d65
	neg	%ax
	mov	%ax,D_0302	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2d65:
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
	mov	%ax,D_0302	/* errno */
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
	.byte	0x00, 0x00	/* |..| */
D_0006:
	.word	D_0002	/* ptr: 0x0002 */
D_0008:
	.byte	0x00, 0x00	/* |..| */
D_000a:
	.byte	0x1a, 0x20	/* |. | */
	.word	D_06c7	/* ptr: 0x06c7 */
	.word	D_06fe	/* ptr: 0x06fe */
	.word	D_06fe	/* ptr: 0x06fe */
	.word	D_06b7	/* ptr: 0x06b7 */
	.word	D_06bf	/* ptr: 0x06bf */
	.word	D_06fe	/* ptr: 0x06fe */
	.word	D_06cf	/* ptr: 0x06cf */
	.byte	0x00	/* |.| */
D_001b:
	.byte	0x66, 0x69, 0x6c, 0x65, 0x73	/* |files| */
D_0020:
	.byte	0x20, 0x74, 0x6f, 0x6f, 0x20, 0x62, 0x69, 0x67	/* | too big| */
	.byte	0x2c, 0x20, 0x74, 0x72, 0x79, 0x20, 0x2d, 0x68	/* |, try -h| */
	.byte	0x0a, 0x00	/* |..| */
D_0032:
	.byte	0x2f, 0x74, 0x6d, 0x70, 0x2f, 0x64, 0x58, 0x58	/* |/tmp/dXX| */
	.byte	0x58, 0x58	/* |XX| */
D_003c:
	.byte	0x58, 0x00	/* |X.| */
D_003e:
	.ascii	"cannot create \0"
D_004d:
	.byte	0x72, 0x00	/* |r.| */
D_004f:
	.ascii	"cannot open \0"
D_005c:
	.ascii	"/usr/lib/diffh\0"
D_006b:
	.ascii	"cannot find diffh\0"
D_007d:
	.byte	0x55, 0x73, 0x61, 0x67, 0x65, 0x3a, 0x20, 0x64	/* |Usage: d| */
	.byte	0x69, 0x66, 0x66, 0x20, 0x5b, 0x2d, 0x65, 0x66	/* |iff [-ef| */
	.byte	0x62, 0x68, 0x5d, 0x20, 0x66, 0x69, 0x6c, 0x65	/* |bh] file| */
	.byte	0x31, 0x20, 0x66, 0x69, 0x6c, 0x65, 0x32, 0x0a	/* |1 file2.| */
	.byte	0x00	/* |.| */
D_009e:
	.byte	0x72, 0x00	/* |r.| */
D_00a0:
	.byte	0x72, 0x00	/* |r.| */
D_00a2:
	.byte	0x72, 0x00	/* |r.| */
D_00a4:
	.byte	0x72, 0x00	/* |r.| */
D_00a6:
	.byte	0x2c, 0x00	/* |,.| */
D_00a8:
	.byte	0x2c, 0x00	/* |,.| */
D_00aa:
	.word	D_0020	/* ptr: 0x0020 */
D_00ac:
	.word	D_003c	/* ptr: 0x203c */
	.byte	0x00	/* |.| */
D_00af:
	.byte	0x2d, 0x2d, 0x2d, 0x0a, 0x00	/* |---..| */
D_00b4:
	.word	D_003e	/* ptr: 0x203e */
	.byte	0x00	/* |.| */
D_00b7:
	.byte	0x2e	/* |.| */
	.word	D_000a	/* ptr: 0x000a */
D_00ba:
	.byte	0x25, 0x64, 0x00	/* |%d.| */
D_00bd:
	.ascii	"%s%d\0"
D_00c2:
	.byte	0x64, 0x69, 0x66, 0x66, 0x3a, 0x20, 0x25, 0x73	/* |diff: %s| */
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_00ce:
	.byte	0x2f, 0x00	/* |/.| */
D_00d0:
	.byte	0x00, 0x00, 0x00	/* |...| */
D_00d3:
	.byte	0x20	/* | | */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.word	D_0020	/* ptr: 0x2020 */
	.byte	0x08, 0x08, 0x08, 0x08, 0x08, 0x20	/* |..... | */
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
	.word	D_0410	/* ptr: 0x0410 */
	.word	D_0404	/* ptr: 0x0404 */
	.word	D_0404	/* ptr: 0x0404 */
	.word	D_0404	/* ptr: 0x0404 */
	.word	D_0404	/* ptr: 0x0404 */
	.byte	0x04, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x01, 0x01	/* |AAAAAA..| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x02, 0x02	/* |BBBBBB..| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x10, 0x10, 0x10, 0x10	/* |......| */
	.word	D_0020	/* ptr: 0x0020 */
D_0154:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_0168:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_0170:
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
D_01bf:
	.byte	0x00	/* |.| */
D_01c0:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_01c8:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_01d0:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_01d8:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_01e0:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_01e8:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_01f0:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_01f8:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0200:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0208:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_0210:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0218:
	.byte	0x52, 0x25, 0x00, 0x00	/* |R%..| */
	.word	D_0552	/* ptr: 0x2552 */
	.byte	0x01, 0x00	/* |..| */
D_0220:
	.byte	0x00, 0x00	/* |..| */
D_0222:
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_0226:
	.byte	0x02	/* |.| */
D_0227:
	.byte	0x01	/* |.| */
D_0228:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
D_0230:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_02b8:
	.byte	0xb8, 0x22	/* |."| */
D_02ba:
	.byte	0x00, 0x00	/* |..| */
D_02bc:
	.byte	0x00, 0x00	/* |..| */
D_02be:
	.byte	0x00, 0x00	/* |..| */
D_02c0:
	.word	D_0758	/* ptr: 0x2758 */

	.bss

D_02c2:	.space	2	/* bss word @ 0x02c2 */
D_02c4:	.space	2	/* bss word @ 0x02c4 */
D_02c6:	.space	2	/* bss word @ 0x02c6 */
	.space	2
D_02ca:	.space	2	/* bss word @ 0x02ca */
D_02cc:	.space	2	/* bss word @ 0x02cc */
D_02ce:	.space	2	/* bss word @ 0x02ce */
	.space	8
D_02d8:	.space	2	/* bss word @ 0x02d8 */
D_02da:	.space	2	/* bss word @ 0x02da */
D_02dc:	.space	2	/* bss word @ 0x02dc */
D_02de:	.space	2	/* bss word @ 0x02de */
D_02e0:	.space	2	/* bss word @ 0x02e0 */
D_02e2:	.space	2	/* bss word @ 0x02e2 */
D_02e4:	.space	2	/* bss word @ 0x02e4 */
D_02e6:	.space	2	/* bss word @ 0x02e6 */
D_02e8:	.space	2	/* bss word @ 0x02e8 */
D_02ea:	.space	2	/* bss word @ 0x02ea */
	.space	6
D_02f2:	.space	2	/* bss word @ 0x02f2 */
D_02f4:	.space	2	/* bss word @ 0x02f4 */
D_02f6:	.space	2	/* bss word @ 0x02f6 */
D_02f8:	.space	2	/* bss word @ 0x02f8 */
D_02fa:	.space	2	/* bss word @ 0x02fa */
D_02fc:	.space	2	/* bss word @ 0x02fc */
D_02fe:	.space	2	/* bss word @ 0x02fe */
D_0300:	.space	2	/* bss word @ 0x0300 */
D_0302:	.space	2	/* bss word @ 0x0302 */
	.space	6
D_030a:	.space	2	/* bss word @ 0x030a */
D_030c:	.space	2	/* bss word @ 0x030c */
D_030e:	.space	2	/* bss word @ 0x030e */
D_0310:	.space	2	/* bss word @ 0x0310 */
D_0312:	.space	2	/* bss word @ 0x0312 */
D_0314:	.space	2	/* bss word @ 0x0314 */
D_0316:	.space	2	/* bss word @ 0x0316 */
D_0318:	.space	2	/* bss word @ 0x0318 */
D_031a:	.space	2	/* bss word @ 0x031a */
D_031c:	.space	2	/* bss word @ 0x031c */
D_031e:	.space	2	/* bss word @ 0x031e */
D_0320:	.space	2	/* bss word @ 0x0320 */
D_0322:	.space	2	/* bss word @ 0x0322 */
D_0324:	.space	2	/* bss word @ 0x0324 */
D_0326:	.space	2	/* bss word @ 0x0326 */
D_0328:	.space	2	/* bss word @ 0x0328 */
D_032a:	.space	2	/* bss word @ 0x032a */
D_032c:	.space	2	/* bss word @ 0x032c */
D_032e:	.space	2	/* bss word @ 0x032e */
D_0330:	.space	2	/* bss word @ 0x0330 */
D_0332:	.space	2	/* bss word @ 0x0332 */
D_0334:	.space	2	/* bss word @ 0x0334 */
D_0336:	.space	2	/* bss word @ 0x0336 */
D_0338:	.space	2	/* bss word @ 0x0338 */
D_033a:	.space	2	/* bss word @ 0x033a */
D_033c:	.space	2	/* bss word @ 0x033c */
D_033e:	.space	2	/* bss word @ 0x033e */
D_0340:	.space	2	/* bss word @ 0x0340 */
D_0342:	.space	2	/* bss word @ 0x0342 */
D_0344:	.space	2	/* bss word @ 0x0344 */
D_0346:	.space	2	/* bss word @ 0x0346 */
D_0348:	.space	2	/* bss word @ 0x0348 */
D_034a:	.space	2	/* bss word @ 0x034a */
	.space	6
D_0352:	.space	2	/* bss word @ 0x0352 */
	.space	176
D_0404:	.space	2	/* bss word @ 0x0404 */
	.space	10
D_0410:	.space	2	/* bss word @ 0x0410 */
	.space	320
D_0552:	.space	2	/* bss word @ 0x0552 */
	.space	355
D_06b7:	.space	2	/* bss word @ 0x06b7 */
	.space	6
D_06bf:	.space	2	/* bss word @ 0x06bf */
	.space	6
D_06c7:	.space	2	/* bss word @ 0x06c7 */
	.space	6
D_06cf:	.space	2	/* bss word @ 0x06cf */
	.space	45
D_06fe:	.space	2	/* bss word @ 0x06fe */
	.space	82
D_0752:	.space	2	/* bss word @ 0x0752 */
D_0754:	.space	2	/* bss word @ 0x0754 */
D_0756:	.space	2	/* bss word @ 0x0756 */
D_0758:	/* endcore: initial break */

/* end of file */
