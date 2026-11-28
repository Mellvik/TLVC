/* join.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 join.s -o join.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: join  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00001efa  (7930 bytes)
//   a_data:   0x000001f0  (496 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xe,%sp
L_0045:
	cmpw   $0x1,0x4(%bp)
	jg   L_004e
	jmp   L_01df
L_004e:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x2d,(%bx)
	je   L_005c
	jmp   L_01df
L_005c:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x0,0x1(%bx)
	jne   L_006b
	jmp   L_01df
L_006b:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	mov    0x1(%bx),%al
	cbtw
	jmp   L_01b0
L_0078:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	mov    0x2(%bx),%al
	cbtw
	jmp   L_009c
L_0084:
	orw    $0x1,D_02a0
	jmp   L_00ca
L_008c:
	orw    $0x2,D_02a0
	jmp   L_00ca
L_0094:
	orw    $0x3,D_02a0
	jmp   L_00ca
L_009c:
	cmp    $0x31,%ax
	je   L_0084
	cmp    $0x32,%ax
	je   L_008c
	jmp   L_0094
L_00a8:
	mov    0x6(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,D_000c
	addw   $0x2,0x6(%bp)
	decw   0x4(%bp)
	jmp   L_00ca
L_00ba:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	mov    0x2(%bx),%al
	cbtw
	mov    %ax,D_000a
	mov    %ax,D_0008
L_00ca:
	jmp   L_01ae
L_00cd:
	movw   $0x0,D_0704
L_00d3:
	cmpw   $0x28,D_0704
	jl   L_00dd
	jmp   L_01ae
L_00dd:
	mov    0x6(%bp),%bx
	mov    0x4(%bx),%bx
	cmpb   $0x31,(%bx)
	jne   L_0119
	mov    0x6(%bp),%bx
	mov    0x4(%bx),%bx
	cmpb   $0x2e,0x1(%bx)
	jne   L_0119
	mov    D_0704,%bx
	shl    %bx
	movw   $0x0,D_01f8(%bx)
	mov    0x6(%bp),%bx
	mov    0x4(%bx),%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_0836
	pop    %cx
	mov    D_0704,%bx
	shl    %bx
	mov    %ax,D_0250(%bx)
	jmp   L_0153
L_0119:
	mov    0x6(%bp),%bx
	mov    0x4(%bx),%bx
	cmpb   $0x32,(%bx)
	jne   L_012e
	mov    0x6(%bp),%bx
	mov    0x4(%bx),%bx
	cmpb   $0x2e,0x1(%bx)
L_012e:
	jne   L_01ae
	mov    0x6(%bp),%bx
	mov    0x4(%bx),%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_0836
	pop    %cx
	mov    D_0704,%bx
	shl    %bx
	mov    %ax,D_0250(%bx)
	mov    D_0704,%bx
	shl    %bx
	movw   $0x1,D_01f8(%bx)
L_0153:
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
	incw   D_0704
	jmp   L_00d3
L_0161:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x31,0x2(%bx)
	jne   L_017c
	mov    0x6(%bp),%bx
	pushw  0x4(%bx)
	call   L_0836
	pop    %cx
	mov    %ax,D_0004
	jmp   L_01a7
L_017c:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x32,0x2(%bx)
	jne   L_0197
	mov    0x6(%bp),%bx
	pushw  0x4(%bx)
	call   L_0836
	pop    %cx
	mov    %ax,D_0006
	jmp   L_01a7
L_0197:
	mov    0x6(%bp),%bx
	pushw  0x4(%bx)
	call   L_0836
	pop    %cx
	mov    %ax,D_0006
	mov    %ax,D_0004
L_01a7:
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
L_01ae:
	jmp   L_01d5
L_01b0:
	cmp    $0x61,%ax
	jne   L_01b8
	jmp   L_0078
L_01b8:
	cmp    $0x65,%ax
	jne   L_01c0
	jmp   L_00a8
L_01c0:
	cmp    $0x6a,%ax
	je   L_0161
	cmp    $0x6f,%ax
	jne   L_01cd
	jmp   L_00cd
L_01cd:
	cmp    $0x74,%ax
	jne   L_01d5
	jmp   L_00ba
L_01d5:
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
	jmp   L_0045
L_01df:
	movw   $0x0,-0x6(%bp)
L_01e4:
	mov    -0x6(%bp),%ax
	cmp    D_0704,%ax
	jge   L_01fb
	mov    -0x6(%bp),%bx
	shl    %bx
	decw   D_0250(%bx)
	incw   -0x6(%bp)
	jmp   L_01e4
L_01fb:
	cmpw   $0x3,0x4(%bp)
	je   L_0209
	mov    $D_000f,%ax
	push   %ax
	call   L_06ed
	pop    %cx
L_0209:
	decw   D_0004
	decw   D_0006
	mov    D_0004,%bx
	shl    %bx
	mov    D_06b0(%bx),%ax
	mov    %ax,D_0700
	mov    D_0006,%bx
	shl    %bx
	mov    D_06d8(%bx),%ax
	mov    %ax,D_0702
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x2d,(%bx)
	jne   L_023e
	/* dual-reloc split: movw $D_0146,D_06a8 */
	mov	$D_0146,%ax
	mov	%ax,D_06a8
	jmp   L_0265
L_023e:
	mov    $D_003f,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	call   L_0904
	add    $0x4,%sp
	mov    %ax,D_06a8
	or     %ax,%ax
	jne   L_0265
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	mov    $D_0041,%ax
	push   %ax
	call   L_06ed
	add    $0x4,%sp
L_0265:
	mov    $D_004f,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  0x4(%bx)
	call   L_0904
	add    $0x4,%sp
	mov    %ax,D_06aa
	or     %ax,%ax
	jne   L_028c
	mov    0x6(%bp),%bx
	pushw  0x4(%bx)
	mov    $D_0051,%ax
	push   %ax
	call   L_06ed
	add    $0x4,%sp
L_028c:
	xor    %ax,%ax
	push   %ax
	call   L_04c5
	pop    %cx
	mov    %ax,-0x8(%bp)
	pushw  D_06aa
	call   L_0756
	pop    %cx
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    $0x1,%ax
	push   %ax
	call   L_04c5
	pop    %cx
	mov    %ax,-0xa(%bp)
L_02af:
	cmpw   $0x0,-0x8(%bp)
	jle   L_02bb
	cmpw   $0x0,-0xa(%bp)
	jg   L_02d0
L_02bb:
	cmpw   $0x0,D_02a0
	jne   L_02c5
	jmp   L_04bc
L_02c5:
	mov    -0x8(%bp),%ax
	add    -0xa(%bp),%ax
	jg   L_02d0
	jmp   L_04bc
L_02d0:
	cmpw   $0x0,-0x8(%bp)
	jle   L_02fa
	cmpw   $0x0,-0xa(%bp)
	jle   L_02fa
	mov    D_0006,%bx
	shl    %bx
	pushw  D_06d8(%bx)
	mov    D_0004,%bx
	shl    %bx
	pushw  D_06b0(%bx)
	call   L_073a
	add    $0x4,%sp
	or     %ax,%ax
	jg   L_0300
L_02fa:
	cmpw   $0x0,-0x8(%bp)
	jne   L_032f
L_0300:
	testw  $0x2,D_02a0
	je   L_0314
	pushw  -0xa(%bp)
	xor    %ax,%ax
	push   %ax
	call   L_058b
	add    $0x4,%sp
L_0314:
	pushw  D_06aa
	call   L_0756
	pop    %cx
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    $0x1,%ax
	push   %ax
	call   L_04c5
	pop    %cx
	mov    %ax,-0xa(%bp)
L_032d:
	jmp   L_02af
L_032f:
	cmpw   $0x0,-0x8(%bp)
	jle   L_0359
	cmpw   $0x0,-0xa(%bp)
	jle   L_0359
	mov    D_0006,%bx
	shl    %bx
	pushw  D_06d8(%bx)
	mov    D_0004,%bx
	shl    %bx
	pushw  D_06b0(%bx)
	call   L_073a
	add    $0x4,%sp
	or     %ax,%ax
	jl   L_035f
L_0359:
	cmpw   $0x0,-0xa(%bp)
	jne   L_037f
L_035f:
	testw  $0x1,D_02a0
	je   L_0373
	xor    %ax,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_058b
	add    $0x4,%sp
L_0373:
	xor    %ax,%ax
	push   %ax
	call   L_04c5
	pop    %cx
	mov    %ax,-0x8(%bp)
L_037d:
	jmp   L_032d
L_037f:
	cmpw   $0x0,-0xa(%bp)
	jle   L_03ca
	mov    D_0006,%bx
	shl    %bx
	pushw  D_06d8(%bx)
	mov    D_0004,%bx
	shl    %bx
	pushw  D_06b0(%bx)
	call   L_073a
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_03ca
	pushw  -0xa(%bp)
	pushw  -0x8(%bp)
	call   L_058b
	add    $0x4,%sp
	pushw  D_06aa
	call   L_0756
	pop    %cx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    $0x1,%ax
	push   %ax
	call   L_04c5
	pop    %cx
	mov    %ax,-0xa(%bp)
	jmp   L_037f
L_03ca:
	xor    %ax,%ax
	push   %ax
	pushw  -0x10(%bp)
	pushw  -0x12(%bp)
	pushw  D_06aa
	call   L_0a18
	add    $0x8,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_04c5
	pop    %cx
	mov    %ax,-0xa(%bp)
	xor    %ax,%ax
	push   %ax
	call   L_04c5
	pop    %cx
	mov    %ax,-0x8(%bp)
L_03f2:
	cmpw   $0x0,-0x8(%bp)
	jle   L_0435
	cmpw   $0x0,-0xa(%bp)
	jle   L_0435
	mov    D_0006,%bx
	shl    %bx
	pushw  D_06d8(%bx)
	mov    D_0004,%bx
	shl    %bx
	pushw  D_06b0(%bx)
	call   L_073a
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0435
	pushw  -0xa(%bp)
	pushw  -0x8(%bp)
	call   L_058b
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_04c5
	pop    %cx
	mov    %ax,-0xa(%bp)
L_0433:
	jmp   L_03f2
L_0435:
	cmpw   $0x0,-0x8(%bp)
	jle   L_045f
	cmpw   $0x0,-0xa(%bp)
	jle   L_045f
	mov    D_0006,%bx
	shl    %bx
	pushw  D_06d8(%bx)
	mov    D_0004,%bx
	shl    %bx
	pushw  D_06b0(%bx)
	call   L_073a
	add    $0x4,%sp
	or     %ax,%ax
	jl   L_0465
L_045f:
	cmpw   $0x0,-0xa(%bp)
	jne   L_048f
L_0465:
	xor    %ax,%ax
	push   %ax
	pushw  -0x10(%bp)
	pushw  -0x12(%bp)
	pushw  D_06aa
	call   L_0a18
	add    $0x8,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_04c5
	pop    %cx
	mov    %ax,-0xa(%bp)
	xor    %ax,%ax
	push   %ax
	call   L_04c5
	pop    %cx
	mov    %ax,-0x8(%bp)
	jmp   L_0433
L_048f:
	xor    %ax,%ax
	push   %ax
	pushw  -0xc(%bp)
	pushw  -0xe(%bp)
	pushw  D_06aa
	call   L_0a18
	add    $0x8,%sp
	mov    -0xe(%bp),%ax
	mov    -0xc(%bp),%dx
	mov    %ax,-0x12(%bp)
	mov    %dx,-0x10(%bp)
	mov    $0x1,%ax
	push   %ax
	call   L_04c5
	pop    %cx
	mov    %ax,-0xa(%bp)
	jmp   L_037d
L_04bc:
	xor    %ax,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_04c5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
L_04d4:
	mov    0x4(%bp),%ax
	mov    $0x9,%cx
	shl    %cl,%ax
	add    $D_02a8,%ax
	mov    %ax,-0x6(%bp)
	mov    0x4(%bp),%ax
	mov    $0x28,%dx
	imul   %dx
	add    $D_06b0,%ax
	mov    %ax,-0x8(%bp)
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_06a8(%bx)
	mov    $0x200,%ax
	push   %ax
	pushw  -0x6(%bp)
	call   L_08a2
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_050e
	xor    %ax,%ax
	jmp   L_0584
L_050e:
	xor    %di,%di
L_0510:
	cmpw   $0x20,D_0008
	jne   L_0530
L_0517:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    D_0008,%ax
	je   L_052b
	cmp    D_000a,%si
	jne   L_0538
L_052b:
	incw   -0x6(%bp)
	jmp   L_0517
L_0530:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
L_0538:
	cmp    $0xa,%si
	je   L_057b
	or     %si,%si
	je   L_057b
	mov    -0x8(%bp),%bx
	mov    -0x6(%bp),%ax
	mov    %ax,(%bx)
	addw   $0x2,-0x8(%bp)
L_054d:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    D_0008,%ax
	je   L_056f
	cmp    $0xa,%si
	je   L_056f
	cmp    D_000a,%si
	je   L_056f
	or     %si,%si
	je   L_056f
	incw   -0x6(%bp)
	jmp   L_054d
L_056f:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x6(%bp)
	inc    %di
	jmp   L_0510
L_057b:
	mov    -0x8(%bp),%bx
	movw   $0x0,(%bx)
	mov    %di,%ax
L_0584:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_058b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
L_059a:
	cmpw   $0x0,D_0704
	jle   L_05a4
	jmp   L_063a
L_05a4:
	cmpw   $0x0,0x4(%bp)
	je   L_05b6
	mov    D_0004,%bx
	shl    %bx
	mov    D_06b0(%bx),%ax
	jmp   L_05c0
L_05b6:
	mov    D_0006,%bx
	shl    %bx
	mov    D_06d8(%bx),%ax
L_05c0:
	push   %ax
	mov    $D_005f,%ax
	push   %ax
	call   L_0bcc
	add    $0x4,%sp
	movw   $0x0,-0x6(%bp)
L_05d0:
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jge   L_05fd
	mov    -0x6(%bp),%ax
	cmp    D_0004,%ax
	je   L_05f8
	mov    -0x6(%bp),%bx
	shl    %bx
	pushw  D_06b0(%bx)
	pushw  D_0008
	mov    $D_0062,%ax
	push   %ax
	call   L_0bcc
	add    $0x6,%sp
L_05f8:
	incw   -0x6(%bp)
	jmp   L_05d0
L_05fd:
	movw   $0x0,-0x6(%bp)
L_0602:
	mov    -0x6(%bp),%ax
	cmp    0x6(%bp),%ax
	jge   L_062f
	mov    -0x6(%bp),%ax
	cmp    D_0006,%ax
	je   L_062a
	mov    -0x6(%bp),%bx
	shl    %bx
	pushw  D_06d8(%bx)
	pushw  D_0008
	mov    $D_0067,%ax
	push   %ax
	call   L_0bcc
	add    $0x6,%sp
L_062a:
	incw   -0x6(%bp)
	jmp   L_0602
L_062f:
	mov    $D_006c,%ax
	push   %ax
	call   L_0bcc
	pop    %cx
	jmp   L_06e6
L_063a:
	movw   $0x0,-0x6(%bp)
L_063f:
	mov    -0x6(%bp),%ax
	cmp    D_0704,%ax
	jl   L_064b
	jmp   L_06e6
L_064b:
	mov    -0x6(%bp),%bx
	shl    %bx
	mov    D_01f8(%bx),%ax
	mov    $0x28,%dx
	imul   %dx
	add    $D_06b0,%ax
	mov    -0x6(%bp),%bx
	shl    %bx
	mov    D_0250(%bx),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,-0x8(%bp)
	mov    -0x6(%bp),%bx
	shl    %bx
	cmpw   $0x0,D_01f8(%bx)
	jne   L_068a
	mov    -0x6(%bp),%bx
	shl    %bx
	mov    0x4(%bp),%ax
	cmp    D_0250(%bx),%ax
	jle   L_06ac
L_068a:
	mov    -0x6(%bp),%bx
	shl    %bx
	cmpw   $0x1,D_01f8(%bx)
	jne   L_06a4
	mov    -0x6(%bp),%bx
	shl    %bx
	mov    0x6(%bp),%ax
	cmp    D_0250(%bx),%ax
	jle   L_06ac
L_06a4:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	jne   L_06b2
L_06ac:
	mov    D_000c,%ax
	mov    %ax,-0x8(%bp)
L_06b2:
	pushw  -0x8(%bp)
	mov    $D_006e,%ax
	push   %ax
	call   L_0bcc
	add    $0x4,%sp
	mov    D_0704,%ax
	dec    %ax
	cmp    %ax,-0x6(%bp)
	jne   L_06d2
	mov    $D_0071,%ax
	push   %ax
	call   L_0bcc
	pop    %cx
	jmp   L_06e0
L_06d2:
	pushw  D_0008
	mov    $D_0073,%ax
	push   %ax
	call   L_0bcc
	add    $0x4,%sp
L_06e0:
	incw   -0x6(%bp)
	jmp   L_063f
L_06e6:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_06ed:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_06f9:
	mov    $D_0076,%ax
	push   %ax
	mov    $D_0156,%ax
	push   %ax
	call   L_080a
	add    $0x4,%sp
	pushw  0xc(%bp)
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	mov    $D_0156,%ax
	push   %ax
	call   L_080a
	add    $0xc,%sp
	mov    $D_007d,%ax
	push   %ax
	mov    $D_0156,%ax
	push   %ax
	call   L_080a
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_073a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0746:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_1db6
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0756:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x2(%bx)
	jge   L_076f
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
L_076f:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_0787
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%ax
	neg    %ax
	mov    %ax,%di
	jmp   L_07c0
L_0787:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
	je   L_07b9
	xor    %di,%di
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	je   L_07c0
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	jne   L_07c0
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	mov    0x4(%bp),%bx
	sub    0x4(%bx),%ax
	mov    %ax,%di
	jmp   L_07c0
L_07b9:
	mov    $0xffff,%ax
	mov    %ax,%dx
	jmp   L_0802
L_07c0:
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
	call   L_0ca4
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmp    $0x0,%dx
	jg   L_07f3
	jl   L_07eb
	cmpw   $0x0,-0x8(%bp)
	jae   L_07f3
L_07eb:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	jmp   L_0802
L_07f3:
	mov    %di,%ax
	cwtd
	add    %ax,-0x8(%bp)
	adc    %dx,-0x6(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
L_0802:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_080a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_0cc2
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_0830
	mov    $0xffff,%ax
	jmp   L_0832
L_0830:
	xor    %ax,%ax
L_0832:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0836:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	xor    %si,%si
	mov    %si,-0x6(%bp)
L_0845:
	mov    (%di),%al
	cbtw
	jmp   L_0850
L_084a:
	incw   -0x6(%bp)
L_084d:
	inc    %di
	jmp   L_0869
L_0850:
	cmp    $0x9,%ax
	je   L_0866
	cmp    $0x20,%ax
	je   L_0866
	cmp    $0x2b,%ax
	je   L_084d
	cmp    $0x2d,%ax
	je   L_084a
	jmp   L_0869
L_0866:
	inc    %di
	jmp   L_0845
L_0869:
	cmpb   $0x30,(%di)
	jl   L_088f
	cmpb   $0x39,(%di)
	jg   L_088f
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
	jmp   L_0869
L_088f:
	cmpw   $0x0,-0x6(%bp)
	je   L_089b
	mov    %si,%ax
	neg    %ax
	jmp   L_089d
L_089b:
	mov    %si,%ax
L_089d:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_08a2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x8(%bp),%di
	mov    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
L_08b2:
	decw   0x6(%bp)
	jle   L_08e2
	decw   0x2(%di)
	jl   L_08c8
	mov    (%di),%bx
	incw   (%di)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_08cd
L_08c8:
	push   %di
	call   L_0bf6
	pop    %cx
L_08cd:
	mov    %ax,%si
	or     %ax,%ax
	jl   L_08e2
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	cmp    $0xa,%si
	jne   L_08b2
L_08e2:
	or     %si,%si
	jge   L_08f2
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jne   L_08f2
	xor    %ax,%ax
	jmp   L_08fe
L_08f2:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x6(%bp)
	mov    0x4(%bp),%ax
L_08fe:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0904:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	cmpb   $0x0,D_0080
	jne   L_0941
	movw   $D_015e,-0x8(%bp)
	/* suppressed: movw $D_01e6,D_01e6 (self-ref in .data) */
	movw   $0x88,-0xa(%bp)
L_0927:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_093c
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_0927
L_093c:
	movb   $0x1,D_0080
L_0941:
	movw   $D_0146,-0x6(%bp)
L_0946:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_0965
	mov    -0x6(%bp),%ax
	cmp    D_01e6,%ax
	jb   L_095f
	xor    %ax,%ax
	jmp   L_09d0
L_095f:
	addw   $0x8,-0x6(%bp)
	jmp   L_0946
L_0965:
	cmpb   $0x77,(%di)
	jne   L_097b
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0a02
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_09ba
L_097b:
	cmpb   $0x61,(%di)
	jne   L_09bc
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1e34
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_09a9
	cmpw   $0x2,D_0706
	jne   L_09a9
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0a02
	add    $0x4,%sp
	mov    %ax,%si
L_09a9:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_0ca4
	add    $0x8,%sp
L_09ba:
	jmp   L_09ca
L_09bc:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1e34
	add    $0x4,%sp
	mov    %ax,%si
L_09ca:
	or     %si,%si
	jge   L_09d2
	xor    %ax,%ax
L_09d0:
	jmp   L_09fa
L_09d2:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_09f0
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_09f7
L_09f0:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_09f7:
	mov    -0x6(%bp),%ax
L_09fa:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0a02:
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
	jns    L_0a16
	neg	%ax
	mov	%ax,D_0706	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_0a16:
	pop    %bp
	ret


L_0a18:
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
	jne   L_0a36
	jmp   L_0b7f
L_0a36:
	cmpw   $0x2,0xa(%bp)
	jl   L_0a3f
	jmp   L_0b20
L_0a3f:
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_0a4b
	jmp   L_0b20
L_0a4b:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0a5a
	jmp   L_0b20
L_0a5a:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%si
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,0xa(%bp)
	jne   L_0ab3
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
	call   L_0ca4
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
	jmp   L_0ace
L_0ab3:
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
L_0ace:
	or     %si,%si
	jle   L_0b0f
	mov    %si,%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jg   L_0b0f
	jl   L_0ae1
	cmp    %ax,-0x8(%bp)
	ja   L_0b0f
L_0ae1:
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x4(%bp),%bx
	sub    (%bx),%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jl   L_0b0f
	jg   L_0af9
	cmp    %ax,-0x8(%bp)
	jb   L_0b0f
L_0af9:
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	add    %ax,(%bx)
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	sub    %ax,0x2(%bx)
	xor    %ax,%ax
	jmp   L_0bc0
L_0b0f:
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x1,%ax
	and    $0x0,%dx
	mov    %ax,%di
	jmp   L_0b22
L_0b20:
	xor    %di,%di
L_0b22:
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
	call   L_0ca4
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
	or     %di,%di
	je   L_0b89
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_0b76
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0baf
L_0b76:
	pushw  0x4(%bp)
	call   L_0bf6
	pop    %cx
	jmp   L_0baf
L_0b7f:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
L_0b89:
	je   L_0baf
	pushw  0x4(%bp)
	call   L_1a7a
	pop    %cx
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_0ca4
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_0baf:
	cmpw   $0xffff,-0x6(%bp)
	jne   L_0bc2
	cmpw   $0xffff,-0x8(%bp)
	jne   L_0bc2
	mov    $0xffff,%ax
L_0bc0:
	jmp   L_0bc4
L_0bc2:
	xor    %ax,%ax
L_0bc4:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0bcc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_014e,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0cc2
	add    $0x6,%sp
	mov    D_0154,%al
	cbtw
	test   $0x20,%ax
	je   L_0bf0
	mov    $0xffff,%ax
	jmp   L_0bf2
L_0bf0:
	xor    %ax,%ax
L_0bf2:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0bf6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_0c0d
	mov    $0xffff,%ax
	jmp   L_0c19
L_0c0d:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_0c1b
	mov    $0xffff,%ax
L_0c19:
	jmp   L_0c93
L_0c1b:
	cmpw   $0x0,0x4(%di)
	jne   L_0c4f
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0c36
	mov    0x7(%di),%al
	cbtw
	add    $D_0082,%ax
	mov    %ax,0x4(%di)
	jmp   L_0c1b
L_0c36:
	mov    $0x200,%ax
	push   %ax
	call   L_1b60
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_0c4b
	orb    $0x4,0x6(%di)
	jmp   L_0c1b
L_0c4b:
	orb    $0x8,0x6(%di)
L_0c4f:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0c62
	mov    $0x1,%ax
	jmp   L_0c65
L_0c62:
	mov    $0x200,%ax
L_0c65:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1e4a
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_0c95
	cmpw   $0xffff,0x2(%di)
	jne   L_0c87
	orb    $0x10,0x6(%di)
	jmp   L_0c8b
L_0c87:
	orb    $0x20,0x6(%di)
L_0c8b:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_0c93:
	jmp   L_0c9f
L_0c95:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_0c9f:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0ca4:
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
L_0cbf:
	pop    %si
	pop    %bp
	ret


L_0cc2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0cda:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_0cec
	jmp   L_1289
L_0cec:
	cmpb   $0x25,-0x5(%bp)
	je   L_0cff
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_0cfd:
	jmp   L_0cda
L_0cff:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0d2a
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_189a
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0d2a:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_0d43
	incb   -0xa6(%bp)
	inc    %di
L_0d43:
	cmpb   $0x30,(%di)
	jne   L_0d4e
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_0d4e:
	cmpb   $0x2a,(%di)
	je   L_0d7e
	movw   $0x0,-0xe(%bp)
L_0d58:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0d94
	cmpw   $0x9,-0xc(%bp)
	jg   L_0d94
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_0d58
L_0d7e:
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
L_0d94:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_0ddb
	cmpb   $0x2a,(%di)
	je   L_0ddd
	movw   $0x0,-0x10(%bp)
L_0dad:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0dd7
	cmpw   $0x9,-0xc(%bp)
	jg   L_0dd7
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_0dad
L_0dd7:
	addw   $0x30,-0xc(%bp)
L_0ddb:
	jmp   L_0df4
L_0ddd:
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
L_0df4:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_11fe
L_0dff:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_0e10
L_0e07:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_0e40
	jmp   L_0e39
L_0e10:
	cmp    $0x64,%ax
	je   L_0e42
	cmp    $0x6f,%ax
	je   L_0e26
	cmp    $0x75,%ax
	je   L_0e39
	cmp    $0x78,%ax
	je   L_0e34
	jmp   L_0e07
L_0e26:
	movw   $0x8,-0x12(%bp)
	jmp   L_0e39
L_0e2d:
	movw   $0x8,-0x12(%bp)
	jmp   L_0e40
L_0e34:
	movw   $0x10,-0x12(%bp)
L_0e39:
	jmp   L_0e8b
L_0e3b:
	movw   $0x10,-0x12(%bp)
L_0e40:
	jmp   L_0e9b
L_0e42:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0e5f
L_0e52:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0e5f:
	cmpw   $0x0,-0x14(%bp)
	jg   L_0ea9
	jl   L_0e6d
	cmpw   $0x0,-0x16(%bp)
	jae   L_0ea9
L_0e6d:
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
	jmp   L_0ea9
L_0e8b:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0ea9
L_0e9b:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0ea9:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0eb3
	cmpw   $0x0,-0x16(%bp)
L_0eb3:
	jne   L_0ec4
	cmpw   $0x0,-0x10(%bp)
	jne   L_0ec4
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_0ec4:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0ecb:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0ed5
	cmpw   $0x0,-0x16(%bp)
L_0ed5:
	je   L_0f20
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_13e8
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_0ef6
	mov    $0x30,%ax
	jmp   L_0ef9
L_0ef6:
	mov    $0x57,%ax
L_0ef9:
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
	call   L_12e0
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_0ecb
L_0f20:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_0f6f
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_0f20
L_0f3e:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_0f4a:
	cmpw   $0x2,-0xc(%bp)
	jge   L_0f6f
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0f6a
	decw   -0x8(%bp)
L_0f6a:
	incw   -0xc(%bp)
	jmp   L_0f4a
L_0f6f:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0f76:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_0f8d
	movw   $0x0,-0xc(%bp)
L_0f8d:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_0f9c
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_0f9c:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_189a
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0fbf:
	jmp   L_0cfd
L_0fc2:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_0fd4
	movw   $D_0096,-0xa(%bp)
L_0fd4:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_0fe3
	movw   $0x7fff,-0xc(%bp)
L_0fe3:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0fe9:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0f76
	decw   -0xc(%bp)
	jl   L_0f76
	incw   -0x8(%bp)
	jmp   L_0fe9
L_0ffb:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_1003:
	jmp   L_0fbf
L_1005:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1011
	movw   $0x6,-0x10(%bp)
L_1011:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_15e6
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_1046
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_1046:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_105b
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_1073
L_105b:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_105b
L_1073:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_1086
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_1086:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_10a5
L_1092:
	decw   -0xc(%bp)
	jl   L_10a5
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_1092
L_10a5:
	cmpw   $0x0,-0xc(%bp)
	jg   L_10ae
	jmp   L_0f6f
L_10ae:
	decw   -0xc(%bp)
	jge   L_10b6
	jmp   L_0f6f
L_10b6:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_10ae
L_10cb:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_10d9
	movw   $0x6,-0x10(%bp)
	jmp   L_10dc
L_10d9:
	incw   -0x10(%bp)
L_10dc:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_15ba
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_1111
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_1111:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_111c
	incw   -0x1a(%bp)
L_111c:
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
L_113e:
	decw   -0xc(%bp)
	jle   L_1158
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_113e
L_1158:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_1175
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_1186
L_1175:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_1186:
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
	jmp   L_0f6f
L_11b7:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_11c3
	movw   $0x6,-0x10(%bp)
L_11c3:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_143e
	add    $0xc,%sp
	add    $0x8,%si
L_11e1:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_11e1
	decw   -0x8(%bp)
	jmp   L_0f6f
L_11f2:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_11fb:
	jmp   L_1003
L_11fe:
	cmp    $0x65,%ax
	jne   L_1206
	jmp   L_10cb
L_1206:
	jg   L_1247
	cmp    $0x4f,%ax
	jne   L_1210
	jmp   L_0e26
L_1210:
	jg   L_1227
	cmp    $0x25,%ax
	je   L_11f2
	cmp    $0x44,%ax
	jne   L_121f
	jmp   L_0e42
L_121f:
	cmp    $0x4c,%ax
L_1222:
	jne   L_1227
	jmp   L_0dff
L_1227:
	cmp    $0x55,%ax
	jne   L_122f
	jmp   L_0e8b
L_122f:
	cmp    $0x58,%ax
	jne   L_1237
	jmp   L_0e34
L_1237:
	cmp    $0x63,%ax
	jne   L_123f
	jmp   L_0f3e
L_123f:
	cmp    $0x64,%ax
	jne   L_1247
	jmp   L_0e52
L_1247:
	cmp    $0x6f,%ax
	jne   L_124f
	jmp   L_0e2d
L_124f:
	jg   L_1266
	cmp    $0x66,%ax
	jne   L_1259
	jmp   L_1005
L_1259:
	cmp    $0x67,%ax
	jne   L_1261
	jmp   L_11b7
L_1261:
	cmp    $0x6c,%ax
	je   L_1222
L_1266:
	cmp    $0x72,%ax
	jne   L_126e
	jmp   L_0ffb
L_126e:
	cmp    $0x73,%ax
	jne   L_1276
	jmp   L_0fc2
L_1276:
	cmp    $0x75,%ax
	jne   L_127e
	jmp   L_0e9b
L_127e:
	cmp    $0x78,%ax
	jne   L_1286
	jmp   L_0e3b
L_1286:
	jmp   L_11fb
L_1289:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_12ad
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_189a
	add    $0x8,%sp
L_12ad:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_141c


L_12b7:
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
	call   L_141c


L_12e0:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_12fe
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_12fe:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_1307:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_1307
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_1317:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_1317
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_12b7
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_1358
	jne   L_134d
	or     $0x0,%ax
	je   L_1358
L_134d:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_1358:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_141c


L_1360:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_137c
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_137c:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_1393
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_1393:
	push   %dx
	push   %ax
	call   L_12e0
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_13ad
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_13ad:
	ret
	call   L_141c
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_1360
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_12b7
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_141c


L_13e8:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_12e0
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_12b7
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_141c:
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


L_143e:
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
	call   L_15ba
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_1478
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_1478:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_147f:
	cmpw   $0x0,-0xa(%bp)
	jle   L_1499
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_1499
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_147f
L_1499:
	cmpw   $0x0,-0x8(%bp)
	jl   L_14aa
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_14b6
L_14aa:
	cmpw   $0x0,-0x8(%bp)
	jge   L_14b4
	cmpw   $0xfffd,-0x8(%bp)
L_14b4:
	jge   L_1535
L_14b6:
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
L_14d1:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_14eb
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_14d1
L_14eb:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_1507
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_150d
L_1507:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_150d:
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
	jmp   L_15a5
L_1535:
	cmpw   $0x0,-0x8(%bp)
	jg   L_1557
	cmpb   $0x30,(%di)
	je   L_1546
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1546:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1557
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_1546
L_1557:
	movw   $0x1,-0xa(%bp)
L_155c:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_1584
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
	jne   L_157f
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_157f:
	incw   -0xa(%bp)
	jmp   L_155c
L_1584:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_15a5
L_158c:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_159f
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_158c
L_159f:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_15a5:
	cmpb   $0x2e,-0x1(%si)
	jne   L_15ac
	dec    %si
L_15ac:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_15ba:
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
	call   L_1611
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_15e6:
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
	call   L_1611
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1611:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_1624
	movw   $0x0,0xc(%bp)
L_1624:
	cmpw   $0x4f,0xc(%bp)
	jl   L_162f
	movw   $L_004e,0xc(%bp)
L_162f:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_009e,%si
	fldl   0x4(%bp)
	fcompl D_00ee
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_166c
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_166c:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1eca
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $D_00ee,-0x16(%bp)
	fldl   -0xc(%bp)
	fcompl D_00f6
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_16b7
	jmp   L_1764
L_16b7:
	movw   $D_00ee,-0x16(%bp)
L_16bc:
	fldl   -0xc(%bp)
	fcompl D_00fe
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_16d8
	jmp   L_1762
L_16d8:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	fdivl  D_0106
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1eca
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   D_010e
	fstl   -0x1e(%bp)
	fdivl  D_0116
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  D_011e
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
	jmp   L_16bc
L_174b:
	cmpw   $D_00ee,-0x16(%bp)
	jae   L_17a5
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_1762:
	jmp   L_174b
L_1764:
	fldl   0x4(%bp)
	fcompl D_0126
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_17b4
L_177d:
	fldl   0x4(%bp)
	fmull  D_012e
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl D_0136
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_17a5:
	jae   L_17b4
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_177d
L_17b4:
	mov    0xc(%bp),%ax
	add    $D_009e,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_17c6
	add    %di,-0x16(%bp)
L_17c6:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_009e,-0x16(%bp)
	jae   L_17dc
	movb   $0x0,D_009e
	mov    $D_009e,%ax
	jmp   L_1847
L_17dc:
	cmp    -0x16(%bp),%si
	ja   L_1838
	cmp    $D_00ee,%si
	jae   L_1838
	fldl   0x4(%bp)
	fmull  D_013e
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1eca
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
	jmp   L_17dc
L_1838:
	cmpw   $D_00ee,-0x16(%bp)
	jb   L_1849
	movb   $0x0,D_00ed
	mov    $D_009e,%ax
L_1847:
	jmp   L_1893
L_1849:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_1851:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_188d
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_009e,-0x16(%bp)
	jbe   L_1870
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_1851
L_1870:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_1851
	cmp    $D_009e,%si
	jbe   L_188a
	movb   $0x30,(%si)
L_188a:
	inc    %si
	jmp   L_1851
L_188d:
	movb   $0x0,(%si)
	mov    $D_009e,%ax
L_1893:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_189a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_18a7:
	cmpw   $0x0,0x8(%bp)
	jl   L_18b0
	jmp   L_193e
L_18b0:
	cmpb   $0x2d,(%di)
	jne   L_18ed
	cmpw   $0x30,0xc(%bp)
	jne   L_18ed
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_18dc
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
	jmp   L_18ec
L_18dc:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_198c
	add    $0x4,%sp
L_18ec:
	dec    %si
L_18ed:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_190b
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
	jmp   L_1917
L_190b:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_198c
	add    $0x4,%sp
L_1917:
	incw   0x8(%bp)
	jmp   L_18a7
L_191c:
	dec    %si
	jl   L_1952
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1940
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
L_193e:
	jmp   L_191c
L_1940:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_198c
	add    $0x4,%sp
	jmp   L_191c
L_1952:
	cmpw   $0x0,0x8(%bp)
	je   L_1987
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1976
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
	jmp   L_1982
L_1976:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_198c
	add    $0x4,%sp
L_1982:
	decw   0x8(%bp)
	jmp   L_1952
L_1987:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_198c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_19a7
	mov    $0xffff,%ax
	jmp   L_1a6e
L_19a7:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_19da
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
	call   L_1eb0
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_1a5f
L_19da:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1a2c
	cmp    $D_014e,%di
	jne   L_1a05
	mov    D_0155,%al
	cbtw
	push   %ax
	call   L_1e0c
	pop    %cx
	or     %ax,%ax
	je   L_19fa
	orb    $0x4,0x6(%di)
	jmp   L_19a7
L_19fa:
	movw   $D_0708,0x4(%di)
	movw   $D_0708,(%di)
	jmp   L_19a7
L_1a05:
	mov    $0x200,%ax
	push   %ax
	call   L_1b60
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1a1c
	orb    $0x4,0x6(%di)
	jmp   L_19a7
L_1a1c:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_1a4e
L_1a2c:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_1a4e
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1eb0
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_1a4e:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_1a5f:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_1a70
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_1a6e:
	jmp   L_1a73
L_1a70:
	mov    0x4(%bp),%ax
L_1a73:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1a7a:
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
	jne   L_1ac6
	mov    0x4(%di),%si
	or     %si,%si
	je   L_1ac6
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_1ac6
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1eb0
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_1ac6
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_1ac8
L_1ac6:
	xor    %ax,%ax
L_1ac8:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1acd:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0146,%di
L_1ad5:
	cmp    D_01e6,%di
	jae   L_1ae5
	push   %di
	call   L_1ae9
	pop    %cx
	add    $0x8,%di
	jmp   L_1ad5
L_1ae5:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ae9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_1b3c
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_1b3c
	push   %di
	call   L_1a7a
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1df8
	pop    %cx
	or     %ax,%ax
	jge   L_1b1e
	mov    $0xffff,%si
L_1b1e:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_1b2e
	pushw  0x4(%di)
	call   L_1cac
	pop    %cx
L_1b2e:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_1b3c
	movw   $0x0,0x4(%di)
L_1b3c:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_1b60:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_01e8
	jne   L_1b8b
	mov    $D_01ea,%ax
	or     $0x1,%ax
	mov    %ax,D_01e8
	mov    $D_01e8,%ax
	or     $0x1,%ax
	mov    %ax,D_01ea
	/* dual-reloc split: movw $D_01ea,D_0b0a */
	mov	$D_01ea,%ax
	mov	%ax,D_0b0a
	/* dual-reloc split: movw $D_01e8,D_0b08 */
	mov	$D_01e8,%ax
	mov	%ax,D_0b08
L_1b8b:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_0b08,%di
L_1b9f:
	movw   $0x0,D_01ec
L_1ba5:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_1bd4
L_1bac:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_1bbb
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_1bac
L_1bbb:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_1bd4
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_1bd4
	jmp   L_1c7d
L_1bd4:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	jbe   L_1be3
L_1be1:
	jmp   L_1ba5
L_1be3:
	cmp    D_0b0a,%si
	jne   L_1bef
	cmp    $D_01e8,%di
	je   L_1bf3
L_1bef:
	xor    %ax,%ax
	jmp   L_1c2b
L_1bf3:
	incw   D_01ec
	cmpw   $0x1,D_01ec
	jle   L_1be1
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_01ec
	xor    %ax,%ax
	push   %ax
	call   L_1d90
	pop    %cx
	mov    %ax,%si
	mov    D_01ec,%ax
	shl    %ax
	add    %si,%ax
	add    $0x0,%ax
	cmp    %si,%ax
	jae   L_1c2d
	xor    %ax,%ax
L_1c2b:
	jmp   L_1c40
L_1c2d:
	mov    D_01ec,%ax
	shl    %ax
	push   %ax
	call   L_1d90
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_1c42
	xor    %ax,%ax
L_1c40:
	jmp   L_1ca7
L_1c42:
	mov    D_0b0a,%bx
	mov    %si,(%bx)
	mov    D_0b0a,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_1c60
	mov    D_0b0a,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_0b0a,%bx
	mov    %ax,(%bx)
L_1c60:
	mov    D_01ec,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_0b0a
	mov    $D_01e8,%ax
	or     $0x1,%ax
	mov    D_0b0a,%bx
	mov    %ax,(%bx)
	jmp   L_1b9f
L_1c7d:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_0b08
	cmp    %ax,%si
	jbe   L_1c9c
	mov    D_0b08,%bx
	mov    (%bx),%ax
	mov    %ax,D_0b0c
	mov    D_0b08,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_1c9c:
	mov    D_0b08,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_1ca7:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1cac:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_0b08
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
	je   L_1ce1
	push   %di
	call   L_1cac
	pop    %cx
L_1ce1:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_1b60
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_1d00
	cmp    %di,%si
	jne   L_1d04
L_1d00:
	mov    %si,%ax
	jmp   L_1d6c
L_1d04:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_1d23
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_1d23:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_1d41
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_1d23
L_1d41:
	cmp    %di,%si
	jae   L_1d6a
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_1d6a
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
	mov    D_0b0c,%ax
	mov    %ax,(%bx)
L_1d6a:
	mov    %si,%ax
L_1d6c:
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
	jns    L_1d87
	neg	%ax
	mov	%ax,D_0706	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1d8e
L_1d87:
	mov    0x4(%bp),%bx
	mov    %bx,D_01ee
L_1d8e:
	pop    %bp
	ret


L_1d90:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_01ee,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1da7
	neg	%ax
	mov	%ax,D_0706	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1db4
L_1da7:
	mov    D_01ee,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_01ee
L_1db4:
	pop    %bp
	ret


L_1db6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1dc3:
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_1de2
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_1dc3
	xor    %ax,%ax
	jmp   L_1df3
L_1de2:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_1df3:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1df8:
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
	jns    L_1e09
	neg	%ax
	mov	%ax,D_0706	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1e09:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1e0c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1e7d
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_1e29
	xor    %ax,%ax
	jmp   L_1e2c
L_1e29:
	mov    $0x1,%ax
L_1e2c:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1e34:
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
	jns    L_1e48
	neg	%ax
	mov	%ax,D_0706	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1e48:
	pop    %bp
	ret


L_1e4a:
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
	jns    L_1e61
	neg	%ax
	mov	%ax,D_0706	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1e61:
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
	call   L_1e96
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e7d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1e96
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e96:
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
	jns    L_1ead
	neg	%ax
	mov	%ax,D_0706	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1ead:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1eb0:
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
	jns    L_1ec7
	neg	%ax
	mov	%ax,D_0706	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1ec7:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1eca:
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


L_1ee6:
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
	jns    L_1ef7
	neg	%ax
	mov	%ax,D_0706	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1ef7:
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
	mov	%ax,D_0706	/* errno */
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
	.byte	0x01, 0x00	/* |..| */
D_0006:
	.byte	0x01, 0x00	/* |..| */
D_0008:
	.byte	0x20, 0x00	/* | .| */
D_000a:
	.byte	0x09, 0x00	/* |..| */
D_000c:
	.byte	0x0e, 0x20, 0x00	/* |. .| */
D_000f:
	.ascii	"usage: join [-j1 x -j2 y] [-o list] file1 file2\0"
D_003f:
	.byte	0x72, 0x00	/* |r.| */
D_0041:
	.ascii	"can't open %s\0"
D_004f:
	.byte	0x72, 0x00	/* |r.| */
D_0051:
	.ascii	"can't open %s\0"
D_005f:
	.byte	0x25	/* |%| */
	.word	D_0073	/* ptr: 0x0073 */
D_0062:
	.ascii	"%c%s\0"
D_0067:
	.ascii	"%c%s\0"
D_006c:
	.word	D_000a	/* ptr: 0x000a */
D_006e:
	.byte	0x25, 0x73	/* |%s| */
	.byte	0x00	/* |.| */
D_0071:
	.byte	0x0a	/* |.| */
	.byte	0x00	/* |.| */
D_0073:
	.byte	0x25, 0x63, 0x00	/* |%c.| */
D_0076:
	.ascii	"join: \0"
D_007d:
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_0080:
	.byte	0x00, 0x00	/* |..| */
D_0082:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_0096:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_009e:
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
D_00ed:
	.byte	0x00	/* |.| */
D_00ee:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_00f6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_00fe:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0106:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_010e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_0116:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_011e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0126:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_012e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0136:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_013e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0146:
	.word	D_0908	/* ptr: 0x2908 */
	.byte	0x00, 0x00	/* |..| */
	.word	D_0908	/* ptr: 0x2908 */
	.byte	0x01, 0x00	/* |..| */
D_014e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_0154:
	.byte	0x02	/* |.| */
D_0155:
	.byte	0x01	/* |.| */
D_0156:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.word	D_0206	/* ptr: 0x0206 */
D_015e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_01e6:
	.byte	0xe6, 0x21	/* |.!| */
D_01e8:
	.byte	0x00, 0x00	/* |..| */
D_01ea:
	.byte	0x00, 0x00	/* |..| */
D_01ec:
	.byte	0x00, 0x00	/* |..| */
D_01ee:
	.word	D_0b0e	/* ptr: 0x2b0e */

	.bss

	.space	8
D_01f8:	.space	2	/* bss word @ 0x01f8 */
	.space	12
D_0206:	.space	2	/* bss word @ 0x0206 */
	.space	72
D_0250:	.space	2	/* bss word @ 0x0250 */
	.space	78
D_02a0:	.space	2	/* bss word @ 0x02a0 */
	.space	6
D_02a8:	.space	2	/* bss word @ 0x02a8 */
	.space	1022
D_06a8:	.space	2	/* bss word @ 0x06a8 */
D_06aa:	.space	2	/* bss word @ 0x06aa */
	.space	4
D_06b0:	.space	2	/* bss word @ 0x06b0 */
	.space	38
D_06d8:	.space	2	/* bss word @ 0x06d8 */
	.space	38
D_0700:	.space	2	/* bss word @ 0x0700 */
D_0702:	.space	2	/* bss word @ 0x0702 */
D_0704:	.space	2	/* bss word @ 0x0704 */
D_0706:	.space	2	/* bss word @ 0x0706 */
D_0708:	.space	2	/* bss word @ 0x0708 */
	.space	510
D_0908:	.space	2	/* bss word @ 0x0908 */
	.space	510
D_0b08:	.space	2	/* bss word @ 0x0b08 */
D_0b0a:	.space	2	/* bss word @ 0x0b0a */
D_0b0c:	.space	2	/* bss word @ 0x0b0c */
D_0b0e:	/* endcore: initial break */

/* end of file */
