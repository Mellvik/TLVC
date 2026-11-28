/* grep.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 grep.s -o grep.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: grep  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00001f54  (8020 bytes)
//   a_data:   0x000002d6  (726 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0042:
	decw   0x4(%bp)
	jle   L_0099
	addw   $0x2,0x6(%bp)
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	cmpb   $0x2d,(%bx)
	jne   L_0099
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	mov    0x1(%bx),%al
	cbtw
	jmp   L_00aa
	incw   D_0418
L_0064:
	jmp   L_0042
	movw   $0x0,D_0004
	jmp   L_0042
	incw   D_03fc
	jmp   L_0042
	incw   D_0404
	jmp   L_0042
	incw   D_03fa
	jmp   L_0042
	incw   D_02d8
	jmp   L_0042
	incw   D_03fe
	jmp   L_0042
	incw   D_02de
	jmp   L_0042
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
L_0099:
	jmp   L_00b9
L_009b:
	xor    %ax,%ax
	push   %ax
	mov    $D_0064,%ax
	push   %ax
	call   L_08f5
	add    $0x4,%sp
	jmp   L_0064
L_00aa:
	sub    $0x62,%ax
	cmp    $0x17,%ax
	ja   L_009b
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x200e(%bx)
L_00b9:
	cmpw   $0x0,0x4(%bp)
	jg   L_00c7
	mov    $0x2,%ax
	push   %ax
	call   _exit
	pop    %cx
L_00c7:
	cmpw   $0x0,D_0418
	jne   L_00d1
	jmp   L_018c
L_00d1:
	mov    $D_0626,%si
	mov    0x6(%bp),%bx
	mov    (%bx),%di
L_00d9:
	cmpb   $0x0,(%di)
	jne   L_00e1
	jmp   L_0182
L_00e1:
	cmpb   $0x5c,(%di)
	jne   L_0107
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	cmpb   $0x0,(%di)
	je   L_0114
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_015a
L_0107:
	cmpb   $0x5b,(%di)
	jne   L_0125
L_010c:
	cmpb   $0x0,(%di)
	je   L_0114
	cmpb   $0x5d,(%di)
L_0114:
	je   L_015a
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_010c
L_0125:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	mov    D_00e7(%bx),%al
	cbtw
	test   $0x2,%ax
	je   L_015c
	mov    %si,%bx
	inc    %si
	movb   $0x5b,(%bx)
	mov    (%di),%al
	cbtw
	sub    $0x20,%ax
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
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
	movb   $0x5d,(%bx)
L_015a:
	jmp   L_0169
L_015c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
L_0169:
	cmp    $D_0721,%si
	jae   L_0172
	jmp   L_00d9
L_0172:
	xor    %ax,%ax
	push   %ax
	mov    $D_0078,%ax
	push   %ax
	call   L_08f5
	add    $0x4,%sp
	jmp   L_00d9
L_0182:
	movb   $0x0,(%si)
	mov    0x6(%bp),%bx
	movw   $D_0626,(%bx)
L_018c:
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_01e7
	pop    %cx
	decw   0x4(%bp)
	mov    0x4(%bp),%ax
	mov    %ax,D_03f8
	cmpw   $0x0,0x4(%bp)
	jg   L_01bc
	cmpw   $0x0,D_02d8
	je   L_01b3
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_01b3:
	xor    %ax,%ax
	push   %ax
	call   L_043f
	pop    %cx
	jmp   L_01d0
L_01bc:
	decw   0x4(%bp)
	jl   L_01d0
	addw   $0x2,0x6(%bp)
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_043f
	pop    %cx
	jmp   L_01bc
L_01d0:
	cmpw   $0x0,D_0400
	jne   L_01dc
	mov    $0x1,%ax
	jmp   L_01de
L_01dc:
	xor    %ax,%ax
L_01de:
	push   %ax
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_01e7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x18,%sp
L_01f6:
	mov    $D_02e6,%si
	mov    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
	movw   $0x0,-0xa(%bp)
	lea    -0x15(%bp),%ax
	mov    %ax,-0x18(%bp)
	movb   $0x0,-0x1b(%bp)
	movw   $0x0,-0x1a(%bp)
	mov    -0x6(%bp),%bx
	cmpb   $0x5e,(%bx)
	jne   L_0222
	incw   D_0402
	incw   -0x6(%bp)
L_0222:
	cmp    $D_03e6,%si
	jae   L_028b
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	mov    (%bx),%al
	cbtw
	mov    %ax,%di
	cmp    $0x2a,%ax
	je   L_023b
	mov    %si,-0xa(%bp)
L_023b:
	mov    %di,%ax
	jmp   L_03f6
L_0240:
	mov    %si,%bx
	inc    %si
	movb   $0xb,(%bx)
	jmp   L_0438
L_0249:
	mov    %si,%bx
	inc    %si
	movb   $0x4,(%bx)
	jmp   L_0222
L_0251:
	cmpw   $0x0,-0xa(%bp)
	je   L_0265
	mov    -0xa(%bp),%bx
	cmpb   $0x1,(%bx)
	je   L_0265
	mov    -0xa(%bp),%bx
	cmpb   $0xc,(%bx)
L_0265:
	jne   L_026a
	jmp   L_040f
L_026a:
	mov    -0xa(%bp),%bx
	orb    $0x1,(%bx)
	jmp   L_0222
L_0272:
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_027d
	jmp   L_040f
L_027d:
	mov    %si,%bx
	inc    %si
	movb   $0xa,(%bx)
	jmp   L_0222
L_0285:
	lea    0x11(%si),%ax
	cmp    $D_03e6,%ax
L_028b:
	jb   L_0290
	jmp   L_042b
L_0290:
	mov    %si,%bx
	inc    %si
	movb   $0x6,(%bx)
	movb   $0x0,-0x1c(%bp)
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	mov    (%bx),%al
	cbtw
	mov    %ax,%di
	cmp    $0x5e,%ax
	jne   L_02b9
	movb   $0x1,-0x1c(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%di
	incw   -0x6(%bp)
L_02b9:
	mov    -0x6(%bp),%ax
	mov    %ax,-0x8(%bp)
L_02bf:
	or     %di,%di
	jne   L_02c6
	jmp   L_042b
L_02c6:
	cmp    $0x2d,%di
	jne   L_030f
	mov    -0x6(%bp),%ax
	cmp    -0x8(%bp),%ax
	jbe   L_030f
	mov    -0x6(%bp),%bx
	cmpb   $0x5d,(%bx)
	je   L_030f
	mov    -0x6(%bp),%bx
	mov    -0x2(%bx),%al
	cbtw
	mov    %ax,%di
L_02e4:
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	cmp    %ax,%di
	jge   L_030c
	mov    %di,%ax
	and    $0x7,%ax
	mov    %ax,%bx
	mov    D_0006(%bx),%al
	cbtw
	mov    %di,%dx
	mov    $0x3,%cx
	sar    %cl,%dx
	add    %si,%dx
	mov    %dx,%bx
	mov    %ax,%cx
	or     %cl,(%bx)
	inc    %di
	jmp   L_02e4
L_030c:
	incw   -0x6(%bp)
L_030f:
	mov    %di,%ax
	and    $0x7,%ax
	mov    %ax,%bx
	mov    D_0006(%bx),%al
	cbtw
	mov    %di,%dx
	mov    $0x3,%cx
	sar    %cl,%dx
	add    %si,%dx
	mov    %dx,%bx
	mov    %ax,%cx
	or     %cl,(%bx)
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	mov    (%bx),%al
	cbtw
	mov    %ax,%di
	cmp    $0x5d,%ax
	jne   L_02bf
	cmpb   $0x0,-0x1c(%bp)
	je   L_035d
	movw   $0x0,-0xc(%bp)
L_0345:
	cmpw   $0x10,-0xc(%bp)
	jge   L_035a
	mov    %si,%ax
	add    -0xc(%bp),%ax
	mov    %ax,%bx
	xorb   $0xff,(%bx)
	incw   -0xc(%bp)
	jmp   L_0345
L_035a:
	andb   $0xfe,(%si)
L_035d:
	add    $0x10,%si
	jmp   L_0222
L_0363:
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	mov    (%bx),%al
	cbtw
	mov    %ax,%di
	cmp    $0x28,%ax
	jne   L_0398
	cmpb   $0x9,-0x1b(%bp)
	jge   L_03d4
	mov    -0x18(%bp),%bx
	mov    -0x1b(%bp),%al
	mov    %al,(%bx)
	incw   -0x18(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x1,(%bx)
	mov    %si,%bx
	inc    %si
	mov    -0x1b(%bp),%al
	mov    %al,(%bx)
	incb   -0x1b(%bp)
	jmp   L_0222
L_0398:
	cmp    $0x29,%di
	jne   L_03c4
	lea    -0x15(%bp),%ax
	cmp    %ax,-0x18(%bp)
	ja   L_03a8
	jmp   L_042b
L_03a8:
	mov    %si,%bx
	inc    %si
	movb   $0xc,(%bx)
	decw   -0x18(%bp)
	mov    -0x18(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x1a(%bp)
	jmp   L_0222
L_03c4:
	cmp    $0x31,%di
	jl   L_03e6
	cmp    $0x39,%di
	jg   L_03e6
	sub    $0x31,%di
	cmp    -0x1a(%bp),%di
L_03d4:
	jge   L_042b
	mov    %si,%bx
	inc    %si
	movb   $0x12,(%bx)
	mov    %si,%bx
	inc    %si
	mov    %di,%cx
	mov    %cl,(%bx)
	jmp   L_0222
L_03e6:
	mov    %si,%bx
	inc    %si
	movb   $0x2,(%bx)
	mov    %si,%bx
	inc    %si
	mov    %di,%cx
	mov    %cl,(%bx)
	jmp   L_0222
L_03f6:
	cmp    $0x2a,%ax
	jne   L_03fe
	jmp   L_0251
L_03fe:
	jg   L_0411
	or     %ax,%ax
	jne   L_0407
	jmp   L_0240
L_0407:
	cmp    $0x24,%ax
	jne   L_040f
	jmp   L_0272
L_040f:
	jmp   L_03e6
L_0411:
	cmp    $0x2e,%ax
	jne   L_0419
	jmp   L_0249
L_0419:
	cmp    $0x5b,%ax
	jne   L_0421
	jmp   L_0285
L_0421:
	cmp    $0x5c,%ax
	jne   L_0429
	jmp   L_0363
L_0429:
	jmp   L_03e6
L_042b:
	xor    %ax,%ax
	push   %ax
	mov    $D_0091,%ax
	push   %ax
	call   L_08f5
	add    $0x4,%sp
L_0438:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_043f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_044c:
	cmpw   $0x0,0x4(%bp)
	je   L_0474
	mov    $D_022c,%ax
	push   %ax
	mov    $D_00a1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_091e
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_0474
	pushw  0x4(%bp)
	mov    $D_00a3,%ax
	push   %ax
	call   L_08f5
	add    $0x4,%sp
L_0474:
	movw   $0x0,D_041a
	movw   $0x0,D_041c
	movw   $0x0,D_02da
	movw   $0x0,D_02dc
L_048c:
	addw   $0x1,D_041a
	adcw   $0x0,D_041c
	mov    $D_041e,%di
L_0499:
	decw   D_022e
	jl   L_04af
	mov    D_022c,%bx
	incw   D_022c
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_04b7
L_04af:
	mov    $D_022c,%ax
	push   %ax
	call   L_0c92
	pop    %cx
L_04b7:
	mov    %ax,-0x6(%bp)
	cmp    $0xa,%ax
	je   L_0505
	cmpw   $0xffff,-0x6(%bp)
	jne   L_04f5
	cmpw   $0x0,D_03fe
	je   L_04f2
	cmpw   $0x1,D_03f8
	jle   L_04e0
	pushw  0x4(%bp)
	mov    $D_00b8,%ax
	push   %ax
	call   L_0c68
	add    $0x4,%sp
L_04e0:
	pushw  D_02dc
	pushw  D_02da
	mov    $D_00bc,%ax
	push   %ax
	call   L_0c68
	add    $0x6,%sp
L_04f2:
	jmp   L_0588
L_04f5:
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	cmp    $D_061d,%di
	jb   L_0499
L_0505:
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	mov    $D_041e,%di
	mov    $D_02e6,%si
	cmpw   $0x0,D_0402
	je   L_0526
	push   %si
	push   %di
	call   L_058d
	add    $0x4,%sp
	or     %ax,%ax
	je   L_054e
	jmp   L_0578
L_0526:
	cmpb   $0x2,(%si)
	jne   L_0550
	mov    0x1(%si),%al
	cbtw
	mov    %ax,-0x6(%bp)
L_0532:
	mov    (%di),%al
	cbtw
	cmp    -0x6(%bp),%ax
	jne   L_0546
	push   %si
	push   %di
	call   L_058d
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0578
L_0546:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_0532
L_054e:
	jmp   L_0564
L_0550:
	push   %si
	push   %di
	call   L_058d
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0578
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_0550
L_0564:
	cmpw   $0x0,D_0404
	jne   L_056e
	jmp   L_048c
L_056e:
	pushw  0x4(%bp)
	call   L_07ec
	pop    %cx
L_0575:
	jmp   L_048c
L_0578:
	cmpw   $0x0,D_0404
	jne   L_0575
	pushw  0x4(%bp)
	call   L_07ec
	pop    %cx
	jmp   L_0575
L_0588:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_058d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
L_059c:
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_05a4:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	jmp   L_07d8
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
	cmp    %dx,%ax
	je   L_05a4
	xor    %ax,%ax
	jmp   L_05df
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_05a4
	xor    %ax,%ax
	jmp   L_05df
	cmpb   $0x0,(%di)
	je   L_05a4
	xor    %ax,%ax
	jmp   L_05df
	mov    $0x1,%ax
L_05df:
	jmp   L_0624
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	and    $0x7f,%ax
	mov    %ax,%cx
	mov    %cl,-0x7(%bp)
	mov    %cl,%al
	cbtw
	mov    $0x3,%cx
	sar    %cl,%ax
	add    %si,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    -0x7(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	and    $0x7,%dx
	mov    %dx,%bx
	mov    D_0006(%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	test   %ax,%dx
	je   L_0622
	add    $0x10,%si
L_0620:
	jmp   L_05a4
L_0622:
	xor    %ax,%ax
L_0624:
	jmp   L_0664
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    %di,D_03e6(%bx)
	jmp   L_0620
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    %di,D_0406(%bx)
L_0644:
	jmp   L_0620
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_03e6(%bx),%ax
	mov    %ax,-0xa(%bp)
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x0,D_0406(%bx)
	jne   L_0666
	xor    %ax,%ax
L_0664:
	jmp   L_0692
L_0666:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_0406(%bx),%ax
	sub    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
	pushw  -0xc(%bp)
	push   %di
	pushw  -0xa(%bp)
	call   L_08b8
	add    $0x6,%sp
	or     %ax,%ax
	je   L_0690
	add    -0xc(%bp),%di
L_068e:
	jmp   L_0644
L_0690:
	xor    %ax,%ax
L_0692:
	jmp   L_06b2
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_03e6(%bx),%ax
	mov    %ax,-0xa(%bp)
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x0,D_0406(%bx)
	jne   L_06b4
	xor    %ax,%ax
L_06b2:
	jmp   L_06f5
L_06b4:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_0406(%bx),%ax
	sub    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
	mov    %di,-0x6(%bp)
L_06cb:
	pushw  -0xc(%bp)
	push   %di
	pushw  -0xa(%bp)
	call   L_08b8
	add    $0x6,%sp
	or     %ax,%ax
	je   L_06e1
	add    -0xc(%bp),%di
	jmp   L_06cb
L_06e1:
	cmp    -0x6(%bp),%di
	jb   L_06fc
	push   %si
	push   %di
	call   L_058d
	add    $0x4,%sp
	or     %ax,%ax
	je   L_06f7
	mov    $0x1,%ax
L_06f5:
	jmp   L_06fe
L_06f7:
	sub    -0xc(%bp),%di
	jmp   L_06e1
L_06fc:
	xor    %ax,%ax
L_06fe:
	jmp   L_079d
	mov    %di,-0x6(%bp)
L_0704:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_0725
	jmp   L_0704
	mov    %di,-0x6(%bp)
L_0711:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    (%si),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_0711
	inc    %si
L_0725:
	jmp   L_0769
	mov    %di,-0x6(%bp)
L_072a:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	and    $0x7f,%ax
	mov    %ax,%cx
	mov    %cl,-0x7(%bp)
	mov    %cl,%al
	cbtw
	mov    $0x3,%cx
	sar    %cl,%ax
	add    %si,%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    -0x7(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	and    $0x7,%dx
	mov    %dx,%bx
	mov    D_0006(%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	test   %ax,%dx
	jne   L_072a
	add    $0x10,%si
L_0769:
	dec    %di
	cmp    -0x6(%bp),%di
	jne   L_0772
	jmp   L_068e
L_0772:
	cmpb   $0x2,(%si)
	jne   L_07ab
	mov    0x1(%si),%al
	mov    %al,-0x7(%bp)
L_077d:
	mov    (%di),%al
	cbtw
	mov    -0x7(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_079f
	push   %si
	push   %di
	call   L_058d
	add    $0x4,%sp
	or     %ax,%ax
	je   L_079f
	mov    $0x1,%ax
L_079d:
	jmp   L_07c6
L_079f:
	mov    %di,%ax
	dec    %di
	cmp    -0x6(%bp),%ax
	ja   L_077d
	xor    %ax,%ax
	jmp   L_07c6
L_07ab:
	push   %si
	push   %di
	call   L_058d
	add    $0x4,%sp
	or     %ax,%ax
	je   L_07bc
	mov    $0x1,%ax
	jmp   L_07c6
L_07bc:
	mov    %di,%ax
	dec    %di
	cmp    -0x6(%bp),%ax
	ja   L_07ab
	xor    %ax,%ax
L_07c6:
	jmp   L_07e5
L_07c8:
	xor    %ax,%ax
	push   %ax
	mov    $D_00c0,%ax
	push   %ax
	call   L_08f5
	add    $0x4,%sp
	jmp   L_068e
L_07d8:
	dec    %ax
	cmp    $0x12,%ax
	ja   L_07c8
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x203e(%bx)
L_07e5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_07ec:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_07f8:
	movw   $0x1,D_0400
	cmpw   $0x0,D_03fc
	jne   L_0840
	cmpw   $0x0,D_03fe
	je   L_0818
	addw   $0x1,D_02da
	adcw   $0x0,D_02dc
	jmp   L_0840
L_0818:
	cmpw   $0x0,D_02d8
	je   L_0842
	pushw  0x4(%bp)
	mov    $D_00cf,%ax
	push   %ax
	call   L_0c68
	add    $0x4,%sp
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    $D_022c,%ax
	push   %ax
	call   L_0ab4
	add    $0x8,%sp
L_0840:
	jmp   L_08b4
L_0842:
	cmpw   $0x1,D_03f8
	jle   L_085d
	cmpw   $0x0,D_0004
	je   L_085d
	pushw  0x4(%bp)
	mov    $D_00d3,%ax
	push   %ax
	call   L_0c68
	add    $0x4,%sp
L_085d:
	cmpw   $0x0,D_03fa
	je   L_088d
	mov    $0x200,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    $D_022c,%ax
	push   %ax
	call   L_09be
	pop    %cx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	push   %dx
	push   %ax
	call   L_13fc
	add    $0x8,%sp
	push   %dx
	push   %ax
	mov    $D_00d7,%ax
	push   %ax
	call   L_0c68
	add    $0x6,%sp
L_088d:
	cmpw   $0x0,D_02de
	je   L_08a6
	pushw  D_041c
	pushw  D_041a
	mov    $D_00dc,%ax
	push   %ax
	call   L_0c68
	add    $0x6,%sp
L_08a6:
	mov    $D_041e,%ax
	push   %ax
	mov    $D_00e1,%ax
	push   %ax
	call   L_0c68
	add    $0x4,%sp
L_08b4:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_08b8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_08c4:
	mov    0x8(%bp),%di
L_08c7:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_08ee
	mov    0x4(%bp),%bx
	incw   0x4(%bp)
	mov    (%bx),%al
	cbtw
	mov    0x6(%bp),%bx
	incw   0x6(%bp)
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_08c7
	xor    %ax,%ax
	jmp   L_08f1
L_08ee:
	mov    $0x1,%ax
L_08f1:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_08f5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0901:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	mov    $D_023c,%ax
	push   %ax
	call   L_0a72
	add    $0x6,%sp
	mov    $0x2,%ax
	push   %ax
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_091e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	push   %si
	call   L_1b85
	pop    %cx
	cmpb   $0x77,(%di)
	jne   L_0948
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0a9e
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	jmp   L_0984
L_0948:
	cmpb   $0x61,(%di)
	jne   L_0986
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1e8e
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jge   L_0971
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0a9e
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_0971:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	call   L_0d40
	add    $0x8,%sp
L_0984:
	jmp   L_0995
L_0986:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1e8e
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_0995:
	cmpw   $0x0,-0x6(%bp)
	jge   L_099f
	xor    %ax,%ax
	jmp   L_09b8
L_099f:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,0x7(%si)
	cmpb   $0x72,(%di)
	je   L_09b2
	orb    $0x2,0x6(%si)
	jmp   L_09b6
L_09b2:
	orb    $0x1,0x6(%si)
L_09b6:
	mov    %si,%ax
L_09b8:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_09be:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x2(%bx)
	jge   L_09d7
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
L_09d7:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_09ef
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%ax
	neg    %ax
	mov    %ax,%di
	jmp   L_0a28
L_09ef:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
	je   L_0a21
	xor    %di,%di
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	je   L_0a28
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	jne   L_0a28
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	mov    0x4(%bp),%bx
	sub    0x4(%bx),%ax
	mov    %ax,%di
	jmp   L_0a28
L_0a21:
	mov    $0xffff,%ax
	mov    %ax,%dx
	jmp   L_0a6a
L_0a28:
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
	call   L_0d40
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmp    $0x0,%dx
	jg   L_0a5b
	jl   L_0a53
	cmpw   $0x0,-0x8(%bp)
	jae   L_0a5b
L_0a53:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	jmp   L_0a6a
L_0a5b:
	mov    %di,%ax
	cwtd
	add    %ax,-0x8(%bp)
	adc    %dx,-0x6(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
L_0a6a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0a72:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_0d5e
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_0a98
	mov    $0xffff,%ax
	jmp   L_0a9a
L_0a98:
	xor    %ax,%ax
L_0a9a:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a9e:
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
	jns    L_0ab2
	neg	%ax
	mov	%ax,D_0726	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_0ab2:
	pop    %bp
	ret


L_0ab4:
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
	jne   L_0ad2
	jmp   L_0c1b
L_0ad2:
	cmpw   $0x2,0xa(%bp)
	jl   L_0adb
	jmp   L_0bbc
L_0adb:
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_0ae7
	jmp   L_0bbc
L_0ae7:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0af6
	jmp   L_0bbc
L_0af6:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%si
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,0xa(%bp)
	jne   L_0b4f
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
	call   L_0d40
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
	jmp   L_0b6a
L_0b4f:
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
L_0b6a:
	or     %si,%si
	jle   L_0bab
	mov    %si,%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jg   L_0bab
	jl   L_0b7d
	cmp    %ax,-0x8(%bp)
	ja   L_0bab
L_0b7d:
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x4(%bp),%bx
	sub    (%bx),%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jl   L_0bab
	jg   L_0b95
	cmp    %ax,-0x8(%bp)
	jb   L_0bab
L_0b95:
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	add    %ax,(%bx)
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	sub    %ax,0x2(%bx)
	xor    %ax,%ax
	jmp   L_0c5c
L_0bab:
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x1,%ax
	and    $0x0,%dx
	mov    %ax,%di
	jmp   L_0bbe
L_0bbc:
	xor    %di,%di
L_0bbe:
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
	call   L_0d40
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
	or     %di,%di
	je   L_0c25
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_0c12
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0c4b
L_0c12:
	pushw  0x4(%bp)
	call   L_0c92
	pop    %cx
	jmp   L_0c4b
L_0c1b:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
L_0c25:
	je   L_0c4b
	pushw  0x4(%bp)
	call   L_1b16
	pop    %cx
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_0d40
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_0c4b:
	cmpw   $0xffff,-0x6(%bp)
	jne   L_0c5e
	cmpw   $0xffff,-0x8(%bp)
	jne   L_0c5e
	mov    $0xffff,%ax
L_0c5c:
	jmp   L_0c60
L_0c5e:
	xor    %ax,%ax
L_0c60:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0c68:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0234,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0d5e
	add    $0x6,%sp
	mov    D_023a,%al
	cbtw
	test   $0x20,%ax
	je   L_0c8c
	mov    $0xffff,%ax
	jmp   L_0c8e
L_0c8c:
	xor    %ax,%ax
L_0c8e:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0c92:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_0ca9
	mov    $0xffff,%ax
	jmp   L_0cb5
L_0ca9:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_0cb7
	mov    $0xffff,%ax
L_0cb5:
	jmp   L_0d2f
L_0cb7:
	cmpw   $0x0,0x4(%di)
	jne   L_0ceb
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0cd2
	mov    0x7(%di),%al
	cbtw
	add    $D_0168,%ax
	mov    %ax,0x4(%di)
	jmp   L_0cb7
L_0cd2:
	mov    $0x200,%ax
	push   %ax
	call   L_1bfc
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_0ce7
	orb    $0x4,0x6(%di)
	jmp   L_0cb7
L_0ce7:
	orb    $0x8,0x6(%di)
L_0ceb:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0cfe
	mov    $0x1,%ax
	jmp   L_0d01
L_0cfe:
	mov    $0x200,%ax
L_0d01:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1ea4
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_0d31
	cmpw   $0xffff,0x2(%di)
	jne   L_0d23
	orb    $0x10,0x6(%di)
	jmp   L_0d27
L_0d23:
	orb    $0x20,0x6(%di)
L_0d27:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_0d2f:
	jmp   L_0d3b
L_0d31:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_0d3b:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0d40:
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
L_0d5b:
	pop    %si
	pop    %bp
	ret


L_0d5e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0d76:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_0d88
	jmp   L_1325
L_0d88:
	cmpb   $0x25,-0x5(%bp)
	je   L_0d9b
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_0d99:
	jmp   L_0d76
L_0d9b:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0dc6
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1936
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0dc6:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_0ddf
	incb   -0xa6(%bp)
	inc    %di
L_0ddf:
	cmpb   $0x30,(%di)
	jne   L_0dea
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_0dea:
	cmpb   $0x2a,(%di)
	je   L_0e1a
	movw   $0x0,-0xe(%bp)
L_0df4:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0e30
	cmpw   $0x9,-0xc(%bp)
	jg   L_0e30
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_0df4
L_0e1a:
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
L_0e30:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_0e77
	cmpb   $0x2a,(%di)
	je   L_0e79
	movw   $0x0,-0x10(%bp)
L_0e49:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0e73
	cmpw   $0x9,-0xc(%bp)
	jg   L_0e73
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_0e49
L_0e73:
	addw   $0x30,-0xc(%bp)
L_0e77:
	jmp   L_0e90
L_0e79:
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
L_0e90:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_129a
L_0e9b:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_0eac
L_0ea3:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_0edc
	jmp   L_0ed5
L_0eac:
	cmp    $0x64,%ax
	je   L_0ede
	cmp    $0x6f,%ax
	je   L_0ec2
	cmp    $0x75,%ax
	je   L_0ed5
	cmp    $0x78,%ax
	je   L_0ed0
	jmp   L_0ea3
L_0ec2:
	movw   $0x8,-0x12(%bp)
	jmp   L_0ed5
L_0ec9:
	movw   $0x8,-0x12(%bp)
	jmp   L_0edc
L_0ed0:
	movw   $0x10,-0x12(%bp)
L_0ed5:
	jmp   L_0f27
L_0ed7:
	movw   $0x10,-0x12(%bp)
L_0edc:
	jmp   L_0f37
L_0ede:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0efb
L_0eee:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0efb:
	cmpw   $0x0,-0x14(%bp)
	jg   L_0f45
	jl   L_0f09
	cmpw   $0x0,-0x16(%bp)
	jae   L_0f45
L_0f09:
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
	jmp   L_0f45
L_0f27:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0f45
L_0f37:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0f45:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0f4f
	cmpw   $0x0,-0x16(%bp)
L_0f4f:
	jne   L_0f60
	cmpw   $0x0,-0x10(%bp)
	jne   L_0f60
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_0f60:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0f67:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0f71
	cmpw   $0x0,-0x16(%bp)
L_0f71:
	je   L_0fbc
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_1484
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_0f92
	mov    $0x30,%ax
	jmp   L_0f95
L_0f92:
	mov    $0x57,%ax
L_0f95:
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
	call   L_137c
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_0f67
L_0fbc:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_100b
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_0fbc
L_0fda:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_0fe6:
	cmpw   $0x2,-0xc(%bp)
	jge   L_100b
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_1006
	decw   -0x8(%bp)
L_1006:
	incw   -0xc(%bp)
	jmp   L_0fe6
L_100b:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_1012:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_1029
	movw   $0x0,-0xc(%bp)
L_1029:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_1038
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_1038:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_1936
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_105b:
	jmp   L_0d99
L_105e:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_1070
	movw   $D_017c,-0xa(%bp)
L_1070:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_107f
	movw   $0x7fff,-0xc(%bp)
L_107f:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_1085:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1012
	decw   -0xc(%bp)
	jl   L_1012
	incw   -0x8(%bp)
	jmp   L_1085
L_1097:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_109f:
	jmp   L_105b
L_10a1:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_10ad
	movw   $0x6,-0x10(%bp)
L_10ad:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1682
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_10e2
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_10e2:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_10f7
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_110f
L_10f7:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_10f7
L_110f:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_1122
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_1122:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_1141
L_112e:
	decw   -0xc(%bp)
	jl   L_1141
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_112e
L_1141:
	cmpw   $0x0,-0xc(%bp)
	jg   L_114a
	jmp   L_100b
L_114a:
	decw   -0xc(%bp)
	jge   L_1152
	jmp   L_100b
L_1152:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_114a
L_1167:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1175
	movw   $0x6,-0x10(%bp)
	jmp   L_1178
L_1175:
	incw   -0x10(%bp)
L_1178:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1656
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_11ad
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_11ad:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_11b8
	incw   -0x1a(%bp)
L_11b8:
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
L_11da:
	decw   -0xc(%bp)
	jle   L_11f4
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_11da
L_11f4:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_1211
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_1222
L_1211:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_1222:
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
	jmp   L_100b
L_1253:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_125f
	movw   $0x6,-0x10(%bp)
L_125f:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_14da
	add    $0xc,%sp
	add    $0x8,%si
L_127d:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_127d
	decw   -0x8(%bp)
	jmp   L_100b
L_128e:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_1297:
	jmp   L_109f
L_129a:
	cmp    $0x65,%ax
	jne   L_12a2
	jmp   L_1167
L_12a2:
	jg   L_12e3
	cmp    $0x4f,%ax
	jne   L_12ac
	jmp   L_0ec2
L_12ac:
	jg   L_12c3
	cmp    $0x25,%ax
	je   L_128e
	cmp    $0x44,%ax
	jne   L_12bb
	jmp   L_0ede
L_12bb:
	cmp    $0x4c,%ax
L_12be:
	jne   L_12c3
	jmp   L_0e9b
L_12c3:
	cmp    $0x55,%ax
	jne   L_12cb
	jmp   L_0f27
L_12cb:
	cmp    $0x58,%ax
	jne   L_12d3
	jmp   L_0ed0
L_12d3:
	cmp    $0x63,%ax
	jne   L_12db
	jmp   L_0fda
L_12db:
	cmp    $0x64,%ax
	jne   L_12e3
	jmp   L_0eee
L_12e3:
	cmp    $0x6f,%ax
	jne   L_12eb
	jmp   L_0ec9
L_12eb:
	jg   L_1302
	cmp    $0x66,%ax
	jne   L_12f5
	jmp   L_10a1
L_12f5:
	cmp    $0x67,%ax
	jne   L_12fd
	jmp   L_1253
L_12fd:
	cmp    $0x6c,%ax
	je   L_12be
L_1302:
	cmp    $0x72,%ax
	jne   L_130a
	jmp   L_1097
L_130a:
	cmp    $0x73,%ax
	jne   L_1312
	jmp   L_105e
L_1312:
	cmp    $0x75,%ax
	jne   L_131a
	jmp   L_0f37
L_131a:
	cmp    $0x78,%ax
	jne   L_1322
	jmp   L_0ed7
L_1322:
	jmp   L_1297
L_1325:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_1349
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1936
	add    $0x8,%sp
L_1349:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_14b8


L_1353:
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
	call   L_14b8


L_137c:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_139a
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_139a:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_13a3:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_13a3
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_13b3:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_13b3
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1353
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_13f4
	jne   L_13e9
	or     $0x0,%ax
	je   L_13f4
L_13e9:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_13f4:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_14b8


L_13fc:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_1418
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_1418:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_142f
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_142f:
	push   %dx
	push   %ax
	call   L_137c
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_1449
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_1449:
	ret
	call   L_14b8
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_13fc
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1353
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_14b8


L_1484:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_137c
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1353
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_14b8:
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


L_14da:
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
	call   L_1656
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_1514
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_1514:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_151b:
	cmpw   $0x0,-0xa(%bp)
	jle   L_1535
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_1535
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_151b
L_1535:
	cmpw   $0x0,-0x8(%bp)
	jl   L_1546
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_1552
L_1546:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1550
	cmpw   $0xfffd,-0x8(%bp)
L_1550:
	jge   L_15d1
L_1552:
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
L_156d:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_1587
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_156d
L_1587:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_15a3
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_15a9
L_15a3:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_15a9:
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
	jmp   L_1641
L_15d1:
	cmpw   $0x0,-0x8(%bp)
	jg   L_15f3
	cmpb   $0x30,(%di)
	je   L_15e2
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_15e2:
	cmpw   $0x0,-0x8(%bp)
	jge   L_15f3
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_15e2
L_15f3:
	movw   $0x1,-0xa(%bp)
L_15f8:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_1620
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
	jne   L_161b
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_161b:
	incw   -0xa(%bp)
	jmp   L_15f8
L_1620:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_1641
L_1628:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_163b
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_1628
L_163b:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1641:
	cmpb   $0x2e,-0x1(%si)
	jne   L_1648
	dec    %si
L_1648:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1656:
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
	call   L_16ad
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1682:
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
	call   L_16ad
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_16ad:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_16c0
	movw   $0x0,0xc(%bp)
L_16c0:
	cmpw   $0x4f,0xc(%bp)
	jl   L_16cb
	movw   $0x4e,0xc(%bp)
L_16cb:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_0184,%si
	fldl   0x4(%bp)
	fcompl D_01d4
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_1708
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_1708:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1f24
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $D_01d4,-0x16(%bp)
	fldl   -0xc(%bp)
	fcompl D_01dc
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_1753
	jmp   L_1800
L_1753:
	movw   $D_01d4,-0x16(%bp)
L_1758:
	fldl   -0xc(%bp)
	fcompl D_01e4
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_1774
	jmp   L_17fe
L_1774:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	fdivl  D_01ec
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1f24
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   D_01f4
	fstl   -0x1e(%bp)
	fdivl  D_01fc
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  D_0204
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
	jmp   L_1758
L_17e7:
	cmpw   $D_01d4,-0x16(%bp)
	jae   L_1841
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_17fe:
	jmp   L_17e7
L_1800:
	fldl   0x4(%bp)
	fcompl D_020c
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_1850
L_1819:
	fldl   0x4(%bp)
	fmull  D_0214
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl D_021c
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_1841:
	jae   L_1850
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_1819
L_1850:
	mov    0xc(%bp),%ax
	add    $D_0184,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_1862
	add    %di,-0x16(%bp)
L_1862:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_0184,-0x16(%bp)
	jae   L_1878
	movb   $0x0,D_0184
	mov    $D_0184,%ax
	jmp   L_18e3
L_1878:
	cmp    -0x16(%bp),%si
	ja   L_18d4
	cmp    $D_01d4,%si
	jae   L_18d4
	fldl   0x4(%bp)
	fmull  D_0224
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_1f24
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
	jmp   L_1878
L_18d4:
	cmpw   $D_01d4,-0x16(%bp)
	jb   L_18e5
	movb   $0x0,D_01d3
	mov    $D_0184,%ax
L_18e3:
	jmp   L_192f
L_18e5:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_18ed:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_1929
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_0184,-0x16(%bp)
	jbe   L_190c
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_18ed
L_190c:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_18ed
	cmp    $D_0184,%si
	jbe   L_1926
	movb   $0x30,(%si)
L_1926:
	inc    %si
	jmp   L_18ed
L_1929:
	movb   $0x0,(%si)
	mov    $D_0184,%ax
L_192f:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1936:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1943:
	cmpw   $0x0,0x8(%bp)
	jl   L_194c
	jmp   L_19da
L_194c:
	cmpb   $0x2d,(%di)
	jne   L_1989
	cmpw   $0x30,0xc(%bp)
	jne   L_1989
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1978
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
	jmp   L_1988
L_1978:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1a28
	add    $0x4,%sp
L_1988:
	dec    %si
L_1989:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_19a7
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
	jmp   L_19b3
L_19a7:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_1a28
	add    $0x4,%sp
L_19b3:
	incw   0x8(%bp)
	jmp   L_1943
L_19b8:
	dec    %si
	jl   L_19ee
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_19dc
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
L_19da:
	jmp   L_19b8
L_19dc:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1a28
	add    $0x4,%sp
	jmp   L_19b8
L_19ee:
	cmpw   $0x0,0x8(%bp)
	je   L_1a23
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1a12
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
	jmp   L_1a1e
L_1a12:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_1a28
	add    $0x4,%sp
L_1a1e:
	decw   0x8(%bp)
	jmp   L_19ee
L_1a23:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1a28:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_1a43
	mov    $0xffff,%ax
	jmp   L_1b0a
L_1a43:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1a76
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
	call   L_1f0a
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_1afb
L_1a76:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1ac8
	cmp    $D_0234,%di
	jne   L_1aa1
	mov    D_023b,%al
	cbtw
	push   %ax
	call   L_1e66
	pop    %cx
	or     %ax,%ax
	je   L_1a96
	orb    $0x4,0x6(%di)
	jmp   L_1a43
L_1a96:
	movw   $D_072e,0x4(%di)
	movw   $D_072e,(%di)
	jmp   L_1a43
L_1aa1:
	mov    $0x200,%ax
	push   %ax
	call   L_1bfc
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1ab8
	orb    $0x4,0x6(%di)
	jmp   L_1a43
L_1ab8:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_1aea
L_1ac8:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_1aea
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1f0a
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_1aea:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_1afb:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_1b0c
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_1b0a:
	jmp   L_1b0f
L_1b0c:
	mov    0x4(%bp),%ax
L_1b0f:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b16:
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
	jne   L_1b62
	mov    0x4(%di),%si
	or     %si,%si
	je   L_1b62
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_1b62
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1f0a
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_1b62
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_1b64
L_1b62:
	xor    %ax,%ax
L_1b64:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b69:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_022c,%di
L_1b71:
	cmp    D_02cc,%di
	jae   L_1b81
	push   %di
	call   L_1b85
	pop    %cx
	add    $0x8,%di
	jmp   L_1b71
L_1b81:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b85:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_1bd8
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_1bd8
	push   %di
	call   L_1b16
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1e52
	pop    %cx
	or     %ax,%ax
	jge   L_1bba
	mov    $0xffff,%si
L_1bba:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_1bca
	pushw  0x4(%di)
	call   L_1d48
	pop    %cx
L_1bca:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_1bd8
	movw   $0x0,0x4(%di)
L_1bd8:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_1bfc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_02ce
	jne   L_1c27
	mov    $D_02d0,%ax
	or     $0x1,%ax
	mov    %ax,D_02ce
	mov    $D_02ce,%ax
	or     $0x1,%ax
	mov    %ax,D_02d0
	/* dual-reloc split: movw $D_02d0,D_0b30 */
	mov	$D_02d0,%ax
	mov	%ax,D_0b30
	/* dual-reloc split: movw $D_02ce,D_0b2e */
	mov	$D_02ce,%ax
	mov	%ax,D_0b2e
L_1c27:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_0b2e,%di
L_1c3b:
	movw   $0x0,D_02d2
L_1c41:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_1c70
L_1c48:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_1c57
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_1c48
L_1c57:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_1c70
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_1c70
	jmp   L_1d19
L_1c70:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	jbe   L_1c7f
L_1c7d:
	jmp   L_1c41
L_1c7f:
	cmp    D_0b30,%si
	jne   L_1c8b
	cmp    $D_02ce,%di
	je   L_1c8f
L_1c8b:
	xor    %ax,%ax
	jmp   L_1cc7
L_1c8f:
	incw   D_02d2
	cmpw   $0x1,D_02d2
	jle   L_1c7d
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_02d2
	xor    %ax,%ax
	push   %ax
	call   L_1e2c
	pop    %cx
	mov    %ax,%si
	mov    D_02d2,%ax
	shl    %ax
	add    %si,%ax
	add    $0x0,%ax
	cmp    %si,%ax
	jae   L_1cc9
	xor    %ax,%ax
L_1cc7:
	jmp   L_1cdc
L_1cc9:
	mov    D_02d2,%ax
	shl    %ax
	push   %ax
	call   L_1e2c
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_1cde
	xor    %ax,%ax
L_1cdc:
	jmp   L_1d43
L_1cde:
	mov    D_0b30,%bx
	mov    %si,(%bx)
	mov    D_0b30,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_1cfc
	mov    D_0b30,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_0b30,%bx
	mov    %ax,(%bx)
L_1cfc:
	mov    D_02d2,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_0b30
	mov    $D_02ce,%ax
	or     $0x1,%ax
	mov    D_0b30,%bx
	mov    %ax,(%bx)
	jmp   L_1c3b
L_1d19:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_0b2e
	cmp    %ax,%si
	jbe   L_1d38
	mov    D_0b2e,%bx
	mov    (%bx),%ax
	mov    %ax,D_0b32
	mov    D_0b2e,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_1d38:
	mov    D_0b2e,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_1d43:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d48:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_0b2e
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
	je   L_1d7d
	push   %di
	call   L_1d48
	pop    %cx
L_1d7d:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_1bfc
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_1d9c
	cmp    %di,%si
	jne   L_1da0
L_1d9c:
	mov    %si,%ax
	jmp   L_1e08
L_1da0:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_1dbf
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_1dbf:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_1ddd
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_1dbf
L_1ddd:
	cmp    %di,%si
	jae   L_1e06
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_1e06
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
	mov    D_0b32,%ax
	mov    %ax,(%bx)
L_1e06:
	mov    %si,%ax
L_1e08:
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
	jns    L_1e23
	neg	%ax
	mov	%ax,D_0726	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1e2a
L_1e23:
	mov    0x4(%bp),%bx
	mov    %bx,D_02d4
L_1e2a:
	pop    %bp
	ret


L_1e2c:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_02d4,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1e43
	neg	%ax
	mov	%ax,D_0726	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1e50
L_1e43:
	mov    D_02d4,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_02d4
L_1e50:
	pop    %bp
	ret


L_1e52:
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
	jns    L_1e63
	neg	%ax
	mov	%ax,D_0726	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1e63:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1e66:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1ed7
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_1e83
	xor    %ax,%ax
	jmp   L_1e86
L_1e83:
	mov    $0x1,%ax
L_1e86:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1e8e:
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
	jns    L_1ea2
	neg	%ax
	mov	%ax,D_0726	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1ea2:
	pop    %bp
	ret


L_1ea4:
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
	jns    L_1ebb
	neg	%ax
	mov	%ax,D_0726	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1ebb:
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
	call   L_1ef0
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ed7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1ef0
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ef0:
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
	jns    L_1f07
	neg	%ax
	mov	%ax,D_0726	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1f07:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1f0a:
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
	jns    L_1f21
	neg	%ax
	mov	%ax,D_0726	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1f21:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1f24:
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


L_1f40:
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
	jns    L_1f51
	neg	%ax
	mov	%ax,D_0726	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1f51:
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
	mov	%ax,D_0726	/* errno */
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
	.byte	0x01, 0x02	/* |..| */
	.word	D_0804	/* ptr: 0x0804 */
	.byte	0x10, 0x20, 0x40, 0x80, 0x7a, 0x00, 0x86, 0x00	/* |. @.z...| */
	.byte	0x9b, 0x00, 0x92, 0x00, 0x9b, 0x00, 0x9b, 0x00	/* |........| */
	.byte	0x66, 0x00, 0x9b, 0x00, 0x9b, 0x00, 0x9b, 0x00	/* |f.......| */
	.byte	0x80, 0x00, 0x9b, 0x00, 0x8c, 0x00, 0x9b, 0x00	/* |........| */
	.byte	0x9b, 0x00, 0x9b, 0x00, 0x9b, 0x00, 0x6e, 0x00	/* |......n.| */
	.byte	0x9b, 0x00, 0x9b, 0x00, 0x74, 0x00, 0x9b, 0x00	/* |....t...| */
	.byte	0x9b, 0x00, 0x60, 0x00	/* |..`.| */
	.word	D_0626	/* ptr: 0x0626 */
	.word	D_05ad	/* ptr: 0x05ad */
	.word	D_070e	/* ptr: 0x070e */
	.word	D_05c7	/* ptr: 0x05c7 */
	.word	D_0701	/* ptr: 0x0701 */
	.word	D_05e1	/* ptr: 0x05e1 */
	.word	D_0727	/* ptr: 0x0727 */
	.word	D_07c8	/* ptr: 0x07c8 */
	.word	D_07c8	/* ptr: 0x07c8 */
	.word	D_05d3	/* ptr: 0x05d3 */
	.word	D_05dc	/* ptr: 0x05dc */
	.word	D_0636	/* ptr: 0x0636 */
	.word	D_07c8	/* ptr: 0x07c8 */
	.word	D_07c8	/* ptr: 0x07c8 */
	.word	D_07c8	/* ptr: 0x07c8 */
	.word	D_07c8	/* ptr: 0x07c8 */
	.word	D_07c8	/* ptr: 0x07c8 */
	.word	D_0646	/* ptr: 0x0646 */
	.word	D_0694	/* ptr: 0x0694 */
D_0064:
	.byte	0x67, 0x72, 0x65, 0x70, 0x3a, 0x20, 0x75, 0x6e	/* |grep: un| */
	.byte	0x6b, 0x6e, 0x6f, 0x77, 0x6e, 0x20, 0x66, 0x6c	/* |known fl| */
	.byte	0x61, 0x67, 0x0a, 0x00	/* |ag..| */
D_0078:
	.byte	0x67, 0x72, 0x65, 0x70, 0x3a, 0x20, 0x61, 0x72	/* |grep: ar| */
	.byte	0x67, 0x75, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x74	/* |gument t| */
	.byte	0x6f, 0x6f, 0x20, 0x6c, 0x6f, 0x6e, 0x67, 0x0a	/* |oo long.| */
	.byte	0x00	/* |.| */
D_0091:
	.byte	0x67, 0x72, 0x65, 0x70, 0x3a, 0x20, 0x52, 0x45	/* |grep: RE| */
	.byte	0x20, 0x65, 0x72, 0x72, 0x6f, 0x72, 0x0a, 0x00	/* | error..| */
D_00a1:
	.byte	0x72, 0x00	/* |r.| */
D_00a3:
	.byte	0x67, 0x72, 0x65, 0x70, 0x3a, 0x20, 0x63, 0x61	/* |grep: ca| */
	.byte	0x6e, 0x27, 0x74, 0x20, 0x6f, 0x70, 0x65, 0x6e	/* |n't open| */
	.byte	0x20, 0x25, 0x73, 0x0a, 0x00	/* | %s..| */
D_00b8:
	.byte	0x25, 0x73, 0x3a, 0x00	/* |%s:.| */
D_00bc:
	.byte	0x25, 0x44, 0x0a, 0x00	/* |%D..| */
D_00c0:
	.byte	0x67, 0x72, 0x65, 0x70, 0x20, 0x52, 0x45, 0x20	/* |grep RE | */
	.byte	0x62, 0x6f, 0x74, 0x63, 0x68, 0x0a, 0x00	/* |botch..| */
D_00cf:
	.byte	0x25	/* |%| */
	.word	D_0a73	/* ptr: 0x0a73 */
	.byte	0x00	/* |.| */
D_00d3:
	.byte	0x25, 0x73, 0x3a, 0x00	/* |%s:.| */
D_00d7:
	.ascii	"%ld:\0"
D_00dc:
	.ascii	"%ld:\0"
D_00e1:
	.byte	0x25	/* |%| */
	.word	D_0a73	/* ptr: 0x0a73 */
	.byte	0x00, 0x00, 0x00	/* |...| */
D_00e7:
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
	.byte	0x02, 0x02, 0x10, 0x10, 0x10, 0x10, 0x20, 0x00	/* |...... .| */
D_0168:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_017c:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_0184:
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
D_01d3:
	.byte	0x00	/* |.| */
D_01d4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_01dc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_01e4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_01ec:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_01f4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_01fc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_0204:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_020c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0214:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_021c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_0224:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_022c:
	.byte	0x2e, 0x29	/* |.)| */
D_022e:
	.byte	0x00, 0x00	/* |..| */
	.word	D_092e	/* ptr: 0x292e */
	.byte	0x01, 0x00	/* |..| */
D_0234:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_023a:
	.byte	0x02	/* |.| */
D_023b:
	.byte	0x01	/* |.| */
D_023c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_02cc:
	.byte	0xcc, 0x22	/* |."| */
D_02ce:
	.byte	0x00, 0x00	/* |..| */
D_02d0:
	.byte	0x00, 0x00	/* |..| */
D_02d2:
	.byte	0x00, 0x00	/* |..| */
D_02d4:
	.word	D_0b34	/* ptr: 0x2b34 */

	.bss

	.space	2
D_02d8:	.space	2	/* bss word @ 0x02d8 */
D_02da:	.space	2	/* bss word @ 0x02da */
D_02dc:	.space	2	/* bss word @ 0x02dc */
D_02de:	.space	2	/* bss word @ 0x02de */
	.space	6
D_02e6:	.space	2	/* bss word @ 0x02e6 */
	.space	254
D_03e6:	.space	2	/* bss word @ 0x03e6 */
	.space	16
D_03f8:	.space	2	/* bss word @ 0x03f8 */
D_03fa:	.space	2	/* bss word @ 0x03fa */
D_03fc:	.space	2	/* bss word @ 0x03fc */
D_03fe:	.space	2	/* bss word @ 0x03fe */
D_0400:	.space	2	/* bss word @ 0x0400 */
D_0402:	.space	2	/* bss word @ 0x0402 */
D_0404:	.space	2	/* bss word @ 0x0404 */
D_0406:	.space	2	/* bss word @ 0x0406 */
	.space	8
D_0410:	.space	2	/* bss word @ 0x0410 */
	.space	6
D_0418:	.space	2	/* bss word @ 0x0418 */
D_041a:	.space	2	/* bss word @ 0x041a */
D_041c:	.space	2	/* bss word @ 0x041c */
D_041e:	.space	2	/* bss word @ 0x041e */
	.space	397
D_05ad:	.space	2	/* bss word @ 0x05ad */
	.space	24
D_05c7:	.space	2	/* bss word @ 0x05c7 */
	.space	10
D_05d3:	.space	2	/* bss word @ 0x05d3 */
	.space	7
D_05dc:	.space	2	/* bss word @ 0x05dc */
	.space	3
D_05e1:	.space	2	/* bss word @ 0x05e1 */
	.space	58
D_061d:	.space	2	/* bss word @ 0x061d */
	.space	7
D_0626:	.space	2	/* bss word @ 0x0626 */
	.space	14
D_0636:	.space	2	/* bss word @ 0x0636 */
	.space	14
D_0646:	.space	2	/* bss word @ 0x0646 */
	.space	76
D_0694:	.space	2	/* bss word @ 0x0694 */
	.space	107
D_0701:	.space	2	/* bss word @ 0x0701 */
	.space	11
D_070e:	.space	2	/* bss word @ 0x070e */
	.space	17
D_0721:	.space	2	/* bss word @ 0x0721 */
	.space	3
D_0726:	.space	2	/* bss word @ 0x0726 */
D_0727:	.space	2	/* bss word @ 0x0727 */
	.space	5
D_072e:	.space	2	/* bss word @ 0x072e */
	.space	152
D_07c8:	.space	2	/* bss word @ 0x07c8 */
	.space	58
D_0804:	.space	2	/* bss word @ 0x0804 */
	.space	2
D_0808:	.space	2	/* bss word @ 0x0808 */
	.space	22
D_0820:	.space	2	/* bss word @ 0x0820 */
	.space	268
D_092e:	.space	2	/* bss word @ 0x092e */
	.space	323
D_0a73:	.space	2	/* bss word @ 0x0a73 */
	.space	185
D_0b2e:	.space	2	/* bss word @ 0x0b2e */
D_0b30:	.space	2	/* bss word @ 0x0b30 */
D_0b32:	.space	2	/* bss word @ 0x0b32 */
D_0b34:	/* endcore: initial break */

/* end of file */
