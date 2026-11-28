/* ranlib.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 ranlib.s -o ranlib.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: ranlib  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00002294  (8852 bytes)
//   a_data:   0x0000032a  (810 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x218,%sp
L_0046:
	decw   0x4(%bp)
L_0049:
	mov    0x4(%bp),%ax
	decw   0x4(%bp)
	or     %ax,%ax
	jne   L_0056
	jmp   L_0478
L_0056:
	/* TODO: data ref: mov    $0x2028,%ax */
	mov    $D_0028,%ax
	push   %ax
	addw   $0x2,0x6(%bp)
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_0908
	add    $0x4,%sp
	/* TODO: data ref: mov    %ax,0x4aa4 */
	mov    %ax,D_2aa4
	or     %ax,%ax
	jne   L_0085
	mov    0x6(%bp),%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x202a,%ax */
	mov    $D_002a,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x6,%sp
L_0083:
	jmp   L_0049
L_0085:
	movw   $0x8,D_4a12
	movw   $0x0,D_4a14
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	mov    $0x8,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	lea    -0x20d(%bp),%ax
	push   %ax
	call   L_0bd0
	add    $0x8,%sp
	mov    $0x8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2042,%ax */
	mov    $D_0042,%ax
	push   %ax
	lea    -0x20d(%bp),%ax
	push   %ax
	call   L_0d56
	add    $0x6,%sp
	or     %ax,%ax
	je   L_00fa
	cmpw   $0xff65,-0x20d(%bp)
	jne   L_00d7
	/* TODO: data ref: mov    $0x204b,%ax */
	mov    $D_004b,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x4,%sp
	jmp   L_00e5
L_00d7:
	/* TODO: data ref: mov    $0x2057,%ax */
	mov    $D_0057,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x4,%sp
L_00e5:
	mov    0x6(%bp),%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x205f,%ax */
	mov    $D_005f,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x6,%sp
L_00f8:
	jmp   L_0083
L_00fa:
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	call   L_0a1c
	add    $0x8,%sp
	/* TODO: data ref: movw   $0x0,0x2344 */
	movw   $0x0,D_0344
	/* TODO: data ref: movw   $0x0,0x4aa2 */
	movw   $0x0,D_2aa2
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	call   L_0486
	pop    %cx
	or     %ax,%ax
	jne   L_012f
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	call   L_1ea9
	pop    %cx
L_012d:
	jmp   L_00f8
L_012f:
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	mov    $0x20,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $D_49f2,%ax
	push   %ax
	call   L_0bd0
	add    $0x8,%sp
	cmpw   $0x107,D_49f2
	je   L_015a
	cmpw   $0x109,D_49f2
	je   L_015a
	mov    $0x1,%ax
	jmp   L_015c
L_015a:
	xor    %ax,%ax
L_015c:
	cwtd
	or     %dx,%dx
	jne   L_0163
	or     %ax,%ax
L_0163:
	jne   L_018a
	cmpw   $0x0,D_4a04
	jne   L_018d
	cmpw   $0x0,D_4a02
	jne   L_018d
	/* TODO: data ref: mov    $0x234a,%ax */
	mov    $D_034a,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x206c,%ax */
	mov    $D_006c,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x8,%sp
L_018a:
	jmp   L_0255
L_018d:
	mov    D_49f6,%ax
	mov    D_49f8,%dx
	add    $0x20,%ax
	adc    $0x0,%dx
	add    D_49fa,%ax
	adc    D_49fc,%dx
	add    D_4a0a,%ax
	adc    D_4a0c,%dx
	add    D_4a0e,%ax
	adc    D_4a10,%dx
	add    D_4a02,%ax
	adc    D_4a04,%dx
	sub    $0x20,%ax
	sbb    $0x0,%dx
	mov    %ax,-0x212(%bp)
	mov    %dx,-0x210(%bp)
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	call   L_0828
	pop    %cx
	add    -0x212(%bp),%ax
	adc    -0x210(%bp),%dx
	add    $0x2,%ax
	adc    $0x0,%dx
	cmp    D_4a14,%dx
	jl   L_020b
	jg   L_01ec
	cmp    D_4a12,%ax
	jb   L_020b
L_01ec:
	/* TODO: data ref: mov    $0x234a,%ax */
	mov    $D_034a,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x2096,%ax */
	mov    $D_0096,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x8,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_020b:
	mov    $0x1,%ax
	push   %ax
	pushw  -0x210(%bp)
	pushw  -0x212(%bp)
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	call   L_0a1c
	add    $0x8,%sp
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	mov    $0x2,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $D_49ea,%ax
	push   %ax
	call   L_0bd0
	add    $0x8,%sp
	cmpw   $0x0,D_49ea
	jne   L_0258
	/* TODO: data ref: mov    $0x234a,%ax */
	mov    $D_034a,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x20ba,%ax */
	mov    $D_00ba,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x8,%sp
L_0255:
	jmp   L_0346
L_0258:
	pushw  D_49ea
	mov    $0x1,%ax
	push   %ax
	call   L_07d2
	add    $0x4,%sp
	mov    %ax,D_49ec
	or     %ax,%ax
	jne   L_0283
	/* TODO: data ref: mov    $0x20e4,%ax */
	mov    $D_00e4,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0283:
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	mov    $0x1,%ax
	push   %ax
	mov    D_49ea,%ax
	dec    %ax
	dec    %ax
	push   %ax
	mov    D_49ec,%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_0bd0
	add    $0x8,%sp
	mov    $0x1,%ax
	push   %ax
	mov    D_49ea,%ax
	xor    %dx,%dx
	mov    D_4a02,%bx
	mov    D_4a04,%si
	add    %ax,%bx
	adc    %dx,%si
	neg    %si
	neg    %bx
	sbb    $0x0,%si
	push   %si
	push   %bx
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	call   L_0a1c
	add    $0x8,%sp
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  D_4a04
	pushw  D_4a02
	call   L_1720
	add    $0x8,%sp
	mov    %ax,%di
L_02dc:
	dec    %di
	jl   L_0346
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	mov    $0xa,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	lea    -0x21c(%bp),%ax
	push   %ax
	call   L_0bd0
	add    $0x8,%sp
	cmpw   $0x0,-0x21c(%bp)
	je   L_02dc
	mov    D_49ec,%ax
	add    -0x21c(%bp),%ax
	mov    %ax,-0x21c(%bp)
	mov    -0x21a(%bp),%al
	cbtw
	test   $0x1,%ax
	je   L_02dc
	mov    -0x21a(%bp),%al
	cbtw
	and    $0x1e,%ax
	jmp   L_0340
L_031c:
	cmpw   $0x0,-0x214(%bp)
	jne   L_032a
	cmpw   $0x0,-0x216(%bp)
	je   L_02dc
L_032a:
	lea    -0x21c(%bp),%ax
	push   %ax
	call   L_0534
	pop    %cx
L_0333:
	jmp   L_02dc
L_0335:
	lea    -0x21c(%bp),%ax
	push   %ax
	call   L_0534
	pop    %cx
	jmp   L_0333
L_0340:
	or     %ax,%ax
	je   L_031c
	jmp   L_0335
L_0346:
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	call   L_0486
	pop    %cx
	or     %ax,%ax
	je   L_0355
	jmp   L_012f
L_0355:
	call   L_05d3
	/* TODO: data ref: mov    %ax,0x4aa2 */
	mov    %ax,D_2aa2
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	call   L_1ea9
	pop    %cx
	/* TODO: data ref: mov    $0x20ff,%ax */
	mov    $D_00ff,%ax
	push   %ax
	/* TODO: data ref: mov    $0x201e,%ax */
	mov    $D_001e,%ax
	push   %ax
	call   L_0908
	add    $0x4,%sp
	/* TODO: data ref: mov    %ax,0x4aa6 */
	mov    %ax,D_2aa6
	or     %ax,%ax
	jne   L_038e
	/* TODO: data ref: mov    $0x2101,%ax */
	mov    $D_0101,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_038e:
	/* TODO: data ref: mov    0x2344,%ax */
	mov    D_0344,%ax
	mov    $0x8,%dx
	imul   %dx
	/* TODO: data ref: mov    %ax,0x2344 */
	mov    %ax,D_0344
	/* TODO: data ref: pushw  0x4aa6 */
	pushw  D_2aa6
	mov    $0x2,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2344,%ax */
	mov    $D_0344,%ax
	push   %ax
	call   L_0c3b
	add    $0x8,%sp
	/* TODO: data ref: mov    0x2344,%ax */
	mov    D_0344,%ax
	cwtd
	mov    $0x8,%cx
	idiv   %cx
	/* TODO: data ref: mov    %ax,0x2344 */
	mov    %ax,D_0344
	/* TODO: data ref: pushw  0x4aa6 */
	pushw  D_2aa6
	mov    $0x8,%ax
	push   %ax
	/* TODO: data ref: pushw  0x2344 */
	pushw  D_0344
	/* TODO: data ref: mov    $0x4aaa,%ax */
	mov    $D_2aaa,%ax
	push   %ax
	call   L_0c3b
	add    $0x8,%sp
	/* TODO: data ref: pushw  0x4aa6 */
	pushw  D_2aa6
	mov    $0x2,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x238a,%ax */
	mov    $D_038a,%ax
	push   %ax
	call   L_0c3b
	add    $0x8,%sp
	/* TODO: data ref: pushw  0x4aa6 */
	pushw  D_2aa6
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: pushw  0x238a */
	pushw  D_038a
	/* TODO: data ref: mov    $0x2392,%ax */
	mov    $D_0392,%ax
	push   %ax
	call   L_0c3b
	add    $0x8,%sp
	/* TODO: data ref: pushw  0x4aa6 */
	pushw  D_2aa6
	call   L_1ea9
	pop    %cx
	/* TODO: data ref: cmpw   $0x0,0x4aa2 */
	cmpw   $0x0,D_2aa2
	je   L_042a
	/* TODO: data ref: mov    $0x201e,%ax */
	mov    $D_001e,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x2332,%ax */
	mov    $D_0332,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2119,%ax */
	mov    $D_0119,%ax
	push   %ax
	lea    -0x204(%bp),%ax
	push   %ax
	call   L_1030
	add    $0xa,%sp
	jmp   L_0442
L_042a:
	/* TODO: data ref: mov    $0x201e,%ax */
	mov    $D_001e,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x212a,%ax */
	mov    $D_012a,%ax
	push   %ax
	lea    -0x204(%bp),%ax
	push   %ax
	call   L_1030
	add    $0x8,%sp
L_0442:
	lea    -0x204(%bp),%ax
	push   %ax
	call   L_0dfe
	pop    %cx
	or     %ax,%ax
	je   L_0464
	lea    -0x204(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x2137,%ax */
	mov    $D_0137,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x6,%sp
	jmp   L_046d
L_0464:
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_069c
	pop    %cx
L_046d:
	/* TODO: data ref: mov    $0x201e,%ax */
	mov    $D_001e,%ax
	push   %ax
	call   L_222e
	pop    %cx
	jmp   L_012d
L_0478:
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0486:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0492:
	mov    D_4a12,%ax
	mov    D_4a14,%dx
	/* TODO: data ref: mov    %ax,0x2386 */
	mov    %ax,D_0386
	/* TODO: data ref: mov    %dx,0x2388 */
	mov    %dx,D_0388
	xor    %ax,%ax
	push   %ax
	pushw  D_4a14
	pushw  D_4a12
	pushw  0x4(%bp)
	call   L_0a1c
	add    $0x8,%sp
	pushw  0x4(%bp)
	mov    $0x3c,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x234a,%ax */
	mov    $D_034a,%ax
	push   %ax
	call   L_0bd0
	add    $0x8,%sp
	mov    %ax,%di
	cmp    $0x3c,%di
	je   L_04d4
	xor    %ax,%ax
	jmp   L_0530
L_04d4:
	/* TODO: data ref: mov    $0x234a,%si */
	mov    $D_034a,%si
L_04d7:
	/* TODO: data ref: cmp    $0x235a,%si */
	cmp    $D_035a,%si
	jae   L_04e8
	cmpb   $0x20,(%si)
	jne   L_04e5
	movb   $0x0,(%si)
L_04e5:
	inc    %si
	jmp   L_04d7
L_04e8:
	/* TODO: data ref: mov    $0x237a,%ax */
	mov    $D_037a,%ax
	push   %ax
	call   L_072e
	pop    %cx
	/* TODO: data ref: mov    %ax,0x232e */
	mov    %ax,D_032e
	/* TODO: data ref: mov    %dx,0x2330 */
	mov    %dx,D_0330
	/* TODO: data ref: mov    0x232e,%ax */
	mov    D_032e,%ax
	/* TODO: data ref: mov    0x2330,%dx */
	mov    D_0330,%dx
	and    $0x1,%ax
	and    $0x0,%dx
	or     %dx,%dx
	jne   L_050b
	or     %ax,%ax
L_050b:
	je   L_0517
	/* TODO: data ref: addw   $0x1,0x232e */
	addw   $0x1,D_032e
	/* TODO: data ref: adcw   $0x0,0x2330 */
	adcw   $0x0,D_0330
L_0517:
	pushw  0x4(%bp)
	call   L_0828
	pop    %cx
	/* TODO: data ref: add    0x232e,%ax */
	add    D_032e,%ax
	/* TODO: data ref: adc    0x2330,%dx */
	adc    D_0330,%dx
	mov    %ax,D_4a12
	mov    %dx,D_4a14
	mov    $0x1,%ax
L_0530:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0534:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_0541:
	/* TODO: data ref: cmpw   $0x3e8,0x2344 */
	cmpw   $0x3e8,D_0344
	jl   L_055f
	/* TODO: data ref: mov    $0x214e,%ax */
	mov    $D_014e,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_055f:
	/* TODO: data ref: mov    0x238a,%ax */
	mov    D_038a,%ax
	cwtd
	/* TODO: data ref: mov    0x2344,%bx */
	mov    D_0344,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	/* TODO: data ref: mov    %ax,0x4aaa(%bx) */
	mov    %ax,D_2aaa(%bx)
	/* TODO: data ref: mov    %dx,0x4aac(%bx) */
	mov    %dx,D_2aac(%bx)
	/* TODO: data ref: mov    0x2344,%bx */
	mov    D_0344,%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	/* TODO: data ref: mov    0x2386,%ax */
	mov    D_0386,%ax
	/* TODO: data ref: mov    0x2388,%dx */
	mov    D_0388,%dx
	/* TODO: data ref: mov    %ax,0x4aae(%bx) */
	mov    %ax,D_2aae(%bx)
	/* TODO: data ref: mov    %dx,0x4ab0(%bx) */
	mov    %dx,D_2ab0(%bx)
	mov    0x4(%bp),%bx
	mov    (%bx),%di
L_0591:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	/* TODO: data ref: mov    0x238a,%bx */
	mov    D_038a,%bx
	/* TODO: data ref: incw   0x238a */
	incw   D_038a
	mov    %ax,%cx
	/* TODO: data ref: mov    %cl,0x2392(%bx) */
	mov    %cl,D_0392(%bx)
	cmp    $0x0,%cl
	je   L_05ca
	/* TODO: data ref: cmpw   $0x2710,0x238a */
	cmpw   $D_0710,D_038a
	jle   L_0591
	/* TODO: data ref: mov    $0x216d,%ax */
	mov    $D_016d,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
	jmp   L_0591
L_05ca:
	/* TODO: data ref: incw   0x2344 */
	incw   D_0344
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_05d3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
L_05e2:
	/* TODO: data ref: testw  $0x1,0x238a */
	testw  $0x1,D_038a
	je   L_05ee
	/* TODO: data ref: incw   0x238a */
	incw   D_038a
L_05ee:
	/* TODO: data ref: mov    0x2344,%ax */
	mov    D_0344,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	add    $0x40,%ax
	/* TODO: data ref: add    0x238a,%ax */
	add    D_038a,%ax
	cwtd
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
	movw   $0x8,D_4a12
	movw   $0x0,D_4a14
	/* TODO: data ref: pushw  0x4aa4 */
	pushw  D_2aa4
	call   L_0486
	pop    %cx
	mov    $0x10,%ax
	push   %ax
	/* TODO: data ref: mov    $0x201e,%ax */
	mov    $D_001e,%ax
	push   %ax
	/* TODO: data ref: mov    $0x234a,%ax */
	mov    $D_034a,%ax
	push   %ax
	call   L_0d56
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_0649
	/* TODO: data ref: movw   $0x0,0x4aa2 */
	movw   $0x0,D_2aa2
	/* TODO: data ref: mov    0x232e,%ax */
	mov    D_032e,%ax
	/* TODO: data ref: mov    0x2330,%dx */
	mov    D_0330,%dx
	add    $0x3c,%ax
	adc    $0x0,%dx
	sub    %ax,-0xa(%bp)
	sbb    %dx,-0x8(%bp)
	jmp   L_0661
L_0649:
	/* TODO: data ref: movw   $0x1,0x4aa2 */
	movw   $0x1,D_2aa2
	mov    $0x10,%ax
	push   %ax
	/* TODO: data ref: mov    $0x234a,%ax */
	mov    $D_034a,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2332,%ax */
	mov    $D_0332,%ax
	push   %ax
	call   L_0da6
	add    $0x6,%sp
L_0661:
	movw   $0x0,-0x6(%bp)
L_0666:
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x2344,%ax */
	cmp    D_0344,%ax
	jge   L_0692
	mov    -0x6(%bp),%bx
	mov    $0x3,%cx
	shl    %cl,%bx
	/* TODO: data ref: mov    0x4aae(%bx),%ax */
	mov    D_2aae(%bx),%ax
	/* TODO: data ref: mov    0x4ab0(%bx),%dx */
	mov    D_2ab0(%bx),%dx
	add    -0xa(%bp),%ax
	adc    -0x8(%bp),%dx
	/* TODO: data ref: mov    %ax,0x4aae(%bx) */
	mov    %ax,D_2aae(%bx)
	/* TODO: data ref: mov    %dx,0x4ab0(%bx) */
	mov    %dx,D_2ab0(%bx)
	incw   -0x6(%bp)
	jmp   L_0666
L_0692:
	/* TODO: data ref: mov    0x4aa2,%ax */
	mov    D_2aa2,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_069c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x1a,%sp
L_06ab:
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_21b2
	add    $0x4,%sp
	mov    %ax,-0x1e(%bp)
	cmp    $0x0,%ax
	jge   L_06d3
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x218c,%ax */
	mov    $D_018c,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2290,%ax */
	mov    $D_0290,%ax
	push   %ax
	call   L_08dc
	add    $0x6,%sp
	jmp   L_0727
L_06d3:
	xor    %ax,%ax
	push   %ax
	call   L_1012
	pop    %cx
	add    $0x5,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	mov    $0xc,%ax
	push   %ax
	/* TODO: data ref: mov    $0x21a5,%ax */
	mov    $D_01a5,%ax
	push   %ax
	lea    -0x1c(%bp),%ax
	push   %ax
	call   L_1030
	add    $0xa,%sp
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x235a,%ax */
	mov    $D_035a,%ax
	/* TODO: data ref: sub    $0x234a,%ax */
	sub    $D_034a,%ax
	cwtd
	add    $0x8,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	pushw  -0x1e(%bp)
	call   L_0ff4
	add    $0x8,%sp
	mov    $0xc,%ax
	push   %ax
	lea    -0x1c(%bp),%ax
	push   %ax
	pushw  -0x1e(%bp)
	call   L_225e
	add    $0x6,%sp
	pushw  -0x1e(%bp)
	call   L_2176
	pop    %cx
L_0727:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_072e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	mov    0x4(%bp),%di
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
	xor    %si,%si
L_0746:
	mov    (%di),%al
	cbtw
	jmp   L_074f
L_074b:
	inc    %si
L_074c:
	inc    %di
	jmp   L_0768
L_074f:
	cmp    $0x9,%ax
	je   L_0765
	cmp    $0x20,%ax
	je   L_0765
	cmp    $0x2b,%ax
	je   L_074c
	cmp    $0x2d,%ax
	je   L_074b
	jmp   L_0768
L_0765:
	inc    %di
	jmp   L_0746
L_0768:
	cmpb   $0x30,(%di)
	jl   L_07b2
	cmpb   $0x39,(%di)
	jg   L_07b2
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	call   L_1677
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
	jmp   L_0768
L_07b2:
	or     %si,%si
	je   L_07c5
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	jmp   L_07cb
L_07c5:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
L_07cb:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_07d2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%ax
	mulw   0x6(%bp)
	mov    %ax,0x4(%bp)
	pushw  0x4(%bp)
	call   L_1f20
	pop    %cx
	mov    %ax,%di
	or     %di,%di
	jne   L_07f2
	xor    %ax,%ax
	jmp   L_0813
L_07f2:
	mov    %di,%si
	mov    0x4(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
L_0802:
	decw   -0x6(%bp)
	jl   L_0811
	mov    %si,%bx
	inc    %si
	inc    %si
	movw   $0x0,(%bx)
	jmp   L_0802
L_0811:
	mov    %di,%ax
L_0813:
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
	call   L_206c
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0828:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x2(%bx)
	jge   L_0841
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
L_0841:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_0859
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%ax
	neg    %ax
	mov    %ax,%di
	jmp   L_0892
L_0859:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
	je   L_088b
	xor    %di,%di
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	je   L_0892
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	jne   L_0892
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	mov    0x4(%bp),%bx
	sub    0x4(%bx),%ax
	mov    %ax,%di
	jmp   L_0892
L_088b:
	mov    $0xffff,%ax
	mov    %ax,%dx
	jmp   L_08d4
L_0892:
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
	call   L_0ff4
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmp    $0x0,%dx
	jg   L_08c5
	jl   L_08bd
	cmpw   $0x0,-0x8(%bp)
	jae   L_08c5
L_08bd:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	jmp   L_08d4
L_08c5:
	mov    %di,%ax
	cwtd
	add    %ax,-0x8(%bp)
	adc    %dx,-0x6(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
L_08d4:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_08dc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_1082
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_0902
	mov    $0xffff,%ax
	jmp   L_0904
L_0902:
	xor    %ax,%ax
L_0904:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0908:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	/* TODO: data ref: cmpb   $0x0,0x21ac */
	cmpb   $0x0,D_01ac
	jne   L_0945
	/* TODO: data ref: movw   $0x2298,-0x8(%bp) */
	movw   $D_0298,-0x8(%bp)
	/* TODO: data ref: movw   $0x2320,0x2320 */
	/* suppressed: movw $D_0320,D_0320 (self-ref in .data) */
	movw   $0x88,-0xa(%bp)
L_092b:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_0940
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_092b
L_0940:
	/* TODO: data ref: movb   $0x1,0x21ac */
	movb   $0x1,D_01ac
L_0945:
	/* TODO: data ref: movw   $0x2280,-0x6(%bp) */
	movw   $D_0280,-0x6(%bp)
L_094a:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_0969
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x2320,%ax */
	cmp    D_0320,%ax
	jb   L_0963
	xor    %ax,%ax
	jmp   L_09d4
L_0963:
	addw   $0x8,-0x6(%bp)
	jmp   L_094a
L_0969:
	cmpb   $0x77,(%di)
	jne   L_097f
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0a06
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_09be
L_097f:
	cmpb   $0x61,(%di)
	jne   L_09c0
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_21b2
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_09ad
	cmpw   $0x2,D_4a16
	jne   L_09ad
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0a06
	add    $0x4,%sp
	mov    %ax,%si
L_09ad:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_0ff4
	add    $0x8,%sp
L_09be:
	jmp   L_09ce
L_09c0:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_21b2
	add    $0x4,%sp
	mov    %ax,%si
L_09ce:
	or     %si,%si
	jge   L_09d6
	xor    %ax,%ax
L_09d4:
	jmp   L_09fe
L_09d6:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_09f4
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_09fb
L_09f4:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_09fb:
	mov    -0x6(%bp),%ax
L_09fe:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0a06:
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
	jns    L_0a1a
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_0a1a:
	pop    %bp
	ret


L_0a1c:
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
	jne   L_0a3a
	jmp   L_0b83
L_0a3a:
	cmpw   $0x2,0xa(%bp)
	jl   L_0a43
	jmp   L_0b24
L_0a43:
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_0a4f
	jmp   L_0b24
L_0a4f:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0a5e
	jmp   L_0b24
L_0a5e:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%si
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,0xa(%bp)
	jne   L_0ab7
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
	call   L_0ff4
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
	jmp   L_0ad2
L_0ab7:
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
L_0ad2:
	or     %si,%si
	jle   L_0b13
	mov    %si,%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jg   L_0b13
	jl   L_0ae5
	cmp    %ax,-0x8(%bp)
	ja   L_0b13
L_0ae5:
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x4(%bp),%bx
	sub    (%bx),%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jl   L_0b13
	jg   L_0afd
	cmp    %ax,-0x8(%bp)
	jb   L_0b13
L_0afd:
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	add    %ax,(%bx)
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	sub    %ax,0x2(%bx)
	xor    %ax,%ax
	jmp   L_0bc4
L_0b13:
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x1,%ax
	and    $0x0,%dx
	mov    %ax,%di
	jmp   L_0b26
L_0b24:
	xor    %di,%di
L_0b26:
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
	call   L_0ff4
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
	or     %di,%di
	je   L_0b8d
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_0b7a
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0bb3
L_0b7a:
	pushw  0x4(%bp)
	call   L_0ca8
	pop    %cx
	jmp   L_0bb3
L_0b83:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
L_0b8d:
	je   L_0bb3
	pushw  0x4(%bp)
	call   L_1e3a
	pop    %cx
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_0ff4
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_0bb3:
	cmpw   $0xffff,-0x6(%bp)
	jne   L_0bc6
	cmpw   $0xffff,-0x8(%bp)
	jne   L_0bc6
	mov    $0xffff,%ax
L_0bc4:
	jmp   L_0bc8
L_0bc6:
	xor    %ax,%ax
L_0bc8:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0bd0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_0c31
L_0beb:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_0c31
	mov    0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0bf9:
	decw   0x2(%si)
	jl   L_0c0a
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0c0f
L_0c0a:
	push   %si
	call   L_0ca8
	pop    %cx
L_0c0f:
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jl   L_0c22
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_0c27
L_0c22:
	mov    -0x8(%bp),%ax
	jmp   L_0c34
L_0c27:
	decw   -0xa(%bp)
	jne   L_0bf9
	incw   -0x8(%bp)
	jmp   L_0beb
L_0c31:
	mov    -0x8(%bp),%ax
L_0c34:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0c3b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_0c9d
L_0c56:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_0c9d
	mov    0x6(%bp),%ax
	mov    %ax,-0x6(%bp)
L_0c64:
	decw   0x2(%si)
	jl   L_0c7c
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
	jmp   L_0c8a
L_0c7c:
	push   %si
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1d4c
	add    $0x4,%sp
L_0c8a:
	decw   -0x6(%bp)
	jne   L_0c64
	mov    0x6(%si),%al
	cbtw
	test   $0x20,%ax
	jne   L_0c9d
	incw   -0x8(%bp)
	jmp   L_0c56
L_0c9d:
	mov    -0x8(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0ca8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_0cbf
	mov    $0xffff,%ax
	jmp   L_0ccb
L_0cbf:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_0ccd
	mov    $0xffff,%ax
L_0ccb:
	jmp   L_0d45
L_0ccd:
	cmpw   $0x0,0x4(%di)
	jne   L_0d01
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0ce8
	mov    0x7(%di),%al
	cbtw
	/* TODO: data ref: add    $0x21ae,%ax */
	add    $D_01ae,%ax
	mov    %ax,0x4(%di)
	jmp   L_0ccd
L_0ce8:
	mov    $0x200,%ax
	push   %ax
	call   L_1f20
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_0cfd
	orb    $0x4,0x6(%di)
	jmp   L_0ccd
L_0cfd:
	orb    $0x8,0x6(%di)
L_0d01:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0d14
	mov    $0x1,%ax
	jmp   L_0d17
L_0d14:
	mov    $0x200,%ax
L_0d17:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_21c8
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_0d47
	cmpw   $0xffff,0x2(%di)
	jne   L_0d39
	orb    $0x10,0x6(%di)
	jmp   L_0d3d
L_0d39:
	orb    $0x20,0x6(%di)
L_0d3d:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_0d45:
	jmp   L_0d51
L_0d47:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_0d51:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0d56:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_0d63:
	decw   0x8(%bp)
	jl   L_0d87
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_0d87
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_0d63
	xor    %ax,%ax
	jmp   L_0da2
L_0d87:
	cmpw   $0x0,0x8(%bp)
	jge   L_0d91
	xor    %ax,%ax
	jmp   L_0da2
L_0d91:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_0da2:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0da6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
L_0dbe:
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_0df4
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0def
L_0dd7:
	incw   -0x6(%bp)
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_0dea
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	jmp   L_0dd7
L_0dea:
	mov    -0x8(%bp),%ax
	jmp   L_0df7
L_0def:
	incw   -0x6(%bp)
	jmp   L_0dbe
L_0df4:
	mov    -0x8(%bp),%ax
L_0df7:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0dfe:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	call   L_0ec4
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jne   L_0e30
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x21cd,%ax */
	mov    $D_01cd,%ax
	push   %ax
	/* TODO: data ref: mov    $0x21ca,%ax */
	mov    $D_01ca,%ax
	push   %ax
	/* TODO: data ref: mov    $0x21c2,%ax */
	mov    $D_01c2,%ax
	push   %ax
	call   L_0e92
	add    $0xa,%sp
	mov    $0x7f,%ax
	push   %ax
	call   L_0eb0
	pop    %cx
L_0e30:
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_0ed4
	add    $0x4,%sp
	mov    %ax,%di
	mov    $0x1,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_0ed4
	add    $0x4,%sp
	mov    %ax,%si
L_0e50:
	lea    -0x6(%bp),%ax
	push   %ax
	call   L_2242
	pop    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0x8(%bp),%ax
	je   L_0e66
	cmpw   $0xffff,-0xa(%bp)
	jne   L_0e50
L_0e66:
	cmpw   $0xffff,-0xa(%bp)
	jne   L_0e71
	movw   $0xffff,-0x6(%bp)
L_0e71:
	push   %di
	mov    $0x2,%ax
	push   %ax
	call   L_0ed4
	add    $0x4,%sp
	push   %si
	mov    $0x3,%ax
	push   %ax
	call   L_0ed4
	add    $0x4,%sp
	mov    -0x6(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0e92:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall exece(59) */
	mov    0x4(%bp),%ax
	/* TODO: data ref: mov    0x232a,%cx */
	mov    D_032a,%cx
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
	jns    L_0ead
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_0ead:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0eb0:
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
	jns    L_0ec1
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_0ec1:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0ec4:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall fork(2) */
	/* fork -> __NR_fork */
	mov	$__NR_fork,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_0ed2
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_0ed2:
	pop    %bp
	ret


L_0ed4:
	push   %bp
	mov    %sp,%bp
	mov    0x4(%bp),%cx
	or     %cx,%cx
	jle   L_0ee3
	cmp    $0x20,%cx
	jle   L_0eee
L_0ee3:
	movw   $0x16,D_4a16
	mov    $0xffff,%ax
	jmp   L_0f1d
L_0eee:
	mov    0x6(%bp),%dx
	mov    %cx,%bx
	shl    %bx
	add    $D_4a1a,%bx
	pushw  (%bx)
	mov    %dx,(%bx)
	cmp    $0x2,%dx
	jl   L_0f0c
	mov    $0x6,%ax
	mul    %cx
	add    $(L_0f1f-6),%ax
	mov    %ax,%dx
L_0f0c:
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
	jns    L_0f1d
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	mov    $0xffff,%ax
L_0f1d:
	pop    %bp
	ret
L_0f1f:
	push   %ax
	mov    $D_4a1c,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a1e,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a20,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a22,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a24,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a26,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a28,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a2a,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a2c,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a2e,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a30,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a32,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a34,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a36,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a38,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a3a,%ax
	jmp   L_0f83
	push   %ax
	mov    $D_4a3c,%ax
L_0f83:
	jmp   L_0fdd
	push   %ax
	mov    $D_4a3e,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a40,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a42,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a44,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a46,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a48,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a4a,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a4c,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a4e,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a50,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a52,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a54,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a56,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a58,%ax
	jmp   L_0fdd
	push   %ax
	mov    $D_4a5a,%ax
L_0fdd:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	sub    $D_4a1a,%ax
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


L_0ff4:
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
L_100f:
	pop    %si
	pop    %bp
	ret


L_1012:
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
	jns    L_1022
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_102e
L_1022:
	mov    0x4(%bp),%bx
	or     %bx,%bx
	je   L_102e
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_102e:
	pop    %bp
	ret


L_1030:
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
	call   L_1082
	add    $0x6,%sp
	decw   -0xa(%bp)
	jl   L_106a
	mov    -0xc(%bp),%bx
	incw   -0xc(%bp)
	movb   $0x0,(%bx)
	xor    %ax,%ax
	jmp   L_1077
L_106a:
	lea    -0xc(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_1d4c
	add    $0x4,%sp
L_1077:
	mov    0x4(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1082:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_109a:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_10ac
	jmp   L_1649
L_10ac:
	cmpb   $0x25,-0x5(%bp)
	je   L_10bf
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_10bd:
	jmp   L_109a
L_10bf:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_10ea
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1c5a
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_10ea:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_1103
	incb   -0xa6(%bp)
	inc    %di
L_1103:
	cmpb   $0x30,(%di)
	jne   L_110e
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_110e:
	cmpb   $0x2a,(%di)
	je   L_113e
	movw   $0x0,-0xe(%bp)
L_1118:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_1154
	cmpw   $0x9,-0xc(%bp)
	jg   L_1154
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_1118
L_113e:
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
L_1154:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_119b
	cmpb   $0x2a,(%di)
	je   L_119d
	movw   $0x0,-0x10(%bp)
L_116d:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_1197
	cmpw   $0x9,-0xc(%bp)
	jg   L_1197
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_116d
L_1197:
	addw   $0x30,-0xc(%bp)
L_119b:
	jmp   L_11b4
L_119d:
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
L_11b4:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_15be
L_11bf:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_11d0
L_11c7:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_1200
	jmp   L_11f9
L_11d0:
	cmp    $0x64,%ax
	je   L_1202
	cmp    $0x6f,%ax
	je   L_11e6
	cmp    $0x75,%ax
	je   L_11f9
	cmp    $0x78,%ax
	je   L_11f4
	jmp   L_11c7
L_11e6:
	movw   $0x8,-0x12(%bp)
	jmp   L_11f9
L_11ed:
	movw   $0x8,-0x12(%bp)
	jmp   L_1200
L_11f4:
	movw   $0x10,-0x12(%bp)
L_11f9:
	jmp   L_124b
L_11fb:
	movw   $0x10,-0x12(%bp)
L_1200:
	jmp   L_125b
L_1202:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_121f
L_1212:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_121f:
	cmpw   $0x0,-0x14(%bp)
	jg   L_1269
	jl   L_122d
	cmpw   $0x0,-0x16(%bp)
	jae   L_1269
L_122d:
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
	jmp   L_1269
L_124b:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_1269
L_125b:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_1269:
	cmpw   $0x0,-0x14(%bp)
	jne   L_1273
	cmpw   $0x0,-0x16(%bp)
L_1273:
	jne   L_1284
	cmpw   $0x0,-0x10(%bp)
	jne   L_1284
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_1284:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_128b:
	cmpw   $0x0,-0x14(%bp)
	jne   L_1295
	cmpw   $0x0,-0x16(%bp)
L_1295:
	je   L_12e0
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_17a8
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_12b6
	mov    $0x30,%ax
	jmp   L_12b9
L_12b6:
	mov    $0x57,%ax
L_12b9:
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
	call   L_16a0
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_128b
L_12e0:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_132f
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_12e0
L_12fe:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_130a:
	cmpw   $0x2,-0xc(%bp)
	jge   L_132f
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_132a
	decw   -0x8(%bp)
L_132a:
	incw   -0xc(%bp)
	jmp   L_130a
L_132f:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_1336:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_134d
	movw   $0x0,-0xc(%bp)
L_134d:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_135c
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_135c:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_1c5a
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_137f:
	jmp   L_10bd
L_1382:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_1394
	/* TODO: data ref: movw   $0x21d0,-0xa(%bp) */
	movw   $D_01d0,-0xa(%bp)
L_1394:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_13a3
	movw   $0x7fff,-0xc(%bp)
L_13a3:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_13a9:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1336
	decw   -0xc(%bp)
	jl   L_1336
	incw   -0x8(%bp)
	jmp   L_13a9
L_13bb:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_13c3:
	jmp   L_137f
L_13c5:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_13d1
	movw   $0x6,-0x10(%bp)
L_13d1:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_19a6
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_1406
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_1406:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_141b
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_1433
L_141b:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_141b
L_1433:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_1446
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_1446:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_1465
L_1452:
	decw   -0xc(%bp)
	jl   L_1465
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_1452
L_1465:
	cmpw   $0x0,-0xc(%bp)
	jg   L_146e
	jmp   L_132f
L_146e:
	decw   -0xc(%bp)
	jge   L_1476
	jmp   L_132f
L_1476:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_146e
L_148b:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1499
	movw   $0x6,-0x10(%bp)
	jmp   L_149c
L_1499:
	incw   -0x10(%bp)
L_149c:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_197a
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_14d1
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_14d1:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_14dc
	incw   -0x1a(%bp)
L_14dc:
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
L_14fe:
	decw   -0xc(%bp)
	jle   L_1518
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_14fe
L_1518:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_1535
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_1546
L_1535:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_1546:
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
	jmp   L_132f
L_1577:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1583
	movw   $0x6,-0x10(%bp)
L_1583:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_17fe
	add    $0xc,%sp
	add    $0x8,%si
L_15a1:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_15a1
	decw   -0x8(%bp)
	jmp   L_132f
L_15b2:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_15bb:
	jmp   L_13c3
L_15be:
	cmp    $0x65,%ax
	jne   L_15c6
	jmp   L_148b
L_15c6:
	jg   L_1607
	cmp    $0x4f,%ax
	jne   L_15d0
	jmp   L_11e6
L_15d0:
	jg   L_15e7
	cmp    $0x25,%ax
	je   L_15b2
	cmp    $0x44,%ax
	jne   L_15df
	jmp   L_1202
L_15df:
	cmp    $0x4c,%ax
L_15e2:
	jne   L_15e7
	jmp   L_11bf
L_15e7:
	cmp    $0x55,%ax
	jne   L_15ef
	jmp   L_124b
L_15ef:
	cmp    $0x58,%ax
	jne   L_15f7
	jmp   L_11f4
L_15f7:
	cmp    $0x63,%ax
	jne   L_15ff
	jmp   L_12fe
L_15ff:
	cmp    $0x64,%ax
	jne   L_1607
	jmp   L_1212
L_1607:
	cmp    $0x6f,%ax
	jne   L_160f
	jmp   L_11ed
L_160f:
	jg   L_1626
	cmp    $0x66,%ax
	jne   L_1619
	jmp   L_13c5
L_1619:
	cmp    $0x67,%ax
	jne   L_1621
	jmp   L_1577
L_1621:
	cmp    $0x6c,%ax
	je   L_15e2
L_1626:
	cmp    $0x72,%ax
	jne   L_162e
	jmp   L_13bb
L_162e:
	cmp    $0x73,%ax
	jne   L_1636
	jmp   L_1382
L_1636:
	cmp    $0x75,%ax
	jne   L_163e
	jmp   L_125b
L_163e:
	cmp    $0x78,%ax
	jne   L_1646
	jmp   L_11fb
L_1646:
	jmp   L_15bb
L_1649:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_166d
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1c5a
	add    $0x8,%sp
L_166d:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_17dc


L_1677:
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
	call   L_17dc


L_16a0:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_16be
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_16be:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_16c7:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_16c7
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_16d7:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_16d7
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1677
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_1718
	jne   L_170d
	or     $0x0,%ax
	je   L_1718
L_170d:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_1718:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_17dc


L_1720:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_173c
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_173c:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_1753
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_1753:
	push   %dx
	push   %ax
	call   L_16a0
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_176d
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_176d:
	ret
	call   L_17dc
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_1720
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1677
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_17dc


L_17a8:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_16a0
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_1677
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_17dc:
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


L_17fe:
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
	call   L_197a
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_1838
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_1838:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_183f:
	cmpw   $0x0,-0xa(%bp)
	jle   L_1859
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_1859
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_183f
L_1859:
	cmpw   $0x0,-0x8(%bp)
	jl   L_186a
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_1876
L_186a:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1874
	cmpw   $0xfffd,-0x8(%bp)
L_1874:
	jge   L_18f5
L_1876:
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
L_1891:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_18ab
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_1891
L_18ab:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_18c7
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_18cd
L_18c7:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_18cd:
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
	jmp   L_1965
L_18f5:
	cmpw   $0x0,-0x8(%bp)
	jg   L_1917
	cmpb   $0x30,(%di)
	je   L_1906
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1906:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1917
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_1906
L_1917:
	movw   $0x1,-0xa(%bp)
L_191c:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_1944
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
	jne   L_193f
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_193f:
	incw   -0xa(%bp)
	jmp   L_191c
L_1944:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_1965
L_194c:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_195f
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_194c
L_195f:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1965:
	cmpb   $0x2e,-0x1(%si)
	jne   L_196c
	dec    %si
L_196c:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_197a:
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
	call   L_19d1
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_19a6:
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
	call   L_19d1
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_19d1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_19e4
	movw   $0x0,0xc(%bp)
L_19e4:
	cmpw   $0x4f,0xc(%bp)
	jl   L_19ef
	movw   $0x4e,0xc(%bp)
L_19ef:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	/* TODO: data ref: mov    $0x21d8,%si */
	mov    $D_01d8,%si
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x2228 */
	fcompl D_0228
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_1a2c
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_1a2c:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2278
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	/* TODO: data ref: movw   $0x2228,-0x16(%bp) */
	movw   $D_0228,-0x16(%bp)
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x2230 */
	fcompl D_0230
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_1a77
	jmp   L_1b24
L_1a77:
	/* TODO: data ref: movw   $0x2228,-0x16(%bp) */
	movw   $D_0228,-0x16(%bp)
L_1a7c:
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x2238 */
	fcompl D_0238
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_1a98
	jmp   L_1b22
L_1a98:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	/* TODO: data ref: fdivl  0x2240 */
	fdivl  D_0240
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2278
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	/* TODO: data ref: fldl   0x2248 */
	fldl   D_0248
	fstl   -0x1e(%bp)
	/* TODO: data ref: fdivl  0x2250 */
	fdivl  D_0250
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	/* TODO: data ref: fmull  0x2258 */
	fmull  D_0258
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
	jmp   L_1a7c
L_1b0b:
	/* TODO: data ref: cmpw   $0x2228,-0x16(%bp) */
	cmpw   $D_0228,-0x16(%bp)
	jae   L_1b65
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_1b22:
	jmp   L_1b0b
L_1b24:
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x2260 */
	fcompl D_0260
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_1b74
L_1b3d:
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x2268 */
	fmull  D_0268
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	/* TODO: data ref: fcompl 0x2270 */
	fcompl D_0270
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_1b65:
	jae   L_1b74
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_1b3d
L_1b74:
	mov    0xc(%bp),%ax
	/* TODO: data ref: add    $0x21d8,%ax */
	add    $D_01d8,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_1b86
	add    %di,-0x16(%bp)
L_1b86:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	/* TODO: data ref: cmpw   $0x21d8,-0x16(%bp) */
	cmpw   $D_01d8,-0x16(%bp)
	jae   L_1b9c
	/* TODO: data ref: movb   $0x0,0x21d8 */
	movb   $0x0,D_01d8
	/* TODO: data ref: mov    $0x21d8,%ax */
	mov    $D_01d8,%ax
	jmp   L_1c07
L_1b9c:
	cmp    -0x16(%bp),%si
	ja   L_1bf8
	/* TODO: data ref: cmp    $0x2228,%si */
	cmp    $D_0228,%si
	jae   L_1bf8
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x2278 */
	fmull  D_0278
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2278
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
	jmp   L_1b9c
L_1bf8:
	/* TODO: data ref: cmpw   $0x2228,-0x16(%bp) */
	cmpw   $D_0228,-0x16(%bp)
	jb   L_1c09
	/* TODO: data ref: movb   $0x0,0x2227 */
	movb   $0x0,D_0227
	/* TODO: data ref: mov    $0x21d8,%ax */
	mov    $D_01d8,%ax
L_1c07:
	jmp   L_1c53
L_1c09:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_1c11:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_1c4d
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	/* TODO: data ref: cmpw   $0x21d8,-0x16(%bp) */
	cmpw   $D_01d8,-0x16(%bp)
	jbe   L_1c30
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_1c11
L_1c30:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_1c11
	/* TODO: data ref: cmp    $0x21d8,%si */
	cmp    $D_01d8,%si
	jbe   L_1c4a
	movb   $0x30,(%si)
L_1c4a:
	inc    %si
	jmp   L_1c11
L_1c4d:
	movb   $0x0,(%si)
	/* TODO: data ref: mov    $0x21d8,%ax */
	mov    $D_01d8,%ax
L_1c53:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1c5a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1c67:
	cmpw   $0x0,0x8(%bp)
	jl   L_1c70
	jmp   L_1cfe
L_1c70:
	cmpb   $0x2d,(%di)
	jne   L_1cad
	cmpw   $0x30,0xc(%bp)
	jne   L_1cad
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1c9c
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
	jmp   L_1cac
L_1c9c:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1d4c
	add    $0x4,%sp
L_1cac:
	dec    %si
L_1cad:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1ccb
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
	jmp   L_1cd7
L_1ccb:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_1d4c
	add    $0x4,%sp
L_1cd7:
	incw   0x8(%bp)
	jmp   L_1c67
L_1cdc:
	dec    %si
	jl   L_1d12
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1d00
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
L_1cfe:
	jmp   L_1cdc
L_1d00:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1d4c
	add    $0x4,%sp
	jmp   L_1cdc
L_1d12:
	cmpw   $0x0,0x8(%bp)
	je   L_1d47
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1d36
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
	jmp   L_1d42
L_1d36:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_1d4c
	add    $0x4,%sp
L_1d42:
	decw   0x8(%bp)
	jmp   L_1d12
L_1d47:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1d4c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_1d67
	mov    $0xffff,%ax
	jmp   L_1e2e
L_1d67:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1d9a
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
	call   L_225e
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_1e1f
L_1d9a:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1dec
	/* TODO: data ref: cmp    $0x2288,%di */
	cmp    $D_0288,%di
	jne   L_1dc5
	/* TODO: data ref: mov    0x228f,%al */
	mov    D_028f,%al
	cbtw
	push   %ax
	call   L_218a
	pop    %cx
	or     %ax,%ax
	je   L_1dba
	orb    $0x4,0x6(%di)
	jmp   L_1d67
L_1dba:
	movw   $D_4a62,0x4(%di)
	movw   $D_4a62,(%di)
	jmp   L_1d67
L_1dc5:
	mov    $0x200,%ax
	push   %ax
	call   L_1f20
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1ddc
	orb    $0x4,0x6(%di)
	jmp   L_1d67
L_1ddc:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_1e0e
L_1dec:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_1e0e
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_225e
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_1e0e:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_1e1f:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_1e30
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_1e2e:
	jmp   L_1e33
L_1e30:
	mov    0x4(%bp),%ax
L_1e33:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e3a:
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
	jne   L_1e86
	mov    0x4(%di),%si
	or     %si,%si
	je   L_1e86
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_1e86
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_225e
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_1e86
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_1e88
L_1e86:
	xor    %ax,%ax
L_1e88:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e8d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x2280,%di */
	mov    $D_0280,%di
L_1e95:
	/* TODO: data ref: cmp    0x2320,%di */
	cmp    D_0320,%di
	jae   L_1ea5
	push   %di
	call   L_1ea9
	pop    %cx
	add    $0x8,%di
	jmp   L_1e95
L_1ea5:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ea9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_1efc
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_1efc
	push   %di
	call   L_1e3a
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_2176
	pop    %cx
	or     %ax,%ax
	jge   L_1ede
	mov    $0xffff,%si
L_1ede:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_1eee
	pushw  0x4(%di)
	call   L_206c
	pop    %cx
L_1eee:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_1efc
	movw   $0x0,0x4(%di)
L_1efc:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_1f20:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: cmpw   $0x0,0x2322 */
	cmpw   $0x0,D_0322
	jne   L_1f4b
	/* TODO: data ref: mov    $0x2324,%ax */
	mov    $D_0324,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x2322 */
	mov    %ax,D_0322
	/* TODO: data ref: mov    $0x2322,%ax */
	mov    $D_0322,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x2324 */
	mov    %ax,D_0324
	/* TODO: data ref: movw   $0x2324,0x6e64 */
	/* dual-reloc split: movw $D_0324,D_4e64 */
	mov	$D_0324,%ax
	mov	%ax,D_4e64
	/* TODO: data ref: movw   $0x2322,0x6e62 */
	/* dual-reloc split: movw $D_0322,D_4e62 */
	mov	$D_0322,%ax
	mov	%ax,D_4e62
L_1f4b:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_4e62,%di
L_1f5f:
	/* TODO: data ref: movw   $0x0,0x2326 */
	movw   $0x0,D_0326
L_1f65:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_1f94
L_1f6c:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_1f7b
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_1f6c
L_1f7b:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_1f94
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_1f94
	jmp   L_203d
L_1f94:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	jbe   L_1fa3
L_1fa1:
	jmp   L_1f65
L_1fa3:
	cmp    D_4e64,%si
	jne   L_1faf
	/* TODO: data ref: cmp    $0x2322,%di */
	cmp    $D_0322,%di
	je   L_1fb3
L_1faf:
	xor    %ax,%ax
	jmp   L_1feb
L_1fb3:
	/* TODO: data ref: incw   0x2326 */
	incw   D_0326
	/* TODO: data ref: cmpw   $0x1,0x2326 */
	cmpw   $0x1,D_0326
	jle   L_1fa1
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	/* TODO: data ref: mov    %ax,0x2326 */
	mov    %ax,D_0326
	xor    %ax,%ax
	push   %ax
	call   L_2150
	pop    %cx
	mov    %ax,%si
	/* TODO: data ref: mov    0x2326,%ax */
	mov    D_0326,%ax
	shl    %ax
	add    %si,%ax
	add    $0x0,%ax
	cmp    %si,%ax
	jae   L_1fed
	xor    %ax,%ax
L_1feb:
	jmp   L_2000
L_1fed:
	/* TODO: data ref: mov    0x2326,%ax */
	mov    D_0326,%ax
	shl    %ax
	push   %ax
	call   L_2150
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_2002
	xor    %ax,%ax
L_2000:
	jmp   L_2067
L_2002:
	mov    D_4e64,%bx
	mov    %si,(%bx)
	mov    D_4e64,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_2020
	mov    D_4e64,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_4e64,%bx
	mov    %ax,(%bx)
L_2020:
	/* TODO: data ref: mov    0x2326,%ax */
	mov    D_0326,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_4e64
	/* TODO: data ref: mov    $0x2322,%ax */
	mov    $D_0322,%ax
	or     $0x1,%ax
	mov    D_4e64,%bx
	mov    %ax,(%bx)
	jmp   L_1f5f
L_203d:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_4e62
	cmp    %ax,%si
	jbe   L_205c
	mov    D_4e62,%bx
	mov    (%bx),%ax
	mov    %ax,D_4e66
	mov    D_4e62,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_205c:
	mov    D_4e62,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_2067:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_206c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_4e62
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
	je   L_20a1
	push   %di
	call   L_206c
	pop    %cx
L_20a1:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_1f20
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_20c0
	cmp    %di,%si
	jne   L_20c4
L_20c0:
	mov    %si,%ax
	jmp   L_212c
L_20c4:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_20e3
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_20e3:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_2101
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_20e3
L_2101:
	cmp    %di,%si
	jae   L_212a
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_212a
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
	mov    D_4e66,%ax
	mov    %ax,(%bx)
L_212a:
	mov    %si,%ax
L_212c:
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
	jns    L_2147
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_214e
L_2147:
	mov    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x2328 */
	mov    %bx,D_0328
L_214e:
	pop    %bp
	ret


L_2150:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	/* TODO: data ref: add    0x2328,%ax */
	add    D_0328,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2167
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_2174
L_2167:
	/* TODO: data ref: mov    0x2328,%bx */
	mov    D_0328,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x2328 */
	mov    %bx,D_0328
L_2174:
	pop    %bp
	ret


L_2176:
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
	jns    L_2187
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2187:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_218a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_21fb
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_21a7
	xor    %ax,%ax
	jmp   L_21aa
L_21a7:
	mov    $0x1,%ax
L_21aa:
	lea    -0x4(%bp),%sp
	pop    %di
L_21ae:
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_21b2:
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
	je	open_wronly_5_7190
	and	$0xfffc,%cx
	or	$0x241,%cx
	mov	$0x1b6,%dx
open_wronly_5_7190:
	mov	$__NR_open,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_21c6
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_21c6:
	pop    %bp
	ret


L_21c8:
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
	jns    L_21df
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_21df:
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
	call   L_2214
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_21fb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2214
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2214:
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
	jns    L_222b
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_222b:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_222e:
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
	jns    L_223f
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_223f:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2242:
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
	jns    L_2252
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2250:
	pop    %bp
	ret
L_2252:
	mov    0x4(%bp),%bx
	cmp    $0x0,%bx
	je   L_2250
	mov    %dx,(%bx)
	jmp   L_2250


L_225e:
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
	jns    L_2275
	neg	%ax
	mov	%ax,D_4a16	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2275:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2278:
	push   %bp
	mov    %sp,%bp
	mov    0xc(%bp),%bx
L_2280:
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
	mov	%ax,D_4a16	/* errno */
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
	.ascii	"@(#)ranlib.c 4.4 3/17/83\0"
	.byte	0x00	/* |.| */
D_001e:
	.ascii	"__.SYMDEF\0"
D_0028:
	.byte	0x72, 0x00	/* |r.| */
D_002a:
	.byte	0x72, 0x61, 0x6e, 0x6c, 0x69, 0x62, 0x3a, 0x20	/* |ranlib: | */
	.byte	0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6f	/* |cannot o| */
	.byte	0x70, 0x65, 0x6e, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |pen %s..| */
D_0042:
	.byte	0x21, 0x3c, 0x61, 0x72, 0x63, 0x68, 0x3e, 0x0a	/* |!<arch>.| */
	.byte	0x00	/* |.| */
D_004b:
	.ascii	"old format \0"
D_0057:
	.ascii	"not an \0"
D_005f:
	.byte	0x61, 0x72, 0x63, 0x68, 0x69, 0x76, 0x65, 0x3a	/* |archive:| */
	.byte	0x20, 0x25, 0x73, 0x0a, 0x00	/* | %s..| */
D_006c:
	.byte	0x72, 0x61, 0x6e, 0x6c, 0x69, 0x62, 0x3a, 0x20	/* |ranlib: | */
	.byte	0x77, 0x61, 0x72, 0x6e, 0x69, 0x6e, 0x67, 0x3a	/* |warning:| */
	.byte	0x20, 0x25, 0x73, 0x28, 0x25, 0x73, 0x29, 0x3a	/* | %s(%s):| */
	.byte	0x20, 0x6e, 0x6f, 0x20, 0x73, 0x79, 0x6d, 0x62	/* | no symb| */
	.byte	0x6f, 0x6c, 0x20, 0x74, 0x61, 0x62, 0x6c, 0x65	/* |ol table| */
	.byte	0x0a, 0x00	/* |..| */
D_0096:
	.byte	0x72, 0x61, 0x6e, 0x6c, 0x69, 0x62, 0x3a, 0x20	/* |ranlib: | */
	.byte	0x25, 0x73, 0x28, 0x25, 0x73, 0x29, 0x3a, 0x20	/* |%s(%s): | */
	.byte	0x6f, 0x6c, 0x64, 0x20, 0x66, 0x6f, 0x72, 0x6d	/* |old form| */
	.byte	0x61, 0x74, 0x20, 0x2e, 0x6f, 0x20, 0x66, 0x69	/* |at .o fi| */
	.byte	0x6c, 0x65, 0x0a, 0x00	/* |le..| */
D_00ba:
	.byte	0x72, 0x61, 0x6e, 0x6c, 0x69, 0x62, 0x3a, 0x20	/* |ranlib: | */
	.byte	0x77, 0x61, 0x72, 0x6e, 0x69, 0x6e, 0x67, 0x3a	/* |warning:| */
	.byte	0x20, 0x25, 0x73, 0x28, 0x25, 0x73, 0x29, 0x3a	/* | %s(%s):| */
	.byte	0x20, 0x6e, 0x6f, 0x20, 0x73, 0x74, 0x72, 0x69	/* | no stri| */
	.byte	0x6e, 0x67, 0x20, 0x74, 0x61, 0x62, 0x6c, 0x65	/* |ng table| */
	.byte	0x0a, 0x00	/* |..| */
D_00e4:
	.byte	0x72, 0x61, 0x6e, 0x6c, 0x69, 0x62, 0x3a, 0x20	/* |ranlib: | */
	.byte	0x72, 0x61, 0x6e, 0x20, 0x6f, 0x75, 0x74, 0x20	/* |ran out | */
	.byte	0x6f, 0x66, 0x20, 0x6d, 0x65, 0x6d, 0x6f, 0x72	/* |of memor| */
	.byte	0x79, 0x0a, 0x00	/* |y..| */
D_00ff:
	.byte	0x77, 0x00	/* |w.| */
D_0101:
	.byte	0x63, 0x61, 0x6e, 0x27, 0x74, 0x20, 0x63, 0x72	/* |can't cr| */
	.byte	0x65, 0x61, 0x74, 0x65, 0x20, 0x74, 0x65, 0x6d	/* |eate tem| */
	.byte	0x70, 0x6f, 0x72, 0x61, 0x72, 0x79, 0x0a, 0x00	/* |porary..| */
D_0119:
	.byte	0x61, 0x72, 0x20, 0x72, 0x6c, 0x62, 0x20, 0x25	/* |ar rlb %| */
	.byte	0x73, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73, 0x0a	/* |s %s %s.| */
	.byte	0x00	/* |.| */
D_012a:
	.byte	0x61, 0x72, 0x20, 0x72, 0x6c, 0x20, 0x25, 0x73	/* |ar rl %s| */
	.byte	0x20, 0x25, 0x73, 0x0a, 0x00	/* | %s..| */
D_0137:
	.byte	0x72, 0x61, 0x6e, 0x6c, 0x69, 0x62, 0x3a, 0x20	/* |ranlib: | */
	.byte	0x60, 0x60, 0x25, 0x73, 0x27, 0x27, 0x20, 0x66	/* |``%s'' f| */
	.byte	0x61, 0x69, 0x6c, 0x65, 0x64, 0x0a, 0x00	/* |ailed..| */
D_014e:
	.byte	0x72, 0x61, 0x6e, 0x6c, 0x69, 0x62, 0x3a, 0x20	/* |ranlib: | */
	.byte	0x73, 0x79, 0x6d, 0x62, 0x6f, 0x6c, 0x20, 0x74	/* |symbol t| */
	.byte	0x61, 0x62, 0x6c, 0x65, 0x20, 0x6f, 0x76, 0x65	/* |able ove| */
	.byte	0x72, 0x66, 0x6c, 0x6f, 0x77, 0x0a, 0x00	/* |rflow..| */
D_016d:
	.byte	0x72, 0x61, 0x6e, 0x6c, 0x69, 0x62, 0x3a, 0x20	/* |ranlib: | */
	.byte	0x73, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x20, 0x74	/* |string t| */
	.byte	0x61, 0x62, 0x6c, 0x65, 0x20, 0x6f, 0x76, 0x65	/* |able ove| */
	.byte	0x72, 0x66, 0x6c, 0x6f, 0x77, 0x0a, 0x00	/* |rflow..| */
D_018c:
	.byte	0x72, 0x61, 0x6e, 0x6c, 0x69, 0x62, 0x3a, 0x20	/* |ranlib: | */
	.byte	0x63, 0x61, 0x6e, 0x27, 0x74, 0x20, 0x72, 0x65	/* |can't re| */
	.byte	0x6f, 0x70, 0x65, 0x6e, 0x20, 0x25, 0x73, 0x0a	/* |open %s.| */
	.byte	0x00	/* |.| */
D_01a5:
	.ascii	"%-*ld\0"
	.byte	0x00	/* |.| */
D_01ac:
	.byte	0x00, 0x00	/* |..| */
D_01ae:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_01c2:
	.ascii	"/bin/sh\0"
D_01ca:
	.word	D_4873	/* ptr: 0x6873 */
	.byte	0x00	/* |.| */
D_01cd:
	.byte	0x2d, 0x63, 0x00	/* |-c.| */
D_01d0:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_01d8:
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
D_0227:
	.byte	0x00	/* |.| */
D_0228:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0230:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0238:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0240:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.word	D_2024	/* ptr: 0x4024 */
D_0248:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.word	D_2008	/* ptr: 0x4008 */
D_0250:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.word	D_2059	/* ptr: 0x4059 */
D_0258:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.word	D_2024	/* ptr: 0x4024 */
D_0260:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0268:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.word	D_2024	/* ptr: 0x4024 */
D_0270:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.word	D_1ff0	/* ptr: 0x3ff0 */
D_0278:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0280:
	.byte	0x62, 0x6c, 0x00, 0x00	/* |bl..| */
	.word	D_4c62	/* ptr: 0x6c62 */
	.byte	0x01, 0x00	/* |..| */
D_0288:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02	/* |.......| */
D_028f:
	.byte	0x01	/* |.| */
D_0290:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
D_0298:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0320:
	.word	D_0320	/* ptr: 0x2320 */
D_0322:
	.byte	0x00, 0x00	/* |..| */
D_0324:
	.byte	0x00, 0x00	/* |..| */
D_0326:
	.byte	0x00, 0x00	/* |..| */
D_0328:
	.word	D_4e68	/* ptr: 0x6e68 */

	.bss

D_032a:	.space	2	/* bss word @ 0x032a */
	.space	2
D_032e:	.space	2	/* bss word @ 0x032e */
D_0330:	.space	2	/* bss word @ 0x0330 */
D_0332:	.space	2	/* bss word @ 0x0332 */
	.space	16
D_0344:	.space	2	/* bss word @ 0x0344 */
	.space	4
D_034a:	.space	2	/* bss word @ 0x034a */
	.space	14
D_035a:	.space	2	/* bss word @ 0x035a */
	.space	30
D_037a:	.space	2	/* bss word @ 0x037a */
	.space	10
D_0386:	.space	2	/* bss word @ 0x0386 */
D_0388:	.space	2	/* bss word @ 0x0388 */
D_038a:	.space	2	/* bss word @ 0x038a */
	.space	6
D_0392:	.space	2	/* bss word @ 0x0392 */
	.space	892
D_0710:	.space	2	/* bss word @ 0x0710 */
	.space	2221
D_0fbf:	.space	2	/* bss word @ 0x0fbf */
	.space	4143
D_1ff0:	.space	2	/* bss word @ 0x1ff0 */
	.space	22
D_2008:	.space	2	/* bss word @ 0x2008 */
	.space	26
D_2024:	.space	2	/* bss word @ 0x2024 */
	.space	51
D_2059:	.space	2	/* bss word @ 0x2059 */
	.space	2631
D_2aa2:	.space	2	/* bss word @ 0x2aa2 */
D_2aa4:	.space	2	/* bss word @ 0x2aa4 */
D_2aa6:	.space	2	/* bss word @ 0x2aa6 */
	.space	2
D_2aaa:	.space	2	/* bss word @ 0x2aaa */
D_2aac:	.space	2	/* bss word @ 0x2aac */
D_2aae:	.space	2	/* bss word @ 0x2aae */
D_2ab0:	.space	2	/* bss word @ 0x2ab0 */
	.space	7617
D_4873:	.space	2	/* bss word @ 0x4873 */
	.space	373
D_49ea:	.space	2	/* bss word @ 0x49ea */
D_49ec:	.space	2	/* bss word @ 0x49ec */
	.space	4
D_49f2:	.space	2	/* bss word @ 0x49f2 */
	.space	2
D_49f6:	.space	2	/* bss word @ 0x49f6 */
D_49f8:	.space	2	/* bss word @ 0x49f8 */
D_49fa:	.space	2	/* bss word @ 0x49fa */
D_49fc:	.space	2	/* bss word @ 0x49fc */
	.space	4
D_4a02:	.space	2	/* bss word @ 0x4a02 */
D_4a04:	.space	2	/* bss word @ 0x4a04 */
	.space	4
D_4a0a:	.space	2	/* bss word @ 0x4a0a */
D_4a0c:	.space	2	/* bss word @ 0x4a0c */
D_4a0e:	.space	2	/* bss word @ 0x4a0e */
D_4a10:	.space	2	/* bss word @ 0x4a10 */
D_4a12:	.space	2	/* bss word @ 0x4a12 */
D_4a14:	.space	2	/* bss word @ 0x4a14 */
D_4a16:	.space	2	/* bss word @ 0x4a16 */
	.space	2
D_4a1a:	.space	2	/* bss word @ 0x4a1a */
D_4a1c:	.space	2	/* bss word @ 0x4a1c */
D_4a1e:	.space	2	/* bss word @ 0x4a1e */
D_4a20:	.space	2	/* bss word @ 0x4a20 */
D_4a22:	.space	2	/* bss word @ 0x4a22 */
D_4a24:	.space	2	/* bss word @ 0x4a24 */
D_4a26:	.space	2	/* bss word @ 0x4a26 */
D_4a28:	.space	2	/* bss word @ 0x4a28 */
D_4a2a:	.space	2	/* bss word @ 0x4a2a */
D_4a2c:	.space	2	/* bss word @ 0x4a2c */
D_4a2e:	.space	2	/* bss word @ 0x4a2e */
D_4a30:	.space	2	/* bss word @ 0x4a30 */
D_4a32:	.space	2	/* bss word @ 0x4a32 */
D_4a34:	.space	2	/* bss word @ 0x4a34 */
D_4a36:	.space	2	/* bss word @ 0x4a36 */
D_4a38:	.space	2	/* bss word @ 0x4a38 */
D_4a3a:	.space	2	/* bss word @ 0x4a3a */
D_4a3c:	.space	2	/* bss word @ 0x4a3c */
D_4a3e:	.space	2	/* bss word @ 0x4a3e */
D_4a40:	.space	2	/* bss word @ 0x4a40 */
D_4a42:	.space	2	/* bss word @ 0x4a42 */
D_4a44:	.space	2	/* bss word @ 0x4a44 */
D_4a46:	.space	2	/* bss word @ 0x4a46 */
D_4a48:	.space	2	/* bss word @ 0x4a48 */
D_4a4a:	.space	2	/* bss word @ 0x4a4a */
D_4a4c:	.space	2	/* bss word @ 0x4a4c */
D_4a4e:	.space	2	/* bss word @ 0x4a4e */
D_4a50:	.space	2	/* bss word @ 0x4a50 */
D_4a52:	.space	2	/* bss word @ 0x4a52 */
D_4a54:	.space	2	/* bss word @ 0x4a54 */
D_4a56:	.space	2	/* bss word @ 0x4a56 */
D_4a58:	.space	2	/* bss word @ 0x4a58 */
D_4a5a:	.space	2	/* bss word @ 0x4a5a */
	.space	6
D_4a62:	.space	2	/* bss word @ 0x4a62 */
	.space	510
D_4c62:	.space	2	/* bss word @ 0x4c62 */
	.space	510
D_4e62:	.space	2	/* bss word @ 0x4e62 */
D_4e64:	.space	2	/* bss word @ 0x4e64 */
D_4e66:	.space	2	/* bss word @ 0x4e66 */
D_4e68:	/* endcore: initial break */

/* end of file */
