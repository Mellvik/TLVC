/* ar.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 ar.s -o ar.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: ar  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x0000349e  (13470 bytes)
//   a_data:   0x000009f4  (2548 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %di,%di
L_003d:
	mov    %di,%bx
	shl    %bx
	cmpw   $0x0,D_001e(%bx)
	je   L_0074
	mov    $0x1,%ax
	push   %ax
	mov    %di,%bx
	shl    %bx
	pushw  D_001e(%bx)
	call   L_2046
	add    $0x4,%sp
	cmp    $0x1,%ax
	je   L_0071
	mov    $0x7e1,%ax
	push   %ax
	mov    %di,%bx
	shl    %bx
	pushw  D_001e(%bx)
	call   L_2046
	add    $0x4,%sp
L_0071:
	inc    %di
	jmp   L_003d
L_0074:
	cmpw   $0x3,0x4(%bp)
	jge   L_007d
	call   L_079b
L_007d:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%si
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%si
L_0089:
	cmpb   $0x0,(%si)
	jne   L_0091
	jmp   L_0116
L_0091:
	mov    (%si),%al
	cbtw
	jmp   L_0103
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	incb   D_0c4b(%bx)
	jmp   L_00db
	mov    $0x1f6,%ax
	push   %ax
	call   L_01c6
	pop    %cx
	jmp   L_00e5
	mov    $0x2a8,%ax
	push   %ax
	call   L_01c6
	pop    %cx
	jmp   L_00e5
	mov    $0x307,%ax
	push   %ax
	call   L_01c6
	pop    %cx
	jmp   L_00e5
	mov    $0x4cc,%ax
	push   %ax
	call   L_01c6
	pop    %cx
	jmp   L_0101
	mov    $0x3b3,%ax
	push   %ax
	call   L_01c6
	pop    %cx
	jmp   L_0101
	mov    $0x420,%ax
	push   %ax
	call   L_01c6
	pop    %cx
L_00db:
	jmp   L_0101
	mov    $0x525,%ax
	push   %ax
	call   L_01c6
	pop    %cx
L_00e5:
	jmp   L_0101
L_00e7:
	mov    (%si),%al
	cbtw
	push   %ax
	mov    $D_00f8,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
L_0101:
	jmp   L_0112
L_0103:
	sub    $0x61,%ax
	cmp    $0x17,%ax
	ja   L_00e7
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x202c(%bx)
L_0112:
	inc    %si
	jmp   L_0089
L_0116:
	cmpb   $0x0,D_0cb7
	je   L_012f
	/* dual-reloc split: movw $D_010d,D_0026 */
	mov	$D_010d,%ax
	mov	%ax,D_0026
	/* dual-reloc split: movw $D_0114,D_0028 */
	mov	$D_0114,%ax
	mov	%ax,D_0028
	/* dual-reloc split: movw $D_011c,D_002a */
	mov	$D_011c,%ax
	mov	%ax,D_002a
L_012f:
	cmpb   $0x0,D_0cb4
	je   L_013a
	incb   D_0cad
L_013a:
	cmpb   $0x0,D_0cac
	jne   L_0148
	cmpb   $0x0,D_0cad
	je   L_016b
L_0148:
	movw   $0x1,D_0cca
	mov    0x6(%bp),%bx
	pushw  0x4(%bx)
	call   L_0e3d
	pop    %cx
	mov    %ax,D_0a86
	addw   $0x2,0x6(%bp)
	decw   0x4(%bp)
	cmpw   $0x3,0x4(%bp)
	jge   L_016b
	call   L_079b
L_016b:
	mov    0x6(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,D_0a1c
	mov    0x6(%bp),%ax
	add    $0x6,%ax
	mov    %ax,D_0aa0
	mov    0x4(%bp),%ax
	sub    $0x3,%ax
	mov    %ax,D_0a88
	cmpw   $0x0,D_0a9c
	jne   L_01b6
	cmpb   $0x0,D_0cc0
	jne   L_01ae
	pushw  D_001a
	mov    $D_0124,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
L_01ae:
	mov    $0x1f6,%ax
	push   %ax
	call   L_01c6
	pop    %cx
L_01b6:
	/* TODO: indirect call - verify */
	call   *0x2a9c
	call   L_082f
	push   %ax
	call   L_07f2
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_01c6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_0a9c
	je   L_01ec
	pushw  D_001a
	mov    $D_0147,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
L_01ec:
	mov    0x4(%bp),%ax
	mov    %ax,D_0a9c
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_05ed
	call   L_065a
L_0201:
	call   L_0bf1
	or     %ax,%ax
	je   L_020b
	jmp   L_02a1
L_020b:
	call   L_0d69
	cmpw   $0x0,D_0a88
	je   L_021c
	call   L_0d03
	or     %ax,%ax
	je   L_0261
L_021c:
	call   L_0ada
	mov    %ax,%di
	or     %di,%di
	jge   L_0240
	cmpw   $0x0,D_0a88
	je   L_0261
	pushw  D_0a80
	mov    $D_0165,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	jmp   L_0285
L_0240:
	cmpb   $0x0,D_0cc0
	je   L_0263
	mov    D_0a3a,%ax
	mov    D_0a3c,%dx
	cmp    D_0a0e,%dx
	jg   L_0263
	jl   L_025c
	cmp    D_0a0c,%ax
	ja   L_0263
L_025c:
	push   %di
	call   L_330a
	pop    %cx
L_0261:
	jmp   L_0285
L_0263:
	mov    $0x72,%ax
	push   %ax
	call   L_0e0f
	pop    %cx
	mov    $0x3,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	pushw  D_0aa4
	call   L_0b15
	add    $0x6,%sp
	push   %di
	call   L_0a5c
	pop    %cx
L_0282:
	jmp   L_0201
L_0285:
	mov    $0x63,%ax
	push   %ax
	call   L_0e0f
	pop    %cx
	mov    $0xe,%ax
	push   %ax
	pushw  D_0a82
	pushw  D_0aa4
	call   L_0b15
	add    $0x6,%sp
	jmp   L_0282
L_02a1:
	call   L_0899
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_05ed
	call   L_065a
	or     %ax,%ax
	je   L_02ba
	call   L_07be
L_02ba:
	call   L_0bf1
	or     %ax,%ax
	jne   L_0300
	call   L_0d03
	or     %ax,%ax
	je   L_02e4
	mov    $0x64,%ax
	push   %ax
	call   L_0e0f
	pop    %cx
	mov    $0x3,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	pushw  D_0aa4
	call   L_0b15
	add    $0x6,%sp
	jmp   L_02ba
L_02e4:
	mov    $0x63,%ax
	push   %ax
	call   L_0e0f
	pop    %cx
	mov    $0xe,%ax
	push   %ax
	pushw  D_0a82
	pushw  D_0aa4
	call   L_0b15
	add    $0x6,%sp
	jmp   L_02ba
L_0300:
	call   L_08fb
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_065a
	or     %ax,%ax
	je   L_0316
	call   L_07be
L_0316:
	call   L_0bf1
	or     %ax,%ax
	je   L_0320
	jmp   L_03af
L_0320:
	cmpw   $0x0,D_0a88
	je   L_032e
	call   L_0d03
	or     %ax,%ax
	je   L_0357
L_032e:
	mov    D_0a14,%ax
	and    $0x1ff,%ax
	push   %ax
	pushw  D_0a80
	call   L_1470
	add    $0x4,%sp
	mov    %ax,%di
	or     %di,%di
	jge   L_0359
	pushw  D_0a80
	mov    $D_0179,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
L_0357:
	jmp   L_0377
L_0359:
	mov    $0x78,%ax
	push   %ax
	call   L_0e0f
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	push   %di
	pushw  D_0aa4
	call   L_0b15
	add    $0x6,%sp
	push   %di
	call   L_330a
	pop    %cx
	jmp   L_0316
L_0377:
	mov    $0x63,%ax
	push   %ax
	call   L_0e0f
	pop    %cx
	mov    $0x3,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	pushw  D_0aa4
	call   L_0b15
	add    $0x6,%sp
	cmpw   $0x0,D_0a88
	jg   L_039b
	jmp   L_0316
L_039b:
	call   L_0871
	or     %ax,%ax
	je   L_03a5
	jmp   L_0316
L_03a5:
	xor    %ax,%ax
	push   %ax
	call   L_07f2
	pop    %cx
	jmp   L_0316
L_03af:
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_065a
	or     %ax,%ax
	je   L_03c2
	call   L_07be
L_03c2:
	call   L_0bf1
	or     %ax,%ax
	jne   L_041c
	cmpw   $0x0,D_0a88
	je   L_03d7
	call   L_0d03
	or     %ax,%ax
	je   L_0408
L_03d7:
	cmpb   $0x0,D_0cc1
	je   L_03f4
	pushw  D_0a80
	mov    $D_018f,%ax
	push   %ax
	call   L_14f6
	add    $0x4,%sp
	mov    $D_0952,%ax
	push   %ax
	call   L_2f8e
	pop    %cx
L_03f4:
	mov    $0x2,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	pushw  D_0aa4
	call   L_0b15
	add    $0x6,%sp
	jmp   L_03c2
L_0408:
	mov    $0x3,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	pushw  D_0aa4
	call   L_0b15
	add    $0x6,%sp
	jmp   L_03c2
L_041c:
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_05ed
	call   L_065a
	or     %ax,%ax
	je   L_0432
	call   L_07be
L_0432:
	pushw  D_002a
	call   L_13da
	pop    %cx
	mov    %ax,D_0a84
	mov    $0x180,%ax
	push   %ax
	pushw  D_0a84
	call   L_1470
	add    $0x4,%sp
	push   %ax
	call   L_330a
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	pushw  D_0a84
	call   L_335c
	add    $0x4,%sp
	mov    %ax,D_0cc8
	cmp    $0x0,%ax
	jge   L_047c
	mov    $D_0197,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
L_047c:
	call   L_0bf1
	or     %ax,%ax
	jne   L_04c5
	call   L_0d69
	call   L_0d03
	or     %ax,%ax
	je   L_04a9
	mov    $0x6d,%ax
	push   %ax
	call   L_0e0f
	pop    %cx
	mov    $0xe,%ax
	push   %ax
	pushw  D_0cc8
	pushw  D_0aa4
	call   L_0b15
	add    $0x6,%sp
	jmp   L_047c
L_04a9:
	mov    $0x63,%ax
	push   %ax
	call   L_0e0f
	pop    %cx
	mov    $0xe,%ax
	push   %ax
	pushw  D_0a82
	pushw  D_0aa4
	call   L_0b15
	add    $0x6,%sp
	jmp   L_047c
L_04c5:
	call   L_08fb
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_065a
	or     %ax,%ax
	je   L_04db
	call   L_07be
L_04db:
	call   L_0bf1
	or     %ax,%ax
	jne   L_0521
	cmpw   $0x0,D_0a88
	je   L_04f0
	call   L_0d03
	or     %ax,%ax
	je   L_050d
L_04f0:
	cmpb   $0x0,D_0cc1
	je   L_04fa
	call   L_0e7a
L_04fa:
	pushw  D_0a80
	call   L_0e3d
	pop    %cx
	push   %ax
	mov    $D_01b5,%ax
	push   %ax
	call   L_14f6
	add    $0x4,%sp
L_050d:
	mov    $0x3,%ax
	push   %ax
	mov    $0xffff,%ax
	push   %ax
	pushw  D_0aa4
	call   L_0b15
	add    $0x6,%sp
	jmp   L_04db
L_0521:
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpb   $0x0,D_0cac
	jne   L_0538
	cmpb   $0x0,D_0cad
	je   L_054e
L_0538:
	mov    $D_01b9,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
L_054e:
	call   L_06cc
	xor    %di,%di
L_0553:
	mov    %di,%bx
	shl    %bx
	cmpw   $0x0,D_001e(%bx)
	je   L_0573
	mov    $0x1,%ax
	push   %ax
	mov    %di,%bx
	shl    %bx
	pushw  D_001e(%bx)
	call   L_2046
	add    $0x4,%sp
	inc    %di
	jmp   L_0553
L_0573:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  D_0a1a
	call   L_2166
	add    $0x8,%sp
	xor    %di,%di
L_0589:
	cmp    D_0a88,%di
	jge   L_05e9
	mov    %di,%ax
	shl    %ax
	add    D_0aa0,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,D_0a80
	or     %ax,%ax
	je   L_05e6
	mov    %di,%ax
	shl    %ax
	add    D_0aa0,%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	mov    $0x71,%ax
	push   %ax
	call   L_0e0f
	pop    %cx
	call   L_0ada
	mov    %ax,%si
	or     %si,%si
	jge   L_05d5
	pushw  D_0a80
	mov    $D_01d5,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	jmp   L_05e6
L_05d5:
	mov    D_0a1a,%ax
	mov    %ax,D_0a82
	push   %si
	call   L_0a5c
	pop    %cx
	mov    D_0a82,%ax
	mov    %ax,D_0a1a
L_05e6:
	inc    %di
	jmp   L_0589
L_05e9:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_05ed:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  D_0026
	call   L_13da
	pop    %cx
	mov    %ax,D_0aa2
	mov    $0x180,%ax
	push   %ax
	pushw  D_0aa2
	call   L_1470
	add    $0x4,%sp
	push   %ax
	call   L_330a
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	pushw  D_0aa2
	call   L_335c
	add    $0x4,%sp
	mov    %ax,D_0a82
	cmp    $0x0,%ax
	jge   L_063c
	mov    $D_01e9,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
L_063c:
	mov    $0x8,%ax
	push   %ax
	mov    $D_0206,%ax
	push   %ax
	pushw  D_0a82
	call   L_343a
	add    $0x6,%sp
	cmp    $0x8,%ax
	je   L_0656
	call   L_0f2d
L_0656:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_065a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	xor    %ax,%ax
	push   %ax
	pushw  D_0a1c
	call   L_335c
	add    $0x4,%sp
	mov    %ax,D_0aa4
	cmp    $0x0,%ax
	jge   L_067c
	mov    $0x1,%ax
	jmp   L_06c5
L_067c:
	mov    $0x8,%ax
	push   %ax
	lea    -0xc(%bp),%ax
	push   %ax
	pushw  D_0aa4
	call   L_3372
	add    $0x6,%sp
	cmp    $0x8,%ax
	jne   L_06a9
	mov    $0x8,%ax
	push   %ax
	mov    $D_020f,%ax
	push   %ax
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_1f9e
	add    $0x6,%sp
	or     %ax,%ax
	je   L_06c3
L_06a9:
	pushw  D_0a1c
	mov    $D_0218,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
L_06c3:
	xor    %ax,%ax
L_06c5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_06cc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    $0x2,%ax
	push   %ax
	pushw  D_0a1c
	call   L_335c
	add    $0x4,%sp
	mov    %ax,D_0a1a
	or     %ax,%ax
	jge   L_074d
	cmpb   $0x0,D_0cae
	jne   L_0702
	pushw  D_0a1c
	mov    $D_0236,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
L_0702:
	mov    $0x1b6,%ax
	push   %ax
	pushw  D_0a1c
	call   L_1470
	add    $0x4,%sp
	mov    %ax,D_0a1a
	or     %ax,%ax
	jge   L_0731
	pushw  D_0a1c
	mov    $D_0247,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
L_0731:
	mov    $0x8,%ax
	push   %ax
	mov    $D_025d,%ax
	push   %ax
	pushw  D_0a1a
	call   L_343a
	add    $0x6,%sp
	cmp    $0x8,%ax
	je   L_0778
	call   L_0f2d
	jmp   L_0794
L_074d:
	mov    $0x8,%ax
	push   %ax
	lea    -0xc(%bp),%ax
	push   %ax
	pushw  D_0a1a
	call   L_3372
	add    $0x6,%sp
	cmp    $0x8,%ax
	jne   L_077a
	mov    $0x8,%ax
	push   %ax
	mov    $D_0266,%ax
	push   %ax
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_1f9e
	add    $0x6,%sp
	or     %ax,%ax
L_0778:
	je   L_0794
L_077a:
	pushw  D_0a1c
	mov    $D_026f,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
L_0794:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_079b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  D_001a
	pushw  D_001c
	mov    $D_028d,%ax
	push   %ax
	call   L_14f6
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_07be:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  D_0a1c
	mov    $D_02b3,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x64,%ax
	push   %ax
	call   L_07f2
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_07f2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_0aa2
	je   L_0806
	pushw  D_0aa2
	call   L_3426
	pop    %cx
L_0806:
	cmpw   $0x0,D_0a9e
	je   L_0815
	pushw  D_0a9e
	call   L_3426
	pop    %cx
L_0815:
	cmpw   $0x0,D_0a84
	je   L_0824
	pushw  D_0a84
	call   L_3426
	pop    %cx
L_0824:
	pushw  0x4(%bp)
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_082f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %si,%si
	mov    %si,%di
L_0838:
	cmp    D_0a88,%di
	jge   L_086b
	mov    %di,%ax
	shl    %ax
	add    D_0aa0,%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	je   L_0868
	mov    %di,%ax
	shl    %ax
	add    D_0aa0,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    $D_02ca,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	inc    %si
L_0868:
	inc    %di
	jmp   L_0838
L_086b:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0871:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %si,%si
	mov    %si,%di
L_087a:
	cmp    D_0a88,%di
	jge   L_0893
	mov    %di,%ax
	shl    %ax
	add    D_0aa0,%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	je   L_0890
	inc    %si
L_0890:
	inc    %di
	jmp   L_087a
L_0893:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0899:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %di,%di
L_08a0:
	cmp    D_0a88,%di
	jge   L_08f4
	mov    %di,%ax
	shl    %ax
	add    D_0aa0,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,D_0a80
	or     %ax,%ax
	je   L_08f1
	mov    %di,%ax
	shl    %ax
	add    D_0aa0,%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	mov    $0x61,%ax
	push   %ax
	call   L_0e0f
	pop    %cx
	call   L_0ada
	mov    %ax,%si
	or     %si,%si
	jge   L_08ec
	pushw  D_0a80
	mov    $D_02dc,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	jmp   L_08f1
L_08ec:
	push   %si
	call   L_0a5c
	pop    %cx
L_08f1:
	inc    %di
	jmp   L_08a0
L_08f4:
	call   L_08fb
	pop    %di
	pop    %si
	pop    %bp
	ret


L_08fb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %di,%di
L_0902:
	mov    %di,%bx
	shl    %bx
	cmpw   $0x0,D_001e(%bx)
	je   L_0922
	mov    $0x1,%ax
	push   %ax
	mov    %di,%bx
	shl    %bx
	pushw  D_001e(%bx)
	call   L_2046
	add    $0x4,%sp
	inc    %di
	jmp   L_0902
L_0922:
	cmpw   $0x0,D_0aa4
	jge   L_0942
	cmpb   $0x0,D_0cae
	jne   L_0942
	pushw  D_0a1c
	mov    $D_02f0,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
L_0942:
	pushw  D_0aa4
	call   L_330a
	pop    %cx
	mov    $0x1b6,%ax
	push   %ax
	pushw  D_0a1c
	call   L_1470
	add    $0x4,%sp
	mov    %ax,D_0aa4
	cmp    $0x0,%ax
	jge   L_097a
	pushw  D_0a1c
	mov    $D_0301,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
L_097a:
	cmpw   $0x0,D_0aa2
	je   L_09c4
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  D_0a82
	call   L_2166
	add    $0x8,%sp
L_0994:
	mov    $0x200,%ax
	push   %ax
	mov    $D_0aac,%ax
	push   %ax
	pushw  D_0a82
	call   L_3372
	add    $0x6,%sp
	mov    %ax,%di
	or     %ax,%ax
	jle   L_09c4
	push   %di
	mov    $D_0aac,%ax
	push   %ax
	pushw  D_0aa4
	call   L_343a
	add    $0x6,%sp
	cmp    %di,%ax
	je   L_0994
	call   L_0f2d
	jmp   L_0994
L_09c4:
	cmpw   $0x0,D_0a84
	je   L_0a0e
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  D_0cc8
	call   L_2166
	add    $0x8,%sp
L_09de:
	mov    $0x200,%ax
	push   %ax
	mov    $D_0aac,%ax
	push   %ax
	pushw  D_0cc8
	call   L_3372
	add    $0x6,%sp
	mov    %ax,%di
	or     %ax,%ax
	jle   L_0a0e
	push   %di
	mov    $D_0aac,%ax
	push   %ax
	pushw  D_0aa4
	call   L_343a
	add    $0x6,%sp
	cmp    %di,%ax
	je   L_09de
	call   L_0f2d
	jmp   L_09de
L_0a0e:
	cmpw   $0x0,D_0a9e
	je   L_0a58
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  D_0cc6
	call   L_2166
	add    $0x8,%sp
L_0a28:
	mov    $0x200,%ax
	push   %ax
	mov    $D_0aac,%ax
	push   %ax
	pushw  D_0cc6
	call   L_3372
	add    $0x6,%sp
	mov    %ax,%di
	or     %ax,%ax
	jle   L_0a58
	push   %di
	mov    $D_0aac,%ax
	push   %ax
	pushw  D_0aa4
	call   L_343a
	add    $0x6,%sp
	cmp    %di,%ax
	je   L_0a28
	call   L_0f2d
	jmp   L_0a28
L_0a58:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a5c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x3e,%sp
	mov    $D_0339,%ax
	push   %ax
	pushw  D_0a34
	pushw  D_0a32
	pushw  D_0a28
	pushw  D_0a2e
	pushw  D_0a2c
	pushw  D_0a3c
	pushw  D_0a3a
	pushw  D_0a80
	call   L_0e3d
	pop    %cx
	push   %ax
	mov    $D_0317,%ax
	push   %ax
	lea    -0x41(%bp),%ax
	push   %ax
	call   L_2184
	add    $0x16,%sp
	mov    $0x3c,%ax
	push   %ax
	lea    -0x41(%bp),%ax
	push   %ax
	mov    $D_0a44,%ax
	push   %ax
	call   L_1fee
	add    $0x6,%sp
	mov    D_0a32,%ax
	mov    D_0a34,%dx
	mov    %ax,D_0a16
	mov    %dx,D_0a18
	mov    $0xc,%ax
	push   %ax
	pushw  D_0a82
	pushw  0x4(%bp)
	call   L_0b15
	add    $0x6,%sp
	pushw  0x4(%bp)
	call   L_330a
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ada:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %ax,%ax
	push   %ax
	pushw  D_0a80
	call   L_335c
	add    $0x4,%sp
	mov    %ax,%di
	or     %di,%di
	jge   L_0af6
	mov    %di,%ax
	jmp   L_0b11
L_0af6:
	mov    $D_0a24,%ax
	push   %ax
	push   %di
	call   L_331e
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_0b0f
	push   %di
	call   L_330a
	pop    %cx
	mov    $0xffff,%ax
	jmp   L_0b11
L_0b0f:
	mov    %di,%ax
L_0b11:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0b15:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	testw  $0x8,0x8(%bp)
	je   L_0b58
	mov    $0xf,%di
L_0b25:
	or     %di,%di
	jl   L_0b3f
	cmpb   $0x20,D_0a44(%di)
	je   L_0b3c
	cmpb   $0x0,D_0a44(%di)
	jne   L_0b3f
	movb   $0x20,D_0a44(%di)
L_0b3c:
	dec    %di
	jmp   L_0b25
L_0b3f:
	mov    $0x3c,%ax
	push   %ax
	mov    $D_0a44,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_343a
	add    $0x6,%sp
	cmp    $0x3c,%ax
	je   L_0b58
	call   L_0f2d
L_0b58:
	movw   $0x0,-0x6(%bp)
L_0b5d:
	cmpw   $0x0,D_0a18
	jl   L_0be3
	jg   L_0b6d
	cmpw   $0x0,D_0a16
	jbe   L_0be3
L_0b6d:
	mov    $0x200,%si
	mov    %si,%di
	mov    %di,%ax
	cwtd
	cmp    %dx,D_0a18
	jg   L_0ba4
	jl   L_0b83
	cmp    %ax,D_0a16
	jae   L_0ba4
L_0b83:
	mov    D_0a16,%si
	mov    %si,%di
	test   $0x1,%di
	je   L_0ba4
	movb   $0xa,D_0aac(%di)
	testw  $0x2,0x8(%bp)
	je   L_0b9c
	inc    %di
L_0b9c:
	testw  $0x4,0x8(%bp)
	je   L_0ba4
	inc    %si
L_0ba4:
	push   %di
	mov    $D_0aac,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_3372
	add    $0x6,%sp
	cmp    %di,%ax
	je   L_0bb9
	incw   -0x6(%bp)
L_0bb9:
	testw  $0x1,0x8(%bp)
	jne   L_0bd5
	push   %si
	mov    $D_0aac,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_343a
	add    $0x6,%sp
	cmp    %si,%ax
	je   L_0bd5
	call   L_0f2d
L_0bd5:
	subw   $0x200,D_0a16
	sbbw   $0x0,D_0a18
	jmp   L_0b5d
L_0be3:
	cmpw   $0x0,-0x6(%bp)
	je   L_0bec
	call   L_0df4
L_0bec:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0bf1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x3c,%ax
	push   %ax
	mov    $D_0a44,%ax
	push   %ax
	pushw  D_0aa4
	call   L_3372
	add    $0x6,%sp
	mov    %ax,%si
	cmp    $0x3c,%si
	je   L_0c2a
	cmpw   $0x0,D_0a9e
	je   L_0c24
	mov    D_0a82,%si
	mov    D_0cc6,%ax
	mov    %ax,D_0a82
	mov    %si,D_0cc6
L_0c24:
	mov    $0x1,%ax
	jmp   L_0cff
L_0c2a:
	mov    $0x2,%ax
	push   %ax
	mov    $D_033c,%ax
	push   %ax
	mov    $D_0a7e,%ax
	push   %ax
	call   L_1f9e
	add    $0x6,%sp
	or     %ax,%ax
	je   L_0c6c
	mov    $0x1,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  D_0aa4
	call   L_2166
	add    $0x8,%sp
	push   %dx
	push   %ax
	mov    $D_033f,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x8,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
L_0c6c:
	mov    $D_0a54,%di
L_0c6f:
	dec    %di
	cmpb   $0x20,(%di)
	je   L_0c6f
	inc    %di
	movb   $0x0,(%di)
	mov    $0x10,%ax
	push   %ax
	mov    $D_0a44,%ax
	push   %ax
	mov    $D_0a8c,%ax
	push   %ax
	call   L_1fee
	add    $0x6,%sp
	/* dual-reloc split: movw $D_0a8c,D_0a80 */
	mov	$D_0a8c,%ax
	mov	%ax,D_0a80
	mov    $0x10,%ax
	push   %ax
	mov    $D_0a8c,%ax
	push   %ax
	mov    $D_09fc,%ax
	push   %ax
	call   L_1fee
	add    $0x6,%sp
	mov    $D_0a0c,%ax
	push   %ax
	mov    $D_035f,%ax
	push   %ax
	mov    $D_0a54,%ax
	push   %ax
	call   L_1553
	add    $0x6,%sp
	mov    $D_0a10,%ax
	push   %ax
	mov    $D_0363,%ax
	push   %ax
	mov    $D_0a60,%ax
	push   %ax
	call   L_1553
	add    $0x6,%sp
	mov    $D_0a12,%ax
	push   %ax
	mov    $D_0367,%ax
	push   %ax
	mov    $D_0a66,%ax
	push   %ax
	call   L_1553
	add    $0x6,%sp
	mov    $D_0a14,%ax
	push   %ax
	mov    $D_036b,%ax
	push   %ax
	mov    $D_0a6c,%ax
	push   %ax
	call   L_1553
	add    $0x6,%sp
	mov    $D_0a16,%ax
	push   %ax
	mov    $D_036f,%ax
	push   %ax
	mov    $D_0a74,%ax
	push   %ax
	call   L_1553
	add    $0x6,%sp
	xor    %ax,%ax
L_0cff:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0d03:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %di,%di
L_0d0a:
	cmp    D_0a88,%di
	jge   L_0d63
	mov    %di,%ax
	shl    %ax
	add    D_0aa0,%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	je   L_0d60
	pushw  D_0a80
	mov    %di,%ax
	shl    %ax
	add    D_0aa0,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_0e3d
	pop    %cx
	push   %ax
	call   L_32c8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0d60
	mov    %di,%ax
	shl    %ax
	add    D_0aa0,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,D_0a80
	mov    %di,%ax
	shl    %ax
	add    D_0aa0,%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	mov    $0x1,%ax
	jmp   L_0d65
L_0d60:
	inc    %di
	jmp   L_0d0a
L_0d63:
	xor    %ax,%ax
L_0d65:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0d69:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_0cca,%ax
	jmp   L_0de6
L_0d73:
	pushw  D_0a86
	pushw  D_0a80
	call   L_32c8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0dd8
	movw   $0x2,D_0cca
	cmpb   $0x0,D_0cac
	jne   L_0dd8
L_0d92:
	movw   $0x0,D_0cca
	pushw  D_0028
	call   L_13da
	pop    %cx
	mov    %ax,D_0a9e
	mov    $0x180,%ax
	push   %ax
	pushw  D_0a9e
	call   L_1470
	add    $0x4,%sp
	push   %ax
	call   L_330a
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	pushw  D_0a9e
	call   L_335c
	add    $0x4,%sp
	mov    %ax,%di
	or     %di,%di
	jge   L_0dda
	mov    $D_0373,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x4,%sp
L_0dd8:
	jmp   L_0df0
L_0dda:
	mov    D_0a82,%ax
	mov    %ax,D_0cc6
	mov    %di,D_0a82
	jmp   L_0df0
L_0de6:
	cmp    $0x1,%ax
	je   L_0d73
	cmp    $0x2,%ax
	je   L_0d92
L_0df0:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0df4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  D_0a80
	mov    $D_0392,%ax
	push   %ax
	mov    $D_095a,%ax
	push   %ax
	call   L_13ae
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e0f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpb   $0x0,D_0cc1
	je   L_0e39
	cmpw   $0x63,0x4(%bp)
	jne   L_0e28
	cmpb   $0x1,D_0cc1
	jle   L_0e39
L_0e28:
	pushw  D_0a80
	pushw  0x4(%bp)
	mov    $D_03a9,%ax
	push   %ax
	call   L_14f6
	add    $0x6,%sp
L_0e39:
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
L_0e45:
	cmpb   $0x0,(%di)
	je   L_0e4d
	inc    %di
	jmp   L_0e45
L_0e4d:
	cmp    0x4(%bp),%di
	jbe   L_0e5d
	dec    %di
	cmpb   $0x2f,(%di)
	jne   L_0e5d
	movb   $0x0,(%di)
	jmp   L_0e4d
L_0e5d:
	mov    0x4(%bp),%si
	mov    %si,%di
L_0e62:
	cmpb   $0x0,(%di)
	je   L_0e74
	cmpb   $0x2f,(%di)
	jne   L_0e71
	lea    0x1(%di),%ax
	mov    %ax,%si
L_0e71:
	inc    %di
	jmp   L_0e62
L_0e74:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e7a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_0ec6
	pushw  D_0a12
	pushw  D_0a10
	mov    $D_03b2,%ax
	push   %ax
	call   L_14f6
	add    $0x6,%sp
	pushw  D_0a18
	pushw  D_0a16
	mov    $D_03ba,%ax
	push   %ax
	call   L_14f6
	add    $0x6,%sp
	mov    $D_0a0c,%ax
	push   %ax
	call   L_0f46
	pop    %cx
	mov    %ax,%di
	lea    0x14(%di),%ax
	push   %ax
	lea    0x4(%di),%ax
	push   %ax
	mov    $D_03bf,%ax
	push   %ax
	call   L_14f6
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ec6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_00b0,%di
L_0ece:
	cmp    $D_00c2,%di
	jae   L_0ee0
	mov    %di,%bx
	inc    %di
	inc    %di
	pushw  (%bx)
	call   L_0ee4
	pop    %cx
	jmp   L_0ece
L_0ee0:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ee4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_0ef2:
	dec    %di
	jl   L_0f04
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    D_0a14,%ax
	test   %ax,(%bx)
	jne   L_0f04
	inc    %si
	inc    %si
	jmp   L_0ef2
L_0f04:
	decw   D_0954
	jl   L_0f1d
	mov    D_0952,%bx
	incw   D_0952
	mov    (%si),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_0f29
L_0f1d:
	mov    $D_0952,%ax
	push   %ax
	pushw  (%si)
	call   L_2ea0
	add    $0x4,%sp
L_0f29:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f2d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_03d1,%ax
	push   %ax
	call   L_1486
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	call   L_07f2
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f46:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_0f5b
	pop    %cx
	push   %ax
	call   L_1219
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f5b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x16,%sp
	lea    -0x16(%bp),%ax
	push   %ax
	call   L_139a
	pop    %cx
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    -0x10(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	call   L_27cb
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
	call   L_1079
	pop    %cx
	mov    %ax,%si
	mov    0xe(%si),%di
	movw   $0x77,-0x6(%bp)
	movw   $L_012f,-0x8(%bp)
	cmpw   $0x4a,0xa(%si)
	je   L_0fbb
	cmpw   $0x4b,0xa(%si)
	jne   L_0fe3
L_0fbb:
	mov    0xa(%si),%ax
	sub    $0x4a,%ax
	mov    %ax,%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	mov    D_0412(%bx),%ax
	mov    %ax,-0x6(%bp)
	mov    0xa(%si),%ax
	sub    $0x4a,%ax
	mov    %ax,%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	mov    D_0414(%bx),%ax
	mov    %ax,-0x8(%bp)
L_0fe3:
	pushw  -0x6(%bp)
	push   %si
	call   L_1042
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	pushw  -0x8(%bp)
	push   %si
	call   L_1042
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	cmpw   $0x0,-0xe(%bp)
	je   L_1039
	cmp    -0x6(%bp),%di
	jg   L_1013
	cmp    -0x6(%bp),%di
	jne   L_1039
	cmpw   $0x2,0x4(%si)
	jl   L_1039
L_1013:
	cmp    -0x8(%bp),%di
	jl   L_1023
	cmp    -0x8(%bp),%di
	jne   L_1039
	cmpw   $0x1,0x4(%si)
	jge   L_1039
L_1023:
	addw   $0xe10,-0xc(%bp)
	adcw   $0x0,-0xa(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_1079
	pop    %cx
	mov    %ax,%si
	incw   0x10(%si)
L_1039:
	mov    %si,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1042:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	cmp    $0x3a,%si
	jl   L_1060
	pushw  0xa(%di)
	call   L_1331
	pop    %cx
	sub    $0x16d,%ax
	add    %ax,%si
L_1060:
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


L_1079:
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
	call   L_28c5
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
	call   L_2874
	add    $0x8,%sp
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
	cmpw   $0x0,-0xa(%bp)
	jg   L_10d8
	jl   L_10c7
	cmpw   $0x0,-0xc(%bp)
	jae   L_10d8
L_10c7:
	addw   $0x5180,-0xc(%bp)
	adcw   $0x1,-0xa(%bp)
	subw   $0x1,-0x10(%bp)
	sbbw   $0x0,-0xe(%bp)
L_10d8:
	mov    $D_041a,%di
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0xa(%bp)
	pushw  -0xc(%bp)
	call   L_28c5
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
	call   L_2874
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
	call   L_28c5
	add    $0x8,%sp
	mov    %ax,D_0426
	cmpw   $0x0,-0xe(%bp)
	jl   L_1183
	jg   L_1157
	cmpw   $0x0,-0x10(%bp)
	jb   L_1183
L_1157:
	movw   $0x46,-0x8(%bp)
L_115c:
	pushw  -0x8(%bp)
	call   L_1331
	pop    %cx
	cwtd
	cmp    %dx,-0xe(%bp)
	jl   L_11ab
	jg   L_1170
	cmp    %ax,-0x10(%bp)
	jb   L_11ab
L_1170:
	pushw  -0x8(%bp)
	call   L_1331
	pop    %cx
	cwtd
	sub    %ax,-0x10(%bp)
	sbb    %dx,-0xe(%bp)
	incw   -0x8(%bp)
	jmp   L_115c
L_1183:
	movw   $0x46,-0x8(%bp)
L_1188:
	cmpw   $0x0,-0xe(%bp)
	jg   L_11ab
	jl   L_1196
	cmpw   $0x0,-0x10(%bp)
	jae   L_11ab
L_1196:
	mov    -0x8(%bp),%ax
	dec    %ax
	push   %ax
	call   L_1331
	pop    %cx
	cwtd
	add    %ax,-0x10(%bp)
	adc    %dx,-0xe(%bp)
	decw   -0x8(%bp)
	jmp   L_1188
L_11ab:
	mov    -0x8(%bp),%ax
	mov    %ax,D_0424
	mov    -0x10(%bp),%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,D_0428
	pushw  -0x8(%bp)
	call   L_1331
	pop    %cx
	cmp    $0x16e,%ax
	jne   L_11cc
	movw   $0x1d,D_03fc
L_11cc:
	movw   $0x0,-0x8(%bp)
L_11d1:
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    -0x6(%bp),%ax
	cmp    D_03fa(%bx),%ax
	jl   L_11f0
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    D_03fa(%bx),%ax
	sub    %ax,-0x6(%bp)
	incw   -0x8(%bp)
	jmp   L_11d1
L_11f0:
	movw   $0x1c,D_03fc
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
	movw   $0x0,D_042a
	mov    $D_041a,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1219:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    $D_03e0,%di
	mov    $D_042c,%si
L_1225:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_1225
	mov    0x4(%bp),%bx
	mov    0xc(%bx),%ax
	mov    $0x3,%dx
	imul   %dx
	add    $D_0446,%ax
	mov    %ax,%si
	mov    $D_03e0,%di
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
	add    $D_045c,%ax
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
	call   L_134f
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_134f
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_134f
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_134f
	add    $0x4,%sp
	mov    %ax,%di
	mov    0x4(%bp),%bx
	cmpw   $0x64,0xa(%bx)
	jl   L_1314
	movb   $0x32,0x1(%di)
	movb   $0x30,0x2(%di)
L_1314:
	inc    %di
	inc    %di
	mov    0x4(%bp),%bx
	mov    0xa(%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   L_134f
	add    $0x4,%sp
	mov    %ax,%di
	mov    $D_03e0,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1331:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	cwtd
	mov    $0x4,%cx
	idiv   %cx
	or     %dx,%dx
	jne   L_1348
	mov    $0x16e,%ax
	jmp   L_134b
L_1348:
	mov    $0x16d,%ax
L_134b:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_134f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	inc    %di
	cmpw   $0xa,0x6(%bp)
	jl   L_137a
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
	jmp   L_1380
L_137a:
	mov    %di,%bx
	inc    %di
	movb   $0x20,(%bx)
L_1380:
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


L_139a:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall ftime(35) */
	mov    0x4(%bp),%ax
	/* TODO: Venix-only ftime(35): no TLVC equivalent */
	xor	%ax,%ax	/* stub */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_13ab
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_13ab:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_13ae:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_21d6
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_13d4
	mov    $0xffff,%ax
	jmp   L_13d6
L_13d4:
	xor    %ax,%ax
L_13d6:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_13da:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	call   L_1460
	mov    %ax,%si
	mov    0x4(%bp),%di
L_13e8:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_13e8
	dec    %di
L_13f1:
	dec    %di
	cmpb   $0x58,(%di)
	jne   L_1414
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
	jmp   L_13f1
L_1414:
	inc    %di
	movw   $0x61,-0x6(%bp)
L_141a:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_144a
	add    $0x4,%sp
	cmp    $0xffff,%ax
	je   L_1442
	cmpw   $0x7a,-0x6(%bp)
	jne   L_1436
	mov    $D_0482,%ax
	jmp   L_1445
L_1436:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%di)
	incw   -0x6(%bp)
	jmp   L_141a
L_1442:
	mov    0x4(%bp),%ax
L_1445:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_144a:
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
	jns    L_145e
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_145e:
	pop    %bp
	ret


L_1460:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getpid(20) */
	/* getpid -> __NR_getpid */
	mov	$__NR_getpid,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_146e
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_146e:
	pop    %bp
	ret


L_1470:
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
	jns    L_1484
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1484:
	pop    %bp
	ret


L_1486:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0484,%di
	mov    D_0ccc,%ax
	cmp    D_04de,%ax
	jge   L_14a1
	mov    D_0ccc,%bx
	shl    %bx
	mov    D_0498(%bx),%di
L_14a1:
	pushw  0x4(%bp)
	call   L_1f82
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_14ce
	push   %si
	pushw  0x4(%bp)
	mov    $0x2,%ax
	push   %ax
	call   L_343a
	add    $0x6,%sp
	mov    $0x2,%ax
	push   %ax
	mov    $D_0492,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_343a
	add    $0x6,%sp
L_14ce:
	push   %di
	call   L_1f82
	pop    %cx
	push   %ax
	push   %di
	mov    $0x2,%ax
	push   %ax
	call   L_343a
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $D_0495,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_343a
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_14f6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0952,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_21d6
	add    $0x6,%sp
	mov    D_0958,%al
	cbtw
	test   $0x20,%ax
	je   L_151a
	mov    $0xffff,%ax
	jmp   L_151c
L_151a:
	xor    %ax,%ax
L_151c:
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
	mov    $D_094a,%ax
	push   %ax
	call   L_1594
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_1594
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1553:
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
L_156e:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_157b
	incw   -0xa(%bp)
	jmp   L_156e
L_157b:
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_1594
	add    $0x6,%sp
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1594:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xe,%sp
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x10(%bp)
L_15ae:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	jmp   L_1758
L_15ba:
	mov    -0x8(%bp),%ax
	jmp   L_1688
L_15c0:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	cmp    $0x25,%ax
	jne   L_15d1
	jmp   L_1776
L_15d1:
	movw   $0x0,-0xe(%bp)
	cmpw   $0x2a,-0x6(%bp)
	je   L_15e5
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xe(%bp)
	jmp   L_15ee
L_15e5:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
L_15ee:
	movw   $0x0,-0xa(%bp)
	movw   $0x1,-0x12(%bp)
L_15f8:
	cmpw   $0x30,-0x6(%bp)
	jl   L_1620
	cmpw   $0x39,-0x6(%bp)
	jg   L_1620
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
	jmp   L_15f8
L_1620:
	cmpw   $0x0,-0xa(%bp)
	jne   L_162b
	movw   $0x7530,-0xa(%bp)
L_162b:
	cmpw   $0x6c,-0x6(%bp)
	jne   L_1641
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	movw   $0x2,-0x12(%bp)
	jmp   L_1664
L_1641:
	cmpw   $0x68,-0x6(%bp)
	jne   L_1657
	movw   $0x0,-0x12(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	jmp   L_1664
L_1657:
	cmpw   $0x5b,-0x6(%bp)
	jne   L_1664
	push   %di
	call   L_1c2c
	pop    %cx
	mov    %ax,%di
L_1664:
	mov    -0x6(%bp),%bx
	mov    D_0805(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_167f
	mov    -0x6(%bp),%ax
	add    $0x20,%ax
	mov    %ax,-0x6(%bp)
	movw   $0x2,-0x12(%bp)
L_167f:
	cmpw   $0x0,-0x6(%bp)
	jne   L_168a
	mov    $0xffff,%ax
L_1688:
	jmp   L_16c7
L_168a:
	lea    -0x10(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	pushw  -0x12(%bp)
	pushw  -0xa(%bp)
	pushw  -0x6(%bp)
	pushw  -0xe(%bp)
	call   L_177f
	add    $0xc,%sp
	or     %ax,%ax
	je   L_16b0
	cmpw   $0x0,-0xe(%bp)
	je   L_16b0
	incw   -0x8(%bp)
L_16b0:
	cmpw   $0x0,-0x10(%bp)
L_16b4:
	jne   L_16b9
	jmp   L_15ae
L_16b9:
	cmpw   $0x0,-0x8(%bp)
	je   L_16c4
	mov    -0x8(%bp),%ax
	jmp   L_16c7
L_16c4:
	mov    $0xffff,%ax
L_16c7:
	jmp   L_1745
L_16c9:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_16e3
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_16ea
L_16e3:
	pushw  0x4(%bp)
	call   L_1ed4
	pop    %cx
L_16ea:
	mov    %ax,-0xc(%bp)
	cmp    $0x20,%ax
	je   L_16c9
	cmpw   $0x9,-0xc(%bp)
	je   L_16c9
	cmpw   $0xa,-0xc(%bp)
L_16fc:
	je   L_16c9
	cmpw   $0xffff,-0xc(%bp)
L_1702:
	je   L_16b4
	pushw  0x4(%bp)
	pushw  -0xc(%bp)
	call   L_33d8
	add    $0x4,%sp
	jmp   L_15ae
L_1713:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_172d
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_1734
L_172d:
	pushw  0x4(%bp)
	call   L_1ed4
	pop    %cx
L_1734:
	mov    %ax,-0xc(%bp)
	cmp    -0x6(%bp),%ax
	je   L_1702
	cmpw   $0xffff,-0xc(%bp)
	jne   L_1747
	mov    $0xffff,%ax
L_1745:
	jmp   L_1778
L_1747:
	pushw  0x4(%bp)
	pushw  -0xc(%bp)
	call   L_33d8
	add    $0x4,%sp
	mov    -0x8(%bp),%ax
	jmp   L_1778
L_1758:
	or     %ax,%ax
	jne   L_175f
	jmp   L_15ba
L_175f:
	cmp    $0x9,%ax
L_1762:
	je   L_16fc
	cmp    $0xa,%ax
	je   L_1762
	cmp    $0x20,%ax
	je   L_1762
	cmp    $0x25,%ax
	jne   L_1776
	jmp   L_15c0
L_1776:
	jmp   L_1713
L_1778:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_177f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x63,0x6(%bp)
	je   L_1799
	cmpw   $0x73,0x6(%bp)
	je   L_1799
	cmpw   $0x5b,0x6(%bp)
	jne   L_17be
L_1799:
	pushw  0xe(%bp)
	pushw  0xc(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	cmpw   $0x0,0x4(%bp)
	je   L_17b2
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	jmp   L_17b4
L_17b2:
	xor    %ax,%ax
L_17b4:
	push   %ax
	call   L_1b1e
	add    $0xa,%sp
	jmp   L_1a8c
L_17be:
	movw   $0x0,-0x58(%bp)
	movw   $0x0,-0x56(%bp)
	movw   $0x0,-0x52(%bp)
	movw   $0x0,-0x4c(%bp)
	cmpw   $0x65,0x6(%bp)
	je   L_17de
	cmpw   $0x66,0x6(%bp)
	jne   L_17e3
L_17de:
	movw   $0x1,-0x4c(%bp)
L_17e3:
	movw   $0xa,-0x48(%bp)
	cmpw   $0x6f,0x6(%bp)
	jne   L_17f5
	movw   $0x8,-0x48(%bp)
	jmp   L_1800
L_17f5:
	cmpw   $0x78,0x6(%bp)
	jne   L_1800
	movw   $0x10,-0x48(%bp)
L_1800:
	lea    -0x44(%bp),%ax
	mov    %ax,%di
	movw   $0x0,-0x4a(%bp)
	movw   $0x0,-0x4e(%bp)
L_180f:
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_1829
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_1830
L_1829:
	pushw  0xc(%bp)
	call   L_1ed4
	pop    %cx
L_1830:
	mov    %ax,-0x46(%bp)
	cmp    $0x20,%ax
	je   L_180f
	cmpw   $0x9,-0x46(%bp)
	je   L_180f
	cmpw   $0xa,-0x46(%bp)
	je   L_180f
	cmpw   $0x2d,-0x46(%bp)
	jne   L_1880
	incw   -0x4e(%bp)
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_1871
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_1878
L_1871:
	pushw  0xc(%bp)
	call   L_1ed4
	pop    %cx
L_1878:
	mov    %ax,-0x46(%bp)
	decw   0x8(%bp)
	jmp   L_18ad
L_1880:
	cmpw   $0x2b,-0x46(%bp)
	jne   L_18ad
	decw   0x8(%bp)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_18a3
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_18aa
L_18a3:
	pushw  0xc(%bp)
	call   L_1ed4
	pop    %cx
L_18aa:
	mov    %ax,-0x46(%bp)
L_18ad:
	decw   0x8(%bp)
	jge   L_18b5
	jmp   L_1a09
L_18b5:
	movw   $0x0,-0x54(%bp)
	cmpw   $0x30,-0x46(%bp)
	jl   L_18cb
	cmpw   $0x39,-0x46(%bp)
	jg   L_18cb
	movw   $0x1,-0x54(%bp)
L_18cb:
	cmpw   $0x61,-0x46(%bp)
	jl   L_18d7
	cmpw   $0x7a,-0x46(%bp)
	jle   L_18e3
L_18d7:
	cmpw   $0x41,-0x46(%bp)
	jl   L_18ee
	cmpw   $0x5a,-0x46(%bp)
	jg   L_18ee
L_18e3:
	cmpw   $0x10,-0x48(%bp)
	jne   L_18ee
	movw   $0x1,-0x54(%bp)
L_18ee:
	cmpw   $0x0,-0x54(%bp)
	jne   L_18f7
	jmp   L_198b
L_18f7:
	incw   -0x52(%bp)
	cmpw   $0x8,-0x48(%bp)
	jne   L_1911
	mov    $0x3,%cx
	or     %cx,%cx
	je   L_194b
L_1907:
	shlw   -0x58(%bp)
	rclw   -0x56(%bp)
	loop   L_1907
	jmp   L_194b
L_1911:
	cmpw   $0xa,-0x48(%bp)
	jne   L_193c
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	mov    $0x2,%cx
	or     %cx,%cx
	je   L_192a
L_1924:
	shl    %ax
	rcl    %dx
	loop   L_1924
L_192a:
	add    -0x58(%bp),%ax
	adc    -0x56(%bp),%dx
	shl    %ax
	rcl    %dx
	mov    %ax,-0x58(%bp)
	mov    %dx,-0x56(%bp)
	jmp   L_194b
L_193c:
	mov    $0x4,%cx
	or     %cx,%cx
	je   L_194b
L_1943:
	shlw   -0x58(%bp)
	rclw   -0x56(%bp)
	loop   L_1943
L_194b:
	mov    -0x46(%bp),%ax
	mov    %ax,-0x50(%bp)
	cmpw   $0x30,-0x46(%bp)
	jl   L_1963
	cmpw   $0x39,-0x46(%bp)
	jg   L_1963
	subw   $0x30,-0x46(%bp)
	jmp   L_1979
L_1963:
	cmpw   $0x61,-0x46(%bp)
	jl   L_1975
	cmpw   $0x66,-0x46(%bp)
	jg   L_1975
	subw   $0x57,-0x46(%bp)
	jmp   L_1979
L_1975:
	subw   $0x37,-0x46(%bp)
L_1979:
	mov    -0x46(%bp),%ax
	cwtd
	add    %ax,-0x58(%bp)
	adc    %dx,-0x56(%bp)
	mov    -0x50(%bp),%ax
	mov    %ax,-0x46(%bp)
	jmp   L_19a0
L_198b:
	cmpw   $0x2e,-0x46(%bp)
	jne   L_19a2
	cmpw   $0xa,-0x48(%bp)
	jne   L_19b8
	cmpw   $0x0,-0x4c(%bp)
	je   L_19c7
	incw   -0x52(%bp)
L_19a0:
	jmp   L_1a11
L_19a2:
	cmpw   $0x65,-0x46(%bp)
	je   L_19ae
	cmpw   $0x45,-0x46(%bp)
	jne   L_19b8
L_19ae:
	cmpw   $0x0,-0x4a(%bp)
	jne   L_19b8
	cmpw   $0xa,-0x48(%bp)
L_19b8:
	je   L_19bd
	jmp   L_1a42
L_19bd:
	cmpw   $0x0,-0x4c(%bp)
	je   L_19c7
	cmpw   $0x0,-0x52(%bp)
L_19c7:
	je   L_1a42
	incw   -0x4a(%bp)
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_19f0
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_19f7
L_19f0:
	pushw  0xc(%bp)
	call   L_1ed4
	pop    %cx
L_19f7:
	mov    %ax,-0x46(%bp)
	cmp    $0x2b,%ax
	je   L_1a11
	cmpw   $0x2d,-0x46(%bp)
	je   L_1a11
	cmpw   $0x30,-0x46(%bp)
L_1a09:
	jl   L_1a42
	cmpw   $0x39,-0x46(%bp)
	jg   L_1a42
L_1a11:
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_1a35
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_1a3c
L_1a35:
	pushw  0xc(%bp)
	call   L_1ed4
	pop    %cx
L_1a3c:
	mov    %ax,-0x46(%bp)
	jmp   L_18ad
L_1a42:
	cmpw   $0x0,-0x4e(%bp)
	je   L_1a5b
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	mov    %ax,-0x58(%bp)
	mov    %dx,-0x56(%bp)
L_1a5b:
	cmpw   $0xffff,-0x46(%bp)
	je   L_1a76
	pushw  0xc(%bp)
	pushw  -0x46(%bp)
	call   L_33d8
	add    $0x4,%sp
	mov    0xe(%bp),%bx
	movw   $0x0,(%bx)
	jmp   L_1a7d
L_1a76:
	mov    0xe(%bp),%bx
	movw   $0x1,(%bx)
L_1a7d:
	cmpw   $0x0,0x4(%bp)
	je   L_1a8a
	lea    -0x44(%bp),%ax
	cmp    %ax,%di
	jne   L_1a8f
L_1a8a:
	xor    %ax,%ax
L_1a8c:
	jmp   L_1b17
L_1a8f:
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	mov    -0x4c(%bp),%ax
	mov    $0x4,%cx
	shl    %cl,%ax
	or     0xa(%bp),%ax
	jmp   L_1b08
	lea    -0x44(%bp),%ax
	push   %ax
	call   L_1c96
	fstpl  -0x60(%bp)
	pop    %cx
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	fldl   -0x60(%bp)
	fstps  (%bx)
	jmp   L_1b06
	lea    -0x44(%bp),%ax
	push   %ax
	call   L_1c96
	fstpl  -0x60(%bp)
	pop    %cx
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	fldl   -0x60(%bp)
	fstpl  (%bx)
	jmp   L_1b06
	mov    -0x58(%bp),%ax
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    %ax,(%bx)
	jmp   L_1b06
	mov    -0x58(%bp),%ax
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    %ax,(%bx)
	jmp   L_1b06
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_1b06:
	jmp   L_1b14
L_1b08:
	cmp    $0x12,%ax
	ja   L_1b14
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x27ae(%bx)
L_1b14:
	mov    $0x1,%ax
L_1b17:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b1e:
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
	jne   L_1b4a
	cmpw   $0x7530,0x8(%bp)
	jne   L_1b4a
	movw   $0x1,0x8(%bp)
L_1b4a:
	movw   $0x0,-0xa(%bp)
	cmpw   $0x73,0x6(%bp)
	jne   L_1b5a
	movw   $0x1,-0xa(%bp)
L_1b5a:
	decw   0x2(%si)
	jl   L_1b6b
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_1b70
L_1b6b:
	push   %si
	call   L_1ed4
	pop    %cx
L_1b70:
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	mov    D_072e(%bx),%al
	cbtw
	test   %ax,-0xa(%bp)
	je   L_1b85
	cmpw   $0xffff,-0x6(%bp)
	jne   L_1b5a
L_1b85:
	movw   $0x1,-0xa(%bp)
	cmpw   $0x63,0x6(%bp)
	jne   L_1b97
	movw   $0x0,-0xa(%bp)
	jmp   L_1ba2
L_1b97:
	cmpw   $0x5b,0x6(%bp)
	jne   L_1ba2
	movw   $0x2,-0xa(%bp)
L_1ba2:
	cmpw   $0xffff,-0x6(%bp)
	je   L_1be3
	mov    -0x6(%bp),%bx
	mov    D_072e(%bx),%al
	cbtw
	test   %ax,-0xa(%bp)
	jne   L_1be3
	or     %di,%di
	je   L_1bc3
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
L_1bc3:
	decw   0x8(%bp)
	jle   L_1be3
	decw   0x2(%si)
	jl   L_1bd9
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_1bde
L_1bd9:
	push   %si
	call   L_1ed4
	pop    %cx
L_1bde:
	mov    %ax,-0x6(%bp)
	jmp   L_1ba2
L_1be3:
	cmpw   $0xffff,-0x6(%bp)
	je   L_1c02
	cmpw   $0x0,0x8(%bp)
	jle   L_1bf9
	push   %si
	pushw  -0x6(%bp)
	call   L_33d8
	add    $0x4,%sp
L_1bf9:
	mov    0xc(%bp),%bx
	movw   $0x0,(%bx)
	jmp   L_1c09
L_1c02:
	mov    0xc(%bp),%bx
	movw   $0x1,(%bx)
L_1c09:
	or     %di,%di
	je   L_1c23
	cmp    -0x8(%bp),%di
	je   L_1c23
	cmpw   $0x63,0x6(%bp)
	je   L_1c1e
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
L_1c1e:
	mov    $0x1,%ax
	jmp   L_1c25
L_1c23:
	xor    %ax,%ax
L_1c25:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1c2c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	movw   $0x0,-0x6(%bp)
	cmpb   $0x5e,(%di)
	jne   L_1c44
	incw   -0x6(%bp)
	inc    %di
L_1c44:
	xor    %si,%si
L_1c46:
	cmp    $0x80,%si
	jge   L_1c61
	cmpw   $0x0,-0x6(%bp)
	je   L_1c59
	andb   $0xfd,D_072e(%si)
	jmp   L_1c5e
L_1c59:
	orb    $0x2,D_072e(%si)
L_1c5e:
	inc    %si
	jmp   L_1c46
L_1c61:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	and    $0x7f,%ax
	cmp    $0x5d,%ax
	je   L_1c8e
	cmpw   $0x0,-0x6(%bp)
	je   L_1c7f
	orb    $0x2,D_072e(%si)
	inc    %si
	jmp   L_1c85
L_1c7f:
	andb   $0xfd,D_072e(%si)
	inc    %si
L_1c85:
	or     %si,%si
	jne   L_1c61
	dec    %di
	mov    %di,%ax
	jmp   L_1c90
L_1c8e:
	mov    %di,%ax
L_1c90:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1c96:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x3c,%sp
	mov    0x4(%bp),%di
	movw   $0x1,-0x24(%bp)
L_1ca7:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    $0x20,%ax
	je   L_1ca7
	cmp    $0x2d,%si
	jne   L_1cc0
	movw   $0xffff,-0x24(%bp)
	jmp   L_1cc6
L_1cc0:
	cmp    $0x2b,%si
	je   L_1cc6
	dec    %di
L_1cc6:
	movw   $0x0,-0x22(%bp)
	fldl   D_07d4
	fstpl  -0xc(%bp)
	movw   $0x0,-0x1e(%bp)
L_1cdb:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    D_0805(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_1d28
	cmpw   $0x10,-0x1e(%bp)
	jge   L_1d20
	fldl   D_07dc
	fmull  -0xc(%bp)
	fstpl  -0x30(%bp)
	lea    -0x30(%si),%ax
	mov    %ax,-0x38(%bp)
	filds  -0x38(%bp)
	fstl   -0x38(%bp)
	faddl  -0x30(%bp)
	fstl   -0x40(%bp)
	fstpl  -0xc(%bp)
	jmp   L_1d23
L_1d20:
	incw   -0x22(%bp)
L_1d23:
	incw   -0x1e(%bp)
	jmp   L_1cdb
L_1d28:
	cmp    $0x2e,%si
	jne   L_1d78
L_1d2d:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    D_0805(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_1d78
	cmpw   $0x10,-0x1e(%bp)
	jge   L_1d73
	fldl   D_07e4
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
L_1d73:
	incw   -0x1e(%bp)
	jmp   L_1d2d
L_1d78:
	movw   $0x1,-0x26(%bp)
	movw   $0x0,-0x20(%bp)
	cmp    $0x45,%si
	je   L_1d8c
	cmp    $0x65,%si
	jne   L_1de1
L_1d8c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    $0x2b,%ax
	je   L_1da6
	cmp    $0x2d,%si
	jne   L_1da5
	movw   $0xffff,-0x26(%bp)
	jmp   L_1da6
L_1da5:
	dec    %di
L_1da6:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    D_0805(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_1dca
	mov    -0x20(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	lea    -0x30(%si),%dx
	add    %dx,%ax
	mov    %ax,-0x20(%bp)
	jmp   L_1da6
L_1dca:
	cmpw   $0x0,-0x26(%bp)
	jge   L_1dd8
	mov    -0x20(%bp),%ax
	neg    %ax
	mov    %ax,-0x20(%bp)
L_1dd8:
	mov    -0x22(%bp),%ax
	add    -0x20(%bp),%ax
	mov    %ax,-0x22(%bp)
L_1de1:
	movw   $0x1,-0x26(%bp)
	cmpw   $0x0,-0x22(%bp)
	jge   L_1df9
	movw   $0xffff,-0x26(%bp)
	mov    -0x22(%bp),%ax
	neg    %ax
	mov    %ax,-0x22(%bp)
L_1df9:
	mov    -0x22(%bp),%ax
	imulw  -0x26(%bp)
	add    -0x1e(%bp),%ax
	cmp    $0xffd9,%ax
	jge   L_1e17
	fldl   D_07ec
	fstpl  -0xc(%bp)
	movw   $0x0,-0x22(%bp)
L_1e17:
	fldl   D_07f4
	fstpl  -0x14(%bp)
	fldl   D_07fc
	fstpl  -0x1c(%bp)
	mov    -0x22(%bp),%ax
	mov    %ax,-0x28(%bp)
L_1e31:
	testw  $0x1,-0x22(%bp)
	je   L_1e46
	fldl   -0x14(%bp)
	fmull  -0x1c(%bp)
	fstpl  -0x14(%bp)
L_1e46:
	sarw   -0x22(%bp)
	cmpw   $0x0,-0x22(%bp)
	je   L_1e5f
	fldl   -0x1c(%bp)
	fmull  -0x1c(%bp)
	fstpl  -0x1c(%bp)
	jmp   L_1e31
L_1e5f:
	cmpw   $0x0,-0x26(%bp)
	jge   L_1e75
	fldl   -0xc(%bp)
	fdivl  -0x14(%bp)
	fstpl  -0xc(%bp)
	jmp   L_1e83
L_1e75:
	fldl   -0xc(%bp)
	fmull  -0x14(%bp)
	fstpl  -0xc(%bp)
L_1e83:
	mov    -0x26(%bp),%ax
	imulw  -0x28(%bp)
	push   %ax
	fldl   -0xc(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_3454
	fstpl  -0x30(%bp)
	add    $0xa,%sp
	fldl   -0x30(%bp)
	fstpl  -0xc(%bp)
	cmpw   $0x0,-0x24(%bp)
	jge   L_1ec7
	fldl   -0xc(%bp)
	fchs
	fstl   -0x30(%bp)
	fstpl  -0xc(%bp)
L_1ec7:
	fldl   -0xc(%bp)
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ed4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_1eeb
	mov    $0xffff,%ax
	jmp   L_1ef7
L_1eeb:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_1ef9
	mov    $0xffff,%ax
L_1ef7:
	jmp   L_1f71
L_1ef9:
	cmpw   $0x0,0x4(%di)
	jne   L_1f2d
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1f14
	mov    0x7(%di),%al
	cbtw
	add    $D_0886,%ax
	mov    %ax,0x4(%di)
	jmp   L_1ef9
L_1f14:
	mov    $0x200,%ax
	push   %ax
	call   L_3074
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1f29
	orb    $0x4,0x6(%di)
	jmp   L_1ef9
L_1f29:
	orb    $0x8,0x6(%di)
L_1f2d:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1f40
	mov    $0x1,%ax
	jmp   L_1f43
L_1f40:
	mov    $0x200,%ax
L_1f43:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_3372
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_1f73
	cmpw   $0xffff,0x2(%di)
	jne   L_1f65
	orb    $0x10,0x6(%di)
	jmp   L_1f69
L_1f65:
	orb    $0x20,0x6(%di)
L_1f69:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_1f71:
	jmp   L_1f7d
L_1f73:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_1f7d:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1f82:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_1f8d:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_1f98
	inc    %si
	jmp   L_1f8d
L_1f98:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1f9e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1fab:
	decw   0x8(%bp)
	jl   L_1fcf
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_1fcf
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_1fab
	xor    %ax,%ax
	jmp   L_1fea
L_1fcf:
	cmpw   $0x0,0x8(%bp)
	jge   L_1fd9
	xor    %ax,%ax
	jmp   L_1fea
L_1fd9:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_1fea:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1fee:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
L_2006:
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_203c
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_2037
L_201f:
	incw   -0x6(%bp)
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_2032
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	jmp   L_201f
L_2032:
	mov    -0x8(%bp),%ax
	jmp   L_203f
L_2037:
	incw   -0x6(%bp)
	jmp   L_2006
L_203c:
	mov    -0x8(%bp),%ax
L_203f:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2046:
	push   %bp
	mov    %sp,%bp
	mov    $0x16,%cx
	mov    0x4(%bp),%ax
	or     %ax,%ax
	jle   L_2089
	cmp    $0x20,%ax
	jg   L_2089
	mov    %ax,%cx
	shl    %ax
	add    $D_0cd4,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    0x6(%bp),%ax
	mov    %ax,(%bx)
	cmp    $0x2,%ax
	jl   L_2075
	mov    $0x6,%ax
	mul    %cx
	add    $D_008c,%ax
L_2075:
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
	jl   L_2086
	pop    %ax
	jmp   L_2087
L_2086:
	pop    %bx
L_2087:
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2090
L_2089:
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	mov    $0xffff,%ax
L_2090:
	pop    %bp
	ret
L_2092:
	push   %ax
	mov    $D_0cd6,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0cd8,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0cda,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0cdc,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0cde,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0ce0,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0ce2,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0ce4,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0ce6,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0ce8,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0cea,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0cec,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0cee,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0cf0,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0cf2,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0cf4,%ax
	jmp   L_20f6
	push   %ax
	mov    $D_0cf6,%ax
L_20f6:
	jmp   L_2150
	push   %ax
	mov    $D_0cf8,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0cfa,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0cfc,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0cfe,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0d00,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0d02,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0d04,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0d06,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0d08,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0d0a,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0d0c,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0d0e,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0d10,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0d12,%ax
	jmp   L_2150
	push   %ax
	mov    $D_0d14,%ax
L_2150:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	sub    $D_0cd4,%ax
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


L_2166:
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
L_2181:
	pop    %si
	pop    %bp
	ret


L_2184:
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
	call   L_21d6
	add    $0x6,%sp
	decw   -0xa(%bp)
	jl   L_21be
	mov    -0xc(%bp),%bx
	incw   -0xc(%bp)
	movb   $0x0,(%bx)
	xor    %ax,%ax
	jmp   L_21cb
L_21be:
	lea    -0xc(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_2ea0
	add    $0x4,%sp
L_21cb:
	mov    0x4(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_21d6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_21ee:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_2200
	jmp   L_279d
L_2200:
	cmpb   $0x25,-0x5(%bp)
	je   L_2213
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_2211:
	jmp   L_21ee
L_2213:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_223e
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_2dae
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_223e:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_2257
	incb   -0xa6(%bp)
	inc    %di
L_2257:
	cmpb   $0x30,(%di)
	jne   L_2262
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_2262:
	cmpb   $0x2a,(%di)
	je   L_2292
	movw   $0x0,-0xe(%bp)
L_226c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_22a8
	cmpw   $0x9,-0xc(%bp)
	jg   L_22a8
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_226c
L_2292:
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
L_22a8:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_22ef
	cmpb   $0x2a,(%di)
	je   L_22f1
	movw   $0x0,-0x10(%bp)
L_22c1:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_22eb
	cmpw   $0x9,-0xc(%bp)
	jg   L_22eb
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_22c1
L_22eb:
	addw   $0x30,-0xc(%bp)
L_22ef:
	jmp   L_2308
L_22f1:
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
L_2308:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_2712
L_2313:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_2324
L_231b:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_2354
	jmp   L_234d
L_2324:
	cmp    $0x64,%ax
	je   L_2356
	cmp    $0x6f,%ax
	je   L_233a
	cmp    $0x75,%ax
	je   L_234d
	cmp    $0x78,%ax
	je   L_2348
	jmp   L_231b
L_233a:
	movw   $0x8,-0x12(%bp)
	jmp   L_234d
L_2341:
	movw   $0x8,-0x12(%bp)
	jmp   L_2354
L_2348:
	movw   $0x10,-0x12(%bp)
L_234d:
	jmp   L_239f
L_234f:
	movw   $0x10,-0x12(%bp)
L_2354:
	jmp   L_23af
L_2356:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_2373
L_2366:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_2373:
	cmpw   $0x0,-0x14(%bp)
	jg   L_23bd
	jl   L_2381
	cmpw   $0x0,-0x16(%bp)
	jae   L_23bd
L_2381:
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
	jmp   L_23bd
L_239f:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_23bd
L_23af:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_23bd:
	cmpw   $0x0,-0x14(%bp)
	jne   L_23c7
	cmpw   $0x0,-0x16(%bp)
L_23c7:
	jne   L_23d8
	cmpw   $0x0,-0x10(%bp)
	jne   L_23d8
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_23d8:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_23df:
	cmpw   $0x0,-0x14(%bp)
	jne   L_23e9
	cmpw   $0x0,-0x16(%bp)
L_23e9:
	je   L_2434
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_28fc
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_240a
	mov    $0x30,%ax
	jmp   L_240d
L_240a:
	mov    $0x57,%ax
L_240d:
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
	call   L_27f4
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_23df
L_2434:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_2483
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
L_2446:
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_2434
L_2452:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_245e:
	cmpw   $0x2,-0xc(%bp)
L_2462:
	jge   L_2483
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_247e
	decw   -0x8(%bp)
L_247e:
	incw   -0xc(%bp)
	jmp   L_245e
L_2483:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_248a:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_24a1
	movw   $0x0,-0xc(%bp)
L_24a1:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_24b0
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_24b0:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_2dae
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_24d3:
	jmp   L_2211
L_24d6:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_24e8
	movw   $D_089a,-0xa(%bp)
L_24e8:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_24f7
	movw   $0x7fff,-0xc(%bp)
L_24f7:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_24fd:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_248a
	decw   -0xc(%bp)
	jl   L_248a
	incw   -0x8(%bp)
	jmp   L_24fd
L_250f:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_2517:
	jmp   L_24d3
L_2519:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_2525
	movw   $0x6,-0x10(%bp)
L_2525:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2afa
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_255a
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_255a:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_256f
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_2587
L_256f:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_256f
L_2587:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_259a
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_259a:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_25b9
L_25a6:
	decw   -0xc(%bp)
	jl   L_25b9
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_25a6
L_25b9:
	cmpw   $0x0,-0xc(%bp)
	jg   L_25c2
	jmp   L_2483
L_25c2:
	decw   -0xc(%bp)
	jge   L_25ca
	jmp   L_2483
L_25ca:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_25c2
L_25df:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_25ed
	movw   $0x6,-0x10(%bp)
	jmp   L_25f0
L_25ed:
	incw   -0x10(%bp)
L_25f0:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2ace
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_2625
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_2625:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_2630
	incw   -0x1a(%bp)
L_2630:
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
L_2652:
	decw   -0xc(%bp)
	jle   L_266c
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_2652
L_266c:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_2689
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_269a
L_2689:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_269a:
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
	jmp   L_2483
L_26cb:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_26d7
	movw   $0x6,-0x10(%bp)
L_26d7:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_2952
	add    $0xc,%sp
	add    $0x8,%si
L_26f5:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_26f5
	decw   -0x8(%bp)
	jmp   L_2483
L_2706:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_270f:
	jmp   L_2517
L_2712:
	cmp    $0x65,%ax
	jne   L_271a
	jmp   L_25df
L_271a:
	jg   L_275b
	cmp    $0x4f,%ax
	jne   L_2724
	jmp   L_233a
L_2724:
	jg   L_273b
	cmp    $0x25,%ax
	je   L_2706
	cmp    $0x44,%ax
	jne   L_2733
	jmp   L_2356
L_2733:
	cmp    $0x4c,%ax
L_2736:
	jne   L_273b
	jmp   L_2313
L_273b:
	cmp    $0x55,%ax
	jne   L_2743
	jmp   L_239f
L_2743:
	cmp    $0x58,%ax
	jne   L_274b
	jmp   L_2348
L_274b:
	cmp    $0x63,%ax
	jne   L_2753
	jmp   L_2452
L_2753:
	cmp    $0x64,%ax
	jne   L_275b
	jmp   L_2366
L_275b:
	cmp    $0x6f,%ax
	jne   L_2763
	jmp   L_2341
L_2763:
	jg   L_277a
	cmp    $0x66,%ax
	jne   L_276d
	jmp   L_2519
L_276d:
	cmp    $0x67,%ax
	jne   L_2775
	jmp   L_26cb
L_2775:
	cmp    $0x6c,%ax
	je   L_2736
L_277a:
	cmp    $0x72,%ax
	jne   L_2782
	jmp   L_250f
L_2782:
	cmp    $0x73,%ax
	jne   L_278a
	jmp   L_24d6
L_278a:
	cmp    $0x75,%ax
	jne   L_2792
	jmp   L_23af
L_2792:
	cmp    $0x78,%ax
	jne   L_279a
	jmp   L_234f
L_279a:
	jmp   L_270f
L_279d:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_27c1
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_2dae
	add    $0x8,%sp
L_27c1:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_2930


L_27cb:
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
	call   L_2930


L_27f4:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_2812
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_2812:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_281b:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_281b
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_282b:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_282b
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_27cb
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_286c
	jne   L_2861
	or     $0x0,%ax
	je   L_286c
L_2861:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_286c:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_2930


L_2874:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_2890
L_2886:
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_2890:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_28a7
	incw   -0x4(%bx)
	neg    %dx
L_28a2:
	neg    %ax
	sbb    $0x0,%dx
L_28a7:
	push   %dx
L_28a8:
	push   %ax
	call   L_27f4
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_28c1
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_28c1:
	ret
	call   L_2930


L_28c5:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_2874
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_27cb
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_2930


L_28fc:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_27f4
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_27cb
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_2930:
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


L_2952:
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
	call   L_2ace
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_298c
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_298c:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_2993:
	cmpw   $0x0,-0xa(%bp)
	jle   L_29ad
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_29ad
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_2993
L_29ad:
	cmpw   $0x0,-0x8(%bp)
	jl   L_29be
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_29ca
L_29be:
	cmpw   $0x0,-0x8(%bp)
	jge   L_29c8
	cmpw   $0xfffd,-0x8(%bp)
L_29c8:
	jge   L_2a49
L_29ca:
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
L_29e5:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_29ff
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_29e5
L_29ff:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_2a1b
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_2a21
L_2a1b:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_2a21:
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
	jmp   L_2ab9
L_2a49:
	cmpw   $0x0,-0x8(%bp)
	jg   L_2a6b
	cmpb   $0x30,(%di)
	je   L_2a5a
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_2a5a:
	cmpw   $0x0,-0x8(%bp)
	jge   L_2a6b
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_2a5a
L_2a6b:
	movw   $0x1,-0xa(%bp)
L_2a70:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_2a98
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
	jne   L_2a93
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_2a93:
	incw   -0xa(%bp)
	jmp   L_2a70
L_2a98:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_2ab9
L_2aa0:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_2ab3
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_2aa0
L_2ab3:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_2ab9:
	cmpb   $0x2e,-0x1(%si)
	jne   L_2ac0
	dec    %si
L_2ac0:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2ace:
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
	call   L_2b25
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2afa:
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
	call   L_2b25
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2b25:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_2b38
	movw   $0x0,0xc(%bp)
L_2b38:
	cmpw   $0x4f,0xc(%bp)
	jl   L_2b43
	movw   $0x4e,0xc(%bp)
L_2b43:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_08a2,%si
	fldl   0x4(%bp)
	fcompl D_08f2
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_2b80
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_2b80:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_346e
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $D_08f2,-0x16(%bp)
	fldl   -0xc(%bp)
	fcompl D_08fa
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_2bcb
	jmp   L_2c78
L_2bcb:
	movw   $D_08f2,-0x16(%bp)
L_2bd0:
	fldl   -0xc(%bp)
	fcompl D_0902
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_2bec
	jmp   L_2c76
L_2bec:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	fdivl  D_090a
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_346e
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   D_0912
	fstl   -0x1e(%bp)
	fdivl  D_091a
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  D_0922
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
	jmp   L_2bd0
L_2c5f:
	cmpw   $D_08f2,-0x16(%bp)
	jae   L_2cb9
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_2c76:
	jmp   L_2c5f
L_2c78:
	fldl   0x4(%bp)
	fcompl D_092a
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_2cc8
L_2c91:
	fldl   0x4(%bp)
	fmull  D_0932
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl D_093a
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_2cb9:
	jae   L_2cc8
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_2c91
L_2cc8:
	mov    0xc(%bp),%ax
	add    $D_08a2,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_2cda
	add    %di,-0x16(%bp)
L_2cda:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_08a2,-0x16(%bp)
	jae   L_2cf0
	movb   $0x0,D_08a2
	mov    $D_08a2,%ax
	jmp   L_2d5b
L_2cf0:
	cmp    -0x16(%bp),%si
	ja   L_2d4c
	cmp    $D_08f2,%si
	jae   L_2d4c
	fldl   0x4(%bp)
	fmull  D_0942
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_346e
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
	jmp   L_2cf0
L_2d4c:
	cmpw   $D_08f2,-0x16(%bp)
	jb   L_2d5d
	movb   $0x0,D_08f1
	mov    $D_08a2,%ax
L_2d5b:
	jmp   L_2da7
L_2d5d:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_2d65:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_2da1
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_08a2,-0x16(%bp)
	jbe   L_2d84
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_2d65
L_2d84:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_2d65
	cmp    $D_08a2,%si
	jbe   L_2d9e
	movb   $0x30,(%si)
L_2d9e:
	inc    %si
	jmp   L_2d65
L_2da1:
	movb   $0x0,(%si)
	mov    $D_08a2,%ax
L_2da7:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2dae:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_2dbb:
	cmpw   $0x0,0x8(%bp)
	jl   L_2dc4
	jmp   L_2e52
L_2dc4:
	cmpb   $0x2d,(%di)
	jne   L_2e01
	cmpw   $0x30,0xc(%bp)
	jne   L_2e01
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_2df0
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
	jmp   L_2e00
L_2df0:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_2ea0
	add    $0x4,%sp
L_2e00:
	dec    %si
L_2e01:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_2e1f
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
	jmp   L_2e2b
L_2e1f:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_2ea0
	add    $0x4,%sp
L_2e2b:
	incw   0x8(%bp)
	jmp   L_2dbb
L_2e30:
	dec    %si
	jl   L_2e66
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_2e54
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
L_2e52:
	jmp   L_2e30
L_2e54:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_2ea0
	add    $0x4,%sp
	jmp   L_2e30
L_2e66:
	cmpw   $0x0,0x8(%bp)
	je   L_2e9b
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_2e8a
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
	jmp   L_2e96
L_2e8a:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_2ea0
	add    $0x4,%sp
L_2e96:
	decw   0x8(%bp)
	jmp   L_2e66
L_2e9b:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2ea0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_2ebb
	mov    $0xffff,%ax
	jmp   L_2f82
L_2ebb:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_2eee
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
	call   L_343a
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_2f73
L_2eee:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_2f40
	cmp    $D_0952,%di
	jne   L_2f19
	mov    D_0959,%al
	cbtw
	push   %ax
	call   L_3334
	pop    %cx
	or     %ax,%ax
	je   L_2f0e
	orb    $0x4,0x6(%di)
	jmp   L_2ebb
L_2f0e:
	movw   $D_0d1c,0x4(%di)
	movw   $D_0d1c,(%di)
	jmp   L_2ebb
L_2f19:
	mov    $0x200,%ax
	push   %ax
	call   L_3074
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_2f30
	orb    $0x4,0x6(%di)
	jmp   L_2ebb
L_2f30:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_2f62
L_2f40:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_2f62
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_343a
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_2f62:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_2f73:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_2f84
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_2f82:
	jmp   L_2f87
L_2f84:
	mov    0x4(%bp),%ax
L_2f87:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2f8e:
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
	jne   L_2fda
	mov    0x4(%di),%si
	or     %si,%si
	je   L_2fda
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_2fda
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_343a
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_2fda
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_2fdc
L_2fda:
	xor    %ax,%ax
L_2fdc:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2fe1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_094a,%di
L_2fe9:
	cmp    D_09ea,%di
	jae   L_2ff9
	push   %di
	call   L_2ffd
	pop    %cx
	add    $0x8,%di
	jmp   L_2fe9
L_2ff9:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2ffd:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_3050
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_3050
	push   %di
	call   L_2f8e
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_330a
	pop    %cx
	or     %ax,%ax
	jge   L_3032
	mov    $0xffff,%si
L_3032:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_3042
	pushw  0x4(%di)
	call   L_31be
	pop    %cx
L_3042:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_3050
	movw   $0x0,0x4(%di)
L_3050:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_3074:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_09ec
	jne   L_309f
	mov    $D_09ee,%ax
	or     $0x1,%ax
	mov    %ax,D_09ec
	mov    $D_09ec,%ax
	or     $0x1,%ax
	mov    %ax,D_09ee
	/* dual-reloc split: movw $D_09ee,D_111e */
	mov	$D_09ee,%ax
	mov	%ax,D_111e
	/* dual-reloc split: movw $D_09ec,D_111c */
	mov	$D_09ec,%ax
	mov	%ax,D_111c
L_309f:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_111c,%di
L_30b3:
	movw   $0x0,D_09f0
L_30b9:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_30e8
L_30c0:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_30cf
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_30c0
L_30cf:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_30e8
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_30e8
	jmp   L_318f
L_30e8:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_30b9
	cmp    D_111e,%si
	jne   L_3101
	cmp    $D_09ec,%di
	je   L_3105
L_3101:
	xor    %ax,%ax
	jmp   L_313d
L_3105:
	incw   D_09f0
	cmpw   $0x1,D_09f0
	jle   L_30b9
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_09f0
	xor    %ax,%ax
	push   %ax
	call   L_32a2
	pop    %cx
	mov    %ax,%si
	mov    D_09f0,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_313f
	xor    %ax,%ax
L_313d:
	jmp   L_3152
L_313f:
	mov    D_09f0,%ax
	shl    %ax
	push   %ax
	call   L_32a2
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_3154
	xor    %ax,%ax
L_3152:
	jmp   L_31b9
L_3154:
	mov    D_111e,%bx
	mov    %si,(%bx)
	mov    D_111e,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_3172
	mov    D_111e,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_111e,%bx
	mov    %ax,(%bx)
L_3172:
	mov    D_09f0,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_111e
	mov    $D_09ec,%ax
	or     $0x1,%ax
	mov    D_111e,%bx
	mov    %ax,(%bx)
	jmp   L_30b3
L_318f:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_111c
	cmp    %ax,%si
	jbe   L_31ae
	mov    D_111c,%bx
	mov    (%bx),%ax
	mov    %ax,D_1120
	mov    D_111c,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_31ae:
	mov    D_111c,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_31b9:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_31be:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_111c
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
	je   L_31f3
	push   %di
	call   L_31be
	pop    %cx
L_31f3:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_3074
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_3212
	cmp    %di,%si
	jne   L_3216
L_3212:
	mov    %si,%ax
	jmp   L_327e
L_3216:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_3235
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_3235:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_3253
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_3235
L_3253:
	cmp    %di,%si
	jae   L_327c
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_327c
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
	mov    D_1120,%ax
	mov    %ax,(%bx)
L_327c:
	mov    %si,%ax
L_327e:
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
	jns    L_3299
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_32a0
L_3299:
	mov    0x4(%bp),%bx
	mov    %bx,D_09f2
L_32a0:
	pop    %bp
	ret


L_32a2:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_09f2,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_32b9
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_32c6
L_32b9:
	mov    D_09f2,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_09f2
L_32c6:
	pop    %bp
	ret


L_32c8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_32d5:
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_32f4
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_32d5
	xor    %ax,%ax
	jmp   L_3305
L_32f4:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_3305:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_330a:
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
	jns    L_331b
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_331b:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_331e:
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
	jns    L_3332
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3332:
	pop    %bp
	ret


L_3334:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_33a5
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_3351
	xor    %ax,%ax
	jmp   L_3354
L_3351:
	mov    $0x1,%ax
L_3354:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_335c:
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
	jns    L_3370
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3370:
	pop    %bp
	ret


L_3372:
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
	jns    L_3389
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3389:
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
	call   L_33be
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_33a5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_33be
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_33be:
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
	jns    L_33d5
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_33d5:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_33d8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x6(%bp),%di
	cmpw   $0xffff,0x4(%bp)
	jne   L_33ec
	mov    $0xffff,%ax
	jmp   L_3422
L_33ec:
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	je   L_33fc
	mov    (%di),%ax
	cmp    0x4(%di),%ax
	ja   L_3412
L_33fc:
	mov    (%di),%ax
	cmp    0x4(%di),%ax
	jne   L_340d
	cmpw   $0x0,0x2(%di)
	jne   L_340d
	incw   (%di)
	jmp   L_3412
L_340d:
	mov    $0xffff,%ax
	jmp   L_3422
L_3412:
	incw   0x2(%di)
	decw   (%di)
	mov    (%di),%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	xor    %ax,%ax
L_3422:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3426:
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
	jns    L_3437
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3437:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_343a:
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
	jns    L_3451
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3451:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3454:
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


L_346e:
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


L_348a:
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
	jns    L_349b
	neg	%ax
	mov	%ax,D_0ccc	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_349b:
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
	mov	%ax,D_0ccc	/* errno */
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
	.byte	0x40, 0x28, 0x23, 0x29	/* |@(#)| */
D_0008:
	.ascii	"ar.c 4.1 10/1/80\0"
	.byte	0x00	/* |.| */
D_001a:
	.word	D_00c2	/* ptr: 0x20c2 */
D_001c:
	.word	D_00ca	/* ptr: 0x20ca */
D_001e:
	.byte	0x01, 0x00	/* |..| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x03, 0x00, 0x00, 0x00	/* |....| */
D_0026:
	.word	D_00d2	/* ptr: 0x20d2 */
D_0028:
	.word	D_00de	/* ptr: 0x20de */
D_002a:
	.word	D_00eb	/* ptr: 0x20eb */
	.byte	0x96, 0x00, 0x96, 0x00, 0x96, 0x00, 0xab, 0x00	/* |........| */
	.byte	0xe7, 0x00, 0xe7, 0x00, 0xe7, 0x00, 0xe7, 0x00	/* |........| */
	.byte	0x96, 0x00, 0xe7, 0x00, 0xe7, 0x00, 0x96, 0x00	/* |........| */
	.byte	0xd3, 0x00, 0x96, 0x00, 0xe7, 0x00, 0xc9, 0x00	/* |........| */
	.byte	0xdd, 0x00, 0xa1, 0x00, 0xe7, 0x00, 0xbf, 0x00	/* |........| */
	.byte	0x96, 0x00, 0x96, 0x00, 0xe7, 0x00, 0xb5, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x00, 0x01, 0x72, 0x00, 0x2d, 0x00	/* |....r.-.| */
	.byte	0x01, 0x00, 0x80, 0x00, 0x77, 0x00, 0x2d, 0x00	/* |....w.-.| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x00, 0x08, 0x73, 0x00, 0x40, 0x00, 0x78, 0x00	/* |..s.@.x.| */
	.byte	0x2d, 0x00, 0x01, 0x00, 0x20, 0x00, 0x72, 0x00	/* |-... .r.| */
	.byte	0x2d, 0x00, 0x01, 0x00, 0x10, 0x00, 0x77, 0x00	/* |-.....w.| */
	.byte	0x2d, 0x00	/* |-.| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x00, 0x04	/* |..| */
D_008c:
	.byte	0x73, 0x00	/* |s.| */
	.word	D_0008	/* ptr: 0x0008 */
	.byte	0x78, 0x00, 0x2d, 0x00, 0x01, 0x00, 0x04, 0x00	/* |x.-.....| */
	.byte	0x72, 0x00, 0x2d, 0x00, 0x01, 0x00	/* |r.-...| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x77, 0x00, 0x2d, 0x00	/* |w.-.| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x00, 0x02, 0x74, 0x00, 0x01, 0x00, 0x78, 0x00	/* |..t...x.| */
	.byte	0x2d, 0x00	/* |-.| */
D_00b0:
	.byte	0x5c, 0x20, 0x64, 0x20, 0x6c, 0x20, 0x78, 0x20	/* |\ d l x | */
	.byte	0x80, 0x20, 0x88, 0x20, 0x94, 0x20, 0x9c, 0x20	/* |. . . . | */
	.byte	0xa4, 0x20	/* |. | */
D_00c2:
	.ascii	"mrxtdpq\0"
D_00ca:
	.ascii	"uvnbail\0"
D_00d2:
	.ascii	"/tmp/vXXXXX\0"
D_00de:
	.ascii	"/tmp/v1XXXXX\0"
D_00eb:
	.ascii	"/tmp/v2XXXXX\0"
D_00f8:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x62, 0x61, 0x64, 0x20	/* |ar: bad | */
	.byte	0x6f, 0x70, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x60	/* |option `| */
	.byte	0x25, 0x63, 0x27, 0x0a, 0x00	/* |%c'..| */
D_010d:
	.ascii	"vXXXXX\0"
D_0114:
	.ascii	"v1XXXXX\0"
D_011c:
	.ascii	"v2XXXXX\0"
D_0124:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x6f, 0x6e, 0x65, 0x20	/* |ar: one | */
	.byte	0x6f, 0x66, 0x20, 0x5b, 0x25, 0x73, 0x5d, 0x20	/* |of [%s] | */
	.byte	0x6d, 0x75, 0x73, 0x74, 0x20, 0x62, 0x65, 0x20	/* |must be | */
	.byte	0x73, 0x70, 0x65, 0x63, 0x69, 0x66, 0x69, 0x65	/* |specifie| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_0147:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x6f, 0x6e, 0x6c, 0x79	/* |ar: only| */
	.byte	0x20, 0x6f, 0x6e, 0x65, 0x20, 0x6f, 0x66, 0x20	/* | one of | */
	.byte	0x5b, 0x25, 0x73, 0x5d, 0x20, 0x61, 0x6c, 0x6c	/* |[%s] all| */
	.byte	0x6f, 0x77, 0x65, 0x64, 0x0a, 0x00	/* |owed..| */
D_0165:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x63, 0x61, 0x6e, 0x6e	/* |ar: cann| */
	.byte	0x6f, 0x74, 0x20, 0x6f, 0x70, 0x65, 0x6e, 0x20	/* |ot open | */
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_0179:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x25, 0x73, 0x20, 0x63	/* |ar: %s c| */
	.byte	0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x63, 0x72	/* |annot cr| */
	.byte	0x65, 0x61, 0x74, 0x65, 0x0a, 0x00	/* |eate..| */
D_018f:
	.byte	0x0a, 0x3c, 0x25, 0x73, 0x3e, 0x0a, 0x0a, 0x00	/* |.<%s>...| */
D_0197:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x63, 0x61, 0x6e, 0x6e	/* |ar: cann| */
	.byte	0x6f, 0x74, 0x20, 0x63, 0x72, 0x65, 0x61, 0x74	/* |ot creat| */
	.byte	0x65, 0x20, 0x74, 0x68, 0x69, 0x72, 0x64, 0x20	/* |e third | */
	.byte	0x74, 0x65, 0x6d, 0x70, 0x0a, 0x00	/* |temp..| */
D_01b5:
	.byte	0x25	/* |%| */
	.word	D_0a73	/* ptr: 0x0a73 */
	.byte	0x00	/* |.| */
D_01b9:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x61, 0x62, 0x69, 0x20	/* |ar: abi | */
	.byte	0x6e, 0x6f, 0x74, 0x20, 0x61, 0x6c, 0x6c, 0x6f	/* |not allo| */
	.byte	0x77, 0x65, 0x64, 0x20, 0x77, 0x69, 0x74, 0x68	/* |wed with| */
	.byte	0x20, 0x71, 0x0a, 0x00	/* | q..| */
D_01d5:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x25, 0x73, 0x20, 0x63	/* |ar: %s c| */
	.byte	0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6f, 0x70	/* |annot op| */
	.byte	0x65, 0x6e, 0x0a, 0x00	/* |en..| */
D_01e9:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x63, 0x61, 0x6e, 0x6e	/* |ar: cann| */
	.byte	0x6f, 0x74, 0x20, 0x63, 0x72, 0x65, 0x61, 0x74	/* |ot creat| */
	.byte	0x65, 0x20, 0x74, 0x65, 0x6d, 0x70, 0x20, 0x66	/* |e temp f| */
	.byte	0x69, 0x6c, 0x65, 0x0a, 0x00	/* |ile..| */
D_0206:
	.byte	0x21, 0x3c, 0x61, 0x72, 0x63, 0x68, 0x3e, 0x0a	/* |!<arch>.| */
	.byte	0x00	/* |.| */
D_020f:
	.byte	0x21, 0x3c, 0x61, 0x72, 0x63, 0x68, 0x3e, 0x0a	/* |!<arch>.| */
	.byte	0x00	/* |.| */
D_0218:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x25, 0x73, 0x20, 0x6e	/* |ar: %s n| */
	.byte	0x6f, 0x74, 0x20, 0x69, 0x6e, 0x20, 0x61, 0x72	/* |ot in ar| */
	.byte	0x63, 0x68, 0x69, 0x76, 0x65, 0x20, 0x66, 0x6f	/* |chive fo| */
	.byte	0x72, 0x6d, 0x61, 0x74, 0x0a, 0x00	/* |rmat..| */
D_0236:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x63, 0x72, 0x65, 0x61	/* |ar: crea| */
	.byte	0x74, 0x69, 0x6e, 0x67, 0x20, 0x25, 0x73, 0x0a	/* |ting %s.| */
	.byte	0x00	/* |.| */
D_0247:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x63, 0x61, 0x6e, 0x6e	/* |ar: cann| */
	.byte	0x6f, 0x74, 0x20, 0x63, 0x72, 0x65, 0x61, 0x74	/* |ot creat| */
	.byte	0x65, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |e %s..| */
D_025d:
	.byte	0x21, 0x3c, 0x61, 0x72, 0x63, 0x68, 0x3e, 0x0a	/* |!<arch>.| */
	.byte	0x00	/* |.| */
D_0266:
	.byte	0x21, 0x3c, 0x61, 0x72, 0x63, 0x68, 0x3e, 0x0a	/* |!<arch>.| */
	.byte	0x00	/* |.| */
D_026f:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x25, 0x73, 0x20, 0x6e	/* |ar: %s n| */
	.byte	0x6f, 0x74, 0x20, 0x69, 0x6e, 0x20, 0x61, 0x72	/* |ot in ar| */
	.byte	0x63, 0x68, 0x69, 0x76, 0x65, 0x20, 0x66, 0x6f	/* |chive fo| */
	.byte	0x72, 0x6d, 0x61, 0x74, 0x0a, 0x00	/* |rmat..| */
D_028d:
	.byte	0x75, 0x73, 0x61, 0x67, 0x65, 0x3a, 0x20, 0x61	/* |usage: a| */
	.byte	0x72, 0x20, 0x5b, 0x25, 0x73, 0x5d, 0x5b, 0x25	/* |r [%s][%| */
	.byte	0x73, 0x5d, 0x20, 0x61, 0x72, 0x63, 0x68, 0x69	/* |s] archi| */
	.byte	0x76, 0x65, 0x20, 0x66, 0x69, 0x6c, 0x65, 0x73	/* |ve files| */
	.byte	0x20, 0x2e, 0x2e, 0x2e, 0x0a, 0x00	/* | .....| */
D_02b3:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x25, 0x73, 0x20, 0x64	/* |ar: %s d| */
	.byte	0x6f, 0x65, 0x73, 0x20, 0x6e, 0x6f, 0x74, 0x20	/* |oes not | */
	.byte	0x65, 0x78, 0x69, 0x73, 0x74, 0x0a, 0x00	/* |exist..| */
D_02ca:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x25, 0x73, 0x20, 0x6e	/* |ar: %s n| */
	.byte	0x6f, 0x74, 0x20, 0x66, 0x6f, 0x75, 0x6e, 0x64	/* |ot found| */
	.byte	0x0a, 0x00	/* |..| */
D_02dc:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x25, 0x73, 0x20, 0x63	/* |ar: %s c| */
	.byte	0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6f, 0x70	/* |annot op| */
	.byte	0x65, 0x6e, 0x0a, 0x00	/* |en..| */
D_02f0:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x63, 0x72, 0x65, 0x61	/* |ar: crea| */
	.byte	0x74, 0x69, 0x6e, 0x67, 0x20, 0x25, 0x73, 0x0a	/* |ting %s.| */
	.byte	0x00	/* |.| */
D_0301:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x63, 0x61, 0x6e, 0x6e	/* |ar: cann| */
	.byte	0x6f, 0x74, 0x20, 0x63, 0x72, 0x65, 0x61, 0x74	/* |ot creat| */
	.byte	0x65, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |e %s..| */
D_0317:
	.ascii	"%-16s%-12ld%-6u%-6u%-8o%-10ld%-2s\0"
D_0339:
	.byte	0x60, 0x0a, 0x00	/* |`..| */
D_033c:
	.word	D_0a60	/* ptr: 0x0a60 */
	.byte	0x00	/* |.| */
D_033f:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x6d, 0x61, 0x6c, 0x66	/* |ar: malf| */
	.byte	0x6f, 0x72, 0x6d, 0x65, 0x64, 0x20, 0x61, 0x72	/* |ormed ar| */
	.byte	0x63, 0x68, 0x69, 0x76, 0x65, 0x20, 0x28, 0x61	/* |chive (a| */
	.byte	0x74, 0x20, 0x25, 0x6c, 0x64, 0x29, 0x0a, 0x00	/* |t %ld)..| */
D_035f:
	.byte	0x25, 0x6c, 0x64	/* |%ld| */
	.byte	0x00	/* |.| */
D_0363:
	.byte	0x25, 0x68, 0x64	/* |%hd| */
	.byte	0x00	/* |.| */
D_0367:
	.byte	0x25, 0x68, 0x64	/* |%hd| */
	.byte	0x00	/* |.| */
D_036b:
	.byte	0x25, 0x68, 0x6f	/* |%ho| */
	.byte	0x00	/* |.| */
D_036f:
	.byte	0x25, 0x6c, 0x64, 0x00	/* |%ld.| */
D_0373:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x63, 0x61, 0x6e, 0x6e	/* |ar: cann| */
	.byte	0x6f, 0x74, 0x20, 0x63, 0x72, 0x65, 0x61, 0x74	/* |ot creat| */
	.byte	0x65, 0x20, 0x73, 0x65, 0x63, 0x6f, 0x6e, 0x64	/* |e second| */
	.byte	0x20, 0x74, 0x65, 0x6d, 0x70, 0x0a, 0x00	/* | temp..| */
D_0392:
	.byte	0x61, 0x72, 0x3a, 0x20, 0x70, 0x68, 0x61, 0x73	/* |ar: phas| */
	.byte	0x65, 0x20, 0x65, 0x72, 0x72, 0x6f, 0x72, 0x20	/* |e error | */
	.byte	0x6f, 0x6e, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |on %s..| */
D_03a9:
	.byte	0x25, 0x63, 0x20, 0x2d, 0x20, 0x25, 0x73, 0x0a	/* |%c - %s.| */
	.byte	0x00	/* |.| */
D_03b2:
	.ascii	"%3d/%1d\0"
D_03ba:
	.ascii	"%7ld\0"
D_03bf:
	.ascii	" %-12.12s %-4.4s \0"
D_03d1:
	.ascii	"ar write error\0"
D_03e0:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
D_03fa:
	.byte	0x1f, 0x00	/* |..| */
D_03fc:
	.word	D_001c	/* ptr: 0x001c */
	.byte	0x1f, 0x00	/* |..| */
	.word	D_001e	/* ptr: 0x001e */
	.byte	0x1f, 0x00	/* |..| */
	.word	D_001e	/* ptr: 0x001e */
	.byte	0x1f, 0x00, 0x1f, 0x00	/* |....| */
	.word	D_001e	/* ptr: 0x001e */
	.byte	0x1f, 0x00	/* |..| */
	.word	D_001e	/* ptr: 0x001e */
	.byte	0x1f, 0x00	/* |..| */
D_0412:
	.byte	0x05, 0x00	/* |..| */
D_0414:
	.byte	0x4d, 0x01, 0x3a, 0x00, 0x2f, 0x01	/* |M.:./.| */
D_041a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
D_0424:
	.byte	0x00, 0x00	/* |..| */
D_0426:
	.byte	0x00, 0x00	/* |..| */
D_0428:
	.byte	0x00, 0x00	/* |..| */
D_042a:
	.byte	0x00, 0x00	/* |..| */
D_042c:
	.byte	0x44, 0x61, 0x79, 0x20, 0x4d, 0x6f, 0x6e, 0x20	/* |Day Mon | */
	.byte	0x30, 0x30, 0x20, 0x30, 0x30, 0x3a, 0x30, 0x30	/* |00 00:00| */
	.byte	0x3a, 0x30, 0x30, 0x20, 0x31, 0x39, 0x30, 0x30	/* |:00 1900| */
	.byte	0x0a, 0x00	/* |..| */
D_0446:
	.ascii	"SunMonTueWedThuFriSat\0"
D_045c:
	.ascii	"JanFebMarAprMayJunJulAugSepOctNovDec\0"
	.byte	0x00	/* |.| */
D_0482:
	.byte	0x2f, 0x00	/* |/.| */
D_0484:
	.ascii	"Unknown error\0"
D_0492:
	.byte	0x3a, 0x20	/* |: | */
	.byte	0x00	/* |.| */
D_0495:
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_0498:
	.word	D_04e0	/* ptr: 0x24e0 */
	.word	D_04e8	/* ptr: 0x24e8 */
	.word	D_04f2	/* ptr: 0x24f2 */
	.word	D_050c	/* ptr: 0x250c */
	.word	D_051c	/* ptr: 0x251c */
	.word	D_0534	/* ptr: 0x2534 */
	.word	D_053e	/* ptr: 0x253e */
	.word	D_0558	/* ptr: 0x2558 */
	.word	D_056a	/* ptr: 0x256a */
	.word	D_057c	/* ptr: 0x257c */
	.word	D_058c	/* ptr: 0x258c */
	.word	D_0598	/* ptr: 0x2598 */
	.word	D_05aa	/* ptr: 0x25aa */
	.word	D_05ba	/* ptr: 0x25ba */
	.word	D_05cc	/* ptr: 0x25cc */
	.word	D_05d8	/* ptr: 0x25d8 */
	.word	D_05ee	/* ptr: 0x25ee */
	.word	D_0600	/* ptr: 0x2600 */
	.word	D_060c	/* ptr: 0x260c */
	.word	D_061e	/* ptr: 0x261e */
	.word	D_062d	/* ptr: 0x262d */
	.word	D_063d	/* ptr: 0x263d */
	.word	D_064c	/* ptr: 0x264c */
	.word	D_065d	/* ptr: 0x265d */
	.word	D_0671	/* ptr: 0x2671 */
	.word	D_0685	/* ptr: 0x2685 */
	.word	D_0696	/* ptr: 0x2696 */
	.word	D_06a5	/* ptr: 0x26a5 */
	.word	D_06b4	/* ptr: 0x26b4 */
	.word	D_06cc	/* ptr: 0x26cc */
	.word	D_06d9	/* ptr: 0x26d9 */
	.word	D_06ef	/* ptr: 0x26ef */
	.word	D_06fe	/* ptr: 0x26fe */
	.word	D_070a	/* ptr: 0x270a */
	.word	D_071d	/* ptr: 0x271d */
D_04de:
	.byte	0x23, 0x00	/* |#.| */
D_04e0:
	.ascii	"Error 0\0"
D_04e8:
	.ascii	"Not owner\0"
D_04f2:
	.byte	0x4e, 0x6f, 0x20, 0x73, 0x75, 0x63, 0x68, 0x20	/* |No such | */
	.byte	0x66, 0x69, 0x6c, 0x65, 0x20, 0x6f	/* |file o| */
D_0500:
	.ascii	"r directory\0"
D_050c:
	.ascii	"No such process\0"
D_051c:
	.ascii	"Interrupted system call\0"
D_0534:
	.ascii	"I/O error\0"
D_053e:
	.ascii	"No such device or address\0"
D_0558:
	.ascii	"Arg list too long\0"
D_056a:
	.ascii	"Exec format error\0"
D_057c:
	.ascii	"Bad file number\0"
D_058c:
	.ascii	"No children\0"
D_0598:
	.ascii	"No more processes\0"
D_05aa:
	.ascii	"Not enough core\0"
D_05ba:
	.ascii	"Permission denied\0"
D_05cc:
	.ascii	"Bad address\0"
D_05d8:
	.ascii	"Block device required\0"
D_05ee:
	.ascii	"Mount device busy\0"
D_0600:
	.ascii	"File exists\0"
D_060c:
	.ascii	"Cross-device link\0"
D_061e:
	.ascii	"No such device\0"
D_062d:
	.ascii	"Not a directory\0"
D_063d:
	.ascii	"Is a directory\0"
D_064c:
	.ascii	"Invalid argument\0"
D_065d:
	.ascii	"File table overflow\0"
D_0671:
	.ascii	"Too many open files\0"
D_0685:
	.ascii	"Not a typewriter\0"
D_0696:
	.ascii	"Text file busy\0"
D_06a5:
	.ascii	"File too large\0"
D_06b4:
	.ascii	"No space left on device\0"
D_06cc:
	.ascii	"Illegal seek\0"
D_06d9:
	.ascii	"Read-only file system\0"
D_06ef:
	.ascii	"Too many links\0"
D_06fe:
	.ascii	"Broken pipe\0"
D_070a:
	.ascii	"Argument too large\0"
D_071d:
	.ascii	"Result too large\0"
D_072e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
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
	.byte	0xde, 0x1a, 0xea, 0x1a, 0xf6, 0x1a, 0x14, 0x1b	/* |........| */
	.byte	0x14, 0x1b, 0x14, 0x1b, 0x14, 0x1b, 0x14, 0x1b	/* |........| */
	.byte	0x14, 0x1b, 0x14, 0x1b, 0x14, 0x1b, 0x14, 0x1b	/* |........| */
	.byte	0x14, 0x1b, 0x14, 0x1b, 0x14, 0x1b, 0x14, 0x1b	/* |........| */
	.byte	0xa2, 0x1a, 0xa2, 0x1a, 0xc0, 0x1a	/* |......| */
D_07d4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_07dc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_07e4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_07ec:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_07f4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_07fc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x14, 0x40	/* |.......@| */
	.byte	0x00	/* |.| */
D_0805:
	.byte	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |        | */
	.byte	0x20, 0x08, 0x08, 0x08, 0x08	/* | ....| */
	.word	D_0008	/* ptr: 0x2008 */
	.byte	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |        | */
	.byte	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |        | */
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
	.byte	0x20, 0x00	/* | .| */
D_0886:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_089a:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_08a2:
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
D_08f1:
	.byte	0x00	/* |.| */
D_08f2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_08fa:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0902:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_090a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0912:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_091a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_0922:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_092a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0932:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_093a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_0942:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_094a:
	.word	D_0f1c	/* ptr: 0x2f1c */
	.byte	0x00, 0x00	/* |..| */
	.word	D_0f1c	/* ptr: 0x2f1c */
	.byte	0x01, 0x00	/* |..| */
D_0952:
	.byte	0x00, 0x00	/* |..| */
D_0954:
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_0958:
	.byte	0x02	/* |.| */
D_0959:
	.byte	0x01	/* |.| */
D_095a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.word	D_0206	/* ptr: 0x0206 */
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
D_09ea:
	.byte	0xea, 0x29	/* |.)| */
D_09ec:
	.byte	0x00, 0x00	/* |..| */
D_09ee:
	.byte	0x00, 0x00	/* |..| */
D_09f0:
	.byte	0x00, 0x00	/* |..| */
D_09f2:
	.word	D_1122	/* ptr: 0x3122 */

	.bss

	.space	8
D_09fc:	.space	2	/* bss word @ 0x09fc */
	.space	14
D_0a0c:	.space	2	/* bss word @ 0x0a0c */
D_0a0e:	.space	2	/* bss word @ 0x0a0e */
D_0a10:	.space	2	/* bss word @ 0x0a10 */
D_0a12:	.space	2	/* bss word @ 0x0a12 */
D_0a14:	.space	2	/* bss word @ 0x0a14 */
D_0a16:	.space	2	/* bss word @ 0x0a16 */
D_0a18:	.space	2	/* bss word @ 0x0a18 */
D_0a1a:	.space	2	/* bss word @ 0x0a1a */
D_0a1c:	.space	2	/* bss word @ 0x0a1c */
	.space	6
D_0a24:	.space	2	/* bss word @ 0x0a24 */
	.space	2
D_0a28:	.space	2	/* bss word @ 0x0a28 */
	.space	2
D_0a2c:	.space	2	/* bss word @ 0x0a2c */
D_0a2e:	.space	2	/* bss word @ 0x0a2e */
	.space	2
D_0a32:	.space	2	/* bss word @ 0x0a32 */
D_0a34:	.space	2	/* bss word @ 0x0a34 */
	.space	4
D_0a3a:	.space	2	/* bss word @ 0x0a3a */
D_0a3c:	.space	2	/* bss word @ 0x0a3c */
	.space	6
D_0a44:	.space	2	/* bss word @ 0x0a44 */
	.space	14
D_0a54:	.space	2	/* bss word @ 0x0a54 */
	.space	10
D_0a60:	.space	2	/* bss word @ 0x0a60 */
	.space	4
D_0a66:	.space	2	/* bss word @ 0x0a66 */
	.space	4
D_0a6c:	.space	2	/* bss word @ 0x0a6c */
	.space	5
D_0a73:	.space	2	/* bss word @ 0x0a73 */
D_0a74:	.space	2	/* bss word @ 0x0a74 */
	.space	8
D_0a7e:	.space	2	/* bss word @ 0x0a7e */
D_0a80:	.space	2	/* bss word @ 0x0a80 */
D_0a82:	.space	2	/* bss word @ 0x0a82 */
D_0a84:	.space	2	/* bss word @ 0x0a84 */
D_0a86:	.space	2	/* bss word @ 0x0a86 */
D_0a88:	.space	2	/* bss word @ 0x0a88 */
	.space	2
D_0a8c:	.space	2	/* bss word @ 0x0a8c */
	.space	14
D_0a9c:	.space	2	/* bss word @ 0x0a9c */
D_0a9e:	.space	2	/* bss word @ 0x0a9e */
D_0aa0:	.space	2	/* bss word @ 0x0aa0 */
D_0aa2:	.space	2	/* bss word @ 0x0aa2 */
D_0aa4:	.space	2	/* bss word @ 0x0aa4 */
	.space	6
D_0aac:	.space	2	/* bss word @ 0x0aac */
	.space	413
D_0c4b:	.space	2	/* bss word @ 0x0c4b */
	.space	95
D_0cac:	.space	2	/* bss word @ 0x0cac */
D_0cad:	.space	2	/* bss word @ 0x0cad */
D_0cae:	.space	2	/* bss word @ 0x0cae */
	.space	4
D_0cb4:	.space	2	/* bss word @ 0x0cb4 */
	.space	1
D_0cb7:	.space	2	/* bss word @ 0x0cb7 */
	.space	7
D_0cc0:	.space	2	/* bss word @ 0x0cc0 */
D_0cc1:	.space	2	/* bss word @ 0x0cc1 */
	.space	3
D_0cc6:	.space	2	/* bss word @ 0x0cc6 */
D_0cc8:	.space	2	/* bss word @ 0x0cc8 */
D_0cca:	.space	2	/* bss word @ 0x0cca */
D_0ccc:	.space	2	/* bss word @ 0x0ccc */
	.space	6
D_0cd4:	.space	2	/* bss word @ 0x0cd4 */
D_0cd6:	.space	2	/* bss word @ 0x0cd6 */
D_0cd8:	.space	2	/* bss word @ 0x0cd8 */
D_0cda:	.space	2	/* bss word @ 0x0cda */
D_0cdc:	.space	2	/* bss word @ 0x0cdc */
D_0cde:	.space	2	/* bss word @ 0x0cde */
D_0ce0:	.space	2	/* bss word @ 0x0ce0 */
D_0ce2:	.space	2	/* bss word @ 0x0ce2 */
D_0ce4:	.space	2	/* bss word @ 0x0ce4 */
D_0ce6:	.space	2	/* bss word @ 0x0ce6 */
D_0ce8:	.space	2	/* bss word @ 0x0ce8 */
D_0cea:	.space	2	/* bss word @ 0x0cea */
D_0cec:	.space	2	/* bss word @ 0x0cec */
D_0cee:	.space	2	/* bss word @ 0x0cee */
D_0cf0:	.space	2	/* bss word @ 0x0cf0 */
D_0cf2:	.space	2	/* bss word @ 0x0cf2 */
D_0cf4:	.space	2	/* bss word @ 0x0cf4 */
D_0cf6:	.space	2	/* bss word @ 0x0cf6 */
D_0cf8:	.space	2	/* bss word @ 0x0cf8 */
D_0cfa:	.space	2	/* bss word @ 0x0cfa */
D_0cfc:	.space	2	/* bss word @ 0x0cfc */
D_0cfe:	.space	2	/* bss word @ 0x0cfe */
D_0d00:	.space	2	/* bss word @ 0x0d00 */
D_0d02:	.space	2	/* bss word @ 0x0d02 */
D_0d04:	.space	2	/* bss word @ 0x0d04 */
D_0d06:	.space	2	/* bss word @ 0x0d06 */
D_0d08:	.space	2	/* bss word @ 0x0d08 */
D_0d0a:	.space	2	/* bss word @ 0x0d0a */
D_0d0c:	.space	2	/* bss word @ 0x0d0c */
D_0d0e:	.space	2	/* bss word @ 0x0d0e */
D_0d10:	.space	2	/* bss word @ 0x0d10 */
D_0d12:	.space	2	/* bss word @ 0x0d12 */
D_0d14:	.space	2	/* bss word @ 0x0d14 */
	.space	6
D_0d1c:	.space	2	/* bss word @ 0x0d1c */
	.space	510
D_0f1c:	.space	2	/* bss word @ 0x0f1c */
	.space	161
D_0fbf:	.space	2	/* bss word @ 0x0fbf */
	.space	67
D_1004:	.space	2	/* bss word @ 0x1004 */
	.space	10
D_1010:	.space	2	/* bss word @ 0x1010 */
	.space	266
D_111c:	.space	2	/* bss word @ 0x111c */
D_111e:	.space	2	/* bss word @ 0x111e */
D_1120:	.space	2	/* bss word @ 0x1120 */
D_1122:	/* endcore: initial break */

/* end of file */
