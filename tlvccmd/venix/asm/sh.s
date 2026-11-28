/* sh.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 sh.s -o sh.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* Wrapping near-call/jmp targets (16-bit address space) */
	.set L_000b, 0x000b	/* wrapping branch target */
	.set L_0400, 0x0400	/* wrapping branch target */
	.set L_083b, 0x083b	/* wrapping branch target */
	.set L_083e, 0x083e	/* wrapping branch target */
	.set L_95f2, 0x95f2	/* wrapping branch target */
	.set L_9e75, 0x9e75	/* wrapping branch target */
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: sh  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00004096  (16534 bytes)
//   a_data:   0x00000662  (1634 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

L_0036:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x6(%bp),%si
	cmpw   $0x1,0x4(%bp)
	jg   L_004a
	jmp   L_00f1
L_004a:
	mov    0x2(%si),%bx
	cmpb   $0x2d,(%bx)
	je   L_0055
	jmp   L_00f1
L_0055:
	mov    0x2(%si),%di
	andw   $0xe7ff,D_0670
L_005e:
	inc    %di
	cmpb   $0x0,(%di)
	jne   L_0067
	jmp   L_00e9
L_0067:
	movw   $D_0006,-0x6(%bp)
L_006c:
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_008c
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_008c
	incw   -0x6(%bp)
	jmp   L_006c
L_008c:
	mov    (%di),%al
	cbtw
	mov    -0x6(%bp),%bx
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_00b3
	mov    -0x6(%bp),%ax
	sub    $D_0006,%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_0012(%bx),%ax
	or     %ax,D_0670
L_00b1:
	jmp   L_005e
L_00b3:
	cmpb   $0x63,(%di)
	jne   L_00d7
	cmpw   $0x2,0x4(%bp)
	jle   L_00d7
	cmpw   $0x0,D_066c
	jne   L_00d7
	mov    0x4(%si),%ax
	mov    %ax,D_066c
	mov    (%si),%ax
	mov    %ax,0x2(%si)
	inc    %si
	inc    %si
	decw   0x4(%bp)
	jmp   L_00b1
L_00d7:
	mov    $D_0180,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	call   L_0ae1
	add    $0x4,%sp
	jmp   L_00b1
L_00e9:
	mov    (%si),%ax
	mov    %ax,0x2(%si)
	decw   0x4(%bp)
L_00f1:
	movw   $D_0006,-0x6(%bp)
	movw   $D_067a,-0x8(%bp)
L_00fb:
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_012b
	mov    -0x6(%bp),%ax
	sub    $D_0006,%ax
	mov    %ax,%bx
	shl    %bx
	mov    D_0670,%ax
	test   %ax,D_0012(%bx)
	je   L_0126
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
L_0126:
	incw   -0x6(%bp)
	jmp   L_00fb
L_012b:
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	mov    0x4(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_013e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_0148:
	mov    %di,%bx
	inc    %di
	inc    %di
	mov    (%bx),%ax
	or     %ax,%ax
	je   L_0155
	inc    %si
	jmp   L_0148
L_0155:
	pushw  D_0004
	call   L_017f
	pop    %cx
	push   %si
	pushw  0x4(%bp)
	call   L_01c4
	add    $0x4,%sp
	mov    %ax,D_0004
	lea    -0x1(%si),%ax
	mov    %ax,D_06a6
	push   %ax
	mov    $D_06c2,%ax
	push   %ax
	call   L_2014
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_017f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	xor    %si,%si
	mov    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_01bd
	mov    -0x6(%bp),%bx
	mov    (%bx),%si
	mov    -0x6(%bp),%bx
	decw   0x2(%bx)
	jne   L_01bd
	mov    -0x6(%bp),%ax
	add    $0x4,%ax
	mov    %ax,%di
L_01a6:
	mov    (%di),%ax
	or     %ax,%ax
	je   L_01b6
	pushw  (%di)
	call   L_037e
	pop    %cx
	inc    %di
	inc    %di
	jmp   L_01a6
L_01b6:
	pushw  -0x6(%bp)
	call   L_037e
	pop    %cx
L_01bd:
	mov    %si,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_01c4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x6(%bp),%ax
	shl    %ax
	add    $0x6,%ax
	push   %ax
	call   L_0262
	pop    %cx
	mov    %ax,%di
	mov    0x4(%bp),%si
	mov    %di,-0x6(%bp)
	movw   $0x1,0x2(%di)
	lea    0x4(%di),%ax
	mov    %ax,%di
	mov    %di,D_06ac
L_01ed:
	mov    0x6(%bp),%ax
	decw   0x6(%bp)
	or     %ax,%ax
	je   L_0209
	mov    %si,%bx
	inc    %si
	inc    %si
	pushw  (%bx)
	call   L_2031
	pop    %cx
	mov    %di,%bx
	inc    %di
	inc    %di
	mov    %ax,(%bx)
	jmp   L_01ed
L_0209:
	mov    %di,%bx
	inc    %di
	inc    %di
	movw   $0x0,(%bx)
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0219:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_021e:
	pushw  D_06a2
	call   L_017f
	pop    %cx
	mov    %ax,D_06a2
	or     %ax,%ax
	jne   L_021e
L_022d:
	call   L_1161
	or     %ax,%ax
	jne   L_022d
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0238:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_0004
	je   L_025c
	mov    D_0004,%bx
	incw   0x2(%bx)
	mov    D_0004,%bx
	mov    D_06a2,%ax
	mov    %ax,(%bx)
	mov    D_0004,%ax
	mov    %ax,D_06a2
	jmp   L_025e
L_025c:
	xor    %ax,%ax
L_025e:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0262:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	and    $0xfffe,%ax
	mov    %ax,%di
L_0275:
	movw   $0x0,-0x6(%bp)
	mov    D_0710,%si
L_027e:
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_02ca
L_0285:
	mov    (%si),%ax
	mov    %ax,-0x8(%bp)
	mov    %ax,%bx
	mov    (%bx),%ax
	test   $0x1,%ax
	jne   L_029c
	mov    -0x8(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,(%si)
	jmp   L_0285
L_029c:
	mov    -0x8(%bp),%ax
	sub    %si,%ax
	cmp    %di,%ax
	jb   L_02ca
	mov    %si,%ax
	add    %di,%ax
	mov    %ax,D_0710
	mov    -0x8(%bp),%ax
	cmp    D_0710,%ax
	jbe   L_02bd
	mov    D_0710,%bx
	mov    (%si),%ax
	mov    %ax,(%bx)
L_02bd:
	mov    D_0710,%ax
	or     $0x1,%ax
	mov    %ax,(%si)
	lea    0x2(%si),%ax
	jmp   L_02ea
L_02ca:
	mov    %si,-0x8(%bp)
	mov    (%si),%ax
	and    $0xfffe,%ax
	mov    %ax,%si
	cmp    -0x8(%bp),%si
	ja   L_027e
	mov    -0x6(%bp),%ax
	incw   -0x6(%bp)
	or     %ax,%ax
	je   L_027e
	push   %di
	call   L_02f1
	pop    %cx
	jmp   L_0275
L_02ea:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_02f1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_068a,%ax
	cmp    D_070e,%ax
	je   L_0332
	mov    D_070e,%bx
	movb   $0x0,(%bx)
	incw   D_070e
	mov    D_070e,%ax
	inc    %ax
	and    $0xfffe,%ax
	mov    %ax,%di
	mov    D_068a,%ax
	dec    %ax
	dec    %ax
	mov    %ax,%si
	mov    D_0672,%ax
	mov    %ax,(%si)
	mov    %si,D_0672
	mov    %di,%ax
	or     $0x1,%ax
	mov    D_002a,%bx
	mov    %ax,(%bx)
	mov    %di,D_002a
L_0332:
	mov    D_0028,%ax
	add    %ax,0x4(%bp)
	mov    D_0028,%ax
	dec    %ax
	not    %ax
	and    %ax,0x4(%bp)
	mov    D_002a,%ax
	mov    %ax,D_0710
	mov    D_002a,%ax
	add    0x4(%bp),%ax
	mov    D_002a,%bx
	mov    %ax,(%bx)
	mov    %ax,D_002a
	mov    %ax,%bx
	movw   $0x27ad,(%bx)
	mov    D_002a,%ax
	add    $0x4,%ax
	mov    %ax,%di
	push   %di
	pushw  D_061e
	call   L_2d24
	add    $0x4,%sp
	mov    %ax,D_070e
	mov    %di,D_061e
	mov    %di,D_068a
	pop    %di
	pop    %si
	pop    %bp
	ret


L_037e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	or     %di,%di
	je   L_0396
	cmp    D_002a,%di
	jae   L_0396
	dec    %di
	dec    %di
	andw   $0xfffe,(%di)
L_0396:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_039a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %ax,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_03a6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x6,%ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,%di
	mov    0x4(%bp),%ax
	or     $0xa,%ax
	mov    %ax,(%di)
	mov    0x6(%bp),%ax
	mov    %ax,0x4(%di)
	movw   $0x0,0x2(%di)
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_03ce:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,0x6(%bp)
	je   L_03df
	cmpw   $0x0,0x8(%bp)
	jne   L_03e4
L_03df:
	call   L_0a3a
	jmp   L_03ff
L_03e4:
	mov    $0x6,%ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,%di
	mov    0x4(%bp),%ax
	mov    %ax,(%di)
	mov    0x6(%bp),%ax
	mov    %ax,0x2(%di)
	mov    0x8(%bp),%ax
	mov    %ax,0x4(%di)
L_03ff:
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0405:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	pushw  0x6(%bp)
	call   L_04ae
	pop    %cx
	mov    %ax,%si
	cmpw   $0xa,D_06c4
	jne   L_0436
	testw  $0x1,0x6(%bp)
	je   L_0444
	movw   $0x3b,D_06c4
	mov    $0xa,%ax
	push   %ax
	call   L_1cbb
	pop    %cx
	jmp   L_0444
L_0436:
	or     %si,%si
	jne   L_0444
	testw  $0x2,0x6(%bp)
	jne   L_0444
	call   L_0a3a
L_0444:
	mov    D_06c4,%ax
	jmp   L_0496
L_0449:
	or     %si,%si
	je   L_045c
	push   %si
	mov    $0x70,%ax
	push   %ax
	call   L_03a6
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_045f
L_045c:
	call   L_0a3a
L_045f:
	mov    0x6(%bp),%ax
	or     $0x2,%ax
	push   %ax
	push   %di
	call   L_0405
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_04a7
	pushw  -0x6(%bp)
	push   %si
	mov    $0x3,%ax
	push   %ax
	call   L_03ce
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_04a7
L_0486:
	cmp    $0xa,%di
	je   L_04a7
L_048b:
	or     %di,%di
	je   L_04a7
	push   %di
	call   L_09ae
	pop    %cx
	jmp   L_04a7
L_0496:
	cmp    $0x26,%ax
	je   L_0449
	cmp    $0x3b,%ax
	je   L_045f
	cmp    $0x400,%ax
	je   L_0486
	jmp   L_048b
L_04a7:
	mov    %si,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_04ae:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_0504
	pop    %cx
	mov    %ax,%di
L_04bc:
	or     %di,%di
	je   L_04fe
	cmpw   $0x826,D_06c4
	jne   L_04cd
	mov    $0x1,%ax
	jmp   L_04cf
L_04cd:
	xor    %ax,%ax
L_04cf:
	mov    %ax,%si
	or     %ax,%ax
	jne   L_04dd
	cmpw   $L_087c,D_06c4
	jne   L_04fe
L_04dd:
	mov    $0x1,%ax
	push   %ax
	call   L_0504
	pop    %cx
	push   %ax
	push   %di
	or     %si,%si
	je   L_04f0
	mov    $0x8,%ax
	jmp   L_04f3
L_04f0:
	mov    $0x9,%ax
L_04f3:
	push   %ax
	call   L_03ce
	add    $0x6,%sp
	mov    %ax,%di
	jmp   L_04bc
L_04fe:
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0504:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	incb   D_0664
	testw  $0x1,0x4(%bp)
	je   L_0519
	call   L_08c2
	jmp   L_051c
L_0519:
	call   L_2ddc
L_051c:
	mov    $0xffff,%ax
	push   %ax
	call   L_95f2
	pop    %cx
	mov    %ax,%di
	or     %ax,%ax
	je   L_0564
	cmpw   $0x5e,D_06c4
	je   L_0538
	cmpw   $0x7c,D_06c4
	jne   L_0564
L_0538:
	mov    $0x1,%ax
	push   %ax
	call   L_0504
	pop    %cx
	push   %ax
	mov    $0x500,%ax
	push   %ax
	call   L_03a6
	add    $0x4,%sp
	push   %ax
	push   %di
	mov    $0x200,%ax
	push   %ax
	call   L_03a6
	add    $0x4,%sp
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_03ce
	add    $0x6,%sp
	jmp   L_0566
L_0564:
	mov    %di,%ax
L_0566:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_056a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	call   L_08c2
	cmp    %di,D_06c4
	jne   L_0580
	xor    %ax,%ax
	jmp   L_05ee
L_0580:
	mov    $0x6,%ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,%si
	movw   $0x0,(%si)
L_058e:
	mov    D_0686,%bx
	mov    (%si),%ax
	mov    %ax,(%bx)
	mov    D_0686,%ax
	mov    %ax,(%si)
	cmpw   $0x0,D_06c4
	jne   L_05b1
	call   L_2ddc
	cmp    $0x29,%ax
	je   L_05b4
	cmpw   $0x7c,D_06c4
	je   L_05b4
L_05b1:
	call   L_0a3a
L_05b4:
	cmpw   $0x7c,D_06c4
	jne   L_05c0
	call   L_2ddc
	jmp   L_058e
L_05c0:
	mov    $0x3,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_0405
	add    $0x4,%sp
	mov    %ax,0x2(%si)
	cmpw   $L_083b,D_06c4
	jne   L_05e2
	push   %di
	call   L_056a
	pop    %cx
	mov    %ax,0x4(%si)
	jmp   L_05ec
L_05e2:
	push   %di
	call   L_09ae
	pop    %cx
	movw   $0x0,0x4(%si)
L_05ec:
	mov    %si,%ax
L_05ee:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_05f2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	cmpb   $0x0,0x4(%bp)
	je   L_060b
	xor    %ax,%ax
	push   %ax
	call   L_08e4
	pop    %cx
	mov    %ax,%si
	jmp   L_060d
L_060b:
	xor    %si,%si
L_060d:
	mov    D_06c4,%ax
	jmp   L_0855
L_0613:
	mov    $0x6,%ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,%di
	call   L_099b
	mov    D_0686,%ax
	inc    %ax
	inc    %ax
	mov    %ax,0x2(%di)
	call   L_08c2
	mov    $0x10e,%ax
	push   %ax
	call   L_09ae
	pop    %cx
	cmpw   $0x10a,D_06c4
	jne   L_0640
	mov    $0x122,%ax
	jmp   L_0643
L_0640:
	mov    $0x128,%ax
L_0643:
	push   %ax
	call   L_056a
	pop    %cx
	mov    %ax,0x4(%di)
	movw   $0x7,(%di)
	jmp   L_0739
L_0652:
	mov    $0x8,%ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,%di
	movw   $0x4,(%di)
	mov    $0x1,%ax
	push   %ax
	mov    $0x124,%ax
	push   %ax
	call   L_0405
	add    $0x4,%sp
	mov    %ax,0x2(%di)
	mov    $0x1,%ax
	push   %ax
	mov    $0x113,%ax
	push   %ax
	call   L_0405
	add    $0x4,%sp
	mov    %ax,0x4(%di)
	mov    D_06c4,%ax
	mov    %ax,-0x6(%bp)
	cmp    $0x111,%ax
	jne   L_069d
	mov    $0x1,%ax
	push   %ax
	mov    $0x110,%ax
	push   %ax
	call   L_0405
	add    $0x4,%sp
	jmp   L_06b5
L_069d:
	cmpw   $0x112,-0x6(%bp)
	jne   L_06b3
	movw   $0x11e,D_06c4
	xor    %ax,%ax
	push   %ax
	call   L_05f2
	pop    %cx
	jmp   L_06b5
L_06b3:
	xor    %ax,%ax
L_06b5:
	mov    %ax,0x6(%di)
	cmpw   $0x112,-0x6(%bp)
	jne   L_0739
	mov    %di,%ax
	jmp   L_07ca
L_06c4:
	mov    $0x8,%ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,%di
	movw   $0xb,(%di)
	movw   $0x0,0x6(%di)
	call   L_099b
	mov    D_0686,%ax
	inc    %ax
	inc    %ax
	mov    %ax,0x4(%di)
	call   L_08c2
	cmp    $0x10a,%ax
	jne   L_0713
	call   L_099b
	xor    %ax,%ax
	push   %ax
	call   L_05f2
	pop    %cx
	mov    %ax,0x6(%di)
	cmpw   $0xa,D_06c4
	je   L_0708
	cmpw   $0x3b,D_06c4
	je   L_0708
	call   L_0a3a
L_0708:
	pushw  D_06c4
	call   L_1cbb
	pop    %cx
	call   L_08c2
L_0713:
	mov    $0x107,%ax
	push   %ax
	call   L_09ae
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	cmpw   $0x105,D_06c4
	jne   L_072c
	mov    $0x121,%ax
	jmp   L_072f
L_072c:
	mov    $0x128,%ax
L_072f:
	push   %ax
	call   L_0405
	add    $0x4,%sp
	mov    %ax,0x2(%di)
L_0739:
	jmp   L_0779
L_073b:
	mov    $0x6,%ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,%di
	cmpw   $0x11b,D_06c4
	jne   L_0752
	mov    $0x5,%ax
	jmp   L_0755
L_0752:
	mov    $0x6,%ax
L_0755:
	mov    %ax,(%di)
	mov    $0x1,%ax
	push   %ax
	mov    $0x105,%ax
	push   %ax
	call   L_0405
	add    $0x4,%sp
	mov    %ax,0x2(%di)
	mov    $0x1,%ax
	push   %ax
	mov    $0x121,%ax
	push   %ax
	call   L_0405
	add    $0x4,%sp
	mov    %ax,0x4(%di)
L_0779:
	jmp   L_07c1
L_077b:
	mov    $0x1,%ax
	push   %ax
	mov    $0x128,%ax
	push   %ax
	call   L_0405
	add    $0x4,%sp
	mov    %ax,%di
	jmp   L_07c1
L_078d:
	mov    $0x4,%ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    $0x1,%ax
	push   %ax
	mov    $0x29,%ax
	push   %ax
	call   L_0405
	add    $0x4,%sp
	mov    -0x6(%bp),%bx
	mov    %ax,0x2(%bx)
	mov    -0x6(%bp),%bx
	movw   $0x1,(%bx)
	pushw  -0x6(%bp)
	xor    %ax,%ax
	push   %ax
	call   L_03a6
	add    $0x4,%sp
	mov    %ax,%di
L_07c1:
	jmp   L_0898
L_07c4:
	or     %si,%si
	jne   L_07cd
	xor    %ax,%ax
L_07ca:
	jmp   L_0853
L_07cd:
	movw   $0x0,-0xa(%bp)
	movw   $0x1,-0xc(%bp)
	mov    $0x8,%ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,%di
	mov    %si,0x2(%di)
	lea    0x4(%di),%ax
	mov    %ax,-0x8(%bp)
L_07ea:
	cmpw   $0x0,D_06c4
	jne   L_0840
	mov    D_0686,%ax
	mov    %ax,-0x6(%bp)
	cmpw   $0x0,D_06ee
	je   L_0814
	cmpw   $0x0,-0xc(%bp)
	je   L_0814
	mov    -0x6(%bp),%bx
	mov    -0xa(%bp),%ax
	mov    %ax,(%bx)
	mov    -0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
	jmp   L_082b
L_0814:
	mov    -0x8(%bp),%bx
	mov    -0x6(%bp),%ax
	mov    %ax,(%bx)
	mov    -0x6(%bp),%ax
	mov    %ax,-0x8(%bp)
	mov    D_0670,%ax
	and    $D_0000,%ax
	mov    %ax,-0xc(%bp)
L_082b:
	call   L_2ddc
	cmpb   $0x0,0x4(%bp)
	je   L_07ea
	pushw  0x2(%di)
	call   L_08e4
	pop    %cx
	mov    %ax,0x2(%di)
	jmp   L_07ea
L_0840:
	movw   $0x0,(%di)
	mov    -0xa(%bp),%ax
	mov    %ax,0x6(%di)
	mov    -0x8(%bp),%bx
	movw   $0x0,(%bx)
	mov    %di,%ax
L_0853:
	jmp   L_08bb
L_0855:
	cmp    $0x10f,%ax
	jne   L_085d
	jmp   L_0613
L_085d:
	jg   L_0879
	or     %ax,%ax
	jne   L_0866
	jmp   L_07cd
L_0866:
	cmp    $0x28,%ax
	jne   L_086e
	jmp   L_078d
L_086e:
	cmp    $0x106,%ax
	jne   L_0876
	jmp   L_077b
L_0876:
	jmp   L_07c4
L_0879:
	cmp    $0x117,%ax
L_087c:
	jne   L_0881
	jmp   L_073b
L_0881:
	cmp    $0x11b,%ax
	je   L_087c
	cmp    $0x11d,%ax
	jne   L_088e
	jmp   L_06c4
L_088e:
	cmp    $0x11e,%ax
	jne   L_0896
	jmp   L_0652
L_0896:
	jmp   L_0876
L_0898:
	incb   D_0664
	call   L_2ddc
	push   %si
	call   L_08e4
	pop    %cx
	mov    %ax,%si
	or     %ax,%ax
	je   L_08b9
	push   %di
	xor    %ax,%ax
	push   %ax
	call   L_03a6
	add    $0x4,%sp
	mov    %ax,%di
	mov    %si,0x2(%di)
L_08b9:
	mov    %di,%ax
L_08bb:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_08c2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_08c7:
	incb   D_0664
	call   L_2ddc
	cmp    $0xa,%ax
	jne   L_08dd
	mov    $0xa,%ax
	push   %ax
	call   L_1cbb
	pop    %cx
	jmp   L_08c7
L_08dd:
	mov    D_06c4,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_08e4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    D_06a0,%di
	mov    D_06c4,%ax
	jmp   L_094a
L_08f3:
	or     $0x10,%di
	jmp   L_0943
L_08f9:
	cmpw   $0x0,D_06a0
	jne   L_0904
	or     $0x1,%di
L_0904:
	or     $0x20,%di
	cmpw   $L_083e,D_06c4
	jne   L_0916
	or     $0x40,%di
	jmp   L_0960
L_0916:
	xor    %ax,%ax
	push   %ax
	call   L_3069
	pop    %cx
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	cmp    $0x26,%cl
	jne   L_092d
	or     $0x80,%di
	jmp   L_0960
L_092d:
	cmpb   $0x3e,-0x5(%bp)
	jne   L_0939
	or     $0x190,%di
	jmp   L_0960
L_0939:
	mov    -0x5(%bp),%al
	cbtw
	or     $0x8000,%ax
	mov    %ax,D_0666
L_0943:
	jmp   L_0960
L_0945:
	mov    0x4(%bp),%ax
	jmp   L_0996
L_094a:
	cmp    $0x3c,%ax
	je   L_0916
	cmp    $0x3e,%ax
L_0952:
	je   L_08f9
	cmp    $0x83c,%ax
	je   L_08f3
	cmp    $0x83e,%ax
	je   L_0952
	jmp   L_0945
L_0960:
	call   L_099b
	mov    $0x8,%ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,%si
	mov    D_0686,%ax
	inc    %ax
	inc    %ax
	mov    %ax,0x2(%si)
	mov    %di,(%si)
	test   $0x10,%di
	je   L_0987
	mov    D_066a,%ax
	mov    %ax,0x6(%si)
	mov    %si,D_066a
L_0987:
	call   L_2ddc
	pushw  0x4(%bp)
	call   L_08e4
	pop    %cx
	mov    %ax,0x4(%si)
	mov    %si,%ax
L_0996:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_099b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_2ddc
	or     %ax,%ax
	je   L_09aa
	call   L_0a3a
L_09aa:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_09ae:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	and    D_06c4,%ax
	mov    %ax,%di
	test   $0x100,%di
	je   L_09c6
	mov    %di,%ax
	jmp   L_09c9
L_09c6:
	mov    0x4(%bp),%ax
L_09c9:
	cmp    D_06c4,%ax
	je   L_09d2
	call   L_0a3a
L_09d2:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_09d6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	testw  $0x100,0x4(%bp)
	je   L_0a00
	mov    $D_035c,%di
L_09e5:
	cmpw   $0x0,0x2(%di)
	je   L_09f8
	mov    0x2(%di),%ax
	cmp    0x4(%bp),%ax
	je   L_09f8
	add    $0x4,%di
	jmp   L_09e5
L_09f8:
	pushw  (%di)
	call   L_233a
	pop    %cx
	jmp   L_0a36
L_0a00:
	cmpw   $L_0400,0x4(%bp)
	jne   L_0a11
	mov    $D_0316,%ax
	push   %ax
	call   L_233a
	pop    %cx
	jmp   L_0a36
L_0a11:
	testw  $0x800,0x4(%bp)
	je   L_0a1f
	pushw  0x4(%bp)
	call   L_235c
	pop    %cx
L_0a1f:
	cmpw   $0xa,0x4(%bp)
	jne   L_0a2f
	mov    $D_002c,%ax
	push   %ax
	call   L_233a
	pop    %cx
	jmp   L_0a36
L_0a2f:
	pushw  0x4(%bp)
	call   L_235c
	pop    %cx
L_0a36:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a3a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_2312
	mov    $D_01a8,%ax
	push   %ax
	call   L_233a
	pop    %cx
	testw  $0x20,D_0670
	jne   L_0a65
	mov    $D_032e,%ax
	push   %ax
	call   L_233a
	pop    %cx
	mov    D_0152,%bx
	pushw  0x2(%bx)
	call   L_241f
	pop    %cx
L_0a65:
	mov    $D_0310,%ax
	push   %ax
	call   L_233a
	pop    %cx
	mov    $0x60,%ax
	push   %ax
	call   L_235c
	pop    %cx
	cmpw   $0x0,D_06c4
	je   L_0a86
	pushw  D_06c4
	call   L_09d6
	pop    %cx
	jmp   L_0a90
L_0a86:
	mov    D_0686,%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_233a
	pop    %cx
L_0a90:
	mov    $0x27,%ax
	push   %ax
	call   L_235c
	pop    %cx
	mov    $D_0322,%ax
	push   %ax
	call   L_233a
	pop    %cx
	call   L_22f0
	mov    $0x2,%ax
	push   %ax
	call   L_0b29
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0ab0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  D_06c0
	mov    $D_06ae,%ax
	push   %ax
	call   L_2014
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
	mov    D_0674,%al
	cbtw
	test   $0x4,%ax
	je   L_0add
	mov    $0x3,%ax
	push   %ax
	call   L_0b29
	pop    %cx
L_0add:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ae1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_2312
	pushw  0x4(%bp)
	call   L_233a
	pop    %cx
	cmpw   $0x0,0x6(%bp)
	je   L_0b05
	mov    $D_0310,%ax
	push   %ax
	call   L_233a
	pop    %cx
	pushw  0x6(%bp)
	call   L_233a
	pop    %cx
L_0b05:
	call   L_22f0
	mov    $0x1,%ax
	push   %ax
	call   L_0b29
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0b14:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0ae1
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0b29:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	mov    %ax,D_06c0
	mov    D_0670,%ax
	and    $0x70,%ax
	cmp    $0x20,%ax
	je   L_0b44
	call   L_0b59
	jmp   L_0b55
L_0b44:
	call   L_0219
	mov    $0x1,%ax
	push   %ax
	mov    $D_0692,%ax
	push   %ax
	call   L_3bea
	add    $0x4,%sp
L_0b55:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0b59:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_06ca,%di
	or     %di,%di
	je   L_0b76
	movw   $0x0,D_06ca
	xor    %ax,%ax
	push   %ax
	push   %di
	call   L_3aa7
	add    $0x4,%sp
L_0b76:
	xor    %ax,%ax
	push   %ax
	call   L_0b89
	pop    %cx
	pushw  D_06c0
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0b89:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0b8e:
	mov    D_06aa,%ax
	cmp    0x4(%bp),%ax
	jbe   L_0bad
	mov    D_06aa,%bx
	pushw  0x2(%bx)
	call   L_4038
	pop    %cx
	mov    D_06aa,%bx
	mov    0x6(%bx),%ax
	mov    %ax,D_06aa
	jmp   L_0b8e
L_0bad:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0bb2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x3e,%sp
	movb   $0x0,-0x9(%bp)
	movw   $0x0,-0xc(%bp)
	mov    D_066e,%ax
	mov    %ax,-0xe(%bp)
	mov    D_0674,%al
	cbtw
	test   $0x4,%ax
	je   L_0bd6
	xor    %ax,%ax
	jmp   L_0c10
L_0bd6:
	mov    0x4(%bp),%si
	mov    %si,%di
	movb   $0x0,-0x10(%bp)
	movb   $0x0,-0x3f(%bp)
L_0be3:
	mov    (%si),%al
	cbtw
	test   $0x80,%ax
	jne   L_0bfa
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	mov    D_00b4(%bx),%al
	cbtw
	test   $0x10,%ax
	jne   L_0c1d
L_0bfa:
	mov    %si,%bx
	inc    %si
	cmpb   $0x0,(%bx)
	jne   L_0c13
	cmpw   $0x0,0x6(%bp)
	je   L_0c0e
	cmpb   $0x0,-0x3f(%bp)
	jne   L_0c1d
L_0c0e:
	xor    %ax,%ax
L_0c10:
	jmp   L_0d71
L_0c13:
	cmpb   $0x2f,(%si)
	jne   L_0be3
	incb   -0x3f(%bp)
	jmp   L_0be3
L_0c1d:
	cmp    %di,%si
	jne   L_0c26
	mov    $D_02fa,%di
	jmp   L_0c36
L_0c26:
	dec    %si
	cmpb   $0x2f,(%si)
	jne   L_0c1d
	movb   $0x0,(%si)
	cmp    %si,%di
	jne   L_0c36
	mov    $D_0134,%di
L_0c36:
	lea    -0x3e(%bp),%ax
	push   %ax
	push   %di
	call   L_3fc2
	add    $0x4,%sp
	or     %ax,%ax
	jl   L_0c64
	mov    -0x3a(%bp),%ax
	and    $0xe000,%ax
	cmp    $0xc000,%ax
	jne   L_0c64
	xor    %ax,%ax
	push   %ax
	push   %di
	call   L_3f92
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_0c64
	incb   -0x9(%bp)
L_0c64:
	movw   $0x0,-0x6(%bp)
	cmpb   $0x0,(%si)
	jne   L_0c74
	mov    %si,%bx
	inc    %si
	movb   $0x80,(%bx)
L_0c74:
	cmpb   $0x0,-0x9(%bp)
	jne   L_0c7d
	jmp   L_0d02
L_0c7d:
	mov    %si,-0x40(%bp)
L_0c80:
	mov    -0x40(%bp),%bx
	cmpb   $0x2f,(%bx)
	jne   L_0c9a
	mov    -0x40(%bp),%ax
	mov    %ax,-0xc(%bp)
	mov    -0x40(%bp),%bx
	movb   $0x0,(%bx)
	movw   $0x0,D_066e
L_0c9a:
	mov    -0x40(%bp),%bx
	incw   -0x40(%bp)
	cmpb   $0x0,(%bx)
	jne   L_0c80
L_0ca5:
	mov    $0x10,%ax
	push   %ax
	lea    -0x20(%bp),%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_3fa8
	add    $0x6,%sp
	cmp    $0x10,%ax
	jne   L_0cf7
	mov    D_0674,%al
	cbtw
	test   $0x4,%ax
	jne   L_0cf7
	cmpw   $0x0,-0x20(%bp)
	je   L_0ca5
	cmpb   $0x2e,-0x1e(%bp)
	jne   L_0cd5
	cmpb   $0x2e,(%si)
	jne   L_0ca5
L_0cd5:
	push   %si
	lea    -0x1e(%bp),%ax
	push   %ax
	call   L_0d78
	add    $0x4,%sp
	or     %ax,%ax
	je   L_0ca5
	pushw  -0xc(%bp)
	lea    -0x1e(%bp),%ax
	push   %ax
	push   %di
	call   L_0e94
	add    $0x6,%sp
	incw   -0x6(%bp)
	jmp   L_0ca5
L_0cf7:
	pushw  -0x8(%bp)
	call   L_3f7e
	pop    %cx
	cmpw   $0x0,-0xc(%bp)
L_0d02:
	je   L_0d44
	mov    D_066e,%ax
	mov    %ax,-0x42(%bp)
	mov    -0xe(%bp),%ax
	mov    %ax,D_066e
	cmpw   $0x0,-0x6(%bp)
	je   L_0d3e
	movw   $0x0,-0x6(%bp)
L_0d1b:
	cmpw   $0x0,-0x42(%bp)
	je   L_0d3e
	mov    $0x1,%ax
	push   %ax
	mov    -0x42(%bp),%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_0bb2
	add    $0x4,%sp
	add    %ax,-0x6(%bp)
	mov    -0x42(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x42(%bp)
	jmp   L_0d1b
L_0d3e:
	mov    -0xc(%bp),%bx
	movb   $0x2f,(%bx)
L_0d44:
	mov    0x4(%bp),%di
L_0d47:
	mov    (%di),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x3f(%bp)
	or     %cl,%cl
	je   L_0d6e
	mov    -0x3f(%bp),%al
	cbtw
	test   $0x7f,%ax
	je   L_0d62
	mov    -0x3f(%bp),%al
	cbtw
	jmp   L_0d65
L_0d62:
	mov    $0x2f,%ax
L_0d65:
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_0d47
L_0d6e:
	mov    -0x6(%bp),%ax
L_0d71:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0d78:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_0da1
	andw   $0x7f,-0x6(%bp)
	jne   L_0da1
	movw   $0x80,-0x6(%bp)
L_0da1:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x7(%bp)
	mov    %cl,%al
	cbtw
	jmp   L_9e75
L_0db2:
	movb   $0x0,-0x9(%bp)
	movw   $0x7fff,-0xc(%bp)
L_0dbb:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x7(%bp)
	or     %cl,%cl
	je   L_0e16
	cmpb   $0x5d,-0x7(%bp)
	jne   L_0de4
	cmpb   $0x0,-0x9(%bp)
	je   L_0de0
	push   %si
	push   %di
	call   L_0d78
	add    $0x4,%sp
	jmp   L_0e18
L_0de0:
	xor    %ax,%ax
	jmp   L_0e28
L_0de4:
	cmpb   $0x2d,-0x7(%bp)
	jne   L_0e02
	mov    -0xc(%bp),%ax
	cmp    -0x6(%bp),%ax
	jg   L_0dbb
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	cmp    %ax,-0x6(%bp)
	jg   L_0dbb
	incb   -0x9(%bp)
L_0e00:
	jmp   L_0dbb
L_0e02:
	mov    -0x7(%bp),%al
	cbtw
	and    $0x7f,%ax
	mov    %ax,-0xc(%bp)
	cmp    %ax,-0x6(%bp)
	jne   L_0e00
	incb   -0x9(%bp)
	jmp   L_0e00
L_0e16:
	xor    %ax,%ax
L_0e18:
	jmp   L_0e46
L_0e1a:
	mov    -0x7(%bp),%al
	cbtw
	and    $0x7f,%ax
	cmp    -0x6(%bp),%ax
	je   L_0e2a
	xor    %ax,%ax
L_0e28:
	jmp   L_0e64
L_0e2a:
	cmpw   $0x0,-0x6(%bp)
	je   L_0e3a
	push   %si
	push   %di
	call   L_0d78
	add    $0x4,%sp
	jmp   L_0e73
L_0e3a:
	xor    %ax,%ax
	jmp   L_0e73
L_0e3e:
	cmpb   $0x0,(%si)
	jne   L_0e48
	mov    $0x1,%ax
L_0e46:
	jmp   L_0e8d
L_0e48:
	dec    %di
L_0e49:
	cmpb   $0x0,(%di)
	je   L_0e62
	push   %si
	mov    %di,%ax
	inc    %di
	push   %ax
	call   L_0d78
	add    $0x4,%sp
	or     %ax,%ax
	je   L_0e49
	mov    $0x1,%ax
	jmp   L_0e8d
L_0e62:
	xor    %ax,%ax
L_0e64:
	jmp   L_0e8d
L_0e66:
	cmpw   $0x0,-0x6(%bp)
	jne   L_0e71
	mov    $0x1,%ax
	jmp   L_0e8d
L_0e71:
	xor    %ax,%ax
L_0e73:
	jmp   L_0e8d
	or     %ax,%ax
	je   L_0e66
	cmp    $0x2a,%ax
	je   L_0e3e
	cmp    $0x3f,%ax
	je   L_0e2a
	cmp    $0x5b,%ax
	jne   L_0e8b
	jmp   L_0db2
L_0e8b:
	jmp   L_0e1a
L_0e8d:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e94:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	call   L_2c4e
	inc    %ax
	inc    %ax
	mov    %ax,%si
	mov    0x4(%bp),%di
L_0ea4:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_0ecc
	andw   $0x7f,-0x6(%bp)
	jne   L_0ec0
	mov    %si,%bx
	inc    %si
	movb   $0x2f,(%bx)
	jmp   L_0ecc
L_0ec0:
	mov    %si,%bx
	inc    %si
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_0ea4
L_0ecc:
	mov    0x6(%bp),%di
L_0ecf:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,(%si)
	or     %cl,%cl
	je   L_0ee0
	inc    %si
	jmp   L_0ecf
L_0ee0:
	mov    0x8(%bp),%di
	or     %di,%di
	je   L_0efc
	mov    %si,%bx
	inc    %si
	movb   $0x2f,(%bx)
L_0eed:
	inc    %di
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0eed
L_0efc:
	push   %si
	call   L_2c88
	pop    %cx
	push   %ax
	call   L_0f0b
	pop    %cx
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f0b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    D_066e,%ax
	mov    %ax,(%di)
	mov    %di,D_066e
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xf22,%ax
	push   %ax
	push   %di
	call   L_3c4a
	add    $0x4,%sp
	cmp    $0xb,%di
	jne   L_0f52
	pushw  D_0028
	call   L_2c12
	pop    %cx
	cmp    $0xffff,%ax
	jne   L_0f85
	mov    $D_019e,%ax
	push   %ax
	call   L_0b14
	pop    %cx
	jmp   L_0f85
L_0f52:
	cmp    $0xe,%di
	jne   L_0f64
	testw  $0x200,D_0670
	je   L_0f85
	call   L_0b59
	jmp   L_0f85
L_0f64:
	mov    %di,%bx
	shl    %bx
	cmpw   $0x0,D_06ca(%bx)
	je   L_0f74
	mov    $0x2,%ax
	jmp   L_0f77
L_0f74:
	mov    $0x4,%ax
L_0f77:
	mov    %ax,%si
	mov    %si,%cx
	or     %cl,D_0674
	mov    %si,%cx
	or     %cl,D_06f2(%di)
L_0f85:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f89:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x3,%ax
	push   %ax
	call   L_0fb2
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	call   L_0fd9
	pop    %cx
	mov    $0xb,%ax
	push   %ax
	call   L_0fd9
	pop    %cx
	mov    $0xe,%ax
	push   %ax
	call   L_0fd9
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0fb2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x1,%ax
	push   %ax
	mov    0x4(%bp),%si
	push   %si
	call   L_3c4a
	add    $0x4,%sp
	and    $0x1,%ax
	mov    %ax,%di
	or     %ax,%ax
	jne   L_0fd3
	orb    $0x8,D_06f2(%si)
L_0fd3:
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0fd9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    D_06f2(%di),%al
	cbtw
	test   $0x8,%ax
	jne   L_0ff4
	push   %di
	call   L_0fb2
	pop    %cx
	or     %ax,%ax
	jne   L_0fff
L_0ff4:
	mov    $0xf22,%ax
	push   %ax
	push   %di
	call   L_3c4a
	add    $0x4,%sp
L_0fff:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1003:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x11,%di
L_100b:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_102f
	mov    %di,%bx
	shl    %bx
	mov    D_06ca(%bx),%si
	or     %si,%si
	je   L_1023
	cmpb   $0x0,(%si)
	je   L_1028
L_1023:
	push   %di
	call   L_1038
	pop    %cx
L_1028:
	movb   $0x0,D_06f2(%di)
	jmp   L_100b
L_102f:
	movb   $0x0,D_0674
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1038:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%bx
	shl    %bx
	pushw  D_06ca(%bx)
	call   L_037e
	pop    %cx
	mov    0x4(%bp),%bx
	shl    %bx
	movw   $0x0,D_06ca(%bx)
	mov    0x4(%bp),%bx
	mov    D_06f2(%bx),%al
	cbtw
	test   $0x8,%ax
	je   L_1077
	mov    $0xf22,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_3c4a
	add    $0x4,%sp
	mov    0x4(%bp),%bx
	andb   $0xf7,D_06f2(%bx)
L_1077:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_107b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    $0x11,%di
	andb   $0xfd,D_0674
L_1089:
	dec    %di
	je   L_10c2
	mov    D_06f2(%di),%al
	cbtw
	test   $0x2,%ax
	je   L_1089
	andb   $0xfd,D_06f2(%di)
	mov    %di,%bx
	shl    %bx
	mov    D_06ca(%bx),%si
	or     %si,%si
	je   L_1089
	mov    D_06c0,%ax
	mov    %ax,-0x6(%bp)
	xor    %ax,%ax
	push   %ax
	push   %si
	call   L_3aa7
	add    $0x4,%sp
	mov    -0x6(%bp),%ax
	mov    %ax,D_06c0
	call   L_0ab0
	jmp   L_1089
L_10c2:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_10c8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_0152,%di
	mov    0x4(%bp),%ax
	mov    %ax,(%di)
	testw  $0xa0,D_0670
	jne   L_10e3
	mov    $0x40,%ax
	jmp   L_10e6
L_10e3:
	mov    $0x1,%ax
L_10e6:
	mov    %ax,%cx
	mov    %cl,0x5(%di)
	lea    0xe(%di),%ax
	mov    %ax,0x8(%di)
	mov    %ax,0x6(%di)
	movw   $0x0,0xa(%di)
	movw   $0x1,0x2(%di)
	movb   $0x0,0x4(%di)
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1106:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    D_0152,%si
	movw   $0xffff,(%si)
	push   %di
	call   L_2dbe
	pop    %cx
	mov    %di,0x6(%si)
	mov    %di,%dx
	add    %ax,%dx
	mov    %dx,0x8(%si)
	movw   $0x1,0x2(%si)
	or     %di,%di
	jne   L_1134
	mov    $0x1,%ax
	jmp   L_1136
L_1134:
	xor    %ax,%ax
L_1136:
	mov    %ax,%cx
	mov    %cl,0x4(%si)
	mov    %cl,%al
	cbtw
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1142:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    D_0152,%ax
	mov    %ax,0xc(%di)
	movb   $0x0,0x4(%di)
	movw   $0x0,0xa(%di)
	mov    %di,D_0152
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1161:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_0152,%di
	cmpw   $0x0,0xc(%di)
	je   L_1186
	cmpw   $0x0,(%di)
	jl   L_117b
	pushw  (%di)
	call   L_3f7e
	pop    %cx
L_117b:
	mov    0xc(%di),%ax
	mov    %ax,D_0152
	mov    $0xffff,%ax
	jmp   L_1188
L_1186:
	xor    %ax,%ax
L_1188:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_118c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_3b76
	pop    %cx
	or     %ax,%ax
	jl   L_11ad
	mov    0x4(%bp),%bx
	cmpw   $0x0,(%bx)
	jl   L_11ad
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x2(%bx)
	jge   L_11b5
L_11ad:
	mov    $D_020c,%ax
	push   %ax
	call   L_0b14
	pop    %cx
L_11b5:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_11b9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_3f92
	add    $0x4,%sp
	mov    %ax,%di
	or     %ax,%ax
	jge   L_11df
	mov    $D_021e,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0ae1
	add    $0x4,%sp
	jmp   L_11e1
L_11df:
	mov    %di,%ax
L_11e1:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_11e5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	cmp    %si,%di
	je   L_1212
	push   %si
	mov    %di,%ax
	or     $0x40,%ax
	push   %ax
	call   L_3b10
	add    $0x4,%sp
	push   %di
	call   L_3f7e
	pop    %cx
	or     %si,%si
	jne   L_1212
	orw    $0x1,D_070c
L_1212:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1216:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_3b60
	add    $0x4,%sp
	mov    %ax,%di
	or     %ax,%ax
	jge   L_123d
	mov    $D_01e6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0ae1
	add    $0x4,%sp
	jmp   L_123f
L_123d:
	mov    %di,%ax
L_123f:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1243:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_06a4,%ax
	incw   D_06a4
	push   %ax
	call   L_2437
	pop    %cx
	pushw  D_06ec
	mov    $D_0706,%ax
	push   %ax
	call   L_2d24
	add    $0x4,%sp
	mov    $D_013e,%ax
	push   %ax
	call   L_1216
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_126e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_1283
	jmp   L_1351
L_1283:
	mov    -0xc(%bp),%bx
	pushw  0x6(%bx)
	call   L_126e
	pop    %cx
	mov    -0xc(%bp),%bx
	pushw  0x2(%bx)
	call   L_2a02
	pop    %cx
	mov    %ax,-0x8(%bp)
	cmpb   $0x0,D_0712
	je   L_12a8
	mov    -0xc(%bp),%bx
	andw   $0xffef,(%bx)
L_12a8:
	call   L_1243
	mov    %ax,-0xa(%bp)
	mov    $D_013e,%ax
	push   %ax
	call   L_2d08
	pop    %cx
	mov    -0xc(%bp),%bx
	mov    %ax,0x2(%bx)
	mov    -0xc(%bp),%bx
	mov    D_06aa,%ax
	mov    %ax,0x6(%bx)
	mov    -0xc(%bp),%ax
	mov    %ax,D_06aa
	call   L_2c4e
	mov    %ax,%di
L_12d0:
	mov    %di,%si
	mov    $0xa,%ax
	push   %ax
	call   L_1cbb
	pop    %cx
L_12da:
	cmpb   $0x0,D_0712
	je   L_12e6
	call   L_30f3
	jmp   L_12f1
L_12e6:
	mov    -0x8(%bp),%bx
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_3069
	pop    %cx
L_12f1:
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	mov    %cl,%al
	cbtw
	test   $0x80,%ax
	jne   L_130e
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_0034(%bx),%al
	cbtw
	test   $0x30,%ax
	jne   L_1318
L_130e:
	mov    %si,%bx
	inc    %si
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	jmp   L_12da
L_1318:
	movb   $0x0,(%si)
	mov    D_0152,%bx
	cmpb   $0x0,0x4(%bx)
	jne   L_134a
	pushw  -0x8(%bp)
	push   %di
	call   L_2d7f
	add    $0x4,%sp
	or     %ax,%ax
	je   L_134a
	mov    %si,%bx
	inc    %si
	movb   $0xa,(%bx)
	mov    %si,%ax
	sub    %di,%ax
	push   %ax
	push   %di
	pushw  -0xa(%bp)
	call   L_4068
	add    $0x6,%sp
	jmp   L_12d0
L_134a:
	pushw  -0xa(%bp)
	call   L_3f7e
	pop    %cx
L_1351:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1358:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_135e:
	mov    0x4(%bp),%al
	cbtw
	push   %ax
	call   L_1448
	pop    %cx
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	mov    %cl,%al
	cbtw
	mov    0x4(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_13a1
	cmpb   $0x0,-0x5(%bp)
	je   L_13a1
	mov    D_0136,%al
	cbtw
	mov    -0x5(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	or     %ax,%dx
	mov    D_070e,%bx
	mov    %dx,%cx
	mov    %cl,(%bx)
	incw   D_070e
	jmp   L_135e
L_13a1:
	mov    D_070e,%bx
	movb   $0x0,(%bx)
	mov    -0x5(%bp),%al
	cbtw
	mov    0x4(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_13c2
	mov    $D_01d4,%ax
	push   %ax
	call   L_0b14
	pop    %cx
L_13c2:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_13c7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_13cd:
	call   L_30f3
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	cmp    $0x0,%cl
	je   L_13ea
	mov    -0x5(%bp),%al
	cbtw
	mov    0x4(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
L_13ea:
	je   L_1429
	mov    -0x5(%bp),%al
	cbtw
	jmp   L_1418
L_13f2:
	mov    $0x60,%ax
	push   %ax
	call   L_13c7
	pop    %cx
L_13fa:
	jmp   L_13cd
L_13fc:
	mov    $0x22,%ax
	push   %ax
	call   L_13c7
	pop    %cx
	jmp   L_13cd
L_1406:
	call   L_30f3
	cmp    $0x7b,%ax
	jne   L_13cd
	mov    $0x7d,%ax
	push   %ax
	call   L_13c7
	pop    %cx
	jmp   L_13fa
L_1418:
	cmp    $0x22,%ax
	je   L_13fc
	cmp    $0x24,%ax
	je   L_1406
	cmp    $0x60,%ax
	je   L_13f2
	jmp   L_13fa
L_1429:
	mov    -0x5(%bp),%al
	cbtw
	mov    0x4(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_1443
	mov    $D_01d4,%ax
	push   %ax
	call   L_0b14
	pop    %cx
L_1443:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1448:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xe,%sp
L_1450:
	call   L_30f3
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	mov    %cl,%al
	cbtw
	test   $0x80,%ax
	jne   L_1470
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_0034(%bx),%al
	cbtw
	test   $0x41,%ax
	jne   L_1477
L_1470:
	mov    -0x5(%bp),%al
	cbtw
	jmp   L_1763
L_1477:
	cmpb   $0x24,-0x5(%bp)
	je   L_1480
	jmp   L_174d
L_1480:
	call   L_30f3
	mov    %ax,%di
	test   $0x80,%di
	je   L_148e
	jmp   L_1743
L_148e:
	mov    D_00b4(%di),%al
	cbtw
	test   $0x6d,%ax
	jne   L_149b
	jmp   L_1743
L_149b:
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0xa(%bp)
	lea    -0x10(%bp),%ax
	mov    %ax,-0x12(%bp)
	cmp    $0x7b,%di
	jne   L_14b5
	mov    $0x1,%ax
	jmp   L_14b7
L_14b5:
	xor    %ax,%ax
L_14b7:
	mov    %ax,%cx
	mov    %cl,-0xb(%bp)
	or     %cl,%cl
	je   L_14c5
	call   L_30f3
	mov    %ax,%di
L_14c5:
	test   $0x80,%di
	jne   L_153b
	mov    D_00b4(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_153b
	mov    D_070e,%ax
	sub    D_061e,%ax
	mov    %ax,%si
L_14de:
	test   $0x80,%di
	jne   L_1501
	mov    D_00b4(%di),%al
	cbtw
	test   $0x48,%ax
	je   L_1501
	mov    D_070e,%bx
	mov    %di,%cx
	mov    %cl,(%bx)
	incw   D_070e
	call   L_30f3
	mov    %ax,%di
	jmp   L_14de
L_1501:
	mov    D_070e,%bx
	movb   $0x0,(%bx)
	mov    %si,%ax
	add    D_061e,%ax
	push   %ax
	call   L_205e
	pop    %cx
	mov    %ax,-0x8(%bp)
	mov    %si,%ax
	add    D_061e,%ax
	mov    %ax,D_070e
	mov    -0x8(%bp),%bx
	mov    0x6(%bx),%ax
	mov    %ax,-0xe(%bp)
	mov    -0x8(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,-0x12(%bp)
	mov    %di,%ax
	or     $0x8000,%ax
	mov    %ax,D_0666
	jmp   L_1598
L_153b:
	test   $0x80,%di
	jne   L_159a
	mov    D_00b4(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_159a
	mov    -0x12(%bp),%bx
	mov    %di,%cx
	mov    %cl,(%bx)
	movb   $0x0,-0xf(%bp)
	test   $0x80,%di
	jne   L_156e
	mov    D_00b4(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_156e
	movw   $0x1,-0xa(%bp)
	mov    $0x31,%di
L_156e:
	sub    $0x30,%di
	or     %di,%di
	jne   L_157a
	mov    D_068c,%ax
	jmp   L_1595
L_157a:
	cmp    D_06a6,%di
	jg   L_158e
	mov    %di,%ax
	shl    %ax
	add    D_06ac,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	jmp   L_1595
L_158e:
	movw   $0x0,-0xa(%bp)
	xor    %ax,%ax
L_1595:
	mov    %ax,-0xe(%bp)
L_1598:
	jmp   L_15eb
L_159a:
	cmp    $0x24,%di
	jne   L_15a7
	mov    D_0684,%ax
	mov    %ax,-0xe(%bp)
	jmp   L_15eb
L_15a7:
	cmp    $0x21,%di
	jne   L_15b4
	mov    D_069e,%ax
	mov    %ax,-0xe(%bp)
	jmp   L_15eb
L_15b4:
	cmp    $0x23,%di
	jne   L_15c1
	mov    D_06c2,%ax
	mov    %ax,-0xe(%bp)
	jmp   L_15eb
L_15c1:
	cmp    $0x3f,%di
	jne   L_15ce
	mov    D_06ae,%ax
	mov    %ax,-0xe(%bp)
	jmp   L_15eb
L_15ce:
	cmp    $0x2d,%di
	jne   L_15da
	movw   $D_067a,-0xe(%bp)
	jmp   L_15eb
L_15da:
	cmpb   $0x0,-0xb(%bp)
L_15de:
	jne   L_15e3
	jmp   L_1450
L_15e3:
	mov    $D_01d4,%ax
	push   %ax
	call   L_0b14
	pop    %cx
L_15eb:
	call   L_30f3
	mov    %ax,%di
	test   $0x80,%di
	jne   L_1600
	mov    D_00b4(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_160e
L_1600:
	cmpb   $0x0,-0xb(%bp)
	je   L_160e
	mov    $D_01d4,%ax
	push   %ax
	call   L_0b14
	pop    %cx
L_160e:
	xor    %si,%si
	cmpb   $0x0,-0xb(%bp)
	je   L_1668
	cmp    $0x7d,%di
	je   L_1666
	mov    D_070e,%ax
	sub    D_061e,%ax
	mov    %ax,%si
	test   $0x80,%di
	jne   L_1639
	mov    D_00b4(%di),%al
	cbtw
	test   $0x80,%ax
	je   L_1639
	mov    $0x1,%ax
	jmp   L_163b
L_1639:
	xor    %ax,%ax
L_163b:
	cmpw   $0x0,-0xe(%bp)
	jne   L_1646
	mov    $0x1,%dx
	jmp   L_1648
L_1646:
	xor    %dx,%dx
L_1648:
	xor    %ax,%dx
	je   L_1656
	mov    $0x7d,%ax
	push   %ax
	call   L_1358
	pop    %cx
	jmp   L_165e
L_1656:
	mov    $0x7d,%ax
	push   %ax
	call   L_13c7
	pop    %cx
L_165e:
	mov    %si,%ax
	add    D_061e,%ax
	mov    %ax,%si
L_1666:
	jmp   L_1672
L_1668:
	mov    %di,%ax
	or     $0x8000,%ax
	mov    %ax,D_0666
	xor    %di,%di
L_1672:
	cmpw   $0x0,-0xe(%bp)
	je   L_16e9
	cmp    $0x2b,%di
L_167b:
	jne   L_1680
	jmp   L_15de
L_1680:
	mov    -0xe(%bp),%bx
	incw   -0xe(%bp)
	mov    (%bx),%al
	cbtw
	mov    %ax,%di
	or     %ax,%ax
	je   L_16a3
	mov    D_0136,%al
	cbtw
	or     %di,%ax
	mov    D_070e,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   D_070e
L_16a1:
	jmp   L_1680
L_16a3:
	cmpw   $0x0,-0xa(%bp)
	je   L_167b
	incw   -0xa(%bp)
	mov    -0xa(%bp),%ax
	cmp    D_06a6,%ax
	jle   L_16b8
	jmp   L_1450
L_16b8:
	mov    -0xa(%bp),%ax
	shl    %ax
	add    D_06ac,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,-0xe(%bp)
	mov    -0x12(%bp),%bx
	cmpb   $0x2a,(%bx)
	jne   L_16d6
	mov    D_0136,%al
	cbtw
	jmp   L_16d8
L_16d6:
	xor    %ax,%ax
L_16d8:
	or     $0x20,%ax
	mov    D_070e,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   D_070e
	jmp   L_16a1
L_16e9:
	or     %si,%si
	je   L_172c
	cmp    $0x3f,%di
	jne   L_170b
	cmpb   $0x0,(%si)
	je   L_16fb
	mov    %si,%ax
	jmp   L_16fe
L_16fb:
	mov    $D_01c2,%ax
L_16fe:
	push   %ax
	pushw  -0x12(%bp)
	call   L_0ae1
	add    $0x4,%sp
L_1708:
	jmp   L_1450
L_170b:
	cmp    $0x3d,%di
	jne   L_1708
	cmpw   $0x0,-0x8(%bp)
	je   L_1722
	push   %si
	pushw  -0x8(%bp)
	call   L_1e73
	add    $0x4,%sp
	jmp   L_1708
L_1722:
	mov    $D_01d4,%ax
	push   %ax
	call   L_0b14
	pop    %cx
L_172a:
	jmp   L_1708
L_172c:
	testw  $0x8,D_0670
	je   L_1708
	mov    $D_01c2,%ax
	push   %ax
	pushw  -0x12(%bp)
	call   L_0ae1
	add    $0x4,%sp
L_1741:
	jmp   L_1708
L_1743:
	mov    %di,%ax
	or     $0x8000,%ax
	mov    %ax,D_0666
	jmp   L_1781
L_174d:
	mov    -0x5(%bp),%al
	cbtw
	mov    0x4(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_1765
	mov    -0x5(%bp),%al
	cbtw
L_1763:
	jmp   L_1785
L_1765:
	cmpb   $0x60,-0x5(%bp)
	jne   L_1770
	call   L_17fc
	jmp   L_172a
L_1770:
	cmpb   $0x22,-0x5(%bp)
	jne   L_1781
	incb   D_0138
	xorb   $0x80,D_0136
	jmp   L_1741
L_1781:
	mov    -0x5(%bp),%al
	cbtw
L_1785:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_178c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x14,%sp
	mov    D_0138,%al
	mov    %al,-0x5(%bp)
	mov    D_0136,%al
	mov    %al,-0x7(%bp)
	lea    -0x18(%bp),%ax
	push   %ax
	call   L_1142
	pop    %cx
	pushw  0x4(%bp)
	call   L_1106
	pop    %cx
	call   L_2c4e
	movb   $0x0,D_0136
	movb   $0x0,D_0138
	xor    %ax,%ax
	push   %ax
	call   L_1358
	pop    %cx
	call   L_1161
	cmpb   $0x0,D_0138
	je   L_17e1
	mov    D_061e,%ax
	cmp    D_070e,%ax
	jne   L_17e1
	mov    D_070e,%bx
	movb   $0x80,(%bx)
	incw   D_070e
L_17e1:
	mov    -0x7(%bp),%al
	mov    %al,D_0136
	mov    -0x5(%bp),%al
	mov    %al,D_0138
	pushw  D_070e
	call   L_2c88
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_17fc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x54,%sp
	pushw  D_070e
	call   L_2c88
	pop    %cx
	mov    %ax,%di
	call   L_2c4e
L_1811:
	call   L_30f3
	mov    %ax,%cx
	mov    %cl,-0x53(%bp)
	cmp    $0x60,%cl
	je   L_1833
	cmpb   $0x0,-0x53(%bp)
	je   L_1833
	mov    D_070e,%bx
	mov    -0x53(%bp),%al
	mov    %al,(%bx)
	incw   D_070e
	jmp   L_1811
L_1833:
	pushw  D_070e
	call   L_2c88
	pop    %cx
	mov    %ax,%si
	push   %ax
	call   L_29b9
	pop    %cx
	lea    -0x52(%bp),%ax
	push   %ax
	call   L_1142
	pop    %cx
	push   %si
	call   L_1106
	pop    %cx
	mov    $0x3,%ax
	push   %ax
	mov    $0x400,%ax
	push   %ax
	call   L_0405
	add    $0x4,%sp
	push   %ax
	mov    $0x200,%ax
	push   %ax
	call   L_03a6
	add    $0x4,%sp
	mov    %ax,%si
	lea    -0x58(%bp),%ax
	push   %ax
	call   L_118c
	pop    %cx
	pushw  -0x58(%bp)
	call   L_10c8
	pop    %cx
	lea    -0x58(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	push   %si
	call   L_31f8
	add    $0x8,%sp
	pushw  -0x56(%bp)
	call   L_3f7e
	pop    %cx
	push   %di
	call   L_2cad
	pop    %cx
	pushw  D_061e
	push   %di
	call   L_2d24
	add    $0x4,%sp
	mov    %ax,D_070e
L_18a4:
	call   L_30f3
	mov    %ax,%cx
	mov    %cl,-0x53(%bp)
	or     %cl,%cl
	je   L_18ce
	mov    D_0136,%al
	cbtw
	mov    -0x53(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	or     %ax,%dx
	mov    D_070e,%bx
	mov    %dx,%cx
	mov    %cl,(%bx)
	incw   D_070e
	jmp   L_18a4
L_18ce:
	xor    %ax,%ax
	push   %ax
	call   L_28a5
	pop    %cx
L_18d5:
	mov    D_061e,%ax
	cmp    D_070e,%ax
	je   L_18f5
	decw   D_070e
	mov    D_070e,%bx
	mov    (%bx),%al
	cbtw
	and    $0x7f,%ax
	cmp    $0xa,%ax
	je   L_18d5
	incw   D_070e
L_18f5:
	call   L_1161
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_18ff:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x50,%sp
	mov    $0x200,%di
	lea    -0x54(%bp),%ax
	push   %ax
	call   L_1142
	pop    %cx
	pushw  0x4(%bp)
	call   L_10c8
	pop    %cx
L_1919:
	mov    $0x22,%ax
	push   %ax
	call   L_1448
	pop    %cx
	and    $0x7f,%ax
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	je   L_1949
	mov    D_070e,%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   D_070e
	dec    %di
	jne   L_1919
	pushw  0x6(%bp)
	call   L_195a
	pop    %cx
	mov    $0x200,%di
	jmp   L_1919
L_1949:
	pushw  0x6(%bp)
	call   L_195a
	pop    %cx
	call   L_1161
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_195a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_070e,%ax
	sub    D_061e,%ax
	push   %ax
	pushw  D_061e
	pushw  0x4(%bp)
	call   L_4068
	add    $0x6,%sp
	testw  $0x800,D_0670
	je   L_1992
	mov    D_070e,%ax
	sub    D_061e,%ax
	push   %ax
	pushw  D_061e
	pushw  D_013a
	call   L_4068
	add    $0x6,%sp
L_1992:
	mov    D_061e,%ax
	mov    %ax,D_070e
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
	mov    $0x20,%di
	call   L_0f89
	mov    $0x200,%ax
	push   %ax
	call   L_2c12
	pop    %cx
	xor    %ax,%ax
	push   %ax
	call   L_02f1
	pop    %cx
	call   L_225c
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_0036
	add    $0x4,%sp
	mov    %ax,D_06a6
	cmp    $0x2,%ax
	jge   L_19d3
	orw    $0x400,D_0670
L_19d3:
	testw  $0x400,D_0670
	jne   L_19df
	decw   D_06a6
L_19df:
	mov    0x4(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    D_06a6,%dx
	shl    %dx
	sub    %dx,%ax
	mov    %ax,D_06ac
	decw   D_06a6
	mov    $D_06b2,%ax
	push   %ax
	call   L_3bbc
	pop    %cx
	pushw  D_06a6
	mov    $D_06c2,%ax
	push   %ax
	call   L_2014
	add    $0x4,%sp
	mov    D_06ac,%bx
	mov    (%bx),%ax
	mov    %ax,D_068c
	call   L_3b50
	push   %ax
	mov    $D_0684,%ax
	push   %ax
	call   L_2014
	add    $0x4,%sp
	call   L_1ce4
	mov    $D_02fc,%ax
	push   %ax
	mov    $D_05ac,%ax
	push   %ax
	call   L_1e55
	add    $0x4,%sp
	incb   D_013c
	mov    D_013c,%al
	cbtw
	dec    %ax
	jne   L_1a8c
	mov    D_068c,%bx
	cmpb   $0x2d,(%bx)
	jne   L_1a6b
	mov    $D_0352,%ax
	push   %ax
	mov    $D_02fa,%ax
	push   %ax
	call   L_2684
	add    $0x4,%sp
	mov    D_0152,%bx
	mov    %ax,(%bx)
	or     %ax,%ax
	jl   L_1a6b
	push   %di
	call   L_1aba
	pop    %cx
	andw   $0xffdf,D_0670
L_1a6b:
	or     %di,%di
	jne   L_1a75
	orw    $0x100,D_0670
L_1a75:
	cmpw   $0x0,D_066c
	je   L_1a8e
	pushw  D_066c
	call   L_1106
	pop    %cx
	mov    D_0152,%bx
	movw   $0xffff,(%bx)
L_1a8c:
	jmp   L_1aac
L_1a8e:
	testw  $0x400,D_0670
	je   L_1a9a
	xor    %ax,%ax
	jmp   L_1aa2
L_1a9a:
	pushw  D_068c
	call   L_11b9
	pop    %cx
L_1aa2:
	mov    D_0152,%bx
	mov    %ax,(%bx)
	decw   D_066c
L_1aac:
	xor    %ax,%ax
	push   %ax
	call   L_1aba
	pop    %cx
	call   L_0b59
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1aba:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x22,%sp
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
	mov    D_0152,%bx
	cmpw   $0x0,(%bx)
	jle   L_1aed
	mov    $0xa,%ax
	push   %ax
	mov    D_0152,%bx
	pushw  (%bx)
	call   L_1d0c
	add    $0x4,%sp
	mov    D_0152,%bx
	movw   $0xa,(%bx)
L_1aed:
	cmpw   $0x2,D_013a
	jne   L_1b0d
	mov    $0xb,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3b10
	pop    %cx
	push   %ax
	call   L_1d0c
	add    $0x4,%sp
	movw   $L_000b,D_013a
L_1b0d:
	call   L_3b40
	mov    %ax,%di
	testw  $0x2,D_0670
	jne   L_1b48
	testw  $0x80,D_0670
	jne   L_1b46
	lea    -0x26(%bp),%ax
	push   %ax
	pushw  D_013a
	call   L_3ff1
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_1b7d
	lea    -0x26(%bp),%ax
	push   %ax
	mov    D_0152,%bx
	pushw  (%bx)
	call   L_3ff1
	add    $0x4,%sp
	or     %ax,%ax
L_1b46:
	jne   L_1b7d
L_1b48:
	or     %di,%di
	je   L_1b51
	mov    $D_034a,%ax
	jmp   L_1b54
L_1b51:
	mov    $D_034e,%ax
L_1b54:
	push   %ax
	mov    $D_05b8,%ax
	push   %ax
	call   L_1e55
	add    $0x4,%sp
	mov    $D_0346,%ax
	push   %ax
	mov    $D_0588,%ax
	push   %ax
	call   L_1e55
	add    $0x4,%sp
	orw    $0x24,D_0670
	mov    $0xf,%ax
	push   %ax
	call   L_0fb2
	pop    %cx
	jmp   L_1b8b
L_1b7d:
	mov    0x4(%bp),%al
	cbtw
	or     %ax,D_0670
	andw   $0xfffb,D_0670
L_1b8b:
	mov    $D_0692,%ax
	push   %ax
	call   L_3bbc
	pop    %cx
	or     %ax,%ax
	je   L_1baa
	cmpb   $0x0,0x4(%bp)
	je   L_1baa
	mov    D_0152,%bx
	pushw  (%bx)
	call   L_3f7e
	pop    %cx
	jmp   L_1cb4
L_1baa:
	movw   $0x0,D_0666
	movw   $0x0,D_06a8
	movw   $0x0,D_06be
	movw   $0x0,D_066a
	mov    D_0152,%bx
	cmpw   $0x0,(%bx)
	jl   L_1bd5
	mov    D_0152,%bx
	pushw  (%bx)
	call   L_10c8
	pop    %cx
L_1bd5:
	xor    %ax,%ax
	push   %ax
	call   L_2cad
	pop    %cx
	call   L_2ceb
	call   L_0ab0
	testw  $0x4,D_0670
	je   L_1c68
	mov    D_0152,%bx
	cmpw   $0x0,0xc(%bx)
	jne   L_1bfc
	mov    D_0152,%bx
	cmpb   $0x0,0x4(%bx)
L_1bfc:
	jne   L_1c68
	cmpw   $0x0,D_05d6
	je   L_1c47
	lea    -0x26(%bp),%ax
	push   %ax
	pushw  D_05d6
	call   L_3fc2
	add    $0x4,%sp
	or     %ax,%ax
	jl   L_1c47
	cmpw   $0x0,-0x16(%bp)
	jne   L_1c21
	cmpw   $0x0,-0x18(%bp)
L_1c21:
	je   L_1c47
	mov    -0x10(%bp),%ax
	mov    -0xe(%bp),%dx
	cmp    -0x6(%bp),%dx
	jne   L_1c33
	cmp    -0x8(%bp),%ax
	je   L_1c47
L_1c33:
	cmpw   $0x0,-0x6(%bp)
	jne   L_1c3d
	cmpw   $0x0,-0x8(%bp)
L_1c3d:
	je   L_1c47
	mov    $D_018e,%ax
	push   %ax
	call   L_233a
	pop    %cx
L_1c47:
	mov    -0x10(%bp),%ax
	mov    -0xe(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	pushw  D_05be
	call   L_233a
	pop    %cx
	xor    %ax,%ax
	push   %ax
	call   L_3b98
	pop    %cx
	orw    $0x200,D_0670
L_1c68:
	movb   $0x0,D_0674
	call   L_30f3
	mov    %ax,D_0666
	mov    D_0152,%bx
	cmpb   $0x0,0x4(%bx)
	jne   L_1cb4
	xor    %ax,%ax
	push   %ax
	call   L_3b98
	pop    %cx
	andw   $0xfdff,D_0670
	xor    %ax,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	mov    $0xa,%ax
	push   %ax
	call   L_0405
	add    $0x4,%sp
	push   %ax
	call   L_31f8
	add    $0x4,%sp
	mov    D_0670,%ax
	and    $0x80,%ax
	mov    D_0152,%bx
	mov    %ax,%cx
	or     %cl,0x4(%bx)
	jmp   L_1bd5
L_1cb4:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1cbb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	testw  $0x4,D_0670
	je   L_1ce0
	mov    D_0152,%bx
	cmpw   $0x0,0xc(%bx)
	jne   L_1ce0
	cmpb   $0xa,0x4(%bp)
	jne   L_1ce0
	pushw  D_058e
	call   L_233a
	pop    %cx
L_1ce0:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ce4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_3b50
	push   %ax
	call   L_2437
	pop    %cx
	movw   $0x0,D_06a4
	mov    $D_0145,%ax
	push   %ax
	mov    $D_0706,%ax
	push   %ax
	call   L_2d24
	add    $0x4,%sp
	mov    %ax,D_06ec
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d0c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	push   %si
	mov    %di,%ax
	or     $0x40,%ax
	push   %ax
	call   L_3b10
	add    $0x4,%sp
	push   %di
	call   L_3f7e
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1d30:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x6(%bp),%si
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	mov    %al,-0x5(%bp)
L_1d41:
	mov    (%si),%di
	or     %di,%di
	je   L_1d70
	mov    -0x5(%bp),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_1d6b
	push   %di
	pushw  0x4(%bp)
	call   L_2d7f
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_1d6b
	mov    0x2(%si),%ax
	jmp   L_1d72
L_1d6b:
	add    $0x4,%si
	jmp   L_1d41
L_1d70:
	xor    %ax,%ax
L_1d72:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d77:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
L_1d80:
	or     %di,%di
	je   L_1db5
	lea    0x2(%di),%ax
	push   %ax
	call   L_2a02
	pop    %cx
	mov    %ax,%si
	pushw  0x6(%bp)
	push   %si
	call   L_1db9
	add    $0x4,%sp
	mov    (%di),%di
	testw  $0x800,D_0670
	je   L_1d80
	push   %si
	call   L_233a
	pop    %cx
	or     %di,%di
	je   L_1db0
	call   L_2301
	jmp   L_1d80
L_1db0:
	call   L_22f0
	jmp   L_1d80
L_1db5:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1db9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    (%di),%al
	cbtw
	test   $0x80,%ax
	jne   L_1df5
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	mov    D_00b4(%bx),%al
	cbtw
	test   $0x40,%ax
	je   L_1e28
L_1dd8:
	mov    (%di),%al
	cbtw
	test   $0x80,%ax
	jne   L_1df2
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	mov    D_00b4(%bx),%al
	cbtw
	test   $0x48,%ax
	je   L_1df2
	inc    %di
	jmp   L_1dd8
L_1df2:
	cmpb   $0x3d,(%di)
L_1df5:
	jne   L_1e28
	movb   $0x0,(%di)
	pushw  0x4(%bp)
	call   L_205e
	pop    %cx
	mov    %ax,%si
	mov    %di,%bx
	inc    %di
	movb   $0x3d,(%bx)
	mov    0x6(%bp),%ax
	or     %ax,0xa(%si)
	testw  $D_0000,0x6(%bp)
	je   L_1e1e
	mov    %di,0x6(%si)
	mov    %di,0x8(%si)
	jmp   L_1e35
L_1e1e:
	push   %di
	push   %si
	call   L_1e73
	add    $0x4,%sp
	jmp   L_1e35
L_1e28:
	mov    $D_02bc,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0ae1
	add    $0x4,%sp
L_1e35:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e39:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	pushw  (%di)
	call   L_037e
	pop    %cx
	pushw  0x6(%bp)
	call   L_2031
	pop    %cx
	mov    %ax,(%di)
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e55:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x6(%bx)
	jne   L_1e6f
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_1e73
	add    $0x4,%sp
L_1e6f:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e73:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%bx
	mov    0xa(%bx),%ax
	test   $0x8000,%ax
	je   L_1e95
	mov    $D_02ae,%ax
	push   %ax
	mov    0x4(%bp),%bx
	pushw  0x4(%bx)
	call   L_0ae1
	add    $0x4,%sp
	jmp   L_1ea5
L_1e95:
	pushw  0x6(%bp)
	mov    0x4(%bp),%ax
	add    $0x6,%ax
	push   %ax
	call   L_1e39
	add    $0x4,%sp
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
	sub    $0x54,%sp
	lea    -0x52(%bp),%ax
	mov    %ax,%di
	xor    %si,%si
	mov    0x4(%bp),%bx
	addw   $0x2,0x4(%bp)
	pushw  (%bx)
	call   L_205e
	pop    %cx
	mov    %ax,-0x56(%bp)
	mov    D_070e,%ax
	sub    D_061e,%ax
	mov    %ax,-0x58(%bp)
	push   %di
	call   L_1142
	pop    %cx
	xor    %ax,%ax
	push   %ax
	call   L_3b10
	pop    %cx
	push   %ax
	call   L_10c8
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_3d6a
	add    $0x8,%sp
	cmp    $0xffff,%ax
	jne   L_1eff
	movb   $0x1,0x5(%di)
L_1eff:
	xor    %ax,%ax
	push   %ax
	call   L_3069
	pop    %cx
	mov    %ax,%cx
	mov    %cl,-0x53(%bp)
	mov    0x4(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_1f26
	pushw  D_05b2
	mov    -0x53(%bp),%al
	cbtw
	push   %ax
	call   L_2d49
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_1f3f
L_1f26:
	mov    -0x53(%bp),%al
	cbtw
	test   $0x80,%ax
	jne   L_1fa1
	mov    -0x53(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_0034(%bx),%al
	cbtw
	test   $0x30,%ax
	je   L_1fa1
L_1f3f:
	mov    D_070e,%bx
	movb   $0x0,(%bx)
	mov    -0x58(%bp),%ax
	add    D_061e,%ax
	push   %ax
	pushw  -0x56(%bp)
	call   L_1e73
	add    $0x4,%sp
	mov    -0x58(%bp),%ax
	add    D_061e,%ax
	mov    %ax,D_070e
	mov    0x4(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_1f7b
	mov    0x4(%bp),%bx
	addw   $0x2,0x4(%bp)
	pushw  (%bx)
	call   L_205e
	pop    %cx
	mov    %ax,-0x56(%bp)
	jmp   L_1f80
L_1f7b:
	movw   $0x0,-0x56(%bp)
L_1f80:
	mov    -0x53(%bp),%al
	cbtw
	test   $0x80,%ax
	je   L_1f8c
	jmp   L_1eff
L_1f8c:
	mov    -0x53(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_0034(%bx),%al
	cbtw
	test   $0x30,%ax
	jne   L_1f9f
	jmp   L_1eff
L_1f9f:
	jmp   L_1fb1
L_1fa1:
	mov    D_070e,%bx
	mov    -0x53(%bp),%al
	mov    %al,(%bx)
	incw   D_070e
	jmp   L_1eff
L_1fb1:
	cmpw   $0x0,-0x56(%bp)
	je   L_1fe5
	mov    $D_02fa,%ax
	push   %ax
	pushw  -0x56(%bp)
	call   L_1e73
	add    $0x4,%sp
	mov    0x4(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_1fde
	mov    0x4(%bp),%bx
	addw   $0x2,0x4(%bp)
	pushw  (%bx)
	call   L_205e
	pop    %cx
	mov    %ax,-0x56(%bp)
	jmp   L_1fb1
L_1fde:
	movw   $0x0,-0x56(%bp)
	jmp   L_1fb1
L_1fe5:
	mov    D_0152,%bx
	cmpb   $0x0,0x4(%bx)
	je   L_1ff2
	mov    $0x1,%si
L_1ff2:
	mov    $0x1,%ax
	push   %ax
	mov    0x6(%di),%ax
	sub    0x8(%di),%ax
	cwtd
	push   %dx
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_3d6a
	add    $0x8,%sp
	call   L_1161
	mov    %si,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2014:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	call   L_2437
	pop    %cx
	mov    $D_0706,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1e39
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2031:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,0x4(%bp)
	je   L_2058
	pushw  0x4(%bp)
	call   L_2dbe
	pop    %cx
	push   %ax
	call   L_0262
	pop    %cx
	mov    %ax,%di
	push   %ax
	pushw  0x4(%bp)
	call   L_2d24
	add    $0x4,%sp
	mov    %di,%ax
	jmp   L_205a
L_2058:
	xor    %ax,%ax
L_205a:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_205e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    D_05dc,%si
	push   %di
	call   L_20eb
	pop    %cx
	or     %ax,%ax
	jne   L_2082
	mov    $D_02bc,%ax
	push   %ax
	push   %di
	call   L_0ae1
	add    $0x4,%sp
L_2082:
	or     %si,%si
	je   L_20b3
	pushw  0x4(%si)
	push   %di
	call   L_2d7f
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jne   L_209b
	mov    %si,%ax
	jmp   L_20e4
L_209b:
	cmpw   $0x0,-0x8(%bp)
	jge   L_20a6
	mov    %si,-0x6(%bp)
	jmp   L_20ac
L_20a6:
	lea    0x2(%si),%ax
	mov    %ax,-0x6(%bp)
L_20ac:
	mov    -0x6(%bp),%bx
	mov    (%bx),%si
	jmp   L_2082
L_20b3:
	mov    $0xc,%ax
	push   %ax
	call   L_0262
	pop    %cx
	mov    %ax,%si
	movw   $0x0,0x2(%si)
	movw   $0x0,(%si)
	push   %di
	call   L_2031
	pop    %cx
	mov    %ax,0x4(%si)
	movw   $0x0,0x6(%si)
	movw   $0x0,0xa(%si)
	movw   $0x0,0x8(%si)
	mov    -0x6(%bp),%bx
	mov    %si,(%bx)
	mov    %si,%ax
L_20e4:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_20eb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    (%di),%al
	cbtw
	test   $0x80,%ax
	jne   L_210a
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	mov    D_00b4(%bx),%al
	cbtw
	test   $0x40,%ax
	jne   L_210e
L_210a:
	xor    %ax,%ax
	jmp   L_2132
L_210e:
	inc    %di
	cmpb   $0x0,(%di)
	je   L_212f
	mov    (%di),%al
	cbtw
	test   $0x80,%ax
	jne   L_212b
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	mov    D_00b4(%bx),%al
	cbtw
	test   $0x48,%ax
	jne   L_210e
L_212b:
	xor    %ax,%ax
	jmp   L_2132
L_212f:
	mov    $0xffff,%ax
L_2132:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2136:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	mov    %ax,D_05de
	pushw  D_05dc
	call   L_214d
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_214d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	or     %di,%di
	je   L_216d
	pushw  (%di)
	call   L_214d
	pop    %cx
	push   %di
	/* TODO: indirect call - verify */
	call   *0x25de
	pop    %cx
	pushw  0x2(%di)
	call   L_214d
	pop    %cx
L_216d:
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_0ac7
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%di
	or     %di,%di
	je   L_219d
	mov    0x4(%bp),%bx
	pushw  0x4(%bx)
	call   L_233a
	pop    %cx
	mov    $0x3d,%ax
	push   %ax
	call   L_235c
	pop    %cx
	push   %di
	call   L_233a
	pop    %cx
	call   L_22f0
L_219d:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_21a1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	pushw  D_070e
	pushw  0x4(%di)
	call   L_2d24
	add    $0x4,%sp
	mov    %ax,%si
	push   %si
	mov    $D_05e4,%ax
	push   %ax
	call   L_2d24
	add    $0x4,%sp
	mov    %ax,%si
	push   %si
	pushw  0x6(%di)
	call   L_2d24
	add    $0x4,%sp
	mov    %ax,%si
	lea    0x1(%si),%ax
	sub    D_061e,%ax
	push   %ax
	call   L_2c2c
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	testw  $0x4000,0xa(%di)
	je   L_2205
	pushw  0x8(%di)
	call   L_037e
	pop    %cx
	pushw  0x6(%di)
	call   L_2031
	pop    %cx
	mov    %ax,0x8(%di)
	jmp   L_2216
L_2205:
	pushw  0x6(%di)
	call   L_037e
	pop    %cx
	pushw  0x8(%di)
	call   L_2031
	pop    %cx
	mov    %ax,0x6(%di)
L_2216:
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	testw  $0x4000,0xa(%di)
	je   L_2235
	mov    $D_03be,%ax
	push   %ax
	call   L_233a
	pop    %cx
	call   L_2301
L_2235:
	testw  $0x8000,0xa(%di)
	je   L_2247
	mov    $D_03c6,%ax
	push   %ax
	call   L_233a
	pop    %cx
	call   L_2301
L_2247:
	testw  $0xc000,0xa(%di)
	je   L_2258
	pushw  0x4(%di)
	call   L_233a
	pop    %cx
	call   L_22f0
L_2258:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_225c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_0662,%di
L_2265:
	cmpw   $0x0,(%di)
	je   L_227c
	mov    $D_0000,%ax
	push   %ax
	mov    %di,%bx
	inc    %di
	inc    %di
	pushw  (%bx)
	call   L_1db9
	add    $0x4,%sp
	jmp   L_2265
L_227c:
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	incw   D_05e0
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x6(%bx)
	je   L_22ad
	pushw  0x4(%bp)
	call   L_21a1
	pop    %cx
	mov    D_05e2,%bx
	mov    %ax,(%bx)
	addw   $0x2,D_05e2
L_22ad:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_22b1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	movw   $0x0,D_05e0
	mov    $D_0280,%ax
	push   %ax
	call   L_2136
	pop    %cx
	mov    D_05e0,%ax
	shl    %ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,%di
	mov    %ax,D_05e2
	mov    $D_028d,%ax
	push   %ax
	call   L_2136
	pop    %cx
	mov    D_05e2,%bx
	movw   $0x0,(%bx)
	addw   $0x2,D_05e2
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_22f0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0xa,%ax
	push   %ax
	call   L_235c
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2301:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x20,%ax
	push   %ax
	call   L_235c
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2312:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	testw  $0x4,D_0670
	jne   L_2336
	cmpw   $0x0,D_068c
	je   L_2336
	pushw  D_068c
	call   L_233a
	pop    %cx
	mov    $D_0310,%ax
	push   %ax
	call   L_233a
	pop    %cx
L_2336:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_233a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	or     %di,%di
	je   L_2358
	push   %di
	call   L_2dbe
	pop    %cx
	dec    %ax
	push   %ax
	push   %di
	pushw  D_013a
	call   L_4068
	add    $0x6,%sp
L_2358:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_235c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpb   $0x0,0x4(%bp)
	je   L_2379
	mov    $0x1,%ax
	push   %ax
	lea    0x4(%bp),%ax
	push   %ax
	pushw  D_013a
	call   L_4068
	add    $0x6,%sp
L_2379:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_237d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	addw   $0x1e,0x4(%bp)
	adcw   $0x0,0x6(%bp)
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	lea    0x4(%bp),%ax
	push   %ax
	call   L_3e45
	add    $0x6,%sp
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_3e99
	add    $0x8,%sp
	mov    %ax,-0x6(%bp)
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	lea    0x4(%bp),%ax
	push   %ax
	call   L_3e45
	add    $0x6,%sp
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_3e99
	add    $0x8,%sp
	mov    %ax,%si
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_3e48
	add    $0x8,%sp
	mov    %ax,%di
	or     %ax,%ax
	je   L_23fe
	push   %di
	call   L_241f
	pop    %cx
	mov    $0x68,%ax
	push   %ax
	call   L_235c
	pop    %cx
L_23fe:
	push   %si
	call   L_241f
	pop    %cx
	mov    $0x6d,%ax
	push   %ax
	call   L_235c
	pop    %cx
	pushw  -0x6(%bp)
	call   L_241f
	pop    %cx
	mov    $0x73,%ax
	push   %ax
	call   L_235c
	pop    %cx
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_241f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_2437
	pop    %cx
	mov    $D_0706,%ax
	push   %ax
	call   L_233a
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2437:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    $D_0706,%di
	movw   $0x0,-0x8(%bp)
	mov    0x4(%bp),%si
	movw   $D_0710,-0x6(%bp)
L_244f:
	cmpw   $0x1,-0x6(%bp)
	je   L_2489
	mov    %si,%ax
	xor    %dx,%dx
	divw   -0x6(%bp)
	mov    %ax,-0xa(%bp)
	or     %ax,-0x8(%bp)
	je   L_2471
	mov    -0xa(%bp),%ax
	add    $0x30,%ax
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
L_2471:
	mov    %si,%ax
	xor    %dx,%dx
	divw   -0x6(%bp)
	mov    %dx,%si
	mov    -0x6(%bp),%ax
	xor    %dx,%dx
	mov    $0xa,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	jmp   L_244f
L_2489:
	lea    0x30(%si),%ax
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_24a0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	xor    %si,%si
L_24ab:
	mov    (%di),%al
	mov    %al,-0x5(%bp)
	cbtw
	test   $0x80,%ax
	jne   L_24eb
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_00b4(%bx),%al
	cbtw
	test   $0x8,%ax
	je   L_24eb
	cmpb   $0x0,-0x5(%bp)
	je   L_24eb
	or     %si,%si
	jl   L_24eb
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    -0x5(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	add    %dx,%ax
	sub    $0x30,%ax
	mov    %ax,%si
	inc    %di
	jmp   L_24ab
L_24eb:
	or     %si,%si
	jl   L_24f4
	cmp    0x4(%bp),%di
	jne   L_2503
L_24f4:
	mov    $D_01b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0ae1
	add    $0x4,%sp
	jmp   L_2505
L_2503:
	mov    %si,%ax
L_2505:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_250a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,0x4(%bp)
	jne   L_2519
	jmp   L_2637
L_2519:
	mov    0x4(%bp),%bx
	mov    (%bx),%si
	mov    0x4(%bp),%bx
	pushw  0x2(%bx)
	call   L_2a02
	pop    %cx
	mov    %ax,%di
	cmpb   $0x0,(%di)
	jne   L_2532
	jmp   L_262d
L_2532:
	testw  $0x1,D_0670
	je   L_253d
	jmp   L_262d
L_253d:
	test   $0x10,%si
	je   L_2572
	call   L_1243
	mov    %ax,-0x6(%bp)
	push   %ax
	push   %di
	call   L_11b9
	pop    %cx
	push   %ax
	call   L_18ff
	add    $0x4,%sp
	pushw  -0x6(%bp)
	call   L_3f7e
	pop    %cx
	mov    $D_013e,%ax
	push   %ax
	call   L_11b9
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    $D_013e,%ax
	push   %ax
	call   L_4038
	pop    %cx
	jmp   L_25b0
L_2572:
	test   $0x80,%si
	je   L_25be
	push   %di
	mov    $D_0314,%ax
	push   %ax
	call   L_2d7f
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_2598
	movw   $0xffff,-0x6(%bp)
	mov    %si,%ax
	and    $0xf,%ax
	push   %ax
	call   L_3f7e
	pop    %cx
	jmp   L_25cc
L_2598:
	push   %di
	call   L_24a0
	pop    %cx
	mov    %ax,-0x6(%bp)
	cmp    $0xa,%ax
	jl   L_25b2
	mov    $D_0278,%ax
	push   %ax
	push   %di
	call   L_0ae1
	add    $0x4,%sp
L_25b0:
	jmp   L_25e1
L_25b2:
	pushw  -0x6(%bp)
	call   L_3b10
	pop    %cx
	mov    %ax,-0x6(%bp)
	jmp   L_25e1
L_25be:
	test   $0x20,%si
	jne   L_25ce
	push   %di
	call   L_11b9
	pop    %cx
	mov    %ax,-0x6(%bp)
L_25cc:
	jmp   L_260e
L_25ce:
	testw  $0x100,D_0670
	je   L_25e3
	mov    $D_0200,%ax
	push   %ax
	push   %di
	call   L_0ae1
	add    $0x4,%sp
L_25e1:
	jmp   L_2618
L_25e3:
	test   $0x40,%si
	je   L_2610
	mov    $0x1,%ax
	push   %ax
	push   %di
	call   L_3f92
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jl   L_2610
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	call   L_3d6a
	add    $0x8,%sp
L_260e:
	jmp   L_2618
L_2610:
	push   %di
	call   L_1216
	pop    %cx
	mov    %ax,-0x6(%bp)
L_2618:
	cmpw   $0x0,-0x6(%bp)
	jl   L_262d
	mov    %si,%ax
	and    $0xf,%ax
	push   %ax
	pushw  -0x6(%bp)
	call   L_11e5
	add    $0x4,%sp
L_262d:
	mov    0x4(%bp),%bx
	pushw  0x4(%bx)
	call   L_250a
	pop    %cx
L_2637:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_263c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	mov    $0x2f,%ax
	push   %ax
	call   L_2d49
	add    $0x4,%sp
	or     %ax,%ax
	je   L_266e
	testw  $0x100,D_0670
	je   L_2669
	mov    $D_0200,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0ae1
	add    $0x4,%sp
	jmp   L_2680
L_2669:
	mov    $D_02fa,%ax
	jmp   L_2680
L_266e:
	mov    D_05a6,%di
	or     %di,%di
	jne   L_267b
	mov    $D_0300,%ax
	jmp   L_2680
L_267b:
	push   %di
	call   L_2d08
	pop    %cx
L_2680:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2684:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_2692:
	push   %si
	push   %di
	call   L_26bc
	add    $0x4,%sp
	mov    %ax,%di
	xor    %ax,%ax
	push   %ax
	pushw  D_070e
	call   L_3f92
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jge   L_26b4
	or     %di,%di
	jne   L_2692
L_26b4:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_26bc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    %di,%si
	call   L_2c4e
	mov    %ax,-0x6(%bp)
L_26ce:
	cmpb   $0x0,(%si)
	je   L_26ea
	cmpb   $0x3a,(%si)
	je   L_26ea
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	jmp   L_26ce
L_26ea:
	cmp    %di,%si
	je   L_26f7
	mov    -0x6(%bp),%bx
	movb   $0x2f,(%bx)
	incw   -0x6(%bp)
L_26f7:
	cmpb   $0x3a,(%si)
	jne   L_26fd
	inc    %si
L_26fd:
	cmpb   $0x0,(%si)
	je   L_2706
	mov    %si,%ax
	jmp   L_2708
L_2706:
	xor    %ax,%ax
L_2708:
	mov    %ax,%di
	mov    0x6(%bp),%si
L_270d:
	mov    %si,%bx
	inc    %si
L_2710:
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_270d
	mov    %di,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2728:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%si
	testw  $0x1,D_0670
	jne   L_276e
	/* dual-reloc split: movw $D_026e,D_05e6 */
	mov	$D_026e,%ax
	mov	%ax,D_05e6
	pushw  (%si)
	call   L_263c
	pop    %cx
	mov    %ax,%di
	mov    $D_01e2,%ax
	push   %ax
	call   L_2136
	pop    %cx
	call   L_22b1
	mov    %ax,D_05e8
L_2754:
	push   %si
	push   %di
	call   L_2772
	add    $0x4,%sp
	mov    %ax,%di
	or     %ax,%ax
	jne   L_2754
	pushw  D_05e6
	pushw  (%si)
	call   L_0ae1
	add    $0x4,%sp
L_276e:
	pop    %di
	pop    %si
L_2770:
	pop    %bp
	ret


L_2772:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x6(%bp),%di
	pushw  (%di)
	pushw  0x4(%bp)
	call   L_26bc
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	mov    D_070e,%si
	push   %si
	call   L_29b9
	pop    %cx
	call   L_0ac7
	pushw  D_05e8
	lea    0x0(%di),%ax
	push   %ax
	push   %si
	call   L_3b26
	add    $0x6,%sp
	mov    D_0768,%ax
	jmp   L_282b
L_27ab:
	movw   $0x0,D_0670
	movw   $0x0,D_066c
	movw   $0x0,D_070c
	call   L_0219
	mov    D_0152,%bx
	cmpw   $0x0,(%bx)
	je   L_27d3
	mov    D_0152,%bx
	pushw  (%bx)
	call   L_3f7e
	pop    %cx
L_27d3:
	pushw  D_013a
	call   L_3f7e
	pop    %cx
	movw   $0x2,D_013a
	push   %si
	call   L_11b9
	pop    %cx
	mov    D_0152,%bx
	mov    %ax,(%bx)
	push   %di
	call   L_013e
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	mov    $D_06b2,%ax
	push   %ax
	call   L_3bea
	add    $0x4,%sp
L_27ff:
	mov    $D_0256,%ax
	push   %ax
	push   %si
	call   L_0ae1
	add    $0x4,%sp
L_280a:
	mov    $D_023a,%ax
	push   %ax
	push   %si
	call   L_0ae1
	add    $0x4,%sp
L_2815:
	mov    $D_024c,%ax
	push   %ax
	push   %si
	call   L_0ae1
	add    $0x4,%sp
L_2820:
	/* dual-reloc split: movw $D_025e,D_05e6 */
	mov	$D_025e,%ax
	mov	%ax,D_05e6
L_2826:
	mov    -0x6(%bp),%ax
	jmp   L_2849
L_282b:
	cmp    $0x2,%ax
	je   L_2826
	cmp    $0x7,%ax
	je   L_280a
	cmp    $0x8,%ax
	jne   L_283d
	jmp   L_27ab
L_283d:
	cmp    $0xc,%ax
	je   L_27ff
	cmp    $0x1a,%ax
	je   L_2815
	jmp   L_2820
L_2849:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_284e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_05ea,%di
L_2856:
	mov    D_0612,%ax
	shl    %ax
	add    $D_05ea,%ax
	cmp    %ax,%di
	ja   L_286c
	mov    %di,%bx
	inc    %di
	inc    %di
	movw   $0x0,(%bx)
	jmp   L_2856
L_286c:
	movw   $0x0,D_0612
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2876:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_05ea,%di
	cmpw   $0x0,0x4(%bp)
	je   L_28a1
L_2884:
	cmpw   $0x0,(%di)
	je   L_288d
	inc    %di
	inc    %di
	jmp   L_2884
L_288d:
	cmpw   $0x13,D_0612
	jl   L_2898
	dec    %di
	dec    %di
	jmp   L_289c
L_2898:
	incw   D_0612
L_289c:
	mov    0x4(%bp),%ax
	mov    %ax,(%di)
L_28a1:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_28a5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	mov    D_0612,%ax
	mov    %ax,-0xc(%bp)
	pushw  0x4(%bp)
	call   L_2876
	pop    %cx
L_28c4:
	cmpw   $0x0,D_0612
	jne   L_28ce
	jmp   L_2994
L_28ce:
	movw   $D_05ea,-0x10(%bp)
	lea    -0xa(%bp),%ax
	push   %ax
	call   L_404c
	pop    %cx
	mov    %ax,%di
L_28dd:
	mov    -0xc(%bp),%ax
	shl    %ax
	add    $D_05ea,%ax
	cmp    %ax,-0x10(%bp)
	ja   L_2904
	mov    -0x10(%bp),%bx
	cmp    %di,(%bx)
	jne   L_28fe
	mov    -0x10(%bp),%bx
	movw   $0x0,(%bx)
	decw   D_0612
	jmp   L_28dd
L_28fe:
	addw   $0x2,-0x10(%bp)
	jmp   L_28dd
L_2904:
	cmp    $0xffff,%di
	je   L_28c4
	mov    -0xa(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	and    $0xff,%ax
	mov    %ax,-0xe(%bp)
	mov    -0xa(%bp),%ax
	and    $0x7f,%ax
	mov    %ax,%si
	or     %ax,%ax
	je   L_2974
	cmp    $0x7f,%si
	jne   L_2933
	mov    $D_0614,%ax
	push   %ax
	call   L_233a
	pop    %cx
	mov    -0xe(%bp),%si
L_2933:
	mov    %si,%bx
	shl    %bx
	cmpw   $0x0,D_039c(%bx)
	je   L_2971
	cmp    %di,0x4(%bp)
	jne   L_294b
	testw  $0x4,D_0670
	jne   L_2956
L_294b:
	call   L_2312
	push   %di
	call   L_241f
	pop    %cx
	call   L_2301
L_2956:
	mov    %si,%bx
	shl    %bx
	pushw  D_039c(%bx)
	call   L_233a
	pop    %cx
	testw  $0x80,-0xa(%bp)
	je   L_2971
	mov    $D_022a,%ax
	push   %ax
	call   L_233a
	pop    %cx
L_2971:
	call   L_22f0
L_2974:
	cmpw   $0x0,-0x6(%bp)
	jne   L_298b
	or     %si,%si
	je   L_2985
	mov    %si,%ax
	or     $0x80,%ax
	jmp   L_2988
L_2985:
	mov    -0xe(%bp),%ax
L_2988:
	mov    %ax,-0x6(%bp)
L_298b:
	mov    -0xa(%bp),%ax
	or     %ax,-0x8(%bp)
	jmp   L_28c4
L_2994:
	cmpw   $0x0,-0x8(%bp)
	je   L_29a9
	testw  $0x10,D_0670
	je   L_29a9
	pushw  -0x6(%bp)
	call   L_0b29
	pop    %cx
L_29a9:
	mov    -0x6(%bp),%ax
	mov    %ax,D_06c0
	call   L_0ab0
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_29b9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	movb   $0x0,-0x6(%bp)
	mov    0x4(%bp),%di
	or     %di,%di
	je   L_29f0
L_29ca:
	mov    (%di),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	cmp    $0x0,%cl
	je   L_29f0
	mov    -0x5(%bp),%al
	cbtw
	and    $0x7f,%ax
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%cx
	or     %cl,-0x6(%bp)
	jmp   L_29ca
L_29f0:
	mov    -0x6(%bp),%al
	cbtw
	and    $0x80,%ax
	mov    %ax,%cx
	mov    %cl,D_0712
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2a02:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_178c
	pop    %cx
	mov    %ax,%di
	push   %di
	call   L_29b9
	pop    %cx
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2a1b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    D_066e,%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	mov    0x4(%bp),%ax
	shl    %ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_2c2c
	pop    %cx
	mov    %ax,%si
	mov    0x4(%bp),%ax
	shl    %ax
	add    %ax,%si
	mov    %si,-0x6(%bp)
	movw   $0x0,(%si)
L_2a45:
	or     %di,%di
	je   L_2a7d
	lea    0x2(%di),%ax
	dec    %si
	dec    %si
	mov    %ax,(%si)
	mov    (%di),%di
	or     %di,%di
	je   L_2a5c
	pushw  (%si)
	call   L_29b9
	pop    %cx
L_2a5c:
	or     %di,%di
	je   L_2a67
	mov    %di,%ax
	test   $0x1,%ax
	je   L_2a74
L_2a67:
	pushw  -0x6(%bp)
	push   %si
	call   L_2a84
	add    $0x4,%sp
	mov    %si,-0x6(%bp)
L_2a74:
	mov    %di,%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	jmp   L_2a45
L_2a7d:
	mov    %si,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2a84:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xa,%sp
	mov    0x6(%bp),%ax
	sub    0x4(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xa(%bp)
	cmp    $0x1,%ax
	jg   L_2aa3
	jmp   L_2b38
L_2aa3:
	mov    $0x1,%si
L_2aa6:
	cmp    -0xa(%bp),%si
	jg   L_2ab6
	mov    %si,%ax
	mov    $0x2,%dx
	imul   %dx
	mov    %ax,%si
	jmp   L_2aa6
L_2ab6:
	mov    %si,%ax
	shl    %ax
	dec    %ax
	mov    %ax,-0x8(%bp)
L_2abe:
	mov    -0x8(%bp),%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	je   L_2b38
	mov    -0xa(%bp),%ax
	sub    -0x8(%bp),%ax
	mov    %ax,-0x6(%bp)
	xor    %si,%si
L_2ad9:
	cmp    -0x6(%bp),%si
	jge   L_2abe
	mov    %si,%di
L_2ae0:
	or     %di,%di
	jl   L_2b35
	mov    %di,%ax
	shl    %ax
	add    0x4(%bp),%ax
	mov    %ax,-0xc(%bp)
	mov    %ax,%bx
	pushw  (%bx)
	mov    -0x8(%bp),%ax
	shl    %ax
	add    -0xc(%bp),%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_2d7f
	add    $0x4,%sp
	or     %ax,%ax
	jg   L_2b35
	mov    -0x8(%bp),%ax
	shl    %ax
	add    -0xc(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,-0xe(%bp)
	mov    -0xc(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%dx
	shl    %dx
	add    -0xc(%bp),%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	mov    -0xc(%bp),%bx
	mov    -0xe(%bp),%ax
	mov    %ax,(%bx)
	sub    -0x8(%bp),%di
	jmp   L_2ae0
L_2b35:
	inc    %si
	jmp   L_2ad9
L_2b38:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2b3f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	xor    %si,%si
	mov    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_2b6e
	mov    -0x6(%bp),%bx
	mov    0x4(%bx),%di
L_2b57:
	or     %di,%di
	je   L_2b6e
	lea    0x2(%di),%ax
	push   %ax
	call   L_178c
	pop    %cx
	push   %ax
	call   L_2b75
	pop    %cx
	add    %ax,%si
	mov    (%di),%di
	jmp   L_2b57
L_2b6e:
	mov    %si,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2b75:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	movw   $0x0,-0x8(%bp)
L_2b86:
	call   L_0ac7
	call   L_2c4e
	inc    %ax
	inc    %ax
	mov    %ax,%si
L_2b90:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	pushw  D_05b2
	pushw  -0x6(%bp)
	call   L_2d49
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_2bbc
	cmpw   $0x0,-0x6(%bp)
	je   L_2bbc
	mov    %si,%bx
	inc    %si
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_2b90
L_2bbc:
	mov    D_070e,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	jne   L_2bd0
	cmpw   $0x0,-0x6(%bp)
	jne   L_2b86
	mov    -0x8(%bp),%ax
	jmp   L_2c0a
L_2bd0:
	cmpw   $0x0,-0x6(%bp)
	jne   L_2bd7
	dec    %di
L_2bd7:
	xor    %ax,%ax
	push   %ax
	push   %si
	call   L_2c88
	pop    %cx
	mov    %ax,%si
	inc    %ax
	inc    %ax
	push   %ax
	call   L_0bb2
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_2bf9
	mov    -0x6(%bp),%ax
	add    %ax,-0x8(%bp)
	jmp   L_2c01
L_2bf9:
	push   %si
	call   L_0f0b
	pop    %cx
	incw   -0x8(%bp)
L_2c01:
	orw    $0x1,D_066e
	jmp   L_2b86
L_2c0a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2c12:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_3f58
	pop    %cx
	mov    %ax,%di
	add    0x4(%bp),%ax
	mov    %ax,D_0688
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2c2c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	inc    %ax
	and    $0xfffe,%ax
	mov    %ax,%si
	mov    D_061e,%di
	add    %si,D_061e
	mov    D_061e,%ax
	mov    %ax,D_070e
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2c4e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_0688,%ax
	sub    D_061e,%ax
	cmp    $0x200,%ax
	jge   L_2c75
	pushw  D_0028
	call   L_2c12
	pop    %cx
	cmpw   $0x800,D_0028
	jae   L_2c75
	addw   $0x100,D_0028
L_2c75:
	mov    D_061e,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2c7c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_061e,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2c88:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	mov    D_061e,%si
	lea    0x1(%di),%ax
	and    $0xfffe,%ax
	mov    %ax,D_070e
	mov    %ax,D_061e
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2cad:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
L_2cb6:
	cmp    %di,D_0672
	jbe   L_2ccf
	pushw  D_0672
	call   L_037e
	pop    %cx
	mov    D_0672,%bx
	mov    (%bx),%ax
	mov    %ax,D_0672
	jmp   L_2cb6
L_2ccf:
	cmp    D_068a,%di
	jbe   L_2cd9
	mov    %di,%ax
	jmp   L_2cdc
L_2cd9:
	mov    D_068a,%ax
L_2cdc:
	mov    %ax,D_061e
	mov    %ax,D_070e
	push   %di
	call   L_0b89
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2ceb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_0688,%ax
	sub    D_068a,%ax
	cmp    $0x400,%ax
	jle   L_2d04
	mov    $0xfe00,%ax
	push   %ax
	call   L_2c12
	pop    %cx
L_2d04:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2d08:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_2c4e
	push   %ax
	pushw  0x4(%bp)
	call   L_2d24
	add    $0x4,%sp
	push   %ax
	call   L_2c88
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2d24:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_2d31:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_2d31
	dec    %si
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2d49:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_2d4f:
	mov    0x6(%bp),%bx
	incw   0x6(%bp)
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	je   L_2d78
	mov    -0x5(%bp),%al
	cbtw
	mov    0x4(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_2d4f
	mov    $0xffff,%ax
	jmp   L_2d7a
L_2d78:
	xor    %ax,%ax
L_2d7a:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2d7f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_2d8c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    (%si),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_2dab
	mov    %si,%bx
	inc    %si
	cmpb   $0x0,(%bx)
	jne   L_2d8c
	xor    %ax,%ax
	jmp   L_2dba
L_2dab:
	dec    %di
	mov    (%di),%al
	cbtw
	mov    (%si),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %dx,%ax
L_2dba:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2dbe:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	or     %di,%di
	je   L_2dd2
L_2dca:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_2dca
L_2dd2:
	mov    %di,%ax
	sub    0x4(%bp),%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2ddc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	call   L_2c4e
	inc    %ax
	inc    %ax
	mov    %ax,%di
	movw   $0x1,-0x8(%bp)
	movw   $0x0,D_06a0
	movw   $0x0,D_06ee
L_2dfc:
	xor    %ax,%ax
	push   %ax
	call   L_3069
	pop    %cx
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	mov    %cl,%al
	cbtw
	test   $0x80,%ax
	jne   L_2e20
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_0034(%bx),%al
	cbtw
	test   $0x4,%ax
	jne   L_2dfc
L_2e20:
	mov    -0x5(%bp),%al
	cbtw
	test   $0x80,%ax
	jne   L_2e3c
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_0034(%bx),%al
	cbtw
	test   $0x3e,%ax
	je   L_2e3c
	jmp   L_2fc6
L_2e3c:
	cmpb   $0x27,-0x5(%bp)
	jne   L_2e7c
	mov    %di,%bx
	inc    %di
	movb   $0x22,(%bx)
L_2e48:
	call   L_30f3
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	je   L_2e73
	cmpb   $0x27,-0x5(%bp)
	je   L_2e73
	mov    -0x5(%bp),%al
	cbtw
	or     $0x80,%ax
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    -0x5(%bp),%al
	cbtw
	push   %ax
	call   L_1cbb
	pop    %cx
	jmp   L_2e48
L_2e73:
	mov    %di,%bx
	inc    %di
	movb   $0x22,(%bx)
	jmp   L_2f07
L_2e7c:
	mov    %di,%bx
	inc    %di
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	cmpb   $0x3d,-0x5(%bp)
	jne   L_2e91
	mov    -0x8(%bp),%ax
	or     %ax,D_06ee
L_2e91:
	mov    -0x5(%bp),%al
	cbtw
	test   $0x80,%ax
	jne   L_2eaa
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_00b4(%bx),%al
	cbtw
	test   $0x48,%ax
	jne   L_2eaf
L_2eaa:
	movw   $0x0,-0x8(%bp)
L_2eaf:
	mov    -0x5(%bp),%al
	cbtw
	test   $0x80,%ax
	jne   L_2f07
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_0034(%bx),%al
	cbtw
	test   $0x40,%ax
	je   L_2efa
	mov    -0x5(%bp),%al
	mov    %al,-0x6(%bp)
L_2ece:
	mov    -0x6(%bp),%al
	cbtw
	push   %ax
	call   L_3069
	pop    %cx
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	mov    %cl,%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	je   L_2f07
	mov    -0x5(%bp),%al
	cbtw
	mov    -0x6(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
L_2efa:
	je   L_2f07
	mov    -0x5(%bp),%al
	cbtw
	push   %ax
	call   L_1cbb
	pop    %cx
	jmp   L_2ece
L_2f07:
	xor    %ax,%ax
	push   %ax
	call   L_3069
	pop    %cx
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	mov    %cl,%al
	cbtw
	test   $0x80,%ax
	je   L_2f1e
	jmp   L_2e3c
L_2f1e:
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_0034(%bx),%al
	cbtw
	test   $0x3e,%ax
	jne   L_2f31
	jmp   L_2e3c
L_2f31:
	push   %di
	call   L_2c88
	pop    %cx
	mov    %ax,%di
	mov    0x2(%di),%al
	cbtw
	test   $0x80,%ax
	jne   L_2f51
	mov    0x2(%di),%al
	cbtw
	mov    %ax,%bx
	mov    D_00b4(%bx),%al
	cbtw
	test   $0x40,%ax
	jne   L_2f57
L_2f51:
	movw   $0x0,D_06ee
L_2f57:
	mov    -0x5(%bp),%al
	cbtw
	or     $0x8000,%ax
	mov    %ax,D_0666
	cmpb   $0x0,0x3(%di)
	jne   L_2f9e
	mov    0x2(%di),%al
	mov    %al,-0x6(%bp)
	cbtw
	test   $0x80,%ax
	jne   L_2f9e
	mov    -0x6(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_00b4(%bx),%al
	cbtw
	test   $0x8,%ax
	je   L_2f9e
	cmpb   $0x3e,-0x5(%bp)
	je   L_2f8f
	cmpb   $0x3c,-0x5(%bp)
	jne   L_2f9e
L_2f8f:
	call   L_2ddc
	mov    -0x6(%bp),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,D_06a0
	jmp   L_2fc4
L_2f9e:
	cmpb   $0x0,D_0664
	je   L_2fba
	mov    $D_035c,%ax
	push   %ax
	lea    0x2(%di),%ax
	push   %ax
	call   L_1d30
	add    $0x4,%sp
	mov    %ax,D_06c4
	or     %ax,%ax
	jne   L_2fc4
L_2fba:
	mov    %di,D_0686
	movw   $0x0,D_06c4
L_2fc4:
	jmp   L_3006
L_2fc6:
	mov    -0x5(%bp),%al
	cbtw
	test   $0x80,%ax
	jne   L_301b
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_0034(%bx),%al
	cbtw
	test   $0x8,%ax
	je   L_301b
	xor    %ax,%ax
	push   %ax
	call   L_3069
	pop    %cx
	mov    %ax,%cx
	mov    %cl,-0x6(%bp)
	mov    %cl,%al
	cbtw
	mov    -0x5(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_3008
	mov    -0x5(%bp),%al
	cbtw
	or     $0x800,%ax
	mov    %ax,D_06c4
L_3006:
	jmp   L_3019
L_3008:
	mov    -0x6(%bp),%al
	cbtw
	or     $0x8000,%ax
	mov    %ax,D_0666
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,D_06c4
L_3019:
	jmp   L_305a
L_301b:
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,D_06c4
	or     %ax,%ax
	jne   L_302c
	movw   $0x400,D_06c4
L_302c:
	cmpw   $0x0,D_066a
	je   L_305a
	mov    -0x5(%bp),%al
	cbtw
	test   $0x80,%ax
	jne   L_305a
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_0034(%bx),%al
	cbtw
	test   $0x30,%ax
	je   L_305a
	pushw  D_066a
	call   L_126e
	pop    %cx
	movw   $0x0,D_066a
L_305a:
	movb   $0x0,D_0664
	mov    D_06c4,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3069:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	call   L_30f3
	mov    %ax,%cx
	mov    %cl,-0x6(%bp)
	cmp    $0x5c,%cl
	jne   L_309f
	call   L_30f3
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	cmp    $0xa,%cl
	jne   L_30a1
	mov    $0xa,%ax
	push   %ax
	call   L_1cbb
	pop    %cx
	mov    0x4(%bp),%al
	cbtw
	push   %ax
	call   L_3069
	pop    %cx
	mov    %ax,%cx
	mov    %cl,-0x6(%bp)
L_309f:
	jmp   L_30dc
L_30a1:
	cmpb   $0x0,0x4(%bp)
	je   L_30de
	mov    -0x5(%bp),%al
	cbtw
	mov    0x4(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	je   L_30de
	mov    -0x5(%bp),%al
	cbtw
	test   $0x80,%ax
	jne   L_30d2
	mov    -0x5(%bp),%al
	cbtw
	mov    %ax,%bx
	mov    D_0034(%bx),%al
	cbtw
	test   $0x80,%ax
	jne   L_30de
L_30d2:
	mov    -0x5(%bp),%al
	cbtw
	or     $0x8000,%ax
	mov    %ax,D_0666
L_30dc:
	jmp   L_30ea
L_30de:
	mov    -0x5(%bp),%al
	cbtw
	or     $0x80,%ax
	mov    %ax,%cx
	mov    %cl,-0x6(%bp)
L_30ea:
	mov    -0x6(%bp),%al
	cbtw
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_30f3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_30f9:
	cmpw   $0x0,D_0666
	je   L_3110
	mov    D_0666,%ax
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	movw   $0x0,D_0666
	jmp   L_3173
L_3110:
	mov    D_0152,%si
	mov    0x6(%si),%ax
	cmp    0x8(%si),%ax
	je   L_3175
	mov    0x6(%si),%bx
	incw   0x6(%si)
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_314f
	cmpw   $0x0,0xa(%si)
	je   L_30f9
	mov    0xa(%si),%bx
	addw   $0x2,0xa(%si)
	pushw  (%bx)
	call   L_1106
	pop    %cx
	or     %ax,%ax
	je   L_314b
	movb   $0x0,-0x5(%bp)
	jmp   L_314f
L_314b:
	movb   $0x20,-0x5(%bp)
L_314f:
	testw  $0x1000,D_0670
	je   L_316a
	mov    D_0152,%bx
	cmpw   $0x0,0xc(%bx)
	jne   L_316a
	mov    -0x5(%bp),%al
	cbtw
	push   %ax
	call   L_235c
	pop    %cx
L_316a:
	cmpb   $0xa,-0x5(%bp)
	jne   L_31b3
	incw   0x2(%si)
L_3173:
	jmp   L_31b3
L_3175:
	cmpb   $0x0,0x4(%si)
	jne   L_3180
	cmpw   $0x0,(%si)
	jge   L_3189
L_3180:
	movb   $0x0,-0x5(%bp)
	incb   0x4(%si)
	jmp   L_31b3
L_3189:
	call   L_31bc
	mov    %ax,%di
	or     %ax,%ax
	jg   L_31a5
	pushw  (%si)
	call   L_3f7e
	pop    %cx
	movw   $0xffff,(%si)
	movb   $0x0,-0x5(%bp)
	incb   0x4(%si)
	jmp   L_31b3
L_31a5:
	lea    0xe(%si),%ax
	mov    %ax,0x6(%si)
	add    %di,%ax
	mov    %ax,0x8(%si)
	jmp   L_30f9
L_31b3:
	mov    -0x5(%bp),%al
	cbtw
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_31bc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_0152,%di
L_31c5:
	mov    D_0674,%al
	cbtw
	test   $0x4,%ax
	je   L_31d4
	call   L_22f0
	call   L_0ac7
L_31d4:
	mov    0x5(%di),%al
	cbtw
	push   %ax
	lea    0xe(%di),%ax
	push   %ax
	pushw  (%di)
	call   L_3fa8
	add    $0x6,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_31f2
	cmpb   $0x0,D_0674
	jne   L_31c5
L_31f2:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_31f8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x22,%sp
	call   L_2c7c
	mov    %ax,-0x6(%bp)
	call   L_0ac7
	mov    0x4(%bp),%di
	or     %di,%di
	jne   L_3213
	jmp   L_3a93
L_3213:
	cmpb   $0x0,D_0668
	je   L_321d
	jmp   L_3a93
L_321d:
	mov    (%di),%si
	mov    %si,%ax
	and    $0xf,%ax
	mov    %ax,-0xa(%bp)
	mov    D_06c0,%ax
	mov    %ax,-0x8(%bp)
	movw   $0x0,D_06c0
	mov    -0xa(%bp),%ax
	jmp   L_3a84
	mov    D_066e,%ax
	mov    %ax,-0x14(%bp)
	mov    0x2(%di),%ax
	mov    %ax,-0x16(%bp)
	movw   $0x0,D_066e
	push   %di
	call   L_2b3f
	pop    %cx
	mov    %ax,-0x10(%bp)
	pushw  -0x10(%bp)
	call   L_2a1b
	pop    %cx
	mov    %ax,-0xc(%bp)
	mov    %ax,%bx
	mov    0x2(%bx),%ax
	mov    %ax,-0xe(%bp)
	mov    -0x14(%bp),%ax
	mov    %ax,D_066e
	mov    $D_03d0,%ax
	push   %ax
	mov    -0xc(%bp),%bx
	pushw  (%bx)
	call   L_1d30
	add    $0x4,%sp
	mov    %ax,-0x12(%bp)
	or     %ax,%ax
	jne   L_3287
	cmpw   $0x0,-0x10(%bp)
	jne   L_3293
L_3287:
	xor    %ax,%ax
	push   %ax
	pushw  0x6(%di)
	call   L_1d77
	add    $0x4,%sp
L_3293:
	cmpw   $0x0,-0x10(%bp)
	jne   L_329c
	jmp   L_3691
L_329c:
	testw  $0x1,D_0670
	je   L_32a7
	jmp   L_3691
L_32a7:
	testw  $0x800,D_0670
	je   L_32e6
	movw   $0x0,-0x10(%bp)
	mov    $D_0342,%ax
	push   %ax
	call   L_233a
	pop    %cx
L_32bc:
	mov    -0x10(%bp),%ax
	shl    %ax
	add    -0xc(%bp),%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	je   L_32e3
	mov    -0x10(%bp),%ax
	incw   -0x10(%bp)
	shl    %ax
	add    -0xc(%bp),%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_233a
	pop    %cx
	call   L_2301
	jmp   L_32bc
L_32e3:
	call   L_22f0
L_32e6:
	mov    -0x12(%bp),%ax
	jmp   L_366a
	cmpw   $0x0,-0xe(%bp)
	je   L_3325
	pushw  -0xe(%bp)
	pushw  -0xe(%bp)
	call   L_263c
	pop    %cx
	push   %ax
	call   L_2684
	add    $0x4,%sp
	mov    %ax,-0x18(%bp)
	or     %ax,%ax
	jge   L_3319
	mov    $D_026e,%ax
	push   %ax
	pushw  -0xe(%bp)
	call   L_0ae1
	add    $0x4,%sp
	jmp   L_334d
L_3319:
	pushw  -0x18(%bp)
	xor    %ax,%ax
	push   %ax
	call   L_3aa7
	add    $0x4,%sp
L_3325:
	jmp   L_336b
	lea    -0x26(%bp),%ax
	push   %ax
	call   L_3d88
	pop    %cx
	pushw  -0x1c(%bp)
	pushw  -0x1e(%bp)
	call   L_237d
	add    $0x4,%sp
	call   L_2301
	pushw  -0x18(%bp)
	pushw  -0x1a(%bp)
	call   L_237d
	add    $0x4,%sp
	call   L_22f0
L_334d:
	jmp   L_3397
	cmpw   $0x0,-0xe(%bp)
	je   L_335e
	pushw  -0xe(%bp)
	call   L_24a0
	pop    %cx
	jmp   L_3361
L_335e:
	mov    -0x8(%bp),%ax
L_3361:
	push   %ax
	call   L_0b29
	pop    %cx
	movw   $0x0,-0x16(%bp)
L_336b:
	jmp   L_3397
	mov    D_06be,%ax
	neg    %ax
	mov    %ax,%cx
	mov    %cl,D_0668
	jmp   L_3397
	mov    D_06be,%ax
	mov    %ax,%cx
	mov    %cl,D_0668
	or     %cl,%cl
	je   L_3397
	cmpw   $0x0,-0xe(%bp)
	je   L_3397
	pushw  -0xe(%bp)
	call   L_24a0
	pop    %cx
	mov    %ax,D_06a8
L_3397:
	jmp   L_34c7
	cmpw   $0x0,-0xe(%bp)
	jne   L_33a3
	jmp   L_3446
L_33a3:
	mov    -0xe(%bp),%bx
	mov    (%bx),%al
	cbtw
	test   $0x80,%ax
	jne   L_33c5
	mov    -0xe(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%bx
	mov    D_00b4(%bx),%al
	cbtw
	test   $0x8,%ax
	je   L_33c5
	mov    $0x1,%ax
	jmp   L_33c7
L_33c5:
	xor    %ax,%ax
L_33c7:
	mov    %ax,%cx
	mov    %cl,-0x17(%bp)
	or     %cl,%cl
	jne   L_33d4
	addw   $0x2,-0xc(%bp)
L_33d4:
	addw   $0x2,-0xc(%bp)
	mov    -0xc(%bp),%bx
	cmpw   $0x0,(%bx)
	jne   L_33e3
	jmp   L_349b
L_33e3:
	mov    -0xc(%bp),%bx
	pushw  (%bx)
	call   L_24a0
	pop    %cx
	mov    %ax,-0x1a(%bp)
	cmp    $0x11,%ax
	jge   L_33fa
	cmpw   $0x0,-0x1a(%bp)
	jge   L_340b
L_33fa:
	mov    $D_02a4,%ax
	push   %ax
	mov    -0xc(%bp),%bx
	pushw  (%bx)
	call   L_0ae1
	add    $0x4,%sp
	jmp   L_33d4
L_340b:
	cmpb   $0x0,-0x17(%bp)
	je   L_341a
	pushw  -0x1a(%bp)
	call   L_1038
	pop    %cx
	jmp   L_33d4
L_341a:
	pushw  -0xe(%bp)
	mov    -0x1a(%bp),%ax
	shl    %ax
	add    $D_06ca,%ax
	push   %ax
	call   L_1e39
	add    $0x4,%sp
	mov    -0xe(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_343d
	pushw  -0x1a(%bp)
	call   L_0fd9
	pop    %cx
	jmp   L_33d4
L_343d:
	pushw  -0x1a(%bp)
	call   L_0fb2
	pop    %cx
	jmp   L_33d4
L_3446:
	movw   $0x0,-0x18(%bp)
L_344b:
	cmpw   $0x11,-0x18(%bp)
	jge   L_34c7
	mov    -0x18(%bp),%bx
	shl    %bx
	cmpw   $0x0,D_06ca(%bx)
	je   L_347c
	pushw  -0x18(%bp)
	call   L_241f
	pop    %cx
	mov    $D_0310,%ax
	push   %ax
	call   L_233a
	pop    %cx
	mov    -0x18(%bp),%bx
	shl    %bx
	pushw  D_06ca(%bx)
	call   L_233a
	pop    %cx
	call   L_22f0
L_347c:
	incw   -0x18(%bp)
	jmp   L_344b
	addw   $0x2,-0xc(%bp)
	pushw  -0x16(%bp)
	call   L_250a
	pop    %cx
	movw   $0x0,D_070c
	movw   $0x0,-0x16(%bp)
	cmpw   $0x0,-0xe(%bp)
L_349b:
	je   L_34c7
	orw    $0x40,D_0670
	call   L_1003
	pushw  -0xc(%bp)
	call   L_2728
	pop    %cx
	call   L_0b59
	testw  $0x100,D_0670
	je   L_34c9
	mov    $D_0200,%ax
	push   %ax
	mov    -0xc(%bp),%bx
	pushw  (%bx)
	call   L_0ae1
	add    $0x4,%sp
L_34c7:
	jmp   L_34f1
L_34c9:
	cmpw   $0x0,-0xe(%bp)
	jne   L_34d9
	mov    D_05ca,%ax
	mov    %ax,-0xe(%bp)
	or     %ax,%ax
	je   L_34e4
L_34d9:
	pushw  -0xe(%bp)
	call   L_3afc
	pop    %cx
	or     %ax,%ax
	jge   L_3525
L_34e4:
	mov    $D_0296,%ax
	push   %ax
	pushw  -0xe(%bp)
	call   L_0ae1
	add    $0x4,%sp
L_34f1:
	jmp   L_3534
	cmpw   $0x1,D_06a6
	jge   L_3504
	mov    $D_0288,%ax
	push   %ax
	call   L_0b14
	pop    %cx
	jmp   L_350d
L_3504:
	addw   $0x2,D_06ac
	decw   D_06a6
L_350d:
	pushw  D_06a6
	mov    $D_06c2,%ax
	push   %ax
	call   L_2014
	add    $0x4,%sp
	jmp   L_3534
	mov    $0xffff,%ax
	push   %ax
	call   L_28a5
	pop    %cx
L_3525:
	jmp   L_3564
	mov    -0xc(%bp),%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_1ea9
	pop    %cx
	mov    %ax,D_06c0
L_3534:
	jmp   L_3574
	cmpw   $0x0,-0xe(%bp)
	je   L_3566
	pushw  -0xc(%bp)
	pushw  -0x10(%bp)
	call   L_0036
	add    $0x4,%sp
	mov    %ax,-0x18(%bp)
	cmp    $0x1,%ax
	jle   L_3574
	mov    -0x10(%bp),%ax
	shl    %ax
	add    -0xc(%bp),%ax
	mov    -0x18(%bp),%dx
	shl    %dx
	sub    %dx,%ax
	push   %ax
	call   L_013e
	pop    %cx
L_3564:
	jmp   L_35bc
L_3566:
	cmpw   $0x0,0x6(%di)
	jne   L_35bc
	mov    $D_0171,%ax
	push   %ax
	call   L_2136
	pop    %cx
L_3574:
	jmp   L_35bc
	/* movw constant -> D_06c0 */
	mov	$0x8000,%ax
	mov	%ax,D_06c0
	cmpw   $0x0,D_06c0
	jne   L_3589
	/* movw constant -> D_06c0 */
	mov	$0x4000,%ax
	mov	%ax,D_06c0
L_3589:
	cmpw   $0x0,-0xe(%bp)
	je   L_35ae
L_358f:
	addw   $0x2,-0xc(%bp)
	mov    -0xc(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_35b6
	mov    -0xc(%bp),%bx
	pushw  (%bx)
	call   L_205e
	pop    %cx
	mov    %ax,%bx
	mov    D_06c0,%ax
	or     %ax,0xa(%bx)
	jmp   L_358f
L_35ae:
	mov    $D_021a,%ax
	push   %ax
	call   L_2136
	pop    %cx
L_35b6:
	movw   $0x0,D_06c0
L_35bc:
	jmp   L_35d4
	cmpw   $0x0,-0xe(%bp)
	je   L_3617
	mov    -0xc(%bp),%ax
	add    $0x4,%ax
	push   %ax
	pushw  -0xe(%bp)
	call   L_3aa7
	add    $0x4,%sp
L_35d4:
	jmp   L_3617
	cmpw   $0x0,-0xe(%bp)
	je   L_3619
	movw   $0x0,-0x1a(%bp)
	movw   $0x0,-0x1a(%bp)
L_35e6:
	mov    -0xe(%bp),%bx
	incw   -0xe(%bp)
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x18(%bp)
	cmp    $0x30,%ax
	jl   L_3610
	cmpw   $0x37,-0x18(%bp)
	jg   L_3610
	mov    -0x1a(%bp),%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	add    -0x18(%bp),%ax
	sub    $0x30,%ax
	mov    %ax,-0x1a(%bp)
	jmp   L_35e6
L_3610:
	pushw  -0x1a(%bp)
	call   L_4024
	pop    %cx
L_3617:
	jmp   L_3657
L_3619:
	xor    %ax,%ax
	push   %ax
	call   L_4024
	pop    %cx
	mov    %ax,-0x18(%bp)
	push   %ax
	call   L_4024
	pop    %cx
	mov    $0x30,%ax
	push   %ax
	call   L_235c
	pop    %cx
	movw   $0x6,-0x1a(%bp)
L_3635:
	cmpw   $0x0,-0x1a(%bp)
	jl   L_3654
	mov    -0x18(%bp),%ax
	mov    -0x1a(%bp),%cx
	sar    %cl,%ax
	and    $0x7,%ax
	add    $0x30,%ax
	push   %ax
	call   L_235c
	pop    %cx
	subw   $0x3,-0x1a(%bp)
	jmp   L_3635
L_3654:
	call   L_22f0
L_3657:
	jmp   L_3668
L_3659:
	pushw  -0xc(%bp)
	pushw  -0x10(%bp)
	call   L_039a
	add    $0x4,%sp
	mov    %ax,-0x12(%bp)
L_3668:
	jmp   L_3677
L_366a:
	dec    %ax
	cmp    $0x12,%ax
	ja   L_3659
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2622(%bx)
L_3677:
	cmpw   $0x0,-0x12(%bp)
	je   L_369a
	cmpw   $0x0,-0x16(%bp)
	je   L_368b
	mov    $D_01f4,%ax
	push   %ax
	call   L_0b14
	pop    %cx
L_368b:
	call   L_107b
	jmp   L_3733
L_3691:
	cmpw   $0x0,0x2(%di)
	jne   L_369a
	jmp   L_3733
L_369a:
	cmpw   $0x0,0x6(%bp)
	je   L_36ae
	test   $0x240,%si
	jne   L_36ae
	movw   $0x0,D_0620
	jmp   L_36c9
L_36ae:
	call   L_3c3a
	mov    %ax,D_0620
	cmp    $0xffff,%ax
	jne   L_36c9
	call   L_0ac7
	mov    $0xa,%ax
	push   %ax
	call   L_3b98
	pop    %cx
	call   L_3bac
	jmp   L_36ae
L_36c9:
	cmpw   $0x0,D_0620
	je   L_3736
	test   $0x10,%si
	je   L_36e9
	testw  $0x20,D_0670
	je   L_36e9
	pushw  D_0620
	call   L_241f
	pop    %cx
	call   L_22f0
L_36e9:
	test   $0x400,%si
	je   L_3702
	mov    0x8(%bp),%bx
	pushw  (%bx)
	call   L_3f7e
	pop    %cx
	mov    0x8(%bp),%bx
	pushw  0x2(%bx)
	call   L_3f7e
	pop    %cx
L_3702:
	test   $0x240,%si
	jne   L_3712
	pushw  D_0620
	call   L_28a5
	pop    %cx
	jmp   L_3730
L_3712:
	test   $0x40,%si
	jne   L_3722
	pushw  D_0620
	call   L_2876
	pop    %cx
	jmp   L_3730
L_3722:
	pushw  D_0620
	mov    $D_069e,%ax
	push   %ax
	call   L_2014
	add    $0x4,%sp
L_3730:
	call   L_107b
L_3733:
	jmp   L_3858
L_3736:
	orw    $0x40,D_0670
	movw   $0x0,D_06aa
	call   L_284e
	call   L_1ce4
	call   L_1003
	test   $0x20,%si
	je   L_376d
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3c4a
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_3c4a
	add    $0x4,%sp
L_376d:
	test   $0x100,%si
	je   L_378b
	xor    %ax,%ax
	push   %ax
	mov    0x8(%bp),%bx
	pushw  (%bx)
	call   L_11e5
	add    $0x4,%sp
	mov    0x8(%bp),%bx
	pushw  0x2(%bx)
	call   L_3f7e
	pop    %cx
L_378b:
	test   $0x200,%si
	je   L_37aa
	mov    $0x1,%ax
	push   %ax
	mov    0xa(%bp),%bx
	pushw  0x2(%bx)
	call   L_11e5
	add    $0x4,%sp
	mov    0xa(%bp),%bx
	pushw  (%bx)
	call   L_3f7e
	pop    %cx
L_37aa:
	test   $0x20,%si
	je   L_37c9
	cmpw   $0x0,D_070c
	jne   L_37c9
	xor    %ax,%ax
	push   %ax
	mov    $D_0338,%ax
	push   %ax
	call   L_11b9
	pop    %cx
	push   %ax
	call   L_11e5
	add    $0x4,%sp
L_37c9:
	pushw  0x2(%di)
	call   L_250a
	pop    %cx
	cmpw   $0x0,-0xa(%bp)
	je   L_37e5
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%di)
	call   L_31f8
	add    $0x4,%sp
	jmp   L_3801
L_37e5:
	mov    -0xc(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_3801
	mov    $0x4000,%ax
	push   %ax
	pushw  0x6(%di)
	call   L_1d77
	add    $0x4,%sp
	pushw  -0xc(%bp)
	call   L_2728
	pop    %cx
L_3801:
	call   L_0b59
	pushw  D_013a
	mov    $0x2,%ax
	push   %ax
	call   L_3b10
	pop    %cx
	push   %ax
	call   L_11e5
	add    $0x4,%sp
	pushw  0x6(%bp)
	pushw  0x2(%di)
	call   L_31f8
	add    $0x4,%sp
	call   L_0b59
	lea    -0x10(%bp),%ax
	push   %ax
	call   L_118c
	pop    %cx
	lea    -0x10(%bp),%ax
	push   %ax
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	pushw  0x2(%di)
	call   L_31f8
	add    $0x8,%sp
	or     %ax,%ax
	jne   L_385a
	pushw  0xa(%bp)
	lea    -0x10(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%di)
	call   L_31f8
	add    $0x8,%sp
L_3858:
	jmp   L_38a0
L_385a:
	pushw  -0x10(%bp)
	call   L_3f7e
	pop    %cx
	pushw  -0xe(%bp)
	call   L_3f7e
	pop    %cx
	jmp   L_38a0
	xor    %ax,%ax
	push   %ax
	pushw  0x2(%di)
	call   L_31f8
	add    $0x4,%sp
	pushw  0x6(%bp)
	pushw  0x4(%di)
	call   L_31f8
	add    $0x4,%sp
	jmp   L_38be
	xor    %ax,%ax
	push   %ax
	pushw  0x2(%di)
	call   L_31f8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_38be
	pushw  0x6(%bp)
	pushw  0x4(%di)
	call   L_31f8
	add    $0x4,%sp
L_38a0:
	jmp   L_38be
	xor    %ax,%ax
	push   %ax
	pushw  0x2(%di)
	call   L_31f8
	add    $0x4,%sp
	or     %ax,%ax
	je   L_38be
	pushw  0x6(%bp)
	pushw  0x4(%di)
	call   L_31f8
	add    $0x4,%sp
L_38be:
	jmp   L_3970
	pushw  0x4(%di)
	call   L_205e
	pop    %cx
	mov    %ax,-0xe(%bp)
	movw   $0x0,-0x12(%bp)
	cmpw   $0x0,0x6(%di)
	jne   L_38e6
	mov    D_06ac,%ax
	inc    %ax
	inc    %ax
	mov    %ax,-0x10(%bp)
	call   L_0238
	mov    %ax,-0x12(%bp)
	jmp   L_390f
L_38e6:
	mov    D_066e,%ax
	mov    %ax,-0x14(%bp)
	movw   $0x0,D_066e
	pushw  0x6(%di)
	call   L_2b3f
	pop    %cx
	push   %ax
	call   L_2a1b
	pop    %cx
	mov    %ax,-0x10(%bp)
	mov    %ax,%bx
	pushw  (%bx)
	call   L_29b9
	pop    %cx
	mov    -0x14(%bp),%ax
	mov    %ax,D_066e
L_390f:
	incw   D_06be
L_3913:
	mov    -0x10(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_394e
	cmpb   $0x0,D_0668
	jne   L_394e
	mov    -0x10(%bp),%bx
	addw   $0x2,-0x10(%bp)
	pushw  (%bx)
	pushw  -0xe(%bp)
	call   L_1e73
	add    $0x4,%sp
	xor    %ax,%ax
	push   %ax
	pushw  0x2(%di)
	call   L_31f8
	add    $0x4,%sp
	cmpb   $0x0,D_0668
	jge   L_3913
	movb   $0x0,D_0668
	jmp   L_3913
L_394e:
	cmpw   $0x0,D_06a8
	je   L_3959
	decw   D_06a8
L_3959:
	mov    D_06a8,%ax
	mov    %ax,%cx
	mov    %cl,D_0668
	decw   D_06be
	pushw  -0x12(%bp)
	call   L_017f
	pop    %cx
	mov    %ax,D_06a2
L_3970:
	jmp   L_39e5
	movw   $0x0,-0xe(%bp)
	incw   D_06be
L_397b:
	cmpb   $0x0,D_0668
	jne   L_39c7
	xor    %ax,%ax
	push   %ax
	pushw  0x2(%di)
	call   L_31f8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_3997
	mov    $0x1,%ax
	jmp   L_3999
L_3997:
	xor    %ax,%ax
L_3999:
	cmpw   $0x5,-0xa(%bp)
	jne   L_39a4
	mov    $0x1,%dx
	jmp   L_39a6
L_39a4:
	xor    %dx,%dx
L_39a6:
	cmp    %dx,%ax
	jne   L_39c7
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%di)
	call   L_31f8
	add    $0x4,%sp
	mov    %ax,-0xe(%bp)
	cmpb   $0x0,D_0668
	jge   L_397b
	movb   $0x0,D_0668
	jmp   L_397b
L_39c7:
	cmpw   $0x0,D_06a8
	je   L_39d2
	decw   D_06a8
L_39d2:
	mov    D_06a8,%ax
	mov    %ax,%cx
	mov    %cl,D_0668
	decw   D_06be
	mov    -0xe(%bp),%ax
	mov    %ax,D_06c0
L_39e5:
	jmp   L_3a11
	xor    %ax,%ax
	push   %ax
	pushw  0x2(%di)
	call   L_31f8
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_3a05
	pushw  0x6(%bp)
	pushw  0x4(%di)
	call   L_31f8
	add    $0x4,%sp
	jmp   L_3a11
L_3a05:
	pushw  0x6(%bp)
	pushw  0x6(%di)
	call   L_31f8
	add    $0x4,%sp
L_3a11:
	jmp   L_3a90
	pushw  0x2(%di)
	call   L_2a02
	pop    %cx
	mov    %ax,-0xe(%bp)
	mov    0x4(%di),%di
L_3a20:
	or     %di,%di
	je   L_3a90
	mov    (%di),%ax
	mov    %ax,-0x10(%bp)
L_3a29:
	cmpw   $0x0,-0x10(%bp)
	je   L_3a6f
	mov    -0x10(%bp),%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_178c
	pop    %cx
	mov    %ax,-0x12(%bp)
	push   %ax
	pushw  -0xe(%bp)
	call   L_0d78
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_3a61
	pushw  -0x12(%bp)
	call   L_29b9
	pop    %cx
	pushw  -0x12(%bp)
	pushw  -0xe(%bp)
	call   L_2d7f
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_3a71
L_3a61:
	xor    %ax,%ax
	push   %ax
	pushw  0x2(%di)
	call   L_31f8
	add    $0x4,%sp
	xor    %di,%di
L_3a6f:
	jmp   L_3a7b
L_3a71:
	mov    -0x10(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_3a29
L_3a7b:
	or     %di,%di
	je   L_3a20
	mov    0x4(%di),%di
	jmp   L_3a20
L_3a84:
	cmp    $0xb,%ax
	ja   L_3a90
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2648(%bx)
L_3a90:
	call   L_0ab0
L_3a93:
	call   L_0ac7
	pushw  -0x6(%bp)
	call   L_2cad
	pop    %cx
	mov    D_06c0,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3aa7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4e,%sp
	lea    -0x52(%bp),%ax
	push   %ax
	call   L_1142
	pop    %cx
	cmpw   $0x0,0x4(%bp)
	je   L_3acc
	pushw  0x4(%bp)
	call   L_1106
	pop    %cx
	mov    0x6(%bp),%ax
	mov    %ax,-0x48(%bp)
	jmp   L_3ad9
L_3acc:
	cmpw   $0x0,0x6(%bp)
	jl   L_3ad9
	pushw  0x6(%bp)
	call   L_10c8
	pop    %cx
L_3ad9:
	xor    %ax,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	mov    $0xa,%ax
	push   %ax
	call   L_0405
	add    $0x4,%sp
	push   %ax
	call   L_31f8
	add    $0x4,%sp
	call   L_1161
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3afc:
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
	jns    L_3b0d
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3b0d:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3b10:
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
	jns    L_3b24
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3b24:
	pop    %bp
	ret


L_3b26:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall exece(59) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* exece(59) -> execve(11) [TLVC# changed] */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_execve,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3b3d
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3b3d:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3b40:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getuid(24) */
	/* getuid -> __NR_getuid */
	mov	$__NR_getuid,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3b4e
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3b4e:
	pop    %bp
	ret


L_3b50:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getpid(20) */
	/* getpid -> __NR_getpid */
	mov	$__NR_getpid,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3b5e
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3b5e:
	pop    %bp
	ret


L_3b60:
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
	jns    L_3b74
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3b74:
	pop    %bp
	ret


L_3b76:
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
	jns    L_3b86
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3b84:
	pop    %bp
	ret
L_3b86:
	mov    0x4(%bp),%cx
	mov    %cx,%bp
	mov    %ax,-0x70(%bp)
	mov    %dx,0x2(%bp)
	mov    $0x0,%ax
	jmp   L_3b84
	/* .byte 0 (alignment) */


L_3b98:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall alarm(27) */
	mov    0x4(%bp),%ax
	/* alarm -> __NR_alarm */
	xchg	%ax,%bx
	mov	$__NR_alarm,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3ba9
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3ba9:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3bac:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall pause(29) */
	/* pause -> __NR_pause */
	mov	$__NR_pause,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3bba
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3bba:
	pop    %bp
	ret


L_3bbc:
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


L_3bea:
	push   %bp
	mov    %sp,%bp
	mov    0x4(%bp),%bx
	mov    0x6(%bp),%ax
	or     %ax,%ax
	jne   L_3bf8
	inc    %ax
L_3bf8:
	mov    (%bx),%cx
	jmp   L_3c03
L_3bfc:
	mov    0x0(%bp),%bp
	or     %bp,%bp
	je   L_3c0e
L_3c03:
	cmp    %cx,0x0(%bp)
	jne   L_3bfc
	mov    -0x2(%bp),%si
	mov    -0x4(%bp),%di
L_3c0e:
	mov    %cs,%cx
	mov    %ds,%dx
	cmp    %dx,%cx
	je   L_3c2d
	mov    0x6(%bx),%bp
	or     %bp,%bp
	je   L_3c2d
	mov    %bp,0x2
	mov    0x8(%bx),%cx
	mov    %cx,0x0(%bp)
	mov    0xa(%bx),%cx
	mov    %cx,0x2(%bp)
L_3c2d:
	mov    (%bx),%bp
	mov    0x2(%bx),%sp
	pop    %cx
	pop    %cx
	mov    0x4(%bx),%cx
	push   %cx
	ret
	/* .byte 0 (alignment) */


L_3c3a:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall fork(2) */
	/* fork -> __NR_fork */
	mov	$__NR_fork,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3c48
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3c48:
	pop    %bp
	ret


L_3c4a:
	push   %bp
	mov    %sp,%bp
	mov    $0x16,%cx
	mov    0x4(%bp),%ax
	or     %ax,%ax
	jle   L_3c8d
	cmp    $0x20,%ax
	jg   L_3c8d
	mov    %ax,%cx
	shl    %ax
	add    $D_076a,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    0x6(%bp),%ax
	mov    %ax,(%bx)
	cmp    $0x2,%ax
	jl   L_3c79
	mov    $0x6,%ax
	mul    %cx
	add    $(L_3c96-6),%ax
L_3c79:
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
	jl   L_3c8a
	pop    %ax
	jmp   L_3c8b
L_3c8a:
	pop    %bx
L_3c8b:
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3c94
L_3c8d:
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	mov    $0xffff,%ax
L_3c94:
	pop    %bp
	ret
L_3c96:
	push   %ax
	mov    $D_076c,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_076e,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_0770,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_0772,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_0774,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_0776,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_0778,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_077a,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_077c,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_077e,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_0780,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_0782,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_0784,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_0786,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_0788,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_078a,%ax
	jmp   L_3cfa
	push   %ax
	mov    $D_078c,%ax
L_3cfa:
	jmp   L_3d54
	push   %ax
	mov    $D_078e,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_0790,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_0792,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_0794,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_0796,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_0798,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_079a,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_079c,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_079e,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_07a0,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_07a2,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_07a4,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_07a6,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_07a8,%ax
	jmp   L_3d54
	push   %ax
	mov    $D_07aa,%ax
L_3d54:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	sub    $D_076a,%ax
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


L_3d6a:
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
L_3d85:
	pop    %si
	pop    %bp
	ret


L_3d88:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall times(43) */
	mov    0x4(%bp),%ax
	/* times -> __NR_times */
	xchg	%ax,%bx
	mov	$__NR_times,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3d99
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3d99:
	pop    %bp
	ret
	/* .byte 0 (alignment) */
	call   L_3f04


L_3d9f:
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
	call   L_3f04


L_3dc8:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_3de6
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_3de6:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_3def:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_3def
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_3dff:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_3dff
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_3d9f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_3e40
	jne   L_3e35
	or     $0x0,%ax
	je   L_3e40
L_3e35:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_3e40:
	pop    %ax
	mov    $0x0,%dx
	ret


L_3e45:
	call   L_3f04


L_3e48:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_3e64
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_3e64:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_3e7b
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_3e7b:
	push   %dx
	push   %ax
	call   L_3dc8
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_3e95
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_3e95:
	ret
	call   L_3f04


L_3e99:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_3e48
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_3d9f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_3f04
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_3dc8
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_3d9f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_3f04:
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

L_3f3a:
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
	jns    L_3f4f
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_3f56
L_3f4f:
	mov    0x4(%bp),%bx
	mov    %bx,D_0660
L_3f56:
	pop    %bp
	ret


L_3f58:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_0660,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3f6f
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_3f7c
L_3f6f:
	mov    D_0660,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_0660
L_3f7c:
	pop    %bp
	ret


L_3f7e:
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
	jns    L_3f8f
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3f8f:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3f92:
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
	jns    L_3fa6
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3fa6:
	pop    %bp
	ret


L_3fa8:
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
	jns    L_3fbf
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3fbf:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3fc2:
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
	jns    L_3fd6
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3fd6:
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
	call   L_400a
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3ff1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_400a
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_400a:
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
	jns    L_4021
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4021:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4024:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall umask(60) */
	mov    0x4(%bp),%ax
	/* umask -> __NR_umask */
	xchg	%ax,%bx
	mov	$__NR_umask,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_4035
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4035:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4038:
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
	jns    L_4049
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4049:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_404c:
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
	jns    L_405c
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_405a:
	pop    %bp
	ret
L_405c:
	mov    0x4(%bp),%bx
	cmp    $0x0,%bx
	je   L_405a
	mov    %dx,(%bx)
	jmp   L_405a


L_4068:
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
	jns    L_407f
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_407f:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4082:
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
	jns    L_4093
	neg	%ax
	mov	%ax,D_0768	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4093:
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
	mov	%ax,D_0768	/* errno */
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

D_0000:
	.byte	0x00, 0x00	/* |..| */
D_0002:
	.byte	0xbf, 0x0f	/* |..| */
D_0004:
	.byte	0x00, 0x00	/* |..| */
D_0006:
	.ascii	"xnvtsierku\0"
	.byte	0x00	/* |.| */
D_0012:
	.byte	0x00, 0x08, 0x01, 0x00, 0x00, 0x10, 0x80, 0x00	/* |........| */
	.byte	0x00, 0x04	/* |..| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0x10, 0x00, 0x00, 0x01, 0x00, 0x20, 0x08, 0x00	/* |..... ..| */
	.byte	0x00, 0x00	/* |..| */
D_0028:
	.word	D_0200	/* ptr: 0x0200 */
D_002a:
	.byte	0xac	/* |.| */
D_002b:
	.byte	0x27	/* |'| */
D_002c:
	.ascii	"newline\0"
D_0034:
	.byte	0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x04	/* |..| */
D_003e:
	.byte	0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* | .......| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.word	D_0004	/* ptr: 0x0004 */
	.byte	0x40, 0x00, 0x81, 0x00, 0x08, 0x00, 0x02, 0x02	/* |@.......| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x08, 0x08, 0x00, 0x08, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x80, 0x00	/* |....| */
	.word	D_0002	/* ptr: 0x0002 */
	.byte	0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00	/* |........| */
D_00b4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x20, 0x00, 0x20, 0x20, 0x00, 0x00, 0x00	/* |. .  ...| */
	.byte	0x00, 0x00, 0x14, 0x82	/* |....| */
	.word	D_0200	/* ptr: 0x2200 */
	.byte	0x00, 0x00, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08	/* |........| */
	.byte	0x08, 0x08, 0x08, 0x08, 0x00, 0x00	/* |......| */
	.word	D_0200	/* ptr: 0x0200 */
	.byte	0x00, 0x32, 0x04, 0x40, 0x40, 0x40, 0x40, 0x40	/* |.2.@@@@@| */
	.byte	0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40	/* |@@@@@@@@| */
	.byte	0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40	/* |@@@@@@@@| */
	.byte	0x40, 0x40, 0x40, 0x40, 0x40, 0x10, 0x00, 0x00	/* |@@@@@...| */
	.byte	0x00, 0x40, 0x00, 0x40, 0x40, 0x40, 0x40, 0x40	/* |.@.@@@@@| */
	.byte	0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40	/* |@@@@@@@@| */
	.byte	0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40	/* |@@@@@@@@| */
	.byte	0x40, 0x40, 0x40, 0x40, 0x40, 0x01	/* |@@@@@.| */
	.word	D_0200	/* ptr: 0x0200 */
	.byte	0x00, 0x00	/* |..| */
D_0134:
	.byte	0x2f, 0x00	/* |/.| */
D_0136:
	.byte	0x00, 0x00	/* |..| */
D_0138:
	.byte	0x00, 0x00	/* |..| */
D_013a:
	.word	D_0002	/* ptr: 0x0002 */
D_013c:
	.byte	0x00, 0x00	/* |..| */
D_013e:
	.byte	0x2f, 0x74, 0x6d, 0x70, 0x2f, 0x73, 0x68	/* |/tmp/sh| */
D_0145:
	.byte	0x2d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |-.......| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00	/* |.....| */
D_0152:
	.word	D_071a	/* ptr: 0x271a */
	.byte	0xfe, 0xff, 0x0a, 0x56, 0x45, 0x52, 0x53, 0x49	/* |...VERSI| */
	.byte	0x4f, 0x4e, 0x20, 0x73, 0x79, 0x73, 0x31, 0x33	/* |ON sys13| */
	.byte	0x37, 0x09, 0x44, 0x41, 0x54, 0x45, 0x20, 0x31	/* |7.DATE 1| */
	.byte	0x39, 0x37, 0x38, 0x20, 0x4e	/* |978 N| */
D_0171:
	.byte	0x6f, 0x76, 0x20, 0x36, 0x20, 0x31, 0x34, 0x3a	/* |ov 6 14:| */
	.byte	0x32, 0x39, 0x3a, 0x32, 0x32, 0x0a, 0x00	/* |29:22..| */
D_0180:
	.ascii	"bad option(s)\0"
D_018e:
	.byte	0x79, 0x6f, 0x75, 0x20, 0x68, 0x61, 0x76, 0x65	/* |you have| */
	.byte	0x20, 0x6d, 0x61, 0x69, 0x6c, 0x0a, 0x00, 0x00	/* | mail...| */
D_019e:
	.ascii	"no space\0"
	.byte	0x00	/* |.| */
D_01a8:
	.ascii	"syntax error\0"
	.byte	0x00	/* |.| */
D_01b6:
	.ascii	"bad number\0"
	.byte	0x00	/* |.| */
D_01c2:
	.ascii	"parameter not set\0"
D_01d4:
	.byte	0x62, 0x61, 0x64, 0x20, 0x73, 0x75, 0x62, 0x73	/* |bad subs| */
	.byte	0x74, 0x69, 0x74, 0x75, 0x74, 0x69	/* |tituti| */
D_01e2:
	.byte	0x6f, 0x6e, 0x00, 0x00	/* |on..| */
D_01e6:
	.ascii	"cannot create\0"
D_01f4:
	.ascii	"illegal io\0"
	.byte	0x00	/* |.| */
D_0200:
	.ascii	"restricted\0"
	.byte	0x00	/* |.| */
D_020c:
	.byte	0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6d	/* |cannot m| */
	.byte	0x61, 0x6b, 0x65, 0x20, 0x70, 0x69	/* |ake pi| */
D_021a:
	.byte	0x70, 0x65, 0x00, 0x00	/* |pe..| */
D_021e:
	.ascii	"cannot open\0"
D_022a:
	.ascii	" - core dumped\0"
	.byte	0x00	/* |.| */
D_023a:
	.ascii	"arg list too long\0"
D_024c:
	.ascii	"text busy\0"
D_0256:
	.ascii	"too big\0"
D_025e:
	.ascii	"cannot execute\0"
	.byte	0x00	/* |.| */
D_026e:
	.ascii	"not found\0"
D_0278:
	.byte	0x62, 0x61, 0x64, 0x20, 0x66, 0x69, 0x6c, 0x65	/* |bad file| */
D_0280:
	.ascii	" number\0"
D_0288:
	.byte	0x63, 0x61, 0x6e, 0x6e, 0x6f	/* |canno| */
D_028d:
	.ascii	"t shift\0"
	.byte	0x00	/* |.| */
D_0296:
	.ascii	"bad directory\0"
D_02a4:
	.ascii	"bad trap\0"
	.byte	0x00	/* |.| */
D_02ae:
	.ascii	"is read only\0"
	.byte	0x00	/* |.| */
D_02bc:
	.ascii	"is not an identifier\0"
	.byte	0x00	/* |.| */
D_02d2:
	.ascii	"PATH\0"
	.byte	0x00	/* |.| */
D_02d8:
	.ascii	"HOME\0"
	.byte	0x00	/* |.| */
D_02de:
	.ascii	"MAIL\0"
	.byte	0x00	/* |.| */
D_02e4:
	.ascii	"FILEMATCH\0"
D_02ee:
	.byte	0x49, 0x46, 0x53, 0x00	/* |IFS.| */
D_02f2:
	.byte	0x50, 0x53, 0x31, 0x00	/* |PS1.| */
D_02f6:
	.byte	0x50, 0x53, 0x32, 0x00	/* |PS2.| */
D_02fa:
	.byte	0x00, 0x00	/* |..| */
D_02fc:
	.byte	0x20, 0x09, 0x0a, 0x00	/* | ...| */
D_0300:
	.ascii	":/bin:/usr/bin\0"
	.byte	0x00	/* |.| */
D_0310:
	.byte	0x3a, 0x20, 0x00, 0x00	/* |: ..| */
D_0314:
	.byte	0x2d, 0x00	/* |-.| */
D_0316:
	.ascii	"end of file\0"
D_0322:
	.ascii	" unexpected\0"
D_032e:
	.ascii	" at line \0"
D_0338:
	.ascii	"/dev/null\0"
D_0342:
	.word	D_002b	/* ptr: 0x202b */
	.byte	0x00, 0x00	/* |..| */
D_0346:
	.word	D_003e	/* ptr: 0x203e */
	.byte	0x00, 0x00	/* |..| */
D_034a:
	.byte	0x24, 0x20, 0x00, 0x00	/* |$ ..| */
D_034e:
	.byte	0x23, 0x20, 0x00, 0x00	/* |# ..| */
D_0352:
	.ascii	".profile\0"
	.byte	0x00	/* |.| */
D_035c:
	.word	D_0424	/* ptr: 0x2424 */
	.byte	0x0a, 0x01	/* |..| */
	.word	D_0427	/* ptr: 0x2427 */
	.byte	0x22, 0x01	/* |".| */
	.word	D_042c	/* ptr: 0x242c */
	.byte	0x0f, 0x01	/* |..| */
	.word	D_0431	/* ptr: 0x2431 */
	.byte	0x1d, 0x01	/* |..| */
	.word	D_0435	/* ptr: 0x2435 */
	.byte	0x21, 0x01	/* |!.| */
	.word	D_043a	/* ptr: 0x243a */
	.byte	0x1e, 0x01	/* |..| */
	.word	D_043d	/* ptr: 0x243d */
	.byte	0x1b, 0x01	/* |..| */
	.word	D_0443	/* ptr: 0x2443 */
	.byte	0x05, 0x01	/* |..| */
	.word	D_0446	/* ptr: 0x2446 */
	.byte	0x24, 0x01	/* |$.| */
	.word	D_044b	/* ptr: 0x244b */
	.byte	0x11, 0x01	/* |..| */
	.word	D_0450	/* ptr: 0x2450 */
	.byte	0x12, 0x01	/* |..| */
	.word	D_0455	/* ptr: 0x2455 */
	.byte	0x10, 0x01	/* |..| */
	.word	D_0458	/* ptr: 0x2458 */
	.byte	0x17, 0x01	/* |..| */
	.word	D_045e	/* ptr: 0x245e */
	.byte	0x06, 0x01	/* |..| */
	.word	D_0460	/* ptr: 0x2460 */
	.byte	0x28, 0x01, 0x00, 0x00, 0x00, 0x00	/* |(.....| */
D_039c:
	.byte	0x00, 0x00	/* |..| */
	.word	D_0462	/* ptr: 0x2462 */
	.byte	0x00, 0x00	/* |..| */
	.word	D_0469	/* ptr: 0x2469 */
	.word	D_046e	/* ptr: 0x246e */
	.word	D_0482	/* ptr: 0x2482 */
	.word	D_0491	/* ptr: 0x2491 */
	.word	D_04b3	/* ptr: 0x24b3 */
	.word	D_04c2	/* ptr: 0x24c2 */
	.word	D_04d5	/* ptr: 0x24d5 */
	.word	D_04dc	/* ptr: 0x24dc */
	.word	D_04e6	/* ptr: 0x24e6 */
	.word	D_04f3	/* ptr: 0x24f3 */
	.byte	0x00, 0x00	/* |..| */
	.word	D_0503	/* ptr: 0x2503 */
	.word	D_050e	/* ptr: 0x250e */
	.byte	0x19, 0x25	/* |.%| */
D_03be:
	.ascii	"export\0"
	.byte	0x00	/* |.| */
D_03c6:
	.ascii	"readonly\0"
	.byte	0x00	/* |.| */
D_03d0:
	.word	D_052a	/* ptr: 0x252a */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_052d	/* ptr: 0x252d */
	.byte	0x11, 0x00	/* |..| */
	.word	D_0532	/* ptr: 0x2532 */
	.byte	0x01, 0x00	/* |..| */
	.word	D_0536	/* ptr: 0x2536 */
	.byte	0x10, 0x00	/* |..| */
	.word	D_0538	/* ptr: 0x2538 */
	.byte	0x05, 0x00	/* |..| */
	.word	D_053d	/* ptr: 0x253d */
	.word	D_0004	/* ptr: 0x0004 */
	.word	D_0543	/* ptr: 0x2543 */
	.byte	0x08, 0x00	/* |..| */
	.word	D_0548	/* ptr: 0x2548 */
	.byte	0x0b, 0x00	/* |..| */
	.word	D_054d	/* ptr: 0x254d */
	.byte	0x0c, 0x00	/* |..| */
	.word	D_054f	/* ptr: 0x254f */
	.word	D_0004	/* ptr: 0x0004 */
	.word	D_03c6	/* ptr: 0x23c6 */
	.byte	0x0d, 0x00	/* |..| */
	.word	D_03be	/* ptr: 0x23be */
	.byte	0x0f, 0x00	/* |..| */
	.word	D_0556	/* ptr: 0x2556 */
	.word	D_0002	/* ptr: 0x0002 */
	.word	D_055c	/* ptr: 0x255c */
	.byte	0x0a, 0x00	/* |..| */
	.word	D_0562	/* ptr: 0x2562 */
	.byte	0x09, 0x00	/* |..| */
	.word	D_056b	/* ptr: 0x256b */
	.byte	0x07, 0x00	/* |..| */
	.word	D_0571	/* ptr: 0x2571 */
	.word	D_0006	/* ptr: 0x0006 */
	.word	D_0576	/* ptr: 0x2576 */
	.byte	0x03, 0x00	/* |..| */
	.word	D_057b	/* ptr: 0x257b */
	.byte	0x0e, 0x00	/* |..| */
	.word	D_0581	/* ptr: 0x2581 */
	.byte	0x13, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_0424:
	.byte	0x69, 0x6e, 0x00	/* |in.| */
D_0427:
	.ascii	"esac\0"
D_042c:
	.ascii	"case\0"
D_0431:
	.byte	0x66, 0x6f, 0x72, 0x00	/* |for.| */
D_0435:
	.ascii	"done\0"
D_043a:
	.byte	0x69, 0x66, 0x00	/* |if.| */
D_043d:
	.ascii	"while\0"
D_0443:
	.byte	0x64, 0x6f, 0x00	/* |do.| */
D_0446:
	.ascii	"then\0"
D_044b:
	.ascii	"else\0"
D_0450:
	.ascii	"elif\0"
D_0455:
	.byte	0x66, 0x69, 0x00	/* |fi.| */
D_0458:
	.ascii	"until\0"
D_045e:
	.byte	0x7b, 0x00	/* |{.| */
D_0460:
	.byte	0x7d, 0x00	/* |}.| */
D_0462:
	.ascii	"Hangup\0"
D_0469:
	.ascii	"Quit\0"
D_046e:
	.ascii	"Illegal instruction\0"
D_0482:
	.ascii	"Trace/BPT trap\0"
D_0491:
	.ascii	"Asynchronous I/O error (IOT trap)\0"
D_04b3:
	.ascii	"Stack overflow\0"
D_04c2:
	.ascii	"Floating exception\0"
D_04d5:
	.ascii	"Killed\0"
D_04dc:
	.ascii	"Bus error\0"
D_04e6:
	.ascii	"Memory fault\0"
D_04f3:
	.ascii	"Bad system call\0"
D_0503:
	.ascii	"Alarm call\0"
D_050e:
	.ascii	"Terminated\0"
	.ascii	"Asynchronous I/O\0"
D_052a:
	.byte	0x63, 0x64, 0x00	/* |cd.| */
D_052d:
	.ascii	"read\0"
D_0532:
	.byte	0x73, 0x65, 0x74, 0x00	/* |set.| */
D_0536:
	.byte	0x3a, 0x00	/* |:.| */
D_0538:
	.ascii	"trap\0"
D_053d:
	.ascii	"login\0"
D_0543:
	.ascii	"wait\0"
D_0548:
	.ascii	"eval\0"
D_054d:
	.byte	0x2e, 0x00	/* |..| */
D_054f:
	.ascii	"newgrp\0"
D_0556:
	.ascii	"chdir\0"
D_055c:
	.ascii	"break\0"
D_0562:
	.ascii	"continue\0"
D_056b:
	.ascii	"shift\0"
D_0571:
	.ascii	"exit\0"
D_0576:
	.ascii	"exec\0"
D_057b:
	.ascii	"times\0"
D_0581:
	.ascii	"umask\0"
	.byte	0x00	/* |.| */
D_0588:
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
	.word	D_02f6	/* ptr: 0x22f6 */
D_058e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
	.word	D_02e4	/* ptr: 0x22e4 */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
	.word	D_02d2	/* ptr: 0x22d2 */
D_05a6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_05ac:
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
	.word	D_02ee	/* ptr: 0x22ee */
D_05b2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_05b8:
	.byte	0xa0, 0x25, 0x88, 0x25	/* |.%.%| */
	.word	D_02f2	/* ptr: 0x22f2 */
D_05be:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x94, 0x25	/* |.......%| */
	.byte	0xac, 0x25	/* |.%| */
	.word	D_02d8	/* ptr: 0x22d8 */
D_05ca:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc4, 0x25	/* |.......%| */
	.byte	0xb8, 0x25	/* |.%| */
	.word	D_02de	/* ptr: 0x22de */
D_05d6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_05dc:
	.byte	0xd0, 0x25	/* |.%| */
D_05de:
	.byte	0x00, 0x00	/* |..| */
D_05e0:
	.byte	0x00, 0x00	/* |..| */
D_05e2:
	.byte	0x00, 0x00	/* |..| */
D_05e4:
	.byte	0x3d, 0x00	/* |=.| */
D_05e6:
	.byte	0x00, 0x00	/* |..| */
D_05e8:
	.byte	0x00, 0x00	/* |..| */
D_05ea:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0612:
	.byte	0x00, 0x00	/* |..| */
D_0614:
	.ascii	"ptrace: \0"
	.byte	0x00	/* |.| */
D_061e:
	.byte	0xfa, 0x22	/* |."| */
D_0620:
	.byte	0x00, 0x00, 0x36, 0x35, 0xb0, 0x34, 0x81, 0x34	/* |..65.4.4| */
	.byte	0x9d, 0x34, 0x9a, 0x33, 0x4f, 0x33, 0xf3, 0x34	/* |.4.3O3.4| */
	.byte	0x1d, 0x35, 0x6d, 0x33, 0x7a, 0x33, 0xbe, 0x35	/* |.5m3z3.5| */
	.byte	0xec, 0x32, 0x76, 0x35, 0x27, 0x33, 0x7c, 0x35	/* |.2v5'3|5| */
	.byte	0x66, 0x33, 0x27, 0x35, 0x59, 0x36, 0xd6, 0x35	/* |f3'5Y6.5| */
	.byte	0x39, 0x32, 0x04, 0x38, 0x26, 0x38, 0x6a, 0x38	/* |92.8&8j8| */
	.byte	0xe7, 0x39, 0x72, 0x39, 0x72, 0x39, 0x13, 0x3a	/* |.9r9r9.:| */
	.byte	0x84, 0x38, 0xa2, 0x38, 0x9a, 0x36, 0xc1, 0x38	/* |.8.8.6.8| */
D_0660:
	.word	D_07ac	/* ptr: 0x27ac */

	.bss

D_0662:	.space	2	/* bss word @ 0x0662 */
D_0664:	.space	2	/* bss word @ 0x0664 */
D_0666:	.space	2	/* bss word @ 0x0666 */
D_0668:	.space	2	/* bss word @ 0x0668 */
D_066a:	.space	2	/* bss word @ 0x066a */
D_066c:	.space	2	/* bss word @ 0x066c */
D_066e:	.space	2	/* bss word @ 0x066e */
D_0670:	.space	2	/* bss word @ 0x0670 */
D_0672:	.space	2	/* bss word @ 0x0672 */
D_0674:	.space	2	/* bss word @ 0x0674 */
	.space	4
D_067a:	.space	2	/* bss word @ 0x067a */
	.space	8
D_0684:	.space	2	/* bss word @ 0x0684 */
D_0686:	.space	2	/* bss word @ 0x0686 */
D_0688:	.space	2	/* bss word @ 0x0688 */
D_068a:	.space	2	/* bss word @ 0x068a */
D_068c:	.space	2	/* bss word @ 0x068c */
	.space	4
D_0692:	.space	2	/* bss word @ 0x0692 */
	.space	10
D_069e:	.space	2	/* bss word @ 0x069e */
D_06a0:	.space	2	/* bss word @ 0x06a0 */
D_06a2:	.space	2	/* bss word @ 0x06a2 */
D_06a4:	.space	2	/* bss word @ 0x06a4 */
D_06a6:	.space	2	/* bss word @ 0x06a6 */
D_06a8:	.space	2	/* bss word @ 0x06a8 */
D_06aa:	.space	2	/* bss word @ 0x06aa */
D_06ac:	.space	2	/* bss word @ 0x06ac */
D_06ae:	.space	2	/* bss word @ 0x06ae */
	.space	2
D_06b2:	.space	2	/* bss word @ 0x06b2 */
	.space	10
D_06be:	.space	2	/* bss word @ 0x06be */
D_06c0:	.space	2	/* bss word @ 0x06c0 */
D_06c2:	.space	2	/* bss word @ 0x06c2 */
D_06c4:	.space	2	/* bss word @ 0x06c4 */
	.space	4
D_06ca:	.space	2	/* bss word @ 0x06ca */
	.space	32
D_06ec:	.space	2	/* bss word @ 0x06ec */
D_06ee:	.space	2	/* bss word @ 0x06ee */
	.space	2
D_06f2:	.space	2	/* bss word @ 0x06f2 */
	.space	18
D_0706:	.space	2	/* bss word @ 0x0706 */
	.space	4
D_070c:	.space	2	/* bss word @ 0x070c */
D_070e:	.space	2	/* bss word @ 0x070e */
D_0710:	.space	2	/* bss word @ 0x0710 */
D_0712:	.space	2	/* bss word @ 0x0712 */
	.space	6
D_071a:	.space	2	/* bss word @ 0x071a */
	.space	76
D_0768:	.space	2	/* bss word @ 0x0768 */
D_076a:	.space	2	/* bss word @ 0x076a */
D_076c:	.space	2	/* bss word @ 0x076c */
D_076e:	.space	2	/* bss word @ 0x076e */
D_0770:	.space	2	/* bss word @ 0x0770 */
D_0772:	.space	2	/* bss word @ 0x0772 */
D_0774:	.space	2	/* bss word @ 0x0774 */
D_0776:	.space	2	/* bss word @ 0x0776 */
D_0778:	.space	2	/* bss word @ 0x0778 */
D_077a:	.space	2	/* bss word @ 0x077a */
D_077c:	.space	2	/* bss word @ 0x077c */
D_077e:	.space	2	/* bss word @ 0x077e */
D_0780:	.space	2	/* bss word @ 0x0780 */
D_0782:	.space	2	/* bss word @ 0x0782 */
D_0784:	.space	2	/* bss word @ 0x0784 */
D_0786:	.space	2	/* bss word @ 0x0786 */
D_0788:	.space	2	/* bss word @ 0x0788 */
D_078a:	.space	2	/* bss word @ 0x078a */
D_078c:	.space	2	/* bss word @ 0x078c */
D_078e:	.space	2	/* bss word @ 0x078e */
D_0790:	.space	2	/* bss word @ 0x0790 */
D_0792:	.space	2	/* bss word @ 0x0792 */
D_0794:	.space	2	/* bss word @ 0x0794 */
D_0796:	.space	2	/* bss word @ 0x0796 */
D_0798:	.space	2	/* bss word @ 0x0798 */
D_079a:	.space	2	/* bss word @ 0x079a */
D_079c:	.space	2	/* bss word @ 0x079c */
D_079e:	.space	2	/* bss word @ 0x079e */
D_07a0:	.space	2	/* bss word @ 0x07a0 */
D_07a2:	.space	2	/* bss word @ 0x07a2 */
D_07a4:	.space	2	/* bss word @ 0x07a4 */
D_07a6:	.space	2	/* bss word @ 0x07a6 */
D_07a8:	.space	2	/* bss word @ 0x07a8 */
D_07aa:	.space	2	/* bss word @ 0x07aa */
D_07ac:	/* endcore: initial break */

/* end of file */
