/* nm.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v10
 * Assemble: ia16-elf-as -march=8087 nm.s -o nm.o
 * Syscalls used:
 *   1:exit            3:read            4:write           5:open            6:close           8:creat
 *   17:brk            19:lseek          28:fstat          54:ioctl
 * INT instructions: $0xf1  $0xf2  $0xf4
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: nm  [linked executable]
//   Magic:    0x0107  OMAGIC (object/impure)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00002482  (9346 bytes)
//   a_data:   0x0000040c  (1036 bytes)
//   a_bss:    0x00000496  (1174 bytes)
//   a_syms:   0x00000000  (0 bytes) [stripped]
//   a_entry:  0x00002000
//   a_trsize: 0x00000000  (0 text relocs)
//   a_drsize: 0x00000000  (0 data relocs)
//   File layout:
//   .bss:  1174 bytes (zero-filled by kernel, not in file)
//   Load:  cs:0x0000 (text)   ds:0x4482 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	/* vrewrite: initialize _environ from envp (TLVC crt0 3rd arg) */
	mov	0x8(%bp),%ax
	mov	%ax,D_040c
	push   %si
L_003a:
	push   %di
	cmp    $0x7f,%sp
	ja   L_0042
	int    $0xf2
L_0042:
	decw   0x4(%bp)
	jg   L_004a
	jmp   L_00dd
L_004a:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x2d,(%bx)
	je   L_0058
	jmp   L_00dd
L_0058:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x0,0x1(%bx)
	je   L_00dd
L_0064:
	addw   $0x2,0x6(%bp)
L_0068:
	mov    0x6(%bp),%bx
	incw   (%bx)
	mov    (%bx),%bx
L_006f:
	cmpb   $0x0,(%bx)
L_0072:
	je   L_00da
L_0074:
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	mov    (%bx),%al
	cbtw
	jmp   L_00cb
L_007e:
	/* TODO: data ref: incw   0x4896 */
	incw   D_0414
L_0082:
	jmp   L_0068
L_0084:
	/* TODO: data ref: incw   0x490e */
	incw   D_048c
	jmp   L_0068
L_008a:
	/* TODO: data ref: incw   0x4906 */
	incw   D_0484
	jmp   L_0068
L_0090:
	/* TODO: data ref: movw   $0xffff,0x449c */
	/* movw constant -> D_001a */
	mov	$0xffff,%ax
	mov	%ax,D_001a
	jmp   L_0068
L_0098:
	/* TODO: data ref: incw   0x4892 */
	incw   D_0410
	jmp   L_0068
L_009e:
	/* TODO: data ref: incw   0x4894 */
	incw   D_0412
	jmp   L_0068
L_00a4:
	/* TODO: data ref: incw   0x48da */
	incw   D_0458
	jmp   L_0068
L_00aa:
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	mov    (%bx),%al
	cbtw
	push   %ax
	/* TODO: data ref: mov    $0x453c,%ax */
	mov    $D_00ba,%ax
	push   %ax
	/* TODO: data ref: mov    $0x47f4,%ax */
	mov    $D_0372,%ax
	push   %ax
	call   L_0ada
	add    $0x6,%sp
	mov    $0x2,%ax
	push   %ax
	call   _exit
	pop    %cx
	jmp   L_0082
L_00cb:
	sub    $0x61,%ax
	cmp    $0x14,%ax
	ja   L_00aa
	shl    $1,%ax
	xchg   %ax,%bx
	/* TODO: data ref: jmp    *0x449e(%bx) */
	jmp    *D_001c(%bx)
L_00da:
	decw   0x4(%bp)
L_00dd:
	cmpw   $0x0,0x4(%bp)
	jne   L_00f0
	movw   $0x1,0x4(%bp)
	mov    0x6(%bp),%bx
	/* TODO: data ref: movw   $0x4556,0x2(%bx) */
	movw   $D_00d4,0x2(%bx)
L_00f0:
	mov    0x4(%bp),%ax
	/* TODO: data ref: mov    %ax,0x48de */
	mov    %ax,D_045c
	mov    0x6(%bp),%ax
	/* TODO: data ref: mov    %ax,0x4910 */
	mov    %ax,D_048e
L_00fc:
	mov    0x4(%bp),%ax
	decw   0x4(%bp)
	or     %ax,%ax
	je   L_0110
	/* TODO: data ref: addw   $0x2,0x4910 */
	addw   $0x2,D_048e
	call   L_011c
	jmp   L_00fc
L_0110:
	/* TODO: data ref: push   0x4890 */
	push   D_040e
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_011c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x36,%sp
	cmp    $0x7f,%sp
	ja   L_012b
	int    $0xf2
L_012b:
	/* TODO: data ref: movw   $0x0,0x4908 */
	movw   $0x0,D_0486
	/* TODO: data ref: mov    $0x455c,%ax */
	mov    $D_00da,%ax
	push   %ax
	/* TODO: data ref: mov    0x4910,%bx */
	mov    D_048e,%bx
	push   (%bx)
	call   L_0b06
	add    $0x4,%sp
	/* TODO: data ref: mov    %ax,0x490a */
	mov    %ax,D_0488
	or     %ax,%ax
	jne   L_0157
	/* TODO: data ref: mov    $0x455e,%ax */
	mov    $D_00dc,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_08c4
	add    $0x4,%sp
	jmp   L_018e
L_0157:
	/* TODO: data ref: movw   $0x8,0x4912 */
	movw   $0x8,D_0490
	/* TODO: data ref: movw   $0x0,0x4914 */
	movw   $0x0,D_0492
	/* TODO: data ref: push   0x490a */
	push   D_0488
	mov    $0x20,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x48e6,%ax */
	mov    $D_0464,%ax
	push   %ax
	call   L_0fee
	add    $0x8,%sp
	/* TODO: data ref: cmpw   $0xff65,0x48e6 */
	cmpw   $0xff65,D_0464
	jne   L_0190
	/* TODO: data ref: mov    $0x456a,%ax */
	mov    $D_00e8,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_08c4
	add    $0x4,%sp
L_018e:
	jmp   L_01d9
L_0190:
	mov    $0x8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x4576,%ax */
	mov    $D_00f4,%ax
	push   %ax
	/* TODO: data ref: mov    $0x48e6,%ax */
	mov    $D_0464,%ax
	push   %ax
	call   L_1174
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_01ac
	/* TODO: data ref: incw   0x4908 */
	incw   D_0486
	jmp   L_01dc
L_01ac:
	/* TODO: data ref: cmpw   $0x107,0x48e6 */
	cmpw   $0x107,D_0464
	je   L_01c1
	/* TODO: data ref: cmpw   $0x109,0x48e6 */
	cmpw   $0x109,D_0464
	je   L_01c1
	mov    $0x1,%ax
	jmp   L_01c3
L_01c1:
	xor    %ax,%ax
L_01c3:
	cwtd
	or     %dx,%dx
	jne   L_01ca
	or     %ax,%ax
L_01ca:
	je   L_01dc
	/* TODO: data ref: mov    $0x457f,%ax */
	mov    $D_00fd,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_08c4
	add    $0x4,%sp
L_01d9:
	jmp   L_05a1
L_01dc:
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: push   0x490a */
	push   D_0488
	call   L_0c1a
	add    $0x8,%sp
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	je   L_0215
	/* TODO: data ref: push   0x490a */
	push   D_0488
	call   L_0823
	pop    %cx
	/* TODO: data ref: cmpw   $0x1,0x48de */
	cmpw   $0x1,D_045c
	jle   L_0215
	/* TODO: data ref: mov    0x4910,%bx */
	mov    D_048e,%bx
	push   (%bx)
	/* TODO: data ref: mov    $0x458a,%ax */
	mov    $D_0108,%ax
	push   %ax
	call   L_0dce
	add    $0x4,%sp
L_0215:
	movw   $0x0,-0xe(%bp)
	/* TODO: data ref: push   0x490a */
	push   D_0488
	mov    $0x20,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x48e6,%ax */
	mov    $D_0464,%ax
	push   %ax
	call   L_0fee
	add    $0x8,%sp
	/* TODO: data ref: cmpw   $0x107,0x48e6 */
	cmpw   $0x107,D_0464
	je   L_0245
	/* TODO: data ref: cmpw   $0x109,0x48e6 */
	cmpw   $0x109,D_0464
	je   L_0245
	mov    $0x1,%ax
	jmp   L_0247
L_0245:
	xor    %ax,%ax
L_0247:
	cwtd
	or     %dx,%dx
	jne   L_024e
	or     %ax,%ax
L_024e:
	je   L_0253
	jmp   L_02db
L_0253:
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	jne   L_026d
	lea    -0x36(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    0x490a,%bx */
	mov    D_0488,%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_237e
	add    $0x4,%sp
L_026d:
	/* TODO: data ref: mov    0x48ea,%ax */
	mov    D_0468,%ax
	/* TODO: data ref: mov    0x48ec,%dx */
	mov    D_046a,%dx
	add    $0x20,%ax
	adc    $0x0,%dx
	/* TODO: data ref: add    0x48ee,%ax */
	add    D_046c,%ax
	/* TODO: data ref: adc    0x48f0,%dx */
	adc    D_046e,%dx
	/* TODO: data ref: add    0x48fe,%ax */
	add    D_047c,%ax
	/* TODO: data ref: adc    0x4900,%dx */
	adc    D_047e,%dx
	/* TODO: data ref: add    0x4902,%ax */
	add    D_0480,%ax
	/* TODO: data ref: adc    0x4904,%dx */
	adc    D_0482,%dx
	sub    $0x20,%ax
	sbb    $0x0,%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    $0x1,%ax
	push   %ax
	push   -0x6(%bp)
	push   -0x8(%bp)
	/* TODO: data ref: push   0x490a */
	push   D_0488
	call   L_0c1a
	add    $0x8,%sp
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: push   0x48f8 */
	push   D_0476
	/* TODO: data ref: push   0x48f6 */
	push   D_0474
	call   L_18d2
	add    $0x8,%sp
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_02de
	/* TODO: data ref: mov    $0x4590,%ax */
	mov    $D_010e,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_08c4
	add    $0x4,%sp
L_02db:
	jmp   L_043d
L_02de:
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	je   L_02ee
	/* TODO: data ref: mov    0x4912,%ax */
	mov    D_0490,%ax
	/* TODO: data ref: mov    0x4914,%dx */
	mov    D_0492,%dx
	jmp   L_02f4
L_02ee:
	mov    -0x28(%bp),%ax
	mov    -0x26(%bp),%dx
L_02f4:
	mov    %ax,-0x3a(%bp)
	mov    %dx,-0x38(%bp)
	/* TODO: data ref: mov    0x48ea,%ax */
	mov    D_0468,%ax
	/* TODO: data ref: mov    0x48ec,%dx */
	mov    D_046a,%dx
	add    $0x20,%ax
	adc    $0x0,%dx
	/* TODO: data ref: add    0x48ee,%ax */
	add    D_046c,%ax
	/* TODO: data ref: adc    0x48f0,%dx */
	adc    D_046e,%dx
	/* TODO: data ref: add    0x48fe,%ax */
	add    D_047c,%ax
	/* TODO: data ref: adc    0x4900,%dx */
	adc    D_047e,%dx
	/* TODO: data ref: add    0x4902,%ax */
	add    D_0480,%ax
	/* TODO: data ref: adc    0x4904,%dx */
	adc    D_0482,%dx
	/* TODO: data ref: add    0x48f6,%ax */
	add    D_0474,%ax
	/* TODO: data ref: adc    0x48f8,%dx */
	adc    D_0476,%dx
	add    $0x4,%ax
	adc    $0x0,%dx
	cmp    -0x38(%bp),%dx
	jl   L_0347
	jg   L_0339
	cmp    -0x3a(%bp),%ax
	jbe   L_0347
L_0339:
	/* TODO: data ref: mov    $0x459d,%ax */
	mov    $D_011b,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_08c4
	add    $0x4,%sp
L_0347:
	xor    %si,%si
	/* TODO: data ref: cmp    %si,0x48dc */
	cmp    %si,D_045a
	je   L_035d
	/* TODO: data ref: push   0x48dc */
	push   D_045a
	call   L_221e
	pop    %cx
	/* TODO: data ref: movw   $0x0,0x48dc */
	movw   $0x0,D_045a
L_035d:
	decw   -0xa(%bp)
	jge   L_0365
	jmp   L_040d
L_0365:
	/* TODO: data ref: push   0x490a */
	push   D_0488
	mov    $0xa,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	lea    -0x18(%bp),%ax
	push   %ax
	call   L_0fee
	add    $0x8,%sp
	/* TODO: data ref: cmpw   $0x0,0x490e */
	cmpw   $0x0,D_048c
	je   L_038b
	mov    -0x16(%bp),%al
	cbtw
	test   $0x1,%ax
	je   L_035d
L_038b:
	mov    -0x16(%bp),%al
	cbtw
	test   $0xe0,%ax
	je   L_03a9
	/* TODO: data ref: cmpw   $0x0,0x48da */
	cmpw   $0x0,D_0458
	je   L_035d
	/* TODO: data ref: cmpw   $0x0,0x490e */
	cmpw   $0x0,D_048c
	jne   L_035d
	/* TODO: data ref: cmpw   $0x0,0x4906 */
	cmpw   $0x0,D_0484
	jne   L_035d
L_03a9:
	cmpw   $0x0,-0xe(%bp)
	jne   L_03bc
	mov    $0xa,%ax
	push   %ax
	call   L_20d2
	pop    %cx
	mov    %ax,-0xe(%bp)
	jmp   L_03d1
L_03bc:
	lea    0x1(%si),%ax
	mov    $0xa,%dx
	imul   %dx
	push   %ax
	push   -0xe(%bp)
	call   L_223a
	add    $0x4,%sp
	mov    %ax,-0xe(%bp)
L_03d1:
	cmpw   $0x0,-0xe(%bp)
	jne   L_03e5
	/* TODO: data ref: mov    $0x45cf,%ax */
	mov    $D_014d,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_08c4
	add    $0x4,%sp
L_03e5:
	lea    -0x18(%bp),%ax
	mov    %ax,-0x3a(%bp)
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xe(%bp),%ax
	mov    %ax,%bx
	push   %si
	lea    (%bx),%bx
	mov    -0x3a(%bp),%si
	mov    $0xa,%cx
L_0400:
	mov    (%si),%al
	mov    %al,(%bx)
	inc    %bx
	inc    %si
	loop   L_0400
	pop    %si
	inc    %si
	jmp   L_035d
L_040d:
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	je   L_0440
	/* TODO: data ref: push   0x490a */
	push   D_0488
	call   L_0a26
	pop    %cx
	add    $0x4,%ax
	adc    $0x0,%dx
	/* TODO: data ref: cmp    0x4914,%dx */
	cmp    D_0492,%dx
	jl   L_0440
	jg   L_0430
	/* TODO: data ref: cmp    0x4912,%ax */
	cmp    D_0490,%ax
	jb   L_0440
L_0430:
	/* TODO: data ref: mov    $0x45dd,%ax */
	mov    $D_015b,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_08c4
	add    $0x4,%sp
L_043d:
	jmp   L_0583
L_0440:
	/* TODO: data ref: push   0x490a */
	push   D_0488
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x490c,%ax */
	mov    $D_048a,%ax
	push   %ax
	call   L_0fee
	add    $0x8,%sp
	cmp    $0x1,%ax
	je   L_046b
	/* TODO: data ref: mov    $0x45fe,%ax */
	mov    $D_017c,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_08c4
	add    $0x4,%sp
	jmp   L_0599
L_046b:
	/* TODO: data ref: push   0x490c */
	push   D_048a
	call   L_20d2
	pop    %cx
	/* TODO: data ref: mov    %ax,0x48dc */
	mov    %ax,D_045a
	or     %ax,%ax
	jne   L_0488
	/* TODO: data ref: mov    $0x461f,%ax */
	mov    $D_019d,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_08c4
	add    $0x4,%sp
L_0488:
	/* TODO: data ref: push   0x490a */
	push   D_0488
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    0x490c,%ax */
	mov    D_048a,%ax
	dec    %ax
	dec    %ax
	push   %ax
	/* TODO: data ref: mov    0x48dc,%ax */
	mov    D_045a,%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_0fee
	add    $0x8,%sp
	cmp    $0x1,%ax
	je   L_04b5
	/* TODO: data ref: mov    $0x4631,%ax */
	mov    $D_01af,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_08c4
	add    $0x4,%sp
L_04b5:
	xor    %di,%di
L_04b7:
	cmp    %si,%di
	jge   L_0507
	mov    %di,%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xe(%bp),%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	je   L_04f4
	mov    %di,%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xe(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	/* TODO: data ref: add    0x48dc,%ax */
	add    D_045a,%ax
	mov    %ax,-0x3a(%bp)
	mov    %di,%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xe(%bp),%ax
	mov    %ax,%bx
	mov    -0x3a(%bp),%ax
	mov    %ax,(%bx)
	jmp   L_0504
L_04f4:
	mov    %di,%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xe(%bp),%ax
	mov    %ax,%bx
L_0500:
	/* TODO: data ref: movw   $0x464c,(%bx) */
	movw   $D_01ca,(%bx)
L_0504:
	inc    %di
	jmp   L_04b7
L_0507:
	/* TODO: data ref: cmpw   $0x0,0x4892 */
	cmpw   $0x0,D_0410
	jne   L_0520
	mov    $0x27bc,%ax
	push   %ax
	mov    $0xa,%ax
	push   %ax
	push   %si
	push   -0xe(%bp)
	call   L_0df8
	add    $0x8,%sp
L_0520:
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	jne   L_052e
	/* TODO: data ref: cmpw   $0x1,0x48de */
	cmpw   $0x1,D_045c
	jle   L_0552
L_052e:
	/* TODO: data ref: cmpw   $0x0,0x4894 */
	cmpw   $0x0,D_0412
	jne   L_0552
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	je   L_0541
	/* TODO: data ref: mov    $0x489e,%ax */
	mov    $D_041c,%ax
	jmp   L_0547
L_0541:
	/* TODO: data ref: mov    0x4910,%bx */
	mov    D_048e,%bx
	mov    (%bx),%ax
L_0547:
	push   %ax
	/* TODO: data ref: mov    $0x464d,%ax */
	mov    $D_01cb,%ax
	push   %ax
	call   L_0dce
	add    $0x4,%sp
L_0552:
	push   %si
	push   -0xe(%bp)
	call   L_05a8
	add    $0x4,%sp
	cmpw   $0x0,-0xe(%bp)
	je   L_056e
	push   -0xe(%bp)
	call   L_221e
	pop    %cx
	movw   $0x0,-0xe(%bp)
L_056e:
	/* TODO: data ref: cmpw   $0x0,0x48dc */
	cmpw   $0x0,D_045a
	je   L_0583
	/* TODO: data ref: push   0x48dc */
	push   D_045a
	call   L_221e
	pop    %cx
	/* TODO: data ref: movw   $0x0,0x48dc */
	movw   $0x0,D_045a
L_0583:
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	je   L_0599
	/* TODO: data ref: push   0x490a */
	push   D_0488
	call   L_0823
	pop    %cx
	or     %ax,%ax
	je   L_0599
	jmp   L_0215
L_0599:
	/* TODO: data ref: push   0x490a */
	push   D_0488
	call   L_205b
	pop    %cx
L_05a1:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_05a8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmp    $0x7f,%sp
	ja   L_05b5
	int    $0xf2
L_05b5:
	mov    0x4(%bp),%di
	xor    %si,%si
L_05bb:
	cmp    0x6(%bp),%si
	jl   L_05c3
	jmp   L_07b7
L_05c3:
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	add    %di,%ax
	mov    %ax,%bx
	mov    0x2(%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	testw  $0xe0,-0x6(%bp)
	jne   L_05df
	jmp   L_0682
L_05df:
	/* TODO: data ref: cmpw   $0x0,0x4894 */
	cmpw   $0x0,D_0412
	je   L_061a
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	je   L_05fd
	/* TODO: data ref: mov    0x4910,%bx */
	mov    D_048e,%bx
	push   (%bx)
	/* TODO: data ref: mov    $0x4653,%ax */
	mov    $D_01d1,%ax
	push   %ax
	call   L_0dce
	add    $0x4,%sp
L_05fd:
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	je   L_0609
	/* TODO: data ref: mov    $0x489e,%ax */
	mov    $D_041c,%ax
	jmp   L_060f
L_0609:
	/* TODO: data ref: mov    0x4910,%bx */
	mov    D_048e,%bx
	mov    (%bx),%ax
L_060f:
	push   %ax
	/* TODO: data ref: mov    $0x4657,%ax */
	mov    $D_01d5,%ax
	push   %ax
	call   L_0dce
	add    $0x4,%sp
L_061a:
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	add    %di,%ax
	mov    %ax,%bx
	push   (%bx)
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	add    %di,%ax
	mov    %ax,%bx
	mov    0x2(%bx),%al
	cbtw
	and    $0xff,%ax
	push   %ax
	call   L_0944
	pop    %cx
	push   %ax
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	add    %di,%ax
	mov    %ax,%bx
	mov    0x4(%bx),%ax
	and    $0xffff,%ax
	push   %ax
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	add    %di,%ax
	mov    %ax,%bx
	mov    0x3(%bx),%al
	cbtw
	and    $0xff,%ax
	push   %ax
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	add    %di,%ax
	mov    %ax,%bx
	push   0x8(%bx)
	push   0x6(%bx)
	/* TODO: data ref: mov    $0x465b,%ax */
	mov    $D_01d9,%ax
	push   %ax
	call   L_0dce
	add    $0xe,%sp
	jmp   L_07b3
L_0682:
	mov    -0x6(%bp),%ax
	and    $0x1e,%ax
	jmp   L_06d0
L_068a:
	movw   $0x75,-0x6(%bp)
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	add    %di,%ax
	mov    %ax,%bx
	cmpw   $0x0,0x8(%bx)
	jne   L_06a4
	cmpw   $0x0,0x6(%bx)
L_06a4:
	je   L_06ef
	movw   $0x63,-0x6(%bp)
	jmp   L_06ef
L_06ad:
	movw   $0x61,-0x6(%bp)
	jmp   L_06ef
L_06b4:
	movw   $0x74,-0x6(%bp)
	jmp   L_06ef
L_06bb:
	movw   $0x64,-0x6(%bp)
	jmp   L_06ef
L_06c2:
	movw   $0x62,-0x6(%bp)
	jmp   L_06ef
L_06c9:
	movw   $0x66,-0x6(%bp)
	jmp   L_06ef
L_06d0:
	cmp    $0x4,%ax
	je   L_06b4
	jg   L_06e0
	or     %ax,%ax
	je   L_068a
	cmp    $0x2,%ax
	je   L_06ad
L_06e0:
	cmp    $0x6,%ax
	je   L_06bb
	cmp    $0x8,%ax
	je   L_06c2
	cmp    $0x1f,%ax
	je   L_06c9
L_06ef:
	/* TODO: data ref: cmpw   $0x0,0x4906 */
	cmpw   $0x0,D_0484
	je   L_06ff
	cmpw   $0x75,-0x6(%bp)
	je   L_06ff
	jmp   L_07b3
L_06ff:
	/* TODO: data ref: cmpw   $0x0,0x4894 */
	cmpw   $0x0,D_0412
	je   L_073a
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	je   L_071d
	/* TODO: data ref: mov    0x4910,%bx */
	mov    D_048e,%bx
	push   (%bx)
	/* TODO: data ref: mov    $0x4677,%ax */
	mov    $D_01f5,%ax
	push   %ax
	call   L_0dce
	add    $0x4,%sp
L_071d:
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	je   L_0729
	/* TODO: data ref: mov    $0x489e,%ax */
	mov    $D_041c,%ax
	jmp   L_072f
L_0729:
	/* TODO: data ref: mov    0x4910,%bx */
	mov    D_048e,%bx
	mov    (%bx),%ax
L_072f:
	push   %ax
	/* TODO: data ref: mov    $0x467b,%ax */
	mov    $D_01f9,%ax
	push   %ax
	call   L_0dce
	add    $0x4,%sp
L_073a:
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	add    %di,%ax
	mov    %ax,%bx
	mov    0x2(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_0757
	mov    -0x6(%bp),%ax
	sub    $0x20,%ax
	mov    %ax,-0x6(%bp)
L_0757:
	/* TODO: data ref: cmpw   $0x0,0x4906 */
	cmpw   $0x0,D_0484
	jne   L_079c
	cmpw   $0x75,-0x6(%bp)
	je   L_076a
	cmpw   $0x55,-0x6(%bp)
	jne   L_0774
L_076a:
	/* TODO: data ref: mov    $0x467f,%ax */
	mov    $D_01fd,%ax
	push   %ax
	call   L_0dce
	pop    %cx
	jmp   L_078f
L_0774:
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	add    %di,%ax
	mov    %ax,%bx
	push   0x8(%bx)
	push   0x6(%bx)
	/* TODO: data ref: mov    $0x4688,%ax */
	mov    $D_0206,%ax
	push   %ax
	call   L_0dce
	add    $0x6,%sp
L_078f:
	push   -0x6(%bp)
	/* TODO: data ref: mov    $0x468e,%ax */
	mov    $D_020c,%ax
	push   %ax
	call   L_0dce
	add    $0x4,%sp
L_079c:
	mov    %si,%ax
	mov    $0xa,%dx
	imul   %dx
	add    %di,%ax
	mov    %ax,%bx
	push   (%bx)
	/* TODO: data ref: mov    $0x4693,%ax */
	mov    $D_0211,%ax
	push   %ax
	call   L_0dce
	add    $0x4,%sp
L_07b3:
	inc    %si
	jmp   L_05bb
L_07b7:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_07c8
	int    $0xf2
L_07c8:
	/* TODO: data ref: cmpw   $0x0,0x4896 */
	cmpw   $0x0,D_0414
	je   L_080b
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    0x6(%bp),%bx
	cmp    0x8(%bx),%dx
	jl   L_07ec
	jg   L_07e7
	cmp    0x6(%bx),%ax
	jbe   L_07ec
L_07e7:
	/* TODO: data ref: mov    0x449c,%ax */
	mov    D_001a,%ax
	jmp   L_081f
L_07ec:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    0x6(%bp),%bx
	cmp    0x8(%bx),%dx
	jg   L_080b
	jl   L_0804
	cmp    0x6(%bx),%ax
	jae   L_080b
L_0804:
	/* TODO: data ref: mov    0x449c,%ax */
	mov    D_001a,%ax
	neg    %ax
	jmp   L_081f
L_080b:
	mov    0x6(%bp),%bx
	push   (%bx)
	mov    0x4(%bp),%bx
	push   (%bx)
	call   L_2328
	add    $0x4,%sp
	/* TODO: data ref: imulw  0x449c */
	imulw  D_001a
L_081f:
	pop    %di
L_0820:
	pop    %si
	pop    %bp
	ret


L_0823:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	cmp    $0x7f,%sp
	ja   L_0832
	int    $0xf2
L_0832:
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: push   0x4914 */
	push   D_0492
	/* TODO: data ref: push   0x4912 */
	push   D_0490
	push   0x4(%bp)
L_0840:
	call   L_0c1a
	add    $0x8,%sp
	push   0x4(%bp)
	mov    $0x3c,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x489e,%ax */
	mov    $D_041c,%ax
	push   %ax
	call   L_0fee
	add    $0x8,%sp
	mov    %ax,%si
	cmp    $0x3c,%si
	je   L_0866
	xor    %ax,%ax
	jmp   L_08bd
L_0866:
	/* TODO: data ref: mov    $0x489e,%di */
	mov    $D_041c,%di
L_0869:
	/* TODO: data ref: cmp    $0x48ae,%di */
	cmp    $D_042c,%di
	jae   L_087a
	cmpb   $0x20,(%di)
	jne   L_0877
	movb   $0x0,(%di)
L_0877:
	inc    %di
	jmp   L_0869
L_087a:
	/* TODO: data ref: mov    $0x48ce,%ax */
	mov    $D_044c,%ax
	push   %ax
	call   L_0982
	pop    %cx
	cwtd
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	and    $0x1,%ax
	and    $0x0,%dx
	or     %dx,%dx
	jne   L_089c
	or     %ax,%ax
L_089c:
	je   L_08a6
	addw   $0x1,-0x8(%bp)
	adcw   $0x0,-0x6(%bp)
L_08a6:
	push   0x4(%bp)
	call   L_0a26
	pop    %cx
	add    -0x8(%bp),%ax
	adc    -0x6(%bp),%dx
	/* TODO: data ref: mov    %ax,0x4912 */
	mov    %ax,D_0490
	/* TODO: data ref: mov    %dx,0x4914 */
	mov    %dx,D_0492
	mov    $0x1,%ax
L_08bd:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_08c4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_08d0
	int    $0xf2
L_08d0:
	/* TODO: data ref: mov    0x4910,%bx */
	mov    D_048e,%bx
	push   (%bx)
	/* TODO: data ref: mov    $0x4697,%ax */
	mov    $D_0215,%ax
	push   %ax
	/* TODO: data ref: mov    $0x47f4,%ax */
	mov    $D_0372,%ax
	push   %ax
	call   L_0ada
	add    $0x6,%sp
	/* TODO: data ref: cmpw   $0x0,0x4908 */
	cmpw   $0x0,D_0486
	je   L_090d
	/* TODO: data ref: mov    $0x489e,%ax */
	mov    $D_041c,%ax
	push   %ax
	/* TODO: data ref: mov    $0x469f,%ax */
	mov    $D_021d,%ax
	push   %ax
	/* TODO: data ref: mov    $0x47f4,%ax */
	mov    $D_0372,%ax
	push   %ax
	call   L_0ada
	add    $0x6,%sp
	/* TODO: data ref: mov    $0x46a4,%ax */
	mov    $D_0222,%ax
	push   %ax
	/* TODO: data ref: mov    $0x47f4,%ax */
	mov    $D_0372,%ax
	push   %ax
	call   L_0ada
	add    $0x4,%sp
	jmp   L_091b
L_090d:
	/* TODO: data ref: mov    $0x46a7,%ax */
	mov    $D_0225,%ax
	push   %ax
	/* TODO: data ref: mov    $0x47f4,%ax */
	mov    $D_0372,%ax
	push   %ax
	call   L_0ada
	add    $0x4,%sp
L_091b:
	push   0x6(%bp)
	/* TODO: data ref: mov    $0x46a9,%ax */
	mov    $D_0227,%ax
	push   %ax
	/* TODO: data ref: mov    $0x47f4,%ax */
	mov    $D_0372,%ax
	push   %ax
	call   L_0ada
	add    $0x6,%sp
	cmpw   $0x0,0x4(%bp)
	je   L_093a
	mov    $0x2,%ax
	push   %ax
	call   _exit
	pop    %cx
L_093a:
	/* TODO: data ref: movw   $0x1,0x4890 */
	movw   $0x1,D_040e
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0944:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_0950
	int    $0xf2
L_0950:
	/* TODO: data ref: mov    $0x44c8,%di */
	mov    $D_0046,%di
L_0953:
	cmpw   $0x0,0x2(%di)
	je   L_096a
	mov    (%di),%ax
	cmp    0x4(%bp),%ax
	jne   L_0965
	mov    0x2(%di),%ax
	jmp   L_097e
L_0965:
	add    $0x4,%di
	jmp   L_0953
L_096a:
	push   0x4(%bp)
	/* TODO: data ref: mov    $0x4715,%ax */
	mov    $D_0293,%ax
	push   %ax
	/* TODO: data ref: mov    $0x451c,%ax */
	mov    $D_009a,%ax
	push   %ax
	call   L_11e2
	add    $0x6,%sp
	/* TODO: data ref: mov    $0x451c,%ax */
	mov    $D_009a,%ax
L_097e:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0982:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	mov    0x4(%bp),%di
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
	xor    %si,%si
L_099a:
	mov    (%di),%al
	cbtw
	jmp   L_09a3
L_099f:
	inc    %si
L_09a0:
	inc    %di
	jmp   L_09bc
L_09a3:
	cmp    $0x9,%ax
	je   L_09b9
	cmp    $0x20,%ax
	je   L_09b9
	cmp    $0x2b,%ax
	je   L_09a0
	cmp    $0x2d,%ax
	je   L_099f
	jmp   L_09bc
L_09b9:
	inc    %di
	jmp   L_099a
L_09bc:
	cmpb   $0x30,(%di)
	jl   L_0a06
	cmpb   $0x39,(%di)
	jg   L_0a06
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	push   -0x6(%bp)
	push   -0x8(%bp)
	call   L_1829
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
	jmp   L_09bc
L_0a06:
	or     %si,%si
	je   L_0a19
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	jmp   L_0a1f
L_0a19:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
L_0a1f:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0a26:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x2(%bx)
	jge   L_0a3f
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
L_0a3f:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_0a57
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%ax
	neg    %ax
	mov    %ax,%di
	jmp   L_0a90
L_0a57:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
	je   L_0a89
	xor    %di,%di
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	je   L_0a90
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	jne   L_0a90
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	mov    0x4(%bp),%bx
	sub    0x4(%bx),%ax
	mov    %ax,%di
	jmp   L_0a90
L_0a89:
	mov    $0xffff,%ax
	mov    %ax,%dx
	jmp   L_0ad2
L_0a90:
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
	call   L_11c4
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmp    $0x0,%dx
	jg   L_0ac3
	jl   L_0abb
	cmpw   $0x0,-0x8(%bp)
	jae   L_0ac3
L_0abb:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	jmp   L_0ad2
L_0ac3:
	mov    %di,%ax
	cwtd
	add    %ax,-0x8(%bp)
	adc    %dx,-0x6(%bp)
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
L_0ad2:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0ada:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	push   0x6(%bp)
	call   L_1234
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_0b00
	mov    $0xffff,%ax
	jmp   L_0b02
L_0b00:
	xor    %ax,%ax
L_0b02:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0b06:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	/* TODO: data ref: cmpb   $0x0,0x471a */
	cmpb   $0x0,D_0298
	jne   L_0b43
	/* TODO: data ref: movw   $0x47fc,-0x8(%bp) */
	movw   $D_037a,-0x8(%bp)
	/* TODO: data ref: movw   $0x4884,0x4884 */
	/* dual-reloc split: movw $D_0402,D_0402 */
	mov	$D_0402,%ax
	mov	%ax,D_0402
	movw   $0x88,-0xa(%bp)
L_0b29:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_0b3e
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_0b29
L_0b3e:
	/* TODO: data ref: movb   $0x1,0x471a */
	movb   $0x1,D_0298
L_0b43:
	/* TODO: data ref: movw   $0x47e4,-0x6(%bp) */
	movw   $D_0362,-0x6(%bp)
L_0b48:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_0b67
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x4884,%ax */
	cmp    D_0402,%ax
	jb   L_0b61
	xor    %ax,%ax
	jmp   L_0bd2
L_0b61:
	addw   $0x8,-0x6(%bp)
	jmp   L_0b48
L_0b67:
	cmpb   $0x77,(%di)
	jne   L_0b7d
	mov    $0x1b6,%ax
	push   %ax
	push   0x4(%bp)
	call   L_0c04
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_0bbc
L_0b7d:
	cmpb   $0x61,(%di)
	jne   L_0bbe
	mov    $0x1,%ax
	push   %ax
	push   0x4(%bp)
	call   L_23bc
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_0bab
	/* TODO: data ref: cmpw   $0x2,0x4916 */
	cmpw   $0x2,D_0494
	jne   L_0bab
	mov    $0x1b6,%ax
	push   %ax
	push   0x4(%bp)
	call   L_0c04
	add    $0x4,%sp
	mov    %ax,%si
L_0bab:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_11c4
	add    $0x8,%sp
L_0bbc:
	jmp   L_0bcc
L_0bbe:
	xor    %ax,%ax
	push   %ax
	push   0x4(%bp)
	call   L_23bc
	add    $0x4,%sp
	mov    %ax,%si
L_0bcc:
	or     %si,%si
	jge   L_0bd4
	xor    %ax,%ax
L_0bd2:
	jmp   L_0bfc
L_0bd4:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_0bf2
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_0bf9
L_0bf2:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_0bf9:
	mov    -0x6(%bp),%ax
L_0bfc:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0c04:
	push   %bp
	mov    %sp,%bp
	mov	$8,%bx	/* sc: creat */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 8:creat */
	int	$0xf1
	jcxz   L_0c18
	/* TODO: data ref: mov    %cx,0x4916 */
	mov    %cx,D_0494
L_0c18:
	pop    %bp
	ret


L_0c1a:
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
	jne   L_0c38
	jmp   L_0d81
L_0c38:
	cmpw   $0x2,0xa(%bp)
	jl   L_0c41
	jmp   L_0d22
L_0c41:
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_0c4d
	jmp   L_0d22
L_0c4d:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_0c5c
	jmp   L_0d22
L_0c5c:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%si
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,0xa(%bp)
	jne   L_0cb5
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
	call   L_11c4
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
	jmp   L_0cd0
L_0cb5:
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
L_0cd0:
	or     %si,%si
	jle   L_0d11
	mov    %si,%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jg   L_0d11
	jl   L_0ce3
	cmp    %ax,-0x8(%bp)
	ja   L_0d11
L_0ce3:
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x4(%bp),%bx
	sub    (%bx),%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jl   L_0d11
	jg   L_0cfb
	cmp    %ax,-0x8(%bp)
	jb   L_0d11
L_0cfb:
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	add    %ax,(%bx)
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	sub    %ax,0x2(%bx)
	xor    %ax,%ax
	jmp   L_0dc2
L_0d11:
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x1,%ax
	and    $0x0,%dx
	mov    %ax,%di
L_0d20:
	jmp   L_0d24
L_0d22:
	xor    %di,%di
L_0d24:
	mov    %di,%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	push   0xa(%bp)
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
	call   L_11c4
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
	or     %di,%di
	je   L_0d8b
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_0d78
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0db1
L_0d78:
	push   0x4(%bp)
	call   L_10c6
	pop    %cx
	jmp   L_0db1
L_0d81:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
L_0d8b:
	je   L_0db1
	push   0x4(%bp)
	call   L_1fec
	pop    %cx
	push   0xa(%bp)
	push   0x8(%bp)
	push   0x6(%bp)
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_11c4
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_0db1:
	cmpw   $0xffff,-0x6(%bp)
	jne   L_0dc4
	cmpw   $0xffff,-0x8(%bp)
	jne   L_0dc4
	mov    $0xffff,%ax
L_0dc2:
	jmp   L_0dc6
L_0dc4:
	xor    %ax,%ax
L_0dc6:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0dce:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x47ec,%ax */
	mov    $D_036a,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	push   0x4(%bp)
	call   L_1234
	add    $0x6,%sp
	/* TODO: data ref: mov    0x47f2,%al */
	mov    D_0370,%al
	cbtw
	test   $0x20,%ax
	je   L_0df2
	mov    $0xffff,%ax
	jmp   L_0df4
L_0df2:
	xor    %ax,%ax
L_0df4:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0df8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0xa(%bp),%ax
	/* TODO: data ref: mov    %ax,0x471c */
	mov    %ax,D_029a
	mov    0x8(%bp),%ax
	/* TODO: data ref: mov    %ax,0x471e */
	mov    %ax,D_029c
	mov    0x6(%bp),%ax
	mulw   0x8(%bp)
	add    0x4(%bp),%ax
	push   %ax
	push   0x4(%bp)
	call   L_0e20
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e20:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x10,%sp
	/* TODO: data ref: mov    0x471e,%ax */
	mov    D_029c,%ax
	mov    %ax,-0x6(%bp)
L_0e2e:
	mov    0x6(%bp),%ax
	sub    0x4(%bp),%ax
	mov    %ax,-0x10(%bp)
	cmp    -0x6(%bp),%ax
	ja   L_0e3f
	jmp   L_0f66
L_0e3f:
	mov    -0x6(%bp),%ax
	shl    $1,%ax
	mov    %ax,-0x14(%bp)
	mov    -0x10(%bp),%ax
	xor    %dx,%dx
	divw   -0x14(%bp)
	mulw   -0x6(%bp)
	mov    %ax,-0x10(%bp)
	mov    0x4(%bp),%ax
	add    -0x10(%bp),%ax
	mov    %ax,-0xa(%bp)
	mov    %ax,-0xc(%bp)
L_0e61:
	mov    0x4(%bp),%di
	mov    0x6(%bp),%ax
	sub    -0x6(%bp),%ax
	mov    %ax,%si
L_0e6c:
	cmp    -0xa(%bp),%di
	jae   L_0ea0
	push   -0xa(%bp)
	push   %di
	/* TODO: indirect call - verify */
	call   *0x471c
	add    $0x4,%sp
	mov    %ax,-0xe(%bp)
	or     %ax,%ax
	jne   L_0e95
	mov    -0x6(%bp),%ax
	sub    %ax,-0xa(%bp)
	push   -0xa(%bp)
	push   %di
	call   L_0f6d
	add    $0x4,%sp
	jmp   L_0e6c
L_0e95:
	cmpw   $0x0,-0xe(%bp)
	jge   L_0ea0
	add    -0x6(%bp),%di
L_0e9e:
	jmp   L_0e6c
L_0ea0:
	cmp    -0xc(%bp),%si
	jbe   L_0f05
	push   %si
	push   -0xc(%bp)
	/* TODO: indirect call - verify */
	call   *0x471c
	add    $0x4,%sp
	mov    %ax,-0xe(%bp)
	or     %ax,%ax
	jne   L_0ec9
	push   %si
	mov    -0x6(%bp),%ax
	add    %ax,-0xc(%bp)
	push   -0xc(%bp)
	call   L_0f6d
	add    $0x4,%sp
	jmp   L_0ea0
L_0ec9:
	cmpw   $0x0,-0xe(%bp)
	jle   L_0f00
	cmp    -0xa(%bp),%di
	jne   L_0ef0
	push   %si
	mov    -0x6(%bp),%ax
	add    %ax,-0xc(%bp)
	push   -0xc(%bp)
	push   %di
	call   L_0fa1
	add    $0x6,%sp
	mov    -0x6(%bp),%ax
	add    %ax,-0xa(%bp)
	mov    -0xa(%bp),%di
	jmp   L_0ea0
L_0ef0:
	push   %si
	push   %di
	call   L_0f6d
	add    $0x4,%sp
	sub    -0x6(%bp),%si
	add    -0x6(%bp),%di
L_0efe:
	jmp   L_0e9e
L_0f00:
	sub    -0x6(%bp),%si
	jmp   L_0ea0
L_0f05:
	cmp    -0xa(%bp),%di
	jne   L_0f4a
	mov    -0xa(%bp),%ax
	sub    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	sub    -0xc(%bp),%dx
	cmp    %dx,%ax
	jl   L_0f33
	push   0x6(%bp)
	mov    -0xc(%bp),%ax
	add    -0x6(%bp),%ax
	push   %ax
	call   L_0e20
	add    $0x4,%sp
	mov    -0xa(%bp),%ax
	mov    %ax,0x6(%bp)
L_0f30:
	jmp   L_0e2e
L_0f33:
	push   -0xa(%bp)
	push   0x4(%bp)
	call   L_0e20
	add    $0x4,%sp
	mov    -0xc(%bp),%ax
	add    -0x6(%bp),%ax
	mov    %ax,0x4(%bp)
	jmp   L_0f30
L_0f4a:
	push   %di
	mov    -0x6(%bp),%ax
	sub    %ax,-0xa(%bp)
	push   -0xa(%bp)
	push   %si
	call   L_0fa1
	add    $0x6,%sp
	mov    -0x6(%bp),%ax
	sub    %ax,-0xc(%bp)
	mov    -0xc(%bp),%si
	jmp   L_0efe
L_0f66:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f6d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	/* TODO: data ref: mov    0x471e,%ax */
	mov    D_029c,%ax
	mov    %ax,-0x8(%bp)
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_0f81:
	mov    (%di),%al
	mov    %al,-0x5(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%si),%al
	mov    %al,(%bx)
	mov    %si,%bx
	inc    %si
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	decw   -0x8(%bp)
	jne   L_0f81
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0fa1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	/* TODO: data ref: mov    0x471e,%ax */
	mov    D_029c,%ax
	mov    %ax,-0xa(%bp)
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    0x8(%bp),%ax
	mov    %ax,-0x6(%bp)
L_0fbb:
	mov    (%di),%al
	cbtw
	mov    %ax,-0x8(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    -0x6(%bp),%bx
	mov    (%si),%al
	mov    %al,(%bx)
	incw   -0x6(%bp)
	mov    %si,%bx
	inc    %si
	mov    -0x8(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	decw   -0xa(%bp)
	jne   L_0fbb
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0fee:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_104f
L_1009:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_104f
	mov    0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
L_1017:
	decw   0x2(%si)
	jl   L_1028
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_102d
L_1028:
	push   %si
	call   L_10c6
	pop    %cx
L_102d:
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jl   L_1040
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_1045
L_1040:
	mov    -0x8(%bp),%ax
	jmp   L_1052
L_1045:
	decw   -0xa(%bp)
	jne   L_1017
	incw   -0x8(%bp)
	jmp   L_1009
L_104f:
	mov    -0x8(%bp),%ax
L_1052:
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
	je   L_10bb
L_1074:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_10bb
	mov    0x6(%bp),%ax
	mov    %ax,-0x6(%bp)
L_1082:
	decw   0x2(%si)
	jl   L_109a
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
	jmp   L_10a8
L_109a:
	push   %si
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1efe
	add    $0x4,%sp
L_10a8:
	decw   -0x6(%bp)
	jne   L_1082
	mov    0x6(%si),%al
	cbtw
	test   $0x20,%ax
	jne   L_10bb
	incw   -0x8(%bp)
	jmp   L_1074
L_10bb:
	mov    -0x8(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_10c6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_10dd
	mov    $0xffff,%ax
	jmp   L_10e9
L_10dd:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_10eb
	mov    $0xffff,%ax
L_10e9:
	jmp   L_1163
L_10eb:
	cmpw   $0x0,0x4(%di)
	jne   L_111f
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1106
	mov    0x7(%di),%al
	cbtw
	/* TODO: data ref: add    $0x4720,%ax */
	add    $D_029e,%ax
	mov    %ax,0x4(%di)
	jmp   L_10eb
L_1106:
	mov    $0x200,%ax
	push   %ax
	call   L_20d2
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_111b
	orb    $0x4,0x6(%di)
	jmp   L_10eb
L_111b:
	orb    $0x8,0x6(%di)
L_111f:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1132
	mov    $0x1,%ax
	jmp   L_1135
L_1132:
	mov    $0x200,%ax
L_1135:
	push   %ax
	push   (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_23d2
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_1165
	cmpw   $0xffff,0x2(%di)
	jne   L_1157
	orb    $0x10,0x6(%di)
	jmp   L_115b
L_1157:
	orb    $0x20,0x6(%di)
L_115b:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_1163:
	jmp   L_116f
L_1165:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_116f:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1174:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1181:
	decw   0x8(%bp)
	jl   L_11a5
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_11a5
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_1181
	xor    %ax,%ax
	jmp   L_11c0
L_11a5:
	cmpw   $0x0,0x8(%bp)
	jge   L_11af
	xor    %ax,%ax
	jmp   L_11c0
L_11af:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_11c0:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_11c4:
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
	jcxz   L_11df
	/* TODO: data ref: mov    %cx,0x4916 */
	mov    %cx,D_0494
L_11df:
	pop    %si
	pop    %bp
	ret


L_11e2:
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
	push   0x6(%bp)
	call   L_1234
	add    $0x6,%sp
	decw   -0xa(%bp)
	jl   L_121c
	mov    -0xc(%bp),%bx
	incw   -0xc(%bp)
	movb   $0x0,(%bx)
	xor    %ax,%ax
	jmp   L_1229
L_121c:
	lea    -0xc(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_1efe
	add    $0x4,%sp
L_1229:
	mov    0x4(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
L_1230:
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1234:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_124c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_125e
	jmp   L_17fb
L_125e:
	cmpb   $0x25,-0x5(%bp)
	je   L_1271
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_126f:
	jmp   L_124c
L_1271:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_129c
	push   0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1e0c
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_129c:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_12b5
	incb   -0xa6(%bp)
	inc    %di
L_12b5:
	cmpb   $0x30,(%di)
	jne   L_12c0
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_12c0:
	cmpb   $0x2a,(%di)
	je   L_12f0
	movw   $0x0,-0xe(%bp)
L_12ca:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_1306
	cmpw   $0x9,-0xc(%bp)
	jg   L_1306
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_12ca
L_12f0:
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
L_1306:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_134d
	cmpb   $0x2a,(%di)
	je   L_134f
	movw   $0x0,-0x10(%bp)
L_131f:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_1349
	cmpw   $0x9,-0xc(%bp)
	jg   L_1349
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_131f
L_1349:
	addw   $0x30,-0xc(%bp)
L_134d:
	jmp   L_1366
L_134f:
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
L_1366:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_1770
L_1371:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_1382
L_1379:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_13b2
	jmp   L_13ab
L_1382:
	cmp    $0x64,%ax
	je   L_13b4
	cmp    $0x6f,%ax
	je   L_1398
	cmp    $0x75,%ax
	je   L_13ab
	cmp    $0x78,%ax
	je   L_13a6
	jmp   L_1379
L_1398:
	movw   $0x8,-0x12(%bp)
	jmp   L_13ab
L_139f:
	movw   $0x8,-0x12(%bp)
	jmp   L_13b2
L_13a6:
	movw   $0x10,-0x12(%bp)
L_13ab:
	jmp   L_13fd
L_13ad:
	movw   $0x10,-0x12(%bp)
L_13b2:
	jmp   L_140d
L_13b4:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_13d1
L_13c4:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_13d1:
	cmpw   $0x0,-0x14(%bp)
	jg   L_141b
	jl   L_13df
	cmpw   $0x0,-0x16(%bp)
	jae   L_141b
L_13df:
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
	jmp   L_141b
L_13fd:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_141b
L_140d:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_141b:
	cmpw   $0x0,-0x14(%bp)
	jne   L_1425
	cmpw   $0x0,-0x16(%bp)
L_1425:
	jne   L_1436
	cmpw   $0x0,-0x10(%bp)
	jne   L_1436
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_1436:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_143d:
	cmpw   $0x0,-0x14(%bp)
	jne   L_1447
	cmpw   $0x0,-0x16(%bp)
L_1447:
	je   L_1492
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	push   -0x14(%bp)
	push   -0x16(%bp)
	call   L_195a
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_1468
	mov    $0x30,%ax
	jmp   L_146b
L_1468:
	mov    $0x57,%ax
L_146b:
	add    -0xc(%bp),%ax
	mov    -0xa(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	push   -0x14(%bp)
	push   -0x16(%bp)
	call   L_1852
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_143d
L_1492:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_14e1
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_1492
L_14b0:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_14bc:
	cmpw   $0x2,-0xc(%bp)
	jge   L_14e1
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_14dc
	decw   -0x8(%bp)
L_14dc:
	incw   -0xc(%bp)
	jmp   L_14bc
L_14e1:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_14e8:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_14ff
	movw   $0x0,-0xc(%bp)
L_14ff:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_150e
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_150e:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	push   0x8(%bp)
	push   -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	push   -0xa(%bp)
	call   L_1e0c
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_1531:
	jmp   L_126f
L_1534:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_1546
	/* TODO: data ref: movw   $0x4734,-0xa(%bp) */
	movw   $D_02b2,-0xa(%bp)
L_1546:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_1555
	movw   $0x7fff,-0xc(%bp)
L_1555:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_155b:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_14e8
	decw   -0xc(%bp)
	jl   L_14e8
	incw   -0x8(%bp)
	jmp   L_155b
L_156d:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_1575:
	jmp   L_1531
L_1577:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1583
	movw   $0x6,-0x10(%bp)
L_1583:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	push   -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_1b58
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_15b8
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_15b8:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_15cd
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_15e5
L_15cd:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_15cd
L_15e5:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_15f8
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_15f8:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_1617
L_1604:
	decw   -0xc(%bp)
	jl   L_1617
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_1604
L_1617:
	cmpw   $0x0,-0xc(%bp)
	jg   L_1620
	jmp   L_14e1
L_1620:
	decw   -0xc(%bp)
	jge   L_1628
	jmp   L_14e1
L_1628:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_1620
L_163d:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_164b
	movw   $0x6,-0x10(%bp)
	jmp   L_164e
L_164b:
	incw   -0x10(%bp)
L_164e:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	push   -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_1b2c
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_1683
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_1683:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_168e
	incw   -0x1a(%bp)
L_168e:
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
L_16b0:
	decw   -0xc(%bp)
	jle   L_16ca
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_16b0
L_16ca:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_16e7
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_16f8
L_16e7:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_16f8:
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
	jmp   L_14e1
L_1729:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1735
	movw   $0x6,-0x10(%bp)
L_1735:
	push   -0x8(%bp)
	push   -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_19b0
	add    $0xc,%sp
	add    $0x8,%si
L_1753:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_1753
	decw   -0x8(%bp)
	jmp   L_14e1
L_1764:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_176d:
	jmp   L_1575
L_1770:
	cmp    $0x65,%ax
	jne   L_1778
	jmp   L_163d
L_1778:
	jg   L_17b9
	cmp    $0x4f,%ax
	jne   L_1782
	jmp   L_1398
L_1782:
	jg   L_1799
	cmp    $0x25,%ax
	je   L_1764
	cmp    $0x44,%ax
	jne   L_1791
	jmp   L_13b4
L_1791:
	cmp    $0x4c,%ax
L_1794:
	jne   L_1799
	jmp   L_1371
L_1799:
	cmp    $0x55,%ax
	jne   L_17a1
	jmp   L_13fd
L_17a1:
	cmp    $0x58,%ax
	jne   L_17a9
	jmp   L_13a6
L_17a9:
	cmp    $0x63,%ax
	jne   L_17b1
	jmp   L_14b0
L_17b1:
	cmp    $0x64,%ax
	jne   L_17b9
	jmp   L_13c4
L_17b9:
	cmp    $0x6f,%ax
	jne   L_17c1
	jmp   L_139f
L_17c1:
	jg   L_17d8
	cmp    $0x66,%ax
	jne   L_17cb
	jmp   L_1577
L_17cb:
	cmp    $0x67,%ax
	jne   L_17d3
	jmp   L_1729
L_17d3:
	cmp    $0x6c,%ax
	je   L_1794
L_17d8:
	cmp    $0x72,%ax
	jne   L_17e0
	jmp   L_156d
L_17e0:
	cmp    $0x73,%ax
	jne   L_17e8
	jmp   L_1534
L_17e8:
	cmp    $0x75,%ax
	jne   L_17f0
	jmp   L_140d
L_17f0:
	cmp    $0x78,%ax
	jne   L_17f8
	jmp   L_13ad
L_17f8:
	jmp   L_176d
L_17fb:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_181f
	push   0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1e0c
	add    $0x8,%sp
L_181f:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_198e


L_1829:
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
	call   L_198e


L_1852:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_1870
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_1870:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_1879:
	shr    $1,%dx
	rcr    $1,%ax
	inc    %cx
	or     %dx,%dx
	jne   L_1879
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_1889:
	shr    $1,%dx
	rcr    $1,%ax
	dec    %cx
	or     %cx,%cx
	jne   L_1889
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	push   0x8(%bx)
	push   0x6(%bx)
	call   L_1829
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_18ca
	jne   L_18bf
	or     $0x0,%ax
	je   L_18ca
L_18bf:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_18ca:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_198e


L_18d2:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_18ee
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_18ee:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_1905
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_1905:
	push   %dx
	push   %ax
	call   L_1852
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_191f
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_191f:
	ret
	call   L_198e
	mov    %sp,%bx
	push   %bx
	push   0x8(%bx)
	push   0x6(%bx)
	push   0x4(%bx)
	push   0x2(%bx)
	call   L_18d2
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	push   0x8(%bx)
	push   0x6(%bx)
	call   L_1829
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_198e


L_195a:
	mov    %sp,%bx
	push   %bx
	push   0x8(%bx)
	push   0x6(%bx)
	push   0x4(%bx)
	push   0x2(%bx)
	call   L_1852
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	push   0x8(%bx)
	push   0x6(%bx)
	call   L_1829
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_198e:
	pop    %ax
	mov    %sp,%bx
	push   0x6(%bx)
	push   0x4(%bx)
	mov    0x2(%bx),%bx
	push   0x2(%bx)
	push   (%bx)
	/* TODO: indirect call - verify */
	call   *%ax
	add    $0x8,%sp
	mov    %sp,%bx
	mov    0x2(%bx),%bx
	mov    %dx,0x2(%bx)
	mov    %ax,(%bx)
	ret
	/* .byte 0 (alignment) */


L_19b0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0x6(%bp),%ax
	push   %ax
	lea    -0x8(%bp),%ax
	push   %ax
	push   0xc(%bp)
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_1b2c
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_19ea
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_19ea:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_19f1:
	cmpw   $0x0,-0xa(%bp)
	jle   L_1a0b
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_1a0b
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_19f1
L_1a0b:
	cmpw   $0x0,-0x8(%bp)
	jl   L_1a1c
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_1a28
L_1a1c:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1a26
	cmpw   $0xfffd,-0x8(%bp)
L_1a26:
	jge   L_1aa7
L_1a28:
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
L_1a43:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_1a5d
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_1a43
L_1a5d:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_1a79
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
L_1a73:
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_1a7f
L_1a79:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_1a7f:
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
	jmp   L_1b17
L_1aa7:
	cmpw   $0x0,-0x8(%bp)
	jg   L_1ac9
	cmpb   $0x30,(%di)
	je   L_1ab8
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1ab8:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1ac9
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_1ab8
L_1ac9:
	movw   $0x1,-0xa(%bp)
L_1ace:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_1af6
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
	jne   L_1af1
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1af1:
	incw   -0xa(%bp)
	jmp   L_1ace
L_1af6:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_1b17
L_1afe:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_1b11
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_1afe
L_1b11:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1b17:
	cmpb   $0x2e,-0x1(%si)
	jne   L_1b1e
	dec    %si
L_1b1e:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1b2c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x1,%ax
	push   %ax
	push   0x10(%bp)
	push   0xe(%bp)
	push   0xc(%bp)
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_1b83
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b58:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %ax,%ax
	push   %ax
	push   0x10(%bp)
	push   0xe(%bp)
	push   0xc(%bp)
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_1b83
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b83:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_1b96
	movw   $0x0,0xc(%bp)
L_1b96:
	cmpw   $0x4f,0xc(%bp)
	jl   L_1ba1
	movw   $0x4e,0xc(%bp)
L_1ba1:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	/* TODO: data ref: mov    $0x473c,%si */
	mov    $D_02ba,%si
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x478c */
	fcompl D_030a
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_1bde
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	int    $0xf4
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_1bde:
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_2452
	int    $0xf4
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	/* TODO: data ref: movw   $0x478c,-0x16(%bp) */
	movw   $D_030a,-0x16(%bp)
	int    $0xf4
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x4794 */
	fcompl D_0312
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
L_1c21:
	xchg   %ah,-0x17(%bp)
	jne   L_1c29
	jmp   L_1cd6
L_1c29:
	/* TODO: data ref: movw   $0x478c,-0x16(%bp) */
	movw   $D_030a,-0x16(%bp)
L_1c2e:
	int    $0xf4
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x479c */
	fcompl D_031a
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_1c4a
	jmp   L_1cd4
L_1c4a:
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   -0xc(%bp)
	/* TODO: data ref: fdivl  0x47a4 */
	fdivl  D_0322
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_2452
	int    $0xf4
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	/* TODO: data ref: fldl   0x47ac */
	fldl   D_032a
	fstl   -0x1e(%bp)
	/* TODO: data ref: fdivl  0x47b4 */
	fdivl  D_0332
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	/* TODO: data ref: fmull  0x47bc */
	fmull  D_033a
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
	jmp   L_1c2e
L_1cbd:
	/* TODO: data ref: cmpw   $0x478c,-0x16(%bp) */
	cmpw   $D_030a,-0x16(%bp)
	jae   L_1d17
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_1cd4:
	jmp   L_1cbd
L_1cd6:
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x47c4 */
	fcompl D_0342
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_1d26
L_1cef:
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x47cc */
	fmull  D_034a
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	/* TODO: data ref: fcompl 0x47d4 */
	fcompl D_0352
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_1d17:
	jae   L_1d26
	int    $0xf4
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_1cef
L_1d26:
	mov    0xc(%bp),%ax
	/* TODO: data ref: add    $0x473c,%ax */
	add    $D_02ba,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_1d38
	add    %di,-0x16(%bp)
L_1d38:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	/* TODO: data ref: cmpw   $0x473c,-0x16(%bp) */
	cmpw   $D_02ba,-0x16(%bp)
	jae   L_1d4e
	/* TODO: data ref: movb   $0x0,0x473c */
	movb   $0x0,D_02ba
	/* TODO: data ref: mov    $0x473c,%ax */
	mov    $D_02ba,%ax
	jmp   L_1db9
L_1d4e:
	cmp    -0x16(%bp),%si
	ja   L_1daa
	/* TODO: data ref: cmp    $0x478c,%si */
	cmp    $D_030a,%si
	jae   L_1daa
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x47dc */
	fmull  D_035a
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_2452
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
	jmp   L_1d4e
L_1daa:
	/* TODO: data ref: cmpw   $0x478c,-0x16(%bp) */
	cmpw   $D_030a,-0x16(%bp)
	jb   L_1dbb
	/* TODO: data ref: movb   $0x0,0x478b */
	movb   $0x0,D_0309
	/* TODO: data ref: mov    $0x473c,%ax */
	mov    $D_02ba,%ax
L_1db9:
	jmp   L_1e05
L_1dbb:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_1dc3:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_1dff
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	/* TODO: data ref: cmpw   $0x473c,-0x16(%bp) */
	cmpw   $D_02ba,-0x16(%bp)
	jbe   L_1de2
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_1dc3
L_1de2:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_1dc3
	/* TODO: data ref: cmp    $0x473c,%si */
	cmp    $D_02ba,%si
	jbe   L_1dfc
	movb   $0x30,(%si)
L_1dfc:
	inc    %si
	jmp   L_1dc3
L_1dff:
	movb   $0x0,(%si)
	/* TODO: data ref: mov    $0x473c,%ax */
	mov    $D_02ba,%ax
L_1e05:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e0c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1e19:
	cmpw   $0x0,0x8(%bp)
	jl   L_1e22
	jmp   L_1eb0
L_1e22:
	cmpb   $0x2d,(%di)
	jne   L_1e5f
	cmpw   $0x30,0xc(%bp)
	jne   L_1e5f
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1e4e
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
	jmp   L_1e5e
L_1e4e:
	push   0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1efe
	add    $0x4,%sp
L_1e5e:
	dec    %si
L_1e5f:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1e7d
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
	jmp   L_1e89
L_1e7d:
	push   0xa(%bp)
	push   0xc(%bp)
	call   L_1efe
	add    $0x4,%sp
L_1e89:
	incw   0x8(%bp)
	jmp   L_1e19
L_1e8e:
	dec    %si
	jl   L_1ec4
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1eb2
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
L_1eb0:
	jmp   L_1e8e
L_1eb2:
	push   0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_1efe
	add    $0x4,%sp
	jmp   L_1e8e
L_1ec4:
	cmpw   $0x0,0x8(%bp)
	je   L_1ef9
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1ee8
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
	jmp   L_1ef4
L_1ee8:
	push   0xa(%bp)
	push   0xc(%bp)
	call   L_1efe
	add    $0x4,%sp
L_1ef4:
	decw   0x8(%bp)
	jmp   L_1ec4
L_1ef9:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1efe:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_1f19
	mov    $0xffff,%ax
	jmp   L_1fe0
L_1f19:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1f4c
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,-0x9(%bp)
	movw   $0x1,-0x8(%bp)
	push   -0x8(%bp)
	lea    -0x9(%bp),%ax
	push   %ax
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_2438
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_1fd1
L_1f4c:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1f9e
	/* TODO: data ref: cmp    $0x47ec,%di */
	cmp    $D_036a,%di
	jne   L_1f77
	/* TODO: data ref: mov    0x47f3,%al */
	mov    D_0371,%al
	cbtw
	push   %ax
	call   L_2394
	pop    %cx
	or     %ax,%ax
	je   L_1f6c
	orb    $0x4,0x6(%di)
	jmp   L_1f19
L_1f6c:
	/* TODO: data ref: movw   $0x491e,0x4(%di) */
	movw   $D_049c,0x4(%di)
	/* TODO: data ref: movw   $0x491e,(%di) */
	movw   $D_049c,(%di)
	jmp   L_1f19
L_1f77:
	mov    $0x200,%ax
	push   %ax
	call   L_20d2
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1f8e
	orb    $0x4,0x6(%di)
	jmp   L_1f19
L_1f8e:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_1fc0
L_1f9e:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_1fc0
	mov    %si,(%di)
	push   -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_2438
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_1fc0:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_1fd1:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_1fe2
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_1fe0:
	jmp   L_1fe5
L_1fe2:
	mov    0x4(%bp),%ax
L_1fe5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1fec:
	push   %bp
	mov    %sp,%bp
	push   %si
L_1ff0:
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	and    $0x6,%ax
	cmp    $0x2,%ax
	jne   L_2038
	mov    0x4(%di),%si
	or     %si,%si
	je   L_2038
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_2038
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	push   -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
L_2024:
	call   L_2438
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_2038
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_203a
L_2038:
	xor    %ax,%ax
L_203a:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_203f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x47e4,%di */
	mov    $D_0362,%di
L_2047:
	/* TODO: data ref: cmp    0x4884,%di */
	cmp    D_0402,%di
	jae   L_2057
	push   %di
	call   L_205b
	pop    %cx
	add    $0x8,%di
	jmp   L_2047
L_2057:
	pop    %di
	pop    %si
L_2059:
	pop    %bp
	ret


L_205b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_20ae
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_20ae
	push   %di
	call   L_1fec
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_236a
	pop    %cx
	or     %ax,%ax
	jge   L_2090
	mov    $0xffff,%si
L_2090:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_20a0
	push   0x4(%di)
	call   L_221e
	pop    %cx
L_20a0:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_20ae
	movw   $0x0,0x4(%di)
L_20ae:
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
	call   L_203f
	push   0x4(%bp)
	call   L_246e
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_20d2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: cmpw   $0x0,0x4886 */
	cmpw   $0x0,D_0404
	jne   L_20fd
	/* TODO: data ref: mov    $0x4888,%ax */
	mov    $D_0406,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x4886 */
	mov    %ax,D_0404
	/* TODO: data ref: mov    $0x4886,%ax */
	mov    $D_0404,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x4888 */
	mov    %ax,D_0406
	/* TODO: data ref: movw   $0x4888,0x4d20 */
	/* dual-reloc split: movw $D_0406,D_089e */
	mov	$D_0406,%ax
	mov	%ax,D_089e
	/* TODO: data ref: movw   $0x4886,0x4d1e */
	/* dual-reloc split: movw $D_0404,D_089c */
	mov	$D_0404,%ax
	mov	%ax,D_089c
L_20fd:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    0x4d1e,%di */
	mov    D_089c,%di
L_2111:
	/* TODO: data ref: movw   $0x0,0x488a */
	movw   $0x0,D_0408
L_2117:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_2146
L_211e:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_212d
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_211e
L_212d:
	mov    -0x6(%bp),%ax
	shl    $1,%ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_2146
	mov    -0x6(%bp),%ax
	shl    $1,%ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_2146
	jmp   L_21ef
L_2146:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	jbe   L_2155
L_2153:
	jmp   L_2117
L_2155:
	/* TODO: data ref: cmp    0x4d20,%si */
	cmp    D_089e,%si
	jne   L_2161
	/* TODO: data ref: cmp    $0x4886,%di */
	cmp    $D_0404,%di
	je   L_2165
L_2161:
	xor    %ax,%ax
	jmp   L_219d
L_2165:
	/* TODO: data ref: incw   0x488a */
	incw   D_0408
	/* TODO: data ref: cmpw   $0x1,0x488a */
	cmpw   $0x1,D_0408
	jle   L_2153
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	/* TODO: data ref: mov    %ax,0x488a */
	mov    %ax,D_0408
	xor    %ax,%ax
	push   %ax
	call   L_2302
	pop    %cx
	mov    %ax,%si
	/* TODO: data ref: mov    0x488a,%ax */
	mov    D_0408,%ax
	shl    $1,%ax
	add    %si,%ax
	add    $0x0,%ax
	cmp    %si,%ax
	jae   L_219f
	xor    %ax,%ax
L_219d:
	jmp   L_21b2
L_219f:
	/* TODO: data ref: mov    0x488a,%ax */
	mov    D_0408,%ax
	shl    $1,%ax
	push   %ax
	call   L_2302
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_21b4
	xor    %ax,%ax
L_21b2:
	jmp   L_2219
L_21b4:
	/* TODO: data ref: mov    0x4d20,%bx */
	mov    D_089e,%bx
	mov    %si,(%bx)
	/* TODO: data ref: mov    0x4d20,%ax */
	mov    D_089e,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_21d2
	/* TODO: data ref: mov    0x4d20,%bx */
	mov    D_089e,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x4d20,%bx */
	mov    D_089e,%bx
	mov    %ax,(%bx)
L_21d2:
	/* TODO: data ref: mov    0x488a,%ax */
	mov    D_0408,%ax
	shl    $1,%ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	/* TODO: data ref: mov    %ax,0x4d20 */
	mov    %ax,D_089e
	/* TODO: data ref: mov    $0x4886,%ax */
	mov    $D_0404,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x4d20,%bx */
	mov    D_089e,%bx
	mov    %ax,(%bx)
	jmp   L_2111
L_21ef:
	mov    -0x6(%bp),%ax
	shl    $1,%ax
	add    %di,%ax
	/* TODO: data ref: mov    %ax,0x4d1e */
	mov    %ax,D_089c
	cmp    %ax,%si
	jbe   L_220e
	/* TODO: data ref: mov    0x4d1e,%bx */
	mov    D_089c,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x4d22 */
	mov    %ax,D_08a0
	/* TODO: data ref: mov    0x4d1e,%bx */
	mov    D_089c,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_220e:
	/* TODO: data ref: mov    0x4d1e,%ax */
	mov    D_089c,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_2219:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_221e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	/* TODO: data ref: mov    %si,0x4d1e */
	mov    %si,D_089c
	mov    (%si),%ax
	and    $0xfffe,%ax
	mov    %ax,(%si)
	pop    %di
	pop    %si
	pop    %bp
	ret


L_223a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%di
	mov    -0x2(%di),%ax
	test   $0x1,%ax
	je   L_2253
	push   %di
	call   L_221e
	pop    %cx
L_2253:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	push   0x6(%bp)
	call   L_20d2
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_2272
	cmp    %di,%si
	jne   L_2276
L_2272:
	mov    %si,%ax
	jmp   L_22de
L_2276:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_2295
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_2295:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_22b3
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_2295
L_22b3:
	cmp    %di,%si
	jae   L_22dc
	mov    -0xa(%bp),%ax
	shl    $1,%ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_22dc
	mov    -0xa(%bp),%ax
	shl    $1,%ax
	add    %si,%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	shl    $1,%ax
	add    %si,%ax
	mov    %ax,%bx
	/* TODO: data ref: mov    0x4d22,%ax */
	mov    D_08a0,%ax
	mov    %ax,(%bx)
L_22dc:
	mov    %si,%ax
L_22de:
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
	jcxz   L_22f9
	/* TODO: data ref: mov    %cx,0x4916 */
	mov    %cx,D_0494
	jmp   L_2300
L_22f9:
	mov    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x488c */
	mov    %bx,D_040a
L_2300:
	pop    %bp
	ret


L_2302:
	push   %bp
	mov    %sp,%bp
	mov	$17,%bx	/* sc: brk */
	mov    0x4(%bp),%ax
	/* TODO: data ref: add    0x488c,%ax */
	add    D_040a,%ax
	/* v7: 17:brk */
	int	$0xf1
	jcxz   L_2319
	/* TODO: data ref: mov    %cx,0x4916 */
	mov    %cx,D_0494
	jmp   L_2326
L_2319:
	/* TODO: data ref: mov    0x488c,%bx */
	mov    D_040a,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x488c */
	mov    %bx,D_040a
L_2326:
	pop    %bp
	ret


L_2328:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_2335:
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_2354
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_2335
L_2350:
	xor    %ax,%ax
	jmp   L_2365
L_2354:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_2365:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_236a:
	push   %bp
	mov    %sp,%bp
	mov	$6,%bx	/* sc: close */
	mov    0x4(%bp),%ax
	/* v7: 6:close */
	int	$0xf1
	jcxz   L_237b
	/* TODO: data ref: mov    %cx,0x4916 */
	mov    %cx,D_0494
L_237b:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_237e:
	push   %bp
	mov    %sp,%bp
	mov	$28,%bx	/* sc: fstat */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 28:fstat */
	int	$0xf1
	jcxz   L_2392
	/* TODO: data ref: mov    %cx,0x4916 */
	mov    %cx,D_0494
L_2392:
	pop    %bp
	ret


L_2394:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	push   0x4(%bp)
	call   L_2405
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_23b1
	xor    %ax,%ax
	jmp   L_23b4
L_23b1:
	mov    $0x1,%ax
L_23b4:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_23bc:
	push   %bp
	mov    %sp,%bp
	mov	$5,%bx	/* sc: open */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 5:open */
	int	$0xf1
	jcxz   L_23d0
	/* TODO: data ref: mov    %cx,0x4916 */
	mov    %cx,D_0494
L_23d0:
	pop    %bp
	ret


L_23d2:
	push   %bp
	mov    %sp,%bp
	mov	$3,%bx	/* sc: read */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 3:read */
	int	$0xf1
	jcxz   L_23e9
	/* TODO: data ref: mov    %cx,0x4916 */
	mov    %cx,D_0494
L_23e9:
	pop    %bp
	ret
	/* .byte 0 (alignment) */
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   0x6(%bp)
	mov    $0x7409,%ax
	push   %ax
	push   0x4(%bp)
	call   L_241e
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2405:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	push   0x4(%bp)
	call   L_241e
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_241e:
	push   %bp
	mov    %sp,%bp
	mov	$54,%bx	/* sc: ioctl */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 54:ioctl */
	int	$0xf1
	jcxz   L_2435
	/* TODO: data ref: mov    %cx,0x4916 */
	mov    %cx,D_0494
L_2435:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2438:
	push   %bp
	mov    %sp,%bp
	mov	$4,%bx	/* sc: write */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 4:write */
	int	$0xf1
	jcxz   L_244f
	/* TODO: data ref: mov    %cx,0x4916 */
	mov    %cx,D_0494
L_244f:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2452:
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


L_246e:
	push   %bp
	mov    %sp,%bp
	mov	$1,%bx	/* sc: exit */
	mov    0x4(%bp),%ax
	/* v7: 1:exit */
	int	$0xf1
	jcxz   L_247f
	/* TODO: data ref: mov    %cx,0x4916 */
	mov    %cx,D_0494
L_247f:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


	.data

	.byte	0x00, 0x00	/* |..| */
D_0002:
	.byte	0xbf, 0x0f	/* |..| */
	.ascii	"@(#)nm.c 4.1 10/1/80\0"
	.byte	0x00	/* |.| */
D_001a:
	.byte	0x01, 0x00	/* |..| */
D_001c:
	.byte	0xa4, 0x20, 0xaa, 0x20, 0xaa, 0x20, 0xaa, 0x20	/* |. . . . | */
	.byte	0xaa, 0x20, 0xaa, 0x20, 0x84, 0x20, 0xaa, 0x20	/* |. . . . | */
	.byte	0xaa, 0x20, 0xaa, 0x20, 0xaa, 0x20, 0xaa, 0x20	/* |. . . . | */
	.byte	0xaa, 0x20, 0x7e, 0x20, 0x9e, 0x20, 0x98, 0x20	/* |. ~ . . | */
	.byte	0xaa, 0x20, 0x90, 0x20, 0xaa, 0x20, 0xaa, 0x20	/* |. . . . | */
	.byte	0x8a, 0x20	/* |. | */
D_0046:
	.byte	0x20, 0x00	/* | .| */
	.word	D_022b	/* ptr: 0x46ad */
	.byte	0x22, 0x00	/* |".| */
	.word	D_0230	/* ptr: 0x46b2 */
	.byte	0x24, 0x00	/* |$.| */
	.word	D_0236	/* ptr: 0x46b8 */
	.byte	0x26, 0x00	/* |&.| */
	.word	D_023a	/* ptr: 0x46bc */
	.byte	0x28, 0x00	/* |(.| */
	.word	D_0240	/* ptr: 0x46c2 */
	.byte	0x40, 0x00	/* |@.| */
	.word	D_0246	/* ptr: 0x46c8 */
	.byte	0x44, 0x00	/* |D.| */
	.word	D_024b	/* ptr: 0x46cd */
	.byte	0x60, 0x00	/* |`.| */
	.word	D_0251	/* ptr: 0x46d3 */
	.byte	0x64, 0x00	/* |d.| */
	.word	D_0256	/* ptr: 0x46d8 */
	.byte	0x80, 0x00	/* |..| */
	.word	D_0259	/* ptr: 0x46db */
	.byte	0x84, 0x00	/* |..| */
	.word	D_025e	/* ptr: 0x46e0 */
	.byte	0xa0, 0x00	/* |..| */
	.word	D_0262	/* ptr: 0x46e4 */
	.byte	0xa4, 0x00	/* |..| */
	.word	D_0267	/* ptr: 0x46e9 */
	.byte	0xc0, 0x00	/* |..| */
	.word	D_026d	/* ptr: 0x46ef */
	.byte	0xe0, 0x00	/* |..| */
	.word	D_0273	/* ptr: 0x46f5 */
	.byte	0xe2, 0x00	/* |..| */
	.word	D_0279	/* ptr: 0x46fb */
	.byte	0xe4, 0x00	/* |..| */
	.word	D_027f	/* ptr: 0x4701 */
	.byte	0xe8, 0x00	/* |..| */
	.word	D_0285	/* ptr: 0x4707 */
	.byte	0xfe, 0x00	/* |..| */
	.word	D_028b	/* ptr: 0x470d */
	.byte	0x30, 0x00	/* |0.| */
	.word	D_0290	/* ptr: 0x4712 */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_009a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_00ba:
	.byte	0x6e, 0x6d, 0x3a, 0x20, 0x69, 0x6e, 0x76, 0x61	/* |nm: inva| */
	.byte	0x6c, 0x69, 0x64, 0x20, 0x61, 0x72, 0x67, 0x75	/* |lid argu| */
	.byte	0x6d, 0x65, 0x6e, 0x74, 0x20, 0x2d, 0x25, 0x63	/* |ment -%c| */
	.byte	0x0a, 0x00	/* |..| */
D_00d4:
	.ascii	"a.out\0"
D_00da:
	.byte	0x72, 0x00	/* |r.| */
D_00dc:
	.ascii	"cannot open\0"
D_00e8:
	.ascii	"old archive\0"
D_00f4:
	.byte	0x21, 0x3c, 0x61, 0x72, 0x63, 0x68, 0x3e, 0x0a	/* |!<arch>.| */
	.byte	0x00	/* |.| */
D_00fd:
	.ascii	"bad format\0"
D_0108:
	.byte	0x0a, 0x25, 0x73, 0x3a, 0x0a, 0x00	/* |.%s:..| */
D_010e:
	.ascii	"no name list\0"
D_011b:
	.ascii	"old format .o (no string table) or truncated file\0"
D_014d:
	.ascii	"out of memory\0"
D_015b:
	.ascii	"no string table (old format .o?)\0"
D_017c:
	.ascii	"no string table (old format .o?)\0"
D_019d:
	.ascii	"ran out of memory\0"
D_01af:
	.ascii	"error reading string table\0"
D_01ca:
	.byte	0x00	/* |.| */
D_01cb:
	.byte	0x0a, 0x25, 0x73, 0x3a, 0x0a, 0x00	/* |.%s:..| */
D_01d1:
	.byte	0x25, 0x73, 0x3a, 0x00	/* |%s:.| */
D_01d5:
	.byte	0x25, 0x73, 0x3a, 0x00	/* |%s:.| */
D_01d9:
	.byte	0x25, 0x30, 0x38, 0x6c, 0x78, 0x20, 0x2d, 0x20	/* |%08lx - | */
	.byte	0x25, 0x30, 0x32, 0x78, 0x20, 0x25, 0x30, 0x34	/* |%02x %04| */
	.byte	0x78, 0x20, 0x25, 0x35, 0x2e, 0x35, 0x73, 0x20	/* |x %5.5s | */
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_01f5:
	.byte	0x25, 0x73, 0x3a, 0x00	/* |%s:.| */
D_01f9:
	.byte	0x25, 0x73, 0x3a, 0x00	/* |%s:.| */
D_01fd:
	.ascii	"        \0"
D_0206:
	.ascii	"%08lx\0"
D_020c:
	.ascii	" %c \0"
D_0211:
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_0215:
	.ascii	"nm: %s:\0"
D_021d:
	.ascii	"(%s)\0"
D_0222:
	.byte	0x3a, 0x20, 0x00	/* |: .| */
D_0225:
	.byte	0x20, 0x00	/* | .| */
D_0227:
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_022b:
	.ascii	"GSYM\0"
D_0230:
	.ascii	"FNAME\0"
D_0236:
	.byte	0x46, 0x55, 0x4e, 0x00	/* |FUN.| */
D_023a:
	.ascii	"STSYM\0"
D_0240:
	.ascii	"LCSYM\0"
D_0246:
	.ascii	"RSYM\0"
D_024b:
	.ascii	"SLINE\0"
D_0251:
	.ascii	"SSYM\0"
D_0256:
	.byte	0x53, 0x4f, 0x00	/* |SO.| */
D_0259:
	.ascii	"LSYM\0"
D_025e:
	.byte	0x53, 0x4f, 0x4c, 0x00	/* |SOL.| */
D_0262:
	.ascii	"PSYM\0"
D_0267:
	.ascii	"ENTRY\0"
D_026d:
	.ascii	"LBRAC\0"
D_0273:
	.ascii	"RBRAC\0"
D_0279:
	.ascii	"BCOMM\0"
D_027f:
	.ascii	"ECOMM\0"
D_0285:
	.ascii	"ECOML\0"
D_028b:
	.ascii	"LENG\0"
D_0290:
	.byte	0x50, 0x43, 0x00	/* |PC.| */
D_0293:
	.ascii	"%02x\0"
D_0298:
	.byte	0x00, 0x00	/* |..| */
D_029a:
	.byte	0x00, 0x00	/* |..| */
D_029c:
	.byte	0x00, 0x00	/* |..| */
D_029e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_02b2:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_02ba:
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
D_0309:
	.byte	0x00	/* |.| */
D_030a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0312:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_031a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0322:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_032a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_0332:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_033a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0342:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_034a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0352:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_035a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0362:
	.word	D_069c	/* ptr: 0x4b1e */
	.byte	0x00, 0x00	/* |..| */
	.word	D_069c	/* ptr: 0x4b1e */
	.byte	0x01, 0x00	/* |..| */
D_036a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_0370:
	.byte	0x02	/* |.| */
D_0371:
	.byte	0x01	/* |.| */
D_0372:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
D_037a:
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
D_0402:
	.word	D_0402	/* ptr: 0x4884 */
D_0404:
	.byte	0x00, 0x00	/* |..| */
D_0406:
	.byte	0x00, 0x00	/* |..| */
D_0408:
	.byte	0x00, 0x00	/* |..| */
D_040a:
	.word	0x08b0	/* _curbrk: dseg=0x0410+bseg=0x04a0=0x08b0 (paragraph-rounded) */

	.bss

D_040c:	.space	2	/* bss word @ 0x040c */
D_040e:	.space	2	/* bss word @ 0x040e */
D_0410:	.space	2	/* bss word @ 0x0410 */
D_0412:	.space	2	/* bss word @ 0x0412 */
D_0414:	.space	2	/* bss word @ 0x0414 */
	.space	6
D_041c:	.space	2	/* bss word @ 0x041c */
	.space	14
D_042c:	.space	2	/* bss word @ 0x042c */
	.space	30
D_044c:	.space	2	/* bss word @ 0x044c */
	.space	10
D_0458:	.space	2	/* bss word @ 0x0458 */
D_045a:	.space	2	/* bss word @ 0x045a */
D_045c:	.space	2	/* bss word @ 0x045c */
	.space	6
D_0464:	.space	2	/* bss word @ 0x0464 */
	.space	2
D_0468:	.space	2	/* bss word @ 0x0468 */
D_046a:	.space	2	/* bss word @ 0x046a */
D_046c:	.space	2	/* bss word @ 0x046c */
D_046e:	.space	2	/* bss word @ 0x046e */
	.space	4
D_0474:	.space	2	/* bss word @ 0x0474 */
D_0476:	.space	2	/* bss word @ 0x0476 */
	.space	4
D_047c:	.space	2	/* bss word @ 0x047c */
D_047e:	.space	2	/* bss word @ 0x047e */
D_0480:	.space	2	/* bss word @ 0x0480 */
D_0482:	.space	2	/* bss word @ 0x0482 */
D_0484:	.space	2	/* bss word @ 0x0484 */
D_0486:	.space	2	/* bss word @ 0x0486 */
D_0488:	.space	2	/* bss word @ 0x0488 */
D_048a:	.space	2	/* bss word @ 0x048a */
D_048c:	.space	2	/* bss word @ 0x048c */
D_048e:	.space	2	/* bss word @ 0x048e */
D_0490:	.space	2	/* bss word @ 0x0490 */
D_0492:	.space	2	/* bss word @ 0x0492 */
D_0494:	.space	2	/* bss word @ 0x0494 */
	.space	6
D_049c:	.space	2	/* bss word @ 0x049c */
	.space	510
D_069c:	.space	2	/* bss word @ 0x069c */
	.space	510
D_089c:	.space	2	/* bss word @ 0x089c */
D_089e:	.space	2	/* bss word @ 0x089e */
D_08a0:	.space	2	/* bss word @ 0x08a0 */

/* end of file */
