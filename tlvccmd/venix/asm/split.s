/* split.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 split.s -o split.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: split  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x000016de  (5854 bytes)
//   a_data:   0x000001a6  (422 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
L_0045:
	movw   $0x0,-0x8(%bp)
	mov    $0x1,%di
L_004d:
	cmp    0x4(%bp),%di
	jl   L_0055
	jmp   L_00fa
L_0055:
	mov    %di,%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%bx
	cmpb   $0x2d,(%bx)
	jne   L_00cd
	mov    %di,%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%bx
	mov    0x1(%bx),%al
	cbtw
	jmp   L_0093
L_0076:
	movw   $0x1,-0x8(%bp)
	jmp   L_0091
L_007d:
	mov    %di,%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	inc    %ax
	push   %ax
	call   L_029c
	pop    %cx
	mov    %ax,D_0004
L_0091:
	jmp   L_00cb
L_0093:
	cmp    $0x34,%ax
L_0096:
	je   L_007d
	jg   L_00b2
	or     %ax,%ax
	je   L_0076
	cmp    $0x30,%ax
	je   L_007d
	cmp    $0x31,%ax
	je   L_007d
	cmp    $0x32,%ax
	je   L_007d
	cmp    $0x33,%ax
	je   L_007d
L_00b2:
	cmp    $0x35,%ax
	je   L_007d
	cmp    $0x36,%ax
	je   L_007d
	cmp    $0x37,%ax
	je   L_007d
	cmp    $0x38,%ax
	je   L_007d
	cmp    $0x39,%ax
	je   L_0096
L_00cb:
	jmp   L_00f6
L_00cd:
	cmpw   $0x0,-0x8(%bp)
	je   L_00e3
	mov    %di,%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,D_01aa
	jmp   L_00f6
L_00e3:
	mov    %di,%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,D_0212
	movw   $0x2,-0x8(%bp)
L_00f6:
	inc    %di
	jmp   L_004d
L_00fa:
	cmpw   $0x2,-0x8(%bp)
	je   L_0108
	/* dual-reloc split: movw $D_00fc,D_0216 */
	mov	$D_00fc,%ax
	mov	%ax,D_0216
	jmp   L_0133
L_0108:
	mov    $D_0006,%ax
	push   %ax
	pushw  D_0212
	call   L_0308
	add    $0x4,%sp
	mov    %ax,D_0216
	or     %ax,%ax
	jne   L_0133
	mov    $D_0008,%ax
	push   %ax
	mov    $D_010c,%ax
	push   %ax
	call   L_0270
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0133:
	cmpw   $0x0,D_01aa
	jne   L_0140
	/* dual-reloc split: movw $D_001b,D_01aa */
	mov	$D_001b,%ax
	mov	%ax,D_01aa
L_0140:
	movw   $0x1,-0x6(%bp)
	xor    %di,%di
L_0147:
	cmp    D_0004,%di
	jb   L_0150
	jmp   L_0264
L_0150:
	mov    D_0216,%bx
	decw   0x2(%bx)
	jl   L_016c
	mov    D_0216,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_0174
L_016c:
	pushw  D_0216
	call   L_041c
	pop    %cx
L_0174:
	mov    %ax,%si
	cmp    $0xffff,%si
	jne   L_0190
	cmpw   $0x0,-0x6(%bp)
	jne   L_0189
	pushw  D_01a8
	call   L_130f
	pop    %cx
L_0189:
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0190:
	cmpw   $0x0,-0x6(%bp)
	jne   L_0199
	jmp   L_0230
L_0199:
	movw   $0x0,-0x6(%bp)
L_019e:
	mov    D_01aa,%ax
	add    -0x6(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	je   L_01c4
	mov    D_01aa,%ax
	add    -0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,D_01ae(%bx)
	incw   -0x6(%bp)
	jmp   L_019e
L_01c4:
	mov    D_0214,%ax
	cwtd
	mov    $0x1a,%cx
	idiv   %cx
	add    $0x61,%ax
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,D_01ae(%bx)
	incw   -0x6(%bp)
	mov    D_0214,%ax
	cwtd
	mov    $0x1a,%cx
	idiv   %cx
	add    $0x61,%dx
	mov    -0x6(%bp),%bx
	mov    %dx,%cx
	mov    %cl,D_01ae(%bx)
	incw   -0x6(%bp)
	mov    -0x6(%bp),%bx
	movb   $0x0,D_01ae(%bx)
	incw   D_0214
	mov    $D_001d,%ax
	push   %ax
	mov    $D_01ae,%ax
	push   %ax
	call   L_0308
	add    $0x4,%sp
	mov    %ax,D_01a8
	or     %ax,%ax
	jne   L_022b
	mov    $D_001f,%ax
	push   %ax
	mov    $D_010c,%ax
	push   %ax
	call   L_0270
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_022b:
	movw   $0x0,-0x6(%bp)
L_0230:
	mov    D_01a8,%bx
	decw   0x2(%bx)
	jl   L_024d
	mov    D_01a8,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_0258
L_024d:
	pushw  D_01a8
	push   %si
	call   L_11b2
	add    $0x4,%sp
L_0258:
	cmp    $0xa,%si
	je   L_0260
	jmp   L_0150
L_0260:
	inc    %di
	jmp   L_0147
L_0264:
	pushw  D_01a8
	call   L_130f
	pop    %cx
	jmp   L_0140
	/* .byte 0 (alignment) */


L_0270:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_04e8
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_0296
	mov    $0xffff,%ax
	jmp   L_0298
L_0296:
	xor    %ax,%ax
L_0298:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_029c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	xor    %si,%si
	mov    %si,-0x6(%bp)
L_02ab:
	mov    (%di),%al
	cbtw
	jmp   L_02b6
L_02b0:
	incw   -0x6(%bp)
L_02b3:
	inc    %di
	jmp   L_02cf
L_02b6:
	cmp    $0x9,%ax
	je   L_02cc
	cmp    $0x20,%ax
	je   L_02cc
	cmp    $0x2b,%ax
	je   L_02b3
	cmp    $0x2d,%ax
	je   L_02b0
	jmp   L_02cf
L_02cc:
	inc    %di
	jmp   L_02ab
L_02cf:
	cmpb   $0x30,(%di)
	jl   L_02f5
	cmpb   $0x39,(%di)
	jg   L_02f5
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
	jmp   L_02cf
L_02f5:
	cmpw   $0x0,-0x6(%bp)
	je   L_0301
	mov    %si,%ax
	neg    %ax
	jmp   L_0303
L_0301:
	mov    %si,%ax
L_0303:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0308:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	cmpb   $0x0,D_0036
	jne   L_0345
	movw   $D_0114,-0x8(%bp)
	/* suppressed: movw $D_019c,D_019c (self-ref in .data) */
	movw   $0x88,-0xa(%bp)
L_032b:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_0340
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_032b
L_0340:
	movb   $0x1,D_0036
L_0345:
	movw   $D_00fc,-0x6(%bp)
L_034a:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_0369
	mov    -0x6(%bp),%ax
	cmp    D_019c,%ax
	jb   L_0363
	xor    %ax,%ax
	jmp   L_03d4
L_0363:
	addw   $0x8,-0x6(%bp)
	jmp   L_034a
L_0369:
	cmpb   $0x77,(%di)
	jne   L_037f
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0406
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_03be
L_037f:
	cmpb   $0x61,(%di)
	jne   L_03c0
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1618
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_03ad
	cmpw   $0x2,D_0218
	jne   L_03ad
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_0406
	add    $0x4,%sp
	mov    %ax,%si
L_03ad:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_04ca
	add    $0x8,%sp
L_03be:
	jmp   L_03ce
L_03c0:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1618
	add    $0x4,%sp
	mov    %ax,%si
L_03ce:
	or     %si,%si
	jge   L_03d6
	xor    %ax,%ax
L_03d4:
	jmp   L_03fe
L_03d6:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_03f4
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_03fb
L_03f4:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_03fb:
	mov    -0x6(%bp),%ax
L_03fe:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0406:
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
	jns    L_041a
	neg	%ax
	mov	%ax,D_0218	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_041a:
	pop    %bp
	ret


L_041c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_0433
	mov    $0xffff,%ax
	jmp   L_043f
L_0433:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_0441
	mov    $0xffff,%ax
L_043f:
	jmp   L_04b9
L_0441:
	cmpw   $0x0,0x4(%di)
	jne   L_0475
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_045c
	mov    0x7(%di),%al
	cbtw
	add    $D_0038,%ax
	mov    %ax,0x4(%di)
	jmp   L_0441
L_045c:
	mov    $0x200,%ax
	push   %ax
	call   L_1386
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_0471
	orb    $0x4,0x6(%di)
	jmp   L_0441
L_0471:
	orb    $0x8,0x6(%di)
L_0475:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0488
	mov    $0x1,%ax
	jmp   L_048b
L_0488:
	mov    $0x200,%ax
L_048b:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_162e
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_04bb
	cmpw   $0xffff,0x2(%di)
	jne   L_04ad
	orb    $0x10,0x6(%di)
	jmp   L_04b1
L_04ad:
	orb    $0x20,0x6(%di)
L_04b1:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_04b9:
	jmp   L_04c5
L_04bb:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_04c5:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_04ca:
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
L_04e5:
	pop    %si
	pop    %bp
	ret


L_04e8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0500:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_0512
	jmp   L_0aaf
L_0512:
	cmpb   $0x25,-0x5(%bp)
	je   L_0525
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_0523:
	jmp   L_0500
L_0525:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0550
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_10c0
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0550:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_0569
	incb   -0xa6(%bp)
	inc    %di
L_0569:
	cmpb   $0x30,(%di)
	jne   L_0574
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_0574:
	cmpb   $0x2a,(%di)
	je   L_05a4
	movw   $0x0,-0xe(%bp)
L_057e:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_05ba
	cmpw   $0x9,-0xc(%bp)
	jg   L_05ba
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_057e
L_05a4:
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
L_05ba:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_0601
	cmpb   $0x2a,(%di)
	je   L_0603
	movw   $0x0,-0x10(%bp)
L_05d3:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_05fd
	cmpw   $0x9,-0xc(%bp)
	jg   L_05fd
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_05d3
L_05fd:
	addw   $0x30,-0xc(%bp)
L_0601:
	jmp   L_061a
L_0603:
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
L_061a:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_0a24
L_0625:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_0636
L_062d:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_0666
	jmp   L_065f
L_0636:
	cmp    $0x64,%ax
	je   L_0668
	cmp    $0x6f,%ax
	je   L_064c
	cmp    $0x75,%ax
	je   L_065f
	cmp    $0x78,%ax
	je   L_065a
	jmp   L_062d
L_064c:
	movw   $0x8,-0x12(%bp)
	jmp   L_065f
L_0653:
	movw   $0x8,-0x12(%bp)
	jmp   L_0666
L_065a:
	movw   $0x10,-0x12(%bp)
L_065f:
	jmp   L_06b1
L_0661:
	movw   $0x10,-0x12(%bp)
L_0666:
	jmp   L_06c1
L_0668:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0685
L_0678:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0685:
	cmpw   $0x0,-0x14(%bp)
	jg   L_06cf
	jl   L_0693
	cmpw   $0x0,-0x16(%bp)
	jae   L_06cf
L_0693:
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
	jmp   L_06cf
L_06b1:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_06cf
L_06c1:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_06cf:
	cmpw   $0x0,-0x14(%bp)
	jne   L_06d9
	cmpw   $0x0,-0x16(%bp)
L_06d9:
	jne   L_06ea
	cmpw   $0x0,-0x10(%bp)
	jne   L_06ea
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_06ea:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_06f1:
	cmpw   $0x0,-0x14(%bp)
	jne   L_06fb
	cmpw   $0x0,-0x16(%bp)
L_06fb:
	je   L_0746
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_0c0e
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_071c
	mov    $0x30,%ax
	jmp   L_071f
L_071c:
	mov    $0x57,%ax
L_071f:
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
	call   L_0b06
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_06f1
L_0746:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_0795
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_0746
L_0764:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_0770:
	cmpw   $0x2,-0xc(%bp)
	jge   L_0795
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0790
	decw   -0x8(%bp)
L_0790:
	incw   -0xc(%bp)
	jmp   L_0770
L_0795:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_079c:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_07b3
	movw   $0x0,-0xc(%bp)
L_07b3:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_07c2
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_07c2:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_10c0
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_07e5:
	jmp   L_0523
L_07e8:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_07fa
	movw   $D_004c,-0xa(%bp)
L_07fa:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_0809
	movw   $0x7fff,-0xc(%bp)
L_0809:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_080f:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_079c
	decw   -0xc(%bp)
	jl   L_079c
	incw   -0x8(%bp)
	jmp   L_080f
L_0821:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_0829:
	jmp   L_07e5
L_082b:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0837
	movw   $0x6,-0x10(%bp)
L_0837:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0e0c
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_086c
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_086c:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_0881
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_0899
L_0881:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_0881
L_0899:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_08ac
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_08ac:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_08cb
L_08b8:
	decw   -0xc(%bp)
	jl   L_08cb
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_08b8
L_08cb:
	cmpw   $0x0,-0xc(%bp)
	jg   L_08d4
	jmp   L_0795
L_08d4:
	decw   -0xc(%bp)
	jge   L_08dc
	jmp   L_0795
L_08dc:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_08d4
L_08f1:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_08ff
	movw   $0x6,-0x10(%bp)
	jmp   L_0902
L_08ff:
	incw   -0x10(%bp)
L_0902:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0de0
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0937
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0937:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_0942
	incw   -0x1a(%bp)
L_0942:
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
L_0964:
	decw   -0xc(%bp)
	jle   L_097e
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_0964
L_097e:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_099b
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_09ac
L_099b:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_09ac:
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
	jmp   L_0795
L_09dd:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_09e9
	movw   $0x6,-0x10(%bp)
L_09e9:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0c64
	add    $0xc,%sp
	add    $0x8,%si
L_0a07:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_0a07
	decw   -0x8(%bp)
	jmp   L_0795
L_0a18:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_0a21:
	jmp   L_0829
L_0a24:
	cmp    $0x65,%ax
	jne   L_0a2c
	jmp   L_08f1
L_0a2c:
	jg   L_0a6d
	cmp    $0x4f,%ax
	jne   L_0a36
	jmp   L_064c
L_0a36:
	jg   L_0a4d
	cmp    $0x25,%ax
	je   L_0a18
	cmp    $0x44,%ax
	jne   L_0a45
	jmp   L_0668
L_0a45:
	cmp    $0x4c,%ax
L_0a48:
	jne   L_0a4d
	jmp   L_0625
L_0a4d:
	cmp    $0x55,%ax
	jne   L_0a55
	jmp   L_06b1
L_0a55:
	cmp    $0x58,%ax
	jne   L_0a5d
	jmp   L_065a
L_0a5d:
	cmp    $0x63,%ax
	jne   L_0a65
	jmp   L_0764
L_0a65:
	cmp    $0x64,%ax
	jne   L_0a6d
	jmp   L_0678
L_0a6d:
	cmp    $0x6f,%ax
	jne   L_0a75
	jmp   L_0653
L_0a75:
	jg   L_0a8c
	cmp    $0x66,%ax
	jne   L_0a7f
	jmp   L_082b
L_0a7f:
	cmp    $0x67,%ax
	jne   L_0a87
	jmp   L_09dd
L_0a87:
	cmp    $0x6c,%ax
	je   L_0a48
L_0a8c:
	cmp    $0x72,%ax
	jne   L_0a94
	jmp   L_0821
L_0a94:
	cmp    $0x73,%ax
	jne   L_0a9c
	jmp   L_07e8
L_0a9c:
	cmp    $0x75,%ax
	jne   L_0aa4
	jmp   L_06c1
L_0aa4:
	cmp    $0x78,%ax
	jne   L_0aac
	jmp   L_0661
L_0aac:
	jmp   L_0a21
L_0aaf:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0ad3
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_10c0
	add    $0x8,%sp
L_0ad3:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_0c42


L_0add:
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
	call   L_0c42


L_0b06:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_0b24
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_0b24:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_0b2d:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_0b2d
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_0b3d:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_0b3d
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0add
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_0b7e
	jne   L_0b73
	or     $0x0,%ax
	je   L_0b7e
L_0b73:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_0b7e:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_0c42


L_0b86:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0ba2
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0ba2:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0bb9
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0bb9:
	push   %dx
	push   %ax
	call   L_0b06
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_0bd3
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_0bd3:
	ret
	call   L_0c42
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0b86
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0add
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_0c42


L_0c0e:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0b06
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0add
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_0c42:
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


L_0c64:
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
	call   L_0de0
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_0c9e
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_0c9e:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_0ca5:
	cmpw   $0x0,-0xa(%bp)
	jle   L_0cbf
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_0cbf
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_0ca5
L_0cbf:
	cmpw   $0x0,-0x8(%bp)
	jl   L_0cd0
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_0cdc
L_0cd0:
	cmpw   $0x0,-0x8(%bp)
	jge   L_0cda
	cmpw   $0xfffd,-0x8(%bp)
L_0cda:
	jge   L_0d5b
L_0cdc:
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
L_0cf7:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_0d11
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_0cf7
L_0d11:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_0d2d
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_0d33
L_0d2d:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_0d33:
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
	jmp   L_0dcb
L_0d5b:
	cmpw   $0x0,-0x8(%bp)
	jg   L_0d7d
	cmpb   $0x30,(%di)
	je   L_0d6c
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0d6c:
	cmpw   $0x0,-0x8(%bp)
	jge   L_0d7d
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_0d6c
L_0d7d:
	movw   $0x1,-0xa(%bp)
L_0d82:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_0daa
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
	jne   L_0da5
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0da5:
	incw   -0xa(%bp)
	jmp   L_0d82
L_0daa:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_0dcb
L_0db2:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_0dc5
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_0db2
L_0dc5:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0dcb:
	cmpb   $0x2e,-0x1(%si)
	jne   L_0dd2
	dec    %si
L_0dd2:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0de0:
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
	call   L_0e37
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e0c:
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
	call   L_0e37
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e37:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_0e4a
	movw   $0x0,0xc(%bp)
L_0e4a:
	cmpw   $0x4f,0xc(%bp)
	jl   L_0e55
	movw   $0x4e,0xc(%bp)
L_0e55:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_0054,%si
	fldl   0x4(%bp)
	fcompl D_00a4
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_0e92
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_0e92:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_16ae
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $D_00a4,-0x16(%bp)
	fldl   -0xc(%bp)
	fcompl D_00ac
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_0edd
	jmp   L_0f8a
L_0edd:
	movw   $D_00a4,-0x16(%bp)
L_0ee2:
	fldl   -0xc(%bp)
	fcompl D_00b4
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_0efe
	jmp   L_0f88
L_0efe:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	fdivl  D_00bc
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_16ae
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   D_00c4
	fstl   -0x1e(%bp)
	fdivl  D_00cc
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  D_00d4
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
	jmp   L_0ee2
L_0f71:
	cmpw   $D_00a4,-0x16(%bp)
	jae   L_0fcb
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_0f88:
	jmp   L_0f71
L_0f8a:
	fldl   0x4(%bp)
	fcompl D_00dc
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_0fda
L_0fa3:
	fldl   0x4(%bp)
	fmull  D_00e4
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl D_00ec
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_0fcb:
	jae   L_0fda
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_0fa3
L_0fda:
	mov    0xc(%bp),%ax
	add    $D_0054,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_0fec
	add    %di,-0x16(%bp)
L_0fec:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_0054,-0x16(%bp)
	jae   L_1002
	movb   $0x0,D_0054
	mov    $D_0054,%ax
	jmp   L_106d
L_1002:
	cmp    -0x16(%bp),%si
	ja   L_105e
	cmp    $D_00a4,%si
	jae   L_105e
	fldl   0x4(%bp)
	fmull  D_00f4
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_16ae
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
	jmp   L_1002
L_105e:
	cmpw   $D_00a4,-0x16(%bp)
	jb   L_106f
	movb   $0x0,D_00a3
	mov    $D_0054,%ax
L_106d:
	jmp   L_10b9
L_106f:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_1077:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_10b3
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_0054,-0x16(%bp)
	jbe   L_1096
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_1077
L_1096:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_1077
	cmp    $D_0054,%si
	jbe   L_10b0
	movb   $0x30,(%si)
L_10b0:
	inc    %si
	jmp   L_1077
L_10b3:
	movb   $0x0,(%si)
	mov    $D_0054,%ax
L_10b9:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_10c0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_10cd:
	cmpw   $0x0,0x8(%bp)
	jl   L_10d6
	jmp   L_1164
L_10d6:
	cmpb   $0x2d,(%di)
	jne   L_1113
	cmpw   $0x30,0xc(%bp)
	jne   L_1113
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1102
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
	jmp   L_1112
L_1102:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_11b2
	add    $0x4,%sp
L_1112:
	dec    %si
L_1113:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1131
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
	jmp   L_113d
L_1131:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_11b2
	add    $0x4,%sp
L_113d:
	incw   0x8(%bp)
	jmp   L_10cd
L_1142:
	dec    %si
	jl   L_1178
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1166
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
L_1164:
	jmp   L_1142
L_1166:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_11b2
	add    $0x4,%sp
	jmp   L_1142
L_1178:
	cmpw   $0x0,0x8(%bp)
	je   L_11ad
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_119c
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
	jmp   L_11a8
L_119c:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_11b2
	add    $0x4,%sp
L_11a8:
	decw   0x8(%bp)
	jmp   L_1178
L_11ad:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_11b2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_11cd
	mov    $0xffff,%ax
	jmp   L_1294
L_11cd:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1200
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
	call   L_1694
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_1285
L_1200:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1252
	cmp    $D_0104,%di
	jne   L_122b
	mov    D_010b,%al
	cbtw
	push   %ax
	call   L_15f0
	pop    %cx
	or     %ax,%ax
	je   L_1220
	orb    $0x4,0x6(%di)
	jmp   L_11cd
L_1220:
	movw   $D_021e,0x4(%di)
	movw   $D_021e,(%di)
	jmp   L_11cd
L_122b:
	mov    $0x200,%ax
	push   %ax
	call   L_1386
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1242
	orb    $0x4,0x6(%di)
	jmp   L_11cd
L_1242:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_1274
L_1252:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_1274
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1694
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_1274:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_1285:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_1296
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_1294:
	jmp   L_1299
L_1296:
	mov    0x4(%bp),%ax
L_1299:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_12a0:
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
	jne   L_12ec
	mov    0x4(%di),%si
	or     %si,%si
	je   L_12ec
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_12ec
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1694
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_12ec
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_12ee
L_12ec:
	xor    %ax,%ax
L_12ee:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_12f3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_00fc,%di
L_12fb:
	cmp    D_019c,%di
	jae   L_130b
	push   %di
	call   L_130f
	pop    %cx
	add    $0x8,%di
	jmp   L_12fb
L_130b:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_130f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_1362
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_1362
	push   %di
	call   L_12a0
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_15dc
	pop    %cx
	or     %ax,%ax
	jge   L_1344
	mov    $0xffff,%si
L_1344:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_1354
	pushw  0x4(%di)
	call   L_14d2
	pop    %cx
L_1354:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_1362
	movw   $0x0,0x4(%di)
L_1362:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_1386:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_019e
	jne   L_13b1
	mov    $D_01a0,%ax
	or     $0x1,%ax
	mov    %ax,D_019e
	mov    $D_019e,%ax
	or     $0x1,%ax
	mov    %ax,D_01a0
	/* dual-reloc split: movw $D_01a0,D_0620 */
	mov	$D_01a0,%ax
	mov	%ax,D_0620
	/* dual-reloc split: movw $D_019e,D_061e */
	mov	$D_019e,%ax
	mov	%ax,D_061e
L_13b1:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_061e,%di
L_13c5:
	movw   $0x0,D_01a2
L_13cb:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_13fa
L_13d2:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_13e1
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_13d2
L_13e1:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_13fa
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_13fa
	jmp   L_14a3
L_13fa:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	jbe   L_1409
L_1407:
	jmp   L_13cb
L_1409:
	cmp    D_0620,%si
	jne   L_1415
	cmp    $D_019e,%di
	je   L_1419
L_1415:
	xor    %ax,%ax
	jmp   L_1451
L_1419:
	incw   D_01a2
	cmpw   $0x1,D_01a2
	jle   L_1407
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_01a2
	xor    %ax,%ax
	push   %ax
	call   L_15b6
	pop    %cx
	mov    %ax,%si
	mov    D_01a2,%ax
	shl    %ax
	add    %si,%ax
	add    $0x0,%ax
	cmp    %si,%ax
	jae   L_1453
	xor    %ax,%ax
L_1451:
	jmp   L_1466
L_1453:
	mov    D_01a2,%ax
	shl    %ax
	push   %ax
	call   L_15b6
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_1468
	xor    %ax,%ax
L_1466:
	jmp   L_14cd
L_1468:
	mov    D_0620,%bx
	mov    %si,(%bx)
	mov    D_0620,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_1486
	mov    D_0620,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_0620,%bx
	mov    %ax,(%bx)
L_1486:
	mov    D_01a2,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_0620
	mov    $D_019e,%ax
	or     $0x1,%ax
	mov    D_0620,%bx
	mov    %ax,(%bx)
	jmp   L_13c5
L_14a3:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_061e
	cmp    %ax,%si
	jbe   L_14c2
	mov    D_061e,%bx
	mov    (%bx),%ax
	mov    %ax,D_0622
	mov    D_061e,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_14c2:
	mov    D_061e,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_14cd:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_14d2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_061e
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
	je   L_1507
	push   %di
	call   L_14d2
	pop    %cx
L_1507:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_1386
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_1526
	cmp    %di,%si
	jne   L_152a
L_1526:
	mov    %si,%ax
	jmp   L_1592
L_152a:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_1549
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_1549:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_1567
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_1549
L_1567:
	cmp    %di,%si
	jae   L_1590
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_1590
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
	mov    D_0622,%ax
	mov    %ax,(%bx)
L_1590:
	mov    %si,%ax
L_1592:
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
	jns    L_15ad
	neg	%ax
	mov	%ax,D_0218	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_15b4
L_15ad:
	mov    0x4(%bp),%bx
	mov    %bx,D_01a4
L_15b4:
	pop    %bp
	ret


L_15b6:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	add    D_01a4,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_15cd
	neg	%ax
	mov	%ax,D_0218	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_15da
L_15cd:
	mov    D_01a4,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_01a4
L_15da:
	pop    %bp
	ret


L_15dc:
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
	jns    L_15ed
	neg	%ax
	mov	%ax,D_0218	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_15ed:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_15f0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1661
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_160d
	xor    %ax,%ax
	jmp   L_1610
L_160d:
	mov    $0x1,%ax
L_1610:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1618:
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
	jns    L_162c
	neg	%ax
	mov	%ax,D_0218	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_162c:
	pop    %bp
	ret


L_162e:
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
	jns    L_1645
	neg	%ax
	mov	%ax,D_0218	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1645:
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
	call   L_167a
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1661:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_167a
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_167a:
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
	jns    L_1691
	neg	%ax
	mov	%ax,D_0218	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1691:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1694:
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
	jns    L_16ab
	neg	%ax
	mov	%ax,D_0218	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_16ab:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_16ae:
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


L_16ca:
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
	jns    L_16db
	neg	%ax
	mov	%ax,D_0218	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_16db:
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
	mov	%ax,D_0218	/* errno */
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
	.word	D_03e8	/* ptr: 0x03e8 */
D_0006:
	.byte	0x72, 0x00	/* |r.| */
D_0008:
	.byte	0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6f	/* |cannot o| */
	.byte	0x70, 0x65, 0x6e, 0x20, 0x69, 0x6e, 0x70, 0x75	/* |pen inpu| */
	.byte	0x74, 0x0a, 0x00	/* |t..| */
D_001b:
	.byte	0x78, 0x00	/* |x.| */
D_001d:
	.byte	0x77, 0x00	/* |w.| */
D_001f:
	.byte	0x43, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x63	/* |Cannot c| */
	.byte	0x72, 0x65, 0x61, 0x74, 0x65, 0x20, 0x6f, 0x75	/* |reate ou| */
	.byte	0x74, 0x70, 0x75, 0x74, 0x0a, 0x00, 0x00	/* |tput...| */
D_0036:
	.byte	0x00, 0x00	/* |..| */
D_0038:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_004c:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_0054:
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
D_00a3:
	.byte	0x00	/* |.| */
D_00a4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_00ac:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_00b4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_00bc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_00c4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_00cc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_00d4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_00dc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_00e4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_00ec:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_00f4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_00fc:
	.word	D_041e	/* ptr: 0x241e */
	.byte	0x00, 0x00	/* |..| */
	.word	D_041e	/* ptr: 0x241e */
	.byte	0x01, 0x00	/* |..| */
D_0104:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02	/* |.......| */
D_010b:
	.byte	0x01	/* |.| */
D_010c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
	.word	D_0206	/* ptr: 0x0206 */
D_0114:
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
D_019c:
	.byte	0x9c, 0x21	/* |.!| */
D_019e:
	.byte	0x00, 0x00	/* |..| */
D_01a0:
	.byte	0x00, 0x00	/* |..| */
D_01a2:
	.byte	0x00, 0x00	/* |..| */
D_01a4:
	.word	D_0624	/* ptr: 0x2624 */

	.bss

	.space	2
D_01a8:	.space	2	/* bss word @ 0x01a8 */
D_01aa:	.space	2	/* bss word @ 0x01aa */
	.space	2
D_01ae:	.space	2	/* bss word @ 0x01ae */
	.space	86
D_0206:	.space	2	/* bss word @ 0x0206 */
	.space	10
D_0212:	.space	2	/* bss word @ 0x0212 */
D_0214:	.space	2	/* bss word @ 0x0214 */
D_0216:	.space	2	/* bss word @ 0x0216 */
D_0218:	.space	2	/* bss word @ 0x0218 */
	.space	4
D_021e:	.space	2	/* bss word @ 0x021e */
	.space	456
D_03e8:	.space	2	/* bss word @ 0x03e8 */
	.space	52
D_041e:	.space	2	/* bss word @ 0x041e */
	.space	510
D_061e:	.space	2	/* bss word @ 0x061e */
D_0620:	.space	2	/* bss word @ 0x0620 */
D_0622:	.space	2	/* bss word @ 0x0622 */
D_0624:	/* endcore: initial break */

/* end of file */
