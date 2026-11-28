/* expand.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 expand.s -o expand.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: expand  [linked executable]
//   Magic:    0x0107  OMAGIC (object/impure)
//   a_stack:  0x0000  (0 bytes)
//   a_text:   0x00001826  (6182 bytes)
//   a_data:   0x0000046a  (1130 bytes)
//   Load:  cs:0x0000 (text)   ds:0x1826 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: mov    $0x1d60,%ax */
	mov    $D_1d60,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1bee,%ax */
	mov    $D_1bee,%ax
	push   %ax
	call   L_04f6
	add    $0x4,%sp
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
L_0051:
	cmpw   $0x0,0x4(%bp)
	jle   L_0073
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	cmpb   $0x2d,(%bx)
	jne   L_0073
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_0268
	pop    %cx
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
L_0071:
	jmp   L_0051
L_0073:
	cmpw   $0x0,0x4(%bp)
	jle   L_00a8
	/* TODO: data ref: mov    $0x1be6,%ax */
	mov    $D_1be6,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1853,%ax */
	mov    $D_1853,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_02f6
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_00a1
	mov    0x6(%bp),%bx
	pushw  (%bx)
	call   L_03d8
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_00a1:
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
L_00a8:
	xor    %si,%si
L_00aa:
	/* TODO: data ref: decw   0x1be8 */
	decw   D_1be8
	jl   L_00c0
	/* TODO: data ref: mov    0x1be6,%bx */
	mov    D_1be6,%bx
	/* TODO: data ref: incw   0x1be6 */
	incw   D_1be6
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_00c8
L_00c0:
	/* TODO: data ref: mov    $0x1be6,%ax */
	mov    $D_1be6,%ax
	push   %ax
	call   L_0448
	pop    %cx
L_00c8:
	mov    %ax,%di
	cmp    $0xffff,%di
	jne   L_00d2
	jmp   L_0253
L_00d2:
	mov    %di,%ax
	jmp   L_023f
L_00d7:
	/* TODO: data ref: cmpw   $0x0,0x1c92 */
	cmpw   $0x0,D_1c92
	jne   L_010b
L_00de:
	/* TODO: data ref: decw   0x1bf0 */
	decw   D_1bf0
	jl   L_00f4
	/* TODO: data ref: mov    0x1bee,%bx */
	mov    D_1bee,%bx
	/* TODO: data ref: incw   0x1bee */
	incw   D_1bee
	movb   $0x20,(%bx)
	mov    $0x20,%ax
	jmp   L_0102
L_00f4:
	/* TODO: data ref: mov    $0x1bee,%ax */
	mov    $D_1bee,%ax
	push   %ax
	mov    $0x20,%ax
	push   %ax
	call   L_12fc
	add    $0x4,%sp
L_0102:
	inc    %si
	test   $0x7,%si
	jne   L_00de
L_0109:
	jmp   L_00aa
L_010b:
	/* TODO: data ref: cmpw   $0x1,0x1c92 */
	cmpw   $0x1,D_1c92
	jne   L_0149
L_0112:
	/* TODO: data ref: decw   0x1bf0 */
	decw   D_1bf0
	jl   L_0128
	/* TODO: data ref: mov    0x1bee,%bx */
	mov    D_1bee,%bx
	/* TODO: data ref: incw   0x1bee */
	incw   D_1bee
	movb   $0x20,(%bx)
	mov    $0x20,%ax
	jmp   L_0136
L_0128:
	/* TODO: data ref: mov    $0x1bee,%ax */
	mov    $D_1bee,%ax
	push   %ax
	mov    $0x20,%ax
	push   %ax
	call   L_12fc
	add    $0x4,%sp
L_0136:
	inc    %si
	lea    -0x1(%si),%ax
	cwtd
	/* TODO: data ref: idivw  0x1c98 */
	idivw  D_1c98
	/* TODO: data ref: mov    0x1c98,%ax */
	mov    D_1c98,%ax
	dec    %ax
	cmp    %ax,%dx
	jne   L_0112
L_0147:
	jmp   L_0109
L_0149:
	movw   $0x0,-0x6(%bp)
L_014e:
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x1c92,%ax */
	cmp    D_1c92,%ax
	jge   L_0167
	mov    -0x6(%bp),%bx
	shl    %bx
	/* TODO: data ref: cmp    %si,0x1c98(%bx) */
	cmp    %si,D_1c98(%bx)
	jg   L_0167
	incw   -0x6(%bp)
	jmp   L_014e
L_0167:
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x1c92,%ax */
	cmp    D_1c92,%ax
	jne   L_0197
	/* TODO: data ref: decw   0x1bf0 */
	decw   D_1bf0
	jl   L_0186
	/* TODO: data ref: mov    0x1bee,%bx */
	mov    D_1bee,%bx
	/* TODO: data ref: incw   0x1bee */
	incw   D_1bee
	movb   $0x20,(%bx)
	mov    $0x20,%ax
	jmp   L_0194
L_0186:
	/* TODO: data ref: mov    $0x1bee,%ax */
	mov    $D_1bee,%ax
	push   %ax
	mov    $0x20,%ax
	push   %ax
	call   L_12fc
	add    $0x4,%sp
L_0194:
	inc    %si
L_0195:
	jmp   L_0147
L_0197:
	mov    -0x6(%bp),%bx
	shl    %bx
	/* TODO: data ref: cmp    0x1c98(%bx),%si */
	cmp    D_1c98(%bx),%si
	jge   L_0195
	/* TODO: data ref: decw   0x1bf0 */
	decw   D_1bf0
	jl   L_01b8
	/* TODO: data ref: mov    0x1bee,%bx */
	mov    D_1bee,%bx
	/* TODO: data ref: incw   0x1bee */
	incw   D_1bee
	movb   $0x20,(%bx)
	mov    $0x20,%ax
	jmp   L_01c6
L_01b8:
	/* TODO: data ref: mov    $0x1bee,%ax */
	mov    $D_1bee,%ax
	push   %ax
	mov    $0x20,%ax
	push   %ax
	call   L_12fc
	add    $0x4,%sp
L_01c6:
	inc    %si
	jmp   L_0197
L_01c9:
	or     %si,%si
	je   L_01ce
	dec    %si
L_01ce:
	/* TODO: data ref: decw   0x1bf0 */
	decw   D_1bf0
	jl   L_01e4
	/* TODO: data ref: mov    0x1bee,%bx */
	mov    D_1bee,%bx
	/* TODO: data ref: incw   0x1bee */
	incw   D_1bee
	movb   $0x8,(%bx)
	mov    $0x8,%ax
L_01e2:
	jmp   L_0195
L_01e4:
	/* TODO: data ref: mov    $0x1bee,%ax */
	mov    $D_1bee,%ax
	push   %ax
	mov    $0x8,%ax
	push   %ax
	call   L_12fc
	add    $0x4,%sp
	jmp   L_01e2
L_01f4:
	/* TODO: data ref: decw   0x1bf0 */
	decw   D_1bf0
	jl   L_020b
	/* TODO: data ref: mov    0x1bee,%bx */
	mov    D_1bee,%bx
	/* TODO: data ref: incw   0x1bee */
	incw   D_1bee
	mov    %di,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_0216
L_020b:
	/* TODO: data ref: mov    $0x1bee,%ax */
	mov    $D_1bee,%ax
	push   %ax
	push   %di
	call   L_12fc
	add    $0x4,%sp
L_0216:
	inc    %si
L_0217:
	jmp   L_01e2
L_0219:
	/* TODO: data ref: decw   0x1bf0 */
	decw   D_1bf0
	jl   L_0230
	/* TODO: data ref: mov    0x1bee,%bx */
	mov    D_1bee,%bx
	/* TODO: data ref: incw   0x1bee */
	incw   D_1bee
	mov    %di,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_023b
L_0230:
	/* TODO: data ref: mov    $0x1bee,%ax */
	mov    $D_1bee,%ax
	push   %ax
	push   %di
	call   L_12fc
	add    $0x4,%sp
L_023b:
	xor    %si,%si
	jmp   L_0217
L_023f:
	cmp    $0x8,%ax
	je   L_01c9
	cmp    $0x9,%ax
	jne   L_024c
	jmp   L_00d7
L_024c:
	cmp    $0xa,%ax
	je   L_0219
	jmp   L_01f4
L_0253:
	cmpw   $0x0,0x4(%bp)
	jle   L_025c
	jmp   L_0071
L_025c:
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0268:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	/* TODO: data ref: movw   $0x0,0x1c92 */
	movw   $0x0,D_1c92
	inc    %di
L_0278:
	xor    %si,%si
L_027a:
	cmpb   $0x30,(%di)
	jl   L_02a0
	cmpb   $0x39,(%di)
	jg   L_02a0
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
	jmp   L_027a
L_02a0:
	or     %si,%si
	jle   L_02aa
	cmp    $0x100,%si
	jle   L_02c0
L_02aa:
	/* TODO: data ref: mov    $0x1855,%ax */
	mov    $D_1855,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1bf6,%ax */
	mov    $D_1bf6,%ax
	push   %ax
	call   L_0396
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_02c0:
	/* TODO: data ref: cmpw   $0x0,0x1c92 */
	cmpw   $0x0,D_1c92
	jle   L_02d5
	/* TODO: data ref: mov    0x1c92,%ax */
	mov    D_1c92,%ax
	dec    %ax
	mov    %ax,%bx
	shl    %bx
	/* TODO: data ref: cmp    0x1c98(%bx),%si */
	cmp    D_1c98(%bx),%si
	jle   L_02aa
L_02d5:
	/* TODO: data ref: mov    0x1c92,%bx */
	mov    D_1c92,%bx
	shl    %bx
	/* TODO: data ref: mov    %si,0x1c98(%bx) */
	mov    %si,D_1c98(%bx)
	/* TODO: data ref: incw   0x1c92 */
	incw   D_1c92
	cmpb   $0x0,(%di)
	je   L_02f2
	mov    %di,%bx
	inc    %di
	cmpb   $0x2c,(%bx)
	je   L_0278
	jmp   L_02aa
L_02f2:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_02f6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	push   %si
	call   L_1459
	pop    %cx
	cmpb   $0x77,(%di)
	jne   L_0320
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_03c2
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	jmp   L_035c
L_0320:
	cmpb   $0x61,(%di)
	jne   L_035e
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1760
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jge   L_0349
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_03c2
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_0349:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	call   L_0554
	add    $0x8,%sp
L_035c:
	jmp   L_036d
L_035e:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1760
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_036d:
	cmpw   $0x0,-0x6(%bp)
	jge   L_0377
	xor    %ax,%ax
	jmp   L_0390
L_0377:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,0x7(%si)
	cmpb   $0x72,(%di)
	je   L_038a
	orb    $0x2,0x6(%si)
	jmp   L_038e
L_038a:
	orb    $0x1,0x6(%si)
L_038e:
	mov    %si,%ax
L_0390:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0396:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_0572
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_03bc
	mov    $0xffff,%ax
	jmp   L_03be
L_03bc:
	xor    %ax,%ax
L_03be:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_03c2:
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
	jns    L_03d6
	neg	%ax
	mov	%ax,D_1f60	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_03d6:
	pop    %bp
	ret


L_03d8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x1868,%di */
	mov    $D_1868,%di
	/* TODO: data ref: mov    0x1f60,%ax */
	mov    D_1f60,%ax
	/* TODO: data ref: cmp    0x18c2,%ax */
	cmp    D_18c2,%ax
	jge   L_03f3
	/* TODO: data ref: mov    0x1f60,%bx */
	mov    D_1f60,%bx
	shl    %bx
	/* TODO: data ref: mov    0x187c(%bx),%di */
	mov    D_187c(%bx),%di
L_03f3:
	pushw  0x4(%bp)
	call   L_0538
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_0420
	push   %si
	pushw  0x4(%bp)
	mov    $0x2,%ax
	push   %ax
	call   L_17dc
	add    $0x6,%sp
	mov    $0x2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1876,%ax */
	mov    $D_1876,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_17dc
	add    $0x6,%sp
L_0420:
	push   %di
	call   L_0538
	pop    %cx
	push   %ax
	push   %di
	mov    $0x2,%ax
	push   %ax
	call   L_17dc
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1879,%ax */
	mov    $D_1879,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_17dc
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0448:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_045f
	mov    $0xffff,%ax
	jmp   L_046b
L_045f:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_046d
	mov    $0xffff,%ax
L_046b:
	jmp   L_04e5
L_046d:
	cmpw   $0x0,0x4(%di)
	jne   L_04a1
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0488
	mov    0x7(%di),%al
	cbtw
	/* TODO: data ref: add    $0x1b12,%ax */
	add    $D_1b12,%ax
	mov    %ax,0x4(%di)
	jmp   L_046d
L_0488:
	mov    $0x200,%ax
	push   %ax
	call   L_14d0
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_049d
	orb    $0x4,0x6(%di)
	jmp   L_046d
L_049d:
	orb    $0x8,0x6(%di)
L_04a1:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_04b4
	mov    $0x1,%ax
	jmp   L_04b7
L_04b4:
	mov    $0x200,%ax
L_04b7:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1776
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_04e7
	cmpw   $0xffff,0x2(%di)
	jne   L_04d9
	orb    $0x10,0x6(%di)
	jmp   L_04dd
L_04d9:
	orb    $0x20,0x6(%di)
L_04dd:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_04e5:
	jmp   L_04f1
L_04e7:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_04f1:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_04f6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	cmpw   $0x0,0x4(%di)
	je   L_0515
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_0515
	pushw  0x4(%di)
	call   L_161a
	pop    %cx
L_0515:
	andb   $0xf3,0x6(%di)
	mov    0x6(%bp),%ax
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_0529
	orb    $0x4,0x6(%di)
	jmp   L_052e
L_0529:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
L_052e:
	movw   $0x0,0x2(%di)
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0538:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_0543:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_054e
	inc    %si
	jmp   L_0543
L_054e:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0554:
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
L_056f:
	pop    %si
	pop    %bp
	ret


L_0572:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_058a:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_059c
	jmp   L_0b39
L_059c:
	cmpb   $0x25,-0x5(%bp)
	je   L_05af
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_05ad:
	jmp   L_058a
L_05af:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_05da
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_120a
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_05da:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_05f3
	incb   -0xa6(%bp)
	inc    %di
L_05f3:
	cmpb   $0x30,(%di)
	jne   L_05fe
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_05fe:
	cmpb   $0x2a,(%di)
	je   L_062e
	movw   $0x0,-0xe(%bp)
L_0608:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0644
	cmpw   $0x9,-0xc(%bp)
	jg   L_0644
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_0608
L_062e:
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
L_0644:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_068b
	cmpb   $0x2a,(%di)
	je   L_068d
	movw   $0x0,-0x10(%bp)
L_065d:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0687
	cmpw   $0x9,-0xc(%bp)
	jg   L_0687
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_065d
L_0687:
	addw   $0x30,-0xc(%bp)
L_068b:
	jmp   L_06a4
L_068d:
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
L_06a4:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_0aae
L_06af:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_06c0
L_06b7:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_06f0
	jmp   L_06e9
L_06c0:
	cmp    $0x64,%ax
	je   L_06f2
	cmp    $0x6f,%ax
	je   L_06d6
	cmp    $0x75,%ax
	je   L_06e9
	cmp    $0x78,%ax
	je   L_06e4
	jmp   L_06b7
L_06d6:
	movw   $0x8,-0x12(%bp)
	jmp   L_06e9
L_06dd:
	movw   $0x8,-0x12(%bp)
	jmp   L_06f0
L_06e4:
	movw   $0x10,-0x12(%bp)
L_06e9:
	jmp   L_073b
L_06eb:
	movw   $0x10,-0x12(%bp)
L_06f0:
	jmp   L_074b
L_06f2:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_070f
L_0702:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_070f:
	cmpw   $0x0,-0x14(%bp)
	jg   L_0759
	jl   L_071d
	cmpw   $0x0,-0x16(%bp)
	jae   L_0759
L_071d:
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
	jmp   L_0759
L_073b:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0759
L_074b:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0759:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0763
	cmpw   $0x0,-0x16(%bp)
L_0763:
	jne   L_0774
	cmpw   $0x0,-0x10(%bp)
	jne   L_0774
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_0774:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_077b:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0785
	cmpw   $0x0,-0x16(%bp)
L_0785:
	je   L_07d0
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_0c98
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_07a6
	mov    $0x30,%ax
	jmp   L_07a9
L_07a6:
	mov    $0x57,%ax
L_07a9:
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
	call   L_0b90
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_077b
L_07d0:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_081f
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_07d0
L_07ee:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_07fa:
	cmpw   $0x2,-0xc(%bp)
	jge   L_081f
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_081a
	decw   -0x8(%bp)
L_081a:
	incw   -0xc(%bp)
	jmp   L_07fa
L_081f:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0826:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_083d
	movw   $0x0,-0xc(%bp)
L_083d:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_084c
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_084c:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_120a
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_086f:
	jmp   L_05ad
L_0872:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_0884
	/* TODO: data ref: movw   $0x1b26,-0xa(%bp) */
	movw   $D_1b26,-0xa(%bp)
L_0884:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_0893
	movw   $0x7fff,-0xc(%bp)
L_0893:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0899:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0826
	decw   -0xc(%bp)
	jl   L_0826
	incw   -0x8(%bp)
	jmp   L_0899
L_08ab:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_08b3:
	jmp   L_086f
L_08b5:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_08c1
	movw   $0x6,-0x10(%bp)
L_08c1:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0e96
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_08f6
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_08f6:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_090b
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_0923
L_090b:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_090b
L_0923:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_0936
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_0936:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_0955
L_0942:
	decw   -0xc(%bp)
	jl   L_0955
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_0942
L_0955:
	cmpw   $0x0,-0xc(%bp)
	jg   L_095e
	jmp   L_081f
L_095e:
	decw   -0xc(%bp)
	jge   L_0966
	jmp   L_081f
L_0966:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_095e
L_097b:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0989
	movw   $0x6,-0x10(%bp)
	jmp   L_098c
L_0989:
	incw   -0x10(%bp)
L_098c:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0e6a
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_09c1
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_09c1:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_09cc
	incw   -0x1a(%bp)
L_09cc:
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
L_09ee:
	decw   -0xc(%bp)
	jle   L_0a08
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_09ee
L_0a08:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_0a25
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_0a36
L_0a25:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_0a36:
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
	jmp   L_081f
L_0a67:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0a73
	movw   $0x6,-0x10(%bp)
L_0a73:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_0cee
	add    $0xc,%sp
	add    $0x8,%si
L_0a91:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_0a91
	decw   -0x8(%bp)
	jmp   L_081f
L_0aa2:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_0aab:
	jmp   L_08b3
L_0aae:
	cmp    $0x65,%ax
	jne   L_0ab6
	jmp   L_097b
L_0ab6:
	jg   L_0af7
	cmp    $0x4f,%ax
	jne   L_0ac0
	jmp   L_06d6
L_0ac0:
	jg   L_0ad7
	cmp    $0x25,%ax
	je   L_0aa2
	cmp    $0x44,%ax
	jne   L_0acf
	jmp   L_06f2
L_0acf:
	cmp    $0x4c,%ax
L_0ad2:
	jne   L_0ad7
	jmp   L_06af
L_0ad7:
	cmp    $0x55,%ax
	jne   L_0adf
	jmp   L_073b
L_0adf:
	cmp    $0x58,%ax
	jne   L_0ae7
	jmp   L_06e4
L_0ae7:
	cmp    $0x63,%ax
	jne   L_0aef
	jmp   L_07ee
L_0aef:
	cmp    $0x64,%ax
	jne   L_0af7
	jmp   L_0702
L_0af7:
	cmp    $0x6f,%ax
	jne   L_0aff
	jmp   L_06dd
L_0aff:
	jg   L_0b16
	cmp    $0x66,%ax
	jne   L_0b09
	jmp   L_08b5
L_0b09:
	cmp    $0x67,%ax
	jne   L_0b11
	jmp   L_0a67
L_0b11:
	cmp    $0x6c,%ax
	je   L_0ad2
L_0b16:
	cmp    $0x72,%ax
	jne   L_0b1e
	jmp   L_08ab
L_0b1e:
	cmp    $0x73,%ax
	jne   L_0b26
	jmp   L_0872
L_0b26:
	cmp    $0x75,%ax
	jne   L_0b2e
	jmp   L_074b
L_0b2e:
	cmp    $0x78,%ax
	jne   L_0b36
	jmp   L_06eb
L_0b36:
	jmp   L_0aab
L_0b39:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0b5d
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_120a
	add    $0x8,%sp
L_0b5d:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_0ccc


L_0b67:
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
	call   L_0ccc


L_0b90:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_0bae
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_0bae:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_0bb7:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_0bb7
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_0bc7:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_0bc7
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0b67
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_0c08
	jne   L_0bfd
	or     $0x0,%ax
	je   L_0c08
L_0bfd:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_0c08:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_0ccc


L_0c10:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0c2c
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0c2c:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_0c43
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_0c43:
	push   %dx
	push   %ax
	call   L_0b90
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_0c5d
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_0c5d:
	ret
	call   L_0ccc
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0c10
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0b67
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_0ccc


L_0c98:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_0b90
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_0b67
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_0ccc:
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


L_0cee:
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
	call   L_0e6a
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_0d28
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_0d28:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_0d2f:
	cmpw   $0x0,-0xa(%bp)
	jle   L_0d49
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_0d49
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_0d2f
L_0d49:
	cmpw   $0x0,-0x8(%bp)
	jl   L_0d5a
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_0d66
L_0d5a:
	cmpw   $0x0,-0x8(%bp)
	jge   L_0d64
	cmpw   $0xfffd,-0x8(%bp)
L_0d64:
	jge   L_0de5
L_0d66:
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
L_0d81:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_0d9b
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_0d81
L_0d9b:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_0db7
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_0dbd
L_0db7:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_0dbd:
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
	jmp   L_0e55
L_0de5:
	cmpw   $0x0,-0x8(%bp)
	jg   L_0e07
	cmpb   $0x30,(%di)
	je   L_0df6
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0df6:
	cmpw   $0x0,-0x8(%bp)
	jge   L_0e07
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_0df6
L_0e07:
	movw   $0x1,-0xa(%bp)
L_0e0c:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_0e34
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
	jne   L_0e2f
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0e2f:
	incw   -0xa(%bp)
	jmp   L_0e0c
L_0e34:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_0e55
L_0e3c:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_0e4f
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_0e3c
L_0e4f:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_0e55:
	cmpb   $0x2e,-0x1(%si)
	jne   L_0e5c
	dec    %si
L_0e5c:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0e6a:
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
	call   L_0ec1
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0e96:
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
	call   L_0ec1
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ec1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_0ed4
	movw   $0x0,0xc(%bp)
L_0ed4:
	cmpw   $0x4f,0xc(%bp)
	jl   L_0edf
	movw   $0x4e,0xc(%bp)
L_0edf:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	/* TODO: data ref: mov    $0x1b2e,%si */
	mov    $D_1b2e,%si
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x1b7e */
	fcompl D_1b7e
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jae   L_0f19
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x46(%bp)
	fstpl  0x4(%bp)
L_0f19:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_17f6
	fstpl  -0x46(%bp)
	add    $0xa,%sp
	fldl   -0x46(%bp)
	fstpl  0x4(%bp)
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x1b86 */
	fcompl D_1b86
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jne   L_0f5a
	jmp   L_1030
L_0f5a:
	/* TODO: data ref: mov    $0x1b2e,%si */
	mov    $D_1b2e,%si
L_0f5d:
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x1b8e */
	fcompl D_1b8e
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jb   L_0fe9
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	/* TODO: data ref: fmull  0x1b96 */
	fmull  D_1b96
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_17f6
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	fldl   -0x4e(%bp)
	/* TODO: data ref: fmull  0x1b9e */
	fmull  D_1b9e
	/* TODO: data ref: faddl  0x1ba6 */
	faddl  D_1ba6
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
	jne   L_0f5d
	fldl   -0xc(%bp)
	/* TODO: data ref: faddl  0x1bae */
	faddl  D_1bae
	fstpl  -0xc(%bp)
	lea    -0x3e(%bp),%ax
	lea    -0x1(%di),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	jmp   L_0f5d
L_0fe9:
	mov    $0x1,%ax
	push   %ax
	push   %si
	fldl   -0xc(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   L_1194
	add    $0x6,%sp
	mov    %ax,%si
L_1004:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_1026
	xor    %ax,%ax
	push   %ax
	push   %si
	lea    -0x3e(%bp),%ax
	mov    %di,%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_1194
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_1004
L_1026:
	mov    %si,%ax
	/* TODO: data ref: sub    $0x1b2e,%ax */
	sub    $D_1b2e,%ax
	mov    %ax,%di
	jmp   L_10b0
L_1030:
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x1bb6 */
	fcompl D_1bb6
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jbe   L_10b0
L_1046:
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x1bbe */
	fmull  D_1bbe
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x1bc6 */
	fcompl D_1bc6
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_107c
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	sub    $0x3,%di
	jmp   L_1046
L_107c:
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x1bce */
	fmull  D_1bce
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x1bd6 */
	fcompl D_1bd6
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_10b0
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_107c
L_10b0:
	mov    0xc(%bp),%ax
	/* TODO: data ref: add    $0x1b2e,%ax */
	add    $D_1b2e,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_10c2
	add    %di,-0x16(%bp)
L_10c2:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	/* TODO: data ref: cmpw   $0x1b2e,-0x16(%bp) */
	cmpw   $D_1b2e,-0x16(%bp)
	jae   L_10d8
	/* TODO: data ref: movb   $0x0,0x1b2e */
	movb   $0x0,D_1b2e
	/* TODO: data ref: mov    $0x1b2e,%ax */
	mov    $D_1b2e,%ax
	jmp   L_1141
L_10d8:
	cmp    -0x16(%bp),%si
	ja   L_1132
	/* TODO: data ref: cmp    $0x1b7e,%si */
	cmp    $D_1b7e,%si
	jae   L_1132
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x1bde */
	fmull  D_1bde
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_17f6
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	fldl   -0x4e(%bp)
	fstpl  0x4(%bp)
	xor    %ax,%ax
	push   %ax
	push   %si
	fldl   -0x14(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   L_1194
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_10d8
L_1132:
	/* TODO: data ref: cmpw   $0x1b7e,-0x16(%bp) */
	cmpw   $D_1b7e,-0x16(%bp)
	jb   L_1143
	/* TODO: data ref: movb   $0x0,0x1b7d */
	movb   $0x0,D_1b7d
	/* TODO: data ref: mov    $0x1b2e,%ax */
	mov    $D_1b2e,%ax
L_1141:
	jmp   L_118d
L_1143:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_114b:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_1187
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	/* TODO: data ref: cmpw   $0x1b2e,-0x16(%bp) */
	cmpw   $D_1b2e,-0x16(%bp)
	jbe   L_116a
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_114b
L_116a:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_114b
	/* TODO: data ref: cmp    $0x1b2e,%si */
	cmp    $D_1b2e,%si
	jbe   L_1184
	movb   $0x30,(%si)
L_1184:
	inc    %si
	jmp   L_114b
L_1187:
	movb   $0x0,(%si)
	/* TODO: data ref: mov    $0x1b2e,%ax */
	mov    $D_1b2e,%ax
L_118d:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1194:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%si
	xor    %di,%di
L_119f:
	or     %si,%si
	je   L_11b0
	mov    %si,%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	mov    %ax,%si
	inc    %di
	jmp   L_119f
L_11b0:
	cmpw   $0x0,0x8(%bp)
	jne   L_11c9
	mov    %di,%si
L_11b8:
	cmp    $0x4,%si
	jge   L_11c9
	mov    0x6(%bp),%bx
	movb   $0x30,(%bx)
	incw   0x6(%bp)
	inc    %si
	jmp   L_11b8
L_11c9:
	mov    0x6(%bp),%ax
	add    %di,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%si
L_11d9:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_1202
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
	jmp   L_11d9
L_1202:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_120a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1217:
	cmpw   $0x0,0x8(%bp)
	jl   L_1220
	jmp   L_12ae
L_1220:
	cmpb   $0x2d,(%di)
	jne   L_125d
	cmpw   $0x30,0xc(%bp)
	jne   L_125d
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_124c
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
	jmp   L_125c
L_124c:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_12fc
	add    $0x4,%sp
L_125c:
	dec    %si
L_125d:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_127b
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
	jmp   L_1287
L_127b:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_12fc
	add    $0x4,%sp
L_1287:
	incw   0x8(%bp)
	jmp   L_1217
L_128c:
	dec    %si
	jl   L_12c2
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_12b0
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
L_12ae:
	jmp   L_128c
L_12b0:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_12fc
	add    $0x4,%sp
	jmp   L_128c
L_12c2:
	cmpw   $0x0,0x8(%bp)
	je   L_12f7
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_12e6
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
	jmp   L_12f2
L_12e6:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_12fc
	add    $0x4,%sp
L_12f2:
	decw   0x8(%bp)
	jmp   L_12c2
L_12f7:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_12fc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_1317
	mov    $0xffff,%ax
	jmp   L_13de
L_1317:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_134a
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
	call   L_17dc
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_13cf
L_134a:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_139c
	/* TODO: data ref: cmp    $0x1bee,%di */
	cmp    $D_1bee,%di
	jne   L_1375
	/* TODO: data ref: mov    0x1bf5,%al */
	mov    D_1bf5,%al
	cbtw
	push   %ax
	call   L_1738
	pop    %cx
	or     %ax,%ax
	je   L_136a
	orb    $0x4,0x6(%di)
	jmp   L_1317
L_136a:
	/* TODO: data ref: movw   $0x1f68,0x4(%di) */
	movw   $D_1f68,0x4(%di)
	/* TODO: data ref: movw   $0x1f68,(%di) */
	movw   $D_1f68,(%di)
	jmp   L_1317
L_1375:
	mov    $0x200,%ax
	push   %ax
	call   L_14d0
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_138c
	orb    $0x4,0x6(%di)
	jmp   L_1317
L_138c:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_13be
L_139c:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_13be
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_17dc
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_13be:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_13cf:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_13e0
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_13de:
	jmp   L_13e3
L_13e0:
	mov    0x4(%bp),%ax
L_13e3:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_13ea:
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
	jne   L_1436
	mov    0x4(%di),%si
	or     %si,%si
	je   L_1436
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_1436
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_17dc
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_1436
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_1438
L_1436:
	xor    %ax,%ax
L_1438:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_143d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x1be6,%di */
	mov    $D_1be6,%di
L_1445:
	/* TODO: data ref: cmp    0x1c86,%di */
	cmp    D_1c86,%di
	jae   L_1455
	push   %di
	call   L_1459
	pop    %cx
	add    $0x8,%di
	jmp   L_1445
L_1455:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1459:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_14ac
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_14ac
	push   %di
	call   L_13ea
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1724
	pop    %cx
	or     %ax,%ax
	jge   L_148e
	mov    $0xffff,%si
L_148e:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_149e
	pushw  0x4(%di)
	call   L_161a
	pop    %cx
L_149e:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_14ac
	movw   $0x0,0x4(%di)
L_14ac:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* _exit: provided by TLVC libc — elided */

L_14d0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: cmpw   $0x0,0x1c88 */
	cmpw   $0x0,D_1c88
	jne   L_14fb
	/* TODO: data ref: mov    $0x1c8a,%ax */
	mov    $D_1c8a,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x1c88 */
	mov    %ax,D_1c88
	/* TODO: data ref: mov    $0x1c88,%ax */
	mov    $D_1c88,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x1c8a */
	mov    %ax,D_1c8a
	/* TODO: data ref: movw   $0x1c8a,0x236a */
	/* dual-reloc split: movw $D_1c8a,D_236a */
	mov	$D_1c8a,%ax
	mov	%ax,D_236a
	/* TODO: data ref: movw   $0x1c88,0x2368 */
	/* dual-reloc split: movw $D_1c88,D_2368 */
	mov	$D_1c88,%ax
	mov	%ax,D_2368
L_14fb:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    0x2368,%di */
	mov    D_2368,%di
L_150f:
	/* TODO: data ref: movw   $0x0,0x1c8c */
	movw   $0x0,D_1c8c
L_1515:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_1544
L_151c:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_152b
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_151c
L_152b:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_1544
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_1544
	jmp   L_15eb
L_1544:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_1515
	/* TODO: data ref: cmp    0x236a,%si */
	cmp    D_236a,%si
	jne   L_155d
	/* TODO: data ref: cmp    $0x1c88,%di */
	cmp    $D_1c88,%di
	je   L_1561
L_155d:
	xor    %ax,%ax
	jmp   L_1599
L_1561:
	/* TODO: data ref: incw   0x1c8c */
	incw   D_1c8c
	/* TODO: data ref: cmpw   $0x1,0x1c8c */
	cmpw   $0x1,D_1c8c
	jle   L_1515
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	/* TODO: data ref: mov    %ax,0x1c8c */
	mov    %ax,D_1c8c
	xor    %ax,%ax
	push   %ax
	call   L_16fe
	pop    %cx
	mov    %ax,%si
	/* TODO: data ref: mov    0x1c8c,%ax */
	mov    D_1c8c,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_159b
	xor    %ax,%ax
L_1599:
	jmp   L_15ae
L_159b:
	/* TODO: data ref: mov    0x1c8c,%ax */
	mov    D_1c8c,%ax
	shl    %ax
	push   %ax
	call   L_16fe
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_15b0
	xor    %ax,%ax
L_15ae:
	jmp   L_1615
L_15b0:
	/* TODO: data ref: mov    0x236a,%bx */
	mov    D_236a,%bx
	mov    %si,(%bx)
	/* TODO: data ref: mov    0x236a,%ax */
	mov    D_236a,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_15ce
	/* TODO: data ref: mov    0x236a,%bx */
	mov    D_236a,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x236a,%bx */
	mov    D_236a,%bx
	mov    %ax,(%bx)
L_15ce:
	/* TODO: data ref: mov    0x1c8c,%ax */
	mov    D_1c8c,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	/* TODO: data ref: mov    %ax,0x236a */
	mov    %ax,D_236a
	/* TODO: data ref: mov    $0x1c88,%ax */
	mov    $D_1c88,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x236a,%bx */
	mov    D_236a,%bx
	mov    %ax,(%bx)
	jmp   L_150f
L_15eb:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	/* TODO: data ref: mov    %ax,0x2368 */
	mov    %ax,D_2368
	cmp    %ax,%si
	jbe   L_160a
	/* TODO: data ref: mov    0x2368,%bx */
	mov    D_2368,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x236c */
	mov    %ax,D_236c
	/* TODO: data ref: mov    0x2368,%bx */
	mov    D_2368,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_160a:
	/* TODO: data ref: mov    0x2368,%ax */
	mov    D_2368,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_1615:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_161a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	/* TODO: data ref: mov    %si,0x2368 */
	mov    %si,D_2368
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
	je   L_164f
	push   %di
	call   L_161a
	pop    %cx
L_164f:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_14d0
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_166e
	cmp    %di,%si
	jne   L_1672
L_166e:
	mov    %si,%ax
	jmp   L_16da
L_1672:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_1691
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_1691:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_16af
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_1691
L_16af:
	cmp    %di,%si
	jae   L_16d8
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_16d8
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
	/* TODO: data ref: mov    0x236c,%ax */
	mov    D_236c,%ax
	mov    %ax,(%bx)
L_16d8:
	mov    %si,%ax
L_16da:
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
	jns    L_16f5
	neg	%ax
	mov	%ax,D_1f60	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_16fc
L_16f5:
	mov    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x1c8e */
	mov    %bx,D_1c8e
L_16fc:
	pop    %bp
	ret


L_16fe:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	/* TODO: data ref: add    0x1c8e,%ax */
	add    D_1c8e,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_1715
	neg	%ax
	mov	%ax,D_1f60	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_1722
L_1715:
	/* TODO: data ref: mov    0x1c8e,%bx */
	mov    D_1c8e,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x1c8e */
	mov    %bx,D_1c8e
L_1722:
	pop    %bp
	ret


L_1724:
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
	jns    L_1735
	neg	%ax
	mov	%ax,D_1f60	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1735:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1738:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_17a9
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_1755
	xor    %ax,%ax
	jmp   L_1758
L_1755:
	mov    $0x1,%ax
L_1758:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1760:
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
	jns    L_1774
	neg	%ax
	mov	%ax,D_1f60	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1774:
	pop    %bp
	ret


L_1776:
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
	jns    L_178d
	neg	%ax
	mov	%ax,D_1f60	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_178d:
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
	call   L_17c2
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_17a9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_17c2
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_17c2:
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
	jns    L_17d9
	neg	%ax
	mov	%ax,D_1f60	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_17d9:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_17dc:
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
	jns    L_17f3
	neg	%ax
	mov	%ax,D_1f60	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_17f3:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_17f6:
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


L_1812:
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
	jns    L_1823
	neg	%ax
	mov	%ax,D_1f60	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_1823:
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
	mov	%ax,D_1f60	/* errno */
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
D_1828:
	.byte	0xbf, 0x0f, 0x2c, 0x18	/* |..,.| */
D_182c:
	.ascii	"@(#)expand.c     4.2 (Berkeley) 2/7/81\0"
D_1853:
	.byte	0x72, 0x00	/* |r.| */
D_1855:
	.byte	0x42, 0x61, 0x64, 0x20, 0x74, 0x61, 0x62, 0x20	/* |Bad tab | */
	.byte	0x73, 0x74, 0x6f, 0x70, 0x20, 0x73, 0x70, 0x65	/* |stop spe| */
	.byte	0x63, 0x0a, 0x00	/* |c..| */
D_1868:
	.ascii	"Unknown error\0"
D_1876:
	.byte	0x3a, 0x20, 0x00	/* |: .| */
D_1879:
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_187c:
	.byte	0xc4, 0x18, 0xcc, 0x18, 0xd6, 0x18, 0xf0, 0x18	/* |........| */
	.byte	0x00, 0x19, 0x18, 0x19, 0x22, 0x19, 0x3c, 0x19	/* |....".<.| */
	.word	D_194e	/* ptr: 0x194e */
	.byte	0x60, 0x19, 0x70, 0x19, 0x7c, 0x19, 0x8e, 0x19	/* |`.p.|...| */
	.byte	0x9e, 0x19, 0xb0, 0x19, 0xbc, 0x19, 0xd2, 0x19	/* |........| */
	.byte	0xe4, 0x19, 0xf0, 0x19, 0x02, 0x1a, 0x11, 0x1a	/* |........| */
	.byte	0x21, 0x1a, 0x30, 0x1a, 0x41, 0x1a, 0x55, 0x1a	/* |!.0.A.U.| */
	.byte	0x69, 0x1a, 0x7a, 0x1a, 0x89, 0x1a, 0x98, 0x1a	/* |i.z.....| */
	.byte	0xb0, 0x1a, 0xbd, 0x1a, 0xd3, 0x1a, 0xe2, 0x1a	/* |........| */
	.byte	0xee, 0x1a, 0x01, 0x1b	/* |....| */
D_18c2:
	.byte	0x23, 0x00	/* |#.| */
D_18c4:
	.ascii	"Error 0\0"
D_18cc:
	.ascii	"Not owner\0"
D_18d6:
	.ascii	"No such file or directory\0"
D_18f0:
	.ascii	"No such process\0"
D_1900:
	.ascii	"Interrupted system call\0"
D_1918:
	.ascii	"I/O error\0"
D_1922:
	.ascii	"No such device or address\0"
D_193c:
	.ascii	"Arg list too long\0"
D_194e:
	.ascii	"Exec format error\0"
D_1960:
	.ascii	"Bad file number\0"
D_1970:
	.ascii	"No children\0"
D_197c:
	.ascii	"No more processes\0"
D_198e:
	.ascii	"Not enough core\0"
D_199e:
	.ascii	"Permission denied\0"
D_19b0:
	.ascii	"Bad address\0"
D_19bc:
	.ascii	"Block device required\0"
D_19d2:
	.ascii	"Mount device busy\0"
D_19e4:
	.ascii	"File exists\0"
D_19f0:
	.ascii	"Cross-device link\0"
D_1a02:
	.ascii	"No such device\0"
D_1a11:
	.ascii	"Not a directory\0"
D_1a21:
	.ascii	"Is a directory\0"
D_1a30:
	.ascii	"Invalid argument\0"
D_1a41:
	.ascii	"File table overflow\0"
D_1a55:
	.ascii	"Too many open files\0"
D_1a69:
	.ascii	"Not a typewriter\0"
D_1a7a:
	.ascii	"Text file busy\0"
D_1a89:
	.ascii	"File too large\0"
D_1a98:
	.ascii	"No space left on device\0"
D_1ab0:
	.ascii	"Illegal seek\0"
D_1abd:
	.ascii	"Read-only file system\0"
D_1ad3:
	.ascii	"Too many links\0"
D_1ae2:
	.ascii	"Broken pipe\0"
D_1aee:
	.ascii	"Argument too large\0"
D_1b01:
	.ascii	"Result too large\0"
D_1b12:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_1b26:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_1b2e:
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
D_1b7d:
	.byte	0x00	/* |.| */
D_1b7e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_1b86:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_1b8e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_1b96:
	.byte	0x2c, 0x43, 0x1c, 0xeb, 0xe2, 0x36, 0x1a, 0x3f	/* |,C...6.?| */
D_1b9e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_1ba6:
	.byte	0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0xb9, 0x3f	/* |.......?| */
D_1bae:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_1bb6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_1bbe:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x8f, 0x40	/* |.....@.@| */
D_1bc6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_1bce:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_1bd6:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_1bde:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_1be6:
	.word	D_2168	/* ptr: 0x2168 */
D_1be8:
	.byte	0x00, 0x00	/* |..| */
	.word	D_2168	/* ptr: 0x2168 */
	.byte	0x01, 0x00	/* |..| */
D_1bee:
	.byte	0x00, 0x00	/* |..| */
D_1bf0:
	.byte	0x00, 0x00, 0x00, 0x00, 0x02	/* |.....| */
D_1bf5:
	.byte	0x01	/* |.| */
D_1bf6:
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
D_1c86:
	.byte	0x86, 0x1c	/* |..| */
D_1c88:
	.byte	0x00, 0x00	/* |..| */
D_1c8a:
	.byte	0x00, 0x00	/* |..| */
D_1c8c:
	.byte	0x00, 0x00	/* |..| */
D_1c8e:
	.byte	0x6e, 0x23	/* |n#| */

	.bss

	.space	2
D_1c92:	.space	2	/* bss word @ 0x1c92 */
	.space	4
D_1c98:	.space	2	/* bss word @ 0x1c98 */
	.space	198
D_1d60:	.space	2	/* bss word @ 0x1d60 */
	.space	510
D_1f60:	.space	2	/* bss word @ 0x1f60 */
	.space	6
D_1f68:	.space	2	/* bss word @ 0x1f68 */
	.space	510
D_2168:	.space	2	/* bss word @ 0x2168 */
	.space	510
D_2368:	.space	2	/* bss word @ 0x2368 */
D_236a:	.space	2	/* bss word @ 0x236a */
D_236c:	.space	2	/* bss word @ 0x236c */
D_236e:	/* endcore: initial break */

/* end of file */
