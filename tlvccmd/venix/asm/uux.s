/* uux.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 uux.s -o uux.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: uux  [linked executable]
//   Magic:    0x0107  OMAGIC (object/impure)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00004d8e  (19854 bytes)
//   a_data:   0x00000d04  (3332 bytes)
//   Load:  cs:0x0000 (text)   ds:0x6d8e (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x956,%sp
	movw   $0x0,-0x60(%bp)
	movw   $0x0,-0x62(%bp)
	movw   $0x0,-0x464(%bp)
	movw   $0x1,-0x466(%bp)
	movb   $0x0,-0x957(%bp)
	call   L_22c2
	/* TODO: data ref: mov    %ax,0x7ac4 */
	mov    %ax,D_7ac4
	/* TODO: data ref: movw   $0x0,0x6d96 */
	movw   $0x0,D_6d96
	/* TODO: data ref: movw   $0x1,0x7aa4 */
	movw   $0x1,D_7aa4
	/* TODO: data ref: movw   $0x0,0x7a94 */
	movw   $0x0,D_7a94
L_0072:
	cmpw   $0x1,0x4(%bp)
	jle   L_00f0
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x2d,(%bx)
	jne   L_00f0
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	mov    0x1(%bx),%al
	cbtw
	jmp   L_00d2
L_008f:
	movw   $0x1,-0x464(%bp)
	jmp   L_00d0
L_0097:
	movw   $0x0,-0x466(%bp)
	jmp   L_00e7
L_009f:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%ax
	inc    %ax
	inc    %ax
	push   %ax
	call   L_2672
	pop    %cx
	/* TODO: data ref: mov    %ax,0x6d96 */
	mov    %ax,D_6d96
	cmp    $0x0,%ax
	jg   L_00e7
	/* TODO: data ref: movw   $0x1,0x6d96 */
	movw   $0x1,D_6d96
	jmp   L_00e7
L_00bc:
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	/* TODO: data ref: mov    $0x6e25,%ax */
	mov    $D_6e25,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_3972
	add    $0x6,%sp
L_00d0:
	jmp   L_00e7
L_00d2:
	or     %ax,%ax
	je   L_008f
	cmp    $0x70,%ax
	je   L_008f
	cmp    $0x72,%ax
	je   L_0097
	cmp    $0x78,%ax
	je   L_009f
	jmp   L_00bc
L_00e7:
	decw   0x4(%bp)
	addw   $0x2,0x6(%bp)
	jmp   L_0072
L_00f0:
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_0109
	/* TODO: data ref: mov    $0x6e42,%ax */
	mov    $D_6e42,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6e36,%ax */
	mov    $D_6e36,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_0109:
	movb   $0x0,-0x462(%bp)
	addw   $0x2,0x6(%bp)
L_0112:
	cmpw   $0x1,0x4(%bp)
	jle   L_015a
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_0132
	mov    0x6(%bp),%bx
	pushw  (%bx)
	/* TODO: data ref: mov    $0x6e48,%ax */
	mov    $D_6e48,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_0132:
	/* TODO: data ref: mov    $0x6e52,%ax */
	mov    $D_6e52,%ax
	push   %ax
	lea    -0x462(%bp),%ax
	push   %ax
	call   L_4c16
	add    $0x4,%sp
	mov    0x6(%bp),%bx
	addw   $0x2,0x6(%bp)
	pushw  (%bx)
	lea    -0x462(%bp),%ax
	push   %ax
	call   L_4c16
	add    $0x4,%sp
	decw   0x4(%bp)
	jmp   L_0112
L_015a:
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_0174
	lea    -0x462(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6e54,%ax */
	mov    $D_6e54,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_0174:
	/* TODO: data ref: mov    $0x7aca,%ax */
	mov    $D_7aca,%ax
	push   %ax
	call   L_0d68
	pop    %cx
	mov    %ax,-0x956(%bp)
	or     %ax,%ax
	je   L_01b0
	/* TODO: data ref: mov    $0x6e6d,%ax */
	mov    $D_6e6d,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6e5e,%ax */
	mov    $D_6e5e,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	pushw  -0x956(%bp)
	/* TODO: data ref: mov    $0x6e76,%ax */
	mov    $D_6e76,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_01b0:
	/* TODO: data ref: pushw  0x6d94 */
	pushw  D_6d94
	call   L_23c0
	pop    %cx
	call   L_2740
	mov    %ax,-0x954(%bp)
	lea    -0x4ca(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x7a9a,%ax */
	mov    $D_7a9a,%ax
	push   %ax
	pushw  -0x954(%bp)
	call   L_120a
	add    $0x6,%sp
	/* TODO: data ref: pushw  0x7ac4 */
	pushw  D_7ac4
	/* TODO: data ref: mov    $0x6e84,%ax */
	mov    $D_6e84,%ax
	push   %ax
	lea    -0x94a(%bp),%ax
	push   %ax
	call   L_3972
	add    $0x6,%sp
	lea    -0x6ca(%bp),%ax
	mov    %ax,-0x6ce(%bp)
	mov    %ax,%bx
	movb   $0x0,(%bx)
	lea    -0x31(%bp),%ax
	push   %ax
	mov    $0x58,%ax
	push   %ax
	lea    -0x94a(%bp),%ax
	push   %ax
	mov    $0x44,%ax
	push   %ax
	call   L_1076
	add    $0x8,%sp
	/* TODO: data ref: mov    $0x6e89,%ax */
	mov    $D_6e89,%ax
	push   %ax
	lea    -0x31(%bp),%ax
	push   %ax
	call   L_2776
	add    $0x4,%sp
	mov    %ax,-0x94c(%bp)
	or     %ax,%ax
	jne   L_024b
	/* TODO: data ref: mov    $0x6e9a,%ax */
	mov    $D_6e9a,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6e8b,%ax */
	mov    $D_6e8b,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	lea    -0x31(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6ea7,%ax */
	mov    $D_6ea7,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_024b:
	mov    $0x1b6,%ax
	push   %ax
	lea    -0x31(%bp),%ax
	push   %ax
	call   L_23d4
	add    $0x4,%sp
	lea    -0x4f(%bp),%ax
	push   %ax
	mov    $0x54,%ax
	push   %ax
	lea    -0x94a(%bp),%ax
	push   %ax
	mov    $0x44,%ax
	push   %ax
	call   L_1076
	add    $0x8,%sp
	/* TODO: data ref: mov    $0x6eb5,%ax */
	mov    $D_6eb5,%ax
	push   %ax
	lea    -0x4f(%bp),%ax
	push   %ax
	call   L_2776
	add    $0x4,%sp
	mov    %ax,-0x94e(%bp)
	or     %ax,%ax
	jne   L_02b2
	/* TODO: data ref: mov    $0x6ec6,%ax */
	mov    $D_6ec6,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6eb7,%ax */
	mov    $D_6eb7,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	lea    -0x4f(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6ed2,%ax */
	mov    $D_6ed2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_02b2:
	mov    $0x1b6,%ax
	push   %ax
	lea    -0x4f(%bp),%ax
	push   %ax
	call   L_23d4
	add    $0x4,%sp
	lea    -0x94a(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x7a9a,%ax */
	mov    $D_7a9a,%ax
	push   %ax
	mov    $0x55,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6ee0,%ax */
	mov    $D_6ee0,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0xa,%sp
	lea    -0x462(%bp),%ax
	mov    %ax,-0x6cc(%bp)
L_02e3:
	lea    -0x8ce(%bp),%ax
	push   %ax
	pushw  -0x6cc(%bp)
	call   L_200e
	add    $0x4,%sp
	mov    %ax,-0x6cc(%bp)
	or     %ax,%ax
	jne   L_02fd
	jmp   L_0391
L_02fd:
	cmpb   $0x3e,-0x8ce(%bp)
	je   L_030b
	cmpb   $0x3c,-0x8ce(%bp)
	jne   L_0320
L_030b:
	lea    -0x8ce(%bp),%ax
	push   %ax
	pushw  -0x6cc(%bp)
	call   L_200e
	add    $0x4,%sp
	mov    %ax,-0x6cc(%bp)
L_031e:
	jmp   L_02e3
L_0320:
	cmpb   $0x3b,-0x8ce(%bp)
	jne   L_0365
	lea    -0x8ce(%bp),%ax
	mov    %ax,-0x95a(%bp)
L_032f:
	mov    -0x95a(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0351
	mov    -0x95a(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x6ce(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x95a(%bp)
	incw   -0x6ce(%bp)
	jmp   L_032f
L_0351:
	mov    -0x6ce(%bp),%bx
	movb   $0x20,(%bx)
	incw   -0x6ce(%bp)
	mov    -0x6ce(%bp),%bx
	movb   $0x0,(%bx)
	jmp   L_031e
L_0365:
	lea    -0x93a(%bp),%ax
	push   %ax
	lea    -0x942(%bp),%ax
	push   %ax
	lea    -0x8ce(%bp),%ax
	push   %ax
	call   L_212d
	add    $0x6,%sp
	cmpb   $0x0,-0x942(%bp)
	jne   L_0391
	lea    -0x94a(%bp),%ax
	push   %ax
	lea    -0x942(%bp),%ax
	push   %ax
	call   L_4c4a
	add    $0x4,%sp
L_0391:
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_03ab
	lea    -0x942(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6eea,%ax */
	mov    $D_6eea,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_03ab:
	lea    -0x942(%bp),%ax
	push   %ax
	call   L_21f4
	pop    %cx
	or     %ax,%ax
	je   L_03f3
	lea    -0x942(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6ef3,%ax */
	mov    $D_6ef3,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	pushw  -0x94c(%bp)
	call   L_4887
	pop    %cx
	pushw  -0x94e(%bp)
	call   L_4887
	pop    %cx
	lea    -0x31(%bp),%ax
	push   %ax
	call   L_4d0e
	pop    %cx
	lea    -0x4f(%bp),%ax
	push   %ax
	call   L_4d0e
	pop    %cx
	mov    $0x65,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_03f3:
	cmpw   $0x0,-0x464(%bp)
	jne   L_03fd
	jmp   L_04f5
L_03fd:
	lea    -0x22(%bp),%ax
	push   %ax
	mov    $0x42,%ax
	push   %ax
	lea    -0x942(%bp),%ax
	push   %ax
	mov    $0x44,%ax
	push   %ax
	call   L_1076
	add    $0x8,%sp
	/* TODO: data ref: mov    $0x6f08,%ax */
	mov    $D_6f08,%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	call   L_2776
	add    $0x4,%sp
	mov    %ax,-0x950(%bp)
	or     %ax,%ax
	jne   L_0456
	/* TODO: data ref: mov    $0x6f19,%ax */
	mov    $D_6f19,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6f0a,%ax */
	mov    $D_6f0a,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	lea    -0x22(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6f25,%ax */
	mov    $D_6f25,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_0456:
	mov    $0x1b6,%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	call   L_23d4
	add    $0x4,%sp
L_0464:
	/* TODO: data ref: mov    $0x79e8,%ax */
	mov    $D_79e8,%ax
	push   %ax
	mov    $0x200,%ax
	push   %ax
	lea    -0x262(%bp),%ax
	push   %ax
	call   L_26de
	add    $0x6,%sp
	or     %ax,%ax
	je   L_048c
	pushw  -0x950(%bp)
	lea    -0x262(%bp),%ax
	push   %ax
	call   L_23ea
	add    $0x4,%sp
	jmp   L_0464
L_048c:
	pushw  -0x950(%bp)
	call   L_4887
	pop    %cx
	lea    -0x942(%bp),%ax
	push   %ax
	lea    -0x94a(%bp),%ax
	push   %ax
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	je   L_04c9
	lea    -0x22(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x7a9a,%ax */
	mov    $D_7a9a,%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6f33,%ax */
	mov    $D_6f33,%ax
	push   %ax
	pushw  -0x94e(%bp)
	call   L_24d6
	add    $0xc,%sp
	incw   -0x60(%bp)
L_04c9:
	lea    -0x22(%bp),%ax
	push   %ax
	mov    $0x46,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6f49,%ax */
	mov    $D_6f49,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0x8,%sp
	lea    -0x22(%bp),%ax
	push   %ax
	mov    $0x49,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6f50,%ax */
	mov    $D_6f50,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0x8,%sp
L_04f5:
	lea    -0x462(%bp),%ax
	mov    %ax,-0x6cc(%bp)
L_04fd:
	lea    -0x8ce(%bp),%ax
	push   %ax
	pushw  -0x6cc(%bp)
	call   L_200e
	add    $0x4,%sp
	mov    %ax,-0x6cc(%bp)
	or     %ax,%ax
	jne   L_0517
	jmp   L_0c3e
L_0517:
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_0531
	lea    -0x8ce(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6f57,%ax */
	mov    $D_6f57,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_0531:
	cmpb   $0x3e,-0x8ce(%bp)
	je   L_053f
	cmpb   $0x3c,-0x8ce(%bp)
	jne   L_0549
L_053f:
	mov    -0x8ce(%bp),%al
	mov    %al,-0x957(%bp)
L_0547:
	jmp   L_04fd
L_0549:
	cmpb   $0x7c,-0x8ce(%bp)
	je   L_0557
	cmpb   $0x5e,-0x8ce(%bp)
	jne   L_059f
L_0557:
	lea    -0x6ca(%bp),%ax
	cmp    %ax,-0x6ce(%bp)
	je   L_04fd
	lea    -0x8ce(%bp),%ax
	mov    %ax,-0x95a(%bp)
L_0569:
	mov    -0x95a(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_058b
	mov    -0x95a(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x6ce(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x95a(%bp)
	incw   -0x6ce(%bp)
	jmp   L_0569
L_058b:
	mov    -0x6ce(%bp),%bx
	movb   $0x20,(%bx)
	incw   -0x6ce(%bp)
	mov    -0x6ce(%bp),%bx
	movb   $0x0,(%bx)
L_059d:
	jmp   L_0547
L_059f:
	lea    -0x93a(%bp),%ax
	push   %ax
	lea    -0x8d6(%bp),%ax
	push   %ax
	lea    -0x8ce(%bp),%ax
	push   %ax
	call   L_212d
	add    $0x6,%sp
	mov    %ax,-0x956(%bp)
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_05d2
	lea    -0x8d6(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6f61,%ax */
	mov    $D_6f61,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_05d2:
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_05ec
	lea    -0x93a(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6f6a,%ax */
	mov    $D_6f6a,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_05ec:
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_0605
	pushw  -0x956(%bp)
	/* TODO: data ref: mov    $0x6f73,%ax */
	mov    $D_6f73,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_0605:
	cmpb   $0x0,-0x8d6(%bp)
	jne   L_061c
	lea    -0x94a(%bp),%ax
	push   %ax
	lea    -0x8d6(%bp),%ax
	push   %ax
	call   L_4c4a
	add    $0x4,%sp
L_061c:
	lea    -0x6ca(%bp),%ax
	cmp    %ax,-0x6ce(%bp)
	jne   L_066c
	cmpb   $0x0,-0x957(%bp)
	jne   L_066c
	lea    -0x93a(%bp),%ax
	mov    %ax,-0x95a(%bp)
L_0635:
	mov    -0x95a(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0657
	mov    -0x95a(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x6ce(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x95a(%bp)
	incw   -0x6ce(%bp)
	jmp   L_0635
L_0657:
	mov    -0x6ce(%bp),%bx
	movb   $0x20,(%bx)
	incw   -0x6ce(%bp)
	mov    -0x6ce(%bp),%bx
	movb   $0x0,(%bx)
L_0669:
	jmp   L_059d
L_066c:
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_0686
	lea    -0x8d6(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6f7d,%ax */
	mov    $D_6f7d,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_0686:
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_06a0
	lea    -0x94a(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6f8a,%ax */
	mov    $D_6f8a,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_06a0:
	cmpb   $0x3e,-0x957(%bp)
	jne   L_06da
	cmpb   $0x7e,-0x93a(%bp)
	je   L_06b7
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_0ebc
	pop    %cx
L_06b7:
	lea    -0x8d6(%bp),%ax
	push   %ax
	lea    -0x93a(%bp),%ax
	push   %ax
	mov    $0x4f,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6f96,%ax */
	mov    $D_6f96,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0xa,%sp
	movb   $0x0,-0x957(%bp)
L_06d8:
	jmp   L_0669
L_06da:
	cmpw   $0x0,-0x956(%bp)
	jne   L_071f
	lea    -0x93a(%bp),%ax
	mov    %ax,-0x95a(%bp)
L_06e9:
	mov    -0x95a(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_070b
	mov    -0x95a(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x6ce(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x95a(%bp)
	incw   -0x6ce(%bp)
	jmp   L_06e9
L_070b:
	mov    -0x6ce(%bp),%bx
	movb   $0x20,(%bx)
	incw   -0x6ce(%bp)
	mov    -0x6ce(%bp),%bx
	movb   $0x0,(%bx)
L_071d:
	jmp   L_06d8
L_071f:
	lea    -0x94a(%bp),%ax
	push   %ax
	lea    -0x942(%bp),%ax
	push   %ax
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0745
	lea    -0x8d6(%bp),%ax
	push   %ax
	lea    -0x942(%bp),%ax
	push   %ax
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
L_0745:
	jne   L_07b4
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_0ebc
	pop    %cx
	cmpb   $0x3c,-0x957(%bp)
	jne   L_0770
	lea    -0x93a(%bp),%ax
	push   %ax
	mov    $0x49,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6fa0,%ax */
	mov    $D_6fa0,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0x8,%sp
	jmp   L_07ac
L_0770:
	lea    -0x93a(%bp),%ax
	mov    %ax,-0x95a(%bp)
L_0778:
	mov    -0x95a(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_079a
	mov    -0x95a(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x6ce(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x95a(%bp)
	incw   -0x6ce(%bp)
	jmp   L_0778
L_079a:
	mov    -0x6ce(%bp),%bx
	movb   $0x20,(%bx)
	incw   -0x6ce(%bp)
	mov    -0x6ce(%bp),%bx
	movb   $0x0,(%bx)
L_07ac:
	movb   $0x0,-0x957(%bp)
L_07b1:
	jmp   L_071d
L_07b4:
	lea    -0x94a(%bp),%ax
	push   %ax
	lea    -0x8d6(%bp),%ax
	push   %ax
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	je   L_07cb
	jmp   L_0901
L_07cb:
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_0ebc
	pop    %cx
	lea    -0x22(%bp),%ax
	push   %ax
	mov    $0x41,%ax
	push   %ax
	lea    -0x942(%bp),%ax
	push   %ax
	mov    $0x44,%ax
	push   %ax
	call   L_1076
	add    $0x8,%sp
	cmpb   $0x3c,-0x957(%bp)
	jne   L_0808
	lea    -0x22(%bp),%ax
	push   %ax
	mov    $0x49,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6fa7,%ax */
	mov    $D_6fa7,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0x8,%sp
L_0808:
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_0822
	lea    -0x93a(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6fae,%ax */
	mov    $D_6fae,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_0822:
	lea    -0x93a(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6fb7,%ax */
	mov    $D_6fb7,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7a9a,%ax */
	mov    $D_7a9a,%ax
	push   %ax
	call   L_1ad6
	add    $0x6,%sp
	or     %ax,%ax
	je   L_0854
	lea    -0x93a(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6fb8,%ax */
	mov    $D_6fb8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_0854:
	lea    -0x22(%bp),%ax
	push   %ax
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_0dd6
	add    $0x4,%sp
	or     %ax,%ax
	je   L_086f
	mov    $0x1,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_086f:
	lea    -0x22(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x7a9a,%ax */
	mov    $D_7a9a,%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	lea    -0x93a(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x6fce,%ax */
	mov    $D_6fce,%ax
	push   %ax
	pushw  -0x94e(%bp)
	call   L_24d6
	add    $0xc,%sp
	incw   -0x60(%bp)
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_12c0
	pop    %cx
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	mov    $0x46,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6fe4,%ax */
	mov    $D_6fe4,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0xa,%sp
	cmpb   $0x3c,-0x957(%bp)
	je   L_08f9
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_12c0
	pop    %cx
	mov    %ax,-0x95a(%bp)
L_08c5:
	mov    -0x95a(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_08e7
	mov    -0x95a(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x6ce(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x95a(%bp)
	incw   -0x6ce(%bp)
	jmp   L_08c5
L_08e7:
	mov    -0x6ce(%bp),%bx
	movb   $0x20,(%bx)
	incw   -0x6ce(%bp)
	mov    -0x6ce(%bp),%bx
	movb   $0x0,(%bx)
L_08f9:
	movb   $0x0,-0x957(%bp)
L_08fe:
	jmp   L_07b1
L_0901:
	lea    -0x942(%bp),%ax
	push   %ax
	lea    -0x94a(%bp),%ax
	push   %ax
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	je   L_0918
	jmp   L_0a58
L_0918:
	lea    -0x40(%bp),%ax
	push   %ax
	mov    $0x52,%ax
	push   %ax
	lea    -0x8d6(%bp),%ax
	push   %ax
	mov    $0x43,%ax
	push   %ax
	call   L_1076
	add    $0x8,%sp
	lea    -0x40(%bp),%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	call   L_4c4a
	add    $0x4,%sp
	movb   $0x44,-0x22(%bp)
	/* TODO: data ref: mov    $0x6fee,%ax */
	mov    $D_6fee,%ax
	push   %ax
	lea    -0x40(%bp),%ax
	push   %ax
	call   L_2776
	add    $0x4,%sp
	mov    %ax,-0x952(%bp)
	or     %ax,%ax
	jne   L_0983
	/* TODO: data ref: mov    $0x6fff,%ax */
	mov    $D_6fff,%ax
	push   %ax
	/* TODO: data ref: mov    $0x6ff0,%ax */
	mov    $D_6ff0,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	lea    -0x40(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x700a,%ax */
	mov    $D_700a,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_0983:
	mov    $0x1b6,%ax
	push   %ax
	lea    -0x40(%bp),%ax
	push   %ax
	call   L_23d4
	add    $0x4,%sp
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_0ebc
	pop    %cx
	/* TODO: data ref: mov    $0x7a9a,%ax */
	mov    $D_7a9a,%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	lea    -0x93a(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x7018,%ax */
	mov    $D_7018,%ax
	push   %ax
	pushw  -0x952(%bp)
	call   L_24d6
	add    $0xa,%sp
	pushw  -0x952(%bp)
	call   L_4887
	pop    %cx
	incw   -0x62(%bp)
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_12c0
	pop    %cx
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	mov    $0x46,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7027,%ax */
	mov    $D_7027,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0xa,%sp
	cmpb   $0x7e,-0x93a(%bp)
	je   L_09f0
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_0ebc
	pop    %cx
L_09f0:
	cmpb   $0x3c,-0x957(%bp)
	jne   L_0a0f
	lea    -0x22(%bp),%ax
	push   %ax
	mov    $0x49,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7031,%ax */
	mov    $D_7031,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0x8,%sp
	jmp   L_0a50
L_0a0f:
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_12c0
	pop    %cx
	mov    %ax,-0x95a(%bp)
L_0a1c:
	mov    -0x95a(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0a3e
	mov    -0x95a(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x6ce(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x95a(%bp)
	incw   -0x6ce(%bp)
	jmp   L_0a1c
L_0a3e:
	mov    -0x6ce(%bp),%bx
	movb   $0x20,(%bx)
	incw   -0x6ce(%bp)
	mov    -0x6ce(%bp),%bx
	movb   $0x0,(%bx)
L_0a50:
	movb   $0x0,-0x957(%bp)
L_0a55:
	jmp   L_08fe
L_0a58:
	lea    -0x942(%bp),%ax
	push   %ax
	lea    -0x8d6(%bp),%ax
	push   %ax
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0a6f
	jmp   L_0bcb
L_0a6f:
	lea    -0x22(%bp),%ax
	push   %ax
	mov    $0x52,%ax
	push   %ax
	lea    -0x8d6(%bp),%ax
	push   %ax
	mov    $0x44,%ax
	push   %ax
	call   L_1076
	add    $0x8,%sp
	lea    -0x22(%bp),%ax
	push   %ax
	lea    -0x40(%bp),%ax
	push   %ax
	call   L_4c4a
	add    $0x4,%sp
	movb   $0x43,-0x40(%bp)
	/* TODO: data ref: mov    $0x7038,%ax */
	mov    $D_7038,%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	call   L_2776
	add    $0x4,%sp
	mov    %ax,-0x950(%bp)
	or     %ax,%ax
	jne   L_0ada
	/* TODO: data ref: mov    $0x7049,%ax */
	mov    $D_7049,%ax
	push   %ax
	/* TODO: data ref: mov    $0x703a,%ax */
	mov    $D_703a,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	lea    -0x22(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x7055,%ax */
	mov    $D_7055,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_0ada:
	mov    $0x1b6,%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	call   L_23d4
	add    $0x4,%sp
	lea    -0x5e(%bp),%ax
	push   %ax
	mov    $0x54,%ax
	push   %ax
	lea    -0x942(%bp),%ax
	push   %ax
	mov    $0x44,%ax
	push   %ax
	call   L_1076
	add    $0x8,%sp
	/* TODO: data ref: mov    $0x7a9a,%ax */
	mov    $D_7a9a,%ax
	push   %ax
	lea    -0x5e(%bp),%ax
	push   %ax
	lea    -0x93a(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x7063,%ax */
	mov    $D_7063,%ax
	push   %ax
	pushw  -0x950(%bp)
	call   L_24d6
	add    $0xa,%sp
	pushw  -0x950(%bp)
	call   L_4887
	pop    %cx
	lea    -0x22(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x7a9a,%ax */
	mov    $D_7a9a,%ax
	push   %ax
	lea    -0x40(%bp),%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x7072,%ax */
	mov    $D_7072,%ax
	push   %ax
	pushw  -0x94e(%bp)
	call   L_24d6
	add    $0xc,%sp
	incw   -0x60(%bp)
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_12c0
	pop    %cx
	push   %ax
	lea    -0x5e(%bp),%ax
	push   %ax
	mov    $0x46,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7088,%ax */
	mov    $D_7088,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0xa,%sp
	cmpb   $0x3c,-0x957(%bp)
	jne   L_0b82
	lea    -0x5e(%bp),%ax
	push   %ax
	mov    $0x49,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7092,%ax */
	mov    $D_7092,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0x8,%sp
	jmp   L_0bc3
L_0b82:
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_12c0
	pop    %cx
	mov    %ax,-0x95a(%bp)
L_0b8f:
	mov    -0x95a(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0bb1
	mov    -0x95a(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x6ce(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x95a(%bp)
	incw   -0x6ce(%bp)
	jmp   L_0b8f
L_0bb1:
	mov    -0x6ce(%bp),%bx
	movb   $0x20,(%bx)
	incw   -0x6ce(%bp)
	mov    -0x6ce(%bp),%bx
	movb   $0x0,(%bx)
L_0bc3:
	movb   $0x0,-0x957(%bp)
L_0bc8:
	jmp   L_0a55
L_0bcb:
	cmpb   $0x7e,-0x93a(%bp)
	je   L_0bdb
	lea    -0x93a(%bp),%ax
	push   %ax
	call   L_0ebc
	pop    %cx
L_0bdb:
	cmpb   $0x3c,-0x957(%bp)
	jne   L_0bfb
	lea    -0x93a(%bp),%ax
	push   %ax
	mov    $0x49,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7099,%ax */
	mov    $D_7099,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0x8,%sp
	jmp   L_0c37
L_0bfb:
	lea    -0x93a(%bp),%ax
	mov    %ax,-0x95a(%bp)
L_0c03:
	mov    -0x95a(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0c25
	mov    -0x95a(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x6ce(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x95a(%bp)
	incw   -0x6ce(%bp)
	jmp   L_0c03
L_0c25:
	mov    -0x6ce(%bp),%bx
	movb   $0x20,(%bx)
	incw   -0x6ce(%bp)
	mov    -0x6ce(%bp),%bx
	movb   $0x0,(%bx)
L_0c37:
	movb   $0x0,-0x957(%bp)
	jmp   L_0bc8
L_0c3e:
	lea    -0x6ca(%bp),%ax
	push   %ax
	mov    $0x43,%ax
	push   %ax
	/* TODO: data ref: mov    $0x70a0,%ax */
	mov    $D_70a0,%ax
	push   %ax
	pushw  -0x94c(%bp)
	call   L_24d6
	add    $0x8,%sp
	pushw  -0x94c(%bp)
	call   L_4887
	pop    %cx
	lea    -0x31(%bp),%ax
	push   %ax
	lea    -0x40(%bp),%ax
	push   %ax
	call   L_4c4a
	add    $0x4,%sp
	movb   $0x58,-0x40(%bp)
	lea    -0x94a(%bp),%ax
	push   %ax
	lea    -0x942(%bp),%ax
	push   %ax
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_0cb5
	lea    -0x40(%bp),%ax
	push   %ax
	lea    -0x31(%bp),%ax
	push   %ax
	call   L_2750
	add    $0x4,%sp
	lea    -0x31(%bp),%ax
	push   %ax
	call   L_4d0e
	pop    %cx
	cmpw   $0x0,-0x466(%bp)
	je   L_0cd6
	cmpw   $0x0,-0x62(%bp)
	je   L_0cb0
	/* TODO: data ref: mov    $0x70a7,%ax */
	mov    $D_70a7,%ax
	push   %ax
	call   L_188e
	pop    %cx
	jmp   L_0cd6
L_0cb0:
	call   L_195f
	jmp   L_0cd6
L_0cb5:
	lea    -0x31(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x7a9a,%ax */
	mov    $D_7a9a,%ax
	push   %ax
	lea    -0x40(%bp),%ax
	push   %ax
	lea    -0x31(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x70a8,%ax */
	mov    $D_70a8,%ax
	push   %ax
	pushw  -0x94e(%bp)
	call   L_24d6
	add    $0xc,%sp
	incw   -0x60(%bp)
L_0cd6:
	pushw  -0x94e(%bp)
	call   L_4887
	pop    %cx
	cmpw   $0x0,-0x60(%bp)
	je   L_0d2a
	lea    -0x13(%bp),%ax
	push   %ax
	mov    $0x41,%ax
	push   %ax
	lea    -0x942(%bp),%ax
	push   %ax
	mov    $0x43,%ax
	push   %ax
	call   L_1076
	add    $0x8,%sp
	lea    -0x13(%bp),%ax
	push   %ax
	lea    -0x4f(%bp),%ax
	push   %ax
	call   L_2750
	add    $0x4,%sp
	lea    -0x4f(%bp),%ax
	push   %ax
	call   L_4d0e
	pop    %cx
	cmpw   $0x0,-0x466(%bp)
	je   L_0d21
	lea    -0x942(%bp),%ax
	push   %ax
	call   L_188e
	pop    %cx
L_0d21:
	xor    %ax,%ax
	push   %ax
	call   L_0d39
	pop    %cx
	jmp   L_0d32
L_0d2a:
	lea    -0x4f(%bp),%ax
	push   %ax
	call   L_4d0e
	pop    %cx
L_0d32:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0d39:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %ax,%ax
	push   %ax
	call   L_1690
	pop    %cx
	/* TODO: data ref: cmpw   $0x1,0x6d96 */
	cmpw   $0x1,D_6d96
	jl   L_0d5d
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x70be,%ax */
	mov    $D_70be,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_0d5d:
	pushw  0x4(%bp)
	call   exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0d68:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	/* TODO: data ref: mov    $0x70d0,%ax */
	mov    $D_70d0,%ax
	push   %ax
	/* TODO: data ref: mov    $0x70cc,%ax */
	mov    $D_70cc,%ax
	push   %ax
	call   L_288a
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_0d8a
	mov    $0xffff,%ax
	jmp   L_0dce
L_0d8a:
	pushw  -0x6(%bp)
	mov    $0x64,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_26de
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_0daa
	pushw  -0x6(%bp)
	call   L_293c
	pop    %cx
	mov    $0xffff,%ax
	jmp   L_0dce
L_0daa:
	pushw  0x4(%bp)
	call   L_3760
	pop    %cx
	add    0x4(%bp),%ax
	dec    %ax
	mov    %ax,-0x8(%bp)
	mov    %ax,%bx
	cmpb   $0xa,(%bx)
	jne   L_0dc5
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
L_0dc5:
	pushw  -0x6(%bp)
	call   L_293c
	pop    %cx
	xor    %ax,%ax
L_0dce:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_0dd6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x206,%sp
	/* TODO: data ref: mov    $0x70d2,%ax */
	mov    $D_70d2,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2776
	add    $0x4,%sp
	mov    %ax,-0x208(%bp)
	or     %ax,%ax
	jne   L_0df9
	mov    $0xffff,%ax
	jmp   L_0e19
L_0df9:
	/* TODO: data ref: mov    $0x70d4,%ax */
	mov    $D_70d4,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_2776
	add    $0x4,%sp
	mov    %ax,-0x20a(%bp)
	or     %ax,%ax
	jne   L_0e1b
	pushw  -0x208(%bp)
	call   L_4887
	pop    %cx
	mov    $0xffff,%ax
L_0e19:
	jmp   L_0e72
L_0e1b:
	pushw  -0x208(%bp)
	mov    $0x200,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	lea    -0x204(%bp),%ax
	push   %ax
	call   L_2aa0
	add    $0x8,%sp
	mov    %ax,-0x206(%bp)
	or     %ax,%ax
	jle   L_0e53
	pushw  -0x20a(%bp)
	pushw  -0x206(%bp)
	mov    $0x1,%ax
	push   %ax
	lea    -0x204(%bp),%ax
	push   %ax
	call   L_2b0b
	add    $0x8,%sp
	jmp   L_0e1b
L_0e53:
	pushw  -0x208(%bp)
	call   L_4887
	pop    %cx
	pushw  -0x20a(%bp)
	call   L_4887
	pop    %cx
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_23d4
	add    $0x4,%sp
	xor    %ax,%ax
L_0e72:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_2750
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_0eae
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_0dd6
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_0ea9
	pushw  0x4(%bp)
	call   L_4d0e
	pop    %cx
L_0ea9:
	mov    -0x6(%bp),%ax
	jmp   L_0eb7
L_0eae:
	pushw  0x4(%bp)
	call   L_4d0e
	pop    %cx
	xor    %ax,%ax
L_0eb7:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0ebc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x7e,%sp
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	cbtw
	jmp   L_0f7e
L_0ecd:
	mov    0x4(%bp),%ax
	inc    %ax
	mov    %ax,-0x6(%bp)
	lea    -0x1a(%bp),%ax
	mov    %ax,-0x1c(%bp)
L_0eda:
	mov    -0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0eff
	mov    -0x6(%bp),%bx
	cmpb   $0x2f,(%bx)
	je   L_0eff
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x1c(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x1c(%bp)
	incw   -0x6(%bp)
	jmp   L_0eda
L_0eff:
	mov    -0x1c(%bp),%bx
	movb   $0x0,(%bx)
	lea    -0x80(%bp),%ax
	push   %ax
	lea    -0x82(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	call   L_124d
	add    $0x6,%sp
	or     %ax,%ax
	je   L_0f2a
	/* TODO: data ref: pushw  0x6d94 */
	pushw  D_6d94
	lea    -0x80(%bp),%ax
	push   %ax
	call   L_4c4a
	add    $0x4,%sp
L_0f2a:
	pushw  -0x6(%bp)
	lea    -0x80(%bp),%ax
	push   %ax
	call   L_4c16
	add    $0x4,%sp
	lea    -0x80(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c4a
	add    $0x4,%sp
	jmp   L_0f8d
L_0f46:
	/* TODO: data ref: mov    $0x7aca,%ax */
	mov    $D_7aca,%ax
	push   %ax
	lea    -0x80(%bp),%ax
	push   %ax
	call   L_4c4a
	add    $0x4,%sp
	/* TODO: data ref: mov    $0x70d6,%ax */
	mov    $D_70d6,%ax
	push   %ax
	lea    -0x80(%bp),%ax
	push   %ax
	call   L_4c16
	add    $0x4,%sp
	pushw  0x4(%bp)
	lea    -0x80(%bp),%ax
	push   %ax
	call   L_4c16
	add    $0x4,%sp
	lea    -0x80(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c4a
	add    $0x4,%sp
	jmp   L_0f8d
L_0f7e:
	cmp    $0x2f,%ax
	je   L_0f8d
	cmp    $0x7e,%ax
	jne   L_0f8b
	jmp   L_0ecd
L_0f8b:
	jmp   L_0f46
L_0f8d:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0f94:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x20,%sp
	lea    -0x24(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c00
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	cmp    $0x0,%ax
	jge   L_0fb5
	xor    %ax,%ax
	jmp   L_0fc7
L_0fb5:
	mov    -0x20(%bp),%ax
	and    $0xe000,%ax
	cmp    $0xc000,%ax
	jne   L_0fc5
	mov    $0x1,%ax
	jmp   L_0fc7
L_0fc5:
	xor    %ax,%ax
L_0fc7:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0fce:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xcc,%sp
	lea    -0xcd(%bp),%ax
	mov    %ax,-0xd0(%bp)
L_0fdf:
	pushw  0x4(%bp)
	lea    -0xce(%bp),%ax
	push   %ax
	call   L_4c4a
	add    $0x4,%sp
	mov    $0x2f,%ax
	push   %ax
	pushw  -0xd0(%bp)
	call   L_128e
	add    $0x4,%sp
	mov    %ax,-0xd0(%bp)
	or     %ax,%ax
	jne   L_1007
	xor    %ax,%ax
	jmp   L_1066
L_1007:
	mov    -0xd0(%bp),%bx
	movb   $0x0,(%bx)
	lea    -0xce(%bp),%ax
	push   %ax
	call   L_0f94
	pop    %cx
	or     %ax,%ax
	jne   L_1068
	lea    -0xce(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x70d8,%ax */
	mov    $D_70d8,%ax
	push   %ax
	lea    -0x6a(%bp),%ax
	push   %ax
	call   L_3972
	add    $0x6,%sp
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_1048
	lea    -0xce(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x70e1,%ax */
	mov    $D_70e1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_1048:
	/* TODO: data ref: mov    $0x7a9a,%ax */
	mov    $D_7a9a,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	lea    -0x6a(%bp),%ax
	push   %ax
	call   L_133a
	add    $0x8,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_1068
	mov    $0xffff,%ax
L_1066:
	jmp   L_106f
L_1068:
	incw   -0xd0(%bp)
	jmp   L_0fdf
L_106f:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1076:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0x9(%bp),%ax
	push   %ax
	call   L_10c3
	pop    %cx
	lea    -0x9(%bp),%ax
	push   %ax
	mov    0x8(%bp),%al
	cbtw
	push   %ax
	pushw  0x6(%bp)
	mov    0x4(%bp),%al
	cbtw
	push   %ax
	/* TODO: data ref: mov    $0x70ee,%ax */
	mov    $D_70ee,%ax
	push   %ax
	pushw  0xa(%bp)
	call   L_3972
	add    $0xc,%sp
	/* TODO: data ref: cmpw   $0x4,0x6d96 */
	cmpw   $0x4,D_6d96
	jl   L_10bc
	pushw  0xa(%bp)
	/* TODO: data ref: mov    $0x70fc,%ax */
	mov    $D_70fc,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_10bc:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_10c3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	movw   $0x0,-0x8(%bp)
L_10d0:
	cmpw   $0x5,-0x8(%bp)
	jge   L_10f8
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	/* TODO: data ref: mov    $0x7107,%ax */
	mov    $D_7107,%ax
	push   %ax
	call   L_148e
	add    $0x6,%sp
	or     %ax,%ax
	je   L_10f8
	mov    $0x5,%ax
	push   %ax
	call   L_3622
	pop    %cx
	incw   -0x8(%bp)
	jmp   L_10d0
L_10f8:
	cmpw   $0x5,-0x8(%bp)
	jl   L_112a
	/* TODO: data ref: mov    $0x711f,%ax */
	mov    $D_711f,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7110,%ax */
	mov    $D_7110,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	/* TODO: data ref: mov    $0x713d,%ax */
	mov    $D_713d,%ax
	push   %ax
	/* TODO: data ref: mov    $0x712e,%ax */
	mov    $D_712e,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_112a:
	/* TODO: data ref: mov    $0x715b,%ax */
	mov    $D_715b,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7146,%ax */
	mov    $D_7146,%ax
	push   %ax
	call   L_2776
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_11a4
	lea    -0x8(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x715d,%ax */
	mov    $D_715d,%ax
	push   %ax
	pushw  -0x6(%bp)
	call   L_2bc6
	add    $0x6,%sp
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x7176,%ax */
	mov    $D_7176,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7161,%ax */
	mov    $D_7161,%ax
	push   %ax
	call   L_2436
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_1194
	/* TODO: data ref: mov    $0x7187,%ax */
	mov    $D_7187,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7178,%ax */
	mov    $D_7178,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	/* TODO: data ref: mov    $0x71a2,%ax */
	mov    $D_71a2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7192,%ax */
	mov    $D_7192,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_1194:
	mov    $0x1b6,%ax
	push   %ax
	/* TODO: data ref: mov    $0x71b7,%ax */
	mov    $D_71b7,%ax
	push   %ax
	call   L_23d4
	add    $0x4,%sp
	jmp   L_11d1
L_11a4:
	/* TODO: data ref: mov    $0x71e1,%ax */
	mov    $D_71e1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x71cc,%ax */
	mov    $D_71cc,%ax
	push   %ax
	call   L_2776
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_11be
	mov    $0xffff,%ax
	jmp   L_1203
L_11be:
	mov    $0x1b6,%ax
	push   %ax
	/* TODO: data ref: mov    $0x71e3,%ax */
	mov    $D_71e3,%ax
	push   %ax
	call   L_23d4
	add    $0x4,%sp
	movw   $0x0,-0x8(%bp)
L_11d1:
	incw   -0x8(%bp)
	pushw  -0x8(%bp)
	/* TODO: data ref: mov    $0x71fb,%ax */
	mov    $D_71fb,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_3972
	add    $0x6,%sp
	push   %ax
	/* TODO: data ref: mov    $0x71f8,%ax */
	mov    $D_71f8,%ax
	push   %ax
	pushw  -0x6(%bp)
	call   L_24d6
	add    $0x6,%sp
	pushw  -0x6(%bp)
	call   L_4887
	pop    %cx
	/* TODO: data ref: mov    $0x7200,%ax */
	mov    $D_7200,%ax
	push   %ax
	call   L_1690
	pop    %cx
	xor    %ax,%ax
L_1203:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_120a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	pushw  0x4(%bp)
	call   L_252c
	pop    %cx
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_1229
	mov    0x8(%bp),%bx
	movb   $0x0,(%bx)
	mov    $0xffff,%ax
	jmp   L_1248
L_1229:
	mov    -0x6(%bp),%bx
	pushw  0xe(%bx)
	pushw  0x8(%bp)
	call   L_4c4a
	add    $0x4,%sp
	mov    -0x6(%bp),%bx
	pushw  (%bx)
	pushw  0x6(%bp)
	call   L_4c4a
	add    $0x4,%sp
	xor    %ax,%ax
L_1248:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_124d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	pushw  0x4(%bp)
	call   L_2502
	pop    %cx
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_126c
	mov    0x8(%bp),%bx
	movb   $0x0,(%bx)
	mov    $0xffff,%ax
	jmp   L_1288
L_126c:
	mov    -0x6(%bp),%bx
	pushw  0xe(%bx)
	pushw  0x8(%bp)
	call   L_4c4a
	add    $0x4,%sp
	mov    -0x6(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x6(%bp),%bx
	mov    %ax,(%bx)
	xor    %ax,%ax
L_1288:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_128e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_1293:
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_12b9
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    0x6(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_12b4
	mov    0x4(%bp),%ax
	jmp   L_12bb
L_12b4:
	incw   0x4(%bp)
	jmp   L_1293
L_12b9:
	xor    %ax,%ax
L_12bb:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_12c0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	pushw  0x4(%bp)
	call   L_3760
	pop    %cx
	add    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
L_12d3:
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jb   L_12e6
	decw   -0x6(%bp)
	mov    -0x6(%bp),%bx
	cmpb   $0x2f,(%bx)
	jne   L_12d3
L_12e6:
	incw   -0x6(%bp)
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_12f2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_12f8:
	mov    0x4(%bp),%bx
	incw   0x4(%bp)
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	mov    %cl,%al
	cbtw
	mov    0x6(%bp),%bx
	incw   0x6(%bp)
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_1327
	cmpb   $0x0,-0x5(%bp)
	jne   L_12f8
	mov    $0x1,%ax
	jmp   L_1334
L_1327:
	cmpb   $0x0,-0x5(%bp)
	jne   L_1332
	mov    $0x1,%ax
	jmp   L_1334
L_1332:
	xor    %ax,%ax
L_1334:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_133a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6e,%sp
	cmpw   $0x0,0x6(%bp)
	jne   L_134d
	/* TODO: data ref: movw   $0x720a,0x6(%bp) */
	movw   $D_720a,0x6(%bp)
L_134d:
	cmpw   $0x0,0x8(%bp)
	jne   L_1358
	/* TODO: data ref: movw   $0x7214,0x8(%bp) */
	movw   $D_7214,0x8(%bp)
L_1358:
	/* TODO: data ref: cmpw   $0x3,0x6d96 */
	cmpw   $0x3,D_6d96
	jl   L_1370
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x721e,%ax */
	mov    $D_721e,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_1370:
	call   L_3806
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_137d
	jmp   L_146a
L_137d:
	/* TODO: data ref: pushw  0x7a94 */
	pushw  D_7a94
	call   L_4b94
	pop    %cx
	/* TODO: data ref: pushw  0x7aa4 */
	pushw  D_7aa4
	call   L_4b94
	pop    %cx
	xor    %ax,%ax
	push   %ax
	call   L_4b94
	pop    %cx
	xor    %ax,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_4bd0
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	je   L_13d2
	/* TODO: data ref: mov    $0x7238,%ax */
	mov    $D_7238,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7229,%ax */
	mov    $D_7229,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	pushw  -0x8(%bp)
	/* TODO: data ref: mov    $0x723f,%ax */
	mov    $D_723f,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_13d2:
	mov    $0x1,%ax
	push   %ax
	call   L_4b94
	pop    %cx
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x8(%bp)
	call   L_2874
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	cmp    $0x1,%ax
	je   L_141a
	/* TODO: data ref: mov    $0x7261,%ax */
	mov    $D_7261,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7252,%ax */
	mov    $D_7252,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	pushw  -0x8(%bp)
	/* TODO: data ref: mov    $0x7268,%ax */
	mov    $D_7268,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_141a:
	lea    -0x72(%bp),%ax
	push   %ax
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0xa(%bp)
	call   L_124d
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_1436
	pushw  -0xa(%bp)
	call   L_360e
	pop    %cx
L_1436:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x7286,%ax */
	mov    $D_7286,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7283,%ax */
	mov    $D_7283,%ax
	push   %ax
	/* TODO: data ref: mov    $0x727b,%ax */
	mov    $D_727b,%ax
	push   %ax
	call   L_37d4
	add    $0xa,%sp
	mov    $0x64,%ax
	push   %ax
	call   exit
	pop    %cx
L_1456:
	lea    -0x6(%bp),%ax
	push   %ax
	call   L_4d22
	pop    %cx
	mov    %ax,-0xe(%bp)
	cmp    -0xc(%bp),%ax
	je   L_146c
	cmpw   $0xffff,-0xe(%bp)
L_146a:
	jne   L_1456
L_146c:
	/* TODO: data ref: cmpw   $0x3,0x6d96 */
	cmpw   $0x3,D_6d96
	jl   L_1484
	pushw  -0x6(%bp)
	/* TODO: data ref: mov    $0x7289,%ax */
	mov    $D_7289,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
L_1484:
	mov    -0x6(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_148e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x24,%sp
	/* TODO: data ref: cmpw   $0x0,0x7294 */
	cmpw   $0x0,D_7294
	jge   L_14b5
	call   L_2766
	/* TODO: data ref: mov    %ax,0x7294 */
	mov    %ax,D_7294
	/* TODO: data ref: pushw  0x7294 */
	pushw  D_7294
	/* TODO: data ref: mov    $0x72a8,%ax */
	mov    $D_72a8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7296,%ax */
	mov    $D_7296,%ax
	push   %ax
	call   L_3972
	add    $0x6,%sp
L_14b5:
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x7296,%ax */
	mov    $D_7296,%ax
	push   %ax
	/* TODO: data ref: pushw  0x7294 */
	pushw  D_7294
	call   L_1752
	add    $0x6,%sp
	cmp    $0xffff,%ax
	je   L_14ce
	jmp   L_15b1
L_14ce:
	lea    -0x22(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c00
	add    $0x4,%sp
	mov    %ax,-0x28(%bp)
	cmp    $0xffff,%ax
	jne   L_150e
	/* TODO: data ref: mov    $0x72bf,%ax */
	mov    $D_72bf,%ax
	push   %ax
	/* TODO: data ref: mov    $0x72b0,%ax */
	mov    $D_72b0,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x72c9,%ax */
	mov    $D_72c9,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_150e:
	lea    -0x26(%bp),%ax
	push   %ax
	call   L_3954
	pop    %cx
	mov    -0x26(%bp),%ax
	mov    -0x24(%bp),%dx
	sub    -0x8(%bp),%ax
	sbb    -0x6(%bp),%dx
	cmp    0x8(%bp),%dx
	jg   L_1534
	jl   L_152e
	cmp    0x6(%bp),%ax
	jae   L_1534
L_152e:
	mov    $0xffff,%ax
	jmp   L_15ba
L_1534:
	pushw  0x4(%bp)
	call   L_4d0e
	pop    %cx
	mov    %ax,-0x28(%bp)
	cmp    $0xffff,%ax
	jne   L_156e
	/* TODO: data ref: mov    $0x72ea,%ax */
	mov    $D_72ea,%ax
	push   %ax
	/* TODO: data ref: mov    $0x72db,%ax */
	mov    $D_72db,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x72f4,%ax */
	mov    $D_72f4,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_156e:
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x7296,%ax */
	mov    $D_7296,%ax
	push   %ax
	/* TODO: data ref: pushw  0x7294 */
	pushw  D_7294
	call   L_1752
	add    $0x6,%sp
	mov    %ax,-0x28(%bp)
	or     %ax,%ax
	je   L_15b1
	/* TODO: data ref: mov    $0x7315,%ax */
	mov    $D_7315,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7306,%ax */
	mov    $D_7306,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x731e,%ax */
	mov    $D_731e,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_15b1:
	pushw  0x4(%bp)
	call   L_15c1
	pop    %cx
	xor    %ax,%ax
L_15ba:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_15c1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	movw   $0x0,-0x8(%bp)
L_15ce:
	mov    -0x8(%bp),%ax
	/* TODO: data ref: cmp    0x72a6,%ax */
	cmp    D_72a6,%ax
	jge   L_15e8
	mov    -0x8(%bp),%bx
	shl    %bx
	/* TODO: data ref: cmpw   $0x0,0x7b32(%bx) */
	cmpw   $0x0,D_7b32(%bx)
	je   L_15e8
	incw   -0x8(%bp)
	jmp   L_15ce
L_15e8:
	cmpw   $0xa,-0x8(%bp)
	jl   L_1619
	/* TODO: data ref: mov    $0x733f,%ax */
	mov    $D_733f,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7330,%ax */
	mov    $D_7330,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	pushw  -0x8(%bp)
	/* TODO: data ref: mov    $0x734c,%ax */
	mov    $D_734c,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_1619:
	mov    -0x8(%bp),%ax
	/* TODO: data ref: cmp    0x72a6,%ax */
	cmp    D_72a6,%ax
	jl   L_162c
	/* TODO: data ref: mov    0x72a6,%ax */
	mov    D_72a6,%ax
	mov    %ax,-0x8(%bp)
	/* TODO: data ref: incw   0x72a6 */
	incw   D_72a6
L_162c:
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_3760
	pop    %cx
	inc    %ax
	push   %ax
	call   L_236a
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_1671
	/* TODO: data ref: mov    $0x736d,%ax */
	mov    $D_736d,%ax
	push   %ax
	/* TODO: data ref: mov    $0x735e,%ax */
	mov    $D_735e,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x7377,%ax */
	mov    $D_7377,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_1671:
	pushw  0x4(%bp)
	pushw  -0x6(%bp)
	call   L_4c4a
	add    $0x4,%sp
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    -0x6(%bp),%ax
	/* TODO: data ref: mov    %ax,0x7b32(%bx) */
	mov    %ax,D_7b32(%bx)
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1690:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	movw   $0x0,-0x6(%bp)
L_169b:
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x72a6,%ax */
	cmp    D_72a6,%ax
	jge   L_16f6
	mov    -0x6(%bp),%bx
	shl    %bx
	/* TODO: data ref: cmpw   $0x0,0x7b32(%bx) */
	cmpw   $0x0,D_7b32(%bx)
	je   L_16f1
	cmpw   $0x0,0x4(%bp)
	je   L_16cc
	mov    -0x6(%bp),%bx
	shl    %bx
	/* TODO: data ref: pushw  0x7b32(%bx) */
	pushw  D_7b32(%bx)
	pushw  0x4(%bp)
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_16f1
L_16cc:
	mov    -0x6(%bp),%bx
	shl    %bx
	/* TODO: data ref: pushw  0x7b32(%bx) */
	pushw  D_7b32(%bx)
	call   L_4d0e
	pop    %cx
	mov    -0x6(%bp),%bx
	shl    %bx
	/* TODO: data ref: pushw  0x7b32(%bx) */
	pushw  D_7b32(%bx)
	call   L_4a48
	pop    %cx
	mov    -0x6(%bp),%bx
	shl    %bx
	/* TODO: data ref: movw   $0x0,0x7b32(%bx) */
	movw   $0x0,D_7b32(%bx)
L_16f1:
	incw   -0x6(%bp)
	jmp   L_169b
L_16f6:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_16fb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x1e,%sp
	lea    -0x22(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c00
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_1718
	xor    %ax,%ax
	jmp   L_172b
L_1718:
	cmpw   $0x0,-0x12(%bp)
	jne   L_1724
	cmpw   $0x2,-0x14(%bp)
	je   L_1728
L_1724:
	xor    %ax,%ax
	jmp   L_172b
L_1728:
	mov    $0x1,%ax
L_172b:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   L_16fb
	pop    %cx
	or     %ax,%ax
	je   L_174b
	pushw  0x4(%bp)
	call   L_4d0e
	pop    %cx
	jmp   L_174e
L_174b:
	mov    $0xffff,%ax
L_174e:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1752:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    $0x124,%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_2874
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	cmp    $0x0,%ax
	jge   L_1772
	mov    $0xffff,%ax
	jmp   L_17af
L_1772:
	mov    $0x2,%ax
	push   %ax
	lea    0x4(%bp),%ax
	push   %ax
	pushw  -0x6(%bp)
	call   L_4d3e
	add    $0x6,%sp
	pushw  -0x6(%bp)
	call   L_4b94
	pop    %cx
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	call   L_2750
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_17a6
	pushw  0x6(%bp)
	call   L_4d0e
	pop    %cx
	mov    $0xffff,%ax
	jmp   L_17af
L_17a6:
	pushw  0x6(%bp)
	call   L_4d0e
	pop    %cx
	xor    %ax,%ax
L_17af:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	movw   $0x0,-0x6(%bp)
L_17c1:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x8(%bp),%ax
	mov    %ax,%bx
	cmpw   $0x0,(%bx)
	je   L_1818
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x8(%bp),%ax
	mov    %ax,%bx
	pushw  (%bx)
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_1752
	add    $0x6,%sp
	or     %ax,%ax
	je   L_1813
	movw   $0x0,-0x8(%bp)
L_17f1:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	jge   L_180e
	mov    -0x8(%bp),%ax
	shl    %ax
	add    0x8(%bp),%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_4d0e
	pop    %cx
	incw   -0x8(%bp)
	jmp   L_17f1
L_180e:
	mov    $0xffff,%ax
	jmp   L_181a
L_1813:
	incw   -0x6(%bp)
	jmp   L_17c1
L_1818:
	xor    %ax,%ax
L_181a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x1e,%sp
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x7395,%ax */
	mov    $D_7395,%ax
	push   %ax
	/* TODO: data ref: mov    $0x738f,%ax */
	mov    $D_738f,%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	call   L_3972
	add    $0x8,%sp
	lea    -0x22(%bp),%ax
	push   %ax
	call   L_1690
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x1e,%sp
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x73a0,%ax */
	mov    $D_73a0,%ax
	push   %ax
	/* TODO: data ref: mov    $0x739a,%ax */
	mov    $D_739a,%ax
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	call   L_3972
	add    $0x8,%sp
	mov    $0x5180,%ax
	mov    $0x1,%dx
	push   %dx
	push   %ax
	lea    -0x22(%bp),%ax
	push   %ax
	call   L_148e
	add    $0x6,%sp
	or     %ax,%ax
	jge   L_1885
	mov    $0xffff,%ax
	jmp   L_1887
L_1885:
	xor    %ax,%ax
L_1887:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_188e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x64,%sp
	call   L_3806
	or     %ax,%ax
	je   L_18a0
	jmp   L_1958
L_18a0:
	xor    %ax,%ax
	push   %ax
	call   L_4b94
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	call   L_4b94
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	call   L_4b94
	pop    %cx
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x73a6,%ax */
	mov    $D_73a6,%ax
	push   %ax
	call   L_4bd0
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x73b0,%ax */
	mov    $D_73b0,%ax
	push   %ax
	call   L_4bd0
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x73ba,%ax */
	mov    $D_73ba,%ax
	push   %ax
	call   L_4bd0
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x9,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1933
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x73c4,%ax */
	mov    $D_73c4,%ax
	push   %ax
	lea    -0x68(%bp),%ax
	push   %ax
	call   L_3972
	add    $0x6,%sp
	jmp   L_1937
L_1933:
	movb   $0x0,-0x68(%bp)
L_1937:
	xor    %ax,%ax
	push   %ax
	lea    -0x68(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x73e7,%ax */
	mov    $D_73e7,%ax
	push   %ax
	/* TODO: data ref: mov    $0x73e0,%ax */
	mov    $D_73e0,%ax
	push   %ax
	/* TODO: data ref: mov    $0x73cb,%ax */
	mov    $D_73cb,%ax
	push   %ax
	call   L_37d4
	add    $0xa,%sp
	mov    $0x64,%ax
	push   %ax
	call   exit
	pop    %cx
L_1958:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_195f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_3806
	or     %ax,%ax
	je   L_196e
	jmp   L_1a00
L_196e:
	xor    %ax,%ax
	push   %ax
	call   L_4b94
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	call   L_4b94
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	call   L_4b94
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x73eb,%ax */
	mov    $D_73eb,%ax
	push   %ax
	call   L_4bd0
	add    $0x4,%sp
	mov    $0x2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x73f5,%ax */
	mov    $D_73f5,%ax
	push   %ax
	call   L_4bd0
	add    $0x4,%sp
	mov    $0x2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x73ff,%ax */
	mov    $D_73ff,%ax
	push   %ax
	call   L_4bd0
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x9,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x741d,%ax */
	mov    $D_741d,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7409,%ax */
	mov    $D_7409,%ax
	push   %ax
	call   L_37d4
	add    $0x6,%sp
	mov    $0x64,%ax
	push   %ax
	call   exit
	pop    %cx
L_1a00:
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x12c,%sp
	call   L_3806
	or     %ax,%ax
	je   L_1a17
	jmp   L_1ac7
L_1a17:
	xor    %ax,%ax
	push   %ax
	call   L_4b94
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	call   L_4b94
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	call   L_4b94
	pop    %cx
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7423,%ax */
	mov    $D_7423,%ax
	push   %ax
	call   L_4bd0
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x742d,%ax */
	mov    $D_742d,%ax
	push   %ax
	call   L_4bd0
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7437,%ax */
	mov    $D_7437,%ax
	push   %ax
	call   L_4bd0
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $0x9,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x744a,%ax */
	mov    $D_744a,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7441,%ax */
	mov    $D_7441,%ax
	push   %ax
	lea    -0x130(%bp),%ax
	push   %ax
	call   L_3972
	add    $0x8,%sp
	xor    %ax,%ax
	push   %ax
	lea    -0x130(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x7467,%ax */
	mov    $D_7467,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7464,%ax */
	mov    $D_7464,%ax
	push   %ax
	/* TODO: data ref: mov    $0x745c,%ax */
	mov    $D_745c,%ax
	push   %ax
	call   L_37d4
	add    $0xa,%sp
	mov    $0x64,%ax
	push   %ax
	call   exit
	pop    %cx
L_1ac7:
	mov    $0xf,%ax
	push   %ax
	call   L_3622
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ad6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	pushw  0x8(%bp)
	/* TODO: data ref: mov    $0x7472,%ax */
	mov    $D_7472,%ax
	push   %ax
	call   L_12f2
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1af5
	mov    $0xffff,%ax
	jmp   L_1bb6
L_1af5:
	/* TODO: data ref: cmpw   $0x0,0x7470 */
	cmpw   $0x0,D_7470
	je   L_1b3d
	/* TODO: data ref: mov    $0x7b4a,%ax */
	mov    $D_7b4a,%ax
	push   %ax
	call   L_1c6e
	pop    %cx
	mov    %ax,-0xe(%bp)
	or     %ax,%ax
	je   L_1b37
	/* TODO: data ref: mov    $0x748f,%ax */
	mov    $D_748f,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7480,%ax */
	mov    $D_7480,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	/* TODO: data ref: pushw  0x746e */
	pushw  D_746e
	/* TODO: data ref: mov    $0x7498,%ax */
	mov    $D_7498,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_1b37:
	/* TODO: data ref: movw   $0x0,0x7470 */
	movw   $0x0,D_7470
L_1b3d:
	/* TODO: data ref: movw   $0x7b4a,-0x6(%bp) */
	movw   $D_7b4a,-0x6(%bp)
	movw   $0x0,-0x10(%bp)
L_1b47:
	mov    -0x10(%bp),%ax
	/* TODO: data ref: cmp    0x746e,%ax */
	cmp    D_746e,%ax
	jge   L_1b8e
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1b6a
	mov    -0x6(%bp),%bx
	pushw  (%bx)
	pushw  0x4(%bp)
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1b8e
L_1b6a:
	mov    0x6(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1b85
	mov    -0x6(%bp),%bx
	pushw  0x2(%bx)
	pushw  0x6(%bp)
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1b8e
L_1b85:
	incw   -0x10(%bp)
	addw   $0x8,-0x6(%bp)
	jmp   L_1b47
L_1b8e:
	mov    -0x10(%bp),%ax
	/* TODO: data ref: cmp    0x746e,%ax */
	cmp    D_746e,%ax
	jl   L_1bb8
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	jne   L_1ba7
	/* TODO: data ref: mov    0x746a,%ax */
	mov    D_746a,%ax
	mov    %ax,-0x6(%bp)
	jmp   L_1bad
L_1ba7:
	/* TODO: data ref: mov    0x746c,%ax */
	mov    D_746c,%ax
	mov    %ax,-0x6(%bp)
L_1bad:
	cmpw   $0x0,-0x6(%bp)
	jne   L_1bb8
	mov    $0xffff,%ax
L_1bb6:
	jmp   L_1c04
L_1bb8:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    %ax,-0x8(%bp)
	mov    0x8(%bp),%ax
	mov    %ax,-0xa(%bp)
L_1bc7:
	mov    -0xa(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1bf5
	mov    -0xa(%bp),%bx
	cmpb   $0x2f,(%bx)
	jne   L_1bf0
	incw   -0xa(%bp)
	pushw  -0xa(%bp)
	/* TODO: data ref: mov    $0x74a9,%ax */
	mov    $D_74a9,%ax
	push   %ax
	call   L_12f2
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1bf0
	mov    $0xffff,%ax
	jmp   L_1c2b
L_1bf0:
	incw   -0xa(%bp)
	jmp   L_1bc7
L_1bf5:
	pushw  0x8(%bp)
	call   L_1eb6
	pop    %cx
	cmp    $0x1,%ax
	jle   L_1c06
	mov    $0xffff,%ax
L_1c04:
	jmp   L_1c2b
L_1c06:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    %ax,-0x8(%bp)
L_1c0f:
	mov    -0x8(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_1c33
	pushw  0x8(%bp)
	mov    -0x8(%bp),%bx
	pushw  (%bx)
	call   L_12f2
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1c2d
	xor    %ax,%ax
L_1c2b:
	jmp   L_1c67
L_1c2d:
	addw   $0x2,-0x8(%bp)
	jmp   L_1c0f
L_1c33:
	pushw  0x8(%bp)
	/* TODO: data ref: pushw  0x6d94 */
	pushw  D_6d94
	call   L_12f2
	add    $0x4,%sp
	or     %ax,%ax
	je   L_1c64
	pushw  0x8(%bp)
	call   L_12c0
	pop    %cx
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0xb(%bp)
	cmp    $0x44,%cl
	je   L_1c60
	cmpb   $0x58,-0xb(%bp)
	jne   L_1c64
L_1c60:
	xor    %ax,%ax
	jmp   L_1c67
L_1c64:
	mov    $0xffff,%ax
L_1c67:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1c6e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x60e,%sp
	/* TODO: data ref: mov    $0x74c4,%ax */
	mov    $D_74c4,%ax
	push   %ax
	/* TODO: data ref: mov    $0x74ad,%ax */
	mov    $D_74ad,%ax
	push   %ax
	call   L_2776
	add    $0x4,%sp
	mov    %ax,-0x60e(%bp)
	or     %ax,%ax
	jne   L_1c92
	mov    $0xffff,%ax
	jmp   L_1cc2
L_1c92:
	pushw  -0x60e(%bp)
	mov    $0x200,%ax
	push   %ax
	lea    -0x205(%bp),%ax
	push   %ax
	call   L_26de
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_1cac
	jmp   L_1e15
L_1cac:
	/* TODO: data ref: incw   0x746e */
	incw   D_746e
	/* TODO: data ref: cmpw   $0x14,0x746e */
	cmpw   $0x14,D_746e
	jle   L_1cc4
	pushw  -0x60e(%bp)
	call   L_4887
	pop    %cx
	mov    $0xffff,%ax
L_1cc2:
	jmp   L_1cec
L_1cc4:
	mov    $0x1,%ax
	push   %ax
	lea    -0x205(%bp),%ax
	push   %ax
	call   L_3760
	pop    %cx
	inc    %ax
	push   %ax
	call   L_236a
	add    $0x4,%sp
	mov    %ax,-0x60a(%bp)
	or     %ax,%ax
	jne   L_1cef
	pushw  -0x60e(%bp)
	call   L_4887
	pop    %cx
	mov    $0xffff,%ax
L_1cec:
	jmp   L_1dd9
L_1cef:
	lea    -0x205(%bp),%ax
	push   %ax
	pushw  -0x60a(%bp)
	call   L_4c4a
	add    $0x4,%sp
	lea    -0x608(%bp),%ax
	push   %ax
	pushw  -0x60a(%bp)
	call   L_1f82
	add    $0x4,%sp
	mov    %ax,-0x610(%bp)
	mov    0x4(%bp),%bx
	mov    -0x608(%bp),%ax
	mov    %ax,(%bx)
	mov    $0x2c,%ax
	push   %ax
	mov    0x4(%bp),%bx
	pushw  (%bx)
	call   L_128e
	add    $0x4,%sp
	mov    %ax,-0x60a(%bp)
	or     %ax,%ax
	je   L_1d3e
	mov    -0x60a(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x60a(%bp)
	jmp   L_1d53
L_1d3e:
	mov    0x4(%bp),%bx
	pushw  (%bx)
	call   L_3760
	pop    %cx
	mov    $0x3,%cx
	shl    %cl,%ax
	add    0x4(%bp),%ax
	mov    %ax,-0x60a(%bp)
L_1d53:
	mov    0x4(%bp),%bx
	mov    -0x60a(%bp),%ax
	mov    %ax,0x2(%bx)
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	cmpb   $0x0,(%bx)
	jne   L_1d6f
	mov    0x4(%bp),%ax
	/* TODO: data ref: mov    %ax,0x746c */
	mov    %ax,D_746c
	jmp   L_1d80
L_1d6f:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x0,(%bx)
	jne   L_1d80
	mov    0x4(%bp),%ax
	/* TODO: data ref: mov    %ax,0x746a */
	mov    %ax,D_746a
L_1d80:
	movw   $0x1,-0x612(%bp)
	/* TODO: data ref: mov    $0x74c6,%ax */
	mov    $D_74c6,%ax
	push   %ax
	pushw  -0x606(%bp)
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_1da5
	mov    0x4(%bp),%bx
	movb   $0x1,0x4(%bx)
	incw   -0x612(%bp)
	jmp   L_1dac
L_1da5:
	mov    0x4(%bp),%bx
	movb   $0x0,0x4(%bx)
L_1dac:
	mov    $0x2,%ax
	push   %ax
	mov    -0x610(%bp),%ax
	sub    -0x612(%bp),%ax
	inc    %ax
	push   %ax
	call   L_236a
	add    $0x4,%sp
	mov    0x4(%bp),%bx
	mov    %ax,0x6(%bx)
	mov    %ax,-0x60c(%bp)
	or     %ax,%ax
	jne   L_1ddb
	pushw  -0x60e(%bp)
	call   L_4887
	pop    %cx
	mov    $0xffff,%ax
L_1dd9:
	jmp   L_1e1f
L_1ddb:
	mov    -0x612(%bp),%ax
	cmp    -0x610(%bp),%ax
	jge   L_1e06
	lea    -0x608(%bp),%ax
	mov    -0x612(%bp),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    -0x60c(%bp),%bx
	mov    %ax,(%bx)
	incw   -0x612(%bp)
	addw   $0x2,-0x60c(%bp)
	jmp   L_1ddb
L_1e06:
	mov    -0x60c(%bp),%bx
	movw   $0x0,(%bx)
	addw   $0x8,0x4(%bp)
	jmp   L_1c92
L_1e15:
	pushw  -0x60e(%bp)
	call   L_4887
	pop    %cx
	xor    %ax,%ax
L_1e1f:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	/* TODO: data ref: cmpw   $0x0,0x7470 */
	cmpw   $0x0,D_7470
	je   L_1e76
	/* TODO: data ref: mov    $0x7b4a,%ax */
	mov    $D_7b4a,%ax
	push   %ax
	call   L_1c6e
	pop    %cx
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	je   L_1e70
	/* TODO: data ref: mov    $0x74d7,%ax */
	mov    $D_74d7,%ax
	push   %ax
	/* TODO: data ref: mov    $0x74c8,%ax */
	mov    $D_74c8,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	/* TODO: data ref: pushw  0x746e */
	pushw  D_746e
	/* TODO: data ref: mov    $0x74e0,%ax */
	mov    $D_74e0,%ax
	push   %ax
	/* TODO: data ref: mov    $0x79f8,%ax */
	mov    $D_79f8,%ax
	push   %ax
	call   L_24d6
	add    $0x6,%sp
	mov    $0xffff,%ax
	push   %ax
	call   L_0d39
	pop    %cx
L_1e70:
	/* TODO: data ref: movw   $0x0,0x7470 */
	movw   $0x0,D_7470
L_1e76:
	/* TODO: data ref: movw   $0x7b4a,-0x6(%bp) */
	movw   $D_7b4a,-0x6(%bp)
	movw   $0x0,-0xa(%bp)
L_1e80:
	mov    -0xa(%bp),%ax
	/* TODO: data ref: cmp    0x746e,%ax */
	cmp    D_746e,%ax
	jge   L_1ead
	pushw  0x4(%bp)
	mov    -0x6(%bp),%bx
	pushw  (%bx)
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_1ea4
	mov    -0x6(%bp),%bx
	mov    0x4(%bx),%al
	cbtw
	jmp   L_1eaf
L_1ea4:
	addw   $0x8,-0x6(%bp)
	incw   -0xa(%bp)
	jmp   L_1e80
L_1ead:
	xor    %ax,%ax
L_1eaf:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1eb6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x1e,%sp
	lea    -0x22(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c00
	add    $0x4,%sp
	cmp    $0xffff,%ax
	jne   L_1ed4
	xor    %ax,%ax
	jmp   L_1ee6
L_1ed4:
	mov    -0x1e(%bp),%ax
	and    $0xe000,%ax
	cmp    $0xc000,%ax
	jne   L_1ee3
	xor    %ax,%ax
	jmp   L_1ee6
L_1ee3:
	mov    -0x1c(%bp),%ax
L_1ee6:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x86,%sp
	lea    -0x22(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c00
	add    $0x4,%sp
	cmp    $0xffff,%ax
	je   L_1f0c
	xor    %ax,%ax
	jmp   L_1f48
L_1f0c:
	pushw  0x4(%bp)
	lea    -0x8a(%bp),%ax
	push   %ax
	call   L_4c4a
	add    $0x4,%sp
	lea    -0x8a(%bp),%ax
	push   %ax
	call   L_12c0
	pop    %cx
	mov    %ax,%bx
	movb   $0x0,(%bx)
	lea    -0x22(%bp),%ax
	push   %ax
	lea    -0x8a(%bp),%ax
	push   %ax
	call   L_4c00
	add    $0x4,%sp
	mov    %ax,-0x24(%bp)
	cmp    $0xffff,%ax
	jne   L_1f4a
	cmpw   $0x0,0x8(%bp)
	jne   L_1f4a
	mov    $0xffff,%ax
L_1f48:
	jmp   L_1f7a
L_1f4a:
	call   L_2740
	or     %ax,%ax
	jne   L_1f56
	mov    $0x2,%ax
	jmp   L_1f59
L_1f56:
	mov    $0x80,%ax
L_1f59:
	mov    %ax,-0x26(%bp)
	cmpw   $0xffff,-0x24(%bp)
	je   L_1f73
	mov    -0x1e(%bp),%ax
	test   %ax,-0x26(%bp)
	jne   L_1f6f
	mov    $0xffff,%ax
	jmp   L_1f7a
L_1f6f:
	xor    %ax,%ax
	jmp   L_1f7a
L_1f73:
	pushw  0x4(%bp)
	call   L_0fce
	pop    %cx
L_1f7a:
	lea    -0x4(%bp),%sp
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
	push   %cx
	movw   $0x0,-0x6(%bp)
L_1f8d:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
L_1f9b:
	mov    0x4(%bp),%bx
	cmpb   $0x20,(%bx)
	je   L_1fab
	mov    0x4(%bp),%bx
	cmpb   $0x9,(%bx)
	jne   L_1fb6
L_1fab:
	mov    0x4(%bp),%bx
	movb   $0x0,(%bx)
	incw   0x4(%bp)
	jmp   L_1f9b
L_1fb6:
	mov    0x4(%bp),%bx
	cmpb   $0xa,(%bx)
	jne   L_1fc4
	mov    0x4(%bp),%bx
	movb   $0x0,(%bx)
L_1fc4:
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_2006
	mov    -0x6(%bp),%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    0x4(%bp),%ax
	mov    %ax,(%bx)
	incw   0x4(%bp)
	incw   -0x6(%bp)
L_1fe1:
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
L_1fe7:
	je   L_1f8d
	mov    0x4(%bp),%bx
	cmpb   $0x20,(%bx)
	je   L_1fe7
	mov    0x4(%bp),%bx
	cmpb   $0x9,(%bx)
	je   L_1fe7
	mov    0x4(%bp),%bx
	cmpb   $0xa,(%bx)
	je   L_1fe7
	incw   0x4(%bp)
	jmp   L_1fe1
L_2006:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_200e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
L_2014:
	mov    0x4(%bp),%bx
	cmpb   $0x20,(%bx)
	je   L_202c
	mov    0x4(%bp),%bx
	cmpb   $0x9,(%bx)
	je   L_202c
	mov    0x4(%bp),%bx
	cmpb   $0xa,(%bx)
	jne   L_2031
L_202c:
	incw   0x4(%bp)
	jmp   L_2014
L_2031:
	mov    0x6(%bp),%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	jne   L_2043
	xor    %ax,%ax
	jmp   L_207f
L_2043:
	mov    0x4(%bp),%bx
	cmpb   $0x3e,(%bx)
	je   L_2063
	mov    0x4(%bp),%bx
	cmpb   $0x3c,(%bx)
	je   L_2063
	mov    0x4(%bp),%bx
	cmpb   $0x7c,(%bx)
	je   L_2063
	mov    0x4(%bp),%bx
	cmpb   $0x3b,(%bx)
	jne   L_2081
L_2063:
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   0x4(%bp)
	incw   0x6(%bp)
	mov    0x6(%bp),%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%ax
L_207f:
	jmp   L_20c8
L_2081:
	mov    0x4(%bp),%bx
	cmpb   $0x28,(%bx)
	jne   L_20ca
	mov    $0x29,%ax
	push   %ax
	mov    0x4(%bp),%ax
	inc    %ax
	push   %ax
	call   L_128e
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_20ca
	incw   -0x6(%bp)
L_20a2:
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	je   L_20bf
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   0x4(%bp)
	incw   0x6(%bp)
	jmp   L_20a2
L_20bf:
	mov    0x6(%bp),%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%ax
L_20c8:
	jmp   L_2128
L_20ca:
	mov    0x4(%bp),%bx
	cmpb   $0x20,(%bx)
	je   L_211f
	mov    0x4(%bp),%bx
	cmpb   $0x9,(%bx)
	je   L_211f
	mov    0x4(%bp),%bx
	cmpb   $0x3c,(%bx)
	je   L_211f
	mov    0x4(%bp),%bx
	cmpb   $0x3e,(%bx)
	je   L_211f
	mov    0x4(%bp),%bx
	cmpb   $0x7c,(%bx)
	je   L_211f
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_211f
	mov    0x4(%bp),%bx
	cmpb   $0x3b,(%bx)
	je   L_211f
	mov    0x4(%bp),%bx
	cmpb   $0xa,(%bx)
	je   L_211f
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   0x4(%bp)
	incw   0x6(%bp)
	jmp   L_20ca
L_211f:
	mov    0x6(%bp),%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%ax
L_2128:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_212d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%bx
	cmpb   $0x28,(%bx)
	jne   L_2183
	mov    $0x29,%ax
	push   %ax
	mov    0x4(%bp),%ax
	inc    %ax
	push   %ax
	call   L_128e
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_2183
	incw   0x4(%bp)
L_2156:
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	je   L_2173
	mov    0x4(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   0x4(%bp)
	incw   0x8(%bp)
	jmp   L_2156
L_2173:
	mov    0x8(%bp),%bx
	movb   $0x0,(%bx)
	mov    0x6(%bp),%bx
	movb   $0x0,(%bx)
	xor    %ax,%ax
	jmp   L_21ab
L_2183:
	mov    $0x21,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_128e
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jne   L_21ad
	pushw  0x4(%bp)
	pushw  0x8(%bp)
	call   L_4c4a
	add    $0x4,%sp
	mov    0x6(%bp),%bx
	movb   $0x0,(%bx)
	xor    %ax,%ax
L_21ab:
	jmp   L_21ec
L_21ad:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	movw   $0x0,-0x8(%bp)
L_21b8:
	cmpw   $0x7,-0x8(%bp)
	jge   L_21da
	mov    0x4(%bp),%bx
	incw   0x4(%bp)
	mov    (%bx),%al
	cbtw
	mov    0x6(%bp),%bx
	incw   0x6(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	je   L_21da
	incw   -0x8(%bp)
	jmp   L_21b8
L_21da:
	incw   -0x6(%bp)
	pushw  -0x6(%bp)
	pushw  0x8(%bp)
	call   L_4c4a
	add    $0x4,%sp
	mov    $0x1,%ax
L_21ec:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_21f4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x140,%sp
	/* TODO: data ref: pushw  0x7ac4 */
	pushw  D_7ac4
	/* TODO: data ref: mov    $0x74f2,%ax */
	mov    $D_74f2,%ax
	push   %ax
	lea    -0x144(%bp),%ax
	push   %ax
	call   L_3972
	add    $0x6,%sp
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x74f7,%ax */
	mov    $D_74f7,%ax
	push   %ax
	lea    -0x13c(%bp),%ax
	push   %ax
	call   L_3972
	add    $0x6,%sp
	lea    -0x144(%bp),%ax
	push   %ax
	lea    -0x13c(%bp),%ax
	push   %ax
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_223a
	xor    %ax,%ax
	jmp   L_22a9
L_223a:
	/* TODO: data ref: movw   $0x6d9a,-0x8(%bp) */
	movw   $D_6d9a,-0x8(%bp)
L_223f:
	mov    -0x8(%bp),%bx
	cmpw   $0x0,(%bx)
	je   L_22b8
	/* TODO: data ref: mov    $0x74fc,%ax */
	mov    $D_74fc,%ax
	push   %ax
	mov    -0x8(%bp),%bx
	pushw  (%bx)
	call   L_2776
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	je   L_22b2
L_225d:
	pushw  -0x6(%bp)
	mov    $0x12c,%ax
	push   %ax
	lea    -0x134(%bp),%ax
	push   %ax
	call   L_26de
	add    $0x6,%sp
	or     %ax,%ax
	je   L_22ab
	mov    $0x20,%ax
	push   %ax
	lea    -0x134(%bp),%ax
	push   %ax
	call   L_128e
	add    $0x4,%sp
	mov    %ax,%bx
	movb   $0x0,(%bx)
	movb   $0x0,-0x12d(%bp)
	lea    -0x134(%bp),%ax
	push   %ax
	lea    -0x13c(%bp),%ax
	push   %ax
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_225d
	pushw  -0x6(%bp)
	call   L_4887
	pop    %cx
	xor    %ax,%ax
L_22a9:
	jmp   L_22bb
L_22ab:
	pushw  -0x6(%bp)
	call   L_4887
	pop    %cx
L_22b2:
	addw   $0x2,-0x8(%bp)
	jmp   L_223f
L_22b8:
	mov    $0xffff,%ax
L_22bb:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_22c2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x40,%sp
	xor    %di,%di
	mov    %di,%ax
	push   %ax
	/* TODO: data ref: mov    $0x74fe,%ax */
	mov    $D_74fe,%ax
	push   %ax
	call   L_4bd0
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jl   L_233e
	mov    $0x40,%ax
	push   %ax
	lea    -0x44(%bp),%ax
	push   %ax
	push   %si
	call   L_4be6
	add    $0x6,%sp
	mov    %ax,%di
	or     %ax,%ax
	jle   L_2339
L_22f4:
	or     %di,%di
	je   L_2307
	lea    -0x44(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	cmpb   $0x22,(%bx)
	je   L_2307
	dec    %di
	jmp   L_22f4
L_2307:
	lea    -0x44(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	movb   $0x0,(%bx)
L_2311:
	or     %di,%di
	je   L_2324
	lea    -0x44(%bp),%ax
	add    %di,%ax
	mov    %ax,%bx
	cmpb   $0x22,(%bx)
	je   L_2324
	dec    %di
	jmp   L_2311
L_2324:
	mov    $0xf,%ax
	push   %ax
	lea    -0x44(%bp),%ax
	add    %di,%ax
	inc    %ax
	push   %ax
	/* TODO: data ref: mov    $0x7bc2,%ax */
	mov    $D_7bc2,%ax
	push   %ax
	call   L_377c
	add    $0x6,%sp
L_2339:
	push   %si
	call   L_4b94
	pop    %cx
L_233e:
	or     %di,%di
	jg   L_2360
	/* TODO: data ref: mov    $0x7513,%ax */
	mov    $D_7513,%ax
	push   %ax
	call   L_2a76
	pop    %cx
	/* TODO: data ref: mov    $0x753c,%ax */
	mov    $D_753c,%ax
	push   %ax
	call   L_2a76
	pop    %cx
	/* TODO: data ref: mov    $0x7551,%ax */
	mov    $D_7551,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7bc2,%ax */
	mov    $D_7bc2,%ax
	push   %ax
	call   L_4c4a
	add    $0x4,%sp
L_2360:
	/* TODO: data ref: mov    $0x7bc2,%ax */
	mov    $D_7bc2,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_236a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%ax
	mulw   0x6(%bp)
	mov    %ax,0x4(%bp)
	pushw  0x4(%bp)
	call   L_48fe
	pop    %cx
	mov    %ax,%di
	or     %di,%di
	jne   L_238a
	xor    %ax,%ax
	jmp   L_23ab
L_238a:
	mov    %di,%si
	mov    0x4(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
L_239a:
	decw   -0x6(%bp)
	jl   L_23a9
	mov    %si,%bx
	inc    %si
	inc    %si
	movw   $0x0,(%bx)
	jmp   L_239a
L_23a9:
	mov    %di,%ax
L_23ab:
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
	call   L_4a48
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_23c0:
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
	jns    L_23d1
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_23d1:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_23d4:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall chmod(15) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* chmod -> __NR_chmod */
	xchg	%ax,%bx
	mov	%dx,%cx
	mov	$__NR_chmod,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_23e8
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_23e8:
	pop    %bp
	ret


L_23ea:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_23fa:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	je   L_242b
	decw   0x2(%si)
	jl   L_241c
	mov    (%si),%bx
	incw   (%si)
	mov    -0x8(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_2426
L_241c:
	push   %si
	pushw  -0x8(%bp)
	call   L_472a
	add    $0x4,%sp
L_2426:
	mov    %ax,-0x6(%bp)
	jmp   L_23fa
L_242b:
	mov    -0x6(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2436:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	push   %si
	call   L_4887
	pop    %cx
	cmpb   $0x77,(%di)
	jne   L_2460
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2874
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	jmp   L_249c
L_2460:
	cmpb   $0x61,(%di)
	jne   L_249e
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4bd0
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jge   L_2489
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2874
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_2489:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	call   L_3936
	add    $0x8,%sp
L_249c:
	jmp   L_24ad
L_249e:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4bd0
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_24ad:
	cmpw   $0x0,-0x6(%bp)
	jge   L_24b7
	xor    %ax,%ax
	jmp   L_24d0
L_24b7:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,0x7(%si)
	cmpb   $0x72,(%di)
	je   L_24ca
	orb    $0x2,0x6(%si)
	jmp   L_24ce
L_24ca:
	orb    $0x1,0x6(%si)
L_24ce:
	mov    %si,%ax
L_24d0:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_24d6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_39c4
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_24fc
	mov    $0xffff,%ax
	jmp   L_24fe
L_24fc:
	xor    %ax,%ax
L_24fe:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2502:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_2550
L_250a:
	call   L_25c0
	mov    %ax,%di
	or     %ax,%ax
	je   L_2522
	pushw  (%di)
	pushw  0x4(%bp)
	call   L_4b52
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_250a
L_2522:
	call   L_257b
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_252c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	call   L_2550
L_2538:
	call   L_25c0
	mov    %ax,%si
	or     %ax,%ax
	je   L_2546
	cmp    %di,0x4(%si)
	jne   L_2538
L_2546:
	call   L_257b
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2550:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: cmpw   $0x0,0x7566 */
	cmpw   $0x0,D_7566
	jne   L_256f
	/* TODO: data ref: mov    $0x777c,%ax */
	mov    $D_777c,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7558,%ax */
	mov    $D_7558,%ax
	push   %ax
	call   L_2776
	add    $0x4,%sp
	/* TODO: data ref: mov    %ax,0x7566 */
	mov    %ax,D_7566
	jmp   L_2577
L_256f:
	/* TODO: data ref: pushw  0x7566 */
	pushw  D_7566
	call   L_2b78
	pop    %cx
L_2577:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_257b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: cmpw   $0x0,0x7566 */
	cmpw   $0x0,D_7566
	je   L_2595
	/* TODO: data ref: pushw  0x7566 */
	pushw  D_7566
	call   L_4887
	pop    %cx
	/* TODO: data ref: movw   $0x0,0x7566 */
	movw   $0x0,D_7566
L_2595:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2599:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
L_25a2:
	cmpb   $0x0,(%di)
	je   L_25af
	cmpb   $0x3a,(%di)
	je   L_25af
	inc    %di
	jmp   L_25a2
L_25af:
	cmpb   $0x0,(%di)
	je   L_25ba
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
L_25ba:
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_25c0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: cmpw   $0x0,0x7566 */
	cmpw   $0x0,D_7566
	jne   L_25e5
	/* TODO: data ref: mov    $0x777e,%ax */
	mov    $D_777e,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7558,%ax */
	mov    $D_7558,%ax
	push   %ax
	call   L_2776
	add    $0x4,%sp
	/* TODO: data ref: mov    %ax,0x7566 */
	mov    %ax,D_7566
	or     %ax,%ax
	jne   L_25e5
	xor    %ax,%ax
	jmp   L_25ff
L_25e5:
	/* TODO: data ref: pushw  0x7566 */
	pushw  D_7566
	mov    $0x200,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7568,%ax */
	mov    $D_7568,%ax
	push   %ax
	call   L_26de
	add    $0x6,%sp
	mov    %ax,%di
	or     %di,%di
	jne   L_2601
	xor    %ax,%ax
L_25ff:
	jmp   L_266e
L_2601:
	/* TODO: data ref: mov    %di,0x776a */
	mov    %di,D_776a
	push   %di
	call   L_2599
	pop    %cx
	mov    %ax,%di
	/* TODO: data ref: mov    %di,0x776c */
	mov    %di,D_776c
	push   %di
	call   L_2599
	pop    %cx
	mov    %ax,%di
	push   %di
	call   L_2672
	pop    %cx
	/* TODO: data ref: mov    %ax,0x776e */
	mov    %ax,D_776e
	push   %di
	call   L_2599
	pop    %cx
	mov    %ax,%di
	push   %di
	call   L_2672
	pop    %cx
	/* TODO: data ref: mov    %ax,0x7770 */
	mov    %ax,D_7770
	/* TODO: data ref: movw   $0x0,0x7772 */
	movw   $0x0,D_7772
	/* TODO: data ref: movw   $0x7564,0x7774 */
	/* dual-reloc split: movw $D_7564,D_7774 */
	mov	$D_7564,%ax
	mov	%ax,D_7774
	push   %di
	call   L_2599
	pop    %cx
	mov    %ax,%di
	/* TODO: data ref: mov    %di,0x7776 */
	mov    %di,D_7776
	push   %di
	call   L_2599
	pop    %cx
	mov    %ax,%di
	/* TODO: data ref: mov    %di,0x7778 */
	mov    %di,D_7778
	push   %di
	call   L_2599
	pop    %cx
	mov    %ax,%di
	/* TODO: data ref: mov    %di,0x777a */
	mov    %di,D_777a
L_265b:
	cmpb   $0x0,(%di)
	je   L_2668
	cmpb   $0xa,(%di)
	je   L_2668
	inc    %di
	jmp   L_265b
L_2668:
	movb   $0x0,(%di)
	/* TODO: data ref: mov    $0x776a,%ax */
	mov    $D_776a,%ax
L_266e:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2672:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	xor    %si,%si
	mov    %si,-0x6(%bp)
L_2681:
	mov    (%di),%al
	cbtw
	jmp   L_268c
L_2686:
	incw   -0x6(%bp)
L_2689:
	inc    %di
	jmp   L_26a5
L_268c:
	cmp    $0x9,%ax
	je   L_26a2
	cmp    $0x20,%ax
	je   L_26a2
	cmp    $0x2b,%ax
	je   L_2689
	cmp    $0x2d,%ax
	je   L_2686
	jmp   L_26a5
L_26a2:
	inc    %di
	jmp   L_2681
L_26a5:
	cmpb   $0x30,(%di)
	jl   L_26cb
	cmpb   $0x39,(%di)
	jg   L_26cb
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
	jmp   L_26a5
L_26cb:
	cmpw   $0x0,-0x6(%bp)
	je   L_26d7
	mov    %si,%ax
	neg    %ax
	jmp   L_26d9
L_26d7:
	mov    %si,%ax
L_26d9:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_26de:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x8(%bp),%di
	mov    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
L_26ee:
	decw   0x6(%bp)
	jle   L_271e
	decw   0x2(%di)
	jl   L_2704
	mov    (%di),%bx
	incw   (%di)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2709
L_2704:
	push   %di
	call   L_3560
	pop    %cx
L_2709:
	mov    %ax,%si
	or     %ax,%ax
	jl   L_271e
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	cmp    $0xa,%si
	jne   L_26ee
L_271e:
	or     %si,%si
	jge   L_272e
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jne   L_272e
	xor    %ax,%ax
	jmp   L_273a
L_272e:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x6(%bp)
	mov    0x4(%bp),%ax
L_273a:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2740:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getuid(24) */
	/* getuid -> __NR_getuid */
	mov	$__NR_getuid,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_274e
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_274e:
	pop    %bp
	ret


L_2750:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall link(9) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* link -> __NR_link */
	xchg	%ax,%bx
	mov	%dx,%cx
	mov	$__NR_link,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2764
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2764:
	pop    %bp
	ret


L_2766:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall getpid(20) */
	/* getpid -> __NR_getpid */
	mov	$__NR_getpid,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_2774
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2774:
	pop    %bp
	ret


L_2776:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	/* TODO: data ref: cmpb   $0x0,0x7780 */
	cmpb   $0x0,D_7780
	jne   L_27b3
	/* TODO: data ref: movw   $0x7a00,-0x8(%bp) */
	movw   $D_7a00,-0x8(%bp)
	/* TODO: data ref: movw   $0x7a88,0x7a88 */
	/* suppressed: movw $D_7a88,D_7a88 (self-ref in .data) */
	movw   $0x88,-0xa(%bp)
L_2799:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_27ae
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_2799
L_27ae:
	/* TODO: data ref: movb   $0x1,0x7780 */
	movb   $0x1,D_7780
L_27b3:
	/* TODO: data ref: movw   $0x79e8,-0x6(%bp) */
	movw   $D_79e8,-0x6(%bp)
L_27b8:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_27d7
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x7a88,%ax */
	cmp    D_7a88,%ax
	jb   L_27d1
	xor    %ax,%ax
	jmp   L_2842
L_27d1:
	addw   $0x8,-0x6(%bp)
	jmp   L_27b8
L_27d7:
	cmpb   $0x77,(%di)
	jne   L_27ed
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2874
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_282c
L_27ed:
	cmpb   $0x61,(%di)
	jne   L_282e
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4bd0
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_281b
	/* TODO: data ref: cmpw   $0x2,0x7bd2 */
	cmpw   $0x2,D_7bd2
	jne   L_281b
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_2874
	add    $0x4,%sp
	mov    %ax,%si
L_281b:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_3936
	add    $0x8,%sp
L_282c:
	jmp   L_283c
L_282e:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4bd0
	add    $0x4,%sp
	mov    %ax,%si
L_283c:
	or     %si,%si
	jge   L_2844
	xor    %ax,%ax
L_2842:
	jmp   L_286c
L_2844:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_2862
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_2869
L_2862:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_2869:
	mov    -0x6(%bp),%ax
L_286c:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2874:
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
	jns    L_2888
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2888:
	pop    %bp
	ret


L_288a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_2a54
	pop    %cx
	or     %ax,%ax
	jge   L_28a2
	xor    %ax,%ax
	jmp   L_2919
L_28a2:
	mov    0x6(%bp),%bx
	cmpb   $0x72,(%bx)
	jne   L_28af
	mov    -0x8(%bp),%ax
	jmp   L_28b2
L_28af:
	mov    -0x6(%bp),%ax
L_28b2:
	mov    %ax,%di
	mov    0x6(%bp),%bx
	cmpb   $0x72,(%bx)
	jne   L_28c1
	mov    -0x6(%bp),%ax
	jmp   L_28c4
L_28c1:
	mov    -0x8(%bp),%ax
L_28c4:
	mov    %ax,%si
	call   L_3806
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_2911
	push   %di
	call   L_4b94
	pop    %cx
	mov    0x6(%bp),%bx
	cmpb   $0x72,(%bx)
	jne   L_28e2
	mov    $0x1,%ax
	jmp   L_28e4
L_28e2:
	xor    %ax,%ax
L_28e4:
	push   %ax
	push   %si
	call   L_29dc
	add    $0x4,%sp
	push   %si
	call   L_4b94
	pop    %cx
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x77b5,%ax */
	mov    $D_77b5,%ax
	push   %ax
	/* TODO: data ref: mov    $0x77b2,%ax */
	mov    $D_77b2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x77aa,%ax */
	mov    $D_77aa,%ax
	push   %ax
	call   L_37d4
	add    $0xa,%sp
	mov    $0x1,%ax
	push   %ax
	call   L_37f2
	pop    %cx
L_2911:
	cmpw   $0xffff,-0xa(%bp)
	jne   L_291b
	xor    %ax,%ax
L_2919:
	jmp   L_2935
L_291b:
	mov    %di,%bx
	shl    %bx
	mov    -0xa(%bp),%ax
	/* TODO: data ref: mov    %ax,0x7782(%bx) */
	mov    %ax,D_7782(%bx)
	push   %si
	call   L_4b94
	pop    %cx
	pushw  0x6(%bp)
	push   %di
	call   L_29f6
	add    $0x4,%sp
L_2935:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_293c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	mov    %ax,%di
	pushw  0x4(%bp)
	call   L_4887
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	mov    $0x1,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    %ax,-0xa(%bp)
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_2987:
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_4d22
	pop    %cx
	mov    %ax,%si
	mov    %di,%bx
	shl    %bx
	/* TODO: data ref: cmp    0x7782(%bx),%ax */
	cmp    D_7782(%bx),%ax
	je   L_29a0
	cmp    $0xffff,%si
	jne   L_2987
L_29a0:
	cmp    $0xffff,%si
	jne   L_29aa
	movw   $0xffff,-0xc(%bp)
L_29aa:
	pushw  -0x8(%bp)
	mov    $0x2,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	pushw  -0xa(%bp)
	mov    $0x3,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	pushw  -0x6(%bp)
	mov    $0x1,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    -0xc(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_29dc:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall dup(41) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	or     $0x40,%ax
	/* dup -> __NR_dup */
	xchg	%ax,%bx
	mov	$__NR_dup,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_29f3
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_29f3:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_29f6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x6(%bp),%di
	/* TODO: data ref: mov    $0x79e8,%si */
	mov    $D_79e8,%si
L_2a02:
	mov    0x6(%si),%al
	cbtw
	test   $0x3,%ax
	je   L_2a1a
	/* TODO: data ref: cmp    0x7a88,%si */
	cmp    D_7a88,%si
	jb   L_2a15
	xor    %ax,%ax
	jmp   L_2a50
L_2a15:
	add    $0x8,%si
	jmp   L_2a02
L_2a1a:
	movw   $0x0,0x2(%si)
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,0x7(%si)
	cmpb   $0x72,(%di)
	je   L_2a4a
	orb    $0x2,0x6(%si)
	cmpb   $0x61,(%di)
	jne   L_2a4e
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  0x4(%bp)
	call   L_3936
	add    $0x8,%sp
	jmp   L_2a4e
L_2a4a:
	orb    $0x1,0x6(%si)
L_2a4e:
	mov    %si,%ax
L_2a50:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2a54:
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
	jns    L_2a64
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_2a62:
	pop    %bp
	ret
L_2a64:
	mov    0x4(%bp),%cx
	mov    %cx,%bp
	mov    %ax,-0x70(%bp)
	mov    %dx,0x2(%bp)
	mov    $0x0,%ax
	jmp   L_2a62
	/* .byte 0 (alignment) */


L_2a76:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x79f0,%ax */
	mov    $D_79f0,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_39c4
	add    $0x6,%sp
	/* TODO: data ref: mov    0x79f6,%al */
	mov    D_79f6,%al
	cbtw
	test   $0x20,%ax
	je   L_2a9a
	mov    $0xffff,%ax
	jmp   L_2a9c
L_2a9a:
	xor    %ax,%ax
L_2a9c:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2aa0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_2b01
L_2abb:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_2b01
	mov    0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
L_2ac9:
	decw   0x2(%si)
	jl   L_2ada
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2adf
L_2ada:
	push   %si
	call   L_3560
	pop    %cx
L_2adf:
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jl   L_2af2
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_2af7
L_2af2:
	mov    -0x8(%bp),%ax
	jmp   L_2b04
L_2af7:
	decw   -0xa(%bp)
	jne   L_2ac9
	incw   -0x8(%bp)
	jmp   L_2abb
L_2b01:
	mov    -0x8(%bp),%ax
L_2b04:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2b0b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_2b6d
L_2b26:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_2b6d
	mov    0x6(%bp),%ax
	mov    %ax,-0x6(%bp)
L_2b34:
	decw   0x2(%si)
	jl   L_2b4c
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
	jmp   L_2b5a
L_2b4c:
	push   %si
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_472a
	add    $0x4,%sp
L_2b5a:
	decw   -0x6(%bp)
	jne   L_2b34
	mov    0x6(%si),%al
	cbtw
	test   $0x20,%ax
	jne   L_2b6d
	incw   -0x8(%bp)
	jmp   L_2b26
L_2b6d:
	mov    -0x8(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2b78:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	push   %di
	call   L_4818
	pop    %cx
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_3936
	add    $0x8,%sp
	movw   $0x0,0x2(%di)
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	andb   $0xcf,0x6(%di)
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
	/* TODO: data ref: mov    $0x79e8,%ax */
	mov    $D_79e8,%ax
	push   %ax
	call   L_2c20
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2bc6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_2c20
	add    $0x6,%sp
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
	movb   $0x41,-0x6(%bp)
	mov    %di,-0x8(%bp)
	mov    %di,-0xc(%bp)
	movw   $0x0,-0xa(%bp)
L_2bfa:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_2c07
	incw   -0xa(%bp)
	jmp   L_2bfa
L_2c07:
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_2c20
	add    $0x6,%sp
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_2c20:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xe,%sp
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x10(%bp)
L_2c3a:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	jmp   L_2de4
L_2c46:
	mov    -0x8(%bp),%ax
	jmp   L_2d14
L_2c4c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	cmp    $0x25,%ax
	jne   L_2c5d
	jmp   L_2e02
L_2c5d:
	movw   $0x0,-0xe(%bp)
	cmpw   $0x2a,-0x6(%bp)
	je   L_2c71
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xe(%bp)
	jmp   L_2c7a
L_2c71:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
L_2c7a:
	movw   $0x0,-0xa(%bp)
	movw   $0x1,-0x12(%bp)
L_2c84:
	cmpw   $0x30,-0x6(%bp)
	jl   L_2cac
	cmpw   $0x39,-0x6(%bp)
	jg   L_2cac
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
	jmp   L_2c84
L_2cac:
	cmpw   $0x0,-0xa(%bp)
	jne   L_2cb7
	/* TODO: data ref: movw   $0x7530,-0xa(%bp) */
	movw   $D_7530,-0xa(%bp)
L_2cb7:
	cmpw   $0x6c,-0x6(%bp)
	jne   L_2ccd
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	movw   $0x2,-0x12(%bp)
	jmp   L_2cf0
L_2ccd:
	cmpw   $0x68,-0x6(%bp)
	jne   L_2ce3
	movw   $0x0,-0x12(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	jmp   L_2cf0
L_2ce3:
	cmpw   $0x5b,-0x6(%bp)
	jne   L_2cf0
	push   %di
	call   L_32b8
	pop    %cx
	mov    %ax,%di
L_2cf0:
	mov    -0x6(%bp),%bx
	/* TODO: data ref: mov    0x788f(%bx),%al */
	mov    D_788f(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_2d0b
	mov    -0x6(%bp),%ax
	add    $0x20,%ax
	mov    %ax,-0x6(%bp)
	movw   $0x2,-0x12(%bp)
L_2d0b:
	cmpw   $0x0,-0x6(%bp)
	jne   L_2d16
	mov    $0xffff,%ax
L_2d14:
	jmp   L_2d53
L_2d16:
	lea    -0x10(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	pushw  -0x12(%bp)
	pushw  -0xa(%bp)
	pushw  -0x6(%bp)
	pushw  -0xe(%bp)
	call   L_2e0b
	add    $0xc,%sp
	or     %ax,%ax
	je   L_2d3c
	cmpw   $0x0,-0xe(%bp)
	je   L_2d3c
	incw   -0x8(%bp)
L_2d3c:
	cmpw   $0x0,-0x10(%bp)
L_2d40:
	jne   L_2d45
	jmp   L_2c3a
L_2d45:
	cmpw   $0x0,-0x8(%bp)
	je   L_2d50
	mov    -0x8(%bp),%ax
	jmp   L_2d53
L_2d50:
	mov    $0xffff,%ax
L_2d53:
	jmp   L_2dd1
L_2d55:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_2d6f
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2d76
L_2d6f:
	pushw  0x4(%bp)
	call   L_3560
	pop    %cx
L_2d76:
	mov    %ax,-0xc(%bp)
	cmp    $0x20,%ax
	je   L_2d55
	cmpw   $0x9,-0xc(%bp)
	je   L_2d55
	cmpw   $0xa,-0xc(%bp)
L_2d88:
	je   L_2d55
	cmpw   $0xffff,-0xc(%bp)
L_2d8e:
	je   L_2d40
	pushw  0x4(%bp)
	pushw  -0xc(%bp)
	call   L_4cc0
	add    $0x4,%sp
	jmp   L_2c3a
L_2d9f:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_2db9
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2dc0
L_2db9:
	pushw  0x4(%bp)
	call   L_3560
	pop    %cx
L_2dc0:
	mov    %ax,-0xc(%bp)
	cmp    -0x6(%bp),%ax
	je   L_2d8e
	cmpw   $0xffff,-0xc(%bp)
	jne   L_2dd3
	mov    $0xffff,%ax
L_2dd1:
	jmp   L_2e04
L_2dd3:
	pushw  0x4(%bp)
	pushw  -0xc(%bp)
	call   L_4cc0
	add    $0x4,%sp
	mov    -0x8(%bp),%ax
	jmp   L_2e04
L_2de4:
	or     %ax,%ax
	jne   L_2deb
	jmp   L_2c46
L_2deb:
	cmp    $0x9,%ax
L_2dee:
	je   L_2d88
	cmp    $0xa,%ax
	je   L_2dee
	cmp    $0x20,%ax
	je   L_2dee
	cmp    $0x25,%ax
	jne   L_2e02
	jmp   L_2c4c
L_2e02:
	jmp   L_2d9f
L_2e04:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2e0b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x63,0x6(%bp)
	je   L_2e25
	cmpw   $0x73,0x6(%bp)
	je   L_2e25
	cmpw   $0x5b,0x6(%bp)
	jne   L_2e4a
L_2e25:
	pushw  0xe(%bp)
	pushw  0xc(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	cmpw   $0x0,0x4(%bp)
	je   L_2e3e
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	jmp   L_2e40
L_2e3e:
	xor    %ax,%ax
L_2e40:
	push   %ax
	call   L_31aa
	add    $0xa,%sp
	jmp   L_3118
L_2e4a:
	movw   $0x0,-0x58(%bp)
	movw   $0x0,-0x56(%bp)
	movw   $0x0,-0x52(%bp)
	movw   $0x0,-0x4c(%bp)
	cmpw   $0x65,0x6(%bp)
	je   L_2e6a
	cmpw   $0x66,0x6(%bp)
	jne   L_2e6f
L_2e6a:
	movw   $0x1,-0x4c(%bp)
L_2e6f:
	movw   $0xa,-0x48(%bp)
	cmpw   $0x6f,0x6(%bp)
	jne   L_2e81
	movw   $0x8,-0x48(%bp)
	jmp   L_2e8c
L_2e81:
	cmpw   $0x78,0x6(%bp)
	jne   L_2e8c
	movw   $0x10,-0x48(%bp)
L_2e8c:
	lea    -0x44(%bp),%ax
	mov    %ax,%di
	movw   $0x0,-0x4a(%bp)
	movw   $0x0,-0x4e(%bp)
L_2e9b:
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_2eb5
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2ebc
L_2eb5:
	pushw  0xc(%bp)
	call   L_3560
	pop    %cx
L_2ebc:
	mov    %ax,-0x46(%bp)
	cmp    $0x20,%ax
	je   L_2e9b
	cmpw   $0x9,-0x46(%bp)
	je   L_2e9b
	cmpw   $0xa,-0x46(%bp)
	je   L_2e9b
	cmpw   $0x2d,-0x46(%bp)
	jne   L_2f0c
	incw   -0x4e(%bp)
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_2efd
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2f04
L_2efd:
	pushw  0xc(%bp)
	call   L_3560
	pop    %cx
L_2f04:
	mov    %ax,-0x46(%bp)
	decw   0x8(%bp)
	jmp   L_2f39
L_2f0c:
	cmpw   $0x2b,-0x46(%bp)
	jne   L_2f39
	decw   0x8(%bp)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_2f2f
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_2f36
L_2f2f:
	pushw  0xc(%bp)
	call   L_3560
	pop    %cx
L_2f36:
	mov    %ax,-0x46(%bp)
L_2f39:
	decw   0x8(%bp)
	jge   L_2f41
	jmp   L_3095
L_2f41:
	movw   $0x0,-0x54(%bp)
	cmpw   $0x30,-0x46(%bp)
	jl   L_2f57
	cmpw   $0x39,-0x46(%bp)
	jg   L_2f57
	movw   $0x1,-0x54(%bp)
L_2f57:
	cmpw   $0x61,-0x46(%bp)
	jl   L_2f63
	cmpw   $0x7a,-0x46(%bp)
	jle   L_2f6f
L_2f63:
	cmpw   $0x41,-0x46(%bp)
	jl   L_2f7a
	cmpw   $0x5a,-0x46(%bp)
	jg   L_2f7a
L_2f6f:
	cmpw   $0x10,-0x48(%bp)
	jne   L_2f7a
	movw   $0x1,-0x54(%bp)
L_2f7a:
	cmpw   $0x0,-0x54(%bp)
	jne   L_2f83
	jmp   L_3017
L_2f83:
	incw   -0x52(%bp)
	cmpw   $0x8,-0x48(%bp)
	jne   L_2f9d
	mov    $0x3,%cx
	or     %cx,%cx
	je   L_2fd7
L_2f93:
	shlw   -0x58(%bp)
	rclw   -0x56(%bp)
	loop   L_2f93
	jmp   L_2fd7
L_2f9d:
	cmpw   $0xa,-0x48(%bp)
	jne   L_2fc8
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	mov    $0x2,%cx
	or     %cx,%cx
	je   L_2fb6
L_2fb0:
	shl    %ax
	rcl    %dx
	loop   L_2fb0
L_2fb6:
	add    -0x58(%bp),%ax
	adc    -0x56(%bp),%dx
	shl    %ax
	rcl    %dx
	mov    %ax,-0x58(%bp)
	mov    %dx,-0x56(%bp)
	jmp   L_2fd7
L_2fc8:
	mov    $0x4,%cx
	or     %cx,%cx
	je   L_2fd7
L_2fcf:
	shlw   -0x58(%bp)
	rclw   -0x56(%bp)
	loop   L_2fcf
L_2fd7:
	mov    -0x46(%bp),%ax
	mov    %ax,-0x50(%bp)
	cmpw   $0x30,-0x46(%bp)
	jl   L_2fef
	cmpw   $0x39,-0x46(%bp)
	jg   L_2fef
	subw   $0x30,-0x46(%bp)
	jmp   L_3005
L_2fef:
	cmpw   $0x61,-0x46(%bp)
	jl   L_3001
	cmpw   $0x66,-0x46(%bp)
	jg   L_3001
	subw   $0x57,-0x46(%bp)
	jmp   L_3005
L_3001:
	subw   $0x37,-0x46(%bp)
L_3005:
	mov    -0x46(%bp),%ax
	cwtd
	add    %ax,-0x58(%bp)
	adc    %dx,-0x56(%bp)
	mov    -0x50(%bp),%ax
	mov    %ax,-0x46(%bp)
	jmp   L_302c
L_3017:
	cmpw   $0x2e,-0x46(%bp)
	jne   L_302e
	cmpw   $0xa,-0x48(%bp)
	jne   L_3044
	cmpw   $0x0,-0x4c(%bp)
	je   L_3053
	incw   -0x52(%bp)
L_302c:
	jmp   L_309d
L_302e:
	cmpw   $0x65,-0x46(%bp)
	je   L_303a
	cmpw   $0x45,-0x46(%bp)
	jne   L_3044
L_303a:
	cmpw   $0x0,-0x4a(%bp)
	jne   L_3044
	cmpw   $0xa,-0x48(%bp)
L_3044:
	je   L_3049
	jmp   L_30ce
L_3049:
	cmpw   $0x0,-0x4c(%bp)
	je   L_3053
	cmpw   $0x0,-0x52(%bp)
L_3053:
	je   L_30ce
	incw   -0x4a(%bp)
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_307c
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_3083
L_307c:
	pushw  0xc(%bp)
	call   L_3560
	pop    %cx
L_3083:
	mov    %ax,-0x46(%bp)
	cmp    $0x2b,%ax
	je   L_309d
	cmpw   $0x2d,-0x46(%bp)
	je   L_309d
	cmpw   $0x30,-0x46(%bp)
L_3095:
	jl   L_30ce
	cmpw   $0x39,-0x46(%bp)
	jg   L_30ce
L_309d:
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_30c1
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_30c8
L_30c1:
	pushw  0xc(%bp)
	call   L_3560
	pop    %cx
L_30c8:
	mov    %ax,-0x46(%bp)
	jmp   L_2f39
L_30ce:
	cmpw   $0x0,-0x4e(%bp)
	je   L_30e7
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	mov    %ax,-0x58(%bp)
	mov    %dx,-0x56(%bp)
L_30e7:
	cmpw   $0xffff,-0x46(%bp)
	je   L_3102
	pushw  0xc(%bp)
	pushw  -0x46(%bp)
	call   L_4cc0
	add    $0x4,%sp
	mov    0xe(%bp),%bx
	movw   $0x0,(%bx)
	jmp   L_3109
L_3102:
	mov    0xe(%bp),%bx
	movw   $0x1,(%bx)
L_3109:
	cmpw   $0x0,0x4(%bp)
	je   L_3116
	lea    -0x44(%bp),%ax
	cmp    %ax,%di
	jne   L_311b
L_3116:
	xor    %ax,%ax
L_3118:
	jmp   L_31a3
L_311b:
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	mov    -0x4c(%bp),%ax
	mov    $0x4,%cx
	shl    %cl,%ax
	or     0xa(%bp),%ax
	jmp   L_3194
	lea    -0x44(%bp),%ax
	push   %ax
	call   L_3322
	fstpl  -0x60(%bp)
	pop    %cx
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	fldl   -0x60(%bp)
	fstps  (%bx)
	jmp   L_3192
	lea    -0x44(%bp),%ax
	push   %ax
	call   L_3322
	fstpl  -0x60(%bp)
	pop    %cx
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	fldl   -0x60(%bp)
	fstpl  (%bx)
	jmp   L_3192
	mov    -0x58(%bp),%ax
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    %ax,(%bx)
	jmp   L_3192
	mov    -0x58(%bp),%ax
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    %ax,(%bx)
	jmp   L_3192
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_3192:
	jmp   L_31a0
L_3194:
	cmp    $0x12,%ax
	ja   L_31a0
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x7838(%bx)
L_31a0:
	mov    $0x1,%ax
L_31a3:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_31aa:
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
	jne   L_31d6
	/* TODO: data ref: cmpw   $0x7530,0x8(%bp) */
	cmpw   $D_7530,0x8(%bp)
	jne   L_31d6
	movw   $0x1,0x8(%bp)
L_31d6:
	movw   $0x0,-0xa(%bp)
	cmpw   $0x73,0x6(%bp)
	jne   L_31e6
	movw   $0x1,-0xa(%bp)
L_31e6:
	decw   0x2(%si)
	jl   L_31f7
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_31fc
L_31f7:
	push   %si
	call   L_3560
	pop    %cx
L_31fc:
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	/* TODO: data ref: mov    0x77b8(%bx),%al */
	mov    D_77b8(%bx),%al
	cbtw
	test   %ax,-0xa(%bp)
	je   L_3211
	cmpw   $0xffff,-0x6(%bp)
	jne   L_31e6
L_3211:
	movw   $0x1,-0xa(%bp)
	cmpw   $0x63,0x6(%bp)
	jne   L_3223
	movw   $0x0,-0xa(%bp)
	jmp   L_322e
L_3223:
	cmpw   $0x5b,0x6(%bp)
	jne   L_322e
	movw   $0x2,-0xa(%bp)
L_322e:
	cmpw   $0xffff,-0x6(%bp)
	je   L_326f
	mov    -0x6(%bp),%bx
	/* TODO: data ref: mov    0x77b8(%bx),%al */
	mov    D_77b8(%bx),%al
	cbtw
	test   %ax,-0xa(%bp)
	jne   L_326f
	or     %di,%di
	je   L_324f
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
L_324f:
	decw   0x8(%bp)
	jle   L_326f
	decw   0x2(%si)
	jl   L_3265
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_326a
L_3265:
	push   %si
	call   L_3560
	pop    %cx
L_326a:
	mov    %ax,-0x6(%bp)
	jmp   L_322e
L_326f:
	cmpw   $0xffff,-0x6(%bp)
	je   L_328e
	cmpw   $0x0,0x8(%bp)
	jle   L_3285
	push   %si
	pushw  -0x6(%bp)
	call   L_4cc0
	add    $0x4,%sp
L_3285:
	mov    0xc(%bp),%bx
	movw   $0x0,(%bx)
	jmp   L_3295
L_328e:
	mov    0xc(%bp),%bx
	movw   $0x1,(%bx)
L_3295:
	or     %di,%di
	je   L_32af
	cmp    -0x8(%bp),%di
	je   L_32af
	cmpw   $0x63,0x6(%bp)
	je   L_32aa
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
L_32aa:
	mov    $0x1,%ax
	jmp   L_32b1
L_32af:
	xor    %ax,%ax
L_32b1:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_32b8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	movw   $0x0,-0x6(%bp)
	cmpb   $0x5e,(%di)
	jne   L_32d0
	incw   -0x6(%bp)
	inc    %di
L_32d0:
	xor    %si,%si
L_32d2:
	cmp    $0x80,%si
	jge   L_32ed
	cmpw   $0x0,-0x6(%bp)
	je   L_32e5
	/* TODO: data ref: andb   $0xfd,0x77b8(%si) */
	andb   $0xfd,D_77b8(%si)
	jmp   L_32ea
L_32e5:
	/* TODO: data ref: orb    $0x2,0x77b8(%si) */
	orb    $0x2,D_77b8(%si)
L_32ea:
	inc    %si
	jmp   L_32d2
L_32ed:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	and    $0x7f,%ax
	cmp    $0x5d,%ax
	je   L_331a
	cmpw   $0x0,-0x6(%bp)
	je   L_330b
	/* TODO: data ref: orb    $0x2,0x77b8(%si) */
	orb    $0x2,D_77b8(%si)
	inc    %si
	jmp   L_3311
L_330b:
	/* TODO: data ref: andb   $0xfd,0x77b8(%si) */
	andb   $0xfd,D_77b8(%si)
	inc    %si
L_3311:
	or     %si,%si
	jne   L_32ed
	dec    %di
	mov    %di,%ax
	jmp   L_331c
L_331a:
	mov    %di,%ax
L_331c:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3322:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x3c,%sp
	mov    0x4(%bp),%di
	movw   $0x1,-0x24(%bp)
L_3333:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    $0x20,%ax
	je   L_3333
	cmp    $0x2d,%si
	jne   L_334c
	movw   $0xffff,-0x24(%bp)
	jmp   L_3352
L_334c:
	cmp    $0x2b,%si
	je   L_3352
	dec    %di
L_3352:
	movw   $0x0,-0x22(%bp)
	/* TODO: data ref: fldl   0x785e */
	fldl   D_785e
	fstpl  -0xc(%bp)
	movw   $0x0,-0x1e(%bp)
L_3367:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	/* TODO: data ref: mov    0x788f(%si),%al */
	mov    D_788f(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_33b4
	cmpw   $0x10,-0x1e(%bp)
	jge   L_33ac
	/* TODO: data ref: fldl   0x7866 */
	fldl   D_7866
	fmull  -0xc(%bp)
	fstpl  -0x30(%bp)
	lea    -0x30(%si),%ax
	mov    %ax,-0x38(%bp)
	filds  -0x38(%bp)
	fstl   -0x38(%bp)
	faddl  -0x30(%bp)
	fstl   -0x40(%bp)
	fstpl  -0xc(%bp)
	jmp   L_33af
L_33ac:
	incw   -0x22(%bp)
L_33af:
	incw   -0x1e(%bp)
	jmp   L_3367
L_33b4:
	cmp    $0x2e,%si
	jne   L_3404
L_33b9:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	/* TODO: data ref: mov    0x788f(%si),%al */
	mov    D_788f(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_3404
	cmpw   $0x10,-0x1e(%bp)
	jge   L_33ff
	/* TODO: data ref: fldl   0x786e */
	fldl   D_786e
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
L_33ff:
	incw   -0x1e(%bp)
	jmp   L_33b9
L_3404:
	movw   $0x1,-0x26(%bp)
	movw   $0x0,-0x20(%bp)
	cmp    $0x45,%si
	je   L_3418
	cmp    $0x65,%si
	jne   L_346d
L_3418:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    $0x2b,%ax
	je   L_3432
	cmp    $0x2d,%si
	jne   L_3431
	movw   $0xffff,-0x26(%bp)
	jmp   L_3432
L_3431:
	dec    %di
L_3432:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	/* TODO: data ref: mov    0x788f(%si),%al */
	mov    D_788f(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_3456
	mov    -0x20(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	lea    -0x30(%si),%dx
	add    %dx,%ax
	mov    %ax,-0x20(%bp)
	jmp   L_3432
L_3456:
	cmpw   $0x0,-0x26(%bp)
	jge   L_3464
	mov    -0x20(%bp),%ax
	neg    %ax
	mov    %ax,-0x20(%bp)
L_3464:
	mov    -0x22(%bp),%ax
	add    -0x20(%bp),%ax
	mov    %ax,-0x22(%bp)
L_346d:
	movw   $0x1,-0x26(%bp)
	cmpw   $0x0,-0x22(%bp)
	jge   L_3485
	movw   $0xffff,-0x26(%bp)
	mov    -0x22(%bp),%ax
	neg    %ax
	mov    %ax,-0x22(%bp)
L_3485:
	mov    -0x22(%bp),%ax
	imulw  -0x26(%bp)
	add    -0x1e(%bp),%ax
	cmp    $0xffd9,%ax
	jge   L_34a3
	/* TODO: data ref: fldl   0x7876 */
	fldl   D_7876
	fstpl  -0xc(%bp)
	movw   $0x0,-0x22(%bp)
L_34a3:
	/* TODO: data ref: fldl   0x787e */
	fldl   D_787e
	fstpl  -0x14(%bp)
	/* TODO: data ref: fldl   0x7886 */
	fldl   D_7886
	fstpl  -0x1c(%bp)
	mov    -0x22(%bp),%ax
	mov    %ax,-0x28(%bp)
L_34bd:
	testw  $0x1,-0x22(%bp)
	je   L_34d2
	fldl   -0x14(%bp)
	fmull  -0x1c(%bp)
	fstpl  -0x14(%bp)
L_34d2:
	sarw   -0x22(%bp)
	cmpw   $0x0,-0x22(%bp)
	je   L_34eb
	fldl   -0x1c(%bp)
	fmull  -0x1c(%bp)
	fstpl  -0x1c(%bp)
	jmp   L_34bd
L_34eb:
	cmpw   $0x0,-0x26(%bp)
	jge   L_3501
	fldl   -0xc(%bp)
	fdivl  -0x14(%bp)
	fstpl  -0xc(%bp)
	jmp   L_350f
L_3501:
	fldl   -0xc(%bp)
	fmull  -0x14(%bp)
	fstpl  -0xc(%bp)
L_350f:
	mov    -0x26(%bp),%ax
	imulw  -0x28(%bp)
	push   %ax
	fldl   -0xc(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4d58
	fstpl  -0x30(%bp)
	add    $0xa,%sp
	fldl   -0x30(%bp)
	fstpl  -0xc(%bp)
	cmpw   $0x0,-0x24(%bp)
	jge   L_3553
	fldl   -0xc(%bp)
	fchs
	fstl   -0x30(%bp)
	fstpl  -0xc(%bp)
L_3553:
	fldl   -0xc(%bp)
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3560:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_3577
	mov    $0xffff,%ax
	jmp   L_3583
L_3577:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_3585
	mov    $0xffff,%ax
L_3583:
	jmp   L_35fd
L_3585:
	cmpw   $0x0,0x4(%di)
	jne   L_35b9
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_35a0
	mov    0x7(%di),%al
	cbtw
	/* TODO: data ref: add    $0x7910,%ax */
	add    $D_7910,%ax
	mov    %ax,0x4(%di)
	jmp   L_3585
L_35a0:
	mov    $0x200,%ax
	push   %ax
	call   L_48fe
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_35b5
	orb    $0x4,0x6(%di)
	jmp   L_3585
L_35b5:
	orb    $0x8,0x6(%di)
L_35b9:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_35cc
	mov    $0x1,%ax
	jmp   L_35cf
L_35cc:
	mov    $0x200,%ax
L_35cf:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4be6
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_35ff
	cmpw   $0xffff,0x2(%di)
	jne   L_35f1
	orb    $0x10,0x6(%di)
	jmp   L_35f5
L_35f1:
	orb    $0x20,0x6(%di)
L_35f5:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_35fd:
	jmp   L_3609
L_35ff:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_3609:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_360e:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall setuid(23) */
	mov    0x4(%bp),%ax
	/* setuid -> __NR_setuid */
	xchg	%ax,%bx
	mov	$__NR_setuid,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_361f
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_361f:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3622:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x4(%bp)
	je   L_3660
	mov    $0x3e8,%ax
	push   %ax
	call   L_36be
	pop    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    $0x7924,%ax */
	mov    $D_7924,%ax
	push   %ax
	call   L_36e2
	pop    %cx
	or     %ax,%ax
	je   L_3662
	pushw  -0x8(%bp)
	mov    $0xe,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	pushw  -0x6(%bp)
	call   L_36be
	pop    %cx
L_3660:
	jmp   L_36a0
L_3662:
	cmpw   $0x0,-0x6(%bp)
	je   L_3683
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jbe   L_3678
	mov    0x4(%bp),%ax
	sub    %ax,-0x6(%bp)
	jmp   L_3683
L_3678:
	mov    -0x6(%bp),%ax
	mov    %ax,0x4(%bp)
	movw   $0x1,-0x6(%bp)
L_3683:
	mov    $0x56a7,%ax
	push   %ax
	mov    $0xe,%ax
	push   %ax
	call   L_3816
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	pushw  0x4(%bp)
	call   L_36be
	pop    %cx
L_369b:
	call   L_36d2
	jmp   L_369b
L_36a0:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x7924,%ax */
	mov    $D_7924,%ax
	push   %ax
	call   L_3710
	add    $0x4,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_36be:
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
	jns    L_36cf
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_36cf:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_36d2:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall pause(29) */
	/* pause -> __NR_pause */
	mov	$__NR_pause,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_36e0
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_36e0:
	pop    %bp
	ret


L_36e2:
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


L_3710:
	push   %bp
	mov    %sp,%bp
	mov    0x4(%bp),%bx
	mov    0x6(%bp),%ax
	or     %ax,%ax
	jne   L_371e
	inc    %ax
L_371e:
	mov    (%bx),%cx
	jmp   L_3729
L_3722:
	mov    0x0(%bp),%bp
	or     %bp,%bp
	je   L_3734
L_3729:
	cmp    %cx,0x0(%bp)
	jne   L_3722
	mov    -0x2(%bp),%si
	mov    -0x4(%bp),%di
L_3734:
	mov    %cs,%cx
	mov    %ds,%dx
	cmp    %dx,%cx
	je   L_3753
	mov    0x6(%bx),%bp
	or     %bp,%bp
	je   L_3753
	mov    %bp,0x2
	mov    0x8(%bx),%cx
	mov    %cx,0x0(%bp)
	mov    0xa(%bx),%cx
	mov    %cx,0x2(%bp)
L_3753:
	mov    (%bx),%bp
	mov    0x2(%bx),%sp
	pop    %cx
	pop    %cx
	mov    0x4(%bx),%cx
	push   %cx
	ret
	/* .byte 0 (alignment) */


L_3760:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_376b:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_3776
	inc    %si
	jmp   L_376b
L_3776:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_377c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
L_3794:
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_37ca
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_37c5
L_37ad:
	incw   -0x6(%bp)
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_37c0
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	jmp   L_37ad
L_37c0:
	mov    -0x8(%bp),%ax
	jmp   L_37cd
L_37c5:
	incw   -0x6(%bp)
	jmp   L_3794
L_37ca:
	mov    -0x8(%bp),%ax
L_37cd:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_37d4:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall exece(59) */
	mov    0x4(%bp),%ax
	/* TODO: data ref: mov    0x7a92,%cx */
	mov    D_7a92,%cx
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
	jns    L_37ef
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_37ef:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_37f2:
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
	jns    L_3803
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3803:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_3806:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall fork(2) */
	/* fork -> __NR_fork */
	mov	$__NR_fork,%ax
	int	$0x80
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3814
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3814:
	pop    %bp
	ret


L_3816:
	push   %bp
	mov    %sp,%bp
	mov    $0x16,%cx
	mov    0x4(%bp),%ax
	or     %ax,%ax
	jle   L_3859
	cmp    $0x20,%ax
	jg   L_3859
	mov    %ax,%cx
	shl    %ax
	/* TODO: data ref: add    $0x7bda,%ax */
	add    $D_7bda,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    0x6(%bp),%ax
	mov    %ax,(%bx)
	cmp    $0x2,%ax
	jl   L_3845
	mov    $0x6,%ax
	mul    %cx
	add    $0x585c,%ax
L_3845:
	mov    %ax,%dx
	mov    %cx,%ax
	/* Venix syscall signal(48) */
	/* TODO: signal() -> sigaction() */
	xchg	%ax,%bx
	mov	%dx,%cx
	mov	$__NR_signal,%ax
	int	$0x80
	cmp    $0x2,%ax
	jl   L_3856
	pop    %ax
	jmp   L_3857
L_3856:
	pop    %bx
L_3857:
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3860
L_3859:
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	mov    $0xffff,%ax
L_3860:
	pop    %bp
	ret
	push   %ax
	/* TODO: data ref: mov    $0x7bdc,%ax */
	mov    $D_7bdc,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7bde,%ax */
	mov    $D_7bde,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7be0,%ax */
	mov    $D_7be0,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7be2,%ax */
	mov    $D_7be2,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7be4,%ax */
	mov    $D_7be4,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7be6,%ax */
	mov    $D_7be6,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7be8,%ax */
	mov    $D_7be8,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7bea,%ax */
	mov    $D_7bea,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7bec,%ax */
	mov    $D_7bec,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7bee,%ax */
	mov    $D_7bee,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7bf0,%ax */
	mov    $D_7bf0,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7bf2,%ax */
	mov    $D_7bf2,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7bf4,%ax */
	mov    $D_7bf4,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7bf6,%ax */
	mov    $D_7bf6,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7bf8,%ax */
	mov    $D_7bf8,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7bfa,%ax */
	mov    $D_7bfa,%ax
	jmp   L_38c6
	push   %ax
	/* TODO: data ref: mov    $0x7bfc,%ax */
	mov    $D_7bfc,%ax
L_38c6:
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7bfe,%ax */
	mov    $D_7bfe,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c00,%ax */
	mov    $D_7c00,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c02,%ax */
	mov    $D_7c02,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c04,%ax */
	mov    $D_7c04,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c06,%ax */
	mov    $D_7c06,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c08,%ax */
	mov    $D_7c08,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c0a,%ax */
	mov    $D_7c0a,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c0c,%ax */
	mov    $D_7c0c,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c0e,%ax */
	mov    $D_7c0e,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c10,%ax */
	mov    $D_7c10,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c12,%ax */
	mov    $D_7c12,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c14,%ax */
	mov    $D_7c14,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c16,%ax */
	mov    $D_7c16,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c18,%ax */
	mov    $D_7c18,%ax
	jmp   L_3920
	push   %ax
	/* TODO: data ref: mov    $0x7c1a,%ax */
	mov    $D_7c1a,%ax
L_3920:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	/* TODO: data ref: sub    $0x7bda,%ax */
	sub    $D_7bda,%ax
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


L_3936:
	push   %bp
	push   %si
	mov    %sp,%bp
	/* Venix syscall lseek(19) */
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    0xa(%bp),%cx
	mov    0xc(%bp),%si
	/* lseek -> __NR_lseek */
	xchg	%ax,%bx
	xchg	%cx,%dx
	mov	$__NR_lseek,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_3951
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_3951:
	pop    %si
	pop    %bp
	ret


L_3954:
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
	jns    L_3964
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_3970
L_3964:
	mov    0x4(%bp),%bx
	or     %bx,%bx
	je   L_3970
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_3970:
	pop    %bp
	ret


L_3972:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	movb   $0x42,-0x6(%bp)
	mov    0x4(%bp),%ax
	mov    %ax,-0xc(%bp)
	/* TODO: data ref: movw   $0x7fff,-0xa(%bp) */
	movw   $D_7fff,-0xa(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_39c4
	add    $0x6,%sp
	decw   -0xa(%bp)
	jl   L_39ac
	mov    -0xc(%bp),%bx
	incw   -0xc(%bp)
	movb   $0x0,(%bx)
	xor    %ax,%ax
	jmp   L_39b9
L_39ac:
	lea    -0xc(%bp),%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_472a
	add    $0x4,%sp
L_39b9:
	mov    0x4(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_39c4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_39dc:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_39ee
	jmp   L_3f8b
L_39ee:
	cmpb   $0x25,-0x5(%bp)
	je   L_3a01
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_39ff:
	jmp   L_39dc
L_3a01:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_3a2c
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_4638
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3a2c:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_3a45
	incb   -0xa6(%bp)
	inc    %di
L_3a45:
	cmpb   $0x30,(%di)
	jne   L_3a50
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_3a50:
	cmpb   $0x2a,(%di)
	je   L_3a80
	movw   $0x0,-0xe(%bp)
L_3a5a:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_3a96
	cmpw   $0x9,-0xc(%bp)
	jg   L_3a96
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_3a5a
L_3a80:
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
L_3a96:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_3add
	cmpb   $0x2a,(%di)
	je   L_3adf
	movw   $0x0,-0x10(%bp)
L_3aaf:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_3ad9
	cmpw   $0x9,-0xc(%bp)
	jg   L_3ad9
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_3aaf
L_3ad9:
	addw   $0x30,-0xc(%bp)
L_3add:
	jmp   L_3af6
L_3adf:
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
L_3af6:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_3f00
L_3b01:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_3b12
L_3b09:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_3b42
	jmp   L_3b3b
L_3b12:
	cmp    $0x64,%ax
	je   L_3b44
	cmp    $0x6f,%ax
	je   L_3b28
	cmp    $0x75,%ax
	je   L_3b3b
	cmp    $0x78,%ax
	je   L_3b36
	jmp   L_3b09
L_3b28:
	movw   $0x8,-0x12(%bp)
	jmp   L_3b3b
L_3b2f:
	movw   $0x8,-0x12(%bp)
	jmp   L_3b42
L_3b36:
	movw   $0x10,-0x12(%bp)
L_3b3b:
	jmp   L_3b8d
L_3b3d:
	movw   $0x10,-0x12(%bp)
L_3b42:
	jmp   L_3b9d
L_3b44:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_3b61
L_3b54:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_3b61:
	cmpw   $0x0,-0x14(%bp)
	jg   L_3bab
	jl   L_3b6f
	cmpw   $0x0,-0x16(%bp)
	jae   L_3bab
L_3b6f:
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
	jmp   L_3bab
L_3b8d:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_3bab
L_3b9d:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_3bab:
	cmpw   $0x0,-0x14(%bp)
	jne   L_3bb5
	cmpw   $0x0,-0x16(%bp)
L_3bb5:
	jne   L_3bc6
	cmpw   $0x0,-0x10(%bp)
	jne   L_3bc6
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_3bc6:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_3bcd:
	cmpw   $0x0,-0x14(%bp)
	jne   L_3bd7
	cmpw   $0x0,-0x16(%bp)
L_3bd7:
	je   L_3c22
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_40ea
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_3bf8
	mov    $0x30,%ax
	jmp   L_3bfb
L_3bf8:
	mov    $0x57,%ax
L_3bfb:
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
	call   L_3fe2
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_3bcd
L_3c22:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_3c71
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_3c22
L_3c40:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_3c4c:
	cmpw   $0x2,-0xc(%bp)
	jge   L_3c71
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_3c6c
	decw   -0x8(%bp)
L_3c6c:
	incw   -0xc(%bp)
	jmp   L_3c4c
L_3c71:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_3c78:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_3c8f
	movw   $0x0,-0xc(%bp)
L_3c8f:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_3c9e
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_3c9e:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_4638
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3cc1:
	jmp   L_39ff
L_3cc4:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_3cd6
	/* TODO: data ref: movw   $0x7930,-0xa(%bp) */
	movw   $D_7930,-0xa(%bp)
L_3cd6:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_3ce5
	/* TODO: data ref: movw   $0x7fff,-0xc(%bp) */
	movw   $D_7fff,-0xc(%bp)
L_3ce5:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3ceb:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_3c78
	decw   -0xc(%bp)
	jl   L_3c78
	incw   -0x8(%bp)
	jmp   L_3ceb
L_3cfd:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_3d05:
	jmp   L_3cc1
L_3d07:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3d13
	movw   $0x6,-0x10(%bp)
L_3d13:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_42e8
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_3d48
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_3d48:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_3d5d
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_3d75
L_3d5d:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_3d5d
L_3d75:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_3d88
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_3d88:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_3da7
L_3d94:
	decw   -0xc(%bp)
	jl   L_3da7
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_3d94
L_3da7:
	cmpw   $0x0,-0xc(%bp)
	jg   L_3db0
	jmp   L_3c71
L_3db0:
	decw   -0xc(%bp)
	jge   L_3db8
	jmp   L_3c71
L_3db8:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_3db0
L_3dcd:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3ddb
	movw   $0x6,-0x10(%bp)
	jmp   L_3dde
L_3ddb:
	incw   -0x10(%bp)
L_3dde:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_42bc
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_3e13
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_3e13:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_3e1e
	incw   -0x1a(%bp)
L_3e1e:
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
L_3e40:
	decw   -0xc(%bp)
	jle   L_3e5a
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_3e40
L_3e5a:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_3e77
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_3e88
L_3e77:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_3e88:
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
	jmp   L_3c71
L_3eb9:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_3ec5
	movw   $0x6,-0x10(%bp)
L_3ec5:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4140
	add    $0xc,%sp
	add    $0x8,%si
L_3ee3:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_3ee3
	decw   -0x8(%bp)
	jmp   L_3c71
L_3ef4:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_3efd:
	jmp   L_3d05
L_3f00:
	cmp    $0x65,%ax
	jne   L_3f08
	jmp   L_3dcd
L_3f08:
	jg   L_3f49
	cmp    $0x4f,%ax
	jne   L_3f12
	jmp   L_3b28
L_3f12:
	jg   L_3f29
	cmp    $0x25,%ax
	je   L_3ef4
	cmp    $0x44,%ax
	jne   L_3f21
	jmp   L_3b44
L_3f21:
	cmp    $0x4c,%ax
L_3f24:
	jne   L_3f29
	jmp   L_3b01
L_3f29:
	cmp    $0x55,%ax
	jne   L_3f31
	jmp   L_3b8d
L_3f31:
	cmp    $0x58,%ax
	jne   L_3f39
	jmp   L_3b36
L_3f39:
	cmp    $0x63,%ax
	jne   L_3f41
	jmp   L_3c40
L_3f41:
	cmp    $0x64,%ax
	jne   L_3f49
	jmp   L_3b54
L_3f49:
	cmp    $0x6f,%ax
	jne   L_3f51
	jmp   L_3b2f
L_3f51:
	jg   L_3f68
	cmp    $0x66,%ax
	jne   L_3f5b
	jmp   L_3d07
L_3f5b:
	cmp    $0x67,%ax
	jne   L_3f63
	jmp   L_3eb9
L_3f63:
	cmp    $0x6c,%ax
	je   L_3f24
L_3f68:
	cmp    $0x72,%ax
	jne   L_3f70
	jmp   L_3cfd
L_3f70:
	cmp    $0x73,%ax
	jne   L_3f78
	jmp   L_3cc4
L_3f78:
	cmp    $0x75,%ax
	jne   L_3f80
	jmp   L_3b9d
L_3f80:
	cmp    $0x78,%ax
	jne   L_3f88
	jmp   L_3b3d
L_3f88:
	jmp   L_3efd
L_3f8b:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_3faf
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_4638
	add    $0x8,%sp
L_3faf:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	call   L_411e


L_3fb9:
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
	call   L_411e


L_3fe2:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_4000
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_4000:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_4009:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_4009
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_4019:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_4019
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_3fb9
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_405a
	jne   L_404f
	or     $0x0,%ax
	je   L_405a
L_404f:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_405a:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_411e


L_4062:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_407e
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_407e:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_4095
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_4095:
	push   %dx
	push   %ax
	call   L_3fe2
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_40af
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_40af:
	ret
	call   L_411e
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_4062
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_3fb9
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_411e


L_40ea:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_3fe2
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_3fb9
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_411e:
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


L_4140:
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
	call   L_42bc
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_417a
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_417a:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_4181:
	cmpw   $0x0,-0xa(%bp)
	jle   L_419b
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_419b
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_4181
L_419b:
	cmpw   $0x0,-0x8(%bp)
	jl   L_41ac
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_41b8
L_41ac:
	cmpw   $0x0,-0x8(%bp)
	jge   L_41b6
	cmpw   $0xfffd,-0x8(%bp)
L_41b6:
	jge   L_4237
L_41b8:
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
L_41d3:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_41ed
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_41d3
L_41ed:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_4209
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_420f
L_4209:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_420f:
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
	jmp   L_42a7
L_4237:
	cmpw   $0x0,-0x8(%bp)
	jg   L_4259
	cmpb   $0x30,(%di)
	je   L_4248
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_4248:
	cmpw   $0x0,-0x8(%bp)
	jge   L_4259
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_4248
L_4259:
	movw   $0x1,-0xa(%bp)
L_425e:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_4286
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
	jne   L_4281
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_4281:
	incw   -0xa(%bp)
	jmp   L_425e
L_4286:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_42a7
L_428e:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_42a1
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_428e
L_42a1:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_42a7:
	cmpb   $0x2e,-0x1(%si)
	jne   L_42ae
	dec    %si
L_42ae:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_42bc:
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
	call   L_4313
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_42e8:
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
	call   L_4313
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4313:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_4326
	movw   $0x0,0xc(%bp)
L_4326:
	cmpw   $0x4f,0xc(%bp)
	jl   L_4331
	movw   $0x4e,0xc(%bp)
L_4331:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	/* TODO: data ref: mov    $0x7938,%si */
	mov    $D_7938,%si
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x7988 */
	fcompl D_7988
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jae   L_436b
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	fldl   0x4(%bp)
	fchs
	fstl   -0x46(%bp)
	fstpl  0x4(%bp)
L_436b:
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4d72
	fstpl  -0x46(%bp)
	add    $0xa,%sp
	fldl   -0x46(%bp)
	fstpl  0x4(%bp)
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x7990 */
	fcompl D_7990
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jne   L_43ac
	jmp   L_445d
L_43ac:
	/* TODO: data ref: mov    $0x7938,%si */
	mov    $D_7938,%si
L_43af:
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x7998 */
	fcompl D_7998
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jb   L_4416
	lea    -0xc(%bp),%ax
	push   %ax
	fldl   -0xc(%bp)
	/* TODO: data ref: fmull  0x79a0 */
	fmull  D_79a0
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4d72
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	fldl   -0x4e(%bp)
	/* TODO: data ref: fmull  0x79a8 */
	fmull  D_79a8
	/* TODO: data ref: faddl  0x79b0 */
	faddl  D_79b0
	fistps -0x60(%bp)
	fwait
	mov    -0x60(%bp),%ax
	lea    -0x3e(%bp),%dx
	mov    %di,%bx
	shl    %bx
	add    %bx,%dx
	mov    %dx,%bx
	mov    %ax,(%bx)
	inc    %di
	jmp   L_43af
L_4416:
	mov    $0x1,%ax
	push   %ax
	push   %si
	fldl   -0xc(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   L_45c1
	add    $0x6,%sp
	mov    %ax,%si
L_4431:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_4453
	xor    %ax,%ax
	push   %ax
	push   %si
	lea    -0x3e(%bp),%ax
	mov    %di,%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_45c1
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_4431
L_4453:
	mov    %si,%ax
	/* TODO: data ref: sub    $0x7938,%ax */
	sub    $D_7938,%ax
	mov    %ax,%di
	jmp   L_44dd
L_445d:
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x79b8 */
	fcompl D_79b8
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jbe   L_44dd
L_4473:
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x79c0 */
	fmull  D_79c0
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x79c8 */
	fcompl D_79c8
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_44a9
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	sub    $0x3,%di
	jmp   L_4473
L_44a9:
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x79d0 */
	fmull  D_79d0
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x79d8 */
	fcompl D_79d8
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_44dd
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_44a9
L_44dd:
	mov    0xc(%bp),%ax
	/* TODO: data ref: add    $0x7938,%ax */
	add    $D_7938,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_44ef
	add    %di,-0x16(%bp)
L_44ef:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	/* TODO: data ref: cmpw   $0x7938,-0x16(%bp) */
	cmpw   $D_7938,-0x16(%bp)
	jae   L_4505
	/* TODO: data ref: movb   $0x0,0x7938 */
	movb   $0x0,D_7938
	/* TODO: data ref: mov    $0x7938,%ax */
	mov    $D_7938,%ax
	jmp   L_456e
L_4505:
	cmp    -0x16(%bp),%si
	ja   L_455f
	/* TODO: data ref: cmp    $0x7988,%si */
	cmp    $D_7988,%si
	jae   L_455f
	lea    -0x14(%bp),%ax
	push   %ax
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x79e0 */
	fmull  D_79e0
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	fstpl  (%bx)
	call   L_4d72
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
	call   L_45c1
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_4505
L_455f:
	/* TODO: data ref: cmpw   $0x7988,-0x16(%bp) */
	cmpw   $D_7988,-0x16(%bp)
	jb   L_4570
	/* TODO: data ref: movb   $0x0,0x7987 */
	movb   $0x0,D_7987
	/* TODO: data ref: mov    $0x7938,%ax */
	mov    $D_7938,%ax
L_456e:
	jmp   L_45ba
L_4570:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_4578:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_45b4
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	/* TODO: data ref: cmpw   $0x7938,-0x16(%bp) */
	cmpw   $D_7938,-0x16(%bp)
	jbe   L_4597
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_4578
L_4597:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_4578
	/* TODO: data ref: cmp    $0x7938,%si */
	cmp    $D_7938,%si
	jbe   L_45b1
	movb   $0x30,(%si)
L_45b1:
	inc    %si
	jmp   L_4578
L_45b4:
	movb   $0x0,(%si)
	/* TODO: data ref: mov    $0x7938,%ax */
	mov    $D_7938,%ax
L_45ba:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_45c1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%si
	xor    %di,%di
L_45cc:
	or     %si,%si
	je   L_45dd
	mov    %si,%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	mov    %ax,%si
	inc    %di
	jmp   L_45cc
L_45dd:
	cmpw   $0x0,0x8(%bp)
	jne   L_45f6
	mov    %di,%si
L_45e5:
	cmp    $0x4,%si
	jge   L_45f6
	mov    0x6(%bp),%bx
	movb   $0x30,(%bx)
	incw   0x6(%bp)
	inc    %si
	jmp   L_45e5
L_45f6:
	mov    0x6(%bp),%ax
	add    %di,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%si
L_4606:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_462f
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
	jmp   L_4606
L_462f:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4638:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_4645:
	cmpw   $0x0,0x8(%bp)
	jl   L_464e
	jmp   L_46dc
L_464e:
	cmpb   $0x2d,(%di)
	jne   L_468b
	cmpw   $0x30,0xc(%bp)
	jne   L_468b
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_467a
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
	jmp   L_468a
L_467a:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_472a
	add    $0x4,%sp
L_468a:
	dec    %si
L_468b:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_46a9
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
	jmp   L_46b5
L_46a9:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_472a
	add    $0x4,%sp
L_46b5:
	incw   0x8(%bp)
	jmp   L_4645
L_46ba:
	dec    %si
	jl   L_46f0
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_46de
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
L_46dc:
	jmp   L_46ba
L_46de:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_472a
	add    $0x4,%sp
	jmp   L_46ba
L_46f0:
	cmpw   $0x0,0x8(%bp)
	je   L_4725
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_4714
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
	jmp   L_4720
L_4714:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_472a
	add    $0x4,%sp
L_4720:
	decw   0x8(%bp)
	jmp   L_46f0
L_4725:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_472a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_4745
	mov    $0xffff,%ax
	jmp   L_480c
L_4745:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_4778
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
	call   L_4d3e
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_47fd
L_4778:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_47ca
	/* TODO: data ref: cmp    $0x79f0,%di */
	cmp    $D_79f0,%di
	jne   L_47a3
	/* TODO: data ref: mov    0x79f7,%al */
	mov    D_79f7,%al
	cbtw
	push   %ax
	call   L_4ba8
	pop    %cx
	or     %ax,%ax
	je   L_4798
	orb    $0x4,0x6(%di)
	jmp   L_4745
L_4798:
	/* TODO: data ref: movw   $0x7c22,0x4(%di) */
	movw   $D_7c22,0x4(%di)
	/* TODO: data ref: movw   $0x7c22,(%di) */
	movw   $D_7c22,(%di)
	jmp   L_4745
L_47a3:
	mov    $0x200,%ax
	push   %ax
	call   L_48fe
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_47ba
	orb    $0x4,0x6(%di)
	jmp   L_4745
L_47ba:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_47ec
L_47ca:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_47ec
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4d3e
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_47ec:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_47fd:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_480e
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_480c:
	jmp   L_4811
L_480e:
	mov    0x4(%bp),%ax
L_4811:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4818:
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
	jne   L_4864
	mov    0x4(%di),%si
	or     %si,%si
	je   L_4864
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_4864
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4d3e
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_4864
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_4866
L_4864:
	xor    %ax,%ax
L_4866:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_486b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x79e8,%di */
	mov    $D_79e8,%di
L_4873:
	/* TODO: data ref: cmp    0x7a88,%di */
	cmp    D_7a88,%di
	jae   L_4883
	push   %di
	call   L_4887
	pop    %cx
	add    $0x8,%di
	jmp   L_4873
L_4883:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4887:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_48da
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_48da
	push   %di
	call   L_4818
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_4b94
	pop    %cx
	or     %ax,%ax
	jge   L_48bc
	mov    $0xffff,%si
L_48bc:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_48cc
	pushw  0x4(%di)
	call   L_4a48
	pop    %cx
L_48cc:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_48da
	movw   $0x0,0x4(%di)
L_48da:
	andb   $0x80,0x6(%di)
	movw   $0x0,0x2(%di)
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */

	/* exit: provided by TLVC libc — elided */

L_48fe:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: cmpw   $0x0,0x7a8a */
	cmpw   $0x0,D_7a8a
	jne   L_4929
	/* TODO: data ref: mov    $0x7a8c,%ax */
	mov    $D_7a8c,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x7a8a */
	mov    %ax,D_7a8a
	/* TODO: data ref: mov    $0x7a8a,%ax */
	mov    $D_7a8a,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x7a8c */
	mov    %ax,D_7a8c
	/* TODO: data ref: movw   $0x7a8c,-0x7fdc */
	movw   $D_7a8c,-D_7fdc
	/* TODO: data ref: movw   $0x7a8a,-0x7fde */
	movw   $D_7a8a,-D_7fde
L_4929:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    -0x7fde,%di */
	mov    -D_7fde,%di
L_493d:
	/* TODO: data ref: movw   $0x0,0x7a8e */
	movw   $0x0,D_7a8e
L_4943:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_4972
L_494a:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_4959
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_494a
L_4959:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_4972
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_4972
	jmp   L_4a19
L_4972:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_4943
	/* TODO: data ref: cmp    -0x7fdc,%si */
	cmp    -D_7fdc,%si
	jne   L_498b
	/* TODO: data ref: cmp    $0x7a8a,%di */
	cmp    $D_7a8a,%di
	je   L_498f
L_498b:
	xor    %ax,%ax
	jmp   L_49c7
L_498f:
	/* TODO: data ref: incw   0x7a8e */
	incw   D_7a8e
	/* TODO: data ref: cmpw   $0x1,0x7a8e */
	cmpw   $0x1,D_7a8e
	jle   L_4943
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	/* TODO: data ref: mov    %ax,0x7a8e */
	mov    %ax,D_7a8e
	xor    %ax,%ax
	push   %ax
	call   L_4b2c
	pop    %cx
	mov    %ax,%si
	/* TODO: data ref: mov    0x7a8e,%ax */
	mov    D_7a8e,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_49c9
	xor    %ax,%ax
L_49c7:
	jmp   L_49dc
L_49c9:
	/* TODO: data ref: mov    0x7a8e,%ax */
	mov    D_7a8e,%ax
	shl    %ax
	push   %ax
	call   L_4b2c
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_49de
	xor    %ax,%ax
L_49dc:
	jmp   L_4a43
L_49de:
	/* TODO: data ref: mov    -0x7fdc,%bx */
	mov    -D_7fdc,%bx
	mov    %si,(%bx)
	/* TODO: data ref: mov    0x8024,%ax */
	mov    D_8024,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_49fc
	/* TODO: data ref: mov    -0x7fdc,%bx */
	mov    -D_7fdc,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    -0x7fdc,%bx */
	mov    -D_7fdc,%bx
	mov    %ax,(%bx)
L_49fc:
	/* TODO: data ref: mov    0x7a8e,%ax */
	mov    D_7a8e,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	/* TODO: data ref: mov    %ax,0x8024 */
	mov    %ax,D_8024
	/* TODO: data ref: mov    $0x7a8a,%ax */
	mov    $D_7a8a,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    -0x7fdc,%bx */
	mov    -D_7fdc,%bx
	mov    %ax,(%bx)
	jmp   L_493d
L_4a19:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	/* TODO: data ref: mov    %ax,0x8022 */
	mov    %ax,D_8022
	cmp    %ax,%si
	jbe   L_4a38
	/* TODO: data ref: mov    -0x7fde,%bx */
	mov    -D_7fde,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x8026 */
	mov    %ax,D_8026
	/* TODO: data ref: mov    -0x7fde,%bx */
	mov    -D_7fde,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_4a38:
	/* TODO: data ref: mov    0x8022,%ax */
	mov    D_8022,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_4a43:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4a48:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	/* TODO: data ref: mov    %si,-0x7fde */
	mov    %si,-D_7fde
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
	je   L_4a7d
	push   %di
	call   L_4a48
	pop    %cx
L_4a7d:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_48fe
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_4a9c
	cmp    %di,%si
	jne   L_4aa0
L_4a9c:
	mov    %si,%ax
	jmp   L_4b08
L_4aa0:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_4abf
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_4abf:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_4add
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_4abf
L_4add:
	cmp    %di,%si
	jae   L_4b06
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_4b06
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
	/* TODO: data ref: mov    0x8026,%ax */
	mov    D_8026,%ax
	mov    %ax,(%bx)
L_4b06:
	mov    %si,%ax
L_4b08:
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
	jns    L_4b23
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_4b2a
L_4b23:
	mov    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x7a90 */
	mov    %bx,D_7a90
L_4b2a:
	pop    %bp
	ret


L_4b2c:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall brk(17) */
	mov    0x4(%bp),%ax
	/* TODO: data ref: add    0x7a90,%ax */
	add    D_7a90,%ax
	/* brk -> __NR_brk */
	xchg	%ax,%bx
	mov	$__NR_brk,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_4b43
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
	jmp   L_4b50
L_4b43:
	/* TODO: data ref: mov    0x7a90,%bx */
	mov    D_7a90,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x7a90 */
	mov    %bx,D_7a90
L_4b50:
	pop    %bp
	ret


L_4b52:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_4b5f:
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_4b7e
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_4b5f
	xor    %ax,%ax
	jmp   L_4b8f
L_4b7e:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_4b8f:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4b94:
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
	jns    L_4ba5
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4ba5:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4ba8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4c8d
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_4bc5
	xor    %ax,%ax
	jmp   L_4bc8
L_4bc5:
	mov    $0x1,%ax
L_4bc8:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4bd0:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall open(5) */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* open: Venix has no mode arg; supply mode=0 for TLVC */
	xchg	%ax,%bx
	mov	%dx,%cx
	xor	%dx,%dx
	mov	$__NR_open,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	test	%ax,%ax
	/* errno: jcxz -> jns (jump past errno-store if ax>=0) */
	jns    L_4be4
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4be4:
	pop    %bp
	ret


L_4be6:
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
	jns    L_4bfd
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4bfd:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4c00:
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
	jns    L_4c14
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4c14:
	pop    %bp
	ret


L_4c16:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x6(%bp)
L_4c27:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_4c27
	dec    %di
L_4c30:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_4c30
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4c4a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x6(%bp)
L_4c5b:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_4c5b
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	/* TODO: data ref: mov    $0x7409,%ax */
	mov    $D_7409,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4ca6
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4c8d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	/* TODO: data ref: mov    $0x7408,%ax */
	mov    $D_7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4ca6
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4ca6:
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
	jns    L_4cbd
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4cbd:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4cc0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x6(%bp),%di
	cmpw   $0xffff,0x4(%bp)
	jne   L_4cd4
	mov    $0xffff,%ax
	jmp   L_4d0a
L_4cd4:
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	je   L_4ce4
	mov    (%di),%ax
	cmp    0x4(%di),%ax
	ja   L_4cfa
L_4ce4:
	mov    (%di),%ax
	cmp    0x4(%di),%ax
	jne   L_4cf5
	cmpw   $0x0,0x2(%di)
	jne   L_4cf5
	incw   (%di)
	jmp   L_4cfa
L_4cf5:
	mov    $0xffff,%ax
	jmp   L_4d0a
L_4cfa:
	incw   0x2(%di)
	decw   (%di)
	mov    (%di),%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	xor    %ax,%ax
L_4d0a:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4d0e:
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
	jns    L_4d1f
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4d1f:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4d22:
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
	jns    L_4d32
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4d30:
	pop    %bp
	ret
L_4d32:
	mov    0x4(%bp),%bx
	cmp    $0x0,%bx
	je   L_4d30
	mov    %dx,(%bx)
	jmp   L_4d30


L_4d3e:
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
	jns    L_4d55
	neg	%ax
	mov	%ax,D_7bd2	/* errno = -ax */
	mov	$0xffff,%ax	/* return -1 on error */
L_4d55:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4d58:
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


L_4d72:
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
	mov	%ax,D_7bd2	/* errno */
	mov	$0xffff,%ax
	jmp	fstat_ret
fstat_ok:
	/* translate TLVC stat -> Venix stat in caller buffer */
	mov	0x6(%bp),%di	/* di = venix_stat_buf */
	lea	-0x20(%bp),%si	/* si = tlvc_stat_buf */
	/* st_dev: same offset +0 */
	mov	(%si),%ax
	mov	%ax,(%di)
	/* st_ino: TLVC +2 (long->short, take low word) -> Venix +2 */
	mov	0x2(%si),%ax
	mov	%ax,0x2(%di)
	/* st_mode: TLVC +6 -> Venix +4 */
	mov	0x6(%si),%ax
	mov	%ax,0x4(%di)
	/* st_nlink: TLVC +8 -> Venix +6 */
	mov	0x8(%si),%ax
	mov	%ax,0x6(%di)
	/* st_uid: TLVC +10 -> Venix +8 */
	mov	0xa(%si),%ax
	mov	%ax,0x8(%di)
	/* st_gid: TLVC +12 -> Venix +10 */
	mov	0xc(%si),%ax
	mov	%ax,0xa(%di)
	/* st_rdev: TLVC +14 -> Venix +12 */
	mov	0xe(%si),%ax
	mov	%ax,0xc(%di)
	/* st_size: TLVC +16 (long) -> Venix +14 (long) */
	mov	0x10(%si),%ax
	mov	%ax,0xe(%di)
	mov	0x12(%si),%ax
	mov	%ax,0x10(%di)
	/* st_atime: TLVC +20 -> Venix +18 */
	mov	0x14(%si),%ax
	mov	%ax,0x12(%di)
	mov	0x16(%si),%ax
	mov	%ax,0x14(%di)
	/* st_mtime: TLVC +24 -> Venix +22 */
	mov	0x18(%si),%ax
	mov	%ax,0x16(%di)
	mov	0x1a(%si),%ax
	mov	%ax,0x18(%di)
	/* st_ctime: TLVC +28 -> Venix +26 */
	mov	0x1c(%si),%ax
	mov	%ax,0x1a(%di)
	mov	0x1e(%si),%ax
	mov	%ax,0x1c(%di)
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
D_6d90:
	.byte	0xbf, 0x0f	/* |..| */
	.word	D_6daa	/* ptr: 0x6daa */
D_6d94:
	.word	D_6db8	/* ptr: 0x6db8 */
D_6d96:
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_6d9a:
	.word	D_6dc8	/* ptr: 0x6dc8 */
	.word	D_6ddc	/* ptr: 0x6ddc */
	.byte	0x00, 0x00	/* |..| */
	.word	D_6df3	/* ptr: 0x6df3 */
	.word	D_6e0b	/* ptr: 0x6e0b */
	.byte	0x00, 0x00, 0x00, 0x00, 0xb0, 0x04	/* |......| */
D_6daa:
	.ascii	"/usr/lib/uucp\0"
D_6db8:
	.ascii	"/usr/spool/uucp\0"
D_6dc8:
	.ascii	"/usr/lib/uucp/L.sys\0"
D_6ddc:
	.ascii	"/usr/lib/uucp/L.sys.cr\0"
D_6df3:
	.ascii	"/usr/lib/uucp/L-devices\0"
D_6e0b:
	.ascii	"/usr/lib/uucp/L-dialcodes\0"
D_6e25:
	.byte	0x75, 0x6e, 0x6b, 0x6e, 0x6f, 0x77, 0x6e, 0x20	/* |unknown | */
	.byte	0x66, 0x6c, 0x61, 0x67, 0x20, 0x25, 0x73, 0x0a	/* |flag %s.| */
	.byte	0x00	/* |.| */
D_6e36:
	.byte	0x0a, 0x0a, 0x2a, 0x2a, 0x20, 0x25, 0x73, 0x20	/* |..** %s | */
	.byte	0x2a, 0x2a, 0x0a, 0x00	/* |**..| */
D_6e42:
	.ascii	"START\0"
D_6e48:
	.ascii	"arg - %s:\0"
D_6e52:
	.byte	0x20, 0x00	/* | .| */
D_6e54:
	.byte	0x61, 0x72, 0x67, 0x20, 0x2d, 0x20, 0x25, 0x73	/* |arg - %s| */
	.byte	0x0a, 0x00	/* |..| */
D_6e5e:
	.ascii	"AERROR - (%s) \0"
D_6e6d:
	.ascii	"ret == 0\0"
D_6e76:
	.ascii	"GWD FAILED %d\0"
D_6e84:
	.ascii	"%.7s\0"
D_6e89:
	.byte	0x77, 0x00	/* |w.| */
D_6e8b:
	.ascii	"AERROR - (%s) \0"
D_6e9a:
	.ascii	"fprx != NULL\0"
D_6ea7:
	.ascii	"CAN'T OPEN %s\0"
D_6eb5:
	.byte	0x77, 0x00	/* |w.| */
D_6eb7:
	.ascii	"AERROR - (%s) \0"
D_6ec6:
	.ascii	"fpc != NULL\0"
D_6ed2:
	.ascii	"CAN'T OPEN %s\0"
D_6ee0:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73	/* |%c %s %s| */
	.byte	0x0a, 0x00	/* |..| */
D_6eea:
	.byte	0x78, 0x73, 0x79, 0x73, 0x20, 0x25, 0x73, 0x0a	/* |xsys %s.| */
	.byte	0x00	/* |.| */
D_6ef3:
	.byte	0x62, 0x61, 0x64, 0x20, 0x73, 0x79, 0x73, 0x74	/* |bad syst| */
	.byte	0x65, 0x6d, 0x20, 0x6e, 0x61, 0x6d, 0x65, 0x3a	/* |em name:| */
	.byte	0x20, 0x25, 0x73, 0x0a, 0x00	/* | %s..| */
D_6f08:
	.byte	0x77, 0x00	/* |w.| */
D_6f0a:
	.ascii	"AERROR - (%s) \0"
D_6f19:
	.ascii	"fpd != NULL\0"
D_6f25:
	.ascii	"CAN'T OPEN %s\0"
D_6f33:
	.byte	0x53, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73, 0x20	/* |S %s %s | */
	.byte	0x25, 0x73, 0x20, 0x2d, 0x20, 0x25, 0x73, 0x20	/* |%s - %s | */
	.byte	0x30, 0x36, 0x36, 0x36, 0x0a, 0x00	/* |0666..| */
D_6f49:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |%c %s..| */
D_6f50:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |%c %s..| */
D_6f57:
	.byte	0x70, 0x72, 0x6d, 0x20, 0x2d, 0x20, 0x25, 0x73	/* |prm - %s| */
	.byte	0x0a, 0x00	/* |..| */
D_6f61:
	.ascii	"s - %s, \0"
D_6f6a:
	.ascii	"r - %s, \0"
D_6f73:
	.byte	0x72, 0x65, 0x74, 0x20, 0x2d, 0x20, 0x25, 0x64	/* |ret - %d| */
	.byte	0x0a, 0x00	/* |..| */
D_6f7d:
	.ascii	"file s- %s, \0"
D_6f8a:
	.byte	0x6c, 0x6f, 0x63, 0x61, 0x6c, 0x20, 0x2d, 0x20	/* |local - | */
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_6f96:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73	/* |%c %s %s| */
	.byte	0x0a, 0x00	/* |..| */
D_6fa0:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |%c %s..| */
D_6fa7:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |%c %s..| */
D_6fae:
	.byte	0x72, 0x65, 0x73, 0x74, 0x20, 0x25, 0x73, 0x0a	/* |rest %s.| */
	.byte	0x00	/* |.| */
D_6fb7:
	.byte	0x00	/* |.| */
D_6fb8:
	.byte	0x70, 0x65, 0x72, 0x6d, 0x69, 0x73, 0x73, 0x69	/* |permissi| */
	.byte	0x6f, 0x6e, 0x20, 0x64, 0x65, 0x6e, 0x69, 0x65	/* |on denie| */
	.byte	0x64, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |d %s..| */
D_6fce:
	.byte	0x53, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73, 0x20	/* |S %s %s | */
	.byte	0x25, 0x73, 0x20, 0x2d, 0x20, 0x25, 0x73, 0x20	/* |%s - %s | */
	.byte	0x30, 0x36, 0x36, 0x36, 0x0a, 0x00	/* |0666..| */
D_6fe4:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73	/* |%c %s %s| */
	.byte	0x0a, 0x00	/* |..| */
D_6fee:
	.byte	0x77, 0x00	/* |w.| */
D_6ff0:
	.ascii	"AERROR - (%s) \0"
D_6fff:
	.ascii	"fp != NULL\0"
D_700a:
	.ascii	"CAN'T OPEN %s\0"
D_7018:
	.byte	0x52, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73, 0x20	/* |R %s %s | */
	.byte	0x25, 0x73, 0x20, 0x2d, 0x20, 0x0a, 0x00	/* |%s - ..| */
D_7027:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73	/* |%c %s %s| */
	.byte	0x0a, 0x00	/* |..| */
D_7031:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |%c %s..| */
D_7038:
	.byte	0x77, 0x00	/* |w.| */
D_703a:
	.ascii	"AERROR - (%s) \0"
D_7049:
	.ascii	"fpd != NULL\0"
D_7055:
	.ascii	"CAN'T OPEN %s\0"
D_7063:
	.byte	0x52, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73, 0x20	/* |R %s %s | */
	.byte	0x25, 0x73, 0x20, 0x2d, 0x20, 0x0a, 0x00	/* |%s - ..| */
D_7072:
	.byte	0x53, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73, 0x20	/* |S %s %s | */
	.byte	0x25, 0x73, 0x20, 0x2d, 0x20, 0x25, 0x73, 0x20	/* |%s - %s | */
	.byte	0x30, 0x36, 0x36, 0x36, 0x0a, 0x00	/* |0666..| */
D_7088:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73	/* |%c %s %s| */
	.byte	0x0a, 0x00	/* |..| */
D_7092:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |%c %s..| */
D_7099:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |%c %s..| */
D_70a0:
	.byte	0x25, 0x63, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |%c %s..| */
D_70a7:
	.byte	0x00	/* |.| */
D_70a8:
	.byte	0x53, 0x20, 0x25, 0x73, 0x20, 0x25, 0x73, 0x20	/* |S %s %s | */
	.byte	0x25, 0x73, 0x20, 0x2d, 0x20, 0x25, 0x73, 0x20	/* |%s - %s | */
	.byte	0x30, 0x36, 0x36, 0x36, 0x0a, 0x00	/* |0666..| */
D_70be:
	.byte	0x65, 0x78, 0x69, 0x74, 0x20, 0x63, 0x6f, 0x64	/* |exit cod| */
	.byte	0x65, 0x20, 0x25, 0x64, 0x0a, 0x00	/* |e %d..| */
D_70cc:
	.word	D_7770	/* ptr: 0x7770 */
	.byte	0x64, 0x00	/* |d.| */
D_70d0:
	.byte	0x72, 0x00	/* |r.| */
D_70d2:
	.byte	0x72, 0x00	/* |r.| */
D_70d4:
	.byte	0x77, 0x00	/* |w.| */
D_70d6:
	.byte	0x2f, 0x00	/* |/.| */
D_70d8:
	.ascii	"mkdir %s\0"
D_70e1:
	.byte	0x6d, 0x6b, 0x64, 0x69, 0x72, 0x20, 0x2d, 0x20	/* |mkdir - | */
	.byte	0x25, 0x73, 0x0a, 0x00, 0x00	/* |%s...| */
D_70ee:
	.ascii	"%c.%.7s%c%.4s\0"
D_70fc:
	.byte	0x66, 0x69, 0x6c, 0x65, 0x20, 0x2d, 0x20, 0x25	/* |file - %| */
	.byte	0x73, 0x0a, 0x00	/* |s..| */
D_7107:
	.ascii	"LCK.SEQL\0"
D_7110:
	.ascii	"AERROR - (%s) \0"
D_711f:
	.ascii	"n < SLOCKTRIES\0"
D_712e:
	.ascii	"CAN NOT GET %s\0"
D_713d:
	.ascii	"LCK.SEQL\0"
D_7146:
	.ascii	"/usr/spool/uucp/SEQF\0"
D_715b:
	.byte	0x72, 0x00	/* |r.| */
D_715d:
	.byte	0x25, 0x34, 0x64, 0x00	/* |%4d.| */
D_7161:
	.ascii	"/usr/spool/uucp/SEQF\0"
D_7176:
	.byte	0x77, 0x00	/* |w.| */
D_7178:
	.ascii	"AERROR - (%s) \0"
D_7187:
	.ascii	"fp != NULL\0"
D_7192:
	.ascii	"CAN NOT OPEN %s\0"
D_71a2:
	.ascii	"/usr/spool/uucp/SEQF\0"
D_71b7:
	.ascii	"/usr/spool/uucp/SEQF\0"
D_71cc:
	.ascii	"/usr/spool/uucp/SEQF\0"
D_71e1:
	.byte	0x77, 0x00	/* |w.| */
D_71e3:
	.ascii	"/usr/spool/uucp/SEQF\0"
D_71f8:
	.word	D_7325	/* ptr: 0x7325 */
	.byte	0x00	/* |.| */
D_71fb:
	.ascii	"%04d\0"
D_7200:
	.ascii	"LCK.SEQL\0"
	.byte	0x00	/* |.| */
D_720a:
	.ascii	"/dev/null\0"
D_7214:
	.ascii	"/dev/null\0"
D_721e:
	.byte	0x73, 0x68, 0x69, 0x6f, 0x20, 0x2d, 0x20, 0x25	/* |shio - %| */
	.byte	0x73, 0x0a, 0x00	/* |s..| */
D_7229:
	.ascii	"AERROR - (%s) \0"
D_7238:
	.ascii	"f == 0\0"
D_723f:
	.ascii	"BAD OPEN fileno %d\0"
D_7252:
	.ascii	"AERROR - (%s) \0"
D_7261:
	.ascii	"f == 1\0"
D_7268:
	.ascii	"BAD OPEN fileno %d\0"
D_727b:
	.ascii	"/bin/sh\0"
D_7283:
	.byte	0x73, 0x68, 0x00	/* |sh.| */
D_7286:
	.byte	0x2d, 0x63, 0x00	/* |-c.| */
D_7289:
	.byte	0x73, 0x74, 0x61, 0x74, 0x75, 0x73, 0x20, 0x25	/* |status %| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_7294:
	.byte	0xff, 0xff	/* |..| */
D_7296:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_72a6:
	.byte	0x00, 0x00	/* |..| */
D_72a8:
	.ascii	"LTMP.%d\0"
D_72b0:
	.ascii	"AERROR - (%s) \0"
D_72bf:
	.ascii	"ret != -1\0"
D_72c9:
	.ascii	"LOCK PROBLEM - %s\0"
D_72db:
	.ascii	"AERROR - (%s) \0"
D_72ea:
	.ascii	"ret != -1\0"
D_72f4:
	.ascii	"LOCK PROBLEM - %s\0"
D_7306:
	.ascii	"AERROR - (%s) \0"
D_7315:
	.ascii	"ret == 0\0"
D_731e:
	.byte	0x4c, 0x4f, 0x43, 0x4b, 0x20, 0x50, 0x52	/* |LOCK PR| */
D_7325:
	.ascii	"OBLEM - %s\0"
D_7330:
	.ascii	"AERROR - (%s) \0"
D_733f:
	.ascii	"i < MAXLOCKS\0"
D_734c:
	.ascii	"TOO MANY LOCKS %d\0"
D_735e:
	.ascii	"AERROR - (%s) \0"
D_736d:
	.ascii	"p != NULL\0"
D_7377:
	.ascii	"CAN NOT ALLOCATE FOR %s\0"
D_738f:
	.ascii	"%s.%s\0"
D_7395:
	.ascii	"LCK.\0"
D_739a:
	.ascii	"%s.%s\0"
D_73a0:
	.ascii	"LCK.\0"
	.byte	0x00	/* |.| */
D_73a6:
	.ascii	"/dev/null\0"
D_73b0:
	.ascii	"/dev/null\0"
D_73ba:
	.ascii	"/dev/null\0"
D_73c4:
	.ascii	"-s%.7s\0"
D_73cb:
	.ascii	"/usr/lib/uucp/uucico\0"
D_73e0:
	.ascii	"UUCICO\0"
D_73e7:
	.byte	0x2d, 0x72, 0x31, 0x00	/* |-r1.| */
D_73eb:
	.ascii	"/dev/null\0"
D_73f5:
	.ascii	"/dev/null\0"
D_73ff:
	.byte	0x2f, 0x64, 0x65, 0x76, 0x2f, 0x6e, 0x75, 0x6c	/* |/dev/nul| */
	.byte	0x6c	/* |l| */
D_7408:
	.byte	0x00	/* |.| */
D_7409:
	.ascii	"/usr/lib/uucp/uuxqt\0"
D_741d:
	.ascii	"UUXQT\0"
D_7423:
	.ascii	"/dev/null\0"
D_742d:
	.ascii	"/dev/null\0"
D_7437:
	.ascii	"/dev/null\0"
D_7441:
	.ascii	"%s -r %s\0"
D_744a:
	.ascii	"usr/lib/uucp/uucp\0"
D_745c:
	.ascii	"/bin/sh\0"
D_7464:
	.byte	0x73, 0x68, 0x00	/* |sh.| */
D_7467:
	.byte	0x2d, 0x63, 0x00	/* |-c.| */
D_746a:
	.byte	0x00, 0x00	/* |..| */
D_746c:
	.byte	0x00, 0x00	/* |..| */
D_746e:
	.byte	0x00, 0x00	/* |..| */
D_7470:
	.byte	0x01, 0x00	/* |..| */
D_7472:
	.ascii	"/usr/lib/uucp\0"
D_7480:
	.ascii	"AERROR - (%s) \0"
D_748f:
	.ascii	"ret == 0\0"
D_7498:
	.ascii	"INIT USERFILE %d\0"
D_74a9:
	.byte	0x2e, 0x2e, 0x2f, 0x00	/* |../.| */
D_74ad:
	.ascii	"/usr/lib/uucp/USERFILE\0"
D_74c4:
	.byte	0x72, 0x00	/* |r.| */
D_74c6:
	.byte	0x63, 0x00	/* |c.| */
D_74c8:
	.ascii	"AERROR - (%s) \0"
D_74d7:
	.ascii	"ret == 0\0"
D_74e0:
	.ascii	"INIT USERFILE %d\0"
	.byte	0x00	/* |.| */
D_74f2:
	.ascii	"%.7s\0"
D_74f7:
	.ascii	"%.7s\0"
D_74fc:
	.byte	0x72, 0x00	/* |r.| */
D_74fe:
	.ascii	"/usr/include/ident.h\0"
D_7513:
	.byte	0x4e, 0x6f, 0x20, 0x73, 0x79, 0x73, 0x74, 0x65	/* |No syste| */
	.byte	0x6d, 0x20, 0x6e, 0x61, 0x6d, 0x65, 0x20, 0x69	/* |m name i| */
	.byte	0x6e, 0x20, 0x2f, 0x75, 0x73, 0x72, 0x2f, 0x69	/* |n /usr/i| */
	.byte	0x6e, 0x63, 0x6c, 0x75, 0x64	/* |nclud| */
D_7530:
	.byte	0x65, 0x2f, 0x69, 0x64, 0x65, 0x6e, 0x74, 0x2e	/* |e/ident.| */
	.byte	0x68, 0x2e, 0x0a, 0x00	/* |h...| */
D_753c:
	.byte	0x44, 0x65, 0x66, 0x61, 0x75, 0x6c, 0x74, 0x69	/* |Defaulti| */
	.byte	0x6e, 0x67, 0x20, 0x74, 0x6f, 0x20, 0x76, 0x65	/* |ng to ve| */
	.byte	0x6e, 0x69, 0x78, 0x0a, 0x00	/* |nix..| */
D_7551:
	.ascii	"venix\0"
	.byte	0x00	/* |.| */
D_7558:
	.ascii	"/etc/passwd\0"
D_7564:
	.byte	0x00, 0x00	/* |..| */
D_7566:
	.byte	0x00, 0x00	/* |..| */
D_7568:
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
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
D_776a:
	.byte	0x00, 0x00	/* |..| */
D_776c:
	.byte	0x00, 0x00	/* |..| */
D_776e:
	.byte	0x00, 0x00	/* |..| */
D_7770:
	.byte	0x00, 0x00	/* |..| */
D_7772:
	.byte	0x00, 0x00	/* |..| */
D_7774:
	.byte	0x00, 0x00	/* |..| */
D_7776:
	.byte	0x00, 0x00	/* |..| */
D_7778:
	.byte	0x00, 0x00	/* |..| */
D_777a:
	.byte	0x00, 0x00	/* |..| */
D_777c:
	.byte	0x72, 0x00	/* |r.| */
D_777e:
	.byte	0x72, 0x00	/* |r.| */
D_7780:
	.byte	0x00, 0x00	/* |..| */
D_7782:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_77aa:
	.ascii	"/bin/sh\0"
D_77b2:
	.byte	0x73, 0x68, 0x00	/* |sh.| */
D_77b5:
	.byte	0x2d, 0x63, 0x00	/* |-c.| */
D_77b8:
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
	.byte	0x6a, 0x51, 0x76, 0x51, 0x82, 0x51, 0xa0, 0x51	/* |jQvQ.Q.Q| */
	.byte	0xa0, 0x51, 0xa0, 0x51, 0xa0, 0x51, 0xa0, 0x51	/* |.Q.Q.Q.Q| */
	.byte	0xa0, 0x51, 0xa0, 0x51, 0xa0, 0x51, 0xa0, 0x51	/* |.Q.Q.Q.Q| */
	.byte	0xa0, 0x51, 0xa0, 0x51, 0xa0, 0x51, 0xa0, 0x51	/* |.Q.Q.Q.Q| */
	.byte	0x2e, 0x51, 0x2e, 0x51, 0x4c, 0x51	/* |.Q.QLQ| */
D_785e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_7866:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_786e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_7876:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_787e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_7886:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x14, 0x40	/* |.......@| */
	.byte	0x00	/* |.| */
D_788f:
	.byte	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |        | */
	.byte	0x20, 0x08, 0x08, 0x08, 0x08, 0x08, 0x20, 0x20	/* | .....  | */
	.byte	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |        | */
	.byte	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20	/* |        | */
	.byte	0x08, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04	/* |........| */
	.byte	0x04, 0x04, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x01	/* |.AAAAAA.| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01	/* |........| */
	.byte	0x01, 0x01, 0x01, 0x10, 0x10, 0x10, 0x10, 0x10	/* |........| */
	.byte	0x10, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x02	/* |.BBBBBB.| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02	/* |........| */
	.byte	0x02, 0x02, 0x02, 0x10, 0x10, 0x10, 0x10, 0x20	/* |....... | */
	.byte	0x00	/* |.| */
D_7910:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_7924:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_7930:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_7938:
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
D_7987:
	.byte	0x00	/* |.| */
D_7988:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_7990:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_7998:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_79a0:
	.byte	0x2c, 0x43, 0x1c, 0xeb, 0xe2, 0x36, 0x1a, 0x3f	/* |,C...6.?| */
D_79a8:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_79b0:
	.byte	0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0xb9, 0x3f	/* |.......?| */
D_79b8:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_79c0:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x8f, 0x40	/* |.....@.@| */
D_79c8:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_79d0:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_79d8:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_79e0:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_79e8:
	.word	D_7e22	/* ptr: 0x7e22 */
	.byte	0x00, 0x00	/* |..| */
	.word	D_7e22	/* ptr: 0x7e22 */
	.byte	0x01, 0x00	/* |..| */
D_79f0:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_79f6:
	.byte	0x02	/* |.| */
D_79f7:
	.byte	0x01	/* |.| */
D_79f8:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |........| */
D_7a00:
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
D_7a88:
	.word	D_7a88	/* ptr: 0x7a88 */
D_7a8a:
	.byte	0x00, 0x00	/* |..| */
D_7a8c:
	.byte	0x00, 0x00	/* |..| */
D_7a8e:
	.byte	0x00, 0x00	/* |..| */
D_7a90:
	.word	D_8028	/* ptr: 0x8028 */

	.bss

D_7a92:	.space	2	/* bss word @ 0x7a92 */
D_7a94:	.space	2	/* bss word @ 0x7a94 */
	.space	4
D_7a9a:	.space	2	/* bss word @ 0x7a9a */
	.space	8
D_7aa4:	.space	2	/* bss word @ 0x7aa4 */
	.space	30
D_7ac4:	.space	2	/* bss word @ 0x7ac4 */
	.space	4
D_7aca:	.space	2	/* bss word @ 0x7aca */
	.space	102
D_7b32:	.space	2	/* bss word @ 0x7b32 */
	.space	22
D_7b4a:	.space	2	/* bss word @ 0x7b4a */
	.space	118
D_7bc2:	.space	2	/* bss word @ 0x7bc2 */
	.space	14
D_7bd2:	.space	2	/* bss word @ 0x7bd2 */
	.space	6
D_7bda:	.space	2	/* bss word @ 0x7bda */
D_7bdc:	.space	2	/* bss word @ 0x7bdc */
D_7bde:	.space	2	/* bss word @ 0x7bde */
D_7be0:	.space	2	/* bss word @ 0x7be0 */
D_7be2:	.space	2	/* bss word @ 0x7be2 */
D_7be4:	.space	2	/* bss word @ 0x7be4 */
D_7be6:	.space	2	/* bss word @ 0x7be6 */
D_7be8:	.space	2	/* bss word @ 0x7be8 */
D_7bea:	.space	2	/* bss word @ 0x7bea */
D_7bec:	.space	2	/* bss word @ 0x7bec */
D_7bee:	.space	2	/* bss word @ 0x7bee */
D_7bf0:	.space	2	/* bss word @ 0x7bf0 */
D_7bf2:	.space	2	/* bss word @ 0x7bf2 */
D_7bf4:	.space	2	/* bss word @ 0x7bf4 */
D_7bf6:	.space	2	/* bss word @ 0x7bf6 */
D_7bf8:	.space	2	/* bss word @ 0x7bf8 */
D_7bfa:	.space	2	/* bss word @ 0x7bfa */
D_7bfc:	.space	2	/* bss word @ 0x7bfc */
D_7bfe:	.space	2	/* bss word @ 0x7bfe */
D_7c00:	.space	2	/* bss word @ 0x7c00 */
D_7c02:	.space	2	/* bss word @ 0x7c02 */
D_7c04:	.space	2	/* bss word @ 0x7c04 */
D_7c06:	.space	2	/* bss word @ 0x7c06 */
D_7c08:	.space	2	/* bss word @ 0x7c08 */
D_7c0a:	.space	2	/* bss word @ 0x7c0a */
D_7c0c:	.space	2	/* bss word @ 0x7c0c */
D_7c0e:	.space	2	/* bss word @ 0x7c0e */
D_7c10:	.space	2	/* bss word @ 0x7c10 */
D_7c12:	.space	2	/* bss word @ 0x7c12 */
D_7c14:	.space	2	/* bss word @ 0x7c14 */
D_7c16:	.space	2	/* bss word @ 0x7c16 */
D_7c18:	.space	2	/* bss word @ 0x7c18 */
D_7c1a:	.space	2	/* bss word @ 0x7c1a */
	.space	6
D_7c22:	.space	2	/* bss word @ 0x7c22 */
	.space	510
D_7e22:	.space	2	/* bss word @ 0x7e22 */
	.space	440
D_7fdc:	.space	2	/* bss word @ 0x7fdc */
D_7fde:	.space	2	/* bss word @ 0x7fde */
	.space	31
D_7fff:	.space	2	/* bss word @ 0x7fff */
	.space	33
D_8022:	.space	2	/* bss word @ 0x8022 */
D_8024:	.space	2	/* bss word @ 0x8024 */
D_8026:	.space	2	/* bss word @ 0x8026 */
D_8028:	/* endcore: initial break */

/* end of file */
