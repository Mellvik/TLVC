/* as.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 as.s -o as.o
 * Syscalls used:
 *   1:exit            3:read            4:write           5:open            6:close           8:creat
 *   10:unlink         17:brk            19:lseek          54:ioctl
 * INT instructions: $0xf1  $0xf2  $0xf3  $0xf4
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: as  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00006426  (25638 bytes)
//   a_data:   0x0000222c  (8748 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

L_0036:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_0042
	int    $0xf2
L_0042:
	/* TODO: data ref: mov    $0x242c,%ax */
	mov    $D_042c,%ax
	push   %ax
	mov    $D_2192,%ax
	push   %ax
	call   L_4196
	add    $0x4,%sp
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	mov    $D_2192,%ax
	push   %ax
	call   L_4196
	add    $0x6,%sp
	call   L_4098
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0067:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_0073
	int    $0xf2
L_0073:
	mov    0x4(%bp),%di
	/* TODO: data ref: cmpw   $0x2,0x2472 */
	cmpw   $0x2,D_0472
	jne   L_00a0
	/* TODO: data ref: incw   0x205e */
	incw   D_005e
	mov    %di,%bx
	shl    %bx
	/* TODO: data ref: pushw  0x2004(%bx) */
	pushw  D_0004(%bx)
	pushw  D_222e
	mov    $D_2234,%ax
	push   %ax
	/* TODO: data ref: mov    $0x243f,%ax */
	mov    $D_043f,%ax
	push   %ax
	mov    $D_2192,%ax
	push   %ax
	call   L_4196
	add    $0xa,%sp
L_00a0:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_00a4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_00b0
	int    $0xf2
L_00b0:
	/* TODO: data ref: cmpw   $0x2,0x2472 */
	cmpw   $0x2,D_0472
	jne   L_00d6
	mov    0x4(%bp),%bx
	shl    %bx
	/* TODO: data ref: pushw  0x2004(%bx) */
	pushw  D_0004(%bx)
	pushw  D_222e
	mov    $D_2234,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2452,%ax */
	mov    $D_0452,%ax
	push   %ax
	mov    $D_2192,%ax
	push   %ax
	call   L_4196
	add    $0xa,%sp
L_00d6:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_00da:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	cmp    $0x7f,%sp
	ja   L_00e9
	int    $0xf2
L_00e9:
	addw   $0x2,0x6(%bp)
L_00ed:
	decw   0x4(%bp)
	jg   L_00f5
	jmp   L_0223
L_00f5:
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	cmpb   $0x2d,(%bx)
	jne   L_0150
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	mov    0x1(%bx),%al
	cbtw
	jmp   L_0145
L_010a:
	/* TODO: data ref: incb   0x2470 */
	incb   D_0470
	mov    $D_0e70,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	mov    $D_229c,%ax
	push   %ax
	call   L_038e
	add    $0x6,%sp
	addw   $0x2,0x6(%bp)
	decw   0x4(%bp)
	jmp   L_0165
L_012b:
	mov    0x6(%bp),%bx
	mov    (%bx),%bx
	mov    0x1(%bx),%al
	cbtw
	push   %ax
	mov    $D_0e71,%ax
	push   %ax
	mov    $D_2192,%ax
	push   %ax
	call   L_4196
	add    $0x6,%sp
	jmp   L_0165
L_0145:
	or     %ax,%ax
	je   L_0165
	cmp    $0x6f,%ax
	je   L_010a
	jmp   L_012b
L_0150:
	/* TODO: data ref: cmpw   $0x0,0x28ac */
	cmpw   $0x0,D_08ac
	je   L_0168
	mov    $D_0e8f,%ax
	push   %ax
	mov    $D_2192,%ax
	push   %ax
	call   L_4196
	add    $0x4,%sp
L_0165:
	jmp   L_021c
L_0168:
	mov    0x6(%bp),%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x28ac */
	mov    %ax,D_08ac
	mov    $D_0eaa,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  (%bx)
	mov    $D_2234,%ax
	push   %ax
	call   L_038e
	add    $0x6,%sp
	mov    $D_2182,%ax
	push   %ax
	mov    $D_0ead,%ax
	push   %ax
	mov    $D_2234,%ax
	push   %ax
	call   L_40f6
	add    $0x6,%sp
	or     %ax,%ax
	jne   L_0200
	mov    $0x2e,%ax
	push   %ax
	/* TODO: data ref: pushw  0x28ac */
	pushw  D_08ac
	call   L_03ec
	add    $0x4,%sp
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	je   L_01bf
	mov    $D_0eaf,%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_623c
	add    $0x4,%sp
	or     %ax,%ax
	je   L_01d9
L_01bf:
	mov    $D_2234,%ax
	push   %ax
	mov    $D_0eb2,%ax
	push   %ax
	mov    $D_2192,%ax
	push   %ax
	call   L_4196
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_01d9:
	mov    $0x64,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  (%bx)
	mov    $D_2234,%ax
	push   %ax
	call   L_041f
	add    $0x6,%sp
	mov    $D_2182,%ax
	push   %ax
	mov    $D_0ece,%ax
	push   %ax
	mov    $D_2234,%ax
	push   %ax
	call   L_40f6
	add    $0x6,%sp
	or     %ax,%ax
L_0200:
	jne   L_021c
	mov    $D_2234,%ax
	push   %ax
	mov    $D_0ed0,%ax
	push   %ax
	mov    $D_2192,%ax
	push   %ax
	call   L_4196
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_021c:
	addw   $0x2,0x6(%bp)
	jmp   L_00ed
L_0223:
	/* TODO: data ref: cmpb   $0x0,0x2470 */
	cmpb   $0x0,D_0470
	jne   L_0262
	mov    $0x2e,%ax
	push   %ax
	/* TODO: data ref: pushw  0x28ac */
	pushw  D_08ac
	call   L_03ec
	add    $0x4,%sp
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	je   L_0250
	mov    $D_0eec,%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_623c
	add    $0x4,%sp
	or     %ax,%ax
	je   L_0264
L_0250:
	mov    $D_0eef,%ax
	push   %ax
	/* TODO: data ref: pushw  0x28ac */
	pushw  D_08ac
	mov    $D_229c,%ax
	push   %ax
	call   L_038e
	add    $0x6,%sp
L_0262:
	jmp   L_0299
L_0264:
	/* TODO: data ref: mov    0x28ac,%ax */
	mov    D_08ac,%ax
	mov    %ax,-0x6(%bp)
	movw   $D_229c,-0x8(%bp)
L_026f:
	mov    -0x6(%bp),%ax
	cmp    -0xa(%bp),%ax
	jae   L_028c
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	incw   -0x8(%bp)
	jmp   L_026f
L_028c:
	mov    $D_0ef2,%ax
	push   %ax
	pushw  -0x8(%bp)
	call   L_62ea
	add    $0x4,%sp
L_0299:
	mov    $D_0ef5,%ax
	push   %ax
	mov    $D_229c,%ax
	push   %ax
	call   L_4224
	add    $0x4,%sp
	mov    %ax,D_2300
	or     %ax,%ax
	jne   L_02c4
	mov    $D_229c,%ax
	push   %ax
	mov    $D_0ef7,%ax
	push   %ax
	call   L_44ec
	add    $0x4,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_02c4:
	pushw  D_2300
	call   L_5f71
	pop    %cx
	call   L_2fb0
	mov    $D_0f15,%ax
	push   %ax
	call   L_3052
	pop    %cx
	mov    %ax,D_2298
	mov    %ax,%bx
	mov    D_1c6a,%ax
	mov    %ax,0x4(%bx)
	mov    D_2298,%bx
	movw   $0xb,0xc(%bx)
	call   L_0355
	call   L_02fe
	call   L_3347
	call   L_22c6
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_02fe:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmp    $0x7f,%sp
	ja   L_030b
	int    $0xf2
L_030b:
	/* TODO: data ref: mov    $0x2480,%di */
	mov    $D_0480,%di
L_030e:
	cmpw   $0x0,(%di)
	je   L_0350
	mov    $0x6,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_40a0
	add    $0x4,%sp
	mov    %ax,%si
	mov    (%di),%ax
	mov    %ax,0x2(%si)
	mov    0x2(%di),%ax
	mov    %ax,0x4(%si)
	pushw  0x2(%si)
	call   L_38a9
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	shl    %bx
	mov    D_2304(%bx),%ax
	mov    %ax,(%si)
	mov    -0x6(%bp),%bx
	shl    %bx
	mov    %si,D_2304(%bx)
	add    $0x4,%di
	jmp   L_030e
L_0350:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0355:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_0361
	int    $0xf2
L_0361:
	/* TODO: data ref: mov    $0x28ae,%si */
	mov    $D_08ae,%si
L_0364:
	cmpw   $0x0,(%si)
	je   L_038a
	pushw  (%si)
	call   L_3052
	pop    %cx
	mov    %ax,%di
	mov    0x2(%si),%ax
	cwtd
	mov    %ax,0x6(%di)
	mov    %dx,0x8(%di)
	movw   $0x0,0x4(%di)
	movw   $0x23,0xc(%di)
	add    $0x4,%si
	jmp   L_0364
L_038a:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_038e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmp    $0x7f,%sp
	ja   L_039b
	int    $0xf2
L_039b:
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    0xc(%di),%ax
	mov    %ax,-0x6(%bp)
L_03a9:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	je   L_03c8
	cmpb   $0x2f,-0x1(%di)
	jne   L_03a9
	lea    0xc(%di),%ax
	mov    %ax,-0x6(%bp)
	jmp   L_03a9
L_03c8:
	cmp    -0x6(%bp),%di
	jbe   L_03d2
	mov    -0x6(%bp),%di
	jmp   L_03d3
L_03d2:
	dec    %di
L_03d3:
	mov    0x8(%bp),%bx
	incw   0x8(%bp)
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_03d3
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_03ec:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_03f8
	int    $0xf2
L_03f8:
	mov    0x4(%bp),%di
	xor    %si,%si
L_03fe:
	mov    (%di),%al
	cbtw
	mov    0x6(%bp),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_0411
	mov    %di,%si
L_0411:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_03fe
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_041f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	cmp    $0x7f,%sp
	ja   L_042e
	int    $0xf2
L_042e:
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
L_043e:
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_0474
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_046f
L_0457:
	incw   -0x6(%bp)
	mov    -0x6(%bp),%ax
	cmp    0x8(%bp),%ax
	jge   L_046a
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	jmp   L_0457
L_046a:
	mov    -0x8(%bp),%ax
	jmp   L_0477
L_046f:
	incw   -0x6(%bp)
	jmp   L_043e
L_0474:
	mov    -0x8(%bp),%ax
L_0477:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_047e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmp    $0x7f,%sp
	ja   L_048b
	int    $0xf2
L_048b:
	movb   $0x1,D_256a
	xor    %di,%di
L_0492:
	cmp    $0xc,%di
	jge   L_049f
	movb   $0x0,D_256c(%di)
	inc    %di
	jmp   L_0492
L_049f:
	mov    0x4(%bp),%ax
	jmp   L_145b
	mov    $0xffff,%ax
	push   %ax
	call   L_1a58
	pop    %cx
	jmp   L_04ec
	mov    $0xffff,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x4,%ax
	push   %ax
	mov    $0x80,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
	jmp   L_050c
	mov    $0xffff,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x14,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1080,%ax */
	mov    $D_1080,%ax
	push   %ax
	mov    $0x10,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_04ec:
	jmp   L_052c
	mov    $0xffff,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x2c,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2880,%ax */
	mov    $D_0880,%ax
	push   %ax
	mov    $0x28,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_050c:
	jmp   L_054c
	mov    $0xffff,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1c,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1880,%ax */
	mov    $D_1880,%ax
	push   %ax
	mov    $0x18,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_052c:
	jmp   L_056b
	mov    $0xffff,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x3c,%ax
	push   %ax
	mov    $D_1880,%ax
	push   %ax
	mov    $0x38,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_054c:
	jmp   L_0589
	mov    $0xffff,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x24,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2080,%ax */
	mov    $D_0080,%ax
	push   %ax
	mov    $0x20,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_056b:
	jmp   L_05a8
	mov    $0xffff,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $0xa8,%ax
	push   %ax
	mov    $0xf6,%ax
	push   %ax
	mov    $0x84,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_0589:
	jmp   L_05c7
	mov    $0xffff,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0xc,%ax
	push   %ax
	mov    $0x880,%ax
	push   %ax
	mov    $0x8,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_05a8:
	jmp   L_05e5
	mov    $0xffff,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x34,%ax
	push   %ax
	mov    $D_1080,%ax
	push   %ax
	mov    $0x30,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_05c7:
	jmp   L_0604
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x4,%ax
	push   %ax
	mov    $0x80,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_05e5:
	jmp   L_0623
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x14,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1080,%ax */
	mov    $D_1080,%ax
	push   %ax
	mov    $0x10,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_0604:
	jmp   L_0642
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x2c,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2880,%ax */
	mov    $D_0880,%ax
	push   %ax
	mov    $0x28,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_0623:
	jmp   L_0660
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1c,%ax
	push   %ax
	/* TODO: data ref: mov    $0x1880,%ax */
	mov    $D_1880,%ax
	push   %ax
	mov    $0x18,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_0642:
	jmp   L_067d
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x24,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2080,%ax */
	mov    $D_0080,%ax
	push   %ax
	mov    $0x20,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_0660:
	jmp   L_069b
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $0xa8,%ax
	push   %ax
	mov    $0xf6,%ax
	push   %ax
	mov    $0x84,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_067d:
	jmp   L_06b9
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0xc,%ax
	push   %ax
	mov    $0x880,%ax
	push   %ax
	mov    $0x8,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_069b:
	jmp   L_06d7
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x34,%ax
	push   %ax
	mov    $D_1080,%ax
	push   %ax
	mov    $0x30,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_06b9:
	jmp   L_06f5
	mov    $0x8d,%ax
	push   %ax
	call   L_19f4
	pop    %cx
	jmp   L_06ff
	mov    $0xc5,%ax
	push   %ax
	call   L_19f4
	pop    %cx
	jmp   L_0709
	mov    $0xc4,%ax
	push   %ax
	call   L_19f4
	pop    %cx
L_06d7:
	jmp   L_0713
	mov    $0xeb,%ax
	push   %ax
	call   L_17ef
	pop    %cx
	jmp   L_071d
	mov    $0x74,%ax
	push   %ax
	call   L_17ef
	pop    %cx
	jmp   L_0727
	mov    $0x7c,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_06f5:
	jmp   L_0731
	mov    $0x7e,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_06ff:
	jmp   L_073b
	mov    $0x72,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_0709:
	jmp   L_0745
	mov    $0x76,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_0713:
	jmp   L_074f
	mov    $0x7a,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_071d:
	jmp   L_0759
	mov    $0x70,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_0727:
	jmp   L_0763
	mov    $0x78,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_0731:
	jmp   L_076d
	mov    $0x75,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_073b:
	jmp   L_0777
	mov    $0x7d,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_0745:
	jmp   L_0781
	mov    $0x7f,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_074f:
	jmp   L_078b
	mov    $0x73,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_0759:
	jmp   L_0795
	mov    $0x77,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_0763:
	jmp   L_079f
	mov    $0x7b,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_076d:
	jmp   L_07a9
	mov    $0x71,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_0777:
	jmp   L_07a9
	mov    $0x79,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_0781:
	jmp   L_07b9
	mov    $0xe2,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_078b:
	jmp   L_07c9
	mov    $0xe1,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_0795:
	jmp   L_07d9
	mov    $0xe0,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_079f:
	jmp   L_07d9
	mov    $0xe3,%ax
	push   %ax
	call   L_17ef
	pop    %cx
L_07a9:
	jmp   L_07e9
	mov    $0x75,%ax
	push   %ax
	mov    $0x74,%ax
	push   %ax
	call   L_1867
	add    $0x4,%sp
L_07b9:
	jmp   L_07f9
	mov    $0x74,%ax
	push   %ax
	mov    $0x75,%ax
	push   %ax
	call   L_1867
	add    $0x4,%sp
L_07c9:
	jmp   L_0809
	mov    $0x7f,%ax
	push   %ax
	mov    $0x7e,%ax
	push   %ax
	call   L_1867
	add    $0x4,%sp
L_07d9:
	jmp   L_0819
	mov    $0x7e,%ax
	push   %ax
	mov    $0x7f,%ax
	push   %ax
	call   L_1867
	add    $0x4,%sp
L_07e9:
	jmp   L_0829
	mov    $0x7d,%ax
	push   %ax
	mov    $0x7c,%ax
	push   %ax
	call   L_1867
	add    $0x4,%sp
L_07f9:
	jmp   L_0839
	mov    $0x7c,%ax
	push   %ax
	mov    $0x7d,%ax
	push   %ax
	call   L_1867
	add    $0x4,%sp
L_0809:
	jmp   L_0849
	mov    $0x77,%ax
	push   %ax
	mov    $0x76,%ax
	push   %ax
	call   L_1867
	add    $0x4,%sp
L_0819:
	jmp   L_0858
	mov    $0x76,%ax
	push   %ax
	mov    $0x77,%ax
	push   %ax
	call   L_1867
	add    $0x4,%sp
L_0829:
	jmp   L_086c
	mov    $0x73,%ax
	push   %ax
	mov    $0x72,%ax
	push   %ax
	call   L_1867
	add    $0x4,%sp
L_0839:
	jmp   L_0876
	mov    $0x72,%ax
	push   %ax
	mov    $0x73,%ax
	push   %ax
	call   L_1867
	add    $0x4,%sp
L_0849:
	jmp   L_0880
	xor    %ax,%ax
	push   %ax
	mov    $0xeb,%ax
	push   %ax
	call   L_1867
	add    $0x4,%sp
L_0858:
	jmp   L_0894
	mov    $0xd7,%ax
	push   %ax
	call   L_17c7
	pop    %cx
	jmp   L_089e
	mov    $0x9f,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_086c:
	jmp   L_08a8
	mov    $0x9e,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_0876:
	jmp   L_08b2
	mov    $0x9c,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_0880:
	jmp   L_08bc
	mov    $0x9d,%ax
	push   %ax
	call   L_17c7
	pop    %cx
	jmp   L_08c6
	mov    $0x37,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_0894:
	jmp   L_08cf
	mov    $0x27,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_089e:
	jmp   L_08d9
	mov    $0x3f,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_08a8:
	jmp   L_08e3
	mov    $0x2f,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_08b2:
	jmp   L_08e3
	mov    $0x98,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_08bc:
	jmp   L_08e3
	mov    $0x99,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_08c6:
	jmp   L_0902
	xor    %ax,%ax
	push   %ax
	call   L_1a58
	pop    %cx
L_08cf:
	jmp   L_090c
	mov    $0xa4,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_08d9:
	jmp   L_0916
	mov    $0xa5,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_08e3:
	jmp   L_0920
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $0x3c,%ax
	push   %ax
	mov    $D_1880,%ax
	push   %ax
	mov    $0x38,%ax
	push   %ax
	call   L_1c1e
	add    $0xc,%sp
L_0902:
	jmp   L_093e
	mov    $0xa6,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_090c:
	jmp   L_0948
	mov    $0xa7,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_0916:
	jmp   L_0952
	mov    $0xae,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_0920:
	jmp   L_095c
	mov    $0xaf,%ax
	push   %ax
	call   L_17c7
	pop    %cx
	jmp   L_0966
	mov    $0xac,%ax
	push   %ax
	call   L_17c7
	pop    %cx
	jmp   L_0970
	mov    $0xad,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_093e:
	jmp   L_097a
	mov    $0xaa,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_0948:
	jmp   L_0984
	mov    $0xab,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_0952:
	jmp   L_098e
	mov    $0xce,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_095c:
	jmp   L_0998
	mov    $0x90cf,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_0966:
	jmp   L_09a2
	mov    $0xf8,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_0970:
	jmp   L_09ac
	mov    $0xf5,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_097a:
	jmp   L_09b6
	mov    $0xf9,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_0984:
	jmp   L_09c0
	mov    $0xfc,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_098e:
	jmp   L_09ca
	mov    $0xfd,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_0998:
	jmp   L_09d4
	mov    $0xfa,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_09a2:
	jmp   L_09de
	mov    $0xfb,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_09ac:
	jmp   L_09de
	mov    $0xf4,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_09b6:
	jmp   L_09ef
	mov    $0x9b,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_09c0:
	jmp   L_09ef
	mov    $0xf0,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_09ca:
	jmp   L_0a0f
	mov    $0xf2,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_09d4:
	jmp   L_0a0f
	mov    $0xf3,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_09de:
	jmp   L_0a0f
	cmpw   $0x0,D_261c
	je   L_09f1
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_09ef:
	jmp   L_0a0f
L_09f1:
	movb   $0x2,D_256a
	cmpw   $0x3,0x4(%bp)
	jne   L_0a01
	mov    $0xd4,%ax
	jmp   L_0a04
L_0a01:
	mov    $0xd5,%ax
L_0a04:
	mov    %ax,%cx
	mov    %cl,D_256c
	movb   $0xa,D_256d
L_0a0f:
	jmp   L_0a4a
	movw   $0x0,-0x6(%bp)
	cmpw   $0x76,0x4(%bp)
	jne   L_0a21
	mov    $0x1,%ax
	jmp   L_0a23
L_0a21:
	xor    %ax,%ax
L_0a23:
	mov    %ax,%di
	jmp   L_0a3b
	movw   $0xffff,-0x6(%bp)
	cmpw   $0x1c,0x4(%bp)
	jne   L_0a37
	mov    $0x1,%ax
	jmp   L_0a39
L_0a37:
	xor    %ax,%ax
L_0a39:
	mov    %ax,%di
L_0a3b:
	cmpw   $0x1,D_261c
	je   L_0a4c
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_0a4a:
	jmp   L_0a8b
L_0a4c:
	mov    $D_257c,%ax
	push   %ax
	call   L_162d
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	jl   L_0a75
	cmp    $0x8,%si
	jge   L_0a75
	or     %di,%di
	je   L_0a68
	mov    $0x40,%ax
	jmp   L_0a6b
L_0a68:
	mov    $0x48,%ax
L_0a6b:
	or     %si,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	jmp   L_0abb
L_0a75:
	pushw  -0x6(%bp)
	or     %di,%di
	je   L_0a81
	mov    $0xfe,%ax
	jmp   L_0a84
L_0a81:
	mov    $0x8fe,%ax
L_0a84:
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0a8b:
	jmp   L_0acb
	mov    $0xffff,%ax
	push   %ax
	/* TODO: data ref: mov    $0x10f6,%ax */
	mov    $D_10f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
	jmp   L_0adb
	mov    $0xffff,%ax
	push   %ax
	/* TODO: data ref: mov    $0x18f6,%ax */
	mov    $D_18f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
	jmp   L_0aeb
	mov    $0xffff,%ax
	push   %ax
	/* TODO: data ref: mov    $0x20f6,%ax */
	mov    $D_00f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0abb:
	jmp   L_0afb
	mov    $0xffff,%ax
	push   %ax
	/* TODO: data ref: mov    $0x28f6,%ax */
	mov    $D_08f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0acb:
	jmp   L_0b0a
	mov    $0xffff,%ax
	push   %ax
	mov    $D_10f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0adb:
	jmp   L_0b19
	mov    $0xffff,%ax
	push   %ax
	mov    $D_18f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0aeb:
	jmp   L_0b28
	mov    $0xffff,%ax
	push   %ax
	mov    $0xd800,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0afb:
	jmp   L_0b37
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x10f6,%ax */
	mov    $D_10f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0b0a:
	jmp   L_0b46
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x18f6,%ax */
	mov    $D_18f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0b19:
	jmp   L_0b55
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x20f6,%ax */
	mov    $D_00f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0b28:
	jmp   L_0b66
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x28f6,%ax */
	mov    $D_08f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0b37:
	jmp   L_0b66
	xor    %ax,%ax
	push   %ax
	mov    $D_10f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0b46:
	jmp   L_0b66
	xor    %ax,%ax
	push   %ax
	mov    $D_18f6,%ax
	push   %ax
	call   L_1d9f
	add    $0x4,%sp
L_0b55:
	jmp   L_0b66
	cmpw   $0x1,D_261c
	je   L_0b68
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_0b66:
	jmp   L_0bb9
L_0b68:
	cmpw   $0x4c,0x4(%bp)
	jne   L_0b73
	mov    $0x1,%ax
	jmp   L_0b75
L_0b73:
	xor    %ax,%ax
L_0b75:
	mov    %ax,%di
	mov    $D_257c,%ax
	push   %ax
	call   L_162d
	pop    %cx
	mov    %ax,%si
	mov    $0x1,%ax
	push   %ax
	call   L_15a8
	pop    %cx
	cmp    $0xffff,%si
	jne   L_0bbb
	or     %di,%di
	je   L_0b97
	mov    $0xff,%ax
	jmp   L_0b9a
L_0b97:
	mov    $0x8f,%ax
L_0b9a:
	mov    %ax,%cx
	mov    %cl,D_256c
	or     %di,%di
	je   L_0ba9
	mov    $0x30,%ax
	jmp   L_0bab
L_0ba9:
	xor    %ax,%ax
L_0bab:
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
L_0bb9:
	jmp   L_0c00
L_0bbb:
	cmp    $0x10,%si
	jl   L_0be6
	cmp    $0x13,%si
	jg   L_0be6
	or     %di,%di
	je   L_0bce
	mov    $0x6,%ax
	jmp   L_0bd1
L_0bce:
	mov    $0x7,%ax
L_0bd1:
	mov    %si,%dx
	and    $0x3,%dx
	mov    $0x3,%cx
	shl    %cl,%dx
	or     %dx,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	jmp   L_0c11
L_0be6:
	or     %di,%di
	je   L_0bef
	mov    $0x50,%ax
	jmp   L_0bf2
L_0bef:
	mov    $0x58,%ax
L_0bf2:
	mov    %si,%dx
	and    $0x7,%dx
	or     %dx,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
L_0c00:
	jmp   L_0c11
	cmpw   $0x2,D_261c
	je   L_0c13
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_0c11:
	jmp   L_0c4f
L_0c13:
	mov    $D_257c,%ax
	push   %ax
	call   L_162d
	pop    %cx
	mov    %ax,%di
	mov    $D_2586,%ax
	push   %ax
	call   L_162d
	pop    %cx
	mov    %ax,%si
	mov    $0xffff,%ax
	push   %ax
	call   L_15a8
	pop    %cx
	mov    %ax,-0x6(%bp)
	movb   $0x90,D_256c
	or     %di,%di
	jne   L_0c51
	or     %si,%si
	jl   L_0c51
	cmp    $0x8,%si
	jge   L_0c51
	mov    %si,%ax
	and    $0x7,%ax
	mov    %ax,%cx
	or     %cl,D_256c
L_0c4f:
	jmp   L_0c98
L_0c51:
	or     %si,%si
	jne   L_0c6b
	or     %di,%di
	jl   L_0c6b
	cmp    $0x8,%di
	jge   L_0c6b
	mov    %di,%ax
	and    $0x7,%ax
	mov    %ax,%cx
	or     %cl,D_256c
	jmp   L_0c98
L_0c6b:
	or     %di,%di
	jl   L_0c9a
	cmp    $0x10,%di
	jge   L_0c9a
	mov    -0x6(%bp),%ax
	or     $0x86,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	mov    $D_2586,%ax
	push   %ax
	call   L_1681
	pop    %cx
	mov    %di,%ax
	and    $0x7,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	mov    %ax,%cx
	or     %cl,D_256d
L_0c98:
	jmp   L_0ce1
L_0c9a:
	or     %si,%si
	jl   L_0cc9
	cmp    $0x10,%si
	jge   L_0cc9
	mov    -0x6(%bp),%ax
	or     $0x86,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
	mov    %si,%ax
	and    $0x7,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	mov    %ax,%cx
	or     %cl,D_256d
	jmp   L_0d01
L_0cc9:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_0d11
	mov    $0xec,%ax
	push   %ax
	mov    $0xe4,%ax
	push   %ax
	call   L_1d29
	add    $0x4,%sp
L_0ce1:
	jmp   L_0d21
	mov    $0xed,%ax
	push   %ax
	mov    $0xe5,%ax
	push   %ax
	call   L_1d29
	add    $0x4,%sp
	jmp   L_0d31
	mov    $0xee,%ax
	push   %ax
	mov    $0xe6,%ax
	push   %ax
	call   L_1d29
	add    $0x4,%sp
L_0d01:
	jmp   L_0d41
	mov    $0xef,%ax
	push   %ax
	mov    $0xe7,%ax
	push   %ax
	call   L_1d29
	add    $0x4,%sp
L_0d11:
	jmp   L_0d51
	mov    $0xcc,%ax
	push   %ax
	mov    $0xcd,%ax
	push   %ax
	call   L_1d29
	add    $0x4,%sp
L_0d21:
	jmp   L_0d61
	mov    $0xffff,%ax
	push   %ax
	/* TODO: data ref: mov    $0x20d0,%ax */
	mov    $D_00d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0d31:
	jmp   L_0d71
	mov    $0xffff,%ax
	push   %ax
	/* TODO: data ref: mov    $0x28d0,%ax */
	mov    $D_08d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0d41:
	jmp   L_0d81
	mov    $0xffff,%ax
	push   %ax
	mov    $D_18d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0d51:
	jmp   L_0d91
	mov    $0xffff,%ax
	push   %ax
	mov    $0xd0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0d61:
	jmp   L_0da0
	mov    $0xffff,%ax
	push   %ax
	mov    $0x8d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0d71:
	jmp   L_0daf
	mov    $0xffff,%ax
	push   %ax
	/* TODO: data ref: mov    $0x10d0,%ax */
	mov    $0x10d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0d81:
	jmp   L_0dbe
	mov    $0xffff,%ax
	push   %ax
	/* TODO: data ref: mov    $0x18d0,%ax */
	mov    $D_18d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0d91:
	jmp   L_0dcd
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x20d0,%ax */
	mov    $D_00d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0da0:
	jmp   L_0ddc
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x28d0,%ax */
	mov    $D_08d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0daf:
	jmp   L_0deb
	xor    %ax,%ax
	push   %ax
	mov    $D_18d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0dbe:
	jmp   L_0dfa
	xor    %ax,%ax
	push   %ax
	mov    $0xd0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0dcd:
	jmp   L_0e0a
	xor    %ax,%ax
	push   %ax
	mov    $0x8d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0ddc:
	jmp   L_0e1a
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x10d0,%ax */
	mov    $0x10d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0deb:
	jmp   L_0e2a
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    $0x18d0,%ax */
	mov    $D_18d0,%ax
	push   %ax
	call   L_1dec
	add    $0x4,%sp
L_0dfa:
	jmp   L_0e3a
	/* TODO: data ref: mov    $0x20ff,%ax */
	mov    $D_00ff,%ax
	push   %ax
	mov    $0xe9,%ax
	push   %ax
	call   L_1e89
	add    $0x4,%sp
L_0e0a:
	jmp   L_0e4b
	/* TODO: data ref: mov    $0x10ff,%ax */
	mov    $0x10ff,%ax
	push   %ax
	mov    $0xe8,%ax
	push   %ax
	call   L_1e89
	add    $0x4,%sp
L_0e1a:
	jmp   L_0e4b
	/* TODO: data ref: mov    $0x28ff,%ax */
	mov    $D_08ff,%ax
	push   %ax
	mov    $0xea,%ax
	push   %ax
	call   L_1f6a
	add    $0x4,%sp
L_0e2a:
	jmp   L_0e75
	/* TODO: data ref: mov    $0x18ff,%ax */
	mov    $0x18ff,%ax
	push   %ax
	mov    $0x9a,%ax
	push   %ax
	call   L_1f6a
	add    $0x4,%sp
L_0e3a:
	jmp   L_0e75
	cmpw   $0x1,D_261c
	jle   L_0e4d
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_0e4b:
	jmp   L_0e75
L_0e4d:
	cmpw   $0x52,0x4(%bp)
	jne   L_0e58
	mov    $0x1,%ax
	jmp   L_0e5a
L_0e58:
	xor    %ax,%ax
L_0e5a:
	mov    %ax,%di
	cmpw   $0x0,D_261c
	jne   L_0e77
	or     %di,%di
	je   L_0e6c
	mov    $0x90c3,%ax
	jmp   L_0e6f
L_0e6c:
	mov    $0x90cb,%ax
L_0e6f:
	mov    %ax,%cx
	mov    %cl,D_256c
L_0e75:
	jmp   L_0ec0
L_0e77:
	or     %di,%di
	je   L_0e80
	mov    $0xc2,%ax
	jmp   L_0e83
L_0e80:
	mov    $0xca,%ax
L_0e83:
	mov    %ax,%cx
	mov    %cl,D_256c
	movb   $0x3,D_256a
	mov    D_2582,%di
	mov    %di,%ax
	and    $0xff,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    %di,%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    %ax,%cx
	mov    %cl,D_256e
	cmpb   $0x1,D_257c
	jne   L_0eb8
	cmpw   $0x0,D_2580
	je   L_0eef
L_0eb8:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_0ec0:
	jmp   L_0f04
	cmpw   $0x1,D_261c
	je   L_0ed3
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_0f14
L_0ed3:
	mov    $D_257c,%ax
	push   %ax
	call   L_162d
	pop    %cx
	mov    %ax,%di
	cmp    $0x10,%di
	jl   L_0ee7
	cmp    $0x13,%di
	jle   L_0ef1
L_0ee7:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_0eef:
	jmp   L_0f32
L_0ef1:
	mov    %di,%ax
	and    $0x3,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	or     $0x26,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
L_0f04:
	jmp   L_0f3c
	mov    $0x1,%ax
	push   %ax
	mov    $0xe1d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_0f14:
	jmp   L_0f4c
	mov    $0xd8,%ax
	push   %ax
	call   L_2076
	pop    %cx
	jmp   L_0f5c
	mov    $0xda,%ax
	push   %ax
	call   L_2076
	pop    %cx
	jmp   L_0f6c
	mov    $0xdc,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_0f32:
	jmp   L_0f6c
	mov    $0xde,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_0f3c:
	jmp   L_0f7b
	mov    $0x1,%ax
	push   %ax
	mov    $0xc0d8,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_0f4c:
	jmp   L_0f8f
	mov    $0x1,%ax
	push   %ax
	mov    $0xc0da,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_0f5c:
	jmp   L_0f99
	mov    $0x1,%ax
	push   %ax
	mov    $0xe0d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_0f6c:
	jmp   L_0fa3
	xor    %ax,%ax
	push   %ax
	mov    $0xe2db,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_0f7b:
	jmp   L_0fb2
	/* TODO: data ref: mov    $0x10d8,%ax */
	mov    $D_10d8,%ax
	push   %ax
	call   L_2076
	pop    %cx
	jmp   L_0fbc
	/* TODO: data ref: mov    $0x10da,%ax */
	mov    $D_10da,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_0f8f:
	jmp   L_0fc6
	/* TODO: data ref: mov    $0x10dc,%ax */
	mov    $D_10dc,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_0f99:
	jmp   L_0fd0
	/* TODO: data ref: mov    $0x10de,%ax */
	mov    $D_10de,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_0fa3:
	jmp   L_0fda
	xor    %ax,%ax
	push   %ax
	mov    $0xd0d8,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_0fb2:
	jmp   L_0fe9
	/* TODO: data ref: mov    $0x18d8,%ax */
	mov    $D_18d8,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_0fbc:
	jmp   L_0ff9
	/* TODO: data ref: mov    $0x18da,%ax */
	mov    $D_18da,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_0fc6:
	jmp   L_1009
	/* TODO: data ref: mov    $0x18dc,%ax */
	mov    $D_18dc,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_0fd0:
	jmp   L_1009
	/* TODO: data ref: mov    $0x18de,%ax */
	mov    $D_18de,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_0fda:
	jmp   L_1018
	xor    %ax,%ax
	push   %ax
	mov    $0xd8d8,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_0fe9:
	jmp   L_102c
	mov    $0x1,%ax
	push   %ax
	mov    $0xd9de,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_0ff9:
	jmp   L_1036
	mov    $0x1,%ax
	push   %ax
	mov    $0xf6d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1009:
	jmp   L_1040
	xor    %ax,%ax
	push   %ax
	mov    $0xe1db,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1018:
	jmp   L_1054
	mov    $D_10d8,%ax
	push   %ax
	call   L_2076
	pop    %cx
	jmp   L_105e
	mov    $D_10da,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_102c:
	jmp   L_1068
	mov    $D_10dc,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1036:
	jmp   L_1068
	mov    $D_10de,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1040:
	jmp   L_1078
	mov    $D_18d8,%ax
	push   %ax
	call   L_2076
	pop    %cx
	jmp   L_1088
	mov    $D_18da,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1054:
	jmp   L_1098
	mov    $D_18dc,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_105e:
	jmp   L_1098
	mov    $D_18de,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1068:
	jmp   L_10a8
	mov    $0x1,%ax
	push   %ax
	mov    $0xf0d8,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_1078:
	jmp   L_10b7
	mov    $0x1,%ax
	push   %ax
	mov    $0xf0da,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_1088:
	jmp   L_10c1
	mov    $0x1,%ax
	push   %ax
	mov    $0xf8d8,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_1098:
	jmp   L_10d1
	mov    $0x1,%ax
	push   %ax
	mov    $0xf8da,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_10a8:
	jmp   L_10ea
	xor    %ax,%ax
	push   %ax
	mov    $0xe0db,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_10b7:
	jmp   L_10f4
	mov    $0xc0dd,%ax
	push   %ax
	call   L_2181
	pop    %cx
L_10c1:
	jmp   L_10fe
	mov    $0x1,%ax
	push   %ax
	mov    $0xf7d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_10d1:
	jmp   L_1108
	xor    %ax,%ax
	push   %ax
	mov    $0xe3db,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
	jmp   L_111c
	/* TODO: data ref: mov    $0x28d9,%ax */
	mov    $D_08d9,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_10ea:
	jmp   L_1126
	/* TODO: data ref: mov    $0x20d9,%ax */
	mov    $D_00d9,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_10f4:
	jmp   L_1126
	mov    $0xd9,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_10fe:
	jmp   L_1136
	mov    $0xdb,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1108:
	jmp   L_1146
	mov    $0xdd,%ax
	push   %ax
	call   L_2076
	pop    %cx
	jmp   L_1156
	mov    $0xdf,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_111c:
	jmp   L_1156
	mov    $0xc0d9,%ax
	push   %ax
	call   L_2181
	pop    %cx
L_1126:
	jmp   L_1166
	mov    $0x1,%ax
	push   %ax
	mov    $0xecd9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1136:
	jmp   L_1176
	mov    $0x1,%ax
	push   %ax
	mov    $0xedd9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1146:
	jmp   L_1186
	mov    $0x1,%ax
	push   %ax
	mov    $0xead9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1156:
	jmp   L_1196
	mov    $0x1,%ax
	push   %ax
	mov    $0xe9e9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1166:
	jmp   L_11a6
	mov    $0x1,%ax
	push   %ax
	mov    $0xebd9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1176:
	jmp   L_11ba
	mov    $0x1,%ax
	push   %ax
	mov    $0xeed9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1186:
	jmp   L_11c4
	mov    $0x1,%ax
	push   %ax
	mov    $0xe8d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1196:
	jmp   L_11ce
	mov    $0x1,%ax
	push   %ax
	mov    $0xd0d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_11a6:
	jmp   L_11de
	mov    $0x8d8,%ax
	push   %ax
	call   L_2076
	pop    %cx
	jmp   L_11ee
	mov    $0x8da,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_11ba:
	jmp   L_11fe
	mov    $0x8dc,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_11c4:
	jmp   L_11fe
	mov    $0x8de,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_11ce:
	jmp   L_120e
	mov    $0x1,%ax
	push   %ax
	mov    $0xc8d8,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_11de:
	jmp   L_121e
	mov    $0x1,%ax
	push   %ax
	mov    $0xc8da,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_11ee:
	jmp   L_1228
	mov    $0x1,%ax
	push   %ax
	mov    $0xf3d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_11fe:
	jmp   L_1242
	mov    $0x1,%ax
	push   %ax
	mov    $0xf8d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_120e:
	jmp   L_1252
	mov    $0x1,%ax
	push   %ax
	mov    $0xf2d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_121e:
	jmp   L_1262
	/* TODO: data ref: mov    $0x20dd,%ax */
	mov    $D_00dd,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1228:
	jmp   L_126c
	mov    $0x1,%ax
	push   %ax
	mov    $0xfcd9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
	jmp   L_1276
	mov    $D_10dd,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1242:
	jmp   L_1280
	mov    $0x1,%ax
	push   %ax
	mov    $0xfdd9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1252:
	jmp   L_128a
	mov    $0x1,%ax
	push   %ax
	mov    $0xfad9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1262:
	jmp   L_129e
	/* TODO: data ref: mov    $0x10d9,%ax */
	mov    $D_10d9,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_126c:
	jmp   L_12a8
	/* TODO: data ref: mov    $0x10db,%ax */
	mov    $0x10db,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1276:
	jmp   L_12b2
	/* TODO: data ref: mov    $0x10dd,%ax */
	mov    $D_10dd,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1280:
	jmp   L_12bc
	/* TODO: data ref: mov    $0x10df,%ax */
	mov    $0x10df,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_128a:
	jmp   L_12c6
	mov    $0xd0dd,%ax
	push   %ax
	call   L_2181
	pop    %cx
	jmp   L_12d0
	/* TODO: data ref: mov    $0x18d9,%ax */
	mov    $D_18d9,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_129e:
	jmp   L_12da
	/* TODO: data ref: mov    $0x18db,%ax */
	mov    $0x18db,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_12a8:
	jmp   L_12e4
	/* TODO: data ref: mov    $0x18dd,%ax */
	mov    $D_18dd,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_12b2:
	jmp   L_12ee
	/* TODO: data ref: mov    $0x18df,%ax */
	mov    $0x18df,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_12bc:
	jmp   L_12f8
	mov    $0xd8dd,%ax
	push   %ax
	call   L_2181
	pop    %cx
L_12c6:
	jmp   L_1302
	mov    $D_18d9,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_12d0:
	jmp   L_130c
	mov    $D_10d9,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_12da:
	jmp   L_1316
	mov    $D_18dd,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_12e4:
	jmp   L_1320
	/* TODO: data ref: mov    $0x20d8,%ax */
	mov    $D_00d8,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_12ee:
	jmp   L_132a
	/* TODO: data ref: mov    $0x20da,%ax */
	mov    $D_00da,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_12f8:
	jmp   L_1334
	/* TODO: data ref: mov    $0x20dc,%ax */
	mov    $D_00dc,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1302:
	jmp   L_1334
	/* TODO: data ref: mov    $0x20de,%ax */
	mov    $D_00de,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_130c:
	jmp   L_1344
	/* TODO: data ref: mov    $0x28d8,%ax */
	mov    $D_08d8,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1316:
	jmp   L_1354
	/* TODO: data ref: mov    $0x28da,%ax */
	mov    $D_08da,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1320:
	jmp   L_1364
	/* TODO: data ref: mov    $0x28dc,%ax */
	mov    $D_08dc,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_132a:
	jmp   L_1364
	/* TODO: data ref: mov    $0x28de,%ax */
	mov    $D_08de,%ax
	push   %ax
	call   L_2076
	pop    %cx
L_1334:
	jmp   L_1374
	mov    $0x1,%ax
	push   %ax
	mov    $0xe0d8,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_1344:
	jmp   L_1384
	mov    $0x1,%ax
	push   %ax
	mov    $0xe0da,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_1354:
	jmp   L_138e
	mov    $0x1,%ax
	push   %ax
	mov    $0xe8d8,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_1364:
	jmp   L_13a8
	mov    $0x1,%ax
	push   %ax
	mov    $0xe8da,%ax
	push   %ax
	call   L_20d0
	add    $0x4,%sp
L_1374:
	jmp   L_13b8
	mov    $0x1,%ax
	push   %ax
	mov    $0xe4d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_1384:
	jmp   L_13c8
	mov    $0x9b,%ax
	push   %ax
	call   L_17c7
	pop    %cx
L_138e:
	jmp   L_13c8
	mov    $0x1,%ax
	push   %ax
	mov    $0xe5d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
	jmp   L_13d8
	mov    $0xc8d9,%ax
	push   %ax
	call   L_2181
	pop    %cx
L_13a8:
	jmp   L_13e8
	mov    $0x1,%ax
	push   %ax
	mov    $0xf4d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_13b8:
	jmp   L_13e8
	mov    $0x1,%ax
	push   %ax
	mov    $0xf1d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_13c8:
	jmp   L_13e8
	mov    $0x1,%ax
	push   %ax
	mov    $0xf9d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_13d8:
	jmp   L_13e8
	mov    $0x1,%ax
	push   %ax
	mov    $0xf0d9,%ax
	push   %ax
	call   L_2027
	add    $0x4,%sp
L_13e8:
	jmp   L_144f
	mov    $0x2,%ax
	push   %ax
	call   L_2396
	pop    %cx
	jmp   L_1432
	mov    $0x1,%ax
	push   %ax
	call   L_2396
	pop    %cx
	jmp   L_143b
	xor    %ax,%ax
	push   %ax
	call   L_2396
	pop    %cx
	jmp   L_144a
	pushw  D_1c6c
	call   L_2577
	pop    %cx
	jmp   L_144a
	pushw  D_1c6e
	call   L_2577
	pop    %cx
	jmp   L_144a
	pushw  D_1c70
	call   L_2577
	pop    %cx
	jmp   L_144a
	call   L_25c3
	jmp   L_144a
	call   L_260a
	jmp   L_144a
	call   L_2353
L_1432:
	jmp   L_144a
	xor    %ax,%ax
	push   %ax
	call   L_24c9
	pop    %cx
L_143b:
	jmp   L_144a
	mov    $0x1,%ax
	push   %ax
	call   L_24c9
	pop    %cx
	jmp   L_144a
	call   L_2523
L_144a:
	movb   $0x0,D_256a
L_144f:
	jmp   L_1459
L_1451:
	mov    $0x11,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_1459:
	jmp   L_1468
L_145b:
	dec    %ax
	cmp    $0xfd,%ax
	ja   L_1451
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x2f6e(%bx)
L_1468:
	cmpb   $0x0,D_256a
	je   L_1485
	mov    D_256a,%al
	cbtw
	push   %ax
	mov    $D_256c,%ax
	push   %ax
	call   L_292f
	add    $0x4,%sp
	mov    D_256a,%al
	cbtw
	mov    %ax,D_2568
L_1485:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_148a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	cmp    $0x7f,%sp
	ja   L_1499
	int    $0xf2
L_1499:
	mov    0x4(%bp),%di
	cmpw   $0x0,0x8(%bp)
	je   L_14df
	cmpw   $0x0,0x6(%bp)
	je   L_14df
	cmpb   $0x4,(%di)
	jne   L_14df
	movw   $0x0,0x6(%bp)
	orb    $0x2,D_256c
	mov    0x6(%di),%ax
	mov    0x8(%di),%dx
	and    $0xff80,%ax
	and    $0x0,%dx
	mov    %ax,%si
	/* TODO: data ref: cmpw   $0x2,0x2472 */
	cmpw   $0x2,D_0472
	jne   L_14df
	or     %si,%si
	je   L_14df
	cmp    $0xff80,%si
	je   L_14df
	mov    $0xd,%ax
	push   %ax
	call   L_00a4
	pop    %cx
L_14df:
	mov    0x6(%di),%ax
	mov    %ax,-0x6(%bp)
	cmpw   $0x0,0x4(%di)
	je   L_1512
	mov    D_256a,%ax
	cbtw
	cwtd
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
	xor    %ax,%ax
	push   %ax
	mov    D_2578,%ax
	mov    D_257a,%dx
	add    -0xa(%bp),%ax
	adc    -0x8(%bp),%dx
	push   %dx
	push   %ax
	pushw  0x6(%bp)
	push   %di
	call   L_29dc
	add    $0xa,%sp
L_1512:
	mov    -0x6(%bp),%ax
	and    $0xff,%ax
	mov    D_256a,%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,D_256c(%bx)
	incb   D_256a
	cmpw   $0x0,0x6(%bp)
	je   L_1554
	mov    -0x6(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    D_256a,%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,D_256c(%bx)
	incb   D_256a
L_1554:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_155b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmp    $0x7f,%sp
	ja   L_1568
	int    $0xf2
L_1568:
	mov    0x4(%bp),%di
	mov    (%di),%al
	cbtw
	mov    %ax,%si
	test   $0x10,%si
	je   L_157b
	xor    %ax,%ax
	jmp   L_15a3
L_157b:
	cmp    $0x2,%si
	jne   L_15a0
	mov    0x6(%di),%ax
	mov    %ax,-0x6(%bp)
	cmp    $0x8,%ax
	jl   L_1597
	cmpw   $0x10,-0x6(%bp)
	jl   L_159c
	cmpw   $0x13,-0x6(%bp)
	jg   L_159c
L_1597:
	mov    $0x1,%ax
	jmp   L_15a3
L_159c:
	xor    %ax,%ax
	jmp   L_15a3
L_15a0:
	mov    $0xffff,%ax
L_15a3:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_15a8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmp    $0x7f,%sp
	ja   L_15b5
	int    $0xf2
L_15b5:
	mov    $D_257c,%di
	movw   $0x0,-0x6(%bp)
L_15bd:
	mov    -0x6(%bp),%ax
	cmp    D_261c,%ax
	jge   L_161a
	push   %di
	call   L_155b
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%si
	je   L_1612
	cmpw   $0xffff,0x4(%bp)
	jne   L_15dd
	mov    %si,0x4(%bp)
	jmp   L_1612
L_15dd:
	cmpw   $0x0,0x4(%bp)
	jne   L_1605
	cmp    $0x1,%si
	jne   L_1605
	cmpb   $0x2,(%di)
	jne   L_1605
	cmpw   $0x0,0x8(%di)
	jg   L_1605
	jl   L_15fb
	cmpw   $0x4,0x6(%di)
	jae   L_1605
L_15fb:
	addw   $0x8,0x6(%di)
	adcw   $0x0,0x8(%di)
	jmp   L_1612
L_1605:
	cmp    %si,0x4(%bp)
	je   L_1612
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_1612:
	add    $0xa,%di
	incw   -0x6(%bp)
	jmp   L_15bd
L_161a:
	cmpw   $0xffff,0x4(%bp)
	jne   L_1625
	mov    $0x1,%ax
	jmp   L_1628
L_1625:
	mov    0x4(%bp),%ax
L_1628:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_162d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_1639
	int    $0xf2
L_1639:
	mov    0x4(%bp),%di
	mov    (%di),%al
	cbtw
	and    $0xf,%ax
	cmp    $0x2,%ax
	je   L_164d
	mov    $0xffff,%ax
	jmp   L_1653
L_164d:
	mov    0x6(%di),%ax
	mov    0x8(%di),%dx
L_1653:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1657:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_1663
	int    $0xf2
L_1663:
	mov    D_2586,%al
	cbtw
	and    $0xf,%ax
	mov    %ax,%di
	cmp    $0x3,%ax
	je   L_1676
	cmp    $0x4,%di
	jne   L_167b
L_1676:
	mov    $0x1,%ax
	jmp   L_167d
L_167b:
	xor    %ax,%ax
L_167d:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1681:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_168d
	int    $0xf2
L_168d:
	mov    0x4(%bp),%di
	cmpb   $0x2,D_256a
	jge   L_169d
	movb   $0x2,D_256a
L_169d:
	mov    (%di),%al
	cbtw
	and    $0xf,%ax
	jmp   L_17b6
	push   %di
	call   L_162d
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	jl   L_16b6
	cmp    $0xf,%si
	jle   L_16c0
L_16b6:
	mov    $0x1b,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_16f0
L_16c0:
	mov    %si,%ax
	and    $0x7,%ax
	or     $0xc0,%ax
	mov    %ax,%cx
	or     %cl,D_256d
	jmp   L_171e
	orb    $0x6,D_256d
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	push   %di
	call   L_148a
	add    $0x6,%sp
	jmp   L_173a
L_16e5:
	mov    0x2(%di),%al
	cbtw
	jmp   L_1754
L_16eb:
	orb    $0x7,D_256d
L_16f0:
	jmp   L_1748
L_16f2:
	mov    (%di),%al
	cbtw
	and    $0xf,%ax
	cmp    $0x5,%ax
	jne   L_1719
	orb    $0x40,D_256d
	movw   $0x0,0x6(%di)
	movw   $0x0,0x8(%di)
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	push   %di
	call   L_148a
	add    $0x6,%sp
L_1719:
	orb    $0x6,D_256d
L_171e:
	jmp   L_1752
L_1720:
	orb    $0x4,D_256d
	jmp   L_1752
L_1727:
	orb    $0x5,D_256d
	jmp   L_1752
L_172e:
	orb    $0x0,D_256d
	jmp   L_1752
L_1735:
	orb    $0x1,D_256d
L_173a:
	jmp   L_1752
L_173c:
	orb    $0x2,D_256d
	jmp   L_1752
L_1743:
	orb    $0x3,D_256d
L_1748:
	jmp   L_1752
L_174a:
	mov    $0x1b,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_1752:
	jmp   L_17b4
L_1754:
	cmp    $0x7,%ax
	je   L_1727
	jg   L_176c
	cmp    $0x3,%ax
	je   L_16eb
	cmp    $0x5,%ax
	je   L_16f2
	cmp    $0x6,%ax
	je   L_1720
	jmp   L_174a
L_176c:
	cmp    $0x40,%ax
	je   L_172e
	cmp    $0x41,%ax
	je   L_1735
	cmp    $0x42,%ax
	je   L_173c
	cmp    $0x43,%ax
	je   L_1743
	jmp   L_174a
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	push   %di
	call   L_148a
	add    $0x6,%sp
	orb    $0x80,D_256d
L_1795:
	jmp   L_16e5
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	push   %ax
	push   %di
	call   L_148a
	add    $0x6,%sp
	orb    $0x40,D_256d
	jmp   L_1795
L_17ac:
	mov    $0x900d,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_17b4:
	jmp   L_17c3
L_17b6:
	dec    %ax
	cmp    $0x6,%ax
	ja   L_17ac
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x316a(%bx)
L_17c3:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_17c7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_17d3
	int    $0xf2
L_17d3:
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	cmpw   $0x0,D_261c
	je   L_17eb
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_17eb:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_17ef:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_17fb
	int    $0xf2
L_17fb:
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	cmpw   $0x1,D_261c
	je   L_1815
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_1863
L_1815:
	mov    D_2578,%ax
	mov    D_257a,%dx
	add    $0x2,%ax
	adc    $0x0,%dx
	mov    D_2582,%bx
	mov    D_2584,%si
	sub    %ax,%bx
	sbb    %dx,%si
	mov    %bx,%di
	cmpb   $0x1,D_257c
	jne   L_184e
	mov    D_2580,%bx
	mov    0x4(%bx),%ax
	cmp    D_1c6a,%ax
	jne   L_184e
	cmp    $0xff80,%di
	jl   L_184e
	cmp    $0x7f,%di
	jle   L_1858
L_184e:
	mov    $0x24,%ax
	push   %ax
	call   L_0067
	pop    %cx
	xor    %di,%di
L_1858:
	movb   $0x2,D_256a
	mov    %di,%cx
	mov    %cl,D_256d
L_1863:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1867:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmp    $0x7f,%sp
	ja   L_1874
	int    $0xf2
L_1874:
	movw   $0x0,-0x6(%bp)
	mov    $D_257c,%di
	cmpw   $0x1,D_261c
	je   L_188d
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_18a0
L_188d:
	mov    (%di),%al
	cbtw
	and    $0xf,%ax
	cmp    $0x1,%ax
	je   L_18a2
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_18a0:
	jmp   L_18fb
L_18a2:
	mov    D_2578,%ax
	mov    D_257a,%dx
	add    $0x2,%ax
	adc    $0x0,%dx
	mov    0x6(%di),%bx
	mov    0x8(%di),%si
	sub    %ax,%bx
	sbb    %dx,%si
	mov    %bx,-0x6(%bp)
	mov    0x1(%di),%al
	cbtw
	test   $0x4,%ax
	jne   L_1907
	/* TODO: data ref: cmpw   $0x1,0x2472 */
	cmpw   $0x1,D_0472
	jne   L_18fd
	mov    $D_1d2e,%ax
	push   %ax
	mov    D_2578,%ax
	mov    D_257a,%dx
	add    $0x2,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	cmpw   $0x0,0x6(%bp)
	jne   L_18ea
	mov    $0x3,%ax
	jmp   L_18ed
L_18ea:
	mov    $0x5,%ax
L_18ed:
	push   %ax
	push   %di
	call   L_2b38
	add    $0xa,%sp
	mov    %ax,%cx
	mov    %cl,D_256a
L_18fb:
	jmp   L_192c
L_18fd:
	mov    0x4(%di),%bx
	mov    0x4(%bx),%ax
	cmp    D_1c6a,%ax
L_1907:
	jne   L_192e
	cmpw   $0xff80,-0x6(%bp)
	jl   L_192e
	cmpw   $0x7f,-0x6(%bp)
	jg   L_192e
	movb   $0x2,D_256a
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256d
L_192c:
	jmp   L_1989
L_192e:
	cmpw   $0x0,0x6(%bp)
	jne   L_198b
	movb   $0xe9,D_256c
	decw   -0x6(%bp)
	mov    D_2580,%bx
	mov    0x4(%bx),%ax
	cmp    D_1c6a,%ax
	je   L_196a
	mov    $0x1,%ax
	push   %ax
	mov    D_2578,%ax
	mov    D_257a,%dx
	add    $0x1,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $D_257c,%ax
	push   %ax
	call   L_29dc
	add    $0xa,%sp
L_196a:
	movb   $0x3,D_256a
	mov    -0x6(%bp),%ax
	and    $0xff,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    -0x6(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    %ax,%cx
	mov    %cl,D_256e
L_1989:
	jmp   L_19ef
L_198b:
	mov    0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	movb   $0x3,D_256d
	movb   $0xe9,D_256e
	subw   $0x3,-0x6(%bp)
	mov    D_2580,%bx
	mov    0x4(%bx),%ax
	cmp    D_1c6a,%ax
	je   L_19d0
	mov    $0x1,%ax
	push   %ax
	mov    D_2578,%ax
	mov    D_257a,%dx
	add    $0x3,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $D_257c,%ax
	push   %ax
	call   L_29dc
	add    $0xa,%sp
L_19d0:
	movb   $0x5,D_256a
	mov    -0x6(%bp),%ax
	and    $0xff,%ax
	mov    %ax,%cx
	mov    %cl,D_256f
	mov    -0x6(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    %ax,%cx
	mov    %cl,D_2570
L_19ef:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_19f4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_1a00
	int    $0xf2
L_1a00:
	cmpw   $0x2,D_261c
	je   L_1a11
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_1a54
L_1a11:
	mov    $0x1,%ax
	push   %ax
	call   L_15a8
	pop    %cx
	mov    $D_257c,%ax
	push   %ax
	call   L_162d
	pop    %cx
	mov    %ax,%di
	or     %di,%di
	jl   L_1a2c
	cmp    $0x7,%di
	jle   L_1a36
L_1a2c:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_1a54
L_1a36:
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	mov    %di,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    $D_2586,%ax
	push   %ax
	call   L_1681
	pop    %cx
L_1a54:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1a58:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmp    $0x7f,%sp
	ja   L_1a65
	int    $0xf2
L_1a65:
	cmpw   $0x2,D_261c
	je   L_1a76
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_1aad
L_1a76:
	pushw  0x4(%bp)
	call   L_15a8
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    $D_257c,%ax
	push   %ax
	call   L_162d
	pop    %cx
	mov    %ax,%di
	mov    $D_2586,%ax
	push   %ax
	call   L_162d
	pop    %cx
	mov    %ax,%si
	call   L_1657
	or     %ax,%ax
	je   L_1b00
	cmp    $0xffff,%di
	je   L_1ada
	cmp    $0x10,%di
	jl   L_1aaf
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_1aad:
	jmp   L_1ad8
L_1aaf:
	mov    -0x6(%bp),%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	or     $0xb0,%ax
	mov    %di,%dx
	and    $0x7,%dx
	or     %dx,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	xor    %ax,%ax
	push   %ax
	pushw  -0x6(%bp)
	mov    $D_2586,%ax
	push   %ax
	call   L_148a
	add    $0x6,%sp
L_1ad8:
	jmp   L_1afe
L_1ada:
	mov    -0x6(%bp),%ax
	or     $0xc6,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
	xor    %ax,%ax
	push   %ax
	pushw  -0x6(%bp)
	mov    $D_2586,%ax
	push   %ax
	call   L_148a
	add    $0x6,%sp
L_1afe:
	jmp   L_1b32
L_1b00:
	or     %di,%di
	je   L_1b09
	cmp    $0x8,%di
	jne   L_1b34
L_1b09:
	mov    D_2586,%al
	cbtw
	and    $0xf,%ax
	cmp    $0x1,%ax
	jne   L_1b34
	mov    -0x6(%bp),%ax
	or     $0xa0,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $D_2586,%ax
	push   %ax
	call   L_148a
	add    $0x6,%sp
L_1b32:
	jmp   L_1b66
L_1b34:
	or     %si,%si
	je   L_1b3d
	cmp    $0x8,%si
	jne   L_1b68
L_1b3d:
	mov    D_257c,%al
	cbtw
	and    $0xf,%ax
	cmp    $0x1,%ax
	jne   L_1b68
	mov    -0x6(%bp),%ax
	or     $0xa2,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $D_257c,%ax
	push   %ax
	call   L_148a
	add    $0x6,%sp
L_1b66:
	jmp   L_1b8f
L_1b68:
	cmp    $0x10,%di
	jl   L_1b91
	cmp    $0x13,%di
	jg   L_1b91
	movb   $0x8e,D_256c
	mov    $D_2586,%ax
	push   %ax
	call   L_1681
	pop    %cx
	mov    %di,%ax
	and    $0x3,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	mov    %ax,%cx
	or     %cl,D_256d
L_1b8f:
	jmp   L_1bb8
L_1b91:
	cmp    $0x10,%si
	jl   L_1bba
	cmp    $0x13,%si
	jg   L_1bba
	movb   $0x8c,D_256c
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
	mov    %si,%ax
	and    $0x3,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	mov    %ax,%cx
	or     %cl,D_256d
L_1bb8:
	jmp   L_1bec
L_1bba:
	mov    -0x6(%bp),%ax
	or     $0x88,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	or     %di,%di
	jl   L_1bee
	cmp    $0x10,%di
	jge   L_1bee
	orb    $0x2,D_256c
	mov    $D_2586,%ax
	push   %ax
	call   L_1681
	pop    %cx
	mov    %di,%ax
	and    $0x7,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	mov    %ax,%cx
	or     %cl,D_256d
L_1bec:
	jmp   L_1c19
L_1bee:
	or     %si,%si
	jl   L_1c11
	cmp    $0x10,%si
	jge   L_1c11
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
	mov    %si,%ax
	and    $0x7,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	mov    %ax,%cx
	or     %cl,D_256d
	jmp   L_1c19
L_1c11:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_1c19:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1c1e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmp    $0x7f,%sp
	ja   L_1c2b
	int    $0xf2
L_1c2b:
	cmpw   $0x2,D_261c
	je   L_1c3c
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_1c86
L_1c3c:
	pushw  0xe(%bp)
	call   L_15a8
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    $D_257c,%ax
	push   %ax
	call   L_162d
	pop    %cx
	mov    %ax,%di
	mov    $D_2586,%ax
	push   %ax
	call   L_162d
	pop    %cx
	mov    %ax,%si
	call   L_1657
	or     %ax,%ax
	je   L_1cbf
	or     %di,%di
	je   L_1c6a
	cmp    $0x8,%di
	jne   L_1c88
L_1c6a:
	mov    0x8(%bp),%ax
	or     -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	xor    %ax,%ax
	push   %ax
	pushw  -0x6(%bp)
	mov    $D_2586,%ax
	push   %ax
	call   L_148a
	add    $0x6,%sp
L_1c86:
	jmp   L_1cbd
L_1c88:
	mov    0x6(%bp),%ax
	and    $0xff,%ax
	or     -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	mov    0x6(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
	pushw  0xc(%bp)
	pushw  -0x6(%bp)
	mov    $D_2586,%ax
	push   %ax
	call   L_148a
	add    $0x6,%sp
L_1cbd:
	jmp   L_1cf7
L_1cbf:
	mov    0x4(%bp),%ax
	or     -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	or     %di,%di
	jl   L_1cf9
	cmp    $0x10,%di
	jge   L_1cf9
	cmpw   $0x0,0xa(%bp)
	je   L_1cdf
	orb    $0x2,D_256c
L_1cdf:
	mov    $D_2586,%ax
	push   %ax
	call   L_1681
	pop    %cx
	mov    %di,%ax
	and    $0x7,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	mov    %ax,%cx
	or     %cl,D_256d
L_1cf7:
	jmp   L_1d24
L_1cf9:
	or     %si,%si
	jl   L_1d1c
	cmp    $0x10,%si
	jge   L_1d1c
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
	mov    %si,%ax
	and    $0x7,%ax
	mov    $0x3,%cx
	shl    %cl,%ax
	mov    %ax,%cx
	or     %cl,D_256d
	jmp   L_1d24
L_1d1c:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_1d24:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d29:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_1d35
	int    $0xf2
L_1d35:
	cmpw   $0x1,D_261c
	jle   L_1d46
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_1d73
L_1d46:
	cmpw   $0x0,D_261c
	jne   L_1d58
	mov    0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	jmp   L_1d9b
L_1d58:
	mov    D_257c,%al
	cbtw
	and    $0xf,%ax
	cmp    $0x1,%ax
	jne   L_1d6b
	cmpw   $0x0,D_2580
	je   L_1d75
L_1d6b:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_1d73:
	jmp   L_1d9b
L_1d75:
	mov    D_2582,%di
	test   $0xff00,%di
	je   L_1d87
	mov    $0xd,%ax
	push   %ax
	call   L_00a4
	pop    %cx
L_1d87:
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	movb   $0x2,D_256a
	mov    %di,%cx
	mov    %cl,D_256d
L_1d9b:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1d9f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_1dab
	int    $0xf2
L_1dab:
	cmpw   $0x1,D_261c
	je   L_1dbc
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_1de8
L_1dbc:
	pushw  0x6(%bp)
	call   L_15a8
	pop    %cx
	mov    0x4(%bp),%dx
	and    $0xff,%dx
	or     %ax,%dx
	mov    %dx,%cx
	mov    %cl,D_256c
	mov    0x4(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
L_1de8:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1dec:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_1df8
	int    $0xf2
L_1df8:
	cmpw   $0x2,D_261c
	je   L_1e09
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_1e5c
L_1e09:
	movw   $0x1,D_261c
	pushw  0x6(%bp)
	call   L_15a8
	pop    %cx
	mov    0x4(%bp),%dx
	and    $0xff,%dx
	or     %ax,%dx
	mov    %dx,%cx
	mov    %cl,D_256c
	mov    0x4(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
	mov    D_2586,%al
	cbtw
	and    $0xf,%ax
	mov    %ax,%di
	cmp    $0x2,%di
	jne   L_1e5e
	cmpw   $0x0,D_258e
	jne   L_1e5e
	cmpw   $0x9,D_258c
	jne   L_1e5e
	orb    $0x2,D_256c
L_1e5c:
	jmp   L_1e85
L_1e5e:
	cmp    $0x3,%di
	je   L_1e68
	cmp    $0x4,%di
	jne   L_1e7d
L_1e68:
	cmpw   $0x0,D_258a
	jne   L_1e7d
	cmpw   $0x0,D_258e
	jne   L_1e7d
	cmpw   $0x1,D_258c
	je   L_1e85
L_1e7d:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_1e85:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1e89:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	cmp    $0x7f,%sp
	ja   L_1e98
	int    $0xf2
L_1e98:
	cmpw   $0x1,D_261c
	je   L_1ea9
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_1ed4
L_1ea9:
	mov    D_257c,%al
	cbtw
	test   $0x20,%ax
	je   L_1ed7
	mov    0x6(%bp),%ax
	and    $0xff,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	mov    0x6(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
L_1ed4:
	jmp   L_1f63
L_1ed7:
	mov    D_257c,%al
	cbtw
	mov    %ax,%di
	and    $0xf,%ax
	cmp    $0x1,%ax
	je   L_1eed
	mov    $0x24,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_1eed:
	mov    D_2578,%ax
	mov    D_257a,%dx
	add    $0x3,%ax
	adc    $0x0,%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    D_2582,%ax
	mov    D_2584,%dx
	sub    -0x8(%bp),%ax
	sbb    -0x6(%bp),%dx
	mov    %ax,%si
	mov    D_2580,%bx
	mov    0x4(%bx),%ax
	cmp    D_1c6a,%ax
	je   L_1f3d
	mov    $0x1,%ax
	push   %ax
	mov    D_2578,%ax
	mov    D_257a,%dx
	add    $0x1,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $D_257c,%ax
	push   %ax
	call   L_29dc
	add    $0xa,%sp
L_1f3d:
	movb   $0x3,D_256a
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	mov    %si,%ax
	and    $0xff,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    %si,%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    %ax,%cx
	mov    %cl,D_256e
L_1f63:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1f6a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_1f76
	int    $0xf2
L_1f76:
	mov    D_257c,%al
	cbtw
	test   $0x20,%ax
	je   L_1fb4
	cmpw   $0x1,D_261c
	je   L_1f90
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_1fd9
L_1f90:
	mov    0x6(%bp),%ax
	and    $0xff,%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	mov    0x6(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
	jmp   L_1fd9
L_1fb4:
	cmpw   $0x2,D_261c
	je   L_1fc5
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_1fd9
L_1fc5:
	mov    D_257c,%al
	cbtw
	and    $0xf,%ax
	cmp    $0x1,%ax
	je   L_1fdb
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_1fd9:
	jmp   L_2010
L_1fdb:
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,D_256c
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $D_257c,%ax
	push   %ax
	call   L_148a
	add    $0x6,%sp
	mov    D_2586,%al
	cbtw
	and    $0xf,%ax
	cmp    $0x1,%ax
	jne   L_2008
	cmpw   $0x0,D_258a
	je   L_2012
L_2008:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_2010:
	jmp   L_2023
L_2012:
	xor    %ax,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	mov    $D_2586,%ax
	push   %ax
	call   L_148a
	add    $0x6,%sp
L_2023:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2027:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_2033
	int    $0xf2
L_2033:
	mov    $0x0,%di
	cmp    %di,D_261c
	je   L_2044
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_2044:
	cmpw   $0x0,0x6(%bp)
	je   L_2050
	movb   $0x9b,D_256c(%di)
	inc    %di
L_2050:
	mov    0x4(%bp),%ax
	and    $0xff,%ax
	mov    %ax,%cx
	mov    %cl,D_256c(%di)
	inc    %di
	mov    0x4(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    %ax,%cx
	mov    %cl,D_256c(%di)
	inc    %di
	mov    %di,%cx
	mov    %cl,D_256a
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2076:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_2082
	int    $0xf2
L_2082:
	cmpw   $0x1,D_261c
	je   L_2093
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_20cc
L_2093:
	movb   $0x3,D_256a
	mov    $D_257c,%ax
	push   %ax
	call   L_1681
	pop    %cx
	mov    0x4(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	mov    D_256d,%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	or     %dx,%ax
	mov    %ax,%cx
	mov    %cl,D_256e
	mov    0x4(%bp),%ax
	and    $0xff,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	movb   $0x9b,D_256c
L_20cc:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_20d0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_20dc
	int    $0xf2
L_20dc:
	xor    %di,%di
	cmpw   $0x2,D_261c
	je   L_20ef
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_2113
L_20ef:
	cmpb   $0x1,D_257c
	jne   L_210b
	cmpw   $0x0,D_2580
	jne   L_210b
	cmpb   $0x1,D_2586
	jne   L_210b
	cmpw   $0x0,D_258a
	je   L_2115
L_210b:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_2113:
	jmp   L_217d
L_2115:
	cmpw   $0x0,D_258e
	jne   L_2138
	cmpw   $0x0,D_258c
	jne   L_2138
	mov    $0x1,%di
	mov    D_2582,%ax
	mov    D_2584,%dx
	and    $0x7,%ax
	and    $0x0,%dx
	mov    %ax,%si
	jmp   L_2148
L_2138:
	mov    D_258c,%ax
	mov    D_258e,%dx
	and    $0x7,%ax
	and    $0x0,%dx
	mov    %ax,%si
L_2148:
	movb   $0x3,D_256a
	movb   $0x9b,D_256c
	mov    0x4(%bp),%ax
	and    $0xff,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    0x4(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	or     %si,%ax
	mov    %ax,%cx
	mov    %cl,D_256e
	cmpw   $0x0,0x6(%bp)
	je   L_217d
	or     %di,%di
	je   L_217d
	orb    $0x4,D_256d
L_217d:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2181:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_218d
	int    $0xf2
L_218d:
	cmpw   $0x1,D_261c
	je   L_219e
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_21ec
L_219e:
	cmpb   $0x1,D_257c
	jne   L_21ac
	cmpw   $0x0,D_2580
	je   L_21b6
L_21ac:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_21ec
L_21b6:
	mov    D_2582,%ax
	mov    D_2584,%dx
	and    $0x7,%ax
	and    $0x0,%dx
	mov    %ax,%di
	movb   $0x3,D_256a
	movb   $0x9b,D_256c
	mov    0x4(%bp),%ax
	and    $0xff,%ax
	mov    %ax,%cx
	mov    %cl,D_256d
	mov    0x4(%bp),%ax
	mov    $0x8,%cx
	sar    %cl,%ax
	or     %di,%ax
	mov    %ax,%cx
	mov    %cl,D_256e
L_21ec:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_21f0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	cmp    $0x7f,%sp
	ja   L_21ff
	int    $0xf2
L_21ff:
	/* TODO: data ref: cmpw   $0x1,0x2472 */
	cmpw   $0x1,D_0472
	jle   L_220c
	call   L_2736
	jmp   L_22bf
L_220c:
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	mov    $D_2182,%ax
	push   %ax
	call   L_4338
	add    $0x8,%sp
	call   L_2c65
	mov    $D_1178,%si
	xor    %di,%di
L_2227:
	cmp    D_1c68,%di
	jge   L_2259
	pushw  0x4(%si)
	pushw  0x2(%si)
	push   %si
	call   L_2eb4
	add    $0x6,%sp
	cwtd
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x2(%si),%ax
	mov    0x4(%si),%dx
	add    -0x8(%bp),%ax
	adc    -0x6(%bp),%dx
	mov    %ax,0x2(%si)
	mov    %dx,0x4(%si)
	inc    %di
	add    $0xe,%si
	jmp   L_2227
L_2259:
	mov    D_1c6c,%bx
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	add    $0x1,%ax
	adc    $0x0,%dx
	and    $0xfffe,%ax
	and    $0xffff,%dx
	/* TODO: data ref: mov    %ax,0x2474 */
	mov    %ax,D_0474
	/* TODO: data ref: mov    %dx,0x2476 */
	mov    %dx,D_0476
	mov    D_1c6e,%bx
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	add    $0x1,%ax
	adc    $0x0,%dx
	and    $0xfffe,%ax
	and    $0xffff,%dx
	/* TODO: data ref: mov    %ax,0x2478 */
	mov    %ax,D_0478
	/* TODO: data ref: mov    %dx,0x247a */
	mov    %dx,D_047a
	mov    D_1c70,%bx
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	add    $0x1,%ax
	adc    $0x0,%dx
	and    $0xfffe,%ax
	and    $0xffff,%dx
	/* TODO: data ref: mov    %ax,0x247c */
	mov    %ax,D_047c
	/* TODO: data ref: mov    %dx,0x247e */
	mov    %dx,D_047e
	call   L_3123
	call   L_2f01
	call   L_264a
	call   L_22c6
L_22bf:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_22c6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_22d2
	int    $0xf2
L_22d2:
	mov    D_2298,%ax
	mov    %ax,D_261e
	movw   $0x0,D_222e
	/* TODO: data ref: movw   $0x0,0x205e */
	movw   $0x0,D_005e
	/* TODO: data ref: incw   0x2472 */
	incw   D_0472
	/* TODO: data ref: cmpw   $0x2,0x2472 */
	cmpw   $0x2,D_0472
	je   L_230f
	xor    %di,%di
L_22f1:
	cmp    D_1c68,%di
	jge   L_2347
	mov    %di,%ax
	mov    $0xe,%dx
	imul   %dx
	mov    %ax,%bx
	movw   $0x0,D_117e(%bx)
	movw   $0x0,D_1180(%bx)
	inc    %di
	jmp   L_22f1
L_230f:
	mov    D_1c6c,%bx
	movw   $0x0,0x6(%bx)
	movw   $0x0,0x8(%bx)
	mov    D_1c6e,%bx
	/* TODO: data ref: mov    0x2474,%ax */
	mov    D_0474,%ax
	/* TODO: data ref: mov    0x2476,%dx */
	mov    D_0476,%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	/* TODO: data ref: mov    0x2474,%ax */
	mov    D_0474,%ax
	/* TODO: data ref: mov    0x2476,%dx */
	mov    D_0476,%dx
	/* TODO: data ref: add    0x2478,%ax */
	add    D_0478,%ax
	/* TODO: data ref: adc    0x247a,%dx */
	adc    D_047a,%dx
	mov    D_1c70,%bx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
L_2347:
	mov    $D_1178,%ax
	push   %ax
	call   L_2577
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2353:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_235f
	int    $0xf2
L_235f:
	mov    D_2578,%ax
	mov    D_257a,%dx
	and    $0x1,%ax
	and    $0x0,%dx
	or     %dx,%dx
	jne   L_2373
	or     %ax,%ax
L_2373:
	je   L_2392
	addw   $0x1,D_2578
	adcw   $0x0,D_257a
	movb   $0x0,D_256c
	mov    $0x1,%ax
	push   %ax
	mov    $D_256c,%ax
	push   %ax
	call   L_292f
	add    $0x4,%sp
L_2392:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2396:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	cmp    $0x7f,%sp
	ja   L_23a5
	int    $0xf2
L_23a5:
	mov    0x4(%bp),%di
	xor    %si,%si
	movw   $D_257c,-0x6(%bp)
L_23b0:
	cmp    D_261c,%si
	jl   L_23b9
	jmp   L_24c2
L_23b9:
	mov    -0x6(%bp),%bx
	cmpb   $0x1,(%bx)
	je   L_23ee
	mov    -0x6(%bp),%bx
	cmpb   $0x4,(%bx)
	je   L_23ee
	mov    -0x6(%bp),%bx
	movw   $0x0,0x4(%bx)
	mov    -0x6(%bp),%bx
	movb   $0x1,(%bx)
	mov    -0x6(%bp),%bx
	movw   $0x0,0x6(%bx)
	movw   $0x0,0x8(%bx)
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_241d
L_23ee:
	mov    -0x6(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	je   L_241d
	mov    D_2568,%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	xor    %ax,%ax
	push   %ax
	mov    D_2578,%ax
	mov    D_257a,%dx
	add    -0xc(%bp),%ax
	adc    -0xa(%bp),%dx
	push   %dx
	push   %ax
	push   %di
	pushw  -0x6(%bp)
	call   L_29dc
	add    $0xa,%sp
L_241d:
	mov    %di,%ax
	jmp   L_24a9
L_2422:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    $0x10,%cx
	or     %cx,%cx
	je   L_2438
L_2432:
	sar    %dx
	rcr    %ax
	loop   L_2432
L_2438:
	mov    D_0f6c,%bx
	mov    %ax,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    D_0f6c,%bx
	mov    %ax,(%bx)
	mov    $0x4,%ax
	push   %ax
	pushw  D_0f6c
	call   L_292f
	add    $0x4,%sp
	addw   $0x4,D_2568
	jmp   L_24a7
L_2463:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    D_0f6c,%bx
	mov    %ax,(%bx)
	mov    $0x2,%ax
	push   %ax
	pushw  D_0f6c
	call   L_292f
	add    $0x4,%sp
	addw   $0x2,D_2568
	jmp   L_24ba
L_2487:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,%cx
	mov    %cl,-0x7(%bp)
	mov    $0x1,%ax
	push   %ax
	lea    -0x7(%bp),%ax
	push   %ax
	call   L_292f
	add    $0x4,%sp
	incw   D_2568
L_24a7:
	jmp   L_24ba
L_24a9:
	or     %ax,%ax
	je   L_2487
	cmp    $0x1,%ax
	je   L_2463
	cmp    $0x2,%ax
	jne   L_24ba
	jmp   L_2422
L_24ba:
	inc    %si
	addw   $0xa,-0x6(%bp)
	jmp   L_23b0
L_24c2:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_24c9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_24d5
	int    $0xf2
L_24d5:
	cmpw   $0x1,D_261c
	jne   L_24e3
	cmpb   $0x8,D_257c
	je   L_24ed
L_24e3:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_251f
L_24ed:
	mov    D_2582,%di
L_24f1:
	cmpb   $0x0,(%di)
	je   L_250a
	mov    $0x1,%ax
	push   %ax
	mov    %di,%ax
	inc    %di
	push   %ax
	call   L_292f
	add    $0x4,%sp
	incw   D_2568
	jmp   L_24f1
L_250a:
	cmpw   $0x0,0x4(%bp)
	je   L_251f
	mov    $0x1,%ax
	push   %ax
	push   %di
	call   L_292f
	add    $0x4,%sp
	incw   D_2568
L_251f:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2523:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmp    $0x7f,%sp
	ja   L_2530
	int    $0xf2
L_2530:
	movw   $0x0,-0x6(%bp)
	cmpw   $0x1,D_261c
	jne   L_254a
	cmpb   $0x1,D_257c
	jne   L_254a
	cmpw   $0x0,D_2580
	je   L_2554
L_254a:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_2572
L_2554:
	mov    D_2582,%di
L_2558:
	or     %di,%di
	jle   L_2572
	mov    $0x2,%ax
	push   %ax
	lea    -0x6(%bp),%ax
	push   %ax
	call   L_292f
	add    $0x4,%sp
	addw   $0x2,D_2568
	dec    %di
	jmp   L_2558
L_2572:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2577:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_2583
	int    $0xf2
L_2583:
	mov    0x4(%bp),%di
	mov    %di,D_1c6a
	mov    0x6(%di),%ax
	mov    0x8(%di),%dx
	mov    %ax,D_2578
	mov    %dx,D_257a
	mov    D_2298,%bx
	mov    D_1c6a,%ax
	mov    %ax,0x4(%bx)
	pushw  (%di)
	call   L_3052
	pop    %cx
	mov    %ax,%si
	orw    $0x43,0xc(%si)
	mov    D_1c6a,%ax
	mov    %ax,0x4(%si)
	movw   $0x0,0x6(%si)
	movw   $0x0,0x8(%si)
	pop    %di
	pop    %si
	pop    %bp
	ret


L_25c3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_25cf
	int    $0xf2
L_25cf:
	/* TODO: data ref: cmpw   $0x1,0x2472 */
	cmpw   $0x1,D_0472
	jne   L_2606
	xor    %di,%di
L_25d8:
	cmp    D_261c,%di
	jge   L_2606
	mov    %di,%ax
	mov    $0xa,%dx
	imul   %dx
	mov    %ax,%bx
	mov    D_2580(%bx),%si
	or     %si,%si
	jne   L_25f9
	mov    $0xe,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_2603
L_25f9:
	movw   $0x0,0x4(%si)
	orw    $0x5,0xc(%si)
L_2603:
	inc    %di
	jmp   L_25d8
L_2606:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_260a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_2616
	int    $0xf2
L_2616:
	/* TODO: data ref: cmpw   $0x1,0x2472 */
	cmpw   $0x1,D_0472
	jne   L_2646
	mov    D_2580,%di
	or     %di,%di
	jne   L_262f
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_2646
L_262f:
	movw   $0x0,0x4(%di)
	orw    $0x85,0xc(%di)
	mov    D_258c,%ax
	mov    D_258e,%dx
	mov    %ax,0x6(%di)
	mov    %dx,0x8(%di)
L_2646:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_264a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x64,%sp
	cmp    $0x7f,%sp
	ja   L_2659
	int    $0xf2
L_2659:
	mov    $D_1c84,%ax
	push   %ax
	mov    $D_229c,%ax
	push   %ax
	call   L_4224
	add    $0x4,%sp
	mov    %ax,D_2648
	or     %ax,%ax
	je   L_2683
	mov    $D_1c86,%ax
	push   %ax
	mov    $D_229c,%ax
	push   %ax
	call   L_4224
	add    $0x4,%sp
	mov    %ax,D_2644
	or     %ax,%ax
	jne   L_2691
L_2683:
	mov    $D_229c,%ax
	push   %ax
	mov    $D_1c88,%ax
	push   %ax
	call   L_0036
	add    $0x4,%sp
L_2691:
	mov    $D_1ca6,%ax
	push   %ax
	/* TODO: data ref: pushw  0x28ac */
	pushw  D_08ac
	lea    -0x68(%bp),%ax
	push   %ax
	call   L_038e
	add    $0x6,%sp
	mov    $D_1cad,%ax
	push   %ax
	lea    -0x68(%bp),%ax
	push   %ax
	call   L_4224
	add    $0x4,%sp
	mov    %ax,D_2646
	mov    $D_1caf,%ax
	push   %ax
	/* TODO: data ref: pushw  0x28ac */
	pushw  D_08ac
	lea    -0x68(%bp),%ax
	push   %ax
	call   L_038e
	add    $0x6,%sp
	mov    $D_1cb6,%ax
	push   %ax
	lea    -0x68(%bp),%ax
	push   %ax
	call   L_4224
	add    $0x4,%sp
	mov    %ax,D_264a
	cmpw   $0x0,D_2646
	je   L_26e5
	cmpw   $0x0,D_264a
	jne   L_26f3
L_26e5:
	lea    -0x68(%bp),%ax
	push   %ax
	mov    $D_1cb8,%ax
	push   %ax
	call   L_0036
	add    $0x4,%sp
L_26f3:
	xor    %ax,%ax
	push   %ax
	mov    $0x20,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  D_2648
	call   L_4338
	add    $0x8,%sp
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    0x2474,%ax */
	mov    D_0474,%ax
	/* TODO: data ref: mov    0x2476,%dx */
	mov    D_0476,%dx
	add    $0x20,%ax
	adc    $0x0,%dx
	push   %dx
	push   %ax
	pushw  D_2644
	call   L_4338
	add    $0x8,%sp
	movw   $0x0,D_264e
	movw   $0x0,D_264c
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2736:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x66,%sp
	cmp    $0x7f,%sp
	ja   L_2745
	int    $0xf2
L_2745:
	pushw  D_2646
	call   L_5f71
	pop    %cx
	pushw  D_264a
	call   L_5f71
	pop    %cx
	pushw  D_2648
	call   L_5f71
	pop    %cx
	movw   $0x107,D_2624
	/* TODO: data ref: mov    0x2474,%ax */
	mov    D_0474,%ax
	/* TODO: data ref: mov    0x2476,%dx */
	mov    D_0476,%dx
	mov    %ax,D_2628
	mov    %dx,D_262a
	/* TODO: data ref: mov    0x2478,%ax */
	mov    D_0478,%ax
	/* TODO: data ref: mov    0x247a,%dx */
	mov    D_047a,%dx
	mov    %ax,D_262c
	mov    %dx,D_262e
	/* TODO: data ref: mov    0x247c,%ax */
	mov    D_047c,%ax
	/* TODO: data ref: mov    0x247e,%dx */
	mov    D_047e,%dx
	mov    %ax,D_2630
	mov    %dx,D_2632
	mov    D_264e,%ax
	cwtd
	mov    %ax,D_263c
	mov    %dx,D_263e
	mov    D_264c,%ax
	cwtd
	mov    %ax,D_2640
	mov    %dx,D_2642
	movw   $0x0,D_2638
	movw   $0x0,D_263a
	xor    %ax,%ax
	push   %ax
	/* TODO: data ref: mov    0x2474,%ax */
	mov    D_0474,%ax
	/* TODO: data ref: mov    0x2476,%dx */
	mov    D_0476,%dx
	add    $0x20,%ax
	adc    $0x0,%dx
	/* TODO: data ref: add    0x2478,%ax */
	add    D_0478,%ax
	/* TODO: data ref: adc    0x247a,%dx */
	adc    D_047a,%dx
	push   %dx
	push   %ax
	pushw  D_2644
	call   L_4338
	add    $0x8,%sp
	mov    $D_1cd6,%ax
	push   %ax
	/* TODO: data ref: pushw  0x28ac */
	pushw  D_08ac
	lea    -0x6a(%bp),%ax
	push   %ax
	call   L_038e
	add    $0x6,%sp
	mov    $D_1cdd,%ax
	push   %ax
	lea    -0x6a(%bp),%ax
	push   %ax
	call   L_4224
	add    $0x4,%sp
	mov    %ax,%di
	or     %ax,%ax
	jne   L_2807
	lea    -0x6a(%bp),%ax
	push   %ax
	mov    $D_1cdf,%ax
	push   %ax
	call   L_0036
	add    $0x4,%sp
L_2807:
	decw   0x2(%di)
	jl   L_2818
	mov    (%di),%bx
	incw   (%di)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_281d
L_2818:
	push   %di
	call   L_4fa2
	pop    %cx
L_281d:
	mov    %ax,-0x6(%bp)
	cmp    $0xffff,%ax
	je   L_2854
	mov    D_2644,%bx
	decw   0x2(%bx)
	jl   L_2845
	mov    D_2644,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_2807
L_2845:
	pushw  D_2644
	pushw  -0x6(%bp)
	call   L_5e14
	add    $0x4,%sp
	jmp   L_2807
L_2854:
	push   %di
	call   L_5f71
	pop    %cx
	lea    -0x6a(%bp),%ax
	push   %ax
	call   L_63ae
	pop    %cx
	mov    $D_1d00,%ax
	push   %ax
	/* TODO: data ref: pushw  0x28ac */
	pushw  D_08ac
	lea    -0x6a(%bp),%ax
	push   %ax
	call   L_038e
	add    $0x6,%sp
	mov    $D_1d07,%ax
	push   %ax
	lea    -0x6a(%bp),%ax
	push   %ax
	call   L_4224
	add    $0x4,%sp
	mov    %ax,%di
	or     %ax,%ax
	jne   L_2895
	lea    -0x6a(%bp),%ax
	push   %ax
	mov    $D_1d09,%ax
	push   %ax
	call   L_0036
	add    $0x4,%sp
L_2895:
	decw   0x2(%di)
	jl   L_28a6
	mov    (%di),%bx
	incw   (%di)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_28ab
L_28a6:
	push   %di
	call   L_4fa2
	pop    %cx
L_28ab:
	mov    %ax,-0x6(%bp)
	cmp    $0xffff,%ax
	je   L_28e2
	mov    D_2644,%bx
	decw   0x2(%bx)
	jl   L_28d3
	mov    D_2644,%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_2895
L_28d3:
	pushw  D_2644
	pushw  -0x6(%bp)
	call   L_5e14
	add    $0x4,%sp
	jmp   L_2895
L_28e2:
	push   %di
	call   L_5f71
	pop    %cx
	lea    -0x6a(%bp),%ax
	push   %ax
	call   L_63ae
	pop    %cx
	pushw  D_2644
	call   L_3203
	pop    %cx
	cwtd
	mov    %ax,D_2634
	mov    %dx,D_2636
	xor    %ax,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  D_2644
	call   L_4338
	add    $0x8,%sp
	pushw  D_2644
	mov    $0x1,%ax
	push   %ax
	mov    $0x20,%ax
	push   %ax
	mov    $D_2624,%ax
	push   %ax
	call   L_4581
	add    $0x8,%sp
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_292f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_293b
	int    $0xf2
L_293b:
	mov    0x4(%bp),%di
	/* TODO: data ref: cmpw   $0x2,0x2472 */
	cmpw   $0x2,D_0472
	jne   L_297e
	mov    D_1c6a,%ax
	cmp    D_1c6c,%ax
	jne   L_2963
	pushw  D_2648
	mov    $0x1,%ax
	push   %ax
	pushw  0x6(%bp)
	push   %di
	call   L_4581
	add    $0x8,%sp
	jmp   L_297e
L_2963:
	mov    D_1c6a,%ax
	cmp    D_1c6e,%ax
	jne   L_297e
	pushw  D_2644
	mov    $0x1,%ax
	push   %ax
	pushw  0x6(%bp)
	push   %di
	call   L_4581
	add    $0x8,%sp
L_297e:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2982:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_298e
	int    $0xf2
L_298e:
	mov    0x4(%bp),%di
	/* TODO: data ref: cmpw   $0x2,0x2472 */
	cmpw   $0x2,D_0472
	jne   L_29d8
	mov    D_1c6a,%ax
	cmp    D_1c6c,%ax
	jne   L_29a8
	mov    D_2648,%si
	jmp   L_29b5
L_29a8:
	mov    D_1c6a,%ax
	cmp    D_1c6e,%ax
	jne   L_29d8
	mov    D_2644,%si
L_29b5:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	jle   L_29d8
	decw   0x2(%si)
	jl   L_29cc
	mov    (%si),%bx
	incw   (%si)
	movb   $0x0,(%bx)
	xor    %ax,%ax
	jmp   L_29b5
L_29cc:
	push   %si
	xor    %ax,%ax
	push   %ax
	call   L_5e14
	add    $0x4,%sp
	jmp   L_29b5
L_29d8:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_29dc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	cmp    $0x7f,%sp
	ja   L_29eb
	int    $0xf2
L_29eb:
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	je   L_2a1e
	mov    D_1c6a,%ax
	cmp    D_1c6c,%ax
	jne   L_2a20
	pushw  D_2646
	pushw  0xc(%bp)
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_2a5b
	add    $0xe,%sp
	add    %ax,D_264e
L_2a1e:
	jmp   L_2a54
L_2a20:
	mov    D_1c6a,%ax
	cmp    D_1c6e,%ax
	jne   L_2a54
	pushw  D_264a
	pushw  0xc(%bp)
	mov    0x8(%bp),%ax
	mov    0xa(%bp),%dx
	/* TODO: data ref: sub    0x2474,%ax */
	sub    D_0474,%ax
	/* TODO: data ref: sbb    0x2476,%dx */
	sbb    D_0476,%dx
	push   %dx
	push   %ax
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_2a5b
	add    $0xe,%sp
	add    %ax,D_264c
L_2a54:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2a5b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	cmp    $0x7f,%sp
	ja   L_2a6a
	int    $0xf2
L_2a6a:
	mov    0x4(%bp),%di
	mov    0x6(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,-0x8(%bp)
	mov    %ax,%bx
	mov    0x4(%bx),%si
	/* TODO: data ref: cmpw   $0x2,0x2472 */
	cmpw   $0x2,D_0472
	je   L_2a86
	jmp   L_2b2e
L_2a86:
	mov    0xa(%bp),%ax
	mov    0xc(%bp),%dx
	mov    %ax,(%di)
	mov    %dx,0x2(%di)
	mov    0xe(%bp),%ax
	mov    $0x0,%cl
	shl    %cl,%ax
	and    $0x1,%ax
	andw   $0xfffe,0x6(%di)
	or     %ax,0x6(%di)
	mov    0x8(%bp),%ax
	mov    $0x1,%cl
	shl    %cl,%ax
	and    $0x6,%ax
	andw   $0xfff9,0x6(%di)
	or     %ax,0x6(%di)
	andw   $0xfff7,0x6(%di)
	mov    -0x8(%bp),%bx
	mov    0xc(%bx),%ax
	test   $0x2,%ax
	jne   L_2aed
	mov    -0x8(%bp),%bx
	mov    0xc(%bx),%ax
	test   $0x4,%ax
	je   L_2aed
	mov    $0x1,%ax
	mov    $0x3,%cl
	shl    %cl,%ax
	and    $0x8,%ax
	andw   $0xfff7,0x6(%di)
	or     %ax,0x6(%di)
	mov    -0x8(%bp),%bx
	mov    0xa(%bx),%ax
	mov    %ax,0x4(%di)
	jmp   L_2b1c
L_2aed:
	cmp    D_1c6c,%si
	jne   L_2afa
	movw   $0x4,0x4(%di)
	jmp   L_2b1c
L_2afa:
	cmp    D_1c6e,%si
	jne   L_2b07
	movw   $0x6,0x4(%di)
	jmp   L_2b1c
L_2b07:
	cmp    D_1c70,%si
	jne   L_2b14
	movw   $0x8,0x4(%di)
	jmp   L_2b1c
L_2b14:
	mov    $0xb,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_2b1c:
	pushw  0x10(%bp)
	mov    $0x1,%ax
	push   %ax
	mov    $0x8,%ax
	push   %ax
	push   %di
	call   L_4581
	add    $0x8,%sp
L_2b2e:
	mov    $0x8,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2b38:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_2b44
	int    $0xf2
L_2b44:
	mov    0x4(%bp),%bx
	mov    0x1(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_2b53
	jmp   L_2bf7
L_2b53:
	mov    $0x1c,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_40a0
	add    $0x4,%sp
	mov    %ax,%di
	or     %ax,%ax
	jne   L_2b6f
	mov    $D_1d56,%ax
	push   %ax
	call   L_0036
	pop    %cx
L_2b6f:
	mov    D_2578,%ax
	mov    D_257a,%dx
	mov    %ax,0x4(%di)
	mov    %dx,0x6(%di)
	mov    D_1c6a,%ax
	mov    %ax,0x2(%di)
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    %ax,0x8(%di)
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,0xa(%di)
	mov    %dx,0xc(%di)
	mov    0x6(%bp),%ax
	mov    %ax,0xe(%di)
	mov    0x8(%bp),%ax
	mov    0xa(%bp),%dx
	mov    %ax,0x16(%di)
	mov    %dx,0x18(%di)
	mov    0x8(%bp),%ax
	mov    0xa(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	mov    %ax,0x12(%di)
	mov    %dx,0x14(%di)
	mov    0xc(%bp),%ax
	mov    %ax,0x1a(%di)
	mov    0xc(%bp),%bx
	mov    0x2(%bx),%ax
	mov    %ax,0x10(%di)
	mov    $D_1d2c,%si
L_2bd1:
	cmpw   $0x0,(%si)
	je   L_2bee
	mov    (%si),%bx
	mov    0x4(%di),%ax
	mov    0x6(%di),%dx
	cmp    0x6(%bx),%dx
	jg   L_2bea
	jl   L_2bee
	cmp    0x4(%bx),%ax
	jb   L_2bee
L_2bea:
	mov    (%si),%si
	jmp   L_2bd1
L_2bee:
	mov    (%si),%ax
	mov    %ax,(%di)
	mov    %di,(%si)
	mov    0x10(%di),%ax
L_2bf7:
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_2c07
	int    $0xf2
L_2c07:
	mov    $0xc,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   L_40a0
	add    $0x4,%sp
	mov    %ax,%di
	or     %ax,%ax
	jne   L_2c23
	mov    $D_1d6c,%ax
	push   %ax
	call   L_0036
	pop    %cx
L_2c23:
	mov    0x4(%bp),%ax
	mov    %ax,0x2(%di)
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,0x4(%di)
	mov    %dx,0x6(%di)
	mov    0xa(%bp),%ax
	mov    0xc(%bp),%dx
	mov    %ax,0x8(%di)
	mov    %dx,0xa(%di)
	lea    0xe(%bp),%ax
	mov    %ax,%si
L_2c46:
	cmpw   $0x0,(%si)
	je   L_2c59
	mov    (%si),%bx
	mov    0x4(%bp),%ax
	cmp    0x2(%bx),%ax
	jle   L_2c59
	mov    (%si),%si
	jmp   L_2c46
L_2c59:
	mov    (%si),%ax
	mov    %ax,(%di)
	mov    %di,(%si)
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2c65:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	cmp    $0x7f,%sp
	ja   L_2c74
	int    $0xf2
L_2c74:
	mov    D_1d2c,%di
L_2c78:
	or     %di,%di
	je   L_2cb3
	mov    0x8(%di),%si
	mov    0x2(%di),%ax
	cmp    0x4(%si),%ax
	jne   L_2caf
	testw  $0x2,0xc(%si)
	je   L_2caf
	mov    0x8(%di),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    0x12(%di),%ax
	mov    0x14(%di),%dx
	add    -0xc(%bp),%ax
	adc    -0xa(%bp),%dx
	mov    %ax,0x12(%di)
	mov    %dx,0x14(%di)
L_2caf:
	mov    (%di),%di
	jmp   L_2c78
L_2cb3:
	movw   $0x0,-0x8(%bp)
	mov    D_1d2c,%di
L_2cbc:
	or     %di,%di
	je   L_2cf2
	push   %di
	call   L_2e4e
	pop    %cx
	sub    0x10(%di),%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_2ce0
	pushw  -0x6(%bp)
	push   %di
	call   L_2cff
	add    $0x4,%sp
	movw   $0x1,-0x8(%bp)
	jmp   L_2cee
L_2ce0:
	cmpw   $0x0,-0x6(%bp)
	jge   L_2cee
	mov    $D_1d8d,%ax
	push   %ax
	call   L_0036
	pop    %cx
L_2cee:
	mov    (%di),%di
	jmp   L_2cbc
L_2cf2:
	cmpw   $0x0,-0x8(%bp)
	jne   L_2cb3
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2cff:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	cmp    $0x7f,%sp
	ja   L_2d0e
	int    $0xf2
L_2d0e:
	mov    0x6(%bp),%di
	mov    0x4(%bp),%bx
	add    %di,0x10(%bx)
	mov    0x4(%bp),%bx
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	mov    0x4(%bp),%bx
	pushw  0x2(%bx)
	call   L_2eb4
	add    $0x6,%sp
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x6(%bx),%dx
	add    -0xc(%bp),%ax
	adc    -0xa(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    D_1d2c,%si
L_2d4d:
	or     %si,%si
	jne   L_2d54
	jmp   L_2e47
L_2d54:
	cmp    0x4(%bp),%si
	jne   L_2d5c
	jmp   L_2e42
L_2d5c:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%ax
	mov    0x8(%si),%bx
	cmp    0x4(%bx),%ax
	je   L_2d6d
	jmp   L_2e42
L_2d6d:
	cmpw   $0x0,0x14(%si)
	jg   L_2dc2
	jl   L_2d7b
	cmpw   $0x0,0x12(%si)
	jae   L_2dc2
L_2d7b:
	mov    0x12(%si),%ax
	mov    0x14(%si),%dx
	add    0x16(%si),%ax
	adc    0x18(%si),%dx
	cmp    -0x6(%bp),%dx
	jg   L_2dc2
	jl   L_2d93
	cmp    -0x8(%bp),%ax
	ja   L_2dc2
L_2d93:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	cmp    0x18(%si),%dx
	jg   L_2dc2
	jl   L_2da5
	cmp    0x16(%si),%ax
	jae   L_2dc2
L_2da5:
	mov    %di,%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    0x12(%si),%ax
	mov    0x14(%si),%dx
	sub    -0xc(%bp),%ax
	sbb    -0xa(%bp),%dx
	mov    %ax,0x12(%si)
	mov    %dx,0x14(%si)
	jmp   L_2e15
L_2dc2:
	cmpw   $0x0,0x14(%si)
	jl   L_2e15
	jg   L_2dd0
	cmpw   $0x0,0x12(%si)
	jb   L_2e15
L_2dd0:
	mov    0x16(%si),%ax
	mov    0x18(%si),%dx
	cmp    -0x6(%bp),%dx
	jg   L_2e15
	jl   L_2de2
	cmp    -0x8(%bp),%ax
	ja   L_2e15
L_2de2:
	mov    0x12(%si),%ax
	mov    0x14(%si),%dx
	add    0x16(%si),%ax
	adc    0x18(%si),%dx
	cmp    %dx,-0x6(%bp)
	jg   L_2e15
	jl   L_2dfa
	cmp    %ax,-0x8(%bp)
	jae   L_2e15
L_2dfa:
	mov    %di,%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    0x12(%si),%ax
	mov    0x14(%si),%dx
	add    -0xc(%bp),%ax
	adc    -0xa(%bp),%dx
	mov    %ax,0x12(%si)
	mov    %dx,0x14(%si)
L_2e15:
	mov    0x16(%si),%ax
	mov    0x18(%si),%dx
	cmp    -0x6(%bp),%dx
	jl   L_2e42
	jg   L_2e27
	cmp    -0x8(%bp),%ax
	jbe   L_2e42
L_2e27:
	mov    %di,%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    0x16(%si),%ax
	mov    0x18(%si),%dx
	add    -0xc(%bp),%ax
	adc    -0xa(%bp),%dx
	mov    %ax,0x16(%si)
	mov    %dx,0x18(%si)
L_2e42:
	mov    (%si),%si
	jmp   L_2d4d
L_2e47:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2e4e:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_2e5a
	int    $0xf2
L_2e5a:
	mov    0x4(%bp),%di
	mov    0x8(%di),%bx
	mov    0xc(%bx),%ax
	test   $0x2,%ax
	je   L_2e74
	mov    0x8(%di),%bx
	mov    0x2(%di),%ax
	cmp    0x4(%bx),%ax
	je   L_2e79
L_2e74:
	mov    0xe(%di),%ax
	jmp   L_2eb0
L_2e79:
	mov    0x1a(%di),%si
L_2e7c:
	or     %si,%si
	je   L_2ead
	mov    0x4(%si),%ax
	mov    0x6(%si),%dx
	cmp    0x14(%di),%dx
	jg   L_2ea9
	jl   L_2e92
	cmp    0x12(%di),%ax
	ja   L_2ea9
L_2e92:
	mov    0x12(%di),%ax
	mov    0x14(%di),%dx
	cmp    0xa(%si),%dx
	jg   L_2ea9
	jl   L_2ea4
	cmp    0x8(%si),%ax
	ja   L_2ea9
L_2ea4:
	mov    0x2(%si),%ax
	jmp   L_2eb0
L_2ea9:
	mov    (%si),%si
	jmp   L_2e7c
L_2ead:
	mov    0xe(%di),%ax
L_2eb0:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2eb4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmp    $0x7f,%sp
	ja   L_2ec1
	int    $0xf2
L_2ec1:
	mov    0x4(%bp),%di
	mov    D_1d2c,%si
	movw   $0x0,-0x6(%bp)
L_2ece:
	or     %si,%si
	je   L_2ef9
	cmp    0x2(%si),%di
	jne   L_2ef5
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	cmp    0x6(%si),%dx
	jg   L_2ee9
	jl   L_2ef9
	cmp    0x4(%si),%ax
	jbe   L_2ef9
L_2ee9:
	mov    0x1a(%si),%bx
	mov    0x10(%si),%ax
	sub    0x2(%bx),%ax
	add    %ax,-0x6(%bp)
L_2ef5:
	mov    (%si),%si
	jmp   L_2ece
L_2ef9:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2f01:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_2f0d
	int    $0xf2
L_2f0d:
	mov    D_1d2c,%di
L_2f11:
	or     %di,%di
	je   L_2f27
	mov    (%di),%si
	pushw  0x1a(%di)
	call   L_2f31
	pop    %cx
	push   %di
	call   L_6132
	pop    %cx
	mov    %si,%di
	jmp   L_2f11
L_2f27:
	movw   $0x0,D_1d2c
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2f31:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_2f3d
	int    $0xf2
L_2f3d:
	mov    0x4(%bp),%di
L_2f41:
	or     %di,%di
	je   L_2f50
	mov    (%di),%si
	push   %di
	call   L_6132
	pop    %cx
	mov    %si,%di
	jmp   L_2f41
L_2f50:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2f54:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_2f60
	int    $0xf2
L_2f60:
	mov    D_1da0,%di
	or     %di,%di
	je   L_2f70
	mov    0x2(%di),%ax
	mov    %ax,D_1da0
	jmp   L_2faa
L_2f70:
	mov    $0xe,%ax
	push   %ax
	mov    $0x32,%ax
	push   %ax
	call   L_40a0
	add    $0x4,%sp
	mov    %ax,%di
	or     %di,%di
	jne   L_2f91
	xor    %ax,%ax
	push   %ax
	mov    $D_1da6,%ax
	push   %ax
	call   L_0036
	add    $0x4,%sp
L_2f91:
	mov    $0x31,%si
L_2f94:
	mov    %si,%ax
	dec    %si
	or     %ax,%ax
	je   L_2faa
	mov    D_1da0,%ax
	mov    %ax,0x2(%di)
	mov    %di,D_1da0
	add    $0xe,%di
	jmp   L_2f94
L_2faa:
	mov    %di,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2fb0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_2fbc
	int    $0xf2
L_2fbc:
	xor    %di,%di
L_2fbe:
	cmp    $0x100,%di
	jge   L_2fd1
	mov    %di,%bx
	shl    %bx
	movw   $0x0,D_2654(%bx)
	inc    %di
	jmp   L_2fbe
L_2fd1:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_2fd5:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	cmp    $0x7f,%sp
	ja   L_2fe4
	int    $0xf2
L_2fe4:
	mov    0x4(%bp),%di
	push   %di
	call   L_5050
	pop    %cx
	mov    %ax,-0x8(%bp)
	incw   -0x8(%bp)
	mov    -0x8(%bp),%ax
	cmp    D_1da4,%ax
	jle   L_3024
	mov    $0x1,%ax
	push   %ax
	mov    $0x200,%ax
	push   %ax
	call   L_40a0
	add    $0x4,%sp
	mov    %ax,D_1da2
	or     %ax,%ax
	jne   L_301e
	xor    %ax,%ax
	push   %ax
	mov    $D_1dbf,%ax
	push   %ax
	call   L_0036
	add    $0x4,%sp
L_301e:
	movw   $L_0200,D_1da4
L_3024:
	mov    D_1da2,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%si
L_302c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_302c
	mov    %si,D_1da2
	mov    -0x8(%bp),%ax
	sub    %ax,D_1da4
	mov    -0x6(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3052:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x36,%sp
	cmp    $0x7f,%sp
	ja   L_3061
	int    $0xf2
L_3061:
	mov    0x4(%bp),%di
	cmpb   $0x30,(%di)
	jl   L_30ab
	cmpb   $0x39,(%di)
	jg   L_30ab
	lea    -0x3a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_3075:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_3075
	decw   -0x8(%bp)
	mov    D_261e,%bx
	mov    (%bx),%di
L_3092:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_3092
	lea    -0x3a(%bp),%ax
	mov    %ax,%di
L_30ab:
	push   %di
	call   L_38a9
	pop    %cx
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	shl    %bx
	mov    D_2654(%bx),%si
L_30bb:
	or     %si,%si
	je   L_30d5
	push   %di
	pushw  (%si)
	call   L_623c
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_30d0
	mov    %si,%ax
	jmp   L_311c
L_30d0:
	mov    0x2(%si),%si
	jmp   L_30bb
L_30d5:
	call   L_2f54
	mov    %ax,%si
	push   %di
	call   L_2fd5
	pop    %cx
	mov    %ax,(%si)
	movw   $0x0,0xc(%si)
	movw   $0x0,0xa(%si)
	xor    %ax,%ax
	cwtd
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
	movw   $0x0,0x4(%si)
	mov    -0x6(%bp),%bx
	shl    %bx
	mov    D_2654(%bx),%ax
	mov    %ax,0x2(%si)
	lea    -0x3a(%bp),%ax
	cmp    %ax,%di
	jne   L_3111
	orw    $0x40,0xc(%si)
L_3111:
	mov    -0x6(%bp),%bx
	shl    %bx
	mov    %si,D_2654(%bx)
	mov    %si,%ax
L_311c:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3123:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	cmp    $0x7f,%sp
	ja   L_3132
	int    $0xf2
L_3132:
	movw   $0x0,-0x6(%bp)
	mov    $D_2654,%di
L_313a:
	cmp    $D_2854,%di
	jb   L_3143
	jmp   L_31fc
L_3143:
	mov    (%di),%si
L_3145:
	or     %si,%si
	jne   L_314c
	jmp   L_31f7
L_314c:
	testw  $0x3,0xc(%si)
	jne   L_315d
	orw    $0x5,0xc(%si)
	movw   $0x0,0x4(%si)
L_315d:
	pushw  0x8(%si)
	pushw  0x6(%si)
	pushw  0x4(%si)
	call   L_2eb4
	add    $0x6,%sp
	cwtd
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
	mov    0x6(%si),%ax
	mov    0x8(%si),%dx
	add    -0xa(%bp),%ax
	adc    -0x8(%bp),%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
	mov    0x4(%si),%ax
	cmp    D_1c6e,%ax
	jne   L_31a4
	mov    0x6(%si),%ax
	mov    0x8(%si),%dx
	/* TODO: data ref: add    0x2474,%ax */
	add    D_0474,%ax
	/* TODO: data ref: adc    0x2476,%dx */
	adc    D_0476,%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
	jmp   L_31d4
L_31a4:
	mov    0x4(%si),%ax
	cmp    D_1c70,%ax
	jne   L_31d4
	/* TODO: data ref: mov    0x2474,%ax */
	mov    D_0474,%ax
	/* TODO: data ref: mov    0x2476,%dx */
	mov    D_0476,%dx
	/* TODO: data ref: add    0x2478,%ax */
	add    D_0478,%ax
	/* TODO: data ref: adc    0x247a,%dx */
	adc    D_047a,%dx
	mov    %ax,-0xa(%bp)
	mov    %dx,-0x8(%bp)
	mov    0x6(%si),%ax
	mov    0x8(%si),%dx
	add    -0xa(%bp),%ax
	adc    -0x8(%bp),%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
L_31d4:
	cmp    D_2298,%si
	je   L_31e1
	testw  $0x170,0xc(%si)
	je   L_31e8
L_31e1:
	movw   $0xffff,0xa(%si)
	jmp   L_31f1
L_31e8:
	mov    -0x6(%bp),%ax
	mov    %ax,0xa(%si)
	incw   -0x6(%bp)
L_31f1:
	mov    0x2(%si),%si
	jmp   L_3145
L_31f7:
	inc    %di
	inc    %di
	jmp   L_313a
L_31fc:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3203:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x10,%sp
	cmp    $0x7f,%sp
	ja   L_3212
	int    $0xf2
L_3212:
	movw   $0x0,-0x8(%bp)
	movw   $0x2,-0xa(%bp)
	movb   $0x0,-0x11(%bp)
	movw   $0x0,-0x10(%bp)
	mov    $D_2654,%di
L_3228:
	cmp    $D_2854,%di
	jb   L_3231
	jmp   L_32c9
L_3231:
	mov    (%di),%si
L_3233:
	or     %si,%si
	jne   L_323a
	jmp   L_32c4
L_323a:
	cmpw   $0xffff,0xa(%si)
	je   L_32be
	testw  $0x2,0xc(%si)
	jne   L_324d
	movb   $0x0,-0x12(%bp)
	jmp   L_327e
L_324d:
	mov    0x4(%si),%ax
	cmp    D_1c6c,%ax
	jne   L_325c
	movb   $0x4,-0x12(%bp)
	jmp   L_327e
L_325c:
	mov    0x4(%si),%ax
	cmp    D_1c6e,%ax
	jne   L_326b
	movb   $0x6,-0x12(%bp)
	jmp   L_327e
L_326b:
	mov    0x4(%si),%ax
	cmp    D_1c70,%ax
	jne   L_327a
	movb   $0x8,-0x12(%bp)
	jmp   L_327e
L_327a:
	movb   $0x2,-0x12(%bp)
L_327e:
	testw  $0x4,0xc(%si)
	je   L_3289
	orb    $0x1,-0x12(%bp)
L_3289:
	mov    0x6(%si),%ax
	mov    0x8(%si),%dx
	mov    %ax,-0xe(%bp)
	mov    %dx,-0xc(%bp)
	mov    -0xa(%bp),%ax
	mov    %ax,-0x14(%bp)
	pushw  0x4(%bp)
	mov    $0x1,%ax
	push   %ax
	mov    $0xa,%ax
	push   %ax
	lea    -0x14(%bp),%ax
	push   %ax
	call   L_4581
	add    $0x8,%sp
	addw   $0xa,-0x8(%bp)
	pushw  (%si)
	call   L_5050
	pop    %cx
	inc    %ax
	add    %ax,-0xa(%bp)
L_32be:
	mov    0x2(%si),%si
	jmp   L_3233
L_32c4:
	inc    %di
	inc    %di
	jmp   L_3228
L_32c9:
	pushw  0x4(%bp)
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	lea    -0xa(%bp),%ax
	push   %ax
	call   L_4581
	add    $0x8,%sp
	mov    $D_2654,%di
L_32e1:
	cmp    $D_2854,%di
	jae   L_333d
	mov    (%di),%si
L_32e9:
	or     %si,%si
	je   L_3339
	cmpw   $0xffff,0xa(%si)
	je   L_3334
	mov    (%si),%ax
	mov    %ax,-0x6(%bp)
L_32f8:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_3319
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%dx
	dec    %dx
	mov    %dx,%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %cl,%al
	cbtw
	jmp   L_3329
L_3319:
	pushw  0x4(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_5e14
	add    $0x4,%sp
L_3329:
	mov    -0x6(%bp),%bx
	incw   -0x6(%bp)
	cmpb   $0x0,(%bx)
	jne   L_32f8
L_3334:
	mov    0x2(%si),%si
	jmp   L_32e9
L_3339:
	inc    %di
	inc    %di
	jmp   L_32e1
L_333d:
	mov    -0x8(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3347:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_3353
	int    $0xf2
L_3353:
	mov    $D_2654,%di
L_3356:
	cmp    $D_2854,%di
	jae   L_3370
	mov    (%di),%si
L_335e:
	or     %si,%si
	je   L_336c
	orw    $0x100,0xc(%si)
	mov    0x2(%si),%si
	jmp   L_335e
L_336c:
	inc    %di
	inc    %di
	jmp   L_3356
L_3370:
	pop    %di
	pop    %si
	pop    %bp
	ret


	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	/* vrewrite: initialize _environ from envp (TLVC crt0 3rd arg) */
	mov	0x8(%bp),%ax
	mov	%ax,D_222c
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_3380
	int    $0xf2
L_3380:
	pushw  0x6(%bp)
	pushw  0x4(%bp)
	call   L_00da
	add    $0x4,%sp
	call   L_33af
	call   L_21f0
	call   L_33af
	call   L_21f0
	/* TODO: data ref: cmpw   $0x0,0x205e */
	cmpw   $0x0,D_005e
	je   L_33a4
	mov    $0xffff,%ax
	jmp   L_33a6
L_33a4:
	xor    %ax,%ax
L_33a6:
	push   %ax
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_33af:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	cmp    $0x7f,%sp
	ja   L_33be
	int    $0xf2
L_33be:
	mov    $D_2182,%ax
	push   %ax
	mov    $0xc8,%ax
	push   %ax
	mov    $D_2854,%ax
	push   %ax
	call   L_41c2
	add    $0x6,%sp
	cmp    $D_2854,%ax
	je   L_33d8
	jmp   L_35d0
L_33d8:
	cmpb   $0x23,D_2854
	jne   L_33fb
	mov    $D_2234,%ax
	push   %ax
	mov    $D_222e,%ax
	push   %ax
	mov    $D_1eea,%ax
	push   %ax
	mov    $D_2854,%ax
	push   %ax
	call   L_4621
	add    $0x8,%sp
	decw   D_222e
L_33f9:
	jmp   L_33be
L_33fb:
	incw   D_222e
	mov    $D_2854,%si
	movw   $0x0,D_2568
	movb   $0x0,D_256a
L_340d:
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x5,D_1dd8(%bx)
	jne   L_341e
	inc    %si
	jmp   L_340d
L_341e:
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%di
	cmp    $0x10,%di
	jne   L_3431
	inc    %si
L_342f:
	jmp   L_340d
L_3431:
	cmp    $0x2,%di
	je   L_33f9
	test   $0x300,%di
	jne   L_3446
	mov    $0x2,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_3444:
	jmp   L_33f9
L_3446:
	mov    %si,-0x6(%bp)
L_3449:
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	test   $0x400,%ax
	jne   L_345e
	cmpb   $0x24,(%si)
	jne   L_3461
L_345e:
	inc    %si
	jmp   L_3449
L_3461:
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x5,D_1dd8(%bx)
	jne   L_3472
	inc    %si
	jmp   L_3461
L_3472:
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%di
	or     %di,%di
	jne   L_348b
	inc    %si
	pushw  -0x6(%bp)
	call   L_3659
	pop    %cx
L_3489:
	jmp   L_342f
L_348b:
	cmp    $0x1,%di
	jne   L_34bc
	pushw  -0x6(%bp)
	inc    %si
	push   %si
	call   L_3727
	add    $0x4,%sp
	mov    %ax,%si
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%di
	cmp    $0x2,%di
	je   L_3489
	cmp    $0x10,%di
	je   L_3489
	mov    $0x2,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_34ba:
	jmp   L_3489
L_34bc:
	pushw  -0x6(%bp)
	call   L_35d7
	pop    %cx
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jne   L_34d5
	mov    $0x11,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_34d2:
	jmp   L_3444
L_34d5:
	cmp    $0x2,%di
	je   L_34df
	cmp    $0x10,%di
	jne   L_34e7
L_34df:
	movw   $0x0,D_261c
	jmp   L_3552
L_34e7:
	movw   $0x1,D_261c
L_34ed:
	cmpw   $0x10,D_261c
	jg   L_3548
	mov    D_261c,%ax
	dec    %ax
	mov    $0xa,%dx
	imul   %dx
	add    $D_257c,%ax
	push   %ax
	push   %si
	call   L_38f6
	add    $0x4,%sp
	mov    %ax,%si
L_350a:
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x5,D_1dd8(%bx)
	jne   L_351b
	inc    %si
	jmp   L_350a
L_351b:
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%di
	cmp    $0xa,%di
	jne   L_352e
	inc    %si
	jmp   L_3542
L_352e:
	cmp    $0x2,%di
	je   L_3552
	cmp    $0x10,%di
	je   L_3552
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_3540:
	jmp   L_34d2
L_3542:
	incw   D_261c
	jmp   L_34ed
L_3548:
	mov    $0x19,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_3540
L_3552:
	movw   $0x0,D_2568
	pushw  -0x8(%bp)
	call   L_047e
	pop    %cx
	mov    D_2568,%ax
	cwtd
	mov    %ax,-0xc(%bp)
	mov    %dx,-0xa(%bp)
	mov    D_2578,%ax
	mov    D_257a,%dx
	add    -0xc(%bp),%ax
	adc    -0xa(%bp),%dx
	mov    %ax,D_2578
	mov    %dx,D_257a
	mov    D_1c6a,%bx
	mov    D_2578,%ax
	mov    D_257a,%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	mov    D_2298,%bx
	mov    D_2578,%ax
	mov    D_257a,%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	mov    D_1c6a,%bx
	mov    D_2578,%ax
	mov    D_257a,%dx
	cmp    0x4(%bx),%dx
	jge   L_35b2
	jmp   L_34ba
L_35b2:
	jg   L_35bc
	cmp    0x2(%bx),%ax
	ja   L_35bc
	jmp   L_34ba
L_35bc:
	mov    D_1c6a,%bx
	mov    D_2578,%ax
	mov    D_257a,%dx
	mov    %ax,0x2(%bx)
	mov    %dx,0x4(%bx)
	jmp   L_34ba
L_35d0:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_35d7:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x34,%sp
	cmp    $0x7f,%sp
	ja   L_35e6
	int    $0xf2
L_35e6:
	mov    0x4(%bp),%di
	lea    -0x38(%bp),%ax
	mov    %ax,%si
L_35ef:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	test   $0x400,%ax
	je   L_360e
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_35ef
L_360e:
	movb   $0x0,(%si)
	lea    -0x38(%bp),%ax
	push   %ax
	call   L_38a9
	pop    %cx
	mov    %ax,%bx
	shl    %bx
	mov    D_2304(%bx),%ax
	mov    %ax,-0x6(%bp)
L_3624:
	cmpw   $0x0,-0x6(%bp)
	je   L_3650
	lea    -0x38(%bp),%ax
	push   %ax
	mov    -0x6(%bp),%bx
	pushw  0x2(%bx)
	call   L_623c
	add    $0x4,%sp
	or     %ax,%ax
	jne   L_3646
	mov    -0x6(%bp),%bx
	mov    0x4(%bx),%ax
	jmp   L_3652
L_3646:
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    %ax,-0x6(%bp)
	jmp   L_3624
L_3650:
	xor    %ax,%ax
L_3652:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3659:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x34,%sp
	cmp    $0x7f,%sp
	ja   L_3668
	int    $0xf2
L_3668:
	mov    0x4(%bp),%di
	lea    -0x38(%bp),%ax
	mov    %ax,%si
L_3671:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	test   $0x400,%ax
	jne   L_3686
	cmpb   $0x24,(%di)
	jne   L_3695
L_3686:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_3671
L_3695:
	movb   $0x0,(%si)
	lea    -0x38(%bp),%ax
	push   %ax
	call   L_3052
	pop    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: cmpw   $0x1,0x2472 */
	cmpw   $0x1,D_0472
	jne   L_36e0
	mov    -0x6(%bp),%bx
	mov    0xc(%bx),%ax
	test   $0x28,%ax
	je   L_36bd
	mov    $0x3,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_36bd:
	mov    -0x6(%bp),%bx
	orw    $0xb,0xc(%bx)
	mov    -0x6(%bp),%bx
	mov    D_1c6a,%ax
	mov    %ax,0x4(%bx)
	mov    -0x6(%bp),%bx
	mov    D_2578,%ax
	mov    D_257a,%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	jmp   L_3709
L_36e0:
	mov    -0x6(%bp),%bx
	mov    0x4(%bx),%ax
	cmp    D_1c6a,%ax
	jne   L_3701
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    D_257a,%dx
	jne   L_3701
	cmp    D_2578,%ax
	je   L_3709
L_3701:
	mov    $0x3,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_3709:
	mov    -0x38(%bp),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	test   $0x100,%ax
	jne   L_3720
	mov    -0x6(%bp),%ax
	mov    %ax,D_261e
L_3720:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3727:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x44,%sp
	cmp    $0x7f,%sp
	ja   L_3736
	int    $0xf2
L_3736:
	mov    0x4(%bp),%si
	mov    0x6(%bp),%di
	lea    -0x44(%bp),%ax
	mov    %ax,-0x6(%bp)
L_3744:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	test   $0x400,%ax
	je   L_3766
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    -0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	jmp   L_3744
L_3766:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	lea    -0x44(%bp),%ax
	push   %ax
	call   L_3052
	pop    %cx
	mov    %ax,-0x8(%bp)
	lea    -0x12(%bp),%ax
	push   %ax
	push   %si
	call   L_3ac3
	add    $0x4,%sp
	mov    %ax,%si
	mov    -0x8(%bp),%ax
	cmp    D_2298,%ax
	je   L_3790
	jmp   L_383b
L_3790:
	cmpw   $0x0,-0xe(%bp)
	je   L_37aa
	mov    -0xe(%bp),%bx
	mov    0x4(%bx),%ax
	cmp    D_1c6a,%ax
	je   L_37aa
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_37aa:
	mov    D_2578,%ax
	mov    D_257a,%dx
	cmp    -0xa(%bp),%dx
	jl   L_37c7
	jg   L_37bd
	cmp    -0xc(%bp),%ax
	jbe   L_37c7
L_37bd:
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_37dd
L_37c7:
	mov    -0xc(%bp),%ax
	mov    -0xa(%bp),%dx
	sub    D_2578,%ax
	sbb    D_257a,%dx
	push   %dx
	push   %ax
	call   L_2982
	add    $0x4,%sp
L_37dd:
	mov    -0xc(%bp),%ax
	mov    -0xa(%bp),%dx
	mov    %ax,D_2578
	mov    %dx,D_257a
	mov    D_2298,%bx
	mov    D_2578,%ax
	mov    D_257a,%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	mov    %ax,-0x48(%bp)
	mov    %dx,-0x46(%bp)
	mov    D_1c6a,%bx
	mov    -0x48(%bp),%ax
	mov    -0x46(%bp),%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	mov    D_1c6a,%bx
	mov    D_2578,%ax
	mov    D_257a,%dx
	cmp    0x4(%bx),%dx
	jl   L_3873
	jg   L_3828
	cmp    0x2(%bx),%ax
	jbe   L_3873
L_3828:
	mov    D_1c6a,%bx
	mov    D_2578,%ax
	mov    D_257a,%dx
	mov    %ax,0x2(%bx)
	mov    %dx,0x4(%bx)
	jmp   L_3873
L_383b:
	mov    -0x8(%bp),%bx
	mov    -0xc(%bp),%ax
	mov    -0xa(%bp),%dx
	mov    %ax,0x6(%bx)
	mov    %dx,0x8(%bx)
	cmpw   $0x0,-0xe(%bp)
	je   L_3858
	mov    -0xe(%bp),%bx
	mov    0x4(%bx),%ax
	jmp   L_385a
L_3858:
	xor    %ax,%ax
L_385a:
	mov    -0x8(%bp),%bx
	mov    %ax,0x4(%bx)
	mov    -0x8(%bp),%bx
	mov    0xc(%bx),%ax
	test   $0x8,%ax
	je   L_3875
	mov    $0xf,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_3873:
	jmp   L_388f
L_3875:
	cmpw   $0x0,-0xe(%bp)
	je   L_3886
	mov    -0xe(%bp),%bx
	mov    0xc(%bx),%ax
	and    $0xfef7,%ax
	jmp   L_3889
L_3886:
	mov    $0x3,%ax
L_3889:
	mov    -0x8(%bp),%bx
	mov    %ax,0xc(%bx)
L_388f:
	mov    (%si),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x5,D_1dd8(%bx)
	jne   L_38a0
	inc    %si
	jmp   L_388f
L_38a0:
	mov    %si,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_38a9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_38b5
	int    $0xf2
L_38b5:
	mov    0x4(%bp),%di
	xor    %si,%si
L_38bb:
	cmpb   $0x0,(%di)
	je   L_38dc
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
	sub    $0x20,%ax
	mov    %ax,%si
	jmp   L_38bb
L_38dc:
	mov    %si,%ax
	cwtd
	mov    $0x100,%cx
	idiv   %cx
	mov    %dx,%si
	or     %si,%si
	jge   L_38f0
	lea    0x100(%si),%ax
	jmp   L_38f2
L_38f0:
	mov    %si,%ax
L_38f2:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_38f6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xc,%sp
	cmp    $0x7f,%sp
	ja   L_3905
	int    $0xf2
L_3905:
	mov    0x4(%bp),%di
	mov    0x6(%bp),%bx
	movb   $0x0,0x2(%bx)
	mov    0x6(%bp),%bx
	movb   $0x0,0x1(%bx)
	mov    0x6(%bp),%bx
	movb   $0x0,(%bx)
	mov    0x6(%bp),%bx
	movw   $0x0,0x6(%bx)
	movw   $0x0,0x8(%bx)
	mov    0x6(%bp),%bx
	movw   $0x0,0x4(%bx)
L_3932:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x5,D_1dd8(%bx)
	jne   L_3943
	inc    %di
	jmp   L_3932
L_3943:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0xf,D_1dd8(%bx)
	jne   L_395a
	mov    0x6(%bp),%bx
	orb    $0x10,(%bx)
	inc    %di
	jmp   L_3932
L_395a:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0xb,D_1dd8(%bx)
	jne   L_3971
	mov    0x6(%bp),%bx
	orb    $0x20,(%bx)
	inc    %di
	jmp   L_3932
L_3971:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x7,D_1dd8(%bx)
	jne   L_39ac
	pushw  0x6(%bp)
	lea    0x1(%di),%ax
	push   %ax
	call   L_3ac3
	add    $0x4,%sp
	mov    %ax,%di
	mov    0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	and    $0xf,%ax
	cmp    $0x1,%ax
	je   L_39a4
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_39a4:
	mov    0x6(%bp),%bx
	addb   $0x2,(%bx)
	jmp   L_39e5
L_39ac:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0xc,D_1dd8(%bx)
	jne   L_39e7
	pushw  0x6(%bp)
	lea    0x1(%di),%ax
	push   %ax
	call   L_3ac3
	add    $0x4,%sp
	mov    %ax,%di
	mov    0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	and    $0xf,%ax
	cmp    $0x1,%ax
	je   L_39df
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_39df:
	mov    0x6(%bp),%bx
	addb   $0x3,(%bx)
L_39e5:
	jmp   L_3a12
L_39e7:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x8,D_1dd8(%bx)
	je   L_3a12
	pushw  0x6(%bp)
	push   %di
	call   L_3ac3
	add    $0x4,%sp
	mov    %ax,%di
L_3a01:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x5,D_1dd8(%bx)
	jne   L_3a12
	inc    %di
	jmp   L_3a01
L_3a12:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x8,D_1dd8(%bx)
	jne   L_3a92
	movb   $0x0,-0xe(%bp)
	movb   $0x0,-0xf(%bp)
	movb   $0x0,-0x10(%bp)
	movw   $0x0,-0xa(%bp)
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0xc(%bp)
	lea    -0x10(%bp),%ax
	push   %ax
	lea    0x1(%di),%ax
	push   %ax
	call   L_3ac3
	add    $0x4,%sp
	mov    %ax,%di
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x9,D_1dd8(%bx)
	je   L_3a63
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
	jmp   L_3a64
L_3a63:
	inc    %di
L_3a64:
	cmpb   $0x2,-0x10(%bp)
	je   L_3a72
	mov    $0xd,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_3a72:
	mov    -0xa(%bp),%al
	cbtw
	mov    0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,0x2(%bx)
	mov    0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	and    $0xf,%ax
	cmp    $0x4,%ax
	jne   L_3a94
	mov    0x6(%bp),%bx
	addb   $0x3,(%bx)
L_3a92:
	jmp   L_3aba
L_3a94:
	mov    0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	test   $0xf,%ax
	jne   L_3aa7
	mov    0x6(%bp),%bx
	orb    $0x5,(%bx)
	jmp   L_3aba
L_3aa7:
	mov    0x6(%bp),%bx
	mov    (%bx),%al
	cbtw
	and    $0xfff0,%ax
	or     $0x6,%ax
	mov    0x6(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
L_3aba:
	mov    %di,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3ac3:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xe,%sp
	cmp    $0x7f,%sp
	ja   L_3ad2
	int    $0xf2
L_3ad2:
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_3ada:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x5,D_1dd8(%bx)
	jne   L_3aeb
	inc    %di
	jmp   L_3ada
L_3aeb:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	mov    %ax,-0x10(%bp)
	cmp    $0x2,%ax
	je   L_3b04
	cmpw   $0xa,-0x10(%bp)
	jne   L_3b18
L_3b04:
	movw   $0x0,0x4(%si)
	movw   $0x0,0x6(%si)
	movw   $0x0,0x8(%si)
	mov    %di,%ax
	jmp   L_3c6a
L_3b18:
	push   %si
	push   %di
	call   L_3c9c
	add    $0x4,%sp
	mov    %ax,%di
L_3b22:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x5,D_1dd8(%bx)
	jne   L_3b33
	inc    %di
L_3b31:
	jmp   L_3b22
L_3b33:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	jmp   L_3c6c
L_3b41:
	lea    -0xe(%bp),%ax
	push   %ax
	inc    %di
	push   %di
	call   L_3c9c
	add    $0x4,%sp
	mov    %ax,%di
	cmpb   $0x2,(%si)
	je   L_3ba3
	cmpb   $0x2,-0xe(%bp)
	je   L_3ba9
	cmpw   $0x0,0x4(%si)
	je   L_3b66
	cmpw   $0x0,-0xa(%bp)
	jne   L_3bb5
L_3b66:
	cmpw   $0x0,-0xa(%bp)
	je   L_3b72
	mov    -0xa(%bp),%ax
	mov    %ax,0x4(%si)
L_3b72:
	mov    0x6(%si),%ax
	mov    0x8(%si),%dx
	add    -0x8(%bp),%ax
	adc    -0x6(%bp),%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
	mov    -0xd(%bp),%al
	cbtw
	and    $0x4,%ax
	mov    %ax,%cx
	or     %cl,0x1(%si)
L_3b90:
	jmp   L_3b31
L_3b92:
	lea    -0xe(%bp),%ax
	push   %ax
	inc    %di
	push   %di
	call   L_3c9c
	add    $0x4,%sp
	mov    %ax,%di
	cmpb   $0x2,(%si)
L_3ba3:
	je   L_3bec
	cmpb   $0x2,-0xe(%bp)
L_3ba9:
	je   L_3bec
	cmpw   $0x0,0x4(%si)
	jne   L_3bf8
	cmpw   $0x0,-0xa(%bp)
L_3bb5:
	jne   L_3bf8
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	lea    0x6(%si),%ax
	push   %ax
	call   L_567c
	add    $0x6,%sp
	mov    -0xd(%bp),%al
	cbtw
	and    $0x4,%ax
	mov    %ax,%cx
	or     %cl,0x1(%si)
L_3bd3:
	jmp   L_3b90
L_3bd5:
	lea    -0xe(%bp),%ax
	push   %ax
	inc    %di
	push   %di
	call   L_3c9c
	add    $0x4,%sp
	mov    %ax,%di
	cmpb   $0x2,(%si)
	je   L_3c2f
	cmpb   $0x2,-0xe(%bp)
L_3bec:
	je   L_3c3b
	cmpw   $0x0,0x4(%si)
	jne   L_3bf8
	cmpw   $0x0,-0xa(%bp)
L_3bf8:
	jne   L_3c49
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	lea    0x6(%si),%ax
	push   %ax
	call   L_5725
	add    $0x6,%sp
	mov    -0xd(%bp),%al
	cbtw
	and    $0x4,%ax
	mov    %ax,%cx
	or     %cl,0x1(%si)
L_3c16:
	jmp   L_3bd3
L_3c18:
	lea    -0xe(%bp),%ax
	push   %ax
	inc    %di
	push   %di
	call   L_3c9c
	add    $0x4,%sp
	mov    %ax,%di
	cmpb   $0x2,(%si)
	je   L_3c3b
	cmpb   $0x2,-0xe(%bp)
L_3c2f:
	je   L_3c8b
	cmpw   $0x0,-0xa(%bp)
	je   L_3c54
	cmpw   $0x0,0x4(%si)
L_3c3b:
	je   L_3c8b
	mov    -0xa(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x4(%si),%bx
	cmp    0x4(%bx),%ax
L_3c49:
	jne   L_3c8b
	movw   $0x0,0x4(%si)
	orb    $0x4,0x1(%si)
L_3c54:
	mov    0x6(%si),%ax
	mov    0x8(%si),%dx
	sub    -0x8(%bp),%ax
	sbb    -0x6(%bp),%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
	jmp   L_3c16
L_3c68:
	mov    %di,%ax
L_3c6a:
	jmp   L_3c95
L_3c6c:
	cmp    $0x3,%ax
	jne   L_3c74
	jmp   L_3b41
L_3c74:
	cmp    $0x4,%ax
	je   L_3c18
	cmp    $0xc,%ax
	jne   L_3c81
	jmp   L_3b92
L_3c81:
	cmp    $0x12,%ax
	jne   L_3c89
	jmp   L_3bd5
L_3c89:
	jmp   L_3c68
L_3c8b:
	mov    $0xb,%ax
	push   %ax
	call   L_0067
	pop    %cx
	mov    %di,%ax
L_3c95:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_3c9c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x46,%sp
	cmp    $0x7f,%sp
	ja   L_3cab
	int    $0xf2
L_3cab:
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	movw   $0xa,-0xc(%bp)
L_3cb8:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	cmpw   $0x5,D_1dd8(%bx)
	jne   L_3cc9
	inc    %di
	jmp   L_3cb8
L_3cc9:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	mov    %ax,-0x6(%bp)
	testw  $0x100,-0x6(%bp)
	jne   L_3ce1
	jmp   L_3e4c
L_3ce1:
	mov    %di,-0xa(%bp)
	cmpb   $0x30,(%di)
	jne   L_3d01
	inc    %di
	cmpb   $0x78,(%di)
	je   L_3cf4
	cmpb   $0x58,(%di)
	jne   L_3cfc
L_3cf4:
	inc    %di
	movw   $0x10,-0xc(%bp)
	jmp   L_3d01
L_3cfc:
	movw   $0x8,-0xc(%bp)
L_3d01:
	movw   $0x0,-0x10(%bp)
	movw   $0x0,-0xe(%bp)
	cmpw   $0x10,-0xc(%bp)
	je   L_3d14
	jmp   L_3e27
L_3d14:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	test   $0x100,%ax
	je   L_3d58
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	cwtd
	mov    %ax,-0x46(%bp)
	mov    %dx,-0x44(%bp)
	mov    -0x10(%bp),%ax
	mov    -0xe(%bp),%dx
	mov    $0x4,%cx
	or     %cx,%cx
	je   L_3d44
L_3d3e:
	shl    %ax
	rcl    %dx
	loop   L_3d3e
L_3d44:
	add    -0x46(%bp),%ax
	adc    -0x44(%bp),%dx
	sub    $0x30,%ax
	sbb    $0x0,%dx
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
L_3d56:
	jmp   L_3d14
L_3d58:
	cmpb   $0x41,(%di)
	jl   L_3d96
	cmpb   $0x46,(%di)
	jg   L_3d96
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	cwtd
	mov    %ax,-0x46(%bp)
	mov    %dx,-0x44(%bp)
	mov    -0x10(%bp),%ax
	mov    -0xe(%bp),%dx
	mov    $0x4,%cx
	or     %cx,%cx
	je   L_3d82
L_3d7c:
	shl    %ax
	rcl    %dx
	loop   L_3d7c
L_3d82:
	add    -0x46(%bp),%ax
	adc    -0x44(%bp),%dx
	sub    $0x37,%ax
	sbb    $0x0,%dx
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
L_3d94:
	jmp   L_3d56
L_3d96:
	cmpb   $0x61,(%di)
	jge   L_3d9e
	jmp   L_3e29
L_3d9e:
	cmpb   $0x66,(%di)
	jle   L_3da6
	jmp   L_3e29
L_3da6:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	cwtd
	mov    %ax,-0x46(%bp)
	mov    %dx,-0x44(%bp)
	mov    -0x10(%bp),%ax
	mov    -0xe(%bp),%dx
	mov    $0x4,%cx
	or     %cx,%cx
	je   L_3dc6
L_3dc0:
	shl    %ax
	rcl    %dx
	loop   L_3dc0
L_3dc6:
	add    -0x46(%bp),%ax
	adc    -0x44(%bp),%dx
	sub    $0x57,%ax
	sbb    $0x0,%dx
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
	jmp   L_3d94
L_3dda:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	test   $0x100,%ax
	je   L_3e29
	mov    -0xc(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0xe(%bp)
	pushw  -0x10(%bp)
	call   L_567f
	add    $0x8,%sp
	mov    %ax,-0x46(%bp)
	mov    %dx,-0x44(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	cwtd
	mov    %ax,-0x4a(%bp)
	mov    %dx,-0x48(%bp)
	mov    -0x46(%bp),%ax
	mov    -0x44(%bp),%dx
	add    -0x4a(%bp),%ax
	adc    -0x48(%bp),%dx
	sub    $0x30,%ax
	sbb    $0x0,%dx
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
L_3e27:
	jmp   L_3dda
L_3e29:
	cmpb   $0x24,(%di)
	jne   L_3e33
	mov    -0xa(%bp),%di
	jmp   L_3e56
L_3e33:
	mov    -0x10(%bp),%ax
	mov    -0xe(%bp),%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
	movw   $0x0,0x4(%si)
	orb    $0x1,(%si)
	mov    %di,%ax
	jmp   L_3efa
L_3e4c:
	testw  $0x200,-0x6(%bp)
	jne   L_3e56
	jmp   L_3efc
L_3e56:
	lea    -0x42(%bp),%ax
	mov    %ax,-0xa(%bp)
L_3e5c:
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	test   $0x400,%ax
	jne   L_3e71
	cmpb   $0x24,(%di)
	jne   L_3e83
L_3e71:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    -0xa(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_3e5c
L_3e83:
	mov    -0xa(%bp),%bx
	movb   $0x0,(%bx)
	lea    -0x42(%bp),%ax
	push   %ax
	call   L_3052
	pop    %cx
	mov    %ax,-0x8(%bp)
	mov    %ax,%bx
	mov    0xc(%bx),%ax
	test   $0x2,%ax
	je   L_3eaf
	mov    -0x8(%bp),%bx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
	jmp   L_3eb9
L_3eaf:
	movw   $0x0,0x6(%si)
	movw   $0x0,0x8(%si)
L_3eb9:
	mov    -0x8(%bp),%bx
	mov    0xc(%bx),%ax
	test   $0x20,%ax
	je   L_3ec9
	orb    $0x2,(%si)
	jmp   L_3ef8
L_3ec9:
	mov    -0x8(%bp),%bx
	mov    0xc(%bx),%ax
	test   $0x2,%ax
	je   L_3eef
	mov    -0x8(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	je   L_3eeb
	mov    -0x8(%bp),%bx
	mov    0x4(%bx),%bx
	mov    0xc(%bx),%ax
	test   $0x8,%ax
	je   L_3eef
L_3eeb:
	xor    %ax,%ax
	jmp   L_3ef2
L_3eef:
	mov    -0x8(%bp),%ax
L_3ef2:
	mov    %ax,0x4(%si)
	orb    $0x1,(%si)
L_3ef8:
	mov    %di,%ax
L_3efa:
	jmp   L_3f30
L_3efc:
	cmpw   $0x4,-0x6(%bp)
	jne   L_3f32
	push   %si
	inc    %di
	push   %di
	call   L_3c9c
	add    $0x4,%sp
	mov    %ax,%di
	cmpw   $0x0,0x4(%si)
	je   L_3f1b
	mov    $0xb,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_3f1b:
	mov    0x6(%si),%ax
	mov    0x8(%si),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
	mov    %di,%ax
L_3f30:
	jmp   L_3f63
L_3f32:
	cmpw   $0xd,-0x6(%bp)
	jne   L_3f65
	push   %si
	inc    %di
	push   %di
	call   L_3c9c
	add    $0x4,%sp
	mov    %ax,%di
	cmpw   $0x0,0x4(%si)
	je   L_3f51
	mov    $0xb,%ax
	push   %ax
	call   L_0067
	pop    %cx
L_3f51:
	mov    0x6(%si),%ax
	mov    0x8(%si),%dx
	not    %ax
	not    %dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
	mov    %di,%ax
L_3f63:
	jmp   L_3f81
L_3f65:
	cmpw   $0x11,-0x6(%bp)
	jne   L_3f83
	mov    0x1(%di),%ax
	cbtw
	cwtd
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
	movw   $0x0,0x4(%si)
	movb   $0x1,(%si)
	lea    0x2(%di),%ax
L_3f81:
	jmp   L_3fca
L_3f83:
	cmpw   $0xe,-0x6(%bp)
	jne   L_3fc0
	lea    0x1(%di),%ax
	xor    %dx,%dx
	mov    %ax,0x6(%si)
	mov    %dx,0x8(%si)
L_3f94:
	inc    %di
	mov    (%di),%al
	cbtw
	mov    %ax,%bx
	shl    %bx
	mov    D_1dd8(%bx),%ax
	mov    %ax,-0x6(%bp)
	cmp    $0x2,%ax
	je   L_3fae
	cmpw   $0xe,-0x6(%bp)
	jne   L_3f94
L_3fae:
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	movw   $0x0,0x4(%si)
	orb    $0x8,(%si)
	mov    %di,%ax
	jmp   L_3fca
L_3fc0:
	mov    $0x9,%ax
	push   %ax
	call   L_0067
	pop    %cx
	mov    %di,%ax
L_3fca:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmp    $0x7f,%sp
	ja   L_3fdd
	int    $0xf2
L_3fdd:
	mov    0x4(%bp),%di
	mov    (%di),%al
	cbtw
	and    $0xf,%ax
	mov    %ax,%bx
	shl    %bx
	pushw  D_1ed8(%bx)
	pushw  D_261c
	mov    $D_1f1c,%ax
	push   %ax
	mov    $D_2192,%ax
	push   %ax
	call   L_4196
	add    $0x8,%sp
	mov    (%di),%al
	cbtw
	test   $0x20,%ax
	je   L_402d
	decw   D_2194
	jl   L_401f
	mov    D_2192,%bx
	incw   D_2192
	movb   $0x40,(%bx)
	mov    $0x40,%ax
	jmp   L_402d
L_401f:
	mov    $D_2192,%ax
	push   %ax
	mov    $0x40,%ax
	push   %ax
	call   L_5e14
	add    $0x4,%sp
L_402d:
	mov    (%di),%al
	cbtw
	test   $0x10,%ax
	je   L_4059
	decw   D_2194
	jl   L_404b
	mov    D_2192,%bx
	incw   D_2192
	movb   $0x24,(%bx)
	mov    $0x24,%ax
	jmp   L_4059
L_404b:
	mov    $D_2192,%ax
	push   %ax
	mov    $0x24,%ax
	push   %ax
	call   L_5e14
	add    $0x4,%sp
L_4059:
	cmpw   $0x0,0x4(%di)
	je   L_407a
	mov    0x4(%di),%bx
	pushw  (%bx)
	mov    0x4(%di),%bx
	mov    0x4(%bx),%bx
	pushw  (%bx)
	mov    $D_1f30,%ax
	push   %ax
	mov    $D_2192,%ax
	push   %ax
	call   L_4196
	add    $0x8,%sp
L_407a:
	mov    0x2(%di),%al
	cbtw
	push   %ax
	pushw  0x8(%di)
	pushw  0x6(%di)
	mov    $D_1f41,%ax
	push   %ax
	mov    $D_2192,%ax
	push   %ax
	call   L_4196
	add    $0xa,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4098:
	push   %bp
	mov    %sp,%bp
	int    $0xf3
L_409d:
	jmp   L_409d
	/* .byte 0 (alignment) */


L_40a0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%ax
	mulw   0x6(%bp)
	mov    %ax,0x4(%bp)
	pushw  0x4(%bp)
	call   L_5fe8
	pop    %cx
	mov    %ax,%di
	or     %di,%di
	jne   L_40c0
	xor    %ax,%ax
	jmp   L_40e1
L_40c0:
	mov    %di,%si
L_40c2:
	mov    0x4(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
L_40d0:
	decw   -0x6(%bp)
	jl   L_40df
	mov    %si,%bx
	inc    %si
	inc    %si
	movw   $0x0,(%bx)
	jmp   L_40d0
L_40df:
	mov    %di,%ax
L_40e1:
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
	call   L_6132
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_40f6:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	push   %si
	call   L_5f71
	pop    %cx
	cmpb   $0x77,(%di)
	jne   L_4120
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4322
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	jmp   L_415c
L_4120:
	cmpb   $0x61,(%di)
	jne   L_415e
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_62ba
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jge   L_4149
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4322
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_4149:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	call   L_506c
	add    $0x8,%sp
L_415c:
	jmp   L_416d
L_415e:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_62ba
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_416d:
	cmpw   $0x0,-0x6(%bp)
	jge   L_4177
	xor    %ax,%ax
	jmp   L_4190
L_4177:
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,0x7(%si)
	cmpb   $0x72,(%di)
L_4182:
	je   L_418a
	orb    $0x2,0x6(%si)
	jmp   L_418e
L_418a:
	orb    $0x1,0x6(%si)
L_418e:
	mov    %si,%ax
L_4190:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4196:
	push   %bp
	mov    %sp,%bp
	push   %si
L_419a:
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_508a
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_41bc
	mov    $0xffff,%ax
	jmp   L_41be
L_41bc:
	xor    %ax,%ax
L_41be:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_41c2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x8(%bp),%di
	mov    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
L_41d2:
	decw   0x6(%bp)
	jle   L_4202
	decw   0x2(%di)
	jl   L_41e8
	mov    (%di),%bx
	incw   (%di)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_41ed
L_41e8:
	push   %di
	call   L_4fa2
	pop    %cx
L_41ed:
	mov    %ax,%si
	or     %ax,%ax
	jl   L_4202
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	cmp    $0xa,%si
	jne   L_41d2
L_4202:
	or     %si,%si
	jge   L_4212
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jne   L_4212
	xor    %ax,%ax
	jmp   L_421e
L_4212:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x6(%bp)
	mov    0x4(%bp),%ax
L_421e:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4224:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	cmpb   $0x0,D_1f54
	jne   L_4261
	movw   $D_219a,-0x8(%bp)
	/* dual-reloc split: movw $D_2222,D_2222 */
	mov	$D_2222,%ax
	mov	%ax,D_2222
	movw   $0x88,-0xa(%bp)
L_4247:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_425c
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_4247
L_425c:
	movb   $0x1,D_1f54
L_4261:
	movw   $D_2182,-0x6(%bp)
L_4266:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_4285
	mov    -0x6(%bp),%ax
	cmp    D_2222,%ax
	jb   L_427f
	xor    %ax,%ax
	jmp   L_42f0
L_427f:
	addw   $0x8,-0x6(%bp)
	jmp   L_4266
L_4285:
	cmpb   $0x77,(%di)
	jne   L_429b
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4322
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_42da
L_429b:
	cmpb   $0x61,(%di)
	jne   L_42dc
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_62ba
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_42c9
	cmpw   $0x2,D_291c
	jne   L_42c9
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_4322
	add    $0x4,%sp
	mov    %ax,%si
L_42c9:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   L_506c
	add    $0x8,%sp
L_42da:
	jmp   L_42ea
L_42dc:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_62ba
	add    $0x4,%sp
	mov    %ax,%si
L_42ea:
	or     %si,%si
	jge   L_42f2
	xor    %ax,%ax
L_42f0:
	jmp   L_431a
L_42f2:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_4310
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_4317
L_4310:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_4317:
	mov    -0x6(%bp),%ax
L_431a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4322:
	push   %bp
	mov    %sp,%bp
	mov	$8,%bx	/* sc: creat */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 8:creat */
	int	$0xf1
	jcxz   L_4336
	mov    %cx,D_291c
L_4336:
	pop    %bp
	ret


L_4338:
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
	jne   L_4356
	jmp   L_449f
L_4356:
	cmpw   $0x2,0xa(%bp)
	jl   L_435f
	jmp   L_4440
L_435f:
	mov    0x4(%bp),%bx
	cmpw   $0x0,0x4(%bx)
	jne   L_436b
	jmp   L_4440
L_436b:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x4,%ax
	je   L_437a
	jmp   L_4440
L_437a:
	mov    0x4(%bp),%bx
	mov    0x2(%bx),%si
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	cmpw   $0x0,0xa(%bp)
	jne   L_43d3
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
	call   L_506c
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
	jmp   L_43ee
L_43d3:
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
L_43ee:
	or     %si,%si
	jle   L_442f
	mov    %si,%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jg   L_442f
	jl   L_4401
	cmp    %ax,-0x8(%bp)
	ja   L_442f
L_4401:
	mov    0x4(%bp),%bx
	mov    0x4(%bx),%ax
	mov    0x4(%bp),%bx
	sub    (%bx),%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jl   L_442f
	jg   L_4419
	cmp    %ax,-0x8(%bp)
	jb   L_442f
L_4419:
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	add    %ax,(%bx)
	mov    0x4(%bp),%bx
	mov    -0x8(%bp),%ax
	sub    %ax,0x2(%bx)
	xor    %ax,%ax
	jmp   L_44e0
L_442f:
	mov    0x6(%bp),%ax
	mov    0x8(%bp),%dx
	and    $0x1,%ax
	and    $0x0,%dx
	mov    %ax,%di
	jmp   L_4442
L_4440:
	xor    %di,%di
L_4442:
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
	call   L_506c
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	mov    0x4(%bp),%bx
	movw   $0x0,0x2(%bx)
	or     %di,%di
	je   L_44a9
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_4496
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_44cf
L_4496:
	pushw  0x4(%bp)
	call   L_4fa2
	pop    %cx
	jmp   L_44cf
L_449f:
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x2,%ax
L_44a9:
	je   L_44cf
	pushw  0x4(%bp)
	call   L_5f02
	pop    %cx
	pushw  0xa(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	mov    0x4(%bp),%bx
	mov    0x7(%bx),%al
	cbtw
	push   %ax
	call   L_506c
	add    $0x8,%sp
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
L_44cf:
	cmpw   $0xffff,-0x6(%bp)
	jne   L_44e2
	cmpw   $0xffff,-0x8(%bp)
	jne   L_44e2
	mov    $0xffff,%ax
L_44e0:
	jmp   L_44e4
L_44e2:
	xor    %ax,%ax
L_44e4:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_44ec:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_218a,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_508a
	add    $0x6,%sp
	mov    D_2190,%al
	cbtw
	test   $0x20,%ax
	je   L_4510
	mov    $0xffff,%ax
	jmp   L_4512
L_4510:
	xor    %ax,%ax
L_4512:
	pop    %di
	pop    %si
	pop    %bp
	ret
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_4577
L_4531:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_4577
	mov    0x6(%bp),%ax
	mov    %ax,-0xa(%bp)
L_453f:
	decw   0x2(%si)
	jl   L_4550
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_4555
L_4550:
	push   %si
	call   L_4fa2
	pop    %cx
L_4555:
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jl   L_4568
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	jmp   L_456d
L_4568:
	mov    -0x8(%bp),%ax
	jmp   L_457a
L_456d:
	decw   -0xa(%bp)
	jne   L_453f
	incw   -0x8(%bp)
	jmp   L_4531
L_4577:
	mov    -0x8(%bp),%ax
L_457a:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4581:
	push   %bp
L_4582:
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    0x4(%bp),%di
	mov    0xa(%bp),%si
	movw   $0x0,-0x8(%bp)
	cmpw   $0x0,0x6(%bp)
	je   L_45e3
L_459c:
	mov    -0x8(%bp),%ax
	cmp    0x8(%bp),%ax
	jae   L_45e3
	mov    0x6(%bp),%ax
	mov    %ax,-0x6(%bp)
L_45aa:
	decw   0x2(%si)
	jl   L_45c2
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
	jmp   L_45d0
L_45c2:
	push   %si
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_5e14
	add    $0x4,%sp
L_45d0:
	decw   -0x6(%bp)
	jne   L_45aa
	mov    0x6(%si),%al
	cbtw
	test   $0x20,%ax
	jne   L_45e3
	incw   -0x8(%bp)
	jmp   L_459c
L_45e3:
	mov    -0x8(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	mov    $D_2182,%ax
	push   %ax
	call   L_4662
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
	call   L_4662
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4621:
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
L_463c:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_4649
	incw   -0xa(%bp)
	jmp   L_463c
L_4649:
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   L_4662
	add    $0x6,%sp
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4662:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xe,%sp
	mov    0x6(%bp),%di
	mov    0x8(%bp),%si
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x10(%bp)
L_467c:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	jmp   L_4826
L_4688:
	mov    -0x8(%bp),%ax
	jmp   L_4756
L_468e:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	cmp    $0x25,%ax
	jne   L_469f
	jmp   L_4844
L_469f:
	movw   $0x0,-0xe(%bp)
	cmpw   $0x2a,-0x6(%bp)
	je   L_46b3
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xe(%bp)
	jmp   L_46bc
L_46b3:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
L_46bc:
	movw   $0x0,-0xa(%bp)
	movw   $0x1,-0x12(%bp)
L_46c6:
	cmpw   $0x30,-0x6(%bp)
	jl   L_46ee
	cmpw   $0x39,-0x6(%bp)
	jg   L_46ee
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
	jmp   L_46c6
L_46ee:
	cmpw   $0x0,-0xa(%bp)
	jne   L_46f9
	movw   $0x7530,-0xa(%bp)
L_46f9:
	cmpw   $0x6c,-0x6(%bp)
	jne   L_470f
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	movw   $0x2,-0x12(%bp)
	jmp   L_4732
L_470f:
	cmpw   $0x68,-0x6(%bp)
	jne   L_4725
	movw   $0x0,-0x12(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x6(%bp)
	jmp   L_4732
L_4725:
	cmpw   $0x5b,-0x6(%bp)
	jne   L_4732
	push   %di
	call   L_4cfa
	pop    %cx
	mov    %ax,%di
L_4732:
	mov    -0x6(%bp),%bx
	mov    D_202d(%bx),%al
	cbtw
	test   $0x1,%ax
	je   L_474d
	mov    -0x6(%bp),%ax
	add    $0x20,%ax
	mov    %ax,-0x6(%bp)
	movw   $0x2,-0x12(%bp)
L_474d:
	cmpw   $0x0,-0x6(%bp)
	jne   L_4758
	mov    $0xffff,%ax
L_4756:
	jmp   L_4795
L_4758:
	lea    -0x10(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	pushw  -0x12(%bp)
	pushw  -0xa(%bp)
	pushw  -0x6(%bp)
	pushw  -0xe(%bp)
	call   L_484d
	add    $0xc,%sp
	or     %ax,%ax
	je   L_477e
	cmpw   $0x0,-0xe(%bp)
	je   L_477e
	incw   -0x8(%bp)
L_477e:
	cmpw   $0x0,-0x10(%bp)
L_4782:
	jne   L_4787
	jmp   L_467c
L_4787:
	cmpw   $0x0,-0x8(%bp)
	je   L_4792
	mov    -0x8(%bp),%ax
	jmp   L_4795
L_4792:
	mov    $0xffff,%ax
L_4795:
	jmp   L_4813
L_4797:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_47b1
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_47b8
L_47b1:
	pushw  0x4(%bp)
	call   L_4fa2
	pop    %cx
L_47b8:
	mov    %ax,-0xc(%bp)
	cmp    $0x20,%ax
	je   L_4797
	cmpw   $0x9,-0xc(%bp)
	je   L_4797
	cmpw   $0xa,-0xc(%bp)
L_47ca:
	je   L_4797
	cmpw   $0xffff,-0xc(%bp)
L_47d0:
	je   L_4782
	pushw  0x4(%bp)
	pushw  -0xc(%bp)
	call   L_6360
	add    $0x4,%sp
	jmp   L_467c
L_47e1:
	mov    0x4(%bp),%bx
	decw   0x2(%bx)
	jl   L_47fb
	mov    0x4(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_4802
L_47fb:
	pushw  0x4(%bp)
	call   L_4fa2
	pop    %cx
L_4802:
	mov    %ax,-0xc(%bp)
	cmp    -0x6(%bp),%ax
	je   L_47d0
	cmpw   $0xffff,-0xc(%bp)
	jne   L_4815
	mov    $0xffff,%ax
L_4813:
	jmp   L_4846
L_4815:
	pushw  0x4(%bp)
	pushw  -0xc(%bp)
	call   L_6360
	add    $0x4,%sp
	mov    -0x8(%bp),%ax
	jmp   L_4846
L_4826:
	or     %ax,%ax
	jne   L_482d
	jmp   L_4688
L_482d:
	cmp    $0x9,%ax
L_4830:
	je   L_47ca
	cmp    $0xa,%ax
	je   L_4830
	cmp    $0x20,%ax
	je   L_4830
	cmp    $0x25,%ax
	jne   L_4844
	jmp   L_468e
L_4844:
	jmp   L_47e1
L_4846:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_484d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x63,0x6(%bp)
	je   L_4867
	cmpw   $0x73,0x6(%bp)
	je   L_4867
	cmpw   $0x5b,0x6(%bp)
	jne   L_488c
L_4867:
	pushw  0xe(%bp)
	pushw  0xc(%bp)
	pushw  0x8(%bp)
	pushw  0x6(%bp)
	cmpw   $0x0,0x4(%bp)
	je   L_4880
	mov    0x4(%bp),%bx
	mov    (%bx),%ax
	jmp   L_4882
L_4880:
	xor    %ax,%ax
L_4882:
	push   %ax
	call   L_4bec
	add    $0xa,%sp
	jmp   L_4b5a
L_488c:
	movw   $0x0,-0x58(%bp)
	movw   $0x0,-0x56(%bp)
	movw   $0x0,-0x52(%bp)
	movw   $0x0,-0x4c(%bp)
	cmpw   $0x65,0x6(%bp)
	je   L_48ac
	cmpw   $0x66,0x6(%bp)
	jne   L_48b1
L_48ac:
	movw   $0x1,-0x4c(%bp)
L_48b1:
	movw   $0xa,-0x48(%bp)
	cmpw   $0x6f,0x6(%bp)
	jne   L_48c3
	movw   $0x8,-0x48(%bp)
	jmp   L_48ce
L_48c3:
	cmpw   $0x78,0x6(%bp)
	jne   L_48ce
	movw   $0x10,-0x48(%bp)
L_48ce:
	lea    -0x44(%bp),%ax
	mov    %ax,%di
	movw   $0x0,-0x4a(%bp)
	movw   $0x0,-0x4e(%bp)
L_48dd:
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_48f7
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_48fe
L_48f7:
	pushw  0xc(%bp)
	call   L_4fa2
	pop    %cx
L_48fe:
	mov    %ax,-0x46(%bp)
	cmp    $0x20,%ax
	je   L_48dd
	cmpw   $0x9,-0x46(%bp)
	je   L_48dd
	cmpw   $0xa,-0x46(%bp)
	je   L_48dd
	cmpw   $0x2d,-0x46(%bp)
	jne   L_494e
	incw   -0x4e(%bp)
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_493f
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_4946
L_493f:
	pushw  0xc(%bp)
	call   L_4fa2
	pop    %cx
L_4946:
	mov    %ax,-0x46(%bp)
	decw   0x8(%bp)
	jmp   L_497b
L_494e:
	cmpw   $0x2b,-0x46(%bp)
	jne   L_497b
	decw   0x8(%bp)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_4971
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_4978
L_4971:
	pushw  0xc(%bp)
	call   L_4fa2
	pop    %cx
L_4978:
	mov    %ax,-0x46(%bp)
L_497b:
	decw   0x8(%bp)
	jge   L_4983
	jmp   L_4ad7
L_4983:
	movw   $0x0,-0x54(%bp)
	cmpw   $0x30,-0x46(%bp)
	jl   L_4999
	cmpw   $0x39,-0x46(%bp)
	jg   L_4999
	movw   $0x1,-0x54(%bp)
L_4999:
	cmpw   $0x61,-0x46(%bp)
	jl   L_49a5
	cmpw   $0x7a,-0x46(%bp)
	jle   L_49b1
L_49a5:
	cmpw   $0x41,-0x46(%bp)
	jl   L_49bc
	cmpw   $0x5a,-0x46(%bp)
	jg   L_49bc
L_49b1:
	cmpw   $0x10,-0x48(%bp)
	jne   L_49bc
	movw   $0x1,-0x54(%bp)
L_49bc:
	cmpw   $0x0,-0x54(%bp)
	jne   L_49c5
	jmp   L_4a59
L_49c5:
	incw   -0x52(%bp)
	cmpw   $0x8,-0x48(%bp)
	jne   L_49df
	mov    $0x3,%cx
	or     %cx,%cx
	je   L_4a19
L_49d5:
	shlw   -0x58(%bp)
	rclw   -0x56(%bp)
	loop   L_49d5
	jmp   L_4a19
L_49df:
	cmpw   $0xa,-0x48(%bp)
	jne   L_4a0a
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	mov    $0x2,%cx
	or     %cx,%cx
	je   L_49f8
L_49f2:
	shl    %ax
	rcl    %dx
	loop   L_49f2
L_49f8:
	add    -0x58(%bp),%ax
	adc    -0x56(%bp),%dx
	shl    %ax
	rcl    %dx
	mov    %ax,-0x58(%bp)
	mov    %dx,-0x56(%bp)
	jmp   L_4a19
L_4a0a:
	mov    $0x4,%cx
	or     %cx,%cx
	je   L_4a19
L_4a11:
	shlw   -0x58(%bp)
	rclw   -0x56(%bp)
	loop   L_4a11
L_4a19:
	mov    -0x46(%bp),%ax
	mov    %ax,-0x50(%bp)
	cmpw   $0x30,-0x46(%bp)
	jl   L_4a31
	cmpw   $0x39,-0x46(%bp)
	jg   L_4a31
	subw   $0x30,-0x46(%bp)
	jmp   L_4a47
L_4a31:
	cmpw   $0x61,-0x46(%bp)
	jl   L_4a43
	cmpw   $0x66,-0x46(%bp)
	jg   L_4a43
	subw   $0x57,-0x46(%bp)
	jmp   L_4a47
L_4a43:
	subw   $0x37,-0x46(%bp)
L_4a47:
	mov    -0x46(%bp),%ax
	cwtd
	add    %ax,-0x58(%bp)
	adc    %dx,-0x56(%bp)
	mov    -0x50(%bp),%ax
	mov    %ax,-0x46(%bp)
	jmp   L_4a6e
L_4a59:
	cmpw   $0x2e,-0x46(%bp)
	jne   L_4a70
	cmpw   $0xa,-0x48(%bp)
	jne   L_4a86
	cmpw   $0x0,-0x4c(%bp)
	je   L_4a95
	incw   -0x52(%bp)
L_4a6e:
	jmp   L_4adf
L_4a70:
	cmpw   $0x65,-0x46(%bp)
	je   L_4a7c
	cmpw   $0x45,-0x46(%bp)
	jne   L_4a86
L_4a7c:
	cmpw   $0x0,-0x4a(%bp)
	jne   L_4a86
	cmpw   $0xa,-0x48(%bp)
L_4a86:
	je   L_4a8b
	jmp   L_4b10
L_4a8b:
	cmpw   $0x0,-0x4c(%bp)
	je   L_4a95
	cmpw   $0x0,-0x52(%bp)
L_4a95:
	je   L_4b10
	incw   -0x4a(%bp)
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_4abe
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_4ac5
L_4abe:
	pushw  0xc(%bp)
	call   L_4fa2
	pop    %cx
L_4ac5:
	mov    %ax,-0x46(%bp)
	cmp    $0x2b,%ax
	je   L_4adf
	cmpw   $0x2d,-0x46(%bp)
	je   L_4adf
	cmpw   $0x30,-0x46(%bp)
L_4ad7:
	jl   L_4b10
	cmpw   $0x39,-0x46(%bp)
	jg   L_4b10
L_4adf:
	mov    %di,%bx
	inc    %di
	mov    -0x46(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    0xc(%bp),%bx
	decw   0x2(%bx)
	jl   L_4b03
	mov    0xc(%bp),%bx
	incw   (%bx)
	mov    (%bx),%ax
	dec    %ax
	mov    %ax,%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_4b0a
L_4b03:
	pushw  0xc(%bp)
	call   L_4fa2
	pop    %cx
L_4b0a:
	mov    %ax,-0x46(%bp)
	jmp   L_497b
L_4b10:
	cmpw   $0x0,-0x4e(%bp)
	je   L_4b29
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	mov    %ax,-0x58(%bp)
	mov    %dx,-0x56(%bp)
L_4b29:
	cmpw   $0xffff,-0x46(%bp)
	je   L_4b44
	pushw  0xc(%bp)
	pushw  -0x46(%bp)
	call   L_6360
	add    $0x4,%sp
	mov    0xe(%bp),%bx
	movw   $0x0,(%bx)
	jmp   L_4b4b
L_4b44:
	mov    0xe(%bp),%bx
	movw   $0x1,(%bx)
L_4b4b:
	cmpw   $0x0,0x4(%bp)
	je   L_4b58
	lea    -0x44(%bp),%ax
	cmp    %ax,%di
	jne   L_4b5d
L_4b58:
	xor    %ax,%ax
L_4b5a:
	jmp   L_4be5
L_4b5d:
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
	mov    -0x4c(%bp),%ax
	mov    $0x4,%cx
	shl    %cl,%ax
	or     0xa(%bp),%ax
	jmp   L_4bd6
	lea    -0x44(%bp),%ax
	push   %ax
	call   L_4d64
	int    $0xf4
	fstpl  -0x60(%bp)
	pop    %cx
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	int    $0xf4
	fldl   -0x60(%bp)
	fstps  (%bx)
	jmp   L_4bd4
	lea    -0x44(%bp),%ax
	push   %ax
	call   L_4d64
	int    $0xf4
	fstpl  -0x60(%bp)
	pop    %cx
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	int    $0xf4
	fldl   -0x60(%bp)
	fstpl  (%bx)
	jmp   L_4bd4
	mov    -0x58(%bp),%ax
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    %ax,(%bx)
	jmp   L_4bd4
	mov    -0x58(%bp),%ax
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    %ax,(%bx)
	jmp   L_4bd4
	mov    0x4(%bp),%bx
	mov    (%bx),%bx
	mov    -0x58(%bp),%ax
	mov    -0x56(%bp),%dx
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_4bd4:
	jmp   L_4be2
L_4bd6:
	cmp    $0x12,%ax
	ja   L_4be2
	shl    %ax
	xchg   %ax,%bx
	jmp    *0x3fd6(%bx)
L_4be2:
	mov    $0x1,%ax
L_4be5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4bec:
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
	jne   L_4c18
	cmpw   $0x7530,0x8(%bp)
	jne   L_4c18
	movw   $0x1,0x8(%bp)
L_4c18:
	movw   $0x0,-0xa(%bp)
	cmpw   $0x73,0x6(%bp)
	jne   L_4c28
	movw   $0x1,-0xa(%bp)
L_4c28:
	decw   0x2(%si)
	jl   L_4c39
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_4c3e
L_4c39:
	push   %si
	call   L_4fa2
	pop    %cx
L_4c3e:
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	mov    D_1f56(%bx),%al
	cbtw
	test   %ax,-0xa(%bp)
	je   L_4c53
	cmpw   $0xffff,-0x6(%bp)
	jne   L_4c28
L_4c53:
	movw   $0x1,-0xa(%bp)
	cmpw   $0x63,0x6(%bp)
	jne   L_4c65
	movw   $0x0,-0xa(%bp)
	jmp   L_4c70
L_4c65:
	cmpw   $0x5b,0x6(%bp)
	jne   L_4c70
	movw   $0x2,-0xa(%bp)
L_4c70:
	cmpw   $0xffff,-0x6(%bp)
	je   L_4cb1
	mov    -0x6(%bp),%bx
	mov    D_1f56(%bx),%al
	cbtw
	test   %ax,-0xa(%bp)
	jne   L_4cb1
	or     %di,%di
	je   L_4c91
	mov    %di,%bx
	inc    %di
	mov    -0x6(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
L_4c91:
	decw   0x8(%bp)
	jle   L_4cb1
	decw   0x2(%si)
	jl   L_4ca7
	mov    (%si),%bx
	incw   (%si)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_4cac
L_4ca7:
	push   %si
	call   L_4fa2
	pop    %cx
L_4cac:
	mov    %ax,-0x6(%bp)
	jmp   L_4c70
L_4cb1:
	cmpw   $0xffff,-0x6(%bp)
	je   L_4cd0
	cmpw   $0x0,0x8(%bp)
	jle   L_4cc7
	push   %si
	pushw  -0x6(%bp)
	call   L_6360
	add    $0x4,%sp
L_4cc7:
	mov    0xc(%bp),%bx
	movw   $0x0,(%bx)
	jmp   L_4cd7
L_4cd0:
	mov    0xc(%bp),%bx
	movw   $0x1,(%bx)
L_4cd7:
	or     %di,%di
	je   L_4cf1
	cmp    -0x8(%bp),%di
	je   L_4cf1
	cmpw   $0x63,0x6(%bp)
	je   L_4cec
	mov    %di,%bx
	inc    %di
	movb   $0x0,(%bx)
L_4cec:
	mov    $0x1,%ax
	jmp   L_4cf3
L_4cf1:
	xor    %ax,%ax
L_4cf3:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4cfa:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	movw   $0x0,-0x6(%bp)
	cmpb   $0x5e,(%di)
	jne   L_4d12
	incw   -0x6(%bp)
	inc    %di
L_4d12:
	xor    %si,%si
L_4d14:
	cmp    $0x80,%si
	jge   L_4d2f
	cmpw   $0x0,-0x6(%bp)
	je   L_4d27
	andb   $0xfd,D_1f56(%si)
	jmp   L_4d2c
L_4d27:
	orb    $0x2,D_1f56(%si)
L_4d2c:
	inc    %si
	jmp   L_4d14
L_4d2f:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	and    $0x7f,%ax
	cmp    $0x5d,%ax
	je   L_4d5c
	cmpw   $0x0,-0x6(%bp)
	je   L_4d4d
	orb    $0x2,D_1f56(%si)
	inc    %si
	jmp   L_4d53
L_4d4d:
	andb   $0xfd,D_1f56(%si)
	inc    %si
L_4d53:
	or     %si,%si
	jne   L_4d2f
	dec    %di
	mov    %di,%ax
	jmp   L_4d5e
L_4d5c:
	mov    %di,%ax
L_4d5e:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_4d64:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x3c,%sp
	mov    0x4(%bp),%di
	movw   $0x1,-0x24(%bp)
L_4d75:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    $0x20,%ax
	je   L_4d75
	cmp    $0x2d,%si
	jne   L_4d8e
	movw   $0xffff,-0x24(%bp)
	jmp   L_4d94
L_4d8e:
	cmp    $0x2b,%si
	je   L_4d94
	dec    %di
L_4d94:
	movw   $0x0,-0x22(%bp)
	int    $0xf4
	fldl   D_1ffc
	fstpl  -0xc(%bp)
	movw   $0x0,-0x1e(%bp)
L_4da9:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    D_202d(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_4df6
	cmpw   $0x10,-0x1e(%bp)
	jge   L_4dee
	int    $0xf4
	fldl   D_2004
	fmull  -0xc(%bp)
	fstpl  -0x30(%bp)
	lea    -0x30(%si),%ax
	mov    %ax,-0x38(%bp)
	int    $0xf4
	filds  -0x38(%bp)
	fstl   -0x38(%bp)
	faddl  -0x30(%bp)
	fstl   -0x40(%bp)
	fstpl  -0xc(%bp)
	jmp   L_4df1
L_4dee:
	incw   -0x22(%bp)
L_4df1:
	incw   -0x1e(%bp)
	jmp   L_4da9
L_4df6:
	cmp    $0x2e,%si
	jne   L_4e46
L_4dfb:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    D_202d(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_4e46
	cmpw   $0x10,-0x1e(%bp)
	jge   L_4e41
	int    $0xf4
	fldl   D_200c
	fmull  -0xc(%bp)
	fstpl  -0x30(%bp)
	lea    -0x30(%si),%ax
	mov    %ax,-0x38(%bp)
	int    $0xf4
	filds  -0x38(%bp)
	fstl   -0x38(%bp)
	faddl  -0x30(%bp)
	fstl   -0x40(%bp)
	fstpl  -0xc(%bp)
	decw   -0x22(%bp)
L_4e41:
	incw   -0x1e(%bp)
	jmp   L_4dfb
L_4e46:
	movw   $0x1,-0x26(%bp)
	movw   $0x0,-0x20(%bp)
	cmp    $0x45,%si
	je   L_4e5a
	cmp    $0x65,%si
	jne   L_4eaf
L_4e5a:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	cmp    $0x2b,%ax
	je   L_4e74
	cmp    $0x2d,%si
	jne   L_4e73
	movw   $0xffff,-0x26(%bp)
	jmp   L_4e74
L_4e73:
	dec    %di
L_4e74:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%si
	mov    D_202d(%si),%al
	cbtw
	test   $0x4,%ax
	je   L_4e98
	mov    -0x20(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	lea    -0x30(%si),%dx
	add    %dx,%ax
	mov    %ax,-0x20(%bp)
	jmp   L_4e74
L_4e98:
	cmpw   $0x0,-0x26(%bp)
	jge   L_4ea6
	mov    -0x20(%bp),%ax
	neg    %ax
	mov    %ax,-0x20(%bp)
L_4ea6:
	mov    -0x22(%bp),%ax
	add    -0x20(%bp),%ax
	mov    %ax,-0x22(%bp)
L_4eaf:
	movw   $0x1,-0x26(%bp)
	cmpw   $0x0,-0x22(%bp)
	jge   L_4ec7
	movw   $0xffff,-0x26(%bp)
	mov    -0x22(%bp),%ax
	neg    %ax
	mov    %ax,-0x22(%bp)
L_4ec7:
	mov    -0x22(%bp),%ax
	imulw  -0x26(%bp)
	add    -0x1e(%bp),%ax
	cmp    $0xffd9,%ax
	jge   L_4ee5
	int    $0xf4
	fldl   D_2014
	fstpl  -0xc(%bp)
	movw   $0x0,-0x22(%bp)
L_4ee5:
	int    $0xf4
	fldl   D_201c
	fstpl  -0x14(%bp)
	fldl   D_2024
	fstpl  -0x1c(%bp)
	mov    -0x22(%bp),%ax
	mov    %ax,-0x28(%bp)
L_4eff:
	testw  $0x1,-0x22(%bp)
	je   L_4f14
	int    $0xf4
	fldl   -0x14(%bp)
	fmull  -0x1c(%bp)
	fstpl  -0x14(%bp)
L_4f14:
	sarw   -0x22(%bp)
	cmpw   $0x0,-0x22(%bp)
	je   L_4f2d
	int    $0xf4
	fldl   -0x1c(%bp)
	fmull  -0x1c(%bp)
	fstpl  -0x1c(%bp)
	jmp   L_4eff
L_4f2d:
	cmpw   $0x0,-0x26(%bp)
	jge   L_4f43
	int    $0xf4
	fldl   -0xc(%bp)
	fdivl  -0x14(%bp)
	fstpl  -0xc(%bp)
	jmp   L_4f51
L_4f43:
	int    $0xf4
	fldl   -0xc(%bp)
	fmull  -0x14(%bp)
	fstpl  -0xc(%bp)
L_4f51:
	mov    -0x26(%bp),%ax
	imulw  -0x28(%bp)
	push   %ax
	int    $0xf4
	fldl   -0xc(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_63dc
	int    $0xf4
	fstpl  -0x30(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x30(%bp)
	fstpl  -0xc(%bp)
	cmpw   $0x0,-0x24(%bp)
	jge   L_4f95
	int    $0xf4
	fldl   -0xc(%bp)
	fchs
	fstl   -0x30(%bp)
	fstpl  -0xc(%bp)
L_4f95:
	int    $0xf4
	fldl   -0xc(%bp)
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_4fa2:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_4fb9
	mov    $0xffff,%ax
	jmp   L_4fc5
L_4fb9:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_4fc7
	mov    $0xffff,%ax
L_4fc5:
	jmp   L_503f
L_4fc7:
	cmpw   $0x0,0x4(%di)
	jne   L_4ffb
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_4fe2
	mov    0x7(%di),%al
	cbtw
	add    $D_20ae,%ax
	mov    %ax,0x4(%di)
	jmp   L_4fc7
L_4fe2:
	mov    $0x200,%ax
	push   %ax
	call   L_5fe8
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_4ff7
	orb    $0x4,0x6(%di)
	jmp   L_4fc7
L_4ff7:
	orb    $0x8,0x6(%di)
L_4ffb:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_500e
	mov    $0x1,%ax
	jmp   L_5011
L_500e:
	mov    $0x200,%ax
L_5011:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_62d0
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_5041
	cmpw   $0xffff,0x2(%di)
	jne   L_5033
	orb    $0x10,0x6(%di)
	jmp   L_5037
L_5033:
	orb    $0x20,0x6(%di)
L_5037:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_503f:
	jmp   L_504b
L_5041:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_504b:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_5050:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_505b:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_5066
	inc    %si
	jmp   L_505b
L_5066:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_506c:
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
	jcxz   L_5087
	mov    %cx,D_291c
L_5087:
	pop    %si
	pop    %bp
	ret


L_508a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_50a2:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_50b4
	jmp   L_5651
L_50b4:
	cmpb   $0x25,-0x5(%bp)
	je   L_50c7
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_50c5:
	jmp   L_50a2
L_50c7:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_50f2
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_5d22
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_50f2:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_510b
	incb   -0xa6(%bp)
	inc    %di
L_510b:
	cmpb   $0x30,(%di)
	jne   L_5116
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_5116:
	cmpb   $0x2a,(%di)
	je   L_5146
	movw   $0x0,-0xe(%bp)
L_5120:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_515c
	cmpw   $0x9,-0xc(%bp)
	jg   L_515c
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_5120
L_5146:
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
L_515c:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_51a3
	cmpb   $0x2a,(%di)
	je   L_51a5
	movw   $0x0,-0x10(%bp)
L_5175:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_519f
	cmpw   $0x9,-0xc(%bp)
	jg   L_519f
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_5175
L_519f:
	addw   $0x30,-0xc(%bp)
L_51a3:
	jmp   L_51bc
L_51a5:
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
L_51bc:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_55c6
L_51c7:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_51d8
L_51cf:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_5208
	jmp   L_5201
L_51d8:
	cmp    $0x64,%ax
	je   L_520a
	cmp    $0x6f,%ax
	je   L_51ee
	cmp    $0x75,%ax
	je   L_5201
	cmp    $0x78,%ax
	je   L_51fc
	jmp   L_51cf
L_51ee:
	movw   $0x8,-0x12(%bp)
	jmp   L_5201
L_51f5:
	movw   $0x8,-0x12(%bp)
	jmp   L_5208
L_51fc:
	movw   $0x10,-0x12(%bp)
L_5201:
	jmp   L_5253
L_5203:
	movw   $0x10,-0x12(%bp)
L_5208:
	jmp   L_5263
L_520a:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_5227
L_521a:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_5227:
	cmpw   $0x0,-0x14(%bp)
	jg   L_5271
	jl   L_5235
	cmpw   $0x0,-0x16(%bp)
	jae   L_5271
L_5235:
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
	jmp   L_5271
L_5253:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_5271
L_5263:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_5271:
	cmpw   $0x0,-0x14(%bp)
	jne   L_527b
	cmpw   $0x0,-0x16(%bp)
L_527b:
	jne   L_528c
	cmpw   $0x0,-0x10(%bp)
	jne   L_528c
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_528c:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_5293:
	cmpw   $0x0,-0x14(%bp)
	jne   L_529d
	cmpw   $0x0,-0x16(%bp)
L_529d:
	je   L_52e8
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_57b0
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_52be
	mov    $0x30,%ax
	jmp   L_52c1
L_52be:
	mov    $0x57,%ax
L_52c1:
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
	call   L_56a8
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_5293
L_52e8:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_5337
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_52e8
L_5306:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_5312:
	cmpw   $0x2,-0xc(%bp)
	jge   L_5337
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_5332
	decw   -0x8(%bp)
L_5332:
	incw   -0xc(%bp)
	jmp   L_5312
L_5337:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_533e:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_5355
	movw   $0x0,-0xc(%bp)
L_5355:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_5364
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_5364:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_5d22
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_5387:
	jmp   L_50c5
L_538a:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_539c
	movw   $D_20c2,-0xa(%bp)
L_539c:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_53ab
	movw   $0x7fff,-0xc(%bp)
L_53ab:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_53b1:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_533e
	decw   -0xc(%bp)
	jl   L_533e
	incw   -0x8(%bp)
	jmp   L_53b1
L_53c3:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_53cb:
	jmp   L_5387
L_53cd:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_53d9
	movw   $0x6,-0x10(%bp)
L_53d9:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_59ae
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_540e
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_540e:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_5423
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_543b
L_5423:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_5423
L_543b:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_544e
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_544e:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_546d
L_545a:
	decw   -0xc(%bp)
	jl   L_546d
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_545a
L_546d:
	cmpw   $0x0,-0xc(%bp)
	jg   L_5476
	jmp   L_5337
L_5476:
	decw   -0xc(%bp)
	jge   L_547e
	jmp   L_5337
L_547e:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_5476
L_5493:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_54a1
	movw   $0x6,-0x10(%bp)
	jmp   L_54a4
L_54a1:
	incw   -0x10(%bp)
L_54a4:
	lea    -0x18(%bp),%ax
	push   %ax
	lea    -0x1a(%bp),%ax
	push   %ax
	pushw  -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_5982
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_54d9
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_54d9:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_54e4
	incw   -0x1a(%bp)
L_54e4:
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
L_5506:
	decw   -0xc(%bp)
	jle   L_5520
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_5506
L_5520:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_553d
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_554e
L_553d:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_554e:
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
	jmp   L_5337
L_557f:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_558b
	movw   $0x6,-0x10(%bp)
L_558b:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_5806
	add    $0xc,%sp
	add    $0x8,%si
L_55a9:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_55a9
	decw   -0x8(%bp)
	jmp   L_5337
L_55ba:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_55c3:
	jmp   L_53cb
L_55c6:
	cmp    $0x65,%ax
	jne   L_55ce
	jmp   L_5493
L_55ce:
	jg   L_560f
	cmp    $0x4f,%ax
	jne   L_55d8
	jmp   L_51ee
L_55d8:
	jg   L_55ef
	cmp    $0x25,%ax
	je   L_55ba
	cmp    $0x44,%ax
	jne   L_55e7
	jmp   L_520a
L_55e7:
	cmp    $0x4c,%ax
L_55ea:
	jne   L_55ef
	jmp   L_51c7
L_55ef:
	cmp    $0x55,%ax
	jne   L_55f7
	jmp   L_5253
L_55f7:
	cmp    $0x58,%ax
	jne   L_55ff
	jmp   L_51fc
L_55ff:
	cmp    $0x63,%ax
	jne   L_5607
	jmp   L_5306
L_5607:
	cmp    $0x64,%ax
	jne   L_560f
	jmp   L_521a
L_560f:
	cmp    $0x6f,%ax
	jne   L_5617
	jmp   L_51f5
L_5617:
	jg   L_562e
	cmp    $0x66,%ax
	jne   L_5621
	jmp   L_53cd
L_5621:
	cmp    $0x67,%ax
	jne   L_5629
	jmp   L_557f
L_5629:
	cmp    $0x6c,%ax
	je   L_55ea
L_562e:
	cmp    $0x72,%ax
	jne   L_5636
	jmp   L_53c3
L_5636:
	cmp    $0x73,%ax
	jne   L_563e
	jmp   L_538a
L_563e:
	cmp    $0x75,%ax
	jne   L_5646
	jmp   L_5263
L_5646:
	cmp    $0x78,%ax
	jne   L_564e
	jmp   L_5203
L_564e:
	jmp   L_55c3
L_5651:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_5675
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_5d22
	add    $0x8,%sp
L_5675:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_567c:
	call   L_57e4


L_567f:
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
	call   L_57e4


L_56a8:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_56c6
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_56c6:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_56cf:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_56cf
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_56df:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_56df
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_567f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_5720
	jne   L_5715
	or     $0x0,%ax
	je   L_5720
L_5715:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_5720:
	pop    %ax
	mov    $0x0,%dx
	ret


L_5725:
	call   L_57e4


L_5728:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_5744
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_5744:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_575b
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_575b:
	push   %dx
	push   %ax
	call   L_56a8
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_5775
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_5775:
	ret
	call   L_57e4
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_5728
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_567f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_57e4


L_57b0:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_56a8
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_567f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_57e4:
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


L_5806:
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
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_5982
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_5840
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_5840:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_5847:
	cmpw   $0x0,-0xa(%bp)
	jle   L_5861
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_5861
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_5847
L_5861:
	cmpw   $0x0,-0x8(%bp)
	jl   L_5872
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_587e
L_5872:
	cmpw   $0x0,-0x8(%bp)
	jge   L_587c
	cmpw   $0xfffd,-0x8(%bp)
L_587c:
	jge   L_58fd
L_587e:
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
L_5899:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_58b3
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_5899
L_58b3:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_58cf
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_58d5
L_58cf:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_58d5:
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
	jmp   L_596d
L_58fd:
	cmpw   $0x0,-0x8(%bp)
	jg   L_591f
	cmpb   $0x30,(%di)
	je   L_590e
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_590e:
	cmpw   $0x0,-0x8(%bp)
	jge   L_591f
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_590e
L_591f:
	movw   $0x1,-0xa(%bp)
L_5924:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_594c
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
	jne   L_5947
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_5947:
	incw   -0xa(%bp)
	jmp   L_5924
L_594c:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_596d
L_5954:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_5967
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_5954
L_5967:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_596d:
	cmpb   $0x2e,-0x1(%si)
	jne   L_5974
	dec    %si
L_5974:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_5982:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $0x1,%ax
	push   %ax
	pushw  0x10(%bp)
	pushw  0xe(%bp)
	pushw  0xc(%bp)
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_59d9
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_59ae:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	xor    %ax,%ax
	push   %ax
	pushw  0x10(%bp)
	pushw  0xe(%bp)
	pushw  0xc(%bp)
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_59d9
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_59d9:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_59ec
	movw   $0x0,0xc(%bp)
L_59ec:
	cmpw   $0x4f,0xc(%bp)
	jl   L_59f7
	movw   $0x4e,0xc(%bp)
L_59f7:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_20ca,%si
	int    $0xf4
	fldl   0x4(%bp)
	fcompl D_211a
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jae   L_5a31
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	int    $0xf4
	fldl   0x4(%bp)
	fchs
	fstl   -0x46(%bp)
	fstpl  0x4(%bp)
L_5a31:
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_63f6
	int    $0xf4
	fstpl  -0x46(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x46(%bp)
	fstpl  0x4(%bp)
	fldl   -0xc(%bp)
	fcompl D_2122
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jne   L_5a72
	jmp   L_5b48
L_5a72:
	mov    $D_20ca,%si
L_5a75:
	int    $0xf4
	fldl   -0xc(%bp)
	fcompl D_212a
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jb   L_5b01
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   -0xc(%bp)
	fmull  D_2132
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_63f6
	int    $0xf4
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x4e(%bp)
	fmull  D_213a
	faddl  D_2142
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
	/* TODO: data ref: cmp    $0x2710,%ax */
	cmp    $D_0710,%ax
	jne   L_5a75
	int    $0xf4
	fldl   -0xc(%bp)
	faddl  D_214a
	fstpl  -0xc(%bp)
	lea    -0x3e(%bp),%ax
	lea    -0x1(%di),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	jmp   L_5a75
L_5b01:
	mov    $0x1,%ax
	push   %ax
	push   %si
	int    $0xf4
	fldl   -0xc(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   L_5cac
	add    $0x6,%sp
	mov    %ax,%si
L_5b1c:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_5b3e
	xor    %ax,%ax
	push   %ax
	push   %si
	lea    -0x3e(%bp),%ax
	mov    %di,%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   L_5cac
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_5b1c
L_5b3e:
	mov    %si,%ax
	sub    $D_20ca,%ax
	mov    %ax,%di
	jmp   L_5bc8
L_5b48:
	int    $0xf4
	fldl   0x4(%bp)
	fcompl D_2152
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jbe   L_5bc8
L_5b5e:
	int    $0xf4
	fldl   0x4(%bp)
	fmull  D_215a
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	fcompl D_2162
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_5b94
	int    $0xf4
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	sub    $0x3,%di
	jmp   L_5b5e
L_5b94:
	int    $0xf4
	fldl   0x4(%bp)
	fmull  D_216a
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	fcompl D_2172
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_5bc8
	int    $0xf4
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_5b94
L_5bc8:
	mov    0xc(%bp),%ax
	add    $D_20ca,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_5bda
	add    %di,-0x16(%bp)
L_5bda:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_20ca,-0x16(%bp)
	jae   L_5bf0
	movb   $0x0,D_20ca
	mov    $D_20ca,%ax
	jmp   L_5c59
L_5bf0:
	cmp    -0x16(%bp),%si
	ja   L_5c4a
	cmp    $D_211a,%si
	jae   L_5c4a
	lea    -0x14(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	fmull  D_217a
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_63f6
	int    $0xf4
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x4e(%bp)
	fstpl  0x4(%bp)
	xor    %ax,%ax
	push   %ax
	push   %si
	int    $0xf4
	fldl   -0x14(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   L_5cac
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_5bf0
L_5c4a:
	cmpw   $D_211a,-0x16(%bp)
	jb   L_5c5b
	movb   $0x0,D_2119
	mov    $D_20ca,%ax
L_5c59:
	jmp   L_5ca5
L_5c5b:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_5c63:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_5c9f
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_20ca,-0x16(%bp)
	jbe   L_5c82
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_5c63
L_5c82:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_5c63
	cmp    $D_20ca,%si
	jbe   L_5c9c
	movb   $0x30,(%si)
L_5c9c:
	inc    %si
	jmp   L_5c63
L_5c9f:
	movb   $0x0,(%si)
	mov    $D_20ca,%ax
L_5ca5:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_5cac:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%si
	xor    %di,%di
L_5cb7:
	or     %si,%si
	je   L_5cc8
	mov    %si,%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	mov    %ax,%si
	inc    %di
	jmp   L_5cb7
L_5cc8:
	cmpw   $0x0,0x8(%bp)
	jne   L_5ce1
	mov    %di,%si
L_5cd0:
	cmp    $0x4,%si
	jge   L_5ce1
	mov    0x6(%bp),%bx
	movb   $0x30,(%bx)
	incw   0x6(%bp)
	inc    %si
	jmp   L_5cd0
L_5ce1:
	mov    0x6(%bp),%ax
	add    %di,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%si
L_5cf1:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_5d1a
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
	jmp   L_5cf1
L_5d1a:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_5d22:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_5d2f:
	cmpw   $0x0,0x8(%bp)
	jl   L_5d38
	jmp   L_5dc6
L_5d38:
	cmpb   $0x2d,(%di)
	jne   L_5d75
	cmpw   $0x30,0xc(%bp)
	jne   L_5d75
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_5d64
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
	jmp   L_5d74
L_5d64:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_5e14
	add    $0x4,%sp
L_5d74:
	dec    %si
L_5d75:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_5d93
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
	jmp   L_5d9f
L_5d93:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_5e14
	add    $0x4,%sp
L_5d9f:
	incw   0x8(%bp)
	jmp   L_5d2f
L_5da4:
	dec    %si
	jl   L_5dda
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_5dc8
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
L_5dc6:
	jmp   L_5da4
L_5dc8:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_5e14
	add    $0x4,%sp
	jmp   L_5da4
L_5dda:
	cmpw   $0x0,0x8(%bp)
	je   L_5e0f
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_5dfe
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
	jmp   L_5e0a
L_5dfe:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_5e14
	add    $0x4,%sp
L_5e0a:
	decw   0x8(%bp)
	jmp   L_5dda
L_5e0f:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_5e14:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_5e2f
	mov    $0xffff,%ax
	jmp   L_5ef6
L_5e2f:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_5e62
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
	call   L_63c2
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_5ee7
L_5e62:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_5eb4
	cmp    $D_218a,%di
	jne   L_5e8d
	mov    D_2191,%al
	cbtw
	push   %ax
	call   L_6292
	pop    %cx
	or     %ax,%ax
	je   L_5e82
	orb    $0x4,0x6(%di)
	jmp   L_5e2f
L_5e82:
	movw   $D_2924,0x4(%di)
	movw   $D_2924,(%di)
	jmp   L_5e2f
L_5e8d:
	mov    $0x200,%ax
	push   %ax
	call   L_5fe8
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_5ea4
	orb    $0x4,0x6(%di)
	jmp   L_5e2f
L_5ea4:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_5ed6
L_5eb4:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_5ed6
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_63c2
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_5ed6:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_5ee7:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_5ef8
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_5ef6:
	jmp   L_5efb
L_5ef8:
	mov    0x4(%bp),%ax
L_5efb:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_5f02:
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
	jne   L_5f4e
	mov    0x4(%di),%si
	or     %si,%si
	je   L_5f4e
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_5f4e
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_63c2
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_5f4e
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_5f50
L_5f4e:
	xor    %ax,%ax
L_5f50:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_5f55:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_2182,%di
L_5f5d:
	cmp    D_2222,%di
	jae   L_5f6d
	push   %di
	call   L_5f71
	pop    %cx
	add    $0x8,%di
	jmp   L_5f5d
L_5f6d:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_5f71:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_5fc4
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_5fc4
	push   %di
	call   L_5f02
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_627e
	pop    %cx
	or     %ax,%ax
	jge   L_5fa6
	mov    $0xffff,%si
L_5fa6:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_5fb6
	pushw  0x4(%di)
	call   L_6132
	pop    %cx
L_5fb6:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_5fc4
	movw   $0x0,0x4(%di)
L_5fc4:
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
	call   L_5f55
	pushw  0x4(%bp)
	call   L_6412
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_5fe8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_2224
	jne   L_6013
	mov    $D_2226,%ax
	or     $0x1,%ax
	mov    %ax,D_2224
	mov    $D_2224,%ax
	or     $0x1,%ax
	mov    %ax,D_2226
	/* dual-reloc split: movw $D_2226,D_2d26 */
	mov	$D_2226,%ax
	mov	%ax,D_2d26
	/* dual-reloc split: movw $D_2224,D_2d24 */
	mov	$D_2224,%ax
	mov	%ax,D_2d24
L_6013:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_2d24,%di
L_6027:
	movw   $0x0,D_2228
L_602d:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_605c
L_6034:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_6043
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_6034
L_6043:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_605c
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_605c
	jmp   L_6103
L_605c:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_602d
	cmp    D_2d26,%si
	jne   L_6075
	cmp    $D_2224,%di
	je   L_6079
L_6075:
	xor    %ax,%ax
	jmp   L_60b1
L_6079:
	incw   D_2228
	cmpw   $0x1,D_2228
	jle   L_602d
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_2228
	xor    %ax,%ax
	push   %ax
	call   L_6216
	pop    %cx
	mov    %ax,%si
	mov    D_2228,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_60b3
	xor    %ax,%ax
L_60b1:
	jmp   L_60c6
L_60b3:
	mov    D_2228,%ax
	shl    %ax
	push   %ax
	call   L_6216
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_60c8
	xor    %ax,%ax
L_60c6:
	jmp   L_612d
L_60c8:
	mov    D_2d26,%bx
	mov    %si,(%bx)
	mov    D_2d26,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_60e6
	mov    D_2d26,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_2d26,%bx
	mov    %ax,(%bx)
L_60e6:
	mov    D_2228,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_2d26
	mov    $D_2224,%ax
	or     $0x1,%ax
	mov    D_2d26,%bx
	mov    %ax,(%bx)
	jmp   L_6027
L_6103:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_2d24
	cmp    %ax,%si
	jbe   L_6122
	mov    D_2d24,%bx
	mov    (%bx),%ax
	mov    %ax,D_2d28
	mov    D_2d24,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_6122:
	mov    D_2d24,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_612d:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_6132:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_2d24
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
	je   L_6167
	push   %di
	call   L_6132
	pop    %cx
L_6167:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_5fe8
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_6186
	cmp    %di,%si
	jne   L_618a
L_6186:
	mov    %si,%ax
	jmp   L_61f2
L_618a:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_61a9
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_61a9:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_61c7
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_61a9
L_61c7:
	cmp    %di,%si
	jae   L_61f0
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_61f0
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
	mov    D_2d28,%ax
	mov    %ax,(%bx)
L_61f0:
	mov    %si,%ax
L_61f2:
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
	jcxz   L_620d
	mov    %cx,D_291c
	jmp   L_6214
L_620d:
	mov    0x4(%bp),%bx
	mov    %bx,D_222a
L_6214:
	pop    %bp
	ret


L_6216:
	push   %bp
	mov    %sp,%bp
	mov	$17,%bx	/* sc: brk */
	mov    0x4(%bp),%ax
	add    D_222a,%ax
	/* v7: 17:brk */
	int	$0xf1
	jcxz   L_622d
	mov    %cx,D_291c
	jmp   L_623a
L_622d:
	mov    D_222a,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_222a
L_623a:
	pop    %bp
	ret


L_623c:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_6249:
	mov    (%di),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_6268
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	jne   L_6249
	xor    %ax,%ax
	jmp   L_6279
L_6268:
	dec    %si
	mov    (%si),%al
	cbtw
	mov    (%di),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	sub    %ax,%dx
	mov    %dx,%ax
L_6279:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_627e:
	push   %bp
	mov    %sp,%bp
	mov	$6,%bx	/* sc: close */
	mov    0x4(%bp),%ax
	/* v7: 6:close */
	int	$0xf1
	jcxz   L_628f
	mov    %cx,D_291c
L_628f:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_6292:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_632d
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_62af
	xor    %ax,%ax
	jmp   L_62b2
L_62af:
	mov    $0x1,%ax
L_62b2:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_62ba:
	push   %bp
	mov    %sp,%bp
	mov	$5,%bx	/* sc: open */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 5:open */
	int	$0xf1
	jcxz   L_62ce
	mov    %cx,D_291c
L_62ce:
	pop    %bp
	ret


L_62d0:
	push   %bp
	mov    %sp,%bp
	mov	$3,%bx	/* sc: read */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 3:read */
	int	$0xf1
	jcxz   L_62e7
	mov    %cx,D_291c
L_62e7:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_62ea:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	mov    %di,-0x6(%bp)
L_62fb:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_62fb
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
	mov    $0x7409,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_6346
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_632d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_6346
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_6346:
	push   %bp
	mov    %sp,%bp
	mov	$54,%bx	/* sc: ioctl */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 54:ioctl */
	int	$0xf1
	jcxz   L_635d
	mov    %cx,D_291c
L_635d:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_6360:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x6(%bp),%di
	cmpw   $0xffff,0x4(%bp)
	jne   L_6374
	mov    $0xffff,%ax
	jmp   L_63aa
L_6374:
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	je   L_6384
	mov    (%di),%ax
	cmp    0x4(%di),%ax
	ja   L_639a
L_6384:
	mov    (%di),%ax
	cmp    0x4(%di),%ax
	jne   L_6395
	cmpw   $0x0,0x2(%di)
	jne   L_6395
	incw   (%di)
	jmp   L_639a
L_6395:
	mov    $0xffff,%ax
	jmp   L_63aa
L_639a:
	incw   0x2(%di)
	decw   (%di)
	mov    (%di),%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	xor    %ax,%ax
L_63aa:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_63ae:
	push   %bp
	mov    %sp,%bp
	mov	$10,%bx	/* sc: unlink */
	mov    0x4(%bp),%ax
	/* v7: 10:unlink */
	int	$0xf1
	jcxz   L_63bf
	mov    %cx,D_291c
L_63bf:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_63c2:
	push   %bp
	mov    %sp,%bp
	mov	$4,%bx	/* sc: write */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 4:write */
	int	$0xf1
	jcxz   L_63d9
	mov    %cx,D_291c
L_63d9:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_63dc:
	push   %bp
	mov    %sp,%bp
	int    $0xf4
	filds  0xc(%bp)
	fldl   0x4(%bp)
	fscale
	fxch   %st(1)
	fistps 0xc(%bp)
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_63f6:
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


L_6412:
	push   %bp
	mov    %sp,%bp
	mov	$1,%bx	/* sc: exit */
	mov    0x4(%bp),%ax
	/* v7: 1:exit */
	int	$0xf1
	jcxz   L_6423
	mov    %cx,D_291c
L_6423:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


	.data

	.byte	0x00, 0x00	/* |..| */
D_0002:
	.byte	0xbf, 0x0f	/* |..| */
D_0004:
	.byte	0x60, 0x20, 0x69, 0x20	/* |` i | */
	.byte	0x80, 0x20, 0x92, 0x20, 0xaa, 0x20, 0xc2, 0x20	/* |. . . . | */
	.byte	0xd3, 0x20, 0xe3, 0x20, 0xf4, 0x20, 0x05, 0x21	/* |. . . .!| */
	.byte	0x12, 0x21, 0x23, 0x21, 0x3e, 0x21, 0x59, 0x21	/* |.!#!>!Y!| */
	.byte	0x69, 0x21, 0x78, 0x21, 0x8b, 0x21, 0x9b, 0x21	/* |i!x!.!.!| */
	.byte	0xab, 0x21, 0xbf, 0x21, 0xce, 0x21, 0xee, 0x21	/* |.!.!.!.!| */
	.byte	0x09, 0x22, 0x1a, 0x22, 0x3d, 0x22, 0x4f, 0x22	/* |."."="O"| */
	.byte	0x68, 0x22, 0x76, 0x22, 0x92, 0x22, 0xaa, 0x22	/* |h"v"."."| */
	.byte	0xc2, 0x22, 0xdc, 0x22, 0xeb, 0x22, 0x04, 0x23	/* |.".".".#| */
	.byte	0x1b, 0x23, 0x33, 0x23, 0x5a, 0x23, 0x88, 0x23	/* |.#3#Z#.#| */
	.byte	0xab, 0x23, 0xcc, 0x23, 0xe7, 0x23, 0xfa, 0x23	/* |.#.#.#.#| */
	.byte	0x16, 0x24, 0x20, 0x24, 0x00, 0x00	/* |.$ $..| */
D_005e:
	.byte	0x00, 0x00	/* |..| */
	.ascii	"<unused>\0"
	.ascii	"Missing .end statement\0"
D_0080:
	.ascii	"Invalid character\0"
	.ascii	"Multiply defined symbol\0"
	.ascii	"Symbol storage exceeded\0"
	.byte	0x4f, 0x66, 0x66, 0x73, 0x65, 0x74, 0x20, 0x74	/* |Offset t| */
	.byte	0x6f, 0x6f, 0x20, 0x6c, 0x61, 0x72	/* |oo lar| */
D_00d0:
	.byte	0x67, 0x65, 0x00, 0x53, 0x79, 0x6d, 0x62, 0x6f	/* |ge.Symbo| */
D_00d8:
	.byte	0x6c	/* |l| */
D_00d9:
	.byte	0x20	/* | | */
D_00da:
	.byte	0x74, 0x6f	/* |to| */
D_00dc:
	.byte	0x6f	/* |o| */
D_00dd:
	.byte	0x20	/* | | */
D_00de:
	.ascii	"long\0"
	.ascii	"Undefined symbol\0"
	.byte	0x49, 0x6e	/* |In| */
D_00f6:
	.byte	0x76, 0x61, 0x6c, 0x69, 0x64, 0x20, 0x63, 0x6f	/* |valid co| */
	.byte	0x6e	/* |n| */
D_00ff:
	.ascii	"stant\0"
	.ascii	"Invalid term\0"
	.ascii	"Invalid operator\0"
	.ascii	"Non-relocatable expression\0"
	.ascii	"Wrong type for instruction\0"
	.ascii	"Invalid operand\0"
	.ascii	"Invalid symbol\0"
	.ascii	"Invalid assignment\0"
	.ascii	"Too many labels\0"
	.ascii	"Invalid op-code\0"
	.ascii	"Invalid entry point\0"
	.ascii	"Invalid string\0"
	.ascii	"Bad filename or too many levels\0"
	.ascii	"Warning--attribute ignored\0"
	.ascii	".Error statement\0"
	.ascii	"Too many levels: statement ignored\0"
	.ascii	"Invalid condition\0"
	.ascii	"Wrong number of operands\0"
	.ascii	"Line too long\0"
	.ascii	"Invalid register expression\0"
	.ascii	"Invalid machine address\0"
	.ascii	"Unimplemented directive\0"
	.ascii	"Cannot open inserted file\0"
	.ascii	"Invalid string\0"
	.ascii	"Too many macro arguments\0"
	.ascii	"Invalid macro argument\0"
	.ascii	"Invalid formal argument\0"
	.ascii	"Inappropriate .endc statement; ignored\0"
	.ascii	"Warning--relative address may be out of range\0"
	.ascii	"Warning--invalid argument; ignored\0"
	.ascii	"Invalid instruction vector index\0"
	.ascii	"Invalid instruction vector\0"
	.ascii	"Invalid macro name\0"
	.ascii	"Unable to expand time macro\0"
	.ascii	"Bad csect\0"
	.ascii	"Odd address\0"
D_042c:
	.ascii	"Assembler Error-- \0"
D_043f:
	.byte	0x22, 0x25, 0x73, 0x22, 0x2c, 0x20, 0x6c, 0x69	/* |"%s", li| */
	.byte	0x6e, 0x65, 0x20, 0x25, 0x64, 0x3a, 0x20, 0x25	/* |ne %d: %| */
	.byte	0x73, 0x0a, 0x00	/* |s..| */
D_0452:
	.byte	0x22, 0x25, 0x73, 0x22, 0x2c, 0x20, 0x6c, 0x69	/* |"%s", li| */
	.byte	0x6e, 0x65, 0x20, 0x25, 0x64, 0x3a, 0x20, 0x28	/* |ne %d: (| */
	.byte	0x77, 0x61, 0x72, 0x6e, 0x69, 0x6e, 0x67, 0x29	/* |warning)| */
	.byte	0x20, 0x25, 0x73, 0x0a, 0x00, 0x00	/* | %s...| */
D_0470:
	.byte	0x00, 0x00	/* |..| */
D_0472:
	.byte	0x00, 0x00	/* |..| */
D_0474:
	.byte	0x00, 0x00	/* |..| */
D_0476:
	.byte	0x00, 0x00	/* |..| */
D_0478:
	.byte	0x00, 0x00	/* |..| */
D_047a:
	.byte	0x00, 0x00	/* |..| */
D_047c:
	.byte	0x00, 0x00	/* |..| */
D_047e:
	.byte	0x00, 0x00	/* |..| */
D_0480:
	.byte	0x12, 0x29, 0x01, 0x00, 0x16, 0x29	/* |.)...)| */
	.byte	0x02, 0x00, 0x1a, 0x29, 0x03, 0x00, 0x1e, 0x29	/* |...)...)| */
	.byte	0x04, 0x00, 0x22, 0x29, 0x05, 0x00, 0x26, 0x29	/* |..")..&)| */
	.byte	0x06, 0x00, 0x2a, 0x29, 0x07, 0x00, 0x2e, 0x29	/* |..*)...)| */
	.byte	0x08, 0x00, 0x33, 0x29, 0x09, 0x00, 0x39, 0x29	/* |..3)..9)| */
	.byte	0x0a, 0x00, 0x3d, 0x29, 0x0b, 0x00, 0x41, 0x29	/* |..=)..A)| */
	.byte	0x0c, 0x00, 0x45, 0x29, 0x0d, 0x00, 0x49, 0x29	/* |..E)..I)| */
	.byte	0x0e, 0x00, 0x4d, 0x29, 0x0f, 0x00, 0x51, 0x29	/* |..M)..Q)| */
	.byte	0x10, 0x00, 0x56, 0x29, 0x11, 0x00, 0x5c, 0x29	/* |..V)..\)| */
	.byte	0x12, 0x00, 0x62, 0x29, 0x13, 0x00, 0x66, 0x29	/* |..b)..f)| */
	.byte	0x14, 0x00, 0x6a, 0x29, 0x15, 0x00, 0x6e, 0x29	/* |..j)..n)| */
	.byte	0x16, 0x00, 0x72, 0x29, 0x17, 0x00, 0x76, 0x29	/* |..r)..v)| */
	.byte	0x18, 0x00, 0x7a, 0x29, 0x19, 0x00, 0x7f, 0x29	/* |..z)...)| */
	.byte	0x1a, 0x00, 0x84, 0x29, 0x1b, 0x00, 0x87, 0x29	/* |...)...)| */
	.byte	0x1c, 0x00, 0x8b, 0x29, 0x1d, 0x00, 0x8f, 0x29	/* |...)...)| */
	.byte	0x1e, 0x00, 0x94, 0x29, 0x1f, 0x00, 0x98, 0x29	/* |...)...)| */
	.byte	0x20, 0x00, 0x9d, 0x29, 0x21, 0x00, 0xa1, 0x29	/* | ..)!..)| */
	.byte	0x22, 0x00, 0xa3, 0x29, 0x23, 0x00, 0xa6, 0x29	/* |"..)#..)| */
	.byte	0x23, 0x00, 0xa9, 0x29, 0x24, 0x00, 0xac, 0x29	/* |#..)$..)| */
	.byte	0x24, 0x00, 0xb1, 0x29, 0x25, 0x00, 0xb5, 0x29	/* |$..)%..)| */
	.byte	0x25, 0x00, 0xb9, 0x29, 0x26, 0x00, 0xbc, 0x29	/* |%..)&..)| */
	.byte	0x26, 0x00, 0xc1, 0x29, 0x27, 0x00, 0xc5, 0x29	/* |&..)'..)| */
	.byte	0x27, 0x00, 0xc9, 0x29, 0x28, 0x00, 0xcc, 0x29	/* |'..)(..)| */
	.byte	0x28, 0x00, 0xd0, 0x29, 0x29, 0x00, 0xd3, 0x29	/* |(..))..)| */
	.byte	0x2a, 0x00, 0xd6, 0x29, 0x2b, 0x00, 0xda, 0x29	/* |*..)+..)| */
	.byte	0x2b, 0x00, 0xde, 0x29, 0x2c, 0x00, 0xe2, 0x29	/* |+..),..)| */
	.byte	0x2c, 0x00, 0xe6, 0x29, 0x2d, 0x00, 0xeb, 0x29	/* |,..)-..)| */
	.byte	0x2d, 0x00, 0xee, 0x29, 0x2e, 0x00, 0xf2, 0x29	/* |-..)...)| */
	.byte	0x2e, 0x00, 0xf6, 0x29, 0x2f, 0x00, 0xfb, 0x29	/* |...)/..)| */
	.byte	0x2f, 0x00, 0xfe, 0x29, 0x30, 0x00, 0x02, 0x2a	/* |/..)0..*| */
	.byte	0x30, 0x00, 0x06, 0x2a, 0x31, 0x00, 0x0a, 0x2a	/* |0..*1..*| */
	.byte	0x32, 0x00, 0x0e, 0x2a, 0x33, 0x00, 0x13, 0x2a	/* |2..*3..*| */
	.byte	0x34, 0x00, 0x17, 0x2a, 0x35, 0x00, 0x1c, 0x2a	/* |4..*5..*| */
	.byte	0x36, 0x00, 0x21, 0x2a, 0x37, 0x00, 0x25, 0x2a	/* |6.!*7.%*| */
	.byte	0x38, 0x00, 0x29, 0x2a, 0x39, 0x00, 0x2d, 0x2a	/* |8.)*9.-*| */
	.byte	0x3a, 0x00, 0x32, 0x2a, 0x3b, 0x00, 0x38, 0x2a	/* |:.2*;.8*| */
	.byte	0x3c, 0x00, 0x3e, 0x2a, 0x3d, 0x00, 0x43, 0x2a	/* |<.>*=.C*| */
	.byte	0x3e, 0x00, 0x49, 0x2a, 0x3e, 0x00, 0x4f, 0x2a	/* |>.I*>.O*| */
	.byte	0x3f, 0x00, 0x56, 0x2a, 0x3f, 0x00, 0x5d, 0x2a	/* |?.V*?.]*| */
	.byte	0x40, 0x00, 0x61, 0x2a, 0x41, 0x00, 0x66, 0x2a	/* |@.a*A.f*| */
	.byte	0x42, 0x00, 0x6c, 0x2a, 0x43, 0x00, 0x72, 0x2a	/* |B.l*C.r*| */
	.byte	0x44, 0x00, 0x76, 0x2a, 0x45, 0x00, 0x7a, 0x2a	/* |D.v*E.z*| */
	.byte	0x46, 0x00, 0x7e, 0x2a, 0x47, 0x00, 0x81, 0x2a	/* |F.~*G..*| */
	.byte	0x48, 0x00, 0x85, 0x2a, 0x49, 0x00, 0x8a, 0x2a	/* |H..*I..*| */
	.byte	0x4a, 0x00, 0x8e, 0x2a, 0x4b, 0x00, 0x93, 0x2a	/* |J..*K..*| */
	.byte	0x4c, 0x00, 0x98, 0x2a, 0x4d, 0x00, 0x9e, 0x2a	/* |L..*M..*| */
	.byte	0x4e, 0x00, 0xa2, 0x2a, 0x4f, 0x00, 0xa6, 0x2a	/* |N..*O..*| */
	.byte	0x50, 0x00, 0xac, 0x2a, 0x51, 0x00, 0xb1, 0x2a	/* |P..*Q..*| */
	.byte	0x52, 0x00, 0xb5, 0x2a, 0x53, 0x00, 0xba, 0x2a	/* |R..*S..*| */
	.byte	0x54, 0x00, 0xbe, 0x2a, 0x55, 0x00, 0xc2, 0x2a	/* |T..*U..*| */
	.byte	0x56, 0x00, 0xc7, 0x2a, 0x57, 0x00, 0xcb, 0x2a	/* |V..*W..*| */
	.byte	0x58, 0x00, 0xcf, 0x2a, 0x5a, 0x00, 0xd5, 0x2a	/* |X..*Z..*| */
	.byte	0x5b, 0x00, 0xdb, 0x2a, 0x57, 0x00, 0xdf, 0x2a	/* |[..*W..*| */
	.byte	0x5d, 0x00, 0xe3, 0x2a	/* |]..*| */
	.byte	0x5e, 0x00, 0xe7, 0x2a, 0x5c, 0x00, 0xeb, 0x2a	/* |^..*\..*| */
	.byte	0x5f, 0x00, 0xef, 0x2a, 0x60, 0x00, 0xf3, 0x2a	/* |_..*`..*| */
	.byte	0x61, 0x00, 0xf9, 0x2a, 0x62, 0x00, 0xff, 0x2a	/* |a..*b..*| */
	.byte	0x63, 0x00, 0x03, 0x2b, 0x59, 0x00, 0x07, 0x2b	/* |c..+Y..+| */
	.byte	0x64, 0x00, 0x0c, 0x2b, 0x65, 0x00, 0x11, 0x2b	/* |d..+e..+| */
	.byte	0x66, 0x00, 0x16, 0x2b, 0x67, 0x00, 0x1b, 0x2b	/* |f..+g..+| */
	.byte	0x68, 0x00, 0x1f, 0x2b, 0x69, 0x00, 0x23, 0x2b	/* |h..+i.#+| */
	.byte	0x6a, 0x00, 0x27, 0x2b, 0x6b, 0x00, 0x2b, 0x2b	/* |j.'+k.++| */
	.byte	0x6c, 0x00, 0x2f, 0x2b, 0x6d, 0x00, 0x33, 0x2b	/* |l./+m.3+| */
	.byte	0x6e, 0x00, 0x37, 0x2b, 0x6f, 0x00, 0x3c, 0x2b	/* |n.7+o.<+| */
	.byte	0x70, 0x00, 0x40, 0x2b, 0x71, 0x00, 0x44, 0x2b	/* |p.@+q.D+| */
	.byte	0x72, 0x00, 0x49, 0x2b, 0x73, 0x00, 0x4c, 0x2b	/* |r.I+s.L+| */
	.byte	0x74, 0x00, 0x51, 0x2b, 0x75, 0x00, 0x56, 0x2b	/* |t.Q+u.V+| */
	.byte	0x76, 0x00, 0x5b, 0x2b, 0x77, 0x00, 0x60, 0x2b	/* |v.[+w.`+| */
	.byte	0x78, 0x00, 0x65, 0x2b, 0x79, 0x00, 0x6a, 0x2b	/* |x.e+y.j+| */
	.byte	0x7a, 0x00, 0x6f, 0x2b, 0x7b, 0x00, 0x74, 0x2b	/* |z.o+{.t+| */
	.byte	0x7c, 0x00, 0x7a, 0x2b, 0x7d, 0x00, 0x7f, 0x2b	/* ||.z+}..+| */
	.byte	0x7e, 0x00, 0x85, 0x2b, 0x7f, 0x00, 0x8a, 0x2b	/* |~..+...+| */
	.byte	0x80, 0x00, 0x8f, 0x2b	/* |...+| */
	.byte	0x80, 0x00, 0x94, 0x2b, 0x81, 0x00, 0x99, 0x2b	/* |...+...+| */
	.byte	0x82, 0x00, 0x9e, 0x2b, 0x83, 0x00, 0xa3, 0x2b	/* |...+...+| */
	.byte	0x84, 0x00, 0xa8, 0x2b, 0x85, 0x00, 0xad, 0x2b	/* |...+...+| */
	.byte	0x86, 0x00, 0xb2, 0x2b, 0x87, 0x00, 0xb7, 0x2b	/* |...+...+| */
	.byte	0x88, 0x00, 0xbd, 0x2b, 0x89, 0x00, 0xc1, 0x2b	/* |...+...+| */
	.byte	0x8a, 0x00, 0xc6, 0x2b, 0x8b, 0x00, 0xcb, 0x2b	/* |...+...+| */
	.byte	0x8c, 0x00, 0xd1, 0x2b, 0x8d, 0x00, 0xd7, 0x2b	/* |...+...+| */
	.byte	0x8e, 0x00, 0xdd, 0x2b, 0x8f, 0x00, 0xe3, 0x2b	/* |...+...+| */
	.byte	0x90, 0x00, 0xe8, 0x2b, 0x91, 0x00, 0xee, 0x2b	/* |...+...+| */
	.byte	0x94, 0x00, 0xf3, 0x2b, 0x95, 0x00, 0xf9, 0x2b	/* |...+...+| */
	.byte	0x96, 0x00	/* |..| */
D_0710:
	.byte	0xff, 0x2b, 0x97, 0x00, 0x05, 0x2c, 0x98, 0x00	/* |.+...,..| */
	.byte	0x0b, 0x2c, 0x99, 0x00, 0x11, 0x2c, 0x9a, 0x00	/* |.,...,..| */
	.byte	0x16, 0x2c, 0x9b, 0x00, 0x1d, 0x2c, 0x9c, 0x00	/* |.,...,..| */
	.byte	0x24, 0x2c, 0x9d, 0x00, 0x2b, 0x2c, 0x9e, 0x00	/* |$,..+,..| */
	.byte	0x32, 0x2c, 0x9f, 0x00, 0x38, 0x2c, 0xa0, 0x00	/* |2,..8,..| */
	.byte	0x3f, 0x2c, 0xa1, 0x00, 0x47, 0x2c, 0xa2, 0x00	/* |?,..G,..| */
	.byte	0x4d, 0x2c, 0xa3, 0x00, 0x53, 0x2c, 0xa4, 0x00	/* |M,..S,..| */
	.byte	0x59, 0x2c, 0xa5, 0x00, 0x5f, 0x2c, 0xa6, 0x00	/* |Y,.._,..| */
	.byte	0x65, 0x2c, 0xa7, 0x00, 0x6a, 0x2c, 0xa8, 0x00	/* |e,..j,..| */
	.byte	0x70, 0x2c, 0xa9, 0x00, 0x76, 0x2c, 0xaa, 0x00	/* |p,..v,..| */
	.byte	0x7d, 0x2c, 0xab, 0x00, 0x82, 0x2c, 0xac, 0x00	/* |},...,..| */
	.byte	0x88, 0x2c, 0xad, 0x00, 0x90, 0x2c, 0xae, 0x00	/* |.,...,..| */
	.byte	0x96, 0x2c, 0xaf, 0x00, 0x9c, 0x2c, 0xb0, 0x00	/* |.,...,..| */
	.byte	0xa3, 0x2c, 0xb1, 0x00, 0xa8, 0x2c, 0xb2, 0x00	/* |.,...,..| */
	.byte	0xad, 0x2c, 0xb3, 0x00, 0xb2, 0x2c, 0xb4, 0x00	/* |.,...,..| */
	.byte	0xb7, 0x2c, 0xb5, 0x00, 0xbb, 0x2c, 0xb6, 0x00	/* |.,...,..| */
	.byte	0xc2, 0x2c, 0xb7, 0x00, 0xc9, 0x2c, 0xb8, 0x00	/* |.,...,..| */
	.byte	0xd0, 0x2c, 0xb9, 0x00, 0xd7, 0x2c, 0xba, 0x00	/* |.,...,..| */
	.byte	0xdd, 0x2c, 0xbb, 0x00, 0xe2, 0x2c, 0xbc, 0x00	/* |.,...,..| */
	.byte	0xe7, 0x2c, 0xbd, 0x00, 0xec, 0x2c, 0xbe, 0x00	/* |.,...,..| */
	.byte	0xf2, 0x2c, 0xbf, 0x00, 0xf8, 0x2c, 0xc0, 0x00	/* |.,...,..| */
	.byte	0xfe, 0x2c, 0xc1, 0x00, 0x04, 0x2d, 0xc2, 0x00	/* |.,...-..| */
	.byte	0x09, 0x2d, 0xc3, 0x00, 0x0f, 0x2d, 0xc6, 0x00	/* |.-...-..| */
	.byte	0x16, 0x2d, 0xc7, 0x00, 0x1c, 0x2d, 0xc8, 0x00	/* |.-...-..| */
	.byte	0x22, 0x2d, 0xc9, 0x00, 0x29, 0x2d, 0xca, 0x00	/* |"-..)-..| */
	.byte	0x31, 0x2d, 0xcb, 0x00, 0x37, 0x2d, 0xcc, 0x00	/* |1-..7-..| */
	.byte	0x3e, 0x2d, 0xcd, 0x00, 0x44, 0x2d, 0xce, 0x00	/* |>-..D-..| */
	.byte	0x49, 0x2d, 0xcf, 0x00, 0x4e, 0x2d	/* |I-..N-| */
	.byte	0xd0, 0x00, 0x53, 0x2d, 0xd1, 0x00, 0x58, 0x2d	/* |..S-..X-| */
	.byte	0xd2, 0x00, 0x5c, 0x2d, 0xd3, 0x00, 0x62, 0x2d	/* |..\-..b-| */
	.byte	0xd4, 0x00, 0x68, 0x2d, 0xd5, 0x00, 0x6e, 0x2d	/* |..h-..n-| */
	.byte	0xd6, 0x00, 0x74, 0x2d, 0xd7, 0x00, 0x79, 0x2d	/* |..t-..y-| */
	.byte	0xd8, 0x00, 0x7f, 0x2d	/* |...-| */
	.byte	0xd9, 0x00, 0x86, 0x2d	/* |...-| */
	.byte	0xda, 0x00, 0x8c, 0x2d, 0xdb, 0x00, 0x92, 0x2d	/* |...-...-| */
	.byte	0xdc, 0x00, 0x98, 0x2d	/* |...-| */
	.byte	0xdd, 0x00, 0x9e, 0x2d	/* |...-| */
	.byte	0xde, 0x00, 0xa4, 0x2d, 0xdf, 0x00, 0xa9, 0x2d	/* |...-...-| */
	.byte	0xe0, 0x00, 0xaf, 0x2d, 0xe1, 0x00, 0xb5, 0x2d	/* |...-...-| */
	.byte	0xe2, 0x00, 0xbc, 0x2d, 0xe3, 0x00, 0xc1, 0x2d	/* |...-...-| */
	.byte	0xe4, 0x00, 0xc7, 0x2d, 0xe5, 0x00, 0xcc, 0x2d	/* |...-...-| */
	.byte	0xe6, 0x00, 0xd1, 0x2d, 0xe7, 0x00, 0xd9, 0x2d	/* |...-...-| */
	.byte	0xe8, 0x00, 0xdf, 0x2d, 0xe9, 0x00, 0xe7, 0x2d	/* |...-...-| */
	.byte	0xea, 0x00, 0xed, 0x2d, 0xeb, 0x00, 0xf4, 0x2d	/* |...-...-| */
	.byte	0xec, 0x00, 0xfb, 0x2d, 0xed, 0x00, 0x02, 0x2e	/* |...-....| */
	.byte	0xee, 0x00, 0x09, 0x2e, 0xef, 0x00, 0x10, 0x2e	/* |........| */
	.byte	0xf0, 0x00, 0x17, 0x2e, 0xf1, 0x00, 0x1e, 0x2e	/* |........| */
	.byte	0xf2, 0x00, 0x25, 0x2e, 0xf3, 0x00, 0x2b, 0x2e	/* |..%...+.| */
	.byte	0xf4, 0x00	/* |..| */
D_0880:
	.byte	0x31, 0x2e, 0xf5, 0x00, 0x37, 0x2e	/* |1...7.| */
	.byte	0xf6, 0x00, 0x3d, 0x2e, 0xf7, 0x00, 0x43, 0x2e	/* |..=...C.| */
	.byte	0xf8, 0x00, 0x48, 0x2e, 0xf9, 0x00, 0x4f, 0x2e	/* |..H...O.| */
	.byte	0xfa, 0x00, 0x55, 0x2e, 0xfb, 0x00, 0x5b, 0x2e	/* |..U...[.| */
	.byte	0xfd, 0x00, 0x62, 0x2e, 0xfc, 0x00, 0x69, 0x2e	/* |..b...i.| */
	.byte	0xfe, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_08ac:
	.byte	0x00, 0x00	/* |..| */
D_08ae:
	.byte	0x17, 0x2f, 0x00, 0x00, 0x1a, 0x2f, 0x01, 0x00	/* |./.../..| */
	.byte	0x1d, 0x2f	/* |./| */
	.byte	0x02, 0x00, 0x20, 0x2f, 0x03, 0x00, 0x23, 0x2f	/* |.. /..#/| */
	.byte	0x04, 0x00, 0x26, 0x2f, 0x05, 0x00, 0x29, 0x2f	/* |..&/..)/| */
	.byte	0x06, 0x00, 0x2c, 0x2f, 0x07, 0x00, 0x2f, 0x2f	/* |..,/..//| */
D_08d0:
	.byte	0x08, 0x00, 0x32, 0x2f, 0x09, 0x00, 0x35, 0x2f	/* |..2/..5/| */
D_08d8:
	.byte	0x0a	/* |.| */
D_08d9:
	.byte	0x00	/* |.| */
D_08da:
	.byte	0x38, 0x2f	/* |8/| */
D_08dc:
	.byte	0x0b, 0x00	/* |..| */
D_08de:
	.byte	0x3b, 0x2f, 0x0c, 0x00, 0x3e, 0x2f, 0x0d, 0x00	/* |;/..>/..| */
	.byte	0x41, 0x2f, 0x0e, 0x00, 0x44, 0x2f, 0x0f, 0x00	/* |A/..D/..| */
	.byte	0x47, 0x2f, 0x10, 0x00, 0x4a, 0x2f, 0x11, 0x00	/* |G/..J/..| */
D_08f6:
	.byte	0x4d, 0x2f, 0x12, 0x00, 0x50, 0x2f, 0x13, 0x00	/* |M/..P/..| */
	.byte	0x53	/* |S| */
D_08ff:
	.byte	0x2f, 0x40, 0x00, 0x59, 0x2f, 0x41, 0x00, 0x5f	/* |/@.Y/A._| */
	.byte	0x2f, 0x42, 0x00, 0x65, 0x2f, 0x43, 0x00, 0x00	/* |/B.e/C..| */
	.byte	0x00, 0x00, 0x00, 0x61, 0x61, 0x61, 0x00, 0x61	/* |...aaa.a| */
	.byte	0x61, 0x64, 0x00, 0x61, 0x61, 0x6d, 0x00, 0x61	/* |ad.aam.a| */
	.byte	0x61, 0x73, 0x00, 0x61, 0x64, 0x63, 0x00, 0x61	/* |as.adc.a| */
	.byte	0x64, 0x64, 0x00, 0x61, 0x6e, 0x64, 0x00	/* |dd.and.| */
	.ascii	"call\0"
	.ascii	"calli\0"
	.byte	0x63, 0x62, 0x77, 0x00, 0x63, 0x6c, 0x63, 0x00	/* |cbw.clc.| */
	.byte	0x63, 0x6c, 0x64, 0x00, 0x63, 0x6c, 0x69, 0x00	/* |cld.cli.| */
	.byte	0x63, 0x6d, 0x63, 0x00, 0x63, 0x6d, 0x70, 0x00	/* |cmc.cmp.| */
	.ascii	"cmpb\0"
	.ascii	"cmpsb\0"
	.ascii	"cmpsw\0"
	.byte	0x63, 0x77, 0x64, 0x00, 0x64, 0x61, 0x61, 0x00	/* |cwd.daa.| */
	.byte	0x64, 0x61, 0x73, 0x00, 0x64, 0x65, 0x63, 0x00	/* |das.dec.| */
	.byte	0x64, 0x69, 0x76, 0x00, 0x65, 0x73, 0x63, 0x00	/* |div.esc.| */
	.ascii	"idiv\0"
	.ascii	"imul\0"
	.byte	0x69, 0x6e, 0x00, 0x69, 0x6e, 0x63, 0x00, 0x69	/* |in.inc.i| */
	.byte	0x6e, 0x74, 0x00	/* |nt.| */
	.ascii	"into\0"
	.byte	0x69, 0x6e, 0x77, 0x00	/* |inw.| */
	.ascii	"iret\0"
	.byte	0x68, 0x6c, 0x74, 0x00, 0x6a, 0x00, 0x6a, 0x65	/* |hlt.j.je| */
	.byte	0x00, 0x6a, 0x7a, 0x00, 0x6a, 0x6c, 0x00	/* |.jz.jl.| */
	.ascii	"jnge\0"
	.byte	0x6a, 0x6c, 0x65, 0x00, 0x6a, 0x6e, 0x67, 0x00	/* |jle.jng.| */
	.byte	0x6a, 0x62, 0x00	/* |jb.| */
	.ascii	"jnae\0"
	.byte	0x6a, 0x62, 0x65, 0x00, 0x6a, 0x6e, 0x61, 0x00	/* |jbe.jna.| */
	.byte	0x6a, 0x70, 0x00, 0x6a, 0x70, 0x65, 0x00, 0x6a	/* |jp.jpe.j| */
	.byte	0x6f, 0x00, 0x6a, 0x73, 0x00, 0x6a, 0x6e, 0x65	/* |o.js.jne| */
	.byte	0x00, 0x6a, 0x6e, 0x7a, 0x00, 0x6a, 0x6e, 0x6c	/* |.jnz.jnl| */
	.byte	0x00, 0x6a, 0x67, 0x65, 0x00	/* |.jge.| */
	.ascii	"jnle\0"
	.byte	0x6a, 0x67, 0x00, 0x6a, 0x6e, 0x62, 0x00, 0x6a	/* |jg.jnb.j| */
	.byte	0x61, 0x65, 0x00	/* |ae.| */
	.ascii	"jnbe\0"
	.byte	0x6a, 0x61, 0x00, 0x6a, 0x6e, 0x70, 0x00, 0x6a	/* |ja.jnp.j| */
	.byte	0x70, 0x6f, 0x00, 0x6a, 0x6e, 0x6f, 0x00, 0x6a	/* |po.jno.j| */
	.byte	0x6e, 0x73, 0x00	/* |ns.| */
	.ascii	"jcxz\0"
	.byte	0x6a, 0x6d, 0x70, 0x00	/* |jmp.| */
	.ascii	"jmpi\0"
	.ascii	"lahf\0"
	.byte	0x6c, 0x64, 0x73, 0x00, 0x6c, 0x65, 0x61, 0x00	/* |lds.lea.| */
	.byte	0x6c, 0x65, 0x73, 0x00	/* |les.| */
	.ascii	"lock\0"
	.ascii	"lodsb\0"
	.ascii	"lodsw\0"
	.ascii	"loop\0"
	.ascii	"loope\0"
	.ascii	"loopz\0"
	.ascii	"loopne\0"
	.ascii	"loopnz\0"
	.byte	0x6d, 0x6f, 0x76, 0x00	/* |mov.| */
	.ascii	"movb\0"
	.ascii	"movsb\0"
	.ascii	"movsw\0"
	.byte	0x6d, 0x75, 0x6c, 0x00, 0x6e, 0x65, 0x67, 0x00	/* |mul.neg.| */
	.byte	0x6e, 0x6f, 0x74, 0x00, 0x6f, 0x72, 0x00, 0x6f	/* |not.or.o| */
	.byte	0x75, 0x74, 0x00	/* |ut.| */
	.ascii	"outw\0"
	.byte	0x70, 0x6f, 0x70, 0x00	/* |pop.| */
	.ascii	"popf\0"
	.ascii	"push\0"
	.ascii	"pushf\0"
	.byte	0x72, 0x63, 0x6c, 0x00, 0x72, 0x63, 0x72, 0x00	/* |rcl.rcr.| */
	.ascii	"repnz\0"
	.ascii	"repz\0"
	.byte	0x72, 0x65, 0x74, 0x00	/* |ret.| */
	.ascii	"reti\0"
	.byte	0x72, 0x6f, 0x6c, 0x00, 0x72, 0x6f, 0x72, 0x00	/* |rol.ror.| */
	.ascii	"sahf\0"
	.byte	0x73, 0x61, 0x6c, 0x00, 0x73, 0x61, 0x72, 0x00	/* |sal.sar.| */
	.ascii	"scasb\0"
	.ascii	"scasw\0"
	.byte	0x73, 0x68, 0x6c, 0x00, 0x73, 0x68, 0x72, 0x00	/* |shl.shr.| */
	.byte	0x73, 0x74, 0x63, 0x00, 0x73, 0x65, 0x67, 0x00	/* |stc.seg.| */
	.byte	0x73, 0x74, 0x64, 0x00, 0x73, 0x74, 0x69, 0x00	/* |std.sti.| */
	.ascii	"stosb\0"
	.ascii	"stosw\0"
	.byte	0x73, 0x75, 0x62, 0x00, 0x73, 0x62, 0x62, 0x00	/* |sub.sbb.| */
	.ascii	"test\0"
	.ascii	"wait\0"
	.ascii	"xchg\0"
	.ascii	"xlat\0"
	.byte	0x78, 0x6f, 0x72, 0x00, 0x62, 0x65, 0x71, 0x00	/* |xor.beq.| */
	.byte	0x62, 0x6e, 0x65, 0x00, 0x62, 0x6c, 0x65, 0x00	/* |bne.ble.| */
	.byte	0x62, 0x67, 0x74, 0x00, 0x62, 0x6c, 0x74, 0x00	/* |bgt.blt.| */
	.byte	0x62, 0x67, 0x65, 0x00	/* |bge.| */
	.ascii	"blos\0"
	.byte	0x62, 0x68, 0x69, 0x00, 0x62, 0x6c, 0x6f, 0x00	/* |bhi.blo.| */
	.ascii	"bhis\0"
	.byte	0x62, 0x72, 0x00	/* |br.| */
	.ascii	"addb\0"
	.ascii	"adcb\0"
	.ascii	"incb\0"
	.ascii	"subb\0"
	.ascii	"sbbb\0"
	.ascii	"decb\0"
	.ascii	"negb\0"
	.ascii	"mulb\0"
	.ascii	"imulb\0"
	.ascii	"divb\0"
	.ascii	"idivb\0"
	.ascii	"notb\0"
	.ascii	"salb\0"
	.ascii	"shlb\0"
	.ascii	"shrb\0"
	.ascii	"sarb\0"
	.ascii	"rolb\0"
	.ascii	"rorb\0"
	.ascii	"rclb\0"
	.ascii	"rcrb\0"
	.ascii	"andb\0"
	.ascii	"testb\0"
	.byte	0x6f, 0x72, 0x62, 0x00	/* |orb.| */
	.ascii	"xorb\0"
	.ascii	"fabs\0"
	.ascii	"faddf\0"
	.ascii	"faddl\0"
	.ascii	"faddd\0"
	.ascii	"faddi\0"
	.ascii	"fadd\0"
	.ascii	"faddp\0"
	.ascii	"fchs\0"
	.ascii	"fclex\0"
	.ascii	"fcomf\0"
	.ascii	"fcoml\0"
	.ascii	"fcomd\0"
	.ascii	"fcomi\0"
	.ascii	"fcom\0"
	.ascii	"fcompf\0"
	.ascii	"fcompl\0"
	.ascii	"fcompd\0"
	.ascii	"fcompi\0"
	.ascii	"fcomp\0"
	.ascii	"fcompp\0"
	.ascii	"fdecstp\0"
	.ascii	"fdisi\0"
	.ascii	"fdivf\0"
	.ascii	"fdivl\0"
	.ascii	"fdivd\0"
	.ascii	"fdivi\0"
	.ascii	"fdiv\0"
	.ascii	"fdivp\0"
	.ascii	"fdivr\0"
	.ascii	"fdivrp\0"
	.ascii	"feni\0"
	.ascii	"ffree\0"
	.ascii	"fincstp\0"
	.ascii	"finit\0"
	.ascii	"fldcw\0"
	.ascii	"fldenv\0"
	.ascii	"fldf\0"
	.ascii	"fldl\0"
	.ascii	"fldd\0"
	.ascii	"fldi\0"
	.byte	0x66, 0x6c, 0x64, 0x00	/* |fld.| */
	.ascii	"fldlg2\0"
	.ascii	"fldln2\0"
	.ascii	"fldl2e\0"
	.ascii	"fldl2t\0"
	.ascii	"fldpi\0"
	.ascii	"fldz\0"
	.ascii	"fld1\0"
	.ascii	"fnop\0"
	.ascii	"fmulf\0"
	.ascii	"fmull\0"
	.ascii	"fmuld\0"
	.ascii	"fmuli\0"
	.ascii	"fmul\0"
	.ascii	"fmulp\0"
	.ascii	"fpatan\0"
	.ascii	"fprem\0"
	.ascii	"fptan\0"
	.ascii	"frstor\0"
	.ascii	"frndint\0"
	.ascii	"fsave\0"
	.ascii	"fscale\0"
	.ascii	"fsqrt\0"
	.ascii	"fstf\0"
	.ascii	"fstl\0"
	.ascii	"fstd\0"
	.ascii	"fsti\0"
	.byte	0x66, 0x73, 0x74, 0x00	/* |fst.| */
	.ascii	"fstpf\0"
	.ascii	"fstpl\0"
	.ascii	"fstpd\0"
	.ascii	"fstpi\0"
	.ascii	"fstp\0"
	.ascii	"fstcw\0"
	.ascii	"fstenv\0"
	.ascii	"fstsw\0"
	.ascii	"fsubf\0"
	.ascii	"fsubl\0"
	.ascii	"fsubd\0"
	.ascii	"fsubi\0"
	.ascii	"fsub\0"
	.ascii	"fsubp\0"
	.ascii	"fsubr\0"
	.ascii	"fsubrp\0"
	.ascii	"ftst\0"
	.ascii	"fwait\0"
	.ascii	"fxam\0"
	.ascii	"fxch\0"
	.ascii	"fxtract\0"
	.ascii	"fyl2x\0"
	.ascii	"fyl2xp1\0"
	.ascii	"f2xm1\0"
	.ascii	"fdivrf\0"
	.ascii	"fdivrl\0"
	.ascii	"fdivrd\0"
	.ascii	"fdivri\0"
	.ascii	"fsubrf\0"
	.ascii	"fsubrl\0"
	.ascii	"fsubrd\0"
	.ascii	"fsubri\0"
	.ascii	".long\0"
	.ascii	".word\0"
	.ascii	".byte\0"
	.ascii	".text\0"
	.ascii	".data\0"
	.ascii	".bss\0"
	.ascii	".globl\0"
	.ascii	".comm\0"
	.ascii	".even\0"
	.ascii	".asciz\0"
	.ascii	".ascii\0"
	.ascii	".zerow\0"
D_0e70:
	.byte	0x00	/* |.| */
D_0e71:
	.byte	0x55, 0x6e, 0x6b, 0x6e, 0x6f, 0x77, 0x6e, 0x20	/* |Unknown | */
	.byte	0x6f, 0x70, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x27	/* |option '| */
	.byte	0x25, 0x63, 0x27, 0x20, 0x69, 0x67, 0x6e, 0x6f	/* |%c' igno| */
	.byte	0x72, 0x65, 0x64, 0x2e, 0x0a, 0x00	/* |red...| */
D_0e8f:
	.byte	0x54, 0x6f, 0x6f, 0x20, 0x6d, 0x61, 0x6e, 0x79	/* |Too many| */
	.byte	0x20, 0x66, 0x69, 0x6c, 0x65, 0x20, 0x6e, 0x61	/* | file na| */
	.byte	0x6d, 0x65, 0x73, 0x20, 0x67, 0x69, 0x76, 0x65	/* |mes give| */
	.byte	0x6e, 0x0a, 0x00	/* |n..| */
D_0eaa:
	.byte	0x2e, 0x73, 0x00	/* |.s.| */
D_0ead:
	.byte	0x72, 0x00	/* |r.| */
D_0eaf:
	.byte	0x2e, 0x73, 0x00	/* |.s.| */
D_0eb2:
	.byte	0x43, 0x61, 0x6e, 0x27, 0x74, 0x20, 0x6f, 0x70	/* |Can't op| */
	.byte	0x65, 0x6e, 0x20, 0x73, 0x6f, 0x75, 0x72, 0x63	/* |en sourc| */
	.byte	0x65, 0x20, 0x66, 0x69, 0x6c, 0x65, 0x3a, 0x20	/* |e file: | */
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_0ece:
	.byte	0x72, 0x00	/* |r.| */
D_0ed0:
	.byte	0x43, 0x61, 0x6e, 0x27, 0x74, 0x20, 0x6f, 0x70	/* |Can't op| */
	.byte	0x65, 0x6e, 0x20, 0x73, 0x6f, 0x75, 0x72, 0x63	/* |en sourc| */
	.byte	0x65, 0x20, 0x66, 0x69, 0x6c, 0x65, 0x3a, 0x20	/* |e file: | */
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_0eec:
	.byte	0x2e, 0x73, 0x00	/* |.s.| */
D_0eef:
	.byte	0x2e, 0x6f, 0x00	/* |.o.| */
D_0ef2:
	.byte	0x2e, 0x6f, 0x00	/* |.o.| */
D_0ef5:
	.byte	0x77, 0x00	/* |w.| */
D_0ef7:
	.byte	0x43, 0x61, 0x6e, 0x27, 0x74, 0x20, 0x63, 0x72	/* |Can't cr| */
	.byte	0x65, 0x61, 0x74, 0x65, 0x20, 0x6f, 0x75, 0x74	/* |eate out| */
	.byte	0x70, 0x75, 0x74, 0x20, 0x66, 0x69, 0x6c, 0x65	/* |put file| */
	.byte	0x3a, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |: %s..| */
D_0f15:
	.byte	0x2e, 0x00, 0x61, 0x78, 0x00, 0x63, 0x78, 0x00	/* |..ax.cx.| */
	.byte	0x64, 0x78, 0x00, 0x62, 0x78, 0x00, 0x73, 0x70	/* |dx.bx.sp| */
	.byte	0x00, 0x62, 0x70, 0x00, 0x73, 0x69, 0x00, 0x64	/* |.bp.si.d| */
	.byte	0x69, 0x00, 0x61, 0x6c, 0x00, 0x63, 0x6c, 0x00	/* |i.al.cl.| */
	.byte	0x64, 0x6c, 0x00, 0x62, 0x6c, 0x00, 0x61, 0x68	/* |dl.bl.ah| */
	.byte	0x00, 0x63, 0x68, 0x00, 0x64, 0x68, 0x00, 0x62	/* |.ch.dh.b| */
	.byte	0x68, 0x00, 0x65, 0x73, 0x00, 0x63, 0x73, 0x00	/* |h.es.cs.| */
	.byte	0x73, 0x73, 0x00, 0x64, 0x73, 0x00	/* |ss.ds.| */
	.ascii	"bx_si\0"
	.ascii	"bx_di\0"
	.ascii	"bp_si\0"
	.ascii	"bp_di\0"
	.byte	0x00	/* |.| */
D_0f6c:
	.byte	0x6c, 0x45, 0x8c, 0x08, 0xe0, 0x09, 0xe0, 0x09	/* |lE......| */
	.byte	0xa0, 0x08, 0xce, 0x04, 0xaf, 0x04, 0x4e, 0x05	/* |......N.| */
	.byte	0x0c, 0x0e, 0x2c, 0x0e, 0xb4, 0x08, 0x68, 0x09	/* |..,...h.| */
	.byte	0x86, 0x09, 0x9a, 0x09, 0x72, 0x09, 0x2e, 0x05	/* |....r...| */
	.byte	0xe5, 0x08, 0x04, 0x09, 0x0e, 0x09, 0xbe, 0x08	/* |........| */
	.byte	0x96, 0x08, 0xaa, 0x08, 0x27, 0x0a, 0xcd, 0x0a	/* |....'...| */
	.byte	0xed, 0x0a, 0xdd, 0x0a, 0xbd, 0x0a, 0xd3, 0x0c	/* |........| */
	.byte	0x27, 0x0a, 0x13, 0x0d, 0x54, 0x09, 0xe3, 0x0c	/* |'...T...| */
	.byte	0x5e, 0x09, 0xae, 0x09, 0xd9, 0x06, 0xe3, 0x06	/* |^.......| */
	.byte	0xed, 0x06, 0xf7, 0x06, 0x01, 0x07, 0x0b, 0x07	/* |........| */
	.byte	0x15, 0x07, 0x1f, 0x07, 0x29, 0x07, 0x33, 0x07	/* |....).3.| */
	.byte	0x3d, 0x07, 0x47, 0x07, 0x51, 0x07, 0x5b, 0x07	/* |=.G.Q.[.| */
	.byte	0x65, 0x07, 0x6f, 0x07, 0x79, 0x07, 0xa1, 0x07	/* |e.o.y...| */
	.byte	0xfc, 0x0d, 0x1c, 0x0e, 0x64, 0x08, 0xc5, 0x06	/* |....d...| */
	.byte	0xbb, 0x06, 0xcf, 0x06, 0xc2, 0x09, 0x2c, 0x09	/* |......,.| */
	.byte	0x36, 0x09, 0x83, 0x07, 0x8d, 0x07, 0x97, 0x07	/* |6.......| */
	.byte	0xa5, 0x04, 0xc8, 0x08, 0xd1, 0x08, 0xdb, 0x08	/* |........| */
	.byte	0xad, 0x0a, 0x9d, 0x0a, 0x8d, 0x0a, 0x8b, 0x05	/* |........| */
	.byte	0xf3, 0x0c, 0x03, 0x0d, 0x57, 0x0b, 0x82, 0x08	/* |....W...| */
	.byte	0x57, 0x0b, 0x78, 0x08, 0x73, 0x0d, 0x83, 0x0d	/* |W.x.s...| */
	.byte	0xcc, 0x09, 0xd6, 0x09, 0x3c, 0x0e, 0x3c, 0x0e	/* |....<.<.| */
	.byte	0x53, 0x0d, 0x63, 0x0d, 0x6e, 0x08, 0x23, 0x0d	/* |S.c.n.#.| */
	.byte	0x43, 0x0d, 0x0e, 0x05, 0x18, 0x09, 0x22, 0x09	/* |C.....".| */
	.byte	0xc2, 0x0e, 0x33, 0x0d, 0x7c, 0x09, 0x90, 0x09	/* |..3.|...| */
	.byte	0xa4, 0x09, 0x40, 0x09, 0x4a, 0x09, 0xee, 0x04	/* |..@.J...| */
	.byte	0x6d, 0x05, 0xb8, 0x09, 0x02, 0x0c, 0x5a, 0x08	/* |m.....Z.| */
	.byte	0xaa, 0x05, 0xab, 0x07, 0xbb, 0x07, 0xcb, 0x07	/* |........| */
	.byte	0xdb, 0x07, 0xeb, 0x07, 0xfb, 0x07, 0x0b, 0x08	/* |........| */
	.byte	0x1b, 0x08, 0x2b, 0x08, 0x3b, 0x08, 0x4b, 0x08	/* |..+.;.K.| */
	.byte	0xc9, 0x05, 0xe7, 0x05, 0x11, 0x0a, 0x06, 0x06	/* |........| */
	.byte	0x25, 0x06, 0x11, 0x0a, 0x0c, 0x0b, 0x1b, 0x0b	/* |%.......| */
	.byte	0x2a, 0x0b, 0x39, 0x0b, 0x48, 0x0b, 0xfd, 0x0a	/* |*.9.H...| */
	.byte	0x93, 0x0d, 0xa2, 0x0d, 0xb1, 0x0d, 0xc0, 0x0d	/* |........| */
	.byte	0xcf, 0x0d, 0xde, 0x0d, 0xed, 0x0d, 0x44, 0x06	/* |......D.| */
	.byte	0x62, 0x06, 0x7f, 0x06	/* |b...| */
D_1080:
	.byte	0x9d, 0x06, 0x06, 0x0f, 0x16, 0x0f, 0x20, 0x0f	/* |...... .| */
	.byte	0x2a, 0x0f, 0x34, 0x0f, 0x3e, 0x0f, 0x4e, 0x0f	/* |*.4.>.N.| */
	.byte	0x51, 0x14, 0x51, 0x14, 0x5e, 0x0f, 0x6e, 0x0f	/* |Q.Q.^.n.| */
	.byte	0x7d, 0x0f, 0x87, 0x0f, 0x91, 0x0f, 0x9b, 0x0f	/* |}.......| */
	.byte	0xa5, 0x0f, 0xb4, 0x0f, 0xbe, 0x0f, 0xc8, 0x0f	/* |........| */
	.byte	0xd2, 0x0f, 0xdc, 0x0f, 0xeb, 0x0f, 0xfb, 0x0f	/* |........| */
	.byte	0x0b, 0x10, 0x1a, 0x10, 0x24, 0x10, 0x2e, 0x10	/* |....$...| */
	.byte	0x38, 0x10, 0x6a, 0x10, 0x7a, 0x10, 0x8a, 0x10	/* |8.j.z...| */
	.byte	0x9a, 0x10, 0xaa, 0x10, 0xb9, 0x10, 0xc3, 0x10	/* |........| */
	.byte	0xd3, 0x10, 0xe2, 0x10, 0xec, 0x10	/* |......| */
	.byte	0xf6, 0x10	/* |..| possible ptr: DS:0x10f6 */
	.byte	0x00, 0x11, 0x0a, 0x11, 0x14, 0x11, 0x1e, 0x11	/* |........| */
D_10d8:
	.byte	0x28	/* |(| */
D_10d9:
	.byte	0x11	/* |.| */
D_10da:
	.byte	0x38, 0x11	/* |8.| */
D_10dc:
	.byte	0x48	/* |H| */
D_10dd:
	.byte	0x11	/* |.| */
D_10de:
	.byte	0x58, 0x11, 0x68, 0x11	/* |X.h.| */
	.byte	0x78, 0x11	/* |x.| possible ptr: DS:0x1178 */
	.byte	0x88, 0x11, 0x98, 0x11, 0xa8, 0x11, 0xb2, 0x11	/* |........| */
	.byte	0xbc, 0x11, 0xc6, 0x11, 0xd0, 0x11, 0xe0, 0x11	/* |........| */
	.byte	0x51, 0x14	/* |Q.| */
D_10f6:
	.byte	0x51, 0x14, 0xf0, 0x11, 0x00, 0x12, 0x10, 0x12	/* |Q.......| */
	.byte	0x20, 0x12, 0x2a, 0x12, 0x3a, 0x12, 0x44, 0x12	/* | .*.:.D.| */
	.byte	0x54, 0x12, 0x64, 0x12, 0x6e, 0x12, 0x78, 0x12	/* |T.d.n.x.| */
	.byte	0x82, 0x12, 0x8c, 0x12, 0x96, 0x12, 0xa0, 0x12	/* |........| */
	.byte	0xaa, 0x12, 0xb4, 0x12, 0xbe, 0x12, 0xc8, 0x12	/* |........| */
	.byte	0xd2, 0x12, 0xdc, 0x12, 0xe6, 0x12, 0xf0, 0x12	/* |........| */
	.byte	0xfa, 0x12, 0x04, 0x13, 0x36, 0x13, 0x46, 0x13	/* |....6.F.| */
	.byte	0x56, 0x13, 0x66, 0x13, 0x76, 0x13, 0x86, 0x13	/* |V.f.v...| */
	.byte	0x90, 0x13, 0xa0, 0x13, 0xaa, 0x13, 0xba, 0x13	/* |........| */
	.byte	0xca, 0x13, 0xda, 0x13, 0x42, 0x10, 0x4c, 0x10	/* |....B.L.| */
	.byte	0x56, 0x10, 0x60, 0x10, 0x0e, 0x13, 0x18, 0x13	/* |V.`.....| */
	.byte	0x22, 0x13, 0x2c, 0x13, 0xea, 0x13, 0xf4, 0x13	/* |".,.....| */
	.byte	0xfe, 0x13, 0x07, 0x14, 0x11, 0x14, 0x1b, 0x14	/* |........| */
	.byte	0x25, 0x14, 0x2a, 0x14, 0x2f, 0x14, 0x34, 0x14	/* |%.*./.4.| */
	.byte	0x3d, 0x14, 0x47, 0x14, 0xd0, 0x16, 0xa6, 0x16	/* |=.G.....| */
	.byte	0xac, 0x17, 0xac, 0x17, 0xe5, 0x16, 0x82, 0x17	/* |........| */
	.byte	0x98, 0x17	/* |..| */
D_1178:
	.byte	0x72, 0x3c, 0x00, 0x00, 0x00, 0x00	/* |r<....| */
D_117e:
	.byte	0x00, 0x00	/* |..| */
D_1180:
	.byte	0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x78, 0x3c	/* |......x<| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x7e, 0x3c, 0x00, 0x00	/* |....~<..| */
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
D_1880:
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
D_18d0:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_18d8:
	.byte	0x00	/* |.| */
D_18d9:
	.byte	0x00	/* |.| */
D_18da:
	.byte	0x00, 0x00	/* |..| */
D_18dc:
	.byte	0x00	/* |.| */
D_18dd:
	.byte	0x00	/* |.| */
D_18de:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_18f6:
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
D_1c68:
	.byte	0x04, 0x00	/* |..| */
D_1c6a:
	.byte	0x78, 0x31	/* |x1| */
D_1c6c:
	.byte	0x78, 0x31	/* |x1| */
D_1c6e:
	.byte	0x86, 0x31	/* |.1| */
D_1c70:
	.byte	0x94	/* |.| */
	.ascii	"1.text\0"
	.ascii	".data\0"
	.ascii	".bss\0"
	.byte	0x00	/* |.| */
D_1c84:
	.byte	0x77, 0x00	/* |w.| */
D_1c86:
	.byte	0x61, 0x00	/* |a.| */
D_1c88:
	.ascii	"open on output file %s failed\0"
D_1ca6:
	.ascii	".textr\0"
D_1cad:
	.byte	0x77, 0x00	/* |w.| */
D_1caf:
	.ascii	".datar\0"
D_1cb6:
	.byte	0x77, 0x00	/* |w.| */
D_1cb8:
	.ascii	"open on output file %s failed\0"
D_1cd6:
	.ascii	".textr\0"
D_1cdd:
	.byte	0x72, 0x00	/* |r.| */
D_1cdf:
	.ascii	"cannot reopen relocation file %s\0"
D_1d00:
	.ascii	".datar\0"
D_1d07:
	.byte	0x72, 0x00	/* |r.| */
D_1d09:
	.ascii	"cannot reopen relocation file %s\0"
	.byte	0x3a, 0x3d	/* |:=| */
D_1d2c:
	.byte	0x00, 0x00	/* |..| */
D_1d2e:
	.byte	0x00, 0x00	/* |..| */
	.byte	0x02, 0x00, 0x80, 0xff, 0xff, 0xff, 0x7f, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
	.ascii	"~|^`s.sdi.c R1.7 on 6/27/80\0"
D_1d56:
	.byte	0x73, 0x64, 0x69, 0x20, 0x73, 0x74, 0x6f, 0x72	/* |sdi stor| */
	.byte	0x61, 0x67, 0x65, 0x20, 0x65, 0x78, 0x63, 0x65	/* |age exce| */
	.byte	0x65, 0x64, 0x65, 0x64, 0x0a, 0x00	/* |eded..| */
D_1d6c:
	.byte	0x73, 0x64, 0x69, 0x20, 0x62, 0x6f, 0x75, 0x6e	/* |sdi boun| */
	.byte	0x64, 0x20, 0x6c, 0x69, 0x73, 0x74, 0x20, 0x73	/* |d list s| */
	.byte	0x74, 0x6f, 0x72, 0x61, 0x67, 0x65, 0x20, 0x65	/* |torage e| */
	.byte	0x78, 0x63, 0x65, 0x65, 0x64, 0x65, 0x64, 0x0a	/* |xceeded.| */
	.byte	0x00	/* |.| */
D_1d8d:
	.byte	0x50, 0x61, 0x74, 0x68, 0x6f, 0x6c, 0x6f, 0x67	/* |Patholog| */
	.byte	0x69, 0x63, 0x61, 0x6c, 0x20, 0x73, 0x64, 0x69	/* |ical sdi| */
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_1da0:
	.byte	0x00, 0x00	/* |..| */
D_1da2:
	.byte	0x00, 0x00	/* |..| */
D_1da4:
	.byte	0x00, 0x00	/* |..| */
D_1da6:
	.byte	0x53, 0x79, 0x6d, 0x62, 0x6f, 0x6c, 0x20, 0x73	/* |Symbol s| */
	.byte	0x74, 0x6f, 0x72, 0x61, 0x67, 0x65, 0x20, 0x65	/* |torage e| */
	.byte	0x78, 0x63, 0x65, 0x65, 0x64, 0x65, 0x64, 0x0a	/* |xceeded.| */
	.byte	0x00	/* |.| */
D_1dbf:
	.byte	0x53, 0x79, 0x6d, 0x62, 0x6f, 0x6c, 0x20, 0x73	/* |Symbol s| */
	.byte	0x74, 0x6f, 0x72, 0x61, 0x67, 0x65, 0x20, 0x65	/* |torage e| */
	.byte	0x78, 0x63, 0x65, 0x65, 0x64, 0x65, 0x64, 0x0a	/* |xceeded.| */
	.byte	0x00	/* |.| */
D_1dd8:
	.byte	0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x06, 0x00, 0x05, 0x00	/* |....| */
	.byte	0x02, 0x00, 0x05, 0x00, 0x05, 0x00, 0x05, 0x00	/* |........| */
	.byte	0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x06, 0x00, 0x06, 0x00, 0x05, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x0e, 0x00, 0x07, 0x00, 0x0f, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x06, 0x00, 0x11, 0x00, 0x08, 0x00, 0x09, 0x00	/* |........| */
	.byte	0x0c, 0x00, 0x03, 0x00, 0x0a, 0x00	/* |......| */
	.byte	0x04, 0x00, 0x00, 0x06, 0x12, 0x00, 0x00, 0x05	/* |........| */
	.byte	0x00, 0x05, 0x00, 0x05, 0x00, 0x05, 0x00, 0x05	/* |........| */
	.byte	0x00, 0x05, 0x00, 0x05, 0x00, 0x05, 0x00, 0x05	/* |........| */
	.byte	0x00, 0x05, 0x00, 0x00, 0x10, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x01, 0x00, 0x06, 0x00, 0x06, 0x00, 0x0b, 0x00	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x06, 0x00, 0x06, 0x00	/* |........| */
	.byte	0x06, 0x00, 0x06, 0x00, 0x00, 0x06, 0x06, 0x00	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x00, 0x06, 0x00, 0x06	/* |........| */
	.byte	0x00, 0x06, 0x00, 0x06, 0x06, 0x00	/* |......| */
	.byte	0x02, 0x00, 0x06, 0x00, 0x0d, 0x00, 0x00, 0x06	/* |........| */
D_1ed8:
	.byte	0xf6, 0x3e, 0xf8, 0x3e, 0xfd, 0x3e, 0x01, 0x3f	/* |.>.>.>.?| */
	.byte	0x05, 0x3f, 0x09, 0x3f, 0x0e, 0x3f, 0x11, 0x3f	/* |.?.?.?.?| */
	.byte	0x15, 0x3f	/* |.?| */
D_1eea:
	.ascii	"# %d \"%[^\"]\0"
	.byte	0x3f, 0x00	/* |?.| */
	.ascii	"norm\0"
	.byte	0x72, 0x65, 0x67, 0x00, 0x69, 0x6d, 0x6d, 0x00	/* |reg.imm.| */
	.byte	0x73, 0x78, 0x74, 0x00	/* |sxt.| */
	.ascii	"indr\0"
	.byte	0x69, 0x78, 0x00, 0x73, 0x69, 0x78, 0x00	/* |ix.six.| */
	.ascii	"string\0"
D_1f1c:
	.ascii	"operand %d: type=%s\0"
D_1f30:
	.ascii	" csect=%s sym=%s\0"
D_1f41:
	.byte	0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x3d, 0x25	/* | value=%| */
	.byte	0x6c, 0x64, 0x20, 0x72, 0x65, 0x67, 0x3d, 0x25	/* |ld reg=%| */
	.byte	0x64, 0x0a, 0x00	/* |d..| */
D_1f54:
	.byte	0x00, 0x00	/* |..| */
D_1f56:
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
	.byte	0xac, 0x4b, 0xb8, 0x4b, 0xc4, 0x4b, 0xe2, 0x4b	/* |.K.K.K.K| */
	.byte	0xe2, 0x4b, 0xe2, 0x4b, 0xe2, 0x4b, 0xe2, 0x4b	/* |.K.K.K.K| */
	.byte	0xe2, 0x4b, 0xe2, 0x4b, 0xe2, 0x4b, 0xe2, 0x4b	/* |.K.K.K.K| */
	.byte	0xe2, 0x4b, 0xe2, 0x4b, 0xe2, 0x4b, 0xe2, 0x4b	/* |.K.K.K.K| */
	.byte	0x70, 0x4b, 0x70, 0x4b, 0x8e, 0x4b	/* |pKpK.K| */
D_1ffc:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_2004:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_200c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_2014:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_201c:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_2024:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x14, 0x40	/* |.......@| */
	.byte	0x00	/* |.| */
D_202d:
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
D_20ae:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_20c2:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_20ca:
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
D_2119:
	.byte	0x00	/* |.| */
D_211a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_2122:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_212a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_2132:
	.byte	0x2c, 0x43, 0x1c, 0xeb, 0xe2, 0x36, 0x1a, 0x3f	/* |,C...6.?| */
D_213a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_2142:
	.byte	0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0xb9, 0x3f	/* |.......?| */
D_214a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_2152:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_215a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x8f, 0x40	/* |.....@.@| */
D_2162:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_216a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_2172:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_217a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_2182:
	.byte	0x24, 0x4b, 0x00, 0x00, 0x24, 0x4b, 0x01, 0x00	/* |$K..$K..| */
D_218a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_2190:
	.byte	0x02	/* |.| */
D_2191:
	.byte	0x01	/* |.| */
D_2192:
	.byte	0x00, 0x00	/* |..| */
D_2194:
	.byte	0x00, 0x00, 0x00, 0x00, 0x06, 0x02	/* |......| */
D_219a:
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
D_2222:
	.word	D_2222	/* self-ref ptr: 0x4222 */
D_2224:
	.byte	0x00, 0x00	/* |..| */
D_2226:
	.byte	0x00, 0x00	/* |..| */
D_2228:
	.byte	0x00, 0x00	/* |..| */
D_222a:
	.word	0x2d30	/* _curbrk: bss_end=0x2d2a rounded to paragraph */

	.bss

D_222c:	.space	2	/* bss word @ 0x222c */
D_222e:	.space	2	/* bss word @ 0x222e */
	.space	4
D_2234:	.space	2	/* bss word @ 0x2234 */
	.space	98
D_2298:	.space	2	/* bss word @ 0x2298 */
	.space	2
D_229c:	.space	2	/* bss word @ 0x229c */
	.space	98
D_2300:	.space	2	/* bss word @ 0x2300 */
	.space	2
D_2304:	.space	2	/* bss word @ 0x2304 */
	.space	610
D_2568:	.space	2	/* bss word @ 0x2568 */
D_256a:	.space	2	/* bss word @ 0x256a */
D_256c:	.space	2	/* bss word @ 0x256c */
D_256d:	.space	2	/* bss word @ 0x256d */
D_256e:	.space	2	/* bss word @ 0x256e */
D_256f:	.space	2	/* bss word @ 0x256f */
D_2570:	.space	2	/* bss word @ 0x2570 */
	.space	6
D_2578:	.space	2	/* bss word @ 0x2578 */
D_257a:	.space	2	/* bss word @ 0x257a */
D_257c:	.space	2	/* bss word @ 0x257c */
	.space	2
D_2580:	.space	2	/* bss word @ 0x2580 */
D_2582:	.space	2	/* bss word @ 0x2582 */
D_2584:	.space	2	/* bss word @ 0x2584 */
D_2586:	.space	2	/* bss word @ 0x2586 */
	.space	2
D_258a:	.space	2	/* bss word @ 0x258a */
D_258c:	.space	2	/* bss word @ 0x258c */
D_258e:	.space	2	/* bss word @ 0x258e */
	.space	140
D_261c:	.space	2	/* bss word @ 0x261c */
D_261e:	.space	2	/* bss word @ 0x261e */
	.space	4
D_2624:	.space	2	/* bss word @ 0x2624 */
	.space	2
D_2628:	.space	2	/* bss word @ 0x2628 */
D_262a:	.space	2	/* bss word @ 0x262a */
D_262c:	.space	2	/* bss word @ 0x262c */
D_262e:	.space	2	/* bss word @ 0x262e */
D_2630:	.space	2	/* bss word @ 0x2630 */
D_2632:	.space	2	/* bss word @ 0x2632 */
D_2634:	.space	2	/* bss word @ 0x2634 */
D_2636:	.space	2	/* bss word @ 0x2636 */
D_2638:	.space	2	/* bss word @ 0x2638 */
D_263a:	.space	2	/* bss word @ 0x263a */
D_263c:	.space	2	/* bss word @ 0x263c */
D_263e:	.space	2	/* bss word @ 0x263e */
D_2640:	.space	2	/* bss word @ 0x2640 */
D_2642:	.space	2	/* bss word @ 0x2642 */
D_2644:	.space	2	/* bss word @ 0x2644 */
D_2646:	.space	2	/* bss word @ 0x2646 */
D_2648:	.space	2	/* bss word @ 0x2648 */
D_264a:	.space	2	/* bss word @ 0x264a */
D_264c:	.space	2	/* bss word @ 0x264c */
D_264e:	.space	2	/* bss word @ 0x264e */
	.space	4
D_2654:	.space	2	/* bss word @ 0x2654 */
	.space	510
D_2854:	.space	2	/* bss word @ 0x2854 */
	.space	198
D_291c:	.space	2	/* bss word @ 0x291c */
	.space	6
D_2924:	.space	2	/* bss word @ 0x2924 */
	.space	1022
D_2d24:	.space	2	/* bss word @ 0x2d24 */
D_2d26:	.space	2	/* bss word @ 0x2d26 */
D_2d28:	.space	2	/* bss word @ 0x2d28 */

/* end of file */
