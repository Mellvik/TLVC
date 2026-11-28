/* dd.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 dd.s -o dd.o
 * Syscalls used:
 *   1:exit            3:read            4:write           5:open            6:close           17:brk
 *   19:lseek          41:dup            48:signal         54:ioctl
 * INT instructions: $0xf1  $0xf4
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: dd  [linked executable]
//   Magic:    0x0109  NMAGIC (separate I&D executable)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x00001d1e  (7454 bytes)
//   a_data:   0x0000083a  (2106 bytes)
//   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	/* vrewrite: initialize _environ from envp (TLVC crt0 3rd arg) */
	mov	0x8(%bp),%ax
	mov	%ax,D_083a
	push   %si
	push   %di
	sub    $0x4,%sp
	movw   $L_06ff,-0x6(%bp)
	mov    $0x1,%si
L_0046:
	cmp    0x4(%bp),%si
	jl   L_004e
	jmp   L_022e
L_004e:
	mov    %si,%ax
	shl    %ax
	add    0x6(%bp),%ax
	mov    %ax,%bx
	mov    (%bx),%ax
	mov    %ax,D_083e
	mov    $D_030a,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_0075
	mov    $0x7fff,%ax
	push   %ax
	call   L_059f
	pop    %cx
	mov    %ax,D_0006
	jmp   L_00a5
L_0075:
	mov    $D_030f,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_008e
	mov    $0x7fff,%ax
	push   %ax
	call   L_059f
	pop    %cx
	mov    %ax,D_0008
	jmp   L_00be
L_008e:
	mov    $D_0314,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_00a7
	mov    $0x7fff,%ax
	push   %ax
	call   L_059f
	pop    %cx
	mov    %ax,D_0844
L_00a5:
	jmp   L_00e6
L_00a7:
	mov    $D_0319,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_00c0
	mov    $0x7fff,%ax
	push   %ax
	call   L_059f
	pop    %cx
	mov    %ax,D_0856
L_00be:
	jmp   L_00ff
L_00c0:
	mov    $D_031d,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_00d4
	mov    D_083e,%ax
	mov    %ax,D_0842
	jmp   L_00ff
L_00d4:
	mov    $D_0321,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_00e8
	mov    D_083e,%ax
	mov    %ax,D_0852
L_00e6:
	jmp   L_0118
L_00e8:
	mov    $D_0325,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_0101
	mov    $0x7fff,%ax
	push   %ax
	call   L_059f
	pop    %cx
	mov    %ax,D_085e
L_00ff:
	jmp   L_0131
L_0101:
	mov    $D_032b,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_011a
	mov    $0x7fff,%ax
	push   %ax
	call   L_059f
	pop    %cx
	mov    %ax,D_0848
L_0118:
	jmp   L_014a
L_011a:
	mov    $D_0331,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_0133
	mov    $0x7fff,%ax
	push   %ax
	call   L_059f
	pop    %cx
	mov    %ax,D_0862
L_0131:
	jmp   L_014a
L_0133:
	mov    $D_0338,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_014d
	mov    $0x7fff,%ax
	push   %ax
	call   L_059f
	pop    %cx
	mov    %ax,D_0004
L_014a:
	jmp   L_022a
L_014d:
	mov    $D_033f,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	jne   L_015c
	jmp   L_0207
L_015c:
	mov    $D_0345,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	jne   L_015c
	mov    D_083e,%bx
	cmpb   $0x0,(%bx)
	jne   L_0174
	jmp   L_022a
L_0174:
	mov    $D_0347,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_0187
	movw   $L_0797,-0x6(%bp)
L_0185:
	jmp   L_015c
L_0187:
	mov    $D_034e,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_019a
	movw   $L_082a,-0x6(%bp)
L_0198:
	jmp   L_015c
L_019a:
	mov    $D_0352,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_01ad
	movw   $L_072d,-0x6(%bp)
L_01ab:
	jmp   L_0185
L_01ad:
	mov    $D_0358,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_01c1
	orw    $0x1,D_0854
L_01bf:
	jmp   L_0185
L_01c1:
	mov    $D_035e,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_01d5
	orw    $0x2,D_0854
L_01d3:
	jmp   L_0198
L_01d5:
	mov    $D_0364,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_01e9
	orw    $0x4,D_0854
	jmp   L_01ab
L_01e9:
	mov    $D_0369,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
	je   L_01fd
	orw    $0x8,D_0854
	jmp   L_01bf
L_01fd:
	mov    $D_0371,%ax
	push   %ax
	call   L_055b
	pop    %cx
	or     %ax,%ax
L_0207:
	je   L_0211
	orw    $0x10,D_0854
	jmp   L_01d3
L_0211:
	pushw  D_083e
	mov    $D_0376,%ax
	push   %ax
	mov    $D_07a0,%ax
	push   %ax
	call   L_0934
	add    $0x6,%sp
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
L_022a:
	inc    %si
	jmp   L_0046
L_022e:
	cmpw   $L_06ff,-0x6(%bp)
	jne   L_0242
	testw  $0x3,D_0854
	je   L_0242
	movw   $L_06c4,-0x6(%bp)
L_0242:
	cmpw   $0x0,D_0842
	je   L_025b
	xor    %ax,%ax
	push   %ax
	pushw  D_0842
	call   L_1c58
	add    $0x4,%sp
	mov    %ax,D_084c
	jmp   L_0265
L_025b:
	xor    %ax,%ax
	push   %ax
	call   L_091e
	pop    %cx
	mov    %ax,D_084c
L_0265:
	cmpw   $0x0,D_084c
	jge   L_0285
	pushw  D_0842
	mov    $D_0383,%ax
	push   %ax
	mov    $D_07a0,%ax
	push   %ax
	call   L_0934
	add    $0x6,%sp
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0285:
	cmpw   $0x0,D_0852
	je   L_029f
	mov    $0x2,%ax
	push   %ax
	pushw  D_0852
	call   L_1c58
	add    $0x4,%sp
	mov    %ax,D_0866
	jmp   L_02aa
L_029f:
	mov    $0x1,%ax
	push   %ax
	call   L_091e
	pop    %cx
	mov    %ax,D_0866
L_02aa:
	cmpw   $0x0,D_0866
	jge   L_02ca
	pushw  D_0852
	mov    $D_0394,%ax
	push   %ax
	mov    $D_07a0,%ax
	push   %ax
	call   L_0934
	add    $0x6,%sp
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
L_02ca:
	cmpw   $0x0,D_0856
	je   L_02e5
	mov    D_0856,%ax
	mov    %ax,D_0008
	mov    %ax,D_0006
	cmpw   $L_06ff,-0x6(%bp)
	jne   L_02e5
	incw   D_0860
L_02e5:
	cmpw   $0x0,D_0006
	je   L_02f3
	cmpw   $0x0,D_0008
	jne   L_0308
L_02f3:
	mov    $D_03a7,%ax
	push   %ax
	mov    $D_07a0,%ax
	push   %ax
	call   L_0934
	add    $0x4,%sp
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0308:
	pushw  D_0006
	call   L_1bf6
	pop    %cx
	mov    %ax,D_0850
	cmpw   $0x0,D_0860
	je   L_0322
	mov    D_0850,%ax
	mov    %ax,D_0868
	jmp   L_032d
L_0322:
	pushw  D_0008
	call   L_1bf6
	pop    %cx
	mov    %ax,D_0868
L_032d:
	mov    $0x40,%ax
	push   %ax
	call   L_1bf6
	pop    %cx
	cmpw   $0xffff,D_0850
	je   L_0343
	cmpw   $0xffff,D_0868
	jne   L_0358
L_0343:
	mov    $D_03bf,%ax
	push   %ax
	mov    $D_07a0,%ax
	push   %ax
	call   L_0934
	add    $0x4,%sp
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
L_0358:
	movw   $0x0,D_084a
	movw   $0x0,D_0864
	movw   $0x0,D_0840
	mov    D_0868,%ax
	mov    %ax,D_083c
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_09ec
	add    $0x4,%sp
	cmp    $0x1,%ax
	je   L_0391
	mov    $0x8bd,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_09ec
	add    $0x4,%sp
L_0391:
	cmpw   $0x0,D_085e
	je   L_03b2
	mov    $0x1,%ax
	push   %ax
	mov    D_0006,%ax
	cwtd
	push   %dx
	push   %ax
	pushw  D_084c
	call   L_0b0c
	add    $0x8,%sp
	decw   D_085e
	jmp   L_0391
L_03b2:
	cmpw   $0x0,D_0848
	je   L_03d3
	mov    $0x1,%ax
	push   %ax
	mov    D_0008,%ax
	cwtd
	push   %dx
	push   %ax
	pushw  D_0866
	call   L_0b0c
	add    $0x8,%sp
	decw   D_0848
	jmp   L_03b2
L_03d3:
	mov    D_084a,%ax
	decw   D_084a
	or     %ax,%ax
	je   L_03e1
	jmp   L_04f7
L_03e1:
	movw   $0x0,D_084a
	cmpw   $0x0,D_0862
	je   L_03fb
	mov    D_0846,%ax
	add    D_085c,%ax
	cmp    D_0862,%ax
	je   L_042d
L_03fb:
	testw  $0x18,D_0854
	je   L_0418
	mov    D_0850,%ax
	add    D_0006,%ax
	mov    %ax,%di
L_040c:
	cmp    D_0850,%di
	jbe   L_0418
	dec    %di
	movb   $0x0,(%di)
	jmp   L_040c
L_0418:
	pushw  D_0006
	pushw  D_0850
	pushw  D_084c
	call   L_1c6e
	add    $0x6,%sp
	mov    %ax,D_084a
L_042d:
	cmpw   $0xffff,D_084a
	jne   L_046e
	mov    $D_03d2,%ax
	push   %ax
	call   L_0960
	pop    %cx
	testw  $0x8,D_0854
	jne   L_044a
	call   L_050d
	call   L_08bd
L_044a:
	movw   $0x0,D_084a
	xor    %si,%si
L_0452:
	cmp    D_0006,%si
	jge   L_046b
	mov    D_0850,%ax
	add    %si,%ax
	mov    %ax,%bx
	cmpb   $0x0,(%bx)
	je   L_0468
	mov    %si,D_084a
L_0468:
	inc    %si
	jmp   L_0452
L_046b:
	call   L_08d0
L_046e:
	cmpw   $0x0,D_084a
	jne   L_0481
	decw   D_0004
	jg   L_0481
	call   L_050d
	call   L_08bd
L_0481:
	mov    D_084a,%ax
	cmp    D_0006,%ax
	je   L_049e
	incw   D_085c
	testw  $0x10,D_0854
	je   L_04a2
	mov    D_0006,%ax
	mov    %ax,D_084a
	jmp   L_04a2
L_049e:
	incw   D_0846
L_04a2:
	mov    D_0850,%di
	mov    D_084a,%ax
	sar    %ax
	and    $0xfffe,%ax
	mov    %ax,%si
	testw  $0x4,D_0854
	je   L_04d7
	or     %si,%si
	je   L_04d7
L_04bc:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,-0x8(%bp)
	mov    (%di),%al
	mov    %al,-0x1(%di)
	mov    %di,%bx
	inc    %di
	mov    -0x8(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	dec    %si
	jne   L_04bc
L_04d7:
	mov    D_0850,%di
	cmpw   $0x0,D_0860
	jne   L_04e5
	jmp   L_03d3
L_04e5:
	mov    D_084a,%ax
	mov    %ax,D_0864
	call   L_050d
	movw   $0x0,D_084a
	jmp   L_03d3
L_04f7:
	xor    %si,%si
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	or     %ax,%si
	and    $0xff,%si
	push   %si
	/* TODO: indirect call - verify */
	call   *-0x6(%bp)
	pop    %cx
	jmp   L_03d3


L_050d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	cmpw   $0x0,D_0864
	je   L_0557
	mov    D_0864,%ax
	cmp    D_0008,%ax
	jne   L_0528
	incw   D_0858
	jmp   L_052c
L_0528:
	incw   D_086a
L_052c:
	pushw  D_0864
	pushw  D_0868
	pushw  D_0866
	call   L_1cd4
	add    $0x6,%sp
	mov    %ax,%di
	cmp    D_0864,%di
	je   L_0551
	mov    $D_03d7,%ax
	push   %ax
	call   L_0960
	pop    %cx
	call   L_08bd
L_0551:
	movw   $0x0,D_0864
L_0557:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_055b:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_083e,%di
L_0564:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    0x4(%bp),%bx
	mov    (%bx),%dl
	mov    $0x8,%cx
	shl    %cl,%dx
	sar    %cl,%dx
	cmp    %dx,%ax
	jne   L_0587
	mov    0x4(%bp),%bx
	incw   0x4(%bp)
	cmpb   $0x0,(%bx)
	jne   L_0564
	jmp   L_0593
L_0587:
	mov    0x4(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0593
	xor    %ax,%ax
	jmp   L_059b
L_0593:
	dec    %di
	mov    %di,D_083e
	mov    $0x1,%ax
L_059b:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_059f:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x4,%sp
	mov    D_083e,%di
	movw   $0x0,-0x8(%bp)
	movw   $0x0,-0x6(%bp)
L_05b5:
	cmpb   $0x30,(%di)
	jl   L_05f8
	cmpb   $0x39,(%di)
	jg   L_05f8
	mov    $0xa,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	call   L_111f
	add    $0x8,%sp
	mov    %di,%bx
	inc    %di
	mov    (%bx),%bl
	mov    $0x8,%cx
	shl    %cl,%bx
	sar    %cl,%bx
	xor    %si,%si
	or     %bx,%bx
	jge   L_05e6
	not    %si
L_05e6:
	add    %bx,%ax
	adc    %si,%dx
	sub    $0x30,%ax
	sbb    $0x0,%dx
	mov    %ax,-0x8(%bp)
	mov    %dx,-0x6(%bp)
	jmp   L_05b5
L_05f8:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_0695
L_0601:
	mov    $0x400,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_111c
	add    $0x6,%sp
	jmp   L_05f8
L_0614:
	mov    $0x2,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_111c
	add    $0x6,%sp
	jmp   L_05f8
L_0627:
	mov    $0x200,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_111c
	add    $0x6,%sp
	jmp   L_05f8
L_063a:
	mov    %di,D_083e
	mov    $0x7fff,%ax
	push   %ax
	call   L_059f
	pop    %cx
	cwtd
	push   %dx
	push   %ax
	lea    -0x8(%bp),%ax
	push   %ax
	call   L_111c
	add    $0x6,%sp
L_0653:
	mov    0x4(%bp),%ax
	cwtd
	cmp    %dx,-0x6(%bp)
	jg   L_0671
	jl   L_0663
	cmp    %ax,-0x8(%bp)
	jae   L_0671
L_0663:
	cmpw   $0x0,-0x6(%bp)
	jg   L_068d
	jl   L_0671
	cmpw   $0x0,-0x8(%bp)
	jae   L_068d
L_0671:
	pushw  -0x6(%bp)
	pushw  -0x8(%bp)
	mov    $D_03dd,%ax
	push   %ax
	mov    $D_07a0,%ax
	push   %ax
	call   L_0934
	add    $0x8,%sp
	mov    $0x1,%ax
	push   %ax
	call   _exit
	pop    %cx
L_068d:
	mov    -0x8(%bp),%ax
	mov    -0x6(%bp),%dx
	jmp   L_06bd
L_0695:
	cmp    $0x62,%ax
	je   L_0627
	jg   L_06a5
	or     %ax,%ax
	je   L_0653
	cmp    $0x2a,%ax
L_06a3:
	je   L_063a
L_06a5:
	cmp    $0x6b,%ax
	jne   L_06ad
	jmp   L_0601
L_06ad:
	cmp    $0x77,%ax
	jne   L_06b5
	jmp   L_0614
L_06b5:
	cmp    $0x78,%ax
	je   L_06a3
	jmp   L_05f8
L_06bd:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_06c4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	testw  $0x2,D_0854
	je   L_06e1
	cmp    $0x61,%di
	jl   L_06e1
	cmp    $0x7a,%di
	jg   L_06e1
	add    $0xffe0,%di
L_06e1:
	testw  $0x1,D_0854
	je   L_06f6
	cmp    $0x41,%di
	jl   L_06f6
	cmp    $0x5a,%di
	jg   L_06f6
	add    $0x20,%di
L_06f6:
	push   %di
	call   L_06ff
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_06ff:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    D_083c,%bx
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   D_083c
	incw   D_0864
	mov    D_0864,%ax
	cmp    D_0008,%ax
	jl   L_0729
	call   L_050d
	mov    D_0868,%ax
	mov    %ax,D_083c
L_0729:
	pop    %di
	pop    %si
	pop    %bp
	ret
L_072d:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%bx
	mov    D_000a(%bx),%al
	cbtw
	and    $0xff,%ax
	mov    %ax,%di
	cmpw   $0x0,D_0844
	jne   L_074d
	push   %di
	call   L_06c4
	pop    %cx
	jmp   L_0793
L_074d:
	cmp    $0x20,%di
	jne   L_0758
	incw   D_084e
	jmp   L_0772
L_0758:
	cmpw   $0x0,D_084e
	jle   L_076d
	mov    $0x20,%ax
	push   %ax
	call   L_06ff
	pop    %cx
	decw   D_084e
	jmp   L_0758
L_076d:
	push   %di
	call   L_06c4
	pop    %cx
L_0772:
	incw   D_0840
	mov    D_0840,%ax
	cmp    D_0844,%ax
	jl   L_0793
	mov    $0xa,%ax
	push   %ax
	call   L_06ff
	pop    %cx
	movw   $0x0,D_0840
	movw   $0x0,D_084e
L_0793:
	pop    %di
	pop    %si
	pop    %bp
	ret
L_0797:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	testw  $0x2,D_0854
	je   L_07b4
	cmp    $0x61,%di
	jl   L_07b4
	cmp    $0x7a,%di
	jg   L_07b4
	add    $0xffe0,%di
L_07b4:
	testw  $0x1,D_0854
	je   L_07c9
	cmp    $0x41,%di
	jl   L_07c9
	cmp    $0x5a,%di
	jg   L_07c9
	add    $0x20,%di
L_07c9:
	mov    D_010a(%di),%al
	cbtw
	and    $0xff,%ax
	mov    %ax,%di
	cmpw   $0x0,D_0844
	jne   L_07e1
	push   %di
	call   L_06ff
	pop    %cx
	jmp   L_0826
L_07e1:
	cmpw   $0xa,0x4(%bp)
	jne   L_0807
L_07e7:
	mov    D_0840,%ax
	cmp    D_0844,%ax
	jge   L_07ff
	mov    D_012a,%al
	cbtw
	push   %ax
	call   L_06ff
	pop    %cx
	incw   D_0840
	jmp   L_07e7
L_07ff:
	movw   $0x0,D_0840
	jmp   L_0826
L_0807:
	mov    D_0840,%ax
	cmp    D_0844,%ax
	jne   L_0814
	incw   D_085a
L_0814:
	incw   D_0840
	mov    D_0840,%ax
	cmp    D_0844,%ax
	jg   L_0826
	push   %di
	call   L_06ff
	pop    %cx
L_0826:
	pop    %di
	pop    %si
	pop    %bp
	ret
L_082a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	testw  $0x2,D_0854
	je   L_0847
	cmp    $0x61,%di
	jl   L_0847
	cmp    $0x7a,%di
	jg   L_0847
	add    $0xffe0,%di
L_0847:
	testw  $0x1,D_0854
	je   L_085c
	cmp    $0x41,%di
	jl   L_085c
	cmp    $0x5a,%di
	jg   L_085c
	add    $0x20,%di
L_085c:
	mov    D_020a(%di),%al
	cbtw
	and    $0xff,%ax
	mov    %ax,%di
	cmpw   $0x0,D_0844
	jne   L_0874
	push   %di
	call   L_06ff
	pop    %cx
	jmp   L_08b9
L_0874:
	cmpw   $0xa,0x4(%bp)
	jne   L_089a
L_087a:
	mov    D_0840,%ax
	cmp    D_0844,%ax
	jge   L_0892
	mov    D_022a,%al
	cbtw
	push   %ax
	call   L_06ff
	pop    %cx
	incw   D_0840
	jmp   L_087a
L_0892:
	movw   $0x0,D_0840
	jmp   L_08b9
L_089a:
	mov    D_0840,%ax
	cmp    D_0844,%ax
	jne   L_08a7
	incw   D_085a
L_08a7:
	incw   D_0840
	mov    D_0840,%ax
	cmp    D_0844,%ax
	jg   L_08b9
	push   %di
	call   L_06ff
	pop    %cx
L_08b9:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_08bd:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	call   L_08d0
	xor    %ax,%ax
	push   %ax
	call   _exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_08d0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  D_085c
	pushw  D_0846
	mov    $D_03fb,%ax
	push   %ax
	mov    $D_07a0,%ax
	push   %ax
	call   L_0934
	add    $0x8,%sp
	pushw  D_086a
	pushw  D_0858
	mov    $D_040d,%ax
	push   %ax
	mov    $D_07a0,%ax
	push   %ax
	call   L_0934
	add    $0x8,%sp
	cmpw   $0x0,D_085a
	je   L_091a
	pushw  D_085a
	mov    $D_0420,%ax
	push   %ax
	mov    $D_07a0,%ax
	push   %ax
	call   L_0934
	add    $0x6,%sp
L_091a:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_091e:
	push   %bp
	mov    %sp,%bp
	mov	$41,%bx	/* sc: dup */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 41:dup */
	int	$0xf1
	jcxz   L_0932
	mov    %cx,D_086c
L_0932:
	pop    %bp
	ret


L_0934:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   L_0b2a
	add    $0x6,%sp
	mov    0x4(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x20,%ax
	je   L_095a
	mov    $0xffff,%ax
	jmp   L_095c
L_095a:
	xor    %ax,%ax
L_095c:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0960:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0436,%di
	mov    D_086c,%ax
	cmp    D_0490,%ax
	jge   L_097b
	mov    D_086c,%bx
	shl    %bx
	mov    D_044a(%bx),%di
L_097b:
	pushw  0x4(%bp)
	call   L_09d0
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_09a8
	push   %si
	pushw  0x4(%bp)
	mov    $0x2,%ax
	push   %ax
	call   L_1cd4
	add    $0x6,%sp
	mov    $0x2,%ax
	push   %ax
	mov    $D_0444,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_1cd4
	add    $0x6,%sp
L_09a8:
	push   %di
	call   L_09d0
	pop    %cx
	push   %ax
	push   %di
	mov    $0x2,%ax
	push   %ax
	call   L_1cd4
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	mov    $D_0447,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   L_1cd4
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_09d0:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_09db:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_09e6
	inc    %si
	jmp   L_09db
L_09e6:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


L_09ec:
	push   %bp
	mov    %sp,%bp
	mov    $0x16,%cx
	mov    0x4(%bp),%ax
	or     %ax,%ax
	jle   L_0a2f
	cmp    $0x20,%ax
	jg   L_0a2f
	mov    %ax,%cx
	shl    %ax
	add    $D_0872,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    0x6(%bp),%ax
	mov    %ax,(%bx)
	cmp    $0x2,%ax
	jl   L_0a1b
	mov    $0x6,%ax
	mul    %cx
	add    $(L_0a38-6),%ax
L_0a1b:
	mov    %ax,%dx
	mov    %cx,%ax
	mov	$48,%bx	/* sc: signal */
	/* v7: 48:signal */
	int	$0xf1
	cmp    $0x2,%ax
	jl   L_0a2c
	pop    %ax
	jmp   L_0a2d
L_0a2c:
	pop    %bx
L_0a2d:
	jcxz   L_0a36
L_0a2f:
	mov    %cx,D_086c
	mov    $0xffff,%ax
L_0a36:
	pop    %bp
	ret
L_0a38:
	push   %ax
	mov    $D_0874,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_0876,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_0878,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_087a,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_087c,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_087e,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_0880,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_0882,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_0884,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_0886,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_0888,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_088a,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_088c,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_088e,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_0890,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_0892,%ax
	jmp   L_0a9c
	push   %ax
	mov    $D_0894,%ax
L_0a9c:
	jmp   L_0af6
	push   %ax
	mov    $D_0896,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_0898,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_089a,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_089c,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_089e,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_08a0,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_08a2,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_08a4,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_08a6,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_08a8,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_08aa,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_08ac,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_08ae,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_08b0,%ax
	jmp   L_0af6
	push   %ax
	mov    $D_08b2,%ax
L_0af6:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	sub    $D_0872,%ax
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


L_0b0c:
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
	jcxz   L_0b27
	mov    %cx,D_086c
L_0b27:
	pop    %si
	pop    %bp
	ret


L_0b2a:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0b42:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_0b54
	jmp   L_10f1
L_0b54:
	cmpb   $0x25,-0x5(%bp)
	je   L_0b67
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_0b65:
	jmp   L_0b42
L_0b67:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_0b92
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1702
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0b92:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_0bab
	incb   -0xa6(%bp)
	inc    %di
L_0bab:
	cmpb   $0x30,(%di)
	jne   L_0bb6
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_0bb6:
	cmpb   $0x2a,(%di)
	je   L_0be6
	movw   $0x0,-0xe(%bp)
L_0bc0:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0bfc
	cmpw   $0x9,-0xc(%bp)
	jg   L_0bfc
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_0bc0
L_0be6:
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
L_0bfc:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_0c43
	cmpb   $0x2a,(%di)
	je   L_0c45
	movw   $0x0,-0x10(%bp)
L_0c15:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_0c3f
	cmpw   $0x9,-0xc(%bp)
	jg   L_0c3f
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_0c15
L_0c3f:
	addw   $0x30,-0xc(%bp)
L_0c43:
	jmp   L_0c5c
L_0c45:
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
L_0c5c:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_1066
L_0c67:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_0c78
L_0c6f:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_0ca8
	jmp   L_0ca1
L_0c78:
	cmp    $0x64,%ax
	je   L_0caa
	cmp    $0x6f,%ax
	je   L_0c8e
	cmp    $0x75,%ax
	je   L_0ca1
	cmp    $0x78,%ax
	je   L_0c9c
	jmp   L_0c6f
L_0c8e:
	movw   $0x8,-0x12(%bp)
	jmp   L_0ca1
L_0c95:
	movw   $0x8,-0x12(%bp)
	jmp   L_0ca8
L_0c9c:
	movw   $0x10,-0x12(%bp)
L_0ca1:
	jmp   L_0cf3
L_0ca3:
	movw   $0x10,-0x12(%bp)
L_0ca8:
	jmp   L_0d03
L_0caa:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0cc7
L_0cba:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0cc7:
	cmpw   $0x0,-0x14(%bp)
	jg   L_0d11
	jl   L_0cd5
	cmpw   $0x0,-0x16(%bp)
	jae   L_0d11
L_0cd5:
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
	jmp   L_0d11
L_0cf3:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_0d11
L_0d03:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_0d11:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0d1b
	cmpw   $0x0,-0x16(%bp)
L_0d1b:
	jne   L_0d2c
	cmpw   $0x0,-0x10(%bp)
	jne   L_0d2c
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_0d2c:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0d33:
	cmpw   $0x0,-0x14(%bp)
	jne   L_0d3d
	cmpw   $0x0,-0x16(%bp)
L_0d3d:
	je   L_0d88
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   L_1250
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_0d5e
	mov    $0x30,%ax
	jmp   L_0d61
L_0d5e:
	mov    $0x57,%ax
L_0d61:
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
	call   L_1148
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_0d33
L_0d88:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_0dd7
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_0d88
L_0da6:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_0db2:
	cmpw   $0x2,-0xc(%bp)
	jge   L_0dd7
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0dd2
	decw   -0x8(%bp)
L_0dd2:
	incw   -0xc(%bp)
	jmp   L_0db2
L_0dd7:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_0dde:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_0df5
	movw   $0x0,-0xc(%bp)
L_0df5:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_0e04
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_0e04:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   L_1702
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0e27:
	jmp   L_0b65
L_0e2a:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_0e3c
	movw   $D_06e0,-0xa(%bp)
L_0e3c:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_0e4b
	movw   $0x7fff,-0xc(%bp)
L_0e4b:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0e51:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_0dde
	decw   -0xc(%bp)
	jl   L_0dde
	incw   -0x8(%bp)
	jmp   L_0e51
L_0e63:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_0e6b:
	jmp   L_0e27
L_0e6d:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0e79
	movw   $0x6,-0x10(%bp)
L_0e79:
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
	call   L_144e
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0eae
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0eae:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_0ec3
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_0edb
L_0ec3:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_0ec3
L_0edb:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_0eee
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_0eee:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_0f0d
L_0efa:
	decw   -0xc(%bp)
	jl   L_0f0d
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_0efa
L_0f0d:
	cmpw   $0x0,-0xc(%bp)
	jg   L_0f16
	jmp   L_0dd7
L_0f16:
	decw   -0xc(%bp)
	jge   L_0f1e
	jmp   L_0dd7
L_0f1e:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_0f16
L_0f33:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_0f41
	movw   $0x6,-0x10(%bp)
	jmp   L_0f44
L_0f41:
	incw   -0x10(%bp)
L_0f44:
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
	call   L_1422
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_0f79
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_0f79:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_0f84
	incw   -0x1a(%bp)
L_0f84:
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
L_0fa6:
	decw   -0xc(%bp)
	jle   L_0fc0
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_0fa6
L_0fc0:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_0fdd
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_0fee
L_0fdd:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_0fee:
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
	jmp   L_0dd7
L_101f:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_102b
	movw   $0x6,-0x10(%bp)
L_102b:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_12a6
	add    $0xc,%sp
	add    $0x8,%si
L_1049:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_1049
	decw   -0x8(%bp)
	jmp   L_0dd7
L_105a:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_1063:
	jmp   L_0e6b
L_1066:
	cmp    $0x65,%ax
	jne   L_106e
	jmp   L_0f33
L_106e:
	jg   L_10af
	cmp    $0x4f,%ax
	jne   L_1078
	jmp   L_0c8e
L_1078:
	jg   L_108f
	cmp    $0x25,%ax
	je   L_105a
	cmp    $0x44,%ax
	jne   L_1087
	jmp   L_0caa
L_1087:
	cmp    $0x4c,%ax
L_108a:
	jne   L_108f
	jmp   L_0c67
L_108f:
	cmp    $0x55,%ax
	jne   L_1097
	jmp   L_0cf3
L_1097:
	cmp    $0x58,%ax
	jne   L_109f
	jmp   L_0c9c
L_109f:
	cmp    $0x63,%ax
	jne   L_10a7
	jmp   L_0da6
L_10a7:
	cmp    $0x64,%ax
	jne   L_10af
	jmp   L_0cba
L_10af:
	cmp    $0x6f,%ax
	jne   L_10b7
	jmp   L_0c95
L_10b7:
	jg   L_10ce
	cmp    $0x66,%ax
	jne   L_10c1
	jmp   L_0e6d
L_10c1:
	cmp    $0x67,%ax
	jne   L_10c9
	jmp   L_101f
L_10c9:
	cmp    $0x6c,%ax
	je   L_108a
L_10ce:
	cmp    $0x72,%ax
	jne   L_10d6
	jmp   L_0e63
L_10d6:
	cmp    $0x73,%ax
	jne   L_10de
	jmp   L_0e2a
L_10de:
	cmp    $0x75,%ax
	jne   L_10e6
	jmp   L_0d03
L_10e6:
	cmp    $0x78,%ax
	jne   L_10ee
	jmp   L_0ca3
L_10ee:
	jmp   L_1063
L_10f1:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_1115
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   L_1702
	add    $0x8,%sp
L_1115:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_111c:
	call   L_1284


L_111f:
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
	call   L_1284


L_1148:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_1166
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_1166:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_116f:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_116f
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_117f:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_117f
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_111f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_11c0
	jne   L_11b5
	or     $0x0,%ax
	je   L_11c0
L_11b5:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_11c0:
	pop    %ax
	mov    $0x0,%dx
	ret
	call   L_1284


L_11c8:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_11e4
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_11e4:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_11fb
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_11fb:
	push   %dx
	push   %ax
	call   L_1148
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_1215
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_1215:
	ret
	call   L_1284
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_11c8
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_111f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret
	call   L_1284


L_1250:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   L_1148
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   L_111f
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


L_1284:
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


L_12a6:
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
	call   L_1422
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_12e0
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_12e0:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_12e7:
	cmpw   $0x0,-0xa(%bp)
	jle   L_1301
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_1301
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_12e7
L_1301:
	cmpw   $0x0,-0x8(%bp)
	jl   L_1312
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_131e
L_1312:
	cmpw   $0x0,-0x8(%bp)
	jge   L_131c
	cmpw   $0xfffd,-0x8(%bp)
L_131c:
	jge   L_139d
L_131e:
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
L_1339:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_1353
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_1339
L_1353:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_136f
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_1375
L_136f:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_1375:
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
	jmp   L_140d
L_139d:
	cmpw   $0x0,-0x8(%bp)
	jg   L_13bf
	cmpb   $0x30,(%di)
	je   L_13ae
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_13ae:
	cmpw   $0x0,-0x8(%bp)
	jge   L_13bf
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_13ae
L_13bf:
	movw   $0x1,-0xa(%bp)
L_13c4:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_13ec
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
	jne   L_13e7
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_13e7:
	incw   -0xa(%bp)
	jmp   L_13c4
L_13ec:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_140d
L_13f4:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_1407
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_13f4
L_1407:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_140d:
	cmpb   $0x2e,-0x1(%si)
	jne   L_1414
	dec    %si
L_1414:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1422:
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
	call   L_1479
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_144e:
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
	call   L_1479
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1479:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x2c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_148c
	movw   $0x0,0xc(%bp)
L_148c:
	cmpw   $0x4f,0xc(%bp)
	jl   L_1497
	movw   $L_004e,0xc(%bp)
L_1497:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	mov    $D_06e8,%si
	int    $0xf4
	fldl   0x4(%bp)
	fcompl D_0738
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jae   L_14d4
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	int    $0xf4
	fldl   0x4(%bp)
	fchs
	fstl   -0x1e(%bp)
	fstpl  0x4(%bp)
L_14d4:
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_1cee
	int    $0xf4
	fstpl  -0x1e(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x1e(%bp)
	fstpl  0x4(%bp)
	movw   $D_0738,-0x16(%bp)
	int    $0xf4
	fldl   -0xc(%bp)
	fcompl D_0740
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_151f
	jmp   L_15cc
L_151f:
	movw   $D_0738,-0x16(%bp)
L_1524:
	int    $0xf4
	fldl   -0xc(%bp)
	fcompl D_0748
	fstsw  -0x18(%bp)
	fwait
	xchg   %ah,-0x17(%bp)
	sahf
	xchg   %ah,-0x17(%bp)
	jne   L_1540
	jmp   L_15ca
L_1540:
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   -0xc(%bp)
	fdivl  D_0750
	fstl   -0x1e(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_1cee
	int    $0xf4
	fstpl  -0x26(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x26(%bp)
	fstpl  -0x14(%bp)
	fldl   D_0758
	fstl   -0x1e(%bp)
	fdivl  D_0760
	fstpl  -0x1e(%bp)
	fldl   -0x14(%bp)
	faddl  -0x1e(%bp)
	fstl   -0x26(%bp)
	fmull  D_0768
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
	jmp   L_1524
L_15b3:
	cmpw   $D_0738,-0x16(%bp)
	jae   L_160d
	mov    -0x16(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x16(%bp)
L_15ca:
	jmp   L_15b3
L_15cc:
	int    $0xf4
	fldl   0x4(%bp)
	fcompl D_0770
	fstsw  -0x20(%bp)
	fwait
	xchg   %ah,-0x1f(%bp)
	sahf
	xchg   %ah,-0x1f(%bp)
	jbe   L_161c
L_15e5:
	int    $0xf4
	fldl   0x4(%bp)
	fmull  D_0778
	fstl   -0x1e(%bp)
	fstl   -0x14(%bp)
	fstl   -0x26(%bp)
	fcompl D_0780
	fstsw  -0x28(%bp)
	fwait
	xchg   %ah,-0x27(%bp)
	sahf
	xchg   %ah,-0x27(%bp)
L_160d:
	jae   L_161c
	int    $0xf4
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_15e5
L_161c:
	mov    0xc(%bp),%ax
	add    $D_06e8,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_162e
	add    %di,-0x16(%bp)
L_162e:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	cmpw   $D_06e8,-0x16(%bp)
	jae   L_1644
	movb   $0x0,D_06e8
	mov    $D_06e8,%ax
	jmp   L_16af
L_1644:
	cmp    -0x16(%bp),%si
	ja   L_16a0
	cmp    $D_0738,%si
	jae   L_16a0
	int    $0xf4
	fldl   0x4(%bp)
	fmull  D_0788
	fstpl  0x4(%bp)
	lea    -0x14(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   L_1cee
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
	jmp   L_1644
L_16a0:
	cmpw   $D_0738,-0x16(%bp)
	jb   L_16b1
	movb   $0x0,D_0737
	mov    $D_06e8,%ax
L_16af:
	jmp   L_16fb
L_16b1:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_16b9:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_16f5
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	cmpw   $D_06e8,-0x16(%bp)
	jbe   L_16d8
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_16b9
L_16d8:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_16b9
	cmp    $D_06e8,%si
	jbe   L_16f2
	movb   $0x30,(%si)
L_16f2:
	inc    %si
	jmp   L_16b9
L_16f5:
	movb   $0x0,(%si)
	mov    $D_06e8,%ax
L_16fb:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1702:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_170f:
	cmpw   $0x0,0x8(%bp)
	jl   L_1718
	jmp   L_17a6
L_1718:
	cmpb   $0x2d,(%di)
	jne   L_1755
	cmpw   $0x30,0xc(%bp)
	jne   L_1755
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1744
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
	jmp   L_1754
L_1744:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_17f4
	add    $0x4,%sp
L_1754:
	dec    %si
L_1755:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1773
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
	jmp   L_177f
L_1773:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_17f4
	add    $0x4,%sp
L_177f:
	incw   0x8(%bp)
	jmp   L_170f
L_1784:
	dec    %si
	jl   L_17ba
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_17a8
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
L_17a6:
	jmp   L_1784
L_17a8:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   L_17f4
	add    $0x4,%sp
	jmp   L_1784
L_17ba:
	cmpw   $0x0,0x8(%bp)
	je   L_17ef
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_17de
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
	jmp   L_17ea
L_17de:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   L_17f4
	add    $0x4,%sp
L_17ea:
	decw   0x8(%bp)
	jmp   L_17ba
L_17ef:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_17f4:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_180f
	mov    $0xffff,%ax
	jmp   L_18d6
L_180f:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1842
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
	call   L_1cd4
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_18c7
L_1842:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1894
	cmp    $D_0798,%di
	jne   L_186d
	mov    D_079f,%al
	cbtw
	push   %ax
	call   L_1c30
	pop    %cx
	or     %ax,%ax
	je   L_1862
	orb    $0x4,0x6(%di)
	jmp   L_180f
L_1862:
	movw   $D_08ba,0x4(%di)
	movw   $D_08ba,(%di)
	jmp   L_180f
L_186d:
	mov    $0x200,%ax
	push   %ax
	call   L_19c8
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1884
	orb    $0x4,0x6(%di)
	jmp   L_180f
L_1884:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_18b6
L_1894:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_18b6
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1cd4
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_18b6:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_18c7:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_18d8
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_18d6:
	jmp   L_18db
L_18d8:
	mov    0x4(%bp),%ax
L_18db:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_18e2:
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
	jne   L_192e
	mov    0x4(%di),%si
	or     %si,%si
	je   L_192e
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_192e
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1cd4
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_192e
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_1930
L_192e:
	xor    %ax,%ax
L_1930:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1935:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    $D_0790,%di
L_193d:
	cmp    D_0830,%di
	jae   L_194d
	push   %di
	call   L_1951
	pop    %cx
	add    $0x8,%di
	jmp   L_193d
L_194d:
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1951:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_19a4
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_19a4
	push   %di
	call   L_18e2
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   L_1c1c
	pop    %cx
	or     %ax,%ax
	jge   L_1986
	mov    $0xffff,%si
L_1986:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_1996
	pushw  0x4(%di)
	call   L_1b12
	pop    %cx
L_1996:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_19a4
	movw   $0x0,0x4(%di)
L_19a4:
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
	call   L_1935
	pushw  0x4(%bp)
	call   L_1d0a
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_19c8:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	cmpw   $0x0,D_0832
	jne   L_19f3
	mov    $D_0834,%ax
	or     $0x1,%ax
	mov    %ax,D_0832
	mov    $D_0832,%ax
	or     $0x1,%ax
	mov    %ax,D_0834
	/* dual-reloc split: movw $D_0834,D_0cbc */
	mov	$D_0834,%ax
	mov	%ax,D_0cbc
	/* dual-reloc split: movw $D_0832,D_0cba */
	mov	$D_0832,%ax
	mov	%ax,D_0cba
L_19f3:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	mov    D_0cba,%di
L_1a07:
	movw   $0x0,D_0836
L_1a0d:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_1a3c
L_1a14:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_1a23
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_1a14
L_1a23:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_1a3c
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_1a3c
	jmp   L_1ae3
L_1a3c:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_1a0d
	cmp    D_0cbc,%si
	jne   L_1a55
	cmp    $D_0832,%di
	je   L_1a59
L_1a55:
	xor    %ax,%ax
	jmp   L_1a91
L_1a59:
	incw   D_0836
	cmpw   $0x1,D_0836
	jle   L_1a0d
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	mov    %ax,D_0836
	xor    %ax,%ax
	push   %ax
	call   L_1bf6
	pop    %cx
	mov    %ax,%si
	mov    D_0836,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_1a93
	xor    %ax,%ax
L_1a91:
	jmp   L_1aa6
L_1a93:
	mov    D_0836,%ax
	shl    %ax
	push   %ax
	call   L_1bf6
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_1aa8
	xor    %ax,%ax
L_1aa6:
	jmp   L_1b0d
L_1aa8:
	mov    D_0cbc,%bx
	mov    %si,(%bx)
	mov    D_0cbc,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_1ac6
	mov    D_0cbc,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	mov    D_0cbc,%bx
	mov    %ax,(%bx)
L_1ac6:
	mov    D_0836,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	mov    %ax,D_0cbc
	mov    $D_0832,%ax
	or     $0x1,%ax
	mov    D_0cbc,%bx
	mov    %ax,(%bx)
	jmp   L_1a07
L_1ae3:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	mov    %ax,D_0cba
	cmp    %ax,%si
	jbe   L_1b02
	mov    D_0cba,%bx
	mov    (%bx),%ax
	mov    %ax,D_0cbe
	mov    D_0cba,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_1b02:
	mov    D_0cba,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_1b0d:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1b12:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	mov    %si,D_0cba
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
	je   L_1b47
	push   %di
	call   L_1b12
	pop    %cx
L_1b47:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   L_19c8
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_1b66
	cmp    %di,%si
	jne   L_1b6a
L_1b66:
	mov    %si,%ax
	jmp   L_1bd2
L_1b6a:
	mov    %di,-0x6(%bp)
	mov    %si,-0x8(%bp)
	mov    0x6(%bp),%ax
	inc    %ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0xa(%bp)
	cmp    -0xc(%bp),%ax
	jae   L_1b89
	mov    -0xa(%bp),%ax
	mov    %ax,-0xc(%bp)
L_1b89:
	mov    -0xc(%bp),%ax
	decw   -0xc(%bp)
	or     %ax,%ax
	je   L_1ba7
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	mov    -0x8(%bp),%bx
	mov    %ax,(%bx)
	addw   $0x2,-0x6(%bp)
	addw   $0x2,-0x8(%bp)
	jmp   L_1b89
L_1ba7:
	cmp    %di,%si
	jae   L_1bd0
	mov    -0xa(%bp),%ax
	shl    %ax
	add    %si,%ax
	cmp    %di,%ax
	jb   L_1bd0
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
	mov    D_0cbe,%ax
	mov    %ax,(%bx)
L_1bd0:
	mov    %si,%ax
L_1bd2:
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
	jcxz   L_1bed
	mov    %cx,D_086c
	jmp   L_1bf4
L_1bed:
	mov    0x4(%bp),%bx
	mov    %bx,D_0838
L_1bf4:
	pop    %bp
	ret


L_1bf6:
	push   %bp
	mov    %sp,%bp
	mov	$17,%bx	/* sc: brk */
	mov    0x4(%bp),%ax
	add    D_0838,%ax
	/* v7: 17:brk */
	int	$0xf1
	jcxz   L_1c0d
	mov    %cx,D_086c
	jmp   L_1c1a
L_1c0d:
	mov    D_0838,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	mov    %bx,D_0838
L_1c1a:
	pop    %bp
	ret


L_1c1c:
	push   %bp
	mov    %sp,%bp
	mov	$6,%bx	/* sc: close */
	mov    0x4(%bp),%ax
	/* v7: 6:close */
	int	$0xf1
	jcxz   L_1c2d
	mov    %cx,D_086c
L_1c2d:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1c30:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1ca1
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_1c4d
	xor    %ax,%ax
	jmp   L_1c50
L_1c4d:
	mov    $0x1,%ax
L_1c50:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1c58:
	push   %bp
	mov    %sp,%bp
	mov	$5,%bx	/* sc: open */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 5:open */
	int	$0xf1
	jcxz   L_1c6c
	mov    %cx,D_086c
L_1c6c:
	pop    %bp
	ret


L_1c6e:
	push   %bp
	mov    %sp,%bp
	mov	$3,%bx	/* sc: read */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 3:read */
	int	$0xf1
	jcxz   L_1c85
	mov    %cx,D_086c
L_1c85:
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
	call   L_1cba
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1ca1:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   L_1cba
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


L_1cba:
	push   %bp
	mov    %sp,%bp
	mov	$54,%bx	/* sc: ioctl */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 54:ioctl */
	int	$0xf1
	jcxz   L_1cd1
	mov    %cx,D_086c
L_1cd1:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1cd4:
	push   %bp
	mov    %sp,%bp
	mov	$4,%bx	/* sc: write */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 4:write */
	int	$0xf1
	jcxz   L_1ceb
	mov    %cx,D_086c
L_1ceb:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


L_1cee:
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


L_1d0a:
	push   %bp
	mov    %sp,%bp
	mov	$1,%bx	/* sc: exit */
	mov    0x4(%bp),%ax
	/* v7: 1:exit */
	int	$0xf1
	jcxz   L_1d1b
	mov    %cx,D_086c
L_1d1b:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


	.data

	.byte	0x00, 0x00	/* |..| */
D_0002:
	.byte	0xbf, 0x0f	/* |..| */
D_0004:
	.byte	0x01, 0x00	/* |..| */
D_0006:
	.byte	0x00, 0x02	/* |..| */
D_0008:
	.byte	0x00, 0x02	/* |..| */
D_000a:
	.byte	0x00, 0x01, 0x02, 0x03, 0x9c, 0x09, 0x86, 0x7f	/* |........| */
	.byte	0x97, 0x8d, 0x8e, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f	/* |........| */
	.byte	0x10, 0x11, 0x12, 0x13, 0x9d, 0x85, 0x08, 0x87	/* |........| */
	.byte	0x18, 0x19, 0x92, 0x8f, 0x1c, 0x1d, 0x1e, 0x1f	/* |........| */
	.byte	0x80, 0x81, 0x82, 0x83, 0x84, 0x0a, 0x17, 0x1b	/* |........| */
	.byte	0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x05, 0x06, 0x07	/* |........| */
	.byte	0x90, 0x91, 0x16, 0x93, 0x94, 0x95, 0x96, 0x04	/* |........| */
	.byte	0x98, 0x99, 0x9a, 0x9b, 0x14, 0x15, 0x9e, 0x1a	/* |........| */
	.byte	0x20, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6	/* | .......| */
	.byte	0xa7, 0xa8, 0x5b, 0x2e, 0x3c, 0x28, 0x2b, 0x21	/* |..[.<(+!| */
	.byte	0x26, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf	/* |&.......| */
	.byte	0xb0, 0xb1, 0x5d, 0x24, 0x2a, 0x29, 0x3b, 0x5e	/* |..]$*);^| */
	.byte	0x2d, 0x2f, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7	/* |-/......| */
	.byte	0xb8, 0xb9, 0x7c, 0x2c, 0x25, 0x5f, 0x3e, 0x3f	/* |..|,%_>?| */
	.byte	0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xc0, 0xc1	/* |........| */
	.byte	0xc2, 0x60, 0x3a, 0x23, 0x40, 0x27, 0x3d, 0x22	/* |.`:#@'="| */
	.byte	0xc3, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67	/* |.abcdefg| */
	.byte	0x68, 0x69, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9	/* |hi......| */
	.byte	0xca, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70	/* |.jklmnop| */
	.byte	0x71, 0x72, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 0xd0	/* |qr......| */
	.byte	0xd1, 0x7e, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78	/* |.~stuvwx| */
	.byte	0x79, 0x7a, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7	/* |yz......| */
	.byte	0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf	/* |........| */
	.byte	0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7	/* |........| */
	.byte	0x7b, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47	/* |{ABCDEFG| */
	.byte	0x48, 0x49, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed	/* |HI......| */
	.byte	0x7d, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50	/* |}JKLMNOP| */
	.byte	0x51, 0x52, 0xee, 0xef, 0xf0, 0xf1, 0xf2, 0xf3	/* |QR......| */
	.byte	0x5c, 0x9f, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58	/* |\.STUVWX| */
	.byte	0x59, 0x5a, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9	/* |YZ......| */
	.byte	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37	/* |01234567| */
	.byte	0x38, 0x39, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff	/* |89......| */
D_010a:
	.byte	0x00, 0x01, 0x02, 0x03, 0x37, 0x2d, 0x2e, 0x2f	/* |....7-./| */
	.byte	0x16, 0x05, 0x25, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f	/* |..%.....| */
	.byte	0x10, 0x11, 0x12, 0x13, 0x3c, 0x3d, 0x32, 0x26	/* |....<=2&| */
	.byte	0x18, 0x19, 0x3f, 0x27, 0x1c, 0x1d, 0x1e, 0x1f	/* |..?'....| */
D_012a:
	.byte	0x40, 0x4f, 0x7f, 0x7b, 0x5b, 0x6c, 0x50, 0x7d	/* |@O.{[lP}| */
	.byte	0x4d, 0x5d, 0x5c, 0x4e, 0x6b, 0x60, 0x4b, 0x61	/* |M]\Nk`Ka| */
	.byte	0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7	/* |........| */
	.byte	0xf8, 0xf9, 0x7a, 0x5e, 0x4c, 0x7e, 0x6e, 0x6f	/* |..z^L~no| */
	.byte	0x7c, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7	/* ||.......| */
	.byte	0xc8, 0xc9, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6	/* |........| */
	.byte	0xd7, 0xd8, 0xd9, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6	/* |........| */
	.byte	0xe7, 0xe8, 0xe9, 0x4a, 0xe0, 0x5a, 0x5f, 0x6d	/* |...J.Z_m| */
	.byte	0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87	/* |y.......| */
	.byte	0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96	/* |........| */
	.byte	0x97, 0x98, 0x99, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6	/* |........| */
	.byte	0xa7, 0xa8, 0xa9, 0xc0, 0x6a, 0xd0, 0xa1, 0x07	/* |....j...| */
	.byte	0x20, 0x21, 0x22, 0x23, 0x24, 0x15, 0x06, 0x17	/* | !"#$...| */
	.byte	0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x09, 0x0a, 0x1b	/* |()*+,...| */
	.byte	0x30, 0x31, 0x1a, 0x33, 0x34, 0x35, 0x36, 0x08	/* |01.3456.| */
	.byte	0x38, 0x39, 0x3a, 0x3b, 0x04, 0x14, 0x3e, 0xe1	/* |89:;..>.| */
	.byte	0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48	/* |ABCDEFGH| */
	.byte	0x49, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57	/* |IQRSTUVW| */
	.byte	0x58, 0x59, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67	/* |XYbcdefg| */
	.byte	0x68, 0x69, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75	/* |hipqrstu| */
	.byte	0x76, 0x77, 0x78, 0x80, 0x8a, 0x8b, 0x8c, 0x8d	/* |vwx.....| */
	.byte	0x8e, 0x8f, 0x90, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e	/* |........| */
	.byte	0x9f, 0xa0, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf	/* |........| */
	.byte	0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7	/* |........| */
	.byte	0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf	/* |........| */
	.byte	0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 0xda, 0xdb	/* |........| */
	.byte	0xdc, 0xdd, 0xde, 0xdf, 0xea, 0xeb, 0xec, 0xed	/* |........| */
	.byte	0xee, 0xef, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff	/* |........| */
D_020a:
	.byte	0x00, 0x01, 0x02, 0x03, 0x37, 0x2d, 0x2e, 0x2f	/* |....7-./| */
	.byte	0x16, 0x05, 0x25, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f	/* |..%.....| */
	.byte	0x10, 0x11, 0x12, 0x13, 0x3c, 0x3d, 0x32, 0x26	/* |....<=2&| */
	.byte	0x18, 0x19, 0x3f, 0x27, 0x1c, 0x1d, 0x1e, 0x1f	/* |..?'....| */
D_022a:
	.byte	0x40, 0x5a, 0x7f, 0x7b, 0x5b, 0x6c, 0x50, 0x7d	/* |@Z.{[lP}| */
	.byte	0x4d, 0x5d, 0x5c, 0x4e, 0x6b, 0x60, 0x4b, 0x61	/* |M]\Nk`Ka| */
	.byte	0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7	/* |........| */
	.byte	0xf8, 0xf9, 0x7a, 0x5e, 0x4c, 0x7e, 0x6e, 0x6f	/* |..z^L~no| */
	.byte	0x7c, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7	/* ||.......| */
	.byte	0xc8, 0xc9, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6	/* |........| */
	.byte	0xd7, 0xd8, 0xd9, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6	/* |........| */
	.byte	0xe7, 0xe8, 0xe9, 0xad, 0xe0, 0xbd, 0x5f, 0x6d	/* |......_m| */
	.byte	0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87	/* |y.......| */
	.byte	0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96	/* |........| */
	.byte	0x97, 0x98, 0x99, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6	/* |........| */
	.byte	0xa7, 0xa8, 0xa9, 0xc0, 0x4f, 0xd0, 0xa1, 0x07	/* |....O...| */
	.byte	0x20, 0x21, 0x22, 0x23, 0x24, 0x15, 0x06, 0x17	/* | !"#$...| */
	.byte	0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x09, 0x0a, 0x1b	/* |()*+,...| */
	.byte	0x30, 0x31, 0x1a, 0x33, 0x34, 0x35, 0x36, 0x08	/* |01.3456.| */
	.byte	0x38, 0x39, 0x3a, 0x3b, 0x04, 0x14, 0x3e, 0xe1	/* |89:;..>.| */
	.byte	0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48	/* |ABCDEFGH| */
	.byte	0x49, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57	/* |IQRSTUVW| */
	.byte	0x58, 0x59, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67	/* |XYbcdefg| */
	.byte	0x68, 0x69, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75	/* |hipqrstu| */
	.byte	0x76, 0x77, 0x78, 0x80, 0x8a, 0x8b, 0x8c, 0x8d	/* |vwx.....| */
	.byte	0x8e, 0x8f, 0x90, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e	/* |........| */
	.byte	0x9f, 0xa0, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf	/* |........| */
	.byte	0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7	/* |........| */
	.byte	0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf	/* |........| */
	.byte	0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 0xda, 0xdb	/* |........| */
	.byte	0xdc, 0xdd, 0xde, 0xdf, 0xea, 0xeb, 0xec, 0xed	/* |........| */
	.byte	0xee, 0xef, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff	/* |........| */
D_030a:
	.ascii	"ibs=\0"
D_030f:
	.ascii	"obs=\0"
D_0314:
	.ascii	"cbs=\0"
D_0319:
	.byte	0x62, 0x73, 0x3d, 0x00	/* |bs=.| */
D_031d:
	.byte	0x69, 0x66, 0x3d, 0x00	/* |if=.| */
D_0321:
	.byte	0x6f, 0x66, 0x3d, 0x00	/* |of=.| */
D_0325:
	.ascii	"skip=\0"
D_032b:
	.ascii	"seek=\0"
D_0331:
	.ascii	"count=\0"
D_0338:
	.ascii	"files=\0"
D_033f:
	.ascii	"conv=\0"
D_0345:
	.byte	0x2c, 0x00	/* |,.| */
D_0347:
	.ascii	"ebcdic\0"
D_034e:
	.byte	0x69, 0x62, 0x6d, 0x00	/* |ibm.| */
D_0352:
	.ascii	"ascii\0"
D_0358:
	.ascii	"lcase\0"
D_035e:
	.ascii	"ucase\0"
D_0364:
	.ascii	"swab\0"
D_0369:
	.ascii	"noerror\0"
D_0371:
	.ascii	"sync\0"
D_0376:
	.byte	0x62, 0x61, 0x64, 0x20, 0x61, 0x72, 0x67, 0x3a	/* |bad arg:| */
	.byte	0x20, 0x25, 0x73, 0x0a, 0x00	/* | %s..| */
D_0383:
	.byte	0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x6f	/* |cannot o| */
	.byte	0x70, 0x65, 0x6e, 0x3a, 0x20, 0x25, 0x73, 0x0a	/* |pen: %s.| */
	.byte	0x00	/* |.| */
D_0394:
	.byte	0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x63	/* |cannot c| */
	.byte	0x72, 0x65, 0x61, 0x74, 0x65, 0x3a, 0x20, 0x25	/* |reate: %| */
	.byte	0x73, 0x0a, 0x00	/* |s..| */
D_03a7:
	.byte	0x63, 0x6f, 0x75, 0x6e, 0x74, 0x73, 0x3a, 0x20	/* |counts: | */
	.byte	0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x62	/* |cannot b| */
	.byte	0x65, 0x20, 0x7a, 0x65, 0x72, 0x6f, 0x0a, 0x00	/* |e zero..| */
D_03bf:
	.byte	0x6e, 0x6f, 0x74, 0x20, 0x65, 0x6e, 0x6f, 0x75	/* |not enou| */
	.byte	0x67, 0x68, 0x20, 0x6d, 0x65, 0x6d, 0x6f, 0x72	/* |gh memor| */
	.byte	0x79, 0x0a, 0x00	/* |y..| */
D_03d2:
	.ascii	"read\0"
D_03d7:
	.ascii	"write\0"
D_03dd:
	.byte	0x64, 0x64, 0x3a, 0x20, 0x61, 0x72, 0x67, 0x75	/* |dd: argu| */
	.byte	0x6d, 0x65, 0x6e, 0x74, 0x20, 0x25, 0x44, 0x20	/* |ment %D | */
	.byte	0x6f, 0x75, 0x74, 0x20, 0x6f, 0x66, 0x20, 0x72	/* |out of r| */
	.byte	0x61, 0x6e, 0x67, 0x65, 0x0a, 0x00	/* |ange..| */
D_03fb:
	.byte	0x25, 0x75, 0x2b, 0x25, 0x75, 0x20, 0x72, 0x65	/* |%u+%u re| */
	.byte	0x63, 0x6f, 0x72, 0x64, 0x73, 0x20, 0x69, 0x6e	/* |cords in| */
	.byte	0x0a, 0x00	/* |..| */
D_040d:
	.byte	0x25, 0x75, 0x2b, 0x25, 0x75, 0x20, 0x72, 0x65	/* |%u+%u re| */
	.byte	0x63, 0x6f, 0x72, 0x64, 0x73, 0x20, 0x6f, 0x75	/* |cords ou| */
	.byte	0x74, 0x0a, 0x00	/* |t..| */
D_0420:
	.byte	0x25, 0x75, 0x20, 0x74, 0x72, 0x75, 0x6e, 0x63	/* |%u trunc| */
	.byte	0x61, 0x74, 0x65, 0x64, 0x20, 0x72, 0x65, 0x63	/* |ated rec| */
	.byte	0x6f, 0x72, 0x64, 0x73, 0x0a, 0x00	/* |ords..| */
D_0436:
	.ascii	"Unknown error\0"
D_0444:
	.byte	0x3a, 0x20, 0x00	/* |: .| */
D_0447:
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_044a:
	.byte	0x92, 0x24, 0x9a, 0x24, 0xa4, 0x24, 0xbe, 0x24	/* |.$.$.$.$| */
	.byte	0xce, 0x24, 0xe6, 0x24, 0xf0, 0x24, 0x0a, 0x25	/* |.$.$.$.%| */
	.byte	0x1c, 0x25, 0x2e, 0x25, 0x3e, 0x25, 0x4a, 0x25	/* |.%.%>%J%| */
	.byte	0x5c, 0x25, 0x6c, 0x25, 0x7e, 0x25, 0x8a, 0x25	/* |\%l%~%.%| */
	.byte	0xa0, 0x25, 0xb2, 0x25, 0xbe, 0x25, 0xd0, 0x25	/* |.%.%.%.%| */
	.byte	0xdf, 0x25, 0xef, 0x25, 0xfe, 0x25, 0x0f, 0x26	/* |.%.%.%.&| */
	.byte	0x23, 0x26, 0x37, 0x26, 0x48, 0x26, 0x57, 0x26	/* |#&7&H&W&| */
	.byte	0x66, 0x26, 0x7e, 0x26, 0x8b, 0x26, 0xa1, 0x26	/* |f&~&.&.&| */
	.byte	0xb0, 0x26, 0xbc, 0x26, 0xcf, 0x26	/* |.&.&.&| */
D_0490:
	.byte	0x23, 0x00	/* |#.| */
	.ascii	"Error 0\0"
	.ascii	"Not owner\0"
	.ascii	"No such file or directory\0"
	.ascii	"No such process\0"
	.ascii	"Interrupted system call\0"
	.ascii	"I/O error\0"
	.ascii	"No such device or address\0"
	.ascii	"Arg list too long\0"
	.ascii	"Exec format error\0"
	.ascii	"Bad file number\0"
	.ascii	"No children\0"
	.ascii	"No more processes\0"
	.ascii	"Not enough core\0"
	.ascii	"Permission denied\0"
	.ascii	"Bad address\0"
	.ascii	"Block device required\0"
	.ascii	"Mount device busy\0"
	.ascii	"File exists\0"
	.ascii	"Cross-device link\0"
	.ascii	"No such device\0"
	.ascii	"Not a directory\0"
	.ascii	"Is a directory\0"
	.ascii	"Invalid argument\0"
	.ascii	"File table overflow\0"
	.ascii	"Too many open files\0"
	.ascii	"Not a typewriter\0"
	.ascii	"Text file busy\0"
	.ascii	"File too large\0"
	.ascii	"No space left on device\0"
	.ascii	"Illegal seek\0"
	.ascii	"Read-only file system\0"
	.ascii	"Too many links\0"
	.ascii	"Broken pipe\0"
	.ascii	"Argument too large\0"
	.ascii	"Result too large\0"
D_06e0:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_06e8:
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
D_0737:
	.byte	0x00	/* |.| */
D_0738:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0740:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0748:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0750:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0758:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40	/* |.......@| */
D_0760:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x40	/* |......Y@| */
D_0768:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0770:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0778:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0780:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_0788:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_0790:
	.byte	0xba, 0x2a, 0x00, 0x00, 0xba, 0x2a, 0x01, 0x00	/* |.*...*..| */
D_0798:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02	/* |.......| */
D_079f:
	.byte	0x01	/* |.| */
D_07a0:
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
D_0830:
	.word	D_0830	/* self-ref ptr: 0x2830 */
D_0832:
	.byte	0x00, 0x00	/* |..| */
D_0834:
	.byte	0x00, 0x00	/* |..| */
D_0836:
	.byte	0x00, 0x00	/* |..| */
D_0838:
	.word	0x0cc0	/* _curbrk: bss_end=0x0cc0 rounded to paragraph */

	.bss

D_083a:	.space	2	/* bss word @ 0x083a */
D_083c:	.space	2	/* bss word @ 0x083c */
D_083e:	.space	2	/* bss word @ 0x083e */
D_0840:	.space	2	/* bss word @ 0x0840 */
D_0842:	.space	2	/* bss word @ 0x0842 */
D_0844:	.space	2	/* bss word @ 0x0844 */
D_0846:	.space	2	/* bss word @ 0x0846 */
D_0848:	.space	2	/* bss word @ 0x0848 */
D_084a:	.space	2	/* bss word @ 0x084a */
D_084c:	.space	2	/* bss word @ 0x084c */
D_084e:	.space	2	/* bss word @ 0x084e */
D_0850:	.space	2	/* bss word @ 0x0850 */
D_0852:	.space	2	/* bss word @ 0x0852 */
D_0854:	.space	2	/* bss word @ 0x0854 */
D_0856:	.space	2	/* bss word @ 0x0856 */
D_0858:	.space	2	/* bss word @ 0x0858 */
D_085a:	.space	2	/* bss word @ 0x085a */
D_085c:	.space	2	/* bss word @ 0x085c */
D_085e:	.space	2	/* bss word @ 0x085e */
D_0860:	.space	2	/* bss word @ 0x0860 */
D_0862:	.space	2	/* bss word @ 0x0862 */
D_0864:	.space	2	/* bss word @ 0x0864 */
D_0866:	.space	2	/* bss word @ 0x0866 */
D_0868:	.space	2	/* bss word @ 0x0868 */
D_086a:	.space	2	/* bss word @ 0x086a */
D_086c:	.space	2	/* bss word @ 0x086c */
	.space	4
D_0872:	.space	2	/* bss word @ 0x0872 */
D_0874:	.space	2	/* bss word @ 0x0874 */
D_0876:	.space	2	/* bss word @ 0x0876 */
D_0878:	.space	2	/* bss word @ 0x0878 */
D_087a:	.space	2	/* bss word @ 0x087a */
D_087c:	.space	2	/* bss word @ 0x087c */
D_087e:	.space	2	/* bss word @ 0x087e */
D_0880:	.space	2	/* bss word @ 0x0880 */
D_0882:	.space	2	/* bss word @ 0x0882 */
D_0884:	.space	2	/* bss word @ 0x0884 */
D_0886:	.space	2	/* bss word @ 0x0886 */
D_0888:	.space	2	/* bss word @ 0x0888 */
D_088a:	.space	2	/* bss word @ 0x088a */
D_088c:	.space	2	/* bss word @ 0x088c */
D_088e:	.space	2	/* bss word @ 0x088e */
D_0890:	.space	2	/* bss word @ 0x0890 */
D_0892:	.space	2	/* bss word @ 0x0892 */
D_0894:	.space	2	/* bss word @ 0x0894 */
D_0896:	.space	2	/* bss word @ 0x0896 */
D_0898:	.space	2	/* bss word @ 0x0898 */
D_089a:	.space	2	/* bss word @ 0x089a */
D_089c:	.space	2	/* bss word @ 0x089c */
D_089e:	.space	2	/* bss word @ 0x089e */
D_08a0:	.space	2	/* bss word @ 0x08a0 */
D_08a2:	.space	2	/* bss word @ 0x08a2 */
D_08a4:	.space	2	/* bss word @ 0x08a4 */
D_08a6:	.space	2	/* bss word @ 0x08a6 */
D_08a8:	.space	2	/* bss word @ 0x08a8 */
D_08aa:	.space	2	/* bss word @ 0x08aa */
D_08ac:	.space	2	/* bss word @ 0x08ac */
D_08ae:	.space	2	/* bss word @ 0x08ae */
D_08b0:	.space	2	/* bss word @ 0x08b0 */
D_08b2:	.space	2	/* bss word @ 0x08b2 */
	.space	6
D_08ba:	.space	2	/* bss word @ 0x08ba */
	.space	1022
D_0cba:	.space	2	/* bss word @ 0x0cba */
D_0cbc:	.space	2	/* bss word @ 0x0cbc */
D_0cbe:	.space	2	/* bss word @ 0x0cbe */

/* end of file */
