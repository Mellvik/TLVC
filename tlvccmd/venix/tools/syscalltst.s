/* syscalltst.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 syscalltst.s -o syscalltst.o
 * Syscalls used:
 *   1:exit            2:fork            3:read            4:write           5:open            6:close
 *   7:wait            8:creat           13:time           17:brk            18:stat           19:lseek
 *   35:ftime          41:dup            42:pipe           48:signal         54:ioctl          59:exece
 * INT instructions: $0xf1  $0xf4
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: syscalltst  [object file]
//   Magic:    0x0107  OMAGIC (object/impure)
//   a_stack:  0x0000  (0 bytes)
//   a_text:   0x00002296  (8854 bytes)
//   a_data:   0x000007ac  (1964 bytes)
//   Load:  cs:0x0000 (text)   ds:0x2296 (data/bss)
	/* start: elided (provided by crt0 or vrewrite synthesis) */

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	/* vrewrite: initialize _environ from envp (TLVC crt0 3rd arg) */
	mov	0x8(%bp),%ax
	mov	%ax,D_07ac
	push   %si
	push   %di
	sub    $0x468,%sp
	/* TODO: data ref: movw   $0x229a,-0x448(%bp) */
	movw   $D_0004,-0x448(%bp)
	lea    -0x410(%bp),%ax
	push   %ax
	call   pipe
	pop    %cx
	push   %ax
	lea    -0x40c(%bp),%ax
	push   %ax
	call   pipe
	pop    %cx
	push   %ax
	lea    -0x408(%bp),%ax
	push   %ax
	call   pipe
	pop    %cx
	push   %ax
	/* TODO: data ref: mov    $0x22a4,%ax */
	mov    $D_000e,%ax
	push   %ax
	call   printf
	add    $0x8,%sp
	pushw  -0x40e(%bp)
	pushw  -0x410(%bp)
	pushw  -0x40a(%bp)
	pushw  -0x40c(%bp)
	pushw  -0x406(%bp)
	pushw  -0x408(%bp)
	/* TODO: data ref: mov    $0x22c2,%ax */
	mov    $D_002c,%ax
	push   %ax
	call   printf
	add    $0xe,%sp
	pushw  -0x408(%bp)
	call   dup
	pop    %cx
	mov    %ax,-0x418(%bp)
	pushw  -0x408(%bp)
	pushw  -0x406(%bp)
	call   dup2
	add    $0x4,%sp
	mov    %ax,-0x41a(%bp)
	pushw  -0x41a(%bp)
	pushw  -0x408(%bp)
	pushw  -0x406(%bp)
	pushw  -0x408(%bp)
	pushw  -0x418(%bp)
	/* TODO: data ref: mov    $0x22e4,%ax */
	mov    $D_004e,%ax
	push   %ax
	call   printf
	add    $0xc,%sp
	pushw  -0x418(%bp)
	call   close
	pop    %cx
	pushw  -0x408(%bp)
	call   close
	pop    %cx
	pushw  -0x406(%bp)
	call   close
	pop    %cx
	lea    -0x442(%bp),%ax
	push   %ax
	call   time
	pop    %cx
	mov    %ax,-0x446(%bp)
	mov    %dx,-0x444(%bp)
	lea    -0x442(%bp),%ax
	push   %ax
	call   ctime
	pop    %cx
	push   %ax
	lea    -0x446(%bp),%ax
	push   %ax
	call   ctime
	pop    %cx
	push   %ax
	pushw  -0x440(%bp)
	pushw  -0x442(%bp)
	pushw  -0x444(%bp)
	pushw  -0x446(%bp)
	/* TODO: data ref: mov    $0x2308,%ax */
	mov    $D_0072,%ax
	push   %ax
	call   printf
	add    $0xe,%sp
	cmpw   $0x1,0x4(%bp)
	jg   L_012b
	jmp   L_02c1
L_012b:
	mov    0x6(%bp),%bx
	mov    0x2(%bx),%bx
	cmpb   $0x2f,(%bx)
	je   L_0139
	jmp   L_02a9
L_0139:
	mov    $0x2,%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	call   open
	add    $0x4,%sp
	mov    %ax,-0x418(%bp)
	cmpw   $0x0,-0x418(%bp)
	jge   L_015e
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	call   err
	pop    %cx
L_015e:
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	/* TODO: data ref: mov    $0x2327,%ax */
	mov    $D_0091,%ax
	push   %ax
	call   printf
	add    $0x4,%sp
	lea    -0x43e(%bp),%ax
	push   %ax
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	call   stat
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_018d
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	call   err
	pop    %cx
L_018d:
	pushw  -0x43c(%bp)
	pushw  -0x42e(%bp)
	pushw  -0x430(%bp)
	pushw  -0x43a(%bp)
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	/* TODO: data ref: mov    $0x2333,%ax */
	mov    $D_009d,%ax
	push   %ax
	call   printf
	add    $0xc,%sp
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	mov    $0x400,%ax
	push   %ax
	lea    -0x404(%bp),%ax
	push   %ax
	pushw  -0x418(%bp)
	call   read
	add    $0x6,%sp
	mov    %ax,-0x41c(%bp)
	push   %ax
	/* TODO: data ref: mov    $0x2352,%ax */
	mov    $D_00bc,%ax
	push   %ax
	call   printf
	add    $0x6,%sp
	mov    $0x0,%ax
	push   %ax
	mov    $0x1170,%ax
	mov    $0x1,%dx
	push   %dx
	push   %ax
	pushw  -0x418(%bp)
	call   lseek
	add    $0x8,%sp
	mov    %ax,-0x44c(%bp)
	mov    %dx,-0x44a(%bp)
	pushw  -0x44a(%bp)
	pushw  -0x44c(%bp)
	/* TODO: data ref: mov    $0x2369,%ax */
	mov    $D_00d3,%ax
	push   %ax
	call   printf
	add    $0x6,%sp
	mov    $0x1b4,%ax
	push   %ax
	pushw  -0x448(%bp)
	call   creat
	add    $0x4,%sp
	mov    %ax,-0x41a(%bp)
	or     %ax,%ax
	jge   L_0223
	pushw  -0x448(%bp)
	call   err
	pop    %cx
L_0223:
	pushw  -0x448(%bp)
	/* TODO: data ref: mov    $0x238a,%ax */
	mov    $D_00f4,%ax
	push   %ax
	call   printf
	add    $0x4,%sp
	pushw  -0x41c(%bp)
	lea    -0x404(%bp),%ax
	push   %ax
	pushw  -0x41a(%bp)
	call   write
	add    $0x6,%sp
	mov    %ax,-0x41e(%bp)
	or     %ax,%ax
	jge   L_0254
	pushw  -0x448(%bp)
	call   err
	pop    %cx
L_0254:
	pushw  -0x448(%bp)
	pushw  -0x41e(%bp)
	/* TODO: data ref: mov    $0x2396,%ax */
	mov    $D_0100,%ax
	push   %ax
	call   printf
	add    $0x6,%sp
	pushw  -0x41a(%bp)
	call   close
	pop    %cx
	lea    -0x43e(%bp),%ax
	push   %ax
	pushw  -0x448(%bp)
	call   stat
	add    $0x4,%sp
	or     %ax,%ax
	jge   L_0289
	/* TODO: data ref: mov    $0x23ac,%ax */
	mov    $D_0116,%ax
	push   %ax
	call   err
	pop    %cx
L_0289:
	pushw  -0x42e(%bp)
	pushw  -0x430(%bp)
	pushw  -0x448(%bp)
	/* TODO: data ref: mov    $0x23b3,%ax */
	mov    $D_011d,%ax
	push   %ax
	call   printf
	add    $0x8,%sp
	pushw  -0x418(%bp)
	call   close
	pop    %cx
	jmp   L_02c1
L_02a9:
	mov    0x6(%bp),%bx
	pushw  0x2(%bx)
	/* TODO: data ref: mov    $0x23c9,%ax */
	mov    $D_0133,%ax
	push   %ax
	call   printf
	add    $0x4,%sp
	mov    $0x2,%ax
	push   %ax
	call   _exit
	pop    %cx
L_02c1:
	/* TODO: data ref: mov    $0x23e8,%ax */
	mov    $D_0152,%ax
	push   %ax
	/* TODO: data ref: mov    $0x23db,%ax */
	mov    $D_0145,%ax
	push   %ax
	call   fopen
	add    $0x4,%sp
	mov    %ax,-0x44e(%bp)
	pushw  -0x44e(%bp)
	/* TODO: data ref: mov    $0x23ea,%ax */
	mov    $D_0154,%ax
	push   %ax
	call   printf
	add    $0x4,%sp
	cmpw   $0x0,-0x44e(%bp)
	je   L_02f8
	/* TODO: data ref: mov    $0x240a,%ax */
	mov    $D_0174,%ax
	push   %ax
	pushw  -0x44e(%bp)
	call   fprintf
	add    $0x4,%sp
	jmp   L_0300
L_02f8:
	/* TODO: data ref: mov    $0x242c,%ax */
	mov    $D_0196,%ax
	push   %ax
	call   perror
	pop    %cx
L_0300:
	pushw  -0x44e(%bp)
	call   fclose
	pop    %cx
	/* TODO: data ref: mov    $0x2436,%ax */
	mov    $D_01a0,%ax
	push   %ax
	/* TODO: data ref: mov    $0x2432,%ax */
	mov    $D_019c,%ax
	push   %ax
	call   popen
	add    $0x4,%sp
	mov    %ax,-0x44e(%bp)
	pushw  -0x44e(%bp)
	/* TODO: data ref: mov    $0x2438,%ax */
	mov    $D_01a2,%ax
	push   %ax
	call   printf
	add    $0x4,%sp
	cmpw   $0x0,-0x44e(%bp)
	jne   L_0339
	/* TODO: data ref: mov    $0x244b,%ax */
	mov    $D_01b5,%ax
	push   %ax
	call   perror
	pop    %cx
	jmp   L_0347
L_0339:
	/* TODO: data ref: mov    $0x245a,%ax */
	mov    $D_01c4,%ax
	push   %ax
	pushw  -0x44e(%bp)
	call   fprintf
	add    $0x4,%sp
L_0347:
	pushw  -0x44e(%bp)
	call   pclose
	pop    %cx
	/* TODO: data ref: mov    $0x248a,%ax */
	mov    $D_01f4,%ax
	push   %ax
	/* TODO: data ref: mov    $0x247c,%ax */
	mov    $D_01e6,%ax
	push   %ax
	call   popen
	add    $0x4,%sp
	mov    %ax,-0x44e(%bp)
	pushw  -0x44e(%bp)
	/* TODO: data ref: mov    $0x248c,%ax */
	mov    $D_01f6,%ax
	push   %ax
	call   printf
	add    $0x4,%sp
	cmpw   $0x0,-0x44e(%bp)
	jne   L_0380
	/* TODO: data ref: mov    $0x24a3,%ax */
	mov    $D_020d,%ax
	push   %ax
	call   perror
	pop    %cx
	jmp   L_03b0
L_0380:
	/* TODO: data ref: mov    $0x24b6,%ax */
	mov    $D_0220,%ax
	push   %ax
	call   printf
	pop    %cx
L_0388:
	pushw  -0x44e(%bp)
	mov    $0x1e,%ax
	push   %ax
	lea    -0x46c(%bp),%ax
	push   %ax
	call   fgets
	add    $0x6,%sp
	or     %ax,%ax
	je   L_03b0
	lea    -0x46c(%bp),%ax
	push   %ax
	/* TODO: data ref: mov    $0x24c2,%ax */
	mov    $D_022c,%ax
	push   %ax
	call   printf
	add    $0x4,%sp
	jmp   L_0388
L_03b0:
	pushw  -0x44e(%bp)
	call   pclose
	pop    %cx
	mov    $0x0,%ax
	push   %ax
	call   sbrk
	pop    %cx
	mov    %ax,-0x416(%bp)
	push   %ax
	/* TODO: data ref: mov    $0x24c5,%ax */
	mov    $D_022f,%ax
	push   %ax
	call   printf
	add    $0x4,%sp
	mov    -0x416(%bp),%ax
	add    $0x400,%ax
	push   %ax
	call   brk
	pop    %cx
	mov    -0x416(%bp),%ax
	mov    %ax,-0x412(%bp)
	mov    $0x0,%ax
	push   %ax
	call   sbrk
	pop    %cx
	mov    %ax,-0x416(%bp)
	push   %ax
	/* TODO: data ref: mov    $0x24d1,%ax */
	mov    $D_023b,%ax
	push   %ax
	call   printf
	add    $0x4,%sp
	mov    $0x80,%ax
	push   %ax
	call   sbrk
	pop    %cx
	mov    %ax,-0x414(%bp)
	mov    $0x0,%ax
	push   %ax
	call   sbrk
	pop    %cx
	sub    -0x412(%bp),%ax
	mov    %ax,-0x420(%bp)
	mov    $0x0,%ax
	push   %ax
	call   sbrk
	pop    %cx
	push   %ax
	pushw  -0x414(%bp)
	/* TODO: data ref: mov    $0x24e1,%ax */
	mov    $D_024b,%ax
	push   %ax
	call   printf
	add    $0x6,%sp
L_042d:
	mov    $0x800,%ax
	push   %ax
	call   malloc
	pop    %cx
	mov    %ax,-0x414(%bp)
	or     %ax,%ax
	je   L_0472
	mov    $0x0,%ax
	push   %ax
	call   sbrk
	pop    %cx
	mov    %ax,-0x412(%bp)
	mov    -0x412(%bp),%ax
	sub    -0x414(%bp),%ax
	push   %ax
	pushw  -0x414(%bp)
	pushw  -0x412(%bp)
	/* TODO: data ref: mov    $0x24f9,%ax */
	mov    $D_0263,%ax
	push   %ax
	call   printf
	add    $0x8,%sp
	mov    -0x412(%bp),%ax
	sub    -0x414(%bp),%ax
	add    %ax,-0x420(%bp)
	jmp   L_042d
L_0472:
	mov    -0x414(%bp),%ax
	add    $0x400,%ax
	sub    -0x416(%bp),%ax
	push   %ax
	pushw  -0x420(%bp)
	pushw  -0x414(%bp)
	/* TODO: data ref: mov    $0x251a,%ax */
	mov    $D_0284,%ax
	push   %ax
	call   printf
	add    $0x8,%sp
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


err:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x0,%sp
	pushw  0x4(%bp)
	call   perror
	pop    %cx
	mov    $0x2,%ax
	push   %ax
	call   _exit
	pop    %cx
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


ctime:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	call   localtime
	pop    %cx
	push   %ax
	call   asctime
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


localtime:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x16,%sp
	lea    -0x16(%bp),%ax
	push   %ax
	call   ftime
	pop    %cx
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	mov    -0x10(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	call   ulmul
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
	call   gmtime
	pop    %cx
	mov    %ax,%si
	mov    0xe(%si),%di
	movw   $0x77,-0x6(%bp)
	movw   $0x12f,-0x8(%bp)
	cmpw   $0x4a,0xa(%si)
	je   L_052b
	cmpw   $0x4b,0xa(%si)
	jne   L_0553
L_052b:
	mov    0xa(%si),%ax
	sub    $0x4a,%ax
	mov    %ax,%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	/* TODO: data ref: mov    0x2572(%bx),%ax */
	mov    D_02dc(%bx),%ax
	mov    %ax,-0x6(%bp)
	mov    0xa(%si),%ax
	sub    $0x4a,%ax
	mov    %ax,%bx
	mov    $0x2,%cx
	shl    %cl,%bx
	/* TODO: data ref: mov    0x2574(%bx),%ax */
	mov    D_02de(%bx),%ax
	mov    %ax,-0x8(%bp)
L_0553:
	pushw  -0x6(%bp)
	push   %si
	call   sunday
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
	pushw  -0x8(%bp)
	push   %si
	call   sunday
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	cmpw   $0x0,-0xe(%bp)
	je   L_05a9
	cmp    -0x6(%bp),%di
	jg   L_0583
	cmp    -0x6(%bp),%di
	jne   L_05a9
	cmpw   $0x2,0x4(%si)
	jl   L_05a9
L_0583:
	cmp    -0x8(%bp),%di
	jl   L_0593
	cmp    -0x8(%bp),%di
	jne   L_05a9
	cmpw   $0x1,0x4(%si)
	jge   L_05a9
L_0593:
	addw   $0xe10,-0xc(%bp)
	adcw   $0x0,-0xa(%bp)
	lea    -0xc(%bp),%ax
	push   %ax
	call   gmtime
	pop    %cx
	mov    %ax,%si
	incw   0x10(%si)
L_05a9:
	mov    %si,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


sunday:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	cmp    $0x3a,%si
	jl   L_05d0
	pushw  0xa(%di)
	call   dysize
	pop    %cx
	sub    $0x16d,%ax
	add    %ax,%si
L_05d0:
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


gmtime:
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
	call   lrem
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
	call   ldiv
	add    $0x8,%sp
	mov    %ax,-0x10(%bp)
	mov    %dx,-0xe(%bp)
	cmpw   $0x0,-0xa(%bp)
	jg   L_0648
	jl   L_0637
	cmpw   $0x0,-0xc(%bp)
	jae   L_0648
L_0637:
	addw   $0x5180,-0xc(%bp)
	adcw   $0x1,-0xa(%bp)
	subw   $0x1,-0x10(%bp)
	sbbw   $0x0,-0xe(%bp)
L_0648:
	/* TODO: data ref: mov    $0x257a,%di */
	mov    $D_02e4,%di
	mov    $0x3c,%ax
	xor    %dx,%dx
	push   %dx
	push   %ax
	pushw  -0xa(%bp)
	pushw  -0xc(%bp)
	call   lrem
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
	call   ldiv
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
	call   lrem
	add    $0x8,%sp
	/* TODO: data ref: mov    %ax,0x2586 */
	mov    %ax,D_02f0
	cmpw   $0x0,-0xe(%bp)
	jl   L_06f3
	jg   L_06c7
	cmpw   $0x0,-0x10(%bp)
	jb   L_06f3
L_06c7:
	movw   $0x46,-0x8(%bp)
L_06cc:
	pushw  -0x8(%bp)
	call   dysize
	pop    %cx
	cwtd
	cmp    %dx,-0xe(%bp)
	jl   L_071b
	jg   L_06e0
	cmp    %ax,-0x10(%bp)
	jb   L_071b
L_06e0:
	pushw  -0x8(%bp)
	call   dysize
	pop    %cx
	cwtd
	sub    %ax,-0x10(%bp)
	sbb    %dx,-0xe(%bp)
	incw   -0x8(%bp)
	jmp   L_06cc
L_06f3:
	movw   $0x46,-0x8(%bp)
L_06f8:
	cmpw   $0x0,-0xe(%bp)
	jg   L_071b
	jl   L_0706
	cmpw   $0x0,-0x10(%bp)
	jae   L_071b
L_0706:
	mov    -0x8(%bp),%ax
	dec    %ax
	push   %ax
	call   dysize
	pop    %cx
	cwtd
	add    %ax,-0x10(%bp)
	adc    %dx,-0xe(%bp)
	decw   -0x8(%bp)
	jmp   L_06f8
L_071b:
	mov    -0x8(%bp),%ax
	/* TODO: data ref: mov    %ax,0x2584 */
	mov    %ax,D_02ee
	mov    -0x10(%bp),%ax
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    %ax,0x2588 */
	mov    %ax,D_02f2
	pushw  -0x8(%bp)
	call   dysize
	pop    %cx
	cmp    $0x16e,%ax
	jne   L_073c
	/* TODO: data ref: movw   $0x1d,0x255c */
	movw   $0x1d,D_02c6
L_073c:
	movw   $0x0,-0x8(%bp)
L_0741:
	mov    -0x8(%bp),%bx
	shl    %bx
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x255a(%bx),%ax */
	cmp    D_02c4(%bx),%ax
	jl   L_0760
	mov    -0x8(%bp),%bx
	shl    %bx
	/* TODO: data ref: mov    0x255a(%bx),%ax */
	mov    D_02c4(%bx),%ax
	sub    %ax,-0x6(%bp)
	incw   -0x8(%bp)
	jmp   L_0741
L_0760:
	/* TODO: data ref: movw   $0x1c,0x255c */
	movw   $0x1c,D_02c6
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
	/* TODO: data ref: movw   $0x0,0x258a */
	movw   $0x0,D_02f4
	/* TODO: data ref: mov    $0x257a,%ax */
	mov    $D_02e4,%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


asctime:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: mov    $0x2540,%di */
	mov    $D_02aa,%di
	/* TODO: data ref: mov    $0x258c,%si */
	mov    $D_02f6,%si
L_0795:
	mov    %si,%bx
	inc    %si
	mov    (%bx),%al
	cbtw
	mov    %di,%bx
	inc    %di
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_0795
	mov    0x4(%bp),%bx
	mov    0xc(%bx),%ax
	mov    $0x3,%dx
	imul   %dx
	/* TODO: data ref: add    $0x25a6,%ax */
	add    $D_0310,%ax
	mov    %ax,%si
	/* TODO: data ref: mov    $0x2540,%di */
	mov    $D_02aa,%di
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
	/* TODO: data ref: add    $0x25bc,%ax */
	add    $D_0326,%ax
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
	call   ct_numb
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   ct_numb
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   ct_numb
	add    $0x4,%sp
	mov    %ax,%di
	subw   $0x2,-0x6(%bp)
	mov    -0x6(%bp),%bx
	mov    (%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   ct_numb
	add    $0x4,%sp
	mov    %ax,%di
	mov    0x4(%bp),%bx
	cmpw   $0x64,0xa(%bx)
	jl   L_0884
	movb   $0x32,0x1(%di)
	movb   $0x30,0x2(%di)
L_0884:
	inc    %di
	inc    %di
	mov    0x4(%bp),%bx
	mov    0xa(%bx),%ax
	add    $0x64,%ax
	push   %ax
	push   %di
	call   ct_numb
	add    $0x4,%sp
	mov    %ax,%di
	/* TODO: data ref: mov    $0x2540,%ax */
	mov    $D_02aa,%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


dysize:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%ax
	cwtd
	mov    $0x4,%cx
	idiv   %cx
	or     %dx,%dx
	jne   L_08b8
	mov    $0x16e,%ax
	jmp   L_08bb
L_08b8:
	mov    $0x16d,%ax
L_08bb:
	pop    %di
	pop    %si
	pop    %bp
	ret


ct_numb:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	inc    %di
	cmpw   $0xa,0x6(%bp)
	jl   L_08ea
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
	jmp   L_08f0
L_08ea:
	mov    %di,%bx
	inc    %di
	movb   $0x20,(%bx)
L_08f0:
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


dup:
	push   %bp
	mov    %sp,%bp
	mov	$41,%bx	/* sc: dup */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 41:dup */
	int	$0xf1
	jcxz   L_091e
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_091e:
	pop    %bp
	ret


ftime:
	push   %bp
	mov    %sp,%bp
	mov	$35,%bx	/* sc: ftime */
	mov    0x4(%bp),%ax
	/* v7: 35:ftime */
	int	$0xf1
	jcxz   L_0931
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_0931:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


fprintf:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x4(%bp)
	lea    0x8(%bp),%ax
	push   %ax
	pushw  0x6(%bp)
	call   __doprnt
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


fgets:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x8(%bp),%di
	mov    0x4(%bp),%ax
	mov    %ax,-0x6(%bp)
L_0970:
	decw   0x6(%bp)
	jle   L_09a0
	decw   0x2(%di)
	jl   L_0986
	mov    (%di),%bx
	incw   (%di)
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	jmp   L_098b
L_0986:
	push   %di
	call   __filbuf
	pop    %cx
L_098b:
	mov    %ax,%si
	or     %ax,%ax
	jl   L_09a0
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,(%bx)
	incw   -0x6(%bp)
	cmp    $0xa,%si
	jne   L_0970
L_09a0:
	or     %si,%si
	jge   L_09b0
	mov    -0x6(%bp),%ax
	cmp    0x4(%bp),%ax
	jne   L_09b0
	xor    %ax,%ax
	jmp   L_09bc
L_09b0:
	mov    -0x6(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x6(%bp)
	mov    0x4(%bp),%ax
L_09bc:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


fopen:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x8,%sp
	mov    0x6(%bp),%di
	/* TODO: data ref: cmpb   $0x0,0x25e2 */
	cmpb   $0x0,D_034c
	jne   L_09ff
	/* TODO: data ref: movw   $0x29b0,-0x8(%bp) */
	movw   $D_071a,-0x8(%bp)
	/* TODO: data ref: movw   $0x2a38,0x2a38 */
	/* dual-reloc split: movw $D_07a2,D_07a2 */
	mov	$D_07a2,%ax
	mov	%ax,D_07a2
	movw   $0x88,-0xa(%bp)
L_09e5:
	mov    -0xa(%bp),%ax
	decw   -0xa(%bp)
	or     %ax,%ax
	je   L_09fa
	mov    -0x8(%bp),%bx
	movb   $0x0,(%bx)
	incw   -0x8(%bp)
	jmp   L_09e5
L_09fa:
	/* TODO: data ref: movb   $0x1,0x25e2 */
	movb   $0x1,D_034c
L_09ff:
	/* TODO: data ref: movw   $0x2998,-0x6(%bp) */
	movw   $D_0702,-0x6(%bp)
L_0a04:
	mov    -0x6(%bp),%bx
	mov    0x6(%bx),%al
	cbtw
	test   $0x3,%ax
	je   L_0a23
	mov    -0x6(%bp),%ax
	/* TODO: data ref: cmp    0x2a38,%ax */
	cmp    D_07a2,%ax
	jb   L_0a1d
	xor    %ax,%ax
	jmp   L_0a8e
L_0a1d:
	addw   $0x8,-0x6(%bp)
	jmp   L_0a04
L_0a23:
	cmpb   $0x77,(%di)
	jne   L_0a39
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   creat
	add    $0x4,%sp
	mov    %ax,%si
	jmp   L_0a78
L_0a39:
	cmpb   $0x61,(%di)
	jne   L_0a7a
	mov    $0x1,%ax
	push   %ax
	pushw  0x4(%bp)
	call   open
	add    $0x4,%sp
	mov    %ax,%si
	or     %ax,%ax
	jge   L_0a67
	/* TODO: data ref: cmpw   $0x2,0x2a44 */
	cmpw   $0x2,D_07ae
	jne   L_0a67
	mov    $0x1b6,%ax
	push   %ax
	pushw  0x4(%bp)
	call   creat
	add    $0x4,%sp
	mov    %ax,%si
L_0a67:
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	push   %si
	call   lseek
	add    $0x8,%sp
L_0a78:
	jmp   L_0a88
L_0a7a:
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	call   open
	add    $0x4,%sp
	mov    %ax,%si
L_0a88:
	or     %si,%si
	jge   L_0a90
	xor    %ax,%ax
L_0a8e:
	jmp   L_0ab8
L_0a90:
	mov    -0x6(%bp),%bx
	movw   $0x0,0x2(%bx)
	mov    -0x6(%bp),%bx
	mov    %si,%cx
	mov    %cl,0x7(%bx)
	cmpb   $0x72,(%di)
	je   L_0aae
	mov    -0x6(%bp),%bx
	orb    $0x2,0x6(%bx)
	jmp   L_0ab5
L_0aae:
	mov    -0x6(%bp),%bx
	orb    $0x1,0x6(%bx)
L_0ab5:
	mov    -0x6(%bp),%ax
L_0ab8:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


creat:
	push   %bp
	mov    %sp,%bp
	mov	$8,%bx	/* sc: creat */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 8:creat */
	int	$0xf1
	jcxz   L_0ad4
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_0ad4:
	pop    %bp
	ret


perror:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x25e4,%di */
	mov    $D_034e,%di
	/* TODO: data ref: mov    0x2a44,%ax */
	mov    D_07ae,%ax
	/* TODO: data ref: cmp    0x263e,%ax */
	cmp    D_03a8,%ax
	jge   L_0af1
	/* TODO: data ref: mov    0x2a44,%bx */
	mov    D_07ae,%bx
	shl    %bx
	/* TODO: data ref: mov    0x25f8(%bx),%di */
	mov    D_0362(%bx),%di
L_0af1:
	pushw  0x4(%bp)
	call   strlen
	pop    %cx
	mov    %ax,%si
	or     %si,%si
	je   L_0b1e
	push   %si
	pushw  0x4(%bp)
	mov    $0x2,%ax
	push   %ax
	call   write
	add    $0x6,%sp
	mov    $0x2,%ax
	push   %ax
	/* TODO: data ref: mov    $0x25f2,%ax */
	mov    $D_035c,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   write
	add    $0x6,%sp
L_0b1e:
	push   %di
	call   strlen
	pop    %cx
	push   %ax
	push   %di
	mov    $0x2,%ax
	push   %ax
	call   write
	add    $0x6,%sp
	mov    $0x1,%ax
	push   %ax
	/* TODO: data ref: mov    $0x25f5,%ax */
	mov    $D_035f,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   write
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


popen:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0x8(%bp),%ax
	push   %ax
	call   pipe
	pop    %cx
	or     %ax,%ax
	jge   L_0b5e
	xor    %ax,%ax
	jmp   L_0bd5
L_0b5e:
	mov    0x6(%bp),%bx
	cmpb   $0x72,(%bx)
	jne   L_0b6b
	mov    -0x8(%bp),%ax
	jmp   L_0b6e
L_0b6b:
	mov    -0x6(%bp),%ax
L_0b6e:
	mov    %ax,%di
	mov    0x6(%bp),%bx
	cmpb   $0x72,(%bx)
	jne   L_0b7d
	mov    -0x6(%bp),%ax
	jmp   L_0b80
L_0b7d:
	mov    -0x8(%bp),%ax
L_0b80:
	mov    %ax,%si
	call   fork
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_0bcd
	push   %di
	call   close
	pop    %cx
	mov    0x6(%bp),%bx
	cmpb   $0x72,(%bx)
	jne   L_0b9e
	mov    $0x1,%ax
	jmp   L_0ba0
L_0b9e:
	xor    %ax,%ax
L_0ba0:
	push   %ax
	push   %si
	call   dup2
	add    $0x4,%sp
	push   %si
	call   close
	pop    %cx
	xor    %ax,%ax
	push   %ax
	pushw  0x4(%bp)
	/* TODO: data ref: mov    $0x28c1,%ax */
	mov    $D_062b,%ax
	push   %ax
	/* TODO: data ref: mov    $0x28be,%ax */
	mov    $D_0628,%ax
	push   %ax
	/* TODO: data ref: mov    $0x28b6,%ax */
	mov    $D_0620,%ax
	push   %ax
	call   execl
	add    $0xa,%sp
	mov    $0x1,%ax
	push   %ax
	call   exit_raw
	pop    %cx
L_0bcd:
	cmpw   $0xffff,-0xa(%bp)
	jne   L_0bd7
	xor    %ax,%ax
L_0bd5:
	jmp   L_0bf1
L_0bd7:
	mov    %di,%bx
	shl    %bx
	mov    -0xa(%bp),%ax
	/* TODO: data ref: mov    %ax,0x288e(%bx) */
	mov    %ax,D_05f8(%bx)
	push   %si
	call   close
	pop    %cx
	pushw  0x6(%bp)
	push   %di
	call   fdopen
	add    $0x4,%sp
L_0bf1:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


pclose:
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
	call   fclose
	pop    %cx
	mov    $0x1,%ax
	push   %ax
	mov    $0x2,%ax
	push   %ax
	call   signal
	add    $0x4,%sp
	mov    %ax,-0x8(%bp)
	mov    $0x1,%ax
	push   %ax
	mov    $0x3,%ax
	push   %ax
	call   signal
	add    $0x4,%sp
	mov    %ax,-0xa(%bp)
	mov    $0x1,%ax
	push   %ax
	mov    $0x1,%ax
	push   %ax
	call   signal
	add    $0x4,%sp
	mov    %ax,-0x6(%bp)
L_0c43:
	lea    -0xc(%bp),%ax
	push   %ax
	call   wait
	pop    %cx
	mov    %ax,%si
	mov    %di,%bx
	shl    %bx
	/* TODO: data ref: cmp    0x288e(%bx),%ax */
	cmp    D_05f8(%bx),%ax
	je   L_0c5c
	cmp    $0xffff,%si
	jne   L_0c43
L_0c5c:
	cmp    $0xffff,%si
	jne   L_0c66
	movw   $0xffff,-0xc(%bp)
L_0c66:
	pushw  -0x8(%bp)
	mov    $0x2,%ax
	push   %ax
	call   signal
	add    $0x4,%sp
	pushw  -0xa(%bp)
	mov    $0x3,%ax
	push   %ax
	call   signal
	add    $0x4,%sp
	pushw  -0x6(%bp)
	mov    $0x1,%ax
	push   %ax
	call   signal
	add    $0x4,%sp
	mov    -0xc(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


dup2:
	push   %bp
	mov    %sp,%bp
	mov	$41,%bx	/* sc: dup */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	or     $0x40,%ax
	/* v7: 41:dup */
	int	$0xf1
	jcxz   L_0caf
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_0caf:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


fdopen:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x6(%bp),%di
	/* TODO: data ref: mov    $0x2998,%si */
	mov    $D_0702,%si
L_0cbe:
	mov    0x6(%si),%al
	cbtw
	test   $0x3,%ax
	je   L_0cd6
	/* TODO: data ref: cmp    0x2a38,%si */
	cmp    D_07a2,%si
	jb   L_0cd1
	xor    %ax,%ax
	jmp   L_0d0c
L_0cd1:
	add    $0x8,%si
	jmp   L_0cbe
L_0cd6:
	movw   $0x0,0x2(%si)
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,0x7(%si)
	cmpb   $0x72,(%di)
	je   L_0d06
	orb    $0x2,0x6(%si)
	cmpb   $0x61,(%di)
	jne   L_0d0a
	mov    $0x2,%ax
	push   %ax
	xor    %ax,%ax
	mov    %ax,%dx
	push   %dx
	push   %ax
	pushw  0x4(%bp)
	call   lseek
	add    $0x8,%sp
	jmp   L_0d0a
L_0d06:
	orb    $0x1,0x6(%si)
L_0d0a:
	mov    %si,%ax
L_0d0c:
	pop    %di
	pop    %si
	pop    %bp
	ret


pipe:
	push   %bp
	mov    %sp,%bp
	mov	$42,%bx	/* sc: pipe */
	/* v7: 42:pipe */
	int	$0xf1
	jcxz   L_0d20
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_0d1e:
	pop    %bp
	ret
L_0d20:
	mov    0x4(%bp),%cx
	mov    %cx,%bp
	mov    %ax,-0x70(%bp)
	mov    %dx,0x2(%bp)
	mov    $0x0,%ax
	jmp   L_0d1e
	/* .byte 0 (alignment) */


printf:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x29a0,%ax */
	mov    $D_070a,%ax
	push   %ax
	lea    0x6(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   __doprnt
	add    $0x6,%sp
	/* TODO: data ref: mov    0x29a6,%al */
	mov    D_0710,%al
	cbtw
	test   $0x20,%ax
	je   L_0d56
	mov    $0xffff,%ax
	jmp   L_0d58
L_0d56:
	xor    %ax,%ax
L_0d58:
	pop    %di
	pop    %si
	pop    %bp
	ret


__filbuf:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x1,%ax
	jne   L_0d73
	mov    $0xffff,%ax
	jmp   L_0d7f
L_0d73:
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	je   L_0d81
	mov    $0xffff,%ax
L_0d7f:
	jmp   L_0df9
L_0d81:
	cmpw   $0x0,0x4(%di)
	jne   L_0db5
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0d9c
	mov    0x7(%di),%al
	cbtw
	/* TODO: data ref: add    $0x28c4,%ax */
	add    $D_062e,%ax
	mov    %ax,0x4(%di)
	jmp   L_0d81
L_0d9c:
	mov    $0x200,%ax
	push   %ax
	call   malloc
	pop    %cx
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_0db1
	orb    $0x4,0x6(%di)
	jmp   L_0d81
L_0db1:
	orb    $0x8,0x6(%di)
L_0db5:
	mov    0x4(%di),%ax
	mov    %ax,(%di)
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_0dc8
	mov    $0x1,%ax
	jmp   L_0dcb
L_0dc8:
	mov    $0x200,%ax
L_0dcb:
	push   %ax
	pushw  (%di)
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   read
	add    $0x6,%sp
	mov    %ax,0x2(%di)
	decw   0x2(%di)
	jge   L_0dfb
	cmpw   $0xffff,0x2(%di)
	jne   L_0ded
	orb    $0x10,0x6(%di)
	jmp   L_0df1
L_0ded:
	orb    $0x20,0x6(%di)
L_0df1:
	movw   $0x0,0x2(%di)
	mov    $0xffff,%ax
L_0df9:
	jmp   L_0e05
L_0dfb:
	mov    (%di),%bx
	mov    (%bx),%al
	cbtw
	and    $0xff,%ax
	incw   (%di)
L_0e05:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


strlen:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	xor    %si,%si
L_0e15:
	mov    %di,%bx
	inc    %di
	cmpb   $0x0,(%bx)
	je   L_0e20
	inc    %si
	jmp   L_0e15
L_0e20:
	mov    %si,%ax
	pop    %di
	pop    %si
	pop    %bp
	ret


execl:
	push   %bp
	mov    %sp,%bp
	mov	$59,%bx	/* sc: exece */
	mov    0x4(%bp),%ax
	/* TODO: data ref: mov    0x2a42,%cx */
	mov    D_07ac,%cx
	mov    %bp,%dx
	add    $0x6,%dx
	/* v7: 59:exece */
	int	$0xf1
	jcxz   L_0e41
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_0e41:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


exit_raw:
	push   %bp
	mov    %sp,%bp
	mov	$1,%bx	/* sc: exit */
	mov    0x4(%bp),%ax
	/* v7: 1:exit */
	int	$0xf1
	jcxz   L_0e55
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_0e55:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


fork:
	push   %bp
	mov    %sp,%bp
	mov	$2,%bx	/* sc: fork */
	/* v7: 2:fork */
	int	$0xf1
	jcxz   L_0e66
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_0e66:
	pop    %bp
	ret


signal:
	push   %bp
	mov    %sp,%bp
	mov    $0x16,%cx
	mov    0x4(%bp),%ax
	or     %ax,%ax
	jle   L_0eab
	cmp    $0x20,%ax
	jg   L_0eab
	mov    %ax,%cx
	shl    %ax
	/* TODO: data ref: add    $0x2a4a,%ax */
	add    $D_07b4,%ax
	mov    %ax,%bx
	pushw  (%bx)
	mov    0x6(%bp),%ax
	mov    %ax,(%bx)
	cmp    $0x2,%ax
	jl   L_0e97
	mov    $0x6,%ax
	mul    %cx
	add    $0xeae,%ax
L_0e97:
	mov    %ax,%dx
	mov    %cx,%ax
	mov	$48,%bx	/* sc: signal */
	/* v7: 48:signal */
	int	$0xf1
	cmp    $0x2,%ax
	jl   L_0ea8
	pop    %ax
	jmp   L_0ea9
L_0ea8:
	pop    %bx
L_0ea9:
	jcxz   L_0eb2
L_0eab:
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
	mov    $0xffff,%ax
L_0eb2:
	pop    %bp
	ret
	push   %ax
	/* TODO: data ref: mov    $0x2a4c,%ax */
	mov    $D_07b6,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a4e,%ax */
	mov    $D_07b8,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a50,%ax */
	mov    $D_07ba,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a52,%ax */
	mov    $D_07bc,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a54,%ax */
	mov    $D_07be,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a56,%ax */
	mov    $D_07c0,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a58,%ax */
	mov    $D_07c2,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a5a,%ax */
	mov    $D_07c4,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a5c,%ax */
	mov    $D_07c6,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a5e,%ax */
	mov    $D_07c8,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a60,%ax */
	mov    $D_07ca,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a62,%ax */
	mov    $D_07cc,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a64,%ax */
	mov    $D_07ce,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a66,%ax */
	mov    $D_07d0,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a68,%ax */
	mov    $D_07d2,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a6a,%ax */
	mov    $D_07d4,%ax
	jmp   L_0f18
	push   %ax
	/* TODO: data ref: mov    $0x2a6c,%ax */
	mov    $D_07d6,%ax
L_0f18:
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a6e,%ax */
	mov    $D_07d8,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a70,%ax */
	mov    $D_07da,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a72,%ax */
	mov    $D_07dc,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a74,%ax */
	mov    $D_07de,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a76,%ax */
	mov    $D_07e0,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a78,%ax */
	mov    $D_07e2,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a7a,%ax */
	mov    $D_07e4,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a7c,%ax */
	mov    $D_07e6,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a7e,%ax */
	mov    $D_07e8,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a80,%ax */
	mov    $D_07ea,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a82,%ax */
	mov    $D_07ec,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a84,%ax */
	mov    $D_07ee,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a86,%ax */
	mov    $D_07f0,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a88,%ax */
	mov    $D_07f2,%ax
	jmp   L_0f72
	push   %ax
	/* TODO: data ref: mov    $0x2a8a,%ax */
	mov    $D_07f4,%ax
L_0f72:
	push   %bx
	push   %cx
	push   %dx
	mov    %ax,%bx
	/* TODO: data ref: sub    $0x2a4a,%ax */
	sub    $D_07b4,%ax
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


lseek:
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
	jcxz   L_0fa3
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_0fa3:
	pop    %si
	pop    %bp
	ret


time:
	push   %bp
	mov    %sp,%bp
	mov	$13,%bx	/* sc: time */
	/* v7: 13:time */
	int	$0xf1
	jcxz   L_0fb6
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
	jmp   L_0fc2
L_0fb6:
	mov    0x4(%bp),%bx
	or     %bx,%bx
	je   L_0fc2
	mov    %ax,(%bx)
	mov    %dx,0x2(%bx)
L_0fc2:
	pop    %bp
	ret


__doprnt:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0xaa,%sp
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_0fdc:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %ax,%cx
	mov    %cl,-0x5(%bp)
	or     %cl,%cl
	jne   L_0fee
	jmp   L_158b
L_0fee:
	cmpb   $0x25,-0x5(%bp)
	je   L_1001
	mov    -0x8(%bp),%bx
	mov    -0x5(%bp),%al
	mov    %al,(%bx)
	incw   -0x8(%bp)
L_0fff:
	jmp   L_0fdc
L_1001:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_102c
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   __strout
	add    $0x8,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_102c:
	movb   $0x0,-0xa6(%bp)
	movw   $0x0,-0x10(%bp)
	movb   $0x20,-0xa5(%bp)
	cmpb   $0x2d,(%di)
	jne   L_1045
	incb   -0xa6(%bp)
	inc    %di
L_1045:
	cmpb   $0x30,(%di)
	jne   L_1050
	movb   $0x30,-0xa5(%bp)
	inc    %di
L_1050:
	cmpb   $0x2a,(%di)
	je   L_1080
	movw   $0x0,-0xe(%bp)
L_105a:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_1096
	cmpw   $0x9,-0xc(%bp)
	jg   L_1096
	mov    -0xe(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0xe(%bp)
	jmp   L_105a
L_1080:
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
L_1096:
	movb   $0x0,-0xa7(%bp)
	addw   $0x30,-0xc(%bp)
	cmpw   $0x2e,-0xc(%bp)
	jne   L_10dd
	cmpb   $0x2a,(%di)
	je   L_10df
	movw   $0x0,-0x10(%bp)
L_10af:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	sub    $0x30,%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jl   L_10d9
	cmpw   $0x9,-0xc(%bp)
	jg   L_10d9
	incb   -0xa7(%bp)
	mov    -0x10(%bp),%ax
	mov    $0xa,%dx
	imul   %dx
	add    -0xc(%bp),%ax
	mov    %ax,-0x10(%bp)
	jmp   L_10af
L_10d9:
	addw   $0x30,-0xc(%bp)
L_10dd:
	jmp   L_10f6
L_10df:
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
L_10f6:
	movw   $0xa,-0x12(%bp)
	mov    -0xc(%bp),%ax
	jmp   L_1500
L_1101:
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	jmp   L_1112
L_1109:
	dec    %di
	cmpb   $0x4c,-0x1(%di)
	jne   L_1142
	jmp   L_113b
L_1112:
	cmp    $0x64,%ax
	je   L_1144
	cmp    $0x6f,%ax
	je   L_1128
	cmp    $0x75,%ax
	je   L_113b
	cmp    $0x78,%ax
	je   L_1136
	jmp   L_1109
L_1128:
	movw   $0x8,-0x12(%bp)
	jmp   L_113b
L_112f:
	movw   $0x8,-0x12(%bp)
	jmp   L_1142
L_1136:
	movw   $0x10,-0x12(%bp)
L_113b:
	jmp   L_118d
L_113d:
	movw   $0x10,-0x12(%bp)
L_1142:
	jmp   L_119d
L_1144:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_1161
L_1154:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	cwtd
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_1161:
	cmpw   $0x0,-0x14(%bp)
	jg   L_11ab
	jl   L_116f
	cmpw   $0x0,-0x16(%bp)
	jae   L_11ab
L_116f:
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
	jmp   L_11ab
L_118d:
	mov    (%si),%ax
	mov    0x2(%si),%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	add    $0x4,%si
	jmp   L_11ab
L_119d:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	xor    %dx,%dx
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
L_11ab:
	cmpw   $0x0,-0x14(%bp)
	jne   L_11b5
	cmpw   $0x0,-0x16(%bp)
L_11b5:
	jne   L_11c6
	cmpw   $0x0,-0x10(%bp)
	jne   L_11c6
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
L_11c6:
	lea    -0xa4(%bp),%ax
	mov    %ax,-0xa(%bp)
L_11cd:
	cmpw   $0x0,-0x14(%bp)
	jne   L_11d7
	cmpw   $0x0,-0x16(%bp)
L_11d7:
	je   L_1222
	mov    -0x12(%bp),%ax
	cwtd
	push   %dx
	push   %ax
	pushw  -0x14(%bp)
	pushw  -0x16(%bp)
	call   ulrem
	add    $0x8,%sp
	mov    %ax,-0xc(%bp)
	cmp    $0xa,%ax
	jge   L_11f8
	mov    $0x30,%ax
	jmp   L_11fb
L_11f8:
	mov    $0x57,%ax
L_11fb:
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
	call   uldiv
	add    $0x8,%sp
	mov    %ax,-0x16(%bp)
	mov    %dx,-0x14(%bp)
	jmp   L_11cd
L_1222:
	lea    -0xa4(%bp),%ax
	cmp    %ax,-0xa(%bp)
	jbe   L_1271
	decw   -0xa(%bp)
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0x8(%bp)
	jmp   L_1222
L_1240:
	mov    %si,%ax
	inc    %si
	inc    %si
	mov    %ax,-0xa(%bp)
	movw   $0x0,-0xc(%bp)
L_124c:
	cmpw   $0x2,-0xc(%bp)
	jge   L_1271
	mov    -0xa(%bp),%bx
	incw   -0xa(%bp)
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	mov    %ax,%cx
	mov    %cl,(%bx)
	or     %cl,%cl
	jne   L_126c
	decw   -0x8(%bp)
L_126c:
	incw   -0xc(%bp)
	jmp   L_124c
L_1271:
	lea    -0x9a(%bp),%ax
	mov    %ax,-0xa(%bp)
L_1278:
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	mov    -0xe(%bp),%dx
	sub    %ax,%dx
	mov    %dx,-0xc(%bp)
	or     %dx,%dx
	jge   L_128f
	movw   $0x0,-0xc(%bp)
L_128f:
	cmpb   $0x0,-0xa6(%bp)
	jne   L_129e
	mov    -0xc(%bp),%ax
	neg    %ax
	mov    %ax,-0xc(%bp)
L_129e:
	mov    -0xa5(%bp),%al
	cbtw
	push   %ax
	pushw  0x8(%bp)
	pushw  -0xc(%bp)
	mov    -0x8(%bp),%ax
	sub    -0xa(%bp),%ax
	push   %ax
	pushw  -0xa(%bp)
	call   __strout
	add    $0xa,%sp
	lea    -0x9a(%bp),%ax
	mov    %ax,-0x8(%bp)
L_12c1:
	jmp   L_0fff
L_12c4:
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%ax
	mov    %ax,-0xa(%bp)
	or     %ax,%ax
	jne   L_12d6
	/* TODO: data ref: movw   $0x28d8,-0xa(%bp) */
	movw   $D_0642,-0xa(%bp)
L_12d6:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jne   L_12e5
	movw   $0x7fff,-0xc(%bp)
L_12e5:
	mov    -0xa(%bp),%ax
	mov    %ax,-0x8(%bp)
L_12eb:
	mov    -0x8(%bp),%bx
	cmpb   $0x0,(%bx)
	je   L_1278
	decw   -0xc(%bp)
	jl   L_1278
	incw   -0x8(%bp)
	jmp   L_12eb
L_12fd:
	mov    (%si),%si
	mov    %si,%bx
	inc    %si
	inc    %si
	mov    (%bx),%di
L_1305:
	jmp   L_12c1
L_1307:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_1313
	movw   $0x6,-0x10(%bp)
L_1313:
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
	call   fcvt
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_1348
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_1348:
	mov    -0x1a(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	jg   L_135d
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	jmp   L_1375
L_135d:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	decw   -0xc(%bp)
	jg   L_135d
L_1375:
	mov    -0x10(%bp),%ax
	mov    %ax,-0xc(%bp)
	or     %ax,%ax
	je   L_1388
	mov    -0x8(%bp),%bx
	movb   $0x2e,(%bx)
	incw   -0x8(%bp)
L_1388:
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
	or     %ax,%ax
	jle   L_13a7
L_1394:
	decw   -0xc(%bp)
	jl   L_13a7
	mov    -0x8(%bp),%bx
	movb   $0x30,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	jg   L_1394
L_13a7:
	cmpw   $0x0,-0xc(%bp)
	jg   L_13b0
	jmp   L_1271
L_13b0:
	decw   -0xc(%bp)
	jge   L_13b8
	jmp   L_1271
L_13b8:
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_13b0
L_13cd:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_13db
	movw   $0x6,-0x10(%bp)
	jmp   L_13de
L_13db:
	incw   -0x10(%bp)
L_13de:
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
	call   ecvt
	add    $0xe,%sp
	mov    %ax,-0xa(%bp)
	add    $0x8,%si
	cmpw   $0x0,-0x18(%bp)
	je   L_1413
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
L_1413:
	mov    -0xa(%bp),%bx
	cmpb   $0x30,(%bx)
	jne   L_141e
	incw   -0x1a(%bp)
L_141e:
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
L_1440:
	decw   -0xc(%bp)
	jle   L_145a
	mov    -0xa(%bp),%bx
	mov    (%bx),%al
	cbtw
	mov    -0x8(%bp),%bx
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	incw   -0x8(%bp)
	jmp   L_1440
L_145a:
	mov    -0x8(%bp),%bx
	movb   $0x65,(%bx)
	incw   -0x8(%bp)
	decw   -0x1a(%bp)
	cmpw   $0x0,-0x1a(%bp)
	jl   L_1477
	mov    -0x8(%bp),%bx
	movb   $0x2b,(%bx)
	incw   -0x8(%bp)
	jmp   L_1488
L_1477:
	mov    -0x8(%bp),%bx
	movb   $0x2d,(%bx)
	incw   -0x8(%bp)
	mov    -0x1a(%bp),%ax
	neg    %ax
	mov    %ax,-0x1a(%bp)
L_1488:
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
	jmp   L_1271
L_14b9:
	cmpb   $0x0,-0xa7(%bp)
	jne   L_14c5
	movw   $0x6,-0x10(%bp)
L_14c5:
	pushw  -0x8(%bp)
	pushw  -0x10(%bp)
	int    $0xf4
	fldl   (%si)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   gcvt
	add    $0xc,%sp
	add    $0x8,%si
L_14e3:
	mov    -0x8(%bp),%bx
	incw   -0x8(%bp)
	cmpb   $0x0,(%bx)
	jne   L_14e3
	decw   -0x8(%bp)
	jmp   L_1271
L_14f4:
	mov    -0x8(%bp),%bx
	movb   $0x25,(%bx)
	incw   -0x8(%bp)
L_14fd:
	jmp   L_1305
L_1500:
	cmp    $0x65,%ax
	jne   L_1508
	jmp   L_13cd
L_1508:
	jg   L_1549
	cmp    $0x4f,%ax
	jne   L_1512
	jmp   L_1128
L_1512:
	jg   L_1529
	cmp    $0x25,%ax
	je   L_14f4
	cmp    $0x44,%ax
	jne   L_1521
	jmp   L_1144
L_1521:
	cmp    $0x4c,%ax
L_1524:
	jne   L_1529
	jmp   L_1101
L_1529:
	cmp    $0x55,%ax
	jne   L_1531
	jmp   L_118d
L_1531:
	cmp    $0x58,%ax
	jne   L_1539
	jmp   L_1136
L_1539:
	cmp    $0x63,%ax
	jne   L_1541
	jmp   L_1240
L_1541:
	cmp    $0x64,%ax
	jne   L_1549
	jmp   L_1154
L_1549:
	cmp    $0x6f,%ax
	jne   L_1551
	jmp   L_112f
L_1551:
	jg   L_1568
	cmp    $0x66,%ax
	jne   L_155b
	jmp   L_1307
L_155b:
	cmp    $0x67,%ax
	jne   L_1563
	jmp   L_14b9
L_1563:
	cmp    $0x6c,%ax
	je   L_1524
L_1568:
	cmp    $0x72,%ax
	jne   L_1570
	jmp   L_12fd
L_1570:
	cmp    $0x73,%ax
	jne   L_1578
	jmp   L_12c4
L_1578:
	cmp    $0x75,%ax
	jne   L_1580
	jmp   L_119d
L_1580:
	cmp    $0x78,%ax
	jne   L_1588
	jmp   L_113d
L_1588:
	jmp   L_14fd
L_158b:
	lea    -0x9a(%bp),%ax
	cmp    %ax,-0x8(%bp)
	je   L_15af
	pushw  0x8(%bp)
	xor    %ax,%ax
	push   %ax
	lea    -0x9a(%bp),%ax
	mov    -0x8(%bp),%dx
	sub    %ax,%dx
	push   %dx
	lea    -0x9a(%bp),%ax
	push   %ax
	call   __strout
	add    $0x8,%sp
L_15af:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret

	/* almul: elided (provided by crt0 or vrewrite synthesis) */

ulmul:
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

	/* auldiv: elided (provided by crt0 or vrewrite synthesis) */

uldiv:
	mov    %sp,%bx
	mov    0x8(%bx),%ax
	cmp    $0x0,%ax
	jne   L_1600
	mov    0x4(%bx),%ax
	mov    $0x0,%dx
	divw   0x6(%bx)
	mov    %ax,%cx
	mov    0x2(%bx),%ax
	divw   0x6(%bx)
	mov    %cx,%dx
	ret
L_1600:
	mov    $0x0,%cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
L_1609:
	shr    %dx
	rcr    %ax
	inc    %cx
	or     %dx,%dx
	jne   L_1609
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
L_1619:
	shr    %dx
	rcr    %ax
	dec    %cx
	or     %cx,%cx
	jne   L_1619
	divw   -0x2(%bx)
	add    $0x2,%sp
	push   %ax
	push   %bx
	mov    $0x0,%dx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   ulmul
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	cmp    $0x0,%dx
	jl   L_165a
	jne   L_164f
	or     $0x0,%ax
	je   L_165a
L_164f:
	pop    %ax
	mov    $0x0,%dx
	sub    $0x1,%ax
	sbb    $0x0,%dx
	ret
L_165a:
	pop    %ax
	mov    $0x0,%dx
	ret

	/* aldiv: elided (provided by crt0 or vrewrite synthesis) */

ldiv:
	mov    %sp,%bx
	mov    $0x0,%cx
	push   %bx
	push   %cx
	mov    0x6(%bx),%ax
	mov    0x8(%bx),%dx
	cmp    $0x0,%dx
	jge   L_167e
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_167e:
	push   %dx
	push   %ax
	mov    0x2(%bx),%ax
	mov    0x4(%bx),%dx
	cmp    $0x0,%dx
	jge   L_1695
	incw   -0x4(%bx)
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
L_1695:
	push   %dx
	push   %ax
	call   uldiv
	add    $0x8,%sp
	pop    %cx
	pop    %bx
	and    $0x1,%cx
	je   L_16af
	not    %ax
	not    %dx
	add    $0x1,%ax
	adc    $0x0,%dx
L_16af:
	ret

	/* alrem: elided (provided by crt0 or vrewrite synthesis) */

lrem:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   ldiv
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   ulmul
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret

	/* aulrem: elided (provided by crt0 or vrewrite synthesis) */

ulrem:
	mov    %sp,%bx
	push   %bx
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	pushw  0x4(%bx)
	pushw  0x2(%bx)
	call   uldiv
	add    $0x8,%sp
	pop    %bx
	push   %bx
	push   %dx
	push   %ax
	pushw  0x8(%bx)
	pushw  0x6(%bx)
	call   ulmul
	add    $0x8,%sp
	pop    %bx
	sub    0x2(%bx),%ax
	sbb    0x4(%bx),%dx
	neg    %dx
	neg    %ax
	sbb    $0x0,%dx
	ret


a_com:
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


gcvt:
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
	call   ecvt
	add    $0xe,%sp
	mov    %ax,%di
	mov    0xe(%bp),%si
	cmpw   $0x0,-0x6(%bp)
	je   L_177a
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
L_177a:
	mov    0xc(%bp),%ax
	dec    %ax
	mov    %ax,-0xa(%bp)
L_1781:
	cmpw   $0x0,-0xa(%bp)
	jle   L_179b
	mov    %di,%ax
	add    -0xa(%bp),%ax
	mov    %ax,%bx
	cmpb   $0x30,(%bx)
	jne   L_179b
	decw   0xc(%bp)
	decw   -0xa(%bp)
	jmp   L_1781
L_179b:
	cmpw   $0x0,-0x8(%bp)
	jl   L_17ac
	mov    -0x8(%bp),%ax
	sub    0xc(%bp),%ax
	cmp    $0x4,%ax
	jg   L_17b8
L_17ac:
	cmpw   $0x0,-0x8(%bp)
	jge   L_17b6
	cmpw   $0xfffd,-0x8(%bp)
L_17b6:
	jge   L_1837
L_17b8:
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
L_17d3:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jge   L_17ed
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	mov    %si,%bx
	inc    %si
	mov    %ax,%cx
	mov    %cl,(%bx)
	incw   -0xa(%bp)
	jmp   L_17d3
L_17ed:
	mov    %si,%bx
	inc    %si
	movb   $0x65,(%bx)
	cmpw   $0x0,-0x8(%bp)
	jge   L_1809
	mov    -0x8(%bp),%ax
	neg    %ax
	mov    %ax,-0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x2d,(%bx)
	jmp   L_180f
L_1809:
	mov    %si,%bx
	inc    %si
	movb   $0x2b,(%bx)
L_180f:
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
	jmp   L_18a7
L_1837:
	cmpw   $0x0,-0x8(%bp)
	jg   L_1859
	cmpb   $0x30,(%di)
	je   L_1848
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1848:
	cmpw   $0x0,-0x8(%bp)
	jge   L_1859
	incw   -0x8(%bp)
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_1848
L_1859:
	movw   $0x1,-0xa(%bp)
L_185e:
	mov    -0xa(%bp),%ax
	cmp    0xc(%bp),%ax
	jg   L_1886
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
	jne   L_1881
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_1881:
	incw   -0xa(%bp)
	jmp   L_185e
L_1886:
	mov    0xc(%bp),%ax
	cmp    -0x8(%bp),%ax
	jge   L_18a7
L_188e:
	mov    0xc(%bp),%ax
	incw   0xc(%bp)
	cmp    -0x8(%bp),%ax
	jge   L_18a1
	mov    %si,%bx
	inc    %si
	movb   $0x30,(%bx)
	jmp   L_188e
L_18a1:
	mov    %si,%bx
	inc    %si
	movb   $0x2e,(%bx)
L_18a7:
	cmpb   $0x2e,-0x1(%si)
	jne   L_18ae
	dec    %si
L_18ae:
	movb   $0x0,(%si)
	mov    0xe(%bp),%ax
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


ecvt:
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
	call   cvt
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


fcvt:
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
	call   cvt
	add    $0x10,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


cvt:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x5c,%sp
	cmpw   $0x0,0xc(%bp)
	jge   L_1926
	movw   $0x0,0xc(%bp)
L_1926:
	cmpw   $0x4f,0xc(%bp)
	jl   L_1931
	movw   $0x4e,0xc(%bp)
L_1931:
	xor    %di,%di
	mov    0x10(%bp),%bx
	movw   $0x0,(%bx)
	/* TODO: data ref: mov    $0x28e0,%si */
	mov    $D_064a,%si
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x2930 */
	fcompl D_069a
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jae   L_196b
	mov    0x10(%bp),%bx
	movw   $0x1,(%bx)
	int    $0xf4
	fldl   0x4(%bp)
	fchs
	fstl   -0x46(%bp)
	fstpl  0x4(%bp)
L_196b:
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   modf
	int    $0xf4
	fstpl  -0x46(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x46(%bp)
	fstpl  0x4(%bp)
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x2938 */
	fcompl D_06a2
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jne   L_19ac
	jmp   L_1a82
L_19ac:
	/* TODO: data ref: mov    $0x28e0,%si */
	mov    $D_064a,%si
L_19af:
	int    $0xf4
	fldl   -0xc(%bp)
	/* TODO: data ref: fcompl 0x2940 */
	fcompl D_06aa
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jb   L_1a3b
	lea    -0xc(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   -0xc(%bp)
	/* TODO: data ref: fmull  0x2948 */
	fmull  D_06b2
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   modf
	int    $0xf4
	fstpl  -0x4e(%bp)
	add    $0xa,%sp
	int    $0xf4
	fldl   -0x4e(%bp)
	/* TODO: data ref: fmull  0x2950 */
	fmull  D_06ba
	/* TODO: data ref: faddl  0x2958 */
	faddl  D_06c2
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
	cmp    $D_047a,%ax
	jne   L_19af
	int    $0xf4
	fldl   -0xc(%bp)
	/* TODO: data ref: faddl  0x2960 */
	faddl  D_06ca
	fstpl  -0xc(%bp)
	lea    -0x3e(%bp),%ax
	lea    -0x1(%di),%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	movw   $0x0,(%bx)
	jmp   L_19af
L_1a3b:
	mov    $0x1,%ax
	push   %ax
	push   %si
	int    $0xf4
	fldl   -0xc(%bp)
	fistps -0x40(%bp)
	fwait
	pushw  -0x40(%bp)
	call   itoa
	add    $0x6,%sp
	mov    %ax,%si
L_1a56:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_1a78
	xor    %ax,%ax
	push   %ax
	push   %si
	lea    -0x3e(%bp),%ax
	mov    %di,%dx
	shl    %dx
	add    %dx,%ax
	mov    %ax,%bx
	pushw  (%bx)
	call   itoa
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_1a56
L_1a78:
	mov    %si,%ax
	/* TODO: data ref: sub    $0x28e0,%ax */
	sub    $D_064a,%ax
	mov    %ax,%di
	jmp   L_1b02
L_1a82:
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fcompl 0x2968 */
	fcompl D_06d2
	fstsw  -0x40(%bp)
	fwait
	mov    -0x3f(%bp),%ah
	sahf
	jbe   L_1b02
L_1a98:
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x2970 */
	fmull  D_06da
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x2978 */
	fcompl D_06e2
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_1ace
	int    $0xf4
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	sub    $0x3,%di
	jmp   L_1a98
L_1ace:
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x2980 */
	fmull  D_06ea
	fstl   -0x46(%bp)
	fstl   -0x14(%bp)
	fstl   -0x4e(%bp)
	/* TODO: data ref: fcompl 0x2988 */
	fcompl D_06f2
	fstsw  -0x50(%bp)
	fwait
	mov    -0x4f(%bp),%ah
	sahf
	jae   L_1b02
	int    $0xf4
	fldl   -0x14(%bp)
	fstpl  0x4(%bp)
	dec    %di
	jmp   L_1ace
L_1b02:
	mov    0xc(%bp),%ax
	/* TODO: data ref: add    $0x28e0,%ax */
	add    $D_064a,%ax
	mov    %ax,-0x16(%bp)
	cmpw   $0x0,0x12(%bp)
	jne   L_1b14
	add    %di,-0x16(%bp)
L_1b14:
	mov    0xe(%bp),%bx
	mov    %di,(%bx)
	/* TODO: data ref: cmpw   $0x28e0,-0x16(%bp) */
	cmpw   $D_064a,-0x16(%bp)
	jae   L_1b2a
	/* TODO: data ref: movb   $0x0,0x28e0 */
	movb   $0x0,D_064a
	/* TODO: data ref: mov    $0x28e0,%ax */
	mov    $D_064a,%ax
	jmp   L_1b93
L_1b2a:
	cmp    -0x16(%bp),%si
	ja   L_1b84
	/* TODO: data ref: cmp    $0x2930,%si */
	cmp    $D_069a,%si
	jae   L_1b84
	lea    -0x14(%bp),%ax
	push   %ax
	int    $0xf4
	fldl   0x4(%bp)
	/* TODO: data ref: fmull  0x2990 */
	fmull  D_06fa
	fstl   -0x46(%bp)
	sub    $0x8,%sp
	mov    %sp,%bx
	int    $0xf4
	fstpl  (%bx)
	call   modf
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
	call   itoa
	add    $0x6,%sp
	mov    %ax,%si
	jmp   L_1b2a
L_1b84:
	/* TODO: data ref: cmpw   $0x2930,-0x16(%bp) */
	cmpw   $D_069a,-0x16(%bp)
	jb   L_1b95
	/* TODO: data ref: movb   $0x0,0x292f */
	movb   $0x0,D_0699
	/* TODO: data ref: mov    $0x28e0,%ax */
	mov    $D_064a,%ax
L_1b93:
	jmp   L_1bdf
L_1b95:
	mov    -0x16(%bp),%si
	mov    %si,%bx
	addb   $0x5,(%bx)
L_1b9d:
	mov    -0x16(%bp),%bx
	cmpb   $0x39,(%bx)
	jle   L_1bd9
	mov    -0x16(%bp),%bx
	movb   $0x30,(%bx)
	/* TODO: data ref: cmpw   $0x28e0,-0x16(%bp) */
	cmpw   $D_064a,-0x16(%bp)
	jbe   L_1bbc
	decw   -0x16(%bp)
	mov    -0x16(%bp),%bx
	incb   (%bx)
	jmp   L_1b9d
L_1bbc:
	mov    -0x16(%bp),%bx
	movb   $0x31,(%bx)
	mov    0xe(%bp),%bx
	incw   (%bx)
	cmpw   $0x0,0x12(%bp)
	jne   L_1b9d
	/* TODO: data ref: cmp    $0x28e0,%si */
	cmp    $D_064a,%si
	jbe   L_1bd6
	movb   $0x30,(%si)
L_1bd6:
	inc    %si
	jmp   L_1b9d
L_1bd9:
	movb   $0x0,(%si)
	/* TODO: data ref: mov    $0x28e0,%ax */
	mov    $D_064a,%ax
L_1bdf:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


itoa:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	mov    0x4(%bp),%si
	xor    %di,%di
L_1bf1:
	or     %si,%si
	je   L_1c02
	mov    %si,%ax
	cwtd
	mov    $0xa,%cx
	idiv   %cx
	mov    %ax,%si
	inc    %di
	jmp   L_1bf1
L_1c02:
	cmpw   $0x0,0x8(%bp)
	jne   L_1c1b
	mov    %di,%si
L_1c0a:
	cmp    $0x4,%si
	jge   L_1c1b
	mov    0x6(%bp),%bx
	movb   $0x30,(%bx)
	incw   0x6(%bp)
	inc    %si
	jmp   L_1c0a
L_1c1b:
	mov    0x6(%bp),%ax
	add    %di,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,%bx
	movb   $0x0,(%bx)
	mov    0x4(%bp),%si
L_1c2b:
	mov    %di,%ax
	dec    %di
	or     %ax,%ax
	je   L_1c54
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
	jmp   L_1c2b
L_1c54:
	mov    -0x6(%bp),%ax
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


__strout:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    0x6(%bp),%si
L_1c69:
	cmpw   $0x0,0x8(%bp)
	jl   L_1c72
	jmp   L_1d00
L_1c72:
	cmpb   $0x2d,(%di)
	jne   L_1caf
	cmpw   $0x30,0xc(%bp)
	jne   L_1caf
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1c9e
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
	jmp   L_1cae
L_1c9e:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   __flsbuf
	add    $0x4,%sp
L_1cae:
	dec    %si
L_1caf:
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1ccd
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
	jmp   L_1cd9
L_1ccd:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   __flsbuf
	add    $0x4,%sp
L_1cd9:
	incw   0x8(%bp)
	jmp   L_1c69
L_1cde:
	dec    %si
	jl   L_1d14
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1d02
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
L_1d00:
	jmp   L_1cde
L_1d02:
	pushw  0xa(%bp)
	mov    %di,%bx
	inc    %di
	mov    (%bx),%al
	cbtw
	push   %ax
	call   __flsbuf
	add    $0x4,%sp
	jmp   L_1cde
L_1d14:
	cmpw   $0x0,0x8(%bp)
	je   L_1d49
	mov    0xa(%bp),%bx
	decw   0x2(%bx)
	jl   L_1d38
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
	jmp   L_1d44
L_1d38:
	pushw  0xa(%bp)
	pushw  0xc(%bp)
	call   __flsbuf
	add    $0x4,%sp
L_1d44:
	decw   0x8(%bp)
	jmp   L_1d14
L_1d49:
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


__flsbuf:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	mov    0x6(%bp),%di
	mov    0x6(%di),%al
	cbtw
	test   $0x2,%ax
	jne   L_1d69
	mov    $0xffff,%ax
	jmp   L_1e30
L_1d69:
	mov    0x6(%di),%al
	cbtw
	test   $0x4,%ax
	je   L_1d9c
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
	call   write
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
	movw   $0x0,0x2(%di)
	jmp   L_1e21
L_1d9c:
	mov    0x4(%di),%si
	or     %si,%si
	jne   L_1dee
	/* TODO: data ref: cmp    $0x29a0,%di */
	cmp    $D_070a,%di
	jne   L_1dc7
	/* TODO: data ref: mov    0x29a7,%al */
	mov    D_0711,%al
	cbtw
	push   %ax
	call   isatty
	pop    %cx
	or     %ax,%ax
	je   L_1dbc
	orb    $0x4,0x6(%di)
	jmp   L_1d69
L_1dbc:
	/* TODO: data ref: movw   $0x2a92,0x4(%di) */
	movw   $D_07fc,0x4(%di)
	/* TODO: data ref: movw   $0x2a92,(%di) */
	movw   $D_07fc,(%di)
	jmp   L_1d69
L_1dc7:
	mov    $0x200,%ax
	push   %ax
	call   malloc
	pop    %cx
	mov    %ax,%si
	mov    %ax,0x4(%di)
	or     %ax,%ax
	jne   L_1dde
	orb    $0x4,0x6(%di)
	jmp   L_1d69
L_1dde:
	orb    $0x8,0x6(%di)
	movw   $0x0,-0x6(%bp)
	movw   $0x0,-0x8(%bp)
	jmp   L_1e10
L_1dee:
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	mov    %ax,-0x8(%bp)
	or     %ax,%ax
	jle   L_1e10
	mov    %si,(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   write
	add    $0x6,%sp
	mov    %ax,-0x6(%bp)
L_1e10:
	movw   $0x1ff,0x2(%di)
	mov    %si,%bx
	inc    %si
	mov    0x4(%bp),%ax
	mov    %ax,%cx
	mov    %cl,(%bx)
	mov    %si,(%di)
L_1e21:
	mov    -0x8(%bp),%ax
	cmp    -0x6(%bp),%ax
	je   L_1e32
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
L_1e30:
	jmp   L_1e35
L_1e32:
	mov    0x4(%bp),%ax
L_1e35:
	lea    -0x4(%bp),%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


fflush:
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
	jne   L_1e88
	mov    0x4(%di),%si
	or     %si,%si
	je   L_1e88
	mov    (%di),%ax
	sub    %si,%ax
	mov    %ax,-0x6(%bp)
	or     %ax,%ax
	jle   L_1e88
	mov    %si,(%di)
	movw   $0x200,0x2(%di)
	pushw  -0x6(%bp)
	push   %si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   write
	add    $0x6,%sp
	cmp    -0x6(%bp),%ax
	je   L_1e88
	orb    $0x20,0x6(%di)
	mov    $0xffff,%ax
	jmp   L_1e8a
L_1e88:
	xor    %ax,%ax
L_1e8a:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


__cleanu:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	/* TODO: data ref: mov    $0x2998,%di */
	mov    $D_0702,%di
L_1e97:
	/* TODO: data ref: cmp    0x2a38,%di */
	cmp    D_07a2,%di
	jae   L_1ea7
	push   %di
	call   fclose
	pop    %cx
	add    $0x8,%di
	jmp   L_1e97
L_1ea7:
	pop    %di
	pop    %si
	pop    %bp
	ret


fclose:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    $0xffff,%si
	mov    0x6(%di),%al
	cbtw
	test   $0x3,%ax
	je   L_1efe
	mov    0x6(%di),%al
	cbtw
	test   $0x40,%ax
	jne   L_1efe
	push   %di
	call   fflush
	pop    %cx
	mov    %ax,%si
	mov    0x7(%di),%al
	cbtw
	push   %ax
	call   close
	pop    %cx
	or     %ax,%ax
	jge   L_1ee0
	mov    $0xffff,%si
L_1ee0:
	mov    0x6(%di),%al
	cbtw
	test   $0x8,%ax
	je   L_1ef0
	pushw  0x4(%di)
	call   free
	pop    %cx
L_1ef0:
	mov    0x6(%di),%al
	cbtw
	test   $0xc,%ax
	je   L_1efe
	movw   $0x0,0x4(%di)
L_1efe:
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
	call   __cleanu
	pushw  0x4(%bp)
	call   exit_raw
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


malloc:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	push   %cx
	/* TODO: data ref: cmpw   $0x0,0x2a3a */
	cmpw   $0x0,D_07a4
	jne   L_1f4d
	/* TODO: data ref: mov    $0x2a3c,%ax */
	mov    $D_07a6,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x2a3a */
	mov    %ax,D_07a4
	/* TODO: data ref: mov    $0x2a3a,%ax */
	mov    $D_07a4,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    %ax,0x2a3c */
	mov    %ax,D_07a6
	/* TODO: data ref: movw   $0x2a3c,0x2e94 */
	/* dual-reloc split: movw $D_07a6,D_0bfe */
	mov	$D_07a6,%ax
	mov	%ax,D_0bfe
	/* TODO: data ref: movw   $0x2a3a,0x2e92 */
	/* dual-reloc split: movw $D_07a4,D_0bfc */
	mov	$D_07a4,%ax
	mov	%ax,D_0bfc
L_1f4d:
	mov    0x4(%bp),%ax
	add    $0x3,%ax
	xor    %dx,%dx
	mov    $0x2,%cx
	div    %cx
	mov    %ax,-0x6(%bp)
	/* TODO: data ref: mov    0x2e92,%di */
	mov    D_0bfc,%di
L_1f61:
	/* TODO: data ref: movw   $0x0,0x2a3e */
	movw   $0x0,D_07a8
L_1f67:
	mov    (%di),%ax
	test   $0x1,%ax
	jne   L_1f96
L_1f6e:
	mov    (%di),%si
	mov    (%si),%ax
	test   $0x1,%ax
	jne   L_1f7d
	mov    (%si),%ax
	mov    %ax,(%di)
	jmp   L_1f6e
L_1f7d:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %ax,%si
	jb   L_1f96
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	cmp    %di,%ax
	jb   L_1f96
	jmp   L_203d
L_1f96:
	mov    %di,%si
	mov    (%di),%ax
	and    $0xfffe,%ax
	mov    %ax,%di
	cmp    %si,%di
	ja   L_1f67
	/* TODO: data ref: cmp    0x2e94,%si */
	cmp    D_0bfe,%si
	jne   L_1faf
	/* TODO: data ref: cmp    $0x2a3a,%di */
	cmp    $D_07a4,%di
	je   L_1fb3
L_1faf:
	xor    %ax,%ax
	jmp   L_1feb
L_1fb3:
	/* TODO: data ref: incw   0x2a3e */
	incw   D_07a8
	/* TODO: data ref: cmpw   $0x1,0x2a3e */
	cmpw   $0x1,D_07a8
	jle   L_1f67
	mov    -0x6(%bp),%ax
	add    $0x200,%ax
	cwtd
	mov    $0x200,%cx
	idiv   %cx
	mov    $0x9,%cx
	shl    %cl,%ax
	/* TODO: data ref: mov    %ax,0x2a3e */
	mov    %ax,D_07a8
	xor    %ax,%ax
	push   %ax
	call   sbrk
	pop    %cx
	mov    %ax,%si
	/* TODO: data ref: mov    0x2a3e,%ax */
	mov    D_07a8,%ax
	shl    %ax
	add    %si,%ax
	add    $0x400,%ax
	cmp    %si,%ax
	jae   L_1fed
	xor    %ax,%ax
L_1feb:
	jmp   L_2000
L_1fed:
	/* TODO: data ref: mov    0x2a3e,%ax */
	mov    D_07a8,%ax
	shl    %ax
	push   %ax
	call   sbrk
	pop    %cx
	mov    %ax,%si
	cmp    $0xffff,%ax
	jne   L_2002
	xor    %ax,%ax
L_2000:
	jmp   L_2067
L_2002:
	/* TODO: data ref: mov    0x2e94,%bx */
	mov    D_0bfe,%bx
	mov    %si,(%bx)
	/* TODO: data ref: mov    0x2e94,%ax */
	mov    D_0bfe,%ax
	inc    %ax
	inc    %ax
	cmp    %ax,%si
	je   L_2020
	/* TODO: data ref: mov    0x2e94,%bx */
	mov    D_0bfe,%bx
	mov    (%bx),%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x2e94,%bx */
	mov    D_0bfe,%bx
	mov    %ax,(%bx)
L_2020:
	/* TODO: data ref: mov    0x2a3e,%ax */
	mov    D_07a8,%ax
	shl    %ax
	add    %si,%ax
	dec    %ax
	dec    %ax
	mov    %ax,(%si)
	/* TODO: data ref: mov    %ax,0x2e94 */
	mov    %ax,D_0bfe
	/* TODO: data ref: mov    $0x2a3a,%ax */
	mov    $D_07a4,%ax
	or     $0x1,%ax
	/* TODO: data ref: mov    0x2e94,%bx */
	mov    D_0bfe,%bx
	mov    %ax,(%bx)
	jmp   L_1f61
L_203d:
	mov    -0x6(%bp),%ax
	shl    %ax
	add    %di,%ax
	/* TODO: data ref: mov    %ax,0x2e92 */
	mov    %ax,D_0bfc
	cmp    %ax,%si
	jbe   L_205c
	/* TODO: data ref: mov    0x2e92,%bx */
	mov    D_0bfc,%bx
	mov    (%bx),%ax
	/* TODO: data ref: mov    %ax,0x2e96 */
	mov    %ax,D_0c00
	/* TODO: data ref: mov    0x2e92,%bx */
	mov    D_0bfc,%bx
	mov    (%di),%ax
	mov    %ax,(%bx)
L_205c:
	/* TODO: data ref: mov    0x2e92,%ax */
	mov    D_0bfc,%ax
	or     $0x1,%ax
	mov    %ax,(%di)
	lea    0x2(%di),%ax
L_2067:
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


free:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	mov    0x4(%bp),%di
	mov    %di,%si
	dec    %si
	dec    %si
	/* TODO: data ref: mov    %si,0x2e92 */
	mov    %si,D_0bfc
	mov    (%si),%ax
	and    $0xfffe,%ax
	mov    %ax,(%si)
	pop    %di
	pop    %si
	pop    %bp
	ret


realloc:
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
	call   free
	pop    %cx
L_20a1:
	mov    -0x2(%di),%ax
	sub    %di,%ax
	cwtd
	mov    $0x2,%cx
	idiv   %cx
	mov    %ax,-0xc(%bp)
	pushw  0x6(%bp)
	call   malloc
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
	/* TODO: data ref: mov    0x2e96,%ax */
	mov    D_0c00,%ax
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


brk:
	push   %bp
	mov    %sp,%bp
	mov	$17,%bx	/* sc: brk */
	mov    0x4(%bp),%ax
	/* v7: 17:brk */
	int	$0xf1
	jcxz   L_2147
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
	jmp   L_214e
L_2147:
	mov    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x2a40 */
	mov    %bx,D_07aa
L_214e:
	pop    %bp
	ret


sbrk:
	push   %bp
	mov    %sp,%bp
	mov	$17,%bx	/* sc: brk */
	mov    0x4(%bp),%ax
	/* TODO: data ref: add    0x2a40,%ax */
	add    D_07aa,%ax
	/* v7: 17:brk */
	int	$0xf1
	jcxz   L_2167
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
	jmp   L_2174
L_2167:
	/* TODO: data ref: mov    0x2a40,%bx */
	mov    D_07aa,%bx
	mov    %bx,%ax
	add    0x4(%bp),%bx
	/* TODO: data ref: mov    %bx,0x2a40 */
	mov    %bx,D_07aa
L_2174:
	pop    %bp
	ret


close:
	push   %bp
	mov    %sp,%bp
	mov	$6,%bx	/* sc: close */
	mov    0x4(%bp),%ax
	/* v7: 6:close */
	int	$0xf1
	jcxz   L_2187
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_2187:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


isatty:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	sub    $0x6,%sp
	lea    -0xa(%bp),%ax
	push   %ax
	pushw  0x4(%bp)
	call   gtty
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
	pop    %si
	pop    %bp
	ret
	/* .byte 0 (alignment) */


open:
	push   %bp
	mov    %sp,%bp
	mov	$5,%bx	/* sc: open */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 5:open */
	int	$0xf1
	jcxz   L_21c6
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_21c6:
	pop    %bp
	ret


read:
	push   %bp
	mov    %sp,%bp
	mov	$3,%bx	/* sc: read */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 3:read */
	int	$0xf1
	jcxz   L_21df
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_21df:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


stat:
	push   %bp
	mov    %sp,%bp
	mov	$18,%bx	/* sc: stat */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	/* v7: 18:stat */
	int	$0xf1
	jcxz   L_21f6
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_21f6:
	pop    %bp
	ret


stty:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7409,%ax
	push   %ax
	pushw  0x4(%bp)
	call   ioctl
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


gtty:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
	pushw  0x6(%bp)
	mov    $0x7408,%ax
	push   %ax
	pushw  0x4(%bp)
	call   ioctl
	add    $0x6,%sp
	pop    %di
	pop    %si
	pop    %bp
	ret


ioctl:
	push   %bp
	mov    %sp,%bp
	mov	$54,%bx	/* sc: ioctl */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 54:ioctl */
	int	$0xf1
	jcxz   L_2241
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_2241:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


wait:
	push   %bp
	mov    %sp,%bp
	mov	$7,%bx	/* sc: wait */
	/* v7: 7:wait */
	int	$0xf1
	jcxz   L_2254
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_2252:
	pop    %bp
	ret
L_2254:
	mov    0x4(%bp),%bx
	cmp    $0x0,%bx
	je   L_2252
	mov    %dx,(%bx)
	jmp   L_2252


write:
	push   %bp
	mov    %sp,%bp
	mov	$4,%bx	/* sc: write */
	mov    0x4(%bp),%ax
	mov    0x6(%bp),%dx
	mov    0x8(%bp),%cx
	/* v7: 4:write */
	int	$0xf1
	jcxz   L_2277
	/* TODO: data ref: mov    %cx,0x2a44 */
	mov    %cx,D_07ae
L_2277:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


modf:
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


	.data

	.byte	0x00, 0x00	/* |..| */
D_0002:
	.byte	0xbf, 0x0f	/* |..| */
D_0004:
	.ascii	"/tmp/abcd\0"
D_000e:
	.ascii	"pipe calls returned %d/%d/%d \0"
D_002c:
	.byte	0x28, 0x66, 0x64, 0x20, 0x25, 0x64, 0x2f, 0x25	/* |(fd %d/%| */
	.byte	0x64, 0x2c, 0x20, 0x66, 0x64, 0x32, 0x20, 0x25	/* |d, fd2 %| */
	.byte	0x64, 0x2f, 0x25, 0x64, 0x2c, 0x20, 0x66, 0x64	/* |d/%d, fd| */
	.byte	0x33, 0x20, 0x25, 0x64, 0x2f, 0x25, 0x64, 0x29	/* |3 %d/%d)| */
	.byte	0x0a, 0x00	/* |..| */
D_004e:
	.byte	0x64, 0x75, 0x70, 0x20, 0x25, 0x64, 0x3a, 0x25	/* |dup %d:%| */
	.byte	0x64, 0x2c, 0x20, 0x64, 0x75, 0x70, 0x32, 0x28	/* |d, dup2(| */
	.byte	0x25, 0x64, 0x2c, 0x25, 0x64, 0x29, 0x20, 0x72	/* |%d,%d) r| */
	.byte	0x65, 0x74, 0x75, 0x72, 0x6e, 0x65, 0x64, 0x20	/* |eturned | */
	.byte	0x25, 0x64, 0x0a, 0x00	/* |%d..| */
D_0072:
	.ascii	"time: ret %lu, parm %lu, %s %s\0"
D_0091:
	.byte	0x6f, 0x70, 0x65, 0x6e, 0x20, 0x25, 0x73, 0x20	/* |open %s | */
	.byte	0x6f, 0x6b, 0x0a, 0x00	/* |ok..| */
D_009d:
	.byte	0x25, 0x73, 0x3a, 0x20, 0x6d, 0x6f, 0x64, 0x65	/* |%s: mode| */
	.byte	0x20, 0x25, 0x6f, 0x2c, 0x20, 0x73, 0x69, 0x7a	/* | %o, siz| */
	.byte	0x65, 0x20, 0x25, 0x6c, 0x75, 0x2c, 0x20, 0x69	/* |e %lu, i| */
	.byte	0x6e, 0x6f, 0x20, 0x25, 0x64, 0x0a, 0x00	/* |no %d..| */
D_00bc:
	.byte	0x72, 0x65, 0x61, 0x64, 0x20, 0x25, 0x64, 0x20	/* |read %d | */
	.byte	0x62, 0x79, 0x74, 0x65, 0x73, 0x20, 0x66, 0x72	/* |bytes fr| */
	.byte	0x6f, 0x6d, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |om %s..| */
D_00d3:
	.byte	0x72, 0x65, 0x74, 0x75, 0x72, 0x6e, 0x20, 0x66	/* |return f| */
	.byte	0x72, 0x6f, 0x6d, 0x20, 0x6c, 0x73, 0x65, 0x65	/* |rom lsee| */
	.byte	0x6b, 0x20, 0x74, 0x6f, 0x20, 0x37, 0x30, 0x30	/* |k to 700| */
	.byte	0x30, 0x30, 0x3a, 0x20, 0x25, 0x6c, 0x75, 0x0a	/* |00: %lu.| */
	.byte	0x00	/* |.| */
D_00f4:
	.byte	0x43, 0x72, 0x65, 0x61, 0x74, 0x65, 0x64, 0x20	/* |Created | */
	.byte	0x25, 0x73, 0x0a, 0x00	/* |%s..| */
D_0100:
	.byte	0x77, 0x72, 0x6f, 0x74, 0x65, 0x20, 0x25, 0x64	/* |wrote %d| */
	.byte	0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x20, 0x74	/* | bytes t| */
	.byte	0x6f, 0x20, 0x25, 0x73, 0x0a, 0x00	/* |o %s..| */
D_0116:
	.ascii	"stat-2\0"
D_011d:
	.byte	0x25, 0x73, 0x20, 0x6e, 0x6f, 0x77, 0x20, 0x68	/* |%s now h| */
	.byte	0x61, 0x73, 0x20, 0x25, 0x6c, 0x64, 0x20, 0x62	/* |as %ld b| */
	.byte	0x79, 0x74, 0x65, 0x73, 0x0a, 0x00	/* |ytes..| */
D_0133:
	.byte	0x77, 0x72, 0x6f, 0x6e, 0x67, 0x20, 0x6f, 0x70	/* |wrong op| */
	.byte	0x74, 0x69, 0x6f, 0x6e, 0x3a, 0x20, 0x25, 0x73	/* |tion: %s| */
	.byte	0x0a, 0x00	/* |..| */
D_0145:
	.ascii	"/tmp/tmp/xxx\0"
D_0152:
	.byte	0x61, 0x00	/* |a.| */
D_0154:
	.byte	0x66, 0x6f, 0x70, 0x65, 0x6e, 0x20, 0x2f, 0x74	/* |fopen /t| */
	.byte	0x6d, 0x70, 0x2f, 0x74, 0x6d, 0x70, 0x2f, 0x78	/* |mp/tmp/x| */
	.byte	0x78, 0x78, 0x20, 0x72, 0x65, 0x74, 0x75, 0x72	/* |xx retur| */
	.byte	0x6e, 0x65, 0x64, 0x20, 0x25, 0x78, 0x0a, 0x00	/* |ned %x..| */
D_0174:
	.byte	0x54, 0x68, 0x69, 0x73, 0x20, 0x69, 0x73, 0x20	/* |This is | */
	.byte	0x63, 0x6f, 0x6d, 0x69, 0x6e, 0x67, 0x20, 0x74	/* |coming t| */
	.byte	0x68, 0x72, 0x6f, 0x75, 0x67, 0x68, 0x20, 0x74	/* |hrough t| */
	.byte	0x68, 0x65, 0x20, 0x70, 0x69, 0x70, 0x65, 0x21	/* |he pipe!| */
	.byte	0x0a, 0x00	/* |..| */
D_0196:
	.ascii	"fopen\0"
D_019c:
	.byte	0x63, 0x61, 0x74, 0x00	/* |cat.| */
D_01a0:
	.byte	0x77, 0x00	/* |w.| */
D_01a2:
	.byte	0x70, 0x6f, 0x70, 0x65, 0x6e, 0x20, 0x72, 0x65	/* |popen re| */
	.byte	0x74, 0x75, 0x72, 0x6e, 0x65, 0x64, 0x20, 0x25	/* |turned %| */
	.byte	0x78, 0x0a, 0x00	/* |x..| */
D_01b5:
	.ascii	"Popen failed: \0"
D_01c4:
	.byte	0x54, 0x68, 0x69, 0x73, 0x20, 0x69, 0x73, 0x20	/* |This is | */
	.byte	0x63, 0x6f, 0x6d, 0x69, 0x6e, 0x67, 0x20, 0x74	/* |coming t| */
	.byte	0x68, 0x72, 0x6f, 0x75, 0x67, 0x68, 0x20, 0x74	/* |hrough t| */
	.byte	0x68, 0x65, 0x20, 0x70, 0x69, 0x70, 0x65, 0x21	/* |he pipe!| */
	.byte	0x0a, 0x00	/* |..| */
D_01e6:
	.ascii	"cat /etc/motd\0"
D_01f4:
	.byte	0x72, 0x00	/* |r.| */
D_01f6:
	.byte	0x32, 0x6e, 0x64, 0x20, 0x70, 0x6f, 0x70, 0x65	/* |2nd pope| */
	.byte	0x6e, 0x20, 0x72, 0x65, 0x74, 0x75, 0x72, 0x6e	/* |n return| */
	.byte	0x65, 0x64, 0x20, 0x25, 0x78, 0x0a, 0x00	/* |ed %x..| */
D_020d:
	.ascii	"2nd popen failed: \0"
D_0220:
	.ascii	"Pipe read: \0"
D_022c:
	.byte	0x25, 0x73, 0x00	/* |%s.| */
D_022f:
	.byte	0x63, 0x75, 0x72, 0x65, 0x6e, 0x64, 0x3a, 0x20	/* |curend: | */
	.byte	0x25, 0x64, 0x0a, 0x00	/* |%d..| */
D_023b:
	.byte	0x6e, 0x65, 0x77, 0x20, 0x63, 0x75, 0x72, 0x65	/* |new cure| */
	.byte	0x6e, 0x64, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0x00	/* |nd: %d..| */
D_024b:
	.byte	0x70, 0x70, 0x20, 0x69, 0x73, 0x20, 0x61, 0x74	/* |pp is at| */
	.byte	0x20, 0x25, 0x78, 0x2c, 0x20, 0x63, 0x75, 0x72	/* | %x, cur| */
	.byte	0x65, 0x6e, 0x64, 0x20, 0x25, 0x64, 0x0a, 0x00	/* |end %d..| */
D_0263:
	.byte	0x73, 0x62, 0x72, 0x6b, 0x3a, 0x20, 0x25, 0x78	/* |sbrk: %x| */
	.byte	0x2c, 0x20, 0x6d, 0x61, 0x6c, 0x6c, 0x6f, 0x63	/* |, malloc| */
	.byte	0x3a, 0x20, 0x25, 0x30, 0x34, 0x78, 0x2c, 0x20	/* |: %04x, | */
	.byte	0x64, 0x69, 0x66, 0x66, 0x20, 0x25, 0x64, 0x0a	/* |diff %d.| */
	.byte	0x00	/* |.| */
D_0284:
	.byte	0x6d, 0x61, 0x6c, 0x6c, 0x6f, 0x63, 0x20, 0x65	/* |malloc e| */
	.byte	0x78, 0x69, 0x74, 0x20, 0x25, 0x64, 0x2c, 0x20	/* |xit %d, | */
	.byte	0x74, 0x6f, 0x74, 0x61, 0x6c, 0x20, 0x61, 0x6c	/* |total al| */
	.byte	0x6c, 0x6f, 0x63, 0x20, 0x25, 0x75, 0x20, 0x28	/* |loc %u (| */
	.byte	0x25, 0x75, 0x29, 0x0a, 0x00, 0x00	/* |%u)...| */
D_02aa:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
D_02c4:
	.byte	0x1f, 0x00	/* |..| */
D_02c6:
	.byte	0x1c, 0x00, 0x1f, 0x00, 0x1e, 0x00, 0x1f, 0x00	/* |........| */
	.byte	0x1e, 0x00, 0x1f, 0x00, 0x1f, 0x00, 0x1e, 0x00	/* |........| */
	.byte	0x1f, 0x00, 0x1e, 0x00, 0x1f, 0x00	/* |......| */
D_02dc:
	.byte	0x05, 0x00	/* |..| */
D_02de:
	.byte	0x4d, 0x01, 0x3a, 0x00, 0x2f, 0x01	/* |M.:./.| */
D_02e4:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00	/* |..| */
D_02ee:
	.byte	0x00, 0x00	/* |..| */
D_02f0:
	.byte	0x00, 0x00	/* |..| */
D_02f2:
	.byte	0x00, 0x00	/* |..| */
D_02f4:
	.byte	0x00, 0x00	/* |..| */
D_02f6:
	.byte	0x44, 0x61, 0x79, 0x20, 0x4d, 0x6f, 0x6e, 0x20	/* |Day Mon | */
	.byte	0x30, 0x30, 0x20, 0x30, 0x30, 0x3a, 0x30, 0x30	/* |00 00:00| */
	.byte	0x3a, 0x30, 0x30, 0x20, 0x31, 0x39, 0x30, 0x30	/* |:00 1900| */
	.byte	0x0a, 0x00	/* |..| */
D_0310:
	.ascii	"SunMonTueWedThuFriSat\0"
D_0326:
	.ascii	"JanFebMarAprMayJunJulAugSepOctNovDec\0"
	.byte	0x00	/* |.| */
D_034c:
	.byte	0x00, 0x00	/* |..| */
D_034e:
	.ascii	"Unknown error\0"
D_035c:
	.byte	0x3a, 0x20, 0x00	/* |: .| */
D_035f:
	.byte	0x0a, 0x00, 0x00	/* |...| */
D_0362:
	.word	D_03aa	/* ptr: 0x2640 */
	.word	D_03b2	/* ptr: 0x2648 */
	.word	D_03bc	/* ptr: 0x2652 */
	.word	D_03d6	/* ptr: 0x266c */
	.word	D_03e6	/* ptr: 0x267c */
	.word	D_03fe	/* ptr: 0x2694 */
	.word	D_0408	/* ptr: 0x269e */
	.word	D_0422	/* ptr: 0x26b8 */
	.word	D_0434	/* ptr: 0x26ca */
	.word	D_0446	/* ptr: 0x26dc */
	.word	D_0456	/* ptr: 0x26ec */
	.word	D_0462	/* ptr: 0x26f8 */
	.word	D_0474	/* ptr: 0x270a */
	.word	D_0484	/* ptr: 0x271a */
	.word	D_0496	/* ptr: 0x272c */
	.word	D_04a2	/* ptr: 0x2738 */
	.word	D_04b8	/* ptr: 0x274e */
	.word	D_04ca	/* ptr: 0x2760 */
	.word	D_04d6	/* ptr: 0x276c */
	.word	D_04e8	/* ptr: 0x277e */
	.word	D_04f7	/* ptr: 0x278d */
	.word	D_0507	/* ptr: 0x279d */
	.word	D_0516	/* ptr: 0x27ac */
	.word	D_0527	/* ptr: 0x27bd */
	.word	D_053b	/* ptr: 0x27d1 */
	.word	D_054f	/* ptr: 0x27e5 */
	.word	D_0560	/* ptr: 0x27f6 */
	.word	D_056f	/* ptr: 0x2805 */
	.byte	0x14, 0x28, 0x2c, 0x28, 0x39, 0x28, 0x4f, 0x28	/* |.(,(9(O(| */
	.byte	0x5e, 0x28, 0x6a, 0x28, 0x7d, 0x28	/* |^(j(}(| */
D_03a8:
	.byte	0x23, 0x00	/* |#.| */
D_03aa:
	.ascii	"Error 0\0"
D_03b2:
	.ascii	"Not owner\0"
D_03bc:
	.ascii	"No such file or directory\0"
D_03d6:
	.ascii	"No such process\0"
D_03e6:
	.ascii	"Interrupted system call\0"
D_03fe:
	.ascii	"I/O error\0"
D_0408:
	.ascii	"No such device or address\0"
D_0422:
	.ascii	"Arg list too long\0"
D_0434:
	.ascii	"Exec format error\0"
D_0446:
	.ascii	"Bad file number\0"
D_0456:
	.ascii	"No children\0"
D_0462:
	.ascii	"No more processes\0"
D_0474:
	.byte	0x4e, 0x6f, 0x74, 0x20, 0x65, 0x6e	/* |Not en| */
D_047a:
	.ascii	"ough core\0"
D_0484:
	.ascii	"Permission denied\0"
D_0496:
	.ascii	"Bad address\0"
D_04a2:
	.ascii	"Block device required\0"
D_04b8:
	.ascii	"Mount device busy\0"
D_04ca:
	.ascii	"File exists\0"
D_04d6:
	.ascii	"Cross-device link\0"
D_04e8:
	.ascii	"No such device\0"
D_04f7:
	.ascii	"Not a directory\0"
D_0507:
	.ascii	"Is a directory\0"
D_0516:
	.ascii	"Invalid argument\0"
D_0527:
	.ascii	"File table overflow\0"
D_053b:
	.ascii	"Too many open files\0"
D_054f:
	.ascii	"Not a typewriter\0"
D_0560:
	.ascii	"Text file busy\0"
D_056f:
	.ascii	"File too large\0"
	.ascii	"No space left on device\0"
	.ascii	"Illegal seek\0"
	.ascii	"Read-only file system\0"
	.ascii	"Too many links\0"
	.ascii	"Broken pipe\0"
	.ascii	"Argument too large\0"
	.ascii	"Result too large\0"
D_05f8:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_0620:
	.ascii	"/bin/sh\0"
D_0628:
	.byte	0x73, 0x68	/* |sh| */
	.byte	0x00	/* |.| */
D_062b:
	.byte	0x2d, 0x63, 0x00	/* |-c.| */
D_062e:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00	/* |....| */
D_0642:
	.ascii	"(null)\0"
	.byte	0x00	/* |.| */
D_064a:
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
D_0699:
	.byte	0x00	/* |.| */
D_069a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_06a2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_06aa:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_06b2:
	.byte	0x2c, 0x43, 0x1c, 0xeb, 0xe2, 0x36, 0x1a, 0x3f	/* |,C...6.?| */
D_06ba:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_06c2:
	.byte	0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0xb9, 0x3f	/* |.......?| */
D_06ca:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_06d2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_06da:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x8f, 0x40	/* |.....@.@| */
D_06e2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_06ea:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x40	/* |......$@| */
D_06f2:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f	/* |.......?| */
D_06fa:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40	/* |.......@| */
D_0702:
	.word	D_09fc	/* ptr: 0x2c92 */
	.byte	0x00, 0x00	/* |..| */
	.word	D_09fc	/* ptr: 0x2c92 */
	.byte	0x01, 0x00	/* |..| */
D_070a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |......| */
D_0710:
	.byte	0x02	/* |.| */
D_0711:
	.byte	0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06	/* |........| */
	.byte	0x02	/* |.| */
D_071a:
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	/* |........| */
D_07a2:
	.word	D_07a2	/* ptr: 0x2a38 */
D_07a4:
	.byte	0x00, 0x00	/* |..| */
D_07a6:
	.byte	0x00, 0x00	/* |..| */
D_07a8:
	.byte	0x00, 0x00	/* |..| */
D_07aa:
	.word	0x0c20	/* _curbrk: bss_end=0x0c02 rounded to paragraph */

	.bss

D_07ac:	.space	2	/* bss word @ 0x07ac */
D_07ae:	.space	2	/* bss word @ 0x07ae */
	.space	4
D_07b4:	.space	2	/* bss word @ 0x07b4 */
D_07b6:	.space	2	/* bss word @ 0x07b6 */
D_07b8:	.space	2	/* bss word @ 0x07b8 */
D_07ba:	.space	2	/* bss word @ 0x07ba */
D_07bc:	.space	2	/* bss word @ 0x07bc */
D_07be:	.space	2	/* bss word @ 0x07be */
D_07c0:	.space	2	/* bss word @ 0x07c0 */
D_07c2:	.space	2	/* bss word @ 0x07c2 */
D_07c4:	.space	2	/* bss word @ 0x07c4 */
D_07c6:	.space	2	/* bss word @ 0x07c6 */
D_07c8:	.space	2	/* bss word @ 0x07c8 */
D_07ca:	.space	2	/* bss word @ 0x07ca */
D_07cc:	.space	2	/* bss word @ 0x07cc */
D_07ce:	.space	2	/* bss word @ 0x07ce */
D_07d0:	.space	2	/* bss word @ 0x07d0 */
D_07d2:	.space	2	/* bss word @ 0x07d2 */
D_07d4:	.space	2	/* bss word @ 0x07d4 */
D_07d6:	.space	2	/* bss word @ 0x07d6 */
D_07d8:	.space	2	/* bss word @ 0x07d8 */
D_07da:	.space	2	/* bss word @ 0x07da */
D_07dc:	.space	2	/* bss word @ 0x07dc */
D_07de:	.space	2	/* bss word @ 0x07de */
D_07e0:	.space	2	/* bss word @ 0x07e0 */
D_07e2:	.space	2	/* bss word @ 0x07e2 */
D_07e4:	.space	2	/* bss word @ 0x07e4 */
D_07e6:	.space	2	/* bss word @ 0x07e6 */
D_07e8:	.space	2	/* bss word @ 0x07e8 */
D_07ea:	.space	2	/* bss word @ 0x07ea */
D_07ec:	.space	2	/* bss word @ 0x07ec */
D_07ee:	.space	2	/* bss word @ 0x07ee */
D_07f0:	.space	2	/* bss word @ 0x07f0 */
D_07f2:	.space	2	/* bss word @ 0x07f2 */
D_07f4:	.space	2	/* bss word @ 0x07f4 */
	.space	6
D_07fc:	.space	2	/* bss word @ 0x07fc */
	.space	510
D_09fc:	.space	2	/* bss word @ 0x09fc */
	.space	510
D_0bfc:	.space	2	/* bss word @ 0x0bfc */
D_0bfe:	.space	2	/* bss word @ 0x0bfe */
D_0c00:	.space	2	/* bss word @ 0x0c00 */

/* end of file */
