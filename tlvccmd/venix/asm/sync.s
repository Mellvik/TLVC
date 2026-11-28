/* sync.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 sync.s -o sync.o
 */

	.code16
	.include "venix_syscalls.inc"
	.text
	/* crt0 provided by TLVC crt0.o */

// Venix/86 a.out: sync  [linked executable]
//   Magic:    0x0107  OMAGIC (object/impure)
//   a_stack:  0x2000  (8192 bytes)
//   a_text:   0x0000008a  (138 bytes)
//   a_data:   0x00000004  (4 bytes)
//   Load:  cs:0x0000 (text)   ds:0x208a (data/bss)

	.global	main
	.type	main,@function
main:
	push   %bp
	mov    %sp,%bp
	push   %si
	push   %di
L_0042:
	call   L_0050
	xor    %ax,%ax
	push   %ax
	call   exit
	pop    %cx
	pop    %di
	pop    %si
	pop    %bp
	ret


L_0050:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall sync(36) */
	/* sync -> __NR_sync */
	mov	$__NR_sync,%ax
	int	$0x80
	/* errno: jcxz -> js (negative ax = error) */
	js     L_005e
	neg	%ax
	mov	%ax,D_2090	/* errno = -ax */
L_005e:
	pop    %bp
	ret

	/* exit: provided by TLVC libc — elided */

L_0074:
	ret
	/* .byte 0 (alignment) */

	.global _exit
_exit:
	mov	$__NR_exit,%ax
	int	$0x80
	/* no return */


L_0076:
	push   %bp
	mov    %sp,%bp
	/* Venix syscall exit(1) */
	mov    0x4(%bp),%ax
	/* exit -> __NR_exit */
	xchg	%ax,%bx
	mov	$__NR_exit,%ax
	int	$0x80
	/* result in %ax; negative = errno */
	/* errno: jcxz -> js (negative ax = error) */
	js     L_0087
	neg	%ax
	mov	%ax,D_2090	/* errno = -ax */
L_0087:
	pop    %bp
	ret
	/* .byte 0 (alignment) */


	.data

	.byte	0x00, 0x00	/* |..| */
D_208c:
	.byte	0xbf, 0x0f	/* |..| */

	.bss

	.space	2
D_2090:	.space	2	/* bss word @ 0x2090 */

/* end of file */
