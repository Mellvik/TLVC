#include <linuxmt/config.h>
#include <linuxmt/limits.h>
#include <linuxmt/trace.h>
#include <arch/asm-offsets.h>
#include <arch/ports.h>
#include <arch/irq.h>

        .arch   i8086, nojumps
        .code16
        .text

//------------------------------------------------------------------------------
//
//      IRQ and IRQ return paths for Linux 8086
//
// The execution thread will not return from the interrupt vector trampoline.
// Instead, the address pushed in the stack will be used to get
// the interrupt number.

/*
!
!       On entry CS:IP is all we can trust
!
!       There are three possible cases to cope with
!
!       Syscall or Interrupted user mode (intr_count == 0)
!               Switch to process's kernel stack
!               Optionally, check (SS == current->t_regs.ss)
!               and panic on failure
!               On return, task switch allowed
!
!       Interrupted kernel mode, interrupted kernel task
!               or second interrupt (intr_count == 1)
!               Switch to interrupt stack
!               On return, no task switch allowed
!
!       Interrupted interrupt service routine (intr_count > 1)
!               Already using interrupt stack, keep using it
!               On return, no task switch allowed
!
!       We do all of this to avoid per process interrupt stacks and
!       related nonsense. This way we need only one dedicated int stack
!
*/
        .extern do_signal
        .extern do_IRQ
        .extern syscall
        .extern trace_begin
        .extern trace_end
        .extern tracing
        .extern check_ustack
        .extern check_istack
        .extern bh_active
        .extern do_bottom_half
        .extern schedule
        .extern panic
	.extern printk	// for debugging

/*
 * June 2026: Optional compatibility syscall layer for Venix 2.1/86 binaries 
 * (Unix V7). In general the system calls are
 * Linux compatible, using IRQ vector 0xF1 instead of our 0x80, and swapping
 * registers AX and BX for syscall # and parm 1 respectively. CX and DX are also
 * swapped while DI and SI are left alone as unsafe to use for syscalls.
 *
 * For most system calls we simply swap 2x2 registers and go, but there are
 * important exceptions. Notably the stat structure is slightly different
 * (inode_t is u_16 on venix) affecting stat/fstat. While TLVC never returns
 * longs in registers (e.g lseek), Venix has many which does. The upper nibble
 * of t_mode in struct stat is different (also stat/fstat). Syscall details in Wiki.
 * Finally, Venix returns result or -1 in ax, while CX holds the error # if any.
 * CX equals zero if all is good (creative use of the jcxz instruction). All 
 * this and more is handled here.
 * 
 * A V7 syscall sets a flag in a (normally) unused variable in the task struct,
 * which informs the syscall routine to do whatever is needed. This flag may
 * be cleared in cases like nested syscalls, and not reliable for proper handling
 * of a syscall return. An extra flag added to the syscall number in AX before
 * pushing it on the stack guarantees correct exit handling.
 */

#ifdef CONFIG_COMPAT_V7
	.global _irqit_v7	/* int F1 */
	.global _sysfpu_v7	/* int F4 */
	.global _abort_v7	/* int F3 */
	.global _stktrap_v7	/* int F2 */
	.global _sysmap_v7	/* int F5 */

_sysfpu_v7:
	// FPU is required, just return
	//mov	$sysfpu_msg,%ax
	//call	_intmsg
_sysret:
	add	$4,%sp	// skip dynamic handler return stack
	iret
	
_intmsg:
	// entered with string ptr in AX
	push	%ds
	push	%si
        mov     %sp,%si
        mov     %ss:8(%si),%ds
	push	%ax
	call	printk
	pop	%ax
	pop	%si
	pop	%ds
	ret

_stktrap_v7:	/* Stack trap INT, may need a more elaborate message */
	mov	$stktrap_msg,%ax
	call	_intmsg
	jmp	_sysret

_sysmap_v7:
	// codemap not implemented, this is fatal
	mov	$codemap_msg,%ax
	call	_intmsg
	add	$2,%sp
	// just drop into _abort

_abort_v7:
	push	%ds
        mov     %sp,%si
        mov     %ss:4(%si),%ds	// Get kernel DS set

	mov	current,%bx;
	mov	$6,%ax
	push	%ax		// SIGABRT/SIGIOT
	push	2(%bx)		// pid
	
	call	sys_kill
	add	$4,%sp
	pop	%ds
	ret

//---------------- start syscall handling -----------------------------

_irqit_v7:			// Handle Venix/V7 syscall
	xchg	%ax,%bx		// rearrange args
	xchg	%cx,%dx
	cmp	$0x13,%al	// lseek needs special treatment to get
				// a long into the arg list...
	jnz	1f
				// TODO: maybe test (%bh != 0) in which case
				// fd is bogus, just let is pass
	push	%dx
	mov	%si,%dx		// whence is in %si
	mov	%dl,%bh		// whence to high byte of fd
	//or	$0x80,%bh	// add flag, to be 'converted src compatible'
				// ... for now
	pop	%dx
1:
	push    %ds
	push    %si
	push    %di

        mov     %sp,%si
        mov     %ss:8(%si),%ds		// Get kernel DS set so we can do our thing

	push	%bx
	mov	current,%bx
	mov	%ax,TASK_USER_V7(%bx)	// set the task_V7 flag, used by the syscalls to adapt
					// behaviour. Any non-zero number will do, but
					// using the syscall # allows for some useful
					// tracing/debugging.

	inc	%ah			// Additional 'protection': set a 'local' V7 return flag.
					// The task_V7 flag may get turned off during syscall
					// processing, IOW not a reliable trigger for correct
					// syscall exit processing.
	pop	%bx
	jmp	_irqit_cont
#endif	/* CONFIG_COMPAT_V7 */

        .global _irqit
_irqit:
//
//      Make room
//
        push    %ds
        push    %si
        push    %di
//
//      Recover kernel data segment
//      Was pushed by the CALLF of the dynamic handler
//      TODO: BP is better for stack work
//
        mov     %sp,%si
        mov     %ss:8(%si),%ds
#ifdef CONFIG_COMPAT_V7
	/* give native syscalls a guaranteed clean slate */
	push    %bx
	mov     current,%bx
	movw    $0,TASK_USER_V7(%bx)
	pop	%bx

#endif
//
//      Determine which stack to use
//
_irqit_cont:
        cmpw    $1,intr_count
        jc      utask           // We were in user mode
        jz      itask           // Using a process's kernel stack
ktask:                          // Already using interrupt stack
//
//      Already using interrupt stack, keep using it
//
        sub     $8,%si          // 14 offsets less 6 already on stack
        jmp     save_regs
//
//      Using a process's kernel stack, switch to interrupt stack
//
itask:
        mov     $istack-14,%si  // 14 offsets 0-13 of SI below
        jmp     save_regs
//
//      User mode case
//
utask:
        mov     current,%si
#ifdef CHECK_SS
//
//      We were in user mode, first confirm
//      FIXME: won't work - panic requires valid SS and SS == DS!
//
        mov     %ss,%di
        cmp     TASK_USER_SS(%si),%di // entry SS = current->t_regs.ss?
        je      utask1                // User using the right stack
        mov     $ssmsg,%ax            // INVALID SS - Corrupted user stack segment
        push    %ax
        call    panic
utask1:
#endif
//
//      Switch to kernel stack
//
        add     $TASK_USER_DI,%si
//
//      Save segment, index, BP and SP registers
//
save_regs:
        incw    intr_count
        pop     (%si)           // DI
        pop     2(%si)          // SI
        pop     8(%si)          // DS
        pop     %di             // Return offset is actually a pointer to the IRQ number
        pop     %ds             // Return segment of the dynamic handler = kernel DS
        push    %bp             // BP
        mov     %sp,10(%si)     // SP
        mov     %ss,12(%si)     // SS
        mov     %es,6(%si)      // ES
        mov     %ax,4(%si)      // orig_ax
//
//      Load new segment and SP registers
//
        mov     %si,%sp
        mov     %ds,%si
        mov     %si,%ss
        mov     %si,%es
//
//      Save remaining registers
//
        push    %dx             // DX
        push    %cx             // CX
        push    %bx             // BX
        push    %ax             // AX
//
//      ds:[di] has IRQ number
//
        movb    (%di),%al
        cmpb    $IDX_SYSCALL,%al
#ifdef CONFIG_COMPAT_V7
	je	do_syscall
	cmpb	$IDX_SYSCALL_V7,%al
#endif
	jne	doirq
/*
!
!       ----------PROCESS SYSCALL----------
*/
do_syscall:
        sti
        call    check_ustack    // Check user mode stack

#ifdef CONFIG_TRACE
        call    trace_begin
#endif

        pop     %ax             // get syscall function code in AX
#if 0				/* DEBUG message: syscall entry */
	push	%ax
	mov	$callno,%bx
	push	%ax
	push	%bx
	call	printk
	add	$4,%sp
	pop	%ax
#endif
	xor	%ah,%ah		// discard V7 flag, it's on the stack anyway
        call    syscall
        push    %ax             // syscall return value in ax
	push	%dx		// for V7 COMPAT, return longs
#ifdef CONFIG_COMPAT_V7
	//mov	%sp,%bx
	//test	$0x100,12(%bx)
	//jnz	restore_regs_V7	
	mov	%sp,%bx
	mov	14(%bx),%cx	// syscall #
	push	%cx
#if 0		/* DEBUG: syscall exit */
	mov	$callno1,%ax
	mov	current,%bx
	push	TASK_USER_V7(%bx)	// V7 flag
	push	%cx		// syscall #
	push	%ax		// format string
	call	printk
	add	$6,%sp
#endif

	// update the V7 flag if needed, it may have been cleared
	// during syscall execution...
	pop %cx
#if 0	/* probably not needed --------------- */
	mov current,%bx
	test $1,%ch
	jz 1f
	cmp $0,TASK_USER_V7(%bx)	// insurance
	jnz 1f
	xor %ch,%ch
	mov %cx,TASK_USER_V7(%bx)
1:
#endif
#endif

#ifdef CONFIG_TRACE
        // strace.c must be compiled with tail optimization off to protect top of stack
        call    trace_end       // syscall return value is top of stack
#endif
        cmpw    $0,bh_active    // Any active bottom halfs?
        je      1f              // No
        call    do_bottom_half  // Run bottom halves
        //call schedule         // experimental

1:	call    do_signal       // process signals
        cli
	pop	%dx
        jmp     restore_regs
//
//      Done.
//

/*
!
!       ----------PROCESS INTERRUPT----------
*/
doirq:
//
//      Call the C code
//
#ifndef CONFIG_ARCH_SWAN
        sti                     // Reenable interrupts
#endif
        mov     %sp,%bx         // Get pointer to pt_regs
        cbw
        push    %ax             // IRQ for later

        push    %bx             // Register base
        push    %ax             // IRQ number
        call    do_IRQ          // Do the work
        pop     %ax             // Clean parameters
        pop     %bx

        pop     %ax             // Saved IRQ
        cli                     // Disable interrupts to avoid reentering ISR

//
//      Determine if hardware or software (trap) interrupt
//
#if defined(CONFIG_ARCH_IBMPC) || defined(CONFIG_ARCH_PC98)
        cmp     $16,%ax
        jge     was_trap        // Traps need no reset

#if defined(CONFIG_TIMER_INT0F) || defined(CONFIG_TIMER_INT1C)
        cmp     $7,%ax          // INT 0F (and INT 1C) also no reset
        jz      was_trap
#endif

#if defined(CONFIG_BLK_DEV_BFD) && !defined(CONFIG_ARCH_PC98)
        or      %ax,%ax         // Is int #0?
        jnz     do_eoi
//
//      Call original BIOS IRQ 0 (timer) ~55ms for floppy motor control
//
        decw    bios_call_cnt   // Will call bios int?
        jne     do_eoi
        movw    $5,bios_call_cnt
        pushf
        lcall   *org_irq0
        jmp     was_trap        // EOI already sent by bios int
#endif

do_eoi: cmp     $8,%ax          // Send EOI to interrupt controller(s)
        mov     $0x20,%al       // EOI
        jb      a6              // IRQ on low chip
//
//      Reset secondary 8259 if we have taken an AT rather
//      than XT irq. We also have to prod the primary controller EOI..
//
        out     %al,$PIC2_CMD   // Ack on secondary controller
        jmp     a5
a5:     jmp     a6
a6:     out     %al,$PIC1_CMD   // Ack on primary controller

#elif defined(CONFIG_ARCH_8018X)
        cmp     $16,%ax         // Determine if trap or interrupt
        jge     was_trap        // Traps need no reset
        mov $0x8000, %ax        // set the NSPEC bit on the
        mov $0xff02, %dx        // EOI register so the ICU
        out %ax, %dx            // acks the highest priority interrupt

#elif defined(CONFIG_ARCH_NECV25)
        cmp     $16,%ax         // Determine if trap or interrupt
        jge     was_trap        // Traps need no reset
        .word   0x920f          // NEC V25 specific FINT (End Of Interrupt) instruction

#elif defined(CONFIG_ARCH_SOLO86)
        mov $0x1A, %dx          // Send INT done
        out %al, %dx
#endif

was_trap:
//
//      Check if bottom halves can be run or rescheduling can occur
//

//  At this time after processing the hardware interrupt, the following actions
//  can be taken, depending on the kernel state described by intr_count:
//  intr_count = 0  not possible
//  intr_count = 1  interrupted user code (using process's kernel stack)
//      Action: Run bottom halves, schedule and do_signal.
//  intr_count = 2  interrupted kernel code or idle task (using interrupt stack)
//      Action: Run bottom halves, no rescheduling.
//  intr_count > 2  interrupted handler code (using interrupt stack)
//      Action: let interrupt stack unwind by returning immediately.
//
//  Hardware interrupts of kernel or idle code have to use the interrupt stack,
//  which means that calling schedule() cannot be allowed. After possibly running
//  BHs on the kernel's user process or interrupt stack, we will return to user mode,
//  or switch back to the interrupted kernel user process or idle task stack.
//  Interrupted kernel code will be resumed with no reschedling, and in the case
//  of idle task interruption, rescheduling will occur by the idle task when resumed.

        cmp     $2,intr_count   // Interrupting user or non-handler kernel code?
        ja      restore_regs    // No, interrupting handler code
        jne     1f              // Yes, interrupting user code
//
// Interrupted kernel or idle task code (intr_count = 2), run BHs and return
//
        cmpw    $0,bh_active    // Any active bottom halfs?
        je      3f              // No, return
        sti
        call    do_bottom_half  // Run bottom halves
        cli
        jmp     3f              // Return
//
// Interrupted user code (intr_count = 1), run BHs and reschedule
//
1:      cmpw    $0,bh_active    // Any active bottom halfs?
        je      2f              // No
        sti
        call    do_bottom_half  // Run bottom halves
2:      sti                     // Interrupts enabled while reschedule or signal checking
        call    schedule        // Task switch
        call    do_signal       // Check signals
        cli
3:

#ifdef CHECK_ISTACK
        mov     tracing,%ax
        and     $TRACE_ISTACK,%ax
        jz      4f
        sti
        call    check_istack    // Determine max interrupt stack used
        cli
4:
#endif

//
//      Restore registers and return
//
restore_regs:
        decw    intr_count
        pop     %ax

#ifdef CONFIG_COMPAT_V7
//
// Some V7 syscalls return a 'long' in AX:DX. if the return value is a real 32bit value
// and not simply two ints like in pipe() and wait(), we need to check whether
// we're dealing with a valid return or an error code. The test is DX == 0xffff.
// If true, we're quite (although not completely) sure it's an error code. The exception
// (so far) is time(): If (when) we get REALLY close to the end of time :-), that is,
// less than 10 hrs (the upper bit of ax must be set in order for the value to qualify
// as an error), all time() returns will be considered errors. I can live with that.
//
	mov	%sp,%bx
	//testw	$0x100,10(%bx)		// check original AX for V7 flag
	//jz	normal_restore
	//mov	10(%si),%bx
	mov	current,%bx
	cmp	$0,TASK_USER_V7(%bx)
	je	normal_restore
	mov	TASK_USER_V7(%bx),%cx	// has syscall #
	add	$6,%sp			// discard BX, CX, DX
	test	$0x100,%cx		// test the 'long return' bit
	mov	$0,%cx			// CX is now the error flag
	jz	1f			// regular error handling
	cmp	$0xffff,%dx		// long error code ??
	jnz	skip_pops		// dx=-1: normal ax error processing
1:
	or	%ax,%ax
	jge	skip_pops		// No error
long_err:
	neg	%ax
	mov	%ax,%cx			// CX goes to 'errno' in the app
	mov	$-1,%ax			// app level error indicator
skip_pops:
	pop	%di
	pop	%si			// CLEAN UP THIS
	jmp	s_pops
normal_restore:
#endif	/* Normal register restore */
	pop	%bx
	pop	%cx
        pop     %dx

        pop     %di
        pop     %si
s_pops:
        pop     %bp             // discard orig_AX
        pop     %es
        pop     %ds
        pop     %bp             // SP
        pop     %ss
        mov     %bp,%sp
        pop     %bp             // user BP
//
//      Iret restores CS:IP and F (thus including the interrupt bit)
//
        iret

/*
 *      ret_from_sys_call()
 *
 *      Called by run_init_process after sys_execve for the init task /bin/init.
 *      The stack is previously setup by kfork_proc(init_task) and arch_build_stack.
 *      Also called by arch_build_stack via do_fork when a new child process starts
 *      execution to exit the kernel to user mode.
 *
 *      ret_from_sys_call() switches to the kernel stack specified by 'current' and
 *      returns into user mode with AX=0. It does not return to the caller.
 */

        .global ret_from_syscall
ret_from_syscall:
        mov     current,%bx           // Ensure we have the
        lea     TASK_USER_BX(%bx),%sp // right kernel SP
        xor     %ax,%ax               // Just in case we are starting a new task
        push    %ax
        cli
        jmp     restore_regs

/*
 *      tswitch()
 *
 *      This function can only be called with SS=DS=kernel DS and
 *      CS=kernel CS. SS:SP is the relevant kernel stack. Thus we don't need
 *      to arse about with segment registers. The kernel isn't relocating.
 *
 *      tswitch() saves the "previous" task registers and state. It in effect
 *      freezes a copy of the caller context. Then restores the "current"
 *      context and returns running the current task.
 */

        .global tswitch
tswitch:
        push    %bp             // schedule()'s bp
        push    %es             // required for gcc-ia16
        push    %di
        push    %si
        mov     previous,%bx
        mov     %sp,TASK_KRNL_SP(%bx)
        mov     current,%bx
        mov     TASK_KRNL_SP(%bx),%sp
        pop     %si
        pop     %di
        pop     %es
        pop     %bp             // BP of schedule()
        ret

// short *getsp(void) - get stack pointer
        .global getsp
getsp:
        mov     %sp,%ax
        ret

// Halt - wait for next interrupt to save CPU power
        .global idle_halt
idle_halt:
        hlt
        ret

//------------------------------------------------------------------------------
// Save BIOS IRQ 0 timer vector
// void save_timer_irq(void)
//------------------------------------------------------------------------------

        .global save_timer_irq
save_timer_irq:
        push    %es
        xor     %ax,%ax
        mov     %ax,%es         // ES -> interrupt table
        mov     $8*4,%bx        // INT 8 (IRQ 0) vector
        mov     %es:(%bx),%ax   // get the old timer intr
        mov     %ax,org_irq0
        mov     %es:2(%bx),%ax
        mov     %ax,org_irq0+2
        pop     %es
        ret

//------------------------------------------------------------------------------
// Set interrupt vector
//------------------------------------------------------------------------------
// void int_vector_set (int vect, int_proc proc, int seg);
// arg1: vector number (byte pushed as word by the C compiler)
// arg2: function offset (word)
// arg3: function segment (word)

        .global int_vector_set
int_vector_set:
        mov %sp,%bx
        mov 6(%bx),%dx  // arg3
        mov 4(%bx),%cx  // arg2
        mov 2(%bx),%bx  // arg1

        shl $1,%bx
        shl $1,%bx

        push %ds
        xor %ax,%ax
        mov %ax,%ds

        pushf
        cli
        mov %cx,0(%bx)
        mov %dx,2(%bx)
        popf

        pop %ds
        ret

        .data
        .global intr_count
        .global endistack
        .global istack
        .extern current
        .extern previous

bios_call_cnt:                  // call BIOS IRQ 0 handler every 5th interrupt
        .word   5
org_irq0:                       // original BIOS IRQ 0 vector
        .long   0
intr_count:                     // stacked interrupts count. Start with 1
        .word   1               // because init_task() is in kernel mode
#ifdef CHECK_SS
ssmsg:  .ascii "INVALID SS\0"
#endif
callno:	.ascii "syscall in 0x%x, \0"
callno1:
	.ascii "out 0x%x (V7: %x)\n\0"
//sysfpu_msg:
	//.ascii "int SYSFPU\n\0"
stktrap_msg:
	.ascii "int STKTRAP\n\0"
codemap_msg:
	.ascii "int CODEMAP\n\0"

        .bss
        .p2align 1
endistack:
        .skip INTRSTACK_BYTES,0 // interrupt stack
istack:
