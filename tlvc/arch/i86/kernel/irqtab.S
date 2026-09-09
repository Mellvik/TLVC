#include <linuxmt/config.h>
#include <linuxmt/limits.h>
#include <linuxmt/trace.h>
#include <arch/asm-offsets.h>
#include <arch/ports.h>
#include <arch/irq.h>
#include <linuxmt/v7.h>

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
 * (inode_t is u_16 on venix) affecting stat/fstat. Also, while TLVC never returns
 * longs in registers (e.g lseek), Venix has many syscalls which does. 
 * More syscall details in Wiki.
 * Finally, like TLVC Venix will in most cases return result or -1 in AX, but
 * unlike TLVC, Venix uses CX to hold the error # if any. errno is updated in the
 * app (libc), not by the kernel. 
 * 
 * The kernel uses a flag in the task struct ('task_is_V7') to identify Venix apps.
 * This flag is updated every time a syscall is made and holds the syscall #.
 * In some syscalls the flag is cleared on entry, restored on 
 * exit in order to not affect nested syscalls or shared support routines.
 * The task flag is also used by a few routines to enable special processing on exit,
 * always related to long returns.
 * For robustness, the task flag is not used as a trigger for special processing 
 * when syscalls returns. Instead, V7 syscalls have their own path into and out of 
 * syscalls.
 * Finally, in addition to syscalls, other Venix traps (INTs) are handled here,
 * see below.
 */

#ifdef CONFIG_COMPAT_V7
	.global _irqit_v7	/* int F1 */
	.global _stktrap_v7	/* int F2 */
	.global _abort_v7	/* int F3 */
	.global _sysfpu_v7	/* int F4 */
	.global _cm_call_v7	/* int F5 */
	.global _cm_ret_v7	/* int F6 */

_sysfpu_v7:
	// FPU is required, ignore and fall through to sysret
	//mov	$sysfpu_msg,%ax
	//call	intmsg
sysret:
	add	$4,%sp	// skip dynamic handler return stack
	iret
	
intmsg:
	// entered with string ptr in AX
	// watch the stack!
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

_stktrap_v7:	/* Stack trap, may need a more elaborate message */
	mov	$stktrap_msg,%ax
	call	intmsg
	jmp	sysret

_cm_ret_v7:
	add	$4,%sp	// skip dynamic handler return stack

	// code adapted from the Venix kernel
	popw    %cx
	popw    %cx
	movw    2,%bx
	subw    (%bx),%cx
	pushw   %cx
	pushw   2(%bx)   
	subw    $4,%bx
	movw    %bx,2  
	iret

_cm_call_v7:
	add	$4,%sp	// skip dynamic handler return stack

	// code adapted from the Venix kernel
	popw    %cx
	popw    %cx
	orw     %dx,%dx
	je      l1d
	addw    %dx,%cx
	movw    2,%bx  
	addw    $4,%bx
	movw    %bx,2 
	movw    %dx,(%bx)
	popw    %dx
	popw    2(%bx)
	xorw    %bx,%bx
	pushw   %bx
	pushw   %dx
l1d:
	pushw   %cx
	pushw   %ax
	iret

_abort_v7:
	// abort trap: find pid, pass on to kill
	push	%ds
	push	%si
        mov     %sp,%si
        mov     %ss:6(%si),%ds	// Set kernel DS
	mov	%ss:4(%si),%si
	movb	$IDX_SYSCALL_V7,(%si)

	mov	current,%si
	mov	$6,%dx		// SIGABRT
	mov	$37,%bx		// sys_kill
	mov	2(%si),%ax	// pid
	pop	%si
	pop	%ds
	// fall through 

//---------------- start syscall handling -----------------------------

_irqit_v7:			// Handle Venix/V7 syscall
	xchg	%ax,%bx		// rearrange args
	xchg	%cx,%dx

	push    %ds
	push    %si
	push    %di

        mov     %sp,%si
        mov     %ss:8(%si),%ds		// Get kernel DS set so we can do our thing

	push	%bx
	mov	current,%bx
	mov	%ax,TASK_USER_V7(%bx)	// save the syscall # in the task flag, useful
					// for tracing/debugging.
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
	jne	doirq		// Not a syscall, must be a trap or interrupt
/*
!
!       ----------PROCESS SYSCALL----------
*/
#ifdef CONFIG_COMPAT_V7
do_syscall_v7:
	sti
	call	check_ustack
#ifdef CONFIG_TRACE
        call    trace_begin
#endif
	pop	%ax		// Syscall #
#ifdef SHOW_SYSCALL_V7		/* DEBUG message: syscall entry */
	push	%ax
	mov	$callno,%bx
	push	%ax
	push	%bx
	call	printk
	add	$4,%sp
	pop	%ax
#endif
	call	syscall

//
// Some V7 syscalls use both AX and DX for returns. If the return value is a 32bit number
// and not simply two ints (like in pipe() and wait()), we need to check whether
// we're dealing with a valid return or an error code. The test is DX == 0xffff.
// If true, we're *almost* sure it's an error code. The exception
// (so far) is time(): If (when) we get REALLY close to the end of time :-), that is,
// less than 10 hrs (the upper bit of AX must be set in order for the value to qualify
// as an error), all time() returns will be considered errors. We can live with that.
//

	mov     current,%bx
	mov	TASK_USER_V7(%bx),%cx	// has syscall # and return flags
	//movw	$0,TASK_USER_V7(%bx)	// Don't clear V7 flag anymore, always on for V7 apps
	add	$6,%sp			// discard BX, CX, DX
	test	$V7_LONG_RETURN,%cx	// test the 'long return' bit
	mov	$0,%cx			// CX is now the error flag
	jz	err_chk			// not set -> regular error handling
	cmp	$0xffff,%dx		// long error code ??
	jnz	no_err			// dx=-1: normal ax error processing
	jmp	is_err
err_chk:
	test	%ax,%ax			// AX negative?
	jns	no_err
is_err:
	neg	%ax
	mov	%ax,%cx			// CX goes to 'errno' in the app
	mov	$-1,%ax			// app level error indicator
no_err:
        push    %ax             	// syscall return low
	push	%dx			// return high

#ifdef SHOW_SYSCALL_V7		/* DEBUG: syscall exit */
	mov	%sp,%bx
	mov	14(%bx),%cx		// syscall #
	mov	$callno1,%ax
	mov	current,%bx
	push	TASK_USER_V7(%bx)	// V7 flag
	push	%cx
	push	%ax			// format string
	call	printk
	add	$6,%sp
#endif

#ifdef CONFIG_TRACE
        // strace.c must be compiled with tail optimization off to protect top of stack
        call    trace_end       // syscall return value is top of stack
#endif
        cmpw    $0,bh_active    // Any active bottom halfs?
        je      1f              // No
        call    do_bottom_half  // Run bottom halves

1:	call    do_signal       // process signals
        cli

        decw    intr_count
	pop	%dx
	pop	%ax

	pop	%di
	pop	%si
	jmp	restore_regs_v7
#endif

// ---------------- 'normal syscall' ------------
do_syscall:
        sti
        call    check_ustack    // Check user mode stack

#ifdef CONFIG_TRACE
        call    trace_begin
#endif

        pop     %ax             // get syscall function code in AX

#if SHOW_SYSCALL		/* DEBUG message: syscall entry */
	push	%ax
	mov	$callno,%bx
	push	%ax
	push	%bx
	call	printk
	add	$4,%sp
	pop	%ax
#endif
        call    syscall
        push    %ax             // syscall return value in ax

#ifdef SHOW_SYSCALL		/* DEBUG: syscall exit */
	mov	%sp,%bx
	mov	12(%bx),%cx	// syscall #
	mov	$callno1,%ax
	mov	current,%bx
	push	TASK_USER_V7(%bx)	// V7 flag
	push	%cx
	push	%ax		// format string
	call	printk
	add	$6,%sp
#endif

#ifdef CONFIG_TRACE
        // strace.c must be compiled with tail optimization off to protect top of stack
        call    trace_end       // syscall return value is top of stack
#endif
syscall_ret:
        cmpw    $0,bh_active    // Any active bottom halfs?
        je      1f		// No
        call    do_bottom_half  // Run bottom halves
        //call	schedule	// experimental

1:	call    do_signal	// process signals
        cli
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

	pop	%bx
	pop	%cx
        pop     %dx

        pop     %di
        pop     %si

restore_regs_v7:
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
1:
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
	//.ascii "int CODEMAP\n\0"

        .bss
        .p2align 1
endistack:
        .skip INTRSTACK_BYTES,0 // interrupt stack
istack:
