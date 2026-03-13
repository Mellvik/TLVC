// Top half of fast serial interrupt handlers for ELKS & TLVC
//
// Runs on any stack and skips _irqit stack switching overhead.
// Must run with interrupts disabled as could interrupt user, kernel or interrupt stack.
// Reads received byte(s) into tty input queue at interrupt time.
// The bottom half (pump_serial) runs later, which processes queue and calls wake_up.
//
// 25 June 2020 Greg Haerr
//  1 Feb 2026 Rewritten completely in assembly
//  1 Mar 2026 Added flow control support (HS)
//
#include <linuxmt/config.h>
#include <linuxmt/ntty.h>
#include <arch/serial-8250.h>

.set INQSIZE, RSINQ_SIZE			// Must be power of two
.set INQMASK, INQSIZE-1

        .code16
        .text

#ifdef CONFIG_ARCH_IBMPC

        .global ports                   // struct serial_info ports[]
        .global asm_fast_com1           // entry points
        .global asm_fast_com2
        .global asm_fast_com3
        .global asm_fast_com4

// Original C handler rewritten in ASM:
//
// void rs_fast_com1(void)
// {
//     struct serial_info *sp = &ports[0];
//     struct ch_queue *q = &sp->tty->inq;
//     unsigned char c;
//
//     do {
//	 c = INB(sp->io + UART_RX);       // Read received data
//	 if (q->len < q->size) {
//         q->base[q->head] = c;
//         if (++q->head >= q->size)
//             q->head = 0;
//         q->len++;
//	 }
//	 if (c == 03)                     // assumes VINTR = ^C and byte queued anyways
//         sp->intrchar = c; 
//     } while (INB(sp->io + UART_LSR) & UART_LSR_DR);
// }

//
// Entry for ttyS0 top half interrupt handler, called by CALLF within dynamic handler
//
asm_fast_com1:
        push    %cx
        mov     $ports+0,%cx            // cx = &ports[0]

// Common entry point, shared by all serial lines
//
common_entry:
        push    %ax                     // save regs, uses 14 bytes of current stack
        push    %bx
        push    %dx
        push    %ds

        // Recover kernel data segment
        // Was pushed by the CALLF of the dynamic handler
        mov     %sp,%bx
        mov     %ss:12(%bx),%ds

        // translated C routine above
        // NOTE: uses hard-coded offsets from struct serial_info, tty and ch_queue
        push    %si
        push    %di
	push	%es

        mov     %cx,%bx                 // bx = sp = &ports[n]
        mov     (%bx),%si               // si = q = &sp->tty->inq
        mov     0x4(%bx),%dx            // dx = sp->io (port IO address)
	mov	%dx,%es			// save sp->io for later
main_loop:
	mov	%es,%dx			// make sure DX = sp->io when entering the loop
	add	$2,%dx
	in	%dx,%al			// Get the IIR to identify the interrupt
	test	$UART_IIR_NO_INT,%al	// Done - or spurious interrupt
	jnz	rs_done
	test	$UART_IIR_RDI,%al	// data to read?
	jnz	more_data
#ifdef CONFIG_HW_SERIAL_OINT
	test	$UART_IIR_THRI,%al	// UART_IIR_THRI - transmitter empty (ready)
	jnz	transm_rdy
#endif
#ifdef CONFIG_HW_SERIAL_HWFLOW
	test	$0xf,%al		// UART_IIR_MSI (=0) - modem line status changed
	jz	status_chg
#endif
	jmp	rs_done			// Exit

	// Process incoming data
more_data:
	mov	%es,%dx			// Restore base port address
        in      %dx,%al
        mov     %al,%dl                 // dl = c = INB(sp->io+UART_RX)
#ifdef CONFIG_HW_SERIAL_XONXOFF		/* Handle XON/XOFF from remote, */
					// let pump_port() do the work
	mov	6(%bx),%ax		// ax = flags
	test	$SERF_IXOFF,%ax		// IXOFF active?
	jz	ixoff_done
	cmp	$XON_CHAR,%dl
	jz	clr_xoff
	cmp	$XOFF_CHAR,%dl
	jnz	ixoff_done
	or	$SERF_IXOFF_S,%ax	// Set the stop-output flag, OK if already set */
	jmp	save_flg
clr_xoff:
	and	$~SERF_IXOFF_S,%ax	// Clear stop-output flag
save_flg:
	mov	%ax,6(%bx)
	jmp	skip_queue		// don't save xon/xoff in the queue
ixoff_done:
#endif
        mov     (%si),%ax               // ax = q->len
        cmp     0x2(%si),%ax            // if (ax >= q->size)
        mov	%dl,%ah			// ah = c (save)
        jge     skip_queue		// don't save if queue full
        mov     0x4(%si),%bx            // bx = q->head
        mov     0x8(%si),%di            // di = q->base
        mov     %dl,(%bx,%di)           // q->base[q->head] = c
        inc     %bx
	and	$INQMASK,%bx		// handle wraparound
        mov     %bx,0x4(%si)            // ++q->head
1:	incw    (%si)			// ++q->len
skip_queue:
	mov	%es,%dx
	add	$UART_LSR,%dx		// get line status (LSR)
	in	%dx,%al
	test	$UART_LSR_DR,%al	// more data in fifo?
	jnz	more_data

	cmp     $0x3,%ah                // if (c == 03) ^C
	jne     main_loop
        mov     %cx,%bx                 // bx = sp
        movw    $0x3,2(%bx)             // sp->intrchar = c
	jmp	main_loop		// check for more interrupts

#ifdef CONFIG_HW_SERIAL_HWFLOW
status_chg:	// modem status line changed, really CTS change from remote end
		// to stop or restart data flow.
		// The short version simply copies the CTS bit from the MSR
		// to the flag variable. This ensures they can never get out of sync.
#if 1
	add	$4,%dx			// dx pointed to IIR (2), now MSR (6)
	in	%dx,%al			// Read MSR, bit 0 is the DCTS bit
	//test	$UART_MSR_DCTS,%al	// If not set, just bolt
	//jz	main_loop		// not useful - for reliability we're copying
					//   the CTS bit every time
	mov	%al,%dl
	mov	6(%bx),%ax		// ax = flags
	or	$SERF_CTSMASK,%ax	// SERF_CTS|SERF_CTSCHG
	test	$UART_MSR_CTS,%dl
	jnz	1f
	and	$~SERF_CTS,%ax		// Clear if CTS off
1:	mov	%ax,6(%bx)		// Save

#else	/* This version covers the entire party, not useful now */
	mov	6(%bx),%ax		// ax = flags
	cmp	$SERF_RTSCTS,%ax	// check SERF_RTSCTS
	jnz	main_loop
	add	$4,%dx			// dx pointed to IIR, now MSR
	in	%dx,%al
	cmp	$UART_MSR_DDSR,%al	// UART_MSR_DDSR
	jnz	main_loop		// nothing to do
	mov	%al,%dl
	ror	$2,%dl			// put the CTS bit into position
	and	$0x4,%dl		// keep this bit only
	mov	6(%bx),%ax		// ax = flags
	and	$0xfb,%ah
	or	%dl,%ah			// copy the CTS bit into SERF_CTSACT
	mov	%ax,6(%bx)		// save flags
#endif
	jmp	main_loop
#endif	
	
#ifdef CONFIG_HW_SERIAL_OINT
transm_rdy:
#endif

rs_done:
	pop	%es
	pop     %di
        pop     %si

        mov     $0x20,%al               // EOI on primary controller
        out     %al,$0x20
        pop     %ds                     // restore regs
        pop     %dx
        pop     %bx
        pop     %ax
        pop     %cx
        add     $4,%sp                  // skip the trampoline DS:*irq
        iret

//
// Entry for ttyS1 top half interrupt handler, called by CALLF within dynamic handler
//
asm_fast_com2:
        push    %cx
        mov     $ports+16,%cx           // cx = &ports[1]
        jmp     common_entry

//
// Entry for ttyS2 top half interrupt handler, called by CALLF within dynamic handler
//
asm_fast_com3:
        push    %cx
        mov     $ports+32,%cx           // cx = &ports[2]
        jmp     common_entry

//
// Entry for ttyS3 top half interrupt handler, called by CALLF within dynamic handler
//
asm_fast_com4:
        push    %cx
        mov     $ports+48,%cx           // cx = &ports[3]
        jmp     common_entry


#endif
