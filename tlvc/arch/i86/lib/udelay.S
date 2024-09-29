//
// (void) u10delay(int wait) - calibrated delay loop, returns in 'wait' * 10 microseconds
//		which is the best resolution possible on a PC/XT.
//		The inner loop and the cost of the call itself are calibrated
//		at boot time and stored in the globals sys_dly_index and
//		sys_dly_base. See init/main.c for details about the calibration.
//
//		Delays are never exact. Short delays suffer from approximation in 
//		calibration, in particular on slow (XT) machines. Long delays
//		will be disturbed by clock interrupts and other interrupts.
//		In general, delays tend to be longer on slow machines, shorter on
//		fast iron, just abnout right on 286 systems.
//
// hs [@mellvik] August 2024
//
        .arch   i8086, nojumps
        .code16
        .text

	.global u10delay

u10delay:
	//push	%bx
	mov	%sp,%bx
	mov	2(%bx),%ax
	mov	sys_dly_index,%bx	// must be positive or zero
	//push	%cx
	sub	sys_dly_base,%ax // correct for 'tare', the cost of the call
				// itself
	jbe	1f
3:	sub	$1,%ax		// sub works if ax=0, dec does not
	jbe	1f
	mov	%bx,%cx
2:
//	nop
//	loop	2b		// fails if CX is zero
	sub	$1,%cx		// now starting on zero is OK
	ja	2b
	jmp	3b
1:	//pop	%bx
	ret

	.data
	.extern sys_dly_index
	.extern sys_dly_base
