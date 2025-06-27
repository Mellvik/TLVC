#include <linuxmt/config.h>
#include <linuxmt/prectimer.h>
#include <arch/irq.h>

#ifdef CONFIG_CALIBRATE_DELAY
unsigned sys_dly_index; /* indicating the speed of the system - for delay loops */
unsigned sys_dly_base;

void u10delay(unsigned);
void printk(const char *, ...);

#define TEST_DELAY 1
/* Create calibration constants for a system independent delay loop.
 * NOTE: The calibration does not work with BIOS IO!
 * The delay function (arch/i86/lib/udelay.S) has 3 parts to be accounted for
 * when scaling:
 * 1) the call (entry/exit), which should be insignificant but isn't on slow
 *    systems
 * 2) the outer loop, which becomes significant for short delays on slow systems.
 *    the outer loop may take 30ms (8MHz 8088) to 55ms (4.77MHz 8088) and thus
 *    becomes an important damage-limiter for short loops on such systems. 
 * 3) the inner loop, which is timed separately and when run 4 times and
 *    averaged, becomes a pretty good indication of the speed of the system (keep in
 *    mind that the # of clock cycles required for this simple loop is varying
 *    wildly between systems). Also, on a 386 (and later) instruction caching speeds
 *    up this loop significantly.
 *
 * Important notes:
 * - On fast QEMU systems, the collected timings vary wildly and are useless
 *   for any calibration, so we set some basic numbers manually and skip the 
 *   calculations. Delays don't matter on QEMU anyway. If QEMU is run with the
 *   -singlestep option, the delay values are 'reasonable'.
 *
 * The items used in the calibration are
 * adj - the time consumed by the precision timer routine, to be subtracted from
 *       all measurements
 * d0 - the delay routine call time, just call and return, no loops.
 * d1 - call time + one outer loop, no inner loop
 * d2 - the difference between d1 and d0, the cost of the outer loop
 * l  - the loop time for the inner loop, which used to be run 1000 times, now 125
 *	which makes the numbmers more predictable (less susceptible to system
 *	interrupt disturbance).
 * p  - the scale, 1 ptick, 0.838 us
 * The chosen unit of measure is 10us, too small for the slowest systems, too big for 
 * the fast ones, IOW a reasonable compromise.
 * In order to avoid MULs and DIVs, we calculate (in sys_dly_ind) the # of
 * inner loops to take, including one outer loop, to spend 10us.
 * On very slow systems, the 'cost' of the call itself becomes significant, and 
 * must be deducted from the loop count. This is the purpose of the sys_dly_base.
 * For the XT@4.77MHz even this is useless as the call to the delay routine 
 * takes close to 50us and the outer loop almost 10us. In this case the closest
 * possible approximation is to set the _base to 5 and the index to 0.
 * turn in the outer loop may exceed 70 pticks, or 59us.
 *
 * Again, in order to avoid conversions (MUL/DIV) at all cost, we use pticks
 * all the way and approximate 10us = 12 pticks.
 *
 * The inner loop formula then becomes
 * X = (12p - d2)/l where X is the number of loops per 10us. 
 * Obviously, if d2 becomes larger than 12p, which happens on a PC/XT, this won't
 * work and the speed index is set to 0 - see comments in the code.
 */

void calibrate_delay(void)
{
	unsigned d0, d1, adj, i, l;

	//sys_dly_base = 1;
	//sys_dly_index = 0;

	/* find processing time for the get_ptime tool, average over 4 runs */
	i = 0;
	get_ptime();
	for (d0 = 0; d0 < 4; d0++) {
		get_ptime();
		//asm volatile ("nop");
		i += (unsigned)get_ptime();
	}
	adj = i >> 2;

	/* time the inner loop. This - compensated for (d1-d0) - 
	 * becomes the machine speed index. 
	 * l = # of pticks to loop 125 times. longer loops
	 * get disturbed by interrupts.
	 */
	d0 = d1 = l = 0;
	for (i = 0; i < 4; i++) {
		get_ptime();
		clr_irq();
		asm volatile ("mov $125,%cx");
		//asm volatile ("1: nop; loop 1b");
		asm volatile ("1:sub $1,%cx;ja 1b");
		set_irq();
		l += (unsigned)get_ptime();
		//get_ptime();		/* time the outer loop */
		u10delay(0);
		d0 += (unsigned)get_ptime();
		u10delay(2);
		d1 += (unsigned)get_ptime();
	}
	l >>= 2;
	d0 >>= 2;
	d1 >>= 2;
	//printk("adj %u, l=%u (%k)", adj, l, l);
	if (l <= adj) {	/* If running QEMU w/HW accel, results are nonsensical */
		adj = l>>1;

	}
	l -= adj;
	d0 -= adj;
	d1 -= adj;
	//printk(" d0=%u (%k)", d0, d0);
	//printk(" d1=%u (%k)\n", d1, d1);
	if (d1 < d0) d1 = d0;	/* for QEMU */
	if (d0 >= 12) {		/* on systems running at <10MHz XT, the call itself 
				 * takes more than 10us
				 * and the best we can do is to set the index to
				 * 0 and increase the base somewhat. Rough
				 * approximation anyway */
		sys_dly_index = 0;
		sys_dly_base = d0/10;
	} else {
		d1 -= d0; 	/* d1 is (net) outer loop cost */
		sys_dly_index = (unsigned)((12UL-d1)*125/(unsigned long)l);
		//sys_dly_index = (unsigned)((12UL-d1)*1000UL/(unsigned long)l);
					/* 12 is # of pticks per 10us */
					/* subtract ptick cost of 1 outer loop */
		sys_dly_base = d0*838/10000;	/* tare, the 'weight' of u10delay()
					   * sans the inner loop. Notice the 
					   * extra 0 to scale to 10us */
		//printk("%u, %u, %u, %u, ", l, adj, d0, d1);
	}

	printk("Delay calibration index: %d, skew: %d\n", sys_dly_index, sys_dly_base);

#if TEST_DELAY
	/* verify */
	unsigned long temp;

	d1 = 5;				/* start off with 5*10us */
	for (d0 = 0; d0 < 4; d0++) {
		int diff, d;
		unsigned t;

		get_ptime();
		u10delay(d1);
		temp = get_ptime();
		if (temp >= adj) 
			temp -= adj;	/* QEMU weirdness */
		else
			temp -= 2;
		printk("temp=%lu; ", temp);
		t = (temp*838UL)/1000UL;	/* convert to microseconds */
		d1 *= 10;	/* scale to 10us - and increment for next loop */
		diff = t - d1;	/* diff in us */
		//printk("\n%k (%u) diff %d, d1=%ld;", temp, t, diff, (long)d1);
		printk("\n%lk (%u) diff %d, d1=%ld;", temp, t, diff, (long)d1);
		d = -diff * 100/d1;
		//if (d < 0) d = -d;
		//printk("\n%k (%u) diff %d (%d%%);", temp, t, diff, d);
		printk("\n%#lk (%u) diff %d (%d%%);", temp, t, diff, d);
	}
#endif
	printk("\n");
}
#endif
