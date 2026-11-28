/*
 * Location of the users' stored
 * registers relative to AX.
 * Usage is u.u_areg[XX].
 */
#define	AX	(0)
#define	BX	(-1)
#define	CX	(-2)
#define	DX	(-6)
#define	BP	(-9)
#define	SI	(-10)
#define	DI	(-11)
#define	SP	(-5)
#define	PC	(1)
#define	RPS	(3)

#define	CS	(2)
#define	DS	(-3)
#define	ES	(-7)
#define	MODE	(-4)

#define	MAXREG	7		/* number of regs to be cleared */
#define	TBIT	0400		/* PS trace bit */
#define	PSIE	0x200		/* Interrupt Enable bits in PS */

/*
 * Registers in calling sequence to trap, etc. routines.
 */
#define	REGSEQ	es,dx,sp,mode,ds,cx,bx,ax,pc,cs,ps
