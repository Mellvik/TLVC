/*
 * ELKS I/O port and IRQ mappings
 *
 * PC IRQ default mappings (* = possible conflicting uses)
 *
 *  IRQ	ELKS Device         Config Option           Status
 *  0   Timer                                       Required
 *  1   Keyboard	    CONFIG_CONSOLE_DIRECT   Optional
 *  2*  Cascade -> 9 on AT  CONFIG_ETH_WD           Optional on XT, not available on AT
 *  3   Com2 (/dev/ttyS1)   CONFIG_CHAR_DEV_RS      Optional
 *  4   Com1 (/dev/ttyS0)   CONFIG_CHAR_DEV_RS      Optional
 *  5*  PC/XT MFM 	    CONFIG_BLK_DEV_MHD
 *  6*  STD floppy ctrlr    CONFIG_BLK_DEV_FD
 *  7   Unused (LPT, Com3)
 *  8   Unused (RTC)
 *  9   Available, use for Ethernet, ...
 * 10   Available, use for Com4, Ethernet, ...
 * 11   Available, use for e.g. Ethernet
 * 12   Available, use for e.g. Ethernet, ...
 * 12*  Unused (Mouse)                              Turned off
 * 13   Unused (Math coproc.)                       Turned off
 * 14*  AT HD IDE (/dev/hda+b) CONFIG_BLK_DEV_HD
 * 15*  AT HD IDE (/dev/hdc+d) CONFIG_BLK_DEV_HD
 *
 * Edit settings below to change port address or IRQ:
 *   Change I/O port and driver IRQ number to match your hardware
 */

#ifdef CONFIG_ARCH_IBMPC
/* timer, timer-8254.c*/
#define TIMER_CMDS_PORT 0x43		/* command port */
#define TIMER_DATA_PORT 0x40		/* data port    */
#define TIMER_IRQ	0		/* can't change*/

/* bell, bell-8254.c*/
#define TIMER2_PORT	0x42		/* timer 2 data port for speaker frequency*/
#define SPEAKER_PORT	0x61

/* 8259 interrupt controller, irq-8259.c*/
#define PIC1_CMD   0x20			/* master */
#define PIC1_DATA  0x21
#define PIC2_CMD   0xA0			/* slave */
#define PIC2_DATA  0xA1
#endif

#ifdef CONFIG_ARCH_8018X
#define TIMER_IRQ	0		/* logical IRQ number, NOT related to the actual IRQ vector! */
#endif

#ifdef CONFIG_ARCH_PC98
/* timer, timer-8254.c*/
#define TIMER_CMDS_PORT 0x77		/* command port */
#define TIMER_DATA_PORT 0x71		/* data port    */
#define TIMER_IRQ	0		/* can't change*/

/* serial, serial-pc98.c*/
#define TIMER2_PORT	0x75		/* timer 2 data port for serial port*/

/* 8259 interrupt controller, irq-8259.c*/
#define PIC1_CMD   0x00			/* master */
#define PIC1_DATA  0x02
#define PIC2_CMD   0x08			/* slave */
#define PIC2_DATA  0x0A
#endif

/* keyboard, kbd-scancode.c*/
#define KBD_IO		0x60
#define KBD_CTL		0x61
#define KBD_IRQ		1

/* serial, serial.c*/
#ifdef CONFIG_CHAR_DEV_RS
//#define CONFIG_FAST_IRQ4             /* COM1 very fast serial driver, no ISIG handling*/
//#define CONFIG_FAST_IRQ3             /* COM2 very fast serial driver, no ISIG handling*/
#endif

#ifdef CONFIG_ARCH_PC98
#define COM1_PORT	0x30
#define COM1_IRQ	4		/* unregistered unless COM1_PORT found*/
#else
#define COM1_PORT	0x3f8
#define COM1_IRQ	4		/* unregistered unless COM1_PORT found*/

#define COM2_PORT	0x2f8
#define COM2_IRQ	3		/* unregistered unless COM2_PORT found*/

#define COM3_PORT	0x3e8
#define COM3_IRQ	5		/* unregistered unless COM3_PORT found*/

#define COM4_PORT	0x2e8
#define COM4_IRQ	7		/* unregistered unless COM4_PORT found*/
#endif

/* Ethernet card settings may be overridden in /bootopts */
/* ne2k, ne2k.c */ 
#define NE2K_PORT	0x300
#define NE2K_IRQ	12
#define NE2K_FLAGS	0x80

/* wd, wd.c*/
#define WD_PORT		0x240
#define WD_IRQ		2
#define WD_RAM		0xC800
#define WD_FLAGS	0x80

/* el3/3C509, el3.c */
#define EL3_PORT	0x330
#define EL3_IRQ		11
#define EL3_FLAGS	0x80

/* Intel EtherExpress 16, ee16.c */
#define EE16_PORT	0x360
#define EE16_IRQ	11
#define EE16_RAM	0xC800		/* Beware: if present, shared mem will be activated */
#define EE16_FLAGS	0x80

/* AMD LANCE (7990) and some later variants, like th 79C760 */
#define LANCE_PORT	0x340
#define LANCE_IRQ	9
#define LANCE_DMA	5
#define LANCE_FLAGS	0x80

/* IDE hard drive, directhd.c */
#define HD1_PORT	0x1f0
#define HD2_PORT	0x170
#define XTHD_IRQ	5		/* IDE controller on XT system (same as MFM) */
#define HD1_AT_IRQ	14
#define HD2_AT_IRQ	15

/* direct (non-BIOS) floppy */
#define FLOPPY_IRQ	6
#define FLOPPY_DMA	2

/* XT type MFM disk */
#define MHD_IRQ		5
#define MHD_DMA		3
#define MHD_PORT	0x320
