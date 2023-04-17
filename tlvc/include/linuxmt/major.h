#ifndef __LINUXMT_MAJOR_H
#define __LINUXMT_MAJOR_H

/*
 * This file has definitions for major device numbers
 */

/* limits */

#define MAX_CHRDEV 11
#define MAX_BLKDEV  7

/*
 * assignments
 *
 * devices are as follows (same as minix, so we can use the minix fs):
 *
 *      character              block                  comments
 *      --------------------   --------------------   --------------------
 *  0 - unnamed                unnamed                minor 0 = true nodev
 *  1 - /dev/mem               /dev/rd[01]            block ramdisk
 *  2 - /dev/ptyp*             /dev/f[0,...]          char pty master (c)
 *						      Direct floppy (bl)
 *  3 - /dev/ttyp*             /dev/{fd*,hd*}         block BIOS fd/hd
 *  4 - /dev/tty*,ttyp*,ttyS*                         char tty, pty slave, serial
 *  5 -
 *  6 - /dev/lp                /dev/rom               block romflash
 *  7 -                        /dev/udd               block meta user device driver
 *			       /dev/sdd
 *  8 - /dev/tcpdev                                   kernel <-> ktcp comm
 *  9 - /dev/eth                                      NIC drivers
 * 10 - /dev/cgatext
 */


/* These are the character devices */

#define UNNAMED_MAJOR     0
#define MEM_MAJOR         1
#define PTY_MASTER_MAJOR  2
#define PTY_SLAVE_MAJOR   3
#define TTY_MAJOR         4
#define TTYAUX_MAJOR      5
#define LP_MAJOR          6
#define UDD_MAJOR         7
#define TCPDEV_MAJOR      8
#define ETH_MAJOR         9  /* should be rather a network-class driver */
#define CGATEXT_MAJOR     10

/* These are the block devices */

#define RAM_MAJOR         1
#define FLOPPY_MAJOR      2  /* Direct FD */
#define BIOSHD_MAJOR      3
#define MSCDEX_MAJOR      4  /* unused*/
#define ATHD_MAJOR        5  /* Direct HD */
#define ROMFLASH_MAJOR    6
#define SSD_MAJOR         7  /* Was 2 */


#endif  /* !__LINUXMT_MAJOR_H */
