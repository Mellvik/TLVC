#ifndef __LINUXMT_DEVNUM_H
#define __LINUXMT_DEVNUM_H

#include <linuxmt/major.h>
#include <linuxmt/kdev_t.h>

/*
 * Device numbers for block and character devices
 *
 * NOTE: If a block device MINOR_SHIFT changes, must be updated here also.
 */

/* block devices */
#define DEV_BDA     MKDEV(BIOSHD_MAJOR, 0)
#define DEV_BDB     MKDEV(BIOSHD_MAJOR, 8)
#define DEV_BDC     MKDEV(BIOSHD_MAJOR, 16)
#define DEV_BDD     MKDEV(BIOSHD_MAJOR, 24)
#define DEV_HDA    MKDEV(ATHD_MAJOR, 0)
#define DEV_HDB    MKDEV(ATHD_MAJOR, 32)
#define DEV_HDC    MKDEV(ATHD_MAJOR, 64)
#define DEV_HDD    MKDEV(ATHD_MAJOR, 96)
#define DEV_FD0     MKDEV(BIOSHD_MAJOR, 32)
#define DEV_FD1     MKDEV(BIOSHD_MAJOR, 40)
#define DEV_DF0     MKDEV(FLOPPY_MAJOR, 0)
#define DEV_DF1     MKDEV(FLOPPY_MAJOR, 1)
#define DEV_XDA	    MKDEV(XD_MAJOR, 0)
#define DEV_XDB	    MKDEV(XD_MAJOR, 32)
#define DEV_ROM     MKDEV(ROMFLASH_MAJOR, 0)

/* char devices */
#define DEV_TTY1    MKDEV(TTY_MAJOR, 0)
#define DEV_TTY2    MKDEV(TTY_MAJOR, 1)
#define DEV_TTY3    MKDEV(TTY_MAJOR, 2)
#define DEV_TTY4    MKDEV(TTY_MAJOR, 3)
#define DEV_TTYS0   MKDEV(TTY_MAJOR, 64)
#define DEV_TTYS1   MKDEV(TTY_MAJOR, 65)
#define DEV_TTYS2   MKDEV(TTY_MAJOR, 66)
#define DEV_TTYS3   MKDEV(TTY_MAJOR, 67)


#endif /* !__LINUXMT_DEVNUM_H */
