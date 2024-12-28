#ifndef __LINUXMT_HDIO_H
#define __LINUXMT_HDIO_H

/* 
 * This file contains defines common to  all HD controller types
 */

struct hd_geometry {
    unsigned char heads;
    unsigned char sectors;
    unsigned short cylinders;
    unsigned long start;
};

/* hd/ide ctl's that pass (arg) ptrs to user space are numbered 0x030n/0x031n */
#define HDIO_GETGEO		0x0301	/* get device geometry */
#define HDIO_GET_UNMASKINTR	0x0302	/* get current unmask setting */
#define HDIO_GET_NOWERR		0x030a	/* get ignore-write-error flag */

/* hd/ide ctl's that pass (arg) non-ptr values are numbered 0x032n/0x033n */
#define HDIO_SET_UNMASKINTR	0x0322	/* permit other irqs during I/O */
#define HDIO_SET_NOWERR		0x0325	/* change ignore-write-error flag */

#endif
