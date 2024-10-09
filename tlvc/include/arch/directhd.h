/* directhd.h header for ELKS kernel - Copyright (C) 1998 Blaz Antonic
 * Expanded for TLVC 2023 by Helge Skrivervik
 */

#ifndef __LINUXMT_DIRECTHD_H
#define __LINUXMT_DIRECTHD_H

/* define offsets from base port address */
#define ATA_ERROR		1	/* read */
#define ATA_FEATURES		1	/* write */
#define ATA_SEC_COUNT		2
#define ATA_SECTOR		3
#define ATA_CYLINDER_LO		4
#define ATA_CYLINDER_HI		5
#define ATA_DH			6	/* drive # + head */
#define ATA_STATUS		7	/* read */
#define ATA_COMMAND		7	/* write */

/* define drive masks */
#define ATA_DRIVE0 0xa0
#define ATA_DRIVE1 0xb0

/* interface cmd addresses */
#define HD1_CMD			0x3f6
#define HD2_CMD			0x376

/* define drive commands */
#define ATA_DRIVE_ID		0xec	/* Get drive id */
#define ATA_SET_MULT		0xc6	/* Set cnt for mult. R/W */
#define ATA_SET_FEAT		0xef	/* Set features */
#define ATA_READ		0x20	/* read with retry */
#define ATA_READM		0xc4	/* Read multiple */
#define ATA_WRITE		0x30	/* write with retry */
#define ATA_WRITEM		0xc5	/* Write multiple */
#define ATA_RECAL		0x11	/* recalibrate/reset */
#define ATA_SPECIFY		0x91	/* 'initialize drive parameters' */

/* Bits of ATA_STATUS */
#define ERR_STAT	0x01
#define INDEX_STAT	0x02	/* Obsolete */
#define ECC_STAT	0x04	/* Obsolete */
#define DRQ_STAT	0x08
#define SEEK_STAT	0x10
#define WRERR_STAT	0x20	/* Not used */
#define READY_STAT	0x40
#define BUSY_STAT	0x80

/* Bits of ATA_ERROR register */
#define ATA_ERR_AMNF	0x01	/* Address mark not found */
#define ATA_ERR_TK0NF	0x02	/* Track zero not found */
#define ATA_ERR_ABRT	0x04	/* Requested command aborted (drive err or invalid command) */
#define ATA_ERR_IDNF	0x10	/* ID field not found */
#define ATA_ERR_UNC	0x40	/* Uncorrectable data error */
#define ATA_ERR_BBK	0x80	/* Bad Block mark detected */

/* Per drive config settings */
#define ATA_CFG_INT	0x02	/* Interrupts in use for drive */
#define ATA_CFG_SSD	0x04	/* drive is solid state */
#define ATA_CFG_OLDIDE	0x10	/* Old IDE drive w/ limited cmd set */
#define ATA_CFG_LBA	0x40	/* Drive has LBA support */

/* Per drive SET FEATURE commands */
#define ATA_FEAT_8BIT	0x01	/* set 8 bit mode */
#define ATA_FEAT_16BIT	0x81	/* disable 8 bit mode */
#define ATA_FEAT_NO_WCACHE 0x82	/* Disable write cache */
#define ATA_FEAT_SAVE	0x66	/* make changes semipermanent */

/* other definitions */
#define MAX_ATA_DRIVES 4		/* 2 per i/o channel and 2 i/o channels */

struct hd_geometry {
    unsigned char heads;
    unsigned char sectors;
    unsigned short cylinders;
    unsigned long start;
};

struct ide_controller {
    int io_port;
    int ctl_port;
    unsigned char irq;
    unsigned char reg_type;
};

/* hd/ide ctl's that pass (arg) ptrs to user space are numbered 0x030n/0x031n */
#define HDIO_GETGEO             0x0301  /* get device geometry */
#define HDIO_GET_UNMASKINTR     0x0302  /* get current unmask setting */
#define HDIO_GET_NOWERR         0x030a  /* get ignore-write-error flag */

/* hd/ide ctl's that pass (arg) non-ptr values are numbered 0x032n/0x033n */
#define HDIO_SET_UNMASKINTR     0x0322  /* permit other irqs during I/O */
#define HDIO_SET_NOWERR         0x0325  /* change ignore-write-error flag */

/* xtide configuration data & flags, mostløy from bootopts. Note that CFLITE and
 * HISPEED are mutually exclusive, setting both will create havoc */
#define XTIDE_CFG_CFLITE	1	/* Use the XT-CF Lite register addressing
					 * scheme. Ctrl reg is base + 0x1c */
#define XTIDE_CFG_HISPEED	2	/* For cards supporting 'high speed PIO' which
					 * means that all command registers are 2 apart
					 * (A0 is zero) in order to use word IO at the
					 * base address */
					/* If HISPEED is set, the address to the control
					 * register block is base+16 instead of base + 8 */
#define XTIDE_CFG_NO_INT	4	/* Use PIO mode only, interrupts not supported */
					/* unused, set the irq value to zero instead */
#endif
