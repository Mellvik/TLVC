/* directhd.h header for ELKS kernel - Copyright (C) 1998 Blaz Antonic
 * Expanded for TLVC 2023 by Helge Skrivervik
 */

#ifndef __LINUXMT_DIRECTHD_H
#define __LINUXMT_DIRECTHD_H

/* define offsets from base port address */
#define ATA_ERROR		1
#define ATA_SEC_COUNT		2
#define ATA_SECTOR		3
#define ATA_CYLINDER_LO		4
#define ATA_CYLINDER_HI		5
#define ATA_DH			6
#define ATA_STATUS		7
#define ATA_COMMAND		7

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

/* Per drive config settings */
#define ATA_CFG_INT	0x02	/* Interrupts in use for drive */
#define ATA_CFG_LBA	0x40	/* Drive has LBA support */
#define ATA_CFG_NMULT	0x01	/* multi r/w NOT supported */
#define ATA_CFG_SSD	0x04	/* drive is solid state */

/* other definitions */
#define MAX_ATA_DRIVES 4		/* 2 per i/o channel and 2 i/o channels */

#if 0
#define ATA_DEVICE_NAME	"dhd"
#endif

#endif
