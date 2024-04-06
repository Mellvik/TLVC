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
#define ATA_CFG_XTIDE	0x80	/* XTIDE cards have odd addressing */
				/* there are many variants, may need details */

/* Per drive SET FEATURE commands */
#define ATA_FEAT_8BIT	0x01	/* set 8 bit mode */
#define ATA_FEAT_16BIT	0x81	/* disable 8 bit mode */
#define ATA_FEAT_NO_WCACHE 0x82	/* Disable write cache */
#define ATA_FEAT_SAVE	0x66	/* make changes semipermanent */

/* other definitions */
#define MAX_ATA_DRIVES 4		/* 2 per i/o channel and 2 i/o channels */

#endif
