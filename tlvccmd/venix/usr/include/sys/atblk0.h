/*
 * Define the Boot Record/Partition Table on Block 0 of the XT winnie.
 */

struct	ap
	{
		char	ap_code[446];			/* bootstrap record */
		struct	{
			unsigned char	ap_boot;	/* boot indicator */
			unsigned char	ap_s_h;		/* starting head */
			unsigned char	ap_s_s;		/* starting sector */
			unsigned char	ap_s_c;		/* starting cylinder */
			unsigned char	ap_sys;		/* system type */
			unsigned char	ap_e_h;		/* ending head */
			unsigned char	ap_e_s;		/* ending sector */
			unsigned char	ap_e_c;		/* ending cylinder */
			long		ap_start;	/* starting sector */
			long		ap_size;	/* size in sectors */
		} ap_tab[4];				/* 4 partition tables */
		short	ap_sig;				/* signature */ 
	};

struct	ah
	{
		unsigned int	ah_sig1;	/* first  signature */
		unsigned int	ah_ndrv;	/* current drive number */
		char		ah_name[8];	/* current drive name */
		unsigned int	ah_npart;	/* phys partitions per drive */
		unsigned int	ah_nunit;	/* logical units per drive */
		unsigned int	ah_ntrack;	/* tracks per drive */
		unsigned int	ah_nhead;	/* heads per drive */
		unsigned int	ah_sechd;	/* sectors per head */
		unsigned int	ah_ncyls;	/* cylinders per drive */
		unsigned int	ah_blkscyl;	/* blocks per cylinder */
		unsigned int	ah_blksize;	/* bytes per block */
		unsigned int	ah_cntf;	/* step rate for drive */
		unsigned int	ah_maxfer;	/* maximum transfer size */
		unsigned int	ah_sig2;	/* second signature */
	};

#define AP_CODESIZE	446			/* boot code size */

#define AH_SIZE		(sizeof(struct ah))	/* header size */
#define AH_START	(AP_CODESIZE - AH_SIZE)	/* start of header */
#define AH_SIG1		0xaa55			/* magic constant */
#define AH_SIG2		0x55aa			/* magic constant */

#define AP_BOOT		0x80			/* boot indicator */
#define AP_NULL		0x00			/* no boot indicator */

#define AP_UNKNOWN	0x00			/* unknown system type */
#define AP_UNUSED	0x00			/* unused  system type */

#define AP_DOS		0x01			/* DOS system 2.0 up <= 10mb */
#define AP_DOS_1	0x01			/* DOS system 2.0 up <= 10mb */
#define AP_DOS_4	0x04			/* DOS system 3.0 up >  10mb */

#define AP_SYS		0x40			/* VENIX system/swap area */
#define AP_TMP		0x41			/* VENIX temp/pipe area */
#define AP_USR		0x42			/* VENIX user area */

#define AP_SYS_0	0x40			/* VENIX system/swap area */
#define AP_SYS_1	0x43			/* VENIX system/swap area */
#define AP_SYS_2	0x44			/* VENIX system/swap area */
#define AP_SYS_3	0x45			/* VENIX system/swap area */

#define AP_TMP_0	0x41			/* VENIX temp/pipe area */
#define AP_TMP_1	0x46			/* VENIX temp/pipe area */
#define AP_TMP_2	0x47			/* VENIX temp/pipe area */
#define AP_TMP_3	0x48			/* VENIX temp/pipe area */

#define AP_USR_0	0x42			/* VENIX user area */
#define AP_USR_1	0x49			/* VENIX user area */
#define AP_USR_2	0x50			/* VENIX user area */
#define AP_USR_3	0x51			/* VENIX user area */

#define AP_SIG		0xaa55			/* magic constant */
