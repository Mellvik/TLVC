/*
 * xtblk0.h: version 1.1 of 6/27/85
 */
/*
 * Define the Boot Record/Partition Table on Block 0 of the XT winnie.
 */

struct xp {
	char    xp_code[446];           /* bootstrap record */
	struct {
		unsigned char   xp_boot;                /* boot indicator */
		unsigned char   xp_s_h, xp_s_s, xp_s_c; /* starting address */
		unsigned char   xp_sys;                 /* system type */
		unsigned char   xp_e_h, xp_e_s, xp_e_c; /* ending address */
		long            xp_start;               /* starting sector */
		long            xp_size;                /* size in sectors */
	} xp_tab[4];                            /* 4 partition tables */
	short   xp_sig;                         /* signature */ 
};

struct xh {
	unsigned int    xh_sig1;                /* first  signature */
	unsigned int    xh_ndrv;                /* current drive number */
	char            xh_name[8];             /* current drive name */
	unsigned int    xh_npart;               /* phys partitions per drive */
	unsigned int    xh_nunit;               /* logical units per drive */
	unsigned int    xh_ntrack;              /* tracks per drive */
	unsigned int    xh_nhead;               /* heads per drive */
	unsigned int    xh_sechd;               /* sectors per head */
	unsigned int    xh_ncyls;               /* cylinders per drive */
	unsigned int    xh_blkscyl;             /* blocks per cylinder */
	unsigned int    xh_blksize;             /* bytes per block */
	unsigned int    xh_cntf;                /* step rate for drive */
	unsigned int    xh_maxfer;              /* maximum transfer size */
	unsigned int    xh_sig2;                /* second signature */
};

#define XP_CODESIZE     446             /* boot code size */

#define XH_SIZE         34              /* header size */
#define XH_START        (XP_CODESIZE - XH_SIZE) /* start of header */
#define XH_SIG1         0xaa55          /* magic constant */
#define XH_SIG2         0x55aa          /* magic constant */

#define XP_BOOT         0x80            /* boot indicator */
#define XP_NULL         0x00            /* no boot indicator */

#define XP_UNKNOWN      0x00            /* unknown system type */
#define XP_UNUSED       0x00            /* unused  system type */

#define XP_DOS          0x01            /* DOS system 2.0 up <= 10mb drive */
#define XP_DOS_1        0x01            /* DOS system 2.0 up <= 10mb drive */
#define XP_DOS_4        0x04            /* DOS system 3.0 up >  10mb drive */

#define XP_SYS          0x40            /* VENIX system/swap area */
#define XP_TMP          0x41            /* VENIX temp/pipe area */
#define XP_USR          0x42            /* VENIX user area */

#define XP_SYS_0        0x40            /* VENIX system/swap area */
#define XP_SYS_1        0x43            /* VENIX system/swap area */
#define XP_SYS_2        0x44            /* VENIX system/swap area */
#define XP_SYS_3        0x45            /* VENIX system/swap area */

#define XP_TMP_0        0x41            /* VENIX temp/pipe area */
#define XP_TMP_1        0x46            /* VENIX temp/pipe area */
#define XP_TMP_2        0x47            /* VENIX temp/pipe area */
#define XP_TMP_3        0x48            /* VENIX temp/pipe area */

#define XP_USR_0        0x42            /* VENIX user area */
#define XP_USR_1        0x49            /* VENIX user area */
#define XP_USR_2        0x50            /* VENIX user area */
#define XP_USR_3        0x51            /* VENIX user area */

#define XP_SIG          0xaa55          /* magic constant */
