/*
 * ioctl calls to disk drivers.
 *      This must be applied to the character ("raw") interface.
 */

#define I_FORMAT        (('X'<<8)|1)    /* format a disk (floppy) */
#define I_GETDPP        (('X'<<8)|2)    /* get disk partition parameters */
/* the following defs will be valid only when executed on the .phy device */
#define I_REREAD        (('X'<<8)|3)    /* reread disk partition parameters */
#define I_SETNDRV       (('X'<<8)|4)    /* set new maximum of active drives */
                                        /* d_nblock and d_offset must equal 0 */
                                        /* d_ntrack has the new maximum */
                                        /* must be between 1 and NDRV, incl */
#define I_LOAD          (('X'<<8)|5)    /* mark drive as loaded, default */
#define I_UNLOAD        (('X'<<8)|6)    /* mark drive as unloaded */
#define I_DUMP          (('X'<<8)|7)    /* dump internal drive parameters */

struct diskparm {               /*** I_GETDPP structure ***/
        long    d_nblock;       /* no. of blocks in this partition */
        long    d_offset;       /* no. of blocks offset from start of disk */
        short   d_nsect;        /* no. of sectors on disk */
        short   d_nhead;        /* no. of heads on disk */
        short   d_ntrack;       /* no. of tracks on disk (0 == unknown) */
};
