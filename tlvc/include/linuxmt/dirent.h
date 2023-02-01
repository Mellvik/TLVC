#ifndef __LINUXMT_DIRENT_H
#define __LINUXMT_DIRENT_H

#include <linuxmt/types.h>

#define MAXNAMLEN       26      /* 14 for MINIX, 26 for FAT */

struct dirent {
    u_ino_t         d_ino;
    off_t           d_offset;
    unsigned short  d_namlen;
    char            d_name[MAXNAMLEN+1];
};

#endif
