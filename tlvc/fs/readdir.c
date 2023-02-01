/*
 *  linux/fs/readdir.c
 *
 *  Copyright (C) 1995  Linus Torvalds
 *
 * THIS NEEDS CLEANUP! - Chad
 */

#include <linuxmt/types.h>
#include <linuxmt/errno.h>
#include <linuxmt/stat.h>
#include <linuxmt/dirent.h>
#include <linuxmt/kernel.h>
#include <linuxmt/sched.h>
#include <linuxmt/mm.h>
#include <linuxmt/debug.h>

#include <arch/segment.h>

/*
 * Traditional linux readdir() handling..
 *
 */
struct readdir_callback {
    struct dirent *dirent;
    int count;
};

static int fillonedir(char *__buf, char *name, size_t namlen, off_t offset, ino_t ino)
{
    struct dirent *dirent;

    if (((struct readdir_callback *)__buf)->count) return -EINVAL;
    ((struct readdir_callback *)__buf)->count = 1;
    dirent = ((struct readdir_callback *)__buf)->dirent;
    put_user_long(ino, &dirent->d_ino);
    put_user_long(offset, &dirent->d_offset);
    if (namlen > MAXNAMLEN) namlen = MAXNAMLEN;
    put_user(namlen, &dirent->d_namlen);
    memcpy_tofs(dirent->d_name, name, namlen);
    put_user_char('\0', dirent->d_name + namlen);
    return 0;
}

int sys_readdir(unsigned int fd, char *dirent, unsigned int count
		/* ignored and unused, noted in Linux man page */ )
{
    int error;
    struct file *file;
    register struct file *filp;
    register struct file_operations *fop;
    struct readdir_callback buf;

    if ((error = fd_check(fd, dirent, sizeof(struct dirent), FMODE_READ, &file)) >= 0) {
	error = -ENOTDIR;
	filp = file;
	fop = filp->f_op;
	if (fop && fop->readdir) {
	    buf.count = 0;
	    buf.dirent = (struct dirent *) dirent;
	    if ((error = fop->readdir(filp->f_inode, filp, &buf, fillonedir)) >= 0)
		error = buf.count;
	}
    }

    return error;
}
