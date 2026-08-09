/*
 *  linux/fs/stat.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

#include <linuxmt/types.h>
#include <linuxmt/errno.h>
#include <linuxmt/string.h>
#include <linuxmt/stat.h>
#include <linuxmt/fs.h>
#include <linuxmt/sched.h>
#include <linuxmt/kernel.h>
#include <linuxmt/mm.h>

#include <arch/segment.h>

static int cp_stat(register struct inode *inode, struct stat *statbuf, int v7)
{
    static struct stat tmp;		/* static not reentrant: conserve stack usage*/

    memset(&tmp, 0, sizeof(tmp));

    tmp.st_dev		= kdev_t_to_nr(inode->i_dev);
    tmp.st_ino		= inode->i_ino;
    tmp.st_mode 	= inode->i_mode;
    tmp.st_nlink	= inode->i_nlink;
    tmp.st_uid		= inode->i_uid;
    tmp.st_gid		= inode->i_gid;
    tmp.st_size 	= (off_t) inode->i_size;
    tmp.st_rdev 	= kdev_t_to_nr(inode->i_rdev);
    tmp.st_atime	= inode->i_atime;
    tmp.st_mtime	= inode->i_mtime;
    tmp.st_ctime	= inode->i_ctime;

    //printk("stat: v7 %x (%x)\n", v7, current->task_is_V7);
#ifdef CONFIG_COMPAT_V7
    /* compensate for short ino_t in V7/Venix stat and different file type mode flags */
    if (v7) {
	//printk("stat: V7 %x, size %lu, mode 0%o\n", v7, tmp.st_size, tmp.st_mode);
	if ((tmp.st_mode&S_IFMT) > 0100000 || (tmp.st_mode&S_IFMT) == 010000) 
	    return -EBADF;					/* ignore sock, link, fifo */
	if  (tmp.st_mode&S_IFMT) tmp.st_mode |= 0100000;	/* file, dir, blkdev, chrdev */
	if  (tmp.st_size > 4096) tmp.st_mode |= 0010000;	/* S_ILRG - probably not useful */
	verified_memcpy_tofs((char *)statbuf, (char *)&tmp, 4);
	return verified_memcpy_tofs((char *)statbuf+4, (char *)&tmp+6, sizeof(tmp)-6);
    } else
#endif
    return verified_memcpy_tofs((char *) statbuf, (char *) &tmp, sizeof(tmp));
}

int sys_stat(char *filename, struct stat *statbuf)
{
#ifdef CONFIG_COMPAT_V7
    int v7 = current->task_is_V7;
    current->task_is_V7 = 0;
#else 
    int v7 = 0;
#endif
    struct inode *inode;
    int error = namei(filename, &inode, 0, 0);

    if (!error) {
	error = cp_stat(inode, statbuf, v7);
	iput(inode);
    }
#ifdef CONFIG_COMPAT_V7		/* namei() may cause disk I/O and the V7 flag may be reset */
    current->task_is_V7 = v7;
#endif

    return error;
}

int sys_lstat(char *filename, struct stat *statbuf)
{
    struct inode *inode;
    int error = lnamei(filename, &inode);

    if (!error) {
	error = cp_stat(inode, statbuf, 0);
	iput(inode);
    }

    return error;
}

int sys_fstat(unsigned int fd, register struct stat *statbuf)
{
    struct file *f;
    int ret;
    int stat_sz = sizeof(struct stat);

#ifdef CONFIG_COMPAT_V7
    int v7 = current->task_is_V7;
    if (v7) {
	current->task_is_V7 = 0;	/* Probably superfluous, check! */
	stat_sz -= 2;			/* V7 stat-struct is 2 bytes shorter */
    }
#else
    int v7 = 0;
#endif

    ret = fd_check(fd, (char *)statbuf, stat_sz,
				FMODE_WRITE | FMODE_READ, &f);
    if (!ret)
	cp_stat(f->f_inode, statbuf, v7);

#ifdef CONFIG_COMPAT_V7
    current->task_is_V7 = v7;
#endif
    return ret;
}

int sys_readlink(char *path, char *buf, size_t bufsiz)
{
    struct inode *inode;
    register struct inode *pinode;
    register struct inode_operations *iop;
    int error = -EINVAL;

#ifdef CONFIG_COMPAT_V7
    /* This is syscall 59, which is exece/execve() on V7/Venix. */
    if (current->task_is_V7) {
	return(sys_execve(path, buf, bufsiz));
    }
#endif
    if ((bufsiz > 0)
	&& !(error = verify_area(VERIFY_WRITE, buf, bufsiz))
	&& !(error = lnamei(path, &inode))
	) {
	pinode = inode;
	iop = pinode->i_op;
	error = (!iop || !iop->readlink)
	    ? (iput(pinode), -EINVAL)
	    : iop->readlink(pinode, buf, bufsiz);
    }

    return error;
}
