/*
 *  linux/fs/fcntl.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

#include <linuxmt/types.h>
#include <arch/segment.h>
#include <linuxmt/sched.h>
#include <linuxmt/kernel.h>
#include <linuxmt/fs.h>
#include <linuxmt/errno.h>
#include <linuxmt/stat.h>
#include <linuxmt/fcntl.h>
#include <linuxmt/string.h>

static int dupfd(unsigned int fd, unsigned int arg)
{
    register struct file_struct *fils = &current->files;

    if (fd >= NR_OPEN || !fils->fd[fd]) return -EBADF;

    if (arg >= NR_OPEN) return -EINVAL;

    while ((arg < NR_OPEN) && (fils->fd[arg])) ++arg;

    if (arg >= NR_OPEN) return -EMFILE;

    clear_bit(arg, &fils->close_on_exec);
    (fils->fd[arg] = fils->fd[fd])->f_count++;

    return arg;
}

int sys_dup2(unsigned int oldfd, unsigned int newfd)
{
    if (oldfd < NR_OPEN && current->files.fd[oldfd]) {
	if (newfd == oldfd) return newfd;
	/* following POSIX.1 6.2.1, if newfd >= NR_OPEN, return -EBADF */
	if (newfd < NR_OPEN) {
	    sys_close(newfd);
	    printk("DUP[%P]: old %d, new %d\n", oldfd, newfd);
	    return dupfd(oldfd, newfd);
	}
    }
    return -EBADF;
}

#ifdef CONFIG_COMPAT_V7
/* V7/Venix doesn't use/have dup2. Instead the 0100 bit of fildes is set to indicate dup2 */
int sys_dup(unsigned int fildes, unsigned int newfd)
{
    //if (current->task_is_V7) printk("DUP[%P]: old %x, new %x\n", fildes, newfd);
    if (current->task_is_V7 && (fildes&0100)) return(sys_dup2(fildes&~0100, newfd));
	//{ int r = sys_dup2(fildes&~0x40, newfd); printk("got r=%d for old0%d, f=%d\n", r, fildes&~0x40, newfd); return r;}
    return dupfd(fildes, 0);
}
#else
int sys_dup(unsigned int fildes)
{
    return dupfd(fildes, 0);
}
#endif

int sys_fcntl(unsigned int fd, unsigned int cmd, unsigned int arg)
{
    register struct file *filp;
    register struct file_struct *fils = &current->files;
    int result = 0;

    if (fd >= NR_OPEN || !(filp = fils->fd[fd])) return -EBADF;

    switch (cmd) {
    case F_DUPFD:
	result = dupfd(fd, arg);
	break;
    case F_GETFD:
	result = test_bit(fd, &fils->close_on_exec);
	break;
    case F_SETFD:
	if (arg & 1)
	    set_bit(fd, &fils->close_on_exec);
	else
	    clear_bit(fd, &fils->close_on_exec);
	break;
    case F_GETFL:
	result = (int) filp->f_flags;
	break;
    case F_SETFL:
	/*
	 * In the case of an append-only file, O_APPEND
	 * cannot be cleared
	 */
	if (!IS_APPEND(filp->f_inode) || (arg & O_APPEND)) {
	    filp->f_flags &= ~(O_APPEND | O_NONBLOCK);
	    filp->f_flags |= arg & (O_APPEND | O_NONBLOCK);
	    break;
	}
	result = -EPERM;
	break;
    default:
	result = -EINVAL;
    }

    return result;
}
