/*
 * Copyright (c) 1993 by David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Sep 2020 - added ro and remount,rw options - ghaerr
 * Feb 2022 - add -a auto mount w/o type specifier, -q query fs type
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/mount.h>
#include <linuxmt/fs.h>
#include <linuxmt/limits.h>
#include <autoconf.h>

#define errmsg(str) write(STDERR_FILENO, str, sizeof(str) - 1)

static char *fs_typename[] = {
	0, "minix", "msdos", "romfs"
};

/* keep this stuff in sync with df */
static struct dev_name_struct {
        char *name;
        dev_t num;
} p_devices[] = {
        { "hda",     0x0300 },
        { "hdb",     0x0320 },
        { "hdc",     0x0340 },
        { "hdd",     0x0360 },
        { "fd0",     0x0380 },
        { "fd1",     0x03a0 },
        { "ssd",     0x0700 },
        { "rd",      0x0100 },
        { "xda",     0x0600 },
        { "xdb",     0x0620 },
        { "dhda",    0x0500 },
        { "dhdb",    0x0520 },
        { "dhdc",    0x0540 },
        { "dhdd",    0x0560 },
        { "df0",     0x0200 },
        { "df1",     0x0220 },
        { NULL,           0 }
};

#define NAMEOFF 5
static char *dev_name(dev_t dev)
{
	static char name[10] = "/dev/";
	struct dev_name_struct *devices = p_devices;
	char *rootdev = getenv("ROOTDEV");

	if (!rootdev)
		printf("df: Warning: Cannot get ROOTDEV from environment\n");

	while (devices->num) {	
		if (devices->num == (dev & 0xfff0)) {
			int k;
			strcpy(&name[NAMEOFF], devices->name);
			k = strlen(name);
			if (name[k-1] > '9') {	/* partitioned devices */
				if (dev & 0x07) {
					name[k] = '0' + (dev & 7);
					name[k+1] = 0;
				}
			}
			return name;
		}
		devices++;
	}
	return NULL;
}

static int show_mount(dev_t dev)
{
	struct statfs statfs;

	if (ustatfs(dev, &statfs, UF_NOFREESPACE) < 0)
		return -1;

	if (statfs.f_type < FST_MSDOS) 
		printf("%-10s (%5s) blocks %6lu free %6lu mount %s\n",
		dev_name(statfs.f_dev), fs_typename[statfs.f_type], statfs.f_blocks,
		statfs.f_bfree, statfs.f_mntonname);
	else
		printf("%-9s (%5s)                           mount %s\n",
		dev_name(statfs.f_dev), fs_typename[statfs.f_type], statfs.f_mntonname);
	return 0;
}

static void show(void)
{
	int i;

	for (i = 0; i < NR_SUPER; i++)
		show_mount(i);
}

static void usage(void)
{
	errmsg("usage: mount [-a][-q][-t type] [-o ro|remount,{rw|ro}] <device> <directory>\n");
}

int main(int argc, char **argv)
{
	char	*str;
	int	type = 0;		/* default fs*/
	int	flags = 0;
	int	query = 0;
	char	*option;

	argc--;
	argv++;

	while ((argc > 0) && (**argv == '-')) {
		argc--;
		str = *argv++ ;

		while (*++str) switch (*str) {
			case 'q':
				query = 1;
				/* fall through and automount */
			case 'a':
				flags |= MS_AUTOMOUNT;
				break;
			case 't':
				if ((argc <= 0) || (**argv == '-')) {
					errmsg("mount: missing file system type\n");
					return 1;
				}

				option = *argv++;
				if (!strcmp(option, "minix"))
					type = FST_MINIX;
				else if (!strcmp(option, "msdos") || !strcmp(option, "fat"))
					type = FST_MSDOS;
				else if (!strcmp(option, "romfs"))
					type = FST_ROMFS;
				argc--;
				break;

			case 'o':
				if ((argc <= 0) || (**argv == '-')) {
					errmsg("mount: missing option string\n");
					return 1;
				}

				option = *argv++;
				if (!strcmp(option, "ro"))
					flags |= MS_RDONLY;
				else if (!strcmp(option, "remount,rw"))
					flags |= MS_REMOUNT;
				else if (!strcmp(option, "remount,ro"))
					flags |= MS_REMOUNT|MS_RDONLY;
				else {
					errmsg("mount: bad option string\n");
					return 1;
				}
				argc--;
				break;

			default:
				errmsg("mount: unknown option\n");
				return 1;
		}
	}

	if (argc == 0) {
		show();
		return 0;
	}

	if (argc != 2) {
		usage();
		return 1;
	}

	if (flags == 0 && type == 0)
		flags = MS_AUTOMOUNT;
	if (mount(argv[0], argv[1], type, flags) < 0) {
		if (flags & MS_AUTOMOUNT) {
			type = (!type || type == FST_MINIX)? FST_MSDOS: FST_MINIX;
			if (mount(argv[0], argv[1], type, flags) >= 0)
				goto mount_ok;
		}
		if (!query)
			perror("mount failed");
		return 1;
	}

mount_ok:
	/* if query return type: 1=fail, 2=MINIX, 3=FAT */
	if (query)
		return (!type || type == FST_MINIX)? 2: 3;
	return 0;
}
