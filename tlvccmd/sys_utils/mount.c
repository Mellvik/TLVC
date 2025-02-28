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
#include <errno.h>
#include <ctype.h>
#include <limits.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <linuxmt/fs.h>
#include <linuxmt/limits.h>

#define errmsg(str) write(STDERR_FILENO, str, sizeof(str) - 1)

static char *fs_typename[] = {
	0, "minix", "msdos", "romfs"
};

static int show_mount(dev_t dev)
{
	struct statfs statfs;

	if (ustatfs(dev, &statfs, UF_NOFREESPACE) < 0)
		return -1;

	if (statfs.f_type < FST_MSDOS) 
		printf("%-10s (%5s) blocks %6lu free %6lu mount %s\n",
		devname(statfs.f_dev, S_IFBLK), fs_typename[statfs.f_type], statfs.f_blocks,
		statfs.f_bfree, statfs.f_mntonname);
	else
		printf("%-9s (%5s)                           mount %s\n",
		devname(statfs.f_dev, S_IFBLK), fs_typename[statfs.f_type], statfs.f_mntonname);
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
	errmsg("usage: mount [-a][-q][-t minix|fat] [-o ro|remount,{rw|ro}] <device> <directory>\n");
	return;
}

int main(int argc, char **argv)
{
	char	*str;
	int	type = 0;		/* default fs */
	int	flags = 0;
	int	query = 0;
	char	*option;
	char	fsname[PATH_MAX];

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

	strncpy(fsname, argv[0], PATH_MAX-2);
	if (isalpha(fsname[strlen(fsname)-1])) {	/* we're mounting a flat drive,
							 * check that it really is flat */
		strcat(fsname, "1");
		if (open(fsname, O_RDWR) != -1) {
			printf("Cannot mount %s: Device is partitioned\n", argv[0]);
			return 1;
		}
	}
	if (flags == 0 && type == 0)
		flags = MS_AUTOMOUNT;
	if (mount(argv[0], argv[1], type, flags) < 0) {
		char *failed = "mount failed";
		if (flags & MS_AUTOMOUNT) {
			type = (!type || type == FST_MINIX)? FST_MSDOS: FST_MINIX;
			if (mount(argv[0], argv[1], type, flags) >= 0)
				goto mount_ok;
		}
		if (errno == ENODEV)
			printf("%s: Filesystem type not available\n", failed);
		else if (!query)
			perror(failed);
		return 1;
	}

mount_ok:
	/* if query return type: 1=fail, 2=MINIX, 3=FAT */
	if (query)
		return (!type || type == FST_MINIX)? 2: 3;
	return 0;
}
