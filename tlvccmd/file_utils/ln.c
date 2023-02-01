/*
 * Copyright (c) 1993 by David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Most simple built-in commands are here.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <pwd.h>
#include <grp.h>
#include <utime.h>
#include <errno.h>
#include "futils.h"

/*
 * Return 1 if a filename is a directory.
 * Nonexistant files return 0.
 */
int isadir(const char *name)
{
	static struct stat statbuf;

	if (stat(name, &statbuf) < 0)
		return 0;

	return S_ISDIR(statbuf.st_mode);
}

/*
 * Build a path name from the specified directory name and file name.
 * If the directory name is NULL, then the original filename is returned.
 * The built path is in a static area, and is overwritten for each call.
 */
char *buildname(const char * const dirname, char *filename)
{
	char *cp;
	static char buf[PATHLEN];


	if ((dirname == NULL) || (*dirname == '\0')) return filename;

	cp = strrchr(filename, '/');
	if (cp) filename = cp + 1;

	strcpy(buf, dirname);
	strcat(buf, "/");
	strcat(buf, filename);

	return buf;
}


int main(int argc, char **argv)
{
	int	dirflag;
	char	*srcname;
	char	*destname;
	char	*lastarg;

	if (argc < 3) goto usage;

	if (argv[1][0] == '-') {
		if (strcmp(argv[1], "-s")) goto usage;

		if (argc != 4) goto usage;

		if (symlink(argv[2], argv[3]) < 0) {
			perror(argv[3]);
			return 1;
		}
		return 0;
	}

	/*
	 * Here for normal hard links.
	 */
	lastarg = argv[argc - 1];
	dirflag = isadir(lastarg);

	if ((argc > 3) && !dirflag) {
		errstr(lastarg);
		errmsg(": not a directory\n");
		goto usage;
	}

	while (argc-- > 2) {
		srcname = *(++argv);
		if (access(srcname, 0) < 0) {
			perror(srcname);
			continue;
		}

		destname = lastarg;
		if (dirflag)
			destname = buildname(destname, srcname);

		if (link(srcname, destname) < 0) {
			perror(destname);
			continue;
		}
	}
	return 0;

usage:
	errmsg("usage: ln [-s] link_target link_name\n");
	errmsg("Hard links are made by default. The -s option creates symbolic links instead.\n");
	errmsg("Creating hard links to directories is not allowed and will return an error.\n");
	return 1;
}
