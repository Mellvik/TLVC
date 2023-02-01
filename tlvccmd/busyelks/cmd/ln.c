/*
 * Copyright (c) 1993 by David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Most simple built-in commands are here.
 */

#include "../sash.h"

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


int ln_main(int argc, char **argv)
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
			exit(1);
		}
		exit(0);
	}

	/*
	 * Here for normal hard links.
	 */
	lastarg = argv[argc - 1];
	dirflag = isadir(lastarg);

	if ((argc > 3) && !dirflag) {
		fprintf(stderr, "%s: not a directory\n", lastarg);
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
	exit(0);

usage:
	fprintf(stderr, "usage: %s [-s] link_target link_name\n", argv[0]);
	fprintf(stderr, "Hard links are made by default. The -s option creates symbolic links instead.\n");
	fprintf(stderr, "Creating hard links to directories is not allowed and will return an error.\n");
	exit(1);
}

