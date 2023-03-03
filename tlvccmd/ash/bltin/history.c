/*
 * The history command.  Lists the current history buffer - which may be empty.
 * to the standard output.
 *
 * Copyright (C) 2023 by Helge Skrivervik.  All rights reserved.
 * This file is part of ash, which is distributed under the terms specified
 * by the Ash General Public License.  See the file named LICENSE.
 */


#include "bltin.h"

void linenoiseHistoryList(void);

void histcmd(int argc, char **argv) {
	linenoiseHistoryList();
	return;
}
