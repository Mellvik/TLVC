#!/bin/bash

# Set up the build environment

# Must be executed with top directory /TLVC as the current one

if [ ! -e "env.sh" ]; then
	echo "ERROR: You did not sourced this script from the top directory.";
	echo "       Set the top directory of TLVC as the current one,";
	echo "       then source this script again.";
	return 1;
fi

export TOPDIR="$(pwd)"
echo TOPDIR set to $TOPDIR

export CROSSDIR="$TOPDIR/cross"	
echo "CROSSDIR set to $CROSSDIR"

add_path () {
	if [[ ":$PATH:" != *":$1:"* ]]; then
		export PATH="$1:$PATH"
	fi
}

add_path "$CROSSDIR/bin"

# Set up internal TLVC tools path

TLVCTOOLSDIR="$TOPDIR/tlvc/tools"

add_path "$TLVCTOOLSDIR/bin"

echo PATH set to $PATH

# May inject some Make options

export MAKEFLAGS="$MAKEFLAGS"

echo MAKEFLAGS set to $MAKEFLAGS
