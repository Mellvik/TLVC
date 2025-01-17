# emkfat - create FAT 16 or 32 file system
#
#########################################################################
#
# Note! Dependencies are done automagically by 'make dep', which also
# removes any old dependencies. DON'T put your own dependencies here
# unless it's something special (ie not a .c file).
#
#########################################################################
# Relative path to base directory.

BASEDIR 	= ../..

#########################################################################
# Define the variables required by the standard rules - see the standard
# rules file (below) for details of these variables.

USEBCC 		= N

CLEANDEP	=

CLEANME 	= ../bin/emkfat

DEPEND  	=

DISTFILES	=

NOINDENT	=

#########################################################################
# Include standard commands.

include $(BASEDIR)/Makefile-rules

#########################################################################
# Objects to be compiled.

SRCS=$(TOPDIR)/tlvccmd/disk_utils/mkfat.c

#OBJS=$(SRCS:.c=.o)

#########################################################################
# Commands.

all:	../bin/emkfat

../bin/emkfat: $(SRCS)
	$(CC) -o ../bin/emkfat $(CFLAGS) $(SRCS)

#########################################################################
### Dependencies:
