# efdisk - manipulate partition tables in MBR on hard disks and 
#	   image files
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

CLEANME 	= ../bin/efdisk

DEPEND  	=

DISTFILES	=

NOINDENT	=

#########################################################################
# Include standard commands.

include $(BASEDIR)/Makefile-rules

#########################################################################
# Objects to be compiled.

SRCS=$(TOPDIR)/tlvccmd/disk_utils/fdisk.c

#OBJS=$(SRCS:.c=.o)

#########################################################################
# Commands.

all:	../bin/efdisk

../bin/efdisk: $(SRCS)
	$(CC) -o ../bin/efdisk $(CFLAGS) $(SRCS)

#########################################################################
### Dependencies:
