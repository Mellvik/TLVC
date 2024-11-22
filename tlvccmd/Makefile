# Makefile for the ELKS command set.
#
###############################################################################
#
# Include standard rules.

BASEDIR = .

include $(BASEDIR)/Makefile-rules

###############################################################################
#
# Subdirectories for clean / build / install

# All subdirectories to build & clean

# TODO: broken command compilations: byacc m4 xvi
# TLVC removed because of nonuse: busyelks, levee, nano-X, screen, tui
# 				  test
SUBDIRS =       \
	lib         \
	ash         \
	basic       \
	bc          \
	disk_utils  \
	elvis	    \
	file_utils  \
	inet        \
	ktcp        \
	minix1      \
	minix2      \
	minix3      \
	misc_utils  \
	sash        \
	cron        \
	sh_utils    \
	sys_utils   \
	# EOL

###############################################################################
#
# Compile everything.

all:
	@if [ ! -e $(TOPDIR)/include/autoconf.h ]; \
	then echo -e "\n*** ERROR: You must configure TLVC first ***\n" >&2; exit 1; fi
	for DIR in $(SUBDIRS); do $(MAKE) -C $$DIR all || exit 1; done
ifdef CONFIG_APP_CGATEXT)
	$(MAKE) -C cgatext
endif

install:
	$(MAKE) -f Make.install "CONFIG=$(TOPDIR)/.config"

clean:
	for DIR in $(SUBDIRS); do $(MAKE) -C $$DIR clean || exit 1; done

###############################################################################
