BASEDIR=..

include $(BASEDIR)/Makefile-rules

CFLAGS += -Wno-maybe-uninitialized

HOSTCC = gcc
HOST_CFLAGS = -O3

###############################################################################

PRGS = basic
OBJS = basic.o host.o

ifeq ($(CONFIG_ARCH_IBMPC), y)
OBJS += host-stubs.o
endif

ifeq ($(CONFIG_ARCH_8018X), y)
OBJS += host-8018x.o
endif

ifeq ($(CONFIG_ARCH_PC98), y)
OBJS += host-pc98.o intC5-pc98.o
endif

all: $(PRGS)

basic: $(OBJS) basic.h host.h
	$(LD) $(LDFLAGS) -o basic $(OBJS) $(LDLIBS)

HOSTSRC = basic.c host.c host-stubs.c
HOSTSRC += ../../libc/misc/ecvt.c
HOSTSRC += ../../libc/misc/dtostr.c
hostbasic: $(HOSTSRC) host.h basic.h
	$(HOSTCC) $(HOST_CFLAGS) $(HOSTSRC) -o $@

install: $(PRGS)
	$(INSTALL) $(PRGS) $(DESTDIR)/bin

clean:
	rm -f $(PRGS) *.o hostbasic
