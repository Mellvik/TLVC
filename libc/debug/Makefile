# Makefile of /libc/debug module

#COMPILER ?= ia16
#LIB ?= out.a

include $(TOPDIR)/libc/Makefile.inc

OBJS = \
	prectimer.o \
	# end of list

.PHONY: all

all: out.a

out.a: $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS_SUB) $@ $^

clean:
	$(RM) *.[aod]
