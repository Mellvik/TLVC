# Makefile of /libc/asm module

include $(TOPDIR)/libc/Makefile.inc

SRCS = \
    memcpy-s.S \
    memset-s.S \
    strcpy-s.S \
    strlen-s.S \
    divmod.S \
    # end of list

LEFTOUT = \
    memcmp-s.S \
    strcmp-s.S \
    # end of list

OBJS = $(SRCS:.S=.o)

all: out.a

out.a: $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS_SUB) $@ $^

clean:
	$(RM) *.[aod]
