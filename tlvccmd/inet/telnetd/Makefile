# Makefile for telnetd

BASEDIR=../..

include $(BASEDIR)/Makefile-rules

###############################################################################

SRCS = telnetd.c
OBJS = $(SRCS:.c=.o) 
LDFLAGS += -maout-heap=1024 -maout-stack=1024

all:	telnetd

telnetd: $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)

install: telnetd
	$(INSTALL) telnetd $(DESTDIR)/bin

clean:
	rm -f $(OBJS) telnetd

