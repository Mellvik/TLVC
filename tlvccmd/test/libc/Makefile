# Makefile for libc tests

BASEDIR=../../..

include $(BASEDIR)/Makefile-rules

PGM = test_libc

SRCS = \
	error.c \
	inet.c \
	main.c \
	malloc.c \
	math.c \
	misc.c \
	regex.c \
	stdio.c \
	string.c \
	system.c \
	testlib.c \
	time.c

OBJS = $(SRCS:.c=.o)

all: $(PGM)

$(PGM): $(OBJS)
	$(LD) $(LDFLAGS) -o $(PGM) $(OBJS) $(LDLIBS)

install: $(PGM)
	$(INSTALL) $(PGM) $(DESTDIR)/bin

clean:
	rm -f $(OBJS) $(PGM)
