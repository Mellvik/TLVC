# Makefile for Berkeley yacc.

BASEDIR = ..

include $(BASEDIR)/Makefile-rules

###############################################################################

BINDIR	      = /bin

HDRS	      = defs.h

LOCALFLAGS    = -DNDEBUG -D_POSIX_SOURCE

LIBS	      =

LINKER	      = $(CC)

MAKEFILE      = Makefile

OBJS	      = closure.o \
		error.o \
		lalr.o \
		lr0.o \
		main.o \
		mkpar.o \
		output.o \
		reader.o \
		skeleton.o \
		symtab.o \
		verbose.o \
		warshall.o

PRINT	      = pr -f -l88

PROGRAM	      = yacc

SRCS	      = closure.c \
		error.c \
		lalr.c \
		lr0.c \
		main.c \
		mkpar.c \
		output.c \
		reader.c \
		skeleton.c \
		symtab.c \
		verbose.c \
		warshall.c

all:		$(PROGRAM)

$(PROGRAM):	$(OBJS) $(LIBS)
	$(LINKER) $(LDFLAGS) -o $(PROGRAM) $(OBJS) $(LIBS)

install: $(PROGRAM)
	$(INSTALL) $(PROGRAM) $(DESTDIR)/bin

clean:
	rm -f $(OBJS) $(PROGRAM) core

#depend:;	@mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)
#
#index:;	@ctags -wx $(HDRS) $(SRCS)
#
#listing:;	@$(PRINT) Makefile $(HDRS) $(SRCS) | lpr
#
#lint:;		@lint $(SRCS)
#
#program:        $(PROGRAM)
#
#tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

###
closure.o: defs.h
error.o: defs.h
lalr.o: defs.h
lr0.o: defs.h
main.o: defs.h
mkpar.o: defs.h
output.o: defs.h
reader.o: defs.h
skeleton.o: defs.h
symtab.o: defs.h
verbose.o: defs.h
warshall.o: defs.h
