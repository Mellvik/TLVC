#
# Makefile for venix to tlvc tool conversion
#
LLDFLAGS=-melks-libc -mcmodel=small -nostdlib
CRT0=$(TOPDIR)/libc/build-ml/elkslibc/crt0.o
ASMDIR=asm
BASEDIR=../..

include $(BASEDIR)/Makefile-rules

###############################################################################


PRGS = \
	uuencode \
	sync \
	cat \
	uux \
	sum \
#	dd \
#	vi \
	# EOL

#asm failures:
	#as  Undefined ref to abort\

PRGS := \
	ar \
	cc \
	diff \
	ed \
	expand \
	grep \
	join \
	make \
	neqn \
	ranlib \
	sh \
	sort \
	split \
	strings_ucb \
	strings \
	tar \
	tsort \
	# EOL

$(PRGS): % : %.o 
	$(LD) $(LLDFLAGS) -o $@ $(CRT0) $^ 

# Optional: Ensure .o files are built from .c files
#%.o: %.s
	#$(LD) -c $< -o $@


all: $(PRGS)

install: $(PRGS)
	$(INSTALL) $(PRGS) $(DESTDIR)/bin

clean:
	rm -f *.o core $(PRGS)
