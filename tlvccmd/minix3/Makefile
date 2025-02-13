BASEDIR=..

include $(BASEDIR)/Makefile-rules

###############################################################################

PRGS = file head sed sort tail tee cal diff find

cal: cal.o $(TINYPRINTF)
	$(LD) $(LDFLAGS) -o cal cal.o $(TINYPRINTF) $(LDLIBS)

diff: diff.o
	$(LD) $(LDFLAGS) -maout-heap=0x2000 -o diff diff.o $(LDLIBS)

file: file.o $(TINYPRINTF)
	$(LD) $(LDFLAGS) -o file file.o $(TINYPRINTF) $(LDLIBS)

find: find.o
	$(LD) $(LDFLAGS) -maout-heap=0x4000 -maout-stack=0x2000 -o find find.o $(LDLIBS)

head: head.o
	$(LD) $(LDFLAGS) -o head head.o $(LDLIBS)

sed: sed.o
	$(LD) $(LDFLAGS) -o sed sed.o $(LDLIBS)

# For TLVC, sort needs more data segment space than the kernel-given default.
sort: sort.o
	$(LD) $(LDFLAGS) -maout-heap=0xb000 -o sort sort.o $(LDLIBS)

tail: tail.o
	$(LD) $(LDFLAGS) -o tail tail.o $(LDLIBS)

tee: tee.o
	$(LD) $(LDFLAGS) -o tee tee.o $(LDLIBS)


all: $(PRGS)


install: $(PRGS)
	$(INSTALL) $(PRGS) $(DESTDIR)/bin

clean:
	rm -f core *.o $(PRGS)
