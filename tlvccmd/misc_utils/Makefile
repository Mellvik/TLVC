BASEDIR=..

###############################################################################

include $(BASEDIR)/Makefile-rules

###############################################################################

#TODO: fix uncompress zcat lpfilter disabled
PRGS = tar miniterm ed fdtest od hd time kilo mined sleep tty uuencode uudecode compress
PRGS += float

#PRGS_HOST=compress.host

all: $(PRGS) $(PRGS_HOST)

float: float.o
	$(LD) $(LDFLAGS) -o float float.o $(LDLIBS) -lc

ed: ed.o
	$(LD) $(LDFLAGS) -o ed ed.o $(LDLIBS)

fdtest: fdtest.o 
	$(LD) $(LDFLAGS) -o fdtest fdtest.o $(LDLIBS)

tar: tar.o
	$(LD) $(LDFLAGS) -o tar tar.o $(LDLIBS)
	
od: od.o
	$(LD) $(LDFLAGS) -o od od.o $(LDLIBS)

hd: hd.o
	$(LD) $(LDFLAGS) -o hd hd.o $(LDLIBS)

time: time.o
	$(LD) $(LDFLAGS) -o time time.o $(LDLIBS)

kilo: kilo.o
	$(LD) $(LDFLAGS) -maout-heap=0xffff -o kilo kilo.o $(LDLIBS)

mined: mined1.o mined2.o
	$(LD) $(LDFLAGS) -maout-heap=0xffff -o mined mined1.o mined2.o $(LDLIBS)

sleep: sleep.o
	$(LD) $(LDFLAGS) -o $@ $^ $(LDLIBS)

tty: tty.o
	$(LD) $(LDFLAGS) -o tty tty.o $(LDLIBS)

compress: compress.o
	$(LD) $(LDFLAGS) -o compress compress.o $(LDLIBS)

miniterm: miniterm.o
	$(LD) $(LDFLAGS) -o miniterm miniterm.o $(LDLIBS)

uuencode: uuencode.o
	$(LD) $(LDFLAGS) -o uuencode uuencode.o $(LDLIBS)

uudecode: uudecode.o
	$(LD) $(LDFLAGS) -o uudecode uudecode.o $(LDLIBS)

compress.host: compress.c
	$(HOSTCC) $(HOST_CFLAGS) -s compress.c -o $@

uncompress: compress
	ln -sf compress uncompress

zcat: compress
	ln -sf compress zcat

install: $(PRGS)
	$(INSTALL) $(PRGS) $(DESTDIR)/bin

clean:
	rm -f $(PRGS) $(PRGS_HOST) *~ *.o
