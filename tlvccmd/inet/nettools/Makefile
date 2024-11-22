BASEDIR=../..

include $(BASEDIR)/Makefile-rules

###############################################################################

PRGS=netstat nslookup arp

LOCALFLAGS=-I$(TLVCCMD_DIR)

all: $(PRGS)

install: $(PRGS)
	$(INSTALL) $(PRGS) $(DESTDIR)/bin
#	$(INSTALL) resolv.cfg $(DESTDIR)/etc

netstat: netstat.o $(TINYPRINTF)
	$(LD) $(LDFLAGS) netstat.o $(TINYPRINTF) -o netstat $(LDLIBS)

nslookup: nslookup.o
	$(LD) $(LDFLAGS) nslookup.o -o nslookup $(LDLIBS)

arp: arp.o $(TINYPRINTF)
	$(LD) $(LDFLAGS) arp.o $(TINYPRINTF) -o arp $(LDLIBS)

clean:
	rm -f core *.o $(PRGS)
