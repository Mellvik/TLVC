BASEDIR=../..

include $(BASEDIR)/Makefile-rules

###############################################################################

PRG=httpd

LOCALFLAGS=-I$(ELKSCMD_DIR)
LDFLAGS += -maout-heap=1024 -maout-stack=1024

all: $(PRG)

$(PRG): httpd.o $(TINYPRINTF)
	$(LD) $(LDFLAGS) -o $(PRG) httpd.o $(TINYPRINTF) $(LDLIBS)

install: $(PRG)
	$(INSTALL) $(PRG) $(DESTDIR)/bin
	$(INSTALL) -d $(DESTDIR)/var/www
	$(INSTALL) sample_index.html $(DESTDIR)/var/www/index.html

clean:
	rm -f *.o $(PRG)
