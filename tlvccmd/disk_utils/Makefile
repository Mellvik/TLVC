BASEDIR=..

include $(BASEDIR)/Makefile-rules

###############################################################################

PRGS = ramdisk mkfs mkfat fsck partype fdisk df

SPRGS=mkfs

all: $(PRGS)

install_sibo: all
	$(INSTALL) $(SPRGS) $(DESTDIR)/bin

install: all
	$(INSTALL) $(PRGS) $(DESTDIR)/bin

fsck: fsck.o
	$(LD) $(LDFLAGS) -o fsck -maout-heap=23000 fsck.o $(LDLIBS)

fdisk: fdisk.o
	$(LD) $(LDFLAGS) -o fdisk fdisk.o $(LDLIBS)

df: df.o $(TINYPRINTF)
	$(LD) $(LDFLAGS) -o df df.o $(TINYPRINTF) $(LDLIBS)

mkfs: mkfs.o $(TINYPRINTF)
	$(LD) $(LDFLAGS) -o mkfs mkfs.o $(TINYPRINTF) $(LDLIBS)

mkfat: mkfat.o $(TINYPRINTF)
	$(LD) $(LDFLAGS) -o mkfat mkfat.o $(TINYPRINTF) $(LDLIBS)

partype: partype.o
	$(LD) $(LDFLAGS) -o partype partype.o $(LDLIBS)

ramdisk: ramdisk.o $(TINYPRINTF)
	$(LD) $(LDFLAGS) -o ramdisk ramdisk.o $(TINYPRINTF) $(LDLIBS)

clean:
	rm -f *.o $(FORMATMOD) core $(PRGS)
