BASEDIR=..

include $(BASEDIR)/Makefile-rules

###############################################################################

PRGS = testsym disasm opcodes
LIBOBJS += syms.o stacktrace.o printreg.o
#OBJS += ulltostr.o
CFLAGS += -fno-optimize-sibling-calls
CFLAGS += -fno-omit-frame-pointer
#CFLAGS += -finstrument-functions
#LDFLAGS += -maout-heap=12000

all: nm86 $(PRGS)

nm86: nm86.c syms.c
	$(HOSTCC) $(HOSTCFLAGS) -o $@ $^

testsym: testsym.o $(LIBOBJS)
	$(LD) $(LDFLAGS) -mno-post-link -o $@ $^ $(LDLIBS)
	elf2elks --symtab --heap 12000 $@
	./nm86 $@ > $@.map
	cp $@ $(TOPDIR)/tlvccmd/rootfs_template/root

disasm: dis.o disasm.o $(LIBOBJS)
	$(LD) $(LDFLAGS) -mno-post-link -o $@ $^ $(LDLIBS)
	elf2elks --symtab --heap 0xffff $@
	cp -p $@ $(TOPDIR)/tlvccmd/rootfs_template/root
	-mkdir $(TOPDIR)/tlvccmd/rootfs_template/lib
	cp -p $(TOPDIR)/tlvc/arch/i86/boot/system.sym $(TOPDIR)/tlvccmd/rootfs_template/lib

opcodes: opcodes.o
	$(LD) $(LDFLAGS) -o $@ $^ $(LDLIBS)
	cp $@ $(TOPDIR)/tlvccmd/rootfs_template/root

disasm.o: disasm.c
	$(CC) $(CFLAGS) -fno-instrument-functions -c -o $*.o $<

ulltostr.o: ulltostr.c
	$(CC) $(CFLAGS) -fno-instrument-functions -c -o $*.o $<

install: $(PRGS)
	$(INSTALL) $(PRGS) $(DESTDIR)/bin

clean:
	rm -f $(PRGS) *.o nm86 testsym.map
