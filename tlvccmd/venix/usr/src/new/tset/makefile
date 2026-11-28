DESTDIR=
CFLAGS=-O -DUSG
CCFLAGS=-O -DUCB_NTTY
usg:	tset.c
	${CC} ${CFLAGS} tset.c -o tset -ltermlib
ucb:	tset.c
	${CC} ${CCFLAGS} tset.c -o tset -ltermlib
install:
	install -s tset ${DESTDIR}/usr/ucb
clean:
	rm -f *.o tset
