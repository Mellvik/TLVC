/*
 * fdtest - A program to test floppy disk I/O speed in user space
 */
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <linuxmt/bioshd.h>
#include <linuxmt/biosparm.h>
#include <linuxmt/memory.h>
#include <linuxmt/mem.h>
#include <arch/hdreg.h>

#define USE_DIRECTFD
#ifndef USE_DIRECTFD
struct biosparms bdt;

/* Useful defines for accessing the above structure. */
#define CARRY_SET (bdt.fl & 0x1)
#define BD_IRQ bdt.irq
#define BD_AX bdt.ax
#define BD_BX bdt.bx
#define BD_CX bdt.cx
#define BD_DX bdt.dx
#define BD_SI bdt.si
#define BD_DI bdt.di
#define BD_ES bdt.es
#define BD_FL bdt.fl
#endif

#define MAX		18*2		/* max # of sectors per read */
#define SECSIZE		512L		/* sector size*/

unsigned char iobuf[MAX * SECSIZE];
unsigned long __far *jp;
int verbose = 0;
int out = 0;

#ifndef USE_DIRECTFD
int read_disk(unsigned short drive, unsigned short cylinder, unsigned short head,
	unsigned short sector, unsigned short count)
{
	if (count > MAX) count = MAX;
	if (verbose)
		fprintf(stderr, "d %d c %d h %d s %d cnt %d\n", drive, cylinder, head, sector, count);

	/* BIOS disk read into buffer*/
	BD_AX = BIOSHD_READ | count;
	BD_BX = _FP_OFF(iobuf);	/* note: 64k DMA boundary ignored here*/
	BD_ES = _FP_SEG(iobuf);
	BD_CX = (unsigned short) (((cylinder&0xff) << 8) | ((cylinder&0x300) >> 2) | sector);
	BD_DX = (head << 8) | drive;
	if (call_bios(&bdt)) {
		fprintf(stderr, "READ ERROR at CHS %d,%d,%d\n", cylinder, head, sector);
		return(1);
	}

	if (out)
		write(1, iobuf, count*512);
	return(0);
}
#endif

int timer_init(void)
{
    unsigned int kds, jaddr;
    int fd = open("/dev/kmem", O_RDONLY);

    if (fd < 0) {
        perror("/dev/kmem");
        return -1;
    }
    if ((ioctl(fd, MEM_GETDS, &kds) < 0) || (ioctl(fd, MEM_GETJIFFADDR, &jaddr)) < 0 )  {
        perror("ktcp: ioctl error in /dev/kmem");
        return -1;
    }
    jp = _MK_FP(kds, jaddr);
    close(fd);
    return 0;
}

/*
 * read a full track of single sectors with increasing sector numbers to get a
 * reasonable average single sector access time - return the average in ms */
static int av_sec_time(int fd, int b)
{
	int i;
	long t, a = 0;

	lseek(fd, 0, SEEK_SET);
	for (i = 0; i < b; i++) {
		t = *jp;
		read(fd, iobuf, SECSIZE);
		a += *jp - t;
	}
	a /= b;		/* average */
	fprintf(stderr, "Average sector read time (no seek): %ldms\n", a*10);
	return (int)(a*10);
}


int main(int ac, char **av)
{
	unsigned short drive, cylinder, head, sector, step = 0;
	unsigned short start_dma_page, end_dma_page;
	unsigned long normalized_seg;
	int i, j, max, direct = 0;
	unsigned short bs = 0;
	long start, end;
#ifdef USE_DIRECTFD
	long lba;
	char *device = "/dev/rdf0";
	struct hd_geometry fd_geo;
	int fd;
#endif

	/* setup starting CHS*/
	drive = 0;		/* 0-1*/
	cylinder = 3;		/* 0-79*/
	head = 0;		/* 0-1*/
	sector = 1;		/* 1-18, 18 on 1.44M floppies */
	max = 18;		/* sectors per track on device */

	while (--ac) {
		av++;
		switch ((*av)[1]) {
		case 'd':	/* select drive, 0 or 1 */
#ifdef USE_DIRECTFD
			device[8] = **(++av);
#else
			drive = atoi(*(++av));
#endif
			ac--;
			break;
		case 'c':	/* select (starting) cylinder, 0 - 79 */
			cylinder = atoi(*(++av));
			ac--;
			break;
		case 'h':	/* select start head, 0 or 1 */
			head = atoi(*(++av));
			ac--;
			break;
		case 'S':	/* use this many cyls to measure step time */
			step = atoi(*(++av));
			ac--;
			break;
		case 's':	/* select staring sector, 1 to MAX or max */
			sector = atoi(*(++av));
			ac--;
			break;
		case 't':	/* set sectors per track */
			max = atoi(*(++av));
			if (max > MAX) max = MAX;
			ac--;
			break;
		case 'D':	/* Direct: Read entire disk from the given starting point */
			direct++;
			break;
		case 'b':	/* blocksize, only in Direct Mode */
			bs = atoi(*(++av));
			ac--;
			if (bs < 1 || bs > MAX) bs = 1;
			break;
		case 'v':	/* verbose, show debug messages */
			verbose++;
			break;
		case 'o':	/* send read data to stdout */
			out++;
			break;
		case 'u':
		default:
			fprintf(stderr, "usage: fdtest [-D] [-v] [-o] [-d drive] [-c cylinder] [-h head]\n[-s sector] [-t sectors per track] [-b blocksize]\n");
			fprintf(stderr, "-D: Read entire device, -v: verbose, -o: send data to stdout\n");
			return(1);
		}	
	}
	timer_init();
#ifdef USE_DIRECTFD
	if ((fd = open(device, O_RDONLY)) < 0) {
		fprintf(stderr, "%s: ", device);
		perror("Device open failure");
		return(1);
	}
	if (read(fd, iobuf, 512) <= 0) {  /* read a sector to make sure
					   * autoprobe is done and there is
					   * a floppy in the drive */
		fprintf(stderr, "Read error on %s, exiting\n", device);
		return(1);
	}
	ioctl(fd, HDIO_GETGEO, &fd_geo);
	/* sanity check */
	max = fd_geo.sectors;
	if (cylinder >= fd_geo.cylinders) 
		cylinder = 0;
	if (sector > max) sector = 1;
	lba = (cylinder<<1 + head)*max + (sector - 1);
	lseek(fd, lba*SECSIZE, SEEK_SET);
	if (!bs) bs = max;

#endif
	/* dma start/end pages must be equal to ensure I/O within 64k DMA boundary for INT 13h*/
	start_dma_page = (_FP_SEG(iobuf) + ((__u16) iobuf >> 4)) >> 12;
	end_dma_page = (_FP_SEG(iobuf) + ((__u16) (iobuf + max * 512 - 1) >> 4)) >> 12;
	normalized_seg = (((__u32)_FP_SEG(iobuf) + (_FP_OFF(iobuf) >> 4)) << 16) +
			(_FP_OFF(iobuf) & 15);
	if (verbose) {
		fprintf(stderr, "[iobuf @ %04x:%04x] ", _FP_SEG(iobuf), (unsigned int)iobuf);
		fprintf(stderr, "dma start page %x, dma end page %x, buffer at %x:%x - ",
			start_dma_page, end_dma_page, _FP_SEG(normalized_seg), _FP_OFF(normalized_seg));
		fprintf(stderr, start_dma_page == end_dma_page? "OK\n": "ERROR\n");
	}
	if (start_dma_page != end_dma_page)
		fprintf(stderr, "Warning: Buffer spans 64k boundary, IO will be split!\n");

	signal(SIGINT, SIG_IGN);	/* any signal here will likely crash the system */

	if (direct) {
		int  blocks = 0;

		fprintf(stderr, "Reading drive %d from cyl %d, spt %d incr %d\n", 
				 drive, cylinder, max, bs);
		start = *jp;

#ifdef USE_DIRECTFD
		while (read(fd, iobuf, bs*SECSIZE) > 0) {
#else
		while (read_disk(drive, cylinder, head, sector, bs) == 0) {
			if (sector + bs <= max) sector += bs;
			else {
				if (bs <= max) {
					sector = sector + bs - max;
					if (!head)
						head++;
					else {
						head--;
						cylinder++;
					}
				} else {
					int j;
					for (j = bs; j > max; j -= max) {
						if (head) {
							head--;
							cylinder++;
						} else head++;
					}
					sector += bs%max;
					if (sector > max) {
						sector -= max;
						if (head) {
							head--;
							cylinder++;
						} else 
							head++;
					}
				}
			}
#endif
			blocks++; 
			if (!verbose) write(2, ".", 1);
		}
		end = *jp;
		i = (end-start)/100;
		j = (end-start)%100;
	        fprintf(stderr, "\nRead %d blocks in %d.%ds (%ldms per block)\n",
			blocks, i, j, (end-start)*10/blocks);
		return(0);
	}

	fprintf(stderr, "%s: Size %dk, using %d sectors per track, %d per cyl, start @ %d/%d/%d (lba %ld)\n\n", 
		device, (fd_geo.heads*fd_geo.cylinders*fd_geo.sectors)>>1, max, 
		max<<1, cylinder, head, sector, lba);
	start = *jp;
	for (i=0; i < bs; i++) {
#ifdef USE_DIRECTFD
		j = read(fd, iobuf, SECSIZE);
		if (j != SECSIZE)
			fprintf(stderr, "short read, expected %d, got %d\n", (int)SECSIZE, j);
#else
		(void)read_disk(drive, cylinder, head, sector+i, 1);
#endif
	}
	end = *jp;
	j = (int)(end-start)*10;	/* turn into ms */
	fprintf(stderr, "%2d sectors (1-by-1):\t%dms (%dms/sector)\n", bs, j, j/bs);
	
	lseek(fd, lba*SECSIZE, SEEK_SET);
	start = *jp;
	for (i=0;  i <= bs; i += 2) {
#ifdef USE_DIRECTFD
		j = read(fd, iobuf, SECSIZE<<1);
		if (j != SECSIZE<<1)
			fprintf(stderr, "short read, expected %d, got %d\n",
							(int)SECSIZE<<1, j);
#else
		(void)read_disk(drive, cylinder, head, sector+i, 2);
#endif
	}
	end = *jp;
	j = (int)(end-start)*10;
	fprintf(stderr, "%2d 1k blocks:\t\t%dms (%dms/block)\n", bs/2, j, j/(bs/2));

	lseek(fd, lba*SECSIZE, SEEK_SET);
	start = *jp;
#ifdef USE_DIRECTFD
	j = read(fd, iobuf, bs*SECSIZE);
	if (j != bs*SECSIZE)
			fprintf(stderr, "short read, expected %d, got %d\n",
							bs*(int)SECSIZE, j);
#else
	(void)read_disk(drive, cylinder, head, sector, max);
#endif
	end = *jp;
	j = end-start;
	fprintf(stderr, "%2d sectors in 1 op:\t%dms\n", bs, (int)j*10);
	i = av_sec_time(fd, max);
	if (out)
		write(1, iobuf, bs*512);
	if (step) {
		long seek = (long)(max*step*2 - 1) * SECSIZE;

		lseek(fd, (off_t)0, SEEK_SET);
		read(fd, iobuf, (size_t)SECSIZE);	/* move to cyl 0 before we start */
		lseek(fd, seek, SEEK_SET);
		start = *jp;
		read(fd, iobuf, (size_t)SECSIZE);
		lseek(fd, (off_t)0, SEEK_SET);
		read(fd, iobuf, (size_t)SECSIZE);
		end = *jp;
		//fprintf(stderr, "seek to %ld (jif %ld)\n", seek, end-start);
		seek = (end-start)*10L - i*2L; /* movement time, back and forth, ms */
		fprintf(stderr, "Seek time (%d steps*2): %ldms, step rate %ldms\n", 
				 step, seek, seek/(2L*(long)step));
	}
}
