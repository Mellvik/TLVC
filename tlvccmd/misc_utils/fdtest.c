/*
 * fdtest - A program to test floppy disk I/O speed in user space
 *	Originally by @ghaerr - for ELKS,
 *	rewritten and expanded byt @mellvik for TLVC 2024
 */
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <linuxmt/memory.h>
#include <arch/hdio.h>
#include <linuxmt/prectimer.h>

#define MAXSPT		18*2		/* max # of sectors per operation */
#define SECSIZE		512L		/* sector size*/
//#define HDTEST		/* allow device to be specified on the command line */

unsigned char iobuf[MAXSPT * SECSIZE];
int verbose = 0, debug = 0;

/*
 * read 16 random sectors from the same cylinder to get a
 * reasonable average single sector access time. A crude random sector #
 * is generated by combining two readings of gettimeofday, using the 
 * microsecond part only and then use the low 4 bits for the sector #.
 * The random vector is shifted 2 positions right per iteration,
 * providing a total of 16 pseudo random numbers in the 0-15 range. 
 */
static unsigned long av_sec_time(int fd, int spt)
{
	unsigned long start = 4 * spt * SECSIZE;
	unsigned long a = 0, random;
	int i, next;
	struct timeval t;
	unsigned long time_arr[16];
	int sec_arr[16];

	gettimeofday(&t, NULL);
	random = t.tv_usec + t.tv_sec;	/* create random vector */
	lseek(fd, start, SEEK_SET);	/* move to cyl 2, sec 1 */
	read(fd, iobuf, SECSIZE);
	gettimeofday(&t, NULL);
	random += t.tv_usec<<16;	/*rndomize even more */
	for (i = 0; i < 16; i++) {
		next = (int)(random&0x000fL);
		lseek(fd, start + (next<<9), SEEK_SET);
		get_ptime();
		if (read(fd, iobuf, SECSIZE) <= 0) return 0;
		if (debug) {
			sec_arr[i] = next+1;
			time_arr[i] = get_ptime();
		} else
			a += get_ptime();
		random >>=2;
	}
	if (debug) {
		for (i = 0; i < 16; i++) {
			fprintf(stderr, "%d (%d):\t%lk\t%lu\n", i, sec_arr[i], 
				time_arr[i], time_arr[i]);
			a += time_arr[i];
		}
		fprintf(stderr,
			"Average sector read time (%d repetitions): %#lk\n", 
			16, a>>4);
		return a>>4;
	} else {
		a >>= 4;
		fprintf(stderr, "Average sector read time (%d repetitions): %#lk\n", 16, a);
		return a;
	}
}


int main(int ac, char **av)
{
	unsigned short cylinder, head, sector, step = 0;
	unsigned short start_dma_page, end_dma_page;
	unsigned long normalized_seg;
	int i, j, spt, fd, direct = 0;
	unsigned short bs = 0;
	long ave, pticks, lba;
	char device[20] = "/dev/rdf0";
	struct hd_geometry fd_geo;

	/* setup starting CHS */
	cylinder = 3;		/* 0-79 */
	head = 0;		/* 0-1 */
	sector = 1;		/* 1-18, 18 on 1.44M floppies */

	while (--ac) {
#ifdef HDTEST
	/* Allow a raw device to be provided as the last argument on the
	 * command line - if progname is 'hdtest'
	 */
		char *progname = *av;
		char *pn = strrchr(progname, '/');
		if (pn) progname = pn+1;
		if (ac == 1 && **av != '-' && !strcmp(progname, "hdtest")) {
			strncpy(device, *(++av), 19);
			/* sanity check, '/dev/' is mandatory */
			if (strncmp(device, "/dev/", 5) ||
			   (*(strrchr(device, '/') + 1) != 'r')) {
				fprintf(stderr, "%s: %s - bad device\n",
					pn, device);
				return 1;
			}
			break;
		}
#endif
		switch ((*av)[1]) {
#ifdef HDTEST
		case 'd':	/* select drive, 0 or 1 */
			device[8] = (char)(**(++av));
			ac--;
			break;
#endif
		case 'c':	/* select (starting) cylinder, 0 - 79 */
			cylinder = atoi(*(++av));
			ac--;
			break;
		case 'h':	/* select start head, 0 or 1 */
			head = atoi(*(++av));
			ac--;
			break;
		case 'S':	/* use this many cyls to measure step time */
			step++;
			break;
		case 's':	/* select staring sector, 1 to MAXSPT or spt */
			sector = atoi(*(++av));
			ac--;
			break;
		case 'D':	/* Direct: Read entire disk from the given starting point */
			direct++;
			break;
		case 'b':	/* clusersize in # of sectors, defaault is spt */
			bs = atoi(*(++av));
			ac--;
			break;
		case 'v':	/* verbose, show debug messages */
			verbose++;
			break;
		case 'V':	/* Very Verbose - trace sector reads and timings */
			debug++;
			break;
		case 'u':
		default:
			fprintf(stderr, "usage: fdtest [-D] [-S] [-v] [-V] [-d drive] [-c cylinder] [-h head]\n[-s sector] [-b clustersize (#sectors)]\n");
			fprintf(stderr, "-D: Read entire device, -v: verbose, -S find steprate\n-V: Very verbose (display individual sector timings when creating the sector average)\n");
			return(1);
		}	
	}
	init_ptime();

	if ((fd = open(device, O_RDONLY)) < 0) {
		fprintf(stderr, "%s: ", device);
		perror("Device open failure");
		return(1);
	}
	if (read(fd, iobuf, 512) <= 0) {  /* read a sector to make sure
					   * autoprobe is finished and there is
					   * a floppy in the drive */
		fprintf(stderr, "Read error on %s, exiting\n", device);
		return(1);
	}
	ioctl(fd, HDIO_GETGEO, &fd_geo);
	/* sanity check */
	spt = fd_geo.sectors;
	if (cylinder >= fd_geo.cylinders) 
		cylinder = 0;
	if (sector > spt || sector < 1) sector = 1;

	/* set the default starting point */
	lba = (cylinder<<1 + head)*spt + (sector - 1);
	lseek(fd, lba*SECSIZE, SEEK_SET);
	read(fd, iobuf, SECSIZE);	/* Start the drive, load the heads */
	lseek(fd, lba*SECSIZE, SEEK_SET);
	if (!bs) bs = spt;
	if (step) step = fd_geo.cylinders;

	/* iobuffer start/end pages must be equal (in the same physical 64k page) 
	 * to avoid IO operations to be split in order to accomodate the limitations
	 * of the system DMA mechanism */
	start_dma_page = (_FP_SEG(iobuf) + ((__u16) iobuf >> 4)) >> 12;
	end_dma_page = (_FP_SEG(iobuf) + ((__u16) (iobuf + spt * 512 - 1) >> 4)) >> 12;
	normalized_seg = (((__u32)_FP_SEG(iobuf) + (_FP_OFF(iobuf) >> 4)) << 16) +
			(_FP_OFF(iobuf) & 15);
	if (verbose) {
		fprintf(stderr, "[iobuf @ %04x:%04x] ", _FP_SEG(iobuf), (unsigned int)iobuf);
		fprintf(stderr, "dma start page %x, dma end page %x, buffer at %x:%x - ",
			start_dma_page, end_dma_page, _FP_SEG(normalized_seg), _FP_OFF(normalized_seg));
		fprintf(stderr, start_dma_page == end_dma_page? "OK\n": "ERROR\n");
	}
	if (start_dma_page != end_dma_page)
		fprintf(stderr,
		   "Warning: Buffer spans 64k boundary, IO will be split, timing affected!\n");

	if (direct) {
		int blocks = 0;
		unsigned int rem;
		unsigned int tt, maxblock = (2 * fd_geo.cylinders * spt)/bs; 
		unsigned long ms;
		struct timeval m_start, m_end;

		fprintf(stderr, "Reading %s from cyl %d, spt %d, clustersize %d sectors\n", 
				 device, cylinder, spt, bs);

		gettimeofday(&m_start, NULL);
		while (read(fd, iobuf, bs*SECSIZE) > 0 && blocks < maxblock) {
			blocks++; 
			if (!verbose) write(2, ".", 1);
		}
		gettimeofday(&m_end, NULL);
		ms = (((m_end.tv_sec * 1000000) + m_end.tv_usec) - 
		     ((m_start.tv_sec * 1000000) + m_start.tv_usec))/1000;
		tt = (int)ms/blocks;
		rem = 1000;
		ms = __divmod(ms, &rem);
	        fprintf(stderr, "\nRead %d clusters in %lu.%us (%u.%us per cluster)\n",
			blocks, ms, rem, tt/1000, tt%1000);
		return(0);
	}

	fprintf(stderr, "%s: Size %dk, %d sects/trk, %d cyls, clustersize %d sects,\n\t   start: %d/%d/%d (lba %ld)\n", 
		device, (fd_geo.heads*fd_geo.cylinders*fd_geo.sectors)>>1, spt, 
		fd_geo.cylinders, bs, cylinder, head, sector, lba);

	/* first do single sector reads of cponsecutive sectors */
	get_ptime();
	for (i=0; i < bs; i++) {
		j = read(fd, iobuf, SECSIZE);
		if (j != SECSIZE)
		    fprintf(stderr, "short read, expected %d, got %d\n", (int)SECSIZE, j);
	}
	pticks = get_ptime();
	fprintf(stderr, "%2d sectors (1-by-1):\t%lk (%lk/sector)\n", bs, pticks, pticks/bs);
	
	/* repeat, this time with 2 sectors/1k per operation */
	lseek(fd, lba*SECSIZE, SEEK_SET);
	get_ptime();
	for (i=0; i <= bs; i++) {	/* double the read size, keep the number of
					 * reads to improve the validity of the
					 * average. */
		j = read(fd, iobuf, SECSIZE<<1);
		if (j != SECSIZE<<1)
			fprintf(stderr, "short read, expected %d, got %d\n",
							(int)SECSIZE<<1, j);
	}
	pticks = get_ptime();
	fprintf(stderr, "%2d 1k blocks:\t\t%lk (%lk/block)\n", bs, pticks, pticks/bs);

	/* finally, read everything in one op (if possible). there are may ways
	 * to screw up this by manipulating the command line parameters, some times
	 * useful, never disastrous, often meaningless */
	lseek(fd, lba*SECSIZE, SEEK_SET);
	bs = (bs <= MAXSPT ? bs : MAXSPT);
	get_ptime();
	j = read(fd, iobuf, bs*SECSIZE);
	if (j != bs*SECSIZE)
	    fprintf(stderr, "short read, expected %d, got %d\n",
						bs*(int)SECSIZE, j);
	pticks = get_ptime();
	fprintf(stderr, "%2d sectors in 1 op:\t%#lk\n", bs, pticks);

	/*
	 * Do the step rate test.
	 * Should have an ioctl for stepping the drive (seek) to get this right
	 */
	if (step) {
	    unsigned long a, seek = (long)(spt*step*2 - spt) * SECSIZE;

	    for (j = 0; j < 2; j++) {	/* Do this twice for good measure */
		ave = av_sec_time(fd, spt);
		if (!ave) {
		    fprintf(stderr, "device error, aborting\n");
		    return(2);
		}
		lseek(fd, (off_t)0, SEEK_SET);
		read(fd, iobuf, (size_t)SECSIZE);	/* move to cyl 0 before we start */
		lseek(fd, seek, SEEK_SET);
		get_ptime();
		read(fd, iobuf, (size_t)SECSIZE);
		lseek(fd, (off_t)0, SEEK_SET);
		read(fd, iobuf, (size_t)SECSIZE);
		pticks = get_ptime();
		a = pticks - ave*2L; /* movement time, back and forth */
		fprintf(stderr, "Seek time (%d steps*2): %#lk, step rate %lk (rough estimate)\n", 
				 step, a, a/(2L*(long)step));
	    }
	}
}
