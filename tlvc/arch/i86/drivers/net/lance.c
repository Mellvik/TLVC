/* lance.c: An AMD LANCE ethernet driver for linux. */
/*
    Written 1993 by Donald Becker.

    Copyright 1993 United States Government as represented by the
    Director, National Security Agency.  This software may be used and
    distributed according to the terms of the GNU Public License,
    incorporated herein by reference.

    This driver is for the Allied Telesis AT1500 and HP J2405A, and should work
    with most other LANCE-based bus-master (NE2100 clone) ethercards.

    The author may be reached as becker@super.org or
    C/O Supercomputing Research Ctr., 17100 Science Dr., Bowie MD 20715

    Rewritten for TLVC by @mellvik/Helge Skrivervik july 2025.
    Copied from https://github.com/zealoussnow/linux1.0/blob/master/drivers/net/lance.c
    static char *version = "lance.c:v0.14g 12/21/93 becker@super.org\n";
*/

/*
  		Theory of Operation

I. Board Compatibility

This device driver is designed for the AMD 79C960, the "PCnet-ISA
single-chip ethernet controller for ISA".  This chip is used in a wide
variety of boards from vendors such as Allied Telesis, HP, Kingston,
and Boca.  This driver is also intended to work with older AMD 7990
designs, such as the NE1500 and NE2100.  For convenience, I use the name
LANCE to refer to either AMD chip.

II. Board-specific settings

The driver is designed to work the boards that use the faster
bus-master mode, rather than in shared memory mode.  (Only older designs
have on-board buffer memory needed to support the slower shared memory mode.)

Most boards have jumpered settings for the I/O base, IRQ line, and DMA channel.
This driver probes the likely base addresses, {0x300, 0x320, 0x340, 0x360}.
After the board is found it generates an DMA-timeout interrupt and uses
autoIRQ to find the IRQ line.  The DMA channel defaults to LANCE_DMA, or it
can be set with the low bits of the otherwise-unused dev->mem_start value.

The HP-J2405A board is an exception: with this board it's easy to read the
EEPROM-set values for the base, IRQ, and DMA.  Of course you must already
_know_ the base address, but that entry is for changing the EEPROM.

III. Driver operation

IIIa. Ring buffers
The LANCE uses ring buffers of Tx and Rx descriptors.  Each entry describes
the base and length of the data buffer, along with status bits.  The length
of these buffers is set by LANCE_LOG_{RX,TX}_BUFFERS, which is log_2() of
the buffer length (rather than being directly the buffer length) for
implementation ease.  The current values are 2 (Tx) and 4 (Rx), which leads to
ring sizes of 4 (Tx) and 16 (Rx).  Increasing the number of ring entries
needlessly uses extra space and reduces the chance that an upper layer will
be able to reorder queued Tx packets based on priority.  Decreasing the number
of entries makes it more difficult to achieve back-to-back packet transmission
and increases the chance that Rx ring will overflow.  (Consider the worst case
of receiving back-to-back minimum-sized packets.)

The LANCE has the capability to "chain" both Rx and Tx buffers, but this driver
statically allocates full-sized (slightly oversized -- PKT_BUF_SZ) buffers to
avoid the administrative overhead. For the Rx side this avoids dynamically
allocating full-sized buffers "just in case", at the expense of a
memory-to-memory data copy for each packet received.  For most systems this
is an good tradeoff: the Rx buffer will always be in low memory, the copy
is inexpensive, and it primes the cache for later packet processing.  For Tx
the buffers are only used when needed as low-memory bounce buffers.

IIIB. 16M memory limitations.
For the ISA bus master mode all structures used directly by the LANCE,
the initialization block, Rx and Tx rings, and data buffers, must be
accessable from the ISA bus, i.e. in the lower 16M of real memory.
This is a problem for current Linux kernels on >16M machines. The network
devices are initialized after memory initialization, and the kernel doles out
memory from the top of memory downward.  The current solution is to have a
special network initialization routine that's called before memory
initialization; this will eventually be generalized for all network devices.
As mentioned before, low-memory "bounce-buffers" are used when needed.

IIIC. Synchronization
The driver runs as two independent, single-threaded flows of control.  One
is the send-packet routine, which enforces single-threaded use by the
dev->tbusy flag.  The other thread is the interrupt handler, which is single
threaded by the hardware and other software.

The send packet thread has partial control over the Tx ring and 'dev->tbusy'
flag.  It sets the tbusy flag whenever it's queuing a Tx packet. If the next
queue slot is empty, it clears the tbusy flag when finished otherwise it sets
the 'lp->tx_full' flag.

The interrupt handler has exclusive control over the Rx ring and records stats
from the Tx ring.  (The Tx-done interrupt can't be selectively turned off, so
we can't avoid the interrupt overhead by having the Tx routine reap the Tx
stats.)  After reaping the stats, it marks the queue entry as empty by setting
the 'base' to zero.  Iff the 'lp->tx_full' flag is set, it clears both the
tx_full and tbusy flags.
*/


#include <arch/io.h>
#include <arch/ports.h>
#include <arch/segment.h>
#include <arch/dma.h>
#include <arch/system.h>
#include <arch/bitops.h>

#include <linuxmt/memory.h>
#include <linuxmt/errno.h>
#include <linuxmt/major.h>
#include <linuxmt/ioctl.h>
#include <linuxmt/fcntl.h>
#include <linuxmt/fs.h>
#include <linuxmt/sched.h>
#include <linuxmt/limits.h>
#include <linuxmt/mm.h>
#include <linuxmt/heap.h>
#include <linuxmt/debug.h>
#include <linuxmt/kernel.h>     /* for ARRAY_SIZE(x) */
#include <linuxmt/string.h>     /* for memset */
#include <linuxmt/netstat.h>

#include <netinet/in.h>
#include "eth-msgs.h"

#define _MK_LINADDR(seg, offs) ((unsigned long)((((unsigned long)(seg)) << 4) + (unsigned)(offs)))

extern struct eth eths[];

/* runtime configuration set in /bootopts or defaults in ports.h */
#define net_irq     (netif_parms[ETH_LANCE].irq)
#define net_port    (netif_parms[ETH_LANCE].port)
#define net_dma	    (netif_parms[ETH_LANCE].ram)
#define net_flags   (netif_parms[ETH_LANCE].flags)

static struct netif_stat netif_stat;
static char model_name[] = "lance";
static char dev_name[] = "le0";

#define LANCE_DEBUG 0
#define final_version

#ifdef LANCE_DEBUG
int lance_debug = LANCE_DEBUG;
#endif

#ifndef LANCE_DMA
#define LANCE_DMA	5
#endif


/* Set the number of Tx and Rx buffers, using Log_2(# buffers).
   Reasonable default values are 4 Tx buffers, and 16 Rx buffers.
   That translates to 2 (4 == 2^^2) and 4 (16 == 2^^4).

   TLVC: Using minimal buffers (4rx/2tx) when allocating from the
   heap. otherwise (regular memory or XMS), use 16rx/4tx like above.
   With XMS there is ample space for more buffers, but it 
   doesn't seem useful (and they do eat some heap space for
   headers). Regular memory is the default. XMS is chosen if 
   available. Heap is a compile time option only - for
   simplicity: The buffer headers are statically allocated
   and changing that is just too messy for the benefit.
 */

//#define USE_HEAP
#ifdef USE_HEAP
#define LANCE_LOG_TX_BUFFERS 1	/* keep small! May even try zero for TX (1 buffer). */
#define LANCE_LOG_RX_BUFFERS 2
#else				/* Real mem or XMS */
#define LANCE_LOG_TX_BUFFERS 2
#define LANCE_LOG_RX_BUFFERS 4
static ramdesc_t xmsbase;
extern int xms_avail;
#endif

#define MEM	0
#define XMS	1
#define HEAP	2
static int memtype;
static const char *mem[] = { "main", "xms", "heap"};

#define TX_RING_SIZE		((unsigned)(1 << (LANCE_LOG_TX_BUFFERS)))
#define TX_RING_MOD_MASK	(TX_RING_SIZE - 1)
#define TX_RING_LEN_BITS	((long)(LANCE_LOG_TX_BUFFERS) << 29)

#define RX_RING_SIZE		((unsigned)(1 << (LANCE_LOG_RX_BUFFERS)))
#define RX_RING_MOD_MASK	(RX_RING_SIZE - 1)
#define RX_RING_LEN_BITS	((long)(LANCE_LOG_RX_BUFFERS) << 29)

#define PKT_BUF_SZ	1544

/* Offsets from base I/O address. */
#define LANCE_DATA 0x10
#define LANCE_ADDR 0x12
#define LANCE_RESET 0x14
#define LANCE_BUS_IF 0x16
#define LANCE_TOTAL_SIZE 0x18

/* The LANCE Rx and Tx ring descriptors. */
struct lance_rx_head {
    long base;
    short buf_length;		/* This length is 2's complement (negative)! */
    short msg_length;		/* This length is "normal". */
};

struct lance_tx_head {
    long  base;
    short length;		/* Length is 2's complement (negative)! */
    short misc;
};

/* The LANCE initialization block, described in databook. */
struct lance_init_block {
    unsigned short mode;	/* Pre-set mode (reg. 15) */
    unsigned char phys_addr[6];	/* Physical ethernet address */
    unsigned long filter[2];		/* Multicast filter (unused). */
    /* Receive and transmit ring base, along with extra bits. */
    unsigned long rx_ring;		/* Tx and Rx ring base pointers */
    unsigned long tx_ring;		/* includes length in the high byte */
};

#if 0
/* Ethernet statistics collection data. */	/* FIX: COMPACT this */
struct enet_statistics {
  int	rx_packets;			/* total packets received	*/
  int	tx_packets;			/* total packets transmitted	*/
  int	rx_errors;			/* bad packets received		*/
  int	tx_errors;			/* packet transmit problems	*/
  int	rx_dropped;			/* no space in linux buffers	*/
  int	tx_dropped;			/* no space available in linux	*/
  //int	multicast;			/* multicast packets received	*/
  int	collisions;

  /* detailed rx_errors: */
  int	rx_length_errors;
  int	rx_over_errors;			/* receiver ring buff overflow	*/
  int	rx_crc_errors;			/* recved pkt with crc error	*/
  int	rx_frame_errors;		/* recv'd frame alignment error */
  int	rx_fifo_errors;			/* recv'r fifo overrun		*/
  int	rx_missed_errors;		/* receiver missed packet	*/

  /* detailed tx_errors */
  int	tx_aborted_errors;
  int	tx_carrier_errors;
  int	tx_fifo_errors;
  int	tx_heartbeat_errors;
  int	tx_window_errors;
};
#endif

struct lance_private {
    /* These must aligned on 8-byte boundaries. */
    struct lance_rx_head rx_ring[RX_RING_SIZE];
    struct lance_tx_head tx_ring[TX_RING_SIZE];
    struct lance_init_block	init_block;
    unsigned rx_buffs;		/* Address (offset) of Rx and Tx buffers. */
    unsigned tx_buffs;
    int	cur_rx, cur_tx;		/* The next free ring entry */
    int dirty_tx;		/* The ring entries to be free()ed. */
    //int dma;
    //struct enet_statistics stats;	/* FIXME: Eliminate! */
    char old_lance;
    long pad0, pad1;		/* Used for alignment */
};

struct lance_private *thisdev;
static unsigned char found;
static unsigned char usecount;  
static unsigned short verbose;
static struct wait_queue rxwait;
static struct wait_queue txwait;
static byte_t tbusy, dev_interrupt;
static unsigned long tstart;
static seg_t ring_seg;

static int INITPROC lance_probe1(word_t);
static int lance_open(struct inode *inode, struct file *filp);
static size_t lance_read(struct inode *, struct file *, char *, size_t);
static size_t lance_write(struct inode *, struct file *, char *, size_t);
static int lance_ioctl(struct inode *, struct file *, unsigned int, unsigned int);
int lance_select(struct inode *, struct file *, int);
static void lance_init_ring(void);
static void lance_load_iblock(struct lance_private *);
static int lance_start_xmit(char *, size_t);
static int lance_rx(char *, size_t);
static void lance_interrupt(int irq, struct pt_regs *regs);
static void lance_release(struct inode *inode, struct file *filp);
//static struct enet_statistics *lance_get_stats(struct device *dev);
#ifdef HAVE_MULTICAST
static void set_multicast_list(struct device *dev, int num_addrs, void *addrs);
#endif


struct file_operations lance_fops =
{
	NULL,        /* lseek */
	lance_read,
	lance_write,
	NULL,        /* readdir */
	lance_select,
	lance_ioctl,
	lance_open,
	lance_release
};

static int port_list[] = {0x300, 0x320, 0x340, 0x360, 0};
void INITPROC lance_drv_init(void) {

	int err;
	word_t ioaddr = 0;	/* initialize to keep gcc happy */
	int *port;

	printk("eth: %s ", dev_name);
	if (arch_cpu < 6) {
	    printk("needs 286 or higher\n");
	    return;
	}
        if (!net_port) {	/* interface may be turned off in bootopts */
            printk("null-port in bootopts, ignored\n");
            return;
        }
	for (port = &port_list[0]; *port; port++) {
	    ioaddr = *port;

	    /* ne2k style: Look for 2x'W' (for 'word') at the end of the PROM */
	    if (inb(ioaddr + 14) == 0x57 && inb(ioaddr + 15) == 0x57)
		break;
	}
	if (!*port)
	    err = -ENODEV;
	else {
	    if (!ioaddr)
		err = -ENODEV;
	    else
		err = lance_probe1(ioaddr);
	}

	if (err) 
            printk("not %s\n", err == -ENODEV ? "found":"available");
	else {
            found++;
            eths[ETH_LANCE].stats = &netif_stat;
        }
}

static int INITPROC ind(char *arr, int val)
{
    int i = 0;
    while (arr[i]) {
	if (arr[i] == (char)val) return i;
	i++;
    }
    return 0;
}

static int INITPROC lance_probe1(word_t ioaddr)
{
    struct lance_private *lp;
    //unsigned long initblk;
    int i, hpJ2405A = 0;
    unsigned reset_val;
    byte_t *mac_addr = (byte_t *)&netif_stat.mac_addr;

    verbose = !!(net_flags&ETHF_VERBOSE);

    /* There is a 16 byte station address PROM at the base address.
       The first six bytes are the station address. */
    for (i = 0; i < 6; i++)
	mac_addr[i] = inb(ioaddr + i);

    /* use the vendor part of the MAC address to detect HP origin */
    hpJ2405A = (mac_addr[0] == 0x08 && mac_addr[1] == 0x00
		&& mac_addr[2] == 0x09);

    /* Reset the LANCE.  */
    reset_val = inw(ioaddr+LANCE_RESET); /* Reset the LANCE */

    /* The Un-Reset is only needed for the real NE2100, and will
       confuse the HP board. */
    if (!hpJ2405A)
	outw(reset_val, ioaddr+LANCE_RESET);

    outw(0x0000, ioaddr+LANCE_ADDR);	/* Reset -> STOP-bit in crs0 is set. */
    if (inw(ioaddr+LANCE_DATA) != 0x0004) /* If not, this is likely something else */
	return -ENODEV;

    /* Match up J2405A settings with bootopts settings, reprogram device if mismatch */
    /* Reset register: */
    /* 7-----6-----5-----4-----3-----2-----1-----0
    | BSY |     IRQ         |    DMA    |    PORT  |
    ------------------------------------------------   */

    if (hpJ2405A) {	/* may override configured values */
	char dma_tbl[] = {3, 5, 6, 7, 0};
	char irq_tbl[] = {3, 4, 5, 9, 10, 11, 12, 15, 0};
	unsigned int cfg_val = 
		0xff00 | ind(dma_tbl, net_dma)<<2 | ind(irq_tbl,net_irq)<<4 | ((net_port>>5)&3);

	reset_val = inw(ioaddr+LANCE_RESET);
        //printk("(settings: b: %02x(%x/%d/%d), c: %02x) ", 
		//cfg_val, net_port, net_irq, net_dma, reset_val);
	if (cfg_val != reset_val) {
	    outw(cfg_val, ioaddr+LANCE_RESET);
	    if (verbose) printk("saving new settings %x (%x/%d/%d)-> %x (%x/%d/%d)\neth: %s ", reset_val, 
		ioaddr, irq_tbl[(reset_val >> 4) & 7], dma_tbl[(reset_val >> 2) & 3], cfg_val,
		net_port, net_irq, net_dma, dev_name);
	}
    } else if (ioaddr != net_port) {
	printk("IOaddress mismatch, using %x\neth: %s ", ioaddr, dev_name);
	net_port = ioaddr;
    }

    /* FIXME: There are comprehensive mechanisms for chiptype and settings detection
     * in the 'big linux' version, add as required. The current setup will work for
     * most cards as long as bootopts matches the card settings. */

    printk("at %#3x, irq %d, DMA %d, MAC %02x", ioaddr, net_irq,
		net_dma, mac_addr[0]);
    i = 1;  
    while (i < 6) printk(":%02x", (mac_addr[i++]&0xff));

    /* Should move this to open, so we can release it on close, but we need it 
     * to complete initialization. Or do we? FIXME!  */
#ifdef USE_HEAP
    thisdev = (struct lance_private *)heap_alloc(sizeof(struct lance_private)
		+ PKT_BUF_SZ*(RX_RING_SIZE + TX_RING_SIZE), HEAP_TAG_NETWORK);
    if (!thisdev) return -ENOMEM;
    ring_seg = kernel_ds;
#else
    thisdev = (struct lance_private *)heap_alloc(sizeof(struct lance_private), 
		HEAP_TAG_NETWORK);
    if (!thisdev) return -ENOMEM;
#ifdef CONFIG_FS_XMS
    if (xms_avail && (xmsbase = xms_alloc((unsigned)(PKT_BUF_SZ*(RX_RING_SIZE +
		TX_RING_SIZE)+0x3ff)>>10))) {
	ring_seg = 0;
	memtype = XMS;		// MAY USE ring_seg == 0 as flag for xms 
    } else
#endif
    {
	segment_s *rseg = seg_alloc((PKT_BUF_SZ*(RX_RING_SIZE + TX_RING_SIZE)+0xf)>>4, 
		SEG_FLAG_NETBUF);
	if (!rseg) return -ENOMEM;
	ring_seg = rseg->base;
    }
#endif

    /* Make certain the data structures used by the LANCE are aligned. */
    thisdev = (void *)(((int)thisdev + 7) & ~7);
    lp = thisdev;
#ifdef USE_HEAP
    lp->rx_buffs = (unsigned)thisdev + sizeof(struct lance_private);
#else
    lp->rx_buffs = 0;
#endif
    lp->tx_buffs = (unsigned) (lp->rx_buffs + (unsigned)(PKT_BUF_SZ*RX_RING_SIZE));

#ifndef final_version
    /* This should never happen. */
    if ((int)(lp->rx_ring) & 0x07) {
	printk(" **ERROR** LANCE Rx and Tx rings not on even boundary.\n");
	return 1;
    }
#endif

    outw(88, ioaddr+LANCE_ADDR);	/* chip ID register */
    lp->old_lance = (inw(ioaddr+LANCE_DATA) != 0x3003);

    if (verbose) 
	printk(", %s (%x) %s\n", lp->old_lance ? "original LANCE" : "PCnet-ISA LANCE",
			       inw(ioaddr+LANCE_DATA), hpJ2405A ? "(HP J2405A)":"");

#if 0	/* this entire block is repeated in _open(), and isn't needed for the J2405A */
	/* Need more exposure to determine. Likely, the 79c960 doesn't need this */
    lp->init_block.mode = 0x0003;	/* Disable Rx and Tx. */
    lance_load_iblock(lp);
    initblk = _MK_LINADDR(kernel_ds, &lp->init_block);

    outw(0x0001, ioaddr+LANCE_ADDR);
    outw((short)initblk, ioaddr+LANCE_DATA);
    outw(0x0002, ioaddr+LANCE_ADDR);
    outw(initblk >> 16, ioaddr+LANCE_DATA);
    outw(0x0000, ioaddr+LANCE_ADDR);

    if (!lp->old_lance) {
	/* Turn on auto-select of media (10baseT or BNC) so that the user
	   can watch the LEDs even if the board isn't opened. */
	outw(0x0002, ioaddr+LANCE_ADDR);
	outw(0x0002, ioaddr+LANCE_BUS_IF);
    }
#endif

    return 0;
}

int lance_open(struct inode *inode, struct file *filp)
{
    struct lance_private *lp = thisdev;
    int ioaddr = net_port;
    unsigned long initblk;
    int i;

    if (!found) return -ENODEV;

    if (!usecount) {

	i = request_irq(net_irq, lance_interrupt, INT_GENERIC);
	if (i) {
	    printk(EMSG_IRQERR, model_name, net_irq, i);
	    return i;
	}

	if ((i = request_dma(net_dma, (void *)model_name))) {
	    free_irq(net_irq);
	    printk("%s: Cannot allocate DMA chan %d\n", dev_name, net_dma);
	    return i;
	}

	/* Reset the LANCE. This will activate config-changes made in probe1()  */
	inw(ioaddr+LANCE_RESET);

	/* The DMA controller is used as a no-operation slave, "cascade mode". */
	set_dma_mode(net_dma, DMA_MODE_CASCADE);
	enable_dma(net_dma);

	/* Un-Reset the LANCE, needed only for the NE2100. */
	if (lp->old_lance)
	    outw(0, ioaddr+LANCE_RESET);

	if (!lp->old_lance) {
	    /* This is 79C960-specific: Turn on auto-select of media (AUI, BNC). */
	    outw(0x0002, ioaddr+LANCE_ADDR);
	    outw(inw(ioaddr+LANCE_BUS_IF) | 0x0002, ioaddr+LANCE_BUS_IF);
	}

#if LANCE_DEBUG
	if (lance_debug > 1) {
	    printk("%s: lance_open() irq %d dma %d buftype %s, tx/rx rings %#x/%#x init %#.x.\n",
	       dev_name, net_irq, net_dma, mem[memtype], lp->tx_ring, lp->rx_ring,
	       &lp->init_block);
	} else
#endif
	    printk("%s: using %drx/%dtx %s-buffers\n", dev_name, RX_RING_SIZE,
		TX_RING_SIZE, mem[memtype]);

	/* Re-initialize the LANCE, and start it when done. */
	lance_init_ring();
	initblk = _MK_LINADDR(kernel_ds, &lp->init_block);
	outw(0x0001, ioaddr+LANCE_ADDR);
	outw((unsigned short)initblk, ioaddr+LANCE_DATA);
	outw(0x0002, ioaddr+LANCE_ADDR);
	outw((unsigned short)(initblk >> 16), ioaddr+LANCE_DATA);

	outw(0x0004, ioaddr+LANCE_ADDR);
	outw(0x0d15, ioaddr+LANCE_DATA);	/* was 0x0d15, big linux has 0x0915  */
						/* autopad xmit, autostrip rcv, MFCOM, TXSTRTM, JABM */
	tbusy = 0;
	dev_interrupt = 0;
#if 0
	outw(15, ioaddr+LANCE_ADDR);
	printk("le0: csr15 before init: %04x\n", inw(ioaddr+LANCE_DATA));
	outw(0x1880, ioaddr+LANCE_DATA);	/* EXPERIMENTAL; turn off DAPC (auto-polarity detection)
						 * (0x*8**) and DLNKTST (TP link status monitoring)  (0x1***) */
						/* PORTSEL (0x**8* = TP) */
						/* NOTE: may not have any effect unless we explicitly
						 * select the TP interface */
#endif
	/* Give the network port time to acquaint itself with the net, otherwise our first packet out,
	 * the gratuituous ARP packet which ktcp sends immediately on startup, will fail with a CERR
	 * code, meaning Collission, which happens then the TP interface hasn't figured out its
	 * physical environment yet. The error is temporary, and affects only this early first packet.
	 * It is however, sufficiently annoying to qualify this delay. The alternative would be to
	 * send the ARP packet twice in ktcp. */

	tstart = jiffies;
	while (jiffies < (tstart + 7)) asm("nop");

	outw(0x0000, ioaddr+LANCE_ADDR);
	outw(0x0001, ioaddr+LANCE_DATA);	/* let the NIC initialize */

	i = 0;
	while (i++ < 100) 	/* may be interrupted, but it's OK */
	    if (inw(ioaddr+LANCE_DATA) & 0x0100)	/* wait for initialization done */
		break;
	if (i >= 100) {	/* this should not happen */
	    printk("le0: open device failed, csr0: %04x\n", inw(ioaddr+LANCE_DATA));
	    //return -EIO;	/* need to let it run through so _release can clean up */
	    // FIXME: clean up allocated resources before returning the appropriate error code
	}

	outw(0x0142, ioaddr+LANCE_DATA);	/* Enable interrupts, start the NIC, clear *
						 * the 'initialization done' (IDONE) bit */ 

#if LANCE_DEBUG
	if (lance_debug > 2)
	    printk("%s: LANCE open after %d ticks, init block %x csr0 %x.\n",
	       dev_name, i, &lp->init_block, inw(ioaddr+LANCE_DATA));
#endif

    }
    usecount++;
    return 0;
}

/* Initialize the LANCE Rx and Tx rings. */
/* Check and compensate for 64k boundary crossing */
static void lance_init_ring(void)
{
    struct lance_private *lp = thisdev;
    long linaddr;
    int i;
    int adjust = 0;	/* Use if we decide to ditch the low mem bounce buffer and just
			 * allocate one extra here just in case. Or instead, we change the allocation
			 * mechanisms to guarantee no-64k-borders */

    lp->cur_rx = lp->cur_tx = 0;
    lp->dirty_tx = 0;
    lp->init_block.mode = 0x0000;

    for (i = 0; i < (RX_RING_SIZE+TX_RING_SIZE); i++) {
#ifndef USE_HEAP
	if (memtype == XMS)
	    linaddr = xmsbase + (long)((i+adjust)*PKT_BUF_SZ);
	else
#endif
	    linaddr = _MK_LINADDR(ring_seg, lp->rx_buffs + (i+adjust)*PKT_BUF_SZ);

	if (((unsigned)linaddr + PKT_BUF_SZ) < PKT_BUF_SZ) {	/* spans 64k phys boundary */
	    linaddr = _MK_LINADDR(XD_BOUNCESEG, 0);
	    //adjust++; i--; continue;
	}
	if (i < RX_RING_SIZE) {
	    lp->rx_ring[i].buf_length = -PKT_BUF_SZ;
	    lp->rx_ring[i].base = linaddr | 0x80000000;
	} else
	    lp->tx_ring[i-RX_RING_SIZE].base = linaddr;
	//if (lance_debug > 2) printk("%d: %lx; ", i, linaddr);
    }
    lance_load_iblock(lp);	/* may be superfluous since we did this in _probe */
}

static void lance_load_iblock(struct lance_private *lp)
{
    byte_t *mac_addr = (byte_t *)&netif_stat.mac_addr;

    for (int i = 0; i < 6; i++)
	lp->init_block.phys_addr[i] = mac_addr[i];
    lp->init_block.filter[0] = 0x00000000;
    lp->init_block.filter[1] = 0x00000000;
    lp->init_block.rx_ring = _MK_LINADDR(kernel_ds, lp->rx_ring) | RX_RING_LEN_BITS;
    lp->init_block.tx_ring = _MK_LINADDR(kernel_ds, lp->tx_ring) | TX_RING_LEN_BITS;
}

/*
 * Move data from user space to the TX ring buffer, kick off sending if the
 * transmitter is free
 */
static int lance_start_xmit(char *data, size_t len)
{
    struct lance_private *lp = thisdev;
    long base;
    int ioaddr = net_port;
    int entry;

    /* Transmitter timeout, serious problems. */
    if (tbusy) {
	long tickssofar = jiffies - tstart;
	if (tickssofar < 10)
	    return -EAGAIN;
	outw(0, ioaddr+LANCE_ADDR);
	printk("%s: transmit timed out, status %4x, resetting.\n",
	       dev_name, inw(ioaddr+LANCE_DATA));
	outw(0x0001, ioaddr+LANCE_DATA);
	netif_stat.tx_errors++;
#ifndef final_version
	{
	    int i;
	    printk(" Ring data dump: dirty_tx %d cur_tx %d cur_rx %d.",
		   lp->dirty_tx, lp->cur_tx, lp->cur_rx);
	    for (i = 0 ; i < RX_RING_SIZE; i++)
		printk("%s %08lx %04x %04x", i & 0x3 ? "" : "\n ",
		       lp->rx_ring[i].base, -lp->rx_ring[i].buf_length,
		       lp->rx_ring[i].msg_length);
	    for (i = 0 ; i < TX_RING_SIZE; i++)
		printk(" %s%08lx %04x %04x", i & 0x3 ? "" : "\n ",
		       lp->tx_ring[i].base, -lp->tx_ring[i].length,
		       lp->tx_ring[i].misc);
	    printk("\n");
	}
#endif
	lance_init_ring();
	//outw(0x0043, ioaddr+LANCE_DATA);	/* STRT|INIT|IENA */
	outw(0x0142, ioaddr+LANCE_DATA);	/* IDON|STRT|IENA */

	tbusy = 0;
	tstart = jiffies;

	return -ETIME;
    }

#if LANCE_DEBUG
    if (lance_debug > 3) {
	outw(0x0000, ioaddr+LANCE_ADDR);
	printk("%s: lance_start_xmit(): csr0 %x.\n", dev_name,
	       inw(ioaddr+LANCE_DATA));
	outw(0x0000, ioaddr+LANCE_DATA);
    }
#endif

    /* Block a timer-based transmit from overlapping.  This could better be
       done with atomic_swap(1, dev->tbusy), but set_bit() works as well. */
    if (set_bit(0, (void *)&tbusy) != 0)
	printk("%s: Transmitter access conflict.\n", dev_name);

    /* Fill in a Tx ring entry */

    /* Mask to ring buffer boundary. */
    entry = lp->cur_tx & TX_RING_MOD_MASK;

    /* Caution: the write order is important here, set the base address
       with the "ownership" bits last. */

    /* The old LANCE chips doesn't automatically pad buffers to min. size. */
    if (lp->old_lance) {
	lp->tx_ring[entry].length =
	    -(64 < len ? len : 64);
    } else
	lp->tx_ring[entry].length = -len;

    lp->tx_ring[entry].misc = 0x0000;

    /* FIXME: when using XMS buffers, we may need (a version of) this even in TLVC */
#if 0
    /* If any part of this buffer is >16M we must copy it to a low-memory
       buffer. */
    if ((int)(skb->data) + skb->len > 0x01000000) {
#if LANCE_DEBUG
	if (lance_debug > 5)
	    printk("%s: bouncing a high-memory packet (%#x).\n",
		   dev->name, (int)(skb->data));
#endif
	memcpy(&lp->tx_bounce_buffs[entry], skb->data, skb->len);
	lp->tx_ring[entry].base =
	    (int)(lp->tx_bounce_buffs + entry) | 0x83000000;
	if (skb->free)
	    kfree_skb (skb, FREE_WRITE);
    } else {
    	/* We can't free the packet yet, so we inform the memory management
	   code that we are still using it. */
    	if(skb->free==0)
    		skb_kept_by_device(skb);
	lp->tx_ring[entry].base = (int)(skb->data) | 0x83000000;
    }
#endif
    base = lp->tx_ring[entry].base & 0x00ffffff;
    if (memtype == XMS)
	xms_fmemcpyb(0, base, data, current->t_regs.ds, len);
    else
	fmemcpyw((void *)(word_t)(base&0xf), (seg_t)(base>>4), 
		data, current->t_regs.ds, (len+1)>>1);
    lp->tx_ring[entry].base |= 0x83000000;
    lp->cur_tx++;

    /* Trigger an immediate send poll. */
    outw(0x0000, ioaddr+LANCE_ADDR);
    outw(0x0048, ioaddr+LANCE_DATA);

    tstart = jiffies;

    if (((lp->tx_ring[(entry+1) & TX_RING_MOD_MASK].base)&0xff000000) == 0)
	tbusy = 0;

    return len;
}

/* The LANCE interrupt handler. */
static void lance_interrupt(int irq, struct pt_regs *regs)
{
    struct lance_private *lp;
    int csr0, ioaddr;

    //kputchar('I');
    ioaddr = net_port;
    lp = thisdev;
    if (dev_interrupt)
	printk("%s: Re-entering the interrupt handler.\n", dev_name);

    dev_interrupt = 1;

    outw(0x00, ioaddr + LANCE_ADDR);
    csr0 = inw(ioaddr + LANCE_DATA);

    /* block interrupts while processing */
    outw(csr0 & ~0x004f, ioaddr + LANCE_DATA);

#if LANCE_DEBUG
    if (lance_debug > 4)
	printk("%s: interrupt  csr0=%04x new csr=%04x.\n",
	       dev_name, csr0, inw(ioaddr + LANCE_DATA));
    else if (lance_debug)
	printk("I%04x;", csr0);
#endif

    if (csr0 & 0x0400)		/* Rx interrupt */
	wake_up(&rxwait);

    if (csr0 & 0x0200) {	/* Tx-done interrupt */
	int dirty_tx = lp->dirty_tx;

	while (dirty_tx < lp->cur_tx) {
	    int entry = dirty_tx & TX_RING_MOD_MASK;
	    int status = (int)(lp->tx_ring[entry].base>>16);
	    
	    if (status < 0)
		break;		/* It still hasn't been Txed */

	    lp->tx_ring[entry].base &= 0x00ffffff;

	/* FIXME: use the TLVC stats struct, simplify ... */
	    if (status & 0x4000) { /* There was an major error, log it. */
		int err_status = lp->tx_ring[entry].misc;
		netif_stat.tx_errors++;
		//if (err_status & 0x0400) lp->stats.tx_aborted_errors++;
		//if (err_status & 0x0800) lp->stats.tx_carrier_errors++;
		//if (err_status & 0x1000) lp->stats.tx_window_errors++;
		//if (err_status & 0x4000) lp->stats.tx_fifo_errors++;
		if (err_status & 0x4000) netif_stat.oflow_errors++;
		/* Perhaps we should re-init() after the FIFO error. */
#if 0
	    } else {
		if (status & 0x1800)
		    lp->stats.collisions++;
		lp->stats.tx_packets++;
#endif
	    }
	    dirty_tx++;
	}

	if (lp->cur_tx - dirty_tx >= TX_RING_SIZE) {	/* will happen every time cur_tx wraps around */
	    //printk("le0: wrap %d %d, new dirty_tx %d\n", lp->cur_tx, dirty_tx, dirty_tx + TX_RING_SIZE);
	    dirty_tx += TX_RING_SIZE;
	}

	if (tbusy  &&  dirty_tx > (lp->cur_tx - TX_RING_SIZE + 2)) {
	    /* The ring is no longer full, clear tbusy. */
	    tbusy = 0;
	}

	lp->dirty_tx = dirty_tx;
	wake_up(&txwait);
    }

    if (csr0 & 0x9000) netif_stat.rx_errors++;

    /* Clear the interrupts we've handled. */
    /* we actually cleared thos above, this is simply enabling interrupts again */
    outw(0x0000, ioaddr + LANCE_ADDR);
    outw(0x7f40, ioaddr + LANCE_DATA);

#if LANCE_DEBUG
    if (lance_debug > 4)
	printk("%s: exiting interrupt, csr%d=%x.\n",
	       dev_name, inw(ioaddr + LANCE_ADDR),
	       inw(ioaddr + LANCE_DATA));
#endif

    dev_interrupt = 0;
    return;
}

#if LANCE_DEBUG
static void dump_data(unsigned long data, int len)
{
	int i = 0;
	char buf[16];

	while (i < len) {
		if (!(i&0xf)) {
		    xms_fmemcpyb(buf, kernel_ds, 0, data, 16);
		    data += 16L;
		}
		printk("%02x ", buf[i]&0xff);
		if (!(++i&0xf)) printk("\n");
	}
}
#endif

/* move data from the ring buffer to user_space, return actual byte count. */
/* 0 means 'no data available' */
static int lance_rx(char *data, size_t len)
{
    struct lance_private *lp = thisdev;
    int entry = lp->cur_rx & RX_RING_MOD_MASK;
    int status, pkt_len = 0;
	
    /* If we own the next entry, it's a new packet. Check for errors, send
     * data up, free buffer */
    while (lp->rx_ring[entry].base >= 0) {
	status = lp->rx_ring[entry].base >> 24;

	if (status != 0x03) {		/* There was an error. */
	    /* There is an tricky error noted by John Murphy,
	       <murf@perftech.com> to Russ Nelson: Even with full-sized
	       buffers it's possible for a jabber packet to use two
	       buffers, with only the last correctly noting the error. */
	    if (status & 0x01)	/* Only count a general error at the */
		netif_stat.rx_errors++; /* end of a packet.*/
	    //if (status & 0x20) lp->stats.rx_frame_errors++;
	    //if (status & 0x10) lp->stats.rx_over_errors++;
	    if (status & 0x10) netif_stat.oflow_errors++;
	    //if (status & 0x08) lp->stats.rx_crc_errors++;
	    //if (status & 0x04) lp->stats.rx_fifo_errors++;
	    if (status & 0x04) netif_stat.oflow_errors++;
	    printk("le0: rx error %x\n", status);
	} else {
	    /* no errors, move data to user space */
	    pkt_len = lp->rx_ring[entry].msg_length;

	    if (pkt_len > len) {	/* Superfluous - should be done by the chip */
		printk("%s: oversized packet (%d), truncated\n", pkt_len);
		pkt_len = len;
	    }
	    long base = lp->rx_ring[entry].base & 0x00ffffff;
	    if (memtype == XMS) {
		xms_fmemcpyb(data, current->t_regs.ds, 0, base, pkt_len);
#if LANCE_DEBUG
		if (lance_debug > 1) printk("READ: (%d) %lx -> %x:%x\n", pkt_len, base,
		    current->t_regs.ds, data);
#endif
	    } else {
		fmemcpyw(data, current->t_regs.ds, (void *)(word_t)(base&0xf),
		    (seg_t)(base>>4), (pkt_len+1)>>1);
#if LANCE_DEBUG
		if (lance_debug > 1) printk("READ: (%d) %x:%x -> %x:%x\n", pkt_len, (seg_t)(base>>4),
		    (word_t)(base&0xf), current->t_regs.ds, data);
#endif
	    }
#if LANCE_DEBUG
	    if (lance_debug > 4) dump_data(base, pkt_len < 64 ? pkt_len:64);
#endif
	}

	lp->rx_ring[entry].base |= 0x80000000;		/* release buffer */
	lp->cur_rx++;
	if (pkt_len) break;
    }
    return pkt_len;
}

static void lance_release(struct inode *inode, struct file *file)
{
    int ioaddr = net_port;

    if (--usecount) return;

    tbusy = 1;
#if 0
    struct lance_private *lp = thisdev;
    outw(112, ioaddr+LANCE_ADDR);
    lp->stats.rx_missed_errors = inw(ioaddr+LANCE_DATA);
#endif

    outw(0, ioaddr+LANCE_ADDR);

#if LANCE_DEBUG
    if (lance_debug > 1)
	printk("%s: Shutting down ethercard, status was %2x.\n",
	       dev_name, inw(ioaddr+LANCE_DATA));
#endif

    /* We stop the LANCE here -- it occasionally polls
       memory if we don't. */
    outw(0x0004, ioaddr+LANCE_DATA);

    free_irq(net_irq);

    disable_dma(net_dma);
    free_dma(net_dma);

    return;
}
#if 0
static struct enet_statistics *
lance_get_stats(struct device *dev)
{
    struct lance_private *lp = (struct lance_private *)dev->priv;
    short ioaddr = dev->base_addr;
    short saved_addr;

    cli();
    saved_addr = inw(ioaddr+LANCE_ADDR);
    outw(112, ioaddr+LANCE_ADDR);
    lp->stats.rx_missed_errors = inw(ioaddr+LANCE_DATA);
    outw(saved_addr, ioaddr+LANCE_ADDR);
    sti();

    return &lp->stats;
}
#endif

static size_t lance_write(struct inode *inode, struct file *file, char *data, size_t len)
{
    struct lance_private *lp = thisdev;
    short entry = lp->cur_tx & TX_RING_MOD_MASK;
    int res;

    while (1) {
	//kputchar('T');
	prepare_to_wait_interruptible(&txwait);

	if (lp->tx_ring[entry].base & 0x80000000) {	//  buffer busy?
	    if (file->f_flags & O_NONBLOCK) {
		res = -EAGAIN;
		break;
	    }
	    do_wait();
	    if (current->signal) {
		res = -EINTR;
		break;
	    }
	}
	res = lance_start_xmit(data, len);
	break;
    }
    finish_wait(&txwait);
    return res;
}

int lance_select(struct inode *inode, struct file *filp, int sel_type)
{       
    struct lance_private *lp = thisdev;
    short entry;
    int res = 0;
	
    switch (sel_type) {
	case SEL_OUT:
	    entry = lp->cur_tx & TX_RING_MOD_MASK;
	    if (lp->tx_ring[entry].base & 0x80000000) {	//  buffer busy?
		//kputchar('O');
		select_wait(&txwait);
		break;
	    }
	    //kputchar('o');
	    res = 1;
	    break;
		
	case SEL_IN:
	    entry = lp->cur_rx & RX_RING_MOD_MASK;
	    if (lp->rx_ring[entry].base & 0x80000000) {	// data in NIC buffer??
		//kputchar('W');
		select_wait(&rxwait);
		break;
	    }
	    //kputchar('w');
	    res = 1;
	    break;
	
	default:
	    res = -EINVAL;
    }
    return res;
}

static int lance_ioctl(struct inode *inode, struct file *file, unsigned int cmd, unsigned int arg)
{       
	int err = 0;
	byte_t *mac_addr = (byte_t *)&netif_stat.mac_addr;
	
	switch (cmd) {
		case IOCTL_ETH_ADDR_GET:
			err = verified_memcpy_tofs((byte_t *)arg, mac_addr, 6);
			break;

		case IOCTL_ETH_GETSTAT:
			/* Return the entire netif_struct */
			err = verified_memcpy_tofs((char *)arg, &netif_stat, sizeof(netif_stat));
			break;
		
		default:
			err = -EINVAL;
	
	}
	return err;
}

static size_t lance_read(struct inode *inode, struct file *filp, char *data, size_t len)
{
    int res;

    while(1) {
	//kputchar('R');
	prepare_to_wait_interruptible(&rxwait);

	if ((res = lance_rx(data, len)) < 0) { // No data in buffer
	    if (filp->f_flags & O_NONBLOCK) {
		res = -EAGAIN;
		break;
	    }
	    //kputchar('!');
	    do_wait();
	    if (current->signal) {
		res = -EINTR;
		break;
	    }
	}

	if (res == 0) {		/* Probably superfluous, delete in final */
	    printk("%s: Network read error (%d)\n", dev_name, res);
	    res = -EIO;
	}
	break;
    }
	
    finish_wait(&rxwait);
    return res;
}

#ifdef HAVE_MULTICAST
/* Set or clear the multicast filter for this adaptor.
   num_addrs == -1	Promiscuous mode, receive all packets
   num_addrs == 0	Normal mode, clear multicast list
   num_addrs > 0	Multicast mode, receive normal and MC packets, and do
   			best-effort filtering.
 */
static void
set_multicast_list(struct device *dev, int num_addrs, void *addrs)
{
    short ioaddr = dev->base_addr;

    /* We take the simple way out and always enable promiscuous mode. */
    outw(0, ioaddr+LANCE_ADDR);
    outw(0x0004, ioaddr+LANCE_DATA); /* Temporarily stop the lance.  */

    outw(15, ioaddr+LANCE_ADDR);
    if (num_addrs >= 0) {
	short multicast_table[4];
	int i;
	/* We don't use the multicast table, but rely on upper-layer filtering. */
	memset(multicast_table, (num_addrs == 0) ? 0 : -1, sizeof(multicast_table));
	for (i = 0; i < 4; i++) {
	    outw(8 + i, ioaddr+LANCE_ADDR);
	    outw(multicast_table[i], ioaddr+LANCE_DATA);
	}
	outw(0x0000, ioaddr+LANCE_DATA); /* Unset promiscuous mode */
    } else {
	outw(0x8000, ioaddr+LANCE_DATA); /* Set promiscuous mode */
    }

    outw(0, ioaddr+LANCE_ADDR);
    outw(0x0142, ioaddr+LANCE_DATA); /* Resume normal operation. */
}
#endif

