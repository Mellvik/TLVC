/*
 *
 * NE2K Ethernet driver - supports NICs using the NS DP8390 and
 * compatible chip sets. 
 *
 * 8 bit i/f support and autoconfig added by Helge Skrivervik (@mellvik) april 2022
 * based on the RTL8019AS chip in the interface card from Weird Electronics.
 *
 * TLVC FEB 2023 - Added experimental io buffering (Helge Skrivervik)
 *
 * For buffer configuration, see netbuf.h
 */

#include <arch/io.h>
#include <linuxmt/errno.h>
#include <linuxmt/major.h>
#include <linuxmt/ioctl.h>
#include <linuxmt/fcntl.h>
#include <linuxmt/fs.h>
#include <linuxmt/sched.h>
#include <linuxmt/limits.h>
#include <linuxmt/mm.h>
#include <linuxmt/string.h>
#include <linuxmt/debug.h>
#include <linuxmt/netstat.h>
#include <linuxmt/heap.h>
#include <linuxmt/kernel.h>
#include "eth-msgs.h"

// Shared declarations between low and high parts

#include "ne2k.h"
#include "netbuf.h"

/* runtime configuration set in /bootopts or defaults in ports.h */
#define net_irq     (netif_parms[ETH_NE2K].irq)
#define NET_PORT    (netif_parms[ETH_NE2K].port)
#define net_flags   (netif_parms[ETH_NE2K].flags)
int net_port;	    /* required for ne2k-asm.S */


static unsigned char usecount;
static unsigned char found;
static unsigned int verbose;
static struct wait_queue rxwait;
static struct wait_queue txwait;
static struct netif_stat netif_stat;
static byte_t model_name[] = "ne2k";
static byte_t dev_name[] = "ne0";
static size_t ne2k_getpkg(char *, size_t);
struct netbuf *netbuf_init(struct netbuf *, int);

extern int    ne2k_next_pk;
extern word_t ne2k_flags;
extern word_t ne2k_has_data;
extern byte_t ne2k_imask;
extern struct eth eths[];
extern unsigned char macaddr[];

//#define LOCAL_DEBUG
#ifdef LOCAL_DEBUG
void kputchar(int);
#else
#define kputchar(x)
#endif

/*
 * Return a complete packet from buffer
 */

static size_t ne2k_read(struct inode *inode, struct file *filp, char *data, size_t len)
{

	size_t res;  // actual packet size

	kputchar('R');
	while(1) {
		prepare_to_wait_interruptible(&rxwait);
		if (rnext->len) {	/* data in buffer */
			res = rnext->len;
			if (verified_memcpy_tofs(data, rnext->data+4, res)) {
			    printk("ne0: memcpy error in read\n");
			    res = 0; 
			    break;
			}
			kputchar('b');
			//printk("b%04x/%d;", rnext, res);
			rnext->len = 0;
			rnext = rnext->next;
			break;
		}
		if ((filp->f_flags & O_NONBLOCK) && !ne2k_has_data) {
			res = -EAGAIN;
			break;
		}
		do_wait();
		if (current->signal) {
			res = -EINTR;
			break;
		}
	}
	if (ne2k_has_data) mark_bh(NETWORK_BH);
	finish_wait(&rxwait);
	return res;
}

/*
 * Get a packet from the NIC.
 */

static size_t ne2k_getpkg(char *data, size_t len) {

	size_t size;
	word_t *nhdr = (word_t *)data;

	ne2k_pack_get(data, len);
	size = nhdr[1];
	//printk("r%04x|%04x/",nhdr[0], nhdr[1]);	// NIC buffer header
	debug_eth("ne0: read: req %d, got %d\n", len, size);

	//if ((nhdr[1] > size) || (nhdr[0] == 0)) {
	if ((nhdr[0]&~0x7f21) || (nhdr[0] == 0)) {	//EXPERIMENTAL: Upper byte = block #, max 7f

		/* Sanity check, this should not happen.
		 * If this happens, we're reading garbage from the NIC, all pointers
		 * may be invalid: Clear device and buffers.
		 * Probably a 8 bit interface running with 16k buffer enabled.
		 */

		netif_stat.rq_errors++;
		//printk("$%04x.%02x$", ne2k_getpage(), ne2k_next_pk&0xff);
		if (verbose) printk(EMSG_DMGPKT, dev_name, nhdr[0], nhdr[1]);
		ne2k_rx_init();	// Resets the ring buffer pointers to initial values,
				// effectively purging the buffer.
		size = -EIO;
	}
	return(size);
}

/*
 * Pass packet to driver for send
 */

static size_t ne2k_write(struct inode *inode, struct file *file, char *data, size_t len)
{
	size_t res = 0;
	struct netbuf *n, *nxt = tnext;

	kputchar('T');
	if (len > MAX_PACKET_ETH) len = MAX_PACKET_ETH;
	if (len < 64) len = 64;

	while (1) {
		prepare_to_wait_interruptible(&txwait);
#if NET_BUF_STRAT != NO_BUFS
		n = nxt;
		while (nxt->len) {	/* search for available buffer */
		    nxt = nxt->next;
		    if (nxt == n) break;
		}
		if (nxt->len == 0) {
		    kputchar('t');
		    if (verified_memcpy_fromfs(nxt->data, data, len)) {
			printk("ne0: memcpy error in write\n");
			res = -EIO; 
			break;
		    }
		    res = len;
		    nxt->len = len;
		    //printk("%04x/%d;",nxt, nxt->len);
		    //mark_bh(NETWORK_BH);
		    break;
		}
#endif
	/* No buffer available. Returning w/o sending means a lost packet and a retransmit cycle,
	 * which is more than 3 secs, while waiting for a buffer to become available 
	 * will take only a few ms.
	 * FIXME May want to put in a counter, so we don't get stuck. */
#if 0
		if (file->f_flags & O_NONBLOCK) {
		    res = -EAGAIN;
		    break;
		}
#endif
		do_wait();
		if(current->signal) {
		    res = -EINTR;
		    break;
		}
		kputchar('Z');
	}

	finish_wait(&txwait);
	if (res>0) mark_bh(NETWORK_BH);
	return res;
}

/*
 * Test for readiness
 */

int ne2k_select(struct inode *inode, struct file *filp, int sel_type)
{
	int res = 0;

	switch (sel_type) {
		case SEL_OUT:
			if (tnext->len == 0) {
				kputchar('s');
				res = 1;
				break;
			}
			kputchar('S');
			select_wait(&txwait);
			break;

		case SEL_IN:
			if (rnext->len) {
				kputchar('w');
				res = 1;
				break;
			}
			kputchar('W');
			//mark_bh(NETWORK_BH);	// EXPERIMENTAL
			select_wait(&rxwait);
			break;

		default:
			res = -EINVAL;
	}
	return res;
}


/*
 * Interrupt handler
 */

static void ne2k_int(int irq, struct pt_regs *regs)
{
	kputchar('I');
	mark_bh(NETWORK_BH);
}

void ne2k_int_bh(void)
{
	word_t stat, page;

	stat = inb(net_port + EN0_ISR);
	kputchar('i');
	//printk("i%x;",stat);
	do {
		if (!stat && !ne2k_has_data && !tnext->len) break;

#if 0	/* debug */
		page = ne2k_getpage();
		printk("$%02x.%04x.%02x$", stat, page, ne2k_next_pk&0xff);
#endif
        	if (stat & NE2K_ISR_OF) {
			netif_stat.oflow_errors++;
			if (verbose) printk(EMSG_OFLOW, dev_name, stat, netif_stat.oflow_keep);
			page = ne2k_clr_oflow(netif_stat.oflow_keep); 

			debug_eth("/CB%04x/ ", page);
			if (ne2k_has_data)
			    wake_up(&rxwait);
			break; 
		}

		if (stat & NE2K_ISR_RX) {
			kputchar('j');
			ne2k_has_data = 1;
			outb(NE2K_ISR_RX, net_port + EN0_ISR);
		}

		if (ne2k_has_data) {		/* Even if we didn't get an RX int, there may be 
						 * data to pull from the NIC - buffer space 
						 * permitting */
#if NET_BUF_STRAT != NO_BUFS
		    struct netbuf *nxt = rnext;
		    do {
			if (nxt->len == 0) {	/* buffer available */
			    nxt->len = ne2k_getpkg(nxt->data, MAX_PACKET_ETH);
			    if (nxt->len < 0) {	/* we may get a bad packet from ne2k_getpkg() */
				nxt->len = 0;
				continue;	/* Nothing to read, exit loop */
			    }
			    //printk("G%04x/%d/%d;", nxt, nxt->len, ne2k_has_data);
			    break; 		/* Important: one pkt per
						 * loop only - to keep 'ne2k_has_data'
						 * in sync with reality */
			} 
			nxt = nxt->next;
			if (nxt == rnext) break;
		    } while (ne2k_has_data);
		    if (nxt->len) 
#endif
			wake_up(&rxwait);
		}

		if (stat & NE2K_ISR_TX) {
			tnext->len = 0;
			tnext = tnext->next;
			outb(NE2K_ISR_TX, net_port + EN0_ISR);
			inb(net_port + EN0_TSR);	// Keep NIC happy
		}

#if NET_BUF_STRAT != NO_BUFS
		if (tnext->len) {
			ne2k_pack_put(tnext->data, tnext->len);
			kputchar('x');
			wake_up(&txwait);
		}
#endif

		if (stat & NE2K_ISR_RDC) {
			/* RDC interrupts are never used and should always be masked.
		 	 * The dma_read, dma_write routines may fail if the RDC intr
			 * bit is cleared randomly.
		 	 * The RDC bit will occasionally be set for other reasons 
			 * such as an aborted remote DMA transfer. 
		 	 */
			outb(NE2K_ISR_RDC, net_port + EN0_ISR);
		}

		/* Transmit error detected, this should not happen. */
		if (stat & NE2K_ISR_TXE) { 	
			int k;
			netif_stat.tx_errors++;
			k = ne2k_get_tx_stat();
			if (verbose) printk(EMSG_TXERR, dev_name, k);
			outb(NE2K_ISR_TXE, net_port + EN0_ISR);
		}

		/* RXErrors occur almost exclusively when using an 8 bit interface.
		 * A bug in QEMU will cause continuous interrupts if RXE intr is unmasked.
		 * Therefore the asm level code will unmask RXE intr only if the NIC is 
		 * running in 8 bit mode */
		if (stat & NE2K_ISR_RXE) { 	
			/* Don't do anything, just count, report & clr */
			netif_stat.rx_errors++;
			if (verbose) printk(EMSG_RXERR, dev_name, inb(net_port + EN0_RSR));
			outb(NE2K_ISR_RXE, net_port + EN0_ISR); // Clear intr bit
		}
		if (stat & NE2K_ISR_CNT) { 	
			/* The tally counters will overflow on 8 bit interfaces with
		 	 * lots of overruns.  Just clear the condition */
			ne2k_clr_err_cnt();
			outb(NE2K_ISR_CNT, net_port + EN0_ISR);
		}
		stat = inb(net_port + EN0_ISR);		/* Refresh - new interrupts may have arrived */
		//printk("#%02x;", stat);
	} while (stat);
	stat = page;		/* to keep compiler happy */
}

/*
 * I/O control
 */

static int ne2k_ioctl(struct inode *inode, struct file *file, unsigned int cmd, unsigned int arg)
{
	int err = 0;
	byte_t *mac_addr = (byte_t *)&netif_stat.mac_addr;

	switch (cmd) {
		case IOCTL_ETH_ADDR_GET:
			err = verified_memcpy_tofs((byte_t *)arg, mac_addr, 6);
			break;

#if LATER
		case IOCTL_ETH_ADDR_SET:
			if ((err = verified_memcpy_fromfs(mac_addr, (byte_t *) arg, 6)) != 0)
				break;
			ne2k_addr_set(mac_addr);
			printk("ne0: MAC address changed to %02x", mac_addr[0]);
			for (i = 1; i < 6; i++) printk(":%02x", mac_addr[i]);
			printk("\n");
			break;
#endif

		case IOCTL_ETH_GETSTAT:
			/* Return the entire netif_struct */
			err = verified_memcpy_tofs((char *)arg, &netif_stat, sizeof(netif_stat));
			break;

		default:
			err = -EINVAL;

	}
	return err;
}

/*
 * Device open
 */

static int ne2k_open(struct inode *inode, struct file *file)
{
	if (!found)
		return -ENODEV;

	if (usecount++ == 0) {	// Don't initialize if already open
		int err = request_irq(net_irq, ne2k_int, INT_GENERIC);
		if (err) {
			printk(EMSG_IRQERR, dev_name, net_irq, err);
			return err;
		}
		ne2k_reset();
		ne2k_init();
#if NET_BUF_STRAT == HEAP_BUFS
		/* allocate buffer control headers from the heap */
		net_ibuf = (struct netbuf *)heap_alloc(sizeof(struct netbuf) * (netbufs[NET_RXBUFS] + 
				netbufs[NET_TXBUFS]), HEAP_TAG_NETWORK);
		net_obuf = net_ibuf + netbufs[NET_RXBUFS];
		//printk("eth: using %d/%d buffers\n", netbufs[NET_RXBUFS], netbufs[NET_TXBUFS]);
		tnext = netbuf_init(net_obuf, netbufs[NET_TXBUFS]);
		rnext = netbuf_init(net_ibuf, netbufs[NET_RXBUFS]);
#endif

		ne2k_start();
		init_bh(NETWORK_BH, ne2k_int_bh);
	}
	return 0;
}

/*
 * Release (close) device
 */

static void ne2k_release(struct inode *inode, struct file *file)
{
	if (--usecount == 0) {
		ne2k_stop();
#if NET_BUF_STRAT == HEAP_BUFS
		if (rnext) netbuf_release(net_ibuf);
		if (tnext) netbuf_release(net_obuf);
		heap_free(net_ibuf);
#endif
		free_irq(net_irq);
	}
}

/*
 * Ethernet operations
 */

struct file_operations ne2k_fops =
{
    NULL,         /* lseek */
    ne2k_read,
    ne2k_write,
    NULL,         /* readdir */
    ne2k_select,
    ne2k_ioctl,
    ne2k_open,
    ne2k_release
};

#if DEBUG_ETH
void ne2k_display_status(void)
{
	printk("\n---- Ethernet Stats ----\n");
	printk("Skip Count %d\n", netif_stat.oflow_keep);
	printk("Receive overflows %d\n", netif_stat.oflow_errors);
	printk("Receive errors %d\n", netif_stat.rx_errors);
	printk("NIC buffer errors %d\n", netif_stat.rq_errors);
	printk("Transmit errors %d\n", netif_stat.tx_errors);
}
#endif

/*
 * Ethernet main initialization (during boot)
 * FIXME: Needs return value to signal if initalization failed.
 */

void INITPROC ne2k_drv_init(void)
{
	int i;
	word_t prom[16];/* The PROM contains HW MAC address and more 
			 * (aka SAPROM, Station Address PROM).
			 * PROM size is 16 words, the low byte holding the info
			 * (newer cards may have proprietary info in the high byte).
			 *
			 * The MAC address occupies the first 6 words, followed by a 'card signature'
			 * which is usually empty except for bytes 28 and 30 (*words* 14 and 15), 
			 * which contain a 'B' if it's an 8bit card, 'W' if it's a 16bit card.
			 *
			 * The SAPROM is always read as 32 bytes, then compacted to 16 bytes. Card 
			 * is determined by loking at the bytes 14 and/or 15. If 'B', 8-bit mode is
			 * set, otherwise not. IOW, we're using word IO even on 8bit ISA systems, which is
			 * supposedly slightly faster than 2 IOB instructions, and simpler driver code.
			 * 
			 * For interfaces that don't adhere to the B/W encoding, overrides via
			 * bootopts is the solution: Set the MAC address and card type manually
			 * as required.
			 */

	byte_t *cprom, *mac_addr;

	netif_stat.oflow_keep = 0;	/* Default - clear buffer if overflow.
					 * Testing indicates 0 means faster recovery under heavy
					 * load if buffer < 16k */
	mac_addr = (byte_t *)&netif_stat.mac_addr;

	net_port = NET_PORT;    // ne2k-asm.S needs this.
	if (!net_port) {
		printk("eth: %s ignored\n", dev_name);
		return;
	}

	i = ne2k_probe();
	verbose = (net_flags&ETHF_VERBOSE);
	printk("eth: %s at 0x%x, irq %d", dev_name, net_port, net_irq);
	if (i) {
		printk(" not found\n");
		return;
	}
	cprom = (byte_t *)prom;
	ne2k_flags = ETHF_8BIT_BUS;	/* Force byte size PROM read */
	ne2k_reset();
	kputchar('!');
	ne2k_get_hw_addr(prom);
	kputchar('#');
#if 0
	for (i = 0; i < 32; i++) printk(" %02x", cprom[i]);
	printk("\n");
#endif

#if 0	/* MAC address sanity check */
	int j = 0;
	for (i = 0; i < 12; i += 2) {
		j += cprom[i];
	}
	if (j == (cprom[0]*6)) {
		printk("ne0: MAC address error, sum is %x\n", j);
		return;
	}
#endif
	found = 1;
	for (i = 0; i < 16; i++)	/* pack the low bytes into a packed byte array */
		cprom[i] = (byte_t)prom[i]&0xff;


	if (cprom[14] == 'B' && cprom[15] == 'B') {	/* keep 8bit setting from above */
		model_name[2] = '1';
		//netif_stat.if_status |= NETIF_AUTO_8BIT; 
	} else if (arch_cpu > 2) {	/* keep 8bit mode if this is a 8bitISA machine */
		ne2k_flags = 0;		/* otherwise 16 bit NIC */
		netif_stat.oflow_keep = 3;	// Experimental: keep 3 if 16k buffer
	}
#if 0
	for (i = 0; i < 16; i++) printk("%02x", cprom[i]);
	printk("\n");
#endif
	if (macaddr[0]+macaddr[1]+macaddr[2])	/* Bootopts mac address takes presedence */
		cprom = macaddr;

	memcpy(mac_addr, cprom, 6);
	printk(", (%s) MAC %s%02x", model_name, cprom==macaddr? "(from bootopts) ":"", mac_addr[0]);
	ne2k_addr_set(cprom);   /* Set NIC mac addr now so IOCTL works from ktcp */
	i = 1;
	while (i < 6) printk(":%02x", mac_addr[i++]);

	if (net_flags&ETHF_8BIT_BUS) {
		if (cprom[14] == 'W' || cprom[15] == 'W') printk(" (forced 8bit)");
		ne2k_flags = ETHF_8BIT_BUS; 	/* Forced 8bit */
	} else if (net_flags&ETHF_16BIT_BUS) {
		if (cprom[14] == 'B' || cprom[15] == 'B') printk(" (forced 16bit)");
		ne2k_flags = 0;
	}

	/* The _BUF flags indicate forced NIC buffer size, ZERO means use defaults */
	if (net_flags&ETHF_BUF_MASK) {
		ne2k_flags |= net_flags&ETHF_BUF_MASK;		/* asm code uses this */
		printk(" (%dk buffer)", 4<<(net_flags&0x3));
		/* possibly adjust oflow_keep here */
	}
#if (NET_BUF_STRAT == NO_BUFS)
	printk(", flags 0x%02x\n", net_flags);
#else
#if NET_BUF_STRAT == HEAP_BUFS
	/* If no netbufs= in bootopts, initialize from netbuf.h */
	if (netbufs[NET_RXBUFS] == -1) netbufs[NET_RXBUFS] = NET_IBUFCNT;
	if (netbufs[NET_TXBUFS] == -1) netbufs[NET_TXBUFS] = NET_OBUFCNT;
#else	/* Static buffers */
	netbufs[NET_RXBUFS] = NET_IBUFCNT;
	netbufs[NET_TXBUFS] = NET_OBUFCNT;
#endif
	printk(", flags 0x%02x, bufs %dr/%dt\n", net_flags, netbufs[NET_RXBUFS],
							    netbufs[NET_TXBUFS]);
#endif

#if DEBUG_ETH
	debug_setcallback(2, ne2k_display_status);	/* ^P lists status */
#endif
	ne2k_has_data = 0;
	eths[ETH_NE2K].stats = &netif_stat;
}
