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
static int getpkg_busy;
static struct wait_queue rxwait;
static struct wait_queue txwait;
static struct netif_stat netif_stat;
static byte_t model_name[] = "ne2k";
static byte_t dev_name[] = "ne0";
static size_t ne2k_getpkg(char *, size_t, word_t);
struct netbuf *netbuf_init(struct netbuf *, int);

extern int ne2k_next_pk;
extern word_t ne2k_flags;
extern word_t ne2k_has_data;
extern struct eth eths[];

/* Distinguish between buffered and non buffered IO */
/* Don't change these constants, the asm code is using them literally */
#define BUF_IS_LOCAL 0
#define BUF_IS_FAR 1
//static int bufsinuse;

/*
 * Read a complete packet from the NIC buffer
 */

static size_t ne2k_read(struct inode *inode, struct file *filp, char *data, size_t len)
{

	size_t res;  // actual packet size

	//kputchar('R');
	while(1) {
		prepare_to_wait_interruptible(&rxwait);
#if NET_BUF_STRAT != NO_BUFS
		if (rnext && rnext->len) {	/* data in buffer */
			res = rnext->len;
			verified_memcpy_tofs(data, rnext->data, res);
			rnext->len = 0;
			//bufsinuse--;
			rnext = rnext->next;
			//kputchar('b');
			break;
		}
#endif
		/* The local buffers are empty - or we have no buffers.
		 * Pull the data directly from the NIC.
		 * In the buffered case this may happen because the host
		 * rx buffer is smaller than the NIC buffer.
		 *
		 * NOTE: Possible race condition. A RCV INTR may happen
		 * while we're reading a packet - thus the semaphore in getpkg 
		 */
		//kputchar('r');
		if (ne2k_has_data && !getpkg_busy) { 
#if NET_BUF_STRAT != NO_BUFS
		    //kputchar('B');
#endif
		    res = ne2k_getpkg(data, len, BUF_IS_FAR);
		    break;
		}
		if (filp->f_flags & O_NONBLOCK) {
			res = -EAGAIN;
			break;
		}
		do_wait();
		if (current->signal) {
			res = -EINTR;
			break;
		}
	}
	finish_wait(&rxwait);
	return res;
}

/*
 * Get a packet from the NIC.
 * May be called from the INTR routine and from read(),
 * thus the semaphore.
 */

static size_t ne2k_getpkg(char *data, size_t len, word_t type) {

	size_t size;
	word_t nhdr[2];	/* buffer header from the NIC, for debugging */

	clr_irq();
	if (getpkg_busy) { set_irq(); return(0);}	/* busy, just return */
	getpkg_busy++;				/* set the busy flag */
	set_irq();

	size = ne2k_pack_get(data, len, nhdr, type);
	//printk("r%04x|%04x/",nhdr[0], nhdr[1]);	// NIC buffer header
	debug_eth("ne0: read: req %d, got %d real %d\n", len, size, nhdr[1]);

	//if ((nhdr[1] > size) || (nhdr[0] == 0)) {
	if ((nhdr[0]&~0x7f21) || (nhdr[0] == 0)) {	//EXPERIMENTAL: Upper byte = block #, max 7f

		/* Sanity check, this should not happen.
		 * If this happens, we're reading garbage from the NIC, all pointers
		 * may be invalid: Clear device and buffers.
		 *	
		 * Likely reason: We have a 8 bit interface running with 16k buffer enabled.
		 * 
		 * May want to add more tests for nhdr[0]:
		 * 	Low byte should be 1 or 21	(receive status reg)
		 *	High byte is a pointer to the next packet in the NIC ring buffer,
		 *		should be < 0x80 and > 0x45
		 */

		netif_stat.rq_errors++;
		//printk("$%04x.%02x$", ne2k_getpage(), ne2k_next_pk&0xff);
		if (verbose) printk(EMSG_DMGPKT, dev_name, nhdr[0], nhdr[1]);
#if 0
		if (nhdr[0] == 0) { 	// When this happens, the NIC has serious trouble,
					// need to reset as if we had a buffer overflow.
			size = ne2k_clr_oflow(0); 
			//printk("<%04x>", res);
		} else {
#endif
			ne2k_rx_init();	// Resets the ring buffer pointers to initial values,
					// effectively purging the buffer.
			size = -EIO;
#if 0
		}
#endif
	}
	getpkg_busy = 0;	/* reset busy-flag */
	return(size);
}

/*
 * Pass packet to driver for send
 */

static size_t ne2k_write(struct inode *inode, struct file *file, char *data, size_t len)
{
	size_t res;

	//printk("T");
	while (1) {
		prepare_to_wait_interruptible(&txwait);
		// tx_stat() checks the command reg, not the tx_status_reg!
		if (ne2k_tx_stat() != NE2K_STAT_TX) {
#if NET_BUF_STRAT != NO_BUFS
		    if (tnext) {
			/* transmiter is busy, put the data in a buffer if available */
			struct netbuf *nxt = tnext;
			while (nxt->len) {	/* search for available buffer */
				if (nxt == tnext) break;
				nxt = nxt->next;
			}
			if (nxt->len == 0) {
				//kputchar('t');
				nxt->len = len;
				verified_memcpy_fromfs(nxt->data, data, len);
				res = len;
				break;
			}
		    }
#endif
		    if (file->f_flags & O_NONBLOCK) {
			res = -EAGAIN;
			break;
		    }
		    do_wait();
		    if(current->signal) {
			res = -EINTR;
			break;
		    }
		}

		/* NIC is ready, send the data, no buffering */
		if (len > MAX_PACKET_ETH) len = MAX_PACKET_ETH;

		if (len < 64) len = 64;  /* issue #133 */
		ne2k_pack_put(data, len, BUF_IS_FAR);

		res = len;
		break;
	}

	finish_wait(&txwait);
	return res;
}

/*
 * Test for readiness
 */

int ne2k_select(struct inode *inode, struct file *filp, int sel_type)
{
	int res = 0;

	//printk("S");
	switch (sel_type) {
		case SEL_OUT:
			if ((ne2k_tx_stat() == NE2K_STAT_TX) 
#if NET_BUF_STRAT != NO_BUFS
				|| (tnext && !tnext->len)
#endif
			) {
				res = 1;
				break;
			}
			select_wait(&txwait);
			break;

		case SEL_IN:
			if (ne2k_has_data
#if NET_BUF_STRAT != NO_BUFS
			|| (rnext && rnext->len)
#endif
			) {
				res = 1;
				break;
			}
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
	word_t stat, page;

	//kputchar("/");
	while (1) {
		stat = ne2k_int_stat();
		if (!stat) break; 	/* If zero, we're done! */
#if 0	/* debug */
		page = ne2k_getpage();
		printk("$%04x.%02x$", page,ne2k_next_pk&0xff);
#endif
        	if (stat & NE2K_STAT_OF) {
			netif_stat.oflow_errors++;
			if (verbose) printk(EMSG_OFLOW, dev_name, stat, netif_stat.oflow_keep);
			page = ne2k_clr_oflow(netif_stat.oflow_keep); 

			debug_eth("/CB%04x/ ", page);
			//printk("%04x/ ", page);

			if (ne2k_has_data)
				wake_up(&rxwait);
			break; 
		}

		if (stat & NE2K_STAT_RX)
			ne2k_has_data = 1;

		if (ne2k_has_data) {		/* Even if we didn't get an RX int, there may be 
						 * data to pull from the NIC - buffer space 
						 * permitting ... */
#if NET_BUF_STRAT != NO_BUFS
		    struct netbuf *nxt = rnext;
		    if (rnext && !getpkg_busy) {	/* sanity check: 0 buffers is a real possibility */
			while (ne2k_has_data) {
			    if (nxt->len == 0) {	/* buffer available */
				nxt->len = ne2k_getpkg(nxt->data, MAX_PACKET_ETH, BUF_IS_LOCAL);
				//bufsinuse++;	/* DEBUG */
				//kputchar('0'+bufsinuse);
			    } 
			    nxt = nxt->next;
			    if (nxt == rnext) break;
			}
		    }
#endif
		    wake_up(&rxwait);
		    outb(NE2K_STAT_RX, net_port + EN0_ISR);
		}

		if (stat & NE2K_STAT_TX) {
#if NET_BUF_STRAT != NO_BUFS
			if (tnext && tnext->len) {
				ne2k_pack_put(tnext->data, tnext->len, BUF_IS_LOCAL);
				tnext->len = 0;
				tnext = tnext->next;
			}
#endif
			outb(NE2K_STAT_TX, net_port + EN0_ISR); // Clear intr bit
			inb(net_port + EN0_TSR);
			wake_up(&txwait);
		}
		//printk("%02X/%d/", stat, ne2k_has_data);
		/* shortcut for speed */
		if (!(stat&~(NE2K_STAT_TX|NE2K_STAT_RX|NE2K_STAT_OF))) continue;

		/* These don't happen very often */
		if (stat & NE2K_STAT_RDC) {
			//printk("ne0: Warning - RDC intr. (0x%02x)\n", stat);
			/* RDC interrupts should be masked in the low level driver.
		 	 * The dma_read, dma_write routines will fail if the RDC intr
			 * bit is cleared elsewhere.
		 	 * OTOH: The RDC bit will occasionally be set for other reasons 
			 * (such as an aborted remote DMA transfer). In such cases the
			 * RDC bit must be reset here, otherwise this function will loop.
		 	 */
			ne2k_rdc();
		}

		if (stat & NE2K_STAT_TXE) { 	
			int k;
			/* transmit error detected, this should not happen. */
			/* ne2k_get_tx_stat resets this bit in the ISR */
			netif_stat.tx_errors++;
			k = ne2k_get_tx_stat();	// read tx status reg to keep the NIC happy
			if (verbose) printk(EMSG_TXERR, dev_name, k);
		}

		/* RXErrors occur almost exclusively when using an 8 bit interface.
		 * A bug in QEMU will cause continuous interrupts if RXE intr is unmasked.
		 * Therefore the low level driver will unmask RXE intr only if the NIC is 
		 * running in 8 bit mode */
		if (stat & NE2K_STAT_RXE) { 	
			/* Receive error detected, may happen when traffic is heavy */
			/* The 8 bit interface gets lots of these, all CRC, packet dropped */
			/* Don't do anything, just count, report & clr */
			netif_stat.rx_errors++;
			if (verbose) printk(EMSG_RXERR, dev_name, inb(net_port + EN0_RSR));
			outb(NE2K_STAT_RXE, net_port + EN0_ISR); // Clear intr bit
		}
		if (stat & NE2K_STAT_CNT) { 	
			/* The tally counters will overflow on 8 bit interfaces with
		 	 * lots of overruns.  Just clear the condition */
			ne2k_clr_err_cnt();
		}
	}
	//printk(".%02x", ne2k_int_stat());
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
		net_ibuf = (struct netbuf *)heap_alloc(sizeof(struct netbuf) * (netbufs[NET_RXBUFS] + 
				netbufs[NET_TXBUFS]), HEAP_TAG_NETWORK);
		net_obuf = net_ibuf + netbufs[NET_RXBUFS];
#endif
#if NET_BUF_STRAT != NO_BUFS
		//printk("eth: using %d/%d buffers\n", netbufs[NET_RXBUFS], netbufs[NET_TXBUFS]);
		tnext = netbuf_init(net_obuf, netbufs[NET_TXBUFS]);
		rnext = netbuf_init(net_ibuf, netbufs[NET_RXBUFS]);
		//bufsinuse = 0;
#endif

		ne2k_start();
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
	int i, j, k;
	word_t prom[16];/* PROM containing HW MAC address and more 
			 * (aka SAPROM, Station Address PROM).
			 * PROM size is 16 words, the low byte holding the info
			 * (newer cards may have proprietary info in the high byte).
			 *
			 * QEMU duplicates the low byte to the high byte, which we use
			 * to set the QEMU flag (for ftp). This feature is currently
			 * unused as there is no way to pass that info up to ftp.
			 *
			 * The MAC address occupies the first 6 bytes, followed by a 'card signature'
			 * which is usually empty except for bytes 28 and 30, which contain
			 * a 'B' if it's an 8bit card, 'W' if it's a 16bit card.
			 * If a NIC doesn't implement this feature, 16-bit mode
			 * may be forced using the flag field in /bootopts - just like a 16bit
			 * card may be forced to run in 8bit mode.
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

	found = 1;
	cprom = (byte_t *)prom;
	ne2k_get_hw_addr(prom);

	j = k = 0;

#if 0
	for (i = 0; i < 32; i++) printk(" %02x", cprom[i]);
	printk("\n");
#endif

	for (i = 0; i < 12; i += 2) {
		k += cprom[i]-cprom[i+1];	/* QEMU test */
		j += cprom[i];			/* All zero test */
	}

					// FIX THIS, read EPROM consistently
	if (!(net_flags&ETHF_16BIT_BUS) && ((cprom[28] == 'B' && cprom[30] == 'B') ||
					    (cprom[14] == 'B' && cprom[15] == 'B'))) {
		ne2k_flags = ETHF_8BIT_BUS;
		model_name[2] = '1';
		netif_stat.if_status |= NETIF_AUTO_8BIT; 
	} else {
		for (i = 0; i < 16; i++) cprom[i] = (char)prom[i]&0xff;
		ne2k_flags = 0;
	}
	if (!k && j) {
		netif_stat.if_status |= NETIF_IS_QEMU;
		running_qemu = 1;
	}
	//for (i = 0; i < 16; i++) printk("%02x", cprom[i]);
	//printk("\n");

	memcpy(mac_addr, cprom, 6);
	printk(", (%s) MAC %02x", model_name, mac_addr[0]);
	ne2k_addr_set(cprom);   /* Set NIC mac addr now so IOCTL works from ktcp */

	i = 1;
	while (i < 6) printk(":%02x", mac_addr[i++]);
	if (net_flags&ETHF_8BIT_BUS) {
		/* flag that we're forcing 8 bit bus on 16 NIC */
		if (!ne2k_flags) printk(" (8bit)");
		ne2k_flags = ETHF_8BIT_BUS; 	/* Forced 8bit */
	} else if (net_flags&ETHF_16BIT_BUS) {
		ne2k_flags = 0;	/* no other flags set yet */
		printk(" (16bit)");
	}
	if (!(ne2k_flags&ETHF_8BIT_BUS))
		netif_stat.oflow_keep = 3;	// Experimental: use 3 if 16k buffer
	if (netif_stat.if_status & NETIF_IS_QEMU) 
		printk(" (QEMU)");

	/* The _BUF flags indicate forced NIC buffer size, ZERO means use defaults */
	if (net_flags&(ETHF_4K_BUF|ETHF_8K_BUF|ETHF_16K_BUF)) {
		ne2k_flags |= net_flags&0xf;		/* asm code uses this */
		printk(" (%dk buffer)", 4<<(net_flags&0x3));
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
