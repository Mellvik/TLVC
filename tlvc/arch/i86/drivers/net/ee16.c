/*
   Driver for the Intel EtherExpress 16 family of ISA network interfaces.
   For TLVC by Helge Skrivervik (@mellvik) may/june 2024

   Adapted from Linux driver by John Sullivan, Donald Becker et al.
*/
/*
 * TODO:
 * - Many optimizations to take full advantage of the NIC buffers
 * - Honor the verbose bit in the bootopts flag
 * - Fix and test 8bit bus functionality
 *
 */

/*
 * Developer notes:
 */

/* Here's the scoop on memory mapping 	(in verbatim from the Linux driver )
 *
 * There are three ways to access EtherExpress card memory: either using the
 * shared-memory mapping, or using PIO through the dataport, or using PIO
 * through the "shadow memory" ports.
 *
 * The shadow memory system works by having the card map some of its memory
 * as follows:
 *
 * (the low five bits of the SMPTR are ignored)
 *
 *  base+0x4000..400f      memory at SMPTR+0..15
 *  base+0x8000..800f      memory at SMPTR+16..31
 *  base+0xc000..c007      dubious stuff (memory at SMPTR+16..23 apparently)
 *  base+0xc008..c00f      memory at 0x0008..0x000f
 *
 * This last set (the one at c008) is particularly handy because the SCB
 * lives at 0x0008.  So that set of ports gives us easy random access to data
 * in the SCB without having to mess around setting up pointers and the like.
 * We always use this method to access the SCB (via the scb_xx() functions).
 *
 * Dataport access works by aiming the appropriate (read or write) pointer
 * at the first address you're interested in, and then read or writ from/to
 * the dataport.  The pointers auto-increment after each transfer. We use
 * this for data transfer.
 *
 * We don't use the shared-memory system because it allegedly doesn't work on
 * all cards, and because it's a bit more prone to go wrong (it's one more
 * thing to configure...).
 */

#include <arch/io.h>
#include <arch/ports.h>
#include <arch/segment.h>
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
#include <linuxmt/kernel.h>	/* for ARRAY_SIZE(x) */
#include <linuxmt/netstat.h>
#include <netinet/in.h>
#include "eth-msgs.h"
#include "ee16.h"

/* TLVC environment */

#define NET_DEBUG	0
//#define NOP_REGIME	1

extern struct eth eths[];

/* runtime configuration set in /bootopts or defaults in ports.h */
#define net_irq     (netif_parms[ETH_EE16].irq)
#define net_port    (netif_parms[ETH_EE16].port)
#define net_ram     (netif_parms[ETH_EE16].ram)
#define net_flags   (netif_parms[ETH_EE16].flags)

static struct netif_stat netif_stat;
static char model_name[] = "ee16";
static char dev_name[] = "ee0";

/* Device specific constants and state-keepers */
static const char *ee16_ifmap[] = {"AUI", "BNC", "TP"};
enum ee16_iftype {AUI=0, BNC=1, TPE=2};

static unsigned short num_tx_bufs;	/* number of transmit buffers */
static unsigned short num_rx_bufs;	/* number of receive buffers */
static unsigned short rx_buf_end;	/* where the rx buffer chain ends */
static unsigned short rx_buf_start;	/* start of the receive buffer chain */
static unsigned short rx_first;		/* First RX buffer (= rx_buf_start) */
static unsigned short rx_last;		/* Last rx-buffer in chain */
static unsigned short rx_ptr;		/* next packet buffer to read */
static unsigned short tx_head;		/* Next free transmit buffer */
static unsigned short tx_reap;		/* Last tx buffer processed, possibly in-use */
static unsigned short tx_tail;		/* The tx buffer before tx_head in the chain */
static unsigned short tx_link;
static unsigned short tx_buf_start;	/* start of TX buffer chain */
//static unsigned long init_time;		/* counts jiffies since last init, do we need this ?? */
static unsigned char dev_started;
static unsigned char tx_avail;		/* set when NIC transmit buffers are available */
//static unsigned short last_tx_restart;

static unsigned char found;
static unsigned short verbose;
static unsigned char usecount;
static struct wait_queue rxwait;
static struct wait_queue txwait;

int ee16_select(struct inode *, struct file *, int);
static size_t ee16_read(struct inode *, struct file *, char *, size_t);
static size_t ee16_write(struct inode *, struct file *, char *, size_t);
static int ee16_ioctl(struct inode *, struct file *, unsigned int, unsigned int);
static int ee16_open(struct inode *, struct file *);
static void ee16_release(struct inode *, struct file *);
static void udelay(int);
static void ee16_hw_set_interface(void);
static unsigned short INITPROC ee16_hw_readeeprom(unsigned short, unsigned char);
static void ee16_hw_init586(unsigned int);
static void ee16_hw_rxinit(unsigned int);
static unsigned short ee16_hw_lasttxstat(unsigned int);
static void ee16_put_packet(unsigned int, char *, int);
static int ee16_get_packet(char *, int);
void ee16_sendpk(int, char *, int);
void ee16_insw(int, unsigned short *, int);

struct file_operations ee16_fops =
{
    NULL,	 /* lseek */
    ee16_read,
    ee16_write,
    NULL,	 /* readdir */
    ee16_select,
    ee16_ioctl,
    ee16_open,
    ee16_release
};

#define STARTED_RU      2
#define STARTED_CU      1

/* macros from Linux jiffies.h, typecheck() removed */
#define time_after(a,b)	 ((long)((b) - (a)) < 0)
#define time_before(a,b)	time_after(b,a)

/* This is the code and data that is downloaded to the EtherExpress card's
 * memory at boot time.
 */
static unsigned short start_code[] = {
/* 0x0000 */
	0x0001,                 /* ISCP: busy - cleared after reset */
	0x0008,0x0000,0x0000,   /* offset,address (lo,hi) of SCB */
	0x0000,0x0000,          /* SCB: status, commands */
	0x0000,0x0000,          /* links to first command block,
				   first receive descriptor */
	0x0000,0x0000,          /* CRC error, alignment error counts */
	0x0000,0x0000,          /* out of resources, overrun error counts */
	0x0000,0x0000,          /* pad */
	0x0000,0x0000,
/* 0x20 -- start of 82586 CU program */
#define CONF_LINK 0x20
	0x0000,Cmd_Config,
	0x0032,                 /* link to next command */
	0x080c,                 /* 12 bytes follow : fifo threshold=8 */
	0x2e40,                 /* don't rx bad frames
				 * SRDY/ARDY => ext. sync. : preamble len=8
	                         * take addresses from data buffers
				 * 6 bytes/address
				 */
	0x6000,                 /* default backoff method & priority
				 * interframe spacing = 0x60 */
	0xf200,                 /* slot time=0x200
				 * max collision retry = 0xf */
#define CONF_PROMISC  0x2e
	0x0000,                 /* no HDLC : normal CRC : enable broadcast
				 * disable promiscuous/multicast modes */
	0x0040,                 /* minimum frame length = 60 octets) */
	0x0000,Cmd_SetAddr,
	0x003e,                 /* link to next command */
#define CONF_HWADDR  0x38
	0x0000,0x0000,0x0000,   /* hardware address placed here */
	0x0000,Cmd_MCast,
	0x0076,                 /* link to next command */
#define CONF_NR_MULTICAST 0x44
	0x0000,                 /* number of bytes in multicast address(es) */
#define CONF_MULTICAST 0x46
	0x0000, 0x0000, 0x0000, /* some addresses */
	0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000,
#define CONF_DIAG_RESULT  0x76
	0x0000, Cmd_Diag,
	0x007c,                 /* link to next command */
	0x0000,Cmd_TDR|Cmd_INT,
	0x0084,
#define CONF_TDR_RESULT  0x82
	0x0000,
	0x0000,Cmd_END|Cmd_Nop, /* end of configure sequence */
	0x0084                  /* dummy link */
};

/* maps irq number to EtherExpress magic value */
static char irqrmap[] = { 0,0,1,2,3,4,0,0,0,1,5,6,0,0,0,0 };

/*
 * Primitive hardware access functions. Could be 'inline', but we don't do that right now ...
 */
// FIXME: Turn most of these into macros
//
//static unsigned short scb_status(unsigned short ioaddr) {
	//return inw(ioaddr + 0xc008);
//}
#define disable_irq(port)	outb(SIRQ_dis|irqrmap[net_irq], port+SET_IRQ)
#define enable_irq(port)	outb(SIRQ_en|irqrmap[net_irq], port+SET_IRQ)

#define scb_status(x)	(inw(x + 0xc008))
static unsigned short scb_rdcmd(unsigned short ioaddr) {
	return inw(ioaddr + 0xc00a);
}
static void scb_command(unsigned short ioaddr, unsigned short cmd) {
	outw(cmd, ioaddr + 0xc00a);
}
static void scb_wrcbl(unsigned short ioaddr, unsigned short val) {
	outw(val, ioaddr + 0xc00c);
}
static void scb_wrrfa(unsigned short ioaddr, unsigned short val) {
	outw(val, ioaddr + 0xc00e);
}
static void set_loopback(unsigned short ioaddr) {
	outb(inb(ioaddr + Config) | 2, ioaddr + Config);
}
static void clear_loopback(unsigned short ioaddr) {
	outb(inb(ioaddr + Config) & ~2, ioaddr + Config);
}
static unsigned short SHADOW(unsigned short addr) {
	addr &= 0x1f;
	if (addr > 0xf) addr += 0x3ff0;
	return addr + 0x4000;
}

/* Fast way to check for more data pending in NIC buffers */
static unsigned short get_rx_status(unsigned int port) {

	outw(rx_ptr & ~31, port + SM_PTR);
	return(inw(port+SHADOW(rx_ptr)));
}

/*
 * Sanity check the suspected EtherExpress card - if it's there.
 * Read hardware address, reset card, size memory and initialize buffer
 * memory pointers. 
 */
static int INITPROC ee16_hw_probe(void)
{
	unsigned short hw_addr[3], ioaddr = net_port;
	unsigned char conn, irq, buswidth;
	unsigned int memory_size;
	word_t *mac = (word_t *)&netif_stat.mac_addr;
	byte_t *mac_addr = (byte_t *)mac;
	unsigned int i;
	unsigned short xsum = 0;

	printk("eth: %s at 0x%x, irq %d", dev_name, ioaddr, net_irq);
	outb(ASIC_RST, ioaddr+EEPROM_Ctrl);
	outb(0, ioaddr+EEPROM_Ctrl);
	udelay(500);
	outb(i586_RST, ioaddr+EEPROM_Ctrl);
	hw_addr[0] = ee16_hw_readeeprom(ioaddr,2);
	hw_addr[1] = ee16_hw_readeeprom(ioaddr,3);
	hw_addr[2] = ee16_hw_readeeprom(ioaddr,4);

	/* Address validity check - Standard Address or Compaq LTE Address */
	if (!((hw_addr[2]==0x00aa && ((hw_addr[1] & 0xff00)==0x0000)) ||
	      (hw_addr[2]==0x0080 && ((hw_addr[1] & 0xff00)==0x5F00)))) {
		printk(" rejected: invalid address %04x%04x%04x\n",
			hw_addr[2],hw_addr[1],hw_addr[0]);
		return -ENODEV;
	}
	//printk("|%04x%04x%04x|", hw_addr[2], hw_addr[1], hw_addr[0]);
	/*
	 * Calculate the EEPROM checksum.  Carry on anyway if it's bad,
	 * though.
	 */
	for (i = 0; i < 64; i++)
		xsum += ee16_hw_readeeprom(ioaddr, i);
	if (xsum != 0xbaba)
		printk(" (bad EEPROM xsum 0x%02x)", xsum);
	for (i = 0; i < 6; i++)
		mac_addr[i] = ((unsigned char *)hw_addr)[5-i];
	{
		static const char irqmap[] = { 0, 9, 3, 4, 5, 10, 11, 0 };
		unsigned short setupval = ee16_hw_readeeprom(ioaddr,0);

		irq = irqmap[setupval>>13];

		/* Use the IRQ from EEPROM if none was given */
		if (!net_irq)
			net_irq = irq;
		/* FIXME: should we just report the internal conn settings, or update the flags? */
		conn = !(setupval & 0x1000) ? AUI :
				ee16_hw_readeeprom(ioaddr,5) & 0x1 ? TPE : BNC;
		buswidth = !((setupval & 0x400) >> 10);
	}
 	printk(" (HWconf: IRQ %d, %s connector, %d-bit bus", irq,
 	       ee16_ifmap[conn], buswidth?8:16);

	/* Find out how much RAM we have on the card */
	outw(0, ioaddr + WRITE_PTR);
	for (i = 0; i < 32768; i++)
		outw(0, ioaddr + DATAPORT);
        for (memory_size = 0; memory_size < 64; memory_size++) {
		outw(memory_size<<10, ioaddr + READ_PTR);
		if (inw(ioaddr + DATAPORT))
			break;
		outw(memory_size<<10, ioaddr + WRITE_PTR);
		outw(memory_size | 0x5000, ioaddr + DATAPORT);
		outw(memory_size<<10, ioaddr + READ_PTR);
		if (inw(ioaddr + DATAPORT) != (memory_size | 0x5000))
			break;
	}
	/* Sort out the number of buffers.  We may have 16, 32, 48 or 64k
	 * of RAM to play with.
	 */
	num_tx_bufs = 4;
	rx_buf_end = 0x3ff6;
	//netif_stat.if_status |= (memory_size/8)-1;	/* sets ETHF_32K_BUF etc., */
							/* not used by this NIC */
	if (buswidth || (net_flags&ETHF_8BIT_BUS))	/* 8bit bus may be set in the NIC */
		netif_stat.if_status |= ETHF_8BIT_BUS;	/* or forced via (bootopts) flags */
	switch (memory_size)
	{
	case 64:
		rx_buf_end += 0x4000;
	case 48:
		num_tx_bufs += 4;
		rx_buf_end += 0x4000;
	case 32:
		rx_buf_end += 0x4000;
	case 16:
		printk(", %dk RAM)\n", memory_size);
		break;
	default:
		printk(") bad memory size (%dk).\n", memory_size);
		return -ENODEV;
		break;
	}
	printk("eth: %s (%s) on MAC %02x", dev_name, model_name, (mac_addr[0]&0xff));
	i = 1;  
	while (i < 6) printk(":%02x", (mac_addr[i++]&0xff));
	printk(", flags 0x%x\n", net_flags); 

	rx_buf_start = tx_buf_start + (num_tx_bufs*TX_BUF_SIZE);
	return 0;
}

void INITPROC ee16_drv_init(void) {

	if (!net_port) {
		printk("ee16: no port, ignored\n");
		return;
	}
	verbose = (net_flags&ETHF_VERBOSE);
	if (ee16_hw_probe() == 0) {
		found++;
		eths[ETH_EE16].stats = &netif_stat;
	} else
		printk("ee16: not found\n");

}

static size_t ee16_write(struct inode *inode, struct file *file, char *data, size_t len)
{
	int res;

	while (1) {
#if NET_DEBUG > 1
		//kputchar('T');
		printk("\nT %d/%d|", len, tx_avail);
#endif
		prepare_to_wait_interruptible(&txwait);
		if (len > MAX_PACKET_ETH) len = MAX_PACKET_ETH;
		if (len < 64) len = 64;

		if (!tx_avail) {
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
		
		ee16_put_packet(net_port, data, len);
		res = len;
		break;
	}
	finish_wait(&txwait);
	return res;
}

/*
 * When we send a command to the 586, then kicks it with SIGNAL_CA,
 * it will read the command, then zero it, which (apparently) indicate
 * some kind of readyness to continue (or at least to issue the next cmd).
 * NOTE:
 * If the 586 is really busy, the 'original' 10 jiffies are insufficient.
 * Some sources claim we may have to wait up to 0.9 secs, a serious delay
 * by any measure. Thus this loop may not be a good idea in the first 
 * place. It may be smarter to check for zero before issuing the next command
 * rather than wait just after issuing the command - of course unless the
 * code depends on the command having been accepted and acted upon.
 * 
 * The NetBSD driver is smart about this, setting an async flag whenever possible.
 * FIXME!!
 */
#define CMD_CLEAR_TIMEOUT 40

static void ee16_cmd_clear(unsigned int ioaddr)
{
	unsigned long oldtime = jiffies;
	unsigned short s;

	while (scb_rdcmd(ioaddr) && time_before(jiffies, oldtime + CMD_CLEAR_TIMEOUT));
	if ((s = scb_rdcmd(ioaddr))) {
		printk("%s: command (%x) didn't clear\n", dev_name, s);
	}
}


/*
 * Handle an EtherExpress interrupt in the odd(!) case that the CU isn't running. 
 * If we've finished initializing, start the RU and CU up.
 * If we've already started, reap tx buffers, handle any received packets,
 * check to make sure we've not become wedged.
 */
static unsigned short ee16_start_irq(unsigned int ioaddr, unsigned short status)
{
	unsigned short ack_cmd = SCB_ack(status);

	if (!(dev_started & STARTED_CU)) {
		short diag_status, tdr_status;
		while (SCB_CUstat(status) == 2)
			status = scb_status(ioaddr);
#if NET_DEBUG
		printk("%s: CU went non-active (status %04x)\n",
		       dev_name, status);
#endif
		outw(CONF_DIAG_RESULT & ~31, ioaddr + SM_PTR);
		diag_status = inw(ioaddr + SHADOW(CONF_DIAG_RESULT));
		if (diag_status & 1<<11) {
			printk("%s: 82586 failed self-test\n", dev_name);
		} else if (!(diag_status & 1<<13)) 
			printk("%s: 82586 self-test failed to complete\n", dev_name);

		outw(CONF_TDR_RESULT & ~31, ioaddr + SM_PTR);
		tdr_status = inw(ioaddr + SHADOW(CONF_TDR_RESULT));
		if (tdr_status & (TDR_SHORT|TDR_OPEN)) {
			printk("%s: TDR reports cable %s at %d tick%s\n", dev_name,
				(tdr_status & TDR_SHORT)?"short":"broken", tdr_status & TDR_TIME,
				((tdr_status & TDR_TIME) != 1) ? "s" : "");
		} else if (tdr_status & TDR_XCVRPROBLEM) {
			printk("%s: TDR reports transceiver problem\n", dev_name);
		} else if (tdr_status & TDR_LINKOK) {
#if NET_DEBUG 
			printk("%s: TDR reports link OK\n", dev_name);
#endif
		} else {
			printk("%s: TDR is ga-ga (status %04x)\n", dev_name,
			       tdr_status);
		}
		dev_started |= STARTED_CU;
		scb_wrcbl(ioaddr, tx_link);

		/* if the RU isn't running, start it now */
		if (!(dev_started & STARTED_RU)) {
			ack_cmd |= SCB_RUstart;
			scb_wrrfa(ioaddr, rx_buf_start);
			rx_ptr = rx_buf_start;
			dev_started |= STARTED_RU;
		}
		ack_cmd |= SCB_CUstart | 0x2000;
	}
	if (!(dev_started & STARTED_RU) && SCB_RUstat(status)==4)
		dev_started |= STARTED_RU;
	return ack_cmd;
}

/*
 * first level interrupt handler
 */
static void ee16_int(int irq, struct pt_regs *regs)
{
	unsigned short ioaddr, status, ack_cmd;
	unsigned short old_read_ptr, old_write_ptr;

	ioaddr = net_port;
	disable_irq(ioaddr);
	old_read_ptr = inw(ioaddr+READ_PTR);	/* in case we interrupted something, */
	old_write_ptr = inw(ioaddr+WRITE_PTR);  /* save the current pointers */
	status = scb_status(ioaddr);

#if NET_DEBUG
	printk("ee0 int %x;", status);
	//kputchar('I');
#endif
	if (dev_started == (STARTED_CU | STARTED_RU)) {
		do {
#if 0	/* Experimental, moved down. The docs suggest this, but it's unclear why it would matter. */
			ee16_cmd_clear(ioaddr);
			ack_cmd = SCB_ack(status);
			scb_command(ioaddr, ack_cmd);
			outb(0, ioaddr+SIGNAL_CA);
			//printk("nt %x;", status);
			ee16_cmd_clear(ioaddr);
#endif
			if (SCB_complete(status)) {	/* TX interrupt */
				if (!ee16_hw_lasttxstat(ioaddr))
					printk("%s: tx interrupt but no status\n", dev_name);
				else {
					// moved to lasttxstat()
					//tx_avail++;	/* One buffer freed (maybe skip if status 0?)  */
					wake_up(&txwait);
				}
			}
			if (SCB_rxdframe(status)) {	/* RX interrupt */
				wake_up(&rxwait);
				//ee16_hw_rx_pio(ioaddr);
			}
#if 1		/* check comment above */
			ee16_cmd_clear(ioaddr);
			ack_cmd = SCB_ack(status);
			scb_command(ioaddr, ack_cmd);
			outb(0, ioaddr+SIGNAL_CA);
			//printk("nt %x;", status);
			ee16_cmd_clear(ioaddr);
#endif
			status = scb_status(ioaddr);
		} while (status & 0xc000);	/* we may have new (supressed) interrupts while 
						 * processing, complete the loop or we 
						 * risk losing some */

		/* this part needs more testing */
		if (SCB_RUdead(status)) {
			printk("%s: RU stopped: status %04x\n", dev_name, status);
#if 0
			printk("%s: cur_rfd=%04x, cur_rbd=%04x\n", dev->name, lp->cur_rfd, lp->cur_rbd);
			outw(lp->cur_rfd, ioaddr+READ_PTR);
			printk("%s: [%04x]\n", dev->name, inw(ioaddr+DATAPORT));
			outw(lp->cur_rfd+6, ioaddr+READ_PTR);
			printk("%s: rbd is %04x\n", dev->name, rbd= inw(ioaddr+DATAPORT));
			outw(rbd, ioaddr+READ_PTR);
			printk("%s: [%04x %04x] ", dev->name, inw(ioaddr+DATAPORT), inw(ioaddr+DATAPORT));
			outw(rbd+8, ioaddr+READ_PTR);
			printk("[%04x]\n", inw(ioaddr+DATAPORT));
#endif
			netif_stat.rx_errors++;
#if 1
		        ee16_hw_rxinit(ioaddr);
#else
			cur_rfd = first_rfd;
#endif
			scb_wrrfa(ioaddr, rx_buf_start);
			scb_command(ioaddr, SCB_RUstart);
			outb(0, ioaddr+SIGNAL_CA);
		}
	} else {
		if (status & 0x8000)
			ack_cmd = ee16_start_irq(ioaddr, status);
		else
			ack_cmd = SCB_ack(status);
		scb_command(ioaddr, ack_cmd);
		outb(0, ioaddr+SIGNAL_CA);
	}
	ee16_cmd_clear(ioaddr);
	outw(old_read_ptr, ioaddr+READ_PTR);
	outw(old_write_ptr, ioaddr+WRITE_PTR);
	enable_irq(ioaddr);

	return;
}

/*
 * Release (close) device
 */

static void ee16_release(struct inode *inode, struct file *file)
{
	if (--usecount == 0) {
		disable_irq(net_port);
		//outb(SIRQ_dis|irqrmap[net_irq], net_port+SET_IRQ);
		scb_command(net_port, SCB_CUsuspend|SCB_RUsuspend);
		outb(0, net_port+SIGNAL_CA);
		outb(i586_RST, net_port+EEPROM_Ctrl);
		free_irq(net_irq);
	}
#if NET_DEBUG > 3
	printk("ee16: release\n");
#endif
}


static size_t ee16_read(struct inode *inode, struct file *filp, char *data, size_t len)
{
	unsigned short rx_status;
	size_t res;

	while(1) {
		
		rx_status = get_rx_status(net_port);
#if NET_DEBUG > 1
		//printk("R%04x", rx_status);		// DEBUG
		kputchar('R');		// DEBUG
#endif
		prepare_to_wait_interruptible(&rxwait);

		if (!Stat_Done(rx_status)) {	// No data in NIC buffer
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

		if ((res = ee16_get_packet(data, len)) <= 0) {
			printk("%d: Network read error (%d)\n", dev_name, res);
			res = -EIO;
		}
		break;
	}
	
	finish_wait(&rxwait);
	return res;
}

/* Open ee16 device: In order to avoid having an unuse interface lock up resources,
 * most initialization is done here. dev_init does the probe 
 * (verify existence) and pulls out the MAC address and the PnP parameters, that's it.
 */

static int ee16_open(struct inode *inode, struct file *file)
{
	int err;
	char *mac_addr = (char *)&netif_stat.mac_addr;

	if (!found) 
		return -ENODEV;

	if (usecount++ != 0)
		return 0;		// Already open, success

	err = request_irq(net_irq, ee16_int, INT_GENERIC);
	if (err) {
		printk(EMSG_IRQERR, model_name, net_irq, err);
		return err;
	}
	ee16_hw_init586(net_port);	/* may fail, need return value, */
					/* release IRQ if failure FIXME */
 	ee16_hw_set_interface();	/* set conn type from flags	*/
#if NET_DEBUG > 3
	printk("ee16 is now open\n");
#endif
	return 0;
}

/*
 * I/O control
 */

static int ee16_ioctl(struct inode *inode, struct file *file, unsigned int cmd, unsigned int arg)
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

/*
 * Test for readiness
 */

int ee16_select(struct inode *inode, struct file *filp, int sel_type)
{       
	int res = 0;
	
#if NET_DEBUG > 1
	printk("S%d/%d;", sel_type, tx_avail);
#endif
	switch (sel_type) {
		case SEL_OUT:

			if (!tx_avail) {
				select_wait(&txwait);
				break;
			}
			res = 1;
			break;
		
		case SEL_IN:
			if (!FD_Done(get_rx_status(net_port))) {	// data in NIC buffer??
#if NET_DEBUG > 1
				printk("SrxW;");
#endif
				select_wait(&rxwait);
				break;
			}
			res = 1;
			break;
		
		default:
			res = -EINVAL;
	}
#if NET_DEBUG > 1
	printk("s%d;", res);
#endif
	return res;
}
/*
 * Set the cable type to use.
 * Don't INITPROC this since we use it from _open.
 */
static void ee16_hw_set_interface(void)
{
	unsigned char oldval = inb(net_port + 0x300e);

#if NET_DEBUG > 2
	/* FIXME: Is AUI always the default ?? */
	printk("ee16: setif, got %x\n", oldval);
#endif
	oldval &= ~0x82;
	switch (net_flags & (ETHF_USE_AUI|ETHF_USE_BNC)) {
	case 0:		/* TPE */
		oldval |= 0x2;	/* OK to fall thru?? */
	case ETHF_USE_BNC:
		oldval |= 0x80;
		break;
	}
	outb(oldval, net_port+0x300e);
	udelay(20000);
}

/*
 * Read a word from the EtherExpress on-board serial EEPROM.
 * The EEPROM contains 64 words of 16 bits.
 */
static unsigned short INITPROC ee16_hw_readeeprom(unsigned short ioaddr,
						    unsigned char location)
{
	unsigned short cmd = 0x180|(location&0x7f);
	unsigned short rval = 0, wval = EC_CS|i586_RST;
	unsigned int i;

	outb(EC_CS|i586_RST, ioaddr+EEPROM_Ctrl);
	for (i = 0x100; i; i >>= 1 ) {
		if (cmd&i)
			wval |= EC_Wr;
		else
			wval &= ~EC_Wr;
		outb(wval, ioaddr+EEPROM_Ctrl);
		outb(wval|EC_Clk, ioaddr+EEPROM_Ctrl);
		eeprom_delay();
		outb(wval, ioaddr+EEPROM_Ctrl);
		eeprom_delay();
	}
	wval &= ~EC_Wr;
	outb(wval, ioaddr+EEPROM_Ctrl);
	for (i = 0x8000; i; i >>= 1) {
		outb(wval|EC_Clk, ioaddr+EEPROM_Ctrl);
		eeprom_delay();
		if (inb(ioaddr+EEPROM_Ctrl)&EC_Rd)
			rval |= i;
		outb(wval, ioaddr+EEPROM_Ctrl);
		eeprom_delay();
	}
	wval &= ~EC_CS;
	outb(wval|EC_Clk, ioaddr+EEPROM_Ctrl);
	eeprom_delay();
	outb(wval, ioaddr+EEPROM_Ctrl);
	eeprom_delay();

	return rval;
}
static void udelay(int usec)
{
	while (usec--)	outb_p(0, 0x80);
}

/*
 * Writes down the list of transmit buffers into card memory.  Each
 * entry consists of an 82586 transmit command, followed by a jump
 * pointing to itself.  When we want to transmit a packet, we write
 * the data into the appropriate transmit buffer and then modify the
 * preceding jump to point at the new transmit command.  This means that
 * the 586 command unit is continuously active.
 *
 * NEW REGIME (HS): Place one NOP per TX buffer at the beginning of the
 * memory block, each pointing (link) to itself. Then, when a buffer is ready 
 * for transmission, it links to the next NOP and when change the link in
 * the currently executing NOP to point to us. That way we can fill up all
 * TX buffer, each pointing to its own NOP and ready to go. And when there
 * nothing to do, the CU loops in the last used NOP.
 */
static void ee16_hw_txinit(unsigned int ioaddr)
{
	unsigned short tx_block = PROG_AREA_START;
	unsigned short curtbuf;

#ifdef NOP_REGIME
	for (curtbuf = 0; curtbuf < num_tx_bufs; curtbuf++) {
		outw(tx_block, ioaddr + WRITE_PTR);
	        outw(0x0000, ioaddr + DATAPORT);	/* clear status */
		outw(Cmd_Nop, ioaddr + DATAPORT);	/* enter command */
		outw(tx_block, ioaddr + DATAPORT);	/* add link to self */
		tx_block += NOP_CMD_SIZE;
	}
	tx_buf_start = tx_block;
#else
	tx_buf_start = PROG_AREA_START;
#endif

//---------------- this is pretty useless, we're doing the same every time we create a new xmit packet
	for (curtbuf = 0; curtbuf < num_tx_bufs; curtbuf++) {
		outw(tx_block, ioaddr + WRITE_PTR);
	        outw(0x0000, ioaddr + DATAPORT);
		outw(Cmd_INT|Cmd_Xmit, ioaddr + DATAPORT);
#ifdef NOP_REGIME
		outw(PROG_AREA_START + curtbuf*NOP_CMD_SIZE, ioaddr + DATAPORT);
#else
		outw(tx_block+0x08, ioaddr + DATAPORT);
#endif
		outw(tx_block+0x0e, ioaddr + DATAPORT);
		outw(0x0000, ioaddr + DATAPORT);
		outw(0x0000, ioaddr + DATAPORT);
		outw(tx_block+0x08, ioaddr + DATAPORT);
		outw(0x8000, ioaddr + DATAPORT);
		outw(-1, ioaddr + DATAPORT);
		outw(tx_block+0x16, ioaddr + DATAPORT);
		outw(0x0000, ioaddr + DATAPORT);
		tx_block += TX_BUF_SIZE;
	}
//----------------------------------

	tx_head = tx_buf_start;
	tx_reap = tx_buf_start;
	tx_tail = tx_block - TX_BUF_SIZE;
#ifdef NOP_REGIME
	tx_link = tx_buf_start - NOP_CMD_SIZE;	/* last NOP command */
#else
	tx_link = tx_tail + 0x08;
#endif
	rx_buf_start = tx_block;
	tx_avail = num_tx_bufs;
}

/*
 * Write the circular list of receive buffer descriptors to card memory.
 * The end of the list isn't marked, which means that the 82586 receive
 * unit will loop until buffers become available (this avoids it giving us
 * "out of resources" messages).
 * HS:That's why we never get overrun message, must FIX.
 */
static void ee16_hw_rxinit(unsigned int ioaddr)
{
	unsigned short rx_block = rx_buf_start;
	num_rx_bufs = 0;
	rx_first = rx_ptr = rx_block;
	do {
		num_rx_bufs++;
		outw(rx_block, ioaddr + WRITE_PTR);
		outw(0, ioaddr + DATAPORT);  outw(0, ioaddr+DATAPORT);
		outw(rx_block + RX_BUF_SIZE, ioaddr+DATAPORT);
		outw(0xffff, ioaddr+DATAPORT);
		outw(0x0000, ioaddr+DATAPORT);
		outw(0xdead, ioaddr+DATAPORT);
		outw(0xdead, ioaddr+DATAPORT);
		outw(0xdead, ioaddr+DATAPORT);
		outw(0xdead, ioaddr+DATAPORT);
		outw(0xdead, ioaddr+DATAPORT);
		outw(0xdead, ioaddr+DATAPORT);
		outw(0x0000, ioaddr+DATAPORT);
		outw(rx_block + RX_BUF_SIZE + 0x16, ioaddr+DATAPORT);
		outw(rx_block + 0x20, ioaddr+DATAPORT);
		outw(0, ioaddr+DATAPORT);
		outw(RX_BUF_SIZE-0x20, ioaddr+DATAPORT);
		rx_last = rx_block;
		rx_block += RX_BUF_SIZE;
	} while (rx_block <= rx_buf_end-RX_BUF_SIZE);

	/* Make first Rx frame descriptor point to first Rx buffer descriptor */
	outw(rx_first + 6, ioaddr+WRITE_PTR);
	outw(rx_first + 0x16, ioaddr+DATAPORT);

	/* Close Rx frame descriptor ring */
  	outw(rx_last + 4, ioaddr+WRITE_PTR);
  	outw(rx_first, ioaddr+DATAPORT);

	/* Close Rx buffer descriptor ring */
	outw(rx_last + 0x16 + 2, ioaddr+WRITE_PTR);
	outw(rx_first + 0x16, ioaddr+DATAPORT);
#if NET_DEBUG
	printk("INIT: #rxbufs %d, #txbufs %d\n", num_rx_bufs, num_tx_bufs);
#endif
}

/*
 * Un-reset the 586, and start the configuration sequence. We don't wait for
 * this to finish, but allow the interrupt handler to start the CU and RU for
 * us.  We can't start the receive/transmission system up before we know that
 * the hardware is configured correctly.
 * FIXME: need to return a value so the caller can exit if we fail !!
 */
static void ee16_hw_init586(unsigned int ioaddr)
{
	unsigned short *mac = (unsigned short *)&netif_stat.mac_addr;
	unsigned int i;

#if NET_DEBUG
	printk("%s: ee16_hw_init586()\n", dev_name);
#endif
	dev_started = 0;
	set_loopback(ioaddr);
	outb(SIRQ_dis|irqrmap[net_irq], ioaddr+SET_IRQ);

	/* Download the startup code */
	outw(rx_buf_end & ~31, ioaddr + SM_PTR);
	outw((netif_stat.if_status|ETHF_8BIT_BUS)?0x0001:0x0000, ioaddr + 0x8006);
	outw(0x0000, ioaddr + 0x8008);
	outw(0x0000, ioaddr + 0x800a);
	outw(0x0000, ioaddr + 0x800c);
	outw(0x0000, ioaddr + 0x800e);
	for (i = 0; i < ARRAY_SIZE(start_code) * 2; i+=32) {
		unsigned int j;
		outw(i, ioaddr + SM_PTR);
		for (j = 0; j < 16 && (i+j)/2 < ARRAY_SIZE(start_code); j+=2)
			outw(start_code[(i+j)/2],
			     ioaddr+0x4000+j);
		for (j = 0; j < 16 && (i+j+16)/2 < ARRAY_SIZE(start_code); j+=2)
			outw(start_code[(i+j+16)/2],
			     ioaddr+0x8000+j);
	}
	/* Do we want promiscuous mode or multicast? */

#if 0	/* TLVC: We don't do promisc or multicast yet */
	outw(CONF_PROMISC & ~31, ioaddr+SM_PTR);
	i = inw(ioaddr+SHADOW(CONF_PROMISC));
	outw((dev->flags & IFF_PROMISC)?(i|1):(i & ~1),
	     ioaddr+SHADOW(CONF_PROMISC));
	lp->was_promisc = dev->flags & IFF_PROMISC;
	ee16_setup_filter(dev);
#endif
	/* Write our hardware address */
#if NET_DEBUG
	printk("set MAC: %04x%04x%04x;", mac[0], mac[1], mac[2]);
#endif
	outw(CONF_HWADDR & ~31, ioaddr+SM_PTR);
	outw(mac[0], ioaddr+SHADOW(CONF_HWADDR));
	outw(mac[1], ioaddr+SHADOW(CONF_HWADDR+2));
	outw(mac[2], ioaddr+SHADOW(CONF_HWADDR+4));
	ee16_hw_txinit(ioaddr);
	ee16_hw_rxinit(ioaddr);
	outb(0, ioaddr+EEPROM_Ctrl);
	udelay(5000);
	scb_command(ioaddr, 0xf000);
	outb(0, ioaddr+SIGNAL_CA);
	outw(0, ioaddr+SM_PTR);
	{
		unsigned short rboguscount=50, rfailcount=5;
		while (inw(ioaddr+0x4000)) {
			if (!--rboguscount) {
				printk("%s: i82586 reset timed out, kicking...\n",
					dev_name);
				scb_command(ioaddr, 0);
				outb(0, ioaddr+SIGNAL_CA);
				rboguscount = 100;
				if (!--rfailcount) {
					printk("%s: i82586 not responding, giving up.\n",
						dev_name);
					return;
				}
			}
		}
	}
        scb_wrcbl(ioaddr, CONF_LINK);
	scb_command(ioaddr, 0xf000|SCB_CUstart);
	outb(0, ioaddr+SIGNAL_CA);
	{
		unsigned short iboguscount=50,ifailcount=5;
		while (!scb_status(ioaddr)) {
			if (!--iboguscount) {
				if (--ifailcount) {
					printk("%s: i82586 initialization timed out, status %04x, cmd %04x\n",
						dev_name, scb_status(ioaddr), scb_rdcmd(ioaddr));
					scb_wrcbl(ioaddr, CONF_LINK);
				        scb_command(ioaddr, 0xf000|SCB_CUstart);
					outb(0, ioaddr+SIGNAL_CA);
					iboguscount = 100;
				} else {
					printk("%s: Failed to initialize i82586, giving up.\n", dev_name);
					return;
				}
			}
		}
	}
	clear_loopback(ioaddr);
	outb(SIRQ_en|irqrmap[net_irq], ioaddr+SET_IRQ);
	//init_time = jiffies;
#if NET_DEBUG
        printk("%s: leaving ee16_hw_init586()\n", dev_name);
#endif
}

/*
 * Called after each transfer-completed intr.
 * Reap tx buffers and return last transmit status.
 * if ==0 then either:
 *    a) we're not transmitting anything, so why are we here?
 *    b) we've died.
 * otherwise, Stat_Busy(return) means we've still got some packets
 * to transmit, Stat_Done(return) means our buffers should be empty
 * again
 * NOTE: There is no Stat_Busy check, must fix.
 */
static unsigned short ee16_hw_lasttxstat(unsigned int ioaddr)
{
	unsigned short tx_block = tx_reap;
	unsigned short status;

	if (tx_head == tx_reap)		/* Insurance really - return if empty */
		return 0;

	do {
		outw(tx_block & ~31, ioaddr + SM_PTR);
		status = inw(ioaddr + SHADOW(tx_block));
#if NET_DEBUG > 1
		printk("|%x|", status);
#endif
		tx_avail++;		/* experimental, moved from _int proper */
		if (!Stat_Done(status)) {	/* hmmmm, looks like this block hasn't been sent yet */
			kputchar('X');		/* maybe the interrupt was all about the previous block, 
						 * still, it may not make sense to try to restart this,
						 * it should be automatically entered into the CU cmd chain */
//#ifdef NOP_REGIME
			//tx_link = PROG_AREA_START + NOP_CMD_SIZE*((tx_block - tx_buf_start)/TX_BUF_SIZE);
//#else
			tx_link = tx_block;
//#endif
			return status;
		} else {
			//last_tx_restart = 0;
			//netif_stat.tx_errors += Stat_NoColl(status);
			if (!Stat_OK(status)) {
				char *whatsup = NULL;
				netif_stat.tx_errors++;
  				//if (Stat_Abort(status))
					//dev->stats.tx_aborted_errors++;
				if (Stat_TNoCar(status)) {
					whatsup = "aborted, no carrier";
					//dev->stats.tx_carrier_errors++;
				}
				if (Stat_TNoCTS(status)) {
					whatsup = "aborted, lost CTS";
					//dev->stats.tx_carrier_errors++;
				}
				if (Stat_TNoDMA(status)) {
					whatsup = "FIFO underran";
					//dev->stats.tx_fifo_errors++;
				}
				if (Stat_TXColl(status)) {
					whatsup = "aborted, too many collisions";
					//dev->stats.tx_aborted_errors++;
				}
				if (whatsup)
					printk("%s: transmit %s\n", dev_name, whatsup);
			}
			//else
				//dev->stats.tx_packets++;
		}
		if (tx_block == tx_buf_start+((num_tx_bufs-1)*TX_BUF_SIZE))
			tx_reap = tx_block = tx_buf_start;
		else
			tx_reap = tx_block += TX_BUF_SIZE;
		//netif_wake_queue(dev);	/** FIXME ****/

	} while (tx_reap != tx_head);

#ifdef NOP_REGIME
	tx_link = PROG_AREA_START + NOP_CMD_SIZE*((tx_tail - tx_buf_start)/TX_BUF_SIZE);
#else
	tx_link = tx_tail + 0x08;
#endif
	return status;
}

/*
 * Check all the receive buffers, and hand any received packets
 * to the upper levels. Basic sanity check on each frame
 * descriptor, though we don't bother trying to fix broken ones.
 * CORRECTION: Get exactly one receive buffer into the provided
 * address, loop only on bad packets.
 */
//static void ee16_hw_rx_pio(unsigned int ioaddr)
static int ee16_get_packet(char *buffer, int len)
{
	unsigned short rx_block = rx_ptr;
	unsigned short boguscount = num_rx_bufs;
	unsigned short ioaddr = net_port;
	unsigned short status, pkt_len = 0;

#if NET_DEBUG > 1
	printk("get_packet (%x/%d)\n", rx_block, len);
#endif
	disable_irq(ioaddr);
 	do {
 		unsigned short rfd_cmd, rx_next, pbuf;

		outw(rx_block, ioaddr + READ_PTR);
		status = inw(ioaddr + DATAPORT);
		if (FD_Done(status)) {
			rfd_cmd = inw(ioaddr + DATAPORT);
			rx_next = inw(ioaddr + DATAPORT);
			pbuf = inw(ioaddr + DATAPORT);	/* get ptr to data */
			outw(pbuf, ioaddr + READ_PTR);	/* prepare to read it */
			pkt_len = inw(ioaddr + DATAPORT);	/* 1st word is length+status */
#if NET_DEBUG > 1
			printk("rx: len 0x%x(%d);", pkt_len, pkt_len&0x3fff);
#endif
			if (rfd_cmd != 0x0000) {
				printk("%s: rfd_cmd not zero:0x%04x\n", dev_name, rfd_cmd);
				continue;
			}
			else if (pbuf != rx_block+0x16) {	/* sanity check */
				printk("%s: rfd and rbd out of sync 0x%04x 0x%04x\n",
				       dev_name, rx_block+0x16, pbuf);
				continue;
			}
			else if ((pkt_len & 0xc000) != 0xc000) {
				printk("%s: EOF or F not set on received buffer (%04x)\n",
				       dev_name, pkt_len & 0xc000);
  				continue;
  			}
  			else if (!FD_OK(status)) {
				netif_stat.rx_errors++;
#if 0			/* keep for reference, we don't collect detailed error stats */
				if (FD_CRC(status))
					dev->stats.rx_crc_errors++;
				if (FD_Align(status))
					dev->stats.rx_frame_errors++;
				if (FD_Resrc(status))
					dev->stats.rx_fifo_errors++;
				if (FD_Short(status))
					dev->stats.rx_length_errors++;
#endif
				if (FD_DMA(status))
					netif_stat.oflow_errors++;
			} else {
				/* All good, get the data */
				pkt_len &= 0x3fff;

				outw(pbuf+10, ioaddr+READ_PTR);
				//ignoring the len parameter, FIXME
				// insw is words only FIXME
			        ee16_insw(ioaddr+DATAPORT, (word_t *)buffer, (pkt_len+1)>>1);
				boguscount = 0; /* for now, one at a time, break the loop */
			}
			/* FIXME: don't think this is meaningful (HS) */
			outw(rx_block, ioaddr+WRITE_PTR);
			outw(0, ioaddr+DATAPORT);	/* clear status field */
			outw(0, ioaddr+DATAPORT);	/* clear cmd field */
			rx_block = rx_next;
		}
		boguscount = 0;		/* We want only one (TLVC) */
	} while (FD_Done(status) && boguscount--);
	rx_ptr = rx_block;
	enable_irq(ioaddr);
	return pkt_len;
}

#if NET_DEBUG > 1
static void dump_header(unsigned short ptr, int len)
{
	int i = 0;
	unsigned short old_read_ptr = inw(net_port+READ_PTR);

	printk("\n");
	outw(ptr, net_port + READ_PTR);
	while (i < len/2) {
		printk("%04x ", inw(net_port+DATAPORT));
		if (!(++i%16)) printk("\n");
	}
	if (i%16) printk("\n");
	outw(old_read_ptr, net_port+READ_PTR);
}
#endif


/*
 * Hand a packet to the card for transmission
 * NOTE: If we get here, we MUST have already checked
 * to make sure there is room in the transmit
 * buffer region.
 */
static void ee16_put_packet(unsigned int ioaddr, char *buf, int len)
{
	unsigned short tail_stat;

#if NET_DEBUG  > 2
	unsigned short old_read_ptr = inw(ioaddr+READ_PTR);

	outw(tx_tail&~31, ioaddr+SM_PTR);
	tail_stat = inw(ioaddr+SHADOW(tx_tail));
	printk("put: %d/%x/%x/%x;", len, tx_head, tx_tail, tail_stat);
	if (!tail_stat) dump_header(tx_tail, XMIT_CMD_SIZE);
#endif
	tx_avail--;						/* the CMD block */
	tail_stat = 0;	/*DEBUG, toggle the CU stop/start regime */
	//clr_irq();
	disable_irq(ioaddr);
#ifdef NOP_REGIME
	unsigned short this_nop = PROG_AREA_START + NOP_CMD_SIZE*((tx_head - tx_buf_start)/TX_BUF_SIZE);

 	outw(tx_head+XMIT_CMD_SIZE, ioaddr + WRITE_PTR);	/* dump data just after */
	ee16_sendpk(ioaddr + DATAPORT, buf, len);
 	outw(tx_head, ioaddr + WRITE_PTR);
	outw(0x0000, ioaddr + DATAPORT);	/* clear status */
        outw(Cmd_INT|Cmd_Xmit, ioaddr + DATAPORT);	/* add cmd */
	outw(this_nop, ioaddr + DATAPORT);	/* Link to the next cmd (NOP) */
	outw(tx_head+0x0e, ioaddr + DATAPORT);	/* Ptr to BD (next word) */
	outw(0x0000, ioaddr + DATAPORT);	/* 6 byte address field */
	outw(0x0000, ioaddr + DATAPORT);	/* not used */
	outw(0x0000, ioaddr + DATAPORT);	
						/* Buffer descriptor starts here (4w) */
	outw(0x8000|len, ioaddr + DATAPORT);	/* Data length in bytes + END marker */
	outw(-1, ioaddr + DATAPORT);		/* Next BD (none) */
	outw(tx_head+0x16, ioaddr + DATAPORT);	/* Where the data is (2 bytes down) */
	outw(0, ioaddr + DATAPORT);

	/* TX cmd ready, link it into the command chain */
 	outw(this_nop, ioaddr + WRITE_PTR);
	outw(0x0000, ioaddr + DATAPORT);	/* clr status */
	outw(Cmd_Nop, ioaddr + DATAPORT);	/* the nop command, already initialized */
						/* but we need to move the pointer anyway */
	outw(this_nop, ioaddr + DATAPORT);	/* the link, point to itself */

	/* this_nop - the one executed when this tx is done, is ready */
	/* At this point the CU is (probably) looping in the previous NOP cmd. */
	/* Thus - change the prevoius NOP cmd to point to us so we can get going */
	if (this_nop == PROG_AREA_START)
		this_nop = PROG_AREA_START + (NOP_CMD_SIZE * (num_tx_bufs - 1));
	else
		this_nop -= NOP_CMD_SIZE;
	outw(this_nop+4, ioaddr + WRITE_PTR);	/* point to the link field */
	//outw(0x0000, ioaddr + DATAPORT);	/* clr status */
	//outw(Cmd_Nop, ioaddr + DATAPORT);	/* incidentally just 0x0000 */
	outw(tx_head, ioaddr + DATAPORT);	/* update link, here we go ... */
	//trans_start = jiffies;
#else
	// EXPERIMENTAL: Avoid getting the CU wedged...
	// Need to find out why it happens.
	if (!tail_stat || netif_stat.if_status & ETHF_8BIT_BUS) {
		/* Stop the CU so that there is no chance that it
		   jumps off to a bogus address while we are writing the
		   pointer to the next transmit packet in 8-bit mode --
		   this eliminates the "CU wedged" errors in 8-bit mode.
		   (Zoltan Szilagyi 10-12-96) */
		/* FIXME: This needs to be throughly tested on TLVC,
		   probably not required, or there is another way to fix it.
		   HS Apr24 */
		scb_command(ioaddr, SCB_CUsuspend);
		outw(0xFFFF, ioaddr+SIGNAL_CA);
	}
	disable_irq(ioaddr);	/* doesn't seem to matter */
	/* most of this is already in place (txinit()), we just have to add the variables */
 	outw(tx_head, ioaddr + WRITE_PTR);
	outw(0x0000, ioaddr + DATAPORT);
        outw(Cmd_INT|Cmd_Xmit, ioaddr + DATAPORT);
	outw(tx_head+0x08, ioaddr + DATAPORT);
	outw(tx_head+0x0e, ioaddr + DATAPORT);
	outw(0x0000, ioaddr + DATAPORT);
	outw(0x0000, ioaddr + DATAPORT);
	outw(tx_head+0x08, ioaddr + DATAPORT);
	outw(0x8000|len, ioaddr + DATAPORT);
	outw(-1, ioaddr + DATAPORT);
	outw(tx_head+0x16, ioaddr + DATAPORT);
	outw(0, ioaddr + DATAPORT);
	ee16_sendpk(ioaddr + DATAPORT, buf, len);
	outw(tx_tail+0xc, ioaddr + WRITE_PTR);
	outw(tx_head, ioaddr + DATAPORT);
	//trans_start = jiffies;
#endif
	//set_irq();
	tx_tail = tx_head;
	if (tx_head == tx_buf_start+((num_tx_bufs-1)*TX_BUF_SIZE))
		tx_head = tx_buf_start;
	else
		tx_head += TX_BUF_SIZE;
#if 0
	if (tx_head != tx_reap) {
		//netif_wake_queue(dev); /* FIXME!!!!! *********/
		// May use this to avoid the tx_avail regime ??
		wake_up(&txwait);
	}
#endif
	enable_irq(ioaddr);
	if (!tail_stat || netif_stat.if_status & ETHF_8BIT_BUS) {
		/* Restart the CU so that the packet can actually
		   be transmitted. (Zoltan Szilagyi 10-12-96) */
		scb_command(ioaddr, SCB_CUresume);
		outw(0xFFFF, ioaddr+SIGNAL_CA);
	}
	//dev->stats.tx_packets++;
	//last_tx = jiffies;
	//kputchar('P');
}

