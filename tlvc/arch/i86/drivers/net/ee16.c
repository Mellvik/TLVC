/*
   Driver for the Intel EtherExpress 16 family of ISA network interfaces.
   For TLVC by Helge Skrivervik (@mellvik) may/june 2024

   Adapted from Linux driver by John Sullivan, Donald Becker et al.
*/
/*
 * TODO:
 * - Fix and test 8bit bus functionality
 * - More testing with 16k, currently seems to work fine with both PIO and shmem,
 *   shmem is slightly (4-5%) faster.
 * - There are still occasional hangs in probe() when changing from shmem to pio mode
 *   w/o powercycling (ie. via /bootopts and reboot)
 * - Much of the error handling code is untested as it's hard to get errors to happen in 
 *   protected environments. This also applied to the CUwedged recovery code.
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
#include <linuxmt/kernel.h>	/* for ARRAY_SIZE(x) */
#include <linuxmt/string.h>	/* for memset */
#include <linuxmt/netstat.h>
#include <netinet/in.h>
#include "eth-msgs.h"
#include "ee16.h"

/* TLVC environment */

#define NET_DEBUG	0
#if NET_DEBUG == 1
void kputchar(int);
#else
#define kputchar(x)
#endif

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

static unsigned long last_tx;		/* jiffies when last transmit started */
static unsigned short num_tx_bufs;	/* number of transmit buffers */
static unsigned short num_rx_bufs;	/* number of receive buffers */
static unsigned short real_mem_top;	/* real end of (phys) memory */
static unsigned short rx_buf_end;	/* where the rx buffer chain ends */
static unsigned short rx_buf_start;	/* start of the receive buffer chain */
static unsigned short rx_first;		/* First RX buffer (= rx_buf_start) */
static unsigned short rx_last;		/* Last rx-buffer in chain */
static unsigned short rx_ptr;		/* next packet buffer to read */
static unsigned short tx_head;		/* Next free transmit buffer */
static unsigned short tx_reap;		/* Last tx buffer processed, possibly in-use */
static unsigned short tx_tail;		/* The tx buffer before tx_head in the chain */
static unsigned short tx_link;		/* Last known executing TX command */
static unsigned short tx_last;		/* Last TX buffer in chain, for convenience */
static unsigned short tx_buf_start;	/* start of TX buffer chain */
//static unsigned long init_time;		/* counts jiffies since last init, do we need this ?? */
static unsigned char dev_started;
static unsigned char tx_avail;		/* set when NIC transmit buffers are available */
static ramdesc_t shmem_seg;		/* share mem segment */
static unsigned short __far *shmem;	/* Base pointer for shared memory */
static unsigned char pio_mode;		/* set if no shared memory */

static unsigned char found;
static unsigned char usecount;
static unsigned short verbose;
static struct wait_queue rxwait;
static struct wait_queue txwait;

int ee16_select(struct inode *, struct file *, int);
static size_t ee16_read(struct inode *, struct file *, char *, size_t);
static size_t ee16_write(struct inode *, struct file *, char *, size_t);
static int ee16_ioctl(struct inode *, struct file *, unsigned int, unsigned int);
static int ee16_open(struct inode *, struct file *);
static void ee16_release(struct inode *, struct file *);
static void ee16_hw_set_interface(void);
static unsigned short INITPROC ee16_hw_readeeprom(unsigned short, unsigned char);
static int ee16_hw_init586(unsigned int);
static void ee16_hw_rxinit(unsigned int);
static unsigned short ee16_hw_lasttxstat(unsigned int);
static void ee16_put_packet(unsigned int, char *, int);
static int ee16_get_packet(char *, int);
void ee16_outsw(int, unsigned short *, seg_t, int);
void ee16_insw(int, unsigned short *, int);
void ee16_udelay(int);


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

#define eeprom_delay()	ee16_udelay(40)

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
	0x003c,                 /* minimum frame length = 60 octets) */
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
 * Primitive hardware access functions
 */
#define ee16_disable_irq(port)	outb(SIRQ_dis|irqrmap[net_irq], port+SET_IRQ)
#define ee16_enable_irq(port)	outb(SIRQ_en|irqrmap[net_irq], port+SET_IRQ)

static unsigned short scb_status(unsigned short ioaddr) {
	return inw(ioaddr + 0xc008);
}
static unsigned short scb_rdcmd(unsigned short ioaddr) {
	return inw(ioaddr + 0xc00a);
}
static void scb_command(unsigned short ioaddr, unsigned short cmd) {
	outw(cmd, ioaddr + 0xc00a);
 	outb(0xff, ioaddr+SIGNAL_CA);
}
static void scb_wrcbl(unsigned short ioaddr, unsigned short val) {
	outw(val, ioaddr + 0xc00c);
}
static void scb_wrrfa(unsigned short ioaddr, unsigned short val) {
	outw(val, ioaddr + 0xc00e);
}
static void set_loopback(unsigned short ioaddr) {
	unsigned opt = Cfg_Loopback;
	if (!pio_mode) opt |= Cfg_EnaMCS16;
	outb(inb(ioaddr + Config) | opt, ioaddr + Config);
}
static void clear_loopback(unsigned short ioaddr) {
	outb((inb(ioaddr + Config) & ~Cfg_Loopback)|(pio_mode?0:Cfg_EnaMCS16), ioaddr + Config);
}
static unsigned short SHADOW(unsigned short addr) {
	addr &= 0x1f;
	if (addr > 0xf) addr += 0x3ff0;
	return addr + 0x4000;
}

/*
 * When we send a command to the 586, then 'kick' it with SIGNAL_CA,
 * it will read and accept the command, then zero it, which doesn't mean
 * it has been processed, but that the CU is ready to accept the next cmd.
 * NOTE:
 * If the 586 is really busy, the 'original' wait (10 jiffies) is insufficient.
 * Some sources claim we may have to wait up to 0.9 secs, a serious delay
 * by any measure. Thus this loop may not be a good idea in the first 
 * place. It may be smarter to check for zero before issuing the next command
 * rather than wait just after issuing the command - unless the
 * code depends on the command having been accepted and acted upon.
 * 
 * The NetBSD driver is smart about this, setting an async flag whenever possible.
 * FIXME!!
 */
#define CMD_CLEAR_TIMEOUT 40	/* approx .8 seconds */

static void ee16_cmd_clear(unsigned int ioaddr)
{
	unsigned long oldtime = jiffies;

	while (scb_rdcmd(ioaddr) && time_before(jiffies, oldtime + CMD_CLEAR_TIMEOUT));
	if (scb_rdcmd(ioaddr)) {
		printk("%s: command didn't clear\n", dev_name);
	}
}

static unsigned short peek_buffer(unsigned int addr) {
	unsigned short ret, old_ptr;

	if (pio_mode) {		/* This wrapping may be overprotective, but it works */
		clr_irq();
		old_ptr = inw(net_port + SM_PTR);
		outw(addr&~31, net_port + SM_PTR);
		ret = inw(net_port+SHADOW(addr));
		outw(old_ptr, net_port + SM_PTR);
		set_irq();
	} else
		ret = shmem[addr/2];
	return ret;
}

#ifdef UNUSED
static void poke_buffer(unsigned short addr, unsigned short val) {
	unsigned short old_ptr;

	clr_irq();
	old_ptr = inw(net_port + SM_PTR);
	outw(addr&~31, net_port + SM_PTR);
	outw(val, net_port+SHADOW(addr));
	outw(old_ptr, net_port + SM_PTR);
	set_irq();
	return;
}
#endif

static void copyout(unsigned short off, unsigned short *src, unsigned seg, int byte_cnt)
{

	if (pio_mode) {
		outw(off, net_port+WRITE_PTR);
		ee16_outsw(net_port+DATAPORT, src, seg, byte_cnt);	
	} else
		fmemcpyw((void *)off, shmem_seg, src, (ramdesc_t)seg, (byte_cnt+1)>>1);
}

/*
 * Check for presence, then sanity check the suspected EtherExpress card.
 * Read hardware address, reset card, size memory and initialize buffer
 * memory pointers. This is the only point at which the card itself gets
 * reset. Resetting it after boot tgurns out to create havoc - in
 * particular if running shmem. The i586 otoh is being stopped and started
 * quite a bit.
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
#ifdef USE_EEPROM_SHM_CFG  
	unsigned short mval;
#endif

	printk("eth: %s at 0x%x, irq %d", dev_name, ioaddr, net_irq);
	outb(ASIC_RST, ioaddr+EEPROM_Ctrl);
	outb(0, ioaddr+EEPROM_Ctrl);
	ee16_udelay(5000);

	outb(inb(ioaddr + Config), ioaddr + Config); /* If we change from shmem to pio mode w/o
						      * powercycling, the MCS16 bit in the config
						      * register sticks and probe() will hang when
						      * accessing memory via PIO.
						      * This trick clears the MCS16 bit.
						      */
	/* The 586 is stopped in readeeprom() */
	//outb(i586_RST, ioaddr+EEPROM_Ctrl);
	hw_addr[0] = ee16_hw_readeeprom(ioaddr,2);
	hw_addr[1] = ee16_hw_readeeprom(ioaddr,3);
	hw_addr[2] = ee16_hw_readeeprom(ioaddr,4);
#ifdef USE_EEPROM_SHM_CFG
	mval = ee16_hw_readeeprom(ioaddr,6) & 0xff;	/* shared mem config */
#endif

#ifdef NOT_USEFUL
	/* Address validity check - Standard Address or Compaq LTE Address */
	if (!((hw_addr[2]==0x00aa && ((hw_addr[1] & 0xff00)==0x0000)) ||
	      (hw_addr[2]==0x0080 && ((hw_addr[1] & 0xff00)==0x5F00)))) {
		printk(" rejected: invalid address %04x%04x%04x\n",
			hw_addr[2],hw_addr[1],hw_addr[0]);
		return -ENODEV;
	}
	printk("|%04x%04x%04x|", hw_addr[2], hw_addr[1], hw_addr[0]);
#endif
	if (!(hw_addr[0]+hw_addr[1]+hw_addr[2]))
		return -ENODEV;
	/*
	 * Calculate the EEPROM checksum.  Carry on anyway if it's bad,
	 * though.
	 */
#ifdef DISPLAY_EEPROM
	unsigned short x;
	for (i = 0; i < 64; i++) {
		x = ee16_hw_readeeprom(ioaddr, i);
		xsum += x;
		printk("%04x ", x);
		if ((i+1)%16 == 0) printk("\n");
	}
#else
	for (i = 0; i < 64; i++)
		xsum += ee16_hw_readeeprom(ioaddr, i);
#endif
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
		conn = !(setupval & 0x1000) ? AUI :
				ee16_hw_readeeprom(ioaddr,5) & 0x1 ? TPE : BNC;
		buswidth = !(setupval & 0x400);			/* 8bit -> true */
		if (buswidth || (net_flags&ETHF_8BIT_BUS))	/* 8bit bus may be set in the eeprom */
			netif_stat.if_status |= ETHF_8BIT_BUS;	/* or forced via (bootopts) flags */
								/* At this time there is no way to
								 * force 16bit bus in case the eeprom
								 * setting is wrong.
								 */
	}
#ifdef USE_EEPROM_SHM_CFG
 	if (verbose) printk(" (HWconf: IRQ %d, %s, %d-bit bus, shmem 0x%x)", irq,
 		ee16_ifmap[conn], (netif_stat.if_status&ETHF_8BIT_BUS)?8:16, mval);
#else
 	if (verbose) printk(" (HWconf: IRQ %d, %s, %d-bit bus)", irq,
 		ee16_ifmap[conn], (netif_stat.if_status&ETHF_8BIT_BUS)?8:16);
#endif

	/* Find out how much RAM we have on the card 
	 * (don't trust the eeprom shared mem size for this).
	 */
	outw(0, ioaddr + WRITE_PTR);
	for (i = 0; i < 0x8000; i++)	/* 32k */
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
	real_mem_top = (memory_size << 10) - 0xa;
	if (net_flags&ETHF_16K_BUF) {
		memory_size = 16;
		netif_stat.if_status |= ETHF_16K_BUF;
	}

	num_tx_bufs = memory_size >> 3;	/* must be >=3 for the restartCU regime to work */

	if (memory_size < 16 || memory_size > 64) {
		printk(", bad memory size (%dk).\n", memory_size);
		return -ENODEV;
	} else
		printk(", %dk RAM\n", memory_size);
	rx_buf_end = (memory_size<<10) - 0xa;

	pio_mode = 1;

	/* 
	 * Shared memory configuration is black art, copied from driver to driver thru 
	 * generations, with no documentation and a lot of assumptions.
	 * 
	 * For now, skip the EEprom shmem config and use the shared mem address from /bootopts.
	 * If no shared memory setting in bootopts (or just 0), turn off shared memory.
	 * Having only 16k buffer space should do the same, for now just emit a warning
	 * (valuable for testing/debugging).
	 */

	if (!net_ram) {	/* forced pio mode */
		if (verbose) printk("eth: no shared mem, using PIO mode\n");
	} else {
		unsigned short pg = 2;	/* 2 for c800, 4 for d000 */
		unsigned short adjust, decode, edecode;

		shmem_seg = net_ram;
		if (shmem_seg != 0xc800) {
			shmem_seg = 0xd000; /* insurance */
			pg = 4;
		}
		shmem = _MK_FP(shmem_seg, 0);
		if (verbose) printk("eth: using shared mem at 0x%x (%lx)\n", shmem_seg, (long) shmem);
		pio_mode = 0;
		if (memory_size < 32)
			printk("eth: Warning - shmem mode may be unstable with memory < 32k!\n");
		adjust = MEM_CTRL_FMCS16 | (pg &0x03) << 2;
		decode = ((1 << (memory_size/16)) - 1) << pg;
		edecode = ((~decode >> 4) & 0xF0) | (decode >> 8);

		outb(decode & 0xFF, ioaddr + MEM_Dec);
		outb(adjust, ioaddr + MEM_Ctrl);
		outb((~decode & 0xFF), ioaddr + MEM_Page_Ctrl);
		outb(edecode, ioaddr + MEM_ECtrl);	/* Clear 0xe0000 */

#ifdef TEST_SHMEM
		*shmem=0xa5a5;
		shmem[1] = 0x5a5a;
		shmem[0x80] = 0x5a5a;
		unsigned short *k = 0x100;
		fmemsetw(k, shmem_seg, 0xa000, (TX_BUF_SIZE*num_tx_bufs)/2);
		printk("test shared memory: %x %x\n", *shmem, *(shmem+TX_BUF_SIZE-1));
#endif
	}

	printk("eth: %s (%s) on MAC %02x", dev_name, model_name, (mac_addr[0]&0xff));
	i = 1;  
	while (i < 6) printk(":%02x", (mac_addr[i++]&0xff));
	printk(", flags 0x%x\n", net_flags); 

	return 0;
}

void INITPROC ee16_drv_init(void) {

	if (!net_port) {
		printk("ee16: no port, ignored\n");
		return;
	}
	verbose = !!(net_flags&ETHF_VERBOSE);
	if (ee16_hw_probe() == 0) {
		found++;
		eths[ETH_EE16].stats = &netif_stat;
	} else
		printk(" not found\n");

}

/* 
 * Restart the (wedged) CU at the given block.
 * When we detect that the CU stopped sending frames,
 * this will restart it, starting at 'addr'.
 */
static void ee16_restartCU(unsigned short addr)
{
#if NET_DEBUG
	printk("@%x|%x:", addr, peek_buffer(addr+0x0c));
#endif
	scb_wrcbl(net_port, addr);
	ee16_cmd_clear(net_port);		/* change when we add an async flag */
	scb_command(net_port, SCB_CUstart);
}

#ifdef SERIOUS_DEBUG
static void dump_header(unsigned short ptr, int len)
{
	int i;

	printk("\n%x: ", ptr);
	for (i = 0; i < len; i += 2) {
		printk("%04x ", peek_buffer(ptr+i));
		if (!((i+2)%16)) printk("\n");
	}
	if (i%16) printk("\n");
}

static void dump_shmem(unsigned short off, int len)
{
	int i = 0;
	printk("0x%lx(%04x)\n%04x: ", (long)shmem, shmem[0], off);
	
	while (i < len/2) {
		printk("%04x ", *(shmem+(off/2)+i));
		if (!(++i%16)) printk("\n%04x: ", off+(i<<1));
	}
	if (i%16) printk("\n");
}
#endif



static size_t ee16_write(struct inode *inode, struct file *file, char *data, size_t len)
{
	int res;

	while (1) {
#if NET_DEBUG > 2
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
 * Handle an EtherExpress interrupt if the CU isn't running. 
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
		diag_status = peek_buffer(CONF_DIAG_RESULT);
		if (diag_status & 1<<11) {
			printk("%s: 82586 failed self-test\n", dev_name);
		} else if (!(diag_status & 1<<13)) 
			printk("%s: 82586 self-test failed to complete\n", dev_name);

		tdr_status = peek_buffer(CONF_TDR_RESULT);
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
		ack_cmd |= SCB_CUstart | ACK_CUstopped;
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

	ioaddr = net_port;
	ee16_disable_irq(ioaddr);
	ee16_cmd_clear(ioaddr);		/* Never read status until cmd has cleared */
	status = scb_status(ioaddr);

#if NET_DEBUG == 2
	printk("ee0 int %x;", status);
#elif NET_DEBUG
	kputchar('I');
#endif
	if (dev_started == (STARTED_CU | STARTED_RU)) {
		kputchar('>');
		do {
#if NET_DEBUG > 1
			printk("i %x;", status);
#endif

			ack_cmd = 0;
			if (!SCB_ack(status)) printk("*%x*", status);

			if (SCB_rxdframe(status)) {	/* RX interrupt */
				ack_cmd |= ACK_rxdframe;
				kputchar('r');
				wake_up(&rxwait);
			}
			if (SCB_complete(status)) {	/* TX interrupt */
							/* may want to skip if CU stopped */
				ack_cmd |= ACK_complete;
				kputchar('t');
				if (tx_head != tx_reap)	{
				   if (!ee16_hw_lasttxstat(ioaddr))  {
					/* if we get here, something might be seriously wrong */
					printk("%s: tx-int, no status (%x)\n", dev_name, scb_status(ioaddr));
				   } else {
				   	wake_up(&txwait);
				   }
				}
#if NET_DEBUG
				else	/* If we process several tx packets in lasttxstat(), there
					 * may be unacknowledged interrupts. They end up here
					 * and eventually get acknowleged */
					kputchar('+');
#endif
			}
			/* Experimental */
			if (SCB_RUstat(status) == 2) {
				printk("%s: RX overrun, status 0x%x\n", dev_name, status);
				/**** should not happen - receive buffers are 'looped'  */
			}

			/* FIXME: this part needs more testing */
			if (SCB_RUdead(status)) {
				ack_cmd |= ACK_RUstopped|SCB_RUstart;
				printk("%s: RU stopped: status %04x\n", dev_name, status);
				netif_stat.rx_errors++;
		        	ee16_hw_rxinit(ioaddr);
				scb_wrrfa(ioaddr, rx_buf_start);
			}
			if (SCB_CUdead(status)) {	/* UNTESTED */
				/* happens all the time when using start/stop tx mode */
				ack_cmd |= ACK_CUstopped|SCB_CUstart;
				printk("%s: CU stopped: status %04x\n", dev_name, status);
				netif_stat.tx_errors++;
				scb_wrcbl(ioaddr, tx_reap);	/* the last tx probably failed, retry */
			}
			scb_command(ioaddr, ack_cmd);
			ee16_cmd_clear(ioaddr);

			status = scb_status(ioaddr);	/* check for new events */
			//if (SCB_actioncmd(status)) kputchar('>');
		} while (SCB_actioncmd(status));

	} else {	/* NIC initialization only */
		if (status & 0x8000)
			ack_cmd = ee16_start_irq(ioaddr, status);
		else
			ack_cmd = SCB_ack(status);
		scb_command(ioaddr, ack_cmd);
	}
	ee16_cmd_clear(ioaddr);
	ee16_enable_irq(ioaddr);
	kputchar('<');

	return;
}

/*
 * Release (close) device
 */

static void ee16_release(struct inode *inode, struct file *file)
{
	if (--usecount == 0) {
		ee16_disable_irq(net_port);
		scb_command(net_port, SCB_CUsuspend|SCB_RUsuspend);
		outb(i586_RST, net_port+EEPROM_Ctrl);	/* stop the NIC */
		free_irq(net_irq);
	}
#if NET_DEBUG > 2
	printk("ee16: release\n");
#endif
}


static size_t ee16_read(struct inode *inode, struct file *filp, char *data, size_t len)
{
	unsigned short rx_status;
	size_t res;

	while(1) {
		
		rx_status = peek_buffer(rx_ptr);
#if NET_DEBUG > 1
		printk("R%04x", rx_status);
#elif NET_DEBUG
		kputchar('R');
#endif
		prepare_to_wait_interruptible(&rxwait);

		if (!Stat_Done(rx_status)) {	// No data in NIC buffer
			if (filp->f_flags & O_NONBLOCK) {
				res = -EAGAIN;
				break;
			}
			kputchar('!');
			do_wait();
			if (current->signal) {
				res = -EINTR;
				break;
			}
		}

		kputchar('^');
		if ((res = ee16_get_packet(data, len)) <= 0) {
			printk("%s: Network read error (%d)\n", dev_name, res);
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

	if (!found) 
		return -ENODEV;

	if (usecount != 0)
		return 0;		// Already open, success

	err = request_irq(net_irq, ee16_int, INT_GENERIC);
	if (err) {
		printk(EMSG_IRQERR, model_name, net_irq, err);
		return err;
	}
	if ((err = ee16_hw_init586(net_port))) {
		free_irq(net_irq);
		printk("%s: initialization failed (%d)\n", dev_name, err);
		return -ENODEV;
	}
 	ee16_hw_set_interface();	/* set conn type from flags	*/
	usecount++;
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
	unsigned rx_status;
	
#if NET_DEBUG > 1
	printk("S%d/%d/%x;", sel_type, tx_avail, tx_head-tx_reap);
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

			rx_status = peek_buffer(rx_ptr);	/* KLUDGE to speed up file transfers */
			if (!FD_Done(rx_status)) ee16_udelay(700);

			if (!FD_Done(peek_buffer(rx_ptr))) {	// data in NIC buffer??
#if NET_DEBUG > 1
				printk("SrxW;");
#endif
				kputchar('W');
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
		oldval |= 0x2;	/* fall thru */
	case ETHF_USE_BNC:
		oldval |= 0x80;	/* TPE = 0xa0 */
		break;
	case ETHF_USE_AUI:
 		oldval &= ~0xa0;
	}
	outb(oldval, net_port+0x300e);
	ee16_udelay(20000);
}

/*
 * Read a word from the EtherExpress on-board serial EEPROM.
 * The EEPROM contains 64 words of 16 bits. Keep the i586 reset
 * throughout.
 */
static unsigned short INITPROC ee16_hw_readeeprom(unsigned short ioaddr,
						    unsigned char location)
{
	unsigned short cmd = 0x180|(location&0x7f);
	unsigned short rval = 0, wval = EC_CS|i586_RST;
	unsigned int i;

	outb(wval, ioaddr+EEPROM_Ctrl);
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

/*
 * Writes down the list of transmit buffers into card memory.  Each
 * entry consists of an 82586 transmit command, followed by a jump
 * pointing to itself.  When we want to transmit a packet, we write
 * the data into the appropriate transmit buffer and then modify the
 * preceding jump to point at the new transmit command.  This means that
 * the 586 command unit is continuously active.
 */
static void ee16_hw_txinit(unsigned int ioaddr)
{
	unsigned short tx_block;
	unsigned short curtbuf;
	unsigned short txcmd[XMIT_CMD_SIZE/2];

	tx_block = tx_buf_start;
	memset((char *)txcmd, 0, XMIT_CMD_SIZE);
	txcmd[0] = 0xa000;	/* fake initial status to keep the CU wedged mechanism happy */
	txcmd[7] = 0x8000;
	txcmd[8] = 0xffff;

	for (curtbuf = 0; curtbuf < num_tx_bufs; curtbuf++) {
		txcmd[1] = Cmd_INT|Cmd_Xmit;
		txcmd[2] = tx_block+0x08;
		txcmd[3] = tx_block+0x0e;
		txcmd[6] = tx_block+0x08;
		txcmd[9] = tx_block+0x16;
		copyout(tx_block, txcmd, kernel_ds, XMIT_CMD_SIZE);
		tx_block += TX_BUF_SIZE;
	}

	tx_head = tx_buf_start;
	tx_reap = tx_buf_start;
	tx_tail = tx_last = tx_block - TX_BUF_SIZE;	/* last block */
	tx_link = tx_tail + 0x08;
	rx_buf_start = tx_block;
	tx_avail = num_tx_bufs;
#ifdef SERIOUS_DEBUG
	if (!pio_mode) dump_shmem(tx_head, XMIT_CMD_SIZE*2);
	dump_header(tx_head, XMIT_CMD_SIZE);
	dump_header(tx_tail, XMIT_CMD_SIZE);
#endif
}

/*
 * Write the circular list of receive buffer descriptors to card memory.
 * The end of the list isn't marked, which means that the 82586 receive
 * unit will loop until buffers become available (this avoids it giving us
 * "out of resources" messages).
 * HS-06/24: Smart - except it prevents us from knowing about overruns ...
 */
static void ee16_hw_rxinit(unsigned int ioaddr)
{
	unsigned short rx_block = rx_buf_start;
	unsigned short rxcmd[RX_CMD_SIZE/2];
	int i;

	num_rx_bufs = 0;
	rx_first = rx_ptr = rx_block;
	for (i = 4; i < 10; i++)
		rxcmd[i] = 0xdead;
	rxcmd[0] = rxcmd[1] = 0;
	rxcmd[3] = 0xffff;
	rxcmd[4] = 0;
	rxcmd[11] = 0;
	rxcmd[14] = 0;

	do {
		num_rx_bufs++;
		rxcmd[2] = rx_block + RX_BUF_SIZE;
		rxcmd[12] = rx_block + RX_BUF_SIZE + 0x16;
		rxcmd[13] = rx_block + 0x20;
		rxcmd[15] = RX_BUF_SIZE-0x20;
		copyout(rx_block, rxcmd, kernel_ds, RX_CMD_SIZE);

		rx_last = rx_block;
		rx_block += RX_BUF_SIZE;
	} while (rx_block <= rx_buf_end-RX_BUF_SIZE);

	if (pio_mode) {
	/* Make first Rx frame descriptor point to first Rx buffer descriptor */
		outw(rx_first + 6, ioaddr+WRITE_PTR);
		outw(rx_first + 0x16, ioaddr+DATAPORT);

	/* Close Rx frame descriptor ring */
  		outw(rx_last + 4, ioaddr+WRITE_PTR);
  		outw(rx_first, ioaddr+DATAPORT);

	/* Close Rx buffer descriptor ring */
		outw(rx_last + 0x16 + 2, ioaddr+WRITE_PTR);
		outw(rx_first + 0x16, ioaddr+DATAPORT);
	} else {
		shmem[(rx_first + 6)/2] = rx_first + 0x16;
		shmem[(rx_last + 4)/2] = rx_first;
		shmem[(rx_last + 0x16 + 2)/2] = rx_first + 0x16;
	}

#if NET_DEBUG
	printk("%s: %d rxBuffers, %d txBuffers\n", dev_name, num_rx_bufs, num_tx_bufs);
	printk("%s: rx start: %d (0x%x), rx end: %d (0x%x)\n", dev_name, rx_first, rx_first,
					rx_last+RX_BUF_SIZE, rx_last+RX_BUF_SIZE);
#endif
}

/*
 * Un-reset the 586, and start the configuration sequence. We don't wait for
 * this to finish, but allow the interrupt handler to start the CU and RU for
 * us.  We can't start the receive/transmission system up before we know that
 * the hardware is configured correctly.
 * Return non-zero on error.
 *
 * The 82586 coldboots from the upper 10 bytes of buffer ram (rx_buf_end), which
 * among a few other things, points it where to continue. Conveniently, if everything
 * is ZERO, it continues at address 0, which is where we put the
 * setup/initialization code.
 * Note in particular that a non-zero value at location top-10 means that the
 * 586 will run in 8bit mode. This mode is not yet supported. 
 * Also note that limiting available buffer space via configuration (like forced use
 * of 16k even though physical is 32k) doesn't change the NIC perception of where RAM
 * ends. It will still boot from location top-10.
 * Note 2: The main IO-port is somehow not available unless the i82586 is running, thus the
 * extended use of the shadow mechanism below (avoiding copyout() for pio_mode).
 */
static int ee16_hw_init586(unsigned int ioaddr)
{
	unsigned short *mac = (unsigned short *)&netif_stat.mac_addr;
	unsigned int i;

#if NET_DEBUG
	printk("%s: ee16_hw_init586()\n", dev_name);
#endif
#ifdef NOT_GOOD
	outb(ASIC_RST, ioaddr+EEPROM_Ctrl);	/* reset card to known state */
	outb(0, ioaddr+EEPROM_Ctrl);
	ee16_udelay(5000);
	outb(i586_RST, ioaddr+EEPROM_Ctrl);
#endif
	dev_started = 0;
	tx_buf_start = PROG_AREA_START;
	rx_buf_start = tx_buf_start + (num_tx_bufs*TX_BUF_SIZE);

	ee16_disable_irq(ioaddr);
	set_loopback(ioaddr);

	/* Download the startup code at the top of phys memory */	
	if (pio_mode) {
		outw(real_mem_top & ~31, ioaddr + SM_PTR);
		outw((netif_stat.if_status&ETHF_8BIT_BUS)?0x0001:0x0000, ioaddr + 0x8006);
		outw(0x0000, ioaddr + 0x8006);
		outw(0x0000, ioaddr + 0x8008);
		outw(0x0000, ioaddr + 0x800a);
		outw(0x0000, ioaddr + 0x800c);
		outw(0x0000, ioaddr + 0x800e);

		for (i = 0; i < ARRAY_SIZE(start_code) * 2; i+=32) {
			unsigned int j;
			outw(i, ioaddr + SM_PTR);
			for (j = 0; j < 16 && (i+j)/2 < ARRAY_SIZE(start_code); j+=2)
				outw(start_code[(i+j)/2], ioaddr+0x4000+j);
			for (j = 0; j < 16 && (i+j+16)/2 < ARRAY_SIZE(start_code); j+=2)
				outw(start_code[(i+j+16)/2], ioaddr+0x8000+j);
		}
	} else {
		fmemsetw((void *)real_mem_top, shmem_seg, 0, 5);
		if (netif_stat.if_status&ETHF_8BIT_BUS)
			shmem[real_mem_top/2] = 1;
		copyout(0, start_code, kernel_ds, ARRAY_SIZE(start_code)*2);
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
	/* Write our hardware address into the config program */
	if (pio_mode) {
		outw(CONF_HWADDR & ~31, ioaddr+SM_PTR);
		outw(mac[0], ioaddr+SHADOW(CONF_HWADDR));
		outw(mac[1], ioaddr+SHADOW(CONF_HWADDR+2));
		outw(mac[2], ioaddr+SHADOW(CONF_HWADDR+4));
	} else 
		copyout(CONF_HWADDR, mac, kernel_ds, 6);
	ee16_hw_txinit(ioaddr);
	ee16_hw_rxinit(ioaddr);
	outb(0, ioaddr+EEPROM_Ctrl);	/* start i586 (stopped in hw_probe or _release)  */
	ee16_udelay(5000);		/* ~5ms */
	scb_command(ioaddr, 0xf000);	/* clear pending interrupts */
	{
		unsigned short rboguscount=50, rfailcount=5;
		while (peek_buffer(0)) {
			if (!--rboguscount) {
				printk("%s: i82586 reset timed out, kicking...\n",
					dev_name);
				scb_command(ioaddr, 0);
				rboguscount = 100;
				if (!--rfailcount) {
					printk("%s: i82586 not responding, giving up.\n",
						dev_name);
					return -1;
				}
			}
		}
	}
        scb_wrcbl(ioaddr, CONF_LINK);
	scb_command(ioaddr, 0xf000|SCB_CUstart);
	{
		unsigned short iboguscount=50,ifailcount=5;
		while (!scb_status(ioaddr)) {
			if (!--iboguscount) {
				if (--ifailcount) {
					printk("%s: i82586 initialization timed out, status %04x, cmd %04x\n",
						dev_name, scb_status(ioaddr), scb_rdcmd(ioaddr));
					scb_wrcbl(ioaddr, CONF_LINK);
				        scb_command(ioaddr, 0xf000|SCB_CUstart);
					iboguscount = 100;
				} else {
					printk("%s: Failed to initialize i82586, giving up.\n", dev_name);
					return -1;
				}
			}
		}
	}
	clear_loopback(ioaddr);
	ee16_enable_irq(ioaddr);
#if NET_DEBUG
        printk("%s: leaving ee16_hw_init586()\n", dev_name);
#endif
	return 0;
}

/*
 * Called after each transfer-completed intr.
 * Reap tx-buffers and return last transmit status.
 * if ==0 then either:
 *    a) we're not transmitting anything, so why are we here?
 *    b) we've died.
 * otherwise, Stat_Busy(return) means we've still got some packets
 * to transmit, Stat_Done(return) means our buffers should be empty
 * again
 */
static unsigned short ee16_hw_lasttxstat(unsigned int ioaddr)
{
	unsigned short tx_block = tx_reap;
	unsigned short status;

	do {
		//status = peek_buffer(tx_block);
		/* fix this - peek_b is much better for shmem */
		outw(tx_block & ~31, ioaddr + SM_PTR);
		status = inw(ioaddr + SHADOW(tx_block));
#if NET_DEBUG  > 2
		printk("|%x|", status);
#endif
		if (Stat_Busy(status)) {	
			/* the current packet is still in process, exit */
			kputchar('B');
			return(status);
		}
		if (!Stat_Done(status)) {
#if NET_DEBUG > 1
			unsigned short prev = 
			    (tx_block == tx_buf_start ? tx_last:tx_block-TX_BUF_SIZE);
			printk("X[%x/%x/%x]", tx_block, peek_buffer(tx_block/*prev+0xc*/), peek_buffer(prev+0x14));
			//dump_header(tx_block, XMIT_CMD_SIZE);
			//dump_header(prev, XMIT_CMD_SIZE);
#else
			kputchar('X');
#endif
			tx_link = tx_block;
			return status;
		} else {
			if (!Stat_OK(status)) {
				/*
				 * Mostly untested, we don't get these kinds of errors 
				 * easily on modern TP/switched networks */
				const char *whatsup = NULL;
				kputchar('E');
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
					whatsup = "FIFO underrun, discarded";
#ifdef TBD
					if (underrun++ < 4) {
						/* more testing needed */
						printk("FIFO underrun\n");
						ee16_restartCU(tx_block);
						return status;
					}
#endif
					//dev->stats.tx_fifo_errors++;
				}
				if (Stat_TXColl(status)) {
					whatsup = "aborted, too many collisions";
					//dev->stats.tx_aborted_errors++;
				}
				if (whatsup)
					printk("%s: transmit %s\n", dev_name, whatsup);
			}
		}
		tx_avail++;
		if (tx_block == tx_last)
			tx_reap = tx_block = tx_buf_start;
		else
			tx_reap = tx_block += TX_BUF_SIZE;

	} while (tx_reap != tx_head);

	tx_link = tx_tail + 0x08;
	return status;
}
/*
 * Retrieve one received backet into the provided
 * address, loop only on bad packets.
 */
static int ee16_get_packet(char *buffer, int len)
{
	unsigned short rx_block = rx_ptr;
	unsigned short boguscount = num_rx_bufs;
	unsigned short ioaddr = net_port;
	unsigned short status, pkt_len = 0;
 	unsigned short rfd_cmd, rx_next, pbuf;

#if NET_DEBUG > 2
	printk("get_packet (%x/%d)\n", rx_block, len);
#endif
 	do {
		if (pio_mode) {
			outw(rx_block, ioaddr + READ_PTR);
			status = inw(ioaddr + DATAPORT);
		} else
			status = shmem[rx_block/2];

		if (FD_Done(status)) {
			if (pio_mode) {
				rfd_cmd = inw(ioaddr + DATAPORT);
				rx_next = inw(ioaddr + DATAPORT);
				pbuf = inw(ioaddr + DATAPORT);	/* get ptr to data */
				outw(pbuf, ioaddr + READ_PTR);	/* prepare to read it */
				pkt_len = inw(ioaddr + DATAPORT);	/* 1st word is length+status */
			} else {
				rfd_cmd = (unsigned short)shmem[(rx_block+2)/2];
				rx_next = (unsigned short)shmem[(rx_block+4)/2];
				pbuf    = (unsigned short)shmem[(rx_block+6)/2];
				pkt_len = (unsigned short)shmem[pbuf/2];
			}
#if NET_DEBUG > 1
			printk("rx: len %d;", pkt_len&0x3fff);
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
				printk("%s: Rcv error, status 0x%x, len %d\n", status, pkt_len&0x3fff);
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

				/* do the actual data transfer, assuming there is always enough space */
				if (pio_mode) {
				    outw(pbuf+10, ioaddr+READ_PTR);
			            ee16_insw(ioaddr+DATAPORT, (word_t *)buffer, pkt_len);
				} else
				    fmemcpyw(buffer, current->t_regs.ds, (void *)(pbuf+10),
				    		shmem_seg, (pkt_len+1)>>1);
			}
			if (pio_mode) {
				outw(rx_block, ioaddr+WRITE_PTR);
				outw(0, ioaddr+DATAPORT);	/* clear status field */
				outw(0, ioaddr+DATAPORT);	/* clear cmd field */
			} else {
				shmem[rx_block/2] = 0;
				shmem[(rx_block+2)/2] = 0;
			}
			rx_block = rx_next;
			boguscount = 0; 	/* we can handle only one pkt for now */
		}
	} while (FD_Done(status) && boguscount--);

	rx_ptr = rx_block;
	return pkt_len;
}

/*
 * Hand a packet to the card for transmission
 * NOTE: If we get here, we MUST have already checked
 * to make sure there is room in the transmit
 * buffer region.
 */
static void ee16_put_packet(unsigned int ioaddr, char *buf, int len)
{
	unsigned short head = tx_head; 	/* concurrency protection */
	unsigned short tail = tx_tail; 	/* concurrency protection */
	unsigned short txcmd[XMIT_CMD_SIZE/2];

#if NET_DEBUG 
	unsigned short tail_stat = peek_buffer(tx_tail);
	//unsigned short __far *chks = _MK_FP(current->t_regs.ds, (unsigned)(buf+50));
	//printk("put: %x/%x/%x/%x(%x)", tx_head, tx_reap, *chks, tail_stat, len);
	printk("put: %x/%x/%x", tx_head, tx_reap, tail_stat);
#endif

	/* Developer Notes [this may be deprecated, but kept until we have 8bit mode running]:
	 * This is a remedy for the CU-Wedged problem which renders the CU idle
	 * at random intervals during transmission - when the 586 is in 8bit mode. Happens only when
	 * NIC buffersize is > 16k. There is no difference between PIO and SHMEM.
	 * 
	 * We check for zero status in the previous tx-block.
	 * If true, that frame has not been transmitted, but transmission 
	 * may still be in progress, so we check the time passed since we let it loose
	 * (the Busy_bit does not seem to be reliable at this point). 
	 * If more than one jiffie has passed, we assume the CU is wedged (even though the 
	 * CU status says OK), and restart the CU at the first uncompleted 
	 * block. Not a perfect solution, but it works and seems faster than the alternatives,
	 * like using a regular timeout mechanism. Still, there is the risk that heavy outgoing 
	 * traffic (packets piling up in the buffer), may cause false negatives.
	 */ 
	 /* If the CU suspend/resume mechanism (see below) is in operation, the recovery
	  * mechanism must be disabled. */

#define USE_RESTART_CU 3
#if USE_RESTART_CU == 2		/* recovery before new packet */
				/* recovery after: See below, the jury is out on which is best */
	if (tx_avail < num_tx_bufs && (jiffies - last_tx)) {
		ee16_restartCU(tx_reap);
	}
#endif

	/* NOTE: If interrupts are allowed here, the xmit-complete interrupt will hit almost
	 * immediately after we update the link (IOW, let the packet loose). At that point,
	 * tx_head MUST be pointing to the next available frame, not this one.
	 * Thus the need for head = tx_head above and the tx_head update just below -
	 * which may happen here or after the new packet is ready - thus the '#if 0'.
	 */
	tx_avail--;					
#if 0
	tx_tail = tx_head;
	if (tx_head == tx_last)
		tx_head = tx_buf_start;
	else
		tx_head += TX_BUF_SIZE;
	//set_irq();
#endif

	txcmd[0] = 0;
	txcmd[1] = Cmd_INT|Cmd_Xmit;
	txcmd[2] = head + 0x08;
	txcmd[3] = head + 0x0e;
	txcmd[4] = 0;
	txcmd[5] = 0;
	txcmd[6] = head + 0x08;
	txcmd[7] = 0x8000|len;
	txcmd[8] = 0xffff;
	txcmd[9] = head + 0x16;
	txcmd[10] = 0;
	clr_irq();	/*EXPERIMENTAL*/
	copyout(head, txcmd, kernel_ds, XMIT_CMD_SIZE);
	copyout(head+XMIT_CMD_SIZE, (void *)buf, current->t_regs.ds, len);
	set_irq();

#if USE_RESTART_CU == 3
	/* If this is placed before filling the buffer, we may get underrun errors,
	 * seemingly from memory contention. TO BE VERIFIED */
	if (tx_avail < (num_tx_bufs-1) && (jiffies - last_tx)) {
		ee16_restartCU(tx_reap);
	}
#endif

	/* if we update tx_head early, we may cause a race condition in the int handler,
	 * which compares tx_head and tx_reap. That said, we need to update them before
	 * the txcomplete interrupt for this packet hits, otherwise we have a different race condition. */
	tx_tail = tx_head;
	if (tx_head == tx_last)
		tx_head = tx_buf_start;
	else
		tx_head += TX_BUF_SIZE;

	last_tx = jiffies;	/* may want to track last TX complete int instead */

	/* Update the link in the previous block to point to this block.
	 * This will allow the CU to process this frame and almost always trigger an immediate
	 * TX complete interrupt. */
	if (pio_mode) {
		outw(tail+0xc, ioaddr + WRITE_PTR);
		outw(head, ioaddr + DATAPORT);
	} else
		shmem[(tail+0x0c)/2] = head;
		
	kputchar('P');
}

