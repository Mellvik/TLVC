# Welcome to TLVC - Tiny Linux for Vintage Computers

TLVC is a Linux-based OS for vintage 16bit PCs: A small (even floppy-based if desired), efficient, configurable tool for testing, diagnosing, understanding and playing with vintage PCs: IBM PC compatibles with ISA bus from the 5150 (original 8088 PC) to the 386 and newer. A platform for learning, experimenting, mastering, having fun with old computers: Getting them to run, check out hardware components, find and fix problems, see what they can (and cannot) do, push them, and in many cases becoming impressed with what the old clunkers with a few hundred Kbytes of RAM and a 4 or 12MHz processor can deliver. And not the least, to develop software to improve on and add to the system. No graphics, no gaming, just text/terminal I/O which is what this age of hardware is suited for. If you're so inclined, running 4 users - console, serial, telnet - concurrently is no problem - with enough memory (640k) and - PC/AT and up - XMS for software, buffers, RAMdisk and more.

TLVC is a fork of ELKS 0.6.0 with a different focus: OS efficiency and speed more than wide compatibility and applications. A key difference from ELKS is BIOS independence. While TLVC - like any other PC operating system - uses the BIOS to boot, that's where BIOS dependency ends. Native (aka 'direct') block device drivers makes the system faster and more responsive. It also allows for experimentation with protected mode if you're so inclined and have a 286 or newer system. 

Curious? Interested? Please check out the growing collection of really detailed and useful Wiki guides for more info:
[About TLVC](https://github.com/Mellvik/TLVC/wiki/About-TLVC#tlvc---tiny-linux-for-vintage-computers) - Getting Started with TLVC - Configure and Build TLVC - TLVC Networking Guide - TLVC and Emulators - [TLVC Memory and Buffer subsystem](https://github.com/Mellvik/TLVC/wiki/TLVC-Memory-and-Buffer-subsystem)

TLVC is ready to run. TCP/IP and many of your familiar Linux/Unix commands and applications are included. The system is also in continuous development. Check it out, let us know what you think. You may not find the pre-configured binaries/images you need, so make a request. 

_The screen dump below (which includes output from the BIOS and the boot process) shows the boot sequence for version 0.8.0 of TLVC:_
```
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+   AMIBIOS System Configuration (C) 1985-1999, American Megatrends Inc.,   +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+ Main Processor   : i386SX                                                 +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+ Math Processor   : None             + Base Memory Size : 640KB            +
+ Floppy Drive A:  : 1.44 MB 3+"      + Ext. Memory Size : 19456KB          +
+ Floppy Drive B:  : 1.2  MB 5+"      + Serial Port(s)   : 3F8,2F8,3E8,2E8  +
+ Display Type     : VGA/EGA          + Parallel Port(s) : 378              +
+ AMIBIOS Date     : 05/16/00         + Processor Clock  : 40MHz            +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+ Hard Disk(s)       Cyl   Head Sector Size     LBA   32Bit Block PIO  UDMA +
+                                               Mode  Mode  Mode  Mode Mode +
+ Primary Master   : 993   16   63     513MB    LBA   Off   4Sec   4   N/A  +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
Searching for Boot Record from IDE-0..OK
Welcome to TLVC MBR Boot Manager
TLVC....Linux OK................................................................
.................................
TLVC Setup .........FHtFFFF f0250 d0698
moving bootopts (size 1722) from 8000:1812 to 698:41fe

Direct console, scan kbd 80x25 emulating ANSI (3 virtual consoles)
ttyS0 at 0x3f8, irq 4 is a 16550A
ttyS1 at 0x2f8, irq 3 is a 16550A
ttyS2 at 0x3e8, irq 7 is a 16550A
ttyS3 at 0x2e8, irq 10 is a 16550A
64 ext buffers (base @ 0x9000), 8K L1-cache, 20 req hdrs
eth: ne0 at 0x340, irq 9 not found
eth: wd0 at 0x320, irq 15, ram 0xcc00, (wd8013) MAC 00:00:C0:BC:8F:4B, type 0x29, flags 0x80
hd0: AT/IDE controller at 0x1f0
hda: IDE CHS: 993/16/63, Multisector I/O, max 4 sects
df: Floppy controller, FDC 8272A @ irq 6, DMA 2
df0: 1.44M (type 4), df1: 1.2M (type 2) [CMOS]
Floppy cache: 4k/7k
Partitions: hda:(0,1000944)  hda1:(63,131985)  hda2:(132048,133056)  hda3:(265104,133056)  hda4:(398160,601776) 
PC/AT class, cpu 7, syscaps 0xff, 640K base ram, 18 tasks, 64 files, 96 inodes
TLVC 0.6.0 (58848 text, 17536 ftext, 10240 data, 6640 bss, 48640 heap)
Kernel text 0xffff, ftext 0x250, init 0x41b, data 0x698, top 0xa000, 549K free
MINIX: inodes 21845 imap 3 zmap 8, first data 696
VFS: Mounted root 0x0503 (minix filesystem).
Running /etc/rc.sys script
fsck -a /dev/hda3: /dev/rhda3 is clean, no check.
Starting networking on wd0
ktcp -b -p wd0 10.0.2.17 10.0.2.1 255.255.255.0
ktcp: ip 10.0.2.17, gateway 10.0.2.1, netmask 255.255.255.0
ktcp: /dev/wd0 mac 00.00.c0.bc.8f.4b mtu 1500
Starting daemons 'telnetd' 'ftpd -d' 
Sun Jun 29 13:33:43 2025


[tlvc17] TLVC 0.8.0

login:
```
### What TLVC is not
- … plug and play: You're expected to have some technical proficiency and experience with basic command line tools and development tools, like running `menuconfig` and familiarity with the `make` command. Some understanding of hardware in general and the PC hardware platform in particular is a big plus.
- … covering all PC variants. TLVC is continuously being tested on several hardware platforms - XT, AT and later, plus QEMU and other emulators, but there will always be holes.
- … a gaming platform. Graphics support is not a priority in TLVC, consider it a text/terminal/command line system.

### Summary of enhancements 
Check out the [v0.8.0 release notes](https://github.com/Mellvik/TLVC/releases/tag/v0.8.0) for a comprehensive run-through of the changes, enhancements and fixes since the fork.

Also, check out the [Wiki](https://github.com/Mellvik/TLVC/wiki) - lots of updated (and some not quite up to date) reading about the system, the tools and the experience.

If you're coming from ELKS and running on real hardware, you'll probably be delighted by the responsiveness of the system. If not, you'll just like the feeling - maybe be impressed by what an old klunker can deliver.
