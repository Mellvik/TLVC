# Welcome to TLVC - Tiny Linux for Vintage Computers

TLVC is a Linux-based OS for vintage 16bit PCs: A small (even floppy-based if desired), efficient, configurable tool for testing, diagnosing and understanding and playing with vintage PCs: IBM PC compatibles with ISA bus from the 5150 (original 8088 PC) to the 386. In short - TLVC is a platform for learning, experimenting, mastering, having fun with old computers: Getting them to run, check out hardware components, see what they can (and cannot) do, push them, becoming impressed with what the old clunkers with a few hundred Kbytes of RAM and a 4 or 12MHz processor can deliver. And not the least, to develop software to improve on and add to the system. No graphics, no gaming, just text/terminal I/O which is what this age of hardware is suited for. If you're so inclined, running 4 users - console, serial, telnet - concurrently is no problem - with enough memory (640k) and - preferably - XMS buffers.

TLVC is a fork of ELKS 0.6.0 with a different focus: OS efficiency and speed more than wide compatibility and applications. A key difference from ELKS is BIOS independence. While TLVC - like any other PC operating system - uses the BIOS to boot, that's where BIOS dependency ends. Native (aka 'direct') block device drivers makes the system faster and more responsive. It also allows for experimentation with protected mode if you're so inclined and have a 286 or newer system. 

Curious? Interested? Please check out the growing collection of really detailed and useful Wiki guides for more info:
[About TLVC](https://github.com/Mellvik/TLVC/wiki/About-TLVC#tlvc---tiny-linux-for-vintage-computers) - Getting Started with TLVC - Configure and Build TLVC - TLVC Networking Guide - TLVC and Emulators - [TLVC Memory and Buffer subsystem](https://github.com/Mellvik/TLVC/wiki/TLVC-Memory-and-Buffer-subsystem)

TLVC is ready to run. TCP/IP and many of your familiar Linux/Unix applications are included. The system is also in continuous development. Check it out, let us know what you think. You may not find the pre-configured binaries/images you'd like, so make a request. 

_The screen dump below (with a lot of debug info) shows the boot sequence for an early version of TLVC:_

<img width="836" alt="Skjermbilde 2023-07-11 kl  13 35 27" src="https://github.com/Mellvik/TLVC/assets/3629880/b3e6c735-4311-483d-a626-14ee214b820b">

### What TLVC is not
- … plug and play: You're expected to have some technical proficiency and experience with basic command line tools and development tools, like running menuconfig and familiarity with the make command.
- … covering all PC variants. TLVC is continuously being tested on several hardware platforms and QEMU but there will always be holes. In particular, pre-AT PCs are hard to come by, i.e. no testing thus far. Contributions welcome.
- … a gaming platform. Graphics support is not a priority in TLVC, consider it a text/terminal/command line system.

### Summary of enhancements since ELKS 0.6.0 (as of July 2023)
- Numerous bug fixes in the kernel and boot code
- Many fixes and enhancements to utilities
- A number of new and enhanced man pages
- New Ethernet drivers: Intel EtherExpress 16 and AMD Lance, the latter is under deveoplment
- Some Ethernet drivers have optional IO buffering which enables then to be completely interrupt driven: Increased stability and performance
- Many enhancements and bug fixes in ktcp, the user-space TCP/IP implementation.
- New 'direct' drivers replace the BIOS hd/floppy driver, making block IO completely interrupt driven. This improvement has improved the general percieved performance of the system as well as the performance and stability of the networking subsystem: No more long delays (while waiting for disk or floppy) which used to cause lots of retransmissions and lost packets.
- The cross-development system has been updated to run on Apple M- (native ARM) platforms.
- Command history has been added to the main shell (ash).
- Raw/char drivers have been added for disk/floppy IO, an important enhancement for both diagnostics and system utilities.
- Support for XT type disks (MFM, pre-IDE).
- Support for XT-IDE type disks.
- Improved user and developer documentation in the Wiki.

If you're coming from ELKS, you'll be delighted by the responsiveness of the system. If not, you'll just like the feeling - maybe be impressed by what an old klunker can deliver.
