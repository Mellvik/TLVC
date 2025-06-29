# Welcome to TLVC - Tiny Linux for Vintage Computers

TLVC is a Linux-based OS for vintage 16bit PCs: A small (even floppy-based if desired), efficient, configurable tool for testing, diagnosing and understanding and playing with vintage PCs: IBM PC compatibles with ISA bus from the 5150 (original 8088 PC) to the 386. In short - TLVC is a platform for learning, experimenting, mastering, having fun with old computers: Getting them to run, check out hardware components, see what they can (and cannot) do, push them, becoming impressed with what the old clunkers with a few hundred Kbytes of RAM and a 4 or 12MHz processor can deliver. And not the least, to develop software to improve on and add to the system. No graphics, no gaming, just text/terminal I/O which is what this age of hardware is suited for. If you're so inclined, running 4 users - console, serial, telnet - concurrently is no problem - with enough memory (640k) and - preferably - XMS buffers.

TLVC is a fork of ELKS 0.6.0 with a different focus: OS efficiency and speed more than wide compatibility and applications. A key difference from ELKS is BIOS independence. While TLVC - like any other PC operating system - uses the BIOS to boot, that's where BIOS dependency ends. Native (aka 'direct') block device drivers makes the system faster and more responsive. It also allows for experimentation with protected mode if you're so inclined and have a 286 or newer system. 

Curious? Interested? Please check out the growing collection of really detailed and useful Wiki guides for more info:
[About TLVC](https://github.com/Mellvik/TLVC/wiki/About-TLVC#tlvc---tiny-linux-for-vintage-computers) - Getting Started with TLVC - Configure and Build TLVC - TLVC Networking Guide - TLVC and Emulators - [TLVC Memory and Buffer subsystem](https://github.com/Mellvik/TLVC/wiki/TLVC-Memory-and-Buffer-subsystem)

TLVC is ready to run. TCP/IP and many of your familiar Linux/Unix applications are included. The system is also in continuous development. Check it out, let us know what you think. You may not find the pre-configured binaries/images you'd like, so make a request. 

_The screen dump below (which includes output from the BIOS and the boot process) shows the boot sequence for version 0.8.0 of TLVC:_

<img width="676" alt="Skjermbilde 2025-06-29 kl  13 53 10" src="https://github.com/user-attachments/assets/2f1d3ee2-8c89-49c9-bba3-38113f1dc3b1" />

### What TLVC is not
- … plug and play: You're expected to have some technical proficiency and experience with basic command line tools and development tools, like running menuconfig and familiarity with the make command.
- … covering all PC variants. TLVC is continuously being tested on several hardware platforms - XT, AT and later, plus QEMU, but there will always be holes.
- … a gaming platform. Graphics support is not a priority in TLVC, consider it a text/terminal/command line system.

### Summary of enhancements 
Check out the v0.8.0 release notes for a comprehensive run-through of the changes, enhancements and fixes since the fork.

Also, check out the [Wiki](https://github.com/Mellvik/TLVC/wiki) - lots of updated (and some not quite bnut almost update) reading about the system, the tools and the experience.

If you're coming from ELKS and running on real hardware, you'll probably be delighted by the responsiveness of the system. If not, you'll just like the feeling - maybe be impressed by what an old klunker can deliver.
