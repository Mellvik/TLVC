# Welcome to TLVC - Tiny Linux for Vintage Computers

TLVC is a Linux based OS for vintage 16bit PCs: A small (even floppy based if desired), efficient, configurable tool for testing, diagnosing and understanding and playing with vintage PCs: IBM PC compatibles with ISA bus from the 5150 (original 8088 PC) to the 386. In short - TLVC is a platform for learning, experimenting, mastering, having fun with old computers: Getting them to run, check out hardware components, see what they can (and cannot) do, push them, becoming impressed with what the old clunkers with a few hundred Kbytes of RAM and a 4 or 12MHz processor can deliver. And not the least, to develop software to improve on and add to the system. No graphics, no gaming, just text/terminal I/O which is what this age of hardware is suited for.

TLVC is a fork of ELKS 0.6.0 with a different focus: Efficiency and speed more than wide compatibility and applications. A key difference from ELKS is BIOS independence. While TLVC - like any other PC operating system - uses the BIOS to boot, that's where the BIOS dependency ends. This makes the system faster and more responsive. It also allows for experimentation with protected mode if you're so inclined and have a 286 or newer system. 

Curious? Interested? Please check out the Wiki for more info:
[About TLVC](https://github.com/Mellvik/TLVC/wiki/About-TLVC#tlvc---tiny-linux-for-vintage-computers) - Getting Started with TLVC - Configure and Build TLVC - TLVC Networking Guide - TLVC and Emulators

TLVC is ready to run. It's also in continuous development. Check it out, let us know what you think. We're just getting started on GitHub, expect some quirks while we hone things out.

_The screen dump below (with a lot of debug info) shows the boot sequence for an early version of TLVC:_

<img width="836" alt="Skjermbilde 2023-07-11 kl  13 35 27" src="https://github.com/Mellvik/TLVC/assets/3629880/b3e6c735-4311-483d-a626-14ee214b820b">

### What TLVC is not
- … plug and play: You're expected to have some technical proficiency and experience with basic command line tools and development tools, like running menuconfig and familiarity with the make command.
- … covering all PC variants. TLVC is continuously being tested on several hardware platforms and QEMU but there will always be holes. In particular, pre-AT PCs are hard to come by, i.e. no testing thus far. Contributions welcome.
- … a gaming platform. Graphics support is not a priority in TLVC, consider it a text/terminal/command line system.
