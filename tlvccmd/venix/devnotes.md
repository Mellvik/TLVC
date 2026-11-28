_Venix 2.1 -> TLVC conversion, developer notes_

_intro_
TLVC and Venix have many commonalities - on the outside and on the inside. Being a 'pure' Unix system,
Venix has predictable, somewhat standardized APIs: System calls, commands, structure etc. And being a PC based system, the hardware level provides many similarities too.

These are what makes such a project viable. Like, mapping system calls is realtively easy. They are mostly the same, same parameters, same data types because the hardware is the same. For system calls in particular, the mappin consists of swapping parameters (and the syscall number) between different registers, replacing the INT vector and off we go.

This works as long as the data structures also match. Which they do at the file level, but not at the 
file system level. Like calling `stat` or `fstat` which takes thr `struct stat` as a parameter will 
fail even though the stuct looks identical - because the size of `ino_t` is different. There are many 
such cases - like when going into `struct dirent`.

This document discusses some of these findings, important details about the tools and valuable 
knowledge about how the Venix system innards work. E.g. analyzing Venix small-model (split I&D) binaries is not only required in order to port,
it's also important to understand the linker `ld` which we probably want to generate TLVC binaries instead.


_General_
- Terminal handling: The traditional sgtty-terminal handling has long since been replaced byt the much more flexible (and complicated) termios, introduced IIRC in AT&T Unix System III. Venix has one, TLVC has the other, V ery different structures and constants, and one thing in common: The bitcoding representing speeds (except termios has vaslies for higher speeds). Fortunately the ioctl codes are different, making it easy to recognize and handle the Venix V7 variants in the kernel (ntty pseudo driver). The mapping between sgtty and termios modes is not 100% accurate but good enough. This change introduces the CONFIG_COMPAT_V7 configuration option in TLVC, which will be used for all kernek changes related to this project.
- `struct stat`, mentioned above: This challenge is -at least for now - handled in the conversion scripts: 
A replacement 'fstat' allocates a TLVC `stat` struct on the stack, passes that to the real `stat` syscall and copies the content to the Venix struct on return. This may be a temporary solution but works for now.
- signal handler: Two issues to handle. The first is that the Venix version of `signal()` uses a
	segment-local (short) pointer to the signal handler, TLVC uses a far pointer, 32 bit. 
	The second is that the signal library routine sets up a bunch of signal vectors and
	trampoline for the handler in the code, which needs to the deciperhed and wither used 
	or deleted/replaced. For now what we do is replace the short function pointer with a long
	zero (SIG_DFL) so we can come back to it later. 
	The trampoline code etc. is left in place as dead code for now.
- lseek: turned out to be a real challenge. While the lseek() c_lib routines are (source) compatible
between Venix/V7 and TLVC, the actual syscalls are not. The TLVC syscall expects a 16 bit pointer to
a long as arg2, not a long spread over 2 registers like Venix. No problem except that pointer must be
in the kernel DS (TLVC is using `get_user_long()/put_user_long()`to do this). So there are two options:
Create a new TLVC system call or modify the one we have to behave differently when a Venix converted
binary is run. The latter was chosen, setting the high bit in the 3rd arg ('origin'/'whence') to indicate
V7 to the TLVC syscall. And put the high word of the long 'offset' parameter into the 4th parameter register
SI. Then another wall was hit. Since this is a 3-argument syscall, neither SI nor DI are available, they're
both clobbered.
Suddenly it got slightly more complicated, but here's a solution that works: The 'whence' (aka 'origin')
parameter - which usually holds values betwwn 0 and 2 - now holds the high word of the offset, shifted
left by 2. The high bit still set to indicate V7 to the TLVC syscall, so the high word of the offset
is truncated by 3 bits, but 13+16 bits is still more than enough. This 'conversion' is built into the
the python tools. Along the way it was discoverfed that Venix is replicating exactly the error handling already done in TLVC, so while at it, that was deleted from the Venix code.

Venix small-model binaries
Small-model binaries have ds=sp on entry which implies that the stack resides in the lower part, typically at 0x2000. It is likely (still guessing) that malloc then allocates from ds=0x0.
