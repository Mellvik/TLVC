[Created April 2026, hs]
[Updated, expanded October 2026, hs]

# Intro
Venix/86 2.1 was an impressive system - for its time and even today. Created for the original (8088) PC it ran with
surprising efficiency handling several users on serial lines in addition to one or more (virtual) users on the physical console.

A complete Unix V7 implementation with all 'standard' tools - and then some. A full K&R C-compiler suite, `make`, `yacc`, `lex`, `nroff/tbl/eqn` and not the least, some UCB 'enhancements' like `more`, `csh` and `vi/ex`. Notably missing were manpages, which were paper only,
unsurprising given the disksizes available for PCs at the time (and licensing restrictions from AT&T). 

A so-called RECONF kit was available to OEMs (at a price), with instructions, tools and sufficient sources to enable development of device drivers. The RECONF (actually kernel building) process was lifted from BSD and worked reasonably well given the resource constraints.

Unfortunately, the Venix/86 source code seems to be lost. Unfortunate because the kernel had lots of enhancements and tailoring for small systems (refer to [Dissecting Venix/86](https://medium.com/@mindset3/vintage-computing-dissecting-venix-86-f8a9467e0a40) on Medium.com for details). The 'distance' between Unix V7 and Venix/86 was significant.
And also because some of the programs were surprisingly compact and the system reasonably fast. A Venix/86 system was completely self-compiling and the vi/ex implementation was complete, likely lifted from a BSD2.x(PDP-11/70) version.

The software development toolkit is what makes Venix/86 interesting in a TLVC context. Even though the C compiler is pure K&R, it does a decent job and would be a very nice addition to TLVC - turning the system into a software development platform.

## TLVC implementation

As of mid-2026 Venix/86 compatibility is available on TLVC. A shim syscall layer enables most Venix binaries to run unmodified and unrestricted on TLVC which opens a host 
of new capabilities on the system, many of them described in the 'implementation report' on [Medium.com](https://medium.com/@mindset3/reviving-venix-86-the-project-400c436aa4df), others in Wikis and readme-files and PR notes..

## What's included
This directory holds all Venix/V7 related files - binaries, sources, tools, data, man-pages and more, structured in a way that makes building TLVC HD images easy. Here's a rough runthrough of the subdirectories and their contets. The individual directories may have more details.

Note that alongside this directory there is also a V7 directory which holds ported V7 programs,
sources and binaries ported from V7, compiled and built like traditional TLVC progrtams with ia16-elf-xxx.
Like `ar` and `cpp`, both ported to gather experience, not because it in any way was necessary. 
Porting `ar` was in fact a significant challenge because Venix libraries (archive files) use BSD-style (ascii) headers instead of the original V7 binary headers. 
One of many examples that the creators of Venix looked forwards more than backwards when creating the system.
The ported binaries replace the Venix binaries where possible, but the Venix binaries are still available - with the extension .venix.
The `make` program may also be found in this directory. While currently not useful because it needs the same porting effort as `ar`, it's a natural candidate for doing so.

The asm-directory has memories from the development process and contains Venix-binaries disassembled, reassembled and linked using the ia16 toolkit. The .s files are from various generations of the conversion tools with varying properties, depending on the options used with the conversion tools. Some need the TLVC libc and the TLVC ctr0 for linking, some need only the latter, and some need none. The Makefile provices some indications.
Not necessarily practically useful, for reference mostly.

```
venix -+ asm	sources and binaries built while experimenting with various approaches to get
       |	get binaries across to TLVC. A few of these are available as binaries in the
       |	distribution. Kept for reference.
       |	
       |
       +-bin	Binaries from the Venix /bin directory. These are copied to /usr/bin in the
       |	distribution. Exceptions are /bin/ld and /bin/as which cc expects to be in /bin.
       |
       +-lib	Venix /lib directory, Venix library files and compiler binaries
       |
       +-usr-+
	     +bin	Venix binaries except some compiler components (in /bin and /usr/bin)
	     |
	     +lib	libraries not in /lib plus a host of other support files
	     |
	     +include	root of the stadard /usr/include directory
	     |
	     +man	root of the standard man directory, contains TlVC man pages
	     |
	     +vman	Venix/V7 man pages, kept separate form TLVC because they cannot
	     |		typically be rendered using the TLVC man command, but need nroff.
	     |		Venix Man pages ar few and far between, but the number keeps
	     |		growing.
	     +tools	testprograms for compilation and running on the target system, check
	     |		the local readme for details
	     +new	binaries not part of the venix distro, mostly from the sources in
	     |		src/new
	     +local	more unoriginal programs which may or may not be useful on TLVC,
	     |		sources lost in time.
	     +src-+
		  +new	sources to (most) of the programs in /usr/new
		  |
		  +v7	sources from Unix V7, useful for testing the compiler suite and
		  |	for making local adjustments. the bourne shell is there, and works.
		  |	nroff and troff are both there (same directory, same source), the 
		  | 	latter not part of Venix, but possibly useful if combined with
		  |	a cat-2-ps converter, which could well be on a different system 
		  |	if BSD lpr/lpd are available.
		  |	The make source is also included, not useful unless adapted to the 
		  |	new ar header format, but a great test.
```

## About man-pages and included commands
Many included commands overlap native TLVC commands. There are two reasons for their existence.
One is their value for testing - compatibility, speed, size. The other and most important is that
not all native commands are V7 compatible. In order to be able to build V7 programs from source, 
the commands expected by the build scripts (mostly `make`) must be available. So, instead of
adding more option to native utilities (which in many cases is the issue), the Venix versions are
made available and may be used instead by simply editing the makefiles.

Similarly - and for the same reasons - the Venix man pages for such commands (and more, like file formats)
are available in the `vman` directory.

