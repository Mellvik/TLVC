#!/usr/bin/env python3
"""
vrewrite.py v11 - Venix/86 to TLVC/ia16-elf-as assembly post-processor

Changes in v4:
  - .global (not .globl); only 'main' gets .global (others are local)
  - .ascii with explicit \0 for null-terminated strings (not .asciz)
  - Tab between mnemonic and operand for consistent column alignment
  - Data lines: 8 bytes per line, xxd-style |ascii| comment
  - String detection works across 16-byte boundaries
  - Data address references replaced with D_xxxx labels (like branches)
  - --replace-libc option: replace identified libc call wrappers with
    direct calls to TLVC libc symbols (skips wrapper definition too)
"""

VERSION = '11'
import sys, os, re, argparse, json, struct

# ---------------------------------------------------------------------------
# Venix/86 a.out symbol table parser
# ---------------------------------------------------------------------------
def load_venix_symbols(binary_path):
    """
    Parse symbol table from a Venix/86 a.out binary.
    Returns (text_syms, data_syms): dicts of {addr: name}.
    Format: 10-byte nlist entries + 2-byte-prefixed string table.
    """
    try:
        with open(binary_path, 'rb') as f:
            raw = f.read()
        a_text = struct.unpack_from('<I', raw, 4)[0]
        a_data = struct.unpack_from('<I', raw, 8)[0]
        a_syms = struct.unpack_from('<I', raw, 16)[0]
        if a_syms == 0:
            return {}, {}
        sym_start = 32 + a_text + a_data
        sym_end   = sym_start + a_syms
        str_raw   = raw[sym_end:]
        if len(str_raw) < 2:
            return {}, {}
        def get_str(strx):
            if strx < 2 or strx >= len(str_raw): return ''
            end = str_raw.find(b'\x00', strx)
            if end < 0: end = len(str_raw)
            return str_raw[strx:end].decode('ascii', errors='replace')
        ENTRY = 10
        text_syms, data_syms = {}, {}
        for i in range(a_syms // ENTRY):
            e = raw[sym_start + i*ENTRY : sym_start + (i+1)*ENTRY]
            if len(e) < ENTRY: break
            strx  = struct.unpack_from('<H', e, 0)[0]
            stype = e[2]
            value = struct.unpack_from('<I', e, 6)[0]
            name  = get_str(strx)
            if not name: continue
            t = stype & 0x1f
            if t in (0x04, 0x05):   # text local / text external
                if value not in text_syms or (stype & 0x01):
                    text_syms[value] = name
            elif t in (0x06, 0x07, 0x08, 0x09):  # data / bss
                if value not in data_syms or (stype & 0x01):
                    data_syms[value] = name
        return text_syms, data_syms
    except Exception as e:
        sys.stderr.write(f'; Warning: could not load symbols: {e}\n')
        return {}, {}

# ---------------------------------------------------------------------------
# TLVC syscall table
# ---------------------------------------------------------------------------
TLVC_SYSCALLS = {
    1:  (1,  'exit',    1, 1),   2:  (2,  'fork',    0, 0),
    3:  (3,  'read',    3, 3),   4:  (4,  'write',   3, 3),
    5:  (5,  'open',    2, 2),   6:  (6,  'close',   1, 1),
    7:  (7,  'wait4',   2, 2),   8:  (8,  'creat',   2, 2),
    9:  (9,  'link',    2, 2),   10: (10, 'unlink',  1, 1),
    11: (11, 'execve',  2, 2),   12: (12, 'chdir',   1, 1),
    13: (13, 'time',    1, 1),   14: (14, 'mknod',   3, 3),
    15: (15, 'chmod',   2, 2),   16: (16, 'chown',   3, 3),
    17: (17, 'brk',     1, 1),   18: (18, 'stat',    2, 2),
    19: (19, 'lseek',   3, 3),   20: (20, 'getpid',  0, 0),
    21: (21, 'mount',   3, 3),   22: (22, 'umount',  1, 1),
    23: (23, 'setuid',  1, 1),   24: (24, 'getuid',  0, 0),
    25: (None,'stime',  2,-1),   26: (26, 'ptrace',  4, 3),
    27: (27, 'alarm',   1, 1),   28: (28, 'fstat',   2, 2),
    29: (29, 'pause',   0, 0),   30: (30, 'utime',   2, 2),
    31: (54, 'ioctl',   2, 2),   32: (54, 'ioctl',   2, 2),
    33: (33, 'access',  2, 2),   34: (34, 'nice',    1, 1),
    35: (None,'ftime',  1,-1),   36: (36, 'sync',    0, 0),
    37: (37, 'kill',    2, 2),   39: (None,'setpgrp',1,-1),
    41: (41, 'dup',     1, 1),   42: (42, 'pipe',    1, 1),
    43: (43, 'times',   1, 1),   44: (44, 'profil',  4, 3),
    45: (None,'sema',   3,-1),   46: (46, 'setgid',  1, 1),
    47: (47, 'getgid',  0, 0),   48: (48, 'signal',  2, 2),
    49: (None,'sdata',  3,-1),   50: (None,'suspend',2,-1),
    52: (52, 'phys',    3, 3),   53: (53, 'lock',    1, 1),
    54: (54, 'ioctl',   3, 3),   59: (11, 'execve',  3, 3),
    60: (60, 'umask',   1, 1),   61: (31, 'chroot',  1, 1),
}

VENIX_SYSCALL_NAMES = {
    1:'exit',2:'fork',3:'read',4:'write',5:'open',6:'close',7:'wait',
    8:'creat',9:'link',10:'unlink',11:'exec',12:'chdir',13:'time',
    14:'mknod',15:'chmod',16:'chown',17:'brk',18:'stat',19:'lseek',
    20:'getpid',21:'mount',22:'umount',23:'setuid',24:'getuid',
    25:'stime',26:'ptrace',27:'alarm',28:'fstat',29:'pause',30:'utime',
    31:'stty',32:'gtty',33:'access',34:'nice',35:'ftime',36:'sync',
    37:'kill',39:'setpgrp',41:'dup',42:'pipe',43:'times',44:'profil',
    45:'sema',46:'setgid',47:'getgid',48:'signal',49:'sdata',
    50:'suspend',52:'phys',53:'lock',54:'ioctl',59:'exece',
    60:'umask',61:'chroot',
}

# Functions whose DEFINITIONS should be elided (TLVC/libgcc provides them)
ELIDE_DEFS = {
    # crt0
    'start', '_start',
    # fstat/stat: TLVC has 32-bit inode, Venix has 16-bit; layout differs.
    # Synthesized wrappers translate TLVC stat -> Venix stat layout.
    'fstat', '_fstat', 'stat', '_stat',
    # stty/gtty: TLVC now has ioctl support, use original Venix code.
    # exit family:
    # 'exit' is KEPT — the Venix exit() calls __cleanup() (flushes stdio) then __exit().
    # Keeping it ensures stdio is flushed before the process exits.
    # '__exit' and '_cuexit' are elided; 'call __exit' is renamed to 'call _exit'
    # (via BARE_RENAME) which routes to our synthesized raw exit syscall.
    # '__cleanup' and '_cleanup' are KEPT — they flush Venix's own _iob[] buffers.
    '__exit', '_cuexit',
    # Venix arith runtime - partial elision:
    #
    # ONE-INSTRUCTION signed stubs (call a_com then fall through) - elide:
    'almul', 'aulmul', 'lmul',    # signed multiply stubs
    'auldiv',                      # signed divide stub (uldiv is kept - user code calls it)
    'aldiv',                       # signed divide stub
    'alrem',                       # signed remainder stub
    'aulrem',                      # signed unsigned-remainder stub
    # a_com: by-ref/by-val adapter, called by the arith stubs AND by ulmul/uldiv/etc.
    # Do NOT elide: the kept functions (ulmul, uldiv, ldiv, lrem) call it.
    # 'a_com',  ← kept in output
    #
    # KEEP (real implementations called from user code or internally):
    #   ulmul  - unsigned multiply (used by ldiv, lrem)
    #   uldiv  - unsigned divide   (called directly from user code)
    #   ldiv   - signed divide     (called by lrem)
    #   lrem   - signed remainder  (uses ldiv + ulmul)
    #   ulrem  - unsigned remainder (called directly from user code)
    # These use Venix stack-based ABI and stay as local functions.
    #
    # Venix stdio (fprintf, printf, sprintf etc.) are KEPT.
    # They call __doprnt internally, which is also in the Venix binary.
    # All stdio functions were compiled together as a unit and must stay together.
}

# ---------------------------------------------------------------------------
# Symbol renames
# ---------------------------------------------------------------------------

def strip_underscore(name):
    if name.startswith('_') and len(name) > 1:
        rest = name[1:]
        if rest.startswith('_') or re.match(r'(?:L|sub)_[0-9a-f]', rest):
            return name
        # Keep _exit as _exit: routes to our synthesized raw exit syscall stub.
        # This is guaranteed to never return (direct kernel exit, no stdio flush).
        if rest == 'exit':
            return name   # keep as _exit
        return rest
    return name

RENAME_TRUNC = {
    'clearer':'clearerr',  'endgren':'endgrent',  'endpwen':'endpwent',
    'getgren':'getgrent',  'getgrgi':'getgrgid',  'getgrna':'getgrnam',
    'getlogi':'getlogin',  'getpwen':'getpwent',  'getpwna':'getpwnam',
    'getpwui':'getpwuid',  'localti':'localtime', 'setgren':'setgrent',
    'setpwen':'setpwent',  'strcatn':'strncat',   'strcmpn':'strncmp',
    'strcpyn':'strncpy',   'timezon':'timezone',  'closedi':'closedir',
    'wrefres':'wrefresh',  'touchwi':'touchwin',  'wclrtoe':'wclrtoeol',
    'wstande':'wstandend', 'wstando':'wstandout', 'overwri':'overwrite',
    'mvwprin':'mvwprintw', 'uucpnam':'uucpname',  'callbac':'callback',
    'notfyus':'notifyuser','pkaccep':'pkaccept',  'pkoutpu':'pkoutput',
    # stdio/scanf internals (flsbuf, filbuf, doprnt, strout, doscan, getccl,
    # innum, instr) are NOT renamed. The Venix binary defines and calls them
    # under the same 7-char truncated names — they resolve to each other.
}
# Venix arith runtime -> ia16-elf libgcc mapping
# These are elided entirely; ia16-elf libgcc provides the correct ABI versions.
# Call sites in code use these names and the linker resolves them from libgcc.
# DO NOT rename in code - the CALL SITES were already rewritten by vdis.py
# annotations to use the ia16 names.
ARITH_RENAME = {}   # elide, don't rename - see ELIDE_DEFS

# Documented mapping for reference:
#   almul/aulmul/lmul  -> __mulsi3   (signed 32-bit multiply)
#   ulmul              -> __umulsi3  (unsigned 32-bit multiply)
#   aldiv/ldiv         -> __divsi3   (signed 32-bit divide)
#   auldiv/uldiv       -> __udivsi3  (unsigned 32-bit divide)
#   alrem/lrem         -> __modsi3   (signed 32-bit remainder)
#   aulrem/ulrem       -> __umodsi3  (unsigned 32-bit remainder)

# stdio internals that appear without leading underscore after strip_underscore:
BARE_RENAME = {
    # 7-char truncated POSIX/C library names (bare form after strip_underscore).
    # These expand to their full names so calls resolve correctly at link time:
    'getlogi': 'getlogin', 'localti': 'localtime',
    # __exit / _cuexit: elided Venix raw-exit wrappers; route to our _exit synthesis.
    # Venix exit() calls __exit(status) after flushing stdio.
    # Renaming makes that call resolve to our synthesized _exit (raw kernel syscall).
    '__exit':  'exit_raw',    # raw kernel exit; distinct from _exit (flush+exit wrapper)
    '_cuexit': 'exit_raw',
    # Venix stdio internals (flsbuf, filbuf, doprnt, strout, doscan, getccl,
    # innum, instr) are NOT renamed here. Both their label definitions and their
    # call sites use the same 7-char truncated name. They resolve to each other.
}

def apply_renames(s):
    for old, new in ARITH_RENAME.items():
        s = re.sub(r'\b' + re.escape(old) + r'\b', new, s)
    for old, new in RENAME_TRUNC.items():
        s = re.sub(r'\b_' + re.escape(old) + r'\b', new, s)
    for old, new in BARE_RENAME.items():
        s = re.sub(r'\b' + re.escape(old) + r'\b', new, s)
    s = re.sub(r'\b(_[a-zA-Z][a-zA-Z0-9_]*)',
               lambda m: strip_underscore(m.group(0)), s)
    return s

# ---------------------------------------------------------------------------
# Syscall state machine
# ---------------------------------------------------------------------------

class SyscallSM:
    def __init__(self, v7layer=True, omagic=False):
        self.pending_sc  = None
        self._syscalls_used = None  # set by caller to collect used sc numbers
        self._v7layer = v7layer    # True = leave int $f1 intact (V7 shim mode)
        self._omagic  = omagic     # True = keep flat addresses in errno stores
        self.post_jcxz   = False
        self.in_fp       = False
        self.stack_guard = 0

    def feed(self, asm):
        # V7 layer mode: minimal intervention — only add syscall identification
        # comment and pass everything else through unchanged.
        if self._v7layer:
            # Track pending syscall number for the comment on int $0xf1:
            m = re.match(r'mov\s+\$0x([0-9a-f]+),%bx', asm)
            if not m:
                m2 = re.match(r'mov\s+\$(\d+),%bx', asm)
                if m2:
                    n = int(m2.group(1))
                    m = type('X',(),{'group':lambda s,i,n=n:f'{n:x}'})()
            if m:
                self.pending_sc = int(m.group(1), 16)
                # Don't add to _syscalls_used yet — wait for int $0xf1 to confirm
                # it's actually a syscall and not an argument or constant in %bx.
                vn = VENIX_SYSCALL_NAMES.get(self.pending_sc, f'?{self.pending_sc}')
                return [f'\tmov\t${self.pending_sc},%bx\t/* sc: {vn} */']
            if re.match(r'int\s+\$0xf1', asm):
                vn = VENIX_SYSCALL_NAMES.get(self.pending_sc, '?') if self.pending_sc else '?'
                sc = self.pending_sc
                self.pending_sc = None
                # Only now record the syscall — int $0xf1 confirms it was a syscall:
                if sc is not None and self._syscalls_used is not None:
                    self._syscalls_used.add(sc)
                comment = f'\t/* v7: {sc}:{vn} */' if sc else '\t/* v7: ? */'
                return [comment, '\tint\t$0xf1']
            # Everything else passes through unchanged (jcxz, mov %cx,errno, etc.)
            return None

        # FP triple: int $f4 / wait / fldcw  -> all suppressed
        if re.match(r'int\s+\$0xf4', asm):
            self.in_fp = True
            return []
        if self.in_fp:
            if asm.strip() in ('wait',): return []
            if 'fldcw' in asm:
                self.in_fp = False; return []
            self.in_fp = False

        # Stack guard: cmp $0x7f,%sp / ja / int $f2  -> all suppressed
        if re.match(r'cmp\s+\$(?:0x7f|127),%sp', asm):
            self.stack_guard = 1; return []
        if self.stack_guard == 1 and re.match(r'ja\s+', asm):
            self.stack_guard = 2; return []
        if self.stack_guard >= 1 and re.match(r'int\s+\$0xf2', asm):
            self.stack_guard = 0; return []
        if self.stack_guard > 0 and not re.match(r'ja\s+', asm):
            self.stack_guard = 0

        if re.match(r'int\s+\$0xf3', asm):
            return [f'\tcall\tabort']

        # Syscall number
        m = re.match(r'mov\s+\$0x([0-9a-f]+),%bx', asm)
        if not m:
            m2 = re.match(r'mov\s+\$(\d+),%bx', asm)
            if m2:
                n = int(m2.group(1))
                m = type('X',(),{'group':lambda s,i,n=n:f'{n:x}'})()
        if m:
            self.pending_sc = int(m.group(1), 16)
            vn = VENIX_SYSCALL_NAMES.get(self.pending_sc, f'?{self.pending_sc}')
            if self._v7layer:
                return [f'\tmov\t${self.pending_sc},%bx\t/* sc: {vn} */']
            return []  # record pending syscall silently; comment emitted at int $f1

        if re.match(r'int\s+\$0xf1', asm):
            return self._emit_syscall()

        # Only clear pending syscall if %bx is overwritten (syscall# changed).
        # Allow any instruction that computes %ax or %dx/%cx (argument setup),
        # including add/sub/and/or/lea/mov etc. The key invariant is that bx
        # holds the syscall number from "mov $N,%bx" until int $f1.
        if self.pending_sc is not None:
            # Instructions that write to %bx: clear pending_sc
            if re.search(r',%bx$|,%%bx$', asm) or re.match(r'pop\s+%bx', asm):
                self.pending_sc = None
            # int $f1 is handled above; other ints clear it
            elif re.match(r'int\s+', asm) and '$0xf1' not in asm:
                self.pending_sc = None

        # jcxz -> js
        if re.match(r'jcxz\s+', asm):
            if self._v7layer:
                # V7 layer mode: leave jcxz intact — the V7 shim sets cx correctly.
                # Mark post_jcxz so the following mov %cx,errno is also passed through.
                self.post_jcxz = True
                target = _addr_to_label(asm.split()[1])
                return [f'\tjcxz\t{target}']
            # TLVC full mode: jcxz → test %ax,%ax; jns
            # Venix: jcxz TARGET means "jump to TARGET if no error (cx==0)"
            # TLVC:  test+jns TARGET means "jump if ax>=0 (no error)"
            # test %ax,%ax is required: int $0x80 does not set flags on return.
            target = _addr_to_label(asm.split()[1])
            self.post_jcxz = True
            return [
                '\ttest\t%ax,%ax',
                '\t/* errno: jcxz -> jns (jump past errno-store if ax>=0) */',
                f'\tjns    {target}',
            ]

        # errno store after jcxz
        if self.post_jcxz and re.match(r'mov\s+%cx,', asm):
            if self._v7layer:
                # V7 layer mode: leave mov %cx,errno intact.
                dest = apply_renames(re.sub(r'^mov\s+%cx,\s*', '', asm))
                dest = _addr_ref_to_label(dest)
                self.post_jcxz = False
                return [f'\tmov\t%cx,{dest}\t/* errno (v7: cx=error code) */']
            dest = apply_renames(re.sub(r'^mov\s+%cx,\s*', '', asm))
            # Replace raw address with D_ label if it looks like one (not in omagic)
            if not self._omagic:
                dest = _addr_ref_to_label(dest)
            self.post_jcxz = False
            return [
                f'\tneg\t%ax',
                f'\tmov\t%ax,{dest}\t/* errno = -ax */',
                f'\tmov\t$0xffff,%ax\t/* return -1 on error */',
            ]
        self.post_jcxz = False
        return None

    def _emit_syscall(self):
        sc = self.pending_sc
        self.pending_sc = None
        if sc is None:
            return ['\t/* TODO: indirect syscall - rewrite manually */']

        # Record this syscall in the usage set for the header inventory:
        if sc is not None and self._syscalls_used is not None:
            self._syscalls_used.add(sc)

        vname = VENIX_SYSCALL_NAMES.get(sc, f'?{sc}')

        # V7 layer mode: leave int $0xf1 completely intact.
        # The TLVC V7 shim layer handles register conventions and error returns.
        # Venix code already does cx→errno correctly. Just add an id comment.
        if self._v7layer:
            return [
                f'\t/* v7: {sc}:{vname} */',
                '\tint\t$0xf1',
            ]
        info  = TLVC_SYSCALLS.get(sc)

        if info is None or info[0] is None:
            return [
                f'\t/* TODO: Venix-only {vname}({sc}): no TLVC equivalent */',
                f'\txor\t%ax,%ax\t/* stub */',
            ]

        tnum, tname, nargs, shuffle = info

        if sc == 5:   # open: Venix 2-arg, TLVC needs 3 (filename, flags, mode)
            # Venix: ax=filename, dx=flags (no mode arg)
            # TLVC:  bx=filename, cx=flags, dx=mode
            # V7 Unix: open(name, O_WRONLY/O_RDWR) creates file if needed.
            # TLVC POSIX: need O_CREAT|O_TRUNC for write-mode create.
            # Add O_CREAT(0x40)|O_TRUNC(0x200) when write bit is set in flags.
            return [
                f'\t/* open: Venix has no mode arg; supply mode=0666 for TLVC */',
                f'\txchg\t%ax,%bx',    # bx = filename
                f'\tmov\t%dx,%cx',     # cx = flags
                f'\txor\t%dx,%dx',     # dx = mode = 0
                f'\ttest\t$0x3,%cx',   # O_WRONLY(1) or O_RDWR(2)?
                f'\tje\topen_wronly_{sc}_{id(self) & 0xffff:04x}',
                f'\tand\t$0xfffc,%cx',  # clear access mode bits (remove O_RDWR)
                f'\tor\t$0x241,%cx',   # set O_WRONLY|O_CREAT|O_TRUNC cleanly
                f'\tmov\t$0x1b6,%dx',  # mode = 0666 (octal)
                f'open_wronly_{sc}_{id(self) & 0xffff:04x}:',
                f'\tmov\t$__NR_open,%ax',
                f'\tint\t$0x80',
                f'\t/* result in %ax; negative = errno */',
            ]

        if sc in (31, 32):
            op = 'TCSETS' if sc == 31 else 'TCGETS'
            return [
                f'\t/* TODO: {vname} -> ioctl(fd, {op}, &termios) */',
                f'\txchg\t%ax,%bx',
                f'\tmov\t%dx,%cx',
                f'\tmov\t$__NR_ioctl,%ax',
                f'\tint\t$0x80',
            ]
        if sc == 48:  # signal
            # Venix signal trampoline incompatible with TLVC far-pointer convention.
            # Pass SIG_DFL to suppress 'bad handler' messages; revisit for vi.
            return [
                f'\t/* using SIG_DFL: Venix trampoline incompatible with TLVC far-ptr convention */',
                f'\txchg\t%ax,%bx',    # bx = signum
                f'\txor\t%cx,%cx',     # cx:dx = SIG_DFL far ptr (0:0)
                f'\txor\t%dx,%dx',     # dx = upper word of handler far ptr = 0
                f'\tmov\t$__NR_signal,%ax',
                f'\tint\t$0x80',
                f'\t/* result in %ax; negative = errno */',
            ]

        lines = []
        if tnum != sc:
            lines.append(f'\t/* {vname}({sc}) -> {tname}({tnum}) [TLVC# changed] */')
        else:
            lines.append(f'\t/* {vname} -> __NR_{tname} */')

        if sc == 19:  # lseek: TLVC patched convention
            # Venix stack: fd=0x6(%bp) off_lo=0x8(%bp) off_hi=0xa(%bp) whence=0xc(%bp)
            # TLVC new encoding (matches updated shim layer):
            #   bx = 0x8000 | (whence << 8) | fd
            #     bit 15 set  = V7 lseek flag to shim
            #     bits 10:8   = whence (0=SET 1=CUR 2=END)
            #     bits  7:0   = fd
            #   cx = offset low word  (full 16 bits)
            #   dx = offset high word (full 16 bits, signed — supports negative SEEK_END offsets)
            lines = [f'\t/* Venix syscall {vname}({sc}) -> TLVC patched _lseek (V7 mode) */']
            lines.append(f'\tmov\t0xc(%bp),%bx\t\t/* whence into bx */')
            lines.append(f'\txchg\t%bl,%bh\t\t/* whence -> bh (high byte) */')
            lines.append(f'\tmov\t0x6(%bp),%bl\t\t/* fd -> bl (low byte) */')
            lines.append(f'\tor\t$0x80,%bh\t\t/* set V7 flag bit in high byte */')
            lines.append(f'\tmov\t0x8(%bp),%cx\t\t/* offset low word */')
            lines.append(f'\tmov\t0xa(%bp),%dx\t\t/* offset high word (full 16-bit signed) */')
            lines.append(f'\tmov\t$__NR_{tname},%ax')
            lines.append(f'\tint\t$0x80')
            lines.append(f'\t/* result: ax=new position low word dx=high word; negative ax = error */')
            return lines
        else:
            if shuffle >= 1: lines.append(f'\txchg\t%ax,%bx')
            if shuffle == 2: lines.append(f'\tmov\t%dx,%cx')
            elif shuffle >= 3: lines.append(f'\txchg\t%cx,%dx')

        lines.append(f'\tmov\t$__NR_{tname},%ax')
        lines.append(f'\tint\t$0x80')
        if nargs > 0:
            lines.append(f'\t/* result in %ax; negative = errno */')
        return lines

# ---------------------------------------------------------------------------
# Address / label helpers
# ---------------------------------------------------------------------------

def _addr_to_label(s):
    """'0x1234' or '1234' -> 'L_1234'"""
    try:
        if s.startswith('0x'):
            return f'L_{int(s,16):04x}'
        return f'L_{int(s):04x}'
    except:
        return s

def _addr_ref_to_label(s):
    """Replace 0xNNNN or -0xNNNN data address with D_nnnn in an operand string.
    Negative addresses (-0xNNNN) are 16-bit wrapped: actual = 0x10000 - 0xNNNN.
    """
    def repl(m):
        neg, hex_val = m.group(1), m.group(2)
        v = int(hex_val, 16)
        if neg:
            v = (0x10000 - v) & 0xffff   # e.g. -0x2406 → 0xdbfa
        return f'D_{v:04x}'
    return re.sub(r'(-?)(0x[0-9a-f]{4})\b', repl, s)

# ---------------------------------------------------------------------------
# Line parsers
# ---------------------------------------------------------------------------

RE_INSN  = re.compile(
    r'^\s{2}([0-9a-f]{6}):\s+((?:[0-9a-f]{2}\s)+)\s*([^;]+?)(?:\s{2,};.*)?$')
RE_DLINE = re.compile(
    r'^\s{2}([0-9a-f]{6}):\s+((?:[0-9a-f]{2}\s)+)\s*(?:\|.*\|)?\s*$')
RE_LABEL = re.compile(r'^(\S[\w.$@]*):$')
RE_CALL  = re.compile(r'^call\s+0x([0-9a-f]+)')
RE_BR    = re.compile(r'^(j\w+|loop)\s+0x([0-9a-f]+)')

def parse_insn(line):
    m = RE_INSN.match(line)
    if m:
        asm = re.sub(r'\s{2,};.*$', '', m.group(3)).strip()
        return m.group(1), asm
    return None, None

def parse_data_line(line):
    m = RE_DLINE.match(line)
    if m:
        return m.group(1), m.group(2).strip().split()
    return None, None

def rewrite_targets(asm, addr_name=None):
    """
    Replace 0xNNNN in calls/branches with labels.
    Uses named label if addr_name map has an entry, else L_nnnn.
    Uses 3 spaces (not tab) between mnemonic and operand to match
    objdump's existing instruction spacing.
    """
    def call_label(m):
        addr = int(m.group(1), 16)
        name = addr_name.get(addr) if addr_name else None
        if name:
            # Apply the same pipeline as label definitions so names match.
            name = apply_renames(strip_underscore(name))
        return f'call   {name}' if name else f'call   L_{addr:04x}'

    def br_label(m):
        addr = int(m.group(2), 16)
        name = addr_name.get(addr) if addr_name else None
        if name:
            name = apply_renames(strip_underscore(name))
        lbl = name if name else f'L_{addr:04x}'
        return f'{m.group(1)}   {lbl}'

    asm = RE_CALL.sub(call_label, asm)
    asm = RE_BR.sub(br_label, asm)
    return asm

def rewrite_text_refs(asm, a_text_size, branch_targets=None, valid_insn_addrs=None, flat_base=0, flat_hi=0):
    """For NMAGIC: replace movw $code_addr,mem with movw $L_label,mem.
    Code addresses stored as immediate function pointers need to become
    relocatable label references so the linker assigns correct cs addresses.
    Only rewrites values that are known branch targets (function entries),
    to avoid mis-converting small integer constants.
    """
    if not a_text_size:
        return asm
    # movw $val, stack/global = function pointer store
    m = re.match(r'(movw\s+\$)(0x[0-9a-f]+)(,.*)', asm)
    if m:
        val  = int(m.group(2), 16)
        dest = m.group(3)
        is_fp_dest = bool(re.match(r',\s*-?0x[0-9a-f]+\(%bp\)', dest) or
                          re.match(r',\s*D_[0-9a-f]{4}', dest))
        if is_fp_dest and 0 < val < a_text_size:
            # Only rewrite if target is a valid instruction start (not mid-instruction).
            if valid_insn_addrs is not None and val not in valid_insn_addrs:
                pass  # false positive: mid-instruction address
            else:
                is_target = branch_targets is not None and val in branch_targets
                # Require an explicit branch target: integer constants (token codes,
                # counts etc.) in text range must not be rewritten as code labels.
                # Only function pointers that are actually called/jumped to qualify.
                if is_target:
                    return f'{m.group(1)}L_{val:04x}{m.group(3)}'
    # --- NMAGIC flat-space data immediate pre-rewrite ---
    # Immediates in flat-space range [flat_base, flat_hi) are DATA pointers,
    # not code pointers. Convert $0xFLAT to $D_(FLAT-flat_base) before any
    # text-pointer checks, which would otherwise misidentify them.
    if flat_base and flat_hi:
        def _sub_flat_imm(m):
            prefix, val_s, suffix = m.group(1), m.group(2), m.group(3)
            val = int(val_s, 16)
            if flat_base <= val < flat_hi:
                # Emit D_FLAT (not D_DS). The NMAGIC post-pass will
                # convert D_FLAT → D_(FLAT-flat_base) = D_DS automatically.
                return f'{prefix}D_{val:04x}{suffix}'
            return m.group(0)
        # Match: $0xNNNN in any operand position
        asm = re.sub(r'((?:^|\s|,|\()\$)(0x[0-9a-f]{4})((?:\)|,|\s|$))',
                     _sub_flat_imm, asm)

    # add/sub $val,%ax = trampoline base address arithmetic
    # e.g. "add $0xa32,%ax" where 0xa32 = virtual base = first_entry - 6
    # Only rewrite values >= 0x800 to avoid false-positives (0x400 = buffer size etc.)
    m2 = re.match(r'((?:add|sub)\s+\$)(0x[0-9a-f]+)(,%ax)', asm)
    if m2:
        val = int(m2.group(2), 16)
        # Only rewrite if val >= 0x2000: signal trampolines are always
        # in the upper half of the text segment (they come after all app code).
        # Values like 0x4692 from FP instruction boundaries are false positives.
        if 0 < val < a_text_size and val >= 0x800:
            # First actual trampoline entry is at val+6 (val is virtual base before table).
            # valid_insn_addrs gates out FP instruction boundaries (mid-instruction false positives).
            first_entry = val + 6
            first_valid = (valid_insn_addrs is None or first_entry in valid_insn_addrs)
            if first_valid and branch_targets is not None and first_entry in branch_targets:
                return f'{m2.group(1)}(L_{first_entry:04x}-6){m2.group(3)}'
    # cmpw $val,dest where val is a text-space function pointer address
    # e.g. "cmpw $0x6ff,-0x6(%bp)" checks if a local var still holds the default fn ptr
    m3 = re.match(r'(cmpw\s+\$)(0x[0-9a-f]+)(,.*)', asm)
    if m3:
        val = int(m3.group(2), 16)
        dest = m3.group(3)
        is_fp_dest = bool(re.match(r',\s*-?0x[0-9a-f]+\(%bp\)', dest) or
                          re.match(r',\s*D_[0-9a-f]{4}', dest))
        # Threshold: only rewrite if val >= 0x400 to avoid false-positives
        # on integer constants (ASCII codes, counts etc. < 0x400).
        # Real function pointer comparisons are at larger addresses.
        if is_fp_dest and 0 < val < a_text_size and val >= 0x400:
            # Only rewrite if target is a valid instruction start.
            if valid_insn_addrs is not None and val not in valid_insn_addrs:
                pass  # false positive: mid-instruction address or data constant
            else:
                is_target = branch_targets is not None and val in branch_targets
                if is_target or val >= 0x400:
                    return f'{m3.group(1)}L_{val:04x}{m3.group(3)}'
    return asm


def rewrite_data_refs(asm, data_addrs, flat_base=0):
    """Replace 0xNNNN data addresses with D_nnnn labels.
    flat_base != 0 for NMAGIC: subtract flat_base to get ds-relative label.
    """
    def repl(m):
        neg, hex_val = m.group(1), m.group(2)
        v = int(hex_val, 16)
        if neg:
            v = (0x10000 - v) & 0xffff   # -0xNNNN → actual 16-bit address
        # Flat-base normalization: code operands may use flat-space addresses.
        # For NMAGIC: flat = a_stack + ds_offset → ds_offset = flat - a_stack.
        # For OMAGIC: flat = a_text + ds_offset  → ds_offset = flat - a_text.
        # In both cases flat_base = a_stack (NMAGIC) or a_text (OMAGIC).
        if flat_base and v >= flat_base:
            ds_v = v - flat_base
            # Check: is the flat address a known data ref?
            if v in data_addrs:
                return f'D_{ds_v:04x}'   # emit DS-relative label
            # Also check DS-relative directly (for pre-normalized refs):
            if ds_v in data_addrs:
                return f'D_{ds_v:04x}'
        # Direct ds-relative reference (v < flat_base or no flat_base)
        if v in data_addrs:
            return f'D_{v:04x}'
        return m.group(0)
    # Only skip DIRECT jumps/calls (not indirect jmp *addr which uses a data table).
    if re.match(r'(?:j\w+|call|loop)\s', asm):
        parts = asm.split(None, 1)
        if len(parts) < 2 or not parts[1].startswith('*'):
            return asm   # direct jump/call: leave operand as-is
    return re.sub(r'(-?)(0x[0-9a-f]{4})\b', repl, asm)

# ---------------------------------------------------------------------------
# Data segment reconstruction
# ---------------------------------------------------------------------------

def collect_branch_targets(lines):
    targets = set()
    for line in lines:
        _, asm = parse_insn(line)
        if not asm: continue
        for m in re.finditer(r'(?:j\w+|call|loop)\s+0x([0-9a-f]+)', asm):
            targets.add(int(m.group(1), 16))
    return targets


def build_addr_name_map(lines):
    """Build {addr: name} from named labels in vdis output."""
    addr_name = {}
    for i, line in enumerate(lines):
        stripped = line.strip()
        if not re.match(r'^_*[a-zA-Z][a-zA-Z0-9_]*:$', stripped):
            continue
        for j in range(i+1, min(i+6, len(lines))):
            m = re.match(r'^\s+([0-9a-f]{6}):', lines[j])
            if m:
                addr = int(m.group(1), 16)
                name = stripped.rstrip(':')
                name = apply_renames(name)
                addr_name[addr] = name
                break
    return addr_name


def merge_symbols_into_addr_name(addr_name, sym_text):
    """
    Merge Venix symbol table (addr→name) into vdis-derived addr_name dict.
    Symbol table entries take precedence over vdis-generated L_XXXX names.
    Preserves the raw symbol name; strip_underscore() handles the single-underscore
    stripping later in the rename pipeline.
    """
    for addr, name in (sym_text or {}).items():
        clean = name  # keep as-is; strip_underscore() is applied at label-emit time
        addr_name[addr] = clean
    return addr_name


def collect_data_refs(lines, data_base=0, data_end=0xffff):
    """Collect 0xNNNN addresses in instruction operands within data/bss range."""
    refs = set()
    for line in lines:
        _, asm = parse_insn(line)
        if not asm: continue
        # Skip direct jumps/calls (their operands are code labels, not data).
        # Allow indirect jumps (jmp *addr) — addr may be a data table pointer.
        if re.match(r'(?:j\w+|call|loop)\s', asm):
            parts = asm.split(None, 1)
            if len(parts) < 2 or not parts[1].startswith('*'):
                continue   # direct jump/call: skip
        if re.match(r'mov\s+\$0x[0-9a-f]+,%bx', asm): continue
        for m in re.finditer(r'(-?)(0x[0-9a-f]{4})\b', asm):
            neg, hex_val = m.group(1), m.group(2)
            v = int(hex_val, 16)
            if neg:
                v = (0x10000 - v) & 0xffff  # -0xNNNN → actual 16-bit address
            if data_base <= v <= data_end:
                refs.add(v)
    return refs


def find_crt0_calls(lines):
    calls = []
    for line in lines[:100]:
        _, asm = parse_insn(line)
        if asm:
            m = RE_CALL.match(asm)
            if m:
                calls.append(int(m.group(1), 16))
                if len(calls) >= 2: break
    return calls


def build_elided_addr_set(lines, elide_defs):
    """Find all instruction addresses within elided function bodies."""
    elided_set = {e.lstrip('_') for e in elide_defs}
    addr_set   = set()
    in_elided  = False
    for i, line in enumerate(lines):
        stripped = line.strip()
        if re.match(r'^_*[a-zA-Z][a-zA-Z0-9_]*:$', stripped):
            name = stripped.rstrip(':').lstrip('_')
            in_elided = name in elided_set
            continue
        m = re.match(r'^\s+([0-9a-f]{6}):', line)
        if m and in_elided:
            addr_set.add(int(m.group(1), 16))
    return addr_set


def build_libc_map(symdb_path, replace_libc):
    """Build {venix_symbol_name -> tlvc_name} from symdb for --replace-libc."""
    if not replace_libc or not symdb_path or not os.path.exists(symdb_path):
        return {}
    with open(symdb_path) as f:
        symdb = json.load(f)
    mapping = {}
    for vname, info in symdb.items():
        if info['library'] not in ('libc', 'libtermcap', 'libtermlib'):
            continue
        if info['seg'] != 'T': continue
        tname = info.get('tlvc_name')
        bare_v = vname.lstrip('_')
        bare_t = (tname or bare_v).lstrip('_')
        mapping[bare_v] = bare_t
    return mapping


def build_string_map(data):
    """
    Return bytearray marking bytes inside null-terminated printable strings.
    Used to distinguish real pointer words from coincidental values in strings.
    """
    n = len(data)
    in_str = bytearray(n)
    i = 0
    while i < n:
        if 0x20 <= data[i] < 0x7f or data[i] in (0x09, 0x0a):
            j = i
            while j < n and (0x20 <= data[j] < 0x7f or data[j] in (0x09, 0x0a)):
                j += 1
            if j < n and data[j] == 0 and (j - i) >= 4:
                for k in range(i, j + 1):
                    in_str[k] = 1
                i = j + 1
                continue
        i += 1
    return in_str


def reconstruct_data(raw_data_bytes, data_base, branch_targets, data_refs,
                     bss_end=0xffff, flat_base=0, is_omagic=False,
                     text_lo=0, text_hi=0):
    """
    Convert raw data bytes to gas directives.

    Key feature: detects embedded data/bss pointers and relocates them.
    In Venix OMAGIC (tiny model, cs=ds), pointer words contain absolute
    addresses. In TLVC small model, they must be ds-relative offsets.
    We subtract data_base from any word value that falls in [data_base, bss_end]
    and is NOT inside a string literal.

    - 8 bytes per .byte line with xxd-style |ascii| comment
    - Detected strings emitted as .ascii "...\0"
    - Pointer words emitted as .word (value - data_base)
    """
    import struct as _struct

    out    = []
    data   = bytes(raw_data_bytes)
    n      = len(data)
    in_str = build_string_map(data)
    all_labels      = data_refs | branch_targets

    def find_string(pos):
        j = pos
        while j < n and data[j] != 0 and 0x20 <= data[j] < 0x7f:
            j += 1
        slen = j - pos
        if slen < 4 or j >= n or data[j] != 0:
            return None
        if any((data_base + k) in all_labels for k in range(pos+1, j+1)):
            return None
        return j + 1

    # Pre-compute lookup table regions: 256-byte blocks where all values
    # appear (bijective table = EBCDIC/ASCII conversion). is_ptr skips these.
    _table_regions = set()
    _step = 256
    for _tstart in range(0, n - _step + 1, 1):
        # Check every 256-byte window (not just aligned) for lookup table signature
        # Only check aligned starts for performance:
        if _tstart % 4 != 0:
            continue
        _block = data[_tstart:_tstart + _step]
        # A lookup table has all 256 unique values (permutation)
        # or at minimum very high entropy (> 200 unique values in 256 bytes)
        _unique = len(set(_block))
        if _unique >= 200:
            for _ti in range(_tstart, _tstart + _step):
                _table_regions.add(_ti)

    def is_ptr(pos):
        """Return relocated value if word at pos is a data/bss pointer, else None."""
        if pos + 1 >= n:
            return None
        if in_str[pos] or in_str[pos+1]:
            return None
        # Skip pointer detection inside lookup table regions (EBCDIC/ASCII tables etc.)
        if pos in _table_regions or (pos + 1) in _table_regions:
            return None
        word, = _struct.unpack_from('<H', data, pos)
        # Note: NMAGIC binaries never reach this function (gated by is_omagic above).
        # For OMAGIC: all words in the flat pointer range are genuine pointers
        # (the linker pre-relocated them). No false-positive filtering needed.
        # NMAGIC: normalize flat-space addresses to ds-relative before range check.
        # Apply string-start check for ALL odd-offset targets (EBCDIC false positives).
        # Also check even-offset targets that are in .data range (not BSS):
        # BSS targets (>= a_data) are always valid (zero-init variables).
        # .data targets should start a valid string or be a known pointer.
        # This rejects cases like 0x3a,0x20 -> word 0x203a -> target 0x003a
        # where 0x003a points to EBCDIC garbage, not a real pointer target.
        _normalized = False  # track if flat_base already gave us DS-relative word
        if flat_base and flat_base <= word <= bss_end and word != 0:
            _norm = word - flat_base
            _in_bss = _norm >= len(data)  # BSS = beyond end of .data bytes
            if not _in_bss and not is_omagic:
                # NMAGIC only: validate .data target with string-start check to
                # filter false-positives (integer constants that happen to fall
                # in the data range). For OMAGIC, the linker pre-relocated every
                # pointer so all values in [flat_base, flat_base+bss_end] are genuine.
                #
                # Special case: self-referential pointers (_norm == pos) are always
                # genuine — no integer constant would equal its own location.
                # Their target bytes ARE the pointer value (not a string), so the
                # string check would always fail for them. Bypass it.
                _self_ref = (_norm == pos)
                _ok = _self_ref  # self-ref → always accept; others need string check
                if not _ok and _norm < len(data):
                    _b = data[_norm]
                    if 0x20 <= _b < 0x7f:
                        for _j in range(_norm, min(_norm + 40, len(data))):
                            if data[_j] == 0:
                                _ok = _j > _norm; break
                            if not (0x20 <= data[_j] < 0x7f) and data[_j] not in (9,10,13):
                                break
                if not _ok:
                    return None
            word = _norm
            _normalized = True
        # Second check: if flat_base normalized word, it's already DS-relative.
        # Compare against DS-relative range [0, bss_end-data_base].
        # Otherwise compare against flat range [data_base, bss_end].
        _ds_hi = bss_end - data_base  # DS-relative bss end
        if _normalized:
            _in_range = (0 <= word <= _ds_hi and word != 0)
            ds_rel = word
        else:
            _in_range = (data_base <= word <= bss_end and word != 0)
            ds_rel = word - data_base
        if _in_range:
            # ds_rel is already DS-relative (computed above).
            if ds_rel in all_labels:
                return ds_rel
            if is_omagic and 0 <= ds_rel <= _ds_hi:
                return ds_rel
        return None

    def emit_bytes(blist):
        hex_part = ', '.join(f'0x{b:02x}' for b in blist)
        asc_part = ''.join(chr(b) if 0x20 <= b < 0x7f else '.' for b in blist)
        out.append(f'\t.byte\t{hex_part}\t/* |{asc_part}| */')

    i = 0
    while i < n:
        addr = data_base + i

        # Label name: DS-relative for OMAGIC, flat for NMAGIC:
        lbl_name = addr - data_base if is_omagic else addr
        # data_refs stores DS-relative values; all_labels stores flat addresses.
        if lbl_name in data_refs or addr in all_labels:    out.append(f'D_{lbl_name:04x}:')
        if addr in branch_targets: out.append(f'L_{addr:04x}:')

        # Try string
        send = find_string(i)
        if send is not None:
            # Before consuming the string, check if any position within it
            # needs a data_refs label — if so, truncate and emit bytes instead.
            _str_break = None
            for _sk in range(i + 1, send):
                _sklbl = (data_base + _sk) - data_base if is_omagic else (data_base + _sk)
                if _sklbl in data_refs or (data_base + _sk) in all_labels:
                    _str_break = _sk; break
            if _str_break is not None:
                # Emit bytes up to the label position, then continue normally
                if _str_break > i:
                    emit_bytes(data[i:_str_break])
                i = _str_break
                continue
        if send is not None:
            try:
                s = bytes(data[i:send-1]).decode('ascii')
                s_esc = (s.replace('\\\\','\\\\\\\\')
                          .replace('"','\\"'  )
                          .replace('\n','\\n'  )
                          .replace('\t','\\t'  )
                          .replace('\r','\\r'  ))
                out.append(f'\t.ascii\t"{s_esc}\\0"')
                i = send
                continue
            except:
                pass

        # Embedded data/bss pointer: emit as label reference so linker relocates.
        # Only at even addresses, not inside strings.
        # Exception: if the ODD byte (i+1) has a label, don't fire is_ptr here
        # — falling through to byte emission preserves the odd-offset label.
        if i % 2 == 0:
            if is_omagic:
                # OMAGIC (CS=DS flat model): the linker pre-relocated all data pointers
                # to flat addresses. We must detect and convert them to DS-relative labels.
                rel = is_ptr(i)
                if rel is not None:
                    # If i+1 needs a label, don't consume it with a .word
                    if (data_base + i + 1) in all_labels:
                        pass  # fall through to byte emission
                    else:
                        flat_addr  = data_base + rel   # flat address of target
                        lbl_addr   = rel               # DS-relative label (OMAGIC)
                        target_lbl = f'D_{lbl_addr:04x}'
                        all_labels.add(flat_addr)
                        word_raw, = _struct.unpack_from('<H', data, i)
                        out.append(f'\t.word\t{target_lbl}'
                                   f'\t/* ptr: 0x{word_raw:04x} */')
                        i += 2
                        continue
            else:
                # NMAGIC (separate I&D): data addresses are already DS-relative.
                # No general pointer relocation needed — the linker placed correct values.
                # Exception: self-referential words (value == own DS-relative address)
                # must be emitted as .word D_XXXX so the linker produces the correct
                # run-time address (e.g. _iob_end limit pointer in stdio).
                if i + 1 < n:
                    word_raw, = _struct.unpack_from('<H', data, i)
                    # Self-referential: stored value equals flat address of this word.
                    # For NMAGIC with a_stack=flat_base: flat = flat_base + i.
                    _self_flat = (flat_base if flat_base else 0) + i
                    if word_raw == _self_flat and word_raw != 0:
                        if (data_base + i + 1) not in all_labels:
                            all_labels.add(data_base + i)
                            out.append(f'\t.word\tD_{i:04x}'
                                       f'\t/* self-ref ptr: 0x{word_raw:04x} */')
                            i += 2
                            continue
                    # Code-referenced address: this word's value appears in data_refs
                    # (code references this DS-relative address), so it may be a pointer.
                    # Only flag values >= 0x0100 to exclude small integer constants
                    # (character class flags, counts, etc.) which also land in data_refs.
                    if word_raw >= 0x0100 and word_raw in data_refs:
                        _lo_b = word_raw & 0xff
                        _hi_b = (word_raw >> 8) & 0xff
                        _pr = ''.join(chr(b) if 0x20 <= b < 0x7f else '.' for b in [_lo_b, _hi_b])
                        out.append(f'\t.byte\t0x{_lo_b:02x}, 0x{_hi_b:02x}'
                                   f'\t/* |{_pr}| possible ptr: DS:0x{word_raw:04x} */')
                        i += 2
                        continue

        # Emit up to 8 bytes, stopping at labels, strings, or pointer words
        chunk_end = min(i + 8, n)
        for k in range(i + 1, chunk_end):
            kaddr = data_base + k
            klbl  = kaddr - data_base if is_omagic else kaddr
            if kaddr in all_labels:
                chunk_end = k; break
            # Stop if this position needs a data_refs label:
            if klbl in data_refs:
                chunk_end = k; break
            if find_string(k) is not None:
                chunk_end = k; break
            # Stop before a pointer word at even offset
            if k % 2 == 0 and is_ptr(k) is not None:
                chunk_end = k; break

        emit_bytes(data[i:chunk_end])
        i = chunk_end

    return out



def rewrite(lines, name='unknown', replace_libc_map=None,
            data_bytes=None, data_base=0,
            sym_text=None, sym_data=None,
            v7layer=True,    # default: V7 shim mode (int $0xf1 passthrough)
            keep_crt0=False,
            omagic=False):
    out = []
    sm  = SyscallSM(v7layer=v7layer, omagic=omagic)
    _syscalls_used = set()
    sm._syscalls_used = _syscalls_used

    in_header   = True
    in_crt0     = False
    in_elided   = False
    in_data     = False
    crt0_done   = False

    replace_libc_map = replace_libc_map or {}

    branch_targets = collect_branch_targets(lines)
    # Build set of all instruction-start addresses from vdis output.
    # Used to gate text-address rewrites: only rewrite if target is a
    # real instruction start, not a mid-instruction byte.
    valid_insn_addrs = set()
    for _vl in lines:
        _vm = re.match(r'^\s+([0-9a-f]{6}):', _vl)
        if _vm:
            valid_insn_addrs.add(int(_vm.group(1), 16))



    # Detect errno address from the Venix jcxz/errno pattern.
    # Every syscall wrapper has: jcxz OK / mov %cx,0xNNNN  (errno)
    # This address is binary-specific; do not hardcode.
    errno_addr = None
    for _i, _line in enumerate(lines):
        if re.match(r'\s+[0-9a-f]+:.*\bjcxz\b', _line):
            for _j in range(_i+1, min(_i+4, len(lines))):
                _m = re.match(r'^\s+[0-9a-f]+:.*mov\s+%cx,0x([0-9a-f]+)', lines[_j])
                if _m:
                    errno_addr = int(_m.group(1), 16)
                    break
            if errno_addr:
                break
    errno_label = f'D_{errno_addr:04x}' if errno_addr else None  # may be renormalized below

    # Build set of addresses belonging to elided function bodies.
    # Branch targets within these ranges should stay suppressed.
    elided_addr_set = build_elided_addr_set(lines, ELIDE_DEFS)

    # Parse vdis header to get data/bss segment bounds for ref filtering
    data_base_addr  = None   # None = not yet found in header
    a_text_size     = 0
    a_data_size     = 0
    a_bss_size      = 0
    is_nmagic       = False
    a_stack_size    = 0
    for line in lines[:40]:
        # Magic line: detect NMAGIC vs OMAGIC
        if 'NMAGIC' in line:
            is_nmagic = True
        # Load: cs:0x0000 (text)   ds:0x18aa (data/bss)
        m = re.search(r'ds:0x([0-9a-f]+)', line)
        if m:
            data_base_addr = int(m.group(1), 16)
        m_text = re.search(r'a_text:\s+0x[0-9a-f]+\s+\((\d+) bytes\)', line)
        if m_text:
            a_text_size = int(m_text.group(1))
        m2 = re.search(r'a_data:\s+0x[0-9a-f]+\s+\((\d+) bytes\)', line)
        if m2:
            a_data_size = int(m2.group(1))
        m3 = re.search(r'a_bss:\s+0x[0-9a-f]+\s+\((\d+) bytes\)', line)
        if m3:
            a_bss_size = int(m3.group(1))
        m4 = re.search(r'a_stack:\s+0x([0-9a-f]+)', line)
        if m4:
            a_stack_size = int(m4.group(1), 16)
    # Pre-scan data section for text pointer words (function/handler tables).
    # These are flat text addresses stored in .data (e.g. dispatch tables).
    # Adding them to branch_targets ensures L_XXXX labels are emitted in text.
    import struct as _st_bt
    _in_data_bt = False
    _data_bytes_bt = bytearray()
    for _btl in lines:
        if 'DATA SEGMENT' in _btl: _in_data_bt = True
        if not _in_data_bt: continue
        _bm = re.match(r'^\s+([0-9a-f]{6}):\s+([0-9a-f ]{2,})', _btl)
        if _bm:
            _hex = _bm.group(2).strip().split()
            for _hb in _hex:
                try: _data_bytes_bt.append(int(_hb, 16))
                except ValueError: pass
    # Scan for words in text range:
    _txt_lo_bt = a_stack_size
    _txt_hi_bt = a_stack_size + a_text_size
    if _txt_lo_bt and len(_data_bytes_bt) >= 2:
        for _bi in range(0, len(_data_bytes_bt)-1, 2):
            _bw = _st_bt.unpack_from('<H', _data_bytes_bt, _bi)[0]
            if _txt_lo_bt <= _bw < _txt_hi_bt and _bw != 0:
                # Store as text-relative offset (flat - a_stack)
                # to match the convention used by the main instruction loop
                branch_targets.add(_bw - _txt_lo_bt)

    # For NMAGIC: code instructions reference data with flat-space addresses
    # (ds_offset + a_stack). We normalize by recording the flat-space base.
    # flat_base: subtracted from flat addresses in instructions to get DS-relative label names.
    # NMAGIC: instructions embed flat addresses = a_stack_size + ds_offset. flat_base = a_stack_size.
    # OMAGIC: instructions embed flat addresses = a_text + ds_offset. flat_base = a_text = data_base_addr.
    # For OMAGIC stack-below-text (all Venix native binaries):
    #   flat data base = a_stack + a_text = a_stack_size + data_base_addr
    # For NMAGIC: flat data base = a_stack_size (separate I&D, DS starts at 0)
    # For OMAGIC stack-above-text (hypothetical): a_stack=0, base = data_base_addr
    if is_nmagic:
        nmagic_flat_base = a_stack_size
    else:
        # OMAGIC: vdis now reports ds: = a_stack + a_text (correct flat base).
        # data_base_addr already contains the correct flat data base.
        nmagic_flat_base = data_base_addr if data_base_addr else 0

    # Pre-scan for NMAGIC function pointer movw patterns.
    # movw $0xNNNN,-0xN(%bp) where 0x100 <= val < a_text_size stores a
    # function pointer in a stack local. Add these as synthetic branch targets
    # so labels get emitted at those addresses in the text section.
    if is_nmagic and a_text_size:
        for _line in lines:
            # movw $val,-N(%bp) = function pointer stored to stack local
            _m = re.search(r'movw\s+\$(0x[0-9a-f]+),-?0x[0-9a-f]+\(%bp\)', _line)
            if _m:
                _val = int(_m.group(1), 16)
                if 0x100 <= _val < a_text_size:
                    branch_targets.add(_val)
            # add/sub $val,%ax = trampoline base address arithmetic
            _m2 = re.search(r'(?:add|sub)\s+\$(0x[0-9a-f]+),%ax', _line)
            if _m2:
                _val = int(_m2.group(1), 16)
                if 0x800 <= _val < a_text_size:
                    branch_targets.add(_val)
                    # Only add first_entry if it's a valid instruction start:
                    _first = _val + 6
                    if _first in valid_insn_addrs:
                        branch_targets.add(_first)

    # Now normalize errno address for NMAGIC (flat-space -> ds-relative)
    if not omagic and errno_addr and nmagic_flat_base and errno_addr >= nmagic_flat_base:
        errno_addr -= nmagic_flat_base
        errno_label = f'D_{errno_addr:04x}'
    # NMAGIC: data segment is separate, ds-relative base is always 0.
    # OMAGIC: data follows text in flat segment, base = text_size (non-zero).
    if data_base_addr is None:
        data_base_addr = 0
    # data_end_addr = flat address of end of BSS.
    # nmagic_flat_base is already the correct flat base of the data segment
    # (set above, accounts for a_stack for OMAGIC stack-below-text).
    data_end_addr = nmagic_flat_base + a_data_size + a_bss_size
    # Validate: if no data size found, or base is 0 for OMAGIC (shouldn't happen),
    # fall back to flagging nothing (safer than flagging everything).
    if data_end_addr <= data_base_addr:
        data_end_addr = 0xffff   # fallback: only if sizes not parsed
    # For NMAGIC: code operands have flat-space addresses (ds_offset + a_stack).
    # collect_data_refs uses the flat-space range; we normalize after.
    nmagic_code_base = nmagic_flat_base  # 0 for OMAGIC, a_stack for NMAGIC
    nmagic_code_end  = nmagic_flat_base + a_data_size + a_bss_size
    if is_nmagic:
        data_refs = collect_data_refs(lines, nmagic_code_base, nmagic_code_end)
        # Normalize: subtract flat_base to get ds-relative addresses
        data_refs = {a - nmagic_flat_base for a in data_refs}
        # Also collect flat-space DATA immediates ($0xFLAT in code instructions).
        # These are pointers stored as immediates (FILE*, string ptrs, etc.).
        # They become $D_(FLAT-flat_base) and need data labels.
        _flat_hi_pre = nmagic_flat_base + a_data_size + a_bss_size
        import re as _re_fi
        for _vl in lines:
            if not _re_fi.match(r'^\s+[0-9a-f]{6}:', _vl):
                continue
            for _m in _re_fi.finditer(r'\$(?:0x)([0-9a-f]{4})\b', _vl):
                _v = int(_m.group(1), 16)
                if nmagic_flat_base <= _v < _flat_hi_pre:
                    data_refs.add(_v - nmagic_flat_base)
    else:
        data_refs = collect_data_refs(lines, data_base_addr, data_end_addr)
        # Normalize OMAGIC data_refs from flat to DS-relative:
        # collect_data_refs returns flat addresses; reconstruct_data
        # expects DS-relative (0-based within data segment).
        if nmagic_flat_base:
            data_refs = {a - nmagic_flat_base for a in data_refs
                         if a >= nmagic_flat_base}
    crt0_calls     = find_crt0_calls(lines)
    main_addr      = crt0_calls[0] if crt0_calls else None
    addr_name      = build_addr_name_map(lines)   # addr -> named label
    # Merge symbol table entries (real function names) into addr_name.
    # These override opaque L_XXXX labels and enable proper elision/linking.
    if sym_text:
        addr_name = merge_symbols_into_addr_name(addr_name, sym_text)
    seen_labels    = set()   # track emitted labels to catch arith duplicates

    # Functions to elide: ELIDE_DEFS + libc replacements (when --replace-libc)
    elide_set = set(ELIDE_DEFS)
    if replace_libc_map:
        elide_set.update(replace_libc_map.keys())
        elide_set.update('_'+k for k in replace_libc_map.keys())

    def should_elide(label):
        # Never elide _exit: it's the Venix flush+exit wrapper we need to keep.
        # (It gets wrongly matched because '__exit'.lstrip('_') == 'exit' == '_exit'.lstrip('_'))
        if label in ('_exit', 'exit'):
            return False
        # In v7layer mode: stat/fstat are handled by the V7 shim — keep the Venix versions.
        # Also keep exit_raw label (it's our synthesis, not a Venix label to elide).
        if v7layer and label in ('stat', '_stat', 'fstat', '_fstat',
                                   'exit_raw', '__exit', '_cuexit'):
            return False
        bare = label.lstrip('_')
        return bare in {e.lstrip('_') for e in elide_set} or label in elide_set

    # Header
    out += [
        f'/* {name}.s — Venix/86 -> TLVC/ia16-elf',
        f' * Rewritten by vrewrite.py v11',
        (f' * Assemble: ia16-elf-as -march=8087 {name}.s -o {name}.o'
         f'\n * Link (OMAGIC tiny): ia16-elf-gcc -mcmodel=tiny -nostdlib -o {name} {name}.o'
         if omagic else
         f' * Assemble: ia16-elf-as -march=8087 {name}.s -o {name}.o'),
        f' * Syscalls: (see end of header)',
        f' */',
        '',
        '\t.code16',
        '\t.include "venix_syscalls.inc"',
        '\t.text',
        ('\t/* crt0: Venix crt0 preserved — link without crt0.o */' if keep_crt0 else '\t/* crt0 provided by TLVC crt0.o */'),
        '',
    ]

    data_collected = []   # raw (addr, bytes) pairs for data section

    for line in lines:

        # ---- Header block ----
        if in_header:
            if 'TEXT SEGMENT' in line:
                in_header = False
                in_crt0   = True
            elif line.startswith(';'):
                if any(k in line for k in ('a_text','a_data','a_bss','a_syms','a_entry',
                                            'a_trsize','a_drsize','Load:','Magic:',
                                            'a_stack','OMAGIC','NMAGIC','object',
                                            'executable','File layout','bss:')):
                    out.append('//' + line[1:])  # gas: // not ;
            continue

        # ---- Data section ----
        if 'DATA SEGMENT' in line:
            in_data   = True
            in_crt0   = False
            in_elided = False
            continue

        if re.match(r'^\s*$', line):
            if not in_crt0 and not in_elided and not in_data:
                out.append('')
            continue

        if 'TEXT SEGMENT' in line:
            continue

        if line.startswith(';'):
            if any(k in line for k in ['====','File layout','strtab','symtab',
                                        'Symbol DB','TLVC PORTING','Patched',
                                        'crt0 entry']):
                continue
            if not in_crt0 and not in_elided:
                # gas uses // for line comments, not ;
                out.append('//' + line[1:])
            continue

        # ---- Data: collect raw bytes ----
        if in_data:
            addr_s, byte_list = parse_data_line(line)
            if byte_list:
                addr = int(addr_s, 16)
                data_collected.append((addr, [int(b,16) for b in byte_list]))
            # Labels in data ignored (handled in reconstruction)
            continue

        # ---- Label lines ----
        stripped = line.strip()
        if RE_LABEL.match(stripped):
            raw = stripped[:-1]
            lbl = apply_renames(raw)
            # If addr_name has an entry for this label's address, use that name.
            # This ensures label definitions match the call targets from addr_name.
            # Works for both L_XXXX labels AND named labels (e.g. __flsbuf → flsbuf).
            _lbl_m = re.match(r'^L_([0-9a-f]+)$', lbl)
            if _lbl_m and addr_name:
                # L_XXXX: address is in the label name
                _lbl_addr = int(_lbl_m.group(1), 16)
                _real_name = addr_name.get(_lbl_addr)
                if _real_name:
                    lbl = strip_underscore(_real_name)
            elif addr_name:
                # Named label (e.g. '__flsbuf'): look up by scanning ahead for address
                _scan_idx = lines.index(line) if line in lines else -1
                if _scan_idx >= 0:
                    for _sl in lines[_scan_idx+1:_scan_idx+4]:
                        _am = re.match(r'^\s+([0-9a-f]{6}):', _sl)
                        if _am:
                            _lbl_addr = int(_am.group(1), 16)
                            _real_name = addr_name.get(_lbl_addr)
                            if _real_name:
                                # Apply full rename pipeline to addr_name result
                                # so truncated names (getlogi→getlogin) get expanded:
                                lbl = apply_renames(strip_underscore(_real_name))
                            break

            # crt0 end
            if in_crt0:
                m = re.match(r'L_([0-9a-f]+)', lbl)
                addr = int(m.group(1),16) if m else None
                is_main = (main_addr and addr == main_addr) or lbl in ('main','_main')
                # In keep_crt0 mode: only end at main (by name or address),
                # NOT at the first L_XXXX label — that may be a branch target
                # inside the crt0 itself (e.g. the argv scan loop label).
                if keep_crt0:
                    is_first_fn = False   # ignore L_XXXX inside crt0
                else:
                    # Normal mode: first L_XXXX = first real function = end of crt0
                    is_first_fn = (m is not None)
                # Also end crt0 on named functions from the symbol table.
                # When --syms/--symmap is used, real function names replace L_XXXX;
                # these are just as valid as L_XXXX for marking crt0 end.
                # 'start'/'_start' are crt0 entry labels, not real functions.
                _crt0_labels = {'start', '_start'}
                is_named_fn = (m is None and lbl not in ('main','_main')
                               and (not keep_crt0 or lbl not in _crt0_labels)
                               and re.match(r'^[a-zA-Z_][a-zA-Z0-9_]*$', lbl))
                if is_main or is_first_fn or is_named_fn:
                    in_crt0 = False; crt0_done = True
                elif keep_crt0:
                    # Emit labels inside the crt0 (branch targets etc.)
                    out.append('')
                    out.append(f'{lbl}:')
                    continue
                else:
                    continue

            # Elision
            if should_elide(lbl) or should_elide(raw):
                in_elided = True
                out.append(f'\t/* {lbl}: elided (provided by crt0 or vrewrite synthesis) */')
                continue

            if in_elided:
                in_elided = False

            # Skip duplicate labels (multiple Venix arith symbols rename
            # to the same ia16 builtin e.g. almul+aulmul -> __mulsi3)
            if lbl in seen_labels:
                in_elided = True
                out.append(f'\t/* {lbl}: duplicate label (arith rename) — elided */')
                continue
            seen_labels.add(lbl)

            out.append('')
            # Only 'main' gets .global
            if lbl == 'main':
                out.append(f'\t.global\tmain')
                out.append(f'\t.type\tmain,@function')
            out.append(f'{lbl}:')
            continue

        # ---- Instruction lines ----
        addr_s, asm = parse_insn(line)
        if addr_s:
            addr  = int(addr_s, 16)
            asm_r = apply_renames(asm)

            if keep_crt0 and in_crt0 and not in_elided:
                # --keep-crt0: pass crt0 instructions through.
                # Crt0 has no data refs typically; just rewrite branch targets.
                asm_out = rewrite_targets(asm_r, addr_name)
                out.append(f'\t{asm_out}')
                prev = line
                continue
            if in_crt0 or in_elided:
                if in_crt0 and main_addr and addr == main_addr:
                    in_crt0 = False; crt0_done = True
                elif in_elided and addr in branch_targets:
                    # This address is called from outside.
                    # Stay suppressed if addr is within an elided function body.
                    if addr in elided_addr_set:
                        continue   # internal to an elided block
                    target_name = addr_name.get(addr, '')
                    target_bare = target_name.lstrip('_')
                    target_elided = (
                        target_bare in {e.lstrip('_') for e in elide_set}
                        or target_name in elide_set
                    )
                    if target_elided:
                        continue   # named target is also elided
                    # Stop eliding - target is a real external function
                    in_elided = False
                    lbl = target_name if target_name else f'L_{addr:04x}'
                    out.append('')
                    out.append(f'{lbl}:')
                    # fall through to normal instruction processing
                else:
                    continue

            # Branch target label
            if addr in branch_targets:
                prev = out[-1].strip() if out else ''
                lbl  = f'L_{addr:04x}'
                if not (prev.endswith(':') or prev == f'{lbl}:'):
                    out.append(f'{lbl}:')

            # Data reference label
            if addr in data_refs and addr not in branch_targets:
                pass   # handled by D_xxxx in operand rewriting below

            # Syscall state machine
            result = sm.feed(asm_r)
            if result is not None:
                out.extend(result)
                continue

            # NOP
            if asm_r == 'nop':
                out.append('\t/* .byte 0 (alignment) */')
                continue

            # Indirect call
            if re.match(r'call\s+\*', asm_r):
                out.append('\t/* TODO: indirect call - verify */')
                out.append(f'\t{asm_r}')
                continue

            # Library call replacement
            cm = re.match(r'call\s+L_([0-9a-f]+)', rewrite_targets(asm_r))
            # (handled below after rewrite_targets)

            # signal/stty/gtty
            if re.match(r'call\s+(signal|stty|gtty)\b', asm_r):
                if 'signal' in asm_r:
                    out.append('\t/* TODO: signal() -> sigaction() */')
                else:
                    fn = 'TCSETS' if 'stty' in asm_r else 'TCGETS'
                    out.append(f'\t/* TODO: -> ioctl(fd, {fn}, &termios) */')
                out.append(f'\t{asm_r}')
                continue

            # Data address TODO - only for addresses in data/bss range
            has_data_ref = False
            # Allow indirect jumps through data tables (jmp *addr):
            _is_indir = (re.match(r'(?:j\w+|call|loop)\s', asm_r) and
                         len(asm_r.split(None,1)) > 1 and
                         asm_r.split(None,1)[1].startswith('*'))
            if ((not re.match(r'(?:j\w+|call|loop)\s', asm_r) or _is_indir) and
                    not re.match(r'mov\s+\$0x[0-9a-f]+,%bx', asm_r)):
                for hm in re.finditer(r'0x([0-9a-f]{4})\b', asm_r):
                    v = int(hm.group(1), 16)
                    if data_base_addr <= v <= data_end_addr:
                        has_data_ref = True
                        break
            if has_data_ref:
                out.append(f'\t/* TODO: data ref: {asm_r} */')

            # Handle movw $0xSRCC,0xDST where both source and dest are data addresses.
            # Both need relocation. ia16-elf-as can only generate one reloc per instruction,
            # so we split into: 
            # In omagic mode: addresses are flat, no relocation needed — skip split.
            if omagic:
                out.append(f'\t{asm_r}')
                prev = line; continue
            # mov $D_SRC,%ax; mov %ax,D_DST.
            # NOTE: self-referential cases (SRC==DST) are NOT suppressed — at runtime
            # the program may zero the target region (e.g. fopen zeroes _iob[]) and use
            # this movw to restore a self-pointer. We always emit the split form.
            import re as _re2
            _self_ref = _re2.match(
                r'movw\s+\$0x([0-9a-f]{4}),0x([0-9a-f]{4})', asm_r)
            if _self_ref:
                _src_h = _self_ref.group(1)
                _dst_h = _self_ref.group(2)
                # Normalize NMAGIC/OMAGIC flat-space addresses to ds-relative:
                def _norm_h(h):
                    v = int(h, 16)
                    if nmagic_flat_base and v >= nmagic_flat_base:
                        ds_v = v - nmagic_flat_base
                        if ds_v in data_refs: return f'{ds_v:04x}'
                    return h
                _src_h = _norm_h(_src_h)
                _dst_h = _norm_h(_dst_h)
                # Two different labels: needs dual relocation; split via %ax.
                # If source is a literal constant (not in data range), emit as literal.
                _src_v = int(_src_h, 16)
                _dst_v = int(_dst_h, 16)
                # Only emit D_ labels for addresses within the STATIC data+bss
                # range. Addresses above that are runtime heap (sbrk'd) and
                # must stay as plain flat immediates — no linker label needed.
                _dst_static = 0 < _dst_v <= data_end_addr
                if not _dst_static:
                    # Runtime address: emit as plain flat store, no D_ label.
                    out.append(f'\t/* movw to runtime addr 0x{_dst_h} (heap/beyond static) */')
                    if 0 < _src_v <= data_end_addr:
                        out.append(f'\tmov\t$D_{_src_h},%ax')
                    else:
                        out.append(f'\tmov\t$0x{_src_h},%ax')
                    out.append(f'\tmov\t%ax,0x{_dst_h}')
                elif 0 < _src_v <= data_end_addr:
                    out.append(f'\t/* dual-reloc split: movw $D_{_src_h},D_{_dst_h} */')
                    out.append(f'\tmov\t$D_{_src_h},%ax')
                    out.append(f'\tmov\t%ax,D_{_dst_h}')
                else:
                    out.append(f'\t/* movw constant -> D_{_dst_h} */')
                    out.append(f'\tmov\t$0x{_src_h},%ax')
                    out.append(f'\tmov\t%ax,D_{_dst_h}')
                continue

            # Apply all rewrites
            asm_r = rewrite_targets(asm_r, addr_name)
            # Apply bare-name renames after rewrite_targets (converts e.g. flsbuf→__flsbuf)
            for _bfrom, _bto in BARE_RENAME.items():
                asm_r = re.sub(r'\b' + re.escape(_bfrom) + r'\b', _bto, asm_r)
            asm_r = rewrite_data_refs(asm_r, data_refs, flat_base=nmagic_flat_base) if not omagic else asm_r
            if is_nmagic:
                asm_r = rewrite_text_refs(asm_r, a_text_size, branch_targets, valid_insn_addrs,
                                      flat_base=nmagic_flat_base,
                                      flat_hi=nmagic_flat_base + a_data_size + a_bss_size)

            # Check for libc replacement after label substitution
            if replace_libc_map:
                cm2 = re.match(r'call\tL_([0-9a-f]+)', asm_r)
                # (Would need addr->name map; leave as future enhancement)

            out.append(f'\t{asm_r}')
            continue

        if line.strip():
            out.append(f'/* {line.rstrip()} */')

    # ---- Ensure text pointer labels from .data table exist in .text ----
    # Words in .data that are text addresses (function/handler pointers)
    # need corresponding L_XXXX labels in the text section.
    if '_text_ptr_lbls' in dir() and _text_ptr_lbls:
        _text_lines = []
        for _tl in out:
            _text_lines.append(_tl)
            # Insert label if this is the instruction at the target address:
        # Scan out and insert labels at the right text positions:
        import re as _re_tp
        for _tp_flat in sorted(_text_ptr_lbls):
            _tp_lbl = f'L_{_tp_flat:04x}:'
            if _tp_lbl not in '\n'.join(out):
                # Find the instruction at this flat address and prepend the label:
                _addr_str = f'  {_tp_flat - a_stack_size:06x}:'
                for _ti, _tline in enumerate(out):
                    if _addr_str in _tline or f'  {_tp_flat:06x}:' in _tline:
                        out.insert(_ti, f'{_tp_lbl}')
                        break
                else:
                    # Not found by address — add as synthetic label before
                    # the first use of L_XXXX in the .data lines:
                    pass  # label will be forward-referenced; gas handles it

    # ---- NMAGIC: ensure function pointer targets have labels ----
    # movw $L_XXXX rewrites create references to labels that may not be in
    # branch_targets (no direct call/jmp to them). Scan and add synthetically.
    if is_nmagic:
        import re as _re_fp
        for _line in out:
            for _m in _re_fp.finditer(r'\$L_([0-9a-f]{4})', _line):
                _addr = int(_m.group(1), 16)
                if _addr not in branch_targets:
                    branch_targets.add(_addr)
                    # Emit label at correct position in output
                    # Find the instruction at this address and insert label before it
                    _lbl = f'L_{_addr:04x}:'
                    for _j, _ol in enumerate(out):
                        if _re_fp.match(rf'^L_{_addr:04x}:', _ol):
                            break  # already present
                    # If not already present, it was inside a function body.
                    # The label needs to be emitted - add to branch_targets so
                    # the next regeneration picks it up. For now just note it.

    # ---- Remove dead register loads before lseek custom block ----
    # The sc==19 special case emits its own loads from stack. The original
    # Venix wrapper loads (mov ax/dx/cx/si from stack) appear before our
    # custom block and are now dead. Remove them for cleanliness.
    _cleaned = []
    _i = 0
    _lines_out = out
    while _i < len(_lines_out):
        _line = _lines_out[_i]
        # Detect the start of our custom lseek block:
        if '-> TLVC patched _lseek (V7 mode)' in _line:
            # Remove the preceding 4 dead load instructions (work backwards):
            _j = len(_cleaned) - 1
            _removed = 0
            while _j >= 0 and _removed < 4:
                _s = _cleaned[_j].strip()
                if '(%bp)' in _s and _s.startswith('mov') and any(_s.endswith(',%'+r) for r in ('ax','bx','cx','dx','si','di')):
                    _cleaned.pop(_j)
                    _removed += 1
                    _j -= 1
                else:
                    break
        _cleaned.append(_line)
        _i += 1
    out = _cleaned

    # ---- Deduplicate consecutive "mov $0xffff,%ax" (double errno return) ----
    # Our errno emit always adds "mov $0xffff,%ax" but some paths already have
    # one from the original Venix jcxz/error pattern. Remove the duplicate.
    deduped = []
    for _line in out:
        if (_line.strip() == 'mov    $0xffff,%ax' and
                deduped and deduped[-1].strip() == 'mov    $0xffff,%ax'):
            continue  # skip duplicate
        deduped.append(_line)
    out = deduped

    # ---- Fix lseek error check: dx (high word) not ax (low word) ----
    # lseek returns a long (dx:ax). Valid positions can have ax sign bit set
    # (any position >= 32KB has the bit set in the low word).
    # Error is indicated by dx=0xffff (all errno values fit in 16 bits).
    # Patch: replace "test %ax,%ax" with "test %dx,%dx" in any lseek wrapper,
    # and add "mov $0xffff,%dx" after the "mov $0xffff,%ax" error return.
    _in_lseek_wrapper = False
    _lseek_patched = []
    for _idx, _line in enumerate(out):
        _s = _line.strip()
        if _s == 'L_0b0c:' or (not _in_lseek_wrapper and
                                '/* Venix syscall lseek(' in _s):
            _in_lseek_wrapper = True
        if _in_lseek_wrapper:
            if _s == 'test\t%ax,%ax':
                _lseek_patched.append(
                    _line.replace('test\t%ax,%ax',
                                  'test\t%dx,%dx') +
                    '\t/* long return: check high word for error */')
                continue
            if (_s.startswith('mov\t$0xffff,%ax') and
                    '/* return -1' in _s):
                _lseek_patched.append(_line)
                _lseek_patched.append(
                    '\tmov\t$0xffff,%dx\t/* lseek long -1 high word */')
                continue
            # End of wrapper when we hit pop %si or pop %bp after the ok label
            if _s in ('pop   %si', 'pop    %si') and not any(
                    'Venix syscall lseek' in _lseek_patched[j]
                    for j in range(max(0, len(_lseek_patched)-3), len(_lseek_patched))):
                _in_lseek_wrapper = False
        _lseek_patched.append(_line)
    out = _lseek_patched

    # ---- Suppress jcxz-derived errno block in lseek wrapper ----
    # sc==19 handles result inline; suppress test/jns/neg/errno block
    # that SyscallSM generates from the Venix jcxz following int $f1.
    _suppressing_lseek_errno = False
    _lseek_final = []
    for _line in out:
        _s = _line.strip()
        if ('kernel handles errno same as standard' in _s or
                'ax=new position low word dx=high word' in _s):
            _suppressing_lseek_errno = True
            _lseek_final.append(_line)
            continue
        if _suppressing_lseek_errno:
            if _s in ('pop    %si', 'pop   %si', 'ret') or (
                    _s.endswith(':') and not _s.startswith('/*')):
                _suppressing_lseek_errno = False
                _lseek_final.append(_line)
            continue
        _lseek_final.append(_line)
    out = _lseek_final


    # ---- Syscall inventory: insert into header after main loop ----
    # Now that the main loop has run, _syscalls_used is populated.
    # Build the inventory and patch it into the header comment.
    if _syscalls_used:
        _sc_sorted = sorted(_syscalls_used)
        _sc_entries = []
        for _sc in _sc_sorted:
            _vn = VENIX_SYSCALL_NAMES.get(_sc, f'?{_sc}')
            _sc_entries.append(f'{_sc}:{_vn}')
        # Build syscall inventory lines:
        _inv_lines = [' * Syscalls used:']
        while _sc_entries:
            row = _sc_entries[:6]; _sc_entries = _sc_entries[6:]
            _inv_lines.append(' *   ' + '  '.join(f'{e:<16}' for e in row).rstrip())
        # Build INT instruction inventory from the output:
        _out_text = '\n'.join(out)
        _int_types = sorted(set(re.findall(r'int\s+(\$0x[0-9a-f]+)', _out_text)))
        if _int_types:
            _inv_lines.append(' * INT instructions: ' + '  '.join(_int_types))
        # Find the placeholder line in out and replace it:
        _placeholder = f' * Syscalls: (see end of header)'
        for _hi, _hl in enumerate(out):
            if _placeholder in _hl:
                out[_hi:_hi+1] = _inv_lines
                break
    else:
        # No syscalls but may still have INT instructions (e.g. FP):
        _out_text = '\n'.join(out)
        _int_types = sorted(set(re.findall(r'int\s+(\$0x[0-9a-f]+)', _out_text)))
        if _int_types:
            _placeholder = f' * Syscalls: (see end of header)'
            for _hi, _hl in enumerate(out):
                if _placeholder in _hl:
                    out[_hi:_hi+1] = [' * INT instructions: ' + '  '.join(_int_types)]
                    break

    # ---- environ init: initialize _environ (D_XXXX) from envp at main entry ----
    # Venix crt0 stores envp in _environ before calling main.
    # TLVC crt0 passes envp as 3rd arg: at main entry 0x8(%bp) = envp.
    # Detect _environ from the crt0 "mov %si,0xNNNN" pattern in vdis header.
    _environ_label = None
    for _cl in lines[:50]:
        _m = re.match(r'^\s+[0-9a-f]+:\s+[0-9a-f ]+\s+mov\s+%si,0x([0-9a-f]+)', _cl)
        if _m:
            _flat = int(_m.group(1), 16)
            # Must be in the data/bss range (not a code address):
            # For NMAGIC: flat addresses in crt0 include the a_stack offset.
            # Use nmagic_flat_base (= a_stack_size) as the base for NMAGIC,
            # data_base_addr (= a_text) for OMAGIC.
            _flat_lo = nmagic_flat_base if is_nmagic else (data_base_addr if data_base_addr else 0)
            _flat_hi = _flat_lo + a_data_size + a_bss_size
            if _flat_lo <= _flat <= _flat_hi and a_data_size > 0:
                _ds_rel = _flat - _flat_lo
                _environ_label = f'D_{_ds_rel:04x}'
                break

    if _environ_label and not keep_crt0:
        # Find main: in out and insert environ init after push %bp / mov %sp,%bp:
        # Suppressed in --keep-crt0 mode: the Venix crt0 already initializes _environ.
        _out_joined = '\n'.join(out)
        _main_pat = re.compile(
            'main:\n\tpush\s+%bp\n\tmov\s+%sp,%bp',
            re.MULTILINE)
        _init = (
            '\n\t/* vrewrite: initialize _environ from envp (TLVC crt0 3rd arg) */'
            '\n\tmov\t0x8(%bp),%ax'
            + '\n\tmov\t%ax,' + _environ_label)
        _out_joined2 = _main_pat.sub(lambda m: m.group(0) + _init, _out_joined, count=1)
        if _out_joined2 != _out_joined:
            out = _out_joined2.split('\n')


    # ---- NMAGIC post-pass: normalize any remaining flat-space D_ labels ----
    # Code operands, errno stores, and synthesized functions may still have
    # flat-space addresses (e.g. D_286c should be D_086c for NMAGIC).
    if nmagic_flat_base and not omagic:
        import re as _re_nf
        flat_lo = nmagic_flat_base
        flat_hi = nmagic_flat_base + a_data_size + a_bss_size
        def _norm_d(m):
            v = int(m.group(1), 16)
            # Only normalize labels that are definitely flat-space residuals:
            # values >= bss_end (outside ds address space) cannot be ds-relative.
            # Values in [flat_lo, bss_end) are ambiguous; skip to avoid
            # double-normalization of labels already emitted as ds-relative.
            bss_end_ds = a_data_size + a_bss_size
            if v >= bss_end_ds and flat_lo <= v < flat_hi:
                return f'D_{v - nmagic_flat_base:04x}'
            return m.group(0)
        out = [_re_nf.sub(r'D_([0-9a-f]{4})', _norm_d, line) for line in out]

    # ---- Synthesize fstat() and stat() with struct translation ----
    # (skipped in v7layer and omagic modes)
    if not v7layer and not omagic:
        out += [
            '',
            '\t/* synthesized: fstat() - translates TLVC stat (32-bit ino) to Venix stat (16-bit ino)',
            '\t   TLVC: st_dev+0 st_ino+2(long) st_mode+6 st_nlink+8 st_uid+10 st_gid+12',
            '\t          st_rdev+14 st_size+16(long) st_atime+20 st_mtime+24 st_ctime+28',
            '\t   Venix: st_dev+0  st_ino+2(short) st_mode+4  st_nlink+6  st_uid+8  st_gid+10',
            '\t          st_rdev+12 st_size+14(long) st_atime+18 st_mtime+22 st_ctime+26 */',
            'fstat:',
            '\tpush\t%bp',
            '\tmov\t%sp,%bp',
            '\tpush\t%si',
            '\tpush\t%di',
            '\tsub\t$0x20,%sp\t\t/* 32-byte TLVC stat buffer on stack */',
            '\t/* call fstat syscall into our TLVC buffer */',
            '\tmov\t0x4(%bp),%bx\t/* fd */',
            '\tlea\t-0x20(%bp),%cx\t/* &tlvc_stat */',
            '\tmov\t$__NR_fstat,%ax',
            '\tint\t$0x80',
            '\ttest\t%ax,%ax',
            '\tjns\tfstat_ok',
            '\tneg\t%ax',
            (f'\tmov\t%ax,{errno_label}\t/* errno */' if errno_label else '\t/* TODO: errno addr not detected */'),
            '\tmov\t$0xffff,%ax',
            '\tjmp\tfstat_ret',
            'fstat_ok:',
            '\t/* translate TLVC stat -> Venix stat in caller buffer */',
            '\tmov\t0x6(%bp),%di\t/* di = venix_stat_buf */',
            '\tlea\t-0x20(%bp),%si\t/* si = tlvc_stat_buf */',
            '\t/* st_dev: TLVC +0 -> Venix +0 (same) */',
            '\tmov\t(%si),%ax',
            '\tmov\t%ax,(%di)',
            '\t/* st_ino: TLVC +2..+5 (long) -> Venix +2 (short, low word only) */',
            '\tmov\t0x2(%si),%ax',
            '\tmov\t%ax,0x2(%di)',
            '\t/* Remaining fields: TLVC offsets +6..+31 -> Venix offsets +4..+29 */',
            '\t/* memcpy(venix+4, tlvc+6, 26): st_mode..st_ctime */',
            '\tlea\t0x4(%di),%di',
            '\tlea\t0x6(%si),%si',
            '\tmov\t$26,%cx',
            '\tcld',
            '\trep\tmovsb',
            '\txor\t%ax,%ax\t/* return 0 */',
            'fstat_ret:',
            '\tadd\t$0x20,%sp',
            '\tpop\t%di',
            '\tpop\t%si',
            '\tpop\t%bp',
            '\tret',
            '',
        ]

        out += [
            '',
            '\t/* synthesized: stat() - translates TLVC stat (32-bit ino) to Venix stat (16-bit ino)',
            '\t   Same struct layout translation as fstat(). */',
            'stat:',
            '\tpush\t%bp',
            '\tmov\t%sp,%bp',
            '\tpush\t%si',
            '\tpush\t%di',
            '\tsub\t$0x20,%sp\t\t/* 32-byte temp buffer for TLVC stat */  ',
            '\tlea\t-0x20(%bp),%ax\t/* &tlvc_stat */',
            '\tpush\t%ax',
            '\tpush\t0x4(%bp)\t\t/* path */',
            '\tmov\t$__NR_stat,%ax',
            '\tint\t$0x80',
            '\tpop\t%cx',
            '\tpop\t%cx',
            '\ttest\t%ax,%ax',
            '\tjs\tstat_err',
            '\t/* translate TLVC stat -> Venix stat in caller buffer */',
            '\tmov\t0x8(%bp),%di\t\t/* Venix stat buf (2nd arg) */',
            '\tlea\t-0x20(%bp),%si\t/* TLVC stat buf */',
            '\t/* st_dev: word at TLVC+0 -> Venix+0 (identical) */',
            '\tmov\t(%si),%ax',
            '\tmov\t%ax,(%di)',
            '\t/* st_ino: TLVC+2..+5 (long) -> Venix+2 (short, low word only) */',
            '\tmov\t0x2(%si),%ax',
            '\tmov\t%ax,0x2(%di)',
            '\t/* remaining fields: TLVC+6..+31 -> Venix+4..+29 (26 bytes) */',
            '\tlea\t0x4(%di),%di',
            '\tlea\t0x6(%si),%si',
            '\tmov\t$13,%cx\t\t/* 13 words = 26 bytes */',
            '\tcld',
            '\trep\tmovsw',
            '\txor\t%ax,%ax\t\t/* return 0 */',
            '\tjmp\tstat_ret',
            'stat_err:',
            '\tneg\t%ax',
            (f'\tmov\t%ax,{errno_label}\t/* errno */' if errno_label else '\t/* TODO: errno addr not detected */'),
       
            '\tmov\t$0xffff,%ax\t/* return -1 */',
            'stat_ret:',
            '\tadd\t$0x20,%sp',
            '\tpop\t%di',
            '\tpop\t%si',
            '\tpop\t%bp',
            '\tret',
            '',
        ]

        # Synthesize _exit only if not already defined by the binary.
        # (The Venix binary may have its own _exit body rewritten by SyscallSM.)
        # Synthesize _exit_raw: the raw kernel exit syscall.
        # The Venix _exit() wrapper (if present) calls __exit → renamed to _exit_raw.
        # This avoids recursion: _exit (flush+exit) → _exit_raw (raw kernel exit).
        # If no Venix _exit wrapper is present (e.g. dd), _exit_raw serves as _exit directly.
        _has_exit_label = any(re.match(r'^_exit:\s*$', l) for l in out)
        _has_exit_raw   = any(re.match(r'^exit_raw:\s*$', l) for l in out)
        if not _has_exit_raw:
            out += [
                '',
                '\t/* synthesized by vrewrite: exit_raw(status) - raw exit syscall */',
                '\t/* Called by Venix _exit() wrapper after flushing stdio. */',
                '\t.global\texit_raw',
                'exit_raw:',
                '\tpush\t%bp',
                '\tmov\t%sp,%bp',
                '\tmov\t$__NR_exit,%ax',
                '\tmov\t0x4(%bp),%bx\t/* arg1: exit status */',
                '\tint\t$0x80',
                '\t/* no return */',
                '',
            ]
        # If no Venix _exit wrapper: also provide _exit as alias for _exit_raw
        # so TLVC crt0 can call _exit and get the raw exit.
        if not _has_exit_label:
            out += [
                '\t/* _exit: alias for _exit_raw when no Venix flush-wrapper is present */',
                '\t.global\t_exit',
                '_exit:',
                '\tjmp\t_exit_raw',
                '',
            ]
        # Ensure _exit is .global if it exists:
        _exit_global_line = '\t.global\t_exit'
        if _exit_global_line not in out:
            _exit_idx = next((i for i,l in enumerate(out) if re.match(r'^_exit:\s*$', l)), None)
            if _exit_idx is not None:
                out.insert(_exit_idx, _exit_global_line)

    # Always ensure _exit is .global (needed in both v7layer and normal mode):
    _exit_global_line = '\t.global\t_exit'
    if _exit_global_line not in out:
        _exit_idx = next((i for i,l in enumerate(out) if re.match(r'^_exit:\s*$', l)), None)
        if _exit_idx is not None:
            out.insert(_exit_idx, _exit_global_line)

    # ---- Emit .set for wrapping branch targets (OMAGIC near-jmp/call wrap) ----
    # In OMAGIC 8086 binaries, near calls/jumps can wrap around the 64KB boundary,
    # producing targets that are mid-instruction or beyond the text segment.
    # These are valid at runtime. Emit them as .set directives so the assembler
    # can compute the correct relative displacements.
    _defined_L  = set(re.findall(r'^L_([0-9a-f]+):', '\n'.join(out), re.MULTILINE))
    _used_L     = set(re.findall(r'\bL_([0-9a-f]+)\b', '\n'.join(out)))
    _missing_L  = _used_L - _defined_L
    if _missing_L:
        _wrap_sets = [f'\t.set L_{a}, 0x{int(a,16):04x}\t/* wrapping branch target */'
                      for a in sorted(_missing_L, key=lambda x: int(x,16))]
        # Insert after the .text directive (near the top of out):
        _text_idx = next((i for i,l in enumerate(out) if l.strip() == '.text'), 0)
        out = (out[:_text_idx+1] +
               ['\t/* Wrapping near-call/jmp targets (16-bit address space) */'] +
               _wrap_sets +
               out[_text_idx+1:])

    # ---- Emit data section ----
    if data_collected:
        out.append('')
        out.append('\t.data')
        out.append('')

        # Flatten to contiguous byte array for reconstruction
        if data_collected:
            base = data_collected[0][0]
            # Build byte array
            end  = max(a + len(bs) for a,bs in data_collected)
            flat = bytearray(end - base)
            for addr, blist in data_collected:
                off = addr - base
                for i, b in enumerate(blist):
                    if off+i < len(flat):
                        flat[off+i] = b

            # Pre-scan flat data for embedded pointer targets.
            # Only needed for OMAGIC (CS=DS flat model) where the linker pre-relocated
            # pointers to flat addresses that must become D_ labels.
            # For NMAGIC: data addresses are already correct DS-relative values;
            # no pre-scan relocation needed.
            if not is_nmagic:  # OMAGIC only
                import struct as _s
                _in_str = bytearray(len(flat))
                _i = 0
                while _i < len(flat):
                    if 0x20 <= flat[_i] < 0x7f or flat[_i] in (9, 10):
                        _j = _i
                        while _j < len(flat) and (0x20 <= flat[_j] < 0x7f
                                                  or flat[_j] in (9, 10)):
                            _j += 1
                        if _j < len(flat) and flat[_j] == 0 and (_j-_i) >= 4:
                            for _k in range(_i, _j+1): _in_str[_k] = 1
                            _i = _j+1; continue
                    _i += 1
                for _i in range(0, len(flat)-1, 2):
                    if _in_str[_i] or _in_str[_i+1]: continue
                    _w, = _s.unpack_from('<H', flat, _i)
                    # OMAGIC: flat-space addresses need DS-relative label targets.
                    # Normalize and add to data_refs if in the data/BSS range.
                    if nmagic_flat_base and nmagic_flat_base <= _w <= data_end_addr:
                        _norm = _w - nmagic_flat_base
                        _in_bss = _norm >= len(flat)
                        if not _in_bss:
                            # Validate .data targets with string-start check:
                            _ok = False
                            if _norm < len(flat):
                                _b = flat[_norm]
                                if 0x20 <= _b < 0x7f:
                                    for _j in range(_norm, min(_norm+40, len(flat))):
                                        if flat[_j] == 0:
                                            _ok = _j > _norm; break
                                        if not (0x20 <= flat[_j] < 0x7f) and flat[_j] not in (9,10,13):
                                            break
                            if not _ok:
                                _i += 2; continue
                        _w = _norm
                        data_refs.add(_w)   # ensure D_xxxx label emitted
                    elif base <= _w <= data_end_addr and _w != 0:
                        data_refs.add(_w)

            # For NMAGIC: code branch targets (L_xxxx) are cs-space addresses.
            # They must NOT appear as labels in the ds-space .data section.
            data_bt = set() if is_nmagic else branch_targets
            data_lines = reconstruct_data(bytes(flat), base,
                                          data_bt, data_refs,
                                          bss_end=data_end_addr,
                                          flat_base=nmagic_flat_base,
                                          is_omagic=not is_nmagic,
                                          text_lo=a_stack_size,
                                          text_hi=a_stack_size + a_text_size)
            out.extend(data_lines)

    # ---- _curbrk rounding: round initial break to paragraph boundary ----
    # TLVC curbrk at startup = (bss_end + 15) & ~15 (nearest 16-byte paragraph).
    # Detect _curbrk as "add D_XXXX,%ax" before brk in the sbrk wrapper.
    _curbrk_v = None
    _joined = '\n'.join(out)
    # Detect _curbrk as 'add D_XXXX,%ax' before a brk call.
    # Works in both normal mode (__NR_brk) and v7layer mode (v7: 17:brk / int $0xf1).
    for _cm in re.finditer(r'\tadd[\t ]+(D_[0-9a-f]{4}),%ax', _joined):
        _la = _joined[_cm.end():_cm.end()+120]
        if '__NR_brk' in _la or 'v7: 17:brk' in _la or '17:brk' in _la:
            _curbrk_v = _cm.group(1)
            break
    if _curbrk_v:
        _bss_ds = a_data_size + a_bss_size
        _rounded = (_bss_ds + 15) & ~15
        # Replace the .word for _curbrk with the rounded numeric value:
        _bss_lbl = f'D_{_bss_ds:04x}'
        # Use regex to handle both cases and any trailing comment:
        _curbrk_pat = re.compile(
            rf'({re.escape(_curbrk_v)}:\n\t\.word\t)({re.escape(_bss_lbl)}|{re.escape(_curbrk_v)})(.*?)$',
            re.MULTILINE)
        # Emit _curbrk as paragraph-rounded bss_end.
        # ia16-elf rounds each segment to 16 bytes, so:
        #   _curbrk = round_para(a_data) + round_para(a_bss)
        # This accounts for linker padding between dseg and bseg.
        # NOTE: gas cannot apply & to section-relative symbols,
        # so we compute the value in Python here.
        _dseg_rounded = (a_data_size + 15) & ~15
        _bseg_rounded = (a_bss_size  + 15) & ~15
        _curbrk_val   = _dseg_rounded + _bseg_rounded
        _new_word = (f'{_curbrk_v}:\n'
                     f'\t.word\t0x{_curbrk_val:04x}'
                     f'\t/* _curbrk: dseg=0x{_dseg_rounded:04x}+bseg=0x{_bseg_rounded:04x}=0x{_curbrk_val:04x} (paragraph-rounded) */')
        _joined2, _n = _curbrk_pat.subn(lambda m: _new_word, _joined, count=1)
        if _n:
            _joined = _joined2
            out = _joined.split('\n')
        else:
            # Also handle raw .byte emission (when is_ptr missed the pointer).
            # The stored value may be either DS-relative or flat (NMAGIC with a_stack>0).
            _flat_bss = _bss_ds + (nmagic_flat_base if nmagic_flat_base else 0)
            for _search_val in sorted(set([_bss_ds, _flat_bss])):
                _lo_b = _search_val & 0xff
                _hi_b = (_search_val >> 8) & 0xff
                _byte_pat = re.compile(
                    rf'({re.escape(_curbrk_v)}:\n\t\.byte\t0x{_lo_b:02x}, 0x{_hi_b:02x}[^\n]*)',
                    re.MULTILINE)
                _joined3, _n2 = _byte_pat.subn(lambda m: _new_word, _joined, count=1)
                if _n2:
                    _joined = _joined3
                    out = _joined.split('\n')
                    break


    # ---- Emit .bss section for undefined D_ labels in BSS range ----
    if True:  # always run BSS emitter; infer extent from undefined refs if a_bss=0
        # BSS address range: use DS-relative for OMAGIC, flat for NMAGIC.
        # OMAGIC: D_XXXX labels are DS-relative; bss starts at DS:a_data_size.
        # NMAGIC: D_XXXX labels are flat; bss starts at flat data_base_addr + a_data_size.
        if not is_nmagic and data_base_addr:  # OMAGIC
            bss_base = a_data_size
            bss_end  = a_data_size + a_bss_size
        else:  # NMAGIC or no data_base
            bss_base = data_base_addr + a_data_size
            bss_end  = bss_base + a_bss_size
        # If a_bss=0 but code references addresses above bss_base,
        # extend bss_end to cover them (binary has implicit BSS):
        if a_bss_size == 0:
            bss_end = 0xffff  # scan all undefined refs regardless of range
        # Find D_ labels referenced in code but not defined in .data
        import re as _re
        out_text = '\n'.join(out)
        defined  = set(_re.findall(r'D_([0-9a-f]{4}):', out_text))
        refs     = set(_re.findall(r'D_([0-9a-f]{4})', out_text))
        bss_undef = sorted(
            int(a, 16) for a in (refs - defined)
            if bss_base <= int(a, 16) <= bss_end
        )
        # Also include bss_end itself if referenced (e.g. as 'endcore' pointer)
        # bss_undef is a set of hex strings from regex; data_refs contains ints
        bss_undef_ints = set(bss_undef)   # already ints from generator above
        # bss_end is already included in bss_undef via the <= range check above.
        # No need for a separate bss_end check here.
        bss_undef_all = sorted(bss_undef_ints)

        if bss_undef_all:
            out += ['', '\t.bss', '']
            prev = bss_base
            for addr in bss_undef_all:
                gap = addr - prev
                if gap > 0:
                    out.append(f'\t.space\t{gap}')
                if addr == bss_end:
                    # End-of-BSS marker: just a label, no space
                    out.append(f'D_{addr:04x}:\t/* endcore: initial break */')
                    prev = addr
                else:
                    out.append(
                        f'D_{addr:04x}:\t.space\t2\t/* bss word @ 0x{addr:04x} */')
                    prev = addr + 2

    out += ['', '/* end of file */']
    return out

# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(
        description='Venix/86 vdis output -> ia16-elf-as .s  (v4)')
    ap.add_argument('--version', action='version', version=f'vrewrite.py v{VERSION}')
    ap.add_argument('input',   help='vdis --tlvc --data output, or - for stdin')
    ap.add_argument('--out', '-o', default=None)
    ap.add_argument('--name', '-n', default=None)
    ap.add_argument('--syms', metavar='BINARY',
                    help='Venix binary with symbol table for label annotation')
    ap.add_argument('--symmap', metavar='FILE',
                    help='Text file with "0xADDR name" lines for label annotation')
    ap.add_argument('--replace-libc', action='store_true',
                    help='Replace libc call wrappers with direct TLVC libc calls')
    ap.add_argument('--compat', action='store_true',
                    help='Full syscall translation mode (legacy): rewrite int $0xf1 '
                         'to int $0x80 with TLVC register conventions. '
                         'Default is V7 shim mode which leaves int $0xf1 intact.')
    ap.add_argument('--keep-crt0', action='store_true',
                    help='Preserve the Venix crt0 as the entry point instead of '
                         'eliding it. Implies no TLVC crt0.o is linked. '
                         'Required when using --omagic.')
    ap.add_argument('--omagic', action='store_true',
                    help='Keep OMAGIC (CS=DS tiny model) memory layout intact. '
                         'Addresses are left as flat values; no data relocation. '
                         'Implies --keep-crt0. The output links without crt0.o '
                         'as a flat tiny-model binary.')
    ap.add_argument('--symdb', default=None,
                    help='Path to venix_symdb.json (for --replace-libc)')
    opts = ap.parse_args()

    if opts.input == '-':
        lines = [l.rstrip('\n') for l in sys.stdin]
        name  = opts.name or 'unknown'
    else:
        with open(opts.input) as f:
            lines = [l.rstrip('\n') for l in f]
        name = opts.name or os.path.splitext(os.path.basename(opts.input))[0]

    # Load symdb for library replacement
    symdb_path = opts.symdb or os.path.join(
        os.path.dirname(os.path.abspath(__file__)), 'venix_symdb.json')
    libc_map = build_libc_map(symdb_path, opts.replace_libc)

    # Load symbol table from binary (if --syms provided)
    sym_text, sym_data = {}, {}
    if opts.syms:
        sym_text, sym_data = load_venix_symbols(opts.syms)
        if sym_text:
            sys.stderr.write(f'; Loaded {len(sym_text)} text + '
                             f'{len(sym_data)} data symbols from {opts.syms}\n')

    # Load text symbol map from file (if --symmap provided)
    # Format: one "0xADDR name" or "ADDR name" per line; # comments ignored
    if getattr(opts, 'symmap', None) and opts.symmap:
        try:
            with open(opts.symmap) as _sf:
                for _line in _sf:
                    _line = _line.strip()
                    if not _line or _line.startswith('#'): continue
                    _parts = _line.split()
                    if len(_parts) >= 2:
                        _addr = int(_parts[0], 16)
                        _name = _parts[1]
                        sym_text[_addr] = _name
            sys.stderr.write(f'; Loaded {len(sym_text)} symbols from {opts.symmap}\n')
        except Exception as _e:
            sys.stderr.write(f'; Warning: could not load symmap {opts.symmap}: {_e}\n')

    result = rewrite(lines, name, replace_libc_map=libc_map,
                     sym_text=sym_text, sym_data=sym_data,
                     v7layer=not opts.compat,
                     keep_crt0=opts.keep_crt0 or opts.omagic,
                     omagic=opts.omagic)
    text   = '\n'.join(result) + '\n'
    n_todo = text.count('TODO')

    if opts.out:
        with open(opts.out, 'w') as f:
            f.write(text)
        print(f"Written: {opts.out}  ({len(result)} lines, {n_todo} TODOs)",
              file=sys.stderr)
    else:
        sys.stdout.write(text)

if __name__ == '__main__':
    main()
