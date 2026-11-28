#!/usr/bin/env python3
"""
vdis.py - Venix/86 2.1 binary disassembler
Target: TLVC (16-bit Linux/ELKS fork) porting aid

Parses Venix a.out headers (executables and .o object files),
disassembles as 8086 AT&T/gas syntax, annotates with:
  - Syscall names (int $f1 with bx=number)
  - Library call names (from venix_symdb.json)
  - TLVC porting notes for each call site
  - Function boundaries (from symbols or heuristics)
  - FP emulation sequences (int $f4/wait/fldcw)
  - Relocation entries (for .o files)
  - Data segment hex dump with symbol labels

Usage:
    python3 vdis.py <binary>                 # disassemble
    python3 vdis.py <binary> --header-only   # header only
    python3 vdis.py <binary> --syms-only     # header + symbols + relocs
    python3 vdis.py <binary> --relocs        # include reloc table in output
    python3 vdis.py <binary> --data          # include data segment dump
    python3 vdis.py <binary> --tlvc          # add TLVC porting annotations
    python3 vdis.py <binary> -o output.s     # write to file
    python3 vdis.py <binary> --symdb path    # use alternate symbol DB

Requires: objdump (GNU binutils, i8086 support)
"""

import sys, os, struct, subprocess, argparse, re, tempfile, json

# ---------------------------------------------------------------------------
# a.out header  (32 bytes, little-endian)
# ---------------------------------------------------------------------------
EXEC_FMT  = '<hH7i'
EXEC_SIZE = struct.calcsize(EXEC_FMT)  # 32

OMAGIC = 0x0107   # object file / old impure
NMAGIC = 0x0109   # separate I&D linked executable

MAGIC_NAMES = {
    OMAGIC: 'OMAGIC (object/impure)',
    NMAGIC: 'NMAGIC (separate I&D executable)',
}


# ---------------------------------------------------------------------------
# ELKS/TLVC a.out header (32 bytes, separate from Venix format)
# ---------------------------------------------------------------------------
ELKS_HDR_FMT  = '<2sBBBBH4I'   # magic(2) flags(1) cpu(1) hdrlen(1) unused(1) version(2) text(4) data(4) bss(4) entry(4) total(4) syms(4) -- but only 5 longs fit
# Actually: 2+1+1+1+1+2 = 8 bytes header, then 6×4 = 24 bytes longs = 32 total
ELKS_HDR_FMT  = '<2sBBBBH6I'   # 2+1+1+1+1+2+24 = 32 ✓
ELKS_MAGIC    = b'\x01\x03'   # ELKS executable magic
ELKS_CPU_8086 = 0x04
ELKS_FL_SEPID = 0x20           # separate I&D flag in a_flags

def parse_elks_header(data):
    if len(data) < 32:
        raise ValueError(f"File too short ({len(data)}b)")
    magic = data[0:2]
    if magic != ELKS_MAGIC:
        return None   # not an ELKS binary
    f = struct.unpack_from(ELKS_HDR_FMT, data, 0)
    # f: magic(bytes), flags, cpu, hdrlen, unused, version, text, data, bss, entry, total, syms
    return dict(
        elks     = True,
        a_magic  = int.from_bytes(f[0], 'little'),
        a_flags  = f[1],
        a_cpu    = f[2],
        a_hdrlen = f[3],
        a_version= f[5],
        a_text   = f[6],
        a_data   = f[7],
        a_bss    = f[8],
        a_entry  = f[9],
        a_total  = f[10],
        a_syms   = f[11],
        # Derived fields matching Venix names for compatibility:
        a_stack  = 0,
        a_trsize = 0,
        a_drsize = 0,
    )

ELKS_CPU_NAMES = {4: 'i8086', 5: 'i80186', 6: 'i286', 11: 'm68k'}

def print_elks_header(hdr, filename):
    flags = hdr['a_flags']
    sepid = bool(flags & ELKS_FL_SEPID)
    cpu   = ELKS_CPU_NAMES.get(hdr['a_cpu'], f"0x{hdr['a_cpu']:02x}")
    print(f"; {'='*62}")
    print(f"; ELKS/TLVC a.out: {os.path.basename(filename)}")
    print(f"; {'='*62}")
    print(f";   Magic:    0x{hdr['a_magic']:04x}  (ELKS executable)")
    print(f";   Flags:    0x{flags:02x}  ({'sep I&D' if sepid else 'combined'})")
    print(f";   CPU:      {cpu}")
    print(f";   Version:  0x{hdr['a_version']:04x}")
    print(f";   a_text:   0x{hdr['a_text']:08x}  ({hdr['a_text']} bytes)")
    print(f";   a_data:   0x{hdr['a_data']:08x}  ({hdr['a_data']} bytes)")
    print(f";   a_bss:    0x{hdr['a_bss']:08x}  ({hdr['a_bss']} bytes)")
    print(f";   a_entry:  0x{hdr['a_entry']:08x}")
    print(f";   a_total:  0x{hdr['a_total']:08x}  ({hdr['a_total']} bytes)")
    print(f";   a_syms:   0x{hdr['a_syms']:08x}  ({hdr['a_syms']} bytes)")
    print(f";")
    hdrlen = hdr['a_hdrlen']
    doff   = hdrlen + hdr['a_text']
    soff   = doff   + hdr['a_data']
    print(f";   File layout:")
    print(f";     [0x{hdrlen:06x}] .text     ({hdr['a_text']} bytes)")
    if hdr['a_data']:
        print(f";     [0x{doff:06x}] .data     ({hdr['a_data']} bytes)")
    if hdr['a_syms']:
        print(f";     [0x{soff:06x}] .syms     ({hdr['a_syms']} bytes)")
    print(f";   .bss:  {hdr['a_bss']} bytes (zero-filled, not in file)")
    if sepid:
        print(f";   Load:  cs:0x0000 (text)   ds:0x0000 (data/bss)  [sep I&D]")
    else:
        print(f";   Load:  cs=ds (combined)")


# ---------------------------------------------------------------------------
# Venix/86 syscall table  (doc/ABI.md)
# int $f1 with syscall number in %bx
# ---------------------------------------------------------------------------
SYSCALLS = {
    0:'indir',   1:'exit',    2:'fork',    3:'read',    4:'write',
    5:'open',    6:'close',   7:'wait',    8:'creat',   9:'link',
    10:'unlink', 11:'exec',   12:'chdir',  13:'time',   14:'mknod',
    15:'chmod',  16:'chown',  17:'brk',    18:'stat',   19:'lseek',
    20:'getpid', 21:'mount',  22:'umount', 23:'setuid', 24:'getuid',
    25:'stime',  26:'ptrace', 27:'alarm',  28:'fstat',  29:'pause',
    30:'utime',  31:'stty',   32:'gtty',   33:'access', 34:'nice',
    35:'ftime',  36:'sync',   37:'kill',   39:'setpgrp',41:'dup',
    42:'pipe',   43:'times',  44:'profil', 45:'sema',   46:'setgid',
    47:'getgid', 48:'signal', 49:'sdata',  50:'suspend',52:'phys',
    53:'lock',   54:'ioctl',  59:'exece',  60:'umask',  61:'chroot',
}

ELKS_SYSCALLS = {
    1: 'exit',      2: 'fork',      3: 'read',      4: 'write',
    5: 'open',      6: 'close',     7: 'waitpid',   8: 'creat',
    9: 'link',     10: 'unlink',   11: 'execve',   12: 'chdir',
    13: 'time',    14: 'mknod',    15: 'chmod',    16: 'chown',
    17: 'brk',     18: 'stat',     19: 'lseek',    20: 'getpid',
    21: 'mount',   22: 'umount',   23: 'setuid',   24: 'getuid',
    25: 'stime',   27: 'alarm',    28: 'fstat',    29: 'pause',
    30: 'utime',   33: 'access',   34: 'nice',     36: 'sync',
    37: 'kill',    41: 'dup',      42: 'pipe',     43: 'times',
    46: 'setgid',  47: 'getgid',   48: 'signal',   54: 'ioctl',
    55: 'fcntl',   57: 'setpgrp',  60: 'umask',    63: 'dup2',
    64: 'getppid', 65: 'getpgrp',  67: 'sigaction',68: 'sgetmask',
    69: 'ssetmask',71: 'setreuid', 72: 'setregid', 75: 'getrlimit',
    76: 'getrusage',77:'gettimeofday',
}

# TLVC syscall number mapping (Venix number -> TLVC __NR_xxx)
# Venix is V7-derived, TLVC is Linux-derived
TLVC_SYSCALL_NR = {
    1:  '__NR_exit',      2:  '__NR_fork',      3:  '__NR_read',
    4:  '__NR_write',     5:  '__NR_open',       6:  '__NR_close',
    7:  '__NR_waitpid',   8:  '__NR_creat',      9:  '__NR_link',
    10: '__NR_unlink',    11: '__NR_execve',     12: '__NR_chdir',
    13: '__NR_time',      14: '__NR_mknod',      15: '__NR_chmod',
    16: '__NR_chown',     17: '__NR_brk',        18: '__NR_stat',
    19: '__NR_lseek',     20: '__NR_getpid',     21: None,
    22: '__NR_umount',    23: '__NR_setuid',     24: '__NR_getuid',
    25: '__NR_stime',     26: '__NR_ptrace',     27: '__NR_alarm',
    28: '__NR_fstat',     29: '__NR_pause',      30: '__NR_utime',
    31: None,             32: None,              33: '__NR_access',
    34: '__NR_nice',      36: '__NR_sync',       37: '__NR_kill',
    41: '__NR_dup',       42: '__NR_pipe',       43: '__NR_times',
    44: '__NR_profil',    46: '__NR_setgid',     47: '__NR_getgid',
    48: '__NR_signal',    54: '__NR_ioctl',      59: '__NR_execve',
    60: '__NR_umask',     61: '__NR_chroot',
    # No TLVC equivalent:
    # 45 sema, 49 sdata, 50 suspend, 52 phys, 53 lock
}

# Venix-specific syscalls with no Linux equivalent
VENIX_ONLY_SYSCALLS = {45, 49, 50, 52, 53}

# ---------------------------------------------------------------------------
# Symbol type constants
# ---------------------------------------------------------------------------
N_UNDF=0x00; N_ABS=0x02; N_TEXT=0x04; N_DATA=0x06; N_BSS=0x08
N_COMM=0x12; N_FN=0x1e;  N_EXT=0x01;  N_TYPE=0x1e; N_STAB=0xe0

SYM_TYPE_NAMES = {
    N_UNDF:'U', N_ABS:'A', N_TEXT:'T', N_DATA:'D',
    N_BSS:'B',  N_COMM:'C', N_FN:'F',
}
STAB_NAMES = {
    0x20:'N_GSYM', 0x22:'N_FNAME',0x24:'N_FUN',  0x26:'N_STSYM',
    0x28:'N_LCSYM',0x40:'N_RSYM', 0x44:'N_SLINE',0x60:'N_SSYM',
    0x64:'N_SO',   0x80:'N_LSYM', 0x84:'N_SOL',  0xa0:'N_PSYM',
    0xa4:'N_ENTRY',0xc0:'N_LBRAC',0xe0:'N_RBRAC',0xe2:'N_BCOMM',
    0xe4:'N_ECOMM',0xe8:'N_ECOML',0xfe:'N_LENG',
}

# struct symtb: uint16 strx, uint8 type, uint8 other, int16 desc, int32 value
SYM_FMT  = '<HBBhi'
SYM_SIZE = struct.calcsize(SYM_FMT)

# struct relocation_info: int32 addr, uint16 symnum, uint16 flags
REL_FMT  = '<iHH'
REL_SIZE = struct.calcsize(REL_FMT)

# ---------------------------------------------------------------------------
# Default symbol DB path (alongside this script)
# ---------------------------------------------------------------------------
DEFAULT_SYMDB = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             'venix_symdb.json')

def load_symdb(path=None):
    p = path or DEFAULT_SYMDB
    if os.path.exists(p):
        return json.load(open(p))
    return {}


# ---------------------------------------------------------------------------
# Header
# ---------------------------------------------------------------------------

def parse_header(data):
    if len(data) < EXEC_SIZE:
        raise ValueError(f"File too short ({len(data)}b)")
    f = struct.unpack_from(EXEC_FMT, data, 0)
    return dict(a_magic=f[0]&0xffff, a_stack=f[1], a_text=f[2], a_data=f[3],
                a_bss=f[4], a_syms=f[5], a_entry=f[6], a_trsize=f[7], a_drsize=f[8])

def file_offsets(hdr):
    t = hdr.get('a_hdrlen', EXEC_SIZE)   # ELKS uses a_hdrlen; Venix uses EXEC_SIZE
    d=t+hdr['a_text']; rt=d+hdr['a_data']
    rd=rt+hdr.get('a_trsize',0); s=rd+hdr.get('a_drsize',0); st=s+hdr['a_syms']
    return dict(text=t,data=d,rel_text=rt,rel_data=rd,syms=s,strings=st)

def is_object_file(hdr):
    return hdr['a_magic']==OMAGIC and (hdr['a_trsize']>0 or hdr['a_syms']>0)

def load_addresses(hdr):
    """Returns (text_base, data_base) ds-relative load addresses.
    For NMAGIC (separate I&D): cs=0, ds=0.
    The data segment is a separate segment starting at ds:0x0000.
    a_stack is the flat-address gap between text and data (for info only).
    For OMAGIC (tiny model): cs=ds, data follows text in same segment.
    """
    if hdr['a_magic'] == NMAGIC:
        return 0x0000, 0x0000   # separate segments: both start at 0
    return 0x0000, hdr['a_text']  # OMAGIC: data_base = text_size

def print_header(hdr, filename):
    magic = hdr['a_magic']
    off   = file_offsets(hdr)
    kind  = 'object file' if is_object_file(hdr) else 'linked executable'
    tb, db = load_addresses(hdr)
    print(f"; {'='*62}")
    print(f"; Venix/86 a.out: {os.path.basename(filename)}  [{kind}]")
    print(f"; {'='*62}")
    print(f";   Magic:    0x{magic:04x}  {MAGIC_NAMES.get(magic,'UNKNOWN')}")
    print(f";   a_stack:  0x{hdr['a_stack']:04x}  ({hdr['a_stack']} bytes)")
    print(f";   a_text:   0x{hdr['a_text']:08x}  ({hdr['a_text']} bytes)")
    print(f";   a_data:   0x{hdr['a_data']:08x}  ({hdr['a_data']} bytes)")
    print(f";   a_bss:    0x{hdr['a_bss']:08x}  ({hdr['a_bss']} bytes)")
    syms_note = " [stripped]" if not hdr['a_syms'] else f", {hdr['a_syms']//SYM_SIZE} entries"
    print(f";   a_syms:   0x{hdr['a_syms']:08x}  ({hdr['a_syms']} bytes){syms_note}")
    print(f";   a_entry:  0x{hdr['a_entry']:08x}")
    print(f";   a_trsize: 0x{hdr['a_trsize']:08x}  ({hdr['a_trsize']//REL_SIZE} text relocs)")
    print(f";   a_drsize: 0x{hdr['a_drsize']:08x}  ({hdr['a_drsize']//REL_SIZE} data relocs)")
    print(f";")
    print(f";   File layout:")
    print(f";     [0x{off['text']:06x}] .text     ({hdr['a_text']} bytes)")
    print(f";     [0x{off['data']:06x}] .data     ({hdr['a_data']} bytes)")
    if hdr['a_trsize']:
        print(f";     [0x{off['rel_text']:06x}] .rel.text ({hdr['a_trsize']//REL_SIZE} entries)")
    if hdr['a_drsize']:
        print(f";     [0x{off['rel_data']:06x}] .rel.data ({hdr['a_drsize']//REL_SIZE} entries)")
    if hdr['a_syms']:
        print(f";     [0x{off['syms']:06x}] .symtab   ({hdr['a_syms']//SYM_SIZE} entries)")
        print(f";     [0x{off['strings']:06x}] .strtab")
    print(f";   .bss:  {hdr['a_bss']} bytes (zero-filled by kernel, not in file)")
    print(f";   Load:  cs:0x{tb:04x} (text)   ds:0x{db:04x} (data/bss)")
    print()


# ---------------------------------------------------------------------------
# Symbols
# ---------------------------------------------------------------------------

def parse_symbols(data, hdr):
    if not hdr['a_syms']: return []
    off = file_offsets(hdr)
    sym_off, str_off = off['syms'], off['strings']
    syms = []
    for i in range(hdr['a_syms'] // SYM_SIZE):
        o = sym_off + i * SYM_SIZE
        if o + SYM_SIZE > len(data): break
        strx, ns_type, ns_other, ns_desc, ns_value = struct.unpack_from(SYM_FMT, data, o)
        name = ''
        if strx and str_off + strx < len(data):
            try:
                end = data.index(b'\x00', str_off + strx)
                name = data[str_off+strx:end].decode('ascii', errors='replace')
            except ValueError: name = '?'
        syms.append(dict(name=name, type=ns_type, other=ns_other,
                         desc=ns_desc, value=ns_value&0xffff))
    return syms

def sym_type_str(t):
    if t & N_STAB: return STAB_NAMES.get(t, f'stab(0x{t:02x})')
    base = t & N_TYPE
    s = SYM_TYPE_NAMES.get(base, f'0x{base:02x}')
    return s.lower() if (t & N_EXT) else s

def print_symbols(syms):
    if not syms: print("; No symbols (stripped)\n"); return
    print(f"; Symbol table ({len(syms)} entries):")
    print(f";   {'Value':6}  {'Type':8}  Name")
    for s in syms:
        print(f";   0x{s['value']:04x}   {sym_type_str(s['type']):8}  {s['name']}")
    print()

def build_symbol_maps(syms):
    """Returns text_syms {addr->name} and all_syms {addr->name}."""
    text_syms, all_syms = {}, {}
    for s in syms:
        if not s['name']: continue
        all_syms[s['value']] = s['name']
        if (s['type'] & N_TYPE) == N_TEXT:
            text_syms[s['value']] = s['name']
    return text_syms, all_syms


# ---------------------------------------------------------------------------
# Relocations
# ---------------------------------------------------------------------------

def parse_relocations(data, hdr, segment='text'):
    off = file_offsets(hdr)
    rel_off  = off['rel_text'] if segment=='text' else off['rel_data']
    rel_size = hdr['a_trsize'] if segment=='text' else hdr['a_drsize']
    relocs = []
    for i in range(rel_size // REL_SIZE):
        o = rel_off + i * REL_SIZE
        if o + REL_SIZE > len(data): break
        r_addr, r_sym, r_flags = struct.unpack_from(REL_FMT, data, o)
        relocs.append(dict(
            address   = r_addr & 0xffff,
            symbolnum = r_sym,
            pcrel     = (r_flags >> 0) & 1,
            length    = (r_flags >> 1) & 3,
            extern    = (r_flags >> 3) & 1,
        ))
    return relocs

def print_relocations(relocs, syms, label='Text'):
    if not relocs: print(f"; No {label.lower()} relocations\n"); return
    lengths = {0:'byte', 1:'word', 2:'long'}
    segs    = {0:'undef',2:'abs',4:'text',6:'data',8:'bss'}
    print(f"; {label} relocations ({len(relocs)} entries):")
    print(f";   {'Addr':6}  {'Sym#':4}  {'PC':2}  {'Len':4}  {'Ext':3}  Name")
    for r in relocs:
        if r['extern'] and r['symbolnum'] < len(syms):
            sym_name = syms[r['symbolnum']]['name']
        elif not r['extern']:
            sym_name = f"<{segs.get(r['symbolnum']&~1, str(r['symbolnum']))}>"
        else:
            sym_name = f"<sym{r['symbolnum']}>"
        print(f";   0x{r['address']:04x}   {r['symbolnum']:4d}  "
              f"{'y' if r['pcrel'] else 'n':2}  "
              f"{lengths.get(r['length'],'?'):4}  "
              f"{'y' if r['extern'] else 'n':3}  {sym_name}")
    print()

def build_reloc_map(relocs, syms):
    """
    Returns dict mapping instruction offset -> symbol name for extern relocs.
    For pcrel call/jmp: the reloc r_address points to the operand word,
    which is at instruction_offset+1. We store both the operand offset
    and instruction offset for robust lookup.
    """
    rmap = {}
    for r in relocs:
        if r['extern'] and r['symbolnum'] < len(syms):
            name = syms[r['symbolnum']]['name']
            addr = r['address']
            rmap[addr]   = name   # operand offset (r_address)
            rmap[addr-1] = name   # instruction offset (for call/jmp e8/e9)
    return rmap


# ---------------------------------------------------------------------------
# Disassembly via objdump
# ---------------------------------------------------------------------------

def detect_objdump():
    import shutil
    candidates = [
        ('gobjdump',            'gnu'),
        ('x86_64-elf-objdump',  'gnu'),
        ('i686-elf-objdump',    'gnu'),
        ('ia16-elf-objdump',    'gnu'),
        ('objdump',             'gnu'),
        ('llvm-objdump',        'llvm'),
        ('llvm-objdump-17',     'llvm'),
        ('llvm-objdump-16',     'llvm'),
        ('llvm-objdump-15',     'llvm'),
    ]
    for name, flavor in candidates:
        path = shutil.which(name)
        if not path:
            continue
        try:
            r = subprocess.run([path, '--version'],
                               capture_output=True, text=True, timeout=5)
            if r.returncode != 0:
                continue
            out = r.stdout + r.stderr
            if 'Apple' in out and name == 'objdump':
                continue
            if 'GNU' in out:
                return path, 'gnu'
            elif 'LLVM' in out or flavor == 'llvm':
                return path, 'llvm'
            else:
                return path, flavor
        except Exception:
            continue
    raise RuntimeError(
        "No suitable objdump found.\n"
        "macOS: brew install binutils   (provides gobjdump)\n"
        "       brew install llvm       (provides llvm-objdump)\n"
        "Linux: apt install binutils"
    )


_OBJDUMP_CACHE = None

def get_objdump():
    global _OBJDUMP_CACHE
    if _OBJDUMP_CACHE is None:
        _OBJDUMP_CACHE = detect_objdump()
        path, flavor = _OBJDUMP_CACHE
        print(f"; objdump: {path} ({flavor})", file=sys.stderr)
    return _OBJDUMP_CACHE


def patch_padding(code_bytes):
    """Replace linker alignment 0x00 padding bytes with NOP (0x90).
    After ret/int $f1, linker inserts 0x00 to align next function to
    an even address. These confuse the disassembler.
    """
    code = bytearray(code_bytes)
    count = 0
    i = 0
    while i < len(code) - 1:
        b = code[i]
        # ret (c3), retf (cb), iret (cf) followed by 0x00
        if b in (0xc3, 0xcb, 0xcf) and code[i+1] == 0x00:
            code[i+1] = 0x90
            count += 1
            i += 2
            continue
        # int $f1 (cd f1) followed by 0x00
        if b == 0xcd and i+2 < len(code) and code[i+1] == 0xf1 and code[i+2] == 0x00:
            code[i+2] = 0x90
            count += 1
            i += 3
            continue
        # near jmp (eb xx) followed by 0x00 — tail-call function ending
        if b == 0xeb and i+2 < len(code) and code[i+2] == 0x00:
            code[i+2] = 0x90
            count += 1
            i += 3
            continue
        # near jmp (e9 lo hi) followed by 0x00 — tail-call function ending
        if b == 0xe9 and i+3 < len(code) and code[i+3] == 0x00:
            code[i+3] = 0x90
            count += 1
            i += 4
            continue
        i += 1
    return bytes(code), count


def raw_disassemble(code_bytes, base_address=0):
    path, flavor = get_objdump()

    # Fix linker alignment padding before disassembling
    patched, n = patch_padding(code_bytes)
    if n:
        print(f"; Patched {n} alignment padding byte(s) -> NOP", file=sys.stderr)

    with tempfile.NamedTemporaryFile(suffix='.bin', delete=False) as f:
        f.write(patched)
        tmpfile = f.name
    try:
        if flavor == 'gnu':
            cmd = [path, '-b', 'binary', '-m', 'i386', '-D',
                   '--disassembler-options=i8086',
                   f'--adjust-vma=0x{base_address:x}', tmpfile]
        else:
            cmd = [path, '--disassemble-all', '--arch-name=x86',
                   '--mcpu=i8086',
                   f'--start-address=0x{base_address:x}', tmpfile]
        result = subprocess.run(cmd, capture_output=True, text=True)
        if result.returncode != 0:
            raise RuntimeError(f"objdump ({flavor}) failed:\n{result.stderr[:500]}")
        return parse_objdump(result.stdout, flavor=flavor)
    finally:
        os.unlink(tmpfile)


def parse_objdump(text, flavor='gnu'):
    instructions = []
    if flavor == 'gnu':
        pat = re.compile(r'^\s*([0-9a-f]+):\s+((?:[0-9a-f]{2}\s)+)\s*(.*)')
    else:
        pat = re.compile(r'^\s*([0-9a-f]+):\s+((?:[0-9a-f]{2} ?)+?)\s{2,}(.*)')
    for line in text.splitlines():
        m = pat.match(line)
        if m:
            asm = m.group(3).strip()
            if flavor == 'llvm':
                asm = re.sub(r'\bintw\b', 'int', asm)
                asm = re.sub(r'\t+', ' ', asm).strip()
            instructions.append({
                'offset': int(m.group(1), 16),
                'hex':    m.group(2).strip(),
                'asm':    asm,
            })
    return instructions


# ---------------------------------------------------------------------------
# Function boundary detection
# ---------------------------------------------------------------------------

def find_func_starts(insns, text_syms):
    """
    Returns set of offsets that are likely function entry points.
    Priority: symbol table > ret+padding+push_bp heuristic > push_bp/mov_sp_bp.
    """
    starts = set(text_syms.keys())
    for i, insn in enumerate(insns):
        if insn['asm'] in ('ret', 'retw', 'lret', 'retf'):
            j = i + 1
            # skip 0x00 alignment padding bytes
            while j < len(insns) and insns[j]['hex'].strip() == '00':
                j += 1
            if j < len(insns) and 'push %bp' in insns[j]['asm']:
                starts.add(insns[j]['offset'])
    # Also: push %bp / mov %sp,%bp as standalone heuristic
    for i in range(len(insns) - 1):
        if ('push %bp' in insns[i]['asm'] and
                'mov %sp,%bp' in insns[i+1]['asm']):
            starts.add(insns[i]['offset'])
    return starts


# ---------------------------------------------------------------------------
# Annotator
# ---------------------------------------------------------------------------

class Annotator:
    def __init__(self, hdr, syms, relocs, code_bytes, data_bytes,
                 symdb=None, tlvc_mode=False, elks_mode=False):
        self.hdr       = hdr
        self.syms      = syms
        self.relocs    = relocs
        self.is_obj    = is_object_file(hdr)
        self.symdb     = symdb or {}
        self.tlvc      = tlvc_mode
        self.elks      = elks_mode
        self.text_base, _ = load_addresses(hdr)
        self.text_syms, self.all_syms = build_symbol_maps(syms)
        self.reloc_map = build_reloc_map(relocs, syms)
        self.pending_syscall = None
        self.in_fp    = False
        self.cur_fn   = None

    def _lookup_call(self, insn_offset, target_addr):
        """
        Resolve a call target to a name.
        Priority: reloc map (exact) > symbol table > synthetic label.
        Returns (name, tlvc_note) or (None, None).
        """
        # Check reloc map at both operand and instruction offsets
        name = self.reloc_map.get(insn_offset) or self.reloc_map.get(insn_offset+1)
        if not name:
            name = self.text_syms.get(target_addr) or self.all_syms.get(target_addr)
        tlvc_note = None
        if name and self.tlvc:
            info = self.symdb.get(name)
            if info:
                tn = info.get('tlvc_name')
                note = info.get('tlvc_note','')
                if tn is None:
                    tlvc_note = f"TLVC: NO EQUIVALENT - {note}"
                elif tn != name and tn != 'same':
                    tlvc_note = f"TLVC: rename to {tn} ({note})"
                elif note not in ('direct','likely direct'):
                    tlvc_note = f"TLVC: {note}"
        return name, tlvc_note

    def _syscall_tlvc(self, sc_num):
        """Return TLVC annotation for a syscall number."""
        if not self.tlvc:
            return None
        if sc_num in VENIX_ONLY_SYSCALLS:
            return f"TLVC: NO EQUIVALENT (Venix-specific)"
        tlvc_nr = TLVC_SYSCALL_NR.get(sc_num)
        if tlvc_nr:
            # Note ABI differences
            notes = []
            if sc_num == 48:  # signal
                notes.append("signal handler return: iret->trampoline")
            if sc_num in (31,32):  # stty/gtty
                notes.append("replace with ioctl(TCSETS/TCGETS)")
            if sc_num == 54:  # ioctl
                notes.append("request codes differ")
            if sc_num in (18,28):  # stat/fstat
                notes.append("struct stat may differ")
            if sc_num == 43:  # times
                notes.append("struct tms may differ")
            note = f"TLVC: {tlvc_nr}" + (f" [{', '.join(notes)}]" if notes else "")
            return note
        return f"TLVC: check mapping for syscall {sc_num}"

    def _inject_crt0_labels(self, insns):
        """
        For stripped linked binaries: identify _main and _exit from
        the fixed crt0 structure.

        crt0 structure (offsets 0x00-0x35):
          int $f4 / wait / fldcw     (FP init)
          ... argv scan loop ...
          call TARGET1               <- _main (first call in text)
          add $6,%sp
          push %ax
          call TARGET2               <- _exit (second call)
          pop %cx
          mov $1,%bx / int $f1       <- fallback _exit syscall

        Works for both OMAGIC (flat) and NMAGIC (separate I&D) binaries.
        """
        call_pat = re.compile(r'call\s+0x([0-9a-f]+)')
        calls_in_crt0 = []

        # crt0 is always < 0x100 bytes; scan just that region
        for insn in insns:
            if insn['offset'] > 0x100:
                break
            m = call_pat.search(insn['asm'])
            if m:
                target = int(m.group(1), 16)
                calls_in_crt0.append((insn['offset'], target))

        # First call -> _main, second call -> _exit
        labels = ['_main', '_exit']
        for i, (call_off, target) in enumerate(calls_in_crt0[:2]):
            name = labels[i]
            if target not in self.text_syms:
                self.text_syms[target] = name
                self.all_syms[target] = name
            if target not in self.reloc_map:
                self.reloc_map[call_off]   = name
                self.reloc_map[call_off+1] = name

    def annotate(self, insns):
        func_starts = find_func_starts(insns, self.text_syms)

        # For stripped binaries: identify _main and _exit from crt0 structure.
        # crt0 always makes exactly two calls before the fallback int $f1:
        #   call _main   (first call in text segment)
        #   call _exit   (second call, immediately after add $6,%sp / push %ax)
        # We inject these into text_syms if not already there.
        if not self.is_obj:
            self._inject_crt0_labels(insns)

        # Synthetic labels for called addresses without symbols
        call_pat = re.compile(r'call\s+0x([0-9a-f]+)')
        called = {}
        for insn in insns:
            m = call_pat.search(insn['asm'])
            if m:
                t = int(m.group(1),16)
                called.setdefault(t,[]).append(insn['offset'])
        synth = {a: f"L_{a:04x}" for a in called
                 if a not in self.text_syms and a not in self.all_syms}

        prev = None
        for insn in insns:
            off = insn['offset']
            asm = insn['asm']
            lines = []

            # --- Labels ---
            if off in self.text_syms:
                self.cur_fn = self.text_syms[off]
                lines.append(f"\n{self.cur_fn}:")
            elif off in synth:
                lines.append(f"\n{synth[off]}:")
            elif off in func_starts:
                lines.append(f"\n; --- function boundary ---")

            # --- crt0 entry ---
            if off == self.text_base and 'int' in asm and '$0xf4' in asm:
                lines.append("; crt0 entry point")

            # --- NOP (alignment padding, patched from 0x00) ---
            if asm == 'nop':
                lines.append(self._f(insn, "; .byte 0x00  (linker alignment, patched to nop)"))
                prev=insn; yield from lines; continue

            # --- FP sequence: int $f4 / wait / fldcw ---
            if 'int' in asm and '$0xf4' in asm:
                self.in_fp = True
                lines.append(self._f(insn, "; FP emulation prefix"))
                prev=insn; yield from lines; continue

            if self.in_fp:
                if asm == 'wait':
                    lines.append(self._f(insn, "; wait for 8087"))
                elif 'fldcw' in asm:
                    lines.append(self._f(insn, "; load FP control word"
                                  + (" [TLVC: remove int $f4/wait prefix, keep fldcw]"
                                     if self.tlvc else "")))
                    self.in_fp = False
                else:
                    lines.append(self._f(insn, "; 8087 instruction"
                                  + (" [TLVC: remove int $f4/wait prefix]"
                                     if self.tlvc else "")))
                    self.in_fp = False
                prev=insn; yield from lines; continue
            self.in_fp = False

            # --- Special interrupts ---
            if 'int' in asm and '$0xf2' in asm:
                lines.append(self._f(insn, "; EMT: stack overflow check"
                              + (" [TLVC: remove - handled by kernel]" if self.tlvc else "")))
                prev=insn; yield from lines; continue

            if 'int' in asm and '$0xf3' in asm:
                lines.append(self._f(insn, "; abort()"
                              + (" [TLVC: replace with call _abort]" if self.tlvc else "")))
                prev=insn; yield from lines; continue

            # --- Syscall number detection ---
            # Venix: mov $N,%bx  →  int $0xf1
            # ELKS:  mov $N,%ax  →  int $0x80  (ax = __NR_xxx)
            if self.elks:
                ax = re.match(r'mov\s+\$0x([0-9a-f]+),%ax', asm)
                if not ax:
                    m2 = re.match(r'mov\s+\$(\d+),%ax', asm)
                    if m2:
                        ax = re.match(r'mov\s+\$0x([0-9a-f]+),%ax',
                                      f'mov $0x{int(m2.group(1)):x},%ax')
                if ax:
                    n = int(ax.group(1), 16)
                    # Only treat as syscall number if it's a known ELKS syscall
                    # (avoids annotating every mov %ax with large data addresses):
                    if n in ELKS_SYSCALLS or n < 128:
                        self.pending_syscall = n
                        if n in ELKS_SYSCALLS:
                            sc_name = ELKS_SYSCALLS[n]
                            lines.append(self._f(insn, f"; __NR_{sc_name} ({n})"))
                            prev=insn; yield from lines; continue
                        # Unknown small number: record silently, comment at int $0x80
                        self.pending_syscall = n
            # --- Venix syscall: mov $N,%bx ---
            bx = re.match(r'mov\s+\$0x([0-9a-f]+),%bx', asm)
            if not bx:
                m2 = re.match(r'mov\s+\$(\d+),%bx', asm)
                if m2:
                    bx = re.match(r'mov\s+\$0x([0-9a-f]+),%bx',
                                  f'mov $0x{int(m2.group(1)):x},%bx')
            if bx:
                n = int(bx.group(1), 16)
                if not self.elks:
                    self.pending_syscall = n  # Venix: %bx holds syscall number
                sc_name = SYSCALLS.get(n, f'?{n}')
                lines.append(self._f(insn, f"; syscall #{n} ({sc_name})"))
                prev=insn; yield from lines; continue

            # --- int $f1: execute syscall ---
            if 'int' in asm and '$0xf1' in asm:
                if self.pending_syscall is not None:
                    sc = self.pending_syscall
                    sc_name = SYSCALLS.get(sc, f'?{sc}')
                    comment = f"; SYSCALL {sc_name}({sc})"
                    tlvc = self._syscall_tlvc(sc)
                    if tlvc:
                        comment += f"  {tlvc}"
                    self.pending_syscall = None
                else:
                    comment = "; SYSCALL (number not statically known)"
                    if self.tlvc:
                        comment += "  [TLVC: rewrite as int $0x80 with ax=__NR_xxx]"
                lines.append(self._f(insn, comment))
                prev=insn; yield from lines; continue
            # --- int $0x80: ELKS/TLVC syscall ---
            elif 'int' in asm and '$0x80' in asm and self.elks:
                sc = self.pending_syscall
                if sc is not None:
                    sc_name = ELKS_SYSCALLS.get(sc, f'?{sc}')
                    comment = f"; ELKS syscall {sc_name}({sc})"
                else:
                    comment = "; ELKS syscall (number unknown)"
                self.pending_syscall = None
                lines.append(self._f(insn, comment))
                prev=insn; yield from lines; continue
            elif 'int' not in asm and not self.elks:
                self.pending_syscall = None

            # --- errno pattern ---
            if asm.startswith('jcxz'):
                comment = "; jcxz: no error if cx==0"
                if self.tlvc:
                    comment += "  [TLVC: replace with jge (negative ax = error)]"
                lines.append(self._f(insn, comment))
                prev=insn; yield from lines; continue

            if (re.match(r'mov\s+%cx,', asm) and
                    prev and 'jcxz' in prev.get('asm','')):
                comment = "; errno = cx"
                if self.tlvc:
                    comment += "  [TLVC: errno = -ax (negate)]"
                lines.append(self._f(insn, comment))
                prev=insn; yield from lines; continue

            # --- Call instructions ---
            cm = re.match(r'call\s+0x([0-9a-f]+)', asm)
            if cm:
                target = int(cm.group(1), 16)
                name, tlvc_note = self._lookup_call(off, target)
                if name:
                    comment = f"; call {name}"
                    if tlvc_note:
                        comment += f"  [{tlvc_note}]"
                elif target in synth:
                    comment = f"; call {synth[target]}"
                else:
                    comment = ""
                lines.append(self._f(insn, comment))
                prev=insn; yield from lines; continue

            # --- Data ref in .o (reloc-annotated) ---
            if self.is_obj and off in self.reloc_map:
                lines.append(self._f(insn, f"; ref {self.reloc_map[off]}"))
                prev=insn; yield from lines; continue

            # --- ret + linker padding ---
            if asm in ('ret','retw','lret','retf'):
                fn = f" (from {self.cur_fn})" if self.cur_fn else ""
                lines.append(self._f(insn, f"; return{fn}"))
                prev=insn; yield from lines; continue

            if prev and prev['asm'] in ('ret','retw','lret','retf') \
                    and insn['hex'].strip() == '00':
                lines.append(self._f(insn, "; .byte 0x00  (linker alignment)"))
                prev=insn; yield from lines; continue

            # --- Common idioms ---
            if re.match(r'xor\s+%ax,%ax', asm):
                lines.append(self._f(insn, "; ax = 0"))
                prev=insn; yield from lines; continue

            if re.match(r'sub\s+\$0x[0-9a-f]+,%sp', asm):
                lines.append(self._f(insn, "; allocate locals"))
                prev=insn; yield from lines; continue

            if re.match(r'add\s+\$0x[0-9a-f]+,%sp', asm):
                c = "; pop args" if (prev and 'call' in prev.get('asm','')) \
                    else "; adjust sp"
                lines.append(self._f(insn, c))
                prev=insn; yield from lines; continue

            if re.match(r'cmp\s+\$0x7f,%sp', asm):
                lines.append(self._f(insn, "; stack guard"
                              + (" [TLVC: remove]" if self.tlvc else "")))
                prev=insn; yield from lines; continue

            lines.append(self._f(insn, ""))
            prev = insn
            yield from lines

    def _f(self, insn, comment):
        line = f"  {insn['offset']:06x}:  {insn['hex']:<22}  {insn['asm']:<38}"
        return (line + comment).rstrip()


# ---------------------------------------------------------------------------
# Data dump
# ---------------------------------------------------------------------------

def dump_data(data_bytes, data_base, all_syms):
    print(f"; ===== DATA SEGMENT =====  ({len(data_bytes)} bytes,"
          f" loads at ds:0x{data_base:04x})")
    print()
    for i in range(0, len(data_bytes), 16):
        addr = data_base + i
        if addr in all_syms:
            print(f"\n{all_syms[addr]}:")
        chunk = data_bytes[i:i+16]
        hp = ' '.join(f'{b:02x}' for b in chunk)
        ap = ''.join(chr(b) if 32<=b<127 else '.' for b in chunk)
        print(f"  {addr:06x}:  {hp:<48}  |{ap}|")
    print()


def dump_elks_data(data_bytes, bss_size, all_syms):
    """xxd-style dump of ELKS data+BSS segments with symbol annotations."""
    print(f"; ===== DATA SEGMENT =====  ({len(data_bytes)} bytes, ds:0x0000)"
          f"  +BSS {bss_size} bytes")
    print()
    addr = 0
    i = 0
    while i < len(data_bytes):
        # Symbol label before this address?
        if addr in all_syms:
            print(f"; {all_syms[addr]}:")
        chunk = data_bytes[i:i+16]
        hp = ' '.join(f'{b:02x}' for b in chunk)
        ap = ''.join(chr(b) if 32<=b<127 else '.' for b in chunk)
        print(f"  {addr:06x}:  {hp:<48}  |{ap}|")
        i    += 16
        addr += 16
    # BSS follows:
    if bss_size:
        print()
        print(f"; ===== BSS SEGMENT =====  ({bss_size} bytes, ds:0x{len(data_bytes):04x})")
        # Show BSS as zeroes with symbol labels only:
        bss_addr = len(data_bytes)
        bss_end  = bss_addr + bss_size
        shown = set()
        for sym_addr in sorted(a for a in all_syms if bss_addr <= a < bss_end):
            if sym_addr not in shown:
                print(f";   ds:0x{sym_addr:04x}  {all_syms[sym_addr]}")
                shown.add(sym_addr)
        if not shown:
            print(f";   (zero-filled, no symbols)")
    print()


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def disassemble(filename, opts):
    raw  = open(filename,'rb').read()
    elks = getattr(opts, 'elks', False)

    # Try ELKS header first when --elks, else fall back to Venix format:
    elks_hdr = parse_elks_header(raw) if elks else None
    if elks_hdr:
        hdr = elks_hdr
    else:
        hdr = parse_header(raw)
        if hdr['a_magic'] not in (OMAGIC, NMAGIC):
            print(f"; WARNING: magic 0x{hdr['a_magic']:04x} unrecognised", file=sys.stderr)

    symdb = load_symdb(opts.symdb if hasattr(opts,'symdb') else None)

    if elks_hdr:
        print_elks_header(hdr, filename)
    else:
        print_header(hdr, filename)
    if opts.header_only: return

    syms    = parse_symbols(raw, hdr)
    relocs  = parse_relocations(raw, hdr, 'text')
    drelocs = parse_relocations(raw, hdr, 'data')

    if opts.syms_only:
        print_symbols(syms)
        print_relocations(relocs,  syms, 'Text')
        print_relocations(drelocs, syms, 'Data')
        return

    if opts.relocs:
        print_symbols(syms)
        print_relocations(relocs,  syms, 'Text')
        print_relocations(drelocs, syms, 'Data')

    off = file_offsets(hdr)
    tb, db = load_addresses(hdr)
    code = raw[off['text'] : off['text'] + hdr['a_text']]
    data = raw[off['data'] : off['data'] + hdr['a_data']]

    tlvc = getattr(opts, 'tlvc', False)
    if tlvc:
        print("; *** TLVC PORTING MODE: annotations show required changes ***")
        print(f"; Symbol DB: {len(symdb)} entries loaded")
        print()
    if elks:
        print("; *** ELKS/TLVC BINARY: int $0x80 syscall annotations ***")
        print()

    print(f"; ===== TEXT SEGMENT =====  ({hdr['a_text']} bytes,"
          f" loads at cs:0x{tb:04x})")
    print()

    insns = raw_disassemble(code, base_address=tb)
    ann   = Annotator(hdr, syms, relocs, code, data,
                      symdb=symdb, tlvc_mode=tlvc, elks_mode=elks)
    for line in ann.annotate(insns):
        print(line)

    if opts.data:
        print()
        _, all_syms = build_symbol_maps(syms)
        if elks_hdr:
            dump_elks_data(data, hdr['a_bss'], all_syms)
        else:
            dump_data(data, db, all_syms)


def main():
    ap = argparse.ArgumentParser(
        description='Venix/86 a.out disassembler — TLVC porting aid')
    ap.add_argument('binary')
    ap.add_argument('--header-only', action='store_true',
                    help='Print header only')
    ap.add_argument('--syms-only',   action='store_true',
                    help='Print header, symbols and relocations')
    ap.add_argument('--relocs',      action='store_true',
                    help='Include relocation table in output')
    ap.add_argument('--data',        action='store_true',
                    help='Include data segment hex dump')
    ap.add_argument('--tlvc',        action='store_true',
                    help='Add TLVC porting annotations to output')
    ap.add_argument('--elks',        action='store_true',
                    help='Disassemble TLVC/ELKS binary (int $0x80 syscalls, no Venix annotations)')
    ap.add_argument('--symdb',       default=None,
                    help='Path to venix_symdb.json (default: alongside vdis.py)')
    ap.add_argument('--out', '-o',   default=None,
                    help='Output file (default: stdout)')
    opts = ap.parse_args()

    if opts.out:
        sys.stdout = open(opts.out, 'w')
    try:
        disassemble(opts.binary, opts)
    except Exception as e:
        import traceback; traceback.print_exc()
        sys.exit(1)
    if opts.out:
        sys.stdout.close()

if __name__ == '__main__':
    main()
