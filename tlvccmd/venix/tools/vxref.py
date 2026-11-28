#!/usr/bin/env python3
"""
vxref.py - Venix/86 binary cross-reference and function map builder

Analyses a vdis.py disassembly output (.s file) or works directly
on a Venix a.out binary to produce:

  1. Call frequency table (most-called functions first)
  2. Call graph (who calls whom)
  3. Function size estimates
  4. Data reference map (which globals are accessed where)
  5. Dispatch table detection (int $f5/$f6 patterns)
  6. Suggested function names based on patterns

Usage:
    python3 vxref.py <binary>              # analyse binary directly
    python3 vxref.py <binary> --freq       # call frequency table
    python3 vxref.py <binary> --graph      # call graph
    python3 vxref.py <binary> --data       # data reference map
    python3 vxref.py <binary> --dispatch   # dispatch table analysis
    python3 vxref.py <binary> --suggest    # name suggestions
    python3 vxref.py <binary> --all        # everything
    python3 vxref.py <binary> --labels labels.txt  # apply known labels
    python3 vxref.py <binary> --out xref.txt        # write to file

Labels file format (one per line):
    0x1135  basename
    0x8917  finish
    0x0c08  main
    0x67c0  setupterm
"""

import sys, os, re, struct, subprocess, tempfile, shutil, argparse, json
from collections import defaultdict, Counter

# ---------------------------------------------------------------------------
# a.out parsing
# ---------------------------------------------------------------------------
EXEC_FMT  = '<hH7i'
EXEC_SIZE = struct.calcsize(EXEC_FMT)
SYM_FMT   = '<HBBhi'
SYM_SIZE  = struct.calcsize(SYM_FMT)
OMAGIC, NMAGIC = 0x0107, 0x0109
N_TEXT, N_EXT, N_TYPE = 0x04, 0x01, 0x1e

def parse_header(data):
    f = struct.unpack_from(EXEC_FMT, data, 0)
    return dict(a_magic=f[0]&0xffff, a_stack=f[1], a_text=f[2],
                a_data=f[3], a_bss=f[4], a_syms=f[5], a_entry=f[6],
                a_trsize=f[7], a_drsize=f[8])

def parse_symbols(data, hdr):
    if not hdr['a_syms']: return {}
    text_off = EXEC_SIZE
    sym_off  = text_off + hdr['a_text'] + hdr['a_data'] + \
               hdr['a_trsize'] + hdr['a_drsize']
    str_off  = sym_off + hdr['a_syms']
    syms = {}
    for i in range(hdr['a_syms'] // SYM_SIZE):
        o = sym_off + i * SYM_SIZE
        if o + SYM_SIZE > len(data): break
        strx, t, _, _, val = struct.unpack_from(SYM_FMT, data, o)
        name = ''
        if strx and str_off + strx < len(data):
            try:
                end = data.index(b'\x00', str_off + strx)
                name = data[str_off+strx:end].decode('ascii', errors='replace')
            except: pass
        if (t & N_TYPE) == N_TEXT and name:
            syms[val & 0xffff] = name
    return syms

def load_addresses(hdr):
    if hdr['a_magic'] == NMAGIC:
        return 0x0000, hdr['a_stack']
    return 0x0000, hdr['a_text'] + hdr['a_stack']


# ---------------------------------------------------------------------------
# Disassembly
# ---------------------------------------------------------------------------

def patch_padding(code_bytes):
    code = bytearray(code_bytes)
    i = 0
    while i < len(code) - 1:
        b = code[i]
        if b in (0xc3, 0xcb, 0xcf) and code[i+1] == 0x00:
            code[i+1] = 0x90; i += 2; continue
        if b == 0xcd and i+2 < len(code) and code[i+1] == 0xf1 and code[i+2] == 0x00:
            code[i+2] = 0x90; i += 3; continue
        i += 1
    return bytes(code)

def get_objdump():
    for name in ['ia16-elf-objdump','gobjdump','x86_64-elf-objdump','objdump']:
        path = shutil.which(name)
        if not path: continue
        try:
            r = subprocess.run([path,'--version'],capture_output=True,text=True,timeout=5)
            out = r.stdout + r.stderr
            if 'Apple' in out and name == 'objdump': continue
            return path, 'gnu' if 'GNU' in out else 'llvm'
        except: continue
    raise RuntimeError("No suitable objdump found")

def disassemble_raw(code_bytes, base=0):
    path, flavor = get_objdump()
    code = patch_padding(code_bytes)
    with tempfile.NamedTemporaryFile(suffix='.bin', delete=False) as f:
        f.write(code); tmp = f.name
    try:
        if flavor == 'gnu':
            cmd = [path, '-b', 'binary', '-m', 'i386', '-D',
                   '--disassembler-options=i8086',
                   f'--adjust-vma=0x{base:x}', tmp]
        else:
            cmd = [path, '--disassemble-all', '--arch-name=x86',
                   '--mcpu=i8086', f'--start-address=0x{base:x}', tmp]
        r = subprocess.run(cmd, capture_output=True, text=True)
        return r.stdout
    finally:
        os.unlink(tmp)

def parse_disasm(text):
    """Parse objdump output into list of {offset, hex, asm} dicts."""
    pat = re.compile(r'^\s*([0-9a-f]+):\s+((?:[0-9a-f]{2}\s)+)\s*(.*)')
    insns = []
    for line in text.splitlines():
        m = pat.match(line)
        if m:
            asm = re.sub(r'\s{2,};.*$', '', m.group(3)).strip()
            insns.append({
                'offset': int(m.group(1), 16),
                'hex':    m.group(2).strip(),
                'asm':    asm,
            })
    return insns


# ---------------------------------------------------------------------------
# Function boundary detection
# ---------------------------------------------------------------------------

def find_functions(insns, known_syms):
    """
    Returns ordered list of (start_offset, end_offset, name).
    Combines: known symbols + ret+nop+push_bp heuristic + push_bp/mov_sp_bp.
    """
    starts = set(known_syms.keys())

    for i, insn in enumerate(insns):
        if insn['asm'] in ('ret', 'retw', 'lret', 'retf'):
            j = i + 1
            while j < len(insns) and insns[j]['asm'] == 'nop':
                j += 1
            if j < len(insns) and 'push %bp' in insns[j]['asm']:
                starts.add(insns[j]['offset'])

    for i in range(len(insns) - 1):
        if ('push %bp' in insns[i]['asm'] and
                'mov %sp,%bp' in insns[i+1]['asm']):
            starts.add(insns[i]['offset'])

    starts_sorted = sorted(starts)
    all_offsets   = [insn['offset'] for insn in insns]
    max_offset    = all_offsets[-1] if all_offsets else 0

    functions = []
    for i, start in enumerate(starts_sorted):
        end = starts_sorted[i+1] if i+1 < len(starts_sorted) else max_offset
        name = known_syms.get(start, f'sub_{start:04x}')
        functions.append((start, end, name))

    return functions

def func_at(functions, addr):
    """Return name of function containing addr."""
    for start, end, name in functions:
        if start <= addr < end:
            return name
    return f'<0x{addr:04x}>'


# ---------------------------------------------------------------------------
# Dispatch table detection
# ---------------------------------------------------------------------------

def find_dispatch_tables(insns, text_bytes, base):
    """
    Detect Venix-style int $f5/$f6 dispatch tables.
    Pattern: mov $ADDR,%ax / mov $0,%dx / int $f5
    Each entry: [b8 lo hi eb XX] = mov $addr,%ax / jmp hub
    Returns list of {hub_offset, entries: [(table_off, handler_addr)]}
    """
    tables = []
    # Find hubs: ba 00 00 cd f5 sequences
    raw = text_bytes
    i = 0
    hub_offsets = []
    while i < len(raw) - 4:
        if raw[i:i+5] == bytes([0xba,0x00,0x00,0xcd,0xf5]):
            hub_offsets.append(base + i)
        i += 1

    for hub in hub_offsets:
        entries = []
        # Scan for entries that jmp back to this hub
        for insn in insns:
            asm = insn['asm']
            if not re.match(r'mov\s+\$0x[0-9a-f]+,%ax', asm):
                continue
            # Check if next instruction is jmp hub
            off = insn['offset']
            # Find next insn
            idx = next((i for i,x in enumerate(insns) if x['offset']==off), None)
            if idx is None or idx+1 >= len(insns):
                continue
            next_asm = insns[idx+1]['asm']
            if not next_asm.startswith('jmp'):
                continue
            # Calculate jump target
            m = re.search(r'0x([0-9a-f]+)', next_asm)
            if not m: continue
            target = int(m.group(1), 16)
            if target != hub: continue
            # Extract handler address
            m2 = re.search(r'\$0x([0-9a-f]+)', asm)
            if not m2: continue
            handler = int(m2.group(1), 16)
            entries.append((off, handler))

        if entries:
            tables.append({
                'hub': hub,
                'entries': sorted(entries, key=lambda x: x[0]),
            })

    return tables


# ---------------------------------------------------------------------------
# Cross-reference analysis
# ---------------------------------------------------------------------------

def build_xref(insns, functions, labels):
    """
    Build cross-reference tables:
    - calls:      {caller_name -> Counter(callee_name)}
    - callers:    {callee_name -> [caller_name]}
    - call_count: Counter(callee_name)  -- total times called
    - data_refs:  {addr -> [func_name]}  -- data address accesses
    - syscalls:   {func_name -> [syscall_name]}
    """
    calls      = defaultdict(Counter)  # caller -> {callee: count}
    callers    = defaultdict(list)
    call_count = Counter()
    data_refs  = defaultdict(list)
    syscalls   = defaultdict(list)

    VENIX_SYSCALLS = {
        1:'exit',2:'fork',3:'read',4:'write',5:'open',6:'close',
        7:'wait',8:'creat',9:'link',10:'unlink',11:'exec',12:'chdir',
        13:'time',14:'mknod',15:'chmod',16:'chown',17:'brk',18:'stat',
        19:'lseek',20:'getpid',23:'setuid',24:'getuid',27:'alarm',
        28:'fstat',29:'pause',31:'stty',32:'gtty',33:'access',
        36:'sync',37:'kill',41:'dup',42:'pipe',48:'signal',54:'ioctl',
    }

    call_pat    = re.compile(r'call\s+0x([0-9a-f]+)')
    abs_pat     = re.compile(r'(?:mov|cmp|push|lea)\s+.*0x([0-9a-f]{4})\b(?!.*%)')
    bx_pat      = re.compile(r'mov\s+\$0x([0-9a-f]+),%bx')

    pending_sc = None

    for insn in insns:
        off = insn['offset']
        asm = insn['asm']
        cur_func = func_at(functions, off)

        # Syscall tracking
        m = bx_pat.match(asm)
        if m:
            pending_sc = int(m.group(1), 16)
        elif 'int' in asm and '$0xf1' in asm:
            if pending_sc is not None:
                sc_name = VENIX_SYSCALLS.get(pending_sc, f'syscall_{pending_sc}')
                syscalls[cur_func].append(sc_name)
                pending_sc = None
        elif 'int' not in asm:
            pending_sc = None

        # Call instructions
        m = call_pat.search(asm)
        if m and 'call *' not in asm:
            target = int(m.group(1), 16)
            callee = labels.get(target, f'sub_{target:04x}')
            # Find callee in functions list
            for start, end, name in functions:
                if start == target:
                    callee = name
                    break
            calls[cur_func][callee] += 1
            callers[callee].append(cur_func)
            call_count[callee] += 1

        # Data references (absolute addresses in data range)
        # We'll flag any 0xXXXX address that appears in mov/cmp operands
        for m in abs_pat.finditer(asm):
            addr = int(m.group(1), 16)
            data_refs[addr].append(cur_func)

    return dict(
        calls      = dict(calls),
        callers    = dict(callers),
        call_count = call_count,
        data_refs  = data_refs,
        syscalls   = dict(syscalls),
    )


# ---------------------------------------------------------------------------
# Pattern-based name suggestions
# ---------------------------------------------------------------------------

KNOWN_PATTERNS = [
    # (pattern_in_asm_sequence, suggested_name, confidence)
    # These are heuristics based on common V7/ex/vi source patterns

    # String operations
    (r'cmpb\s+\$0x2f',        'contains_slash_scan', 'filename handling'),
    (r'cmpb\s+\$0xa,',        'newline_scan',         'line scanning'),
    (r'cmpb\s+\$0x0,',        'null_scan',            'string length/copy'),

    # Terminal operations
    (r'int\s+\$0xf5',         'terminal_output',      'terminal I/O'),
    (r'int\s+\$0xf6',         'terminal_input',       'terminal I/O'),
    (r'int\s+\$0xf2',         'stack_check',          'stack overflow guard'),

    # Syscall wrappers (identified by syscall number in bx)
    # These are identified dynamically from syscall tracking

    # Common vi patterns
    (r'cmpb\s+\$0x1b,',       'esc_check',            'escape key handling'),
    (r'cmpb\s+\$0xd,',        'cr_check',             'carriage return'),
    (r'mov\s+\$0x76,',        'v_char',               'v key (0x76)'),
    (r'mov\s+\$0x77,',        'w_char',               'w key (0x77)'),
    (r'mov\s+\$0x64,',        'd_char',               'd key (0x64)'),
]

def suggest_names(functions, insns, xref, dispatch_tables, labels):
    """
    Suggest names for unnamed functions based on:
    - Syscall usage
    - Call patterns
    - Instruction patterns
    - Dispatch table position
    - Size (tiny functions are often wrappers)
    """
    suggestions = {}

    # Build per-function instruction list
    func_insns = defaultdict(list)
    for insn in insns:
        fn = func_at(functions, insn['offset'])
        func_insns[fn].append(insn)

    for start, end, name in functions:
        if name in labels.values():
            continue  # already named
        if not name.startswith('sub_'):
            continue  # already has a meaningful name

        fn_insns = func_insns[name]
        fn_asms  = ' '.join(i['asm'] for i in fn_insns)
        size     = end - start
        sc_list  = xref['syscalls'].get(name, [])
        callee_counts = xref['calls'].get(name, {})
        times_called  = xref['call_count'].get(name, 0)

        hints = []

        # Single-syscall wrapper
        if len(sc_list) == 1 and size < 0x40:
            hints.append(f'wrapper_{sc_list[0]}')

        # Tiny function (< 16 bytes) called many times
        if size < 16 and times_called > 5:
            hints.append(f'inline_{times_called}calls')

        # Filename slash scan -> basename-like
        if re.search(r'cmpb\s+\$0x2f', fn_asms) and size < 0x40:
            hints.append('basename_like')

        # Newline scanning + small size -> line length or similar
        if re.search(r'cmpb\s+\$0xa,', fn_asms) and size < 0x60:
            hints.append('line_scan')

        # Signal handling (calls _signal)
        if 'signal' in str(callee_counts):
            hints.append('sets_signals')

        # Large function (> 0x200 bytes) -> likely a main dispatch loop
        if size > 0x200 and times_called <= 2:
            hints.append('large_dispatch_or_main')

        # Called from many places (> 10) -> utility function
        if times_called > 10:
            hints.append(f'utility_called_{times_called}x')

        if hints:
            suggestions[start] = (name, hints, size, times_called, sc_list)

    return suggestions


# ---------------------------------------------------------------------------
# Reporters
# ---------------------------------------------------------------------------

def report_freq(xref, functions, labels, top_n=60):
    print(f"\n{'='*70}")
    print(f"CALL FREQUENCY TABLE  (top {top_n} by total calls received)")
    print(f"{'='*70}")
    print(f"{'Calls':>6}  {'Address':>8}  {'Name':<30}  Syscalls / Notes")
    print(f"{'-'*70}")

    func_map = {start: name for start,end,name in functions}

    for name, count in xref['call_count'].most_common(top_n):
        # Find address
        addr = next((s for s,n in func_map.items() if n==name), None)
        addr_s = f'0x{addr:04x}' if addr is not None else '??????'
        sc = xref['syscalls'].get(name, [])
        sc_s = f'[{",".join(sc)}]' if sc else ''
        print(f"  {count:4d}   {addr_s:>8}  {name:<30}  {sc_s}")


def report_graph(xref, functions, focus=None, depth=2):
    print(f"\n{'='*70}")
    print(f"CALL GRAPH")
    print(f"{'='*70}")

    if focus:
        # Show calls to/from a specific function
        print(f"\nCallers of {focus}:")
        for caller in sorted(set(xref['callers'].get(focus, []))):
            print(f"  {caller}")
        print(f"\n{focus} calls:")
        for callee, cnt in sorted(xref['calls'].get(focus, {}).items(),
                                   key=lambda x: -x[1]):
            print(f"  {cnt:4d}x  {callee}")
    else:
        # Show top-level call structure
        # Functions that are called by few others (likely top-level)
        roots = [(n,c) for n,c in xref['call_count'].items()
                 if xref['call_count'][n] <= 2]
        for name, _ in sorted(roots, key=lambda x: x[0]):
            callees = xref['calls'].get(name, {})
            if callees:
                print(f"\n{name}:")
                for callee, cnt in sorted(callees.items(), key=lambda x: -x[1])[:8]:
                    print(f"  {'':4} {cnt:3d}x  {callee}")


def report_dispatch(tables, labels, functions):
    print(f"\n{'='*70}")
    print(f"DISPATCH TABLE ANALYSIS")
    print(f"{'='*70}")

    func_names = {start: name for start, end, name in functions}

    for t in tables:
        hub = t['hub']
        print(f"\nHub at 0x{hub:04x}  ({len(t['entries'])} entries):")
        print(f"  {'#':>3}  {'Table':>8}  {'Handler':>8}  Name")
        print(f"  {'-'*50}")
        for i, (toff, handler) in enumerate(t['entries']):
            name = labels.get(handler) or func_names.get(handler, f'sub_{handler:04x}')
            print(f"  {i:3d}  0x{toff:04x}    0x{handler:04x}   {name}")


def report_suggest(suggestions, labels):
    print(f"\n{'='*70}")
    print(f"NAME SUGGESTIONS FOR UNNAMED FUNCTIONS")
    print(f"{'='*70}")
    print(f"{'Address':>8}  {'Current':>15}  {'Size':>5}  {'Calls':>5}  Hints")
    print(f"{'-'*70}")
    for addr, (name, hints, size, calls, sc) in sorted(suggestions.items()):
        hint_s = ', '.join(hints[:3])
        sc_s   = f' [{",".join(sc)}]' if sc else ''
        print(f"0x{addr:04x}    {name:>15}  {size:5d}  {calls:5d}  {hint_s}{sc_s}")


def report_data(xref, top_n=40):
    print(f"\n{'='*70}")
    print(f"MOST-REFERENCED DATA ADDRESSES (top {top_n})")
    print(f"{'='*70}")
    print(f"{'Refs':>5}  {'Address':>8}  Functions accessing")
    print(f"{'-'*70}")
    sorted_refs = sorted(xref['data_refs'].items(),
                         key=lambda x: -len(x[1]))
    for addr, funcs in sorted_refs[:top_n]:
        unique_funcs = sorted(set(funcs))
        func_s = ', '.join(unique_funcs[:4])
        if len(unique_funcs) > 4:
            func_s += f' +{len(unique_funcs)-4}'
        print(f"  {len(funcs):3d}   0x{addr:04x}    {func_s}")


def report_syscalls(xref):
    print(f"\n{'='*70}")
    print(f"SYSCALL USAGE BY FUNCTION")
    print(f"{'='*70}")
    for func, scs in sorted(xref['syscalls'].items()):
        print(f"  {func:<40}  {', '.join(scs)}")


def report_sizes(functions, top_n=20):
    print(f"\n{'='*70}")
    print(f"LARGEST FUNCTIONS (top {top_n})")
    print(f"{'='*70}")
    by_size = sorted(functions, key=lambda x: -(x[1]-x[0]))
    for start, end, name in by_size[:top_n]:
        print(f"  0x{start:04x}-0x{end:04x}  {end-start:5d} bytes  {name}")


def report_summary(hdr, functions, xref, tables):
    print(f"\n{'='*70}")
    print(f"SUMMARY")
    print(f"{'='*70}")
    named = sum(1 for _,_,n in functions if not n.startswith('sub_'))
    unnamed = sum(1 for _,_,n in functions if n.startswith('sub_'))
    print(f"  Functions detected:  {len(functions)}")
    print(f"  Named (symbol tbl):  {named}")
    print(f"  Unnamed (heuristic): {unnamed}")
    print(f"  Dispatch tables:     {len(tables)}")
    total_dispatch = sum(len(t['entries']) for t in tables)
    print(f"  Dispatch entries:    {total_dispatch}")
    print(f"  Unique call targets: {len(xref['call_count'])}")
    print(f"  Functions w/syscalls:{len(xref['syscalls'])}")
    total_calls = sum(xref['call_count'].values())
    print(f"  Total call sites:    {total_calls}")

    # Most called
    top3 = xref['call_count'].most_common(3)
    print(f"  Top 3 called:        " +
          ', '.join(f"{n}({c})" for n,c in top3))


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description='Venix/86 binary cross-referencer')
    ap.add_argument('binary',        help='Venix a.out binary')
    ap.add_argument('--freq',        action='store_true', help='Call frequency table')
    ap.add_argument('--graph',       action='store_true', help='Call graph')
    ap.add_argument('--data',        action='store_true', help='Data reference map')
    ap.add_argument('--dispatch',    action='store_true', help='Dispatch table analysis')
    ap.add_argument('--suggest',     action='store_true', help='Name suggestions')
    ap.add_argument('--syscalls',    action='store_true', help='Syscall usage')
    ap.add_argument('--sizes',       action='store_true', help='Function sizes')
    ap.add_argument('--all',         action='store_true', help='All reports')
    ap.add_argument('--focus',       default=None,       help='Focus call graph on function')
    ap.add_argument('--top',         type=int, default=60, help='Top N for freq table')
    ap.add_argument('--labels', '-l', default=None,
                    help='Labels file: "0xADDR name" per line')
    ap.add_argument('--out',  '-o',  default=None,       help='Output file')
    ap.add_argument('--json',        default=None,       help='Save xref data as JSON')
    opts = ap.parse_args()

    if opts.out:
        sys.stdout = open(opts.out, 'w')

    # Load binary
    with open(opts.binary, 'rb') as f:
        data = f.read()

    hdr     = parse_header(data)
    text_b  = data[EXEC_SIZE : EXEC_SIZE + hdr['a_text']]
    tb, _   = load_addresses(hdr)

    # Known labels from symbol table
    labels = parse_symbols(data, hdr)

    # Load user-supplied labels
    if opts.labels:
        with open(opts.labels) as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'): continue
                parts = line.split(None, 1)
                if len(parts) == 2:
                    try:
                        addr = int(parts[0], 16)
                        labels[addr] = parts[1].strip()
                    except: pass
        print(f"; Loaded {len(labels)} labels", file=sys.stderr)

    # Inject crt0 labels if binary starts with standard crt0
    if hdr['a_magic'] in (OMAGIC, NMAGIC) and len(text_b) > 0x40:
        if text_b[0] == 0xcd and text_b[1] == 0xf4:  # int $f4 = standard crt0
            # First call target = _main, second = _exit
            call_pat = re.compile(rb'\xe8(..)') 
            calls_found = []
            for m in call_pat.finditer(text_b[:0x60]):
                rel = struct.unpack_from('<h', m.group(1))[0]
                target = (m.start() + 3 + rel) & 0xffff
                calls_found.append(target)
            if len(calls_found) >= 1 and calls_found[0] not in labels:
                labels[calls_found[0]] = '_main'
            if len(calls_found) >= 2 and calls_found[1] not in labels:
                labels[calls_found[1]] = '_exit'

    print(f"; Disassembling {os.path.basename(opts.binary)} "
          f"({hdr['a_text']} bytes text)...", file=sys.stderr)

    raw_asm = disassemble_raw(text_b, base=tb)
    insns   = parse_disasm(raw_asm)

    print(f"; {len(insns)} instructions parsed", file=sys.stderr)

    functions = find_functions(insns, labels)
    print(f"; {len(functions)} functions identified", file=sys.stderr)

    tables = find_dispatch_tables(insns, text_b, tb)
    print(f"; {len(tables)} dispatch tables found", file=sys.stderr)

    xref = build_xref(insns, functions, labels)
    print(f"; {len(xref['call_count'])} unique call targets", file=sys.stderr)

    # Reports
    do_all = opts.all
    report_summary(hdr, functions, xref, tables)

    if do_all or opts.freq:
        report_freq(xref, functions, labels, top_n=opts.top)

    if do_all or opts.dispatch:
        report_dispatch(tables, labels, functions)

    if do_all or opts.syscalls:
        report_syscalls(xref)

    if do_all or opts.sizes:
        report_sizes(functions)

    if do_all or opts.data:
        report_data(xref)

    if do_all or opts.graph:
        report_graph(xref, functions, focus=opts.focus)

    if do_all or opts.suggest:
        suggestions = suggest_names(functions, insns, xref, tables, labels)
        report_suggest(suggestions, labels)

    # Save JSON for further analysis
    if opts.json:
        # Serialise (Counter etc. -> dict)
        out = {
            'binary':    opts.binary,
            'functions': [(s, e, n) for s,e,n in functions],
            'labels':    {f'0x{k:04x}': v for k,v in labels.items()},
            'call_count': dict(xref['call_count'].most_common()),
            'syscalls':  xref['syscalls'],
            'dispatch':  [{'hub': t['hub'],
                           'entries': t['entries']} for t in tables],
        }
        with open(opts.json, 'w') as jf:
            json.dump(out, jf, indent=2)
        print(f"; JSON saved to {opts.json}", file=sys.stderr)

    if opts.out:
        sys.stdout.close()


if __name__ == '__main__':
    main()
