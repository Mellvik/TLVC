# Venix/86 → TLVC Porting Tools

**`vdis.py`** — Venix/86 a.out disassembler and ELKS/TLVC binary inspector  
**`vrewrite.py`** — Assembly post-processor: converts `vdis` output to `ia16-elf-as`-compatible `.s` files

Both tools are used together in a two-stage pipeline. The typical workflow is:

```bash
python3 vdis.py binary --tlvc --data | \
    python3 vrewrite.py - -n name [options] -o name_tlvc.s
```

---

## vdis.py

Disassembles Venix/86 a.out binaries (OMAGIC and NMAGIC formats) and ELKS/TLVC
binaries, using `objdump` (GNU binutils with ia16 support) as the underlying
disassembler. Adds human-readable annotations for syscalls, header fields,
symbol names, and porting hints.

### Synopsis

```
python3 vdis.py BINARY [options]
```

### Options

| Option | Description |
|---|---|
| `--header-only` | Print parsed header and exit |
| `--syms-only` | Print header, symbol table, and relocation table |
| `--relocs` | Include relocation table in disassembly output |
| `--data` | Include data segment after text disassembly |
| `--tlvc` | Add TLVC porting annotations (shows syscall names and changes needed) |
| `--elks` | ELKS/TLVC binary mode: parse minix_exec_hdr, annotate `int $0x80` syscalls |
| `--symdb FILE` | Path to `venix_symdb.json` (default: alongside `vdis.py`) |
| `-o FILE` | Write output to file instead of stdout |

### Binary Formats

#### Venix/86 a.out (default mode)

Standard V7-derived 32-byte a.out header:

| Field | Description |
|---|---|
| `a_magic` | `0x0107` OMAGIC (tiny, CS=DS) or `0x0109` NMAGIC (separate I&D) |
| `a_stack` | Flat-space base of data segment (NMAGIC: usually `0x2000`) |
| `a_text` | Text segment size in bytes |
| `a_data` | Data segment size in bytes |
| `a_bss` | BSS size in bytes |
| `a_syms` | Symbol table size in bytes |
| `a_entry` | Entry point |
| `a_trsize` | Text relocation table size |
| `a_drsize` | Data relocation table size |

**OMAGIC** (`0x0107`): CS=DS tiny model. All addresses are flat offsets from
the start of the combined text+data segment. Data pointers need DS-relative
relocation when porting.

**NMAGIC** (`0x0109`): Separate I&D. Text and data are independent segments,
each starting at `0x0000` in their respective segment register. Data addresses
are already DS-relative.

#### ELKS/TLVC minix_exec_hdr (`--elks` mode)

ELKS and TLVC use a Minix-derived 32-byte header:

```c
struct minix_exec_hdr {
    unsigned long  type;      /* 0x00: magic+cpu+flags (low 16 = 0x0301) */
    unsigned char  hlen;      /* 0x04: header length (usually 32)        */
    unsigned char  reserved1; /* 0x05: reserved                          */
    unsigned short version;   /* 0x06: binary variant version            */
    unsigned long  tseg;      /* 0x08: text segment size in bytes        */
    unsigned long  dseg;      /* 0x0c: data segment size in bytes        */
    unsigned long  bseg;      /* 0x10: BSS segment size in bytes         */
    unsigned long  entry;     /* 0x14: entry point offset                */
    unsigned short chmem;     /* 0x18: extra heap+stack to allocate      */
    unsigned short minstack;  /* 0x1a: minimum stack size                */
    unsigned long  syms;      /* 0x1c: symbol table size in bytes        */
};                            /* total: 32 bytes                         */
```

The `type` field encodes:
- Bits 0-15: magic number (`0x0301` for ELKS executables)
- Bits 16-23: CPU type (`0x04` = i8086)
- Bits 24-31: flags (`0x20` = separate I&D)

In `--elks` mode:
- The minix_exec_hdr is parsed and all fields displayed
- `int $0x80` instructions are annotated with ELKS syscall names
- `mov $N,%ax` before a syscall is annotated as `; __NR_name (N)`
- Venix `int $0xf1` annotations are also shown when present
- Disassembly stops cleanly at `tseg` bytes
- `--data` shows data as xxd-style hex dump followed by BSS summary

### Syscall Annotations

**Venix mode** (`--tlvc`):
```
  000030:  bb 01 00   mov $0x1,%bx         ; syscall #1 (exit)
  000033:  cd f1      int $0xf1            ; SYSCALL exit(1)
```

**ELKS mode** (`--elks`):
```
  000000:  b8 04 00   mov $0x4,%ax         ; __NR_write (4)
  00000c:  cd 80      int $0x80            ; ELKS syscall write(4)
  00000e:  b8 01 00   mov $0x1,%ax         ; __NR_exit (1)
  000013:  cd 80      int $0x80            ; ELKS syscall exit(1)
```

### Examples

```bash
# Show Venix binary header:
python3 vdis.py /venix/bin/dd --header-only

# Full disassembly with porting annotations and data dump:
python3 vdis.py /venix/bin/dd --tlvc --data

# Pipe to vrewrite:
python3 vdis.py /venix/bin/dd --tlvc --data | python3 vrewrite.py - -n dd ...

# Inspect a TLVC compiled binary:
python3 vdis.py ./myprogram --elks --data

# ELKS header only:
python3 vdis.py ./myprogram --elks --header-only

# Save disassembly to file:
python3 vdis.py /venix/bin/awk --tlvc --data -o awk.vdis
```

---

## vrewrite.py

Converts `vdis.py --tlvc --data` output into a `.s` file that assembles
and links with the `ia16-elf` toolchain targeting TLVC.

### Synopsis

```
python3 vrewrite.py INPUT [options] -o OUTPUT.s
```

`INPUT` is a file path or `-` to read from stdin.

### Options

| Option | Description |
|---|---|
| `-n NAME`, `--name NAME` | Binary name used in output header |
| `-o FILE`, `--out FILE` | Output `.s` file path |
| `--syms BINARY` | Load symbol table from the Venix binary for label names |
| `--symmap FILE` | Load symbol map from text file (`0xADDR name` per line) |
| `--compat` | Full syscall translation mode: rewrite `int $0xf1` to `int $0x80` with TLVC register conventions (legacy — see below) |
| `--keep-crt0` | Preserve the Venix crt0 as the binary entry point |
| `--omagic` | OMAGIC tiny-model passthrough: keep flat addresses intact (implies `--keep-crt0`) |
| `--replace-libc` | Experimental: replace Venix libc wrappers with direct TLVC calls |
| `--symdb FILE` | Path to `venix_symdb.json` (for `--replace-libc`) |

### Default Mode: V7 Shim Passthrough

By default, vrewrite leaves Venix syscalls **completely intact** for the
TLVC V7 compatibility shim layer:

```asm
    mov    $4,%bx          /* sc: write */
    mov    %ax,%bx         /* arg setup */
    int    $0xf1           /* v7: 4:write */
    jcxz   L_ok
    mov    %cx,D_4f16      /* errno (v7: cx=error code) */
L_ok:
```

Only a `/* v7: N:name */` comment is added to identify each syscall.
This is the correct mode when the TLVC V7 shim layer is active.

### `--compat` Mode: Full Syscall Translation

Use `--compat` when the V7 shim is **not** available. The Venix syscall
sequence is rewritten to TLVC `int $0x80` conventions:

- `int $0xf1` → `int $0x80` with `__NR_xxx` in `%ax`
- `jcxz TARGET` → `test %ax,%ax; jns TARGET`
- `mov %cx,errno_var` → `neg %ax; mov %ax,errno_var`
- `stat`/`fstat` struct layout translated (Venix 16-bit ino → TLVC 32-bit ino)
- `exit_raw` syscall stub synthesized

### `--keep-crt0`

Preserves the Venix crt0 as the binary entry point instead of eliding it.
The Venix crt0:
- Sets up FP emulation (`int $0xf4`)
- Scans the stack to find `argc`/`argv`/`envp`
- Initializes `_environ`
- Calls `main()`

With `--keep-crt0`, TLVC's `crt0.o` is **not** needed at link time.
`_environ` initialization is suppressed (the Venix crt0 handles it).

### `--omagic`

For OMAGIC (CS=DS tiny model) binaries only. Keeps flat addresses intact
throughout — no data relocation, no D_ label generation in code, no
stat/fstat/exit synthesis. Implies `--keep-crt0`. The output links as a
tiny-model binary without TLVC's `crt0.o`.

### What vrewrite Always Does

Regardless of mode:

- **Label generation**: branch/call targets become `L_XXXX` labels; named
  symbols from `--syms`/`--symmap` replace `L_XXXX` where available
- **Data relocation** (OMAGIC only): flat data addresses in instructions
  converted to DS-relative `D_XXXX` labels
- **Self-referential data pointers**: `_iob_end` stdio limit pointer and
  similar self-referential words are correctly relocated for NMAGIC binaries
- **`_curbrk` initialization**: the initial malloc break is rounded up to
  the nearest 16-byte paragraph boundary to match TLVC's startup convention
- **Syscall inventory**: the header lists every Venix syscall used and all
  `int` instruction types found in the binary

### Generated File Header

```asm
/* dd.s — Venix/86 -> TLVC/ia16-elf
 * Rewritten by vrewrite.py v4
 * Assemble: ia16-elf-as -march=8087 dd.s -o dd.o
 * Syscalls used:
 *   1:exit            3:read            4:write           5:open
 *   6:close           17:brk            19:lseek          41:dup
 * INT instructions: $0xf1  $0xf4
 */
```

### Build Commands

```bash
# Assemble:
ia16-elf-as -march=8087 name_tlvc.s -o name_tlvc.o

# Link with TLVC crt0 (default mode — no --keep-crt0):
ia16-elf-gcc -melks-libc -mcmodel=small -nostdlib \
    /path/to/tlvc/libc/build-ml/elkslibc/crt0.o \
    -o name name_tlvc.o

# Link without TLVC crt0 (--keep-crt0 or --omagic):
ia16-elf-gcc -melks-libc -mcmodel=small -nostdlib \
    -o name name_tlvc.o
```

### Symbol Sources

**`--syms BINARY`**: Parses the Venix a.out symbol table from the original
binary. Use when the binary has not been stripped.

**`--symmap FILE`**: Text file with one entry per line:
```
# Comments begin with #
0x1a4c  main
0x1234  fopen
0x29c0  error
```
Addresses are TEXT segment offsets as shown in `vdis` output.

Both `--syms` and `--symmap` can be combined; `--symmap` takes precedence.

### Data Section Notes

For NMAGIC binaries, words in the data section that are both ≥ `0x0100`
and code-referenced get an annotation comment:

```asm
    .byte   0x10, 0x3e      /* |.>| possible ptr: DS:0x3e10 */
```

These are not automatically relocated — flagged for human review. The
`_iob_end` self-referential pointer is an exception and IS correctly
relocated to `.word D_XXXX`.

---

## Complete Examples

```bash
# Stripped binary (dd), no symbols — default V7 shim mode:
python3 vdis.py /venix/bin/dd --tlvc --data | \
    python3 vrewrite.py - -n dd -o dd_tlvc.s

# Binary with symbol table (uuencode):
python3 vdis.py /venix/bin/uuencode --tlvc --data | \
    python3 vrewrite.py - -n uuencode \
        --syms /venix/bin/uuencode \
        -o uuencode_tlvc.s

# Stripped binary with manual symbol map (awk):
python3 vdis.py /venix/bin/awk --tlvc --data | \
    python3 vrewrite.py - -n awk \
        --symmap awk_symmap.txt \
        -o awk_tlvc.s

# Full translation mode (no V7 shim, --compat):
python3 vdis.py /venix/bin/dd --tlvc --data | \
    python3 vrewrite.py - -n dd --compat -o dd_tlvc.s

# Keep Venix crt0, link without TLVC crt0.o:
python3 vdis.py /venix/bin/uuencode --tlvc --data | \
    python3 vrewrite.py - -n uuencode \
        --syms /venix/bin/uuencode \
        --keep-crt0 \
        -o uuencode_tlvc.s

# OMAGIC tiny-model passthrough:
python3 vdis.py /venix/bin/testprog --tlvc --data | \
    python3 vrewrite.py - -n testprog --omagic -o testprog_tlvc.s

# Inspect TLVC output binary:
python3 vdis.py ./dd --elks --data
```

---

## Typical Session

```bash
# 1. Check the binary header:
python3 vdis.py /venix/bin/foo --header-only

# 2. Check if it has symbols:
python3 vdis.py /venix/bin/foo --syms-only | head -30

# 3. Convert (V7 shim mode is the default):
python3 vdis.py /venix/bin/foo --tlvc --data | \
    python3 vrewrite.py - -n foo --syms /venix/bin/foo -o foo_tlvc.s

# 4. Check TODO count and syscall inventory:
head -15 foo_tlvc.s
grep -c TODO foo_tlvc.s

# 5. Assemble and link:
ia16-elf-as -march=8087 foo_tlvc.s -o foo.o
ia16-elf-gcc -melks-libc -mcmodel=small -nostdlib crt0.o -o foo foo.o

# 6. Debug the result:
python3 vdis.py foo --elks --data
```

---

## Known Limitations

- **Jump tables**: Data words used as jump table entries are annotated with
  `/* possible ptr: DS:0xXXXX */` but not automatically relocated.

- **`exec*` family**: Requires `_environ` initialized before the call.
  vrewrite injects initialization at `main()` entry in normal mode.
  In `--keep-crt0` mode, the Venix crt0 handles this automatically.

- **FP emulation**: Venix uses `int $0xf4` for FP emulation trap setup.
  Passed through in all modes; TLVC handles it.

- **`int $0xf5`** (code mapping): Not currently translated. Binaries using
  this are among the exceptions to binary compatibility.

- **`--replace-libc`**: Experimental. Results vary by binary.
