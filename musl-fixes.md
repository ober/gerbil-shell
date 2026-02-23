# Making dlopen Work in musl Static Binaries

## Problem

The gsh static binary is linked with musl libc via `musl-gcc -static`. This
produces a fully static ELF executable with no dynamic linker. When gsh tries
to load compiled `.o1` modules (Gambit shared objects) via `dlopen()`, it gets:

    "Dynamic loading not supported"

This is because musl's `libc.a` contains only weak stubs for `dlopen`/`dlsym`
that unconditionally fail. The real dynamic linker code lives in
`ldso/dynlink.c` and is only linked into `libc.so`, not `libc.a`.

## musl Architecture (source: ~/mine/musl)

### Two-layer stub system

musl uses `weak_alias` to provide stub implementations that the real dynamic
linker overrides:

**`src/ldso/dlopen.c`** (linked into `libc.a`):
```c
static void *stub_dlopen(const char *file, int mode)
{
    __dl_seterr("Dynamic loading not supported");
    return 0;
}
weak_alias(stub_dlopen, dlopen);
```

**`src/ldso/__dlsym.c`** (linked into `libc.a`):
```c
static void *stub_dlsym(void *restrict p, const char *restrict s, void *restrict ra)
{
    __dl_seterr("Symbol not found: %s", s);
    return 0;
}
weak_alias(stub_dlsym, __dlsym);
```

**`src/ldso/dlsym.c`** (thin wrapper):
```c
void *dlsym(void *restrict p, const char *restrict s)
{
    return __dlsym(p, s, 0);
}
```

**`src/ldso/dlclose.c`** (stub — always returns error):
```c
int dlclose(void *p)
{
    return __dl_invalid_handle(p);
}
```

### Build system split (Makefile lines 31-32)

```makefile
LIBC_OBJS = $(filter obj/src/%,$(ALL_OBJS)) $(filter obj/compat/%,$(ALL_OBJS))
LDSO_OBJS = $(filter obj/ldso/%,$(ALL_OBJS:%.o=%.lo))
```

- `LIBC_OBJS` includes `obj/src/ldso/*.o` — the **stubs**
- `LDSO_OBJS` includes `obj/ldso/*.lo` — the **real dynamic linker** (`dynlink.c`)
- `libc.so` = `LOBJS` + `LDSO_OBJS` (stubs overridden by strong symbols)
- `libc.a` = `LIBC_OBJS` only (stubs win, no dynamic linker)

### The real dynamic linker: `ldso/dynlink.c` (2441 lines)

The real `dlopen()` at line 2101 does:
1. **`load_library()`** (line 1055): Opens the .so file, calls `map_library()`
2. **`map_library()`** (line 687): Reads ELF headers, `mmap()`s PT_LOAD segments
3. **`load_deps()`**: Recursively loads DT_NEEDED dependencies
4. **`reloc_all()`** (line 1410): Processes relocations via `do_relocs()` (line 381)
5. **Constructor execution**: Calls `.init` and `.init_array` functions
6. Returns opaque `struct dso *` handle

The real `__dlsym()` at line 2361 does:
1. Validates handle
2. Walks the DSO's symbol hash table (GNU hash or SysV)
3. Returns symbol address

### Internal dependencies of `dynlink.c`

```c
#include "pthread_impl.h"   // thread-local storage, locks
#include "fork_impl.h"      // atfork handlers
#include "libc.h"           // internal libc state
#include "dynlink.h"        // ELF types, relocation constants
```

The dynamic linker is deeply entangled with:
- **Thread-local storage (TLS)**: Manages `PT_TLS` segments, `tls_cnt`, `install_new_tls()`
- **pthreads**: Uses `pthread_rwlock_t`, `pthread_mutex_t`, `pthread_setcancelstate()`
- **Internal malloc**: Uses `__libc_malloc/__libc_calloc/__libc_free`
- **Signal handling**: `__block_all_sigs()/__restore_sigs()` during operations
- **Process state**: Tracks loaded DSOs in a linked list (`head`→`tail`)

## What Gambit .o1 Files Look Like

Gambit `.o1` files are standard ELF shared objects compiled by `gambuild-C`
(which invokes `cc -shared -fPIC`). Inspecting `bench.o1` and `bench~0.o1`:

**Relocation types used** (x86_64):
- `R_X86_64_RELATIVE` — position-independent base adjustment (vast majority)
- `R_X86_64_GLOB_DAT` — GOT entries for external symbols

**Dynamic symbols**: All undefined symbols are `WEAK`:
```
WEAK UND __cxa_finalize
WEAK UND _ITM_registerTMCloneTable
WEAK UND _ITM_deregisterTMCloneTable
WEAK UND __gmon_start__
```

**Exported symbol**: One `GLOBAL` function:
```
GLOBAL DEFAULT ___LNK_bench_2e_o1      (in bench.o1)
GLOBAL DEFAULT ___LNK_bench_7e_30_2e_o1 (in bench~0.o1)
```

**Key insight**: .o1 files have **zero hard dependencies**. They communicate
with the host process entirely through Gambit's internal global variable table
(`##global-var-ref`/`##global-var-set!`), not through C symbol linkage. The
`___LNK_*` function is the module initializer that registers the module's
definitions in Gambit's runtime.

**No DT_NEEDED entries**: The .o1 files don't link against libc or any other
library. All 4 undefined symbols are weak and can be left unresolved.

## Options

### Option A: Link `dynlink.c` into the Static Binary

Compile `ldso/dynlink.c` as a `.o` and add it to the static link. Since
`dynlink.c` defines strong `dlopen`/`__dlsym` symbols, they would override the
weak stubs from `libc.a`.

**Problems**:
- `dynlink.c` has deep dependencies on musl internals (`pthread_impl.h`,
  `fork_impl.h`, `libc.h`) that assume a specific build environment
- The dynamic linker manages global state (`head` DSO list, TLS counters,
  debug state) that conflicts with a static binary's assumptions
- TLS management in particular is problematic — the static binary already has
  its TLS set up by the kernel, and `dynlink.c` would try to manage it
- Would need to build musl from source with a custom configuration
- Fragile — any musl version update could break the integration

**Effort**: High. Would essentially require maintaining a patched musl fork.

### Option B: Custom musl Build with Static+dlopen

Build musl with a configure option or patch that includes `dynlink.c` in
`libc.a`. Some musl forks (like Alpine's) have explored this.

**Problems**:
- Same internal dependency issues as Option A
- No upstream support — musl explicitly does not support `dlopen` in static
  binaries (by design, per Rich Felker's statements)
- Would require users to install a custom musl, breaking the "just use
  `musl-gcc`" simplicity

**Effort**: High, plus ongoing maintenance burden.

### Option C: Minimal Custom dlopen (RECOMMENDED)

Write a minimal `dlopen`/`dlsym` implementation (~400-600 lines of C) that
handles only what Gambit needs. This is practical because .o1 files are
extremely simple ELF shared objects.

**What it needs to handle**:

1. **ELF parsing**: Read `Ehdr`, iterate `Phdr` entries for `PT_LOAD` and
   `PT_DYNAMIC` segments
2. **Memory mapping**: `mmap()` the loadable segments with correct permissions
   (`PROT_READ|PROT_EXEC` for code, `PROT_READ|PROT_WRITE` for data)
3. **Relocation processing** (only 2 types needed for .o1):
   - `R_X86_64_RELATIVE` (type 8): `*reloc = base + addend` — simple base
     adjustment for position-independent references
   - `R_X86_64_GLOB_DAT` (type 6): GOT slot for weak undefined symbols — can
     be left as zero since all are weak
4. **Symbol lookup**: Walk the `.dynsym` table to find `___LNK_*` by name.
   Support both SysV hash (`DT_HASH`) and GNU hash (`DT_GNU_HASH`)
5. **Init functions**: Call `.init` (if `DT_INIT` present) and `.init_array`
   entries
6. **dlclose**: `munmap()` the mapped regions

**What it does NOT need**:
- DT_NEEDED / recursive dependency loading (no dependencies)
- TLS (`PT_TLS`) management (not used by .o1)
- RTLD_LAZY / PLT stubs (use RTLD_NOW)
- `LD_LIBRARY_PATH` search (Gambit passes absolute paths)
- Thread safety beyond what Gambit already provides
- `dladdr`, `dl_iterate_phdr`, or other introspection APIs

**Implementation sketch**:

```c
// gsh-dlopen.c — Minimal dlopen for Gambit .o1 in musl static binaries
#include <elf.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>

struct gsh_dso {
    void *base;        // mmap'd base address
    size_t map_len;    // total mapped length
    Elf64_Sym *syms;   // .dynsym
    char *strings;     // .dynstr
    uint32_t *gnu_hash; // DT_GNU_HASH (or NULL)
    Elf64_Word *sysv_hash; // DT_HASH (or NULL)
    int nsyms;
};

// Override the weak stubs from libc.a
void *dlopen(const char *file, int mode) {
    if (!file) return NULL;  // RTLD_DEFAULT not supported

    int fd = open(file, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return NULL;

    // 1. Read ELF header
    Elf64_Ehdr ehdr;
    if (read(fd, &ehdr, sizeof(ehdr)) != sizeof(ehdr)) goto fail;
    if (memcmp(ehdr.e_ident, ELFMAG, SELFMAG) != 0) goto fail;
    if (ehdr.e_type != ET_DYN) goto fail;

    // 2. Read program headers, find PT_LOAD range
    // 3. mmap with MAP_ANONYMOUS, then mmap individual segments
    // 4. Find PT_DYNAMIC, parse DT_* entries
    // 5. Process relocations (R_X86_64_RELATIVE only needed)
    // 6. Call DT_INIT and DT_INIT_ARRAY
    // 7. Return handle

fail:
    close(fd);
    return NULL;
}

void *dlsym(void *handle, const char *name) {
    struct gsh_dso *dso = handle;
    // Walk .dynsym looking for name match
    // Use GNU hash if available, else linear scan
    for (int i = 0; i < dso->nsyms; i++) {
        if (dso->syms[i].st_shndx != SHN_UNDEF &&
            strcmp(dso->strings + dso->syms[i].st_name, name) == 0)
            return (char *)dso->base + dso->syms[i].st_value;
    }
    return NULL;
}

int dlclose(void *handle) {
    struct gsh_dso *dso = handle;
    // Call DT_FINI and DT_FINI_ARRAY
    munmap(dso->base, dso->map_len);
    free(dso);
    return 0;
}

char *dlerror(void) {
    return "gsh-dlopen: error";  // Minimal
}
```

**Advantages**:
- Self-contained: ~500 lines of C, no musl internal dependencies
- Portable: Only needs standard POSIX + ELF headers
- Sufficient: Handles the exact relocation types Gambit produces
- Testable: Can verify with `readelf -r` that .o1 files stay within bounds
- No musl fork: Ships with gsh, linked as a regular .o file

**Risks**:
- Architecture-specific relocation handling (x86_64 first, aarch64 if needed)
- Must be kept in sync if Gambit's compile-file output changes (unlikely —
  the format hasn't changed in years)
- Does not handle .o1 files that link against external C libraries (e.g.,
  `-lssl`). For those, the full system `dlopen` would be needed

**Effort**: Medium. ~2-3 days for initial implementation + testing.

### Option D: Use Gambit's Built-in `.c` Loading

Instead of `.o1` (shared objects), have Gambit compile to `.c` files and then
use an in-process C compiler (tcc or embedded gcc) to compile and load them.

**Problems**:
- Requires shipping a C compiler in the static binary (huge)
- Much slower than loading pre-compiled .o1
- tcc produces slower code than gcc

**Effort**: High, with worse performance. Not recommended.

## Recommendation

**Option C (minimal custom dlopen)** is the clear winner. The key insight
is that Gambit .o1 files are trivial ELF objects:

- Only `R_X86_64_RELATIVE` relocations (base adjustment) — the most common
  and simplest type
- Only `R_X86_64_GLOB_DAT` for 4 weak symbols that can be left NULL
- No library dependencies, no TLS, no complex symbol resolution
- Single exported symbol (`___LNK_*`) found by name

This means ~90% of `dynlink.c`'s 2441 lines of code are unnecessary for our
use case. A focused implementation handles exactly:

1. ELF header validation
2. `mmap` of PT_LOAD segments
3. RELATIVE relocation patching (one line per reloc: `*addr = base + addend`)
4. Linear symbol table scan for `dlsym`
5. Init function calls
6. Cleanup on `dlclose`

## Implementation Plan

1. **`_vendor/gsh-dlopen/gsh-dlopen.c`**: Core implementation
2. **`_vendor/gsh-dlopen/gsh-dlopen.h`**: Declarations (if needed beyond standard `<dlfcn.h>`)
3. **Makefile changes**: Compile `gsh-dlopen.o` and link into static binary
4. **Build integration**: `gsh-dlopen.o` defines strong `dlopen`/`dlsym`/`dlclose`/`dlerror`
   symbols that override musl's weak stubs in `libc.a`
5. **Testing**: Compile a `.ss` file with `,use`, verify native speed in static binary

The weak alias mechanism makes integration seamless — just link `gsh-dlopen.o`
before `-lc` and the linker picks up the strong definitions.

## Architecture-Specific Notes

### x86_64 (from `arch/x86_64/reloc.h`)

```c
#define REL_SYMBOLIC    R_X86_64_64          // Direct 64-bit
#define REL_OFFSET32    R_X86_64_PC32        // PC-relative 32-bit
#define REL_GOT         R_X86_64_GLOB_DAT    // GOT entry
#define REL_PLT         R_X86_64_JUMP_SLOT   // PLT entry
#define REL_RELATIVE    R_X86_64_RELATIVE    // Base-relative
#define REL_COPY        R_X86_64_COPY        // Symbol copy
```

For .o1 files, only REL_RELATIVE and REL_GOT are observed. REL_GOT targets
are all weak undefined — safe to leave as NULL.

### aarch64 (future)

Would need `R_AARCH64_RELATIVE` and `R_AARCH64_GLOB_DAT` handling. Same
pattern — the relocation types differ but the logic is identical.

## References

- `~/mine/musl/src/ldso/dlopen.c` — weak stub (10 lines)
- `~/mine/musl/src/ldso/__dlsym.c` — weak stub (14 lines)
- `~/mine/musl/src/ldso/dlsym.c` — thin wrapper (7 lines)
- `~/mine/musl/src/ldso/dlclose.c` — stub (7 lines)
- `~/mine/musl/src/ldso/dlerror.c` — error message management (89 lines)
- `~/mine/musl/ldso/dynlink.c` — real dynamic linker (2441 lines)
- `~/mine/musl/src/internal/dynlink.h` — ELF type definitions, reloc constants
- `~/mine/musl/arch/x86_64/reloc.h` — x86_64 relocation type mapping
- `~/mine/musl/Makefile` — LIBC_OBJS vs LDSO_OBJS split (lines 31-32)
