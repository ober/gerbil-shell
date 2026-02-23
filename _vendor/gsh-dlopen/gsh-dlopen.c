/*
 * gsh-dlopen.c — Minimal ELF loader for Gambit .o1 shared objects
 *
 * Provides strong definitions of dlopen/dlsym/dlclose/dlerror that
 * override musl's weak stubs in static binaries. This enables loading
 * compiled .o1 files without a real dynamic linker.
 *
 * Gambit .o1 files are simple ELF shared objects with only:
 *   - R_X86_64_RELATIVE relocations (position-independent fixups)
 *   - R_X86_64_GLOB_DAT relocations (all weak — can resolve to 0)
 *   - A single exported ___LNK_* symbol
 *   - No dependencies, no TLS, no TEXTREL
 *
 * Link this before libc.a so these strong symbols override the weak stubs.
 */

#define _GNU_SOURCE
#include <elf.h>
#include <fcntl.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

/* ----- Error handling ----- */

static __thread char gsh_dl_errbuf[256];
static __thread int  gsh_dl_errset = 0;

static void dl_set_error(const char *msg) {
    size_t len = strlen(msg);
    if (len >= sizeof(gsh_dl_errbuf))
        len = sizeof(gsh_dl_errbuf) - 1;
    memcpy(gsh_dl_errbuf, msg, len);
    gsh_dl_errbuf[len] = '\0';
    gsh_dl_errset = 1;
}

static void dl_set_error2(const char *a, const char *b) {
    size_t la = strlen(a), lb = strlen(b);
    size_t total = la + lb;
    if (total >= sizeof(gsh_dl_errbuf))
        total = sizeof(gsh_dl_errbuf) - 1;
    if (la > total) la = total;
    lb = total - la;
    memcpy(gsh_dl_errbuf, a, la);
    memcpy(gsh_dl_errbuf + la, b, lb);
    gsh_dl_errbuf[total] = '\0';
    gsh_dl_errset = 1;
}

/* ----- Handle structure ----- */

typedef struct gsh_dl_handle {
    uint8_t      *map_base;   /* mmap base address (for munmap) */
    size_t        map_size;   /* total mmap size */
    uint8_t      *load_bias;  /* bias = map_base - lowest_vaddr */
    Elf64_Sym    *symtab;     /* .dynsym */
    const char   *strtab;     /* .dynstr */
    uint32_t      nsyms;      /* number of symbols */
} gsh_dl_handle;

/* Sentinel for dlopen(NULL, ...) */
static gsh_dl_handle gsh_dl_self_sentinel;

/* ----- Forward declarations ----- */

static uint32_t gnu_hash_nsyms(const uint32_t *gnu_hash);

/* ----- Self-symbol lookup via /proc/self/exe ----- */

/*
 * In a static binary, dlsym(RTLD_DEFAULT, name) must resolve symbols
 * from the main executable.  We parse /proc/self/exe's .symtab on first
 * use and cache it.  This is needed for Gambit's ___CAN_IMPORT_CLIB_DYNAMICALLY
 * which populates a function jump-table at startup via dlsym(NULL, ...).
 */

static Elf64_Sym  *self_symtab  = NULL;
static const char *self_strtab  = NULL;
static uint32_t    self_nsyms   = 0;
static uint8_t    *self_map     = NULL;
static size_t      self_map_sz  = 0;
static int         self_loaded  = 0;

static void load_self_symbols(void) {
    if (self_loaded) return;
    self_loaded = 1;

    int fd = open("/proc/self/exe", O_RDONLY);
    if (fd < 0) return;

    struct stat st;
    if (fstat(fd, &st) < 0) { close(fd); return; }
    size_t sz = (size_t)st.st_size;

    uint8_t *m = (uint8_t *)mmap(NULL, sz, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);
    if (m == MAP_FAILED) return;

    if (sz < sizeof(Elf64_Ehdr)) { munmap(m, sz); return; }
    Elf64_Ehdr *eh = (Elf64_Ehdr *)m;
    if (memcmp(eh->e_ident, ELFMAG, SELFMAG) != 0) { munmap(m, sz); return; }

    /* Walk section headers to find .symtab + .strtab */
    Elf64_Shdr *shdr = (Elf64_Shdr *)(m + eh->e_shoff);
    for (int i = 0; i < eh->e_shnum; i++) {
        if (shdr[i].sh_type == SHT_SYMTAB) {
            self_symtab = (Elf64_Sym *)(m + shdr[i].sh_offset);
            self_nsyms  = (uint32_t)(shdr[i].sh_size / shdr[i].sh_entsize);
            /* .strtab is linked via sh_link */
            uint32_t str_idx = shdr[i].sh_link;
            if (str_idx < eh->e_shnum)
                self_strtab = (const char *)(m + shdr[str_idx].sh_offset);
            break;
        }
    }

    if (!self_symtab || !self_strtab) {
        munmap(m, sz);
        self_symtab = NULL;
        self_strtab = NULL;
        self_nsyms  = 0;
        return;
    }

    /* Keep the mapping alive — symbols reference it */
    self_map    = m;
    self_map_sz = sz;
}

static void *lookup_self_symbol(const char *name) {
    load_self_symbols();
    if (!self_symtab || !self_strtab) return NULL;

    for (uint32_t i = 0; i < self_nsyms; i++) {
        Elf64_Sym *sym = &self_symtab[i];
        if (sym->st_shndx == SHN_UNDEF) continue;
        if (ELF64_ST_BIND(sym->st_info) == STB_LOCAL) continue;
        if (sym->st_name == 0) continue;

        if (strcmp(self_strtab + sym->st_name, name) == 0)
            return (void *)(uintptr_t)sym->st_value;
    }
    return NULL;
}

/* ----- Known symbol resolution for JUMP_SLOT ----- */

/*
 * .o1 files compiled with stack protection reference __stack_chk_fail
 * via a JUMP_SLOT relocation. In a static binary, this symbol exists
 * in libc but isn't in any dynamic symbol table. Provide it here.
 */
extern void __stack_chk_fail(void);

static void *gsh_resolve_known_symbol(const char *name) {
    if (strcmp(name, "__stack_chk_fail") == 0)
        return (void *)__stack_chk_fail;
    /* Fall back to self-symbol lookup */
    return lookup_self_symbol(name);
}

/* ----- Core implementation ----- */

void *dlopen(const char *file, int flags) {
    (void)flags;

    /* dlopen(NULL) → return sentinel (main program handle) */
    if (!file)
        return &gsh_dl_self_sentinel;

    /* Open the .o1 file */
    int fd = open(file, O_RDONLY);
    if (fd < 0) {
        dl_set_error2("cannot open: ", file);
        return NULL;
    }

    struct stat st;
    if (fstat(fd, &st) < 0) {
        close(fd);
        dl_set_error2("cannot stat: ", file);
        return NULL;
    }
    size_t file_size = (size_t)st.st_size;

    /* Map the entire file read-only for parsing */
    uint8_t *file_map = (uint8_t *)mmap(NULL, file_size, PROT_READ,
                                         MAP_PRIVATE, fd, 0);
    close(fd);
    if (file_map == MAP_FAILED) {
        dl_set_error2("cannot mmap: ", file);
        return NULL;
    }

    /* Validate ELF header */
    if (file_size < sizeof(Elf64_Ehdr)) {
        munmap(file_map, file_size);
        dl_set_error("not an ELF file");
        return NULL;
    }

    Elf64_Ehdr *ehdr = (Elf64_Ehdr *)file_map;
    if (memcmp(ehdr->e_ident, ELFMAG, SELFMAG) != 0 ||
        ehdr->e_ident[EI_CLASS] != ELFCLASS64 ||
        ehdr->e_type != ET_DYN ||
        ehdr->e_machine != EM_X86_64) {
        munmap(file_map, file_size);
        dl_set_error("unsupported ELF format");
        return NULL;
    }

    /* Find the extent of all PT_LOAD segments */
    Elf64_Phdr *phdr = (Elf64_Phdr *)(file_map + ehdr->e_phoff);
    uint64_t vaddr_lo = UINT64_MAX, vaddr_hi = 0;

    for (int i = 0; i < ehdr->e_phnum; i++) {
        if (phdr[i].p_type != PT_LOAD) continue;
        uint64_t seg_end = phdr[i].p_vaddr + phdr[i].p_memsz;
        if (phdr[i].p_vaddr < vaddr_lo) vaddr_lo = phdr[i].p_vaddr;
        if (seg_end > vaddr_hi) vaddr_hi = seg_end;
    }

    if (vaddr_lo >= vaddr_hi) {
        munmap(file_map, file_size);
        dl_set_error("no PT_LOAD segments");
        return NULL;
    }

    /* Page-align */
    vaddr_lo &= ~(uint64_t)0xFFF;
    vaddr_hi  = (vaddr_hi + 0xFFF) & ~(uint64_t)0xFFF;
    size_t map_size = (size_t)(vaddr_hi - vaddr_lo);

    /* Reserve address space with PROT_NONE */
    uint8_t *map_base = (uint8_t *)mmap(NULL, map_size, PROT_NONE,
                                         MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (map_base == MAP_FAILED) {
        munmap(file_map, file_size);
        dl_set_error("mmap reserve failed");
        return NULL;
    }

    /* load_bias: address arithmetic so that vaddr V maps to load_bias + V */
    uint8_t *load_bias = map_base - vaddr_lo;

    /* Map each PT_LOAD segment: anonymous RW + memcpy from file_map */
    for (int i = 0; i < ehdr->e_phnum; i++) {
        if (phdr[i].p_type != PT_LOAD) continue;

        uint64_t seg_vaddr  = phdr[i].p_vaddr;
        uint64_t seg_offset = phdr[i].p_offset;
        uint64_t seg_filesz = phdr[i].p_filesz;
        uint64_t seg_memsz  = phdr[i].p_memsz;

        /* Page-align the segment */
        uint64_t page_off       = seg_vaddr & 0xFFF;
        uint64_t aligned_vaddr  = seg_vaddr - page_off;
        uint64_t aligned_offset = seg_offset - page_off;
        uint64_t aligned_filesz = seg_filesz + page_off;
        uint64_t aligned_memsz  = (seg_memsz + page_off + 0xFFF) & ~(uint64_t)0xFFF;

        uint8_t *seg_dest = load_bias + aligned_vaddr;

        void *m = mmap(seg_dest, aligned_memsz, PROT_READ | PROT_WRITE,
                       MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
        if (m == MAP_FAILED) {
            munmap(map_base, map_size);
            munmap(file_map, file_size);
            dl_set_error("segment mmap failed");
            return NULL;
        }

        if (aligned_filesz > 0)
            memcpy(seg_dest, file_map + aligned_offset, aligned_filesz);
    }

    /* Done with file mapping — ehdr is now invalid */
    munmap(file_map, file_size);

    /* ELF header is in the first PT_LOAD segment (vaddr 0), already copied */
    Elf64_Ehdr *lehdr = (Elf64_Ehdr *)load_bias;

    /* Parse DYNAMIC section from the loaded image */
    Elf64_Phdr *lphdr = (Elf64_Phdr *)(load_bias + lehdr->e_phoff);
    Elf64_Dyn  *dynamic    = NULL;
    Elf64_Rela *rela       = NULL;
    size_t      rela_sz    = 0;
    Elf64_Sym  *symtab     = NULL;
    const char *strtab     = NULL;
    uint32_t   *gnu_hash_p = NULL;
    uint64_t    relro_vaddr = 0, relro_memsz = 0;

    for (int i = 0; i < lehdr->e_phnum; i++) {
        if (lphdr[i].p_type == PT_DYNAMIC)
            dynamic = (Elf64_Dyn *)(load_bias + lphdr[i].p_vaddr);
        else if (lphdr[i].p_type == PT_GNU_RELRO) {
            relro_vaddr = lphdr[i].p_vaddr;
            relro_memsz = lphdr[i].p_memsz;
        }
    }

    if (!dynamic) {
        munmap(map_base, map_size);
        dl_set_error("no DYNAMIC segment");
        return NULL;
    }

    /* PLT relocations (separate from .rela.dyn) */
    Elf64_Rela *jmprel    = NULL;
    size_t      jmprel_sz = 0;

    /* Walk DYNAMIC entries */
    for (Elf64_Dyn *d = dynamic; d->d_tag != DT_NULL; d++) {
        switch (d->d_tag) {
        case DT_RELA:         rela = (Elf64_Rela *)(load_bias + d->d_un.d_ptr); break;
        case DT_RELASZ:       rela_sz = d->d_un.d_val; break;
        case DT_JMPREL:       jmprel = (Elf64_Rela *)(load_bias + d->d_un.d_ptr); break;
        case DT_PLTRELSZ:     jmprel_sz = d->d_un.d_val; break;
        case DT_SYMTAB:       symtab = (Elf64_Sym *)(load_bias + d->d_un.d_ptr); break;
        case DT_STRTAB:       strtab = (const char *)(load_bias + d->d_un.d_ptr); break;
        case DT_GNU_HASH:     gnu_hash_p = (uint32_t *)(load_bias + d->d_un.d_ptr); break;
        }
    }

    /* Process .rela.dyn relocations */
    if (rela && rela_sz) {
        size_t nrela = rela_sz / sizeof(Elf64_Rela);
        for (size_t i = 0; i < nrela; i++) {
            uint32_t type = ELF64_R_TYPE(rela[i].r_info);
            uint64_t *target = (uint64_t *)(load_bias + rela[i].r_offset);

            switch (type) {
            case R_X86_64_RELATIVE:
                *target = (uint64_t)(load_bias + rela[i].r_addend);
                break;
            case R_X86_64_GLOB_DAT:
                /* All GLOB_DAT in .o1 files are weak (cxa_finalize, ITM, gmon).
                   Resolve to 0 — they're optional. */
                *target = 0;
                break;
            default:
                break;
            }
        }
    }

    /* Process .rela.plt (JUMP_SLOT) relocations.
       .o1 files typically have only __stack_chk_fail here.
       Resolve known symbols; leave others as 0 (crash-on-call). */
    if (jmprel && jmprel_sz && symtab && strtab) {
        size_t njmp = jmprel_sz / sizeof(Elf64_Rela);
        for (size_t i = 0; i < njmp; i++) {
            uint32_t type = ELF64_R_TYPE(jmprel[i].r_info);
            if (type != R_X86_64_JUMP_SLOT) continue;

            uint64_t *target = (uint64_t *)(load_bias + jmprel[i].r_offset);
            uint32_t sym_idx = ELF64_R_SYM(jmprel[i].r_info);
            const char *name = strtab + symtab[sym_idx].st_name;

            void *addr = gsh_resolve_known_symbol(name);
            *target = addr ? (uint64_t)addr : 0;
        }
    }

    /* Apply RELRO: make the relro region read-only */
    if (relro_memsz > 0) {
        uint64_t relro_start = relro_vaddr & ~(uint64_t)0xFFF;
        uint64_t relro_end   = (relro_vaddr + relro_memsz + 0xFFF) & ~(uint64_t)0xFFF;
        mprotect(load_bias + relro_start, (size_t)(relro_end - relro_start), PROT_READ);
    }

    /* Set final segment permissions */
    for (int i = 0; i < lehdr->e_phnum; i++) {
        if (lphdr[i].p_type != PT_LOAD) continue;

        int prot = 0;
        if (lphdr[i].p_flags & PF_R) prot |= PROT_READ;
        if (lphdr[i].p_flags & PF_W) prot |= PROT_WRITE;
        if (lphdr[i].p_flags & PF_X) prot |= PROT_EXEC;

        uint64_t aligned_vaddr = lphdr[i].p_vaddr & ~(uint64_t)0xFFF;
        uint64_t aligned_end   = (lphdr[i].p_vaddr + lphdr[i].p_memsz + 0xFFF) & ~(uint64_t)0xFFF;
        mprotect(load_bias + aligned_vaddr, (size_t)(aligned_end - aligned_vaddr), prot);
    }

    /* Derive symbol count from GNU hash */
    uint32_t nsyms = 0;
    if (gnu_hash_p && symtab)
        nsyms = gnu_hash_nsyms(gnu_hash_p);

    /* Build handle (mmap to avoid depending on malloc) */
    gsh_dl_handle *h = (gsh_dl_handle *)mmap(NULL, sizeof(gsh_dl_handle),
                                              PROT_READ | PROT_WRITE,
                                              MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (h == MAP_FAILED) {
        munmap(map_base, map_size);
        dl_set_error("handle alloc failed");
        return NULL;
    }

    h->map_base       = map_base;
    h->map_size       = map_size;
    h->load_bias      = load_bias;
    h->symtab         = symtab;
    h->strtab         = strtab;
    h->nsyms          = nsyms;

    /* Skip INIT / INIT_ARRAY / FINI / FINI_ARRAY.
     * Gambit .o1 files don't need GCC runtime constructors (frame_dummy,
     * __register_frame_info_bases, etc.).  These try to call through GOT
     * entries we resolved to 0, causing segfaults.
     * Gambit explicitly calls ___LNK_* via dlsym — no INIT needed. */

    return h;
}

void *dlsym(void *handle, const char *name) {
    if (!handle || !name) {
        dl_set_error("invalid argument");
        return NULL;
    }

    /* Self-handle: look up symbol in the main executable */
    if (handle == &gsh_dl_self_sentinel) {
        void *addr = lookup_self_symbol(name);
        if (!addr)
            dl_set_error2("symbol not found: ", name);
        return addr;
    }

    gsh_dl_handle *h = (gsh_dl_handle *)handle;

    if (!h->symtab || !h->strtab || h->nsyms == 0) {
        dl_set_error("no symbol table");
        return NULL;
    }

    /* Linear scan of .dynsym — .o1 files have ~6 symbols */
    for (uint32_t i = 0; i < h->nsyms; i++) {
        Elf64_Sym *sym = &h->symtab[i];
        if (sym->st_shndx == SHN_UNDEF) continue;
        if (ELF64_ST_BIND(sym->st_info) == STB_LOCAL) continue;

        if (strcmp(h->strtab + sym->st_name, name) == 0) {
            /* sym->st_value is the vaddr; actual address = load_bias + vaddr */
            return (void *)(h->load_bias + sym->st_value);
        }
    }

    dl_set_error2("symbol not found: ", name);
    return NULL;
}

int dlclose(void *handle) {
    if (!handle || handle == &gsh_dl_self_sentinel)
        return 0;

    gsh_dl_handle *h = (gsh_dl_handle *)handle;

    /* Skip FINI / FINI_ARRAY destructors (see INIT comment in dlopen). */

    if (h->map_base && h->map_size)
        munmap(h->map_base, h->map_size);

    munmap(h, sizeof(gsh_dl_handle));
    return 0;
}

char *dlerror(void) {
    if (gsh_dl_errset) {
        gsh_dl_errset = 0;
        return gsh_dl_errbuf;
    }
    return NULL;
}

/* ----- GNU hash: derive symbol count ----- */

/*
 * GNU hash table layout:
 *   uint32_t nbuckets, symoffset, bloom_size, bloom_shift
 *   uint64_t bloom[bloom_size]
 *   uint32_t buckets[nbuckets]
 *   uint32_t chains[]  (one per symbol from symoffset onwards, last has bit 0 set)
 */
static uint32_t gnu_hash_nsyms(const uint32_t *gnu_hash) {
    uint32_t nbuckets   = gnu_hash[0];
    uint32_t symoffset  = gnu_hash[1];
    uint32_t bloom_size = gnu_hash[2];

    const uint64_t *bloom   = (const uint64_t *)(gnu_hash + 4);
    const uint32_t *buckets = (const uint32_t *)(bloom + bloom_size);
    const uint32_t *chains  = buckets + nbuckets;

    /* Find the maximum bucket value */
    uint32_t max_idx = 0;
    for (uint32_t i = 0; i < nbuckets; i++) {
        if (buckets[i] > max_idx)
            max_idx = buckets[i];
    }

    if (max_idx == 0)
        return symoffset;

    /* Walk the chain from max_idx until terminator (bit 0 set) */
    uint32_t idx = max_idx;
    while (!(chains[idx - symoffset] & 1))
        idx++;

    return idx + 1;
}
