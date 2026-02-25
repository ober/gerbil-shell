# gsh — Gerbil Shell

## Build & Test Requirements

After modifying any `.ss` source files, you MUST:

1. **Dynamic build**: `make build` — must succeed with no errors
2. **Dynamic smoke test**: `make compat-smoke` — must pass 18/18
3. **Dynamic ,eval test**: `printf ',(+ 1 2)\n' | .gerbil/bin/gsh` — must output `3`
4. **Static build**: `make static` — must succeed (uses Docker)
5. **Static shell test**: `printf 'echo hello\n' | static/gsh` — must output `hello`
6. **Static ,eval test**: `printf ',(+ 1 2)\n' | static/gsh` — must output `3`
7. **Static ,use test**: `printf ',use bench.ss\n' | static/gsh` — must run benchmarks

Never consider a change complete until both dynamic AND static builds work correctly.
The static binary must be fully self-contained — no `-:~~=` flag, no external Gerbil installation.

## Build Verification: MANDATORY Before Commit or Push

**NEVER commit or push code without verifying that EVERY `make` target builds successfully AND that all resulting binaries run without error.** If ANY step fails, fix it before committing. No exceptions.

### Full verification checklist (run in order)

1. **`make build`** — dynamic build must complete without errors
2. **`make compat-smoke`** — must pass 18/18
3. **Dynamic `,eval` test**: `printf ',(+ 1 2)\n' | .gerbil/bin/gsh` — must output `3`
4. **`make static`** — static Docker build must complete without errors
5. **Static shell test**: `printf 'echo hello\n' | static/gsh` — must output `hello`
6. **Static `,eval` test**: `printf ',(+ 1 2)\n' | static/gsh` — must output `3`
7. **Static `,use` test**: `printf ',use bench.ss\n' | static/gsh` — must run benchmarks

**All builds must compile AND run without error.** A commit that breaks any build target or produces a binary that errors at runtime is a broken commit — do not push it.

## Project Structure

- 22+ modules in `.ss` files, compiled via `gerbil build`
- Executable: `.gerbil/bin/gsh` (dynamic), `static/gsh` (static)
- `_vendor/gambitgsc/` — embedded Gambit compiler (compile-file)
- `_vendor/gerbil-runtime/` — embedded runtime modules + .ssi/.scm data for static binary
- `scripts/generate-gambitgsc` — generates gambitgsc vendor files
- `scripts/generate-gerbil-runtime` — generates gerbil-runtime vendor files
- `scripts/gsc-with-gambitgsc` — gsc wrapper that injects vendor modules into link step

## Static Binary Architecture

The static binary embeds:
- All Gerbil runtime modules (195 `.scm` → `.o` from `lib/static/`)
- All `.ssi` expander metadata (embedded as C string constants)
- All lib/ `.scm` source files (compressed tar.gz archive)
- The Gambit compiler (33 gambitgsc modules)

At startup (`static-compat.ss`):
1. Extracts `.ssi` + `.scm` files to `~/.cache/gsh/lib/`
2. Patches `load-module` to use `.scm` files (no dlopen in static musl)
3. Sub-modules loaded on-demand from cache by patched load-module
