/*
 * Replacement for cbits/symbols.x86_64-musl.c in the iserv-proxy dependency.
 * The version selected by our nixpkgs pin uses upstream revision
 * 3f7b2815307c20a0dfd816bdf4a39ab86af3e0d4:
 * https://github.com/stable-haskell/iserv-proxy/blob/3f7b2815307c20a0dfd816bdf4a39ab86af3e0d4/cbits/symbols.x86_64-musl.c
 *
 * static-interpreter.nix copies this file over that source during postPatch.
 * Upstream's Cabal file already compiles that path into the x86_64 Linux
 * iserv-proxy-interpreter executable, so replacing it needs no Cabal changes.
 * This code belongs to the interpreter used during compilation, not hevm.
 *
 * Why replace it?
 * Upstream disables the entire original file with #if 0 and leaves a TODO
 * about fixing its symbol table. Consequently it exports no extra symbols
 * to GHC's runtime linker. A statically linked interpreter cannot use normal
 * dynamic symbol lookup to find all of its existing libc functions. When
 * loading Template Haskell code, the runtime linker can instead pull another
 * copy of those functions, and their global state, out of libc.a.
 *
 * hevm's tGitInfoCwdTry splice launches Git to obtain version information.
 * With GHC 9.12.3 and musl 1.2.6, even a splice using just
 * runIO (readProcess "git" ["--version"] "") reproduced the crash. Syscall
 * tracing and GDB showed a fault in __malloc_alloc_meta before Git executed:
 * the newly loaded allocator dereferenced its uninitialized __libc.auxv.
 * Providing Git in PATH did not fix this; subprocess setup itself crashed.
 *
 * rtsExtraSyms supplies the missing name-to-address mapping, pointing to the
 * libc functions already linked into the interpreter and initialized at
 * startup. The table covers allocation and the subprocess functions used by
 * this Template Haskell path. It is deliberately smaller than upstream's
 * disabled table, and uses the installed GHC/libc headers for declarations.
 * This hook and the duplicate-library-state problem are documented in
 * Note [Extra RTS symbols]:
 * https://github.com/ghc/ghc/blob/ghc-9.12.3-release/rts/RtsSymbols.c
 *
 * Revisit this override when updating nixpkgs/iserv-proxy. Once upstream has
 * a working x86_64 musl table, remove the replacement and verify a splice
 * that actually runs a subprocess, followed by the full static hevm build.
 */
#define _GNU_SOURCE
#include <RtsSymbols.h>
#include <stdlib.h>
#include <spawn.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/auxv.h>

#define SYM(name) { #name, (void *) &name, STRENGTH_NORMAL, SYM_TYPE_CODE }

static RtsSymbolVal symbols[] = {
    SYM(malloc),
    SYM(calloc),
    SYM(realloc),
    SYM(free),
    SYM(getauxval),
    SYM(pipe),
    SYM(fcntl),
    SYM(posix_spawn),
    SYM(posix_spawnp),
    SYM(posix_spawn_file_actions_init),
    SYM(posix_spawn_file_actions_destroy),
    SYM(posix_spawn_file_actions_addopen),
    SYM(posix_spawn_file_actions_addclose),
    SYM(posix_spawn_file_actions_adddup2),
    SYM(posix_spawn_file_actions_addchdir_np),
    SYM(posix_spawnattr_init),
    SYM(posix_spawnattr_destroy),
    SYM(posix_spawnattr_setflags),
    SYM(posix_spawnattr_setpgroup),
    SYM(posix_spawnattr_setsigmask),
    SYM(posix_spawnattr_setsigdefault),
    { 0, 0, STRENGTH_NORMAL, SYM_TYPE_CODE }
};

RtsSymbolVal *rtsExtraSyms(void) {
    return symbols;
}
