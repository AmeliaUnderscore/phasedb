# Building phasedb-hs with the crypto dependencies

This project now depends on `cryptonite` (PBKDF2-SHA256 password
hashing + cross-platform entropy for the user/role system in
`Phase.Auth`), which pulls in `memory` and `basement`. If your `cabal`
can reach Hackage normally, `cabal build` should just work once these
are listed in `phasedb-hs.cabal`'s `build-depends` (already done).

If you're building offline (as this was developed), the three package
tarballs are included in `cabal-dependencies/`. Two ways to use them:

## Option A, drop them into cabal's own package cache

```sh
mkdir -p ~/.cabal/packages/hackage.haskell.org
cp cabal-dependencies/*.tar.gz ~/.cabal/packages/hackage.haskell.org/
```

Cabal's solver still wants an index (`01-index.tar`) to know these
packages exist; if you don't have one, use Option B instead.

## Option B, build them directly with GHC's own Setup.hs interface

This is what was actually used to verify everything in this project
compiles and runs. No `cabal-install` solver involved, build and
register each package directly against a local package database:

```sh
mkdir -p /tmp/local-pkg-db
ghc-pkg init /tmp/local-pkg-db/package.conf.d

for pkg in basement-0.0.16 memory-0.18.0 cryptonite-0.30; do
  tar xzf cabal-dependencies/$pkg.tar.gz -C /tmp
  (cd /tmp/$pkg && \
    runghc Setup.hs configure --package-db=/tmp/local-pkg-db/package.conf.d && \
    runghc Setup.hs build && \
    runghc Setup.hs copy && \
    runghc Setup.hs register --gen-pkg-config=$pkg.conf && \
    ghc-pkg --package-db=/tmp/local-pkg-db/package.conf.d register $pkg.conf)
done
```

Then compile phasedb-hs against that package DB directly, e.g.:

```sh
ghc --make -isrc -package-db=/tmp/local-pkg-db/package.conf.d \
    -o phasedb-hs-repl app/Main.hs

ghc --make -isrc -package-db=/tmp/local-pkg-db/package.conf.d \
    -o phasedb-hs-tests test/Main.hs
```

(build order matters: `basement` first, then `memory`, then
`cryptonite`, each depends on the ones before it)

Requires a C toolchain (`gcc`, `make`, `ar`), `cryptonite` compiles
some C stubs for its cryptographic primitives. All standard on
FreeBSD's base system or via `pkg install`.

## Note on `Data.Scientific`

This project also depends on `scientific`, which, like the crypto
packages, may not be reachable if your build environment has no
Hackage access either. It's a small, self-contained package with no
further dependencies, so `cabal get scientific` (or the same
Setup.hs-based approach above) should be enough if needed.

## Option C: an older/distro GHC (e.g. GHC 9.4.x via `apt`)

Verified end to end in a sandboxed Ubuntu 24.04 container with no access
to Hackage, ghcup, or any Haskell-specific download server, only
`apt`'s own mirrors and the `cabal-packages/` tarballs bundled with this
project. `cabal build`, the full test suite (324/324), and the
`phasedb-hs` executable (bootstrap + a `WRITE` command) all came out
clean this way. The one real bug this turned up (a `unix` API
difference) is already fixed in `src/Phase/Storage/Sync.hs`; everything
else below is this *build's own* configuration, kept out of the
project's own `phasedb-hs.cabal` and `~/.cabal/`.

This path exists because `phasedb-hs.cabal` asks for `base ^>=4.20.2.0`
and `default-language: GHC2024`, which need GHC 9.10+, not available
via `apt` on Ubuntu 24.04 (which has GHC 9.4.7). Nothing in the actual
source turned out to need GHC2024 or base-4.20 specifically; GHC2021
and an older `base` bound built the real thing without a single change
to `src/` or `app/` (`Sync.hs`'s fix aside, which isn't GHC-version
related, see below). The relaxation is confined to a *build-only* copy
of the `.cabal` file's version pins, done fresh each time rather than
committed, so the project's own stated minimum (GHC 9.10+) is untouched
for anyone who has it.

1. **Install GHC and cabal-install from the distro:**

   ```sh
   apt-get update
   apt-get install -y ghc cabal-install build-essential
   ```

   This is GHC 9.4.7 / base-4.17.2.0 / cabal-install 3.8.1.0 on Ubuntu
   24.04 ("noble"); adjust for your distro. Confirm with `ghc-pkg list
   | grep base`.

2. **Stop `cabal` from touching the network at all.** Even for an
   all-local build, `cabal`'s first run tries to fetch Hackage's index
   metadata and fails loudly if it can't reach it. Comment out the
   `repository hackage.haskell.org` stanza in `~/.cabal/config`
   (generated on `cabal`'s first invocation if it doesn't exist yet):

   ```sh
   cabal build   # first run: just creates ~/.cabal/config, then errors, expected
   sed -i '/^repository hackage.haskell.org/,/^  -- key-threshold: 3/s/^/-- /' ~/.cabal/config
   ```

3. **Unpack the bundled dependencies as local source packages.** Don't
   try to use `cabal-packages/` as a package *index* (it isn't one,
   there's no `01-index.tar`, so `cabal update` can't use it as a
   repository). Instead extract each tarball and point `cabal.project`
   at the resulting directories directly, which sidesteps Hackage
   entirely, these become ordinary local packages, no index needed:

   ```sh
   mkdir -p vendor && cd vendor
   for pkg in hashable basement primitive integer-logarithms cryptonite scientific memory; do
     ver=$(ls "$CABAL_PACKAGES/$pkg")
     tar xzf "$CABAL_PACKAGES/$pkg/$ver"/*.tar.gz
   done
   cd ..
   ```

   (`$CABAL_PACKAGES` = wherever this project's `cabal-packages/`
   directory ended up.) Then, alongside `phasedb-hs/`:

   ```
   # cabal.project
   packages:
     phasedb-hs
     vendor/hashable-1.5.1.0
     vendor/basement-0.0.16
     vendor/primitive-0.9.1.0
     vendor/integer-logarithms-1.0.5
     vendor/cryptonite-0.30
     vendor/scientific-0.3.8.1
     vendor/memory-0.18.0
   active-repositories: none
   ```

4. **Patch a *working copy* of `phasedb-hs.cabal` for GHC 9.4**, never
   the checked-in file:

   ```sh
   cp phasedb-hs/phasedb-hs.cabal /tmp/phasedb-hs.cabal.orig   # to diff/restore against
   sed -i \
     -e 's/^cabal-version:      3.14/cabal-version:      3.0/' \
     -e 's/base \^>=4\.20\.2\.0/base >=4.14 \&\& <5/g' \
     -e 's/default-language: GHC2024/default-language: GHC2021/g' \
     phasedb-hs/phasedb-hs.cabal
   ```

   (`cabal-version: 3.14` is also unsupported by cabal-install 3.8.1
   itself, separately from the GHC/base question, that's what the
   first `sed` line is for.)

5. **Patch the vendored `hashable` for GHC 9.4's older boot libraries.**
   `hashable-1.5.1.0` wants slightly newer `base`/`bytestring`/`deepseq`
   than GHC 9.4.7 ships (by one patch version each, it was built
   against a just-barely-newer GHC, not a fundamentally incompatible
   one), and unconditionally needs the `os-string` package, which does
   not exist before GHC 9.6/9.8:

   ```sh
   cd vendor/hashable-1.5.1.0
   sed -i \
     -e 's/base        >=4.18.0.0 && <4.23/base        >=4.14.0.0 \&\& <4.23/' \
     -e 's/bytestring  >=0.11.5.3 && <0.13/bytestring  >=0.11.0.0 \&\& <0.13/' \
     -e 's/deepseq     >=1.4.8.1  && <1.6/deepseq     >=1.4.8.0  \&\& <1.6/' \
     hashable.cabal
   ```

   Then replace the unconditional
   `build-depends: os-string >=2.0.2 && <2.1` /
   `build-depends: filepath >=1.4.200.1 && <1.6` pair (meant to be
   version-gated per the surrounding comment, but aren't, in this
   release) with a bare `build-depends: filepath`, this GHC's
   filepath-1.4.2.2 doesn't satisfy either lower bound, so
   `Data.Hashable.Class`'s own `MIN_VERSION_filepath`/
   `MIN_VERSION_os_string` CPP guards correctly fall back to omitting
   the `OsString`/`OsPath` `Hashable` instances, which this project
   never uses anyway.

   That still leaves a real type error, not just a bounds issue: the
   `Hashable (Compose f g a)` / `Hashable (FP.Product f g a)` /
   `Hashable (FS.Sum f g a)` instances (and the `Hashable1` instances
   for the same three types, which depend on them as a superclass
   obligation) need `Eq (Compose f g a)` etc. to follow from
   `Eq (f (g a))` via a newtype-coercion `Eq` instance that `base`
   only gained after 4.17, GHC 9.4.7's constraint solver can't deduce
   it through the older `Eq1`-based instances alone. Delete all six
   instances from `src/Data/Hashable/Class.hs` (search for
   `Hashable (Compose f g a)` to find the start). phasedb-hs never
   hashes a `Compose`/`Product`/`Sum` value (it only reaches `hashable`
   transitively, through `scientific`'s `Hashable Scientific`
   instance), so this is a scope reduction for this build, not a
   behavior change for anything it actually exercises.

6. **Build:**

   ```sh
   cabal build all
   ```

7. **Run the tests, in a UTF-8 locale.** Some test-group names contain
   non-ASCII characters (e.g. "§"); this sandbox's default locale is
   `POSIX`, under which GHC's stdout handle can't encode them and the
   test binary crashes partway through with `commitBuffer: invalid
   argument`. Check `locale -a` for what's available (`C.utf8` worked
   here) and set it for the run:

   ```sh
   LANG=C.utf8 LC_ALL=C.utf8 cabal run phasedb-hs-test
   ```

8. **Run the REPL** the same way, piping answers to the first-run
   bootstrap prompts if testing non-interactively:

   ```sh
   LANG=C.utf8 LC_ALL=C.utf8 cabal run phasedb-hs -- /path/to/some.db
   ```
