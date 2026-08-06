# Fuzzing harnesses

Hobbes consumes untrusted bytes in exactly three places (see the security
model documentation in `doc/en/security.rst`, added by #514), and each has a
harness here:

| Harness              | Surface                                                          |
| -------------------- | ---------------------------------------------------------------- |
| `fuzz-type-decode`   | binary type descriptions (`hobbes::decode`, used by the RPC layer on peer-supplied bytes) |
| `fuzz-fregion-reader`| structured data file images (`hobbes::fregion::reader`: header, page table, environment records) |
| `fuzz-parse-expr`    | source text through the lexer/LALR parser (`cc::readExpr`; parse only, nothing is evaluated) |

For all three, throwing an exception on malformed input is the expected
behavior; the harnesses catch those. What fuzzing hunts for is memory
unsafety — out-of-bounds access, overflow-driven size math — which is why
these should run under sanitizers.

## Building

Requires a Clang that ships libFuzzer (upstream or Homebrew Clang; Apple's
Xcode Clang does not). With any other compiler the same targets build as
standalone runners that replay input files named on the command line, which
is also the convenient way to check a crash reproducer.

With Clang, `BUILD_FUZZERS=ON` compiles the whole build (including
`libhobbes`) with `-fsanitize=fuzzer-no-link` so the fuzzers observe coverage
inside the library, not just in the harness. Use a dedicated build directory
for fuzzing rather than sharing one with normal development builds.

```bash
cmake -B build-fuzz -DCMAKE_BUILD_TYPE=Debug -DBUILD_FUZZERS=ON -DUSE_ASAN_AND_UBSAN=ON
cmake --build build-fuzz -j --target fuzz-type-decode fuzz-fregion-reader fuzz-parse-expr
```

## Running

```bash
mkdir -p corpus/type-decode
./build-fuzz/fuzz/fuzz-type-decode -max_len=512 corpus/type-decode
```

Notes per harness:

* **fuzz-type-decode** — seeds can be generated from any hobbes type with
  `hobbes::encode(type, &bytes)`; short random inputs also make progress
  quickly since the format is compact.
* **fuzz-fregion-reader** — writes each input to one scratch file under
  `$TMPDIR` and opens it. Seed by copying small structured data files
  produced by `hog` or the `Storage` tests. Malformed page metadata can make
  the reader attempt large mappings, so keep the default `-rss_limit_mb` in
  place.
* **fuzz-parse-expr** — seeds in `corpus/parse-expr/`. The compiler instance
  is constructed once and reused; parsing allocates from arenas that are not
  reclaimed per-iteration, so run with `-detect_leaks=0` and let
  `-rss_limit_mb` restart the process as needed.

Replaying a reproducer (works in both build modes):

```bash
./build-fuzz/fuzz/fuzz-fregion-reader crash-abc123
```

## Running a campaign

The commands above are fine for a quick look, but fuzzing pays off over hours
and days. `run-fuzzer.sh` runs one harness for a bounded time and is meant to
be invoked repeatedly by a scheduler (cron, systemd, launchd), so a campaign
survives reboots and OOM kills:

```bash
export FUZZ_HOME=~/hobbes-fuzz   # holds the build, corpus/, artifacts/, logs/
./fuzz/run-fuzzer.sh parse-expr 3600
```

The build directory is `$FUZZ_BUILD` if you set it, otherwise whichever of
`build-fuzz/` or `build/` it finds under `$FUZZ_HOME`. Copy the script into
your campaign directory if you would rather not run it from a checkout.

It runs libFuzzer in **fork mode**, which matters once anything has been
found: by default libFuzzer stops at the first crash, so a single known bug
blocks all further progress. It also writes a one-line summary per run to
`logs/summary.log`, and calls `$FUZZ_HOME/bin/notify.sh "<message>"` if you
provide one.

Two environment notes it handles for you:

* Linking against an LLVM that was not itself built with ASan produces
  spurious `container-overflow` reports, because only one side of a shared
  `std::vector` updates the annotations. This is normal for distro and
  Homebrew LLVM packages; set `HOBBES_FUZZ_UNINSTRUMENTED_LLVM=0` if your
  LLVM *is* instrumented.
* Leak detection needs disabling for `parse-expr` in two places — libFuzzer's
  `-detect_leaks=0` and LeakSanitizer's own at-exit check via `ASAN_OPTIONS`.

## Triaging findings

A campaign produces far more artifacts than distinct bugs — one defect
reachable many ways yields many reproducers. `triage.py` replays every
artifact, groups them by root cause (error class plus the first hobbes source
location, with value-specific detail normalised away), and writes one
markdown report per distinct issue:

```bash
FUZZ_HOME=~/hobbes-fuzz ./fuzz/triage.py
```

It resolves the build directory the same way as `run-fuzzer.sh`, and the
reproduce command in each report uses the paths it actually found.

Each `reports/<issue>.md` contains the reproduce command, the sanitizer
output, the hobbes stack frames, a hexdump of the smallest reproducer, and a
triage checklist. `reports/README.md` indexes them.

**Before filing anything**, check the finding against the threat model in
`doc/en/security.rst`. Memory unsafety reachable from malformed data files,
wire bytes or source text is a security issue and should be reported through
the process in `SECURITY.md` — *not* a public issue. Behaviour that the
threat model calls out as intended (Hobbes code having full host-process
access, an RPC peer executing code) is not a vulnerability.
