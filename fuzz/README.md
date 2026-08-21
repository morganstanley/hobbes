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
  `$TMPDIR` and opens it. Seeds in `corpus/fregion-reader/`, which holds past
  reproducers; add to them by copying small structured data files produced by
  `hog` or the `Storage` tests. Malformed page metadata can make the reader
  attempt large mappings, so keep the default `-rss_limit_mb` in place, and a
  malformed page table can make it spin, so keep `-timeout` in place too.
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

## OSS-Fuzz

The same harnesses are meant to run continuously on Google's OSS-Fuzz. That
needs a submission to the [OSS-Fuzz
repository](https://github.com/google/oss-fuzz) which has not been accepted
yet, so nothing runs there until it is; ClusterFuzzLite below is what covers
pull requests in the meantime. What gets submitted is a `projects/hobbes/`
directory there — a `project.yaml`, a
`Dockerfile` that installs LLVM and clones this repository, and a `build.sh`
that is a one-line wrapper around `fuzz/oss-fuzz-build.sh` here. Keeping the
real build script in this tree means harness changes and build changes land in
the same commit.

`oss-fuzz-build.sh` differs from a local fuzzing build in three ways worth
knowing about:

* It links `$LIB_FUZZING_ENGINE` through the `FUZZING_ENGINE_LIB` CMake
  variable instead of `-fsanitize=fuzzer`, so the harnesses also build under
  AFL++ and honggfuzz. Not centipede: its runner is prebuilt against libc++,
  which the dropped `-stdlib=libc++` below rules out.
* It writes a `.options` file per target disabling ASan's
  `detect_container_overflow`. hobbes links an LLVM that OSS-Fuzz did not
  build, and the two disagree about `std::vector` container annotations — the
  same problem `run-fuzzer.sh` works around locally.
* It drops `-stdlib=libc++` from `CXXFLAGS`. OSS-Fuzz defaults C++ builds to
  libc++, and the packaged LLVM is built against libstdc++; mixing them breaks
  the link. MemorySanitizer is not enabled for the same underlying reason —
  MSan needs every dependency instrumented, LLVM included.

To reproduce an OSS-Fuzz build locally you need Docker and a checkout of the
OSS-Fuzz repository:

```bash
python3 infra/helper.py build_image hobbes
python3 infra/helper.py build_fuzzers --sanitizer address hobbes
python3 infra/helper.py check_build hobbes
python3 infra/helper.py run_fuzzer hobbes fuzz-parse-expr
```

Findings arrive as OSS-Fuzz issues with a reproducer attached; `triage.py`
above is for local campaigns, but the same rule applies — check a finding
against the threat model in `doc/en/security.rst` before treating it as a
vulnerability.

## ClusterFuzzLite

`.clusterfuzzlite/` and `.github/workflows/clusterfuzzlite.yml` run the same
harnesses on pull requests that touch code they cover, for a few minutes each,
against the change itself rather than against `main`. Unlike OSS-Fuzz this
needs no registration with any service, so it applies to every pull request
regardless of how the OSS-Fuzz submission is received.

The image mirrors the OSS-Fuzz one and the build script is shared; the only
difference is that the source is copied in from the checkout being tested
instead of cloned. A finding fails the check and attaches the reproducer to the
workflow run, which `fuzz-<harness> <reproducer>` replays locally.

Two extensions are deliberately not configured, because both need a separate
repository to hold state that this project does not have yet:

* **Batch fuzzing** on a schedule, which builds up a corpus over time rather
  than starting cold on each pull request.
* **Continuous builds** on pushes to `main`, which is what lets pull request
  fuzzing tell a newly introduced crash from one that was already there.

See the [ClusterFuzzLite documentation](https://google.github.io/clusterfuzzlite/)
for both.
