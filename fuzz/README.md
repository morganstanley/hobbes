# Fuzzing harnesses

Hobbes consumes untrusted bytes in exactly three places (see the security
model in `doc/en/security.rst`), and each has a harness here:

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
