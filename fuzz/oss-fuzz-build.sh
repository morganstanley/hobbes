#!/bin/bash -eu
#
# Build the fuzz harnesses the way OSS-Fuzz expects them.
#
# This lives in the hobbes tree rather than in google/oss-fuzz so that changes
# to the harnesses and to their build stay together; projects/hobbes/build.sh
# in the OSS-Fuzz repository is a one-line wrapper that execs this script.
#
# Consumes the OSS-Fuzz build environment: $SRC, $WORK, $OUT, $CC, $CXX,
# $CFLAGS, $CXXFLAGS and $LIB_FUZZING_ENGINE.
# See https://google.github.io/oss-fuzz/getting-started/new-project-guide/

# Under OSS-Fuzz the checkout is $SRC/hobbes; fall back to this script's own
# checkout so the build can be reproduced by hand.
SRCDIR="${SRC:-}/hobbes"
[ -n "${SRC:-}" ] && [ -d "$SRCDIR" ] || SRCDIR="$(cd "$(dirname "$0")/.." && pwd)"
BUILD="${WORK:-$SRCDIR}/build-oss-fuzz"

# The Dockerfile installs a versioned LLVM from apt.llvm.org; hobbes's CMake
# probes for LLVM 12 first, so point it at the install we actually want.
LLVM_VERSION="${HOBBES_LLVM_VERSION:-18}"
LLVM_CMAKE_DIR="/usr/lib/llvm-${LLVM_VERSION}/lib/cmake/llvm"

# OSS-Fuzz defaults C++ builds to libc++, but the LLVM hobbes links against
# comes from a distribution package built against libstdc++, and mixing the two
# standard libraries breaks the link on LLVM's std::string/StringRef APIs.
# Two things are given up by dropping it, neither of which this project uses:
# MemorySanitizer, which needs an instrumented libc++, and the centipede engine,
# whose prebuilt runner is itself compiled against libc++ and so cannot be
# linked into a libstdc++ binary. project.yaml enables neither.
CXXFLAGS="${CXXFLAGS//-stdlib=libc++/}"

# hobbes compiles with -Werror on Linux, appended after CMAKE_CXX_FLAGS, so it
# cannot be overridden from the command line. OSS-Fuzz tracks a recent Clang,
# where a newly added warning would otherwise break the build rather than the
# fuzzers; add_compile_options lands after CMAKE_CXX_FLAGS on the compile line.
cat > "$BUILD-overrides.cmake" <<'CMAKE'
add_compile_options(-Wno-error -Wno-error=old-style-cast)
CMAKE

# Configure from scratch. $WORK survives between runs when the same tree is
# rebuilt for another sanitizer or engine (infra/helper.py does this, and so
# does anyone reproducing a build by hand). Switching engine changes the
# compiler to a wrapper such as afl-clang-fast++, and CMake responds by
# deleting the cache and re-configuring itself -- which drops the -D options
# from this command line, leaving BUILD_FUZZERS at its OFF default so that the
# harnesses are silently never defined.
rm -rf "$BUILD"

cmake -S "$SRCDIR" -B "$BUILD" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_C_COMPILER="$CC" \
  -DCMAKE_CXX_COMPILER="$CXX" \
  -DCMAKE_C_FLAGS="$CFLAGS" \
  -DCMAKE_CXX_FLAGS="$CXXFLAGS" \
  -DCMAKE_PROJECT_hobbes_INCLUDE="$BUILD-overrides.cmake" \
  -DLLVM_DIR="$LLVM_CMAKE_DIR" \
  -DBUILD_FUZZERS=ON \
  -DFUZZING_ENGINE_LIB="$LIB_FUZZING_ENGINE"

# Only the harnesses: this pulls in libhobbes but skips hi, hog, hobbes-pic and
# the test binary, none of which the fuzzers need.
HARNESSES=(fuzz-type-decode fuzz-fregion-reader fuzz-parse-expr)
cmake --build "$BUILD" -j"$(nproc)" --target "${HARNESSES[@]}"

for h in "${HARNESSES[@]}"; do
  cp "$BUILD/fuzz/$h" "$OUT/$h"
done

# hobbes links a copy of LLVM that OSS-Fuzz did not build, so the two sides
# disagree about std::vector's ASan container annotations and every run would
# report a spurious container-overflow. Same reasoning as fuzz/run-fuzzer.sh.
for h in "${HARNESSES[@]}"; do
  cat > "$OUT/$h.options" <<'OPTS'
[asan]
detect_container_overflow=0
OPTS
done

# The parser harness reuses one hobbes::cc across iterations and its arenas are
# not reclaimed per input, so LeakSanitizer reports the accumulated corpus as
# leaked. Revisit if per-iteration allocation is ever made reclaimable.
#
# Disabled on both sides, as fuzz/run-fuzzer.sh does locally: the [asan] entry
# turns LeakSanitizer itself off, which is what suppresses the report, and the
# [libfuzzer] entry stops libFuzzer running its own per-input leak check that
# would then find nothing.
cat >> "$OUT/fuzz-parse-expr.options" <<'OPTS'
detect_leaks=0

[libfuzzer]
detect_leaks=0
OPTS

# Named after the target, so libFuzzer picks it up without an options entry.
cp "$SRCDIR/fuzz/parse-expr.dict" "$OUT/fuzz-parse-expr.dict"

if [ -d "$SRCDIR/fuzz/corpus/parse-expr" ]; then
  zip -jq "$OUT/fuzz-parse-expr_seed_corpus.zip" "$SRCDIR/fuzz/corpus/parse-expr/"*
fi
