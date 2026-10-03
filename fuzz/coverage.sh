#!/bin/bash
#
# Measure what the fuzz corpora cover beside what the test suite covers.
#
#   fuzz/coverage.sh <build-dir> [corpus-root] [out-dir]
#
# The build directory has to be configured with Clang source-based coverage,
# and with the harnesses built as standalone runners so that they and
# hobbes-test link against the same instrumented libhobbes:
#
#   COV="-fprofile-instr-generate -fcoverage-mapping"
#   cmake -B build-cov -DCMAKE_BUILD_TYPE=RelWithDebInfo \
#     -DBUILD_FUZZERS=ON -DFUZZ_STANDALONE=ON \
#     -DCMAKE_CXX_FLAGS="$COV" -DCMAKE_C_FLAGS="$COV" -DCMAKE_EXE_LINKER_FLAGS="$COV"
#   cmake --build build-cov
#
# corpus-root holds one directory per harness, named as in fuzz/corpus
# (parse-expr, typecheck-expr, ...); it defaults to fuzz/corpus, which holds
# only reproducers. A corpus a fuzzer has grown is what says something about
# fuzzing's reach. Every file in a harness's directory is replayed through it
# once; nothing is mutated.
#
# Writes tests.lcov, fuzz.lcov and one lcov per harness to out-dir (default
# coverage-compare under the build directory), and the comparison report,
# from coverage-compare.py, to report.md there.

set -eu

BUILD="$(cd "${1:?usage: coverage.sh <build-dir> [corpus-root] [out-dir]}" && pwd)"
SRC="$(cd "$(dirname "$0")/.." && pwd)"
CORPORA="$(cd "${2:-$SRC/fuzz/corpus}" && pwd)"
OUT="${3:-$BUILD/coverage-compare}"
HARNESSES=(parse-expr typecheck-expr type-decode fregion-reader hog-session)

# the profile tools have to match the compiler that wrote the profiles
CXX="$(sed -n 's/^CMAKE_CXX_COMPILER:[A-Z]*=//p' "$BUILD/CMakeCache.txt")"
LLVM_BIN="$(dirname "$CXX")"
PROFDATA="${LLVM_PROFDATA:-$LLVM_BIN/llvm-profdata}"
LLVMCOV="${LLVM_COV:-$LLVM_BIN/llvm-cov}"

rm -rf "$OUT"
mkdir -p "$OUT/raw"

# the tests, from the build directory as CI runs them (PREPL and Spawn
# launch hi and mock-proc from there); a failing test still leaves its
# coverage, and says so
echo "coverage: running hobbes-test"
(cd "$BUILD" && LLVM_PROFILE_FILE="$OUT/raw/tests-%p.profraw" ./hobbes-test > "$OUT/tests.log" 2>&1) ||
  echo "coverage: hobbes-test reported failures (see $OUT/tests.log)"

OBJECTS=(-object "$BUILD/hi" -object "$BUILD/hog")
REPLAYED=()
for h in "${HARNESSES[@]}"; do
  exe="$BUILD/fuzz/fuzz-$h"
  dir="$CORPORA/$h"
  [ -x "$exe" ] || { echo "coverage: no $exe, skipping"; continue; }
  [ -d "$dir" ] || { echo "coverage: no corpus for $h in $CORPORA, skipping"; continue; }
  count=$(find "$dir" -maxdepth 1 -type f | wc -l | tr -d ' ')
  echo "coverage: replaying $count inputs through fuzz-$h"
  # batched, since a grown corpus can outrun the argument list; an input
  # that crashes the harness ends its batch, not the measurement
  find "$dir" -maxdepth 1 -type f -print0 |
    LLVM_PROFILE_FILE="$OUT/raw/fuzz-$h-%p.profraw" xargs -0 -n 500 "$exe" > "$OUT/fuzz-$h.log" 2>&1 ||
    echo "coverage: fuzz-$h did not finish every batch (see $OUT/fuzz-$h.log)"
  OBJECTS+=(-object "$exe")
  REPLAYED+=("$h")
done

# one export per profile over the same binaries, so that every report lists
# the same instrumented lines and only their counts differ
IGNORE='(^|/)(test|fuzz|build[^/]*)/|^/usr/|^/opt/|^/Library/'
export_lcov() { # profdata out
  "$LLVMCOV" export -format=lcov -instr-profile="$1" -ignore-filename-regex="$IGNORE" \
    "$BUILD/hobbes-test" "${OBJECTS[@]}" > "$2"
}

"$PROFDATA" merge -sparse -o "$OUT/tests.profdata" "$OUT"/raw/tests-*.profraw
export_lcov "$OUT/tests.profdata" "$OUT/tests.lcov"

"$PROFDATA" merge -sparse -o "$OUT/fuzz.profdata" "$OUT"/raw/fuzz-*.profraw
export_lcov "$OUT/fuzz.profdata" "$OUT/fuzz.lcov"

PER_HARNESS=()
for h in "${REPLAYED[@]}"; do
  "$PROFDATA" merge -sparse -o "$OUT/fuzz-$h.profdata" "$OUT"/raw/fuzz-"$h"-*.profraw
  export_lcov "$OUT/fuzz-$h.profdata" "$OUT/fuzz-$h.lcov"
  PER_HARNESS+=("$h=$OUT/fuzz-$h.lcov")
done

python3 "$SRC/fuzz/coverage-compare.py" "$SRC" "$OUT/tests.lcov" "$OUT/fuzz.lcov" "${PER_HARNESS[@]}" > "$OUT/report.md"
echo "coverage: report in $OUT/report.md"
