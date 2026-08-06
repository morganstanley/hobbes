#!/bin/bash
# Run one fuzz harness for a bounded time, writing new inputs back to its
# corpus and any crash reproducers to an artifacts directory.
#
# Designed to be invoked repeatedly by a scheduler (cron, systemd, launchd)
# rather than to run forever: each invocation ends cleanly after $DURATION,
# so a campaign survives reboots and OOM kills.
#
#   FUZZ_HOME=~/hobbes-fuzz ./run-fuzzer.sh parse-expr 3600
#
# Expects $FUZZ_HOME to hold the campaign state (corpus/, artifacts/, logs/)
# and a fuzzing build configured with -DBUILD_FUZZERS=ON
# -DUSE_ASAN_AND_UBSAN=ON. The build directory is $FUZZ_BUILD if set,
# otherwise whichever of build-fuzz/ or build/ is found. See README.md.
set -u

FUZZ_HOME="${FUZZ_HOME:-$PWD}"
HARNESS="${1:?usage: run-fuzzer.sh <type-decode|fregion-reader|parse-expr> [seconds]}"
DURATION="${2:-3600}"

# Locate the fuzzing build: explicit override, else the name the README uses,
# else the one a campaign layout typically uses.
BUILD="${FUZZ_BUILD:-}"
if [ -z "$BUILD" ]; then
  for d in "$FUZZ_HOME/build-fuzz" "$FUZZ_HOME/build"; do
    [ -d "$d/fuzz" ] && BUILD="$d" && break
  done
  BUILD="${BUILD:-$FUZZ_HOME/build}"
fi

BIN="$BUILD/fuzz/fuzz-$HARNESS"
CORPUS="$FUZZ_HOME/corpus/$HARNESS"
LOG="$FUZZ_HOME/logs/$HARNESS-$(date +%Y%m%d-%H%M%S).log"

[ -x "$BIN" ] || { echo "no such harness binary: $BIN" >&2; exit 1; }
mkdir -p "$CORPUS" "$FUZZ_HOME/artifacts" "$FUZZ_HOME/logs"

export ASAN_OPTIONS="${ASAN_OPTIONS:-abort_on_error=0}"
export UBSAN_OPTIONS="${UBSAN_OPTIONS:-print_stacktrace=1}"

# When libhobbes links against an LLVM that was not itself built with ASan,
# the two share std::vector instances whose annotations only one side updates,
# which produces spurious container-overflow reports. Common with distro or
# Homebrew LLVM packages.
case "${HOBBES_FUZZ_UNINSTRUMENTED_LLVM:-1}" in
  1|yes|true) ASAN_OPTIONS="$ASAN_OPTIONS:detect_container_overflow=0" ;;
esac

EXTRA=()
if [ "$HARNESS" = "parse-expr" ]; then
  # The compiler allocates from arenas that are not reclaimed per iteration,
  # so leak detection reports the whole corpus as leaked. Both flags are
  # needed: libFuzzer's own check and LeakSanitizer's at-exit check.
  EXTRA+=(-detect_leaks=0)
  ASAN_OPTIONS="$ASAN_OPTIONS:detect_leaks=0"
fi
export ASAN_OPTIONS

before=$(find "$FUZZ_HOME/artifacts" -type f | wc -l | tr -d ' ')

# Fork mode keeps the campaign going past a crash. Without it libFuzzer stops
# at the first finding, so one known bug blocks all further progress.
nice -n 10 "$BIN" "$CORPUS" \
  -max_total_time="$DURATION" \
  -fork=2 -ignore_crashes=1 -ignore_ooms=1 -ignore_timeouts=1 \
  -rss_limit_mb=2048 \
  -print_final_stats=1 \
  -artifact_prefix="$FUZZ_HOME/artifacts/" \
  ${EXTRA[@]+"${EXTRA[@]}"} >>"$LOG" 2>&1
rc=$?

# fork mode may leave per-child logs in the working directory
for f in fuzz-[0-9]*.log; do [ -e "$f" ] && cat "$f" >>"$LOG" && rm -f "$f"; done

after=$(find "$FUZZ_HOME/artifacts" -type f | wc -l | tr -d ' ')
new=$((after - before))
echo "[$(date)] $HARNESS rc=$rc new_artifacts=$new corpus=$(ls "$CORPUS" | wc -l | tr -d ' ')" \
  >>"$FUZZ_HOME/logs/summary.log"

if [ "$new" -gt 0 ]; then
  echo "[$(date)] $new new artifact(s) from $HARNESS; see $LOG" \
    >>"$FUZZ_HOME/logs/findings.log"
  # Optional hook: $FUZZ_HOME/bin/notify.sh "<message>"
  if [ -x "$FUZZ_HOME/bin/notify.sh" ]; then
    "$FUZZ_HOME/bin/notify.sh" "hobbes fuzzing: $new new artifact(s) from $HARNESS" \
      >/dev/null 2>&1 || true
  fi
fi
exit 0
