#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HI="${1:-$SCRIPT_DIR/../../build/hi}"
UPDATE=false

for arg in "$@"; do
  if [ "$arg" = "--update" ]; then
    UPDATE=true
  fi
done

if [ ! -x "$HI" ]; then
  echo "ERROR: hi binary not found or not executable: $HI" >&2
  exit 1
fi

passed=0
failed=0
updated=0
errors=""

for hob in "$SCRIPT_DIR"/*.hob; do
  [ -f "$hob" ] || continue
  name="$(basename "$hob" .hob)"
  expected="$SCRIPT_DIR/$name.expected"

  # Extract @test: directive from line 1
  directive=$(head -1 "$hob")
  if [[ ! "$directive" =~ ^//[[:space:]]*@test:[[:space:]]*(.*) ]]; then
    echo "SKIP $name (no @test: directive)"
    continue
  fi
  expr="${BASH_REMATCH[1]}"

  # Run hi
  actual=$("$HI" "$hob" -e "$expr" -s -x 2>&1) || true

  if [ "$UPDATE" = true ]; then
    printf '%s\n' "$actual" > "$expected"
    echo "UPDATED $name"
    updated=$((updated + 1))
    continue
  fi

  if [ ! -f "$expected" ]; then
    echo "FAIL $name (missing $name.expected)"
    errors="$errors\n  $name: missing expected file"
    failed=$((failed + 1))
    continue
  fi

  expected_content=$(cat "$expected")
  if [ "$actual" = "$expected_content" ]; then
    echo "PASS $name"
    passed=$((passed + 1))
  else
    echo "FAIL $name"
    echo "  expected: $expected_content"
    echo "  actual:   $actual"
    errors="$errors\n  $name"
    failed=$((failed + 1))
  fi
done

echo ""
if [ "$UPDATE" = true ]; then
  echo "Updated $updated test(s)"
else
  echo "$passed passed, $failed failed"
  if [ $failed -gt 0 ]; then
    printf "Failed tests:%b\n" "$errors"
    exit 1
  fi
fi
