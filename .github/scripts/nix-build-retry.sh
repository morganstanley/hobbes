#!/usr/bin/env bash
#
# Build a flake attribute, retrying past GitHub's secondary rate limit.
#
# Nix resolves the github: flake inputs (flake-utils, nixpkgs) by downloading a
# tarball from api.github.com. When the build matrices start, 23 jobs fetch both
# inputs within a minute or two, and GitHub answers some of them with
#
#     HTTP error 429 ... 429: Too Many Requests
#     For more on scraping GitHub and how it may affect your rights, ...
#
# That is the secondary (anti-abuse) limit rather than the documented hourly
# quota: it is applied per source address, and hosted runners share addresses.
# A token does not lift it, and install-nix-action already configures one from
# GITHUB_TOKEN regardless. It does clear on its own after about a minute.
#
# Nix retries the download itself, but gives up after roughly 45 seconds --
# just short of the reset -- which is why re-running the job by hand works.
# Waiting longer between attempts is what actually gets past it.
#
# Only that failure is retried. A compile error or a failing test is not going
# to build on the second attempt, and retrying it would just delay the report.
set -euo pipefail

attr=${1:?usage: nix-build-retry.sh <flake-attr>}
attempts=${NIX_BUILD_ATTEMPTS:-3}
delay=${NIX_BUILD_RETRY_DELAY:-90}

log=$(mktemp)
trap 'rm -f "$log"' EXIT

# Nix reports a failed derivation with the last 25 lines of its build log and
# a note saying where the rest is: "For full logs, run: nix log /nix/store/...".
# Twenty-five lines is the shadow-byte legend of a sanitizer report, with the
# report itself -- the error, the stack -- cut off above it, and by the time
# anyone reads the job the runner and its store are gone. So run that command
# here, while the store is still there, and keep enough of the tail to hold a
# sanitizer report and the ctest summary after it.
print_failed_build_log() {
  local cmd
  cmd=$(grep -oE 'nix log /nix/store/[^ ]+\.drv' "$1" | tail -1) || true
  if [ -n "$cmd" ]; then
    echo "::group::full build log of the failed derivation (last 600 lines)"
    $cmd 2>/dev/null | tail -n 600 || echo "(could not read the build log)"
    echo "::endgroup::"
  fi
}

for attempt in $(seq 1 "$attempts"); do
  if nix build "$attr" 2>&1 | tee "$log"; then
    exit 0
  fi

  # Match the rate limit itself, not download failures in general: a 404 from a
  # bad flake ref is a download failure too, and it will still be a 404 three
  # attempts and two waits later. Both spellings below come from the same 429 --
  # nix prints the status line, then quotes GitHub's body.
  if ! grep -qE 'HTTP error 429|429: Too Many Requests' "$log"; then
    echo "nix build $attr failed, and not on a GitHub rate limit -- not retrying" >&2
    print_failed_build_log "$log"
    exit 1
  fi

  if [ "$attempt" -lt "$attempts" ]; then
    echo "nix build $attr hit GitHub's rate limit (attempt $attempt/$attempts); retrying in ${delay}s"
    sleep "$delay"
  fi
done

echo "nix build $attr still rate limited after $attempts attempts" >&2
exit 1
