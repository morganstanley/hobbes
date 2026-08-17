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

for attempt in $(seq 1 "$attempts"); do
  if nix build "$attr" 2>&1 | tee "$log"; then
    exit 0
  fi

  if ! grep -qE 'HTTP error 429|error: unable to download' "$log"; then
    echo "nix build $attr failed, and not on a GitHub rate limit -- not retrying" >&2
    exit 1
  fi

  if [ "$attempt" -lt "$attempts" ]; then
    echo "nix build $attr hit GitHub's rate limit (attempt $attempt/$attempts); retrying in ${delay}s"
    sleep "$delay"
  fi
done

echo "nix build $attr still rate limited after $attempts attempts" >&2
exit 1
