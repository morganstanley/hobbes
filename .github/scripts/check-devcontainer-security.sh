#!/usr/bin/env bash

set -euo pipefail

dockerfile=.devcontainer/Dockerfile

grep -Fq 'apt-get install -y --no-install-recommends' "${dockerfile}"
grep -Eq '(^|[[:space:]])nix-bin([[:space:]\\]|$)' "${dockerfile}"
grep -Eq '^build-users-group =[[:space:]]*$' .devcontainer/nix.conf

if grep -Eq 'NIX_INSTALL_SCRIPT|nixos\.org/nix/install|print-dev-env|profile\.sh' "${dockerfile}"; then
  echo "devcontainer must not download a Nix installer or auto-load the checkout" >&2
  exit 1
fi

if grep -Eq 'curl[^|]*\|[^|]*(sh|bash)([[:space:]]|$)' "${dockerfile}"; then
  echo "devcontainer must not pipe downloaded code into a shell" >&2
  exit 1
fi

if [[ -e .devcontainer/profile.sh ]]; then
  echo "devcontainer login profile must not auto-evaluate workspace code" >&2
  exit 1
fi

grep -Fq 'nix develop --profile .devcontainer/.profile' README.md
