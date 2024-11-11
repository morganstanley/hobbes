#!/bin/bash

PROJECT_DIR=${PROJECT_DIR:-/workspace}

# Make Nix available as it's not installed system-wide.
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ] ; then
   . $HOME/.nix-profile/etc/profile.d/nix.sh
fi

# Only load the development environment if this is a login shell so calling a
# shell later on doesn't reload the environment again.
if shopt -q login_shell; then
  pushd "${PROJECT_DIR}"
  eval "$(nix print-dev-env --profile "${PROJECT_DIR}/.devcontainer/.profile")"
  popd
fi