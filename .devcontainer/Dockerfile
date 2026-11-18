FROM mcr.microsoft.com/devcontainers/base:dev-ubuntu-24.04

# Install build tools and Nix from Ubuntu's signed package archive.  Do not
# replace nix-bin with a downloaded installer: executing a mutable remote
# script during the image build makes the development environment depend on
# unverified code.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      cmake curl gh libncurses-dev libreadline-dev nix-bin nodejs npm xz-utils \
 && rm -rf /var/lib/apt/lists/*
RUN [ -e /usr/bin/gmake ] || sudo ln -s /usr/bin/make /usr/bin/gmake

# Configuration for Nix from the repository shared amongst developers.
RUN mkdir -p /etc/nix
COPY .devcontainer/nix.conf /etc/nix/nix.conf

# The Ubuntu package is configured for a system daemon.  Devcontainers do not
# run that daemon, so initialise a single-user store owned by the container
# user instead.
RUN mkdir -p /nix \
 && chown -R vscode:vscode /nix \
 && sudo -u vscode nix-store --init

# Install Claude Code CLI.  Node.js and npm come from Ubuntu's signed package
# archive above rather than an unverified repository-setup script.
RUN npm install -g @anthropic-ai/claude-code
