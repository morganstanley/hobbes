FROM mcr.microsoft.com/vscode/devcontainers/base:jammy

# These dependencies are required by Nix.
RUN apt update -y
RUN apt -y install --no-install-recommends curl xz-utils cmake libreadline-dev libncurses-dev llvm-12 llvm-12-dev ack
RUN [ -e /usr/bin/gmake ] || sudo ln -s /usr/bin/make /usr/bin/gmake

# Install Nix 
ARG NIX_INSTALL_SCRIPT=https://github.com/nix-community/nix-unstable-installer/releases/download/nix-2.23.0pre20240603_da92ad7/install
RUN curl -L ${NIX_INSTALL_SCRIPT} | sudo -u vscode NIX_INSTALLER_NO_MODIFY_PROFILE=1 sh

# Configuration for Nix from the repository shared amongst developers.
RUN mkdir -p /etc/nix
COPY .devcontainer/nix.conf /etc/nix/nix.conf
# COPY nix.conf /etc/nix/nix.conf

# This loads the development environment when the container is started.
COPY .devcontainer/profile.sh /etc/profile.d/devcontainer.sh

# COPY /home/vscode/.nix-profile/etc/profile.d/nix.sh /etc/profile.d/devcontainer.sh