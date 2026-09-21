FROM mcr.microsoft.com/devcontainers/base:dev-ubuntu-24.04

# These dependencies are required by Nix.
RUN apt update -y
RUN apt -y install --no-install-recommends curl xz-utils cmake libreadline-dev libncurses-dev
RUN [ -e /usr/bin/gmake ] || sudo ln -s /usr/bin/make /usr/bin/gmake

# Install Nix
ARG NIX_INSTALL_SCRIPT=https://nixos.org/nix/install
RUN curl -L ${NIX_INSTALL_SCRIPT} | sudo -u vscode NIX_INSTALLER_NO_MODIFY_PROFILE=1 sh

# Configuration for Nix from the repository shared amongst developers.
RUN mkdir -p /etc/nix
COPY .devcontainer/nix.conf /etc/nix/nix.conf
# COPY nix.conf /etc/nix/nix.conf

# This loads the development environment when the container is started.
COPY .devcontainer/profile.sh /etc/profile.d/devcontainer.sh

# COPY /home/vscode/.nix-profile/etc/profile.d/nix.sh /etc/profile.d/devcontainer.sh

# Install GitHub CLI
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg && \
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null && \
    apt-get update && apt-get install -y gh

# Install Node.js and Claude Code CLI
RUN curl -fsSL https://deb.nodesource.com/setup_lts.x | bash - && \
    apt-get install -y nodejs && \
    npm install -g @anthropic-ai/claude-code