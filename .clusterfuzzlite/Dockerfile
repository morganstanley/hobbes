# Build environment for ClusterFuzzLite, which fuzzes the harnesses in fuzz/ on
# every pull request. It stands in for the OSS-Fuzz image
# (projects/hobbes/Dockerfile in google/oss-fuzz), building the same targets the
# same way, but it takes the source from the checkout being tested rather than
# cloning main, and it installs LLVM more carefully than that image does: pinned
# installer, vendored signing key, retries. Those belong there too, but that
# image installs LLVM before it clones, so moving them across is its own change.

FROM gcr.io/oss-fuzz-base/base-builder:v1

# hobbes is built on LLVM, which the base image does not provide as a
# development package. 18 is covered by the build matrix in build.yml.
ENV HOBBES_LLVM_VERSION=18

# apt.llvm.org publishes an installer that adds its apt repository. Fetching it
# from that address runs whatever the site serves at build time; this pins the
# revision and checks the script against its hash before running it, so the
# build installs the compiler the same way every time (this is also what
# OpenSSF Scorecard's pinned-dependencies check asks for).
ENV LLVM_SH_COMMIT=6dc0d1ad7de83d0782731687fd555a7859b4da58
ENV LLVM_SH_SHA256=03878e08f47b66cc95bc4b544b0db3c6d9ce8d60e6cf2492ae357984330a9eae

# The repository's signing key, from the checkout rather than from the network.
#
# The installer fetches this key itself, but it writes it with
# 'download_key ... | tee /etc/apt/trusted.gpg.d/apt.llvm.org.asc', and tee
# creates that file before the download has produced anything: a fetch that
# comes back short or empty leaves a zero-byte key behind. The installer guards
# the download with '[[ ! -f ... ]]', so every later attempt finds that file,
# skips the key, adds the repository anyway, and fails 'apt-get update' with
# NO_PUBKEY 15CF4D18AF4F7421. One bad fetch therefore fails the whole layer no
# matter how many times the retry below runs it again.
#
# Placing the key here means the installer never fetches it: the file is
# already there, so its guard skips the download that can go wrong. That also
# takes one more build-time network dependency out (what Scorecard's
# pinned-dependencies check asks for), the same reason the installer itself is
# pinned above.
#
# It goes in trusted.gpg.d, where the installer would have put it, so the guard
# sees it; that trusts the key for every repository, which is what installing
# LLVM this way has always done.
#
# To refresh it: fetch https://apt.llvm.org/llvm-snapshot.gpg.key, confirm the
# fingerprint below, and replace the file.
ENV LLVM_APT_KEY_FPR=6084F3CF814B57C1CF12EFD515CF4D18AF4F7421
COPY .github/apt.llvm.org.asc /etc/apt/trusted.gpg.d/apt.llvm.org.asc

# One layer, so that the apt indexes this writes are removed by the cleanup at
# the end of it rather than being kept in a layer the cleanup cannot reach.
#
# The retry is around the installer: it decides whether this distribution is
# supported by asking apt.llvm.org for its repository index, and reports a
# request that did not come back as an unsupported distribution -- so a single
# dropped request fails the build with a message about the distribution rather
# than about the network. The wait between attempts doubles, because the site
# has stayed away from a builder for longer than three quick attempts last. The
# subshell leaves with the installer's success, or fails the layer once six
# attempts over some minutes have not come back. With the key already in place
# the attempts are now independent of each other, as they were meant to be.
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        cmake make python3 zip \
        zlib1g-dev libncurses-dev libzstd-dev libreadline-dev \
        wget gnupg lsb-release software-properties-common && \
    gpg --show-keys --with-colons /etc/apt/trusted.gpg.d/apt.llvm.org.asc \
      | grep -qx "fpr:::::::::${LLVM_APT_KEY_FPR}:" && \
    wget -q -O /tmp/llvm.sh \
      "https://raw.githubusercontent.com/opencollab/llvm-jenkins.debian.net/${LLVM_SH_COMMIT}/llvm.sh" && \
    echo "${LLVM_SH_SHA256}  /tmp/llvm.sh" | sha256sum -c - && \
    ( wait=15; \
      for i in 1 2 3 4 5 6; do \
        bash /tmp/llvm.sh "$HOBBES_LLVM_VERSION" && exit 0; \
        [ "$i" = 6 ] && break; \
        echo "llvm.sh failed (attempt $i), retrying in ${wait}s"; \
        sleep "$wait"; \
        wait=$((wait * 2)); \
      done; \
      exit 1 ) && \
    apt-get install -y --no-install-recommends -o Acquire::Retries=5 \
        llvm-$HOBBES_LLVM_VERSION-dev && \
    rm -f /tmp/llvm.sh && \
    rm -rf /var/lib/apt/lists/*

COPY . $SRC/hobbes
WORKDIR $SRC/hobbes
COPY ./.clusterfuzzlite/build.sh $SRC/
