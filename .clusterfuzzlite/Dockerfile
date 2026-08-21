# Build environment for ClusterFuzzLite, which fuzzes the harnesses in fuzz/ on
# every pull request. It matches the OSS-Fuzz image (projects/hobbes/Dockerfile
# in google/oss-fuzz) except that the source is copied in from the checkout
# being tested rather than cloned from main.

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

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        cmake make python3 zip \
        zlib1g-dev libncurses-dev libzstd-dev libreadline-dev \
        wget gnupg lsb-release software-properties-common

RUN wget -q -O /tmp/llvm.sh \
      "https://raw.githubusercontent.com/opencollab/llvm-jenkins.debian.net/${LLVM_SH_COMMIT}/llvm.sh" && \
    echo "${LLVM_SH_SHA256}  /tmp/llvm.sh" | sha256sum -c -

# The installer decides whether this distribution is supported by asking
# apt.llvm.org for its repository index, and reports a request that did not
# come back as an unsupported distribution -- so a single dropped request fails
# the build with a message about the distribution rather than about the
# network. Retry before believing it.
RUN for i in 1 2 3; do \
      if bash /tmp/llvm.sh "$HOBBES_LLVM_VERSION"; then exit 0; fi; \
      echo "llvm.sh failed (attempt $i), retrying"; \
      sleep 15; \
    done; \
    exit 1

RUN apt-get install -y --no-install-recommends llvm-$HOBBES_LLVM_VERSION-dev && \
    rm -f /tmp/llvm.sh && \
    rm -rf /var/lib/apt/lists/*

COPY . $SRC/hobbes
WORKDIR $SRC/hobbes
COPY ./.clusterfuzzlite/build.sh $SRC/
