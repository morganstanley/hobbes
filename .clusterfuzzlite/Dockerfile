# Build environment for ClusterFuzzLite, which fuzzes the harnesses in fuzz/ on
# every pull request. It matches the OSS-Fuzz image (projects/hobbes/Dockerfile
# in google/oss-fuzz) except that the source is copied in from the checkout
# being tested rather than cloned from main.

FROM gcr.io/oss-fuzz-base/base-builder:v1

# hobbes is built on LLVM, which the base image does not provide as a
# development package. 18 is covered by the build matrix in build.yml.
ENV HOBBES_LLVM_VERSION=18

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        cmake make python3 zip \
        zlib1g-dev libncurses-dev libzstd-dev libreadline-dev \
        wget gnupg lsb-release software-properties-common && \
    wget -qO- https://apt.llvm.org/llvm.sh | bash -s -- $HOBBES_LLVM_VERSION && \
    apt-get install -y --no-install-recommends llvm-$HOBBES_LLVM_VERSION-dev && \
    rm -rf /var/lib/apt/lists/*

COPY . $SRC/hobbes
WORKDIR $SRC/hobbes
COPY ./.clusterfuzzlite/build.sh $SRC/
