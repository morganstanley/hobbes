# Build environment for Anthropic's OSS Scanner (github.com/anthropics/oss-scanner),
# which builds this image with the checkout as the build context, then scans it
# with no network access. Everything the build and the tests need is fetched here.
#
# One build, in /src/build: optimized, with debug info and frame pointers, under
# AddressSanitizer and UndefinedBehaviorSanitizer, and with the fuzzing
# harnesses in fuzz/ built against libFuzzer. Those harnesses cover most of the
# untrusted-input surfaces in .oss-scanner/threat_model.md; the RPC and hi -w
# listeners have none, and are driven through the built hi and hog instead. A
# reproducer is most useful when a sanitizer says what went wrong. One build
# rather than a plain one beside it keeps the image inside the scanner's build
# time limit.

FROM ubuntu:24.04

# LLVM 18 from the distribution rather than apt.llvm.org: it is in the CI build
# matrix (build.yml) and is what the fuzzing builds use, and installing it this
# way needs no third-party repository. libclang-rt-18-dev carries the sanitizer
# and libFuzzer runtimes.
ENV DEBIAN_FRONTEND=noninteractive \
    CC=clang-18 \
    CXX=clang++-18
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates build-essential cmake make git python3 gdb \
      clang-18 llvm-18-dev libclang-18-dev libclang-rt-18-dev \
      libncurses-dev zlib1g-dev libreadline-dev libzstd-dev \
    && rm -rf /var/lib/apt/lists/*
ENV PATH=/usr/lib/llvm-18/bin:$PATH

# Evaluation memory is arena-scoped and reclaimed in bulk, so LeakSanitizer
# reports it as lost by design (doc/en/embedding/compiler.rst); the fuzz
# corpus tests and OSS-Fuzz run with leak detection off for the same reason.
ENV LSAN_OPTIONS=detect_leaks=0 \
    ASAN_OPTIONS=detect_container_overflow=0:detect_stack_use_after_return=1

# scanner contract: the checkout lives inside the image, at /src
COPY . /src
WORKDIR /src

# Release here is -O2 without NDEBUG (CMakeLists.txt sets its flags), so asserts
# stay in, and the unused-variable warnings an NDEBUG build turns into errors
# under -Werror do not arise.
RUN cmake -S /src -B /src/build \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_C_FLAGS="-g -fno-omit-frame-pointer" \
      -DCMAKE_CXX_FLAGS="-g -fno-omit-frame-pointer" \
      -DLLVM_DIR=/usr/lib/llvm-18/lib/cmake/llvm \
      -DUSE_ASAN_AND_UBSAN=ON \
      -DBUILD_FUZZERS=ON \
    && cmake --build /src/build -j"$(nproc)"

# Run the tests (hobbes-test, the Rosetta Code examples, and a replay of every
# checked-in fuzz corpus input) so the image is known to work. The scanner
# wants an image even when a test fails, so by default a failure is only a
# warning; CI builds this same image with STRICT_TESTS=1 (build.yml,
# oss-scanner-image), where a failure fails the build.
ARG STRICT_TESTS=0
RUN cd /src/build && (ctest --output-on-failure -j"$(nproc)" --timeout 900 \
      || { [ "$STRICT_TESTS" = 1 ] && exit 1; echo "WARNING: ctest reported failures"; })
