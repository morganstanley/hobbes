#include <hobbes/hobbes.H>
#include <hobbes/util/codec.H>
#include "test.H"

#include <cstring>
#include <unistd.h>

namespace hobbes { long fdReadBlockLen(int); }

using namespace hobbes;

namespace {
int pipeContaining(const void* data, size_t size) {
  int fds[2];
  if (pipe(fds) != 0) {
    throw std::runtime_error("failed to make test pipe");
  }
  if (size != 0 && write(fds[1], data, size) != static_cast<ssize_t>(size)) {
    close(fds[0]);
    close(fds[1]);
    throw std::runtime_error("failed to write test pipe");
  }
  close(fds[1]);
  return fds[0];
}

void expectBlockLengthRejected(const void* data, size_t size, const std::string& reason) {
  const int fd = pipeContaining(data, size);
  bool rejected = false;
  try {
    static_cast<void>(fdReadBlockLen(fd));
  } catch (const std::exception& ex) {
    rejected = true;
    EXPECT_TRUE(std::string(ex.what()).find(reason) != std::string::npos);
  }
  EXPECT_TRUE(rejected);
  EXPECT_TRUE(unmarkBadFD(fd));
  close(fd);
}
}

TEST(Codec, BlockArrayLengthIsBoundedAndComplete) {
  const long ordinary = 3;
  int fd = pipeContaining(&ordinary, sizeof(ordinary));
  EXPECT_EQ(fdReadBlockLen(fd), ordinary);
  EXPECT_FALSE(unmarkBadFD(fd));
  close(fd);

  const long negative = -1;
  expectBlockLengthRejected(&negative, sizeof(negative), "out of range");

  const long overCap = (1L << 28) + 1;
  expectBlockLengthRejected(&overCap, sizeof(overCap), "out of range");

  const long partial = 7;
  expectBlockLengthRejected(&partial, sizeof(partial) / 2, "truncated");
  expectBlockLengthRejected(nullptr, 0, "truncated");
}

TEST(Codec, BlockArrayStillDecodesValidInput) {
  const long n = 3;
  const unsigned char payload[] = {4, 5, 6};
  int fds[2];
  EXPECT_EQ(pipe(fds), 0);
  fdwrite(fds[1], reinterpret_cast<const char*>(&n), sizeof(n));
  fdwrite(fds[1], payload, sizeof(payload));
  close(fds[1]);

  cc compiler;
  auto readBytes = compiler.compileFn<const array<unsigned char>*(int)>(
    "fd", "readFrom(fd) :: [byte]");
  const array<unsigned char>* result = readBytes(fds[0]);
  EXPECT_EQ(result->size, static_cast<size_t>(n));
  EXPECT_TRUE(std::memcmp(result->data, payload, sizeof(payload)) == 0);
  close(fds[0]);
}
