#include "test.H"

#include "../bin/hog/config.H"
#include "../bin/hog/netconnection.H"

#include <cstring>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

class BufferConnection : public hog::NetConnection {
public:
  explicit BufferConnection(const std::string& body) : pos(0) {
    const uint64_t size = body.size();
    this->data.resize(sizeof(size) + body.size());
    memcpy(this->data.data(), &size, sizeof(size));
    memcpy(this->data.data() + sizeof(size), body.data(), body.size());
  }

  bool send(const void*, size_t) override { return false; }
  bool sendFile(int) override { return false; }

  bool receive(void* dst, size_t size) override {
    if (size > this->data.size() - this->pos) {
      return false;
    }
    memcpy(dst, this->data.data() + this->pos, size);
    this->pos += size;
    return true;
  }

  std::string remoteHost() override { return "test"; }
  int remotePort() override { return 0; }

private:
  std::vector<uint8_t> data;
  size_t pos;
};

}

TEST(Hog, ReceiveLimitDefaultsToOneGiB) {
  const char* argv[] = {"hog", "-s", "1234"};
  const auto mode = hog::config(3, argv);
  EXPECT_EQ(mode.maxSegmentBytes, 1ull * 1024 * 1024 * 1024);
}

TEST(Hog, ReceiveLimitCanBeSetAtRuntime) {
  const char* argv[] = {"hog", "-s", "1234", "--max-segment-size", "2GB"};
  const auto mode = hog::config(5, argv);
  EXPECT_EQ(mode.maxSegmentBytes, 2ull * 1024 * 1024 * 1024);
}

TEST(Hog, ReceiveLimitMustBePositive) {
  const char* argv[] = {"hog", "-s", "1234", "--max-segment-size", "0"};
  EXPECT_EXCEPTION_MSG(hog::config(5, argv), std::runtime_error, "must be greater than zero");
}

TEST(Hog, ReceiveLimitIsOnlyValidForReceiver) {
  const char* argv[] = {"hog", "--max-segment-size", "2GB"};
  EXPECT_EXCEPTION_MSG(hog::config(3, argv), std::runtime_error, "only valid in receive mode");
}

TEST(Hog, FrameAtRuntimeLimitIsAccepted) {
  BufferConnection connection("abc");
  std::vector<uint8_t> result;
  hog::receiveIntoBuffer(connection, &result, 3);
  EXPECT_EQ(std::string(result.begin(), result.end()), std::string("abc"));
}

TEST(Hog, FramePastRuntimeLimitIsRejected) {
  BufferConnection connection("abc");
  std::vector<uint8_t> result;
  EXPECT_EXCEPTION_MSG(
    hog::receiveIntoBuffer(connection, &result, 2),
    std::runtime_error,
    "3 byte frame (max 2)"
  );
}

TEST(Hog, InflatedDataUsesRuntimeLimit) {
  hog::InflatedByteCounter counter(3);
  counter.add(2);
  counter.add(1);
  EXPECT_EXCEPTION_MSG(counter.add(1), std::runtime_error, "inflated past 3 bytes");
}
