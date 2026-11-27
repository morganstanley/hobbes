#include "test.H"

#include "../bin/hog/config.H"
#include "../bin/hog/batchrecv.H"
#include "../bin/hog/netconnection.H"
#include "../bin/hog/session.H"

#include <chrono>
#include <cerrno>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <future>
#include <stdexcept>
#include <string>
#include <vector>

#include <unistd.h>
#include <zlib.h>

namespace {

class TempDir {
public:
  explicit TempDir(const char* pattern) {
    std::vector<char> tmp(pattern, pattern + strlen(pattern) + 1);
    const char* result = mkdtemp(tmp.data());
    if (result == nullptr) {
      throw std::runtime_error("failed to create test directory");
    }
    this->path = result;
  }

  ~TempDir() {
    std::error_code ignored;
    std::filesystem::remove_all(this->path, ignored);
  }

  const std::string& str() const { return this->path; }

private:
  std::string path;
};

class BufferConnection : public hog::NetConnection {
public:
  explicit BufferConnection(const std::string& body) : BufferConnection(frame(body)) {}

  explicit BufferConnection(std::vector<uint8_t> data) : data(std::move(data)), pos(0) {}

  static std::vector<uint8_t> frame(const std::string& body) {
    const uint64_t size = body.size();
    std::vector<uint8_t> result(sizeof(size) + body.size());
    memcpy(result.data(), &size, sizeof(size));
    memcpy(result.data() + sizeof(size), body.data(), body.size());
    return result;
  }

  bool send(const void*, size_t) override { return false; }
  bool sendFile(int) override { return false; }

  bool receive(void* dst, size_t size) override {
    if (size > this->data.size() - this->pos) {
      errno = 0;
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

class ClosingConnection : public hog::NetConnection {
public:
  explicit ClosingConnection(std::promise<void>* closed) : closed(closed) {}
  ~ClosingConnection() override { this->closed->set_value(); }

  bool send(const void*, size_t) override { return false; }
  bool sendFile(int) override { return false; }
  bool receive(void*, size_t) override { errno = 0; return false; }
  std::string remoteHost() override { return "test"; }
  int remotePort() override { return 0; }

private:
  std::promise<void>* closed;
};

template <typename T>
void appendValue(std::vector<uint8_t>* bytes, const T& value) {
  const auto* p = reinterpret_cast<const uint8_t*>(&value);
  bytes->insert(bytes->end(), p, p + sizeof(value));
}

template <typename C>
void appendSized(std::vector<uint8_t>* bytes, const C& value) {
  appendValue(bytes, value.size());
  bytes->insert(bytes->end(), value.begin(), value.end());
}

void appendFrame(std::vector<uint8_t>* bytes, const std::vector<uint8_t>& body) {
  const uint64_t size = body.size();
  appendValue(bytes, size);
  bytes->insert(bytes->end(), body.begin(), body.end());
}

std::vector<uint8_t> zlibCompress(const std::vector<uint8_t>& input) {
  uLongf size = compressBound(input.size());
  std::vector<uint8_t> result(size);
  const int rc = compress2(result.data(), &size, input.data(), input.size(), Z_BEST_SPEED);
  if (rc != Z_OK) {
    throw std::runtime_error("test compression failed");
  }
  result.resize(size);
  return result;
}

bool processMapsContain(const std::string& path) {
#if defined(__linux__)
  std::ifstream maps("/proc/self/maps");
  std::string line;
  while (std::getline(maps, line)) {
    if (line.find(path) != std::string::npos) {
      return true;
    }
  }
#else
  (void)path;
#endif
  return false;
}

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

TEST(Hog, OversizedFirstSegmentDoesNotCreateStorage) {
  TempDir tmpDir("/tmp/hobbes-hog-limit-XXXXXX");

  std::vector<uint8_t> input;
  appendFrame(&input, std::vector<uint8_t>{'g'});

  std::vector<uint8_t> init;
  appendValue(&init, uint32_t{0});
  appendValue(&init, uint32_t{0});
  appendValue(&init, size_t{0});
  appendFrame(&input, zlibCompress(init));

  // Only the length is needed: receiveIntoBuffer rejects it before reading
  // a body or allocating a storage session.
  appendValue(&input, uint64_t{65});

  auto* sessions = hog::makeSessionGroup(false);
  hog::runRecvConnection(
    sessions,
    std::make_unique<BufferConnection>(std::move(input)),
    tmpDir.str() + "/$GROUP/",
    64
  );
  hog::destroySessionGroup(sessions);

  EXPECT_TRUE(std::filesystem::is_empty(tmpDir.str()));
}

TEST(Hog, CompletedConnectionWorkerReleasesConnection) {
  auto* sessions = hog::makeSessionGroup(false);
  std::promise<void> closed;
  auto closedFuture = closed.get_future();

  hog::startRecvConnection(sessions, std::make_unique<ClosingConnection>(&closed), ".", 64);
  EXPECT_TRUE(closedFuture.wait_for(std::chrono::seconds(2)) == std::future_status::ready);

  hog::destroySessionGroup(sessions);
}

TEST(Hog, ClosedConnectionUnmapsItsStorageSession) {
  TempDir tmpDir("/tmp/hobbes-hog-session-XXXXXX");

  std::vector<uint8_t> input;
  appendFrame(&input, std::vector<uint8_t>{'g'});

  std::vector<uint8_t> init;
  appendValue(&init, static_cast<uint32_t>(hobbes::storage::Reliable));
  appendValue(&init, static_cast<uint32_t>(hobbes::storage::AutoCommit));
  appendValue(&init, size_t{1});

  hobbes::ty::bytes type;
  hobbes::encode(hobbes::primty("int"), &type);
  appendSized(&init, std::string("x"));
  appendValue(&init, uint64_t{0});
  appendSized(&init, std::string());
  appendSized(&init, std::string());
  appendValue(&init, uint32_t{0});
  appendValue(&init, uint32_t{0});
  appendSized(&init, type);
  appendFrame(&input, zlibCompress(init));

  // A zero-byte transaction is still a transaction, so this creates the
  // storage session without requiring a statement payload.
  std::vector<uint8_t> data;
  appendValue(&data, uint64_t{0});
  appendFrame(&input, zlibCompress(data));

  auto* sessions = hog::makeSessionGroup(false);
  hog::runRecvConnection(
    sessions,
    std::make_unique<BufferConnection>(std::move(input)),
    tmpDir.str() + "/$GROUP/",
    1024
  );
  hog::destroySessionGroup(sessions);

  EXPECT_TRUE(!std::filesystem::is_empty(tmpDir.str()));
  EXPECT_TRUE(!processMapsContain(tmpDir.str()));
}
