#ifndef HOG_LIMITS_H_INCLUDED
#define HOG_LIMITS_H_INCLUDED

#include <cstdint>
#include <stdexcept>
#include <string>

namespace hog {

// The largest compressed frame or inflated segment accepted by batchrecv.
// This stays bounded because both forms are held in memory while a batch is
// validated, and the inflated data is also copied into a transaction buffer.
constexpr uint64_t defaultMaxSegmentBytes = 1ull * 1024 * 1024 * 1024;

class InflatedByteCounter {
public:
  explicit InflatedByteCounter(uint64_t maxBytes) : totalBytes(0), maxBytes(maxBytes) {}

  void add(uint64_t bytes) {
    // Subtract before comparing so an extreme runtime setting cannot make the
    // cumulative count wrap and bypass the limit.
    if (bytes > this->maxBytes - this->totalBytes) {
      throw std::runtime_error("gzip segment inflated past " + std::to_string(this->maxBytes) + " bytes");
    }
    this->totalBytes += bytes;
  }

private:
  uint64_t totalBytes;
  uint64_t maxBytes;
};

}

#endif
