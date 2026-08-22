// Fuzz the fregion structured-data file reader (hobbes::fregion::reader).
//
// Opening a file parses its header, page table, and environment records
// (variable bindings and their type descriptions), all taken from the mapped
// image. Structured data files assume a trusted writer, but the reader must
// reject a corrupt or crafted image with an error rather than reading or
// writing out of bounds.
//
// libFuzzer hands us a byte buffer while the reader wants a file path, so
// each input is written to one scratch file that is rewritten per iteration.
//
// The type descriptions in those environment records go through the same
// decoder fuzz-type-decode exercises, and so into the same process-wide type
// memo, which keeps every type it has ever interned until compactMTypeMemory()
// is called. The memo is compacted every few dozen inputs for the reason given
// in fuzz_type_decode.C: otherwise a campaign that mutates type descriptions
// grows by every distinct one it has read.

#include <hobbes/fregion.H>
#include <hobbes/lang/type.H>

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <exception>
#include <stdexcept>
#include <string>

#include <unistd.h>

namespace {

const unsigned inputsPerCompaction = 64;

void compactTypeMemoPeriodically() {
  static unsigned sinceCompaction = 0;
  if (++sinceCompaction >= inputsPerCompaction) {
    sinceCompaction = 0;
    hobbes::compactMTypeMemory();
  }
}

const std::string& scratchPath() {
  static const std::string p = [] {
    const char* dir = getenv("TMPDIR");
    return std::string(dir ? dir : "/tmp") + "/hobbes-fuzz-fregion-" +
           std::to_string(getpid()) + ".db";
  }();
  return p;
}

void writeInput(const uint8_t* data, size_t size) {
  FILE* f = fopen(scratchPath().c_str(), "wb");
  if (f == nullptr) {
    throw std::runtime_error("can't open scratch file: " + scratchPath());
  }
  if (size > 0 && fwrite(data, 1, size, f) != size) {
    fclose(f);
    throw std::runtime_error("short write to scratch file: " + scratchPath());
  }
  fclose(f);
}

} // namespace

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
  try {
    writeInput(data, size);
  } catch (const std::exception& e) {
    // an unwritable scratch file is an environment problem, not a finding --
    // fail loudly rather than letting it look like a fuzzer crash
    fprintf(stderr, "fuzz-fregion-reader: fatal environment error: %s\n", e.what());
    exit(1);
  }
  try {
    hobbes::fregion::reader r(scratchPath());
  } catch (const std::exception&) {
    // rejecting a malformed image is the expected behavior
  }
  compactTypeMemoPeriodically();
  return 0;
}
