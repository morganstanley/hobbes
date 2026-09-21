// Fuzz the binary MonoType decoder (hobbes::decode in lang/type.C).
//
// This decoder backs the type descriptions exchanged by the RPC layer
// (ipc/net.C), where it runs on bytes from an unauthenticated peer before any
// expression is accepted. A malformed or truncated buffer must throw, never
// read out of bounds.
//
// Decoding is not free of side effects: every type it builds is interned in
// the process-wide type memo (tctorMaps in lang/type.C), which holds a
// reference of its own, and only compactMTypeMemory() lets go of the entries
// nothing else refers to. A decoder that reads a fresh type per input and
// never compacts therefore grows by every type it has ever read -- measured
// at about 380 bytes per distinct type -- and a campaign runs the process out
// of memory with no one input to blame. That is what OSS-Fuzz testcase
// 6130308650172416 reports: an out-of-memory in this target whose reproducer
// is empty.
//
// Compacting keeps the memo at its resting size. It costs about as much as a
// decode does, so it is done every few dozen inputs rather than every one:
// what accumulates in between is a few dozen types, and the fuzzer keeps its
// throughput.

#include <hobbes/lang/type.H>

#include <cstddef>
#include <cstdint>
#include <exception>

namespace {

const unsigned inputsPerCompaction = 64;

void compactTypeMemoPeriodically() {
  static unsigned sinceCompaction = 0;
  if (++sinceCompaction >= inputsPerCompaction) {
    sinceCompaction = 0;
    hobbes::compactMTypeMemory();
  }
}

} // namespace

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
  try {
    hobbes::decode(data, data + size);
  } catch (const std::exception&) {
    // rejecting malformed input is the expected behavior
  }
  compactTypeMemoPeriodically();
  return 0;
}
