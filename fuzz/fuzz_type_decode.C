// Fuzz the binary MonoType decoder (hobbes::decode in lang/type.C).
//
// This decoder backs the type descriptions exchanged by the RPC layer
// (ipc/net.C), where it runs on bytes from an unauthenticated peer before any
// expression is accepted. A malformed or truncated buffer must throw, never
// read out of bounds.

#include <hobbes/lang/type.H>

#include <cstddef>
#include <cstdint>
#include <exception>

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
  try {
    hobbes::decode(data, data + size);
  } catch (const std::exception&) {
    // rejecting malformed input is the expected behavior
  }
  return 0;
}
