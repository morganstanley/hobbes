// Fuzz the lexer and LALR parser via hobbes::cc::readExpr.
//
// Evaluating Hobbes code is running trusted native code, but *reading* source
// text sits on the near side of that trust boundary: lexing and parsing must
// be safe on arbitrary bytes. This harness only parses -- nothing is
// compiled or evaluated.

#include <hobbes/hobbes.H>

#include <cstddef>
#include <cstdint>
#include <exception>
#include <string>

namespace {

hobbes::cc& compiler() {
  static hobbes::cc c;
  return c;
}

} // namespace

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
  std::string src(reinterpret_cast<const char*>(data), size);
  try {
    compiler().readExpr(src);
  } catch (const std::exception&) {
    // rejecting malformed source is the expected behavior
  }
  return 0;
}
