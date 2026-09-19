// Fuzz the hog collector's transaction deserializer: the JIT-compiled
// HStoreRead instances in bin/hog/boot/read.hob, which read attacker-supplied
// length fields out of a storage::Transaction and feed them to
// hstoreUnsafeReadFixedArray / newArray (see STRFR-433920). This is a fourth
// untrusted-input surface -- hog's live "hog -s <port>" transaction stream --
// that doc/en/security.rst does not yet name alongside the RPC type-decoder
// and the fregion file reader, and none of the other harnesses here reach it.
//
// storage::Transaction has a constructor built for exactly this: "a
// non-persistent transaction over constant data (just used by remote
// consumers)" (include/hobbes/storage.H). That gives a zero-copy view over
// the fuzz input with no sockets or files involved.
//
// Setup (compiler + boot code + one compiled reader per exercised element
// type) runs once, lazily, on the first input; each iteration after that is
// just a Transaction view plus a handful of native calls through
// already-JITed function pointers, so this should run about as fast as any
// harness here despite depending on the JIT.

#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/eval/cc.H>

#include "../bin/hog/hstore_bridge.H"
#include "../bin/hog/boot/gen/boot.H" // hog::compileBootCode

#include <cstddef>
#include <cstdint>
#include <exception>
#include <string>
#include <vector>

using namespace hobbes;

namespace {

using ReadFn = void (*)(storage::Transaction *);

// One compiled reader per shape named in STRFR-433920's three flaws:
//  - a FixedWidth element wider than a byte (the elements-vs-bytes
//    confusion; already patched for this one case in commit 9754380, kept
//    here as a regression check)
//  - a FixedWidth element exactly one byte wide (the signed/unsigned
//    wraparound in Transaction::canRead, still open)
//  - a non-FixedWidth element (the unguarded newArray(n) path at
//    bin/hog/boot/read.hob:139, still open)
const std::vector<ReadFn> &readers() {
  static const std::vector<ReadFn> fns = [] {
    static cc c; // cc's own constructor already loads the core prelude
    c.bind("hstoreCanRead", &hog::hstoreCanRead);
    c.bind("hstoreUnsafeRead", &hog::hstoreUnsafeRead);
    c.bind("hstoreUnsafeReadFixedArray", &hog::hstoreUnsafeReadFixedArray);
    hog::compileBootCode(c);

    auto compileDiscard = [&](const char *ty) {
      // ignore both the success and failure arm -- we only care whether
      // *reading* corrupts memory, not about persisting a value
      return c.compileFn<void(storage::Transaction *)>(
          "txn", std::string("either(hstoreRead(txn) :: (()+") + ty +
                     "), (), \\_.())");
    };

    return std::vector<ReadFn>{
        compileDiscard("[long]"),    // FixedWidth element, width 8
        compileDiscard("[byte]"),    // FixedWidth element, width 1
        compileDiscard("[[byte]]"),  // element [byte] is not FixedWidth
                                      // (dynamic arrays never are) -> the
                                      // outer array takes the unguarded
                                      // newArray(n) path, read.hob:138-139
    };
  }();
  return fns;
}

} // namespace

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  const auto &fns = readers(); // first call pays the one-time JIT cost

  try {
    storage::Transaction txn(data, size);
    // mirrors the dispatch loop in bin/hog/session.C's initStorageSession:
    // read a uint32 id, run the reader picked out for it, repeat
    while (txn.canRead(sizeof(uint32_t))) {
      uint32_t id = *txn.read<uint32_t>();
      fns[id % fns.size()](&txn);
    }
  } catch (const std::exception &) {
    // malformed input rejected cleanly -- expected, not a finding
  }

  return 0;
}
