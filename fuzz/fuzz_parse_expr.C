// Fuzz the lexer and LALR parser via hobbes::cc::readExpr.
//
// Evaluating Hobbes code is running trusted native code, but *reading* source
// text sits on the near side of that trust boundary: lexing and parsing must
// be safe on arbitrary bytes. Nothing here is evaluated.
//
// Reading is not entirely free of compilation, though, and the difference
// shows up over a campaign rather than on any one input. A regex literal is
// turned into a matching function where it is read: makeRegexFn determinizes
// it and defines the result in the compiler under a fresh name, and the types
// that takes are interned in the process-wide type memo (tctorMaps in
// lang/type.C), which holds a reference of its own to each of them. Nothing
// removes the definition, and only compactMTypeMemory() lets go of the types.
// So a `cc` reused across inputs, with the memo left alone, grows by every
// regex it has ever read: measured at about 168KB per read of the twenty
// character regex in OSS-Fuzz testcase 4850385077207040 -- roughly half of
// it in the memo and half in the compiler -- against no growth that can be
// measured at all for an input with no regex in it. Left alone, a campaign
// feeding regexes runs the process out of memory. That is what the testcase
// reports: an out-of-memory that ClusterFuzz could not minimize, because no
// single input causes it.
//
// So two things are done periodically, one for each half. The type memo is
// compacted every few dozen inputs, as the decoder harnesses do; that gives
// back the memo's half and costs about as much as a parse. The compiler is
// replaced every thousand or so; that gives back its half, and costs the
// fraction of a second it takes to build one, amortised over enough inputs
// not to show. Neither alone is enough: each leaves the other half growing
// without bound. Counts of inputs are a coarse stand-in for how much has
// accumulated, but a portable and predictable one -- resident size is not,
// because freeing memory does not hand it back to the operating system, so a
// harness that watched RSS rebuilt the compiler on every input once it first
// went over.

#include <hobbes/hobbes.H>

#include <cstddef>
#include <cstdint>
#include <exception>
#include <memory>
#include <string>

// Leak detection is off for this target as a matter of policy, not oversight:
// parsing allocates from arenas that are not reclaimed per iteration, the
// compiler's bootstrap parse strands a few grammar allocations, and LLVM
// leaves a little of its own -- all documented in fuzz/README.md, and encoded
// as detect_leaks=0 in the .options file this target ships with. ClusterFuzz's
// progression task replays old testcases without honoring that file, so an
// at-exit leak report can pin a long-fixed crash at "still reproduces"
// (OSS-Fuzz 549863810 sat that way: the stack overflow it reports was fixed,
// and the "crash" its progression kept seeing was LeakSanitizer complaining
// about the bootstrap allocations after a clean run). LSan consults this hook
// before its at-exit check, so defining it makes the policy binding wherever
// the binary runs, whatever environment it is run with.
extern "C" int __lsan_is_turned_off() { return 1; }

namespace {

const unsigned long inputsPerCompaction = 64;
const unsigned long inputsPerCompiler   = 1024;

// The first compiler is built in the initializer rather than on first use.
// Constructing a cc also constructs the LLVM statics it depends on, and at
// exit everything static is destroyed in reverse order of construction: a slot
// that was registered empty and filled afterwards would be destroyed after
// those statics, and the cc inside it would tear down against an LLVM context
// that was already gone.
std::unique_ptr<hobbes::cc>& compilerSlot() {
  static std::unique_ptr<hobbes::cc> c(new hobbes::cc());
  return c;
}

hobbes::cc& compiler() {
  std::unique_ptr<hobbes::cc>& c = compilerSlot();
  if (!c) {
    c = std::unique_ptr<hobbes::cc>(new hobbes::cc());
  }
  return *c;
}

void reclaimPeriodically() {
  static unsigned long read = 0;
  ++read;
  if (read % inputsPerCompiler == 0) {
    compilerSlot().reset();
  }
  if (read % inputsPerCompaction == 0) {
    // after the compiler is let go, where that happened, so that the types
    // only it was holding are released too
    hobbes::compactMTypeMemory();
  }
}

} // namespace

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
  std::string src(reinterpret_cast<const char*>(data), size);
  try {
    compiler().readExpr(src);
  } catch (const std::exception&) {
    // rejecting malformed source is the expected behavior
  }
  reclaimPeriodically();
  return 0;
}
