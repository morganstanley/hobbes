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

// Hobbes reclaims evaluation memory by resetting an arena at the end of a
// transaction, not by releasing objects one at a time -- so a per-allocation
// leak audit reports transaction-scoped data as lost by design. For this
// target that means two families of expected reports: allocations made by
// grammar actions during a parse (yyparse and the paths into it), which the
// compiler's bootstrap parse strands once per cc, and a little that LLVM
// keeps for itself. The .options file ships detect_leaks=0 for exactly this
// reason. ClusterFuzz's libFuzzer and AFL engines apply that file when they
// replay a testcase; its honggfuzz engine does not, so on the honggfuzz job
// every replay ends in an at-exit leak report. A leak report is a crash, and
// for a Stack-overflow testcase progression accepts any crash as "still
// reproduces" (it compares crash state for the other types, but a stack
// overflow's state is too unstable to compare), so a fixed stack overflow
// found by honggfuzz stays open for as long as the bootstrap leaks are
// reported. OSS-Fuzz 549863810 and 556791547 sat that way.
//
// An earlier version of this file named the expected stacks in
// __lsan_default_suppressions, to keep the checker alive for everything
// else. That does not work where it is needed: ClusterFuzz runs every target
// with symbolize=0 and symbolizes offline, and with symbolization off LSan
// has no function names to match `leak:yyparse` against, so the suppressions
// matched locally and never on a bot. Turning the checker off is the only
// form of the policy that is binding whatever the engine or environment;
// nothing is given up, since the shipped .options already turns it off for
// the engines that honor it.
extern "C" int __lsan_is_turned_off() {
  return 1;
}

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
