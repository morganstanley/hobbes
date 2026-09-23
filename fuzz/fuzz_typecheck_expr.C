// Fuzz the compiler front end past the parser: type inference, type-class
// constraint resolution, pattern-match compilation and desugaring, via
// hobbes::cc::unsweetenExpression. Nothing is evaluated.
//
// fuzz_parse_expr stops at readExpr, which is lexing and LALR parsing; a
// well-formed expression is a success for it the moment the parser accepts
// it. This harness carries the same input on into the type checker, which is
// where most of the compiler's logic lives, and asks the same question of
// it: arbitrary source text may be rejected with an exception, but it must
// not crash the process. Whether the *evaluation* of an expression can crash
// is a different question with a different answer -- compiled Hobbes code is
// native code with the host's privileges, so a crash there is the
// expression's, not the compiler's -- and this harness stays clear of it.
// Type checking is not entirely free of code generation, though: resolving
// a constraint can leave residual definitions (type-class instance members,
// for instance) that unsweetenExpression hands to the JIT before returning.
// That code is generated, never run.
//
// Two things distinguish this from the parser harness.
//
// Cost. Parsing is linear in the input; type checking is not. Pattern-match
// compilation and regex determinization are both super-linear in the size of
// the match or regex (see test/Matching.C and the parse-expr corpus for
// inputs that showed it), and unification recurses over the types it builds.
// A fuzzer looking for coverage finds the expensive corners of the input
// space quickly, and then spends the rest of the campaign mutating inputs
// that each take seconds, so that fewer and fewer executions fit in an hour.
// The harness times each input and hands anything over slowInputSeconds() back
// to libFuzzer with a return of -1, which tells it not to add the input to
// the corpus whatever coverage it found. The hard timeout (-timeout, in the
// .options file) still catches an input that never finishes, and stays well
// above the soft limit: what is wanted is that a slow input that *does*
// finish is dropped from the corpus, not reported as a crash. Slow inputs
// are still worth seeing: -report_slow_units, also set in .options to match,
// logs and saves each new slowest unit as it is seen.
//
// State. The parser harness only has to worry about the regex literals that
// readExpr compiles as a side effect. Here every input that gets past the
// parser leaves something behind in the compiler -- interned types in the
// process-wide type memo, memoized instance resolutions, residual
// definitions and the LLVM modules holding them -- and nothing takes it back
// out. So the memo is compacted and the compiler replaced on a schedule, as
// fuzz_parse_expr does, but by input count alone rather than by the presence
// of a quote, since here any input can have grown the compiler. The
// replacement lands inside a timed input, which is why it is done after the
// input has been timed rather than before, and why the first compiler is
// built in LLVMFuzzerInitialize (see the parser harness for the AFL++
// timing budget this avoids). A rebuilt compiler is also the only thing
// that makes findings reproducible: a crash that depends on what an earlier
// input defined cannot be replayed from the one file the fuzzer saves.
//
// How much a window holds was measured on a corpus the fuzzer had grown
// from the seeds (3.7k inputs, mostly near-misses): reading them alone,
// with no type checking, left about 270KB per input in the compiler --
// readExpr compiles matches, regexes and `parse {}` grammars as it reads
// them -- and interned about 400KB per input into the type memo, and
// destroying the compiler and compacting the memo gave all of it back. So
// the two schedules above are the whole story for growth, and a run's peak
// (about 1-1.5GB was seen on that corpus, unsanitized) is a window's worth
// of residue plus the memo between compactions plus whatever one input
// needs while it runs; the engine's RSS limit is the backstop for the last
// of those.
//
// One class of side effect is left in: resolving a (LoadFile "path" t)
// constraint for a readable file opens the file named in the source during
// type checking (writes, connections, remote invocation, process spawning
// and directory listing are all disabled by default on a fresh cc, and this
// harness enables none of them). The path would have to be spelled out in
// the input, so it is reachable only from a seed that names one, and what
// it opens goes through the same reader that fuzz_fregion_reader covers.

#include <hobbes/hobbes.H>

#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <memory>
#include <string>

// Hobbes reclaims evaluation memory by resetting an arena at the end of a
// transaction rather than freeing objects one at a time, so a
// per-allocation leak audit reports transaction-scoped data as lost by
// design. Off from inside the binary so that the policy binds whatever the
// engine or environment; fuzz_parse_expr.C explains why the suppression
// approach did not.
extern "C" int __lsan_is_turned_off() {
  return 1;
}

namespace {

const unsigned long inputsPerCompaction = 64;
const unsigned long inputsPerCompiler   = 1024;

// an input that type checks but takes this long to do it is not kept in the
// corpus; see the note on cost above. Measured per input, so under a
// sanitizer this admits several times less work than it does unsanitized;
// HOBBES_FUZZ_SLOW_SECONDS overrides it for a build that wants a different
// budget (or, set very low, to see the rejection path work).
double slowInputSeconds() {
  static const double s = [] {
    const char* v = std::getenv("HOBBES_FUZZ_SLOW_SECONDS");
    return (v != nullptr && *v != '\0') ? std::strtod(v, nullptr) : 5.0;
  }();
  return s;
}

// Built in the slot's initializer rather than on first use: constructing a
// cc also constructs the LLVM statics it depends on, and static destruction
// runs in reverse order of construction, so a slot filled later would be
// destroyed after those statics and tear the cc down against an LLVM
// context that was already gone.
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
  static unsigned long inputs = 0;
  ++inputs;
  if (inputs % inputsPerCompiler == 0) {
    compilerSlot().reset();
  }
  if (inputs % inputsPerCompaction == 0) {
    // after the compiler is let go, where that happened, so that the types
    // only it was holding are released too
    hobbes::compactMTypeMemory();
  }
}

} // namespace

extern "C" int LLVMFuzzerInitialize(int*, char***) {
  compiler();
  return 0;
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
  std::string src(reinterpret_cast<const char*>(data), size);

  const auto start = std::chrono::steady_clock::now();
  try {
    hobbes::cc& c = compiler();
    c.unsweetenExpression(c.readExpr(src));
  } catch (const std::exception&) {
    // rejecting source that does not parse or type check is the expected
    // behavior
  }
  const std::chrono::duration<double> elapsed = std::chrono::steady_clock::now() - start;

  // outside the timed region: a compiler rebuild is the harness's cost, not
  // the input's
  reclaimPeriodically();

  return elapsed.count() > slowInputSeconds() ? -1 : 0;
}
