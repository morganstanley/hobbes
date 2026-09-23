#include <hobbes/hobbes.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/db/file.H>
#include <hobbes/util/region.H>
#include <atomic>
#include <iomanip>
#include <thread>
#include "hobbes/eval/funcdefs.H"
#include "test.H"

#include <sys/stat.h>

using namespace hobbes;
static cc& c() { static __thread cc* x = nullptr; if (x == nullptr) { x = new cc(); } return *x; }

using BufferView = std::pair<const char *, size_t>;

TEST(Compiler, compileToSupportsMoreThanSixArgs) {
  std::string expression =
    "match vegetables spices fruits meat cheese technique occasion skill drink side with \n"
    "| \"carrots\" \"parsley\" \"lemon\" \"ossobuco\" _ \"braising\" \"everyday\" \"basic\" \"wine\" \"polenta\" -> 10 \n"
    "| _ _ _ _ _ _ _ _ _ _ -> -1";

  using MatchFunPtr = int (*)(const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *);  // side
  
  hobbes::cc compiler;

  auto funPtr = hobbes::compileTo<MatchFunPtr>(
    &compiler,
    hobbes::list<std::string>("vegetables", "spices", "fruits",
                              "meat","cheese", "technique",
                              "occasion", "skill", "drink", "side"),
    expression);
  
  EXPECT_TRUE(nullptr != funPtr);
}

TEST(Compiler, charArrExpr) {
  char buffer[256];
  strncpy(buffer, "\"hello world\"", sizeof(buffer));
  const auto* p = c().compileFn<const array<char>*()>(buffer)();
  EXPECT_TRUE(hobbes::makeStdString(p) != "\"hello world\"");
}

class BV { };
namespace hobbes {
  template <>
    struct lift<BV*> : public lift<std::pair<const char*, size_t>*> {
    };
}

TEST(Compiler, liftApathy) {
  // one definition by-ref should be enough to infer the equivalent references
  EXPECT_EQ(show(lift<BV*>::type(c())),       "(<char> * long)");
  EXPECT_EQ(show(lift<const BV*>::type(c())), "(<char> * long)");
  EXPECT_EQ(show(lift<BV&>::type(c())),       "(<char> * long)");
  EXPECT_EQ(show(lift<const BV&>::type(c())), "(<char> * long)");
}

TEST(Compiler, compileFnTypes) {
  EXPECT_EQ(c().compileFn<int(const std::string&)>("_","42")(""), 42);
  EXPECT_EQ(c().compileFn<int(*)(const std::string&)>("_","42")(""), 42);
}

// issue #584: a failed compile must not poison a long-lived compiler
// (these run against the shared compiler on purpose: every later test in this
//  group then also checks that the recovery was complete)
static QualTypePtr intToInt() { return qualtype(functy(list(primty("int")), primty("int"))); }

TEST(Compiler, compileRecoversAfterCodegenFailure) {
  c().forwardDeclare("missingResidual", intToInt()); // type-checks, but there's nothing to call

  EXPECT_EXCEPTION_MSG(c().compileFn<int(int)>("x", "missingResidual(x)"), std::exception, "missingResidual");
  EXPECT_EQ(c().compileFn<int()>("42")(), 42);

  // the same through a function definition and a data definition ...
  EXPECT_EXCEPTION_MSG(c().define("recoverF", "\\x.missingResidual(x)"), std::exception, "missingResidual");
  EXPECT_EXCEPTION_MSG(c().define("recoverG", "missingResidual(1)"), std::exception, "missingResidual");
  EXPECT_EQ(c().compileFn<int()>("42")(), 42);

  // ... and a name whose definition failed can be defined properly afterwards
  c().define("recoverF", "\\x.x+1");
  EXPECT_EQ(c().compileFn<int()>("recoverF(41)")(), 42);
}

TEST(Compiler, compileRecoversAfterResidualDefinitionFailure) {
  c().forwardDeclare("recoverGood", intToInt());

  // 'recoverGood' is accepted into the batch, then 'recoverBad' fails it
  Definitions ds;
  ds.emplace_back("recoverGood", c().readExpr("\\x.x"));
  ds.emplace_back("recoverBad",  c().readExpr("unboundValue"));
  EXPECT_EXCEPTION_MSG(c().drainUnqualifyDefs(ds), std::exception, "unboundValue");

  // later batches (here: 'show' instances) must still be compiled
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>("show([1, 2, 3])")()), "[1, 2, 3]");

  // and nothing of the discarded batch lingers as a half-definition
  EXPECT_FALSE(c().hasValueBinding("recoverGood"));
  c().define("recoverGood", "\\x.x+2");
  EXPECT_EQ(c().compileFn<int()>("recoverGood(40)")(), 42);
}

static int appC(const closure<int(int)>& c) { return c(7); }
TEST(Compiler, liftClosTypes) {
  c().bind("appC", &appC);
  EXPECT_EQ(c().compileFn<int()>("(\\x.appC(\\y.x*y-x))(7)")(), 42);
}

TEST(Compiler, Parsing) {
  EXPECT_TRUE((c().compileFn<bool(const std::pair<char,char>&)>("p", "p==('\\\\','\\\\')")(std::make_pair('\\','\\'))));
}

TEST(Compiler, ParseTyDefStaging) {
  compile(
    &c(),
    c().readModule(
      "bob = 42\n"
      "type BT = (TypeOf `bob` x) => x\n"
      "type BTI = (TypeOf `newPrim()::BT` x) => x\n"
      "frank :: BTI\n"
      "frank = 3\n"
    )
  );
  EXPECT_EQ(c().compileFn<int()>("frank")(), 3);
}

TEST(Compiler, ParameterizedTypeAliasInSignature) {
  // a type application in a signature is written in parentheses; the parser
  // first reads "(Box0 int)" as a constraint list and has to recover the type
  // when no "=>" follows, wherever the application leads the type
  compile(
    &c(),
    c().readModule(
      "type Box0 a = {v:a}\n"
      "b0 :: (Box0 int)\n"
      "b0 = {v=0}\n"
      "unbox0 :: (Box0 int) -> int\n"
      "unbox0 b = b.v\n"
      "pair0 :: (Box0 int) * int\n"
      "pair0 = ({v=1}, 2)\n"
      "boxed0 :: int -> (Box0 int)\n"
      "boxed0 x = {v=x}\n"
      "shown0 :: (Show a) => (Box0 a) -> [char]\n"
      "shown0 b = show(b.v)\n"
    )
  );
  EXPECT_EQ(c().compileFn<int()>("b0.v")(), 0);
  EXPECT_EQ(c().compileFn<int()>("unbox0({v=7})")(), 7);
  EXPECT_EQ(c().compileFn<int()>("pair0.0.v + pair0.1")(), 3);
  EXPECT_EQ(c().compileFn<int()>("boxed0(9).v")(), 9);
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>("shown0({v=42})")()), "42");
}

TEST(Compiler, ParameterizedTypeAliasWithoutArguments) {
  // "b1 :: Box1 int" cannot be an application: newlines are not significant
  // between definitions, so "int" begins the next definition and b1 is
  // declared with the bare alias name, which has to be reported as such
  EXPECT_EXCEPTION_MSG(
    compile(&c(), c().readModule("type Box1 a = {v:a}\nb1 :: Box1\nb1 = {v=0}\n")),
    std::exception,
    "The type alias 'Box1' takes 1 type argument but is used here without any"
  );
  EXPECT_EXCEPTION_MSG(
    compile(&c(), c().readModule("type Box2 a = {v:a}\nb2 :: (Box2 int int)\nb2 = {v=0}\n")),
    std::exception,
    "requires exactly 1 arguments"
  );
  // a constraint list still has to introduce a qualified type
  EXPECT_EXCEPTION_MSG(
    c().readModule("b3 :: (Show a, Show b)\nb3 = 0\n"),
    std::exception,
    "expecting =>"
  );
}

TEST(Compiler, ccInManyThreads) {
  std::vector<std::thread*> ps;
  size_t badChecks = 0;
  for (size_t p = 0; p < 10; ++p) {
    ps.push_back(new std::thread(([&]() {
      hobbes::cc c;
      badChecks += c.compileFn<int()>("sum([1..100])-100*101/2")(); // just 0, but complex enough to hit many areas of the compiler
    })));
  }
  for (auto *p : ps) { p->join(); delete p; }
  EXPECT_EQ(badChecks, size_t(0));
}

using IArr = std::array<int, 10>;
using IMat = std::array<IArr, 10>;

DEFINE_STRUCT(ArrTest,
  (short,  a),
  (IMat,   xss),
  (double, y)
);

TEST(Compiler, liftStdArray) {
  IArr xs;
  for (size_t i = 0; i < xs.size(); ++i) {
    xs[i] = i;
  }
  EXPECT_EQ((c().compileFn<int(IArr*)>("xs","sum(xs[0:])")(&xs)), 45);

  ArrTest atst;
  atst.a = 0;
  for (size_t i = 0; i < atst.xss.size(); ++i) {
    for (size_t j = 0; j < atst.xss[i].size(); ++j) {
      atst.xss[i][j] = i+j;
    }
  }
  atst.y = 3.14159;
  EXPECT_EQ((c().compileFn<int(ArrTest*)>("s","sum(concat([[x|x<-xs[0:]]|xs<-s.xss[0:]]))")(&atst)), 900);
}

using ctimespanT = std::chrono::duration<int64_t, std::micro>;

std::ostream& operator<<(std::ostream& out, ctimespanT dt) {
  out << *reinterpret_cast<int64_t*>(&dt) << "us";
  return out;
}

TEST(Compiler, liftChronoTimespan) {
  EXPECT_EQ(c().compileFn<ctimespanT()>("20ms")(), std::chrono::milliseconds(20));
  EXPECT_TRUE(c().compileFn<bool(ctimespanT)>("x", "x==20ms")(std::chrono::milliseconds(20)));
}

// verify that types are lifted as expected for values without "return value optimization" (or "copy elision")
TEST(Compiler, liftWithoutRVO) {
  using strref = hobbes::fileref<const hobbes::array<char> *>;

  EXPECT_EQ(c().compileFn<strref(int)>("x", "unsafeCast(42L)")(0).index, strref(42UL).index);
}

TEST(Compiler, threadRegionsAreReleasedOnThreadExit) {
  // A thread's scratch region is created on its first hobbes allocation and
  // used to outlive the thread: the __thread pointer to it died with the
  // thread and the region -- every page it had grown to -- became memory
  // nothing could reach or free. That is invisible to a process with pinned
  // evaluation threads, and a steady drip for one that evaluates on a
  // churning thread pool. Regions this machinery makes are now freed by a
  // TSD destructor when their thread exits; regions handed in by address
  // through addThreadRegion stay the caller's to free, exactly as before.
  //
  // The freeing itself is asserted by LeakSanitizer on the Linux fuzz builds
  // (an unfixed build reports one leaked region per departed thread); what
  // is asserted portably here is the behavior around it.

  // allocation works on short-lived threads, repeatedly
  std::atomic<size_t> ok{0};
  for (size_t i = 0; i < 4; ++i) {
    std::thread([&ok]{
      const array<char>* s = makeString("thread-scoped");
      if (s != nullptr && s->size == 13) { ++ok; }
    }).join();
  }
  EXPECT_EQ(ok.load(), size_t(4));

  // a region added by address is not this machinery's to free: it must
  // survive its thread untouched (a cleanup that wrongly freed it would
  // double-free when it is destroyed at scope end below)
  region ext(32768);
  std::atomic<bool> extOk{false};
  std::thread([&]{
    size_t rid = addThreadRegion("ext", &ext);
    size_t old = setThreadRegion(rid);
    char* p = memalloc(64, sizeof(size_t));
    if (p != nullptr) { p[0] = 'x'; p[63] = 'y'; }
    setThreadRegion(old);
    removeThreadRegion(rid);
    extOk = (p != nullptr);
  }).join();
  EXPECT_TRUE(extOk.load());
  EXPECT_TRUE(ext.used() > 0);

  // and the main thread's region is unaffected by other threads coming and going
  EXPECT_TRUE(makeString("still here") != nullptr);
}


TEST(Compiler, destroyingCCReleasesItsTypes) {
  // every type a compiler interns lands in a process-wide memo that only
  // releases entries when compacted, and nothing on the compile path
  // compacts it: a process that compiled a large definition kept its types
  // for good, even after the cc that made them was gone (a 400-row match
  // table pinned ~150MB per compile). A cc now compacts the memo as the last
  // step of its destruction, so what it alone was keeping alive is released.
  std::weak_ptr<MonoType> t;
  {
    cc lc;
    // a type nothing else in the process will have interned
    MonoTypePtr rt = lc.readMonoType("{ccDestroyReleasesTypesF0:int, ccDestroyReleasesTypesF1:[char], ccDestroyReleasesTypesF2:{a:double, b:[byte]}}");
    EXPECT_TRUE(is<Record>(rt));
    t = rt;
    // rt is released here, before lc; a live cc does not by itself free
    // types (they are shared with every other cc), so the entry survives
    // until the compiler goes
  }
  EXPECT_TRUE(t.expired());
}

// issue #586: the module compiler memoizes type-alias expansion by the address of
// the parsed type. That type may be freed once the module is compiled (the type
// memo is compacted whenever a compiler is destroyed), and a later parse can put
// a different type at the same address and be handed the stale expansion.
TEST(Compiler, typeDefStagingSurvivesTypeMemoCompaction) {
  hobbes::cc lc;
  for (int i = 0; i < 64; ++i) {
    const std::string n = str::from(i);
    // aliases whose expansion is a different type than their parse (so the
    // cache does not keep the parsed type alive) and differs per iteration (so
    // a stale expansion is always the wrong one), plus a staged definition
    // that must be expanded from its own parse
    compile(&lc, lc.readModule(
      "type Box" + n + " = {v" + n + ":int}\n"
      "b" + n + " :: Box" + n + "\n"
      "b" + n + " = {v" + n + "=" + n + "}\n"
      "v" + n + " = " + n + "\n"
      "type T" + n + " = (TypeOf `v" + n + "` x) => x\n"
      "f" + n + " :: T" + n + "\n"
      "f" + n + " = " + n + "\n"
    ));
    EXPECT_EQ(lc.compileFn<int()>("f" + n)(), i);
    EXPECT_EQ(lc.compileFn<int()>("b" + n + ".v" + n)(), i);
    compactMTypeMemory(); // as any compiler going away would
  }
}

// Each sizeOf / cppType site is folded by the constraint it carries, not by
// whichever instance of the class is resolved first.
TEST(Compiler, eachSizeOfSiteUsesItsOwnConstraint) {
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>(
    "show((sizeOf::(SizeOf char _)=>_, sizeOf::(SizeOf [:byte|9:] _)=>_, sizeOf::(SizeOf double _)=>_))")()),
    "(1, 9, 8)");
}

TEST(Compiler, eachCPPTypeSiteUsesItsOwnConstraint) {
  auto ds = c().compileFn<const array<char>*()>(
    "(cppType::(CPPType \"A\" int)=>_) ++ \"|\" ++ (cppType::(CPPType \"B\" double)=>_)")();
  std::string d = makeStdString(ds);
  EXPECT_TRUE(d.find("typedef int A;") != std::string::npos);
  EXPECT_TRUE(d.find("typedef double B;") != std::string::npos);
}

// STRFR-433927: resolving a (Process "cmd" p) constraint used to fork+execv
// "cmd" as a side effect of type unification -- merely type-checking an
// expression (compileFn here stands in for hi's ':t', a net REPL prepare(),
// or a web GET /?<expr>) ran the command whether or not the expression was
// ever going to be evaluated. Spawning is now denied unless the embedding
// cc explicitly opts in via enableProcessSpawning with an exact-match
// allowlist.
// spawn()'s result is an opaque 'process' primitive type (procman.C's
// mkPidTy), not a C++-liftable long -- discard it with 'let _ = ... in ()'
// so these tests only need compileFn<void()> to type-check, independent of
// that representation, and can focus on whether resolving the constraint
// throws or not.
static std::string processCmdExpr(const std::string& cmd) {
  return "let x = (spawn() :: (Process \"" + cmd + "\" q) => q) in ()";
}

// a command that exists wherever these tests run and speaks the init protocol
// a spawned hobbes sub-process must speak
//
// this used to be "/usr/bin/true", which is neither: it is absent from a
// sandboxed build (the Nix builds CI runs have no /usr/bin at all), and it
// exits without answering. mock-proc is built beside the test binary for
// exactly this purpose, and Spawn's tests find it the same way.
static std::string spawnableCmd() {
  std::string cmd;
  execPath([&](const std::string& ep) {
    struct stat sb {};
    cmd = ep + "/mock-proc";
    if (stat(cmd.c_str(), &sb) != 0) {
      cmd = ep + "/mock-proc-g";
    }
  });
  // mock-proc takes the seconds it should live for, and requires it to be
  // positive; Spawn's tests pass 2 for the same reason
  return cmd + " 2";
}

TEST(Compiler, processConstraintSpawningDeniedByDefault) {
  hobbes::cc compiler;
  bool threw = false;
  try {
    compiler.compileFn<void()>(processCmdExpr("/usr/bin/true"));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("Process constraint rejected") != std::string::npos);
  }
  EXPECT_TRUE(threw);
}

TEST(Compiler, processConstraintSpawningOptInAllowsExactCommand) {
  const std::string cmd = spawnableCmd();
  hobbes::cc compiler;
  compiler.enableProcessSpawning({cmd});
  // must not throw: the exact allowlisted command is permitted to spawn
  compiler.compileFn<void()>(processCmdExpr(cmd))();
}

TEST(Compiler, processConstraintOptInDoesNotAllowOtherCommands) {
  // negative control: opting in for one command must not open the gate for
  // every command
  hobbes::cc compiler;
  compiler.enableProcessSpawning({spawnableCmd()});
  bool threw = false;
  try {
    compiler.compileFn<void()>(processCmdExpr("/usr/bin/false"));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("Process constraint rejected") != std::string::npos);
  }
  EXPECT_TRUE(threw);
}

// STRFR-433924: pexec/writefile/removefile/openfd/readfile (bin/hi/funcdefs.C)
// and linkTarget/slurpFile (bin/hi/www.C) are process/filesystem primitives
// that hi binds into the same compiler context its unauthenticated net REPL
// and web server share. translateExprWithOpts(Safe) is what makes hi's
// default-on 'option Safe' actually withhold them -- this check is purely
// name-based against the parsed AST (see makeSafe::with(const Var*)) so it
// applies whether or not the names are actually bound, exactly as they would
// be in an expression string arriving over the wire before any binding
// lookup happens.
static void expectSafeRejects(const std::string& fnName, const std::string& expr) {
  bool threw = false;
  try {
    hobbes::translateExprWithOpts(std::vector<std::string>{"Safe"}, c().readExpr(expr));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find(fnName) != std::string::npos);
  }
  EXPECT_TRUE(threw);
}

TEST(Compiler, safeModeDeniesHiProcessAndFileSystemPrimitives) {
  expectSafeRejects("pexec",      "pexec(\"/bin/sh\")");
  expectSafeRejects("writefile",  "writefile(\"/tmp/x\", \"y\")");
  expectSafeRejects("removefile", "removefile(\"/tmp/x\")");
  expectSafeRejects("openfd",     "openfd(\"/tmp/x\", 0)");
  expectSafeRejects("readfile",   "readfile(\"/tmp/x\")");
  expectSafeRejects("fdReadLine", "fdReadLine(3)");
  expectSafeRejects("linkTarget", "linkTarget(\"/tmp/x\")");
  expectSafeRejects("slurpFile",  "slurpFile(\"/tmp/x\")");
}

// 'element' is not denied outright -- Safe rewrites it to elementM, which
// bounds the index. saelem and saacopy are the raw accessors that rewrite is
// built out of, and naming either one directly walks around the check:
// saelem indexes a fixed array with no bound, and saacopy copies a
// caller-chosen number of bytes *into* one. Both must be refused, or the
// rewrite of 'element' is decoration.
TEST(Compiler, safeModeDeniesRawStaticArrayAccessors) {
  // as above, the rewrite is name-based over the parsed AST, so the operands
  // need not be bound -- and deliberately are not, since the obvious way to
  // make a fixed array (newPrim) is itself denied and would be what the
  // rejection named
  expectSafeRejects("saelem",  "saelem(xs, 100000L)");
  expectSafeRejects("saacopy", "saacopy(xs, ys, 100000L)");

  // and the one that makes the checked route lie: elementM bounds an index
  // against size(x), which for [a] is the length field this writes
  expectSafeRejects("unsafeSetLength", "unsafeSetLength(xs, 1000000L)");
}

TEST(Compiler, safeModeStillAllowsCheckedArrayAccess) {
  // the bounds-checked route stays open: 'element' is rewritten to elementM
  // rather than refused, and 'salength' only reports a static length and
  // touches no memory
  // assert the rewrite actually happened rather than merely that nothing
  // threw -- "did not throw" would still hold if the element -> elementM
  // substitution were dropped, which is the regression worth catching
  auto e = hobbes::translateExprWithOpts(std::vector<std::string>{"Safe"},
                                         c().readExpr("element(xs, 1L)"));
  EXPECT_TRUE(hobbes::show(e).find("elementM") != std::string::npos);

  // salength only reports a static length and touches no memory, so it is
  // left alone entirely -- neither refused nor rewritten
  auto l = hobbes::translateExprWithOpts(std::vector<std::string>{"Safe"},
                                         c().readExpr("salength(xs)"));
  EXPECT_TRUE(hobbes::show(l).find("salength") != std::string::npos);
}

// The structured-storage entry points open a caller-named path just as
// writefile does -- writeFileRT is 'new writer(fname)' on whatever string it
// is given (db/bindings.C) -- and were the pair STRFR-433924's remediation
// review found still reachable after the other eight were denied. Verified
// before the fix: under default Safe,
//   let f = (writeFile("/tmp/x.db") :: (file _ {x:int})) in print(1)
// created the file.
TEST(Compiler, safeModeDeniesStructuredFileOpen) {
  expectSafeRejects("writeFile", "writeFile(\"/tmp/x.db\")");
  expectSafeRejects("readFile",  "readFile(\"/tmp/x.db\")");

  // openfd's twin: denying one direction and not the other leaves the same
  // fd-guessing problem, and closefd(5) can shut a descriptor belonging to
  // someone else entirely
  expectSafeRejects("closefd", "closefd(5)");

  // and the dot-prefixed bridges those compile down to, which cannot be
  // parsed from source but can arrive in a serialized AST
  auto la = hobbes::LexicalAnnotation::null();
  for (const auto& n : {".writeFileRT", ".readFileRT"}) {
    EXPECT_EXCEPTION(hobbes::translateExprWithOpts(hobbes::str::strings("Safe"), hobbes::var(n, la)));
  }
}

TEST(Compiler, safeModeStillAllowsOrdinaryExpressions) {
  // negative control: Safe mode's deny-list is name-specific, not a general
  // lockdown -- an unrelated expression must still translate and compile
  auto e = hobbes::translateExprWithOpts(std::vector<std::string>{"Safe"}, c().readExpr("1 + 2"));
  EXPECT_EQ(c().compileFn<int()>(e)(), 3);
}

// newArray's length is a runtime long. A negative one, or one large enough
// that multiplying by the element size wraps, used to produce a small
// allocation carrying the full claimed length in its header -- so every
// later index ran off the buffer (STRFR-433921). The check lives in
// checkedArrayByteSize (funcdefs.C), which generated code calls before it
// allocates; it is not in a header, so it is declared here to be tested
// on its own.
namespace hobbes { long checkedArrayByteSize(long len, long esz); }

TEST(Compiler, newArrayRefusesALengthItCannotAllocate) {
  // the check itself
  EXPECT_EXCEPTION(checkedArrayByteSize(-1, sizeof(int)));
  EXPECT_EXCEPTION(checkedArrayByteSize((1L << 62) + 1, sizeof(long)));
  EXPECT_EQ(checkedArrayByteSize(4, sizeof(int)), long(sizeof(long) + 4 * sizeof(int)));
  EXPECT_EQ(checkedArrayByteSize(0, 0), long(sizeof(long)));

  // an ordinary length still allocates and reports its length through
  // generated code
  EXPECT_EQ(c().compileFn<long()>("let a = newArray(4L) :: [int] in size(a)")(), 4L);

#if !defined(__APPLE__)
  // and a refused one reaches the caller as an exception. Not on macOS: the
  // exception is thrown from a runtime function called by JIT-compiled code,
  // and the JIT's frames carry no unwind information the system unwinder can
  // use, so the throw terminates the process there rather than unwinding to
  // the catch -- which used to take the rest of the suite with it.
  EXPECT_EXCEPTION(c().compileFn<long()>("let a = newArray(-1L) :: [int] in size(a)")());
  EXPECT_EXCEPTION(c().compileFn<long()>(
    "let a = newArray(" + std::to_string((1L << 62) + 1) + "L) :: [long] in size(a)")());
#endif
}
