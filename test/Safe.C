// Safe mode: the name-based deny-list translateExprWithOpts(Safe) applies to
// what hi's unauthenticated net REPL and web server compile, and the session
// options that carry it into the modules those load.

#include <hobbes/hobbes.H>
#include "test.H"

#include <fstream>
#include <sys/stat.h>
#include <unistd.h>

using namespace hobbes;
static cc& c() { static __thread cc* x = nullptr; if (x == nullptr) { x = new cc(); } return *x; }

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

TEST(Safe, safeModeDeniesHiProcessAndFileSystemPrimitives) {
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
TEST(Safe, safeModeDeniesRawStaticArrayAccessors) {
  // as above, the rewrite is name-based over the parsed AST, so the operands
  // need not be bound -- and deliberately are not, since the obvious way to
  // make a fixed array (newPrim) is itself denied and would be what the
  // rejection named
  expectSafeRejects("saelem",  "saelem(xs, 100000L)");
  expectSafeRejects("saacopy", "saacopy(xs, ys, 100000L)");
  // the same raw accessor for the <std.string> Array instance (STRFR-434029)
  expectSafeRejects("stdstrelem", "stdstrelem(xs, 100000L)");
  expectSafeRejects("cstrelem", "cstrelem(xs, 100000L)");

  // and the one that makes the checked route lie: elementM bounds an index
  // against size(x), which for [a] is the length field this writes
  expectSafeRejects("unsafeSetLength", "unsafeSetLength(xs, 1000000L)");
}

TEST(Safe, safeModeStillAllowsCheckedArrayAccess) {
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

  // denying the raw accessors must not close the checked route for the
  // instances they back: indexing a <char> C string still rewrites to the
  // bounds-checked elementM (built on cstrelem), it is not refused along with
  // the raw accessor
  auto s = hobbes::translateExprWithOpts(std::vector<std::string>{"Safe"},
                                         c().readExpr("element(\"hello\", 1L)"));
  EXPECT_TRUE(hobbes::show(s).find("elementM") != std::string::npos);
}

// The structured-storage entry points open a caller-named path just as
// writefile does -- writeFileRT is 'new writer(fname)' on whatever string it
// is given (db/bindings.C) -- and were the pair STRFR-433924's remediation
// review found still reachable after the other eight were denied. Verified
// before the fix: under default Safe,
//   let f = (writeFile("/tmp/x.db") :: (file _ {x:int})) in print(1)
// created the file.
TEST(Safe, safeModeDeniesStructuredFileOpen) {
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

TEST(Safe, safeModeStillAllowsOrdinaryExpressions) {
  // negative control: Safe mode's deny-list is name-specific, not a general
  // lockdown -- an unrelated expression must still translate and compile
  auto e = hobbes::translateExprWithOpts(std::vector<std::string>{"Safe"}, c().readExpr("1 + 2"));
  EXPECT_EQ(c().compileFn<int()>(e)(), 3);
}

// A module's definitions are translated with that module's own options, and a
// module almost never declares 'option Safe' of its own. So an application
// compiling expressions under Safe got no filtering at all on the modules it
// loaded (STRFR-434027), nor on the modules those imported (STRFR-434017,
// STRFR-434009) -- putting the work in a file was a way around the option the
// operator chose.
//
// The transitive-closure check already refused a *call* to such a definition
// from a Safe prompt, so this is the compile-time half of that: the definition
// is now refused where it is loaded rather than accepted and caught later.
TEST(Safe, safeModeAppliesToLoadedModules) {
  hobbes::cc lc;
  lc.setModuleOptions(std::vector<std::string>{"Safe"});

  // a module defining something in terms of a denied primitive is refused at
  // load, naming the primitive
  bool threw = false;
  try {
    hobbes::compile(&lc, lc.readModule("module m where\nsneak = \\x.pexec(\"/bin/sh\")\n"));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("pexec") != std::string::npos);
  }
  EXPECT_TRUE(threw);

  // ... and an ordinary module still loads and is usable
  hobbes::compile(&lc, lc.readModule("module ok where\ndbl = \\x.x*2\n"));
  EXPECT_EQ(lc.compileFn<int()>("dbl(21)")(), 42);
}

// The case that depends on the seed being inside compile() rather than at the
// call site: import() re-enters compile() for each imported script, so a
// module that imports another cannot launder the denial through it
// (STRFR-434017, STRFR-434009). Seeding only the top-level module would pass
// the test above but fail this one.
TEST(Safe, safeModeFollowsImportsBetweenModules) {
  std::string dir = "/tmp/hobbes-safe-import." + hobbes::str::from(getpid());
  ::mkdir(dir.c_str(), 0700);
  { std::ofstream f((dir + "/inner.hob").c_str()); f << "sneakI = \\x.pexec(\"/bin/sh\")\n"; }
  { std::ofstream f((dir + "/outer.hob").c_str()); f << "import inner\nviaImport = \\x.sneakI(x)\n"; }

  hobbes::cc lc;
  lc.setModuleOptions(std::vector<std::string>{"Safe"});
  hobbes::pushModuleDir(dir);

  bool threw = false;
  try {
    hobbes::compile(&lc, lc.readModuleFile(dir + "/outer.hob"));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("pexec") != std::string::npos);
  }
  EXPECT_TRUE(threw);

  ::unlink((dir + "/inner.hob").c_str());
  ::unlink((dir + "/outer.hob").c_str());
  ::rmdir(dir.c_str());
}

// A module cannot lift a Safe-mode denial for the process that loaded it: a
// file consisting of nothing but {-# SAFE pexec #-} used to do exactly that,
// because pragmas are applied after the definition loop so the module never
// had to name the primitive (STRFR-434028).
TEST(Safe, aSafePragmaCannotLiftADenialUnderSafe) {
  hobbes::cc lc;
  lc.setModuleOptions(std::vector<std::string>{"Safe"});
  bool threw = false;
  try {
    hobbes::compile(&lc, lc.readModule("module p where\n{-# SAFE pexec #-}\n"));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("not allowed while") != std::string::npos);
  }
  EXPECT_TRUE(threw);

  // without Safe the pragma is still available, as it is for boot code
  hobbes::cc unsafeCC;
  hobbes::compile(&unsafeCC, unsafeCC.readModule("module p2 where\n{-# SAFE pexec #-}\n"));
}

TEST(Safe, moduleOptionsDefaultToNoneSoEmbeddersAreUnaffected) {
  // the library imposes no option set of its own: a cc that never calls
  // setModuleOptions compiles modules exactly as before. pexec is one of hi's
  // bindings and is not present in a bare cc, so the give-away is *which*
  // error comes back -- an unbound name rather than a Safe rejection, which
  // is refused by name before any binding lookup happens
  hobbes::cc lc;
  EXPECT_TRUE(lc.moduleOptions().empty());
  bool threw = false;
  try {
    hobbes::compile(&lc, lc.readModule("module u where\nsneak2 = \\x.pexec(\"/bin/sh\")\n"));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("Undefined variable") != std::string::npos);
    EXPECT_TRUE(std::string(ex.what()).find("not allowed") == std::string::npos);
  }
  EXPECT_TRUE(threw);
}
