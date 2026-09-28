#include "test.H"
#include <hobbes/hobbes.H>
#include <string>

using namespace hobbes;

static cc& c() { static __thread cc* x = nullptr; if (x == nullptr) { x = new cc(); } return *x; }

// an expression whose operators associate to the left nests one level per term,
// while the parser's own stack stays flat -- so the tree it builds is as deep as
// the input describes
static std::string leftNestedSum(size_t terms) {
  std::string r = "x";
  for (size_t i = 0; i < terms; ++i) {
    r += "+x";
  }
  return r;
}

TEST(Parse, DeeplyNestedExpressionsAreRejected) {
  // reading source text is on the near side of the trust boundary (see
  // SECURITY.md), and every walk over a parsed expression recurs through its
  // nesting -- the expression's own destructor first among them. Deep enough
  // input used to run the stack out rather than being rejected; the depth here
  // is well past what a stack can hold, so an unfixed build crashes on it
  // rather than failing this test
  EXPECT_EXCEPTION(c().readExpr(leftNestedSum(150000)));

  // the same is true of a chain of applications
  std::string apps = "f";
  for (size_t i = 0; i < 150000; ++i) {
    apps += "(1)";
  }
  EXPECT_EXCEPTION(c().readExpr(apps));

  // and the process is still usable afterwards -- the rejected expression was
  // released a level at a time rather than through the recursive destructor
  // chain that dropping it would otherwise run
  EXPECT_EQ(show(c().readExpr("1+2")), "+(1, 2)");
}

TEST(Parse, FailedParsesReleaseDeepExpressionsIteratively) {
  // the nesting bound guards what the parser returns, but a parse that fails
  // can strand a deep tree where the bound cannot reach it: an operator chain
  // inside a call that is never closed fails at end of file ("expecting ) or
  // ','"), and by then the chain -- one level of nesting per term -- is
  // referenced from the parser's book-keeping rather than from any expression
  // the nesting check will see. Releasing it ran the recursive destructor
  // cascade, one stack frame per level, that OSS-Fuzz 554085275 reports as a
  // stack overflow in App::~App under fuzz-parse-expr (116KB of `N<N<N<...`
  // with one unclosed paren). Expression teardown is now iterative at the
  // source (see expr.C), so the failed parse throws its syntax error and the
  // stranded tree is drained a node at a time; an unfixed build crashes here
  // -- measured at 300,000 terms even without sanitizer-sized frames --
  // rather than failing the test.
  std::string chain = "N(";
  for (size_t i = 0; i < 300000; ++i) {
    chain += "N<";
  }
  chain += "N"; // and the '(' is never closed
  EXPECT_EXCEPTION(c().readExpr(chain));

  // and the process is still usable afterwards
  EXPECT_EQ(show(c().readExpr("1+2")), "+(1, 2)");
}

// whether reading the source fails for nesting past the limit, as opposed to
// any other reason (a syntax error would also throw, and would prove nothing)
static bool rejectedForNesting(const std::string& src) {
  try {
    c().readExpr(src);
    return false;
  } catch (const std::exception& ex) {
    return std::string(ex.what()).find("past the limit") != std::string::npos;
  }
}

TEST(Parse, DeepExpressionsCompiledMidParseAreRejected) {
  // the nesting bound is checked on what the parser returns, but some grammar
  // actions compile a sub-expression while the parse is still under way -- a
  // pattern function's body, a match row's result, the body under a `let` with
  // a pattern, a comprehension's body, a parse rule's reduction -- and
  // compiling walks it, one stack frame per level: the match compiler renames
  // pattern variables by substitution, for one. OSS-Fuzz 556791547 is a `\x.`
  // whose body is a chain of 11,000 `<` comparisons; the parser never got to
  // bound it, because substVarF overflowed the stack first. Those compilers
  // now bound what they are handed, so each shape below is rejected for its
  // depth (checked by message, since a syntax error would also throw); an
  // unfixed build crashes here rather than failing the test.
  std::string chain = "N";
  for (size_t i = 0; i < 300000; ++i) {
    chain += "<N";
  }

  // each shape reads as the construct it is meant to be when it is shallow
  EXPECT_TRUE(c().readExpr("\\x.y<-N<N")                     != nullptr);
  EXPECT_TRUE(c().readExpr("match 1 with | _ -> N<N")         != nullptr);
  EXPECT_TRUE(c().readExpr("let (a, b) = p in N<N")           != nullptr);
  EXPECT_TRUE(c().readExpr("[N<N | x <- xs]")                 != nullptr);
  EXPECT_TRUE(c().readExpr("parse { S := x:\"a\" { N<N } }") != nullptr);

  // and is rejected for its depth when it is not
  EXPECT_TRUE(rejectedForNesting("\\x.y<-" + chain));                     // the reported shape
  EXPECT_TRUE(rejectedForNesting("match 1 with | _ -> " + chain));
  EXPECT_TRUE(rejectedForNesting("let (a, b) = p in " + chain));
  EXPECT_TRUE(rejectedForNesting("[" + chain + " | x <- xs]"));
  EXPECT_TRUE(rejectedForNesting("parse { S := x:\"a\" { " + chain + " } }"));

  // and the process is still usable afterwards
  EXPECT_EQ(show(c().readExpr("1+2")), "+(1, 2)");
  EXPECT_EQ(c().compileFn<int()>("(\\x.x+1)(1)")(), 2);
}

TEST(Parse, DeepQuotedExpressionsAreRejected) {
  // a quoted expression is folded into a type as it is parsed, and making
  // that type prints the expression (TExpr::make interns it by its printed
  // form) -- one stack frame per level, before the parse has returned
  // anything the nesting bound could be checked on. That holds wherever a
  // quote may appear: as an expression, and as a type in a module's type
  // definition. The quote's expression is now bounded before the type is
  // made; an unfixed build crashes in show() here rather than failing the
  // test.
  std::string chain = "N";
  for (size_t i = 0; i < 150000; ++i) {
    chain += "<N";
  }

  EXPECT_TRUE(c().readExpr("`N<N`") != nullptr);
  EXPECT_TRUE(rejectedForNesting("`" + chain + "`"));

  EXPECT_TRUE(c().readModule("type Q = `N<N`") != nullptr);
  try {
    c().readModule("type Q = `" + chain + "`");
    EXPECT_TRUE(false);
  } catch (const std::exception& ex) {
    EXPECT_TRUE(std::string(ex.what()).find("past the limit") != std::string::npos);
  }

  // and the process is still usable afterwards
  EXPECT_EQ(show(c().readExpr("1+2")), "+(1, 2)");
}

TEST(Parse, ExpressionsWithinTheNestingLimitStillParse) {
  // ordinary expressions are nowhere near the limit, and expressions that are
  // deeply nested but still within it read as they always have
  EXPECT_TRUE(c().readExpr(leftNestedSum(500)) != nullptr);

  std::string hundred = "0";
  for (size_t i = 0; i < 100; ++i) {
    hundred += "+1";
  }
  EXPECT_EQ(c().compileFn<int()>(hundred)(), 100);
}

TEST(Parse, NestingDepthAndRelease) {
  // nesting depth counts levels of expression, so a variable is one level and
  // each operator applied to it adds another
  EXPECT_EQ(nestingDepth(c().readExpr("x")), size_t(1));
  EXPECT_EQ(nestingDepth(c().readExpr("x+1")), size_t(2));
  EXPECT_EQ(nestingDepth(c().readExpr("(x+1)*2")), size_t(3));

  // and both it and the level-at-a-time release run without recursing, so they
  // hold up on a tree too deep to walk recursively
  ExprPtr e = c().readExpr(leftNestedSum(500));
  EXPECT_EQ(nestingDepth(e), size_t(501));
  releaseNesting(e);
  EXPECT_TRUE(e == nullptr);
}

// The scanner used to carry a backtracking state stack -- one entry per
// character scanned while matching a token -- in a buffer allocated once at
// (YY_BUF_SIZE + 2) states and never grown, so a token longer than 16,386
// characters wrote four bytes past the end of it for every further character
// (OSS-Fuzz testcase 6480698828193792, which ASan reports as a
// heap-buffer-overflow WRITE in yylex). The buffer existed only to support one
// rule's trailing context; nothing bounded a token against it.
static const size_t pastTheOldStateBuffer = 20000;

TEST(Parse, TokensLongerThanTheScanBufferAreSafe) {
  // an identifier is the plainest way to ask for one long token
  const std::string longIdent(pastTheOldStateBuffer, 'a');
  ExprPtr e = c().readExpr(longIdent);
  EXPECT_TRUE(e != nullptr);
  EXPECT_EQ(show(e), longIdent);

  // as is a string literal, which reads as one token of its own
  EXPECT_TRUE(c().readExpr("\"" + longIdent + "\"") != nullptr);

  // a regex literal that long is one token too, and is rejected for its term
  // count -- the point here is that it is rejected rather than overrunning the
  // scanner on the way
  EXPECT_EXCEPTION(c().readExpr("'" + longIdent + "'"));

  // and the compiler still works afterwards
  EXPECT_EQ(show(c().readExpr("1+2")), "+(1, 2)");
}

TEST(Parse, IndentedDefinitionsStillRead) {
  // the rule that reads indentation needs a character or two of lookahead to
  // tell an indented definition from a comment, and it asks for that by
  // matching and giving back rather than by trailing context. This is the
  // behaviour that depends on it: members of a class or instance are found by
  // their indentation, and an indented comment is not one of them.
  // (test/Objects.C covers this too, but only on a non-clang build.)
  cc lc;
  compile(&lc, lc.readModule(
    "class Sizeable a where\n"
    "  sizeOfIt :: a -> int\n"
    "instance Sizeable int where\n"
    "  sizeOfIt _ = 4\n"
    "instance Sizeable [char] where\n"
    "  // an indented comment is not a member\n"
    "  sizeOfIt _ = 7\n"
  ));
  EXPECT_EQ(lc.compileFn<int()>("sizeOfIt(1) + sizeOfIt(\"ab\")")(), 11);
}

// The lexer keeps state between tokens -- which start condition it is in, and
// the off-side-rule bookkeeping (whether an indent is significant, and a stack
// of that for each bracket opened) -- and a parse that fails leaves that state
// wherever the failure found it. It used to stay there for the next parse, of
// any text, in the same process: an unterminated block comment left every
// later input read as comment, and a class body cut short left later inputs
// with an indent token where none belongs. Each parse now begins and ends with
// the lexer's state as a fresh process would have it.
TEST(Parse, AFailedParseLeavesNoLexerStateBehind) {
  // an unterminated block comment: the scanner is left inside it
  EXPECT_EXCEPTION(c().readExpr("1 + /* never closed"));
  EXPECT_EQ(show(c().readExpr("1+2")), "+(1, 2)");

  // a class body cut short: 'class' made indentation significant, and nothing
  // made it insignificant again
  cc lc;
  EXPECT_EXCEPTION(lc.readModule("class Foo a where\n  f :: int ->"));
  EXPECT_EQ(lc.compileFn<int()>("1 +\n  2")(), 3);

  // a bracket left open inside that class body: the indent flag was saved on
  // the bracket stack and never popped
  EXPECT_EXCEPTION(lc.readModule("class Bar a where\n  g :: (int,"));
  EXPECT_EQ(lc.compileFn<int()>("(1 +\n  2) +\n  3")(), 6);

  // and a parse that ends by throwing from inside a reduction (here a regex
  // literal over its term limit) is finished the same as one that returns:
  // the buffer it ran on is closed and nothing of it is left open
  EXPECT_EXCEPTION(c().readExpr("'" + std::string(2000, 'a') + "'"));
  EXPECT_EQ(openParseCount(), size_t(0));
  EXPECT_EQ(show(c().readExpr("1+2")), "+(1, 2)");
}

// a parser generated from a `parse { ... }` grammar first works out which of
// its rules can derive the empty string. That pass walked a set while erasing
// from it -- erasing the element its iterator named, then stepping the dead
// iterator -- which is undefined behaviour reached by any grammar with a rule
// that can be empty (a debug libstdc++ reports "attempt to increment a
// singular iterator"; an ordinary build reads freed memory and may crash or
// mis-classify a rule). Nullable rules are common (an optional prefix, an
// empty list), so the parser they produce should just work.
TEST(Parse, GrammarsWithNullableRulesAreGenerated) {
  // O is nullable directly, P only through O: both must be seen as such for
  // "a" to be accepted at all
  cc lc;
  lc.define("np", "parse { S := p:P \"a\" { p }   P := o:O { o }   O := \"b\" { 1 } | { 0 } }");

  EXPECT_EQ(lc.compileFn<int()>("match np(\"a\")  with | |1=x| -> x | _ -> -1")(), 0);
  EXPECT_EQ(lc.compileFn<int()>("match np(\"ba\") with | |1=x| -> x | _ -> -1")(), 1);
  EXPECT_EQ(lc.compileFn<int()>("match np(\"bb\") with | |1=x| -> x | _ -> -1")(), -1);
  EXPECT_EQ(lc.compileFn<int()>("match np(\"\")   with | |1=x| -> x | _ -> -1")(), -1);
}

// the code generated for a `parse { ... }` grammar is quadratic in the length
// of a rule (each character of a literal is a terminal, and each state takes
// the stack below it as arguments), and readExpr generates it: a rule with a
// literal of a few thousand characters took some 3GB to read (OSS-Fuzz
// 566458274). A grammar past cc::parserMaxStackDepthSum is refused up front.
static std::string literalGrammar(size_t n) {
  return "parse { S := \"" + std::string(n, 'a') + "\" { 1 } }";
}

TEST(Parse, OversizedGrammarsAreRejected) {
  cc lc;

  // a rule of n characters has states of depth 1..n: 3000 sums to ~4.5M
  try {
    lc.readExpr(literalGrammar(3000));
    EXPECT_TRUE(false && "an oversized grammar was accepted");
  } catch (std::exception& ex) {
    EXPECT_TRUE(std::string(ex.what()).find("grammar is too large") != std::string::npos);
  }

  // the limit is the cc's to set, either way
  lc.parserMaxStackDepthSum(1000);
  EXPECT_EXCEPTION(lc.readExpr(literalGrammar(100)));  // ~5k
  lc.parserMaxStackDepthSum(10000);
  EXPECT_TRUE(lc.readExpr(literalGrammar(100)) != nullptr);

  // and a grammar within the default reads, and parses, as it always has
  cc dc;
  dc.define("p", literalGrammar(200));  // ~20k
  EXPECT_EQ(dc.compileFn<int()>(("match p(\"" + std::string(200, 'a') + "\") with | |1=x| -> x | _ -> -1").c_str())(), 1);
  EXPECT_EQ(dc.compileFn<int()>("match p(\"aa\") with | |1=x| -> x | _ -> -1")(), -1);
}

// an error thrown from a lexer or grammar action (an unsupported literal here)
// goes past yyerror, which is what records where a syntax error was found. The
// position reported for such an error used to be whatever the last syntax
// error in the process had left there -- a location in some earlier input --
// and 0,0 in a process that had seen none. It is now the token the parser was
// on when the error was thrown.
TEST(Parse, ActionErrorsAreReportedAtTheirOwnPosition) {
  cc lc;
  static const char* hugeLit = "9999999999999999999999999999";

  // a fresh process: the literal's own position, not 0,0
  EXPECT_EXCEPTION_MSG(lc.readExpr(hugeLit), std::exception, "1,1-28");

  // after a syntax error at another position in another input
  EXPECT_EXCEPTION_MSG(lc.readExpr("let x = in x"), std::exception, "1,9-10");
  EXPECT_EXCEPTION_MSG(lc.readExpr(hugeLit), std::exception, "1,1-28");

  // and further into the input, where the two positions differ
  EXPECT_EXCEPTION_MSG(lc.readExpr(std::string("1 + ") + hugeLit), std::exception, "1,5-32");

  // a syntax error still reports where the parser found it
  EXPECT_EXCEPTION_MSG(lc.readExpr("let x = in x"), std::exception, "1,9-10");

  // an empty input has no token for the lexer to place, so its end-of-file
  // error was reported wherever the previous parse's last token was -- deep
  // in the boot module for a fresh compiler, or here at line 5 -- instead of
  // at the start of the (empty) input
  EXPECT_TRUE(lc.readExpr("1+\n2+\n3+\n4+\n5+      6") != nullptr);
  EXPECT_EXCEPTION_MSG(lc.readExpr(""), std::exception, "1,1-1");
  EXPECT_EXCEPTION_MSG(lc.readExpr("   "), std::exception, "1,3-3"); // end of file sits on the last thing lexed
}

// the nesting bound on a parsed expression was checked on what readExpr and
// readExprDefn return, but not on the expressions inside a module, so a
// script definition past it (`hi -x`) was compiled, and the compiler's walks
// over it ran the stack out
static bool moduleRejectedForNesting(const std::string& src) {
  try {
    c().readModule(src);
    return false;
  } catch (const std::exception& ex) {
    return std::string(ex.what()).find("past the limit") != std::string::npos;
  }
}

TEST(Parse, DeeplyNestedModuleDefinitionsAreRejected) {
  // the 15,000-term definition that used to crash `hi -x`
  EXPECT_TRUE(moduleRejectedForNesting("x = " + leftNestedSum(15000)));

  // an instance member's body is an expression of the module too
  EXPECT_TRUE(moduleRejectedForNesting(
    "class Deep a where\n"
    "  deep :: a -> a\n"
    "instance Deep int where\n"
    "  deep x = " + leftNestedSum(15000) + "\n"
  ));

  // and a module-level expression evaluated for its effect
  EXPECT_TRUE(moduleRejectedForNesting("print(" + leftNestedSum(15000) + ")"));

  // while a module within the limit reads as it always has
  EXPECT_TRUE(c().readModule("x = " + leftNestedSum(500)) != nullptr);
  EXPECT_TRUE(c().readModule(
    "class Shallow a where\n"
    "  shallow :: a -> a\n"
    "instance Shallow int where\n"
    "  shallow x = " + leftNestedSum(500) + "\n"
  ) != nullptr);
}
