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
