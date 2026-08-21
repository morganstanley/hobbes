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
