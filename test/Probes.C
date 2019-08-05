
#include <hobbes/hobbes.H>
#include "test.H"

using namespace hobbes;
static cc& c() { static __thread cc* x = 0; if (!x) { x = new cc(); } return *x; }

TEST(Probes, Basic) {
  // Simple test: i.e. does the Probe TString unqualifier and injectProbe op compile
  // and not break the wrapped code?
  c().define("a000",
    "do { injectProbe(\"a000\"); return 123 }");
  c().define("factorial",
    "(\\x. (if (x == 0) then 1 else (x * factorial(x - 1))) :: (Probe \"fact\")=>a)");

  EXPECT_TRUE(c().compileFn<bool()>("a000 == 123")());
  EXPECT_TRUE(c().compileFn<bool()>("factorial(10) == 3628800")());
}

