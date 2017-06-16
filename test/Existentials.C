
#include <hobbes/hobbes.H>
#include "test.H"

using namespace hobbes;
static cc& c() { static cc x; return x; }

TEST(Existentials, Basic) {
  c().define("exf", "\\x.((pack ((\\e y.iadd(e.x,y))::({x:int},int)->int,{x=x}))::exists E.(((E,int)->int)*E))");

  // test explicit existential package construction/deconstruction
  EXPECT_EQ(c().compileFn<int()>("unpack z = (exf(10)) in (z.0(z.1,5))")(), 15);
}

TEST(Existentials, Closures) {
  // does closure-conversion implicitly produce an existential package as required?
  EXPECT_EQ(c().compileFn<int()>("unpack z = ((\\x.\\y.x+y)(10)) in (z.0(z.1,5))")(), 15);
}

