
#include <hobbes/hobbes.H>
#include "test.H"

using namespace hobbes;
static cc& c() { static __thread cc* x = 0; if (!x) { x = new cc(); } return *x; }

TEST(Definitions, Basic) {
  c().define("defInt",      "42");
  c().define("defArray",    "[0, 42, 9, 100]");
  c().define("defStrArray", "[\"jimmy\", \"chicken\", \"hamburger\"]");
  c().define("defRecord",   "{x=9,y=\"foobar\",z=42.0}");
  c().define("factorial",   "(\\x.if (x == 0) then 1 else (x * factorial(x - 1))) :: int->int");

  c().define("a000", "123");
  c().define("b000", "456");
  c().define("c000", "{abc=a000, def=b000}");

  EXPECT_TRUE(c().compileFn<bool()>("c000.abc == 123")());

  EXPECT_EQ(c().compileFn<int()>("defInt")(), 42);
  EXPECT_EQ(c().compileFn<size_t()>("length(defStrArray)")(), size_t(3));
  EXPECT_EQ(c().compileFn<int()>("defArray[1]")(), 42);
  EXPECT_TRUE(c().compileFn<bool()>("defStrArray[1] == \"chicken\"")());
  EXPECT_TRUE(c().compileFn<bool()>("defRecord.z >= 41.9999 and defRecord.z <= 42.0001")());
  EXPECT_TRUE(c().compileFn<bool()>("factorial(10) == 3628800")());
}

TEST(Definitions, Arrays) {
  c().define("gaps0", "[(1,2), (3,4)]");
  EXPECT_TRUE(c().compileFn<bool()>("sum([x+y | (x,y) <- gaps0]) == 10")());
}

TEST(Definitions, RecType) {
  c().define("junroll", "unroll :: (^x.(()+(a*x))) -> (()+(a*^x.(()+(a*x))))");

  EXPECT_TRUE(c().compileFn<bool()>("show(junroll(cons(1,nil()))) == \"|1=(1, [])|\"")());
  EXPECT_TRUE(c().compileFn<bool()>("show(junroll(cons(1L,nil()))) == \"|1=(1, [])|\"")());

  c().define("llen", "(\\xs.case unroll(xs) of |0:_=0L, 1:p=1+llen(p.1)|) :: (^x.(()+(a*x))) -> long");

  EXPECT_TRUE(c().compileFn<bool()>("llen(cons(1,nil())) == 1L")());
  EXPECT_TRUE(c().compileFn<bool()>("llen(cons('c',nil())) == 1L")());
  EXPECT_TRUE(c().compileFn<bool()>("llen(cons(0xdeadbeef,nil())) == 1L")());
  EXPECT_TRUE(c().compileFn<bool()>("llen(cons(0Xde,nil())) == 1L")());
  EXPECT_TRUE(c().compileFn<bool()>("llen(cons(9L,nil())) == 1L")());
  EXPECT_TRUE(c().compileFn<bool()>("llen(cons(9S,nil())) == 1L")());
}

TEST(Definitions, FuncAliases) {
  c().define("f1", "(\\(). 42)::()->int");
  c().define("f2", "f1");
  EXPECT_EQ(c().compileFn<int()>("f2()")(), 42)
}

TEST(Definitions, StructsWithFns) {
  c().define("prof", "{x=[1,2,3], y=\\().putStrLn(\"hello world\")}");
  EXPECT_EQ(c().compileFn<int()>("sum(prof.x)")(), 6);
}

