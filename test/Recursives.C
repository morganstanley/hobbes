
#include <hobbes/hobbes.H>
#include "test.H"

using namespace hobbes;
static cc& c() { static cc x; return x; }

TEST(Recursives, Lists) {
  // make a dummy linked list from C++ to verify that binding between hobbes and C++ is correct
  static seq<int> nil;
  static seq<int> tn(3, &nil);
  static seq<int> ttn(2, &tn);
  static seq<int> ottn(1, &ttn);

  c().bind("rectyll", &ottn);

  EXPECT_TRUE(c().compileFn<bool()>("show(rectyll) == \"1:2:3:[]\"")());
  EXPECT_TRUE(c().compileFn<bool()>("show(cons(1,cons(2,cons(3,nil())))) == \"1:2:3:[]\"")());
  EXPECT_TRUE(c().compileFn<bool()>("show(lmap(\\x.x+1, cons(1,cons(2,cons(3,nil()))))) == \"2:3:4:[]\"")());
}

TEST(Recursives, DuplicatedNamesInLetShouldFail) {
  cc c;
  EXPECT_EXCEPTION_MSG(c.compileFn<void()>("(\\x.let a = x; a = 3; in print(a))(0)")(), "has conflicting definitions");
}

TEST(Recursives, DuplicatedNamesInDoShouldFail) {
  cc c;
  EXPECT_EXCEPTION_MSG(c.compileFn<void()>("(\\x.do { a = x; a = 3; print(a); })(0)")(), "has conflicting definitions");
}
