
#include <hobbes/hobbes.H>
#include "test.H"

using namespace hobbes;
static cc& c() { static __thread cc* x = 0; if (!x) { x = new cc(); } return *x; }

#define EXPTEST(s) EXPECT_TRUE(c().compileFn<bool()>(s)())

TEST(Prelude, AMapping) {
  EXPTEST("apply(\\x.iadd(x,x), 2) == 4");
  EXPTEST("apply((\\x.\\y.iadd(x,y))(1), 2) == 3");

  EXPTEST("size([1,2,3]) == 3L");
  EXPTEST("element([1,2,3], 0L) == 1");
}

TEST(Prelude, Arith) {
}

TEST(Prelude, Timespan) {
  EXPECT_EQ(microseconds(c().compileFn<timespanT()>("20us")()), size_t(20));
  EXPECT_EQ(milliseconds(c().compileFn<timespanT()>("20ms")()), size_t(20));
  EXPECT_EQ(seconds(c().compileFn<timespanT()>("20s")()), size_t(20));
}

TEST(Prelude, COrd) {
  EXPTEST("(convert({a=1,b=2,c=3}) :: {c:long,a:int}) == {c=3L,a=1}");

  EXPTEST("(1,2) < (1,3)");
  EXPTEST("(1,2) < (3,4)");
}

TEST(Prelude, Eq) {
  EXPTEST("[1..10] == [1,2,3,4,5,6,7,8,9,10]");
  EXPTEST("\"foo\" ~ \"FOO\"");
  EXPTEST("1 == 1L");
}

TEST(Prelude, FArray) {
  EXPTEST("any(\\x.x==9, [1..10])");
  EXPTEST("some(\\x.x==9, [1..10]) === |1=9|");
  EXPTEST("map(\\x.x+9, [1..10]) == [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]");
  EXPTEST("sum([1..10]) == 55");
  EXPTEST("sum([1L..10L]) == 55L");
}

TEST(Prelude, List) {
  EXPTEST("lfoldl(\\r x.cons(x, r), nil(), cons(1, cons(2, cons(3, nil())))) == cons(3, cons(2, cons(1, nil())))");
  EXPTEST("toArray(cons(1, cons(2, cons(3, nil())))) == [1,2,3]");
}

TEST(Prelude, Lookup) {
  EXPTEST("lookup(1, [(0, 'a'), (1, 'b'), (2, 'c')]) === |1='b'|");
  EXPTEST("[(0, 'a'), (1, 'b'), (2, 'c')].1 === |1='b'|");
}

TEST(Prelude, Set) {
  EXPTEST("'o' in \"foobar\"");
  EXPTEST("\"oba\" in \"foobar\"");
  EXPTEST("5 in [1..20]");
  EXPTEST("[5..15] in [1..20]");
  EXPTEST("0xbeef in 0xdeadbeef");
}

TEST(Prelude, Sort) {
  EXPTEST("sort([1..10]) == [1..10]");
  EXPTEST("sort([7,5,1,0,22,8,3,24]) == [0,1,3,5,7,8,22,24]");
  EXPTEST("sortBy((<), [1..10]) == [1..10]");
  EXPTEST("sortBy((<), [7,5,1,0,22,8,3,24]) == [0,1,3,5,7,8,22,24]");
}

TEST(Prelude, SScan) {
  EXPTEST("unique([1..10]) == [1..10]");
}

