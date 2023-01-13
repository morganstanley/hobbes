
#include <exception>
#include <hobbes/hobbes.H>
#include <stdexcept>
#include "test.H"

using namespace hobbes;
static cc& c() { static __thread cc* x = nullptr; if (x == nullptr) { x = new cc(); } return *x; }

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
  EXPTEST("sum(0x000102) == 3");
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

TEST(Prelude, Int128) {
  EXPECT_EQ(c().compileFn<int128_t()>("40H + 2S")(), 42);
  EXPECT_EQ(c().compileFn<int128_t()>("40H - 42S")(), -2);
  EXPECT_TRUE((c().compileFn<bool(int128_t)>("x", "x==42")(42)));
  EXPECT_EQ(hobbes::makeStdString(c().compileFn<const hobbes::array<char>*()>("show(42H)")()), "42");

  // INT128_MAX w/  'H'
  EXPECT_EQ((makeStdString(c().compileFn<const array<char>*()>("show(readInt128(\"170141183460469231731687303715884105727H\"))")())), "|1=170141183460469231731687303715884105727|");
  // INT128_MAX w/o 'H'
  EXPECT_EQ((makeStdString(c().compileFn<const array<char>*()>("show(readInt128(\"170141183460469231731687303715884105727\"))")())), "|1=170141183460469231731687303715884105727|");
  // INT128_MIN w/  'H'
  EXPECT_EQ((makeStdString(c().compileFn<const array<char>*()>("show(readInt128(\"-170141183460469231731687303715884105728H\"))")())), "|1=-170141183460469231731687303715884105728|");
  // INT128_MIN w/o 'H'
  EXPECT_EQ((makeStdString(c().compileFn<const array<char>*()>("show(readInt128(\"-170141183460469231731687303715884105728\"))")())), "|1=-170141183460469231731687303715884105728|");
  // underflow
  EXPECT_EQ((makeStdString(c().compileFn<const array<char>*()>("show(readInt128(\"-170141183460469231731687303715884105729\"))")())), "|0|");
  // overflow
  EXPECT_EQ((makeStdString(c().compileFn<const array<char>*()>("show(readInt128(\"170141183460469231731687303715884105728\"))")())), "|0|");

  // INT128_MAX
  EXPECT_EQ((makeStdString(c().compileFn<const array<char>*()>("show(170141183460469231731687303715884105727H)")())), "170141183460469231731687303715884105727");
  // INT128_MIN + 1
  EXPECT_EQ((makeStdString(c().compileFn<const array<char>*()>("show(-170141183460469231731687303715884105727H)")())), "-170141183460469231731687303715884105727");

  // INT128_MIN
  EXPECT_EXCEPTION_MSG((makeStdString(c().compileFn<const array<char>*()>("show(-170141183460469231731687303715884105728H)")())), std::exception, "literal 170141183460469231731687303715884105728 is not supported");
  // INT128_MIN - 1
  EXPECT_EXCEPTION_MSG((makeStdString(c().compileFn<const array<char>*()>("show(-170141183460469231731687303715884105729H)")())), std::exception, "literal 170141183460469231731687303715884105729 is not supported");
}

TEST(Prelude, Short) {
  // SHORT_MAX
  EXPECT_EQ(c().compileFn<short()>("32767S")(), 32767);
  // SHORT_MIN + 1
  EXPECT_EQ(c().compileFn<short()>("-32767S")(), -32767);

  // SHORT_MIN
  EXPECT_EXCEPTION_MSG(c().compileFn<short()>("-32768S")(), std::exception, "literal 32768 is not supported");
  // SHORT_MIN - 1
  EXPECT_EXCEPTION_MSG(c().compileFn<short()>("-32769S")(), std::exception, "literal 32769 is not supported");
  // INT32_MAX
  EXPECT_EXCEPTION_MSG(c().compileFn<short()>("2147483647S")(), std::exception, "literal 2147483647 is not supported");
  // INT32_MIN
  EXPECT_EXCEPTION_MSG(c().compileFn<short()>("-2147483648S")(), std::exception, "literal 2147483648 is not supported");
}

TEST(Prelude, Int) {
  // INT32_MAX
  EXPECT_EQ(c().compileFn<int()>("2147483647")(), 2147483647);
  // INT32_MIN + 1
  EXPECT_EQ(c().compileFn<int()>("-2147483647")(), -2147483647);

  // INT32_MAX + 1
  EXPECT_EXCEPTION_MSG(c().compileFn<int()>("2147483648")(), std::exception, "literal 2147483648 is not supported");
  // INT32_MIN
  EXPECT_EXCEPTION_MSG(c().compileFn<int()>("-2147483648")(), std::exception, "literal 2147483648 is not supported");
  // INT32_MIN - 1
  EXPECT_EXCEPTION_MSG(c().compileFn<int()>("-2147483649")(), std::exception, "literal 2147483649 is not supported");
}

TEST(Prelude, Long) {
  // LONG_MAX
  EXPECT_EQ(c().compileFn<long()>("9223372036854775807L")(), 9223372036854775807L);
  // LONG_MIN + 1
  EXPECT_EQ(c().compileFn<long()>("-9223372036854775807L")(), -9223372036854775807L);

  // LONG_MAX + 1
  EXPECT_EXCEPTION_MSG(c().compileFn<long()>("9223372036854775808L")(), std::exception, "literal 9223372036854775808 is not supported");
  // LONG_MIN
  EXPECT_EXCEPTION_MSG(c().compileFn<long()>("-9223372036854775808L")(), std::exception, "literal 9223372036854775808 is not supported");
  // LONG_MIN - 1
  EXPECT_EXCEPTION_MSG(c().compileFn<long()>("-9223372036854775809L")(), std::exception, "literal 9223372036854775809 is not supported");
}