
#include <hobbes/hobbes.H>
#include "test.H"

using namespace hobbes;
static cc& c() { static cc x; return x; }

// arrays of structs exercise cexpr MkArray/AIndex with typed GEP (opaque pointers)
TEST(Codegen, ArrayOfStructs) {
  EXPECT_TRUE(c().compileFn<bool()>(
    "[{x=1,y=2.0}, {x=3,y=4.0}][1].x == 3")());
  EXPECT_TRUE(c().compileFn<bool()>(
    "let rs = [{a=\"hello\",b=10}, {a=\"world\",b=20}] in rs[0].a == \"hello\"")());
  EXPECT_TRUE(c().compileFn<bool()>(
    "sum([r.x | r <- [{x=1,y=2}, {x=3,y=4}, {x=5,y=6}]]) == 9")());
}

// arrays of variants exercise typed array element access
TEST(Codegen, ArrayOfVariants) {
  EXPECT_TRUE(c().compileFn<bool()>(
    "show([|0=42|::(int+double), |1=3.14|]) == \"[|0=42|, |1=3.14|]\"")());
  EXPECT_EQ(c().compileFn<int()>(
    "case [|0=1|::(int+char), |1='x'|][0] of |0:i=i, 1:_=0|")(), 1);
}

// variant construction with large payloads exercises offset calculation
TEST(Codegen, VariantLargePayload) {
  EXPECT_EQ(c().compileFn<int()>(
    "case |0=42|::(int+(int*double*long*double)) of |0:v=v| default 0")(), 42);
  EXPECT_TRUE(c().compileFn<bool()>(
    "case |1=(1,2.0,3L,4.0)|::(int+(int*double*long*double)) of |1:t=t.0 == 1| default false")());
}

// variant matching with default branches exercises Case codegen
TEST(Codegen, VariantDefaultBranch) {
  EXPECT_EQ(c().compileFn<int()>(
    "(\\v.(case v of |0:x=x| default -1))(|1=99|::int+int+int)")(), -1);
  EXPECT_EQ(c().compileFn<int()>(
    "(\\v.(case v of |0:x=x| default -1))(|0=42|::int+int+int)")(), 42);
}

// record field access with mixed types exercises structFieldPtr with explicit types
TEST(Codegen, RecordFieldAccess) {
  EXPECT_TRUE(c().compileFn<bool()>(
    "let r = {a=true, b=42, c=3.14, d=\"hello\"} in r.a and r.b == 42 and r.c > 3.0 and r.d == \"hello\"")());
  EXPECT_EQ(c().compileFn<long()>(
    "let r = {x=1L, y=2L, z=3L} in r.x + r.y + r.z")(), 6L);
}

// nested record access
TEST(Codegen, NestedRecords) {
  EXPECT_TRUE(c().compileFn<bool()>(
    "let r = {outer={inner=42}} in r.outer.inner == 42")());
  EXPECT_TRUE(c().compileFn<bool()>(
    "let r = {a={b={c=99}}} in r.a.b.c == 99")());
}

// function pointers through JIT exercise getMachineCode/allocFunction
TEST(Codegen, FunctionPointers) {
  auto add = c().compileFn<int(int,int)>("x", "y", "x + y");
  EXPECT_EQ(add(10, 20), 30);
  EXPECT_EQ(add(-5, 5), 0);

  auto applyTwice = c().compileFn<int(int)>("x", "let f = (\\y.y*2) in f(f(x))");
  EXPECT_EQ(applyTwice(3), 12);
}

// higher-order functions exercise closure compilation
TEST(Codegen, HigherOrderFunctions) {
  EXPECT_TRUE(c().compileFn<bool()>(
    "let apply = (\\f x.f(x)) in apply((\\x.x*x), 5) == 25")());
  EXPECT_TRUE(c().compileFn<bool()>(
    "let compose = (\\f g x.f(g(x))) in compose((\\x.x+1), (\\x.x*2), 3) == 7")());
}

// constant folding / reuse exercises ConstantList in jitcc
TEST(Codegen, Constants) {
  EXPECT_EQ(c().compileFn<int()>("42")(), 42);
  EXPECT_EQ(c().compileFn<long()>("100L")(), 100L);
  EXPECT_TRUE(c().compileFn<bool()>("3.14 > 3.13 and 3.14 < 3.15")());
  EXPECT_TRUE(c().compileFn<bool()>("\"constant string\" == \"constant string\"")());
}

// tuple operations exercise record codegen with positional fields
TEST(Codegen, Tuples) {
  EXPECT_EQ(c().compileFn<int()>("(1,2,3).0")(), 1);
  EXPECT_EQ(c().compileFn<int()>("(1,2,3).2")(), 3);
  EXPECT_TRUE(c().compileFn<bool()>(
    "let t = (\"hello\", 42, true) in t.1 == 42 and t.2")());
}

// pattern matching on multiple types exercises Case compilation
TEST(Codegen, PatternMatching) {
  // match on bool
  EXPECT_EQ(c().compileFn<int()>("if true then 1 else 0")(), 1);
  EXPECT_EQ(c().compileFn<int()>("if false then 1 else 0")(), 0);

  // match on nested structures
  EXPECT_TRUE(c().compileFn<bool()>(
    "match (1, |a=42|::|a:int,b:double|) with | (1, |a=x|) -> x == 42 | _ -> false")());
}

// recursive definitions exercise function compilation and machine code lookup
TEST(Codegen, Recursion) {
  cc rc;
  rc.define("fib", "(\\n.if (n < 2) then n else (fib(n-1) + fib(n-2))) :: int -> int");
  EXPECT_EQ(rc.compileFn<int()>("fib(10)")(), 55);
}

// array comprehensions with complex element types
TEST(Codegen, ArrayComprehensions) {
  EXPECT_TRUE(c().compileFn<bool()>(
    "[x*x | x <- [1..5]] == [1, 4, 9, 16, 25]")());
  EXPECT_TRUE(c().compileFn<bool()>(
    "[(x,y) | x <- [1..3] | y <- [1..x]] == [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3)]")());
}

// sub-32-bit types exercise sign/zero extension in return values
TEST(Codegen, SubWordTypes) {
  EXPECT_EQ(c().compileFn<char()>("'A'")(), 'A');
  EXPECT_EQ(c().compileFn<short()>("100S")(), short(100));
  EXPECT_EQ(c().compileFn<short()>("sadd(100S, 1S)")(), short(101));
  EXPECT_EQ(c().compileFn<char()>("'\\n'")(), '\n');
}

// type conversions exercise cast operations in func.C
TEST(Codegen, TypeConversions) {
  EXPECT_TRUE(c().compileFn<bool()>("i2d(42) > 41.9")());
  EXPECT_EQ(c().compileFn<long()>("i2l(42)")(), 42L);
  EXPECT_TRUE(c().compileFn<bool()>("l2d(100L) > 99.9")());
}
