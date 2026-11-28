
#include <hobbes/hobbes.H>
#include <hobbes/eval/ctype.H>
#include "test.H"
#include <pthread.h>
#include <string>

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

// verifyCodeForExpr lowers to IR, checks it and takes it back out again, leaving
// the compiler as it found it (it is what the type-checker fuzzer runs on every input)
TEST(Codegen, VerifyCodeForExpr) {
  cc vc;
  const char* exprs[] = {
    "i2l(42)",
    "(\\x.x+1)(2)",
    "[x*x | x <- [1..5]]",
    "match 1 2 with | 1 y -> y | _ _ -> 0",
    "{x=[1,2,3], y=\\().putStrLn(\"hello world\")}",
    "let f = (\\x.if (x > 0) then x else 0) in f(3)",
  };
  for (const char* e : exprs) {
    ExprPtr x = vc.readExpr(e);
    vc.unsweetenExpression(x); // leave any residual definitions behind before counting
    size_t before = vc.module()->size();
    EXPECT_EQ(vc.verifyCodeForExpr(x), std::string());
    // nothing left but a trapping stub for a function a constant still refers to
    EXPECT_TRUE(vc.module()->size() <= before + 1);
  }

  // an expression that does not type check throws as it would from compileFn
  EXPECT_EXCEPTION(vc.verifyCodeForExpr(vc.readExpr("1 + \"one\"")));

  // and the compiler still works afterwards
  EXPECT_EQ(vc.compileFn<int()>("let f = (\\x.if (x > 0) then x else 0) in f(3)")(), 3);
}

// Translating a type to LLVM recurs into the types it holds and into what its
// type-level applications reduce to. A type built (or decoded from an untrusted
// description) to nest without bound, or an application that reduces to
// itself, is rejected rather than allowed to run the stack out. The types are
// built directly, without the parser, which has nesting bounds of its own.
//
// The bound is on the depth of one translation, and a type that has been
// translated is not translated again, so what a type costs depends on which of
// its parts have been translated before. Each type here is therefore built on a
// leaf of its own that nothing else has translated, which makes its cost
// exactly a level for the leaf and what its wrappers take: one each for a
// structure or a reduction, two for an alias of a structure.
static const size_t translateNestingBound = 1000; // maxTranslateNesting in ctype.C

static MonoTypePtr untranslatedLeaf() {
  static size_t n = 0;
  return OpaquePtr::make("CodegenNestingLeaf" + str::from(n++), 8, false);
}

using WrapType = MonoTypePtr (*)(const MonoTypePtr&);

static MonoTypePtr nestType(size_t levels, WrapType wrap) {
  MonoTypePtr r = untranslatedLeaf();
  for (size_t i = 0; i < levels; ++i) {
    r = wrap(r);
  }
  return r;
}

static MonoTypePtr inArray(const MonoTypePtr& t)    { return arrayty(t); }
static MonoTypePtr inRecord(const MonoTypePtr& t)   { return tuplety(list(primty("long"), t)); }
static MonoTypePtr inFuncArg(const MonoTypePtr& t)  { return MonoTypePtr(Func::make(tuplety(list(t)), primty("int"))); }
static MonoTypePtr inArrayApp(const MonoTypePtr& t) { return tapp(primty("[]"), list(t)); }
// 'CodegenId t' reduces to t, 'CodegenArr t' to [t], and 'CodegenArrAlias t' to 'CodegenArr t'
static MonoTypePtr inIdApp(const MonoTypePtr& t) {
  return tapp(MonoTypePtr(Prim::make("CodegenId", tabs(str::strings("x"), tvar("x")))), list(t));
}
static MonoTypePtr inAliasApp(const MonoTypePtr& t) {
  return tapp(MonoTypePtr(Prim::make("CodegenArr", tabs(str::strings("x"), arrayty(tvar("x"))))), list(t));
}
static MonoTypePtr inAliasOfAliasApp(const MonoTypePtr& t) {
  const MonoTypePtr arr(Prim::make("CodegenArr", tabs(str::strings("x"), arrayty(tvar("x")))));
  return tapp(MonoTypePtr(Prim::make("CodegenArrAlias", tabs(str::strings("x"), tapp(arr, list(tvar("x")))))), list(t));
}

TEST(Codegen, TypeTranslationNestingIsBounded) {
  c(); // the LLVM context that types are translated into

  const size_t n = translateNestingBound;
  const char* refused = "LLVM type translation nests";

  // the type-level reading of '(\x.[x x]) (\x.[x x])', an array of itself
  const MonoTypePtr selfArr(Prim::make("selfArr", tabs(str::strings("x"), arrayty(tapp(tvar("x"), list(tvar("x")))))));
  EXPECT_EXCEPTION_MSG(toLLVM(tapp(selfArr, list(selfArr))), std::runtime_error, refused);

  // and '(\x.x x) (\x.x x)', which reduces to itself without holding anything
  const MonoTypePtr selfApp(Prim::make("selfApp", tabs(str::strings("x"), tapp(tvar("x"), list(tvar("x"))))));
  EXPECT_EXCEPTION_MSG(toLLVM(tapp(selfApp, list(selfApp))), std::runtime_error, refused);

  // structural nesting far past the bound (kept to a few thousand levels:
  // interned types are compacted level by level at exit)
  EXPECT_EXCEPTION_MSG(toLLVM(nestType(3 * n, &inArray)), std::runtime_error, refused);

  // the bound is where it says it is. A structure is a level, so with the leaf
  // n-1 of them translate and n don't, alone or mixed
  EXPECT_TRUE(toLLVM(nestType(n - 1, &inArray)) != nullptr);
  EXPECT_EXCEPTION_MSG(toLLVM(nestType(n, &inArray)), std::runtime_error, refused);
  EXPECT_TRUE(toLLVM(nestType(n - 1, &inRecord)) != nullptr);
  EXPECT_EXCEPTION_MSG(toLLVM(nestType(n, &inRecord)), std::runtime_error, refused);
  EXPECT_TRUE(toLLVM(nestType(n - 1, &inFuncArg)) != nullptr);
  EXPECT_EXCEPTION_MSG(toLLVM(nestType(n, &inFuncArg)), std::runtime_error, refused);
  EXPECT_TRUE(toLLVM(nestType(n - 1, [](const MonoTypePtr& t) { return is<Array>(t) ? inRecord(t) : inArray(t); })) != nullptr);

  // a reduction is a level too: deep types that reduce as they go still
  // translate, up to the same bound
  EXPECT_TRUE(toLLVM(nestType(n - 1, &inIdApp)) != nullptr);
  EXPECT_EXCEPTION_MSG(toLLVM(nestType(n, &inIdApp)), std::runtime_error, refused);

  // so a level that is an alias for a structure counts twice (the application,
  // then what it reduces to), and reaches the bound at half the nesting
  EXPECT_TRUE(toLLVM(nestType((n - 1) / 2, &inArrayApp)) != nullptr);
  EXPECT_EXCEPTION_MSG(toLLVM(nestType((n - 1) / 2 + 1, &inArrayApp)), std::runtime_error, refused);
  EXPECT_TRUE(toLLVM(nestType((n - 1) / 2, &inAliasApp)) != nullptr);
  EXPECT_EXCEPTION_MSG(toLLVM(nestType((n - 1) / 2 + 1, &inAliasApp)), std::runtime_error, refused);

  // and an alias for an alias three times, at a third: deeper chains of
  // aliases are refused sooner, not translated unbounded
  EXPECT_TRUE(toLLVM(nestType((n - 1) / 3, &inAliasOfAliasApp)) != nullptr);
  EXPECT_EXCEPTION_MSG(toLLVM(nestType((n - 1) / 3 + 1, &inAliasOfAliasApp)), std::runtime_error, refused);

  // applying a type function substitutes into its body, which recurs as deep
  // as the body nests (and, where an argument has variables of its own, on
  // into the arguments), so that counts against the bound as well: a function
  // whose body nests too deeply is refused before it is applied
  //
  // (these are arrays, and no deeper than it takes: toLLVM asks for the size of
  // each of these types first, and the type system's own reduction walks them
  // on this thread, at 1.7KB of stack a level in an unoptimized build with
  // AddressSanitizer)
  const char* refusedApp = "levels deep in applying a type function";
  MonoTypePtr deepBody = tvar("x");
  for (size_t i = 0; i < n; ++i) {
    deepBody = arrayty(deepBody);
  }
  const MonoTypePtr deepFn(Prim::make("CodegenDeepBody", tabs(str::strings("x"), deepBody)));
  EXPECT_EXCEPTION_MSG(toLLVM(tapp(deepFn, list(untranslatedLeaf()))), std::runtime_error, refusedApp);

  // an argument with no variables in it is put where the body has its
  // variable without being walked, however deep it is: one just short of the
  // bound comes through an alias, and one past it is refused by its own
  // translation
  EXPECT_TRUE(toLLVM(inIdApp(nestType(n - 2, &inArray))) != nullptr);
  EXPECT_EXCEPTION_MSG(toLLVM(inIdApp(nestType(n, &inArray))), std::runtime_error, refused);

  // an argument with a variable in it is walked, so its depth counts:
  // 'CodegenFst t b' reduces to t, and is refused where t nests too deeply
  // for the walk although it has been translated (so that translating it
  // again would cost nothing)
  const MonoTypePtr fst(Prim::make("CodegenFst", tabs(str::strings("x", "y"), tvar("x"))));
  const MonoTypePtr shallow = nestType(10, &inRecord);
  EXPECT_TRUE(toLLVM(tapp(fst, list(shallow, tvar("b")))) == toLLVM(shallow));
  const MonoTypePtr walked = nestType(n - 1, &inArray);
  EXPECT_TRUE(toLLVM(walked) != nullptr);
  EXPECT_TRUE(toLLVM(inIdApp(walked)) == toLLVM(walked));
  EXPECT_EXCEPTION_MSG(toLLVM(tapp(fst, list(walked, tvar("b")))), std::runtime_error, refusedApp);

  // the guard is released after a rejection, and ordinary types are untouched
  EXPECT_TRUE(toLLVM(tuplety(list(primty("int"), arrayty(primty("char"))))) != nullptr);

  // the bound is on one translation, not on the type: a type nested twice as
  // deep as the bound translates once its inner half has been
  MonoTypePtr half = nestType(n - 1, &inArray);
  MonoTypePtr whole = half;
  for (size_t i = 0; i < n; ++i) {
    whole = arrayty(whole);
  }
  EXPECT_EXCEPTION_MSG(toLLVM(whole), std::runtime_error, refused);
  EXPECT_TRUE(toLLVM(half) != nullptr);
  EXPECT_TRUE(toLLVM(whole) != nullptr);
}

// The bound is only a defense if reaching it fits on the stack. It is sized to
// take about half of the 512KB that a thread other than the main one gets by
// default on macOS, in an optimized build (see maxTranslateNesting in ctype.C),
// so here translations that run right up to the bound, and into it, are run on
// a thread with that much stack: along the costliest path (a function type
// nested in a parameter of another), through records, and through a type-level
// application that reduces to itself, which is what a hostile description
// would use. If the frames that recur grow, or the bound is raised without
// regard to them, this overflows the stack rather than failing the test.
//
// An unoptimized build takes about 750 bytes a level along the costliest path
// (three times what an optimized one does), and AddressSanitizer pads every
// frame with redzones: unoptimized, with it, a level takes 3.1KB. Those builds
// get the stack that they need instead (the bound is reached within 763KB of
// the 2MB, and within 3.1MB of the 6MB), so what is checked there is only that
// the bound fits a stack smaller than the 8MB a main thread has.
#if defined(__SANITIZE_ADDRESS__)
#define HOBBES_TEST_ASAN 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define HOBBES_TEST_ASAN 1
#endif
#endif
#if defined(HOBBES_TEST_ASAN)
static const size_t translateTestStackSize = 6 * 1024 * 1024;
#elif defined(__OPTIMIZE__)
static const size_t translateTestStackSize = 512 * 1024;
#else
static const size_t translateTestStackSize = 2 * 1024 * 1024;
#endif

// run a function of a type on a thread with a stack of a given size, for the
// error if there is one
static std::string onAThreadWithStack(size_t stackSize, void (*f)(const MonoTypePtr&), const MonoTypePtr& ty) {
  struct Run {
    void (*f)(const MonoTypePtr&);
    MonoTypePtr ty;
    std::string error;
  } run{f, ty, ""};

  pthread_attr_t attr;
  if (pthread_attr_init(&attr) != 0) {
    return "failed to set up a thread";
  }
  if (pthread_attr_setstacksize(&attr, stackSize) != 0) {
    pthread_attr_destroy(&attr);
    return "failed to set the stack size of a thread";
  }
  auto body = [](void* p) -> void* {
    auto* r = static_cast<Run*>(p);
    try {
      r->f(r->ty);
    } catch (const std::exception& ex) {
      r->error = ex.what();
    }
    return nullptr;
  };
  pthread_t t;
  if (pthread_create(&t, &attr, body, &run) != 0) {
    pthread_attr_destroy(&attr);
    return "failed to start a thread";
  }
  pthread_join(t, nullptr);
  pthread_attr_destroy(&attr);

  return run.error;
}

// translate a type on a thread with a small stack, for the error if it has one
static std::string translateOnASmallStack(const MonoTypePtr& ty) {
  const std::string error = onAThreadWithStack(translateTestStackSize, [](const MonoTypePtr& t) {
    if (toLLVM(t) == nullptr) {
      throw std::runtime_error("translated to nothing");
    }
  }, ty);
  return error.empty() ? std::string("translated") : error;
}

TEST(Codegen, TypeTranslationAtTheNestingBoundFitsOnASmallStack) {
  c();

  const size_t n = translateNestingBound;
  const std::string refused = "LLVM type translation nests";

  // as deep as the bound allows, and translated
  EXPECT_EQ(translateOnASmallStack(nestType(n - 1, &inFuncArg)), std::string("translated"));
  EXPECT_EQ(translateOnASmallStack(nestType(n - 1, &inRecord)),  std::string("translated"));
  EXPECT_EQ(translateOnASmallStack(nestType(n - 1, &inIdApp)),   std::string("translated"));

  // past it, and refused from the full depth of the bound
  EXPECT_TRUE(translateOnASmallStack(nestType(2 * n, &inFuncArg)).find(refused) != std::string::npos);
  EXPECT_TRUE(translateOnASmallStack(nestType(2 * n, &inRecord)).find(refused) != std::string::npos);

  const MonoTypePtr selfApp(Prim::make("selfApp", tabs(str::strings("x"), tapp(tvar("x"), list(tvar("x"))))));
  EXPECT_TRUE(translateOnASmallStack(tapp(selfApp, list(selfApp))).find(refused) != std::string::npos);
  const MonoTypePtr selfArr(Prim::make("selfArr", tabs(str::strings("x"), arrayty(tapp(tvar("x"), list(tvar("x")))))));
  EXPECT_TRUE(translateOnASmallStack(tapp(selfArr, list(selfArr))).find(refused) != std::string::npos);

  // A reduction substitutes into the body of a type function, and that recurs
  // into the body, and into arguments that have variables in them, uncounted
  // by the levels of translation: it is refused where it would recur deeper
  // than translation has room left for. These are types nested deeper than
  // any description decodes to (maxDecodeNesting is 2000 levels, and a level
  // here is two), in the costliest shape, so that walking one takes more
  // stack than there is: as an argument (which is not walked: it has no
  // variables in it), as an argument beside one with a variable (which is
  // refused), and as the body of the function (likewise).
  //
  // toLLVM first asks the type system for the size of the type, to tell
  // whether it is unit, and for an application that is a reduction of the type
  // system's own (repType), with bounds of its own that are not sized for this
  // stack: for the first of these types it takes 606KB in an optimized build
  // (and 8.4MB unoptimized with AddressSanitizer, more than a main thread
  // has). That is not what is tested here, so each type is sized on a thread
  // with room for it first; the reduction is remembered, and what then runs
  // on the small stack is translation and the reductions that translation
  // makes.
  const size_t deep = 1990;
  const MonoTypePtr fst(Prim::make("CodegenFst", tabs(str::strings("x", "y"), tvar("x"))));
  MonoTypePtr deepBody = tvar("x");
  for (size_t i = 0; i < deep / 2; ++i) {
    deepBody = inFuncArg(deepBody);
  }
  const MonoTypePtr deepFn(Prim::make("CodegenDeepFuncBody", tabs(str::strings("x"), deepBody)));

  const MonoTypePtr throughAlias[] = {
    inIdApp(nestType(deep, &inFuncArg)),
    tapp(fst, list(nestType(deep, &inFuncArg), tvar("b"))),
    tapp(deepFn, list(untranslatedLeaf()))
  };
  for (const auto& ty : throughAlias) {
    // (and if it could not be, the small stack is not the place to find out)
    EXPECT_EQ(onAThreadWithStack(64 * 1024 * 1024, [](const MonoTypePtr& t) { sizeOf(t); }, ty), std::string());
    EXPECT_TRUE(translateOnASmallStack(ty).find(refused) != std::string::npos);
  }
}
