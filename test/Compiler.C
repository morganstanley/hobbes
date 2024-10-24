#include <hobbes/hobbes.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/db/file.H>
#include <iomanip>
#include <thread>
#include "hobbes/eval/funcdefs.H"
#include "test.H"

using namespace hobbes;
static cc& c() { static __thread cc* x = nullptr; if (x == nullptr) { x = new cc(); } return *x; }

using BufferView = std::pair<const char *, size_t>;

TEST(Compiler, compileToSupportsMoreThanSixArgs) {
  std::string expression =
    "match vegetables spices fruits meat cheese technique occasion skill drink side with \n"
    "| \"carrots\" \"parsley\" \"lemon\" \"ossobuco\" _ \"braising\" \"everyday\" \"basic\" \"wine\" \"polenta\" -> 10 \n"
    "| _ _ _ _ _ _ _ _ _ _ -> -1";

  using MatchFunPtr = int (*)(const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *, const BufferView *);  // side
  
  hobbes::cc compiler;

  auto funPtr = hobbes::compileTo<MatchFunPtr>(
    &compiler,
    hobbes::list<std::string>("vegetables", "spices", "fruits",
                              "meat","cheese", "technique",
                              "occasion", "skill", "drink", "side"),
    expression);
  
  EXPECT_TRUE(nullptr != funPtr);
}

TEST(Compiler, charArrExpr) {
  char buffer[256];
  strncpy(buffer, "\"hello world\"", sizeof(buffer));
  const auto* p = c().compileFn<const array<char>*()>(buffer)();
  EXPECT_TRUE(hobbes::makeStdString(p) != "\"hello world\"");
}

class BV { };
namespace hobbes {
  template <>
    struct lift<BV*> : public lift<std::pair<const char*, size_t>*> {
    };
}

TEST(Compiler, liftApathy) {
  // one definition by-ref should be enough to infer the equivalent references
  EXPECT_EQ(show(lift<BV*>::type(c())),       "(<char> * long)");
  EXPECT_EQ(show(lift<const BV*>::type(c())), "(<char> * long)");
  EXPECT_EQ(show(lift<BV&>::type(c())),       "(<char> * long)");
  EXPECT_EQ(show(lift<const BV&>::type(c())), "(<char> * long)");
}

TEST(Compiler, compileFnTypes) {
  EXPECT_EQ(c().compileFn<int(const std::string&)>("_","42")(""), 42);
  EXPECT_EQ(c().compileFn<int(*)(const std::string&)>("_","42")(""), 42);
}

static int appC(const closure<int(int)>& c) { return c(7); }
TEST(Compiler, liftClosTypes) {
  c().bind("appC", &appC);
  EXPECT_EQ(c().compileFn<int()>("(\\x.appC(\\y.x*y-x))(7)")(), 42);
}

TEST(Compiler, Parsing) {
  EXPECT_TRUE((c().compileFn<bool(const std::pair<char,char>&)>("p", "p==('\\\\','\\\\')")(std::make_pair('\\','\\'))));
}

TEST(Compiler, ParseTyDefStaging) {
  compile(
    &c(),
    c().readModule(
      "bob = 42\n"
      "type BT = (TypeOf `bob` x) => x\n"
      "type BTI = (TypeOf `newPrim()::BT` x) => x\n"
      "frank :: BTI\n"
      "frank = 3\n"
    )
  );
  EXPECT_EQ(c().compileFn<int()>("frank")(), 3);
}

TEST(Compiler, ccInManyThreads) {
  std::vector<std::thread*> ps;
  size_t badChecks = 0;
  for (size_t p = 0; p < 10; ++p) {
    ps.push_back(new std::thread(([&]() {
      hobbes::cc c;
      badChecks += c.compileFn<int()>("sum([1..100])-100*101/2")(); // just 0, but complex enough to hit many areas of the compiler
    })));
  }
  for (auto *p : ps) { p->join(); delete p; }
  EXPECT_EQ(badChecks, size_t(0));
}

using IArr = std::array<int, 10>;
using IMat = std::array<IArr, 10>;

DEFINE_STRUCT(ArrTest,
  (short,  a),
  (IMat,   xss),
  (double, y)
);

TEST(Compiler, liftStdArray) {
  IArr xs;
  for (size_t i = 0; i < xs.size(); ++i) {
    xs[i] = i;
  }
  EXPECT_EQ((c().compileFn<int(IArr*)>("xs","sum(xs[0:])")(&xs)), 45);

  ArrTest atst;
  atst.a = 0;
  for (size_t i = 0; i < atst.xss.size(); ++i) {
    for (size_t j = 0; j < atst.xss[i].size(); ++j) {
      atst.xss[i][j] = i+j;
    }
  }
  atst.y = 3.14159;
  EXPECT_EQ((c().compileFn<int(ArrTest*)>("s","sum(concat([[x|x<-xs[0:]]|xs<-s.xss[0:]]))")(&atst)), 900);
}

using ctimespanT = std::chrono::duration<int64_t, std::micro>;

std::ostream& operator<<(std::ostream& out, ctimespanT dt) {
  out << *reinterpret_cast<int64_t*>(&dt) << "us";
  return out;
}

TEST(Compiler, liftChronoTimespan) {
  EXPECT_EQ(c().compileFn<ctimespanT()>("20ms")(), std::chrono::milliseconds(20));
  EXPECT_TRUE(c().compileFn<bool(ctimespanT)>("x", "x==20ms")(std::chrono::milliseconds(20)));
}

// verify that types are lifted as expected for values without "return value optimization" (or "copy elision")
TEST(Compiler, liftWithoutRVO) {
  using strref = hobbes::fileref<const hobbes::array<char> *>;

  EXPECT_EQ(c().compileFn<strref(int)>("x", "unsafeCast(42L)")(0).index, strref(42UL).index);
}

