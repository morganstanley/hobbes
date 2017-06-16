#include <hobbes/hobbes.H>
#include <hobbes/lang/tylift.H>
#include "test.H"

using namespace hobbes;
static cc& c() { static __thread cc* x = 0; if (!x) { x = new cc(); } return *x; }

typedef std::pair<const char*, size_t> BufferView;

TEST(Compiler, compileToSupportsMoreThanSixArgs) {
  std::string expression =
    "match vegetables spices fruits meat cheese technique occasion skill drink side with \n"
    "| \"carrots\" \"parsley\" \"lemon\" \"ossobuco\" _ \"braising\" \"everyday\" \"basic\" \"wine\" \"polenta\" -> 10 \n"
    "| _ _ _ _ _ _ _ _ _ _ -> -1";

  typedef int (*MatchFunPtr)(const BufferView*,   // vegetables
                             const BufferView*,   // spices
                             const BufferView*,   // fruits
                             const BufferView*,   // meat
                             const BufferView*,   // cheese
                             const BufferView*,   // technique
                             const BufferView*,   // occasion
                             const BufferView*,   // skill
                             const BufferView*,   // drink
                             const BufferView*);  // side
  
  hobbes::cc compiler;

  MatchFunPtr funPtr = hobbes::compileTo<MatchFunPtr>(
    &compiler,
    hobbes::list<std::string>("vegetables", "spices", "fruits",
                              "meat","cheese", "technique",
                              "occasion", "skill", "drink", "side"),
    expression);
  
  EXPECT_TRUE(NULL != funPtr);
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

