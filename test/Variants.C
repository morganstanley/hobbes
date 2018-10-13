
#include <hobbes/hobbes.H>
#include "test.H"

using namespace hobbes;

extern const char leftLBL[];
extern const char rightLBL[];
typedef label<leftLBL, int> leftCtor;
typedef label<rightLBL, double> rightCtor;
const char leftLBL[] = "left";
const char rightLBL[] = "right";

int intCastVariant(const variant<leftCtor, rightCtor>* bv) {
  if (const leftCtor* lv = bv->get<leftCtor>()) {
    return lv->value;
  } else {
    return static_cast<int>(bv->get<rightCtor>()->value);
  }
}

int sumCtorID(const variant<char, unsigned char>* bv) {
  if (bv->get<char>()) {
    return 0;
  } else {
    return 1;
  }
}

static cc& c() {
  static cc x;
  static bool i = false;
  if (!i) {
    x.bind("intCastVariant", &intCastVariant);
    x.bind("sumCtorID",      &sumCtorID);

    i = true;
  }
  return x;
}

TEST(Variants, Basic) {
  EXPECT_EQ(c().compileFn<int()>("case |x=1| of |x=x+1,y=y|")(), 2);
  EXPECT_EQ(c().compileFn<int()>("case (\\x.|x=x|)(100) of |x=x|")(), 100);
  EXPECT_EQ(c().compileFn<int()>("case (\\x.|p=(x,x*x)|)(9) of |p=p.1|")(), 81);
  EXPECT_EQ(c().compileFn<int()>("(\\v.(case v of |0:x=x| default 2))(|1='c'|::int+char)")(), 2);
}

TEST(Variants, Destruct) {
  EXPECT_TRUE(c().compileFn<bool()>("show(|foo=9|::|foo:int,bar:double,baz:short|) == \"|foo=9|\"")());
}

TEST(Variants, Arrays) {
  EXPECT_TRUE(c().compileFn<bool()>("show([|0=(4,5,6)|::(int*int*int)+(int*int), |0=(7,8,9)|, |1=(25,25)|]) == \"[|0=(4, 5, 6)|, |0=(7, 8, 9)|, |1=(25, 25)|]\"")());
  EXPECT_EQ(c().compileFn<int()>("variantSplit([|a|::|a:(),b:int|,|b=99|][0], toClosure(\\x.0), toClosure(\\x.1))")(), 0);
}

TEST(Variants, Binding) {
  EXPECT_EQ(c().compileFn<int()>("intCastVariant(|left=42|)")(), 42);
  EXPECT_EQ(c().compileFn<int()>("intCastVariant(|right=88.8|)")(), 88);
  EXPECT_EQ(c().compileFn<int()>("sumCtorID(|0='c'|)")(), 0);
  EXPECT_EQ(c().compileFn<int()>("sumCtorID(|1=0Xde|)")(), 1);
}

TEST(Variants, MemoryLayout) {
  typedef unsigned char uuid[16];

  typedef variant<unit, char> MChar;
  typedef variant<unit, uuid> MUuid;

  EXPECT_EQ(sizeof(MChar), sizeOf(lift<const MChar*>::type(nulltdb)));
  EXPECT_EQ(sizeof(MUuid), sizeOf(lift<const MUuid*>::type(nulltdb)));
}

DEFINE_ENUM(Color,
  (Red),
  (Green),
  (Blue)
);

DEFINE_PACKED_ENUM(PColor, uint8_t, (Red), (Green), (Blue));

TEST(Variants, Enums) {
  Color red   = Color::Red();
  Color green = Color::Green();
  Color blue  = Color::Blue();

  EXPECT_EQ((c().compileFn<int(const Color*)>("x", "case x of |Red=0,Green=1,Blue=2|")(&red)),   0);
  EXPECT_EQ((c().compileFn<int(const Color*)>("x", "case x of |Red=0,Green=1,Blue=2|")(&green)), 1);
  EXPECT_EQ((c().compileFn<int(const Color*)>("x", "case x of |Red=0,Green=1,Blue=2|")(&blue)),  2);

  EXPECT_EQ((makeStdString(c().compileFn<const array<char>*(PColor pc)>("x", "show(x)")(PColor::Red()))), "|Red|");
}

TEST(Variants, WithFunctions) {
  EXPECT_TRUE(c().compileFn<bool()>("either((\\|1=f|.f(1L,0L))(just(if (0 > 0) then llt else lgt)), false, id)")());
}

typedef variant<unit, std::pair<const char*, size_t>*> BadT;
TEST(Variants, DontLiftInlineRefs) {
  EXPECT_NEQ(show(lift<const BadT*>::type(c())), "(() + (<char> * long))");
}

TEST(Variants, Generic) {
  EXPECT_TRUE(c().compileFn<bool()>("variantApp(|car=6L|::|car:long,house:[char],dog:double|,{car=toClosure(\\x.x+x*x),house=toClosure(length),dog=toClosure(\\_.0L)}) == 42")());
}

