
#include <hobbes/hobbes.H>
#include "test.H"

using namespace hobbes;

extern const char leftLBL[];
extern const char rightLBL[];
using leftCtor = label<leftLBL, int>;
using rightCtor = label<rightLBL, double>;
const char leftLBL[] = "left";
const char rightLBL[] = "right";

int intCastVariant(const variant<leftCtor, rightCtor>* bv) {
  if (const auto* lv = bv->get<leftCtor>()) {
    return lv->value;
  } else {
    return static_cast<int>(bv->get<rightCtor>()->value);
  }
}

int sumCtorID(const variant<char, unsigned char>* bv) {
  if (bv->get<char>() != nullptr) {
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

TEST(Variants, NoDuplicateConstructorNames) {
  bool introExn = false;
  try {
    c().compileFn<void()>("print(|foo=1|::|foo:int,foo:[char]|)");
  } catch (std::exception&) {
    introExn = true;
  }
  EXPECT_TRUE( introExn && "Expected rejection of variant introduction with duplicate constructor names" );
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

  using MChar = variant<unit, char>;
  using MUuid = variant<unit, uuid>;

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

DEFINE_VARIANT(Item,
  (vehicle, int),
  (food, float)
);

TEST(Variants, VariantRecord) {
  Item aVehicle = Item::vehicle(123);
  Item aFood = Item::food(321.123);

  EXPECT_EQ((c().compileFn<int(const Item*)>("x", "match x with | |vehicle=v| -> v | |food=_| -> 321")(&aVehicle)), 123);
  EXPECT_EQ((c().compileFn<int(const Item*)>("x", "match x with | |vehicle=v| -> v | |food=_| -> 321")(&aFood)), 321);
}

TEST(Variants, WithFunctions) {
  EXPECT_TRUE(c().compileFn<bool()>("either((\\|1=f|.f(1L,0L))(just(if (0 > 0) then llt else lgt)), false, id)")());
}

using BadT = variant<unit, std::pair<const char *, size_t> *>;
TEST(Variants, DontLiftInlineRefs) {
  EXPECT_NEQ(show(lift<const BadT*>::type(c())), "(() + (<char> * long))");
}

TEST(Variants, Generic) {
  EXPECT_TRUE(c().compileFn<bool()>("variantApp(|car=6L|::|car:long,house:[char],dog:double|,{car=toClosure(\\x.x+x*x),house=toClosure(length),dog=toClosure(\\_.0L)}) == 42")());
}

TEST(Variants, Trunc) {
  EXPECT_TRUE(c().compileFn<bool()>("trunc(|Red=42|::|Blue:[char],Red:int,Green:(double*double)|)===|Red|")());
}

template <typename T, T v0, T v1, T v2> struct PEnumBase {
  static constexpr bool is_hmeta_enum = true;
  using rep_t = T;
  enum class Enum : T { e0 = v0, e1 = v1, e2 = v2 };
  Enum value;
  PEnumBase() : value(Enum::e0) {}
  explicit PEnumBase(Enum e) : value(e) {}
  static std::vector<std::pair<std::string, T>> meta() {
    return {std::make_pair("e0", v0), std::make_pair("e1", v1),
            std::make_pair("e2", v2)};
  }
};

struct PECharNContinous : PEnumBase<char, '0', '1', '2'> {};
static_assert(sizeof(PECharNContinous::rep_t) == sizeof(PECharNContinous), "");
DEFINE_PACKED_ENUM(PECharContinous, char, (e0), (e1), (e2));
DEFINE_STRUCT(SP_C_Char, (PECharContinous, pe), (PECharNContinous, pen),
              (int, x));

namespace hobbes {
template<> struct lift<PECharContinous *, false>
    : liftEnum<PECharContinous, PECharContinous::rep_t> {};
template<> struct lift<PECharNContinous *, false>
    : liftEnum<PECharNContinous, PECharNContinous::rep_t> {};
} // namespace hobbes

TEST(Variants, PEnumsChar) {
  struct PECharNFail : PEnumBase<char, '2', '4', '2'> {};
  PECharNFail penf;
  // raise from liftEnum
  EXPECT_EXCEPTION_MSG((c().compileFn<const array<char>*(PECharNFail)>("x", "show(x)")(PECharNFail{})), "(50)"); // 50 == '2';

  SP_C_Char spcc;
  spcc.pe.value = PECharContinous::Enum::e2;
  spcc.pen.value = PECharNContinous::Enum::e1;
  spcc.x = 42;

  cc x;
  x.bind("spcc", &spcc);
  compile(&x, x.readModule(R"(
type e012 = ((penum char |e0('0'), e1('1'), e2('2')|))

newpen :: e012
newpen = unsafeCast('0')::e012
)"));

  EXPECT_TRUE(x.compileFn<bool()>("show(spcc.pe) == \"|e2|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(spcc.pen) == \"|e1|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(spcc) == \"{pe=|e2|, pen=|e1|, x=42}\"")());

  // show with pre-defined type
  EXPECT_TRUE(x.compileFn<bool()>("show(|e1|::e012) == \"|e1|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(unsafeCast('2')::e012) == \"|e2|\"")());

  // show with type defined on the fly
  EXPECT_TRUE(x.compileFn<bool()>("show(unsafeCast('A')::((penum char |AP('A'), BP('B')|))) == \"|AP|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(|CP|::((penum char |CP(3), DP(4), EP(5)|))) == \"|CP|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(unsafeCast(unsafeCast(0X01)::char)::((penum char |AN, BN|))) == \"|BN|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(|CN|::((penum char |CN, DN, EN|))) == \"|CN|\"")());

  // either all or none have custom defined values
  EXPECT_EXCEPTION_MSG(x.compileFn<void()>("let _ = |FP|::((penum char |FP('F'), GP, HP('H')|)) in ()")(), R"(syntax error, unexpected ",", expecting ()");

  // value must be unique
  EXPECT_EXCEPTION_MSG(x.compileFn<void()>("let _ = |GN|::((penum char |FN('O'), GN('P'), HN('O')|)) in ()")(), "(79)"); // 79 == 'O'

  // write to a penum defined in C++ land
  x.compileFn<void()>("spcc.pen <- unsafeCast('2')::e012")();
  EXPECT_TRUE(x.compileFn<bool()>("show(spcc.pen) == \"|e2|\"")());
  x.compileFn<void()>("spcc.pen <- |e1|::e012")();
  EXPECT_TRUE(x.compileFn<bool()>("show(spcc.pen) == \"|e1|\"")());

  // write to a penum defined in hobbes
  EXPECT_TRUE(x.compileFn<bool()>("show(newpen) == \"|e0|\"")());
  x.compileFn<void()>("newpen <- unsafeCast('1')::e012")();
  EXPECT_TRUE(x.compileFn<bool()>("show(newpen) == \"|e1|\"")());
  x.compileFn<void()>("newpen <- |e2|::e012")();
  EXPECT_TRUE(x.compileFn<bool()>("show(newpen) == \"|e2|\"")());
}

struct PEShortNContinous : PEnumBase<short, 3, 4, 7> {};
static_assert(sizeof(PEShortNContinous::rep_t) == sizeof(PEShortNContinous), "");
DEFINE_PACKED_ENUM(PEShortContinous, short, (e0), (e1), (e2));
DEFINE_STRUCT(SP_C_Short, (PEShortContinous, pe), (PEShortNContinous, pen),
              (int, x));

namespace hobbes {
template<> struct lift<PEShortContinous *, false>
    : liftEnum<PEShortContinous, PEShortContinous::rep_t> {};
template<> struct lift<PEShortNContinous *, false>
    : liftEnum<PEShortNContinous, PEShortNContinous::rep_t> {};
} // namespace hobbes

TEST(Variants, PEnumsShort) {
  SP_C_Short spcc;
  spcc.pe.value = PEShortContinous::Enum::e2;
  spcc.pen.value = PEShortNContinous::Enum::e1;
  spcc.x = 42;

  cc x;
  x.bind("spcc", &spcc);
  compile(&x, x.readModule(R"(
type e347 = ((penum short |e0(3), e1(4), e2(7)|))

newpen :: e347
newpen = unsafeCast(3S)::e347
)"));

  EXPECT_TRUE(x.compileFn<bool()>("show(spcc.pe) == \"|e2|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(spcc.pen) == \"|e1|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(spcc) == \"{pe=|e2|, pen=|e1|, x=42}\"")());

  // show with pre-defined type
  EXPECT_TRUE(x.compileFn<bool()>("show(|e1|::e347) == \"|e1|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(unsafeCast(7S)::e347) == \"|e2|\"")());

  // show with type defined on the fly
  EXPECT_TRUE(x.compileFn<bool()>("show(unsafeCast(1S)::((penum short |AP(1), BP(2)|))) == \"|AP|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(|CP|::((penum short |CP(3), DP(4), EP(5)|))) == \"|CP|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(unsafeCast(1S)::((penum short |AN, BN|))) == \"|BN|\"")());
  EXPECT_TRUE(x.compileFn<bool()>("show(|CN|::((penum short |CN, DN, EN|))) == \"|CN|\"")());

  // either all or none have custom defined values
  EXPECT_EXCEPTION_MSG(x.compileFn<void()>("let _ = |FP|::((penum char |FP(3), GP, HP(5)|)) in ()")(), R"(syntax error, unexpected ",", expecting ()");

  // value must be unique
  EXPECT_EXCEPTION_MSG(x.compileFn<void()>("let _ = |GN|::((penum short |FN(79), GN(80), HN(79)|)) in ()")(), "(79)");

  // write to a penum defined in C++ land
  x.compileFn<void()>("spcc.pen <- unsafeCast(7S)::e347")();
  EXPECT_TRUE(x.compileFn<bool()>("show(spcc.pen) == \"|e2|\"")());
  x.compileFn<void()>("spcc.pen <- |e1|::e347")();
  EXPECT_TRUE(x.compileFn<bool()>("show(spcc.pen) == \"|e1|\"")());

  // write to a penum defined in hobbes
  EXPECT_TRUE(x.compileFn<bool()>("show(newpen) == \"|e0|\"")());
  x.compileFn<void()>("newpen <- unsafeCast(4S)::e347")();
  EXPECT_TRUE(x.compileFn<bool()>("show(newpen) == \"|e1|\"")());
  x.compileFn<void()>("newpen <- |e2|::e347")();
  EXPECT_TRUE(x.compileFn<bool()>("show(newpen) == \"|e2|\"")());
}
