
#include <hobbes/hobbes.H>
#include "test.H"

using namespace hobbes;

DEFINE_STRUCT(
  Bob,
  (int,         x),
  (double,      y),
  (std::string, z),
  (char,        w)
);
Bob bob;

DEFINE_STRUCT(
  DynStructTest,
  (bool,     f),
  (uint64_t, a),
  (uint64_t, b)
);

typedef short short3[3];
DEFINE_STRUCT(
  NM,
  (unsigned long, sz),
  (short3,        ss)
);

DEFINE_STRUCT(
  M0,
  (NM,            c1),
  (unsigned char, c2)
);

typedef NM NM3[3];
DEFINE_STRUCT(
  M,
  (unsigned long, sz),
  (NM3,           ss)
);

typedef char char25[25];
DEFINE_STRUCT(
  GenStruct,
  (int,    f0),
  (bool,   f1),
  (double, f2),
  (char25, f3)
);

static cc& c() {
  static cc x;
  static bool i = false;
  if (!i) {
    bob.x = 42;
    bob.y = 42.2;
    bob.z = "Hello!";
    bob.w = 'c';
    x.bind("bob", &bob);

    // verify some odd alignment cases
    static M m;
    x.bind(".align_m", &m);

    // verify binding correctness in general structs (try to cover odd cases here)
    static GenStruct gs;
    x.bind("genstruct", &gs);
    
    i = true;
  }
  return x;
}

TEST(Structs, Consts) {
  EXPECT_EQ(c().compileFn<int()>("{a=1,b=2,c=3}.b")(), 2);
  EXPECT_EQ(c().compileFn<int()>("{a=1,b={c=2,d={e=3,f=4},g=5},h=6}.b.d.f")(), 4);
  EXPECT_TRUE(c().compileFn<bool()>("(1,\"jimmy\",13L).1 == \"jimmy\"")());
  EXPECT_TRUE(c().compileFn<bool()>("(if (0 == 0) then (1,2) else (3,4)).0 == 1")());

  typedef std::pair<unsigned char, double> FWeight;
  typedef array<FWeight>                   FWeights;
  EXPECT_EQ(c().compileFn<FWeights*()>("[(0XAE, 1.2),(0XBD, 2.4),(0XFF, 5.9)]")()->data[1].second, 2.4);
}

TEST(Structs, Reflect) {
  EXPECT_EQ(c().compileFn<DynStructTest*()>("{f=true, a=42L, b=100L}")()->a, uint64_t(42));
}

TEST(Structs, Functions) {
  EXPECT_EQ(c().compileFn<int()>("(\\x.{a=1,b=x,c=9})(10).b")(), 10);
  EXPECT_EQ(c().compileFn<int()>("(\\r.10+r.x)({y=1,x=100})")(), 110);
}

TEST(Structs, Bindings) {
  EXPECT_TRUE(c().compileFn<bool()>("bob.x == 42")());
  EXPECT_TRUE(c().compileFn<bool()>("bob.y > 42.0 and bob.y < 43.0")());
  EXPECT_TRUE(c().compileFn<bool()>("bob.z == \"Hello!\"")());
  EXPECT_TRUE(c().compileFn<bool()>("bob.w == 'c'")());

  EXPECT_TRUE(c().compileFn<array<char>*()>("show(genstruct)") != 0);
}

TEST(Structs, Assignment) {
  bool assignExn = false;
  try {
    c().compileFn<void()>("bob.x <- 'c'")();
  } catch (std::exception&) {
    assignExn = true;
  }
  EXPECT_TRUE( assignExn && "Expected char assignment to int field to fail to compile (mistake in assignment compilation?)" );

  bool introExn = false;
  try {
    c().compileFn<int()>("{x=1, x='c'}.x");
  } catch (std::exception&) {
    introExn = true;
  }
  EXPECT_TRUE( introExn && "Expected rejection of record introduction with duplicate field names" );

  c().compileFn<void()>("bob.x <- 90")();
  EXPECT_TRUE(c().compileFn<bool()>("bob.x == 90")());

  c().compileFn<void()>("bob.y <- 0.1")();
  EXPECT_TRUE(c().compileFn<bool()>("bob.y > 0.0 and bob.y < 1.0")());

  // assignment between std::string and [char]
  c().compileFn<void()>("bob.z <- \"Word\"")();
  EXPECT_TRUE(c().compileFn<bool()>("bob.z == \"Word\"")());

  // this one will also test correctness of assignment within array comprehensions
  c().compileFn<void()>("let _ = [bob.w <- 'a' | _ <- [0..10]] in ()")();
  EXPECT_TRUE(c().compileFn<bool()>("bob.w == 'a'")());
}

TEST(Structs, Objects) {
  // test array construction of opaque pointers inside structures
  try {
    c().compileFn<void()>("let s = show([bob.z | _ <- [0..10]]) in ()")();
  } catch (std::exception& ex) {
    std::cout << ex.what() << std::endl;
    EXPECT_TRUE(false && "Array construction over opaque pointer struct members failed.");
  }

  // test record/tuple construction of opaque pointers into structures
  EXPECT_TRUE(c().compileFn<bool()>("(bob.z, bob.z) === (bob.z, bob.z)")());
}

TEST(Structs, Prelude) {
  // test prelude generic struct functions (equality/show)
  EXPECT_TRUE(c().compileFn<bool()>("(1, 'c', 3L) == (1, 'c', 3L)")());
  EXPECT_TRUE(c().compileFn<bool()>("show((1,'c',3L)) == \"(1, 'c', 3)\"")());
  EXPECT_TRUE(c().compileFn<bool()>("{x=1, y='c', z=3L} == {x=1, y='c', z=3L}")());

  // test structural subtyping conversion for records
  EXPECT_EQ(c().compileFn<int()>("(convert({w=3,x=0xdeadbeef,y=42S,z=4.45}) :: {y:int,z:double,w:long}).y")(), 42);

  // test ordering in structs and tuples
  EXPECT_TRUE(c().compileFn<bool()>("(1,2,3) < (1,2,4)")());
  EXPECT_TRUE(c().compileFn<bool()>("(1,2,3) <= (1,2,3)")());
  EXPECT_TRUE(c().compileFn<bool()>("{x=1,y=2} < {x=1,y=3}")());
  EXPECT_TRUE(c().compileFn<bool()>("{x=1,y=2} <= {x=1,y=2}")());
}

// weird alignment tests
typedef char l1ft[48];
DEFINE_STRUCT(AlignA,
  (l1ft, data)
);

typedef char ibft[50];
DEFINE_STRUCT(AlignB,
  (ibft, data)
);

TEST(Structs, LiftTuple) {
  hobbes::tuple<short, int, std::string> p(42, 314159, "jimmy");
  EXPECT_TRUE(c().compileFn<bool(const hobbes::tuple<short,int,std::string>&)>("p", "p==(42,314159,\"jimmy\")")(p));
}

typedef variant<unit, AlignA> MaybeAlignA;
typedef variant<unit, AlignB> MaybeAlignB;

DEFINE_STRUCT(AlignTest,
  (MaybeAlignA, ma),
  (MaybeAlignB, mb)
);
const AlignTest* makeAT() { return new AlignTest(); }

struct PadTest {
  int  x;
  long y;
};

TEST(Structs, Alignment) {
  c().bind("makeAT", &makeAT);
  EXPECT_TRUE(true);

  Record::Members ms;
  ms.push_back(Record::Member("x", lift<int>::type(c()), 0));
  ms.push_back(Record::Member("y", lift<long>::type(c()), static_cast<int>(reinterpret_cast<size_t>(&reinterpret_cast<PadTest*>(0)->y))));
  MonoTypePtr pty(Record::make(Record::withExplicitPadding(ms)));

  PadTest p;
  p.x = 1; p.y = 42;
  c().bind(polytype(pty), "padTest", &p);
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>("show(padTest)")()), "{x=1, y=42}");
}

