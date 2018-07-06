
#include <hobbes/hobbes.H>
#include "test.H"

using namespace hobbes;

// classes for testing member function binding
class Top {
public:
  Top(int x, double y, int z) : x(x), y(y), z(z) { }

  virtual int foo(int, double, char) = 0;

  int    getX() const { return this->x; }
  double getY() const { return this->y; }
  int    getZ() const { return this->z; }
private:
  int    x;
  double y;
  int    z;
};

class Left : public virtual Top {
public:
  Left(int x, double y, int z) : Top(x, y, z) { }

  int foo(int,double,char) { return 10; }
  int bar()                { return 1111; }
};

class Right : public virtual Top {
public:
  Right(int x, double y, int z) : Top(x, y, z) { }

  int foo(int,double,char) { return 20; }
  int baz()                { return 2222; }
};

class Bottom : public Left, public Right {
public:
  Bottom(int x, double y, int z) : Top(x, y, z), Left(x*z, y*y, z*x), Right(x*x, y*y, z*z) { }

  int foo(int,double,char) { return 30; }
  int zzz()                { return 3333; }
};

// verify compilation of array access + subtyping (which was once very wrong)
class ArrObjV {
public:
  int age() {
    return 42;
  }
};

array<ArrObjV*>* arrobjs(int n) {
  array<ArrObjV*>* r = reinterpret_cast<array<ArrObjV*>*>(memalloc(sizeof(long) + sizeof(ArrObjV) * n));
  r->size = n;
  for (int i = 0; i < n; ++i) {
    new (r->data + n) ArrObjV();
  }
  return r;
}

// verify compilation of simple single-inheritance
class SIBase {
public:
  virtual ~SIBase() { }

  virtual int doblah() {
    return 42;
  }

  void doit() {
  }
};

class SIDerived : public SIBase {
public:
  int doblah() {
    return 20;
  }
};

static cc& c() {
  static cc x;
  static bool i = false;
  if (!i) {
    i = true;

    // init bindings
    x.bind("aoage",   memberfn(&ArrObjV::age));
    x.bind("arrobjs", &arrobjs);

    static Left*   l = new Left  (1, 2.0, 3);
    static Right*  r = new Right (4, 5.0, 6);
    static Bottom* b = new Bottom(7, 8.0, 9);

    x.bind("foo",  memberfn(&Top::foo));
    x.bind("getX", memberfn(&Top::getX));
    x.bind("getY", memberfn(&Top::getY));
    x.bind("getZ", memberfn(&Top::getZ));

    x.bind("bar", memberfn(&Left::bar));
    x.bind("baz", memberfn(&Right::baz));
    x.bind("zzz", memberfn(&Bottom::zzz));

    x.bind("l", l);
    x.bind("r", r);
    x.bind("b", b);

    static SIDerived* sid = new SIDerived();
    x.bind("doblah", memberfn(&SIBase::doblah));
    x.bind("doit",   memberfn(&SIBase::doit));
    x.bind("sid",    sid);

// disable implicit coercions under clang -- for now it seems
// to be impossible to get __class_info details out of
// clang's RTTI like we do with GCC
#ifndef __clang__
    compile(&x, x.readModule(
      "class ObjReader a b where\n"
      "  objRead :: (a, b) -> [char]\n"
      "instance ObjReader <Bottom> int where\n"
      "  objRead b _ = show(b.getX())\n"
      "instance ObjReader <Bottom> double where\n"
      "  objRead b _ = show(b.getY())\n"
      "instance (Show a) => ObjReader () a where\n"
      "  objRead _ i = show(i)\n"
    ));
#endif
  }
  return x;
}

#ifndef __clang__
TEST(Objects, Inheritance) {
  EXPECT_TRUE(c().compileFn<bool()>("l.foo(42, 3.14159, 'c') == 10")());
  EXPECT_TRUE(c().compileFn<bool()>("r.foo(42, 3.14159, 'c') == 20")());
  EXPECT_TRUE(c().compileFn<bool()>("b.foo(42, 3.14159, 'c') == 30")());

  EXPECT_TRUE(c().compileFn<bool()>("l.getX() == 1")());
  EXPECT_TRUE(c().compileFn<bool()>("r.getX() == 4")());
  EXPECT_TRUE(c().compileFn<bool()>("b.getX() == 7")());

  EXPECT_TRUE(c().compileFn<bool()>("l.getY() == 2.0")());
  EXPECT_TRUE(c().compileFn<bool()>("r.getY() == 5.0")());
  EXPECT_TRUE(c().compileFn<bool()>("b.getY() == 8.0")());

  EXPECT_TRUE(c().compileFn<bool()>("l.getZ() == 3")());
  EXPECT_TRUE(c().compileFn<bool()>("r.getZ() == 6")());
  EXPECT_TRUE(c().compileFn<bool()>("b.getZ() == 9")());

  EXPECT_TRUE(c().compileFn<bool()>("l.bar() == 1111")());
  EXPECT_TRUE(c().compileFn<bool()>("r.baz() == 2222")());
  EXPECT_TRUE(c().compileFn<bool()>("b.zzz() == 3333")());

  EXPECT_TRUE(c().compileFn<bool()>("sid.doblah() == 20")());
}

TEST(Objects, Using) {
  Bottom b(7, 8.0, 9);
  EXPECT_TRUE((c().compileFn<bool(const Bottom*)>("b", "do { b.bar(); b.baz(); objRead(b, 10); objRead(b, 3.14159); return objRead(b, 42) == \"7\" }")(&b)));
}
#endif

TEST(Objects, Arrays) {
  EXPECT_EQ(c().compileFn<int()>("aoage((arrobjs(9))[0])")(), 42);
}

class Undef;
size_t undefCount(const Undef*) { return 42; }

TEST(Objects, FwdDecl) {
  c().bind("undefCount", &undefCount);
  EXPECT_EQ((c().compileFn<size_t(const Undef*)>("u", "undefCount(u)")(reinterpret_cast<const Undef*>(0))), size_t(42));
}

