
#include <hobbes/hobbes.H>
#include <hobbes/db/file.H>
#include <hobbes/db/series.H>
#include <hobbes/db/signals.H>
#include <hobbes/fregion.H>
#include <hobbes/cfregion.H>
#include "test.H"

#include <thread>

using namespace hobbes;
static cc& c() { static cc x; return x; }

template <typename T>
  T sum(const T* b, const T* e) {
    T r = T(0);
    while (b != e) {
      r += *b;
      ++b;
    }
    return r;
  }

template <typename T>
  T sum(const array<T>* vs) {
    return sum<T>(vs->data, vs->data + vs->size);
  }

template <typename T>
  void initSeq(array<T>* vs, size_t k, T i, T e) {
    while (i != e) {
      vs->data[k++] = i;
      ++i;
    }
    vs->size = k;
  }

template <typename T>
  T sumFromTo(T s, T e) {
    T n = e - s;
    return ((n*(n+1))/2) - ((s*(s+1))/2);
  }

static bool fileExists(const char* fileName) {
  FILE* fd = fopen(fileName, "r");
  if (fd != nullptr) {
    fclose(fd);
    return true;
  } else {
    return false;
  }
}

static std::string mkFName() {
  return uniqueFilename("/tmp/hdb-unittest", ".db");
}

TEST(Storage, uniqueFilenames) {
  // Create file1
  std::string file1 = uniqueFilename("/tmp/hfiles-unittest", ".log.staging");
  EXPECT_TRUE(fileExists(file1.c_str()));

  // Create file2
  std::string file2;
  try {
    file2 = uniqueFilename("/tmp/hfiles-unittest", ".log.staging");
  }
  catch(...) {
    unlink(file1.c_str());
    throw;
  }
  EXPECT_TRUE(fileExists(file2.c_str()));

  // Rename file2 to file 3
  std::string file3;
  try{
    file3 = moveToUniqueFilename(file2, "/tmp/hfiles-unittest", ".log");
  }
  catch(...) {
    unlink(file2.c_str());
    unlink(file1.c_str());
    throw;
  }
  EXPECT_FALSE(fileExists(file2.c_str()));
  EXPECT_TRUE(fileExists(file3.c_str()));

  // Clean up the files
  unlink(file3.c_str());
  unlink(file1.c_str());
}

TEST(Storage, Create) {
  std::string fname = mkFName();
  try {
    // make sure that we can create a file and write some stuff to it
    writer f(fname);

    int n = 1020;
    array<int>* vs = f.define<int>("vs", n);
    EXPECT_TRUE(vs != nullptr);
    EXPECT_TRUE(vs->size == 1020);

    initSeq(vs, 0, 0, n);
    EXPECT_EQ(sum(vs), sumFromTo(0, 1019));

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

typedef char fixarray[8];

DEFINE_STRUCT(SeriesTest,
  (int,                x),
  (double,             y),
  (const array<char>*, z),
  (bool,               b),
  (fixarray,           v)
              
);

TEST(Storage, RawSeriesAPI) {
  std::string fname = mkFName();
  try {
    // make sure that we can write struct data through the series API correctly
    writer f(fname);
    series<SeriesTest> ss(&c(), &f, "series_test");

    for (size_t i = 0; i < 10; ++i) {
      SeriesTest st;
      st.x = i;
      st.y = 3.14159 * static_cast<double>(i);
      st.z = makeString("string_" + str::from(i));
      st.b = true;
      memcpy(st.v, "12345678", 8);
      ss(st);
    }

    c().define("f", "inputFile :: (LoadFile \"" + fname + "\" w) => w");
    EXPECT_TRUE(c().compileFn<bool()>("[x|{x=x}<-f.series_test][:0] == [0..9]")());

    ss.clear();
    for (size_t i = 0; i < 5; ++i) {
      SeriesTest st;
      st.x = i;
      st.y = 3.14159 * static_cast<double>(i);
      st.z = makeString("string_" + str::from(i));
      st.b = true;
      memcpy(st.v, "12345678", 8);

      ss(st);
    }
    EXPECT_TRUE(c().compileFn<bool()>("[x|{x=x}<-f.series_test][:0] == [0..4]")());

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, CSeriesAPI) {
  std::string fname = mkFName();
  try {
    // make sure that we can write struct data through the series API correctly
    writer f(fname);
    series<SeriesTest> ss(&c(), &f, "series_test", 100, StoredSeries::Compressed);

    for (size_t i = 0; i < 1000; ++i) {
      SeriesTest st;
      st.x = i;
      st.y = 3.14159 * static_cast<double>(i);
      st.z = makeString("string_" + str::from(i));
      st.b = i % 2 == 0;
      memcpy(st.v, st.z->data, 8);
      ss(st);
    }

    hobbes::cc c;
    c.define("cf", "inputFile :: (LoadFile \"" + fname + "\" w) => w");
    EXPECT_TRUE(c.compileFn<bool()>("[x|{x=x}<-cf.series_test] == [0..999]")());
    EXPECT_TRUE(c.compileFn<bool()>("[b|{b=b}<-cf.series_test][:0] == [x%2==0|{x=x}<-cf.series_test][:0]")());
    EXPECT_TRUE(c.compileFn<bool()>("[v|{v=v}<-cf.series_test][:0] == [z[0:8]|{z=z}<-cf.series_test][:0]")());
    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, Modify) {
  std::string fname = mkFName();
  try {
    // make a file with a sequence in it, which we extend with a second writer, then make sure that the unified view is visible
    writer f0(fname);
    int n = 1020;
    array<int>* vs = f0.define<int>("vs", n);
    initSeq(vs, 0, 0, n / 2);
    EXPECT_EQ(sum(vs), sumFromTo(0, (1020/2) - 1));

    writer f1(fname);
    array<int>* vs2 = f1.lookup<array<int>*>("vs");
    EXPECT_EQ(sum(vs2), sumFromTo(0, (1020/2) - 1));
    initSeq(vs2, n / 2, n / 2, n);

    reader f2(fname);
    array<int>* vs3 = f2.lookup<array<int>*>("vs");
    EXPECT_EQ(sum(vs3), sumFromTo(0, 1019));

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, Inference) {
  cc c;
  EXPECT_EQ(show(c.unsweetenExpression(c.readExpr("newPrim() :: (StoreInto () {a:int,b:[char]} z) => z"))->type()), "{ a:int, b:[char]@? }");
}

TEST(Storage, Scripting) {
  std::string fname = mkFName();
  try {
    static int n = 1020;

    cc wc;
    wc.define("f", "writeFile(\"" + fname + "\") :: ((file _ {vs:[int]@?}))");
    wc.compileFn<void()>("f.vs <- allocateArray(" + str::from(n) + "L)")();
    array<int>* vs = wc.compileFn<array<int>*()>("load(f.vs)")();
    initSeq(vs, 0, 0, n);

    cc rc;
    rc.define("f", "readFile(\"" + fname + "\") :: ((file _ {vs:[int]@?}))");
    EXPECT_EQ(rc.compileFn<int()>("sum(load(f.vs))")(), sumFromTo(0, n - 1));

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, Alignment) {
  std::string fname = mkFName();
  try {
    writer f(fname);
    auto*        s = f.define<short>("s");
    int*          i = f.define<int>("i");
    auto*       d = f.define<double>("d");
    array<int>*   a = f.define<int>("a", 100);
    auto* r = f.define<fileref<int>>("r");

    EXPECT_EQ(reinterpret_cast<size_t>(s)%sizeof(short),  size_t(0));
    EXPECT_EQ(reinterpret_cast<size_t>(i)%sizeof(int),    size_t(0));
    EXPECT_EQ(reinterpret_cast<size_t>(d)%sizeof(double), size_t(0));
    EXPECT_EQ(reinterpret_cast<size_t>(a)%sizeof(size_t), size_t(0));
    EXPECT_EQ(reinterpret_cast<size_t>(r)%sizeof(size_t), size_t(0));
    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

DEFINE_STRUCT(
  Point,
  (double, x),
  (double, y)
);

DEFINE_STRUCT(
  Box,
  (Point, topLeft),
  (Point, botRight)
);

DEFINE_STRUCT(
  Food,
  (std::string, name),
  (double,      price),
  (double,      weight),
  (Box,         packAs)
);

DEFINE_VARIANT(
  Vehicle,
  (Car,   std::string),
  (Feet,  int),
  (Cat,   double),
  (Horse, short)
);

DEFINE_VARIANT_WITH_LABELS(
  IntOrDouble,
  (left,  .f0, int),
  (right, .f1, double)
);

void expectAligned(fregion::imagefile* f, const std::string& n, size_t alignment) {
  auto b = f->bindings.find(n);
  if (b == f->bindings.end()) {
    throw std::runtime_error("File doesn't define '" + n + "', test error");
  }
  if (b->second.offset%alignment != 0) {
    throw std::runtime_error("The data for '" + n + "' is stored at " + str::from(b->second.offset) + " but should be aligned to " + str::from(alignment) + " (slipped by " + str::from(b->second.offset%alignment) + ")");
  }
}

double randV(double s) {
  return s * (static_cast<double>(rand())/static_cast<double>(RAND_MAX));
}

TEST(Storage, FRegionAlignment) {
  std::string fname = mkFName();
  fregion::writer f(fname);
# define FRALIGN_TEST(n) \
    f.define(#n, n); \
    expectAligned(f.fileData(), #n, fregion::store<decltype(n)>::alignment())

  try {
    std::array<int, 42> someInts;
    FRALIGN_TEST(someInts);

    int oneInt{};
    FRALIGN_TEST(oneInt);

    std::pair<int,double> intAndDouble;
    FRALIGN_TEST(intAndDouble);
  
    Food favFood;
    favFood.name = "chicken";
    favFood.price = 3.14159;
    favFood.weight = 4.2;
    favFood.packAs.topLeft.x  = -20;
    favFood.packAs.topLeft.y  =  20;
    favFood.packAs.botRight.x =  20;
    favFood.packAs.botRight.y = -20;
    FRALIGN_TEST(favFood);
    
    std::vector<Food> availFoods;
    for (size_t i = 0; i < 10; ++i) {
      static std::string names[]  = { "hot dog", "hamburger", "samosa", "egg drop soup", "pizza", "salad", "cake", "ice cream", "haggis", "bagel" };
      static double      prices[] = {       3.2,         9.1,      2.3,            10.4,     1.1,     7.0,   11.9,         0.5,    -31.4,     4.1 };

      Food f;
      f.name  = names[i%(sizeof(names)/sizeof(names[0]))];
      f.price = prices[i%(sizeof(prices)/sizeof(prices[0]))];
      f.weight = randV(100.0);
      f.packAs.topLeft.x  = randV(100.0);
      f.packAs.topLeft.y  = randV(100.0);
      f.packAs.botRight.x = randV(100.0);
      f.packAs.botRight.y = randV(100.0);
      availFoods.push_back(f);
    }
    FRALIGN_TEST(availFoods);

    Vehicle favVehicle = Vehicle::Feet(20);
    FRALIGN_TEST(favVehicle);

    std::vector<Vehicle> vehicles;
    for (size_t i = 0; i < 20; ++i) {
      switch (static_cast<int>(randV(1000.0)) % 4) {
      case 0:
        vehicles.push_back(Vehicle::Car("red"));
        break;
      case 1:
        vehicles.push_back(Vehicle::Feet(randV(70)));
        break;
      case 2:
        vehicles.push_back(Vehicle::Cat(randV(99)));
        break;
      case 3:
        vehicles.push_back(Vehicle::Horse(randV(42)));
        break;
      }
    }
    FRALIGN_TEST(vehicles);

    IntOrDouble intOrDouble = IntOrDouble::left(42);
    FRALIGN_TEST(intOrDouble);
 
    std::array<IntOrDouble, 20> someIntsOrDoubles;
    FRALIGN_TEST(someIntsOrDoubles);

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, GrowAwayFromReader) {
  std::string fname = mkFName();
  try {
    // start the writer with a small array
    cc wc;
    wc.define("f", "writeFile(\"" + fname + "\") :: ((file _ {vs:[int]@?}))");
    wc.compileFn<void()>("f.vs <- allocateArray(1L)")();
    wc.compileFn<void()>("do { vs = load(f.vs); vs[0] <- 1; unsafeSetLength(vs, 1L); }")();

    // start the reader and bind file variables
    cc rc;
    rc.define("f", "readFile(\"" + fname + "\") :: ((file _ {vs:[int]@?}))");

    // at this point we've got an array of length 1 that sums to 1
    EXPECT_TRUE(rc.compileFn<bool()>("length(load(f.vs)) == 1 and sum(load(f.vs)) == 1")());

    // now update the file so that it grows away from the reader's current state
    //  (the new array will be filled with 0s automatically)
    size_t nlen = 10000;
    wc.compileFn<void()>("f.vs <- allocateArray(" + str::from(nlen) + "L)")();
    wc.compileFn<void()>("do { vs = load(f.vs); vs[" + str::from(nlen - 1) + "] <- 8675309; unsafeSetLength(vs, " + str::from(nlen) + "L); }")();

    // and make sure that the reader sees this new state correctly
    EXPECT_TRUE(rc.compileFn<bool()>("length(load(f.vs)) == " + str::from(nlen) + " and sum(load(f.vs)) == 8675309")());

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, EarlyStaging) {
  std::string fname = mkFName();
  try {
    cc wc;
    wc.define("f", "writeFile(\"" + fname + "\") :: ((file _ {vs:[int]@?}))");
    wc.compileFn<void()>("f.vs <- allocateArray(100L)")();

    cc rc;
    rc.define("f", "inputFile :: (LoadFile \"" + fname + "\" x) => x");
    EXPECT_TRUE(rc.compileFn<bool()>("length(load(f.vs)) == 100")());

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

// this test is currently disabled on Linux because our CI environment suddenly made inotify unreliable
// it could be related to this: https://william-yeh.net/post/2019/06/inotify-in-containers/
#ifndef BUILD_LINUX
TEST(Storage, ScriptSignals) {
  std::string fname = mkFName();
  try {
    // start the writer with an array
    cc wc;
    wc.define("f", "writeFile(\"" + fname + "\") :: ((file _ {vs:[int]@?}))");
    wc.compileFn<void()>("do{f.vs <- allocateArray(1000L);unsafeSetLength(load(f.vs),0L);}")();
    
    wc.define("pushv", "\\f vs v.do { lvs = load(vs); lvs[length(lvs)] <- v; unsafeSetLength(lvs, length(lvs) + 1); signalUpdate(f); }");
    auto pushv = wc.compileFn<void(int)>("x", "pushv(f, f.vs, x)");

    // start a reader with a watch on the writer's array set to increment a couple of local values
    cc rc;
    std::pair<long, int> lensum(0, 0);
    rc.bind("lensum", &lensum);
    rc.define("f", "readFile(\"" + fname + "\") :: ((file _ {vs:[int]@?}))");
    rc.compileFn<void()>("addFileSignal(f.vs, \\_.do{lensum.0 <- length(load(f.vs));lensum.1 <- sum(load(f.vs)); return true})")();

    // now write a few changes and make sure that they're detected
    pushv(0);
    EXPECT_TRUE(stepEventLoop(30000));
    EXPECT_EQ(lensum.first,  1);
    EXPECT_EQ(lensum.second, 0);

    pushv(1);
    EXPECT_TRUE(stepEventLoop(30000));
    EXPECT_EQ(lensum.first,  2);
    EXPECT_EQ(lensum.second, 1);

    pushv(2);
    EXPECT_TRUE(stepEventLoop(30000));
    EXPECT_EQ(lensum.first,  3);
    EXPECT_EQ(lensum.second, 3);

    pushv(3);
    EXPECT_TRUE(stepEventLoop(30000));
    EXPECT_EQ(lensum.first,  4);
    EXPECT_EQ(lensum.second, 6);

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}
#endif

TEST(Storage, CppSignals) {
  std::string fname = mkFName();
  try {
    writer w(fname);
    *w.define<int>("x") = 42;
    w.signalUpdate();

    reader r(fname);
    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, Comprehensions) {
  std::string fname = mkFName();
  try {
    cc wc;
    wc.define("f", "writeFile(\"" + fname + "\") :: ((file _ {vs:(^x.(()+([int]@?*x@?)))@?}))");
    wc.compileFn<void()>("f.vs <- store(roll(|0=()|) :: (^x.(()+([int]@f*x@f))))")();

    // for now, make sure that we can just compile a comprehension over a stored rope
    EXPECT_TRUE(wc.compileFn<bool()>("let _ = [x | x <- f.vs, x > 20] in true")());

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, Lift) {
  std::string fname = mkFName();
  try {
    writer w(fname);

    // make sure that type lifting works for arrays of pairs
    auto* xs = w.define< std::pair<int, double> >("xs", 100);
    for (size_t i = 0; i < 100; ++i) {
      xs->data[i].first  = i+1;
      xs->data[i].second = static_cast<double>(i);
    }
    xs->size = 100;

    cc rc;
    rc.define("f", "inputFile :: (LoadFile \"" + fname + "\" w) => w");
    EXPECT_EQ(rc.compileFn<int()>("sum([x|(x,_)<-f.xs])")(), 5050);

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

DEFINE_ENUM(
  FRTestFood,
  (Hamburger),
  (HotDog),
  (Pickle)
);

DEFINE_VARIANT(
  MStr,
  (nothing, char),
  (just,    std::string)
);

using FRStrArr = std::array<std::string, 2>;

DEFINE_STRUCT(
  FRTest,
  (int,                      x),
  (double,                   y),
  (std::vector<std::string>, z),
  (FRTestFood,               u),
  (MStr,                     v),
  (FRStrArr,                 w)
);

TEST(Storage, FRegionCompatibility) {
  std::string fname = mkFName();
  try {
    fregion::writer f(fname);
    auto& s = f.series<FRTest>("frtest");
    for (size_t i = 0; i < 1000; ++i) {
      FRTest t;
      t.x = static_cast<int>(i);
      t.y = 3.14159*static_cast<double>(i);
      t.z.emplace_back("a");
      t.z.emplace_back("b");
      t.z.emplace_back("c");
      t.u = FRTestFood::HotDog();
      t.v = MStr::just("chicken");
      t.w[0] = "a";
      t.w[1] = "b";
      s(t);
    }

    EXPECT_TRUE(isDBFile(fname));

    cc rc;
    rc.define("f", "inputFile :: (LoadFile \"" + fname + "\" w) => w");
    EXPECT_EQ(rc.compileFn<size_t()>("size([() | x <- f.frtest, x.z == [\"a\", \"b\", \"c\"] and x.u === |HotDog| and x.v matches |just=\"chicken\"| and x.w == [\"a\", \"b\"]])")(), size_t(1000));

    fregion::reader rf(fname);
    auto rs = rf.series<FRTest>("frtest");
    FRTest t;
    size_t j = 0;
    while (rs.next(&t)) {
      EXPECT_EQ(t.x, static_cast<int>(j));
      ++j;
    }
    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, FRegion_FSeq_Write_Resume_After_Restart) {
  std::string fname = mkFName();
  try {
    // initial write transaction
    { fregion::writer w0(fname); auto& s = w0.series<int>("s"); s(1); }

    // second write transaction
    { fregion::writer w1(fname); auto& s = w1.series<int>("s"); s(2); }

    // expect to read back [1,2]
    fregion::reader r(fname);
    auto& s = r.series<int>("s");
    int x;
    EXPECT_TRUE(s.next(&x));
    EXPECT_EQ(x, 1);
    EXPECT_TRUE(s.next(&x));
    EXPECT_EQ(x, 2);

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, DArrayMemLayout) {
  EXPECT_TRUE(c().compileFn<bool()>("show([unsafeCast(\"jimmy\")::((darray char)),unsafeCast(\"chicken\")]) == \"[\\\"jimmy\\\", \\\"chicken\\\"]\"")());
}

DEFINE_VARIANT(
  MyVariant,
  (jimmy, int),
  (bob, std::string)
);

DEFINE_ENUM(
  MyColor,
  (Red),
  (Green),
  (Blue),
  (Magenta)
);

using strs = std::vector<std::string>;

DEFINE_STRUCT(
  MyStruct,
  (int,       x),
  (uint8_t,   y),
  (int128_t,  z),
  (MyColor,   c),
  (MyVariant, v),
  (strs,      f)
);

TEST(Storage, CFRegion_C2C) {
  std::string fname = mkFName();
  try {
    // write some compressed data
    {
      hobbes::fregion::cwriter w(fname);
      auto& xs = w.series<MyStruct>("xs");
      for (size_t i = 0; i < 100; ++i) {
        MyStruct s;
        s.x = i;
        s.y = i;
        s.z = i;
        s.c = static_cast<MyColor::Enum>(i%4);
        if (i%2 == 0) {
          s.v = MyVariant::jimmy(static_cast<int>(42.0*sin(i)));
        } else {
          s.v = MyVariant::bob("bob #" + str::from(i));
        }
        s.f.resize(i%10);
        for (size_t k = 0; k < s.f.size(); ++k) {
          s.f[k] = str::from(k) + " Yellowstone bears";
        }
        xs(s);
      }
    }

    // verify that it reads back in-order correctly
    {
      hobbes::fregion::creader r(fname);
      auto& xs = r.series<MyStruct>("xs");
      size_t i = 0;
      MyStruct s;
      while (xs.next(&s)) {
        EXPECT_EQ(size_t(s.x), i);
        EXPECT_EQ(s.y, uint8_t(i));
        EXPECT_EQ(s.z, int128_t(i));
        EXPECT_EQ(s.c, MyColor(static_cast<MyColor::Enum>(i%4)));
        if (i%2 == 0) {
          EXPECT_EQ(s.v, MyVariant::jimmy(static_cast<int>(42.0*sin(i))));
        } else {
          EXPECT_EQ(s.v, MyVariant::bob("bob #" + str::from(i)));
        }
        EXPECT_EQ(s.f.size(), i%10);
        for (size_t k = 0; k < s.f.size(); ++k) {
          EXPECT_EQ(s.f[k], str::from(k) + " Yellowstone bears");
        }
        ++i;
      }
    }

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, CFRegion_C2H) {
  std::string fname = mkFName();
  try {
    // write some compressed data
    {
      hobbes::fregion::cwriter w(fname);
      auto& xs = w.series<MyStruct>("xs");
      for (size_t i = 0; i < 100; ++i) {
        MyStruct s;
        s.x = i;
        s.y = i;
        s.z = i;
        s.c = static_cast<MyColor::Enum>(i%4);
        if (i%2 == 0) {
          s.v = MyVariant::jimmy(static_cast<int>(42.0*sin(i)));
        } else {
          s.v = MyVariant::bob("bob #" + str::from(i));
        }
        s.f.resize(i%10);
        for (size_t k = 0; k < s.f.size(); ++k) {
          s.f[k] = str::from(k) + " Yellowstone bears";
        }
        xs(s);
      }
    }

    // verify that it reads back correctly
    hobbes::cc c;
    c.define("cdb", "inputFile::(LoadFile \"" + fname + "\" w)=>w");
    EXPECT_TRUE((c.compileFn<bool()>("let rs = [s==\"bob #\"++show(i)|{x=i,v=|bob=s|,f=fs}<-cdb.xs, size(fs)>0] in size(rs)>0 and all(id,rs)")()));

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

double isine(int x) { return sin(x); }

TEST(Storage, CFRegion_H2C) {
  std::string fname = mkFName();
  try {
    // write some compressed data
    hobbes::cc c;
    c.bind("sine", &isine);
    hobbes::ty::bytes tdef;
    hobbes::ty::encode(hobbes::ty::elimFileRefs(hobbes::fregion::store<MyStruct>::storeType()), &tdef);
    c.defineTypeAlias("MyStruct", hobbes::str::seq(), hobbes::decode(tdef));
    c.define("cdb", "writeFile(\"" + fname + "\")::(UCModel MyStruct sm _)=>(file _ {xs:(cseq MyStruct sm 100000)})");
    c.compileFn<void()>("cdb.xs <- initCSeq(cdb)")();
    c.compileFn<void()>("cseqPut(cdb.xs, [{x=i, y=ti2b(i), z=convert(i)::int128, c=unsafeCast({t=i%4}), v=if (i%2==0) then |jimmy=tl2i(truncd(42.0*sine(i)))| else |bob=\"bob #\"++show(i)|, f=[show(k)++\" Yellowstone bears\"|k<-[0..(i%10)-1]]} |i<-[0..99]])")();

    // verify that it reads back in-order correctly
    {
      hobbes::fregion::creader r(fname);
      auto& xs = r.series<MyStruct>("xs");
      size_t i = 0;
      MyStruct s;
      while (xs.next(&s)) {
        EXPECT_EQ(size_t(s.x), i);
        EXPECT_EQ(s.y, uint8_t(i));
        EXPECT_EQ(s.z, int128_t(i));
        EXPECT_EQ(s.c, MyColor(static_cast<MyColor::Enum>(i%4)));
        if (i%2 == 0) {
          EXPECT_EQ(s.v, MyVariant::jimmy(static_cast<int>(42.0*sin(i))));
        } else {
          EXPECT_EQ(s.v, MyVariant::bob("bob #" + str::from(i)));
        }
        EXPECT_EQ(s.f.size(), i%10);
        for (size_t k = 0; k < s.f.size(); ++k) {
          EXPECT_EQ(s.f[k], str::from(k) + " Yellowstone bears");
        }
        ++i;
      }
    }

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

// disable inotify tests due to travis-ci/docker bug that breaks it
// this code does actually work on Linux outside of travis-ci
#ifndef BUILD_LINUX
TEST(Storage, FRegion_CPPAPI_BlockingRead) {
  std::string fname = mkFName();
  try {
    hobbes::fregion::writer w(fname);
    auto& wws = w.series<int>("ws");

    hobbes::fregion::reader r(fname);
    auto& rws = r.series<int>("ws");
    int x;

    // initially we don't expect to read any values
    EXPECT_EQ(rws.next(&x), false);

    // but if we spawn a thread to write a few values, then the reader should be able to block to read them
    std::thread([&]() { sleep(2); wws(42); w.signal(); }).detach();

    EXPECT_EQ(rws.next(&x, 30000), true);
    EXPECT_EQ(x, 42);

    // start a fresh reader, verify the same blocking read behavior with a new value after reading the previous one
    hobbes::fregion::reader nr(fname);
    auto& nrws = nr.series<int>("ws");

    EXPECT_EQ(nrws.next(&x), true);
    EXPECT_EQ(x, 42);

    std::thread([&]() { sleep(2); wws(8675309); w.signal(); }).detach();

    EXPECT_EQ(rws.next(&x, 30000), true);
    EXPECT_EQ(x, 8675309);

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}
#endif

DEFINE_STRUCT(
  CFTypeTest,
  (datetimeT, t)
);

TEST(Storage, CFRegion_CTypes) {
  std::string fname = mkFName();
  try {
    auto t = hobbes::now();

    // write some compressed data
    {
      hobbes::fregion::cwriter w(fname);
      auto& xs = w.series<CFTypeTest>("cts");
      for (size_t i = 0; i < 100; ++i) {
        CFTypeTest s;
        s.t = t;
        xs(s);
      }
    }

    // verify that it reads back correctly
    {
      hobbes::fregion::creader r(fname);
      auto& xs = r.series<CFTypeTest>("cts");
      CFTypeTest s;
      while (xs.next(&s)) {
        EXPECT_TRUE(s.t.value == t.value);
      }
    }

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

TEST(Storage, FRegionCArrays) {
  std::string fname = mkFName();
  try {
    hobbes::fregion::writer w(fname);

    // verify memcopyable carrays can be accessed by reference
    //   'carray (int*double) 20'
    using IandD = std::pair<int, double>;
    using XS = hobbes::carray<IandD, 20>;

    auto& xs = *w.define<XS>("xs");
    for (size_t i = 0; i < 10; ++i) {
      xs[i] = std::pair<int,double>(i,i*3.14159);
    }
    xs.size = 10;
    
    // verify that non-memcopyable can be written
    //     'carray (int * [char]@? * [int+double]@?) 20'
    using IorD = hobbes::variant<int, double>;
    using IorDs = std::vector<IorD>;
    using Y = hobbes::tuple<int, std::string, IorDs>;
    using YS = hobbes::carray<Y, 20>;

    IorDs idi; idi.push_back(IorD(100)); idi.push_back(IorD(3.14159)); idi.push_back(IorD(200));
    IorDs ddi; ddi.push_back(IorD(2.1)); ddi.push_back(IorD(4.2));     ddi.push_back(IorD(29));
    YS ys;
    ys[0] = Y(42, "cowboy", idi);
    ys[1] = Y(8675, "chicken", ddi);
    ys.size = 2;
    w.define<YS>("ys", ys);

    // expect to be able to read back the carray data from C++
    hobbes::fregion::reader r(fname);

    const auto& rxs = *r.definition<XS>("xs");
    EXPECT_EQ(rxs.size, 10UL);
    for (size_t i = 0; i < rxs.size; ++i) {
      EXPECT_EQ(rxs[i].first, static_cast<int>(i));
      EXPECT_EQ(rxs[i].second, i*3.14159);
    }

    YS rys;
    r.definition<YS>("ys", &rys);
    EXPECT_EQ(rys.size, 2UL);
    EXPECT_EQ(rys[0].at<0>(), 42);
    EXPECT_EQ(rys[0].at<1>(), "cowboy");
    EXPECT_EQ(rys[0].at<2>().size(), 3UL);
    EXPECT_TRUE(rys[0].at<2>()[0] == IorD(100));
    EXPECT_TRUE(rys[0].at<2>()[1] == IorD(3.14159));
    EXPECT_TRUE(rys[0].at<2>()[2] == IorD(200));

    EXPECT_EQ(rys[1].at<0>(), 8675);
    EXPECT_EQ(rys[1].at<1>(), "chicken");
    EXPECT_EQ(rys[1].at<2>().size(), 3UL);
    EXPECT_TRUE(rys[1].at<2>()[0] == IorD(2.1));
    EXPECT_TRUE(rys[1].at<2>()[1] == IorD(4.2));
    EXPECT_TRUE(rys[1].at<2>()[2] == IorD(29));

    // expect to be able to read back carrays from hobbes
    hobbes::cc c;
    c.define("db", "inputFile::(LoadFile \"" + fname + "\" w)=>w");

    EXPECT_TRUE(c.compileFn<bool()>("db.xs == [(i, i*3.14159) | i <- [0..9]]")());
    EXPECT_TRUE(c.compileFn<bool()>("db.ys == [(42, \"cowboy\", [|0=100|::(int+double), |1=3.14159|, |0=200|]), (8675, \"chicken\", [|1=2.1|, |1=4.2|, |0=29|])]")());

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

DEFINE_STRUCT(
  Quote,
  (double, bpx),
  (size_t, bsz),
  (double, apx),
  (size_t, asz)
);

TEST(Storage, KeySeries) {
  std::string fname = mkFName();
  try {
    writer w(fname);
    keyseries<std::array<char,10>, Quote> ticks(&c(), &w, "ticks", 10000, StoredSeries::Compressed);

    std::array<char,10> bob{'b', 'o', 'b', '\0'};
    for (size_t i = 0; i < 20000; ++i) {
      Quote q;
      q.bpx = 3.14159;
      q.bsz = 20;
      q.apx = 6.282;
      q.asz = 1;
      ticks.record(bob, q);
    }

    std::array<char,10> jim{'j', 'i', 'm', '\0'};
    for (size_t i = 0; i < 20000; ++i) {
      Quote q;
      q.bpx = 4.2;
      q.bsz = 90;
      q.apx = 8.4;
      q.asz = 1;
      ticks.record(jim, q);
    }

    hobbes::cc c;
    c.define("db", "inputFile::(LoadFile \"" + fname + "\" w)=>w");
    EXPECT_TRUE(c.compileFn<bool()>("concat([[({k=k},t)|t<-v[0:10]]|(k,v)<-db.ticks])==[({k=\"bob\"},{bpx=3.14159,bsz=20,apx=6.282,asz=1})|_<-[0..9]]++[({k=\"jim\"},{bpx=4.2,bsz=90,apx=8.4,asz=1})|_<-[0..9]]")());

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

// this test was added after some analysis found a case where "torn reads" could happen
// (where a partial map is considered total for some value that we want to read, though it
// actually is not and so an attempt to read the whole value causes a crash)
//
// this test was verified to reproduce the bug
DEFINE_VARIANT(
  Tick,
  (bob,   int),
  (frank, int),
  (steve, std::string)
);
DEFINE_STRUCT(
  TTick,
  (Tick, t0),
  (Tick, t1),
  (Tick, t2)
);
TEST(Storage, AvoidTornRead) {
  std::string fname = mkFName();
  try {
    hobbes::fregion::writer f(hobbes::fregion::openFile(fname, false, HFREGION_CURRENT_FILE_FORMAT_VERSION, HFREGION_CURRENT_FILE_FORMAT_VERSION, 1));
    size_t batchsize = 1234;
    auto& s = f.series<TTick>("tticks", batchsize);
    f.recordOrdering("log", s);

    // read now
    std::ostringstream ss;

    hobbes::fregion::reader rf(hobbes::fregion::openFile(fname, true, HFREGION_CURRENT_FILE_FORMAT_VERSION, HFREGION_CURRENT_FILE_FORMAT_VERSION, 1));
    auto rlog = rf.ordering("log");
    rlog.match<TTick>("tticks", [&](const TTick& tt) {
      ss << tt << "\n";
    });

    // then dump two batches
    for (size_t i = 0; i < 2; ++i) {
      for (size_t i = 0; i < batchsize; ++i) {
        TTick tt;
        tt.t0 = Tick::bob(i);
        tt.t1 = Tick::frank(i);
        tt.t2 = Tick::bob(i);
        s(tt);
      }
    }

    // now try to read, with the torn read bug it will crash
    while (rlog.next());

    // if we get here, we are good
    EXPECT_TRUE(!ss.str().empty());

    unlink(fname.c_str());
  } catch (...) {
    unlink(fname.c_str());
    throw;
  }
}

