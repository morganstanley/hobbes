
#include <hobbes/hobbes.H>
#include "test.H"

namespace hobbes { region& threadRegion(); }

using namespace hobbes;
static cc& c() { static cc x; return x; }

TEST(Arrays, Strings) {
  EXPECT_TRUE ((c().compileFn<bool()>("\"foo\" <= \"foo\"")()));
  EXPECT_TRUE ((c().compileFn<bool()>("\"foo\" < \"fooo\"")()));
  EXPECT_FALSE((c().compileFn<bool()>("\"fooo\" <  \"foo\"")()));
  EXPECT_FALSE((c().compileFn<bool()>("\"fooo\" <= \"foo\"")()));
  EXPECT_TRUE ((c().compileFn<bool()>("\"fob\" > \"foa\"")()));

  std::string x = "frank";
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x <= x")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x <= \"frank\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "\"fran\" < x")(&x)));
}

TEST(Arrays, Slices) {
  std::string x = "abcdefghijklmnopqrstuvwxyz";

  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[0:2] == \"ab\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[2:0] == \"ba\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[-5:-1] == \"vwxy\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[-5:] == \"vwxyz\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[:-10] == \"zyxwvutsrq\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[0:0] == \"\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "length(x[:-10]) == 10L")(&x)));

  // slice through lexed time (resolve lexical ambiguity in a way that makes the most sense)
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[10:20] == \"klmnopqrst\"")(&x)));
}

TEST(Arrays, Comprehensions) {
  EXPECT_TRUE((c().compileFn<bool()>("sum([x | x <- [1..1000], x <= 100]) == 5050")()));
  EXPECT_TRUE((c().compileFn<bool()>("countBy(.0,[(x,y)|x<-[1..5]|y<-[1..x]]) == [(i,i)|i<-[1..5]]")()));
}

TEST(Arrays, Streams) {
  EXPECT_TRUE((c().compileFn<bool()>(
    "takeS(10, [n+1 | n <- [100..], n%2 == 0]) == [101, 103, 105, 107, 109, 111, 113, 115, 117, 119]")()
  ));
  EXPECT_TRUE((c().compileFn<bool()>(
    "takeS(10, [s | |even='12(?<s>[4-9]+)'| <- [if (x%2 == 0) then |even=show(x)| else |odd=x|::|even:[char],odd:int| | x <- [0..], x > 20]]) == [\"4\", \"6\", \"8\", \"44\", \"46\", \"48\", \"54\", \"56\", \"58\", \"64\"]"
  )()));
}

TEST(Arrays, WithStdVector) {
  std::vector<int> xs = {1, 2, 3, 4, 5};
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*(std::vector<int>&)>("xs", "show(xs)")(xs)), "[1, 2, 3, 4, 5]");
  EXPECT_EQ(((c().compileFn<int(std::vector<int>&)>("xs", "sum(xs[0:])")(xs))), 15);
}

TEST(Arrays, CppRAIIConsistency) {
  array<std::string>* xs = makeArray<std::string>(300);
  for (size_t i = 0; i < xs->size; ++i) {
    EXPECT_EQ(xs->data[i], "");
  }
}

TEST(Arrays, Alignment) {
  auto*        s = reinterpret_cast<short*>(threadRegion().malloc(sizeof(short), alignof(short)));
  int*          i = reinterpret_cast<int*>(threadRegion().malloc(sizeof(int)), alignof(int));
  auto*       d = reinterpret_cast<double*>(threadRegion().malloc(sizeof(double)), alignof(double));
  array<int>*   a = makeArray<int>(100);

  EXPECT_EQ(reinterpret_cast<size_t>(s)%sizeof(short),  size_t(0));
  EXPECT_EQ(reinterpret_cast<size_t>(i)%sizeof(int),    size_t(0));
  EXPECT_EQ(reinterpret_cast<size_t>(d)%sizeof(double), size_t(0));
  EXPECT_EQ(reinterpret_cast<size_t>(a)%sizeof(size_t), size_t(0));
}

TEST(Arrays, Elements) {
  std::string x = "ab";

  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "elements(x, 0L, 2L) == \"ab\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "elements(x, -1L, 2L) == \"\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "elements(x, 0L, 3L) == \"\"")(&x))); 
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "elements(x, 2L, 1L) == \"\"")(&x)));
}

