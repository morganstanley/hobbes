
#include <iostream>
#include <array>

namespace hobbes {
template <typename T, size_t N>
  std::ostream& operator<<(std::ostream& o, const std::array<T,N>& x) {
    o << "[";
    if (x.size() > 0) {
      o << x[0];
      for (size_t i = 1; i < x.size(); ++i) {
        o << ", " << x[i];
      }
    }
    o << "]";
    return o;
  }
}

#include <hobbes/fregion.H>
#include <hobbes/convert.H>

using namespace hobbes;

// the type we want to convert FROM (the type we receive)
typedef std::array<int, 5> IArr;

DEFINE_VARIANT(
  Friend,
  (Bob,   int),
  (Frank, short)
);

DEFINE_STRUCT(
  From,
  (int,    x),
  (Friend, y),
  (int,    z),
  (IArr,   w)
);

// the type we want to convert TO
typedef std::array<size_t,5> LArr;

DEFINE_VARIANT(
  NewFriend,
  (Steve,   char),
  (George,  double),
  (Bob,     long),
  (Stewart, int),
  (Horace,  IArr),
  (Frank,   int)
);

DEFINE_STRUCT(
  To,
  (LArr,      w),
  (long,      z),
  (NewFriend, y),
  (int,       x)
);

#include "test.H"

TEST(Convert, convertibleTypes) {
  From from;
  from.x = 1;
  from.y = Friend::Frank(42);
  from.z = 3;
  for (size_t i = 0; i < 5; ++i) {
    from.w[i] = i;
  }
  hobbes::ty::desc dynamicFromType = hobbes::fregion::store<From>::storeType(); // could come from anywhere

  // make sure that this described 'from' value can be converted to type To
  auto cvt = hobbes::convert::into<To>::from(dynamicFromType);

  To to;
  cvt(&from, &to);

  // verify conversion worked as expected
  std::ostringstream fromS;
  fromS << from;
  EXPECT_EQ(fromS.str(), "{ x=1, y=|Frank=42|, z=3, w=[0, 1, 2, 3, 4] }");

  std::ostringstream toS;
  toS << to;
  EXPECT_EQ(toS.str(), "{ w=[0, 1, 2, 3, 4], z=3, y=|Frank=42|, x=1 }");
}

