#pragma once

#include <hobbes/db/file.H>
#include <hobbes/db/series.H>
#include <hobbes/hobbes.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/storage.H>

const int option = 100;

struct TestEnum {
  enum Enum : uint32_t { Invalid = 0, Option = option };
  Enum _value;
  using is_enum = void;
};

namespace hobbes {
struct liftTestEnum {
  static MonoTypePtr type(typedb &) {
    Variant::Members vms;
    auto i = 0;
    vms.push_back(Variant::Member("Invalid", primty("unit"), 0));
    vms.push_back(Variant::Member("Option", primty("unit"), option));
    return MonoTypePtr(Variant::make(vms));
  }
};
template <class T>
struct lift<T *, false, typename T::is_enum> : public liftTestEnum {};
template <class T>
struct lift<T, true, typename T::is_enum> : public liftTestEnum {};
} // namespace hobbes

DEFINE_STRUCT(FixarrayS, (TestEnum, value),
              (const hobbes::array<char> *, body));

void run(const std::string &fname, bool compressed) {
  hobbes::cc c;
  FixarrayS v1;
  v1.body = hobbes::makeString("Hello world!");
  v1.value._value = TestEnum::Option;

  hobbes::writer writer{fname};
  hobbes::series<FixarrayS> ss1(&c, &writer, "udata", 10000,
                                compressed ? hobbes::StoredSeries::Compressed
                                           : hobbes::StoredSeries::Raw);
  ss1(v1);
  std::cout << "------------testing -----------------" << std::endl;
  // c.define("f", "inputFile :: (LoadFile "
  //               " + fname + "
  //               " w) => w");
  // c.compileFn<void()>(
  //     "print([(x.value, (unsafeCast(x.value)::{t:int}).t) | x<-f.udata])")();
}
