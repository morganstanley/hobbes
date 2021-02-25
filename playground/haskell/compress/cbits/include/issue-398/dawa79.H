#pragma once

#include <hobbes/db/file.H>
#include <hobbes/db/series.H>
#include <hobbes/hobbes.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/storage.H>

const int option = 100;

struct Test {
  enum Enum : uint32_t { Invalid = 0, Option = option };
  Enum _enum;
};

namespace hobbes {
namespace fregion {
template <> struct compress<Test> : public compress<uint32_t> {};
} // namespace fregion
template <> struct lift<Test, true> {
  static MonoTypePtr type(typedb &) {
    auto variantTy = [&]() -> MonoTypePtr {
      Variant::Members vms;
      auto testTy = primty("Test", tabs(str::strings("x"), primty("unit")));
      vms.push_back(
          Variant::Member("invalid", tapp(testTy, list(tlong(0))), 0));
      vms.push_back(
          Variant::Member("option", tapp(testTy, list(tlong(option))), 1));
      return MonoTypePtr(Variant::make(vms));
    };
    return tapp(primty("test", tabs(str::strings("x"), primty("int"))),
                list(variantTy()));
  }
};
template <> struct lift<Test *, false> : public lift<Test, true> {};
} // namespace hobbes

DEFINE_STRUCT(Struct, (Test, test), (const hobbes::array<char> *, string));

void run(const std::string &fname, bool compressed) {
  hobbes::cc c;
  Struct v1;
  v1.string = hobbes::makeString("Hello world!");
  v1.test._enum = Test::Option;

  hobbes::writer writer{fname};
  hobbes::series<Struct> ss1(&c, &writer, "udata", 10000,
                             compressed ? hobbes::StoredSeries::Compressed
                                        : hobbes::StoredSeries::Raw);
  ss1(v1);
  std::cout << "------------start testing -----------------" << std::endl;
  // c.define("f", "inputFile :: (LoadFile \"" + fname + "\" w) => w");
  // c.compileFn<void()>(
  //     "print([(x.test, (unsafeCast(x.test)::{t:int}).t) | x<-f.udata])")();
  std::cout << "------------end testing -----------------" << std::endl;
}
