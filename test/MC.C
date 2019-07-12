
#include <hobbes/hobbes.H>
#include <hobbes/mc/encode.H>
#include "test.H"

using namespace hobbes;

template <typename T>
T assemble(const mc::MInsts& insts) {
  auto* b = new mc::buffer(); // leak just for convenience in tests
  mc::encode(b, insts);
  return reinterpret_cast<T>(b->finalize());
}

TEST(MC, BasicIntFn) {
  auto f = assemble<int(*)(int)>({
    mc::MInst::make("mov", "eax", "edi"),
    mc::MInst::make("add", "eax", "eax"),
    mc::MInst::make("ret")
  });
  EXPECT_EQ(f(21), 42);
}

TEST(MC, BasicFloatFn) {
  auto f = assemble<float(*)(float)>({
    mc::MInst::make("addss", "xmm0f", "xmm0f"),
    mc::MInst::make("ret")
  });
  EXPECT_ALMOST_EQ(f(3.14159f), 6.28318f, 0.0001f);
}

TEST(MC, BasicDoubleFn) {
  auto f = assemble<double(*)(double)>({
    mc::MInst::make("addsd", "xmm0", "xmm0"),
    mc::MInst::make("ret")
  });
  EXPECT_ALMOST_EQ(f(3.14159), 6.28318, 0.0001);
}

