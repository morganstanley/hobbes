
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

int twentyOne() { return 21; }
int twiceInt(int x) { return x+x; }

TEST(MC, BasicCalls) {
  auto f = assemble<int(*)(int(*)())>({
    mc::MInst::make("sub", "rsp", mc::ui32(8)),
    mc::MInst::make("call", "rdi"),              // on Windows, std arg0 is in rcx/ecx instead of rdi/edi
    mc::MInst::make("mov", "edi", "eax"),
    mc::MInst::make("call", mc::ui64(reinterpret_cast<int64_t>(&twiceInt))),
    mc::MInst::make("add", "rsp", mc::ui32(8)),
    mc::MInst::make("ret")
  });
  EXPECT_EQ(f(&twentyOne), 42);
}

TEST(MC, CmpSetcc) {
    auto f = assemble<bool(*)(int)>({
      mc::MInst::make("sub", "rsp", mc::ui32(8)),
      mc::MInst::make("cmp", "edi", mc::ui32(40)),
      mc::MInst::make("setl", "al"),
      mc::MInst::make("add", "rsp", mc::ui32(8)),
      mc::MInst::make("ret")
    });
    EXPECT_EQ(f(21), true);
    EXPECT_EQ(f(42), false);
}

TEST(MC, CmpJcc) {
    auto f = assemble<bool(*)(int)>({
      mc::MInst::make("sub", "rsp", mc::ui32(8)),
      mc::MInst::make("cmp", "edi", mc::ui32(42)),
      mc::MInst::make("je", mc::MArg::labelRef("is_42")),
      mc::MInst::make("mov", "al", mc::ui8(0)),
      mc::MInst::make("add", "rsp", mc::ui32(8)),
      mc::MInst::make("ret"),
      mc::MInst::defineLabel("is_42"),
      mc::MInst::make("mov", "al", mc::ui8(1)),
      mc::MInst::make("add", "rsp", mc::ui32(8)),
      mc::MInst::make("ret"),
    });
    EXPECT_EQ(f(21), false);
    EXPECT_EQ(f(42), true);
}

