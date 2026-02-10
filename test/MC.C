
#include <hobbes/hobbes.H>
#include <hobbes/mc/encode.H>
#include <hobbes/mc/liveness.H>
#include <hobbes/mc/regalloc.H>
#include "test.H"

using namespace hobbes;

// generate some random programs up to a reasonable size
// verify that the basic liveness view (simple, expensive) is the same as the block liveness view (complex, cheap)
mc::RInsts genLivenessProgram(size_t n) {
  using namespace hobbes::mc;

  std::string genLabel;

  RInsts r;
  r.push_back(RInst::make("mov", RArg::ireg("save_bp",8), RArg::ireg("rbp", 8)));

  for (size_t i = 0; i < n; ++i) {
    std::ostringstream ss;
    ss << "v" << i;
    auto vn = ss.str();

    r.push_back(RInst::make("mov", RArg::ireg(vn, 4), int32_t(8675309)));

    if (i > 0) {
      std::ostringstream pss;
      pss << "v" << i-1;

      r.push_back(RInst::make("add", RArg::ireg(vn, 4), RArg::ireg(pss.str(), 4)));
    }

    if (rand() % 7 == 0) {
      if (genLabel.empty()) {
        std::ostringstream lss;
        lss << "label_at_" << i;
        genLabel = lss.str();

        r.push_back(RInst::make("cmp", RArg::ireg(vn, 4), int32_t(0)));
        r.push_back(RInst::make("jne", RArg::labelRef(genLabel)));
      } else {
        r.push_back(RInst::defineLabel(genLabel));
        genLabel = "";
      }
    }
  }
  if (!genLabel.empty()) {
    r.push_back(RInst::defineLabel(genLabel));
  }

  r.push_back(RInst::make("mov", RArg::ireg("rbp", 8), RArg::ireg("save_bp",8)));
  r.push_back(RInst::make("ret"));
  return r;
}

std::set<std::string> varNames(const mc::VarUniverse<mc::RInstM>& vu, const std::set<mc::VarID>& vs) {
  std::set<std::string> r;
  for (auto v : vs) {
    r.insert(vu.var(v));
  }
  return r;
}

std::string show(const std::set<std::string>& xs) {
  std::ostringstream ss;
  ss << "{";
  if (!xs.empty()) {
    auto x = xs.begin();
    ss << "\"" << *x << "\"";
    ++x;
    for (; x != xs.end(); ++x) {
      ss << ", \"" << *x << "\"";
    }
  }
  ss << "}";
  return ss.str();
}

TEST(MC, BlockLivenessEqualsBasicLiveness) {
  for (size_t n = 0; n < 100; ++n) {
    auto insts = genLivenessProgram(n);
    mc::RInstM m(insts);
    mc::BasicLiveness<mc::RInstM> basic(insts, m);
    mc::BlockLiveness<mc::RInstM> block(insts, m);

    EXPECT_EQ(basic.variables().size(), block.variables().size());

    auto basic_i = basic.iterate();
    auto block_i = block.iterate();

    while (true) {
      EXPECT_EQ(basic_i.end(), block_i.end());
      if (basic_i.end()) break;
      EXPECT_EQ(basic_i.pc(), block_i.pc());

      auto basic_in  = varNames(basic.variables(), basic_i.liveIn());
      auto basic_out = varNames(basic.variables(), basic_i.liveOut());
      auto block_in  = varNames(block.variables(), block_i.liveIn());
      auto block_out = varNames(block.variables(), block_i.liveOut());

      if (basic_in != block_in || basic_out != block_out) {
        std::ostringstream ss;
        ss << "block/basic liveness disagreement in program at pc=" << basic_i.pc() << "\n"
           << "  basic_in  = " << show(basic_in)  << "\n"
           << "  basic_out = " << show(basic_out) << "\n"
           << "  block_in  = " << show(block_in)  << "\n"
           << "  block_out = " << show(block_out) << "\n";
        throw std::runtime_error(ss.str());
      }

      basic_i.step();
      block_i.step();
    }
  }
}

// test some encoded assembly programs (x86-64 only)
#if defined(__x86_64__)
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

hobbes::mc::RArg ir(const std::string& n, uint8_t sz) { return hobbes::mc::RArg::reg(n, sz, hobbes::mc::RegClass::Int); }
hobbes::mc::RArg fr(const std::string& n, uint8_t sz) { return hobbes::mc::RArg::reg(n, sz, hobbes::mc::RegClass::Float); }
hobbes::mc::RArg ir32(const std::string& n) { return ir(n, 4); }
hobbes::mc::RArg fr32(const std::string& n) { return fr(n, 4); }
hobbes::mc::RArg ir64(const std::string& n) { return ir(n, 8); }
hobbes::mc::RArg fr64(const std::string& n) { return fr(n, 8); }

TEST(MC, EasyRegCoalesce) {
  using namespace hobbes::mc;

  // make sure that easy coalesce decisions are always made in register allocation
  // this verifies that large sets of copied variables merge down to just the minimal set
  RInsts p = {
    RInst::make("mov", ir64("a0"), ir64("rdi")),
  };
  for (size_t i = 1; i < 100; ++i) {
    p.push_back(RInst::make("mov", ir64("a"+mc::str(i)), ir64("a"+mc::str(i-1))));
  }
  p.push_back(RInst::make("mov", ir64("rax"), ir64("a99")));
  p.push_back(RInst::make("add", ir64("rax"), ir64("rax")));
  p.push_back(RInst::make("ret"));

  MInsts expect = {
    MInst::make("mov", "rax", "rdi"),
    MInst::make("add", "rax", "rax"),
    MInst::make("ret")
  };
  EXPECT_EQ(assignRegisters(p) == expect, true);
}

float FloatSaveAroundCallFn(float x) {
  return x*x+x;
}
TEST(MC, FloatSaveAroundCall) {
  using namespace hobbes::mc;

  // make sure that float registers are saved around calls
  auto f = assemble<float(*)(float)>(assignRegisters({
    RInst::make("mov", fr32("r"), fr32("xmm0")),
    RInst::make("call", RArg::i64(reinterpret_cast<int64_t>(&FloatSaveAroundCallFn))),
    RInst::make("mov", fr32("xmm0"), fr32("r")),
    RInst::make("ret")
  }));

  EXPECT_ALMOST_EQ(f(3.14159), 3.14159, 0.0001);
}

DEFINE_STRUCT(
  SUTest,
  (int,    x),
  (char,   c),
  (double, y),
  (int,    z)
);
TEST(MC, StructUsage) {
  using namespace hobbes::mc;

  // f s = s.x + s.z
  auto f = assemble<int(*)(const SUTest&)>(assignRegisters({
    RInst::make("mov", ir32("s_x"), RArg::regDeref("rdi", offsetof(SUTest, x), sizeof(int), RegClass::Int)), // s_x = arg.x
    RInst::make("mov", ir32("s_z"), RArg::regDeref("rdi", offsetof(SUTest, z), sizeof(int), RegClass::Int)), // s_z = arg.z

    RInst::make("mov", ir32("result"), ir32("s_x")), // result = s_x+s_z
    RInst::make("add", ir32("result"), ir32("s_z")),

    RInst::make("mov", ir32("rax"), ir32("result")),

    RInst::make("ret")
  }));

  SUTest s;
  s.x = 7;
  s.c = 'a';
  s.y = 31337;
  s.z = 35;

  EXPECT_EQ(f(s), 42);
}
#endif // __x86_64__

