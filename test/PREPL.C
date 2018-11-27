
#include <hobbes/hobbes.H>
#include <hobbes/ipc/prepl.H>
#include "test.H"

using namespace hobbes;

static proc* hiSession() {
  static proc p;
  static bool init = false;
  if (!init) {
    spawn("./hi -z", &p);
  }
  return &p;
}

static std::string exprEval(const std::string& x) {
  proc* p = hiSession();
  procEval(p, x);
  std::ostringstream ss;
  procRead(p, &ss);
  return ss.str();
}

std::string exprTypeof(const std::string& x) {
  proc* p = hiSession();
  procTypeof(p, x);
  std::ostringstream ss;
  procRead(p, &ss);
  return ss.str();
}

std::string exprTEnv() {
  proc* p = hiSession();
  procTypeEnv(p);
  std::ostringstream ss;
  procRead(p, &ss);
  return ss.str();
}

TEST(PREPL, EvalBasicExprs) {
  EXPECT_EQ(exprEval("1+1"), "2");
  EXPECT_TRUE(exprEval("undefinedVariableNameForTest").find("undefinedVariableNameForTest") != std::string::npos);
}

TEST(PREPL, Typeof) {
  EXPECT_EQ(exprTypeof("1+1"), "int");
  EXPECT_EQ(exprTypeof("[x|x<-[0..10],x%2==0]"), "[int]");
}

TEST(PREPL, TEnvWithoutHiddenTCs) {
  EXPECT_TRUE(exprTEnv().find(".genc.") == std::string::npos);
}

