/*
 * test : a simple system for introducing tests
 */

#ifndef HOBBES_TEST_SYSTEM_HPP_INCLUDED
#define HOBBES_TEST_SYSTEM_HPP_INCLUDED

#include <map>
#include <vector>
#include <string>
#include <stdexcept>
#include <sstream>
#include <hobbes/util/str.H>

class TestCoord {
public:
  typedef void (*PTEST)();
  static TestCoord& instance();
  bool installTest(const std::string& group, const std::string& test, PTEST pf);
  int runAllTests();
private:
  typedef std::vector<std::pair<std::string, PTEST>> Tests;
  typedef std::map<std::string, Tests> GroupedTests;
  GroupedTests tests;
};

#define TEST(G,N) \
  void test_##G##_##N(); \
  bool install_##G##_##N = TestCoord::instance().installTest(#G, #N, &test_##G##_##N); \
  void test_##G##_##N()

#define EXPECT_TRUE(p) \
  if (!(p)) { \
    throw std::runtime_error("Expression false, expected true: " #p); \
  }
#define EXPECT_FALSE(p) \
  if ((p)) { \
    throw std::runtime_error("Expression true, expected false: " #p); \
  }
#define EXPECT_EQ(p,x) \
  { \
    auto v = (p); \
    auto z = (x); \
    if (!(v == z)) { \
      std::ostringstream __errmsg; \
      __errmsg << "Expression '" #p "' == " << v << ", but expected " << z; \
      throw std::runtime_error(__errmsg.str()); \
    } \
  }

#endif

