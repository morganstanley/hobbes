/*
 * test : a simple system for introducing tests
 */

#ifndef HOBBES_TEST_SYSTEM_HPP_INCLUDED
#define HOBBES_TEST_SYSTEM_HPP_INCLUDED

#include <map>
#include <vector>
#include <string>
#include <set>
#include <stdexcept>
#include <sstream>
#include <hobbes/util/str.H>

class TestCoord {
public:
  typedef void (*PTEST)();
  static TestCoord& instance();
  bool installTest(const std::string& group, const std::string& test, PTEST pf);
  std::set<std::string> testGroupNames() const;
  int runTestGroups(const std::set<std::string>&);
private:
  typedef std::vector<std::pair<std::string, PTEST>> Tests;
  typedef std::map<std::string, Tests> GroupedTests;
  GroupedTests tests;
};

#define TEST(G,N) \
  void test_##G##_##N(); \
  bool install_##G##_##N = TestCoord::instance().installTest(#G, #N, &test_##G##_##N); \
  void test_##G##_##N()

#define FILEINFO(file, line) (std::string("(") + hobbes::str::rsplit(file, "/").second + ":" + std::to_string(line) + ")")

#define EXPECT_TRUE(p) \
  if (!(p)) { \
    std::ostringstream __errmsg; \
    __errmsg << "Expression false, expected true: " #p << " " << FILEINFO(__FILE__, __LINE__); \
    throw std::runtime_error(__errmsg.str()); \
  }
#define EXPECT_FALSE(p) \
  if ((p)) { \
    std::ostringstream __errmsg; \
    __errmsg << "Expression true, expected false: " #p << " " << FILEINFO(__FILE__, __LINE__); \
    throw std::runtime_error("Expression true, expected false: " #p); \
  }
#define EXPECT_EQ(p,x) \
  { \
    auto v = (p); \
    auto z = (x); \
    if (!(v == z)) { \
      std::ostringstream __errmsg; \
      __errmsg << "Expression '" #p "' == " << v << ", but expected " << z << " " << FILEINFO(__FILE__, __LINE__); \
      throw std::runtime_error(__errmsg.str()); \
    } \
  }
#define EXPECT_NEQ(p,x) \
  { \
    auto v = (p); \
    auto z = (x); \
    if (v == z) { \
      std::ostringstream __errmsg; \
      __errmsg << "Expression '" #p "' == " << v << ", but expected anything else" << " " << FILEINFO(__FILE__, __LINE__); \
      throw std::runtime_error(__errmsg.str()); \
    } \
  }

#define EXPECT_EXCEPTION(p) \
  { \
    bool __pass = true; \
    try { \
      (p); \
      __pass = false; \
    } catch (...) { \
    } \
    if (!__pass) { \
      std::ostringstream __errmsg; \
      __errmsg << "Expression '" #p "' ran successfully but an exception was expected" << " " << FILEINFO(__FILE__, __LINE__); \
      throw std::runtime_error(__errmsg.str()); \
    } \
  }

#endif

