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
#include <hobbes/util/stream.H>

#include <unistd.h>
#include <libgen.h>

#if defined(__APPLE__) && defined(__MACH__)
#  include <mach-o/dyld.h>
#elif __linux__
#  include <linux/limits.h>
#else
#  error "platform is not supported"
#endif

template<class Fn>
auto execPath(Fn && fn) -> void {

#if defined(__APPLE__) && defined(__MACH__)
#  if !defined(PROC_PIDPATHINFO_MAXSIZE) && !defined(PROC_PIDPATHINFO_MAXSIZE_HOBBES_FEATURED)
#    define PROC_PIDPATHINFO_MAXSIZE_HOBBES_FEATURED 2048
#  elif defined(PROC_PIDPATHINFO_MAXSIZE)
#    define PROC_PIDPATHINFO_MAXSIZE_HOBBES_FEATURED = PROC_PIDPATHINFO_MAXSIZE
#  endif
  std::string r(PROC_PIDPATHINFO_MAXSIZE_HOBBES_FEATURED, 0);
  uint32_t count = PROC_PIDPATHINFO_MAXSIZE_HOBBES_FEATURED;
  if (-1 != _NSGetExecutablePath(const_cast<char*>(r.data()), &count)) {
#elif __linux__  
  std::string r(PATH_MAX, 0);
  if (-1 != ::readlink("/proc/self/exe", const_cast<char*>(r.data()), r.size())) {
#endif
    fn(std::string(dirname(const_cast<char*>(r.data()))));
  } else {
    throw std::runtime_error("failed to readlink(\"/proc/self/exe\") ...");
  }
}

struct Args final {
  Args() : report("test_report.json") {}

  std::set<std::string> tests;
  const char* report;
};

struct Result final {
  enum class Status {
    Pass,
    Fail,
    Skipped,
  };

  Result(const std::string& name) : testcase(name) {}

  void record(Status s, long d, const std::string& e = "") {
    status = s;
    duration = d;
    error = e;
  }

  std::string testcase;
  Status status {Status::Skipped};
  long duration {0};
  std::string error;
};

class TestCoord {
public:
  using PTEST = void (*)();
  static TestCoord& instance();
  bool installTest(const std::string& group, const std::string& test, PTEST pf);
  std::set<std::string> testGroupNames() const;
  int runTestGroups(const Args&);

private:
  std::string toJSON();
  using Tests = std::vector<std::pair<std::string, PTEST>>;
  using GroupedTests = std::map<std::string, Tests>;
  using Results = std::vector<Result>;
  using GroupedResults = std::map<std::string, Results>;
  GroupedTests tests;
  GroupedResults results;
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
#define EXPECT_ALMOST_EQ(p,x,eps) \
  { \
    auto v    = (p); \
    auto z    = (x); \
    auto d    = v - z; \
    auto absd = (d < 0.0 ? -1.0 : 1.0) * d; \
    if (absd > eps) { \
      std::ostringstream __errmsg; \
      __errmsg << "Expression '" #p "' == " << v << ", but expected " << z << " " << FILEINFO(__FILE__, __LINE__); \
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

#define EXPECT_EXCEPTION_MSG(p, E, S) \
  { \
    bool exp = false; \
    const std::string s = S; \
    std::string es; \
    try { \
      (p); \
    } catch (const E& e) { \
      es = e.what(); \
      if (es.find(s) != std::string::npos) { \
        exp = true; \
      } \
    } \
    if (!exp) { \
      std::ostringstream errmsg; \
      errmsg << "Expression '" #p "' expects a '" #E; \
      if (!s.empty()) { \
        errmsg << "' which contains string '" #S; \
      } \
      errmsg << "' at " << " " << FILEINFO(__FILE__, __LINE__); \
      if (!es.empty()) { \
        errmsg << ", got '" << es << "' instead"; \
      } \
      throw std::runtime_error(errmsg.str()); \
    } \
  }

#endif

