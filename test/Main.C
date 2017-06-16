
#include "test.H"
#include <hobbes/util/perf.H>

TestCoord& TestCoord::instance() {
  static TestCoord tc;
  return tc;
}

bool TestCoord::installTest(const std::string& group, const std::string& test, PTEST pf) {
  this->tests[group].push_back(std::make_pair(test, pf));
  return true;
}

int TestCoord::runAllTests() {
  std::vector<std::string> failures;

  std::cout << "Running " << this->tests.size() << " group" << (this->tests.size() == 1 ? "" : "s") << " of tests" << std::endl
            << "---------------------------------------------------------------------" << std::endl
            << std::endl;

  long tt0 = hobbes::tick();
  for (const auto& g : this->tests) {
    std::cout << "  " << g.first << " (" << g.second.size() << " test" << (g.second.size() == 1 ? "" : "s") << ")" << std::endl
              << "  ---------------------------------------------------------" << std::endl;

    long gt0 = hobbes::tick();
    for (const auto& t : g.second) {
      std::cout << "    " << t.first << std::flush;
      long t0 = hobbes::tick();
      try {
        t.second();
        std::cout << " SUCCESS ";
      } catch (std::exception& ex) {
        std::cout << " FAIL ";
        failures.push_back("[" + g.first + "/" + t.first + "]: " + ex.what());
      }
      std::cout << "(" << hobbes::describeNanoTime(hobbes::tick()-t0) << ")" << std::endl;
    }
    std::cout << "  ---------------------------------------------------------" << std::endl
              << "  " << hobbes::describeNanoTime(hobbes::tick()-gt0) << std::endl
              << std::endl;
  }
  std::cout << "---------------------------------------------------------------------" << std::endl
            << hobbes::describeNanoTime(hobbes::tick()-tt0) << std::endl;

  if (failures.size() > 0) {
    std::cout << "\n\nFAILURE" << (failures.size() == 1 ? "" : "S") << ":" << std::endl
              << "---------------------------------------------------------------------" << std::endl;
    for (const auto& failure : failures) {
      std::cout << failure << std::endl;
    }
  }

  return (int)failures.size();
}

int main(int argc_, char** argv_) {
  return TestCoord::instance().runAllTests();
}

