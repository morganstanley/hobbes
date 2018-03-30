
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

std::set<std::string> TestCoord::testGroupNames() const {
  std::set<std::string> r;
  for (const auto& g : this->tests) {
    r.insert(g.first);
  }
  return r;
}

int TestCoord::runTestGroups(const std::set<std::string>& gs) {
  std::vector<std::string> failures;

  std::cout << "Running " << gs.size() << " group" << (gs.size() == 1 ? "" : "s") << " of tests" << std::endl
            << "---------------------------------------------------------------------" << std::endl
            << std::endl;

  long tt0 = hobbes::tick();
  for (const auto& gn : gs) {
    auto gi = this->tests.find(gn);
    if (gi == this->tests.end()) {
      std::cout << "ERROR: no test group named '" << gn << "' exists" << std::endl;
      continue;
    }
    const auto& g = gi->second;

    std::cout << "  " << gn << " (" << g.size() << " test" << (g.size() == 1 ? "" : "s") << ")" << std::endl
              << "  ---------------------------------------------------------" << std::endl;

    long gt0 = hobbes::tick();
    for (const auto& t : g) {
      std::cout << "    " << t.first << std::flush;
      long t0 = hobbes::tick();
      try {
        t.second();
        std::cout << " SUCCESS ";
      } catch (std::exception& ex) {
        std::cout << " FAIL ";
        failures.push_back("[" + gn + "/" + t.first + "]: " + ex.what());
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

int main(int argc, char** argv) {
  if (argc <= 1) {
    return TestCoord::instance().runTestGroups(TestCoord::instance().testGroupNames());
  } else {
    std::set<std::string> gs;
    for (size_t i = 1; i < argc; ++i) {
      gs.insert(argv[i]);
    }
    return TestCoord::instance().runTestGroups(gs);
  }
}

