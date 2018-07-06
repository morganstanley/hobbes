#include "test.H"
#include "format.H"
#include <getopt.h>
#include <hobbes/util/perf.H>
#include <fstream>

TestCoord& TestCoord::instance() {
  static TestCoord tc;
  return tc;
}

bool TestCoord::installTest(const std::string& group, const std::string& test, PTEST pf) {
  this->tests[group].push_back(std::make_pair(test, pf));
  this->results[group].push_back(Result(test));
  return true;
}

std::set<std::string> TestCoord::testGroupNames() const {
  std::set<std::string> r;
  for (const auto& g : this->tests) {
    r.insert(g.first);
  }
  return r;
}

int TestCoord::runTestGroups(const Args& args) {
  std::vector<std::string> failures;
  const auto & groups = args.tests;

  auto updateTerminal = [&failures](const std::string& name, const Result& result) {
    std::cout << "    " << name;
    if (result.status == Result::Status::Pass) {
      std::cout << " SUCCESS ";
    } else if (result.status == Result::Status::Fail) {
      std::cout << " FAIL ";
      failures.push_back(result.error);
    }
    std::cout << "(" << hobbes::describeNanoTime(result.duration) << ")" << std::endl;
  };

  std::cout << "Running " << groups.size() << " group" << (groups.size() == 1 ? "" : "s") << " of tests" << std::endl
            << "---------------------------------------------------------------------" << std::endl
            << std::endl;

  long tt0 = hobbes::tick();
  for (const auto& gn : groups) {
    auto gi = this->tests.find(gn);
    if (gi == this->tests.end()) {
      std::cout << "ERROR: no test group named '" << gn << "' exists" << std::endl;
      continue;
    }
    const auto& g = gi->second;
    auto & r = this->results[gn];

    std::cout << "  " << gn << " (" << g.size() << " test" << (g.size() == 1 ? "" : "s") << ")" << std::endl
              << "  ---------------------------------------------------------" << std::endl;

    long gt0 = hobbes::tick();
    for (size_t i = 0; i < g.size(); ++i) {
      const auto & t = g[i];
      auto & result = r[i];
      long t0 = hobbes::tick();
      try {
        t.second();
        result.record(Result::Status::Pass, hobbes::tick() - t0);
      } catch (std::exception& ex) {
        result.record(Result::Status::Fail, hobbes::tick() - t0, "[" + gn + "/" + t.first + "]: " + ex.what());
      }
      updateTerminal(t.first, result);
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

  if (auto path = args.report) {
    std::ofstream outfile(path, std::ios::out | std::ios::trunc);
    if (outfile) {
      outfile << toJSON();
      std::cout << "JSON report generated: " << path << std::endl;
    } else {
      std::cerr << "error in generating JSON report: " << strerror(errno) << std::endl;
    }
  }

  return static_cast<int>(failures.size());
}

std::string TestCoord::toJSON() {
  std::ostringstream os;
  showJSON(std::vector<GroupedResults::value_type>(results.begin(), results.end()), os);
  return os.str();
}

void listTest() {
  for (auto & test : TestCoord::instance().testGroupNames()) {
    std::cout << test << std::endl;
  }
}

void usage() {
  std::cout << "hobbes-test [--list_tests][--tests <name> [--tests <name>...][--json <path>]]" << std::endl;
}

Args parseArgs(int argc, char** argv) {
  static const struct option options[] = {
    {"help",       no_argument,       nullptr, 'h'},
    {"list_tests", no_argument,       nullptr, 'l'},
    {"tests",      required_argument, nullptr, 't'},
    {"json",       required_argument, nullptr, 'r'},
    {0,            no_argument,       nullptr, ' '}
  };

  Args args;
  int key;
  while ((key = getopt_long(argc, argv, "hlt:r:", options, nullptr)) != -1) {
    switch (key) {
      case 'l': listTest(); exit(EXIT_SUCCESS);
      case 't': args.tests.insert(optarg); break;
      case 'r': args.report = optarg; break;
      case 'h':
      case '?':
      default: usage(); exit(EXIT_SUCCESS);
    }
  }
  if (args.tests.empty()) {
    args.tests = TestCoord::instance().testGroupNames();
  }
  return args;
}

int main(int argc, char** argv) {
  return TestCoord::instance().runTestGroups(parseArgs(argc, argv));
}

