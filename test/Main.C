#include "test.H"
#include "format.H"
#include <argp.h>
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
    for (auto i = 0; i < g.size(); ++i) {
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

  return (int)failures.size();
}

std::string TestCoord::toJSON() {
  std::ostringstream os;
  showJSON(std::vector<GroupedResults::value_type>(results.begin(), results.end()), os);
  return os.str();
}

enum ShortKey {
  ListTests = 'l',
  FilterTests = 't',
  JsonReport = 'r',
};

static error_t parseArgs(int key, char* arg, struct argp_state* state) {
  auto args = reinterpret_cast<Args*>(state->input);
  switch (key) {
    case ListTests:
      for (const auto & test : TestCoord::instance().testGroupNames()) {
        std::cout << test << std::endl;
      }
      exit(0);
    case FilterTests:
      args->tests.insert(arg);
      break;
    case JsonReport:
      args->report = arg;
      break;
    case ARGP_KEY_ARG:
      args->tests.insert(arg);
      break;
    case ARGP_KEY_END:
      if (args->tests.empty()) {
        args->tests = TestCoord::instance().testGroupNames();
      }
      break;
    default:
      return ARGP_ERR_UNKNOWN;
  }
  return 0;
}

static const char* doc = "hobbes-test";
static const char* argsdoc = "[--list_tests][--tests <name>+ [--json <path>]]";
static struct argp_option options[] = {
  {"list_tests", ListTests,   nullptr, OPTION_ARG_OPTIONAL, "List available tests and exit"},
  {"tests",      FilterTests, "NAME",  0,                   "Run one or more specified test(s), --tests <name>+"},
  {"json",       JsonReport,  "FILE",  0,                   "Generate test report in JSON, --json <file>"},
  {0}
};
static struct argp argp = { options, parseArgs, argsdoc, doc };

int main(int argc, char** argv) {
  Args args;
  argp_parse(&argp, argc, argv, 0, 0, &args);
  return TestCoord::instance().runTestGroups(args);
}

