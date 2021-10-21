#include <sys/wait.h>
#include <sys/stat.h>

#include <csignal>

#include <hobbes/ipc/prepl.H>

#include "test.H"

namespace {
bool exists(const std::string &filename) {
  struct stat sb {};
  return stat(filename.c_str(), &sb) == 0;
}

void startChild(int t, hobbes::proc &p) {
  execPath([&](const std::string &ep) {
    const auto execName = [&] {
      auto name = ep + "/mock-proc";
      if (exists(name)) {
        return name;
      }
      return ep + "/mock-proc-g";
    }();

    spawn(execName + " " + std::to_string(t), &p);
  });
}

bool confirmExit(pid_t pid) {
  const std::string procDir = "/proc/" + std::to_string(pid);
  return !exists(procDir);
}

bool msgInException(const std::exception &e, const std::string &msg) {
  return std::string(e.what()).find(msg) != std::string::npos;
}

const char *const KEY = "HOBBES_LOADING_TIMEOUT";
} // namespace

TEST(Spawn, TimeoutKill) {
  hobbes::proc p;
  hobbes::str::env(KEY, "1"); // this may affect other tests' behavior
  bool exp = false;
  try {
    startChild(2, p);
  } catch (const std::runtime_error &e) {
    exp = true;
    EXPECT_TRUE(msgInException(e, "timed out"));
  }
  EXPECT_TRUE(exp);
  EXPECT_TRUE(confirmExit(p.pid));
}

TEST(Spawn, Normal) {
  hobbes::proc p;
  unsetenv(KEY);
  startChild(2, p); // should not throw any exceptions
}
