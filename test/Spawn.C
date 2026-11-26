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

void startChild(int t, hobbes::proc &p, const std::string& extraArg = "") {
  execPath([&](const std::string &ep) {
    const auto execName = [&] {
      auto name = ep + "/mock-proc";
      if (exists(name)) {
        return name;
      }
      return ep + "/mock-proc-g";
    }();

    spawn(execName + " " + std::to_string(t) + extraArg, &p);
  });
}

bool msgInException(const std::exception &e, const std::string &msg) {
  return std::string(e.what()).find(msg) != std::string::npos;
}

const char *const KEY = "HOBBES_LOADING_TIMEOUT_IN_SECS";
} // namespace

TEST(Spawn, TimeoutKill) {
  hobbes::proc p{};
  hobbes::str::env(KEY, "1"); // this may affect other tests' behavior
  bool exp = false;
  try {
    startChild(2, p);
  } catch (const std::runtime_error &e) {
    exp = true;
    EXPECT_TRUE(msgInException(e, "timed out"));
  }
  EXPECT_TRUE(exp);
  //p is invalid when spawn goes wrong, cannot be checked against
}

TEST(Spawn, Normal) {
  hobbes::proc p{};
  unsetenv(KEY);
  startChild(2, p); // should not throw any exceptions
}

TEST(Spawn, ChildStderrIsNotPartOfTheRPCProtocol) {
  hobbes::proc p{};
  unsetenv(KEY);

  // Before stderr was separated, the parent's four-byte init read consumed
  // "mock" as the response word and rejected an otherwise healthy child.
  startChild(1, p, " stderr");
}

TEST(Spawn, ChildThatExitsWithoutInitSignalIsNotLaunched) {
  hobbes::proc p{};
  hobbes::str::env(KEY, "5"); // nothing here should wait, and a failure shouldn't wait for long

  // mock-proc refuses a timeout of 0 and exits without writing anything, so
  // the parent's wait for the init signal ends with an end of file rather
  // than with a wrong answer. That used to be taken for a launched process.
  bool exp = false;
  try {
    startChild(0, p);
  } catch (const std::runtime_error &e) {
    exp = true;
    EXPECT_TRUE(msgInException(e, "no init signal"));
    EXPECT_TRUE(msgInException(e, "exited with status 1"));
  }
  EXPECT_TRUE(exp);
  unsetenv(KEY);
}

TEST(Spawn, PartialInitSignalIsNotLaunched) {
  hobbes::proc p{};
  hobbes::str::env(KEY, "5");

  // one byte of the init signal and then the end of the output: the byte is
  // the low byte of a 1 on a little-endian machine, which read into a zeroed
  // int used to compare equal to the whole signal
  bool exp = false;
  try {
    startChild(1, p, " partial");
  } catch (const std::runtime_error &e) {
    exp = true;
    EXPECT_TRUE(msgInException(e, "invalid init response"));
    EXPECT_TRUE(msgInException(e, "with output:\n\x01"));
    EXPECT_TRUE(!msgInException(e, "exec failure"));
  }
  EXPECT_TRUE(exp);
  unsetenv(KEY);
}

TEST(Spawn, PartialInitSignalFromAChildThatStaysIsBounded) {
  hobbes::proc p{};
  hobbes::str::env(KEY, "3"); // long enough for the child to start on a loaded machine

  // the same byte from a child that then stays around for a minute: the rest
  // of the signal is waited for as long as the signal is, and then the child
  // is stopped rather than waited for until it goes on its own
  const time_t t0 = ::time(nullptr);
  bool exp = false;
  try {
    startChild(60, p, " partial-stay");
  } catch (const std::runtime_error &e) {
    exp = true;
    EXPECT_TRUE(msgInException(e, "invalid init response"));
  }
  EXPECT_TRUE(exp);
  EXPECT_TRUE(::time(nullptr) - t0 < 30);
  unsetenv(KEY);
}
