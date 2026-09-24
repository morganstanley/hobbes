// Resolving a (Process "cmd" p) constraint spawns a process as a side effect
// of type-checking; these cover the opt-in that gates it.

#include <hobbes/hobbes.H>
#include "test.H"

#include <sys/stat.h>

using namespace hobbes;
static cc& c() { static __thread cc* x = nullptr; if (x == nullptr) { x = new cc(); } return *x; }

// STRFR-433927: resolving a (Process "cmd" p) constraint used to fork+execv
// "cmd" as a side effect of type unification -- merely type-checking an
// expression (compileFn here stands in for hi's ':t', a net REPL prepare(),
// or a web GET /?<expr>) ran the command whether or not the expression was
// ever going to be evaluated. Spawning is now denied unless the embedding
// cc explicitly opts in via enableProcessSpawning with an exact-match
// allowlist.
// spawn()'s result is an opaque 'process' primitive type (procman.C's
// mkPidTy), not a C++-liftable long -- discard it with 'let _ = ... in ()'
// so these tests only need compileFn<void()> to type-check, independent of
// that representation, and can focus on whether resolving the constraint
// throws or not.
static std::string processCmdExpr(const std::string& cmd) {
  return "let x = (spawn() :: (Process \"" + cmd + "\" q) => q) in ()";
}

// a command that exists wherever these tests run and speaks the init protocol
// a spawned hobbes sub-process must speak
//
// this used to be "/usr/bin/true", which is neither: it is absent from a
// sandboxed build (the Nix builds CI runs have no /usr/bin at all), and it
// exits without answering. mock-proc is built beside the test binary for
// exactly this purpose, and Spawn's tests find it the same way.
static std::string spawnableCmd() {
  std::string cmd;
  execPath([&](const std::string& ep) {
    struct stat sb {};
    cmd = ep + "/mock-proc";
    if (stat(cmd.c_str(), &sb) != 0) {
      cmd = ep + "/mock-proc-g";
    }
  });
  // mock-proc takes the seconds it should live for, and requires it to be
  // positive; Spawn's tests pass 2 for the same reason
  return cmd + " 2";
}

TEST(Process, processConstraintSpawningDeniedByDefault) {
  hobbes::cc compiler;
  bool threw = false;
  try {
    compiler.compileFn<void()>(processCmdExpr("/usr/bin/true"));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("Process constraint rejected") != std::string::npos);
  }
  EXPECT_TRUE(threw);
}

TEST(Process, processConstraintSpawningOptInAllowsExactCommand) {
  const std::string cmd = spawnableCmd();
  hobbes::cc compiler;
  compiler.enableProcessSpawning({cmd});
  // must not throw: the exact allowlisted command is permitted to spawn
  compiler.compileFn<void()>(processCmdExpr(cmd))();
}

TEST(Process, processConstraintOptInDoesNotAllowOtherCommands) {
  // negative control: opting in for one command must not open the gate for
  // every command
  hobbes::cc compiler;
  compiler.enableProcessSpawning({spawnableCmd()});
  bool threw = false;
  try {
    compiler.compileFn<void()>(processCmdExpr("/usr/bin/false"));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("Process constraint rejected") != std::string::npos);
  }
  EXPECT_TRUE(threw);
}
