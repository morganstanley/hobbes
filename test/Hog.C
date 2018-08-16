#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/db/file.H>
#include <hobbes/ipc/net.H>
#include <hobbes/util/perf.H>
#include <hobbes/util/str.H>
#include "test.H"

#include <vector>
#include <string>
#include <cstring>
#include <cstdio>
#include <climits>
#include <thread>

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#include <glob.h>
#include <ftw.h>

std::string hogBinaryPath() {
  static char path[PATH_MAX];
  auto p = getenv("HOG_BIN");
  if (p) {
    return p;
  }
  // assuming hog and hobbes-test are in the same folder
  getcwd(path, sizeof(path));
  strcat(path, "/hog");
  return path;
}

DEFINE_HSTORE_STRUCT(
  Point3D,
  (int, x),
  (int, y),
  (int, z)
);

DEFINE_STORAGE_GROUP(
  Space,
  1024,
  hobbes::storage::Reliable,
  hobbes::storage::AutoCommit
);

struct RunMode {
  enum type { local, batchsend, batchrecv };

  type                     t;

  // batchsend
  std::vector<std::string> groups;
  std::vector<std::string> sendto;
  std::string              batchsendsize;
  std::string              batchsendtime;

  // batchrecv
  std::string              port;
  bool                     consolidate;

  // binary
  std::string              program;

  RunMode(const std::vector<std::string>& groups, const std::vector<std::string>& sendto, size_t size, long time)
  : t(batchsend), groups(groups), sendto(sendto),
    batchsendsize(std::to_string(size)),
    batchsendtime(std::to_string(time)),
    program(hogBinaryPath()) {
  }

  RunMode(int port, bool consolidate = false)
    : t(batchrecv), port(std::to_string(port)), consolidate(consolidate), program(hogBinaryPath()) {}

  RunMode(const std::vector<std::string>& groups, bool consolidate = false)
    : t(local), groups(groups), consolidate(consolidate), program(hogBinaryPath()) {}

  std::vector<const char*> argv() const {
    std::vector<const char*> args { program.c_str() };
    switch (t) {
    case local:
      args.push_back("-g");
      for (const auto & g : groups) {
        args.push_back(g.c_str());
      }
      if (consolidate) {
        args.push_back("-c");
      }
      break;
    case batchsend:
      args.push_back("-g");
      for (const auto & g : groups) {
        args.push_back(g.c_str());
      }
      args.push_back("-p");
      args.push_back(batchsendtime.c_str());
      args.push_back(batchsendsize.c_str());
      for (const auto & d : sendto) {
        args.push_back(d.c_str());
      }
      break;
    case batchrecv:
      args.push_back("-s");
      args.push_back(port.c_str());
      if (consolidate) {
        args.push_back("-c");
      }
      break;
    }
    args.push_back(nullptr); // required by execv()
    return args;
  }
};

class HogApp {
public:
  HogApp(const RunMode& mode)
  : mode(mode), pid(-1) {
    strcpy(cwd, "./.htest/hog/XXXXXX");
    hobbes::ensureDirExists(hobbes::str::rsplit(cwd, "/").first);
    if (mkdtemp(cwd) == NULL) {
      throw std::runtime_error("error while mkdtemp('" + std::string(cwd) + "'): " + std::string(strerror(errno)));
    }
  }

  ~HogApp() {
    if (started()) {
      stop();
    }
  }

  void start() {
    pid = fork();
    if (pid == -1) {
      throw std::runtime_error("error while fork: " + std::string(strerror(errno)));
    } else if (pid == 0) {
      chdir(cwd);

      // IO redirect
      int ofd  = dup(STDOUT_FILENO);
      int nofd = open("stdout", O_WRONLY|O_APPEND|O_CREAT, 00644);
      dup2(nofd, STDOUT_FILENO);
      close(nofd);

      int efd  = dup(STDERR_FILENO);
      int nefd = open("stderr", O_WRONLY|O_APPEND|O_CREAT, 00644);
      dup2(nefd, STDERR_FILENO);

      // exec
      auto argv = mode.argv();
      execv(argv[0], const_cast<char* const*>(argv.data()));

      // if we get here, restore IO, show an error
      dup2(ofd, STDOUT_FILENO);
      close(ofd);
      dup2(efd, STDERR_FILENO);
      close(efd);

      std::cout << "error trying to exec '" << argv[0] << "' : " << strerror(errno) << std::endl;
      exit(-1);
    }
  }

  void stop() {
    if (started()) {
      if (kill(pid, SIGTERM) == -1) {
        throw std::runtime_error("error while kill: " + std::string(strerror(errno)));
      }
      int ws;
      if (waitpid(pid, &ws, 0) == -1) {
        throw std::runtime_error("error while waitpid: " + std::string(strerror(errno)));
      }
      pid = -1;
    }
  }

  void restart(std::function<void()> meanwhileFn = [](){}) {
    stop();
    if (meanwhileFn) {
      meanwhileFn();
    }
    start();
  }

  bool started() const { return pid != -1; }

  std::string name() const {
    return hobbes::str::rsplit(cwd, "/").second;
  }

  std::string localport() const {
    return "localhost:" + mode.port;
  }

  std::string basePath() const { return this->cwd; }

  std::vector<std::string> logpaths() const {
    std::string group;
    if (this->mode.groups.size() == 0) {
      group = "Space"; // maybe refactor a little to avoid this (if we ever need a batchrecv test for a different group)
    } else if (this->mode.groups.size() > 1) {
      throw std::runtime_error("multi groups through logpaths nyi");
    } else {
      group = this->mode.groups[0];
    }

    std::vector<std::string> paths;
    std::string pattern(cwd);
    pattern += "/" + group + "/*/*.log";

    glob_t g;
    if (glob(pattern.c_str(), GLOB_NOSORT, nullptr, &g) == 0) {
      for (size_t i = 0; i < g.gl_pathc; ++i) {
        paths.push_back(g.gl_pathv[i]);
      }
      globfree(&g);
    }

    std::sort(paths.begin(), paths.end(), [](const std::string& p1, const std::string& p2){
      struct stat s1, s2;
      stat(p1.c_str(), &s1);
      stat(p2.c_str(), &s2);
      return s1.st_ctime < s2.st_ctime;
    });

    return paths;
  }

private:
  const RunMode mode;
  char cwd[PATH_MAX];
  pid_t pid;
};

int availablePort(int low, int high) {
  auto port = low;
  while (port < high) {
    try {
      close(hobbes::allocateServer(port));
      return port;
    } catch (std::exception&) {
      ++port;
    }
  }
  throw std::runtime_error("port unavailable");
}

void flushData(int first, int last) {
  for (auto i = first; i < last; ++i) {
    HSTORE(Space, coordinate, Point3D{i, i, i});
  }
  Space.commit(); // flushing any data in the queue
}

#define EXPECT_EQ_IN_SERIES(logs, series, expr, expect) \
  do { \
    hobbes::cc cc; \
    std::vector<const char*> d expect; \
    EXPECT_EQ(logs.size(), d.size()); \
    for (size_t i = 0; i < logs.size(); ++i) { \
      std::ostringstream logname, loadexpr, evalexpr; \
      logname << "log" << i; \
      loadexpr << "inputFile :: (LoadFile \"" << logs[i] << "\" w) => w"; \
      evalexpr << "map(" << expr << ", alflatten([p|p<-" << logname.str() << "." << series << "]))"; \
      cc.define(logname.str(), loadexpr.str()); \
      cc.compileFn<void()>("let a=(" + std::string(evalexpr.str()) + "); b=" + std::string(d[i]) + " in if (a != b) then putStrLn(\"warning: \" ++ show(a) ++ \" != " + std::string(d[i]) + "\") else ()")(); \
/*      EXPECT_TRUE(cc.compileFn<bool()>(evalexpr.str() + "==" + d[i])()); */ \
    } \
  } while (0)

#define WITH_TIMEOUT(second, expectFn) \
  do { \
    auto now = hobbes::time(); \
    const auto end = now + second * 1000000000L; \
    while (true) { \
      try { \
        expectFn; \
        break; \
      } catch (std::exception&) { \
        now = hobbes::time(); \
        if (now < end) { \
          sleep(1); \
        } else { \
          throw; \
        } \
      } \
    } \
  } while (0)

#if !defined(BUILD_OSX)

#if 0
TEST(Hog, MultiDestination) {
  std::vector<HogApp> batchrecv {
    HogApp(RunMode(availablePort(10000, 10100))),
    HogApp(RunMode(availablePort(10100, 10200)))
  };
  HogApp batchsend(RunMode{{"Space"}, {batchrecv[0].localport(), batchrecv[1].localport()}, 1024, 1000000});

  for (auto & hog : batchrecv) {
    hog.start();
  }
  batchsend.start();
  sleep(5);

  flushData(0, 100);

  WITH_TIMEOUT(30, EXPECT_EQ_IN_SERIES(batchrecv[0].logpaths(), "coordinate", ".x", {"[0..99]"}));
  WITH_TIMEOUT(30, EXPECT_EQ_IN_SERIES(batchrecv[1].logpaths(), "coordinate", ".x", {"[0..99]"}));

  batchrecv[0].restart([&batchrecv](){
    flushData(100, 200);

    WITH_TIMEOUT(10, EXPECT_EQ_IN_SERIES(batchrecv[0].logpaths(), "coordinate", ".x", {"[0..99]"}));
    WITH_TIMEOUT(10, EXPECT_EQ_IN_SERIES(batchrecv[1].logpaths(), "coordinate", ".x", {"[0..199]"}));
  });

  WITH_TIMEOUT(60, EXPECT_EQ(batchrecv[0].logpaths().size(), size_t(2)));
  WITH_TIMEOUT(30, EXPECT_EQ_IN_SERIES(batchrecv[0].logpaths(), "coordinate", ".x", ({"[0..99]", "[100..199]"})));

  batchrecv[0].restart([&batchrecv](){
    batchrecv[1].restart([](){
      flushData(200, 300);
    });
  });

  WITH_TIMEOUT(60, EXPECT_EQ(batchrecv[0].logpaths().size(), size_t(3)));
  WITH_TIMEOUT(30, EXPECT_EQ_IN_SERIES(batchrecv[0].logpaths(), "coordinate", ".x", ({"[0..99]", "[100..199]", "[200..299]"})));
  WITH_TIMEOUT(30, EXPECT_EQ_IN_SERIES(batchrecv[1].logpaths(), "coordinate", ".x", ({"[0..199]", "[200..299]"})));
}
#endif

DEFINE_STORAGE_GROUP(
  KillAndResume,
  8,
  hobbes::storage::Reliable,
  hobbes::storage::ManualCommit
);

TEST(Hog, KillAndResume) {
  HogApp local(RunMode{{"KillAndResume"}, true});
  local.start();
  sleep(5);

  HLOG(KillAndResume, seq, "seq $0", 0); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0", 1); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0", 2); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0", 3); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0", 4); // <-- keep this one pending

  sleep(5);
  local.restart([](){sleep(1);});
  sleep(5);

  HLOG(KillAndResume, seq, "seq $0", 5); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0", 6); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0", 7); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0", 8); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0", 9); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0",10); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0",11); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0",12); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0",13); KillAndResume.commit();
  HLOG(KillAndResume, seq, "seq $0",14); KillAndResume.commit();

  sleep(5);

  hobbes::cc c;
  size_t i = 0;
  for (const auto& log : local.logpaths()) {
    c.define("f"+hobbes::str::from(i++), "inputFile::(LoadFile \""+log+"\" w)=>w");
  }
  EXPECT_TRUE(c.compileFn<bool()>("f0.seq[0:] == [3,2,1,0]")());
  EXPECT_TRUE(c.compileFn<bool()>("f1.seq[0:] == [14,13,12,11,10,9,8,7,6,5,4]")());
}

DEFINE_STORAGE_GROUP(
  RestartEngine,
  8,
  hobbes::storage::Unreliable,
  hobbes::storage::AutoCommit,
  hobbes::storage::Spin
);

TEST(Hog, RestartEngine) {
  HogApp local(RunMode{{"RestartEngine"}, /* consolidate = */true});
  local.start();
  sleep(5);

  auto fn = [](){
    HLOG(RestartEngine, seq, "seq $0", 0);
    HLOG(RestartEngine, seq, "seq $0", 1);
    HLOG(RestartEngine, seq, "seq $0", 2);
    HLOG(RestartEngine, seq, "seq $0", 3);
    RestartEngine.commit();
  };

  std::thread t1(fn), t2(fn);
  t1.join(); t2.join();

  sleep(5);

  std::thread t3(fn), t4(fn);
  t3.join(); t4.join();

  sleep(5);

  hobbes::cc c;
  auto i = 0;
  for (const auto& log : local.logpaths()) {
    c.define("f"+hobbes::str::from(i++), "inputFile::(LoadFile \""+log+"\" w)=>w");
  }
  EXPECT_TRUE(c.compileFn<bool()>("size(f0.seq[0:]) == 4 * 4")());
}

DEFINE_STORAGE_GROUP(
  TestRecTypes,
  8,
  hobbes::storage::Reliable,
  hobbes::storage::ManualCommit
);

struct SectTest {
  int x;
  double y() const { return 3.14159; }
  std::array<short, 10> z;
  std::string w;
  hobbes::datetimeT dt;
  hobbes::timespanT ts;
  void* q;
};
DEFINE_HSTORE_STRUCT_VIEW(SectTest, x, y, z, w, dt, ts);

TEST(Hog, SupportedTypes) {
  HogApp local(RunMode{{"TestRecTypes"}, /* consolidate = */ true});
  local.start();
  sleep(5);

  SectTest st;
  st.x = 42;
  for (size_t i = 0; i < 10; ++i) { st.z[i] = short(i); }
  st.w = "test";
  st.dt.value = hobbes::now();
  st.ts.value = 60*1000*1000;
  HLOG(TestRecTypes, sectView, "test sect view", st);
  TestRecTypes.commit();

  sleep(5);

  hobbes::cc c;
  auto i = 0;
  for (const auto& log : local.logpaths()) {
    c.define("f"+hobbes::str::from(i++), "inputFile::(LoadFile \""+log+"\" w)=>w");
  }
  EXPECT_TRUE(c.compileFn<bool()>("all(\\x.x  matches {x=42,y=3.14159,z=[0S,1S,2S,3S,4S,5S,6S,7S,8S,9S],w=\"test\"},f0.sectView[0:])")());
}

DEFINE_STORAGE_GROUP(
  TestUnrReconnect,
  8,
  hobbes::storage::Unreliable,
  hobbes::storage::AutoCommit
);

TEST(Hog, UnreliableReconnect) {
  // no consumer running, produce until we fail
  while (HLOG(TestUnrReconnect, seq, "unreliable seq"));

  // now that we've failed to log once, start a hog process
  HogApp local(RunMode{{"TestUnrReconnect"}, /* consolidate = */ true});
  local.start();
  sleep(10);

  // at some point we should succeed in logging when we transparently reconnect
  auto t0 = hobbes::storage::internal::spin::poll_tickNS();
  bool s = false;
  while (((hobbes::storage::internal::spin::poll_tickNS() - t0)/(1000L*1000L*1000L)) < 120) {
    if (HLOG(TestUnrReconnect, seq, "unreliable seq")) {
      s = true;
      break;
    }
  }
  EXPECT_TRUE(s && "reconnection failed");
}

void rmrf(const char* p) {
  nftw(p, [](const char* fp, const struct stat*, int tf, struct FTW*) -> int { remove(fp); return 0; }, 64, FTW_DEPTH | FTW_PHYS);
}
TEST(Hog, Cleanup) {
  rmrf("./.htest");
  rmrf("./Space");
}

#endif


