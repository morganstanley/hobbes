#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/db/file.H>
#include <hobbes/ipc/net.H>
#include <hobbes/util/perf.H>
#include "test.H"

#include <vector>
#include <string>
#include <cstring>
#include <cstdio>
#include <climits>

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#include <glob.h>

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

  std::vector<const char*> argv() const {
    std::vector<const char*> args { program.c_str() };
    if (t == batchrecv) {
      args.push_back("-s");
      args.push_back(port.c_str());
      if (consolidate) {
        args.push_back("-c");
      }
    } else if (t == batchsend) {
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
    }
    args.push_back(nullptr); // required by execv()
    return args;
  }
};

class HogApp {
public:
  HogApp(const RunMode& mode)
  : mode(mode), pid(-1) {
    strcpy(cwd, "/var/tmp/hobbes-test/hog/XXXXXX");
    hobbes::ensureDirExists(hobbes::str::rsplit(cwd, "/").first);
    if (mkdtemp(cwd) == NULL) {
      throw std::runtime_error("error while mkdtemp: " + std::string(strerror(errno)));
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
      std::freopen("stdout", "a", stdout);
      std::freopen("stderr", "a", stderr);

      auto argv = mode.argv();
      execv(argv[0], (char* const*)argv.data());

      // shouldn't reach here
      throw std::runtime_error("error while exec: " + std::string(strerror(errno)));
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

  std::vector<std::string> logpaths() const {
    std::vector<std::string> paths;
    std::string pattern(cwd);
    pattern += "/Space/*/*.log";

    glob_t g;
    if (glob(pattern.c_str(), GLOB_NOSORT, nullptr, &g) == 0) {
      for (auto i = 0; i < g.gl_pathc; ++i) {
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
    for (auto i = 0; i < logs.size(); ++i) { \
      std::ostringstream logname, loadexpr, evalexpr; \
      logname << "log" << i; \
      loadexpr << "inputFile :: (LoadFile \"" << logs[i] << "\" w) => w"; \
      evalexpr << "map(" << expr << ", alflatten([p|p<-" << logname.str() << "." << series << "]))"; \
      cc.define(logname.str(), loadexpr.str()); \
      EXPECT_TRUE(cc.compileFn<bool()>(evalexpr.str() + "==" + d[i])()); \
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

TEST(BatchSend, MultiDestination) {
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

  WITH_TIMEOUT(30, EXPECT_EQ_IN_SERIES(batchrecv[0].logpaths(), "coordinate", ".x", ({"[0..99]", "[100..199]"})));

  batchrecv[0].restart([&batchrecv](){
    batchrecv[1].restart([](){
      flushData(200, 300);
    });
  });

  WITH_TIMEOUT(30, EXPECT_EQ_IN_SERIES(batchrecv[0].logpaths(), "coordinate", ".x", ({"[0..99]", "[100..199]", "[200..299]"})));
  WITH_TIMEOUT(30, EXPECT_EQ_IN_SERIES(batchrecv[1].logpaths(), "coordinate", ".x", ({"[0..199]", "[200..299]"})));
}

