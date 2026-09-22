
#include "test.H"
#include <hobbes/eval/cmodule.H>
#include <hobbes/hobbes.H>
#include <hobbes/ipc/net.H>
#include <hobbes/net.H>
#include <hobbes/util/codec.H>

#include <atomic>
#include <condition_variable>
#include <cstdlib>
#include <mutex>
#include <thread>

#include <fcntl.h>
#include <sys/socket.h>
#include <unistd.h>

using namespace hobbes;

// Forward declaration so the atexit cleanup can reach both server thread sets.
static void joinServerThreadsAtExit();

static cc &c() {
  static cc x;
  // Register the thread-join cleanup AFTER cc x is fully constructed, so at
  // process exit the handler runs before cc x is destroyed (atexit handlers
  // run in reverse registration order, interleaved with static destructors).
  static std::once_flag registered;
  std::call_once(registered, [] { std::atexit(joinServerThreadsAtExit); });
  return x;
}

// start a basic server to validate RPC communication
static int serverPort = -1;
static bool serverReady = false;
static std::mutex serverMtx;
static std::condition_variable serverStartup;
static std::atomic<bool> serverStop{false};
static std::thread serverThread;

static void runTestServer(int ps, int pe) {
  std::unique_lock<std::mutex> lk(serverMtx);
  serverPort = ps;
  while (serverPort < pe) {
    try {
      installNetREPL(serverPort, &c());
      serverReady = true;
      lk.unlock();
      serverStartup.notify_one();
      runEventLoop([&]{ return serverStop.load(); });
      return;
    } catch (std::exception &) {
      ++serverPort;
    }
  }
  serverPort = -1;
  serverReady = true;
  lk.unlock();
  serverStartup.notify_one();
}

int testServerPort() {
  if (serverPort < 0) {
    // If a previous attempt left a finished thread, join it before reassigning
    // (move-assigning into a joinable std::thread calls std::terminate).
    if (serverThread.joinable()) {
      serverThread.join();
    }
    std::unique_lock<std::mutex> lk(serverMtx);
    serverReady = false;
    serverThread = std::thread([] { return runTestServer(8765, 9500); });
    serverStartup.wait(lk, []{ return serverReady; });
    if (serverPort < 0) {
      throw std::runtime_error("Couldn't allocate port for test server");
    }
  }
  return serverPort;
}

// start a basic server with configurable hostname and port to validate RPC
// communication
static int serverPortWithHost = -1;
static bool serverWithHostReady = false;
static std::mutex serverMtxWithHost;
static std::condition_variable serverWithHostStartup;
static std::atomic<bool> serverWithHostStop{false};
static std::thread serverWithHostThread;

static void runTestServerWithHost(int ps, int pe, const std::string &host) {
  std::unique_lock<std::mutex> lk(serverMtxWithHost);
  serverPortWithHost = ps;
  while (serverPortWithHost < pe) {
    try {
      installNetREPL(host, serverPortWithHost, &c());
      serverWithHostReady = true;
      lk.unlock();
      serverWithHostStartup.notify_one();
      runEventLoop([&]{ return serverWithHostStop.load(); });
      return;
    } catch (std::exception &) {
      ++serverPortWithHost;
    }
  }
  serverPortWithHost = -1;
  serverWithHostReady = true;
  lk.unlock();
  serverWithHostStartup.notify_one();
}

int testServerWithHostPort(const std::string &host = "") {
  if (serverPortWithHost < 0) {
    if (serverWithHostThread.joinable()) {
      serverWithHostThread.join();
    }
    std::unique_lock<std::mutex> lk(serverMtxWithHost);
    serverWithHostReady = false;
    serverWithHostThread = std::thread(
        [host] { return runTestServerWithHost(9501, 10500, host); });
    serverWithHostStartup.wait(lk, []{ return serverWithHostReady; });
    if (serverPortWithHost < 0) {
      throw std::runtime_error("Couldn't allocate port for test server");
    }
  }
  return serverPortWithHost;
}

static void joinServerThreadsAtExit() {
  serverStop = true;
  serverWithHostStop = true;
  if (serverThread.joinable()) {
    serverThread.join();
  }
  if (serverWithHostThread.joinable()) {
    serverWithHostThread.join();
  }
}
/**************************
 * types/data for net communication
 **************************/
using NameCounts = std::map<std::string, size_t>;
using NC = std::pair<std::string, size_t>;
bool operator==(const NameCounts &ncs, const std::vector<NC> &ncsc) {
  for (const auto &nc : ncsc) {
    auto i = ncs.find(nc.first);
    if (i == ncs.end() || i->second != nc.second) {
      return false;
    }
  }
  return ncs.size() == ncsc.size();
}

std::ostream &operator<<(std::ostream &os, const NC &nc) {
  os << "(\"" << nc.first << "\", " << nc.second << ")";
  return os;
}
template <typename T>
std::ostream &operator<<(std::ostream &os, const std::vector<T> &xs) {
  os << "[";
  if (!xs.empty()) {
    os << xs[0];
    for (size_t i = 1; i < xs.size(); ++i)
      os << ", " << xs[i];
  }
  os << "]";
  return os;
}
std::ostream &operator<<(std::ostream &os, const NameCounts &ncsc) {
  os << "{";
  auto nc = ncsc.begin();
  if (nc != ncsc.end()) {
    os << "\"" << nc->first << "\" => " << nc->second;
    ++nc;
    for (; nc != ncsc.end(); ++nc)
      os << ", "
         << "\"" << nc->first << "\" => " << nc->second;
  }
  os << "}";
  return os;
}

DEFINE_ENUM(Kid, (Jim), (Bob));

DEFINE_STRUCT(Group, (std::string, id), (Kid, kid), (double, aa), (size_t, bb));
using Groups = std::vector<Group>;
std::ostream &operator<<(std::ostream &os, const Group &g) {
  os << "{id=\"" << g.id
     << "\", kid=" << ((g.kid == Kid::Jim()) ? "|Jim|" : "|Bob|")
     << ", aa=" << g.aa << ", bb=" << g.bb << "}";
  return os;
}

DEFINE_VARIANT(V, (Bob, int), (Frank, std::string), (Nothing, hobbes::unit));

// make sure that enums with custom ctor IDs transfer correctly
enum class CustomIDEnum : uint32_t { Red = 55, Green = 12, Blue = 257 };
std::ostream &operator<<(std::ostream &os, const CustomIDEnum &e) {
  switch (e) {
  case CustomIDEnum::Red:
    os << "|Red|";
    break;
  case CustomIDEnum::Green:
    os << "|Green|";
    break;
  case CustomIDEnum::Blue:
    os << "|Blue|";
    break;
  default:
    os << "???";
    break;
  }
  return os;
}

namespace hobbes {
namespace net {
template <> struct io<CustomIDEnum> {
  static const bool can_memcpy = true;
  static ty::desc type() {
    ty::Variant::Ctors cs;
    cs.push_back(ty::Variant::Ctor(
        "Red", static_cast<uint32_t>(CustomIDEnum::Red), ty::prim("unit")));
    cs.push_back(ty::Variant::Ctor(
        "Green", static_cast<uint32_t>(CustomIDEnum::Green), ty::prim("unit")));
    cs.push_back(ty::Variant::Ctor(
        "Blue", static_cast<uint32_t>(CustomIDEnum::Blue), ty::prim("unit")));
    return ty::variant(cs);
  }
  static void write(int s, const CustomIDEnum &x) {
    io<uint32_t>::write(s, static_cast<uint32_t>(x));
  }
  static void read(int s, CustomIDEnum *x) {
    io<uint32_t>::read(s, reinterpret_cast<uint32_t *>(x));
  }

  using async_read_state = io<uint32_t>::async_read_state;
  static void prepare(async_read_state *o) { io<uint32_t>::prepare(o); }
  static bool accum(int s, async_read_state *o, CustomIDEnum *x) {
    return io<uint32_t>::accum(s, o, reinterpret_cast<uint32_t *>(x));
  }
};
} // namespace net
} // namespace hobbes

using rgb_t = int[3];
DEFINE_STRUCT(RGB, (rgb_t, val));

/**************************
 * the synchronous client networking API
 **************************/
DEFINE_NET_CLIENT(
    SyncClient, (add, int(int, int), "\\x y.x+y"),
    (doit, std::string(), "\\().\"missiles launched\""),
    (misc, NameCounts(std::string, size_t),
     "\\n c.[(n++\"_\"++show(i), i) | i <- [0L..c]]"),
    (grpv, V(Group), "\\_.|Frank=\"frank\"|"),
    (nothing, V(), "\\_.|Nothing=()|"),
    (recover, Groups(int, int),
     "\\i "
     "e.[{id=\"group_\"++show(k),kid=|Jim|,aa=convert(k),bb=convert(k)}|k<-[i.."
     "e]]"),
    (eidv, CustomIDEnum(CustomIDEnum), "id"),
    (inverse, RGB(RGB),
     "\\x.do{saelem(x.val,0L)<-255-saelem(x.val,0L);saelem(x.val,1L)<-255-"
     "saelem(x.val,1L);saelem(x.val,2L)<-255-saelem(x.val,2L); return x}"));

TEST(Net, syncClientAPI) {
  SyncClient c("localhost", testServerPort());
  EXPECT_EQ(c.add(1, 2), 3);
  EXPECT_EQ(c.doit(), "missiles launched");
  EXPECT_EQ(c.misc("foo", 5),
            list(NC("foo_0", 0), NC("foo_1", 1), NC("foo_2", 2), NC("foo_3", 3),
                 NC("foo_4", 4), NC("foo_5", 5)));

  Group grp = {"id", Kid::Jim(), 4.2, 42};
  EXPECT_EQ(grp, grp);
  for (size_t i = 0; i < 10; ++i) {
    EXPECT_EQ(c.grpv(grp), V::Frank("frank"));
  }
  EXPECT_EQ(c.nothing(), V::Nothing(hobbes::unit()));

  Groups ros = {{"group_0", Kid::Jim(), 0.0, 0},
                {"group_1", Kid::Jim(), 1.0, 1},
                {"group_2", Kid::Jim(), 2.0, 2},
                {"group_3", Kid::Jim(), 3.0, 3},
                {"group_4", Kid::Jim(), 4.0, 4}};
  EXPECT_EQ(c.recover(0, 4), ros);

  EXPECT_EQ(c.eidv(CustomIDEnum::Green), CustomIDEnum::Green);

  auto inv = c.inverse(RGB{{0, 255, 0}});
  EXPECT_EQ(std::vector<int>(inv.val, inv.val + 3),
            std::vector<int>({255, 0, 255}));
}

TEST(Net, rejectedHandshakeLeavesTheServerServing) {
  // a peer that opens with a protocol version the server does not speak is
  // dropped at the handshake. The drop used to fall through to register an
  // event handler on the socket it had just closed; that registration failed,
  // and the failure path closed the socket a second time -- by then possibly
  // some other thread's. The peer should simply see its connection closed,
  // and the next client should be served as usual.
  int fd = connectSocket("localhost", testServerPort());
  fdwrite(fd, static_cast<uint32_t>(0xdeadbeef));
  char b = 0;
  EXPECT_EQ(::recv(fd, &b, 1, 0), ssize_t(0)); // orderly EOF: the server hung up
  ::close(fd);

  SyncClient c("localhost", testServerPort());
  EXPECT_EQ(c.add(1, 2), 3);
}

TEST(Net, syncClientAPIWithConfiguredHostName) {
  SyncClient c("localhost", testServerWithHostPort("localhost"));
  EXPECT_EQ(c.add(1, 2), 3);
  EXPECT_EQ(c.doit(), "missiles launched");
  EXPECT_EQ(c.misc("foo", 5),
            list(NC("foo_0", 0), NC("foo_1", 1), NC("foo_2", 2), NC("foo_3", 3),
                 NC("foo_4", 4), NC("foo_5", 5)));

  Group grp = {"id", Kid::Jim(), 4.2, 42};
  EXPECT_EQ(grp, grp);
  for (size_t i = 0; i < 10; ++i) {
    EXPECT_EQ(c.grpv(grp), V::Frank("frank"));
  }
  EXPECT_EQ(c.nothing(), V::Nothing(hobbes::unit()));

  Groups ros = {{"group_0", Kid::Jim(), 0.0, 0},
                {"group_1", Kid::Jim(), 1.0, 1},
                {"group_2", Kid::Jim(), 2.0, 2},
                {"group_3", Kid::Jim(), 3.0, 3},
                {"group_4", Kid::Jim(), 4.0, 4}};
  EXPECT_EQ(c.recover(0, 4), ros);

  EXPECT_EQ(c.eidv(CustomIDEnum::Green), CustomIDEnum::Green);

  auto inv = c.inverse(RGB{{0, 255, 0}});
  EXPECT_EQ(std::vector<int>(inv.val, inv.val + 3),
            std::vector<int>({255, 0, 255}));
}

TEST(Net, syncClientAPIWithUnConfiguredHostName) {
  SyncClient c("localhost", testServerWithHostPort());
  EXPECT_EQ(c.add(1, 2), 3);
  EXPECT_EQ(c.doit(), "missiles launched");
  EXPECT_EQ(c.misc("foo", 5),
            list(NC("foo_0", 0), NC("foo_1", 1), NC("foo_2", 2), NC("foo_3", 3),
                 NC("foo_4", 4), NC("foo_5", 5)));

  Group grp = {"id", Kid::Jim(), 4.2, 42};
  EXPECT_EQ(grp, grp);
  for (size_t i = 0; i < 10; ++i) {
    EXPECT_EQ(c.grpv(grp), V::Frank("frank"));
  }
  EXPECT_EQ(c.nothing(), V::Nothing(hobbes::unit()));

  Groups ros = {{"group_0", Kid::Jim(), 0.0, 0},
                {"group_1", Kid::Jim(), 1.0, 1},
                {"group_2", Kid::Jim(), 2.0, 2},
                {"group_3", Kid::Jim(), 3.0, 3},
                {"group_4", Kid::Jim(), 4.0, 4}};
  EXPECT_EQ(c.recover(0, 4), ros);

  EXPECT_EQ(c.eidv(CustomIDEnum::Green), CustomIDEnum::Green);

  auto inv = c.inverse(RGB{{0, 255, 0}});
  EXPECT_EQ(std::vector<int>(inv.val, inv.val + 3),
            std::vector<int>({255, 0, 255}));
}
/**************************
 * the asynchronous client networking API
 **************************/
DEFINE_ASYNC_NET_CLIENT(
    AsyncClient, (add, int(int, int), "\\x y.x+y"),
    (doit, std::string(), "\\().\"missiles launched\""),
    (misc, NameCounts(std::string, size_t),
     "\\n c.[(n++\"_\"++show(i), i) | i <- [0L..c]]"),
    (grpv, V(Group), "\\_.|Frank=\"frank\"|"),
    (nothing, V(), "\\().|Nothing=()|"),
    (recover, Groups(int, int),
     "\\i "
     "e.[{id=\"group_\"++show(k),kid=|Jim|,aa=convert(k),bb=convert(k)}|k<-[i.."
     "e]]"),
    (eidv, CustomIDEnum(CustomIDEnum), "id"),
    (inverse, RGB(RGB),
     "\\x.do{saelem(x.val,0L)<-255-saelem(x.val,0L);saelem(x.val,1L)<-255-"
     "saelem(x.val,1L);saelem(x.val,2L)<-255-saelem(x.val,2L); return x}"));
void stepAsyncClient(int, void *p) {
  reinterpret_cast<AsyncClient *>(p)->step();
}

TEST(Net, asyncClientAPI) {
  AsyncClient c("127.0.0.1", "127.0.0.1", testServerPort());
  c.add(1, 2, [](int r) { EXPECT_EQ(r, 3); });
  c.doit([](const std::string &r) { EXPECT_EQ(r, "missiles launched") });
  c.misc("foo", 5, [](const NameCounts &ncs) {
    EXPECT_EQ(ncs, list(NC("foo_0", 0), NC("foo_1", 1), NC("foo_2", 2),
                        NC("foo_3", 3), NC("foo_4", 4), NC("foo_5", 5)));
  });

  Group grp = {"id", Kid::Jim(), 4.2, 42};
  for (size_t i = 0; i < 1000; ++i) {
    c.grpv(grp, [](const V &r) { EXPECT_EQ(r, V::Frank("frank")); });
    c.nothing([](const V &r) { EXPECT_EQ(r, V::Nothing(hobbes::unit())); });
  }

  Groups ros = {{"group_0", Kid::Jim(), 0.0, 0},
                {"group_1", Kid::Jim(), 1.0, 1},
                {"group_2", Kid::Jim(), 2.0, 2},
                {"group_3", Kid::Jim(), 3.0, 3},
                {"group_4", Kid::Jim(), 4.0, 4}};
  c.recover(0, 4, [&](const Groups &r) { EXPECT_EQ(r, ros); });

  c.eidv(CustomIDEnum::Green,
         [](const CustomIDEnum &x) { EXPECT_EQ(x, CustomIDEnum::Green); });

  c.inverse(RGB{{0, 255, 0}}, [](const RGB &inv) {
    EXPECT_EQ(std::vector<int>(inv.val, inv.val + 3),
              std::vector<int>({255, 0, 255}));
  });

  // run a quick epoll loop to process results asynchronously (expect all
  // results within 30 seconds)
  registerEventHandler(c.fd(), &stepAsyncClient, &c);
  for (size_t s = 0; s < 30 && c.pendingRequests() > 0; ++s) {
    runEventLoop(1000 * 1000);
  }
  EXPECT_EQ(c.pendingRequests(), size_t(0));
}

TEST(Net, asyncClientAPIWithConfiguredHostName) {
  AsyncClient c("127.0.0.1", "127.0.0.1", testServerWithHostPort("127.0.0.1"));
  c.add(1, 2, [](int r) { EXPECT_EQ(r, 3); });
  c.doit([](const std::string &r) { EXPECT_EQ(r, "missiles launched") });
  c.misc("foo", 5, [](const NameCounts &ncs) {
    EXPECT_EQ(ncs, list(NC("foo_0", 0), NC("foo_1", 1), NC("foo_2", 2),
                        NC("foo_3", 3), NC("foo_4", 4), NC("foo_5", 5)));
  });

  Group grp = {"id", Kid::Jim(), 4.2, 42};
  for (size_t i = 0; i < 1000; ++i) {
    c.grpv(grp, [](const V &r) { EXPECT_EQ(r, V::Frank("frank")); });
    c.nothing([](const V &r) { EXPECT_EQ(r, V::Nothing(hobbes::unit())); });
  }

  Groups ros = {{"group_0", Kid::Jim(), 0.0, 0},
                {"group_1", Kid::Jim(), 1.0, 1},
                {"group_2", Kid::Jim(), 2.0, 2},
                {"group_3", Kid::Jim(), 3.0, 3},
                {"group_4", Kid::Jim(), 4.0, 4}};
  c.recover(0, 4, [&](const Groups &r) { EXPECT_EQ(r, ros); });

  c.eidv(CustomIDEnum::Green,
         [](const CustomIDEnum &x) { EXPECT_EQ(x, CustomIDEnum::Green); });

  c.inverse(RGB{{0, 255, 0}}, [](const RGB &inv) {
    EXPECT_EQ(std::vector<int>(inv.val, inv.val + 3),
              std::vector<int>({255, 0, 255}));
  });

  // run a quick epoll loop to process results asynchronously (expect all
  // results within 30 seconds)
  registerEventHandler(c.fd(), &stepAsyncClient, &c);
  for (size_t s = 0; s < 30 && c.pendingRequests() > 0; ++s) {
    runEventLoop(1000 * 1000);
  }
  EXPECT_EQ(c.pendingRequests(), size_t(0));
}

TEST(Net, asyncClientAPIWithUnConfiguredHostName) {
  AsyncClient c("127.0.0.1", "127.0.0.1", testServerWithHostPort());
  c.add(1, 2, [](int r) { EXPECT_EQ(r, 3); });
  c.doit([](const std::string &r) { EXPECT_EQ(r, "missiles launched") });
  c.misc("foo", 5, [](const NameCounts &ncs) {
    EXPECT_EQ(ncs, list(NC("foo_0", 0), NC("foo_1", 1), NC("foo_2", 2),
                        NC("foo_3", 3), NC("foo_4", 4), NC("foo_5", 5)));
  });

  Group grp = {"id", Kid::Jim(), 4.2, 42};
  for (size_t i = 0; i < 1000; ++i) {
    c.grpv(grp, [](const V &r) { EXPECT_EQ(r, V::Frank("frank")); });
    c.nothing([](const V &r) { EXPECT_EQ(r, V::Nothing(hobbes::unit())); });
  }

  Groups ros = {{"group_0", Kid::Jim(), 0.0, 0},
                {"group_1", Kid::Jim(), 1.0, 1},
                {"group_2", Kid::Jim(), 2.0, 2},
                {"group_3", Kid::Jim(), 3.0, 3},
                {"group_4", Kid::Jim(), 4.0, 4}};
  c.recover(0, 4, [&](const Groups &r) { EXPECT_EQ(r, ros); });

  c.eidv(CustomIDEnum::Green,
         [](const CustomIDEnum &x) { EXPECT_EQ(x, CustomIDEnum::Green); });

  c.inverse(RGB{{0, 255, 0}}, [](const RGB &inv) {
    EXPECT_EQ(std::vector<int>(inv.val, inv.val + 3),
              std::vector<int>({255, 0, 255}));
  });

  // run a quick epoll loop to process results asynchronously (expect all
  // results within 30 seconds)
  registerEventHandler(c.fd(), &stepAsyncClient, &c);
  for (size_t s = 0; s < 30 && c.pendingRequests() > 0; ++s) {
    runEventLoop(1000 * 1000);
  }
  EXPECT_EQ(c.pendingRequests(), size_t(0));
}

namespace {
// a pipe whose reads cannot block, so a handler run on the wrong descriptor
// fails its read instead of hanging the suite
int nbpipe(int p[2]) {
  if (pipe(p) != 0) return -1;
  for (int fd : {p[0], p[1]}) {
    if (fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK) != 0) return -1;
  }
  return 0;
}
}

TEST(Net, eventHandlerRegistrationTracksTheDescriptor) {
  // a descriptor the kernel refuses must not leave a handler registered under
  // its number, so unregistering that number afterwards has nothing to do
  runEventLoop(1000); // so the poll set exists, and does not take the number below
  int p[2];
  EXPECT_EQ(nbpipe(p), 0);
  close(p[0]);
  close(p[1]);
  int dead = p[0];
  EXPECT_EXCEPTION(registerEventHandler(dead, [](int) {}));
  unregisterEventHandler(dead);
  unregisterEventHandler(dead);

  // when the number comes back into use, the handler registered for the new
  // descriptor is the one that runs
  EXPECT_EQ(nbpipe(p), 0);
  std::atomic<int> fired{0};
  registerEventHandler(p[0], [&fired](int fd) {
    char b;
    if (read(fd, &b, 1) == 1) {
      ++fired;
    }
  });
  EXPECT_EQ(write(p[1], "x", 1), ssize_t(1));
  for (size_t s = 0; s < 30 && fired == 0; ++s) {
    runEventLoop(100 * 1000);
  }
  EXPECT_EQ(fired.load(), 1);

  // unregistering removes the handler outright, so doing it again is harmless
  // (this used to delete the same closure twice)
  unregisterEventHandler(p[0]);
  unregisterEventHandler(p[0]);
  close(p[0]);
  close(p[1]);
}

namespace {
// two pipes, each with a byte waiting, so that both handlers are delivered in
// one batch; whichever runs first does 'retire' to the other's pipe. The
// second handler of the batch must then not run: its closure has been retired
// (and freed, before this was fixed -- the loop called through freed memory)
void retireTheOtherMidBatch(const std::function<void(int* other)>& retire, std::atomic<int>* replacementRan) {
  int a[2], b[2];
  EXPECT_EQ(nbpipe(a), 0);
  EXPECT_EQ(nbpipe(b), 0);
  int* pipes[2] = {a, b};

  std::atomic<int> ran{0};
  int first = -1;
  auto handler = [&](int me) {
    char c;
    EXPECT_EQ(read(me, &c, 1), ssize_t(1));
    first = me;
    ++ran;
    retire((me == a[0]) ? b : a);
  };
  for (int* p : pipes) {
    registerEventHandler(p[0], handler);
    EXPECT_EQ(write(p[1], "x", 1), ssize_t(1));
  }

  for (size_t s = 0; s < 30 && (ran == 0 || (replacementRan != nullptr && *replacementRan == 0)); ++s) {
    runEventLoop(100 * 1000);
  }
  EXPECT_EQ(ran.load(), 1);

  int* survivor = (first == a[0]) ? a : b;
  unregisterEventHandler(survivor[0]);
  close(survivor[0]);
  close(survivor[1]);
}
}

TEST(Net, handlersRetiredMidBatchAreNotDispatched) {
  // a handler unregisters the other descriptor of its batch
  retireTheOtherMidBatch([](int* other) {
    unregisterEventHandler(other[0]);
    close(other[0]);
    close(other[1]);
  }, nullptr);

  // a handler replaces the other descriptor of its batch with a fresh one
  // under the same number (the old one is closed without being unregistered)
  // and registers that: the stale closure under the number is retired, and it
  // is the new handler that runs, on the next batch
  std::atomic<int> replacementRan{0};
  int replacement[2] = {-1, -1};
  retireTheOtherMidBatch([&](int* other) {
    EXPECT_EQ(nbpipe(replacement), 0);
    EXPECT_EQ(dup2(replacement[0], other[0]), other[0]); // closes the other's, and takes its number whatever else is open
    close(replacement[0]);
    replacement[0] = other[0];
    close(other[1]);
    registerEventHandler(replacement[0], [&replacementRan](int fd) {
      char d;
      if (read(fd, &d, 1) == 1) {
        ++replacementRan;
      }
    });
    EXPECT_EQ(write(replacement[1], "y", 1), ssize_t(1));
  }, &replacementRan);
  EXPECT_EQ(replacementRan.load(), 1);
  unregisterEventHandler(replacement[0]);
  close(replacement[0]);
  close(replacement[1]);
}

TEST(Net, staleInterestFromADuplicatedDescriptorIsNotDispatched) {
  // a registered descriptor is closed (not unregistered) while a duplicate of
  // it stays open, so the kernel keeps its interest, and its number is reused
  // for another descriptor that is then registered: the old interest still
  // delivers the old closure (which the loop used to have freed by then)
  int p[2];
  EXPECT_EQ(nbpipe(p), 0);
  std::atomic<int> staleRan{0};
  registerEventHandler(p[0], [&staleRan](int fd) {
    char b;
    if (read(fd, &b, 1) == 1) {
      ++staleRan;
    }
  });
  int dup0 = dup(p[0]);
  EXPECT_TRUE(dup0 >= 0);

  int q[2];
  EXPECT_EQ(nbpipe(q), 0);
  EXPECT_EQ(dup2(q[0], p[0]), p[0]); // p[0] is now the number of q's read end
  close(q[0]);
  std::atomic<int> freshRan{0};
  registerEventHandler(p[0], [&freshRan](int fd) {
    char b;
    if (read(fd, &b, 1) == 1) {
      ++freshRan;
    }
  });

  // a byte on the old pipe is reported through the old interest
  EXPECT_EQ(write(p[1], "x", 1), ssize_t(1));
  for (size_t s = 0; s < 3; ++s) {
    runEventLoop(100 * 1000);
  }
  EXPECT_EQ(staleRan.load(), 0);
  EXPECT_EQ(freshRan.load(), 0);

  // and one on the new pipe reaches its handler
  EXPECT_EQ(write(q[1], "y", 1), ssize_t(1));
  for (size_t s = 0; s < 30 && freshRan == 0; ++s) {
    runEventLoop(100 * 1000);
  }
  EXPECT_EQ(freshRan.load(), 1);

  unregisterEventHandler(p[0]);
  close(p[0]);
  close(p[1]);
  close(q[1]);
  close(dup0);

  // the same when the number is unregistered after it was closed: the delete
  // has nothing to remove, the interest is still live through the duplicate
  EXPECT_EQ(nbpipe(p), 0);
  std::atomic<int> lateRan{0};
  registerEventHandler(p[0], [&lateRan](int fd) {
    char b;
    if (read(fd, &b, 1) == 1) {
      ++lateRan;
    }
  });
  dup0 = dup(p[0]);
  close(p[0]);
  unregisterEventHandler(p[0]);
  EXPECT_EQ(write(p[1], "x", 1), ssize_t(1));
  for (size_t s = 0; s < 3; ++s) {
    runEventLoop(100 * 1000);
  }
  EXPECT_EQ(lateRan.load(), 0);
  close(p[1]);
  close(dup0);
}

TEST(Net, aHandlerMayReplaceItsOwnRegistration) {
  // a handler replaces its own descriptor with another under the same number
  // and registers that: the closure it is running from is the one displaced,
  // and its captures must outlive the handler (they were released under it once)
  int p[2];
  EXPECT_EQ(nbpipe(p), 0);
  std::atomic<int> ran{0};
  std::atomic<int> newRan{0};
  int q[2] = {-1, -1};
  registerEventHandler(p[0], [&, p](int fd) {
    char b;
    EXPECT_EQ(read(fd, &b, 1), ssize_t(1));
    EXPECT_EQ(nbpipe(q), 0);
    EXPECT_EQ(dup2(q[0], p[0]), p[0]); // closes this handler's descriptor and reuses its number
    close(q[0]);
    q[0] = p[0];
    registerEventHandler(q[0], [&newRan](int nfd) {
      char c;
      if (read(nfd, &c, 1) == 1) {
        ++newRan;
      }
    });
    ++ran; // a capture, after this closure was displaced
  });
  EXPECT_EQ(write(p[1], "x", 1), ssize_t(1));
  for (size_t s = 0; s < 30 && ran == 0; ++s) {
    runEventLoop(100 * 1000);
  }
  EXPECT_EQ(ran.load(), 1);

  EXPECT_EQ(write(q[1], "y", 1), ssize_t(1));
  for (size_t s = 0; s < 30 && newRan == 0; ++s) {
    runEventLoop(100 * 1000);
  }
  EXPECT_EQ(newRan.load(), 1);
  unregisterEventHandler(q[0]);
  close(q[0]);
  close(q[1]);
  close(p[1]);
}

namespace {
template <typename EventLoopFn, typename ExpectPred>
void eventLoopShutdownWithStopFImpl(EventLoopFn elFn, ExpectPred expectPred) {
  struct {
    std::atomic_bool flag{false};
  } stopper;
  std::function<bool()> f = [&stopper]() -> bool { return stopper.flag; };

  using namespace std::chrono_literals;
  using Clock = std::chrono::steady_clock;

  auto t = std::thread([&stopper] {
    std::this_thread::sleep_for(2s);
    stopper.flag = true;
  });

  long long millisecs = 0;
  try {
    const auto startTime = Clock::now();
    elFn(f);
    const auto endTime = Clock::now();
    millisecs =
        std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime)
            .count();
  } catch (...) {
    stopper.flag = true;
    t.join();
    throw;
  }
  t.join();
  EXPECT_TRUE(expectPred(millisecs));
}
} // namespace

TEST(Net, eventLoopShutdownWithStopF) {
  // eventLoopShutdownWithStopFImpl sets up a stopFn that returns true after 2000 ms

  // a repeatable timer triggering every 700ms is added to check stopFn periodically,
  // so the loop notices the flag on the first tick after 2000ms plus scheduling
  // overhead; macOS CI runners consistently land around ~2600ms, hence the 3000ms
  // upper bound (do not tighten it back to 2500ms)
  eventLoopShutdownWithStopFImpl(
      [](const std::function<bool()> &stopFn) {
        hobbes::addTimer([] { return true; }, 700);
        hobbes::runEventLoop(stopFn);
      },
      [](auto millisecs) { return millisecs > 1'500 && millisecs < 3'000; });

  // a timeout at 500ms is set for this "one shot" event loop
  // event loop should stop after ~500 ms
  eventLoopShutdownWithStopFImpl(
      [](const std::function<bool()> &stopFn) {
        hobbes::runEventLoop(500'000, stopFn);
      },
      [](auto millisecs) { return millisecs < 1'000; });
}

TEST(Net, clientReadBridgesAreNotUserNameable) {
  // the raw-pointer Client bridges are for generated code only, so their
  // plain names must not resolve in a user expression
  EXPECT_EXCEPTION_MSG(c().compileFn<void()>("unsafeClientRead(1L, 0L)"),
                       std::exception, "Undefined variable");
  EXPECT_EXCEPTION_MSG(
      c().compileFn<void()>("unsafeAppendClientReadFn(1L, 0L)"), std::exception,
      "Undefined variable");
}

TEST(Net, clientReadBridgesRejectForgedHandles) {
  // a handle that names no live connection must be rejected rather than
  // reinterpret_cast and dereferenced (the set lookup never loads from it)
  const auto forged = static_cast<size_t>(0x1234);
  EXPECT_EXCEPTION(Client::unsafeRead(forged, 0));
  EXPECT_EXCEPTION(Client::unsafeAppendReadFn(forged, nullptr));
}

TEST(Net, clientReadBridgesAreDeniedInSafeMode) {
  // the dot-prefixed names cannot be parsed from source text, but a
  // serialized AST can still carry a Var naming one of them, so Safe mode
  // must reject them too
  auto la = LexicalAnnotation::null();
  for (const auto &n :
       {".unsafeClientRead", ".unsafeAppendClientReadFn", ".printConnection"}) {
    EXPECT_EXCEPTION(
        translateExprWithOpts(str::strings("Safe"), var(n, la)));
  }

  // a name that is not a raw-pointer bridge still passes the Safe rewrite
  EXPECT_TRUE(translateExprWithOpts(str::strings("Safe"),
                                    var("remoteHost", la)) != nullptr);
}

TEST(Net, connectSocketReportsAResolutionFailure) {
  // a host that does not resolve used to hand a null hostent straight to
  // '*(in_addr*)host->h_addr_list[0]', which faulted the process. Every
  // dotted-quad took that same path, because the digit case called
  // gethostbyaddr with the address as text where it expects four bytes, so
  // it never matched and returned null too. Resolution failure must be an
  // error the caller can catch.
  //
  // ".invalid" is reserved for exactly this (RFC 2606): it never resolves.
  EXPECT_EXCEPTION(connectSocket("nosuchhost.invalid", 1));

  // a dotted-quad resolves and connects like any other spelling of the host
  int fd = connectSocket("127.0.0.1", testServerPort());
  EXPECT_TRUE(fd >= 0);
  ::close(fd);
}

TEST(Net, aForgedConnectionHandleIsNotDereferenced) {
  // the handle in a 'connection N' type is the number N, and an expression
  // can write any number there -- nothing about the type says it came from
  // makeConnection. Uses that dereference it ran while the expression was
  // compiled, so this faulted the process during a net REPL 'prepare' or a
  // ':t', before any decision to evaluate it.
  cc c;
  EXPECT_EXCEPTION(c.compileFn<int()>("let f = (\\x.remoteHost(x::((connection 1094795585)))) in 1"));
  EXPECT_EXCEPTION(c.compileFn<int()>("let f = (\\x.printConnection(x::((connection 1094795585)))) in 1"));

  // and the compiler is still usable afterwards
  EXPECT_EQ(c.compileFn<int()>("1+1")(), 2);
}

TEST(Net, aVariantTagFromTheWireIsChecked) {
  // a variant's tag selects which function handles its payload, out of a
  // table with one entry per constructor. The tag in a reply is whatever the
  // peer sent, and it was used to index that table directly, so a tag past
  // the end read a function pointer from beyond the table and called it.
  using V = variant<int, double>;

  int fds[2] = {-1, -1};
  EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);

  // a tag naming no constructor of this variant, with a payload behind it
  fdwrite(fds[1], static_cast<uint32_t>(7));
  fdwrite(fds[1], static_cast<int>(42));

  V got;
  EXPECT_EXCEPTION(net::io<V>::read(fds[0], &got));
  ::close(fds[0]);
  ::close(fds[1]);

  // ... while a tag that does name one still reads (on its own pair: a
  // rejected tag leaves the payload behind it unread, so that stream is done)
  EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);
  fdwrite(fds[1], static_cast<uint32_t>(0));
  fdwrite(fds[1], static_cast<int>(7));
  net::io<V>::read(fds[0], &got);
  EXPECT_EQ(got.unsafeTag(), uint32_t(0));
  ::close(fds[0]);
  ::close(fds[1]);
}

TEST(Net, aClaimedLengthDoesNotSizeTheReadBuffer) {
  // the length of a string or vector arrives before the data behind it, so a
  // peer can claim a size it will never send. Sizing the target from the
  // claim alone let a couple of dozen bytes name any amount of memory; the
  // buffer now grows as the data arrives, so a claim the peer cannot back
  // fails on the read that runs out.
  int fds[2] = {-1, -1};
  EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);

  // a claim of 1GiB with nothing behind it, and the writer gone
  fdwrite(fds[1], static_cast<size_t>(1) << 30);
  ::close(fds[1]);

  std::string s;
  EXPECT_EXCEPTION(fdread(fds[0], &s));
  ::close(fds[0]);

  // an ordinary string of each shape still round-trips, including one larger
  // than a single chunk of the incremental read (the writer runs on its own
  // thread: more than the socket buffer holds is in flight at once)
  EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);
  std::string sent(128 * 1024, 'x');
  std::thread w([&]() {
    fdwrite(fds[1], sent);
    fdwrite(fds[1], std::string());
  });

  std::string got;
  fdread(fds[0], &got);
  EXPECT_TRUE(got == sent);
  fdread(fds[0], &got);
  EXPECT_TRUE(got.empty());

  w.join();
  ::close(fds[0]);
  ::close(fds[1]);
}

TEST(Net, aPartialHandshakeDoesNotStallTheServer) {
  // the version word used to be read in the accept handler with a blocking
  // read, on the shared event loop: a peer that connected and sent fewer than
  // four bytes stopped that loop until it went away, and every other listener
  // in the process waited with it. Accepting a connection says nothing about
  // the peer having sent anything yet.
  int fd = connectSocket("localhost", testServerPort());
  uint16_t half = 0;
  EXPECT_EQ(::send(fd, &half, sizeof(half), 0), ssize_t(sizeof(half)));

  // the server keeps serving everyone else while that peer says nothing more
  {
    SyncClient c("localhost", testServerPort());
    EXPECT_EQ(c.add(1, 2), 3);
  }

  // and the half-spoken peer is still connected, not dropped: a read of its
  // socket times out rather than seeing the orderly EOF a rejected peer gets
  struct timeval tv;
  tv.tv_sec  = 1;
  tv.tv_usec = 0;
  ::setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
  char b = 0;
  EXPECT_EQ(::recv(fd, &b, 1, 0), ssize_t(-1));

  // when the rest of its version word arrives, it is served like any other
  uint16_t rest = 0x0001; // completing 00 00 01 00
  EXPECT_EQ(::send(fd, &rest, sizeof(rest), 0), ssize_t(sizeof(rest)));

  {
    SyncClient c("localhost", testServerPort());
    EXPECT_EQ(c.add(2, 3), 5);
  }
  ::close(fd);
}
// Resolving a (Connect "host:port" c) constraint opens a live outbound
// connection, and an (Invoke ...) constraint ships code to the peer and
// executes it there, both as a side effect of type-constraint resolution --
// not evaluation. Merely type-checking untrusted text carrying such
// annotations used to reach out over the network and run code on a remote
// peer before any decision to evaluate anything. Both are denied unless the
// embedding cc opts in with an exact-match host:port allowlist, and the two
// are independent: allowing a connection does not imply trusting that peer
// to run code.
static std::string testServerHostPort() {
  // the allowlist matches the literal string in the constraint, so the
  // spelling here has to be the one the tests write; "localhost" is what
  // the rest of this file uses
  return "localhost:" + hobbes::str::from(testServerPort());
}

TEST(Net, connectConstraintDeniedByDefault) {
  hobbes::cc client;
  bool threw = false;
  try {
    client.compileFn<void()>("let x = (connection :: (Connect \"" + testServerHostPort() + "\" p) => p) in ()");
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("Connect constraint rejected") != std::string::npos);
  }
  EXPECT_TRUE(threw);
}

TEST(Net, connectConstraintAllowedWithExactAllowlist) {
  hobbes::cc client;
  client.enableRemoteConnections({testServerHostPort()});
  // must not throw: the exact allowlisted target is permitted to connect
  client.compileFn<void()>("let x = (connection :: (Connect \"" + testServerHostPort() + "\" p) => p) in ()")();
}

TEST(Net, invokeConstraintDeniedByDefaultEvenWhenConnectingIsAllowed) {
  hobbes::cc client;
  client.enableRemoteConnections({testServerHostPort()});
  bool threw = false;
  try {
    client.compileFn<void()>(
      "let c = (connection :: (Connect \"" + testServerHostPort() + "\" p) => p) in "
      "let x = invoke(c, `1+1`, ()) in ()"
    );
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("Invoke constraint rejected") != std::string::npos);
  }
  EXPECT_TRUE(threw);
}

// The remaining case -- both gates enabled, the full connect+invoke round
// trip succeeding -- is not automated here.
//
// Against this file's in-process server it deadlocks on the compiler lock:
// the test thread holds hccmtx (cc.C) for the whole of cc::compileFn while
// it blocks inside Client::remoteExpr, and the server thread needs that same
// lock to answer, since CCServer::prepare compiles the expression it was
// sent (net.C). Nothing here can drive a connect+invoke from a thread that
// is holding the compiler lock, which is what compiling the constraint does.
//
// Pointing it at a separate `hi -p` child instead does not finish either --
// left running for over six minutes -- and that one is not diagnosed. Both
// are properties of driving this from inside the test process, not of the
// gate: the denial paths above cover the gate, and the allowed path is
// reproducible directly against the binaries, where hi enables all three
// gates for a local session:
//
//   $ (sleep 600 | ./hi -s -p 9601) &          # a peer; stdin must be a pipe,
//                                              # hi registers it with epoll
//   $ ./hi -s -x -o no-Safe -e 'let c = (connection ::
//       (Connect "localhost:9601" p) => p) in
//       print(receive(invoke(c, `(\x.x+1)`, 41)))'
//   42
//
// -o no-Safe is required independently of these gates, because invoke's
// generated code names unsafeCast, which Safe denies.
