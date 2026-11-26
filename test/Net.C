
#include "test.H"
#include <hobbes/eval/cmodule.H>
#include <hobbes/hobbes.H>
#include <hobbes/ipc/net.H>
#include <hobbes/net.H>
#include <hobbes/util/codec.H>

#include <atomic>
#include <cerrno>
#include <chrono>
#include <condition_variable>
#include <cstdlib>
#include <functional>
#include <memory>
#include <mutex>
#include <thread>

#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <sys/socket.h>
#include <unistd.h>

using namespace hobbes;

// Forward declaration so the atexit cleanup can reach all server thread sets.
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

static const std::string authenticatedServerSecret =
    "test-only-netrepl-shared-secret";
static int authenticatedServerPort = -1;
static bool authenticatedServerReady = false;
static std::mutex authenticatedServerMtx;
static std::condition_variable authenticatedServerStartup;
static std::atomic<bool> authenticatedServerStop{false};
static std::thread authenticatedServerThread;

// listeners that authenticate their peers with a custom NetREPLAuthenticator
// (defined with the tests that use them)
static std::atomic<bool> customAuthServerStop{false};
static std::thread customAuthServerThread;

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

static void runAuthenticatedTestServer(int ps, int pe) {
  std::unique_lock<std::mutex> lk(authenticatedServerMtx);
  authenticatedServerPort = ps;
  while (authenticatedServerPort < pe) {
    try {
      installNetREPL(authenticatedServerPort, &c(),
                     authenticatedServerSecret);
      authenticatedServerReady = true;
      lk.unlock();
      authenticatedServerStartup.notify_one();
      runEventLoop([&]{ return authenticatedServerStop.load(); });
      return;
    } catch (std::exception &) {
      ++authenticatedServerPort;
    }
  }
  authenticatedServerPort = -1;
  authenticatedServerReady = true;
  lk.unlock();
  authenticatedServerStartup.notify_one();
}

int testAuthenticatedServerPort() {
  if (authenticatedServerPort < 0) {
    if (authenticatedServerThread.joinable()) {
      authenticatedServerThread.join();
    }
    std::unique_lock<std::mutex> lk(authenticatedServerMtx);
    authenticatedServerReady = false;
    authenticatedServerThread = std::thread(
        [] { return runAuthenticatedTestServer(10501, 11500); });
    authenticatedServerStartup.wait(lk, []{ return authenticatedServerReady; });
    if (authenticatedServerPort < 0) {
      throw std::runtime_error(
          "Couldn't allocate port for authenticated test server");
    }
  }
  return authenticatedServerPort;
}

static void joinServerThreadsAtExit() {
  serverStop = true;
  serverWithHostStop = true;
  authenticatedServerStop = true;
  customAuthServerStop = true;
  if (customAuthServerThread.joinable()) {
    customAuthServerThread.join();
  }
  if (serverThread.joinable()) {
    serverThread.join();
  }
  if (serverWithHostThread.joinable()) {
    serverWithHostThread.join();
  }
  if (authenticatedServerThread.joinable()) {
    authenticatedServerThread.join();
  }
}

namespace {

class NullNetREPLServer : public Server {
public:
  void connect(int) override {}
  ExprPtr readExpr(const std::string &) override { return ExprPtr(); }
  MonoTypePtr prepare(int, exprid, const ExprPtr &,
                      const MonoTypePtr &) override {
    return MonoTypePtr();
  }
  void evaluate(int, exprid) override {}
  void disconnect(int) override {}
};

class RegisteredListener {
public:
  explicit RegisteredListener(int fd) : fd(fd) {}
  ~RegisteredListener() {
    unregisterEventHandler(fd);
    close(fd);
  }

  int get() const { return fd; }

private:
  int fd;
};

uint32_t boundIPv4Address(int fd) {
  sockaddr_in address = {};
  socklen_t addressSize = sizeof(address);
  if (getsockname(fd, reinterpret_cast<sockaddr *>(&address), &addressSize) !=
      0) {
    throw std::runtime_error("Unable to inspect netREPL listening address");
  }
  if (address.sin_family != AF_INET) {
    throw std::runtime_error("netREPL listener did not use IPv4");
  }
  return ntohl(address.sin_addr.s_addr);
}

} // namespace

TEST(Net, netREPLBindsToLoopbackByDefault) {
  NullNetREPLServer server;

  RegisteredListener defaultListener(installNetREPL(0, &server));
  EXPECT_EQ(boundIPv4Address(defaultListener.get()),
            static_cast<uint32_t>(INADDR_LOOPBACK));

  RegisteredListener explicitListener(
      installNetREPL("0.0.0.0", 0, &server));
  EXPECT_EQ(boundIPv4Address(explicitListener.get()),
            static_cast<uint32_t>(INADDR_ANY));
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

// a raw-socket read in these tests must fail rather than hang the suite if
// the server ever stops answering or dropping the peer
static int withReceiveTimeout(int fd) {
  struct timeval tv;
  tv.tv_sec  = 10;
  tv.tv_usec = 0;
  ::setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
  return fd;
}

TEST(Net, rejectedHandshakeLeavesTheServerServing) {
  // a peer that opens with a protocol version the server does not speak is
  // dropped at the handshake. The drop used to fall through to register an
  // event handler on the socket it had just closed; that registration failed,
  // and the failure path closed the socket a second time -- by then possibly
  // some other thread's. The peer should simply see its connection closed,
  // and the next client should be served as usual.
  int fd = withReceiveTimeout(connectSocket("localhost", testServerPort()));
  fdwrite(fd, static_cast<uint32_t>(0xdeadbeef));
  char b = 0;
  EXPECT_EQ(::recv(fd, &b, 1, 0), ssize_t(0)); // orderly EOF: the server hung up
  ::close(fd);

  SyncClient c("localhost", testServerPort());
  EXPECT_EQ(c.add(1, 2), 3);
}

TEST(Net, authenticatedNetREPLRejectsMissingAndWrongSecrets) {
  const int port = testAuthenticatedServerPort();

  int legacy = withReceiveTimeout(connectSocket("localhost", port));
  fdwrite(legacy, static_cast<uint32_t>(0x00010000));
  char b = 0;
  EXPECT_EQ(::recv(legacy, &b, 1, 0), ssize_t(0));
  ::close(legacy);

  // a wrong secret is answered with a rejection byte, then the hang-up
  int wrong = withReceiveTimeout(connectSocket("localhost", port));
  fdwrite(wrong, static_cast<uint32_t>(0x00010001));
  fdwrite(wrong, std::string("wrong-secret"));
  b = 1;
  EXPECT_EQ(::recv(wrong, &b, 1, 0), ssize_t(1));
  EXPECT_EQ(static_cast<int>(b), 0);
  EXPECT_EQ(::recv(wrong, &b, 1, 0), ssize_t(0));
  ::close(wrong);

  int oversized = withReceiveTimeout(connectSocket("localhost", port));
  fdwrite(oversized, static_cast<uint32_t>(0x00010001));
  fdwrite(oversized, static_cast<size_t>(4097));
  EXPECT_EQ(::recv(oversized, &b, 1, 0), ssize_t(0));
  ::close(oversized);

  SyncClient authorized(
      "localhost", static_cast<size_t>(port),
      net::NetREPLSharedSecret(authenticatedServerSecret));
  EXPECT_EQ(authorized.add(20, 22), 42);
}

static std::string testServerHostPort();
static std::string testAuthenticatedServerHostPort();

TEST(Net, authenticatedClientsReportARejectedSecret) {
  // without an answer from the server a wrong secret looked like a working
  // session until the first request hit the closed connection -- an EOF, or a
  // SIGPIPE that could take an embedding process down
  const int port = testAuthenticatedServerPort();
  const std::string hostPort = testAuthenticatedServerHostPort();

  bool syncClientThrew = false;
  try {
    SyncClient wrong("localhost", static_cast<size_t>(port),
                     net::NetREPLSharedSecret("wrong-secret"));
  } catch (std::exception& ex) {
    syncClientThrew = std::string(ex.what()).find("authentication rejected") != std::string::npos;
  }
  EXPECT_TRUE(syncClientThrew);

  bool clientThrew = false;
  try {
    hobbes::Client wrong(hostPort, "wrong-secret");
  } catch (std::exception& ex) {
    clientThrew = std::string(ex.what()).find("authentication rejected") != std::string::npos;
  }
  EXPECT_TRUE(clientThrew);

  // a listener without a secret does not speak the authenticated protocol
  bool legacyThrew = false;
  try {
    hobbes::Client toLegacy(testServerHostPort(), authenticatedServerSecret);
  } catch (std::exception& ex) {
    legacyThrew = std::string(ex.what()).find("authentication rejected") != std::string::npos;
  }
  EXPECT_TRUE(legacyThrew);

  // the right secret is accepted through the same constructor
  hobbes::Client right(hostPort, authenticatedServerSecret);
  EXPECT_EQ(right.remoteHost(), hostPort);

  // and a Connect constraint with a wrong configured secret fails to compile
  hobbes::cc client;
  client.enableRemoteConnections({hostPort});
  client.setRemoteConnectionSharedSecret(hostPort, "wrong-secret");
  EXPECT_EXCEPTION(client.compileFn<void()>(
      "let x = (connection :: (Connect \"" + hostPort + "\" p) => p) in ()"));
}

// A listener that refuses the authenticated handshake (a legacy one sees the
// version word it does not speak) hangs up while the client may still be
// sending the secret. The client wrote the handshake with plain write()s, and
// Linux has no SO_NOSIGPIPE, so a client that lost that race would be killed
// by SIGPIPE rather than see the rejection. Here the peer resets each
// connection as soon as the version word arrives; every attempt must come
// back as an ordinary rejection. (The race was narrow -- the client usually
// finished writing before the peer read -- and the handshake now goes out in
// one send that fits the socket buffer, so this checks how a reset peer is
// reported rather than reproducing the old failure.)
TEST(Net, authenticatedClientReportsAPeerThatResetsDuringTheHandshake) {
  int listener = ::socket(AF_INET, SOCK_STREAM, 0);
  EXPECT_TRUE(listener >= 0);
  sockaddr_in addr = {};
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  addr.sin_port = 0;
  EXPECT_EQ(::bind(listener, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)), 0);
  EXPECT_EQ(::listen(listener, 16), 0);
  socklen_t addrLen = sizeof(addr);
  EXPECT_EQ(::getsockname(listener, reinterpret_cast<sockaddr*>(&addr), &addrLen), 0);
  const std::string hostPort = "127.0.0.1:" + str::from(ntohs(addr.sin_port));

  // accept() waits at most this long for each client, so the peer cannot block
  // forever on one that never connects
  timeval acceptTimeout = {10, 0};
  EXPECT_EQ(::setsockopt(listener, SOL_SOCKET, SO_RCVTIMEO, &acceptTimeout, sizeof(acceptTimeout)), 0);

  const int attempts = 50;
  std::thread peer([listener] {
    for (int i = 0; i < attempts; ++i) {
      int c = ::accept(listener, nullptr, nullptr);
      if (c < 0) {
        if (errno == EINTR) {
          --i;
          continue;
        }
        // anything else, including the listener's receive timeout running out
        // because a client never arrived, ends the peer; closing the listener
        // refuses later clients rather than leaving them waiting on a status
        // byte no one will send
        break;
      }
      uint32_t version = 0;
      ssize_t ignored = ::recv(c, &version, sizeof(version), MSG_WAITALL);
      (void)ignored;
      linger reset = {1, 0}; // close with a reset, as a peer that is gone would
      ::setsockopt(c, SOL_SOCKET, SO_LINGER, &reset, sizeof(reset));
      ::close(c);
    }
    ::close(listener);
  });

  int rejected = 0;
  for (int i = 0; i < attempts; ++i) {
    try {
      hobbes::Client client(hostPort, authenticatedServerSecret);
    } catch (std::exception& ex) {
      if (std::string(ex.what()).find("authentication rejected") != std::string::npos) {
        ++rejected;
      }
    }
  }
  peer.join();
  EXPECT_EQ(rejected, attempts);
}

TEST(Net, authenticatedHandshakeDeliveredOneByteAtATime) {
  const int port = testAuthenticatedServerPort();
  int fd = withReceiveTimeout(connectSocket("localhost", port));

  std::string handshake;
  const uint32_t version = 0x00010001;
  const size_t n = authenticatedServerSecret.size();
  handshake.append(reinterpret_cast<const char*>(&version), sizeof(version));
  handshake.append(reinterpret_cast<const char*>(&n), sizeof(n));
  handshake.append(authenticatedServerSecret);
  for (char ch : handshake) {
    EXPECT_EQ(::send(fd, &ch, 1, 0), ssize_t(1));
    std::this_thread::sleep_for(std::chrono::milliseconds(2));
  }

  char b = 0;
  EXPECT_EQ(::recv(fd, &b, 1, 0), ssize_t(1));
  EXPECT_EQ(static_cast<int>(b), 1);
  ::close(fd);
}

TEST(Net, partialAuthenticatedHandshakeDoesNotStallTheServer) {
  const int port = testAuthenticatedServerPort();
  int stalled = connectSocket("localhost", port);
  fdwrite(stalled, static_cast<uint32_t>(0x00010001));
  fdwrite(stalled, authenticatedServerSecret.size());
  fdwrite(stalled, authenticatedServerSecret.data(), 1);

  SyncClient authorized(
      "localhost", static_cast<size_t>(port),
      net::NetREPLSharedSecret(authenticatedServerSecret));
  EXPECT_EQ(authorized.add(19, 23), 42);
  ::close(stalled);
}

// A listener can authenticate its peers with a handshake the embedding
// application supplies (NetREPLAuthenticator), and a client answers with the
// matching NetREPLClientAuthenticator. The mechanism here is a toy
// challenge-response with the shape of a real one: the client names a user,
// the server sends a challenge, the client answers it with that user's key,
// and the server's acceptance carries a proof of its own for the client to
// check. Two rounds, tokens in both directions, an identity at the end.
namespace {

const std::string testAuthMechanism = "test-challenge";
const std::string testAuthUser      = "alice";
const std::string testAuthKey       = "alice-test-only-key";

class TestChallengeAuthenticator : public NetREPLAuthenticator {
public:
  std::string mechanism() const override { return testAuthMechanism; }
  size_t maxTokenBytes() const override { return 256; }

  std::unique_ptr<NetREPLAuthSession> newSession(int) override {
    return std::unique_ptr<NetREPLAuthSession>(new Session(++this->sessions));
  }

private:
  class Session : public NetREPLAuthSession {
  public:
    explicit Session(int n) : challenge("challenge-" + str::from(n)) {}

    Result step(const std::string& token, std::string* reply) override {
      if (this->user.empty()) {
        if (token == "throw") {
          throw std::runtime_error("this session cannot go on");
        }
        if (token != testAuthUser && token != "mallory" && token != "loop") {
          return Result::Reject; // no such user
        }
        this->user = token;
        *reply = this->challenge;
        return Result::Continue;
      }
      if (this->user == "loop") {
        *reply = this->challenge;
        return Result::Continue; // never settles
      }
      const std::string key = this->user == testAuthUser ? testAuthKey : "mallory-test-only-key";
      if (token != this->challenge + ":" + key) {
        return Result::Reject;
      }
      this->proven = true;
      *reply = "server-proof:" + key;
      return Result::Accept;
    }

    std::string peerIdentity() const override {
      return this->proven ? this->user : std::string();
    }

  private:
    std::string challenge;
    std::string user;
    bool        proven = false;
  };

  int sessions = 0; // only the listener's event loop makes sessions
};

class TestChallengeClient : public net::NetREPLClientAuthenticator {
public:
  TestChallengeClient(const std::string& user, const std::string& key,
                      const std::string& mechanism = testAuthMechanism)
      : user(user), key(key), mech(mechanism) {}

  std::string mechanism() const override { return this->mech; }

  std::unique_ptr<net::NetREPLClientAuthSession> begin() const override {
    return std::unique_ptr<net::NetREPLClientAuthSession>(new Session(*this));
  }

  // how many sessions checked the server's proof and were satisfied
  int serversVerified() const { return this->verified.load(); }

private:
  class Session : public net::NetREPLClientAuthSession {
  public:
    explicit Session(const TestChallengeClient& auth) : auth(auth) {}

    std::string next(const std::string& serverToken) override {
      if (!this->named) {
        this->named = true;
        return this->auth.user;
      }
      return serverToken + ":" + this->auth.key;
    }

    void accepted(const std::string& finalToken) override {
      if (finalToken != "server-proof:" + this->auth.key) {
        throw std::runtime_error("the server did not prove itself");
      }
      ++this->auth.verified;
    }

  private:
    const TestChallengeClient& auth;
    bool named = false;
  };

  std::string user;
  std::string key;
  std::string mech;
  mutable std::atomic<int> verified{0};
};

// a Server that notes what the listener tells it about each connection, and
// turns one authenticated peer away
class RecordingNetREPLServer : public NullNetREPLServer {
public:
  void authenticated(int, const std::string& mechanism, const std::string& identity) override {
    record("authenticated " + mechanism + " " + identity);
    if (identity == "mallory") {
      throw std::runtime_error("mallory is not welcome here");
    }
  }
  void connect(int) override { record("connect"); }
  void disconnect(int) override { record("disconnect"); }

  // the listener calls in from its own thread, some time after the client has
  // its answer: wait for the calls a test expects
  std::vector<std::string> eventsOnceThereAre(size_t n) {
    std::unique_lock<std::mutex> lk(this->mtx);
    this->changed.wait_for(lk, std::chrono::seconds(10), [&] { return this->events.size() >= n; });
    return this->events;
  }
  void clear() {
    std::lock_guard<std::mutex> lk(this->mtx);
    this->events.clear();
  }

private:
  void record(const std::string& e) {
    {
      std::lock_guard<std::mutex> lk(this->mtx);
      this->events.push_back(e);
    }
    this->changed.notify_all();
  }

  std::mutex               mtx;
  std::condition_variable  changed;
  std::vector<std::string> events;
};

RecordingNetREPLServer& recordingServer() {
  static RecordingNetREPLServer x;
  return x;
}

int customAuthServerPort = -1;    // a cc behind the test mechanism
int recordingAuthServerPort = -1; // recordingServer() behind the same mechanism
bool customAuthServerReady = false;
std::mutex customAuthServerMtx;
std::condition_variable customAuthServerStartup;

void runCustomAuthTestServers() {
  std::unique_lock<std::mutex> lk(customAuthServerMtx);
  auto installFrom = [](int port, const std::function<void(int)>& install) {
    for (int end = port + 500; port < end; ++port) {
      try {
        install(port);
        return port;
      } catch (std::exception &) {
      }
    }
    throw std::runtime_error("Couldn't allocate port for custom-auth test server");
  };
  bool installed = false;
  try {
    const int p = installFrom(11501, [](int port) {
      installNetREPL(port, &c(), std::make_shared<TestChallengeAuthenticator>());
    });
    const int r = installFrom(p + 1, [](int port) {
      installNetREPL(port, &recordingServer(), std::make_shared<TestChallengeAuthenticator>());
    });
    customAuthServerPort    = p;
    recordingAuthServerPort = r;
    installed = true;
  } catch (std::exception &) {
  }
  customAuthServerReady = true;
  lk.unlock();
  customAuthServerStartup.notify_one();
  if (installed) {
    runEventLoop([&]{ return customAuthServerStop.load(); });
  }
}

void startCustomAuthTestServers() {
  if (customAuthServerPort < 0) {
    if (customAuthServerThread.joinable()) {
      customAuthServerThread.join();
    }
    c(); // construct the compiler (and register the exit cleanup) on this thread first
    std::unique_lock<std::mutex> lk(customAuthServerMtx);
    customAuthServerReady = false;
    customAuthServerThread = std::thread(&runCustomAuthTestServers);
    customAuthServerStartup.wait(lk, []{ return customAuthServerReady; });
    if (customAuthServerPort < 0) {
      throw std::runtime_error("Couldn't allocate ports for custom-auth test servers");
    }
  }
}

int testCustomAuthServerPort()    { startCustomAuthTestServers(); return customAuthServerPort; }
int testRecordingAuthServerPort() { startCustomAuthTestServers(); return recordingAuthServerPort; }

// the handshake as it appears on the wire, for peers that speak it by hand
std::string sizedField(const std::string& x) {
  const size_t n = x.size();
  return std::string(reinterpret_cast<const char*>(&n), sizeof(n)) + x;
}
std::string mechanismOpening(const std::string& mechanism) {
  const uint32_t version = 0x00010002;
  return std::string(reinterpret_cast<const char*>(&version), sizeof(version)) + sizedField(mechanism);
}
bool sendRaw(int fd, const std::string& x) {
  return ::send(fd, x.data(), x.size(), 0) == static_cast<ssize_t>(x.size());
}
// the next status byte, or -1 if the server hung up (or said nothing in time)
int recvStatus(int fd) {
  uint8_t b = 0;
  return ::recv(fd, &b, 1, MSG_WAITALL) == 1 ? static_cast<int>(b) : -1;
}
std::string recvSizedField(int fd) {
  size_t n = 0;
  if (::recv(fd, &n, sizeof(n), MSG_WAITALL) != static_cast<ssize_t>(sizeof(n)) || n > 4096) {
    return "<no token>";
  }
  std::string x(n, '\0');
  if (n > 0 && ::recv(fd, &x[0], n, MSG_WAITALL) != static_cast<ssize_t>(n)) {
    return "<short token>";
  }
  return x;
}
int connectForHandshake(int port) {
  int fd = connectSocket("localhost", port);
  struct timeval tv;
  tv.tv_sec  = 10;
  tv.tv_usec = 0;
  ::setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
  return fd;
}

// what an attempt to open an authenticated session failed with, or "" if it worked
std::string authFailure(const std::function<void()>& open) {
  try {
    open();
  } catch (std::exception& ex) {
    return ex.what();
  }
  return "";
}
bool contains(const std::string& s, const std::string& part) {
  return s.find(part) != std::string::npos;
}

} // namespace

TEST(Net, customAuthenticatorAcceptsAPeerAndReportsItsIdentity) {
  const int port = testCustomAuthServerPort();
  const TestChallengeClient alice(testAuthUser, testAuthKey);

  // the header-only client, through DEFINE_NET_CLIENT
  SyncClient sync("localhost", static_cast<size_t>(port), alice);
  EXPECT_EQ(sync.add(20, 22), 42);
  sync.reconnect("localhost", static_cast<size_t>(port), alice);
  EXPECT_EQ(sync.add(1, 2), 3);

  // libhobbes' client
  const std::string hostPort = "localhost:" + str::from(port);
  hobbes::Client client(hostPort, alice);
  EXPECT_EQ(client.remoteHost(), hostPort);

  // and a Connect constraint in Hobbes code, with the authenticator
  // registered for its host:port
  hobbes::cc compiler;
  compiler.enableRemoteConnections({hostPort});
  compiler.setRemoteConnectionAuthenticator(
      hostPort, std::make_shared<TestChallengeClient>(testAuthUser, testAuthKey));
  compiler.compileFn<void()>(
      "let x = (connection :: (Connect \"" + hostPort + "\" p) => p) in ()")();

  // every one of alice's sessions was handed the token the server sent with
  // its acceptance, and checked it
  EXPECT_EQ(alice.serversVerified(), 3);

  // the Server is told who authenticated, before it is told of the connection
  recordingServer().clear();
  int fd = connectForHandshake(testRecordingAuthServerPort());
  net::authenticateSession(fd, alice);
  auto events = recordingServer().eventsOnceThereAre(2);
  EXPECT_EQ(events.size(), size_t(2));
  if (events.size() == 2) {
    EXPECT_EQ(events[0], "authenticated " + testAuthMechanism + " " + testAuthUser);
    EXPECT_EQ(events[1], "connect");
  }
  ::close(fd);
  events = recordingServer().eventsOnceThereAre(3);
  EXPECT_EQ(events.size(), size_t(3));
  if (events.size() == 3) {
    EXPECT_EQ(events[2], "disconnect");
  }
}

TEST(Net, customAuthenticatorRejectsAPeer) {
  const int port = testCustomAuthServerPort();
  const std::string hostPort = "localhost:" + str::from(port);
  recordingServer().clear();

  // a wrong answer to the challenge, through each client
  const TestChallengeClient wrongKey(testAuthUser, "not-alices-key");
  std::string why = authFailure([&] { SyncClient x("localhost", static_cast<size_t>(port), wrongKey); });
  EXPECT_TRUE(contains(why, "authentication rejected"));
  EXPECT_TRUE(contains(why, "refused the 'test-challenge' handshake"));

  why = authFailure([&] { hobbes::Client x(hostPort, wrongKey); });
  EXPECT_TRUE(contains(why, "authentication rejected by '" + hostPort + "'"));

  hobbes::cc compiler;
  compiler.enableRemoteConnections({hostPort});
  compiler.setRemoteConnectionAuthenticator(
      hostPort, std::make_shared<TestChallengeClient>(testAuthUser, "not-alices-key"));
  EXPECT_EXCEPTION(compiler.compileFn<void()>(
      "let x = (connection :: (Connect \"" + hostPort + "\" p) => p) in ()"));
  EXPECT_EXCEPTION(compiler.setRemoteConnectionAuthenticator(hostPort, nullptr));

  // a user the server does not know is refused at the first token, and a
  // session that throws refuses its peer
  why = authFailure([&] { hobbes::Client x(hostPort, TestChallengeClient("nobody", testAuthKey)); });
  EXPECT_TRUE(contains(why, "refused the 'test-challenge' handshake"));
  why = authFailure([&] { hobbes::Client x(hostPort, TestChallengeClient("throw", testAuthKey)); });
  EXPECT_TRUE(contains(why, "refused the 'test-challenge' handshake"));

  // a session that never settles is cut off: the listener refuses the peer
  // once it has gone the most rounds a handshake may take
  why = authFailure([&] { hobbes::Client x(hostPort, TestChallengeClient("loop", testAuthKey)); });
  EXPECT_TRUE(contains(why, "authentication rejected"));

  // on the wire: a rejection byte, then the hang-up, and no request is read
  int fd = connectForHandshake(port);
  EXPECT_TRUE(sendRaw(fd, mechanismOpening(testAuthMechanism) + sizedField(testAuthUser)));
  EXPECT_EQ(recvStatus(fd), 2);
  EXPECT_TRUE(contains(recvSizedField(fd), "challenge-"));
  EXPECT_TRUE(sendRaw(fd, sizedField("a guess")));
  EXPECT_EQ(recvStatus(fd), 0);
  EXPECT_EQ(recvStatus(fd), -1);
  ::close(fd);

  // a client whose session is not satisfied with the server's acceptance
  // does not open the session, and what its session threw is what is reported
  why = authFailure([&] {
    class DistrustfulClient : public TestChallengeClient {
    public:
      DistrustfulClient() : TestChallengeClient(testAuthUser, testAuthKey) {}
      std::unique_ptr<net::NetREPLClientAuthSession> begin() const override {
        class Session : public net::NetREPLClientAuthSession {
        public:
          explicit Session(std::unique_ptr<net::NetREPLClientAuthSession> inner) : inner(std::move(inner)) {}
          std::string next(const std::string& t) override { return this->inner->next(t); }
          [[noreturn]] void accepted(const std::string&) override { throw std::runtime_error("the server did not prove itself"); }
        private:
          std::unique_ptr<net::NetREPLClientAuthSession> inner;
        };
        return std::unique_ptr<net::NetREPLClientAuthSession>(new Session(TestChallengeClient::begin()));
      }
    };
    hobbes::Client x(hostPort, DistrustfulClient());
  });
  EXPECT_TRUE(contains(why, "the server did not prove itself"));

  // the Server hears nothing of a peer that did not authenticate ...
  const int recording = testRecordingAuthServerPort();
  fd = connectForHandshake(recording);
  why = authFailure([&] { net::authenticateSession(fd, wrongKey); });
  EXPECT_TRUE(contains(why, "authentication rejected"));
  ::close(fd);

  // ... and can itself turn away one that did: the connection is dropped
  // without Server::connect
  fd = connectForHandshake(recording);
  net::authenticateSession(fd, TestChallengeClient("mallory", "mallory-test-only-key"));
  EXPECT_EQ(recvStatus(fd), -1);
  ::close(fd);

  fd = connectForHandshake(recording);
  net::authenticateSession(fd, TestChallengeClient(testAuthUser, testAuthKey));
  auto events = recordingServer().eventsOnceThereAre(3);
  EXPECT_EQ(events.size(), size_t(3));
  if (events.size() == 3) {
    EXPECT_EQ(events[0], "authenticated " + testAuthMechanism + " mallory");
    EXPECT_EQ(events[1], "authenticated " + testAuthMechanism + " " + testAuthUser);
    EXPECT_EQ(events[2], "connect");
  }
  ::close(fd);

  // an authenticator that cannot be installed is refused outright, rather
  // than leaving a listener that lets everyone in
  NullNetREPLServer server;
  EXPECT_EXCEPTION(installNetREPL(0, &server, std::shared_ptr<NetREPLAuthenticator>()));
  class Unnamed : public TestChallengeAuthenticator {
    std::string mechanism() const override { return ""; }
  };
  EXPECT_EXCEPTION(installNetREPL(0, &server, std::make_shared<Unnamed>()));
  class LongNamed : public TestChallengeAuthenticator {
    std::string mechanism() const override { return std::string(65, 'x'); }
  };
  EXPECT_EXCEPTION(installNetREPL(0, &server, std::make_shared<LongNamed>()));
  class Unbounded : public TestChallengeAuthenticator {
    size_t maxTokenBytes() const override { return static_cast<size_t>(-1); }
  };
  EXPECT_EXCEPTION(installNetREPL(0, &server, std::make_shared<Unbounded>()));
  EXPECT_EXCEPTION(installNetREPL(0, &c(), std::make_shared<Unbounded>()));

  // after all of which the listener still serves a peer that authenticates
  SyncClient ok("localhost", static_cast<size_t>(port), TestChallengeClient(testAuthUser, testAuthKey));
  EXPECT_EQ(ok.add(19, 23), 42);
}

TEST(Net, aPeerStallingInACustomHandshakeDoesNotStallTheServer) {
  // the authenticator never sees the connection: the event loop gathers each
  // token without waiting and calls the session only when a whole one is in,
  // so a peer that stops anywhere in the handshake holds up no one else
  const int port = testCustomAuthServerPort();
  const std::string opening = mechanismOpening(testAuthMechanism);
  const std::string first   = opening + sizedField(testAuthUser);

  // ... part way through the mechanism's name
  int inName = connectForHandshake(port);
  EXPECT_TRUE(sendRaw(inName, first.substr(0, sizeof(uint32_t) + sizeof(size_t) + 3)));

  // ... part way through its first token
  int inToken = connectForHandshake(port);
  EXPECT_TRUE(sendRaw(inToken, first.substr(0, first.size() - 1)));

  // ... having been sent the challenge, and not answering it
  int inChallenge = connectForHandshake(port);
  EXPECT_TRUE(sendRaw(inChallenge, first));
  EXPECT_EQ(recvStatus(inChallenge), 2);
  const std::string challenge = recvSizedField(inChallenge);

  // ... and one that claims a token it will never send, of the largest size
  // the authenticator allows
  int inLargeToken = connectForHandshake(port);
  EXPECT_TRUE(sendRaw(inLargeToken, opening + sizedField(std::string(256, 'x')).substr(0, sizeof(size_t))));

  const TestChallengeClient alice(testAuthUser, testAuthKey);
  {
    SyncClient c("localhost", static_cast<size_t>(port), alice);
    EXPECT_EQ(c.add(1, 2), 3);
  }
  {
    SyncClient c("localhost", testServerPort());
    EXPECT_EQ(c.add(2, 3), 5);
  }

  // the stalled peers were kept, not dropped: each picks up where it stopped
  EXPECT_TRUE(sendRaw(inName, first.substr(sizeof(uint32_t) + sizeof(size_t) + 3)));
  EXPECT_EQ(recvStatus(inName), 2);
  EXPECT_TRUE(sendRaw(inName, sizedField(recvSizedField(inName) + ":" + testAuthKey)));
  EXPECT_EQ(recvStatus(inName), 4);
  EXPECT_EQ(recvSizedField(inName), "server-proof:" + testAuthKey);

  EXPECT_TRUE(sendRaw(inToken, first.substr(first.size() - 1)));
  EXPECT_EQ(recvStatus(inToken), 2);

  EXPECT_TRUE(sendRaw(inChallenge, sizedField(challenge + ":" + testAuthKey)));
  EXPECT_EQ(recvStatus(inChallenge), 4);
  EXPECT_EQ(recvSizedField(inChallenge), "server-proof:" + testAuthKey);

  // a whole handshake delivered a byte at a time comes to the same end
  int slow = connectForHandshake(port);
  auto dribble = [&](const std::string& bytes) {
    for (char ch : bytes) {
      EXPECT_EQ(::send(slow, &ch, 1, 0), ssize_t(1));
      std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
  };
  dribble(first);
  EXPECT_EQ(recvStatus(slow), 2);
  dribble(sizedField(recvSizedField(slow) + ":" + testAuthKey));
  EXPECT_EQ(recvStatus(slow), 4);

  // a token larger than the authenticator allows is not waited for at all
  int oversized = connectForHandshake(port);
  EXPECT_TRUE(sendRaw(oversized, opening + sizedField(std::string(257, 'x')).substr(0, sizeof(size_t))));
  EXPECT_EQ(recvStatus(oversized), -1);
  int longName = connectForHandshake(port);
  EXPECT_TRUE(sendRaw(longName, mechanismOpening(std::string(65, 'x')).substr(0, sizeof(uint32_t) + sizeof(size_t))));
  EXPECT_EQ(recvStatus(longName), -1);

  for (int fd : {inName, inToken, inChallenge, inLargeToken, slow, oversized, longName}) {
    ::close(fd);
  }

  SyncClient c("localhost", static_cast<size_t>(port), alice);
  EXPECT_EQ(c.add(4, 5), 9);
}

TEST(Net, mismatchedAuthenticatorsAreRefusedOnBothSides) {
  const int customPort = testCustomAuthServerPort();
  const int secretPort = testAuthenticatedServerPort();
  const int legacyPort = testServerPort();
  const TestChallengeClient alice(testAuthUser, testAuthKey);

  // a client with another custom mechanism is told the listener does not use it
  const TestChallengeClient otherMechanism(testAuthUser, testAuthKey, "another-mechanism");
  std::string why = authFailure([&] { SyncClient x("localhost", static_cast<size_t>(customPort), otherMechanism); });
  EXPECT_TRUE(contains(why, "authentication rejected"));
  EXPECT_TRUE(contains(why, "does not use the 'another-mechanism' mechanism"));

  // a shared secret offered to a listener with a custom authenticator
  why = authFailure([&] {
    SyncClient x("localhost", static_cast<size_t>(customPort), net::NetREPLSharedSecret(authenticatedServerSecret));
  });
  EXPECT_TRUE(contains(why, "authentication rejected"));

  // a custom mechanism offered to a listener with a shared secret
  why = authFailure([&] { SyncClient x("localhost", static_cast<size_t>(secretPort), alice); });
  EXPECT_TRUE(contains(why, "authentication rejected"));
  EXPECT_TRUE(contains(why, "may not use this mechanism"));

  // ... and to a listener with no authentication at all
  why = authFailure([&] { SyncClient x("localhost", static_cast<size_t>(legacyPort), alice); });
  EXPECT_TRUE(contains(why, "authentication rejected"));
  why = authFailure([&] { hobbes::Client x("localhost:" + str::from(legacyPort), alice); });
  EXPECT_TRUE(contains(why, "authentication rejected"));

  // a client that does not authenticate is hung up on by a listener that
  // requires it, before anything it sends is read as a request
  int fd = connectForHandshake(customPort);
  fdwrite(fd, static_cast<uint32_t>(0x00010000));
  EXPECT_EQ(recvStatus(fd), -1);
  ::close(fd);
  // a client of the shared secret is not served by the custom mechanism's
  // version word, nor the reverse, whatever it sends next
  // (each opening goes out in one send: the listener hangs up at the version
  // word, and a second write could find the connection already reset)
  const uint32_t sharedSecretVersion = 0x00010001;
  fd = connectForHandshake(customPort);
  EXPECT_TRUE(sendRaw(fd, std::string(reinterpret_cast<const char*>(&sharedSecretVersion), sizeof(sharedSecretVersion)) + sizedField(testAuthUser)));
  EXPECT_EQ(recvStatus(fd), -1);
  ::close(fd);
  fd = connectForHandshake(secretPort);
  EXPECT_TRUE(sendRaw(fd, mechanismOpening(testAuthMechanism) + sizedField(testAuthUser)));
  EXPECT_EQ(recvStatus(fd), -1);
  ::close(fd);

  // no listener was any the worse for it
  {
    SyncClient c("localhost", static_cast<size_t>(customPort), alice);
    EXPECT_EQ(c.add(1, 2), 3);
  }
  {
    SyncClient c("localhost", static_cast<size_t>(secretPort), net::NetREPLSharedSecret(authenticatedServerSecret));
    EXPECT_EQ(c.add(2, 3), 5);
  }
  {
    SyncClient c("localhost", legacyPort);
    EXPECT_EQ(c.add(3, 4), 7);
  }
}

// The shared secret is the built-in authenticator: installing a listener with
// a secret installs sharedSecretAuthenticator(secret), whose one-token session
// is what the existing shared-secret tests above exercise over the wire.
TEST(Net, sharedSecretIsTheBuiltInAuthenticator) {
  EXPECT_EXCEPTION(sharedSecretAuthenticator(""));
  EXPECT_EXCEPTION(sharedSecretAuthenticator(std::string(4097, 'x')));
  auto authenticator = sharedSecretAuthenticator(authenticatedServerSecret);
  EXPECT_EQ(authenticator->mechanism(), std::string("shared-secret"));
  EXPECT_EQ(net::NetREPLSharedSecret(authenticatedServerSecret).mechanism(), std::string("shared-secret"));

  auto session = authenticator->newSession(-1);
  std::string reply;
  EXPECT_TRUE(session->step("wrong-secret", &reply) == NetREPLAuthSession::Result::Reject);
  EXPECT_TRUE(session->step(authenticatedServerSecret + "x", &reply) == NetREPLAuthSession::Result::Reject);
  EXPECT_TRUE(session->step("", &reply) == NetREPLAuthSession::Result::Reject);
  EXPECT_TRUE(session->step(authenticatedServerSecret, &reply) == NetREPLAuthSession::Result::Accept);
  EXPECT_TRUE(reply.empty());
  EXPECT_EQ(session->peerIdentity(), std::string());
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

// a variant payload that counts its live instances, so that a test can tell
// (without a sanitizer) when a payload is destroyed that was never
// constructed, or constructed and never destroyed
struct CountedPayload {
  static int live;
  int value;

  CountedPayload() : value(0) { ++live; }
  explicit CountedPayload(int v) : value(v) { ++live; }
  CountedPayload(const CountedPayload& rhs) : value(rhs.value) { ++live; }
  CountedPayload& operator=(const CountedPayload&) = default;
  ~CountedPayload() { --live; }
};
int CountedPayload::live = 0;

namespace hobbes { namespace net {
template <>
  struct io<CountedPayload> {
    static const bool can_memcpy = false;
    static ty::desc type() { return io<int>::type(); }
    static void write(int s, const CountedPayload& x) { io<int>::write(s, x.value); }
    static void read(int s, CountedPayload* x) { io<int>::read(s, &x->value); }

    using async_read_state = io<int>::async_read_state;
    static void prepare(async_read_state* o) { io<int>::prepare(o); }
    static bool accum(int s, async_read_state* o, CountedPayload* x) { return io<int>::accum(s, o, &x->value); }
  };
}}

DEFINE_VARIANT(WireNumOrText,
  (num,  int),
  (text, std::string)
);

// read one value the way DEFINE_NET_CLIENT does (blocking), or the way
// DEFINE_ASYNC_NET_CLIENT does (in steps, as bytes arrive)
template <typename V>
static void readReply(bool async, int s, V* v) {
  if (async) {
    typename net::io<V>::async_read_state st;
    net::io<V>::prepare(&st);
    while (!net::io<V>::accum(s, &st, v)) { }
  } else {
    net::io<V>::read(s, v);
  }
}

// a variant is destroyed according to its tag, so its tag has to name the
// payload it holds at every point where reading into it can fail. The tag in
// a reply used to be received straight into the variant, ahead of the payload:
// a reply that ended inside the tag's four bytes left part of a new tag over
// the old payload, and that payload was then destroyed as another type (or a
// payload it owned was never destroyed).
template <typename V, typename Same>
static void expectACutOffTagToLeave(const V& was, char tagByte, const Same& same) {
  // the blocking read
  {
    int fds[2] = {-1, -1};
    EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);
    fdwrite(fds[1], &tagByte, 1);
    ::close(fds[1]);

    V got(was);
    EXPECT_EXCEPTION(net::io<V>::read(fds[0], &got));
    EXPECT_TRUE(same(got));
    ::close(fds[0]);
  }

  // the incremental read, where the tag may also just be slow to arrive, and
  // which may be stepped again after it has failed
  {
    int fds[2] = {-1, -1};
    EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);

    V got(was);
    typename net::io<V>::async_read_state st;
    net::io<V>::prepare(&st);

    fdwrite(fds[1], &tagByte, 1);
    EXPECT_FALSE(net::io<V>::accum(fds[0], &st, &got));
    EXPECT_TRUE(same(got));

    const char zero = 0;
    fdwrite(fds[1], &zero, 1);
    EXPECT_FALSE(net::io<V>::accum(fds[0], &st, &got));
    EXPECT_TRUE(same(got));

    ::close(fds[1]);
    EXPECT_EXCEPTION(net::io<V>::accum(fds[0], &st, &got));
    EXPECT_TRUE(same(got));
    EXPECT_EXCEPTION(net::io<V>::accum(fds[0], &st, &got));
    EXPECT_TRUE(same(got));
    ::close(fds[0]);
  }
}

TEST(Net, aReplyCutOffInsideAVariantTagLeavesTheVariantAsItWas) {
  const std::string text(64, 'x'); // too long to be stored inside the string

  // holding a string, and sent the first byte of the tag of an int
  using IorS = variant<int, std::string>;
  expectACutOffTagToLeave(IorS(text), '\x00', [&](const IorS& v) {
    return v.get<std::string>() != nullptr && *v.get<std::string>() == text;
  });

  // holding an int, and sent the first byte of the tag of a payload with a
  // destructor, which must not run for a payload that was never there
  using IorC = variant<int, CountedPayload>;
  CountedPayload::live = 0;
  expectACutOffTagToLeave(IorC(42), '\x01', [](const IorC& v) {
    return v.get<int>() != nullptr && *v.get<int>() == 42;
  });
  EXPECT_EQ(CountedPayload::live, 0);

  // and the same for a variant with named constructors
  expectACutOffTagToLeave(WireNumOrText::text(text), '\x00', [&](const WireNumOrText& v) {
    return v == WireNumOrText::text(text);
  });
}

TEST(Net, aRejectedVariantTagLeavesTheVariantAsItWas) {
  // a whole tag that names no constructor is not stored either. (The variant
  // is destroyed by hand here, after its tag has been checked: a variant left
  // holding such a tag used to end the process when it was destroyed.)
  using IorS = variant<int, std::string>;
  const std::string text(64, 'x');

  for (bool async : {false, true}) {
    int fds[2] = {-1, -1};
    EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);
    fdwrite(fds[1], static_cast<uint32_t>(7));
    fdwrite(fds[1], static_cast<int>(42));
    ::close(fds[1]);

    alignas(IorS) char mem[sizeof(IorS)];
    IorS* got = new (mem) IorS(text);
    EXPECT_EXCEPTION(readReply(async, fds[0], got));
    EXPECT_EQ(got->unsafeTag(), uint32_t(1));
    EXPECT_TRUE(*got->get<std::string>() == text);
    got->~IorS();
    ::close(fds[0]);
  }
}

TEST(Net, aVariantThatIsReadIntoGivesUpItsOldPayload) {
  // the payload that a variant holds when a reply is read into it is
  // destroyed (once, as what it is) when the new tag is known, rather than
  // being written over
  using IorC = variant<int, CountedPayload>;
  CountedPayload::live = 0;

  for (bool async : {false, true}) {
    int fds[2] = {-1, -1};

    // a counted payload, replaced by an int
    EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);
    fdwrite(fds[1], static_cast<uint32_t>(0));
    fdwrite(fds[1], static_cast<int>(7));
    ::close(fds[1]);
    {
      IorC got{CountedPayload(3)};
      EXPECT_EQ(CountedPayload::live, 1);
      readReply(async, fds[0], &got);
      EXPECT_TRUE(got.get<int>() != nullptr && *got.get<int>() == 7);
      EXPECT_EQ(CountedPayload::live, 0);
    }
    ::close(fds[0]);

    // ... and by another counted payload
    EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);
    fdwrite(fds[1], static_cast<uint32_t>(1));
    fdwrite(fds[1], static_cast<int>(9));
    ::close(fds[1]);
    {
      IorC got{CountedPayload(3)};
      readReply(async, fds[0], &got);
      EXPECT_TRUE(got.get<CountedPayload>() != nullptr && got.get<CountedPayload>()->value == 9);
      EXPECT_EQ(CountedPayload::live, 1);
    }
    EXPECT_EQ(CountedPayload::live, 0);
    ::close(fds[0]);

    // a reply cut off after its tag leaves the (empty) payload that the tag
    // names, to be destroyed as that
    EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);
    fdwrite(fds[1], static_cast<uint32_t>(1));
    fdwrite(fds[1], "\x09", 1);
    ::close(fds[1]);
    {
      IorC got(42);
      EXPECT_EXCEPTION(readReply(async, fds[0], &got));
      EXPECT_TRUE(got.get<CountedPayload>() != nullptr);
      EXPECT_EQ(CountedPayload::live, 1);
    }
    EXPECT_EQ(CountedPayload::live, 0);
    ::close(fds[0]);

    // a variant with named constructors starts out holding nothing
    const std::string text(64, 'x');
    EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);
    fdwrite(fds[1], static_cast<uint32_t>(1));
    fdwrite(fds[1], text);
    fdwrite(fds[1], static_cast<uint32_t>(0));
    fdwrite(fds[1], static_cast<int>(7));
    ::close(fds[1]);
    {
      WireNumOrText got;
      readReply(async, fds[0], &got);
      EXPECT_TRUE(got == WireNumOrText::text(text));
      readReply(async, fds[0], &got);
      EXPECT_TRUE(got == WireNumOrText::num(7));
    }
    ::close(fds[0]);
  }
}

struct NoScheduler : net::AsyncScheduler {
  void enqueue(net::AsyncReader*) override { }
};

TEST(Net, anAsyncRPCResultIsNotRetaggedByAReplyCutOffInsideItsTag) {
  // an asynchronous RPC accumulates each reply into one result that it keeps
  // between replies, so a reply that ends inside its tag must leave that
  // result as it was (here an int, with no payload to destroy)
  using IorC = variant<int, CountedPayload>;
  CountedPayload::live = 0;

  int fds[2] = {-1, -1};
  EXPECT_TRUE(::socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);
  char request[64];
  {
    NoScheduler sched;
    int s = fds[0];
    net::AsyncRPCFunc<IorC()> f(&sched, &s, 0);

    // a whole reply
    int seen = 0;
    f([&](const IorC& r) { if (const auto* c = r.get<CountedPayload>()) { seen = c->value; } });
    EXPECT_TRUE(::recv(fds[1], request, sizeof(request), 0) > 0);
    fdwrite(fds[1], static_cast<uint32_t>(1));
    fdwrite(fds[1], static_cast<int>(9));
    for (size_t i = 0; i < 100 && !f.readAndFinish(); ++i) { }
    EXPECT_EQ(seen, 9);
    EXPECT_EQ(CountedPayload::live, 0);

    // then one byte of the next reply's tag, and the end of the stream
    f([](const IorC&) { });
    EXPECT_TRUE(::recv(fds[1], request, sizeof(request), 0) > 0);
    fdwrite(fds[1], "\x01", 1);
    ::close(fds[1]);
    EXPECT_FALSE(f.readAndFinish());
    EXPECT_EXCEPTION(f.readAndFinish());
  }
  EXPECT_EQ(CountedPayload::live, 0);
  ::close(fds[0]);
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

TEST(Net, aPartialRequestDoesNotStallTheServer) {
  // completing the handshake and then sending only part of a request used to
  // wedge the loop just as an incomplete handshake did: the request fields
  // (the command byte, then the eid / expression / type payloads) were read
  // with blocking fdreads on the shared event loop, so a peer that sent one
  // command byte and stopped parked every other connection until it gave up
  // (STRFR-434002, the "identical-effect" variant the finding itself called
  // out). The request bytes are now accumulated without blocking and a request
  // is acted on only once it has fully arrived.
  int fd = connectSocket("localhost", testServerPort());

  // a complete handshake, so the connection is past the version word and into
  // the request phase
  fdwrite(fd, static_cast<uint32_t>(0x00010000));

  // one command byte (0 == prepare a lexical expression) and then nothing more:
  // the server is now waiting for the rest of a request that never comes
  uint8_t cmd = 0;
  EXPECT_EQ(::send(fd, &cmd, sizeof(cmd), 0), ssize_t(sizeof(cmd)));

  // the server keeps serving everyone else while that peer says nothing more
  {
    SyncClient c("localhost", testServerPort());
    EXPECT_EQ(c.add(1, 2), 3);
  }
  {
    SyncClient c("localhost", testServerPort());
    EXPECT_EQ(c.add(4, 5), 9);
  }

  ::close(fd);
}
// net.C's knob for these tests, in no header: how many invocations may wait
// suspended for their arguments on one event loop. Returns what it was.
namespace hobbes {
size_t setNetREPLMaxSuspendedInvocations(size_t);
size_t setNetREPLMaxBufferedInput(size_t);
bool   setNetREPLDirectInvocations(bool);
}

namespace {
// what the server has been asked to note, in the order it was asked
std::mutex       notedMtx;
std::vector<int> noted;

void netTestNote(int x) {
  std::lock_guard<std::mutex> lk(notedMtx);
  noted.push_back(x);
}

std::vector<int> notedSoFar() {
  std::lock_guard<std::mutex> lk(notedMtx);
  return noted;
}

// the test server, with netTestNote bound for its peers to call
int testNotingServerPort() {
  static std::once_flag bound;
  std::call_once(bound, [] { c().bind("netTestNote", &netTestNote); });
  return testServerPort();
}

// eids follow declaration order from 1: echo is 1, upTo is 2, note is 3,
// countOf is 4, sumOf is 5, echoAll is 6.
// int64_t rather than long: it is the 64-bit type with a codec on every
// platform (on macOS it is long long, and long has none)
DEFINE_NET_CLIENT(
    PayloadClient, (echo, std::string(std::string), "\\s.s"),
    (upTo, std::vector<int64_t>(int64_t), "\\n.[0L..n]"),
    (note, void(int), "netTestNote"),
    (countOf, int64_t(std::vector<std::string>), "\\xs.size(xs)"),
    (sumOf, int64_t(std::vector<int64_t>), "\\xs.sum(xs)"),
    (echoAll, std::vector<std::string>(std::vector<std::string>), "\\xs.xs"));

const uint32_t echoExprID    = 1;
const uint32_t upToExprID    = 2;
const uint32_t noteExprID    = 3;
const uint32_t countOfExprID = 4;
const uint32_t sumOfExprID   = 5;
const uint32_t echoAllExprID = 6;

std::string invocationOf(uint32_t eid, const std::string& arg) {
  std::string wire(1, static_cast<char>(HNET_CMD_INVOKE));
  wire.append(reinterpret_cast<const char*>(&eid), sizeof(eid));
  wire.append(arg);
  return wire;
}

std::string encodedString(const std::string& s) {
  const size_t n = s.size();
  return std::string(reinterpret_cast<const char*>(&n), sizeof(n)) + s;
}

std::string encodedInt(int x) {
  return std::string(reinterpret_cast<const char*>(&x), sizeof(x));
}

std::string encodedSize(size_t n) {
  return std::string(reinterpret_cast<const char*>(&n), sizeof(n));
}

std::string encodedInt64(int64_t x) {
  return std::string(reinterpret_cast<const char*>(&x), sizeof(x));
}

// a raw-socket write in these tests must fail rather than hang the suite if
// the server ever stops taking what it is sent (see withReceiveTimeout)
int withIOTimeouts(int fd) {
  struct timeval tv;
  tv.tv_sec  = 20;
  tv.tv_usec = 0;
  ::setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
  ::setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
#if defined(SO_NOSIGPIPE)
  int on = 1;
  ::setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &on, sizeof(on));
#endif
  return fd;
}

// a connection to the test server that has those from the start, so that a
// client's own handshake and prepares cannot hang either
int timedConnection(int port) {
  return withIOTimeouts(hobbes::net::makeConnection("localhost", static_cast<size_t>(port)));
}

bool sendAll(int fd, const std::string& x) {
#if defined(MSG_NOSIGNAL)
  const int flags = MSG_NOSIGNAL;
#else
  const int flags = 0;
#endif
  size_t i = 0;
  while (i < x.size()) {
    ssize_t r = ::send(fd, x.data() + i, x.size() - i, flags);
    if (r <= 0) return false;
    i += static_cast<size_t>(r);
  }
  return true;
}

// wait, but not forever, for the server to have noted 'n' things
bool waitForNoted(size_t n, int seconds) {
  const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(seconds);
  while (notedSoFar().size() < n) {
    if (std::chrono::steady_clock::now() > deadline) {
      return false;
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(5));
  }
  return true;
}

// the reply to an upTo(n) invocation
bool readUpToReply(int fd, int64_t n) {
  size_t len = 0;
  fdread(fd, reinterpret_cast<char*>(&len), sizeof(len));
  if (len != static_cast<size_t>(n + 1)) {
    return false;
  }
  std::vector<int64_t> xs(len);
  fdread(fd, reinterpret_cast<char*>(xs.data()), len * sizeof(int64_t));
  return xs.front() == 0 && xs.back() == n;
}

// one exchange with a separate client, timed: the server answering it at all
// while another peer is stuck mid-request is the point
long msToServeAnotherClient(SyncClient& other, int x) {
  auto t0 = std::chrono::steady_clock::now();
  EXPECT_EQ(other.add(x, 1), x + 1);
  return static_cast<long>(std::chrono::duration_cast<std::chrono::milliseconds>(
      std::chrono::steady_clock::now() - t0).count());
}
}

TEST(Net, aPartialInvocationArgumentDoesNotStallTheServer) {
  // the command byte and eid of an invocation were read without blocking, but
  // its argument was then decoded by the compiled call straight off the
  // socket, on the shared event loop: a peer that sent an invocation and only
  // part of its argument parked every other connection in read() until it
  // gave up (STRFR-434002). The argument is now decoded from what has arrived,
  // waiting -- off the loop -- for the rest.
  SyncClient other(timedConnection(testServerPort()));
  PayloadClient p(timedConnection(testNotingServerPort()));
  int fd = withIOTimeouts(p.fd());

  // far longer than one read (64KB), and than all the reads of one wake-up
  // (4MB), so it is buffered over many wake-ups. (Its decode stops only once:
  // a string says how long it is, and is waited for whole. See
  // anArgumentOfUnknownLengthIsDecodedAsItArrives for one that stops and
  // resumes many times.) Its echo is more than a socket takes in one write, so
  // the reply goes out in pieces too
  std::string arg(6000000, 'q');
  for (size_t i = 0; i < arg.size(); i += 4099) {
    arg[i] = static_cast<char>('a' + (i % 26)); // a misplaced piece would show
  }
  const std::string wire = invocationOf(echoExprID, encodedString(arg));

  size_t sent = 0;
  auto sendThrough = [&](size_t upTo) {
    EXPECT_TRUE(sendAll(fd, wire.substr(sent, upTo - sent)));
    sent = upTo;
    EXPECT_TRUE(msToServeAnotherClient(other, static_cast<int>(sent)) < 5000);
  };
  sendThrough(3);                                  // part of the eid
  sendThrough(1 + sizeof(uint32_t) + 3);           // part of the string's length
  sendThrough(1 + sizeof(uint32_t) + sizeof(size_t) + 70000); // some of the string
  for (size_t i = 0; i < 5; ++i) {
    sendThrough(sent + 1);                         // a byte at a time
  }
  sendThrough(wire.size() - 1);                    // all but the last byte

  // and once it has all arrived, the invocation is answered like any other
  EXPECT_TRUE(sendAll(fd, wire.substr(sent)));
  std::string reply;
  fdread(fd, &reply);
  EXPECT_EQ(reply.size(), arg.size());
  EXPECT_TRUE(reply == arg);

  // the connection is in step afterwards, for an invocation that arrives whole
  EXPECT_EQ(p.echo("again"), "again");
}

TEST(Net, aPeerLeavingMidArgumentDoesNotDisturbTheServer) {
  // the decode of an argument that never finishes is abandoned when its peer
  // goes, and nothing of it is left behind to run or to hold the loop
  SyncClient other(timedConnection(testServerPort()));
  for (size_t i = 0; i < 3; ++i) {
    PayloadClient p(timedConnection(testNotingServerPort()));
    // half of it: an invocation with a stack to wait on, though its decode
    // has not begun on it (a string is waited for whole; for one that has,
    // see aPeerLeavingMidDecodeDoesNotDisturbTheServer)
    std::string wire = invocationOf(echoExprID, encodedString(std::string(1000000, 'z')));
    EXPECT_TRUE(sendAll(withIOTimeouts(p.fd()), wire.substr(0, 500000)));
    EXPECT_TRUE(msToServeAnotherClient(other, static_cast<int>(i)) < 5000);
  }
  EXPECT_TRUE(msToServeAnotherClient(other, 42) < 5000);
  PayloadClient p(timedConnection(testNotingServerPort()));
  EXPECT_EQ(p.echo("after"), "after");
}

namespace {
// An argument that does not say how long it is: an array of strings, each
// of which does, but only once the decode has reached it. '*cuts' are places
// to split what is sent, one in the middle of each string, so that a decode
// fed a piece at a time has to stop within every one of them.
std::vector<std::string> stringsOfManyLengths() {
  std::vector<std::string> xs;
  for (size_t i = 0; i < 12; ++i) {
    xs.push_back(std::string(1000 + 37 * i, static_cast<char>('a' + i)));
  }
  return xs;
}
std::string echoAllWire(const std::vector<std::string>& xs, std::vector<size_t>* cuts) {
  std::string wire = invocationOf(echoAllExprID, encodedSize(xs.size()));
  for (const auto& x : xs) {
    cuts->push_back(wire.size() + sizeof(size_t) + x.size() / 2);
    wire += encodedString(x);
  }
  return wire;
}
}

TEST(Net, anArgumentOfUnknownLengthIsDecodedAsItArrives) {
  // with a stack to wait on, an argument whose length cannot be told ahead is
  // decoded as far as its bytes go, and the decode waits where it stopped --
  // within a string, here, a dozen times over -- to carry on when more is in
  SyncClient other(timedConnection(testServerPort()));
  PayloadClient p(timedConnection(testNotingServerPort()));

  const std::vector<std::string> xs = stringsOfManyLengths();
  std::vector<size_t> cuts;
  const std::string wire = echoAllWire(xs, &cuts);

  // the exchange with 'other' after each piece gives the server every chance
  // to have taken it before the next is sent (it is answered just the same
  // if it has not)
  size_t sent = 0;
  for (size_t cut : cuts) {
    EXPECT_TRUE(sendAll(p.fd(), wire.substr(sent, cut - sent)));
    sent = cut;
    EXPECT_TRUE(msToServeAnotherClient(other, static_cast<int>(sent)) < 5000);
  }
  EXPECT_TRUE(sendAll(p.fd(), wire.substr(sent)));

  size_t n = 0;
  fdread(p.fd(), reinterpret_cast<char*>(&n), sizeof(n));
  EXPECT_EQ(n, xs.size());
  bool same = n == xs.size();
  for (size_t i = 0; same && i < n; ++i) {
    std::string x;
    fdread(p.fd(), &x);
    same = x == xs[i];
  }
  EXPECT_TRUE(same);
  EXPECT_EQ(p.echo("in step"), "in step");
}

TEST(Net, aPeerLeavingMidDecodeDoesNotDisturbTheServer) {
  // ... and a peer that goes while its decode waits like that has the decode
  // given up where it stopped, on its own stack, and the stack given back
  SyncClient other(timedConnection(testServerPort()));

  const std::vector<std::string> xs = stringsOfManyLengths();
  std::vector<size_t> cuts;
  const std::string wire = echoAllWire(xs, &cuts);

  for (size_t i = 0; i < 3; ++i) {
    PayloadClient p(timedConnection(testNotingServerPort()));
    size_t sent = 0;
    for (size_t c = 0; c < 4; ++c) {
      EXPECT_TRUE(sendAll(p.fd(), wire.substr(sent, cuts[c] - sent)));
      sent = cuts[c];
      EXPECT_TRUE(msToServeAnotherClient(other, static_cast<int>(sent)) < 5000);
    }
  }
  EXPECT_TRUE(msToServeAnotherClient(other, 42) < 5000);
  PayloadClient p(timedConnection(testNotingServerPort()));
  EXPECT_EQ(p.echoAll({"after", "that"}), std::vector<std::string>({"after", "that"}));
}

TEST(Net, aPeerThatDoesNotReadItsRepliesDoesNotStallTheServer) {
  // replies went out with blocking sends, bounded only by a 10s SO_SNDTIMEO
  // per send: a peer that asked for large results and never read them held
  // the shared event loop in send() that long each time (STRFR-434002).
  // Replies are now queued and sent as the peer takes them.
  SyncClient other(timedConnection(testServerPort()));
  PayloadClient p(timedConnection(testNotingServerPort()));
  int fd = withReceiveTimeout(p.fd());

  // each reply is about 16MB, far more than the socket buffers between us
  const int64_t n = 2000000;
  for (size_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(sendAll(fd, invocationOf(upToExprID, encodedInt64(n))));
  }

  for (int i = 0; i < 3; ++i) {
    EXPECT_TRUE(msToServeAnotherClient(other, i) < 5000);
  }

  // the peer that was not reading is still served, in order, once it reads
  for (size_t i = 0; i < 2; ++i) {
    EXPECT_TRUE(readUpToReply(fd, n));
  }

  // and it leaves with replies still queued for it, which the server must
  // survive trying to write
}

TEST(Net, requestsSentBeforeAPeerClosesAreStillServed) {
  // a peer that sends invocations which return nothing and then closes has
  // done all it meant to. The end of its input used to be taken as the end of
  // the connection as soon as it was seen, with whatever requests had already
  // been received and not yet acted on thrown away.
  const size_t before = notedSoFar().size();
  const int    count  = 500;
  {
    PayloadClient p(timedConnection(testNotingServerPort()));
    std::string wire;
    for (int i = 0; i < count; ++i) {
      wire += invocationOf(noteExprID, encodedInt(i));
    }
    // and part of one more, which is never finished and must not run
    wire += invocationOf(noteExprID, encodedInt(-1)).substr(0, 7);
    EXPECT_TRUE(sendAll(withIOTimeouts(p.fd()), wire));
  } // closed, with every request probably still unread by the server

  EXPECT_TRUE(waitForNoted(before + count, 20));

  // served by a later exchange on another connection, so anything the server
  // was still going to do for the closed one has been done
  PayloadClient q(timedConnection(testNotingServerPort()));
  withIOTimeouts(q.fd());
  EXPECT_EQ(q.echo("sync"), "sync");

  const std::vector<int> got = notedSoFar();
  EXPECT_EQ(got.size(), before + count);
  bool inOrder = got.size() == before + count;
  for (int i = 0; inOrder && i < count; ++i) {
    inOrder = got[before + static_cast<size_t>(i)] == i;
  }
  EXPECT_TRUE(inOrder);
}

TEST(Net, aPeerThatHalfClosesGetsEveryReply) {
  // the same, for a peer that sends its requests, shuts down its sending side
  // and then reads: every reply comes back, in order, and then the end of the
  // stream. One reply in the middle is far larger than the socket buffers, so
  // the peer has long finished sending while the server still has that reply
  // to get out and the requests behind it to act on.
  PayloadClient p(timedConnection(testNotingServerPort()));
  int fd = withIOTimeouts(p.fd());

  const int64_t n     = 2000000;
  const int     count = 40;
  std::string wire;
  for (int i = 0; i < count; ++i) {
    wire += invocationOf(echoExprID, encodedString("before " + std::to_string(i)));
  }
  wire += invocationOf(upToExprID, encodedInt64(n));
  for (int i = 0; i < count; ++i) {
    wire += invocationOf(echoExprID, encodedString("after " + std::to_string(i)));
  }
  EXPECT_TRUE(sendAll(fd, wire));
  EXPECT_EQ(::shutdown(fd, SHUT_WR), 0);

  bool inOrder = true;
  for (int i = 0; i < count; ++i) {
    std::string reply;
    fdread(fd, &reply);
    inOrder = inOrder && reply == "before " + std::to_string(i);
  }
  EXPECT_TRUE(inOrder);
  EXPECT_TRUE(readUpToReply(fd, n));
  for (int i = 0; i < count; ++i) {
    std::string reply;
    fdread(fd, &reply);
    inOrder = inOrder && reply == "after " + std::to_string(i);
  }
  EXPECT_TRUE(inOrder);

  // and with nothing left to do for it, the server closes its side
  char b = 0;
  EXPECT_EQ(::recv(fd, &b, 1, 0), ssize_t(0));
}

TEST(Net, manyPipelinedRequestsAreServedInOrder) {
  // many small requests sent in one go: each used to shift everything behind
  // it down the connection's buffer, and all of them were read into memory
  // before the first was acted on. Requests with replies are mixed in, read as
  // they come, so the connection is also in step throughout.
  const size_t before = notedSoFar().size();
  PayloadClient p(timedConnection(testNotingServerPort()));
  int fd = withIOTimeouts(p.fd());

  const auto t0 = std::chrono::steady_clock::now();
  const int rounds   = 20;
  const int perRound = 1000;
  for (int r = 0; r < rounds; ++r) {
    std::string wire;
    for (int i = 0; i < perRound; ++i) {
      wire += invocationOf(noteExprID, encodedInt(r * perRound + i));
    }
    wire += invocationOf(echoExprID, encodedString("round " + std::to_string(r)));
    EXPECT_TRUE(sendAll(fd, wire));
  }
  bool inOrder = true;
  for (int r = 0; r < rounds; ++r) {
    std::string reply;
    fdread(fd, &reply);
    inOrder = inOrder && reply == "round " + std::to_string(r);
  }
  EXPECT_TRUE(inOrder);
  const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - t0).count();
  EXPECT_TRUE(ms < 30000);

  // the last reply follows the last note, so they have all been made
  const std::vector<int> got = notedSoFar();
  const size_t total = static_cast<size_t>(rounds) * static_cast<size_t>(perRound);
  EXPECT_EQ(got.size(), before + total);
  inOrder = got.size() == before + total;
  for (size_t i = 0; inOrder && i < total; ++i) {
    inOrder = got[before + i] == static_cast<int>(i);
  }
  EXPECT_TRUE(inOrder);
}

namespace {
// for as long as one of these is in scope
struct SuspendedInvocationCap {
  explicit SuspendedInvocationCap(size_t n) : was(hobbes::setNetREPLMaxSuspendedInvocations(n)) { }
  ~SuspendedInvocationCap() { hobbes::setNetREPLMaxSuspendedInvocations(this->was); }
  SuspendedInvocationCap(const SuspendedInvocationCap&) = delete;
  SuspendedInvocationCap& operator=(const SuspendedInvocationCap&) = delete;
  size_t was;
};

// true if the server has closed this connection (and nothing else is left on
// it to read); false if it has not within the socket's receive timeout
bool closedByServer(int fd) {
  char b = 0;
  return ::recv(fd, &b, 1, 0) == 0;
}
}

TEST(Net, moreWaitingInvocationsThanStacks) {
  // an invocation whose argument has not all arrived waits on a stack of its
  // own, and there are only so many of those. One that finds none free was
  // dropped; now none is, and each is answered once its argument is complete
  // -- with a stack or without, in whatever order they are completed.
  SuspendedInvocationCap cap(2);
  SyncClient other(timedConnection(testServerPort()));

  const std::string arg(300000, 'f');
  const std::string wire = invocationOf(echoExprID, encodedString(arg));
  const size_t      part = 100000;

  // three times as many part-sent arguments as there are stacks
  std::vector<std::unique_ptr<PayloadClient>> ps;
  for (int i = 0; i < 6; ++i) {
    ps.emplace_back(new PayloadClient(timedConnection(testNotingServerPort())));
    EXPECT_TRUE(sendAll(ps.back()->fd(), wire.substr(0, part)));
    EXPECT_TRUE(msToServeAnotherClient(other, i) < 5000);
  }
  auto finish = [&](size_t i) { return sendAll(ps[i]->fd(), wire.substr(part)); };
  auto answered = [&](size_t i) {
    std::string reply;
    fdread(ps[i]->fd(), &reply); // throws, failing the test, if it never comes
    return reply == arg;
  };

  // one peer leaves; the others are finished last-come first
  ps[1].reset();
  for (size_t i : {size_t(5), size_t(4), size_t(0), size_t(3), size_t(2)}) {
    EXPECT_TRUE(finish(i));
    EXPECT_TRUE(answered(i));
    EXPECT_TRUE(msToServeAnotherClient(other, 10 + static_cast<int>(i)) < 5000);
  }

  // and every one of them is still in step
  for (size_t i : {size_t(0), size_t(2), size_t(3), size_t(4), size_t(5)}) {
    EXPECT_EQ(ps[i]->echo("in step"), "in step");
  }
}

namespace {
// for as long as one of these is in scope
struct BufferedInputCap {
  explicit BufferedInputCap(size_t n) : was(hobbes::setNetREPLMaxBufferedInput(n)) { }
  ~BufferedInputCap() { hobbes::setNetREPLMaxBufferedInput(this->was); }
  BufferedInputCap(const BufferedInputCap&) = delete;
  BufferedInputCap& operator=(const BufferedInputCap&) = delete;
  size_t was;
};
struct NoDirectInvocations {
  NoDirectInvocations() : was(hobbes::setNetREPLDirectInvocations(false)) { }
  ~NoDirectInvocations() { hobbes::setNetREPLDirectInvocations(this->was); }
  NoDirectInvocations(const NoDirectInvocations&) = delete;
  NoDirectInvocations& operator=(const NoDirectInvocations&) = delete;
  bool was;
};

// An invocation that takes a stack and keeps it: part of its argument is
// sent, and the rest not until finishHeld. The server has answered 'other'
// twice since those bytes were there to be read by the time this returns, so
// it has been through its loop and has them.
const size_t heldSize = 1000;
std::string heldWire() {
  return invocationOf(echoExprID, encodedString(std::string(heldSize, 'h')));
}
void holdAStack(PayloadClient& holder, SyncClient& other) {
  EXPECT_TRUE(sendAll(holder.fd(), heldWire().substr(0, 100)));
  EXPECT_TRUE(msToServeAnotherClient(other, 100) < 5000);
  EXPECT_TRUE(msToServeAnotherClient(other, 101) < 5000);
}
bool finishHeld(PayloadClient& holder) {
  if (!sendAll(holder.fd(), heldWire().substr(100))) {
    return false;
  }
  std::string reply;
  fdread(holder.fd(), &reply);
  return reply == std::string(heldSize, 'h');
}

// With direct invocations off and every stack held, no invocation is served,
// another client's included: what shows the loop is still serving is that a
// new client is let in and has its expressions prepared.
bool serverTakesANewClient() {
  SyncClient probe(timedConnection(testServerPort())); // throws if it is not
  return probe.fd() >= 0;
}

// true if nothing arrives on this connection, and it is not closed, for this long
bool nothingToReadFor(int fd, int ms) {
  struct pollfd p;
  p.fd      = fd;
  p.events  = POLLIN;
  p.revents = 0;
  return ::poll(&p, 1, ms) == 0;
}

// true if the server has dropped this connection, whatever was left unread
// on either side of it; false if it has not within the socket's receive timeout
bool droppedByServer(int fd) {
  char b[4096];
  for (;;) {
    ssize_t r = ::recv(fd, b, sizeof(b), 0);
    if (r == 0) {
      return true;
    } else if (r < 0 && errno != EINTR) {
      return errno != EAGAIN && errno != EWOULDBLOCK;
    }
  }
}
}

TEST(Net, aWaitingInvocationIsGivenAStackThatComesFree) {
  // a connection waiting for a stack has sent all it has to say, so nothing
  // on its socket will have the loop call it again: when a stack comes free,
  // it has to be found and served by the server. (An invocation that is all
  // there is served without a stack, which would hide that; it is turned off
  // here.)
  SuspendedInvocationCap cap(1);
  SyncClient other(timedConnection(testServerPort()));

  PayloadClient holder(timedConnection(testNotingServerPort()));
  holdAStack(holder, other);
  NoDirectInvocations noDirect;

  // more of them than are served in one go when the stack comes free
  const size_t waiting = 24;
  std::vector<std::unique_ptr<PayloadClient>> ps;
  for (size_t i = 0; i < waiting; ++i) {
    ps.emplace_back(new PayloadClient(timedConnection(testNotingServerPort())));
    EXPECT_TRUE(sendAll(ps.back()->fd(), invocationOf(echoExprID, encodedString("waiter " + std::to_string(i)))));
  }
  EXPECT_TRUE(serverTakesANewClient());

  // not one is answered while the only stack is held ...
  bool quiet = true;
  for (size_t i = 0; i < waiting; ++i) {
    quiet = quiet && nothingToReadFor(ps[i]->fd(), i == 0 ? 1500 : 0);
  }
  EXPECT_TRUE(quiet);
  EXPECT_TRUE(serverTakesANewClient());

  // ... and every one is once it is given up, having sent nothing more
  EXPECT_TRUE(finishHeld(holder));
  bool allAnswered = true;
  for (size_t i = 0; i < waiting; ++i) {
    std::string reply;
    fdread(ps[i]->fd(), &reply);
    allAnswered = allAnswered && reply == "waiter " + std::to_string(i);
  }
  EXPECT_TRUE(allAnswered);
  EXPECT_TRUE(msToServeAnotherClient(other, 3) < 5000);
}

TEST(Net, aWaitingInvocationInManyPiecesIsServedOnceItIsWhole) {
  // with no stack to wait on, an argument is decoded from its start again as
  // it arrives, and so not for every piece of it. That must not leave one
  // that has all arrived undecoded: a string says how long it is, and is
  // decoded when that much is there -- however many pieces it came in, and
  // with the only stack still held.
  SuspendedInvocationCap cap(1);
  SyncClient other(timedConnection(testServerPort()));

  PayloadClient holder(timedConnection(testNotingServerPort()));
  holdAStack(holder, other);

  std::string arg(300000, 'm');
  for (size_t i = 0; i < arg.size(); i += 1009) {
    arg[i] = static_cast<char>('a' + (i % 26));
  }
  const std::string wire = invocationOf(echoExprID, encodedString(arg));

  PayloadClient p(timedConnection(testNotingServerPort()));
  int nodelay = 1;
  ::setsockopt(p.fd(), IPPROTO_TCP, TCP_NODELAY, &nodelay, sizeof(nodelay));
  const size_t piece = 8000;
  for (size_t sent = 0; sent < wire.size(); sent += piece) {
    EXPECT_TRUE(sendAll(p.fd(), wire.substr(sent, piece)));
    std::this_thread::sleep_for(std::chrono::milliseconds(2));
  }

  std::string reply;
  fdread(p.fd(), &reply); // throws, failing the test, if it never comes
  EXPECT_TRUE(reply == arg);
  EXPECT_TRUE(msToServeAnotherClient(other, 1) < 5000);

  // the same for an argument of several such parts, sent a part at a time
  const std::vector<std::string> xs = {std::string(50000, 'a'), "", std::string(70000, 'b'), "c"};
  std::string parts = encodedSize(xs.size());
  EXPECT_TRUE(sendAll(p.fd(), invocationOf(countOfExprID, parts)));
  for (const auto& x : xs) {
    std::this_thread::sleep_for(std::chrono::milliseconds(2));
    EXPECT_TRUE(sendAll(p.fd(), encodedString(x)));
  }
  int64_t n = 0;
  fdread(p.fd(), reinterpret_cast<char*>(&n), sizeof(n));
  EXPECT_EQ(n, static_cast<int64_t>(xs.size()));

  // and for elements of more than a byte each, in pieces that split them
  const int64_t count = 30000;
  std::string longs = encodedSize(static_cast<size_t>(count));
  for (int64_t i = 1; i <= count; ++i) {
    longs += encodedInt64(i);
  }
  const std::string sumWire = invocationOf(sumOfExprID, longs);
  for (size_t sent = 0; sent < sumWire.size(); sent += 7001) {
    EXPECT_TRUE(sendAll(p.fd(), sumWire.substr(sent, 7001)));
    std::this_thread::sleep_for(std::chrono::milliseconds(2));
  }
  int64_t sum = 0;
  fdread(p.fd(), reinterpret_cast<char*>(&sum), sizeof(sum));
  EXPECT_EQ(sum, count * (count + 1) / 2);

  EXPECT_TRUE(finishHeld(holder));
  EXPECT_EQ(p.echo("in step"), "in step");
}

TEST(Net, aWaitingInvocationThatFillsItsBufferIsSettledThere) {
  // a connection waiting for a stack reads on, as its invocation is not known
  // to be whole, up to the most that may be buffered. There it was left,
  // unread, until it had a stack: asking the loop for nothing, so that its
  // peer could go unnoticed -- or, if it reset the connection, be reported on
  // every pass of the loop (on Linux) with nothing done about it. Now what is
  // buffered is decoded there and then, and the connection served or dropped.
  SuspendedInvocationCap cap(1);
  BufferedInputCap       inputCap(256 * 1024);
  SyncClient other(timedConnection(testServerPort()));

  PayloadClient holder(timedConnection(testNotingServerPort()));
  holdAStack(holder, other);
  NoDirectInvocations noDirect;

  // whole invocations, more of them than may be buffered: the first is there
  // in full when the buffer fills, and is answered with the stack still held
  {
    const std::string arg(100000, 'w');
    PayloadClient p(timedConnection(testNotingServerPort()));
    std::string wire;
    for (size_t i = 0; i < 5; ++i) {
      wire += invocationOf(echoExprID, encodedString(arg));
    }
    EXPECT_TRUE(sendAll(p.fd(), wire));
    std::string reply;
    fdread(p.fd(), &reply);
    EXPECT_TRUE(reply == arg);
    EXPECT_TRUE(serverTakesANewClient());
  }

  // one invocation larger than may be buffered: dropped, as it would be if
  // it had a stack, rather than held
  PayloadClient q(timedConnection(testNotingServerPort()));
  const std::string tooBig = invocationOf(echoExprID, encodedSize(1000000) + std::string(600000, 'x'));
  sendAll(q.fd(), tooBig); // which may fail, as the server goes part-way through
  EXPECT_TRUE(droppedByServer(q.fd()));
  EXPECT_TRUE(serverTakesANewClient());

  EXPECT_TRUE(finishHeld(holder));
  EXPECT_EQ(holder.echo("in step"), "in step");
  EXPECT_TRUE(msToServeAnotherClient(other, 1) < 5000);
}

TEST(Net, aWaitingInvocationSentInSliversIsStillServed) {
  // an array of strings does not say how long it is, only how many: with no
  // stack to wait on, the work of decoding it again as its pieces arrive is
  // bounded, so it may or may not be decoded on arrival -- and is answered
  // either way once a stack is free
  SuspendedInvocationCap cap(1);
  SyncClient other(timedConnection(testServerPort()));

  PayloadClient holder(timedConnection(testNotingServerPort()));
  holdAStack(holder, other);

  // 40000 one-byte strings, the first 3000 bytes of them a byte at a time
  std::vector<std::string> xs(40000, "x");
  std::string arg = encodedSize(xs.size());
  for (const auto& x : xs) {
    arg += encodedString(x);
  }
  const std::string wire = invocationOf(countOfExprID, arg);

  PayloadClient p(timedConnection(testNotingServerPort()));
  int fd = p.fd();
  size_t sent = 0;
  for (; sent < 3000; ++sent) {
    EXPECT_TRUE(sendAll(fd, wire.substr(sent, 1)));
  }
  EXPECT_TRUE(msToServeAnotherClient(other, 1) < 5000);
  EXPECT_TRUE(sendAll(fd, wire.substr(sent)));
  EXPECT_TRUE(msToServeAnotherClient(other, 2) < 5000);

  EXPECT_TRUE(finishHeld(holder));

  int64_t n = 0;
  fdread(fd, reinterpret_cast<char*>(&n), sizeof(n));
  EXPECT_EQ(n, static_cast<int64_t>(xs.size()));
}

TEST(Net, aMalformedArgumentDropsOnlyItsConnection) {
  // an array length no array can have stops the decode of an argument. It
  // did so with an exception thrown through the compiled invocation, which
  // cannot be unwound on every platform: on macOS it ended the process.
  SyncClient other(timedConnection(testServerPort()));

  // found by an invocation run on what has arrived ...
  {
    PayloadClient p(timedConnection(testNotingServerPort()));
    int fd = withIOTimeouts(p.fd());
    EXPECT_TRUE(sendAll(fd, invocationOf(echoExprID, encodedSize(static_cast<size_t>(-1)))));
    EXPECT_TRUE(closedByServer(fd));
    EXPECT_TRUE(msToServeAnotherClient(other, 1) < 5000);
  }
  {
    PayloadClient p(timedConnection(testNotingServerPort()));
    int fd = withIOTimeouts(p.fd());
    EXPECT_TRUE(sendAll(fd, invocationOf(echoExprID, encodedSize(static_cast<size_t>(1) << 40))));
    EXPECT_TRUE(closedByServer(fd));
    EXPECT_TRUE(msToServeAnotherClient(other, 2) < 5000);
  }

  // ... and by one suspended part-way through its argument
  {
    PayloadClient p(timedConnection(testNotingServerPort()));
    int fd = withIOTimeouts(p.fd());
    // (the decode starts on its stack when the second piece is in, and waits
    // there for the rest of "three"; the third piece brings the bad length)
    EXPECT_TRUE(sendAll(fd, invocationOf(countOfExprID, encodedSize(3) + encodedString("one"))));
    EXPECT_TRUE(msToServeAnotherClient(other, 3) < 5000);
    EXPECT_TRUE(sendAll(fd, encodedString("three").substr(0, sizeof(size_t) + 2)));
    EXPECT_TRUE(msToServeAnotherClient(other, 4) < 5000);
    EXPECT_TRUE(sendAll(fd, encodedString("three").substr(sizeof(size_t) + 2) + encodedSize(static_cast<size_t>(-1))));
    EXPECT_TRUE(closedByServer(fd));
    EXPECT_TRUE(msToServeAnotherClient(other, 5) < 5000);
  }

  PayloadClient q(timedConnection(testNotingServerPort()));
  withIOTimeouts(q.fd());
  EXPECT_EQ(q.countOf({"a", "b", "c"}), int64_t(3));
}

TEST(Net, manyPreparesOnOneConnectionAreAllAnswered) {
  // each prepare's reply is queued, and the type memo is compacted (under the
  // compiler lock) only once that reply has gone. Sent all at once, accepted
  // and refused alike, every one is answered in full and in order, and the
  // connection and the server are in step afterwards.
  SyncClient other(timedConnection(testServerPort()));
  PayloadClient p(timedConnection(testNotingServerPort()));
  int fd = withIOTimeouts(p.fd());

  auto encodedBytes = [](const hobbes::net::bytes& x) {
    return encodedSize(x.size()) + std::string(x.begin(), x.end());
  };
  const std::string intys = encodedBytes(hobbes::net::RPCTyDef<int(int, int)>::inputType());
  const std::string outty = encodedBytes(hobbes::net::RPCTyDef<int(int, int)>::outputType());

  const uint32_t firstID = 100;
  const int      count   = 40;
  auto refused = [](int i) { return i % 5 == 3; };
  std::string wire;
  for (int i = 0; i < count; ++i) {
    const uint32_t eid = firstID + static_cast<uint32_t>(i);
    const std::string expr =
      refused(i) ? "\\x y.netTestNoSuchFunction(x, y)" : "\\x y.x+y+" + std::to_string(i);
    wire += std::string(1, static_cast<char>(HNET_CMD_DEFEXPR));
    wire += std::string(reinterpret_cast<const char*>(&eid), sizeof(eid));
    wire += encodedString(expr) + intys + outty;
  }
  EXPECT_TRUE(sendAll(fd, wire));

  bool asExpected = true;
  for (int i = 0; i < count; ++i) {
    uint8_t status = 2;
    fdread(fd, &status);
    if (refused(i)) {
      std::string why;
      fdread(fd, &why);
      asExpected = asExpected && status == 0 && !why.empty();
    } else {
      asExpected = asExpected && status == 1;
    }
  }
  EXPECT_TRUE(asExpected);
  EXPECT_TRUE(msToServeAnotherClient(other, 1) < 5000);

  // the last one prepared is there to be invoked
  const int last = count - 1;
  EXPECT_TRUE(sendAll(fd, invocationOf(firstID + static_cast<uint32_t>(last), encodedInt(20) + encodedInt(22))));
  int sum = 0;
  fdread(fd, &sum);
  EXPECT_EQ(sum, 42 + last);
  EXPECT_EQ(p.echo("in step"), "in step");
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

static std::string testAuthenticatedServerHostPort() {
  return "localhost:" +
      hobbes::str::from(testAuthenticatedServerPort());
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

TEST(Net, connectConstraintUsesConfiguredSharedSecret) {
  hobbes::cc client;
  const std::string hostPort = testAuthenticatedServerHostPort();
  client.enableRemoteConnections({hostPort});
  client.setRemoteConnectionSharedSecret(hostPort,
                                         authenticatedServerSecret);
  client.compileFn<void()>(
      "let x = (connection :: (Connect \"" + hostPort +
      "\" p) => p) in ()")();
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
// (With the query now bounded, that deadlock is what
// invokingAPeerInThisProcessFailsInsteadOfHanging below asserts on.)
//
// Pointing it at a separate `hi -p` child instead does not finish either --
// left running for over six minutes -- and that one is not diagnosed. Both
// are properties of driving this from inside the test process, not of the
// gate: the denial paths above cover the gate, and the allowed path is
// reproducible directly against the binaries, where hi enables all three
// gates for a local session:
//
//   $ umask 077; echo secret > /tmp/netrepl.secret
//   $ (sleep 600 | ./hi -s -p 9601 --netrepl-secret-file /tmp/netrepl.secret) &
//                                              # a peer; stdin must be a pipe,
//                                              # hi registers it with epoll
//   (one command, split over lines here)
//   $ ./hi -s -x -o no-Safe
//       --netrepl-client-secret-file localhost:9601=/tmp/netrepl.secret
//       -e 'let c = (connection ::
//       (Connect "localhost:9601" p) => p) in
//       print(receive(invoke(c, `(\x.x+1)`, 41)))'
//   42
//
// -o no-Safe is required independently of these gates, because invoke's
// generated code names unsafeCast, which Safe denies.

// Resolving an (Invoke ...) constraint asks the peer for the type of the
// expression, and that happens while the constraint is being resolved --
// i.e. while this thread holds the process-wide compiler lock. A peer
// served by this same process can never answer, because building its reply
// means compiling the expression it was sent, which needs that same lock.
//
// This used to hang the process with no error: the test below could not be
// written at all, and a suite that reached it never finished. The wait is
// now bounded, so the compile fails and says why.
TEST(Net, invokingAPeerInThisProcessFailsInsteadOfHanging) {
  const long budget = 2000; // don't make the suite wait the full default
  long prev = getRemoteTypeQueryTimeoutMS();
  setRemoteTypeQueryTimeoutMS(budget);
  struct Restore {
    long v;
    ~Restore() { setRemoteTypeQueryTimeoutMS(this->v); }
  } restore{prev};

  const std::string hp = testServerHostPort();

  hobbes::cc client;
  // both gates open: the point here is what happens once the invoke is
  // permitted and the query goes out, not the denial paths above
  client.enableRemoteConnections({hp});
  client.enableRemoteInvocation({hp});
  bool threw = false;
  auto t0 = std::chrono::steady_clock::now();
  try {
    // Connect on its own is fine -- the handshake needs no compiling, which
    // is why this gets as far as the invoke before there is any trouble
    client.compileFn<void()>(
      "let c = (connection :: (Connect \"" + hp + "\" p) => p) in "
      "let x = invoke(c, `(\\x.x+1)`, 41) in ()"
    );
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find(hp) != std::string::npos);
    EXPECT_TRUE(std::string(ex.what()).find("did not answer") != std::string::npos);
  }
  EXPECT_TRUE(threw);

  // it really waited for the peer rather than failing for some other reason
  // (a rejected handshake, a connection reset) that would produce an error
  // just as quickly
  auto waitedMS = std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::steady_clock::now() - t0).count();
  EXPECT_TRUE(waitedMS >= budget);

  // and the compiler still works afterwards -- the lock was given back
  EXPECT_EQ(client.compileFn<int()>("1+1")(), 2);
}

TEST(Net, aRetiredConnectionIsNotReusedForTheNextQuery) {
  // A query that gives up leaves the peer's reply still to come on that
  // socket, so reusing the connection would read it as the answer to the
  // next question -- compiling the next invoke against the previous
  // expression's result type. The connection is retired instead.
  //
  // Called directly like this the test thread is not holding the compiler
  // lock, so the server can answer, and nothing would be abandoned at the
  // default budget. This used to rely on a 1ms budget being shorter than
  // the server's answer; usually it was, but not always -- `x` is unbound,
  // the server has nothing to compile to say so, and on a quiet machine the
  // reply beat the deadline, the query failed with the server's error
  // instead of a timeout, and the connection was never retired. So hold
  // the compiler lock across the query instead: answering means compiling
  // (CCServer::prepare), which needs that lock, so the peer cannot reply
  // until the query has already given up. That is also the situation the
  // retirement exists for.
  long prev = getRemoteTypeQueryTimeoutMS();
  setRemoteTypeQueryTimeoutMS(50);
  struct Restore {
    long v;
    ~Restore() { setRemoteTypeQueryTimeoutMS(this->v); }
  } restore{prev};

  // deliberately not destroyed, the way makeConnection's Clients aren't:
  // the peer still owes this socket a reply, and closing it here would make
  // that write fail with EPIPE inside this very process
  auto* c = new Client("localhost:" + str::from(testServerPort()));

  auto la = LexicalAnnotation::null();
  {
    hlock _;
    EXPECT_EXCEPTION(c->remoteExpr(var("x", la), primty("int")));
  }

  // the second query must be refused outright rather than read the first
  // query's reply off the socket
  bool threw = false;
  try {
    c->remoteExpr(var("y", la), primty("int"));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("retired") != std::string::npos);
  }
  EXPECT_TRUE(threw);
}
