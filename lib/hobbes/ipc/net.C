
// A suspended invocation runs on its own stack (see InvokeFiber below), with
// the ucontext calls. They are deprecated on macOS but still provided, and are
// what is available on every platform hobbes builds on.
//
// On macOS these must be defined before the first system header, not just
// before <ucontext.h>: <sys/signal.h> defines struct ucontext either way, and
// without _XOPEN_SOURCE leaves out the register state at its end, which
// getcontext/makecontext/swapcontext write regardless -- past the end of the
// shorter struct. _XOPEN_SOURCE alone hides the non-POSIX declarations the
// rest of this file uses; _DARWIN_C_SOURCE keeps them.
#if defined(__APPLE__)
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE 1
#endif
#endif

#include <hobbes/hobbes.H>
#include <hobbes/ipc/net.H>
#include <hobbes/net.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/region.H>
#include <hobbes/util/str.H>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <exception>
#include <map>
#include <memory>
#include <mutex>
#include <sstream>
#include <vector>

#include <cerrno>
#include <csetjmp>
#include <cstring>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

#include <ucontext.h>

#if defined(__APPLE__)
// the ucontext_t this file sees must have room for the register state the
// ucontext calls store in it (see the feature macros at the top of this file)
static_assert(sizeof(ucontext_t) > sizeof(*static_cast<ucontext_t*>(nullptr)->uc_mcontext),
              "ucontext_t was declared without its machine context: _XOPEN_SOURCE must be defined before the first system header");
#endif

#if defined(__SANITIZE_ADDRESS__)
#define HOBBES_NET_ASAN 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define HOBBES_NET_ASAN 1
#endif
#endif
#ifdef HOBBES_NET_ASAN
#include <sanitizer/common_interface_defs.h>
#endif

namespace hobbes {

// get the port number associated with a named port
int lookupPort(const std::string &x) {
  if (str::is<int>(x)) {
    return str::to<int>(x);
  } else {
    struct servent *s = getservbyname(x.c_str(), "tcp");
    if (s != nullptr) {
      return ntohs(s->s_port);
    } else {
      throw std::runtime_error("Failed to resolve port name: " + x);
    }
  }
}

// create a listening socket on a given port and a given host
int allocateServer(int port, const std::string &host) {
  struct addrinfo *addrs = net::lookupAddrInfo(host, std::to_string(port));
  struct addrinfo *p = nullptr;
  int s;
  for (p = addrs; p != nullptr; p = p->ai_next) {
    if (p->ai_family != AF_INET || p->ai_protocol != IPPROTO_TCP)
      continue;
    s = socket(p->ai_family, p->ai_socktype, p->ai_protocol);

    if (s == -1) {
      throw std::runtime_error("Unable to allocate socket: " +
                               std::string(strerror(errno)));
    }
    // make sure that we can quickly restart the server if necessary
    int ra = 1;
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<char *>(&ra),
               sizeof(ra));
    if (bind(s, p->ai_addr, p->ai_addrlen) == 0)
      break;
    close(s);
  }
  freeaddrinfo(addrs);
  if (p == nullptr) {
    throw std::runtime_error("Unable to bind socket to address: " +
                             std::string(strerror(errno)));
  }
  // and then start to listen
  if (listen(s, SOMAXCONN) == -1) {
    close(s);
    throw std::runtime_error("Unable to listen on address: " +
                             std::string(strerror(errno)));
  }
  return s;
}

int allocateServer(const std::string &port) {
  return allocateServer(lookupPort(port));
}

int allocateFileSocketServer(const std::string &filepath) {
  int s = socket(AF_UNIX, SOCK_STREAM, 0);
  if (s == -1) {
    throw std::runtime_error("Unable to allocate socket: " +
                             std::string(strerror(errno)));
  }

  sockaddr_un addr;
  memset(&addr, 0, sizeof(addr));
  addr.sun_family = AF_UNIX;
  unlink(filepath.c_str());
  snprintf(addr.sun_path, sizeof(addr.sun_path), "%s", filepath.c_str());

  if (bind(s, reinterpret_cast<sockaddr *>(&addr), sizeof(addr)) == -1) {
    close(s);
    throw std::runtime_error("Unable to bind socket to file: " + filepath +
                             std::string(" : ") + std::string(strerror(errno)));
  }

  // and then start to listen
  if (listen(s, SOMAXCONN) == -1) {
    close(s);
    throw std::runtime_error("Unable to listen socket on file: " + filepath +
                             std::string(" : ") + std::string(strerror(errno)));
  }

  return s;
}

int connectSocket(int r, sockaddr *saddr, size_t len) {
  if (connect(r, saddr, len) == -1) {
    std::ostringstream ss;
    ss << "Unable to connect socket: " << strerror(errno) << std::flush;
    close(r);
    throw std::runtime_error(ss.str());
  }

  // wait for writeability
  fd_set wd;
  FD_ZERO(&wd);
  FD_SET(r, &wd);

  if (select(r + 1, nullptr, &wd, nullptr, nullptr) == -1) {
    std::ostringstream ss;
    ss << "Failed to connect socket while waiting for writeability: "
       << strerror(errno) << std::flush;
    close(r);
    throw std::runtime_error(ss.str());
  }

  // we're connected!
  return r;
}

// create a connected socket to a remote process
int connectSocket(const addrinfo *ai) {
  int r = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
  if (r == -1) {
    throw std::runtime_error("Unable to allocate socket: " +
                             std::string(strerror(errno)));
  }

  return connectSocket(r, ai->ai_addr, ai->ai_addrlen);
}

int connectFileSocket(const std::string &filepath) {
  int r = socket(AF_UNIX, SOCK_STREAM, 0);
  if (r == -1) {
    throw std::runtime_error("Unable to allocate socket: " +
                             std::string(strerror(errno)));
  }

  sockaddr_un addr;
  memset(&addr, 0, sizeof(addr));
  addr.sun_family = AF_UNIX;
  snprintf(addr.sun_path, sizeof(addr.sun_path), "%s", filepath.c_str());

  return connectSocket(r, reinterpret_cast<sockaddr *>(&addr), sizeof(addr));
}

int connectSocket(const std::string &host, int port) {
  // resolve with getaddrinfo, which reports failure rather than returning a
  // null the caller has to remember to check. Both legs of what was here
  // returned one: gethostbyname for a name that does not resolve, and
  // gethostbyaddr for every dotted-quad -- it was handed the address as text
  // where it expects the four bytes, so it never matched anything. The null
  // was then dereferenced for its address list, which crashed the process.
  // A resolvable name could crash it too: h_addr_list may be empty, and
  // h_addr_list[0] was read without checking.
  addrinfo hints;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family   = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = IPPROTO_TCP;

  addrinfo *res = nullptr;
  const std::string service = str::from(port);
  int rc = getaddrinfo(host.c_str(), service.c_str(), &hints, &res);
  if (rc != 0 || res == nullptr) {
    throw std::runtime_error("Failed to resolve host '" + host +
                             "': " + std::string(gai_strerror(rc)));
  }

  std::string lastError;
  for (const addrinfo *ai = res; ai != nullptr; ai = ai->ai_next) {
    try {
      int c = connectSocket(ai);
      freeaddrinfo(res);
      return c;
    } catch (std::exception &ex) {
      lastError = ex.what();
    }
  }
  freeaddrinfo(res);

  throw std::runtime_error("Failed to connect to '" + host + ":" + service +
                           "': " + lastError);
}

int connectSocket(const std::string &hostport) {
  str::pair p = str::lsplit(hostport, ":");
  if (!p.second.empty())
    return connectSocket(p.first, lookupPort(p.second));
  else
    return connectFileSocket(p.first);
}

// get the name of the host on the other end of this socket
std::string remoteHostname(int c) {
  struct sockaddr_in sin;
  socklen_t len = sizeof(sin);
  if (getpeername(c, reinterpret_cast<struct sockaddr *>(&sin), &len) < 0) {
    throw std::runtime_error("Couldn't get peer name for socket");
  } else {
    if (struct hostent *host =
            gethostbyaddr(reinterpret_cast<char *>(&sin.sin_addr),
                          sizeof(sin.sin_addr), AF_INET)) {
      return std::string(host->h_name);
    } else {
      throw std::runtime_error("Couldn't get host name for socket");
    }
  }
}

// get the port on the other end of this socket
int remotePort(int c) {
  struct sockaddr_in sin;
  socklen_t len = sizeof(sin);
  if (getpeername(c, reinterpret_cast<struct sockaddr *>(&sin), &len) < 0) {
    throw std::runtime_error("Couldn't get peer name for socket");
  } else {
    return ntohs(sin.sin_port);
  }
}

void prepareStrExpr(Server *s, int c, exprid eid, const std::string &expr,
                    const MonoTypes &intys, const MonoTypePtr &outty) {
  auto la = LexicalAnnotation::null();

  if (intys.empty() || (intys.size() == 1 && isUnit(intys[0]))) {
    s->prepare(c, eid,
               assume(s->readExpr(expr), functy(tuplety(intys), outty), la),
               tuplety());
  } else {
    ExprPtr f = assume(s->readExpr(expr), functy(intys, outty), la);
    Exprs args;
    for (size_t i = 0; i < intys.size(); ++i) {
      args.push_back(proj(var("p", la), ".f" + str::from(i), la));
    }
    s->prepare(c, eid, fn(str::strings("p"), fncall(f, args, la), la),
               tuplety(intys));
  }
}

// process one net-REPL request whose bytes are all present in [b, b + n).
//
// The fields used to be read straight off the socket with a run of blocking
// fdread calls -- the command byte, then the eid / expression / type payloads
// behind it -- on the shared, single-threaded event loop. A peer that sent one
// command byte and then went silent parked this handler in read() waiting for
// the next field, freezing every other connection in the process until it gave
// up or disconnected (STRFR-434002). The bytes are now accumulated without
// blocking (readNetREPLRequests, below) and a request is handed here only once
// it has fully arrived, so a partial request waits in a buffer rather than
// stopping the loop.
//
// Returns true to keep serving the connection, false if it should be
// disconnected (the caller performs the disconnect). The per-command paths that
// reject a bad expression or type are unchanged: they report the failure back
// to the peer and keep the connection.
//
// Replies are appended to '*out' when it is given, to be sent as the socket
// takes them; otherwise they are written to the socket directly. A connection
// served from memory always passes 'out', and never hands this an invocation
// (see serveBufferedConnection). Its caller also compacts the type memo for a
// prepare, once the queued reply has been sent (see the comment at cmd 0).
bool processNetREPLRequest(Server *s, int c, const char *b, size_t n, std::string* out = nullptr) {
  // a cursor over the frame. scanRequestFrame has already checked that every
  // field is fully present, so in normal operation these reads stay in bounds;
  // 'need' re-checks against n anyway, so that if the two parsers ever drift out
  // of step a mismatch becomes a caught exception and a disconnect rather than
  // an out-of-bounds read
  size_t off = 0;
  auto need = [&](size_t k) {
    if (k > n - off) {
      throw std::runtime_error("net REPL request frame shorter than its fields");
    }
  };
  auto readPrim = [&](void *dst, size_t k) {
    need(k);
    std::memcpy(dst, b + off, k);
    off += k;
  };
  auto readStr = [&](std::string *out) {
    size_t l = 0;
    readPrim(&l, sizeof(l));
    need(l);
    out->assign(b + off, l);
    off += l;
  };
  auto readBlob = [&](RawData *blob) {
    size_t l = 0;
    readPrim(&l, sizeof(l));
    need(l);
    blob->assign(b + off, b + off + l);
    off += l;
  };

  auto putBytes = [&](const void *p, size_t k) {
    if (k == 0) {
      return;
    } else if (out != nullptr) {
      out->append(reinterpret_cast<const char *>(p), k);
    } else {
      fdwrite(c, reinterpret_cast<const char *>(p), k);
    }
  };
  auto putStatus = [&](uint8_t x) { putBytes(&x, sizeof(x)); };
  // a size_t length and then the bytes, as fdwrite sends a string or a blob
  auto putSized = [&](const void *p, size_t k) {
    putBytes(&k, sizeof(k));
    putBytes(p, k);
  };

  try {
    uint8_t cmd = 0;
    readPrim(&cmd, sizeof(cmd));

    switch (cmd) {
    case 0: {
      // prepare a lexical expression with input and output types given
      //
      // the type descriptions read below are interned in the process-wide
      // type memo whether or not they are accepted; release what this
      // request does not end up keeping, on every way out of here. Only
      // once the reply is written, though: compacting takes the compiler
      // lock, and the peer may be a thread of this process that holds it
      // while it waits for this answer. A reply that is only queued here
      // ('out') has not been written yet, so then the compaction is left to
      // the caller, which does it once the reply has gone (or the connection
      // has)
      CompactMTypeMemoryAtExit compactAfter(out == nullptr);
      try {
        exprid eid = 0;
        readPrim(&eid, sizeof(eid));

        std::string expr;
        readStr(&expr);

        RawData ityd, otyd;
        readBlob(&ityd);
        readBlob(&otyd);

        MonoTypePtr itye = decode(ityd);
        MonoTypes itys;
        if (const Record *argl = is<Record>(itye)) {
          itys = selectTypes(argl->members());
        } else {
          itys.push_back(itye);
        }
        prepareStrExpr(s, c, eid, expr, itys, decode(otyd));

        // if we got this far, we have a successful result
        putStatus(1);
      } catch (std::exception &ex) {
        const std::string msg = ex.what();
        putStatus(0);
        putSized(msg.data(), msg.size());
      }
      break;
    }
    case 1: {
      // prepare a serialized expression, also return its type
      CompactMTypeMemoryAtExit compactAfter(out == nullptr); // as for cmd 0
      try {
        exprid eid = 0;
        readPrim(&eid, sizeof(eid));
        RawData exprd;
        readBlob(&exprd);
        ExprPtr expr;
        decode(exprd, &expr);

        RawData tyd;
        readBlob(&tyd);
        MonoTypePtr ty = decode(tyd);

        MonoTypePtr rty = s->prepare(c, eid, expr, ty);

        RawData rtyd;
        encode(rty, &rtyd);

        // if we got this far, we have a successful result
        putStatus(1);
        putSized(rtyd.data(), rtyd.size());
      } catch (std::exception &ex) {
        const std::string msg = ex.what();
        putStatus(0);
        putSized(msg.data(), msg.size());
      }
      break;
    }
    case 2: {
      // invoke a prepared expression (on a connection whose Server reads the
      // argument from the socket itself; see BufferedInvoker)
      exprid evid = 0;
      readPrim(&evid, sizeof(evid));
      s->evaluate(c, evid);
      break;
    }
    default:
      // unreachable: scanRequestFrame rejects an unknown command before a
      // frame is ever handed to this function
      return false;
    }
  } catch (std::exception &) {
    // a failure outside a per-command report path -- an invoke that threw, or
    // a broken pipe while writing a response -- means disconnect
    return false;
  }
  return true;
}

// the protocol version word, read a piece at a time
//
// this used to be read in the accept handler with a blocking fdread, which
// runs on the shared event loop: a peer that connected and sent fewer than
// four bytes stopped that loop until it went away, and every other listener
// in the process (another net REPL, hi's web server) waited with it. Nothing
// about accepting a connection says the peer has sent anything yet.
//
// So accept, remember the connection, and let the event loop say when there
// is something to read. One handler serves a connection for its whole life
// and dispatches on whether the handshake is still outstanding, rather than
// being swapped for another once it completes: registering a descriptor that
// is already registered is EPOLL_CTL_ADD of an existing entry on Linux, which
// fails.
//
// The handshake's reads are MSG_DONTWAIT rather than O_NONBLOCK on the
// descriptor, which may go on to be served by a Server that does blocking I/O
// on it (only a connection served from memory is made non-blocking, once its
// handshake is done: see bufferedConn). A peer
// that sends the version word and a command in one packet is not left waiting:
// the epoll set is level triggered, so the bytes still unread wake this handler
// again -- for the handshake here, and for the request bytes that follow it.
namespace {

using net::netREPLAuthAccepted;
using net::netREPLAuthAcceptedWithToken;
using net::netREPLAuthContinue;
using net::netREPLAuthRejected;
using net::netREPLAuthWrongMechanism;

#if defined(MSG_NOSIGNAL)
const int netREPLSendFlags = MSG_NOSIGNAL;
#else
const int netREPLSendFlags = 0;
#endif

// The server's answer to a client token: a status byte and, for the two
// statuses that carry one, the server's own token. It goes out in one send
// that never waits. A status byte always fits the send buffer of a connection
// this fresh; a token that does not fit (the peer is not reading its replies,
// or the token is far larger than a handshake token has any reason to be)
// means the connection is dropped by the caller, rather than the shared event
// loop waiting for room.
bool sendHandshakeReply(int c, uint8_t status, const std::string* token = nullptr) {
  std::string msg(1, static_cast<char>(status));
  if (token != nullptr) {
    const size_t n = token->size();
    msg.append(reinterpret_cast<const char *>(&n), sizeof(n));
    msg.append(*token);
  }
  return send(c, msg.data(), msg.size(), MSG_DONTWAIT | netREPLSendFlags) ==
         static_cast<ssize_t>(msg.size());
}

void validateSharedSecret(const std::string& sharedSecret) {
  if (sharedSecret.empty()) {
    throw std::runtime_error("The netREPL shared secret must not be empty");
  }
  if (sharedSecret.size() > net::maxNetREPLSharedSecretBytes) {
    throw std::runtime_error("The netREPL shared secret is too large");
  }
}

// a length-prefixed field of the handshake (the mechanism's name, a token),
// read a piece at a time
struct SizedField {
  size_t      size = 0;
  size_t      sizeHave = 0;
  std::string data;
  size_t      dataHave = 0;
};

struct HandshakeState {
  uint32_t    word = 0;
  size_t      have = 0;

  // for a listener with an authenticator: the mechanism the client named (when
  // the version word says it names one), the field being read now, and this
  // connection's session once its first token has arrived
  bool        named = false;
  std::string mechanism;
  SizedField  field;
  std::unique_ptr<NetREPLAuthSession> session;
  size_t      rounds = 0;
};
using HandshakeStatePtr = std::shared_ptr<HandshakeState>;

// what a listener was installed with, worked out once
struct NetREPLRegistration {
  Server*     server = nullptr;

  // null for a listener that does not authenticate its peers
  std::shared_ptr<NetREPLAuthenticator> authenticator;
  std::string mechanism;
  uint32_t    version = net::netREPLVersion; // the version word a client must open with
  bool        namesMechanism = false;        // ... and whether the mechanism's name follows it
  size_t      maxTokenBytes = 0;
};

std::mutex                    handshakeMutex;
std::map<int, HandshakeStatePtr> handshakes;

// bytes received for a connection's next request but not yet forming a whole
// one. Like the handshake, request reads no longer block the event loop: bytes
// are pulled off the socket as they arrive and a request is processed only once
// it has fully landed, so a peer that sends part of a request and stops waits
// here instead of parking the shared loop in read() (STRFR-434002).
//
// Only the one thread whose epoll owns a descriptor ever handles it, so a
// connection's own bytes are never touched concurrently; the mutex guards the
// map structure against handlers running for other descriptors in other event
// loop threads, exactly as handshakeMutex does.
std::mutex                    requestMutex;
std::map<int, std::string>    requestBuffers;

// a backstop, not the primary control: the fix is that an incomplete request
// no longer stalls the loop. This just keeps a peer that streams bytes without
// ever completing a request (for instance a cmd-0 request that claims a huge
// expression length and then dribbles it) from growing one buffer without
// bound.
//
// This bounds a request *header* -- the command byte, the eid, and, for a
// prepare, the serialized expression and type descriptions. Those are small: a
// serialized expression or type is orders of magnitude under this limit, so a
// real request never approaches it. Invocation argument data, the one payload
// that can legitimately be large, is not bounded by this: where it is buffered
// (a connection served from memory) maxBufferedInputBytes bounds it instead.
// (CWE-400 is the finding's own class, so bounding the buffer is on point
// rather than a new restriction out of nowhere.)
const size_t maxBufferedRequestBytes = 64 * 1024 * 1024;

// The compiled invocation for a prepared expression: it reads its argument
// with readFrom from the descriptor it is given, evaluates, and writes the
// result there with writeTo.
using NetFn = void (*)(int);

// A Server whose invocations are compiled NetFns can hand them over, and the
// connection is then served entirely from memory: argument bytes are buffered
// as they arrive and decoded from the buffer, and replies are queued and sent
// as the socket takes them, so no part of a request or reply ever waits on the
// peer inside the shared event loop (STRFR-434002). The compiler's own Server
// is one; a Server that only implements evaluate() reads its arguments from
// the socket itself, and is served as it always was.
struct BufferedInvoker {
  virtual ~BufferedInvoker() = default;

  // the invocation prepared as 'eid' on 'conn', or null if there is none
  virtual NetFn invocation(int conn, exprid eid) = 0;
};

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

// An invocation whose argument has not all arrived, suspended where its
// decoding ran out of bytes and resumed where it left off when more come.
//
// The argument is decoded by compiled readFrom code, which can take any
// BlockCodec type -- including user-defined instances -- so its length is not
// known until it has been read. Rather than restart the decode from the
// first byte each time more arrive (quadratic in the argument's size, and
// driven by however finely the peer chooses to split it), the invocation runs
// on a stack of its own: when a read finds the buffer short, the read swaps
// back to the event loop, and the next wake-up with enough bytes swaps back
// in. Only the argument decode ever suspends -- the compiled function reads
// all of its input before it evaluates anything -- so the expression itself
// never runs with a partial argument, and runs on this stack exactly once.
//
// While it is suspended, allocations it has made must outlive the memory pool
// reset the event loop does after every handler, so it allocates from a
// region of its own, switched in while it runs and out while it waits.
//
// A decode that is given up (its connection has gone, or its argument is not
// one that can be decoded) leaves the compiled code with siglongjmp, back to the frame that called it on this same stack,
// not with an exception: compiled frames cannot be unwound on every platform
// (on macOS they carry no unwind information the system unwinder can use, and
// an exception thrown through one ends the process). The frames passed over
// are the compiled function's and the read primitive's (readFromChannel,
// funcdefs.C), which own nothing that a destructor would have released.
class InvokeFiber {
public:
  InvokeFiber(NetFn fn, int fd);
  ~InvokeFiber();
  InvokeFiber(const InvokeFiber&) = delete;
  InvokeFiber& operator=(const InvokeFiber&) = delete;

  // the argument bytes (from the connection's buffer), the reply bytes, and
  // how many argument bytes the decode needs before it can go further
  FDMemoryChannel ch;

  // run until the invocation finishes or needs more input than 'ch' has
  void resume();

  bool finished() const { return this->done; }
  bool failed() const { return this->malformed || static_cast<bool>(this->err); }

private:
  NetFn              fn;
  bool               started = false;
  bool               done    = false;
  bool               abandon = false;
  bool               malformed = false; // the argument could not be decoded (FDMemoryChannel::malformed)
  std::exception_ptr err;

  ucontext_t loopCtx;
  ucontext_t fiberCtx;
  sigjmp_buf givenUp; // where a decode that is given up goes, in entry()
  char*      mem     = nullptr;
  size_t     memSize = 0;
  size_t     guard   = 0;

  size_t     regionSlot = 0; // the region this invocation allocates from
  size_t     fiberRegion = 0; // the region current in the fiber when it last stopped

#ifdef HOBBES_NET_ASAN
  void*       loopFakeStack  = nullptr;
  void*       fiberFakeStack = nullptr;
  const void* loopStackBottom = nullptr;
  size_t      loopStackSize   = 0;
#endif

  static void entry();
  bool suspend(); // from inside the fiber; does not return if it is being abandoned
  void switchIn();
};

// fibers and their regions are per event-loop thread, as the loop is
thread_local InvokeFiber* startingFiber = nullptr;
thread_local size_t       liveInvokeFibers = 0;

// at most this many invocations wait for their arguments at once on one event
// loop, each on a stack of its own. A connection that would make one more is
// not dropped: its argument is buffered as it arrives, and it takes its turn
// when a stack is free (fiberWaiters), or is served without one as soon as
// its argument is found to be all there (nextRequest).
std::atomic<size_t> maxSuspendedInvocations{256};

bool invokeStackFree() {
  return liveInvokeFibers < maxSuspendedInvocations.load();
}

// connections waiting for one of those, oldest first. One stays here, in its
// place, until it has a stack, has been served without one, or is dropped
// (forgetBufferedConn), so every entry is a live connection of this thread's
// event loop.
struct BufferedConn;
struct FiberWaiter {
  int              fd;
  Server*          server;
  BufferedInvoker* invoker;
  BufferedConn*    conn;
};
thread_local std::vector<FiberWaiter> fiberWaiters;

void forgetFiberWaiter(int c) {
  fiberWaiters.erase(std::remove_if(fiberWaiters.begin(), fiberWaiters.end(),
                                    [c](const FiberWaiter& w) { return w.fd == c; }),
                     fiberWaiters.end());
}

struct InvokeRegion {
  size_t                  slot;
  std::unique_ptr<region> r;
};
thread_local std::vector<InvokeRegion> invokeRegions;
thread_local std::vector<size_t>       freeInvokeRegions; // indexes into invokeRegions

size_t acquireInvokeRegion(size_t* index) {
  if (freeInvokeRegions.empty()) {
    std::unique_ptr<region> r(new region(32768));
    size_t slot = addThreadRegion(".netREPL.invoke", r.get());
    invokeRegions.push_back(InvokeRegion{slot, std::move(r)});
    freeInvokeRegions.push_back(invokeRegions.size() - 1);
  }
  *index = freeInvokeRegions.back();
  freeInvokeRegions.pop_back();
  return invokeRegions[*index].slot;
}

void releaseInvokeRegion(size_t index) {
  invokeRegions[index].r->clear();
  freeInvokeRegions.push_back(index);
}

size_t invokeStackSize() {
  // as deep as a thread's stack would be, within reason
  size_t sz = 8 * 1024 * 1024;
  struct rlimit rl;
  if (getrlimit(RLIMIT_STACK, &rl) == 0 && rl.rlim_cur != RLIM_INFINITY) {
    sz = static_cast<size_t>(rl.rlim_cur);
  }
  return std::min<size_t>(std::max<size_t>(sz, 1024 * 1024), 64 * 1024 * 1024);
}

InvokeFiber::InvokeFiber(NetFn fn, int fd) : fn(fn) {
  this->ch.fd = fd;

  const long pg = sysconf(_SC_PAGESIZE);
  this->guard   = pg > 0 ? static_cast<size_t>(pg) : 4096;
  this->memSize = this->guard + invokeStackSize();
  // most of a stack is never touched: where the system would otherwise count
  // all of it against what it will commit, ask it not to
#if defined(MAP_NORESERVE)
  const int noReserve = MAP_NORESERVE;
#else
  const int noReserve = 0;
#endif
  void* m = mmap(nullptr, this->memSize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON | noReserve, -1, 0);
  if (m == MAP_FAILED) {
    throw std::runtime_error("Unable to allocate a stack for a net REPL invocation: " + std::string(strerror(errno)));
  }
  this->mem = static_cast<char*>(m);

  // no destructor runs for a constructor that throws, so the stack is
  // unmapped here if any later step fails
  try {
    // the stack grows down into this page and faults there rather than into
    // whatever is mapped below it. Without the guard a deep decode could run
    // off the stack unnoticed, so its failure is not ignored
    if (mprotect(this->mem, this->guard, PROT_NONE) != 0) {
      throw std::runtime_error("Unable to guard the stack of a net REPL invocation: " + std::string(strerror(errno)));
    }

    memset(&this->loopCtx, 0, sizeof(this->loopCtx));
    memset(&this->fiberCtx, 0, sizeof(this->fiberCtx));
    if (getcontext(&this->fiberCtx) != 0) {
      throw std::runtime_error("Unable to set up a net REPL invocation: " + std::string(strerror(errno)));
    }
    this->fiberCtx.uc_stack.ss_sp   = this->mem + this->guard;
    this->fiberCtx.uc_stack.ss_size = this->memSize - this->guard;
    this->fiberCtx.uc_link          = &this->loopCtx;
    makecontext(&this->fiberCtx, &InvokeFiber::entry, 0);

    this->ch.refill    = [this](FDMemoryChannel*) { return this->suspend(); };
    this->ch.malformed = [this](FDMemoryChannel*) { siglongjmp(this->givenUp, 2); };

    // last, so that nothing after it can fail and leave the region taken
    size_t index = 0;
    this->fiberRegion = acquireInvokeRegion(&index);
    this->regionSlot  = index;
  } catch (...) {
    munmap(this->mem, this->memSize);
    throw;
  }
  ++liveInvokeFibers;
}

InvokeFiber::~InvokeFiber() {
  // a connection dropped while its invocation waits: let the decode give up
  // where it stopped and finish on its own stack (see suspend), rather than
  // discard a stack that is still in use
  if (this->started && !this->done) {
    this->abandon = true;
    this->resume();
  }
  munmap(this->mem, this->memSize);
  releaseInvokeRegion(this->regionSlot);
  --liveInvokeFibers;
}

void InvokeFiber::switchIn() {
  FDMemoryChannel* prevCh = setFDMemoryChannel(&this->ch);
  size_t loopRegion = setThreadRegion(this->fiberRegion);
#ifdef HOBBES_NET_ASAN
  __sanitizer_start_switch_fiber(&this->loopFakeStack, this->mem + this->guard, this->memSize - this->guard);
#endif
  swapcontext(&this->loopCtx, &this->fiberCtx);
#ifdef HOBBES_NET_ASAN
  __sanitizer_finish_switch_fiber(this->loopFakeStack, nullptr, nullptr);
#endif
  this->fiberRegion = setThreadRegion(loopRegion);
  setFDMemoryChannel(prevCh);
}

void InvokeFiber::resume() {
  if (this->done) {
    return;
  }
  if (!this->started) {
    this->started = true;
    startingFiber = this;
  }
  switchIn();
}

void InvokeFiber::entry() {
  InvokeFiber* f = startingFiber;
  startingFiber = nullptr;
#ifdef HOBBES_NET_ASAN
  __sanitizer_finish_switch_fiber(nullptr, &f->loopStackBottom, &f->loopStackSize);
#endif
  switch (sigsetjmp(f->givenUp, 0)) {
  case 0:
    try {
      f->fn(f->ch.fd);
    } catch (...) {
      f->err = std::current_exception();
    }
    break;
  case 2:
    f->malformed = true; // the decode refused its input, and nothing ran
    break;
  default:
    break; // the decode was abandoned part-way (suspend), and nothing ran
  }
  f->done = true;
#ifdef HOBBES_NET_ASAN
  // this stack is finished with: nothing to save
  __sanitizer_start_switch_fiber(nullptr, f->loopStackBottom, f->loopStackSize);
#endif
  swapcontext(&f->fiberCtx, &f->loopCtx); // never resumed
}

bool InvokeFiber::suspend() {
  if (!this->abandon) {
#ifdef HOBBES_NET_ASAN
    __sanitizer_start_switch_fiber(&this->fiberFakeStack, this->loopStackBottom, this->loopStackSize);
#endif
    swapcontext(&this->fiberCtx, &this->loopCtx);
#ifdef HOBBES_NET_ASAN
    __sanitizer_finish_switch_fiber(this->fiberFakeStack, &this->loopStackBottom, &this->loopStackSize);
#endif
  }
  if (this->abandon) {
    siglongjmp(this->givenUp, 1); // not by returning false: see the class comment
  }
  return true;
}

#if defined(__clang__)
#pragma clang diagnostic pop
#elif defined(__GNUC__)
#pragma GCC diagnostic pop
#endif

// An invocation run directly on the bytes a connection has buffered, as most
// can be: its channel, and where it goes if those bytes run out before its
// argument has all been read, or cannot be decoded. That is a siglongjmp out
// of the compiled code rather than an exception through it, for the reason
// given at InvokeFiber; and these are not locals of the function that calls
// sigsetjmp, as they are changed between that call and the jump back.
struct DirectInvoke {
  FDMemoryChannel  ch;
  FDMemoryChannel* prev = nullptr;
  sigjmp_buf       leftEarly; // jumped to with 1 (out of bytes) or 2 (malformed)
};

enum class Direct { Whole, Short, Malformed };

// Whatever the invocation throws is passed on.
#if defined(__GNUC__)
__attribute__((noinline))
#endif
Direct invokeOnBufferedBytes(NetFn fn, int c, DirectInvoke& d) {
  d.prev = setFDMemoryChannel(&d.ch);
  switch (sigsetjmp(d.leftEarly, 0)) {
  case 0:
    break;
  case 1:
    setFDMemoryChannel(d.prev);
    return Direct::Short;
  default:
    setFDMemoryChannel(d.prev);
    return Direct::Malformed;
  }
  try {
    fn(c);
  } catch (...) {
    setFDMemoryChannel(d.prev);
    throw;
  }
  setFDMemoryChannel(d.prev);
  return Direct::Whole;
}

// a connection served from memory (see BufferedInvoker): the bytes received
// (those before 'inPos' already used), the reply bytes not yet sent, and an
// invocation waiting for the rest of its argument (which starts 'invokeBase'
// bytes after 'inPos')
struct BufferedConn {
  std::string                  in;
  size_t                       inPos = 0;
  std::string                  out;
  size_t                       outSent = 0;
  std::unique_ptr<InvokeFiber> invoke;
  size_t                       invokeBase = 0;
  DirectInvoke                 direct;

  bool eof             = false; // the peer has sent all it is going to
  bool compactPending  = false; // a prepare's reply is queued: compact the type memo once it has gone
  bool waitingForFiber = false; // in fiberWaiters
  bool turnAsked       = false; // ... and asked to be called when writable, for its turn (askTurnsFrom)

  // for an invocation whose direct attempt came up short: how many argument
  // bytes the last attempt had and found it needed, and how much decoding
  // such attempts have cost so far (see nextRequest)
  size_t directTried = 0;
  size_t directNeed  = 0;
  size_t directSpent = 0;

  const char* unused() const { return this->in.data() + this->inPos; }
  size_t      unusedSize() const { return this->in.size() - this->inPos; }
};
using BufferedConnPtr = std::shared_ptr<BufferedConn>;

// guarded by requestMutex, as requestBuffers is. Never destroyed: a
// connection still open at exit may hold a suspended invocation, which belongs
// to its event loop's thread and must not be unwound by a static destructor
// running on whichever thread exits.
std::map<int, BufferedConnPtr>& bufferedConns = *new std::map<int, BufferedConnPtr>();

// Everything a connection served from memory has received but not yet used.
// Nothing more is read once a whole request is known to be in the buffer, so
// this is one request -- its header and, for an invocation, its argument (any
// BlockCodec value the prepared expression takes, so possibly large) -- and at
// most one read's worth of what follows it. A peer whose single request is
// larger than this is dropped; one that sends many requests faster than they
// are served is not, as what it has outstanding stays in the socket. (An
// invocation waiting for a stack is not known to be whole until it is next
// decoded, so its connection may read past it -- up to this much, at which
// point it is decoded once more, come what may, and dropped if that is still
// not a whole argument: see stepBufferedConnection.)
std::atomic<size_t> maxBufferedInputBytes{256 * 1024 * 1024};

// a buffer that held something large gives its memory back once it is empty
const size_t maxIdleBufferBytes = 1024 * 1024;

BufferedConnPtr bufferedConn(int c) {
  std::lock_guard<std::mutex> lk(requestMutex);
  auto i = bufferedConns.find(c);
  if (i != bufferedConns.end()) {
    return i->second;
  }

  // Nothing but this file's sends and receives touches the descriptor of a
  // connection served from memory, so it can be made non-blocking -- and has
  // to be: MSG_DONTWAIT alone does not keep a large send from waiting for
  // room on every platform (on macOS it waits, up to SO_SNDTIMEO).
  const int fl = fcntl(c, F_GETFL);
  if (fl == -1 || fcntl(c, F_SETFL, fl | O_NONBLOCK) == -1) {
    throw std::runtime_error("Unable to make a net REPL connection non-blocking: " + std::string(strerror(errno)));
  }
  return bufferedConns[c] = std::make_shared<BufferedConn>();
}

void forgetBufferedConn(int c) {
  BufferedConnPtr k;
  {
    std::lock_guard<std::mutex> lk(requestMutex);
    auto i = bufferedConns.find(c);
    if (i == bufferedConns.end()) {
      return;
    }
    k = std::move(i->second);
    bufferedConns.erase(i);
  }
  // outside the lock: abandoning a waiting invocation resumes it, to leave
  // its decode and finish on its own stack (InvokeFiber::suspend)
  k->invoke.reset();
  if (k->waitingForFiber) {
    // the descriptor's number may be someone else's by the time a stack is free
    forgetFiberWaiter(c);
    k->waitingForFiber = false;
  }
}

// The state of a connection still in its handshake, or null once it is done.
// Only the thread whose event loop owns a descriptor handles it, so the state
// is updated in place through this pointer; the mutex guards the map.
HandshakeStatePtr pendingHandshake(int c) {
  std::lock_guard<std::mutex> lk(handshakeMutex);
  auto i = handshakes.find(c);
  return i == handshakes.end() ? HandshakeStatePtr() : i->second;
}

void rememberHandshake(int c) {
  auto st = std::make_shared<HandshakeState>();
  std::lock_guard<std::mutex> lk(handshakeMutex);
  handshakes[c] = st;
}

void forgetHandshake(int c) {
  std::lock_guard<std::mutex> lk(handshakeMutex);
  handshakes.erase(c);
}

void forgetRequestBuffer(int c) {
  std::lock_guard<std::mutex> lk(requestMutex);
  requestBuffers.erase(c);
}

// the handler must go before the descriptor: the number can be reused the
// moment it is closed
void dropConnection(int c) {
  forgetHandshake(c);
  forgetRequestBuffer(c);
  forgetBufferedConn(c);
  unregisterEventHandler(c);
  close(c);
}

// read what there is of the version word; true once all four bytes are in
bool readVersionWord(int c, HandshakeState* st) {
  if (st->have == sizeof(st->word)) {
    return true;
  }
  ssize_t r = recv(c, reinterpret_cast<char *>(&st->word) + st->have, sizeof(st->word) - st->have, MSG_DONTWAIT);
  if (r > 0) {
    st->have += static_cast<size_t>(r);
  } else if (r == 0) {
    dropConnection(c); // the peer went away before it said anything
    return false;
  } else if (errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK) {
    return false; // nothing to read after all; wait to be called again
  } else {
    dropConnection(c);
    return false;
  }

  return st->have == sizeof(st->word); // else a piece of it; wait for the rest
}

// read what there is of a length-prefixed field, never waiting for more. A
// field that claims more than 'limit' bytes fails before any of it is read or
// any room is made for it. More may be readable when this returns Incomplete;
// the event loop is level triggered and calls again.
enum class FieldRead { Complete, Incomplete, Failed };

FieldRead readSizedField(int c, SizedField* f, size_t limit) {
  // -1: the connection is no good, 0: nothing more to read for now, 1: read some
  auto readSome = [c](char* into, size_t want, size_t* have) {
    ssize_t r = recv(c, into + *have, want - *have, MSG_DONTWAIT);
    if (r > 0) {
      *have += static_cast<size_t>(r);
      return 1;
    } else if (r == 0) {
      return -1; // the peer went away
    } else if (errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK) {
      return 0;
    }
    return -1;
  };

  if (f->sizeHave < sizeof(f->size)) {
    if (readSome(reinterpret_cast<char *>(&f->size), sizeof(f->size), &f->sizeHave) < 0) {
      return FieldRead::Failed;
    }
    if (f->sizeHave < sizeof(f->size)) {
      return FieldRead::Incomplete;
    }
    if (f->size > limit) {
      return FieldRead::Failed;
    }
  }

  // room is made for the bytes as they arrive, not for the size announced: a
  // peer that claims a large field and sends none of it holds nothing
  if (f->dataHave < f->size) {
    const size_t upTo = f->dataHave + std::min<size_t>(f->size - f->dataHave, 16 * 1024);
    f->data.resize(upTo);
    const int r = readSome(&f->data[0], upTo, &f->dataHave);
    f->data.resize(f->dataHave);
    if (r < 0) {
      return FieldRead::Failed;
    }
  }
  return f->dataHave == f->size ? FieldRead::Complete : FieldRead::Incomplete;
}

bool sharedSecretsMatch(const std::string& actual,
                        const std::string& expected) {
  volatile size_t difference = actual.size() ^ expected.size();
  for (size_t i = 0; i < expected.size(); ++i) {
    const auto actualByte = static_cast<unsigned char>(
        i < actual.size() ? actual[i] : 0);
    const auto expectedByte = static_cast<unsigned char>(expected[i]);
    difference |= static_cast<size_t>(actualByte ^ expectedByte);
  }
  return difference == 0;
}

// The shared secret as an authenticator: one token, the secret, accepted or
// refused. Its wire format is the one shared secrets have always used (see
// net::netREPLSharedSecretMechanism).
class SharedSecretAuthenticator : public NetREPLAuthenticator {
public:
  explicit SharedSecretAuthenticator(const std::string& secret)
      : secret(std::make_shared<const std::string>(secret)) {}

  std::string mechanism() const override { return net::netREPLSharedSecretMechanism(); }
  size_t maxTokenBytes() const override { return net::maxNetREPLSharedSecretBytes; }

  std::unique_ptr<NetREPLAuthSession> newSession(int) override {
    class Session : public NetREPLAuthSession {
    public:
      explicit Session(const std::shared_ptr<const std::string>& secret) : secret(secret) {}
      Result step(const std::string& token, std::string*) override {
        return sharedSecretsMatch(token, *this->secret) ? Result::Accept : Result::Reject;
      }
    private:
      std::shared_ptr<const std::string> secret;
    };
    return std::unique_ptr<NetREPLAuthSession>(new Session(this->secret));
  }

private:
  std::shared_ptr<const std::string> secret;
};

// check an authenticator before a listener is allocated for it. A null one is
// an error rather than a listener that lets everyone in.
NetREPLRegistration describeAuthenticator(const std::shared_ptr<NetREPLAuthenticator>& authenticator) {
  if (!authenticator) {
    throw std::runtime_error("The netREPL authenticator must not be null");
  }
  NetREPLRegistration r;
  r.authenticator = authenticator;
  r.mechanism     = authenticator->mechanism();
  if (r.mechanism.empty() || r.mechanism.size() > net::maxNetREPLMechanismNameBytes) {
    throw std::runtime_error("A netREPL authentication mechanism name must be between 1 and 64 bytes");
  }
  r.namesMechanism = r.mechanism != net::netREPLSharedSecretMechanism();
  r.version        = r.namesMechanism ? net::netREPLMechanismVersion : net::netREPLSharedSecretVersion;
  r.maxTokenBytes  = authenticator->maxTokenBytes();
  if (r.maxTokenBytes > net::maxNetREPLAuthTokenBytesLimit) {
    throw std::runtime_error("A netREPL authenticator may not accept tokens larger than 1MB");
  }
  return r;
}

// look at the request header bytes accumulated so far in 'b'.
//
//  - Complete: a whole header is present; '*frameLen' is its byte length. For
//    cmd 0/1 that is the entire request; for cmd 2 (invoke) it is only the
//    command byte and the eid -- the argument bytes that follow are read
//    straight off the socket by the compiled invocation, so the header must
//    stop before them and leave them in the socket (see readNetREPLRequests).
//  - NeedMore: the header is not all here yet; '*want' is how many more bytes
//    can be read *without* passing the end of this header -- i.e. the shortfall
//    of the field currently being read. Reading only that many keeps invoke
//    arguments (and any following request) in the socket.
//  - Invalid: the command byte is not one the protocol defines.
//
// The field layout mirrors what a client writes: a size_t length ahead of each
// string / blob payload, a uint32 eid, a one-byte command.
enum class FrameScan { NeedMore, Complete, Invalid };

FrameScan scanRequestFrame(const char* b, size_t n, size_t* frameLen, size_t* want) {
  size_t off = 0;

  // a fixed-width field of k bytes
  auto fixed = [&](size_t k) {
    size_t have = n - off;
    if (have < k) { *want = k - have; return false; }
    off += k;
    return true;
  };
  // a size_t length prefix followed by that many payload bytes
  auto sized = [&]() {
    size_t have = n - off;
    if (have < sizeof(size_t)) { *want = sizeof(size_t) - have; return false; }
    size_t len = 0;
    std::memcpy(&len, b + off, sizeof(len));
    size_t payloadHave = n - off - sizeof(size_t);
    if (payloadHave < len) { *want = len - payloadHave; return false; }
    off += sizeof(size_t) + len;
    return true;
  };

  if (n == 0) {
    *want = 1; // the command byte
    return FrameScan::NeedMore;
  }

  const auto cmd = static_cast<uint8_t>(b[0]);
  off = 1;

  bool complete = false;
  switch (cmd) {
  case 0: // eid, expr string, input-type blob, output-type blob
    complete = fixed(sizeof(exprid)) && sized() && sized() && sized();
    break;
  case 1: // eid, expr blob, type blob
    complete = fixed(sizeof(exprid)) && sized() && sized();
    break;
  case 2: // eid only; the invocation's arguments stay in the socket
    complete = fixed(sizeof(exprid));
    break;
  default:
    return FrameScan::Invalid;
  }

  if (!complete) {
    return FrameScan::NeedMore; // *want was set by the field that came up short
  }
  *frameLen = off;
  return FrameScan::Complete;
}

// Send what the socket will take of a connection's queued replies, without
// waiting. False if the connection has failed.
bool flushReplies(int c, BufferedConn& k) {
  while (k.outSent < k.out.size()) {
    ssize_t r = send(c, k.out.data() + k.outSent, k.out.size() - k.outSent, MSG_DONTWAIT | netREPLSendFlags);
    if (r > 0) {
      k.outSent += static_cast<size_t>(r);
    } else if (r < 0 && errno == EINTR) {
      continue;
    } else if (r < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
      return true; // the rest goes when the socket says it has room
    } else {
      return false;
    }
  }
  if (k.out.capacity() > maxIdleBufferBytes) {
    std::string().swap(k.out);
  } else {
    k.out.clear();
  }
  k.outSent = 0;
  return true;
}

// Drop the bytes of a connection's input that have been used. Each request
// only advances 'inPos'; the bytes behind it are moved out of the way here,
// and only once there are at least as many of them as there are bytes to
// move, so a run of small requests in one buffer costs time in proportion to
// its length rather than to its length times its count.
void compactInput(BufferedConn& k) {
  if (k.inPos == k.in.size()) {
    if (k.in.capacity() > maxIdleBufferBytes) {
      std::string().swap(k.in);
    } else {
      k.in.clear();
    }
    k.inPos = 0;
  } else if (k.inPos >= k.in.size() - k.inPos) {
    k.in.erase(0, k.inPos);
    k.inPos = 0;
  }
}

enum class Pull { Got, Drained, Eof, Full, Failed };

// Take one chunk of whatever has arrived. Unlike the header reader below, this
// can read past the end of a request: a connection served from memory keeps
// every byte it has received, an invocation's argument and any request after
// it included, so nothing needs to be left in the socket.
Pull pullBytes(int c, BufferedConn& k) {
  const size_t chunk = 64 * 1024;
  if (k.unusedSize() + chunk > maxBufferedInputBytes.load()) {
    return Pull::Full;
  }
  compactInput(k);
  size_t base = k.in.size();
  k.in.resize(base + chunk);
  for (;;) {
    ssize_t r = recv(c, &k.in[base], chunk, MSG_DONTWAIT);
    if (r > 0) {
      k.in.resize(base + static_cast<size_t>(r));
      return Pull::Got;
    } else if (r < 0 && errno == EINTR) {
      continue;
    }
    k.in.resize(base);
    if (r == 0) {
      return Pull::Eof; // the peer has closed, or will send no more
    } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
      return Pull::Drained;
    } else {
      return Pull::Failed;
    }
  }
}

// Done: a request was acted on. Wait: the next one is not all here yet.
// NoFiber: it is an invocation that has to wait for its argument, and has no
// stack to wait on yet (maxSuspendedInvocations).
enum class Step { Done, Wait, NoFiber, Failed };

// Carry on with an invocation waiting for its argument, if enough has arrived
// for its decode to get further.
Step continueInvocation(BufferedConn& k) {
  InvokeFiber& f = *k.invoke;
  const size_t have = k.unusedSize() - k.invokeBase;
  if (have < f.ch.needAtLeast) {
    return Step::Wait;
  }

  // the buffer may have moved as it grew
  f.ch.in    = k.unused() + k.invokeBase;
  f.ch.inLen = have;
  f.resume();
  if (!f.finished()) {
    return Step::Wait;
  }

  const bool   failed = f.failed();
  const size_t used   = k.invokeBase + f.ch.inPos;
  if (!failed) {
    k.out.append(f.ch.out);
  }
  k.invoke.reset();
  if (failed) {
    return Step::Failed; // an invocation that threw, or could not be decoded, means disconnect
  }
  k.inPos += used;
  return Step::Done;
}

// While no stack is free, an invocation that came up short can only be
// decoded again directly -- from its first byte, as there is nowhere to keep
// a decode in progress. (So a BlockCodec instance of the user's own whose
// readFrom does more than read has that done once for every attempt, not
// once for the invocation. The prelude's instances only read and allocate.)
//
// An attempt that comes up short says how many bytes it was waiting for
// (FDMemoryChannel::needAtLeast), and there is no other before those are
// here. For an array of primitives -- a string, most of all -- that is where
// the array ends, known from its length (fdReadBlockLenOf), and the attempt
// stops there having decoded none of it. So an argument that is one such
// array, with or without fixed-size values around it, costs only a few
// short attempts of a few bytes each however many pieces it comes in, and is
// answered when its last byte is in.
//
// Other shapes say less. An argument of several such arrays is tried again
// as each is completed, decoding those before it again; an array of strings
// or of records says only where the element being read ends, so its bytes
// being here says little. So that an argument sent in many pieces does not
// cost a decode of everything so far for each piece, an attempt whose bytes
// are here is still made only when there is twice what the last attempt had,
// or when there is nothing more to read for now and the attempts so far have
// cost (in bytes decoded) no more than four times what has arrived, plus this
// allowance. An argument whose attempts have cost more than that -- many
// variable-sized elements, or more than a few large arrays, arriving piece by
// piece -- can therefore be all here and still wait: for a stack, or for its
// peer to say it has finished sending, or for its connection's buffer to fill
// (each of the last two has it decoded once more, whatever it has cost).
// Plainly: such a request, complete, is not answered for as long as every
// stack stays held and its peer does neither of those; and nothing takes a
// stack back from an invocation whose peer has stopped sending (there is no
// idle timeout), so that can be indefinitely.
const size_t directRetryAllowance = 64 * 1024;

// for tests (setNetREPLDirectInvocations): whether an invocation is decoded at
// all while no stack is free, other than that once more
std::atomic<bool> directWithoutStack{true};

// Act on the next request in a connection's buffer, if all of it is there.
// 'quiet' says the socket has nothing more to read for now, and 'last' that
// nothing more will be read for this request at all (the buffer is full).
Step nextRequest(int c, Server* s, BufferedInvoker* bi, BufferedConn& k, bool quiet, bool last) {
  const char*  b = k.unused();
  const size_t n = k.unusedSize();

  size_t frameLen = 0;
  size_t want     = 0;
  switch (scanRequestFrame(b, n, &frameLen, &want)) {
  case FrameScan::Invalid:
    return Step::Failed;
  case FrameScan::NeedMore:
    // see maxBufferedRequestBytes: a request header has no reason to be this big
    return (n > maxBufferedRequestBytes) ? Step::Failed : Step::Wait;
  case FrameScan::Complete:
    break;
  }

  if (static_cast<uint8_t>(b[0]) != HNET_CMD_INVOKE) {
    // whether it is accepted or not, a prepare interns types: release what it
    // did not keep once its reply has gone (see processNetREPLRequest)
    k.compactPending = true;
    if (!processNetREPLRequest(s, c, b, frameLen, &k.out)) {
      return Step::Failed;
    }
    k.inPos += frameLen;
    return Step::Done;
  }

  // an invocation: the header is the command and eid, and its argument follows
  exprid eid = 0;
  std::memcpy(&eid, b + 1, sizeof(eid));
  NetFn fn = bi->invocation(c, eid);
  if (fn == nullptr) {
    return Step::Failed; // no such expression: disconnect, as ever
  }

  // Most arguments have all arrived by now, so first run the invocation on the
  // bytes there are. A read past them leaves the compiled code before the
  // expression is evaluated (the compiled function reads all of its input
  // first), so if this comes up short nothing has happened yet but a partial
  // decode, which the memory pool reset after this handler discards.
  //
  // An attempt that came up short is not repeated for every piece that
  // arrives -- that is the quadratic work the stacks exist to avoid: not at
  // all before the bytes it needed are here, and with no stack free, only as
  // directRetryAllowance describes. Once there is no more to come -- the peer
  // has sent all it is going to, or as much is buffered as may be -- there is
  // nothing to wait for: what is here is the whole argument or never will be.
  const size_t argHave = n - frameLen;
  const bool   final   = last || k.eof;
  bool attempt = true;
  if (final) {
    // whatever it costs: it is the last
  } else if (!invokeStackFree()) {
    if (!directWithoutStack.load()) {
      return Step::NoFiber;
    } else if (k.directNeed != 0) {
      if (argHave < k.directNeed) {
        return Step::NoFiber;
      }
      const bool doubled    = argHave >= 2 * k.directTried;
      const bool affordable = quiet && k.directSpent <= 4 * argHave + directRetryAllowance;
      if (!doubled && !affordable) {
        return Step::NoFiber;
      }
    }
  } else if (k.directNeed != 0 && argHave < k.directNeed) {
    attempt = false;
  }

  if (attempt) {
    DirectInvoke& d = k.direct;
    d.ch           = FDMemoryChannel();
    d.ch.fd        = c;
    d.ch.in        = b + frameLen;
    d.ch.inLen     = argHave;
    d.ch.refill    = [&d](FDMemoryChannel*) -> bool { siglongjmp(d.leftEarly, 1); };
    d.ch.malformed = [&d](FDMemoryChannel*) { siglongjmp(d.leftEarly, 2); };

    Direct r = Direct::Malformed;
    try {
      r = invokeOnBufferedBytes(fn, c, d);
    } catch (...) {
      return Step::Failed; // an invocation that threw means disconnect, as ever
    }

    if (r == Direct::Whole) {
      k.out.append(d.ch.out);
      k.inPos += frameLen + d.ch.inPos;
      k.directTried = 0;
      k.directNeed  = 0;
      k.directSpent = 0;
      d.ch = FDMemoryChannel();
      return Step::Done;
    } else if (r == Direct::Malformed || final) {
      return Step::Failed; // not an argument, or the rest of it is never coming
    }
    k.directTried  = argHave;
    k.directNeed   = d.ch.needAtLeast;
    k.directSpent += d.ch.inPos + 64;
  }

  // The rest of the argument is still to come. Decode it as it arrives, on a
  // stack of its own (see InvokeFiber), starting over just this once, and only
  // once there is more than the last attempt had.
  if (!invokeStackFree()) {
    return Step::NoFiber;
  }
  k.invoke.reset(new InvokeFiber(fn, c));
  k.invokeBase             = frameLen;
  k.invoke->ch.needAtLeast = k.directNeed;
  k.directTried = 0;
  k.directNeed  = 0;
  k.directSpent = 0;
  return continueInvocation(k);
}

// at most this much of a connection's work is done in one wake-up, so one busy
// peer takes its turn with the others rather than holding the loop
const size_t maxRequestsPerWakeup  = 64;
const size_t maxBytesReadPerWakeup = 4 * 1024 * 1024;

// One wake-up's work for a connection served from memory. False if the
// connection is finished with and should be dropped; otherwise '*again' says
// whether it stopped with work left that does not wait on the peer, and
// '*needsStack' whether its next request is an invocation waiting for a stack.
bool stepBufferedConnection(int c, Server* s, BufferedInvoker* bi, BufferedConn& k, bool* again, bool* needsStack) {
  size_t handled   = 0;
  size_t bytesRead = 0;
  bool   quiet     = false; // the last read found nothing more to read
  bool   last      = false; // ... and nothing more may be read for this request
  for (;;) {
    *needsStack = false;
    if (!flushReplies(c, k)) {
      return false;
    }
    if (!k.out.empty()) {
      // the peer has replies it has not taken: act on nothing more, and read
      // nothing more, until it does
      break;
    }
    if (k.compactPending) {
      k.compactPending = false;
      compactMTypeMemory();
    }

    if (handled == maxRequestsPerWakeup) {
      *again = true;
      break;
    }

    Step st = k.invoke ? continueInvocation(k) : nextRequest(c, s, bi, k, quiet, last);
    if (st == Step::Failed) {
      return false;
    } else if (st == Step::Done) {
      ++handled;
      quiet = false;
      last  = false;
      continue;
    } else if (last) {
      return false; // one request, larger than may be buffered
    }
    *needsStack = (st == Step::NoFiber);

    // The next request is not all here, as far as is known. Only now is more
    // read: while a whole request waits in the buffer, the rest of what the
    // peer has sent stays in the socket, where TCP holds the peer to the pace
    // it is served at.
    if (k.eof) {
      // ... and never will be. Every request that did arrive has been acted
      // on and answered (there is nothing unsent, above), so this is the end
      // of the connection -- whether the buffer is empty or holds part of a
      // request the peer did not finish
      return false;
    }
    if (bytesRead >= maxBytesReadPerWakeup) {
      break; // the rest is still readable, so the loop calls again
    }
    const size_t before = k.unusedSize();
    const Pull   pulled = pullBytes(c, k);
    if (pulled == Pull::Failed) {
      return false;
    } else if (pulled == Pull::Eof) {
      k.eof = true; // act on what is complete, then go
    } else if (pulled == Pull::Got) {
      bytesRead += k.unusedSize() - before;
      quiet = false;
    } else if (pulled == Pull::Full && !*needsStack) {
      return false; // one request, larger than may be buffered
    } else if (*needsStack && !quiet) {
      // an invocation with no stack to wait on, and nothing more to read for
      // now: this is when it may be worth decoding what there is (nextRequest)
      quiet = true;
    } else if (pulled == Pull::Full) {
      // As much is buffered as may be, for an invocation that has no stack
      // and so may not have been decoded as far as it could be. It is decoded
      // now, and that settles it: if its argument is all here it is served,
      // and if not it never could be, with a stack or without. (Holding it
      // unread until a stack came free would leave a connection that nothing
      // is asked of: not its input, which may not be read, and not a reply.
      // Its peer could then go without the loop being told, or -- reset, on
      // Linux -- with the loop told on every pass and unable to act on it.)
      last = true;
    } else {
      break; // waiting for bytes that have not come yet
    }
  }
  return true;
}

// One call of a connection's handler; see serveBufferedConnection.
void serveBufferedConnectionOnce(int c, Server* s, BufferedInvoker* bi) {
  BufferedConnPtr k;
  bool keep = false;
  try {
    k = bufferedConn(c);
    k->turnAsked = false;

    bool again      = false;
    bool needsStack = false;
    keep = stepBufferedConnection(c, s, bi, *k, &again, &needsStack);
    if (keep) {
      // a connection keeps its place among those waiting for a stack for as
      // long as it is waiting for one
      if (needsStack && !k->waitingForFiber) {
        fiberWaiters.push_back(FiberWaiter{c, s, bi, k.get()});
        k->waitingForFiber = true;
      } else if (!needsStack && k->waitingForFiber) {
        forgetFiberWaiter(c);
        k->waitingForFiber = false;
      }

      // Input that is not going to be read must not keep waking the handler:
      // the end of the peer's input, and anything it sends while it has
      // replies to take, stay readable for as long as they are left unread.
      // One or the other is always asked for -- input, or room for a reply
      // that is waiting to go (a peer that has finished sending is dropped
      // above unless it has one) -- so a peer that goes is always noticed, by
      // the read or the write that fails.
      setEventReadInterest(c, k->out.empty() && !k->eof);
      setEventWriteInterest(c, again || !k->out.empty());
    }
  } catch (...) {
    keep = false; // no room for a buffer or an invocation's stack, say
  }

  if (!keep) {
    const bool compact = k && k->compactPending;
    dropConnection(c);
    try {
      s->disconnect(c);
      if (compact) {
        compactMTypeMemory();
      }
    } catch (...) {
    }
  }
}

// at most this many waiters are given their turn within one call of another
// connection's handler (see serveBufferedConnection)
const size_t maxWaitersPerWakeup = 16;

// Have the event loop call the waiters from the i'th on, each as its socket
// is writable -- the one thing the loop can be asked for that does not wait
// on the peer saying more. A waiter whose socket is not writable (its peer is
// not taking what it has already been sent) has its turn when it is, or when
// it is reached in a later call here, and holds up nobody behind it: each is
// asked for itself. False if one could not be asked.
bool askTurnsFrom(size_t i) {
  try {
    for (; i < fiberWaiters.size(); ++i) {
      const FiberWaiter& w = fiberWaiters[i];
      if (!w.conn->turnAsked) {
        setEventWriteInterest(w.fd, true);
        w.conn->turnAsked = true; // until it is next called (serveBufferedConnectionOnce)
      }
    }
    return true;
  } catch (...) {
    return false;
  }
}

// Serve a connection whose Server hands over compiled invocations (see
// BufferedInvoker), never waiting on the peer.
//
// A request is acted on once enough of it is in the connection's buffer, and
// more is read from the socket only when the buffer is not known to hold a
// whole request. For a prepare that is the whole request; for an invocation,
// its argument is decoded from the buffer, and one that has not all arrived
// waits, suspended, for the rest (continueInvocation). Replies are queued and
// sent as the socket takes them; while any are unsent, no further request is
// acted on and nothing more is read, and the connection is called back as
// soon as the socket can take more (setEventWriteInterest). A peer that never
// reads its replies therefore holds at most one reply's worth of queue and one
// request's worth of input, the rest of what it sends waits in the socket (it
// is not timed out), and through all of it the event loop serves everyone
// else.
//
// A peer that has finished sending (it closed, or shut down its sending side)
// is still served the requests it sent in full, in order, with their replies
// written for as long as the socket takes them; the connection is dropped
// once nothing more can be done for it.
//
// Nothing thrown here reaches the event loop, where it would end the loop
// for every connection: whatever goes wrong drops this connection alone.
void serveBufferedConnection(int c, Server* s, BufferedInvoker* bi) {
  serveBufferedConnectionOnce(c, s, bi);

  // That may have given up an invocation's stack that others are waiting for.
  // They are served here and now, oldest first, rather than left for the
  // event loop to call: a waiter has nothing more to say, and so nothing that
  // would wake it. With a stack free, serving one always takes it out of the
  // queue (it has the stack, was served without one, or was dropped); it ends
  // when the stacks are taken again or nobody is waiting.
  //
  // Only so many in this call, though: each served without a stack leaves
  // the stack free for the next, and every one of them is a decode and an
  // evaluation that the rest of the loop waits for. The others are called by
  // the loop instead (askTurnsFrom). That bounds one call of this function,
  // and no more than that: a waiter called that way comes through here as
  // well, and may serve as many again after itself, and the loop may call
  // several of them in one pass before it gets to anything else.
  size_t i      = 0;
  size_t served = 0;
  while (i < fiberWaiters.size() && invokeStackFree()) {
    if (served >= maxWaitersPerWakeup && askTurnsFrom(i)) {
      break;
    }
    const FiberWaiter w = fiberWaiters[i];
    serveBufferedConnectionOnce(w.fd, w.server, w.invoker);
    resetMemoryPool(); // as the loop does after each handler it calls
    ++served;
    if (i < fiberWaiters.size() && fiberWaiters[i].fd == w.fd) {
      ++i; // (not expected, as above; but never go round on one waiter)
    }
  }
}

// serve a connection whose handshake is done. Read the next request's header a
// field at a time without ever blocking, and only when a whole header has
// arrived hand it to processNetREPLRequest.
//
// This is how a connection is served when its Server reads invocation
// arguments from the socket itself (one that is not a BufferedInvoker): no
// more than the current field's outstanding bytes are read at a time, so a
// cmd-2 invocation's arguments are left untouched in the socket for it. Such a
// Server's evaluate() reads its argument and writes its reply with blocking
// I/O of its own, which this cannot change; the compiler's Server, which is
// what installNetREPL(.., cc*, ..) and hi use, is served from memory instead
// (serveBufferedConnection), and never waits on its peer.
//
// A peer that sends part of a request and then stops does not park the shared
// event loop in read(): its bytes wait in a per-connection buffer and every
// other connection keeps being served (STRFR-434002).
void readNetREPLRequests(int c, void *d) {
  auto *s = reinterpret_cast<Server *>(d);
  if (auto *bi = dynamic_cast<BufferedInvoker *>(s)) {
    serveBufferedConnection(c, s, bi);
    return;
  }

  // take this connection's carried-over header bytes; moving a std::string is
  // O(1), so the map is locked only to hand off the buffer, not across reads
  std::string buf;
  {
    std::lock_guard<std::mutex> lk(requestMutex);
    auto it = requestBuffers.find(c);
    if (it != requestBuffers.end()) {
      buf = std::move(it->second);
    }
  }

  // carry the leftover header bytes (if any) to the next wake-up. An empty tail
  // gets no entry, so a connection that just finished a request -- including a
  // cmd-2 invoke whose handler closed the descriptor on a bad eid -- leaves no
  // stale buffer behind.
  auto saveTail = [&]() {
    std::lock_guard<std::mutex> lk(requestMutex);
    if (buf.empty()) {
      requestBuffers.erase(c);
    } else {
      requestBuffers[c] = std::move(buf);
    }
  };

  // Process at most one request per wake-up, then return -- exactly as the old
  // handler did. The event loop is level triggered, so if more requests are
  // already readable it wakes us again. Returning right after processing also
  // means we never touch the descriptor once a cmd-2 invocation may have closed
  // it (CCServer::evaluate closes on a bad eid), avoiding a double close.
  for (;;) {
    size_t frameLen = 0;
    size_t want     = 0;
    FrameScan sc = scanRequestFrame(buf.data(), buf.size(), &frameLen, &want);

    if (sc == FrameScan::Invalid) {
      dropConnection(c);
      s->disconnect(c);
      return;
    }

    if (sc == FrameScan::Complete) {
      if (!processNetREPLRequest(s, c, buf.data(), frameLen)) {
        dropConnection(c);
        s->disconnect(c);
        return;
      }
      buf.erase(0, frameLen);
      saveTail();
      return;
    }

    // NeedMore: read up to the current field's shortfall (never past the header,
    // so a following invocation's argument bytes stay in the socket)
    size_t chunk = std::min<size_t>(want, 64 * 1024);
    if (buf.size() + chunk > maxBufferedRequestBytes) {
      dropConnection(c);
      s->disconnect(c);
      return;
    }

    // read straight into the buffer's tail, then shrink to what actually
    // arrived -- no intermediate copy
    size_t base = buf.size();
    buf.resize(base + chunk);
    ssize_t r = recv(c, &buf[base], chunk, MSG_DONTWAIT);
    if (r > 0) {
      buf.resize(base + static_cast<size_t>(r)); // re-scan with the new bytes
    } else if (r == 0) {
      dropConnection(c); // the peer closed the connection
      s->disconnect(c);
      return;
    } else if (errno == EINTR) {
      buf.resize(base);
      continue;
    } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
      buf.resize(base);
      saveTail(); // nothing more right now; keep what we have for next time
      return;
    } else {
      dropConnection(c);
      s->disconnect(c);
      return;
    }
  }
}

// Take one step of an authenticating listener's handshake: finish reading the
// client's next token if it has all arrived, and act on the session's verdict.
// True once the peer is authenticated; otherwise the connection was dropped or
// is waiting for more bytes, and either way there is nothing more to do now.
bool authenticatePeer(int c, const NetREPLRegistration& reg, HandshakeState* st, std::string* identity) {
  if (st->word != reg.version) {
    dropConnection(c); // not this listener's way of opening a session
    return false;
  }

  if (reg.namesMechanism && !st->named) {
    switch (readSizedField(c, &st->field, net::maxNetREPLMechanismNameBytes)) {
    case FieldRead::Incomplete: return false;
    case FieldRead::Failed:     dropConnection(c); return false;
    case FieldRead::Complete:   break;
    }
    st->mechanism = std::move(st->field.data);
    st->named     = true;
    st->field     = SizedField();
  }

  switch (readSizedField(c, &st->field, reg.maxTokenBytes)) {
  case FieldRead::Incomplete: return false;
  case FieldRead::Failed:     dropConnection(c); return false;
  case FieldRead::Complete:   break;
  }
  const std::string token = std::move(st->field.data);
  st->field = SizedField();

  // a client set up for another mechanism is told so, once its first token
  // has been read: closing with that token still unread would reset the
  // connection and could take the answer with it
  if (reg.namesMechanism && st->mechanism != reg.mechanism) {
    sendHandshakeReply(c, netREPLAuthWrongMechanism);
    dropConnection(c);
    return false;
  }

  auto        result = NetREPLAuthSession::Result::Reject;
  std::string reply;
  try {
    if (!st->session) {
      st->session = reg.authenticator->newSession(c);
    }
    if (st->session) {
      result = st->session->step(token, &reply);
      if (result == NetREPLAuthSession::Result::Accept) {
        *identity = st->session->peerIdentity();
      }
    }
  } catch (...) {
    // whatever an authenticator throws refuses the peer; nothing of it may
    // reach the event loop
    result = NetREPLAuthSession::Result::Reject;
  }

  // the client stops after this many rounds too; a session that never
  // settles is not left to trade tokens with a peer forever
  if (result == NetREPLAuthSession::Result::Continue &&
      ++st->rounds >= net::maxNetREPLAuthRounds) {
    result = NetREPLAuthSession::Result::Reject;
  }

  switch (result) {
  case NetREPLAuthSession::Result::Continue:
    if (!sendHandshakeReply(c, netREPLAuthContinue, &reply)) {
      dropConnection(c);
    }
    return false; // the event loop says when the next token arrives
  case NetREPLAuthSession::Result::Accept:
    if (!(reply.empty() ? sendHandshakeReply(c, netREPLAuthAccepted)
                        : sendHandshakeReply(c, netREPLAuthAcceptedWithToken, &reply))) {
      dropConnection(c);
      return false;
    }
    return true;
  case NetREPLAuthSession::Result::Reject:
    break;
  }
  sendHandshakeReply(c, netREPLAuthRejected);
  dropConnection(c);
  return false;
}

void netREPLConnection(int c, void *d) {
  auto* registration = reinterpret_cast<NetREPLRegistration *>(d);
  HandshakeStatePtr st = pendingHandshake(c);
  if (!st) {
    readNetREPLRequests(c, registration->server);
    return;
  }

  if (!readVersionWord(c, st.get())) {
    return;
  }

  std::string identity;
  if (!registration->authenticator) {
    if (st->word != net::netREPLVersion) {
      dropConnection(c);
      return;
    }
  } else if (!authenticatePeer(c, *registration, st.get(), &identity)) {
    return;
  }

  forgetHandshake(c);

  try {
    if (registration->authenticator) {
      registration->server->authenticated(c, registration->mechanism, identity);
    }
    registration->server->connect(c);
  } catch (...) {
    dropConnection(c);
  }
}

}

// For tests, which declare it themselves: set how many invocations may wait
// suspended for their arguments on each event loop, and return what it was.
// Not part of the API, and in no header.
size_t setNetREPLMaxSuspendedInvocations(size_t n) {
  return maxSuspendedInvocations.exchange(n);
}

// Likewise: how much of a connection's input may be buffered, and whether an
// invocation with no stack to wait on is decoded from what has arrived (which
// it is; a test of the queue for stacks needs it not to be).
size_t setNetREPLMaxBufferedInput(size_t n) {
  return maxBufferedInputBytes.exchange(n);
}
bool setNetREPLDirectInvocations(bool on) {
  return directWithoutStack.exchange(on);
}

// A ceiling on how long one blocking response write can hold the shared event
// loop, for the connections that still make them.
//
// A connection whose Server hands over compiled invocations -- the compiler's
// own, which installNetREPL(.., cc*, ..) and hi use -- never makes one: its
// replies are queued and sent as the socket takes them (serveBufferedConnection),
// so this does not apply to it. A Server that only implements evaluate() reads
// its argument and writes its reply on the socket itself, and the request
// handler writes its prepare replies to such a connection directly; for those,
// SO_SNDTIMEO makes a send to a peer that has stopped reading fail rather than
// block forever, and the connection is dropped. It limits each send() syscall,
// not a whole transfer, so it is a backstop for that path rather than a bound.
static const time_t netREPLSendTimeoutSeconds = 10;

static void registerNetREPL(int s, Server *svr, NetREPLRegistration described = NetREPLRegistration()) {
  auto* registration = new NetREPLRegistration(std::move(described));
  registration->server = svr;
  registerEventHandler(
      s,
      [](int s, void *d) {
        int c = accept(s, nullptr, nullptr);
        if (c != -1) {
          // best effort: if this cannot be set the connection still works, it
          // just keeps the prior unbounded-write behavior
          struct timeval sndTimeout;
          sndTimeout.tv_sec  = netREPLSendTimeoutSeconds;
          sndTimeout.tv_usec = 0;
          setsockopt(c, SOL_SOCKET, SO_SNDTIMEO, &sndTimeout, sizeof(sndTimeout));

#if defined(SO_NOSIGPIPE)
          // where there may be no MSG_NOSIGNAL to send with (macOS), and for the
          // plain writes a Server's evaluate() makes: replying to a peer that
          // has gone must fail, not raise SIGPIPE and end the process
          int noSigPipe = 1;
          setsockopt(c, SOL_SOCKET, SO_NOSIGPIPE, &noSigPipe, sizeof(noSigPipe));
#endif

          try {
            rememberHandshake(c);
            registerEventHandler(c, &netREPLConnection, d);
          } catch (std::exception &) {
            forgetHandshake(c);
            close(c);
          }
        }
      },
      registration);
}

int installNetREPL(int port, Server *svr) {
  // A netREPL can execute native code in this process. Keep the convenient
  // port-only API local to this host; callers that deliberately expose it
  // must choose a listening address through the host overload below.
  int s = allocateServer(port, "127.0.0.1");
  registerNetREPL(s, svr);
  return s;
}

int installNetREPL(const std::string &host, int port, Server *svr) {
  int s = allocateServer(port, host);
  registerNetREPL(s, svr);
  return s;
}

int installNetREPL(const std::string &filepath, Server *svr) {
  int s = allocateFileSocketServer(filepath);
  registerNetREPL(s, svr);
  return s;
}

// the authenticator is checked before a listening socket is allocated for it
int installNetREPL(int port, Server *svr,
                   const std::shared_ptr<NetREPLAuthenticator>& authenticator) {
  auto described = describeAuthenticator(authenticator);
  int s = allocateServer(port, "127.0.0.1");
  registerNetREPL(s, svr, std::move(described));
  return s;
}

int installNetREPL(const std::string &host, int port, Server *svr,
                   const std::shared_ptr<NetREPLAuthenticator>& authenticator) {
  auto described = describeAuthenticator(authenticator);
  int s = allocateServer(port, host);
  registerNetREPL(s, svr, std::move(described));
  return s;
}

int installNetREPL(const std::string &filepath, Server *svr,
                   const std::shared_ptr<NetREPLAuthenticator>& authenticator) {
  auto described = describeAuthenticator(authenticator);
  int s = allocateFileSocketServer(filepath);
  registerNetREPL(s, svr, std::move(described));
  return s;
}

std::shared_ptr<NetREPLAuthenticator> sharedSecretAuthenticator(const std::string& sharedSecret) {
  validateSharedSecret(sharedSecret);
  return std::make_shared<SharedSecretAuthenticator>(sharedSecret);
}

int installNetREPL(int port, Server *svr,
                   const std::string& sharedSecret) {
  return installNetREPL(port, svr, sharedSecretAuthenticator(sharedSecret));
}

int installNetREPL(const std::string &host, int port, Server *svr,
                   const std::string& sharedSecret) {
  return installNetREPL(host, port, svr, sharedSecretAuthenticator(sharedSecret));
}

int installNetREPL(const std::string &filepath, Server *svr,
                   const std::string& sharedSecret) {
  return installNetREPL(filepath, svr, sharedSecretAuthenticator(sharedSecret));
}

class CCServer : public Server, public BufferedInvoker {
public:
  CCServer(cc *c, ReWriteExprFn const &wrExprFn) : c(c), wrExprFn(wrExprFn) {}

  void connect(int) override {}

  ExprPtr readExpr(const std::string &x) override { return this->c->readExpr(x); }

  MonoTypePtr prepare(int c, exprid eid, const ExprPtr &expr,
                      const MonoTypePtr &inty) override {
    const auto &la = expr->la();

    // E(readFrom(in)::T) :: ?
    MonoTypePtr rty = requireMonotype(
        this->c
            ->unsweetenExpression(fncall(
                wrExprFn(expr),
                list(assume(fncall(var("readFrom", la),
                                   list(constant(static_cast<int>(0), la)), la),
                            inty, la)),
                la))
            ->type());

    // let x = readFrom(input) :: T in writeTo(output, E(x))
    this->cnetFns[c][eid] = this->c->compileFn<void(int)>(
        ".c", let(".in",
                  assume(fncall(var("readFrom", la), list(var(".c", la)), la),
                         inty, la),
                  fncall(var("writeTo", la),
                         list(var(".c", la),
                              fncall(wrExprFn(expr), list(var(".in", la)), la)),
                         la),
                  la));

    return rty;
  }

  NetFn invocation(int c, exprid eid) override {
    auto cfns = this->cnetFns.find(c);
    if (cfns == this->cnetFns.end()) {
      return nullptr;
    }
    auto f = cfns->second.find(eid);
    return f == cfns->second.end() ? nullptr : f->second;
  }

  void evaluate(int c, exprid eid) override {
    auto cfns = this->cnetFns[c];
    auto f = cfns.find(eid);

    if (f != cfns.end()) {
      // perform the call
      f->second(c);
    } else {
      // invalid expression, disconnect (handler first, then the descriptor)
      unregisterEventHandler(c);
      close(c);
      disconnect(c);
    }
  }

  void disconnect(int) override {}

private:
  cc *c;

  using NetFns = std::map<exprid, NetFn>;
  using ConnNetFns = std::map<int, NetFns>;
  ConnNetFns cnetFns;
  ReWriteExprFn wrExprFn;
};

int installNetREPL(int port, cc *c, ReWriteExprFn const &wrExprFn) {
  return installNetREPL(port, new CCServer(c, wrExprFn));
}
int installNetREPL(const std::string &host, int port, cc *c,
                   ReWriteExprFn const &wrExprFn) {
  return installNetREPL(host, port, new CCServer(c, wrExprFn));
}

int installNetREPL(const std::string &filepath, cc *c,
                   ReWriteExprFn const &wrExprFn) {
  return installNetREPL(filepath, new CCServer(c, wrExprFn));
}

// the authenticator is checked, and the listening socket allocated, before the
// server object is: one that cannot be installed must not leave a CCServer behind
int installNetREPL(int port, cc *c,
                   const std::shared_ptr<NetREPLAuthenticator>& authenticator,
                   ReWriteExprFn const &wrExprFn) {
  auto described = describeAuthenticator(authenticator);
  int s = allocateServer(port, "127.0.0.1");
  registerNetREPL(s, new CCServer(c, wrExprFn), std::move(described));
  return s;
}

int installNetREPL(const std::string &host, int port, cc *c,
                   const std::shared_ptr<NetREPLAuthenticator>& authenticator,
                   ReWriteExprFn const &wrExprFn) {
  auto described = describeAuthenticator(authenticator);
  int s = allocateServer(port, host);
  registerNetREPL(s, new CCServer(c, wrExprFn), std::move(described));
  return s;
}

int installNetREPL(const std::string &filepath, cc *c,
                   const std::shared_ptr<NetREPLAuthenticator>& authenticator,
                   ReWriteExprFn const &wrExprFn) {
  auto described = describeAuthenticator(authenticator);
  int s = allocateFileSocketServer(filepath);
  registerNetREPL(s, new CCServer(c, wrExprFn), std::move(described));
  return s;
}

int installNetREPL(int port, cc *c, const std::string& sharedSecret,
                   ReWriteExprFn const &wrExprFn) {
  return installNetREPL(port, c, sharedSecretAuthenticator(sharedSecret), wrExprFn);
}

int installNetREPL(const std::string &host, int port, cc *c,
                   const std::string& sharedSecret,
                   ReWriteExprFn const &wrExprFn) {
  return installNetREPL(host, port, c, sharedSecretAuthenticator(sharedSecret), wrExprFn);
}

int installNetREPL(const std::string &filepath, cc *c,
                   const std::string& sharedSecret,
                   ReWriteExprFn const &wrExprFn) {
  return installNetREPL(filepath, c, sharedSecretAuthenticator(sharedSecret), wrExprFn);
}

// connect to a running net REPL
Client::Client(const std::string &hostport)
    : hostport(hostport), eid(0), rbno(0), reno(0) {
  this->c = connectSocket(hostport);
  fdwrite(this->c, net::netREPLVersion);
}

// the secret is checked before a connection is opened for it
Client::Client(const std::string &hostport, const std::string& sharedSecret)
    : Client(hostport, net::NetREPLSharedSecret(sharedSecret)) {}

// The handshake (net::authenticateSession) sends its opening in one send that
// cannot raise SIGPIPE. A listener that refuses it (one without an
// authenticator sees a version word it does not speak and hangs up) can close
// the connection while the first token is still being written; Linux has no
// SO_NOSIGPIPE, so a plain write() there killed the client process instead of
// letting it report the refusal.
Client::Client(const std::string &hostport, const net::NetREPLClientAuthenticator& auth)
    : hostport(hostport), eid(0), rbno(0), reno(0) {
  this->c = connectSocket(hostport);
  try {
    net::authenticateSession(this->c, auth, hostport);
  } catch (...) {
    close(this->c);
    throw;
  }
}

Client::~Client() { close(this->c); }

const std::string &Client::remoteHost() const { return this->hostport; }

// Asking a peer for the type of an expression happens while the constraint
// is being resolved, which is part of compiling it -- so the thread waiting
// on the answer is holding the process-wide compiler lock (hlock, cc.C) for
// as long as it waits. Anything that keeps the peer from answering therefore
// stops every thread in this process from compiling, permanently.
//
// The sharpest case is a peer served by this same process: answering means
// compiling the expression that was sent (CCServer::prepare below), which
// needs the lock this thread is holding, so the reply can never come. That
// used to wedge the process with no error and no way out. A peer that is
// gone, wedged or firewalled does the same thing.
//
// So bound the exchange. This covers only the compile-time query: reads made
// while evaluating a remote call are left alone, since a remote call may
// legitimately take as long as it likes.
//
// The bound is applied to each send and receive, and the total is checked
// once the exchange ends. It is not a hard deadline on the exchange as a
// whole: a peer that keeps dribbling bytes inside one message restarts the
// per-operation timer, and only trips the total on the next operation.
static std::atomic<long> remoteTypeQueryTimeoutMS{30000};

void setRemoteTypeQueryTimeoutMS(long ms) { remoteTypeQueryTimeoutMS.store(ms); }
long getRemoteTypeQueryTimeoutMS() { return remoteTypeQueryTimeoutMS.load(); }

namespace {

// bound each send and receive on a socket for as long as this is in scope
struct ScopedIOTimeout {
  int     fd;
  timeval prevRcv{};
  timeval prevSnd{};
  bool    applied = false;

  explicit ScopedIOTimeout(int fd) : fd(fd) {
    long ms = remoteTypeQueryTimeoutMS.load();
    if (ms <= 0) {
      return; // disabled: wait forever, as it used to
    }

    // if the old value cannot be read, assume there was none rather than
    // leaving the wait unbounded (which is the thing being fixed)
    socklen_t plen = sizeof(this->prevRcv);
    if (getsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &this->prevRcv, &plen) != 0) {
      this->prevRcv = timeval{};
    }
    plen = sizeof(this->prevSnd);
    if (getsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &this->prevSnd, &plen) != 0) {
      this->prevSnd = timeval{};
    }

    timeval tv{};
    tv.tv_sec  = static_cast<time_t>(ms / 1000);
    tv.tv_usec = static_cast<suseconds_t>((ms % 1000) * 1000);
    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
    this->applied = true;
  }

  ~ScopedIOTimeout() {
    if (this->applied) {
      setsockopt(this->fd, SOL_SOCKET, SO_RCVTIMEO, &this->prevRcv, sizeof(this->prevRcv));
      setsockopt(this->fd, SOL_SOCKET, SO_SNDTIMEO, &this->prevSnd, sizeof(this->prevSnd));
    }
  }

  ScopedIOTimeout(const ScopedIOTimeout&) = delete;
  ScopedIOTimeout& operator=(const ScopedIOTimeout&) = delete;
};

bool timedOut(int e) { return e == EAGAIN || e == EWOULDBLOCK; }

}

void Client::retire(const std::string& why) {
  this->usable      = false;
  this->unusableWhy = why;
  // Deliberately no shutdown()/close() here. Refusing to use the connection
  // again is what keeps the abandoned reply from being read as the answer to
  // the next question, and that is enough. Tearing the socket down would
  // make the peer's write fail with EPIPE, and the library does not ignore
  // SIGPIPE (only hi's main does, main.C), so it could take an embedder --
  // or, when the peer is served by this same process, this one -- down with
  // it. The descriptor is released when the Client is destroyed.
}

void Client::requireUsable() const {
  if (!this->usable) {
    throw std::runtime_error(
      "This connection to '" + this->hostport + "' was retired and cannot be used again: " +
      this->unusableWhy);
  }
}

exprid Client::remoteExpr(const ExprPtr &expr, const MonoTypePtr &inty) {
  // have we already exchanged this exprty?
  ExprTy exprty(expr.get(), inty.get());
  auto etid = this->exprTyToID.find(exprty);
  if (etid != this->exprTyToID.end()) {
    return etid->second;
  }

  requireUsable();

  // first we send the ID, expression, and the input type
  exprid rid = ++this->eid;

  RawData exprd;
  encode(expr, &exprd);

  RawData intyd;
  encode(inty, &intyd);

  ScopedIOTimeout   _io(this->c);
  const auto        started = std::chrono::steady_clock::now();
  const long        budgetMS = remoteTypeQueryTimeoutMS.load();

  auto elapsedMS = [&]() -> long {
    return static_cast<long>(std::chrono::duration_cast<std::chrono::milliseconds>(
             std::chrono::steady_clock::now() - started).count());
  };

  // any exchange that does not run to completion leaves this socket with an
  // unread reply on it, so the connection cannot be used again
  auto failed = [&](const char* what, const std::exception& ex) -> std::runtime_error {
    const bool late = timedOut(errno) || (budgetMS > 0 && elapsedMS() >= budgetMS);
    std::string why =
      late
        ? ("'" + this->hostport + "' did not answer a compile-time type query within " +
           str::from(budgetMS) + "ms. That wait holds the compiler lock, so it is bounded rather "
           "than left to block every thread in this process. A net REPL served by this same "
           "process can never answer, because compiling the reply needs the lock the waiting "
           "thread already holds.")
        : ("the connection to '" + this->hostport + "' failed during a compile-time type query (" +
           ex.what() + ")");
    this->retire(why);
    return std::runtime_error(std::string(what) + ": " + why);
  };

  try {
    fdwrite(this->c, uint8_t(1));
    fdwrite(this->c, rid);
    fdwrite(this->c, exprd);
    fdwrite(this->c, intyd);
  } catch (std::exception& ex) {
    throw failed("Could not send a compile-time type query", ex);
  }

  // then we expect to get back a result type
  uint8_t v = 0;
  try {
    fdread(this->c, &v);
  } catch (std::exception& ex) {
    throw failed("No answer to a compile-time type query", ex);
  }

  if (v == 1) {
    RawData outtyd;
    try {
      fdread(this->c, &outtyd);
    } catch (std::exception& ex) {
      throw failed("Truncated answer to a compile-time type query", ex);
    }

    ExprDef &ed = this->exprDefs[rid];
    ed.expr = expr;
    ed.inty = inty;
    ed.outty = decode(outtyd);

    this->exprTyToID[exprty] = rid;

    return rid;
  } else if (v == 0) {
    std::string errmsg;
    try {
      fdread(this->c, &errmsg);
    } catch (std::exception& ex) {
      throw failed("Truncated error from a compile-time type query", ex);
    }
    throw std::runtime_error("Error from server: " + errmsg);
  } else {
    // the stream is no longer where we think it is
    this->retire("'" + this->hostport + "' sent a malformed answer to a compile-time type query");
    throw std::runtime_error("Received malformed message from server");
  }
}

MonoTypePtr Client::input(exprid ex) const {
  auto ed = this->exprDefs.find(ex);
  if (ed == this->exprDefs.end()) {
    throw std::runtime_error(
        "Remote process has no compiled expression with id=" + str::from(ex));
  } else {
    return ed->second.inty;
  }
}

MonoTypePtr Client::output(exprid ex) const {
  auto ed = this->exprDefs.find(ex);
  if (ed == this->exprDefs.end()) {
    throw std::runtime_error(
        "Remote process has no compiled expression with id=" + str::from(ex));
  } else {
    return ed->second.outty;
  }
}

MonoTypePtr Client::output(const ExprPtr &e, const MonoTypePtr &inty) {
  return output(remoteExpr(e, inty));
}

void Client::show(std::ostream &out) const {
  out << this->hostport << "\n\n";
  str::seqs cs;
  cs.resize(4);

  cs[0].push_back("id");
  for (const auto &ce : this->exprDefs) {
    cs[0].push_back(str::from(ce.first));
  }

  cs[1].push_back("expr");
  for (const auto &ce : this->exprDefs) {
    cs[1].push_back(hobbes::show(ce.second.expr));
  }

  cs[2].push_back("input");
  for (const auto &ce : this->exprDefs) {
    cs[2].push_back(hobbes::show(ce.second.inty));
  }

  cs[3].push_back("output");
  for (const auto &ce : this->exprDefs) {
    cs[3].push_back(hobbes::show(ce.second.outty));
  }

  str::printRightAlignedTable(out, cs);
}

// read response values from the other end of the connection
size_t Client::appendReadFn(ReadFn f) {
  this->readFns.push(f);
  return this->reno++;
}

char *Client::readValue(size_t x) {
  if (x < this->rbno) {
    // too late, we've already read past this
    std::cerr << "Can't read remote value out of sequence." << std::endl;
    abort();
  } else {
    while (x > this->rbno) {
      this->readFns.front()(this->c);
      this->readFns.pop();
      ++this->rbno;
    }

    char *r = this->readFns.front()(this->c);
    this->readFns.pop();
    ++this->rbno;

    // if something happened to the socket, kill the process (too severe?)
    if (unmarkBadFD(this->c)) {
      std::cerr << "I/O error on socket" << std::endl;
      abort();
    }

    // well if we got here then we read the requested value (finally)
    return r;
  }
}

size_t Client::unsafeAppendReadFn(size_t p, ReadFn f) {
  auto *c = reinterpret_cast<Client *>(p);
  if (!isAllocatedConnection(c)) {
    throw std::runtime_error(
        "unsafeAppendClientReadFn: handle is not a live connection");
  }
  return c->appendReadFn(f);
}

char *Client::unsafeRead(size_t p, size_t x) {
  auto *c = reinterpret_cast<Client *>(p);
  if (!isAllocatedConnection(c)) {
    throw std::runtime_error("unsafeClientRead: handle is not a live connection");
  }
  return c->readValue(x);
}

} // namespace hobbes
