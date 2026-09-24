
#include <hobbes/hobbes.H>
#include <hobbes/ipc/net.H>
#include <hobbes/net.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/str.H>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <map>
#include <mutex>
#include <sstream>
#include <vector>

#include <cstring>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

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
// to the peer over the same socket and keep the connection.
bool processNetREPLRequest(Server *s, int c, const char *b, size_t n) {
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
  auto readBlob = [&](RawData *out) {
    size_t l = 0;
    readPrim(&l, sizeof(l));
    need(l);
    out->assign(b + off, b + off + l);
    off += l;
  };

  try {
    uint8_t cmd = 0;
    readPrim(&cmd, sizeof(cmd));

    switch (cmd) {
    case 0:
      // prepare a lexical expression with input and output types given
      try {
        // the type descriptions read below are interned in the process-wide
        // type memo whether or not they are accepted; release what this
        // request does not end up keeping, on every way out of here
        CompactMTypeMemoryAtExit compactAfter;

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
        fdwrite(c, uint8_t(1));
      } catch (std::exception &ex) {
        fdwrite(c, uint8_t(0));
        fdwrite(c, std::string(ex.what()));
      }
      break;
    case 1:
      // prepare a serialized expression, also return its type
      try {
        CompactMTypeMemoryAtExit compactAfter;

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
        fdwrite(c, uint8_t(1));
        fdwrite(c, rtyd);
      } catch (std::exception &ex) {
        fdwrite(c, uint8_t(0));
        fdwrite(c, std::string(ex.what()));
      }
      break;
    case 2: {
      // invoke a prepared expression
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
// The read is MSG_DONTWAIT rather than O_NONBLOCK on the descriptor. A peer
// that sends the version word and a command in one packet is not left waiting:
// the epoll set is level triggered, so the bytes still unread wake this handler
// again -- for the handshake here, and for the request bytes that follow it.
namespace {

struct HandshakeState {
  uint32_t word = 0;
  size_t   have = 0;
};

std::mutex                    handshakeMutex;
std::map<int, HandshakeState> handshakes;

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
// Only a request *header* is buffered here -- the command byte, the eid, and,
// for a prepare, the serialized expression and type descriptions. Those are
// small: a serialized expression or type is orders of magnitude under this
// limit, so a real request never approaches it. Invocation argument data, the
// one payload that can legitimately be large, is not buffered here at all: it
// is read straight off the socket by the compiled call, so this cap does not
// constrain it. (CWE-400 is the finding's own class, so bounding the buffer is
// on point rather than a new restriction out of nowhere.)
const size_t maxBufferedRequestBytes = 64 * 1024 * 1024;

bool handshakePending(int c, HandshakeState* st) {
  std::lock_guard<std::mutex> lk(handshakeMutex);
  auto i = handshakes.find(c);
  if (i == handshakes.end()) {
    return false;
  }
  *st = i->second;
  return true;
}

void rememberHandshake(int c, const HandshakeState& st) {
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
  unregisterEventHandler(c);
  close(c);
}

// read what there is of the version word; true once all four bytes are in
bool readVersionWord(int c, HandshakeState* st) {
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

  if (st->have < sizeof(st->word)) {
    rememberHandshake(c, *st); // a piece of it; wait for the rest
    return false;
  }
  return true;
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

FrameScan scanRequestFrame(const std::string& b, size_t* frameLen, size_t* want) {
  size_t off = 0;

  // a fixed-width field of k bytes
  auto fixed = [&](size_t k) {
    size_t have = b.size() - off;
    if (have < k) { *want = k - have; return false; }
    off += k;
    return true;
  };
  // a size_t length prefix followed by that many payload bytes
  auto sized = [&]() {
    size_t have = b.size() - off;
    if (have < sizeof(size_t)) { *want = sizeof(size_t) - have; return false; }
    size_t len = 0;
    std::memcpy(&len, b.data() + off, sizeof(len));
    size_t payloadHave = b.size() - off - sizeof(size_t);
    if (payloadHave < len) { *want = len - payloadHave; return false; }
    off += sizeof(size_t) + len;
    return true;
  };

  if (b.empty()) {
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

// serve a connection whose handshake is done. Read the next request's header a
// field at a time without ever blocking, and only when a whole header has
// arrived hand it to processNetREPLRequest. Crucially, no more than the current
// field's outstanding bytes are read at a time, so a cmd-2 invocation's
// arguments -- which the compiled call reads from the socket itself -- are left
// untouched in the socket rather than swallowed into this buffer.
//
// A peer that sends part of a request and then stops no longer parks the shared
// event loop in read(): its bytes wait in a per-connection buffer and every
// other connection keeps being served (STRFR-434002).
//
// This change covers the finding's enumerated reads -- cmd, eid, expr, type.
// Two same-class stalls on this path remain, both a larger change than the
// request reader and called out here so they are a stated boundary, not a
// silent gap:
//
//   - Invocation arguments. A cmd-2 invoke whose eid resolves reads its
//     argument bytes synchronously in compiled code (CCServer::evaluate -> the
//     prepared NetFn), so a peer that sends cmd+eid and withholds the arguments
//     can still stall the loop. Making argument deserialization resumable means
//     reworking the generated read path.
//
//   - Response writes. Replies (the status byte and error text here, and the
//     compiled result of an invoke) go back with blocking fdwrite/writes. A
//     peer that sends a whole request but never reads the reply can, once its
//     receive window and the local send buffer fill, park the loop in write()
//     just as the read did. Non-blocking responses need an outbound queue and
//     EPOLLOUT handling.
//
// Both are the write/eval half of the same event-loop-blocking class and are
// better tracked as their own follow-up than folded into the request reader.
void readNetREPLRequests(int c, void *d) {
  auto *s = reinterpret_cast<Server *>(d);

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
    FrameScan sc = scanRequestFrame(buf, &frameLen, &want);

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

void netREPLConnection(int c, void *d) {
  HandshakeState st;
  if (!handshakePending(c, &st)) {
    readNetREPLRequests(c, d);
    return;
  }

  if (!readVersionWord(c, &st)) {
    return;
  }

  forgetHandshake(c);

  if (st.word != 0x00010000) {
    // the peer speaks another protocol: drop it here rather than falling
    // through to serve a connection that cannot be understood
    unregisterEventHandler(c);
    close(c);
    return;
  }

  try {
    reinterpret_cast<Server *>(d)->connect(c);
  } catch (std::exception &) {
    unregisterEventHandler(c);
    close(c);
  }
}

}

void registerNetREPL(int s, Server *svr) {
  registerEventHandler(
      s,
      [](int s, void *d) {
        int c = accept(s, nullptr, nullptr);
        if (c != -1) {
          try {
            rememberHandshake(c, HandshakeState());
            registerEventHandler(c, &netREPLConnection, d);
          } catch (std::exception &) {
            forgetHandshake(c);
            close(c);
          }
        }
      },
      svr);
}

int installNetREPL(int port, Server *svr) {
  int s = allocateServer(port);
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

class CCServer : public Server {
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

  using NetFn = void (*)(int); // socket -> ()
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

// connect to a running net REPL
Client::Client(const std::string &hostport)
    : hostport(hostport), eid(0), rbno(0), reno(0) {
  this->c = connectSocket(hostport);
  fdwrite(this->c, static_cast<uint32_t>(0x00010000));
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
