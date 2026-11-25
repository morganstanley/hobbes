/********
 * net : I/O for a "net REPL" between processes
 ********/

#ifndef HOBBES_EVENTS_NET_HPP_INCLUDED
#define HOBBES_EVENTS_NET_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/netauth.H>
#include <memory>
#include <string>
#include <map>
#include <queue>

namespace hobbes {

// get the port number associated with a named port
int lookupPort(const std::string&);

// create a socket listening on a port
int allocateServer(int port, const std::string& host = "");
int allocateServer(const std::string& port);

// connect to a host/port
int connectSocket(const std::string& host, int port);
int connectSocket(const std::string& hostport);

// create a unix domain socket backed by a file
int allocateFileSocketServer(const std::string& filepath);

// get the name of the host on the other end of this socket
std::string remoteHostname(int socket);

// get the port on the remote end of this socket
int remotePort(int socket);

// run a REPL server at some port, either indirectly or directly with a compiler
using RawData = std::vector<uint8_t>;
using exprid = uint32_t;
using ReWriteExprFn = std::function<ExprPtr (const ExprPtr &)>;

struct Server {
  virtual void        connect   (int conn) = 0;
  virtual ExprPtr     readExpr  (const std::string&) = 0;
  virtual MonoTypePtr prepare   (int conn, exprid, const ExprPtr&, const MonoTypePtr&) = 0;
  virtual void        evaluate  (int conn, exprid) = 0;
  virtual void        disconnect(int conn) = 0;

  // A peer has authenticated on this connection; called just before connect()
  // on a listener installed with a shared secret or a NetREPLAuthenticator,
  // and never on one installed without. 'mechanism' is the authenticator's
  // name and 'identity' what its session reported for this peer (empty for a
  // shared secret, which proves knowledge of the secret and nothing about who
  // holds it). Throwing here drops the connection without calling connect --
  // or disconnect, as when connect itself throws, so forget anything noted
  // for 'conn' before throwing.
  virtual void        authenticated(int /*conn*/, const std::string& /*mechanism*/, const std::string& /*identity*/) {}
};

// The port-only overload listens on IPv4 loopback. Use the host overload to
// deliberately select another interface.
int installNetREPL(int port, Server*);
int installNetREPL(const std::string& host, int port, Server*);
// install a net repl on a unix domain socket (using file paths)
int installNetREPL(const std::string& /*filepath*/, Server*);

// The server's half of one authentication handshake, on one connection. The
// handshake is an exchange of opaque tokens, begun by the client; the event
// loop reads each client token without blocking and calls step() once a whole
// one has arrived.
//
// step() runs on the event loop that serves every listener in the process, so
// it must not block: no network calls, no waiting on another process. Work
// from the token and local state (a keytab, a key, a table of tokens).
struct NetREPLAuthSession {
  virtual ~NetREPLAuthSession() = default;

  enum class Result {
    Continue, // send *reply; the client must answer with another token
    Accept,   // the peer is authenticated; *reply, if not empty, is sent with the acceptance
    Reject    // drop the peer
  };
  // Throwing rejects the peer.
  virtual Result step(const std::string& token, std::string* reply) = 0;

  // Who the peer is, read once after step() accepts and handed to
  // Server::authenticated. Decide here in step() whether that peer is allowed
  // at all: a Reject is reported to the client as a refused handshake.
  virtual std::string peerIdentity() const { return std::string(); }
};

// How a listener authenticates its peers. One of these serves every connection
// to the listener; newSession() is called once for each.
struct NetREPLAuthenticator {
  virtual ~NetREPLAuthenticator() = default;

  // Names the mechanism; a client must use a NetREPLClientAuthenticator with
  // the same name. Read once, when the listener is installed. Must not be
  // empty, and at most net::maxNetREPLMechanismNameBytes.
  virtual std::string mechanism() const = 0;

  // The largest client token to buffer, at most
  // net::maxNetREPLAuthTokenBytesLimit. A peer that announces a larger one is
  // dropped before any of it is read. Read once, when the listener is installed.
  virtual size_t maxTokenBytes() const { return net::defaultMaxNetREPLAuthTokenBytes; }

  // 'conn' is the accepted connection, for inspection only (getpeername,
  // SO_PEERCRED on a unix socket): do not read from, write to or close it.
  // Returning null or throwing rejects the peer.
  virtual std::unique_ptr<NetREPLAuthSession> newSession(int conn) = 0;
};

// Authenticated variants require the client to complete the authenticator's
// handshake before Server::connect or any expression decoding occurs. This is
// authentication of the peer, not encryption of the session: for remote
// clients, terminate TLS in a same-host proxy and keep the Hobbes listener on
// loopback. See doc/en/netrepl-custom-authentication.md.
int installNetREPL(int port, Server*, const std::shared_ptr<NetREPLAuthenticator>&);
int installNetREPL(const std::string& host, int port, Server*, const std::shared_ptr<NetREPLAuthenticator>&);
int installNetREPL(const std::string& /*filepath*/, Server*, const std::shared_ptr<NetREPLAuthenticator>&);

// The built-in authenticator: the client proves knowledge of a shared secret
// (net::NetREPLSharedSecret on the client). The overloads taking a secret
// install exactly this.
std::shared_ptr<NetREPLAuthenticator> sharedSecretAuthenticator(const std::string& sharedSecret);
int installNetREPL(int port, Server*, const std::string& sharedSecret);
int installNetREPL(const std::string& host, int port, Server*, const std::string& sharedSecret);
int installNetREPL(const std::string& /*filepath*/, Server*, const std::string& sharedSecret);

class cc;
// The port-only overload listens on IPv4 loopback. Use the host overload to
// deliberately select another interface.
int installNetREPL(int port, cc*, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });
int installNetREPL(const std::string& host, int port, cc*, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });
// install a net repl on a unix domain socket (using file paths)
int installNetREPL(const std::string& /*filepath*/, cc*, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });

int installNetREPL(int port, cc*, const std::string& sharedSecret, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });
int installNetREPL(const std::string& host, int port, cc*, const std::string& sharedSecret, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });
int installNetREPL(const std::string& /*filepath*/, cc*, const std::string& sharedSecret, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });

int installNetREPL(int port, cc*, const std::shared_ptr<NetREPLAuthenticator>&, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });
int installNetREPL(const std::string& host, int port, cc*, const std::shared_ptr<NetREPLAuthenticator>&, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });
int installNetREPL(const std::string& /*filepath*/, cc*, const std::shared_ptr<NetREPLAuthenticator>&, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });

// How long to wait for a peer to answer a compile-time type query (the
// (Connect ...)/(Invoke ...) exchanges), in milliseconds; 0 or less waits
// forever, as it used to. This wait happens with the process-wide compiler
// lock held, so an unbounded one stops every thread in the process from
// compiling -- always, if the peer is served by this same process, since
// answering needs that very lock. Reads made while *evaluating* a remote
// call are not affected. Defaults to 30s.
void setRemoteTypeQueryTimeoutMS(long ms);
long getRemoteTypeQueryTimeoutMS();

// connect to a running net REPL somewhere
class Client {
public:
  /* init/compile-time methods */
  Client(const std::string& hostport);
  Client(const std::string& hostport, const std::string& sharedSecret);
  // authenticate with any mechanism; the authenticator is used only during construction
  Client(const std::string& hostport, const net::NetREPLClientAuthenticator&);
  ~Client();
  const std::string& remoteHost() const;

  exprid remoteExpr(const ExprPtr&, const MonoTypePtr& inty);

  MonoTypePtr input(exprid) const;
  MonoTypePtr output(exprid) const;
  MonoTypePtr output(const ExprPtr&, const MonoTypePtr& inty);

  inline int fd() const { return this->c; }
private:
  int         c;
  std::string hostport;
  exprid      eid;

  // A compile-time query that does not complete leaves the peer's reply, or
  // the tail of it, still to come on this socket. Reusing the connection
  // after that would read the abandoned reply as the answer to the next
  // question, so an abandoned exchange retires the connection instead.
  bool        usable = true;
  std::string unusableWhy;
  void retire(const std::string& why);
  void requireUsable() const;

  struct ExprDef {
    ExprPtr     expr;
    MonoTypePtr inty;
    MonoTypePtr outty;
  };
  using ExprDefs = std::map<exprid, ExprDef>;
  ExprDefs exprDefs;

  using ExprTy = std::pair<const void *, const void *>;
  using ExprTyToID = std::map<ExprTy, exprid>;
  ExprTyToID exprTyToID;
public:
  /* run-time methods */
  void show(std::ostream&) const;

  // append a receive function to the end of the sequence of receive handlers
  // these functions will be applied in order to read values from the remote process
  using ReadFn = char *(*)(int);
  size_t appendReadFn(ReadFn);

  // read a result (reading and discarding any intermediate results)
  char* readValue(size_t);

  // only used by generated code
  static size_t unsafeAppendReadFn(size_t, ReadFn);
  static char* unsafeRead(size_t, size_t);
private:
  using ReadFns = std::queue<ReadFn>;
  ReadFns readFns;
  size_t rbno;
  size_t reno;
};

// true iff c points to a Client created by makeConnection and still registered
// (defined with the connection registry in lib/hobbes/ipc/nbindings.C)
bool isAllocatedConnection(Client*);

}

#endif

