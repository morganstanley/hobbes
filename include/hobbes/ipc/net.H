/********
 * net : I/O for a "net REPL" between processes
 ********/

#ifndef HOBBES_EVENTS_NET_HPP_INCLUDED
#define HOBBES_EVENTS_NET_HPP_INCLUDED

#include <hobbes/lang/type.H>
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
};

int installNetREPL(int port, Server*);
int installNetREPL(const std::string& host, int port, Server*);
// install a net repl on a unix domain socket (using file paths)
int installNetREPL(const std::string& /*filepath*/, Server*);

class cc;
int installNetREPL(int port, cc*, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });
int installNetREPL(const std::string& host, int port, cc*, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });
// install a net repl on a unix domain socket (using file paths)
int installNetREPL(const std::string& /*filepath*/, cc*, ReWriteExprFn const& = [](ExprPtr const& e) -> ExprPtr { return e; });

// connect to a running net REPL somewhere
class Client {
public:
  /* init/compile-time methods */
  Client(const std::string& hostport);
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

}

#endif

