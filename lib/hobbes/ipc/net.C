
#include <hobbes/ipc/net.H>
#include <hobbes/hobbes.H>
#include <hobbes/util/str.H>
#include <hobbes/util/codec.H>

#include <sstream>

#include <sys/types.h>
#include <sys/un.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

namespace hobbes {

// get the port number associated with a named port
int lookupPort(const std::string& x) {
  if (str::is<int>(x)) {
    return str::to<int>(x);
  } else {
    struct servent* s = getservbyname(x.c_str(), "tcp");
    if (s != 0) {
      return ntohs(s->s_port);
    } else {
      throw std::runtime_error("Failed to resolve port name: " + x);
    }
  }
}

// create a listening socket on a given port
int allocateServer(int port) {
  int s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (s == -1) {
    throw std::runtime_error("Unable to allocate socket: " + std::string(strerror(errno)));
  }

  // make sure that we can quickly restart the server if necessary
  int ra = 1;
  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<char*>(&ra), sizeof(ra));

  // bind the new socket to a local address
  sockaddr_in laddr;
  memset(&laddr, 0, sizeof(laddr));

  laddr.sin_family      = AF_INET;
  laddr.sin_port        = htons(port);
  laddr.sin_addr.s_addr = INADDR_ANY;

  if (bind(s, reinterpret_cast<sockaddr*>(&laddr), sizeof(laddr)) == -1) {
    close(s);
    throw std::runtime_error("Unable to bind socket to local address: " + std::string(strerror(errno)));
  }

  // and then start to listen
  if (listen(s, SOMAXCONN) == -1) {
    close(s);
    throw std::runtime_error("Unable to listen on local address: " + std::string(strerror(errno)));
  }

  return s;
}

int allocateServer(const std::string& port) {
  return allocateServer(lookupPort(port));
}

int allocateFileSocketServer(const std::string& filepath) {
  int s = socket(AF_UNIX, SOCK_STREAM, 0);
  if (s == -1) {
    throw std::runtime_error("Unable to allocate socket: " + std::string(strerror(errno)));
  }
  
  sockaddr_un addr;
  memset(&addr, 0, sizeof(addr));
  addr.sun_family = AF_UNIX;
  unlink(filepath.c_str());
  snprintf(addr.sun_path, sizeof(addr.sun_path), "%s", filepath.c_str());

  if (bind(s, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) == -1) {
    close(s);
    throw std::runtime_error("Unable to bind socket to file: " + filepath + std::string(" : ") + std::string(strerror(errno)));
  }

  // and then start to listen
  if (listen(s, SOMAXCONN) == -1) {
    close(s);
    throw std::runtime_error("Unable to listen socket on file: " + filepath + std::string(" : ") + std::string(strerror(errno)));
  }

  return s;
}

int connectSocket(int r, sockaddr* saddr, size_t len) {
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

  if (select(r + 1, 0, &wd, 0, 0) == -1) {
    std::ostringstream ss;
    ss << "Failed to connect socket while waiting for writeability: " << strerror(errno) << std::flush;
    close(r);
    throw std::runtime_error(ss.str());
  }

  // we're connected!
  return r;
}

// create a connected socket to a remote process
int connectSocket(hostent* host, int port) {
  int r = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (r == -1) {
    throw std::runtime_error("Unable to allocate socket: " + std::string(strerror(errno)));
  }

  sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr   = *reinterpret_cast<in_addr*>(host->h_addr_list[0]);
  addr.sin_port   = htons(port);

  return connectSocket(r, reinterpret_cast<sockaddr*>(&addr), sizeof(addr));
}

int connectFileSocket(const std::string& filepath) {
  int r = socket(AF_UNIX, SOCK_STREAM, 0);
  if (r == -1) {
    throw std::runtime_error("Unable to allocate socket: " + std::string(strerror(errno)));
  }
  
  sockaddr_un addr;
  memset(&addr, 0, sizeof(addr));
  addr.sun_family = AF_UNIX;
  snprintf(addr.sun_path, sizeof(addr.sun_path), "%s", filepath.c_str());

  return connectSocket(r, reinterpret_cast<sockaddr*>(&addr), sizeof(addr));
}

int connectSocket(const std::string& host, int port) {
  if (host.size() > 0 && str::isDigit(host[0])) {
    return connectSocket(gethostbyaddr(host.c_str(), host.size(), AF_INET), port);
  } else {
    return connectSocket(gethostbyname(host.c_str()), port);
  }
}
      
int connectSocket(const std::string& hostport) {
  str::pair p = str::lsplit(hostport, ":");
  if (p.second.size() > 0)
    return connectSocket(p.first, lookupPort(p.second));
  else
    return connectFileSocket(p.first);
}

// get the name of the host on the other end of this socket
std::string remoteHostname(int c) {
  struct sockaddr_in sin;
  socklen_t len = sizeof(sin);
  if (getpeername(c, reinterpret_cast<struct sockaddr*>(&sin), &len) < 0) {
    throw std::runtime_error("Couldn't get peer name for socket");
  } else {
    if (struct hostent* host = gethostbyaddr(reinterpret_cast<char*>(&sin.sin_addr), sizeof(sin.sin_addr), AF_INET)) {
      return std::string(host->h_name);
    } else {
      throw std::runtime_error("Couldn't get host name for socket");
    }
  }
}

void prepareStrExpr(Server* s, int c, exprid eid, const std::string& expr, const MonoTypes& intys, const MonoTypePtr& outty) {
  auto la = LexicalAnnotation::null();

  if (intys.size() == 0 || (intys.size() == 1 && isUnit(intys[0]))) {
    s->prepare(c, eid, assume(s->readExpr(expr), functy(tuplety(intys), outty), la), tuplety());
  } else {
    ExprPtr f = assume(s->readExpr(expr), functy(intys, outty), la);
    Exprs args;
    for (size_t i = 0; i < intys.size(); ++i) {
      args.push_back(proj(var("p", la), ".f"+str::from(i), la));
    }
    s->prepare(c, eid, fn(str::strings("p"), fncall(f, args, la), la), tuplety(intys));
  }
}

void evaluateNetREPLRequest(int c, void* d) {
  Server* s = reinterpret_cast<Server*>(d);

  try {
    uint8_t cmd = 0;
    fdread(c, &cmd);

    switch (cmd) {
    case 0:
      // prepare a lexical expression with input and output types given
      try {
        exprid eid = 0;
        fdread(c, &eid);

        std::string expr;
        fdread(c, &expr);
  
        RawData ityd, otyd;
        fdread(c, &ityd);
        fdread(c, &otyd);

        MonoTypePtr itye = decode(ityd);
        MonoTypes itys;
        if (const Record* argl = is<Record>(itye)) {
          itys = selectTypes(argl->members());
        } else {
          itys.push_back(itye);
        }
        prepareStrExpr(s, c, eid, expr, itys, decode(otyd));
  
        // if we got this far, we have a successful result
        fdwrite(c, uint8_t(1));
      } catch (std::exception& ex) {
        fdwrite(c, uint8_t(0));
        fdwrite(c, std::string(ex.what()));
      }
      break;
    case 1:
      // prepare a serialized expression, also return its type
      try {
        exprid eid = 0;
        fdread(c, &eid);
        RawData exprd;
        fdread(c, &exprd);
        ExprPtr expr;
        decode(exprd, &expr);
  
        RawData tyd;
        fdread(c, &tyd);
        MonoTypePtr ty = decode(tyd);
  
        MonoTypePtr rty = s->prepare(c, eid, expr, ty);
  
        RawData rtyd;
        encode(rty, &rtyd);
  
        // if we got this far, we have a successful result
        fdwrite(c, uint8_t(1));
        fdwrite(c, rtyd);
      } catch (std::exception& ex) {
        fdwrite(c, uint8_t(0));
        fdwrite(c, std::string(ex.what()));
      }
      break;
    case 2:
      // invoke a prepared expression
      exprid evid;
      fdread(c, &evid);
      s->evaluate(c, evid);
      break;
    default:
      throw std::runtime_error("protocol violation: cmd=" + str::from(cmd));
    }
  } catch (std::exception& ex) {
    // something went wrong, disconnect
    close(c);
    unregisterEventHandler(c);
    s->disconnect(c);
  }
}

void registerNetREPL(int s, Server* svr) {
  registerEventHandler(
    s,
    [](int s, void* d) {
      int c = accept(s, 0, 0);
      if (c != -1) {
        try {
          uint32_t version = 0;
          fdread(c, &version);
          if (version != 0x00010000) {
            close(c);
          }

          reinterpret_cast<Server*>(d)->connect(c);
          registerEventHandler(c, &evaluateNetREPLRequest, d);
        } catch (std::exception&) {
          close(c);
        }
      }
    },
    svr
  );
}

int installNetREPL(int port, Server* svr) {
  int s = allocateServer(port);
  registerNetREPL(s, svr);
  return s;
}

int installNetREPL(const std::string& filepath, Server* svr) {
  int s = allocateFileSocketServer(filepath);
  registerNetREPL(s, svr);
  return s;
}

class CCServer : public Server {
public:
  CCServer(cc* c) : c(c) {
  }
  
  void connect(int) { }
  
  ExprPtr readExpr(const std::string& x) {
    return this->c->readExpr(x);
  }

  MonoTypePtr prepare(int c, exprid eid, const ExprPtr& expr, const MonoTypePtr& inty) {
    const auto& la = expr->la();

    // E(readFrom(in)::T) :: ?
    MonoTypePtr rty = requireMonotype(
      this->c->unsweetenExpression(
        fncall(
          expr,
          list(assume(fncall(var("readFrom", la), list(constant(static_cast<int>(0), la)), la), inty, la)),
          la
        )
      )->type()
    );

    // let x = readFrom(input) :: T in writeTo(output, E(x))
    this->cnetFns[c][eid] =
      this->c->compileFn<void(int)>
      (
        ".c",
        let(".in", assume(fncall(var("readFrom", la), list(var(".c", la)), la), inty, la), fncall(var("writeTo", la), list(var(".c", la),
          fncall(expr, list(var(".in", la)), la)), la), la)
      );

    return rty;
  }

  void evaluate(int c, exprid eid) {
    auto cfns = this->cnetFns[c];
    auto f    = cfns.find(eid);

    if (f != cfns.end()) {
      // perform the call
      f->second(c);
    } else {
      // invalid expression, disconnect
      close(c);
      unregisterEventHandler(c);
      disconnect(c);
    }
  }

  void disconnect(int) { }
private:
  cc* c;

  typedef void (*NetFn)(int); // socket -> ()
  typedef std::map<exprid, NetFn> NetFns;
  typedef std::map<int, NetFns> ConnNetFns;
  ConnNetFns cnetFns;
};

int installNetREPL(int port, cc* c) {
  return installNetREPL(port, new CCServer(c));
}
int installNetREPL(const std::string& filepath, cc* c) {
  return installNetREPL(filepath, new CCServer(c));
}

// connect to a running net REPL
Client::Client(const std::string& hostport) : hostport(hostport), eid(0), rbno(0), reno(0) {
  this->c = connectSocket(hostport);
  fdwrite(this->c, static_cast<uint32_t>(0x00010000));
}

Client::~Client() {
  close(this->c);
}

const std::string& Client::remoteHost() const {
  return this->hostport;
}

exprid Client::remoteExpr(const ExprPtr& expr, const MonoTypePtr& inty) {
  // have we already exchanged this exprty?
  ExprTy exprty(expr.get(), inty.get());
  auto etid = this->exprTyToID.find(exprty);
  if (etid != this->exprTyToID.end()) {
    return etid->second;
  }

  // first we send the ID, expression, and the input type
  exprid rid = ++this->eid;

  RawData exprd;
  encode(expr, &exprd);

  RawData intyd;
  encode(inty, &intyd);

  fdwrite(this->c, uint8_t(1));
  fdwrite(this->c, rid);
  fdwrite(this->c, exprd);
  fdwrite(this->c, intyd);

  // then we expect to get back a result type
  uint8_t v = 0;
  fdread(this->c, &v);
  if (v == 1) {
    RawData outtyd;
    fdread(this->c, &outtyd);

    ExprDef& ed = this->exprDefs[rid];
    ed.expr  = expr;
    ed.inty  = inty;
    ed.outty = decode(outtyd);

    this->exprTyToID[exprty] = rid;

    return rid;
  } else if (v == 0) {
    std::string errmsg;
    fdread(this->c, &errmsg);
    throw std::runtime_error("Error from server: " + errmsg);
  } else {
    throw std::runtime_error("Received malformed message from server");
  }
}

MonoTypePtr Client::input(exprid ex) const {
  auto ed = this->exprDefs.find(ex);
  if (ed == this->exprDefs.end()) {
    throw std::runtime_error("Remote process has no compiled expression with id=" + str::from(ex));
  } else {
    return ed->second.inty;
  }
}

MonoTypePtr Client::output(exprid ex) const {
  auto ed = this->exprDefs.find(ex);
  if (ed == this->exprDefs.end()) {
    throw std::runtime_error("Remote process has no compiled expression with id=" + str::from(ex));
  } else {
    return ed->second.outty;
  }
}

MonoTypePtr Client::output(const ExprPtr& e, const MonoTypePtr& inty) {
  return output(remoteExpr(e, inty));
}

void Client::show(std::ostream& out) const {
  out << this->hostport << "\n\n";
  str::seqs cs;
  cs.resize(4);

  cs[0].push_back("id");
  for (const auto& ce : this->exprDefs) { cs[0].push_back(str::from(ce.first)); }

  cs[1].push_back("expr");
  for (const auto& ce : this->exprDefs) { cs[1].push_back(hobbes::show(ce.second.expr)); }

  cs[2].push_back("input");
  for (const auto& ce : this->exprDefs) { cs[2].push_back(hobbes::show(ce.second.inty)); }

  cs[3].push_back("output");
  for (const auto& ce : this->exprDefs) { cs[3].push_back(hobbes::show(ce.second.outty)); }

  str::printRightAlignedTable(out, cs);
}

// read response values from the other end of the connection
size_t Client::appendReadFn(ReadFn f) {
  this->readFns.push(f);
  return this->reno++;
}

char* Client::readValue(size_t x) {
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

    char* r = this->readFns.front()(this->c);
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
  return reinterpret_cast<Client*>(p)->appendReadFn(f);
}

char* Client::unsafeRead(size_t p, size_t x) {
  return reinterpret_cast<Client*>(p)->readValue(x);
}

}

