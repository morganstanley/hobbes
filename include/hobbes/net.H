/*
 * net : structured communication between processes
 *
 *   use DEFINE_NET_CLIENT(T, C) to create a type T to send/receive through the commands in C
 *     each tuple in C is written (N, T, E) where
 *       N is a name for a member function in T to mediate the network command
 *       T is a C++ _function type_ (the type we expect N to have)
 *       E is a constant string expression to evaluate in the remote process
 *
 */

#ifndef HNET_H_INCLUDED
#define HNET_H_INCLUDED

#include <vector>
#include <queue>
#include <functional>
#include <string>
#include <sstream>
#include <tuple>
#include <map>
#include <stdexcept>

#include <sys/types.h>
#include <sys/un.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

namespace hobbes { namespace net {

#define HNET_VERSION     ((uint32_t)0x00010000)
#define HNET_CMD_DEFEXPR ((uint8_t)0)
#define HNET_CMD_INVOKE  ((uint8_t)2)
#define HNET_RESULT_FAIL 0

typedef std::vector<uint8_t> bytes;

// basic socket I/O
inline void sendData(int socket, const uint8_t* d, size_t sz) {
  size_t i = 0;
  while (i < sz) {
    ssize_t c = ::send(socket, d + i, sz - i, 0);
    if (c < 0) {
      throw std::runtime_error("Couldn't write to socket: " + std::string(strerror(errno)));
    }
    i += c;
  }
}

inline void sendString(int socket, const std::string& s) {
  size_t n = s.size();
  sendData(socket, (const uint8_t*)&n, sizeof(n));
  sendData(socket, (const uint8_t*)s.data(), n);
}

inline void sendBytes(int socket, const bytes& x) {
  size_t n = x.size();
  sendData(socket, (const uint8_t*)&n, sizeof(n));
  if (n > 0) {
    sendData(socket, &x[0], n);
  }
}


inline void recvData(int socket, uint8_t* d, size_t sz) {
  size_t i = 0;
  while (i < sz) {
    ssize_t di = recv(socket, d + i, sz - i, 0);

    if (di < 0) {
      if (errno != EINTR) {
        throw std::runtime_error("Couldn't read socket: " + std::string(strerror(errno)));
      }
    } else if (di == 0) {
      throw std::runtime_error("Remote process closed session prematurely");
    } else {
      i += di;
    }
  }
}

inline void recvString(int socket, std::string* x) {
  size_t n = 0;
  recvData(socket, (uint8_t*)&n, sizeof(n));

  x->resize(n);
  recvData(socket, (uint8_t*)&((*x)[0]), n);
}

inline void setBlockingBit(int socket, bool block) {
  int f = fcntl(socket, F_GETFL, 0);
  if (f == -1) f = 0;
  fcntl(socket, F_SETFL, block ? (f & (~O_NONBLOCK)) : (f | O_NONBLOCK));
}

inline size_t recvDataPartial(int socket, uint8_t* d, size_t sz) {
  ssize_t di = recv(socket, d, sz, 0);

  if (di == 0) {
    throw std::runtime_error("Remote process closed session prematurely");
  } else if (di < 0) {
    if (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR) {
      return 0;
    } else {
      throw std::runtime_error("Couldn't read socket: " + std::string(strerror(errno)));
    }
  }
  return (size_t)di;
}

// socket connection and session initiation
struct RPCDef {
  RPCDef(uint32_t id = 0, const std::string& expr = "", const bytes& willPut = bytes(), const bytes& willGet = bytes()) :
    id(id), expr(expr), willPut(willPut), willGet(willGet)
  {
  }

  uint32_t    id;      // how will this RPC be identified?
  std::string expr;    // what expression will be applied for this RPC on the remote side?
  bytes       willPut; // what type will be sent?
  bytes       willGet; // what type will be received?
};
typedef std::vector<RPCDef> RPCDefs;

// initiate a session on a connected socket by sending all of the RPC defs
inline int initSession(int s, const RPCDefs& rpcds) {
  uint32_t version = HNET_VERSION;
  sendData(s, (const uint8_t*)&version, sizeof(version));

  for (const auto& rpcd : rpcds) {
    uint8_t defCmd = HNET_CMD_DEFEXPR;
    sendData(s, &defCmd, sizeof(defCmd));
    sendData(s, (const uint8_t*)&rpcd.id, sizeof(rpcd.id));
    sendString(s, rpcd.expr);
    sendBytes(s, rpcd.willPut);
    sendBytes(s, rpcd.willGet);

    uint8_t result = HNET_RESULT_FAIL;
    recvData(s, &result, sizeof(result));
    if (result == HNET_RESULT_FAIL) {
      std::string err;
      recvString(s, &err);
      std::ostringstream m;
      m << "While trying to define '" << rpcd.expr << "' with id=" << rpcd.id << ": " << err << std::flush;
      throw std::runtime_error(m.str());
    }
  }
  return s;
}

inline addrinfo* lookupAddrInfo(const std::string& host, const std::string& port) {
  struct addrinfo  h;
  memset(&h, 0, sizeof(h));
  h.ai_family   = AF_UNSPEC;
  h.ai_socktype = SOCK_STREAM;

  struct addrinfo* addrs = 0;
  switch (getaddrinfo(host.c_str(), port.empty() ? 0 : port.c_str(), &h, &addrs)) {
  case 0:              return addrs;
  case EAI_ADDRFAMILY: throw std::runtime_error("Cannot make socket connection to " + host + ":" + port);
  case EAI_AGAIN:      throw std::runtime_error(host + ":" + port + " is temporarily unavailable");
  case EAI_FAIL:       throw std::runtime_error("Failed to resolve hostname: " + host);
  case EAI_SYSTEM:     throw std::runtime_error("Error while trying to resolve " + host + ":" + port + " (" + std::string(strerror(errno)) + ")");
  case EAI_NONAME:     throw std::runtime_error("Failed to resolve " + host + ":" + port);
  case EAI_SERVICE:    throw std::runtime_error("Failed to resolve service: " + port);
  defualt:
  case EAI_BADFLAGS:
  case EAI_MEMORY:
  case EAI_SOCKTYPE:   throw std::runtime_error("Unknown error while trying to resolve " + host + ":" + port);
  }
}

inline int makeConnection(const std::string& localAddr, const std::string& host, const std::string& port) {
  struct addrinfo* localAddrs = localAddr.empty() ? 0 : lookupAddrInfo(localAddr, "");
  struct addrinfo* addrs      = lookupAddrInfo(host, port);

  for (auto p = addrs; p != 0; p = p->ai_next) {
    int s = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (s == -1) continue;

    auto la = localAddrs;
    for (; la != 0; la = la->ai_next) {
      if (la->ai_family == p->ai_family && la->ai_socktype == p->ai_socktype && la->ai_protocol == p->ai_protocol) {
        if (bind(s, la->ai_addr, la->ai_addrlen) != -1) {
          break;
        }
      }
    }
    if (la == 0 && localAddrs != 0) continue;

    if (connect(s, p->ai_addr, p->ai_addrlen) == -1) {
      close(s);
    } else {
      freeaddrinfo(addrs);
      return s;
    }
  }

  std::string e = "Cannot connect to " + host + ":" + port + " (" + std::string(strerror(errno)) + ")";
  freeaddrinfo(addrs);
  freeaddrinfo(localAddrs);
  throw std::runtime_error(e);
}

inline int makeConnection(const std::string& host, const std::string& port) {
  return makeConnection("", host, port);
}

inline int makeConnection(const std::string& localAddr, const std::string& host, size_t port) {
  std::ostringstream ss;
  ss << port;
  return makeConnection(localAddr, host, ss.str());
}

inline int makeConnection(const std::string& host, size_t port) {
  return makeConnection("", host, port);
}

inline int makeConnection(const std::string& hostport) {
  auto p = hostport.find(":");
  if (p == std::string::npos) {
    throw std::runtime_error("Failed to determine port: " + hostport);
  } else {
    return makeConnection(hostport.substr(0, p), hostport.substr(p + 1, hostport.size()));
  }
}

// very basic macro metaprogramming
#define _HNET_FIRST(a, ...) a
#define _HNET_SECOND(a, b, ...) b
#define _HNET_JOIN(a,b) a ## b
#define _HNET_IS_NEGATE(...) _HNET_SECOND(__VA_ARGS__, 0)
#define _HNET_NOT(x) _HNET_IS_NEGATE(_HNET_JOIN(_HNET__NOT_, x))
#define _HNET__NOT_0 NEGATE, 1
#define _HNET_BOOL(x) _HNET_NOT(_HNET_NOT(x))
#define _HNET_IF_ELSE(condition) _HNET__IF_ELSE(_HNET_BOOL(condition))
#define _HNET__IF_ELSE(condition) _HNET_JOIN(_HNET__IF_, condition)
#define _HNET__IF_1(...) __VA_ARGS__ _HNET__IF_1_ELSE
#define _HNET__IF_0(...)             _HNET__IF_0_ELSE
#define _HNET__IF_1_ELSE(...)
#define _HNET__IF_0_ELSE(...) __VA_ARGS__
#define _HNET_EMPTY()
#define _HNET_EVAL(...) _HNET_EVAL256(__VA_ARGS__)
#define _HNET_EVAL256(...) _HNET_EVAL128(_HNET_EVAL128(__VA_ARGS__))
#define _HNET_EVAL128(...) _HNET_EVAL64(_HNET_EVAL64(__VA_ARGS__))
#define _HNET_EVAL64(...) _HNET_EVAL32(_HNET_EVAL32(__VA_ARGS__))
#define _HNET_EVAL32(...) _HNET_EVAL16(_HNET_EVAL16(__VA_ARGS__))
#define _HNET_EVAL16(...) _HNET_EVAL8(_HNET_EVAL8(__VA_ARGS__))
#define _HNET_EVAL8(...) _HNET_EVAL4(_HNET_EVAL4(__VA_ARGS__))
#define _HNET_EVAL4(...) _HNET_EVAL2(_HNET_EVAL2(__VA_ARGS__))
#define _HNET_EVAL2(...) _HNET_EVAL1(_HNET_EVAL1(__VA_ARGS__))
#define _HNET_EVAL1(...) __VA_ARGS__
#define _HNET_DEFER2(m) m _HNET_EMPTY _HNET_EMPTY()()
#define _HNET_HAS_PARGS(...) _HNET_BOOL(_HNET_FIRST(_HNET__EOAP_ __VA_ARGS__)())
#define _HNET__EOAP_(...) _HNET_BOOL(_HNET_FIRST(_HNET__EOA_ __VA_ARGS__)())
#define _HNET__EOA_() 0
#define _HNET_MAP(f, VS...) _HNET_EVAL(_HNET_MAPP(f, VS))
#define _HNET_MAPP(f, H, T...)        \
  f H                                 \
  _HNET_IF_ELSE(_HNET_HAS_PARGS(T))(  \
    _HNET_DEFER2(_HNET__MAPP)()(f, T) \
  )(                                  \
  )
#define _HNET__MAPP() _HNET_MAPP

/*****************************
 * BEGIN serialization by type / type-family
 *****************************/
template <typename T, typename P = void>
  struct io {
  };

// type serialization
#define _HNET_TYCTOR_PRIM      ((int)0)
#define _HNET_TYCTOR_TVAR      ((int)2)
#define _HNET_TYCTOR_FIXEDARR  ((int)4)
#define _HNET_TYCTOR_ARR       ((int)5)
#define _HNET_TYCTOR_VARIANT   ((int)6)
#define _HNET_TYCTOR_STRUCT    ((int)7)
#define _HNET_TYCTOR_SIZE      ((int)11)
#define _HNET_TYCTOR_RECURSIVE ((int)13)

template <typename T>
  void w(const T& x, bytes* out) {
    out->insert(out->end(), (uint8_t*)&x, ((uint8_t*)&x) + sizeof(x));
  }
inline void ws(const char* x, bytes* out) {
  size_t n = strlen(x);
  w(n, out);
  out->insert(out->end(), x, x + n);
}
inline void ws(const std::string& x, bytes* out) {
  w((size_t)x.size(), out);
  out->insert(out->end(), x.begin(), x.end());
}
inline void ws(const bytes& x, bytes* out) {
  w((size_t)x.size(), out);
  out->insert(out->end(), x.begin(), x.end());
}

inline void encode_primty(const char* tn, bytes* out) {
  w(_HNET_TYCTOR_PRIM, out);
  ws(tn, out);
  w((bool)false, out);
}

// primitive serialization
#define _HNET_DEFINE_PRIMTYS(T, n) \
  template <> \
    struct io<T> { \
      typedef void can_memcpy; \
      static void        encode(bytes* out)       { encode_primty(n, out); } \
      static std::string describe()               { return n; } \
      static void        write(int s, const T& x) { sendData(s, (const uint8_t*)&x, sizeof(x)); } \
      static void        read(int s, T* x)        { recvData(s, (uint8_t*)x, sizeof(T)); } \
      typedef uint8_t async_read_state; \
      static void prepare(uint8_t* o) { *o = 0; } \
      static bool accum(int s, uint8_t* o, T* x) { *o += recvDataPartial(s, ((uint8_t*)x) + *o, sizeof(T) - *o); return *o == sizeof(T); } \
    }
_HNET_DEFINE_PRIMTYS(bool,     "bool");
_HNET_DEFINE_PRIMTYS(uint8_t,  "byte");
_HNET_DEFINE_PRIMTYS(char,     "char");
_HNET_DEFINE_PRIMTYS(int16_t,  "short");
_HNET_DEFINE_PRIMTYS(uint16_t, "short");
_HNET_DEFINE_PRIMTYS(int32_t,  "int");
_HNET_DEFINE_PRIMTYS(uint32_t, "int");
_HNET_DEFINE_PRIMTYS(int64_t,  "long");
_HNET_DEFINE_PRIMTYS(uint64_t, "long");
#ifdef __clang__
_HNET_DEFINE_PRIMTYS(size_t,   "long");
#endif
_HNET_DEFINE_PRIMTYS(float,    "float");
_HNET_DEFINE_PRIMTYS(double,   "double");

template <typename T, typename P = void>
  struct cannot_memcpy {
    typedef void type;
  };
template <typename T>
  struct cannot_memcpy<T, typename io<T>::can_memcpy> {
  };

// support unit
struct unit {
  unit() { }
  bool operator==(const unit&) const { return true; }
  bool operator< (const unit&) const { return false; }
};

template <>
  struct io<unit> {
    static void encode(bytes* out) { encode_primty("unit", out); }
    static std::string describe() { return "()"; }
    static void write(int s, const unit&) { }
    static void read(int s, unit*) { }

    typedef uint8_t async_read_state;
    static void prepare(uint8_t* o) {}
    static bool accum(int s, uint8_t* o, unit* x) { return true; }
  };

// support enumerations
#define HNET_ENUM_CTOR_DEF(n) n ,
#define HNET_ENUM_CTOR_CTOR(n) static const SelfT n() { return SelfT(SelfT::Enum::n); }
#define HNET_ENUM_CTORC_SUCC(n) +1
#define HNET_ENUM_CTOR_STR(n) + "|" #n
#define HNET_ENUM_CTOR_ENCODE(n) \
  ::hobbes::net::ws(#n, out); \
  ::hobbes::net::w((uint32_t)(Enum :: n), out); \
  ::hobbes::net::encode_primty("unit", out);
#define HNET_ENUM_TOSTR_CASE(n) \
  case SelfT :: Enum :: n: o << "|" #n "|"; break;

#define DEFINE_HNET_ENUM(T, CTORS...) \
  struct T { \
    typedef void is_hnet_enum; \
    enum class Enum : uint32_t { \
      _HNET_MAP(HNET_ENUM_CTOR_DEF, CTORS) \
      COUNT \
    }; \
    Enum value; \
    T() : value() { } \
    T(Enum v) : value(v) { } \
    T& operator=(Enum v) { this->value = v; return *this; } \
    operator Enum() { return this->value; } \
    typedef T SelfT; \
    _HNET_MAP(HNET_ENUM_CTOR_CTOR, CTORS) \
    static void encode(::hobbes::net::bytes* out) { \
      ::hobbes::net::w(_HNET_TYCTOR_VARIANT, out); \
      ::hobbes::net::w((size_t)(0 _HNET_MAP(HNET_ENUM_CTORC_SUCC, CTORS)), out); \
      _HNET_MAP(HNET_ENUM_CTOR_ENCODE, CTORS); \
    } \
    static std::string describe() { \
      return (std::string("") _HNET_MAP(HNET_ENUM_CTOR_STR, CTORS)).substr(1); \
    } \
    bool operator==(const T& rhs) const { return this->value == rhs.value; } \
  }; \
  inline std::ostream& operator<<(std::ostream& o, const T& x) { \
    typedef T SelfT; \
    switch (x.value) { \
    _HNET_MAP(HNET_ENUM_TOSTR_CASE, CTORS) \
    default: \
      o << "|?|"; \
      break; \
    } \
    return o; \
  }

template <typename T>
  struct io<T, typename T::is_hnet_enum> {
    typedef void can_memcpy;
    static void        encode(bytes* out)          { T::encode(out); }
    static std::string describe()                  { return T::describe(); }
    static size_t      size(const T&)              { return sizeof(T); }
    static void        write(int s, const T& x)    { io<uint32_t>::write(s, (uint32_t)x.value); }
    static void        read(int s, T* x)           { io<uint32_t>::read(s, (uint32_t*)&x->value); }

    typedef uint8_t async_read_state;
    static void prepare(uint8_t* o) { *o = 0; }
    static bool accum(int s, uint8_t* o, T* x) { *o += recvDataPartial(s, ((uint8_t*)x) + *o, sizeof(uint32_t) - *o); return *o == sizeof(uint32_t); }
  };

// support variants (with and without explicit constructor names)
#define DEFINE_HNET_VARIANT_GEN(T, VDECL, VCTORS, VCOPY, VDESTROY, CTORCOUNT, VENCODE, VDESC, VWRITE, VREAD, VVISITCASE, VEQCASE, CTAGS, CDATA, CAIOSTATES, CAIOSTATEINIT, CAIOREAD) \
  template <typename R> \
    struct T##Visitor { \
      VDECL \
    }; \
  struct T { \
    typedef void is_hnet_variant; \
    typedef T SelfT; \
    T() : tag(Enum::COUNT) { } \
    VCTORS \
    T(const T& rhs) : tag(rhs.tag) { \
      switch (this->tag) { \
      VCOPY \
      default: break; \
      } \
    } \
    ~T() { \
      switch (this->tag) { \
      VDESTROY \
      default: break; \
      } \
    } \
    T& operator=(const T& rhs) { \
      if (this == &rhs) return *this; \
      switch (this->tag) { \
      VDESTROY \
      default: break; \
      } \
      this->tag = rhs.tag; \
      switch (this->tag) { \
      VCOPY \
      default: break; \
      } \
      return *this; \
    } \
    static void encode(::hobbes::net::bytes* out) { \
      ::hobbes::net::w(_HNET_TYCTOR_VARIANT, out); \
      ::hobbes::net::w((size_t)(0 CTORCOUNT), out); \
      VENCODE; \
    } \
    static std::string describe() { \
      return "|" + (std::string("") VDESC).substr(1) + "|"; \
    } \
    void write(int s) const { \
      switch (this->tag) { \
      VWRITE \
      default: break; \
      } \
    } \
    void read(int s) { \
      switch (this->tag) { \
      VDESTROY \
      default: break; \
      } \
      this->tag = Enum::COUNT; \
      ::hobbes::net::io<uint32_t>::read(s, (uint32_t*)&this->tag); \
      switch (this->tag) { \
      VREAD \
      default: break; \
      } \
    } \
    template <typename R> \
      R visit(const T##Visitor<R>& v) const { \
        switch (this->tag) { \
        VVISITCASE \
        default: throw std::runtime_error("while deconstructing the " #T " variant, cannot decide payload type because tag is invalid"); \
        } \
      } \
    bool operator==(const T& rhs) const { \
      if (this->tag != rhs.tag) { \
        return false; \
      } else { \
        switch (this->tag) { \
        VEQCASE \
        default: return false; \
        } \
      } \
    } \
  private: \
    enum class Enum : uint32_t { \
      CTAGS \
      COUNT \
    }; \
    Enum tag; \
    union { \
      char data[1]; \
      CDATA \
    }; \
  public: \
    typedef ::hobbes::net::io<uint32_t>::async_read_state TagState; \
    union PayloadState { \
      char data[1]; \
      CAIOSTATES \
    }; \
    struct async_read_state { \
      bool         readTag; \
      TagState     tagS; \
      PayloadState payloadS; \
    }; \
    static void prepare(async_read_state* o) { o->readTag = true; ::hobbes::net::io<uint32_t>::prepare(&o->tagS); } \
    static bool accum(int s, async_read_state* o, T* x) { \
      if (o->readTag) { \
        if (::hobbes::net::io<uint32_t>::accum(s, &o->tagS, (uint32_t*)&x->tag)) { \
          o->readTag = false; \
          switch (x->tag) { \
          CAIOSTATEINIT \
          default: break; \
          } \
        } \
      } else { \
        switch (x->tag) { \
        CAIOREAD \
        default: break; \
        } \
      } \
      return false; \
    } \
  }

// (with implicit ctor names)
#define HNET_VARIANT_CTOR(n, t) static SelfT n(const t & x) { SelfT r; r.tag = Enum::tag_##n; new (r.data) t(x); return r; }
#define HNET_VARIANT_CTOR_STR(n, t) + "," #n ":" + ::hobbes::net::io< t >::describe()
#define HNET_VARIANT_CTOR_TAG(n, t) tag_##n,
#define HNET_VARIANT_WRITE_CASE(n, t) case Enum::tag_##n: ::hobbes::net::io<int>::write(s, (int)this->tag); ::hobbes::net::io< t >::write(s, this->n##_data); break;
#define HNET_VARIANT_READ_CASE(n, t)  case Enum::tag_##n: new (this->data) t(); ::hobbes::net::io< t >::read(s, &this->n##_data); break;
#define HNET_VARIANT_PCOPY(n, t)      case Enum::tag_##n: new (this->data) t(rhs.n##_data); break;
#define HNET_VARIANT_PDESTROY(n, t)   case Enum::tag_##n: { typedef t __DT; ((__DT*)&this->n##_data)->~__DT(); } break;
#define HNET_VARIANT_SUCC(n, t) +1
#define HNET_VARIANT_CTOR_OPAQUEDATA(n, t) t n##_data;
#define HNET_VARIANT_CTOR_ENCODE(n, t)      \
  ::hobbes::net::ws(#n, out); \
  ::hobbes::net::w((uint32_t)Enum::tag_##n, out); \
  ::hobbes::net::io< t >::encode(out);

#define HNET_VARIANT_VDECL(n, t) virtual R n(const t & x) const = 0;
#define HNET_VARIANT_VCASE(n, t) case Enum::tag_##n: return v.n (this->n##_data);
#define HNET_VARIANT_EQCASE(n, t) case Enum::tag_##n: return (this->n##_data == rhs.n##_data);

#define HNET_VARIANT_AIOSTATE_DATA(n, t) ::hobbes::net::io<t>::async_read_state n##_aioS;
#define HNET_VARIANT_AIOSTATE_INIT(n, t) case Enum::tag_##n: { new (o->payloadS.data) ::hobbes::net::io<t>::async_read_state(); ::hobbes::net::io<t>::prepare(&o->payloadS.n##_aioS); new (x->data) t(); } break;
#define HNET_VARIANT_AIOSTATE_ACCUM(n, t) case Enum::tag_##n: return ::hobbes::net::io<t>::accum(s, &o->payloadS.n##_aioS, &x->n##_data);

#define DEFINE_HNET_VARIANT(T, CTORS...) \
  DEFINE_HNET_VARIANT_GEN(T, _HNET_MAP(HNET_VARIANT_VDECL, CTORS), _HNET_MAP(HNET_VARIANT_CTOR, CTORS), _HNET_MAP(HNET_VARIANT_PCOPY, CTORS), _HNET_MAP(HNET_VARIANT_PDESTROY, CTORS), _HNET_MAP(HNET_VARIANT_SUCC, CTORS), _HNET_MAP(HNET_VARIANT_CTOR_ENCODE, CTORS), _HNET_MAP(HNET_VARIANT_CTOR_STR, CTORS), _HNET_MAP(HNET_VARIANT_WRITE_CASE, CTORS), _HNET_MAP(HNET_VARIANT_READ_CASE, CTORS), _HNET_MAP(HNET_VARIANT_VCASE, CTORS), _HNET_MAP(HNET_VARIANT_EQCASE, CTORS), _HNET_MAP(HNET_VARIANT_CTOR_TAG, CTORS), _HNET_MAP(HNET_VARIANT_CTOR_OPAQUEDATA, CTORS), _HNET_MAP(HNET_VARIANT_AIOSTATE_DATA, CTORS), _HNET_MAP(HNET_VARIANT_AIOSTATE_INIT, CTORS), _HNET_MAP(HNET_VARIANT_AIOSTATE_ACCUM, CTORS))

// (with explicit ctor names)
#define HNET_VARIANT_LBL_CTOR(n, lbl, t) static SelfT n(const t & x) { SelfT r; r.tag = Enum::tag_##n; new (r.data) t(x); return r; }
#define HNET_VARIANT_LBL_CTOR_STR(n, lbl, t) + "," #n ":" + ::hobbes::net::io< t >::describe()
#define HNET_VARIANT_LBL_CTOR_TAG(n, lbl, t) tag_##n,
#define HNET_VARIANT_LBL_WRITE_CASE(n, lbl, t) case Enum::tag_##n: ::hobbes::net::io<int>::write(s, (int)this->tag); ::hobbes::net::io< t >::write(s, this->n##_data); break;
#define HNET_VARIANT_LBL_READ_CASE(n, lbl, t)  case Enum::tag_##n: new (this->data) t(); ::hobbes::net::io< t >::read(s, &this->n##_data); break;
#define HNET_VARIANT_LBL_PCOPY(n, lbl, t)      case Enum::tag_##n: new (this->data) t(rhs.n##_data); break;
#define HNET_VARIANT_LBL_PDESTROY(n, lbl, t)   case Enum::tag_##n: { typedef t __DT; ((__DT*)&this->n##_data)->~__DT(); } break;
#define HNET_VARIANT_LBL_SUCC(n, lbl, t) +1
#define HNET_VARIANT_LBL_CTOR_OPAQUEDATA(n, lbl, t) t n##_data;
#define HNET_VARIANT_LBL_CTOR_ENCODE(n, lbl, t) \
  ::hobbes::net::ws(#lbl, out); \
  ::hobbes::net::w((uint32_t)Enum::tag_##n, out); \
  ::hobbes::net::io< t >::encode(out);

#define HNET_VARIANT_LBL_VDECL(n, lbl, t) virtual R n(const t & x) const = 0;
#define HNET_VARIANT_LBL_VCASE(n, lbl, t) case Enum::tag_##n: return v. n (this->n##_data);
#define HNET_VARIANT_LBL_EQCASE(n, lbl, t) case Enum::tag_##n: return (this->n##_data == rhs.n##_data);

#define HNET_VARIANT_LBL_CTOR_AIOSTATE_DATA(n, lbl, t) ::hobbes::net::io<t>::async_read_state n##_aioS;
#define HNET_VARIANT_LBL_CTOR_AIOSTATE_INIT(n, lbl, t) case Enum::tag_##n: { new (o->payloadS.data) ::hobbes::net::io<t>::async_read_state(); ::hobbes::net::io<t>::prepare(&o->payloadS.n##_aioS); new (x->data) t(); } break;
#define HNET_VARIANT_LBL_CTOR_AIOSTATE_ACCUM(n, lbl, t) case Enum::tag_##n: return ::hobbes::net::io<t>::accum(s, &o->payloadS.n##_aioS, &x->n##_data);

#define DEFINE_HNET_VARIANT_WITH_LABELS(T, CTORS...) \
  DEFINE_HNET_VARIANT_GEN(T, _HNET_MAP(HNET_VARIANT_LBL_VDECL, CTORS), _HNET_MAP(HNET_VARIANT_LBL_CTOR, CTORS), _HNET_MAP(HNET_VARIANT_LBL_PCOPY, CTORS), _HNET_MAP(HNET_VARIANT_LBL_PDESTROY, CTORS), _HNET_MAP(HNET_VARIANT_LBL_SUCC, CTORS), _HNET_MAP(HNET_VARIANT_LBL_CTOR_ENCODE, CTORS), _HNET_MAP(HNET_VARIANT_LBL_CTOR_STR, CTORS), _HNET_MAP(HNET_VARIANT_LBL_WRITE_CASE, CTORS), _HNET_MAP(HNET_VARIANT_LBL_READ_CASE, CTORS), _HNET_MAP(HNET_VARIANT_LBL_VCASE, CTORS), _HNET_MAP(HNET_VARIANT_LBL_EQCASE, CTORS), _HNET_MAP(HNET_VARIANT_LBL_CTOR_TAG, CTORS), _HNET_MAP(HNET_VARIANT_LBL_CTOR_OPAQUEDATA, CTORS), _HNET_MAP(HNET_VARIANT_LBL_CTOR_AIOSTATE_DATA, CTORS), _HNET_MAP(HNET_VARIANT_LBL_CTOR_AIOSTATE_INIT, CTORS), _HNET_MAP(HNET_VARIANT_LBL_CTOR_AIOSTATE_ACCUM, CTORS))

template <typename T>
  struct io<T, typename T::is_hnet_variant> {
    static void        encode(bytes* out)       { T::encode(out); }
    static std::string describe()               { return T::describe(); }
    static void        write(int s, const T& x) { return x.write(s); }
    static void        read(int s, T* x)        { return x->read(s); }

    // async reading of variants
    typedef typename T::async_read_state async_read_state;
    static void prepare(async_read_state* o) { T::prepare(o); }
    static bool accum(int s, async_read_state* o, T* x) { return T::accum(s, o, x); }
  };

// support pairs
template <typename U, typename V>
  struct io<std::pair<U,V>> {
    static void encode(bytes* out) {
      w(_HNET_TYCTOR_STRUCT, out);
      w((size_t)2, out);
      
      ws(".f0", out);
      w((int)-1, out);
      io<U>::encode(out);

      ws(".f1", out);
      w((int)-1, out);
      io<V>::encode(out);
    }
    static std::string describe() {
      return "(" + io<U>::describe() + "*" + io<V>::describe() + ")";
    }
    static void write(int s, const std::pair<U,V>& p) {
      io<U>::write(s, p.first);
      io<V>::write(s, p.second);
    }
    static void read(int s, std::pair<U,V>* p) {
      io<U>::read(s, &p->first);
      io<V>::read(s, &p->second);
    }

    // async reading of pairs
    struct async_read_state {
      typedef typename io<U>::async_read_state Ustate;
      typedef typename io<V>::async_read_state Vstate;

      bool   readFirst;
      Ustate fstState;
      Vstate sndState;
    };

    static void prepare(async_read_state* o) {
      o->readFirst = true;
      io<U>::prepare(&o->fstState);
      io<V>::prepare(&o->sndState);
    }

    static bool accum(int s, async_read_state* o, std::pair<U, V>* x) {
      if (o->readFirst) {
        if (io<U>::accum(s, &o->fstState, &x->first)) {
          o->readFirst = false;
        }
      } else {
        if (io<V>::accum(s, &o->sndState, &x->second)) {
          return true;
        }
      }
      return false;
    }
  };

// support vectors of mem-copyable type
template <typename T>
  struct io<std::vector<T>, typename io<T>::can_memcpy> {
    static void encode(bytes* out) { w(_HNET_TYCTOR_ARR, out); io<T>::encode(out); }
    static std::string describe() { return "[" + io<T>::describe() + "]"; }
    static void write(int s, const std::vector<T>& x) { size_t n = x.size(); io<size_t>::write(s, n); if (n > 0) sendData(s, (const uint8_t*)&x[0], sizeof(T) * n); }
    static void read(int s, std::vector<T>* x) { size_t n = 0; io<size_t>::read(s, &n); x->resize(n); if (n > 0) recvData(s, (uint8_t*)&(*x)[0], n); }

    // async reading of mem-copyable vectors
    struct async_read_state {
      typedef io<size_t>::async_read_state LenS;

      bool   readLen;
      LenS   lenS;
      size_t bytesRead;
      size_t byteLen;
    };

    static void prepare(async_read_state* o) {
      o->readLen = true;
      io<size_t>::prepare(&o->lenS);
    }

    static bool accum(int s, async_read_state* o, std::vector<T>* x) {
      if (o->readLen) {
        if (io<size_t>::accum(s, &o->lenS, &o->byteLen)) {
          x->resize(o->byteLen);
          o->bytesRead = 0;
          o->byteLen   = sizeof(T) * o->byteLen;
          o->readLen   = false;
        }
      } else {
        uint8_t* buf = (uint8_t*)&((*x)[0]);
        o->bytesRead += recvDataPartial(s, buf + o->bytesRead, o->byteLen - o->bytesRead);
      }
      return !o->readLen && o->bytesRead == o->byteLen;
    }
  };

template <typename T>
  struct io<std::vector<T>, typename cannot_memcpy<T>::type> {
    static void encode(bytes* out) { w(_HNET_TYCTOR_ARR, out); io<T>::encode(out); }
    static std::string describe() { return "[" + io<T>::describe() + "]"; }
    static void write(int s, const std::vector<T>& x) {
      size_t n = x.size();
      io<size_t>::write(s, n);
      for (size_t i = 0; i < n; ++i) {
        io<T>::write(s, x[i]);
      }
    }
    static void read(int s, std::vector<T>* x) {
      size_t n = 0;
      io<size_t>::read(s, &n);
      x->resize(n);
      for (size_t i = 0; i < n; ++i) {
        io<T>::read(s, &(*x)[i]);
      }
    }

    // async reading of vectors
    struct async_read_state {
      typedef io<size_t>::async_read_state LenS;
      typedef typename io<T>::async_read_state ElemS;

      bool   readLen;
      LenS   lenS;
      size_t idx;
      ElemS  elemS;
    };

    static void prepare(async_read_state* o) {
      o->readLen = true;
      io<size_t>::prepare(&o->lenS);
    }

    static bool accum(int s, async_read_state* o, std::vector<T>* x) {
      if (o->readLen) {
        if (io<size_t>::accum(s, &o->lenS, &o->idx)) {
          x->resize(o->idx);
          o->idx     = 0;
          o->readLen = false;
          io<T>::prepare(&o->elemS);
        }
      } else {
        if (io<T>::accum(s, &o->elemS, &(*x)[o->idx])) {
          ++o->idx;
          io<T>::prepare(&o->elemS);
        }
      }
      return !o->readLen && o->idx == x->size();
    }
  };

// support maps (as if vectors of pairs)
template <typename K, typename T>
  struct io<std::map<K,T>> {
    static void encode(bytes* out) { io<std::vector<std::pair<K,T>>>::encode(out); }
    static std::string describe() { return io<std::vector<std::pair<K,T>>>::describe(); }
    static void write(int s, const std::map<K,T>& x) {
      size_t n = x.size();
      io<size_t>::write(s, n);
      for (const auto& xp : x) {
        io<K>::write(s, xp.first);
        io<T>::write(s, xp.second);
      }
    }
    static void read(int s, std::map<K,T>* x) {
      size_t n = 0;
      io<size_t>::read(s, &n);
      for (size_t i = 0; i < n; ++i) {
        K k;
        io<K>::read(s, &k);
        T t;
        io<T>::read(s, &t);
        (*x)[k] = t;
      }
    }

    typedef io<size_t>::async_read_state LenS;
    typedef typename io<K>::async_read_state      KS;
    typedef typename io<T>::async_read_state      TS;
    enum class ReadS : uint8_t { LenS, KS, TS };

    struct async_read_state {
      ReadS   readS;
      LenS    lenS;
      size_t  len;
      KS      kS;
      K       k;
      TS      tS;
      T       t;
    };
    static void prepare(async_read_state* o) {
      o->readS = ReadS::LenS;
      io<size_t>::prepare(&o->lenS);
    }
    static bool accum(int s, async_read_state* o, std::map<K,T>* x) {
      switch (o->readS) {
      case ReadS::LenS:
        if (io<size_t>::accum(s, &o->lenS, &o->len)) {
          o->readS = ReadS::KS;
          io<K>::prepare(&o->kS);
        }
        break;
      case ReadS::KS:
        if (io<K>::accum(s, &o->kS, &o->k)) {
          o->readS = ReadS::TS;
          io<T>::prepare(&o->tS);
        }
        break;
      case ReadS::TS:
        if (io<T>::accum(s, &o->tS, &o->t)) {
          (*x)[o->k] = o->t;
          --o->len;
          o->readS = ReadS::KS;
          io<K>::prepare(&o->kS);
        }
        break;
      }
      return o->readS != ReadS::LenS && o->len == 0;
    }
  };

// support strings (but const char* can only be sent, not received)
template <>
  struct io<const char*> {
    static void encode(bytes* out) { w(_HNET_TYCTOR_ARR, out); io<char>::encode(out); }
    static std::string describe() { return "[char]"; }
    static void write(int s, const char* x) { size_t n = strlen(x); io<size_t>::write(s, n); sendData(s, (const uint8_t*)x, n); }
  };
template <>
  struct io<std::string> {
    static void encode(bytes* out) { w(_HNET_TYCTOR_ARR, out); io<char>::encode(out); }
    static std::string describe() { return "[char]"; }
    static void write(int s, const std::string& x) { io<size_t>::write(s, x.size()); sendData(s, (const uint8_t*)x.data(), x.size()); }
    static void read(int s, std::string* x) { size_t n = 0; io<size_t>::read(s, &n); x->resize(n); recvData(s, (uint8_t*)x->data(), n); }

    // async reading of strings
    struct async_read_state {
      typedef io<size_t>::async_read_state LenS;

      bool   readLen;
      LenS   lenS;
      size_t bytesRead;
      size_t byteLen;
    };

    static void prepare(async_read_state* o) {
      o->readLen = true;
      io<size_t>::prepare(&o->lenS);
    }

    static bool accum(int s, async_read_state* o, std::string* x) {
      if (o->readLen) {
        if (io<size_t>::accum(s, &o->lenS, &o->byteLen)) {
          x->resize(o->byteLen);
          o->bytesRead = 0;
          o->readLen = false;
        }
      } else {
        uint8_t* buf = (uint8_t*)&((*x)[0]);
        o->bytesRead += recvDataPartial(s, buf + o->bytesRead, o->byteLen - o->bytesRead);
      }
      return !o->readLen && o->bytesRead == o->byteLen;
    }
  };

// support standard, reflective structs
#define HNET_FIELDC_SUCC(t,n) +1
#define HNET_FIELD_COUNT(FIELDS...) (0 _HNET_MAP(HNET_FIELDC_SUCC, FIELDS))

#define HNET_FIELDW_SUCC(t,n) ::hobbes::net::io< t >::write(s, this-> n);
#define HNET_FIELD_WRITES(FIELDS...) _HNET_MAP(HNET_FIELDW_SUCC, FIELDS)

#define HNET_FIELDR_SUCC(t,n) ::hobbes::net::io< t >::read(s, &this-> n);
#define HNET_FIELD_READS(FIELDS...) _HNET_MAP(HNET_FIELDR_SUCC, FIELDS)

#define HNET_STRUCT_FIELD(t, n) t n;
#define HNET_STRUCT_FIELD_ENC(t, n) ::hobbes::net::ws(#n, out); ::hobbes::net::w((int)-1, out); ::hobbes::net::io< t >::encode(out);
#define HNET_STRUCT_FIELD_DESC(t, n) + (", " #n " : " + ::hobbes::net::io< t >::describe())
#define HNET_STRUCT_FIELD_EQ(t, n) && this->n == rhs.n

#define HNET_FIELD_ASYNC_READ_IDX_DEF(_, n) readAt_##n,
#define HNET_FIELD_ASYNC_READ_STATE_DEC(t, n) ::hobbes::net::io< t >::async_read_state n;
#define HNET_FIELD_ASYNC_READ_STATE_INIT(t, n) ::hobbes::net::io< t >::prepare(&o->fieldStates.n);
#define HNET_FIELD_ASYNC_READ_FIELD(t, n) case FieldIndex::readAt_##n: if (::hobbes::net::io< t >::accum(s, &o->fieldStates.n, &x->n)) { o->idx = (FieldIndex)(((uint32_t)o->idx) + 1); } break;

#define DEFINE_HNET_STRUCT(T, FIELDS...) \
  struct T { \
    _HNET_MAP(HNET_STRUCT_FIELD, FIELDS) /* struct fields */ \
    typedef void is_hnet_struct; /* identify this type as a struct */ \
    static void encode(::hobbes::net::bytes* out) { \
      size_t arity = HNET_FIELD_COUNT(FIELDS); \
      if (arity > 0) { \
        ::hobbes::net::w(_HNET_TYCTOR_STRUCT, out); \
        ::hobbes::net::w(arity, out); \
        _HNET_MAP(HNET_STRUCT_FIELD_ENC, FIELDS) \
      } else { \
        ::hobbes::net::encode_primty("unit", out); \
      } \
    } \
    static std::string describe() { \
      return "{" + ( std::string("") _HNET_MAP(HNET_STRUCT_FIELD_DESC, FIELDS) ).substr(2) + "}"; \
    } \
    void write(int s) const { \
      HNET_FIELD_WRITES(FIELDS); \
    } \
    void read(int s) { \
      HNET_FIELD_READS(FIELDS); \
    } \
    bool operator==(const T& rhs) const { \
      return true _HNET_MAP(HNET_STRUCT_FIELD_EQ, FIELDS); \
    } \
    /* async reading */ \
    enum class FieldIndex : uint32_t { \
      _ZeroInit = 0, \
      _HNET_MAP(HNET_FIELD_ASYNC_READ_IDX_DEF, FIELDS) \
      _FieldListEnd \
    }; \
    struct async_read_state { \
      FieldIndex idx; \
      struct { \
      _HNET_MAP(HNET_FIELD_ASYNC_READ_STATE_DEC, FIELDS) \
      } fieldStates; \
    }; \
    static void prepare(async_read_state* o) { \
      o->idx = (FieldIndex)1; \
      _HNET_MAP(HNET_FIELD_ASYNC_READ_STATE_INIT, FIELDS) \
    } \
    static bool accum(int s, async_read_state* o, T* x) { \
      switch (o->idx) { \
      _HNET_MAP(HNET_FIELD_ASYNC_READ_FIELD, FIELDS) \
      default: break; \
      } \
      return o->idx == FieldIndex::_FieldListEnd; \
    } \
  }

template <typename T>
  struct io<T, typename T::is_hnet_struct> {
    static void        encode(bytes* out)          { T::encode(out); }
    static std::string describe()                  { return T::describe(); }
    static void        write(int s, const T& x)    { x.write(s); }
    static void        read(int s, T* x)           { x->read(s); }

    typedef typename T::async_read_state async_read_state;
    static void prepare(async_read_state* o) { T::prepare(o); }
    static bool accum(int s, async_read_state* o, T* x) { return T::accum(s, o, x); }
  };

// tuple/sequence serialization
template <typename ... Ts>
  struct oSeq {
    static const size_t count = 0;
    static void        encode(size_t,bytes*)    { }
    static std::string describe()               { return ""; }
    static void        write(int, const Ts&...) { }
  };
template <typename T, typename ... Ts>
  struct oSeq<T, Ts...> {
    static const size_t count = 1 + oSeq<Ts...>::count;

    static void encode(size_t f, bytes* out) {
      char fn[32];
      snprintf(fn, sizeof(fn), ".f%lld", (unsigned long long)f);
      ws(fn, out);
      w((int)-1, out);
      io<T>::encode(out);

      oSeq<Ts...>::encode(f+1, out);
    }
    static std::string describe() {
      return io<T>::describe() + "*" + oSeq<Ts...>::describe();
    }
    static void write(int socket, const T& x, const Ts&... xs) {
      io<T>::write(socket, x);
      oSeq<Ts...>::write(socket, xs...);
    }
  };

template <size_t i, size_t e, typename ... Ts>
  struct ioTuple {
    static void write(int s, const std::tuple<Ts...>& t) {
      io<typename std::tuple_element<i, std::tuple<Ts...>>::type>::write(s, std::get<i>(t));
      ioTuple<i+1, e, Ts...>::write(s, t);
    }
    static void read(int s, std::tuple<Ts...>* t) {
      io<typename std::tuple_element<i, std::tuple<Ts...>>::type>::read(s, &std::get<i>(*t));
      ioTuple<i+1, e, Ts...>::read(s, t);
    }
  };
template <size_t e, typename ... Ts>
  struct ioTuple<e, e, Ts...> {
    static void write(int s, const std::tuple<Ts...>&) { }
    static void read(int s, std::tuple<Ts...>*) { }
  };

template <typename ... Ts>
  struct aioTupleState {
    typedef std::tuple<> async_read_state;
    static void prepare(async_read_state*) { }
  };
template <typename T, typename ... Ts>
  struct aioTupleState<T, Ts...> {
    typedef typename io<T>::async_read_state FstRS;
    typedef typename aioTupleState<Ts...>::async_read_state SndRS;
    typedef std::pair<FstRS, SndRS> async_read_state;

    static void prepare(async_read_state* o) {
      io<T>::prepare(&o->first);
      aioTupleState<Ts...>::prepare(&o->second);
    }
  };
template <size_t i, typename T>
  struct aioTupleStateAt { };
template <typename U, typename V>
  struct aioTupleStateAt<0, std::pair<U,V>> {
    typedef U type;
    static U* get(std::pair<U,V>* x) { return &x->first; }
  };
template <size_t i, typename U, typename V>
  struct aioTupleStateAt<i, std::pair<U,V>> {
    typedef typename aioTupleStateAt<i-1, V>::type type;
    static type* get(std::pair<U,V>* x) { return aioTupleStateAt<i-1, V>::get(&x->second); }
  };

template <size_t i, size_t e, typename ... Ts>
  struct aioTupleRead {
    static bool accum(int s, size_t ti, typename aioTupleState<Ts...>::async_read_state* o, std::tuple<Ts...>* x) {
      if (ti == i) {
        return io<typename std::tuple_element<i, std::tuple<Ts...>>::type>::accum(s, aioTupleStateAt<i, typename aioTupleState<Ts...>::async_read_state>::get(o), &std::get<i>(*x));
      } else {
        return aioTupleRead<i+1, e, Ts...>::accum(s, ti, o, x);
      }
    }
  };
template <size_t e, typename ... Ts>
  struct aioTupleRead<e, e, Ts...> {
    static bool accum(int s, size_t, typename aioTupleState<Ts...>::async_read_state*, std::tuple<Ts...>*) { return true; }
  };

template <typename ... Ts>
  struct io< std::tuple<Ts...> > {
    static void encode(bytes* out) {
      size_t arity = oSeq<Ts...>::count;

      if (arity > 0) {
        w(_HNET_TYCTOR_STRUCT, out);
        w(arity, out);
        oSeq<Ts...>::encode(0, out);
      } else {
        encode_primty("unit", out);
      }
    }
    static std::string describe() {
      std::string x = oSeq<Ts...>::describe();
      return (x.size() > 1) ? x.substr(0,x.size()-1) : x;
    }
    static void write(int s, const std::tuple<Ts...>& t) {
      ioTuple<0, std::tuple_size<std::tuple<Ts...>>::value, Ts...>::write(s, t);
    }
    static void read(int s, std::tuple<Ts...>* t) {
      ioTuple<0, std::tuple_size<std::tuple<Ts...>>::value, Ts...>::read(s, t);
    }

    // async reading of tuples
    typedef std::pair<size_t, typename aioTupleState<Ts...>::async_read_state> async_read_state;
    static void prepare(async_read_state* o) { o->first = 0; aioTupleState<Ts...>::prepare(&o->second); }
    static bool accum(int s, async_read_state* o, std::tuple<Ts...>* x) {
      if (aioTupleRead<0, std::tuple_size<std::tuple<Ts...>>::value, Ts...>::accum(s, o->first, &o->second, x)) {
        ++o->first;
      }
      return o->first == oSeq<Ts...>::count;
    }
  };

// support opaque type aliases
#define DEFINE_HNET_TYPE_ALIAS(_ATY, _REPTY) \
  struct _ATY { \
    typedef void is_hnet_alias; \
    typedef _REPTY type; \
    static const char* name() { return #_ATY; } \
    inline operator _REPTY() { return this->value; } \
    _REPTY value; \
    _ATY() : value() { } \
    _ATY(const _REPTY& x) : value(x) { } \
    _ATY(const _ATY& x) : value(x.value) { } \
    _ATY& operator=(const _ATY& x) { this->value = x.value; return *this; } \
    bool operator==(const _ATY& x) const { return this->value == x.value; } \
  }

template <typename T>
  struct io<T, typename T::is_hnet_alias> {
    static void        encode(bytes* out)          { w(_HNET_TYCTOR_PRIM, out); ws(T::name(), out); w((bool)true, out); io<typename T::type>::encode(out); }
    static std::string describe()                  { return T::name(); }
    static void        write(int s, const T& x)    { io<typename T::type>::write(s, x.value); }
    static void        read(int s, T* x)           { io<typename T::type>::read(s, &x->value); }

    typedef typename io<typename T::type>::async_read_state async_read_state;
    static void prepare(async_read_state* o) { io<typename T::type>::prepare(o); }
    static bool accum(int s, async_read_state* o, T* x) { return io<typename T::type>::accum(s, o, &x->value); }
  };
/*****************************
 * END serialization by type / type-family
 *****************************/


// convert C++ compile-time types to hobbes type descriptions
template <typename F>
  struct RPCTyDef {
  };
template <typename R, typename ... Args>
  struct RPCTyDef<R(Args...)> {
    static bytes inputType()  { bytes r; io<std::tuple<Args...>>::encode(&r); return r; }
    static bytes outputType() { bytes r; io<R>::encode(&r); return r; }
  };
template <typename ... Args>
  struct RPCTyDef<void(Args...)> {
    static bytes inputType()  { bytes r; io<std::tuple<Args...>>::encode(&r); return r; }
    static bytes outputType() { bytes r; encode_primty("unit", &r); return r; }
  };

// synchronous request/reply
template <typename F>
  struct RPCFunc {
  };
template <typename R, typename ... Args>
  struct RPCFunc<R(Args...)> {
    RPCFunc(int* socket, uint32_t exprid) : socket(socket), exprid(exprid) { }

    R operator()(const Args&... args) {
      int s = *this->socket;

      io<uint8_t>::write(s, HNET_CMD_INVOKE);
      io<uint32_t>::write(s, this->exprid);
      oSeq<Args...>::write(s, args...);

      R result;
      io<R>::read(s, &result);
      return result;
    }
  private:
    int*     socket;
    uint32_t exprid;
  };
template <typename ... Args>
  struct RPCFunc<void(Args...)> {
    RPCFunc(int* socket, uint32_t exprid) : socket(socket), exprid(exprid) { }

    void operator()(const Args&... args) {
      int s = *this->socket;

      io<uint8_t>::write(s, HNET_CMD_INVOKE);
      io<uint32_t>::write(s, this->exprid);
      oSeq<Args...>::write(s, args...);
    }
  private:
    int*     socket;
    uint32_t exprid;
  };

#define _HNET_CLIENT_MAKE_EXPRID(n, _, __) , exprID_##n
#define _HNET_CLIENT_MAKE_RPCDEF(n, t, e) result.push_back(::hobbes::net::RPCDef((uint32_t)exprID_##n, e, ::hobbes::net::RPCTyDef<t>::inputType(), ::hobbes::net::RPCTyDef<t>::outputType()));
#define _HNET_CLIENT_INIT_RPCFUNC(n, t, _) , n(&this->s, (uint32_t)exprID_##n)
#define _HNET_CLIENT_MAKE_RPCFUNC(n, t, _) ::hobbes::net::RPCFunc<t> n;

#define DEFINE_NET_CLIENT(T, C...) \
  class T { \
  private: \
    int s; \
  public: \
    T(int fd) : s(::hobbes::net::initSession(fd, makeRPCDefs())) _HNET_MAP(_HNET_CLIENT_INIT_RPCFUNC, C) { } \
    T(const std::string& host, size_t port) : T(::hobbes::net::makeConnection(host, port)) { } \
    T(const std::string& host, const std::string& port) : T(::hobbes::net::makeConnection(host, port)) { } \
    T(const std::string& localAddr, const std::string& host, size_t port) : T(::hobbes::net::makeConnection(localAddr, host, port)) { } \
    T(const std::string& localAddr, const std::string& host, const std::string& port) : T(::hobbes::net::makeConnection(localAddr, host, port)) { } \
    T(const std::string& hostport) : T(::hobbes::net::makeConnection(hostport)) { } \
    virtual ~T() { closeC(); } \
    int fd() const { return this->s; } \
    void reconnect(int fd) { closeC(); this->s = ::hobbes::net::initSession(fd, makeRPCDefs()); } \
    void reconnect(const std::string& host, size_t port) { reconnect(::hobbes::net::makeConnection(host, port)); } \
    void reconnect(const std::string& host, const std::string& port) { reconnect(::hobbes::net::makeConnection(host, port)); } \
    void reconnect(const std::string& localAddr, const std::string& host, size_t port) { reconnect(::hobbes::net::makeConnection(localAddr, host, port)); } \
    void reconnect(const std::string& localAddr, const std::string& host, const std::string& port) { reconnect(::hobbes::net::makeConnection(localAddr, host, port)); } \
    void reconnect(const std::string& hostport) { reconnect(::hobbes::net::makeConnection(hostport)); } \
    \
    _HNET_MAP(_HNET_CLIENT_MAKE_RPCFUNC, C) \
  private: \
    enum ExprIDs { \
      NullExpr = 0 \
      _HNET_MAP(_HNET_CLIENT_MAKE_EXPRID, C) \
    }; \
    static ::hobbes::net::RPCDefs makeRPCDefs() { \
      ::hobbes::net::RPCDefs result; \
      _HNET_MAP(_HNET_CLIENT_MAKE_RPCDEF, C) \
      return result; \
    } \
    void closeC() { \
      ::close(this->s); \
    } \
  };

// asynchronous request/reply
struct AsyncReader    { virtual bool readAndFinish() = 0; };
struct AsyncScheduler { virtual void enqueue(AsyncReader*) = 0; };

template <typename F>
  struct AsyncRPCFunc {
  };
template <typename R, typename ... Args>
  struct AsyncRPCFunc<R(Args...)> : public AsyncReader {
    typedef std::function<void(const R&)> K;

    AsyncRPCFunc(AsyncScheduler* sched, int* socket, uint32_t exprid) :
      sched(sched), socket(socket), exprid(exprid)
    {
      io<R>::prepare(&this->pr);
    }

    void operator()(const Args&... args, const K& k) {
      int s = *this->socket;

      // block to write input
      setBlockingBit(s, true);
      io<uint8_t>::write(s, HNET_CMD_INVOKE);
      io<uint32_t>::write(s, this->exprid);
      oSeq<Args...>::write(s, args...);

      // don't block to read output
      setBlockingBit(s, false);
      this->ks.push(k);
      this->sched->enqueue(this);
    }

    bool readAndFinish() {
      if (io<R>::accum(*this->socket, &this->pr, &this->r)) {
        this->ks.front()(this->r);
        this->ks.pop();
        this->r = R();
        io<R>::prepare(&this->pr);
        return true;
      } else {
        return false;
      }
    }
  private:
    AsyncScheduler* sched;
    int*            socket;
    uint32_t        exprid;

    typedef typename io<R>::async_read_state async_read_state;
    typedef std::queue<K> KS;

    KS               ks;
    R                r;
    async_read_state pr;
  };
template <typename ... Args>
  struct AsyncRPCFunc<void(Args...)> {
    AsyncRPCFunc(AsyncScheduler*, int* socket, uint32_t exprid) : socket(socket), exprid(exprid) { }

    void operator()(const Args&... args) {
      int s = *this->socket;

      // block to write input
      setBlockingBit(s, true);
      io<uint8_t>::write(s, HNET_CMD_INVOKE);
      io<uint32_t>::write(s, this->exprid);
      oSeq<Args...>::write(s, args...);

      // don't block to read output
      setBlockingBit(s, false);
    }
  private:
    int*     socket;
    uint32_t exprid;
  };

#define _HNET_CLIENT_INIT_ASYNC_RPCFUNC(n, t, _) , n(this, &this->s, (uint32_t)exprID_##n)
#define _HNET_CLIENT_MAKE_ASYNC_RPCFUNC(n, t, _) ::hobbes::net::AsyncRPCFunc<t> n;

#define DEFINE_ASYNC_NET_CLIENT(T, C...) \
  class T : public ::hobbes::net::AsyncScheduler { \
  private: \
    int s; \
  public: \
    T(int fd) : s(::hobbes::net::initSession(fd, makeRPCDefs())) _HNET_MAP(_HNET_CLIENT_INIT_ASYNC_RPCFUNC, C) { } \
    T(const std::string& host, size_t port) : T(::hobbes::net::makeConnection(host, port)) { } \
    T(const std::string& host, const std::string& port) : T(::hobbes::net::makeConnection(host, port)) { } \
    T(const std::string& localAddr, const std::string& host, size_t port) : T(::hobbes::net::makeConnection(localAddr, host, port)) { } \
    T(const std::string& localAddr, const std::string& host, const std::string& port) : T(::hobbes::net::makeConnection(localAddr, host, port)) { } \
    T(const std::string& hostport) : T(::hobbes::net::makeConnection(hostport)) { } \
    virtual ~T() { closeC(); } \
    int fd() const { return this->s; } \
    void reconnect(int fd) { closeC(); this->s = ::hobbes::net::initSession(fd, makeRPCDefs()); } \
    void reconnect(const std::string& host, size_t port) { reconnect(::hobbes::net::makeConnection(host, port)); } \
    void reconnect(const std::string& host, const std::string& port) { reconnect(::hobbes::net::makeConnection(host, port)); } \
    void reconnect(const std::string& localAddr, const std::string& host, size_t port) { reconnect(::hobbes::net::makeConnection(localAddr, host, port)); } \
    void reconnect(const std::string& localAddr, const std::string& host, const std::string& port) { reconnect(::hobbes::net::makeConnection(localAddr, host, port)); } \
    void reconnect(const std::string& hostport) { reconnect(::hobbes::net::makeConnection(hostport)); } \
    void step() { while (this->asyncReaders.size() > 0 && this->asyncReaders.front()->readAndFinish()) { this->asyncReaders.pop(); } } \
    size_t pendingRequests() const { return this->asyncReaders.size(); } \
    \
    _HNET_MAP(_HNET_CLIENT_MAKE_ASYNC_RPCFUNC, C) \
  private: \
    enum ExprIDs { \
      NullExpr = 0 \
      _HNET_MAP(_HNET_CLIENT_MAKE_EXPRID, C) \
    }; \
    static ::hobbes::net::RPCDefs makeRPCDefs() { \
      ::hobbes::net::RPCDefs result; \
      _HNET_MAP(_HNET_CLIENT_MAKE_RPCDEF, C) \
      return result; \
    } \
    std::queue<::hobbes::net::AsyncReader*> asyncReaders; \
    void enqueue(::hobbes::net::AsyncReader* r) { this->asyncReaders.push(r); } \
    void closeC() { \
      ::close(this->s); \
      this->asyncReaders = std::queue<::hobbes::net::AsyncReader*>(); \
    } \
  };

}}

#endif

