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

#ifndef HOBBES_HNET_H_INCLUDED
#define HOBBES_HNET_H_INCLUDED

#include <hobbes/reflect.H>

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

#define HNET_VERSION     static_cast<uint32_t>(0x00010000)
#define HNET_CMD_DEFEXPR static_cast<uint8_t>(0)
#define HNET_CMD_INVOKE  static_cast<uint8_t>(2)
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
  sendData(socket, reinterpret_cast<const uint8_t*>(&n), sizeof(n));
  sendData(socket, reinterpret_cast<const uint8_t*>(s.data()), n);
}

inline void sendBytes(int socket, const bytes& x) {
  size_t n = x.size();
  sendData(socket, reinterpret_cast<const uint8_t*>(&n), sizeof(n));
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
  recvData(socket, reinterpret_cast<uint8_t*>(&n), sizeof(n));

  x->resize(n);
  recvData(socket, reinterpret_cast<uint8_t*>(&(*x)[0]), n);
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
  return static_cast<size_t>(di);
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
  sendData(s, reinterpret_cast<const uint8_t*>(&version), sizeof(version));

  for (const auto& rpcd : rpcds) {
    uint8_t defCmd = HNET_CMD_DEFEXPR;
    sendData(s, &defCmd, sizeof(defCmd));
    sendData(s, reinterpret_cast<const uint8_t*>(&rpcd.id), sizeof(rpcd.id));
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
  default:             throw std::runtime_error("Unknown error while trying to resolve " + host + ":" + port);
  }
  return 0;
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

/*****************************
 *
 * io<T> : the main interface for type-directed network serialization/deserialization
 *
 *****************************/
template <typename T, typename P = void>
  struct io {
  };

// primitive serialization
#define PRIV_HNET_DEFINE_PRIMTYS(T, n) \
  template <> \
    struct io<T> { \
      static const bool can_memcpy = true; \
      static ty::desc    type()                   { return ty::prim(n); } \
      static void        write(int s, const T& x) { sendData(s, reinterpret_cast<const uint8_t*>(&x), sizeof(x)); } \
      static void        read(int s, T* x)        { recvData(s, reinterpret_cast<uint8_t*>(x), sizeof(T)); } \
      \
      typedef uint8_t async_read_state; \
      static void prepare(uint8_t* o) { *o = 0; } \
      static bool accum(int s, uint8_t* o, T* x) { *o += recvDataPartial(s, reinterpret_cast<uint8_t*>(x) + *o, sizeof(T) - *o); return *o == sizeof(T); } \
    }
PRIV_HNET_DEFINE_PRIMTYS(bool,     "bool");
PRIV_HNET_DEFINE_PRIMTYS(uint8_t,  "byte");
PRIV_HNET_DEFINE_PRIMTYS(char,     "char");
PRIV_HNET_DEFINE_PRIMTYS(int16_t,  "short");
PRIV_HNET_DEFINE_PRIMTYS(uint16_t, "short");
PRIV_HNET_DEFINE_PRIMTYS(int32_t,  "int");
PRIV_HNET_DEFINE_PRIMTYS(uint32_t, "int");
PRIV_HNET_DEFINE_PRIMTYS(int64_t,  "long");
PRIV_HNET_DEFINE_PRIMTYS(uint64_t, "long");
#if defined(__APPLE__) && defined(__MACH__)
PRIV_HNET_DEFINE_PRIMTYS(size_t,   "long");
#endif
PRIV_HNET_DEFINE_PRIMTYS(float,    "float");
PRIV_HNET_DEFINE_PRIMTYS(double,   "double");

// support unit
template <>
  struct io<unit> {
    static const bool can_memcpy = false;

    static ty::desc type()                    { return ty::prim("unit"); }
    static void     write(int s, const unit&) { }
    static void     read (int s, unit*)       { }

    typedef uint8_t async_read_state;
    static void prepare(uint8_t* o) {}
    static bool accum(int s, uint8_t* o, unit* x) { return true; }
  };

// support enumerations
template <typename T>
  struct io<T, typename tbool<T::is_hmeta_enum>::type> {
    static const bool can_memcpy = true;

    static ty::desc type()                   { return ty::enumdef(T::meta()); }
    static void     write(int s, const T& x) { io<uint32_t>::write(s, static_cast<uint32_t>(x.value)); }
    static void     read(int s, T* x)        { io<uint32_t>::read(s, reinterpret_cast<uint32_t*>(&x->value)); }

    typedef uint8_t async_read_state;
    static void prepare(uint8_t* o) { *o = 0; }
    static bool accum(int s, uint8_t* o, T* x) { *o += recvDataPartial(s, reinterpret_cast<uint8_t*>(x) + *o, sizeof(uint32_t) - *o); return *o == sizeof(uint32_t); }
  };

// support variants
template <size_t i, size_t n, typename ... Ts>
  struct descVariantTy {
    typedef typename nth<i, Ts...>::type H;
    typedef descVariantTy<i+1, n, Ts...> Recurse;

    static void ctorDefs(ty::Variant::Ctors* cs) {
      std::ostringstream fn;
      fn << ".f" << i;
      cs->push_back(ty::Variant::Ctor(fn.str(), static_cast<int>(i), io<H>::type()));
    }
    static ty::desc type() {
      ty::Variant::Ctors cs;
      ctorDefs(&cs);
      return ty::variant(cs);
    }
  };
template <size_t n, typename ... Ts>
  struct descVariantTy<n, n, Ts...> {
    static void ctorDefs(ty::Variant::Ctors*) { }
    static ty::desc type()   { return ty::prim("void"); }
  };
template <size_t tag, typename T, typename M>
  struct variantGenWrite {
    static void fn(T* vd, int s) {
      io<T>::write(s, *vd);
    }
  };
template <size_t tag, typename T, typename M>
  struct variantGenRead {
    static void fn(T* vd, int s) {
      new (vd) T();
      io<T>::read(s, vd);
    }
  };
template <typename T>
  struct AsyncStateOf {
    typedef typename io<T>::async_read_state type;
  };
template <size_t tag, typename T, typename M>
  struct variantAsyncInit {
    static void fn(T* p, void* rs) {
      new (p) T();

      typedef typename io<T>::async_read_state RSType;
      new (rs) RSType();
      io<T>::prepare(reinterpret_cast<RSType*>(rs));
    }
  };
template <size_t tag, typename T, typename M>
  struct variantAsyncAccum {
    static bool fn(T* p, int s, void* rs) {
      typedef typename io<T>::async_read_state RSType;
      return io<T>::accum(s, reinterpret_cast<RSType*>(rs), p);
    }
  };
template <typename ... Ctors>
  struct io<variant<Ctors...>> {
    static const bool can_memcpy = false;

    static ty::desc type() { return descVariantTy<0, sizeof...(Ctors), Ctors...>::type(); }

    static void write(int s, const variant<Ctors...>& x) {
      io<uint32_t>::write(s, x.unsafeTag());
      x.template apply<void, variantGenWrite, void, int>(s);
    }
    static void read(int s, variant<Ctors...>* x) {
      io<uint32_t>::read(s, &x->unsafeTag());
      variantApp<void, variantGenRead, void, tuple<Ctors...>, int>::apply(x->unsafeTag(), x->unsafePayload(), s);
    }

    typedef typename io<uint32_t>::async_read_state            TagState;
    typedef typename fmap<AsyncStateOf, tuple<Ctors...>>::type PayloadSAsTuple;
    typedef typename toVariant<PayloadSAsTuple>::type          PayloadState;

    struct async_read_state {
      bool         readTag;
      TagState     tagState;
      PayloadState payloadState;
    };
    static void prepare(async_read_state* o) {
      o->readTag = true;
      io<uint32_t>::prepare(&o->tagState);
    }
    static bool accum(int s, async_read_state* o, variant<Ctors...>* x) {
      if (o->readTag) {
        if (io<uint32_t>::accum(s, &o->tagState, &x->unsafeTag())) {
          o->readTag = false;
          o->payloadState.unsafeTag() = x->unsafeTag();
          variantApp<void, variantAsyncInit, void, tuple<Ctors...>, void*>::apply(x->unsafeTag(), x->unsafePayload(), o->payloadState.unsafePayload());
        }
        return false;
      } else {
        return variantApp<bool, variantAsyncAccum, void, tuple<Ctors...>, int, void*>::apply(x->unsafeTag(), x->unsafePayload(), s, o->payloadState.unsafePayload());
      }
    }
  };

// support variants with named constructors
struct descVariantF {
  ty::Variant::Ctors* ctors;
  descVariantF(ty::Variant::Ctors* ctors) : ctors(ctors) { }

  template <typename T>
    void ctor(const char* n, int id) {
      this->ctors->push_back(ty::Variant::Ctor(n, id, io<T>::type()));
    }
};
template <typename T>
  struct io<T, typename tbool<T::is_hmeta_variant>::type> {
    typedef typename T::as_variant_type VT;
    static const bool can_memcpy = io<VT>::can_memcpy;

    static ty::desc type() {
      ty::Variant::Ctors cs;
      descVariantF f(&cs);
      T::meta(f);
      return ty::variant(cs);
    }

    static void write(int s, const T& x) { io<VT>::write(s, *reinterpret_cast<const VT*>(&x)); }
    static void read (int s, T* x)       { io<VT>::read(s, reinterpret_cast<VT*>(x)); }

    typedef typename io<VT>::async_read_state async_read_state;
    static void prepare(async_read_state* o)            { io<VT>::prepare(o); }
    static bool accum(int s, async_read_state* o, T* x) { return io<VT>::accum(s, o, reinterpret_cast<VT*>(x)); }
  };

// support pairs
template <typename U, typename V>
  struct io<std::pair<U,V>> {
    static const bool can_memcpy = false;

    static ty::desc type() { return ty::tup(-1, io<U>::type(), -1, io<V>::type()); }

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

// support fixed-length array of mem-copyable type
template <typename T, size_t N>
  struct FixedArrTyDesc {
    static ty::desc type() { return ty::array(io<T>::type(), ty::nat(N)); }
  };

template <typename T, size_t N>
  struct FixedArrMemcpyReadWrite {
    static const bool can_memcpy = false;

    static void read(int s, T (*x)[N]) { recvData(s, reinterpret_cast<uint8_t*>(x), sizeof(T) * N); }
    static void write(int s, const T (&x)[N]) { sendData(s, reinterpret_cast<const uint8_t*>(x), sizeof(T) * N); }

    struct async_read_state {
      size_t bytesRead;
    };

    static void prepare(async_read_state* o) {
      o->bytesRead = 0;
    }

    static bool accum(int s, async_read_state* o, T (*x)[N]) {
      const auto len = sizeof(T) * N;
      uint8_t* buf = reinterpret_cast<uint8_t*>(&((*x)[0]));
      o->bytesRead += recvDataPartial(s, buf + o->bytesRead, len - o->bytesRead);
      return o->bytesRead == len;
    }
  };

template <typename T, size_t N>
  struct FixedArrIterReadWrite {
    static const bool can_memcpy = false;

    static void read(int s, T (*x)[N]) {
      for (auto i = 0; i < N; ++i) {
        io<T>::read(s, &(*x)[i]);
      }
    }

    static void write(int s, const T (&x)[N]) {
      for (auto i = 0; i < N; ++i) {
        io<T>::write(s, x[i]);
      }
    }

    struct async_read_state {
      typedef typename io<T>::async_read_state ElemS;

      size_t idx;
      ElemS  elemS;
    };

    static void prepare(async_read_state* o) {
      o->idx = 0;
      io<T>::prepare(&o->elemS);
    }

    static bool accum(int s, async_read_state* o, T (*x)[N]) {
      if (io<T>::accum(s, &o->elemS, &(*x)[o->idx])) {
        ++o->idx;
        io<T>::prepare(&o->elemS);
      }
      return o->idx == N;
    }
  };

template <typename T, size_t N>
struct io<T[N], typename tbool<io<T>::can_memcpy>::type> : FixedArrTyDesc<T, N>, FixedArrMemcpyReadWrite<T, N> {};

template <typename T, size_t N>
struct io<T[N], typename tbool<!io<T>::can_memcpy>::type> : FixedArrTyDesc<T, N>, FixedArrIterReadWrite<T, N> {};

// support vectors of mem-copyable type
template <typename T>
  struct io<std::vector<T>, typename tbool<io<T>::can_memcpy>::type> {
    static const bool can_memcpy = false;
    static ty::desc type() { return ty::array(io<T>::type()); }
    static void write(int s, const std::vector<T>& x) { size_t n = x.size(); io<size_t>::write(s, n); if (n > 0) sendData(s, reinterpret_cast<const uint8_t*>(&x[0]), sizeof(T) * n); }
    static void read(int s, std::vector<T>* x) { size_t n = 0; io<size_t>::read(s, &n); x->resize(n); if (n > 0) recvData(s, reinterpret_cast<uint8_t*>(&(*x)[0]), n); }

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
        uint8_t* buf = reinterpret_cast<uint8_t*>(&(*x)[0]);
        o->bytesRead += recvDataPartial(s, buf + o->bytesRead, o->byteLen - o->bytesRead);
      }
      return !o->readLen && o->bytesRead == o->byteLen;
    }
  };

template <typename T>
  struct io<std::vector<T>, typename tbool<!io<T>::can_memcpy>::type> {
    static const bool can_memcpy = false;
    static ty::desc type() { return ty::array(io<T>::type()); }
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
    static const bool can_memcpy = false;
    static ty::desc type() { return io<std::vector<std::pair<K,T>>>::type(); }
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
          x->insert(std::pair<K,T>(o->k, o->t));
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
    static const bool can_memcpy = false;
    static ty::desc type() { return ty::array(io<char>::type()); }
    static void write(int s, const char* x) { size_t n = strlen(x); io<size_t>::write(s, n); sendData(s, reinterpret_cast<const uint8_t*>(x), n); }
  };
template <>
  struct io<std::string> {
    static const bool can_memcpy = false;
    static ty::desc type() { return ty::array(io<char>::type()); }
    static void write(int s, const std::string& x) { io<size_t>::write(s, x.size()); sendData(s, reinterpret_cast<const uint8_t*>(&x[0]), x.size()); }
    static void read(int s, std::string* x) { size_t n = 0; io<size_t>::read(s, &n); x->resize(n); recvData(s, reinterpret_cast<uint8_t*>(&(*x)[0]), n); }

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
        uint8_t* buf = reinterpret_cast<uint8_t*>(&(*x)[0]);
        o->bytesRead += recvDataPartial(s, buf + o->bytesRead, o->byteLen - o->bytesRead);
      }
      return !o->readLen && o->bytesRead == o->byteLen;
    }
  };

// support tuples
template <size_t i, size_t n, typename ... Fs>
  struct tupInd {
    typedef typename nth<i, Fs...>::type H;
    typedef tuple<Fs...>                 TT;
    typedef typename TT::offs            offs;
    typedef tupInd<i+1, n, Fs...>        Recurse;

    static void accFields(ty::Struct::Fields* fs) {
      std::ostringstream ss;
      ss << ".f" << i;
      fs->push_back(ty::Struct::Field(ss.str(), -1, io<H>::type()));
      Recurse::accFields(fs);
    }
    static ty::desc type() { ty::Struct::Fields fs; accFields(&fs); return ty::record(fs); }

    static void write(int s, const tuple<Fs...>& x) { io<H>::write(s,   x.template at<i>()); Recurse::write(s, x); }
    static void read (int s, tuple<Fs...>*       x) { io<H>::read (s, &x->template at<i>()); Recurse::read(s, x); }
  };
template <size_t n, typename ... Fs>
  struct tupInd<n, n, Fs...> {
    static void accFields(ty::Struct::Fields* fs) { }
    static ty::desc type() { return ty::prim("unit"); }

    static void write(int, const tuple<Fs...>&) { }
    static void read (int, tuple<Fs...>*      ) { }
  };
template <size_t tag, typename T, typename M>
  struct tupAsyncInit {
    static void fn(T*, void* rs) {
      typedef typename io<T>::async_read_state RSType;
      new (rs) RSType();
      io<T>::prepare(reinterpret_cast<RSType*>(rs));
    }
  };
template <size_t tag, typename S, typename Tup>
  struct tupAsyncAcc {
    static bool fn(S* rs, int s, Tup* x) {
      return io<typename tupType<tag, Tup>::type>::accum(s, rs, &x->template at<tag>());
    }
  };

template <typename ... Fs>
  struct io<tuple<Fs...>> {
    static const bool can_memcpy = false;
    static ty::desc type()                          { return tupInd<0, sizeof...(Fs), Fs...>::type(); }
    static void write(int s, const tuple<Fs...>& x) { tupInd<0, sizeof...(Fs), Fs...>::write(s, x); }
    static void read (int s, tuple<Fs...>* x)       { tupInd<0, sizeof...(Fs), Fs...>::read (s, x); }

    typedef typename fmap<AsyncStateOf, tuple<Fs...>>::type SAsTuple;
    typedef typename toVariant<SAsTuple>::type              async_read_state;

    static void prepare(async_read_state* o) {
      o->unsafeTag() = 0;
      variantApp<void, tupAsyncInit, void, tuple<Fs...>, void*>::apply(o->unsafeTag(), o->unsafePayload(), o->unsafePayload());
    }
    static bool accum(int s, async_read_state* o, tuple<Fs...>* x) {
      if (o->template apply<bool, tupAsyncAcc, tuple<Fs...>, int, tuple<Fs...>*>(s, x)) {
        ++o->unsafeTag();
        if (o->unsafeTag() == sizeof...(Fs)) {
          return true;
        } else {
          variantApp<void, tupAsyncInit, void, tuple<Fs...>, void*>::apply(o->unsafeTag(), o->unsafePayload(), o->unsafePayload());
          return false;
        }
      }
      return false;
    }
  };

// support reflective structs
struct defStructF {
  ty::Struct::Fields* fs;
  defStructF(ty::Struct::Fields* fs) : fs(fs) { }

  template <typename T>
    void visit(const char* fname) {
      this->fs->push_back(ty::Struct::Field(fname, -1, io<T>::type()));
    }
};

template <typename T>
  struct io<T, typename tbool<T::is_hmeta_struct>::type> {
    static const bool can_memcpy = false;
    static ty::desc type() { ty::Struct::Fields fs; defStructF df(&fs); T::meta(df); return ty::record(fs); }

    typedef typename T::as_tuple_type TT;
    static void write(int s, const T& x) { io<TT>::write(s, *reinterpret_cast<const TT*>(&x)); }
    static void read (int s, T* x)       { io<TT>::read (s, reinterpret_cast<TT*>(x)); }

    typedef typename io<TT>::async_read_state async_read_state;
    static void prepare(async_read_state* o) { io<TT>::prepare(o); }
    static bool accum(int s, async_read_state* o, T* x) { return io<TT>::accum(s, o, reinterpret_cast<TT*>(x)); }
  };

// sequence serialization
template <typename ... Ts>
  struct oSeq {
    static const size_t count = 0;
    static void write(int, const Ts&...) { }
  };
template <typename T, typename ... Ts>
  struct oSeq<T, Ts...> {
    static const size_t count = 1 + oSeq<Ts...>::count;

    static void write(int socket, const T& x, const Ts&... xs) {
      io<T>::write(socket, x);
      oSeq<Ts...>::write(socket, xs...);
    }
  };

// support opaque type aliases
template <typename T>
  struct io<T, typename tbool<T::is_hmeta_alias>::type> {
    typedef typename T::type RT;
    static const bool can_memcpy = io<RT>::can_memcpy;

    static ty::desc type()                   { return ty::prim(T::name(), io<RT>::type()); }
    static void     write(int s, const T& x) { io<RT>::write(s, x.value); }
    static void     read (int s, T* x)       { io<RT>::read (s, &x->value); }

    typedef typename io<RT>::async_read_state async_read_state;
    static void prepare(async_read_state* o)            { io<RT>::prepare(o); }
    static bool accum(int s, async_read_state* o, T* x) { return io<typename T::type>::accum(s, o, &x->value); }
  };

/*****************************
 *
 * RPC interfaces coordinating queries against remote processes
 *
 *****************************/

// convert C++ compile-time types to hobbes type descriptions
template <typename F>
  struct RPCTyDef {
  };
template <typename R, typename ... Args>
  struct RPCTyDef<R(Args...)> {
    static bytes inputType()  { return ty::encoding(io<tuple<Args...>>::type()); }
    static bytes outputType() { return ty::encoding(io<R>::type()); }
  };
template <typename ... Args>
  struct RPCTyDef<void(Args...)> {
    static bytes inputType()  { return ty::encoding(io<tuple<Args...>>::type()); }
    static bytes outputType() { return ty::encoding(ty::prim("unit")); }
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

#define PRIV_HNET_CLIENT_MAKE_EXPRID(n, _, __) , exprID_##n
#define PRIV_HNET_CLIENT_MAKE_RPCDEF(n, t, e) result.push_back(::hobbes::net::RPCDef(static_cast<uint32_t>(exprID_##n), e, ::hobbes::net::RPCTyDef<t>::inputType(), ::hobbes::net::RPCTyDef<t>::outputType()));
#define PRIV_HNET_CLIENT_INIT_RPCFUNC(n, t, _) , n(&this->s, static_cast<uint32_t>(exprID_##n))
#define PRIV_HNET_CLIENT_MAKE_RPCFUNC(n, t, _) ::hobbes::net::RPCFunc<t> n;

#define DEFINE_NET_CLIENT(T, C...) \
  class T { \
  private: \
    int s; \
  public: \
    T(int fd) : s(::hobbes::net::initSession(fd, makeRPCDefs())) PRIV_HPPF_MAP(PRIV_HNET_CLIENT_INIT_RPCFUNC, C) { } \
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
    PRIV_HPPF_MAP(PRIV_HNET_CLIENT_MAKE_RPCFUNC, C) \
  private: \
    enum ExprIDs { \
      NullExpr = 0 \
      PRIV_HPPF_MAP(PRIV_HNET_CLIENT_MAKE_EXPRID, C) \
    }; \
    static ::hobbes::net::RPCDefs makeRPCDefs() { \
      ::hobbes::net::RPCDefs result; \
      PRIV_HPPF_MAP(PRIV_HNET_CLIENT_MAKE_RPCDEF, C) \
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

#define PRIV_HNET_CLIENT_INIT_ASYNC_RPCFUNC(n, t, _) , n(this, &this->s, static_cast<uint32_t>(exprID_##n))
#define PRIV_HNET_CLIENT_MAKE_ASYNC_RPCFUNC(n, t, _) ::hobbes::net::AsyncRPCFunc<t> n;

#define DEFINE_ASYNC_NET_CLIENT(T, C...) \
  class T : public ::hobbes::net::AsyncScheduler { \
  private: \
    int s; \
  public: \
    T(int fd) : s(::hobbes::net::initSession(fd, makeRPCDefs())) PRIV_HPPF_MAP(PRIV_HNET_CLIENT_INIT_ASYNC_RPCFUNC, C) { } \
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
    PRIV_HPPF_MAP(PRIV_HNET_CLIENT_MAKE_ASYNC_RPCFUNC, C) \
  private: \
    enum ExprIDs { \
      NullExpr = 0 \
      PRIV_HPPF_MAP(PRIV_HNET_CLIENT_MAKE_EXPRID, C) \
    }; \
    static ::hobbes::net::RPCDefs makeRPCDefs() { \
      ::hobbes::net::RPCDefs result; \
      PRIV_HPPF_MAP(PRIV_HNET_CLIENT_MAKE_RPCDEF, C) \
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

