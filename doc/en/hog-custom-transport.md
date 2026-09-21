# Custom hog transports (authentication / encryption)

`hog` reaches the network only through two abstract interfaces:

- `hog::NetServer` — `accept()` returns a `hog::NetConnection` (`bin/hog/netserver.H`)
- `hog::NetConnection` — `send` / `sendFile` / `receive` / `remoteHost` / `remotePort`
  (`bin/hog/netconnection.H`)

Both the receive server (`batchrecv`) and the send path (`batchsend`) obtain their
transport from injectable factories in `bin/hog/network.H`:

```cpp
void setNetServerFactory(hog::NetServerFactory);         // receive side (the listener)
void setNetConnectionFactory(hog::NetConnectionFactory); // send side (outbound connections)
```

A consumer that wants authenticated and/or encrypted transport implements the two
interfaces and installs them at startup — **before** the receive server or any
senders start. The default (unauthenticated, plaintext) transport is used whenever
no factory is installed, so upstream hog carries no dependency on any auth library.

The wire format is unchanged for the default transport: a custom transport only
changes what flows once you install it.

## Worked example: Kerberos via GSSAPI

This example lives entirely in the **consumer's** tree and links against
`-lgssapi_krb5`; nothing here is compiled into upstream hog. The mechanism is a
pair of decorators around the built-in transport:

- `GssNetConnection` performs per-message `gss_wrap`/`gss_unwrap` (integrity +
  confidentiality) over an established security context.
- `GssNetServer` decorates `DefaultNetServer`: after the inner `accept()` it runs
  the server side of the GSSAPI handshake, then hands back a `GssNetConnection`.
- The send side connects, runs the client side of the handshake, and wraps the fd
  in a `GssNetConnection`.

Two impedance mismatches to be aware of, both handled below:

1. **`receive(buf, N)` asks for exactly N bytes; GSSAPI is message-oriented.** A
   wrapped token is variable-length and self-delimiting, so you cannot unwrap
   "8 bytes". Keep an internal plaintext buffer (a small record layer, like TLS):
   each `receive` is served from it, refilling by reading and unwrapping the next
   whole token when it drains.
2. **`sendFile` uses zero-copy `sendfile(2)` and cannot be wrapped.** Fall back to
   reading the file in chunks, wrapping each, and sending it.

### Token framing helpers

GSSAPI handshake and message tokens are opaque byte blobs; frame each with a
length prefix. These helpers operate directly on the fd (the handshake happens
before any `NetConnection` exists):

```cpp
#include <gssapi/gssapi.h>
#include <cstdint>
#include <stdexcept>
#include <vector>
#include <unistd.h>

namespace {

void writeN(int fd, const void* p, size_t n) {
  const auto* b = static_cast<const uint8_t*>(p);
  while (n) {
    ssize_t w = ::write(fd, b, n);
    if (w < 0) { if (errno == EINTR) continue; throw std::runtime_error("write failed"); }
    b += w; n -= size_t(w);
  }
}

void readN(int fd, void* p, size_t n) {
  auto* b = static_cast<uint8_t*>(p);
  while (n) {
    ssize_t r = ::read(fd, b, n);
    if (r < 0) { if (errno == EINTR) continue; throw std::runtime_error("read failed"); }
    if (r == 0) throw std::runtime_error("connection closed mid-token");
    b += r; n -= size_t(r);
  }
}

void sendToken(int fd, gss_buffer_t t) {
  uint64_t len = t->length;
  writeN(fd, &len, sizeof(len));
  writeN(fd, t->value, t->length);
}

std::vector<uint8_t> recvToken(int fd) {
  uint64_t len = 0;
  readN(fd, &len, sizeof(len));
  if (len > (64u << 20)) throw std::runtime_error("token too large"); // cap: reject hostile lengths
  std::vector<uint8_t> v(len);
  if (len) readN(fd, v.data(), len);
  return v;
}

void check(OM_uint32 major, OM_uint32 minor, const char* what) {
  if (GSS_ERROR(major)) throw std::runtime_error(std::string("GSSAPI ") + what + " failed");
  (void)minor;
}

} // namespace
```

### The connection decorator

```cpp
#include "netconnection.H"

class GssNetConnection : public hog::NetConnection {
public:
  GssNetConnection(int fd, gss_ctx_id_t ctx) : fd(fd), ctx(ctx) {}
  ~GssNetConnection() override {
    OM_uint32 minor;
    gss_delete_sec_context(&minor, &ctx, GSS_C_NO_BUFFER);
    ::close(fd);
  }

  bool send(const void* buf, size_t size) override {
    gss_buffer_desc in{size, const_cast<void*>(buf)};
    gss_buffer_desc out{0, nullptr};
    OM_uint32 minor;
    int confState = 0;
    OM_uint32 major = gss_wrap(&minor, ctx, /*conf_req=*/1, GSS_C_QOP_DEFAULT,
                               &in, &confState, &out);
    if (GSS_ERROR(major)) return false;
    try { sendToken(fd, &out); } catch (...) { gss_release_buffer(&minor, &out); return false; }
    gss_release_buffer(&minor, &out);
    return true;
  }

  bool receive(void* buf, size_t size) override {
    auto* dst = static_cast<uint8_t*>(buf);
    while (size) {
      if (inbuf.empty()) { if (!refill()) return false; }
      size_t take = std::min(size, inbuf.size() - pos);
      std::memcpy(dst, inbuf.data() + pos, take);
      dst += take; pos += take; size -= take;
      if (pos == inbuf.size()) { inbuf.clear(); pos = 0; }
    }
    return true;
  }

  bool sendFile(int filefd) override {
    struct stat sb; if (fstat(filefd, &sb) < 0) return false;
    uint64_t fsize = sb.st_size;
    if (!send(&fsize, sizeof(fsize))) return false;   // matches DefaultNetConnection's header
    uint8_t chunk[64 * 1024]; off_t left = sb.st_size;
    while (left > 0) {
      ssize_t r = ::read(filefd, chunk, sizeof(chunk));
      if (r < 0) { if (errno == EINTR) continue; return false; }
      if (r == 0) break;
      if (!send(chunk, size_t(r))) return false;      // wrap each chunk
      left -= r;
    }
    return true;
  }

  std::string remoteHost() override { return hobbes::remoteHostname(fd); }
  int         remotePort() override { return hobbes::remotePort(fd); }

private:
  bool refill() {
    try {
      auto tok = recvToken(fd);
      gss_buffer_desc in{tok.size(), tok.data()};
      gss_buffer_desc out{0, nullptr};
      OM_uint32 minor; int confState = 0; gss_qop_t qop = GSS_C_QOP_DEFAULT;
      OM_uint32 major = gss_unwrap(&minor, ctx, &in, &out, &confState, &qop);
      if (GSS_ERROR(major)) return false;
      inbuf.assign(static_cast<uint8_t*>(out.value),
                   static_cast<uint8_t*>(out.value) + out.length);
      pos = 0;
      gss_release_buffer(&minor, &out);
      return true;
    } catch (...) { return false; }
  }

  int fd;
  gss_ctx_id_t ctx;
  std::vector<uint8_t> inbuf; // plaintext record layer
  size_t pos = 0;
};
```

> Note: `sendFile`'s chunked framing means the receiver must read the file body via
> the same `receive` path (which it does — `batchsend`/`batchrecv` exchange sizes
> then bodies over the `NetConnection`). If you rely on `sendFile`, keep the sender
> and receiver transports symmetric.

### The server decorator

```cpp
#include "netserver.H"
#include "network.H"

class GssNetServer : public hog::NetServer {
public:
  explicit GssNetServer(std::unique_ptr<hog::NetServer> inner) : inner(std::move(inner)) {}

  std::unique_ptr<hog::NetConnection> accept() override {
    auto raw = inner->accept();
    if (!raw) return {};
    int fd = /* obtain the accepted fd */ fdOf(*raw);

    gss_ctx_id_t ctx = GSS_C_NO_CONTEXT;
    gss_cred_id_t noCred = GSS_C_NO_CREDENTIAL; // acceptor cred from the keytab (KRB5_KTNAME)
    OM_uint32 minor, major;
    do {
      auto tok = recvToken(fd);
      gss_buffer_desc in{tok.size(), tok.data()};
      gss_buffer_desc out{0, nullptr};
      major = gss_accept_sec_context(&minor, &ctx, noCred, &in, GSS_C_NO_CHANNEL_BINDINGS,
                                     nullptr, nullptr, &out, nullptr, nullptr, nullptr);
      if (out.length) { sendToken(fd, &out); gss_release_buffer(&minor, &out); }
      if (GSS_ERROR(major)) { gss_delete_sec_context(&minor, &ctx, GSS_C_NO_BUFFER); return {}; }
    } while (major & GSS_S_CONTINUE_NEEDED);

    return std::make_unique<GssNetConnection>(fd, ctx);
  }

private:
  std::unique_ptr<hog::NetServer> inner;
};
```

`fdOf`/`hobbes::remoteHostname` above assume the accepted fd is reachable. The
simplest robust approach is to have `GssNetServer` do its own `accept(2)` (mirror
`DefaultNetServer`, which is ~10 lines) rather than decorate `DefaultNetServer`, so
it directly owns the fd it hands to `GssNetConnection`.

### Client side (send path)

```cpp
std::unique_ptr<hog::NetConnection> makeGssClient(const std::string& hostport) {
  int fd = hobbes::connectSocket(hostport);
  if (fd < 0) throw std::runtime_error("connect failed: " + hostport);

  gss_name_t target = importServicePrincipal(hostport); // e.g. "hog@<host>" as GSS_C_NT_HOSTBASED_SERVICE
  gss_ctx_id_t ctx = GSS_C_NO_CONTEXT;
  gss_buffer_desc in{0, nullptr};
  OM_uint32 minor, major;
  bool first = true;
  do {
    gss_buffer_desc out{0, nullptr};
    major = gss_init_sec_context(&minor, GSS_C_NO_CREDENTIAL, &ctx, target,
                                 GSS_C_NO_OID, GSS_C_MUTUAL_FLAG | GSS_C_CONF_FLAG | GSS_C_INTEG_FLAG,
                                 0, GSS_C_NO_CHANNEL_BINDINGS,
                                 first ? GSS_C_NO_BUFFER : &in, nullptr, &out, nullptr, nullptr);
    first = false;
    if (out.length) { sendToken(fd, &out); gss_release_buffer(&minor, &out); }
    if (major & GSS_S_CONTINUE_NEEDED) { auto t = recvToken(fd); in = {t.size(), t.data()}; }
  } while (major & GSS_S_CONTINUE_NEEDED);

  return std::make_unique<GssNetConnection>(fd, ctx);
}
```

### Installing the transport

In the consumer's `main`, before starting hog's receive server or senders:

```cpp
#include "network.H"

int main(int argc, char** argv) {
  hog::setNetServerFactory([](const std::string& hostport) {
    return std::make_unique<GssNetServer>(std::make_unique<hog::DefaultNetServer>(hostport));
  });
  hog::setNetConnectionFactory(makeGssClient);

  return hogMain(argc, argv); // hog's normal entry
}
```

From here, `batchrecv` rejects any client that cannot complete the Kerberos
handshake, and every subsequent message is integrity- and confidentiality-
protected — with no change to hog's storage, session, or protocol code, and no
GSSAPI dependency in the default build.

### Operational notes

- The acceptor identity comes from the keytab (`KRB5_KTNAME`); the initiator uses
  the ambient credential cache (`KRB5CCNAME`). These are deployment configuration,
  not hog options.
- Cap token sizes on read (shown above) so a peer cannot force an unbounded
  allocation before the handshake even completes.
- Authentication proves *who* the peer is; it does not by itself validate message
  contents. Input-validation hardening of the receive path (group-name checks,
  bounds checks in the deserializers) is still worthwhile if any authenticated
  principal is not fully trusted.
