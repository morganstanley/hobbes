# Custom netREPL authentication

A net REPL listener (`installNetREPL`, `hi -p`) lets a connected peer compile
and run native code in the server process, so who may connect matters. Out of
the box a listener can require a shared secret. An embedding application that
needs something else -- Kerberos/GSSAPI, a signed token, a challenge-response
against its own user table, the peer credentials of a Unix socket -- can supply
its own handshake on both sides, without libhobbes depending on any such
library.

This is the netREPL counterpart of [hog's pluggable transport](hog-custom-transport.md),
with one deliberate difference in scope: it replaces the **authentication
handshake**, not the transport.

> **Authentication only, not encryption.** After the handshake the connection
> is the same plaintext socket it always was. netREPL reads and writes the raw
> descriptor from the event loop, from the header-only C++ client and from
> JIT-compiled Hobbes code, so there is no single place to wrap it. For a
> remote peer, keep the listener on loopback (or a Unix socket) and carry the
> connection through a same-host TLS proxy or an SSH/stunnel tunnel. A
> handshake authenticates whoever opened the connection; it does not stop an
> attacker on the path from reading or altering what follows.

## The shape of a handshake

A handshake is an exchange of opaque tokens, begun by the client and strictly
alternating:

1. the client sends a token;
2. the server answers *accepted*, *rejected*, or *continue* with a token of its
   own, in which case the client sends another token, and so on.

That is the shape of GSSAPI and SASL, and one round of it is a bearer token or
a shared secret. You write the two halves of the token logic; libhobbes frames
the tokens, moves them, and enforces the limits. **Neither half ever touches
the socket.**

| | Server (libhobbes, `hobbes/ipc/net.H`) | Client (header-only, `hobbes/netauth.H`) |
|---|---|---|
| per listener / per client | `hobbes::NetREPLAuthenticator` | `hobbes::net::NetREPLClientAuthenticator` |
| per connection | `hobbes::NetREPLAuthSession` | `hobbes::net::NetREPLClientAuthSession` |
| built in | `sharedSecretAuthenticator(secret)` | `NetREPLSharedSecret(secret)` |

Both sides name their mechanism (`mechanism()`), and the names must match.

### Server side

```cpp
struct NetREPLAuthSession {
  enum class Result { Continue, Accept, Reject };
  virtual Result step(const std::string& token, std::string* reply) = 0;
  virtual std::string peerIdentity() const;       // read once, after Accept
};

struct NetREPLAuthenticator {
  virtual std::string mechanism() const = 0;
  virtual size_t maxTokenBytes() const;            // default 64KB
  virtual std::unique_ptr<NetREPLAuthSession> newSession(int conn) = 0;
};

int installNetREPL(int port, Server*, const std::shared_ptr<NetREPLAuthenticator>&);
int installNetREPL(int port, cc*,     const std::shared_ptr<NetREPLAuthenticator>&);
// ... and the host/port and Unix-socket overloads of each
```

The server's half is **driven by the event loop, a token at a time**. The loop
that serves a net REPL listener serves every other listener in the process too
(another net REPL, `hi`'s web server), so nothing in a handshake may wait on a
peer. libhobbes reads each client token with non-blocking reads, keeps the
partial token per connection, and calls `step()` only when a whole token has
arrived. A peer that stalls anywhere -- mid-name, mid-token, after a challenge
-- holds its own descriptor and nothing else.

What that asks of an authenticator:

- **`step()` must not block.** It runs on the shared event loop. Work from the
  token and local state: a keytab, a public key, a table of users.
  `gss_accept_sec_context` against a local keytab is fine; a call out to a
  token-introspection service or a directory is not -- verify tokens offline,
  or have a fronting proxy do the lookup.
- **Keep reply tokens small.** A reply is sent with one non-blocking send. If
  it does not fit the socket's send buffer the connection is dropped rather
  than waited on. Handshake replies (a challenge, a GSSAPI mutual-auth token)
  are tens to hundreds of bytes, which always fits a fresh connection.
- **`newSession(conn)` may look at the connection, not use it.** `getpeername`
  or `SO_PEERCRED`/`getpeereid` on a Unix socket are fine. Do not read, write
  or close it.
- **Decide authorization in `step()`.** Returning `Reject` (or throwing) tells
  the client its handshake was refused. A null session rejects too.

A peer is dropped, without `step()` being called, if it announces a token
larger than `maxTokenBytes()` (which may be set to at most 1MB) or a mechanism
name longer than 64 bytes. A handshake is cut off after 32 rounds. Anything an
authenticator throws refuses the peer.

### Who the peer is

Once a session accepts, the listener calls

```cpp
virtual void Server::authenticated(int conn, const std::string& mechanism, const std::string& identity);
```

with the session's `peerIdentity()`, and then `Server::connect(conn)` as usual.
It is called on every authenticated listener (for a shared secret, with
mechanism `"shared-secret"` and an empty identity) and never on an
unauthenticated one. The default does nothing, so existing `Server`
implementations are unaffected. A `Server` that wants per-peer policy keeps
the identity by `conn` and forgets it in `disconnect(conn)`. Throwing from
`authenticated` drops the connection without calling `connect` or
`disconnect` (as when `connect` itself throws), so forget anything noted for
`conn` before throwing. The client has by then been told it was accepted and
will see the connection close, so prefer rejecting in `step()`.

The `cc*` overloads of `installNetREPL` evaluate whatever an authenticated
peer sends; they have no per-identity policy. Either reject unwanted
principals in the authenticator, or implement `Server` yourself.

### Client side

```cpp
class NetREPLClientAuthSession {
  virtual std::string next(const std::string& serverToken) = 0;  // "" on the first call
  virtual void accepted(const std::string& finalToken) {}        // throw if the server is not verified
};

class NetREPLClientAuthenticator {
  virtual std::string mechanism() const = 0;
  virtual size_t maxTokenBytes() const;
  virtual std::unique_ptr<NetREPLClientAuthSession> begin() const = 0;
};
```

The client's half blocks, as opening a session always has. Pass the
authenticator wherever a `NetREPLSharedSecret` is accepted today:

```cpp
// the header-only client (no libhobbes needed)
DEFINE_NET_CLIENT(Connection, (add, int(int,int), "\\x y.x+y"));
MyClientAuth auth(/* ... */);
Connection c("localhost:8080", auth);
c.reconnect("localhost:8080", auth);

// or by hand on a connected socket
int fd = hobbes::net::initSession(hobbes::net::makeConnection("localhost:8080"), rpcDefs, auth);

// libhobbes' client
hobbes::Client client("localhost:8080", auth);

// Connect constraints in Hobbes code, keyed by the exact host:port they name
hobbes::cc c;
c.enableRemoteConnections({"localhost:8080"});
c.setRemoteConnectionAuthenticator("localhost:8080", std::make_shared<MyClientAuth>(/* ... */));
// > conn = connection :: (Connect "localhost:8080" p) => p
```

If the handshake fails, opening the session throws
`netREPL authentication rejected: ...` -- the server refused the handshake, it
uses a different mechanism, or it hung up (an unauthenticated listener, or a
shared-secret one, does not speak named mechanisms at all). Exceptions thrown
by `next()` or `accepted()` propagate unchanged.

A mechanism that authenticates the server as well should check it in
`accepted()` and throw if the server has not proven itself: the session is not
opened.

`hi` has no flag for this -- it cannot load an authenticator. `hi -p` keeps
`--netrepl-secret-file`; a custom mechanism is for applications that embed
hobbes.

## Sketch: Kerberos via GSSAPI

This lives entirely in the **consumer's** tree and links `-lgssapi_krb5`;
nothing here is compiled into hobbes. It is a sketch to show how the pieces
fit, not a vetted implementation: error reporting, credential selection and
the authorization policy are yours to fill in.

The mapping is direct. `gss_init_sec_context` and `gss_accept_sec_context`
each take the peer's last token and produce the next one, which is exactly
`next()` and `step()`; the framing GSSAPI leaves to the application is what
libhobbes provides. With mutual authentication, Kerberos is one round: the
client's token, then the server's acceptance carrying its own token.

### Server

```cpp
#include <gssapi/gssapi.h>
#include <hobbes/ipc/net.H>

class GssSession : public hobbes::NetREPLAuthSession {
public:
  ~GssSession() override {
    OM_uint32 minor = 0;
    if (ctx != GSS_C_NO_CONTEXT) gss_delete_sec_context(&minor, &ctx, GSS_C_NO_BUFFER);
  }

  // uses the acceptor credentials in the default keytab (KRB5_KTNAME): no
  // network round trip, so it is safe on the event loop
  Result step(const std::string& token, std::string* reply) override {
    gss_buffer_desc in  = { token.size(), const_cast<char*>(token.data()) };
    gss_buffer_desc out = GSS_C_EMPTY_BUFFER;
    gss_name_t      client = GSS_C_NO_NAME;
    OM_uint32       minor = 0;

    OM_uint32 major = gss_accept_sec_context(
        &minor, &ctx, GSS_C_NO_CREDENTIAL, &in, GSS_C_NO_CHANNEL_BINDINGS,
        &client, nullptr, &out, nullptr, nullptr, nullptr);

    if (out.length > 0) reply->assign(static_cast<const char*>(out.value), out.length);
    gss_release_buffer(&minor, &out);

    Result result = Result::Reject;
    if (major == GSS_S_CONTINUE_NEEDED) {
      result = Result::Continue;
    } else if (major == GSS_S_COMPLETE) {
      gss_buffer_desc name = GSS_C_EMPTY_BUFFER;
      if (gss_display_name(&minor, client, &name, nullptr) == GSS_S_COMPLETE) {
        principal.assign(static_cast<const char*>(name.value), name.length);
        gss_release_buffer(&minor, &name);
        // your policy: an allowlist, a group lookup done ahead of time, ...
        // (compare canonical names in real code; a display name is for people)
        if (mayUseThisREPL(principal)) {
          result = Result::Accept;         // 'reply' (the mutual-auth token) goes out with the acceptance
        }
      }
    }
    if (client != GSS_C_NO_NAME) gss_release_name(&minor, &client);
    return result;
  }

  std::string peerIdentity() const override { return principal; }  // e.g. "alice@EXAMPLE.COM"

private:
  gss_ctx_id_t ctx = GSS_C_NO_CONTEXT;
  std::string  principal;
};

class GssAuthenticator : public hobbes::NetREPLAuthenticator {
public:
  std::string mechanism() const override { return "gssapi-krb5"; }
  std::unique_ptr<hobbes::NetREPLAuthSession> newSession(int) override {
    return std::unique_ptr<hobbes::NetREPLAuthSession>(new GssSession());
  }
};

// at startup, on the thread that runs the event loop (loopback; remote
// clients arrive through a tunnel or proxy on this host)
hobbes::installNetREPL(8080, &compiler, std::make_shared<GssAuthenticator>());
```

### Client

```cpp
#include <gssapi/gssapi.h>
#include <hobbes/net.H>

class GssClientSession : public hobbes::net::NetREPLClientAuthSession {
public:
  explicit GssClientSession(const std::string& service) {   // e.g. "hobbes@server.example.com"
    gss_buffer_desc name = { service.size(), const_cast<char*>(service.data()) };
    OM_uint32 minor = 0;
    if (gss_import_name(&minor, &name, GSS_C_NT_HOSTBASED_SERVICE, &target) != GSS_S_COMPLETE) {
      throw std::runtime_error("cannot name the service " + service);
    }
  }
  ~GssClientSession() override {
    OM_uint32 minor = 0;
    if (ctx != GSS_C_NO_CONTEXT) gss_delete_sec_context(&minor, &ctx, GSS_C_NO_BUFFER);
    gss_release_name(&minor, &target);
  }

  std::string next(const std::string& serverToken) override { return advance(serverToken); }

  // the server's acceptance carries its half of mutual authentication; a
  // server that cannot produce it is not the service we asked for
  void accepted(const std::string& finalToken) override {
    if (!finalToken.empty()) advance(finalToken);
    if (!established) throw std::runtime_error("the server did not complete mutual authentication");
  }

private:
  // uses the caller's ticket cache; may contact the KDC, which is fine here:
  // the client's half blocks anyway
  std::string advance(const std::string& serverToken) {
    gss_buffer_desc in  = { serverToken.size(), const_cast<char*>(serverToken.data()) };
    gss_buffer_desc out = GSS_C_EMPTY_BUFFER;
    OM_uint32       minor = 0, flags = 0;

    OM_uint32 major = gss_init_sec_context(
        &minor, GSS_C_NO_CREDENTIAL, &ctx, target, GSS_C_NO_OID,
        GSS_C_MUTUAL_FLAG, 0, GSS_C_NO_CHANNEL_BINDINGS,
        serverToken.empty() ? GSS_C_NO_BUFFER : &in, nullptr, &out, &flags, nullptr);

    std::string token;
    if (out.length > 0) token.assign(static_cast<const char*>(out.value), out.length);
    gss_release_buffer(&minor, &out);
    if (GSS_ERROR(major)) throw std::runtime_error("gss_init_sec_context failed");
    established = major == GSS_S_COMPLETE && (flags & GSS_C_MUTUAL_FLAG) != 0;
    return token;
  }

  gss_ctx_id_t ctx = GSS_C_NO_CONTEXT;
  gss_name_t   target = GSS_C_NO_NAME;
  bool         established = false;
};

class GssClientAuth : public hobbes::net::NetREPLClientAuthenticator {
public:
  explicit GssClientAuth(const std::string& service) : service(service) {}
  std::string mechanism() const override { return "gssapi-krb5"; }
  std::unique_ptr<hobbes::net::NetREPLClientAuthSession> begin() const override {
    return std::unique_ptr<hobbes::net::NetREPLClientAuthSession>(new GssClientSession(service));
  }
private:
  std::string service;
};

// through a tunnel to the server's loopback listener; the service name is
// the server's, whatever address the tunnel is reached at
Connection c("localhost:8080", GssClientAuth("hobbes@server.example.com"));
```

The security context is established and then discarded: netREPL has nowhere to
apply `gss_wrap` to the session that follows. That is the limit stated at the
top -- Kerberos here answers "who connected", and a tunnel still has to answer
"who can read it".

## Wire format

The first word a client sends selects how the session opens. Lengths are
native `size_t`, as everywhere else in this protocol.

| Listener installed with | Client's version word | Then |
|---|---|---|
| nothing (unauthenticated) | `0x00010000` | requests |
| a shared secret (mechanism `shared-secret`) | `0x00010001` | `[size_t n][secret]`, answered by one status byte |
| any other mechanism | `0x00010002` | `[size_t n][mechanism name]`, then token rounds |

A token round is `[size_t n][token]` from the client, answered by a status
byte from the server:

| Status | Meaning | Followed by |
|---|---|---|
| `0` | rejected; the server hangs up | |
| `1` | accepted | |
| `2` | continue | `[size_t n][server token]`; the client sends another token |
| `3` | the listener uses a different mechanism; the server hangs up | |
| `4` | accepted | `[size_t n][final server token]` |

The shared-secret format is unchanged from earlier releases: it is exactly a
one-round exchange in this framing, and is implemented as the built-in
`shared-secret` mechanism. That name is reserved for this wire format (no
mechanism name on the wire, one token, a bare `1` or `0`); an authenticator
that returns it speaks to existing shared-secret peers, which is how to check
a secret against more than one accepted value during a rotation.

A listener hangs up without answering on a version word other than its own.
An unauthenticated listener is unchanged: it accepts `0x00010000` and nothing
else, and never calls `Server::authenticated`.

## Operational notes

- **No handshake deadline.** A peer that connects and never finishes holds a
  descriptor (not the event loop) until it goes away, as with the shared
  secret. Limit who can reach the port.
- **Install before serving.** The authenticator is read once
  (`mechanism()`, `maxTokenBytes()`) when the listener is installed and is
  kept for the life of the listener. `newSession` and `step` are called on the
  thread running that listener's event loop.
- **A null authenticator is an error**, not an unauthenticated listener:
  `installNetREPL` and `setRemoteConnectionAuthenticator` throw.
- **Replay and binding are the mechanism's job.** A static bearer token sent
  over a plaintext hop can be captured and replayed, exactly like the shared
  secret; use a challenge or a mechanism with its own replay protection
  (Kerberos has one) if the hop is not private.
