/*
 * netauth : the client side of a net REPL authentication handshake
 *
 *   A net REPL listener may require a peer to authenticate before it accepts
 *   any command. The handshake is an exchange of opaque tokens: the client
 *   sends one, the server answers "accepted", "rejected" or "continue" with a
 *   token of its own, and so on. What the tokens mean is up to the
 *   authenticator installed on both sides -- a shared secret (built in), a
 *   Kerberos/GSSAPI context, a signed token -- and libhobbes links against
 *   none of them. See doc/en/netrepl-custom-authentication.md.
 *
 *   This authenticates the peer when the connection is opened. It does not
 *   encrypt or integrity-protect what follows: for a remote peer, keep the
 *   listener on loopback and carry the connection through a same-host TLS
 *   proxy or tunnel.
 *
 *   This header is self-contained so the header-only client in net.H can use
 *   it without linking libhobbes.
 */

#ifndef HOBBES_NETAUTH_H_INCLUDED
#define HOBBES_NETAUTH_H_INCLUDED

#include <cerrno>
#include <cstdint>
#include <cstring>
#include <memory>
#include <stdexcept>
#include <string>

#include <sys/socket.h>
#include <sys/types.h>

namespace hobbes { namespace net {

// the first word a client sends says how it means to open the session
static constexpr uint32_t netREPLVersion             = 0x00010000; // no authentication
static constexpr uint32_t netREPLSharedSecretVersion = 0x00010001; // the built-in shared secret
static constexpr uint32_t netREPLMechanismVersion    = 0x00010002; // a named mechanism

// the server's answer to each client token
static constexpr uint8_t netREPLAuthRejected          = 0;
static constexpr uint8_t netREPLAuthAccepted          = 1;
static constexpr uint8_t netREPLAuthContinue          = 2; // a server token follows; send another
static constexpr uint8_t netREPLAuthWrongMechanism    = 3; // the listener uses another mechanism
static constexpr uint8_t netREPLAuthAcceptedWithToken = 4; // accepted, and a last server token follows

static constexpr size_t maxNetREPLSharedSecretBytes    = 4096;
static constexpr size_t maxNetREPLMechanismNameBytes   = 64;
static constexpr size_t defaultMaxNetREPLAuthTokenBytes = 64 * 1024;
static constexpr size_t maxNetREPLAuthTokenBytesLimit   = 1024 * 1024; // the most a listener may be set to buffer
static constexpr size_t maxNetREPLAuthRounds           = 32;

// The name of the built-in mechanism. It is the one mechanism with a wire
// format of its own (netREPLSharedSecretVersion, no name, one token, a bare
// accepted/rejected answer), which is the format shared secrets have always
// used; every other mechanism is announced by name.
inline const std::string& netREPLSharedSecretMechanism() {
  static const std::string name = "shared-secret";
  return name;
}

// The client's half of one handshake, on one connection.
class NetREPLClientAuthSession {
public:
  virtual ~NetREPLClientAuthSession() = default;

  // The next token to send. 'serverToken' is empty on the first call and is
  // the server's "continue" token on each later one. Throw to abandon the
  // handshake.
  virtual std::string next(const std::string& serverToken) = 0;

  // The server accepted. 'finalToken' is the token it sent with its
  // acceptance, or empty if it sent none. A mechanism that authenticates the
  // server as well (GSSAPI mutual authentication) verifies it here and throws
  // if the server has not proven itself.
  virtual void accepted(const std::string& /*finalToken*/) {}
};

// How a client authenticates to a net REPL listener. One of these may open
// any number of connections; begin() is called once for each.
class NetREPLClientAuthenticator {
public:
  virtual ~NetREPLClientAuthenticator() = default;

  // Names the mechanism, and must equal the listener's
  // NetREPLAuthenticator::mechanism(). At most maxNetREPLMechanismNameBytes.
  virtual std::string mechanism() const = 0;

  // The largest server token this client will read.
  virtual size_t maxTokenBytes() const { return defaultMaxNetREPLAuthTokenBytes; }

  virtual std::unique_ptr<NetREPLClientAuthSession> begin() const = 0;
};

// The built-in mechanism: prove knowledge of a secret configured on both
// sides. The secret is sent as is, before the server has proven anything, so
// it belongs on a loopback or otherwise private hop.
class NetREPLSharedSecret : public NetREPLClientAuthenticator {
public:
  explicit NetREPLSharedSecret(const std::string& value) : secret(value) {
    if (this->secret.empty()) {
      throw std::runtime_error("The netREPL shared secret must not be empty");
    }
    if (this->secret.size() > maxNetREPLSharedSecretBytes) {
      throw std::runtime_error("The netREPL shared secret is too large");
    }
  }

  const std::string& value() const { return this->secret; }

  std::string mechanism() const override { return netREPLSharedSecretMechanism(); }

  std::unique_ptr<NetREPLClientAuthSession> begin() const override {
    class Session : public NetREPLClientAuthSession {
    public:
      explicit Session(const std::string& secret) : secret(secret) {}
      std::string next(const std::string&) override { return this->secret; }
    private:
      std::string secret;
    };
    return std::unique_ptr<NetREPLClientAuthSession>(new Session(this->secret));
  }

private:
  std::string secret;
};

// Run the client's half of the handshake on a connected socket, before
// anything else is sent on it. Blocks until the server accepts or refuses.
// Throws if the session is not authenticated; the socket is left open for the
// caller to close. 'peer' only names the server in error messages.
inline void authenticateSession(int s, const NetREPLClientAuthenticator& auth, const std::string& peer = std::string()) {
  const std::string mechanism    = auth.mechanism();
  const bool        sharedSecret = mechanism == netREPLSharedSecretMechanism();

  if (mechanism.empty() || mechanism.size() > maxNetREPLMechanismNameBytes) {
    throw std::runtime_error("A netREPL authentication mechanism name must be between 1 and 64 bytes");
  }

  auto rejected = [&](const std::string& why) {
    return std::runtime_error("netREPL authentication rejected" + (peer.empty() ? std::string() : " by '" + peer + "'") + ": " + why);
  };
  // a server that does not speak this mechanism at all (an unauthenticated
  // listener, or one using the other version word) hangs up without a word
  auto disconnected = [&]() {
    return rejected(
      sharedSecret
        ? "the server refused the shared secret or does not require one"
        : "the connection closed during the '" + mechanism + "' handshake; the server may not use this mechanism, or would not read a token of that size");
  };

  // where the platform has it, keep a write to a server that has hung up from
  // raising SIGPIPE; the failed send is reported as an exception instead
#if defined(MSG_NOSIGNAL)
  const int sendFlags = MSG_NOSIGNAL;
#else
  const int sendFlags = 0;
#endif
#if defined(SO_NOSIGPIPE)
  int noSigPipe = 1;
  setsockopt(s, SOL_SOCKET, SO_NOSIGPIPE, &noSigPipe, sizeof(noSigPipe));
#endif

  auto sendAll = [&](const std::string& b) {
    size_t i = 0;
    while (i < b.size()) {
      ssize_t n = ::send(s, b.data() + i, b.size() - i, sendFlags);
      if (n < 0) {
        if (errno == EINTR) continue;
        return false;
      }
      i += static_cast<size_t>(n);
    }
    return true;
  };
  auto recvAll = [&](void* d, size_t sz) {
    size_t i = 0;
    while (i < sz) {
      ssize_t n = ::recv(s, static_cast<char*>(d) + i, sz - i, 0);
      if (n < 0) {
        if (errno == EINTR) continue;
        return false;
      } else if (n == 0) {
        return false;
      }
      i += static_cast<size_t>(n);
    }
    return true;
  };
  auto put = [](std::string* b, const void* p, size_t n) {
    b->append(static_cast<const char*>(p), n);
  };
  // a size_t length ahead of the bytes, as every other string on this protocol
  auto putSized = [&](std::string* b, const std::string& x) {
    const size_t n = x.size();
    put(b, &n, sizeof(n));
    b->append(x);
  };

  std::unique_ptr<NetREPLClientAuthSession> session = auth.begin();
  if (!session) {
    throw std::runtime_error("The netREPL '" + mechanism + "' authenticator did not begin a session");
  }
  const size_t maxTokenBytes = auth.maxTokenBytes();

  // the version word, the mechanism's name and the first token go out in one
  // send, so a server that refuses the opening and hangs up cannot leave the
  // client part way through writing it
  std::string out;
  const uint32_t version = sharedSecret ? netREPLSharedSecretVersion : netREPLMechanismVersion;
  put(&out, &version, sizeof(version));
  if (!sharedSecret) {
    putSized(&out, mechanism);
  }

  std::string serverToken;
  for (size_t round = 0; round < maxNetREPLAuthRounds; ++round) {
    putSized(&out, session->next(serverToken));

    uint8_t status = netREPLAuthRejected;
    if (!sendAll(out) || !recvAll(&status, sizeof(status))) {
      throw disconnected();
    }
    out.clear();
    serverToken.clear();

    if (status == netREPLAuthContinue || status == netREPLAuthAcceptedWithToken) {
      size_t n = 0;
      if (!recvAll(&n, sizeof(n))) {
        throw disconnected();
      }
      if (n > maxTokenBytes) {
        throw rejected("the server sent a '" + mechanism + "' token larger than this client accepts");
      }
      serverToken.resize(n);
      if (n > 0 && !recvAll(&serverToken[0], n)) {
        throw disconnected();
      }
    }

    switch (status) {
    case netREPLAuthAccepted:
    case netREPLAuthAcceptedWithToken:
      session->accepted(serverToken);
      return;
    case netREPLAuthContinue:
      break;
    case netREPLAuthWrongMechanism:
      throw rejected("the server does not use the '" + mechanism + "' mechanism");
    default:
      throw rejected(
        sharedSecret
          ? "the server refused the shared secret or does not require one"
          : "the server refused the '" + mechanism + "' handshake");
    }
  }
  throw rejected("the '" + mechanism + "' handshake did not finish in " + std::to_string(maxNetREPLAuthRounds) + " rounds");
}

}}

#endif
