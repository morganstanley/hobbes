#ifndef HOG_NETWORK_H_INCLUDED
#define HOG_NETWORK_H_INCLUDED

#include "netconnection.H"
#include "netserver.H"

#include <functional>
#include <memory>

namespace hog {

// Transport factories. hog reaches the network only through these, so a consumer
// can install its own NetServer/NetConnection implementations -- e.g. to add
// authentication and/or encryption (Kerberos/GSSAPI, TLS, ...) -- without hog
// itself depending on any such library. See doc/en/hog-custom-transport.md.
//
// Install factories BEFORE starting the receive server or any senders. Installation
// is not synchronized against concurrent create* calls, so do it during startup.
// Passing an empty std::function restores the built-in plaintext transport.
using NetServerFactory     = std::function<std::unique_ptr<NetServer>(const std::string& hostport)>;
using NetConnectionFactory = std::function<std::unique_ptr<NetConnection>(const std::string& hostport)>;

void setNetServerFactory(NetServerFactory);
void setNetConnectionFactory(NetConnectionFactory);

// Receive side: the listener/acceptor (batchrecv). Overridable via setNetServerFactory.
std::unique_ptr<NetServer> createNetServer(const std::string& hostport);

// Send side: an outbound connection (batchsend). Overridable via setNetConnectionFactory.
std::unique_ptr<NetConnection> createNetConnection(const std::string& hostport);

// Wrap an already-accepted socket fd. Used by DefaultNetServer::accept(); a custom
// NetServer performs its own accept/handshake and need not call this.
std::unique_ptr<NetConnection> createNetConnection(int fd);

} // namespace hog

#endif // HOG_NETWORK_H_INCLUDED
