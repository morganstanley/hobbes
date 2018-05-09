#include "netserver.H"
#include "network.H"

#include <hobbes/ipc/net.H>

#include <sys/socket.h>
#include <unistd.h> // close

namespace hog {

// TODO allocateServer does not support host, only port
// - that's an issue if the host has multiple net device
DefaultNetServer::DefaultNetServer(const std::string& hostport)
  :_socket(hobbes::allocateServer(hostport)) // throws on error
{}

DefaultNetServer::~DefaultNetServer()
{
  close(_socket);
}

std::unique_ptr<NetConnection> DefaultNetServer::accept() {
  const int client = ::accept(_socket, nullptr, nullptr);
  if (client < 0) {
    return {};
  }

  return createNetConnection(client);
}

} // namespace hog
