#include "network.H"

namespace hog {

std::unique_ptr<NetServer> createNetServer(const std::string& hostport)
{
  return std::unique_ptr<NetServer>(new DefaultNetServer(hostport));
}

std::unique_ptr<NetConnection> createNetConnection(const std::string& hostport)
{
  return std::unique_ptr<NetConnection>(new DefaultNetConnection(hostport));
}

std::unique_ptr<NetConnection> createNetConnection(int fd)
{
  return std::unique_ptr<NetConnection>(new DefaultNetConnection(fd));
}

} // namespace hog
