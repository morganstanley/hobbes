#include "network.H"

#include <utility>

namespace hog {

namespace {

std::unique_ptr<NetServer> defaultNetServer(const std::string& hostport) {
  return std::unique_ptr<NetServer>(new DefaultNetServer(hostport));
}

std::unique_ptr<NetConnection> defaultNetConnection(const std::string& hostport) {
  return std::unique_ptr<NetConnection>(new DefaultNetConnection(hostport));
}

NetServerFactory& serverFactory() {
  static NetServerFactory f = defaultNetServer;
  return f;
}

NetConnectionFactory& connectionFactory() {
  static NetConnectionFactory f = defaultNetConnection;
  return f;
}

} // namespace

void setNetServerFactory(NetServerFactory f) {
  serverFactory() = f ? std::move(f) : NetServerFactory(defaultNetServer);
}

void setNetConnectionFactory(NetConnectionFactory f) {
  connectionFactory() = f ? std::move(f) : NetConnectionFactory(defaultNetConnection);
}

std::unique_ptr<NetServer> createNetServer(const std::string& hostport)
{
  return serverFactory()(hostport);
}

std::unique_ptr<NetConnection> createNetConnection(const std::string& hostport)
{
  return connectionFactory()(hostport);
}

std::unique_ptr<NetConnection> createNetConnection(int fd)
{
  return std::unique_ptr<NetConnection>(new DefaultNetConnection(fd));
}

} // namespace hog
