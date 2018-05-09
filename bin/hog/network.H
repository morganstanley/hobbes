#ifndef HOG_NETWORK_H_INCLUDED
#define HOG_NETWORK_H_INCLUDED

#include "netconnection.H"
#include "netserver.H"

#include <memory>

namespace hog {

std::unique_ptr<NetServer> createNetServer(const std::string& hostport);

std::unique_ptr<NetConnection> createNetConnection(const std::string& hostport);
std::unique_ptr<NetConnection> createNetConnection(int fd);

} // namespace hog

#endif // HOG_NETWORK_H_INCLUDED
