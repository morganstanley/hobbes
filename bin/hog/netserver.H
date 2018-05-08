#ifndef HOG_NETSERVER_H_INCLUDED
#define HOG_NETSERVER_H_INCLUDED

#include "netconnection.H"

#include <memory>

namespace hog {

class NetServer {
public:
  virtual ~NetServer() = default;

  virtual std::unique_ptr<NetConnection> accept() = 0;
};

class DefaultNetServer : public NetServer {
public:
  explicit DefaultNetServer(const std::string& hostport);
  ~DefaultNetServer();

  std::unique_ptr<NetConnection> accept() override;

private:
  int _socket;
};

} // namespace hog

#endif // HOG_NETSERVER_H_INCLUDED
