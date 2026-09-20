#ifndef HOG_NETCONNECTION_H_INCLUDED
#define HOG_NETCONNECTION_H_INCLUDED

#include <string>
#include <vector>
#include <cstdint>

namespace hog {

class NetConnection {
public:
  NetConnection() = default;
  virtual ~NetConnection() = default;

  NetConnection(const NetConnection&) = delete;
  void operator=(const NetConnection&) = delete;

  virtual bool send(const void* buf, size_t size) = 0;
  virtual bool sendFile(int fd) = 0;

  virtual bool receive(void* buf, size_t size) = 0;

  virtual std::string remoteHost() = 0;
  virtual int remotePort() = 0;
};

class DefaultNetConnection : public NetConnection {
public:
  explicit DefaultNetConnection(const std::string& hostport);
  explicit DefaultNetConnection(int fd);
  ~DefaultNetConnection();

  bool send(const void* buf, size_t size) override;
  bool sendFile(int fd) override;

  bool receive(void* buf, size_t size) override;

  std::string remoteHost() override;
  int remotePort() override;
private:
  int fd;
};

// the largest length-prefixed frame this receiver will accept from a sender
// (see receiveIntoBuffer): a sender cuts a segment at ~10MB of log data and
// sends it compressed, so this is generous for any batch hog produces, while
// keeping what an unauthenticated sender can make a receiver allocate to a
// fixed bound
constexpr uint64_t maxFrameBytes = 64ull * 1024 * 1024;

void sendString(NetConnection& c, const std::string& str);

void receiveIntoBuffer(NetConnection& c, std::vector<uint8_t>* dst);
std::vector<uint8_t> receiveBuffer(NetConnection& c);
std::string receiveString(NetConnection& c);

}

#endif
