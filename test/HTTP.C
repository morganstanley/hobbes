#include "test.H"

#include <hobbes/events/events.H>
#include <hobbes/events/httpd.H>

#include <cstdint>
#include <stdexcept>

#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

using namespace hobbes;

namespace {

void ignoreRequest(const HTTPRequest &, int, void *) {}

class RegisteredHTTPListener {
public:
  explicit RegisteredHTTPListener(int fd) : fd(fd) {}
  ~RegisteredHTTPListener() {
    unregisterEventHandler(fd);
    close(fd);
  }

  int get() const { return fd; }

private:
  int fd;
};

uint32_t boundIPv4Address(int fd) {
  sockaddr_in address = {};
  socklen_t addressSize = sizeof(address);
  if (getsockname(fd, reinterpret_cast<sockaddr *>(&address), &addressSize) !=
      0) {
    throw std::runtime_error("Unable to inspect HTTP listening address");
  }
  if (address.sin_family != AF_INET) {
    throw std::runtime_error("HTTP listener did not use IPv4");
  }
  return ntohl(address.sin_addr.s_addr);
}

} // namespace

TEST(HTTP, bindsToLoopbackByDefault) {
  RegisteredHTTPListener defaultListener(installHTTPD(0, &ignoreRequest));
  EXPECT_EQ(boundIPv4Address(defaultListener.get()),
            static_cast<uint32_t>(INADDR_LOOPBACK));

  RegisteredHTTPListener explicitListener(
      installHTTPD("0.0.0.0", 0, &ignoreRequest));
  EXPECT_EQ(boundIPv4Address(explicitListener.get()),
            static_cast<uint32_t>(INADDR_ANY));
}
