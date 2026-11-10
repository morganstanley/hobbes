#include "test.H"

#include <hobbes/events/events.H>
#include <hobbes/events/httpd.H>
#include "../bin/hi/www.H"

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

TEST(HTTP, matchesSecretHeadersCaseInsensitively) {
  HTTPRequest request;
  request.headers["x-hObBeS-pRoXy-SeCrEt"] = "correct horse battery staple";

  EXPECT_TRUE(httpHeaderMatches(request, "X-Hobbes-Proxy-Secret",
                                "correct horse battery staple"));
  EXPECT_TRUE(!httpHeaderMatches(request, "X-Hobbes-Proxy-Secret",
                                 "correct horse battery staplf"));
  EXPECT_TRUE(!httpHeaderMatches(request, "X-Hobbes-Proxy-Secret",
                                 "correct horse battery"));
  EXPECT_TRUE(!httpHeaderMatches(request, "X-Hobbes-Proxy-Secret",
                                 "correct horse battery staple!"));
  EXPECT_TRUE(!httpHeaderMatches(request, "Authorization",
                                 "correct horse battery staple"));
}

TEST(HTTP, identifiesEveryExecutableWebRoute) {
  HTTPRequest request;

  request.document = "/";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
  request.document = "/index.html?theme=light";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
  request.document = "/?1%2B1";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/report.hxp";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/report.hxp?desk=FX";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/report.hxp.txt";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
}
