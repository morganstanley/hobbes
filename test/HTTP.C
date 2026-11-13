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
void countRequest(const HTTPRequest &, int, void *ud) {
  ++*static_cast<int*>(ud);
}

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

uint16_t boundIPv4Port(int fd) {
  sockaddr_in address = {};
  socklen_t addressSize = sizeof(address);
  if (getsockname(fd, reinterpret_cast<sockaddr *>(&address), &addressSize) != 0) {
    throw std::runtime_error("Unable to inspect HTTP listening port");
  }
  return ntohs(address.sin_port);
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
  request.document = "/report.hxp/";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/reports//./daily.hxp//?desk=FX";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/reports/archive/../daily.hxp/.";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/report.hxp.txt";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
  request.document = "/report.hxp/assets";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
  request.document = "/../report.hxp";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
}

TEST(HTTP, RejectsOversizedRequestLineWithoutDispatch) {
  int requests = 0;
  RegisteredHTTPListener listener(installHTTPD(0, &countRequest, &requests));

  int client = socket(AF_INET, SOCK_STREAM, 0);
  EXPECT_TRUE(client >= 0);
  sockaddr_in address = {};
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  address.sin_port = htons(boundIPv4Port(listener.get()));
  EXPECT_EQ(connect(client, reinterpret_cast<sockaddr*>(&address), sizeof(address)), 0);

  std::string request(8193, 'G');
  EXPECT_EQ(send(client, request.data(), request.size(), 0), static_cast<ssize_t>(request.size()));
  EXPECT_TRUE(stepEventLoop(1000)); // accept
  EXPECT_TRUE(stepEventLoop(1000)); // reject request data
  EXPECT_EQ(requests, 0);
  close(client);
}
