#include "test.H"

#include <hobbes/events/events.H>
#include <hobbes/events/httpd.H>
#include "../bin/hi/www.H"

#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>

#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

using namespace hobbes;

namespace {

void ignoreRequest(const HTTPRequest &, int, void *) {}
void countRequest(const HTTPRequest &, int, void *ud) {
  ++*static_cast<int*>(ud);
}
void recordRequestBody(const HTTPRequest &req, int, void *ud) {
  static_cast<std::vector<std::string>*>(ud)->emplace_back(req.data.begin(), req.data.end());
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

int connectHTTPClient(int listener) {
  int client = socket(AF_INET, SOCK_STREAM, 0);
  if (client < 0) {
    throw std::runtime_error("Unable to allocate HTTP client socket");
  }

  sockaddr_in address = {};
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  address.sin_port = htons(boundIPv4Port(listener));
  if (connect(client, reinterpret_cast<sockaddr*>(&address), sizeof(address)) != 0) {
    close(client);
    throw std::runtime_error("Unable to connect HTTP client socket");
  }
  return client;
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

TEST(HTTP, RejectsOversizedRequestLineWithoutDispatch) {
  int requests = 0;
  RegisteredHTTPListener listener(installHTTPD(0, &countRequest, &requests));

  int client = connectHTTPClient(listener.get());

  std::string request(8193, 'G');
  EXPECT_EQ(send(client, request.data(), request.size(), 0), static_cast<ssize_t>(request.size()));
  EXPECT_TRUE(stepEventLoop(1000)); // accept
  EXPECT_TRUE(stepEventLoop(1000)); // reject request data
  EXPECT_EQ(requests, 0);
  close(client);
}

TEST(HTTP, RejectsInvalidOrOversizedContentLengthWithoutDispatch) {
  int requests = 0;
  RegisteredHTTPListener listener(installHTTPD(0, &countRequest, &requests));

  for (const std::string& length : {
           std::string("-1"),
           std::string("67108865"),
           std::string("99999999999999999999999999")}) {
    int client = connectHTTPClient(listener.get());
    const std::string request =
        "POST / HTTP/1.1\r\nContent-Length: " + length + "\r\n\r\n";
    EXPECT_EQ(send(client, request.data(), request.size(), 0),
              static_cast<ssize_t>(request.size()));
    EXPECT_TRUE(stepEventLoop(1000)); // accept
    EXPECT_TRUE(stepEventLoop(1000)); // reject the completed headers
    EXPECT_EQ(requests, 0);
    close(client);
  }
}

TEST(HTTP, DispatchesAZeroLengthRequestBody) {
  int requests = 0;
  RegisteredHTTPListener listener(installHTTPD(0, &countRequest, &requests));
  int client = connectHTTPClient(listener.get());
  const std::string request =
      "POST / HTTP/1.1\r\nContent-Length: 0\r\n\r\n";

  EXPECT_EQ(send(client, request.data(), request.size(), 0),
            static_cast<ssize_t>(request.size()));
  EXPECT_TRUE(stepEventLoop(1000)); // accept
  EXPECT_TRUE(stepEventLoop(1000)); // dispatch the completed request
  EXPECT_EQ(requests, 1);
  close(client);
}

// The blank line that ends the headers is "\r\n"; the body begins after its
// '\n' (which was once read as the body's first byte). Header names match in
// any case, as proxies bridging from HTTP/2 send them in lower case.
TEST(HTTP, ReadsTheBodyNamedByAnyCaseOfContentLength) {
  for (const std::string& name : {std::string("Content-Length"), std::string("content-length")}) {
    std::vector<std::string> bodies;
    RegisteredHTTPListener listener(installHTTPD(0, &recordRequestBody, &bodies));
    int client = connectHTTPClient(listener.get());
    const std::string request =
        "POST / HTTP/1.1\r\n" + name + ": 5\r\n\r\nhello";

    EXPECT_EQ(send(client, request.data(), request.size(), 0),
              static_cast<ssize_t>(request.size()));
    for (int i = 0; i < 4 && bodies.empty(); ++i) {
      EXPECT_TRUE(stepEventLoop(1000));
    }
    EXPECT_EQ(bodies.size(), static_cast<size_t>(1));
    if (!bodies.empty()) {
      EXPECT_EQ(bodies[0], std::string("hello"));
    }
    close(client);
  }
}
