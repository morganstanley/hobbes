/*****************************************
 * an optional quick-and-dirty web server that works through events
 ****************************************/

#ifndef HOBBES_EVENTS_HTTPD_HPP_INCLUDED
#define HOBBES_EVENTS_HTTPD_HPP_INCLUDED

#include <string>
#include <vector>
#include <map>

namespace hobbes {

struct HTTPRequest {
  using Headers = std::map<std::string, std::string>;
  using Data = std::vector<char>;

  std::string method;
  std::string document;
  Headers     headers;
  Data        data;
};

// HTTP field names are case-insensitive. Compare a request header value with
// an expected secret without returning early on the first differing byte.
// The running time is bounded by the server-owned expected value rather than
// by a matching prefix supplied by the client.
bool httpHeaderMatches(const HTTPRequest&, const std::string& name,
                       const std::string& expectedValue);

using HTTPRequestHandler = void (*)(const HTTPRequest &, int, void *);

// The port-only overload listens on IPv4 loopback. Use the host overload to
// deliberately select another interface. The server provides no
// authentication or TLS; put non-loopback listeners behind an authenticated,
// encrypted fronting proxy.
int installHTTPD(int port, HTTPRequestHandler, void* ud = nullptr);
int installHTTPD(const std::string& host, int port, HTTPRequestHandler,
                 void* ud = nullptr);

}

#endif

