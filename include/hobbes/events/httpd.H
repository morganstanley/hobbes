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

using HTTPRequestHandler = void (*)(const HTTPRequest &, int, void *);

int installHTTPD(int port, HTTPRequestHandler, void* ud = nullptr);

}

#endif

