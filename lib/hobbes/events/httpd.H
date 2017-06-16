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
  typedef std::map<std::string, std::string> Headers;
  typedef std::vector<char> Data;

  std::string method;
  std::string document;
  Headers     headers;
  Data        data;
};

typedef void (*HTTPRequestHandler)(const HTTPRequest&, int fd, void* ud);

int installHTTPD(int port, HTTPRequestHandler, void* ud = 0);

}

#endif

