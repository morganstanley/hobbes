
#include <hobbes/events/events.H>
#include <hobbes/events/httpd.H>
#include <hobbes/ipc/net.H>
#include <hobbes/util/str.H>

#include <sstream>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

namespace hobbes {

// evaluate HTTP request data until completed
class PartialHTTPRequestState {
public:
  PartialHTTPRequestState(int s, HTTPRequestHandler f, void* ud) : s(s), f(f), ud(ud), state(Method) {
  }

  bool transition(const char* b, const char* e) {
    while (b != e) {
      if (!transition(*b)) {
        this->f(this->req, this->s, this->ud);
        return false;
      }
      ++b;
    }
    return true;
  }
private:
  int                s;
  HTTPRequestHandler f;
  void*              ud;

  enum State {
    Method = 0,
    Document,
    FindHeaderBegin,
    HeaderNameOrEnd,
    HeaderName,
    HeaderValue,
    AccData
  };
  State state;

  std::ostringstream r0;
  std::ostringstream r1;
  size_t             len;

  HTTPRequest req;

  bool transition(char c) {
    switch (this->state) {
    case Method:
      if (c != ' ') {
        this->r0.put(c);
      } else {
        this->req.method = this->r0.str();
        this->r0.str("");

        this->state = Document;
        break;
      }
      break;
    case Document:
      if (c != ' ') {
        this->r0.put(c);
      } else {
        this->req.document = this->r0.str();
        this->r0.str("");

        this->state = FindHeaderBegin;
      }
      break;
    case FindHeaderBegin:
      if (c == '\n') {
        this->state = HeaderNameOrEnd;
      }
      break;
    case HeaderNameOrEnd:
      if (c == '\r' || c == '\n') {
        auto cl = this->req.headers.find("Content-Length");
        if (cl == this->req.headers.end()) {
          return false;
        } else {
          std::istringstream slen(cl->second);
          slen >> this->len;
          if (!slen || this->len == 0) { return false; }

          this->state = AccData;
        }
      } else {
        this->r0.put(c);
        this->state = HeaderName;
      }
      break;
    case HeaderName:
      if (c == ':') {
        this->state = HeaderValue;
      } else {
        this->r0.put(c);
      }
      break;
    case HeaderValue:
      if (c == '\n') {
        this->req.headers[this->r0.str()] = str::trim(this->r1.str());

        this->r0.str("");
        this->r1.str("");

        this->state = HeaderNameOrEnd;
      } else {
        this->r1.put(c);
      }
      break;
    case AccData:
      this->req.data.push_back(c);
      if (this->req.data.size() == this->len) {
        return false;
      }
      break;
    }
    return true;
  }
};

void evaluatePartialHTTPRequest(int c, void* ud) {
  PartialHTTPRequestState* s = reinterpret_cast<PartialHTTPRequestState*>(ud);

  char buf[1024];
  ssize_t m = 0;
  bool terminal = false;
  do {
    m = read(c, buf, sizeof(buf));
    terminal = (m == 0 || (m == -1 && errno != EAGAIN) || (m > 0 && !s->transition(buf, buf + m)));
  } while (m > 0 && !terminal);

  if (terminal) {
    unregisterEventHandler(c);
    close(c);
    delete s;
  }
}

int installHTTPD(int port, HTTPRequestHandler f, void* ud) {
  int s = allocateServer(port);

  typedef std::pair<HTTPRequestHandler, void*> ReqCB;
  ReqCB* rcb = new ReqCB(f, ud);

  registerEventHandler(
    s,
    [](int s, void* d) {
      int c = accept(s, 0, 0);
      if (c != -1) {
        ReqCB* rcb = reinterpret_cast<ReqCB*>(d);
        fcntl(c, F_SETFL, fcntl(c, F_GETFL) | O_NONBLOCK);
        registerEventHandler(c, &evaluatePartialHTTPRequest, reinterpret_cast<void*>(new PartialHTTPRequestState(c, rcb->first, rcb->second)));
      }
    },
    rcb
  );

  return s;
}

}

