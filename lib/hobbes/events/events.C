
#include <hobbes/hobbes.H>
#include <hobbes/events/events.H>
#include <hobbes/util/perf.H>
#include <hobbes/util/os.H>

#include <chrono>
#include <queue>
#include <utility>

#ifdef BUILD_LINUX
#include <sys/epoll.h>
#elif defined(BUILD_OSX)
#include <sys/event.h>
#endif

namespace hobbes {

struct eventcbclosure {
  eventcbclosure(int fd, const std::function<void(int)>& fn) : fd(fd), fn(fn) { }

  int                      fd;
  std::function<void(int)> fn;
};
typedef std::map<int, eventcbclosure*> EventClosures;

void registerEventHandler(int fd, eventhandler fn, void* ud, bool f) {
  registerEventHandler(fd, [fn,ud](int c){fn(c,ud);}, f);
}

#ifdef BUILD_LINUX
__thread bool           epInitialized = false;
__thread int            epFD          = 0;
__thread EventClosures* epClosures    = 0;

struct timer {
  timerfunc func;
  std::chrono::high_resolution_clock::time_point callTime;
  std::chrono::milliseconds interval;
};

bool operator>(const timer& a, const timer& b) {
  return a.callTime > b.callTime;
}

thread_local std::priority_queue<timer, std::vector<timer>, std::greater<timer>> timers;

int threadEPollFD() {
  if (!epInitialized) {
    epFD       = epoll_create(1);
    epClosures = new EventClosures();

    if (epFD < 0) {
      throw std::runtime_error("Failed to allocate epoll FD: " + std::string(strerror(errno)));
    }
    epInitialized = true;
  }
  return epFD;
}

void unregisterEventHandler(int fd) {
  auto ec = epClosures->find(fd);
  if (ec != epClosures->end()) {
    struct epoll_event evt;
    epoll_ctl(threadEPollFD(), EPOLL_CTL_DEL, fd, &evt);
    delete ec->second;
  }
}

void registerEventHandler(int fd, const std::function<void(int)>& fn, bool) {
  int epfd = threadEPollFD();

  eventcbclosure* c = new eventcbclosure(fd, fn);
  (*epClosures)[fd] = c;

  struct epoll_event evt;
  memset(&evt, 0, sizeof(evt));
  evt.events   = EPOLLIN | EPOLLPRI | EPOLLERR;
  evt.data.fd  = fd;
  evt.data.ptr = reinterpret_cast<void*>(c);

  if (epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &evt) != 0) {
    delete c;
    throw std::runtime_error("Failed to add FD to epoll set: " + std::string(strerror(errno)));
  }
}

bool stepEventLoop() {
  while (true) {
    int timeout = -1;
    if (!timers.empty()) {
      auto next = timers.top().callTime;
      auto timeUntilNext = next - std::chrono::high_resolution_clock::now();
      int millis = std::chrono::duration_cast<std::chrono::milliseconds>(timeUntilNext).count();
      timeout = std::max(1, millis);
    }

    struct epoll_event evts[64];
    int fds = epoll_wait(threadEPollFD(), evts, sizeof(evts)/sizeof(evts[0]), timeout);
    bool status = true;
    if (fds > 0) {
      for (int fd = 0; fd < fds; ++fd) {
        eventcbclosure* c = reinterpret_cast<eventcbclosure*>(evts[fd].data.ptr);
        (c->fn)(c->fd);
        resetMemoryPool();
      }
    } else if (fds < 0 && errno != EINTR) {
      status = false;
    } else if (!timers.empty()) {
      std::vector<timer> newTimers;

      auto now = std::chrono::high_resolution_clock::now();
      while(!timers.empty() && timers.top().callTime <= now) {
        auto t = timers.top();
        timers.pop();
        
        bool repeat = t.func();
        resetMemoryPool();
        
        if(repeat) {
          timer newT;
          newT.callTime = std::chrono::high_resolution_clock::now() + t.interval,
          newT.func = t.func,
          newT.interval = t.interval,
          
          newTimers.push_back(newT);
        }
      }
      
      for (auto& timer : newTimers) {
        timers.push(timer);
      }
    }
    return status;
  }
}

void runEventLoop() {
  while (stepEventLoop());
}

void addTimer(timerfunc f, int millisecInterval) {
  timer t;
  t.func = f;
  t.interval = std::chrono::milliseconds(millisecInterval);
  t.callTime = std::chrono::high_resolution_clock::now() + t.interval;

  timers.push(t);
}

void runEventLoop(int microsecondDuration) {
  long t  = hobbes::time();
  long dt = static_cast<long>(microsecondDuration) * 1000L;
  long tf = t + dt;

  do {
    double nsleft = tf-t;
    int timeout = (ceil(nsleft / 1000000.0));
    if (timeout < 0) timeout = 0;

    struct epoll_event evts[64];
    int fds = epoll_wait(threadEPollFD(), evts, sizeof(evts)/sizeof(evts[0]), timeout);
    if (fds > 0) {
      for (int fd = 0; fd < fds; ++fd) {
        eventcbclosure* c = reinterpret_cast<eventcbclosure*>(evts[fd].data.ptr);
        (c->fn)(c->fd);
        resetMemoryPool();
      }
    }
    t = hobbes::time();
  } while (t < tf);
}

#elif defined(BUILD_OSX)

__thread bool           kqInitialized = false;
__thread int            kqFD          = 0;
__thread EventClosures* kqClosures    = 0;

int threadKQFD() {
  if (!kqInitialized) {
    kqFD       = kqueue();
    kqClosures = new EventClosures();

    if (kqFD < 0) {
      throw std::runtime_error("Failed to allocate kqueue: " + std::string(strerror(errno)));
    }
    kqInitialized = true;
  }
  return kqFD;
}

void unregisterEventHandler(int fd) {
  auto ec = kqClosures->find(fd);
  if (ec != kqClosures->end()) {
    struct kevent ke;
    EV_SET(&ke, fd, EVFILT_READ, EV_DELETE, 0, 0, 0);
    kevent(threadKQFD(), &ke, 1, 0, 0, 0);
    delete ec->second;
  }
}

void registerEventHandler(int fd, const std::function<void(int)>& fn, bool vn) {
  int kqfd = threadKQFD();

  eventcbclosure* c = new eventcbclosure(fd, fn);
  (*kqClosures)[fd] = c;

  struct kevent ke;
  if (vn) {
    EV_SET(&ke, fd, EVFILT_VNODE, EV_ADD, NOTE_DELETE | NOTE_WRITE, 0, (void*)c);
  } else {
    EV_SET(&ke, fd, EVFILT_READ, EV_ADD, 0, 0, (void*)c);
  }
  if (kevent(kqfd, &ke, 1, 0, 0, 0) == -1) {
    delete c;
    throw std::runtime_error("Failed to add FD to kqueue: " + std::string(strerror(errno)));
  }
}

bool stepEventLoop() {
  while (true) {
    struct kevent evts[64];
    int fds = kevent(threadKQFD(), 0, 0, evts, sizeof(evts)/sizeof(evts[0]), 0);
    if (fds > 0) {
      for (size_t fd = 0; fd < fds; ++fd) {
        eventcbclosure* c = (eventcbclosure*)evts[fd].udata;
        (c->fn)(c->fd);
        resetMemoryPool();
      }
      return true;
    } else if (errno != EINTR) {
      return false;
    }
  }
}

void runEventLoop() {
  while (stepEventLoop());
}

void addTimer(timerfunc f, int millisecInterval) {
  throw std::runtime_error("addTimer nyi for OSX");
}

void runEventLoop(int microsecondDuration) {
  long t  = hobbes::time();
  long dt = ((long)microsecondDuration) * 1000L;
  long tf = t + dt;

  do {
    double nsleft = tf-t;
    int nstimeout = (ceil(nsleft / 1000000.0));
    if (nstimeout < 0) nstimeout = 0;

    struct timespec timeout;
    timeout.tv_sec = 0;
    timeout.tv_nsec = nstimeout;

    struct kevent evts[64];
    int fds = kevent(threadKQFD(), 0, 0, evts, sizeof(evts)/sizeof(evts[0]), &timeout);
    if (fds > 0) {
      for (size_t fd = 0; fd < fds; ++fd) {
        eventcbclosure* c = (eventcbclosure*)evts[fd].udata;
        (c->fn)(c->fd);
        resetMemoryPool();
      }
    }
    t = hobbes::time();
  } while (t < tf);
}

#endif

}
