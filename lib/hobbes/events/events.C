
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
  bool                     vnode   = false; // registered for file changes rather than readability
  bool                     retired = false; // unregistered or replaced while a batch was being dispatched
};
using EventClosures = std::map<int, eventcbclosure *>;

void registerEventHandler(int fd, eventhandler fn, void* ud, bool f) {
  registerEventHandler(fd, [fn,ud](int c){fn(c,ud);}, f);
}

// a closure is not deleted while a batch of events is being dispatched: the
// batch holds raw closure pointers, and a callback in it can close another
// descriptor of the same batch (or unregister it) and register a replacement
// under the same number, which would free a closure the loop is about to
// call. Closures retired during a batch are kept until it has been dispatched,
// and the batch skips them: their descriptor is gone, or is someone else's now.
thread_local std::vector<eventcbclosure*> retiredClosures;
thread_local size_t                       dispatchDepth = 0;

void retireClosure(eventcbclosure* c) {
  if (c == nullptr) {
    return;
  } else if (dispatchDepth == 0) {
    delete c;
  } else {
    c->retired = true;
    retiredClosures.push_back(c);
  }
}

struct DispatchingBatch {
  DispatchingBatch() { ++dispatchDepth; }
  ~DispatchingBatch() {
    if (--dispatchDepth == 0) {
      for (auto* c : retiredClosures) {
        delete c;
      }
      retiredClosures.clear();
    }
  }
  DispatchingBatch(const DispatchingBatch&) = delete;
  DispatchingBatch& operator=(const DispatchingBatch&) = delete;
};

#ifdef BUILD_LINUX
thread_local bool           epInitialized = false;
thread_local int            epFD          = 0;
thread_local EventClosures* epClosures    = nullptr;

struct timer {
  timerfunc func;
  std::chrono::high_resolution_clock::time_point callTime;
  std::chrono::milliseconds interval;
};

bool operator>(const timer& a, const timer& b) {
  return a.callTime > b.callTime;
}

thread_local std::priority_queue<timer, std::vector<timer>, std::greater<>> timers;

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

// the closure map owns exactly the closures the kernel can still deliver: an
// entry is made only once the descriptor is in the epoll set, and is erased
// when its closure is deleted, so a descriptor number that comes back into
// use later never finds a stale pointer under its key
void unregisterEventHandler(int fd) {
  auto ec = epClosures->find(fd);
  if (ec != epClosures->end()) {
    struct epoll_event evt;
    epoll_ctl(threadEPollFD(), EPOLL_CTL_DEL, fd, &evt);
    retireClosure(ec->second);
    epClosures->erase(ec);
  }
}

void registerEventHandler(int fd, const std::function<void(int)>& fn, bool) {
  int epfd = threadEPollFD();

  auto* c = new eventcbclosure(fd, fn);

  struct epoll_event evt;
  memset(&evt, 0, sizeof(evt));
  evt.events   = EPOLLIN | EPOLLPRI | EPOLLERR;
  evt.data.fd  = fd;
  evt.data.ptr = reinterpret_cast<void*>(c);

  if (epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &evt) != 0) {
    delete c;
    throw std::runtime_error("Failed to add FD to epoll set: " + std::string(strerror(errno)));
  }

  auto& slot = (*epClosures)[fd];
  retireClosure(slot); // a previous closure left under this key after its fd was closed unregistered
  slot = c;
}

void registerInterruptHandler(const std::function<void()>& fn) {
  threadEPollFD();
  auto* c = new eventcbclosure(-1, [fn](int){fn();});
  (*epClosures)[-1] = c;
}

bool stepEventLoop(int timeoutMS, const std::function<bool()>& stopFn) {
  while (!stopFn()) {
    if (!timers.empty()) {
      auto next = timers.top().callTime;
      auto timeUntilNext = next - std::chrono::high_resolution_clock::now();
      int millis = std::chrono::duration_cast<std::chrono::milliseconds>(timeUntilNext).count();
      timeoutMS = std::max(1, millis);
    }

    // When stopFn is provided with no explicit timeout, poll every 500ms
    // so the stop condition gets checked instead of blocking indefinitely.
    int effectiveTimeoutMS = timeoutMS < 0 ? 500 : timeoutMS;

    struct epoll_event evts[64];
    int fds = epoll_wait(threadEPollFD(), evts, sizeof(evts)/sizeof(evts[0]), effectiveTimeoutMS);
    bool status = true;
    if (fds > 0) {
      DispatchingBatch batch;
      for (int fd = 0; fd < fds; ++fd) {
        auto* c = reinterpret_cast<eventcbclosure*>(evts[fd].data.ptr);
        if (!c->retired) {
          (c->fn)(c->fd);
          resetMemoryPool();
        }
      }
    } else if (fds < 0) {
      if (errno != EINTR) {
        status = false;
      } else if (epClosures != nullptr) {
        auto f = epClosures->find(-1);
        if (f != epClosures->end()) {
          f->second->fn(-1);
        }
      }
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
  return false;
}

void runEventLoop(const std::function<bool()>& stopFn) {
  while (stepEventLoop(-1, stopFn));
}

void addTimer(timerfunc f, int millisecInterval) {
  timer t;
  t.func = f;
  t.interval = std::chrono::milliseconds(millisecInterval);
  t.callTime = std::chrono::high_resolution_clock::now() + t.interval;

  timers.push(t);
}

void runEventLoop(int microsecondDuration, const std::function<bool()>& stopFn) {
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
      DispatchingBatch batch;
      for (int fd = 0; fd < fds; ++fd) {
        auto* c = reinterpret_cast<eventcbclosure*>(evts[fd].data.ptr);
        if (!c->retired) {
          (c->fn)(c->fd);
          resetMemoryPool();
        }
      }
    }
    t = hobbes::time();
  } while (t < tf && !stopFn());
}

#elif defined(BUILD_OSX)

thread_local bool           kqInitialized = false;
thread_local int            kqFD          = 0;
thread_local EventClosures* kqClosures    = 0;

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

// as on Linux: the closure map holds only closures the kernel can still
// deliver, entered once the descriptor is in the kqueue and erased with the
// closure, so a reused descriptor number never finds a stale pointer
void unregisterEventHandler(int fd) {
  auto ec = kqClosures->find(fd);
  if (ec != kqClosures->end()) {
    // a kqueue registration is keyed by (fd, filter), so the delete must name
    // the filter the handler was registered with: deleting EVFILT_READ for a
    // file-change handler is ENOENT, and its EVFILT_VNODE registration stays
    // live in the kqueue with udata pointing at the closure freed below
    struct kevent ke;
    EV_SET(&ke, fd, ec->second->vnode ? EVFILT_VNODE : EVFILT_READ, EV_DELETE, 0, 0, 0);
    kevent(threadKQFD(), &ke, 1, 0, 0, 0);
    retireClosure(ec->second);
    kqClosures->erase(ec);
  }
}

void registerEventHandler(int fd, const std::function<void(int)>& fn, bool vn) {
  int kqfd = threadKQFD();

  eventcbclosure* c = new eventcbclosure(fd, fn);
  c->vnode = vn;

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

  auto& slot = (*kqClosures)[fd];
  if (slot != nullptr && slot->vnode != vn) {
    // the previous closure under this key was registered with the other
    // filter; the EV_ADD above replaced nothing, so if its descriptor is still
    // open that registration is still live and would deliver to the closure
    // retired below (a closed descriptor has already left the kqueue, and the
    // delete is then a harmless ENOENT)
    EV_SET(&ke, fd, slot->vnode ? EVFILT_VNODE : EVFILT_READ, EV_DELETE, 0, 0, 0);
    kevent(kqfd, &ke, 1, 0, 0, 0);
  }
  retireClosure(slot); // a previous closure left under this key after its fd was closed unregistered
  slot = c;
}

void registerInterruptHandler(const std::function<void()>& fn) {
  threadKQFD();
  auto* c = new eventcbclosure(-1, [fn](int){fn();});
  (*kqClosures)[-1] = c;
}

bool stepEventLoop(int timeoutMS, const std::function<bool()>& stopFn) {
  while (!stopFn()) {
    // When a stop function is provided and no explicit timeout is set,
    // use a 500ms poll interval so the stop condition gets checked
    // periodically instead of blocking indefinitely in kevent().
    int effectiveTimeoutMS = timeoutMS;
    if (timeoutMS < 0) {
      effectiveTimeoutMS = 500;
    }

    struct timespec timeout;
    timeout.tv_sec  = effectiveTimeoutMS / 1000;
    timeout.tv_nsec = (effectiveTimeoutMS % 1000) * 1000000UL;

    struct kevent evts[64];
    int fds = kevent(threadKQFD(), 0, 0, evts, sizeof(evts)/sizeof(evts[0]), &timeout);
    if (fds > 0) {
      DispatchingBatch batch;
      for (size_t fd = 0; fd < fds; ++fd) {
        eventcbclosure* c = (eventcbclosure*)evts[fd].udata;
        if (!c->retired) {
          (c->fn)(c->fd);
          resetMemoryPool();
        }
      }
      return true;
    } else if (fds == 0) {
      // Timeout - loop back to check stopFn
      continue;
    } else if (errno != EINTR) {
      return false;
    } else if (kqClosures) {
      auto f = kqClosures->find(-1);
      if (f != kqClosures->end()) {
        f->second->fn(-1);
      }
    }
  }
  return false;
}

void runEventLoop(const std::function<bool()>& stopFn) {
  while (stepEventLoop(-1, stopFn));
}

thread_local int nextTimerIdent = 1;

void addTimer(timerfunc f, int millisecInterval) {
  int kqfd = threadKQFD();

  auto* c = new eventcbclosure(-1, [f](int) {
    f();
  });

  int ident = nextTimerIdent++;
  struct kevent ke;
  EV_SET(&ke, ident, EVFILT_TIMER, EV_ADD, 0, millisecInterval, (void*)c);
  if (kevent(kqfd, &ke, 1, 0, 0, 0) == -1) {
    delete c;
    throw std::runtime_error("Failed to add timer to kqueue: " + std::string(strerror(errno)));
  }
}

void runEventLoop(int microsecondDuration, const std::function<bool()>& stopFn) {
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
      DispatchingBatch batch;
      for (size_t fd = 0; fd < fds; ++fd) {
        eventcbclosure* c = (eventcbclosure*)evts[fd].udata;
        if (!c->retired) {
          (c->fn)(c->fd);
          resetMemoryPool();
        }
      }
    }
    t = hobbes::time();
  } while (t < tf && !stopFn());
}

#endif

}
