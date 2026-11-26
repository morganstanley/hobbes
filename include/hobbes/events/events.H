/*******************************
 * events : a runtime system for reactive programs
 ******************************/

#ifndef HOBBES_EVENTS_EVENTS_HPP_INCLUDED
#define HOBBES_EVENTS_EVENTS_HPP_INCLUDED

#include <functional>

namespace hobbes {

// add a read event handler for a file descriptor
using eventhandler = void (*)(int, void *);

// return true to automatically repeat after an interval
using timerfunc = bool (*)();

void addTimer(timerfunc f, int millisecInterval);

void registerEventHandler(int fd, const std::function<void(int)>& fn, bool vn = false /* only used on BSD */);
void registerEventHandler(int fd, eventhandler f, void* ud, bool vn = false /* only used on BSD */);
void unregisterEventHandler(int fd);

// also call fd's registered handler while fd is writable (or stop, given false).
// The handler is the one registerEventHandler installed; it is not told which
// condition woke it, so it should simply try both its reads and its pending
// writes without blocking. Write interest is level triggered: clear it once
// there is nothing left to write, or the handler is called on every pass of the
// loop. Does nothing for a descriptor with no registered handler; throws
// std::runtime_error if the change is refused (epoll_ctl / kevent failed).
void setEventWriteInterest(int fd, bool wantWritable);

// stop calling fd's registered handler for readability (given false), or
// start again (given true, as a newly registered handler is). For a handler
// that has input it is not ready to take: the loop is level triggered, so a
// descriptor left readable and unread would otherwise call its handler on
// every pass. A failed or reset descriptor may still be reported while reads
// are paused. Does nothing for a descriptor with no registered handler; throws
// std::runtime_error if the change is refused (epoll_ctl / kevent failed).
void setEventReadInterest(int fd, bool wantReadable);
void registerInterruptHandler(const std::function<void()>& fn);

// run a single-step or indefinite event loop
bool stepEventLoop(int timeoutMS = -1, const std::function<bool()>& stopFn = [] { return false; });
// if stopFn is user defined, a repeatable timer *MUST* be added to trigger the check on stopFn()
// hobbes::addTimer([] { return true; /* repeatable */}, myTimeout);
// hobbes::runEventLoop(myStopFn);
void runEventLoop(const std::function<bool()>& stopFn = [] { return false; });

// run the event loop for some number of microseconds
void runEventLoop(int microsecondDuration, const std::function<bool()>& stopFn = [] { return false; });

}

#endif

