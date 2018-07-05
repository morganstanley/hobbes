
#ifndef HOBBES_UTIL_PERF_HPP_INCLUDED
#define HOBBES_UTIL_PERF_HPP_INCLUDED

#include <hobbes/util/os.H>
#include <string>
#include <sstream>
#include <iostream>
#include <time.h>
#include <sys/time.h>

namespace hobbes {

#if !defined(BUILD_OSX) or defined(CLOCK_REALTIME)
// access a nanosecond timer to do performance-testing on expressions
inline long tick() {
  timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
    return ts.tv_sec * 1000000000L + ts.tv_nsec;
  } else {
    return 0;
  }
}

// access a nanosecond clock
inline long time() {
  timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts) == 0) {
    return ts.tv_sec * 1000000000L + ts.tv_nsec;
  } else {
    return 0;
  }
}
#else
// degenerate timers on older versions of OSX
inline long tick() {
  struct timeval t;
  if (gettimeofday(&t, 0) == 0) {
    return ((t.tv_sec*1000000)+t.tv_usec)*1000;
  } else {
    return 0;
  }
}

inline long time() {
  return tick();
}
#endif

inline std::string describeNanoTime(long x) {
  static const double micros  = 1000;
  static const double millis  = 1000*micros;
  static const double seconds = 1000*millis;
  static const double minutes = 60*seconds;

  double t = static_cast<double>(x);
  std::ostringstream ss;

  if ((t / minutes) >= 1) {
    ss << (t / minutes) << "m";
  } else if ((t / seconds) >= 1) {
    ss << (t / seconds) << "s";
  } else if ((t / millis) >= 1) {
    ss << (t / millis) << "ms";
  } else if ((t / micros) >= 1) {
    ss << (t / micros) << "us";
  } else {
    ss << t << "ns";
  }

  return ss.str();
}

struct probe {
  std::string msg;
  long t0;
  probe(const std::string& msg) : msg(msg) {
    t0 = tick();
  }

  void report() const {
    long dt = tick() - this->t0;
    static const double micros  = 1000;
    static const double millis  = 1000*micros;
    static const double seconds = 1000*millis;

    if (dt > (1*seconds)) {
      std::cout << msg << ": " << std::flush;
      std::cout << describeNanoTime(tick() - this->t0) << std::endl;
    }
  }
};

template <long* globalTC>
  struct timed_region {
    long t0;

    static bool activelyTiming() {
      return (*globalTC % 2) == 1;
    }

    static void activelyTiming(bool f) {
      if (f) {
        if ((*globalTC % 2) == 0) { *globalTC += 1; }
      } else {
        if ((*globalTC % 2) == 1) { *globalTC += 1; }
      }
    }

    timed_region() {
      if (activelyTiming()) {
        t0 = -1;
      } else {
        t0 = hobbes::tick();
        activelyTiming(true);
      }
    }

    ~timed_region() {
      if (t0 != -1) {
        *globalTC += hobbes::tick() - t0;
        activelyTiming(false);
      }
    }
  };

#if defined(__i386__)
inline unsigned long long rdtsc(void) {
  unsigned long long int x;
  __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
  return x;
}
#elif defined(__x86_64__)
inline unsigned long long rdtsc(void) {
  unsigned hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return static_cast<unsigned long long>(lo)|(static_cast<unsigned long long>(hi)<<32);
}
#endif

}

#endif
