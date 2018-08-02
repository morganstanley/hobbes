
#ifndef HOBBES_UTIL_TIME_HPP_INCLUDED
#define HOBBES_UTIL_TIME_HPP_INCLUDED

#include <hobbes/util/str.H>
#include <string>
#include <string.h>
#include <sstream>
#include <ctime>

namespace hobbes {

/*******
 * timespans (a number of microseconds)
 *
 *  e.g.: 5s, 20m, 4day2h3m30s200ms456us, ...
 *
 * *****/
inline long readTimespan(std::string s) {
  long r = 0;

  while (s.size() > 0) {
    str::pair p = str::readWhile(&str::isDigit,    s);
    str::pair u = str::readWhile(&str::isNotDigit, p.second);

    long m = 1;
    if (u.first == "us") {                            // microseconds
      m = 1;
    } else if (u.first == "ms") {                     // milliseconds
      m = 1000;
    } else if (u.first == "s") {                      // seconds
      m = 1000000;
    } else if (u.first == "m" || u.first == "min") {  // minutes
      m = 1000000*60;
    } else if (u.first == "h" || u.first == "hour") { // hours
      m = 1000000L*60*60;
    } else if (u.first == "d" || u.first == "day") {  // days
      m = 1000000L*60*60*24;
    }

    r += str::to<uint64_t>(p.first) * m;
    s = u.second;
  }

  return r;
}

inline long readTimespan(const str::seq& ss) {
  long r = 0;
  for (auto s : ss) {
    r += readTimespan(s);
  }
  return r;
}

inline std::string showTimespan(long ts) {
  static const long msus  = 1000;
  static const long sus   = msus * 1000;
  static const long mus   = sus * 60;
  static const long hus   = mus * 60;
  static const long dayus = hus * 24;

  uint64_t x  = labs(ts);
  uint64_t d  = x / dayus; x -= d*dayus;
  uint64_t h  = x / hus;   x -= h*hus;
  uint64_t m  = x / mus;   x -= m*mus;
  uint64_t s  = x / sus;   x -= s*sus;
  uint64_t ms = x / msus;  x -= ms*msus;
  uint64_t us = x;

  std::ostringstream ss;
  if (ts < 0) ss << "-";
  if (d > 0)        ss << d << "d";
  if (h > 0)        ss << h << "h";
  if (m > 0)        ss << m << "m";
  if (s > 0 || (d == 0 && h == 0 && m == 0 && ms == 0 && us == 0)) {
    ss << s << "s";
  }
  if (ms > 0)       ss << ms << "ms";
  if (us > 0)       ss << us << "us";

  return ss.str();
}

/*******
 * times (microseconds since a base time)
 *
 *   e.g.: 01:00:00.000000, 15:34:57.123456, ...
 *
 *******/
inline long mkTime(int h, int m, int s, int u) {
  tm t;
  memset(&t, 0, sizeof(t));

  t.tm_sec   = s;
  t.tm_min   = m;
  t.tm_hour  = h;
  t.tm_mday  = 1;
  t.tm_mon   = 0;
  t.tm_year  = 70;
  t.tm_isdst = -1;

  return (mktime(&t) * 1000 * 1000) + u;
}

inline long readTime(const std::string& x) {
  str::pair h_msu = str::lsplit(x, ":");
  str::pair m_su  = str::lsplit(h_msu.second, ":");
  str::pair s_u   = str::lsplit(m_su.second,  ".");

  int h = h_msu.first.empty() ? 0 : str::to<int>(h_msu.first);
  int m = m_su.first.empty()  ? 0 : str::to<int>(m_su.first);
  int s = s_u.first.empty()   ? 0 : str::to<int>(s_u.first);
  int u = s_u.second.empty()  ? 0 : str::to<int>(s_u.second);

  return mkTime(h,m,s,u);
}

inline std::string showTime(long x) {
  int64_t s  = x / (1000 * 1000);
  int64_t us = x % (1000 * 1000);

  static char buf[256];
  strftime(buf, sizeof(buf), "%H:%M:%S", localtime(reinterpret_cast<time_t*>(&s)));

  std::ostringstream uss;
  uss << us;

  std::ostringstream ss;
  ss << buf;
  ss << ".";
  ss << std::string((uss.str().size() < 6) ? (6 - uss.str().size()) : 0, '0') << uss.str();

  return ss.str();
}

/*******
 * datetimes (microseconds since epoch)
 *
 *   e.g.: 2015-01-01T01:00:00.000000, 1980-05-19T15:34:57.123456, ...
 *
 *******/
inline long mkDateTime(int y, int mon, int d, int h, int min, int s, int u) {
  tm t;
  memset(&t, 0, sizeof(t));

  t.tm_sec   = s;
  t.tm_min   = min;
  t.tm_hour  = h;
  t.tm_mday  = d;
  t.tm_mon   = mon - 1;
  t.tm_year  = y - 1900;
  t.tm_isdst = -1;

  return (mktime(&t) * 1000L * 1000L) + u;
}

inline long timeFromDateTime(long x) {
  int64_t s  = x / (1000L * 1000L);
  int64_t us = x % (1000L * 1000L);
  tm*     t  = localtime(reinterpret_cast<time_t*>(&s));

  if (t) {
    return mkTime(t->tm_hour, t->tm_min, t->tm_sec, us);
  } else {
    return mkTime(0, 0, 0, 0);
  }
}

inline long dateFromDateTime(long x) {
  int64_t s  = x / (1000L * 1000L);
  tm t;
  localtime_r(reinterpret_cast<time_t*>(&s), &t);
  
  t.tm_sec  = 0;
  t.tm_min  = 0;
  t.tm_hour = 0;

  return mktime(&t) * 1000L * 1000L;
}

inline long readDateTime(const std::string& x) {
  str::pair y_mdThmsu = str::lsplit(x, "-");
  str::pair m_dThmsu  = str::lsplit(y_mdThmsu.second, "-");
  str::pair d_Thmsu   = str::lsplit(m_dThmsu.second, "T");
  str::pair h_msu     = str::lsplit(d_Thmsu.second, ":");
  str::pair m_su      = str::lsplit(h_msu.second, ":");
  str::pair s_u       = str::lsplit(m_su.second, ".");

  int y   = y_mdThmsu.first.empty() ? 0 : str::to<int>(y_mdThmsu.first);
  int mon = m_dThmsu.first.empty()  ? 0 : str::to<int>(m_dThmsu.first);
  int d   = d_Thmsu.first.empty()   ? 0 : str::to<int>(d_Thmsu.first);

  int h   = h_msu.first.empty() ? 0 : str::to<int>(h_msu.first);
  int min = m_su.first.empty()  ? 0 : str::to<int>(m_su.first);
  int s   = s_u.first.empty()   ? 0 : str::to<int>(s_u.first);
  int u   = s_u.second.empty()  ? 0 : str::to<int>(s_u.second);

  return mkDateTime(y, mon, d, h, min, s, u);
}

inline std::string showDateTime(long x) {
  int64_t s  = x / (1000L * 1000L);
  int64_t us = x % (1000L * 1000L);

  static char buf[256];
  strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%S", localtime(reinterpret_cast<time_t*>(&s)));

  std::ostringstream uss;
  uss << us;

  std::ostringstream ss;
  ss << buf;
  ss << ".";
  ss << std::string((uss.str().size() < 6) ? (6 - uss.str().size()) : 0, '0') << uss.str();

  return ss.str();
}

}

#endif
