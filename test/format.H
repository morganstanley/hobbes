#ifndef HOBBES_TEST_FORMAT_H_INCLUDED
#define HOBBES_TEST_FORMAT_H_INCLUDED

#include <hobbes/util/perf.H>

struct Result;

struct null {};

template <typename T>
inline void showJSON(const T& x, std::ostream& os) {
  os << x;
}

inline void showJSON(const char* xs, std::ostream& os) {
  os << "\"";
  while (auto c = *xs++) {
    switch (c) {
      case '"':  os << "\\\""; break;
      case '\\': os << "\\\\"; break;
      case '\n': os << "\\n";  break;
      case '\r': os << "\\r";  break;
      case '\t': os << "\\t";  break;
      default:   os << c;      break;
    }
  }
  os << "\"";
}

inline void showJSON(const std::string& x, std::ostream& os) {
  showJSON(x.c_str(), os);
}

inline void showJSON(const null&, std::ostream& os) {
  os << "null";
}

inline void showJSON(const Result::Status& x, std::ostream& os) {
  switch (x) {
    case Result::Status::Pass: showJSON("pass", os); break;
    case Result::Status::Fail: showJSON("fail", os); break;
    case Result::Status::Skipped: showJSON("skipped", os); break;
  }
}

inline void showJSON(const Result& x, std::ostream& os) {
  os << "{";
  showJSON("name", os); os << ":"; showJSON(x.testcase, os); os << ",";
  showJSON("status", os); os << ":"; showJSON(x.status, os); os << ",";
  showJSON("duration", os); os << ":"; showJSON(hobbes::describeNanoTime(x.duration), os); os << ",";
  showJSON("error", os); os << ":"; x.error.empty() ? showJSON(null(), os) : showJSON(x.error.c_str(), os);
  os << "}";
}

template <typename T, typename U>
inline void showJSON(const std::pair<T, U>& x, std::ostream& os) {
  os << "{";
  showJSON("name", os); os << ":"; showJSON(x.first, os); os << ",";
  showJSON("data", os); os << ":"; showJSON(x.second, os);
  os << "}";
}

template <typename T>
inline void showJSON(const std::vector<T>& xs, std::ostream& os) {
  bool skip = true;
  os << "[";
  for (const auto & p : xs) {
    if (skip) {
      skip = false;
    } else {
      os << ",";
    }
    showJSON(p, os);
  }
  os << "]";
}

#endif

