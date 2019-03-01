/*
 * config.H : functions for configuring from command line parameters and
 * validating configuration.
 */

#ifndef HOG_CONFIG_H_INCLUDED
#define HOG_CONFIG_H_INCLUDED

#include <ostream>
#include <set>
#include <string>
#include <vector>
#include <hobbes/db/series.H>
#include <hobbes/util/str.H>

#include "stat.H"

namespace hog {

struct RunMode {
  enum type { local, batchsend, batchrecv };
  // common / local
  type t;
  std::string dir;
  std::string groupServerDir;
  std::set<std::string> groups;
  bool consolidate;
  bool skipRecovery;
  hobbes::StoredSeries::StorageMode storageMode;

  // batchsend
  size_t clevel;
  size_t batchsendsize;
  long batchsendtime;
  std::vector<std::string> sendto;

  // batchrecv
  std::string localport;
};

template <typename T>
inline std::ostream& operator<<(std::ostream& o, const std::vector<T>& xs) {
  o << "[";
  if (xs.size() > 0) {
    auto x = xs.cbegin();
    o << "\"" << *x << "\"";
    ++x;
    for (; x != xs.cend(); ++x) {
      o << ", \"" << *x << "\"";
    }
  }
  o << "]";
  return o;
}

template <typename T>
inline std::ostream& operator<<(std::ostream& o, const std::set<T>& xs) {
  o << "{";
  if (xs.size() > 0) {
    auto x = xs.cbegin();
    o << "\"" << *x << "\"";
    ++x;
    for (; x != xs.cend(); ++x) {
      o << ", \"" << *x << "\"";
    }
  }
  o << "}";
  return o;
}

std::ostream& operator<<(std::ostream& o, const RunMode& m);

RunMode config(int argc, const char** argv);

}

#endif
