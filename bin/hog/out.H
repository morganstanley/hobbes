#include <hobbes/util/time.H> // showDateTime
#include <hobbes/util/perf.H> // time

#include <iostream>

namespace hog {

inline std::ostream& out() {
  return std::cout << "[" << hobbes::showDateTime(hobbes::time() / 1000) << "]: ";
}

} // namespace hog
