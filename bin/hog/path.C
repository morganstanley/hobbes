#include <hobbes/util/str.H>

#include "path.H"

namespace hog {

std::string instantiateDir(const std::string& groupName, const std::string& dir) {
  // instantiate group name
  std::string x = hobbes::str::replace<char>(dir, "$GROUP", groupName);

  // instantiate date
  time_t now = ::time(0);
  if (const tm* t = localtime(&now)) {
    std::ostringstream ss;
    ss << t->tm_year + 1900 << "." << (t->tm_mon < 10 ? "0" : "") << t->tm_mon + 1 << "." << (t->tm_mday < 10 ? "0" : "") << t->tm_mday;
    x = hobbes::str::replace<char>(x, "$DATE", ss.str());
  }

  // that's all we will substitute
  return x;
}

}

