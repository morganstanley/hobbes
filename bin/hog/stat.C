#include "stat.H"

#include <string>

#include <hobbes/util/hash.H>

namespace hog {

std::string StatFile::directory = hobbes::storage::defaultStoreDir();

StatFile& StatFile::instance() {
  static StatFile statFile;
  return statFile;
}

StatFile::StatFile() : statFile(StatFile::directory + "/hogstat.db") {}

size_t createSessionHash(const hobbes::datetimeT& timestamp, const hobbes::storage::ProcThread& pt) {
  const hobbes::genHash<decltype(timestamp.value)> hasher;
  size_t hashed = hasher(timestamp.value);
  hobbes::hashAppend(hashed, pt.first);
  hobbes::hashAppend(hashed, pt.second);
  return hashed;
}

}

