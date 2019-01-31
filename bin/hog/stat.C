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

std::string createSessionHash(const hobbes::datetimeT& timestamp, const hobbes::storage::ProcThread& pt) {
  const hobbes::genHash<std::string> hasher;
  size_t hashed = hasher(hobbes::showDateTime(timestamp.value));
  hobbes::hashAppend(hashed, pt.first);
  hobbes::hashAppend(hashed, pt.second);
  return std::to_string(hashed);
}

}

