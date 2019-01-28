#include "stat.H"

#include <string>

namespace hog {

std::string StatFile::directory = hobbes::storage::defaultStoreDir();

StatFile& StatFile::instance() {
  static StatFile statFile;
  return statFile;
}

StatFile::StatFile() : statFile(StatFile::directory + "/hogstat.db") {}

}

