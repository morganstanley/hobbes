#include "stat.H"

#include <string>

namespace hog {

StatFile& StatFile::instance() {
  static StatFile statFile;
  return statFile;
}

StatFile::StatFile() : statFile(hobbes::storage::defaultStoreDir() + "/hogstat.db") {}

}

