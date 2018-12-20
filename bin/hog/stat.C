#include "stat.H"

#include <string>

namespace hog {

inline std::string statFilePrefix() {
  return hobbes::storage::defaultStoreDir() + "/hogstat";
}

StatFile& StatFile::instance() {
  static StatFile statFile;
  return statFile;
}

StatFile::StatFile() : statFile(hobbes::fregion::uniqueFilename(statFilePrefix(), ".db")) {}

}

