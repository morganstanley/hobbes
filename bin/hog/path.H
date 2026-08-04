#ifndef HOG_PATH_H_INCLUDED
#define HOG_PATH_H_INCLUDED

#include <string>

namespace hog {

// Is 'g' safe to substitute as the $GROUP component of a storage path?
//
// A group name arriving from an untrusted source (a batchrecv network client)
// is substituted verbatim into a filesystem path template, so it must denote a
// single, non-hidden path component. Reject anything that could escape the
// intended data directory: empty names, a leading '.' (covers "." and ".." and
// hidden dirs), and any name containing a path separator or NUL.
inline bool isValidGroupName(const std::string& g) {
  if (g.empty() || g[0] == '.') {
    return false;
  }
  for (const char c : g) {
    if (c == '/' || c == '\0') {
      return false;
    }
  }
  return true;
}

std::string instantiateDir(const std::string& groupName, const std::string& dir);

}

#endif

