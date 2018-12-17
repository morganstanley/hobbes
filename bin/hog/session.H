/*
 * session : shorthand for running sessions to accumulate structured data
 */

#ifndef HOG_SESSION_H_INCLUDED
#define HOG_SESSION_H_INCLUDED

#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/db/series.H>

#include <stdexcept>

namespace hog {

// make a storage file (via appendStorageSession) and produce a function to write transactions into it
// support optionally merging log session data where type structures are identical
class SessionGroup;
SessionGroup* makeSessionGroup(bool consolidate = false, hobbes::StoredSeries::StorageMode sm = hobbes::StoredSeries::Raw);

typedef std::function<void(hobbes::storage::Transaction&)> ProcessTxnF;
ProcessTxnF appendStorageSession(SessionGroup*, const std::string& dirPfx, hobbes::storage::PipeQOS qos, hobbes::storage::CommitMethod cm, const hobbes::storage::statements& stmts);

// common way to prepare output directories from dir prefix patterns
std::string ensureDirExists(const std::string& dirPfx);

class ShutdownException : public std::runtime_error {
public:
  explicit ShutdownException(const std::string& msg) : std::runtime_error(msg) {}
};

}

#endif

