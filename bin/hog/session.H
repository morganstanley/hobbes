/*
 * session : shorthand for running sessions to accumulate structured data
 */

#ifndef HOG_SESSION_H_INCLUDED
#define HOG_SESSION_H_INCLUDED

#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/db/series.H>

namespace hog {

struct Session {
  // the file being written into
  hobbes::writer* db;

  // sections of the file for structured data
  typedef std::vector<hobbes::StoredSeries*> StoredSeriess;
  StoredSeriess streams;

  // functions for actually writing stream data
  typedef void (*WriteFn)(hobbes::storage::Transaction*);
  typedef std::vector<WriteFn> WriteFns;

  WriteFns writeFns;

  // scratch space to accumulate transaction descriptions
  // (just used for manual-commit sessions)
  std::vector<size_t> txnScratch;
};

// initStorageSession : make a unique storage file and a function to write transactions into it
typedef std::function<void(hobbes::storage::Transaction&)> ProcessTxnF;
ProcessTxnF initStorageSession(Session* s, const std::string& dirPfx, hobbes::storage::PipeQOS qos, hobbes::storage::CommitMethod cm, const hobbes::storage::statements& stmts);

// common way to prepare output directories from dir prefix patterns
std::string ensureDirExists(const std::string& dirPfx);

}

#endif

