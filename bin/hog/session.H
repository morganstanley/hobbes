/*
 * session : shorthand for running sessions to accumulate structured data
 */

#ifndef HOG_SESSION_H_INCLUDED
#define HOG_SESSION_H_INCLUDED

#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/db/series.H>
#include <hobbes/util/str.H>

#include <stdexcept>

namespace hog {

// Is 'n' usable as a log statement name?
//
// A statement name arriving from an untrusted source (a batchrecv network
// client) is used three ways that all treat it as code rather than data: it is
// bound as a hobbes symbol (StoredSeries::bindAs), spliced into hobbes source
// text that is then compiled, and used as a variant constructor name. A name
// carrying hobbes syntax therefore rewrites the expression the daemon
// compiles, so names are restricted to what the producers actually emit --
// HSTORE(group, name, ...) names a C++ token -- rather than to whatever
// parses.
inline bool isValidStatementName(const std::string& n) {
  return hobbes::str::isIdentifier(n);
}

// Does 't' embed an expression node?
//
// A statement's type description is decoded with the general type decoder,
// which accepts TExpr -- a whole serialized expression tree carried inside a
// type. Nothing a log producer legitimately stores needs one, and accepting it
// hands the compiler an expression that came off the wire, so reject it.
bool embedsExpression(const hobbes::MonoTypePtr&);

// make a storage file (via appendStorageSession) and produce a function to write transactions into it
// support optionally merging log session data where type structures are identical
// provide a hook to users who want to see what output file gets decided and why
class SessionGroup;
SessionGroup* makeSessionGroup(bool consolidate = false, hobbes::StoredSeries::StorageMode sm = hobbes::StoredSeries::Raw);

using ProcessTxnF = std::function<void (hobbes::storage::Transaction &)>;
ProcessTxnF appendStorageSession(SessionGroup*, const std::string& dirPfx, hobbes::storage::PipeQOS qos, hobbes::storage::CommitMethod cm, const hobbes::storage::statements& stmts);

// common way to prepare output directories from dir prefix patterns
std::string ensureDirExists(const std::string& dirPfx);

class ShutdownException : public std::runtime_error {
public:
  explicit ShutdownException(const std::string& msg) : std::runtime_error(msg) {}
};

}

#endif

