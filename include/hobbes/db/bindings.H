
#ifndef HOBBES_DB_BINDINGS_HPP_INCLUDED
#define HOBBES_DB_BINDINGS_HPP_INCLUDED

#include <hobbes/eval/cc.H>

#include <hobbes/lang/tyunqualify.H>
#include <hobbes/lang/preds/hasfield.H>

#include <set>
#include <string>

namespace hobbes {

void initStorageFileDefs(FieldVerifier*, cc&);

// Resolving a (LoadFile "path" t) constraint for an outputFile-shaped t
// creates/truncates a file at "path" as a side effect of type-checking,
// not evaluation -- disabled by default. Call this to allow the exact
// paths in allowedPaths to be opened for writing.
void enableFileWrites(cc&, const std::set<std::string>& allowedPaths);
// ... or any path at all, for a compiler that only ever sees trusted source
void enableFileWrites(cc&);

}

#endif

