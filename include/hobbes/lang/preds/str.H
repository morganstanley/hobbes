
#ifndef HOBBES_LANG_TYPEPREDS_STR_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_STR_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>
#include <set>
#include <string>

namespace hobbes {

void initStrPredicates(const TEnvPtr&);

// Resolving an (Ls "pattern" x) constraint globs the filesystem and puts the
// matching names into the type, as a side effect of type-constraint
// resolution rather than evaluation -- disabled by default. Call this to
// allow the exact patterns in allowedPatterns to be expanded ...
void enableFilesystemGlobs(const TEnvPtr&, const std::set<std::string>& allowedPatterns);
// ... or any pattern at all, for a compiler that only ever sees trusted source
void enableFilesystemGlobs(const TEnvPtr&);

}

#endif

