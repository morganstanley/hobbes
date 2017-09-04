
#ifndef HOBBES_EVAL_SEARCH_HPP_INCLUDED
#define HOBBES_EVAL_SEARCH_HPP_INCLUDED

#include <hobbes/lang/expr.H>
#include <hobbes/lang/type.H>
#include <hobbes/util/lannotation.H>
#include <vector>
#include <map>

namespace hobbes {

struct SearchEntry {
  LexicalAnnotation la;  // where in a user's source this entry comes from
  std::string       sym; // a symbol matching a user query
  MonoTypePtr       ty;  // the exact result type given by this symbol
};
typedef std::vector<SearchEntry> SearchEntries;

struct SearchCache {
  std::map<MonoTypePtr, SearchEntries> univByType;
};
class cc;

// find functions/properties from an expression/type to reach a result type
SearchEntries search(cc&, SearchCache&, const MonoTypePtr&, const MonoTypePtr&);
SearchEntries search(cc&, SearchCache&, const ExprPtr&, const MonoTypePtr&);

}

#endif

