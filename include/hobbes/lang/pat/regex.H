/*
 * regex : constructs functions to perform regex matches in match expressions
 */

#ifndef HOBBES_LANG_PAT_REGEX_HPP_INCLUDED
#define HOBBES_LANG_PAT_REGEX_HPP_INCLUDED

#include <hobbes/util/lannotation.H>
#include <hobbes/util/str.H>
#include <map>
#include <vector>
#include <string>
#include <set>

namespace hobbes {

class cc;

/*
 * makeRegexFn - generate a function that performs an 'ordered regex match'
 *
 *   This generated function F will be defined:
 *     F :: (Array cs char) => cs -> int
 *
 *   Such that a 'string' (e.g. [char], <std.string>, ...) can be tested to determine which regexes
 *   it matches (the 'int' result represents an index into a table determined here that classifies
 *   states representing one or more regex matches).
 *
 *   inputs:
 *     cc*     : a compiler context within which to generate this regex function
 *     regexes : a sequences of regexes in priority order to match against later input
 *
 *   outputs:
 *     result  : the name of the generated function
 *     rstates : a mapping of F return codes to input regex indexes
 */
struct Regex {
  virtual ~Regex() = default;
  virtual void show(std::ostream&) const = 0;
};
using RegexPtr = std::shared_ptr<Regex>;
using Regexes = std::vector<RegexPtr>;
RegexPtr parseRegex(const std::string&);
str::seq bindingNames(const RegexPtr&);

using RegexIdx = size_t;
using RegexIdxs = std::set<RegexIdx>;
using RStates = std::map<size_t, RegexIdxs>;

using CaptureVarsAt = std::map<size_t, str::seq>;

struct CRegexes {
  std::string   fname;         // the low-level function that evaluates a set of regular expressions against an input
  ExprPtr       captureBuffer; // an expression producing a buffer for recording capture groups
  CaptureVarsAt captureVarsAt; // capture variable names by regex index
  RStates       rstates;       // regex result state -> set of input regex ids (to determine which outer match rows to select for a given regex match)
};

CRegexes makeRegexFn(cc*, const Regexes&, const LexicalAnnotation&);

using CVarDef = std::pair<std::string, ExprPtr>;
using CVarDefs = std::vector<CVarDef>;

CVarDefs unpackCaptureVars(const std::string& strVar, const std::string& bufferVar, const CRegexes&, size_t state, const LexicalAnnotation&);

}

#endif

