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
  virtual ~Regex(void) = default;
  virtual void show(std::ostream&) const = 0;
};
typedef std::shared_ptr<Regex> RegexPtr;
typedef std::vector<RegexPtr> Regexes;
RegexPtr parseRegex(const std::string&);
str::seq bindingNames(const RegexPtr&);

typedef size_t RegexIdx;
typedef std::set<RegexIdx> RegexIdxs;
typedef std::map<size_t, RegexIdxs> RStates;

typedef std::map<size_t, str::seq> CaptureVarsAt;

struct CRegexes {
  std::string   fname;         // the low-level function that evaluates a set of regular expressions against an input
  ExprPtr       captureBuffer; // an expression producing a buffer for recording capture groups
  CaptureVarsAt captureVarsAt; // capture variable names by regex index
  RStates       rstates;       // regex result state -> set of input regex ids (to determine which outer match rows to select for a given regex match)
};

CRegexes makeRegexFn(cc*, const Regexes&, const LexicalAnnotation&);

typedef std::pair<std::string, ExprPtr> CVarDef;
typedef std::vector<CVarDef> CVarDefs;

CVarDefs unpackCaptureVars(const std::string& strVar, const std::string& bufferVar, const CRegexes&, size_t state, const LexicalAnnotation&);

}

#endif

