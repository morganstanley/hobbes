/*
 * parser : a module for deriving code to parse context-free grammars
 */

#ifndef HOBBES_PARSE_PARSER_HPP_INCLUDED
#define HOBBES_PARSE_PARSER_HPP_INCLUDED

#include <hobbes/parse/terminal.H>
#include <hobbes/lang/expr.H>
#include <hobbes/util/lannotation.H>
#include <map>
#include <vector>

namespace hobbes {

// a parser rule looks like:
//   S -> x0:C0 ... xn:Cn { R(x0..xn) }
// where:
//   S is the name of a non-terminal symbol
//   xi is a variable name fresh for this rule
//   Ci is the name of a terminal or non-terminal symbol
//   R is an arbitrary user expression that may involve values bound by the xi
// such that:
//   the type of R matches the type of R for all other definitions of S
struct ParseRule {
  typedef std::pair<std::string, terminal*> Binding;
  typedef std::vector<Binding> Bindings;

  inline ParseRule(terminal* s, const Bindings& bs, const ExprPtr& r) : symbol(s), bindings(bs), reducer(r) { }

  terminal* symbol;
  Bindings  bindings;
  ExprPtr   reducer;
};

// and a parser is a set of such rules
typedef std::vector<ParseRule> Parser;

// consume a grammar to produce a parser for that grammar
class cc;

ExprPtr makeParser(cc*, const Parser&, const precedence& prec, const LexicalAnnotation&);
ExprPtr makeParser(cc*, const Parser&, terminal* root, const precedence& prec, const LexicalAnnotation&);

// show a parser (for convenient debugging)
void show(std::ostream&, const Parser&);

}

#endif

