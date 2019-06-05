
#ifndef HOBBES_PARSE_TERMINAL_HPP_INCLUDED
#define HOBBES_PARSE_TERMINAL_HPP_INCLUDED

#include <hobbes/parse/data.H>
#include <hobbes/lang/pat/pattern.H>
#include <iostream>
#include <vector>
#include <map>
#include <set>

namespace hobbes {

/*
 * standard terminals
 */

// a "terminal" is an opaque value, assumed primitive
struct terminal {
  // format this terminal for output on a text console
  virtual void show(std::ostream&) const = 0;

  // produce match patterns and value-reference expressions
  // (to generate code to try to match for a terminal and, if matched, to refer to its value)
  virtual PatternPtr matchPattern() const = 0;
  virtual ExprPtr matchRefExpr() const = 0;
};
typedef std::vector<terminal*> terminals;
typedef std::set<terminal*> terminalset;

// characters can be terminals
class character : public terminal {
public:
  character(char x = 0);
  char value() const;
  void value(char);
  void show(std::ostream&) const;

  PatternPtr matchPattern() const;
  ExprPtr matchRefExpr() const;
private:
  char x;
};

// a named symbol, distinct from other terminals
class symbol : public terminal {
public:
  symbol(const std::string& sname);
  const std::string& name() const;
  void show(std::ostream& out) const;

  [[noreturn]] PatternPtr matchPattern() const;
  [[noreturn]] ExprPtr matchRefExpr() const;
private:
  std::string sname;
};

// EOF is a special terminal
class endOfFile : public terminal {
public:
  endOfFile();
  void show(std::ostream&) const;
  static terminal* value();

  [[noreturn]] PatternPtr matchPattern() const;
  [[noreturn]] ExprPtr matchRefExpr() const;
};

// allow terminals to be ordered by basic precedence / associativity definitions
namespace assoc {
  enum pref { non, left, right };
}

struct prec {
  inline prec() : level(0), asc(assoc::non) { }
  inline prec(nat level, assoc::pref asc) : level(level), asc(asc) { }

  nat         level;
  assoc::pref asc;
};

typedef std::map<terminal*, prec> precedence;

}

#endif
