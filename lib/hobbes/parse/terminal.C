
#include <hobbes/parse/terminal.H>

namespace hobbes {

// characters
character::character(char x) : x(x) { }
char character::value() const { return this->x; }
void character::value(char c) { this->x = c; }

void character::show(std::ostream& out) const {
  out << "'" << this->x << "'";
}

PatternPtr character::matchPattern() const {
  return PatternPtr(new MatchLiteral(PrimitivePtr(new Char(this->x, LexicalAnnotation::null())), LexicalAnnotation::null()));
}

ExprPtr character::matchRefExpr() const {
  return ExprPtr(new Unit(LexicalAnnotation::null()));
}

// symbols
symbol::symbol(const std::string& sname) : sname(sname) { }
const std::string& symbol::name() const { return this->sname; }

void symbol::show(std::ostream& out) const {
  out << this->sname;
}

[[noreturn]] PatternPtr symbol::matchPattern() const {
  throw std::runtime_error("Internal error, can't match abstract symbol");
}

[[noreturn]] ExprPtr symbol::matchRefExpr() const {
  throw std::runtime_error("Internal error, can't match abstract symbol");
}

// eof
endOfFile::endOfFile() { }

void endOfFile::show(std::ostream& out) const {
  out << "$";
}

terminal* endOfFile::value() {
  static endOfFile e;
  return &e;
}

[[noreturn]] PatternPtr endOfFile::matchPattern() const {
  throw std::runtime_error("Internal error, can't match EOF");
}

[[noreturn]] ExprPtr endOfFile::matchRefExpr() const {
  throw std::runtime_error("Internal error, can't match EOF");
}

}

