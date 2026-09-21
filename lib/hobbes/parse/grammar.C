
#include <hobbes/parse/grammar.H>
#include <hobbes/parse/parser.H>

namespace hobbes {

using symbols = std::map<std::string, terminal *>;

terminal* sym(symbols* ss, const std::string& x) {
  auto si = ss->find(x);
  if (si != ss->end()) {
    return si->second;
  } else {
    terminal* r = new symbol(x);
    (*ss)[x] = r;
    return r;
  }
}

using chars = std::map<char, terminal *>;

terminal* chr(chars* cs, char x) {
  auto ci = cs->find(x);
  if (ci != cs->end()) {
    return ci->second;
  } else {
    terminal* r = new character(x);
    (*cs)[x] = r;
    return r;
  }
}

ParseRule::Bindings toParseRuleBindings(symbols* ss, chars* cs, const BoundGrammarValues& gvs) {
  ParseRule::Bindings r;
  for (const auto& gv : gvs) {
    if (const auto* x = dynamic_cast<const GSymRef*>(gv.element.get())) {
      r.push_back(ParseRule::Binding(gv.varname, sym(ss, x->value())));
    } else if (const GStr* x = dynamic_cast<const GStr*>(gv.element.get())) {
      for (auto c : x->value()) {
        r.push_back(ParseRule::Binding("_", chr(cs, c)));
      }
    } else {
      throw std::runtime_error("Internal error, can't translate unknown grammar rule element");
    }
  }
  return r;
}

ExprPtr makeParser(cc* c, const Grammar& g, const LexicalAnnotation& la) {
  Parser p;
  symbols syms;
  chars cs;

  for (const auto& gp : g) {
    terminal* s = sym(&syms, gp.first);
    for (const GrammarRule& gr : gp.second) {
      p.push_back(ParseRule(s, toParseRuleBindings(&syms, &cs, gr.values), gr.reduction));
    }
  }

  return makeParser(c, p, precedence(), la);
}

// grammar rule data
GrammarValue::GrammarValue(const LexicallyAnnotated& la) : LexicallyAnnotated(la) {
}

GSymRef::GSymRef(const std::string& x, const LexicalAnnotation& la) : GrammarValue(la), x(x) { }
void GSymRef::show(std::ostream& out) const { out << this->x; }
const std::string& GSymRef::value() const { return this->x; }

GStr::GStr(const std::string& x, const LexicalAnnotation& la) : GrammarValue(la), x(x) { }
void GStr::show(std::ostream& out) const { out << "\"" << this->x << "\""; }
const std::string& GStr::value() const { return this->x; }

}

