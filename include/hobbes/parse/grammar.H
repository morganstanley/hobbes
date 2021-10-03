/*
 * grammar : simplify shorthand for grammar rules (translated "in the obvious way" to plain generative grammars)
 */

#ifndef HOBBES_PARSE_GRAMMAR_HPP_INCLUDED
#define HOBBES_PARSE_GRAMMAR_HPP_INCLUDED

#include <hobbes/lang/expr.H>
#include <hobbes/util/lannotation.H>
#include <string>
#include <sstream>
#include <memory>
#include <map>
#include <vector>

namespace hobbes {

class GrammarValue : public LexicallyAnnotated {
public:
  GrammarValue() = delete;

  GrammarValue(const LexicallyAnnotated&);
  virtual ~GrammarValue() = default;
  virtual void show(std::ostream&) const = 0;
};
using GrammarValuePtr = std::shared_ptr<GrammarValue>;

class GSymRef : public GrammarValue {
public:
  GSymRef(const std::string&, const LexicalAnnotation&);
  void show(std::ostream&) const override;

  const std::string& value() const;
private:
  std::string x;
};

class GStr : public GrammarValue {
public:
  GStr(const std::string&, const LexicalAnnotation&);
  void show(std::ostream&) const override;

  const std::string& value() const;
private:
  std::string x;
};

template <typename T>
  struct switchGrammarValue {
    virtual T with(const GSymRef*) const = 0;
    virtual T with(const GStr*) const = 0;
  };

template <typename T>
  T switchOf(const GrammarValuePtr& p, const switchGrammarValue<T>& f) {
    if (const auto* x = dynamic_cast<const GSymRef*>(p.get())) {
      return f.with(x);
    } else if (const GStr* x = dynamic_cast<const GStr*>(p.get())) {
      return f.with(x);
    } else {
      std::ostringstream ss;
      ss << "Internal error, can't switch on unknown grammar value type: ";
      p->show(ss);
      throw std::runtime_error(ss.str());
    }
  }

struct BoundGrammarValue {
  inline BoundGrammarValue(const std::string& vn, const GrammarValuePtr& e) : varname(vn), element(e) { }
  std::string     varname;
  GrammarValuePtr element;
};
using BoundGrammarValues = std::vector<BoundGrammarValue>;

struct GrammarRule {
  inline GrammarRule(const BoundGrammarValues& vs, const ExprPtr& e) : values(vs), reduction(e) { }

  BoundGrammarValues values;
  ExprPtr            reduction;
};
using GrammarRules = std::vector<GrammarRule>;

using GrammarSymDef = std::pair<std::string, GrammarRules>;
using Grammar = std::vector<GrammarSymDef>;

// allow the construction of parsers from grammars
class cc;
ExprPtr makeParser(cc*, const Grammar&, const LexicalAnnotation&);

}

#endif

