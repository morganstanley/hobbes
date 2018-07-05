/*
 * pattern : defines a high-level pattern matching expression
 */

#ifndef HOBBES_LANG_PAT_PATTERN_HPP_INCLUDED
#define HOBBES_LANG_PAT_PATTERN_HPP_INCLUDED

#include <hobbes/lang/expr.H>
#include <hobbes/lang/pat/regex.H>
#include <hobbes/util/str.H>
#include <hobbes/util/ptr.H>
#include <hobbes/util/hash.H>
#include <hobbes/util/lannotation.H>

namespace hobbes {

typedef std::vector<size_t> Idxs;

// the main signature for this module -- convert a pattern match expression to a primitive compilable expression
class Pattern;
typedef std::shared_ptr<Pattern> PatternPtr;
typedef std::vector<PatternPtr>  Patterns;

struct PatternRow {
  inline PatternRow() { }
  inline PatternRow(const Patterns& ps, const ExprPtr& r) : patterns(ps), result(r) { }
  inline PatternRow(const Patterns& ps, const ExprPtr& g, const ExprPtr& r) : patterns(ps), guard(g), result(r) { }

  Patterns patterns; // match conditions for this row
  ExprPtr  guard;    // a required condition for a match (may be null if there is no guard)
  ExprPtr  result;   // the expression to be evaluated when the given pattern conditions are met
};
typedef std::vector<PatternRow> PatternRows;
typedef std::shared_ptr<std::vector<std::pair<size_t, PatternRow>>> UnreachableMatchRowsPtr;

// support quick hashing of pattern tables
bool operator==(const PatternRow&, const PatternRow&);
size_t hash(const PatternRow&);

// sneak the whole compiler through a back door to see how bindings are set up and maybe to do low-level compilation for special cases of pattern matches
class cc;

// evaluate the expression for the first pattern row matching the input
ExprPtr compileMatch(cc*, const Exprs&, const PatternRows&, const LexicalAnnotation&);

// produce an expression to test whether an expression matches a single pattern
ExprPtr compileMatchTest(cc*, const ExprPtr&, const PatternPtr&, const LexicalAnnotation&);

// a general representation of patterns to match
class Pattern : public LexicallyAnnotated {
public:
  virtual void show(std::ostream&) const = 0;
  virtual bool operator==(const Pattern&) const = 0;

  // allow explicit naming of stages of a pattern (for internal use only)
  const std::string& name() const;
  void name(const std::string&);
private:
  std::string pname;
protected:
  virtual void assignSubNames(const std::string&) = 0;

  // improves performance of case-analysis over instances (to avoid 'dynamic_cast')
public:
  int case_id() const;
protected:
  Pattern(int cid, const LexicalAnnotation&);
private:
  int cid;
};

template <typename Case>
  class PatternCase : public Pattern {
  public:
    typedef PatternCase<Case> Base;
    PatternCase(const LexicalAnnotation&);
  };

std::string show(const PatternPtr& p);
std::string show(const Patterns& ps);
std::string show(const PatternRow& pr);

// literal patterns
//   contain a "proxy value" for internal equality/ordering checks: ((), false, 0xde, 'c', 1, 3L, 2.3, ...)
//   contain a "residual expression value" for generating code
//   (this distinction allows us to use aliased constants like exchanges, timespans, etc in match expressions)
class MatchLiteral : public PatternCase<MatchLiteral> {
public:
  MatchLiteral(const PrimitivePtr& proxy, const LexicalAnnotation&);
  MatchLiteral(const PrimitivePtr& proxy, const ExprPtr& value, const LexicalAnnotation&);

  const ExprPtr&      expression()    const;
  const PrimitivePtr& equivConstant() const;

  void show(std::ostream&) const;
  bool operator==(const Pattern&) const;

  static const int type_case_id = 0;
private:
  PrimitivePtr p;
  ExprPtr      e;

  void assignSubNames(const std::string&);
};

// match any value, maybe bind it to a variable name
class MatchAny : public PatternCase<MatchAny> {
public:
  MatchAny(const std::string& vn, const LexicalAnnotation&);

  const std::string& value() const;

  void show(std::ostream&) const;
  bool operator==(const Pattern&) const;

  static const int type_case_id = 1;
private:
  std::string vn;

  void assignSubNames(const std::string&);
};

// match an array of patterns
class MatchArray : public PatternCase<MatchArray> {
public:
  MatchArray(const Patterns& ps, const LexicalAnnotation&); // default indexes = [0,ps.size())

  const PatternPtr& pattern(size_t i) const;
  const Idxs&       indexes() const;
  size_t            size() const; // alias for indexes().size()

  // allow external permutation of array match sequence
  void indexes(const Idxs&);

  void show(std::ostream&) const;
  bool operator==(const Pattern&) const;

  static const int type_case_id = 2;
private:
  Patterns ps;
  Idxs     idxs;

  void assignSubNames(const std::string&);
};

// match a regular expression
class MatchRegex : public PatternCase<MatchRegex> {
public:
  MatchRegex(const RegexPtr&, const LexicalAnnotation&);
  MatchRegex(const std::string&, const LexicalAnnotation&);

  std::string text() const;
  const RegexPtr& value() const;

  static PatternPtr toRegex(const MatchArray&);

  void show(std::ostream&) const;
  bool operator==(const Pattern&) const;
  static const int type_case_id = 3;
private:
  RegexPtr regex;

  void assignSubNames(const std::string&);
};

// match a record of patterns
class MatchRecord : public PatternCase<MatchRecord> {
public:
  typedef std::pair<std::string, PatternPtr> Field;
  typedef std::vector<Field>                 Fields;

  MatchRecord(const Fields& fs, const LexicalAnnotation&); // default indexes = [0,fs.size())

  const Field& pattern(size_t i) const;
  const Idxs&  indexes() const;
  size_t       size() const; // alias for indexes().size()

  const Fields& fields() const;
  void fields(const Fields&);

  void show(std::ostream&) const;
  bool operator==(const Pattern&) const;

  static const int type_case_id = 4;
private:
  Fields fs;
  Idxs   is;
  
  void assignSubNames(const std::string&);

  static void show(std::ostream&, const Field& f);
};

// match a variant constructor
class MatchVariant : public PatternCase<MatchVariant> {
public:
  MatchVariant(const std::string& lbl, const PatternPtr& p, const LexicalAnnotation&);

  const std::string& label() const;
  const PatternPtr&  value() const;

  void show(std::ostream&) const;
  bool operator==(const Pattern&) const;

  static const int type_case_id = 5;
private:
  std::string lbl;
  PatternPtr  p;
  
  void assignSubNames(const std::string&);
};

template <typename Case>
  PatternCase<Case>::PatternCase(const LexicalAnnotation& la) : Pattern(Case::type_case_id, la) {
  }

// destruction-side for high-level pattern representations
template <typename T>
  struct switchPattern {
    virtual T with(const MatchLiteral*) const = 0;
    virtual T with(const MatchAny*)     const = 0;
    virtual T with(const MatchArray*)   const = 0;
    virtual T with(const MatchRegex*)   const = 0;
    virtual T with(const MatchRecord*)  const = 0;
    virtual T with(const MatchVariant*) const = 0;
  };

template <typename T>
  T switchOf(const PatternPtr& p, const switchPattern<T>& f) {
    switch (p->case_id()) {
    case MatchLiteral::type_case_id:
      return f.with(rcast<const MatchLiteral*>(p.get()));
    case MatchAny::type_case_id:
      return f.with(rcast<const MatchAny*>(p.get()));
    case MatchArray::type_case_id:
      return f.with(rcast<const MatchArray*>(p.get()));
    case MatchRegex::type_case_id:
      return f.with(rcast<const MatchRegex*>(p.get()));
    case MatchRecord::type_case_id:
      return f.with(rcast<const MatchRecord*>(p.get()));
    case MatchVariant::type_case_id:
      return f.with(rcast<const MatchVariant*>(p.get()));
    default:
      throw annotated_error(*p, "Internal error, cannot switch on unknown high-level pattern: " + show(p));
    }
  }

template <typename T>
  std::vector<T> switchOf(const Patterns& ps, const switchPattern<T>& f) {
    std::vector<T> r;
    for (auto p : ps) {
      r.push_back(switchOf(p, f));
    }
    return r;
  }

// construction utilities
inline MatchArray* mkpatarray(const std::vector<unsigned char>& bs, const LexicalAnnotation& la) {
  Patterns ps;
  for (auto b : bs) {
    ps.push_back(PatternPtr(new MatchLiteral(PrimitivePtr(new Byte(b, la)), la)));
  }
  return new MatchArray(ps, la);
}

inline MatchArray* mkpatarray(const std::string& cs, const LexicalAnnotation& la) {
  Patterns ps;
  for (auto c : cs) {
    ps.push_back(PatternPtr(new MatchLiteral(PrimitivePtr(new Char(c, la)), la)));
  }
  return new MatchArray(ps, la);
}

// is it possible for a pattern to be refuted?
bool refutable(const PatternPtr&);

// is a pattern a match on unit?
bool isUnitPat(const PatternPtr&);

// what variable names are introduced by a pattern (excluding _)?
str::set accessibleBindingNames(const PatternPtr&);

}

// make sure that pattern rows can be used as keys in hash maps
namespace std {
template <>
  struct hash< hobbes::PatternRow > {
    inline size_t operator()(const hobbes::PatternRow& x) const {
      return hobbes::hash(x);
    }
  };
}

#endif
