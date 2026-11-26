
#include <hobbes/lang/expr.H>
#include <hobbes/lang/type.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/lang/pat/pattern.H>
#include <hobbes/lang/pat/dfa.H>
#include <hobbes/lang/pat/print.H>
#include <hobbes/util/array.H>
#include <hobbes/util/perf.H>
#include <hobbes/util/str.H>
#include <hobbes/eval/cc.H>
#include <fstream>
#include <unordered_map>

namespace hobbes {

Pattern::Pattern(int cid, const LexicalAnnotation& la) : LexicallyAnnotated(la), cid(cid) { }
int Pattern::case_id() const { return this->cid; }
const std::string& Pattern::name() const { return this->pname; }
void Pattern::name(const std::string& x) {
  this->pname = x;
  assignSubNames(x);
}

// match literal (constant) values
MatchLiteral::MatchLiteral(const PrimitivePtr& proxy, const LexicalAnnotation& la) : Base(la), p(proxy) {
}

MatchLiteral::MatchLiteral(const PrimitivePtr& proxy, const ExprPtr& value, const LexicalAnnotation& la) : Base(la), p(proxy), e(value) {
}

const ExprPtr&      MatchLiteral::expression()    const { return this->e; }
const PrimitivePtr& MatchLiteral::equivConstant() const { return this->p; }

void MatchLiteral::show(std::ostream& out) const {
  this->p->show(out);
}

bool MatchLiteral::operator==(const Pattern& rhs) const {
  if (const auto* lrhs = is<MatchLiteral>(&rhs)) {
    return *this->p == *lrhs->p;
  } else {
    return false;
  }
}

void MatchLiteral::assignSubNames(const std::string&) { }

// match any value, maybe bind it to a variable name
MatchAny::MatchAny(const std::string& vn, const LexicalAnnotation& la) : Base(la), vn(vn) { }
const std::string& MatchAny::value() const { return this->vn; }
void MatchAny::show(std::ostream& out) const { out << this->vn; }

bool MatchAny::operator==(const Pattern& rhs) const {
  if (const auto* arhs = is<MatchAny>(&rhs)) {
    return this->vn == arhs->vn;
  } else {
    return false;
  }
}

void MatchAny::assignSubNames(const std::string&) { }

// match an array of patterns
MatchArray::MatchArray(const Patterns& ps, const LexicalAnnotation& la) : Base(la), ps(ps), idxs(range<size_t>(0, ps.size())) {
}

const PatternPtr& MatchArray::pattern(size_t i) const {
  if (i < this->ps.size()) {
    return this->ps[i];
  } else {
    std::ostringstream ss;
    ss << "Index " << i << " out of bounds in pattern ";
    show(ss);

    throw annotated_error(*this, ss.str());
  }
}

const Idxs& MatchArray::indexes() const {
  return this->idxs;
}

size_t MatchArray::size() const {
  return this->idxs.size();
}

void MatchArray::indexes(const Idxs& idxs) {
  this->idxs = idxs;
}

void MatchArray::show(std::ostream& out) const {
  out << "[";
  if (!this->ps.empty()) {
    this->ps[0]->show(out);
    for (unsigned int i = 1; i < this->ps.size(); ++i) {
      out << ", ";
      this->ps[i]->show(out);
    }
  }
  out << "]";
}

bool MatchArray::operator==(const Pattern& rhs) const {
  const auto* arhs = is<MatchArray>(&rhs);
  if ((arhs == nullptr) || this->ps.size() != arhs->ps.size()) return false;

  for (unsigned int i = 0; i < this->ps.size(); ++i) {
    if (!(*this->ps[i] == *arhs->ps[i])) {
      return false;
    }
  }
  return true;
}

void MatchArray::assignSubNames(const std::string& x) {
  // array size check will be .0
  for (size_t i = 0; i < this->ps.size(); ++i) {
    this->ps[i]->name(x + "." + str::from(i + 1));
  }
}

// match a regular expression
MatchRegex::MatchRegex(const RegexPtr& regex, const LexicalAnnotation& la) : Base(la), regex(regex) {
}

MatchRegex::MatchRegex(const std::string& regex, const LexicalAnnotation& la) : MatchRegex(parseRegex(regex), la) {
}

std::string MatchRegex::text() const {
  std::ostringstream ss;
  this->regex->show(ss);
  return ss.str();
}

const RegexPtr& MatchRegex::value() const {
  return this->regex;
}

void MatchRegex::show(std::ostream& out) const {
  out << "/" << text() << "/";
}

bool MatchRegex::operator==(const Pattern& rhs) const {
  if (const auto* trhs = is<MatchRegex>(&rhs)) {
    return text() == trhs->text();
  }
  return false;
}

void MatchRegex::assignSubNames(const std::string&) {
}

// a literal character that the regex parser would read as an operator
// (diffRegex/returnR in regex.C: grouping, alternation, any-char, charsets,
// quantifiers, and the escape itself) has to be escaped when a string literal
// is spelled as a regex, or "a*" stops matching itself and matches "aaa"
static bool isRegexMetaChar(char c) {
  switch (c) {
  case '\\': case '(': case ')': case '|': case '.': case '[': case ']':
  case '?': case '*': case '+':
    return true;
  default:
    return false;
  }
}

PatternPtr MatchRegex::toRegex(const MatchArray& ma) {
  std::ostringstream ss;
  for (size_t i = 0; i < ma.size(); ++i) {
    if (const MatchLiteral* cm = is<MatchLiteral>(ma.pattern(i))) {
      if (const Char* c = is<Char>(cm->equivConstant())) {
        if (isRegexMetaChar(c->value())) {
          ss << '\\';
        }
        ss << c->value();
      } else {
        throw annotated_error(*ma.pattern(i), "Internal error, can't normalize non-char array match to regex");
      }
//    } else if (const MatchAny* var = is<MatchAny>(ma.pattern(i))) {
//      ss << "(?<" << var->name() << ">.)";                             <---- not quite right, would change capture var from char to [char]
    } else {
      throw annotated_error(ma, "Can't translate match pattern to regex");
    }
  }

  PatternPtr result(new MatchRegex(ss.str(), ma.la()));
  result->name(ma.name());
  return result;
}

// match a record of patterns
MatchRecord::MatchRecord(const Fields& fs, const LexicalAnnotation& la) : Base(la) {
  fields(fs);
}

const MatchRecord::Field& MatchRecord::pattern(size_t i) const {
  if (i < this->fs.size()) {
    return this->fs[i];
  } else {
    std::ostringstream ss;
    ss << "Index " << i << " out of bounds in pattern ";
    show(ss);

    throw annotated_error(*this, ss.str());
  }
}

const Idxs& MatchRecord::indexes() const {
  return this->is;
}

size_t MatchRecord::size() const {
  return this->fs.size();
}

const MatchRecord::Fields& MatchRecord::fields() const {
  return this->fs;
}

void MatchRecord::fields(const Fields& fs) {
  this->fs = fs;
  this->is = range<size_t>(0, fs.size());
  std::sort(this->fs.begin(), this->fs.end(), [](const Field& f0, const Field& f1) { return f0.first < f1.first; });
}

void MatchRecord::show(std::ostream& out) const {
  out << "{";
  if (!this->fs.empty()) {
    show(out, this->fs[0]);
    for (unsigned int i = 1; i < this->fs.size(); ++i) {
      out << ", ";
      show(out, this->fs[i]);
    }
  }
  out << "}";
}

void MatchRecord::show(std::ostream& out, const Field& f) {
  out << f.first << " = "; f.second->show(out);
}

bool MatchRecord::operator==(const Pattern& rhs) const {
  const auto* rrhs = hobbes::is<MatchRecord>(&rhs);
  if ((rrhs == nullptr) || this->fs.size() != rrhs->fs.size()) return false;

  for (unsigned int i = 0; i < this->fs.size(); ++i) {
    if (this->fs[i].first != rrhs->fs[i].first) {
      return false;
    } else if (!(*this->fs[i].second == *rrhs->fs[i].second)) {
      return false;
    }
  }
  return true;
}

void MatchRecord::assignSubNames(const std::string& x) {
  for (auto& f : this->fs) {
    f.second->name(x + "." + f.first);
  }
}

// match a variant
MatchVariant::MatchVariant(const std::string& lbl, const PatternPtr& p, const LexicalAnnotation& la) : Base(la), lbl(lbl), p(p) {
}

const std::string& MatchVariant::label() const { return this->lbl; }
const PatternPtr&  MatchVariant::value() const { return this->p; }

void MatchVariant::show(std::ostream& out) const {
  out << "|" << this->lbl << "="; this->p->show(out); out << "|";
}

bool MatchVariant::operator==(const Pattern& rhs) const {
  if (const auto* vrhs = is<MatchVariant>(&rhs)) {
    return this->lbl == vrhs->lbl && *this->p == *vrhs->p;
  } else {
    return false;
  }
}

void MatchVariant::assignSubNames(const std::string& x) {
  // assume that the variant constructor will be .0
  this->p->name(x + ".1");
}

// hash match table rows for efficient lookup
bool operator==(const PatternRow& pr0, const PatternRow& pr1) {
  if (pr0.guard.get() == pr1.guard.get() && pr0.result.get() == pr1.result.get() && pr0.patterns.size() == pr1.patterns.size()) {
    for (size_t i = 0; i < pr0.patterns.size(); ++i) {
      if (!(pr0.patterns[i].get() == pr1.patterns[i].get() || *pr0.patterns[i] == *pr1.patterns[i])) {
        return false;
      }
    }
    return true;
  } else {
    return false;
  }
}

size_t hash(const PatternRow& pr) {
  size_t r = 0;
  hashAppend(r, reinterpret_cast<void*>(pr.result.get()));
  hashAppend(r, pr.patterns.size());
  return r;
}

// pattern utilities
std::string show(const PatternPtr& p) {
  std::ostringstream ss;
  p->show(ss);
  return ss.str();
}

std::string show(const Patterns& ps) {
  std::ostringstream ss;
  if (!ps.empty()) {
    ps[0]->show(ss);
    for (size_t i = 1; i < ps.size(); ++i) {
      ss << " ";
      ps[i]->show(ss);
    }
  }
  return ss.str();
}

std::string show(const PatternRow& pr) {
  if (pr.guard) {
    return show(pr.patterns) + " where " + show(pr.guard) + " -> " + show(pr.result);
  } else {
    return show(pr.patterns) + " -> " + show(pr.result);
  }
}

// allow incomplete sets of field patterns in record cells by expanding all such cases to 'match any' patterns for unnamed fields
void normalizeRecPatterns(MatchRecord* r, const std::set<std::string>& fnames) {
  MatchRecord::Fields nfs = r->fields();
  for (const auto& f : setDifference(fnames, toSet(first(r->fields())))) {
    nfs.push_back(MatchRecord::Field(f, PatternPtr(new MatchAny(freshName(), r->la()))));
  }
  r->fields(nfs);
}

void normalizeRecPatterns(const Patterns& ps, const std::set<std::string>& fnames) {
  for (const auto& p : ps) {
    if (auto* r = is<MatchRecord>(p)) {
      normalizeRecPatterns(r, fnames);
    }
  }
}

using NamedPatternGroups = std::map<std::string, Patterns>;

NamedPatternGroups groupRecordPatterns(const Patterns& ps) {
  NamedPatternGroups r;
  for (const auto& p : ps) {
    if (const MatchRecord* rec = is<MatchRecord>(p)) {
      for (size_t f = 0; f < rec->size(); ++f) {
        const auto& fd = rec->pattern(f);
        r[fd.first].push_back(fd.second);
      }
    }
  }
  return r;
}

NamedPatternGroups groupVariantPatterns(const Patterns& ps) {
  NamedPatternGroups r;
  for (const auto& p : ps) {
    if (const MatchVariant* v = is<MatchVariant>(p)) {
      r[v->label()].push_back(v->value());
    }
  }
  return r;
}

Patterns concatArrayPatterns(const Patterns& ps) {
  Patterns r;
  for (const auto& p : ps) {
    if (const MatchArray* a = is<MatchArray>(p)) {
      for (size_t i = 0; i < a->size(); ++i) {
        r.push_back(a->pattern(i));
      }
    }
  }
  return r;
}

void normalizeRecAccess(const Patterns& ps) {
  // a wildcard can stand beside any kind of pattern and has nothing to normalize,
  // so the first pattern that isn't one decides what has to be normalized here
  // (deciding from a leading wildcard left the partial records after it unpadded)
  size_t k = 0;
  while (k < ps.size() && is<MatchAny>(ps[k]) != nullptr) {
    ++k;
  }

  if (k == ps.size()) {
    // nothing to normalize
  } else if (is<MatchRecord>(ps[k]) != nullptr) {
    // this is actually the case we care about
    // first recursively normalize within each field's pattern
    auto gs = groupRecordPatterns(ps);
    for (const auto& g : gs) {
      normalizeRecAccess(g.second);
    }

    // then normalize within patterns
    normalizeRecPatterns(ps, keys(gs));
  } else if (is<MatchArray>(ps[k]) != nullptr) {
    normalizeRecAccess(concatArrayPatterns(ps));
  } else if (is<MatchVariant>(ps[k]) != nullptr) {
    for (const auto& g : groupVariantPatterns(ps)) {
      normalizeRecAccess(g.second);
    }
  } else {
    // no other cases matter
  }
}

bool canHoldRecPatterns(const PatternPtr& p) {
  if (const MatchArray* a = is<MatchArray>(p)) {
    for (size_t i = 0; i < a->size(); ++i) {
      if (!canHoldRecPatterns(a->pattern(i))) {
        return false;
      }
    }
    return true;
  } else if ((is<MatchLiteral>(p) != nullptr) || (is<MatchRegex>(p) != nullptr)) {
    // prim matches obviously can't contain record patterns
    return false;
  } else {
    return true;
  }
}

void normalizeRecAccess(const PatternRows& prs) {
  size_t maxc = (prs.empty()) ? 0 : prs[0].patterns.size();
  for (size_t c = 0; c < maxc; ++c) {
    // gather all patterns across this column, normalize record access within the column
    Patterns ps;

    for (const auto &pr : prs) {
      const PatternPtr& p = pr.patterns[c];

      // (we can trivially ignore this column if it matches a literal value)
      if (!canHoldRecPatterns(p)) {
        ps.clear();
        break;
      } else if (is<MatchAny>(p) == nullptr) {
        ps.push_back(p);
      }
    }

    if (!ps.empty()) {
      normalizeRecAccess(ps);
    }
  }
}

// make sure that the table is "well formed" (all rows have the same number of columns)
void validateDimensions(size_t c, const PatternRows& ps, const LexicalAnnotation& la) {
  if (ps.empty()) {
    throw annotated_error(la, "Internal error, pattern sequence cannot be empty");
  } else {
    for (unsigned int i = 0; i < ps.size(); ++i) {
      if (ps[i].patterns.size() != c) {
        throw annotated_error(
          la,
          "Invalid pattern match sequence, row #" + str::from(i) + " has " + str::from(ps[i].patterns.size()) + " columns, "
          "but should have " + str::from(c)
        );
      }
    }
  }
}

// a variant pattern names just one constructor of the type that it matches, which isn't enough to say what that type is
// so we infer a type variable for it and remember what the pattern assumes about that type
struct VariantPatType {
  const MatchVariant* pattern;
  MonoTypePtr         ty;      // the variant type matched (a type variable, unless patterns disagree)
  MonoTypePtr         payload; // the type that the pattern assumes for its constructor in that type
};

// the variant patterns of a match, grouped by how many variant patterns each one is nested inside of
struct VariantPatTypes {
  std::vector<std::vector<VariantPatType>> byDepth;
  size_t depth = 0;
};

// infer the types assumed by patterns
struct inferTypeF : public switchPattern<MonoTypePtr> {
  MonoTypeUnifier* s;
  VariantPatTypes* vpts;
  inferTypeF(MonoTypeUnifier* s, VariantPatTypes* vpts) : s(s), vpts(vpts) { }

  MonoTypePtr with(const MatchLiteral* v) const override {
    return v->equivConstant()->primType();
  }

  MonoTypePtr with(const MatchAny*) const override {
    return freshTypeVar();
  }

  MonoTypePtr with(const MatchArray* v) const override {
    if (v->size() == 0) {
      return arrayty(freshTypeVar());
    } else {
      MonoTypePtr elemTy = switchOf(v->pattern(0), *this);
      for (size_t i = 1; i < v->size(); ++i) {
        mgu(elemTy, switchOf(v->pattern(i), *this), this->s);
      }
      return arrayty(elemTy);
    }
  }

  MonoTypePtr with(const MatchRegex*) const override {
    return arrayty(primty("char"));
  }

  MonoTypePtr with(const MatchRecord* v) const override {
    Record::Members ms;
    for (size_t i = 0; i < v->size(); ++i) {
      const MatchRecord::Field& f = v->pattern(i);
      ms.push_back(Record::Member(f.first, switchOf(f.second, *this)));
    }
    return MonoTypePtr(Record::make(ms));
  }

  MonoTypePtr with(const MatchVariant* v) const override {
    // too broad on its own, 'unifyVariantPatTypes' relates this to the other patterns on the same type
    MonoTypePtr ty = freshTypeVar();

    const size_t d = this->vpts->depth;
    if (this->vpts->byDepth.size() <= d) {
      this->vpts->byDepth.resize(d + 1);
    }

    ++this->vpts->depth;
    MonoTypePtr payload = switchOf(v->value(), *this);
    --this->vpts->depth;

    this->vpts->byDepth[d].push_back(VariantPatType{v, ty, payload});
    return ty;
  }
};

// variant patterns are inferred as unrelated type variables, so unification alone can't tell that
//   * a variant pattern is matched against a type that some other pattern needs to be a primitive, array or record, or
//   * patterns for the same constructor of the same type disagree on the type of its payload
// (the payloads of a constructor are deconstructed as one column, which has to be as consistent as any other:
//  the match compiler indexes every row of that column by the fields of the first)
//
// The type variable of a variant pattern can only be identified with another's by unifying the columns or the
// payloads that hold them, and a payload holds only patterns nested one level deeper. So once the patterns
// nested less deeply have been unified, the types of the patterns at a level are settled, and one pass over each
// level from the outside in visits every variant pattern exactly once.
void unifyVariantPatTypes(const VariantPatTypes& vpts, MonoTypeUnifier* u) {
  for (const auto& level : vpts.byDepth) {
    std::map<std::pair<std::string, std::string>, MonoTypePtr> ctorTys;
    for (const auto& vpt : level) {
      MonoTypePtr ty = u->substitute(vpt.ty);
      if (const TVar* tv = is<TVar>(ty)) {
        auto ct = ctorTys.insert(std::make_pair(std::make_pair(tv->name(), vpt.pattern->label()), vpt.payload));
        if (!ct.second) {
          mgu(ct.first->second, vpt.payload, u);
        }
      } else {
        throw annotated_error(*vpt.pattern, "Cannot match a variant pattern against the type " + show(ty));
      }
    }
  }
}

// assume dimensions have already been validated
MonoTypes matchRowType(const TEnvPtr& tenv, const PatternRows& ps) {
  MonoTypeUnifier u(tenv);
  VariantPatTypes vpts;

  // determine the initial row type
  MonoTypes ts;
  const Patterns& pr = ps[0].patterns;
  for (const auto &p : pr) {
    ts.push_back(switchOf(p, inferTypeF(&u, &vpts)));
  }

  // unify with subsequent rows
  for (unsigned int i = 1; i < ps.size(); ++i) {
    for (unsigned int c = 0; c < ps[i].patterns.size(); ++c) {
      mgu(ts[c], switchOf(ps[i].patterns[c], inferTypeF(&u, &vpts)), &u);
    }
  }

  // and unify within variant patterns
  unifyVariantPatTypes(vpts, &u);

  return u.substitute(ts);
}

void validate(const TEnvPtr& tenv, size_t c, const PatternRows& ps, const LexicalAnnotation& la) {
  // the rows to match should be rectangular (so that we're matching against the same number of values)
  // implied types should agree at all columns
  // no row should make following rows unreachable
  // all rows together should exhaustively cover the input space
  validateDimensions(c, ps, la);
  normalizeRecAccess(ps);
  matchRowType(tenv, ps);
}

str::seq varNames(unsigned int n) {
  str::seq r;
  for (unsigned int i = 0; i < n; ++i) {
    r.push_back(freshName() + ".rv" + str::from(i));
  }
  return r;
}

ExprPtr inLetExp(const str::seq& vns, const Exprs& es, const ExprPtr& b, const LexicalAnnotation& la) {
  if (vns.size() != es.size()) {
    throw annotated_error(la, "Internal error in let exp constructions");
  } else {
    ExprPtr r = b;
    for (unsigned int i = vns.size(); i > 0; --i) {
      r = ExprPtr(new Let(vns[i-1], es[i-1], r, la));
    }
    return r;
  }
}

struct inferMappingF : public switchPattern<UnitV> {
  VarMapping* vm;
  inferMappingF(VarMapping* vm) : vm(vm) { }
  UnitV with(const MatchLiteral*) const override { return unitv; }

  UnitV with(const MatchAny* x) const override {
    if (x->value() != "_") {
      (*this->vm)[x->value()] = ExprPtr(new Var(x->name(), x->la()));
    }
    return unitv;
  }

  UnitV with(const MatchArray* x) const override {
    for (auto i : x->indexes()) {
      switchOf(x->pattern(i), *this);
    }
    return unitv;
  }

  UnitV with(const MatchRegex*) const override {
    // regex translation will decide how to conflate binding names
    return unitv;
  }

  UnitV with(const MatchRecord* x) const override {
    for (auto i : x->indexes()) {
      switchOf(x->pattern(i).second, *this);
    }
    return unitv;
  }

  UnitV with(const MatchVariant* x) const override {
    switchOf(x->value(), *this);
    return unitv;
  }
};

VarMapping varMapping(const Patterns& ps) {
  // assuming that the input patterns have assigned names,
  // our renaming finds user-names in the input patterns and renames them to assigned names
  VarMapping r;
  for (const auto& p : ps) {
    switchOf(p, inferMappingF(&r));
  }
  return r;
}

void assignNames(const str::seq& vns, PatternRows& ps, bool* mappedVs) {
  // each row needs to be assigned the same names across columns
  for (auto &p : ps) {
    for (size_t c = 0; c < p.patterns.size(); ++c) {
      p.patterns[c]->name(vns[c]);
    }
  }

  // each row expression needs to be renamed consistent with the above name assignment
  for (auto &p : ps) {
    VarMapping vm = varMapping(p.patterns);
    if (!vm.empty()) {
      *mappedVs = true;

      if (p.guard) {
        p.guard = substitute(vm, p.guard);
      }

      p.result = substitute(vm, p.result);
    }
  }
}

// The match compiler walks patterns by recursion, one stack frame (or
// several) per level of nesting: validation, naming, and every DFA state's
// choice of pivot column (scorePatternF) all recur through each pattern still
// in its table. The parser cannot hand it a pattern nested more than ~200
// deep before its own stack is exhausted, but a pattern built through the
// C++ API (compileMatch takes PatternRows as given) has no such bound, and on
// a release x86-64 build one level costs ~740 bytes for a record pattern,
// ~1KB for a variant and ~1.7KB for an array: an 8MB stack overflowed at
// ~4,900 nested arrays and ~11,000 nested records, a 512KB one at 311 and
// 696, and the DFA depth bound in dfa.C was never reached (each of those
// levels costs one DFA state as well, so the states were within budget when
// the walk ran the stack out).
//
// So nesting is measured without recursion before anything walks it. The
// bound sits well above the parser's own ceiling, so no pattern that parses
// is turned away, and what it allows costs a few hundred KB of stack at most
// even on top of the DFA's own recursion (see maxMatchDFADepth in dfa.C).
const size_t maxPatternNestingDepth = 256;

struct subPatternsF : public switchPattern<Patterns> {
  Patterns with(const MatchLiteral*) const override { return Patterns(); }
  Patterns with(const MatchAny*)     const override { return Patterns(); }
  Patterns with(const MatchRegex*)   const override { return Patterns(); }
  Patterns with(const MatchVariant* v) const override { return list(v->value()); }

  Patterns with(const MatchArray* v) const override {
    Patterns r;
    for (size_t i = 0; i < v->size(); ++i) {
      r.push_back(v->pattern(i));
    }
    return r;
  }

  Patterns with(const MatchRecord* v) const override {
    Patterns r;
    for (const auto& f : v->fields()) {
      r.push_back(f.second);
    }
    return r;
  }
};

// measure a pattern's nesting into *depth, or return false if the pattern
// reaches itself and so has none
static bool measurePatternNesting(const PatternPtr& root, size_t* depth) {
  *depth = 0;
  if (!root) {
    return true;
  }

  // a pattern can reach the same sub-pattern by more than one path, so depths
  // are memoized -- without that, a tree of shared sub-patterns takes
  // exponential time to measure.
  //
  // What is memoized is a finished depth, and only that is ever read. A
  // sub-pattern waiting on the stack is not finished: if a second parent left
  // it there for the first to get to, that parent would be measured before it
  // was (and one level short for each time that happened on the way up). So a
  // parent pushes every child not yet finished, whoever else has pushed it,
  // and a frame whose pattern was finished through another parent in the
  // meantime is dropped. Each pattern is still expanded only once, so this
  // pushes at most one frame per edge.
  //
  // A pattern is entered here at 0, which no finished pattern measures, when
  // its children are pushed. The patterns in that state are exactly the ones
  // on the path down to the frame on top, so a child found in it is its own
  // ancestor. MatchRecord::fields can make such a pattern, and nothing that
  // walks one would ever finish.
  const size_t inProgress = 0;
  std::unordered_map<const Pattern*, size_t> depths;

  using Frame = std::pair<PatternPtr, bool>; // the pattern, and whether its children are on the stack
  std::vector<Frame> stack;
  stack.push_back(Frame(root, false));

  while (!stack.empty()) {
    const Pattern* top = stack.back().first.get();

    if (!stack.back().second) {
      if (depths.find(top) != depths.end()) {
        // finished through another parent since this was pushed
        stack.pop_back();
        continue;
      }
      stack.back().second = true;
      depths[top] = inProgress;

      const Patterns cs = switchOf(stack.back().first, subPatternsF());
      for (const auto& c : cs) {
        if (!c) {
          continue;
        }
        const auto d = depths.find(c.get());
        if (d == depths.end()) {
          stack.push_back(Frame(c, false));
        } else if (d->second == inProgress) {
          return false;
        }
      }
    } else {
      // every child was finished or pushed above this frame, and everything
      // above it has since been popped, so every child is finished now
      const PatternPtr p = stack.back().first;
      stack.pop_back();

      size_t d = 1;
      for (const auto& c : switchOf(p, subPatternsF())) {
        if (c) {
          d = std::max(d, depths.at(c.get()) + 1);
        }
      }
      depths[p.get()] = d;
    }
  }

  *depth = depths.at(root.get());
  return true;
}

static const char* const cyclicPatternMsg = "match pattern contains itself as a sub-pattern";

size_t patternNestingDepth(const PatternPtr& p) {
  size_t d = 0;
  if (!measurePatternNesting(p, &d)) {
    throw std::runtime_error(cyclicPatternMsg);
  }
  return d;
}

void checkPatternNestingDepth(const PatternPtr& p, const LexicalAnnotation& la) {
  size_t d = 0;
  if (!measurePatternNesting(p, &d)) {
    throw annotated_error(la, cyclicPatternMsg);
  }
  if (d > maxPatternNestingDepth) {
    throw annotated_error(la, "match pattern is too complex to compile (nests more than " + str::from(maxPatternNestingDepth) + " levels deep)");
  }
}

ExprPtr compileMatch(cc* c, const Exprs& es, const PatternRows& ps, const LexicalAnnotation& rootLA) {
  // everything handed in is walked below (each row's result and guard are
  // renamed by substitution, for one), and every walk recurs through nesting.
  // This is run from grammar actions, on sub-expressions of a parse still under
  // way that the parser's nesting bound has not yet had a chance to see:
  // OSS-Fuzz 556791547 is a `\x.` whose body is a chain of 11,000 comparisons,
  // and the substitution overflowed the stack before the parse finished. So
  // the bound is applied here, before any of it is walked.
  for (const auto& e : es) {
    checkNestingDepth(e);
  }
  for (const auto& p : ps) {
    checkNestingDepth(p.guard);
    checkNestingDepth(p.result);
    for (const auto& pat : p.patterns) {
      checkPatternNestingDepth(pat, rootLA);
    }
  }

  validate(c->typeEnv(), es.size(), ps, rootLA);

  // make variables to store each of the expressions being matched
  str::seq vns = varNames(es.size());

  // push these variable names through pattern rows (assign explicit names to every step of pattern matching)
  bool mappedVs = false;
  assignNames(vns, const_cast<PatternRows&>(ps), &mappedVs);

  // produce a match expression from the DFA induced by these patterns
  ExprPtr r = inLetExp(vns, es, liftDFAExpr(c, ps, rootLA), rootLA);

  // what comes out can nest far more deeply than what went in: each variable a
  // pattern binds is a 'let' around the rest of the row's result, so a tuple
  // pattern binding 80,000 names is a chain of 80,000 nested lets. The grammar
  // actions that run this copy the result (clone(), one frame per level) before
  // the parser's own bound sees it, and that overflowed the stack. So the bound
  // is applied to the result too, before it is handed back; a result past it is
  // released iteratively as the exception unwinds.
  checkNestingDepth(r);
  return r;
}

ExprPtr compileMatchTest(cc* c, const ExprPtr& e, const PatternPtr& p, const LexicalAnnotation& rootLA) {
  auto anames = accessibleBindingNames(p);

  if (!anames.empty()) {
    throw annotated_error(*p, "Inaccessible names in 'matches' test: " + str::cdelim(toVector(anames), ", "));
  } else if (!refutable(p)) {
    return ExprPtr(new Bool(true, rootLA));
  } else {
    return compileMatch(c, list(e), list(PatternRow(list(p), ExprPtr(new Bool(true, rootLA))), PatternRow(list(PatternPtr(new MatchAny("_", rootLA))), ExprPtr(new Bool(false, rootLA)))), rootLA);
  }
}

ExprPtr compileRegexFn(cc* c, const std::string& regex, const LexicalAnnotation& rootLA) {
  PatternPtr p(new MatchRegex(regex, rootLA));

  auto ns = accessibleBindingNames(p);
  if (ns.empty()) {
    return fn("x", compileMatchTest(c, var("x", rootLA), p, rootLA), rootLA);
  } else {
    MkRecord::FieldDefs fs;
    for (const auto& n : ns) {
      fs.push_back(MkRecord::FieldDef(n, var(n, rootLA)));
    }
    return fn("x", compileMatch(c, list(var("x", rootLA)), list(PatternRow(list(p), fncall(var("just", rootLA), mkrecord(fs, rootLA), rootLA)), PatternRow(list(PatternPtr(new MatchAny("_", rootLA))), var("nothing", rootLA))), rootLA), rootLA);
  }
}

// a simple test to determine whether or not a pattern can possibly be refuted
//
// This and accessibleBindingNames below run from grammar actions before the
// match compiler's nesting bound (checkPatternNestingDepth) is applied, so
// they walk with an explicit stack rather than by recursion.
struct refutableP : public switchPattern<bool> {
  // a record is refuted if any of its fields can be, so its fields are
  // queued here for the caller to test rather than tested by recursion
  Patterns* pending;
  explicit refutableP(Patterns* pending) : pending(pending) { }

  bool with(const MatchLiteral* v) const override { return sizeOf(v->equivConstant()->primType()) > 0; } // the only irrefutable literal is unit
  bool with(const MatchAny*      ) const override { return false; }
  bool with(const MatchArray*    ) const override { return true; }
  bool with(const MatchRegex*    ) const override { return true; } // maybe too conservative?  /.*/ is not refutable actually, does it matter?
  bool with(const MatchVariant*  ) const override { return true; }

  bool with(const MatchRecord* x) const override {
    for (auto i : x->indexes()) {
      this->pending->push_back(x->pattern(i).second);
    }
    return false;
  }
};

bool refutable(const PatternPtr& p) {
  Patterns pending;
  refutableP f(&pending);
  pending.push_back(p);
  while (!pending.empty()) {
    const PatternPtr q = pending.back();
    pending.pop_back();
    if (switchOf(q, f)) {
      return true;
    }
  }
  return false;
}

bool isUnitPat(const PatternPtr& p) {
  if (const MatchLiteral* v = is<MatchLiteral>(p)) {
    return sizeOf(v->equivConstant()->primType()) == 0;
  } else {
    return false;
  }
}

struct accBindingNamesF : public switchPattern<UnitV> {
  // sub-patterns are queued here for the caller to visit, not visited by recursion
  str::set* r;
  Patterns* pending;
  accBindingNamesF(str::set* r, Patterns* pending) : r(r), pending(pending) { }

  UnitV with(const MatchLiteral*) const override {
    return unitv;
  }

  UnitV with(const MatchAny* v) const override {
    if (!v->value().empty() && v->value() != "_" && v->value()[0] != '.') {
      this->r->insert(v->value());
    }
    return unitv;
  }

  UnitV with(const MatchArray* ps) const override {
    for (size_t i = 0; i < ps->size(); ++i) {
      this->pending->push_back(ps->pattern(i));
    }
    return unitv;
  }

  UnitV with(const MatchRegex* v) const override {
    auto vbs = bindingNames(v->value());
    this->r->insert(vbs.begin(), vbs.end());
    return unitv;
  }

  UnitV with(const MatchVariant* v) const override {
    this->pending->push_back(v->value());
    return unitv;
  }

  UnitV with(const MatchRecord* ps) const override {
    for (auto i : ps->indexes()) {
      this->pending->push_back(ps->pattern(i).second);
    }
    return unitv;
  }
};

str::set accessibleBindingNames(const PatternPtr& p) {
  str::set r;
  Patterns pending;
  accBindingNamesF f(&r, &pending);
  pending.push_back(p);
  while (!pending.empty()) {
    const PatternPtr q = pending.back();
    pending.pop_back();
    switchOf(q, f);
  }
  return r;
}

// desugar list comprehensions
ExprPtr irpatFunc(cc* c, const PatternPtr& pat, const ExprPtr& body, const LexicalAnnotation& la) {
  return fn(str::strings(".arg"), compileMatch(c, list(var(".arg", la)), list(PatternRow(list(pat), body)), la), la);
}

ExprPtr rpatFunc(cc* c, const PatternPtr& pat, const ExprPtr& cond, const ExprPtr& body, const LexicalAnnotation& la) {
  MonoTypePtr rty   = freshTypeVar();
  MonoTypePtr mrty  = sumtype(primty("unit"), rty);
  ExprPtr     fbody = assume(ExprPtr(new MkVariant(".f0", mktunit(la), la)), mrty, la);
  ExprPtr     sbody = assume(ExprPtr(new MkVariant(".f1", body, la)), mrty, la);
  PatternRow  pr    = cond ? PatternRow(list(pat), cond, sbody) : PatternRow(list(pat), sbody);

  return fn(str::strings(".arg"), compileMatch(c, list(var(".arg", la)), list(pr, PatternRow(list(PatternPtr(new MatchAny("_", la))), fbody)), la), la);
}

ExprPtr conjoinConds(const Exprs& es, const LexicalAnnotation& la) {
  if (es.empty()) {
    return constant(bool(true), la);
  } else {
    ExprPtr c = es[0];
    for (size_t i = 1; i < es.size(); ++i) {
      c = fncall(var("and", la), list(c, es[i]), la);
    }
    return c;
  }
}

ExprPtr desugarComprehension(cc* c, const ExprPtr& e, const CSelection& cs, const LexicalAnnotation& la) {
  if (refutable(cs.pat)) {
    return fncall(var("ffilterMMap", la), list(rpatFunc(c, cs.pat, !cs.conds.empty() ? conjoinConds(cs.conds, la) : ExprPtr(), e, la), cs.seq), la);
  } else if (!cs.conds.empty()) {
    return fncall(var("ffilterMap", la), list(irpatFunc(c, cs.pat, conjoinConds(cs.conds, la), la), irpatFunc(c, cs.pat, e, la), cs.seq), la);
  } else {
    return fncall(var("fmap", la), list(irpatFunc(c, cs.pat, e, la), cs.seq), la);
  }
}

// the grammar action runs this mid-parse and takes a copy of the result, which
// recurs once per level, so the result is bounded first: each selection's
// source ('xs' in 'x <- xs') goes into it as written, without passing through
// the match compiler's bound, and '[1 | x <- 1+1+...]' with 50,000 terms
// overflowed the stack in that copy
Expr* desugarComprehension(cc* c, const ExprPtr& ex, const CSelections& cs, const LexicalAnnotation& la) {
  ExprPtr e = ex;
  if (!cs.empty()) {
    e = desugarComprehension(c, ex, *cs[cs.size()-1], la);
    for (size_t i = cs.size()-1; i > 0; --i) {
      e = fncall(var("mflatten", la), list(desugarComprehension(c, e, *cs[i-1], la)), la);
    }
  }
  checkNestingDepth(e);
  return e->clone();
}

}

