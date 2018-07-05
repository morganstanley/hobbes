
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

namespace hobbes {

Pattern::Pattern(int cid, const LexicalAnnotation& la) : LexicallyAnnotated(la), cid(cid) { }
int Pattern::case_id() const { return this->cid; }
const std::string& Pattern::name() const { return this->pname; }
void Pattern::name(const std::string& x) {
  this->pname = x;
  assignSubNames(x);
}

// match literal (constant) values
MatchLiteral::MatchLiteral(const PrimitivePtr& proxy, const LexicalAnnotation& la) : Base(la), p(proxy), e() {
}

MatchLiteral::MatchLiteral(const PrimitivePtr& proxy, const ExprPtr& value, const LexicalAnnotation& la) : Base(la), p(proxy), e(value) {
}

const ExprPtr&      MatchLiteral::expression()    const { return this->e; }
const PrimitivePtr& MatchLiteral::equivConstant() const { return this->p; }

void MatchLiteral::show(std::ostream& out) const {
  this->p->show(out);
}

bool MatchLiteral::operator==(const Pattern& rhs) const {
  if (const MatchLiteral* lrhs = is<MatchLiteral>(&rhs)) {
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
  if (const MatchAny* arhs = is<MatchAny>(&rhs)) {
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
  if (this->ps.size() > 0) {
    this->ps[0]->show(out);
    for (unsigned int i = 1; i < this->ps.size(); ++i) {
      out << ", ";
      this->ps[i]->show(out);
    }
  }
  out << "]";
}

bool MatchArray::operator==(const Pattern& rhs) const {
  const MatchArray* arhs = is<MatchArray>(&rhs);
  if (!arhs || this->ps.size() != arhs->ps.size()) return false;

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
  if (const MatchRegex* trhs = is<MatchRegex>(&rhs)) {
    return text() == trhs->text();
  }
  return false;
}

void MatchRegex::assignSubNames(const std::string&) {
}

PatternPtr MatchRegex::toRegex(const MatchArray& ma) {
  std::ostringstream ss;
  for (size_t i = 0; i < ma.size(); ++i) {
    if (const MatchLiteral* cm = is<MatchLiteral>(ma.pattern(i))) {
      if (const Char* c = is<Char>(cm->equivConstant())) {
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
  if (this->fs.size() > 0) {
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
  const MatchRecord* rrhs = hobbes::is<MatchRecord>(&rhs);
  if (!rrhs || this->fs.size() != rrhs->fs.size()) return false;

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
  if (const MatchVariant* vrhs = is<MatchVariant>(&rhs)) {
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
  if (ps.size() > 0) {
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
  for (auto f : setDifference(fnames, toSet(first(r->fields())))) {
    nfs.push_back(MatchRecord::Field(f, PatternPtr(new MatchAny(freshName(), r->la()))));
  }
  r->fields(nfs);
}

void normalizeRecPatterns(const Patterns& ps, const std::set<std::string>& fnames) {
  for (const auto& p : ps) {
    if (MatchRecord* r = is<MatchRecord>(p)) {
      normalizeRecPatterns(r, fnames);
    }
  }
}

typedef std::map<std::string, Patterns> NamedPatternGroups;

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
  if (ps.size() == 0) {
    // nothing to normalize
  } else if (is<MatchRecord>(ps[0])) {
    // this is actually the case we care about
    // first recursively normalize within each field's pattern
    auto gs = groupRecordPatterns(ps);
    for (const auto& g : gs) {
      normalizeRecAccess(g.second);
    }

    // then normalize within patterns
    normalizeRecPatterns(ps, keys(gs));
  } else if (is<MatchArray>(ps[0])) {
    normalizeRecAccess(concatArrayPatterns(ps));
  } else if (is<MatchVariant>(ps[0])) {
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
  } else if (is<MatchLiteral>(p) || is<MatchRegex>(p)) {
    // prim matches obviously can't contain record patterns
    return false;
  } else {
    return true;
  }
}

void normalizeRecAccess(const PatternRows& prs) {
  size_t maxc = (prs.size() == 0) ? 0 : prs[0].patterns.size();
  for (size_t c = 0; c < maxc; ++c) {
    // gather all patterns across this column, normalize record access within the column
    Patterns ps;

    for (size_t r = 0; r < prs.size(); ++r) {
      const PatternPtr& p = prs[r].patterns[c];

      // (we can trivially ignore this column if it matches a literal value)
      if (!canHoldRecPatterns(p)) {
        ps.clear();
        break;
      } else if (!is<MatchAny>(p)) {
        ps.push_back(p);
      }
    }

    if (ps.size() > 0) {
      normalizeRecAccess(ps);
    }
  }
}

// make sure that the table is "well formed" (all rows have the same number of columns)
void validateDimensions(size_t c, const PatternRows& ps, const LexicalAnnotation& la) {
  if (ps.size() == 0) {
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

// infer the types assumed by patterns
struct inferTypeF : public switchPattern<MonoTypePtr> {
  MonoTypeUnifier* s;
  inferTypeF(MonoTypeUnifier* s) : s(s) { }

  MonoTypePtr with(const MatchLiteral* v) const {
    return v->equivConstant()->primType();
  }

  MonoTypePtr with(const MatchAny* v) const {
    return freshTypeVar();
  }

  MonoTypePtr with(const MatchArray* v) const {
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

  MonoTypePtr with(const MatchRegex*) const {
    return arrayty(primty("char"));
  }

  MonoTypePtr with(const MatchRecord* v) const {
    Record::Members ms;
    for (size_t i = 0; i < v->size(); ++i) {
      const MatchRecord::Field& f = v->pattern(i);
      ms.push_back(Record::Member(f.first, switchOf(f.second, *this)));
    }
    return MonoTypePtr(Record::make(ms));
  }

  MonoTypePtr with(const MatchVariant* v) const {
    // too broad, but for this purpose should be fine
    return freshTypeVar();
  }
};

// assume dimensions have already been validated
MonoTypes matchRowType(const TEnvPtr& tenv, const PatternRows& ps) {
  MonoTypeUnifier u(tenv);

  // determine the initial row type
  MonoTypes ts;
  const Patterns& pr = ps[0].patterns;
  for (Patterns::const_iterator p = pr.begin(); p != pr.end(); ++p) {
    ts.push_back(switchOf(*p, inferTypeF(&u)));
  }

  // unify with subsequent rows
  for (unsigned int i = 1; i < ps.size(); ++i) {
    for (unsigned int c = 0; c < ps[i].patterns.size(); ++c) {
      mgu(ts[c], switchOf(ps[i].patterns[c], inferTypeF(&u)), &u);
    }
  }

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
  UnitV with(const MatchLiteral*) const { return unitv; }

  UnitV with(const MatchAny* x) const {
    if (x->value() != "_") {
      (*this->vm)[x->value()] = ExprPtr(new Var(x->name(), x->la()));
    }
    return unitv;
  }

  UnitV with(const MatchArray* x) const {
    for (auto i : x->indexes()) {
      switchOf(x->pattern(i), *this);
    }
    return unitv;
  }

  UnitV with(const MatchRegex* x) const {
    // regex translation will decide how to conflate binding names
    return unitv;
  }

  UnitV with(const MatchRecord* x) const {
    for (auto i : x->indexes()) {
      switchOf(x->pattern(i).second, *this);
    }
    return unitv;
  }

  UnitV with(const MatchVariant* x) const {
    switchOf(x->value(), *this);
    return unitv;
  }
};

VarMapping varMapping(const Patterns& ps) {
  // assuming that the input patterns have assigned names,
  // our renaming finds user-names in the input patterns and renames them to assigned names
  VarMapping r;
  for (auto p : ps) {
    switchOf(p, inferMappingF(&r));
  }
  return r;
}

void assignNames(const str::seq& vns, PatternRows& ps, bool* mappedVs) {
  // each row needs to be assigned the same names across columns
  for (size_t r = 0; r < ps.size(); ++r) {
    for (size_t c = 0; c < ps[r].patterns.size(); ++c) {
      ps[r].patterns[c]->name(vns[c]);
    }
  }

  // each row expression needs to be renamed consistent with the above name assignment
  for (size_t r = 0; r < ps.size(); ++r) {
    VarMapping vm = varMapping(ps[r].patterns);
    if (vm.size() > 0) {
      *mappedVs = true;

      if (ps[r].guard) {
        ps[r].guard = substitute(vm, ps[r].guard);
      }

      ps[r].result = substitute(vm, ps[r].result);
    }
  }
}

ExprPtr compileMatch(cc* c, const Exprs& es, const PatternRows& ps, const LexicalAnnotation& rootLA) {
  validate(c->typeEnv(), es.size(), ps, rootLA);

  // make variables to store each of the expressions being matched
  str::seq vns = varNames(es.size());

  // push these variable names through pattern rows (assign explicit names to every step of pattern matching)
  bool mappedVs = false;
  assignNames(vns, const_cast<PatternRows&>(ps), &mappedVs);

  // produce a match expression from the DFA induced by these patterns
  return inLetExp(vns, es, liftDFAExpr(c, ps, rootLA), rootLA);
}

ExprPtr compileMatchTest(cc* c, const ExprPtr& e, const PatternPtr& p, const LexicalAnnotation& rootLA) {
  auto anames = accessibleBindingNames(p);

  if (anames.size() > 0) {
    throw annotated_error(*p, "Inaccessible names in 'matches' test: " + str::cdelim(toVector(anames), ", "));
  } else if (!refutable(p)) {
    return ExprPtr(new Bool(true, rootLA));
  } else {
    return compileMatch(c, list(e), list(PatternRow(list(p), ExprPtr(new Bool(true, rootLA))), PatternRow(list(PatternPtr(new MatchAny("_", rootLA))), ExprPtr(new Bool(false, rootLA)))), rootLA);
  }
}

// a simple test to determine whether or not a pattern can possibly be refuted
struct refutableP : public switchPattern<bool> {
  bool with(const MatchLiteral* v) const { return sizeOf(v->equivConstant()->primType()) > 0; } // the only irrefutable literal is unit
  bool with(const MatchAny*      ) const { return false; }
  bool with(const MatchArray*    ) const { return true; }
  bool with(const MatchRegex*    ) const { return true; } // maybe too conservative?  /.*/ is not refutable actually, does it matter?
  bool with(const MatchVariant*  ) const { return true; }

  bool with(const MatchRecord* x) const {
    for (auto i : x->indexes()) {
      if (switchOf(x->pattern(i).second, *this)) {
        return true;
      }
    }
    return false;
  }
};

bool refutable(const PatternPtr& p) {
  return switchOf(p, refutableP());
}

bool isUnitPat(const PatternPtr& p) {
  if (const MatchLiteral* v = is<MatchLiteral>(p)) {
    return sizeOf(v->equivConstant()->primType()) == 0;
  } else {
    return false;
  }
}

struct accBindingNamesF : public switchPattern<UnitV> {
  str::set* r;
  accBindingNamesF(str::set* r) : r(r) { }

  UnitV with(const MatchLiteral*) const {
    return unitv;
  }

  UnitV with(const MatchAny* v) const {
    if (v->value().size() > 0 && v->value() != "_" && v->value()[0] != '.') {
      this->r->insert(v->value());
    }
    return unitv;
  }

  UnitV with(const MatchArray* ps) const {
    for (size_t i = 0; i < ps->size(); ++i) {
      switchOf(ps->pattern(i), *this);
    }
    return unitv;
  }

  UnitV with(const MatchRegex* v) const {
    auto vbs = bindingNames(v->value());
    this->r->insert(vbs.begin(), vbs.end());
    return unitv;
  }

  UnitV with(const MatchVariant* v) const {
    switchOf(v->value(), *this);
    return unitv;
  }

  UnitV with(const MatchRecord* ps) const {
    for (auto i : ps->indexes()) {
      switchOf(ps->pattern(i).second, *this);
    }
    return unitv;
  }
};

str::set accessibleBindingNames(const PatternPtr& p) {
  str::set r;
  switchOf(p, accBindingNamesF(&r));
  return r;
}

}

