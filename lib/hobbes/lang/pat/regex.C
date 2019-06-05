
#include <hobbes/eval/cc.H>
#include <hobbes/lang/pat/regex.H>
#include <hobbes/util/array.H>
#include <hobbes/util/str.H>
#include <hobbes/util/rmap.H>

namespace hobbes {

/******************
 * parse regular expressions into an AST
 ******************/

typedef uint8_t rchar_t;

// epsilon / the empty string
struct REps : public Regex {
  void show(std::ostream& out) const { out << "`"; }
};

// a range of chars [b,e] (inclusive)
struct RCharRange : public Regex {
  rchar_t b, e;
  RCharRange(rchar_t b, rchar_t e) : b(b), e(e) { }

  void show(std::ostream& out) const {
    if (this->b == this->e) {
      out << this->b;
    } else if (this->b == rchar_t(0) && this->e == rchar_t(255)) {
      out << ".";
    } else {
      out << this->b << "-" << this->e;
    }
  }
};

// E* (zero or more instances of the regex E)
struct RStar : public Regex {
  RegexPtr v;
  RStar(const RegexPtr& v) : v(v) { }

  void show(std::ostream& out) const { this->v->show(out); out << "*"; }
};

// A|B (either match A, or match B)
struct REither : public Regex {
  RegexPtr lhs;
  RegexPtr rhs;
  REither(const RegexPtr& lhs, const RegexPtr& rhs) : lhs(lhs), rhs(rhs) { }

  void show(std::ostream& out) const { this->lhs->show(out); out << "|"; this->rhs->show(out); }
};

// AB (match A, then match B)
struct RSeq : public Regex {
  RegexPtr lhs;
  RegexPtr rhs;
  RSeq(const RegexPtr& lhs, const RegexPtr& rhs) : lhs(lhs), rhs(rhs) { }

  void show(std::ostream& out) const { this->lhs->show(out); this->rhs->show(out); }
};

// (?<x>E) (match E, bind the substring to a variable "x")
struct RBind : public Regex {
  std::string var;
  RegexPtr    def;
  RBind(const std::string& var, const RegexPtr& def) : var(var), def(def) { }

  void show(std::ostream& out) const {
    out << "(?<" << this->var << ">";
    this->def->show(out);
    out << ")";
  }
};

template <typename T>
  struct switchRegex {
    virtual T with(const REps*)       const = 0;
    virtual T with(const RCharRange*) const = 0;
    virtual T with(const RStar*)      const = 0;
    virtual T with(const REither*)    const = 0;
    virtual T with(const RSeq*)       const = 0;
    virtual T with(const RBind*)      const = 0;
  };

template <typename T>
  T switchOf(const RegexPtr& p, const switchRegex<T>& f) {
    if (const REps* x = dynamic_cast<REps*>(p.get())) {
      return f.with(x);
    } else if (const RCharRange* x = dynamic_cast<RCharRange*>(p.get())) {
      return f.with(x);
    } else if (const RStar* x = dynamic_cast<RStar*>(p.get())) {
      return f.with(x);
    } else if (const REither* x = dynamic_cast<REither*>(p.get())) {
      return f.with(x);
    } else if (const RSeq* x = dynamic_cast<RSeq*>(p.get())) {
      return f.with(x);
    } else if (const RBind* x = dynamic_cast<RBind*>(p.get())) {
      return f.with(x);
    } else {
      throw std::runtime_error("Internal error, not a valid regex case");
    }
  }

std::string show(const Regex& rgx) {
  std::ostringstream ss;
  rgx.show(ss);
  return ss.str();
}
std::string show(const RegexPtr& rgx) { return show(*rgx); }

// shorthand constructors for regex AST forms
RegexPtr epsilon() {
  return RegexPtr(new REps());
}

RegexPtr charLit(rchar_t c) {
  return RegexPtr(new RCharRange(c, c));
}

RegexPtr charRange(rchar_t b, rchar_t e) {
  return RegexPtr(new RCharRange(b, e));
}

RegexPtr zeroOrMore(const RegexPtr& p) {
  return RegexPtr(new RStar(p));
}

RegexPtr either(const RegexPtr& p0, const RegexPtr& p1) {
  return RegexPtr(new REither(p0, p1));
}

RegexPtr bindTo(const std::string& name, const RegexPtr& p) {
  if (name.empty()) {
    return p;
  } else {
    return RegexPtr(new RBind(name, p));
  }
}

RegexPtr sequence(const RegexPtr& p0, const RegexPtr& p1) {
  return RegexPtr(new RSeq(p0, p1));
}

RegexPtr anyOf(const Regexes& rs) {
  if (rs.size() == 0) {
    return epsilon();
  } else {
    RegexPtr a = rs[0];
    for (size_t i = 1; i < rs.size(); ++i) {
      a = either(a, rs[i]);
    }
    return a;
  }
}

typedef std::pair<rchar_t, rchar_t> CharRange;
typedef std::vector<CharRange> CharRanges;

CharRanges toRanges(const std::set<rchar_t>& cs) {
  if (cs.size() == 0) {
    return CharRanges();
  } else {
    auto ci = cs.begin();
    rchar_t b = *ci;
    rchar_t e = b;

    CharRanges r;
    do {
      ++ci;
      if (ci == cs.end()) {
        r.push_back(std::make_pair(b, e));
      } else if (e+1 == *ci) {
        e = *ci;
      } else {
        r.push_back(std::make_pair(b, e));
        b = e = *ci;
      }
    } while (ci != cs.end());
    return r;
  }
}

RegexPtr anyOf(const std::set<rchar_t>& cs) {
  Regexes rs;
  for (const auto& p : toRanges(cs)) {
    rs.push_back(charRange(p.first, p.second));
  }
  return anyOf(rs);
}

void unescapeInto(rchar_t e, std::set<rchar_t>* out) {
  switch (e) {
  case 't': out->insert('\t'); break;
  case 'r': out->insert('\r'); break;
  case 'n': out->insert('\n'); break;
  default:  out->insert(e);    break;
  }
}

RegexPtr unescapePatChar(rchar_t x) {
  std::set<rchar_t> cs;
  unescapeInto(x, &cs);
  return anyOf(cs);
}

// read char sets
typedef std::pair<size_t, std::set<rchar_t>> DCharset;

void charRange(rchar_t i, rchar_t e, std::set<rchar_t>* out) {
  for (size_t x = static_cast<size_t>(i); x <= static_cast<size_t>(e); ++x) {
    out->insert(static_cast<rchar_t>(x));
  }
}

const std::set<rchar_t>& anyChars() {
  static std::set<rchar_t> r;
  if (r.size() == 0) {
    charRange(0x00, 0xff, &r);
  }
  return r;
}

DCharset readCharset(const std::string& x, size_t i) {
  DCharset r;
  r.first = i;

  // lookahead by one char so that we can catch ranges and escapes
  while (r.first < x.size()) {
    rchar_t c0 = x[r.first];
    ++r.first;

    rchar_t c1 = (r.first == x.size()) ? '\0' : x[r.first];
    if (c0 == ']') {
      return r;
    } else if (c0 == '\\') {
      unescapeInto(c1, &r.second);
      ++r.first;
    } else if (c1 == '-' && (r.first+1) < x.size()) {
      charRange(c0, x[r.first+1], &r.second);
      ++r.first;
      ++r.first;
    } else {
      r.second.insert(c0);
    }
  }
  return r;
}

// parse a complete regex
typedef std::pair<size_t, RegexPtr> DRegex;
DRegex diffRegex(const RegexPtr& lhs, const std::string& x, size_t i);

DRegex returnR(const std::string& x, size_t k, const RegexPtr& r) {
  switch (k==x.size() ? '\0' : x[k]) {
  case '?': return DRegex(k+1, either(epsilon(), r));
  case '*': return DRegex(k+1, zeroOrMore(r));
  case '+': return DRegex(k+1, sequence(r, zeroOrMore(r)));
  default:  return DRegex(k, r);
  }
}

DRegex seqR(const RegexPtr& lhs, const RegexPtr& c, const std::string& x, size_t k) {
  DRegex cm = returnR(x, k, c);
  return diffRegex(sequence(lhs, cm.second), x, cm.first);
}

DRegex diffRegex(const RegexPtr& lhs, const std::string& x, size_t i) {
  if (i == x.size()) {
    return DRegex(i, lhs);
  } else {
    rchar_t n = (i+1==x.size()) ? '\0' : x[i+1];

    switch (x[i]) {
    case ')': return returnR(x, i+1, lhs);
    case '(': {
      // maybe read a binding name for this group
      // (according to typical accepted regex syntax)
      std::string b;
      if (n == '?') {
        size_t k = i+2;
        if (k < x.size() && x[k] == 'P') ++k;
        rchar_t gd = (k < x.size() ? x[k] : '\0');
        ++k;
        rchar_t ed;
        if (gd == '<') {
          ed = '>';
        } else if (gd == '\'') {
          ed = '\'';
        } else {
          throw std::runtime_error("Expected capturing group name delimited as 'name' or <name>");
        }
        size_t j = k;
        while (j < x.size() && x[j] != ed) { ++j; }

        b = x.substr(k, j-k);
        i = j;
      }

      // now the group body just matches as if inline
      // (but we may bind to the group match result)
      DRegex g = diffRegex(epsilon(), x, i+1);
      return diffRegex(sequence(lhs, bindTo(b, g.second)), x, g.first);
    }
    case '|': {
      DRegex n = diffRegex(epsilon(), x, i+1);
      return DRegex(n.first, either(lhs, n.second));
    }
    case '.': {
      return seqR(lhs, anyOf(anyChars()), x, i+1);
    }
    case '[': {
      if (i+1 < x.size()) {
        bool invert = x[i+1] == '^';
        auto p      = readCharset(x, invert ? (i+2) : (i+1));
        auto cs     = invert ? setDifference(anyChars(), p.second) : p.second;

        return seqR(lhs, anyOf(cs), x, p.first);
      } else {
        throw std::runtime_error("Unexpected end in regex (expecting ']')");
      }
    }
    case '\\': {
      if (i+1 < x.size()) {
        return seqR(lhs, unescapePatChar(x[i+1]), x, i+2);
      } else {
        throw std::runtime_error("Unexpected end in regex (expecting escape code)");
      }
    }
    default: {
      return seqR(lhs, charLit(x[i]), x, i+1);
    }}
  }
}

RegexPtr parseRegex(const std::string& x) {
  DRegex dr = diffRegex(epsilon(), x, 0);
  while (dr.first < x.size()) {
    dr = diffRegex(dr.second, x, dr.first);
  }
  return dr.second;
}

struct bnamesF : public switchRegex<UnitV> {
  str::set* bnames;

  bnamesF(str::set* bnames) : bnames(bnames) { }

  UnitV with(const REps*) const { return unitv; }
  UnitV with(const RCharRange*) const { return unitv; }
  UnitV with(const RStar* x) const { return switchOf(x->v, *this); }

  UnitV with(const REither* x) const {
    switchOf(x->lhs, *this);
    return switchOf(x->rhs, *this);
  }

  UnitV with(const RSeq* x) const {
    switchOf(x->lhs, *this);
    return switchOf(x->rhs, *this);
  }

  UnitV with(const RBind* x) const {
    this->bnames->insert(x->var);
    return switchOf(x->def, *this);
  }
};

str::seq bindingNames(const RegexPtr& rgx) {
  str::set ns;
  switchOf(rgx, bnamesF(&ns));
  return str::seq(ns.begin(), ns.end());
}

/******************************
 * translate the regex AST to an NFA
 ******************************/
typedef uint32_t state;
typedef std::set<state> stateset;
static const state nullState = static_cast<state>(-1);

typedef uint32_t result;
static const result nullResult = static_cast<result>(-1);

struct char_range_ord {
  static bool lt(rchar_t lhs, rchar_t rhs) {
    return lhs < rhs;
  }
  static rchar_t pred(rchar_t x) {
    return x-1;
  }
  static rchar_t succ(rchar_t x) {
    return x+1;
  }
  static void copyRange(rchar_t t0, rchar_t t1, std::set<rchar_t>* out) {
    auto t = t0;
    auto e = t1;
    while (true) {
      out->insert(t);
      if (t == e) {
        break;
      } else {
        ++t;
      }
    }
  }
};

typedef range_map<rchar_t, stateset, char_range_ord> ntransitions;

typedef std::map<result, str::set> srcmarkers;

struct NFAState {
  // transitions to successor states
  stateset     eps;
  ntransitions chars;

  // if defined, the accept value for this state
  result acc = nullResult;

  // markers to begin and end recording subranges
  srcmarkers begins, ends;
  void beginMark(result r, const std::string& m) { this->begins[r].insert(m); }
  void endMark  (result r, const std::string& m) {   this->ends[r].insert(m); }
};
typedef std::vector<NFAState> NFA;

// for a given set of NFA states, find the set of non-overlapping char ranges
CharRanges usedCharRanges(const NFA& nfa, const stateset& ss) {
  CharRanges rs;
  for (auto s : ss) {
    rs = nfa[s].chars.disjointRanges(rs);
  }
  return rs;
}

// accumulate a regex match into an NFA
state regexBefore(const RegexPtr&, state cont, result, NFA*);

struct linkStateF : public switchRegex<UnitV> {
  NFA*   nfa;
  state  self;
  state  succ;
  result id;

  linkStateF(NFA* nfa, state self, state succ, result id) : nfa(nfa), self(self), succ(succ), id(id) {
  }

  NFAState& s(state k) const { return (*this->nfa)[k]; }
  NFAState& s()        const { return s(this->self); }

  state ins(const RegexPtr& p) const { return regexBefore(p, this->succ, this->id, this->nfa); }

  UnitV with(const REps*) const {
    s().eps.insert(this->succ);
    return unitv;
  }

  UnitV with(const RCharRange* x) const {
    s().chars.mergeRange(x->b, x->e, [&](stateset& ss){ ss.insert(this->succ); });
    return unitv;
  }

  UnitV with(const RStar* x) const {
    switchOf(x->v, linkStateF(this->nfa, this->self, this->self, this->id));
    s().eps.insert(this->succ);
    return unitv;
  }

  UnitV with(const REither* x) const {
    state lstate = ins(x->lhs);
    state rstate = ins(x->rhs);
    s().eps.insert(lstate);
    s().eps.insert(rstate);
    return unitv;
  }

  UnitV with(const RSeq* x) const {
    state rstate = ins(x->rhs);
    switchOf(x->lhs, linkStateF(this->nfa, this->self, rstate, this->id));
    return unitv;
  }

  UnitV with(const RBind* x) const {
    s().beginMark(this->id, x->var);
    switchOf(x->def, *this);
    s(this->succ).endMark(this->id, x->var);
    return unitv;
  }
};

state regexBefore(const RegexPtr& p, state cont, result x, NFA* nfa) {
  state rstate = nfa->size();
  nfa->resize(rstate+1);
  switchOf(p, linkStateF(nfa, rstate, cont, x));
  return rstate;
}

state accumRegex(const RegexPtr& p, result x, NFA* nfa) {
  state rstate = nfa->size();
  nfa->resize(rstate+1);
  (*nfa)[rstate].acc = x;

  return regexBefore(p, rstate, x, nfa);
}

std::set<rchar_t> usedChars(const NFA& nfa) {
  std::set<rchar_t> cs;
  for (const auto& s : nfa) {
    s.chars.keys(&cs);
  }
  return cs;
}

/*****************************
 * print an NFA
 *****************************/
std::string descStates(const stateset& ss) {
  std::ostringstream out;
  if (ss.size() > 0) {
    auto s = ss.begin();
    out << *s;
    ++s;
    for (; s != ss.end(); ++s) {
      out << "/" << *s;
    }
  }
  return out.str();
}

void print(std::ostream& out, const NFA& nfa) {
  // make columns for state, eps, and acc
  str::seq state;
  state.push_back("");

  str::seq eps;
  eps.push_back("eps");

  str::seq acc;
  acc.push_back("acc");

  size_t si = 0;
  for (const auto& s : nfa) {
    state.push_back(str::from(si++) + ":");
    eps.push_back(descStates(s.eps));
    acc.push_back(s.acc == nullResult ? "" : str::from(s.acc));
  }

  // make columns for used chars
  str::seqs ccols;
  for (auto c : usedChars(nfa)) {
    ccols.push_back(str::seq());
    str::seq& ccol = ccols.back();

    ccol.push_back(str::from(c));
    for (const auto& s : nfa) {
      if (const auto* ts = s.chars.lookup(c)) {
        ccol.push_back(descStates(*ts));
      } else {
        ccol.push_back("");
      }
    }
  }

  // make columns for begin/end capture markers
  str::seq mbegin;
  mbegin.push_back("mbegin");

  str::seq mend;
  mend.push_back("mend");

  for (const auto& s : nfa) {
    std::ostringstream mb;
    for (const auto& b : s.begins) {
      mb << "{" << b.first << ":";
      for (const auto& v : b.second) {
        mb << " " << v;
      }
      mb << "}";
    }
    mbegin.push_back(mb.str());

    std::ostringstream me;
    for (const auto& e : s.ends) {
      me << "{" << e.first << ":";
      for (const auto& v : e.second) {
        me << " " << v;
      }
      me << "}";
    }
    mend.push_back(me.str());
  }

  // print a table out of all of this
  str::seqs tbl;
  tbl.push_back(state);
  tbl.push_back(eps);
  tbl.insert(tbl.end(), ccols.begin(), ccols.end());
  tbl.push_back(acc);
  tbl.push_back(mbegin);
  tbl.push_back(mend);
  str::printRightAlignedTable(out, tbl);
}

/**********************
 * find eps* for an NFA
 **********************/
typedef std::map<state, stateset> EpsClosure;

typedef std::vector<bool> statemarks;

void findEpsClosure(const NFA& nfa, state s, statemarks* sms, EpsClosure* ec) {
  if (!(*sms)[s]) {
    (*sms)[s] = true;
    for (auto et : nfa[s].eps) {
      if (!(*sms)[et]) {
        findEpsClosure(nfa, et, sms, ec);
      }
    }

    stateset stes = (*ec)[s];
    stes.insert(s);
    for (auto et : nfa[s].eps) {
      stes.insert(et);

      const stateset& rstes = (*ec)[et];
      stes.insert(rstes.begin(), rstes.end());
    }
    (*ec)[s] = stes;
  }
}

void findEpsClosure(const NFA& nfa, EpsClosure* ec) {
  statemarks ms(nfa.size(), false);

  for (state s = 0; s < nfa.size(); ++s) {
    if (!ms[s]) {
      findEpsClosure(nfa, s, &ms, ec);
    }
  }
}

const stateset& epsState(const EpsClosure& ec, state s) {
  auto eci = ec.find(s);
  if (eci != ec.end()) {
    return eci->second;
  } else {
    throw std::runtime_error("Internal error, NFA state not in eps*");
  }
}

stateset epsState(const EpsClosure& ec, const stateset& ss) {
  stateset r;
  for (state s : ss) {
    auto nss = epsState(ec, s);
    r.insert(nss.begin(), nss.end());
  }
  return r;
}

void print(std::ostream& out, const stateset& ss) {
  out << "{";
  auto s = ss.begin();
  if (s != ss.end()) {
    out << *s;
    ++s;
    for (; s != ss.end(); ++s) {
      out << ", " << *s;
    }
  }
  out << "}";
}

void print(std::ostream& out, const EpsClosure& ec) {
  for (const auto& eset : ec) {
    out << eset.first << " -> ";
    print(out, eset.second);
    out << std::endl;
  }
}

/**********************
 * convert an NFA to a DFA
 **********************/
typedef range_map<rchar_t, state, char_range_ord> dtransitions;
struct DFAState {
  dtransitions chars;

  // if defined, the accept value for this state
  result acc = nullResult;

  // markers to begin and end recording subranges
  srcmarkers begins, ends;
};
typedef std::vector<DFAState> DFA;

void insert(stateset* o, const stateset& i) {
  o->insert(i.begin(), i.end());
}

// find the set of NFA states we'd transition to from a char from within a set of states
stateset nfaTransition(const NFA& nfa, const EpsClosure& ec, const stateset& ss, const CharRange& cr) {
  stateset result;
  for (state s : ss) {
    if (const auto* tss = nfa[s].chars.lookupRangeSubset(cr)) {
      insert(&result, epsState(ec, *tss));
    }
  }
  return result;
}

// sets of NFA states are mapped to distinct DFA states
typedef std::map<stateset, state> Nss2Ds;

// create a DFA state from a set of NFA states
// (or if it's already been made, just return the existing state)
state dfaState(const cc* c, const NFA& nfa, const EpsClosure& ec, Nss2Ds* nss2ds, DFA* dfa, const stateset& ss, RStates* rstates) {
  // did we already make this state?  if so, just return it
  auto didIt = nss2ds->find(ss);
  if (didIt != nss2ds->end()) {
    return didIt->second;
  }

  // we need to make this state -- allocate it and remember it
  state result = dfa->size();
  dfa->resize(dfa->size() + 1);

  if (c->throwOnHugeRegexDFA() and c->regexDFAOverNFAMaxRatio() > 0 and (dfa->size() / nfa.size() > size_t(c->regexDFAOverNFAMaxRatio()))) {
    throw std::runtime_error("regexes DFA over NFA Max ratio was breached");
  }

  (*nss2ds)[ss] = result;

  // ok, how can we transition out of here?
  // for each case, we'll go to a set of NFA states (recursively)
  for (auto cr : usedCharRanges(nfa, ss)) {
    auto ns = dfaState(c, nfa, ec, nss2ds, dfa, nfaTransition(nfa, ec, ss, cr), rstates);
    (*dfa)[result].chars.insert(cr, ns);
  }

  // our DFA state accepts if any of its NFA states accept
  // we may have multiple potential matches here, so we should
  // keep track of every such set so that outer match compilation
  // can choose the right one
  for (state s : ss) {
    auto nr = nfa[s].acc;
    if (nr != nullResult) {
      (*dfa)[result].acc = result;
      (*rstates)[result].insert(nr);
    }
  }

  // our DFA state begins/ends subrange recording for each collapsed NFA state
  for (state s : ss) {
    for (const auto& b : nfa[s].begins) {
      (*dfa)[result].begins[b.first].insert(b.second.begin(), b.second.end());
    }
    for (const auto& e : nfa[s].ends) {
      (*dfa)[result].ends[e.first].insert(e.second.begin(), e.second.end());
    }
  }

  // that's it, we're done
  return result;
}

void disambiguate(const cc* c, const NFA& nfa, DFA* dfa, RStates* rstates) {
  // determine eps* for this NFA
  EpsClosure ec;
  findEpsClosure(nfa, &ec);

  // starting from the eps* start state,
  // follow non-eps transitions to eps* successor states
  Nss2Ds nss2ds;
  dfaState(c, nfa, ec, &nss2ds, dfa, epsState(ec, 0), rstates);
}

/*****************************
 * print a DFA
 *****************************/
std::set<rchar_t> usedChars(const DFA& dfa) {
  std::set<rchar_t> cs;
  for (const auto& s : dfa) {
    s.chars.keys(&cs);
  }
  return cs;
}

void print(std::ostream& out, const DFA& dfa) {
  // make columns for state, eps, and acc
  str::seq state;
  state.push_back("");

  str::seq acc;
  acc.push_back("acc");

  str::seq mbegin;
  mbegin.push_back("mbegin");

  str::seq mend;
  mend.push_back("mend");

  size_t si = 0;
  for (const auto& s : dfa) {
    state.push_back(str::from(si++) + ":");
    acc.push_back(s.acc == nullResult ? "" : str::from(s.acc));

    std::ostringstream mb;
    for (const auto& b : s.begins) {
      mb << "{" << b.first << ":";
      for (const auto& v : b.second) {
        mb << " " << v;
      }
      mb << "}";
    }
    mbegin.push_back(mb.str());

    std::ostringstream me;
    for (const auto& e : s.ends) {
      me << "{" << e.first << ":";
      for (const auto& v : e.second) {
        me << " " << v;
      }
      me << "}";
    }
    mend.push_back(me.str());
  }

  // make columns for used chars
  str::seqs ccols;
  for (auto c : usedChars(dfa)) {
    ccols.push_back(str::seq());
    str::seq& ccol = ccols.back();

    ccol.push_back(str::from(c));
    for (const auto& s : dfa) {
      if (const auto* st = s.chars.lookup(c)) {
        ccol.push_back(str::from(*st));
      } else {
        ccol.push_back("");
      }
    }
  }

  // print a table out of all of this
  str::seqs tbl;
  tbl.push_back(state);
  tbl.insert(tbl.end(), ccols.begin(), ccols.end());
  tbl.push_back(acc);
  tbl.push_back(mbegin);
  tbl.push_back(mend);
  str::printRightAlignedTable(out, tbl);
}

/**********************
 * convert a DFA to an equivalent function
 **********************/
static ExprPtr transitionToState(const std::string& fname, int s, const LexicalAnnotation& rootLA) {
  // produce an expression fragment to jump to the given state
  return fncall(var(fname, rootLA), list(var("cap", rootLA), var("cs", rootLA), var("n", rootLA), var("e", rootLA), constant(s, rootLA)), rootLA);
}

static ExprPtr transitionAsCharSwitch(const std::string& fname, const DFAState& s, const ExprPtr& charExpr, const ExprPtr& defaultResult, const LexicalAnnotation& rootLA) {
  Switch::Bindings cs;
  std::set<rchar_t> tcs;
  s.chars.keys(&tcs);
  for (auto tc : tcs) {
    cs.push_back(Switch::Binding(PrimitivePtr(new Char(tc, rootLA)),
      transitionToState(fname, *s.chars.lookup(tc), rootLA)
    ));
  }
  return
    switchE(
      charExpr,
      cs,
      defaultResult,
      rootLA
    );
}

static ExprPtr charInRange(const ExprPtr& c, const std::pair<rchar_t, rchar_t>& crange, const LexicalAnnotation& rootLA) {
  if (crange.first == 0 && crange.second == 255) {
    return constant(true, rootLA);
  } else if (crange.first == 0) {
    return fncall(var("blte", rootLA), list(c, constant(static_cast<uint8_t>(crange.second), rootLA)), rootLA);
  } else if (crange.first == crange.second) {
    return fncall(var("beq", rootLA), list(c, constant(static_cast<uint8_t>(crange.first), rootLA)), rootLA);
  } else {
    return
      fncall(
        var("and", rootLA),
        list(
          fncall(var("blte", rootLA), list(constant(static_cast<uint8_t>(crange.first), rootLA), c), rootLA),
          fncall(var("blte", rootLA), list(c, constant(static_cast<uint8_t>(crange.second), rootLA)), rootLA)
        ),
        rootLA
      );
  }
}

static ExprPtr transitionAsRangeChecks(const std::string& fname, const std::vector<std::pair<std::pair<rchar_t,rchar_t>, state>>& ranges, const ExprPtr& charExpr, const ExprPtr& defaultResult, const LexicalAnnotation& rootLA) {
  std::string n = freshName();
  ExprPtr cvar = var(n, rootLA);

  ExprPtr ifvn = var("if", rootLA);

  ExprPtr result = defaultResult;
  for (auto r = ranges.rbegin(); r != ranges.rend(); ++r) {
    result =
      fncall(
        ifvn,
        list(
          charInRange(cvar, r->first, rootLA),
          transitionToState(fname, r->second, rootLA),
          result
        ),
        rootLA
      );
  }
  return
    let(n, assume(fncall(var("unsafeCast", rootLA), list(charExpr), rootLA), primty("byte"), rootLA),
      result,
      rootLA
    );
}

static ExprPtr transitionMapping(const std::string& fname, const DFAState& s, const ExprPtr& charExpr, const ExprPtr& defaultResult, const LexicalAnnotation& rootLA) {
  // for the full 'range -> state' transition map at this state
  //   are we mostly looking at single character transitions?
  //   if so, we're better off using a switch
  //   else use a sequence of range tests
  auto rtns = s.chars.mapping();

  if (rtns.size() == 0) {
    // shouldn't happen, but it's the right thing to do
    return defaultResult;
  } else if (rtns.size() == 1 && rtns[0].first.first == 0 && rtns[0].first.second == 255) {
    // a catch-all case, don't need any special logic
    return transitionToState(fname, rtns[0].second, rootLA);
  }

  double sd = 0.0;
  for (const auto& rtn : rtns) {
    sd += 1 + rtn.first.second - rtn.first.first;
  }
  double avgs = sd / static_cast<double>(rtns.size());

  if (avgs <= 2.0) {
    return transitionAsCharSwitch(fname, s, charExpr, defaultResult, rootLA);
  } else {
    return transitionAsRangeChecks(fname, rtns, charExpr, defaultResult, rootLA);
  }
}

void makeExprDFAFunc(cc* c, const std::string& fname, const MonoTypePtr& captureTy, const DFA& dfa, const LexicalAnnotation& rootLA) {
  // F(cap,cs,i,e,s) =
  //   switch (s) {
  //   ...
  //   case S:
  //     cap.N.beginX <- i;
  //     ... etc for every begin marker
  //     cap.N.endX <- i;
  //     ... etc for every end marker
  //
  //     if (i == e) then
  //       ACC(S)
  //     else
  //       switch (c) {
  //       ...
  //       case C:
  //         F(cs, i+1, e, SUCC(S,C))
  //       ...
  //   ...
  Switch::Bindings bs;
  MonoTypePtr arrT = freshTypeVar();
  QualTypePtr qarrElemTy = qualtype(list(ConstraintPtr(new Constraint("Array", list(arrT, primty("char"))))), functy(list(arrT, primty("long")), primty("char")));
  QualTypePtr qarrT = qualtype(list(ConstraintPtr(new Constraint("Array", list(arrT, primty("char"))))), arrT);

  for (size_t s = 0; s < dfa.size(); ++s) {
    // all transitions out of this state
    ExprPtr dispatchExpr =
      transitionMapping(
        fname,
        dfa[s],
        fncall(var("elem", rootLA), list(var("cs", rootLA), var("i", rootLA)), rootLA),
        constant(static_cast<int>(-1), rootLA),
        rootLA
      );

    // evaluate the input char and transition
    ExprPtr evalChar =
      fncall(
        var("if", rootLA),
        list(
          fncall(var("leq", rootLA), list(var("i", rootLA), var("e", rootLA)), rootLA),
          constant(static_cast<int>(dfa[s].acc), rootLA),
          dispatchExpr
        ),
        rootLA
      );

    // mark begin/end capture ranges as appropriate
    for (const auto& b : dfa[s].begins) {
      for (const auto& v : b.second) {
        // cap.N.beginvar <- i
        // ...
        evalChar =
          let("_", assign(proj(var("cap", rootLA), str::from(b.first) + "_begin_" + v, rootLA), var("i", rootLA), rootLA),
            evalChar,
            rootLA
          );
      }
    }

    for (const auto& e : dfa[s].ends) {
      for (const auto& v : e.second) {
        // cap.N.endvar <- i
        // ...
        evalChar =
          let("_", assign(proj(var("cap", rootLA), str::from(e.first) + "_end_" + v, rootLA), var("i", rootLA), rootLA),
            evalChar,
            rootLA
          );
      }
    }

    // do all of this when in this state
    bs.push_back(Switch::Binding(PrimitivePtr(new Int(static_cast<int>(s), rootLA)), evalChar));
  }

  ExprPtr fndef =
    fn(str::strings("cap", "cs", "i", "e", "s"),
      let("n", fncall(var("ladd", rootLA), list(var("i", rootLA), constant(static_cast<long>(1), rootLA)), rootLA),
      let("elem", assume(var("element", rootLA), qarrElemTy, rootLA),
        switchE(
          var("s", rootLA),
          bs,
          constant(static_cast<int>(-1), rootLA),
          rootLA
        ),
      rootLA),rootLA),
      rootLA
    );

  c->define(fname, assume(fndef, qualtype(qarrT->constraints(), functy(list(captureTy, arrT, primty("long"), primty("long"), primty("int")), primty("int"))), rootLA));
}

typedef std::pair<char,char>  CRange;
typedef std::pair<CRange,int> CTransition;
typedef array<CTransition>    CTransitions;

DEFINE_STRUCT(
  DFAStateRep,
  (CTransitions*, transitions),
  (int,           acc)
);

array<DFAStateRep>* makeDFARep(cc* c, const DFA& dfa) {
  auto result = c->makeArray<DFAStateRep>(dfa.size());
  for (size_t i = 0; i < dfa.size(); ++i) {
    DFAStateRep& s = result->data[i];
    auto ctnm = dfa[i].chars.mapping();
    s.transitions = c->makeArray<CTransition>(ctnm.size());
    for (size_t j = 0; j < ctnm.size(); ++j) {
      s.transitions->data[j] = ctnm[j];
    }
    s.transitions->size = ctnm.size();
    s.acc = dfa[i].acc;
  }
  result->size = dfa.size();
  return result;
}

void makeInterpDFAFunc(cc* c, const std::string& fname, const MonoTypePtr& captureTy, const DFA& dfa, const LexicalAnnotation& rootLA) {
  MonoTypePtr arrT = freshTypeVar();
  QualTypePtr qarrElemTy = qualtype(list(ConstraintPtr(new Constraint("Array", list(arrT, primty("char"))))), functy(list(arrT, primty("long")), primty("char")));
  QualTypePtr qarrT = qualtype(list(ConstraintPtr(new Constraint("Array", list(arrT, primty("char"))))), arrT);

  std::string regexDFADef = ".regexDFA." + freshName();
  c->bind(regexDFADef, makeDFARep(c, dfa));

  ExprPtr fndef =
    fn(str::strings("cap", "cs", "i", "e", "s"),
      fncall(var("runRegexDFA", rootLA), list(var("cs", rootLA), var("i", rootLA), var("e", rootLA), var("s", rootLA), var(regexDFADef, rootLA)), rootLA),
      rootLA
    );

  c->define(fname, assume(fndef, qualtype(qarrT->constraints(), functy(list(captureTy, arrT, primty("long"), primty("long"), primty("int")), primty("int"))), rootLA));
}

void makeDFAFunc(cc* c, const std::string& fname, const MonoTypePtr& captureTy, const DFA& dfa, const LexicalAnnotation& rootLA) {
  if (dfa.size() < 1000 || !isUnit(captureTy)) {
    makeExprDFAFunc(c, fname, captureTy, dfa, rootLA);
  } else {
    makeInterpDFAFunc(c, fname, captureTy, dfa, rootLA);
  }
}

// merge char-range mappings where possible and conflate duplicate result states
void mergeCharRangesAndEqResults(DFA* dfa, const RStates& fstates, RStates* rstates) {
  std::map<RegexIdxs, size_t> results;

  for (auto& s : *dfa) {
    s.chars.compact();

    if (s.acc != nullResult) {
      auto fsi = fstates.find(s.acc);
      if (fsi == fstates.end()) { throw std::runtime_error("Internal error, invalid regex state mapping determined"); }

      auto si = results.find(fsi->second);
      if (si != results.end()) {
        s.acc = si->second;
      } else {
        size_t ns = results.size();

        results[fsi->second] = ns;
        (*rstates)[ns]       = fsi->second;
        s.acc                = ns;
      }
    }
  }
}

/**************************
 * compress a DFA by merging equivalent states
 **************************/
typedef std::map<state, state> EqStates;
EqStates findEquivStates(const DFA& dfa) {
  bit_table eqStates(dfa.size(), dfa.size(), false);
  for (state s = 0; s < dfa.size(); ++s) { eqStates.set(s, s, true); }

  // identify equivalent states to a fixed point
  // (probably this could be done more efficiently)
  bool updatedEQ = true;
  while (updatedEQ) {
    updatedEQ = false;
    for (size_t s0 = 0; s0 < dfa.size(); ++s0) {
      for (size_t s1 = 0; s1 < s0; ++s1) {
        // if we already know that two states are equivalent, they're still equivalent
        if (eqStates(s0, s1)) continue;

        // if two states have different accepting bits, they can't be equivalent
        if (dfa[s0].acc != dfa[s1].acc) continue;

        // if two states have different capture sets, they can't be equivalent
        if (dfa[s0].begins != dfa[s1].begins) continue;
        if (dfa[s0].ends   != dfa[s1].ends)   continue;

        // if the shape of mapping sets differs between states, they can't be equivalent
        auto m0 = dfa[s0].chars.mapping();
        auto m1 = dfa[s1].chars.mapping();
        if (m0.size() != m1.size()) continue;

        // if there is a transition (c,q) in s0 and (c,q') in s1 and q != q', then s0 != s1 (in this cycle)
        bool tsEq = true;
        for (size_t i = 0; i < m0.size() && tsEq; ++i) {
          tsEq = m0[i].first == m1[i].first && eqStates(m0[i].second, m1[i].second);
        }
        if (tsEq) {
          eqStates.set(s0, s1, true);
          updatedEQ = true;
        }
      }
    }
  }

  // convert to a representation that makes state substitution explicit
  //  (follow mapped-to states in case they are also mapped)
  EqStates r;
  for (size_t s0 = 0; s0 < dfa.size(); ++s0) {
    for (size_t s1 = 0; s1 < s0; ++s1) {
      if (eqStates(s0, s1)) {
        auto s1t = r.find(s1);
        r[s0] = s1t == r.end() ? s1 : s1t->second;
      }
    }
  }
  return r;
}

DFA removeEquivStates(const DFA& dfa, const EqStates& eqs) {
  // after removing states, some states will need to be renumbered
  // we can infer this mapping from the eqstate mapping
  std::map<state, state> shifted;
  size_t accShift = 0;
  for (auto eq = eqs.begin(); eq != eqs.end();) {
    state s0 = eq->first+1;
    ++eq;
    state s1 = (eq == eqs.end()) ? dfa.size() : eq->first;
    ++accShift;

    for (state s = s0; s < s1; ++s) {
      shifted[s] = s - accShift;
    }
  }

  // now make a new DFA with eq states removed
  // all references to states need to be
  // patched up to account for merging/shifting
  DFA result;
  result.reserve(dfa.size()-eqs.size());

  for (size_t s = 0; s < dfa.size(); ++s) {
    const auto& sd = dfa[s];

    // don't include eliminated states
    if (eqs.find(s) != eqs.end()) continue;

    // bring this state over to the result
    result.push_back(dfa[s]);
    auto& rsd = result.back();

    // patch its transitions to account for merged and shifted states
    for (const auto& m : sd.chars.mapping()) {
      auto  eq    = eqs.find(m.second);
      state tgt   = (eq == eqs.end()) ? m.second : eq->second;
      auto  shift = shifted.find(tgt);
      state stgt  = (shift == shifted.end()) ? tgt : shift->second;

      if (stgt != m.second) {
        rsd.chars.insert(m.first, stgt);
      }
    }
  }
  return result;
}

/**************************
 * make an expression to allocate capture group data for a set of regular expressions
 **************************/
MonoTypePtr regexCaptureBufferType(const Regexes& regexes) {
  Record::Members ms;
  for (size_t r = 0; r < regexes.size(); ++r) {
    for (const auto& b : bindingNames(regexes[r])) {
      ms.push_back(Record::Member(str::from(r) + "_begin_" + b, primty("long")));
      ms.push_back(Record::Member(str::from(r) + "_end_"   + b, primty("long")));
    }
  }
  return ms.size() == 0 ? primty("unit") : MonoTypePtr(Record::make(ms));
}

ExprPtr makeRegexCaptureBuffer(const Regexes& regexes, const LexicalAnnotation& rootLA) {
  return
    assume(
      fncall(
        var("newPrimZ", rootLA),
        list(mktunit(rootLA)),
        rootLA
      ),
      regexCaptureBufferType(regexes),
      rootLA
    );
}

/**************************
 * make a function to determine which among the input regexes here a later string matches
 **************************/
CRegexes makeRegexFn(cc* c, const Regexes& regexes, const LexicalAnnotation& rootLA) {
  CRegexes result;

  // save capturing-group settings
  result.captureBuffer = makeRegexCaptureBuffer(regexes, rootLA);
  for (size_t i = 0; i < regexes.size(); ++i) {
    result.captureVarsAt[i] = bindingNames(regexes[i]);
  }

  // our NFA will non-deterministically jump to every possible start state
  NFA nfa;
  nfa.resize(1);
  for (size_t i = 0; i < regexes.size(); ++i) {
    auto s = accumRegex(regexes[i], i, &nfa);
    nfa[0].eps.insert(s);
  }

  // now map this NFA to a DFA
  DFA dfa;
  RStates fstates;
  disambiguate(c, nfa, &dfa, &fstates);

  // make all char ranges compact and minimize the results to avoid redundant work in the caller
  mergeCharRangesAndEqResults(&dfa, fstates, &result.rstates);

  // in case of blowup, minimize the size of this DFA
  dfa = removeEquivStates(dfa, findEquivStates(dfa));

  // translate this DFA to a function
  std::string fname = ".regex." + freshName();
  makeDFAFunc(c, fname, regexCaptureBufferType(regexes), dfa, rootLA);

  // and that's the function that the outer match logic should use
  result.fname = fname;
  return result;
}

/**************************
 * produce code to load capture vars out of a buffer for a given DFA accept state (which may map back to multiple source regexes)
 **************************/
CVarDefs unpackCaptureVars(const std::string& strVar, const std::string& bufferVar, const CRegexes& crgxs, size_t state, const LexicalAnnotation& rootLA) {
  auto rss = crgxs.rstates.find(state);
  if (rss == crgxs.rstates.end()) return CVarDefs();

  CVarDefs result;
  for (auto rs : rss->second) {
    auto cvars = crgxs.captureVarsAt.find(rs);
    if (cvars == crgxs.captureVarsAt.end()) continue;

    for (const auto& vn : cvars->second) {
      result.push_back(CVarDef(vn,
        fncall(
          var("slice", rootLA),
          list(
            var(strVar, rootLA),
            proj(var(bufferVar, rootLA), str::from(rs) + "_begin_" + vn, rootLA),
            proj(var(bufferVar, rootLA), str::from(rs) + "_end_"   + vn, rootLA)
          ),
          rootLA
        )
      ));
    }
  }
  return result;
}

}
