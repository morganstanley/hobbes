
#include <hobbes/parse/lalr.H>
#include <stdexcept>

namespace hobbes {

/*
 * basic grammar/rule analysis
 */
void removeRuleDefs(terminal* s, grammar* g) {
  grammar::iterator r = g->find(s);
  if (r != g->end()) {
    g->erase(r);
  }
}

void removeRuleRefs(terminal* s, grammar* g) {
  for (grammar::iterator sd = g->begin(); sd != g->end(); ++sd) {
    for (nat r = 0; r < sd->second.size();) {
      if (in(s, sd->second[r])) {
        sd->second.erase(sd->second.begin() + r);
      } else {
        ++r;
      }
    }
  }
}

void undefineSymbol(terminal* s, grammar* g) {
  removeRuleDefs(s, g);
  removeRuleRefs(s, g);
}

grammar removeRuleDefs(terminal* s, const grammar& g) {
  grammar r = g;
  removeRuleDefs(s, &r);
  return r;
}

grammar removeRuleRefs(terminal* s, const grammar& g) {
  grammar r = g;
  removeRuleRefs(s, &r);
  return r;
}

grammar undefineSymbol(terminal* s, const grammar& g) {
  grammar r = g;
  undefineSymbol(s, &r);
  return r;
}

terminalset definedSymbols(const grammar& g) {
  return keys(g);
}

// a top-level symbol is used by nothing, or just by itself
terminalset topLevelSymbols(const grammar& g) {
  terminalset result;
  terminalset dsyms = definedSymbols(g);

  for (terminalset::const_iterator t = dsyms.begin(); t != dsyms.end(); ++t) {
    terminalset users = symbolsUsing(g, *t);
    if (users.size() == 0 || (users.size() == 1 && in(*t, users))) {
      result.insert(*t);
    }
  }

  return result;
}

void symbolsUsed(const grammar& g, const rule& r, terminalset* ss) {
  for (rule::const_iterator t = r.begin(); t != r.end(); ++t) {
    if (hasRule(g, *t)) {
      if (!in(*t, *ss)) {
        ss->insert(*t);
        symbolsUsed(g, *t, ss);
      }
    }
  }
}

void symbolsUsed(const grammar& g, const rules& rs, terminalset* ss) {
  for (rules::const_iterator r = rs.begin(); r != rs.end(); ++r) {
    symbolsUsed(g, *r, ss);
  }
}

void symbolsUsed(const grammar& g, terminal* s, terminalset* ss) {
  grammar::const_iterator sd = g.find(s);
  if (sd != g.end()) {
    symbolsUsed(g, sd->second, ss);
  }
}

terminalset symbolsUsed(const grammar& g, terminal* s) {
  terminalset r;
  symbolsUsed(g, s, &r);
  return r;
}

terminalset symbolsUsing(const grammar& g, terminal* s) {
  terminalset result;
  terminalset dsyms = definedSymbols(g);

  for (terminalset::const_iterator t = dsyms.begin(); t != dsyms.end(); ++t) {
    if (in(s, symbolsUsed(g, *t))) {
      result.insert(*t);
    }
  }

  return result;
}

bool hasRule(const grammar& g, terminal* t) {
  return g.find(t) != g.end();
}

const rules* aggRules(const grammar& g, terminal* t) {
  grammar::const_iterator ad = g.find(t);
  if (ad == g.end()) {
    throw std::runtime_error("Internal error, no such aggregate defined.");
  } else {
    return &(ad->second);
  }
}

const rule& nthRule(const rules* rs, nat n) {
  if (n >= rs->size()) {
    throw std::runtime_error("Internal error, no such rule.");
  } else {
    return (*rs)[n];
  }
}

terminal* ruleElement(const rule& r, nat i) {
  if (i >= r.size()) {
    return 0;
  } else {
    return r[i];
  }
}

bool directlyDerivesNull(const rule& r) {
  return r.size() == 0;
}

bool directlyDerivesNull(const grammar& g, terminal* s) {
  const rules* rs = aggRules(g, s);
  for (rules::const_iterator r = rs->begin(); r != rs->end(); ++r) {
    if (directlyDerivesNull(*r)) {
      return true;
    }
  }
  return false;
}

bool derivesNull(const terminalset& nullAggs, const rule& r) {
  for (rule::const_iterator v = r.begin(); v != r.end(); ++v) {
    if (nullAggs.find(*v) == nullAggs.end()) {
      return false;
    }
  }
  return true;
}

bool derivesNull(const grammar& g, const terminalset& nullAggs, terminal* s) {
  const rules* rs = aggRules(g, s);
  for (rules::const_iterator r = rs->begin(); r != rs->end(); ++r) {
    if (derivesNull(nullAggs, *r)) {
      return true;
    }
  }
  return false;
}

bool derivesNull(const grammar& g, const terminalset& nullAggs, const terminals& ss) {
  for (terminals::const_iterator s = ss.begin(); s != ss.end(); ++s) {
    if (!derivesNull(g, nullAggs, *s)) {
      return false;
    }
  }
  return true;
}

bool derivesNull(const parserdef& p, terminal* s) {
  return p.nullable.find(s) != p.nullable.end();
}

terminalset symbolsDerivingNull(const grammar& g) {
  terminalset nulls;
  terminalset potentials = definedSymbols(g);
  bool        changed    = true;

  while (changed) {
    changed = false;
    for (terminalset::const_iterator s = potentials.begin(); s != potentials.end(); ++s) {
      if (nulls.find(*s) == nulls.end() && derivesNull(g, nulls, *s)) {
        nulls.insert(*s);
        potentials.erase(*s);
        changed = true;
      }
    }
  }
  return nulls;
}

/*
 * LR(0) parser determination
 */
item::item(terminal* agg, nat p, nat r, const terminalset& la) : agg(agg), p(p), r(r), la(la) { }

bool item::operator<(const item& rhs) const {
  if (this->agg != rhs.agg) {
    return this->agg < rhs.agg;
  } else if (this->p != rhs.p) {
    return this->p < rhs.p;
  } else {
    return this->r < rhs.r;
  }
}

bool item::operator==(const item& rhs) const {
  return this->agg == rhs.agg &&
       this->p   == rhs.p   &&
       this->r   == rhs.r;
}

void show(std::ostream& out, const grammar& g, const item& i) {
  i.agg->show(out);
  out << " -> ";
  const rule& r = nthRule(aggRules(g, i.agg), i.r);
  for (nat k = 0; k < r.size(); ++k) {
    if (k == i.p) {
      out << "#";
    }
    r[k]->show(out);
  }
  if (i.p == r.size()) {
    out << "#";
    out << " [";
    if (i.la.size() > 0) {
      terminalset::const_iterator lat = i.la.begin();
      (*lat)->show(out);
      ++lat;
      while (lat != i.la.end()) {
        out << ", ";
        (*lat)->show(out);
        ++lat;
      }
    }
    out << "]";
  }
  out << std::endl;
}

void show(std::ostream& out, const grammar& g, const itemset& is) {
  for (itemset::const_iterator i = is.begin(); i != is.end(); ++i) {
    show(out, g, *i);
  }
}

bool ruleItems(const grammar& g, terminal* t, itemset* is) {
  bool         inserted = false;
  const rules* rs       = aggRules(g, t);

  for (nat r = 0; r < rs->size(); ++r) {
    inserted |= is->insert(item(t, 0, r)).second;
  }

  return inserted;
}

itemset ruleItems(const grammar& g, terminal* t) {
  itemset is;
  ruleItems(g, t, &is);
  return is;
}

item succ(const item& i) {
  return item(i.agg, i.p+1, i.r, i.la);
}

terminal* next(const grammar& g, const item& i) {
  return ruleElement(nthRule(aggRules(g, i.agg), i.r), i.p);
}

terminalset next(const grammar& g, const itemset& is) {
  terminalset r;
  for (itemset::const_iterator i = is.begin(); i != is.end(); ++i) {
    if (terminal* t = next(g, *i)) {
      r.insert(t);
    }
  }
  return r;
}

terminalset nextPrim(const grammar& g, const itemset& is) {
  terminalset r;
  for (itemset::const_iterator i = is.begin(); i != is.end(); ++i) {
    if (terminal* v = next(g, *i)) {
      if (!hasRule(g, v)) {
        r.insert(v);
      }
    }
  }
  return r;
}

itemset follow(const grammar& g, const itemset& is, terminal* v) {
  itemset r;
  for (itemset::const_iterator i = is.begin(); i != is.end(); ++i) {
    if (next(g, *i) == v) {
      r.insert(succ(*i));
    }
  }
  return r;
}

itemsets follow(const grammar& g, const itemset& is, const terminals& vs) {
  itemsets r;
  for (terminals::const_iterator v = vs.begin(); v != vs.end(); ++v) {
    r.push_back(follow(g, is, *v));
  }
  return r;
}

void gclosure(const grammar& g, itemset* is) {
  // add implied items to a fixed point
  bool changed = true;
  while (changed) {
    changed = false;

    terminalset ts = next(g, *is);
    for (terminalset::const_iterator t = ts.begin(); t != ts.end(); ++t) {
      if (*t && hasRule(g, *t)) {
        changed |= ruleItems(g, *t, is);
      }
    }
  }
}

itemset gclosure(const grammar& g, const itemset& is) {
  itemset r = is;
  gclosure(g, &r);
  return r;
}

nat parserState(const itemset& s, parserdef* p) {
  parser_states::const_iterator si = p->states.find(s);
  if (si != p->states.end()) {
    return si->second;
  } else {
    nat n = p->states.size();
    p->states    [s] = n;
    p->state_defs[n] = s;
    return n;
  }
}

const itemset& parserStateDef(const parserdef& p, nat n) {
  state_definitions::const_iterator sd = p.state_defs.find(n);
  if (sd == p.state_defs.end()) {
    throw std::runtime_error("Internal error, invalid state referenced.");
  } else {
    return sd->second;
  }
}

state_transitions follow(const grammar& g, const itemset& is, const terminalset& vs, parserdef* p) {
  state_transitions r;
  for (terminalset::const_iterator v = vs.begin(); v != vs.end(); ++v) {
    if (*v != endOfFile::value()) {
      r[*v] = parserState(gclosure(g, follow(g, is, *v)), p);
    }
  }
  return r;
}

parserdef lr0parser(const grammar& g, terminal* s) {
  parserdef p;

  // tweak this grammar to have a boot rule
  // s' -> s $
  p.g = g;
  p.s = s;

  static symbol privstart(".S");
  terminal* boot = &privstart;
  rule br; br.push_back(s); br.push_back(endOfFile::value());
  p.g[boot].push_back(br);

  // enumerate the state space implied by this grammar
  nat n = parserState(gclosure(p.g, ruleItems(p.g, boot)), &p);
  while (n < p.states.size()) {
    const itemset& nis = parserStateDef(p, n);
    p.transitions[n] = follow(p.g, nis, next(p.g, nis), &p);
    ++n;
  }
  p.nullable = symbolsDerivingNull(p.g);
  return p;
}

/*
 * Relations on the LR(0) parser and LALR(1) determination
 */

transitionset transitions(const parserdef& p) {
  transitionset r;
  for (parser_state_transitions::const_iterator pst = p.transitions.begin(); pst != p.transitions.end(); ++pst) {
    for (state_transitions::const_iterator st = pst->second.begin(); st != pst->second.end(); ++st) {
      if (hasRule(p.g, st->first)) {
        r.insert(transition(pst->first, st->first));
      }
    }
  }
  return r;
}

const state_transitions& transitions(const parserdef& p, nat q) {
  parser_state_transitions::const_iterator r = p.transitions.find(q);
  if (r == p.transitions.end()) {
    throw std::runtime_error("Internal error, invalid state given for finding transitions.");
  } else {
    return r->second;
  }
}

bool follows(const state_transitions& st, terminal* t) {
  return st.find(t) != st.end();
}

bool follows(const parserdef& p, nat q, terminal* t) {
  return follows(transitions(p, q), t);
}

nat follow(const state_transitions& st, terminal* t) {
  state_transitions::const_iterator x = st.find(t);
  if (x == st.end()) {
    throw std::runtime_error("Internal error, invalid symbol given for determining state transition.");
  } else {
    return x->second;
  }
}

nat follow(const parserdef& p, nat q, terminal* t) {
  return follow(transitions(p, q), t);
}

nat follow(const parserdef& p, const transition& x) {
  return follow(p, x.first, x.second);
}

nat follow(const parserdef& p, nat s, const terminals& ts) {
  nat r = s;
  for (terminals::const_iterator t = ts.begin(); t != ts.end(); ++t) {
    r = follow(p, r, *t);
  }
  return r;
}

// check if a transition "looks back" to a state where a given rule has been completed
bool lookback(const parserdef& p, nat q, terminal* r, const rule& w, const transition& t) {
  if (t.second != r) {
    return false;
  } else {
    nat s = t.first;
    for (rule::const_iterator t = w.begin(); t != w.end(); ++t) {
      const state_transitions& st = transitions(p, s);
      if (follows(st, *t)) {
        s = follow(st, *t);
      } else {
        return false;
      }
    }
    return s == q;
  }
}

// for a rule w and sym A, find all prefixes b such that w = bAs, s =>* eps
typedef std::vector<terminals> prefixes;

prefixes endingAt(const parserdef& p, const rule& w, terminal* a) {
  prefixes r;
  nat i = w.size();
  while (i > 0) {
    --i;

    if (w[i] == a) {
      r.push_back(terminals(w.begin(), w.begin() + i));
    }
    if (!derivesNull(p, w[i])) {
      break;
    }
  }
  return r;
}

// x=(s,A) includes y=(s',B) if B -> wAg, g =>* eps, and s' --w--> s
bool includes(const parserdef& p, const transition& x, const transition& y) {
  const rules* rs = aggRules(p.g, y.second);
  for (rules::const_iterator r = rs->begin(); r != rs->end(); ++r) {
    prefixes pr = endingAt(p, *r, x.second);
    for (prefixes::const_iterator pfx = pr.begin(); pfx != pr.end(); ++pfx) {
      if (follow(p, y.first, *pfx) == x.first) {
        return true;
      }
    }
  }
  return false;
}

// (s,A) reads (s',B) if s --A--> s' and B =>* eps
bool reads(const parserdef& p, const transition& x, const transition& y) {
  return follow(p, x) == y.first && derivesNull(p, y.second);
}

// from x=(s,A), find all non-aggregate terminals immediately expected after transitioning from state s to s' along A
terminalset directReads(const parserdef& p, const transition& x) {
  return nextPrim(p.g, parserStateDef(p, follow(p, x)));
}

// a utility to make it easier to create closures
template <typename E, typename X0, typename R>
  struct clos1 {
    typedef R (*PFN)(const E&, const X0&);
    const E& env;
    PFN      fn;
    
    clos1(const E& env, PFN fn) : env(env), fn(fn) { }
    R operator()(const X0& x0) const { return fn(env, x0); }
  };

template <typename E, typename X0, typename X1, typename R>
  struct clos2 {
    typedef R (*PFN)(const E&, const X0&, const X1&);
    const E& env;
    PFN      fn;

    clos2(const E& env, PFN fn) : env(env), fn(fn) { }
    R operator()(const X0& x0, const X1& x1) const { return fn(env, x0, x1); }
  };

// typedefs to work around type inference failure for the below digraph expressions
typedef terminalset (*ptsappend)(const terminalset&, const terminalset&);

typedef clos1<parserdef, transition,             terminalset> DRT;
typedef clos2<parserdef, transition, transition, bool>        readsT;

typedef clos1<transition_lookahead, transition,             terminalset> RT;
typedef clos2<parserdef,            transition, transition, bool>        includesT;

// compute Read(x=(s,A)) = directReads(s,A) + (Read(q,B) | (s,A) reads (q,B))
transition_lookahead Reads(const parserdef& p, const transitionset& ts) {
  return digraphF<transition, terminalset, DRT, ptsappend, readsT>::map(ts, DRT(p, &directReads), &setUnion, readsT(p, &reads));
}

// compute Follow(x=(s,A)) = Read((s,A)) + (Follow(q,B) | (s,A) includes (q,B))
transition_lookahead Follow(const parserdef& p, const transitionset& ts) {
  transition_lookahead r = Reads(p, ts);
  return digraphF<transition, terminalset, RT, ptsappend, includesT>::map(ts, RT(r, &mapLookup), &setUnion, includesT(p, &includes));
}

// apply transition lookahead to every applicable completed item in an LR(0) parser
void apply(const transition_lookahead& tl, parserdef* p) {
  for (parser_states::iterator s = p->states.begin(); s != p->states.end(); ++s) {
    for (itemset::iterator i = s->first.begin(); i != s->first.end(); ++i) {
      // if this is a completed item, then we need to give it lookahead terminals
      if (next(p->g, *i) == 0) {
        for (transition_lookahead::const_iterator tli = tl.begin(); tli != tl.end(); ++tli) {
          if (lookback(*p, s->second, i->agg, nthRule(aggRules(p->g, i->agg), i->r), tli->first)) {
            const_cast<item*>(&(*(i)))->la.insert(tli->second.begin(), tli->second.end());
          }
        }
      }
    }
  }
}

// determine the LALR(1) parser definition
parserdef lalr1parser(const grammar& g, terminal* s) {
  parserdef     p  = lr0parser(g, s);
  transitionset ts = transitions(p);

  apply(Follow(p, ts), &p);
  return p;
}

terminal* reduceOpTerminal(const parserdef& pd, const action& a) {
  const rule& r = nthRule(aggRules(pd.g, a.reduceSym()), a.reduceRule());
  for (rule::const_reverse_iterator e = r.rbegin(); e != r.rend(); ++e) {
    if (!hasRule(pd.g, *e)) {
      return *e;
    }
  }
  return 0;
}

bool shiftInstead(const parserdef& pd, const precedence& px, nat, terminal* t, const action& a) {
  terminal* u = reduceOpTerminal(pd, a);

  if (!u) {
    throw ambiguity_conflict("Unable to resolve shift/reduce conflict in grammar", t);
  }

  precedence::const_iterator tp = px.find(t);
  precedence::const_iterator up = px.find(u);

  if (tp == px.end() || up == px.end()) {
    throw ambiguity_conflict("Unable to resolve shift/reduce conflict in grammar", t);
  }

  if (tp->second.level < up->second.level) {
    return false;
  } else if (up->second.level < tp->second.level) {
    return true;
  } else if (tp->second.asc == up->second.asc && tp->second.asc != assoc::non) {
    if (tp->second.asc == assoc::left) {
      return false;
    } else {
      return true;
    }
  } else {
    throw ambiguity_conflict("Unable to resolve shift/reduce conflict in grammar", t);
  }
}

// determine the LALR(1) table to drive an LR parser
lrtable lalrTable(const grammar& g, terminal* s, const precedence& px) {
  return lalrTable(lalr1parser(g, s), px);
}

void addAction(const parserdef& pd, lrstate& s, terminal* t, const action& a, const precedence& px) {
  lrstate::iterator sc = s.find(t);
  if (sc == s.end()) {
    s.insert(std::make_pair(t, a));
  } else if (sc->second != a) {
    if (a.isReduce() && sc->second.isReduce()) {
      std::ostringstream ss;
      ss << "Conflict between " << a << " and " << sc->second << " on " << show(t);
      throw ambiguity_conflict(ss.str(), t);
    } else if (a.isShift()) {
      if (shiftInstead(pd, px, a.shiftState(), t, sc->second)) {
        sc->second = a;
      }
    } else {
      if (!shiftInstead(pd, px, sc->second.shiftState(), t, a)) {
        sc->second = a;
      }
    }
  }
}

void addAccept(lrstate& s, terminal* t) {
  s.insert(std::make_pair(t, action::accept()));
}

void addReduce(const parserdef& pd, lrstate& s, const terminalset& ts, terminal* agg, nat rule, nat len, const precedence& px) {
  for (terminalset::const_iterator t = ts.begin(); t != ts.end(); ++t) {
    addAction(pd, s, *t, action::reduce(agg, rule, len), px);
  }
}

void addGoto(lrstate& s, terminal* nt, nat next) {
  s.insert(std::make_pair(nt, action::goTo(next)));
}

void addShift(const parserdef& pd, lrstate& s, terminal* t, nat next, const precedence& px) {
  addAction(pd, s, t, action::shift(next), px);
}

lrtable lalrTable(const parserdef& p, const precedence& px) {
  lrtable r;
  r.resize(p.states.size());

  for (parser_states::const_iterator s = p.states.begin(); s != p.states.end(); ++s) {
    // each item in this state's set translates to an LR parser action (goto, shift, reduce, or accept)
    const itemset& is = s->first;
    nat            n  = s->second;
    lrstate&       ps = r[n];

    // if we fail to compile a table row, report the itemset that failed
    try {
      for (itemset::const_iterator i = is.begin(); i != is.end(); ++i) {
        terminal* t = next(p.g, *i);
  
        if (t == 0) {
          // reduce
          addReduce(p, ps, i->la, i->agg, i->r, i->p, px);
        } else if (hasRule(p.g, t)) {
          // goto
          addGoto(ps, t, follow(p, n, t));
        } else if (t == endOfFile::value()) {
          // accept
          addAccept(ps, t);
        } else {
          // shift
          addShift(p, ps, t, follow(p, n, t), px);
        }
      }
    } catch (ambiguity_conflict& ex) {
      throw compile_table_failure(ex.what(), p.g, is, ex.failedTerminal());
    }
  }

  return r;
}

ambiguity_conflict::ambiguity_conflict(const std::string& ex, terminal* t) throw() : std::runtime_error(ex), t(t) {
}

terminal* ambiguity_conflict::failedTerminal() const {
  return this->t;
}

compile_table_failure::compile_table_failure(const std::string& msg, const grammar& g, const itemset& faileditems, terminal* t) throw() : std::runtime_error(msg), g(g), faileditems(faileditems), t(t) {
}

const grammar& compile_table_failure::failedGrammar() const {
  return this->g;
}

const itemset& compile_table_failure::failedItems() const {
  return this->faileditems;
}

terminal* compile_table_failure::failedTerminal() const {
  return this->t;
}

inline bool referencesSym(const grammar& g, const item& i, terminal* t) {
  return in(t, nthRule(aggRules(g, i.agg), i.r)) || in(t, i.la);
}

void compile_table_failure::print(std::ostream& out) const {
  for (itemset::const_iterator i = this->faileditems.begin(); i != this->faileditems.end(); ++i) {
    if (referencesSym(this->g, *i, this->t)) {
      print(out, *i);
    }
  }
}

void beginHighlight(std::ostream& out) {
  static const char bgccmd[] = { 27, '[', '4', '1', 'm' };
  static const char fgccmd[] = { 27, '[', '3', '7', 'm' };

  out.write(bgccmd, sizeof(bgccmd));
  out.write(fgccmd, sizeof(fgccmd));
}

void endHighlight(std::ostream& out) {
  static const char rstcmd[] = { 27, '[', '0',  'm' };

  out.write(rstcmd, sizeof(rstcmd));
}

inline void showHLTerm(std::ostream& out, terminal* x, terminal* hx) {
  if (x == hx) {
    beginHighlight(out);
    x->show(out);
    endHighlight(out);
  } else {
    x->show(out);
  }
}

void compile_table_failure::print(std::ostream& out, const item& i) const {
  i.agg->show(out);
  out << " ->";

  const rule& r = nthRule(aggRules(this->g, i.agg), i.r);
  for (nat k = 0; k < r.size(); ++k) {
    out << ' ';
    if (k == i.p) {
      out << '#';
      showHLTerm(out, r[k], this->t);
    } else {
      r[k]->show(out);
    }
  }
  if (i.p == r.size()) {
    out << "#";
    out << " [";
    if (i.la.size() > 0) {
      terminalset::const_iterator lat = i.la.begin();
      showHLTerm(out, *lat, this->t);
      ++lat;
      while (lat != i.la.end()) {
        out << ", ";
        showHLTerm(out, *lat, this->t);
        ++lat;
      }
    }
    out << "]";
  }
  out << std::endl;
}

// lr tables / actions
action::action() {
}

action action::goTo(nat s) {
  action a;
  a.act = 0;
  a.d.s = s;
  return a;
}
bool action::isGoTo()    const { return this->act == 0; }
nat  action::goToState() const { return this->d.s; }

action action::shift(nat s) {
  action a;
  a.act = 1;
  a.d.s = s;
  return a;
}
bool action::isShift()    const { return this->act == 1; }
nat  action::shiftState() const { return this->d.s; }

action action::reduce(terminal* x, nat r, nat n) {
  action a;
  a.act = 2;
  a.d.x = x;
  a.d.r = r;
  a.d.n = n;
  return a;
}
bool      action::isReduce()   const { return this->act == 2; }
terminal* action::reduceSym()  const { return this->d.x; }
nat       action::reduceRule() const { return this->d.r; }
nat       action::reduceSize() const { return this->d.n; }

action action::accept() {
  action a;
  a.act = 3;
  return a;
}
bool action::isAccept() const { return this->act == 3; }

std::ostream& action::fmt(std::ostream& out) const {
  switch (this->act) {
  case 0: out << this->d.s; break;
  case 1: out << "s" << this->d.s; break;
  case 2: out << "r" << this->d.r; break;
  case 3: out << "A"; break;
  }
  return out;
}

std::ostream& operator<<(std::ostream& out, const action& act) {
  return act.fmt(out);
}

bool action::operator==(const action& rhs) const {
  if (this->act != rhs.act) {
    return false;
  } else {
    switch (this->act) {
    default:
    case 0: return rhs.d.s == this->d.s;
    case 1: return rhs.d.s == this->d.s;
    case 2: return rhs.d.x == this->d.x && rhs.d.r == this->d.r && rhs.d.n == this->d.n;
    case 3: return true;
    }
  }
}

bool action::operator!=(const action& rhs) const {
  return !(*this == rhs);
}

terminalset definedSymbols(const lrtable& t) {
  terminalset r;
  for (const auto& s : t) {
    for (const auto& k : s) {
      r.insert(k.first);
    }
  }
  return r;
}

terminals dispOrd(const terminalset& ts) {
  terminals r(ts.begin(), ts.end());
  std::sort(r.begin(), r.end(),
    [](terminal* a, terminal* b) {
      if (a == endOfFile::value()) {
        return b != endOfFile::value();
      } else if (b == endOfFile::value()) {
        return false;
      } else if (const character* c = dynamic_cast<character*>(a)) {
        if (const character* z = dynamic_cast<character*>(b)) {
          return c->value() < z->value();
        } else {
          return true;
        }
      } else if (dynamic_cast<character*>(b)) {
        return false;
      } else {
        std::ostringstream as, bs;
        a->show(as);
        b->show(bs);
        return as.str() < bs.str();
      }
    }
  );
  return r;
}

void show(std::ostream& out, const lrtable& tbl) {
  auto      ts = dispOrd(definedSymbols(tbl));
  str::seqs stbl;
  for (auto t : ts) {
    stbl.push_back(str::seq());
    str::seq& c = stbl.back();

    std::ostringstream ss;
    t->show(ss);
    c.push_back(ss.str());

    for (const auto& s : tbl) {
      auto ki = s.find(t);
      if (ki == s.end()) {
        c.push_back("");
      } else {
        std::ostringstream ss;
        ki->second.fmt(ss);
        c.push_back(ss.str());
      }
    }
  }
  str::printRightAlignedTable(out, stbl);
}

}

