
#include <hobbes/parse/lalr.H>
#include <algorithm>
#include <stdexcept>
#include <string>
#include <vector>

namespace hobbes {

/*
 * basic grammar/rule analysis
 */
void removeRuleDefs(terminal* s, grammar* g) {
  auto r = g->find(s);
  if (r != g->end()) {
    g->erase(r);
  }
}

void removeRuleRefs(terminal* s, grammar* g) {
  for (auto &sd : *g) {
    for (nat r = 0; r < sd.second.size();) {
      if (in(s, sd.second[r])) {
        sd.second.erase(sd.second.begin() + r);
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

  for (auto *dsym : dsyms) {
    terminalset users = symbolsUsing(g, dsym);
    if (users.empty() || (users.size() == 1 && in(dsym, users))) {
      result.insert(dsym);
    }
  }

  return result;
}

void symbolsUsed(const grammar& g, const rule& r, terminalset* ss) {
  for (auto *t : r) {
    if (hasRule(g, t)) {
      if (!in(t, *ss)) {
        ss->insert(t);
        symbolsUsed(g, t, ss);
      }
    }
  }
}

void symbolsUsed(const grammar& g, const rules& rs, terminalset* ss) {
  for (const auto &r : rs) {
    symbolsUsed(g, r, ss);
  }
}

void symbolsUsed(const grammar& g, terminal* s, terminalset* ss) {
  auto sd = g.find(s);
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

  for (auto *dsym : dsyms) {
    if (in(s, symbolsUsed(g, dsym))) {
      result.insert(dsym);
    }
  }

  return result;
}

bool hasRule(const grammar& g, terminal* t) {
  return g.find(t) != g.end();
}

const rules* aggRules(const grammar& g, terminal* t) {
  auto ad = g.find(t);
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
    return nullptr;
  } else {
    return r[i];
  }
}

bool directlyDerivesNull(const rule& r) {
  return r.empty();
}

bool directlyDerivesNull(const grammar& g, terminal* s) {
  const rules* rs = aggRules(g, s);
  for (const auto &r : *rs) {
    if (directlyDerivesNull(r)) {
      return true;
    }
  }
  return false;
}

bool derivesNull(const terminalset& nullAggs, const rule& r) {
  for (auto *v : r) {
    if (nullAggs.find(v) == nullAggs.end()) {
      return false;
    }
  }
  return true;
}

bool derivesNull(const grammar& g, const terminalset& nullAggs, terminal* s) {
  const rules* rs = aggRules(g, s);
  for (const auto & r : *rs) {
    if (derivesNull(nullAggs, r)) {
      return true;
    }
  }
  return false;
}

bool derivesNull(const grammar& g, const terminalset& nullAggs, const terminals& ss) {
  for (auto *s : ss) {
    if (!derivesNull(g, nullAggs, s)) {
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
    // erasing the element an iterator refers to invalidates that iterator, so
    // advance from what erase returns rather than incrementing the dead one
    for (auto s = potentials.begin(); s != potentials.end(); ) {
      if (nulls.find(*s) == nulls.end() && derivesNull(g, nulls, *s)) {
        nulls.insert(*s);
        s = potentials.erase(s);
        changed = true;
      } else {
        ++s;
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
    if (!i.la.empty()) {
      auto lat = i.la.begin();
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
  for (const auto &i : is) {
    show(out, g, i);
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
  for (const auto &i : is) {
    if (terminal* t = next(g, i)) {
      r.insert(t);
    }
  }
  return r;
}

terminalset nextPrim(const grammar& g, const itemset& is) {
  terminalset r;
  for (const auto &i : is) {
    if (terminal* v = next(g, i)) {
      if (!hasRule(g, v)) {
        r.insert(v);
      }
    }
  }
  return r;
}

itemset follow(const grammar& g, const itemset& is, terminal* v) {
  itemset r;
  for (const auto &i : is) {
    if (next(g, i) == v) {
      r.insert(succ(i));
    }
  }
  return r;
}

itemsets follow(const grammar& g, const itemset& is, const terminals& vs) {
  itemsets r;
  for (auto *v : vs) {
    r.push_back(follow(g, is, v));
  }
  return r;
}

void gclosure(const grammar& g, itemset* is) {
  // add implied items to a fixed point
  //   (each nonterminal is expanded once, and only the items it adds are looked at again,
  //    rather than the whole set once per round)
  terminalset              expanded;
  std::vector<const item*> pending;
  for (const auto& i : *is) {
    pending.push_back(&i);
  }

  while (!pending.empty()) {
    terminal* t = next(g, *pending.back());
    pending.pop_back();

    if ((t != nullptr) && hasRule(g, t) && expanded.insert(t).second) {
      const rules* rs = aggRules(g, t);
      for (nat r = 0; r < rs->size(); ++r) {
        auto ins = is->insert(item(t, 0, r));
        if (ins.second) {
          pending.push_back(&(*ins.first));
        }
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
  auto sd = p.state_defs.find(n);
  if (sd == p.state_defs.end()) {
    throw std::runtime_error("Internal error, invalid state referenced.");
  } else {
    return sd->second;
  }
}

parser_too_large::parser_too_large(limit l, size_t bound) :
  std::runtime_error(
    l == limit::items     ? "parser states hold more than " + std::to_string(bound) + " items" :
    l == limit::lookahead ? "parser lookahead holds more than " + std::to_string(bound) + " terminals" :
                            "parser states' stack depths sum to more than " + std::to_string(bound)),
  l(l), b(bound) {
}
parser_too_large::limit parser_too_large::which() const { return this->l; }
size_t parser_too_large::bound() const { return this->b; }

namespace {

// counts the size of an LR(0) parser as its states are made, giving up once it is past its limits
//   (checked as each state is made, not as it is visited: a state can have as many successors
//    as the grammar has rules, each of them as large as the grammar, so the parser can grow
//    quadratically in the grammar before the next state is visited)
class lr0budget {
public:
  explicit lr0budget(const parserlimits& limits) : limits(limits) { }

  // the state reached from a kernel of items (the successors of some state's items on one symbol)
  //   (a state is found by its kernel before its closure is made: many states can reach one
  //    state, and making its closure again for each of them took time quadratic in the grammar;
  //    a kernel's items have all read something, and its closure adds only items that haven't,
  //    so kernels and the states made from them determine each other)
  nat successor(const grammar& g, itemset* kernel, parserdef* p) {
    auto k = this->kernels.find(*kernel);
    if (k != this->kernels.end()) {
      return k->second;
    }
    itemset ks = *kernel;
    gclosure(g, kernel);
    nat r = state(*kernel, p);
    this->kernels[std::move(ks)] = r;
    return r;
  }

  nat state(const itemset& s, parserdef* p) {
    size_t n = p->states.size();
    nat    r = parserState(s, p);
    if (p->states.size() != n) {
      nat d = 0;
      for (const auto& i : s) {
        d = std::max(d, i.p);
      }
      this->depthSum += d;
      this->items    += s.size();

      if (this->depthSum > this->limits.maxStackDepthSum) {
        throw parser_too_large(parser_too_large::limit::stackDepthSum, this->limits.maxStackDepthSum);
      }
      if (this->items > this->limits.maxItems) {
        throw parser_too_large(parser_too_large::limit::items, this->limits.maxItems);
      }
    }
    return r;
  }
private:
  const parserlimits&     limits;
  size_t                  depthSum = 0;
  size_t                  items    = 0;
  std::map<itemset, nat>  kernels;
};

state_transitions follow(const grammar& g, const itemset& is, const terminalset& vs, parserdef* p, lr0budget* b) {
  // sort the items by the symbol they expect in one pass,
  // rather than passing over all of them for each symbol
  std::map<terminal*, itemset> succs;
  for (const auto& i : is) {
    terminal* v = next(g, i);
    if (v != nullptr && vs.count(v) != 0) {
      succs[v].insert(succ(i));
    }
  }

  // and number the states they make in the order of vs
  state_transitions r;
  for (auto *v : vs) {
    if (v != endOfFile::value()) {
      r[v] = b->successor(g, &succs[v], p);
    }
  }
  return r;
}

}

state_transitions follow(const grammar& g, const itemset& is, const terminalset& vs, parserdef* p) {
  static const parserlimits unlimited;
  lr0budget b(unlimited);
  return follow(g, is, vs, p, &b);
}

parserdef lr0parser(const grammar& g, terminal* s) {
  return lr0parser(g, s, parserlimits());
}

parserdef lr0parser(const grammar& g, terminal* s, const parserlimits& limits) {
  parserdef p;
  lr0budget b(limits);

  // tweak this grammar to have a boot rule
  // s' -> s $
  p.g = g;
  p.s = s;

  static symbol privstart(".S");
  terminal* boot = &privstart;
  rule br; br.push_back(s); br.push_back(endOfFile::value());
  p.g[boot].push_back(br);

  // enumerate the state space implied by this grammar
  nat n = b.state(gclosure(p.g, ruleItems(p.g, boot)), &p);
  while (n < p.states.size()) {
    const itemset& nis = parserStateDef(p, n);
    p.transitions[n] = follow(p.g, nis, next(p.g, nis), &p, &b);
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
  for (auto pst = p.transitions.begin(); pst != p.transitions.end(); ++pst) {
    for (auto st = pst->second.begin(); st != pst->second.end(); ++st) {
      if (hasRule(p.g, st->first)) {
        r.insert(transition(pst->first, st->first));
      }
    }
  }
  return r;
}

const state_transitions& transitions(const parserdef& p, nat q) {
  auto r = p.transitions.find(q);
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
  auto x = st.find(t);
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
  for (auto *t : ts) {
    r = follow(p, r, t);
  }
  return r;
}

// check if a transition "looks back" to a state where a given rule has been completed
bool lookback(const parserdef& p, nat q, terminal* r, const rule& w, const transition& t) {
  if (t.second != r) {
    return false;
  } else {
    nat s = t.first;
    for (auto *t : w) {
      const state_transitions& st = transitions(p, s);
      if (follows(st, t)) {
        s = follow(st, t);
      } else {
        return false;
      }
    }
    return s == q;
  }
}

// for a rule w and sym A, find all prefixes b such that w = bAs, s =>* eps
using prefixes = std::vector<terminals>;

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
  for (const auto &r : *rs) {
    prefixes pr = endingAt(p, r, x.second);
    for (const auto & pfx : pr) {
      if (follow(p, y.first, pfx) == x.first) {
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

// the relations above (reads, includes, lookback) define LALR(1) lookahead,
// but deciding them one pair at a time costs a search over every pair of
// transitions (and, for lookback, over every completed item besides): more
// than a minute, under a sanitizer, for a `parse {}` grammar of a few
// thousand states (OSS-Fuzz 571908642).
// The edges of each relation can instead be built forward from each
// transition, as DeRemer and Pennello do, in time linear in the size of the
// LR(0) parser, which is what the functions below do.
namespace {

// the nonterminal transitions of a parser, numbered in the order of a
// transitionset, with each relation as a list of edges between those numbers
struct transitionIndex {
  std::vector<transition> ts;
  std::map<transition, nat> idx;

  explicit transitionIndex(const transitionset& xs) : ts(xs.begin(), xs.end()) {
    for (nat i = 0; i < this->ts.size(); ++i) {
      this->idx[this->ts[i]] = i;
    }
  }

  // the number of a transition, or ts.size() if it isn't one
  nat find(nat s, terminal* t) const {
    auto i = this->idx.find(transition(s, t));
    return (i == this->idx.end()) ? static_cast<nat>(this->ts.size()) : i->second;
  }
};

using edges = std::vector<std::vector<nat>>;

// list each node's edges in transitionset order, without repeats
void normalize(edges* es) {
  for (auto& e : *es) {
    std::sort(e.begin(), e.end());
    e.erase(std::unique(e.begin(), e.end()), e.end());
  }
}

// (s,A) reads (s',B) if s --A--> s' and B =>* eps
//   (which depends only on s', so the edges are found once for each state that transitions lead
//    to, rather than once for each transition: many transitions can lead to one state, which can
//    have many transitions out of it)
edges readsEdges(const parserdef& p, const transitionIndex& ti) {
  edges r(p.states.size());
  std::vector<bool> done(p.states.size(), false);
  for (const auto& x : ti.ts) {
    nat s = follow(p, x);
    if (s < done.size() && !done[s]) {
      done[s] = true;
      for (const auto& st : transitions(p, s)) {
        if (derivesNull(p, st.first)) {
          nat y = ti.find(s, st.first);
          if (y < ti.ts.size()) {
            r[s].push_back(y);
          }
        }
      }
    }
  }
  normalize(&r);
  return r;
}

// x=(s,A) includes y=(s',B) if B -> wAg, g =>* eps, and s' --w--> s
//   so walk each rule of B from s', and take the edge at each A in its nullable tail
edges includesEdges(const parserdef& p, const transitionIndex& ti) {
  edges r(ti.ts.size());
  std::vector<nat> path;
  for (nat y = 0; y < ti.ts.size(); ++y) {
    for (const auto& w : *aggRules(p.g, ti.ts[y].second)) {
      // the states before each symbol (but the last, after which nothing is looked for)
      path.resize(w.size());
      nat s = ti.ts[y].first;
      for (nat i = 0; i < w.size(); ++i) {
        path[i] = s;
        if (i + 1 < w.size()) {
          s = follow(p, s, w[i]);
        }
      }

      for (nat i = w.size(); i > 0;) {
        --i;
        nat x = ti.find(path[i], w[i]);
        if (x < ti.ts.size()) {
          r[x].push_back(y);
        }
        if (!derivesNull(p, w[i])) {
          break;
        }
      }
    }
  }
  normalize(&r);
  return r;
}

// counts the lookahead terminals computed for a parser, giving up once there are too many
//   (the sets are as many as its transitions and items, each as large as its terminals, so
//    their size can be quadratic in a grammar that passes the bounds on its LR(0) states)
class lookaheadBudget {
public:
  explicit lookaheadBudget(size_t limit) : limit(limit) { }

  void resized(size_t before, size_t after) {
    this->n = this->n + after - before;
    if (this->n > this->limit) {
      throw parser_too_large(parser_too_large::limit::lookahead, this->limit);
    }
  }

  // add ts to s
  void insert(terminalset* s, const terminalset& ts) {
    size_t before = s->size();
    s->insert(ts.begin(), ts.end());
    resized(before, s->size());
  }
private:
  size_t limit;
  size_t n = 0;
};

// for each x, f(x) = fp(x) + sum(f(y) | x R y), as digraphF computes it
// (visiting nodes and edges in the same order, so with the same result),
// but with R given as edges, and without recursion: an edge chain can be as
// long as the parser has transitions.
//
// Many nodes can share one fp and one list of edges (all of the transitions
// to one state, for reads). Once one of them is done, having read only values
// that were themselves done (none of its edges went back into the stack), its
// value is what any other node with the same fp and edges would compute, so
// that is used rather than visiting the same edges again for each of them.
transition_lookahead digraph(const transitionIndex& ti, const std::vector<const terminalset*>& fp, const std::vector<const std::vector<nat>*>& es, lookaheadBudget* b) {
  static const nat inf = static_cast<nat>(-1);

  nat                      n = ti.ts.size();
  std::vector<nat>         depth(n, 0);
  std::vector<terminalset> f(n);
  std::vector<nat>         stack;

  using shape = std::pair<const terminalset*, const std::vector<nat>*>;
  std::map<shape, nat> doneShapes;

  struct frame { nat x; nat d; nat e; bool partial; };
  std::vector<frame> frames;

  auto enter = [&](nat x) {
    stack.push_back(x);
    nat d = stack.size();
    depth[x] = d;

    auto ds = doneShapes.find(shape(fp[x], es[x]));
    if (ds != doneShapes.end()) {
      f[x] = f[ds->second];
      b->resized(0, f[x].size());
      frames.push_back(frame{x, d, static_cast<nat>(es[x]->size()), false});
    } else {
      f[x] = *fp[x];
      b->resized(0, f[x].size());
      frames.push_back(frame{x, d, 0, false});
    }
  };

  for (nat root = 0; root < n; ++root) {
    if (depth[root] != 0) {
      continue;
    }
    enter(root);

    while (!frames.empty()) {
      frame& fr = frames.back();
      const auto& e = *es[fr.x];

      if (fr.e < e.size()) {
        nat y = e[fr.e];
        if (depth[y] == 0) {
          // visit y first, then come back to this edge to add in its value
          enter(y);
        } else {
          if (depth[y] != inf) {
            fr.partial = true; // y is still in the stack, so its value isn't done
          }
          b->insert(&f[fr.x], f[y]);
          ++fr.e;
        }
        continue;
      }

      // join all the nodes in this SCC
      nat  x       = fr.x;
      bool partial = fr.partial;
      if (depth[x] == fr.d) {
        while (true) {
          nat cx = stack.back();
          stack.pop_back();
          depth[cx] = inf;
          if (cx == x) {
            break;
          } else {
            size_t before = f[cx].size();
            f[cx] = f[x];
            b->resized(before, f[cx].size());
          }
        }
      }
      frames.pop_back();
      if (!partial) {
        doneShapes.insert(std::make_pair(shape(fp[x], es[x]), x));
      }

      // and add this node's value to the edge that led here
      if (!frames.empty()) {
        frame& pr = frames.back();
        b->insert(&f[pr.x], f[x]);
        ++pr.e;
      }
    }
  }

  transition_lookahead r;
  for (nat x = 0; x < n; ++x) {
    r[ti.ts[x]] = std::move(f[x]);
  }
  return r;
}

transition_lookahead Reads(const parserdef& p, const transitionIndex& ti, lookaheadBudget* b) {
  // the terminals read directly after a transition, and the transitions it reads,
  // depend only on the state that it leads to, so are found once for each such state
  std::vector<terminalset> drs(p.states.size());
  std::vector<bool>        done(p.states.size(), false);
  edges                    res = readsEdges(p, ti);

  std::vector<const terminalset*>      dr(ti.ts.size());
  std::vector<const std::vector<nat>*> es(ti.ts.size());
  for (nat x = 0; x < ti.ts.size(); ++x) {
    nat s = follow(p, ti.ts[x]);
    if (!done[s]) {
      done[s] = true;
      drs[s]  = directReads(p, ti.ts[x]);
    }
    dr[x] = &drs[s];
    es[x] = &res[s];
  }
  return digraph(ti, dr, es, b);
}

transition_lookahead Follow(const parserdef& p, const transitionIndex& ti, lookaheadBudget* b) {
  transition_lookahead rs = Reads(p, ti, b);
  std::vector<terminalset> rds;
  rds.reserve(ti.ts.size());
  for (const auto& x : ti.ts) {
    rds.push_back(std::move(rs[x]));
  }
  rs.clear();

  edges ies = includesEdges(p, ti);
  std::vector<const terminalset*>      fp(ti.ts.size());
  std::vector<const std::vector<nat>*> es(ti.ts.size());
  for (nat x = 0; x < ti.ts.size(); ++x) {
    fp[x] = &rds[x];
    es[x] = &ies[x];
  }
  return digraph(ti, fp, es, b);
}

// (q, A -> w.) looks back to (s,A) when s --w--> q, so walk each rule of A from s
void apply(const transition_lookahead& tl, parserdef* p, lookaheadBudget* b) {
  std::vector<itemset*> states(p->states.size(), nullptr);
  for (auto& s : p->states) {
    if (s.second < states.size()) {
      states[s.second] = const_cast<itemset*>(&s.first);
    }
  }

  for (const auto& tli : tl) {
    terminal*    a  = tli.first.second;
    const rules* rs = aggRules(p->g, a);
    for (nat r = 0; r < rs->size(); ++r) {
      const rule& w = (*rs)[r];
      nat  q  = tli.first.first;
      bool ok = true;
      for (auto* t : w) {
        const state_transitions& st = transitions(*p, q);
        auto n = st.find(t);
        if (n == st.end()) {
          ok = false;
          break;
        }
        q = n->second;
      }
      if (!ok || q >= states.size() || states[q] == nullptr) {
        continue;
      }

      auto i = states[q]->find(item(a, w.size(), r));
      if (i != states[q]->end()) {
        b->insert(&const_cast<item*>(&(*i))->la, tli.second);
      }
    }
  }
}

}

// compute Read(x=(s,A)) = directReads(s,A) + (Read(q,B) | (s,A) reads (q,B))
transition_lookahead Reads(const parserdef& p, const transitionset& ts) {
  lookaheadBudget b(static_cast<size_t>(-1));
  return Reads(p, transitionIndex(ts), &b);
}

// compute Follow(x=(s,A)) = Read((s,A)) + (Follow(q,B) | (s,A) includes (q,B))
transition_lookahead Follow(const parserdef& p, const transitionset& ts) {
  lookaheadBudget b(static_cast<size_t>(-1));
  return Follow(p, transitionIndex(ts), &b);
}

// apply transition lookahead to every applicable completed item in an LR(0) parser
void apply(const transition_lookahead& tl, parserdef* p) {
  lookaheadBudget b(static_cast<size_t>(-1));
  apply(tl, p, &b);
}

// determine the LALR(1) parser definition
void lalr1lookahead(parserdef* p) {
  lalr1lookahead(p, parserlimits());
}

void lalr1lookahead(parserdef* p, const parserlimits& limits) {
  lookaheadBudget b(limits.maxLookahead);
  apply(Follow(*p, transitionIndex(transitions(*p)), &b), p, &b);
}

parserdef lalr1parser(const grammar& g, terminal* s) {
  parserdef p = lr0parser(g, s);
  lalr1lookahead(&p);
  return p;
}

terminal* reduceOpTerminal(const parserdef& pd, const action& a) {
  const rule& r = nthRule(aggRules(pd.g, a.reduceSym()), a.reduceRule());
  for (auto e = r.rbegin(); e != r.rend(); ++e) {
    if (!hasRule(pd.g, *e)) {
      return *e;
    }
  }
  return nullptr;
}

bool shiftInstead(const parserdef& pd, const precedence& px, nat, terminal* t, const action& a) {
  terminal* u = reduceOpTerminal(pd, a);

  if (u == nullptr) {
    throw ambiguity_conflict("Unable to resolve shift/reduce conflict in grammar", t);
  }

  auto tp = px.find(t);
  auto up = px.find(u);

  if (tp == px.end() || up == px.end()) {
    throw ambiguity_conflict("Unable to resolve shift/reduce conflict in grammar", t);
  }

  if (tp->second.level < up->second.level) {
    return false;
  } else if (up->second.level < tp->second.level) {
    return true;
  } else if (tp->second.asc == up->second.asc && tp->second.asc != assoc::non) {
    return tp->second.asc != assoc::left;
  } else {
    throw ambiguity_conflict("Unable to resolve shift/reduce conflict in grammar", t);
  }
}

// determine the LALR(1) table to drive an LR parser
lrtable lalrTable(const grammar& g, terminal* s, const precedence& px) {
  return lalrTable(lalr1parser(g, s), px);
}

void addAction(const parserdef& pd, lrstate& s, terminal* t, const action& a, const precedence& px) {
  auto sc = s.find(t);
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
  for (auto *t : ts) {
    addAction(pd, s, t, action::reduce(agg, rule, len), px);
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

  for (auto s = p.states.begin(); s != p.states.end(); ++s) {
    // each item in this state's set translates to an LR parser action (goto, shift, reduce, or accept)
    const itemset& is = s->first;
    nat            n  = s->second;
    lrstate&       ps = r[n];

    // if we fail to compile a table row, report the itemset that failed
    try {
      for (const auto & i : is) {
        terminal* t = next(p.g, i);
  
        if (t == nullptr) {
          // reduce
          addReduce(p, ps, i.la, i.agg, i.r, i.p, px);
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

ambiguity_conflict::ambiguity_conflict(const std::string& ex, terminal* t) noexcept : std::runtime_error(ex), t(t) {
}

terminal* ambiguity_conflict::failedTerminal() const {
  return this->t;
}

compile_table_failure::compile_table_failure(const std::string& msg, const grammar& g, const itemset& faileditems, terminal* t) noexcept : std::runtime_error(msg), g(g), faileditems(faileditems), t(t) {
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
  for (const auto &faileditem : this->faileditems) {
    if (referencesSym(this->g, faileditem, this->t)) {
      print(out, faileditem);
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
    if (!i.la.empty()) {
      auto lat = i.la.begin();
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
action::action() = default;

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
      } else if (dynamic_cast<character*>(b) != nullptr) {
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
  for (auto *t : ts) {
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

