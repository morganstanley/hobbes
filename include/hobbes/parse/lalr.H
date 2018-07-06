/*
 * lalr : an LALR(1) parser generator for abstract terminals
 */

#ifndef HOBBES_PARSE_LALR_HPP_INCLUDED
#define HOBBES_PARSE_LALR_HPP_INCLUDED

#include <hobbes/parse/terminal.H>
#include <hobbes/parse/data.H>
#include <map>
#include <set>
#include <stack>

namespace hobbes {

// a grammar is a mapping from abstract terminals to the set of rules matching those terminals
typedef terminals                  rule;
typedef std::vector<rule>          rules;
typedef std::map<terminal*, rules> grammar;

// an item represents the state of recognizing a rule
struct item {
  item(terminal* agg, nat p, nat r, const terminalset& la = terminalset());

  terminal* agg; // the 'non-terminal'
  nat       p;   // the read position in the indexed rule
  nat       r;   // the indexed rule

  terminalset la; // filled by the LALR(1) determination for items representing lookahead reduce actions

  bool operator<(const item& rhs) const;
  bool operator==(const item& rhs) const;
};

// an itemset is a partial description of the state of a parser
//  (with no duplicate items)
typedef std::set<item>       itemset;
typedef std::vector<itemset> itemsets;

// transitions : a set of states to transition to indexed on terminals
typedef std::map<terminal*, nat> state_transitions;

// each closed itemset represents a numbered parser state
typedef std::map<itemset, nat> parser_states;

// the reverse parser state map
typedef std::map<nat, itemset> state_definitions;

// a set of state transitions map values to target parser states (by index)
typedef std::map<terminal*, nat> state_transitions;

// each state has its own set of state transitions
typedef std::map<nat, state_transitions> parser_state_transitions;

// a parserdef is an ordered sequence of states, along with the transitions defined for each state
struct parserdef {
  grammar                  g;
  terminal*                s;
  parser_states            states;
  state_definitions        state_defs;
  parser_state_transitions transitions;
  terminalset              nullable;
};

/*
 * parser algorithms
 */

// remove rules that define symbols, refer to symbols, or both
void removeRuleDefs(terminal* s, grammar* g);
void removeRuleRefs(terminal* s, grammar* g);
void undefineSymbol(terminal* s, grammar* g);

grammar removeRuleDefs(terminal* s, const grammar& g);
grammar removeRuleRefs(terminal* s, const grammar& g);
grammar undefineSymbol(terminal* s, const grammar& g);

// the set of symbols with rule definitions
terminalset definedSymbols(const grammar& g);

// the set of symbols
terminalset topLevelSymbols(const grammar& g);

// the set of symbols used by this symbol
void symbolsUsed(const grammar& g, const rule& r,   terminalset* ss);
void symbolsUsed(const grammar& g, const rules& rs, terminalset* ss);
void symbolsUsed(const grammar& g, terminal* s,     terminalset* ss);
terminalset symbolsUsed(const grammar& g, terminal* s);

// the set of symbols that use this symbol
terminalset symbolsUsing(const grammar& g, terminal* s);

// is this a 'terminal' or 'non-terminal'?
bool hasRule(const grammar& g, terminal* t);

// an aggregate symbol directly derives null if one of its rules does
bool directlyDerivesNull(const rule& r);
bool directlyDerivesNull(const grammar& g, terminal* s);

// an aggregate symbol derives null if it directly derives null or if it has a rule consisting of a path of aggregate symbols that derive null
bool derivesNull(const terminalset& nullAggs, const rule& r);
bool derivesNull(const grammar& g, const terminalset& nullAggs, terminal* s);
bool derivesNull(const grammar& g, const terminalset& nullAggs, const terminals& ss);
bool derivesNull(const parserdef& p, terminal* s);

// determine which aggregate symbols in a grammar are nullable (can derive the empty string) either directly or transitively
terminalset symbolsDerivingNull(const grammar& g);

// increment an item's read position
item succ(const item& i);

// the next value to read from an item, or the next values to read from a set of items
//   (in the case of reading from an item, the next value may be null if the item represents a completed rule)
terminal*   next(const grammar& g, const item& i);
terminalset next(const grammar& g, const itemset& is);

// the set of _non-aggregate_ (primitive terminals) following an itemset
terminalset nextPrim(const grammar& g, const itemset& is);

// the set of items that follow a given value (not closed)
itemset  follow(const grammar& g, const itemset& is, terminal* v);
itemsets follow(const grammar& g, const itemset& is, const terminals& vs);

// close an itemset
itemset gclosure(const grammar& g, const itemset& is);
void gclosure(const grammar& g, itemset* is);

// find or create a state in a parser from a (closed) itemset
nat parserState(const itemset& s, parserdef* p);

// find the (closed) state transitions from a closed itemset
state_transitions follow(const grammar& g, const itemset& is, const terminalset& vs, parserdef* p);

// determine the parser state graph (ie: the LR(0) parser) for a grammar, given a start symbol
parserdef lr0parser(const grammar& g, terminal* s);

// from a state q and symbol t, what state do you go to?
nat follow(const parserdef& p, nat q, terminal* t);

// a transition of the LR(0) parser can be described by a state and terminal
typedef std::pair<nat, terminal*> transition;
typedef std::set<transition>      transitionset;

transitionset transitions(const parserdef& p);

// does (q, r -> w) look back to the transition t?
//  (that is, does sym(t) == r, and does state(t) follow w to get to state q?)
bool lookback(const parserdef& p, nat q, terminal* r, const rule& w, const transition& t);

// x=(s0, A) includes y=(s1, B) if B -> aAm and m derives the empty string and s1 follows a to get to s0
bool includes(const parserdef& p, const transition& x, const transition& y);

// x=(s0, A) reads y=(s1, B) if s0 follows A to s1 and B derives the empty string
bool reads(const parserdef& p, const transition& x, const transition& y);

// t in directReads(x(s, A)) if s follows A to s' and t is in the set of non-aggregate terminals at s'
terminalset directReads(const parserdef& p, const transition& x);

// for x in X, compute a function f : X -> Y from f' : X -> Y and + : Y -> Y -> Y and R : X -> X -> bool, such that f(x) = f'(x) + sum(f(y) | y in X, R(x,y))
template <typename X, typename Y, typename Fp, typename A, typename R>
  class digraphF {
  public:
    typedef std::set<X>    Xs;
    typedef std::map<X, Y> F;

    static F map(const Xs& xs, const Fp& fp, const A& append, const R& r) {
      return digraphF(xs, fp, append, r).map();
    }
  private:
    const Xs& xs;
    const Fp& fp;
    const A&  append;
    const R&  r;

    digraphF(const Xs& xs, const Fp& fp, const A& append, const R& r) : xs(xs), fp(fp), append(append), r(r) {
    }

    typedef std::stack<X>    Xstack;
    typedef std::map<X, nat> Xidxs;

    F map() const {
      F      f;
      Xstack s;
      Xidxs  n = freshIdxs(this->xs);

      for (typename Xs::const_iterator x = this->xs.begin(); x != this->xs.end(); ++x) {
        if (n[*x] == 0) {
          traverse(*x, &s, &n, &f);
        }
      }

      return f;
    }

    void traverse(X x, Xstack* s, Xidxs* n, F* f) const {
      s->push(x);
      nat d = s->size();

      // base case, this node contains its immediate value
      (*n)[x] = d;
      (*f)[x] = this->fp(x);

      // and the values of any nodes in the input relation
      for (typename Xs::const_iterator y = this->xs.begin(); y != this->xs.end(); ++y) {
        if (this->r(x, *y)) {
          // the node y is in the relation
          // if it hasn't been visited yet, then recurse into it to determine its value
          if ((*n)[*y] == 0) {
            traverse(*y, s, n, f);
          }

          // finally, add its value to the output for x
          (*f)[x] = this->append((*f)[x], (*f)[*y]);
        }
      }

      // join all the nodes in this SCC
      if ((*n)[x] == d) {
        while (true) {
          X cx = s->top();
          s->pop();
          (*n)[cx] = inf();
          if (cx == x) {
            break;
          } else {
            (*f)[cx] = (*f)[x];
          }
        }
      }
    }

    static Xidxs freshIdxs(const Xs& xs) {
      Xidxs r;
      for (typename Xs::const_iterator x = xs.begin(); x != xs.end(); ++x) {
        r[*x] = 0;
      }
      return r;
    }

    static nat inf() { return static_cast<nat>(-1); }
  };

// the across/up sets of LALR(1) determination
typedef std::map<transition, terminalset> transition_lookahead;

transition_lookahead Reads(const parserdef& p, const transitionset& ts);
transition_lookahead Follow(const parserdef& p, const transitionset& ts);

// determine the LALR(1) parser for a grammar, given a start symbol
parserdef lalr1parser(const grammar& g, terminal* s);

// an ambiguity conflict on a particular terminal
class ambiguity_conflict : public std::runtime_error {
public:
  ambiguity_conflict(const std::string& ex, terminal* t) throw();

  terminal* failedTerminal() const;
private:
  terminal* t;
};

// a failure to compile a grammar to an LALR table, with cause
class compile_table_failure : public std::runtime_error {
public:
  compile_table_failure(const std::string& msg, const grammar& g, const itemset& faileditems, terminal* t) throw();

  const grammar& failedGrammar() const;
  const itemset& failedItems() const;
  terminal* failedTerminal() const;

  void print(std::ostream&) const;
private:
  grammar   g;
  itemset   faileditems;
  terminal* t;

  void print(std::ostream&,const item&) const;
};

// an LR parser action (shift, reduce, or accept)
class action {
public:
  // go to state s
  static action goTo(nat s);

  // shift to state s
  static action shift(nat s);

  // for symbol x, rule r, reduce n states
  static action reduce(terminal* x, nat r, nat n);

  // the input has been accepted
  static action accept();

  // what type of action is this?
  bool isGoTo()    const;
  nat  goToState() const;

  bool isShift()    const;
  nat  shiftState() const;

  bool      isReduce()   const;
  terminal* reduceSym()  const;
  nat       reduceRule() const;
  nat       reduceSize() const;

  bool isAccept() const;

  // basic ops
  bool operator==(const action& rhs) const;
  bool operator!=(const action& rhs) const;

  std::ostream& fmt(std::ostream& out) const;
private:
  uint8_t act;
  union {
    // for shift
    nat s;

    // for reduce
    struct {
      terminal* x;
      nat       r;
      nat       n;
    };
  } d;

  action();
};

std::ostream& operator<<(std::ostream& out, const action& act);

// a parser state maps a set of terminals to actions
typedef std::map<terminal*, action> lrstate;

// an ordered sequence of states describes the whole behavior of an LR parser
typedef std::vector<lrstate> lrtable;

// determine the LR table, derived from the LALR(1) parser
lrtable lalrTable(const parserdef& pd, const precedence& p = precedence());
lrtable lalrTable(const grammar& g, terminal* s, const precedence& p = precedence());

// show an LR table (useful for debugging)
void show(std::ostream&, const lrtable&);

}

#endif

