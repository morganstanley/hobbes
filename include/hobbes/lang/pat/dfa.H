/*
 * dfa : constructs a DFA for pattern match translation
 */

#ifndef HOBBES_LANG_PAT_DFA_HPP_INCLUDED
#define HOBBES_LANG_PAT_DFA_HPP_INCLUDED

#include <hobbes/lang/pat/pattern.H>
#include <hobbes/util/str.H>
#include <hobbes/util/lannotation.H>
#include <unordered_map>
#include <set>

namespace hobbes {

using stateidx_t = size_t;
using stateidxset = std::set<stateidx_t>;

using PrimFArg = std::pair<std::string, MonoTypePtr>;
using PrimFArgs = std::vector<PrimFArg>;

// the various states of pattern matching
class MState {
public:
  virtual ~MState() = default;
  virtual std::string stamp() = 0;

  // how many times is this state referenced?
  //  (if 0, we can cull the state, if >1 we may want to fold the state into a function)
  size_t refs;

  // does this state represent the start of a primitive match?
  // if so, we may be able to translate it to an efficient low-level matching function directly
  bool      isPrimMatchRoot;
  PrimFArgs primFArgs;

  // improves performance of case-analysis over instances (to avoid 'dynamic_cast')
public:
  int case_id() const;
protected:
  MState(int cid);
private:
  int cid;
};

template <typename Case>
  class MStateCase : public MState {
  public:
    MStateCase();
  };

class LoadVars : public MStateCase<LoadVars> {
public:
  using Def = std::pair<std::string, ExprPtr>;
  using Defs = std::vector<Def>;

  LoadVars(const Defs&, stateidx_t);
  const Defs& defs() const;
  stateidx_t nextState() const;

  std::string stamp() override;

  static const int type_case_id = 0;
private:
  Defs ds;
  stateidx_t next;
};

class SwitchVal : public MStateCase<SwitchVal> {
public:
  using Jump = std::pair<PrimitivePtr, stateidx_t>;
  using Jumps = std::vector<Jump>;

  SwitchVal(const std::string&, const Jumps&, stateidx_t);
  const std::string& switchVar() const;
  const Jumps& jumps() const;
  stateidx_t defaultState() const;

  std::string stamp() override;

  static const int type_case_id = 1;
private:
  std::string var;
  Jumps       jmps;
  stateidx_t  def;
};

class SwitchVariant : public MStateCase<SwitchVariant> {
public:
  using CtorJump = std::pair<std::string, stateidx_t>;
  using CtorJumps = std::vector<CtorJump>;

  SwitchVariant(const std::string&, const CtorJumps&, stateidx_t);
  const std::string& switchVar() const;
  const CtorJumps& jumps() const;
  stateidx_t defaultState() const;

  std::string stamp() override;

  static const int type_case_id = 2;
private:
  std::string var;
  CtorJumps   jmps;
  stateidx_t  def;
};

class FinishExpr : public MStateCase<FinishExpr> {
public:
  FinishExpr(const ExprPtr&);

  const ExprPtr& expr() const;

  std::string stamp() override;

  static const int type_case_id = 3;
private:
  ExprPtr exp;
};

extern stateidx_t nullState;

template <typename Case>
  MStateCase<Case>::MStateCase() : MState(Case::type_case_id) {
  }

using MStatePtr = std::shared_ptr<MState>;
using MStates = std::vector<MStatePtr>;

// destruction-side for high-level pattern representations
template <typename T>
struct switchMState {
    virtual ~switchMState() = default;
    virtual T with(const LoadVars*)      const = 0;
    virtual T with(const SwitchVal*)     const = 0;
    virtual T with(const SwitchVariant*) const = 0;
    virtual T with(const FinishExpr*)    const = 0;
  };

template <typename T>
  T switchOf(const MState& s, const switchMState<T>& f) {
    switch (s.case_id()) {
    case LoadVars::type_case_id:
      return f.with(reinterpret_cast<const LoadVars*>(&s));
    case SwitchVal::type_case_id:
      return f.with(reinterpret_cast<const SwitchVal*>(&s));
    case SwitchVariant::type_case_id:
      return f.with(reinterpret_cast<const SwitchVariant*>(&s));
    case FinishExpr::type_case_id:
      return f.with(reinterpret_cast<const FinishExpr*>(&s));
    default:
      throw std::runtime_error("Internal error, cannot switch on unknown match state");
    }
  }

template <typename T>
  T switchOf(const MStatePtr& s, const switchMState<T>& f) {
    return switchOf(*s, f);
  }

// DFA construction from annotated/normalized pattern match tables
using StatesIdx = std::unordered_map<std::string, stateidx_t>;

using VarNames = std::unordered_map<std::string, ExprPtr>;       // memoize usage of variable names
using ArrayElem = std::map<size_t, ExprPtr>;
using VarArrayElem = std::unordered_map<std::string, ArrayElem>;   // memoize array 'element' access in match variables
using VarArrayLen = std::unordered_map<std::string, ExprPtr>;    // memoize array 'size' access in match variables
using StructField = std::unordered_map<std::string, ExprPtr>;
using VarStructField = std::unordered_map<std::string, StructField>; // memoize struct field access in match variables

using FoldedState = std::pair<std::string, ExprPtr>;
using FoldedStates = std::vector<FoldedState>;     // local functions for states that should be lifted out
using FoldedStateCalls = std::unordered_map<stateidx_t, ExprPtr>; // call expressions into states that have been folded into local functions

using TableCfgStates = std::unordered_map<PatternRows, stateidx_t, hobbes::genHash<PatternRows>>; // map distinct pattern table configs to their corresponding states (avoid reconstructing the whole state description)

using ExprIdxs = std::unordered_map<Expr *, size_t>; // map back from result expressions to row IDs

struct MDFA {
  // the lexical extent of the whole match in the original source program
  LexicalAnnotation rootLA;

  // dfa state
  MStates        states;
  StatesIdx      statesIdx;
  TableCfgStates tableCfgStates;
  ExprIdxs       exprIdxs;
  bool           inPrimSel;

  str::set rootVars;
  cc*      c;

  // memo expressions
  VarNames       varExps;
  VarArrayElem   elementExps;
  VarArrayLen    sizeExps;
  VarStructField fieldExps;

  // fold states with multiple references into local functions
  FoldedStates     foldedStates;
  FoldedStateCalls foldedStateCalls;
};

stateidx_t makeDFA(MDFA*, const PatternRows&, const LexicalAnnotation&);
stateidx_t makeDFAState(MDFA* dfa, const PatternRows& ps);

ExprPtr liftDFAExpr(cc*, const PatternRows&, const LexicalAnnotation&);

}

#endif

