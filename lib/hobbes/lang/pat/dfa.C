
#include <hobbes/lang/pat/dfa.H>
#include <hobbes/lang/pat/regex.H>
#include <hobbes/lang/pat/print.H>
#include <hobbes/util/perf.H>
#include <sstream>
#include <fstream>

#include <hobbes/eval/cc.H>
#include <hobbes/eval/cexpr.H>

namespace hobbes {

stateidx_t nullState = static_cast<stateidx_t>(-1);

// match DFA state defs
MState::MState(int cid) : refs(0), isPrimMatchRoot(false), cid(cid) { }
int MState::case_id() const { return this->cid; }

LoadVars::LoadVars(const Defs& ds, stateidx_t next) : ds(ds), next(next) { }
const LoadVars::Defs& LoadVars::defs() const { return this->ds; }
stateidx_t LoadVars::nextState() const { return this->next; }

std::string LoadVars::stamp() {
  std::ostringstream ss;
  ss << "l";
  for (auto d : this->ds) {
    ss << "." << d.first << "=" << reinterpret_cast<void*>(d.second.get()); // assumes that we deliberately share equivalent expressions
  }
  ss << "." << this->next;
  return ss.str();
}

SwitchVal::SwitchVal(const std::string& var, const Jumps& jmps, stateidx_t def) : var(var), jmps(jmps), def(def) { }
const std::string& SwitchVal::switchVar() const { return this->var; }
const SwitchVal::Jumps& SwitchVal::jumps() const { return this->jmps; }
stateidx_t SwitchVal::defaultState() const { return this->def; }

std::string SwitchVal::stamp() {
  std::ostringstream ss;
  ss << "s." << this->var;
  for (auto j : this->jmps) {
    ss << ".";
    j.first->show(ss);
    ss << ":" << j.second;
  }
  ss << "." << this->def;
  return ss.str();
}

SwitchVariant::SwitchVariant(const std::string& var, const CtorJumps& jmps, stateidx_t def) : var(var), jmps(jmps), def(def) { }
const std::string& SwitchVariant::switchVar() const { return this->var; }
const SwitchVariant::CtorJumps& SwitchVariant::jumps() const { return this->jmps; }
stateidx_t SwitchVariant::defaultState() const { return this->def; }

std::string SwitchVariant::stamp() {
  std::ostringstream ss;
  ss << "c." << this->var;
  for (auto j : this->jmps) {
    ss << "." << j.first << "=" << j.second;
  }
  ss << "." << this->def;
  return ss.str();
}

FinishExpr::FinishExpr(const ExprPtr& exp) : exp(exp) { }
const ExprPtr& FinishExpr::expr() const { return this->exp; }

std::string FinishExpr::stamp() {
  std::ostringstream ss;
  ss << "e." << reinterpret_cast<void*>(this->exp.get());
  return ss.str();
}

// make a trivial state that performs a single action and continues
MStatePtr actAndThen(const ExprPtr& e, stateidx_t s) {
  LoadVars::Defs ds;
  ds.push_back(LoadVars::Def(freshName(), e));
  return MStatePtr(new LoadVars(ds, s));
}

// make a primitive value switch, validating exhaustiveness
MStatePtr makeSwitch(const MDFA* dfa, const std::string& switchVar, const SwitchVal::Jumps& jmps, stateidx_t defState) {
  if (defState != nullState) {
    // if we've only got a default switch and no cases, just use the default case
    // otherwise if this would be a 'switch' on unit, don't bother (just pass the type equality through)
    //  -- only the first option can match for unit (or default if there's only a default)
    if (jmps.size() == 0) {
      return dfa->states[defState];
    } else if (isUnit(jmps[0].first->primType())) {
      return actAndThen(assume(var(switchVar, dfa->rootLA), primty("unit"), dfa->rootLA), jmps[0].second);
    } else {
      return MStatePtr(new SwitchVal(switchVar, jmps, defState));
    }
  } else {
    if (jmps.size() > 0) {
      MonoTypePtr sty = jmps[0].first->primType();

      if (const Prim* pty = is<Prim>(sty)) {
        if (pty->name() == "bool" && jmps.size() == 2) {
          return MStatePtr(new SwitchVal(switchVar, list(jmps[0]), jmps[1].second));
        } else if ((pty->name() == "char" || pty->name() == "byte") && jmps.size() == 256) {
          return MStatePtr(new SwitchVal(switchVar, SwitchVal::Jumps(jmps.begin(), jmps.end() - 1), jmps[jmps.size() - 1].second));
        } else if (pty->name() == "unit") {
          return actAndThen(assume(var(switchVar, dfa->rootLA), primty("unit"), dfa->rootLA), jmps[0].second);
        }
      }
    }
    throw annotated_error(dfa->rootLA, "Inexhaustive patterns in match expression");
  }
}

// increment the ref count for a given state in the DFA
void addRef(MDFA* dfa, stateidx_t s) {
  dfa->states[s]->refs += 1;
}

// add a state to the DFA and return its index
// (if the state already exists in the DFA, increment its reference count and return it)
stateidx_t addState(MDFA* dfa, const MStatePtr& state, bool addref = true) {
  StatesIdx::const_iterator si = dfa->statesIdx.find(state->stamp());
  if (si != dfa->statesIdx.end()) {
    dfa->states[si->second]->refs += addref ? 1 : 0;
    return si->second;
  }

  // create a new state, give it a ref count, determine which states it leads to
  stateidx_t s = dfa->states.size();
  dfa->states.push_back(state);
  dfa->statesIdx[state->stamp()] = s;
  state->refs += addref ? 1 : 0;
  return s;
}

// memoize common expressions
ExprPtr varName(MDFA* dfa, const std::string& vn) {
  VarNames::const_iterator ve = dfa->varExps.find(vn);
  if (ve != dfa->varExps.end()) {
    return ve->second;
  }

  ExprPtr r = var(vn, dfa->rootLA);
  dfa->varExps[vn] = r;
  return r;
}

ExprPtr arrayElement(MDFA* dfa, const std::string& vn, size_t i) {
  VarArrayElem::const_iterator ae = dfa->elementExps.find(vn);
  if (ae != dfa->elementExps.end()) {
    ArrayElem::const_iterator ie = ae->second.find(i);
    if (ie != ae->second.end()) {
      return ie->second;
    }
  }

  ExprPtr r = fncall(var("element", dfa->rootLA), list(varName(dfa, vn), ExprPtr(new Long(i, dfa->rootLA))), dfa->rootLA);
  dfa->elementExps[vn][i] = r;
  return r;
}

ExprPtr charArrElement(MDFA* dfa, const std::string& packFn, const std::string& vn, size_t i) {
  VarArrayElem::const_iterator ae = dfa->elementExps.find(vn);
  if (ae != dfa->elementExps.end()) {
    ArrayElem::const_iterator ie = ae->second.find(i);
    if (ie != ae->second.end()) {
      return ie->second;
    }
  }

  ExprPtr r = fncall(var(packFn, dfa->rootLA), list(varName(dfa, vn), ExprPtr(new Long(i, dfa->rootLA))), dfa->rootLA);
  dfa->elementExps[vn][i] = r;
  return r;
}

ExprPtr openArray(MDFA* dfa, const std::string& vn) {
  return fncall(var("openArrayView", dfa->rootLA), list(varName(dfa, vn)), dfa->rootLA);
}

ExprPtr arraySize(MDFA* dfa, const std::string& vn) {
  VarArrayLen::const_iterator al = dfa->sizeExps.find(vn);
  if (al != dfa->sizeExps.end()) {
    return al->second;
  }

  ExprPtr r = fncall(var("size", dfa->rootLA), list(varName(dfa, vn)), dfa->rootLA);
  dfa->sizeExps[vn] = r;
  return r;
}

ExprPtr field(MDFA* dfa, const std::string& vn, const std::string& fn) {
  VarStructField::const_iterator vsf = dfa->fieldExps.find(vn);
  if (vsf != dfa->fieldExps.end()) {
    StructField::const_iterator sf = vsf->second.find(fn);
    if (sf != vsf->second.end()) {
      return sf->second;
    }
  }

  ExprPtr r = proj(varName(dfa, vn), fn, dfa->rootLA);
  dfa->fieldExps[vn][fn] = r;
  return r;
}

// produce a copy of a pattern table, without the head row
void tableTail(PatternRows* out, const PatternRows& in) {
  out->resize(in.size() - 1);
  for (size_t r = 1; r < in.size(); ++r) {
    (*out)[r - 1] = in[r];
  }
}

// eliminate unused columns from a table
void dropUnusedColumns(PatternRows* out, const PatternRows& in) {
  if (in.size() == 0) {
    return;
  }

  std::set<size_t> usedColumns;
  for (size_t c = 0; c < in[0].patterns.size(); ++c) {
    bool hasUse = false;
    for (size_t r = 0; r < in.size(); ++r) {
      if (!is<MatchAny>(in[r].patterns[c])) {
        hasUse = true;
        break;
      }
    }
    if (hasUse) {
      usedColumns.insert(c);
    }
  }

  if (usedColumns.size() == in[0].patterns.size()) {
    *out = in;
  } else {
    for (size_t r = 0; r < in.size(); ++r) {
      Patterns ps;
      for (auto c : usedColumns) {
        ps.push_back(in[r].patterns[c]);
      }
      out->push_back(PatternRow(ps, in[r].guard, in[r].result));
    }
  }
}

// split a row on a column written into an output table
void copyRowWithoutColumn(PatternRows* out, const PatternRow& row, size_t c) {
  Patterns ps;
  for (size_t ci = 0; ci < row.patterns.size(); ++ci) {
    if (ci != c) {
      ps.push_back(row.patterns[ci]);
    }
  }
  out->push_back(PatternRow(ps, row.guard, row.result));
}

// stick some patterns on the front of a pattern row
void prependPatterns(PatternRow* row, const Patterns& ps) {
  row->patterns.insert(row->patterns.begin(), ps.begin(), ps.end());
}

// get the sub-patterns in an array match
Patterns arrayPatterns(const MatchArray& ma) {
  Patterns r;
  r.reserve(ma.size());
  for (size_t i = 0; i < ma.size(); ++i) {
    r.push_back(ma.pattern(i));
  }
  return r;
}

// create an array of dummy "match anything" patterns
Patterns arrayAnyMatches(size_t n) {
  static PatternPtr pany(new MatchAny("_", LexicalAnnotation::null()));
  Patterns r;
  r.resize(n);
  for (size_t i = 0; i < n; ++i) { r[i] = pany; }
  return r;
}

// pull out the patterns from a record match
Patterns recordFieldPatterns(const MatchRecord& mr) {
  Patterns r;
  r.resize(mr.size());
  for (size_t i = 0; i < mr.size(); ++i) {
    r[i] = mr.pattern(i).second;
  }
  return r;
}

///////////////////////////////////////////////////
// makeSplitState
//
//   Purpose:
//     Creates a DFA state by splitting a pattern table on a given column generically
//
//   Inputs:
//     MType  ::: * - the match type split on
//     SValue ::: * - the primitive value type to switch on
//     svalueF :: MType -> SValue - extracts a primitive switch value from a match value
//     makeMatchRow :: MType*Row*Column -> [Row] | Produce a new row after having matched with the match value in the given row/column
//     makeMatchAnyRow :: SValue*Row*Column -> [Row] | Produce a new row from the given row/column after having switched on the given primitive value
//     makeNextState   :: Var*DFA*[Row] -> State     | Produce a successor state for a match on the given variable with a given DFA and successor match table
//     makeSwitchState :: Var*DFA*[(SValue,State)]*State | Produce a switch state on the given variable with the given set of value/state pairs and a default state
//
//   Outputs:
//     A DFA state description performing the required split
///////////////////////////////////////////////////
template <
  typename MType,
  typename SValue,
  SValue (*svalueF)(const MType&),
  void (*makeMatchRow)(const MType&, PatternRows*, const PatternRow&, size_t),
  void (*makeMatchAnyRow)(SValue, PatternRows*, const PatternRow&, size_t),
  stateidx_t (*makeNextState)(const std::string&, SValue, MDFA*, const PatternRows&),
  MStatePtr (*makeSwitchState)(const std::string&, MDFA*, const std::vector< std::pair<SValue, stateidx_t> >&, stateidx_t),
  typename SVLT = std::less<SValue>
>
MStatePtr makeSplitState(MDFA* dfa, const PatternRows& ps, size_t c) {
  typedef std::map<SValue, PatternRows, SVLT> Branches;
  typedef std::map<SValue, size_t>            BranchOrder;

  Branches    bs;
  BranchOrder bos;
  Idxs        anys;
  PatternRows def;

  // accumulate branches for fixed cases
  for (size_t r = 0; r < ps.size(); ++r) {
    if (const MType* mt = is<MType>(ps[r].patterns[c])) {
      SValue sv = svalueF(*mt);
      if (bs.find(sv) == bs.end()) {
        bos[sv] = bs.size(); // remember branch introduction order (this is important for variants)
      }
      PatternRows& outrs = bs[sv];

      // the first time through this branch, we need to make sure that prior match-any rows are prepended
      if (outrs.size() == 0) {
        for (auto i : anys) {
          makeMatchAnyRow(sv, &outrs, ps[i], c);
        }
      }

      // add this row to the fixed branch
      makeMatchRow(*mt, &outrs, ps[r], c);
    } else if (is<MatchAny>(ps[r].patterns[c])) {
      // record where this match-any came from, then append it to all states recorded so far
      anys.push_back(r);

      for (typename Branches::iterator b = bs.begin(); b != bs.end(); ++b) {
        makeMatchAnyRow(b->first, &b->second, ps[r], c);
      }
    } else {
      throw annotated_error(dfa->rootLA, "Internal error, invalid pattern table received");
    }
  }

  // now accumulate 'catch-all' rows into the default state
  for (auto i : anys) {
    copyRowWithoutColumn(&def, ps[i], c);
  }

  // finally, pull the branch and default jumps together (in introduction order) and make the new state
  typedef std::pair<SValue, stateidx_t> Jump;
  typedef std::vector<Jump>             Jumps;
  Jumps jmps;

  jmps.resize(bs.size());
  for (const auto& b : bs) {
    jmps[bos[b.first]] = Jump(b.first, makeNextState(ps[0].patterns[c]->name(), b.first, dfa, b.second));
  }

  stateidx_t defState = def.size() > 0 ? makeDFAState(dfa, def) : nullState;

  return makeSwitchState(ps[0].patterns[c]->name(), dfa, jmps, defState);
}

// split on a primitive column into a switch state
PrimitivePtr litValue(const MatchLiteral& ml) { return ml.equivConstant(); }
void makeLitSplitRow(const MatchLiteral&, PatternRows* out, const PatternRow& r, size_t c) { copyRowWithoutColumn(out, r, c); }
void makeLSAnyRow(PrimitivePtr, PatternRows* out, const PatternRow& r, size_t c) { copyRowWithoutColumn(out, r, c); }
stateidx_t makeLSSuccState(const std::string&, PrimitivePtr, MDFA* dfa, const PatternRows& nps) { return makeDFAState(dfa, nps); }

MStatePtr makeLSSwitch(const std::string& switchVar, MDFA* dfa, const SwitchVal::Jumps& jmps, stateidx_t defState) {
  return makeSwitch(dfa, switchVar, jmps, defState);
}

MStatePtr makeLSCvtSwitch(const std::string& switchVar, MDFA* dfa, const SwitchVal::Jumps& jmps, stateidx_t defState) {
  std::string cvn = switchVar + ".cvt";
  LoadVars::Defs ds;
  ds.push_back(LoadVars::Def(cvn, fncall(var("convert", dfa->rootLA), list(var(switchVar, dfa->rootLA)), dfa->rootLA)));
  return MStatePtr(new LoadVars(ds, addState(dfa, makeLSSwitch(cvn, dfa, jmps, defState))));
}

bool forConvertibility(const PatternPtr& p) {
  if (const MatchLiteral* ml = is<MatchLiteral>(p)) {
    return ml->expression() != ExprPtr();
  } else {
    return false;
  }
}

MStatePtr makeLiteralState(MDFA* dfa, const PatternRows& ps, size_t c) {
  if (forConvertibility(ps[0].patterns[c])) {
    return
      makeSplitState<
        MatchLiteral,
        PrimitivePtr,
        &litValue,
        &makeLitSplitRow,
        &makeLSAnyRow,
        &makeLSSuccState,
        &makeLSCvtSwitch,
        PrimPtrLT
      >
      (dfa, ps, c);
  } else {
    return
      makeSplitState<
        MatchLiteral,
        PrimitivePtr,
        &litValue,
        &makeLitSplitRow,
        &makeLSAnyRow,
        &makeLSSuccState,
        &makeLSSwitch,
        PrimPtrLT
      >
      (dfa, ps, c);
  }
}

// split on array match columns into a length load and switch
size_t maSize(const MatchArray& ma) { return ma.size(); }

void makeASplitRow(const MatchArray& ma, PatternRows* out, const PatternRow& r, size_t c) {
  copyRowWithoutColumn(out, r, c);
  prependPatterns(&out->back(), arrayPatterns(ma));
}

void makeASAnyRow(size_t len, PatternRows* out, const PatternRow& r, size_t c) {
  copyRowWithoutColumn(out, r, c);
  prependPatterns(&out->back(), arrayAnyMatches(len));
}

stateidx_t makeASSuccState(const std::string& arrayVar, size_t len, MDFA* dfa, const PatternRows& nps) {
  LoadVars::Defs ds;
  for (size_t i = 0; i < len; ++i) {
    ds.push_back(LoadVars::Def(arrayVar + "." + str::from(i + 1), arrayElement(dfa, arrayVar, i)));
  }
  return addState(dfa, MStatePtr(new LoadVars(ds, makeDFAState(dfa, nps))));
}

MStatePtr makeASSwitch(const std::string& arrayVar, MDFA* dfa, const std::vector< std::pair<size_t, stateidx_t> >& lenjmps, stateidx_t defState) {
  // switch on array length, then load that many elements and continue
  std::string switchVar = arrayVar + ".0";
  SwitchVal::Jumps jmps;
  for (const auto& lj : lenjmps) {
    jmps.push_back(SwitchVal::Jump(PrimitivePtr(new Long(lj.first, dfa->rootLA)), lj.second));
  }

  // open an array view out of this value (for most types this should be a no-op)
  // load array length
  // and then perform the switch
  std::string oarrayVar = arrayVar + ".a";

  stateidx_t switchState = addState(dfa, makeSwitch(dfa, switchVar, jmps, defState));
  LoadVars::Defs ds;
  ds.push_back(LoadVars::Def(oarrayVar, openArray(dfa, arrayVar)));
  ds.push_back(LoadVars::Def(switchVar, arraySize(dfa, oarrayVar)));

  return MStatePtr(new LoadVars(ds, switchState));
}

MStatePtr makeArrayState(MDFA* dfa, const PatternRows& ps, size_t c) {
  return
    makeSplitState<
      MatchArray,
      size_t,
      &maSize,
      &makeASplitRow,
      &makeASAnyRow,
      &makeASSuccState,
      &makeASSwitch
    >
    (dfa, ps, c);
}

// split on string matching
size_t maxStringLen(const PatternRows& ps, size_t c) {
  size_t mlen = 0;
  for (size_t r = 0; r < ps.size(); ++r) {
    if (const MatchArray* ma = is<MatchArray>(ps[r].patterns[c])) {
      mlen = std::max<size_t>(ma->size(), mlen);
    }
  }
  return mlen;
}

size_t packedSValues(size_t cs) {
  assert(cs % 8 == 0);
  return cs / 8;
}

inline unsigned char spatChar(const MatchArray& ma, size_t i) {
  if (i >= ma.size()) {
    return 0;
  } else if (const MatchLiteral* ml = is<MatchLiteral>(ma.pattern(i))) {
    if (const Char* c = is<Char>(ml->equivConstant())) {
      return static_cast<unsigned char>(c->value());
    } else {
      throw annotated_error(*ml, "Internal error, can't unpack non-char as char");
    }
  } else {
    throw annotated_error(ma, "Internal error, can't unpack non-string as string");
  }
}

template <typename T>
  T translatep(const MatchArray& ma, size_t i) {
    T x = 0;
    for (size_t j = 0; j < sizeof(T); ++j) {
      x |= static_cast<T>(spatChar(ma, i + (sizeof(T) - 1 - j))) << (8 * j);
    }
    return x;
  }

Patterns sarrayPatterns(const std::string& svarName, size_t cs, const MatchArray& ma) {
  Patterns r;
  size_t i  = 0;
  while (cs >= 8) {
    PatternPtr qwp(new MatchLiteral(PrimitivePtr(new Long(translatep<unsigned long>(ma, i), ma.la())), ma.la()));
    qwp->name(svarName + ".l" + str::from(i));
    r.push_back(qwp);

    cs -= 8;
    i  += 8;
  }
  assert(cs == 0);
  return r;
}

void addSATableRow(size_t len, const PatternRow& r, size_t c, PatternRows* out) {
  copyRowWithoutColumn(out, r, c);
  if (const MatchArray* ma = is<MatchArray>(r.patterns[c])) {
    prependPatterns(&out->back(), sarrayPatterns(r.patterns[c]->name(), len, *ma));
  } else {
    assert(is<MatchAny>(r.patterns[c]));
    prependPatterns(&out->back(), arrayAnyMatches(packedSValues(len)));
  }
}

MStatePtr makeCharArrayState(MDFA* dfa, const PatternRows& ps, size_t c) {
  static MonoTypePtr longTy(Prim::make("long"));

  std::string arrayVar = ps[0].patterns[c]->name();
  size_t      mlen     = align<size_t>(maxStringLen(ps, c) + 1, 8);
  size_t      cs       = mlen;
  
  LoadVars::Defs ds;

  // open this array (usually a no-op)
  std::string oarrayVar = arrayVar + ".a";
  ds.push_back(LoadVars::Def(oarrayVar, openArray(dfa, arrayVar)));

  // load string values
  size_t i = 0;
  while (cs >= 8) {
    ds.push_back(LoadVars::Def(arrayVar + ".l" + str::from(i), assume(charArrElement(dfa, "packCArrLong", oarrayVar, i), longTy, dfa->rootLA)));
    cs -= 8;
    i  += 8;
  }
  assert(cs == 0);

  // generate the successor table
  PatternRows nps;
  for (size_t r = 0; r < ps.size(); ++r) {
    addSATableRow(mlen, ps[r], c, &nps);
  }

  // and that's it!
  return MStatePtr(new LoadVars(ds, makeDFAState(dfa, nps)));
}

bool canMakeCharArrayState(const PatternRows& ps, size_t c) {
  bool seemsLegit = false;
  for (size_t r = 0; r < ps.size(); ++r) {
    if (const MatchArray* ma = is<MatchArray>(ps[r].patterns[c])) {
      for (size_t i = 0; i < ma->size(); ++i) {
        if (const MatchLiteral* ml = is<MatchLiteral>(ma->pattern(i))) {
          if (is<Char>(ml->equivConstant())) {
            seemsLegit = true;
          }
        } else {
          return false;
        }
      }
    } else if (!is<MatchAny>(ps[r].patterns[c])) {
      return false;
    }
  }
  return seemsLegit;
}

size_t canMakeCharArrStateAtColumn(MDFA* dfa, const PatternRows& ps) {
  if (dfa->c->buildColumnwiseMatches()) {
    return ps[0].patterns.size(); // don't bother packing strings if we're compiling columnwise
  }

  for (size_t c = 0; c < ps[0].patterns.size(); ++c) {
    if (canMakeCharArrayState(ps, c)) {
      return c;
    }
  }
  return ps[0].patterns.size();
}

// split on record load/matches
MStatePtr makeRecordState(MDFA* dfa, const PatternRows& ps, size_t c) {
  // get the fields to load
  LoadVars::Defs defs;

  if (const MatchRecord* mr = is<MatchRecord>(ps[0].patterns[c])) {
    for (size_t fi = 0; fi < mr->size(); ++fi) {
      const MatchRecord::Field& f = mr->pattern(fi);
      defs.push_back(LoadVars::Def(f.second->name(), field(dfa, mr->name(), f.first)));
    }
  } else {
    throw annotated_error(*ps[0].patterns[c], "Internal error, can't make record state from non-record pattern");
  }

  // eliminate this column and add new columns for field patterns
  PatternRows cdef;

  for (size_t r = 0; r < ps.size(); ++r) {
    if (const MatchRecord* mr = is<MatchRecord>(ps[r].patterns[c])) {
      copyRowWithoutColumn(&cdef, ps[r], c);
      prependPatterns(&cdef.back(), recordFieldPatterns(*mr));
    } else if (is<MatchAny>(ps[r].patterns[c])) {
      copyRowWithoutColumn(&cdef, ps[r], c);
      prependPatterns(&cdef.back(), arrayAnyMatches(defs.size()));
    } else {
      throw annotated_error(*ps[r].patterns[c], "Internal error, invalid pattern table received");
    }
  }

  // now just load and continue
  return MStatePtr(new LoadVars(defs, makeDFAState(dfa, cdef)));
}

// split on variant switch/matches
std::string varCtor(const MatchVariant& mv) { return mv.label(); }

void makeVariantSplitRow(const MatchVariant& mv, PatternRows* out, const PatternRow& r, size_t c) {
  copyRowWithoutColumn(out, r, c);
  out->back().patterns.insert(out->back().patterns.begin(), mv.value());
}

void makeVSAnyRow(std::string, PatternRows* out, const PatternRow& r, size_t c) {
  copyRowWithoutColumn(out, r, c);
  out->back().patterns.insert(out->back().patterns.begin(), r.patterns[c]);
}

stateidx_t makeVSSuccState(const std::string&, std::string, MDFA* dfa, const PatternRows& nps) {
  return makeDFAState(dfa, nps);
}

MStatePtr makeVSSwitch(const std::string& switchVar, MDFA*, const SwitchVariant::CtorJumps& jmps, stateidx_t defState) {
  return MStatePtr(new SwitchVariant(switchVar, jmps, defState));
}

MStatePtr makeVariantState(MDFA* dfa, const PatternRows& ps, size_t c) {
  return
    makeSplitState<
      MatchVariant,
      std::string,
      &varCtor,
      &makeVariantSplitRow,
      &makeVSAnyRow,
      &makeVSSuccState,
      &makeVSSwitch
    >
    (dfa, ps, c);
}

// split on regex switch/matches
MStatePtr makeRegexState(MDFA* dfa, const PatternRows& ps, size_t c) {
  // remember the match-any rows
  std::set<size_t> matchAnyRows;

  // select this column as a sequence of regular expressions
  Regexes regexes;
  for (size_t r = 0; r < ps.size(); ++r) {
    if (const MatchRegex* mr = is<MatchRegex>(ps[r].patterns[c])) {
      regexes.push_back(mr->value());
    } else if (is<MatchAny>(ps[r].patterns[c])) {
      matchAnyRows.insert(r);
      regexes.push_back(parseRegex(""));
    } else {
      throw annotated_error(*ps[r].patterns[c], "Internal error, invalid pattern table received");
    }
  }

  // make a function to do the regex matching
  CRegexes regexFn = makeRegexFn(dfa->c, regexes, dfa->rootLA);

  // open a char array on the match value (usually this will be a no-op)
  // then call the regex function to get the set of continuation rows to follow and a buffer for captured data
  std::string switchVar   = ps[0].patterns[c]->name();
  std::string oarrayVar   = switchVar + ".a";
  std::string rcaptureVar = switchVar + ".rgxcapture";
  std::string rcheckVar   = switchVar + ".rgxcheck";

  LoadVars::Defs ds;
  ds.push_back(LoadVars::Def(oarrayVar, openArray(dfa, switchVar)));
  ds.push_back(LoadVars::Def(rcaptureVar, regexFn.captureBuffer));
  ds.push_back(LoadVars::Def(rcheckVar,
    fncall(
      var(regexFn.fname, dfa->rootLA), list(
        var(rcaptureVar, dfa->rootLA),
        var(oarrayVar, dfa->rootLA),
        constant(static_cast<long>(0), dfa->rootLA),
        fncall(var("size", dfa->rootLA), list(var(oarrayVar, dfa->rootLA)), dfa->rootLA),
        constant(static_cast<int>(0), dfa->rootLA)),
      dfa->rootLA
    )
  ));

  // based on the match result, branch to a reduced table
  SwitchVal::Jumps sjmps;
  for (const auto& rstate : regexFn.rstates) {
    PatternRows ktbl;
    auto anyr = matchAnyRows.begin();

    for (const auto& r : rstate.second) {
      // all match-any values prior to this row must take priority
      for (; anyr != matchAnyRows.end() && *anyr < r; ++anyr) {
        copyRowWithoutColumn(&ktbl, ps[*anyr], c);
      }

      // then we've matched this row at this column
      copyRowWithoutColumn(&ktbl, ps[r], c);

      // and in case this is already a match-any row, consider it consumed to avoid redundant references
      if (anyr != matchAnyRows.end() && *anyr == r) {
        ++anyr;
      }
    }

    // include all trailing match-any rows at the end of this reduced table
    for (; anyr != matchAnyRows.end(); ++anyr) {
      copyRowWithoutColumn(&ktbl, ps[*anyr], c);
    }

    // in this case, follow this continuation and load variables for it
    sjmps.push_back(
      SwitchVal::Jump(PrimitivePtr(new Int(static_cast<int>(rstate.first), dfa->rootLA)),
        addState(dfa,
          MStatePtr(
            new LoadVars(
              unpackCaptureVars(switchVar, rcaptureVar, regexFn, rstate.first, dfa->rootLA),
              makeDFAState(dfa, ktbl)
            )
          )
        )
      )
    );
  }

  // otherwise if we didn't match anything, branch to a default table containing just the match-any states (if applicable)
  PatternRows def;
  for (auto r : matchAnyRows) {
    copyRowWithoutColumn(&def, ps[r], c);
  }
  stateidx_t defState = def.size() > 0 ? makeDFAState(dfa, def) : nullState;

  // and that's our state ... a load for the regex call and a branch on its result
  return MStatePtr(new LoadVars(ds, addState(dfa, makeLSSwitch(rcheckVar, dfa, sjmps, defState))));
}

// there are five ways that we can deconstruct a pattern table:
//   literal -- a switch on literal values branching to other states
//   array   -- a switch on array sizes, branching to other states (with corresponding element loads)
//              (in the case of char arrays, we do something a little different for fast compile times)
//   regex   -- a switch on regular expressions, branching to states where matched
//   record  -- load the fields of a record and branch to other states
//   variant -- switch on the constructors of a variant and branch to other states
struct makeSuccStateF : public switchPattern<MStatePtr> {
  MDFA* dfa;
  const PatternRows& ps;
  size_t c;
  makeSuccStateF(MDFA* dfa, const PatternRows& ps, size_t c) : dfa(dfa), ps(ps), c(c) { }

  MStatePtr with(const MatchAny* ma) const {
    throw annotated_error(*ma, "Internal error, can't deconstruct wildcard columns in match expression");
  }

  MStatePtr with(const MatchLiteral*) const { return makeLiteralState(dfa, ps, c); }
  MStatePtr with(const MatchRegex*)   const { return makeRegexState(dfa, regexNormalize(ps, c), c); }
  MStatePtr with(const MatchRecord*)  const { return makeRecordState(dfa, ps, c); }
  MStatePtr with(const MatchVariant*) const { return makeVariantState(dfa, ps, c); }

  // if we have a 'match array' column but it's got a regex somewhere, then we actually
  // need to apply regex match logic (otherwise we can match as an array)
  MStatePtr with(const MatchArray*) const {
    for (size_t r = 0; r < ps.size(); ++r) {
      if (is<MatchRegex>(ps[r].patterns[c])) {
        return makeRegexState(dfa, regexNormalize(ps, c), c);
      }
    }
    return makeArrayState(dfa, ps, c);
  }

  // normalize patterns in an entire column into regular expressions
  // (this allows mixing of literal matches and regex matches in a single column)
  static PatternRows regexNormalize(const PatternRows& ps, size_t c) {
    PatternRows nps = ps;
    for (size_t r = 0; r < nps.size(); ++r) {
      if (const MatchArray* ma = is<MatchArray>(nps[r].patterns[c])) {
        nps[r].patterns[c] = MatchRegex::toRegex(*ma);
      }
    }
    return nps;
  }
};

MStatePtr makeSuccState(MDFA* dfa, const PatternRows& ps, size_t c) {
  return switchOf(ps[0].patterns[c], makeSuccStateF(dfa, ps, c));
}

// choose which column to deconstruct
// the current scoring function just gives a rough measurement of how many matches are done in a given column
struct scorePatternF : public switchPattern<size_t> {
  // don't count the same primitive twice
  mutable PrimitiveSet recprims;

  size_t with(const MatchAny*) const {
    return 0;
  }

  size_t with(const MatchLiteral* ml) const {
    return this->recprims.insert(ml->equivConstant()).second ? 1 : 0;
  }

  size_t with(const MatchArray* ma) const {
    size_t s = 1;
    for (size_t i = 0; i < ma->size(); ++i) {
      s += switchOf(ma->pattern(i), *this);
    }
    return s;
  }

  size_t with(const MatchRegex*) const {
    return 1;
  }

  size_t with(const MatchRecord* mr) const {
    size_t s = 0;
    for (size_t i = 0; i < mr->size(); ++i) {
      s += switchOf(mr->pattern(i).second, *this);
    }
    return s;
  }

  size_t with(const MatchVariant* mv) const {
    return 1 + switchOf(mv->value(), *this);
  }
};

size_t columnScore(const PatternRows& ps, size_t c) {
  size_t s = 0;
  scorePatternF matchCounter;
  for (const auto& p : ps) {
    s += switchOf(p.patterns[c], matchCounter);
  }
  return s;
}

size_t choosePivotColumn(const PatternRows& ps) {
  size_t chosenColumn = ps[0].patterns.size();
  size_t currentScore = std::numeric_limits<size_t>::max();

  size_t bestArrayColumn = ps[0].patterns.size();
  size_t bestArrayScore  = std::numeric_limits<size_t>::max();

  for (size_t c = 0; c < ps[0].patterns.size(); ++c) {
    if (!is<MatchAny>(ps[0].patterns[c])) {
      size_t csc = 1 + columnScore(ps, c);
      if (csc < currentScore) {
        chosenColumn = c;
        currentScore = csc;
      }

      if (is<MatchArray>(ps[0].patterns[c]) && csc < bestArrayScore) {
        bestArrayColumn = c;
        bestArrayScore  = csc;
      }
    }
  }

  // prefer array selection (for GLM)
  if (bestArrayColumn < ps[0].patterns.size()) {
    return bestArrayColumn;
  } else {
    return chosenColumn;
  }
}

// is a pattern table just a set of primitive selection rules?
bool isPrimSelection(bool alwaysLowerPrimMatchTables, const PatternRows& ps) {
  // heuristically avoid prim selection derivation for "small tables"
  if (!alwaysLowerPrimMatchTables && ps.size() < 500) {
    return false;
  }

  // there can't be any guards in a primitive selection
  for (size_t r = 0; r < ps.size(); ++r) {
    if (ps[r].guard) {
      return false;
    }
  }

  // any match on a composite value (record, array, variant) doesn't count as primitive
  // we also can't (currently) have convertible values in primitive tests
  bool seemsLegit = false;

  for (size_t c = 0; c < ps[0].patterns.size(); ++c) {
    for (size_t r = 0; r < ps.size(); ++r) {
      if (const MatchLiteral* ml = is<MatchLiteral>(ps[r].patterns[c])) {
        if (ml->expression()) {
          return false;
        } else {
          // one constant is as good as all constants
          seemsLegit = true;
          break;
        }
      } else if (!is<MatchAny>(ps[r].patterns[c])) {
        return false;
      }
    }
  }

  return seemsLegit;
}

// assuming that a pattern table represents a primitive selection, determine its argument sequence
PrimFArgs makePrimFArgs(const PatternRows& ps) {
  PrimFArgs args;
  for (size_t c = 0; c < ps[0].patterns.size(); ++c) {
    for (size_t r = 0; r < ps.size(); ++r) {
      if (const MatchLiteral* ml = is<MatchLiteral>(ps[r].patterns[c])) {
        args.push_back(PrimFArg(ml->name(), ml->equivConstant()->primType()));
        break;
      }
    }
  }
  return args;
}

// does it make sense and is it worthwhile to decompose this table column-wise?
bool shouldDecomposeColumnwise(MDFA* dfa, const PatternRows& ps) {
  // it only makes sense to decompose columnwise if the table has at least one row, at least two columns
  if (ps.size() == 0 || ps[0].patterns.size() <= 1) {
    return false;
  }

  // there's some overhead with this method, so only do it if the user asks for it
  if (!dfa->c->buildColumnwiseMatches()) {
    return false;
  }

  // don't bother doing this for small tables
  if (ps.size() * ps[0].patterns.size() < 100) {
    return false;
  }

  // the logic in makeColRowsetCalcExpr assumes column counts fit in a byte
  if (ps[0].patterns.size() > 256) {
    return false;
  }

  // OK let's do it!
  return true;
}

// calculate a set of implied rows from a single column of a match table
ExprPtr makeColRowsetCalcExpr(MDFA* dfa, const PatternRows& ps, size_t c, const std::string& rcVarName, std::vector<uint8_t>* skipCounts) {
  bool isFinal = c == ps[0].patterns.size() - 1;

  PatternRows cps;
  for (size_t r = 0; r < ps.size(); ++r) {
    auto rcVar = fncall(var("saelem", dfa->rootLA), list(var(rcVarName, dfa->rootLA), constant(static_cast<size_t>(r), dfa->rootLA)), dfa->rootLA);

    if (!isFinal) {
      if (refutable(ps[r].patterns[c])) {
        auto markRowExpr = assign(rcVar, fncall(var("badd", dfa->rootLA), list(rcVar, constant(static_cast<uint8_t>(1), dfa->rootLA)), dfa->rootLA), dfa->rootLA);
        cps.push_back(PatternRow(list(ps[r].patterns[c]), let("_", markRowExpr, constant(false, dfa->rootLA), dfa->rootLA), mktunit(dfa->rootLA)));
      } else {
        ++(*skipCounts)[r];
      }
    } else {
      if (r < ps.size()-1) {
        auto checkCount = fncall(var("beq", dfa->rootLA), list(rcVar, constant(static_cast<uint8_t>(ps[0].patterns.size()-(1+(*skipCounts)[r])), dfa->rootLA)), dfa->rootLA);
        cps.push_back(PatternRow(list(ps[r].patterns[c]), ps[r].guard ? fncall(var("and",dfa->rootLA), list(checkCount, ps[r].guard), dfa->rootLA) : checkCount, ps[r].result));
      } else {
        if (ps[r].guard != ExprPtr()) {
          throw annotated_error(*ps[r].guard, "Inexhaustive patterns in match expression after guard");
        }
        cps.push_back(PatternRow(list(ps[r].patterns[c]), ps[r].result));
      }

      // always ref the row result (may leave unreachable rows undetected)
      addState(dfa, MStatePtr(new FinishExpr(ps[r].result)));
    }
  }
  if (!isFinal) {
    cps.push_back(PatternRow(list(PatternPtr(new MatchAny("_", dfa->rootLA))), mktunit(dfa->rootLA)));
  }

  return liftDFAExpr(dfa->c, cps, dfa->rootLA);
}

// convert a pattern table to a DFA state by aggregating test results for each column
stateidx_t makeColAggrDFAState(MDFA* dfa, const PatternRows& ps) {
  // make an expr for each column to compute its implied rowset
  LoadVars::Defs       sdefs;
  std::string          rcname = ".rc." + freshName();
  std::vector<uint8_t> skipCounts(ps.size(), 0);
  
  sdefs.push_back(LoadVars::Def(rcname, assume(fncall(var("newPrimZ", dfa->rootLA), list(mktunit(dfa->rootLA)), dfa->rootLA), arrayty(primty("byte"), ps.size()), dfa->rootLA)));
  for (size_t c = 0; c < ps[0].patterns.size()-1; ++c) {
    sdefs.push_back(LoadVars::Def("_", makeColRowsetCalcExpr(dfa, ps, c, rcname, &skipCounts)));
  }

  return
    addState(dfa, MStatePtr(
      new LoadVars(
        sdefs,
        addState(dfa, MStatePtr(new FinishExpr(makeColRowsetCalcExpr(dfa, ps, ps[0].patterns.size()-1, rcname, &skipCounts))))
      )
    ));
}

// convert a pattern table to a DFA state by picking a column to discriminate on, branching to subtables on that column value
stateidx_t makeColPivotDFAState(MDFA* dfa, const PatternRows& ps) {
  stateidx_t result = nullState;

  // choose a column to deconstruct
  // if no column can be chosen, there is only one path -- the immediate return expression
  size_t c = choosePivotColumn(ps);
  if (c < ps[0].patterns.size()) {
    if (dfa->inPrimSel || !isPrimSelection(dfa->c->alwaysLowerPrimMatchTables(), ps)) {
      result = addState(dfa, makeSuccState(dfa, ps, c));
    } else {
      dfa->inPrimSel = true;
      MStatePtr succState = makeSuccState(dfa, ps, c);
      dfa->inPrimSel = false;

      succState->isPrimMatchRoot = true;
      succState->primFArgs       = makePrimFArgs(ps);
      result = addState(dfa, succState);
    }
  } else if (ps[0].guard) {
    SwitchVal::Jumps jmps;
    jmps.push_back(SwitchVal::Jump(PrimitivePtr(new Bool(true, dfa->rootLA)), addState(dfa, MStatePtr(new FinishExpr(ps[0].result)))));

    PatternRows tail;
    tableTail(&tail, ps);
    if (tail.size() == 0) {
      throw annotated_error(*ps[0].guard, "Inexhaustive patterns in match expression after guard");
    }

    size_t guardS = addState(dfa, MStatePtr(new SwitchVal(".guardcheck", jmps, makeDFAState(dfa, tail))));

    LoadVars::Defs ds;
    ds.push_back(LoadVars::Def(".guardcheck", ps[0].guard));
    result = addState(dfa, MStatePtr(new LoadVars(ds, guardS)));
  } else {
    result = addState(dfa, MStatePtr(new FinishExpr(ps[0].result)));
  }

  // next time we come through, just use the state implied by the table config
  dfa->tableCfgStates[ps] = result;

  // and that's it
  return result;
}

// make a state out of the input pattern table (recursively constructing sub-states as necessary)
stateidx_t makeDFAState(MDFA* dfa, const PatternRows& xps) {
  PatternRows ps;
  dropUnusedColumns(&ps, xps);

  // if we can deconstruct strings here, do it before anything else
  // (it has a potential runtime performance impact and should only be done to reduce compilation time for large schemas)
  size_t strc = canMakeCharArrStateAtColumn(dfa, ps);
  if (strc < ps[0].patterns.size()) {
    return addState(dfa, makeCharArrayState(dfa, ps, strc));
  }

  // did we already produce this state?  if so, just add to its ref-count and return it
  auto tcfg = dfa->tableCfgStates.find(ps);
  if (tcfg != dfa->tableCfgStates.end()) {
    addRef(dfa, tcfg->second);
    return tcfg->second;
  }

  // for very large tables, we can avoid giant DFAs and long compile times by column-wise decomposition
  // otherwise, create a branch point with sub-tables based on a chosen column
  if (shouldDecomposeColumnwise(dfa, ps)) {
    return makeColAggrDFAState(dfa, ps);
  } else {
    return makeColPivotDFAState(dfa, ps);
  }
}

// deconstruct the pattern match table to produce the equivalent DFA
stateidx_t makeDFA(MDFA* dfa, const PatternRows& ps, const LexicalAnnotation& la) {
  dfa->rootLA = la;

  // start by adding 0-ref states and placeholder parameters for each final expression
  std::vector<stateidx_t> finalStates;
  for (auto pr : ps) {
    stateidx_t fst = addState(dfa, MStatePtr(new FinishExpr(pr.result)), false);
    finalStates.push_back(fst);
    dfa->exprIdxs[pr.result.get()] = fst;
  }

  // recursively add states for each step of deconstruction of the match table
  stateidx_t rootS = makeDFAState(dfa, ps);

  // make sure that every provided final state is reachable
  if (dfa->c->requireMatchReachability()) {
    std::vector<size_t> unreachableRows;
    for (size_t r = 0; r < ps.size(); ++r) {
      size_t fs = finalStates[r];
 
      if (dfa->states[fs]->refs == 0) {
        unreachableRows.push_back(r);
      }
    }
  
    if (unreachableRows.size() > 0) {
      std::ostringstream fss;
      fss << "Unreachable row" << (unreachableRows.size() > 1 ? "s" : "") << " in match expression:\n";
      for (size_t ur : unreachableRows) {
        fss << "  " << show(ps[ur]) << std::endl;
      }
      throw annotated_error(la, fss.str());
    }
  }

  // save unreachable rows for the caller instead of raising an error
  if (dfa->c->unreachableMatchRowsPtr) {
    for (size_t r = 0; r < ps.size(); ++r) {
      size_t fs = finalStates[r];

      if (dfa->states[fs]->refs == 0) {
        dfa->c->unreachableMatchRowsPtr->push_back(std::make_pair(r, ps[r]));
      }
    }
  }

  // OK, this is a good DFA
  return rootS;
}

// translate a pattern match table to an equivalent low level matching expression
ExprPtr liftDFAExpr(MDFA* dfa, stateidx_t state);

struct liftDFAExprF : public switchMState<ExprPtr> {
  MDFA* dfa;
  liftDFAExprF(MDFA* dfa) : dfa(dfa) { }

  ExprPtr with(const LoadVars* x) const {
    const LoadVars::Defs& ds = x->defs();
    ExprPtr b = liftDFAExpr(this->dfa, x->nextState());

    for (auto d = ds.rbegin(); d != ds.rend(); ++d) {
      b = let(d->first, d->second, b, dfa->rootLA);
    }

    return b;
  }

  ExprPtr with(const SwitchVal* x) const {
    ExprPtr def = x->defaultState() == nullState ? ExprPtr() : liftDFAExpr(this->dfa, x->defaultState());

    const SwitchVal::Jumps& jmps = x->jumps();
    Switch::Bindings bs;
    for (const auto& jmp : jmps) {
      bs.push_back(Switch::Binding(jmp.first, liftDFAExpr(this->dfa, jmp.second)));
    }

    return ExprPtr(new Switch(varName(this->dfa, x->switchVar()), bs, def, dfa->rootLA));
  }

  ExprPtr with(const SwitchVariant* x) const {
    ExprPtr def = x->defaultState() == nullState ? ExprPtr() : liftDFAExpr(this->dfa, x->defaultState());

    const SwitchVariant::CtorJumps& jmps = x->jumps();
    Case::Bindings bs;
    for (const auto& jmp : jmps) {
      bs.push_back(Case::Binding(jmp.first, x->switchVar() + ".1", liftDFAExpr(this->dfa, jmp.second)));
    }

    return ExprPtr(new Case(varName(this->dfa, x->switchVar()), bs, def, dfa->rootLA));
  }

  ExprPtr with(const FinishExpr* x) const {
    return x->expr();
  }
};

typedef std::pair<ExprPtr, PrimitivePtr> ExprCheck;
typedef std::vector<ExprCheck> ExprChecks;

ExprPtr checkExpr(const ExprChecks& cs, const LexicalAnnotation& la) {
  if (cs.size() == 0) {
    throw annotated_error(la, "Internal error, can't produce empty check expression");
  } else {
    ExprPtr v = fncall(var("===", la), list(cs[0].first, std::dynamic_pointer_cast<Expr>(cs[0].second)), la);

    for (size_t i = 1; i < cs.size(); ++i) {
      v = fncall(var("and", la), list(v, fncall(var("===", la), list(cs[i].first, std::dynamic_pointer_cast<Expr>(cs[i].second)), la)), la);
    }

    return v;
  }
}

struct IfCheck {
  IfCheck() : trueb(nullState), falseb(nullState) { }

  ExprChecks check;
  stateidx_t trueb;
  stateidx_t falseb;
};

bool compressToIfCheck(MDFA* dfa, const SwitchVal& sv, IfCheck* out) {
  if (sv.jumps().size() == 1) {
    if (out->falseb == nullState || out->falseb == sv.defaultState()) {
      out->check.push_back(ExprCheck(varName(dfa, sv.switchVar()), sv.jumps()[0].first));
      out->trueb  = sv.jumps()[0].second;
      out->falseb = sv.defaultState();

      if (const SwitchVal* nsv = is<SwitchVal>(dfa->states[out->trueb])) {
        compressToIfCheck(dfa, *nsv, out);
        return true;
      } else {
        return true;
      }
    } else {
      return false;
    }
  } else {
    return false;
  }
}

ExprPtr liftDFAExprWithSwitchCompression(MDFA* dfa, stateidx_t state) {
  const MStatePtr& s = dfa->states[state];

  if (const SwitchVal* sv = is<SwitchVal>(s)) {
    IfCheck ifc;
    if (compressToIfCheck(dfa, *sv, &ifc)) {
      return fncall(var("if", dfa->rootLA), list(checkExpr(ifc.check, dfa->rootLA), liftDFAExpr(dfa, ifc.trueb), liftDFAExpr(dfa, ifc.falseb)), dfa->rootLA);
    }
  }

  return switchOf(s, liftDFAExprF(dfa));
}

bool shouldInlineState(const MDFA* dfa, stateidx_t state) {
  const MStatePtr& s = dfa->states[state];

  if (dfa->states[state]->isPrimMatchRoot) {
    return false;
  } else if (s->refs <= 1) {
    return true;
  } else if (const FinishExpr* fe = is<FinishExpr>(s)) {
    return isConst(fe->expr()) || is<Var>(fe->expr());
  } else {
    return false;
  }
}

ExprPtr liftPrimMatchExpr(MDFA* dfa, stateidx_t state);

ExprPtr liftDFAExpr(MDFA* dfa, stateidx_t state) {
  if (shouldInlineState(dfa, state)) {
    return liftDFAExprWithSwitchCompression(dfa, state);
  } else {
    FoldedStateCalls::const_iterator fsc = dfa->foldedStateCalls.find(state);

    if (fsc != dfa->foldedStateCalls.end()) {
      return fsc->second;
    } else if (dfa->states[state]->isPrimMatchRoot) {
      return liftPrimMatchExpr(dfa, state);
    } else {
      ExprPtr  def   = liftDFAExprWithSwitchCompression(dfa, state);
      str::set fvnst = setDifference(freeVars(def), dfa->rootVars);
      str::seq fvns  = str::seq(fvnst.begin(), fvnst.end());

      std::string stateFn = ".patfs." + str::from(state);
      ExprPtr callexp = fncall(varName(dfa, stateFn), vars(fvns, dfa->rootLA), dfa->rootLA);

      dfa->foldedStates.push_back(FoldedState(stateFn, ExprPtr(new Fn(fvns, def, dfa->rootLA))));
      dfa->foldedStateCalls[state] = callexp;
      dfa->rootVars.insert(stateFn);

      return callexp;
    }
  }
}

ExprPtr liftDFAExpr(cc* c, const PatternRows& ps, const LexicalAnnotation& rootLA) {
  MDFA pdfa;
  pdfa.rootVars  = c->typeEnv()->boundVariables();
  pdfa.c         = c;
  pdfa.inPrimSel = false;

  stateidx_t initState = makeDFA(&pdfa, ps, rootLA);
  ExprPtr    me        = liftDFAExpr(&pdfa, initState);

  if (pdfa.foldedStates.size() == 0) {
    return me;
  } else {
    LetRec::Bindings bs;
    for (const auto& fs : pdfa.foldedStates) {
      bs.push_back(LetRec::Binding(fs.first, fs.second));
    }
    return ExprPtr(new LetRec(bs, me, rootLA));
  }
}

// determine the row/expr set reachable from a DFA state
typedef std::map<size_t, ExprPtr> RowResults;

struct reachableRowExprsF : public switchMState<UnitV> {
  const MDFA* dfa;
  RowResults* results;

  reachableRowExprsF(const MDFA* dfa, RowResults* results) : dfa(dfa), results(results) { }

  UnitV with(const SwitchVal* x) const {
    if (x->defaultState() != nullState) {
      switchOf(this->dfa->states[x->defaultState()], *this);
    }
    for (const auto& jmp : x->jumps()) {
      switchOf(this->dfa->states[jmp.second], *this);
    }
    return unitv;
  }

  UnitV with(const FinishExpr* x) const {
    auto ei = this->dfa->exprIdxs.find(x->expr().get());
    if (ei == this->dfa->exprIdxs.end()) {
      throw std::runtime_error("Internal error, match DFA returns non-indexed expression");
    } else {
      (*this->results)[ei->second] = x->expr();
    }
    return unitv;
  }
  
  UnitV with(const LoadVars* x) const {
    switchOf(this->dfa->states[x->nextState()], *this);
    return unitv;
  }

  UnitV with(const SwitchVariant* x) const {
    if (x->defaultState() != nullState) {
      switchOf(this->dfa->states[x->defaultState()], *this);
    }
    for (const auto& jmp : x->jumps()) {
      switchOf(this->dfa->states[jmp.second], *this);
    }
    return unitv;
  }
};

RowResults findRowResults(MDFA* dfa, stateidx_t s) {
  RowResults result;
  switchOf(dfa->states[s], reachableRowExprsF(dfa, &result));
  return result;
}

long asLongRep(long*  x) { return *x; }
long asLongRep(double x) { return asLongRep(reinterpret_cast<long*>(&x)); }

// derive a primitive search function from a DFA (or sub-DFA) that contains _only_ primitive tests
typedef std::map<std::string, llvm::Value*>      Args;
typedef std::map<stateidx_t,  llvm::BasicBlock*> StateBranches;

struct makePrimDFASF : public switchMState<UnitV> {
  const Args&    args;
  StateBranches* branches;
  MDFA*          dfa;

  makePrimDFASF(const Args& args, StateBranches* branches, MDFA* dfa) : args(args), branches(branches), dfa(dfa) {
  }

  llvm::Value* arg(const std::string& an) const {
    auto a = this->args.find(an);
    if (a == this->args.end()) {
      throw std::runtime_error("Internal error, reference to undefined variable in primitive match: " + an);
    } else {
      return a->second;
    }
  }

  UnitV with(const SwitchVal* x) const {
    if (x->defaultState() == nullState) {
      if (x->jumps().size() == 0) {
        throw std::runtime_error("Internal error, empty switch statement in primitive match compilation");
      } else {
        throw std::runtime_error("Internal error, switches without default states not supported for primitive match compilation (on switch for " + x->switchVar() + " :: " + show(x->jumps()[0].first->primType()) + " with " + str::from(x->jumps().size()) + " cases)");
      }
    }

    if (x->jumps().size() > 0 && is<Double>(x->jumps().begin()->first)) {
      llvm::SwitchInst* s = this->dfa->c->builder()->CreateSwitch(this->dfa->c->builder()->CreateBitCast(arg(x->switchVar()), longType()), blockForState(x->defaultState()), x->jumps().size());
      for (const auto& jmp : x->jumps()) {
        if (const Double* d = is<Double>(jmp.first)) {
          s->addCase(civalue(asLongRep(d->value())), blockForState(jmp.second));
        }
      }
    } else {
      llvm::SwitchInst* s = this->dfa->c->builder()->CreateSwitch(arg(x->switchVar()), blockForState(x->defaultState()), x->jumps().size());
      for (const auto& jmp : x->jumps()) {
        s->addCase(toLLVMConstantInt(jmp.first), blockForState(jmp.second));
      }
    }
    return unitv;
  }

  UnitV with(const FinishExpr* x) const {
    auto ei = this->dfa->exprIdxs.find(x->expr().get());
    if (ei == this->dfa->exprIdxs.end()) {
      throw std::runtime_error("Internal error, primitive match DFA returns non-indexed expression");
    } else {
      this->dfa->c->builder()->CreateRet(cvalue(static_cast<int>(ei->second)));
    }
    return unitv;
  }
  
  UnitV with(const LoadVars*) const {
    throw std::runtime_error("Internal error, not a primitive match table (load vars)");
  }

  UnitV with(const SwitchVariant*) const {
    throw std::runtime_error("Internal error, not a primitive match table (switch variant)");
  }

  llvm::BasicBlock* blockForState(stateidx_t s) const {
    auto b = this->branches->find(s);
    if (b != this->branches->end()) {
      return b->second;
    } else {
      llvm::BasicBlock* obb = this->dfa->c->builder()->GetInsertBlock();
      llvm::BasicBlock* bb  = llvm::BasicBlock::Create(context(), ".pmst" + freshName(), obb->getParent());
      this->dfa->c->builder()->SetInsertPoint(bb);
      switchOf(this->dfa->states[s], *this);
      (*this->branches)[s] = bb;
      this->dfa->c->builder()->SetInsertPoint(obb);
      return bb;
    }
  }
};

llvm::Function* makePrimMatchDFAFunc(const std::string& fname, MDFA* dfa, stateidx_t s, const PrimFArgs& args) {
  Types atys;
  for (const auto& arg : args) {
    if (!isUnit(arg.second)) {
      atys.push_back(toLLVM(arg.second, true));
    }
  }

  llvm::Function*   result = llvm::Function::Create(llvm::FunctionType::get(intType(), atys, false), llvm::Function::ExternalLinkage, fname, dfa->c->module());
  llvm::BasicBlock* bb     = llvm::BasicBlock::Create(context(), "entry", result);

  dfa->c->builder()->SetInsertPoint(bb);

  Args fargs;
  llvm::Function::arg_iterator a = result->arg_begin();
  for (unsigned int i = 0; i < args.size(); ++i) {
    if (isUnit(args[i].second)) {
      fargs[args[i].first] = cvalue(true);
    } else {
      a->setName(args[i].first);
      fargs[args[i].first] = &*a;
      ++a;
    }
  }

  StateBranches sb;
  switchOf(dfa->states[s], makePrimDFASF(fargs, &sb, dfa));

  return result;
}

class primdfafunc : public op {
public:
  primdfafunc(llvm::Function* vfn, const PrimFArgs& args) : vfn(vfn) {
    MonoTypes atys;
    if (args.size() == 0) {
      atys.push_back(MonoTypePtr(Prim::make("unit")));
    } else {
      for (const auto& arg : args) {
        atys.push_back(arg.second);
      }
    }
    this->ftype = polytype(functy(atys, primty("int")));
  }

  llvm::Value* apply(jitcc* c, const MonoTypes&, const MonoTypePtr&, const Exprs& es) {
    return fncall(c->builder(), this->vfn, compileArgs(c, es));
  }

  PolyTypePtr type(typedb&) const {
    return this->ftype;
  }
private:
  llvm::Function* vfn;
  PolyTypePtr     ftype;
};

void makeCompiledPrimMatchFunction(const std::string& fname, MDFA* dfa, stateidx_t state) {
  const MStatePtr& mstate = dfa->states[state];
  dfa->c->bindLLFunc(fname, new primdfafunc(makePrimMatchDFAFunc(fname, dfa, state, mstate->primFArgs), mstate->primFArgs));
}

// derive a primitive match DFA to run in an interpreted mode, to minimize compilation overhead
typedef variant<int, long> IDFATransition;
typedef array< std::pair<long, IDFATransition> > IDFATransitions;

struct IDFAState {
  long             reads;
  IDFATransitions* transitions;
  IDFATransition   def;
};

MonoTypePtr dfaStateType() {
  Variant::Members tcns;
  tcns.push_back(Variant::Member("done", MonoTypePtr(Prim::make("int")),  0));
  tcns.push_back(Variant::Member("step", MonoTypePtr(Prim::make("long")), 1));
  MonoTypePtr tty(Variant::make(tcns));

  Record::Members dtfns;
  dtfns.push_back(Record::Member(".f0", MonoTypePtr(Prim::make("long"))));
  dtfns.push_back(Record::Member(".f1", tty));
  MonoTypePtr dtty(Record::make(dtfns));

  Record::Members dfafns;
  dfafns.push_back(Record::Member("reads",       MonoTypePtr(Prim::make("long"))));
  dfafns.push_back(Record::Member("transitions", arrayty(dtty)));
  dfafns.push_back(Record::Member("def",         tty));
  return arrayty(MonoTypePtr(Record::make(dfafns)));
}

typedef std::unordered_map<std::string, size_t> ArgPos;
typedef std::unordered_map<size_t, size_t>      GlobalToLocalState;

void mapStatesFrom(MDFA* dfa, stateidx_t state, GlobalToLocalState* localstate) {
  if (localstate->find(state) == localstate->end()) {
    if (state > dfa->states.size()) {
      throw std::runtime_error("Internal error, invalid state produced for primitive DFA interpretation");
    } else if (const SwitchVal* sv = is<SwitchVal>(dfa->states[state])) {
      size_t lc = localstate->size();
      (*localstate)[state] = lc;
      for (const auto& jmp : sv->jumps()) {
        mapStatesFrom(dfa, jmp.second, localstate);
      }

      if (sv->defaultState() == nullState) {
        throw std::runtime_error("Internal error, primitive DFAs without default states not supported for interpretation");
      }
      mapStatesFrom(dfa, sv->defaultState(), localstate);
    } else if (!is<FinishExpr>(dfa->states[state])) {
      throw std::runtime_error("Internal error, invalid primitive DFA for interpretation");
    }
  }
}

size_t localState(const GlobalToLocalState& localstate, stateidx_t state) {
  auto ls = localstate.find(state);
  if (ls == localstate.end()) {
    throw std::runtime_error("Internal error, incorrectly determined local state index");
  } else {
    return ls->second;
  }
}

size_t reads(const ArgPos& argpos, const std::string& a) {
  auto ai = argpos.find(a);
  if (ai == argpos.end()) {
    throw std::runtime_error("Internal error, undefined argument: " + a);
  } else {
    return ai->second;
  }
}

IDFATransitions* transitions(const ArgPos&, MDFA*, const SwitchVal::Jumps&, const GlobalToLocalState&, std::set<stateidx_t>*, array<IDFAState>*);
IDFATransition transitionDef(const ArgPos&, MDFA*, stateidx_t, const GlobalToLocalState&, std::set<stateidx_t>*, array<IDFAState>*);

void copyStateDef(const ArgPos& argpos, MDFA* dfa, stateidx_t state, const GlobalToLocalState& localstate, std::set<stateidx_t>* dones, array<IDFAState>* dfaStates) {
  // don't copy the same state twice
  if (dones->find(state) != dones->end()) {
    return;
  }
  dones->insert(state);

  // expect a primitive switch state at a predetermined output index
  const SwitchVal* sv     = is<SwitchVal>(dfa->states[state]);
  size_t           statei = localState(localstate, state);

  if (!sv) {
    throw std::runtime_error("Internal error, expected primitive switch for interpreted DFA");
  }

  // encode this particular state structure
  IDFAState& staterec = dfaStates->data[statei];

  staterec.reads       = reads(argpos, sv->switchVar());
  staterec.transitions = transitions(argpos, dfa, sv->jumps(), localstate, dones, dfaStates);
  staterec.def         = transitionDef(argpos, dfa, sv->defaultState(), localstate, dones, dfaStates);
}

IDFATransitions* transitions(const ArgPos& argpos, MDFA* dfa, const SwitchVal::Jumps& jmps, const GlobalToLocalState& localstate, std::set<stateidx_t>* dones, array<IDFAState>* dfaStates) {
  typedef std::map<long, IDFATransition> SortedSVJumps;

  SortedSVJumps ssvj;
  for (const auto& jmp : jmps) {
    if (const Long* lv = is<Long>(jmp.first)) {
      ssvj[lv->value()] = transitionDef(argpos, dfa, jmp.second, localstate, dones, dfaStates);
    } else {
      throw std::runtime_error("Internal error, expected long value in DFA transition data");
    }
  }

  size_t msz = sizeof(long) + (ssvj.size() * sizeof(std::pair<long, IDFATransition>));
  IDFATransitions* result = reinterpret_cast<IDFATransitions*>(malloc(msz));
  memset(result, 0, msz);
  result->size = ssvj.size();
  size_t i = 0;
  for (const auto& svj : ssvj) {
    new (&result->data[i]) std::pair<long, IDFATransition>();
    result->data[i].first  = svj.first;
    result->data[i].second = svj.second;
    ++i;
  }
  return result;
}

IDFATransition transitionDef(const ArgPos& argpos, MDFA* dfa, stateidx_t s, const GlobalToLocalState& localstate, std::set<stateidx_t>* dones, array<IDFAState>* dfaStates) {
  if (const FinishExpr* fe = is<FinishExpr>(dfa->states[s])) {
    auto ei = dfa->exprIdxs.find(fe->expr().get());
    if (ei == dfa->exprIdxs.end()) {
      throw std::runtime_error("Internal error, unexpected result expression");
    }
    return IDFATransition(static_cast<int>(ei->second));
  } else {
    long nextState = localState(localstate, s);
    copyStateDef(argpos, dfa, s, localstate, dones, dfaStates);
    return IDFATransition(static_cast<long>(nextState));
  }
}

void makeInterpretedPrimMatchFunction(const std::string& fname, MDFA* dfa, stateidx_t state) {
  const MStatePtr& mstate = dfa->states[state];

  // figure out what our arguments are
  str::seq vnames;
  Exprs    vargs;
  ArgPos   argpos;
  for (const auto& arg : mstate->primFArgs) {
    argpos[arg.first] = vargs.size();
    vnames.push_back(arg.first);
    vargs.push_back(varName(dfa, arg.first));
  }

  // map global states to local positions
  GlobalToLocalState localstate;
  mapStatesFrom(dfa, state, &localstate);

  // construct the DFA description in a consumable format
  size_t msz = sizeof(long) + (localstate.size() + sizeof(IDFAState));
  array<IDFAState>* dfaStates = reinterpret_cast<array<IDFAState>*>(malloc(msz));
  memset(dfaStates, 0, msz);
  dfaStates->size = localstate.size();

  std::set<stateidx_t> dones;
  copyStateDef(argpos, dfa, state, localstate, &dones, dfaStates);

  // bind this DFA def to the global data set for this evaluator
  std::string dfavar = fname + "_data";
  dfa->c->bind(polytype(dfaStateType()), dfavar, dfaStates);

  // then we can interpret this DFA by filling in a fresh argument vector and then running our generic DFA evaluation function over the definition
  ExprPtr runDFADef =
    ExprPtr(
      new Fn(
        vnames,
        fncall(varName(dfa, "runLongDFA"), list(var(dfavar, dfa->rootLA), ExprPtr(new Long(0, dfa->rootLA)), ExprPtr(new MkArray(vargs, dfa->rootLA))), dfa->rootLA),
        dfa->rootLA
      )
    );

  dfa->foldedStates.push_back(FoldedState(fname, runDFADef));
}

bool canMakeInterpretedPrimMatchFunction(MDFA* dfa, stateidx_t state) {
  const MStatePtr& mstate = dfa->states[state];
  for (const auto& arg : mstate->primFArgs) {
    if (const Prim* pty = is<Prim>(arg.second)) {
      if (pty->name() != "long") {
        return false;
      }
    } else {
      return false;
    }
  }
  return true;
}

// given that we want to compile a primitive DFA, decide how best to do it
ExprPtr liftPrimMatchExpr(MDFA* dfa, stateidx_t state) {
  static size_t    c      = 0;
  std::string      fname  = ".pm.gen." + str::from(c++);
  const MStatePtr& mstate = dfa->states[state];

  if (dfa->c->buildInterpretedMatches() && canMakeInterpretedPrimMatchFunction(dfa, state)) {
    makeInterpretedPrimMatchFunction(fname, dfa, state);
  } else {
    makeCompiledPrimMatchFunction(fname, dfa, state);
  }

  Exprs findEndStateArgs;
  for (const auto& arg : mstate->primFArgs) {
    findEndStateArgs.push_back(varName(dfa, arg.first));
  }

  // defer to the primitive match, switch on the determined row
  Switch::Bindings bs;
  for (const auto& r : findRowResults(dfa, state)) {
    bs.push_back(Switch::Binding(PrimitivePtr(new Int(static_cast<int>(r.first), dfa->rootLA)), r.second));
  }
  return ExprPtr(new Switch(fncall(var(fname, dfa->rootLA), findEndStateArgs, dfa->rootLA), bs, bs[0].exp, dfa->rootLA));
}

}

