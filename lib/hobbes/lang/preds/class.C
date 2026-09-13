
#include <hobbes/lang/expr.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/lang/typepreds.H>
#include <hobbes/lang/tyunqualify.H>
#include <hobbes/util/array.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/perf.H>
#include <atomic>
#include <exception>
#include <memory>
#include <unordered_map>

namespace hobbes {

// Instance resolution recurses through instance generators: applying one
// resolves the constraints in its context, and each of those may apply
// another. The memos in TClass (testedInstances, satfInstances) stop the
// recursion when a constraint comes back to a type it has already been asked
// about, which is what a well-founded recursive instance does. An instance
// whose context asks about a type strictly larger than its head -- say
// (Grow [a]) => Grow a -- never comes back to one: every step asks about a
// new, bigger type, and nothing bounds the descent but the stack (the
// compiler ran for minutes and then crashed on that instance).
//
// Depth alone doesn't tell the two apart. The Prelude's record and tuple
// instances recurse once per field (ShowR, PrintR, Eq, ...), so a 300-field
// record legitimately nests 300 generators deep, and a group of mutually
// recursive instances nests once per member while their dictionaries are
// bound. What marks divergence is growth: each step asks about a type larger
// than the last, without bound. So each generator step measures the type it
// is asked about, and a resolution is rejected, with a message that says
// why, when either
//
//   * more than this many steps on the stack each ask about a larger type
//     than the step that asked for them (a well-founded descent asks about
//     smaller types -- a field of the record, a member of the group -- and
//     the few legitimate steps that grow, like a compression model asking
//     about the model type for its data type, are followed by shrinking
//     ones; loading the Prelude and running the test suite never stack more
//     than two such steps), or
//
//   * a step asks about a type this many times larger than the outermost
//     request, plus a fixed allowance so that a small request is not held to
//     a small bound (the count above stops slow growth in a few hundred
//     steps; an instance whose context doubles its head would reach an
//     unrepresentable type long before that many, and this stops it in a
//     dozen; the largest growth the Prelude or the test suite asks for is
//     11x, that compression model).
const size_t maxInstanceResolutionGrowthSteps = 256;
const size_t maxInstanceResolutionGrowth      = 32;
const size_t instanceResolutionAllowance      = 4096;

namespace {
  struct instance_resolution_depth_error : public std::runtime_error {
    using std::runtime_error::runtime_error;
  };

  // the size of a type is its node count, the measure in which an instance
  // context "larger than its head" is larger
  struct typeSizeF : public walkTy {
    mutable size_t n = 0;
    UnitV with(const Prim*       v) const override { ++n; return walkTy::with(v); }
    UnitV with(const OpaquePtr*  v) const override { ++n; return walkTy::with(v); }
    UnitV with(const TVar*       v) const override { ++n; return walkTy::with(v); }
    UnitV with(const TGen*       v) const override { ++n; return walkTy::with(v); }
    UnitV with(const TAbs*       v) const override { ++n; return walkTy::with(v); }
    UnitV with(const TApp*       v) const override { ++n; return walkTy::with(v); }
    UnitV with(const FixedArray* v) const override { ++n; return walkTy::with(v); }
    UnitV with(const Array*      v) const override { ++n; return walkTy::with(v); }
    UnitV with(const Variant*    v) const override { ++n; return walkTy::with(v); }
    UnitV with(const Record*     v) const override { ++n; return walkTy::with(v); }
    UnitV with(const Func*       v) const override { ++n; return walkTy::with(v); }
    UnitV with(const Exists*     v) const override { ++n; return walkTy::with(v); }
    UnitV with(const Recursive*  v) const override { ++n; return walkTy::with(v); }
    UnitV with(const TString*    v) const override { ++n; return walkTy::with(v); }
    UnitV with(const TLong*      v) const override { ++n; return walkTy::with(v); }
    UnitV with(const TExpr*      v) const override { ++n; return walkTy::with(v); }
  };

  size_t typeSize(const MonoTypes& tys) {
    typeSizeF f;
    for (const auto& ty : tys) {
      switchOf(ty, f);
    }
    return f.n;
  }

  // the size of the request at each generator step on this thread's stack
  // (the front is the outermost request), and how many of those steps grew
  thread_local std::vector<size_t> instanceResolutionSizes;
  thread_local size_t              instanceResolutionGrowthSteps = 0;

  // how many resolutions are in progress on this thread: a generator step
  // or a class holding a provisional memo entry (below) each count as one
  thread_local size_t resolutionsInProgress = 0;

  // one instance generator step, entered at TCInstanceFn::apply and
  // TCInstanceFn::satisfiable (the only places resolution recurses without a
  // memo to stop it)
  struct InstanceResolutionStep {
    InstanceResolutionStep(const std::string& tcname, const MonoTypes& tys) : grew(false) {
      size_t n = typeSize(tys);
      if (!instanceResolutionSizes.empty()) {
        size_t root = instanceResolutionSizes.front();
        if (n > maxInstanceResolutionGrowth * root + instanceResolutionAllowance) {
          throw instance_resolution_depth_error(
            "instance resolution for " + show(Constraint(tcname, tys)) + " asks about a type of " + str::from(n) +
            " nodes, more than " + str::from(maxInstanceResolutionGrowth) + "x the " + str::from(root) +
            "-node request it is resolving (an instance whose context is larger than its head would do this)"
          );
        }
        this->grew = n > instanceResolutionSizes.back();
        if (this->grew && instanceResolutionGrowthSteps >= maxInstanceResolutionGrowthSteps) {
          throw instance_resolution_depth_error(
            "instance resolution for " + show(Constraint(tcname, tys)) + " asks about a larger type than the step before it, for the " +
            str::from(maxInstanceResolutionGrowthSteps + 1) + "th time (an instance whose context is larger than its head would do this)"
          );
        }
      }
      instanceResolutionSizes.push_back(n);
      if (this->grew) {
        ++instanceResolutionGrowthSteps;
      }
      ++resolutionsInProgress;
    }
    ~InstanceResolutionStep() {
      --resolutionsInProgress;
      instanceResolutionSizes.pop_back();
      if (this->grew) {
        --instanceResolutionGrowthSteps;
      }
    }
    InstanceResolutionStep(const InstanceResolutionStep&) = delete;
    InstanceResolutionStep& operator=(const InstanceResolutionStep&) = delete;
  private:
    bool grew;
  };

  // TClass::matches, satisfiable and explain memoize a type as "assumed
  // satisfiable" before recursing on it and settle the entry when they
  // return. If the depth error above unwinds through them instead, the
  // provisional entry must not be left behind: a later request for the same
  // type would take it as an answer, skip resolution, and (having been told
  // its context holds) generate an instance whose own context is the next
  // type up, and so on without bound -- the third request for the same
  // constraint hung where the first two had been rejected in milliseconds.
  struct ProvisionalMemo {
    ProvisionalMemo(type_map<bool>& memo, const MonoTypes& mts) : memo(memo), mts(mts), unwinding(std::uncaught_exceptions()) {
      memo.insert(mts, true);
      ++resolutionsInProgress;
    }
    ~ProvisionalMemo() {
      --resolutionsInProgress;
      if (std::uncaught_exceptions() > this->unwinding) {
        this->memo.insert(this->mts, false);
      }
    }
    ProvisionalMemo(const ProvisionalMemo&) = delete;
    ProvisionalMemo& operator=(const ProvisionalMemo&) = delete;
  private:
    type_map<bool>& memo;
    MonoTypes       mts;
    int             unwinding;
  };
}

inline bool isHiddenTCName(const std::string& n) {
  return n.empty() || n[0] == '.';
}

FunDeps mergeFundeps(const FunDeps& lhs, const FunDeps& rhs) {
  FunDeps r = lhs;
  for (const auto& fd : rhs) {
    if (!in(fd, r)) {
      r.push_back(fd);
    }
  }
  return r;
}

void includeFundep(const Constraint& c, const FunDep& fd, FunDeps* out) {
  TGenVarSet input;
  for (auto i : fd.first) {
    input = setUnion(input, tgenVars(c.arguments()[i]));
  }

  for (auto o : tgenVars(c.arguments()[fd.second])) {
    if (!in(o, input)) {
      FunDep fd(VarIDs(input.begin(), input.end()), o);
      if (!in(fd, *out)) {
        out->push_back(fd);
      }
    }
  }
}

FunDeps inferFundeps(const TEnvPtr& tenv, const Constraints& cs) {
  FunDeps result;
  for (const auto& c : cs) {
    UnqualifierPtr uq = tenv->lookupUnqualifier(c);

    FunDeps cdeps = uq->dependencies(c);
    for (const auto& cdep : cdeps) {
      includeFundep(*c, cdep, &result);
    }
  }
  return result;
}

// type class definitions
TClass::TClass(const Constraints& reqs, const std::string& tcname, size_t tvs, const Members& tcmembers, const FunDeps& fundeps, const LexicalAnnotation& la) :
  LexicallyAnnotated(la), tcname(tcname), tvs(tvs), reqs(reqs), tcmembers(tcmembers), fundeps(fundeps)
{
}

TClass::TClass(const Constraints& reqs, const std::string& tcname, size_t tvs, const Members& tcmembers, const LexicalAnnotation& la) : TClass(reqs, tcname, tvs, tcmembers, FunDeps(), la) {
}

TClass::TClass(const std::string& tcname, size_t tvs, const Members& tcmembers, const FunDeps& fundeps, const LexicalAnnotation& la) : TClass(Constraints(), tcname, tvs, tcmembers, fundeps, la) {
}

TClass::TClass(const std::string& tcname, size_t tvs, const Members& tcmembers, const LexicalAnnotation& la) : TClass(Constraints(), tcname, tvs, tcmembers, FunDeps(), la) {
}

const std::string& TClass::name() const {
  return this->tcname;
}

const Constraints& TClass::constraints() const {
  return this->reqs;
}

size_t TClass::typeVars() const {
  return this->tvs;
}

const TClass::Members& TClass::members() const {
  return this->tcmembers;
}

const FunDeps& TClass::deps() const {
  return this->fundeps;
}

void TClass::insert(const TEnvPtr& tenv, const TCInstancePtr& ip, Definitions* ds) {
  if (ip->arity() != this->tvs) {
    std::ostringstream ss;
    ss << "Arity mismatch between instance definition (" << ip->arity() << ") and type class definition (" << this->tvs << ").";
    throw annotated_error(*ip, ss.str());
  } else {
    this->tcinstances.push_back(ip);
    this->tcinstdb.insert(ip->types(), ip);
    forgetResolutions();
    ip->bind(tenv, this, ds);
  }
}

void TClass::insert(const TCInstanceFnPtr& ifp) {
  if (ifp->arity() != this->tvs) {
    std::ostringstream ss;
    ss << "Arity mismatch between instance generator definition (" << ifp->arity() << ") and type class definition (" << this->tvs << ").";
    throw annotated_error(*ifp, ss.str());
  } else {
    // make sure that this instance generator covers all required members
    auto mm = ifp->members(MonoTypeSubst());

    std::ostringstream ss;
    size_t errors = 0;
    for (const auto& tcm : this->tcmembers) {
      if (mm.count(tcm.first) == 0u) {
        ss << (errors != 0u?", ":"") << "expected definition of '" << tcm.first << "'";
        ++errors;
      }
    }
    for (const auto& m : mm) {
      if (this->tcmembers.count(m.first) == 0u) {
        ss << (errors != 0u?", ":"") << "unexpected definition of '" << m.first << "'";
        ++errors;
      }
    }
    if (errors != 0u) {
      std::ostringstream ess;
      ess << "Can't introduce instance generator for '" << this->tcname << "': " << ss.str();
      throw annotated_error(*ifp, ess.str());
    }

    // insert the instance generator
    ifp->order = this->tcinstancefns.size();
    this->tcinstancefns.push_back(ifp);

    if (TCInstanceFns* hfns = this->tcinstfndb.lookup(ifp->itys)) {
      hfns->push_back(ifp);
    } else {
      TCInstanceFns x;
      x.push_back(ifp);
      this->tcinstfndb.insert(ifp->itys, x);
    }
    forgetResolutions();
  }
}

// the memos record what resolution found, and adding an instance can change
// it: a constraint refused as unsatisfiable is satisfiable once the instance
// for it is defined (the REPL pattern -- ask, define the instance, ask again),
// and one resolved to a single instance could resolve to two. The instance
// need not belong to the class asked, either: (B a) => A a refuses A int until
// B int exists. So an addition to any class voids every class's memos, through
// a generation count that each class compares with its own before it reads
// them; what is still true is rederived on the next request.
//
// Not in the middle of a request, though: the memos are also the recursion
// guard, holding "assumed satisfiable" entries for the constraints being
// resolved, and a resolution in progress adds instances of its own (the ones
// it generates). Clearing then would drop the guard from under it. So the
// count is read once, when a request starts with no resolution in progress,
// and every class touched by that request is brought up to the count read
// then; an addition made during the request takes effect at the next one,
// which is the first that could ask about it anyway.
//
// The count is process-wide rather than per type environment: an instance
// added in one compiler clears the memos of another's classes, which costs a
// rederivation of what was memoized and nothing else, and is not a change
// from before for the common case of one compiler per process.
namespace {
  std::atomic<uint64_t> instanceGeneration{1};
  thread_local uint64_t requestGeneration = 0;
}

void TClass::forgetResolutions() {
  ++instanceGeneration;
}

void TClass::refreshResolutions() const {
  if (resolutionsInProgress == 0) {
    requestGeneration = instanceGeneration.load();
  }
  if (this->memoGeneration != requestGeneration) {
    this->testedInstances.clear();
    this->satfInstances.clear();
    this->memoGeneration = requestGeneration;
  }
}

TCInstances TClass::matches(const TEnvPtr& tenv, const ConstraintPtr& c, MonoTypeUnifier* u, Definitions* ds) const {
  return matches(tenv, c->arguments(), u, ds);
}

void TClass::candidateTCInstFns(const TEnvPtr& tenv, const MonoTypes& mts, TCInstanceFns* x) const {
  std::vector<TCInstanceFns> fss;
  this->tcinstfndb.bidimatches(tenv, mts, &fss);

  for (const auto& fs : fss) {
    x->insert(x->end(), fs.begin(), fs.end());
  }

  std::sort(x->begin(), x->end(),
            [] (const TCInstanceFnPtr& a, const TCInstanceFnPtr& b) { return a->order < b->order; });
}

TCInstances TClass::matches(const TEnvPtr& tenv, const MonoTypes& mts, MonoTypeUnifier* u, Definitions* ds) const {
  refreshResolutions();

  // do any ground instances match?
  TCInstances r;
  this->tcinstdb.matches(tenv, mts, &r);

  // if no ground instances match, can we generate a ground instance to match?
  //  (this can only work when we can feed back derived type information)
  if (r.empty()) {
    ProvisionalMemo assumed(this->testedInstances, mts);

    TCInstanceFns ifns;
    candidateTCInstFns(tenv, mts, &ifns);
    for (const auto& f : ifns) {
      // for recursive instance definitions, initially assume that this instantiation is satisfiable
      // (this will prevent nested instance requests from recursing infinitely)
      //
      // a generator refused for growth (InstanceResolutionStep) unwinds through
      // here without trying the generators after it, and the provisional memo
      // settles this type as unsatisfiable, though a later generator might
      // have resolved it. That is the refusal's cost: it is an error in the
      // instance that diverged, reported as such, and defining another
      // instance clears the memo
      TCInstancePtr ninst;
      if (f->apply(tenv, mts, this, u, ds, &ninst)) {
        const_cast<TClass*>(this)->insert(tenv, ninst, ds);
        r.push_back(ninst);
        break;
      } else if (ninst.get() != nullptr) {
        r.push_back(ninst);
        break;
      }
    }

    this->testedInstances.insert(mts, false);
  }

  return r;
}

bool TClass::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* s, Definitions* ds) {
  // if the arity is wrong on this constraint, it can't ever work
  if (cst->arguments().size() != this->tvs) {
    return false;
  }

  // start off assuming we'll add no information
  bool r = false;

  // apply refinement across all constraints implied by this class
  for (const auto& req : this->reqs) {
    r |= hobbes::refine(tenv, instantiate(cst->arguments(), req), s, ds);
  }

  // apply refinement across all fundeps
  for (const auto &fundep : this->fundeps) {
    r |= refine(tenv, cst, fundep, s, ds);
  }
  return r;
}

bool TClass::refine(const TEnvPtr& tenv, const ConstraintPtr& c, const FunDep& fd, MonoTypeUnifier* s, Definitions* ds) const {
  size_t    bsz  = s->size();
  MonoTypes args = s->substitute(c->arguments());

  if (!hasFreeVariables(select(args, fd.first))) {
    MonoTypePtr rty = select(args, fd.second);

    if (hasFreeVariables(rty)) {
      TCInstances ms = matches(tenv, args, s, ds);
      if (ms.size() == 1) {
        mgu(rty, select(ms[0]->types(), fd.second), s);
      }
    }
  }
  return bsz != s->size();
}

bool isLiteralFnTerm(const ExprPtr& e) {
  return is<Fn>(stripAssumpHead(e)) != nullptr;
}

bool TClass::satisfied(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) const {
  refreshResolutions();
  if (c->arguments().size() != this->tvs) {
    return false;
  } else if (c->hasFreeVariables()) {
    return false;
  } else if (bool* f = this->testedInstances.lookup(c->arguments())) {
    if (*f) return true;
  }

  // consider whether all implied constraints are satisfied
  for (const auto& req : this->reqs) {
    if (!::hobbes::satisfied(tenv, instantiate(c->arguments(), req), ds)) {
      return false;
    }
  }

  // finally see if we can get an instance
  return matches(tenv, c->arguments(), nullptr, ds).size() == 1;
}

bool TClass::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) const {
  // take care of the obvious
  if (c->arguments().size() != this->tvs) return false;

  // consider satisfiability across all constraints implied by this class
  for (const auto& req : this->reqs) {
    if (!::hobbes::satisfiable(tenv, instantiate(c->arguments(), req), ds)) {
      return false;
    }
  }

  // for empty class definitions, assume allow constraint usage before definitions
  if (this->tcinstdb.values().empty() && this->tcinstancefns.empty()) {
    return true;
  }

  MonoTypes mts = c->arguments();

  // did we already assume that this constraint was satisfiable?
  refreshResolutions();
  if (bool* f = this->satfInstances.lookup(mts)) {
    return *f;
  }
  
  // assume we're satisfiable until we can prove we're not
  ProvisionalMemo assumed(this->satfInstances, mts);

  // we're satisfiable if there's at least one satisfiable instance for this constraint
  if (this->tcinstdb.hasMatch(tenv, mts)) return true;

  // or an instance generator
  for (const auto& f : this->tcinstancefns) {
    if (f->satisfiable(tenv, mts, ds)) {
      return true;
    }
  }

  // we couldn't find a single way that this constraint was satisfiable, it's not satisfiable
  this->satfInstances.insert(mts, false);
  return false;
}

void TClass::explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs) {
  if (isHiddenTCName(this->tcname)) {
    Constraints fcs;
    for (const auto& c : expandHiddenTCs(tenv, list(cst))) {
      if (!hobbes::satisfiable(tenv, c, ds)) {
        fcs.push_back(c);
      }
    }
    
    if (fcs.size() == 1) {
      msgs->push_back(annmsg("constraint not satisfiable: " + hobbes::show(fcs[0]), e->la()));
    } else if (fcs.size() > 1) {
      std::ostringstream ss;
      ss << "constraints not satisfiable:";
      for (const auto& c : fcs) {
        ss << "\n  " << hobbes::show(c);
      }
      msgs->push_back(annmsg(ss.str(), e->la()));
    }
  } else {
    const MonoTypes& mts = cst->arguments();

    // avoid infinitely-recursive explanations
    refreshResolutions();
    if (bool* f = this->testedInstances.lookup(mts)) {
      if (*f) {
        return;
      }
    }

    // why couldn't we generate a ground instance?
    size_t          maxSuccCount = 0;
    TCInstanceFnPtr likelyTarget;
    Constraints     fcs;

    ProvisionalMemo assumed(this->testedInstances, mts);
    TCInstanceFns ifns;
    candidateTCInstFns(tenv, mts, &ifns);
    for (const auto& f : ifns) {
      Constraints scs;
      fcs.clear();
      f->explainSatisfiability(tenv, mts, ds, &scs, &fcs);
      if (scs.size() > maxSuccCount && !fcs.empty()) {
        maxSuccCount = scs.size();
        likelyTarget = f;
      }
    }

    // for now, report on just the most likely cause of failure
    if (likelyTarget) {
      msgs->push_back(annmsg("Constraint not satisfiable: " + hobbes::show(cst), e->la()));

      std::ostringstream ss;
      ss << "most likely instance fails at:";
      for (const auto& c : fcs) {
        ss << "\n  " << hobbes::show(c);
      }
      msgs->push_back(annmsg(ss.str(), likelyTarget->la()));
    }

    this->testedInstances.insert(mts, false);
  }
}

ExprPtr TClass::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  // we can unqualify iff there's one (ONE!) matching instance for this constraint
  TCInstances tis = matches(tenv, cst, nullptr, ds);
  if (tis.size() != 1) {
    throw annotated_error(*e, "Cannot unqualify ambiguous or unsatisfiable predicate: " + hobbes::show(cst));
  } else {
    return tis[0]->unqualify(ds, tenv, cst, e);
  }
}

TCInstancePtr TClass::uniqueInstance(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  TCInstances tis = matches(tenv, cst, nullptr, ds);
  if (tis.size() == 1) {
    return tis[0];
  }
  return TCInstancePtr();
}

PolyTypePtr TClass::lookup(const std::string& vn) const {
  auto m = this->tcmembers.find(vn);
  if (m != this->tcmembers.end()) {
    return std::make_shared<PolyType>(this->typeVars(), qualtype(list(std::make_shared<Constraint>(this->name(), tgens(this->typeVars()))), m->second));
  } else {
    return PolyTypePtr();
  }
}

MonoTypePtr TClass::memberType(const std::string& vn) const {
  auto m = this->tcmembers.find(vn);
  if (m != this->tcmembers.end()) {
    return m->second;
  } else {
    throw std::runtime_error("Undefined class member, '" + vn + "'");
  }
}

SymSet TClass::bindings() const {
  SymSet r;
  for (const auto &tcmember : this->tcmembers) {
    r.insert(tcmember.first);
  }
  return r;
}

FunDeps TClass::dependencies(const ConstraintPtr&) const {
  return this->fundeps;
}

void showFundep(const FunDep& fd, std::ostream& out) {
  for (int x : fd.first) {
    out << "#" << x << " ";
  }
  out << "-> #" << fd.second;
}

void TClass::show(std::ostream& out) const {
  out << "class ";
  if (!this->reqs.empty()) {
    out << "(";
    this->reqs[0]->show(out);
    for (size_t i = 1; i < this->reqs.size(); ++i) {
      out << ", ";
      this->reqs[i]->show(out);
    }
    out << ") => ";
  }
  out << this->tcname;
  if (!this->fundeps.empty()) {
    out << " | ";
    showFundep(this->fundeps[0], out);
    for (size_t i = 1; i < this->fundeps.size(); ++i) {
      out << ", ";
      showFundep(this->fundeps[i], out);
    }
  }
  out << " where\n";
  for (const auto &tcmember : this->tcmembers) {
    out << "  " << tcmember.first << " :: " << hobbes::show(tcmember.second) << "\n";
  }
}

const TCInstances& TClass::instances() const {
  return this->tcinstances;
}

bool TClass::hasGroundInstanceAt(const MonoTypes& mts) const {
  return this->tcinstdb.lookup(mts) != nullptr;
}

const TCInstanceFns& TClass::instanceFns() const {
  return this->tcinstancefns;
}

/////////////
// class instances define a particular overloading scheme for a fixed monotype sequence
/////////////
TCInstance::TCInstance(const std::string& tcname, const MonoTypes& itys, const MemberMapping& mmap, const LexicalAnnotation& la) :
  LexicallyAnnotated(la), tcname(tcname), itys(itys), mmap(mmap) {
}

size_t TCInstance::arity() const {
  return this->itys.size();
}

const MonoTypes& TCInstance::types() const {
  return this->itys;
}

const MemberMapping& TCInstance::memberMapping() const {
  return this->mmap;
}

bool TCInstance::hasMapping(const std::string& oname) const {
  return this->mmap.find(oname) != this->mmap.end();
}

const TCInstance::ExprPtr& TCInstance::memberMapping(const std::string& oname) const {
  auto mm = this->mmap.find(oname);
  if (mm != this->mmap.end()) {
    return mm->second;
  } else {
    throw std::runtime_error("Overloaded symbol not defined in type-class instance: " + oname);
  }
}

bool TCInstance::matches(const TEnvPtr& tenv, const MonoTypes& ctys) const {
  return unifiable(tenv, this->itys, ctys);
}

struct TCUnqualify : public switchExprTyFn {
  const TCInstance*    inst;
  Definitions*         ds;
  const TEnvPtr&       tenv;
  const ConstraintPtr& constraint;
  TCUnqualify(const TCInstance* inst, Definitions* ds, const TEnvPtr& tenv, const ConstraintPtr& constraint) : inst(inst), ds(ds), tenv(tenv), constraint(constraint) { }

  QualTypePtr withTy(const QualTypePtr& qt) const override {
    return removeConstraint(this->constraint, qt);
  }

  ExprPtr with(const Var* v) const override {
    // if we can resolve this symbol as an overload, replace it
    auto mm = inst->memberMapping().find(v->value());

    if (mm != inst->memberMapping().end() && hasConstraint(this->constraint, v->type())) {
      return mm->second;
    } else {
      return wrapWithTy(v->type(), v->clone());
    }
  }
};

// resolve member definitions ahead of time, so that we can just substitute into use-sites
void TCInstance::bind(const TEnvPtr& tenv, const TClass* c, Definitions* ds) {
  for (auto &mm : this->mmap) {
    mm.second = unqualifyTypes(tenv, validateType(tenv, assume(mm.second, instantiate(this->itys, c->memberType(mm.first)), mm.second->la()), ds), ds);
  }
}

ExprPtr TCInstance::unqualify(Definitions* ds, const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e) const {
  return switchOf(e, TCUnqualify(this, ds, tenv, cst));
}

// eliminate a batch of class constraints in a single traversal
// (equivalent to sequentially applying TCUnqualify for each constraint, but
//  avoids one full expression rewrite and teardown per eliminated constraint)
struct TCUnqualifyBatch : public switchExprTyFn {
  // member-name -> [(constraint, resolved member expr)] in batch order
  using MemberSubs = std::unordered_map<std::string, std::vector<std::pair<ConstraintPtr, ExprPtr>>>;
  Constraints cs;
  MemberSubs  subs;

  explicit TCUnqualifyBatch(const TCInstConstraints& cis) {
    for (const auto& ci : cis) {
      this->cs.push_back(ci.first);
      for (const auto& mm : ci.second->memberMapping()) {
        this->subs[mm.first].push_back(std::make_pair(ci.first, mm.second));
      }
    }
  }

  QualTypePtr withTy(const QualTypePtr& qt) const override {
    Constraints r;
    for (const auto& c : qt->constraints()) {
      if (!hasConstraint(c, this->cs)) {
        r.push_back(c);
      }
    }
    if (r.size() == qt->constraints().size()) {
      return qt;
    }
    return qualtype(r, qt->monoType());
  }

  ExprPtr with(const Var* v) const override {
    // if we can resolve this symbol as an overload of any batched constraint, replace it
    auto s = this->subs.find(v->value());
    if (s != this->subs.end()) {
      for (const auto& ce : s->second) {
        if (hasConstraint(ce.first, v->type())) {
          return ce.second;
        }
      }
    }
    return wrapWithTy(v->type(), v->clone());
  }
};

ExprPtr unqualifyClassConstraints(const TEnvPtr&, const TCInstConstraints& cis, const ExprPtr& e, Definitions*) {
  return switchOf(e, TCUnqualifyBatch(cis));
}

void TCInstance::show(std::ostream& out) const {
  out << "instance " << this->tcname << " " << str::cdelim(hobbes::show(this->itys), " ") << " where\n";
  for (const auto &mm : this->mmap) {
    out << "  " << mm.first << " = " << hobbes::show(mm.second) << "\n";
  }
}

////////
// generate class instances from class instance functions
////////
TCInstanceFn::TCInstanceFn(const std::string& tcname, const Constraints& reqs, const MonoTypes& itys, const MemberMapping& mmap, const LexicalAnnotation& la) :
  LexicallyAnnotated(la), tcname(tcname), reqs(reqs), mmap(mmap), itys(itys)
{
}

size_t TCInstanceFn::arity() const {
  return this->itys.size();
}

bool TCInstanceFn::satisfiable(const TEnvPtr& tenv, const MonoTypes& tys, Definitions* rdefs) const {
  // immediately reject arity mismatch (though this should never happen)
  if (this->itys.size() != tys.size()) {
    return false;
  }

  InstanceResolutionStep step(this->tcname, tys);

  // can the input unify with this generator's head?  can it satisfy its constraints?
  MonoTypeSubst s;
  IFnDef        fdef  = freshDef(&s);
  Constraints&  acsts = fdef.first;
  MonoTypes&    argl  = fdef.second;

  // determine definition types by unifying argument and generator-definition types
  MonoTypeUnifier u(tenv);

  try {
    mgu(argl, tys, &u);
  } catch (std::exception& ex) {
    return false;
  }

  // require that all assumed constraints are satisfiable
  // accumulate (non-recursive) constraints to ensure recursive unqualification
  try {
    refine(tenv, acsts, &u, rdefs);

    for (const auto& req : acsts) {
      if (!hobbes::satisfiable(tenv, simplifyVarNames(req), rdefs)) {
        return false;
      }
    }
  } catch (instance_resolution_depth_error&) {
    throw;
  } catch (std::exception& ex) {
    return false;
  }

  // if we got here, it's satisfiable (if not yet satisfied)
  return true;
}

void TCInstanceFn::explainSatisfiability(const TEnvPtr& tenv, const MonoTypes& tys, Definitions* rdefs, Constraints* scs, Constraints* fcs) const {
  if (this->itys.size() != tys.size()) {
    return;
  }

  // can the input unify with this generator's head?
  MonoTypeSubst s;
  IFnDef        fdef  = freshDef(&s);
  Constraints&  acsts = fdef.first;
  MonoTypes&    argl  = fdef.second;

  // determine definition types by unifying argument and generator-definition types
  MonoTypeUnifier u(tenv);

  try {
    mgu(argl, tys, &u);
  } catch (std::exception& ex) {
    return;
  }

  // well let's find the constraints that aren't satisfiable here
  try {
    refine(tenv, acsts, &u, rdefs);

    for (const auto& req : acsts) {
      ConstraintPtr c = simplifyVarNames(req);

      if (hobbes::satisfiable(tenv, c, rdefs)) {
        scs->push_back(c);
      } else {
        fcs->push_back(c);
      }
    }
  } catch (instance_resolution_depth_error&) {
    throw;
  } catch (std::exception& ex) {
  }
}

// an instance function can be applied to a sequence of types to produce a new instance if:
//   * argument types unify with definition types
//   * all type constraints are satisfied
//   * the derived instance would be defined at a mono-type sequence
//
// if these conditions are met, 'apply' will return the generated instance (and fill an input sequence with residual required definitions)
// if these conditions aren't met, 'apply' will return a null pointer
bool TCInstanceFn::apply(const TEnvPtr& tenv, const MonoTypes& tys, const TClass* pc, MonoTypeUnifier* callsubst, Definitions* rdefs, TCInstancePtr* out) const {
  // immediately reject arity mismatch (though this should never happen)
  if (this->itys.size() != tys.size()) {
    return false;
  }

  InstanceResolutionStep step(this->tcname, tys);

  // generate a fresh copy of this generator's type variables consistent between constraints and definition
  MonoTypeSubst s;
  IFnDef        fdef  = freshDef(&s);
  Constraints&  acsts = fdef.first;
  MonoTypes&    argl  = fdef.second;

  // determine definition types by unifying argument and generator-definition types
  MonoTypeUnifier u(tenv);

  try {
    mgu(argl, tys, &u);
  } catch (std::exception& ex) {
    return false;
  }

  // require that all assumed constraints are satisfied
  // accumulate (non-recursive) constraints to ensure recursive unqualification
  try {
    refine(tenv, acsts, &u, rdefs);

    for (const auto& req : acsts) {
      if (req->hasFreeVariables() || !hobbes::satisfied(tenv, req, rdefs)) {
        return false;
      }
    }
  } catch (instance_resolution_depth_error&) {
    throw;
  } catch (std::exception& ex) {
    return false;
  }

  // require that the final type sequence is mono
  MonoTypes nitys = u.substitute(argl);
  if (hasFreeVariables(nitys)) {
    return false;
  }

  // we've definitely found a complete match
  // merge local bindings to the nested unifier scope
  MonoTypeSubst ms = u.substitution();
  if (!ms.empty() && (callsubst != nullptr)) {
    for (const auto& m : ms) {
      callsubst->bind(m.first, m.second);
    }
  }

  // if we've actually already generated this instance, just return it
  TCInstances r;
  pc->tcinstdb.matches(tenv, nitys, &r);
  if (r.size() == 1) {
    *out = r[0];
    return false;
  }

  // and produce the output type class instance (generating residual definitions as necessary)
  MemberMapping mm;
  for (const auto& sm : this->mmap) {
    QualTypePtr smty = qualtype(acsts, instantiate(nitys, pc->memberType(sm.first)));

    if (isLiteralFnTerm(sm.second)) {
      std::string rfname = ".rfn" + freshName();
      ExprPtr     thisFn = ExprPtr(new Var(rfname, sm.second->la()));

      // forward-declare this generated function
      tenv->root()->bind(rfname, polytype(smty));

      ExprPtr iexp = substitute(&u, substitute(s, sm.second));
      rdefs->push_back(Definition(rfname, substitute(MonoTypeSubst(), assume(iexp, smty, iexp->la())))); // make a fresh term/typing for this generated definition

      mm[sm.first] = thisFn;
    } else {
      mm[sm.first] = assume(substitute(&u, substitute(s, sm.second)), smty, sm.second->la());
    }
  }

  // that's it, we've got a new ground type class instance
  *out = std::make_shared<TCInstance>(this->tcname, nitys, mm, la());
  return true;
}

TCInstanceFn::IFnDef TCInstanceFn::freshDef(MonoTypeSubst* s) const {
  IFnDef result;
  Constraints& acsts = result.first;
  MonoTypes&   argl  = result.second;

  NameSet arglNames;
  tvarNames(this->itys, &arglNames);
  tvarNames(this->reqs, &arglNames);
  
  for (const auto& tn : arglNames) {
    (*s)[tn] = freshTypeVar();
  }
  argl = substitute(*s, this->itys);

  for (const auto& c : this->reqs) {
    ConstraintPtr acst = c->substitute(*s);
    acsts.push_back(acst);
  }

  return result;
}

MonoTypes TCInstanceFn::instantiatedArgs(MonoTypeUnifier* s, const MonoTypes& tys) const {
  // make fresh instance types to destructively update
  MonoTypes fitys = substitute(MonoTypeSubst(), this->itys);
  mgu(fitys, tys, s);
  return s->substitute(fitys);
}

MemberMapping TCInstanceFn::members(const MonoTypeSubst& s) const {
  MemberMapping result;
  for (const auto &mm : this->mmap) {
    result[mm.first] = substitute(s, mm.second);
  }
  return result;
}

const Constraints& TCInstanceFn::constraints() const {
  return this->reqs;
}

void TCInstanceFn::show(std::ostream& out) const {
  NameSet rnames = tvarNames(this->reqs);
  NameSet x = tvarNames(this->itys);
  rnames.insert(x.begin(), x.end());
  for (const auto& mm : this->mmap) {
    NameSet x = tvarNames(mm.second);
    rnames.insert(x.begin(), x.end());
  }
  MonoTypeSubst simpl = canonicalNameSubst(rnames);

  out << "instance (" << str::cdelim(hobbes::showNoSimpl(substitute(simpl, this->reqs)), ", ") << ") => "
      << this->tcname << " " << str::cdelim(hobbes::showNoSimpl(substitute(simpl, this->itys)), " ") << " where\n";

  for (const auto& mm : this->mmap) {
    out << "  " << mm.first << " = " << hobbes::show(substitute(simpl, mm.second)) << "\n";
  }
}

// generate a fresh type class and instance generator to control poly/qualtype instantiation in an expression
void definePrivateClass(const TEnvPtr& tenv, const std::string& memberName, const ExprPtr& expr) {
  PolyTypePtr xety   = generalize(expr->type());
  std::string tcname = ".genc" + freshName();

  NameSet   tvns   = tvarNames(expr->type());
  MonoTypes gtvars = typeVars(toVector(tvns));

  if (xety->typeVariables() != gtvars.size()) {
    throw annotated_error(
      *expr,
      "Internal error while defining private class: " +
      str::from(xety->typeVariables()) + " type variables generalized from expression, but " +
      str::from(gtvars.size()) + " inferred from:\n  " + showAnnotated(expr) +
      "\nwith type vars:\n  {" + str::cdelim(show(gtvars), ", ") + "}"
    );
  }

  TClass::Members ms;
  ms[memberName] = xety->qualtype()->monoType();
  TClassPtr nclass(new TClass(xety->qualtype()->constraints(), tcname, xety->typeVariables(), ms, inferFundeps(tenv, xety->qualtype()->constraints()), expr->la()));

  MemberMapping mm;
  mm[memberName] = expr;

  nclass->insert(std::make_shared<TCInstanceFn>(tcname, Constraints(), gtvars, mm, expr->la()));

  tenv->bind(tcname, nclass);
}

// reverse "hidden" type classes to get the original set of constraints
Constraints expandHiddenTCs(const TEnvPtr& tenv, const Constraints& cs) {
  Constraints r;
  for (const auto& c : cs) {
    if (!isHiddenTCName(c->name())) {
      r.push_back(c);
    } else {
      auto uq = tenv->lookupUnqualifier(c->name());
      if (const auto* cc = dynamic_cast<const TClass*>(uq.get())) {
        Constraints ncs = expandHiddenTCs(tenv, instantiate(c->arguments(), cc->constraints()));
        r.insert(r.end(), ncs.begin(), ncs.end());
      } else {
        // ??
        r.push_back(c);
      }
    }
  }
  return r;
}

const TClass* findClass(const TEnvPtr& tenv, const std::string& cname) {
  UnqualifierPtr uq = tenv->lookupUnqualifier(cname);
  if (uq.get() == nullptr) {
    throw std::runtime_error("No such type class: " + cname);
  }
  const auto* c = dynamic_cast<const TClass*>(uq.get());
  if (c == nullptr) {
    throw std::runtime_error("Not a type class: " + cname);
  }
  return c;
}

bool isClassSatisfied(const TEnvPtr& tenv, const std::string& cname, const MonoTypes& tys, Definitions* ds) {
  return findClass(tenv, cname)->satisfied(tenv, std::make_shared<Constraint>(cname, tys), ds);
}

bool isClassSatisfiable(const TEnvPtr& tenv, const std::string& cname, const MonoTypes& tys, Definitions* ds) {
  return findClass(tenv, cname)->satisfiable(tenv, std::make_shared<Constraint>(cname, tys), ds);
}

ExprPtr unqualifyClass(const TEnvPtr& tenv, const std::string& cname, const MonoTypes& tys, const ExprPtr& e, Definitions* ds) {
  const TClass* c = findClass(tenv, cname);
  ConstraintPtr cst(new Constraint(cname, tys));

  if (!c->satisfied(tenv, cst, ds)) {
    throw annotated_error(*e, "Cannot unqualify unsatisfied constraint: " + show(cst));
  } else {
    return c->unqualify(tenv, cst, e, ds);
  }
}

bool isClassMember(const TEnvPtr& tenv, const std::string& memberName) {
  // a name that isn't bound at all can't be a class member, and asking for its
  // type would build a "did you mean" suggestion list from every binding in
  // the environment before throwing
  if (!tenv->hasBinding(memberName)) {
    return false;
  }
  try {
    Constraints cs = tenv->lookup(memberName)->qualtype()->constraints();
    return (cs.size() == 1) && (tenv->lookupUnqualifier(cs[0])->lookup(memberName) != PolyTypePtr());
  } catch (std::exception&) {
    return false;
  }
}

// show class, instance, instance-generator definitions
std::string show(const TClassPtr& x) {
  std::ostringstream ss;
  x->show(ss);
  return ss.str();
}

std::string show(const TCInstancePtr& x) {
  std::ostringstream ss;
  x->show(ss);
  return ss.str();
}

std::string show(const TCInstanceFnPtr& x) {
  std::ostringstream ss;
  x->show(ss);
  return ss.str();
}

// expedient serialization of ground type class instances ..
void serializeGroundInstance(const TEnvPtr&, const TClass*, const TCInstancePtr& inst, std::ostream& out) {
  encode(inst->types(), out);
  encode(inst->memberMapping(), out);
}

void serializeGroundInstances(const TEnvPtr& tenv, const TClass* c, const TCInstances& insts, std::ostream& out) {
  encode(insts.size(), out);
  for (const auto &inst : insts) {
    serializeGroundInstance(tenv, c, inst, out);
  }
}

using Classes = std::vector<const TClass *>;

void serializeGroundClasses(const TEnvPtr& tenv, const Classes& cs, std::ostream& out) {
  encode(cs.size(), out);
  for (const auto *c : cs) {
    encode(c->name(), out);
    serializeGroundInstances(tenv, c, c->instances(), out);
  }
}

void serializeGroundClasses(const TEnvPtr& tenv, std::ostream& out) {
  const TEnv::Unqualifiers& uqs = tenv->unqualifiers();
  Classes cs;

  for (const auto &uq : uqs) {
    if (const auto* c = dynamic_cast<const TClass*>(uq.second.get())) {
      if (!c->instances().empty()) {
        cs.push_back(c);
      }
    }
  }

  serializeGroundClasses(tenv, cs, out);
}

// expedient deserialization of ground type class instances ...

void deserializeGroundClasses(const TEnvPtr& tenv, std::istream& in, Definitions* ds) {
  size_t cc = 0;
  decode(&cc, in);

  for (size_t i = 0; i < cc; ++i) {
    std::string cname;
    decode(&cname, in);

    TClass* c = nullptr;
    try {
      c = dynamic_cast<TClass*>(tenv->lookupUnqualifier(cname).get());
    } catch (std::exception&) {
      c = nullptr;
    }

    size_t ic = 0;
    decode(&ic, in);

    for (size_t j = 0; j < ic; ++j) {
      MonoTypes mts;
      decode(&mts, in);

      MemberMapping mm;
      decode(&mm, in);

      if (c != nullptr) {
        if (!c->hasGroundInstanceAt(mts)) {
          c->insert(tenv, std::make_shared<TCInstance>(cname, mts, mm, c->la()), ds);
        }
      }
    }
  }
}

}

