
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/tyunqualify.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/lang/typepreds.H>
#include <hobbes/util/array.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/perf.H>

namespace hobbes {

inline bool isHiddenTCName(const std::string& n) {
  return n.size() == 0 || n[0] == '.';
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
    for (auto cdep : cdeps) {
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
    ip->bind(tenv, this, ds);
  }
}

void TClass::insert(const TCInstanceFnPtr& ifp) {
  if (ifp->arity() != this->tvs) {
    std::ostringstream ss;
    ss << "Arity mismatch between instance generator definition (" << ifp->arity() << ") and type class definition (" << this->tvs << ").";
    throw annotated_error(*ifp, ss.str());
  } else {
    ifp->order = this->tcinstancefns.size();
    this->tcinstancefns.push_back(ifp);

    if (TCInstanceFns* hfns = this->tcinstfndb.lookup(ifp->itys)) {
      hfns->push_back(ifp);
    } else {
      TCInstanceFns x;
      x.push_back(ifp);
      this->tcinstfndb.insert(ifp->itys, x);
    }
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
  // do any ground instances match?
  TCInstances r;
  this->tcinstdb.matches(tenv, mts, &r);

  // if no ground instances match, can we generate a ground instance to match?
  //  (this can only work when we can feed back derived type information)
  if (r.size() == 0) {
    this->testedInstances.insert(mts, true);

    TCInstanceFns ifns;
    candidateTCInstFns(tenv, mts, &ifns);
    for (const auto& f : ifns) {
      // for recursive instance definitions, initially assume that this instantiation is satisfiable
      // (this will prevent nested instance requests from recursing infinitely)
      TCInstancePtr ninst;
      if (f->apply(tenv, mts, this, u, ds, &ninst)) {
        const_cast<TClass*>(this)->insert(tenv, ninst, ds);
        r.push_back(ninst);
        break;
      } else if (ninst.get()) {
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
  for (FunDeps::const_iterator fd = this->fundeps.begin(); fd != this->fundeps.end(); ++fd) {
    r |= refine(tenv, cst, *fd, s, ds);
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
  return is<Fn>(stripAssumpHead(e));
}

bool TClass::satisfied(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) const {
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
  return matches(tenv, c->arguments(), 0, ds).size() == 1;
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
  if (this->tcinstdb.values().size() == 0 && this->tcinstancefns.size() == 0) {
    return true;
  }

  MonoTypes mts = c->arguments();

  // did we already assume that this constraint was satisfiable?
  if (bool* f = this->satfInstances.lookup(mts)) {
    return *f;
  }
  
  // assume we're satisfiable until we can prove we're not
  this->satfInstances.insert(mts, true);

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
    if (bool* f = this->testedInstances.lookup(mts)) {
      if (*f) {
        return;
      }
    }

    // why couldn't we generate a ground instance?
    size_t          maxSuccCount = 0;
    TCInstanceFnPtr likelyTarget;
    Constraints     fcs;

    this->testedInstances.insert(mts, true);
    TCInstanceFns ifns;
    candidateTCInstFns(tenv, mts, &ifns);
    for (const auto& f : ifns) {
      Constraints scs;
      fcs.clear();
      f->explainSatisfiability(tenv, mts, ds, &scs, &fcs);
      if (scs.size() > maxSuccCount && fcs.size() > 0) {
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
  TCInstances tis = matches(tenv, cst, 0, ds);
  if (tis.size() != 1) {
    throw annotated_error(*e, "Cannot unqualify ambiguous or unsatisfiable predicate: " + hobbes::show(cst));
  } else {
    return tis[0]->unqualify(ds, tenv, cst, e);
  }
}

PolyTypePtr TClass::lookup(const std::string& vn) const {
  Members::const_iterator m = this->tcmembers.find(vn);
  if (m != this->tcmembers.end()) {
    return PolyTypePtr(new PolyType(this->typeVars(), qualtype(list(ConstraintPtr(new Constraint(this->name(), tgens(this->typeVars())))), m->second)));
  } else {
    return PolyTypePtr();
  }
}

MonoTypePtr TClass::memberType(const std::string& vn) const {
  Members::const_iterator m = this->tcmembers.find(vn);
  if (m != this->tcmembers.end()) {
    return m->second;
  } else {
    throw std::runtime_error("Undefined class member, '" + vn + "'");
  }
}

SymSet TClass::bindings() const {
  SymSet r;
  for (Members::const_iterator m = this->tcmembers.begin(); m != this->tcmembers.end(); ++m) {
    r.insert(m->first);
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
  if (this->reqs.size() > 0) {
    out << "(";
    this->reqs[0]->show(out);
    for (size_t i = 1; i < this->reqs.size(); ++i) {
      out << ", ";
      this->reqs[i]->show(out);
    }
    out << ") => ";
  }
  out << this->tcname;
  if (this->fundeps.size() > 0) {
    out << " | ";
    showFundep(this->fundeps[0], out);
    for (size_t i = 1; i < this->fundeps.size(); ++i) {
      out << ", ";
      showFundep(this->fundeps[i], out);
    }
  }
  out << " where\n";
  for (Members::const_iterator m = this->tcmembers.begin(); m != this->tcmembers.end(); ++m) {
    out << "  " << m->first << " :: " << hobbes::show(m->second) << "\n";
  }
}

const TCInstances& TClass::instances() const {
  return this->tcinstances;
}

bool TClass::hasGroundInstanceAt(const MonoTypes& mts) const {
  return this->tcinstdb.lookup(mts) != 0;
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
  MemberMapping::const_iterator mm = this->mmap.find(oname);
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

  QualTypePtr withTy(const QualTypePtr& qt) const {
    return removeConstraint(this->constraint, qt);
  }

  ExprPtr with(const Var* v) const {
    // if we can resolve this symbol as an overload, replace it
    MemberMapping::const_iterator mm = inst->memberMapping().find(v->value());

    if (mm != inst->memberMapping().end() && hasConstraint(this->constraint, v->type())) {
      return mm->second;
    } else {
      return wrapWithTy(v->type(), v->clone());
    }
  }
};

// resolve member definitions ahead of time, so that we can just substitute into use-sites
void TCInstance::bind(const TEnvPtr& tenv, const TClass* c, Definitions* ds) {
  for (MemberMapping::iterator mm = this->mmap.begin(); mm != this->mmap.end(); ++mm) {
    mm->second = unqualifyTypes(tenv, validateType(tenv, assume(mm->second, instantiate(this->itys, c->memberType(mm->first)), mm->second->la()), ds), ds);
  }
}

ExprPtr TCInstance::unqualify(Definitions* ds, const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e) const {
  return switchOf(e, TCUnqualify(this, ds, tenv, cst));
}

void TCInstance::show(std::ostream& out) const {
  out << "instance " << this->tcname << " " << str::cdelim(hobbes::show(this->itys), " ") << " where\n";
  for (MemberMapping::const_iterator mm = this->mmap.begin(); mm != this->mmap.end(); ++mm) {
    out << "  " << mm->first << " = " << hobbes::show(mm->second) << "\n";
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

bool TCInstanceFn::satisfiable(const TEnvPtr& tenv, const MonoTypes& tys, Definitions* rdefs) {
  // immediately reject arity mismatch (though this should never happen)
  if (this->itys.size() != tys.size()) {
    return false;
  }

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
  } catch (std::exception& ex) {
    return false;
  }

  // if we got here, it's satisfiable (if not yet satisfied)
  return true;
}

void TCInstanceFn::explainSatisfiability(const TEnvPtr& tenv, const MonoTypes& tys, Definitions* rdefs, Constraints* scs, Constraints* fcs) {
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
  if (ms.size() > 0 && callsubst) {
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
  *out = TCInstancePtr(new TCInstance(this->tcname, nitys, mm, la()));
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
  for (MemberMapping::const_iterator mm = this->mmap.begin(); mm != this->mmap.end(); ++mm) {
    result[mm->first] = substitute(s, mm->second);
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

  nclass->insert(TCInstanceFnPtr(new TCInstanceFn(tcname, Constraints(), gtvars, mm, expr->la())));

  tenv->bind(tcname, nclass);
}

// reverse "hidden" type classes to get the original set of constraints
Constraints expandHiddenTCs(const TEnvPtr& tenv, const Constraints& cs) {
  Constraints r;
  for (auto c : cs) {
    if (!isHiddenTCName(c->name())) {
      r.push_back(c);
    } else {
      auto uq = tenv->lookupUnqualifier(c->name());
      if (const TClass* cc = dynamic_cast<const TClass*>(uq.get())) {
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
  if (uq.get() == 0) {
    throw std::runtime_error("No such type class: " + cname);
  }
  const TClass* c = dynamic_cast<const TClass*>(uq.get());
  if (!c) {
    throw std::runtime_error("Not a type class: " + cname);
  }
  return c;
}

bool isClassSatisfied(const TEnvPtr& tenv, const std::string& cname, const MonoTypes& tys, Definitions* ds) {
  return findClass(tenv, cname)->satisfied(tenv, ConstraintPtr(new Constraint(cname, tys)), ds);
}

bool isClassSatisfiable(const TEnvPtr& tenv, const std::string& cname, const MonoTypes& tys, Definitions* ds) {
  return findClass(tenv, cname)->satisfiable(tenv, ConstraintPtr(new Constraint(cname, tys)), ds);
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
  for (TCInstances::const_iterator inst = insts.begin(); inst != insts.end(); ++inst) {
    serializeGroundInstance(tenv, c, *inst, out);
  }
}

typedef std::vector<const TClass*> Classes;

void serializeGroundClasses(const TEnvPtr& tenv, const Classes& cs, std::ostream& out) {
  encode(cs.size(), out);
  for (Classes::const_iterator c = cs.begin(); c != cs.end(); ++c) {
    encode((*c)->name(), out);
    serializeGroundInstances(tenv, *c, (*c)->instances(), out);
  }
}

void serializeGroundClasses(const TEnvPtr& tenv, std::ostream& out) {
  const TEnv::Unqualifiers& uqs = tenv->unqualifiers();
  Classes cs;

  for (TEnv::Unqualifiers::const_iterator uq = uqs.begin(); uq != uqs.end(); ++uq) {
    if (const TClass* c = dynamic_cast<const TClass*>(uq->second.get())) {
      if (c->instances().size() > 0) {
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

    TClass* c = 0;
    try {
      c = dynamic_cast<TClass*>(tenv->lookupUnqualifier(cname).get());
    } catch (std::exception&) {
      c = 0;
    }

    size_t ic = 0;
    decode(&ic, in);

    for (size_t j = 0; j < ic; ++j) {
      MonoTypes mts;
      decode(&mts, in);

      MemberMapping mm;
      decode(&mm, in);

      if (c) {
        if (!c->hasGroundInstanceAt(mts)) {
          c->insert(tenv, TCInstancePtr(new TCInstance(cname, mts, mm, c->la())), ds);
        }
      }
    }
  }
}

}

