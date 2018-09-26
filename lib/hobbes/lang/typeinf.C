#include <hobbes/lang/typeinf.H>
#include <hobbes/lang/typepreds.H>
#include <hobbes/lang/constraints.H>
#include <hobbes/util/array.H>
#include <hobbes/util/perf.H>

namespace hobbes {

// define type substitution and unification
UTypeRec LiftUType::apply(const MonoTypePtr& t) {
  UTypeRec r;
  r.ty      = t;
  r.visited = false;
  r.donebc  = 0;
  return r;
}

const UTypeRec& MoreDefinedType::apply(const UTypeRec& lhs, const UTypeRec& rhs) {
  if (const TVar* lv = is<TVar>(lhs.ty)) {
    if (const TVar* rv = is<TVar>(rhs.ty)) {
      return (lv->name() < rv->name()) ? lhs : rhs;
    } else {
      return rhs;
    }
  } else {
    if (is<TVar>(rhs.ty)) {
      return lhs;
    } else {
      return (tvarNames(lhs.ty).size() < tvarNames(rhs.ty).size()) ? lhs : rhs;
    }
  }
}

MonoTypeUnifier::MonoTypeUnifier(const TEnvPtr& tenv) : tenv(tenv), bcount(0) {
}

MonoTypeUnifier::MonoTypeUnifier(const MonoTypeUnifier& u) : tenv(u.tenv), bcount(u.bcount) {
  this->merge(u);
}

MonoTypeUnifier& MonoTypeUnifier::operator=(const MonoTypeUnifier& u) {
  this->tenv   = u.tenv;
  this->bcount = u.bcount;
  this->m      = M();

  this->merge(u);
  return *this;
}

const TEnvPtr& MonoTypeUnifier::typeEnv() const {
  return this->tenv;
}

size_t MonoTypeUnifier::size() const {
  return this->bcount;
}

void MonoTypeUnifier::suppress(const std::string& vn) {
  ++this->suppressVarCounts[vn];
}

void MonoTypeUnifier::suppress(const str::seq& vns) {
  for (str::seq::const_iterator vn = vns.begin(); vn != vns.end(); ++vn) {
    suppress(*vn);
  }
}

void MonoTypeUnifier::unsuppress(const std::string& vn) {
  SuppressVarCounts::iterator vnc = this->suppressVarCounts.find(vn);

  if (vnc != this->suppressVarCounts.end()) {
    if (--vnc->second == 0) {
      this->suppressVarCounts.erase(vnc);
    }
  }
}

void MonoTypeUnifier::unsuppress(const str::seq& vns) {
  for (str::seq::const_iterator vn = vns.begin(); vn != vns.end(); ++vn) {
    unsuppress(*vn);
  }
}

bool MonoTypeUnifier::suppressed(const std::string& vn) const {
  SuppressVarCounts::const_iterator vnc = this->suppressVarCounts.find(vn);
  return vnc != this->suppressVarCounts.end() && vnc->second > 0;
}

bool MonoTypeUnifier::suppressed(const MonoTypePtr& ty) const {
  if (const TVar* tv = is<TVar>(ty)) {
    return suppressed(tv->name());
  } else {
    return false;
  }
}

// record a var-name/type association
void MonoTypeUnifier::bind(const std::string& vn, const MonoTypePtr& t) {
  unify(MonoTypePtr(TVar::make(vn)), t);
}

MonoTypePtr MonoTypeUnifier::binding(const std::string& vn) {
  return this->m.find(MonoTypePtr(TVar::make(vn))).ty;
}

// deconstruct a type into generic constructor form
//   (this might be better done in the type representation itself!)
struct encodeCtorForm : public switchType<std::string> {
  const MonoTypePtr& forTy;
  MonoTypes*         targs;
  str::seq*          ignvs;

  encodeCtorForm(const MonoTypePtr& forTy, MonoTypes* targs, str::seq* ignvs) : forTy(forTy), targs(targs), ignvs(ignvs) { }

  std::string with(const Prim* v) const {
    return "prim:" + v->name();
  }

  std::string with(const OpaquePtr* v) const {
    return "ptr" + std::string(v->storedContiguously() ? "c" : "") + ":" + v->name();
  }

  std::string with(const TVar* v) const {
    return "var:" + v->name();
  }

  std::string with(const TGen* v) const {
    return "tgen:" + str::from(v->id());
  }

  std::string with(const TAbs* v) const {
    return "tabs:" + show(v);
  }

  std::string with(const TApp* v) const {
    this->targs->push_back(v->fn());
    for (const auto& arg : v->args()) {
      this->targs->push_back(arg);
    }
    return "tapp";
  }

  std::string with(const FixedArray* v) const {
    this->targs->push_back(v->type());
    this->targs->push_back(v->length());
    return "farray";
  }

  std::string with(const Array* v) const {
    this->targs->push_back(v->type());
    return "array";
  }

  std::string with(const Variant* v) const {
    std::ostringstream vn;
    vn << "variant:";
    for (Variant::Members::const_iterator c = v->members().begin(); c != v->members().end(); ++c) {
      vn << c->selector << "=" << ";";
      this->targs->push_back(c->type);
    }
    return vn.str();
  }

  std::string with(const Record* v) const {
    std::ostringstream rn;
    rn << "record:";
    for (Record::Members::const_iterator f = v->members().begin(); f != v->members().end(); ++f) {
      rn << f->field << "=" << ";";
      this->targs->push_back(f->type);
    }
    return rn.str();
  }

  std::string with(const Func* v) const {
    this->targs->push_back(v->argument());
    this->targs->push_back(v->result());
    return "->";
  }

  std::string with(const Exists* v) const {
    this->ignvs->push_back(v->absTypeName());
    this->targs->push_back(v->absType());
    return "exists";
  }
  
  std::string with(const Recursive* v) const {
    this->ignvs->push_back(v->recTypeName());
    this->targs->push_back(v->recType());
    return "recur";
  }

  std::string with(const TString* v) const {
    return "string:" + v->value();
  }

  std::string with(const TLong* v) const {
    return "long:" + str::from(v->value());
  }

  std::string with(const TExpr* v) const {
    return "expr:" + show(v);
  }
};

void typeCtorForm(const MonoTypePtr& ty, std::string* cname, MonoTypes* targs, str::seq* ignvs) {
  *cname = switchOf(ty, encodeCtorForm(ty, targs, ignvs));
}

// specify that two types should be equal
void MonoTypeUnifier::unify(const MonoTypePtr& lhs, const MonoTypePtr& rhs) {
  if (suppressed(lhs) || suppressed(rhs)) {
    return;
  }

  const MonoTypePtr& lhsv = this->m.find(lhs).ty;
  const MonoTypePtr& rhsv = this->m.find(rhs).ty;

  if (lhsv != rhsv) {
    // unify the constituents of these types (if applicable)
    if (!is<TVar>(lhsv) && !is<TVar>(rhsv)) {
      str::seq ignvs;

      std::string lhscn;
      MonoTypes lhsts;
      typeCtorForm(lhsv, &lhscn, &lhsts, &ignvs);

      std::string rhscn;
      MonoTypes rhsts;
      typeCtorForm(rhsv, &rhscn, &rhsts, &ignvs);

      if (lhsts.size() != rhsts.size() || lhscn != rhscn) {
        throw std::runtime_error("Cannot unify types: " + show(substitute(lhsv)) + " != " + show(substitute(rhsv)));
      }

      suppress(ignvs);
      try {
        for (size_t i = 0; i < lhsts.size(); ++i) {
          unify(lhsts[i], rhsts[i]);
        }
        unsuppress(ignvs);
      } catch (...) {
        unsuppress(ignvs);
        throw;
      }
    } else {
      // one of the values is a variable -- let's increase our binding count
      ++this->bcount;
    }

    // and the types themselves are the same
    this->m.join(lhsv, rhsv);
  }
}

// resolve the input type against the local mapping
struct substituteInto : public switchTyFn {
  MonoTypeUnifier* s;
  UTypeRec&        uty;
  substituteInto(MonoTypeUnifier* s, UTypeRec& uty) : s(s), uty(uty) { }

  MonoTypePtr with(const TVar* v) const {
    return this->s->binding(v->name());
  }

  MonoTypePtr with(const TApp* v) const {
    this->uty.visited = true;
    MonoTypePtr f = this->s->substitute(v->fn());
    MonoTypes args;
    for (auto& arg : v->args()) {
      args.push_back(this->s->substitute(arg));
    }
    this->uty.visited = false;
    return MonoTypePtr(TApp::make(f, args));
  }

  MonoTypePtr with(const FixedArray* v) const {
    this->uty.visited = true;
    MonoTypePtr t = this->s->substitute(v->type());
    MonoTypePtr l = this->s->substitute(v->length());
    this->uty.visited = false;
    return MonoTypePtr(FixedArray::make(t, l));
  }

  MonoTypePtr with(const Array* v) const {
    this->uty.visited = true;
    MonoTypePtr e = this->s->substitute(v->type());
    this->uty.visited = false;
    return MonoTypePtr(Array::make(e));
  }

  MonoTypePtr with(const Variant* v) const {
    this->uty.visited = true;
    Variant::Members vms;
    for (const auto& c : v->members()) {
      vms.push_back(Variant::Member(c.selector, this->s->substitute(c.type), c.id));
    }
    this->uty.visited = false;
    return MonoTypePtr(Variant::make(vms));
  }

  MonoTypePtr with(const Record* v) const {
    this->uty.visited = true;
    Record::Members rms;
    for (const auto& f : v->members()) {
      rms.push_back(Record::Member(f.field, this->s->substitute(f.type), f.offset));
    }
    this->uty.visited = false;
    return MonoTypePtr(Record::make(rms));
  }

  MonoTypePtr with(const Func* v) const {
    this->uty.visited = true;
    MonoTypePtr a = this->s->substitute(v->argument());
    MonoTypePtr r = this->s->substitute(v->result());
    this->uty.visited = false;
    return MonoTypePtr(Func::make(a, r));
  }

  MonoTypePtr with(const Exists* v) const {
    this->uty.visited = true;
    MonoTypePtr at = this->s->substitute(v->absType());
    this->uty.visited = false;
    return MonoTypePtr(Exists::make(v->absTypeName(), at));
  }

  MonoTypePtr with(const Recursive* v) const {
    this->uty.visited = true;
    MonoTypePtr rt = this->s->substitute(v->recType());
    this->uty.visited = false;
    return MonoTypePtr(Recursive::make(v->recTypeName(), rt));
  }

  MonoTypePtr with(const TExpr* v) const {
    this->uty.visited = true;
    ExprPtr e = substitute(this->s, v->expr());
    this->uty.visited = false;
    return MonoTypePtr(TExpr::make(e));
  }
};

MonoTypePtr MonoTypeUnifier::substitute(const MonoTypePtr& ty) {
  if (isMonoSingular(ty)) {
    return ty;
  }

  UTypeRec& uty = this->m.find(ty);
  if (uty.donebc > 0 && uty.donebc == this->bcount) {
    return uty.ty;
  } else if (uty.visited) {
    throw std::runtime_error("Cannot infer infinite type");
  } else {
    uty.ty     = switchOf(uty.ty, substituteInto(this, uty));
    uty.donebc = this->bcount;
    return uty.ty;
  }
}

MonoTypes MonoTypeUnifier::substitute(const MonoTypes& ts) {
  MonoTypes result;
  for (MonoTypes::const_iterator t = ts.begin(); t != ts.end(); ++t) {
    result.push_back(substitute(*t));
  }
  return result;
}

size_t MonoTypeUnifier::merge(const MonoTypeUnifier& u) {
  return this->m.merge(u.m);
}

MonoTypeSubst MonoTypeUnifier::substitution() {
  MonoTypeSubst result;
  for (const MonoTypePtr& bt : this->m.values()) {
    if (const TVar* vn = is<TVar>(bt)) {
      MonoTypePtr bv = substitute(bt);
      if (bv != bt) {
        result[vn->name()] = bv;
      }
    }
  }
  return result;
}

scoped_unification_suppression::scoped_unification_suppression(MonoTypeUnifier* u, const std::string& sv) : u(u), sv(sv) {
  this->u->suppress(this->sv);
}

scoped_unification_suppression::~scoped_unification_suppression() {
  this->u->unsuppress(this->sv);
}

// in-place substitutions for qualified types
MonoTypePtr substitute(MonoTypeUnifier* u, const MonoTypePtr& ty) {
  return u->substitute(ty);
}

MonoTypes substitute(MonoTypeUnifier* u, const MonoTypes& tys) {
  MonoTypes r;
  for (MonoTypes::const_iterator ty = tys.begin(); ty != tys.end(); ++ty) {
    r.push_back(u->substitute(*ty));
  }
  return r;
}

ConstraintPtr substitute(MonoTypeUnifier* u, const ConstraintPtr& cst) {
  cst->update(u);
  return cst;
}

Constraints substitute(MonoTypeUnifier* u, const Constraints& cs) {
  for (Constraints::const_iterator c = cs.begin(); c != cs.end(); ++c) {
    substitute(u, *c);
  }
  return cs;
}

QualTypePtr substitute(MonoTypeUnifier* u, const QualTypePtr& qty) {
  qty->constraints() = substitute(u, qty->constraints());
  qty->monoType(substitute(u, qty->monoType()));
  return qty;
}

struct exprTypeSubst : public switchExprTyFnM {
  MonoTypeUnifier* u;
  exprTypeSubst(MonoTypeUnifier* u) : u(u) { }

  QualTypePtr withTy(const QualTypePtr& qt) const {
    return substitute(this->u, qt);
  }
};

ExprPtr substitute(MonoTypeUnifier* u, const ExprPtr& e) {
  switchOf(e, exprTypeSubst(u));
  return e;
}

// a simple test to determine if two types _can_ be unified
bool unifiable(const TEnvPtr& tenv, const MonoTypePtr& t0, const MonoTypePtr& t1) {
  if (*t0 == *t1) {
    return true;
  }

  try {
    MonoTypeUnifier s(tenv);
    mgu(t0, t1, &s);
    s.substitute(t0);
    return true;
  } catch (std::exception&) {
    return false;
  }
}

bool unifiable(const TEnvPtr& tenv, const MonoTypes& ts0, const MonoTypes& ts1) {
  if (ts0.size() != ts1.size()) {
    return false;
  } else if (ts0.size() == 0) {
    return true;
  } else {
    try {
      MonoTypeUnifier s(tenv);
      for (size_t i = 0; i < ts0.size(); ++i) {
        if (!(ts0[i] == ts1[i])) {
          mgu(ts0[i], ts1[i], &s);
        }
      }
      s.substitute(ts0[0]);
      return true;
    } catch (std::exception&) {
      return false;
    }
  }
}

bool unifiable(const TEnvPtr& tenv, const ConstraintPtr& c0, const ConstraintPtr& c1) {
  MonoTypes ts0;
  typeSeqForm(c0, &ts0);

  MonoTypes ts1;
  typeSeqForm(c1, &ts1);

  return unifiable(tenv, ts0, ts1);
}

// allow unqualifiers to fill in any implied type variables in a constraint
//   (e.g.: multi-parameter type classes with functional dependencies)
bool refine(const TEnvPtr& tenv, const ConstraintPtr& c, MonoTypeUnifier* s, Definitions* ds) {
  if (c->state != Constraint::Unresolved) {
    return false;
  } else {
    c->update(s);
    if (tenv->lookupUnqualifier(c)->refine(tenv, c, s, ds)) {
      c->update(s);
      return true;
    } else {
      return false;
    }
  }
}

bool refine(const TEnvPtr& tenv, const Constraints& cs, MonoTypeUnifier* s, Definitions* ds) {
  bool upd = true;
  while (upd) {
    upd = false;
    for (Constraints::const_iterator c = cs.begin(); c != cs.end(); ++c) {
      upd |= refine(tenv, *c, s, ds);
    }
  }
  return false;
}

bool refine(const TEnvPtr& tenv, const QualTypePtr& qty, MonoTypeUnifier* s, Definitions* ds) {
  // first reduce this set of constraints as much as possible
  ConstraintSet cs;
  for (auto c : qty->constraints()) {
    cs.insert(tenv, c, s);
  }

  // then refine the remaining constraints after reduction
  Constraints qtr = cs.constraints();
  bool result = refine(tenv, qtr, s, ds);

  // and the input type should reflect this refined constraint set
  qty->constraints() = qtr;
  return result;
}

bool refine(const TEnvPtr& tenv, const ExprPtr& e, MonoTypeUnifier* s, Definitions* ds) {
  return refine(tenv, e->type(), s, ds);
}

// utilities for working with some basic types
Record::Members makeRecordMembers(const MkRecord::FieldDefs& fds, const MonoTypes& mts) {
  Record::Members r;
  int i = 0;
  for (MkRecord::FieldDefs::const_iterator f = fds.begin(); f != fds.end(); ++f, ++i) {
    r.push_back(Record::Member(f->first, mts[i]));
  }
  return r;
}

// type inference
struct expTypeInfF : public switchExprM<UnitV> {
  const TEnvPtr& tenv;
  MonoTypeUnifier* u;
  Definitions* ds;
  expTypeInfF(const TEnvPtr& tenv, MonoTypeUnifier* u, Definitions* ds) : tenv(tenv), u(u), ds(ds) { }

  const QualTypePtr& qt(const QualTypePtr& qty) {
    refine(this->tenv, qty, this->u, this->ds);
    return qty;
  }

  QualTypePtr qt(const Constraints& csts, const MonoTypePtr& mt) {
    return qt(qualtype(csts, mt));
  }

  UnitV mkprim(Expr* e, const std::string& pn) const {
    e->type(qualtype(MonoTypePtr(Prim::make(pn))));
    return unitv;
  }

  UnitV with(Unit* v)   { return mkprim(v, "unit"); }
  UnitV with(Bool* v)   { return mkprim(v, "bool"); }
  UnitV with(Char* v)   { return mkprim(v, "char"); }
  UnitV with(Byte* v)   { return mkprim(v, "byte"); }
  UnitV with(Short* v)  { return mkprim(v, "short"); }
  UnitV with(Int* v)    { return mkprim(v, "int"); }
  UnitV with(Long* v)   { return mkprim(v, "long"); }
  UnitV with(Int128* v) { return mkprim(v, "int128"); }
  UnitV with(Float* v)  { return mkprim(v, "float"); }
  UnitV with(Double* v) { return mkprim(v, "double"); }

  UnitV with(Var* v) {
    v->type(this->tenv->lookup(v->value())->instantiate());
    return unitv;
  }

  UnitV with(Let* v) {
    switchOf(v->varExpr(), *this);
    switchOf(v->bodyExpr(), expTypeInfF(bindFrame(this->tenv, v->var(), v->varExpr()->type()), this->u, this->ds));

    Constraints cs = mergeConstraints(v->varExpr()->type()->constraints(), v->bodyExpr()->type()->constraints());
    v->type(qt(cs, v->bodyExpr()->type()->monoType()));
    return unitv;
  }

  static TEnvPtr letRecFrame(const TEnvPtr& ptenv, const LetRec::Bindings& bs) {
    // assume that letrec bindings must be functions
    TEnvPtr r(new TEnv(ptenv));
    for (const auto& b : bs) {
      r->bind(b.first, polytype(MonoTypePtr(Func::make(freshTypeVar(), freshTypeVar()))));
    }
    return r;
  }

  UnitV with(LetRec* v) {
    TEnvPtr lrtenv = letRecFrame(this->tenv, v->bindings());

    Constraints cs;
    for (auto& b : v->bindings()) {
      switchOf(b.second, expTypeInfF(lrtenv, this->u, this->ds));
      mgu(b.second, lrtenv->lookup(b.first)->instantiate(), this->u);
      mergeConstraints(b.second->type()->constraints(), &cs);
    }

    switchOf(v->bodyExpr(), expTypeInfF(lrtenv, this->u, this->ds));
    mergeConstraints(v->bodyExpr()->type()->constraints(), &cs);

    v->type(qt(cs, v->bodyExpr()->type()->monoType()));
    return unitv;
  }

  UnitV with(Fn* v) {
    MonoTypes argl = freshTypeVars(v->varNames().size());
    switchOf(v->body(), expTypeInfF(fnFrame(this->tenv, v->varNames(), argl), this->u, this->ds));

    v->type(qt(v->body()->type()->constraints(), MonoTypePtr(Func::make(tuplety(argl), v->body()->type()->monoType()))));
    return unitv;
  }

  UnitV with(App* v) {
    switchOf(v->fn(), *this);

    Constraints csts;
    mergeConstraints(v->fn()->type()->constraints(), &csts);

    MonoTypes atys;
    for (Exprs::iterator a = v->args().begin(); a != v->args().end(); ++a) {
      switchOf(*a, *this);
      mergeConstraints((*a)->type()->constraints(), &csts);
      atys.push_back((*a)->type()->monoType());
    }

    MonoTypePtr irty = freshTypeVar();
    mgu(v->fn(), qt(csts, functy(atys, irty)), this->u);
    v->type(qt(csts, irty));
    return unitv;
  }

  UnitV with(Assign* v) {
    switchOf(v->left(),  *this);
    switchOf(v->right(), *this);

    // for now, we have a special-case for assignment into records
    if (Proj* p = is<Proj>(v->left())) {
      // try to patch the constraint for the left read to instead become a write
      // there should always be such a constraint, given the definition of type inference on record projections
      bool foundcst = false;
      for (const auto& cst : v->left()->type()->constraints()) {
        HasField hf;
        if (dec(cst, &hf)) {
          if (const TString* lbl = is<TString>(hf.fieldName)) {
            MonoTypePtr trecty = this->u->substitute(hf.recordType);
            MonoTypePtr arecty = this->u->substitute(p->record()->type()->monoType());

            if (lbl->value() == p->field() && *trecty == *arecty) {
              hf.direction  = HasField::Write;
              hf.recordType = arecty;
              hf.fieldType  = v->right()->type()->monoType();
              upd(cst, hf);

              p->record()->type()->monoType(arecty);
              foundcst = true;
            }
          }
        }
      }

      if (!foundcst) {
        throw annotated_error(*v, "Internal error, type inference on record assignment is internally inconsistent");
      }

      Constraints cs;
      mergeConstraints(v->left()->type()->constraints(), &cs);
      mergeConstraints(v->right()->type()->constraints(), &cs);

      v->type(qt(cs, MonoTypePtr(Prim::make("unit"))));
      return unitv;
    } else {
      mgu(v->left(), v->right(), this->u);

      Constraints cs = mergeConstraints(v->left()->type()->constraints(), v->right()->type()->constraints());
      v->type(qt(cs, MonoTypePtr(Prim::make("unit"))));
      return unitv;
    }
  }

  UnitV with(MkArray* v) {
    Constraints cs;
    MonoTypePtr ety  = freshTypeVar();
    QualTypePtr qety = qualtype(ety);

    for (Exprs::iterator e = v->values().begin(); e != v->values().end(); ++e) {
      switchOf(*e, *this);
      mgu(*e, qety, this->u);
      cs = mergeConstraints((*e)->type()->constraints(), cs);
    }

    v->type(qt(cs, arrayty(ety)));
    return unitv;
  }

  // V hasfield x:T => <:x:T:> :: V
  UnitV with(MkVariant* v) {
    switchOf(v->value(), *this);

    MonoTypePtr   vty  = freshTypeVar();
    ConstraintPtr c    = ConstraintPtr(new Constraint(CtorVerifier::constraintName(), list(vty, MonoTypePtr(TString::make(v->label())), v->value()->type()->monoType())));
    Constraints   cs   = mergeConstraints(v->value()->type()->constraints(), list(c));

    v->type(qt(cs, vty));
    return unitv;
  }

  UnitV with(MkRecord* v) {
    QualTypes ftys;
    str::set fnames;

    for (MkRecord::FieldDefs::const_iterator f = v->fields().begin(); f != v->fields().end(); ++f) {
      if (!fnames.insert(f->first).second) {
        throw annotated_error(*v, "Duplicate field name introduction: " + f->first);
      }

      switchOf(f->second, *this);
      ftys.push_back(f->second->type());
    }

    QualLiftedMonoTypes qmt = liftQualifiers(ftys);
    v->type(qt(qmt.first, MonoTypePtr(Record::make(makeRecordMembers(v->fields(), qmt.second)))));
    return unitv;
  }

  UnitV with(AIndex* v) {
    v->index(assume(fncall(var("convert", v->index()->la()), list(v->index()), v->index()->la()), primty("long"), v->index()->la()));

    switchOf(v->array(), *this);
    switchOf(v->index(), *this);

    MonoTypePtr elemTy = freshTypeVar();
    mgu(v->array(), qualtype(arrayty(elemTy)), this->u);
    mgu(v->index(), qualtype(Prim::make("long")), this->u);

    Constraints cs = mergeConstraints(v->array()->type()->constraints(), v->index()->type()->constraints());
    v->type(qt(cs, elemTy));
    return unitv;
  }

  // infer a qualified or unqualified type for variant case analysis
  // (depending on whether or not a default case is specified)
  UnitV withGenericMeaning(Case* v) {
    switchOf(v->variant(), *this);

    MonoTypePtr    rty = freshTypeVar();
    Case::Bindings cbs;
    Constraints    cs;
    Constraints    vcs;

    for (Case::Bindings::const_iterator b = v->bindings().begin(); b != v->bindings().end(); ++b) {
      MonoTypePtr bty = freshTypeVar();
      switchOf(b->exp, expTypeInfF(bindFrame(this->tenv, b->vname, bty), this->u, this->ds));

      mgu(b->exp, qualtype(rty), this->u);

      cbs.push_back(Case::Binding(b->selector, b->vname, b->exp));
      cs = mergeConstraints(cs, b->exp->type()->constraints());
      vcs.push_back(ConstraintPtr(new Constraint(CtorVerifier::constraintName(), list(v->variant()->type()->monoType(), MonoTypePtr(TString::make(b->selector)), bty))));
    }
    v->variant()->type(qt(mergeConstraints(vcs, v->variant()->type()->constraints()), v->variant()->type()->monoType()));

    if (v->defaultExpr()) {
      switchOf(v->defaultExpr(), *this);
      mgu(v->defaultExpr(), qualtype(rty), this->u);
      cs = mergeConstraints(cs, v->defaultExpr()->type()->constraints());
    }
    cs = mergeConstraints(cs, v->variant()->type()->constraints());
    v->type(qt(cs, rty));

    return unitv;
  }

  UnitV with(Case* v) {
    // if a default case is specified, the set of variant constructors is open
    if (v->defaultExpr().get() != 0) {
      return withGenericMeaning(v);
    }

    // no default case, assume that this 'case' term fully contains the constructor set
    Case::Bindings   cbs;
    Variant::Members vms;
    MonoTypePtr      rty = freshTypeVar();
    Constraints      cs;
    int              cidx = 0;

    for (Case::Bindings::iterator b = v->bindings().begin(); b != v->bindings().end(); ++b) {
      MonoTypePtr bty = freshTypeVar();
      switchOf(b->exp, expTypeInfF(bindFrame(this->tenv, b->vname, bty), this->u, this->ds));

      mgu(b->exp, qualtype(rty), this->u);

      cbs.push_back(Case::Binding(b->selector, b->vname, b->exp));
      vms.push_back(Variant::Member(b->selector, bty, cidx++));
      cs = mergeConstraints(cs, b->exp->type()->constraints());
    }

    switchOf(v->variant(), *this);

    // now we need to infer the variant type, but as a bit of a hack, let's see if the lhs actually has a type on it
    // and if so then we can patch up the inferred constructor IDs
    if (const Variant* vty = is<Variant>(v->variant()->type()->monoType())) {
      for (size_t i = 0; i < vms.size(); ++i) {
        try {
          vms[i].id = vty->id(vms[i].selector);
        } catch (...) {
        }
      }
    }
    mgu(v->variant(), qualtype(Variant::make(vms)), this->u);

    // finally the type of this case analysis merges constraints across all cases and all branches have the same result type
    cs = mergeConstraints(cs, v->variant()->type()->constraints());
    v->type(qt(cs, rty));
    return unitv;
  }

  UnitV with(Switch* v) {
    switchOf(v->expr(), *this);
    mgu(v->expr(), qualtype(v->bindings()[0].value->primType()), this->u);

    Constraints cs = v->expr()->type()->constraints();

    Switch::Bindings& bs = v->bindings();
    switchOf(bs[0].exp, *this);
    QualTypePtr bty = bs[0].exp->type();
    cs = mergeConstraints(cs, bty->constraints());

    for (size_t i = 1; i < bs.size(); ++i) {
      switchOf(bs[i].exp, *this);
      mgu(bs[i].exp, bty, this->u);
      cs = mergeConstraints(cs, bs[i].exp->type()->constraints());
    }

    if (v->defaultExpr()) {
      switchOf(v->defaultExpr(), *this);
      mgu(v->defaultExpr(), bty, this->u);
      cs = mergeConstraints(cs, v->defaultExpr()->type()->constraints());
    }

    v->type(qt(cs, bty->monoType()));
    return unitv;
  }

  // E.f => (E::R).f :: R hasfield f:t => t
  UnitV with(Proj* v) {
    switchOf(v->record(), *this);

    MonoTypePtr   fty  = freshTypeVar();
    ConstraintPtr fcst = HasField::constraint(HasField::Read, v->record()->type()->monoType(), MonoTypePtr(TString::make(v->field())), fty, v->record());
    Constraints   cs   = mergeConstraints(v->record()->type()->constraints(), list(fcst));

    v->type(qt(cs, fty));
    return unitv;
  }

  UnitV with(Assump* v) {
    switchOf(v->expr(), *this);
    mgu(v->expr(), v->ty(), this->u);

    // hack?  allow user refinement of type predicates
    // this works by unifying user constraints with inferred constraints IFF there's just one unifiable constraint inferred for each user constraint
    QualTypePtr eqty = qt(v->expr()->type()->constraints(), v->expr()->type()->monoType());
    QualTypePtr uqty = qt(v->ty()->constraints(), v->ty()->monoType());

    for (const ConstraintPtr& uc : uqty->constraints()) {
      ConstraintPtr cec; // the one plausible constraint to unify with (if just one can be found)

      for (const ConstraintPtr& ec : eqty->constraints()) {
        ec->update(this->u);
        if (!satisfied(this->tenv, ec, this->ds)) {
          if (ec->name() == uc->name() && unifiable(this->tenv, uc, ec)) {
            if (cec) {
              // ambiguous, do nothing
              cec = ConstraintPtr();
              break;
            } else {
              cec = ec;
            }
          }
        }
      }

      if (cec) {
        mgu(cec, uc, this->u);
      }
    }
    v->type(qt(mergeConstraints(uqty->constraints(), eqty->constraints()), eqty->monoType()));
    return unitv;
  }

  UnitV with(Pack* v) {
    switchOf(v->expr(), *this);

    MonoTypePtr   packedType = freshTypeVar();
    ConstraintPtr pcst       = ConstraintPtr(new Constraint(Existentials::constraintName(), list(v->expr()->type()->monoType(), packedType)));

    Constraints cs = mergeConstraints(v->expr()->type()->constraints(), list(pcst));
    v->type(qt(cs, packedType));
    return unitv;
  }

  UnitV with(Unpack* v) {
    switchOf(v->package(), *this);
    switchOf(v->expr(), expTypeInfF(bindFrame(this->tenv, v->varName(), unpackedType(substitute(this->u, v->package()->type()))), this->u, this->ds));

    Constraints cs = mergeConstraints(v->package()->type()->constraints(), v->expr()->type()->constraints());

    v->type(qt(cs, v->expr()->type()->monoType()));
    return unitv;
  }
};

// apply a substitution across all type references in a term
struct applySubstitutionF : public switchExprTyFnM {
  MonoTypeUnifier* s;
  applySubstitutionF(MonoTypeUnifier* s) : s(s) { }

  QualTypePtr withTy(const QualTypePtr& qt) const {
    return substitute(this->s, qt);
  }
};

// perform type inference, explicit annotation and substitution
ExprPtr validateType(const TEnvPtr& tenv, const ExprPtr& e, Definitions* ds) {
  try {
    MonoTypeUnifier u(tenv);
    switchOf(e, expTypeInfF(tenv, &u, ds));
    refine(tenv, e, &u, ds);
    switchOf(e, applySubstitutionF(&u));
    return e;
  } catch (annotated_error&) {
    throw;
  } catch (std::exception& ex) {
    throw annotated_error(*e, ex.what());
  }
}

ExprPtr validateType(const TEnvPtr& tenv, const std::string& vname, const ExprPtr& e, Definitions* ds) {
  try {
    MonoTypeUnifier u(tenv);

    TEnvPtr rtenv(new TEnv(tenv));
    MonoTypePtr defTy = freshTypeVar();
    rtenv->bind(vname, polytype(defTy));

    ExprPtr result(new Assump(e, qualtype(defTy), e->la()));
    switchOf(result, expTypeInfF(rtenv, &u, ds));
    switchOf(result, applySubstitutionF(&u));
    return result;
  } catch (annotated_error&) {
    throw;
  } catch (std::exception& ex) {
    throw annotated_error(*e, ex.what());
  }
}

// find the most general unifier between two types
void mgu(const ExprPtr& e0, const ExprPtr& e1, MonoTypeUnifier* u) {
  try {
    mgu(e0->type(), e1->type(), u);
  } catch (std::runtime_error& ex) {
    throw annotated_error(list(annmsg(ex.what(), e0->la()), annmsg("while unifying with:", e1->la())));
  }
}

void mgu(const ExprPtr& e, const QualTypePtr& t, MonoTypeUnifier* u) {
  try {
    mgu(e->type(), t, u);
  } catch (std::runtime_error& ex) {
    throw annotated_error(list(annmsg(ex.what(), e->la())));
  }
}

void mgu(const ExprPtr& e, const MonoTypePtr& t, MonoTypeUnifier* u) {
  mgu(e, qualtype(t), u);
}

void mgu(const QualTypePtr& t0, const QualTypePtr& t1, MonoTypeUnifier* u) {
  mgu(t0->monoType(), t1->monoType(), u);
}

void mgu(const MonoTypePtr& t0, const MonoTypePtr& t1, MonoTypeUnifier* u) {
  u->unify(t0, t1);
}

void mgu(const MonoTypes& t0s, const MonoTypes& t1s, MonoTypeUnifier* u) {
  if (t0s.size() == t1s.size()) {
    for (size_t i = 0; i < t0s.size(); ++i) {
      mgu(t0s[i], t1s[i], u);
    }
  } else {
    throw std::runtime_error("Cannot unify parameter lists (length mismatch).");
  }
}

void mgu(const ConstraintPtr& c0, const ConstraintPtr& c1, MonoTypeUnifier* u) {
  MonoTypes c0ts;
  typeSeqForm(c0, &c0ts);

  MonoTypes c1ts;
  typeSeqForm(c1, &c1ts);

  mgu(c0ts, c1ts, u);
}

// utilities for dealing with qualified types
QualLiftedMonoTypes liftQualifiers(const QualTypes& qts) {
  QualLiftedMonoTypes r;
  for (QualTypes::const_iterator qt = qts.begin(); qt != qts.end(); ++qt) {
    mergeConstraints((*qt)->constraints(), &r.first);
    r.second.push_back((*qt)->monoType());
  }
  return r;
}

}

