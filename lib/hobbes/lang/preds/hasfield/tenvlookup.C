
#include <hobbes/lang/preds/hasfield/tenvlookup.H>
#include <hobbes/lang/tyunqualify.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>

namespace hobbes {

// from "object-view" function type to "flat-view" function type
MonoTypePtr objViewToFlatView(const MonoTypePtr& obj, const Func* fty) {
  MonoTypes targl = fty->parameters();
  if (targl.size() == 1 && isUnit(targl[0])) {
    return functy(list(obj), fty->result());
  } else {
    return functy(cons(obj, targl), fty->result());
  }
}

// account for empty parameter lists represented as a single-element parameter list on the unit type
MonoTypes trCons(const MonoTypePtr& hty, const MonoTypes& ttys) {
  if (ttys.size() == 1 && isUnit(ttys[0])) {
    return list(hty);
  } else {
    return cons(hty, ttys);
  }
}

Exprs trCons(const ExprPtr& hexp, const Exprs& texps) {
  if (texps.size() == 1 && is<Unit>(texps[0])) {
    return list(hexp);
  } else {
    return cons(hexp, texps);
  }
}

QualTypePtr underlyingFnType(const TEnvPtr& tenv, const MonoTypePtr& rty, const std::string& fieldName, const MonoTypePtr& fty, MonoTypeUnifier* s) {
  const Func* ffty = is<Func>(fty);
  if (!ffty) return QualTypePtr();

  try {
    QualTypePtr vfty = tenv->lookup(fieldName)->instantiate();
    MonoTypePtr ifty(Func::make(tuplety(trCons(rty, ffty->parameters())), ffty->result()));
    mgu(qualtype(ifty), vfty, s);
    return substitute(s, vfty);
  } catch (std::exception&) {
    return QualTypePtr();
  }
}

QualTypePtr underlyingFnType(const TEnvPtr& tenv, const ConstraintPtr& c) {
  HasField hf;
  if (dec(c, &hf)) {
    if (const TString* fname = is<TString>(hf.fieldName)) {
      MonoTypeUnifier s(tenv);
      return underlyingFnType(tenv, hf.recordType, fname->value(), hf.fieldType, &s);
    }
  }

  return QualTypePtr();
}

bool HFTEnvLookupEliminator::satisfied(const TEnvPtr& tenv, const HasField& hf, Definitions*) const {
  const auto& rty    = hf.recordType;
  const auto& fnamet = hf.fieldName;
  const auto& fty    = hf.fieldType;

  const Func* ffnty = is<Func>(fty);
  if (!ffnty) { return false; }
  const TString* fname = is<TString>(fnamet);
  if (!fname) { return false; }
  if (!isMonoSingular(rty) || !isMonoSingular(fty)) { return false; }
  if (!tenv->hasBinding(fname->value())) { return false; }

  return unifiable(tenv, objViewToFlatView(rty, ffnty), tenv->lookup(fname->value())->instantiate()->monoType());
}

bool HFTEnvLookupEliminator::satisfiable(const TEnvPtr& tenv, const HasField& hf, Definitions*) const {
  auto rty    = hf.recordType;
  auto fnamet = hf.fieldName;
  auto fty    = hf.fieldType;

  if (const TString* fname = is<TString>(fnamet)) {
    const Func* ffnty = is<Func>(fty);
    if (!ffnty) {
      return is<TVar>(fty);
    }
    if (!tenv->hasBinding(fname->value())) {
      return false;
    }
    return unifiable(tenv, objViewToFlatView(rty, ffnty), tenv->lookup(fname->value())->instantiate()->monoType());
  } else {
    return is<TVar>(fnamet);
  }
}

bool HFTEnvLookupEliminator::refine(const TEnvPtr& tenv, const HasField& hf, MonoTypeUnifier* s, Definitions*) {
  auto rty    = hf.recordType;
  auto fnamet = hf.fieldName;
  auto fty    = hf.fieldType;

  // short-circuit refinement -- no need to do anything if there's no information to add
  if (!is<Func>(fty)) { return false; }

  auto invars = tvarNames(rty).size() + tvarNames(fty).size();
  if (invars == 0) { return false; }

  if (const TString* fname = is<TString>(fnamet)) {
    if (tenv->hasBinding(fname->value())) {
      const Func* ufty = is<Func>(tenv->lookup(fname->value())->instantiate()->monoType());
      if (!ufty || ufty->parameters().size() == 0) return false;

      MonoTypePtr uobj     = ufty->parameters()[0];
      MonoTypePtr ufieldty = functy(drop(ufty->parameters(), 1), ufty->result());

      if (unifiable(tenv, rty, uobj) && unifiable(tenv, fty, ufieldty)) {
        mgu(rty, uobj,     s);
        mgu(fty, ufieldty, s);
        return (tvarNames(substitute(s, rty)).size() + tvarNames(substitute(s, fty)).size()) < invars;
      }
    }
  }
  return false;
}

// rewrite funny-looking function calls
struct HFTEnvLookupUnqualify : public switchExprTyFn {
  const TEnvPtr&       tenv;
  const ConstraintPtr& constraint;
  Definitions*         defs;

  HFTEnvLookupUnqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* defs) : tenv(tenv), constraint(cst), defs(defs) {
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }

  ExprPtr with(const Fn* v) const {
    const Func* fty = is<Func>(v->type()->monoType());
    if (!fty) {
      throw annotated_error(*v, "Internal error, expected annotated function type");
    }
    return wrapWithTy(v->type(),
      new Fn(
        v->varNames(), 
        switchOf(v->body(), HFTEnvLookupUnqualify(fnFrame(this->tenv, v->varNames(), fty->parameters()), this->constraint, this->defs)),
        v->la()
      )
    );
  }

  ExprPtr with(const App* v) const {
    // fixup in application position (the preferred position)
    if (const Proj* p = is<Proj>(stripAssumpHead(v->fn()))) {
      if (hasConstraint(this->constraint, p->type())) {
        const Func* fty = is<Func>(p->type()->monoType());
        if (!fty) {
          throw annotated_error(*p, "Internal error, expected annotated function type");
        }

        QualTypePtr nfty = underlyingFnType(this->tenv, this->constraint);
        ExprPtr f(new Var(p->field(), p->la()));
        f->type(nfty);

        ExprPtr napp(new App(f, switchOf(trCons(p->record(), v->args()), *this), v->la()));
        napp->type(qualtype(append(removeConstraint(this->constraint, v->type())->constraints(), nfty->constraints()), v->type()->monoType()));

        if (napp->type()->constraints().size() > 0) {
          return unqualifyTypes(this->tenv, napp, this->defs);
        } else {
          return napp;
        }
      }
    }

    return wrapWithTy(v->type(), new App(switchOf(v->fn(), *this), switchOf(v->args(), *this), v->la()));
  }

  ExprPtr with(const Proj* v) const {
    // this should create a closure
    if (hasConstraint(this->constraint, v->type())) {
      throw annotated_error(*v, "Closure creation from tenv-lookup as record lookup, NYI.");
    }

    return wrapWithTy(v->type(), new Proj(switchOf(v->record(), *this), v->field(), v->la()));
  }
};

ExprPtr HFTEnvLookupEliminator::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  return switchOf(e, HFTEnvLookupUnqualify(tenv, cst, ds));
}

std::string HFTEnvLookupEliminator::name() const { return "type environment lookup"; }

}

