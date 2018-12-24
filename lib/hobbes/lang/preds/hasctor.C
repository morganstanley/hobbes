
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>

#include <hobbes/lang/preds/hasctor/variant.H>

namespace hobbes {

#define MSELECT_CTOR_FN "maybeFromCtor"

/////////////////////////////////////////////////////
// generic field verification and constraint removal
/////////////////////////////////////////////////////
CtorVerifier::CtorVerifier() {
  this->eliminators.push_back(new HCVariantEliminator());
}

std::string CtorVerifier::constraintName() {
  return "HasCtor";
}

void CtorVerifier::addEliminator(HCEliminator* hce) {
  this->eliminators.push_back(hce);
}

HCEliminator* CtorVerifier::findEliminator(const TEnvPtr& tenv, const HasCtor& hc, Definitions* ds) const {
  for (auto hce : this->eliminators) {
    if (hce->satisfiable(tenv, hc, ds)) {
      return hce;
    }
  }
  return 0;
}

bool CtorVerifier::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* s, Definitions* ds) {
  HasCtor hc;
  if (dec(cst, &hc)) {
    if (HCEliminator* hce = findEliminator(tenv, hc, ds)) {
      return hce->refine(tenv, hc, s, ds);
    }
  }
  return false;
}

bool CtorVerifier::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  HasCtor hc;
  if (dec(cst, &hc)) {
    if (HCEliminator* hce = findEliminator(tenv, hc, ds)) {
      return hce->satisfied(tenv, hc, ds);
    }
  }
  return false;
}

bool CtorVerifier::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  HasCtor hc;
  return dec(cst, &hc) && findEliminator(tenv, hc, ds);
}

void CtorVerifier::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

struct RewriteMSelect : public switchExprTyFn {
  HasCtor       hc;
  ConstraintPtr cst;
  std::string   ctor;

  RewriteMSelect(const HasCtor& hc, const ConstraintPtr& cst) : hc(hc), cst(cst) {
    const TString* fn = is<TString>(hc.ctorlbl);
    if (!fn) { throw std::runtime_error("Internal error, invalid hasctor constraint for unqualification"); }
    this->ctor = fn->value();
  }

  ExprPtr with(const App* ap) const {
    if (ap->args().size() == 1) {
      if (hasConstraint(this->cst, ap->type())) {
        if (const Var* f = is<Var>(stripAssumpHead(ap->fn()))) {
          if (f->value() == MSELECT_CTOR_FN) {
            ExprPtr r(new Case(ap->args()[0], list(Case::Binding(this->ctor, ".p", justE(var(".p", this->hc.ctorty, ap->la()), ap->la()))), nothingE(this->hc.ctorty, ap->la()), ap->la()));
            r->type(qualtype(maybety(this->hc.ctorty)));
            return r;
          }
        }
      }
    }
    return wrapWithTy(ap->type(), new App(switchOf(ap->fn(), *this), switchOf(ap->args(), *this), ap->la()));
  }

  ExprPtr with(const Var* v) const {
    if (hasConstraint(this->cst, v->type())) {
      if (v->value() == MSELECT_CTOR_FN) {
        std::string vn = freshName();

        ExprPtr b(new Case(var(vn, this->hc.variant, v->la()), list(Case::Binding(this->ctor, ".p", justE(var(".p", this->hc.ctorty, v->la()), v->la()))), nothingE(this->hc.ctorty, v->la()), v->la()));
        b->type(qualtype(maybety(this->hc.ctorty)));
        
        ExprPtr rv = fn(vn, b, v->la());
        rv->type(qualtype(functy(this->hc.variant, maybety(this->hc.ctorty))));
        return rv;
      }
    }
    return wrapWithTy(v->type(), new Var(v->value(), v->la()));
  }
};


ExprPtr CtorVerifier::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  HasCtor hc;
  if (dec(cst, &hc)) {
    if (HCEliminator* hce = findEliminator(tenv, hc, ds)) {
      return hce->unqualify(tenv, cst, hc, switchOf(e, RewriteMSelect(hc, cst)), ds);
    }
  }
  throw annotated_error(*e, "Cannot unqualify constraint: " + show(cst));
}

PolyTypePtr CtorVerifier::lookup(const std::string& vn) const {
  if (vn == MSELECT_CTOR_FN) {
    // select either the given constructor payload from a variant, or nothing
    // :: (|c/p|::v) => v -> (()+p)
    return polytype(3, qualtype(list(ConstraintPtr(new Constraint(CtorVerifier::constraintName(), list(tgen(0), tgen(1), tgen(2))))), functy(list(tgen(0)), maybety(tgen(2)))));
  } else {
    return PolyTypePtr();
  }
}

SymSet CtorVerifier::bindings() const {
  SymSet r;
  r.insert(MSELECT_CTOR_FN);
  return r;
}

FunDeps CtorVerifier::dependencies(const ConstraintPtr&) const {
  FunDeps result;
  result.push_back(FunDep(list(0, 1), 2));
  return result;
}

bool CtorVerifier::dec(const ConstraintPtr& c, HasCtor* hc) {
  if (c->name() == CtorVerifier::constraintName() && c->arguments().size() == 3) {
    hc->variant = c->arguments()[0];
    hc->ctorlbl = c->arguments()[1];
    hc->ctorty  = c->arguments()[2];
    return true;
  }
  return false;
}

}

