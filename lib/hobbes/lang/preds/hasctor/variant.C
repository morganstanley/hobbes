
#include "hobbes/lang/type.H"
#include <hobbes/lang/preds/hasctor/variant.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

const Variant *isVariantPEnum(const MonoTypePtr &e) {
  if (const TApp *tappty = is<TApp>(e)) {
    if (const Prim *fn = is<Prim>(tappty->fn())) {
      if (fn->name() == "penum" && tappty->args().size() == 2) {
        return is<Variant>(tappty->args()[1]);
      }
    }
  }
  return nullptr;
}

static const Variant *isMaybeNestedVariant(const MonoTypePtr &e) {
  if (const auto *vty = is<Variant>(e)) {
    return vty;
  }
  return isVariantPEnum(e);
}

bool HCVariantEliminator::satisfied(const TEnvPtr&, const HasCtor& hc, Definitions*) const {
  if (const Variant* vty = isMaybeNestedVariant(hc.variant)) {
    if (const TString* fname = is<TString>(hc.ctorlbl)) {
      if (const Variant::Member* m = vty->mmember(fname->value())) {
        return *m->type == *hc.ctorty;
      }
    }
  }
  return false;
}

bool HCVariantEliminator::satisfiable(const TEnvPtr& tenv, const HasCtor& hc, Definitions*) const {
  if (const Variant* vty = isMaybeNestedVariant(hc.variant)) {
    if (const TString* fname = is<TString>(hc.ctorlbl)) {
      const Variant::Member* vm = vty->mmember(fname->value());
      return vm != 0 && unifiable(tenv, vm->type, hc.ctorty);
    } else {
      return is<TVar>(hc.ctorlbl);
    }
  }
  return is<TVar>(hc.variant);
}

bool HCVariantEliminator::refine(const TEnvPtr&, const HasCtor& hc, MonoTypeUnifier* s, Definitions*) {
  if (const Variant* vty = isMaybeNestedVariant(hc.variant)) {
    if (const TString* fname = is<TString>(hc.ctorlbl)) {
      if (const Variant::Member* m = vty->mmember(fname->value())) {
        size_t uc = s->size();
        mgu(hc.ctorty, normIfOpaquePtr(m->type), s);
        return uc != s->size();
      }
    }
  }
  return false;
}

// just remove the 'hasfield' constraint from variant constructor expressions
struct HCVariantUnqualify : public switchExprTyFn {
  const ConstraintPtr& constraint;
  HCVariantUnqualify(const ConstraintPtr& cst) : constraint(cst) {
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const override {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }
};

ExprPtr HCVariantEliminator::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const HasCtor&, const ExprPtr& e, Definitions*) const {
  return switchOf(e, HCVariantUnqualify(cst));
}

std::string HCVariantEliminator::name() const { return "variant constructors"; }

}

