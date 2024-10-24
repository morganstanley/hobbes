
#include <hobbes/lang/expr.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/preds/vtrunc.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>
#include <memory>

namespace hobbes {

MonoTypePtr truncEnumType(const Variant& vty) {
  Variant::Members cs;
  for (const auto& vm : vty.members()) {
    cs.push_back(Variant::Member(vm.selector, primty("unit"), vm.id));
  }
  return Variant::make(cs);
}

struct VariantTruncD {
  MonoTypePtr variantType;
  MonoTypePtr enumType;
};

static bool dec(const ConstraintPtr& c, VariantTruncD* vt) {
  if (c->name() == VariantTruncP::constraintName() && c->arguments().size() == 2) {
    vt->variantType = c->arguments()[0];
    vt->enumType    = c->arguments()[1];
    return true;
  }
  return false;
}

#define REF_VAR_TRUNC "trunc"

std::string VariantTruncP::constraintName() {
  return "VariantTrunc";
}

bool VariantTruncP::refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
  VariantTruncD vt;
  if (dec(cst, &vt)) {
    if (const Variant* vty = is<Variant>(vt.variantType)) {
      auto s = u->size();
      mgu(vt.enumType, truncEnumType(*vty), u);
      return s != u->size();
    }
  }
  return false;
}

bool VariantTruncP::satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
  VariantTruncD vt;
  if (dec(cst, &vt)) {
    if (const auto* vty = is<Variant>(vt.variantType)) {
      return !hasFreeVariables(vt.variantType) && *vt.enumType == *truncEnumType(*vty);
    }
  }
  return false;
}

bool VariantTruncP::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  VariantTruncD vt;
  if (dec(cst, &vt)) {
    if (!hasFreeVariables(vt.variantType) && (is<Variant>(vt.variantType) != nullptr)) {
      return (is<TVar>(vt.enumType) != nullptr) || satisfied(tenv, cst, ds);
    } else {
      return is<TVar>(vt.variantType) != nullptr;
    }
  }
  return false;
}

void VariantTruncP::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

PolyTypePtr VariantTruncP::lookup(const std::string& vn) const {
  if (vn == REF_VAR_TRUNC) {
    // trunc :: (VariantTrunc v e) => v -> e
    return polytype(2, qualtype(list(std::make_shared<Constraint>(VariantTruncP::constraintName(), list(tgen(0), tgen(1)))), functy(list(tgen(0)), tgen(1))));
  } else {
    return PolyTypePtr();
  }
}

SymSet VariantTruncP::bindings() const {
  SymSet r;
  r.insert(REF_VAR_TRUNC);
  return r;
}

FunDeps VariantTruncP::dependencies(const ConstraintPtr&) const {
  FunDeps result;
  result.push_back(FunDep(list(0), 1));
  return result;
}

// resolve satisfied predicates
struct VTUnqualify : public switchExprTyFn {
  const ConstraintPtr& constraint;
  VTUnqualify(const ConstraintPtr& constraint) : constraint(constraint) { }

  QualTypePtr withTy(const QualTypePtr& qt) const override {
    return removeConstraint(this->constraint, qt);
  }

  ExprPtr with(const Var* v) const override {
    if (hasConstraint(this->constraint, v->type())) {
      if (v->value() == REF_VAR_TRUNC) {
        return wrapWithTy(v->type(), new Var("unsafeCast", v->la()));
      }
    }
    return wrapWithTy(v->type(), v->clone());
  }
};

ExprPtr VariantTruncP::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, VTUnqualify(cst));
}

}

