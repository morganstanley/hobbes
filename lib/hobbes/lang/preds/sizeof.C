
#include <hobbes/lang/preds/sizeof.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

#define REF_SIZEOF "sizeOf"

std::string SizeOfP::constraintName() {
  return "SizeOf";
}

bool SizeOfP::refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
  size_t uc = u->size();
  if (cst->arguments().size() == 2 && !hasFreeVariables(cst->arguments()[0])) {
    mgu(cst->arguments()[1], tlong(sizeOf(cst->arguments()[0])), u);
  }
  return uc != u->size();
}

bool SizeOfP::satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
  if (cst->arguments().size() == 2 && !hasFreeVariables(cst->arguments()[0])) {
    if (const TLong* n = is<TLong>(cst->arguments()[1])) {
      return n->value() == sizeOf(cst->arguments()[0]);
    }
  }
  return false;
}

bool SizeOfP::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  return cst->arguments().size() == 2 && (hasFreeVariables(cst->arguments()[0]) || satisfied(tenv, cst, ds));
}

void SizeOfP::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

struct SizeOfPUnqualify : public switchExprTyFn {
  const ConstraintPtr& constraint;

  SizeOfPUnqualify(const ConstraintPtr& constraint) : constraint(constraint) {
  }

  QualTypePtr withTy(const QualTypePtr& qt) const {
    return removeConstraint(this->constraint, qt);
  }

  ExprPtr with(const Var* vn) const {
    if (vn->value() == REF_SIZEOF) {
      return constant(static_cast<size_t>(sizeOf(this->constraint->arguments()[0])), vn->la());
    } else {
      return wrapWithTy(vn->type(), new Var(vn->value(), vn->la()));
    }
  }
};

ExprPtr SizeOfP::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, SizeOfPUnqualify(cst));
}

PolyTypePtr SizeOfP::lookup(const std::string& vn) const {
  if (vn == REF_SIZEOF) {
    return polytype(2, qualtype(list(ConstraintPtr(new Constraint(SizeOfP::constraintName(), list(tgen(0), tgen(1))))), primty("long")));
  } else {
    return PolyTypePtr();
  }
}

SymSet SizeOfP::bindings() const {
  SymSet r;
  r.insert(REF_SIZEOF);
  return r;
}

FunDeps SizeOfP::dependencies(const ConstraintPtr&) const {
  FunDeps result;
  result.push_back(FunDep(list(0), 1));
  return result;
}

}

