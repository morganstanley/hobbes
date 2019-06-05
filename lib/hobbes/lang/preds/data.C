
#include <hobbes/lang/preds/data.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

std::string DataP::constraintName() {
  return "Data";
}

bool DataP::refine(const TEnvPtr&, const ConstraintPtr& c, MonoTypeUnifier* u, Definitions*) {
  if (c->name() == DataP::constraintName() && c->arguments().size() == 2) {
    if (!hasFreeVariables(c->arguments()[0]) && hasFreeVariables(c->arguments()[1])) {
      size_t z = u->size();
      mgu(c->arguments()[1], repTypeStep(c->arguments()[0]), u);
      return z != u->size();
    }
  }
  return false;
}

bool DataP::satisfied(const TEnvPtr&, const ConstraintPtr& c, Definitions*) const {
  if (c->name() == DataP::constraintName() && c->arguments().size() == 2) {
    return *c->arguments()[1] == *repTypeStep(c->arguments()[0]) && !(*c->arguments()[0] == *c->arguments()[1]);
  }
  return false;
}

bool DataP::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions*) const {
  if (c->name() != DataP::constraintName() || c->arguments().size() != 2) {
    return false;
  } else if (is<TVar>(c->arguments()[0])) {
    return true;
  } else {
    return unifiable(tenv, repTypeStep(c->arguments()[0]), c->arguments()[1]);
  }
}

void DataP::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

struct StripCst : public switchExprTyFn {
  const ConstraintPtr& constraint;
  StripCst(const ConstraintPtr& cst) : constraint(cst) {
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }
};

ExprPtr DataP::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, StripCst(cst));
}

PolyTypePtr DataP::lookup(const std::string&) const {
  return PolyTypePtr();
}

SymSet DataP::bindings() const {
  return SymSet();
}

FunDeps DataP::dependencies(const ConstraintPtr&) const {
  return list(FunDep(list(0), 1));
}

}

