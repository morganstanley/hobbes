
#include <hobbes/lang/preds/not.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

static bool dec(const ConstraintPtr& c, MonoTypePtr* lhs, MonoTypePtr* rhs) {
  if (c->name() == NotEqualTypes::constraintName() && c->arguments().size() == 2) {
    *lhs = c->arguments()[0];
    *rhs = c->arguments()[1];
    return true;
  }
  return false;
}

/****
 * enforce/translate type judgements
 ****/
std::string NotEqualTypes::constraintName() {
  return "!equals";
}

bool NotEqualTypes::refine(const TEnvPtr&, const ConstraintPtr&, MonoTypeUnifier*, Definitions*) {
  return false;
}

bool NotEqualTypes::satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
  MonoTypePtr lhs, rhs;
  return dec(cst, &lhs, &rhs) && !hasFreeVariables(lhs) && !hasFreeVariables(rhs) && !(*lhs == *rhs);
}

bool NotEqualTypes::satisfiable(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
  MonoTypePtr lhs, rhs;
  return dec(cst, &lhs, &rhs) && (hasFreeVariables(lhs) || hasFreeVariables(rhs) || !(*lhs == *rhs));
}

void NotEqualTypes::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
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

ExprPtr NotEqualTypes::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, StripCst(cst));
}

PolyTypePtr NotEqualTypes::lookup(const std::string&) const {
  return PolyTypePtr(); // this won't be necessary
}

SymSet NotEqualTypes::bindings() const {
  return SymSet(); // no overloaded symbols are defined here
}

FunDeps NotEqualTypes::dependencies(const ConstraintPtr&) const {
  return FunDeps();
}

}

