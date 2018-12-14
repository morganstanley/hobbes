
#include <hobbes/lang/preds/equal.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

static bool dec(const ConstraintPtr& c, MonoTypePtr* lhs, MonoTypePtr* rhs) {
  if (c->name() == EqualTypes::constraintName() && c->arguments().size() == 2) {
    *lhs = c->arguments()[0];
    *rhs = c->arguments()[1];
    return true;
  }
  return false;
}

/****
 * enforce/translate type judgements
 ****/
std::string EqualTypes::constraintName() {
  return "equals";
}

bool EqualTypes::refine(const TEnvPtr&, const ConstraintPtr&, MonoTypeUnifier*, Definitions*) {
  return false;
}

bool EqualTypes::satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
  MonoTypePtr lhs, rhs;
  return dec(cst, &lhs, &rhs) && *lhs == *rhs;
}

bool EqualTypes::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const {
  MonoTypePtr lhs, rhs;
  return dec(cst, &lhs, &rhs) && unifiable(tenv, lhs, rhs);
}

void EqualTypes::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

struct StripEqCst : public switchExprTyFn {
  const ConstraintPtr& constraint;
  StripEqCst(const ConstraintPtr& cst) : constraint(cst) {
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }
};

ExprPtr EqualTypes::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  // equality between types has no runtime significance
  return switchOf(e, StripEqCst(cst));
}

PolyTypePtr EqualTypes::lookup(const std::string&) const {
  return PolyTypePtr(); // this won't be necessary
}

SymSet EqualTypes::bindings() const {
  return SymSet(); // no overloaded symbols are defined here
}

FunDeps EqualTypes::dependencies(const ConstraintPtr&) const {
  return FunDeps();
}

}

