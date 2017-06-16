
#include <hobbes/lang/preds/data.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

std::string DataP::constraintName() {
  return "Data";
}

bool DataP::refine(const TEnvPtr&, const ConstraintPtr& c, MonoTypeUnifier* u, Definitions*) {
  if (c->name() == DataP::constraintName() && c->arguments().size() == 2) {
    if (const Prim* pt = is<Prim>(c->arguments()[0])) {
      if (pt->representation()) {
        size_t z = u->size();
        mgu(c->arguments()[1], pt->representation(), u);
        return z != u->size();
      }
    }
  }
  return false;
}

bool DataP::satisfied(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions*) const {
  if (c->name() == DataP::constraintName() && c->arguments().size() == 2) {
    if (const Prim* pt = is<Prim>(c->arguments()[0])) {
      if (pt->representation()) {
        return *c->arguments()[1] == *pt->representation();
      }
    }
  }
  return false;
}

bool DataP::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) const {
  if (c->name() != DataP::constraintName() || c->arguments().size() != 2) {
    return false;
  } else if (is<TVar>(c->arguments()[0])) {
    return true;
  } else if (const Prim* pt = is<Prim>(c->arguments()[0])) {
    return pt->representation() && unifiable(tenv, pt->representation(), c->arguments()[1]);
  } else {
    return false;
  }
}

void DataP::explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs) {
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

