
#include <hobbes/lang/preds/typeof.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/tyunqualify.H>

namespace hobbes {

std::string TypeofP::constraintName() {
  return "TypeOf";
}

static bool decodeTO(const ConstraintPtr& c, ExprPtr* e, MonoTypePtr* t) {
  if (c->arguments().size() == 2) {
    if (const TApp* etapp = is<TApp>(c->arguments()[0])) {
      if (const Prim* etan = is<Prim>(etapp->fn())) {
        if (etapp->args().size() == 1 && etan->name() == "quote") {
          if (const TExpr* te = is<TExpr>(etapp->args()[0])) {
            *e = te->expr();
            *t = c->arguments()[1];
            return true;
          }
        }
      }
    }
  }
  
  return false;
}

static bool decideTO(const TEnvPtr& tenv, Definitions* ds, const ConstraintPtr& c, MonoTypePtr* t0, MonoTypePtr* t1) {
  ExprPtr e;
  if (!decodeTO(c, &e, t1)) {
    return false;
  } else if (!e->type()) {
    e->type(unqualifyTypes(tenv, validateType(tenv, e, ds), ds)->type());
  }
  *t0 = requireMonotype(e->type());
  return true;
}

bool TypeofP::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions* ds) {
  MonoTypePtr t0, t1;

  size_t z = u->size();
  try {
    if (decideTO(tenv, ds, cst, &t0, &t1) && hasFreeVariables(t1)) {
      mgu(t0, t1, u);
    }
  } catch (std::exception&) {
    return false;
  }
  return z != u->size();
}

bool TypeofP::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  MonoTypePtr t0, t1;

  if (decideTO(tenv, ds, cst, &t0, &t1) && !hasFreeVariables(t1)) {
    return *t0 == *t1;
  }
  return false;
}

bool TypeofP::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) const {
  if (c->name() != TypeofP::constraintName() || c->arguments().size() != 2) {
    return false;
  } else {
    return hasFreeVariables(c->arguments()[0]) || satisfied(tenv, c, ds);
  }
}

void TypeofP::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
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

ExprPtr TypeofP::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, StripCst(cst));
}

PolyTypePtr TypeofP::lookup(const std::string&) const {
  return PolyTypePtr();
}

SymSet TypeofP::bindings() const {
  return SymSet();
}

FunDeps TypeofP::dependencies(const ConstraintPtr&) const {
  return list(FunDep(list(0), 1));
}

}

