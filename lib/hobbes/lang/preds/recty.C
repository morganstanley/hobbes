
#include <hobbes/lang/preds/recty.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

struct IsoRecur {
  MonoTypePtr rolled;
  MonoTypePtr unrolled;
};

static bool dec(const ConstraintPtr& c, IsoRecur* ir) {
  if (c->name() == FixIsoRecur::constraintName() && c->arguments().size() == 2) {
    ir->rolled   = c->arguments()[0];
    ir->unrolled = c->arguments()[1];
    return true;
  }
  return false;
}

#define RECTY_ROLL   "roll"
#define RECTY_UNROLL "unroll"

std::string FixIsoRecur::constraintName() {
  return "IsoRecur";
}

// resolve recursive type constraints
bool FixIsoRecur::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
  size_t uc = u->size();
  IsoRecur ir;
  if (dec(cst, &ir)) {
    if (is<Recursive>(ir.rolled)) {
      mgu(unroll(ir.rolled), ir.unrolled, u);
    }
  }
  return uc != u->size();
}

bool FixIsoRecur::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const {
  IsoRecur ir;
  if (dec(cst, &ir)) {
    if (is<Recursive>(ir.rolled)) {
      // make sure that the recursive type unrolls to the unrolled type
      return (*unroll(ir.rolled) == *ir.unrolled);
    }
  }
  return false;
}

bool FixIsoRecur::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const {
  IsoRecur ir;
  if (dec(cst, &ir)) {
    if (is<TVar>(ir.rolled)) {
      return true;
    } else if (is<Recursive>(ir.rolled)) {
      if (is<TVar>(ir.unrolled)) {
        return true;
      } else {
        return unifiable(tenv, unroll(ir.rolled), ir.unrolled);
      }
    } else {
      return false;
    }
  } else {
    return false;
  }
}

void FixIsoRecur::explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs) {
}

struct IsoRecUnqualify : public switchExprTyFn {
  const ConstraintPtr& constraint;

  IsoRecUnqualify(const ConstraintPtr& constraint) : constraint(constraint) {
  }

  QualTypePtr withTy(const QualTypePtr& qt) const {
    return removeConstraint(this->constraint, qt);
  }

  ExprPtr with(const Var* v) const {
    if (hasConstraint(this->constraint, v->type())) {
      // replace safe functions with 'unsafe' ones
      if (v->value() == RECTY_ROLL) {
        return assume(wrapWithTy(v->type(), new Var(".cast", v->la())), withTy(v->type()), v->la());
      } else if (v->value() == RECTY_UNROLL) {
        return assume(wrapWithTy(v->type(), new Var(".cast", v->la())), withTy(v->type()), v->la());
      }
    }
    return wrapWithTy(v->type(), v->clone());
  }
};

ExprPtr FixIsoRecur::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, IsoRecUnqualify(cst));
}

PolyTypePtr FixIsoRecur::lookup(const std::string& vn) const {
  if (vn == RECTY_ROLL) {
    return polytype(2, qualtype(list(ConstraintPtr(new Constraint(FixIsoRecur::constraintName(), list(tgen(0), tgen(1))))), functy(list(tgen(1)), tgen(0))));
  } else if (vn == RECTY_UNROLL) {
    return polytype(2, qualtype(list(ConstraintPtr(new Constraint(FixIsoRecur::constraintName(), list(tgen(0), tgen(1))))), functy(list(tgen(0)), tgen(1))));
  } else {
    return PolyTypePtr();
  }
}

SymSet FixIsoRecur::bindings() const {
  SymSet r;
  r.insert(RECTY_ROLL);
  r.insert(RECTY_UNROLL);
  return r;
}

FunDeps FixIsoRecur::dependencies(const ConstraintPtr&) const {
  FunDeps result;
  result.push_back(FunDep(list(0), 1));
  result.push_back(FunDep(list(1), 0));
  return result;
}

}

