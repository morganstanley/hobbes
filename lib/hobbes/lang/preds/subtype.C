#include <hobbes/lang/preds/subtype.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>

namespace hobbes {

bool dec(const ConstraintPtr& c, Subtype* st) {
  if (c->name() == SubtypeUnqualifier::constraintName() && c->arguments().size() == 2) {
    st->lower   = c->arguments()[0];
    st->greater = c->arguments()[1];
    return true;
  }
  return false;
}

// the subtype eliminator
SubtypeUnqualifier::SubtypeUnqualifier() {
}

std::string SubtypeUnqualifier::constraintName() {
  return "Subtype";
}

void SubtypeUnqualifier::addEliminator(SubtypeEliminator* e) {
  this->eliminators.push_back(e);
}

SubtypeEliminator* SubtypeUnqualifier::findEliminator(const TEnvPtr& tenv, const Subtype& st) const {
  for (SubtypeEliminators::const_iterator e = this->eliminators.begin(); e != this->eliminators.end(); ++e) {
    if ((*e)->satisfiable(tenv, st.lower, st.greater)) {
      return *e;
    }
  }
  return 0;
}

bool SubtypeUnqualifier::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* s, Definitions*) {
  Subtype st;
  if (dec(cst, &st)) {
    if (SubtypeEliminator* e = findEliminator(tenv, st)) {
      return e->refine(tenv, st.lower, st.greater, s);
    }
  }
  return false;
}

bool SubtypeUnqualifier::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const {
  Subtype st;
  if (dec(cst, &st)) {
    if (const SubtypeEliminator* e = findEliminator(tenv, st)) {
      return e->satisfied(tenv, st.lower, st.greater);
    }
  }
  return false;
}

bool SubtypeUnqualifier::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const {
  Subtype st;
  return dec(cst, &st) && findEliminator(tenv, st) != 0;
}

void SubtypeUnqualifier::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

ExprPtr SubtypeUnqualifier::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& expr, Definitions* ds) const {
  Subtype st;
  if (dec(cst, &st)) {
    if (const SubtypeEliminator* e = findEliminator(tenv, st)) {
      return e->unqualify(tenv, cst, expr, ds);
    }
  }
  throw annotated_error(*expr, "Cannot unqualify constraint: " + show(cst));
}

PolyTypePtr SubtypeUnqualifier::lookup(const std::string& vn) const {
  for (SubtypeEliminators::const_iterator e = this->eliminators.begin(); e != this->eliminators.end(); ++e) {
    PolyTypePtr r = (*e)->lookup(vn);
    if (r) {
      return r;
    }
  }
  return PolyTypePtr();
}

SymSet SubtypeUnqualifier::bindings() const {
  SymSet r;
  for (SubtypeEliminators::const_iterator e = this->eliminators.begin(); e != this->eliminators.end(); ++e) {
    SymSet x = (*e)->bindings();
    r.insert(x.begin(), x.end());
  }
  return r;
}

FunDeps SubtypeUnqualifier::dependencies(const ConstraintPtr&) const {
  return FunDeps();
}

}

