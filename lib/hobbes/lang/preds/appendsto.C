
#include <hobbes/lang/preds/appendsto.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/util/array.H>

#include <hobbes/lang/preds/appendsto/record.H>

namespace hobbes {

/////////////////////////////////////////////////////
// append two types together
/////////////////////////////////////////////////////
bool dec(const ConstraintPtr& c, AppendsTo* r) {
  if (c->name() == AppendsToUnqualifier::constraintName() && c->arguments().size() == 3) {
    r->leftType   = c->arguments()[0];
    r->rightType  = c->arguments()[1];
    r->resultType = c->arguments()[2];
    return true;
  }
  return false;
}

AppendsToUnqualifier::AppendsToUnqualifier() {
  addEliminator(new ATRecordEliminator());
}

std::string AppendsToUnqualifier::constraintName() {
  return "AppendsTo";
}

void AppendsToUnqualifier::addEliminator(ATEliminator* ate) {
  this->eliminators.push_back(ate);
}

ATEliminator* AppendsToUnqualifier::findEliminator(const TEnvPtr& tenv, const AppendsTo* at) const {
  for (ATEliminators::const_iterator ate = this->eliminators.begin(); ate != this->eliminators.end(); ++ate) {
    if ((*ate)->satisfiable(tenv, at->leftType, at->rightType, at->resultType)) {
      return *ate;
    }
  }
  return 0;
}

bool AppendsToUnqualifier::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* s, Definitions*) {
  AppendsTo at;
  if (dec(cst, &at)) {
    if (ATEliminator* ate = findEliminator(tenv, &at)) {
      return ate->refine(tenv, at.leftType, at.rightType, at.resultType, s);
    }
  }
  return false;
}

bool AppendsToUnqualifier::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const {
  AppendsTo at;
  if (dec(cst, &at)) {
    if (const ATEliminator* ate = findEliminator(tenv, &at)) {
      return ate->satisfied(tenv, at.leftType, at.rightType, at.resultType);
    }
  }
  return false;
}

bool AppendsToUnqualifier::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const {
  AppendsTo at;
  return dec(cst, &at) && findEliminator(tenv, &at) != 0;
}

void AppendsToUnqualifier::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

ExprPtr AppendsToUnqualifier::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  AppendsTo at;
  if (dec(cst, &at)) {
    if (const ATEliminator* ate = findEliminator(tenv, &at)) {
      return ate->unqualify(tenv, cst, e, ds);
    }
  }
  throw annotated_error(*e, "Cannot unqualify constraint: " + show(cst));
}

PolyTypePtr AppendsToUnqualifier::lookup(const std::string& vn) const {
  for (ATEliminators::const_iterator ate = this->eliminators.begin(); ate != this->eliminators.end(); ++ate) {
    PolyTypePtr ty = (*ate)->lookup(vn);
    if (ty != PolyTypePtr()) {
      return ty;
    }
  }
  return PolyTypePtr();
}

SymSet AppendsToUnqualifier::bindings() const {
  SymSet result;
  for (ATEliminators::const_iterator ate = this->eliminators.begin(); ate != this->eliminators.end(); ++ate) {
    SymSet x = (*ate)->bindings();
    result.insert(x.begin(), x.end());
  }
  return result;
}

FunDeps AppendsToUnqualifier::dependencies(const ConstraintPtr&) const {
  FunDeps result;
  result.push_back(FunDep(list(0, 1), 2));
  result.push_back(FunDep(list(0, 2), 1));
  result.push_back(FunDep(list(1, 2), 0));
  return result;
}

}

