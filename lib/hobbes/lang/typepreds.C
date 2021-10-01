
#include <hobbes/lang/typepreds.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

void UnqualifierSet::add(const std::string& name, const UnqualifierPtr& uq) {
  if (this->uqs.find(name) != this->uqs.end()) {
    throw std::runtime_error("Redefinition of the '" + name + "' type predicate.");
  } else {
    this->uqs[name] = uq;
  }
}

UnqualifierPtr UnqualifierSet::findUnqualifier(const std::string& name) {
  Unqualifiers::const_iterator uq = this->uqs.find(name);
  if (uq == this->uqs.end()) {
    throw std::runtime_error("Undefined predicate: " + name);
  } else {
    return uq->second;
  }
}

const UnqualifierSet::Unqualifiers& UnqualifierSet::unqualifiers() const {
  return this->uqs;
}

bool UnqualifierSet::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions* ds) {
  bool upd = false;
  Unqualifiers::const_iterator uq = this->uqs.find(cst->name());
  if (uq != this->uqs.end()) {
    upd |= uq->second->refine(tenv, cst, u, ds);
  }
  return upd;
}

bool UnqualifierSet::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  auto uq = this->uqs.find(cst->name());
  if (uq == this->uqs.end()) {
    return false;
  } else {
    return uq->second->satisfied(tenv, cst, ds);
  }
}

bool UnqualifierSet::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  auto uq = this->uqs.find(cst->name());
  if (uq == this->uqs.end()) {
    return false;
  } else {
    return uq->second->satisfiable(tenv, cst, ds);
  }
}

void UnqualifierSet::explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs) {
  auto uq = this->uqs.find(cst->name());
  if (uq != this->uqs.end()) {
    uq->second->explain(tenv, cst, e, ds, msgs);
  }
}

ExprPtr UnqualifierSet::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  auto uq = this->uqs.find(cst->name());
  if (uq == this->uqs.end()) {
    throw annotated_error(*e, "Unknown predicate '" + cst->name() + "' can't be unqualified.");
  } else {
    return uq->second->unqualify(tenv, cst, e, ds);
  }
}

PolyTypePtr UnqualifierSet::lookup(const std::string& vn) const {
  for (auto uq = this->uqs.begin(); uq != this->uqs.end(); ++uq) {
    PolyTypePtr pt = uq->second->lookup(vn);
    if (pt != PolyTypePtr()) {
      return pt;
    }
  }
  return PolyTypePtr();
}

SymSet UnqualifierSet::bindings() const {
  SymSet r;
  for (auto uq = this->uqs.begin(); uq != this->uqs.end(); ++uq) {
    SymSet qr = uq->second->bindings();
    r.insert(qr.begin(), qr.end());
  }
  return r;
}

FunDeps UnqualifierSet::dependencies(const ConstraintPtr& cst) const {
  auto uq = this->uqs.find(cst->name());
  if (uq == this->uqs.end()) {
    return FunDeps();
  } else {
    return uq->second->dependencies(cst);
  }
}

bool hasConstraint(const ConstraintPtr& c, const Constraints& cs) {
  Constraints r;
  for (auto ci = cs.begin(); ci != cs.end(); ++ci) {
    if (*c == **ci) {
      return true;
    }
  }
  return false;
}

bool hasConstraint(const ConstraintPtr& c, const QualTypePtr& qt) {
  return hasConstraint(c, qt->constraints());
}

Constraints removeConstraint(const ConstraintPtr& c, const Constraints& cs) {
  Constraints r;
  for (auto ci = cs.begin(); ci != cs.end(); ++ci) {
    if (!(*c == **ci)) {
      r.push_back(*ci);
    }
  }
  return r;
}

QualTypePtr removeConstraint(const ConstraintPtr& c, const QualTypePtr& qt) {
  return qualtype(removeConstraint(c, qt->constraints()), qt->monoType());
}

}

