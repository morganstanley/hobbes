
#include <hobbes/lang/preds/loadref.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

#define REF_LOAD "load"

struct FRefLoadable {
  MonoTypePtr ref;
  MonoTypePtr ty;
};

static bool dec(const ConstraintPtr& c, FRefLoadable* fl) {
  if (c->name() == DBFileRefLoader::constraintName() && c->arguments().size() == 2) {
    fl->ref = c->arguments()[0];
    fl->ty  = c->arguments()[1];
    return true;
  }
  return false;
}

std::string DBFileRefLoader::constraintName() {
  return "Loadable";
}

// resolve requests to load values from a local database
bool DBFileRefLoader::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
  size_t uc = u->size();
  FRefLoadable fref;
  if (dec(cst, &fref)) {
    if (const TApp* ap = is<TApp>(fref.ref)) {
      if (const Prim* f = is<Prim>(ap->fn())) {
        if (f->name() == "fileref") {
          if (ap->args().size() > 1) {
            mgu(ap->args()[1], fref.ty, u);
          }
        }
      }
    }
  }
  return uc != u->size();
}

bool DBFileRefLoader::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const {
  FRefLoadable fref;
  if (dec(cst, &fref)) {
    if (const TApp* ap = is<TApp>(fref.ref)) {
      if (const Prim* f = is<Prim>(ap->fn())) {
        if (f->name() == "fileref" && ap->args().size() == 2) {
          if (const TLong* idf = is<TLong>(ap->args()[0])) {
            return idf->value() != 0 && *ap->args()[1] == *fref.ty;
          }
        }
      }
    }
  }
  return false;
}

bool DBFileRefLoader::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const {
  FRefLoadable fref;
  if (dec(cst, &fref)) {
    if (const TApp* ap = is<TApp>(fref.ref)) {
      if (const Prim* f = is<Prim>(ap->fn())) {
        if (f->name() == "fileref" && ap->args().size() == 2) {
          if (const TLong* idf = is<TLong>(ap->args()[0])) {
            return idf->value() != 0 && (*ap->args()[1] == *fref.ty || is<TVar>(ap->args()[1]) || is<TVar>(fref.ty));
          } else {
            return is<TVar>(ap->args()[0]);
          }
        } else {
          return false;
        }
      } else {
        return is<TVar>(ap->fn());
      }
    } else {
      return is<TVar>(fref.ref);
    }
  } else {
    return false;
  }
}

void DBFileRefLoader::explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs) {
}

struct DBFLUnqualify : public switchExprTyFn {
  const ConstraintPtr& constraint;

  DBFLUnqualify(const ConstraintPtr& constraint) : constraint(constraint) {
    bool valid = false;
    FRefLoadable fref;
    if (dec(constraint, &fref)) {
      if (const TApp* ap = is<TApp>(fref.ref)) {
        if (const Prim* f = is<Prim>(ap->fn())) {
          if (f->name() == "fileref" && ap->args().size() == 2) {
            if (is<TLong>(ap->args()[0])) {
              valid = true;
            }
          }
        }
      }
    }

    if (!valid) {
      throw std::runtime_error("Internal error, constraint not valid for value loading: " + show(constraint));
    }
  }

  QualTypePtr withTy(const QualTypePtr& qt) const {
    return removeConstraint(this->constraint, qt);
  }

  ExprPtr with(const Var* vn) const {
    if (vn->value() == REF_LOAD) {
      throw annotated_error(*vn, "load to local function NYI");
    } else {
      return wrapWithTy(vn->type(), new Var(vn->value(), vn->la()));
    }
  }

  ExprPtr with(const App* ap) const {
    if (const Var* fn = is<Var>(stripAssumpHead(ap->fn()))) {
      if (fn->value() == REF_LOAD) {
        return wrapWithTy(ap->type(), new App(wrapWithTy(fn->type(), new Var(".dbload", ap->la())), list(switchOf(ap->args()[0], *this)), ap->la()));
      }
    }

    return wrapWithTy(ap->type(), new App(switchOf(ap->fn(), *this), switchOf(ap->args(), *this), ap->la()));
  }
};

ExprPtr DBFileRefLoader::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, DBFLUnqualify(cst));
}

PolyTypePtr DBFileRefLoader::lookup(const std::string& vn) const {
  if (vn == REF_LOAD) {
    return polytype(2, qualtype(list(ConstraintPtr(new Constraint(DBFileRefLoader::constraintName(), list(tgen(0), tgen(1))))), functy(list(tgen(0)), tgen(1))));
  } else {
    return PolyTypePtr();
  }
}

SymSet DBFileRefLoader::bindings() const {
  SymSet r;
  r.insert(REF_LOAD);
  return r;
}

FunDeps DBFileRefLoader::dependencies(const ConstraintPtr&) const {
  // reference and load types are 1:1
  FunDeps result;
  result.push_back(FunDep(list(0), 1));
  result.push_back(FunDep(list(1), 0));
  return result;
}

}

