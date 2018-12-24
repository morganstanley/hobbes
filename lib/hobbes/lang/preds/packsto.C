
#include <hobbes/lang/preds/packsto.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

// temporarily restrict supported existential types to the immediate use-case for closures
// this lets us avoid awkward problems (for now) like how to skip past an existentially-quantified field in a record
bool isSafeExistentialType(const Exists* e) {
  if (const Record* r = is<Record>(e->absType())) {
    const Record::Members& ms = r->members();
    if (ms.size() != 2) return false;

    if (const TVar* ev = is<TVar>(ms[1].type)) {
      if (ev->name() != e->absTypeName()) {
        return false;
      }
    } else {
      return false;
    }

    if (const Func* f = is<Func>(ms[0].type)) {
      if (const TVar* rt = is<TVar>(f->result())) {
        if (rt->name() == e->absTypeName()) {
          return false;
        }
      }
    } else {
      return false;
    }

    return true;
  } else {
    return false;
  }
}

struct PacksTo {
  MonoTypePtr underlyingType;
  MonoTypePtr abstractType;
};

static bool dec(const ConstraintPtr& c, PacksTo* pt) {
  if (c->name() == Existentials::constraintName() && c->arguments().size() == 2) {
    pt->underlyingType = c->arguments()[0];
    pt->abstractType   = c->arguments()[1];
    return true;
  }
  
  return false;
}
static void upd(const ConstraintPtr& c, const PacksTo& pt) {
  MonoTypes& args = const_cast<MonoTypes&>(c->arguments());
  if (args.size() == 2) {
    args[0] = pt.underlyingType;
    args[1] = pt.abstractType;
  }
}

/****
 * enforce/translate type judgements and computations with existential types
 ****/
std::string Existentials::constraintName() {
  return "packsto";
}

bool Existentials::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
  PacksTo pt;
  if (dec(cst, &pt)) {
    if (const Exists* e = is<Exists>(pt.abstractType)) {
      MonoTypeUnifier lu(tenv);
      scoped_unification_suppression slus(&lu, e->absTypeName());
      mgu(u->substitute(pt.underlyingType), e->absType(), &lu);

      MonoTypeSubst lus = lu.substitution();
      for (MonoTypeSubst::const_iterator si = lus.begin(); si != lus.end(); ++si) {
        u->bind(si->first, si->second);
      }
      pt.abstractType = substitute(lus, pt.abstractType);
      upd(cst, pt);
      return lus.size() > 0;
    }
  }
  return false;
}

bool Existentials::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const {
  PacksTo pt;
  if (dec(cst, &pt)) {
    if (const Exists* e = is<Exists>(pt.abstractType)) {
      if (!isSafeExistentialType(e)) {
        return false;
      }

      if (!unifiable(tenv, pt.underlyingType, e->absType())) {
        return false;
      }

      NameSet utns = tvarNames(pt.underlyingType);
      NameSet atns = tvarNames(e->absType());

      return utns.size() == 0 && atns.size() == 1 && (e->absTypeName() == *atns.begin());
    } else {
      return *pt.underlyingType == *pt.abstractType;
    }
  } else {
    return false;
  }
}

bool Existentials::satisfiable(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
  PacksTo pt;
  if (dec(cst, &pt)) {
    if (is<TVar>(pt.abstractType)) {
      return true;
    } else if (const Exists* e = is<Exists>(pt.abstractType)) {
      if (!isSafeExistentialType(e)) {
        return false;
      } else {
        return true; // too liberal?  changed from 'unifiable' to work with streams
      }
    } else {
      return true;
    }
  } else {
    return false;
  }
}

void Existentials::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

struct UnqualifySafePackF : public switchExprTyFn {
  const ConstraintPtr& constraint;
  UnqualifySafePackF(const ConstraintPtr& cst) : constraint(cst) {
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }
};

struct StripTransparentPackF : public switchExprTyFn {
  const ConstraintPtr& constraint;
  StripTransparentPackF(const ConstraintPtr& cst) : constraint(cst) {
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }

  ExprPtr with(const Pack* p) const {
    if (hasConstraint(this->constraint, p->type())) {
      return switchOf(p->expr(), *this);
    } else {
      return wrapWithTy(p->type(), new Pack(switchOf(p->expr(), *this), p->la()));
    }
  }
};

ExprPtr Existentials::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  // if we're eliminating a constraint that indicates no information is hidden, then get rid of the superfluous packs
  // else, just remove the constraint from terms and let final compilation determine how to do the packing
  PacksTo pt;
  if (dec(cst, &pt)) {
    if (is<Exists>(pt.abstractType)) {
      return switchOf(e, UnqualifySafePackF(cst));
    } else {
      return switchOf(e, StripTransparentPackF(cst));
    }
  } else {
    // ???
    throw annotated_error(*e, "Internal error in existential unqualification, unexpected constraint: " + show(cst));
  }
}

PolyTypePtr Existentials::lookup(const std::string&) const {
  return PolyTypePtr(); // this won't be necessary
}

SymSet Existentials::bindings() const {
  return SymSet(); // no overloaded symbols are defined here
}

FunDeps Existentials::dependencies(const ConstraintPtr&) const {
  return FunDeps();
}

}

