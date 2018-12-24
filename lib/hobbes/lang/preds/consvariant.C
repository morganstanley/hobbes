
#include <hobbes/lang/preds/consvariant.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>

namespace hobbes {

Variant::Members normalizeCtorIDs(const Variant::Members& ms) {
  Variant::Members r;
  uint32_t i = 0;
  for (const auto& m : ms) {
    if (m.selector.size() < 2 || (m.selector[0] != '.' || m.selector[1] != 'p')) {
      r.push_back(Variant::Member(m.selector, m.type, i++));
    }
  }
  return r;
}

MonoTypePtr normalizeCtorIDs(const MonoTypePtr& t) {
  if (const Variant* v = is<Variant>(t)) {
    return MonoTypePtr(Variant::make(normalizeCtorIDs(v->members())));
  } else {
    return t;
  }
}

struct ConsVariant {
  bool        forward;
  MonoTypePtr variantType;
  MonoTypePtr headCtorName;
  MonoTypePtr headType;
  MonoTypePtr tailType;
};

static bool dec(const ConstraintPtr& c, ConsVariant* cv) {
  if (c->name() == VariantDeconstructor::constraintName() && c->arguments().size() == 5) {
    if (const TLong* fwd = is<TLong>(c->arguments()[0])) {
      cv->forward      = fwd->value() != 0;
      cv->variantType  = c->arguments()[1];
      cv->headCtorName = c->arguments()[2];
      cv->headType     = c->arguments()[3];
      cv->tailType     = c->arguments()[4];
      return true;
    }
  }
  return false;
}

#define REF_VAR_LABEL "variantHeadLabel"
#define REF_VAR_SPLIT "variantSplit"
#define REF_VAR_INJH  "variantInjectHead"
#define REF_VAR_LIFTT "variantLiftTail"

std::string VariantDeconstructor::constraintName() {
  return "ConsVariant";
}

bool VariantDeconstructor::refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
  ConsVariant cv;
  if (dec(cst, &cv)) {
    if (const Variant* vty = is<Variant>(cv.variantType)) {
      if (vty->members().size() > 0) {
        size_t uc = u->size();
        mgu(tstring(vty->headMember().selector), cv.headCtorName, u);
        mgu(vty->headMember().type,              cv.headType, u);

        if (cv.forward) {
          mgu(vty->tailType(), cv.tailType, u);
        } else {
          mgu(normalizeCtorIDs(vty->tailType()), normalizeCtorIDs(cv.tailType), u);
        }
        return uc != u->size();
      }
    } else if (*cv.tailType == *primty("void")) {
      if (const TString* lbl = is<TString>(cv.headCtorName)) {
        mgu(cv.variantType, MonoTypePtr(Variant::make(lbl->value(), cv.headType, Variant::Members())), u);
      }
    } else if (const Variant* tvty = is<Variant>(cv.tailType)) {
      if (const TString* lbl = is<TString>(cv.headCtorName)) {
        mgu(cv.variantType, MonoTypePtr(Variant::make(lbl->value(), cv.headType, tvty->members())), u);
      }
    }
  }
  return false;
}

bool VariantDeconstructor::satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
  ConsVariant cv;
  if (dec(cst, &cv)) {
    if (const Variant* vty = is<Variant>(cv.variantType)) {
      return (vty->members().size() > 0)                                &&
             (*cv.headCtorName == *tstring(vty->headMember().selector)) &&
             (*cv.headType == *vty->headMember().type)                  &&
             (*normalizeCtorIDs(cv.tailType) == *normalizeCtorIDs(vty->tailType()));
    } else {
      return false;
    }
  } else {
    return false;
  }
}

bool VariantDeconstructor::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  ConsVariant cv;
  if (dec(cst, &cv)) {
    return satisfied(tenv, cst, ds) ||
           is<TVar>(cv.variantType) ||
           (is<Variant>(cv.variantType) && (is<TVar>(cv.headCtorName) || is<TVar>(cv.headType) || is<TVar>(cv.tailType)));
  } else {
    return false;
  }
}

void VariantDeconstructor::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

PolyTypePtr VariantDeconstructor::lookup(const std::string& vn) const {
  if (vn == REF_VAR_LABEL) {
    // variantHeadLabel :: (v=|a+vt|) => v -> [char]
    return polytype(4, qualtype(list(ConstraintPtr(new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), tgen(0), tgen(1), tgen(2), tgen(3))))), functy(list(tgen(0)), arrayty(prim<char>()))));
  } else if (vn == REF_VAR_SPLIT) {
    // variantSplit :: (v=|a+vt|) => (v, closure a c, closure vt c) -> c
    return
      polytype(5,
        qualtype(
          list(ConstraintPtr(new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), tgen(0), tgen(1), tgen(2), tgen(3))))),
          functy(list(tgen(0), closty(list(tgen(2)), tgen(4)), closty(list(tgen(3)), tgen(4))), tgen(4))
        )
      );
  } else if (vn == REF_VAR_INJH) {
    // variantInjectHead :: (v=|a+vt|) => a -> v
    return
      polytype(4,
        qualtype(
          list(ConstraintPtr(new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), tgen(0), tgen(1), tgen(2), tgen(3))))),
          functy(list(tgen(2)), tgen(0))
        )
      );
  } else if (vn == REF_VAR_LIFTT) {
    // variantLiftTail :: (v=|a+vt|) => vt -> v
    return
      polytype(4,
        qualtype(
          list(ConstraintPtr(new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), tgen(0), tgen(1), tgen(2), tgen(3))))),
          functy(list(tgen(3)), tgen(0))
        )
      );
  } else {
    return PolyTypePtr();
  }
}

SymSet VariantDeconstructor::bindings() const {
  SymSet r;
  r.insert(REF_VAR_LABEL);
  r.insert(REF_VAR_SPLIT);
  r.insert(REF_VAR_INJH);
  r.insert(REF_VAR_LIFTT);
  return r;
}

FunDeps VariantDeconstructor::dependencies(const ConstraintPtr&) const {
  // ordering determined by constraints map:
  //   (fwd, x->variantType(), x->headCtorName(), x->headType(), x->tailType())
  FunDeps result;
  result.push_back(FunDep(list(1), 2));
  result.push_back(FunDep(list(1), 3));
  result.push_back(FunDep(list(1), 4));

  // the fundep that says that a variant is uniquely determined by its head label, head type and tail type
  // is not sound when hidden prefix fields may be different
  //result.push_back(FunDep(list(2, 3, 4), 1));
  return result;
}

// resolve satisfied variant deconstruction predicates
struct VDUnqualify : public switchExprTyFn {
  const ConstraintPtr& constraint;
  VDUnqualify(const ConstraintPtr& constraint) : constraint(constraint) { }

  QualTypePtr withTy(const QualTypePtr& qt) const {
    return removeConstraint(this->constraint, qt);
  }

  ExprPtr with(const Var* v) const {
    if (hasConstraint(this->constraint, v->type())) {
      // replace safe functions with 'unsafe' ones
      if (v->value() == REF_VAR_LABEL) {
        return wrapWithTy(v->type(), new Var(".variantHeadLabel", v->la()));
      } else if (v->value() == REF_VAR_SPLIT) {
        return wrapWithTy(v->type(), new Var(".variantSplit", v->la()));
      } else if (v->value() == REF_VAR_INJH) {
        return wrapWithTy(v->type(), new Var(".variantInjectHead", v->la()));
      } else if (v->value() == REF_VAR_LIFTT) {
        return wrapWithTy(v->type(), new Var(".cast", v->la()));
      } else {
        return wrapWithTy(v->type(), v->clone());
      }
    } else {
      return wrapWithTy(v->type(), v->clone());
    }
  }
};

ExprPtr VariantDeconstructor::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, VDUnqualify(cst));
}

}

