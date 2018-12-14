
#include <hobbes/lang/preds/deconstruct.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

std::string DeconstructP::constraintName() {
  return "Deconstruct";
}

struct destructType : public switchType<bool> {
  MonoTypePtr* r = 0;
  destructType(MonoTypePtr* r) : r(r) { }

  bool with(const Prim*) const {
    return false;
  }

  bool with(const OpaquePtr*) const {
    return false;
  }

  bool with(const TVar*) const {
    return false;
  }

  bool with(const TGen*) const {
    return false;
  }

  bool with(const TAbs*) const {
    return false;
  }

  bool with(const TApp*) const {
    return false;
  }

  bool with(const FixedArray* v) const {
    *this->r = tuplety(list(
      tstring("fixedarray"),
      v->type(),
      v->length()
    ));
    return false;
  }

  bool with(const Array* v) const {
    *this->r = tuplety(list(
      tstring("array"),
      v->type()
    ));
    return true;
  }

  bool with(const Variant*) const {
    return false;
  }

  bool with(const Record*) const {
    return false;
  }

  bool with(const Func* v) const {
    auto ps = v->parameters();

    *this->r = tuplety(list(
      tstring("->"),
      (ps.size() == 1) ? ps[0] : v->argument(), // a little bit of a hack -- we need to deal with tuple argls better ...
      v->result()
    ));
    return true;
  }

  bool with(const Exists* v) const {
    // again a bit of a hack
    //   exists E.((E,a)->b)*E = (closure a b)
    if (const Record* crec = is<Record>(v->absType())) {
      if (crec->members().size() == 2 && crec->isTuple()) {
        if (const Func* cfn = is<Func>(crec->members()[0].type)) {
          MonoTypes ps = cfn->parameters();
          if (ps.size() >= 2 && *ps[0] == *tvar(v->absTypeName())) {
            *this->r = tuplety(list(
              tstring("closure"),
              (ps.size() == 2) ? ps[1] : tuplety(drop(ps, 1)),
              cfn->result()
            ));
            return true;
          }
        }
      }
    }
    return false;
  }

  bool with(const Recursive* v) const {
    *this->r = tuplety(list(
      tstring("recursive"),
      MonoTypePtr(Recursive::make(v->recTypeName(), v->recType()))
    ));
    return true;
  }
  
  bool with(const TString*) const {
    return false;
  }

  bool with(const TLong*) const {
    return false;
  }
 
  bool with(const TExpr*) const {
    return false;
  }
};

static bool asDestruct(const MonoTypePtr& t, MonoTypePtr* dty) {
  return switchOf(t, destructType(dty));
}

static bool asRestruct(const MonoTypePtr& t, MonoTypePtr* rty) {
  const Record* r = is<Record>(t);
  if (!r || !r->isTuple()) return false;

  const Record::Members& ms = r->members();
  if (ms.size() == 0) return false;

  const TString* ctor = is<TString>(ms[0].type);
  if (!ctor) return false;

  if (ctor->value() == "->" && ms.size() == 3) {
    *rty = MonoTypePtr(Func::make(tuplety(list(ms[1].type)), ms[2].type));
    return true;
  } else if (ctor->value() == "array" && ms.size() == 2) {
    *rty = MonoTypePtr(Array::make(ms[1].type));
    return true;
  } else if (ctor->value() == "fixedarray" && ms.size() == 3) {
    *rty = MonoTypePtr(FixedArray::make(ms[1].type, ms[2].type));
    return true;
  } else if (ctor->value() == "recursive" && ms.size() == 2) {
    *rty = ms[1].type;
    return true;
  } else if (ctor->value() == "closure" && ms.size() == 3) {
    *rty = MonoTypePtr(Exists::make("E", tuplety(list(functy(list(tvar("E"), ms[1].type), ms[2].type), tvar("E")))));
    return true;
  }

  return false;
}

bool DeconstructP::refine(const TEnvPtr&, const ConstraintPtr& c, MonoTypeUnifier* u, Definitions*) {
  if (c->name() != DeconstructP::constraintName() || c->arguments().size() != 2) {
    return false;
  }

  MonoTypePtr dty;
  if (asDestruct(c->arguments()[0], &dty)) {
    size_t z = u->size();
    mgu(dty, c->arguments()[1], u);
    return z != u->size();
  }

  MonoTypePtr rty;
  if (asRestruct(c->arguments()[1], &rty)) {
    size_t z = u->size();
    mgu(rty, c->arguments()[0], u);
    return z != u->size();
  }

  return false;
}

bool DeconstructP::satisfied(const TEnvPtr&, const ConstraintPtr& c, Definitions*) const {
  if (c->name() != DeconstructP::constraintName() || c->arguments().size() != 2) {
    return false;
  }

  if (is<TVar>(c->arguments()[0])) {
    return false;
  } else {
    MonoTypePtr dty, rty;

    return asDestruct(c->arguments()[0], &dty) &&
           asRestruct(c->arguments()[1], &rty) &&
           *c->arguments()[0] == *rty &&
           *c->arguments()[1] == *dty;
  }
}

bool DeconstructP::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) const {
  if (c->name() != DeconstructP::constraintName() || c->arguments().size() != 2) {
    return false;
  }

  if (is<TVar>(c->arguments()[0]) || is<TVar>(c->arguments()[1])) {
    return true;
  } else {
    return satisfied(tenv, c, ds);
  }
}

void DeconstructP::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
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

ExprPtr DeconstructP::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, StripCst(cst));
}

PolyTypePtr DeconstructP::lookup(const std::string&) const {
  return PolyTypePtr();
}

SymSet DeconstructP::bindings() const {
  return SymSet();
}

FunDeps DeconstructP::dependencies(const ConstraintPtr&) const {
  return list(FunDep(list(0), 1), FunDep(list(1), 0));
}

}

