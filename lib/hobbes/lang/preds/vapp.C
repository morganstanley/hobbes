
#include <hobbes/lang/preds/vapp.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>

namespace hobbes {

MonoTypePtr recordDtorType(const Variant& vty, const MonoTypePtr& result) {
  Record::Members fs;
  for (const auto& vm : vty.members()) {
    fs.push_back(Record::Member(vm.selector, closty(vm.type, result)));
  }
  return Record::make(fs);
}

void decClosure(const MonoTypePtr& cty, MonoTypePtr* arg, MonoTypePtr* result) {
  if (const Exists* e = is<Exists>(cty)) {
    if (const Record* r = is<Record>(e->absType())) {
      if (r->members().size() == 2) {
        if (const Func* f = is<Func>(r->members()[0].type)) {
          if (f->parameters().size() == 2) {
            *arg    = f->parameters()[1];
            *result = f->result();
            return;
          }
        }
      }
    }
  }
  throw std::runtime_error("Can't decode as variant applicator, field not a closure type: " + show(cty));
}

MonoTypePtr variantTypeFromDtor(const Record& rty) {
  Variant::Members vms;
  uint32_t id = 0;
  for (const auto& rm : rty.members()) {
    MonoTypePtr arg, result;
    decClosure(rm.type, &arg, &result);
    vms.push_back(Variant::Member(rm.field, arg, id++));
  }
  return Variant::make(vms);
}

MonoTypePtr dtorResultFromDtor(const Record& rty) {
  if (rty.members().size() == 0) {
    throw std::runtime_error("Internal error, impossible variant applicator type: ()");
  } else {
    MonoTypePtr arg, result;
    decClosure(rty.members()[0].type, &arg, &result);
    return result;
  }
}

bool vappVarEquiv(const Variant& lhs, const Variant& rhs) {
  if (lhs.members().size() != rhs.members().size()) {
    return false;
  } else {
    for (size_t i = 0; i < lhs.members().size(); ++i) {
      const auto& lhsc = lhs.members()[i];
      const auto& rhsc = rhs.members()[i];

      if (lhsc.selector != rhsc.selector || !(*lhsc.type == *rhsc.type)) {
        return false;
      }
    }
    return true;
  }
}
bool vappVarEquiv(const Variant& lhs, const MonoTypePtr& rhs) {
  if (const Variant* trhs = is<Variant>(rhs)) {
    return vappVarEquiv(lhs, *trhs);
  } else {
    return false;
  }
}

struct VariantAppD {
  MonoTypePtr variantType;
  MonoTypePtr recordDtorType;
  MonoTypePtr resultType;
};

static bool dec(const ConstraintPtr& c, VariantAppD* va) {
  if (c->name() == VariantAppP::constraintName() && c->arguments().size() == 3) {
    va->variantType    = c->arguments()[0];
    va->recordDtorType = c->arguments()[1];
    va->resultType     = c->arguments()[2];
    return true;
  }
  return false;
}

#define REF_VAR_APP "variantApp"

std::string VariantAppP::constraintName() {
  return "VariantApp";
}

bool VariantAppP::refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
  VariantAppD va;
  if (dec(cst, &va)) {
    if (const Variant* vty = is<Variant>(va.variantType)) {
      auto s = u->size();
      mgu(va.recordDtorType, recordDtorType(*vty, va.resultType), u);
      return s != u->size();
    } else if (const Record* rty = is<Record>(va.recordDtorType)) {
      auto s = u->size();
      mgu(va.resultType, dtorResultFromDtor(*rty), u);
      return s != u->size();
    }
  }
  return false;
}

bool VariantAppP::satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
  VariantAppD va;
  if (dec(cst, &va)) {
    if (!(hasFreeVariables(va.variantType) || hasFreeVariables(va.recordDtorType) || hasFreeVariables(va.resultType))) {
      if (const Variant* vty = is<Variant>(va.variantType)) {
        if (const Record* rty = is<Record>(va.recordDtorType)) {
          return vappVarEquiv(*vty, variantTypeFromDtor(*rty)) && *va.recordDtorType == *recordDtorType(*vty, va.resultType);
        }
      }
    }
  }
  return false;
}

bool VariantAppP::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  VariantAppD va;
  if (dec(cst, &va)) {
    if (is<Variant>(va.variantType)) {
      if (is<Record>(va.recordDtorType)) {
        return hasFreeVariables(va.variantType) || hasFreeVariables(va.recordDtorType) || hasFreeVariables(va.resultType) || satisfied(tenv, cst, ds);
      } else {
        return is<TVar>(va.recordDtorType);
      }
    } else {
      return is<TVar>(va.variantType);
    }
  }
  return false;
}

void VariantAppP::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

PolyTypePtr VariantAppP::lookup(const std::string& vn) const {
  if (vn == REF_VAR_APP) {
    // variantApp :: (VariantApp v f r) => (v,f) -> r
    return polytype(3, qualtype(list(ConstraintPtr(new Constraint(VariantAppP::constraintName(), list(tgen(0), tgen(1), tgen(2))))), functy(list(tgen(0), tgen(1)), tgen(2))));
  } else {
    return PolyTypePtr();
  }
}

SymSet VariantAppP::bindings() const {
  SymSet r;
  r.insert(REF_VAR_APP);
  return r;
}

FunDeps VariantAppP::dependencies(const ConstraintPtr&) const {
  FunDeps result;
  result.push_back(FunDep(list(0, 2), 1));
  result.push_back(FunDep(list(1), 2));
  return result;
}

// resolve satisfied variant deconstruction predicates
struct VAUnqualify : public switchExprTyFn {
  const ConstraintPtr& constraint;
  VAUnqualify(const ConstraintPtr& constraint) : constraint(constraint) { }

  QualTypePtr withTy(const QualTypePtr& qt) const {
    return removeConstraint(this->constraint, qt);
  }

  ExprPtr with(const Var* v) const {
    if (hasConstraint(this->constraint, v->type())) {
      if (v->value() == REF_VAR_APP) {
        // rewrite this variable to become a function that does the promised case analysis and dispatch to closure calls
        auto uvty = this->constraint->arguments()[0];
        auto urty = this->constraint->arguments()[1];

        if (const Variant* vty = is<Variant>(uvty)) {
          if (is<Record>(urty)) {
            Case::Bindings cs;
            for (const auto& vm : vty->members()) {
              cs.push_back(Case::Binding(vm.selector, ".x", closcall(proj(var(".f", urty, v->la()), vm.selector, v->la()), list(var(".x", vm.type, v->la())), v->la())));
            }
            ExprPtr c(new Case(var(".v", uvty, v->la()), cs, v->la()));
            c->type(qualtype(this->constraint->arguments()[2]));
            return wrapWithTy(v->type(), new Fn(str::strings(".v", ".f"), c, v->la()));
          }
        }
      }
    }
    return wrapWithTy(v->type(), v->clone());
  }
};

ExprPtr VariantAppP::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, VAUnqualify(cst));
}

}

