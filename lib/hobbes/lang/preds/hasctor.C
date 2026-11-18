
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>

#include <hobbes/lang/preds/hasctor/variant.H>

#include <memory>
#include <set>
#include <unordered_map>

namespace hobbes {

#define MSELECT_CTOR_FN "maybeFromCtor"

/////////////////////////////////////////////////////
// generic field verification and constraint removal
/////////////////////////////////////////////////////
CtorVerifier::CtorVerifier() : variants(std::make_shared<HCVariantEliminator>()) {
  addEliminator(this->variants);
}

std::string CtorVerifier::constraintName() {
  return "HasCtor";
}

void CtorVerifier::addEliminator(const std::shared_ptr<HCEliminator>& hce) {
  this->eliminators.push_back(hce);
}

HCEliminator* CtorVerifier::findEliminator(const TEnvPtr& tenv, const HasCtor& hc, Definitions* ds) const {
  for (const auto& hce : this->eliminators) {
    if (hce->satisfiable(tenv, hc, ds)) {
      return hce.get();
    }
  }
  return nullptr;
}

bool CtorVerifier::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* s, Definitions* ds) {
  HasCtor hc;
  if (dec(cst, &hc)) {
    if (HCEliminator* hce = findEliminator(tenv, hc, ds)) {
      return hce->refine(tenv, hc, s, ds);
    }
  }
  return false;
}

bool CtorVerifier::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  HasCtor hc;
  if (dec(cst, &hc)) {
    if (HCEliminator* hce = findEliminator(tenv, hc, ds)) {
      return hce->satisfied(tenv, hc, ds);
    }
  }
  return false;
}

bool CtorVerifier::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  HasCtor hc;
  return dec(cst, &hc) && (findEliminator(tenv, hc, ds) != nullptr);
}

void CtorVerifier::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

// rewrite 'maybeFromCtor' at the constraints it stands for to the case analyses that they describe
//
// For several constraints, this gives what rewriting at each of them in turn
// does (and taking each out of every type before the next), which is to say:
//  * the first of them that a use states decides it, and
//  * where an applied use is rewritten, its argument is not looked into for
//    the constraint that decided it, as it never has been
struct RewriteMSelect : public switchExprTyFn {
  struct Ctor {
    size_t      order; // this constraint's place in the sequence
    HasCtor     hc;
    std::string ctor;
  };
  using Ctors = std::unordered_map<ConstraintPtr, Ctor, ConstraintHash, ConstraintEq>;
  using CtorsPtr = std::shared_ptr<Ctors>;

  CtorsPtr         ctors;
  std::set<size_t> decided; // the constraints (by place) that decided the uses that this is inside the arguments of

  RewriteMSelect() : ctors(std::make_shared<Ctors>()) {
  }
  RewriteMSelect(const RewriteMSelect& outer, const Ctor& by) : ctors(outer.ctors), decided(outer.decided) {
    this->decided.insert(by.order);
  }

  void add(const HasCtor& hc, const ConstraintPtr& cst) {
    const TString* fn = is<TString>(hc.ctorlbl);
    if (fn == nullptr) { throw std::runtime_error("Internal error, invalid hasctor constraint for unqualification"); }

    // (a repeated constraint keeps its first place)
    this->ctors->insert(std::make_pair(cst, Ctor{this->ctors->size(), hc, fn->value()}));
  }

  // the first of these constraints that a type states, if any
  //
  // (an applied use is decided from the type of the application, not of 'maybeFromCtor' itself, as it always has
  //  been: type inference gives an application the constraints of its head, and an annotation those of what it
  //  annotates, so the constraint that a use stands for is among those stated where it is applied)
  const Ctor* ctorFor(const QualTypePtr& t) const {
    const Ctor* r = nullptr;
    for (const auto& c : t->constraints()) {
      auto ct = this->ctors->find(c);
      if (ct != this->ctors->end() && (r == nullptr || ct->second.order < r->order) && this->decided.count(ct->second.order) == 0) {
        r = &ct->second;
      }
    }
    return r;
  }

  ExprPtr with(const App* ap) const override {
    if (ap->args().size() == 1) {
      if (const Var* f = is<Var>(stripAssumpHead(ap->fn()))) {
        if (f->value() == MSELECT_CTOR_FN) {
          if (const Ctor* ct = ctorFor(ap->type())) {
            RewriteMSelect inArg(*this, *ct);
            const ExprPtr& arg = ap->args()[0];
            ExprPtr r(new Case(inArg.decided.size() == this->ctors->size() ? arg : switchOf(arg, inArg), list(Case::Binding(ct->ctor, ".p", justE(var(".p", ct->hc.ctorty, ap->la()), ap->la()))), nothingE(ct->hc.ctorty, ap->la()), ap->la()));
            r->type(qualtype(maybety(ct->hc.ctorty)));
            return r;
          }
        }
      }
    }
    return wrapWithTy(ap->type(), new App(switchOf(ap->fn(), *this), switchOf(ap->args(), *this), ap->la()));
  }

  ExprPtr with(const Var* v) const override {
    if (v->value() == MSELECT_CTOR_FN) {
      if (const Ctor* ct = ctorFor(v->type())) {
        std::string vn = freshName();

        ExprPtr b(new Case(var(vn, ct->hc.variant, v->la()), list(Case::Binding(ct->ctor, ".p", justE(var(".p", ct->hc.ctorty, v->la()), v->la()))), nothingE(ct->hc.ctorty, v->la()), v->la()));
        b->type(qualtype(maybety(ct->hc.ctorty)));

        ExprPtr rv = fn(vn, b, v->la());
        rv->type(qualtype(functy(ct->hc.variant, maybety(ct->hc.ctorty))));
        return rv;
      }
    }
    return wrapWithTy(v->type(), new Var(v->value(), v->la()));
  }
};


ExprPtr CtorVerifier::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  HasCtor hc;
  if (dec(cst, &hc)) {
    if (HCEliminator* hce = findEliminator(tenv, hc, ds)) {
      RewriteMSelect rewrite;
      rewrite.add(hc, cst);
      return hce->unqualify(tenv, cst, hc, switchOf(e, rewrite), ds);
    }
  }
  throw annotated_error(*e, "Cannot unqualify constraint: " + show(cst));
}

bool CtorVerifier::batchable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  HasCtor hc;
  return dec(cst, &hc) && findEliminator(tenv, hc, ds) == this->variants.get();
}

ExprPtr CtorVerifier::unqualifyBatch(const TEnvPtr& tenv, const Constraints& csts, const ExprPtr& e, Definitions* ds) const {
  RewriteMSelect rewrite;
  for (const auto& cst : csts) {
    HasCtor hc;
    if (!dec(cst, &hc) || findEliminator(tenv, hc, ds) != this->variants.get()) {
      throw annotated_error(*e, "Cannot unqualify constraint: " + show(cst));
    }
    rewrite.add(hc, cst);
  }
  return this->variants->unqualify(tenv, csts, switchOf(e, rewrite), ds);
}

PolyTypePtr CtorVerifier::lookup(const std::string& vn) const {
  if (vn == MSELECT_CTOR_FN) {
    // select either the given constructor payload from a variant, or nothing
    // :: (|c/p|::v) => v -> (()+p)
    return polytype(3, qualtype(list(std::make_shared<Constraint>(CtorVerifier::constraintName(), list(tgen(0), tgen(1), tgen(2)))), functy(list(tgen(0)), maybety(tgen(2)))));
  } else {
    return PolyTypePtr();
  }
}

SymSet CtorVerifier::bindings() const {
  SymSet r;
  r.insert(MSELECT_CTOR_FN);
  return r;
}

FunDeps CtorVerifier::dependencies(const ConstraintPtr&) const {
  FunDeps result;
  result.push_back(FunDep(list(0, 1), 2));
  return result;
}

bool CtorVerifier::dec(const ConstraintPtr& c, HasCtor* hc) {
  if (c->name() == CtorVerifier::constraintName() && c->arguments().size() == 3) {
    hc->variant = c->arguments()[0];
    hc->ctorlbl = c->arguments()[1];
    hc->ctorty  = c->arguments()[2];
    return true;
  }
  return false;
}

}

