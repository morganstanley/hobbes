
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>

#include <hobbes/lang/preds/hasfield/record.H>
#include <hobbes/lang/preds/hasfield/slookup.H>
#include <hobbes/lang/preds/hasfield/lookup.H>
#include <hobbes/lang/preds/hasfield/tenvlookup.H>

namespace hobbes {

#define FIELD_VALUE_FN "fieldValue"

/////////////////////////////////////////////////////
// generic field verification and constraint removal
/////////////////////////////////////////////////////
Constraint*   HasField::newConstraint(Direction d, const MonoTypePtr& t0, const MonoTypePtr& t1, const MonoTypePtr& t2, const ExprPtr& e) { return new Constraint(FieldVerifier::constraintName(), list(tlong(d == HasField::Read ? 0 : 1), t0, t1, t2, texpr(e))); }
Constraint*   HasField::newConstraint(Direction d, const MonoTypePtr& t0, const MonoTypePtr& t1, const MonoTypePtr& t2)                   { return new Constraint(FieldVerifier::constraintName(), list(tlong(d == HasField::Read ? 0 : 1), t0, t1, t2, primty("unit"))); }
ConstraintPtr HasField::constraint   (Direction d, const MonoTypePtr& t0, const MonoTypePtr& t1, const MonoTypePtr& t2, const ExprPtr& e) { return ConstraintPtr(newConstraint(d,t0,t1,t2,e)); }
ConstraintPtr HasField::constraint   (Direction d, const MonoTypePtr& t0, const MonoTypePtr& t1, const MonoTypePtr& t2)                   { return ConstraintPtr(newConstraint(d,t0,t1,t2)); }

bool dec(const ConstraintPtr& c, HasField* hf) {
  if (c->name() == FieldVerifier::constraintName() && c->arguments().size() == 5) {
    if (const TLong* d = is<TLong>(c->arguments()[0])) {
      hf->direction = d->value() == 0 ? HasField::Read : HasField::Write;
      hf->recordType = c->arguments()[1];
      hf->fieldName = c->arguments()[2];
      hf->fieldType = c->arguments()[3];
      if (const TExpr* e = is<TExpr>(c->arguments()[4])) {
        hf->recordExpr = e->expr();
      }
      return true;
    }
  }
  return false;
}

void upd(const ConstraintPtr& c, const HasField& hf) {
  if (c->name() == FieldVerifier::constraintName() && c->arguments().size() == 5) {
    MonoTypes& mts = const_cast<MonoTypes&>(c->arguments());
    mts[0] = tlong(hf.direction == HasField::Read ? 0 : 1);
    mts[1] = hf.recordType;
    mts[2] = hf.fieldName;
    mts[3] = hf.fieldType;
    mts[4] = hf.recordExpr ? texpr(hf.recordExpr) : primty("unit");
  }
}

std::string FieldVerifier::constraintName() {
  return "HasField";
}

FieldVerifier::FieldVerifier() {
  this->eliminators.push_back(new HFRecordEliminator());
  this->eliminators.push_back(new HFSLookupEliminator());
  this->eliminators.push_back(new HFLookupEliminator());
  this->eliminators.push_back(new HFTEnvLookupEliminator());
}

void FieldVerifier::addEliminator(HFEliminator* hfe) {
  this->eliminators.push_back(hfe);

  // a hacky way to make sure that the TEnv eliminator is the last one considered, since it's kind of a catch-all
  std::swap(this->eliminators[this->eliminators.size() - 1], this->eliminators[this->eliminators.size() - 2]);
}

bool FieldVerifier::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* s, Definitions* ds) {
  HasField hf;
  if (dec(cst, &hf)) {
    for (size_t i = 0; i < this->eliminators.size(); ++i) {
      auto hfe = this->eliminators[i];
      try {
        if (hfe->satisfiable(tenv, hf, ds) && hfe->refine(tenv, hf, s, ds)) {
          return true;
        }
      } catch (std::exception&) { }
    }
  }

  return false;
}

bool FieldVerifier::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  HasField hf;
  if (dec(cst, &hf)) {
    for (auto hfe : this->eliminators) {
      try {
        if (hfe->satisfied(tenv, hf, ds)) {
          return true;
        }
      } catch (std::exception&) { }
    }
  }
  return false;
}

bool FieldVerifier::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  HasField hf;
  if (dec(cst, &hf)) {
    for (auto hfe : this->eliminators) {
      try {
        if (hfe->satisfiable(tenv, hf, ds)) {
          return true;
        }
      } catch (std::exception& ex) {
      }
    }
  }
  return false;
}

void FieldVerifier::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

struct RewriteFnAccess : public switchExprTyFn {
  const HFEliminator&  hfe;
  const TEnvPtr&       tenv;
  Definitions*         ds;
  const ConstraintPtr& cst;
  std::string          fieldName;

  RewriteFnAccess(const HasField& hf, const HFEliminator& hfe, const TEnvPtr& tenv, Definitions* ds, const ConstraintPtr& cst) : hfe(hfe), tenv(tenv), ds(ds), cst(cst) {
    if (const TString* s = is<TString>(hf.fieldName)) {
      this->fieldName = s->value();
    } else if (const TLong* n = is<TLong>(hf.fieldName)) {
      this->fieldName = ".f" + str::from(n->value());
    }
  }

  ExprPtr with(const App* ap) const {
    if (ap->args().size() == 1) {
      if (hasConstraint(this->cst, ap->type())) {
        if (const Var* f = is<Var>(stripAssumpHead(ap->fn()))) {
          if (f->value() == FIELD_VALUE_FN && hasConstraint(this->cst, f->type())) {
            ExprPtr r(new Proj(switchOf(ap->args()[0], *this), this->fieldName, ap->la()));
            r->type(ap->type());
            return this->hfe.unqualify(this->tenv, this->cst, r, this->ds);
          }
        }
      }
    }
    return wrapWithTy(ap->type(), new App(switchOf(ap->fn(), *this), switchOf(ap->args(), *this), ap->la()));
  }

  ExprPtr with(const Var* v) const {
    if (hasConstraint(this->cst, v->type())) {
      if (v->value() == FIELD_VALUE_FN) {
        std::string vn = freshName();
        
        Func* fty = is<Func>(v->type()->monoType());
        MonoTypePtr recordType = fty->parameters()[0];
        ExprPtr p(new Proj(var(vn, recordType, v->la()), this->fieldName, v->la()));
        p->type(qualtype(list(this->cst), fty->result()));

        ExprPtr rv = fn(vn, p, v->la());
        rv->type(v->type());
        return this->hfe.unqualify(this->tenv, this->cst, rv, this->ds);
      }
    }
    return wrapWithTy(v->type(), new Var(v->value(), v->la()));
  }
};

ExprPtr FieldVerifier::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  HasField hf;
  if (dec(cst, &hf)) {
    for (auto hfe : this->eliminators) {
      try {
        if (hfe->satisfied(tenv, hf, ds)) {
          return hfe->unqualify(tenv, cst, switchOf(e, RewriteFnAccess(hf, *hfe, tenv, ds, cst)), ds);
        }
      } catch (std::exception& exn) {
      }
    }
  }
  throw annotated_error(*e, "Cannot unqualify constraint: " + show(cst));
}

PolyTypePtr FieldVerifier::lookup(const std::string& vn) const {
  if (vn == FIELD_VALUE_FN) {
    return polytype(3, qualtype(list(HasField::constraint(HasField::Read, tgen(0), tgen(1), tgen(2))), functy(list(tgen(0)), tgen(2))));
  } else {
    return PolyTypePtr();
  }
}

SymSet FieldVerifier::bindings() const {
  SymSet r;
  r.insert(FIELD_VALUE_FN);
  return r;
}

FunDeps FieldVerifier::dependencies(const ConstraintPtr& c) const {
  // ordering determined by constraints map:
  //    (ty(x->direction() == HasField::Read), x->recordType(), x->fieldName(), x->fieldType())
  HasField hf;
  if (dec(c, &hf)) {
    if (hf.direction == HasField::Read) {
      // we always read out in _one_ way
      // (only writes can be overloaded)
      FunDeps result;
      result.push_back(FunDep(list(0, 1, 2), 3));
      return result;
    }
  }
  return FunDeps();
}

}

