
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>
#include <unordered_map>

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
    auto& mts = const_cast<MonoTypes&>(c->arguments());
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

FieldVerifier::FieldVerifier() : records(std::make_shared<HFRecordEliminator>()) {
  this->eliminators.push_back(this->records);
  this->eliminators.push_back(std::make_shared<HFSLookupEliminator>());
  this->eliminators.push_back(std::make_shared<HFLookupEliminator>());
  this->eliminators.push_back(std::make_shared<HFTEnvLookupEliminator>());
}

void FieldVerifier::addEliminator(const std::shared_ptr<HFEliminator>& hfe) {
  this->eliminators.push_back(hfe);

  // a hacky way to make sure that the TEnv eliminator is the last one considered, since it's kind of a catch-all
  std::swap(this->eliminators[this->eliminators.size() - 1], this->eliminators[this->eliminators.size() - 2]);
}

bool FieldVerifier::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* s, Definitions* ds) {
  HasField hf;
  if (dec(cst, &hf)) {
    for (const auto& hfe : this->eliminators) {
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
    for (const auto& hfe : this->eliminators) {
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
    for (const auto& hfe : this->eliminators) {
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

// rewrite 'fieldValue' at the constraints it stands for to the projections that they describe
// (for several constraints, the first of them that a use states decides it, as it would if each were rewritten in turn)
struct RewriteFnAccess : public switchExprTyFn {
  // constraint -> (its place in the sequence, its field's name)
  using Field  = std::pair<size_t, std::string>;
  using Fields = std::unordered_map<ConstraintPtr, Field, ConstraintHash, ConstraintEq>;

  const HFEliminator& hfe;
  const TEnvPtr&      tenv;
  Definitions*        ds;
  Fields              fields;

  RewriteFnAccess(const HFEliminator& hfe, const TEnvPtr& tenv, Definitions* ds) : hfe(hfe), tenv(tenv), ds(ds) {
  }

  void add(const ConstraintPtr& cst, const HasField& hf) {
    // (a repeated constraint keeps its first place)
    std::string fieldName;
    isFieldName(hf.fieldName, &fieldName);
    this->fields.insert(std::make_pair(cst, Field(this->fields.size(), fieldName)));
  }

  // the first of these constraints that both types state, if any
  //
  // (an applied use is decided from the application's type and its head's together: type inference gives an
  //  application the constraints of its head, and an annotation those of what it annotates, so the constraints
  //  that a use states can be read from where it is applied)
  const Fields::value_type* fieldFor(const QualTypePtr& t0, const QualTypePtr& t1) const {
    const Fields::value_type* r = nullptr;
    for (const auto& c : t0->constraints()) {
      auto f = this->fields.find(c);
      if (f != this->fields.end() && (r == nullptr || f->second.first < r->second.first) && (t1 == t0 || hasConstraint(c, t1))) {
        r = &*f;
      }
    }
    return r;
  }

  ExprPtr with(const App* ap) const override {
    if (ap->args().size() == 1) {
      if (const Var* f = is<Var>(stripAssumpHead(ap->fn()))) {
        if (f->value() == FIELD_VALUE_FN) {
          if (const auto* fd = fieldFor(ap->type(), f->type())) {
            ExprPtr r(new Proj(switchOf(ap->args()[0], *this), fd->second.second, ap->la()));
            r->type(ap->type());
            return this->hfe.unqualify(this->tenv, fd->first, r, this->ds);
          }
        }
      }
    }
    return wrapWithTy(ap->type(), new App(switchOf(ap->fn(), *this), switchOf(ap->args(), *this), ap->la()));
  }

  ExprPtr with(const Var* v) const override {
    if (v->value() == FIELD_VALUE_FN) {
      if (const auto* fd = fieldFor(v->type(), v->type())) {
        std::string vn = freshName();

        Func* fty = is<Func>(v->type()->monoType());
        MonoTypePtr recordType = fty->parameters()[0];
        ExprPtr p(new Proj(var(vn, recordType, v->la()), fd->second.second, v->la()));
        p->type(qualtype(list(fd->first), fty->result()));

        ExprPtr rv = fn(vn, p, v->la());
        rv->type(v->type());
        return this->hfe.unqualify(this->tenv, fd->first, rv, this->ds);
      }
    }
    return wrapWithTy(v->type(), new Var(v->value(), v->la()));
  }
};

ExprPtr FieldVerifier::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  HasField hf;
  if (dec(cst, &hf)) {
    for (const auto& hfe : this->eliminators) {
      try {
        if (hfe->satisfied(tenv, hf, ds)) {
          RewriteFnAccess rewrite(*hfe, tenv, ds);
          rewrite.add(cst, hf);
          return hfe->unqualify(tenv, cst, switchOf(e, rewrite), ds);
        }
      } catch (std::exception& exn) {
      }
    }
  }
  throw annotated_error(*e, "Cannot unqualify constraint: " + show(cst));
}

bool FieldVerifier::batchable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  HasField hf;
  if (dec(cst, &hf)) {
    try {
      return this->records->satisfied(tenv, hf, ds);
    } catch (std::exception&) {
    }
  }
  return false;
}

ExprPtr FieldVerifier::unqualifyBatch(const TEnvPtr& tenv, const Constraints& csts, const ExprPtr& e, Definitions* ds) const {
  // (an error here is reported as it is: to catch it and go back to one constraint at a
  //  time would hide it, and bring back the cost that batching is here to avoid)
  RewriteFnAccess rewrite(*this->records, tenv, ds);
  for (const auto& cst : csts) {
    HasField hf;
    if (!dec(cst, &hf)) {
      throw annotated_error(*e, "Cannot unqualify constraint: " + show(cst));
    }
    rewrite.add(cst, hf);
  }
  return this->records->unqualify(tenv, csts, switchOf(e, rewrite), ds);
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

