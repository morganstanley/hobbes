
#include <hobbes/lang/preds/hasfield/slookup.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/tyunqualify.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>

namespace hobbes {

static MonoTypePtr impliedType(const std::string& fieldName) {
  if (fieldName.size() > 2 && fieldName[0] == '.' && fieldName[1] == 'f') {
    return tlong(str::to<long>(fieldName.substr(2)));
  } else {
    return MonoTypePtr(TString::make(fieldName));
  }
}

static MonoTypePtr impliedType(const MonoTypePtr& fieldName) {
  if (const TString* fn = is<TString>(fieldName)) {
    return impliedType(fn->value());
  } else {
    return fieldName;
  }
}

bool mightRewriteToSLookup(const TEnvPtr& tenv, const MonoTypePtr& rty, const MonoTypePtr& fieldName, const MonoTypePtr& fty, Definitions* ds) {
  if (is<TVar>(rty)) {
    return true;
  }

  try {
    MonoTypeUnifier s(tenv);
    ConstraintPtr cst(new Constraint("SLookup", list(rty, impliedType(fieldName), fty)));

    refine(tenv, cst, &s, ds);
    return satisfiable(tenv, cst, ds);
  } catch (std::exception& ex) {
    return false;
  }
}

MonoTypePtr rewritesToSLookup(const TEnvPtr& tenv, const MonoTypePtr& rty, const MonoTypePtr& fieldName, const MonoTypePtr& fty, Definitions* ds) {
  try {
    MonoTypeUnifier s(tenv);
    ConstraintPtr cst(new Constraint("SLookup", list(rty, impliedType(fieldName), fty)));

    refine(tenv, cst, &s, ds);
    if (satisfied(tenv, cst, ds)) {
      return s.substitute(fty);
    } else {
      return MonoTypePtr();
    }
  } catch (std::exception& ex) {
    return MonoTypePtr();
  }
}

ExprPtr rewriteSLookup(const TEnvPtr& tenv, const ExprPtr& r, const std::string& fieldName, const MonoTypePtr& rty, const MonoTypePtr& fty, Definitions* ds, const LexicalAnnotation& la) {
  MonoTypePtr xty = impliedType(fieldName);

  return
    unqualifyTypes(
      tenv,
      fncall(
        var("slookup", qualtype(list(ConstraintPtr(new Constraint("SLookup", list(rty, xty, fty)))), functy(list(rty), fty)), la),
        list(r),
        la
      ),
      ds
    );
}

bool HFSLookupEliminator::satisfied(const TEnvPtr& tenv, const HasField& hf, Definitions* ds) const {
  auto rty    = hf.recordType;
  auto fnamet = hf.fieldName;
  auto fty    = hf.fieldType;

  if (is<TString>(fnamet) || is<TLong>(fnamet)) {
    MonoTypePtr efty = rewritesToSLookup(tenv, rty, fnamet, fty, ds);
    return efty != MonoTypePtr() && isMonoSingular(efty) && *efty == *fty;
  } else {
    return false;
  }
}

bool HFSLookupEliminator::satisfiable(const TEnvPtr& tenv, const HasField& hf, Definitions* ds) const {
  auto rty    = hf.recordType;
  auto fnamet = hf.fieldName;
  auto fty    = hf.fieldType;

  return mightRewriteToSLookup(tenv, rty, fnamet, fty, ds);
}

bool HFSLookupEliminator::refine(const TEnvPtr& tenv, const HasField& hf, MonoTypeUnifier* s, Definitions* ds) {
  auto rty    = hf.recordType;
  auto fnamet = hf.fieldName;
  auto fty    = hf.fieldType;

  if (is<TString>(fnamet) || is<TLong>(fnamet)) {
    MonoTypePtr efty = rewritesToSLookup(tenv, rty, fnamet, fty, ds);

    if (efty != MonoTypePtr() && isMonoSingular(efty)) {
      size_t sz = s->size();
      mgu(efty, fty, s);
      return s->size() != sz;
    }
  }
  return false;
}

// rewrite funny-looking function calls
struct HFSLookupUnqualify : public switchExprTyFn {
  const TEnvPtr&       tenv;
  const ConstraintPtr& constraint;
  Definitions*         defs;

  HFSLookupUnqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* defs) : tenv(tenv), constraint(cst), defs(defs) {
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }

  ExprPtr with(const Proj* v) const {
    ExprPtr prec = switchOf(v->record(), *this);

    if (hasConstraint(this->constraint, v->type()) && rewritesToSLookup(this->tenv, prec->type()->monoType(), impliedType(v->field()), v->type()->monoType(), this->defs)) {
      return rewriteSLookup(this->tenv, prec, v->field(), prec->type()->monoType(), v->type()->monoType(), this->defs, v->la());
    }

    return wrapWithTy(v->type(), new Proj(prec, v->field(), v->la()));
  }
};

ExprPtr HFSLookupEliminator::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  return switchOf(e, HFSLookupUnqualify(tenv, cst, ds));
}

std::string HFSLookupEliminator::name() const { return "slookup"; }

}

