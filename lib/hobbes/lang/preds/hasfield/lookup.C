
#include <hobbes/lang/preds/hasfield/lookup.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/tyunqualify.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>

namespace hobbes {

static MonoTypePtr impliedType(const std::string& fieldName) {
  if (fieldName.size() > 2 && fieldName[0] == '.' && fieldName[1] == 'f') {
    return primty("int");
  } else {
    return arrayty(primty("char"));
  }
}

static ExprPtr impliedValue(const std::string& fieldName, const LexicalAnnotation& la) {
  if (fieldName.size() > 2 && fieldName[0] == '.' && fieldName[1] == 'f') {
    return constant(str::to<int>(fieldName.substr(2)), la);
  } else {
    return ExprPtr(mkarray(fieldName, la));
  }
}

static bool decFieldName(const MonoTypePtr& fieldName, std::string* fn) {
  if (const TString* x = is<TString>(fieldName)) {
    *fn = x->value();
    return true;
  } else if (const TLong* n = is<TLong>(fieldName)) {
    *fn = ".f" + str::from(n->value());
    return true;
  } else {
    return false;
  }
}

static bool mightRewriteToLookup(const TEnvPtr& tenv, const MonoTypePtr& rty, const std::string& fieldName, const MonoTypePtr& fty, Definitions* ds) {
  try {
    MonoTypeUnifier s(tenv);
    ConstraintPtr cst(new Constraint("Lookup", list(impliedType(fieldName), rty, fty)));

    refine(tenv, cst, &s, ds);
    return satisfiable(tenv, cst, ds);
  } catch (std::exception& ex) {
    return false;
  }
}

static MonoTypePtr rewritesToLookup(const TEnvPtr& tenv, const MonoTypePtr& rty, const std::string& fieldName, const MonoTypePtr& fty, Definitions* ds) {
  try {
    MonoTypeUnifier s(tenv);
    ConstraintPtr cst(new Constraint("Lookup", list(impliedType(fieldName), rty, fty)));

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

static ExprPtr rewriteLookup(const TEnvPtr& tenv, const ExprPtr& r, const std::string& fieldName, const MonoTypePtr& rty, const MonoTypePtr& fty, Definitions* ds, const LexicalAnnotation& la) {
  MonoTypePtr xty = impliedType(fieldName);

  return
    unqualifyTypes(
      tenv,
      fncall(
        var("lookup", qualtype(list(ConstraintPtr(new Constraint("Lookup", list(xty, rty, fty)))), functy(list(xty, rty), fty)), la),
        list(impliedValue(fieldName, la), r),
        la
      ),
      ds
    );
}

bool HFLookupEliminator::satisfied(const TEnvPtr& tenv, const HasField& hf, Definitions* ds) const {
  auto rty = hf.recordType;
  auto fty = hf.fieldType;

  std::string fname;
  if (decFieldName(hf.fieldName, &fname)) {
    MonoTypePtr efty = rewritesToLookup(tenv, rty, fname, fty, ds);
    return efty != MonoTypePtr() && isMonoSingular(efty) && *efty == *fty;
  } else {
    return false;
  }
}

bool HFLookupEliminator::satisfiable(const TEnvPtr& tenv, const HasField& hf, Definitions* ds) const {
  auto rty = hf.recordType;
  auto fty = hf.fieldType;

  std::string fname;
  if (decFieldName(hf.fieldName, &fname)) {
    return mightRewriteToLookup(tenv, rty, fname, fty, ds);
  } else {
    return is<TVar>(hf.fieldName);
  }
}

bool HFLookupEliminator::refine(const TEnvPtr& tenv, const HasField& hf, MonoTypeUnifier* s, Definitions* ds) {
  auto rty = hf.recordType;
  auto fty = hf.fieldType;

  std::string fname;
  if (decFieldName(hf.fieldName, &fname)) {
    MonoTypePtr efty = rewritesToLookup(tenv, rty, fname, fty, ds);

    if (efty != MonoTypePtr() && isMonoSingular(efty)) {
      size_t sz = s->size();
      mgu(efty, fty, s);
      return s->size() != sz;
    }
  }
  return false;
}

// rewrite funny-looking function calls
struct HFLookupUnqualify : public switchExprTyFn {
  const TEnvPtr&       tenv;
  const ConstraintPtr& constraint;
  Definitions*         defs;

  HFLookupUnqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* defs) : tenv(tenv), constraint(cst), defs(defs) {
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }

  ExprPtr with(const Proj* v) const {
    ExprPtr prec = switchOf(v->record(), *this);

    if (hasConstraint(this->constraint, v->type()) && rewritesToLookup(this->tenv, prec->type()->monoType(), v->field(), v->type()->monoType(), this->defs)) {
      return rewriteLookup(this->tenv, prec, v->field(), prec->type()->monoType(), v->type()->monoType(), this->defs, v->la());
    }

    return wrapWithTy(v->type(), new Proj(prec, v->field(), v->la()));
  }
};

ExprPtr HFLookupEliminator::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  return switchOf(e, HFLookupUnqualify(tenv, cst, ds));
}

std::string HFLookupEliminator::name() const { return "lookup"; }

}

