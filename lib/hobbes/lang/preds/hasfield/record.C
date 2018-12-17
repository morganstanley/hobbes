
#include <hobbes/lang/preds/hasfield/record.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

static MonoTypePtr stdstrty  = OpaquePtr::make("std::string", 0, false);
static MonoTypePtr chararrty = arrayty(primty("char"));
static MonoTypePtr unitty    = primty("unit");

static bool isFieldName(const MonoTypePtr& fn, std::string* sfn) {
  if (const TString* fns = is<TString>(fn)) {
    *sfn = fns->value();
    return true;
  } else if (const TLong* tnn = is<TLong>(fn)) {
    *sfn = ".f" + str::from(tnn->value());
    return true;
  } else {
    return false;
  }
}

bool HFRecordEliminator::satisfied(const TEnvPtr& tenv, const HasField& hf, Definitions*) const {
  const Record* r = is<Record>(hf.recordType);
  if (!r) return false;

  std::string fname;
  if (!isFieldName(hf.fieldName, &fname)) return false;

  if (const Record::Member* m = r->mmember(fname)) {
    MonoTypePtr lhst = normIfOpaquePtr(m->type);

    // a little hack to allow for assigning [char] into <std.string>
    if (hf.direction == HasField::Write) {
      if (*lhst == *stdstrty && *hf.fieldType == *chararrty) {
        return true;
      }
    }
    return unifiable(tenv, lhst, hf.fieldType);
  } else {
    return false;
  }
}

bool HFRecordEliminator::satisfiable(const TEnvPtr& tenv, const HasField& hf, Definitions* ds) const {
  if (const Record* r = is<Record>(hf.recordType)) {
    std::string fname;
    if (isFieldName(hf.fieldName, &fname)) {
      const Record::Member* rm = r->mmember(fname);
      return rm != 0 && (is<TVar>(hf.fieldType) || is<TVar>(rm->type) || satisfied(tenv, hf, ds));
    } else {
      return is<TVar>(hf.fieldName);
    }
  }
  return is<TVar>(hf.recordType);
}

bool HFRecordEliminator::refine(const TEnvPtr&, const HasField& hf, MonoTypeUnifier* s, Definitions*) {
  if (const Record* r = is<Record>(hf.recordType)) {
    std::string fname;
    if (isFieldName(hf.fieldName, &fname)) {
      if (const Record::Member* m = r->mmember(fname)) {
        MonoTypePtr lhst = normIfOpaquePtr(m->type);

        if (hf.direction == HasField::Read || !(*lhst == *stdstrty)) {
          size_t uc = s->size();
          mgu(hf.fieldType, lhst, s);
          return uc != s->size();
        }
      }
    }
  }
  return false;
}

// just remove the 'hasfield' constraint from record projection expressions
struct HFRecordUnqualify : public switchExprTyFn {
  const ConstraintPtr& constraint;
  HFRecordUnqualify(const ConstraintPtr& cst) : constraint(cst) {
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }

  ExprPtr with(const Assign* v) const {
    // hack to assign [char] into <std.string>
    if (hasConstraint(this->constraint, v->type())) {
      if (*v->left()->type()->monoType() == *stdstrty && *v->right()->type()->monoType() == *chararrty) {
        static MonoTypePtr assignty = functy(list(stdstrty, chararrty), unitty);

        return fncall(var("stdstringAssign", assignty, v->left()->la()), list(switchOf(v->left(), *this), switchOf(v->right(), *this)), v->la());
      }
    }

    return wrapWithTy(v->type(), new Assign(switchOf(v->left(), *this), switchOf(v->right(), *this), v->la()));
  }
};

ExprPtr HFRecordEliminator::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, HFRecordUnqualify(cst));
}

std::string HFRecordEliminator::name() const { return "record fields"; }

}

