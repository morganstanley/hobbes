
#include <hobbes/lang/preds/appendsto/record.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/lang/preds/class.H>
#include <sstream>

namespace hobbes {

#define REF_REC_APPEND "recordAppend"
#define REF_REC_PREFIX "recordPrefix"
#define REF_REC_SUFFIX "recordSuffix"

bool isRecordLike(const MonoTypePtr& mt) {
  return is<Record>(mt) || isUnit(mt);
}

const Record::Members& recordMembers(const MonoTypePtr& mt) {
  if (const Record* rty = is<Record>(mt)) {
    return rty->members();
  } else if (isUnit(mt)) {
    static Record::Members ums;
    return ums;
  } else {
    throw std::runtime_error("Not a record type: " + show(mt));
  }
}

bool isTupleLike(const std::string& fname) {
  return fname.substr(0, 2) == ".f";
}

bool isTupleLike(const Record::Members& ms) {
  return ms.size() > 0 && isTupleLike(ms[0].field);
}

bool isTupleLike(const MkRecord::FieldDefs& fds) {
  return fds.size() > 0 && isTupleLike(fds[0].first);
}

Record::Members tupleNormalize(const Record::Members& ms) {
  if (isTupleLike(ms)) {
    Record::Members result;
    size_t i = 0;
    for (Record::Members::const_iterator m = ms.begin(); m != ms.end(); ++m) {
      result.push_back(Record::Member(".f" + str::from(i++), m->type, m->offset));
    }
    return result;
  } else {
    return ms;
  }
}

MkRecord::FieldDefs tupleNormalize(const MkRecord::FieldDefs& fds) {
  if (isTupleLike(fds)) {
    MkRecord::FieldDefs result;
    size_t i = 0;
    for (MkRecord::FieldDefs::const_iterator fd = fds.begin(); fd != fds.end(); ++fd) {
      result.push_back(MkRecord::FieldDef(".f" + str::from(i++), fd->second));
    }
    return result;
  } else {
    return fds;
  }
}

MonoTypePtr makeRecordType(const Record::Members& ms) {
  if (ms.size() == 0) {
    static MonoTypePtr u(Prim::make("unit"));
    return u;
  } else {
    return MonoTypePtr(Record::make(tupleNormalize(ms)));
  }
}

ExprPtr makeRecord(const MkRecord::FieldDefs& fds, const LexicalAnnotation& la) {
  if (fds.size() == 0) {
    return mktunit(la);
  } else {
    return mkrecord(tupleNormalize(fds), la);
  }
}

[[noreturn]] void failAppendRecordConstraint(const std::string& lhs, const std::string& rhs, const std::string& result) {
  std::ostringstream ss;
  ss << "Cannot solve type constraint: " << lhs << " ++ " << rhs << " = " << result;
  throw std::runtime_error(ss.str());
}

void importDefs(const Record::Members& ms, Record::Members* out) {
  for (Record::Members::const_iterator m = ms.begin(); m != ms.end(); ++m) {
    out->push_back(Record::Member(m->field, m->type));
  }
}

MonoTypePtr joinedRecord(const MonoTypePtr& lhs, const MonoTypePtr& rhs) {
  Record::Members result;
  importDefs(recordMembers(lhs), &result);
  importDefs(recordMembers(rhs), &result);
  return makeRecordType(result);
}

QualTypePtr joinedRecord(const QualTypePtr& lhs, const QualTypePtr& rhs) {
  return qualtype(mergeConstraints(lhs->constraints(), rhs->constraints()), joinedRecord(lhs->monoType(), rhs->monoType()));
}

bool equivalent(const TEnvPtr& tenv, const Record::Member& m0, const Record::Member& m1) {
  return ((isTupleLike(m0.field) && isTupleLike(m1.field)) || (m0.field == m1.field)) && unifiable(tenv, m0.type, m1.type);
}

MonoTypePtr recordSuffix(const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& result) {
  Record::Members rhs;
  const Record::Members& lhsms = recordMembers(lhs);
  const Record::Members& resms = recordMembers(result);

  if (lhsms.size() > resms.size()) {
    failAppendRecordConstraint(show(lhs), "X", show(result));
  } else {
    // verify that the prefix matches
    for (size_t i = 0; i < lhsms.size(); ++i) {
      if (!equivalent(tenv, lhsms[i], resms[i])) {
        failAppendRecordConstraint(show(lhs), "X", show(result));
      }
    }

    // then we can determine the suffix
    for (size_t j = lhsms.size(); j < resms.size(); ++j) {
      rhs.push_back(Record::Member(resms[j].field, resms[j].type));
    }
  }

  return makeRecordType(rhs);
}

MonoTypePtr recordPrefix(const TEnvPtr& tenv, const MonoTypePtr& rhs, const MonoTypePtr& result) {
  Record::Members lhs;
  const Record::Members& rhsms = recordMembers(rhs);
  const Record::Members& resms = recordMembers(result);

  if (rhsms.size() > resms.size()) {
    failAppendRecordConstraint("X", show(rhs), show(result));
  } else {
    size_t lhsz = resms.size() - rhsms.size();

    // verify that the suffix matches
    for (size_t i = 0; i < rhsms.size(); ++i) {
      if (!equivalent(tenv, rhsms[i], resms[lhsz + i])) {
        failAppendRecordConstraint("X", show(rhs), show(result));
      }
    }

    // then we can determine the prefix
    for (size_t j = 0; j < lhsz; ++j) {
      lhs.push_back(Record::Member(resms[j].field, resms[j].type));
    }
  }

  return makeRecordType(lhs);
}

bool ATRecordEliminator::satisfied(const TEnvPtr&, const MonoTypePtr& lhs, const MonoTypePtr& rhs, const MonoTypePtr& result) const {
  return isRecordLike(lhs) && isRecordLike(rhs) && isRecordLike(result) && (*result == *joinedRecord(lhs, rhs));
}

void checkSatisfiable(const MonoTypePtr& ty) {
  if (!isRecordLike(ty) && !is<TVar>(ty)) {
    throw std::runtime_error("Not eligible for record-append relationship: " + show(ty));
  }
}

void tryRefine(const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs, const MonoTypePtr& result, MonoTypeUnifier* s) {
  // refinement of this constraint is only meaningful if all arguments are either "record-like" or variables
  checkSatisfiable(lhs);
  checkSatisfiable(rhs);
  checkSatisfiable(result);

  // there are three cases we can refine:
  //   the left record is known, the right record is known, the result isn't known
  //   the left record is known, the right record is unknown, the result is known
  //   the left record is unknown, the right record is known, the result is known
  if (isRecordLike(lhs) && isRecordLike(rhs)) {
    mgu(joinedRecord(lhs, rhs), result, s);
  }

  if (isRecordLike(lhs) && isRecordLike(result)) {
    mgu(recordSuffix(tenv, lhs, result), rhs, s);
  }

  if (isRecordLike(rhs) && isRecordLike(result)) {
    mgu(recordPrefix(tenv, rhs, result), lhs, s);
  }
}

bool ATRecordEliminator::satisfiable(const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs, const MonoTypePtr& result) const {
  MonoTypeUnifier s(tenv);
  try {
    tryRefine(tenv, lhs, rhs, result, &s);
    return true;
  } catch (std::exception&) {
    return false;
  }
}

bool ATRecordEliminator::refine(const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs, const MonoTypePtr& result, MonoTypeUnifier* s) {
  size_t uc = s->size();

  try {
    tryRefine(tenv, lhs, rhs, result, s);
    return uc != s->size();
  } catch (std::exception&) {
    return false;
  }
}

PolyTypePtr ATRecordEliminator::lookup(const std::string& vn) const {
  if (vn == REF_REC_APPEND) {
    return polytype(3, qualtype(list(ConstraintPtr(new Constraint(AppendsToUnqualifier::constraintName(), list(tgen(0), tgen(1), tgen(2))))), functy(list(tgen(0), tgen(1)), tgen(2))));
  } else if (vn == REF_REC_PREFIX) {
    return polytype(3, qualtype(list(ConstraintPtr(new Constraint(AppendsToUnqualifier::constraintName(), list(tgen(0), tgen(1), tgen(2))))), functy(list(tgen(2)), tgen(0))));
  } else if (vn == REF_REC_SUFFIX) {
    return polytype(3, qualtype(list(ConstraintPtr(new Constraint(AppendsToUnqualifier::constraintName(), list(tgen(0), tgen(1), tgen(2))))), functy(list(tgen(2)), tgen(1))));
  } else {
    return PolyTypePtr();
  }
}

SymSet ATRecordEliminator::bindings() const {
  SymSet result;
  result.insert(REF_REC_APPEND);
  result.insert(REF_REC_PREFIX);
  result.insert(REF_REC_SUFFIX);
  return result;
}

void insertFieldDefs(const MkRecord::FieldDefs& ifds, MkRecord::FieldDefs* out) {
  out->insert(out->end(), ifds.begin(), ifds.end());
}

void insertFieldDefs(const MkRecord::FieldDefs& ifds, size_t c, MkRecord::FieldDefs* out) {
  out->insert(out->end(), ifds.begin(), ifds.begin() + std::min<size_t>(ifds.size(), c));
}

void insertFieldDefsSfx(const MkRecord::FieldDefs& ifds, size_t c, MkRecord::FieldDefs* out) {
  out->insert(out->end(), ifds.begin() + (ifds.size() - c), ifds.end());
}

void insertFieldDefsFromProj(const ExprPtr& rec, const Record* rty, MkRecord::FieldDefs* out) {
  if (!rty) {
    throw annotated_error(*rec, "Internal error, can't insert projections out of non-record type: " + show(rec->type()));
  }

  for (Record::Members::const_iterator m = rty->members().begin(); m != rty->members().end(); ++m) {
    out->push_back(MkRecord::FieldDef(m->field, proj(rec, rty, m->field, rec->la())));
  }
}

void insertFieldDefsFromProj(const ExprPtr& rec, const MonoTypePtr& rty, MkRecord::FieldDefs* out) {
  insertFieldDefsFromProj(rec, is<Record>(rty), out);
}

void insertFieldDefsFromProj(const ExprPtr& rec, MkRecord::FieldDefs* out) {
  insertFieldDefsFromProj(rec, rec->type()->monoType(), out);
}

ExprPtr recordAppendExpr(const MkRecord* lhs, const MkRecord* rhs) {
  MkRecord::FieldDefs fds;
  insertFieldDefs(lhs->fields(), &fds);
  insertFieldDefs(rhs->fields(), &fds);
  return makeRecord(fds, LexicalAnnotation::merge(lhs->la(), rhs->la()));
}

ExprPtr recordAppendExpr(const MkRecord* lhs, const ExprPtr& rhs) {
  if (const MkRecord* mrhs = is<MkRecord>(rhs)) {
    return recordAppendExpr(lhs, mrhs);
  } else {
    std::string rvn = freshName();
    ExprPtr     rv  = var(rvn, rhs->type(), rhs->la());

    MkRecord::FieldDefs fds;
    insertFieldDefs(lhs->fields(), &fds);
    insertFieldDefsFromProj(rv, &fds);

    auto la = LexicalAnnotation::merge(lhs->la(), rhs->la());
    return let(rvn, rhs, makeRecord(fds, la), la);
  }
}

ExprPtr recordAppendExpr(const ExprPtr& lhs, const MkRecord* rhs) {
  if (const MkRecord* mlhs = is<MkRecord>(lhs)) {
    return recordAppendExpr(mlhs, rhs);
  } else {
    std::string lvn = freshName();
    ExprPtr     lv  = var(lvn, lhs->type(), lhs->la());

    MkRecord::FieldDefs fds;
    insertFieldDefsFromProj(lv, &fds);
    insertFieldDefs(rhs->fields(), &fds);

    auto la = LexicalAnnotation::merge(lhs->la(), rhs->la());
    return let(lvn, lhs, makeRecord(fds, la), la);
  }
}

ExprPtr recordAppendExpr(const ExprPtr& lhs, const ExprPtr& rhs) {
  if (isUnit(lhs->type()->monoType())) {
    return rhs;
  } else if (isUnit(rhs->type()->monoType())) {
    return lhs;
  } else if (const MkRecord* mlhs = is<MkRecord>(lhs)) {
    return recordAppendExpr(mlhs, rhs);
  } else if (const MkRecord* mrhs = is<MkRecord>(rhs)) {
    return recordAppendExpr(lhs, mrhs);
  } else {
    std::string lvn = freshName();
    ExprPtr     lv  = var(lvn, lhs->type(), lhs->la());
    std::string rvn = freshName();
    ExprPtr     rv  = var(rvn, rhs->type(), rhs->la());

    MkRecord::FieldDefs fds;
    insertFieldDefsFromProj(lv, &fds);
    insertFieldDefsFromProj(rv, &fds);

    auto la = LexicalAnnotation::merge(lhs->la(), rhs->la());
    return let(lvn, lhs, let(rvn, rhs, makeRecord(fds, la), la), la);
  }
}

ExprPtr recordAppendFunction(const MonoTypePtr& lty, const MonoTypePtr& rty, const MonoTypePtr& resty, const LexicalAnnotation& la) {
  std::string lvn = freshName();
  ExprPtr     lv  = var(lvn, lty, la);
  std::string rvn = freshName();
  ExprPtr     rv  = var(rvn, rty, la);

  MkRecord::FieldDefs fds;
  insertFieldDefsFromProj(lv, &fds);
  insertFieldDefsFromProj(rv, &fds);
  
  ExprPtr result = fn(list(lvn, rvn), makeRecord(fds, la), la);
  result->type(qualtype(functy(list(lty, rty), resty)));

  return result;
}

ExprPtr recordPrefixFunction(const MonoTypePtr& recty, const MonoTypePtr& resty, const LexicalAnnotation& la) {
  return var(".cast", functy(recty, resty), la);
}

ExprPtr recordPrefix(const ExprPtr& rec, const MonoTypePtr& resty) {
  const Record::Members& ms = recordMembers(resty);

  if (ms.size() == 0) {
    return mktunit(rec->la());
  } else if (const MkRecord* r = is<MkRecord>(rec)) {
    MkRecord::FieldDefs fds;
    insertFieldDefs(r->fields(), ms.size(), &fds);
    return makeRecord(fds, rec->la());
  } else {
    return fncall(recordPrefixFunction(rec->type()->monoType(), resty, rec->la()), rec, rec->la());
  }
}

ExprPtr recordSuffixFunction(const MonoTypePtr& recty, const MonoTypePtr& resty, const LexicalAnnotation& la) {
  std::string vn = freshName();
  ExprPtr     v  = var(vn, recty, la);

  MkRecord::FieldDefs fds;
  insertFieldDefsFromProj(v, resty, &fds);

  ExprPtr result = fn(list(vn), makeRecord(fds, la), la);
  result->type(qualtype(functy(list(recty), resty)));

  return result;
}

ExprPtr recordSuffix(const ExprPtr& rec, const MonoTypePtr& resty) {
  const Record::Members& ms = recordMembers(resty);

  if (ms.size() == 0) {
    return mktunit(rec->la());
  } else if (const MkRecord* r = is<MkRecord>(rec)) {
    MkRecord::FieldDefs fds;
    insertFieldDefsSfx(r->fields(), ms.size(), &fds);
    return makeRecord(fds, rec->la());
  } else {
    std::string vn = freshName();
    ExprPtr     v  = var(vn, rec->type(), rec->la());

    MkRecord::FieldDefs fds;
    insertFieldDefsFromProj(v, resty, &fds);

    return let(vn, rec, makeRecord(fds, rec->la()), rec->la());
  }
}

struct ATRecordUnqualify : public switchExprTyFn {
  TEnvPtr              tenv;
  const ConstraintPtr& constraint;
  AppendsTo            appto;
  ATRecordUnqualify(const TEnvPtr& tenv, const ConstraintPtr& cst) : tenv(tenv), constraint(cst) {
    if (!dec(cst, &this->appto)) {
      throw std::runtime_error("Internal error, invalid constraint for appends-to resolution: " + show(this->constraint));
    }
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }

  ExprPtr with(const Var* vn) const {
    if (vn->value() == REF_REC_APPEND) {
      return recordAppendFunction(this->appto.leftType, this->appto.rightType, this->appto.resultType, vn->la());
    } else if (vn->value() == REF_REC_PREFIX) {
      return recordPrefixFunction(this->appto.resultType, this->appto.leftType, vn->la());
    } else if (vn->value() == REF_REC_SUFFIX) {
      return recordSuffixFunction(this->appto.resultType, this->appto.rightType, vn->la());
    } else {
      return wrapWithTy(vn->type(), new Var(vn->value(), vn->la()));
    }
  }

  ExprPtr with(const App* ap) const {
    if (const Var* fn = is<Var>(stripAssumpHead(ap->fn()))) {
      if (fn->value() == REF_REC_APPEND) {
        return recordAppendExpr(switchOf(ap->args()[0], *this), switchOf(ap->args()[1], *this));
      } else if (fn->value() == REF_REC_PREFIX) {
        return recordPrefix(switchOf(ap->args()[0], *this), this->appto.leftType);
      } else if (fn->value() == REF_REC_SUFFIX) {
        return recordSuffix(switchOf(ap->args()[0], *this), this->appto.rightType);
      }
    }

    return wrapWithTy(ap->type(), new App(switchOf(ap->fn(), *this), switchOf(ap->args(), *this), ap->la()));
  }
};

ExprPtr ATRecordEliminator::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, ATRecordUnqualify(tenv, cst));
}

}

