
#include <hobbes/lang/expr.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/preds/consrecord.H>
#include <hobbes/lang/preds/hasfield.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>
#include <memory>

namespace hobbes {

//////////
// record type deconstruction
//////////

#define REF_REC_LABEL "recordHeadLabel"
#define REF_REC_VALUE "recordHeadValue"
#define REF_REC_TAIL  "recordTail"
#define REF_TUP_TAIL  "tupleTail"

MonoTypePtr stripHiddenFields(const Record* rty) {
  Record::Members ms;
  for (const auto& m : rty->members()) {
    if (m.field.size() < 2 || (m.field[0] != '.' || m.field[1] != 'p')) {
      ms.push_back(Record::Member(m.field, m.type));
    }
  }
  return Record::make(ms);
}

MonoTypePtr stripHiddenFields(const MonoTypePtr& ty) {
  if (const Record* rty = is<Record>(ty)) {
    return stripHiddenFields(rty);
  } else {
    return ty;
  }
}

static bool isVisibleField(const std::string& f) {
  return f.size() < 2 || (f[0] != '.' || f[1] != 'p');
}

static bool hasVisibleField(const Record* rty) {
  for (const auto& m : rty->members()) {
    if (isVisibleField(m.field)) {
      return true;
    }
  }
  return false;
}

// do two types describe records with the same visible fields (by name and type)?
// (compared member-wise so that neither hidden padding fields nor member offsets --
// which legitimately differ between a standalone record and the tail of a larger
// one -- affect the answer)
static bool equalVisibleFields(const MonoTypePtr& aty, const MonoTypePtr& bty) {
  if (*aty == *bty) {
    return true;
  }
  const Record* a = is<Record>(aty);
  const Record* b = is<Record>(bty);
  if ((a == nullptr) || (b == nullptr)) {
    return false;
  }
  const auto& ams = a->members();
  const auto& bms = b->members();
  size_t i = 0, j = 0;
  while (true) {
    while (i < ams.size() && !isVisibleField(ams[i].field)) ++i;
    while (j < bms.size() && !isVisibleField(bms[j].field)) ++j;
    if (i == ams.size() || j == bms.size()) {
      return i == ams.size() && j == bms.size();
    }
    if (ams[i].field != bms[j].field || !(*ams[i].type == *bms[j].type)) {
      return false;
    }
    ++i; ++j;
  }
}

struct ConsRecord {
  bool        forward;
  bool        asTuple;
  MonoTypePtr recordType;
  MonoTypePtr headFieldName;
  MonoTypePtr headType;
  MonoTypePtr tailType;
};

static bool dec(const ConstraintPtr& c, ConsRecord* cr) {
  if (c->name() == RecordDeconstructor::constraintName() && c->arguments().size() == 6) {
    if (const TLong* fwd = is<TLong>(c->arguments()[0])) {
      if (const TLong* astup = is<TLong>(c->arguments()[1])) {
        cr->forward       = fwd->value() != 0;
        cr->asTuple       = astup->value() != 0;
        cr->recordType    = c->arguments()[2];
        cr->headFieldName = c->arguments()[3];
        cr->headType      = c->arguments()[4];
        cr->tailType      = c->arguments()[5];

        if (const Record* r = is<Record>(cr->recordType)) {
          if (!r->members().empty()) {
            auto lpfx = r->members()[r->members().size()-1].field.substr(0,2);

            if (lpfx != ".p" && ((lpfx != ".f" && cr->asTuple) || (lpfx == ".f" && !cr->asTuple))) {
              return false;
            }
          }
        }
        return true;
      }
    }
  }
  return false;
}

std::string RecordDeconstructor::constraintName() {
  return "ConsRecord";
}

bool RecordDeconstructor::refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
  size_t uc = u->size();
  ConsRecord cr;
  if (dec(cst, &cr)) {
    if (const Record* rty = is<Record>(cr.recordType)) {
      if (!rty->members().empty()) {
        MonoTypePtr fname(TString::make(rty->headMember().field));

        mgu(fname,                  cr.headFieldName, u);
        mgu(rty->headMember().type, cr.headType,      u);

        if (cr.forward) {
          mgu(rty->tailType(), cr.tailType, u);
        } else {
          mgu(stripHiddenFields(rty->tailType()), stripHiddenFields(cr.tailType), u);
        }
      }
    } else if (isUnit(cr.tailType)) {
      if (cr.asTuple) {
        mgu(cr.headFieldName, MonoTypePtr(TString::make(".f0")), u);
        mgu(cr.recordType, MonoTypePtr(Record::make(cr.headType, Record::Members())), u);
      } else if (const TString* lbl = is<TString>(cr.headFieldName)) {
        mgu(cr.recordType, MonoTypePtr(Record::make(lbl->value(), cr.headType, Record::Members())), u);
      }
    } else if (const Record* rtail = is<Record>(cr.tailType)) {
      if (cr.asTuple) {
        mgu(cr.headFieldName, MonoTypePtr(TString::make(".f0")), u);
        mgu(cr.recordType, MonoTypePtr(Record::make(cr.headType, rtail->members())), u);
      } else if (const TString* lbl = is<TString>(cr.headFieldName)) {
        mgu(cr.recordType, MonoTypePtr(Record::make(lbl->value(), cr.headType, rtail->members())), u);
      }
    }
  }
  return uc != u->size();
}

bool RecordDeconstructor::satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
  ConsRecord cr;
  if (!dec(cst, &cr)) {
    return false;
  }

  const TString* fname = is<TString>(cr.headFieldName);
  if (fname == nullptr) {
    return false;
  }

  const Record* rty = is<Record>(cr.recordType);
  if (rty == nullptr) {
    return false;
  }
  if (cr.asTuple != rty->isTuple()) {
    return false;
  }

  const Record* tty = is<Record>(cr.tailType);
  if ((tty == nullptr) && !isUnit(cr.tailType)) {
    return false;
  }

  if (!hasVisibleField(rty)) {
    return false;
  }

  // compare the head and tail of the record independently (as 'refine' unifies them)
  // rather than reconstructing a record out of the constraint's head and tail types --
  // 'headMember' normalizes opaque types stored inline to their by-reference form, so a
  // reconstructed record can never match a record whose head field is an inline opaque
  // type (eg: a lifted struct with a std::string field followed by any other field)
  if (cr.asTuple) {
    if (tty != nullptr) {
      return *rty->headMember().type == *cr.headType &&
             equalVisibleFields(rty->tailType(), cr.tailType);
    } else {
      return *rty->headMember().type == *cr.headType;
    }
  } else {
    if (tty != nullptr) {
      return rty->headMember().field == fname->value() &&
             *rty->headMember().type == *cr.headType &&
             equalVisibleFields(rty->tailType(), cr.tailType);
    } else {
      return rty->headMember().field == fname->value() && *rty->headMember().type == *cr.headType;
    }
  }
}

bool RecordDeconstructor::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  ConsRecord cr;
  if (dec(cst, &cr)) {
    return satisfied(tenv, cst, ds) || (is<TVar>(cr.recordType) != nullptr) || (is<TVar>(cr.headFieldName) != nullptr) || ((is<Record>(cr.recordType) != nullptr) && ((is<TVar>(cr.headType) != nullptr) || (is<TVar>(cr.tailType) != nullptr)));
  } else {
    return false;
  }
}

void RecordDeconstructor::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

PolyTypePtr RecordDeconstructor::lookup(const std::string& vn) const {
  if (vn == REF_REC_LABEL) {
    return polytype(4, qualtype(list(std::make_shared<Constraint>(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), tgen(0), tgen(1), tgen(2), tgen(3)))), functy(list(tgen(0)), arrayty(prim<char>()))));
  } else if (vn == REF_REC_VALUE) {
    return polytype(4, qualtype(list(std::make_shared<Constraint>(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), tgen(0), tgen(1), tgen(2), tgen(3)))), functy(list(tgen(0)), tgen(2))));
  } else if (vn == REF_REC_TAIL) {
    return polytype(4, qualtype(list(std::make_shared<Constraint>(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), tgen(0), tgen(1), tgen(2), tgen(3)))), functy(list(tgen(0)), tgen(3))));
  } else if (vn == REF_TUP_TAIL) {
    return polytype(4, qualtype(list(std::make_shared<Constraint>(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), tgen(0), tgen(1), tgen(2), tgen(3)))), functy(list(tgen(0)), tgen(3))));
  } else {
    return PolyTypePtr();
  }
}

SymSet RecordDeconstructor::bindings() const {
  SymSet r;
  r.insert(REF_REC_LABEL);
  r.insert(REF_REC_VALUE);
  r.insert(REF_REC_TAIL);
  r.insert(REF_TUP_TAIL);
  return r;
}

FunDeps RecordDeconstructor::dependencies(const ConstraintPtr&) const {
  // ordering determined by constraints map:
  //   (fwd, ty(x->asTuple()), x->recordType(), x->headFieldName(), x->headType(), x->tailType())
  FunDeps result;
  result.push_back(FunDep(list(2), 3));
  result.push_back(FunDep(list(2), 4));
  result.push_back(FunDep(list(2), 5));

  // the fundep that says that a record is uniquely determined by its head label, head type and tail type
  // is not sound when hidden prefix fields may be different
  //result.push_back(FunDep(list(2, 3, 4), 1));
  return result;
}

ExprPtr stripAssumps(const ExprPtr& e) {
  if (const Assump* a = is<Assump>(e)) {
    return stripAssumps(a->expr());
  } else {
    return e;
  }
}

// resolve satisfied record deconstruction predicates
struct RDUnqualify : public switchExprTyFn {
  const ConstraintPtr& constraint;
  const Record*        rty;

  RDUnqualify(const ConstraintPtr& constraint) : constraint(constraint) {
    ConsRecord cr;
    if (dec(constraint, &cr)) {
      if (const Record* rty = is<Record>(cr.recordType)) {
        this->rty = rty;
      } else {
        throw std::runtime_error("For cons-record resolution, unqualifying non record type: " + show(cr.recordType));
      }
    } else {
      throw std::runtime_error("For cons-record resolution, unqualifying non cons constraint: " + show(constraint));
    }
  }

  QualTypePtr withTy(const QualTypePtr& qt) const override {
    return removeConstraint(this->constraint, qt);
  }

  ExprPtr with(const Assign* v) const override {
    if (hasConstraint(this->constraint, v->left()->type())) {
      if (const App* a = is<App>(stripAssumps(v->left()))) {
        if (const Var* f = is<Var>(stripAssumps(a->fn()))) {
          if (f->value() == REF_REC_VALUE) {
            if (a->args().size() != 1) {
              throw annotated_error(*v, "Invalid application of 'recordHeadValue' in assignment");
            }

            const Record::Member& hm = this->rty->headMember();
            ExprPtr lhsp = wrapWithTy(qualtype(hm.type), new Proj(switchOf(a->args()[0], *this), hm.field, a->la()));
            return wrapWithTy(v->type(), new Assign(lhsp, switchOf(v->right(), *this), v->la()));
          }
        }
      }
    }

    return wrapWithTy(v->type(), new Assign(switchOf(v->left(), *this), switchOf(v->right(), *this), v->la()));
  }

  ExprPtr with(const Var* v) const override {
    if (hasConstraint(this->constraint, v->type())) {
      // replace safe functions with 'unsafe' ones
      if (v->value() == REF_REC_LABEL) {
        return wrapWithTy(v->type(), new Var(".recordHeadLabel", v->la()));
      } else if (v->value() == REF_REC_VALUE) {
        return wrapWithTy(v->type(), new Var(".recordHeadValue", v->la()));
      } else if (v->value() == REF_REC_TAIL || v->value() == REF_TUP_TAIL) {
        return wrapWithTy(v->type(), new Var(".recordTail", v->la()));
      } else {
        return wrapWithTy(v->type(), v->clone());
      }
    } else {
      return wrapWithTy(v->type(), v->clone());
    }
  }
};

ExprPtr RecordDeconstructor::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, RDUnqualify(cst));
}

}

