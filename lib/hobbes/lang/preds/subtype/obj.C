
#include <hobbes/lang/expr.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/preds/subtype/obj.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/os.H>
#include <hobbes/util/str.H>
#include <memory>

namespace hobbes {

void appendPath(PtrAdjustmentPath* p, const PtrAdjustmentPath& sfx);

#ifdef __clang__
// TODO -- figure out how to get class type info from clang
bool Objs::add(const std::type_info*) {
  return false;
}

bool Objs::pathExists(const std::string&, const std::string&) const {
  return false;
}

PtrAdjustmentPath Objs::adjustment(const std::string& derived, const std::string& base) const {
  throw std::runtime_error("No path exists from the class '" + derived + "' to '" + base + "'.");
}
#else
// object relationship recording and resolution
void Objs::add(const class_type* ct) {
  std::string cn = str::demangle(ct->name());
  
  ClassDefs::const_iterator cd = this->classDefs.find(cn);
  if (cd != this->classDefs.end()) {
    if (*cd->second != *ct) {
      throw std::runtime_error("While recording class type information, inconsistent definitions for the '" + cn + "' class.");
    }
  } else {
    this->classDefs[cn] = ct;

    // and recursively add parent classes
    if (const si_class_type* si = dynamic_cast<const si_class_type*>(ct)) {
      add(si->__base_type);
    } else if (const vmi_class_type* vmi = dynamic_cast<const vmi_class_type*>(ct)) {
      for (unsigned int i = 0; i < vmi->__base_count; ++i) {
        add(vmi->__base_info[i].__base_type);
      }
    }
  }
}

bool Objs::add(const std::type_info* ti) {
  if (const class_type* cti = dynamic_cast<const class_type*>(ti)) {
    add(cti);
    return true;
  } else if (const abi::__pointer_type_info* pti = dynamic_cast<const abi::__pointer_type_info*>(ti)) {
    return add(pti->__pointee);
  } else {
    return false;
  }
}

bool Objs::pathExists(const std::string& from, const std::string& to) const {
  // a path always exists from an object to itself
  if (from == to) {
    return true;
  }

  // try to find an immediate link, or else paths from all parent classes of 'from'
  ClassDefs::const_iterator cd = this->classDefs.find(from);
  if (cd == this->classDefs.end()) {
    return false;
  }

  if (const si_class_type* si = dynamic_cast<const si_class_type*>(cd->second)) {
    return pathExists(str::demangle(si->__base_type->name()), to);
  } else if (const vmi_class_type* vmi = dynamic_cast<const vmi_class_type*>(cd->second)) {
    for (unsigned int i = 0; i < vmi->__base_count; ++i) {
      if (pathExists(str::demangle(vmi->__base_info[i].__base_type->name()), to)) {
        return true;
      }
    }
    return false;
  } else {
    return false;
  }
}

PtrAdjustmentPath Objs::adjustment(const std::string& derived, const std::string& base) const {
  if (derived == base) {
    return PtrAdjustmentPath();
  } else {
    ClassDefs::const_iterator dc = this->classDefs.find(derived);
    if (dc == this->classDefs.end()) { throw std::runtime_error("Can't determine adjustment for unknown class: " + derived); }

    if (const si_class_type* si = dynamic_cast<const si_class_type*>(dc->second)) {
      std::string targetTy = str::demangle(si->__base_type->name());
      PtrAdjustmentPath result;
      result.push_back(PtrAdjustment::id(targetTy));
      appendPath(&result, adjustment(targetTy, base));
      return result;
    } else if (const vmi_class_type* vmi = dynamic_cast<const vmi_class_type*>(dc->second)) {
      for (unsigned int i = 0; i < vmi->__base_count; ++i) {
        try {
          std::string       targetTy = str::demangle(vmi->__base_info[i].__base_type->name());
          PtrAdjustmentPath suffix   = adjustment(targetTy, base);

          PtrAdjustmentPath result;
          if (vmi->__base_info[i].__is_virtual_p()) {
            result.push_back(PtrAdjustment::vtbl(vmi->__base_info[i].__offset(), targetTy));
          } else {
            result.push_back(PtrAdjustment::by(vmi->__base_info[i].__offset(), targetTy));
          }
          appendPath(&result, suffix);
          return result;
        } catch (std::exception&) {
          // oh, I guess it wasn't that one
        }
      }
    }
    throw std::runtime_error("No path exists from the class '" + derived + "' to '" + base + "'.");
  }
}
#endif

bool Objs::add(const std::type_info& ti) {
  return add(&ti);
}

bool Objs::isObjName(const std::string& tn) const {
  return this->classDefs.find(tn) != this->classDefs.end();
}

bool Objs::isObjType(const MonoTypePtr& mt) const {
  if (auto* op = is<OpaquePtr>(mt)) {
    return isObjName(op->name());
  } else {
    return false;
  }
}

std::string show(const PtrAdjustmentPath& p) {
  if (p.empty()) {
    return "[]";
  } else {
    std::string r = "[";
    r += p[0].show();
    for (size_t i = 1; i < p.size(); ++i) {
      r += ", ";
      r += p[i].show();
    }
    r += "]";
    return r;
  }
}

void appendPath(PtrAdjustmentPath* p, const PtrAdjustmentPath& sfx) {
  p->insert(p->end(), sfx.begin(), sfx.end());
}

PtrAdjustmentPath Objs::adjustment(const MonoTypePtr& derived, const MonoTypePtr& base) const {
  auto* d = is<OpaquePtr>(derived);
  auto* b = is<OpaquePtr>(base);

  if ((d == nullptr) || (b == nullptr)) {
    throw std::runtime_error("Expected class types for pointer adjustment determination, but received '" + show(derived) + " <: " + show(base));
  } else {
    return adjustment(d->name(), b->name());
  }
}

PtrAdjustmentPath Objs::adjustment(const ConstraintPtr& cst) const {
  Subtype isa;
  if (dec(cst, &isa)) {
    return adjustment(isa.lower, isa.greater);
  } else {
    throw std::runtime_error("Can't determine subclass pointer adjustment for constraint: " + show(cst));
  }
}

PolyTypePtr Objs::generalize(const MonoTypePtr& mt) const {
  // for now, just generalize function types
  Func* fty = is<Func>(mt);
  if (fty == nullptr) return polytype(mt);

  const MonoTypes&   fatys = fty->parameters();
  const MonoTypePtr& nfrty = fty->result();
  MonoTypes          nfatys;
  int                tvs = 0;
  Constraints        cs;

  for (const auto &faty : fatys) {
    if (isObjType(faty)) {
      MonoTypePtr tgv(TGen::make(tvs));
      nfatys.push_back(tgv);
      cs.push_back(std::make_shared<Constraint>(SubtypeUnqualifier::constraintName(), list(tgv, faty)));
      ++tvs;
    } else {
      nfatys.push_back(faty);
    }
  }

  return polytype(tvs, qualtype(cs, MonoTypePtr(Func::make(tuplety(nfatys), nfrty))));
}

bool Objs::refine(const TEnvPtr&, const MonoTypePtr&, const MonoTypePtr&, MonoTypeUnifier*) {
  // this should never be necessary
  return false;
}

bool Objs::satisfied(const TEnvPtr&, const MonoTypePtr& lhs, const MonoTypePtr& rhs) const {
  const OpaquePtr* derived = is<OpaquePtr>(lhs);
  const OpaquePtr* base    = is<OpaquePtr>(rhs);

  if ((derived != nullptr) && (base != nullptr)) {
    return pathExists(derived->name(), base->name());
  } else {
    return false;
  }
}

bool Objs::satisfiable(const TEnvPtr&, const MonoTypePtr& lhs, const MonoTypePtr& rhs) const {
  const OpaquePtr* derived = is<OpaquePtr>(lhs);
  const OpaquePtr* base    = is<OpaquePtr>(rhs);

  if ((derived != nullptr) && (base != nullptr)) {
    return pathExists(derived->name(), base->name());
  } else {
    return mayBeKnown(lhs) && mayBeKnown(rhs);
  }
}

bool Objs::mayBeKnown(const MonoTypePtr& mt) const {
  if (auto* op = is<OpaquePtr>(mt)) {
    // this is a specific C++ type name
    // we can directly tell if it's known
    return this->classDefs.find(op->name()) != this->classDefs.end();
  } else {
    // this type may only be known if it's a type-variable or polytype-arg
    return (is<TGen>(mt) != nullptr) || (is<TVar>(mt) != nullptr);
  }
}

ExprPtr applyVtblAdjustment(const ExprPtr& e, int o, const std::string& targetTy) {
  MonoTypePtr tty(OpaquePtr::make(targetTy, 8, false));
  ExprPtr adj(new Var(".adjustVtblPtr", e->la()));
  adj->type(qualtype(MonoTypePtr(Func::make(tuplety(list(e->type()->monoType(), MonoTypePtr(Prim::make("int")))), tty))));
  ExprPtr off(new Int(o, e->la()));
  off->type(qualtype(MonoTypePtr(Prim::make("int"))));
  ExprPtr ap(new App(adj, list(e, off), e->la()));
  ap->type(qualtype(tty));
  ExprPtr apwty = assume(ap, tty, e->la());
  apwty->type(qualtype(tty));
  return apwty;
}

ExprPtr applyOffsetAdjustment(const ExprPtr& e, int o, const std::string& targetTy) {
  MonoTypePtr tty(OpaquePtr::make(targetTy, 8, false));
  ExprPtr adj(new Var(".adjustPtr", e->la()));
  adj->type(qualtype(MonoTypePtr(Func::make(tuplety(list(e->type()->monoType(), MonoTypePtr(Prim::make("int")))), tty))));
  ExprPtr off(new Int(o, e->la()));
  off->type(qualtype(MonoTypePtr(Prim::make("int"))));
  ExprPtr ap(new App(adj, list(e, off), e->la()));
  ap->type(qualtype(tty));
  ExprPtr apwty = assume(ap, tty, e->la());
  apwty->type(qualtype(tty));
  return apwty;
}

ExprPtr applyAdjustment(const PtrAdjustment& p, const ExprPtr& e) {
  if (p.vtblLookup) {
    return applyVtblAdjustment(e, p.offset, p.targetTy);
  } else if (p.offset > 0) {
    return applyOffsetAdjustment(e, p.offset, p.targetTy);
  } else {
    return e;
  }
}

ExprPtr applyAdjustments(const PtrAdjustmentPath& p, const ExprPtr& e) {
  ExprPtr r = e;
  for (const auto &i : p) {
    r = applyAdjustment(i, r);
  }
  return r;
}

struct ObjUnqualify : public switchExprTyFn {
  const Objs*          objs;
  const TEnvPtr&       tenv;
  const ConstraintPtr& constraint;
  Subtype              isa;
  Definitions*         ds;

  ObjUnqualify(const Objs* objs, const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) : objs(objs), tenv(tenv), constraint(cst), ds(ds) {
    if (!dec(cst, &this->isa)) {
      throw std::runtime_error("Cannot unqualify subtyping with unknown constraint: " + show(cst));
    }
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const override {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }

  ExprPtr with(const Var* v) const override {
    if (!hasConstraint(this->constraint, v->type())) {
      return wrapWithTy(v->type(), v->clone());
    } else if (const Func* fty = is<Func>(v->type()->monoType())) {
      ExprPtr newFn(v->clone());
      newFn->type(v->type());

      auto ts = fty->parameters();
      auto ns = freshNames(ts.size());
      Exprs args;
      for (size_t i = 0; i < ns.size(); ++i) {
        ExprPtr arg(new Var(ns[i], v->la()));
        arg->type(qualtype(ts[i]));
        args.push_back(arg);
      }
      auto body = ExprPtr(new App(newFn, args, v->la()));
      body->type(qualtype(list(this->constraint), fty->result()));
      auto result = ExprPtr(new Fn(ns, body, v->la()));
      result->type(v->type());
      return switchOf(result, *this);
    } else {
      return wrapWithTy(v->type(), v->clone());
    }
  }

  ExprPtr with(const Fn* v) const override {
    const Func* fty = is<Func>(v->type()->monoType());
    if (fty == nullptr) {
      throw std::runtime_error("Internal error, expected annotated function type");
    }
    return wrapWithTy(v->type(),
      new Fn(
        v->varNames(), 
        switchOf(v->body(), ObjUnqualify(this->objs, fnFrame(this->tenv, v->varNames(), fty->parameters()), this->constraint, this->ds)),
        v->la()
      )
    );
  }
  
  ExprPtr with(const App* v) const override {
    if (hasConstraint(this->constraint, v->fn()->type())) {
      ExprPtr f    = is<Var>(v->fn()) != nullptr ? wrapWithTy(v->fn()->type(), v->fn()->clone()) : switchOf(v->fn(), *this);
      Exprs   args = switchOf(v->args(), *this);
      Idxs    ixs  = coercionIndexes(this->tenv, v->fn(), this->isa.greater, this->ds);

      for (const unsigned long ix : ixs) {
        if (*args[ix]->type()->monoType() == *this->isa.lower) {
          args[ix] = applyAdjustments(this->objs->adjustment(this->constraint), args[ix]);
        }
      }

      return wrapWithTy(v->type(), new App(f, args, v->la()));
    } else {
      return wrapWithTy(v->type(), new App(v->fn(), switchOf(v->args(), *this), v->la()));
    }
  }

  using Idxs = std::vector<size_t>;

  static Idxs coercionIndexes(const TEnvPtr& tenv, const ExprPtr& fe, const MonoTypePtr& convTo, Definitions* ds) {
    QualTypePtr fty  = validateType(tenv, fe, ds)->type();
    const Func* ffty = is<Func>(fty->monoType());
    if (ffty == nullptr) return Idxs();

    MonoTypes ctys = convFroms(fty->constraints(), convTo);
    if (ctys.empty()) return Idxs();

    Idxs result;
    const MonoTypes& atys = ffty->parameters();
    for (size_t i = 0; i < atys.size(); ++i) {
      for (const auto &cty : ctys) {
        if (*atys[i] == *cty) {
          result.push_back(i);
        }
      }
    }
    return result;
  }

  static MonoTypes convFroms(const Constraints& cs, const MonoTypePtr& convTo) {
    MonoTypes result;
    for (const auto &c : cs) {
      Subtype sc;
      if (dec(c, &sc)) {
        if (*sc.greater == *convTo) {
          result.push_back(sc.lower);
        }
      }
    }
    return result;
  }
};

ExprPtr Objs::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  return switchOf(e, ObjUnqualify(this, tenv, cst, ds));
}

PolyTypePtr Objs::lookup(const std::string&) const {
  return PolyTypePtr(); // nothing to see here
}

SymSet Objs::bindings() const {
  return SymSet(); // we don't actually overload any symbols
}

}

