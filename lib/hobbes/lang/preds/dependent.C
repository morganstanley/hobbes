#include <hobbes/reflect.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/preds/dependent.H>
#include <hobbes/eval/funcdefs.H>

namespace hobbes{

static bool tvalueToMonoTypePtr(const TEnvPtr& tenv, Definitions* ds, const MonoTypePtr& in, MonoTypePtr& out) {    
  if (is<TString>(in)) {
    out = arrayty(primty("char"));
    return true;
  }      
  if (is<TLong>(in)) {
    out = primty("long");
    return true;
  }
  if (TApp *tapp = is<TApp>(in)) {
    if (*tapp->fn() == *primty("quote")) {
      if (TExpr* e = is<TExpr>(tapp->args()[0])) {
        out = validateType(tenv, e->expr(), ds)->type()->monoType();
        return true;
      }
    }
  }
  return false; 
}


struct MonoTypePtrToExpr : public switchType<ExprPtr> {
  TEnvPtr tenv;
  MonoTypePtr type;
  LexicalAnnotation la;
  MonoTypePtrToExpr(const TEnvPtr tenv, const MonoTypePtr& type) : tenv(tenv), type(type) {}

  ExprPtr roll(ExprPtr e) const {
    return fncall(var("roll", la), list(e), la);
  }

  ExprPtr ret(const char * name, ExprPtr e) const {
    ExprPtr o = fncall(var("roll", la), list(ExprPtr(new MkVariant(name, e, la))), la);
    return ExprPtr(new Assump(o, qualtype(type), la));;
  }

  ExprPtr with(const TVar* tv) const {
    return ret("TVar", ExprPtr(mkarray(tv->name(), la)));
  }

  ExprPtr with(const TGen*) const {
    // I don't think we can get one of these here, and we don't want it.
    throw std::runtime_error("TGen in TypeApply");
  }

  ExprPtr with(const TExpr *) const {
    // We can't put an arbitrary value in a Type, can we do something else?
    throw std::runtime_error("TExpr in TypeApply");
  }

  ExprPtr with(const Prim* prim) const {
    ExprPtr name(mkarray(prim->name(), la));
    ExprPtr rep;
    if (prim->representation()) {
      rep = ExprPtr(new MkVariant(".f1", switchOf(prim->representation(), *this), la));
    } else {
      rep = var("nothing", la);
    }

    MkRecord::FieldDefs fs;
    fs.push_back(MkRecord::FieldDef(".f0", name));
    fs.push_back(MkRecord::FieldDef(".f1", rep));
    return ret("Prim", mkrecord(fs, la));
  } 

  ExprPtr with(const Record* r) const {
    Exprs entries;
    for(auto m : r->members()) {
      MkRecord::FieldDefs fs;
      fs.push_back(MkRecord::FieldDef(".f0", ExprPtr(mkarray(m.field, la))));
      fs.push_back(MkRecord::FieldDef(".f1", switchOf(m.type, *this)));
      entries.push_back(mkrecord(fs, la));
    }
    return ret("Record", ExprPtr(new MkArray(entries, la)));
  }

  ExprPtr with(const Variant* v) const {
    Exprs entries;
    for(auto m : v->members()) {
      MkRecord::FieldDefs fs;
      fs.push_back(MkRecord::FieldDef(".f0", ExprPtr(mkarray(m.selector, la))));
      fs.push_back(MkRecord::FieldDef(".f1", switchOf(m.type, *this)));
      fs.push_back(MkRecord::FieldDef(".f2", constant(static_cast<int>(m.id), la)));
      entries.push_back(mkrecord(fs, la));
    }
    return ret("Variant", ExprPtr(new MkArray(entries, la)));
  }

  ExprPtr with(const OpaquePtr* op) const {
    MkRecord::FieldDefs fs;
    fs.push_back(MkRecord::FieldDef(".f0", ExprPtr(mkarray(op->name(), la))));
    fs.push_back(MkRecord::FieldDef(".f1", constant(static_cast<int>(op->size()), la)));
    fs.push_back(MkRecord::FieldDef(".f2", constant(op->storedContiguously(), la)));
    return ret("OpaquePtr", mkrecord(fs, la));
  }

  ExprPtr with(const Exists *e) const {
    ExprPtr name = ExprPtr(mkarray(e->absTypeName(), la));
    ExprPtr type = switchOf(e->absType(), *this);
    MkRecord::FieldDefs fs;
    fs.push_back(MkRecord::FieldDef(".f0", name));
    fs.push_back(MkRecord::FieldDef(".f1", type));
    return ret("Exists", mkrecord(fs, la));
  }

  ExprPtr with(const Recursive *e) const {
    ExprPtr name = ExprPtr(mkarray(e->recTypeName(), la));
    ExprPtr type = switchOf(e->recType(), *this);
    MkRecord::FieldDefs fs;
    fs.push_back(MkRecord::FieldDef(".f0", name));
    fs.push_back(MkRecord::FieldDef(".f1", type));
    return ret("Recursive", mkrecord(fs, la));
  }

  ExprPtr with(const TAbs* tabs) const {
    Exprs args;
    args.resize(tabs->args().size());
    for(auto s : tabs->args()) {
      args.push_back(ExprPtr(mkarray(s, la)));
    }
    ExprPtr arge(new MkArray(args, la));
    ExprPtr body = switchOf(tabs->body(), *this);
    MkRecord::FieldDefs fs;
    fs.push_back(MkRecord::FieldDef(".f0", arge));
    fs.push_back(MkRecord::FieldDef(".f1", body));
    return ret("TAbs", mkrecord(fs, la));
  }

  ExprPtr with(const TApp* tapp) const {

    ExprPtr fn = switchOf(tapp->fn(), *this);
    Exprs args;
    for(auto t : tapp->args()) {
      args.push_back(switchOf(t, *this));
    }
    MkRecord::FieldDefs fs;
    fs.push_back(MkRecord::FieldDef(".f0", fn));
    fs.push_back(MkRecord::FieldDef(".f1", ExprPtr(new MkArray(args, la))));
    return ret("TApp", mkrecord(fs, la));
  }

  ExprPtr with(const Func* v) const {
    Exprs ps;
    for(auto p : v->parameters()) {
      ps.push_back(switchOf(p, *this));
    }
    ExprPtr r = switchOf(v->result(), *this);
    MkRecord::FieldDefs fs;
    fs.push_back(MkRecord::FieldDef(".f0", ExprPtr(new MkArray(ps, la))));
    fs.push_back(MkRecord::FieldDef(".f1", r));
    return ret("Func", mkrecord(fs, la));
  }

  ExprPtr with(const Array* ar) const {
    return ret("Array", switchOf(ar->type(), *this));
  }

  ExprPtr with(const FixedArray* ar) const {
    ExprPtr l = switchOf(ar->length(), *this);
    ExprPtr t = switchOf(ar->type(), *this);
    MkRecord::FieldDefs fs;
    fs.push_back(MkRecord::FieldDef(".f0", t));
    fs.push_back(MkRecord::FieldDef(".f1", l));
    return ret("FixedArray", mkrecord(fs, la));

  }

  ExprPtr with(const TString* tstr) const {
    return ret("TString", ExprPtr(mkarray(tstr->value(), la)));
  }

  ExprPtr with(const TLong* tlong) const {
    return ret("TLong", constant(tlong->value(), la));
  }

};


struct Type;
MonoTypePtr typeExprToMonoTypePtr(Type*);

struct _Prim {
  typedef std::pair<array<char>*, variant<unit, Type*>> type;
  static MonoTypePtr build(_Prim::type& p){
    MonoTypePtr aty;
    if (Type** at = p.second.get<Type*>()) {
      aty = typeExprToMonoTypePtr(*at);
    }
    return primty(makeStdString(p.first).c_str(), aty);
  }
};

struct _Record {
  typedef array<std::pair<array<char>*, Type*>>* type;
  static MonoTypePtr build(_Record::type& rec) {
    Record::Members mems;
    for(unsigned int i = 0; i < rec->size; ++i) {
      mems.push_back(Record::Member(makeStdString(rec->data[i].first), typeExprToMonoTypePtr(rec->data[i].second)));
    }
    return Record::make(mems);
  }
};

struct _Variant {
  typedef array<tuple<array<char>*, Type*, unsigned int>>* type;
  static MonoTypePtr build(_Variant::type& rec) {
    Variant::Members mems;
    for(unsigned int i = 0; i < rec->size; ++i) {
      mems.push_back(Variant::Member(makeStdString(rec->data[i].at<0>()), typeExprToMonoTypePtr(rec->data[i].at<1>()), rec->data[i].at<2>()));
    }
    return Variant::make(mems);
  }
};

struct _OpaquePtr {
  typedef tuple<array<char>*, int, bool> type;
  static MonoTypePtr build(_OpaquePtr::type& op) {
    return OpaquePtr::make(makeStdString(op.at<0>()), op.at<1>(), op.at<2>());
  }
};

struct _Exists {
  typedef std::pair<array<char>*, Type*> type;
  static MonoTypePtr build(_Exists::type& ex) {
    return Exists::make(makeStdString(ex.first), typeExprToMonoTypePtr(ex.second));
  }
};

struct _Recursive {
  typedef std::pair<array<char>*, Type*> type;
  static MonoTypePtr build(_Recursive::type& ex) {
    return Recursive::make(makeStdString(ex.first), typeExprToMonoTypePtr(ex.second));
  }
};

struct _TAbs {
  typedef std::pair<array<array<char>*>*, Type*> type;
  static MonoTypePtr build(_TAbs::type& tabs) {
    str::seq s;
    for(unsigned int i = 0; i < tabs.first->size; ++i) {
      s.push_back(makeStdString(tabs.first->data[i]));
    }
    return TAbs::make(s, typeExprToMonoTypePtr(tabs.second));
  }
};

struct _TApp {
  typedef std::pair<Type*, array<Type*>*> type;
  static MonoTypePtr build(_TApp::type& tapp) {
    MonoTypes ms;
    for(unsigned int i = 0; i < tapp.second->size; ++i) {
      ms.push_back(typeExprToMonoTypePtr(tapp.second->data[i]));
    }
    return TApp::make(typeExprToMonoTypePtr(tapp.first), ms);
  }
};

struct _Func {
  typedef std::pair<array<Type*>*, Type*> type;
  static MonoTypePtr build(_Func::type& f) {
    Record::Members mems;
    for(unsigned int i = 0; i < f.first->size; ++i) {
      mems.push_back(Record::Member(".f" + str::from(i), typeExprToMonoTypePtr(f.first->data[i])));
    }
    return Func::make(Record::make(mems), typeExprToMonoTypePtr(f.second));
  }
};

struct _Array {
  typedef Type* type;
  static MonoTypePtr build(_Array::type& ar) {
    return Array::make(typeExprToMonoTypePtr(ar));
  }
};

struct _FixedArray {
  typedef std::pair<Type*, Type*> type;
  static MonoTypePtr build(_FixedArray::type& far) {
    return FixedArray::make(typeExprToMonoTypePtr(far.first), typeExprToMonoTypePtr(far.second));
  }
};

struct _TString {
  typedef array<char>* type;
  static MonoTypePtr build(_TString::type& str) {
    return TString::make(std::string(str->data, str->data + str->size));
  }
};

struct _TLong {
  typedef long type;
  static MonoTypePtr build(_TLong::type& l) {
    return TLong::make(l);
  }
};

struct _TVar {
  typedef array<char>* type;
  static MonoTypePtr build(_TString::type& str) {
    return TVar::make(std::string(str->data, str->data + str->size));
  }
};

struct Type {
  typedef variant<_Prim::type, _Record::type, _Variant::type, _OpaquePtr::type, _Exists::type, _Recursive::type, _TAbs::type, _TApp::type, _Func::type, _Array::type, _FixedArray::type, _TString, _TLong, _TVar> def;
  def data;
};

#define BUILD(x) x ::build(*reinterpret_cast< x ::type*>(t->data.unsafePayload()))
MonoTypePtr typeExprToMonoTypePtr(Type* t){
  switch (t->data.unsafeTag()) {
    case 0:
      return BUILD(_Prim);
    case 1:
      return BUILD(_Record);
    case 2:
      return BUILD(_Variant);
    case 3:
      return BUILD(_OpaquePtr);
    case 4:
      return BUILD(_Exists);
    case 5:
      return BUILD(_Recursive);
    case 6:
      return BUILD(_TAbs);
    case 7:
      return BUILD(_TApp);
    case 8:
      return BUILD(_Func);
    case 9:
      return BUILD(_Array);
    case 10:
      return BUILD(_FixedArray);
    case 11:
      return BUILD(_TString);
    case 12:
      return BUILD(_TLong);
    case 13:
      return BUILD(_TVar);
  }
  return MonoTypePtr();
}


template <>
  struct lift<Type*, false> {
    static MonoTypePtr type(typedb& tenv) {
      return tenv.replaceTypeAliases(primty("Type"));
    }
  };

bool evalType(cc* ctx, const TEnvPtr& tenv, Definitions* ds, MonoTypePtr& tptr, ExprPtr ex, MonoTypePtr &r) {
  try{
    ExprPtr e = validateType(tenv, ex, ds);
    if (!e) return false;
    if (*e->type()->monoType() == *primty("long")) {
      r = MonoTypePtr(TLong::make(ctx->compileFn<long()>(e)()));
      return true;
    } else if (*e->type()->monoType() == *arrayty(primty("char"))) {
      array<char> * hs = ctx->compileFn<array<char>*()>(e)();
      r = MonoTypePtr(TString::make(std::string(hs->data, hs->data + hs->size)));
      return true;
    } else if (*e->type()->monoType() == *tptr) {     
      Type* te = ctx->compileFn<Type*()>(e)();
      r = typeExprToMonoTypePtr(te);
      return true;
    } else {
      return false;
    }
  } catch(...) { return false; }
}


bool decodeTAConstraint(cc* ctx, const TEnvPtr tenv, Definitions* ds, const ConstraintPtr& c, MonoTypePtr& l, MonoTypePtr& r) {
  MonoTypePtr tptr = ctx->replaceTypeAliases(primty("Type"));
  LexicalAnnotation la;
  if (c->name() == TypeApply::constraintName() && c->arguments().size() > 1) {
    l = c->arguments()[0];
    if (TApp * tapp = is<TApp>(c->arguments()[1])) {
      if(tapp->args().size() == 1 && *tapp->fn() == *primty("quote")) {
        if (TExpr* te = is<TExpr>(tapp->args()[0])) {
          ExprPtr f = te->expr();
          Exprs args;

          for(unsigned int i = 2; i < c->arguments().size(); ++i) {
            ExprPtr arg;
            MonoTypePtr targ = c->arguments()[i];
            if (is<TVar>(targ)) {
              return false;
            }
            if (TApp* tapp = is<TApp>(targ)) {
              if (*tapp->fn() == *primty("quote")) {
                if (TExpr* e = is<TExpr>(tapp->args()[0])) {
                  arg = e->expr();
                }
              }
            }
            if (TString* tstr = is<TString>(targ)) {
              arg = ExprPtr(mkarray(tstr->value(), la));
            }
            if (TLong* tlong = is<TLong>(targ)) {
              arg = constant(tlong->value(), la);
            }

            if(!arg) {arg = switchOf(targ, MonoTypePtrToExpr(tenv, tptr));}
            if (arg) {
              args.push_back(arg);
            } else {
              return false;
            }
          }

          if(evalType(ctx, tenv, ds, tptr, fncall(f, args, la), r)) {
            return true;
          }
          // This doesn't seem to work how I'd hoped, closures don't get eval'ed
          // properly.
          if(evalType(ctx, tenv, ds, tptr, closcall(f, args, la), r)) {
            return true;
          }
          return false;
        }
      }
    }
  }
  return false;
}


std::string TypeApply::constraintName() {
  return "TypeApply";
}

bool TypeApply::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions* ds) {
  MonoTypePtr l, r;
  if(decodeTAConstraint(this->c, tenv, ds, cst, l, r)){
    size_t uc = u->size();
    mgu(l, r, u);
    bool ret= uc != u->size();
    return ret;
  }
  return false;
}

struct TAUnqualify : public switchExprTyFn {
  ConstraintPtr c;
  TAUnqualify(const ConstraintPtr& c) : c(c) { }

  QualTypePtr withTy(const QualTypePtr& qt) const {
    return removeConstraint(this->c, qt);
  }

};

ExprPtr TypeApply::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, TAUnqualify(cst));
}

bool TypeApply::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  MonoTypePtr l, r;
  if(decodeTAConstraint(this->c, tenv, ds, cst, l, r)){
    bool ret = *l == *r;
    return ret;
  }
  return false;
}

bool TypeApply::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  for(auto a : cst->arguments()) {
    if (is<TVar>(a)) return true;
  }
  return satisfied(tenv, cst, ds);
}

void TypeApply::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

PolyTypePtr TypeApply::lookup(const std::string&) const {
  return PolyTypePtr();
}

SymSet TypeApply::bindings() const {
  return SymSet();
}

FunDeps TypeApply::dependencies(const ConstraintPtr&) const {
  return FunDeps();
}



static bool decodeTVLConstraint(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions *ds, MonoTypePtr* l, MonoTypePtr* r) {
  if (c->name() == TypeValueLower::constraintName() && c->arguments().size() == 2) {
    *r = c->arguments()[1];
    return tvalueToMonoTypePtr(tenv, ds, c->arguments()[0], *l);
  }
  return false;
}

std::string TypeValueLower::constraintName() {
  return "TypeValueLower";
}

bool TypeValueLower::refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions* ds) {
  MonoTypePtr l, r;
  if(decodeTVLConstraint(tenv, cst, ds, &l, &r)){
    size_t uc = u->size();
    mgu(l, r, u);
    return uc != u->size();
  }
  return false;
}

struct TVLUnqualify : public switchExprTyFn {
  ConstraintPtr c;
  TVLUnqualify(const ConstraintPtr& c) : c(c) { }

  QualTypePtr withTy(const QualTypePtr& qt) const {
    return removeConstraint(this->c, qt);
  }

  ExprPtr with(const Var* vn) const {
    if (vn->value() == "typeValueLower") {
      return wrapWithTy(vn->type(), new Var(".typeValueLower", vn->la()));
    } else {
      return wrapWithTy(vn->type(), new Var(vn->value(), vn->la()));
    }
  }
};

ExprPtr TypeValueLower::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, TVLUnqualify(cst));
}

bool TypeValueLower::satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  MonoTypePtr l, r;
  if(decodeTVLConstraint(tenv, cst, ds, &l, &r)){
    return *l == *r;
  }
  return false;
}

bool TypeValueLower::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  if (is<TVar>(cst->arguments()[0])) {
    return true;
  }
  MonoTypePtr l, r;
  if(decodeTVLConstraint(tenv, cst, ds, &l, &r)){
    return unifiable(tenv, l, r);
  }
  return false;
}

void TypeValueLower::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

PolyTypePtr TypeValueLower::lookup(const std::string& vn) const {
  if (vn == "typeValueLower") {
    return polytype(2, qualtype(list(ConstraintPtr(new Constraint(constraintName(), list(tgen(0), tgen(1))))), functy(list(tgen(0)), tgen(1))));
  } else {
    return PolyTypePtr();
  }
}

SymSet TypeValueLower::bindings() const {
  SymSet r;
  r.insert("typeValueLower");
  return r;
}

FunDeps TypeValueLower::dependencies(const ConstraintPtr&) const {
  FunDeps result;
  return result;
}
}
