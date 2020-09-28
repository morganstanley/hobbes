
#include <hobbes/eval/cc.H>
#include <hobbes/eval/cmodule.H>
#include <hobbes/lang/module.H>
#include <hobbes/lang/type.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/util/str.H>
#include <hobbes/util/array.H>
#include <stdexcept>
#include <dlfcn.h>
#include <glob.h>

namespace hobbes {

bool fileExists(const std::string& fname) {
  // not the most elegant, but it does the job
  FILE* f = fopen(fname.c_str(), "r");
  if (!f) return false;
  fclose(f);
  return true;
}

bool importObject(cc* e, const std::string& sopath) {
  if (!fileExists(sopath)) {
    return false;
  } else {
    void* h = dlopen(sopath.c_str(), RTLD_NOW);
    if (!h) { throw std::runtime_error(std::string("Failed to load .so file: ") + dlerror()); }

    typedef void (*InitF)(cc*);
    InitF initF = reinterpret_cast<InitF>(dlsym(h, "initialize"));

    if (!initF) { dlclose(h); throw std::runtime_error(std::string("Failed to load .so file init: ") + dlerror()); }

    initF(e);
    return true;
  }
}

bool importScript(cc* e, const std::string& fname) {
  if (!fileExists(fname)) {
    return false;
  } else {
    compile(e, e->readModuleFile(fname));
    return true;
  }
}

typedef std::vector<std::string> ModulePaths;
static ModulePaths& modulePaths() {
  static thread_local ModulePaths mps;
  if (mps.size()==0) {
    mps.push_back(".");
  }
  return mps;
}
void pushModuleDir(const std::string& d) { modulePaths().push_back(d); }
void popModuleDir() { if (modulePaths().size() > 0) modulePaths().resize(modulePaths().size()-1); }

// import a "module" from a path spec (A.B.C => [.|$MODULEPATH]/A/B/C.*)
void import(cc* e, const std::string& mname) {
  for (size_t p = modulePaths().size(); p > 0; --p) {
    std::string path  = modulePaths()[p-1];
    std::string mpath = (path.empty() ? "." : path) + "/" + str::replace<char>(mname, ".", "/");

    if (importObject(e, mpath + ".so")) {
      return;
    } else {
      for (const auto& p : str::paths(mpath + ".*")) {
        if (importScript(e, p)) {
          return;
        }
      }
    }
  }
  throw std::runtime_error("No such module to load: " + mname);
}

// replace type variable references with expanded aliases or opaque definitions as necessary
ExprPtr applyTypeDefns(const ModulePtr&, cc*, const ExprPtr&);

struct appTyDefnF : public switchTyFn {
  ModulePtr m;
  cc* e;
  appTyDefnF(const ModulePtr& m, cc* e) : m(m), e(e) { }
  MonoTypePtr with(const TVar* v) const {
    const auto& tn = v->name();

    if (isPrimName(tn) || e->isTypeAliasName(tn)) {
      return e->replaceTypeAliases(Prim::make(tn));
    } else if (e->isTypeName(tn)) {
      return Prim::make(tn, e->namedTypeRepresentation(tn));
    } else {
      return TVar::make(tn);
    }
  }

  MonoTypePtr with(const TApp* ap) const {
    return e->replaceTypeAliases(TApp::make(switchOf(ap->fn(), *this), switchOf(ap->args(), *this)));
  }

  MonoTypePtr with(const TExpr* x) const {
    return TExpr::make(applyTypeDefns(this->m, this->e, translateExprWithOpts(m->options(), x->expr())));
  }
};
MonoTypePtr applyTypeDefns(const ModulePtr& m, cc* e, const MonoTypePtr& t) {
  auto ua = e->unappTyDefns.find(t.get());
  if (ua != e->unappTyDefns.end()) return ua->second;

  MonoTypePtr r = switchOf(t, appTyDefnF(m, e));
  e->unappTyDefns[t.get()] = r;
  return r;
}

MonoTypes applyTypeDefns(const ModulePtr& m, cc* e, const MonoTypes& ts) {
  MonoTypes r;
  for (const auto& t : ts) {
    r.push_back(applyTypeDefns(m, e, t));
  }
  return r;
}

QualTypePtr applyTypeDefns(const ModulePtr& m, cc* e, const QualTypePtr& t) {
  Constraints cs;
  for (const auto& c : t->constraints()) {
    cs.push_back(ConstraintPtr(new Constraint(c->name(), applyTypeDefns(m, e, c->arguments()))));
  }
  return QualTypePtr(new QualType(cs, applyTypeDefns(m, e, t->monoType())));
}

struct appTyDefnEF : public switchExprTyFn {
  ModulePtr m;
  cc* e;
  appTyDefnEF(const ModulePtr& m, cc* e) : m(m), e(e) { }
  QualTypePtr withTy(const QualTypePtr& t) const { if (t) return applyTypeDefns(this->m, this->e, t); else return t; }
};
ExprPtr applyTypeDefns(const ModulePtr& m, cc* e, const ExprPtr& x) {
  return switchOf(x, appTyDefnEF(m, e));
}

struct appTyDefnMF : public switchMDefTyFn {
  ModulePtr m;
  cc* e;
  appTyDefnMF(const ModulePtr& m, cc* e) : m(m), e(e) { }
  QualTypePtr withTy(const QualTypePtr& t) const { if (t) return applyTypeDefns(this->m, this->e, t); else return t; }
};
ModuleDefPtr applyTypeDefns(const ModulePtr& m, cc* e, const ModuleDefPtr& md) {
  return switchOf(md, appTyDefnMF(m, e));
}

ModuleDefs applyTypeDefns(const ModulePtr& m, cc* e, const ModuleDefs& mds) {
  ModuleDefs r;
  for (const auto& md : mds) {
    r.push_back(applyTypeDefns(m, e, md));
  }
  return r;
}

ModulePtr applyTypeDefns(cc* e, const ModulePtr& m) {
  return ModulePtr(new Module(m->name(), applyTypeDefns(m, e, m->definitions())));
}

// index type variables and sanity check names to ensure no duplicates
typedef std::map<std::string, int> NameIndexing;

NameIndexing nameIndexing(const str::seq& ns) {
  NameIndexing r;
  for (size_t i = 0; i < ns.size(); ++i) {
    if (r.find(ns[i]) != r.end()) {
      throw std::runtime_error("Duplicate name '" + ns[i] + "'");
    } else {
      r[ns[i]] = i;
    }
  }
  return r;
}

NameIndexing nameIndexing(const std::set<std::string>& ns) {
  return nameIndexing(str::seq(ns.begin(), ns.end()));
}

int nameIndex(const NameIndexing& ns, const std::string& vn) {
  NameIndexing::const_iterator ni = ns.find(vn);
  if (ni == ns.end()) {
    throw std::runtime_error("Undefined type name, '" + vn + "'");
  } else {
    return ni->second;
  }
}

std::vector<int> nameIndex(const NameIndexing& ns, const str::seq& vns) {
  std::vector<int> r;
  for (str::seq::const_iterator vn = vns.begin(); vn != vns.end(); ++vn) {
    r.push_back(nameIndex(ns, *vn));
  }
  return r;
}

MonoTypeSubst substitution(const NameIndexing& ns) {
  MonoTypeSubst s;
  for (NameIndexing::const_iterator ni = ns.begin(); ni != ns.end(); ++ni) {
    s[ni->first] = MonoTypePtr(TGen::make(ni->second));
  }
  return s;
}

MonoTypeSubst uvarSubstitution(const NameIndexing& ns) {
  MonoTypeSubst s;
  for (NameIndexing::const_iterator ni = ns.begin(); ni != ns.end(); ++ni) {
    s[ni->first] = freshTypeVar();
  }
  return s;
}

// convert functional dependencies into index form
void resolveNames(const NameIndexing& ns, const CFunDepDef& nfdep, FunDeps* out) {
  VarIDs lhs = nameIndex(ns, nfdep.first);
  for (str::seq::const_iterator vn = nfdep.second.begin(); vn != nfdep.second.end(); ++vn) {
    out->push_back(FunDep(lhs, nameIndex(ns, *vn)));
  }
}

FunDeps resolveNames(const NameIndexing& ns, const CFunDepDefs& nfdeps) {
  FunDeps r;
  for (CFunDepDefs::const_iterator fd = nfdeps.begin(); fd != nfdeps.end(); ++fd) {
    resolveNames(ns, *fd, &r);
  }
  return r;
}

// convert member definitions
TClass::Members resolveMembers(const MonoTypeSubst& s, const MVarTypeDefs& mvtds) {
  TClass::Members r;
  for (const auto& mvtd : mvtds) {
    if (r.find(mvtd->varName()) != r.end()) {
      throw annotated_error(*mvtd, "Duplicate class member name, '" + mvtd->varName() + "'");
    } else {
      r[mvtd->varName()] = requireMonotype(substitute(s, mvtd->varType()));
    }
  }
  return r;
}

// make a type class
void compile(const ModulePtr&, cc* e, const ClassDef* cd) {
  try {
    NameIndexing    tns  = nameIndexing(cd->vars());
    MonoTypeSubst   s    = substitution(tns);
    Constraints     reqs = substitute(s, cd->constraints());
    FunDeps         fds  = resolveNames(tns, cd->fundeps());
    TClass::Members mems = resolveMembers(s, cd->members());

    e->typeEnv()->bind(cd->name(), UnqualifierPtr(new TClass(reqs, cd->name(), tns.size(), mems, mergeFundeps(inferFundeps(e->typeEnv(), reqs), fds), cd->la())));
  } catch (std::exception& ex) {
    throw annotated_error(*cd, ex.what());
  }
}

// compile class instance member definitions
MemberMapping compileMembers(const ModulePtr& m, MonoTypeUnifier* u, const TClassPtr& c, const MonoTypes& targs, cc* e, const MVarDefs& ds, bool asfn) {
  // Class X => Instance Y, X unify Y applied to class member types should yield instance member types
  MonoTypes cargs = freshTypeVars(c->typeVars()); 
  mgu(targs, cargs, u);

  // compile each member symbol binding
  MemberMapping ms;
  for (const auto& d : ds) {
    std::string n = d->varWithArgs()[0];

    if (ms.find(n) != ms.end()) {
      throw annotated_error(*d, "Duplicate instance member name, '" + n + "'");
    }

    // generate the expression for the member definition
    bool gendef = d->varWithArgs().size() > 1;
    ExprPtr mexp;
    if (gendef) {
      Fn::VarNames vns(d->varWithArgs().begin() + 1, d->varWithArgs().end());
      mexp = ExprPtr(new Fn(vns, d->varExpr(), d->la()));
    } else {
      mexp = d->varExpr();
    }
    mexp = translateExprWithOpts(m, mexp);

    // determine how to store this member depending on whether we're making an instance function,
    // or if we're generating a new function for a ground instance
    if (asfn) {
      ms[n] = mexp;
    } else {
      MonoTypePtr expectedMemberType = instantiate(u->substitute(cargs), c->memberType(n));

      if (!gendef) {
        ms[n] = e->normalize(assume(mexp, expectedMemberType, mexp->la()));
      } else {
        std::string fn = "." + n + freshName();
        e->define(fn, assume(mexp, expectedMemberType, mexp->la()));
        ms[n] = e->normalize(ExprPtr(new Var(fn, mexp->la())));
      }
    }
  }
  return ms;
}

// make a type class instance
void compile(const ModulePtr& m, cc* e, const InstanceDef* id) {
  try {
    UnqualifierPtr tyc = e->typeEnv()->lookupUnqualifier(id->className());
    TClassPtr      c   = std::dynamic_pointer_cast<TClass>(tyc);
    
    if (c.get() == 0) {
      throw std::runtime_error("Cannot define overload in '" + id->className() + "', class does not exist.");
    }

    MonoTypeUnifier u(e->typeEnv());

    NameIndexing  tns   = nameIndexing(tvarNames(id->args()));
    MonoTypes     targs = id->args();
    bool          asfn  = id->constraints().size() > 0 || tvarNames(targs).size() > 0;
    MemberMapping ms    = compileMembers(m, &u, c, targs, e, id->members(), asfn);

    // is this a ground instance or an instance function?
    if (!asfn) {
      Definitions ds;
      try {
        c->insert(e->typeEnv(), TCInstancePtr(new TCInstance(id->className(), targs, ms, id->la())), &ds);
        e->drainUnqualifyDefs(ds);
      } catch (...) {
        e->drainUnqualifyDefs(ds);
        throw;
      }
    } else {
      c->insert(TCInstanceFnPtr(new TCInstanceFn(id->className(), id->constraints(), targs, ms, id->la())));
    }
  } catch (annotated_error&) {
    throw;
  } catch (std::exception& ex) {
    throw annotated_error(*id, ex.what());
  }
}

// compile import statements
void compile(const ModulePtr&, cc* e, const MImport* mimp) {
  pushModuleDir(mimp->path());
  try {
    import(e, mimp->name());
    popModuleDir();
  } catch (std::exception&) {
    popModuleDir();
    throw;
  }
}

// compile type definitions
MonoTypePtr forceMonotype(cc* e, const QualTypePtr& qt, const LexicalAnnotation& la) {
  MonoTypeUnifier u(e->typeEnv());
  Definitions ds;
  while (refine(e->typeEnv(), qt->constraints(), &u, &ds)) {
    e->drainUnqualifyDefs(ds);
    ds.clear();
  }
  e->drainUnqualifyDefs(ds);
  ds.clear();

  // make sure that the output type exists and is realizable
  if (hobbes::satisfied(e->typeEnv(), qt->constraints(), &ds)) {
    e->drainUnqualifyDefs(ds);
    return u.substitute(qt->monoType());
  } else {
    throw annotated_error(la, "Cannot resolve qualifications in type");
  }
}

void compile(const ModulePtr&, cc* e, const MTypeDef* mtd) {
  switch (mtd->visibility()) {
  case MTypeDef::Transparent:
    e->defineTypeAlias(mtd->name(), mtd->arguments(), forceMonotype(e, mtd->type(), mtd->la()));
    break;
  case MTypeDef::Opaque:
    e->defineNamedType(mtd->name(), mtd->arguments(), forceMonotype(e, mtd->type(), mtd->la()));
    break;
  default:
    break;
  }
}

// compile regular variable definitions
void compile(const ModulePtr& m, cc* e, const MVarDef* mvd) {
  ExprPtr vde = translateExprWithOpts(m, (mvd->varWithArgs().size() == 1) ? mvd->varExpr() : ExprPtr(new Fn(Fn::VarNames(mvd->varWithArgs().begin() + 1, mvd->varWithArgs().end()), mvd->varExpr(), mvd->la())));

  // make sure that globals with inaccessible names (evaluated for side-effects) have monomorphic type
  // (otherwise they'll quietly fail to run)
  if (mvd->varWithArgs().size() >= 1 && mvd->varWithArgs()[0].size() > 0 && mvd->varWithArgs()[0][0] == '.') {
    requireMonotype(e->typeEnv(), e->unsweetenExpression(mvd->varWithArgs()[0], vde));
  }

  // ok we're fine, define this variable
  e->define(mvd->varWithArgs()[0], vde);
}

// compile forward-declarations
void compile(const ModulePtr&, cc* e, const MVarTypeDef* vtd) {
  try {
    e->forwardDeclare(vtd->varName(), vtd->varType());
  } catch (std::exception& ex) {
    throw annotated_error(*vtd, ex.what());
  }
}

// keep unsafe function symbols
struct SafeExpr {
  using Status = SafeSet::Status;
  struct UnsafeDefs {
    UnsafeDefs() = default;
    UnsafeDefs(std::string const& var, std::string const& fn_) : var(var), fn(fn_), status(Status::UnSafe) {}

    const std::string& varName() const { return var; }
    const std::string& safeFn() const { return fn; }
    const std::set<std::string>& unSafeRefs() const { return closure; } 
    const Status& varStatus() const { return status; }
    const LexicalAnnotation& la() const { return lexAnno; }
    const std::string& exprDef() const { return typeDesc; }

    std::string& varName() { return var; }
    std::set<std::string>& unSafeRefs() { return closure; }
    Status& varStatus() { return status; }
    LexicalAnnotation& la() { return lexAnno; }
    std::string& exprDef() { return typeDesc; }

    friend std::ostream& operator<<(std::ostream& os, const UnsafeDefs& s) {
      os << s.var << "  " << s.status << "  " << str::show(s.closure) << ". " << s.typeDesc;
      return os;
    }

    std::string desc() const {
      std::stringstream ss;
      ss << *this;
      return ss.str();
    }

  private:
    std::string var;
    std::string fn;
    Status status { Status::Undefined };
    std::set<std::string> closure;
    LexicalAnnotation lexAnno;
    std::string typeDesc;
  };

  using Map = std::map<std::string, UnsafeDefs>;

  static auto with(std::function<void (Map&)> const& mapModifier) -> void {
    instance()._with(mapModifier);
  };

  template<class R>
  static auto with(std::string const& n, std::function<R(const SafeExpr::UnsafeDefs&)> const& hit, std::function<R(void)> const& miss) -> R {
    return instance().template _with(n, hit, miss);
  }

private:
  auto _with(std::function<void (Map&)> const& mapModifier) -> void {
    mapModifier(map);
  }

  template<class R>
  auto _with(std::string const& n, std::function<R(const SafeExpr::UnsafeDefs&)> const& hit, std::function<R(void)> const& miss) -> R {
    auto it = map.find(n);
    if (it != std::end(map) && it->second.varStatus() != SafeExpr::Status::Safe)
      return hit(it->second);
    else
      return miss();
  }

  static auto instance() -> SafeExpr& {
    thread_local SafeExpr ms {Map{{"element"    , {"element", "elementM"}},
                                  {"elements"   , {"elements", "elementsM"}},
                                  {"newArray"   , {"newArray", {}}},
                                  {"newPrim"    , {"newPrim", {}}},
                                  {"newPrimZ"   , {"newPrimZ", {}}},
                                  {"unsafeCast" , {"unsafeCast", {}}
                                }}};
    return ms;
  }

  SafeExpr() = default;
  SafeExpr(Map const &map) : map(map) {}
  Map map;
};

struct UnsafeRefs : public SafeExpr::UnsafeDefs {
  friend auto operator< (SafeExpr::UnsafeDefs const& l, SafeExpr::UnsafeDefs const& r) -> bool {
    return l.varName() < r.varName();
  }

  auto stepFn(std::pair<std::deque<std::string>, std::set<std::string>&>& v) -> void {
    if (v.first.empty())
      return;
    auto e = v.first.front();
    SafeExpr::template with<void>(e, [&](const SafeExpr::UnsafeDefs& unsafeDef) {
      for (auto &f: unsafeDef.unSafeRefs()) {
        if (v.second.find(f) == v.second.end()) {
          v.first.push_back(f);
        }
      }
    }, []() {});

    v.first.pop_front();
    v.second.insert(e);
  }

  void stepAll() {
    auto& var = varName();
    std::pair<std::deque<std::string>, std::set<std::string>&> r = { { var }, unSafeRefs() };
    while (not r.first.empty())
      stepFn(r);
    unSafeRefs().erase(var);
    SafeExpr::template with<void>(var, [&](const SafeExpr::UnsafeDefs& unsafeDef) {
      la() = unsafeDef.la();
      exprDef() = unsafeDef.exprDef();
    }, []() {});
  }

  auto show(std::ostream& os) -> void {
    os << la().filename() << ", " << la().lineDesc() << ": " << varName() << " is not allowed";
    if (unSafeRefs().size()>0)
      os << ", its transitive closure has disabled expressions: " << str::show(unSafeRefs()) ;
    os << "." << exprDef() << std::endl;
  }
};

void SafeSet::setUnsafeFn(std::string const& varName) {
  SafeExpr::with([varName](SafeExpr::Map& m) {
    auto& v = m[varName];
    v.varName() = varName;
    v.varStatus() = SafeExpr::Status::UnSafe;
  });
}

void SafeSet::setSafeFn(std::string const& varName) {
  SafeExpr::with([varName](SafeExpr::Map& m) {
    auto& v = m[varName];
    v.varName() = varName;
    v.varStatus() = SafeExpr::Status::Safe;
  });
}

void SafeSet::forEach(std::function<void (std::string const& /*var*/, Status const& /*status*/, std::string const& /*desc*/)> const& fn) {
  SafeExpr::with([&](SafeExpr::Map& m) {
    for (auto& d : m) {
      fn(d.second.varName(), d.second.varStatus(), d.second.desc());
    }
  });
}
std::string const& SafeSet::get(std::string const& binding) {
  return SafeExpr::with<std::string const&>(binding,
                                            [&binding](SafeExpr::UnsafeDefs const& udefs) -> std::string const& {
                                              return udefs.safeFn().empty() ? binding : udefs.safeFn();
                                            },
                                            [&binding]() -> std::string const& {
                                              return binding;
                                            });
}

struct buildTransitiveUnsafePragmaClosure : public switchExprC<bool> {
  buildTransitiveUnsafePragmaClosure(MVarDef const& mvd) : mvd(mvd) { }

  ~buildTransitiveUnsafePragmaClosure() {
    SafeExpr::with([&](SafeExpr::Map& m) {
      auto iter = m.find(mvd.varWithArgs()[0]);
      if (iter != m.end()) {
        auto& var = iter->second;
        std::stringstream ss;
        mvd.show(ss);
        var.exprDef() = ss.str();
      }
    });  
  }
  MVarDef const& mvd;

  virtual bool withConst(const Expr*) const { return true; };
  virtual bool with(const Var* v) const {
    return SafeExpr::with<bool>(v->value(), [&](const SafeExpr::UnsafeDefs&) {
        SafeExpr::with([&](SafeExpr::Map& m) {
          auto varName=mvd.varWithArgs()[0];
          auto r = m.insert({varName, {}});
          auto& var = r.first->second;
          if (r.second) {
            var.varName() = varName;
            var.la() = mvd.la();
          }
          auto &status = var.varStatus();
          if (status != SafeExpr::Status::Safe) {
            status = SafeExpr::Status::UnSafe;
            var.unSafeRefs().insert(v->value());
          }
    });
        return true; 
      }, 
      []() { return false; });
  }
  virtual bool with     (const Let* v)       const { switchOf(v->varExpr(), *this); switchOf(v->bodyExpr(), *this); return true; }
  virtual bool with     (const LetRec* v)    const {
    str::set vns = toSet(v->varNames());
    LetRec::Bindings bs;
    for (const auto& b : v->bindings()) {
      switchOf(b.second, *this);
    }
    switchOf(v->bodyExpr(), *this);
    return true;
  }
  virtual bool with     (const Fn* v)        const { switchOf(v->body(), *this); return true; }
  virtual bool with     (const App* v)       const { switchOf(v->fn(), *this); switchOf(v->args(), *this); return true; }
  virtual bool with     (const Assign* v)    const { switchOf(v->left(), *this); switchOf(v->right(), *this); return true; }
  virtual bool with     (const MkArray* v)   const { switchOf(v->values(), *this); return true; }
  virtual bool with     (const MkVariant* v) const { switchOf(v->value(), *this); return true; }
  virtual bool with     (const MkRecord* v)  const { switchOf(v->fields(), *this); return true; }
  virtual bool with     (const AIndex*) const { return true; }
  virtual bool with     (const Case* v) const {
    const Case::Bindings& cbs = v->bindings();
    Case::Bindings rcbs;
    for (Case::Bindings::const_iterator cb = cbs.begin(); cb != cbs.end(); ++cb) {
      switchOf(cb->exp, *this);
    }
    ExprPtr de = v->defaultExpr();
    if (de.get()) {
      switchOf(de, *this);
    }
    switchOf(v->variant(), *this);
    return true;
  }
  virtual bool with     (const Switch* v)    const {
    Switch::Bindings rsbs;
    for (auto sb : v->bindings()) {
      switchOf(sb.exp, *this);
    }
    ExprPtr de = v->defaultExpr();
    if (de) {
      switchOf(de, *this);
    }
    switchOf(v->expr(), *this);
    return true;
  }
  virtual bool with     (const Proj* v)      const { switchOf(v->record(), *this); return true; }
  virtual bool with     (const Assump* v)    const { switchOf(v->expr(), *this); return true; }
  virtual bool with     (const Pack* v)      const { switchOf(v->expr(), *this); return true; }
  virtual bool with     (const Unpack* v)    const { switchOf(v->package(), *this); switchOf(v->expr(), *this); return true; }
};

// compile pragma defines
void compile(const ModulePtr&, cc*, const MUnsafePragmaDef* mpd) {
  SafeSet::setUnsafeFn(mpd->symbolValue());
  SafeExpr::with([&](SafeExpr::Map& m){
    auto& v = m[mpd->symbolValue()];
    v.la() = mpd->la();
  });
}

void compile(const ModulePtr&, cc*, const MSafePragmaDef* mpd) {
  SafeSet::setSafeFn(mpd->symbolValue());
  SafeExpr::with([&](SafeExpr::Map& m){
    auto& v = m[mpd->symbolValue()];
    v.la() = mpd->la();
  });
}

// for now, just treat each definition independently and stick it in the input environment
//   (this disallows things like mutual recursion)
void compile(cc* e, const ModulePtr& m) {
  for (auto tmd : m->definitions()) {
    auto md = applyTypeDefns(m, e, tmd);

    if (const MImport* imp = is<MImport>(md)) {
      compile(m, e, imp);
    } else if (const ClassDef* cd = is<ClassDef>(md)) {
      compile(m, e, cd);
    } else if (const InstanceDef* id = is<InstanceDef>(md)) {
      compile(m, e, id);
    } else if (const MTypeDef* td = is<MTypeDef>(md)) {
      compile(m, e, td);
    } else if (const MVarDef* vd = is<MVarDef>(md)) {
      compile(m, e, vd);
    } else if (const MVarTypeDef* vtd = is<MVarTypeDef>(md)) {
      compile(m, e, vtd);
    } else if (const MUnsafePragmaDef* vpd = is<MUnsafePragmaDef>(md)) {
      (void)vpd;
    } else if (const MSafePragmaDef* vpd = is<MSafePragmaDef>(md)) {
      (void)vpd;
    } else {
      throw std::runtime_error("Cannot compile module definition: " + show(md));
    }
  }

  // compile unsafe pragma
  for (auto tmd : m->definitions()) {
    if (const MUnsafePragmaDef* vpd = is<MUnsafePragmaDef>(tmd)) {
      compile(m, e, vpd); 
    }
  }

  // compile safe pragma
  for (auto tmd : m->definitions()) {
    if (const MSafePragmaDef* vpd = is<MSafePragmaDef>(tmd)) {
      compile(m, e, vpd); 
    }
  }

  // generate unsafe transitive closure
  for (auto tmd : m->definitions()) {
    if (const MVarDef* vd = is<MVarDef>(tmd)) {
      switchOf(vd->varExpr(), buildTransitiveUnsafePragmaClosure(*vd));
    }
  }
}

std::vector<std::string> getDefaultOptions() {
  return str::strings();
}

// make hobbes run in "safe" mode
struct makeSafe : public switchExprC<ExprPtr> {
  std::map<std::string, UnsafeRefs> collectUnsafes;

  makeSafe() { }

  auto show() -> std::string {
    std::stringstream ss;
    for (auto& kv : collectUnsafes) {
      kv.second.show(ss);
    }
    return ss.str();
  }

  ExprPtr withConst(const Expr* v) const { return ExprPtr(v->clone()); }

  ExprPtr with(const Var* v) const {
    return SafeExpr::template with<ExprPtr>(v->value(), [&, this](const SafeExpr::UnsafeDefs& unsafeDef) {
      ExprPtr vc(v->clone());
      if (auto vcc = is<Var>(vc.get())) {
        if (not unsafeDef.safeFn().empty()) {
          const_cast<Var *>(vcc)->value(unsafeDef.safeFn());
        } else {
          auto& entry = const_cast<makeSafe*>(this)->collectUnsafes[v->value()];
          if (not v->value().empty()) {
            entry.varName() = v->value();
            entry.stepAll();
          }
         }
      }
      return vc;
    }, [&]() { return ExprPtr(v->clone()); });
  }

  ExprPtr with(const Let* v) const {
    return ExprPtr(new Let(v->var(), switchOf(v->varExpr(), *this), switchOf(v->bodyExpr(), *this), v->la()));
  }

  ExprPtr with(const LetRec* v) const {
    str::set vns = toSet(v->varNames());
    LetRec::Bindings bs;
    for (const auto& b : v->bindings()) {
      bs.push_back(LetRec::Binding(b.first, switchOf(b.second, *this)));
    }
    return ExprPtr(new LetRec(bs, switchOf(v->bodyExpr(), *this), v->la()));
  }

  ExprPtr with(const Fn* v) const {
    return ExprPtr(new Fn(v->varNames(), switchOf(v->body(), *this), v->la()));
  }

  ExprPtr with(const App* v) const {
    return ExprPtr(new App(switchOf(v->fn(), *this), switchOf(v->args(), *this), v->la()));
  }

  ExprPtr with(const Assign* v) const {
    return ExprPtr(new Assign(switchOf(v->left(), *this), switchOf(v->right(), *this), v->la()));
  }

  ExprPtr with(const MkArray* v) const {
    return ExprPtr(new MkArray(switchOf(v->values(), *this), v->la()));
  }

  ExprPtr with(const MkVariant* v) const {
    return ExprPtr(new MkVariant(v->label(), switchOf(v->value(), *this), v->la()));
  }
  
  ExprPtr with(const MkRecord* v) const {
    return ExprPtr(new MkRecord(switchOf(v->fields(), *this), v->la()));
  }

  ExprPtr with(const AIndex* v) const {
    return fncall(var("atm", v->la()), list(switchOf(v->array(), *this), switchOf(v->index(), *this)), v->la());
  }

  ExprPtr with(const Case* v) const {
    const Case::Bindings& cbs = v->bindings();
    Case::Bindings rcbs;
    for (Case::Bindings::const_iterator cb = cbs.begin(); cb != cbs.end(); ++cb) {
      rcbs.push_back(Case::Binding(cb->selector, cb->vname, switchOf(cb->exp, *this)));
    }
    ExprPtr de = v->defaultExpr();
    if (de.get()) {
      de = switchOf(de, *this);
    }
    return ExprPtr(new Case(switchOf(v->variant(), *this), rcbs, de, v->la()));
  }

  ExprPtr with(const Switch* v) const {
    Switch::Bindings rsbs;
    for (auto sb : v->bindings()) {
      rsbs.push_back(Switch::Binding(sb.value, switchOf(sb.exp, *this)));
    }
    ExprPtr de = v->defaultExpr();
    if (de) {
      de = switchOf(de, *this);
    }
    return ExprPtr(new Switch(switchOf(v->expr(), *this), rsbs, de, v->la()));
  }

  ExprPtr with(const Proj* v) const {
    return ExprPtr(new Proj(switchOf(v->record(), *this), v->field(), v->la()));
  }

  ExprPtr with(const Assump* v) const {
    return ExprPtr(new Assump(switchOf(v->expr(), *this), v->ty(), v->la()));
  }

  ExprPtr with(const Pack* v) const {
    return ExprPtr(new Pack(switchOf(v->expr(), *this), v->la()));
  }

  ExprPtr with(const Unpack* v) const {
    return ExprPtr(new Unpack(v->varName(), switchOf(v->package(), *this), switchOf(v->expr(), *this), v->la()));
  }
};

struct makeSafeArrays : public makeSafe {
  struct SafeArray {
    using Map = SafeExpr::Map;

    static auto with(const Var *v) -> ExprPtr {
      thread_local Map safeArrayTable{ Map{ { {"element", {"element", "elementM"} }, { "elements", {"elements", "elementsM" } } } } };
      ExprPtr vc(v->clone());
      if (auto vcc = is<Var>(vc.get())) {
        auto iter = safeArrayTable.find(v->value());
        if (iter != std::end(safeArrayTable)) {
          const_cast<Var *>(vcc)->value(iter->second.safeFn());
        }
      }
      return vc;
    }
  };

  ExprPtr with(const Var* v) const { return SafeArray::with(v); }
};

OptDescs getAllOptions() {
  OptDescs d;
  d["Safe"]       = "Interpret hobbes in safe mode";
  d["SafeArrays"] = "Interpret array indexing 'safely' (always bounds-checked and mapped to an optional type in case of out-of-bounds access)";
  return d;
}

ExprPtr translateExprWithOpts(const ModulePtr& m, const ExprPtr& e) {
  return translateExprWithOpts(m->options(), e);
}

ExprPtr translateExprWithOpts(const std::vector<std::string>& opts, const ExprPtr& e, std::function<void(std::string const&)> const& exceptionFn) {
  auto dispatch = std::map<std::string, std::function<ExprPtr(std::string const&, const ExprPtr&)>> {
    { 
      "Safe", [&exceptionFn](std::string const&, const ExprPtr& e) -> ExprPtr {
        auto ms = makeSafe();
        auto r = switchOf(e, ms);
        if (ms.collectUnsafes.size() != 0) {
          exceptionFn(ms.show());
        };
        return r; 
      }
    },
    { "SafeArrays", [](std::string const&, const ExprPtr& e) -> ExprPtr { return switchOf(e, makeSafeArrays()); } },
  };
  thread_local auto ignoreFn = [](std::string const& optName, const ExprPtr& e) -> ExprPtr { 
    throw std::runtime_error("unsupported option, " + optName); return e; 
  };

  ExprPtr r = e;
  for (const auto& opt : opts) {
    r = dispatch.insert(std::make_pair(opt, ignoreFn)).first->second(opt, r);
  }
  return r;
}

}

