
#include <hobbes/eval/cc.H>
#include <hobbes/eval/funcdefs.H>
#include <hobbes/eval/ctype.H>
#include <hobbes/eval/cexpr.H>
#include <hobbes/boot/gen/boot.H>
#include <hobbes/lang/closcvt.H>
#include <hobbes/lang/typepreds.H>
#include <hobbes/lang/macroexpand.H>
#include <hobbes/util/llvm.H>
#include <hobbes/util/array.H>
#include <hobbes/util/codec.H>

#include <hobbes/util/perf.H>

// structured file support
#include <hobbes/db/bindings.H>

// network IPC
#include <hobbes/ipc/nbindings.H>

// translate LLVM's 'abort' calls into exceptions
#include <csetjmp>
#include <csignal>
#include <cstdlib>

#include <fstream>
#include <memory>
#include <mutex>
#include <stdexcept>
#include <unordered_map>

namespace hobbes {

// protect access to hobbes/llvm resources
static std::recursive_mutex hccmtx;
hlock:: hlock() { hccmtx.lock();   }
hlock::~hlock() { hccmtx.unlock(); }

// the compiler
cc::cc() :
  // init local resources
  readModuleFileF(&defReadModuleFile),
  readModuleF(&defReadModule),
  readExprDefnF(&defReadExprDefn),
  readExprF(&defReadExpr),
  drainingDefs(false),
  unreachableMatchRowsPtr(nullptr),
  runModInlinePass(true),
  genInterpretedMatch(false),
  checkMatchReachability(true),
  lowerPrimMatchTables(false),
  columnwiseMatches(false),
  tenv(new TEnv()),
  objs(new Objs())
{
  // protect access to LLVM
  hlock _;
  this->jit = new jitcc(this->tenv);

  // initialize the environment of primitive instructions
  initDefOperators(this);

  // this seems to want to be a very primitive type class ...
  TClass::Members cvtcms;
  cvtcms["convert"] = functy(list(tgen(0)), tgen(1));
  auto* cvtc = new TClass("Convert", 2, cvtcms, FunDeps(), LexicalAnnotation::null());
  this->tenv->bind("Convert", UnqualifierPtr(cvtc));

  // support equality constraints
  this->tenv->bind(EqualTypes::constraintName(),    UnqualifierPtr(new EqualTypes()));
  this->tenv->bind(NotEqualTypes::constraintName(), UnqualifierPtr(new NotEqualTypes()));

  // support destructuring types
  this->tenv->bind(DeconstructP::constraintName(), UnqualifierPtr(new DeconstructP()));

  // support reversing opaque type aliases
  this->tenv->bind(DataP::constraintName(), UnqualifierPtr(new DataP()));

  // support type-level string functions (a hack until type and value levels can be merged)
  initStrPredicates(this->tenv);

  // support association by expression type
  this->tenv->bind(TypeofP::constraintName(), UnqualifierPtr(new TypeofP()));

  // determine type storage sizes
  this->tenv->bind(SizeOfP::constraintName(), UnqualifierPtr(new SizeOfP()));

  // support intro/elim of existential types (necessary for closures)
  this->tenv->bind(Existentials::constraintName(), UnqualifierPtr(new Existentials()));

  // support intro/elim of recursive types
  this->tenv->bind(FixIsoRecur::constraintName(), UnqualifierPtr(new FixIsoRecur()));

  // support subtype constraints (including safe upcasting between C++ objects)
  auto* subuq = new SubtypeUnqualifier();
  subuq->addEliminator(this->objs);
  this->tenv->bind(SubtypeUnqualifier::constraintName(), UnqualifierPtr(subuq));

  // support constraints on "field projection" (for records, "objects", ...)
  auto* fv = new FieldVerifier();
  this->tenv->bind(FieldVerifier::constraintName(), UnqualifierPtr(fv));

  // support constraints on "selective construction" (for variants, ...)
  this->tenv->bind(CtorVerifier::constraintName(), UnqualifierPtr(new CtorVerifier()));

  // support deconstructing records (compile-time reflection on records)
  this->tenv->bind(RecordDeconstructor::constraintName(), UnqualifierPtr(new RecordDeconstructor()));

  // support deconstructing variants (compile-time reflection on variants)
  this->tenv->bind(VariantDeconstructor::constraintName(), UnqualifierPtr(new VariantDeconstructor()));
  this->tenv->bind(VariantAppP::constraintName(), UnqualifierPtr(new VariantAppP()));
  this->tenv->bind(VariantTruncP::constraintName(), UnqualifierPtr(new VariantTruncP()));

  // support appending (appendable) types
  this->tenv->bind(AppendsToUnqualifier::constraintName(), UnqualifierPtr(new AppendsToUnqualifier()));

  // support translation of hobbes type descs to C++ type descs
  this->tenv->bind(CPPTypeDescP::constraintName(), UnqualifierPtr(new CPPTypeDescP()));

  // support connecting to remote processes
  initNetworkDefs(*this);

  // support sub-process I/O
  this->tenv->bind(ProcessP::constraintName(), UnqualifierPtr(new ProcessP(fv)));

  // initialize the macro environment (maybe this should be user-controlled, the set of macros is hard-coded for now)
  initMacroEnvironment(this->tenv);

  // initialize default built-in functions
  initStdFuncDefs(*this);

  // initialize structured storage support
  initStorageFileDefs(fv, *this);

  // boot
  compileBootCode(*this);
}
cc::~cc() {
  hlock _;
  delete this->jit;
}

SearchEntries cc::search(const MonoTypePtr& src, const MonoTypePtr& dst) { hlock _; return hobbes::search(*this, this->searchCache, src, dst); }
SearchEntries cc::search(const ExprPtr&     e,   const MonoTypePtr& dst) { hlock _; return hobbes::search(*this, this->searchCache, e, dst); }
SearchEntries cc::search(const std::string& e,   const MonoTypePtr& dst) { hlock _; return search(readExpr(e), dst); }
SearchEntries cc::search(const std::string& e,   const std::string& t)   { hlock _; return search(readExpr(e), readMonoType(t)); }

ModulePtr cc::readModuleFile(const std::string& x) { hlock _; return this->readModuleFileF(this, x); }
void cc::setReadModuleFileFn(readModuleFileFn f) { this->readModuleFileF = f; }

ModulePtr cc::readModule(const std::string& x) { hlock _; return this->readModuleF(this, x); }
void cc::setReadModuleFn(readModuleFn f) { this->readModuleF = f; }

std::pair<std::string, ExprPtr> cc::readExprDefn(const std::string& x) { hlock _; return this->readExprDefnF(this, x); }
void cc::setReadExprDefnFn(readExprDefnFn f) { this->readExprDefnF = f; }

ExprPtr cc::readExpr(const std::string& x) { hlock _; return this->readExprF(this, x); }
void cc::setReadExprFn(readExprFn f) { this->readExprF = f; }
MonoTypePtr cc::readMonoType(const std::string& x) {
  ExprPtr e = readExpr("()::"+x);
  if (const Assump* a = is<Assump>(e)) {
    return a->ty()->monoType();
  } else {
    throw std::runtime_error("Couldn't parse as type: " + x);
  }
}

void cc::gatherUnreachableMatches(const UnreachableMatches& m) { hlock _; this->gatherUnreachableMatchesF(m); }
void cc::setGatherUnreachableMatchesFn(gatherUnreachableMatchesFn f) { this->gatherUnreachableMatchesF = f; }

ExprPtr cc::unsweetenExpression(const TEnvPtr& te, const ExprPtr& e) {
  return unsweetenExpression(te, "", e);
}

ExprPtr cc::unsweetenExpression(const ExprPtr& e) {
  return unsweetenExpression(this->tenv, e);
}

ExprPtr cc::unsweetenExpression(const std::string& vname, const ExprPtr& e) {
  return unsweetenExpression(this->tenv, vname, e);
}

ExprPtr cc::normalize(const ExprPtr& e) {
  return unsweetenExpression(this->tenv, e);
}

llvm::GlobalVariable* extractGlobal(llvm::Value* e) {
  if (auto* g = llvm::dyn_cast<llvm::GlobalVariable>(e)) {
    return g;
  } else if (auto* c = llvm::dyn_cast<llvm::ConstantExpr>(e)) {
    if (c->isCast() && c->getNumOperands() == 1) {
      return extractGlobal(c->getOperand(0));
    } else {
      return nullptr;
    }
  } else {
    return nullptr;
  }
}

void cc::forwardDeclare(const std::string& vname, const QualTypePtr& qt) {
  hlock _;
  this->tenv->bind(vname, hobbes::generalize(qt));
}

bool cc::hasValueBinding(const std::string& vname) {
  hlock _;
  // either we have a bound/compiled mono-typed value, or we have a polytype value (through a generated or user-defined type class)
  return this->jit->isDefined(vname) || isClassMember(this->tenv, vname);
}

//
// "unsweeten" a term (remove syntactic sugar)
//
// this entails the following conversions:
//   * explicit closure construction (for lexically-scoped variables in functions)
//   * explicit type annotation on terms (ie: type inference)
//   * remove satisfied qualified types by rewriting
//   * expand trivial macro definitions
//
// The process of unqualifying types can produce new residual definitions, so we
// need to "drain" those (ie: commit those definitions).  However, because we can
// qualify on recursive types, the set of residual definitions may be mutually recursive.
// This means that the unsweeten/drain process needs to be mutually recursive as well,
// and we need to accumulate the "big set" of definitions before compiling them in one
// big mutually-recursive set.
//
ExprPtr cc::unsweetenExpression(const TEnvPtr& te, const std::string& vname, const ExprPtr& e) {
  hlock _;
  Definitions ds;

  ExprPtr result;
  try {
    result = macroExpand(unqualifyTypes(te, validateType(te, vname, closureConvert(this->tenv, vname, e), &ds), &ds));
  } catch (std::exception& ex) {
    drainUnqualifyDefs(ds);
    throw;
  }

  drainUnqualifyDefs(ds);
  return result;
}

[[noreturn]] void raiseUnsolvedCsts(cc& c, const std::string&, const ExprPtr& e, const PolyTypePtr&) {
  throw unsolved_constraints(*e, "Constraints left unresolved in residual expression", expandHiddenTCs(c.typeEnv(), simplifyVarNames(e->type())->constraints()));
}

void cc::drainUnqualifyDefs(const Definitions& ds) {
  hlock _;
  bool finaldef = !this->drainingDefs;
  this->drainingDefs = true;

  // forward declare the polymorphic functions, batch letrec compile the monomorphic functions
  for (const auto &d : ds) {
    const std::string& vname = d.first;
    const ExprPtr&     e     = d.second;

    bool forwardDeclared = this->tenv->hasBinding(vname) && !hasValueBinding(vname);

    ExprPtr     ne   = forwardDeclared ? ExprPtr(new Assump(e, this->tenv->lookup(vname)->instantiate(), e->la())) : e;
    ExprPtr     xe   = unsweetenExpression(this->tenv, vname, ne);
    PolyTypePtr xety = hobbes::generalize(xe->type());
    
    if (isMonotype(xety)) {
      this->drainDefs.push_back(LetRec::Binding(vname, xe));
      if (!forwardDeclared) { this->tenv->bind(vname, xety); }
    } else {
      if (forwardDeclared) {
        if (vname.substr(0, 4) == ".rfn" || isMonotype(this->tenv->lookup(vname))) {
          raiseUnsolvedCsts(*this, vname, xe, this->tenv->lookup(vname));
        }
        this->tenv->unbind(vname);
      }
      definePolyValue(vname, xe);
    }
  }

  if (finaldef) {
    this->drainingDefs = false;

    if (!this->drainDefs.empty()) {
      this->jit->compileFunctions(this->drainDefs);
      this->drainDefs.clear();
    }
  }
}

void cc::define(const std::string& vname, const ExprPtr& e) {
  hlock _;

  // don't allow redefinitions of existing bindings
  if (hasValueBinding(vname)) {
    throw annotated_error(*e, "Variable already defined: " + vname);
  }

  // define this either as a monotyped value (in which case it can be machine-represented)
  //                 or as a polytyped value (in which case we need to internalize the definition in a hidden type class)
  bool forwardDeclared = this->tenv->hasBinding(vname);

  ExprPtr     ne   = forwardDeclared ? ExprPtr(new Assump(e, this->tenv->lookup(vname)->instantiate(), e->la())) : e;
  ExprPtr     xe   = unsweetenExpression(this->tenv, vname, ne);
  PolyTypePtr xety = hobbes::generalize(xe->type());

  if (isMonotype(xety)) {
    this->jit->defineGlobal(vname, xe);

    if (!forwardDeclared) {
      this->tenv->bind(vname, xety);
    }
  } else {
    if (forwardDeclared) {
      if (isMonotype(this->tenv->lookup(vname))) {
        raiseUnsolvedCsts(*this, vname, xe, this->tenv->lookup(vname));
      }
      this->tenv->unbind(vname);
    }

    definePolyValue(vname, xe);
  }
}

// should only be called by public 'define'
//  here we can just piggyback off of the existing type class / instance-generator system
//  to create a private type class with one instance generator matching this type signature
void cc::definePolyValue(const std::string& vname, const ExprPtr& unsweetExpr) {
  hlock _;
  definePrivateClass(this->tenv, vname, unsweetExpr);
}

void cc::define(const std::string& vname, const std::string& expr) {
  hlock _;
  define(vname, readExpr(expr));
}

void cc::bind(const PolyTypePtr& ty, const std::string& vn, void* x) {
  hlock _;
  this->tenv->bind(vn, ty);
  this->jit->bindGlobal(vn, requireMonotype(ty), x);
}

// define a transparent type alias
void cc::defineTypeAlias(const std::string& name, const str::seq& argNames, const MonoTypePtr& ty) {
  this->ttyDefs[name] = TTyDef(argNames, ty);
}

bool cc::isTypeAliasName(const std::string& name) const {
  return this->ttyDefs.find(name) != this->ttyDefs.end();
}

struct repTypeAliasesF : public switchTyFn {
  using TTyDef = std::pair<str::seq, MonoTypePtr>;
  using TTyDefs = std::unordered_map<std::string, TTyDef>;
  const TTyDefs& ttyDefs;
 
  repTypeAliasesF(const TTyDefs& ttyDefs) : ttyDefs(ttyDefs) {
  }

  MonoTypePtr with(const Prim* v) const override {
    if (!v->representation()) {
      auto td = this->ttyDefs.find(v->name());
      if (td != this->ttyDefs.end()) {
        if (td->second.first.empty()) {
          return td->second.second;
        }
      }
    }
    return Prim::make(v->name(), v->representation());
  }

  MonoTypePtr with(const TApp* v) const override {
    if (const Prim* f = is<Prim>(v->fn())) {
      auto td = this->ttyDefs.find(f->name());
      if (td != this->ttyDefs.end()) {
        if (v->args().size() != td->second.first.size()) {
          throw std::runtime_error("The type constructor '" + f->name() + "' requires exactly " + str::from(td->second.first.size()) + " arguments");
        }

        MonoTypeSubst s;
        for (size_t i = 0; i < v->args().size(); ++i) {
          s[td->second.first[i]] = switchOf(v->args()[i], *this);
        }
        return substituteStep(s, td->second.second);
      }
    }

    return MonoTypePtr(TApp::make(switchOf(v->fn(), *this), switchOf(v->args(), *this)));
  }
};

MonoTypePtr cc::replaceTypeAliases(const MonoTypePtr& ty) const {
  hlock _;
  return switchOf(ty, repTypeAliasesF(this->ttyDefs));
}

// map C++ types
PolyTypePtr cc::opaquePtrPolyType(const std::type_info& ti, unsigned int sz, bool inStruct) {
  hlock _;

  // if this is an object type, record its class structure
  this->objs->add(ti);

  // in any case, it's an opaque C++ type
  return polytype(opaquePtrMonoType(ti, sz, inStruct));
}

MonoTypePtr cc::opaquePtrMonoType(const std::type_info& ti, unsigned int sz, bool inStruct) {
  hlock _;

  // we don't necesarily *HAVE* to make this type opaque, if we've previously been given a type mapping AND the type has a pointer representation
  auto t = this->typeAliases.find(ti.name());

  if (t != this->typeAliases.end() && hasPointerRep(t->second)) {
    return requireMonotype(t->second);
  } else {
    this->objs->add(ti);

    // OK, we don't know what this type looks like so we'll give it an opaque pointer type
    // but strip the pointer char from the name, we assume opaqueptr types are always pointers
    std::string tn = str::demangle(ti.name());
    while (!tn.empty() && tn.back()=='*') {
      tn=tn.substr(0,tn.size()-1);
    }
    return MonoTypePtr(OpaquePtr::make(tn, sz, inStruct));
  }
}

PolyTypePtr cc::generalize(const MonoTypePtr& mt) const {
  hlock _;
  return this->objs->generalize(mt);
}

void cc::overload(const std::string& tyclass, const MonoTypes& tys) {
  hlock _;
  UnqualifierPtr tyc = this->tenv->lookupUnqualifier(tyclass);
  TClassPtr      c   = std::dynamic_pointer_cast<TClass>(tyc);
  
  if (c.get() == nullptr) {
    throw std::runtime_error("Cannot define overload in '" + tyclass + "', class does not exist.");
  } else if (!c->members().empty()) {
    throw std::runtime_error("Cannot define partial overload for type class '" + tyclass + "', which has " + str::from(c->members().size()) + " members.");
  }

  MemberMapping insts;

  if (tgenSize(tys) == 0) {
    Definitions ds;
    c->insert(typeEnv(), std::make_shared<TCInstance>(tyclass, tys, insts, LexicalAnnotation::null()), &ds);
    drainUnqualifyDefs(ds);
  } else {
    throw std::runtime_error("cc::overload forgot how to produce instance functions");
  }
}

void cc::overload(const std::string& tyclass, const MonoTypes& tys, const ExprPtr& e) {
  hlock _;
  UnqualifierPtr tyc = this->tenv->lookupUnqualifier(tyclass);
  TClassPtr      c   = std::dynamic_pointer_cast<TClass>(tyc);
  
  if (c.get() == nullptr) {
    throw std::runtime_error("Cannot define overload in '" + tyclass + "', class does not exist.");
  } else if (c->members().size() != 1) {
    throw std::runtime_error("Cannot define partial overload for type class '" + tyclass + "', which has " + str::from(c->members().size()) + " members.");
  }

  MemberMapping insts;
  insts[c->members().begin()->first] = e;

  if (tgenSize(tys) == 0) {
    Definitions ds;
    c->insert(typeEnv(), std::make_shared<TCInstance>(tyclass, tys, insts, e->la()), &ds);
    drainUnqualifyDefs(ds);
  } else {
    throw std::runtime_error("cc::overload forgot how to produce instance functions");
  }
}

void cc::overload(const std::string& tyclass, const MonoTypes& tys, const std::string& v) {
  overload(tyclass, tys, var(v, LexicalAnnotation::null()));
}

void cc::addInstance(const TClassPtr& c, const TCInstancePtr& i) {
  hlock _;
  Definitions ds;
  c->insert(this->typeEnv(), i, &ds);
  drainUnqualifyDefs(ds);
}

MonoTypePtr cc::defineNamedType(const std::string& name, const str::seq& argNames, const MonoTypePtr& ty) {
  hlock _;
  if (!argNames.empty()) {
    MonoTypePtr tfn    = tabs(argNames, ty);
    MonoTypePtr talias = MonoTypePtr(Prim::make(name, tfn));
    MonoTypePtr tappd  = tapp(talias, typeVars(argNames));
    this->tenv->alias(name, tfn);
    this->overload("Convert", list(tappd, ty), ".cast");
    this->overload("Convert", list(ty, tappd), ".cast");
    return talias;
  } else {
    MonoTypePtr talias(Prim::make(name, ty));
    this->tenv->alias(name, ty);
    this->overload("Convert", list(talias, ty), ".cast");
    this->overload("Convert", list(ty, talias), ".cast");
    return talias;
  }
}

MonoTypePtr cc::namedTypeRepresentation(const std::string& tn) const {
  hlock _;
  return this->tenv->unalias(tn);
}

bool cc::isTypeName(const std::string& tn) const {
  hlock _;
  return this->tenv->isOpaqueTypeAlias(tn);
}

llvm::IRBuilder<>* cc::builder() const {
  return this->jit->builder();
}

llvm::Module* cc::module() const {
  return const_cast<cc*>(this)->jit->module();
}

void cc::dumpTypeEnv(std::function<std::string const&(std::string const&)> const& rewriteFn) const {
  std::cout << showTypeEnv(rewriteFn) << std::endl;
}

void cc::dumpTypeEnv(str::seq* syms, str::seq* types, std::function<std::string const&(std::string const&)> const& rewriteFn) const {
  for (const auto& te : this->tenv->typeEnvTable(rewriteFn)) {
    // don't show hidden symbols since they're not meant for users
    if (!te.first.empty() && te.first[0] != '.') {
      syms->push_back(te.first);

      auto t = te.second->instantiate();
      auto cs = expandHiddenTCs(typeEnv(), t->constraints());
      types->push_back(show(hobbes::qualtype(cs, t->monoType())));
    }
  }
}

std::string cc::showTypeEnv(std::function<std::string const&(std::string const&)> const& rewriteFn) const {
  str::seqs table;
  table.resize(3);

  table[0].push_back("Variable");
  table[1].push_back("");
  table[2].push_back("Type");
  
  dumpTypeEnv(&table[0], &table[2], rewriteFn);
  str::repeat(table[0].size() - 1, " :: ", &table[1]);

  return str::showLeftAlignedTable(table);
}

const TEnvPtr& cc::typeEnv() const {
  return this->tenv;
}

void cc::dumpModule() {
#if LLVM_VERSION_MAJOR < 11
  hlock _;
  this->jit->dump();
#endif
}

cc::bytes cc::machineCodeForExpr(const std::string& expr) {
  hlock _;
  return this->jit->machineCodeForExpr(unsweetenExpression(readExpr(expr)));
}

template <typename K, typename T>
  inline T lookup(const std::map<K, T>& tenv, const K& n) {
    typename std::map<K, T>::const_iterator t = tenv.find(n);
    if (t != tenv.end()) {
      return t->second;
    } else {
      throw std::runtime_error("Undefined environment entry: " + n);
    }
  }

PolyTypePtr cc::lookupVarType(const std::string& vname) const {
  hlock _;
  return this->tenv->lookup(vname);
}

void cc::bindLLFunc(const std::string& fname, op* f) {
  hlock _;
  this->tenv->bind(fname, f->type(*this));
  this->jit->bindInstruction(fname, f);
}

void cc::bindExternFunction(const std::string& fname, const MonoTypePtr& fty, void* fn) {
  hlock _;
  this->tenv->bind(fname, generalize(fty));
  this->jit->bindGlobal(fname, fty, fn);
}

void* cc::memalloc(size_t sz, size_t asz) {
  return this->jit->memalloc(sz, asz);
}

inline TEnvPtr allocTEnvFrame(const str::seq& names, const MonoTypes& tys, const TEnvPtr& ptenv) {
  if (names.empty()) {
    return ptenv;
  } else {
    TEnvPtr r(new TEnv(ptenv));
    unsigned int n = std::min<unsigned int>(names.size(), tys.size());
    for (unsigned int i = 0; i < n; ++i) {
      r->bind(names[i], polytype(tys[i]));
    }
    return r;
  }
}

void* cc::unsafeCompileFn(const MonoTypePtr& retTy, const str::seq& tnames, const MonoTypes& argTys, const ExprPtr& exp) {
  hlock _;
  str::seq names = tnames;

  if (names.empty() && argTys.size() == 1 && isUnit(argTys[0])) {
    names.push_back("_");
  } else if (names.size() != argTys.size()) {
    std::ostringstream ss;
    ss << "Function parameter name list [" << str::cdelim(names, ", ") << "] and parameter type list [" << str::cdelim(show(argTys), ", ") << "] have inconsistent lengths.";
    throw annotated_error(*exp, ss.str());
  }

  return
    this->jit->reifyMachineCodeForFn(
      retTy,
      names,
      argTys,
      unsweetenExpression(allocTEnvFrame(names, argTys, this->tenv), assume(exp, retTy, exp->la()))
    );
}

void* cc::unsafeCompileFn(const MonoTypePtr& fnTy, const str::seq& names, const ExprPtr& exp) {
  if (const Func* fty = is<Func>(fnTy)) {
    return unsafeCompileFn(fty->result(), names, fty->parameters(), exp);
  } else {
    throw annotated_error(*exp, "Expected function type: " + show(fnTy));
  }
}

void* cc::unsafeCompileFn(const MonoTypePtr& fnTy, const str::seq& names, const std::string& exp) {
  return unsafeCompileFn(fnTy, names, readExpr(exp));
}

void cc::releaseMachineCode(void* f) {
  hlock _;
  this->jit->releaseMachineCode(f);
}

void cc::enableModuleInlining(bool f) { this->runModInlinePass = f; }
bool cc::enableModuleInlining() const { return this->runModInlinePass; }

void cc::buildInterpretedMatches(bool f) { this->genInterpretedMatch = f; }
bool cc::buildInterpretedMatches() const { return this->genInterpretedMatch; }

void cc::requireMatchReachability(bool f) { this->checkMatchReachability = f; }
bool cc::requireMatchReachability() const { return this->checkMatchReachability; }

void cc::ignoreUnreachableMatches(bool f) { this->ignoreUnreachablePatternMatchRows = f; }
bool cc::ignoreUnreachableMatches() const { return this->ignoreUnreachablePatternMatchRows; }

void cc::alwaysLowerPrimMatchTables(bool f) { this->lowerPrimMatchTables = f; }
bool cc::alwaysLowerPrimMatchTables() const { return this->lowerPrimMatchTables; }

void cc::buildColumnwiseMatches(bool f) { this->columnwiseMatches = f; }
bool cc::buildColumnwiseMatches() const { return this->columnwiseMatches; }

void cc::regexMaxExprDFASize(size_t f) { this->maxExprDFASize = f; }
size_t cc::regexMaxExprDFASize() const { return this->maxExprDFASize; }

void cc::throwOnHugeRegexDFA(bool f) { this->shouldThrowOnHugeRegexDFA = f; }
bool cc::throwOnHugeRegexDFA() const { return this-> shouldThrowOnHugeRegexDFA; }

void cc::regexDFAOverNFAMaxRatio(int f) { this->dfaOverNfaMaxRatio = f; }
int  cc::regexDFAOverNFAMaxRatio() const { return this->dfaOverNfaMaxRatio; }

}

