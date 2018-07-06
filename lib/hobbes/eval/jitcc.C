
#include <hobbes/hobbes.H>
#include <hobbes/eval/jitcc.H>
#include <hobbes/eval/cexpr.H>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"

#if LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
#include "llvm/ExecutionEngine/JIT.h"
#else
#include "llvm/ExecutionEngine/MCJIT.h"
#endif

#if LLVM_VERSION_MINOR >= 8 || LLVM_VERSION_MAJOR == 4
#include "llvm/Analysis/BasicAliasAnalysis.h"
#endif

#if LLVM_VERSION_MAJOR == 4
#include "llvm/Transforms/Scalar/GVN.h"
#endif

#include "llvm/Object/ELFObjectFile.h"
#include "llvm/ExecutionEngine/JITEventListener.h"

#pragma GCC diagnostic pop

namespace hobbes {

// this should be moved out of here eventually
bool isFileType(const MonoTypePtr&);

#if LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4
class jitmm : public llvm::SectionMemoryManager {
public:
  jitmm(jitcc* jit) : jit(jit) { }

  // link symbols across modules :T
  uint64_t getSymbolAddress(const std::string& n) override {
    if (uint64_t laddr = reinterpret_cast<uint64_t>(this->jit->getSymbolAddress(n))) {
      return laddr;
    }
    if (n.size() > 0 && n[0] == '_') {
      uint64_t sv = reinterpret_cast<uint64_t>(this->jit->getSymbolAddress(n.substr(1)));
      if (sv) return sv;
    }
    if (uint64_t baddr = llvm::SectionMemoryManager::getSymbolAddress(n)) {
      return baddr;
    } else {
      throw std::runtime_error("Internal error, can't resolve symbol: " + n);
    }
  }
private:
  jitcc* jit;
};
#endif

jitcc::jitcc(const TEnvPtr& tenv) :
  tenv(tenv), currentModule(0), irbuilder(0),
  ignoreLocalScope(false),
  globalData(32768 /* min global page size = 32K */)
{
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmParser();
  llvm::InitializeNativeTargetAsmPrinter();

  // allocate an IR builder with an initial dummy basic-block to write into
  this->irbuilder = new llvm::IRBuilder<>(context());
  this->irbuilder->SetInsertPoint(llvm::BasicBlock::Create(context(), "dummy"));

  // make sure we've always got one frame for variable defs
  this->vtenv.push_back(VarBindings());

#if LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
  // for older LLVM versions, we create one module and an execution engine ahead of time
  this->eengine = makeExecutionEngine(module(), 0);

#if LLVM_VERSION_MINOR >= 5
  this->mpm = new llvm::legacy::PassManager();
  this->mpm->add(llvm::createFunctionInliningPass());

  this->fpm = new llvm::legacy::FunctionPassManager(module());
  this->fpm->add(new llvm::DataLayoutPass(*this->eengine->getDataLayout()));
#else
  this->mpm = new llvm::PassManager();
  this->mpm->add(llvm::createFunctionInliningPass());

  this->fpm = new llvm::FunctionPassManager(module());
  this->fpm->add(new llvm::DataLayout(*this->eengine->getDataLayout()));
#endif
  this->fpm->add(llvm::createBasicAliasAnalysisPass());
  this->fpm->add(llvm::createInstructionCombiningPass());
  this->fpm->add(llvm::createReassociatePass());
  this->fpm->add(llvm::createGVNPass());
  this->fpm->add(llvm::createCFGSimplificationPass());
  this->fpm->add(llvm::createTailCallEliminationPass());
  this->fpm->doInitialization();
#endif

#if LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4
  this->mpm = new llvm::legacy::PassManager();
  this->mpm->add(llvm::createFunctionInliningPass());
#endif
}

jitcc::~jitcc() {
  // release low-level functions
  for (auto f : this->fenv) {
    delete f.second;
  }

  // release LLVM resources
#if LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4
  for (auto ee : this->eengines) {
    delete ee;
  }
  delete this->currentModule;
#elif LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
  delete this->eengine;
#endif

  delete this->irbuilder;
}

const TEnvPtr& jitcc::typeEnv() const {
  return this->tenv;
}

llvm::IRBuilder<>* jitcc::builder() const {
  return this->irbuilder;
}

llvm::Module* jitcc::module() {
  if (!this->currentModule) {
    this->currentModule = new llvm::Module("jitModule" + str::from(this->modules.size()), context());
    this->modules.push_back(this->currentModule);
  }
  return this->currentModule;
}

void* jitcc::getSymbolAddress(const std::string& vn) {
  // do we have a global with this name?
  auto gd = this->globals.find(vn);
  if (gd != this->globals.end()) {
    return gd->second.value;
  }

#if LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4
  // do we have a compiled function with this name?
  for (auto ee : this->eengines) {
    if (uint64_t faddr = ee->getFunctionAddress(vn)) {
      return reinterpret_cast<void*>(faddr);
    }
  }
#endif

  // shrug
  return 0;
}

void jitcc::dump() const {
  for (auto m : this->modules) {
    m->dump();
  }
}

void* jitcc::getMachineCode(llvm::Function* f, llvm::JITEventListener* listener) {
#if LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4
  // try to get the machine code for this function out of an existing compiled module
  for (auto ee : this->eengines) {
    if (void* pf = ee->getPointerToFunction(f)) {
      return pf;
    }
  }

  // we've never seen this function, it must be in the current module
  if (!this->currentModule) {
    throw std::runtime_error("Internal compiler error, can't derive machine code for unknown function");
  }

  // make a new execution engine out of this module (finalizing the module)
  std::string err;
  llvm::ExecutionEngine* ee = makeExecutionEngine(this->currentModule, reinterpret_cast<llvm::SectionMemoryManager*>(new jitmm(this)));

  if (listener) {
    ee->RegisterJITEventListener(listener);
  }

  // set up the function optimization pipeline for this module
  llvm::legacy::FunctionPassManager fpm(this->currentModule);

#if LLVM_VERSION_MINOR == 6
  this->currentModule->setDataLayout(ee->getDataLayout());
  fpm.add(llvm::createBasicAliasAnalysisPass());
#elif LLVM_VERSION_MINOR == 7
  this->currentModule->setDataLayout(*ee->getDataLayout());
  fpm.add(llvm::createBasicAliasAnalysisPass());
#else // LLVM_VERSION_MINOR >= 8
  this->currentModule->setDataLayout(ee->getDataLayout());
#endif
  fpm.add(llvm::createInstructionCombiningPass());
  fpm.add(llvm::createReassociatePass());
  fpm.add(llvm::createGVNPass());
  fpm.add(llvm::createCFGSimplificationPass());
  fpm.add(llvm::createTailCallEliminationPass());
  fpm.doInitialization();

  // optimize the module
  for (auto mf = this->currentModule->begin(); mf != this->currentModule->end(); ++mf) {
    fpm.run(*mf);
  }

  // apply module-level optimizations
  this->mpm->run(*this->currentModule);

  // but we can still get at it through its execution engine
  this->eengines.push_back(ee);
  ee->finalizeObject();

  // now we can't touch this module again
  this->currentModule = 0;

  // and _now_ we must be able to get machine code for this function
  void* pf = ee->getPointerToFunction(f);

  if (listener) {
    ee->UnregisterJITEventListener(listener);
  }

  if (!pf) {
    throw std::runtime_error("Internal error, failed to derive machine code from head module");
  }

  return pf;
#elif LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
  // apply module-level optimizations
  this->mpm->run(*this->currentModule);

  if (listener) {
    this->eengine->RegisterJITEventListener(listener);
  }

  void* pf = this->eengine->getPointerToFunction(f);

  if (listener) {
    this->eengine->UnregisterJITEventListener(listener);
  }

  if (pf) {
    return pf;
  } else {
    throw std::runtime_error("Internal error, failed to derive machine code for function");
  }
#endif
}

#if LLVM_VERSION_MINOR >= 7 || LLVM_VERSION_MAJOR == 4
// get the machine code produced for a given expression
// (there must be a simpler way)
class LenWatch : public llvm::JITEventListener {
public:
  std::string fname;
  size_t sz;
  LenWatch(const std::string& fname) : fname(fname), sz(0) { }
  size_t size() const { return this->sz; }
  void NotifyObjectEmitted(const llvm::object::ObjectFile& o, const llvm::RuntimeDyld::LoadedObjectInfo& dl) {
    for (auto s : o.symbols()) {
      const llvm::object::ELFSymbolRef* esr = reinterpret_cast<const llvm::object::ELFSymbolRef*>(&s);

      if (esr) {
        auto nr = esr->getName();
        if (nr) {
          std::string n(nr.get().data(), nr.get().size());
          if (n == this->fname) {
            this->sz = esr->getSize();
          }
        }
      }
    }
  }
};
#elif LLVM_VERSION_MINOR == 6
// get the machine code produced for a given expression
// (there must be a simpler way)
class LenWatch : public llvm::JITEventListener {
public:
  std::string fname;
  size_t sz;
  LenWatch(const std::string& fname) : fname(fname), sz(0) { }
  size_t size() const { return this->sz; }
  void NotifyObjectEmitted(const llvm::object::ObjectFile& o, const llvm::RuntimeDyld::LoadedObjectInfo& dl) {
    for (auto s : o.symbols()) {
      llvm::StringRef nm;
      if (s.getName(nm)) {
        size_t ssz;
        if (nm.str() == this->fname && s.getSize(ssz)) {
          this->sz = ssz;
        }
      }
    }
  }
};
#elif LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
class LenWatch {
public:
  LenWatch(const std::string& fname) : fname(fname), sz(0) { }
  size_t size() const { return this->sz; }
  virtual ~LenWatch() { }
  virtual void NotifyFunctionEmitted(const llvm::Function& f, void* mc, size_t sz, const llvm::JITEventListener::EmittedFunctionDetails&) {
    if (f.getName().str() == this->fname) {
      this->sz = sz;
    }
  }
  virtual void NotifyFreeingMachineCode(void*) { }
  virtual void NotifyObjectEmitted(const llvm::ObjectImage&) { }
  virtual void NotifyFreeingObject(const llvm::ObjectImage&) { }
private:
  std::string fname;
  size_t sz;
};
#endif

jitcc::bytes jitcc::machineCodeForExpr(const ExprPtr& e) {
  std::string     fname = ".asm" + freshName();
  LenWatch        lenwatch(fname);
  llvm::Function* af = compileFunction(fname, str::seq(), MonoTypes(), e);
  void*           f  = getMachineCode(af, reinterpret_cast<llvm::JITEventListener*>(&lenwatch));
  bytes           r  = bytes(reinterpret_cast<uint8_t*>(f), reinterpret_cast<uint8_t*>(f) + lenwatch.size());

  releaseMachineCode(f);
  return r;
}

bool jitcc::isDefined(const std::string& vn) const {
  if (this->globals.find(vn) != this->globals.end()) {
    return true;
  } else if (this->constants.find(vn) != this->constants.end()) {
    return true;
  } else if (lookupOp(vn) != 0) {
    return true;
  } else {
    for (const auto& vb : this->vtenv) {
      if (vb.find(vn) != vb.end()) {
        return true;
      }
    }
    return false;
  }
}

llvm::Value* jitcc::compile(const ExprPtr& exp) {
  return toLLVM(this, exp);
}

llvm::Value* jitcc::compile(const std::string& vname, const ExprPtr& exp) {
  return toLLVM(this, vname, exp);
}

void jitcc::bindInstruction(const std::string& vn, op* f) {
  this->fenv[vn] = f;
}

op* jitcc::lookupOp(const std::string& vn) const {
  auto f = this->fenv.find(vn);
  return (f == this->fenv.end()) ? 0 : f->second;
}

void jitcc::bindGlobal(const std::string& vn, const MonoTypePtr& ty, void* x) {
  Global g;
  g.type  = ty;
  g.value = x;
  if (is<Func>(ty)) {
    g.ref.fn =
      llvm::Function::Create(
        reinterpret_cast<llvm::FunctionType*>(toLLVM(ty)),
        llvm::Function::ExternalLinkage,
        vn,
        this->module()
      );

#if LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
    this->eengine->addGlobalMapping(g.ref.fn, x);
#endif
  } else {
    if (hasPointerRep(ty) || isFileType(ty)) {
      void** p = reinterpret_cast<void**>(this->globalData.malloc(sizeof(void*)));
      *p = x;
      g.value = p;
    }

    g.ref.var =
      new llvm::GlobalVariable(
        *module(),
        toLLVM(ty, true),
        false,
        llvm::GlobalValue::ExternalLinkage,
        0,
        vn
      );
    g.ref.var->setAlignment(sizeof(void*));

#if LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
  this->eengine->addGlobalMapping(g.ref.var, g.value);
#endif
  }
  this->globals[vn] = g;
}

llvm::Value* jitcc::maybeRefGlobalV(llvm::Value* v) {
  llvm::Module* thisMod = module();

  if (auto f = llvm::dyn_cast<llvm::Function>(v)) {
    if (f->getParent() == thisMod) {
      return f;
    } else {
      return externDecl(f, thisMod);
    }
  } else if (auto gv = llvm::dyn_cast<llvm::GlobalVariable>(v)) {
    if (gv->getParent() == thisMod) {
      return v;
    } else if (llvm::GlobalVariable* rgv = thisMod->getGlobalVariable(gv->getName())) {
      return rgv;
    } else {
      return new llvm::GlobalVariable(*thisMod, gv->getType()->getElementType(), gv->isConstant(), llvm::GlobalVariable::ExternalLinkage, 0, gv->getName());
    }
  } else {
    return v;
  }
}

llvm::GlobalVariable* jitcc::maybeRefGlobal(const std::string& vn) {
  auto gv = this->globals.find(vn);
  if (gv != this->globals.end() && !is<Func>(gv->second.type)) {
    return refGlobal(vn, gv->second.ref.var);
  }
  return 0;
}

llvm::GlobalVariable* jitcc::refGlobal(const std::string& vn, llvm::GlobalVariable* gv) {
  llvm::Module* mod = module();

  if (!gv) {
    return 0;
  } else if (gv->getParent() == mod) {
    return gv;
  } else if (llvm::GlobalVariable* rgv = mod->getGlobalVariable(vn)) {
    return rgv;
  } else {
    auto ret = new llvm::GlobalVariable(*mod, gv->getType()->getElementType(), gv->isConstant(), llvm::GlobalVariable::ExternalLinkage, 0, vn);
    ret->setAlignment(sizeof(void*));
    return ret;
  }
}

llvm::GlobalVariable* jitcc::lookupVarRef(const std::string& vn) {
  // if any local variables shadow this global, hide it
  for (auto vbs : this->vtenv) {
    if (vbs.find(vn) != vbs.end()) {
      return 0;
    }
  }

  // now if we've got a global with this name, we can get a pointer to it
  return maybeRefGlobal(vn);
}

llvm::Value* jitcc::loadConstant(const std::string& vn) {
  auto cv = this->constants.find(vn);
  if (cv != this->constants.end()) {
    if (is<Array>(cv->second.mtype)) {
      return builder()->CreateLoad(refGlobal(vn, cv->second.ref));
    } else if (llvm::Value* r = refGlobal(vn, cv->second.ref)) {
      return hasPointerRep(cv->second.mtype) ? r : builder()->CreateLoad(r);
    } else {
      return cv->second.value;
    }
  }
  return 0;
}

void jitcc::defineGlobal(const std::string& vn, const ExprPtr& ue) {
  std::string vname = vn.empty() ? (".global" + freshName()) : vn;
  this->globalExprs[vn] = ue;

  typedef void (*Thunk)();
  MonoTypePtr uety = requireMonotype(this->tenv, ue);

  if (isUnit(uety)) {
    // no storage necessary for units
    Thunk f = reinterpret_cast<Thunk>(reifyMachineCodeForFn(uety, list<std::string>(), list<MonoTypePtr>(), ue));
    f();
    releaseMachineCode(reinterpret_cast<void*>(f));
    resetMemoryPool();
  } else if (llvm::Constant* c = toLLVMConstant(this, vname, ue)) {
    // make a global constant ...
    Constant& cv = this->constants[vname];
    cv.value = c;
    cv.type  = toLLVM(uety);
    cv.mtype = uety;

    if (is<Func>(uety)) {
      // functions are loaded by name rather than by constant value
      cv.ref = 0;
    } else {
      cv.ref = new llvm::GlobalVariable(*module(), cv.type, true, llvm::GlobalVariable::InternalLinkage, c, vname);
    }
  } else {
    // make some space for this global data ...
    if (isLargeType(uety)) {
      void** pdata = reinterpret_cast<void**>(this->globalData.malloc(sizeof(void*)));
      *pdata = this->globalData.malloc(sizeOf(uety));
      bindGlobal(vname, uety, pdata);
    } else {
      bindGlobal(vname, uety, this->globalData.malloc(sizeOf(uety)));
    }

    // compile an initializer function for this expression
    llvm::BasicBlock* ibb = this->builder()->GetInsertBlock();
    llvm::Function* initfn = allocFunction("." + vname + "_init", MonoTypes(), primty("unit"));
    if (initfn == 0) {
      throw annotated_error(*ue, "Failed to allocate initializer function for '" + vname + "'.");
    }
    llvm::BasicBlock* bb = llvm::BasicBlock::Create(context(), "entry", initfn);
    this->builder()->SetInsertPoint(bb);

    compile(assign(var(vname, uety, ue->la()), ue, ue->la()));
    this->builder()->CreateRetVoid();

    // compile and run this function, it should then perform the global variable assignment
    // (make sure that any allocation happens in the global context iff we need it)
    Thunk f = reinterpret_cast<Thunk>(getMachineCode(initfn));

    if (hasPointerRep(uety)) {
      size_t oldregion = pushGlobalRegion();
      f();
      popGlobalRegion(oldregion);
    } else {
      f();
      resetMemoryPool();
    }

    // clean up
    releaseMachineCode(reinterpret_cast<void*>(f));
    this->builder()->SetInsertPoint(ibb);
    initfn->eraseFromParent();
  }
}

size_t jitcc::pushGlobalRegion() {
  std::string n = "global region @ " + str::from(reinterpret_cast<void*>(this));
  size_t grid = findThreadRegion(n);
  if (grid == static_cast<size_t>(-1)) {
    grid = addThreadRegion(n, &this->globalData);
  }
  return setThreadRegion(grid);
}

void jitcc::popGlobalRegion(size_t x) {
  setThreadRegion(x);
}

void* jitcc::memalloc(size_t sz) {
  size_t r = pushGlobalRegion();
  void* result = ::hobbes::memalloc(sz);
  popGlobalRegion(r);
  return result;
}

llvm::Value* jitcc::lookupVar(const std::string& vn, const MonoTypePtr& vty) {
  // all units are the same
  if (isUnit(vty)) {
    return cvalue(true);
  }

  // try to find this variable up the local variable stack (unless we're ignoring local scope)
  if (!this->ignoreLocalScope) {
    for (size_t i = 0; i < this->vtenv.size(); ++i) {
      const VarBindings& vbs = this->vtenv[this->vtenv.size() - (i + 1)];
      auto vb = vbs.find(vn);
      if (vb != vbs.end()) {
        return maybeRefGlobalV(vb->second);
      }
    }
  }

  // try to find this variable as a global
  if (llvm::GlobalVariable* gv = maybeRefGlobal(vn)) {
    return builder()->CreateLoad(gv);
  }

  // maybe it's a function?
  if (llvm::Function* f = lookupFunction(vn)) {
    return f;
  }

  // try to find this variable as a constant
  if (llvm::Value* lc = loadConstant(vn)) {
    return lc;
  }

  // maybe it's an op?
  if (lookupOp(vn)) {
    return compile(etaLift(vn, vty, LexicalAnnotation::null()));
  }

  // well there's no other way to find a variable ...
  throw std::runtime_error("Internal error, reference to undefined variable: " + vn);
}

llvm::Value* jitcc::internConstString(const std::string& x) {
  auto v = this->internConstVars.find(x);
  if (v != this->internConstVars.end()) return lookupVar(v->second, arrayty(primty("char")));

  std::string vn = ".intern.str." + freshName();
  this->internConstVars[x] = vn;
  defineGlobal(vn, ExprPtr(mkarray(x, LexicalAnnotation::null())));
  return lookupVar(vn, arrayty(primty("char")));
}

void jitcc::pushScope() {
  this->vtenv.push_back(VarBindings());
}

void jitcc::bindScope(const std::string& vn, llvm::Value* v) {
  this->vtenv.back()[vn] = v;
}

void jitcc::popScope() {
  this->vtenv.pop_back();
}

llvm::Value* jitcc::compileAtGlobalScope(const ExprPtr& exp) {
  this->ignoreLocalScope = true;
  try {
    llvm::Value* r = compile(exp);
    this->ignoreLocalScope = false;
    return r;
  } catch (...) {
    this->ignoreLocalScope = false;
    throw;
  }
}

llvm::Function* jitcc::lookupFunction(const std::string& fn) {
  llvm::Module* thisMod = module();

  for (auto m : this->modules) {
    if (llvm::Function* f = m->getFunction(fn)) {
      if (m == thisMod) {
        return f;
      } else {
        return externDecl(f, thisMod);
      }
    }
  }
  return 0;
}

llvm::Function* jitcc::compileFunction(const std::string& name, const str::seq& argns, const MonoTypes& argtys, const ExprPtr& exp) {
  UCFS fs;
  fs.push_back(UCF(name, argns, argtys, exp));
  unsafeCompileFunctions(&fs);
  return fs[0].result;
}

void jitcc::compileFunctions(const LetRec::Bindings& bs, std::vector<llvm::Function*>* result) {
  UCFS fs;
  for (const auto& b : bs) {
    this->globalExprs[b.first] = b.second;

    if (const Fn* f = is<Fn>(stripAssumpHead(b.second))) {
      if (const Func* fty = is<Func>(requireMonotype(this->tenv, b.second))) {
        fs.push_back(UCF(b.first, f->varNames(), fty->parameters(), f->body()));
      } else {
        throw std::runtime_error("Internal error, mutual recursion must be defined over mono-typed functions");
      }
    } else {
      throw std::runtime_error("Internal error, mutual recursion must be defined over functions");
    }
  }

  unsafeCompileFunctions(&fs);

  if (result) {
    for (const auto& f : fs) {
      result->push_back(f.result);
    }
  }
}

void jitcc::compileFunctions(const LetRec::Bindings& bs) {
  compileFunctions(bs, 0);
}

void jitcc::unsafeCompileFunctions(UCFS* ufs) {
  UCFS& fs = *ufs;

  // save our current write context to restore later
  llvm::BasicBlock* ibb = this->builder()->GetInsertBlock();

  // prepare the environment for these mutually-recursive definitions
  for (size_t f = 0; f < fs.size(); ++f) {
    llvm::Function* fval = allocFunction(fs[f].name.empty() ? ("/" + freshName()) : fs[f].name, fs[f].argtys, requireMonotype(this->tenv, fs[f].exp));
    if (fval == 0) {
      throw std::runtime_error("Failed to allocate function");
    }

    this->vtenv.back()[fs[f].name] = fval;
    fs[f].result = fval;
  }

  // now compile each function
  for (size_t f = 0; f < fs.size(); ++f) {
    llvm::Function*   fval = fs[f].result;
    const UCF&        ucf  = fs[f];
    MonoTypePtr       rty  = requireMonotype(this->tenv, ucf.exp);
    llvm::BasicBlock* bb   = llvm::BasicBlock::Create(context(), "entry", fval);

    this->builder()->SetInsertPoint(bb);

    // set argument names for safe referencing here
    this->vtenv.push_back(VarBindings());

    llvm::Function::arg_iterator a = fval->arg_begin();
    for (unsigned int i = 0; i < ucf.argns.size(); ++i) {
      if (isUnit(ucf.argtys[i])) {
        this->vtenv.back()[ucf.argns[i]] = cvalue(true); // this should never even be seen
      } else {
        a->setName(ucf.argns[i]);

        llvm::Value* argv = &*a;
        if (is<FixedArray>(ucf.argtys[i])) {
          argv = cast(this->builder(), ptrType(toLLVM(ucf.argtys[i], false)), argv);
        }
        this->vtenv.back()[ucf.argns[i]] = argv;
        ++a;
      }
    }

    try {
      // compile the function body
      llvm::Value* cexp = compile(ucf.exp);
      if (isUnit(rty)) {
        this->builder()->CreateRetVoid();
      } else {
        this->builder()->CreateRet(cexp);
      }

#if LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
      this->fpm->run(*fval);
#endif

      // and we're done
      this->vtenv.pop_back();
      if (ibb != 0) { this->builder()->SetInsertPoint(ibb); }
    } catch (...) {
      if (ibb != 0) { this->builder()->SetInsertPoint(ibb); }
      this->vtenv.pop_back();
      throw;
    }
  }
}

llvm::Value* jitcc::compileAllocStmt(llvm::Value* sz, llvm::Type* mty, bool zeroMem) {
  llvm::Function* f = lookupFunction(zeroMem ? "mallocz" : "malloc");
  if (!f) throw std::runtime_error("Expected heap allocation function as call.");
  return builder()->CreateBitCast(fncall(builder(), f, sz), mty);
}

llvm::Value* jitcc::compileAllocStmt(size_t sz, llvm::Type* mty, bool zeroMem) {
  return compileAllocStmt(cvalue(static_cast<long>(sz)), mty, zeroMem);
}

void jitcc::releaseMachineCode(void*) {
}

llvm::Function* jitcc::allocFunction(const std::string& fname, const MonoTypes& argl, const MonoTypePtr& rty) {
  return
    llvm::Function::Create(
      llvm::FunctionType::get(toLLVM(rty, true), toLLVM(argl, true), false),
      llvm::Function::ExternalLinkage,
      fname,
      module()
    );
}

void* jitcc::reifyMachineCodeForFn(const MonoTypePtr& reqTy, const str::seq& names, const MonoTypes& tys, const ExprPtr& exp) {
  return getMachineCode(compileFunction("", names, tys, exp));
}

// compilation shorthand
Values compile(jitcc* c, const Exprs& es) {
  Values r;
  for (Exprs::const_iterator exp = es.begin(); exp != es.end(); ++exp) {
    r.push_back(c->compile(*exp));
  }
  return r;
}

Values compileArgs(jitcc* c, const Exprs& es) {
  Values r;
  for (auto e : es) {
    MonoTypePtr  et = requireMonotype(c->typeEnv(), e);
    llvm::Value* ev = c->compile(e);

    if (const Prim* pt = is<Prim>(et)) {
      if (pt->representation()) {
        et = pt->representation();
      }
    }

    if (is<Array>(et)) {
      // variable-length arrays need to be cast to a single type to pass LLVM's check
      r.push_back(c->builder()->CreateBitCast(ev, toLLVM(et)));
    } else if (isUnit(et)) {
      // no need to pass unit anywhere
    } else {
      r.push_back(ev);
    }
  }
  return r;
}

ExprPtr jitcc::inlineGlobals(const ExprPtr& e) {
  ExprPtr s = e;
  while (true) {
    bool f = false;
    s = substitute(this->globalExprs, s, &f);
    if (!f) break;
  }
  return s;
}

op::~op() { }

}
