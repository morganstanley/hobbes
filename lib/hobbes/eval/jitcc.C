
#include <hobbes/hobbes.H>
#include <hobbes/eval/jitcc.H>
#include <hobbes/eval/cexpr.H>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/ValueHandle.h>
#include <llvm/Support/Error.h>
#include <llvm/Target/TargetMachine.h>
#include <stdexcept>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wctor-dtor-privacy"

#if LLVM_VERSION_MAJOR >= 11
#include <hobbes/eval/orcjitcc.H>
#else
#if LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
#include "llvm/ExecutionEngine/JIT.h"
#else
#include "llvm/ExecutionEngine/MCJIT.h"
#endif

#if LLVM_VERSION_MINOR >= 8 || LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 6
#include "llvm/Analysis/BasicAliasAnalysis.h"
#endif

#if LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 6
#include "llvm/Transforms/Scalar/GVN.h"
#endif
#endif

#include "llvm/Object/ELFObjectFile.h"
#include "llvm/ExecutionEngine/JITEventListener.h"

#pragma GCC diagnostic pop

namespace hobbes {
#if LLVM_VERSION_MAJOR >= 11
class ConstantList {
  using VarFnTy = std::function<llvm::GlobalVariable*(llvm::Module&)>;
  struct Constant {
    llvm::Type* type = nullptr;
    MonoTypePtr mtype;
    llvm::WeakTrackingVH gv;
    VarFnTy fn;
  };
  llvm::StringMap<Constant> constants;

public:
  ConstantList() = default;
  ConstantList(const ConstantList&) = delete;
  ConstantList(ConstantList&&) = delete;
  ConstantList& operator=(const ConstantList&) = delete;
  ConstantList& operator=(ConstantList&&) = delete;
  ~ConstantList() = default;

  LLVM_NODISCARD bool hasName(llvm::StringRef name) const {
    return constants.find(name) != constants.end();
  }

  void add(const std::string& name, llvm::Module& m, llvm::Constant* initVal,
           const MonoTypePtr& mtype) {
    assert(!hasName(name));

    Constant c{.type = toLLVM(mtype), .mtype = mtype};

    if (is<Func>(mtype) != nullptr) {
      c.fn = [name](llvm::Module&) -> llvm::GlobalVariable* { return nullptr; };
      c.gv = c.fn(m);
    } else {
      c.gv = new llvm::GlobalVariable(m, c.type, /*isConstant=*/true,
                                      llvm::GlobalVariable::ExternalLinkage, initVal, name);

      c.fn = [&, type = c.type, name](llvm::Module& m) -> llvm::GlobalVariable* {
        return prepgv(new llvm::GlobalVariable(
            m, type, /*isConstant=*/true, llvm::GlobalVariable::ExternalLinkage,
            /*initializer=*/nullptr, name));
      };
    }

    constants[name] = c;
  }

  LLVM_NODISCARD llvm::Value* loadConstant(llvm::StringRef name, llvm::Module& m,
                                           llvm::IRBuilder<>& builder) {
    auto it = constants.find(name);
    if (it == constants.end()) {
      return nullptr;
    }

    return withContext([&](auto&) -> llvm::Value* {
      if (!it->second.gv.pointsToAliveValue()) {
        it->second.gv = it->second.fn(m);
      }

      if (static_cast<llvm::Value*>(it->second.gv) == nullptr) {
        return nullptr;
      }
      auto* g = llvm::cast<llvm::GlobalVariable>(static_cast<llvm::Value*>(it->second.gv));

      if (is<Array>(it->second.mtype) != nullptr) {
        return builder.CreateLoad(g);
      }
      if (hasPointerRep(it->second.mtype)) {
        return g;
      }
      return builder.CreateAlignedLoad(g->getType()->getPointerElementType(), g,
                                       llvm::MaybeAlign(8));
    });
  }
};

class VTEnv {
public:
  using FunFnTy = std::function<llvm::Function*(llvm::Module&)>;

private:
  using VarBindings = llvm::StringMap<llvm::WeakTrackingVH>;
  using VarBindingStack = llvm::SmallVector<VarBindings, 8>;
  VarBindingStack vtenv;
  llvm::StringMap<FunFnTy> vtenvFuns;

public:
  VTEnv() = default;
  VTEnv(const VTEnv&) = delete;
  VTEnv(VTEnv&&) = delete;
  VTEnv& operator=(const VTEnv&) = delete;
  VTEnv& operator=(VTEnv&&) = delete;
  ~VTEnv() = default;

  LLVM_NODISCARD bool hasName(llvm::StringRef name) const {
    return std::any_of(vtenv.rbegin(), vtenv.rend(),
                       [name](const auto& vb) { return vb.find(name) != vb.end(); });
  }

  LLVM_NODISCARD llvm::Value* get(llvm::StringRef name, llvm::Module& m) {
    for (auto vb = vtenv.rbegin(); vb != vtenv.rend(); ++vb) {
      auto it = vb->find(name);
      if (it == vb->end()) {
        continue;
      }
      if (!it->second.pointsToAliveValue()) {
        assert(vtenvFuns.find(name) != vtenvFuns.end());
        it->second = vtenvFuns[name](m);
      }
      return it->second;
    }
    return nullptr;
  }

  void add(llvm::StringRef name, llvm::Value* v) {
    this->vtenv.back()[name] = v;
  }

  void add(llvm::StringRef name, FunFnTy f, llvm::Function* fp) {
    this->vtenv.back()[name] = fp;
    vtenvFuns[name] = std::move(f);
  }
  void pushScope() {
    vtenv.push_back(VarBindings());
  }

  void popScope() {
    vtenv.pop_back();
  }
};

class Globals {
private:
  using FunFnTy = std::function<llvm::Function*(llvm::Module&)>;
  using VarFnTy = std::function<llvm::GlobalVariable*(llvm::Module&)>;
  struct Global {
    //MonoTypePtr type;
    variant<FunFnTy, VarFnTy> protoTypes;
    llvm::WeakTrackingVH handle;
  };

public:
  Globals() = default;
  Globals(const Globals&) = delete;
  Globals(Globals&&) = delete;
  Globals& operator=(const Globals&) = delete;
  Globals& operator=(Globals&&) = delete;
  ~Globals() = default;

  LLVM_NODISCARD llvm::GlobalVariable* getVar(llvm::StringRef name, llvm::Module& m) {
    auto it = globals.find(name);
    if (it == globals.end() || isFunc(it)) {
      return nullptr;
    }
    if (!it->second.handle.pointsToAliveValue()) {
      it->second.handle = (*it->second.protoTypes.get<VarFnTy>())(m);
    }
    return llvm::cast<llvm::GlobalVariable>(static_cast<llvm::Value*>(it->second.handle));
  }

  LLVM_NODISCARD llvm::Function* getFunc(llvm::StringRef name, llvm::Module& m) {
    auto it = globals.find(name);
    if (it == globals.end() || !isFunc(it)) {
      return nullptr;
    }
    if (!it->second.handle.pointsToAliveValue()) {
      it->second.handle = (*it->second.protoTypes.get<FunFnTy>())(m);
    }
    return llvm::cast<llvm::Function>(static_cast<llvm::Value*>(it->second.handle));
  }

  LLVM_NODISCARD bool hasName(llvm::StringRef name) const {
    return globals.find(name) != globals.end();
  }

  void addVar(const std::string& name, llvm::Module& m, llvm::Type* ty) {
    Global g;
    g.protoTypes = VarFnTy([ty, name](llvm::Module& m) {
      return prepgv(new llvm::GlobalVariable(m, ty, /*isConstant=*/false,
                                             llvm::GlobalValue::ExternalLinkage,
                                             /*initializer=*/nullptr, name),
                    sizeof(void*));
    });
    g.handle = (*g.protoTypes.get<VarFnTy>())(m);
    globals[name] = g;
  }

  void addFunc(const std::string& name, llvm::Module& m, llvm::FunctionType* ty,
               llvm::Function* existingF) {
    Global g;
    g.protoTypes = FunFnTy([ty, name](llvm::Module& m) {
      return llvm::Function::Create(ty, llvm::Function::ExternalLinkage, name, m);
    });
    if (existingF != nullptr) {
      g.handle = existingF;
    } else {
      g.handle = (*g.protoTypes.get<FunFnTy>())(m);
    }
    globals[name] = g;
  }
private:
  llvm::StringMap<Global> globals;
  LLVM_NODISCARD bool isFunc(decltype(globals)::iterator it) const {
    return it->second.protoTypes.get<FunFnTy>() != nullptr;
  }
};
#endif

// this should be moved out of here eventually
bool isFileType(const MonoTypePtr&);

#if (LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 6 || LLVM_VERSION_MAJOR >= 8) && LLVM_VERSION_MAJOR < 11
class jitmm : public llvm::SectionMemoryManager {
public:
  explicit jitmm(jitcc* jit) : jit(jit) { }

  // link symbols across modules :T
  uint64_t getSymbolAddress(const std::string& n) override {
    if (uint64_t laddr = reinterpret_cast<uint64_t>(this->jit->getSymbolAddress(n))) {
      return laddr;
    }
    if (!n.empty() && n[0] == '_') {
      uint64_t sv = reinterpret_cast<uint64_t>(this->jit->getSymbolAddress(n.substr(1)));
      if (sv != 0U) return sv;
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

#if LLVM_VERSION_MAJOR >= 11
jitcc::jitcc(const TEnvPtr& tenv)
    : tenv(tenv), vtenv(std::make_unique<VTEnv>()), ignoreLocalScope(false),
      globals(std::make_unique<Globals>()), globalData(32768 /* min global page size = 32K */),
      constants(std::make_unique<ConstantList>()) {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmParser();
  llvm::InitializeNativeTargetAsmPrinter();

  this->orcjit = llvm::cantFail(ORCJIT::create(), "orcjit init failed");

  // allocate an IR builder with an initial dummy basic-block to write into
  this->irbuilder = withContext([](llvm::LLVMContext& c) {
      auto r = std::make_unique<llvm::IRBuilder<>>(c);
      r->SetInsertPoint(llvm::BasicBlock::Create(c, "dummy"));
      return r;
  });

  // make sure we've always got one frame for variable defs
  this->vtenv->pushScope();
}

jitcc::~jitcc() {
  // release low-level functions
  for (auto f : this->fenv) {
    delete f.second;
  }
}
#else

jitcc::jitcc(const TEnvPtr& tenv) :
  tenv(tenv),
  ignoreLocalScope(false),
  globalData(32768 /* min global page size = 32K */)
{
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmParser();
  llvm::InitializeNativeTargetAsmPrinter();

  // allocate an IR builder with an initial dummy basic-block to write into
  this->irbuilder = withContext([](llvm::LLVMContext& c) {
      auto r = std::make_unique<llvm::IRBuilder<>>(c);
      r->SetInsertPoint(llvm::BasicBlock::Create(c, "dummy"));
      return r;
  });

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

#if LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 6 || LLVM_VERSION_MAJOR >= 8
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
#if LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 6 || LLVM_VERSION_MAJOR >= 8
  for (auto ee : this->eengines) {
    delete ee;
  }
  delete this->currentModule;
#elif LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
  delete this->eengine;
#endif
}
#endif

const TEnvPtr& jitcc::typeEnv() const {
  return this->tenv;
}

llvm::IRBuilder<>* jitcc::builder() const {
  return this->irbuilder.get();
}

#if LLVM_VERSION_MAJOR >= 11
llvm::Module* jitcc::module() {
  if (this->currentModule == nullptr) {
    this->currentModule = withContext([](llvm::LLVMContext& c) {
      auto m = std::make_unique<llvm::Module>(createUniqueName("jitModule"), c);
      return m;
    });
  }
  return this->currentModule.get();
}
#else
llvm::Module* jitcc::module() {
  if (this->currentModule == nullptr) {
    this->currentModule = withContext([this](llvm::LLVMContext& c) {
      return new llvm::Module("jitModule" + str::from(this->modules.size()), c);
    });
    this->modules.push_back(this->currentModule);
  }
  return this->currentModule;
}
#endif

#if LLVM_VERSION_MAJOR >= 11
void* jitcc::getMachineCode(llvm::Function* f, llvm::JITEventListener* /*listener*/) {
  const std::string fname = f->getName().str();
  auto sym = orcjit->lookup(fname);
  if (auto e = sym.takeError()) {
    llvm::consumeError(std::move(e));
  } else {
    return llvm::jitTargetAddressToPointer<void*>(sym->getAddress());
  }

  llvm::errs() << this->currentModule->getName() << " :";
  this->currentModule->print(llvm::dbgs(), nullptr, /*ShouldPreserveUseListOrder=*/false, /*IsForDebug=*/true);
  if (auto e = orcjit->addModule(std::move(this->currentModule))) {
    throw std::runtime_error(", cannot add module");
  }

  sym = orcjit->lookup(fname);
  if (auto e = sym.takeError()) {
    logAllUnhandledErrors(std::move(e), llvm::errs());
    throw std::runtime_error(
        "Internal compiler error, no current module and no machine code for function (" + fname +
        ")");
  }

  return llvm::jitTargetAddressToPointer<void*>(sym->getAddress());
}
#else
void* jitcc::getSymbolAddress(const std::string& vn) {
  // do we have a global with this name?
  auto gd = this->globals.find(vn);
  if (gd != this->globals.end()) {
    return gd->second.value;
  }

  // do we have a compiled function with this name?
#if LLVM_VERSION_MAJOR == 6 || LLVM_VERSION_MAJOR >= 8
  for (auto ee : this->eengines) {
    if (ee->FindFunctionNamed(vn) || ee->FindGlobalVariableNamed(vn)) {
      if (uint64_t faddr = ee->getFunctionAddress(vn)) {
        return reinterpret_cast<void*>(faddr);
      }
    }
  }
#elif LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4
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
#if LLVM_VERSION_MAJOR == 6 || LLVM_VERSION_MAJOR >= 8
    m->print(llvm::dbgs(), nullptr, /*ShouldPreserveUseListOrder=*/false, /*IsForDebug=*/true);
#else
    m->dump();
#endif
  }
}

void* jitcc::getMachineCode(llvm::Function* f, llvm::JITEventListener* listener) {
#if LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 6 || LLVM_VERSION_MAJOR >= 8
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
  this->currentModule->print(llvm::dbgs(), nullptr, /*ShouldPreserveUseListOrder=*/false, /*IsForDebug=*/true);
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
#if LLVM_VERSION_MAJOR >= 8
  // todo: smunix: what do we substitute this with?
#else
  fpm.add(llvm::createInstructionCombiningPass());
#endif
  fpm.add(llvm::createReassociatePass());
#if LLVM_VERSION_MAJOR == 6 || LLVM_VERSION_MAJOR >= 8
  fpm.add(llvm::createNewGVNPass());
#else
  fpm.add(llvm::createGVNPass());
#endif
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
#endif

#if LLVM_VERSION_MINOR >= 7 || LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 6 || LLVM_VERSION_MAJOR >= 8
// get the machine code produced for a given expression
// (there must be a simpler way)
class LenWatch : public llvm::JITEventListener {
public:
  std::string fname;
  size_t sz;
  explicit LenWatch(const std::string& fname) : fname(fname), sz(0) { }
  size_t size() const { return this->sz; }
  void NotifyObjectEmitted(const llvm::object::ObjectFile& o, const llvm::RuntimeDyld::LoadedObjectInfo&) {
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
  void NotifyObjectEmitted(const llvm::object::ObjectFile& o, const llvm::RuntimeDyld::LoadedObjectInfo&) {
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
  virtual void NotifyFunctionEmitted(const llvm::Function& f, void*, size_t sz, const llvm::JITEventListener::EmittedFunctionDetails&) {
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

#if LLVM_VERSION_MAJOR >= 11
bool jitcc::isDefined(const std::string& vn) const {
  if (this->globals->hasName(vn)) {
    return true;
  }
  if (this->constants->hasName(vn)) {
    return true;
  }
  if (lookupOp(vn) != nullptr) {
    return true;
  }
  if (this->vtenv->hasName(vn)) {
    return true;
  }
  return false;
}
#else
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
#endif

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

#if LLVM_VERSION_MAJOR >= 11
void jitcc::bindGlobal(const std::string& vn, const MonoTypePtr& ty, void* x) {
  assert(not this->globals->hasName(vn));

  void* value = x;
  if (is<Func>(ty) != nullptr) {
    this->globals->addFunc(vn, *this->module(),
                           llvm::cast<llvm::FunctionType>(toLLVM(ty, /*asArg=*/false)),
                           /*existingF=*/nullptr);
  } else {
    if (hasPointerRep(ty) || isFileType(ty)) {
      void** p = reinterpret_cast<void**>(this->globalData.malloc(sizeof(void*)));
      *p = x;
      value = p;
    }

    this->globals->addVar(vn, *this->module(), toLLVM(ty, /*asArg=*/true));
  }
  llvm::cantFail(orcjit->addExternalSymbol(vn, value));
}
#else
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

    g.ref.var = prepgv(new llvm::GlobalVariable(
                         *module(),
                         toLLVM(ty, true),
                         false,
                         llvm::GlobalValue::ExternalLinkage,
                         0,
                         vn
                       ),
                       sizeof(void*));

#if LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
  this->eengine->addGlobalMapping(g.ref.var, g.value);
#endif
  }
  this->globals[vn] = g;
}
#endif

#if LLVM_VERSION_MAJOR < 11
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
#endif

#if LLVM_VERSION_MAJOR >= 11
llvm::GlobalVariable* jitcc::maybeRefGlobal(const std::string& vn) {
  if (auto* gv = this->globals->getVar(vn, *this->module())) {
    //return refGlobal(vn, gv);
    return gv;
  }
  return nullptr;
}
#else
llvm::GlobalVariable* jitcc::maybeRefGlobal(const std::string& vn) {
  auto gv = this->globals.find(vn);
  if (gv != this->globals.end() && !is<Func>(gv->second.type)) {
    return refGlobal(vn, gv->second.ref.var);
  }
  return 0;
}
#endif

#if LLVM_VERSION_MAJOR < 11
llvm::GlobalVariable* jitcc::refGlobal(const std::string& vn, llvm::GlobalVariable* gv) {
  llvm::Module* mod = module();

  if (!gv) {
    return 0;
  } else if (gv->getParent() == mod) {
    return gv;
  } else if (llvm::GlobalVariable* rgv = mod->getGlobalVariable(vn)) {
    return rgv;
  } else {
    return prepgv(new llvm::GlobalVariable(
                    *mod,
                    gv->getType()->getElementType(),
                    gv->isConstant(),
                    llvm::GlobalVariable::ExternalLinkage,
                    0,
                    vn),
                  sizeof(void*));
  }
}
#endif

llvm::GlobalVariable* jitcc::lookupVarRef(const std::string& vn) {
  // if any local variables shadow this global, hide it
#if LLVM_VERSION_MAJOR >= 11
  if (this->vtenv->hasName(vn)) {
    return nullptr;
  }
#else
  for (auto vbs : this->vtenv) {
    if (vbs.find(vn) != vbs.end()) {
      return 0;
    }
  }
#endif

  // now if we've got a global with this name, we can get a pointer to it
  return maybeRefGlobal(vn);
}

#if LLVM_VERSION_MAJOR >= 11
llvm::Value* jitcc::loadConstant(const std::string& vn) {
  return this->constants->loadConstant(vn, *module(), *builder());
}
#else
llvm::Value* jitcc::loadConstant(const std::string& vn) {
  auto cv = this->constants.find(vn);
  if (cv != this->constants.end()) {
    return withContext([&](auto&) -> llvm::Value* {
        if (cv->second.ref == nullptr) {
        }
      if (is<Array>(cv->second.mtype)) {
        return builder()->CreateLoad(refGlobal(vn, cv->second.ref));
      } else if (llvm::Value* r = refGlobal(vn, cv->second.ref)) {
#if   LLVM_VERSION_MAJOR <= 10
        return hasPointerRep(cv->second.mtype) ? r : builder()->CreateLoad(r);
#elif LLVM_VERSION_MAJOR <= 12 
        return hasPointerRep(cv->second.mtype) ? r : builder()->CreateAlignedLoad(r->getType()->getPointerElementType(), r, llvm::MaybeAlign(8));
#endif
      } else {
        return cv->second.value;
      }
    });
  }
  return nullptr;
}
#endif

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
    if (is<Func>(uety) == nullptr) {
    }else {
    }
#if LLVM_VERSION_MAJOR >= 11
    this->constants->add(vname, *module(), c, uety);
#else
    Constant& cv = this->constants[vname];
    cv.value = c;
    cv.type  = toLLVM(uety);
    cv.mtype = uety;

    if (is<Func>(uety)) {
      // functions are loaded by name rather than by constant value
      cv.ref = 0;
    } else {
      cv.ref = new llvm::GlobalVariable(*module(), cv.type, true, llvm::GlobalVariable::ExternalLinkage, c, vname);
    }
#endif
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
    llvm::BasicBlock* ibb = withContext([this](auto&) { return this->builder()->GetInsertBlock(); });
    llvm::Function* initfn = allocFunction("." + vname + "_init", MonoTypes(), primty("unit"));
    if (initfn == nullptr) {
      throw annotated_error(*ue, "Failed to allocate initializer function for '" + vname + "'.");
    }
    withContext([&](llvm::LLVMContext& c) {
      llvm::BasicBlock* bb = llvm::BasicBlock::Create(c, "entry", initfn);
      this->builder()->SetInsertPoint(bb);

      compile(assign(var(vname, uety, ue->la()), ue, ue->la()));
      this->builder()->CreateRetVoid();
    });

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
    withContext([this, ibb](auto&) { this->builder()->SetInsertPoint(ibb); });
#if LLVM_VERSION_MAJOR < 11
    initfn->eraseFromParent();
#endif
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

void* jitcc::memalloc(size_t sz, size_t asz) {
  size_t r = pushGlobalRegion();
  void* result = ::hobbes::memalloc(sz, asz);
  popGlobalRegion(r);
  return result;
}

llvm::Value* jitcc::lookupVar(const std::string& vn, const MonoTypePtr& vty) {
  // all units are the same
  if (isUnit(vty)) {
    return cvalue(true);
  }

  // try to find this variable up the local variable stack (unless we're ignoring local scope)
#if LLVM_VERSION_MAJOR >= 11
  if (!this->ignoreLocalScope) {
    if (auto* fn = this->vtenv->get(vn, *this->module())) {
      return fn;
    }
  }
#else
  if (!this->ignoreLocalScope) {
    for (size_t i = 0; i < this->vtenv.size(); ++i) {
      const VarBindings& vbs = this->vtenv[this->vtenv.size() - (i + 1)];
      auto vb = vbs.find(vn);
      if (vb != vbs.end()) {
        return maybeRefGlobalV(vb->second);
      }
    }
  }
#endif

  // try to find this variable as a global
  if (llvm::GlobalVariable* gv = maybeRefGlobal(vn)) {
    return withContext([this, gv](auto&) { return builder()->CreateLoad(gv); });
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

#if LLVM_VERSION_MAJOR >= 11
void jitcc::pushScope() {
  this->vtenv->pushScope();
}

void jitcc::bindScope(const std::string& vn, llvm::Value* v) {
  this->vtenv->add(vn, v);
}

void jitcc::popScope() {
  this->vtenv->popScope();
}
#else
void jitcc::pushScope() {
  this->vtenv.push_back(VarBindings());
}

void jitcc::bindScope(const std::string& vn, llvm::Value* v) {
  this->vtenv.back()[vn] = v;
}

void jitcc::popScope() {
  this->vtenv.pop_back();
}
#endif

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

#if LLVM_VERSION_MAJOR >= 11
llvm::Function* jitcc::lookupFunction(const std::string& fn) {
  if (auto* f = this->module()->getFunction(fn)) {
    return f;
  }

  if (auto* func = this->globals->getFunc(fn, *this->module())) {
    return func;
  }
  return nullptr;
}
#else
llvm::Function* jitcc::lookupFunction(const std::string& fn) {
  llvm::Module* thisMod = module();

  for (size_t i = 0; i<this->modules.size(); ++i) {
    auto m = this->modules[i];
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
#endif
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
  llvm::BasicBlock* ibb = withContext([this](auto&) { return this->builder()->GetInsertBlock(); });

  // prepare the environment for these mutually-recursive definitions
  for (size_t f = 0; f < fs.size(); ++f) {
    llvm::Function* fval = allocFunction(fs[f].name.empty() ? ("/" + freshName()) : fs[f].name, fs[f].argtys, requireMonotype(this->tenv, fs[f].exp));
    if (fval == 0) {
      throw std::runtime_error("Failed to allocate function");
    }

#if LLVM_VERSION_MAJOR >= 11
    this->vtenv->add(fs[f].name, fval);
#else
    this->vtenv.back()[fs[f].name] = fval;
#endif
    fs[f].result = fval;
  }

  // now compile each function
  for (size_t f = 0; f < fs.size(); ++f) {
    llvm::Function*   fval = fs[f].result;
    const UCF&        ucf  = fs[f];
    MonoTypePtr       rty  = requireMonotype(this->tenv, ucf.exp);
    llvm::BasicBlock* bb = withContext(
        [fval](llvm::LLVMContext& c) { return llvm::BasicBlock::Create(c, "entry", fval); });

    withContext([this, bb](auto&) { return this->builder()->SetInsertPoint(bb); });

    // set argument names for safe referencing here
#if LLVM_VERSION_MAJOR >= 11
    this->vtenv->pushScope();
#else
    this->vtenv.push_back(VarBindings());
#endif

    llvm::Function::arg_iterator a = fval->arg_begin();
    for (unsigned int i = 0; i < ucf.argns.size(); ++i) {
      if (isUnit(ucf.argtys[i])) {
#if LLVM_VERSION_MAJOR >= 11
        this->vtenv->add(ucf.argns[i], cvalue(true)); // this should never even be seen
#else
        this->vtenv.back()[ucf.argns[i]] = cvalue(true); // this should never even be seen
#endif
      } else {
        a->setName(ucf.argns[i]);

        llvm::Value* argv = &*a;
        if (is<FixedArray>(ucf.argtys[i])) {
          argv = withContext([&](auto&) {
            return cast(this->builder(), ptrType(toLLVM(ucf.argtys[i], false)), argv);
          });
        }
#if LLVM_VERSION_MAJOR >= 11
        this->vtenv->add(ucf.argns[i], argv);
#else
        this->vtenv.back()[ucf.argns[i]] = argv;
#endif
        ++a;
      }
    }

    withContext([&](auto&) {
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
#if LLVM_VERSION_MAJOR >= 11
        this->vtenv->popScope();
#else
        this->popScope();
        //this->vtenv.pop_back();
#endif
        if (ibb != 0) { this->builder()->SetInsertPoint(ibb); }
      } catch (...) {
        if (ibb != 0) { this->builder()->SetInsertPoint(ibb); }
#if LLVM_VERSION_MAJOR >= 11
        this->vtenv->popScope();
#else
        this->popScope();
        //this->vtenv.pop_back();
#endif
        throw;
      }
    });
  }
}

llvm::Value* jitcc::compileAllocStmt(llvm::Value* sz, llvm::Value* asz, llvm::Type* mty, bool zeroMem) {
  llvm::Function* f = lookupFunction(zeroMem ? "mallocz" : "malloc");
  if (!f) throw std::runtime_error("Expected heap allocation function as call.");
  return withContext([&](auto&) {
    return builder()->CreateBitCast(fncall(builder(), f, f->getFunctionType(), list(sz, asz)), mty);
  });
}

llvm::Value* jitcc::compileAllocStmt(size_t sz, size_t asz, llvm::Type* mty, bool zeroMem) {
  return compileAllocStmt(cvalue(static_cast<long>(sz)), cvalue(static_cast<long>(asz)), mty, zeroMem);
}

void jitcc::releaseMachineCode(void*) {
}

#if LLVM_VERSION_MAJOR >= 11
llvm::Function* jitcc::allocFunction(const std::string& fname, const MonoTypes& argl, const MonoTypePtr& rty) {
  const auto f = [=](llvm::Module& m) {
    return llvm::Function::Create(
        llvm::FunctionType::get(toLLVM(rty, true), toLLVM(argl, true), false),
        llvm::Function::ExternalLinkage, fname, m);
  };
  llvm::Function* ret = f(*this->module());
  if (fname.find(".rfn.t") != std::string::npos) {
    this->vtenv->add(fname, f, ret);
  } else if (! this->globals->hasName(fname)) {
    this->globals->addFunc(fname, *this->module(),
                           llvm::FunctionType::get(toLLVM(rty, true), toLLVM(argl, true), false),
                           ret);
  }
  return ret;
}
#else
llvm::Function* jitcc::allocFunction(const std::string& fname, const MonoTypes& argl, const MonoTypePtr& rty) {
  return
    llvm::Function::Create(
      llvm::FunctionType::get(toLLVM(rty, true), toLLVM(argl, true), false),
      llvm::Function::ExternalLinkage,
      fname,
      module()
    );
}
#endif

void* jitcc::reifyMachineCodeForFn(const MonoTypePtr&, const str::seq& names, const MonoTypes& tys, const ExprPtr& exp) {
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
      r.push_back(withContext([&](auto&) {
          return c->builder()->CreateBitCast(ev, toLLVM(et));
      }));
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
