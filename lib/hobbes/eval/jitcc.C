
#include <hobbes/eval/func.H>
#include <hobbes/util/llvm.H>
#include <hobbes/eval/cexpr.H>
#include <hobbes/eval/jitcc.H>
#include <hobbes/hobbes.H>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/ValueHandle.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/Compiler.h>
#include <llvm/Support/Error.h>
#include <llvm/Target/TargetMachine.h>
#include <stdexcept>
#include <utility>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wctor-dtor-privacy"

#if LLVM_VERSION_MAJOR >= 11
#include <hobbes/eval/orcjitcc.H>

#include <cstdio>
#include <cassert>
#include <type_traits>
#include <new>
#include <memory>
#include <limits>
#else
#if LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
#include <llvm/ExecutionEngine/JIT.h>
#else
#include <llvm/ExecutionEngine/MCJIT.h>
#endif

#if LLVM_VERSION_MINOR >= 8 || LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 6
#include <llvm/Analysis/BasicAliasAnalysis.h>
#endif

#if LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 6
#include <llvm/Transforms/Scalar/GVN.h>
#endif
#endif

#include <llvm/Object/ELFObjectFile.h>
#include <llvm/ExecutionEngine/JITEventListener.h>

#pragma GCC diagnostic pop

#if LLVM_VERSION_MAJOR >= 11
namespace {
template <typename T, typename... Ts> struct hasType {
  static constexpr bool value = false;
};

template <typename T, typename H, typename... Ts> struct hasType<T, H, Ts...> {
  static constexpr bool value =
      std::is_same<T, H>::value || hasType<T, Ts...>::value;
};

template <typename... Ts> struct hasDuplication {
  static constexpr bool value = false;
};

template <typename T, typename... Ts> struct hasDuplication<T, Ts...> {
  static constexpr bool value =
      hasType<T, Ts...>::value || hasDuplication<Ts...>::value;
};

template <typename T, typename... Ts> struct IndexByType;

template <typename T, typename... Ts> struct IndexByType<T, T, Ts...> {
  static constexpr int value = 0;
};

template <typename T, typename U, typename... Ts>
struct IndexByType<T, U, Ts...> {
  static constexpr int value = 1 + IndexByType<T, Ts...>::value;
};

template <typename U, typename... Ts> struct VariantLiteTypeMatch {
  static bool act(std::size_t, std::size_t) { return false; }
};

template <typename U, typename T, typename... Ts>
struct VariantLiteTypeMatch<U, T, Ts...> {
  static bool act(std::size_t N, std::size_t tag) {
    if (tag == (N - sizeof...(Ts) - 1U)) {
      return std::is_same<U, T>::value;
    } else {
      return VariantLiteTypeMatch<U, Ts...>::act(N, tag);
    }
  }
};

template <typename... Ts> struct VariantLiteDeletor {
  static void act(std::size_t, std::size_t, void *) {}
};

template <typename T, typename... Ts> struct VariantLiteDeletor<T, Ts...> {
  static void act(std::size_t N, std::size_t tag, void *storage) {
    if (tag == (N - sizeof...(Ts) - 1)) {
      reinterpret_cast<T *>(storage)->~T();
    } else {
      VariantLiteDeletor<Ts...>::act(N, tag, storage);
    }
  }
};

template <typename... Ts> struct VariantLiteCopyCreator {
  static void act(std::size_t, std::size_t, void *, const void *) {}
};

template <typename T, typename... Ts> struct VariantLiteCopyCreator<T, Ts...> {
  static void act(std::size_t N, std::size_t other_tag, void *this_storage,
                  const void *other_storage) {
    if (other_tag == (N - sizeof...(Ts) - 1)) {
      new (this_storage) T(*reinterpret_cast<const T *>(other_storage));
    } else {
      VariantLiteCopyCreator<Ts...>::act(N, other_tag, this_storage,
                                         other_storage);
    }
  }
};

template <typename... Ts> struct VariantLiteMoveCreator {
  static void act(std::size_t, std::size_t, void *, const void *) {}
};

template <typename T, typename... Ts> struct VariantLiteMoveCreator<T, Ts...> {
  static void act(std::size_t N, std::size_t other_tag, void *this_storage,
                  const void *other_storage) {
    if (other_tag == (N - sizeof...(Ts) - 1)) {
      new (this_storage)
          T(std::move(*reinterpret_cast<const T *>(other_storage)));
    } else {
      VariantLiteMoveCreator<Ts...>::act(N, other_tag, this_storage,
                                         other_storage);
    }
  }
};

template <typename... Ts> struct VariantLite {
  static constexpr std::size_t N = sizeof...(Ts);

  static_assert(N > 0, "");
  static_assert(not hasDuplication<Ts...>::value, "");

  VariantLite() = default;

  template <typename T> VariantLite(T t) {
    static_assert(hasType<T, Ts...>::value, "");
    new (&storage) T(std::move(t));
    tag = IndexByType<T, Ts...>::value;
  }

  VariantLite(const VariantLite &rhs) {
    tag = rhs.tag;
    if (isValid()) {
      VariantLiteCopyCreator<Ts...>::act(N, rhs.tag, &storage, &rhs.storage);
    }
  }

  VariantLite &operator=(const VariantLite &rhs) & {
    if (isValid()) {
      VariantLiteDeletor<Ts...>::act(N, tag, &storage);
    }
    tag = rhs.tag;
    if (isValid()) {
      VariantLiteCopyCreator<Ts...>::act(N, rhs.tag, &storage, &rhs.storage);
    }
    return *this;
  }

  VariantLite(VariantLite &&rhs) noexcept {
    tag = rhs.tag;
    if (isValid()) {
      VariantLiteMoveCreator<Ts...>::act(N, rhs.tag, &storage, &rhs.storage);
    }
  }

  VariantLite &operator=(VariantLite &&rhs) & noexcept {
    if (isValid()) {
      VariantLiteDeletor<Ts...>::act(N, tag, &storage);
    }
    tag = rhs.tag;
    if (isValid()) {
      VariantLiteMoveCreator<Ts...>::act(N, rhs.tag, &storage, &rhs.storage);
    }
    return *this;
  }

  template <typename T> VariantLite &operator=(T t) {
    static_assert(hasType<T, Ts...>::value, "");
    if (isValid()) {
      VariantLiteDeletor<Ts...>::act(N, tag, &storage);
    }
    new (&storage) T(std::move(t));
    tag = IndexByType<T, Ts...>::value;
    return *this;
  }

  ~VariantLite() {
    if (isValid()) {
      VariantLiteDeletor<Ts...>::act(N, tag, &storage);
    }
  }

  bool isValid() const { return tag != InvalidTag; }

  template <typename T> T *get() {
    static_assert(hasType<T, Ts...>::value, "");
    if (isValid() && VariantLiteTypeMatch<T, Ts...>::act(N, tag)) {
      return reinterpret_cast<T *>(&storage);
    }
    return nullptr;
  }

private:
  static constexpr std::size_t InvalidTag =
      std::numeric_limits<std::size_t>::max();

  std::aligned_union_t<0, Ts...> storage;
  std::size_t tag = InvalidTag;
};

LLVM_NODISCARD llvm::Function *createFnDecl(llvm::Function *f, llvm::Module &m,
                                            llvm::StringRef name) {
  if (f->getReturnType() == hobbes::boolType()) {
    f->addAttribute(llvm::AttributeList::ReturnIndex, llvm::Attribute::ZExt);
  }
  if (f->getName() != name) {
    f->eraseFromParent();
    return m.getFunction(name);
  }
  return f;
}

LLVM_NODISCARD llvm::GlobalVariable *
createGVDecl(llvm::GlobalVariable *gv, llvm::Module &m, llvm::StringRef name) {
  if (gv->getName() != name) {
    gv->eraseFromParent();
    return m.getGlobalVariable(name);
  }
  return gv;
}
} // namespace
#endif

namespace hobbes {
#if LLVM_VERSION_MAJOR >= 11
class ConstantList {
  using VarDeclFnTy = std::function<llvm::GlobalVariable *(llvm::Module &)>;
  enum class Ty : std::int8_t {
    Function,
    GlobalVar,
    Array,
    HasPointerRep,
    Other,
  };
  struct Constant {
    // function is a constant
    // globalvariable returned by a function
    VariantLite<VarDeclFnTy, llvm::Constant *> value;
    Ty ty = Ty::Other;
  };
  llvm::StringMap<Constant> constants;

public:
  ConstantList() = default;
  ConstantList(const ConstantList &) = delete;
  ConstantList(ConstantList &&) = delete;
  ConstantList &operator=(const ConstantList &) = delete;
  ConstantList &operator=(ConstantList &&) = delete;
  ~ConstantList() = default;

  LLVM_NODISCARD bool contains(llvm::StringRef name) const {
    return constants.find(name) != constants.end();
  }

  void storeFnAddr(const std::string &name, llvm::Constant *funcAddr);
  void createGVDef(const std::string &name, llvm::Module &m,
                   llvm::Constant *initVal, const MonoTypePtr &mtype);

  /// Gets constant by generating load inst or possibly recreating decl
  LLVM_NODISCARD llvm::Value *loadConstant(llvm::StringRef name,
                                           llvm::Module &m,
                                           llvm::IRBuilder<> &builder);
};

void ConstantList::storeFnAddr(const std::string &name,
                               llvm::Constant *funcAddr) {
  constants[name] = Constant{.value = funcAddr, .ty = Ty::Function};
}

void ConstantList::createGVDef(const std::string &name, llvm::Module &m,
                               llvm::Constant *initVal,
                               const MonoTypePtr &mtype) {
  llvm::Type *type = toLLVM(mtype);
  new llvm::GlobalVariable(m, type, /*isConstant=*/true,
                           llvm::GlobalVariable::ExternalLinkage, initVal,
                           name);

  const auto vf = [&, type, name](llvm::Module &m) -> llvm::GlobalVariable * {
    return createGVDecl(
        prepgv(new llvm::GlobalVariable(m, type, /*isConstant=*/true,
                                        llvm::GlobalVariable::ExternalLinkage,
                                        /*initializer=*/nullptr, name)),
        m, name);
  };
  const auto tf = [&mtype] {
    if (is<Array>(mtype) != nullptr) {
      return Ty::Array;
    }
    if (hasPointerRep(mtype)) {
      return Ty::HasPointerRep;
    }
    return Ty::Other;
  };

  constants[name] = Constant{
      .value = VarDeclFnTy(vf),
      .ty = tf(),
  };
}

llvm::Value *ConstantList::loadConstant(llvm::StringRef name, llvm::Module &m,
                                        llvm::IRBuilder<> &builder) {
  const auto it = constants.find(name);
  if (it == constants.end()) {
    return nullptr;
  }

  if (it->second.ty == Ty::Function) {
    return *it->second.value.get<llvm::Constant *>();
  }

  return withContext([&](auto &) -> llvm::Value * {
    auto *g = llvm::cast<llvm::GlobalVariable>(
        static_cast<llvm::Value *>((*it->second.value.get<VarDeclFnTy>())(m)));

    switch (it->second.ty) {
    case Ty::Array:
      return builder.CreateLoad(g);
    case Ty::HasPointerRep:
      return g;
    default:
      return builder.CreateAlignedLoad(g->getType()->getPointerElementType(), g,
                                       llvm::MaybeAlign(8));
    }
  });
}

class VTEnv {
public:
  using FnDeclFnTy = std::function<llvm::Function *(llvm::Module &)>;

private:
  using ValOrFnTy = VariantLite<FnDeclFnTy, llvm::WeakTrackingVH>;
  using VarBindingStack = llvm::SmallVector<llvm::StringMap<ValOrFnTy>, 8>;
  VarBindingStack vtenv;

public:
  VTEnv() = default;
  VTEnv(const VTEnv &) = delete;
  VTEnv(VTEnv &&) = delete;
  VTEnv &operator=(const VTEnv &) = delete;
  VTEnv &operator=(VTEnv &&) = delete;
  ~VTEnv() = default;

  LLVM_NODISCARD bool contains(llvm::StringRef name) const {
    return std::any_of(vtenv.rbegin(), vtenv.rend(), [name](const auto &vb) {
      return vb.find(name) != vb.end();
    });
  }

  /// Gets \p name by looking up from inner to outer scope
  ///
  /// Possibly recreating decl
  LLVM_NODISCARD llvm::Value *getOrCreateDecl(llvm::StringRef name,
                                              llvm::Module &m);

  /// Adds \p name to current scope
  void add(llvm::StringRef name, llvm::Value *v) {
    // do not allow value overwritten function prototype
    // otherwise some decls will not created correctly
    if (this->vtenv.back().find(name) == this->vtenv.back().end()) {
      this->vtenv.back()[name] = llvm::WeakTrackingVH(v);
    }
  }

  /// Adds function \p name to current scope
  ///
  /// It stores function prototype for decl recreating
  void add(const std::string &name, FnDeclFnTy f) {
    this->vtenv.back()[name] = FnDeclFnTy(
        [name, f = std::move(f)](llvm::Module &m) -> llvm::Function * {
          return createFnDecl(f(m), m, name);
        });
  }

  /// Create a new inner scope
  void pushScope() { vtenv.emplace_back(); }

  /// Destroys the inner scope
  void popScope() { vtenv.pop_back(); }
};

llvm::Value *VTEnv::getOrCreateDecl(llvm::StringRef name, llvm::Module &m) {
  for (auto vb = vtenv.rbegin(); vb != vtenv.rend(); ++vb) {
    const auto it = vb->find(name);
    if (it == vb->end()) {
      continue;
    }

    if (auto *const f = it->second.get<FnDeclFnTy>()) {
      return (*f)(m);
    }

    auto *p = it->second.get<llvm::WeakTrackingVH>();
    if (!p->pointsToAliveValue()) {
      throw std::runtime_error((name + " has been invalidated").str());
    }
    return *p;
  }
  return nullptr;
}

class Globals {
private:
  using FnDeclFnTy = std::function<llvm::Function *(llvm::Module &)>;
  using VarDeclFnTy = std::function<llvm::GlobalVariable *(llvm::Module &)>;
  llvm::StringMap<VariantLite<FnDeclFnTy, VarDeclFnTy>> globals;

  LLVM_NODISCARD bool isFunc(decltype(globals)::iterator it) const {
    return it->second.get<FnDeclFnTy>() != nullptr;
  }

public:
  Globals() = default;
  Globals(const Globals &) = delete;
  Globals(Globals &&) = delete;
  Globals &operator=(const Globals &) = delete;
  Globals &operator=(Globals &&) = delete;
  ~Globals() = default;

  LLVM_NODISCARD bool contains(llvm::StringRef name) const {
    return globals.find(name) != globals.end();
  }

  /// Gets a global variable \p name by possibly recreating decl in current
  /// module
  LLVM_NODISCARD llvm::GlobalVariable *getOrCreateVarDecl(llvm::StringRef name,
                                                          llvm::Module &m);
  /// Gets a global function \p name by possibly recreating decl in current
  /// module
  LLVM_NODISCARD llvm::Function *getOrCreateFuncDecl(llvm::StringRef name,
                                                     llvm::Module &m);

  /// Creates a global variable decl for \p name with type \p ty
  ///
  /// It stores variable prototype for decl recreating
  void add(const std::string &name, llvm::Type *ty);
  /// Creates a global function decl for \p name with type \p ty if \p existingF
  /// is null
  ///
  /// It stores function prototype for decl recreating
  void add(const std::string &name, llvm::FunctionType *ty);
};

llvm::GlobalVariable *Globals::getOrCreateVarDecl(llvm::StringRef name,
                                                  llvm::Module &m) {
  const auto it = globals.find(name);
  if (it == globals.end() || isFunc(it)) {
    return nullptr;
  }

  auto *p = (*it->second.get<VarDeclFnTy>())(m);
  return llvm::cast<llvm::GlobalVariable>(static_cast<llvm::Value *>(p));
}

llvm::Function *Globals::getOrCreateFuncDecl(llvm::StringRef name,
                                             llvm::Module &m) {
  const auto it = globals.find(name);
  if (it == globals.end() || !isFunc(it)) {
    return nullptr;
  }

  auto *p = (*it->second.get<FnDeclFnTy>())(m);
  return llvm::cast<llvm::Function>(static_cast<llvm::Value *>(p));
}

void Globals::add(const std::string &name, llvm::Type *ty) {
  globals[name] = VarDeclFnTy([ty, name](llvm::Module &m) {
    return createGVDecl(
        prepgv(new llvm::GlobalVariable(m, ty, /*isConstant=*/false,
                                        llvm::GlobalValue::ExternalLinkage,
                                        /*initializer=*/nullptr, name),
               sizeof(void *)),
        m, name);
  });
}

void Globals::add(const std::string &name, llvm::FunctionType *ty) {
  globals[name] = FnDeclFnTy([ty, name](llvm::Module &m) {
    return createFnDecl(
        llvm::Function::Create(ty, llvm::Function::ExternalLinkage, name, m), m,
        name);
  });
}
#endif

// this should be moved out of here eventually
bool isFileType(const MonoTypePtr&);

#if (LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 6 || LLVM_VERSION_MAJOR >= 8) && LLVM_VERSION_MAJOR < 11
class jitmm : public llvm::SectionMemoryManager {
public:
  explicit jitmm(jitcc * jit) : jit(jit) {}

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

  this->orcjit = std::make_unique<ORCJIT>();

  // allocate an IR builder with an initial dummy basic-block to write into
  this->irbuilder = withContext([](llvm::LLVMContext& c) {
      auto r = std::make_unique<llvm::IRBuilder<>>(c);
      r->SetInsertPoint(llvm::BasicBlock::Create(c, "dummy"));
      return r;
  });

  // make sure we've always got one frame for variable defs
  this->pushScope();
}

jitcc::~jitcc() {
  // release low-level functions
  for (const auto& f : this->fenv) {
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
  this->fpm = new llvm::legacy::FunctionPassManager(module());
  this->fpm->add(new llvm::DataLayoutPass(*this->eengine->getDataLayout()));
#else
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
llvm::Module *jitcc::module() {
  if (this->currentModule == nullptr) {
    this->currentModule = withContext([](llvm::LLVMContext &c) {
      static std::atomic_int mc{};
      return std::make_unique<llvm::Module>("jitModule" + std::to_string(mc++), c);
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

  withContext([&](auto&) {
    const std::string name = this->currentModule->getName().str();
    if (auto e = orcjit->addModule(std::move(this->currentModule))) {
      llvm::logAllUnhandledErrors(std::move(e), llvm::errs());
      throw std::runtime_error("cannot add module " + name);
    }
  });

  sym = orcjit->lookup(fname);
  if (auto e = sym.takeError()) {
    logAllUnhandledErrors(std::move(e), llvm::errs());
    throw std::runtime_error(
        "Internal compiler error, no current module and no machine code for (" + fname + ")");
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

  // may apply FunctionInliningPass depends upon some "scores"
  maybeInlineFunctionsIn(*this->currentModule);

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
  explicit LenWatch(const std::string &fname) : fname(fname), sz(0) {}
  size_t size() const { return this->sz; }
#if LLVM_VERSION_MAJOR >= 8
  void notifyObjectLoaded(ObjectKey, const llvm::object::ObjectFile &o, const llvm::RuntimeDyld::LoadedObjectInfo &) override {
#else
  void NotifyObjectEmitted(const llvm::object::ObjectFile& o, const llvm::RuntimeDyld::LoadedObjectInfo&) override {
#endif
    for (auto s : o.symbols()) {
      const auto* esr = reinterpret_cast<const llvm::object::ELFSymbolRef*>(&s);

      if (esr != nullptr) {
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
  if (this->globals->contains(vn)) {
    return true;
  }
  if (this->constants->contains(vn)) {
    return true;
  }
  if (lookupOp(vn) != nullptr) {
    return true;
  }
  if (this->vtenv->contains(vn)) {
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
void jitcc::bindGlobal(const std::string &vn, const MonoTypePtr &ty, void *x) {
  void *value = x;
  if (is<Func>(ty) != nullptr) {
    withContext([&](auto &) {
      this->globals->add(
          vn, llvm::cast<llvm::FunctionType>(toLLVM(ty, /*asArg=*/false)));
    });
    llvm::cantFail(orcjit->addExternalCallableSymbol(vn, value));
  } else {
    if (hasPointerRep(ty) || isFileType(ty)) {
      void **p =
          reinterpret_cast<void **>(this->globalData.malloc(sizeof(void *)));
      *p = x;
      value = p;
    }

    withContext([&](auto &) {
      return this->globals->add(vn, toLLVM(ty, /*asArg=*/true));
    });
    llvm::cantFail(orcjit->addExternalNonCallableSymbol(vn, value));
  }
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
    } else if (llvm::GlobalVariable *rgv =
                   thisMod->getGlobalVariable(gv->getName())) {
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
llvm::GlobalVariable* jitcc::lookupGlobalVar(const std::string& vn) {
  return withContext([&](auto&) { return this->globals->getOrCreateVarDecl(vn, *this->module()); });
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
  } else if (llvm::GlobalVariable *rgv = mod->getGlobalVariable(vn)) {
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
  if (this->vtenv->contains(vn)) {
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
#if LLVM_VERSION_MAJOR >= 11
  return lookupGlobalVar(vn);
#else
  return maybeRefGlobal(vn);
#endif
}

#if LLVM_VERSION_MAJOR >= 11
llvm::Value* jitcc::loadConstant(const std::string& vn) {
  return withContext(
      [&](auto&) { return this->constants->loadConstant(vn, *this->module(), *builder()); });
}
#else
llvm::Value* jitcc::loadConstant(const std::string& vn) {
  auto cv = this->constants.find(vn);
  if (cv != this->constants.end()) {
    return withContext([&](auto&) -> llvm::Value* {
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
  using Thunk = void (*)();
  MonoTypePtr uety = requireMonotype(this->tenv, ue);

  if (isUnit(uety)) {
    // no storage necessary for units
    Thunk f = reinterpret_cast<Thunk>(reifyMachineCodeForFn(uety, list<std::string>(), list<MonoTypePtr>(), ue));
    f();
    releaseMachineCode(reinterpret_cast<void*>(f));
    resetMemoryPool();
  } else if (llvm::Constant *c = toLLVMConstant(this, vname, ue)) {
    // make a global constant ...
#if LLVM_VERSION_MAJOR >= 11
    if (is<Func>(uety) != nullptr) {
      withContext(
          [&](auto &) { return this->constants->storeFnAddr(vname, c); });
    } else {
      withContext([&](auto &) {
        return this->constants->createGVDef(vname, *module(), c, uety);
      });
    }
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
    auto f = reinterpret_cast<Thunk>(getMachineCode(initfn));

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
    if (auto* fn =
            withContext([&](auto&) { return this->vtenv->getOrCreateDecl(vn, *this->module()); })) {
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
#if LLVM_VERSION_MAJOR >= 11
  if (llvm::GlobalVariable* gv = lookupGlobalVar(vn)) {
#else
  if (llvm::GlobalVariable* gv = maybeRefGlobal(vn)) {
#endif
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
  if (lookupOp(vn) != nullptr) {
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
  return withContext(
      [&](auto&) { return this->globals->getOrCreateFuncDecl(fn, *this->module()); });
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

  if (result != nullptr) {
    for (const auto& f : fs) {
      result->push_back(f.result);
    }
  }
}

void jitcc::compileFunctions(const LetRec::Bindings& bs) {
  compileFunctions(bs, nullptr);
}

void jitcc::unsafeCompileFunctions(UCFS* ufs) {
  UCFS& fs = *ufs;

  // save our current write context to restore later
  llvm::BasicBlock* ibb = withContext([this](auto&) { return this->builder()->GetInsertBlock(); });

  // prepare the environment for these mutually-recursive definitions
  for (auto &f : fs) {
    llvm::Function* fval = allocFunction(f.name.empty() ? ("/" + freshName()) : f.name, f.argtys, requireMonotype(this->tenv, f.exp));
    if (fval == nullptr) {
      throw std::runtime_error("Failed to allocate function");
    }

#if LLVM_VERSION_MAJOR >= 11
    this->bindScope(f.name, fval);
#else
    this->vtenv.back()[f.name] = fval;
#endif
    f.result = fval;
  }

  // now compile each function
  for (const auto &f : fs) {
    llvm::Function*   fval = f.result;
    const UCF&        ucf  = f;
    MonoTypePtr       rty  = requireMonotype(this->tenv, ucf.exp);
    llvm::BasicBlock* bb = withContext(
        [fval](llvm::LLVMContext& c) { return llvm::BasicBlock::Create(c, "entry", fval); });

    withContext([this, bb](auto&) { return this->builder()->SetInsertPoint(bb); });

    // set argument names for safe referencing here
#if LLVM_VERSION_MAJOR >= 11
    this->pushScope();
#else
    this->vtenv.push_back(VarBindings());
#endif

    llvm::Function::arg_iterator a = fval->arg_begin();
    for (unsigned int i = 0; i < ucf.argns.size(); ++i) {
      if (isUnit(ucf.argtys[i])) {
#if LLVM_VERSION_MAJOR >= 11
        this->bindScope(ucf.argns[i], cvalue(true)); // this should never even be seen
#else
        this->vtenv.back()[ucf.argns[i]] = cvalue(true); // this should never even be seen
#endif
      } else {
        a->setName(ucf.argns[i]);

        llvm::Value* argv = &*a;
        if (is<FixedArray>(ucf.argtys[i]) != nullptr) {
          argv = withContext([&](auto&) {
            return cast(this->builder(), ptrType(toLLVM(ucf.argtys[i], false)), argv);
          });
        }
#if LLVM_VERSION_MAJOR >= 11
        this->bindScope(ucf.argns[i], argv);
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
        this->popScope();
        if (ibb != nullptr) { this->builder()->SetInsertPoint(ibb); }
      } catch (...) {
        if (ibb != nullptr) { this->builder()->SetInsertPoint(ibb); }
        this->popScope();
        throw;
      }
    });
  }
}

llvm::Value* jitcc::compileAllocStmt(llvm::Value* sz, llvm::Value* asz, llvm::Type* mty, bool zeroMem) {
  llvm::Function* f = lookupFunction(zeroMem ? "mallocz" : "malloc");
  if (f == nullptr) throw std::runtime_error("Expected heap allocation function as call.");
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
    llvm::Type* retType = toLLVM(rty, true);
    auto* f = llvm::Function::Create(
        llvm::FunctionType::get(retType, toLLVM(argl, true), false),
        fname.find(".patfs.") == 0 ? llvm::Function::InternalLinkage
                                   : llvm::Function::ExternalLinkage,
        fname, m);
    // https://bugs.llvm.org/show_bug.cgi?id=51163
    // for a llvm version >=9, zeroext has to be added to functions return boolean
    // otherwise, 255 will be returned as true
    if (retType == boolType()) {
      f->addAttribute(llvm::AttributeList::ReturnIndex, llvm::Attribute::ZExt);
    }
    return f;
  };
  llvm::Function* ret = withContext([&](auto&) { return f(*this->module()); });

  if (fname.find(".rfn.t") != std::string::npos) {
    this->vtenv->add(fname, f);
  } else if (!this->globals->contains(fname)) {
    withContext([&](auto &) {
      return this->globals->add(
          fname, llvm::FunctionType::get(toLLVM(rty, true), toLLVM(argl, true),
                                         false));
    });
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
  for (const auto &e : es) {
    r.push_back(c->compile(e));
  }
  return r;
}

Values compileArgs(jitcc* c, const Exprs& es) {
  Values r;
  for (const auto& e : es) {
    MonoTypePtr  et = requireMonotype(c->typeEnv(), e);
    llvm::Value* ev = c->compile(e);

    if (const Prim* pt = is<Prim>(et)) {
      if (pt->representation()) {
        et = pt->representation();
      }
    }

    if (is<Array>(et) != nullptr) {
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

op::~op() = default;

} // namespace hobbes
