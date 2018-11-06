
#ifndef HOBBES_EVAL_JITCC_HPP_INCLUDED
#define HOBBES_EVAL_JITCC_HPP_INCLUDED

#include <hobbes/lang/expr.H>
#include <hobbes/lang/type.H>
#include <hobbes/util/region.H>

#include <hobbes/util/llvm.H>
#include <hobbes/eval/func.H>
#include <hobbes/eval/ctype.H>

#include <map>
#include <string>
#include <vector>

namespace hobbes {

// an operation, which can emit some specialized assembly code
class jitcc;
struct op {
  virtual ~op();

  // type : reports the functional type of this operator (may be polymorphic)
  virtual PolyTypePtr type(typedb&) const = 0;

  // apply : produces some assembly code out of a JIT compiler,
  //         assuming the given input/output types and with expressions provided for arguments
  virtual llvm::Value* apply(jitcc* ev, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) = 0;
};

// a JIT compiler for monotyped expressions
class jitcc {
public:
  jitcc(const TEnvPtr&);
  ~jitcc();

  const TEnvPtr&     typeEnv() const;
  llvm::IRBuilder<>* builder() const;
  llvm::Module*      module();

  // get the address of a bound symbol
  void* getSymbolAddress(const std::string&);

  // print all module contents
  void dump() const;

  // define a global from a primitive expression
  void defineGlobal(const std::string& vname, const ExprPtr& unsweetExp);

  // define a global on some existing memory
  void bindGlobal(const std::string& vn, const MonoTypePtr& ty, void* v);

  // is there a definition of the named symbol?
  bool isDefined(const std::string&) const;

  // compile a named or anonymous expression (into the current instruction stream)
  // these assume that expressions have explicit (mono-)type annotations
  llvm::Value* compile(const ExprPtr& exp);
  llvm::Value* compile(const std::string& vname, const ExprPtr& exp);

  // backtrack on local-scope to compile an expression that uses only global data
  llvm::Value* compileAtGlobalScope(const ExprPtr& exp);

  // compile a function or a set of mutually-recursive functions
  llvm::Function* compileFunction(const std::string& name, const str::seq& argns, const MonoTypes& argtys, const ExprPtr& exp);
  void            compileFunctions(const LetRec::Bindings&, std::vector<llvm::Function*>* result);
  void            compileFunctions(const LetRec::Bindings&);

  // compile an allocation statement (to dynamically allocate some data)
  llvm::Value* compileAllocStmt(size_t sz, size_t asz, llvm::Type* mty, bool zeroMem = false);
  llvm::Value* compileAllocStmt(llvm::Value* sz, llvm::Value* asz, llvm::Type* mty, bool zeroMem = false);

  // begin a function with the given name, argument type list, return type
  llvm::Function* allocFunction(const std::string& fname, const MonoTypes& argl, const MonoTypePtr& rty);

  // bind within local scopes, and begin/end new nested local scopes
  void pushScope();
  void bindScope(const std::string& vn, llvm::Value* v);
  void popScope();

  // produce some machine code from a function specification (input names, input types, expression body)
  void* reifyMachineCodeForFn(const MonoTypePtr& reqTy, const str::seq& names, const MonoTypes& tys, const ExprPtr& exp);
  void releaseMachineCode(void*);

  // bind a low-level function definition
  void bindInstruction(const std::string&, op*);

  // find a low-level function definition by name
  op* lookupOp(const std::string&) const;

  // lookup a variable, either in local scopes, globals, or constants
  llvm::Value* lookupVar(const std::string&, const MonoTypePtr&);

  // find a function by name (returns nullptr if not found)
  llvm::Function* lookupFunction(const std::string&);

  // maybe get a pointer to global data
  //   this will give a nullptr either if the variable is in local scope, or if there is no global variable with that name
  llvm::GlobalVariable* lookupVarRef(const std::string&);

  // produce a constant reference to an interned string
  llvm::Value* internConstString(const std::string&);

  // get the machine code produced for a given expression
  typedef std::vector<uint8_t> bytes;
  bytes machineCodeForExpr(const ExprPtr&);

  // inline all global definitions within an expression
  ExprPtr inlineGlobals(const ExprPtr&);

  // allocate some global data attached to this JIT
  void* memalloc(size_t, size_t);
private:
  TEnvPtr tenv;

  // produce some machine code for a compiled function
  void* getMachineCode(llvm::Function*, llvm::JITEventListener* listener = 0);

  // the current non-finalized module
  // (new definitions will be accumulated here)
  // (may be null, to lazily allocate modules)
  llvm::Module* currentModule;

  // the set of allocated modules
  typedef std::vector<llvm::Module*> Modules;
  Modules modules;

#if LLVM_VERSION_MINOR == 6 || LLVM_VERSION_MINOR == 7 || LLVM_VERSION_MINOR == 8 || LLVM_VERSION_MAJOR == 4
  llvm::legacy::PassManager* mpm;

  // the set of allocated execution engines (each will own a finalized module from the set of modules)
  typedef std::vector<llvm::ExecutionEngine*> ExecutionEngines;
  ExecutionEngines eengines;
#elif LLVM_VERSION_MINOR == 3
  llvm::PassManager*         mpm;
  llvm::ExecutionEngine*     eengine;
  llvm::FunctionPassManager* fpm;
#elif LLVM_VERSION_MINOR == 5
  llvm::legacy::PassManager*         mpm;
  llvm::ExecutionEngine*             eengine;
  llvm::legacy::FunctionPassManager* fpm;
#else
#error "This version of LLVM is not supported"
#endif

  // support incremental construction of LLVM assembly sequences
  llvm::IRBuilder<>* irbuilder;

  // the bound root function environment
  typedef std::map<std::string, op*> FuncEnv;
  FuncEnv fenv;

  // keep track of variables and local scopes during compilation
  typedef std::map<std::string, llvm::Value*> VarBindings;
  typedef std::vector<VarBindings>            VarBindingStack;
  VarBindingStack vtenv;
  bool ignoreLocalScope;

  // compile sets of mutually-recursive functions (as a special case, single-function compilation)
  struct UCF {
    const std::string& name;
    const str::seq&    argns;
    MonoTypes          argtys;
    const ExprPtr&     exp;

    llvm::Function*    result;

    inline UCF(const std::string& name, const str::seq& argns, const MonoTypes& argtys, const ExprPtr& exp)
      : name(name), argns(argns), argtys(argtys), exp(exp) { }
  };
  typedef std::vector<UCF> UCFS;
  void unsafeCompileFunctions(UCFS*);

  // keep track of global variables
  struct Global {
    MonoTypePtr           type;
    void*                 value;
    union {
      llvm::GlobalVariable* var;
      llvm::Function*       fn;
    } ref;
  };
  typedef std::map<std::string, Global> Globals;
  Globals globals;

  // keep track of global data (in case we need to dynamically allocate global variables of any type)
  region globalData;
  size_t pushGlobalRegion();
  void popGlobalRegion(size_t x);

  // keep track of global constants
  struct Constant {
    llvm::Constant*       value;
    llvm::Type*           type;
    MonoTypePtr           mtype;
    llvm::GlobalVariable* ref;
  };
  typedef std::map<std::string, Constant> Constants;
  Constants constants;
  llvm::Value* loadConstant(const std::string&);

  // keep some interned strings, helpful for global constants and debug info
  typedef std::unordered_map<std::string, std::string> InternConstVars;
  InternConstVars internConstVars;

  // try to load a symbol as a global (may return nullptr if this can't be done)
  llvm::GlobalVariable* maybeRefGlobal(const std::string&);
  llvm::GlobalVariable* refGlobal(const std::string&, llvm::GlobalVariable*);

  // pass through a value if it's not a global or if it's a global in the current module
  // else wrap it in an extern decl
  llvm::Value* maybeRefGlobalV(llvm::Value*);

  // keep track of monotyped definitions as expressions
  // (in case we want to inline them later)
  typedef std::map<std::string, ExprPtr> GlobalExprs;
  GlobalExprs globalExprs;
};

// shorthand for compilation over a sequence of expressions
typedef std::vector<llvm::Value*> Values;

Values compile(jitcc*, const Exprs&);
Values compileArgs(jitcc*, const Exprs&);

}

#endif

