
#ifndef HOBBES_EVAL_CC_HPP_INCLUDED
#define HOBBES_EVAL_CC_HPP_INCLUDED

#include <hobbes/lang/expr.H>
#include <hobbes/lang/type.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/lang/tyunqualify.H>
#include <hobbes/lang/preds/subtype/obj.H>
#include <hobbes/lang/typepreds.H>
#include <hobbes/read/parser.H>
#include <hobbes/eval/jitcc.H>
#include <hobbes/eval/search.H>

#include <hobbes/util/func.H>
#include <hobbes/util/llvm.H>
#include <hobbes/util/str.H>

#include <string>
#include <map>
#include <vector>
#include <stdexcept>
#include <iostream>
#include <unordered_map>

namespace hobbes {

// protect access to hobbes/llvm resources between threads
class hlock {
public:
  hlock(); ~hlock();
};
class phlock {
public:
  phlock();
  void release();
  ~phlock();
private:
  bool f;
};

// parameter parsing for the 'compileFn' family of functions
typedef std::function<ExprPtr(const std::string&)> ExprParser;

template <typename ... NamesAndExpr>
  struct PArgl {
  };
template <>
  struct PArgl<ExprPtr> {
    static str::seq names(const ExprPtr&) { return str::seq(); }
    static ExprPtr expr(const ExprParser&, const ExprPtr& e) { return e; }
  };
template <>
  struct PArgl<std::string> {
    static str::seq names(const std::string&) { return str::seq(); }
    static ExprPtr expr(const ExprParser& p, const std::string& e) { return p(e); }
  };
template <>
  struct PArgl<char*> {
    static str::seq names(char*) { return str::seq(); }
    static ExprPtr expr(const ExprParser& p, char* e) { return p(std::string(e)); }
  };
template <>
  struct PArgl<const char*> {
    static str::seq names(const char*) { return str::seq(); }
    static ExprPtr expr(const ExprParser& p, const char* e) { return p(std::string(e)); }
  };
template <typename ... NamesAndExpr>
  struct PArgl<const char*, NamesAndExpr...> {
    static str::seq names(const char* x, const NamesAndExpr& ... args) { str::seq r = PArgl<NamesAndExpr...>::names(args...); r.insert(r.begin(), std::string(x)); return r; }
    static ExprPtr expr(const ExprParser& p, const char*, const NamesAndExpr& ... args) { return PArgl<NamesAndExpr...>::expr(p, args...); }
  };
template <typename ... NamesAndExpr>
  struct PArgl<std::string, NamesAndExpr...> {
    static str::seq names(const std::string& x, const NamesAndExpr& ... args) { str::seq r = PArgl<NamesAndExpr...>::names(args...); r.insert(r.begin(), x); return r; }
    static ExprPtr expr(const ExprParser& p, const std::string&, const NamesAndExpr& ... args) { return PArgl<NamesAndExpr...>::expr(p, args...); }
  };

// the main compiler
class cc : public typedb {
public:
  cc();
  virtual ~cc();

  // parse expressions
  typedef ModulePtr (*readModuleFileFn)(cc*, const std::string&);
  ModulePtr readModuleFile(const std::string&);
  void setReadModuleFileFn(readModuleFileFn);

  typedef ModulePtr (*readModuleFn)(cc*, const std::string&);
  ModulePtr readModule(const std::string&);
  void setReadModuleFn(readModuleFn);

  typedef std::pair<std::string, ExprPtr> (*readExprDefnFn)(cc*, const std::string&);
  std::pair<std::string, ExprPtr> readExprDefn(const std::string&);
  void setReadExprDefnFn(readExprDefnFn);

  typedef ExprPtr (*readExprFn)(cc*, const std::string&);
  ExprPtr readExpr(const std::string&);
  MonoTypePtr readMonoType(const std::string&);
  void setReadExprFn(readExprFn);

  // search for paths from one type to another
  // (currently just one-step paths, may be useful to consider multi-step paths)
  SearchEntries search(const MonoTypePtr&, const MonoTypePtr&);
  SearchEntries search(const ExprPtr&, const MonoTypePtr&);
  SearchEntries search(const std::string&, const MonoTypePtr&);
  SearchEntries search(const std::string&, const std::string&);
private:
  // parser functions
  readModuleFileFn readModuleFileF;
  readModuleFn     readModuleF;
  readExprDefnFn   readExprDefnF;
  readExprFn       readExprF;
public:
  // type-safe compilation to C++ function pointers
  template <typename RFn, typename ... NamesAndExpr>
    typename func<RFn>::type compileFn(NamesAndExpr ... args) {
      static_assert(func<RFn>::arity == sizeof...(NamesAndExpr)-1, "Formal parameter list and expected function type arity mismatch");
      return rcast<typename func<RFn>::type>(
        unsafeCompileFn(
          lift<RFn>::type(*this),
          PArgl<NamesAndExpr...>::names(args...),
          PArgl<NamesAndExpr...>::expr([&](const std::string& x){return this->readExpr(x);}, args...)
        )
      );
    }

  // perform type-checking, explicit type annotation, and type class resolution
  ExprPtr unsweetenExpression(const TEnvPtr& te, const ExprPtr& e);
  ExprPtr unsweetenExpression(const TEnvPtr& te, const std::string& vname, const ExprPtr& e);
  ExprPtr unsweetenExpression(const ExprPtr& e);
  ExprPtr unsweetenExpression(const std::string& vname, const ExprPtr& e);
  ExprPtr normalize(const ExprPtr& e); // unalias + unsweeten

  // access the LLVM resources
  llvm::IRBuilder<>*     builder() const;
  llvm::Module*          module()  const;

  // dump the contents of the active type environment (useful for debugging)
  void dumpTypeEnv() const;
  void dumpTypeEnv(str::seq* syms, str::seq* types) const;
  std::string showTypeEnv() const;

  const TEnvPtr& typeEnv() const;

  // forward-declare a variable binding
  void forwardDeclare(const std::string& vname, const QualTypePtr& qt);

  // is a variable merely forward-declared, or does it have a definition?
  bool hasValueBinding(const std::string& vname);
  
  // process and define a set of residual definitions produced by type unqualification
  bool drainingDefs;
  LetRec::Bindings drainDefs;
  void drainUnqualifyDefs(const Definitions& ds);

  // compile an expression and associate it with a name
  void define(const std::string& vname, const ExprPtr& e);
  void define(const std::string& vname, const std::string& expr);

  // shorthand for class instance definitions for classes with 0 or 1 members
  void overload(const std::string&, const MonoTypes&);
  void overload(const std::string&, const MonoTypes&, const ExprPtr&);
  void overload(const std::string&, const MonoTypes&, const std::string&);

  // add a type class instance to a known class (wrap up recursive unsweetening/def-draining)
  void addInstance(const TClassPtr&, const TCInstancePtr&);

  // dump the contents of the generated module (useful for debugging)
  void dumpModule();

  // get the x86 machine code for an expression (useful for debugging)
  typedef std::vector<unsigned char> bytes;
  bytes machineCodeForExpr(const std::string& expr);

  // keep track of C++ classes so that we can perform upcasts where necessary
  template <typename T>
    void addObj() {
      hlock _;
      this->objs->add<T>();
    }

  // convenience method for lifting C++ types
  template <typename T>
    PolyTypePtr liftType() {
      hlock _;
      return generalize(lift<T>::type(*this));
    }

  template <typename T>
    MonoTypePtr liftMonoType() {
      return lift<T>::type(*this);
    }

  // bind a C++ value (be sure it stays in scope!)
  void bind(const PolyTypePtr& tn, const std::string& vn, void* x);

  template <typename T>
    void bind(const std::string& vn, T* x) {
      hlock _;
      bind(generalize(liftValue<T*>::type(*this, x)), vn, rcast<void*>(x));
    }

  template <typename T, int N>
    void bindArr(const std::string& vn, T x[N]) {
      hlock _;
      bind(polytype(qualtype(arrayty(lift<T>::type(*this), N))), vn, rcast<void*>(x));
    }

  // simplify binding user functions
  template <typename R, typename ... Args>
    void bind(const std::string& fn, R (*pfn)(Args...)) {
      hlock _;
      bindExternFunction(fn, lift<R(Args...)>::type(*this), rcast<void*>(pfn));
    }
private:
  typedef std::pair<str::seq, MonoTypePtr>        TTyDef;
  typedef std::unordered_map<std::string, TTyDef> TTyDefs;
  TTyDefs ttyDefs;
public:
  // allow the definition of transparent type aliases
  // ("transparent" in the sense that all other aspects of compilation see the fully expanded type)
  void defineTypeAlias(const std::string& name, const str::seq& argNames, const MonoTypePtr& ty);
  bool isTypeAliasName(const std::string& name) const;
  MonoTypePtr replaceTypeAliases(const MonoTypePtr& ty) const;

  // typedb interface
  PolyTypePtr opaquePtrPolyType(const std::type_info& ti, unsigned int sz, bool inStruct);
  MonoTypePtr opaquePtrMonoType(const std::type_info& ti, unsigned int sz, bool inStruct);

  PolyTypePtr generalize(const MonoTypePtr& mt) const;

  MonoTypePtr defineNamedType(const std::string& name, const str::seq& argNames, const MonoTypePtr& ty);
  bool isTypeName(const std::string&) const;
  MonoTypePtr namedTypeRepresentation(const std::string&) const;

  // an 'unsafe' compilation method
  //   (the compiled function will conform to the input type description, but this type structure will not be available to C++)
  void* unsafeCompileFn(const MonoTypePtr& retTy, const str::seq& names, const MonoTypes& argTys, const ExprPtr& exp);
  void* unsafeCompileFn(const MonoTypePtr& fnTy, const str::seq& names, const ExprPtr& exp);
  void* unsafeCompileFn(const MonoTypePtr& fnTy, const str::seq& names, const std::string& exp);
  void releaseMachineCode(void*);

  // compile/optimization options
  void enableModuleInlining(bool f);
  bool enableModuleInlining() const;
  void buildInterpretedMatches(bool f);
  bool buildInterpretedMatches() const;
  void requireMatchReachability(bool f);
  bool requireMatchReachability() const;
  void alwaysLowerPrimMatchTables(bool);
  bool alwaysLowerPrimMatchTables() const;
  void buildColumnwiseMatches(bool f);
  bool buildColumnwiseMatches() const;
  void throwOnHugeRegexDFA(bool f);
  bool throwOnHugeRegexDFA() const;
  void regexDFAOverNFAMaxRatio(int f);
  int  regexDFAOverNFAMaxRatio() const;

  // allow caller to gather a vector of unreachable rows arising from match compilation
  UnreachableMatchRowsPtr unreachableMatchRowsPtr;

  // allow low-level functions to be added
  void bindLLFunc(const std::string&, op*);

  // bind external functions
  void bindExternFunction(const std::string& fname, const MonoTypePtr& fty, void* fn);

  // allocate global data
  void* memalloc(size_t, size_t);

  template <typename T>
    array<T>* makeArray(size_t n) {
      auto* r = reinterpret_cast<array<T>*>(this->memalloc(sizeof(long) + (sizeof(T) * n), std::max<size_t>(sizeof(long), alignof(T))));
      r->size = n;
      if (r->size > 0) {
        new (r->data) T[r->size];
      }
      return r;
    }
private:
  // cache for expression search results
  SearchCache searchCache;

  // optimization options
  bool runModInlinePass;
  bool genInterpretedMatch;
  bool checkMatchReachability;
  bool lowerPrimMatchTables;
  bool columnwiseMatches;

  // abort compilation of regexes which translate into huge dfa transition states
  bool shouldThrowOnHugeRegexDFA = false;
  int  dfaOverNfaMaxRatio        = 4;

  // the bound root type-def environment
  typedef std::map<std::string, PolyTypePtr> TypeAliasMap;

  TEnvPtr      tenv;
  TypeAliasMap typeAliases;

  PolyTypePtr lookupVarType(const std::string& vname) const;

  // global variables
  void definePolyValue(const std::string& vname, const ExprPtr& unsweetExp);

  // track C++ object relationships
  ObjsPtr objs;

  // the JIT engine that compiles our monotyped expressions
  jitcc* jit;
public:
  // compiler-local type structure caches for internal use
  std::unordered_map<MonoType*, MonoTypePtr> unappTyDefns;
private:
  // disable copying
  cc(const cc&);
  void operator=(const cc&);
};

#define LIFTCTY(cc, e) (cc).liftMonoType<decltype(e)>()

template <typename T>
  struct rccF {
    static T compile(cc* c, const str::seq& vns, const std::string& expr) {
      throw std::runtime_error("Internal error, unsupported compilation target type");
    }
  };

template <typename R, typename ... Args>
  struct rccF<R(*)(Args...)> {
    typedef R (*cbF)(Args...);
    static cbF compile(cc* c, const str::seq& vns, const std::string& expr) {
      return rcast<cbF>(c->unsafeCompileFn(lift<cbF>::type(*c), vns, expr));
    }
  };

template <typename T>
  T compileTo(cc* c, const str::seq& vns, const std::string& expr) {
    return rccF<T>::compile(c, vns, expr);
  }

}

// support binding to C++ class member functions
#define memberfn(e) &hobbes::mfnThunk< decltype(e), decltype(e), e >::fn

#endif
