
#ifndef HOBBES_LANG_TYPEPREDS_CLASS_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_CLASS_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/typemap.H>
#include <memory>

namespace hobbes {

// given a set of constraints in TGen form, we can find the implied set of fundeps among them
// (this may be more specific than the union of fundeps between constraint types since the
// type forms in constraints may add additional structure)
FunDeps inferFundeps(const TEnvPtr& tenv, const Constraints& cs);

// fundeps union
FunDeps mergeFundeps(const FunDeps& lhs, const FunDeps& rhs);

// a 'member mapping' is a set of named overloaded definitions
typedef std::map<std::string, ExprPtr> MemberMapping;

// a 'type class' is a scheme for overloading terms
class TCInstance;
typedef std::shared_ptr<TCInstance> TCInstancePtr;
typedef std::vector<TCInstancePtr>  TCInstances;

class TCInstanceFn;
typedef std::shared_ptr<TCInstanceFn> TCInstanceFnPtr;
typedef std::vector<TCInstanceFnPtr>  TCInstanceFns;

class TClass : public Unqualifier, public LexicallyAnnotated {
public:
  typedef std::map<std::string, MonoTypePtr> Members;

  TClass(const Constraints& reqs, const std::string& tcname, size_t tvs, const Members& tcmembers, const LexicalAnnotation&);
  TClass(const Constraints& reqs, const std::string& tcname, size_t tvs, const Members& tcmembers, const FunDeps& fundeps, const LexicalAnnotation&);
  TClass(const std::string& tcname, size_t tvs, const Members& tcmembers, const LexicalAnnotation&);
  TClass(const std::string& tcname, size_t tvs, const Members& tcmembers, const FunDeps& fundeps, const LexicalAnnotation&);

  const std::string& name() const;
  const Constraints& constraints() const;
  size_t typeVars() const;
  const Members& members() const;
  const FunDeps& deps() const;

  // allow any number of instances and instance functions (they'll be checked in insertion order)
  void insert(const TEnvPtr& tenv, const TCInstancePtr& ip, Definitions* ds);
  void insert(const TCInstanceFnPtr& ifp);

  // find the set of instances that match a given constraint
  TCInstances matches(const TEnvPtr& tenv, const ConstraintPtr& c, MonoTypeUnifier*, Definitions* ds) const;
  TCInstances matches(const TEnvPtr& tenv, const MonoTypes& mts, MonoTypeUnifier*, Definitions* ds) const;

  // unqualifier interface
  bool        refine     (const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* s, Definitions* ds);
  bool        satisfied  (const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds)                   const;
  bool        satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds)                   const;
  void        explain    (const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs);
  ExprPtr     unqualify  (const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const;
  PolyTypePtr lookup     (const std::string& vn)                                                            const;
  SymSet      bindings   ()                                                                                 const;
  FunDeps     dependencies(const ConstraintPtr&)                                                            const;

  // the member type in the implied context of this class's qua[nt|l]ification
  //   (normally you should use 'lookup' to get fully qualified types)
  MonoTypePtr memberType(const std::string& vn) const;

  // show this class definition
  void show(std::ostream&) const;
public:
  const TCInstances& instances() const;
  bool hasGroundInstanceAt(const MonoTypes&) const;
  const TCInstanceFns& instanceFns() const;
private:
  friend class TCInstanceFn;

  typedef type_map<TCInstancePtr> TCMonoInstDB;
  typedef type_map<TCInstanceFns> TCInstFnDB;

  std::string   tcname;
  size_t        tvs;
  Constraints   reqs;
  Members       tcmembers;
  FunDeps       fundeps;
  TCInstances   tcinstances;
  TCMonoInstDB  tcinstdb;
  TCInstanceFns tcinstancefns;
  TCInstFnDB    tcinstfndb;

  void candidateTCInstFns(const TEnvPtr&, const MonoTypes&, TCInstanceFns*) const;

  bool refine(const TEnvPtr& tenv, const ConstraintPtr& c, const FunDep& fd, MonoTypeUnifier* s, Definitions* ds) const;

  // to support recursive types, track recursive invocations and assume they're satisfiable
  typedef type_map<bool> TestedInstances;
  mutable TestedInstances testedInstances;

  // cache satisfiability tests
  mutable TestedInstances satfInstances;
};
typedef std::shared_ptr<TClass>          TClassPtr;
typedef std::map<std::string, TClassPtr> TClassEnv;

class TCInstance : public LexicallyAnnotated {
public:
  typedef std::shared_ptr<Expr> ExprPtr;

  TCInstance(const std::string& tcname, const MonoTypes& itys, const MemberMapping& mmap, const LexicalAnnotation&);

  size_t arity() const;
  const MonoTypes& types() const;
  const MemberMapping& memberMapping() const;
  bool hasMapping(const std::string& oname) const;
  const ExprPtr& memberMapping(const std::string& oname) const;

  void bind(const TEnvPtr&, const TClass*, Definitions*);

  bool matches(const TEnvPtr&, const MonoTypes&) const;

  ExprPtr unqualify(Definitions* ds, const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e) const;

  // show this instance definition
  void show(std::ostream&) const;
private:
  std::string   tcname;
  MonoTypes     itys;
  MemberMapping mmap;
};

class TCInstanceFn : public LexicallyAnnotated {
public:
  TCInstanceFn(const std::string& tcname, const Constraints& reqs, const MonoTypes& itys, const MemberMapping& mmap, const LexicalAnnotation&);

  size_t arity() const;

  // can we immediately exclude this instance generator for this type list?
  bool satisfiable(const TEnvPtr&, const MonoTypes&, Definitions*);

  // assuming we're not satisfiable, what are the constraints that aren't satisfiable?
  void explainSatisfiability(const TEnvPtr&, const MonoTypes&, Definitions*, Constraints*, Constraints*);

  // (possibly) produce a type class instance from this generator
  bool apply(const TEnvPtr& tenv, const MonoTypes& tys, const TClass* c, MonoTypeUnifier*, Definitions* rdefs, TCInstancePtr* out) const;

  // assuming that the input conditions are satisfied, what are the output args and member mapping?
  MonoTypes instantiatedArgs(MonoTypeUnifier* s, const MonoTypes& tys) const;
  MemberMapping members(const MonoTypeSubst&) const;

  // constraints that this generator depends on
  const Constraints& constraints() const;

  // show this instance-generator definition
  void show(std::ostream&) const;

  // produce a sequence of type variables and constraints unique to this invocation
  typedef std::pair<Constraints, MonoTypes> IFnDef;
  IFnDef freshDef(MonoTypeSubst* s) const;
private:
  std::string   tcname;
  Constraints   reqs;
  MemberMapping mmap;
public:
  MonoTypes     itys;
  size_t order; // where does this instance function belong in its family?
};

// utilities for testing satisfiability of arbitrary class constraints
bool isClassSatisfied(const TEnvPtr&, const std::string&, const MonoTypes&, Definitions*);
bool isClassSatisfiable(const TEnvPtr&, const std::string&, const MonoTypes&, Definitions*);

ExprPtr unqualifyClass(const TEnvPtr&, const std::string&, const MonoTypes&, const ExprPtr&, Definitions*);

// utility for generating new classes from existing ones
void definePrivateClass(const TEnvPtr& tenv, const std::string& memberName, const ExprPtr& expr);
bool isClassMember(const TEnvPtr& tenv, const std::string& memberName);

// reverse private class constraints to find the leaf public constraints
Constraints expandHiddenTCs(const TEnvPtr&, const Constraints&);

// temporarily support serialization of ground type class instances to avoid recomputation at startup
void serializeGroundClasses(const TEnvPtr&, std::ostream&);
void deserializeGroundClasses(const TEnvPtr&, std::istream&, Definitions*);

// show class, instance, and instance-generator definitions
std::string show(const TClassPtr&);
std::string show(const TCInstancePtr&);
std::string show(const TCInstanceFnPtr&);

}

#endif

