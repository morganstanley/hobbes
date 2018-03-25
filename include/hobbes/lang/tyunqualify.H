
#ifndef HOBBES_LANG_TYUNQUALIFY_HPP_INCLUDED
#define HOBBES_LANG_TYUNQUALIFY_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/lang/expr.H>
#include <hobbes/util/lannotation.H>
#include <memory>

namespace hobbes {

typedef std::set<std::string> SymSet;

// how do some parameters of a constraint determine others?
typedef std::vector<int>       VarIDs;
typedef std::pair<VarIDs, int> FunDep;
typedef std::vector<FunDep>    FunDeps;

// if E :: T and R.satisfied(T), then R.unqualify(E)
class Unqualifier {
public:
  // bind any implied type variables in a constraint
  virtual bool refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions* ds) = 0;

  // determine whether or not this constraint is satisfied
  virtual bool satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const = 0;

  // determine whether or not it's possible to satisfy this constraint
  virtual bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const = 0;

  // why couldn't this constraint be eliminated?
  virtual void explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) = 0;

  // resolve a qualified expression
  virtual ExprPtr unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const = 0;

  // allow overloaded symbols (as in type classes)
  virtual PolyTypePtr lookup(const std::string& vn) const = 0;

  // list overloaded symbols (if any)
  virtual SymSet bindings() const = 0;

  // list functional dependencies between constraint parameters (if any)
  virtual FunDeps dependencies(const ConstraintPtr&) const = 0;
};
typedef std::shared_ptr<Unqualifier> UnqualifierPtr;

// type predicates are resolved by an assumed disjoint set of unqualifiers
class UnqualifierSet : public Unqualifier {
public:
  typedef std::map<std::string, UnqualifierPtr> Unqualifiers;

  void                add(const std::string& name, const UnqualifierPtr& uq);
  UnqualifierPtr      findUnqualifier(const std::string& name);
  const Unqualifiers& unqualifiers() const;

  bool        refine(const TEnvPtr&,const ConstraintPtr&,MonoTypeUnifier*,Definitions*);
  bool        satisfied(const TEnvPtr&,const ConstraintPtr&,Definitions*)                  const;
  bool        satisfiable(const TEnvPtr&,const ConstraintPtr&,Definitions*)                const;
  void        explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*);
  ExprPtr     unqualify(const TEnvPtr&,const ConstraintPtr&, const ExprPtr&, Definitions*) const;
  PolyTypePtr lookup(const std::string& vn)                                                const;
  SymSet      bindings()                                                                   const;
  FunDeps     dependencies(const ConstraintPtr&)                                           const;
private:
  Unqualifiers uqs;
};
typedef std::shared_ptr<UnqualifierSet> UnqualifierSetPtr;

// utilities to remove discharged type predicates
bool hasConstraint(const ConstraintPtr& c, const Constraints& cs);
bool hasConstraint(const ConstraintPtr& c, const QualTypePtr& qt);

Constraints removeConstraint(const ConstraintPtr& c, const Constraints& cs);
QualTypePtr removeConstraint(const ConstraintPtr& c, const QualTypePtr& qt);

// if possible, this procedure will eliminate predicates in qualified types and apply the necessary term transformations
// in the process, new definitions may need to be introduced (in which case they'll be inserted in the supplied array)
ExprPtr unqualifyTypes(const TEnvPtr& tenv, const ExprPtr& e, Definitions* ds);

}

#endif

