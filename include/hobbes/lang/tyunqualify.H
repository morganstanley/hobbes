
#ifndef HOBBES_LANG_TYUNQUALIFY_HPP_INCLUDED
#define HOBBES_LANG_TYUNQUALIFY_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/lang/expr.H>
#include <hobbes/util/lannotation.H>
#include <memory>

namespace hobbes {

using SymSet = std::set<std::string>;

// how do some parameters of a constraint determine others?
using VarIDs = std::vector<int>;
using FunDep = std::pair<VarIDs, int>;
using FunDeps = std::vector<FunDep>;

// if E :: T and R.satisfied(T), then R.unqualify(E)
class Unqualifier {
  public:
    virtual ~Unqualifier() = default;;
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
using UnqualifierPtr = std::shared_ptr<Unqualifier>;

// type predicates are resolved by an assumed disjoint set of unqualifiers
class UnqualifierSet : public Unqualifier {
  public:
    using Unqualifiers = std::map<std::string, UnqualifierPtr>;

    void                add(const std::string& name, const UnqualifierPtr& uq);
    UnqualifierPtr      findUnqualifier(const std::string& name);
    const Unqualifiers& unqualifiers() const;

    bool        refine(const TEnvPtr&,const ConstraintPtr&,MonoTypeUnifier*,Definitions*) override;
    bool        satisfied(const TEnvPtr&,const ConstraintPtr&,Definitions*)                  const override;
    bool        satisfiable(const TEnvPtr&,const ConstraintPtr&,Definitions*)                const override;
    void        explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) override;
    ExprPtr     unqualify(const TEnvPtr&,const ConstraintPtr&, const ExprPtr&, Definitions*) const override;
    PolyTypePtr lookup(const std::string& vn)                                                const override;
    SymSet      bindings()                                                                   const override;
    FunDeps     dependencies(const ConstraintPtr&)                                           const override;
  private:
    Unqualifiers uqs;
};
using UnqualifierSetPtr = std::shared_ptr<UnqualifierSet>;

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

