
#ifndef HOBBES_LANG_TYPEPREDS_APPENDSTO_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_APPENDSTO_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>

namespace hobbes {

// an 'appendsto' constraint ensures that two types can be 'appended' to a third type
//  A++B=C | A B -> C, B C -> A, A C -> B
//   e.g.:
//     {x:int}++{y:int}=C        [C = {x:int,y:int}]
//     A++{y:int}={x:int,y:int}  [A = {x:int}]
//     {x:int}++B={x:int,y:int}  [B = {y:int}]
struct AppendsTo {
  MonoTypePtr leftType;
  MonoTypePtr rightType;
  MonoTypePtr resultType;
};
bool dec(const ConstraintPtr&, AppendsTo*);

// an "appendsto eliminator" knows how to resolve an "AppendsTo" constraint at a particular (category of) type
struct ATEliminator {
  virtual ~ATEliminator() = default;

  // is this AT instance eliminable?
  virtual bool satisfied(const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs, const MonoTypePtr& result) const = 0;

  // is it possible for this AT instance to eventually be eliminated?
  virtual bool satisfiable(const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs, const MonoTypePtr& result) const = 0;

  // refine the substitution set associated with this constraint
  virtual bool refine(const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs, const MonoTypePtr& result, MonoTypeUnifier* s) = 0;

  // unqualify a constraint (satisfied() must have returned true)
  virtual ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*) const = 0;

  // allow overloaded functions to be defined at each eliminator type
  virtual PolyTypePtr lookup(const std::string& vn) const = 0;
  virtual SymSet bindings() const = 0;
};

// a 'field verifier' is a scheme for validating types as containing a named field somehow
class AppendsToUnqualifier : public Unqualifier {
public:
  AppendsToUnqualifier();
  static std::string constraintName();

  // extend the set of 'appendsto' eliminators dynamically (dangerous?)
  void addEliminator(const std::shared_ptr<ATEliminator>&);

  // unqualifier interface
  bool        refine(const TEnvPtr&,const ConstraintPtr&,MonoTypeUnifier*,Definitions*) override;
  bool        satisfied(const TEnvPtr&,const ConstraintPtr&,Definitions*)                  const override;
  bool        satisfiable(const TEnvPtr&,const ConstraintPtr&,Definitions*)                const override;
  void        explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs) override;
  ExprPtr     unqualify(const TEnvPtr&,const ConstraintPtr&, const ExprPtr&, Definitions*) const override;
  PolyTypePtr lookup   (const std::string& vn)                                             const override;
  SymSet      bindings ()                                                                  const override;
  FunDeps     dependencies(const ConstraintPtr&)                                           const override;
private:
  using ATEliminators = std::vector<std::shared_ptr<ATEliminator>>;
  ATEliminators eliminators;

  ATEliminator* findEliminator(const TEnvPtr&, const AppendsTo*) const;
};

}

#endif

