
#ifndef HOBBES_LANG_TYPEPREDS_SUBTYPE_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_SUBTYPE_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>

namespace hobbes {

// a 'subtype' constraint ensures that a lower type can be translated to a greater type
struct Subtype {
  MonoTypePtr lower;
  MonoTypePtr greater;
};
bool dec(const ConstraintPtr&, Subtype*);

// a "subtype eliminator" knows how to resolve a "Subtype" constraint at a particular (category of) type
struct SubtypeEliminator {
  virtual ~SubtypeEliminator() = default;
  // is this instance eliminable?
  virtual bool satisfied(const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs) const = 0;

  // is it possible for this instance to eventually be eliminated?
  virtual bool satisfiable(const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs) const = 0;

  // refine the substitution set associated with this constraint
  virtual bool refine(const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs, MonoTypeUnifier* s) = 0;

  // unqualify a constraint (satisfied() must have returned true)
  virtual ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*) const = 0;

  // pass on requests for overloaded identifiers
  virtual PolyTypePtr lookup(const std::string& vn) const = 0;
  virtual SymSet bindings() const = 0;
};

// a 'subtype unqualifier' resolves subtype constraints
class SubtypeUnqualifier : public Unqualifier {
public:
  SubtypeUnqualifier();
  static std::string constraintName();

  // extend the set of 'subtype' eliminators dynamically (dangerous?)
  void addEliminator(const std::shared_ptr<SubtypeEliminator>&);

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
  using SubtypeEliminators = std::vector<std::shared_ptr<SubtypeEliminator>>;
  SubtypeEliminators eliminators;

  SubtypeEliminator* findEliminator(const TEnvPtr&, const Subtype&) const;
};

}

#endif
