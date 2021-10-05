
#ifndef HOBBES_LANG_TYPEPREDS_CONSVARIANT_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_CONSVARIANT_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>

namespace hobbes {

// a 'consvariant' constraint asserts that a type is a variant with a head type and a tail type
//   e.g.:
//    yes: ConsVariant |x:int,y:bool| int  |y:bool|
//    no:  ConsVariant |x:int,y:bool| bool |x:int|    (doesn't match structure, x should come first)
//
// this constraint can be inferred "forward" (when deconstructing a large variant into smaller parts),
// or it can be inferred "backward" (when constructing a large variant from smaller parts)
// a 'variant deconstructor' is a scheme for consuming variant type structures at compile-time
class VariantDeconstructor : public Unqualifier {
public:
  static std::string constraintName();

  // unqualifier interface
  bool        refine(const TEnvPtr&,const ConstraintPtr&,MonoTypeUnifier*,Definitions*) override;
  bool        satisfied(const TEnvPtr&,const ConstraintPtr&,Definitions*)                  const override;
  bool        satisfiable(const TEnvPtr&,const ConstraintPtr&,Definitions*)                const override;
  void        explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs) override;
  ExprPtr     unqualify(const TEnvPtr&,const ConstraintPtr&, const ExprPtr&, Definitions*) const override;
  PolyTypePtr lookup   (const std::string& vn)                                             const override;
  SymSet      bindings ()                                                                  const override;
  FunDeps     dependencies(const ConstraintPtr&)                                           const override;
};

}

#endif

