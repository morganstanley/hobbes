
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
  bool        refine(const TEnvPtr&,const ConstraintPtr&,MonoTypeUnifier*,Definitions*);
  bool        satisfied(const TEnvPtr&,const ConstraintPtr&,Definitions*)                  const;
  bool        satisfiable(const TEnvPtr&,const ConstraintPtr&,Definitions*)                const;
  void        explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs);
  ExprPtr     unqualify(const TEnvPtr&,const ConstraintPtr&, const ExprPtr&, Definitions*) const;
  PolyTypePtr lookup   (const std::string& vn)                                             const;
  SymSet      bindings ()                                                                  const;
  FunDeps     dependencies(const ConstraintPtr&)                                           const;
};

}

#endif

