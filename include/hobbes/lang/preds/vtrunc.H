
#ifndef HOBBES_LANG_TYPEPREDS_VARIANTTRUNC_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_VARIANTTRUNC_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>

namespace hobbes {

// a 'VariantTrunc' constraint determines the 'enum section' from any variant type
//   e.g.:
//    yes: VariantTrunc |x:int,y:bool| |x, y|
//    no:  VariantTrunc |x:int,y:bool| |y, x|    (doesn't match structure, x should come first)
//
// this constraint can be inferred "forward" (when reducing a variant with known structure)
class VariantTruncP : public Unqualifier {
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

