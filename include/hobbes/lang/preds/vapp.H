
#ifndef HOBBES_LANG_TYPEPREDS_VARIANTAPP_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_VARIANTAPP_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>

namespace hobbes {

// a 'VariantApp' constraint asserts that a record of functions can be applied to a variant as analogous to 'match' on cases
//   e.g.:
//    yes: VariantApp |x:int,y:bool| {x:int -> r, y:bool -> r}
//    no:  VariantApp |x:int,y:bool| {y:bool -> r, x:int -> r}    (doesn't match structure, x should come first)
//
// this constraint can be inferred "forward" (when reducing a variant with known structure but unknown function)
class VariantAppP : public Unqualifier {
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

