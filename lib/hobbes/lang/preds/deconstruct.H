/*
 * deconstruct : a single step of "destructuring" a type (maybe a little bit of a hack?)
 */

#ifndef HOBBES_LANG_TYPEPREDS_DECONSTRUCT_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_DECONSTRUCT_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>
#include <string>

namespace hobbes {

class DeconstructP : public Unqualifier {
public:
  static std::string constraintName();

  // unqualifier interface
  bool        refine(const TEnvPtr&,const ConstraintPtr&,MonoTypeUnifier*,Definitions*);
  bool        satisfied(const TEnvPtr&,const ConstraintPtr&,Definitions*)                const;
  bool        satisfiable(const TEnvPtr&,const ConstraintPtr&,Definitions*)              const;
  void        explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs);
  ExprPtr     unqualify(const TEnvPtr&,const ConstraintPtr&,const ExprPtr&,Definitions*) const;
  PolyTypePtr lookup   (const std::string& vn)                                           const;
  SymSet      bindings ()                                                                const;
  FunDeps     dependencies(const ConstraintPtr&)                                         const;
};

}

#endif

