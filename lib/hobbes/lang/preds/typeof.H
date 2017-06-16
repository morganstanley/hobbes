
#ifndef HOBBES_LANG_TYPEPREDS_TYPEOF_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_TYPEOF_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>
#include <string>

namespace hobbes {

// associate a type variable with the inferred type of an expression
class TypeofP : public Unqualifier {
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

