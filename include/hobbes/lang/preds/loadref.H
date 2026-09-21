
#ifndef HOBBES_LANG_TYPEPREDS_LOADREF_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_LOADREF_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>
#include <string>

namespace hobbes {

// resolve references to values loaded from a database file
class DBFileRefLoader : public Unqualifier {
public:
  static std::string constraintName();

  // unqualifier interface
  bool        refine(const TEnvPtr&,const ConstraintPtr&,MonoTypeUnifier*,Definitions*) override;
  bool        satisfied(const TEnvPtr&,const ConstraintPtr&,Definitions*)                const override;
  bool        satisfiable(const TEnvPtr&,const ConstraintPtr&,Definitions*)              const override;
  void        explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs) override;
  ExprPtr     unqualify(const TEnvPtr&,const ConstraintPtr&,const ExprPtr&,Definitions*) const override;
  PolyTypePtr lookup   (const std::string& vn)                                           const override;
  SymSet      bindings ()                                                                const override;
  FunDeps     dependencies(const ConstraintPtr&)                                         const override;
};

}

#endif

