
#ifndef HOBBES_LANG_TYPEPREDS_PROCESS_PROCESS_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_PROCESS_PROCESS_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>
#include <hobbes/ipc/procman.H>
#include <string>

namespace hobbes {

// resolve process invocation constraints
class ProcessP : public Unqualifier {
public:
  ProcessP(FieldVerifier*);

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
private:
  ProcManager procman;
};

}

#endif

