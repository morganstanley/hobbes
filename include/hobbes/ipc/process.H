
#ifndef HOBBES_LANG_TYPEPREDS_PROCESS_PROCESS_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_PROCESS_PROCESS_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>
#include <hobbes/ipc/procman.H>
#include <string>
#include <memory>

namespace hobbes {

// resolve process invocation constraints
class ProcessP : public Unqualifier {
public:
  ProcessP(FieldVerifier*);

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
private:
  std::shared_ptr<ProcManager> procman{std::make_shared<ProcManager>()};
};

}

#endif

