
#ifndef HOBBES_LANG_TYPEPREDS_PROCESS_PROCMAN_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_PROCESS_PROCMAN_HPP_INCLUDED

#include <hobbes/lang/preds/hasfield.H>
#include <hobbes/ipc/prepl.H>

namespace hobbes {

class ProcManager : public HFEliminator {
public:
  long spawnedPid(const std::string&);
  bool isSpawnedPid(const std::string&, long) const;

  // hf eliminator interface
  bool satisfied(const TEnvPtr&, const HasField&, Definitions*) const override;
  bool satisfiable(const TEnvPtr&, const HasField&, Definitions*) const override;
  bool refine(const TEnvPtr&, const HasField&, MonoTypeUnifier*, Definitions*) override;
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*) const override;
  std::string name() const override;
private:
  friend struct ProcManUnqualify;

  using SpawnedProcs = std::map<std::string, proc>;
  SpawnedProcs procs;

  const proc& lp(long) const;
};

// shorthand for working with process types
MonoTypePtr mkPidTy(long pid);
const TLong* pidTy(const MonoTypePtr& mty);

}

#endif

