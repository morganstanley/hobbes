
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
  bool satisfied(const TEnvPtr&, const HasField&, Definitions*) const;
  bool satisfiable(const TEnvPtr&, const HasField&, Definitions*) const;
  bool refine(const TEnvPtr&, const HasField&, MonoTypeUnifier*, Definitions*);
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*) const;
  std::string name() const;
private:
  friend struct ProcManUnqualify;

  typedef std::map<std::string, proc> SpawnedProcs;
  SpawnedProcs procs;

  const proc& lp(long) const;
};

// shorthand for working with process types
MonoTypePtr mkPidTy(long pid);
const TLong* pidTy(const MonoTypePtr& mty);

}

#endif

