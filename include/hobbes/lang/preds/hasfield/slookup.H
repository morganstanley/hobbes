// eliminate (A.x :: T) constraints by trying to compile slookup(A) :: (SLookup typeof(A) value(x) T) => T

#ifndef HOBBES_LANG_TYPEPREDS_HASFIELD_SLOOKUP_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_HASFIELD_SLOOKUP_HPP_INCLUDED

#include <hobbes/lang/preds/hasfield.H>

namespace hobbes {

struct HFSLookupEliminator : public HFEliminator {
  bool satisfied(const TEnvPtr& tenv, const HasField&, Definitions*) const override;
  bool satisfiable(const TEnvPtr& tenv, const HasField&, Definitions*) const override;
  bool refine(const TEnvPtr& tenv, const HasField&, MonoTypeUnifier* s, Definitions*) override;
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*) const override;
  std::string name() const override;
};

}

#endif

