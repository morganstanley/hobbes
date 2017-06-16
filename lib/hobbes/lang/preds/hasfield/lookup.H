// eliminate (A.x :: T) constraints by trying to compile lookup(value(x), A) :: T

#ifndef HOBBES_LANG_TYPEPREDS_HASFIELD_LOOKUP_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_HASFIELD_LOOKUP_HPP_INCLUDED

#include <hobbes/lang/preds/hasfield.H>

namespace hobbes {

struct HFLookupEliminator : public HFEliminator {
  bool satisfied(const TEnvPtr& tenv, const HasField&, Definitions*) const;
  bool satisfiable(const TEnvPtr& tenv, const HasField&, Definitions*) const;
  bool refine(const TEnvPtr& tenv, const HasField&, MonoTypeUnifier* s, Definitions*);
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*) const;
  std::string name() const;
};

}

#endif

