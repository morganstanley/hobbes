
#ifndef HOBBES_LANG_TYPEPREDS_HASCTOR_VARIANT_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_HASCTOR_VARIANT_HPP_INCLUDED

#include <hobbes/lang/preds/hasctor.H>

namespace hobbes {

struct HCVariantEliminator : public HCEliminator {
  bool satisfied(const TEnvPtr& tenv, const HasCtor&, Definitions*) const;
  bool satisfiable(const TEnvPtr& tenv, const HasCtor&, Definitions*) const;
  bool refine(const TEnvPtr& tenv, const HasCtor&, MonoTypeUnifier* s, Definitions*);
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const HasCtor&, const ExprPtr&, Definitions*) const;
  std::string name() const;
};

const Variant* variantInPEnum(const MonoTypePtr& e);

}

#endif

