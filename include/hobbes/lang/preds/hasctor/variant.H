
#ifndef HOBBES_LANG_TYPEPREDS_HASCTOR_VARIANT_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_HASCTOR_VARIANT_HPP_INCLUDED

#include <hobbes/lang/preds/hasctor.H>

namespace hobbes {

struct HCVariantEliminator : public HCEliminator {
  bool satisfied(const TEnvPtr& tenv, const HasCtor&, Definitions*) const override;
  bool satisfiable(const TEnvPtr& tenv, const HasCtor&, Definitions*) const override;
  bool refine(const TEnvPtr& tenv, const HasCtor&, MonoTypeUnifier* s, Definitions*) override;
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const HasCtor&, const ExprPtr&, Definitions*) const override;
  std::string name() const override;
};

const Variant* isVariantPEnum(const MonoTypePtr& e);

}

#endif

