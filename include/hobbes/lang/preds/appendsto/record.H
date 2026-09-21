
#ifndef HOBBES_LANG_TYPEPREDS_APPENDSTO_RECORD_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_APPENDSTO_RECORD_HPP_INCLUDED

#include <hobbes/lang/preds/appendsto.H>

namespace hobbes {

struct ATRecordEliminator : public ATEliminator {
  bool satisfied(const TEnvPtr& tenv, const MonoTypePtr& rty, const MonoTypePtr& fname, const MonoTypePtr& fty) const override;
  bool satisfiable(const TEnvPtr& tenv, const MonoTypePtr& rty, const MonoTypePtr& fname, const MonoTypePtr& fty) const override;
  bool refine(const TEnvPtr& tenv, const MonoTypePtr& rty, const MonoTypePtr& fieldName, const MonoTypePtr& fty, MonoTypeUnifier* s) override;
  PolyTypePtr lookup(const std::string& vn) const override;
  SymSet bindings() const override;
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*) const override;
};

}

#endif
