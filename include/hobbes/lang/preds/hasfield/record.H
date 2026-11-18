
#ifndef HOBBES_LANG_TYPEPREDS_HASFIELD_RECORD_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_HASFIELD_RECORD_HPP_INCLUDED

#include <hobbes/lang/preds/hasfield.H>

namespace hobbes {

// the name of the record field that a constraint's field name stands for, if it stands for one
// (a string names a field, and a number the tuple field at that position)
bool isFieldName(const MonoTypePtr& fn, std::string* sfn);

struct HFRecordEliminator : public HFEliminator {
  bool satisfied(const TEnvPtr& tenv, const HasField&, Definitions*) const override;
  bool satisfiable(const TEnvPtr& tenv, const HasField&, Definitions*) const override;
  bool refine(const TEnvPtr& tenv, const HasField&, MonoTypeUnifier* s, Definitions*) override;
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*) const override;

  // unqualify several constraints in one traversal of the expression
  // (equivalent to unqualifying each of them in turn; satisfied() must have returned true for each)
  ExprPtr unqualify(const TEnvPtr&, const Constraints&, const ExprPtr&, Definitions*) const;
  std::string name() const override;
};

}

#endif

