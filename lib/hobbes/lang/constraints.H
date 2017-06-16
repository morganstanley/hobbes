/******
 * constraints : eliminate redundant constraints (avoid redundant and costly unqualification)
 ******/

#ifndef HOBBES_LANG_CONSTRAINTS_HPP_INCLUDED
#define HOBBES_LANG_CONSTRAINTS_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/lang/typemap.H>
#include <set>

namespace hobbes {

// we may need to perform unification when simplifying constraints
class MonoTypeUnifier;

// represent a set of unique constraints for efficient uniqueness determination
class ConstraintSet {
public:
  ConstraintSet();

  // add a constraint to the set
  // if the constraint is entailed by the set already, unify anything necessary in the input constraint and add nothing
  void insert(const TEnvPtr&, const ConstraintPtr&, MonoTypeUnifier*);
  Constraints constraints() const;
private:
  typedef type_map<ConstraintPtr> CSet;
  CSet csts;
};

void typeSeqForm(const ConstraintPtr&, MonoTypes*);

}

#endif
