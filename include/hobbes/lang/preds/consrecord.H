
#ifndef HOBBES_LANG_TYPEPREDS_CONSRECORD_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_CONSRECORD_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>

namespace hobbes {

// a 'consrecord' constraint asserts that a type is a record with a head type and a tail type
//   e.g.:
//    yes: ConsRecord {x:int,y:bool} int {y:bool}
//    no:  ConsRecord {x:int,y:bool} bool {x:int}    (doesn't match structure, x should come first)
//
// for syntactic convenience, this constraint conditionally works for either records or tuples
// (tuples are treated as a special case of records where the field names are unimportant and are hidden)
//
// this constraint can be inferred "forward" (when deconstructing a large record into smaller parts),
// or it can be inferred "backward" (when constructing a large record from smaller parts)
// a 'record deconstructor' is a scheme for consuming record type structures at compile-time
class RecordDeconstructor : public Unqualifier {
public:
  static std::string constraintName();

  // unqualifier interface
  bool        refine(const TEnvPtr&,const ConstraintPtr&,MonoTypeUnifier*,Definitions*) override;
  bool        satisfied(const TEnvPtr&,const ConstraintPtr&,Definitions*)                  const override;
  bool        satisfiable(const TEnvPtr&,const ConstraintPtr&,Definitions*)                const override;
  void        explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs) override;
  ExprPtr     unqualify(const TEnvPtr&,const ConstraintPtr&, const ExprPtr&, Definitions*) const override;
  PolyTypePtr lookup   (const std::string& vn)                                             const override;
  SymSet      bindings ()                                                                  const override;
  FunDeps     dependencies(const ConstraintPtr&)                                           const override;
};

}

#endif

