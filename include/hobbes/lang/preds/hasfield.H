
#ifndef HOBBES_LANG_TYPEPREDS_HASFIELD_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_HASFIELD_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>

namespace hobbes {

// a 'hasfield' constraint ensures that a given type has a field with a certain name, with a certain type
// this constraint is intended to be generic, to subsume record projection, variant construction, and "object methods"
//   e.g.:
//    yes: HasField {x:int,y:bool} "y" bool
//    no:  HasField {x:int,y:bool} "chicken" int
//    no:  HasField {x:int,y:bool} "y" int
struct HasField {
  enum Direction { Read, Write };

  Direction   direction;
  MonoTypePtr recordType;
  ExprPtr     recordExpr;
  MonoTypePtr fieldName;
  MonoTypePtr fieldType;

  static Constraint* newConstraint(Direction,const MonoTypePtr&, const MonoTypePtr&, const MonoTypePtr&, const ExprPtr&);
  static Constraint* newConstraint(Direction,const MonoTypePtr&, const MonoTypePtr&, const MonoTypePtr&);
  static ConstraintPtr constraint(Direction,const MonoTypePtr&, const MonoTypePtr&, const MonoTypePtr&, const ExprPtr&);
  static ConstraintPtr constraint(Direction,const MonoTypePtr&, const MonoTypePtr&, const MonoTypePtr&);
};
bool dec(const ConstraintPtr&, HasField*);
void upd(const ConstraintPtr&, const HasField&);

// a "has-field eliminator" knows how to resolve a "HasField" constraint at a particular (category of) type
struct HFEliminator {
  virtual ~HFEliminator() = default;

  // is this HF instance eliminable?
  virtual bool satisfied(const TEnvPtr& tenv, const HasField&, Definitions*) const = 0;

  // is it possible for this HF instance to eventually be eliminated?
  virtual bool satisfiable(const TEnvPtr& tenv, const HasField&, Definitions*) const = 0;

  // refine the substitution set associated with this constraint
  virtual bool refine(const TEnvPtr& tenv, const HasField&, MonoTypeUnifier* s, Definitions*) = 0;

  // unqualify a constraint (satisfied() must have returned true)
  virtual ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*) const = 0;

  // what would you call this eliminator?
  virtual std::string name() const = 0;
};

// a 'field verifier' is a scheme for validating types as containing a named field somehow
class FieldVerifier : public Unqualifier {
public:
  FieldVerifier();
  static std::string constraintName();

  // extend the set of 'hasfield' eliminators dynamically (dangerous?)
  void addEliminator(HFEliminator*);

  // unqualifier interface
  bool        refine(const TEnvPtr&,const ConstraintPtr&,MonoTypeUnifier*,Definitions*);
  bool        satisfied(const TEnvPtr&,const ConstraintPtr&,Definitions*)                  const;
  bool        satisfiable(const TEnvPtr&,const ConstraintPtr&,Definitions*)                const;
  void        explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs);
  ExprPtr     unqualify(const TEnvPtr&,const ConstraintPtr&, const ExprPtr&, Definitions*) const;
  PolyTypePtr lookup   (const std::string& vn)                                             const;
  SymSet      bindings ()                                                                  const;
  FunDeps     dependencies(const ConstraintPtr&)                                           const;
private:
  typedef std::vector<HFEliminator*> HFEliminators;
  HFEliminators eliminators;
};

}

#endif

