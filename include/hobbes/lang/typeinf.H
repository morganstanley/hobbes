
#ifndef HOBBES_LANG_TYPEINF_HPP_INCLUDED
#define HOBBES_LANG_TYPEINF_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/lang/expr.H>
#include <hobbes/util/unionfind.H>
#include <map>
#include <set>

namespace hobbes {

// type equivalence data
struct UTypeRec {
  MonoTypePtr ty;
  bool        visited;
  size_t      donebc;
};

// represent a type substitution abstractly as disjoint sets of equivalent types ordered by 'definedness'
struct LiftUType {
  static UTypeRec apply(const MonoTypePtr&);
};

struct MoreDefinedType {
  static const UTypeRec& apply(const UTypeRec&, const UTypeRec&);
};

class MonoTypeUnifier {
public:
  MonoTypeUnifier(const TEnvPtr&);
  MonoTypeUnifier(const MonoTypeUnifier&);
  MonoTypeUnifier& operator=(const MonoTypeUnifier&);

  // what type environment are we unifying in?
  const TEnvPtr& typeEnv() const;

  // how many unifications have we made?
  size_t size() const;

  // suppress/unsuppress bindings to a particularly-named variable
  void suppress(const std::string&);
  void suppress(const str::seq&);
  void unsuppress(const std::string&);
  void unsuppress(const str::seq&);

  // read/write a var-name/type association
  void bind(const std::string&, const MonoTypePtr&);
  MonoTypePtr binding(const std::string&);

  // specify that two types should be equal
  void unify(const MonoTypePtr&, const MonoTypePtr&);

  // resolve the input type against the local mapping
  MonoTypePtr substitute(const MonoTypePtr&);
  MonoTypes   substitute(const MonoTypes&);

  // merge another unifier's equivalence set with this one
  size_t merge(const MonoTypeUnifier&);

  // represent this unification set as a type substitution
  MonoTypeSubst substitution();
private:
  TEnvPtr tenv;
  size_t bcount;

  // avoid binding to certain type variables
  using SuppressVarCounts = std::map<std::string, size_t>;
  SuppressVarCounts suppressVarCounts;

  bool suppressed(const std::string&) const;
  bool suppressed(const MonoTypePtr&) const;

  // equivalences between types
  using M = equivalence_mapping<MonoTypePtr, UTypeRec, LiftUType, MoreDefinedType>;
  M m;
};

// simplify safe suppression of type variables
class scoped_unification_suppression {
public:
  scoped_unification_suppression(MonoTypeUnifier*, const std::string&);
  ~scoped_unification_suppression();
private:
  MonoTypeUnifier* u;
  std::string      sv;
};

// recursively apply unification substitution in-place
MonoTypePtr   substitute(MonoTypeUnifier*, const MonoTypePtr&);
MonoTypes     substitute(MonoTypeUnifier*, const MonoTypes&);
ConstraintPtr substitute(MonoTypeUnifier*, const ConstraintPtr&);
Constraints   substitute(MonoTypeUnifier*, const Constraints&);
QualTypePtr   substitute(MonoTypeUnifier*, const QualTypePtr&);
ExprPtr       substitute(MonoTypeUnifier*, const ExprPtr&);

// a simple test to determine if two types _can_ be unified
bool unifiable(const TEnvPtr&, const MonoTypePtr&, const MonoTypePtr&);
bool unifiable(const TEnvPtr&, const MonoTypes&, const MonoTypes&);
bool unifiable(const TEnvPtr&, const ConstraintPtr&, const ConstraintPtr&);

// allow constraint refinement according to implied type membership
bool refine(const TEnvPtr& tenv, const ConstraintPtr& c, MonoTypeUnifier* s, Definitions*);
bool refine(const TEnvPtr& tenv, const Constraints& cs, MonoTypeUnifier* s, Definitions*);

// when we infer/validate types, this can produce a modified expression (for explicit resolution of implicit qualifiers, and explicit type annotations)
ExprPtr validateType(const TEnvPtr& tenv, const ExprPtr& e, Definitions*);

// extend type inference to definitions, which can be generally recursive
ExprPtr validateType(const TEnvPtr& tenv, const std::string& vname, const ExprPtr& e, Definitions*);

// type unification
void mgu(const ExprPtr&, const ExprPtr&, MonoTypeUnifier*);
void mgu(const ExprPtr&, const QualTypePtr&, MonoTypeUnifier*);
void mgu(const ExprPtr&, const MonoTypePtr&, MonoTypeUnifier*);

void mgu(const QualTypePtr& t0, const QualTypePtr& t1, MonoTypeUnifier* u);
void mgu(const MonoTypePtr& t0, const MonoTypePtr& t1, MonoTypeUnifier* u);
void mgu(const MonoTypes& ts0, const MonoTypes& ts1, MonoTypeUnifier* u);

void mgu(const ConstraintPtr&, const ConstraintPtr&, MonoTypeUnifier*);

// some utilities for dealing with qualified types
using QualLiftedMonoTypes = std::pair<Constraints, MonoTypes>;
QualLiftedMonoTypes liftQualifiers(const QualTypes& qts);

using QualLiftedMonoType = std::pair<Constraints, MonoTypePtr>;

}

#endif

