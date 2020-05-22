#ifndef HOBBES_LANG_TYPEPREDS_DEPENDENT_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_DEPENDENT_HPP_INCLUDED

#include <hobbes/lang/tyunqualify.H>
#include <hobbes/eval/cc.H>
namespace hobbes{
  class TypeApply : public Unqualifier {
	  private:
		  cc*c;
    public:
	  TypeApply(cc*c) : c(c) {};
      static std::string constraintName();
      bool refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*);
      ExprPtr unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const;
      bool satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const;
      bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const;
      void explain(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*, annmsgs* msgs);
      PolyTypePtr lookup(const std::string& vn) const;
      SymSet bindings() const;
      FunDeps dependencies(const ConstraintPtr&) const;
  };

  class TypeValueLower : public Unqualifier {
    public:
      static std::string constraintName();
      bool refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*);
      ExprPtr unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const;
      bool satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const;
      bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions*) const;
      void explain(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*, annmsgs* msgs);
      PolyTypePtr lookup(const std::string& vn) const;
      SymSet bindings() const;
      FunDeps dependencies(const ConstraintPtr&) const;
  };
}

#endif
