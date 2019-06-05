
#include <hobbes/lang/preds/str.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/util/str.H>

namespace hobbes {

// split a string on a token into a tuple of strings
class SplitP : public Unqualifier {
public:
  static std::string constraintName() { return "Split"; }

  bool refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
    // try to refine as a csplit
    {
      std::string str;
      std::string delim;
      MonoTypePtr result;
      if (asCSplit(cst, &str, &delim, &result)) {
        size_t z = u->size();
        mgu(result, csplitType(str, delim), u);
        return z != u->size();
      }
    }

    // try to refine as a cdelim
    {
      std::string delim;
      str::seq    ss;
      MonoTypePtr result;
      if (asCDelim(cst, &delim, &ss, &result)) {
        size_t z = u->size();
        mgu(result, MonoTypePtr(TString::make(str::cdelim(ss, delim))), u);
        return z != u->size();
      }
    }

    return false;
  }

  bool satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
    std::string delim;
    str::seq    ss;
    MonoTypePtr result;
    if (asCDelim(cst, &delim, &ss, &result)) {
      return *result == *MonoTypePtr(TString::make(str::cdelim(ss, delim)));
    }
    return false;
  }

  bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) const {
    if (c->name() != SplitP::constraintName() || c->arguments().size() != 3) {
      return false;
    }
  
    const TString* str   = is<TString>(c->arguments()[0]);
    const TString* delim = is<TString>(c->arguments()[1]);
    const Record*  ss    = is<Record>(c->arguments()[2]);
  
    if (str && delim && ss) {
      return satisfied(tenv, c, ds);
    } else if (!str && !is<TVar>(c->arguments()[0])) {
      return false;
    } else if (!delim && !is<TVar>(c->arguments()[1])) {
      return false;
    } else if (!ss && !is<TVar>(c->arguments()[2])) {
      return false;
    } else {
      return true;
    }
  }

  void explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
  }

  struct StripCst : public switchExprTyFn {
    const ConstraintPtr& constraint;
    StripCst(const ConstraintPtr& cst) : constraint(cst) {
    }
  
    ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
      ExprPtr result(e);
      result->type(removeConstraint(this->constraint, qty));
      return result;
    }
  };
  
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
    return switchOf(e, StripCst(cst));
  }
  
  PolyTypePtr lookup(const std::string&) const {
    return PolyTypePtr();
  }
  
  SymSet bindings() const {
    return SymSet();
  }
  
  FunDeps dependencies(const ConstraintPtr&) const {
    return list(FunDep(list(0, 1), 2), FunDep(list(1, 2), 0));
  }
private:
  static bool asCSplit(const ConstraintPtr& c, std::string* str, std::string* delim, MonoTypePtr* result) {
    if (c->name() == SplitP::constraintName() && c->arguments().size() == 3) {
      if (const TString* strT = is<TString>(c->arguments()[0])) {
        if (const TString* delimT = is<TString>(c->arguments()[1])) {
          *str    = strT->value();
          *delim  = delimT->value();
          *result = c->arguments()[2];
  
          return true;
        }
      }
    }
    return false;
  }
  
  static MonoTypePtr csplitType(const std::string& str, const std::string& delim) {
    str::seq ss = str::csplit(str, delim);
    Record::Members ms;
    for (size_t i = 0; i < ss.size(); ++i) {
      ms.push_back(Record::Member(".f" + str::from(i), MonoTypePtr(TString::make(ss[i]))));
    }
    return MonoTypePtr(Record::make(ms));
  }
  
  static bool asCDelim(const ConstraintPtr& c, std::string* delim, str::seq* strs, MonoTypePtr* result) {
    if (c->name() == SplitP::constraintName() && c->arguments().size() == 3) {
      if (const TString* delimT = is<TString>(c->arguments()[1])) {
        if (const Record* strsR = is<Record>(c->arguments()[2])) {
          *result = c->arguments()[0];
          *delim  = delimT->value();
  
          MonoTypes strsT = selectTypes(strsR->members());
          for (const auto& strT : strsT) {
            if (const TString* str = is<TString>(strT)) {
              strs->push_back(str->value());
            } else {
              return false;
            }
          }
  
          return true;
        }
      }
    }
    
    return false;
  }
};

// get a directory listing
class LsP : public Unqualifier {
public:
  static std::string constraintName() { return "Ls"; }

  bool refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
    if (cst->arguments().size() == 2) {
      if (const TString* dir = is<TString>(cst->arguments()[0])) {
        size_t c = u->size();
        mgu(cst->arguments()[1], tstrings(str::paths(dir->value())), u);
        return c != u->size();
      }
    }
    return false;
  }

  bool satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
    if (cst->arguments().size() == 2) {
      if (const TString* dir = is<TString>(cst->arguments()[0])) {
        return *cst->arguments()[1] == *tstrings(str::paths(dir->value()));
      }
    }
    return false;
  }

  bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) const {
    return c->arguments().size() == 2 && (is<TVar>(c->arguments()[0]) || is<TVar>(c->arguments()[1]) || satisfied(tenv, c, ds));
  }

  void explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
  }

  struct StripCst : public switchExprTyFn {
    const ConstraintPtr& constraint;
    StripCst(const ConstraintPtr& cst) : constraint(cst) {
    }
  
    ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
      ExprPtr result(e);
      result->type(removeConstraint(this->constraint, qty));
      return result;
    }
  };
  
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
    return switchOf(e, StripCst(cst));
  }
  
  PolyTypePtr lookup(const std::string&) const {
    return PolyTypePtr();
  }
  
  SymSet bindings() const {
    return SymSet();
  }
  
  FunDeps dependencies(const ConstraintPtr&) const {
    return list(FunDep(list(0), 1));
  }
};



void initStrPredicates(const TEnvPtr& tenv) {
  tenv->bind(SplitP::constraintName(), UnqualifierPtr(new SplitP()));
  tenv->bind(LsP::constraintName(),    UnqualifierPtr(new LsP()));
}

}

