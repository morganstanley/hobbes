
#include <hobbes/lang/preds/str.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/eval/cc.H>
#include <hobbes/util/str.H>
#include <memory>
#include <set>

namespace hobbes {

// split a string on a token into a tuple of strings
class SplitP : public Unqualifier {
public:
  static std::string constraintName() { return "Split"; }

  bool refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) override {
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

  bool satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const override {
    std::string delim;
    str::seq    ss;
    MonoTypePtr result;
    if (asCDelim(cst, &delim, &ss, &result)) {
      return *result == *MonoTypePtr(TString::make(str::cdelim(ss, delim)));
    }
    return false;
  }

  bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) const override {
    if (c->name() != SplitP::constraintName() || c->arguments().size() != 3) {
      return false;
    }
  
    const TString* str   = is<TString>(c->arguments()[0]);
    const TString* delim = is<TString>(c->arguments()[1]);
    const Record*  ss    = is<Record>(c->arguments()[2]);
  
    if ((str != nullptr) && (delim != nullptr) && (ss != nullptr)) {
      return satisfied(tenv, c, ds);
    } else if ((str == nullptr) && (is<TVar>(c->arguments()[0]) == nullptr)) {
      return false;
    } else if ((delim == nullptr) && (is<TVar>(c->arguments()[1]) == nullptr)) {
      return false;
    } else if ((ss == nullptr) && (is<TVar>(c->arguments()[2]) == nullptr)) {
      return false;
    } else {
      return true;
    }
  }

  void explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) override {
  }

  struct StripCst : public switchExprTyFn {
    const ConstraintPtr& constraint;
    StripCst(const ConstraintPtr& cst) : constraint(cst) {
    }
  
    ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const override {
      ExprPtr result(e);
      result->type(removeConstraint(this->constraint, qty));
      return result;
    }
  };
  
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const override {
    return switchOf(e, StripCst(cst));
  }
  
  PolyTypePtr lookup(const std::string&) const override {
    return PolyTypePtr();
  }
  
  SymSet bindings() const override {
    return SymSet();
  }
  
  FunDeps dependencies(const ConstraintPtr&) const override {
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

  // Resolving an (Ls "pattern" x) constraint globs the filesystem and puts
  // the matching names *into the type*, as a side effect of type-constraint
  // resolution rather than evaluation. Type-checking untrusted text is
  // enough to read them back: `(\x.x) :: (Ls "/etc/pa*" ps) => (ps -> ps)`
  // reports the matches in the error it fails with, so a net REPL
  // prepare(), a web GET /?<expr> or a ':t' can enumerate the filesystem of
  // the host running the compiler without evaluating anything. Disabled
  // unless the embedding application opts in, either for an exact set of
  // patterns or for any pattern.
  void enableGlobbing(const std::set<std::string>& allowedPatterns) {
    this->anyPattern      = false;
    this->allowedPatterns = allowedPatterns;
  }
  void enableGlobbing() {
    this->anyPattern = true;
    this->allowedPatterns.clear();
  }
  bool globbingAllowed(const std::string& pattern) const {
    return this->anyPattern || this->allowedPatterns.count(pattern) > 0;
  }

  bool refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) override {
    if (cst->arguments().size() == 2) {
      if (const TString* dir = is<TString>(cst->arguments()[0])) {
        if (!globbingAllowed(dir->value())) {
          throw std::runtime_error(
            "Ls constraint rejected: refusing to expand the filesystem glob '" + dir->value() +
            "' during type-constraint resolution (filesystem globbing is disabled by default; "
            "the embedding application must call cc::enableFilesystemGlobs to permit this "
            "pattern -- note that hi does not, for the compiler behind -p or -w)");
        }
        size_t c = u->size();
        mgu(cst->arguments()[1], tstrings(str::paths(dir->value())), u);
        return c != u->size();
      }
    }
    return false;
  }

  bool satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const override {
    if (cst->arguments().size() == 2) {
      if (const TString* dir = is<TString>(cst->arguments()[0])) {
        // prune rather than throw out of a satisfaction probe, the way
        // ProcessP does -- only refine() reports the rejection
        if (!globbingAllowed(dir->value())) {
          return false;
        }
        return *cst->arguments()[1] == *tstrings(str::paths(dir->value()));
      }
    }
    return false;
  }

  bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) const override {
    return c->arguments().size() == 2 && ((is<TVar>(c->arguments()[0]) != nullptr) || (is<TVar>(c->arguments()[1]) != nullptr) || satisfied(tenv, c, ds));
  }

  void explain(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*, annmsgs* msgs) override {
    if (cst->arguments().size() == 2) {
      if (const TString* dir = is<TString>(cst->arguments()[0])) {
        if (!globbingAllowed(dir->value())) {
          msgs->push_back(annmsg("expanding the filesystem glob '" + dir->value() + "' is not allowed here (see cc::enableFilesystemGlobs)", e->la()));
        }
      }
    }
  }

  struct StripCst : public switchExprTyFn {
    const ConstraintPtr& constraint;
    StripCst(const ConstraintPtr& cst) : constraint(cst) {
    }
  
    ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const override {
      ExprPtr result(e);
      result->type(removeConstraint(this->constraint, qty));
      return result;
    }
  };
  
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const override {
    return switchOf(e, StripCst(cst));
  }
  
  PolyTypePtr lookup(const std::string&) const override {
    return PolyTypePtr();
  }
  
  SymSet bindings() const override {
    return SymSet();
  }
  
  FunDeps dependencies(const ConstraintPtr&) const override {
    return list(FunDep(list(0), 1));
  }
private:
  bool                  anyPattern = false;
  std::set<std::string> allowedPatterns;
};



void initStrPredicates(const TEnvPtr& tenv) {
  tenv->bind(SplitP::constraintName(), UnqualifierPtr(new SplitP()));
  tenv->bind(LsP::constraintName(),    UnqualifierPtr(new LsP()));
}

static std::shared_ptr<LsP> lsUnqualifier(const TEnvPtr& tenv) {
  auto lp = std::dynamic_pointer_cast<LsP>(tenv->lookupUnqualifier(LsP::constraintName()));
  if (!lp) {
    throw std::runtime_error("cannot allow filesystem globs: '" + LsP::constraintName() + "' is bound to a replacement unqualifier, not the built-in one");
  }
  return lp;
}

void enableFilesystemGlobs(const TEnvPtr& tenv, const std::set<std::string>& allowedPatterns) {
  hlock _;
  lsUnqualifier(tenv)->enableGlobbing(allowedPatterns);
}

void enableFilesystemGlobs(const TEnvPtr& tenv) {
  hlock _;
  lsUnqualifier(tenv)->enableGlobbing();
}

}

