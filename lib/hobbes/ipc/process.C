#include <hobbes/ipc/process.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/lang/preds/class.H>

namespace hobbes {

#define PROCESS_SPAWN "spawn"

ProcessP::ProcessP(FieldVerifier* fv) {
  fv->addEliminator(&this->procman);
}

static bool dec(const ConstraintPtr& c, MonoTypePtr* lhs, MonoTypePtr* rhs) {
  if (c->name() == ProcessP::constraintName() && c->arguments().size() == 2) {
    *lhs = c->arguments()[0];
    *rhs = c->arguments()[1];
    return true;
  }
  return false;
}

std::string ProcessP::constraintName() {
  return "Process";
}

// resolve process spawn constraints
bool ProcessP::refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
  size_t uc = u->size();
  MonoTypePtr cmdt, pidt;
  if (dec(cst, &cmdt, &pidt)) {
    if (const TString* cmd = is<TString>(cmdt)) {
      mgu(pidt, mkPidTy(this->procman.spawnedPid(cmd->value())), u);
    }
  }
  return uc != u->size();
}

bool ProcessP::satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
  MonoTypePtr cmdt, pidt;
  if (dec(cst, &cmdt, &pidt)) {
    if (const TString* cmd = is<TString>(cmdt)) {
      if (const TLong* pid = pidTy(pidt)) {
        return this->procman.isSpawnedPid(cmd->value(), pid->value());
      }
    }
  }
  return false;
}

bool ProcessP::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  MonoTypePtr cmdt, pidt;
  if (dec(cst, &cmdt, &pidt)) {
    if (is<TVar>(cmdt)) {
      return true;
    } else if (is<TVar>(pidt)) {
      return true;
    } else {
      return satisfied(tenv, cst, ds);
    }
  } else {
    return false;
  }
}

void ProcessP::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

struct ProcessPUnqualify : public switchExprTyFn {
  const ConstraintPtr& constraint;
  long pid;

  ProcessPUnqualify(const ConstraintPtr& constraint) : constraint(constraint) {
    MonoTypePtr cmdt, pidt;
    if (!dec(constraint, &cmdt, &pidt)) {
      throw std::runtime_error("Internal error, invalid constraint for process resolution");
    }
    const TLong* tpid = pidTy(pidt);
    if (!tpid) { throw std::runtime_error("Internal error, unresolved process constraint for resolution"); }
    this->pid = tpid->value();
  }

  QualTypePtr withTy(const QualTypePtr& qt) const {
    return removeConstraint(this->constraint, qt);
  }

  ExprPtr with(const App* ap) const {
    if (const Var* fn = is<Var>(stripAssumpHead(ap->fn()))) {
      if (fn->value() == PROCESS_SPAWN && hasConstraint(this->constraint, fn->type())) {
        return wrapWithTy(ap->type(), new Long(this->pid, fn->la()));
      }
    }
    return ExprPtr(new App(switchOf(ap->fn(), *this), switchOf(ap->args(), *this), ap->la()));
  }

  ExprPtr with(const Var* v) const {
    if (hasConstraint(this->constraint, v->type())) {
      throw std::runtime_error("spawn to lambda NYI (unnecessary?)");
    }
    return wrapWithTy(v->type(), v->clone());
  }
};

ExprPtr ProcessP::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, ProcessPUnqualify(cst));
}

PolyTypePtr ProcessP::lookup(const std::string& vn) const {
  if (vn == PROCESS_SPAWN) {
    return polytype(2, qualtype(list(ConstraintPtr(new Constraint(ProcessP::constraintName(), list(tgen(0), tgen(1))))), functy(list(tuplety()), tgen(1))));
  } else {
    return PolyTypePtr();
  }
}

SymSet ProcessP::bindings() const {
  SymSet r;
  r.insert(PROCESS_SPAWN);
  return r;
}

FunDeps ProcessP::dependencies(const ConstraintPtr&) const {
  FunDeps result;
  result.push_back(FunDep(list(0), 1));
  return result;
}

}

