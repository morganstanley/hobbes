
#include <hobbes/ipc/procman.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/util/str.H>

namespace hobbes {

bool ProcManager::isSpawnedPid(const std::string& cmd, long pid) const {
  auto ce = this->procs.find(cmd);
  return (ce != this->procs.end()) && (ce->second.pid == pid);
}

const proc& ProcManager::lp(long pid) const {
  for (const auto& pe : this->procs) {
    if (pe.second.pid == pid) {
      return pe.second;
    }
  }
  throw std::runtime_error("Not a spawned process: " + str::from(pid));
}

bool ProcManager::refine(const TEnvPtr&, const HasField& hf, MonoTypeUnifier* u, Definitions*) {
  auto rty   = hf.recordType;
  auto fname = hf.fieldName;
  auto hasty = hf.fieldType;

  size_t uc = u->size();
  if (const TString* fn = is<TString>(fname)) {
    if (const TLong* pid = pidTy(rty)) {
      mgu(hasty, refinedType(lp(pid->value()), fn->value(), hasty), u);
    }
  }
  return uc != u->size();
}

bool ProcManager::satisfied(const TEnvPtr& tenv, const HasField& hf, Definitions* ds) const {
  auto dir   = hf.direction;
  auto rty   = hf.recordType;
  auto fname = hf.fieldName;
  auto hasty = hf.fieldType;

  if (dir == HasField::Write) { return false; }

  if (const TString* fn = is<TString>(fname)) {
    if (const TLong* pid = pidTy(rty)) {
      if (const Func* fty = is<Func>(hasty)) {
        // this ensures that we _can_ get an invocation ID for the requested function/type
        invocationID(lp(pid->value()), fn->value(), hasty);

        // then we're satisfied IFF we can pack the argument and unpack the result
        return isClassSatisfied(tenv, "BlockCodec", list(fty->argument()), ds) &&
               isClassSatisfied(tenv, "BlockCodec", list(fty->result()), ds);
      }
    }
  }

  return false;
}

bool ProcManager::satisfiable(const TEnvPtr& tenv, const HasField& hf, Definitions* ds) const {
  auto dir   = hf.direction;
  auto rty   = hf.recordType;
  auto fname = hf.fieldName;
  auto hasty = hf.fieldType;

  if (dir == HasField::Write) { return false; }

  if (is<TVar>(rty)) {
    return true;
  } else if (!pidTy(rty)) {
    return false;
  } else if (is<TVar>(fname) || !isMonoSingular(hasty)) {
    return true;
  } else {
    return satisfied(tenv, hf, ds);
  }
}

struct ProcManUnqualify : public switchExprTyFn {
  const ProcManager*   pthis;
  const TEnvPtr&       tenv;
  const ConstraintPtr& constraint;
  Definitions*         defs;

  ProcManUnqualify(const ProcManager* pthis, const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* defs) : pthis(pthis), tenv(tenv), constraint(cst), defs(defs) {
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    ExprPtr aresult(new Assump(result, result->type(), result->la()));
    aresult->type(result->type());
    return aresult;
  }

  ExprPtr annotateTypes(const ExprPtr& e) const {
    ExprPtr sexp = validateType(this->tenv, e, this->defs);

    if (sexp->type()->constraints().size() == 0) {
      return sexp;
    } else {
      return unqualifyTypes(this->tenv, sexp, this->defs);
    }
  }

  ExprPtr with(const Fn* v) const {
    const Func* fty = is<Func>(v->type()->monoType());
    if (!fty) {
      throw std::runtime_error("Internal error, expected annotated function type");
    }
    return wrapWithTy(v->type(),
      new Fn(
        v->varNames(), 
        switchOf(v->body(), ProcManUnqualify(this->pthis, fnFrame(this->tenv, v->varNames(), fty->parameters()), this->constraint, this->defs)),
        v->la()
      )
    );
  }

  ExprPtr with(const Let* v) const {
    return wrapWithTy(v->type(),
      new Let(
        v->var(),
        switchOf(v->varExpr(),  *this),
        switchOf(v->bodyExpr(), ProcManUnqualify(this->pthis, fnFrame(this->tenv, list(v->var()), list(v->varExpr()->type()->monoType())), this->constraint, this->defs)),
        v->la()
      )
    );
  }

  const proc* isProcRef(const Proj* f) const {
    if (hasConstraint(this->constraint, f->type())) {
      if (const TLong* pid = pidTy(f->record()->type()->monoType())) {
        return &(pthis->lp(pid->value()));
      }
    }
    return 0;
  }

  static ConstraintPtr blockCodecCst(const MonoTypePtr& ty) {
    return ConstraintPtr(new Constraint("BlockCodec", list(ty)));
  }

  ExprPtr blockWrite(const proc* p, const ExprPtr& e) const {
    const auto& la = e->la();
    ExprPtr we = fncall(var("writeTo", qualtype(list(blockCodecCst(e->type()->monoType())), functy(list(primty("int"), e->type()->monoType()), primty("unit"))), la), list(constant(p->write_fd, la), e), la);
    return unqualifyClass(this->tenv, "BlockCodec", list(e->type()->monoType()), we, this->defs);
  }

  ExprPtr blockRead(const proc* p, const MonoTypePtr& ty) const {
    auto la = LexicalAnnotation::null();
    ExprPtr re = fncall(var("readFrom", qualtype(list(blockCodecCst(ty)), functy(list(primty("int")), ty)), la), list(constant(p->read_fd, la)), la);
    return unqualifyClass(this->tenv, "BlockCodec", list(re->type()->monoType()), re, this->defs);
  }

  // let
  //   _ = writeTo(FD, <code>);
  //   _ = writeTo(FD, <args>)
  // in
  //   readFrom(FD) :: rty
  ExprPtr makeInvocation(const proc* p, const ExprPtr& pe, const std::string& fname, const MonoTypePtr& ftyv, const Exprs& args) const {
    const Func* fty = is<Func>(ftyv);
    if (!fty) { throw std::runtime_error("Internal error, process RPC call expected function type, not: " + show(ftyv)); }

    MkRecord::FieldDefs argtupv;
    for (size_t i = 0; i < args.size(); ++i) {
      argtupv.push_back(MkRecord::FieldDef(".f" + str::from(i), args[i]));
    }
    const auto& la = pe->la();

    // avoid partial remote invocations by completely evaluating arguments before sending remote procedure calls
    std::string arglVN = freshName();
    ExprPtr argl = mkrecord(argtupv, la);

    return
      let(arglVN,      argl,
      let(freshName(), blockWrite(p, constant(invocationID(*p, fname, ftyv), la)),
      let(freshName(), blockWrite(p, var(arglVN, argl->type(), la)),
      blockRead(p, fty->result()), la), la), la);
  }

  ExprPtr with(const App* v) const {
    if (const Proj* f = is<Proj>(stripAssumpHead(v->fn()))) {
      if (const proc* p = isProcRef(f)) {
        return makeInvocation(p, switchOf(f->record(), *this), f->field(), f->type()->monoType(), switchOf(v->args(), *this));
      }
    }
    return wrapWithTy(v->type(), new App(switchOf(v->fn(), *this), switchOf(v->args(), *this), v->la()));
  }

  ExprPtr with(const Proj* v) const {
    // generate a function for this reference to an external function
    if (const proc* p = isProcRef(v)) {
      const Func* fty = is<Func>(v->type()->monoType());

      MonoTypes argtys = fty->parameters();
      str::seq  vns;
      Exprs     args;

      for (size_t i = 0; i < argtys.size(); ++i) {
        std::string vname = freshName();

        vns.push_back(vname);
        args.push_back(var(vname, qualtype(argtys[i]), v->la()));
      }

      return wrapWithTy(v->type(), new Fn(vns, makeInvocation(p, switchOf(v->record(), *this), v->field(), v->type()->monoType(), args), v->la()));
    } else {
      return wrapWithTy(v->type(), new Proj(switchOf(v->record(), *this), v->field(), v->la()));
    }
  }
};

ExprPtr ProcManager::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  return switchOf(e, ProcManUnqualify(this, tenv, cst, ds));
}

std::string ProcManager::name() const { return "process manager"; }

const TLong* pidTy(const MonoTypePtr& mty) {
  if (const TApp* aty = is<TApp>(mty)) {
    if (const Prim* tf = is<Prim>(aty->fn())) {
      if (tf->name() == "process") {
        if (aty->args().size() == 1) {
          return is<TLong>(aty->args()[0]);
        }
      }
    }
  }
  return 0;
}

MonoTypePtr mkPidTy(long pid) {
  return tapp(primty("process"), list(tlong(pid)));
}

}

