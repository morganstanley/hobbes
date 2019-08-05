
#include <hobbes/db/bindings.H>
#include <hobbes/lang/tyunqualify.H>
#include <hobbes/eval/probes.H>
#include <hobbes/eval/cc.H>
#include <hobbes/eval/jitcc.H>

#include <atomic>

namespace hobbes {

static std::atomic<uint32_t> probeIdCtr{0};
static uint64_t freshProbeId() noexcept {
  return InjectedProbeMetadata::typeTag | static_cast<uint64_t>(probeIdCtr++);
}

struct injectProbeF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& /*rty*/, const Exprs& es) {
    // Requires us to be able to fully evaluate the argument to the compile-time string
    MkArray* probeNameArr = is<MkArray>(stripAssumpHead(es[0]));
    if (!probeNameArr) {
      throw annotated_error(*es[0], "injectProbe intrinsic expected compile-time [char] as argument" + show(tys[0]));
    }
    if (probeNameArr->values().size() == 0) {
      throw annotated_error(*es[0], "injectProbe intrinsic expected compile-time [char] as argument" + show(tys[0]));
    }
    std::string probeName = "";
    for (auto v : probeNameArr->values()) {
      Char* casted = is<Char>(v);
      if (!casted) {
        throw annotated_error(*es[0], "injectProbe intrinsic expected compile-time [char] as argument" + show(tys[0]));
      }
      probeName += casted->value();
    }

    const uint64_t probeId = freshProbeId();
    constexpr uint32_t caveSize = 5; // Size in bytes of the nop to insert at patchpoint. Xpedite expects 5

    // we need to register the probe (id and name) with the compiler such that the stackmap
    // can be walked and probe location extracted & dumped to disk for xpedite.
    c->addProbeMapping(
      InjectedProbeMetadata{
        probeId,
        probeName,
        es[0]->la().filename(),
        es[0]->la().p0.first,  // lineno
        "N/A",  // function name
        reinterpret_cast<void*>(0), // callsite - to be filled later
        reinterpret_cast<void*>(0), // returnsite - to be filled later
      }
    );

    // patchpoints are utterly broken in patchpoint followed by call scenarios under x86_64
    // instead we use stackmap followed by a manual inline asm 5 byte nop
    llvm::InlineAsm* multibyteNop = llvm::InlineAsm::get(
      llvm::FunctionType::get(c->builder()->getVoidTy(), false /* isVarArg */),
      "nopl 0(%eax, %eax, 1)", // 5 byte nop
      "", // clobbers
      false, // HasSideEffect
      false, // IsAlignStack
      llvm::InlineAsm::AD_ATT);

    llvm::SmallVector<llvm::Value*, 2> args;
    args.push_back(c->builder()->getInt64(probeId));
    args.push_back(c->builder()->getInt32(caveSize));

    llvm::CallInst* stackMapCall = c->builder()->CreateCall(
      llvm::Intrinsic::getDeclaration(c->module(), llvm::Intrinsic::experimental_stackmap),
      llvm::ArrayRef<llvm::Value*>(args)
    );
    c->builder()->CreateCall(multibyteNop);

    return stackMapCall;
  }

  PolyTypePtr type(typedb&) const {
    return polytype(intrinsicType());
  }

private:
  // injectProbe :: [char] -> ()
  static MonoTypePtr intrinsicType() {
    return functy(list(arrayty(primty("char"))), primty("unit"));
  }
};

class ProbeP : public Unqualifier {
  struct ConstraintArgs {
    std::string probeName;
  };

  static ConstraintArgs extractConstraintArgs(const ConstraintPtr& c) {
    ConstraintArgs args;
    args.probeName = is<TString>(c->arguments()[0])->value();
    return args;
  }

public:
  ProbeP() = default;
  ~ProbeP() = default;
  ProbeP(const ProbeP&) = default;
  ProbeP(ProbeP&&) = default;
  ProbeP& operator=(const ProbeP&) = default;
  ProbeP& operator=(ProbeP&&) = default;

  static std::string constraintName() {
    return "Probe";
  }

  bool refine(const TEnvPtr&, const ConstraintPtr&, MonoTypeUnifier*, Definitions*) {
    return false;
  }

  bool satisfied(const TEnvPtr&, const ConstraintPtr& c, Definitions*) const {
    TString* typedName = nullptr;
    if (c->name() == constraintName() && c->arguments().size() == 1 && !hasFreeVariables(c->arguments()[0])) {
      typedName = is<TString>(c->arguments()[0]);
      if (typedName != nullptr && typedName->value() != "") {
        return true;
      }
    }

    return false;
  }

  bool satisfiable(const TEnvPtr&, const ConstraintPtr&, Definitions*) const {
    return true;
  }

  void explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
  }

  struct insertProbeF : public switchExprTyFn {
    // Do a transform of:
    // e :: (Probe "probeName") => T
    //    =>
    // (do { injectProbe("probeName.start"); let x = e; injectProbe("probeName.end"); return e; }) :: T
    //    => desugaring to
    // (let freshSymbol1 = injectProbe("probeName.start") in
    //   (let freshSymbol2 = e in
    //     (let freshSymbol3 = injectProbe("probeName.end") in
    //       freshSymbol2))) :: T
    // where probeName.start and probeName.end are two named 5 byte nop patchpoints
    // with locational information being emitted to the .llvm.stackmap data section
    const TEnvPtr& tenv;
    const ConstraintPtr& constraint;
    Definitions* ds;
    const std::string& probeName;

    insertProbeF(const TEnvPtr& tenv, const ConstraintPtr& constraint, Definitions* ds, const std::string& probeName)
      : tenv(tenv), constraint(constraint), ds(ds), probeName(probeName) {}
    ~insertProbeF() = default;
    insertProbeF(const insertProbeF&) = default;
    insertProbeF(insertProbeF&&) = default;
    insertProbeF& operator=(const insertProbeF&) = default;
    insertProbeF& operator=(insertProbeF&&) = default;

    ExprPtr wrapWithProbes(const ExprPtr e) const {
      const std::string expressionName = ".probe.expr" + freshName();
      const std::string probeStartName = ".probe.start" + freshName();
      const std::string probeEndName = ".probe.end" + freshName();
      const std::string injectProbeFuncName = "injectProbe";

      const MonoTypePtr charT = primty("char");
      const MonoTypePtr arrayCharT = arrayty(charT);
      const MonoTypePtr unitT = primty("unit");
      const MonoTypePtr injectProbeFuncType = functy(list(arrayCharT), unitT);

      ExprPtr probeStartArg = ExprPtr(mkarray(probeName + ".start", e->la()));
      ExprPtr probeEndArg = ExprPtr(mkarray(probeName + ".end", e->la()));
      ExprPtr injectProbeExpr = var(injectProbeFuncName,
        qualtype(injectProbeFuncType),
        e->la()
      );
      injectProbeExpr->type(qualtype(injectProbeFuncType));

      ExprPtr injectProbeStartCall = fncall(
        injectProbeExpr,
        list(probeStartArg),
        e->la()
      );
      injectProbeStartCall->type(qualtype(unitT));

      ExprPtr injectProbeEndCall = fncall(
        injectProbeExpr,
        list(probeEndArg),
        e->la()
      );
      injectProbeEndCall->type(qualtype(unitT));

      ExprPtr finalResultReference = var(expressionName, e->la());
      finalResultReference->type(e->type());

      ExprPtr injectProbeEndBinding = let(probeEndName,
        injectProbeEndCall,
        finalResultReference,
        e->la()
      );
      injectProbeEndBinding->type(finalResultReference->type());

      ExprPtr letBindingsWrapper = let(probeStartName,
        injectProbeStartCall,
        let(expressionName,
          e,
          injectProbeEndBinding,
          e->la()
        ),
        e->la()
      );
      letBindingsWrapper->type(e->type());

      return letBindingsWrapper;
    }

    QualTypePtr withTy(const QualTypePtr& qt) const override {
      return removeConstraint(this->constraint, qt);
    }

    ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const override {
      ExprPtr result(e);
      result->type(withTy(qty));
      return result;
    }

    ExprPtr with(const Assump* v) const override {
      if (hasConstraint(this->constraint, v->type()) && !hasConstraint(this->constraint, v->expr()->type())) {
        return wrapWithTy(v->type(), new Assump(wrapWithProbes(switchOf(v->expr(), *this)), withTy(v->ty()), v->la()));
      } else {
        return wrapWithTy(v->type(), new Assump(switchOf(v->expr(), *this), withTy(v->ty()), v->la()));
      }
    }
  };

  ExprPtr unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
    auto args = extractConstraintArgs(cst);

    if (hasConstraint(cst, e->type())) {
      return switchOf(e, insertProbeF(tenv, cst, ds, args.probeName));
    } else {
      return e;
    }
  }

  PolyTypePtr lookup(const std::string& vn) const {
    if (vn == constraintName()) {
      return polytype(2, qualtype(list(ConstraintPtr(new Constraint(constraintName(), list(tgen(0))))), tgen(1)));
    } else {
      return PolyTypePtr{};
    }
  }

  SymSet bindings() const {
    SymSet r;
    r.insert(constraintName());
    return r;
  }

  FunDeps dependencies(const ConstraintPtr&) const {
    return FunDeps{};
  }
};

// load definitions for working with code caves for profiler instrumentation into compiler context
void initProbeDefs(cc& c) {
  c.typeEnv()->bind("Probe", UnqualifierPtr(new ProbeP()));
  c.bindLLFunc("injectProbe", new injectProbeF());
}

}

