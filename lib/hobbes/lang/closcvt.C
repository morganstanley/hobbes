
#include <hobbes/lang/type.H>
#include <hobbes/lang/expr.H>
#include <hobbes/util/array.H>

namespace hobbes {

ExprPtr makeTuple(const Exprs& exprs, const LexicalAnnotation& la) {
  MkRecord::FieldDefs fds;
  for (size_t i = 0; i < exprs.size(); ++i) {
    fds.push_back(MkRecord::FieldDef(".f" + str::from(i), exprs[i]));
  }
  return ExprPtr(new MkRecord(fds, la));
}

QualTypePtr concreteClosureMethodType(const MonoTypePtr& envty, const MonoTypes& fargtys, const MonoTypePtr& rty) {
  return qualtype(MonoTypePtr(Func::make(tuplety(cons(envty, fargtys)), rty)));
}

QualTypePtr abstractClosureType(const Fn* f, const MonoTypes& fargtys, const MonoTypePtr& rty) {
  MonoTypes fargs = cons(MonoTypePtr(TVar::make("E")), fargtys);

  return qualtype(MonoTypePtr(Exists::make("E", tuplety(list(MonoTypePtr(Func::make(tuplety(fargs), rty)), MonoTypePtr(TVar::make("E")))))));
}

ExprPtr closLookup(const std::string& envVarName, const VarSet& vs, const MonoTypePtr& envty, const ExprPtr& fbody, const LexicalAnnotation& la) {
  ExprPtr result = fbody;
  int i = 0;
  for (const auto& v : vs) {
    result = ExprPtr(new Let(v, ExprPtr(new Proj(assume(var(envVarName, la), envty, la), ".f" + str::from(i++), la)), result, la));
  }
  return result;
}

ExprPtr closureMethod(const Fn* f, const VarSet& vs, const MonoTypePtr& envty, const MonoTypes& fargtys, const MonoTypePtr& rty, const LexicalAnnotation& la) {
  return assume(ExprPtr(new Fn(cons(std::string(".env"), f->varNames()), closLookup(".env", vs, envty, f->body(), la), la)), concreteClosureMethodType(envty, fargtys, rty), la);
}

ExprPtr closureEnv(const VarSet& vs, const MonoTypePtr& envty, const LexicalAnnotation& la) {
  Exprs es;
  for (const auto& v : vs) {
    es.push_back(var(v, la));
  }
  return assume(makeTuple(es, la), envty, la);
}

MonoTypePtr concreteClosureEnvType(unsigned int size) {
  return tuplety(freshTypeVars(size));
}

ExprPtr makeClosureOver(const Fn* f, const VarSet& vs) {
  if (vs.size() == 0) {
    return ExprPtr(f->clone());
  } else {
    MonoTypePtr              envty   = concreteClosureEnvType(vs.size());
    MonoTypes                fargtys = freshTypeVars(f->varNames().size());
    MonoTypePtr              fret    = freshTypeVar();
    const LexicalAnnotation& la      = f->la();

    return assume(ExprPtr(new Pack(makeTuple(list(closureMethod(f, vs, envty, fargtys, fret, la), closureEnv(vs, envty, la)), la), la)), abstractClosureType(f, fargtys, fret), la);
  }
}

typedef std::set<TEnvPtr> TEnvs;

static bool hasRootBinding(const std::string& vn, const TEnvPtr& tenv, const TEnvs roots) {
  if (!tenv) {
    return true;
  } else if (tenv->hasImmediateBinding(vn)) {
    return in(tenv, roots);
  } else {
    return hasRootBinding(vn, tenv->parentTypeEnv(), roots);
  }
}

struct ClosureConvertF : public switchExprTyFn {
  TEnvPtr tenv;
  TEnvs   roots;
  ClosureConvertF(const TEnvPtr& tenv, const TEnvs& roots) : tenv(tenv), roots(roots) { }
  
  VarSet excludeRootVars(const VarSet& vs) const {
    VarSet r;
    for (const auto& v : vs) {
      if (!hasRootBinding(v, this->tenv, this->roots)) {
        r.insert(v);
      }
    }
    return r;
  }

  static TEnvPtr fnFrame(const TEnvPtr& ptenv, const std::vector<std::string>& vns) {
    TEnvPtr r(new TEnv(ptenv));
    for (const auto& vn : vns) {
      r->bind(vn, tgen(0));
    }
    return r;
  }

  ExprPtr with(const Fn* v) const {
    ExprPtr nbody = switchOf(v->body(), ClosureConvertF(fnFrame(this->tenv, v->varNames()), this->roots));
    return makeClosureOver(new Fn(v->varNames(), nbody, v->la()), excludeRootVars(setDifference(freeVars(nbody), toSet(v->varNames()))));
  }

  ExprPtr with(const Let* v) const {
    return ExprPtr(new Let(v->var(), switchOf(v->varExpr(), *this), switchOf(v->bodyExpr(), ClosureConvertF(bindFrame(this->tenv, v->var(), tgen(0)), this->roots)), v->la()));
  }

  ExprPtr with(const Unpack* v) const {
    return ExprPtr(new Unpack(v->varName(), switchOf(v->package(), *this), switchOf(v->expr(), ClosureConvertF(bindFrame(this->tenv, v->varName(), tgen(0)), this->roots)), v->la()));
  }

  ExprPtr with(const Case* v) const {
    Case::Bindings cbs;
    for (const auto& b : v->bindings()) {
      cbs.push_back(Case::Binding(b.selector, b.vname, switchOf(b.exp, ClosureConvertF(bindFrame(this->tenv, b.vname, tgen(0)), this->roots))));
    }

    if (v->defaultExpr()) {
      return ExprPtr(new Case(switchOf(v->variant(), *this), cbs, switchOf(v->defaultExpr(), *this), v->la()));
    } else {
      return ExprPtr(new Case(switchOf(v->variant(), *this), cbs, v->la()));
    }
  }

  static TEnvPtr letRecFrame(const TEnvPtr& ptenv, const LetRec::Bindings& bs) {
    // assume that letrec bindings must be functions
    TEnvPtr r(new TEnv(ptenv));
    for (const auto& b : bs) {
      r->bind(b.first, tgen(0));
    }
    return r;
  }

  ExprPtr with(const LetRec* v) const {
    TEnvPtr lrtenv = letRecFrame(this->tenv, v->bindings());
    TEnvs   recRVS = this->roots;
    recRVS.insert(lrtenv);
    ClosureConvertF recF(lrtenv, recRVS);

    LetRec::Bindings bs;
    for (const auto& b : v->bindings()) {
      bs.push_back(LetRec::Binding(b.first, switchOf(b.second, recF)));
    }

    return ExprPtr(new LetRec(bs, switchOf(v->bodyExpr(), recF), v->la()));
  }
};

template <typename T>
  std::set<T> set() {
    return std::set<T>();
  }

template <typename T, typename ... Ts>
  std::set<T> set(const T& x, const Ts& ... xs) {
    std::set<T> r = {x,xs...};
    return r;
  }

ExprPtr closureConvert(const TEnvPtr& rootTEnv, const ExprPtr& e) {
  return switchOf(e, ClosureConvertF(rootTEnv, set(rootTEnv)));
}

ExprPtr closureConvert(const TEnvPtr& rootTEnv, const std::string& vn, const ExprPtr& e) {
  TEnvPtr rectenv = bindFrame(rootTEnv, vn, tgen(0));
  return switchOf(e, ClosureConvertF(rectenv, set(rootTEnv, rectenv)));
}

}

