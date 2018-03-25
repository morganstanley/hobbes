
#include <hobbes/lang/macroexpand.H>
#include <hobbes/lang/tylift.H>

namespace hobbes {

// walk the expression tree, find all macro references, and expand them
//   (because this should be called after type inference, we need to preserve explicit type annotations)
struct macroExpandF : public switchExprC<ExprPtr> {
  static ExprPtr mk(const QualTypePtr& qty, Expr* out) {
    out->type(qty);
    return ExprPtr(out);
  }
  static ExprPtr mk(const MonoTypePtr& mty, Expr* out) {
    return mk(qualtype(mty), out);
  }
  static ExprPtr mk(const Expr* src, Expr* out) {
    return mk(src->type(), out);
  }

  ExprPtr withConst(const Expr* v)  const { return mk(v, v->clone()); }
  ExprPtr with(const Fn* v)         const { return mk(v, new Fn(v->varNames(), switchOf(v->body(), *this), v->la())); }
  ExprPtr with(const MkArray* v)    const { return mk(v, new MkArray(switchOf(v->values(), *this), v->la())); }
  ExprPtr with(const MkVariant* v)  const { return mk(v, new MkVariant(v->label(), switchOf(v->value(), *this), v->la())); }
  ExprPtr with(const MkRecord* v)   const { return mk(v, new MkRecord(switchOf(v->fields(), *this), v->la())); }
  ExprPtr with(const AIndex* v)     const { return mk(v, new AIndex(switchOf(v->array(), *this), switchOf(v->index(), *this), v->la())); }
  ExprPtr with(const Proj* v)       const { return mk(v, new Proj(switchOf(v->record(), *this), v->field(), v->la())); }
  ExprPtr with(const Assump* v)     const { return mk(v, new Assump(switchOf(v->expr(), *this), v->ty(), v->la())); }

  ExprPtr with(const Case* v) const {
    ExprPtr de = v->defaultExpr();
    if (de.get()) { de = switchOf(de, *this); }
    return mk(v, new Case(switchOf(v->variant(), *this), switchOf(v->bindings(), *this), de, v->la()));
  }

  ExprPtr with(const Switch* v) const {
    ExprPtr de = v->defaultExpr();
    if (de) { de = switchOf(de, *this); }
    return mk(v, new Switch(switchOf(v->expr(), *this), switchOf(v->bindings(), *this), de, v->la()));
  }

  ExprPtr with(const Var* v) const {
    if (v->value() == "and") {
      return macroEtaLiftAnd(v->la());
    } else if (v->value() == "or") {
      return macroEtaLiftOr(v->la());
    } else {
      return mk(v, v->clone());
    }
  }

  ExprPtr with(const Let* v) const {
    return mk(v, new Let(v->var(), switchOf(v->varExpr(), *this), switchOf(v->bodyExpr(), *this), v->la()));
  }

  ExprPtr with(const LetRec* v) const {
    LetRec::Bindings bs;
    for (const auto& b : v->bindings()) {
      bs.push_back(LetRec::Binding(b.first, switchOf(b.second, *this)));
    }
    return mk(v, new LetRec(bs, switchOf(v->bodyExpr(), *this), v->la()));
  }

  ExprPtr with(const App* v) const {
    Var* fn = is<Var>(v->fn());
    if (fn && fn->value() == "and") {
      return macroExpandAnd(switchOf(v->args(), *this), v->la());
    } else if (fn && fn->value() == "or") {
      return macroExpandOr(switchOf(v->args(), *this), v->la());
    } else {
      return mk(v, new App(switchOf(v->fn(), *this), switchOf(v->args(), *this), v->la()));
    }
  }

  ExprPtr with(const Assign* v) const {
    return mk(v, new Assign(switchOf(v->left(), *this), switchOf(v->right(), *this), v->la()));
  }

  ExprPtr with(const Pack* v) const {
    return mk(v, new Pack(switchOf(v->expr(), *this), v->la()));
  }

  ExprPtr with(const Unpack* v) const {
    return mk(v, new Unpack(v->varName(), switchOf(v->package(), *this), switchOf(v->expr(), *this), v->la()));
  }

  // x and y = if x then y else false
  static ExprPtr macroExpandAnd(const Exprs& es, const LexicalAnnotation& la) {
    MonoTypePtr tbool = MonoTypePtr(Prim::make("bool"));
    QualTypePtr tbfn  = qualtype(Func::make(tuplety(list(tbool, tbool, tbool)), tbool));
    return mk(tbool, new App(mk(tbfn, new Var("if", la)), list(es[0], es[1], mk(tbool, new Bool(false, la))), la));
  }

  // and => \(x,y).x and y
  static ExprPtr macroEtaLiftAnd(const LexicalAnnotation& la) {
    MonoTypePtr tbool = MonoTypePtr(Prim::make("bool"));
    QualTypePtr tafn  = qualtype(Func::make(tuplety(list(tbool, tbool)), tbool));
    return mk(tafn, new Fn(list<std::string>("x", "y"), macroExpandAnd(list(mk(tbool, new Var("x", la)), mk(tbool, new Var("y", la))), la), la));
  }
  
  // x or y = if x then true else y
  static ExprPtr macroExpandOr(const Exprs& es, const LexicalAnnotation& la) {
    MonoTypePtr tbool = MonoTypePtr(Prim::make("bool"));
    QualTypePtr tbfn  = qualtype(Func::make(tuplety(list(tbool, tbool, tbool)), tbool));
    return mk(tbool, new App(mk(tbfn, new Var("if", la)), list(es[0], mk(tbool, new Bool(true, la)), es[1]), la));
  }

  // or => \(x,y).x or y
  static ExprPtr macroEtaLiftOr(const LexicalAnnotation& la) {
    MonoTypePtr tbool = MonoTypePtr(Prim::make("bool"));
    QualTypePtr tafn  = qualtype(Func::make(tuplety(list(tbool, tbool)), tbool));
    return mk(tafn, new Fn(list<std::string>("x", "y"), macroExpandOr(list(mk(tbool, new Var("x", la)), mk(tbool, new Var("y", la))), la), la));
  }
};

ExprPtr macroExpand(const ExprPtr& e) {
  return switchOf(e, macroExpandF());
}

void initMacroEnvironment(const TEnvPtr& tenv) {
  tenv->bind("and", prim<bool(bool,bool)>());
  tenv->bind("or",  prim<bool(bool,bool)>());
}

}

