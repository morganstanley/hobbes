
#include <hobbes/lang/macroexpand.H>
#include <hobbes/lang/tylift.H>
#include <set>

namespace hobbes {

// walk the expression tree, find all macro references, and expand them
//   (because this should be called after type inference, we need to preserve explicit type annotations)
struct macroExpandF : public switchExprC<ExprPtr> {
  std::set<std::string> boundNames;

  macroExpandF() = default;
  explicit macroExpandF(const std::set<std::string>& boundNames) : boundNames(boundNames) { }

  macroExpandF under(const std::string& name) const {
    auto names = this->boundNames;
    names.insert(name);
    return macroExpandF(names);
  }

  macroExpandF under(const str::seq& names) const {
    auto result = this->boundNames;
    result.insert(names.begin(), names.end());
    return macroExpandF(result);
  }

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

  ExprPtr withConst(const Expr* v)  const override { return mk(v, v->clone()); }
  ExprPtr with(const Fn* v)         const override { auto body = under(v->varNames()); return mk(v, new Fn(v->varNames(), switchOf(v->body(), body), v->la())); }
  ExprPtr with(const MkArray* v)    const override { return mk(v, new MkArray(switchOf(v->values(), *this), v->la())); }
  ExprPtr with(const MkVariant* v)  const override { return mk(v, new MkVariant(v->label(), switchOf(v->value(), *this), v->la())); }
  ExprPtr with(const MkRecord* v)   const override { return mk(v, new MkRecord(switchOf(v->fields(), *this), v->la())); }
  ExprPtr with(const AIndex* v)     const override { return mk(v, new AIndex(switchOf(v->array(), *this), switchOf(v->index(), *this), v->la())); }
  ExprPtr with(const Proj* v)       const override { return mk(v, new Proj(switchOf(v->record(), *this), v->field(), v->la())); }
  ExprPtr with(const Assump* v)     const override { return mk(v, new Assump(switchOf(v->expr(), *this), v->ty(), v->la())); }

  ExprPtr with(const Case* v) const override {
    ExprPtr de = v->defaultExpr();
    if (de.get() != nullptr) { de = switchOf(de, *this); }
    Case::Bindings bs;
    for (const auto& b : v->bindings()) {
      auto branch = under(b.vname);
      bs.push_back(Case::Binding(b.selector, b.vname, switchOf(b.exp, branch)));
    }
    return mk(v, new Case(switchOf(v->variant(), *this), bs, de, v->la()));
  }

  ExprPtr with(const Switch* v) const override {
    ExprPtr de = v->defaultExpr();
    if (de) { de = switchOf(de, *this); }
    return mk(v, new Switch(switchOf(v->expr(), *this), switchOf(v->bindings(), *this), de, v->la()));
  }

  ExprPtr with(const Var* v) const override {
    if (this->boundNames.find(v->value()) != this->boundNames.end()) {
      return mk(v, v->clone());
    } else if (v->value() == "and") {
      return macroEtaLiftAnd(v->la());
    } else if (v->value() == "or") {
      return macroEtaLiftOr(v->la());
    } else {
      return mk(v, v->clone());
    }
  }

  ExprPtr with(const Let* v) const override {
    auto body = under(v->var());
    return mk(v, new Let(v->var(), switchOf(v->varExpr(), *this), switchOf(v->bodyExpr(), body), v->la()));
  }

  ExprPtr with(const LetRec* v) const override {
    str::seq names;
    for (const auto& b : v->bindings()) { names.push_back(b.first); }
    auto scope = under(names);
    LetRec::Bindings bs;
    for (const auto& b : v->bindings()) {
      bs.push_back(LetRec::Binding(b.first, switchOf(b.second, scope)));
    }
    return mk(v, new LetRec(bs, switchOf(v->bodyExpr(), scope), v->la()));
  }

  ExprPtr with(const App* v) const override {
    Var* fn = is<Var>(v->fn());
    if ((fn != nullptr) && this->boundNames.find(fn->value()) == this->boundNames.end() && fn->value() == "and") {
      return macroExpandAnd(switchOf(v->args(), *this), v->la());
    } else if ((fn != nullptr) && this->boundNames.find(fn->value()) == this->boundNames.end() && fn->value() == "or") {
      return macroExpandOr(switchOf(v->args(), *this), v->la());
    } else {
      return mk(v, new App(switchOf(v->fn(), *this), switchOf(v->args(), *this), v->la()));
    }
  }

  ExprPtr with(const Assign* v) const override {
    return mk(v, new Assign(switchOf(v->left(), *this), switchOf(v->right(), *this), v->la()));
  }

  ExprPtr with(const Pack* v) const override {
    return mk(v, new Pack(switchOf(v->expr(), *this), v->la()));
  }

  ExprPtr with(const Unpack* v) const override {
    auto body = under(v->varName());
    return mk(v, new Unpack(v->varName(), switchOf(v->package(), *this), switchOf(v->expr(), body), v->la()));
  }

  // x and y = if x then y else false
  static ExprPtr macroExpandAnd(const Exprs& es, const LexicalAnnotation& la) {
    if (es.size() != 2) {
      throw annotated_error(la, "and expects exactly two arguments");
    }
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
    if (es.size() != 2) {
      throw annotated_error(la, "or expects exactly two arguments");
    }
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

