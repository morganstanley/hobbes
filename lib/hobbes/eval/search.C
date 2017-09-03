
#include <hobbes/eval/cc.H>
#include <hobbes/eval/search.H>

namespace hobbes {

// labels :: TEnv -> [string]
//  (find the universe of all "labels" [record field names, variant constructor names, ...])
struct findLabelsF : public switchType<unit> {
  std::set<std::string>* lbls;
  findLabelsF(std::set<std::string>* lbls) : lbls(lbls) { }

  unit with(const TString* v) const {
    this->lbls->insert(v->value());
    return unit();
  }

  unit with(const Variant* v) const {
    for (const auto& m : v->members()) {
      if (!m.selector.empty() && m.selector[0] != '.') {
        this->lbls->insert(m.selector);
      }
      switchOf(m.type, *this);
    }
    return unit();
  }

  unit with(const Record* v) const {
    for (const auto& m : v->members()) {
      if (!m.field.empty() && m.field[0] != '.') {
        this->lbls->insert(m.field);
      }
      switchOf(m.type, *this);
    }
    return unit();
  }

  unit with(const TAbs*       v) const { return switchOf(v->body(), *this); }
  unit with(const TApp*       v) const { switchOf(v->fn(), *this); switchOf(v->args(), *this); return unit(); }
  unit with(const FixedArray* v) const { switchOf(v->type(), *this); return switchOf(v->length(), *this); }
  unit with(const Array*      v) const { return switchOf(v->type(), *this); }
  unit with(const Func*       v) const { switchOf(v->argument(), *this); return switchOf(v->result(), *this); }
  unit with(const Exists*     v) const { return switchOf(v->absType(), *this); }
  unit with(const Recursive*  v) const { return switchOf(v->recType(), *this); }
  
  unit with(const Prim*       v) const { return unit(); }
  unit with(const OpaquePtr*  v) const { return unit(); }
  unit with(const TVar*       v) const { return unit(); }
  unit with(const TGen*       v) const { return unit(); }
  unit with(const TLong*      v) const { return unit(); }
  unit with(const TExpr*      v) const { return unit(); }
};
static void accumLabels(std::set<std::string>* lbls, const MonoTypePtr& t) {
  switchOf(t, findLabelsF(lbls));
}
static void accumLabels(std::set<std::string>* lbls, const TEnvPtr& tenv) {
  // gather labels across ground SLookup instances since these will be relevant
  // (it might be reasonable to gather labels across types in _all_ ground type class instances)
  try {
    auto uq = tenv->lookupUnqualifier("SLookup");
    if (const TClass* c = dynamic_cast<const TClass*>(uq.get())) {
      for (auto i : c->instances()) {
        if (i->types().size() > 2) {
          if (const TString* s = is<TString>(i->types()[1])) {
            lbls->insert(s->value());
          }
        }
      }
    }
  } catch (std::exception&) {
  }

  // gather labels across all bindings in the type environment
  for (const auto& vnt : tenv->typeEnvTable()) {
    if (!vnt.first.empty() && vnt.first[0] != '.') {
      lbls->insert(vnt.first);

      if (vnt.second->typeVariables() == 0 && vnt.second->qualtype()->constraints().size() == 0) {
        accumLabels(lbls, vnt.second->qualtype()->monoType());
      }
    }
  }
}

static std::set<std::string> labels(const TEnvPtr& tenv) {
  std::set<std::string> r;
  accumLabels(&r, tenv);
  return r;
}

// find all "properties" for a given type
void findProperties(cc& c, SearchEntries* es, const MonoTypePtr& t) {
  for (const auto& lbl : labels(c.typeEnv())) {
    try {
      MonoTypeUnifier u(c.typeEnv());
      MonoTypePtr sty = freshTypeVar();
      ConstraintPtr tcst(HasField::newConstraint(HasField::Read, t, TString::make(lbl), sty));

      // refine this constraint to a fixed point
      Definitions ds;
      while (refine(c.typeEnv(), tcst, &u, &ds)) {
        c.drainUnqualifyDefs(ds);
        ds.clear();
      }
      c.drainUnqualifyDefs(ds);

      // make sure that the output type exists and is realizable
      MonoTypePtr result = u.substitute(sty);

      if (isMonoSingular(result)) {
        SearchEntry e;
        e.la  = LexicalAnnotation::null();
        e.sym = "." + lbl;
        e.ty  = result;
        es->push_back(e);
      }
    } catch (std::exception&) {
    }
  }
  hobbes::compactMTypeMemory();
}


// find all possible search entries for a given type
SearchEntries findAll(cc& c, const MonoTypePtr& src) {
  SearchEntries result;
  findProperties(c, &result, src);
  return result;
}

// test whether an existing search entry matches a desired destination type
struct containsTypeF : public switchType<bool> {
  MonoTypePtr ty;
  containsTypeF(const MonoTypePtr& ty) : ty(ty) { }
  bool baseC(const MonoType& t) const { return t == *this->ty; }
  bool anyC(const MonoTypes& ts) const { for (const auto& t : ts) { if (switchOf(t, *this)) return true; } return false; }

  bool with(const Variant* v) const {
    if (baseC(*v)) return true;
    for (const auto& m : v->members()) {
      if (switchOf(m.type, *this)) {
        return true;
      }
    }
    return false;
  }

  bool with(const Record* v) const {
    if (baseC(*v)) return true;
    for (const auto& m : v->members()) {
      if (switchOf(m.type, *this)) {
        return true;
      }
    }
    return false;
  }

  bool with(const TAbs*       v) const { return baseC(*v) || switchOf(v->body(), *this); }
  bool with(const TApp*       v) const { return baseC(*v) || switchOf(v->fn(), *this) || anyC(v->args()); }
  bool with(const FixedArray* v) const { return baseC(*v) || switchOf(v->type(), *this) || switchOf(v->length(), *this); }
  bool with(const Array*      v) const { return baseC(*v) || switchOf(v->type(), *this); }
  bool with(const Func*       v) const { return baseC(*v) || switchOf(v->argument(), *this) || switchOf(v->result(), *this); }
  bool with(const Exists*     v) const { return baseC(*v) || switchOf(v->absType(), *this); }
  bool with(const Recursive*  v) const { return baseC(*v) || switchOf(v->recType(), *this); }
  
  bool with(const Prim*      v) const { return baseC(*v); }
  bool with(const OpaquePtr* v) const { return baseC(*v); }
  bool with(const TVar*      v) const { return baseC(*v); }
  bool with(const TGen*      v) const { return baseC(*v); }
  bool with(const TLong*     v) const { return baseC(*v); }
  bool with(const TString*   v) const { return baseC(*v); }
  bool with(const TExpr*     v) const { return baseC(*v); }
};
bool validSearchResult(const SearchEntry& e, const MonoTypePtr& dst) {
  return switchOf(e.ty, containsTypeF(dst));
}

// search for symbols going from one type to another
SearchEntries search(cc& c, SearchCache& sc, const MonoTypePtr& src, const MonoTypePtr& dst) {
  if (sc.univByType[src].size() == 0) {
    sc.univByType[src] = findAll(c, src);
  }

  if (is<TVar>(dst)) {
    // searching for everything, we already have it
    return sc.univByType[src];
  } else {
    // include just entries that match the desired dest type
    SearchEntries r;
    for (const auto& e : sc.univByType[src]) {
      if (validSearchResult(e, dst)) {
        r.push_back(e);
      }
    }
    return r;
  }
}

SearchEntries search(cc& c, SearchCache& sc, const ExprPtr& e, const MonoTypePtr& t) {
  try {
    return search(c, sc, requireMonotype(c.unsweetenExpression(e)->type()), t);
  } catch (std::exception&) {
    return SearchEntries();
  }
}

}

