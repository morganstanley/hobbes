
#include <hobbes/lang/module.H>
#include <hobbes/util/array.H>
#include <sstream>

namespace hobbes {

/* modules */
Module::Module(const std::string& mname, const ModuleDefs& defs) : mname(mname), defs(defs) { }
const std::string& Module::name()        const { return this->mname; }
const ModuleDefs&  Module::definitions() const { return this->defs; }

void Module::show(std::ostream& out) const {
  out << "module " << this->mname << " where" << std::endl;
  for (ModuleDefs::const_iterator md = this->defs.begin(); md != this->defs.end(); ++md) {
    (*md)->show(out);
    out << std::endl;
  }
}

/* module defs */
ModuleDef::ModuleDef(int cid, const LexicalAnnotation& la) : LexicallyAnnotated(la), cid(cid) { }
int ModuleDef::case_id() const { return this->cid; }
ModuleDef::~ModuleDef() { }

// module imports
MImport::MImport(const std::string& p, const std::string& n, const LexicalAnnotation& la) : Base(la), p(p), n(n) { }
const std::string& MImport::path() const { return this->p; }
const std::string& MImport::name() const { return this->n; }
void MImport::show(std::ostream& out) const { out << "import " << this->n; }

// type definitions
MTypeDef::MTypeDef(Visibility v, const std::string& tname, const str::seq& targs, const QualTypePtr& t, const LexicalAnnotation& la) : Base(la), v(v), tname(tname), targs(targs), t(t) { }

MTypeDef::Visibility MTypeDef::visibility() const { return this->v; }
const std::string&   MTypeDef::name()       const { return this->tname; }
const str::seq&      MTypeDef::arguments()  const { return this->targs; }
const QualTypePtr&   MTypeDef::type()       const { return this->t;     }

void MTypeDef::show(std::ostream& out) const {
  if (this->v == Transparent) {
    out << "type " << this->tname << (this->targs.size() > 0 ? " " : "") << str::cdelim(this->targs, " ") << " = " << hobbes::show(this->t);
  } else {
    out << "data " << this->tname << (this->targs.size() > 0 ? " " : "") << str::cdelim(this->targs, " ") << " = " << hobbes::show(this->t);
  }
}

// variable type bindings
MVarTypeDef::MVarTypeDef(const std::string& vname, const QualTypePtr& qty, const LexicalAnnotation& la) : Base(la), vname(vname), qty(qty) { }
const std::string& MVarTypeDef::varName() const { return this->vname; }
const QualTypePtr& MVarTypeDef::varType() const { return this->qty; }

void MVarTypeDef::show(std::ostream& out) const {
  out << this->vname << " :: " << hobbes::show(this->qty);
}

// variable expression bindings
str::seq nameBlanks(const str::seq& xs) {
  str::seq r;
  for (auto x : xs) {
    if (x == "_") {
      r.push_back(freshName());
    } else {
      r.push_back(x);
    }
  }
  return r;
}

MVarDef::MVarDef(const str::seq& vargl, const ExprPtr& e, const LexicalAnnotation& la) : Base(la), vargl(nameBlanks(vargl)), expr(e) { }
const str::seq& MVarDef::varWithArgs() const { return this->vargl; }
const ExprPtr&  MVarDef::varExpr() const { return this->expr; }

void MVarDef::show(std::ostream& out) const {
  out << str::cdelim(this->vargl, " ") << " = " << hobbes::show(this->expr);
}

// class declarations
ClassDef::ClassDef(const Constraints& cs, const std::string& cname, const str::seq& tvars, const CFunDepDefs& fdeps, const MVarTypeDefs& mvtydefs, const LexicalAnnotation& la) :
  Base(la), cs(cs), cname(cname), tvars(tvars), fdeps(fdeps), mvtydefs(mvtydefs)
{}

const Constraints&  ClassDef::constraints() const { return this->cs; }
const std::string&  ClassDef::name() const { return this->cname; }
const str::seq&     ClassDef::vars() const { return this->tvars; }
const CFunDepDefs   ClassDef::fundeps() const { return this->fdeps; }
const MVarTypeDefs& ClassDef::members() const { return this->mvtydefs; }

void ClassDef::show(std::ostream& out) const {
  out << "class " << this->cname << " " << str::cdelim(this->tvars, " ");
  if (this->fdeps.size() > 0) {
    out << " | ";
    out << hobbes::show(this->fdeps);
  }
  out << " where" << std::endl;
  for (MVarTypeDefs::const_iterator mvtd = this->mvtydefs.begin(); mvtd != this->mvtydefs.end(); ++mvtd) {
    out << "  ";
    (*mvtd)->show(out);
    out << std::endl;
  }
}

// instance declarations
InstanceDef::InstanceDef(const Constraints& cs, const std::string& cname, const MonoTypes& targs, const MVarDefs& mdefs, const LexicalAnnotation& la) :
  Base(la), cs(cs), cname(cname), targs(targs), mdefs(mdefs)
{}

const Constraints& InstanceDef::constraints() const { return this->cs; }
const std::string& InstanceDef::className()   const { return this->cname; }
const MonoTypes&   InstanceDef::args()        const { return this->targs; }
const MVarDefs&    InstanceDef::members()     const { return this->mdefs; }

void InstanceDef::show(std::ostream& out) const {
  out << "instance (" << str::cdelim(hobbes::show(this->cs), ", ") << ") => " << this->cname << " " << str::cdelim(hobbes::show(this->targs), " ") << " where" << std::endl;
  for (MVarDefs::const_iterator mvd = this->mdefs.begin(); mvd != this->mdefs.end(); ++mvd) {
    out << "  ";
    (*mvd)->show(out);
    out << std::endl;
  }
}

/* utility methods */
std::string show(const Module& m) {
  std::ostringstream ss;
  m.show(ss);
  return ss.str();
}
std::string show(const Module* m)    { return show(*m); }
std::string show(const ModulePtr& m) { return show(*m); }

std::string show(const MTypeDef* td) {
  std::ostringstream ss;
  td->show(ss);
  return ss.str();
}

std::string show(const ClassDef& cd) {
  std::ostringstream ss;
  cd.show(ss);
  return ss.str();
}

std::string show(const ClassDef* cd) {
  return show(*cd);
}

std::string show(const InstanceDef& id) {
  std::ostringstream ss;
  id.show(ss);
  return ss.str();
}

std::string show(const InstanceDef* id) {
  return show(*id);
}

std::string show(const ModuleDefPtr& md) {
  std::ostringstream ss;
  md->show(ss);
  return ss.str();
}

std::string show(const CFunDepDef& fundep) {
  return str::cdelim(fundep.first, " ") + " -> " + str::cdelim(fundep.second, " ");
}

std::string show(const CFunDepDefs& fundeps) {
  if (fundeps.size() == 0) {
    return "";
  } else {
    std::ostringstream ss;
    ss << show(fundeps[0]);
    for (size_t i = 1; i < fundeps.size(); ++i) {
      ss << ", ";
      ss << show(fundeps[i]);
    }
    return ss.str();
  }
}

MVarDefs substitute(const MonoTypeSubst& s, const MVarDefs& vds) {
  MVarDefs result;
  for (MVarDefs::const_iterator vd = vds.begin(); vd != vds.end(); ++vd) {
    result.push_back(MVarDefPtr(new MVarDef((*vd)->varWithArgs(), substitute(s, (*vd)->varExpr()), (*vd)->la())));
  }
  return result;
}

// switchMDefTyFn
ModuleDefPtr switchMDefTyFn::with(const MImport*     x) const { return ModuleDefPtr(new MImport(x->path(), x->name(), x->la())); }
ModuleDefPtr switchMDefTyFn::with(const MTypeDef*    x) const { return ModuleDefPtr(new MTypeDef(x->visibility(), x->name(), x->arguments(), withTy(x->type()), x->la())); }
ModuleDefPtr switchMDefTyFn::with(const MVarTypeDef* x) const { return ModuleDefPtr(new MVarTypeDef(x->varName(), withTy(x->varType()), x->la())); }

struct appMTySwitchF : public switchExprTyFn {
  const switchMDefTyFn* f;
  appMTySwitchF(const switchMDefTyFn* f) : f(f) { }
  QualTypePtr withTy(const QualTypePtr& t) const { if (t) return this->f->withTy(t); else return t; }
};
ModuleDefPtr switchMDefTyFn::with(const MVarDef* x) const {
  return ModuleDefPtr(new MVarDef(x->varWithArgs(), switchOf(x->varExpr(), appMTySwitchF(this)), x->la()));
}

MonoTypes appMTyTys(const switchMDefTyFn* f, const MonoTypes& ts) {
  MonoTypes r;
  for (const auto& t : ts) {
    r.push_back(f->withTy(qualtype(t))->monoType());
  }
  return r;
}
Constraints appMTyCS(const switchMDefTyFn* f, const Constraints& cs) {
  Constraints r;
  for (const auto& c : cs) {
    r.push_back(ConstraintPtr(new Constraint(c->name(), appMTyTys(f, c->arguments()))));
  }
  return r;
}
MVarTypeDefs appMTyMVTDs(const switchMDefTyFn* f, const MVarTypeDefs& tds) {
  MVarTypeDefs r;
  for (const auto& td : tds) {
    r.push_back(MVarTypeDefPtr(new MVarTypeDef(td->varName(), f->withTy(td->varType()), td->la())));
  }
  return r;
}
ModuleDefPtr switchMDefTyFn::with(const ClassDef* x) const {
  return ModuleDefPtr(new ClassDef(appMTyCS(this, x->constraints()), x->name(), x->vars(), x->fundeps(), appMTyMVTDs(this, x->members()), x->la()));
}

MVarDefs appMTyMVDs(const switchMDefTyFn* f, const MVarDefs& vds) {
  MVarDefs r;
  for (const auto& vd : vds) {
    r.push_back(MVarDefPtr(new MVarDef(vd->varWithArgs(), switchOf(vd->varExpr(), appMTySwitchF(f)), vd->la())));
  }
  return r;
}
ModuleDefPtr switchMDefTyFn::with(const InstanceDef* x) const {
  return ModuleDefPtr(new InstanceDef(appMTyCS(this, x->constraints()), x->className(), appMTyTys(this, x->args()), appMTyMVDs(this, x->members()), x->la()));
}

}

