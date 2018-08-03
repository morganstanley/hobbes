
#include <hobbes/lang/type.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/lang/typepreds.H>
#include <hobbes/lang/constraints.H>
#include <hobbes/util/array.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/str.H>
#include <hobbes/util/perf.H>
#include <sstream>
#include <stdexcept>
#include <string.h>
#include <unordered_map>
#include <atomic>

namespace hobbes {

template <typename T>
  std::string showT(const T& t) {
    std::ostringstream ss;
    t.show(ss);
    return ss.str();
  }

std::string show(const PolyType& e)   { return showT(*simplifyVarNames(e)); }
std::string show(const QualType& e)   { return showT(*simplifyVarNames(e)); }
std::string show(const Constraint& e) { return showT(*simplifyVarNames(e)); }
std::string show(const MonoType& e)   { return showT(*simplifyVarNames(e)); }

std::string show(const PolyType* e)      { return show(*e); }
std::string show(const PolyTypePtr& e)   { return show(*e); }
std::string show(const QualType* e)      { return show(*e); }
std::string show(const QualTypePtr& e)   { return show(*e); }
std::string show(const Constraint* e)    { return show(*e); }
std::string show(const ConstraintPtr& e) { return show(*e); }
std::string show(const MonoType* e)      { return show(*e); }
std::string show(const MonoTypePtr& e)   { return show(*e); }

template <typename T>
  std::string show(const std::set<T>& ts) {
    if (ts.size() == 0) {
      return "{}";
    } else {
      typedef typename std::set<T>::const_iterator TCIter;
      TCIter t = ts.begin();
      std::string r = "{" + str::from(*t);
      ++t;
      for (; t != ts.end(); ++t) {
        r += ", ";
        r += str::from(*t);
      }
      return r + "}";
    }
  }

std::string showNoSimpl(const PolyType& e)      { return showT(e); }
std::string showNoSimpl(const QualType& e)      { return showT(e); }
std::string showNoSimpl(const Constraint& e)    { return showT(e); }
std::string showNoSimpl(const MonoType& e)      { return showT(e); }

std::string showNoSimpl(const PolyType* e)      { return showNoSimpl(*e); }
std::string showNoSimpl(const PolyTypePtr& e)   { return showNoSimpl(*e); }
std::string showNoSimpl(const QualType* e)      { return showNoSimpl(*e); }
std::string showNoSimpl(const QualTypePtr& e)   { return showNoSimpl(*e); }
std::string showNoSimpl(const Constraint* e)    { return showNoSimpl(*e); }
std::string showNoSimpl(const ConstraintPtr& e) { return showNoSimpl(*e); }
std::string showNoSimpl(const MonoType* e)      { return showNoSimpl(*e); }
std::string showNoSimpl(const MonoTypePtr& e)   { return showNoSimpl(*e); }

str::seq showNoSimpl(const MonoTypes& ts) {
  str::seq r;
  for (const auto& t : ts) {
    r.push_back(showNoSimpl(t));
  }
  return r;
}

str::seq showNoSimpl(const Constraints& cs) {
  str::seq r;
  for (const auto& c : cs) {
    r.push_back(showNoSimpl(c));
  }
  return r;
}

// type environments
TEnv::TEnv(const TEnvPtr& parent) : parent(parent) {
}

TEnv::TEnv() : parent(), unquals(new UnqualifierSet()) {
}

bool TEnv::hasBinding(const std::string& vname) const {
  return hasImmediateBinding(vname) || (this->parent && this->parent->hasBinding(vname));
}

bool TEnv::hasImmediateBinding(const std::string& vname) const {
  return (this->ptenv.find(vname) != this->ptenv.end()) || (this->unquals && this->unquals->lookup(vname) != PolyTypePtr());
}

void TEnv::bind(const std::string& vname, const PolyTypePtr& pt) {
  if (hasImmediateBinding(vname)) {
    throw std::runtime_error("Variable already defined: " + vname);
  } else {
    this->ptenv[vname] = pt;
  }
}

void TEnv::bind(const std::string& vname, const QualTypePtr& t) {
  bind(vname, polytype(t));
}

void TEnv::bind(const std::string& vname, const MonoTypePtr& t) {
  bind(vname, polytype(t));
}

void TEnv::unbind(const std::string& vname) {
  PolyTypeEnv::iterator b = this->ptenv.find(vname);
  if (b != this->ptenv.end()) {
    this->ptenv.erase(b);
  }
}

PolyTypePtr TEnv::lookup(const std::string& vname) const {
  PolyTypeEnv::const_iterator t = this->ptenv.find(vname);
  if (t != this->ptenv.end()) {
    return t->second;
  } else if (this->parent) {
    return this->parent->lookup(vname);
  } else {
    PolyTypePtr pt = this->unquals->lookup(vname);
    if (pt != PolyTypePtr()) {
      return pt;
    } else {
      std::ostringstream ss;
      ss << "Undefined variable: '" << vname << "'";

      str::seq suggestions = str::closestMatches(vname, boundVariables(), 3);
      if (suggestions.size() == 1) {
        ss << " (did you mean '" << suggestions[0] << "'?)";
      } else if (suggestions.size() > 0) {
        ss << " (did you mean one of: '" << suggestions[0] << "'";
        for (size_t i = 1; i < suggestions.size(); ++i) {
          ss << ", '" << suggestions[i] << "'";
        }
        ss << ")";
      }

      throw std::runtime_error(ss.str());
    }
  }
}

void TEnv::bind(const std::string& predName, const UnqualifierPtr& uq) {
  if (this->parent) {
    this->parent->bind(predName, uq);
  } else {
    // add this unqualifier only if it doesn't introduce conflicting variable definitions
    for (auto vn : uq->bindings()) {
      if (hasBinding(vn)) {
        throw std::runtime_error("Variable already defined: " + vn);
      }
    }

    this->unquals->add(predName, uq);
  }
}

UnqualifierPtr TEnv::lookupUnqualifier(const std::string& predName) const {
  if (this->parent) {
    return this->parent->lookupUnqualifier(predName);
  } else {
    return this->unquals->findUnqualifier(predName);
  }
}

UnqualifierPtr TEnv::lookupUnqualifier(const ConstraintPtr& cst) const {
  return lookupUnqualifier(cst->name());
}

SymSet TEnv::boundVariables() const {
  SymSet r;
  if (this->unquals) {
    r = this->unquals->bindings();
  }
  for (const auto& ptb : this->ptenv) {
    r.insert(ptb.first);
  }
  if (this->parent) {
    SymSet pr = this->parent->boundVariables();
    r.insert(pr.begin(), pr.end());
  }
  return r;
}

TEnv::PolyTypeEnv TEnv::typeEnvTable() const {
  if (this->parent) {
    return this->parent->typeEnvTable();
  } else {
    PolyTypeEnv pte       = this->ptenv;
    SymSet      overloads = this->unquals->bindings();
    for (SymSet::const_iterator s = overloads.begin(); s != overloads.end(); ++s) {
      pte[*s] = this->unquals->lookup(*s);
    }
    return pte;
  }
}

const TEnv::Unqualifiers& TEnv::unqualifiers() const {
  if (this->parent) {
    return this->parent->unqualifiers();
  } else {
    return this->unquals->unqualifiers();
  }
}

const TEnvPtr& TEnv::parentTypeEnv() const {
  return this->parent;
}

TEnv* TEnv::root() {
  if (this->parent) {
    return this->parent->root();
  } else {
    return this;
  }
}

void TEnv::alias(const std::string& tn, const MonoTypePtr& t) {
  if (this->parent) {
    this->parent->alias(tn, t);
  } else {
    auto ta = this->typeAliases.find(tn);
    if (ta != this->typeAliases.end()) {
      if (!(*ta->second == *t)) {
        throw std::runtime_error("Can't redefine type alias " + tn + "=" + show(ta->second) + " to incompatible " + show(t));
      }
    } else {
      this->typeAliases[tn] = t;
    }
  }
}

MonoTypePtr TEnv::unalias(const std::string& tn) const {
  auto ta = this->typeAliases.find(tn);
  if (ta != this->typeAliases.end()) {
    return ta->second;
  } else {
    if (this->parent) {
      return this->parent->unalias(tn);
    } else {
      throw std::runtime_error("Internal error, not a type alias: " + tn);
    }
  }
}

bool TEnv::isOpaqueTypeAlias(const std::string& tn) const {
  return
    (this->typeAliases.find(tn) != this->typeAliases.end()) ||
    (this->parent && this->parent->isOpaqueTypeAlias(tn));
}

TEnvPtr fnFrame(const TEnvPtr& p, const str::seq& vs, const MonoTypes& atys) {
  TEnvPtr r(new TEnv(p));
  unsigned int n = std::min<unsigned int>(vs.size(), atys.size());
  for (unsigned int i = 0; i < n; ++i) {
    r->bind(vs[i], polytype(atys[i]));
  }
  return r;
}

TEnvPtr bindFrame(const TEnvPtr& p, const std::string& vn, const MonoTypePtr& vty) {
  TEnvPtr r(new TEnv(p));
  r->bind(vn, polytype(vty));
  return r;
}

TEnvPtr bindFrame(const TEnvPtr& p, const std::string& vn, const QualTypePtr& qty) {
  TEnvPtr r(new TEnv(p));
  r->bind(vn, polytype(qty));
  return r;
}

bool satisfied(const UnqualifierPtr& uq, const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) {
  switch (c->state) {
  case Constraint::Satisfied:
    return true;
  case Constraint::Unsatisfiable:
    return false;
  default:
    if (uq->satisfied(tenv, c, ds)) {
      c->state = Constraint::Satisfied;
      return true;
    } else {
      return false;
    }
  }
}

bool satisfied(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) {
  return satisfied(tenv->lookupUnqualifier(c), tenv, c, ds);
}

bool satisfied(const TEnvPtr& tenv, const Constraints& cs, Definitions* ds) {
  for (Constraints::const_iterator c = cs.begin(); c != cs.end(); ++c) {
    if (!satisfied(tenv, *c, ds)) {
      return false;
    }
  }
  return true;
}

bool satisfiable(const UnqualifierPtr& uq, const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) {
  switch (c->state) {
  case Constraint::Satisfied:
    return true;
  case Constraint::Unsatisfiable:
    return false;
  default:
    if (!uq->satisfiable(tenv, c, ds)) {
      c->state = Constraint::Unsatisfiable;
      return false;
    } else {
      return true;
    }
  }
}

bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& c, Definitions* ds) {
  return satisfiable(tenv->lookupUnqualifier(c), tenv, c, ds);
}

bool satisfiable(const TEnvPtr& tenv, const Constraints& cs, Definitions* ds) {
  for (Constraints::const_iterator c = cs.begin(); c != cs.end(); ++c) {
    if (!satisfiable(tenv, *c, ds)) {
      return false;
    }
  }
  return true;
}

////////
// polytypes
////////
PolyType::PolyType(size_t vs, const QualTypePtr& qt) : vs(vs), qt(qt) {
}

PolyType::PolyType(const QualTypePtr& qt) : vs(0), qt(qt) {
}

size_t PolyType::typeVariables() const {
  return this->vs;
}

QualTypePtr PolyType::instantiate() const {
  return hobbes::instantiate(this->vs, this->qt);
}

void PolyType::show(std::ostream& out) const {
  simplifyVarNames(instantiate())->show(out);
}

bool PolyType::operator==(const PolyType& rhs) const {
  return this->vs == rhs.vs && *this->qt == *rhs.qt;
}

const QualTypePtr& PolyType::qualtype() const {
  return this->qt;
}

////////
// qualified types
////////
QualType::QualType(const Constraints& cs, const MonoTypePtr& mt) : cs(cs), mt(mt) {
}

QualType::QualType(const MonoTypePtr& mt) : mt(mt) {
}

const Constraints& QualType::constraints() const { return this->cs; }
const MonoTypePtr& QualType::monoType() const { return this->mt; }
Constraints& QualType::constraints() { return this->cs; }
void QualType::monoType(const MonoTypePtr& nt) { this->mt = nt; }

void QualType::show(std::ostream& out) const {
  if (this->cs.size() == 0) {
    this->mt->show(out);
  } else if (this->cs.size() == 1) {
    this->cs[0]->show(out);
    out << " => ";
    this->mt->show(out);
  } else {
    out << "(";
    this->cs[0]->show(out);
    for (size_t i = 1; i < this->cs.size(); ++i) {
      out << ", ";
      this->cs[i]->show(out);
    }
    out << ") => ";
    this->mt->show(out);
  }
}

bool QualType::operator==(const QualType& rhs) const {
  return this->cs == rhs.cs && *this->mt == *rhs.mt;
}

////////
// the 'contained' constraint for type classes
////////
Constraint::Constraint(const std::string& cat, const MonoTypes& mts) :
  cat(cat), mts(mts), state(Unresolved)
{
}

std::string        Constraint::name()      const { return this->cat; }
const MonoTypes&   Constraint::arguments() const { return this->mts; }

ConstraintPtr Constraint::instantiate(const MonoTypes& ts) const {
  return ConstraintPtr(new Constraint(this->name(), hobbes::instantiate(ts, this->arguments())));
}

NameSet Constraint::tvarNames() const {
  NameSet result;
  hobbes::tvarNames(this->arguments(), &result);
  return result;
}

bool Constraint::hasFreeVariables() const {
  return hobbes::hasFreeVariables(this->arguments());
}

ConstraintPtr Constraint::substitute(const MonoTypeSubst& s) const {
  return ConstraintPtr(new Constraint(this->name(), hobbes::substitute(s, this->arguments())));
}

void Constraint::update(MonoTypeUnifier* u) {
  this->mts = u->substitute(this->mts);
}

void Constraint::show(std::ostream& out) const {
  if (this->cat == CtorVerifier::constraintName() && this->mts.size() >= 3) {
    if (const TString* lbl = is<TString>(this->mts[1])) {
      out << "|" << lbl->value() << ":";
    } else {
      out << "|";
      this->mts[1]->show(out);
      out << ":";
    }
    this->mts[2]->show(out);
    out << "|::";
    this->mts[0]->show(out);
  } else if (this->cat == FieldVerifier::constraintName() && this->mts.size() >= 4) {
    this->mts[1]->show(out);
    if (const TString* lbl = is<TString>(this->mts[2])) {
      out << "." << lbl->value();
    } else {
      out << "/";
      this->mts[2]->show(out);
    }
    out << "::";
    this->mts[3]->show(out);
  } else {
    out << this->cat;
    for (size_t i = 0; i < this->mts.size(); ++i) {
      out << " ";
      this->mts[i]->show(out);
    }
  }
}

bool Constraint::operator==(const Constraint& rhs) const {
  if (this->cat != rhs.cat || this->mts.size() != rhs.mts.size()) {
    return false;
  } else {
    for (size_t i = 0; i < this->mts.size(); ++i) {
      if (!(*this->mts[i] == *rhs.mts[i])) {
        return false;
      }
    }
    return true;
  }
}

////////
// monotypes
////////
MonoType::MonoType(int cid) : cid(cid), tgenCount(0), memorySize(-1) { }
int MonoType::case_id() const { return this->cid; }
MonoType::~MonoType() { }

bool MonoType::operator==(const MonoType& rhs) const {
  return this == &rhs;
}

// a convenient shorthand to determine if we've got a fileref
bool isFileRef(const MonoTypePtr& mt) {
  if (const TApp* ap = is<TApp>(mt)) {
    if (const Prim* fn = is<Prim>(ap->fn())) {
      return fn->name() == "fileref";
    }
  }
  return false;
}

// memory alignment for monotypes -- this may need to be looked at more closely and factored out of this module
unsigned int alignment(const MonoTypePtr& pty) {
  MonoTypePtr ty = repType(pty);

  if (is<Prim>(ty)) {
    if (isUnit(ty)) {
      return 1;
    } else {
      return sizeOf(ty);
    }
  } else if (const Variant* vf = is<Variant>(ty)) {
    if (vf->payloadOffset() == sizeof(int)) {
      return sizeof(int);
    } else {
      return sizeof(void*);
    }
  } else if (const FixedArray* farr = is<FixedArray>(ty)) {
    return alignment(farr->type());
  } else if (is<OpaquePtr>(ty) || is<Array>(ty) || is<Func>(ty)) {
    return sizeof(void*);
  } else if (const Record* rty = is<Record>(ty)) {
    size_t a = 1;
    for (auto f : rty->members()) {
      a = std::max<unsigned int>(a, alignment(f.type));
    }
    return a;
  } else if (is<Recursive>(ty)) {
    return sizeof(void*);
  } else if (isFileRef(ty)) {
    return sizeof(uint64_t);
  } else {
    return 1;
  }
}

////////////
// manage allocation of mono types so that type pointers are uniquely determined by constructor arguments
////////////
typedef unique_refc_map<const Prim,       std::string, MonoTypePtr>        PrimMem;
typedef unique_refc_map<const OpaquePtr,  std::string, unsigned int, bool> OpaquePtrMem;
typedef unique_refc_map<const TVar,       std::string>                     TVarMem;
typedef unique_refc_map<const TGen,       int>                             TGenMem;
typedef unique_refc_map<const TAbs,       str::seq,    MonoTypePtr>        TAbsMem;
typedef unique_refc_map<const TApp,       MonoTypePtr, MonoTypes>          TAppMem;
typedef unique_refc_map<const FixedArray, MonoTypePtr, MonoTypePtr>        FixedArrayMem;
typedef unique_refc_map<const Array,      MonoTypePtr>                     ArrayMem;
typedef unique_refc_map<const Variant,    Variant::Members>                VariantMem;
typedef unique_refc_map<const Record,     Record::Members>                 RecordMem;
typedef unique_refc_map<const Func,       MonoTypePtr, MonoTypePtr>        FuncMem;
typedef unique_refc_map<const Exists,     std::string, MonoTypePtr>        ExistsMem;
typedef unique_refc_map<const Recursive,  std::string, MonoTypePtr>        RecursiveMem;

typedef unique_refc_map<const TString,    std::string>                     TStringMem;
typedef unique_refc_map<const TLong,      long>                            TLongMem;
typedef unique_refc_map<const TExpr,      std::string>                     TExprMem;

typedef
  unique_refc_maps<
    PrimMem,
    OpaquePtrMem,
    TVarMem,
    TGenMem,
    TAbsMem,
    TAppMem,
    FixedArrayMem,
    ArrayMem,
    VariantMem,
    RecordMem,
    FuncMem,
    ExistsMem,
    RecursiveMem,

    TStringMem,
    TLongMem,
    TExprMem
  > MTypeCtorMaps;

MTypeCtorMaps* tctorMaps() {
  static MTypeCtorMaps* x = 0;
  if (x == 0) {
    x = new MTypeCtorMaps();
  }
  return x;
}

void compactMTypeMemory() {
  tctorMaps()->compact();
}

template <typename Class, typename T, typename ... Args>
  MonoTypePtr MonoType::makeType(const Args&... args) {
    return
      std::const_pointer_cast<MonoType>(std::static_pointer_cast<const MonoType>(
        tctorMaps()->at<Class>().get(
          [](const Args&... nargs) { return new T(nargs...); },
          args...
        )
      ));
  }

////////
// primitive monotypes
////////
MonoTypePtr Prim::make(const std::string& nm, const MonoTypePtr& t) {
  return makeType<PrimMem, Prim>(nm, t);
}

Prim::Prim(const std::string& nm, const MonoTypePtr& t) : nm(nm), t(t) {
  if (t) {
    this->freeTVars = t->freeTVars;
    this->tgenCount = t->tgenCount;
  }
}

void Prim::show(std::ostream& out) const { out << (this->nm == "unit" ? "()" : this->nm); }
const std::string& Prim::name() const { return this->nm; }
const MonoTypePtr& Prim::representation() const { return this->t; }

MonoTypePtr OpaquePtr::make(const std::string& nm, unsigned int sz, bool scontig) {
  return makeType<OpaquePtrMem, OpaquePtr>(nm, scontig ? sz : 0, scontig);
}

OpaquePtr::OpaquePtr(const std::string& nm, unsigned int sz, bool scontig) : nm(nm), sz(sz), scontig(scontig) {
}

const std::string& OpaquePtr::name() const { return this->nm; }
unsigned int OpaquePtr::size() const { return this->sz; }
bool OpaquePtr::storedContiguously() const { return this->scontig; }
void OpaquePtr::show(std::ostream& out) const { out << "<" << str::replace<char>(this->nm, "::", ".") << ">"; }

MonoTypePtr normIfOpaquePtr(const MonoTypePtr& ty) {
  if (const OpaquePtr* op = is<OpaquePtr>(ty)) {
    return OpaquePtr::make(op->name(), op->size(), false);
  } else {
    return ty;
  }
}

MonoTypePtr TVar::make(const std::string& nm) {
  return makeType<TVarMem, TVar>(nm);
}

TVar::TVar(const std::string& nm) : nm(nm) {
  this->freeTVars.insert(nm);
}

const std::string& TVar::name() const { return this->nm; }
void TVar::show(std::ostream& out) const { out << this->nm; }

MonoTypePtr TGen::make(int x) {
  return makeType<TGenMem, TGen>(x);
}

TGen::TGen(int x) : x(x) {
  this->tgenCount = x + 1;
}
int TGen::id() const { return this->x; }
void TGen::show(std::ostream& out) const { out << "#" << this->x; }

// a type-level abstraction
MonoTypePtr TAbs::make(const str::seq& targns, const MonoTypePtr& b) {
  return makeType<TAbsMem, TAbs>(targns, b);
}

TAbs::TAbs(const str::seq& targns, const MonoTypePtr& b) : targns(targns), b(b) {
}

void TAbs::show(std::ostream& out) const {
  out << "\\" << (this->targns.size() == 0 ? "()" : str::cdelim(this->targns, " ")) << "." << hobbes::show(this->b);
}

const str::seq&    TAbs::args() const { return this->targns; }
const MonoTypePtr& TAbs::body() const { return this->b; }

// TODO: come up with a nicer way to generically print types in constructor form
bool showFileRef(const MonoTypePtr& f, const MonoTypes& targs, std::ostream& out) {
  if (const Prim* fn = is<Prim>(f)) {
    if (fn->name() == "fileref") {
      targs[0]->show(out);
      out << "@";
      if (targs.size() == 1) {
        out << "?";
      } else if (targs.size() == 2) {
        targs[1]->show(out);
      }
      return true;
    }
  }
  return false;
}

MonoTypePtr TApp::make(const MonoTypePtr& f, const MonoTypes& targs) {
  return makeType<TAppMem, TApp>(f, targs);
}

TApp::TApp(const MonoTypePtr& f, const MonoTypes& targs) : f(f), targs(targs) {
  this->freeTVars = setUnion(f->freeTVars, tvarNames(targs));
  this->tgenCount = std::max<int>(f->tgenCount, tgenSize(targs));
}

void TApp::show(std::ostream& out) const {
  if (showFileRef(this->f, this->targs, out)) return;

  out << "(";
  this->f->show(out);
  if (this->targs.size() > 0) {
    for (size_t i = 0; i < this->targs.size(); ++i) {
      out << " ";
      this->targs[i]->show(out);
    }
  }
  out << ")";
}
const MonoTypePtr& TApp::fn() const { return this->f; }
const MonoTypes&   TApp::args() const { return this->targs; }

MonoTypePtr FixedArray::make(const MonoTypePtr& ty, const MonoTypePtr& len) {
  return makeType<FixedArrayMem, FixedArray>(ty, len);
}

FixedArray::FixedArray(const MonoTypePtr& ty, const MonoTypePtr& len) : ty(normIfOpaquePtr(ty)), len(len) {
  this->freeTVars = setUnion(ty->freeTVars, len->freeTVars);
  this->tgenCount = std::max<int>(ty->tgenCount, len->tgenCount);
}

void FixedArray::show(std::ostream& out) const { out << "[:"; this->ty->show(out); out << "|"; this->len->show(out); out << ":]"; }
const MonoTypePtr& FixedArray::type() const { return this->ty; }
const MonoTypePtr& FixedArray::length() const { return this->len; }

long FixedArray::requireLength() const {
  if (const TLong* sz = is<TLong>(this->len)) {
    return sz->value();
  } else {
    throw std::runtime_error("Cannot determine length of array from non-fixed length: " + hobbes::show(this->len));
  }
}

MonoTypePtr Array::make(const MonoTypePtr& t) {
  return makeType<ArrayMem, Array>(t);
}

Array::Array(const MonoTypePtr& ty) : ty(normIfOpaquePtr(ty)) {
  this->freeTVars = ty->freeTVars;
  this->tgenCount = ty->tgenCount;
}

void Array::show(std::ostream& out) const { out << "["; this->ty->show(out); out << "]"; }
const MonoTypePtr& Array::type() const { return this->ty; }

// variant types
MonoTypePtr Variant::make(const Members& ms) {
  return makeType<VariantMem, Variant>(ms);
}

static void resetCtorIDs(Variant::Members* ms) {
  size_t id = 0;
  for (Variant::Members::iterator m = ms->begin(); m != ms->end(); ++m) {
    m->id = id++;
  }
}

int findHiddenMember(int i, const std::string& lbl, const Variant::Members& ms) {
  std::string hlbl = ".p" + lbl;
  while (i >= 0 && ms[i].selector != hlbl) {
    --i;
  }
  return i;
}

Variant::Members consMember(const std::string& lbl, const MonoTypePtr& hty, const Variant::Members& tty) {
  // try to find a place to insert this type, otherwise insert it at the head
  int slot = findHiddenMember(tty.size() - 1, lbl, tty);

  Variant::Members r;
  if (slot < 0) {
    r.push_back(Variant::Member(lbl, hty, 0));
    r.insert(r.end(), tty.begin(), tty.end());
    resetCtorIDs(&r);
  } else {
    r = tty;
    r[slot] = Variant::Member(lbl, hty, r[slot].id);
  }
  return r;
}

static bool hiddenCtor(const Variant::Member& m) {
  return m.selector.size() > 2 && m.selector[0] == '.' && m.selector[1] == 'p';
}

static void normalizeSumFields(Variant::Members* ms) {
  size_t i = 0;
  for (auto& m : *ms) {
    if (!hiddenCtor(m)) {
      m.selector = ".f" + str::from(i++);
    }
  }
}

MonoTypePtr Variant::make(const MonoTypePtr& hty, const Members& tty) {
  Members ms = consMember(".f0", hty, tty);
  normalizeSumFields(&ms);
  return make(ms);
}

MonoTypePtr Variant::make(const std::string& lbl, const MonoTypePtr& hty, const Members& tty) {
  Members ms = consMember(lbl, hty, tty);
  return make(ms);
}

Variant::Member::Member(const std::string& selector, const MonoTypePtr& type, unsigned int id) : selector(selector), type(type), id(id) { }
Variant::Member::Member() : selector(""), type(), id(0) { }
bool Variant::Member::operator==(const Variant::Member& rhs) const { return this->selector == rhs.selector && *this->type == *rhs.type && this->id == rhs.id; }

bool Variant::Member::operator<(const Variant::Member& rhs) const {
  if (this->selector < rhs.selector) {
    return true;
  } else if (rhs.selector < this->selector) {
    return false;
  } else if (this->type < rhs.type) {
    return true;
  } else if (rhs.type < this->type) {
    return false;
  } else if (this->id < rhs.id) {
    return true;
  } else {
    return false;
  }
}

Variant::Variant(const Members& ms) : payloadSizeM(-1), ms(ms) {
  for (const Member& m : ms) {
    this->freeTVars = setUnion(this->freeTVars, m.type->freeTVars);
    this->tgenCount = std::max<int>(this->tgenCount, m.type->tgenCount);
  }
}

size_t nextVisibleMember(size_t i, const Variant::Members& ms) {
  while (i < ms.size() && hiddenCtor(ms[i])) {
    ++i;
  }
  return i;
}

void showFullVarPayloadSuffix(const MonoTypePtr& t, std::ostream& out) {
  if (const Prim* p = is<Prim>(t)) {
    if (p->name() == "unit") {
      return;
    }
  }
  out << ":";
  t->show(out);
}

void showFull(const Variant::Members& ms, std::ostream& out) {
  out << "|";
  size_t i = nextVisibleMember(0, ms);
  if (i < ms.size()) {
    out << ms[i].selector;
    showFullVarPayloadSuffix(ms[i].type, out);
    for (size_t j = i + 1; j < ms.size(); ++j) {
      out << ", ";
      out << ms[j].selector;
      showFullVarPayloadSuffix(ms[j].type, out);
    }
  }
  out << "|";
}

void showSum(const Variant::Members& ms, std::ostream& out) {
  out << "(";
  size_t i = nextVisibleMember(0, ms);
  if (i < ms.size()) {
    ms[i].type->show(out);
    for (size_t j = i + 1; j < ms.size(); ++j) {
      out << " + ";
      ms[j].type->show(out);
    }
  }
  out << ")";
}

bool looksLikeSum(const Variant::Members& ms) {
  for (size_t i = 0; i < ms.size(); ++i) {
    if (ms[i].selector.size() > 1 && ms[i].selector[0] == '.' && ms[i].selector[1] == 'f') {
      return true;
    }
  }
  return false;
}

bool Variant::isSum() const {
  return looksLikeSum(this->ms);
}

void Variant::show(std::ostream& out) const {
  if (looksLikeSum(this->ms)) {
    showSum(this->ms, out);
  } else {
    showFull(this->ms, out);
  }
}

const Variant::Members& Variant::members() const {
  return this->ms;
}

const MonoTypePtr& Variant::payload(const std::string& selector) const {
  if (const Member* m = mmember(selector)) {
    return m->type;
  } else {
    throw std::runtime_error("No selector named '" + selector + "' in the variant '" + hobbes::show(this) + "'.");
  }
}

unsigned int Variant::index(const std::string& selector) const {
  int i = 0;
  for (Members::const_iterator m = this->ms.begin(); m != this->ms.end(); ++m, ++i) {
    if (m->selector == selector) {
      return i;
    }
  }
  throw std::runtime_error("No selector named '" + selector + "' in the variant '" + hobbes::show(this) + "'.");
}
unsigned int Variant::id(const std::string& selector) const {
  for (Members::const_iterator m = this->ms.begin(); m != this->ms.end(); ++m) {
    if (m->selector == selector) {
      return m->id;
    }
  }
  throw std::runtime_error("No selector named '" + selector + "' in the variant '" + hobbes::show(this) + "'.");
}

const Variant::Member* Variant::mmember(const std::string& selector) const {
  for (Members::const_iterator m = this->ms.begin(); m != this->ms.end(); ++m) {
    if (m->selector == selector) {
      return &(*(m));
    }
  }
  return 0;
}

Variant::Members tailMembers(const Variant::Members& ms) {
  // copy the remaining fields
  Variant::Members tms = ms;
  unsigned int h = nextVisibleMember(0, tms);
  if (h >= tms.size()) {
    throw std::runtime_error("Variant has no tail members");
  }
  tms[h].selector = ".p" + tms[h].selector;
  return tms;
}

Variant::Member Variant::headMember() const {
  unsigned int h = nextVisibleMember(0, this->ms);
  if (h >= this->ms.size()) {
    throw std::runtime_error("Variant has no head member");
  }

  Variant::Member m = this->ms[h];
  m.type = normIfOpaquePtr(m.type);
  return m;
}

MonoTypePtr Variant::tailType() const {
  // bottom out at the void type (ie: X = X+0)
  Members tms = tailMembers(this->ms);
  if (nextVisibleMember(0, tms) >= tms.size()) {
    return Prim::make("void");
  } else {
    return Variant::make(tms);
  }
}

// possibly too simplistic to account for alignment rules
// we should try to unify this logic with record alignment-determination logic
unsigned int Variant::payloadOffset() const {
  unsigned int o = sizeof(int);
  for (auto m : this->ms) {
    o = std::max<unsigned int>(o, align<unsigned int>(sizeof(int), alignment(m.type)));
  }
  return o;
}

unsigned int Variant::payloadSize() const {
  if (this->payloadSizeM == static_cast<unsigned int>(-1)) {
    this->payloadSizeM = 0;
    for (auto m : this->ms) {
      this->payloadSizeM = std::max(this->payloadSizeM, sizeOf(m.type));
    }
  }
  return this->payloadSizeM;
}

unsigned int Variant::size() const {
  // align to strictest field
  unsigned int malign = sizeof(int);
  for (auto m : this->ms) {
    malign = std::max<unsigned int>(malign, alignment(m.type));
  }
  return align<unsigned int>(payloadOffset() + payloadSize(), malign);
}

// record types
Record::Member::Member(const std::string& field, const MonoTypePtr& type, int offset) : field(field), type(type), offset(offset) { }
Record::Member::Member() : field(""), type(), offset(0) { }
bool Record::Member::operator==(const Record::Member& rhs) const { return this->field == rhs.field && *this->type == *rhs.type && this->offset == rhs.offset; }

bool Record::Member::operator<(const Record::Member& rhs) const {
  if (this->field < rhs.field) {
    return true;
  } else if (rhs.field < this->field) {
    return false;
  } else if (this->type < rhs.type) {
    return true;
  } else if (rhs.type < this->type) {
    return false;
  } else if (this->offset < rhs.offset) {
    return true;
  } else {
    return false;
  }
}

inline Record::Member addoffset(const Record::Member& m, int o) {
  return Record::Member(m.field, m.type, o);
}

MonoTypePtr Record::make(const Members& ms) {
  return makeType<RecordMem, Record>(Record::withResolvedMemoryLayout(ms));
}

Record::Record(const Record::Members& tms) : ms(tms), maxFieldAlignmentM(-1) {
  for (const Member& m : ms) {
    this->freeTVars = setUnion(this->freeTVars, m.type->freeTVars);
    this->tgenCount = std::max<int>(this->tgenCount, m.type->tgenCount);
  }

  if (isMonoSingular(this)) {
    this->ams = withExplicitPadding(this->ms);
  }
}

unsigned int nextVisibleMember(unsigned int i, const Record::Members& ms) {
  while (i < ms.size() && ms[i].field.substr(0, 2) == ".p") {
    ++i;
  }
  return i;
}

int findHiddenMember(int i, const std::string& lbl, const Record::Members& ms) {
  std::string hlbl = ".p" + lbl;
  while (i >= 0 && ms[i].field != hlbl) {
    --i;
  }
  return i;
}

static void resetFieldOffsets(Record::Members* ms) {
  for (Record::Members::iterator m = ms->begin(); m != ms->end(); ++m) {
    m->offset = -1;
  }
}

Record::Members consMember(const std::string& lbl, const MonoTypePtr& hty, const Record::Members& tty) {
  // try to find a place to insert this type, otherwise insert it at the head
  int slot = findHiddenMember(tty.size() - 1, lbl, tty);

  Record::Members r;
  if (slot < 0) {
    r.push_back(Record::Member(lbl, hty));
    r.insert(r.end(), tty.begin(), tty.end());
    resetFieldOffsets(&r);
  } else {
    r = tty;
    r[slot].field = lbl;
  }
  return r;
}

static void normalizeTupleFields(Record::Members* ms) {
  size_t i = 0;
  for (Record::Members::iterator m = ms->begin(); m != ms->end(); ++m) {
    if (m->field.substr(0, 2) != ".p") {
      m->field = ".f" + str::from(i++);
    }
  }
}

MonoTypePtr Record::make(const MonoTypePtr& hty, const Members& tty) {
  Members ms = consMember(".f0", hty, tty);
  normalizeTupleFields(&ms);
  return make(ms);
}

MonoTypePtr Record::make(const std::string& lbl, const MonoTypePtr& hty, const Members& tty) {
  Members ms = consMember(lbl, hty, tty);
  return make(ms);
}

Record::Members tailMembers(bool tup, const Record::Members& ms) {
  // copy the remaining fields, take care to shift tuple field names if necessary
  Record::Members tms = ms;
  unsigned int h = nextVisibleMember(0, tms);
  if (h >= tms.size()) {
    throw std::runtime_error("Record has no tail members");
  }
  tms[h].field = ".p" + tms[h].field;

  if (tup) {
    int i = 0;
    for (unsigned int k = h + 1; k < tms.size(); ++k) {
      tms[k].field = ".f" + str::from(i++);
    }
  }
  return tms;
}

Record::Member Record::headMember() const {
  unsigned int h = nextVisibleMember(0, this->ms);
  if (h >= this->ms.size()) {
    throw std::runtime_error("Record has no head member");
  }

  Record::Member m = this->ms[h];
  m.type = normIfOpaquePtr(m.type);
  return m;
}

MonoTypePtr Record::tailType() const {
  // bottom out at the unit type (ie: X = X*1)
  Members tms = tailMembers(isTuple(), this->ms);
  if (nextVisibleMember(0, tms) >= tms.size()) {
    return prim<void>();
  } else {
    return Record::make(tms);
  }
}

Record::Members Record::withResolvedMemoryLayout(const Members& ms) {
  Members r;

  // infer offsets and/or insert padding as necessary
  int o = 0;
  for (Record::Members::const_iterator m = ms.begin(); m != ms.end(); ++m) {
    if (!isMonoSingular(m->type)) {
      return ms;
    }

    o = align<unsigned int>(o, alignment(m->type));

    // determine the 'in-memory' layout of this structure
    //   (must be consistent with GCC so that we can interoperate)
    if (m->offset == -1) {
      r.push_back(addoffset(*m, o));
    } else if (m->offset > o) {
      throw
        std::runtime_error(
          "Actual/calculated member offset mismatch "
          "at field '" + m->field + "':" + str::from(sizeOf(m->type)) +
          " (" + str::from(m->offset) + " > " + str::from(o) + ") in record: " +
          showRecord(ms)
        );
    } else if (m->offset == o) {
      r.push_back(*m);
    } else {
      throw std::runtime_error(
        "Invalid record structure defined with misaligned offsets found at field "
        "'" + m->field + "':" + str::from(sizeOf(m->type)) +
        " (" + str::from(m->offset) + " < " + str::from(o) + ") in record: " +
        showRecord(ms)
      );
    }

    o += sizeOf(m->type);
  }

  return r;
}

unsigned int maxFieldAlignmentF(const Record::Members& ms) {
  unsigned int result = 1;
  for (const auto& m : ms) {
    result = std::max<unsigned int>(result, alignment(m.type));
  }
  return result;
}

unsigned int Record::maxFieldAlignment() const {
  if (this->maxFieldAlignmentM == static_cast<unsigned int>(-1)) {
    this->maxFieldAlignmentM = maxFieldAlignmentF(this->ms);
  }
  return this->maxFieldAlignmentM;
}

unsigned int Record::size() const {
  // a record with no members is equivalent to unit
  if (this->ms.size() == 0) {
    return 0;
  }

  // align to strictest field
  unsigned int malign = maxFieldAlignment();

  // align from last non-unit member offset
  for (auto m = this->ms.rbegin(); m != this->ms.rend(); ++m) {
    size_t fsz = sizeOf(m->type);

    if (fsz > 0) {
      return align<unsigned int>(m->offset + fsz, malign);
    }
  }

  // if we got here, we must be all unit
  return 0;
}

Record::Members Record::withExplicitPadding(const Members& ms, const std::string& pfx) {
  Members r;
  int     o = 0; // the active determined offset in memory
  int     p = 0; // unique names for pad fields

  for (Record::Members::const_iterator m = ms.begin(); m != ms.end(); ++m) {
    if (m->offset > o) {
      r.push_back(Member(pfx + str::from(p++), arrayty(prim<char>(), m->offset - o)));
    }
    size_t msz = sizeOf(m->type);
    o = m->offset + msz;

    if (msz > 0) {
      r.push_back(*m);
    }
  }

  // account for trailing padding
  unsigned int talign = maxFieldAlignmentF(ms);

  if (talign > 0) {
    unsigned int asz = align<unsigned int>(o, talign);
    if (int(asz) > o) {
      r.push_back(Member(pfx + str::from(p++), arrayty(prim<char>(), asz - o)));
    }
  }

  return r;
}

bool isTupleDesc(const Record::Members& ms) {
  for (unsigned int i = 0; i < ms.size(); ++i) {
    if (ms[i].field.substr(0, 2) == ".f") {
      return true;
    }
  }
  return false;
}

void showAsTuple(std::ostream& out, const Record::Members& ms) {
  bool once = false;

  out << "(";
  for (size_t i = 0; i < ms.size(); ++i) {
    if (once) { out << " * "; }
    if (ms[i].field.size() > 2 && ms[i].field[1] != 'p') {
      ms[i].type->show(out);
      once = true;
    }
  }
  out << ")";
}

void showAsRecord(std::ostream& out, const Record::Members& ms) {
  if (ms.size() == 0) {
    out << "{}";
  } else {
    out << "{ ";
    out << ms[0].field << ":";
    ms[0].type->show(out);
    for (size_t i = 1; i < ms.size(); ++i) {
      out << ", " << ms[i].field << ":";
      ms[i].type->show(out);
    }
    out << " }";
  }
}

void Record::showRecord(std::ostream& out, const Record::Members& ms) {
  if (isTupleDesc(ms)) {
    showAsTuple(out, ms);
  } else {
    showAsRecord(out, ms);
  }
}

std::string Record::showRecord(const Members& ms) {
  std::ostringstream ss;
  showRecord(ss, ms);
  return ss.str();
}

void Record::show(std::ostream& out) const {
  showRecord(out, this->ms);
}

bool Record::isTuple() const {
  return isTupleDesc(this->ms);
}

const MonoTypePtr& Record::member(const std::string& mn) const {
  if (auto m = mmember(mn)) {
    return m->type;
  }

  // try to make a helpful error message out of this
  std::string tdesc = hobbes::show(this);
  static const size_t truncTyLen = 40;
  if (tdesc.size() < truncTyLen) {
    throw std::runtime_error("Field '" + mn + "' does not exist in type: " + tdesc);
  }

  str::seq suggestions = str::closestMatches(mn, selectNames(this->ms), 3);
  if (suggestions.size() == 1) {
    throw std::runtime_error("Field '" + mn + "' (did you mean '" + suggestions[0] + "'?) does not exist in type: " + tdesc.substr(0, truncTyLen) + "...");
  } else if (suggestions.size() > 0) {
    std::ostringstream ss;
    ss << "Field '" << mn << "' (did you mean one of: '" << suggestions[0] << "'";
    for (size_t i = 1; i < suggestions.size(); ++i) {
      ss << ", '" << suggestions[i] << "'";
    }
    ss << "?) does not exist in type: " << tdesc.substr(0, truncTyLen) << "...";
    throw std::runtime_error(ss.str());
  } else {
    throw std::runtime_error("Field '" + mn + "' does not exist in type: " + tdesc.substr(0, truncTyLen) + "...");
  }
}

unsigned int Record::index(const std::string& mn) const {
  return index(this->ms, mn);
}

const Record::Member* Record::mmember(const std::string& mn) const {
  for (Members::const_iterator m = this->ms.begin(); m != this->ms.end(); ++m) {
    if (m->field == mn) {
      return &(*m);
    }
  }
  return 0;
}

const Record::Members& Record::members() const {
  return this->ms;
}

const Record::Members& Record::alignedMembers() const {
  return this->ams;
}

unsigned int Record::alignedIndex(const std::string& mn) const {
  return index(this->ams, mn);
}

unsigned int Record::index(const Members& ms, const std::string& mn) const {
  unsigned int k = 0;
  for (Members::const_iterator m = ms.begin(); m != ms.end(); ++m) {
    // pretend that we don't see fields with unit type
    //   (they don't make it into the final compiled record anyway)
    if (m->field == mn) {
      return k;
    }
    if (!isUnit(m->type)) {
      ++k;
    }
  }

  throw std::runtime_error("Struct member '" + mn + "' does not exist in " + showRecord(ms));
}

MonoTypePtr Func::make(const MonoTypePtr& aty, const MonoTypePtr& rty) {
  return makeType<FuncMem, Func>(aty, rty);
}

Func::Func(const MonoTypePtr& aty, const MonoTypePtr& rty) : aty(aty), rty(rty) {
  this->freeTVars = setUnion(aty->freeTVars, rty->freeTVars);
  this->tgenCount = std::max<int>(aty->tgenCount, rty->tgenCount);
}

const MonoTypePtr& Func::argument() const { return this->aty; }
const MonoTypePtr& Func::result() const { return this->rty; }

MonoTypes Func::parameters() const {
  // simplify multi-argument lookup
  const Record* arty = is<Record>(this->aty);
  if (arty && arty->isTuple()) {
    return selectTypes(arty->members());
  } else {
    return list(this->aty);
  }
}

void Func::show(std::ostream& out) const {
  this->aty->show(out);
  out << " -> ";
  this->rty->show(out);
}

MonoTypePtr Exists::make(const std::string& tname, const MonoTypePtr& bty) {
  return makeType<ExistsMem, Exists>(tname, bty);
}

Exists::Exists(const std::string& tname, const MonoTypePtr& bty) : tname(tname), bty(bty) {
  this->freeTVars = setDifference(bty->freeTVars, tname);
  this->tgenCount = bty->tgenCount;
}

void Exists::show(std::ostream& out) const {
  out << "exists " << this->tname << ".";
  this->bty->show(out);
}

const std::string& Exists::absTypeName() const { return this->tname; }
const MonoTypePtr& Exists::absType() const { return this->bty; }

// recursive types
MonoTypePtr Recursive::make(const std::string& tname, const MonoTypePtr& bty) {
  return makeType<RecursiveMem, Recursive>(tname, bty);
}

Recursive::Recursive(const std::string& tname, const MonoTypePtr& bty) : tname(tname), bty(bty) {
  this->freeTVars = setDifference(bty->freeTVars, tname);
  this->tgenCount = bty->tgenCount;
}

void Recursive::show(std::ostream& out) const {
  out << "^" << this->tname << ".";
  this->bty->show(out);
}

const std::string& Recursive::recTypeName() const { return this->tname; }
const MonoTypePtr& Recursive::recType() const { return this->bty; }

// type-level values
MonoTypePtr TString::make(const std::string& x) {
  return makeType<TStringMem, TString>(x);
}

TString::TString(const std::string& val) : val(val) {
}

void TString::show(std::ostream& out) const { out << "'" << this->val << "'"; }
const std::string& TString::value() const { return this->val; }

MonoTypePtr TLong::make(long x) {
  return makeType<TLongMem, TLong>(x);
}

TLong::TLong(long x) : x(x) {
}

void TLong::show(std::ostream& out) const { out << this->x << "L"; }
long TLong::value() const { return this->x; }

// type-level expressions (which should soon consume the type-level 'long' and 'string' constructions above
MonoTypePtr TExpr::make(const ExprPtr& e) {
  // XXX: a subtle point here -- rather than uniquely identify type-level expressions by expression pointers,
  //      we identify them by _printed_ expression forms, under the assumption that this is adequate to uniquely
  //      identify expressions in the global context (otherwise we'd wind up with a lot of equivalent types
  //      unintentionally interpreted as distinct
  return
    std::const_pointer_cast<MonoType>(std::static_pointer_cast<const MonoType>(
      tctorMaps()->at<TExprMem>().get([e](const std::string&){return new TExpr(e);}, hobbes::show(e))
    ));
}

TExpr::TExpr(const ExprPtr& e) : e(e) {
}

void TExpr::show(std::ostream& out) const { this->e->show(out); }
const ExprPtr& TExpr::expr() const { return this->e; }

/////////////////////
// type visitor utilities
/////////////////////
UnitV walkTy::with(const Prim*       v) const { return unitv; }
UnitV walkTy::with(const OpaquePtr*  v) const { return unitv; }
UnitV walkTy::with(const TVar*       v) const { return unitv; }
UnitV walkTy::with(const TGen*       v) const { return unitv; }
UnitV walkTy::with(const TAbs*       v) const { switchOf(v->body(), *this); return unitv; }
UnitV walkTy::with(const TApp*       v) const { switchOf(v->fn(), *this); for (auto a : v->args()) { switchOf(a, *this); } return unitv; }
UnitV walkTy::with(const FixedArray* v) const { switchOf(v->type(), *this); switchOf(v->length(), *this); return unitv; }
UnitV walkTy::with(const Array*      v) const { switchOf(v->type(), *this); return unitv; }
UnitV walkTy::with(const Variant*    v) const { for (auto c : v->members()) { switchOf(c.type, *this); } return unitv; }
UnitV walkTy::with(const Record*     v) const { for (auto f : v->members()) { switchOf(f.type, *this); } return unitv; }
UnitV walkTy::with(const Func*       v) const { switchOf(v->argument(), *this); switchOf(v->result(), *this); return unitv; }
UnitV walkTy::with(const Exists*     v) const { switchOf(v->absType(), *this); return unitv; }
UnitV walkTy::with(const Recursive*  v) const { switchOf(v->recType(), *this); return unitv; }
UnitV walkTy::with(const TString*    v) const { return unitv; }
UnitV walkTy::with(const TLong*      v) const { return unitv; }
UnitV walkTy::with(const TExpr*      v) const { return unitv; }

MonoTypePtr switchTyFn::with(const Prim*       v) const { return Prim::make(v->name(), v->representation()); }
MonoTypePtr switchTyFn::with(const OpaquePtr*  v) const { return OpaquePtr::make(v->name(), v->size(), v->storedContiguously()); }
MonoTypePtr switchTyFn::with(const TVar*       v) const { return TVar::make(v->name()); }
MonoTypePtr switchTyFn::with(const TGen*       v) const { return TGen::make(v->id()); }
MonoTypePtr switchTyFn::with(const TAbs*       v) const { return TAbs::make(v->args(), switchOf(v->body(), *this)); }
MonoTypePtr switchTyFn::with(const TApp*       v) const { return TApp::make(switchOf(v->fn(), *this), switchOf(v->args(), *this)); }
MonoTypePtr switchTyFn::with(const FixedArray* v) const { return FixedArray::make(switchOf(v->type(), *this), switchOf(v->length(), *this)); }
MonoTypePtr switchTyFn::with(const Array*      v) const { return Array::make(switchOf(v->type(), *this)); }
MonoTypePtr switchTyFn::with(const Variant*    v) const { return Variant::make(switchOf(v->members(), *this)); }
MonoTypePtr switchTyFn::with(const Record*     v) const { return Record::make(switchOf(v->members(), *this)); }
MonoTypePtr switchTyFn::with(const Func*       v) const { return Func::make(switchOf(v->argument(), *this), switchOf(v->result(), *this)); }
MonoTypePtr switchTyFn::with(const Exists*     v) const { return Exists::make(v->absTypeName(), switchOf(v->absType(), *this)); }
MonoTypePtr switchTyFn::with(const Recursive*  v) const { return Recursive::make(v->recTypeName(), switchOf(v->recType(), *this)); }

MonoTypePtr switchTyFn::with(const TString* v) const { return TString::make(v->value()); }
MonoTypePtr switchTyFn::with(const TLong*   v) const { return TLong::make(v->value()); }

MonoTypePtr switchTyFn::with(const TExpr* v) const {
  // to avoid infinite regress, just leave type-level expressions alone for now
  // we might need to revisit this at some point ...
  return TExpr::make(v->expr());
}

MonoTypePtr clone(const MonoTypePtr& t) { return t; }
MonoTypePtr clone(const MonoType*    t) { return clone(*t); }

struct cloneF : public switchType<MonoTypePtr> {
  MonoTypePtr with(const Prim*       v) const { return Prim::make(v->name(), v->representation()); }
  MonoTypePtr with(const OpaquePtr*  v) const { return OpaquePtr::make(v->name(), v->size(), v->storedContiguously()); }
  MonoTypePtr with(const TVar*       v) const { return TVar::make(v->name()); }
  MonoTypePtr with(const TGen*       v) const { return TGen::make(v->id()); }
  MonoTypePtr with(const TAbs*       v) const { return TAbs::make(v->args(), v->body()); }
  MonoTypePtr with(const TApp*       v) const { return TApp::make(v->fn(), v->args()); }
  MonoTypePtr with(const FixedArray* v) const { return FixedArray::make(v->type(), v->length()); }
  MonoTypePtr with(const Array*      v) const { return Array::make(v->type()); }
  MonoTypePtr with(const Variant*    v) const { return Variant::make(v->members()); }
  MonoTypePtr with(const Record*     v) const { return Record::make(v->members()); }
  MonoTypePtr with(const Func*       v) const { return Func::make(v->argument(), v->result()); }
  MonoTypePtr with(const Exists*     v) const { return Exists::make(v->absTypeName(), v->absType()); }
  MonoTypePtr with(const Recursive*  v) const { return Recursive::make(v->recTypeName(), v->recType()); }
  
  MonoTypePtr with(const TString* v) const { return TString::make(v->value()); }
  MonoTypePtr with(const TLong*   v) const { return TLong::make(v->value()); }
  MonoTypePtr with(const TExpr*   v) const { return TExpr::make(v->expr()); }
};
MonoTypePtr clone(const MonoType& t) { return switchOf(t, cloneF()); }

QualTypePtr cloneP(const QualTypePtr& p) {
  return substitute(MonoTypeSubst(), p);
}

MonoTypePtr cloneP(const MonoTypePtr& p) {
  return substitute(MonoTypeSubst(), p);
}

///////////////////
// record type analysis utilities
///////////////////
QualTypePtr lookupFieldType(const QualTypePtr& qt, const std::string& fieldName) {
  MonoTypePtr mt = lookupFieldType(qt->monoType(), fieldName);
  return QualTypePtr(new QualType(qt->constraints(), mt));
}

MonoTypePtr lookupFieldType(const MonoTypePtr& mt, const std::string& fieldName) {
  if (Record* rt = is<Record>(mt)) {
    return rt->member(fieldName);
  } else {
    throw std::runtime_error("Cannot index field '" + fieldName + "' in non-record type: " + show(mt));
  }
}

///////////////////
// constraint set utilities
///////////////////
Constraints mergeConstraints(const Constraints& lhs, const Constraints& rhs) {
  Constraints r;
  mergeConstraints(lhs, &r);
  mergeConstraints(rhs, &r);
  return r;
}

void mergeConstraints(const Constraints& fcs, Constraints* tcs) {
  for (Constraints::const_iterator c = fcs.begin(); c != fcs.end(); ++c) {
    tcs->push_back(*c);
  }
}

///////////////////
// polytype / gen utilities
///////////////////

static std::atomic<std::size_t> uidCtr(0); // used to generate fresh symbols

TVName freshName() {
  return ".t" + str::from(uidCtr++);
}

Names freshNames(int vs) {
  Names r;
  for (int i = 0; i < vs; ++i) {
    r.push_back(freshName());
  }
  return r;
}

MonoTypePtr freshTypeVar() {
  return TVar::make(freshName());
}

MonoTypes freshTypeVars(int vs) {
  MonoTypes r;
  for (int i = 0; i < vs; ++i) {
    r.push_back(freshTypeVar());
  }
  return r;
}

MonoTypes freshen(const MonoTypes& ts) {
  MonoTypeSubst s;

  NameSet tvns = tvarNames(ts);
  for (NameSet::const_iterator n = tvns.begin(); n != tvns.end(); ++n) {
    s[*n] = freshTypeVar();
  }

  return substitute(s, ts);
}

ConstraintPtr freshen(const ConstraintPtr& cst) {
  NameSet cvns = tvarNames(cst);
  MonoTypeSubst s;
  for (NameSet::const_iterator cvn = cvns.begin(); cvn != cvns.end(); ++cvn) {
    s[*cvn] = freshTypeVar();
  }
  return substitute(s, cst);
}

Constraints freshen(const Constraints& cs) {
  NameSet cvns = tvarNames(cs);
  MonoTypeSubst s;
  for (NameSet::const_iterator cvn = cvns.begin(); cvn != cvns.end(); ++cvn) {
    s[*cvn] = freshTypeVar();
  }
  return substitute(s, cs);
}

MonoTypes typeVars(const Names& ns) {
  MonoTypes r;
  for (Names::const_iterator n = ns.begin(); n != ns.end(); ++n) {
    r.push_back(TVar::make(*n));
  }
  return r;
}

MonoTypes tgens(int c) {
  MonoTypes r;
  for (int i = 0; i < c; ++i) {
    r.push_back(TGen::make(i));
  }
  return r;
}

// determine whether or not a monotype actually contains references to any type variables or variable generator points
bool isMonoSingular(const MonoType& mt)    { return mt.freeTVars.size() == 0 && mt.tgenCount == 0; }
bool isMonoSingular(const MonoType* mt)    { return isMonoSingular(*mt); }
bool isMonoSingular(const MonoTypePtr& mt) { return isMonoSingular(*mt); }

bool isMonoSingular(const QualTypePtr& qt) {
  return qt->constraints().size() == 0 && isMonoSingular(qt->monoType());
}

// find the highest TGen reference (useful for deducing the number of type variables required to generalize a mono type)
int tgenSize(const MonoTypePtr& mt) {
  return mt->tgenCount;
}

int tgenSize(const MonoTypes& mts) {
  int x = 0;
  for (MonoTypes::const_iterator mt = mts.begin(); mt != mts.end(); ++mt) {
    x = std::max<int>(x, tgenSize(*mt));
  }
  return x;
}

int tgenSize(const Constraints& cs) {
  throw std::runtime_error("tgenSize on constraints NYI");
}

int tgenSize(const QualTypePtr& qt) {
  return std::max<int>(tgenSize(qt->constraints()), tgenSize(qt->monoType()));
}

// find the set of tgen variables in a type expression
struct tgenVarsF : public walkTy {
  TGenVarSet* s;
  tgenVarsF(TGenVarSet* s) : s(s) { }

  UnitV with(const TGen* v) const {
    this->s->insert(v->id());
    return unitv;
  }
};

TGenVarSet tgenVars(const MonoTypePtr& mt) {
  if (isMonoSingular(mt)) {
    return TGenVarSet();
  } else {
    TGenVarSet result;
    switchOf(mt, tgenVarsF(&result));
    return result;
  }
}


// type instantiation
QualTypePtr   instantiate(int vs, const QualTypePtr& scheme) { if (vs == 0)                       { return scheme; } else { return instantiate(freshTypeVars(vs), scheme); } }
Constraints   instantiate(int vs, const Constraints& cs)     { if (vs == 0)                       { return cs;     } else { return instantiate(freshTypeVars(vs), cs);     } }
ConstraintPtr instantiate(int vs, const ConstraintPtr& c)    { if (vs == 0)                       { return c;      } else { return instantiate(freshTypeVars(vs), c);      } }
MonoTypePtr   instantiate(int vs, const MonoTypePtr& mt)     { if (vs == 0 || isMonoSingular(mt)) { return mt;     } else { return instantiate(freshTypeVars(vs), mt);     } }
MonoTypes     instantiate(int vs, const MonoTypes& ts)       { if (vs == 0)                       { return ts;     } else { return instantiate(freshTypeVars(vs), ts);     } }

QualTypePtr instantiate(const MonoTypes& ts, const QualTypePtr& scheme) {
  return QualTypePtr(new QualType(instantiate(ts, scheme->constraints()), instantiate(ts, scheme->monoType())));
}

Constraints instantiate(const MonoTypes& ts, const Constraints& cs) {
  Constraints r;
  for (Constraints::const_iterator c = cs.begin(); c != cs.end(); ++c) {
    r.push_back(instantiate(ts, *c));
  }
  return r;
}

ConstraintPtr instantiate(const MonoTypes& ts, const ConstraintPtr& c) {
  return c->instantiate(ts);
}

class instantiateF : public switchTyFn {
public:
  instantiateF(const MonoTypes& ts) : ts(ts) { }

  MonoTypePtr with(const TGen* v) const {
    return MonoTypePtr(this->ts[v->id()]);
  }
private:
  const MonoTypes& ts;
};

MonoTypePtr instantiate(const MonoTypes& ts, const MonoTypePtr& mt) {
  if (isMonoSingular(mt)) {
    return mt;
  } else {
    return switchOf(mt, instantiateF(ts));
  }
}

MonoTypes instantiate(const MonoTypes& ts, const MonoTypes& sts) {
  MonoTypes result;
  for (auto st : sts) {
    result.push_back(instantiate(ts, st));
  }
  return result;
}

QualTypePtr   instantiate(const Names& ns, const QualTypePtr& scheme) { return instantiate(typeVars(ns), scheme); }
Constraints   instantiate(const Names& ns, const Constraints& cs)     { return instantiate(typeVars(ns), cs); }
ConstraintPtr instantiate(const Names& ns, const ConstraintPtr& c)    { return instantiate(typeVars(ns), c); }
MonoTypePtr   instantiate(const Names& ns, const MonoTypePtr& mt)     { return instantiate(typeVars(ns), mt); }
MonoTypes     instantiate(const Names& ns, const MonoTypes& ts)       { return instantiate(typeVars(ns), ts); }

// type variable reference analysis
NameSet tvarNames(const QualTypePtr& qt) {
  NameSet r;
  tvarNames(qt, &r);
  return r;
}

NameSet tvarNames(const Constraints& cs) {
  NameSet r;
  tvarNames(cs, &r);
  return r;
}

NameSet tvarNames(const ConstraintPtr& c) {
  NameSet r;
  tvarNames(c, &r);
  return r;
}

NameSet tvarNames(const MonoTypePtr& mt) {
  NameSet r;
  tvarNames(mt, &r);
  return r;
}

NameSet tvarNames(const MonoType& mt) {
  NameSet r;
  tvarNames(mt, &r);
  return r;
}

NameSet tvarNames(const MonoTypes& mts) {
  NameSet r;
  tvarNames(mts, &r);
  return r;
}

void tvarNames(const QualTypePtr& qt, NameSet* out) {
  tvarNames(qt->constraints(), out);
  tvarNames(qt->monoType(), out);
}

void tvarNames(const Constraints& cs, NameSet* out) {
  for (Constraints::const_iterator c = cs.begin(); c != cs.end(); ++c) {
    tvarNames(*c, out);
  }
}

void tvarNames(const ConstraintPtr& c, NameSet* out) {
  NameSet ctvns = c->tvarNames();
  out->insert(ctvns.begin(), ctvns.end());
}

void tvarNames(const MonoTypePtr& mt, NameSet* out) {
  tvarNames(*mt, out);
}

void tvarNames(const MonoType& mt, NameSet* out) {
  out->insert(mt.freeTVars.begin(), mt.freeTVars.end());
}

void tvarNames(const MonoTypes& mts, NameSet* out) {
  for (const auto& mt : mts) {
    tvarNames(mt, out);
  }
}

bool isFreeVarNameIn(const TVName& n, const MonoTypePtr& t) {
  return t->freeTVars.find(n) != t->freeTVars.end();
}

bool isFreeVarNameIn(const TVName& n, const MonoTypes& ts) {
  for (const auto& t : ts) {
    if (isFreeVarNameIn(n, t)) {
      return true;
    }
  }
  return false;
}

// are there free variables in a type?
bool hasFreeVariables(const QualTypePtr& qt) {
  return hasFreeVariables(qt->constraints()) || hasFreeVariables(qt->monoType());
}

bool hasFreeVariables(const Constraints& cs) {
  for (Constraints::const_iterator c = cs.begin(); c != cs.end(); ++c) {
    if (hasFreeVariables(*c)) {
      return true;
    }
  }
  return false;
}

bool hasFreeVariables(const ConstraintPtr& c) {
  return c->hasFreeVariables();
}

bool hasFreeVariables(const MonoTypePtr& mt) {
  return !isMonoSingular(mt);
}

bool hasFreeVariables(const MonoTypes& mts) {
  for (MonoTypes::const_iterator mt = mts.begin(); mt != mts.end(); ++mt) {
    if (hasFreeVariables(*mt)) {
      return true;
    }
  }
  return false;
}

// show a substitution
std::string show(const MonoTypeSubst& s) {
  str::seqs stbl;
  stbl.resize(3);
  stbl[0].push_back("Variable");
  stbl[1].push_back("");
  stbl[2].push_back("Type");

  for (MonoTypeSubst::const_iterator si = s.begin(); si != s.end(); ++si) {
    stbl[0].push_back(si->first);
    stbl[1].push_back(" = ");
    stbl[2].push_back(show(si->second));
  }

  return str::showLeftAlignedTable(stbl);
}

void show(const MonoTypeSubst& s, std::ostream& out) {
  for (MonoTypeSubst::const_iterator si = s.begin(); si != s.end(); ++si) {
    out << si->first << " = " << std::flush;
    si->second->show(out);
    out << std::endl;
  }
}

// simplifying substitution from free type variables
QualTypePtr substitute(const MonoTypeSubst& s, const QualTypePtr& qt) {
  return QualTypePtr(new QualType(substitute(s, qt->constraints()), substitute(s, qt->monoType())));
}

inline bool in(const ConstraintPtr& c, const Constraints& cs) {
  for (Constraints::const_iterator ci = cs.begin(); ci != cs.end(); ++ci) {
    if (*c == **ci) {
      return true;
    }
  }
  return false;
}

Constraints substitute(const MonoTypeSubst& s, const Constraints& cs) {
  Constraints r;
  for (Constraints::const_iterator c = cs.begin(); c != cs.end(); ++c) {
    ConstraintPtr sc = substitute(s, *c);
    if (!in(sc, r)) {
      r.push_back(substitute(s, *c));
    }
  }
  return r;
}

ConstraintPtr substitute(const MonoTypeSubst& s, const ConstraintPtr& p) {
  return p->substitute(s);
}

class substituteF : public switchTyFn {
public:
  substituteF(bool transitive, const MonoTypeSubst& s) : transitive(transitive), s(s) { }

  MonoTypePtr with(const TVar* v) const {
    MonoTypeSubst::const_iterator si = this->s.find(v->name());
    if (si != this->s.end()) {
      if (this->transitive) {
        return switchOf(si->second, *this);
      } else {
        return si->second;
      }
    } else {
      return TVar::make(v->name());
    }
  }

  MonoTypePtr with(const Exists* v) const {
    if (this->s.find(v->absTypeName()) == this->s.end()) {
      return Exists::make(v->absTypeName(), switchOf(v->absType(), *this));
    } else {
      MonoTypeSubst cuts = this->s;
      cuts.erase(v->absTypeName());
      return Exists::make(v->absTypeName(), switchOf(v->absType(), substituteF(this->transitive, cuts)));
    }
  }

  MonoTypePtr with(const Recursive* v) const {
    if (this->s.find(v->recTypeName()) == this->s.end()) {
      return Recursive::make(v->recTypeName(), switchOf(v->recType(), *this));
    } else {
      MonoTypeSubst cuts = this->s;
      cuts.erase(v->recTypeName());
      return Recursive::make(v->recTypeName(), switchOf(v->recType(), substituteF(this->transitive, cuts)));
    }
  }
private:
  bool transitive;
  const MonoTypeSubst& s;
};

MonoTypePtr substituteStep(const MonoTypeSubst& s, const MonoTypePtr& mt) {
  if (isMonoSingular(mt)) {
    return mt;
  } else {
    return switchOf(mt, substituteF(false, s));
  }
}

MonoTypePtr substitute(const MonoTypeSubst& s, const MonoTypePtr& mt) {
  if (isMonoSingular(mt)) {
    return mt;
  } else {
    return switchOf(mt, substituteF(true, s));
  }
}

MonoTypePtr substitute(const MonoTypeSubst& s, const MonoType& mt) {
  if (isMonoSingular(mt)) {
    return clone(mt);
  } else {
    return switchOf(mt, substituteF(true, s));
  }
}

MonoTypes substitute(const MonoTypeSubst& s, const MonoTypes& ts) {
  MonoTypes r;
  for (MonoTypes::const_iterator t = ts.begin(); t != ts.end(); ++t) {
    r.push_back(substitute(s, *t));
  }
  return r;
}

// introduce a quantifier for all free type variables
PolyTypePtr generalize(const QualTypePtr& qt) {
  // these are the free types
  NameSet fnames = tvarNames(qt);

  // [(v, TGen i) | (v, i) <- zip vnames [0..]]
  MonoTypeSubst s;
  int i = 0;
  for (NameSet::const_iterator n = fnames.begin(); n != fnames.end(); ++n) {
    s[*n] = TGen::make(i);
    ++i;
  }

  return PolyTypePtr(new PolyType(fnames.size(), substitute(s, qt)));
}

// simplify ugly generated variable names
TVName canonicalName(int v) {
  if (v >= 26) {
    return "t" + str::from(v - 26);
  } else {
    static const char cs[] = "abcdefghijklmnopqrstuvwxyz";
    return TVName(1, cs[v]);
  }
}

MonoTypeSubst canonicalNameSubst(const NameSet& ns) {
  MonoTypeSubst r;
  int v = 0;
  for (NameSet::const_iterator n = ns.begin(); n != ns.end(); ++n) {
    TVName cn = canonicalName(v);
    // avoid making substitution cycles
    if (*n != cn) {
      r[*n] = TVar::make(cn);
    }
    ++v;
  }
  return r;
}

QualTypePtr   simplifyVarNames(const PolyType&      t) { return simplifyVarNames(t.instantiate()); }
QualTypePtr   simplifyVarNames(const PolyTypePtr&   t) { return simplifyVarNames(*t); }
ConstraintPtr simplifyVarNames(const Constraint&    c) { return simplifyVarNames(ConstraintPtr(new Constraint(c))); }
ConstraintPtr simplifyVarNames(const ConstraintPtr& c) { return substitute(canonicalNameSubst(tvarNames(c)), c); }
QualTypePtr   simplifyVarNames(const QualType&      t) { return simplifyVarNames(QualTypePtr(new QualType(t.constraints(), t.monoType()))); }
QualTypePtr   simplifyVarNames(const QualTypePtr&   t) { return substitute(canonicalNameSubst(tvarNames(t)), t); }
MonoTypePtr   simplifyVarNames(const MonoType&      t) { return substitute(canonicalNameSubst(tvarNames(t)), t); }
MonoTypePtr   simplifyVarNames(const MonoTypePtr&   t) { return substitute(canonicalNameSubst(tvarNames(t)), t); }

MonoTypes simplifyVarNames(const MonoTypes& mts) {
  return substitute(canonicalNameSubst(tvarNames(mts)), mts);
}

// reduce types to their primitive representation
MonoTypePtr repType(const MonoTypePtr& t) {
  if (const Prim* pt = is<Prim>(t)) {
    if (pt->representation()) {
      return repType(pt->representation());
    }
  } else if (const TApp* a = is<TApp>(t)) {
    if (const TAbs* tf = is<TAbs>(repType(a->fn()))) {
      return repType(substitute(substitution(tf->args(), a->args()), tf->body()));
    }
  }
  return t;
}

// compute the size of a monotype (in bytes)
typedef unsigned int nat;
nat nadd(nat lhs, nat rhs) { return lhs + rhs; }
nat nmax(nat lhs, nat rhs) { return std::max<nat>(lhs, rhs); }

class sizeOfF : public switchType<nat> {
public:
  nat with(const Prim* v) const {
    if (v->representation()) {
      return r(v->representation());
    } else {
      return withPrim(v->name());
    }
  }

  nat with(const OpaquePtr*  v) const { return v->storedContiguously() ? v->size() : sizeof(void*); }
  nat with(const TVar*       v) const { throw std::runtime_error("Can't determine size of type variable '" + v->name() + "'"); }
  nat with(const TGen*       v) const { throw std::runtime_error("Can't determine size of polytype argument #" + str::from(v->id())); }
  nat with(const FixedArray* v) const { return r(v->type()) * v->requireLength(); }
  nat with(const Array*      v) const { return sizeof(void*); }
  nat with(const Variant*    v) const { return rv(v); }
  nat with(const Record*     v) const { return rv(v); }
  nat with(const Func*       v) const { return sizeof(void*); }
  nat with(const Exists*     v) const { return sizeof(void*); }
  nat with(const Recursive*  v) const { return sizeof(void*); }

  nat with(const TAbs* v) const {
    throw std::runtime_error("Can't determine size of type abstraction: " + show(v));
  }

  nat with(const TApp* v) const {
    // uncurry type function applications
    while (const TApp* lv = is<TApp>(v->fn())) {
      v = lv;
    }

    // TODO: replace this with TApp/TFn application
    if (const Prim* f = is<Prim>(v->fn())) {
      if (f->name() == "->") {
        return sizeof(void*);
      } else if (f->name() == "closure") {
        return sizeof(void*);
      } else if (f->name() == "[]") {
        return sizeof(void*);
      } else if (f->name() == "list") {
        return sizeof(void*);
      } else if (f->name() == "fseq") {
        return sizeof(long);
      } else if (f->name() == "lseq") {
        return sizeof(void*);
      } else if (f->name() == "file") {
        return sizeof(long);
      } else if (f->name() == "fileref") {
        return sizeof(long);
      } else if (f->name() == "process") {
        return sizeof(long);
      } else if (f->name() == "connection") {
        return 0;
      } else if (f->name() == "quote") {
        return 0;
      } else if (f->name() == "promise") {
        return sizeof(long);
      } else if (const TAbs* tf = is<TAbs>(f->representation())) {
        return r(substitute(substitution(tf->args(), v->args()), tf->body()));
      }
    }
    throw std::runtime_error("Can't determine size of monotype: " + show(v));
  }

  // type-level values/expressions have no runtime content (they're equivalent to unit)
  nat with(const TString* v) const { return 0; }
  nat with(const TLong*   v) const { return 0; }
  nat with(const TExpr*   v) const { return 0; }
private:
  nat withPrim(const std::string& pn) const {
    if (pn == "unit") {
      return 0;
    } else if (pn == "bool") {
      return 1;
    } else if (pn == "char") {
      return 1;
    } else if (pn == "byte") {
      return 1;
    } else if (pn == "short") {
      return sizeof(short);
    } else if (pn == "int") {
      return sizeof(int);
    } else if (pn == "long") {
      return sizeof(long);
    } else if (pn == "float") {
      return sizeof(float);
    } else if (pn == "double") {
      return sizeof(double);
    } else if (pn == "void") {
      return 0; // <-- this should be harmless since you can't make one of these anyway
    } else {
      throw std::runtime_error("Can't determine size of unknown primitive type: " + pn);
    }
  }

  unsigned int r(const MonoTypePtr& t) const {
    if (t->memorySize == static_cast<unsigned int>(-1)) {
      t->memorySize = switchOf(t, *this);
    }
    return t->memorySize;
  }

  unsigned int rv(const Record* t) const {
    if (t->memorySize == static_cast<unsigned int>(-1)) {
      t->memorySize = t->size();
    }
    return t->memorySize;
  }

  unsigned int rv(const Variant* t) const {
    if (t->memorySize == static_cast<unsigned int>(-1)) {
      t->memorySize = t->size();
    }
    return t->memorySize;
  }
};

unsigned int sizeOf(const MonoTypePtr& mt) {
  if (mt->memorySize == static_cast<unsigned int>(-1)) {
    mt->memorySize = switchOf(mt, sizeOfF());
  }
  return mt->memorySize;
}

bool isPrimName(const std::string& tn) {
  static const char* prims[] = {
    /* ::: Set */
    "unit", "void", "bool", "char", "byte", "short", "int", "long", "float", "double",

    /* ::: Set -> Set */
    "[]", "list", "lseq", "process", "quote", "promise", "connection", "wpipe", "rpipe",

    /* ::: Set -> Set -> Set */
    "->", "closure", "fseq", "file", "fileref"
  };
  
  for (unsigned int i = 0; i < sizeof(prims)/sizeof(prims[0]); ++i) {
    if (tn == prims[i]) {
      return true;
    }
  }
  return false;
}

MonoTypePtr unroll(const MonoTypePtr& rty) {
  if (const Recursive* x = is<Recursive>(rty)) {
    MonoTypeSubst s;
    s[x->recTypeName()] = rty;
    return substituteStep(s, x->recType());
  } else {
    throw std::runtime_error("Can't unroll non-recursive type: " + show(rty));
  }
}

#define ntdbthrow(x, ti) \
   throw std::runtime_error("Internal compiler error, " x " on null type db for '" + str::demangle((ti).name()) + "'.")

MonoTypePtr nulltypedb::defineNamedType(const std::string& name, const str::seq& argNames, const MonoTypePtr& ty) { return ty; }
bool        nulltypedb::isTypeName(const std::string&) const { return false; }
MonoTypePtr nulltypedb::namedTypeRepresentation(const std::string&) const {throw std::runtime_error("Can't unalias types in a null type database"); }
void        nulltypedb::defineTypeAlias(const std::string& name, const str::seq& argNames, const MonoTypePtr& ty) { throw std::runtime_error("Can't define transparent type aliases through a null type database."); }
bool        nulltypedb::isTypeAliasName(const std::string& name) const { return false; }
MonoTypePtr nulltypedb::replaceTypeAliases(const MonoTypePtr& ty) const { return ty; }
PolyTypePtr nulltypedb::opaquePtrPolyType(const std::type_info& ti, unsigned int sz, bool inStruct) { ntdbthrow("opaquePtrPolyType", ti); }
MonoTypePtr nulltypedb::opaquePtrMonoType(const std::type_info& ti, unsigned int sz, bool inStruct) { ntdbthrow("opaquePtrMonoType", ti); }
PolyTypePtr nulltypedb::generalize(const MonoTypePtr& mt) const { return polytype(mt); }
nulltypedb nulltdb;

bool isMonotype(const QualTypePtr& qt) {
  return qt->constraints().size() == 0;
}

bool isMonotype(const PolyTypePtr& pt) {
  return pt->typeVariables() == 0 && isMonotype(pt->qualtype());
}

MonoTypePtr requireMonotype(const QualTypePtr& qt) {
  if (isMonotype(qt)) {
    return qt->monoType();
  } else {
    throw std::runtime_error("Required monotype but encountered qualified type: " + show(qt));
  }
}

MonoTypePtr requireMonotype(const PolyTypePtr& pt) {
  if (isMonotype(pt)) {
    return requireMonotype(pt->instantiate());
  } else {
    throw std::runtime_error("Required monotype but encountered polymorphic type: " + show(pt));
  }
}

MonoTypes requireMonotype(const PolyTypes& pts) {
  MonoTypes r;
  for (PolyTypes::const_iterator pt = pts.begin(); pt != pts.end(); ++pt) {
    r.push_back(requireMonotype(*pt));
  }
  return r;
}

// project out of existential types
MonoTypePtr unpackedType(const Exists* ety) {
  MonoTypeSubst s;
  s[ety->absTypeName()] = arrayty(primty("char"), 1);
  return substitute(s, ety->absType());
}

MonoTypePtr unpackedType(const MonoTypePtr& mty) {
  if (const Exists* ety = is<Exists>(mty)) {
    return unpackedType(ety);
  } else {
    return mty;
  }
}

QualTypePtr unpackedType(const QualTypePtr& qty) {
  return qualtype(qty->constraints(), unpackedType(qty->monoType()));
}

// support an efficient binary codec for type descriptions
typedef std::vector<unsigned char> bytes;

void write(bool b,               bytes* out) { out->push_back(b ? 0x01 : 0x00); }
void write(char c,               bytes* out) { out->push_back(c); }
void write(unsigned char c,      bytes* out) { out->push_back(c); }
void write(int x,                bytes* out) { out->insert(out->end(), reinterpret_cast<unsigned char*>(&x), reinterpret_cast<unsigned char*>(&x) + sizeof(x)); }
void write(long x,               bytes* out) { out->insert(out->end(), reinterpret_cast<unsigned char*>(&x), reinterpret_cast<unsigned char*>(&x) + sizeof(x)); }
void write(size_t x,             bytes* out) { out->insert(out->end(), reinterpret_cast<unsigned char*>(&x), reinterpret_cast<unsigned char*>(&x) + sizeof(x)); }
void write(unsigned int x,       bytes* out) { out->insert(out->end(), reinterpret_cast<unsigned char*>(&x), reinterpret_cast<unsigned char*>(&x) + sizeof(x)); }
void write(const std::string& s, bytes* out) { write(s.size(), out); out->insert(out->end(), s.begin(), s.end()); }

template <typename T>
  void write(const std::vector<T>& xs, bytes* out) {
    size_t n = xs.size();
    write(n, out);
    for (size_t i = 0; i < n; ++i) {
      write(xs[i], out);
    }
  }

template <typename T>
  struct readF {
    static T read(const bytes& in, unsigned int* n) {
      T r = *reinterpret_cast<const T*>(&(in[*n]));
      *n += sizeof(T);
      return r;
    }
  };

template <>
  struct readF<bool> {
    static bool read(const bytes& in, unsigned int* n) {
      return readF<unsigned char>::read(in, n) != 0;
    }
  };

template <>
  struct readF<std::string> {
    static std::string read(const bytes& in, unsigned int* n) {
      size_t sz = readF<size_t>::read(in, n);
      if (sz > (in.size()-*n)) {
        throw std::runtime_error("Encoded type information is invalid (recorded string with size=" + str::from(sz) + " but only " + str::from(in.size()-*n) + " bytes are available to read)");
      }
      std::string r(reinterpret_cast<const char*>(&(in[*n])), sz);
      *n += sz;
      return r;
    }
  };

template <typename T>
  struct readF<std::vector<T>> {
    static std::vector<T> read(const bytes& in, unsigned int* n) {
      std::vector<T> rs;
      size_t sz = readF<size_t>::read(in, n);
      for (size_t i = 0; i < sz; ++i) {
        rs.push_back(readF<T>::read(in, n));
      }
      return rs;
    }
  };

template <typename T>
  T read(const bytes& in, unsigned int* n) {
    return readF<T>::read(in, n);
  }

#define TCODE_PRIM      ((char)0)
#define TCODE_OPAQUEPTR ((char)1)
#define TCODE_TGEN      ((char)2)
#define TCODE_TVAR      ((char)3)
#define TCODE_FIXEDARR  ((char)4)
#define TCODE_ARR       ((char)5)
#define TCODE_VARIANT   ((char)6)
#define TCODE_RECORD    ((char)7)
#define TCODE_FUNC      ((char)8)
#define TCODE_EXISTS    ((char)9)
#define TCODE_TSTRING   ((char)10)
#define TCODE_TLONG     ((char)11)

class encodeMonoTypeF : public switchType<UnitV> {
public:
  encodeMonoTypeF(bytes* out) : out(out) { }

  UnitV with(const Prim* v) const {
    write(Prim::type_case_id, this->out);
    write(v->name(),          this->out);
    if (v->representation()) {
      write(true, this->out);
      switchOf(v->representation(), *this);
    } else {
      write(false, this->out);
    }
    return unitv;
  }

  UnitV with(const OpaquePtr* v) const {
    write(OpaquePtr::type_case_id, this->out);
    write(v->name(),               this->out);
    write(v->size(),               this->out);
    write(v->storedContiguously(), this->out);
    return unitv;
  }

  UnitV with(const TGen* v) const {
    write(TGen::type_case_id, this->out);
    write(v->id(),            this->out);
    return unitv;
  }

  UnitV with(const TVar* v) const {
    write(TVar::type_case_id, this->out);
    write(v->name(),          this->out);
    return unitv;
  }

  UnitV with(const TAbs* v) const {
    write(TAbs::type_case_id, this->out);
    write(v->args(), this->out);
    switchOf(v->body(), *this);
    return unitv;
  }

  UnitV with(const TApp* v) const {
    write(TApp::type_case_id, this->out);

    switchOf(v->fn(), *this);
    write(v->args().size(), this->out);
    for (const auto& arg : v->args()) {
      switchOf(arg, *this);
    }
    return unitv;
  }

  UnitV with(const FixedArray* v) const {
    write(FixedArray::type_case_id, this->out);

    switchOf(v->type(),   *this);
    switchOf(v->length(), *this);
    return unitv;
  }

  UnitV with(const Array* v) const {
    write(Array::type_case_id, this->out);

    switchOf(v->type(), *this);
    return unitv;
  }

  UnitV with(const Variant* v) const {
    write(Variant::type_case_id, this->out);

    const Variant::Members& ms = v->members();
    write(ms.size(), this->out);

    for (Variant::Members::const_iterator m = ms.begin(); m != ms.end(); ++m) {
      write(m->selector, this->out);
      write(m->id,       this->out);

      switchOf(m->type, *this);
    }

    return unitv;
  }

  UnitV with(const Record* v) const {
    write(Record::type_case_id, this->out);

    const Record::Members& ms = v->members();
    write(ms.size(), this->out);

    for (Record::Members::const_iterator m = ms.begin(); m != ms.end(); ++m) {
      write(m->field,  this->out);
      write(m->offset, this->out);

      switchOf(m->type, *this);
    }

    return unitv;
  }

  UnitV with(const Func* v) const {
    write(Func::type_case_id, this->out);
    switchOf(v->argument(), *this);
    switchOf(v->result(),   *this);
    return unitv;
  }

  UnitV with(const Exists* v) const {
    write(Exists::type_case_id, this->out);
    write(v->absTypeName(),     this->out);

    switchOf(v->absType(), *this);
    return unitv;
  }

  UnitV with(const Recursive* v) const {
    write(Recursive::type_case_id, this->out);
    write(v->recTypeName(),        this->out);

    switchOf(v->recType(), *this);
    return unitv;
  }

  UnitV with(const TString* v) const {
    write(TString::type_case_id, this->out);
    write(v->value(),            this->out);
    return unitv;
  }

  UnitV with(const TLong* v) const {
    write(TLong::type_case_id, this->out);
    write(v->value(),          this->out);
    return unitv;
  }

  UnitV with(const TExpr* v) const {
    write(TExpr::type_case_id, this->out);
    std::vector<uint8_t> ebs;
    encode(stripAssumpHead(v->expr()), &ebs);
    write(ebs, this->out);
    return unitv;
  }
private:
  bytes* out;
};

void encode(const MonoTypePtr& mt, std::vector<unsigned char>* out) {
  switchOf(mt, encodeMonoTypeF(out));
}

void encode(const QualTypePtr& qty, std::vector<unsigned char>* out) {
  if (isMonotype(qty)) {
    write(static_cast<size_t>(0), out);
    encode(qty->monoType(), out);
  } else {
    throw std::runtime_error("Qualified type serialization, NYI\n\t" + show(qty));
  }
}

MonoTypePtr decodeFrom(const bytes& in, unsigned int* n);

MonoTypePtr decodePrim(const bytes& in, unsigned int* n) {
  std::string pname   = read<std::string>(in, n);
  bool        hasHRep = read<bool>(in, n);
  MonoTypePtr hrep    = hasHRep ? decodeFrom(in, n) : MonoTypePtr();

  return Prim::make(pname, hrep);
}

MonoTypePtr decodeOpaquePtr(const bytes& in, unsigned int* n) {
  std::string  s  = read<std::string>(in, n);
  unsigned int sz = read<unsigned int>(in, n);
  bool         sc = read<bool>(in, n);

  return OpaquePtr::make(s, sz, sc);
}

MonoTypePtr decodeTGen(const bytes& in, unsigned int* n) {
  int i = read<int>(in, n);

  return TGen::make(i);
}

MonoTypePtr decodeTAbs(const bytes& in, unsigned int* n) {
  str::seq    ns = read<str::seq>(in, n);
  MonoTypePtr b  = decodeFrom(in, n);

  return TAbs::make(ns, b);
}

MonoTypePtr decodeTApp(const bytes& in, unsigned int* n) {
  MonoTypePtr f = decodeFrom(in, n);

  MonoTypes args;
  size_t sz = read<size_t>(in, n);
  for (size_t i = 0; i < sz; ++i) {
    args.push_back(decodeFrom(in, n));
  }

  return TApp::make(f, args);
}

MonoTypePtr decodeTVar(const bytes& in, unsigned int* n) {
  std::string v = read<std::string>(in, n);

  return TVar::make(v);
}

MonoTypePtr decodeFixedArr(const bytes& in, unsigned int* n) {
  MonoTypePtr ty  = decodeFrom(in, n);
  MonoTypePtr len = decodeFrom(in, n);

  return FixedArray::make(ty, len);
}

MonoTypePtr decodeArr(const bytes& in, unsigned int* n) {
  MonoTypePtr ty = decodeFrom(in, n);

  return Array::make(ty);
}

MonoTypePtr decodeVariant(const bytes& in, unsigned int* n) {
  Variant::Members ms;

  size_t c = read<size_t>(in, n);
  for (size_t i = 0; i < c; ++i) {
    std::string  ctor = read<std::string>(in, n);
    unsigned int id   = read<unsigned int>(in, n);
    MonoTypePtr  ty   = decodeFrom(in, n);

    ms.push_back(Variant::Member(ctor, ty, id));
  }

  return Variant::make(ms);
}

MonoTypePtr decodeRecord(const bytes& in, unsigned int* n) {
  Record::Members ms;

  size_t c = read<size_t>(in, n);
  for (size_t i = 0; i < c; ++i) {
    std::string  field  = read<std::string>(in, n);
    unsigned int offset = read<unsigned int>(in, n);
    MonoTypePtr  ty     = decodeFrom(in, n);

    ms.push_back(Record::Member(field, ty, offset));
  }

  return Record::make(ms);
}

MonoTypePtr decodeFunc(const bytes& in, unsigned int* n) {
  MonoTypePtr aty = decodeFrom(in, n);
  MonoTypePtr rty = decodeFrom(in, n);

  return Func::make(aty, rty);
}

MonoTypePtr decodeExists(const bytes& in, unsigned int* n) {
  std::string tn  = read<std::string>(in, n);
  MonoTypePtr aty = decodeFrom(in, n);

  return Exists::make(tn, aty);
}

MonoTypePtr decodeRecursive(const bytes& in, unsigned int* n) {
  std::string tn  = read<std::string>(in, n);
  MonoTypePtr rty = decodeFrom(in, n);

  return Recursive::make(tn, rty);
}

MonoTypePtr decodeTString(const bytes& in, unsigned int* n) {
  return TString::make(read<std::string>(in, n));
}

MonoTypePtr decodeTLong(const bytes& in, unsigned int* n) {
  return TLong::make(read<long>(in, n));
}

MonoTypePtr decodeTExpr(const bytes& in, unsigned int* n) {
  auto ebs = read<std::vector<uint8_t>>(in, n);
  ExprPtr e;
  decode(ebs, &e);
  return TExpr::make(e);
}

MonoTypePtr decodeFrom(const bytes& in, unsigned int* n) {
  switch (read<int>(in, n)) {
  case Prim::type_case_id:       return decodePrim(in, n);
  case OpaquePtr::type_case_id:  return decodeOpaquePtr(in, n);
  case TVar::type_case_id:       return decodeTVar(in, n);
  case TGen::type_case_id:       return decodeTGen(in, n);
  case TAbs::type_case_id:       return decodeTAbs(in, n);
  case TApp::type_case_id:       return decodeTApp(in, n);
  case FixedArray::type_case_id: return decodeFixedArr(in, n);
  case Array::type_case_id:      return decodeArr(in, n);
  case Variant::type_case_id:    return decodeVariant(in, n);
  case Record::type_case_id:     return decodeRecord(in, n);
  case Func::type_case_id:       return decodeFunc(in, n);
  case Exists::type_case_id:     return decodeExists(in, n);
  case Recursive::type_case_id:  return decodeRecursive(in, n);
  case TString::type_case_id:    return decodeTString(in, n);
  case TLong::type_case_id:      return decodeTLong(in, n);
  case TExpr::type_case_id:      return decodeTExpr(in, n);

  default:
    throw std::runtime_error("Malformed monotype encoding");
  }
}

MonoTypePtr decode(const std::vector<unsigned char>& in) {
  unsigned int n = 0;
  return decodeFrom(in, &n);
}

MonoTypePtr decode(const unsigned char* b, const unsigned char* e) {
  return decode(bytes(b, e));
}

void encode(const QualTypePtr& qty, std::ostream& out) {
  if (isMonotype(qty)) {
    encode(static_cast<size_t>(0), out);
    encode(qty->monoType(), out);
  } else {
    throw std::runtime_error("Qualified type serialization, NYI\n\t" + show(qty));
  }
}

void encode(const MonoTypePtr& mty, std::ostream& out) {
  std::vector<unsigned char> cs;
  encode(mty, &cs);
  encode(cs, out);
}

void decode(QualTypePtr* qty, std::istream& in) {
  size_t x = 0;
  decode(&x, in);

  MonoTypePtr mty;
  decode(&mty, in);

  *qty = qualtype(mty);
}

void decode(MonoTypePtr* mty, std::istream& in) {
  std::vector<unsigned char> cs;
  decode(&cs, in);

  if (cs.size() == 0) {
    throw std::runtime_error("Type data corrupted in source file");
  }

  *mty = decode(cs);
}

// open opaque type aliases not defined by this library
struct unaliasPrimTypesF : public switchTyFn {
  MonoTypePtr with(const Prim* t) const {
    if (t->representation()) {
      // well this is awkward ...
      if (t->name() != "time" && t->name() != "datetime" && t->name() != "timespan") {
        return switchOf(t->representation(), *this);
      }
    }
    return clone(t);
  }
};

MonoTypePtr unalias(const MonoTypePtr& ty) {
  if (isMonoSingular(ty)) {
    return switchOf(ty, unaliasPrimTypesF());
  } else {
    if (!ty->unaliasedType.get()) {
      ty->unaliasedType = switchOf(ty, unaliasPrimTypesF());
    }
    return ty->unaliasedType;
  }
}

MonoTypePtr makeFileRef(const MonoTypePtr& ty, const MonoTypePtr& f) {
  return TApp::make(primty("fileref"), list(ty, f));
}

}

