
#include <hobbes/lang/expr.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/util/array.H>
#include <hobbes/util/time.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/stream.H>
#include <sstream>

namespace hobbes {

Exprs clones(const Exprs& es) {
  Exprs r;
  for (Exprs::const_iterator e = es.begin(); e != es.end(); ++e) {
    r.push_back(ExprPtr((*e)->clone()));
  }
  return r;
}

std::string show(const Expr& e) {
  std::ostringstream ss;
  e.show(ss);
  return ss.str();
}
std::string show(const Expr* e)    { return show(*e); }
std::string show(const ExprPtr& e) { return show(*e); }

std::string show(const Definition& d) {
  return d.first + " = " + show(d.second);
}

inline void showTy(std::ostream& out, const QualTypePtr& qty) {
  if (qty != QualTypePtr()) {
    out << ":" << showNoSimpl(qty);
  }
}

Expr::Expr(int cid, const LexicalAnnotation& la) : LexicallyAnnotated(la), cid(cid) { }
Expr::~Expr() { }
const QualTypePtr& Expr::type() const { return this->annotatedType; }
void Expr::type(const QualTypePtr& ty) { this->annotatedType = ty; }
int Expr::case_id() const { return this->cid; }

Primitive::Primitive(int cid, const LexicalAnnotation& la) : Expr(cid, la) { }
bool PrimPtrLT::operator()(const PrimitivePtr& p0, const PrimitivePtr& p1) const { return *p0 < *p1; }

Unit::Unit(const LexicalAnnotation& la) : Base(la) { }
Expr* Unit::clone() const { return new Unit(la()); }
void Unit::show(std::ostream& out) const          { out << "()"; }
void Unit::showAnnotated(std::ostream& out) const { show(out); showTy(out, type()); }
bool Unit::equiv(const Unit&)               const { return true; }
bool Unit::lt(const Unit&)                  const { return false; }
MonoTypePtr Unit::primType() const { return MonoTypePtr(Prim::make("unit")); }

Bool::Bool(bool x, const LexicalAnnotation& la) : Base(la), x(x) { }
bool Bool::value() const { return this->x; }
void Bool::value(bool nx) { this->x = nx; }
Expr* Bool::clone() const { return new Bool(this->x,la()); }
void Bool::show(std::ostream& out)          const { out << (this->x ? "true" : "false"); }
void Bool::showAnnotated(std::ostream& out) const { show(out); showTy(out, type()); }
bool Bool::equiv(const Bool& rhs)           const { return this->x == rhs.x; }
bool Bool::lt(const Bool& rhs)              const { return this->x < rhs.x; }
MonoTypePtr Bool::primType() const { return MonoTypePtr(Prim::make("bool")); }

Char::Char(char x, const LexicalAnnotation& la) : Base(la), x(x) { }
char Char::value() const { return this->x; }
void Char::value(char nx) { this->x = nx; }
Expr* Char::clone() const { return new Char(this->x,la()); }
void Char::show(std::ostream& out)          const { out << "'" << this->x << "'"; }
void Char::showAnnotated(std::ostream& out) const { show(out); showTy(out, type()); }
bool Char::equiv(const Char& rhs)           const { return this->x == rhs.x; }
bool Char::lt(const Char& rhs)              const { return this->x < rhs.x; }
MonoTypePtr Char::primType() const { return MonoTypePtr(Prim::make("char")); }

Byte::Byte(unsigned char x, const LexicalAnnotation& la) : Base(la), x(x) { }
unsigned char Byte::value() const { return this->x; }
void Byte::value(unsigned char nx) { this->x = nx; }
Expr* Byte::clone() const { return new Byte(this->x,la()); }
void Byte::show(std::ostream& out)          const { out << str::hex(this->x); }
void Byte::showAnnotated(std::ostream& out) const { show(out); showTy(out, type()); }
bool Byte::equiv(const Byte& rhs)           const { return this->x == rhs.x; }
bool Byte::lt(const Byte& rhs)              const { return this->x < rhs.x; }
MonoTypePtr Byte::primType() const { return MonoTypePtr(Prim::make("byte")); }

Short::Short(short x, const LexicalAnnotation& la) : Base(la), x(x) { }
short Short::value() const { return this->x; }
void Short::value(short nx) { this->x = nx; }
Expr* Short::clone() const { return new Short(this->x,la()); }
void Short::show(std::ostream& out)          const { out << this->x << "S"; }
void Short::showAnnotated(std::ostream& out) const { show(out); showTy(out, type()); }
bool Short::equiv(const Short& rhs)          const { return this->x == rhs.x; }
bool Short::lt(const Short& rhs)             const { return this->x < rhs.x; }
MonoTypePtr Short::primType() const { return MonoTypePtr(Prim::make("short")); }

Int::Int(int x, const LexicalAnnotation& la) : Base(la), x(x) { }
int Int::value() const { return this->x; }
void Int::value(int nx) { this->x = nx; }
Expr* Int::clone() const { return new Int(this->x,la()); }
void Int::show(std::ostream& out)          const { out << this->x; }
void Int::showAnnotated(std::ostream& out) const { show(out); showTy(out, type()); }
bool Int::equiv(const Int& rhs)            const { return this->x == rhs.x; }
bool Int::lt(const Int& rhs)               const { return this->x < rhs.x; }
MonoTypePtr Int::primType() const { return MonoTypePtr(Prim::make("int")); }

Long::Long(long x, const LexicalAnnotation& la) : Base(la), x(x) { }
long Long::value() const { return this->x; }
void Long::value(long nx) { this->x = nx; }
Expr* Long::clone() const { return new Long(this->x,la()); }
void Long::show(std::ostream& out)          const { out << this->x << "L"; }
void Long::showAnnotated(std::ostream& out) const { show(out); showTy(out, type()); }
bool Long::equiv(const Long& rhs)           const { return this->x == rhs.x; }
bool Long::lt(const Long& rhs)              const { return this->x < rhs.x; }
MonoTypePtr Long::primType() const { return MonoTypePtr(Prim::make("long")); }

Int128::Int128(int128_t x, const LexicalAnnotation& la) : Base(la), x(x) { }
int128_t Int128::value() const { return this->x; }
void Int128::value(int128_t nx) { this->x = nx; }
Expr* Int128::clone() const { return new Int128(this->x,la()); }
void Int128::show(std::ostream& out)          const { printInt128(out, this->x); out << "H"; }
void Int128::showAnnotated(std::ostream& out) const { show(out); showTy(out, type()); }
bool Int128::equiv(const Int128& rhs)         const { return this->x == rhs.x; }
bool Int128::lt(const Int128& rhs)            const { return this->x < rhs.x; }
MonoTypePtr Int128::primType() const { return MonoTypePtr(Prim::make("int128")); }

Float::Float(float x, const LexicalAnnotation& la) : Base(la), x(x) { }
float Float::value() const { return this->x; }
void Float::value(float nx) { this->x = nx; }
Expr* Float::clone() const { return new Float(this->x,la()); }
void Float::show(std::ostream& out)          const { out << this->x << "e"; }
void Float::showAnnotated(std::ostream& out) const { show(out); showTy(out, type()); }
bool Float::equiv(const Float& rhs)          const { return this->x == rhs.x; }
bool Float::lt(const Float& rhs)             const { return this->x < rhs.x; }
MonoTypePtr Float::primType() const { return MonoTypePtr(Prim::make("float")); }

Double::Double(double x, const LexicalAnnotation& la) : Base(la), x(x) { }
double Double::value() const { return this->x; }
void Double::value(double nx) { this->x = nx; }
Expr* Double::clone() const { return new Double(this->x,la()); }
void Double::show(std::ostream& out)          const { out << this->x; }
void Double::showAnnotated(std::ostream& out) const { show(out); showTy(out, type()); }
bool Double::equiv(const Double& rhs)         const { return this->x == rhs.x; }
bool Double::lt(const Double& rhs)            const { return this->x < rhs.x; }
MonoTypePtr Double::primType() const { return MonoTypePtr(Prim::make("double")); }

Var::Var(const std::string& id, const LexicalAnnotation& la) : Base(la), id(id) { }
bool Var::operator==(const Var& rhs) const { return this->id == rhs.id; }
const std::string& Var::value() const { return this->id; }
void Var::value(const std::string& nid) { this->id = nid; }
Expr* Var::clone() const { return new Var(this->id,la()); }
void Var::show(std::ostream& out) const { out << this->id; }
void Var::showAnnotated(std::ostream& out) const { show(out); showTy(out, type()); }

Let::Let(const std::string& id, const ExprPtr& e, const ExprPtr& b, const LexicalAnnotation& la) : Base(la), id(id), e(e), b(b) { }
bool Let::operator==(const Let& rhs) const { return this->id == rhs.id && *this->e == *rhs.e && *this->b == *rhs.b; }

const std::string& Let::var()      const { return this->id; }
const ExprPtr&     Let::varExpr()  const { return this->e; }
const ExprPtr&     Let::bodyExpr() const { return this->b; }
Expr*              Let::clone()    const { return new Let(this->id, ExprPtr(this->e->clone()), ExprPtr(this->b->clone()),la()); }

void Let::var(const std::string&  nid) { this->id = nid; }
void Let::varExpr(const ExprPtr&  ne)  { this->e  = ne; }
void Let::bodyExpr(const ExprPtr& nb)  { this->b  = nb; }

void Let::show(std::ostream& out) const {
  out << "let " << this->id << " = ";
  this->e->show(out);
  out << " in ";
  this->b->show(out);
}

void Let::showAnnotated(std::ostream& out) const {
  out << "(let " << this->id << " = ";
  this->e->showAnnotated(out);
  out << " in ";
  this->b->showAnnotated(out);
  out << ")";
  showTy(out, type());
}

LetRec::LetRec(const Bindings& bs, const ExprPtr& e, const LexicalAnnotation& la) : Base(la), bs(bs), e(e) { }
const LetRec::Bindings& LetRec::bindings() const { return this->bs; }
const ExprPtr& LetRec::bodyExpr() const { return this->e; }
LetRec::Bindings& LetRec::bindings() { return this->bs; }
void LetRec::bodyExpr(const ExprPtr& e) { this->e = e; }

bool LetRec::operator==(const LetRec& rhs) const {
  if (this->bs.size() != rhs.bs.size() || !(*this->e == *rhs.e)) {
    return false;
  } else {
    for (size_t i = 0; i < this->bs.size(); ++i) {
      if ((this->bs[i].first != rhs.bs[i].first) || !(*this->bs[i].second == *rhs.bs[i].second)) {
        return false;
      }
    }
    return true;
  }
}

str::seq LetRec::varNames() const {
  str::seq r;
  for (const auto& b : this->bs) {
    r.push_back(b.first);
  }
  return r;
}

Expr* LetRec::clone() const {
  Bindings bs;
  for (const auto& b : this->bs) {
    bs.push_back(Binding(b.first, ExprPtr(b.second->clone())));
  }
  return new LetRec(bs, ExprPtr(this->e->clone()),la());
}

void LetRec::show(std::ostream& out) const {
  if (this->bs.size() == 0) {
    out << "letrec {} in ";
    this->e->show(out);
  } else {
    out << "letrec ";
    out << this->bs[0].first << " = ";
    this->bs[0].second->show(out);
    for (size_t i = 1; i < this->bs.size(); ++i) {
      out << "; " << this->bs[i].first << " = ";
      this->bs[i].second->show(out);
    }
    out << " in ";
    this->e->show(out);
  }
}

void LetRec::showAnnotated(std::ostream& out) const {
  if (this->bs.size() == 0) {
    out << "letrec {} in ";
    this->e->showAnnotated(out);
  } else {
    out << "letrec ";
    out << this->bs[0].first << " = ";
    this->bs[0].second->showAnnotated(out);
    for (size_t i = 1; i < this->bs.size(); ++i) {
      out << "; " << this->bs[i].first << " = ";
      this->bs[i].second->showAnnotated(out);
    }
    out << " in ";
    this->e->showAnnotated(out);
  }
}

Fn::Fn(const VarNames& vs, const ExprPtr& e, const LexicalAnnotation& la) : Base(la), vs(vs), e(e) {
  // to make type-checking homogeneous, we equate the empty parameter list with a parameter list containing just unit
  //   (unit is compiled away to nothing, so it's equivalent)
  if (this->vs.size() == 0) {
    this->vs.push_back("_");
  }
}
bool Fn::operator==(const Fn& rhs) const { return this->vs == rhs.vs && *this->e == *rhs.e; }
const Fn::VarNames& Fn::varNames() const { return this->vs; }
const ExprPtr& Fn::body() const { return this->e; }
Fn::VarNames& Fn::varNames() { return this->vs; }
void Fn::body(const ExprPtr& ne) { this->e = ne; }
Expr* Fn::clone() const { return new Fn(this->vs, ExprPtr(this->e->clone()),la()); }
void Fn::show(std::ostream& out) const {
  out << "\\(";
  if (this->vs[0] != "_") {
    out << this->vs[0];
    for (size_t i = 1; i < this->vs.size(); ++i) {
      out << ", ";
      out << this->vs[i];
    }
  }
  out << ").(";
  this->e->show(out);
  out << ")";
}
void Fn::showAnnotated(std::ostream& out) const {
  out << "(\\(";
  if (this->vs[0] != "_") {
    out << this->vs[0];
    for (size_t i = 1; i < this->vs.size(); ++i) {
      out << ", ";
      out << this->vs[i];
    }
  }
  out << ").(";
  this->e->showAnnotated(out);
  out << "))";
  showTy(out, type());
}

App::App(const ExprPtr& fne, const Exprs& argl, const LexicalAnnotation& la) : Base(la), fne(fne), argl(argl) {
  // to make type-checking homogeneous, we equate the empty parameter list with a parameter list containing just unit
  //   (unit is compiled away to nothing, so it's equivalent)
  if (this->argl.size() == 0) {
    this->argl.push_back(ExprPtr(new Unit(la)));
  }
}

bool App::operator==(const App& rhs) const {
  if (this->argl.size() != rhs.argl.size() || !(*this->fne == *rhs.fne)) {
    return false;
  } else {
    for (size_t i = 0; i < this->argl.size(); ++i) {
      if (!(*this->argl[i] == *rhs.argl[i])) {
        return false;
      }
    }
    return true;
  }
}

const ExprPtr& App::fn() const { return this->fne; }
const Exprs& App::args() const { return this->argl; }
void App::fn(const ExprPtr& nfne) { this->fne = nfne; }
Exprs& App::args() { return this->argl; }
Expr* App::clone() const { return new App(ExprPtr(this->fne->clone()), clones(this->argl),la()); }
void App::show(std::ostream& out) const {
  // show the function part
  if (Var* fnv = is<Var>(this->fne)) {
    out << fnv->value();
  } else {
    out << "(";
    this->fne->show(out);
    out << ")";
  }

  // show the argl part
  out << "(";
  if (this->argl.size() > 0) {
    Exprs::const_iterator e = this->argl.begin();
    (*e)->show(out);
    while (++e != this->argl.end()) {
      out << ", ";
      (*e)->show(out);
    }
  }
  out << ")";
}
void App::showAnnotated(std::ostream& out) const {
  // show the function part
  out << "(";
  this->fne->showAnnotated(out);
  out << ")";

  // show the argl part
  out << "(";
  if (this->argl.size() > 0) {
    Exprs::const_iterator e = this->argl.begin();
    (*e)->showAnnotated(out);
    while (++e != this->argl.end()) {
      out << ", ";
      (*e)->showAnnotated(out);
    }
  }
  out << ")";
  showTy(out, type());
}

Assign::Assign(const ExprPtr& lhs, const ExprPtr& rhs, const LexicalAnnotation& la) : Base(la), lhs(lhs), rhs(rhs) {
}
bool Assign::operator==(const Assign& rhs) const {
  return (*this->lhs == *rhs.lhs) && (*this->rhs == *rhs.rhs);
}
const ExprPtr& Assign::left()  const { return this->lhs; }
const ExprPtr& Assign::right() const { return this->rhs; }
void Assign::left(const ExprPtr&  nlhs) { this->lhs = nlhs; }
void Assign::right(const ExprPtr& nrhs) { this->rhs = nrhs; }
Expr* Assign::clone() const { return new Assign(ExprPtr(this->lhs->clone()), ExprPtr(this->rhs->clone()),la()); }

void Assign::show(std::ostream& out) const {
  this->lhs->show(out);
  out << " <- ";
  this->rhs->show(out);
}

void Assign::showAnnotated(std::ostream& out) const {
  this->lhs->showAnnotated(out);
  out << " <- ";
  this->rhs->showAnnotated(out);
}

MkArray::MkArray(const Exprs& es, const LexicalAnnotation& la) : Base(la), es(es) {
}
bool MkArray::operator==(const MkArray& rhs) const {
  if (this->es.size() != rhs.es.size()) {
    return false;
  } else {
    for (size_t i = 0; i < this->es.size(); ++i) {
      if (!(this->es[i] == rhs.es[i])) {
        return false;
      }
    }
    return true;
  }
}
const Exprs& MkArray::values() const { return this->es; }
Exprs& MkArray::values() { return this->es; }
Expr* MkArray::clone() const { return new MkArray(clones(this->es),la()); }
void MkArray::show(std::ostream& out) const {
  out << "[";
  if (this->es.size() > 0) {
    this->es[0]->show(out);
    for (size_t i = 1; i < this->es.size(); ++i) {
      out << ", ";
      this->es[i]->show(out);
    }
  }
  out << "]";
}
void MkArray::showAnnotated(std::ostream& out) const {
  out << "[";
  if (this->es.size() > 0) {
    this->es[0]->showAnnotated(out);
    for (size_t i = 1; i < this->es.size(); ++i) {
      out << ", ";
      this->es[i]->showAnnotated(out);
    }
  }
  out << "]";
  showTy(out, type());
}

MkVariant::MkVariant(const std::string& lbl, const ExprPtr& e, const LexicalAnnotation& la) : Base(la), lbl(lbl), e(e) { }
bool MkVariant::operator==(const MkVariant& rhs) const { return this->lbl == rhs.lbl && *this->e == *rhs.e; }
const std::string& MkVariant::label() const { return this->lbl; }
const ExprPtr& MkVariant::value() const { return this->e; }
void MkVariant::label(const std::string& nlbl) { this->lbl = nlbl; }
void MkVariant::value(const ExprPtr& ne) { this->e = ne; }
Expr* MkVariant::clone() const { return new MkVariant(this->lbl, ExprPtr(this->e->clone()),la()); }
void MkVariant::show(std::ostream& out) const {
  out << "|" << this->lbl << " = ";
  this->e->show(out);
  out << "|";
}
void MkVariant::showAnnotated(std::ostream& out) const {
  out << "|" << this->lbl << " = ";
  this->e->showAnnotated(out);
  out << "|";
  showTy(out, type());
}

MkRecord::MkRecord(const FieldDefs& fs, const LexicalAnnotation& la) : Base(la), fs(fs) { }
bool MkRecord::operator==(const MkRecord& rhs) const {
  if (this->fs.size() != rhs.fs.size()) {
    return false;
  } else {
    for (size_t i = 0; i < this->fs.size(); ++i) {
      if ((this->fs[i].first != rhs.fs[i].first) || !(*this->fs[i].second == *rhs.fs[i].second)) {
        return false;
      }
    }
    return true;
  }
}
const MkRecord::FieldDefs& MkRecord::fields() const { return this->fs; }
MkRecord::FieldDefs& MkRecord::fields() { return this->fs; }
bool MkRecord::isTuple() const { return this->fs.size() > 0 && this->fs[0].first.size() > 0 && this->fs[0].first[0] == '.'; }
Expr* MkRecord::clone() const {
  FieldDefs cfs;
  for (FieldDefs::const_iterator f = this->fs.begin(); f != this->fs.end(); ++f) {
    cfs.push_back(FieldDef(f->first, ExprPtr(f->second->clone())));
  }
  return new MkRecord(cfs,la());
}
void MkRecord::show(std::ostream& out) const {
  if (isTuple()) {
    showTuple(out);
  } else {
    showRecord(out);
  }
}
void MkRecord::showAnnotated(std::ostream& out) const {
  if (isTuple()) {
    showTupleAnnotated(out);
  } else {
    showRecordAnnotated(out);
  }
}
void MkRecord::showRecord(std::ostream& out) const {
  out << "{";
  if (this->fs.size() > 0) {
    out << this->fs[0].first << " = "; this->fs[0].second->show(out);
    for (size_t i = 1; i < this->fs.size(); ++i) {
      out << ", " << this->fs[i].first << " = "; this->fs[i].second->show(out);
    }
  }
  out << "}";
}
void MkRecord::showRecordAnnotated(std::ostream& out) const {
  out << "{";
  if (this->fs.size() > 0) {
    out << this->fs[0].first << " = "; this->fs[0].second->showAnnotated(out);
    for (size_t i = 1; i < this->fs.size(); ++i) {
      out << ", " << this->fs[i].first << " = "; this->fs[i].second->showAnnotated(out);
    }
  }
  out << "}";
  showTy(out, type());
}
void MkRecord::showTuple(std::ostream& out) const {
  out << "(";
  this->fs[0].second->show(out);
  for (size_t i = 1; i < this->fs.size(); ++i) {
    out << ", ";
    this->fs[i].second->show(out);
  }
  out << ")";
}
void MkRecord::showTupleAnnotated(std::ostream& out) const {
  out << "(";
  this->fs[0].second->showAnnotated(out);
  for (size_t i = 1; i < this->fs.size(); ++i) {
    out << ", ";
    this->fs[i].second->showAnnotated(out);
  }
  out << ")";
  showTy(out, type());
}

AIndex::AIndex(const ExprPtr& arr, const ExprPtr& i, const LexicalAnnotation& la) : Base(la), arr(arr), i(i) { }
bool AIndex::operator==(const AIndex& rhs) const { return *this->arr == *rhs.arr && *this->i == *rhs.i; }
const ExprPtr& AIndex::array() const { return this->arr; }
const ExprPtr& AIndex::index() const { return this->i; }
void AIndex::array(const ExprPtr& na) { this->arr = na; }
void AIndex::index(const ExprPtr& ni) { this->i   = ni; }
Expr* AIndex::clone() const { return new AIndex(ExprPtr(this->arr->clone()), ExprPtr(this->i->clone()),la()); }
void AIndex::show(std::ostream& out) const {
  out << "(";
  this->arr->show(out);
  out << ")[";
  this->i->show(out);
  out << "]";
}
void AIndex::showAnnotated(std::ostream& out) const {
  out << "(";
  this->arr->showAnnotated(out);
  out << ")[";
  this->i->showAnnotated(out);
  out << "]";
  showTy(out, type());
}

Case::Case(const ExprPtr& v, const Bindings& bs, const ExprPtr& def, const LexicalAnnotation& la) : Base(la), v(v), bs(bs), def(def) {
}

Case::Case(const ExprPtr& v, const Bindings& bs, const LexicalAnnotation& la) : Case(v, bs, ExprPtr(), la) {
}

bool Case::operator==(const Case& rhs) const {
  if ((this->bs.size() != rhs.bs.size()) || !(*this->v == *rhs.v)) {
    return false;
  } else {
    if (this->def.get()) {
      if (!rhs.def.get() || !(*this->def == *rhs.def)) {
        return false;
      }
    } else if (rhs.def.get()) {
      return false;
    }

    for (size_t i = 0; i < this->bs.size(); ++i) {
      if (
        (this->bs[i].selector != rhs.bs[i].selector) ||
        (this->bs[i].vname != rhs.bs[i].vname) ||
        !(*this->bs[i].exp == *rhs.bs[i].exp)
      ) {
        return false;
      }
    }
    return true;
  }
}

const ExprPtr&        Case::variant()     const { return this->v; }
const Case::Bindings& Case::bindings()    const { return this->bs; }
const ExprPtr&        Case::defaultExpr() const { return this->def; }

void            Case::variant(const ExprPtr& nv)       { this->v = nv; }
Case::Bindings& Case::bindings()                       { return this->bs; }
void            Case::defaultExpr(const ExprPtr& ndef) { this->def = ndef; }

bool Case::hasBinding(const std::string& bn) const {
  for (Bindings::const_iterator b = this->bs.begin(); b != this->bs.end(); ++b) {
    if (b->selector == bn) {
      return true;
    }
  }
  return false;
}
void Case::addBinding(const std::string& selector, const std::string& vname, const ExprPtr& exp) {
  this->bs.push_back(Case::Binding(selector, vname, exp));
}
Expr* Case::clone() const {
  Bindings cbs;
  for (Bindings::const_iterator b = this->bs.begin(); b != this->bs.end(); ++b) {
    cbs.push_back(Binding(b->selector, b->vname, ExprPtr(b->exp->clone())));
  }
  return new Case(ExprPtr(this->v->clone()), cbs, this->def.get() ? ExprPtr(this->def->clone()) : ExprPtr(),la());
}
void Case::show(std::ostream& out) const {
  out << "case (";
  this->v->show(out);
  out << ") {";
  for (Bindings::const_iterator b = this->bs.begin(); b != this->bs.end(); ++b) {
    if (b != this->bs.begin()) out << ", ";

    if (b->selector != b->vname) {
      out << b->selector << ":" << b->vname;
    } else {
      out << b->selector;
    }
    out << ".";
    b->exp->show(out);
  }
  if (this->def.get() != 0) {
    out << ",default.";
    this->def->show(out);
  }
  out << "}";
}
void Case::showAnnotated(std::ostream& out) const {
  out << "case (";
  this->v->showAnnotated(out);
  out << ") {";
  for (Bindings::const_iterator b = this->bs.begin(); b != this->bs.end(); ++b) {
    if (b != this->bs.begin()) out << ", ";

    if (b->selector != b->vname) {
      out << b->selector << ":" << b->vname;
    } else {
      out << b->selector;
    }
    out << ".";
    b->exp->showAnnotated(out);
  }
  out << "}";
  showTy(out, type());
}

Switch::Switch(const ExprPtr& v, const Bindings& bs, const ExprPtr& def, const LexicalAnnotation& la) : Base(la), v(v), bs(bs), def(def) {
  // ensure exhaustive selection
  // (only a few primitive types work with no default case)
  bool exhaustive = def != ExprPtr();

  if (bs.size() > 0) {
    switch (bs[0].value->case_id()) {
    case Unit::type_case_id:
      exhaustive = true;
      break;
    case Bool::type_case_id:
      exhaustive |= bs.size() == 2;
      break;
    case Byte::type_case_id:
      exhaustive |= bs.size() == 256;
      break;
    case Case::type_case_id:
      exhaustive |= bs.size() == 256;
      break;
    }
  }

  if (!exhaustive) {
    std::ostringstream ss;
    ss << "Inexhaustive switch coverage:\n  ";
    show(ss);
    throw annotated_error(la, ss.str());
  }

  // ensure unambiguous selection
  PrimitiveSet selectors;

  for (size_t i = 0; i < bs.size(); ++i) {
    if (selectors.find(bs[i].value) != selectors.end()) {
      std::ostringstream ss;
      ss << "Duplicate selector in switch expression:\n";
      show(ss);
      throw annotated_error(la, ss.str());
    } else {
      selectors.insert(bs[i].value);
    }
  }
}

Switch::Switch(const ExprPtr& v, const Bindings& bs, const LexicalAnnotation& la) : Switch(v, bs, ExprPtr(), la) {
}

bool Switch::operator==(const Switch& rhs) const {
  if (this->bs.size() != rhs.bs.size() || !(*this->v == *rhs.v)) {
    return false;
  } else {
    if (this->def.get()) {
      if (!rhs.def.get() || !(*this->def == *rhs.def)) {
        return false;
      }
    } else if (rhs.def.get()) {
      return false;
    }

    for (size_t i = 0; i < this->bs.size(); ++i) {
      if (
        !(*this->bs[i].value == *rhs.bs[i].value) ||
        !(*this->bs[i].exp   == *rhs.bs[i].exp)
      ) {
        return false;
      }
    }
    return true;
  }
}

const ExprPtr&          Switch::expr()        const { return this->v; }
const Switch::Bindings& Switch::bindings()    const { return this->bs; }
const ExprPtr&          Switch::defaultExpr() const { return this->def; }

void Switch::expr(const ExprPtr& x) { this->v = x; }
Switch::Bindings& Switch::bindings() { return this->bs; }
void Switch::defaultExpr(const ExprPtr& x) { this->def = x; }

Expr* Switch::clone() const {
  ExprPtr cdef = this->def ? ExprPtr(this->def->clone()) : ExprPtr();
  Bindings cbs;
  for (auto b : this->bs) {
    cbs.push_back(Binding(PrimitivePtr(reinterpret_cast<Primitive*>(b.value->clone())), ExprPtr(b.exp->clone())));
  }
  return new Switch(ExprPtr(this->v->clone()), cbs, cdef, la());
}

void Switch::show(std::ostream& out) const {
  out << "switch (";
  this->v->show(out);
  out << ") {";
  for (auto b : this->bs) {
    b.value->show(out);
    out << " => ";
    b.exp->show(out);
    out << "; ";
  }
  if (this->def) {
    out << "default => ";
    this->def->show(out);
    out << "; ";
  }
  out << "}";
}

void Switch::showAnnotated(std::ostream& out) const {
  out << "switch (";
  this->v->showAnnotated(out);
  out << ") {";
  for (auto b : this->bs) {
    b.value->show(out);
    out << " => ";
    b.exp->showAnnotated(out);
    out << "; ";
  }
  if (this->def) {
    out << "default => ";
    this->def->showAnnotated(out);
    out << "; ";
  }
  out << "}";
  showTy(out, type());
}


Proj::Proj(const ExprPtr& r, const std::string& fn, const LexicalAnnotation& la) : Base(la), r(r), fn(fn) { }
bool Proj::operator==(const Proj& rhs) const { return *this->r == *rhs.r && this->fn == rhs.fn; }
const ExprPtr& Proj::record() const { return this->r; }
const std::string& Proj::field() const { return this->fn; }
void Proj::record(const ExprPtr& nr) { this->r = nr; }
void Proj::field(const std::string& nfn) { this->fn = nfn; }
Expr* Proj::clone() const { return new Proj(ExprPtr(this->r->clone()), this->fn, la()); }

void Proj::show(std::ostream& out) const {
  this->r->show(out);
  out << "." << this->fn;
}
void Proj::showAnnotated(std::ostream& out) const {
  out << "(";
  this->r->showAnnotated(out);
  out << ")." << this->fn;
  showTy(out, type());
}

Assump::Assump(const ExprPtr& e, const QualTypePtr& t, const LexicalAnnotation& la) : Base(la), e(e), t(t) { }
bool Assump::operator==(const Assump& rhs) const { return *this->e == *rhs.e && *this->t == *rhs.t; }
Expr* Assump::clone() const { return new Assump(ExprPtr(this->e->clone()), hobbes::cloneP(this->t), la()); }
const ExprPtr& Assump::expr() const { return this->e; }
const QualTypePtr& Assump::ty() const { return this->t; }
void Assump::expr(const ExprPtr& ne) { this->e = ne; }
void Assump::ty(const QualTypePtr& nt) { this->t = nt; }

void Assump::show(std::ostream& out) const {
  this->e->show(out);
  out << "::";
  out << showNoSimpl(this->t);
}
void Assump::showAnnotated(std::ostream& out) const {
  out << "(";
  this->e->showAnnotated(out);
  out << ")::";
  out << showNoSimpl(this->t);
}

// existential introduction
Pack::Pack(const ExprPtr& e, const LexicalAnnotation& la) : Base(la), e(e) { }
bool Pack::operator==(const Pack& rhs) const { return *this->e == *rhs.e; }
Expr* Pack::clone() const { return new Pack(ExprPtr(this->e->clone()), la()); }
const ExprPtr& Pack::expr() const { return this->e; }
void Pack::expr(const ExprPtr& ne) { this->e = ne; }

void Pack::show(std::ostream& out) const {
  out << "pack ";
  this->e->show(out);
}
void Pack::showAnnotated(std::ostream& out) const {
  out << "pack (";
  this->e->showAnnotated(out);
  out << ")";
  showTy(out, type());
}

// existential elimination
Unpack::Unpack(const std::string& vn, const ExprPtr& pkg, const ExprPtr& body, const LexicalAnnotation& la) : Base(la), vn(vn), pkg(pkg), body(body) { }
bool Unpack::operator==(const Unpack& rhs) const { return this->vn == rhs.vn && *this->pkg == *rhs.pkg && *this->body == *rhs.body; }
Expr* Unpack::clone() const { return new Unpack(this->vn, ExprPtr(this->pkg->clone()), ExprPtr(this->body->clone()), la()); }
const std::string& Unpack::varName() const { return this->vn; }
const ExprPtr&     Unpack::package() const { return this->pkg; }
const ExprPtr&     Unpack::expr()    const { return this->body; }

void Unpack::varName(const std::string& nvn)   { this->vn   = nvn; }
void Unpack::package(const ExprPtr&     npkg)  { this->pkg  = npkg; }
void Unpack::expr   (const ExprPtr&     nbody) { this->body = nbody; }

void Unpack::show(std::ostream& out) const {
  out << "unpack ";
  this->pkg->show(out);
  out << " as " << this->vn << " in ";
  this->body->show(out);
}

void Unpack::showAnnotated(std::ostream& out) const {
  out << "unpack (";
  this->pkg->showAnnotated(out);
  out << ") as " << this->vn;
  if (this->pkg->type() != QualTypePtr()) {
    out << " ";
    showTy(out, unpackedType(this->pkg->type()));
  }
  out << " in ";
  this->body->showAnnotated(out);
}

// a convenience method for showing expressions with explicit type annotations (useful for debugging/testing the compiler)
std::string showAnnotated(const Expr& e) {
  std::ostringstream ss;
  e.showAnnotated(ss);
  return ss.str();
}

std::string showAnnotated(const Expr* e) {
  return showAnnotated(*e);
}

std::string showAnnotated(const ExprPtr& e) {
  return showAnnotated(*e);
}

std::string showAnnotated(const Definition& d) {
  return d.first + " = " + showAnnotated(d.second);
}

// replace variables in an expression with other expressions
struct substVarF : public switchExprC<ExprPtr> {
  const VarMapping& vm;
  bool* mapped;
  substVarF(const VarMapping& vm, bool* mapped) : vm(vm), mapped(mapped) { }

  ExprPtr withoutNames(const str::set&    ns, const ExprPtr& e) const { return switchOf(e, substVarF(drop(this->vm, ns), this->mapped)); }
  ExprPtr withoutNames(const str::seq&    ns, const ExprPtr& e) const { return withoutNames(toSet(ns), e); }
  ExprPtr withoutNames(const std::string& n,  const ExprPtr& e) const { return withoutNames(toSet(list(n)), e); }

  ExprPtr withConst(const Expr* v) const { return ExprPtr(v->clone()); }

  ExprPtr with(const Var* v) const {
    VarMapping::const_iterator ve = this->vm.find(v->value());
    if (ve == this->vm.end()) {
      return ExprPtr(v->clone());
    } else {
      if (mapped) *mapped = true;

      return ve->second;
    }
  }

  ExprPtr with(const Let* v) const {
    return ExprPtr(new Let(v->var(), switchOf(v->varExpr(), *this), withoutNames(v->var(), v->bodyExpr()), v->la()));
  }

  ExprPtr with(const LetRec* v) const {
    str::set vns = toSet(v->varNames());
    LetRec::Bindings bs;
    for (const auto& b : v->bindings()) {
      bs.push_back(LetRec::Binding(b.first, withoutNames(vns, b.second)));
    }
    return ExprPtr(new LetRec(bs, withoutNames(vns, v->bodyExpr()), v->la()));
  }

  ExprPtr with(const Fn* v) const {
    return ExprPtr(new Fn(v->varNames(), withoutNames(v->varNames(), v->body()), v->la()));
  }

  ExprPtr with(const App* v) const {
    return ExprPtr(new App(switchOf(v->fn(), *this), switchOf(v->args(), *this), v->la()));
  }

  ExprPtr with(const Assign* v) const {
    return ExprPtr(new Assign(switchOf(v->left(), *this), switchOf(v->right(), *this), v->la()));
  }

  ExprPtr with(const MkArray* v) const {
    return ExprPtr(new MkArray(switchOf(v->values(), *this), v->la()));
  }

  ExprPtr with(const MkVariant* v) const {
    return ExprPtr(new MkVariant(v->label(), switchOf(v->value(), *this), v->la()));
  }
  
  ExprPtr with(const MkRecord* v) const {
    return ExprPtr(new MkRecord(switchOf(v->fields(), *this), v->la()));
  }

  ExprPtr with(const AIndex* v) const {
    return ExprPtr(new AIndex(switchOf(v->array(), *this), switchOf(v->index(), *this), v->la()));
  }

  ExprPtr with(const Case* v) const {
    const Case::Bindings& cbs = v->bindings();
    Case::Bindings rcbs;
    for (Case::Bindings::const_iterator cb = cbs.begin(); cb != cbs.end(); ++cb) {
      rcbs.push_back(Case::Binding(cb->selector, cb->vname, withoutNames(cb->vname, cb->exp)));
    }
    ExprPtr de = v->defaultExpr();
    if (de.get()) {
      de = switchOf(de, *this);
    }
    return ExprPtr(new Case(switchOf(v->variant(), *this), rcbs, de, v->la()));
  }

  ExprPtr with(const Switch* v) const {
    Switch::Bindings rsbs;
    for (auto sb : v->bindings()) {
      rsbs.push_back(Switch::Binding(sb.value, switchOf(sb.exp, *this)));
    }
    ExprPtr de = v->defaultExpr();
    if (de) {
      de = switchOf(de, *this);
    }
    return ExprPtr(new Switch(switchOf(v->expr(), *this), rsbs, de, v->la()));
  }

  ExprPtr with(const Proj* v) const {
    return ExprPtr(new Proj(switchOf(v->record(), *this), v->field(), v->la()));
  }

  ExprPtr with(const Assump* v) const {
    return ExprPtr(new Assump(switchOf(v->expr(), *this), v->ty(), v->la()));
  }

  ExprPtr with(const Pack* v) const {
    return ExprPtr(new Pack(switchOf(v->expr(), *this), v->la()));
  }

  ExprPtr with(const Unpack* v) const {
    return ExprPtr(new Unpack(v->varName(), switchOf(v->package(), *this), withoutNames(v->varName(), v->expr()), v->la()));
  }
};

ExprPtr substitute(const VarMapping& vm, const ExprPtr& e, bool* mapped) {
  return switchOf(e, substVarF(vm, mapped));
}

struct substTyF : public switchExprTyFn {
  const MonoTypeSubst& s;
  substTyF(const MonoTypeSubst& s) : s(s) { }

  QualTypePtr withTy(const QualTypePtr& qt) const {
    if (qt) {
      return substitute(this->s, qt);
    } else {
      return qt;
    }
  }
};

ExprPtr substitute(const MonoTypeSubst& s, const ExprPtr& e) {
  return switchOf(e, substTyF(s));
}

struct isConstP : switchExprC<bool> {
  bool withConst(const Expr*)      const { return true; }
  bool with     (const Var*)       const { return false; }
  bool with     (const Let*)       const { return false; }
  bool with     (const LetRec*)    const { return false; }
  bool with     (const Fn*)        const { return false; }
  bool with     (const App*)       const { return false; }
  bool with     (const Assign*)    const { return false; }
  bool with     (const MkArray*)   const { return false; }
  bool with     (const MkVariant*) const { return false; }
  bool with     (const MkRecord*)  const { return false; }
  bool with     (const AIndex*)    const { return false; }
  bool with     (const Case*)      const { return false; }
  bool with     (const Switch*)    const { return false; }
  bool with     (const Proj*)      const { return false; }
  bool with     (const Assump*)    const { return false; }
  bool with     (const Pack*)      const { return false; }
  bool with     (const Unpack*)    const { return false; }
};

bool isConst(const ExprPtr& e) {
  return switchOf(e, isConstP());
}

// a convenient encapsulation of type-directed term transformation (e.g.: for unqualification)
QualTypePtr switchExprTyFn::withTy(const QualTypePtr& qt) const {
  return qt;
}

ExprPtr switchExprTyFn::wrapWithTy(const QualTypePtr& qty, Expr* e) const {
  e->type(withTy(qty));
  return ExprPtr(e);
}

Case::Bindings switchOf(const Case::Bindings& bs, const switchExpr<ExprPtr>& f) {
  Case::Bindings r;
  for (Case::Bindings::const_iterator b = bs.begin(); b != bs.end(); ++b) {
    r.push_back(Case::Binding(b->selector, b->vname, switchOf(b->exp, f)));
  }
  return r;
}

Switch::Bindings switchOf(const Switch::Bindings& bs, const switchExpr<ExprPtr>& f) {
  Switch::Bindings r;
  for (auto b : bs) {
    r.push_back(Switch::Binding(b.value, switchOf(b.exp, f)));
  }
  return r;
}

ExprPtr switchExprTyFn::withConst(const Expr* v)      const { return wrapWithTy(v->type(), v->clone()); }
ExprPtr switchExprTyFn::with     (const Var* v)       const { return wrapWithTy(v->type(), v->clone()); }
ExprPtr switchExprTyFn::with     (const Let* v)       const { return wrapWithTy(v->type(), new Let(v->var(), switchOf(v->varExpr(), *this), switchOf(v->bodyExpr(), *this), v->la())); }
ExprPtr switchExprTyFn::with     (const Fn* v)        const { return wrapWithTy(v->type(), new Fn(v->varNames(), switchOf(v->body(), *this), v->la())); }
ExprPtr switchExprTyFn::with     (const App* v)       const { return wrapWithTy(v->type(), new App(switchOf(v->fn(), *this), switchOf(v->args(), *this), v->la())); }
ExprPtr switchExprTyFn::with     (const Assign* v)    const { return wrapWithTy(v->type(), new Assign(switchOf(v->left(), *this), switchOf(v->right(), *this), v->la())); }
ExprPtr switchExprTyFn::with     (const MkArray* v)   const { return wrapWithTy(v->type(), new MkArray(switchOf(v->values(), *this), v->la())); }
ExprPtr switchExprTyFn::with     (const MkVariant* v) const { return wrapWithTy(v->type(), new MkVariant(v->label(), switchOf(v->value(), *this), v->la())); }
ExprPtr switchExprTyFn::with     (const MkRecord* v)  const { return wrapWithTy(v->type(), new MkRecord(switchOf(v->fields(), *this), v->la())); }
ExprPtr switchExprTyFn::with     (const AIndex* v)    const { return wrapWithTy(v->type(), new AIndex(switchOf(v->array(), *this), switchOf(v->index(), *this), v->la())); }
ExprPtr switchExprTyFn::with     (const Proj* v)      const { return wrapWithTy(v->type(), new Proj(switchOf(v->record(), *this), v->field(), v->la())); }
ExprPtr switchExprTyFn::with     (const Assump* v)    const { return wrapWithTy(v->type(), new Assump(switchOf(v->expr(), *this), withTy(v->ty()), v->la())); }
ExprPtr switchExprTyFn::with     (const Pack* v)      const { return wrapWithTy(v->type(), new Pack(switchOf(v->expr(), *this), v->la())); }
ExprPtr switchExprTyFn::with     (const Unpack* v)    const { return wrapWithTy(v->type(), new Unpack(v->varName(), switchOf(v->package(), *this), switchOf(v->expr(), *this), v->la())); }

ExprPtr switchExprTyFn::with(const LetRec* v) const {
  LetRec::Bindings bs;
  for (const auto& b : v->bindings()) {
    bs.push_back(LetRec::Binding(b.first, switchOf(b.second, *this)));
  }
  return wrapWithTy(v->type(), new LetRec(bs, switchOf(v->bodyExpr(), *this), v->la()));
}

ExprPtr switchExprTyFn::with(const Case* v) const {
  ExprPtr de = v->defaultExpr();
  if (de.get() != 0) { de = switchOf(de, *this); }

  return wrapWithTy(v->type(), new Case(switchOf(v->variant(), *this), switchOf(v->bindings(), *this), de, v->la()));
}

ExprPtr switchExprTyFn::with(const Switch* v) const {
  ExprPtr de = v->defaultExpr();
  if (de) { de = switchOf(de, *this); }

  return wrapWithTy(v->type(), new Switch(switchOf(v->expr(), *this), switchOf(v->bindings(), *this), de, v->la()));
}

// mutable type-translation in terms
UnitV switchOf(const Case::Bindings& bs, const switchExprTyFnM& f) {
  for (Case::Bindings::const_iterator b = bs.begin(); b != bs.end(); ++b) {
    switchOf(b->exp, const_cast<switchExprTyFnM&>(f));
  }
  return unitv;
}

UnitV switchOf(const Switch::Bindings& bs, const switchExprTyFnM& f) {
  for (auto b : bs) {
    switchOf(b.exp, const_cast<switchExprTyFnM&>(f));
  }
  return unitv;
}

QualTypePtr switchExprTyFnM::withTy(const QualTypePtr& qt) const {
  return qt;
}

UnitV switchExprTyFnM::updateTy(Expr* e) {
  if (e->type() != QualTypePtr()) {
    e->type(withTy(e->type()));
  }
  return unitv;
}

UnitV switchExprTyFnM::with(Unit* v) { return updateTy(v); }
UnitV switchExprTyFnM::with(Bool* v) { return updateTy(v); }
UnitV switchExprTyFnM::with(Char* v) { return updateTy(v); }
UnitV switchExprTyFnM::with(Byte* v) { return updateTy(v); }
UnitV switchExprTyFnM::with(Short* v) { return updateTy(v); }
UnitV switchExprTyFnM::with(Int* v) { return updateTy(v); }
UnitV switchExprTyFnM::with(Long* v) { return updateTy(v); }
UnitV switchExprTyFnM::with(Int128* v) { return updateTy(v); }
UnitV switchExprTyFnM::with(Float* v) { return updateTy(v); }
UnitV switchExprTyFnM::with(Double* v) { return updateTy(v); }
UnitV switchExprTyFnM::with(Var* v) { return updateTy(v); }

UnitV switchExprTyFnM::with(Let* v) {
  switchOf(v->varExpr(), *this);
  switchOf(v->bodyExpr(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(LetRec* v) {
  for (const auto& b : v->bindings()) {
    switchOf(b.second, *this);
  }
  switchOf(v->bodyExpr(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(Fn* v) {
  switchOf(v->body(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(App* v) {
  switchOf(v->fn(), *this);
  switchOf(v->args(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(Assign* v) {
  switchOf(v->left(), *this);
  switchOf(v->right(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(MkArray* v) {
  switchOf(v->values(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(MkVariant* v) {
  switchOf(v->value(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(MkRecord* v) {
  switchOf(v->fields(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(AIndex* v) {
  switchOf(v->array(), *this);
  switchOf(v->index(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(Case* v) {
  ExprPtr de = v->defaultExpr();
  if (de.get() != 0) { switchOf(de, *this); }

  switchOf(v->variant(), *this);
  switchOf(v->bindings(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(Switch* v) {
  ExprPtr de = v->defaultExpr();
  if (de) { switchOf(de, *this); }

  switchOf(v->expr(), *this);
  switchOf(v->bindings(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(Proj* v) {
  switchOf(v->record(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(Assump* v) {
  switchOf(v->expr(), *this);
  v->ty(withTy(v->ty()));
  return updateTy(v);
}

UnitV switchExprTyFnM::with(Pack* v) {
  switchOf(v->expr(), *this);
  return updateTy(v);
}

UnitV switchExprTyFnM::with(Unpack* v) {
  switchOf(v->package(), *this);
  switchOf(v->expr(), *this);
  return updateTy(v);
}

// lift any inferred type information to explicit assumptions (avoid losing type info over multiple runs of inference)
struct liftTypeAsAssumpF : public switchExprTyFn {
  const ExprPtr& setTy(const ExprPtr& e, const QualTypePtr& qty) const {
    e->type(qty);
    return e;
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    if (qty != QualTypePtr()) {
      return setTy(ExprPtr(new Assump(setTy(ExprPtr(e), qty), qty, e->la())), qty);
    } else {
      return ExprPtr(e);
    }
  }
};


unsolved_constraints::unsolved_constraints(const LexicalAnnotation&  la, const std::string& msg, const Constraints& cs) : annotated_error(la, msg), cs(cs) { }
unsolved_constraints::unsolved_constraints(const LexicallyAnnotated& la, const std::string& msg, const Constraints& cs) : annotated_error(la, msg), cs(cs) { }
const Constraints& unsolved_constraints::constraints() const { return this->cs; }

const MonoTypePtr& requireMonotype(const TEnvPtr& tenv, const ExprPtr& e) {
  if (e->type() == QualTypePtr()) {
    throw annotated_error(*e, "Expression '" + show(e) + "' not explicitly annotated.  Internal compiler error.");
  }

  if (e->type()->constraints().size() > 0) {
    Constraints cs = expandHiddenTCs(tenv, simplifyVarNames(e->type())->constraints());
    std::ostringstream ss;
    ss << "Failed to compile expression due to unresolved type constraint" << (cs.size() > 1 ? "s" : "");
    throw unsolved_constraints(*e, ss.str(), cs);
  }
  
  return e->type()->monoType();
}

MonoTypes requireMonotype(const TEnvPtr& tenv, const Exprs& es) {
  MonoTypes r;
  for (auto e : es) {
    r.push_back(requireMonotype(tenv, e));
  }
  return r;
}

ExprPtr liftTypesAsAssumptions(const ExprPtr& e) {
  return switchOf(e, liftTypeAsAssumpF());
}

struct stripAssumpF : public switchExprTyFn {
  ExprPtr with(const Assump* v) const {
    return switchOf(v->expr(), *this);
  }
};

ExprPtr stripExplicitAssumptions(const ExprPtr& e) {
  return switchOf(e, stripAssumpF());
}

const ExprPtr& stripAssumpHead(const ExprPtr& e) {
  if (const Assump* ae = is<Assump>(e)) {
    return stripAssumpHead(ae->expr());
  } else {
    return e;
  }
}

// find the set of free variables in an expression
struct freeVarF : public switchExprC<VarSet> {
  VarSet withConst(const Expr*)        const { return VarSet(); }
  VarSet with     (const Var* v)       const { return toSet(list(v->value())); }
  VarSet with     (const Let* v)       const { return setUnion(list(freeVars(v->varExpr()), setDifference(freeVars(v->bodyExpr()), toSet(list(v->var()))))); }
  VarSet with     (const Fn* v)        const { return setDifference(freeVars(v->body()), toSet(v->varNames())); }
  VarSet with     (const App* v)       const { return setUnion(cons(freeVars(v->fn()), switchOf(v->args(), *this))); }
  VarSet with     (const Assign* v)    const { return setUnion(list(freeVars(v->left()), freeVars(v->right()))); }
  VarSet with     (const MkArray* v)   const { return setUnion(switchOf(v->values(), *this)); }
  VarSet with     (const MkVariant* v) const { return freeVars(v->value()); }
  VarSet with     (const MkRecord* v)  const { return setUnion(switchOf(exprs(v->fields()), *this)); }
  VarSet with     (const AIndex* v)    const { return setUnion(list(freeVars(v->array()), freeVars(v->index()))); }
  VarSet with     (const Proj* v)      const { return freeVars(v->record()); }
  VarSet with     (const Assump* v)    const { return freeVars(v->expr()); }
  VarSet with     (const Pack* v)      const { return freeVars(v->expr()); }
  VarSet with     (const Unpack* v)    const { return setUnion(list(freeVars(v->package()), setDifference(freeVars(v->expr()), toSet(list(v->varName()))))); }
  
  VarSet with(const LetRec* v) const {
    std::vector<VarSet> fvs;
    str::set            vns = toSet(v->varNames());

    for (const auto& b : v->bindings()) {
      fvs.push_back(setDifference(freeVars(b.second), vns));
    }
    fvs.push_back(setDifference(freeVars(v->bodyExpr()), vns));

    return setUnion(fvs);
  }

  VarSet with(const Case* v) const {
    std::vector<VarSet> fvs;
    fvs.push_back(freeVars(v->variant()));
    for (Case::Bindings::const_iterator b = v->bindings().begin(); b != v->bindings().end(); ++b) {
      fvs.push_back(setDifference(freeVars(b->exp), toSet(list(b->vname))));
    }
    if (v->defaultExpr().get()) {
      fvs.push_back(freeVars(v->defaultExpr()));
    }
    return setUnion(fvs);
  }

  VarSet with(const Switch* v) const {
    std::vector<VarSet> fvs;
    fvs.push_back(freeVars(v->expr()));
    for (auto sb : v->bindings()) {
      fvs.push_back(freeVars(sb.exp));
    }
    if (v->defaultExpr()) {
      fvs.push_back(freeVars(v->defaultExpr()));
    }
    return setUnion(fvs);
  }
};

VarSet freeVars(const ExprPtr& e) {
  return switchOf(e, freeVarF());
}

VarSet freeVars(const Expr& e) {
  return switchOf(e, freeVarF());
}

// find the set of type variables in an expression
struct etvarNamesF : public switchExprC<UnitV> {
  NameSet* out;
  etvarNamesF(NameSet* out) : out(out) { }
  UnitV an(const Expr* v) const { if (v->type()) { tvarNames(v->type(), this->out); } return unitv; }

  UnitV withConst(const Expr*)        const { return unitv; }
  UnitV with     (const Var* v)       const { return an(v); }
  UnitV with     (const Let* v)       const { switchOf(v->varExpr(), *this); switchOf(v->bodyExpr(), *this); return an(v); }
  UnitV with     (const Fn* v)        const { switchOf(v->body(), *this); return an(v); }
  UnitV with     (const App* v)       const { switchOf(v->fn(), *this); switchOf(v->args(), *this); return an(v); }
  UnitV with     (const Assign* v)    const { switchOf(v->left(), *this); switchOf(v->right(), *this); return an(v); }
  UnitV with     (const MkArray* v)   const { switchOf(v->values(), *this); return an(v); }
  UnitV with     (const MkVariant* v) const { switchOf(v->value(), *this); return an(v); }
  UnitV with     (const MkRecord* v)  const { switchOf(exprs(v->fields()), *this); return an(v); }
  UnitV with     (const AIndex* v)    const { switchOf(v->array(), *this); switchOf(v->index(), *this); return an(v); }
  UnitV with     (const Proj* v)      const { switchOf(v->record(), *this); return an(v); }
  UnitV with     (const Assump* v)    const { switchOf(v->expr(), *this); tvarNames(v->ty(), this->out); return an(v); }
  UnitV with     (const Pack* v)      const { switchOf(v->expr(), *this); return an(v); }
  UnitV with     (const Unpack* v)    const { switchOf(v->package(), *this); switchOf(v->expr(), *this); return an(v); }
  
  UnitV with(const LetRec* v) const {
    for (const auto& b : v->bindings()) {
      switchOf(b.second, *this);
    }
    switchOf(v->bodyExpr(), *this);
    return an(v);
  }

  UnitV with(const Case* v) const {
    switchOf(v->variant(), *this);
    for (Case::Bindings::const_iterator b = v->bindings().begin(); b != v->bindings().end(); ++b) {
      switchOf(b->exp, *this);
    }
    if (v->defaultExpr().get()) {
      switchOf(v->defaultExpr(), *this);
    }
    return an(v);
  }

  UnitV with(const Switch* v) const {
    switchOf(v->expr(), *this);
    for (auto sb : v->bindings()) {
      switchOf(sb.exp, *this);
    }
    if (v->defaultExpr()) {
      switchOf(v->defaultExpr(), *this);
    }
    return an(v);
  }
};

NameSet tvarNames(const ExprPtr& e) {
  NameSet r;
  switchOf(e, etvarNamesF(&r));
  return r;
}

// desugar array comprehensions
ComprehensionDef::ComprehensionDef(const std::string& varname, const ExprPtr& ex) : isfilter(false), varname(varname), ex(ex) {
}

ComprehensionDef::ComprehensionDef(const ExprPtr& ex) : isfilter(true), ex(ex) {
}

ExprPtr desugarCompFilter(const ExprPtr& ex, const ExprPtr& filterE, const LexicalAnnotation& la) {
  return fncall(
    var("if", la),
    list(
      filterE,
      ExprPtr(new MkArray(list(ex), la)),
      ExprPtr(new MkArray(Exprs(), la))
    ),
    la
  );
}

ExprPtr desugarCompMap(const std::string& vn, const ExprPtr& ex, const ExprPtr& ae, const LexicalAnnotation& la) {
  return fncall(var("map", la), list(fn(vn, ex, la), ae), la);
}

ExprPtr desugarCompConcat(const ExprPtr& ex, const LexicalAnnotation& la) {
  return fncall(var("concat", la), list(ex), la);
}

ExprPtr desugarComprehensionFrom(const ExprPtr& ex, const ComprehensionDefs& cdefs, unsigned int i, const LexicalAnnotation& la) {
  i = i - 1; // translate from count to index

  const ComprehensionDef& cdef   = cdefs[i];
  ExprPtr                 nextEx = cdef.isfilter ? desugarCompFilter(ex, cdef.ex, la) : desugarCompMap(cdef.varname, ex, cdef.ex, la);

  if (i == 0) {
    return nextEx;
  } else {
    return desugarCompConcat(desugarComprehensionFrom(nextEx, cdefs, i, la), la);
  }
}

ExprPtr desugarComprehension(const ExprPtr& ex, const ComprehensionDefs& cdefs, const LexicalAnnotation& la) {
  return desugarComprehensionFrom(ex, cdefs, cdefs.size(), la);
}

// generate a format expression from a format string
ExprPtr fmtFoldConst(const ExprPtr& e, const std::string& c) {
  return fncall(var("append", e->la()), list(e, ExprPtr(mkarray(c, e->la()))), e->la());
}

ExprPtr fmtFoldExpr(const ExprPtr& e, const std::string& expr) {
  return fncall(var("append", e->la()), list(e, fncall(var("format", e->la()), list(var(expr, e->la())), e->la())), e->la());
}

ExprPtr mkFormatExpr(const std::string& fmt, const LexicalAnnotation& la) {
  if (fmt.size() == 0) {
    return ExprPtr(mkarray("", la));
  } else {
    return str::foldWithFormat(str::trimq(fmt, '`'), ExprPtr(mkarray("", la)), &fmtFoldConst, &fmtFoldExpr);
  }
}

PrimitivePtr mkTimespanPrim(const str::seq& x, const LexicalAnnotation& la) {
  return PrimitivePtr(new Long(readTimespan(x), la));
}
ExprPtr mkTimespanExpr(const str::seq& x, const LexicalAnnotation& la) {
  return assume(fncall(var("convert", la), list(ExprPtr(new Long(readTimespan(x), la))), la), qualtype(primty("timespan", primty("long"))), la);
}

PrimitivePtr mkTimePrim(const std::string& x, const LexicalAnnotation& la) {
  return PrimitivePtr(new Long(readTime(x), la));
}
ExprPtr mkTimeExpr(const std::string& x, const LexicalAnnotation& la) {
  return assume(fncall(var("convert", la), list(ExprPtr(new Long(readTime(x), la))), la), qualtype(primty("time", primty("long"))), la);
}

PrimitivePtr mkDateTimePrim(const std::string& x, const LexicalAnnotation& la) {
  return PrimitivePtr(new Long(readDateTime(x), la));
}
ExprPtr mkDateTimeExpr(const std::string& x, const LexicalAnnotation& la) {
  return assume(fncall(var("convert", la), list(ExprPtr(new Long(readDateTime(x), la))), la), qualtype(primty("datetime", primty("long"))), la);
}

// support a binary codec for expressions
void encode(const Case::Binding& b, std::ostream& out) {
  encode(b.selector, out);
  encode(b.vname,    out);
  encode(b.exp,      out);
}

void decode(Case::Binding* b, std::istream& in) {
  decode(&b->selector, in);
  decode(&b->vname,    in);
  decode(&b->exp,      in);
}

void encode(const Switch::Binding& b, std::ostream& out) {
  encode(b.value, out);
  encode(b.exp,   out);
}

void decode(Switch::Binding* b, std::istream& in) {
  decode(&b->value, in);
  decode(&b->exp,   in);
}

struct encodeExprF : public switchExpr<UnitV> {
  std::ostream& out;
  encodeExprF(std::ostream& out) : out(out) { }

  UnitV with(const Unit*) const {
    encode(Unit::type_case_id, this->out);
    return unitv;
  }

  UnitV with(const Bool* v) const {
    encode(Bool::type_case_id, this->out);
    encode(v->value(), this->out);
    return unitv;
  }

  UnitV with(const Char* v) const {
    encode(Char::type_case_id, this->out);
    encode(v->value(), this->out);
    return unitv;
  }

  UnitV with(const Byte* v) const {
    encode(Byte::type_case_id, this->out);
    encode(v->value(), this->out);
    return unitv;
  }

  UnitV with(const Short* v) const {
    encode(Short::type_case_id, this->out);
    encode(v->value(), this->out);
    return unitv;
  }

  UnitV with(const Int* v) const {
    encode(Int::type_case_id, this->out);
    encode(v->value(), this->out);
    return unitv;
  }

  UnitV with(const Long* v) const {
    encode(Long::type_case_id, this->out);
    encode(v->value(), this->out);
    return unitv;
  }

  UnitV with(const Int128* v) const {
    encode(Int128::type_case_id, this->out);
    encode(v->value(), this->out);
    return unitv;
  }

  UnitV with(const Float* v) const {
    encode(Float::type_case_id, this->out);
    encode(v->value(), this->out);
    return unitv;
  }

  UnitV with(const Double* v) const {
    encode(Double::type_case_id, this->out);
    encode(v->value(), this->out);
    return unitv;
  }

  UnitV with(const Var* v) const {
    encode(Var::type_case_id, this->out);
    encode(v->value(), this->out);
    return unitv;
  }
  
  UnitV with(const Let* v) const {
    encode(Let::type_case_id, this->out);
    encode(v->var(),      this->out);
    encode(v->varExpr(),  this->out);
    encode(v->bodyExpr(), this->out);
    return unitv;
  }

  UnitV with(const LetRec* v) const {
    encode(LetRec::type_case_id, this->out);
    encode(v->bindings(), this->out);
    encode(v->bodyExpr(), this->out);
    return unitv;
  }
  
  UnitV with(const Fn* v) const {
    encode(Fn::type_case_id, this->out);
    encode(v->varNames(), this->out);
    encode(v->body(),     this->out);
    return unitv;
  }
  
  UnitV with(const App* v) const {
    encode(App::type_case_id, this->out);
    encode(v->fn(),   this->out);
    encode(v->args(), this->out);
    return unitv;
  }
  
  UnitV with(const Assign* v) const {
    encode(Assign::type_case_id, this->out);
    encode(v->left(),  this->out);
    encode(v->right(), this->out);
    return unitv;
  }

  UnitV with(const MkArray* v) const {
    encode(MkArray::type_case_id, this->out);
    encode(v->values(), this->out);
    return unitv;
  }

  UnitV with(const MkVariant* v) const {
    encode(MkVariant::type_case_id, this->out);
    encode(v->label(), this->out);
    encode(v->value(), this->out);
    return unitv;
  }

  UnitV with(const MkRecord* v) const {
    encode(MkRecord::type_case_id, this->out);
    encode(v->fields(), this->out);
    return unitv;
  }

  UnitV with(const AIndex* v) const {
    encode(AIndex::type_case_id, this->out);
    encode(v->array(), this->out);
    encode(v->index(), this->out);
    return unitv;
  }

  UnitV with(const Case* v) const {
    encode(Case::type_case_id, this->out);
    encode(v->variant(), this->out);
    encode(v->bindings(), this->out);
    if (v->defaultExpr()) {
      encode(true,             this->out);
      encode(v->defaultExpr(), this->out);
    } else {
      encode(false, this->out);
    }
    return unitv;
  }
  
  UnitV with(const Switch* v) const {
    encode(Switch::type_case_id, this->out);
    encode(v->expr(), this->out);
    encode(v->bindings(), this->out);
    if (v->defaultExpr()) {
      encode(true, this->out);
      encode(v->defaultExpr(), this->out);
    } else {
      encode(false, this->out);
    }
    return unitv;
  }
 
  UnitV with(const Proj* v) const {
    encode(Proj::type_case_id, this->out);
    encode(v->record(), this->out);
    encode(v->field(),  this->out);
    return unitv;
  }
  
  UnitV with(const Assump* v) const {
    encode(Assump::type_case_id, this->out);
    encode(v->expr(), this->out);
    encode(v->ty(),   this->out);
    return unitv;
  }

  UnitV with(const Pack* v) const {
    encode(Pack::type_case_id, this->out);
    encode(v->expr(), this->out);
    return unitv;
  }
  
  UnitV with(const Unpack* v) const {
    encode(Unpack::type_case_id, this->out);
    encode(v->varName(), this->out);
    encode(v->package(), this->out);
    encode(v->expr(),    this->out);
    return unitv;
  }
};

void encode(const PrimitivePtr& p, std::ostream& out) {
  encode(reinterpret_cast<const ExprPtr&>(p), out);
}

void decode(PrimitivePtr* p, std::istream& in) {
  decode(reinterpret_cast<ExprPtr*>(p), in);
}

void encode(const ExprPtr& e, std::ostream& out) {
  try {
    switchOf(e, encodeExprF(out));

    if (e->type()) {
      encode(true, out);
      encode(e->type(), out);
    } else {
      encode(false, out);
    }
  } catch (std::exception& ex) {
    std::cout << "while encoding expression:" << std::endl << showAnnotated(e) << std::endl;
    throw;
  }
}

void decode(ExprPtr* out, std::istream& in) {
  int cid = 0;
  decode(&cid, in);

  ExprPtr& result = *out;

  switch (cid) {
  case Unit::type_case_id:
    result = ExprPtr(new Unit(LexicalAnnotation::null()));
    break;
  case Bool::type_case_id: {
    bool x = false;
    decode(&x, in);
    result = ExprPtr(new Bool(x, LexicalAnnotation::null()));
    break;
  }
  case Char::type_case_id: {
    char x = '\0';
    decode(&x, in);
    result = ExprPtr(new Char(x, LexicalAnnotation::null()));
    break;
  }
  case Byte::type_case_id: {
    unsigned char x = 0;
    decode(&x, in);
    result = ExprPtr(new Byte(x, LexicalAnnotation::null()));
    break;
  }
  case Short::type_case_id: {
    short x = 0;
    decode(&x, in);
    result = ExprPtr(new Short(x, LexicalAnnotation::null()));
    break;
  }
  case Int::type_case_id: {
    int x = 0;
    decode(&x, in);
    result = ExprPtr(new Int(x, LexicalAnnotation::null()));
    break;
  }
  case Long::type_case_id: {
    long x = 0;
    decode(&x, in);
    result = ExprPtr(new Long(x, LexicalAnnotation::null()));
    break;
  }
  case Int128::type_case_id: {
    int128_t x = 0;
    decode(&x, in);
    result = ExprPtr(new Int128(x, LexicalAnnotation::null()));
    break;
  }
  case Double::type_case_id: {
    double x = 0;
    decode(&x, in);
    result = ExprPtr(new Double(x, LexicalAnnotation::null()));
    break;
  }
  case Var::type_case_id: {
    std::string x;
    decode(&x, in);
    result = ExprPtr(new Var(x, LexicalAnnotation::null()));
    break;
  }
  case Let::type_case_id: {
    std::string vn;
    ExprPtr     e;
    ExprPtr     b;

    decode(&vn, in);
    decode(&e,  in);
    decode(&b,  in);

    result = ExprPtr(new Let(vn, e, b, LexicalAnnotation::null()));
    break;
  }
  case LetRec::type_case_id: {
    LetRec::Bindings bs;
    ExprPtr body;

    decode(&bs,   in);
    decode(&body, in);

    result = ExprPtr(new LetRec(bs, body, LexicalAnnotation::null()));
    break;
  }
  case Fn::type_case_id: {
    Fn::VarNames args;
    ExprPtr      b;

    decode(&args, in);
    decode(&b,    in);

    result = ExprPtr(new Fn(args, b, LexicalAnnotation::null()));
    break;
  }
  case App::type_case_id: {
    ExprPtr f;
    Exprs   args;

    decode(&f,    in);
    decode(&args, in);

    result = ExprPtr(new App(f, args, LexicalAnnotation::null()));
    break;
  }
  case Assign::type_case_id: {
    ExprPtr le;
    ExprPtr re;

    decode(&le, in);
    decode(&re, in);

    result = ExprPtr(new Assign(le, re, LexicalAnnotation::null()));
    break;
  }
  case MkArray::type_case_id: {
    Exprs es;
    decode(&es, in);
    result = ExprPtr(new MkArray(es, LexicalAnnotation::null()));
    break;
  }
  case MkVariant::type_case_id: {
    std::string lbl;
    ExprPtr     v;

    decode(&lbl, in);
    decode(&v,   in);

    result = ExprPtr(new MkVariant(lbl, v, LexicalAnnotation::null()));
    break;
  }
  case MkRecord::type_case_id: {
    MkRecord::FieldDefs fs;
    decode(&fs, in);
    result = ExprPtr(new MkRecord(fs, LexicalAnnotation::null()));
    break;
  }
  case AIndex::type_case_id: {
    ExprPtr a;
    ExprPtr i;

    decode(&a, in);
    decode(&i, in);

    result = ExprPtr(new AIndex(a, i, LexicalAnnotation::null()));
    break;
  }
  case Case::type_case_id: {
    ExprPtr v;
    Case::Bindings bs;

    decode(&v,  in);
    decode(&bs, in);
    
    bool hasDef = false;
    decode(&hasDef, in);
    if (hasDef) {
      ExprPtr d;
      decode(&d, in);

      result = ExprPtr(new Case(v, bs, d, LexicalAnnotation::null()));
    } else {
      result = ExprPtr(new Case(v, bs, LexicalAnnotation::null()));
    }
    break;
  }
  case Switch::type_case_id: {
    ExprPtr v;
    Switch::Bindings bs;

    decode(&v, in);
    decode(&bs, in);

    bool hasDef = false;
    decode(&hasDef, in);
    if (hasDef) {
      ExprPtr d;
      decode(&d, in);

      result = ExprPtr(new Switch(v, bs, d, LexicalAnnotation::null()));
    } else {
      result = ExprPtr(new Switch(v, bs, LexicalAnnotation::null()));
    }
    break;
  }
 
  case Proj::type_case_id: {
    ExprPtr     r;
    std::string f;

    decode(&r, in);
    decode(&f, in);

    result = ExprPtr(new Proj(r, f, LexicalAnnotation::null()));
    break;
  }
  case Assump::type_case_id: {
    ExprPtr e;
    QualTypePtr t;

    decode(&e, in);
    decode(&t, in);

    result = ExprPtr(new Assump(e, t, LexicalAnnotation::null()));
    break;
  }
  case Pack::type_case_id: {
    ExprPtr e;
    decode(&e, in);
    result = ExprPtr(new Pack(e, LexicalAnnotation::null()));
    break;
  }
  case Unpack::type_case_id: {
    std::string vn;
    ExprPtr     p;
    ExprPtr     e;

    decode(&vn, in);
    decode(&p,  in);
    decode(&e,  in);

    result = ExprPtr(new Unpack(vn, p, e, LexicalAnnotation::null()));
    break;
  }
  default:
    throw std::runtime_error("Internal error, cannot switch on unknown expression type: " + str::from(cid));
  }

  // pull out the annotated type if it's there
  bool hasType = false;
  decode(&hasType, in);

  if (hasType) {
    QualTypePtr qty;
    decode(&qty, in);
    result->type(qty);
  }
}

// also encode/decode to/from raw byte arrays
void encode(const ExprPtr& e, std::vector<uint8_t>* d) {
  stream::raw_ostream<char> ds(d);
  encode(e, ds);
}

void decode(const std::vector<uint8_t>& d, ExprPtr* e) {
  stream::raw_istream<char> ds(d);
  return decode(e, ds);
}

// determine whether or not an expression is fully annotated everywhere with a singular type
struct hasSingularTypeF : public switchExprC<bool> {
  bool withConst(const Expr* v) const {
    return d(v);
  }

  bool with(const Var* v) const {
    return d(v);
  }

  bool with(const Let* v) const {
    return d(v) && r(v->varExpr()) && r(v->bodyExpr());
  }

  bool with(const LetRec* v) const {
    if (!d(v) || !r(v->bodyExpr())) return false;

    for (auto b : v->bindings()) {
      if (!r(b.second)) {
        return false;
      }
    }
    return true;
  }

  bool with(const Fn* v) const {
    return d(v) && r(v->body());
  }

  bool with(const App* v) const {
    if (!d(v) || !r(v->fn())) return false;

    for (auto a : v->args()) {
      if (!r(a)) {
        return false;
      }
    }
    return true;
  }

  bool with(const Assign* v) const {
    return d(v) && r(v->left()) && r(v->right());
  }

  bool with(const MkArray* v) const {
    if (!d(v)) return false;
    for (auto val : v->values()) {
      if (!r(val)) return false;
    }
    return true;
  }

  bool with(const MkVariant* v) const {
    return d(v) && r(v->value());
  }

  bool with(const MkRecord* v) const {
    if (!d(v)) return false;
    for (auto f : v->fields()) {
      if (!r(f.second)) return false;
    }
    return true;
  }

  bool with(const AIndex* v) const {
    return d(v) && r(v->array()) && r(v->index());
  }

  bool with(const Case* v) const {
    if (!d(v) || !r(v->variant()) || (v->defaultExpr() && !r(v->defaultExpr()))) return false;
    for (auto b : v->bindings()) {
      if (!r(b.exp)) return false;
    }
    return true;
  }

  bool with(const Switch* v) const {
    if (!d(v) || !r(v->expr()) || (v->defaultExpr() && !r(v->defaultExpr()))) return false;
    for (auto b : v->bindings()) {
      if (!r(b.exp)) return false;
    }
    return true;
  }

  bool with(const Proj* v) const {
    return d(v) && r(v->record());
  }

  bool with(const Assump* v) const {
    return d(v) && r(v->expr()) && isMonoSingular(v->ty());
  }

  bool with(const Pack* v) const {
    return d(v) && r(v->expr());
  }

  bool with(const Unpack* v) const {
    return d(v) && r(v->package()) && r(v->expr());
  }
private:
  bool d(const Expr*    v) const { return v->type().get() && isMonoSingular(v->type()); }
  bool r(const ExprPtr& v) const { return switchOf(v, *this); }
};

bool hasSingularType(const ExprPtr& e) {
  return switchOf(e, hasSingularTypeF());
}

// determine the tgen size of types across an expression
struct tgenSizeExprF : public switchExprC<int> {
  int withConst(const Expr* v) const {
    return d(v);
  }

  int with(const Var* v) const {
    return d(v);
  }

  int with(const Let* v) const {
    return c(c(d(v), r(v->varExpr())), r(v->bodyExpr()));
  }

  int with(const LetRec* v) const {
    int x = c(d(v), r(v->bodyExpr()));
    for (auto b : v->bindings()) {
      x = c(x, r(b.second));
    }
    return x;
  }

  int with(const Fn* v) const {
    return c(d(v), r(v->body()));
  }

  int with(const App* v) const {
    int x = c(d(v), r(v->fn()));
    for (auto a : v->args()) {
      x = c(x, r(a));
    }
    return x;
  }

  int with(const Assign* v) const {
    return c(c(d(v), r(v->left())), r(v->right()));
  }

  int with(const MkArray* v) const {
    int x = d(v);
    for (auto val : v->values()) {
      x = c(x, r(val));
    }
    return x;
  }

  int with(const MkVariant* v) const {
    return c(d(v), r(v->value()));
  }

  int with(const MkRecord* v) const {
    int x = d(v);
    for (auto f : v->fields()) {
      x = c(x, r(f.second));
    }
    return x;
  }

  int with(const AIndex* v) const {
    return c(c(d(v), r(v->array())), r(v->index()));
  }

  int with(const Case* v) const {
    int x = c(d(v), r(v->variant()));
    if (v->defaultExpr()) x = c(x, r(v->defaultExpr()));
    for (auto b : v->bindings()) {
      x = c(x, r(b.exp));
    }
    return x;
  }

  int with(const Switch* v) const {
    int x = c(d(v), r(v->expr()));
    if (v->defaultExpr()) x = c(x, r(v->defaultExpr()));
    for (auto b : v->bindings()) {
      x = c(x, r(b.exp));
    }
    return x;
  }

  int with(const Proj* v) const {
    return c(d(v), r(v->record()));
  }

  int with(const Assump* v) const {
    return c(c(d(v), r(v->expr())), tgenSize(v->ty()));
  }

  int with(const Pack* v) const {
    return c(d(v), r(v->expr()));
  }

  int with(const Unpack* v) const {
    return c(c(d(v), r(v->package())), r(v->expr()));
  }
private:
  int d(const Expr*    v) const { return v->type().get() ? tgenSize(v->type()) : 0; }
  int r(const ExprPtr& v) const { return switchOf(v, *this); }
  int c(int x, int y)     const { return std::max<int>(x, y); }
};

int tgenSize(const ExprPtr& e) {
  return switchOf(e, tgenSizeExprF());
}

}

