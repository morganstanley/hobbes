#ifndef HOBBES_LANG_EXPR_HPP_INCLUDED
#define HOBBES_LANG_EXPR_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/util/ptr.H>
#include <hobbes/util/array.H>
#include <hobbes/util/lannotation.H>
#include <memory>
#include <iostream>
#include <string>
#include <vector>

namespace hobbes {

typedef __int128 int128_t;

class Expr : public LexicallyAnnotated {
public:
  virtual ~Expr();

  virtual void show(std::ostream&) const = 0;
  virtual void showAnnotated(std::ostream&) const = 0;
  virtual Expr* clone() const = 0;
  virtual bool operator==(const Expr&) const = 0;

  // for use by type-inference, to explicitly annotate every term with its type
  const QualTypePtr& type() const;
  void type(const QualTypePtr& ty);
private:
  QualTypePtr annotatedType;

  // improves performance of case-analysis over instances (to avoid 'dynamic_cast')
public:
  int case_id() const;
protected:
  Expr(int cid, const LexicalAnnotation&);
private:
  int cid;
};

// improves performance of case-analysis over MonoType instances (to avoid 'dynamic_cast')
template <typename Case>
  class ExprCase : public Expr {
  public:
    typedef ExprCase<Case> Base;
    ExprCase(const LexicalAnnotation&);
    virtual bool operator==(const Case&) const = 0;

    bool operator==(const Expr& rhs) const {
      if (this == &rhs) {
        return true;
      } else if (const Case* trhs = is<Case>(&rhs)) {
        return *this == *trhs;
      } else {
        return false;
      }
    }
  };

typedef std::shared_ptr<Expr> ExprPtr;
typedef std::vector<ExprPtr>  Exprs;

typedef std::pair<std::string, ExprPtr> Definition;
typedef std::vector<Definition>         Definitions;

std::string show(const Expr& e);
std::string show(const Expr* e);
std::string show(const ExprPtr& e);
std::string show(const Definition& d);
std::string showAnnotated(const Expr& e);
std::string showAnnotated(const Expr* e);
std::string showAnnotated(const ExprPtr& e);
std::string showAnnotated(const Definition& d);

//////////////////
// primitive constants
//////////////////

class Primitive : public Expr {
public:
  virtual bool operator==(const Expr&) const = 0;
  virtual bool operator< (const Primitive&) const = 0;
  virtual MonoTypePtr primType() const = 0;

  // for efficient case dispatch
  Primitive(int cid, const LexicalAnnotation&);
};
typedef std::shared_ptr<Primitive> PrimitivePtr;

struct PrimPtrLT {
  bool operator()(const PrimitivePtr&, const PrimitivePtr&) const;
};
typedef std::set<PrimitivePtr, PrimPtrLT> PrimitiveSet;

template <typename Case>
  class PrimitiveCase : public Primitive {
  public:
    typedef PrimitiveCase<Case> Base;
    PrimitiveCase(const LexicalAnnotation&);
    virtual bool equiv(const Case&) const = 0;
    virtual bool lt(const Case&) const = 0;

    bool operator<(const Primitive& rhs) const {
      if (const Case* trhs = is<Case>(&rhs)) {
        return lt(*trhs);
      } else {
        return case_id() < rhs.case_id();
      }
    }

    bool operator==(const Expr& rhs) const {
      if (const Case* trhs = is<Case>(&rhs)) {
        return this->equiv(*trhs);
      } else {
        return false;
      }
    }
  };

class Unit : public PrimitiveCase<Unit> {
public:
  Unit(const LexicalAnnotation&);
  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;
  bool equiv(const Unit&) const;
  bool lt(const Unit&) const;
  MonoTypePtr primType() const;

  static const int type_case_id = 0;
};

class Bool : public PrimitiveCase<Bool> {
public:
  Bool(bool x, const LexicalAnnotation&);
  bool value() const;
  void value(bool);
  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;
  bool equiv(const Bool&) const;
  bool lt(const Bool&) const;
  MonoTypePtr primType() const;

  static const int type_case_id = 1;
private:
  bool x;
};

class Char : public PrimitiveCase<Char> {
public:
  Char(char x, const LexicalAnnotation&);
  char value() const;
  void value(char);
  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;
  bool equiv(const Char&) const;
  bool lt(const Char&) const;
  MonoTypePtr primType() const;

  static const int type_case_id = 2;
private:
  char x;
};

// 8-bit int
class Byte : public PrimitiveCase<Byte> {
public:
  Byte(unsigned char x, const LexicalAnnotation&);
  unsigned char value() const;
  void value(unsigned char);
  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;
  bool equiv(const Byte&) const;
  bool lt(const Byte&) const;
  MonoTypePtr primType() const;

  static const int type_case_id = 3;
private:
  unsigned char x;
};

// 16-bit int
class Short : public PrimitiveCase<Short> {
public:
  Short(short x, const LexicalAnnotation&);
  short value() const;
  void value(short);
  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;
  bool equiv(const Short&) const;
  bool lt(const Short&) const;
  MonoTypePtr primType() const;

  static const int type_case_id = 4;
private:
  short x;
};

// 32-bit int
class Int : public PrimitiveCase<Int> {
public:
  Int(int x, const LexicalAnnotation&);
  int value() const;
  void value(int);
  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;
  bool equiv(const Int&) const;
  bool lt(const Int&) const;
  MonoTypePtr primType() const;

  static const int type_case_id = 5;
private:
  int x;
};

// 64-bit int
class Long : public PrimitiveCase<Long> {
public:
  Long(long x, const LexicalAnnotation&);
  long value() const;
  void value(long);
  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;
  bool equiv(const Long&) const;
  bool lt(const Long&) const;
  MonoTypePtr primType() const;

  static const int type_case_id = 6;
private:
  long x;
};

// 32-bit float
class Float : public PrimitiveCase<Float> {
public:
  Float(float x, const LexicalAnnotation&);
  float value() const;
  void value(float);
  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;
  bool equiv(const Float&) const;
  bool lt(const Float&) const;
  MonoTypePtr primType() const;

  static const int type_case_id = 7;
private:
  float x;
};

// 64-bit float
class Double : public PrimitiveCase<Double> {
public:
  Double(double x, const LexicalAnnotation&);
  double value() const;
  void value(double);
  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;
  bool equiv(const Double&) const;
  bool lt(const Double&) const;
  MonoTypePtr primType() const;

  static const int type_case_id = 8;
private:
  double x;
};

template <typename Case>
  PrimitiveCase<Case>::PrimitiveCase(const LexicalAnnotation& la) : Primitive(Case::type_case_id, la) {
  }

///////////////////////////////////////////////////
// non-primitive terms
///////////////////////////////////////////////////

// v (variable reference)
class Var : public ExprCase<Var> {
public:
  Var(const std::string& id, const LexicalAnnotation&);
  bool operator==(const Var&) const;

  const std::string& value() const;
  void value(const std::string&);
  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 9;
private:
  std::string id;
};

// local variable definition
class Let : public ExprCase<Let> {
public:
  Let(const std::string& id, const ExprPtr& e, const ExprPtr& b, const LexicalAnnotation&);
  bool operator==(const Let&) const;

  const std::string& var() const;
  const ExprPtr&     varExpr() const;
  const ExprPtr&     bodyExpr() const;

  void var(const std::string&);
  void varExpr(const ExprPtr&);
  void bodyExpr(const ExprPtr&);

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 10;
private:
  std::string id;
  ExprPtr     e;
  ExprPtr     b;
};

// local mutually-recursive function definition
class LetRec : public ExprCase<LetRec> {
public:
  typedef std::pair<std::string, ExprPtr> Binding;
  typedef std::vector<Binding> Bindings;

  LetRec(const Bindings&, const ExprPtr&, const LexicalAnnotation&);
  bool operator==(const LetRec&) const;

  const Bindings& bindings() const;
  const ExprPtr& bodyExpr() const;
  str::seq varNames() const;

  Bindings& bindings();
  void bodyExpr(const ExprPtr&);

  Expr* clone() const;
  void show(std::ostream&) const;
  void showAnnotated(std::ostream&) const;

  static const int type_case_id = 11;
private:
  Bindings bs;
  ExprPtr  e;
};

// \x0...xn -> E (function introduction)
class Fn : public ExprCase<Fn> {
public:
  typedef std::vector<std::string> VarNames;

  Fn(const VarNames& vs, const ExprPtr& e, const LexicalAnnotation&);
  bool operator==(const Fn&) const;

  const VarNames& varNames() const;
  const ExprPtr&  body() const;

  VarNames& varNames();
  void body(const ExprPtr&);

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 12;
private:
  VarNames vs;
  ExprPtr  e;
};

// E(E0...EN) (function elimination)
class App : public ExprCase<App> {
public:
  App(const ExprPtr& fn, const Exprs& args, const LexicalAnnotation&);
  bool operator==(const App&) const;

  const ExprPtr& fn() const;
  const Exprs& args() const;

  void fn(const ExprPtr&);
  Exprs& args();

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 13;
private:
  ExprPtr fne;
  Exprs   argl;
};

// E <- E (storage assignment)
class Assign : public ExprCase<Assign> {
public:
  Assign(const ExprPtr& lhs, const ExprPtr& rhs, const LexicalAnnotation&);
  bool operator==(const Assign&) const;

  const ExprPtr& left() const;
  const ExprPtr& right() const;

  void left(const ExprPtr&);
  void right(const ExprPtr&);

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 14;
private:
  ExprPtr lhs;
  ExprPtr rhs;
};

// [E0,...,EN] (fixed-length array introduction)
class MkArray : public ExprCase<MkArray> {
public:
  MkArray(const Exprs& es, const LexicalAnnotation&);
  bool operator==(const MkArray&) const;

  const Exprs& values() const;
  Exprs& values();

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 15;
private:
  Exprs es;
};

// |x=E| (variant introduction)
class MkVariant : public ExprCase<MkVariant> {
public:
  MkVariant(const std::string& lbl, const ExprPtr& e, const LexicalAnnotation&);
  bool operator==(const MkVariant&) const;

  const std::string& label() const;
  const ExprPtr& value() const;

  void label(const std::string&);
  void value(const ExprPtr&);

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 16;
private:
  std::string lbl;
  ExprPtr     e;
};

// {f0=E0,...,fN=EN} (record introduction)
class MkRecord : public ExprCase<MkRecord> {
public:
  typedef std::pair<std::string, ExprPtr> FieldDef;
  typedef std::vector<FieldDef>           FieldDefs;

  MkRecord(const FieldDefs& fs, const LexicalAnnotation&);
  bool operator==(const MkRecord&) const;

  const FieldDefs& fields() const;
  FieldDefs& fields();

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 17;
private:
  FieldDefs fs;

  bool isTuple() const;
  void showRecord(std::ostream& out) const;
  void showRecordAnnotated(std::ostream& out) const;
  void showTuple(std::ostream& out) const;
  void showTupleAnnotated(std::ostream& out) const;
};

// E[i] (array index)
class AIndex : public ExprCase<AIndex> {
public:
  AIndex(const ExprPtr&, const ExprPtr&, const LexicalAnnotation&);
  bool operator==(const AIndex&) const;

  const ExprPtr& array() const;
  const ExprPtr& index() const;
  
  void array(const ExprPtr&);
  void index(const ExprPtr&);

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 18;
private:
  ExprPtr arr;
  ExprPtr i;
};

// case E of |c0:x0=>E0;...;cN:xN=>EN| (variant case analysis)
class Case : public ExprCase<Case> {
public:
  struct Binding {
    Binding() { }
    Binding(const std::string& selector, const std::string& vname, const ExprPtr& exp) : selector(selector), vname(vname), exp(exp) { }

    std::string selector;
    std::string vname;
    ExprPtr     exp;
  };
  typedef std::vector<Binding> Bindings;

  Case(const ExprPtr& v, const Bindings& bs, const LexicalAnnotation&);
  Case(const ExprPtr& v, const Bindings& bs, const ExprPtr& def, const LexicalAnnotation&);
  bool operator==(const Case&) const;

  const ExprPtr&  variant() const;
  const Bindings& bindings() const;
  const ExprPtr&  defaultExpr() const;

  void variant(const ExprPtr&);
  Bindings& bindings();
  void defaultExpr(const ExprPtr&);

  bool hasBinding(const std::string&) const;
  void addBinding(const std::string& selector, const std::string& vname, const ExprPtr& exp);

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 19;
private:
  ExprPtr  v;
  Bindings bs;
  ExprPtr  def;
};

// switch E of |c0=>E0;...;cN=>EN;default=>Ed|
class Switch : public ExprCase<Switch> {
public:
  struct Binding {
    Binding() { }
    Binding(const PrimitivePtr& value, const ExprPtr& exp) : value(value), exp(exp) { }

    PrimitivePtr value;
    ExprPtr      exp;
  };
  typedef std::vector<Binding> Bindings;

  Switch(const ExprPtr& v, const Bindings& bs, const LexicalAnnotation&);
  Switch(const ExprPtr& v, const Bindings& bs, const ExprPtr& def, const LexicalAnnotation&);
  bool operator==(const Switch&) const;

  const ExprPtr&  expr()        const;
  const Bindings& bindings()    const;
  const ExprPtr&  defaultExpr() const;

  void expr(const ExprPtr&);
  Bindings& bindings();
  void defaultExpr(const ExprPtr&);

  Expr* clone() const;
  void show(std::ostream&) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 20;
private:
  ExprPtr  v;
  Bindings bs;
  ExprPtr  def;
};

// E.l (record projection)
class Proj : public ExprCase<Proj> {
public:
  Proj(const ExprPtr& r, const std::string& fn, const LexicalAnnotation&);
  bool operator==(const Proj&) const;

  const ExprPtr& record() const;
  const std::string& field() const;
  
  void record(const ExprPtr&);
  void field(const std::string&);

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 21;
private:
  ExprPtr     r;
  std::string fn;
};

// E :: T (type casting/assumption)
class Assump : public ExprCase<Assump> {
public:
  Assump(const ExprPtr& e, const QualTypePtr& t, const LexicalAnnotation&);
  bool operator==(const Assump&) const;

  const ExprPtr& expr() const;
  const QualTypePtr& ty() const;

  void expr(const ExprPtr&);
  void ty(const QualTypePtr&);

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 22;
private:
  ExprPtr     e;
  QualTypePtr t;
};

// pack E (existential type introduction)
class Pack : public ExprCase<Pack> {
public:
  Pack(const ExprPtr& e, const LexicalAnnotation&);
  bool operator==(const Pack&) const;

  const ExprPtr& expr() const;
  void expr(const ExprPtr&);

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 23;
private:
  ExprPtr e;
};

// unpack E in E (existential type elimination)
class Unpack : public ExprCase<Unpack> {
public:
  Unpack(const std::string&, const ExprPtr&, const ExprPtr&, const LexicalAnnotation&);
  bool operator==(const Unpack&) const;

  const std::string& varName() const;
  const ExprPtr&     package() const;
  const ExprPtr&     expr()    const;

  void varName(const std::string&);
  void package(const ExprPtr&);
  void expr(const ExprPtr&);

  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;

  static const int type_case_id = 24;
private:
  std::string vn;
  ExprPtr     pkg;
  ExprPtr     body;
};

// 128-bit int
class Int128 : public PrimitiveCase<Int128> {
public:
  Int128(int128_t x, const LexicalAnnotation&);
  int128_t value() const;
  void value(int128_t);
  Expr* clone() const;
  void show(std::ostream& out) const;
  void showAnnotated(std::ostream& out) const;
  bool equiv(const Int128&) const;
  bool lt(const Int128&) const;
  MonoTypePtr primType() const;

  static const int type_case_id = 25;
private:
  int128_t x;
};

template <typename Case>
  ExprCase<Case>::ExprCase(const LexicalAnnotation& la) : Expr(Case::type_case_id, la) {
  }

// expression utilities
inline ExprPtr constant(bool x, const LexicalAnnotation& la) {
  ExprPtr r(new Bool(x, la));
  r->type(qualtype(MonoTypePtr(Prim::make("bool"))));
  return r;
}

inline ExprPtr constant(uint8_t x, const LexicalAnnotation& la) {
  ExprPtr r(new Byte(x, la));
  r->type(qualtype(MonoTypePtr(Prim::make("byte"))));
  return r;
}

inline ExprPtr constant(int x, const LexicalAnnotation& la) {
  ExprPtr r(new Int(x, la));
  r->type(qualtype(MonoTypePtr(Prim::make("int"))));
  return r;
}

inline ExprPtr constant(long x, const LexicalAnnotation& la) {
  ExprPtr r(new Long(x, la));
  r->type(qualtype(MonoTypePtr(Prim::make("long"))));
  return r;
}

inline ExprPtr constant(size_t x, const LexicalAnnotation& la) {
  ExprPtr r(new Long(x, la));
  r->type(qualtype(MonoTypePtr(Prim::make("long"))));
  return r;
}

inline ExprPtr constant(int128_t x, const LexicalAnnotation& la) {
  ExprPtr r(new Int128(x, la));
  r->type(qualtype(MonoTypePtr(Prim::make("int128"))));
  return r;
}

inline ExprPtr var(const std::string& vn, const LexicalAnnotation& la) {
  return ExprPtr(new Var(vn, la));
}

inline Exprs vars(const std::vector<std::string>& vns, const LexicalAnnotation& la) {
  Exprs r;
  for (const std::string& vn : vns) {
    r.push_back(var(vn, la));
  }
  return r;
}

inline ExprPtr var(const std::string& vn, const QualTypePtr& ty, const LexicalAnnotation& la) {
  ExprPtr r(new Var(vn, la));
  r->type(ty);
  return r;
}

inline ExprPtr var(const std::string& vn, const MonoTypePtr& ty, const LexicalAnnotation& la) {
  return var(vn, qualtype(ty), la);
}

inline ExprPtr let(const std::string& vn, const ExprPtr& e, const ExprPtr& b, const LexicalAnnotation& la) {
  ExprPtr r(new Let(vn, e, b, la));
  if (e->type() != QualTypePtr() && b->type() != QualTypePtr()) {
    r->type(qualtype(mergeConstraints(e->type()->constraints(), b->type()->constraints()), b->type()->monoType()));
  }
  return r;
}

inline ExprPtr let(const Definitions& bs, const ExprPtr& b, const LexicalAnnotation& la) {
  ExprPtr r = b;
  for (auto rb = bs.rbegin(); rb != bs.rend(); ++rb) {
    r = let(rb->first, rb->second, r, la);
  }
  return r;
}

inline ExprPtr let(const Exprs& es, const ExprPtr& b, const LexicalAnnotation& la) {
  Definitions bs;
  for (const auto& e : es) {
    bs.push_back(Definition(freshName(), e));
  }
  return let(bs, b, la);
}

inline ExprPtr mktunit(const LexicalAnnotation& la) {
  ExprPtr r(new Unit(la));
  r->type(qualtype(MonoTypePtr(Prim::make("unit"))));
  return r;
}

inline ExprPtr mkrecord(const MkRecord::FieldDefs& fds, const LexicalAnnotation& la) {
  if (fds.size() == 0) {
    return mktunit(la);
  } else {
    ExprPtr result(new MkRecord(fds, la));
  
    Constraints     csts;
    Record::Members ms;
    for (MkRecord::FieldDefs::const_iterator fd = fds.begin(); fd != fds.end(); ++fd) {
      if (fd->second->type() == QualTypePtr()) {
        return result;
      } else {
        mergeConstraints(fd->second->type()->constraints(), &csts);
        ms.push_back(Record::Member(fd->first, fd->second->type()->monoType()));
      }
    }

    result->type(qualtype(csts, MonoTypePtr(Record::make(ms))));
    return result;
  }
}

// maybe generalize to make a tuple expr of any size
inline ExprPtr mktuple(const ExprPtr& e, const LexicalAnnotation& la) {
  MkRecord::FieldDefs fs;
  fs.push_back(MkRecord::FieldDef(".f0", e));
  return mkrecord(fs, la);
}

inline ExprPtr proj(const ExprPtr& rec, const Record* rty, const std::string& field, const LexicalAnnotation& la) {
  ExprPtr p(new Proj(rec, field, la));
  p->type(qualtype(rec->type()->constraints(), rty->member(field)));
  return p;
}

inline ExprPtr proj(const ExprPtr& rec, const std::string& field, const LexicalAnnotation& la) {
  if (rec->type() != QualTypePtr()) {
    if (const Record* rty = is<Record>(rec->type()->monoType())) {
      return proj(rec, rty, field, la);
    } else {
      throw annotated_error(*rec, "Expected record type in projection");
    }
  }
  return ExprPtr(new Proj(rec, field, la));
}

inline ExprPtr proj(const ExprPtr& rec, const str::seq& fields, const LexicalAnnotation& la) {
  ExprPtr r = rec;
  for (const auto& f : fields) {
    r = proj(r, f, la);
  }
  return r;
}

inline Case::Bindings caseOfBindings() {
  return Case::Bindings();
}

template <typename ... Cases>
  Case::Bindings caseOfBindings(const char* ctor, const char* vname, const ExprPtr& e, Cases... cs) {
    Case::Bindings hbs;
    hbs.push_back(Case::Binding(ctor, vname, e));
    Case::Bindings tbs = caseOfBindings(cs...);
    hbs.insert(hbs.end(), tbs.begin(), tbs.end());
    return hbs;
  }

template <typename ... Cases>
  ExprPtr caseOf(const ExprPtr& e, Cases... cs) {
    return ExprPtr(new Case(e, caseOfBindings(cs...), e->la()));
  }

inline Constraints liftConstraints(const Exprs& es) {
  Constraints r;
  for (Exprs::const_iterator e = es.begin(); e != es.end(); ++e) {
    QualTypePtr qt = (*e)->type();
    if (qt != QualTypePtr()) {
      append(&r, qt->constraints());
    }
  }
  return r;
}

inline ExprPtr fn(const str::seq& vns, const ExprPtr& b, const LexicalAnnotation& la) {
  return ExprPtr(new Fn(vns, b, la));
}

inline ExprPtr fn(const std::string& vn, const ExprPtr& b, const LexicalAnnotation& la) {
  return fn(str::strings(vn), b, la);
}

inline ExprPtr fncall(const ExprPtr& f, const Exprs& args, const LexicalAnnotation& la) {
  ExprPtr r(new App(f, args, la));
  if (f->type() != QualTypePtr()) {
    r->type(qualtype(liftConstraints(cons(f, args)), fnresult(f->type()->monoType())));
  }
  return r;
}

inline ExprPtr fncall(const ExprPtr& f, const ExprPtr& a, const LexicalAnnotation& la) {
  return fncall(f, list(a), la);
}

inline ExprPtr closcall(const ExprPtr& c, const Exprs& args, const LexicalAnnotation& la) {
  QualTypePtr qt = c->type();
  std::string cn = freshName();

  if (qt == QualTypePtr()) {
    return ExprPtr(new Unpack(cn, c, fncall(proj(var(cn, la), ".f0", la), cons(proj(var(cn, la), ".f1", la), args), la), la));
  }

  const Exists* ety = is<Exists>(qt->monoType());
  if (!ety) {
    throw annotated_error(*c, "Expected existential type in closure application");
  }

  ExprPtr cv = var(cn, unpackedType(ety), la);
  ExprPtr r  = fncall(proj(cv, ".f0", la), cons(proj(cv, ".f1", la), args), la);
  ExprPtr up = ExprPtr(new Unpack(cn, c, r, la));

  up->type(r->type());
  return up;
}

// op :: (a0..aN) -> r => \(a0..aN).op(a0..aN)
inline ExprPtr etaLift(const std::string& opname, const MonoTypePtr& oty, const LexicalAnnotation& la) {
  Func* fty = is<Func>(oty);
  if (!fty) { throw annotated_error(la, "Internal compiler error while eta-expanding primitive op: " + opname); }

  ExprPtr op = var(opname, oty, la);

  const MonoTypes& atys = fty->parameters();
  Exprs            args;
  Fn::VarNames     vns;
  int              i = 0;

  for (const auto& a : atys) {
    std::string vname = canonicalName(i);
    args.push_back(var(vname, a, la));
    vns.push_back(vname);
    ++i;
  }

  ExprPtr ret = fn(vns, fncall(op, args, la), la);
  ret->type(qualtype(oty));
  return ret;
}

inline Expr* mkarray(const std::vector<unsigned char>& v, const LexicalAnnotation& la) {
  MonoTypePtr bty = primty("byte");
  QualTypePtr ety = qualtype(bty);
  QualTypePtr aty = qualtype(arrayty(bty));

  Exprs cs;
  for (size_t b = 0; b < v.size(); ++b) {
    ExprPtr be(new Byte(v[b], la));
    be->type(ety);
    cs.push_back(be);
  }

  ExprPtr marr(new MkArray(cs, la));
  marr->type(aty);

  Expr* result = new Assump(marr, aty, la);
  result->type(aty);

  return result;
}

inline Expr* mkarray(const std::string& v, const LexicalAnnotation& la) {
  MonoTypePtr cty = primty("char");
  QualTypePtr ety = qualtype(cty);
  QualTypePtr aty = qualtype(arrayty(cty));

  Exprs cs;
  for (size_t c = 0; c < v.size(); ++c) {
    ExprPtr ce(new Char(v[c], la));
    ce->type(ety);
    cs.push_back(ce);
  }

  ExprPtr marr(new MkArray(cs, la));
  marr->type(aty);

  Expr* result = new Assump(marr, aty, la);
  result->type(aty);

  return result;
}

inline ExprPtr switchE(const ExprPtr& e, const Switch::Bindings& bs, const ExprPtr& def, const LexicalAnnotation& la) {
  if (bs.size() == 0) {
    return def;
  } else {
    return ExprPtr(new Switch(e, bs, def, la));
  }
}

inline ExprPtr assume(const ExprPtr& e, const QualTypePtr& t, const LexicalAnnotation& la) {
  if (const Assump* ae = is<Assump>(e)) {
    if (*ae->ty() == *t) {
      return e;
    }
  }

  QualTypePtr ety = e->type();
  if (ety) {
    ExprPtr r(new Assump(e, t, la));
    r->type(qualtype(mergeConstraints(ety->constraints(), t->constraints()), ety->monoType()));
    return r;
  } else {
    return ExprPtr(new Assump(e, t, la));
  }
}

inline ExprPtr assume(const ExprPtr& e, const MonoTypePtr& t, const LexicalAnnotation& la) {
  return assume(e, qualtype(t), la);
}

// justE :: expr:a -> |1=expr|::()+a
inline ExprPtr justE(const ExprPtr& e, const LexicalAnnotation& la) {
  if (e->type()) {
    ExprPtr r(new MkVariant(".f1", e, la));
    r->type(qualtype(e->type()->constraints(), sumtype(primty("unit"), e->type()->monoType())));
    return assume(r, r->type(), la);
  } else {
    MonoTypePtr t = freshTypeVar();
    return assume(ExprPtr(new MkVariant(".f1", assume(e, t, la), la)), sumtype(primty("unit"), t), la);
  }
}

// nothingE :: () -> |0=()|
inline ExprPtr nothingE(const LexicalAnnotation& la) {
  return ExprPtr(new MkVariant(".f0", mktunit(la), la));
}

inline ExprPtr nothingE(const MonoTypePtr& jt, const LexicalAnnotation& la) {
  ExprPtr r(new MkVariant(".f0", mktunit(la), la));
  r->type(qualtype(maybety(jt)));
  return r;
}

inline ExprPtr assign(const ExprPtr& lhs, const ExprPtr& rhs, const LexicalAnnotation& la) {
  ExprPtr r(new Assign(lhs, rhs, la));
  if (lhs->type() && rhs->type()) {
    r->type(qualtype(mergeConstraints(lhs->type()->constraints(), rhs->type()->constraints()), primty("unit")));
  }
  return r;
}

inline Exprs exprs(const MkRecord::FieldDefs& ms) {
  Exprs r;
  for (MkRecord::FieldDefs::const_iterator m = ms.begin(); m != ms.end(); ++m) {
    r.push_back(m->second);
  }
  return r;
}

inline Exprs exprs(const Case::Bindings& bs) {
  Exprs r;
  for (Case::Bindings::const_iterator b = bs.begin(); b != bs.end(); ++b) {
    r.push_back(b->exp);
  }
  return r;
}

// replaces instances of a given variable name with the specified expression
//  (NOTE: this does nothing to prevent variable capture, other than respecting shadowing)
//  (ie: the free variables of the substituted expression should be disjoint with program variables)
typedef std::map<std::string, ExprPtr> VarMapping;
ExprPtr substitute(const VarMapping& vm, const ExprPtr& e, bool* mapped = 0);

// apply a type substitution across an expression
ExprPtr substitute(const MonoTypeSubst& s, const ExprPtr& e);

// safely consume constants
//   (if the set of constants is extended but functions on constants aren't extended, it will be a compile-error)
template <typename T>
  struct switchConst {
    virtual T with(const Unit*   v) const = 0;
    virtual T with(const Bool*   v) const = 0;
    virtual T with(const Char*   v) const = 0;
    virtual T with(const Byte*   v) const = 0;
    virtual T with(const Short*  v) const = 0;
    virtual T with(const Int*    v) const = 0;
    virtual T with(const Long*   v) const = 0;
    virtual T with(const Int128* v) const = 0;
    virtual T with(const Float*  v) const = 0;
    virtual T with(const Double* v) const = 0;
  };

template <typename T>
  T switchOf(const PrimitivePtr& p, const switchConst<T>& f) {
    switch (p->case_id()) {
    case Unit::type_case_id:
      return f.with(crcast<Unit*>(p.get()));
    case Bool::type_case_id:
      return f.with(crcast<Bool*>(p.get()));
    case Char::type_case_id:
      return f.with(crcast<Char*>(p.get()));
    case Byte::type_case_id:
      return f.with(crcast<Byte*>(p.get()));
    case Short::type_case_id:
      return f.with(crcast<Short*>(p.get()));
    case Int::type_case_id:
      return f.with(crcast<Int*>(p.get()));
    case Long::type_case_id:
      return f.with(crcast<Long*>(p.get()));
    case Int128::type_case_id:
      return f.with(crcast<Int128*>(p.get()));
    case Float::type_case_id:
      return f.with(crcast<Float*>(p.get()));
    case Double::type_case_id:
      return f.with(crcast<Double*>(p.get()));
    default:
      throw annotated_error(*p, "Internal error, cannot switch on unknown constant: " + show(p));
    }
  }

// safely consume expressions
//   (if the expression type is extended but functions on expressions aren't extended, it will be a compile-error)
template <typename T>
  struct switchExpr {
    //virtual ~switchExpr() = default;
    virtual T with(const Unit* v)      const = 0;
    virtual T with(const Bool* v)      const = 0;
    virtual T with(const Char* v)      const = 0;
    virtual T with(const Byte* v)      const = 0;
    virtual T with(const Short* v)     const = 0;
    virtual T with(const Int* v)       const = 0;
    virtual T with(const Int128* v)    const = 0;
    virtual T with(const Long* v)      const = 0;
    virtual T with(const Float* v)     const = 0;
    virtual T with(const Double* v)    const = 0;
    virtual T with(const Var* v)       const = 0;
    virtual T with(const Let* v)       const = 0;
    virtual T with(const LetRec* v)    const = 0;
    virtual T with(const Fn* v)        const = 0;
    virtual T with(const App* v)       const = 0;
    virtual T with(const Assign* v)    const = 0;
    virtual T with(const MkArray* v)   const = 0;
    virtual T with(const MkVariant* v) const = 0;
    virtual T with(const MkRecord* v)  const = 0;
    virtual T with(const AIndex* v)    const = 0;
    virtual T with(const Case* v)      const = 0;
    virtual T with(const Switch* v)    const = 0;
    virtual T with(const Proj* v)      const = 0;
    virtual T with(const Assump* v)    const = 0;
    virtual T with(const Pack* v)      const = 0;
    virtual T with(const Unpack* v)    const = 0;
  };

template <typename T>
  struct switchExprC : public switchExpr<T> {
    virtual ~switchExprC() = default;
    virtual T withConst(const Expr* v)      const = 0;
    virtual T with     (const Var* v)       const = 0;
    virtual T with     (const Let* v)       const = 0;
    virtual T with     (const LetRec* v)    const = 0;
    virtual T with     (const Fn* v)        const = 0;
    virtual T with     (const App* v)       const = 0;
    virtual T with     (const Assign* v)    const = 0;
    virtual T with     (const MkArray* v)   const = 0;
    virtual T with     (const MkVariant* v) const = 0;
    virtual T with     (const MkRecord* v)  const = 0;
    virtual T with     (const AIndex* v)    const = 0;
    virtual T with     (const Case* v)      const = 0;
    virtual T with     (const Switch* v)    const = 0;
    virtual T with     (const Proj* v)      const = 0;
    virtual T with     (const Assump* v)    const = 0;
    virtual T with     (const Pack* v)      const = 0;
    virtual T with     (const Unpack* v)    const = 0;

    // implement just the constant 'with' terms to collapse them
    T with(const Unit*   v) const { return withConst(v); }
    T with(const Bool*   v) const { return withConst(v); }
    T with(const Char*   v) const { return withConst(v); }
    T with(const Byte*   v) const { return withConst(v); }
    T with(const Short*  v) const { return withConst(v); }
    T with(const Int*    v) const { return withConst(v); }
    T with(const Long*   v) const { return withConst(v); }
    T with(const Int128* v) const { return withConst(v); }
    T with(const Float*  v) const { return withConst(v); }
    T with(const Double* v) const { return withConst(v); }
  };

bool isConst(const ExprPtr&);

template <typename T>
  struct switchExprM {
    virtual T with(Unit* v)      = 0;
    virtual T with(Bool* v)      = 0;
    virtual T with(Char* v)      = 0;
    virtual T with(Byte* v)      = 0;
    virtual T with(Short* v)     = 0;
    virtual T with(Int* v)       = 0;
    virtual T with(Long* v)      = 0;
    virtual T with(Int128* v)    = 0;
    virtual T with(Float* v)     = 0;
    virtual T with(Double* v)    = 0;
    virtual T with(Var* v)       = 0;
    virtual T with(Let* v)       = 0;
    virtual T with(LetRec* v)    = 0;
    virtual T with(Fn* v)        = 0;
    virtual T with(App* v)       = 0;
    virtual T with(Assign* v)    = 0;
    virtual T with(MkArray* v)   = 0;
    virtual T with(MkVariant* v) = 0;
    virtual T with(MkRecord* v)  = 0;
    virtual T with(AIndex* v)    = 0;
    virtual T with(Case* v)      = 0;
    virtual T with(Switch* v)    = 0;
    virtual T with(Proj* v)      = 0;
    virtual T with(Assump* v)    = 0;
    virtual T with(Pack* v)      = 0;
    virtual T with(Unpack* v)    = 0;
  };


template <typename T, typename F>
  T switchOfF(const Expr& e, F f) {
    switch (e.case_id()) {
    case Unit::type_case_id:
      return f.with(crcast<Unit*>(&e));
    case Bool::type_case_id:
      return f.with(crcast<Bool*>(&e));
    case Char::type_case_id:
      return f.with(crcast<Char*>(&e));
    case Byte::type_case_id:
      return f.with(crcast<Byte*>(&e));
    case Short::type_case_id:
      return f.with(crcast<Short*>(&e));
    case Int::type_case_id:
      return f.with(crcast<Int*>(&e));
    case Long::type_case_id:
      return f.with(crcast<Long*>(&e));
    case Int128::type_case_id:
      return f.with(crcast<Int128*>(&e));
    case Float::type_case_id:
      return f.with(crcast<Float*>(&e));
    case Double::type_case_id:
      return f.with(crcast<Double*>(&e));
    case Var::type_case_id:
      return f.with(crcast<Var*>(&e));
    case Let::type_case_id:
      return f.with(crcast<Let*>(&e));
    case LetRec::type_case_id:
      return f.with(crcast<LetRec*>(&e));
    case Fn::type_case_id:
      return f.with(crcast<Fn*>(&e));
    case App::type_case_id:
      return f.with(crcast<App*>(&e));
    case Assign::type_case_id:
      return f.with(crcast<Assign*>(&e));
    case MkArray::type_case_id:
      return f.with(crcast<MkArray*>(&e));
    case MkVariant::type_case_id:
      return f.with(crcast<MkVariant*>(&e));
    case MkRecord::type_case_id:
      return f.with(crcast<MkRecord*>(&e));
    case AIndex::type_case_id:
      return f.with(crcast<AIndex*>(&e));
    case Case::type_case_id:
      return f.with(crcast<Case*>(&e));
    case Switch::type_case_id:
      return f.with(crcast<Switch*>(&e));
    case Proj::type_case_id:
      return f.with(crcast<Proj*>(&e));
    case Assump::type_case_id:
      return f.with(crcast<Assump*>(&e));
    case Pack::type_case_id:
      return f.with(crcast<Pack*>(&e));
    case Unpack::type_case_id:
      return f.with(crcast<Unpack*>(&e));
    default:
      throw annotated_error(e, "Internal error, cannot switch on unknown expression: " + show(e));
    }
  }

template <typename T>
  T switchOf(const Expr& e, const switchExpr<T>& f) {
    return switchOfF< T, const switchExpr<T>& >(e, f);
  }

template <typename T>
  T switchOf(const ExprPtr& e, const switchExpr<T>& f) {
    return switchOfF< T, const switchExpr<T>& >(*e, f);
  }

template <typename T>
  T switchOf(const ExprPtr& e, const switchExprM<T>& f) {
    return switchOfF< T, switchExprM<T>& >(*e, const_cast<switchExprM<T>&>(f));
  }

template <typename T>
  std::vector<T> switchOf(const Exprs& es, const switchExpr<T>& f) {
    std::vector<T> result;
    for (auto e : es) {
      result.push_back(switchOf(e, f));
    }
    return result;
  }

template <typename T>
  std::vector<T> switchOf(const Exprs& es, const switchExprM<T>& f) {
    std::vector<T> result;
    for (auto e : es) {
      result.push_back(switchOf(e, const_cast<switchExprM<T>&>(f)));
    }
    return result;
  }

template <typename K, typename T>
  std::vector< std::pair<K, T> > switchOf(const std::vector< std::pair<K, ExprPtr> >& kes, const switchExpr<T>& f) {
    typedef std::pair<K, T> KT;
    typedef std::vector<KT> KTS;
    KTS kts;
    for (auto ke : kes) {
      kts.push_back(KT(ke.first, switchOf(ke.second, f)));
    }
    return kts;
  }

template <typename K, typename T>
  std::vector< std::pair<K, T> > switchOf(const std::vector< std::pair<K, ExprPtr> >& kes, const switchExprM<T>& f) {
    typedef std::pair<K, T> KT;
    typedef std::vector<KT> KTS;
    KTS kts;
    for (auto ke : kes) {
      kts.push_back(KT(ke.first, switchOf(ke.second, const_cast<switchExprM<T>&>(f))));
    }
    return kts;
  }

// simplify type-directed expression transforms (e.g.: unqualification)
//   as a function or an in-place mutation
//
struct switchExprTyFn : public switchExprC<ExprPtr> {
  virtual ~switchExprTyFn() = default;
  // whatever type transform is being done (default is the identity transform)
  virtual QualTypePtr withTy(const QualTypePtr& qt) const;

  // e :: withTy(qty)  -- wrapped in a shared pointer
  virtual ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const;

  // allocate a fresh expression, updating types as we go
  ExprPtr withConst(const Expr* v)      const;
  ExprPtr with     (const Var* v)       const;
  ExprPtr with     (const Let* v)       const;
  ExprPtr with     (const LetRec* v)    const;
  ExprPtr with     (const Fn* v)        const;
  ExprPtr with     (const App* v)       const;
  ExprPtr with     (const Assign* v)    const;
  ExprPtr with     (const MkArray* v)   const;
  ExprPtr with     (const MkVariant* v) const;
  ExprPtr with     (const MkRecord* v)  const;
  ExprPtr with     (const AIndex* v)    const;
  ExprPtr with     (const Case* v)      const;
  ExprPtr with     (const Switch* v)    const;
  ExprPtr with     (const Proj* v)      const;
  ExprPtr with     (const Assump* v)    const;
  ExprPtr with     (const Pack* v)      const;
  ExprPtr with     (const Unpack* v)    const;
};

struct switchExprTyFnM : public switchExprM<UnitV> {
  // whatever type transform is being done (default is the identity transform)
  virtual QualTypePtr withTy(const QualTypePtr& qt) const;

  UnitV with(Unit* v);
  UnitV with(Bool* v);
  UnitV with(Char* v);
  UnitV with(Byte* v);
  UnitV with(Short* v);
  UnitV with(Int* v);
  UnitV with(Long* v);
  UnitV with(Int128* v);
  UnitV with(Float* v);
  UnitV with(Double* v);
  UnitV with(Var* v);
  UnitV with(Let* v);
  UnitV with(LetRec* v);
  UnitV with(Fn* v);
  UnitV with(App* v);
  UnitV with(Assign* v);
  UnitV with(MkArray* v);
  UnitV with(MkVariant* v);
  UnitV with(MkRecord* v);
  UnitV with(AIndex* v);
  UnitV with(Case* v);
  UnitV with(Switch* v);
  UnitV with(Proj* v);
  UnitV with(Assump* v);
  UnitV with(Pack* v);
  UnitV with(Unpack* v);
private:
  UnitV updateTy(Expr* e);
};

Case::Bindings switchOf(const Case::Bindings& bs, const switchExpr<ExprPtr>& f);
Switch::Bindings switchOf(const Switch::Bindings& bs, const switchExpr<ExprPtr>& f);

// when asserting that an expression has a monotype, we might find an exceptional circumstance where some type constraints remain unsolved
class unsolved_constraints : public annotated_error {
public:
  unsolved_constraints(const LexicalAnnotation&, const std::string&, const Constraints&);
  unsolved_constraints(const LexicallyAnnotated&, const std::string&, const Constraints&);

  const Constraints& constraints() const;
private:
  Constraints cs;
};

// require that a mono-type can be extracted from an expression
const MonoTypePtr& requireMonotype(const TEnvPtr&, const ExprPtr&);
MonoTypes requireMonotype(const TEnvPtr&, const Exprs&);

// add explicit type assumption terms where we have annotations
ExprPtr liftTypesAsAssumptions(const ExprPtr& e);

// strip type assumptions from terms (these aren't necessary when we're dealing with mono-typed expressions)
ExprPtr stripExplicitAssumptions(const ExprPtr&);
const ExprPtr& stripAssumpHead(const ExprPtr&);

// find the free variables in a term
typedef std::set<std::string> VarSet;

VarSet freeVars(const ExprPtr&);
VarSet freeVars(const Expr&);

// find the type variables used in a term
NameSet tvarNames(const ExprPtr&);

// simplify introducing array comprehensions (these should just be syntax sugar for core terms anyway)
struct ComprehensionDef {
  ComprehensionDef(const std::string& varname, const ExprPtr& ex);
  ComprehensionDef(const ExprPtr& ex);

  bool        isfilter;
  std::string varname;
  ExprPtr     ex;
};

typedef std::vector<ComprehensionDef> ComprehensionDefs;

ExprPtr desugarComprehension(const ExprPtr& ex, const ComprehensionDefs& cdefs, const LexicalAnnotation& la);

// generate a format expression from a format string
ExprPtr mkFormatExpr(const std::string& fmt, const LexicalAnnotation&);

// generate constants for some common types
PrimitivePtr mkTimespanPrim(const str::seq&, const LexicalAnnotation&);
ExprPtr mkTimespanExpr(const str::seq&, const LexicalAnnotation&);

PrimitivePtr mkTimePrim(const std::string&, const LexicalAnnotation&);
ExprPtr mkTimeExpr(const std::string&, const LexicalAnnotation&);

ExprPtr mkDateTimeExpr(const std::string&, const LexicalAnnotation&);
PrimitivePtr mkDateTimePrim(const std::string&, const LexicalAnnotation&);

// support a binary codec for expressions
void encode(const PrimitivePtr&, std::ostream&);
void decode(PrimitivePtr*,       std::istream&);

void encode(const ExprPtr&, std::ostream&);
void decode(ExprPtr*,       std::istream&);

void encode(const ExprPtr&, std::vector<uint8_t>*);
void decode(const std::vector<uint8_t>&, ExprPtr*);

// determine whether or not an expression is fully annotated everywhere with a singular type
bool hasSingularType(const ExprPtr&);

// determine the tgen size of types across an expression
int tgenSize(const ExprPtr&);

}

#endif
