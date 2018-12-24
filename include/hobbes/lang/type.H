
#ifndef HOBBES_LANG_TYPE_HPP_INCLUDED
#define HOBBES_LANG_TYPE_HPP_INCLUDED

#include <hobbes/util/hash.H>
#include <hobbes/util/ptr.H>
#include <hobbes/util/array.H>
#include <hobbes/util/str.H>
#include <hobbes/util/lannotation.H>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <memory>
#include <stdexcept>

namespace hobbes {

// there is a type for holding type equality constraints
class MonoTypeUnifier;

// forall a0..aN : T
class PolyType;
typedef std::shared_ptr<PolyType> PolyTypePtr;
typedef std::vector<PolyTypePtr>    PolyTypes;

// (C0, ..., Cn) => T
class QualType;
typedef std::shared_ptr<QualType> QualTypePtr;
typedef std::vector<QualTypePtr>    QualTypes;

// P(T)
class Constraint;
typedef std::shared_ptr<Constraint> ConstraintPtr;
typedef std::vector<ConstraintPtr>    Constraints;

// T
class MonoType {
public:
  virtual ~MonoType();
  typedef std::shared_ptr<MonoType> ptr;

  virtual void show(std::ostream&) const = 0;
  bool operator==(const MonoType& rhs) const;

  // improves performance of case-analysis over instances (to avoid 'dynamic_cast')
public:
  int case_id() const;
protected:
  MonoType(int cid);

  // memoize type construction
  template <typename Class, typename T, typename ... Args>
    static ptr makeType(const Args&... args);
private:
  int cid;

public:
  // improves performance of identifying type variables and tgens
  typedef std::set<std::string> TypeVarNames;
  TypeVarNames freeTVars;
  int tgenCount;

  // improves performance of computing the memory size of a type
  mutable unsigned int memorySize;

  // improves performance of unhiding opaque type aliases
  mutable ptr unaliasedType;
};

typedef MonoType::ptr MonoTypePtr;
typedef std::vector<MonoTypePtr>  MonoTypes;

inline std::ostream& operator<<(std::ostream& out, const MonoTypePtr& t) {
  t->show(out);
  return out;
}

// nested environments for types and type predicate resolvers
class TEnv;
typedef std::shared_ptr<TEnv> TEnvPtr;

class Unqualifier;
typedef std::shared_ptr<Unqualifier> UnqualifierPtr;

class UnqualifierSet;
typedef std::shared_ptr<UnqualifierSet> UnqualifierSetPtr;

class TEnv {
public:
  TEnv(const TEnvPtr& parent);
  TEnv();

  bool hasBinding(const std::string& vname) const;
  bool hasImmediateBinding(const std::string& vname) const;

  // in/out type bindings
  void bind(const std::string& vname, const PolyTypePtr&);
  void bind(const std::string& vname, const QualTypePtr&);
  void bind(const std::string& vname, const MonoTypePtr&);
  void unbind(const std::string& vname);
  PolyTypePtr lookup(const std::string& vname) const;

  // overloading / subtyping
  void bind(const std::string& predName, const UnqualifierPtr& uq);
  UnqualifierPtr lookupUnqualifier(const std::string& predName) const;
  UnqualifierPtr lookupUnqualifier(const ConstraintPtr& cst) const;

  // get access to the parent and root type environment from here
  const TEnvPtr& parentTypeEnv() const;
  TEnv* root();
public:
  // get access to the internal type environment map (should only be used for debugging)
  typedef std::map<std::string, PolyTypePtr> PolyTypeEnv;

  PolyTypeEnv typeEnvTable() const;
  str::set boundVariables() const;

  typedef std::map<std::string, UnqualifierPtr> Unqualifiers;
  const Unqualifiers& unqualifiers() const;
private:
  TEnvPtr           parent;
  UnqualifierSetPtr unquals; // non-empty iff parent==0
  PolyTypeEnv       ptenv;

public:
  // pack/unpack opaque type aliases
  void alias(const std::string&, const MonoTypePtr&);
  MonoTypePtr unalias(const std::string&) const;
  bool isOpaqueTypeAlias(const std::string&) const;
private:
  typedef std::map<std::string, MonoTypePtr> TypeAliases;
  TypeAliases typeAliases;
};

TEnvPtr fnFrame(const TEnvPtr&, const str::seq&, const MonoTypes&);
TEnvPtr bindFrame(const TEnvPtr&, const std::string&, const MonoTypePtr&);
TEnvPtr bindFrame(const TEnvPtr&, const std::string&, const QualTypePtr&);

typedef std::string                   TVName;
typedef std::vector<TVName>           Names;
typedef std::set<TVName>              NameSet;
typedef std::map<TVName, MonoTypePtr> MonoTypeSubst;

inline MonoTypeSubst substitution(const str::seq& ns, const MonoTypes& ts) {
  MonoTypeSubst s;
  size_t n = std::min<size_t>(ns.size(), ts.size());
  for (size_t i = 0; i < n; ++i) {
    s[ns[i]] = ts[i];
  }
  return s;
}

// determine if a constraint is satisfied/satisfiable in this type environment (or if a sequence of constraints are all satisfied/satisfiable)
// (forward declare the 'definitions' type from expr -- this is a little awkward)
class Expr;
bool satisfied(const UnqualifierPtr&, const TEnvPtr&, const ConstraintPtr&, std::vector<std::pair<std::string, std::shared_ptr<Expr> > >*);
bool satisfied(const TEnvPtr& tenv, const ConstraintPtr& c, std::vector<std::pair<std::string, std::shared_ptr<Expr> > >*);
bool satisfied(const TEnvPtr& tenv, const Constraints& cs, std::vector<std::pair<std::string, std::shared_ptr<Expr> > >*);
bool satisfiable(const UnqualifierPtr&, const TEnvPtr&, const ConstraintPtr&, std::vector<std::pair<std::string, std::shared_ptr<Expr> > >*);
bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& c, std::vector<std::pair<std::string, std::shared_ptr<Expr> > >*);
bool satisfiable(const TEnvPtr& tenv, const Constraints& cs, std::vector<std::pair<std::string, std::shared_ptr<Expr> > >*);

// a polytype (or type scheme) can describe many types
class PolyType {
public:
  PolyType(size_t vs, const QualTypePtr& qt);
  PolyType(const QualTypePtr& qt);

  size_t typeVariables() const;
  QualTypePtr instantiate() const;

  void show(std::ostream&) const;
  bool operator==(const PolyType& rhs) const;

  // this should only be used if you don't mind capturing bound type variables
  const QualTypePtr& qualtype() const;
private:
  size_t      vs;
  QualTypePtr qt;
};

inline std::ostream& operator<<(std::ostream& out, const PolyTypePtr& t) {
  t->show(out);
  return out;
}

// a qualified type is a monotype with a set of constraints
class QualType {
public:
  QualType(const Constraints& cs, const MonoTypePtr& mt);
  QualType(const MonoTypePtr& mt);

  const Constraints& constraints() const;
  const MonoTypePtr& monoType() const;

  Constraints& constraints();
  void monoType(const MonoTypePtr&);

  void show(std::ostream&) const;
  bool operator==(const QualType& rhs) const;
private:
  Constraints cs;
  MonoTypePtr mt;
};

inline std::ostream& operator<<(std::ostream& out, const QualTypePtr& t) {
  t->show(out);
  return out;
}

// a constraint qualifies types
class Constraint {
public:
  Constraint(const std::string& cat, const MonoTypes& mts);

  // the name of the predicate for this constraint
  std::string name() const;

  // the predicate's arguments
  const MonoTypes& arguments() const;

  // type variable names in/out
  ConstraintPtr instantiate(const MonoTypes& ts) const;
  NameSet tvarNames() const;
  ConstraintPtr substitute(const MonoTypeSubst& s) const;

  // update internally-stored types in-place according to a unification set
  void update(MonoTypeUnifier*);

  // compare/show
  void show(std::ostream&) const;
  bool operator==(const Constraint& rhs) const;

  // are there free variables referenced in this constraint?
  bool hasFreeVariables() const;
private:
  std::string cat;
  MonoTypes   mts;
public:
  // avoid unnecessary satisfied/satisfiable checks
  enum { Unresolved, Satisfied, Unsatisfiable } state;
};

inline std::ostream& operator<<(std::ostream& out, const ConstraintPtr& t) {
  t->show(out);
  return out;
}

// improves performance of case-analysis over MonoType instances (to avoid 'dynamic_cast')
template <typename Case>
  class MonoTypeCase : public MonoType {
  public:
    MonoTypeCase();
  };

// a type treated as primitive (by name)
// may be an "actual primitive" (void, unit, bool, byte, char, int, long, float, double, ...)
// or may be an alias for a composite type
class Prim : public MonoTypeCase<Prim> {
public:
  void show(std::ostream& out) const;

  const std::string& name() const;
  const MonoTypePtr& representation() const;

  static const int type_case_id = 0;

  static MonoTypePtr make(const std::string&, const MonoTypePtr& t = MonoTypePtr());
private:
  std::string nm;
  MonoTypePtr t;

  friend class MonoType;
  Prim(const std::string&, const MonoTypePtr&);
};

// an opaque pointer to some C++ type
class OpaquePtr : public MonoTypeCase<OpaquePtr> {
public:
  void show(std::ostream& out) const;

  const std::string& name()               const;
  unsigned int       size()               const;
  bool               storedContiguously() const;

  static const int type_case_id = 1;

  static MonoTypePtr make(const std::string& nm, unsigned int sz, bool scontig = false);
private:
  std::string  nm;
  unsigned int sz;
  bool         scontig;

  friend class MonoType;
  OpaquePtr(const std::string&, unsigned int, bool);
};

// remove the contiguity flag on opaque pointers
MonoTypePtr normIfOpaquePtr(const MonoTypePtr& ty);

// a type variable (may be substituted for some type)
class TVar : public MonoTypeCase<TVar> {
public:
  const std::string& name() const;
  void show(std::ostream& out) const;

  static const int type_case_id = 2;

  static MonoTypePtr make(const std::string&);
private:
  std::string nm;

  friend class MonoType;
  TVar(const std::string&);
};

// this constructor is ONLY used for polytype instantiation
class TGen : public MonoTypeCase<TGen> {
public:
  int id() const;
  void show(std::ostream& out) const; // should never be shown

  static const int type_case_id = 3;

  static MonoTypePtr make(int);
private:
  int x;

  friend class MonoType;
  TGen(int);
};

// an array whose length is determined
class FixedArray : public MonoTypeCase<FixedArray> {
public:
  void show(std::ostream& out) const;

  const MonoTypePtr& type() const;
  const MonoTypePtr& length() const;

  long requireLength() const;

  static const int type_case_id = 4;

  static MonoTypePtr make(const MonoTypePtr& ty, const MonoTypePtr& len);
private:
  MonoTypePtr ty;
  MonoTypePtr len;

  friend class MonoType;
  FixedArray(const MonoTypePtr&, const MonoTypePtr&);
};

// an array whose length is not known at compile-time
class Array : public MonoTypeCase<Array> {
public:
  void show(std::ostream& out) const;

  const MonoTypePtr& type() const;

  static const int type_case_id = 5;

  static MonoTypePtr make(const MonoTypePtr&);
private:
  MonoTypePtr ty;

  friend class MonoType;
  Array(const MonoTypePtr&);
};

// the 'coproduct' construction (a variant is _one of_ a set of types)
class Variant : public MonoTypeCase<Variant> {
public:
  struct Member {
    Member(const std::string& selector, const MonoTypePtr& type, unsigned int id);
    Member();
    bool operator==(const Member&) const;
    bool operator<(const Member&) const;

    std::string  selector;
    MonoTypePtr  type;
    unsigned int id;
  };
  typedef std::vector<Member> Members;

  void show(std::ostream& out) const;

  // get the first visible member and the type of the 'remainder' of the variant after ignoring the first field
  // (these are used for structural decomposition in generic functions)
  Member headMember() const;
  MonoTypePtr tailType() const;

  const Members& members() const;
  const MonoTypePtr& payload(const std::string& selector) const;
  unsigned int index(const std::string& selector) const;
  unsigned int id(const std::string& selector) const;
  const Member* mmember(const std::string& selector) const;

  // does this variant actually represent a 'sum'?
  bool isSum() const;

  // handle layout logic for variants
  mutable unsigned int payloadSizeM; // cache for 'payloadSize' query

  unsigned int payloadOffset() const;
  unsigned int payloadSize() const;
  unsigned int size() const;

  static const int type_case_id = 6;

  static MonoTypePtr make(const Members&);
  static MonoTypePtr make(const MonoTypePtr& hty, const Members& tty);
  static MonoTypePtr make(const std::string& lbl, const MonoTypePtr& hty, const Members& tty);
private:
  Members ms;

  friend class MonoType;
  Variant(const Members&);
};

// the 'product' construction (a record is _all of_ a set of types)
class Record : public MonoTypeCase<Record> {
public:
  struct Member {
    Member(const std::string& field, const MonoTypePtr& type, int offset = -1);
    Member();
    bool operator==(const Member&) const;
    bool operator<(const Member&) const;

    std::string field;
    MonoTypePtr type;
    int         offset;
  };
  typedef std::vector<Member> Members;

  void show(std::ostream& out) const;

  const Members& members() const;
  const MonoTypePtr& member(const std::string& mn) const;
  unsigned int index(const std::string& mn) const;
  const Member* mmember(const std::string& mn) const;

  // get the first visible member and the type of the 'remainder' of the struct after ignoring the first field
  // (these are used for structural decomposition in generic functions)
  Member headMember() const;
  MonoTypePtr tailType() const;

  // handle alignment determination (with explicit padding added)
  const Members& alignedMembers() const;
  unsigned int alignedIndex(const std::string& mn) const;

  // the amount of contiguous memory used to represent this record
  unsigned int size() const;

  // does this record actually represent a 'tuple'?
  bool isTuple() const;

  // this function must be idempotent
  static Members withExplicitPadding(const Members&, const std::string& pfx = ".p");

  static const int type_case_id = 7;
private:
  Members ms;
  Members ams;

  mutable unsigned int maxFieldAlignmentM;
  unsigned int maxFieldAlignment() const;

  static void showRecord(std::ostream&, const Members&);
  static std::string showRecord(const Members&);

  unsigned int index(const Members& ms, const std::string& mn) const;
public:
  // this function must be idempotent
  static Members withResolvedMemoryLayout(const Members&);

  // constructors
  static MonoTypePtr make(const Members& ms);
  static MonoTypePtr make(const MonoTypePtr& hty, const Members& tty);
  static MonoTypePtr make(const std::string& lbl, const MonoTypePtr& hty, const Members& tty);
private:
  friend class MonoType;
  Record(const Members& ms);
};

// determine type alignment (for efficient/correct access in memory)
unsigned int alignment(const MonoTypePtr&);

// a flat (contextless) function type
class Func : public MonoTypeCase<Func> {
public:
  void show(std::ostream& out) const;

  const MonoTypePtr& argument() const;
  const MonoTypePtr& result() const;

  // untuples the 'argument' type, for convenience
  MonoTypes parameters() const;

  static const int type_case_id = 8;

  static MonoTypePtr make(const MonoTypePtr&, const MonoTypePtr&);
private:
  MonoTypePtr aty;
  MonoTypePtr rty;

  friend class MonoType;
  Func(const MonoTypePtr&, const MonoTypePtr&);
};

// an abstraction of type structure suitable for unifying otherwise distinct types
class Exists : public MonoTypeCase<Exists> {
public:
  void show(std::ostream& out) const;

  const std::string& absTypeName() const;
  const MonoTypePtr& absType() const;

  static const int type_case_id = 9;

  static MonoTypePtr make(const std::string& tname, const MonoTypePtr& bty);
private:
  std::string tname;
  MonoTypePtr bty;

  friend class MonoType;
  Exists(const std::string& tname, const MonoTypePtr& bty);
};

// project out of existential types
MonoTypePtr unpackedType(const Exists* e);
MonoTypePtr unpackedType(const MonoTypePtr& mty);
QualTypePtr unpackedType(const QualTypePtr& qty);

// type-level values (the writing is on the wall here -- this is a hack and we are going to need full dependent types)
class TString : public MonoTypeCase<TString> {
public:
  void show(std::ostream& out) const;

  const std::string& value() const;

  static const int type_case_id = 10;

  static MonoTypePtr make(const std::string&);
private:
  std::string val;

  friend class MonoType;
  TString(const std::string&);
};

class TLong : public MonoTypeCase<TLong> {
public:
  void show(std::ostream& out) const;

  long value() const;

  static const int type_case_id = 11;

  static MonoTypePtr make(long);
private:
  long x;

  friend class MonoType;
  TLong(long x);
};

// a type-level abstraction
class TAbs : public MonoTypeCase<TAbs> {
public:
  void show(std::ostream&) const;

  const str::seq&    args() const;
  const MonoTypePtr& body() const;

  static const int type_case_id = 15;

  static MonoTypePtr make(const str::seq&, const MonoTypePtr&);
private:
  str::seq    targns;
  MonoTypePtr b;

  friend class MonoType;
  TAbs(const str::seq&, const MonoTypePtr&);
};

// a type-level application
class TApp : public MonoTypeCase<TApp> {
public:
  void show(std::ostream&) const;

  const MonoTypePtr& fn() const;
  const MonoTypes&   args() const;

  static const int type_case_id = 12;

  static MonoTypePtr make(const MonoTypePtr&, const MonoTypes&);
private:
  MonoTypePtr f;
  MonoTypes   targs;

  friend class MonoType;
  TApp(const MonoTypePtr&, const MonoTypes&);
};

// a recursive type
class Recursive : public MonoTypeCase<Recursive> {
public:
  void show(std::ostream&) const;

  const std::string& recTypeName() const;
  const MonoTypePtr& recType() const;

  static const int type_case_id = 13;

  static MonoTypePtr make(const std::string&, const MonoTypePtr&);
private:
  std::string tname;
  MonoTypePtr bty;

  friend class MonoType;
  Recursive(const std::string& tname, const MonoTypePtr& bty);
};

// expressions in types
//  (eventually we will need to eliminate the distinction between types and expressions)
class Expr;
typedef std::shared_ptr<Expr> ExprPtr;

class TExpr : public MonoTypeCase<TExpr> {
public:
  void show(std::ostream&) const;

  const ExprPtr& expr() const;

  static const int type_case_id = 14;

  static MonoTypePtr make(const ExprPtr&);
private:
  ExprPtr e;
  
  friend class MonoType;
  TExpr(const ExprPtr&);
};

template <typename Case>
  MonoTypeCase<Case>::MonoTypeCase() : MonoType(Case::type_case_id) {
  }

// simplify showing types (with or without simplifying type variable names)
std::string show(const PolyType& e);
std::string show(const PolyType* e);
std::string show(const PolyTypePtr& e);
std::string show(const QualType& e);
std::string show(const QualType* e);
std::string show(const QualTypePtr& e);
std::string show(const Constraint& e);
std::string show(const Constraint* e);
std::string show(const ConstraintPtr& e);
std::string show(const MonoType& e);
std::string show(const MonoType* e);
std::string show(const MonoTypePtr& e);

str::seq showNoSimpl(const MonoTypes&);
str::seq showNoSimpl(const Constraints&);
std::string showNoSimpl(const PolyType& e);
std::string showNoSimpl(const PolyType* e);
std::string showNoSimpl(const PolyTypePtr& e);
std::string showNoSimpl(const QualType& e);
std::string showNoSimpl(const QualType* e);
std::string showNoSimpl(const QualTypePtr& e);
std::string showNoSimpl(const Constraint& e);
std::string showNoSimpl(const Constraint* e);
std::string showNoSimpl(const ConstraintPtr& e);
std::string showNoSimpl(const MonoType& e);
std::string showNoSimpl(const MonoType* e);
std::string showNoSimpl(const MonoTypePtr& e);

// 'safely' consume types
//    (if the set of mono types is extended but functions on types aren't extended, it will be a compile-error)
template <typename T>
  struct switchType {
    virtual T with(const Prim*       v) const = 0;
    virtual T with(const OpaquePtr*  v) const = 0;
    virtual T with(const TVar*       v) const = 0;
    virtual T with(const TGen*       v) const = 0;
    virtual T with(const TAbs*       v) const = 0;
    virtual T with(const TApp*       v) const = 0;
    virtual T with(const FixedArray* v) const = 0;
    virtual T with(const Array*      v) const = 0;
    virtual T with(const Variant*    v) const = 0;
    virtual T with(const Record*     v) const = 0;
    virtual T with(const Func*       v) const = 0;
    virtual T with(const Exists*     v) const = 0;
    virtual T with(const Recursive*  v) const = 0;

    virtual T with(const TString* v) const = 0;
    virtual T with(const TLong*   v) const = 0;

    virtual T with(const TExpr*) const = 0;
  };

struct CPtr {
  template <typename T>
    struct as {
      typedef const T* ty;
    };
};

struct MPtr {
  template <typename T>
    struct as {
      typedef T* ty;
    };
};

template <typename T, typename Ptr, typename PtrC, typename F>
  T switchOfF(Ptr ty, F f) {
    typedef typename PtrC::template as<Prim>::ty       PrimT;
    typedef typename PtrC::template as<OpaquePtr>::ty  OpaquePtrT;
    typedef typename PtrC::template as<TVar>::ty       TVarT;
    typedef typename PtrC::template as<TGen>::ty       TGenT;
    typedef typename PtrC::template as<TAbs>::ty       TAbsT;
    typedef typename PtrC::template as<TApp>::ty       TAppT;
    typedef typename PtrC::template as<FixedArray>::ty FixedArrayT;
    typedef typename PtrC::template as<Array>::ty      ArrayT;
    typedef typename PtrC::template as<Variant>::ty    VariantT;
    typedef typename PtrC::template as<Record>::ty     RecordT;
    typedef typename PtrC::template as<Func>::ty       FuncT;
    typedef typename PtrC::template as<Exists>::ty     ExistsT;
    typedef typename PtrC::template as<Recursive>::ty  RecursiveT;
    typedef typename PtrC::template as<TString>::ty    TStringT;
    typedef typename PtrC::template as<TLong>::ty      TLongT;
    typedef typename PtrC::template as<TExpr>::ty      TExprT;

    switch (ty->case_id()) {
    case Prim::type_case_id:
      return f.with(rcast<PrimT>(ty));
    case OpaquePtr::type_case_id:
      return f.with(rcast<OpaquePtrT>(ty));
    case TVar::type_case_id:
      return f.with(rcast<TVarT>(ty));
    case TGen::type_case_id:
      return f.with(rcast<TGenT>(ty));
    case TAbs::type_case_id:
      return f.with(rcast<TAbsT>(ty));
    case TApp::type_case_id:
      return f.with(rcast<TAppT>(ty));
    case FixedArray::type_case_id:
      return f.with(rcast<FixedArrayT>(ty));
    case Array::type_case_id:
      return f.with(rcast<ArrayT>(ty));
    case Variant::type_case_id:
      return f.with(rcast<VariantT>(ty));
    case Record::type_case_id:
      return f.with(rcast<RecordT>(ty));
    case Func::type_case_id:
      return f.with(rcast<FuncT>(ty));
    case Exists::type_case_id:
      return f.with(rcast<ExistsT>(ty));
    case Recursive::type_case_id:
      return f.with(rcast<RecursiveT>(ty));
    case TString::type_case_id:
      return f.with(rcast<TStringT>(ty));
    case TLong::type_case_id:
      return f.with(rcast<TLongT>(ty));
    case TExpr::type_case_id:
      return f.with(rcast<TExprT>(ty));
    default:
      throw std::runtime_error("Internal error, cannot switch on unknown type: " + show(ty));
    }
  }

template <typename T>
  T switchOf(const MonoType& ty, const switchType<T>& f) {
    return switchOfF< T, const MonoType*, CPtr, const switchType<T>& >(&ty, f);
  }

template <typename T>
  T switchOf(const MonoTypePtr& ty, const switchType<T>& f) {
    return switchOfF< T, const MonoType*, CPtr, const switchType<T>& >(ty.get(), f);
  }

template <typename T>
  std::vector<T> switchOf(const MonoTypes& ts, const switchType<T>& f) {
    std::vector<T> result;
    for (MonoTypes::const_iterator t = ts.begin(); t != ts.end(); ++t) {
      result.push_back(switchOf(*t, f));
    }
    return result;
  }

template <typename T>
  T switchOf(const MonoTypes& ts, T s, T (*appendF)(T,T), const switchType<T>& f) {
    for (MonoTypes::const_iterator t = ts.begin(); t != ts.end(); ++t) {
      s = appendF(s, switchOf(*t, f));
    }
    return s;
  }

template <typename K, typename T>
  std::vector< std::pair<K, T> > switchOf(const std::vector< std::pair<K, MonoTypePtr> >& kts, const switchType<T>& f) {
    typedef std::pair<K, T> KT;
    typedef std::vector<KT> KTS;
    KTS r;
    for (typename std::vector< std::pair<K, MonoTypePtr> >::const_iterator kt = kts.begin(); kt != kts.end(); ++kt) {
      r.push_back(KT(kt->first, switchOf(kt->second, f)));
    }
    return r;
  }

template <typename T>
  Variant::Members switchOf(const Variant::Members& ms, const switchType<T>& f) {
    Variant::Members r;
    for (Variant::Members::const_iterator m = ms.begin(); m != ms.end(); ++m) {
      r.push_back(Variant::Member(m->selector, switchOf(m->type, f), m->id));
    }
    return r;
  }

template <typename T>
  Record::Members switchOf(const Record::Members& ms, const switchType<T>& f) {
    Record::Members r;
    for (Record::Members::const_iterator m = ms.begin(); m != ms.end(); ++m) {
      r.push_back(Record::Member(m->field, switchOf(m->type, f), m->offset));
    }
    return r;
  }

inline MonoTypes selectTypes(const Variant::Members& ms) {
  MonoTypes r;
  for (Variant::Members::const_iterator m = ms.begin(); m != ms.end(); ++m) {
    r.push_back(m->type);
  }
  return r;
}

inline str::seq selectNames(const Variant::Members& ms) {
  str::seq r;
  for (const auto& m : ms) {
    r.push_back(m.selector);
  }
  return r;
}

inline MonoTypes selectTypes(const Record::Members& ms) {
  MonoTypes r;
  for (Record::Members::const_iterator m = ms.begin(); m != ms.end(); ++m) {
    r.push_back(m->type);
  }
  return r;
}

inline str::seq selectNames(const Record::Members& ms) {
  str::seq r;
  for (const auto& m : ms) {
    r.push_back(m.field);
  }
  return r;
}

// walk a type structure for a side-effect (override any part to do per-constructor walks)
struct walkTy : public switchType<UnitV> {
  UnitV with(const Prim*       v) const;
  UnitV with(const OpaquePtr*  v) const;
  UnitV with(const TVar*       v) const;
  UnitV with(const TGen*       v) const;
  UnitV with(const TAbs*       v) const;
  UnitV with(const TApp*       v) const;
  UnitV with(const FixedArray* v) const;
  UnitV with(const Array*      v) const;
  UnitV with(const Variant*    v) const;
  UnitV with(const Record*     v) const;
  UnitV with(const Func*       v) const;
  UnitV with(const Exists*     v) const;
  UnitV with(const Recursive*  v) const;
  UnitV with(const TString*    v) const;
  UnitV with(const TLong*      v) const;
  UnitV with(const TExpr*      v) const;
};

// copy a type structure entirely (override any part to do a partial 'Type -> Type' transform)
struct switchTyFn : public switchType<MonoTypePtr> {
  MonoTypePtr with(const Prim*       v) const;
  MonoTypePtr with(const OpaquePtr*  v) const;
  MonoTypePtr with(const TVar*       v) const;
  MonoTypePtr with(const TGen*       v) const;
  MonoTypePtr with(const TAbs*       v) const;
  MonoTypePtr with(const TApp*       v) const;
  MonoTypePtr with(const FixedArray* v) const;
  MonoTypePtr with(const Array*      v) const;
  MonoTypePtr with(const Variant*    v) const;
  MonoTypePtr with(const Record*     v) const;
  MonoTypePtr with(const Func*       v) const;
  MonoTypePtr with(const Exists*     v) const;
  MonoTypePtr with(const Recursive*  v) const;

  // bleh
  MonoTypePtr with(const TString* v) const;
  MonoTypePtr with(const TLong*   v) const;

  // not quite as bleh
  MonoTypePtr with(const TExpr*) const;
};

MonoTypePtr clone(const MonoTypePtr&);
MonoTypePtr clone(const MonoType*);
MonoTypePtr clone(const MonoType&);

QualTypePtr cloneP(const QualTypePtr& p);
MonoTypePtr cloneP(const MonoTypePtr& p);

// simplify working with record types
QualTypePtr lookupFieldType(const QualTypePtr& qt, const std::string& fieldName);
MonoTypePtr lookupFieldType(const MonoTypePtr& mt, const std::string& fieldName);

// join two sets of constraints together (eliminating redundant constraints)
Constraints mergeConstraints(const Constraints& lhs, const Constraints& rhs);
void mergeConstraints(const Constraints& fcs, Constraints* tcs);

// polytype / gen utilities
MonoTypes tgens(int vs);
int tgenSize(const MonoTypePtr& mt);
int tgenSize(const MonoTypes& mts);
[[noreturn]] int tgenSize(const Constraints& cs);
int tgenSize(const ConstraintPtr& c);
[[noreturn]] int tgenSize(const QualTypePtr& qt);

typedef std::set<int> TGenVarSet;
TGenVarSet tgenVars(const MonoTypePtr&);

TVName        canonicalName(int v);
MonoTypeSubst canonicalNameSubst(const NameSet& ns);
TVName        freshName();
Names         freshNames(int vs);
MonoTypePtr   freshTypeVar();
MonoTypes     freshTypeVars(int vs);
MonoTypes     typeVars(const Names& ns);

MonoTypes     freshen(const MonoTypes&);
ConstraintPtr freshen(const ConstraintPtr&);
Constraints   freshen(const Constraints& cs);

QualTypePtr   instantiate(int vs, const QualTypePtr& scheme);
Constraints   instantiate(int vs, const Constraints& cs);
ConstraintPtr instantiate(int vs, const ConstraintPtr& c);
MonoTypePtr   instantiate(int vs, const MonoTypePtr& mt);
MonoTypes     instantiate(int vs, const MonoTypes& ts);
QualTypePtr   instantiate(const Names& ns, const QualTypePtr& scheme);
Constraints   instantiate(const Names& ns, const Constraints& cs);
ConstraintPtr instantiate(const Names& ns, const ConstraintPtr& c);
MonoTypePtr   instantiate(const Names& ns, const MonoTypePtr& mt);
MonoTypes     instantiate(const Names& ns, const MonoTypes& ts);
QualTypePtr   instantiate(const MonoTypes& ts, const QualTypePtr& scheme);
Constraints   instantiate(const MonoTypes& ts, const Constraints& cs);
ConstraintPtr instantiate(const MonoTypes& ts, const ConstraintPtr& c);
MonoTypePtr   instantiate(const MonoTypes& ts, const MonoTypePtr& mt);
MonoTypes     instantiate(const MonoTypes& ts, const MonoTypes& sts);

NameSet tvarNames(const QualTypePtr& qt);
NameSet tvarNames(const Constraints& cs);
NameSet tvarNames(const ConstraintPtr& c);
NameSet tvarNames(const MonoTypePtr& mt);
NameSet tvarNames(const MonoType& mt);
NameSet tvarNames(const MonoTypes&   mts);
void tvarNames(const QualTypePtr& qt,  NameSet* out);
void tvarNames(const Constraints& cs,  NameSet* out);
void tvarNames(const ConstraintPtr& c, NameSet* out);
void tvarNames(const MonoTypePtr& mt,  NameSet* out);
void tvarNames(const MonoType& mt,     NameSet* out);
void tvarNames(const MonoTypes& mts,   NameSet* out);

bool isFreeVarNameIn(const TVName&, const MonoTypePtr&);
bool isFreeVarNameIn(const TVName&, const MonoTypes&);

bool hasFreeVariables(const QualTypePtr&);
bool hasFreeVariables(const Constraints&);
bool hasFreeVariables(const ConstraintPtr&);
bool hasFreeVariables(const MonoTypePtr&);
bool hasFreeVariables(const MonoTypes&);

void show(const MonoTypeSubst& s, std::ostream& out);
std::string show(const MonoTypeSubst& s);

QualTypePtr   substitute(const MonoTypeSubst& s, const QualTypePtr& qt);
Constraints   substitute(const MonoTypeSubst& s, const Constraints& cs);
ConstraintPtr substitute(const MonoTypeSubst& s, const ConstraintPtr& p);
MonoTypePtr   substitute(const MonoTypeSubst& s, const MonoType& mt);
MonoTypePtr   substitute(const MonoTypeSubst& s, const MonoTypePtr& mt);
MonoTypes     substitute(const MonoTypeSubst& s, const MonoTypes& ts);

MonoTypePtr substituteStep(const MonoTypeSubst& s, const MonoTypePtr& mt);

PolyTypePtr generalize(const QualTypePtr& qt);

QualTypePtr   simplifyVarNames(const PolyType&);
QualTypePtr   simplifyVarNames(const PolyTypePtr&);
ConstraintPtr simplifyVarNames(const Constraint&);
ConstraintPtr simplifyVarNames(const ConstraintPtr&);
QualTypePtr   simplifyVarNames(const QualType&);
QualTypePtr   simplifyVarNames(const QualTypePtr&);
MonoTypePtr   simplifyVarNames(const MonoType&);
MonoTypePtr   simplifyVarNames(const MonoTypePtr&);
MonoTypes     simplifyVarNames(const MonoTypes& mts);

// determine a single step of type alias removal (or the identity function at types that are already representation-determined)
MonoTypePtr repTypeStep(const MonoTypePtr&);

// forget type aliases, determine a final reduced representation type (the fixed point of repTypeStep)
MonoTypePtr repType(const MonoTypePtr&);

// simplify working with type structures lifting of C++ types
inline MonoTypePtr primty(const char* x) {
  return Prim::make(x);
}

inline MonoTypePtr primty(const char* x, const MonoTypePtr& aty) {
  return Prim::make(x, aty);
}

inline QualTypePtr qualtype(const Constraints& cs, const MonoTypePtr& p) {
  return QualTypePtr(new QualType(cs, p));
}

inline QualTypePtr qualtype(const Constraints& cs, MonoType* mt) {
  return qualtype(cs, MonoTypePtr(mt));
}

inline QualTypePtr qualtype(const MonoTypePtr& p) {
  return QualTypePtr(new QualType(p));
}

inline QualTypePtr qualtype(MonoType* mt) {
  return QualTypePtr(new QualType(MonoTypePtr(mt)));
}

inline QualTypePtr qualtype(const char* x) {
  return qualtype(primty(x));
}

inline PolyTypePtr polytype(int tvs, const QualTypePtr& qt) {
  return PolyTypePtr(new PolyType(tvs, qt));
}

inline PolyTypePtr polytype(const QualTypePtr& qt) {
  return PolyTypePtr(new PolyType(qt));
}

inline PolyTypePtr polytype(const MonoTypePtr& p) {
  return polytype(qualtype(p));
}

inline PolyTypePtr polytype(MonoType* mt) {
  return polytype(MonoTypePtr(mt));
}

inline PolyTypePtr polytype(const char* x) {
  return polytype(primty(x));
}

inline MonoTypePtr tgen(int i) {
  return TGen::make(i);
}

inline MonoTypePtr tvar(const std::string& vn) {
  return TVar::make(vn);
}

inline MonoTypePtr tstring(const std::string& x) {
  return TString::make(x);
}

inline MonoTypePtr tuplety(const MonoTypes& mtys = MonoTypes()) {
  if (mtys.size() == 0) {
    return primty("unit");
  } else {
    Record::Members ms;
    for (unsigned int i = 0; i < mtys.size(); ++i) {
      ms.push_back(Record::Member(".f" + str::from(i), mtys[i]));
    }
    return Record::make(ms);
  }
}

inline MonoTypePtr sumtype(const MonoTypePtr& t0, const MonoTypePtr& t1) {
  Variant::Members ms;
  ms.push_back(Variant::Member(".f0", t0, 0));
  ms.push_back(Variant::Member(".f1", t1, 1));
  return Variant::make(ms);
}

inline MonoTypePtr maybety(const MonoTypePtr& t) {
  return sumtype(primty("unit"), t);
}

inline MonoTypePtr isMaybe(const MonoTypePtr& t) {
  if (const Variant* vt = is<Variant>(t)) {
    const Variant::Members& ms = vt->members();
    if (ms.size() == 2 && ms[0].selector == ".f0" && ms[1].selector == ".f1" && ms[0].type == primty("unit")) {
      return ms[1].type;
    }
  }
  return MonoTypePtr();
}

inline MonoTypePtr tstrings(const std::vector<std::string>& xs) {
  MonoTypes ts;
  for (const auto& x : xs) {
    ts.push_back(tstring(x));
  }
  return tuplety(ts);
}

inline MonoTypePtr functy(const MonoTypePtr& aty, const MonoTypePtr& rty) {
  return Func::make(aty, rty);
}

inline MonoTypePtr functy(const MonoTypes& atys, const MonoTypePtr& rty) {
  if (atys.size() == 0) {
    return Func::make(tuplety(list(tuplety())), rty);
  } else {
    return Func::make(tuplety(atys), rty);
  }
}

inline MonoTypePtr closty(const MonoTypePtr& aty, const MonoTypePtr& rty) {
  return Exists::make("E", tuplety(list(functy(tuplety(list(tvar("E"), aty)), rty), tvar("E"))));
}

inline MonoTypePtr closty(const MonoTypes& atys, const MonoTypePtr& rty) {
  return Exists::make("E", tuplety(list(functy(tuplety(cons(tvar("E"), atys)), rty), tvar("E"))));
}

inline MonoTypePtr tabs(const str::seq& tns, const MonoTypePtr& body) {
  return TAbs::make(tns, body);
}

inline MonoTypePtr tapp(const MonoTypePtr& f, const MonoTypes& args) {
  return TApp::make(f, args);
}

inline MonoTypePtr tlong(long x) {
  return TLong::make(x);
}

inline MonoTypePtr texpr(const ExprPtr& x) {
  return TExpr::make(x);
}

bool isMonoSingular(const MonoType&);
bool isMonoSingular(const MonoType*);
bool isMonoSingular(const MonoTypePtr&);
bool isMonoSingular(const QualTypePtr&);
bool isMonotype(const QualTypePtr& qt);
bool isMonotype(const PolyTypePtr& pt);
MonoTypePtr requireMonotype(const QualTypePtr& qt);
MonoTypePtr requireMonotype(const PolyTypePtr& pt);
MonoTypes requireMonotype(const PolyTypes& pts);

inline MonoTypePtr arrayty(const MonoTypePtr& ty, size_t n) {
  return FixedArray::make(ty, tlong(n));
}

inline MonoTypePtr arrayty(const MonoTypePtr& ty) {
  return Array::make(ty);
}

template <typename T>
  MonoTypePtr opaqueptr(bool insertcontig) {
    return OpaquePtr::make(str::demangle<T>(), insertcontig ? sizeof(T) : 0, insertcontig);
  }

inline bool isVoid(const MonoTypePtr& t) {
  if (Prim* pt = is<Prim>(t)) {
    return pt->name() == "void";
  } else {
    return false;
  }
}

// determine the size of monotypes
unsigned int sizeOf(const MonoTypePtr& mt);

// a type is equivalent to unit if its size is 0
inline bool isUnit(const MonoTypePtr& t) {
  try {
    return sizeOf(t) == 0;
  } catch (std::exception&) {
    return false; // well if we can't get a size, it must not be unit (yet?)
  }
}

// does this string refer to a named primitive type?
bool isPrimName(const std::string& tn);

// unroll a recursive type
MonoTypePtr unroll(const MonoTypePtr&);

// support an efficient binary codec for type descriptions
void encode(const QualTypePtr&, std::ostream&);
void encode(const MonoTypePtr&, std::ostream&);

void decode(QualTypePtr*, std::istream&);
void decode(MonoTypePtr*, std::istream&);

void        encode(const QualTypePtr&, std::vector<unsigned char>*);
void        encode(const MonoTypePtr&, std::vector<unsigned char>*);
MonoTypePtr decode(const std::vector<unsigned char>& in);
MonoTypePtr decode(const unsigned char* b, const unsigned char* e);

// open all opaque type aliases
MonoTypePtr unalias(const MonoTypePtr&);

// record type constructors
template <typename ... Xs>
  struct accumRecTy {
    static void accum(Record::Members*) {
    }
  };

template <typename ... Xs>
  struct accumRecTy<const char*, MonoTypePtr, Xs...> {
    static void accum(Record::Members* r, const char* n, const MonoTypePtr& t, Xs ... xs) {
      r->push_back(Record::Member(n, t));
      accumRecTy<Xs...>::accum(r, xs...);
    }
  };

template <typename ... Xs>
  inline MonoTypePtr makeRecordType(Xs ... xs) {
    Record::Members ms;
    accumRecTy<Xs...>::accum(&ms, xs...);
    return Record::make(ms);
  }


// some temporary abstraction/hacking on the way to fully general type constructor form
inline MonoTypePtr fnresult(const MonoTypePtr& fty) {
  if (const Func* f = is<Func>(fty)) {
    return f->result();
  } else if (const TApp* fa = is<TApp>(fty)) {
    if (const Prim* fn = is<Prim>(fa->fn())) {
      if (fn->name() == "->" && fa->args().size() == 2) {
        return fa->args()[1];
      }
    }
  }

  throw std::runtime_error("Expected function type: " + show(fty));
}

void compactMTypeMemory();

// shorthand for making file ref types
MonoTypePtr fileRefTy(const MonoTypePtr&, const MonoTypePtr&);
MonoTypePtr fileRefTy(const MonoTypePtr&);

// hash record and variant members
template <>
  struct genHash<Record::Member> {
    inline size_t operator()(const Record::Member& x) const {
      typedef std::tuple<std::string, MonoTypePtr, unsigned int> RMTup;
      static genHash<RMTup> h;
      return h(RMTup(x.field, x.type, x.offset));
    }
  };
template <>
  struct genHash<Variant::Member> {
    inline size_t operator()(const Variant::Member& x) const {
      typedef std::tuple<std::string, MonoTypePtr, unsigned int> VMTup;
      static genHash<VMTup> h;
      return h(VMTup(x.selector, x.type, x.id));
    }
  };
template <>
  struct genHash<MonoTypePtr> {
    inline size_t operator()(const MonoTypePtr& x) const {
      static std::hash<void*> h;
      return h(rcast<void*>(x.get()));
    }
  };

}

#endif

