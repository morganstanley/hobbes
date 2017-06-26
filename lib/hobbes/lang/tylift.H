
#ifndef HOBBES_LANG_TYPELIFT_HPP_INCLUDED
#define HOBBES_LANG_TYPELIFT_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/util/array.H>
#include <hobbes/util/ptr.H>
#include <hobbes/util/str.H>
#include <hobbes/util/variant.H>
#include <hobbes/util/preprocessor.H>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <stdexcept>

namespace hobbes {

/*********
 * typedb : an interface to a type environment
 *********/
struct typedb {
  // define new opaque types with an internal representation
  virtual MonoTypePtr defineNamedType(const std::string& name, const str::seq& argNames, const MonoTypePtr& ty) = 0;
  virtual bool isTypeName(const std::string&) const = 0;
  virtual MonoTypePtr namedTypeRepresentation(const std::string&) const = 0;

  // define new type aliases
  virtual void defineTypeAlias(const std::string& name, const str::seq& argNames, const MonoTypePtr& ty) = 0;
  virtual bool isTypeAliasName(const std::string& name) const = 0;
  virtual MonoTypePtr replaceTypeAliases(const MonoTypePtr& ty) const = 0;

  // handle wrapping C++ types
  virtual PolyTypePtr opaquePtrPolyType(const std::type_info&, unsigned int sz, bool inStruct) = 0;
  virtual MonoTypePtr opaquePtrMonoType(const std::type_info&, unsigned int sz, bool inStruct) = 0;

  // insert subtype coercions as necessary to witness C++ inheritance
  virtual PolyTypePtr generalize(const MonoTypePtr& mt) const = 0;
};

struct nulltypedb : public typedb {
  MonoTypePtr defineNamedType(const std::string& name, const str::seq& argNames, const MonoTypePtr& ty);
  bool isTypeName(const std::string&) const;
  MonoTypePtr namedTypeRepresentation(const std::string&) const;

  void defineTypeAlias(const std::string& name, const str::seq& argNames, const MonoTypePtr& ty);
  bool isTypeAliasName(const std::string& name) const;
  MonoTypePtr replaceTypeAliases(const MonoTypePtr& ty) const;
  
  PolyTypePtr opaquePtrPolyType(const std::type_info&, unsigned int sz, bool inStruct);
  MonoTypePtr opaquePtrMonoType(const std::type_info&, unsigned int sz, bool inStruct);
 
  PolyTypePtr generalize(const MonoTypePtr& mt) const;
};
extern nulltypedb nulltdb;

/*************
 * C++ representations for common hobbes types
 *************/

// allow opaque type aliases to reuse basic types
template <const char* TN, typename T>
  struct typeAlias {
    typeAlias(const T& v) : value(v) { }
    typeAlias() { }
    T value;
  };

// allow the generic definition of simple recursive types
template <typename T>
  struct recursive {
    T value;
  };
struct recursion {
  void* value;
  recursion(void* x) : value(x) { }
};

// represent arrays of unknown length by a size and a variable-length array payload
template <typename T>
  struct array {
    typedef array<T>* ptr;

    long size;
    T    data[]; // unknown length, bytes inline with this structure
  };
template <typename T> const T* begin(const array<T>* d) { return d->data; }
template <typename T> const T* end  (const array<T>* d) { return d->data + d->size; }
template <typename T>       T* begin(      array<T>* d) { return d->data; }
template <typename T>       T* end  (      array<T>* d) { return d->data + d->size; }

template <typename T> const T* begin(const array<T>& d) { return d.data; }
template <typename T> const T* end  (const array<T>& d) { return d.data + d.size; }
template <typename T>       T* begin(      array<T>& d) { return d.data; }
template <typename T>       T* end  (      array<T>& d) { return d.data + d.size; }

// a linked list is a recursive sum of nested tuples starting at unit
template <typename T>
  struct seq {
    typedef std::pair<T, seq<T>*> pair_t;
    typedef variant<unit, pair_t> rep_t;
    rep_t data;

    seq() : data(unit()) { }
    seq(const T& x, seq<T>* xs) : data(pair_t(x, xs)) { }

    bool empty() const { return get<unit>(this->data) != 0; }
    const pair_t* head() const { return get<pair_t>(this->data); }
  };

/******************
 * lift(T) : map C++ types to hobbes type descriptions
 ******************/
template <typename T, bool InStruct = false, typename P = void>
  struct lift;

// the default lift just refers to opaque C++ data (either as pointers or inline data)
template <typename T, bool InStruct> struct opaquePtrLift { };
template <typename T> struct opaquePtrLift<T,true>  { static MonoTypePtr type() { return OpaquePtr::make(str::demangle<T>(), sizeof(T), true); } };
template <typename T> struct opaquePtrLift<T,false> { static MonoTypePtr type() { return OpaquePtr::make(str::demangle<T>(), 0, false); } };

// also by default : strip 'const' annotations and rewrite references to pointers
template <typename T, bool InStruct> struct defaultLift { static MonoTypePtr type(typedb&) { return opaquePtrLift<T,InStruct>::type(); } };
template <typename T, bool InStruct> struct defaultLift<const T*, InStruct> : public lift<T*, InStruct> { };
template <typename T, bool InStruct> struct defaultLift<const T&, InStruct> : public lift<T*, InStruct> { };
template <typename T, bool InStruct> struct defaultLift<T&,       InStruct> : public lift<T*, InStruct> { };

template <typename T, bool InStruct>
  struct defaultLift<T*, InStruct> {
    static MonoTypePtr type(typedb& tenv) {
      return tenv.opaquePtrMonoType(typeid(T), 0, false);
    }
  };
template <typename T, bool InStruct, typename P>
  struct lift {
    static MonoTypePtr type(typedb& tenv) {
      return defaultLift<T, InStruct>::type(tenv);
    }
  };

// we can lift a list of T as ^x.()+T*x, represented by a pointer whether top-level or inline
template <typename T>
  struct liftSeq {
    static MonoTypePtr type(typedb& tenv) {
      // ^x.()+T*x
      return Recursive::make("x", Variant::make(list(Variant::Member(".f0", primty("unit"), 0), Variant::Member(".f1", tuple(list(lift<T, true>::type(tenv), tvar("x"))), 1))));
    }
  };

template <typename T, bool InStruct>
  struct lift< seq<T>*, InStruct > : public liftSeq<T> { };

// or a generic recursive type as with lists
template <typename T>
  struct liftRecursive {
    static MonoTypePtr type(typedb& tenv) {
      return Recursive::make("x", lift<T, true>::type(tenv));
    }
  };

template <typename T, bool InStruct>
  struct lift< recursive<T>*, InStruct > : public liftRecursive<T> { };

template <bool InStruct>
  struct lift< recursion, InStruct > {
    static MonoTypePtr type(typedb&) {
      return tvar("x");
    }
  };

// we can lift any pair as a tuple (at the top-level must be a pointer rep, else not)
template <typename F, typename S>
  struct liftPair {
    static MonoTypePtr type(typedb& tenv) {
      typedef std::pair<F,S> FS;
      Record::Members fs;
      fs.push_back(Record::Member(".f0", lift<F, true>::type(tenv), offsetof(FS, first)));
      fs.push_back(Record::Member(".f1", lift<S, true>::type(tenv), offsetof(FS, second)));
      return Record::make(fs);
    }
  };

template <typename F, typename S>
  struct lift< std::pair<F, S>, true > : public liftPair<F, S> { };
template <typename F, typename S>
  struct lift< std::pair<F, S>*, false > : public liftPair<F, S> { };

template <>
  struct lift< std::string, true > { static MonoTypePtr type(typedb&) { return OpaquePtr::make("std::string", sizeof(std::string), true); }  };
template <>
  struct lift< std::string*, false > { static MonoTypePtr type(typedb&) { return OpaquePtr::make("std::string", 0, false); }  };

// lift only indeterminate-length arrays passed by reference
// (arrays are always represented by a pointer but should internally store inline)
template <typename T>
  struct liftVarArray {
    static MonoTypePtr type(typedb& tenv) {
      return Array::make(lift<T, true>::type(tenv));
    }
  };

template <typename T, bool InStruct>
  struct lift< array<T>*, InStruct> : public liftVarArray<T> { };

// lift the basic primitive types
#define HOBBES_LIFT_PRIMITIVE(T, n) \
  template <bool InStruct> \
    struct lift<T, InStruct> { \
      static MonoTypePtr type(typedb&) { \
        return Prim::make(#n); \
      } \
    }

HOBBES_LIFT_PRIMITIVE(void,           unit);
HOBBES_LIFT_PRIMITIVE(unit,           unit);
HOBBES_LIFT_PRIMITIVE(bool,           bool);
HOBBES_LIFT_PRIMITIVE(char,           char);
HOBBES_LIFT_PRIMITIVE(unsigned char,  byte);
HOBBES_LIFT_PRIMITIVE(short,          short);
HOBBES_LIFT_PRIMITIVE(unsigned short, short);
HOBBES_LIFT_PRIMITIVE(int,            int);
HOBBES_LIFT_PRIMITIVE(unsigned int,   int);
HOBBES_LIFT_PRIMITIVE(long,           long);
HOBBES_LIFT_PRIMITIVE(unsigned long,  long);
HOBBES_LIFT_PRIMITIVE(float,          float);
HOBBES_LIFT_PRIMITIVE(double,         double);

template <typename T>
  inline MonoTypePtr prim() { return lift<T, false>::type(nulltdb); }

// introduce opaque (named) alias types
template <const char* TN, typename T, bool InStruct>
  struct lift< typeAlias<TN, T>, InStruct > {
    static MonoTypePtr type(typedb& tenv) {
      return tenv.defineNamedType(TN, str::seq(), lift<T, InStruct>::type(tenv));
    }
  };

template <const char* TN, typename T, bool InStruct>
  struct lift< typeAlias<TN, T>*, InStruct > {
    static MonoTypePtr type(typedb& tenv) {
      return tenv.defineNamedType(TN, str::seq(), lift<T*, InStruct>::type(tenv));
    }
  };

// generic case for fixed-size arrays (has to be by reference to match)
// values are stored inline in fixed-size arrays
template <typename T, size_t N>
  struct liftFixedArray {
    static MonoTypePtr type(typedb& tenv) {
      return arrayty(lift<T, true>::type(tenv), N);
    }
  };

template <typename T, size_t N, bool InStruct>
  struct lift<T(&)[N], InStruct> : public liftFixedArray<T, N> { };
template <typename T, size_t N, bool InStruct>
  struct lift<const T(&)[N], InStruct> : public liftFixedArray<T, N> { };
template <typename T, size_t N, bool InStruct>
  struct lift<T(*)[N], InStruct> : public liftFixedArray<T, N> { };
template <typename T, size_t N, bool InStruct>
  struct lift<const T(*)[N], InStruct> : public liftFixedArray<T, N> { };
template <typename T, size_t N, bool InStruct>
  struct lift<T[N], InStruct> : public liftFixedArray<T, N> { };
template <typename T, size_t N, bool InStruct>
  struct lift<const T[N], InStruct> : public liftFixedArray<T, N> { };

// generated lifts for C functions
template <typename ... Ts>
  struct typeSeq {
    static void accum(MonoTypes*, typedb&) { }
    static MonoTypes liftSeq(typedb&) { return MonoTypes(); }
  };
template <typename T, typename ... Ts>
  struct typeSeq<T, Ts...> {
    static void accum(MonoTypes* r, typedb& tenv) {
      r->push_back(lift<T, false>::type(tenv));
      typeSeq<Ts...>::accum(r, tenv);
    }
    static MonoTypes liftSeq(typedb& tenv) { MonoTypes r; accum(&r, tenv); return r; }
  };

template <typename F>
  struct liftFunction { };

template <typename R, typename ... Args>
  struct liftFunction<R(Args...)> {
    static MonoTypePtr type(typedb& tenv) {
      return functy(typeSeq<Args...>::liftSeq(tenv), lift<R, false>::type(tenv));
    }
  };

template <typename R, typename ... Args>
  struct lift<R(Args...), true> : public liftFunction<R(Args...)> { };
template <typename R, typename ... Args>
  struct lift<R(Args...), false> : public liftFunction<R(Args...)> { };
template <typename R, typename ... Args>
  struct lift<R(*)(Args...), true> : public liftFunction<R(Args...)> { };
template <typename R, typename ... Args>
  struct lift<R(*)(Args...), false> : public liftFunction<R(Args...)> { };

// lift variant types
// -- first by lifting constructor names
//       if label(N,T) then the constructor name is N, else it's derived from its position in the total sum type
// -- then as with the hobbes::variant layout
template <const char* TN, typename T>
  struct label {
    typedef T type;
    T value;
    label(const T& x) : value(x) { }
    static std::string name() { return TN; }
  };
template <typename T, typename P = void>
  struct liftVarCtor {
    typedef T type;
    static std::string labelText(int idx) { return ".f" + str::from(idx); }
  };
template <const char* TN, typename T>
  struct liftVarCtor< label<TN, T> > {
    typedef typename label<TN, T>::type type;
    static std::string labelText(int idx) { return label<TN, T>::name(); }
  };
template <typename ... CTys>
  struct liftVarCtors {
    static void constructors(typedb&, Variant::Members*) { }
  };
template <typename CTy, typename ... CTys>
  struct liftVarCtors<CTy, CTys...> {
    static void constructors(typedb& tenv, Variant::Members* ms) {
      ms->push_back(Variant::Member(liftVarCtor<CTy>::labelText(ms->size()), lift< typename liftVarCtor<CTy>::type, true >::type(tenv), ms->size()));
      liftVarCtors<CTys...>::constructors(tenv, ms);
    }
  };

template <typename T>
  struct liftVariant { };
template <typename ... CTys>
  struct liftVariant< variant<CTys...> > {
    static MonoTypePtr type(typedb& tenv) {
      Variant::Members ms;
      liftVarCtors<CTys...>::constructors(tenv, &ms);
      MonoTypePtr result = Variant::make(ms);
      const Variant* vty = is<Variant>(result);
      size_t vsz = vty->size();
      size_t csz = sizeof(variant<CTys...>);
      if (vsz != csz) {
        size_t offset = vty->payloadOffset();
        size_t psz    = vty->payloadSize();
        throw std::runtime_error(
          "Internal error, computed size for variant '" + show(result) + "' (" +
          str::from(offset) + "+" + str::from(psz) + "(+" + str::from(vsz - (offset + psz)) +
          ")) inconsistent with C++ memory layout (size=" + str::from(csz) + ")"
        );
      }
      return result;
    }
  };

template <typename ... CTys>
  struct lift< variant<CTys...>, true > : public liftVariant< variant<CTys...> > { };
template <typename ... CTys>
  struct lift< variant<CTys...>*, false > : public liftVariant< variant<CTys...> > { };

// allow basic reflective enumerations
#define HOBBES_ENUM_CTOR(n, v) n = v,
#define HOBBES_ENUM_METAENT(n, v) r.push_back(MetaEnt(#n, v));

#define DEFINE_ENUM(T, CTORS...) \
  struct T { \
    enum Enum : unsigned int { \
      _HOBPP_MAP(HOBBES_ENUM_CTOR, CTORS) \
    }; \
    explicit operator bool() const = delete; \
    \
    typedef void is_hobbes_enum; \
    typedef std::pair<std::string, unsigned int> MetaEnt; \
    typedef std::vector<MetaEnt> MetaSeq; \
    \
    static MetaSeq meta() { \
      MetaSeq r; \
      _HOBPP_MAP(HOBBES_ENUM_METAENT, CTORS) \
      return r; \
    } \
    \
    Enum value; \
    operator Enum() const { \
      return this->value; \
    } \
    T& operator=(Enum x) { \
      this->value = x; \
      return *this; \
    } \
    T() : value(Enum()) { } \
    T(const T& x) : value(x.value) { } \
    T(Enum x) : value(x) { } \
  }

template <typename T>
  struct liftEnum {
    static MonoTypePtr type(typedb&) {
      Variant::Members vms;

      typedef typename T::MetaSeq MS;
      MS ms = T::meta();
      for (typename MS::const_iterator m = ms.begin(); m != ms.end(); ++m) {
        vms.push_back(Variant::Member(m->first, Prim::make("unit"), (unsigned int)(m->second)));
      }

      return Variant::make(vms);
    }
  };

template <typename T>
  struct lift<T, true, typename T::is_hobbes_enum> : public liftEnum<T> { };
template <typename T>
  struct lift<T*, false, typename T::is_hobbes_enum> : public liftEnum<T> { };

// allow basic reflective structs
#define HOBBES_STRUCT_FIELD(t, n) t n;
#define HOBBES_META_FIELD(t, n) r.push_back(::hobbes::Record::Member(#n, ::hobbes::lift<t, true>::type(tenv), offsetof(SelfT, n)));

#define DEFINE_STRUCT(T, FIELDS...) \
  struct T { \
    _HOBPP_MAP(HOBBES_STRUCT_FIELD, FIELDS) \
    \
    typedef void is_hobbes_struct; \
    typedef T SelfT; \
    static ::hobbes::MonoTypePtr meta(::hobbes::typedb& tenv) { \
      ::hobbes::Record::Members r; \
      _HOBPP_MAP(HOBBES_META_FIELD, FIELDS) \
      return ::hobbes::Record::make(r); \
    } \
  }

template <typename T>
  struct liftStruct {
    static MonoTypePtr type(typedb& tenv) {
      return T::meta(tenv);
    }
  };

template <typename T>
  struct lift<T, true, typename T::is_hobbes_struct> : public liftStruct<T> { };
template <typename T>
  struct lift<T*, false, typename T::is_hobbes_struct> : public liftStruct<T> { };

// perhaps some lifts might depend on their C++ values
template <typename T>
  struct liftValue {
    static MonoTypePtr type(typedb& tenv, T) {
      return lift<T>::type(tenv);
    }
  };

}

#endif

