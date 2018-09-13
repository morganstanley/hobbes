/*
 * reflect : common definitions to help introduce zero-overhead reflective types (consistent with algebraic data types in hobbes) and write their meta-data descriptions
 *
 * A macro 'DEFINE_STRUCT(T, (T0, fn0), ... (TN, fnN))' is included to be equivalent to '{fn0:T0, ..., fnN:TN}'
 * A macro 'DEFINE_VARIANT(T, (cn0, T0), ... (cnN, TN))' is included to be equivalent to '|cn0:T0, ..., cnN:TN|'
 * A type 'tuple<T0, ..., TN>' is included to be equivalent to '(T0 * ... * TN)' and have standard memory layout
 * A type 'variant<T0, ..., TN>' is included to be equivalent to '(T0 + ... + TN)' and have standard memory layout
 *
 * functions are also included to read and write serialized type descriptions
 */

#ifndef HOBBES_REFLECT_H_INCLUDED
#define HOBBES_REFLECT_H_INCLUDED

#include <memory>
#include <string>
#include <vector>
#include <iostream>
#include <sstream>
#include <assert.h>
#include <stdexcept>
#include <type_traits>

namespace hobbes {

// very basic macro metaprogramming
#define PRIV_HPPF_FIRST(a, ...) a
#define PRIV_HPPF_SECOND(a, b, ...) b
#define PRIV_HPPF_JOIN(a,b) a ## b
#define PRIV_HPPF_IS_NEGATE(...) PRIV_HPPF_SECOND(__VA_ARGS__, 0)
#define PRIV_HPPF_NOT(x) PRIV_HPPF_IS_NEGATE(PRIV_HPPF_JOIN(PRIV_HPPF_SNOT_, x))
#define PRIV_HPPF_SNOT_0 NEGATE, 1
#define PRIV_HPPF_BOOL(x) PRIV_HPPF_NOT(PRIV_HPPF_NOT(x))
#define PRIV_HPPF_IF_ELSE(condition) PRIV_HPPF_SIF_ELSE(PRIV_HPPF_BOOL(condition))
#define PRIV_HPPF_SIF_ELSE(condition) PRIV_HPPF_JOIN(PRIV_HPPF_SIF_, condition)
#define PRIV_HPPF_SIF_1(...) __VA_ARGS__ PRIV_HPPF_SIF_1_ELSE
#define PRIV_HPPF_SIF_0(...)             PRIV_HPPF_SIF_0_ELSE
#define PRIV_HPPF_SIF_1_ELSE(...)
#define PRIV_HPPF_SIF_0_ELSE(...) __VA_ARGS__
#define PRIV_HPPF_EMPTY()
#define PRIV_HPPF_EVAL(...) PRIV_HPPF_EVAL256(__VA_ARGS__)
#define PRIV_HPPF_EVAL256(...) PRIV_HPPF_EVAL128(PRIV_HPPF_EVAL128(__VA_ARGS__))
#define PRIV_HPPF_EVAL128(...) PRIV_HPPF_EVAL64(PRIV_HPPF_EVAL64(__VA_ARGS__))
#define PRIV_HPPF_EVAL64(...) PRIV_HPPF_EVAL32(PRIV_HPPF_EVAL32(__VA_ARGS__))
#define PRIV_HPPF_EVAL32(...) PRIV_HPPF_EVAL16(PRIV_HPPF_EVAL16(__VA_ARGS__))
#define PRIV_HPPF_EVAL16(...) PRIV_HPPF_EVAL8(PRIV_HPPF_EVAL8(__VA_ARGS__))
#define PRIV_HPPF_EVAL8(...) PRIV_HPPF_EVAL4(PRIV_HPPF_EVAL4(__VA_ARGS__))
#define PRIV_HPPF_EVAL4(...) PRIV_HPPF_EVAL2(PRIV_HPPF_EVAL2(__VA_ARGS__))
#define PRIV_HPPF_EVAL2(...) PRIV_HPPF_EVAL1(PRIV_HPPF_EVAL1(__VA_ARGS__))
#define PRIV_HPPF_EVAL1(...) __VA_ARGS__
#define PRIV_HPPF_DEFER2(m) m PRIV_HPPF_EMPTY PRIV_HPPF_EMPTY()()
#define PRIV_HPPF_HAS_PARGS(...) PRIV_HPPF_BOOL(PRIV_HPPF_FIRST(PRIV_HPPF_SEOAP_ __VA_ARGS__)())
#define PRIV_HPPF_SEOAP_(...) PRIV_HPPF_BOOL(PRIV_HPPF_FIRST(PRIV_HPPF_SEOA_ __VA_ARGS__)())
#define PRIV_HPPF_SEOA_() 0
#define PRIV_HPPF_MAP(f, VS...) PRIV_HPPF_EVAL(PRIV_HPPF_MAPP(f, VS))
#define PRIV_HPPF_MAPP(f, H, T...)        \
  f H                                 \
  PRIV_HPPF_IF_ELSE(PRIV_HPPF_HAS_PARGS(T))(  \
    PRIV_HPPF_DEFER2(PRIV_HPPF_SMAPP)()(f, T) \
  )(                                  \
  )
#define PRIV_HPPF_SMAPP() PRIV_HPPF_MAPP

// value-level bool -> type-level bool
template <bool f> struct tbool { };
template <>       struct tbool<true> { typedef void type; };

// a standard layout tuple type
constexpr size_t alignTo(size_t x, size_t a) {
  return (x % a == 0) ? x : (a * (1 + x/a));
}
constexpr size_t szmax(size_t x, size_t y) {
  return (x > y) ? x : y;
}

template <size_t index, size_t base, typename ... Fields>
  struct offsetInfo {
    static const size_t offset     = base;
    static const size_t malignment = 1;
    static const size_t size       = 0;

    static void defInit(uint8_t*) { }
    static void initFrom(uint8_t*, const Fields&...) { }
    static void copyFrom(uint8_t*, const uint8_t*) { }
    static void destroy(uint8_t*) { }
    static bool eq(const uint8_t*, const uint8_t*) { return true; }
  };
template <size_t index, size_t base, typename Field, typename ... Fields>
  struct offsetInfo<index, base, Field, Fields...> {
    static const size_t offset = alignTo(base, alignof(Field));

    typedef offsetInfo<index+1, offset+sizeof(Field), Fields...> tail;

    static const size_t malignment = szmax(alignof(Field), tail::malignment);
    static const size_t size       = (offset-base) + sizeof(Field) + tail::size;

    static void defInit(uint8_t* b) {
      new (b+offset) Field();
      tail::defInit(b);
    }
    static void initFrom(uint8_t* b, const Field& f, const Fields&... fs) {
      new (b+offset) Field(f);
      tail::initFrom(b, fs...);
    }
    static void copyFrom(uint8_t* lhs, const uint8_t* rhs) {
      new (lhs+offset) Field(*reinterpret_cast<const Field*>(rhs+offset));
      tail::copyFrom(lhs, rhs);
    }
    static void destroy(uint8_t* b) {
      reinterpret_cast<Field*>(b+offset)->~Field();
      tail::destroy(b);
    }
    static bool eq(const uint8_t* lhs, const uint8_t* rhs) {
      return (*reinterpret_cast<const Field*>(lhs+offset) == *reinterpret_cast<const Field*>(rhs+offset)) ? tail::eq(lhs,rhs) : false;
    }
  };

template <size_t n, typename Offs>
  struct offsetAt : public offsetAt<n-1, typename Offs::tail> { };
template <typename Offs>
  struct offsetAt<0, Offs> { static const size_t value = Offs::offset; };

template <size_t, typename ... Fields>
  struct nth { };
template <typename Field, typename ... Fields>
  struct nth<0, Field, Fields...> { typedef Field type; };
template <size_t n, typename Field, typename ... Fields>
  struct nth<n, Field, Fields...> : nth<n-1, Fields...> { };

template <typename ... Fields>
  struct tuple {
    typedef offsetInfo<0, 0, Fields...> offs;
    static const size_t alignment = offs::malignment;
    static const size_t size      = alignTo(offs::size, offs::malignment);
    uint8_t buffer[size];

    tuple() {
      offs::defInit(this->buffer);
    }
    tuple(const Fields&... fs) {
      offs::initFrom(this->buffer, fs...);
    }
    tuple(const tuple<Fields...>& rhs) {
      offs::copyFrom(this->buffer, rhs.buffer);
    }
    ~tuple() {
      offs::destroy(this->buffer);
    }
    tuple<Fields...>& operator=(const tuple<Fields...>& rhs) {
      if (this != &rhs) {
        offs::destroy(this->buffer);
        offs::copyFrom(this->buffer, rhs.buffer);
      }
      return *this;
    }

    template <size_t k>
      typename nth<k, Fields...>::type* atP() {
        return reinterpret_cast<typename nth<k, Fields...>::type*>(this->buffer + offsetAt<k, offs>::value);
      }
    template <size_t k>
      typename nth<k, Fields...>::type& at() { return *atP<k>(); }
    template <size_t k>
      const typename nth<k, Fields...>::type* atP() const {
        return reinterpret_cast<const typename nth<k, Fields...>::type*>(this->buffer + offsetAt<k, offs>::value);
      }
    template <size_t k>
      const typename nth<k, Fields...>::type& at() const { return *atP<k>(); }
  };
template <>
  struct tuple<> {
    static const size_t alignment = 1;
    static const size_t size      = 0;

    tuple() { }
    tuple(const tuple<>&) { }
    ~tuple() { }

    tuple<>& operator=(const tuple<>&) { return *this; }
  };

template <typename ... Fields>
  inline bool operator==(const tuple<Fields...>& lhs, const tuple<Fields...>& rhs) {
    return offsetInfo<0, 0, Fields...>::eq(lhs.buffer, rhs.buffer);
  }

template <size_t i, typename T>
  struct tupType { };
template <size_t i, typename ... Ts>
  struct tupType<i, tuple<Ts...>> { typedef typename nth<i, Ts...>::type type; };

// map a function over a tuple's types (useful for structurally-derived types)
template <typename ... Ts>
  struct concatT { };
template <typename ... Ts1, typename ... Ts2, typename ... Rest>
  struct concatT<tuple<Ts1...>, tuple<Ts2...>, Rest...> {
    typedef typename concatT<tuple<Ts1..., Ts2...>, Rest...>::type type;
  }; 
template <typename ... Ts>
  struct concatT<tuple<Ts...>> {
    typedef tuple<Ts...> type;
  };

template <template <typename T> class F, typename X>
  struct fmap { };
template <template <typename T> class F, typename H, typename ... Rest>
  struct fmap<F, tuple<H, Rest...>> {
    typedef typename concatT<tuple<typename F<H>::type>, typename fmap<F, tuple<Rest...>>::type>::type type;
  }; 
template <template <typename T> class F, typename H>
  struct fmap<F, tuple<H>> {
    typedef tuple<typename F<H>::type> type;
  };
template <template <typename T> class F>
  struct fmap<F, tuple<>> {
    typedef tuple<> type;
  };

// the trivially true proposition -- ie: C's "void" with its one value constructible
struct unit {
  unit() { }
  bool operator==(const unit&) const { return true; }
  bool operator< (const unit&) const { return false; }
};
inline std::ostream& operator<<(std::ostream& o, const unit&) { o << "()"; return o; }

// drop the first type from a sequence to determine a tuple type
// (useful with these macro expansions where we can't distinguish first and rest values)
template <typename ... Ts>             struct tupleTail           {                            };
template <typename T, typename ... Ts> struct tupleTail<T, Ts...> { typedef tuple<Ts...> type; };

// reflective structs
#define PRIV_HPPF_STRUCT_FIELD(t, n) t n;
#define PRIV_HPPF_STRUCT_FIELD_VISIT(t, n) v.template visit<t>(#n);
#define PRIV_HPPF_STRUCT_FIELD_EQ(t, n) && this->n == rhs.n
#define PRIV_HPPF_STRUCT_FIELD_TYARGL(t, n) , t

#define DEFINE_STRUCT(T, FIELDS...) \
  struct T { \
    PRIV_HPPF_MAP(PRIV_HPPF_STRUCT_FIELD, FIELDS) /* struct fields */ \
    static const bool is_hmeta_struct = true; /* identify this type as a struct */ \
    typedef typename ::hobbes::tupleTail<int PRIV_HPPF_MAP(PRIV_HPPF_STRUCT_FIELD_TYARGL, FIELDS)>::type as_tuple_type; \
    template <typename V> \
      static void meta(V& v) { \
        PRIV_HPPF_MAP(PRIV_HPPF_STRUCT_FIELD_VISIT, FIELDS) \
      } \
    template <typename X> \
      inline typename std::enable_if<std::is_base_of<X, T>::value, bool>::type operator==(const X& rhs) const { \
        return true PRIV_HPPF_MAP(PRIV_HPPF_STRUCT_FIELD_EQ, FIELDS); \
      } \
  }

// reflective enumerations
#define PRIV_HPPF_ENUM_CTOR_DEF(n) n ,
#define PRIV_HPPF_ENUM_CTOR_CTOR(n) static const SelfT n() { return SelfT(SelfT::Enum::n); }
#define PRIV_HPPF_ENUM_META(n) m.push_back(std::pair<std::string,uint32_t>(#n, static_cast<uint32_t>(SelfT::Enum::n)));
#define PRIV_HPPF_ENUM_SHOW(n) case SelfT::Enum::n : return "|" #n "|";
#define PRIV_HPPF_ENUM_CTOR_COUNT(n) +1

#define DEFINE_ENUM(T, CTORS...) \
  struct T { \
    static const bool is_hmeta_enum = true; \
    static const uint32_t ctorCount = 0 PRIV_HPPF_MAP(PRIV_HPPF_ENUM_CTOR_COUNT, CTORS); \
    enum class Enum : uint32_t { \
      PRIV_HPPF_MAP(PRIV_HPPF_ENUM_CTOR_DEF, CTORS) \
      COUNT \
    }; \
    Enum value; \
    T() : value() { } \
    T(Enum v) : value(v) { } \
    T& operator=(Enum v) { this->value = v; return *this; } \
    operator Enum() const { return this->value; } \
    typedef T SelfT; \
    PRIV_HPPF_MAP(PRIV_HPPF_ENUM_CTOR_CTOR, CTORS) \
    typedef std::vector<std::pair<std::string,uint32_t>> MetaSeq; \
    static MetaSeq meta() { \
      MetaSeq m; \
      PRIV_HPPF_MAP(PRIV_HPPF_ENUM_META, CTORS); \
      return m; \
    } \
    bool operator==(const T& rhs) const { return this->value == rhs.value; } \
    std::string show() const { switch (this->value) { PRIV_HPPF_MAP(PRIV_HPPF_ENUM_SHOW, CTORS); default: return "?"; } } \
  }; \
  inline std::ostream& operator<<(std::ostream& o, const T& v) { o << v.show(); return o; }

// standard layout sum type with efficient dispatch
template <bool f, typename T, typename F>
  struct TIfF { };
template <typename T, typename F>
  struct TIfF<true, T, F> { typedef T type; };
template <typename T, typename F>
  struct TIfF<false, T, F> { typedef F type; };
template <typename T>
  struct TSizeOfF { static const size_t value = sizeof(T); };
template <typename T>
  struct TAlignOfF { static const size_t value = alignof(T); };
template <template <class> class SzP, typename T0, typename ... Ts>
  struct maximum { static const size_t value = SzP<T0>::value; typedef T0 type; };
template <template <class> class SzP, typename T0, typename T1, typename ... Ts>
  struct maximum<SzP, T0, T1, Ts...> : public maximum<SzP, typename TIfF<SzP<T1>::value < SzP<T0>::value, T0, T1>::type, Ts...> { };

template <typename T, typename ... Ctors>
  struct CtorIndexOf { static const uint32_t value = 0; };
template <typename T, typename ... Ctors>
  struct CtorIndexOf<T, T, Ctors...> { static const uint32_t value = 0; };
template <typename T, typename Ctor, typename ... Ctors>
  struct CtorIndexOf<T, Ctor, Ctors...> { static const uint32_t value = 1 + CtorIndexOf<T, Ctors...>::value; };
template <typename T, typename ... Ts>
  struct First { typedef T type; };

template <size_t i, typename R, template <size_t,class,class> class F, typename U, typename A, typename ... Ctors>
  struct variantAppInit {
  };
template <size_t i, typename R, template <size_t,class,class> class F, typename U, typename ... Args, typename ... Ctors>
  struct variantAppInit<i, R, F, U, tuple<Args...>, Ctors...> {
    typedef R (*PF)(void*, Args...);
    static bool init(PF*) { return true; }
  };
template <size_t i, typename R, template <size_t,class,class> class F, typename U, typename ... Args, typename Ctor, typename ... Ctors>
  struct variantAppInit<i, R, F, U, tuple<Args...>, Ctor, Ctors...> {
    typedef R (*PF)(void*, Args...);
    static bool init(PF* pfs) {
      typedef R (*TPF)(Ctor*, Args...);
      TPF tpf = &F<i, Ctor, U>::fn;
      pfs[i] = reinterpret_cast<PF>(tpf);
      return variantAppInit<i+1, R, F, U, tuple<Args...>, Ctors...>::init(pfs);
    }
  };

template <typename R, template <size_t,class,class> class F, typename U, typename V, typename ... Args>
  struct variantApp {
  };
template <template <size_t,class,class> class F, typename U, typename ... Ctors, typename ... Args>
  struct variantApp<void, F, U, tuple<Ctors...>, Args...> {
    typedef void (*PF)(void*, Args...);
    static PF* fns() {
      static PF fvec[sizeof...(Ctors)];
      static bool init = variantAppInit<0, void, F, U, tuple<Args...>, Ctors...>::init(fvec);
      return fvec;
      if (init) return fvec; // <-- pointless, but prevents an unused variable error
    }

    static void apply(uint32_t id, void* payload, Args... args) {
      fns()[id](payload, args...);
    }
  };

template <typename R, template <size_t,class,class> class F, typename U, typename ... Ctors, typename ... Args>
  struct variantApp<R, F, U, tuple<Ctors...>, Args...> {
    typedef R (*PF)(void*, Args...);
    static PF* fns() {
      static PF fvec[sizeof...(Ctors)];
      static bool init = variantAppInit<0, R, F, U, tuple<Args...>, Ctors...>::init(fvec);
      return fvec;
      if (init) return fvec; // <-- pointless, but prevents an unused variable error
    }

    static R apply(uint32_t id, void* payload, Args... args) {
      return fns()[id](payload, args...);
    }
  };

template <size_t, typename T, typename U>
  struct variantPayloadCtor {
    static void fn(T* p, const void* rhsp) {
      new (p) T(*reinterpret_cast<const T*>(rhsp));
    }
  };

template <size_t, typename T, typename U>
  struct variantPayloadDtor {
    static void fn(T* p) {
      p->~T();
    }
  };

template <size_t, typename T, typename U>
  struct variantPayloadEq {
    static bool fn(T* lhs, const void* rhs) {
      return *lhs == *reinterpret_cast<const T*>(rhs);
    }
  };

template <typename ... Ctors>
  class variant {
  public:
    static_assert(sizeof...(Ctors) > 0, "Empty variants are impossible to construct");

    typedef variantApp<void, variantPayloadCtor, void, tuple<Ctors...>, const void*> VCCtor;
    typedef variantApp<void, variantPayloadDtor, void, tuple<Ctors...>>              VDtor;

    variant() : tag(0) {
      new (this->storage) typename First<Ctors...>::type();
    }
    template <typename T>
      variant(const T& t) : tag(CtorIndexOf<T, Ctors...>::value) {
        static_assert(CtorIndexOf<T, Ctors...>::value < sizeof...(Ctors), "Constructor type isn't part of variant");
        new (this->storage) T(t);
      }
    variant(const variant<Ctors...>& rhs) : tag(rhs.tag) {
      VCCtor::apply(this->tag, this->storage, rhs.storage);
    }
    ~variant() {
      VDtor::apply(this->tag, this->storage);
    }
    variant<Ctors...>& operator=(const variant<Ctors...>& rhs) {
      if (this != &rhs) {
        VDtor::apply(this->tag, this->storage);
        this->tag = rhs.tag;
        VCCtor::apply(this->tag, this->storage, rhs.storage);
      }
      return *this;
    }

    template <typename T>
      T* get() { return findByCtor<T>(); }
    template <typename T>
      const T* get() const { return findByCtor<T>(); }

    template <typename R, template <size_t,class,class> class F, typename U, typename ... Args>
      R apply(Args... args) {
        return variantApp<R, F, U, tuple<Ctors...>, Args...>::apply(this->tag, this->storage, args...);
      }
    template <typename R, template <size_t,class,class> class F, typename U, typename ... Args>
      R apply(Args... args) const {
        return variantApp<R, F, U, tuple<Ctors...>, Args...>::apply(this->tag, const_cast<void*>(reinterpret_cast<const void*>(this->storage)), args...);
      }
  public:
    const uint32_t& unsafeTag() const { return this->tag; }
    uint32_t&       unsafeTag()       { return this->tag; }
    void*           unsafePayload()   { return this->storage; }
  private:
    uint32_t tag;
    union {
      char storage[maximum<TSizeOfF, Ctors...>::value];
      typename maximum<TAlignOfF, Ctors...>::type maxAlignedT;
    };

    template <typename T>
      T* findByCtor() const {
        static_assert(CtorIndexOf<T, Ctors...>::value < sizeof...(Ctors), "Constructor type isn't part of variant");

        if (this->tag == CtorIndexOf<T, Ctors...>::value) {
          return const_cast<T*>(reinterpret_cast<const T*>(this->storage));
        } else {
          return 0;
        }
      }
  };
template <typename ... Ctors>
  inline bool operator==(const variant<Ctors...>& lhs, const variant<Ctors...>& rhs) {
    return lhs.unsafeTag() == rhs.unsafeTag() &&
           variantApp<bool, variantPayloadEq, void, tuple<Ctors...>, const void*>::apply(lhs.unsafeTag(), const_cast<void*>(reinterpret_cast<const void*>(lhs.unsafePayload())), reinterpret_cast<const void*>(rhs.unsafePayload()));
  }
template <typename T, typename ... Ctors>
  T* get(variant<Ctors...>& v) {
    return v.template get<T>();
  }
template <typename T, typename ... Ctors>
  const T* get(const variant<Ctors...>& v) {
    return v.template get<T>();
  }

template <typename ... Ts>             struct variantTail           {                              };
template <typename T, typename ... Ts> struct variantTail<T, Ts...> { typedef variant<Ts...> type; };

template <typename T>
  struct toVariant { };
template <typename ... Ts>
  struct toVariant<tuple<Ts...>> { typedef variant<Ts...> type; };

// reflective variants
#define PRIV_HPPF_VARIANT_TYARGL(n, t)    , t
#define PRIV_HPPF_VARIANT_CTOR(n, t)      static SelfT n(const t & x) { SelfT r; r.tag = Enum::tag_##n; new (&r.n##_data) t(x); return r; }
#define PRIV_HPPF_VARIANT_PCOPY(n, t)     case Enum::tag_##n: new (&this->n##_data) t(rhs.n##_data); break;
#define PRIV_HPPF_VARIANT_GINIT(n, t)     case Enum::tag_##n: new (&this->n##_data) t(); v.template init<t>(&this->n##_data); break;
#define PRIV_HPPF_VARIANT_PDESTROY(n, t)  case Enum::tag_##n: { typedef t PRIV_DT; reinterpret_cast<PRIV_DT*>(&this->n##_data)->~PRIV_DT(); } break;
#define PRIV_HPPF_VARIANT_CTOR_TAG(n, t)  tag_##n,
#define PRIV_HPPF_VARIANT_CTOR_DATA(n, t) t n##_data;
#define PRIV_HPPF_VARIANT_CTOR_COUNT(n,t) +1
#define PRIV_HPPF_VARIANT_EQCASE(n, t)    case Enum::tag_##n: return (this->n##_data == rhs.n##_data);
#define PRIV_HPPF_VARIANT_META(n, t)      v.template ctor<t>(#n, static_cast<uint32_t>(Enum::tag_##n));
#define PRIV_HPPF_VARIANT_VDECL(n, t)     virtual R n(const t & x) const = 0;
#define PRIV_HPPF_VARIANT_VCASE(n, t)     case Enum::tag_##n: return v. n (this->n##_data);
#define PRIV_HPPF_VARIANT_GVCASE(n, t)    case Enum::tag_##n: return v.template visit<t>(#n, static_cast<uint32_t>(Enum::tag_##n), this->n##_data);
#define PRIV_HPPF_VARIANT_SHOW(n, t)      case Enum::tag_##n: o << "|" << #n << "=" << this->n##_data << "|"; break;

#define DEFINE_VARIANT(T, CTORS...) \
  template <typename R> \
    struct T##Visitor { \
      PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_VDECL, CTORS) \
    }; \
  struct T { \
    static const bool is_hmeta_variant = true; \
    typedef T SelfT; \
    static const uint32_t ctorCount = 0 PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_CTOR_COUNT, CTORS); \
    typedef typename ::hobbes::variantTail<int PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_TYARGL, CTORS)>::type as_variant_type; \
    T() : tag(Enum::COUNT) { } \
    PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_CTOR, CTORS) \
    T(const T& rhs) : tag(rhs.tag) { \
      switch (this->tag) { \
      PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_PCOPY, CTORS) \
      default: break; \
      } \
    } \
    ~T() { \
      switch (this->tag) { \
      PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_PDESTROY, CTORS) \
      default: break; \
      } \
    } \
    T& operator=(const T& rhs) { \
      if (this == &rhs) return *this; \
      switch (this->tag) { \
      PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_PDESTROY, CTORS) \
      default: break; \
      } \
      this->tag = rhs.tag; \
      switch (this->tag) { \
      PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_PCOPY, CTORS) \
      default: break; \
      } \
      return *this; \
    } \
    template <typename V> \
      static void meta(V& v) { \
        PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_META, CTORS) \
      } \
    template <typename R> \
      R visit(const T##Visitor<R>& v) const { \
        switch (this->tag) { \
        PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_VCASE, CTORS) \
        default: throw std::runtime_error("while deconstructing the " #T " variant, cannot decide payload type because tag is invalid"); \
        } \
      } \
    template <typename V> \
      void gvisit(V& v) const { \
        switch (this->tag) { \
        PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_GVCASE, CTORS) \
        default: break; \
        } \
      } \
    template <typename V> \
      void ginit(uint32_t t, V& v) { \
        switch (this->tag) { \
        PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_PDESTROY, CTORS) \
        default: break; \
        } \
        this->tag = static_cast<Enum>(t); \
        switch (this->tag) { \
        PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_GINIT, CTORS) \
        default: break; \
        } \
      } \
    std::ostream& show(std::ostream& o) const { \
      switch (this->tag) { \
      PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_SHOW, CTORS) \
      default: o << "?"; break; \
      } \
      return o; \
    } \
    template <typename X> \
      inline typename std::enable_if<std::is_base_of<X, T>::value, bool>::type operator==(const X& rhs) const { \
        if (this->tag != rhs.tag) { \
          return false; \
        } else { \
          switch (this->tag) { \
          PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_EQCASE, CTORS) \
          default: return false; \
          } \
        } \
      } \
  private: \
    enum class Enum : uint32_t { \
      PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_CTOR_TAG, CTORS) \
      COUNT \
    }; \
    Enum tag; \
    union { \
      char data[1]; \
      PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_CTOR_DATA, CTORS) \
    }; \
  }; \
  inline std::ostream& operator<<(std::ostream& o, const T& v) { return v.show(o); }

// reflective variants with explicit constructor names
#define PRIV_HPPF_LBLVARIANT_TYARGL(n, lbl, t)     , t
#define PRIV_HPPF_LBLVARIANT_CTOR(n, lbl, t)       static SelfT n(const t & x) { SelfT r; r.tag = Enum::tag_##n; new (&r.n##_data) t(x); return r; }
#define PRIV_HPPF_LBLVARIANT_PCOPY(n, lbl, t)      case Enum::tag_##n: new (&this->n##_data) t(rhs.n##_data); break;
#define PRIV_HPPF_LBLVARIANT_GINIT(n, lbl, t)      case Enum::tag_##n: new (&this->n##_data) t(); v.template init<t>(&this->n##_data); break;
#define PRIV_HPPF_LBLVARIANT_PDESTROY(n, lbl, t)   case Enum::tag_##n: { typedef t PRIV_DT; reinterpret_cast<PRIV_DT*>(&this->n##_data)->~PRIV_DT(); } break;
#define PRIV_HPPF_LBLVARIANT_CTOR_TAG(n, lbl, t)   tag_##n,
#define PRIV_HPPF_LBLVARIANT_CTOR_DATA(n, lbl, t)  t n##_data;
#define PRIV_HPPF_LBLVARIANT_CTOR_COUNT(n, lbl, t) +1
#define PRIV_HPPF_LBLVARIANT_EQCASE(n, lbl, t)     case Enum::tag_##n: return (this->n##_data == rhs.n##_data);
#define PRIV_HPPF_LBLVARIANT_META(n, lbl, t)       v.template ctor<t>(#lbl, static_cast<uint32_t>(Enum::tag_##n));
#define PRIV_HPPF_LBLVARIANT_VDECL(n, lbl, t)      virtual R n(const t & x) const = 0;
#define PRIV_HPPF_LBLVARIANT_VCASE(n, lbl, t)      case Enum::tag_##n: return v. n (this->n##_data);
#define PRIV_HPPF_LBLVARIANT_GVCASE(n, lbl, t)     case Enum::tag_##n: return v.template visit<t>(#lbl, static_cast<uint32_t>(Enum::tag_##n), this->n##_data);
#define PRIV_HPPF_LBLVARIANT_SHOW(n, lbl, t)       case Enum::tag_##n: o << "|" << #lbl << "=" << this->n##_data << "|"; break;

#define DEFINE_VARIANT_WITH_LABELS(T, CTORS...) \
  template <typename R> \
    struct T##Visitor { \
      PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_VDECL, CTORS) \
    }; \
  struct T { \
    static const bool is_hmeta_variant = true; \
    typedef T SelfT; \
    static const uint32_t ctorCount = 0 PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_CTOR_COUNT, CTORS); \
    typedef typename ::hobbes::variantTail<int PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_TYARGL, CTORS)>::type as_variant_type; \
    T() : tag(Enum::COUNT) { } \
    PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_CTOR, CTORS) \
    T(const T& rhs) : tag(rhs.tag) { \
      switch (this->tag) { \
      PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_PCOPY, CTORS) \
      default: break; \
      } \
    } \
    ~T() { \
      switch (this->tag) { \
      PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_PDESTROY, CTORS) \
      default: break; \
      } \
    } \
    T& operator=(const T& rhs) { \
      if (this == &rhs) return *this; \
      switch (this->tag) { \
      PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_PDESTROY, CTORS) \
      default: break; \
      } \
      this->tag = rhs.tag; \
      switch (this->tag) { \
      PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_PCOPY, CTORS) \
      default: break; \
      } \
      return *this; \
    } \
    template <typename V> \
      static void meta(V& v) { \
        PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_META, CTORS) \
      } \
    template <typename R> \
      R visit(const T##Visitor<R>& v) const { \
        switch (this->tag) { \
        PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_VCASE, CTORS) \
        default: throw std::runtime_error("while deconstructing the " #T " variant, cannot decide payload type because tag is invalid"); \
        } \
      } \
    template <typename V> \
      void gvisit(V& v) const { \
        switch (this->tag) { \
        PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_GVCASE, CTORS) \
        default: break; \
        } \
      } \
    template <typename V> \
      void ginit(uint32_t t, V& v) { \
        switch (this->tag) { \
        PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_PDESTROY, CTORS) \
        default: break; \
        } \
        this->tag = static_cast<Enum>(t); \
        switch (this->tag) { \
        PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_GINIT, CTORS) \
        default: break; \
        } \
      } \
    std::ostream& show(std::ostream& o) const { \
      switch (this->tag) { \
      PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_SHOW, CTORS) \
      default: o << "?"; break; \
      } \
      return o; \
    } \
    template <typename X> \
      inline typename std::enable_if<std::is_base_of<X, T>::value, bool>::type operator==(const X& rhs) const { \
        if (this->tag != rhs.tag) { \
          return false; \
        } else { \
          switch (this->tag) { \
          PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_EQCASE, CTORS) \
          default: return false; \
          } \
        } \
      } \
  private: \
    enum class Enum : uint32_t { \
      PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_CTOR_TAG, CTORS) \
      COUNT \
    }; \
    Enum tag; \
    union { \
      char data[1]; \
      PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_CTOR_DATA, CTORS) \
    }; \
  }; \
  inline std::ostream& operator<<(std::ostream& o, const T& v) { return v.show(o); }


// define opaque type aliases
#define DEFINE_TYPE_ALIAS_AS(PRIV_ATY, N, PRIV_REPTY) \
  struct PRIV_ATY { \
    static const bool is_hmeta_alias = true; \
    typedef void is_hstore_alias; \
    typedef PRIV_REPTY type; \
    static const char* name() { return #N; } \
    inline operator PRIV_REPTY() { return this->value; } \
    PRIV_REPTY value; \
    PRIV_ATY() : value() { } \
    PRIV_ATY(const PRIV_REPTY& x) : value(x) { } \
    constexpr PRIV_ATY(const PRIV_ATY& x) = default; \
    PRIV_ATY& operator=(const PRIV_ATY& x) = default; \
  }; \
  template <typename T> \
    inline typename std::enable_if<std::is_base_of<T, PRIV_ATY>::value, bool>::type operator==(const T& a, const T& b) { \
      return a.value == b.value; \
    }

#define DEFINE_TYPE_ALIAS(PRIV_ATY, PRIV_REPTY) DEFINE_TYPE_ALIAS_AS(PRIV_ATY, PRIV_ATY, PRIV_REPTY)

/***************************************************
 *
 * type description/encoding
 *
 ***************************************************/

namespace ty {

typedef std::vector<uint8_t> bytes;

// type encoding logic
#define PRIV_HPPF_TYCTOR_PRIM      (static_cast<uint32_t>(0))
#define PRIV_HPPF_TYCTOR_TVAR      (static_cast<uint32_t>(2))
#define PRIV_HPPF_TYCTOR_FIXEDARR  (static_cast<uint32_t>(4))
#define PRIV_HPPF_TYCTOR_ARR       (static_cast<uint32_t>(5))
#define PRIV_HPPF_TYCTOR_VARIANT   (static_cast<uint32_t>(6))
#define PRIV_HPPF_TYCTOR_STRUCT    (static_cast<uint32_t>(7))
#define PRIV_HPPF_TYCTOR_SIZE      (static_cast<uint32_t>(11))
#define PRIV_HPPF_TYCTOR_TAPP      (static_cast<uint32_t>(12))
#define PRIV_HPPF_TYCTOR_RECURSIVE (static_cast<uint32_t>(13))
#define PRIV_HPPF_TYCTOR_TABS      (static_cast<uint32_t>(15))

struct D { uint32_t tid; D(uint32_t tid) : tid(tid) { } };
typedef std::shared_ptr<D> desc;

struct Nat : public D {
  Nat(size_t x) : D(PRIV_HPPF_TYCTOR_SIZE), x(x) { }

  size_t x;
};
inline desc nat(size_t x) { return desc(new Nat(x)); }

struct Prim : public D {
  Prim(const std::string& n, const desc& rep) : D(PRIV_HPPF_TYCTOR_PRIM), n(n), rep(rep) { }

  std::string n;   // the name of this type, assumed primitive
  desc        rep; // may be null if not an opaque type alias
};
inline desc prim(const std::string& n, const desc& rep = desc()) { return desc(new Prim(n, rep)); }

struct Var : public D {
  Var(const std::string& n) : D(PRIV_HPPF_TYCTOR_TVAR), n(n) { }
  std::string n;
};
inline desc var(const std::string& n) { return desc(new Var(n)); }

struct FArr : public D {
  FArr(const desc& t, const desc& len) : D(PRIV_HPPF_TYCTOR_FIXEDARR), t(t), len(len) { }
  desc t;
  desc len;
};
inline desc array(const desc& t, const desc& len) { return desc(new FArr(t, len)); }

struct Arr : public D {
  Arr(const desc& t) : D(PRIV_HPPF_TYCTOR_ARR), t(t) { }
  desc t;
};
inline desc array(const desc& t) { return desc(new Arr(t)); }

struct Variant : public D {
  typedef tuple<std::string, uint32_t, desc> Ctor;
  typedef std::vector<Ctor> Ctors;

  Variant(const Ctors& ctors) : D(PRIV_HPPF_TYCTOR_VARIANT), ctors(ctors) { }

  Ctors ctors;
};
inline desc variant(const Variant::Ctors& ctors) { return desc(new Variant(ctors)); }
inline desc enumdef(const std::vector<std::pair<std::string, uint32_t>>& eds) {
  Variant::Ctors ctors;
  for (const auto& ed : eds) {
    ctors.push_back(Variant::Ctor(ed.first, ed.second, prim("unit")));
  }
  return variant(ctors);
}

template <typename ... Tys>
  struct sumAcc { static void acc(std::vector<desc>*, const Tys& ...) { } };
template <typename ... Tys>
  struct sumAcc<desc, Tys...> { static void acc(std::vector<desc>* ts, const desc& t, const Tys& ... tt) { ts->push_back(t); sumAcc<Tys...>::acc(ts, tt...); } };

template <typename ... Tys>
  inline desc sum(const Tys&... tt) {
    std::vector<desc> ts;
    sumAcc<Tys...>::acc(&ts, tt...);
    
    Variant::Ctors cs;
    for (size_t i = 0; i < ts.size(); ++i) {
      std::ostringstream n;
      n << ".f" << i;
      cs.push_back(Variant::Ctor(n.str(), static_cast<uint32_t>(i), ts[i]));
    }
    return variant(cs);
  }

struct Struct : public D {
  typedef tuple<std::string, int, desc> Field;
  typedef std::vector<Field> Fields;

  Struct(const Fields& fields) : D(PRIV_HPPF_TYCTOR_STRUCT), fields(fields) { }

  Fields fields;
};
inline desc record(const Struct::Fields& fields) { return fields.size() == 0 ? prim("unit") : desc(new Struct(fields)); }

template <typename ... Tys>
  struct tupAcc { static void acc(Struct::Fields*) { } };
template <typename ... Tys>
  struct tupAcc<int, desc, Tys...> { static void acc(Struct::Fields* fs, const int& o, const desc& t, const Tys& ... tt) { fs->push_back(Struct::Field("", o, t)); tupAcc<Tys...>::acc(fs, tt...); } };
template <typename ... Tys>
  struct tupAcc<size_t, desc, Tys...> { static void acc(Struct::Fields* fs, const size_t& o, const desc& t, const Tys& ... tt) { fs->push_back(Struct::Field("", o, t)); tupAcc<Tys...>::acc(fs, tt...); } };
template <typename ... Tys>
  desc tup(const Tys& ... tt) {
    Struct::Fields fs;
    tupAcc<Tys...>::acc(&fs, tt...);
    for (size_t i = 0; i < fs.size(); ++i) {
      std::ostringstream n;
      n << ".f" << i;
      fs[i].at<0>() = n.str();
    }
    return record(fs);
  }

template <typename ... Tys>
  struct recAcc { static void acc(Struct::Fields*) { } };
template <size_t N, typename ... Tys>
  struct recAcc<char[N], int, desc, Tys...> { static void acc(Struct::Fields* fs, const char (&n)[N], const int& o, const desc& t, const Tys& ... tt) { fs->push_back(Struct::Field(n, o, t)); recAcc<Tys...>::acc(fs, tt...); } };
template <size_t N, typename ... Tys>
  struct recAcc<char[N], size_t, desc, Tys...> { static void acc(Struct::Fields* fs, const char (&n)[N], const size_t& o, const desc& t, const Tys& ... tt) { fs->push_back(Struct::Field(n, o, t)); recAcc<Tys...>::acc(fs, tt...); } };
template <typename ... Tys>
  desc rec(const Tys& ... tt) {
    Struct::Fields fs;
    recAcc<Tys...>::acc(&fs, tt...);
    return record(fs);
  }

struct Recursive : public D {
  Recursive(const std::string& x, const desc& t) : D(PRIV_HPPF_TYCTOR_RECURSIVE), x(x), t(t) { }

  std::string x;
  desc        t;
};
inline desc recursive(const std::string& x, const desc& t) { return desc(new Recursive(x, t)); }

struct Fn : public D {
  typedef std::vector<std::string> Args;
  Fn(const Args& args, const desc& t) : D(PRIV_HPPF_TYCTOR_TABS), args(args), t(t) { }

  Args args;
  desc t;
};
inline desc fnc(const Fn::Args&    args, const desc& t) { return desc(new Fn(args, t)); }

template <typename ... Tys>
  struct fnAcc { static desc acc(Fn::Args*) { assert(false && "fn must define arg list and body"); return desc(); } };
template <>
  struct fnAcc<desc> { static desc acc(Fn::Args* args, const desc& t) { return fnc(*args, t); } };
template <size_t N, typename ... Tys>
  struct fnAcc<char[N], Tys...> { static desc acc(Fn::Args* args, const char (&n)[N], const Tys& ... tt) { args->push_back(n); return fnAcc<Tys...>::acc(args, tt...); } };

template <typename ... Tys>
  inline desc fn(const Tys& ... tt) { Fn::Args args; return fnAcc<Tys...>::acc(&args, tt...); }

struct App : public D {
  typedef std::vector<desc> Args;

  App(const desc& f, const Args& args) : D(PRIV_HPPF_TYCTOR_TAPP), f(f), args(args) { }

  desc f;
  Args args;
};
inline desc appc(const desc& f, const App::Args& args) { return desc(new App(f, args)); }

template <typename ... Tys>
  struct appAcc { static void acc(std::vector<desc>*) { } };
template <typename ... Tys>
  struct appAcc<desc, Tys...> { static void acc(std::vector<desc>* ds, const desc& d, const Tys& ... tt) { ds->push_back(d); appAcc<Tys...>::acc(ds, tt...); } };

template <typename ... Tys>
  inline desc app(const desc& f, const Tys& ... tt) {
    std::vector<desc> ds;
    appAcc<Tys...>::acc(&ds, tt...);
    return appc(f, ds);
  }

inline desc fileRef(const desc& t) {
  return app(prim("fileref", fn("x", prim("long"))), t);
}

// encode a type description as a byte array (for storage/transmission)
template <typename T>
  void w(const T& x, bytes* out) {
    out->insert(out->end(), reinterpret_cast<const uint8_t*>(&x), reinterpret_cast<const uint8_t*>(&x) + sizeof(x));
  }
inline void ws(const std::string& x, bytes* out) {
  w(static_cast<size_t>(x.size()), out);
  out->insert(out->end(), x.begin(), x.end());
}

inline void encode(const desc& t, bytes* o) {
  const D*         pd   = t.get();
  const Prim*      pt   = reinterpret_cast<const Prim*>(pd);
  const Var*       pvn  = reinterpret_cast<const Var*>(pd);
  const FArr*      pfa  = reinterpret_cast<const FArr*>(pd);
  const Arr*       pa   = reinterpret_cast<const Arr*>(pd);
  const Variant*   pv   = reinterpret_cast<const Variant*>(pd);
  const Struct*    ps   = reinterpret_cast<const Struct*>(pd);
  const Recursive* pr   = reinterpret_cast<const Recursive*>(pd);
  const Fn*        pfn  = reinterpret_cast<const Fn*>(pd);
  const App*       papp = reinterpret_cast<const App*>(pd);
  const Nat*       pn   = reinterpret_cast<const Nat*>(pd);

  w(t->tid, o);
  switch (t->tid) {
  case PRIV_HPPF_TYCTOR_PRIM:
    ws(pt->n, o);
    if (pt->rep) {
      w(true, o);
      encode(pt->rep, o);
    } else {
      w(false, o);
    }
    break;
  case PRIV_HPPF_TYCTOR_TVAR:
    ws(pvn->n, o);
    break;
  case PRIV_HPPF_TYCTOR_FIXEDARR:
    encode(pfa->t,   o);
    encode(pfa->len, o);
    break;
  case PRIV_HPPF_TYCTOR_ARR:
    encode(pa->t, o);
    break;
  case PRIV_HPPF_TYCTOR_VARIANT:
    w(static_cast<size_t>(pv->ctors.size()), o);
    for (const auto& ctor : pv->ctors) {
      ws(ctor.at<0>(), o);
      w(static_cast<uint32_t>(ctor.at<1>()), o);
      encode(ctor.at<2>(), o);
    }
    break;
  case PRIV_HPPF_TYCTOR_STRUCT:
    w(static_cast<size_t>(ps->fields.size()), o);
    for (const auto& field : ps->fields) {
      ws(field.at<0>(), o);
      w(static_cast<uint32_t>(field.at<1>()), o);
      encode(field.at<2>(), o);
    }
    break;
  case PRIV_HPPF_TYCTOR_SIZE:
    w(pn->x, o);
    break;
  case PRIV_HPPF_TYCTOR_TAPP:
    encode(papp->f, o);
    w(static_cast<size_t>(papp->args.size()), o);
    for (const auto& arg : papp->args) {
      encode(arg, o);
    }
    break;
  case PRIV_HPPF_TYCTOR_RECURSIVE:
    ws(pr->x, o);
    encode(pr->t, o);
    break;
  case PRIV_HPPF_TYCTOR_TABS:
    w(static_cast<size_t>(pfn->args.size()), o);
    for (const auto& arg : pfn->args) {
      ws(arg, o);
    }
    encode(pfn->t, o);
    break;
  default:
    assert(false && "Invalid type description, internal error");
    break;
  }
}

inline bytes encoding(const desc& t) {
  bytes r;
  encode(t, &r);
  return r;
}

inline bool operator==(const desc& t0, const desc& t1) {
  return encoding(t0) == encoding(t1);
}

// decode a type description from a byte array generated by 'encode'
template <typename T>
  T r(const bytes& bs, size_t* i) {
    assert((bs.size() >= (*i + sizeof(T))) && "Invalid type encoding, expected data not available");
    T result = *reinterpret_cast<const T*>(&bs[*i]);
    *i += sizeof(T);
    return result;
  }
inline std::string rs(const bytes& bs, size_t* i) {
  auto sz = r<size_t>(bs, i);
  assert((bs.size() >= (*i + sz)) && "Invalid type encoding, expected string data not available");
  const char* s = reinterpret_cast<const char*>(&bs[*i]);
  std::string result(s, s + sz);
  *i += sz;
  return result;
}

inline desc decodeFrom(const bytes& bs, size_t* i) {
  switch (r<uint32_t>(bs, i)) {
  case PRIV_HPPF_TYCTOR_PRIM: {
      std::string n = rs(bs, i);
      desc rep;
      if (r<bool>(bs, i)) {
        rep = decodeFrom(bs, i);
      }
      return prim(n, rep);
    }

  case PRIV_HPPF_TYCTOR_TVAR: {
      std::string n = rs(bs, i);
      return var(n);
    }

  case PRIV_HPPF_TYCTOR_FIXEDARR: {
      desc t = decodeFrom(bs, i);
      desc n = decodeFrom(bs, i);
      return array(t, n);
    }

  case PRIV_HPPF_TYCTOR_ARR: {
      return array(decodeFrom(bs, i));
    }

  case PRIV_HPPF_TYCTOR_VARIANT: {
      size_t n = r<size_t>(bs, i);
      Variant::Ctors cs;
      for (size_t k = 0; k < n; ++k) {
        std::string n  = rs(bs, i);
        int         id = r<int>(bs, i);
        desc        t  = decodeFrom(bs, i);

        cs.push_back(Variant::Ctor(n, id, t));
      }
      return variant(cs);
    }

  case PRIV_HPPF_TYCTOR_STRUCT: {
      size_t n = r<size_t>(bs, i);
      Struct::Fields fs;
      for (size_t k = 0; k < n; ++k) {
        std::string n = rs(bs, i);
        int         o = r<int>(bs, i);
        desc        t = decodeFrom(bs, i);

        fs.push_back(Struct::Field(n, o, t));
      }
      return record(fs);
    }

  case PRIV_HPPF_TYCTOR_SIZE: {
      return nat(r<size_t>(bs, i));
    }

  case PRIV_HPPF_TYCTOR_TAPP: {
      desc   f = decodeFrom(bs, i);
      size_t n = r<size_t>(bs, i);

      App::Args args;
      for (size_t k = 0; k < n; ++k) {
        args.push_back(decodeFrom(bs, i));
      }

      return appc(f, args);
    }

  case PRIV_HPPF_TYCTOR_RECURSIVE: {
      std::string n = rs(bs, i);
      desc        t = decodeFrom(bs, i);

      return recursive(n, t);
    }

  case PRIV_HPPF_TYCTOR_TABS: {
      size_t   n = r<size_t>(bs, i);
      Fn::Args args;
      for (size_t k = 0; k < n; ++k) {
        args.push_back(rs(bs, i));
      }
      desc t = decodeFrom(bs, i);

      return fnc(args, t);
    }

  default:
    assert(false && "Invalid type description, internal error");
    throw std::runtime_error("Invalid type description");
  }
}

inline desc decode(const bytes& bs) {
  size_t i = 0;
  return decodeFrom(bs, &i);
}

// describe a type description for human consumption
inline void describe(const desc& t, std::ostream& o) {
  const D*         pd   = t.get();
  const Prim*      pt   = reinterpret_cast<const Prim*>(pd);
  const Var*       pvn  = reinterpret_cast<const Var*>(pd);
  const FArr*      pfa  = reinterpret_cast<const FArr*>(pd);
  const Arr*       pa   = reinterpret_cast<const Arr*>(pd);
  const Variant*   pv   = reinterpret_cast<const Variant*>(pd);
  const Struct*    ps   = reinterpret_cast<const Struct*>(pd);
  const Recursive* pr   = reinterpret_cast<const Recursive*>(pd);
  const Fn*        pfn  = reinterpret_cast<const Fn*>(pd);
  const App*       papp = reinterpret_cast<const App*>(pd);
  const Nat*       pn   = reinterpret_cast<const Nat*>(pd);

  switch (t->tid) {
  case PRIV_HPPF_TYCTOR_PRIM:
    if (pt->n == "unit") {
      o << "()";
    } else {
      o << pt->n;
    }
    break;
  case PRIV_HPPF_TYCTOR_TVAR:
    o << pvn->n;
    break;
  case PRIV_HPPF_TYCTOR_FIXEDARR:
    o << "[:";
    describe(pfa->t, o);
    o << "|";
    describe(pfa->len, o);
    o << ":]";
    break;
  case PRIV_HPPF_TYCTOR_ARR:
    o << "[";
    describe(pa->t, o);
    o << "]";
    break;
  case PRIV_HPPF_TYCTOR_VARIANT:
    if (pv->ctors.size() == 0) {
      o << "void";
    } else {
      // show as sum or variant
      if (pv->ctors[0].at<0>().substr(0,1) == ".") {
        o << "(";
        describe(pv->ctors[0].at<2>(), o);
        for (size_t i = 1; i < pv->ctors.size(); ++i) {
          o << "+";
          describe(pv->ctors[i].at<2>(), o);
        }
        o << ")";
      } else {
        o << "|" << pv->ctors[0].at<0>() << ":";
        describe(pv->ctors[0].at<2>(), o);
        for (size_t i = 1; i < pv->ctors.size(); ++i) {
          o << ", " << pv->ctors[i].at<0>() << ":";
          describe(pv->ctors[i].at<2>(), o);
        }
        o << "|";
      }
    }
    break;
  case PRIV_HPPF_TYCTOR_STRUCT:
    if (ps->fields.size() == 0) {
      o << "()";
    } else {
      // show as sum or variant
      if (ps->fields[0].at<0>().substr(0,1) == ".") {
        o << "(";
        describe(ps->fields[0].at<2>(), o);
        for (size_t i = 1; i < ps->fields.size(); ++i) {
          o << "*";
          describe(ps->fields[i].at<2>(), o);
        }
        o << ")";
      } else {
        o << "{" << ps->fields[0].at<0>() << ":";
        describe(ps->fields[0].at<2>(), o);
        for (size_t i = 1; i < ps->fields.size(); ++i) {
          o << ", " << ps->fields[i].at<0>() << ":";
          describe(ps->fields[i].at<2>(), o);
        }
        o << "}";
      }
    }
    break;
  case PRIV_HPPF_TYCTOR_SIZE:
    o << pn->x;
    break;
  case PRIV_HPPF_TYCTOR_TAPP:
    if (papp->args.size() > 0 && papp->f->tid == PRIV_HPPF_TYCTOR_PRIM && reinterpret_cast<const ty::Prim*>(papp->f.get())->n == "fileref") {
      describe(papp->args[0], o);
      o << "@?";
    } else {
      describe(papp->f, o);
      o << "(";
      if (papp->args.size() > 0) {
        describe(papp->args[0], o);
        for (size_t i = 1; i < papp->args.size(); ++i) {
          o << ", ";
          describe(papp->args[i], o);
        }
      }
      o << ")";
    }
    break;
  case PRIV_HPPF_TYCTOR_RECURSIVE:
    o << "^" << pr->x << ".";
    describe(pr->t, o);
    break;
  case PRIV_HPPF_TYCTOR_TABS:
    o << "\\";
    if (pfn->args.size() > 0) {
      o << pfn->args[0];
      for (size_t i = 1; i < pfn->args.size(); ++i) {
        o << " " << pfn->args[i];
      }
      o << ".";
      describe(pfn->t, o);
    }
    break;
  default:
    assert(false && "Invalid type description, internal error");
    break;
  }
}

inline std::string show(const desc& t) {
  std::ostringstream ss;
  describe(t, ss);
  return ss.str();
}

}

}

#endif

