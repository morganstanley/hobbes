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
#include <map>
#include <iostream>
#include <sstream>
#include <string.h>
#include <assert.h>
#include <stdexcept>
#include <type_traits>
#include <cxxabi.h>

namespace hobbes {

// basic string utilities used here
namespace string {
  template <typename T>
    inline std::string from(const T& x) {
      std::ostringstream ss;
      ss << x;
      return ss.str();
    }

  // pretty-print C++ type names
  template <typename T>
    inline std::string demangle() {
      if (const char* tn = typeid(T).name()) {
        int s = 0;
        if (char* dmn = abi::__cxa_demangle(tn, 0, 0, &s)) {
          std::string r(dmn);
          free(dmn);
          return r;
        } else {
          return std::string(tn);
        }
      } else {
        return "";
      }
    }

  // tokenize strings
  typedef std::vector<std::string> seq;
  inline seq csplit(const std::string& str, const std::string& pivot) {
    typedef std::string::size_type size_type;
  
    seq       ret;
    size_type mp = 0;
  
    while (mp != std::string::npos) {
      size_type nmp = str.find(pivot, mp);
  
      if (nmp == std::string::npos) {
        ret.push_back(str.substr(mp, str.size() - mp));
        mp = nmp;
      } else {
        ret.push_back(str.substr(mp, nmp - mp));
        mp = nmp + pivot.size();
      }
    }
  
    return ret;
  }

  typedef std::pair<std::string, std::string> pair;
  inline pair rsplit(const std::string& s, const std::string& ss) {
    size_t p = s.rfind(ss);
    if (p == std::string::npos) {
      return pair("", s);
    } else {
      return pair(s.substr(0, p), s.substr(p + ss.size(), s.size()));
    }
  }

  inline std::string cdelim(const seq& ss, const std::string& d) {
    if (ss.size() > 0) {
      std::ostringstream x;
      x << ss[0];
      for (size_t i = 1; i < ss.size(); ++i) {
        x << d << ss[i];
      }
      return x.str();
    } else {
      return "";
    }
  }
}

typedef __int128 int128_t;

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
#define PRIV_HPPF_MAPL(f, VS...) PRIV_HPPF_EVAL(PRIV_HPPF_MAPLP(f, VS))
#define PRIV_HPPF_MAPLP(f, H, T...)              \
  f(H)                                             \
  PRIV_HPPF_IF_ELSE(PRIV_HPPF_HAS_PARGS(T))(   \
    PRIV_HPPF_DEFER2(PRIV_HPPF_SMAPLP)()(f, T) \
  )(                                               \
  )
#define PRIV_HPPF_SMAPLP() PRIV_HPPF_MAPLP

// basic compile-time strings
template <size_t N>
  constexpr char at(size_t i, const char (&s)[N]) {
    return (i < N) ? s[i] : '\0';
  }
template <size_t N>
  constexpr size_t at8S(size_t i, size_t k, const char (&s)[N]) {
    return (k==8) ? 0 : ((static_cast<size_t>(at(i+k,s))<<(8*k))|at8S(i,k+1,s));
  }
template <size_t N>
  constexpr size_t at8(size_t i, const char (&s)[N]) {
    return at8S(i, 0, s);
  }
template <size_t... pcs>
  struct strpack {
    static const char* str() {
      constexpr size_t msg[] = {pcs...};
      static_assert(msg[(sizeof(msg)/sizeof(msg[0]))-1] == 0, "compile-time string larger than internal max limit (this limit can be bumped in storage.H)");

      static const size_t smsg[] = {pcs...};
      return reinterpret_cast<const char*>(smsg);
    }
  };
#define PRIV_HPPF_TSTR32(i,s)   ::hobbes::at8(i+(8*0),s),::hobbes::at8(i+(8*1),s),::hobbes::at8(i+(8*2),s),::hobbes::at8(i+(8*3),s)
#define PRIV_HPPF_TSTR128(i,s)  PRIV_HPPF_TSTR32(i+(32*0),s),PRIV_HPPF_TSTR32(i+(32*1),s),PRIV_HPPF_TSTR32(i+(32*2),s),PRIV_HPPF_TSTR32(i+(32*3),s)
#define PRIV_HPPF_TSTR512(i,s)  PRIV_HPPF_TSTR128(i+(128*0),s),PRIV_HPPF_TSTR128(i+(128*1),s),PRIV_HPPF_TSTR128(i+(128*2),s),PRIV_HPPF_TSTR128(i+(128*3),s)
#define PRIV_HPPF_TSTR1024(i,s) PRIV_HPPF_TSTR512(i+(512*0),s),PRIV_HPPF_TSTR512(i+(512*1),s)
#define PRIV_HPPF_TSTR(s)       ::hobbes::strpack<PRIV_HPPF_TSTR1024(0,s)>

// value-level bool -> type-level bool
template <bool f> struct tbool { };
template <>       struct tbool<true> { typedef void type; };

// test to see if a type(s) can be printed
template <typename T, class = decltype(std::declval<std::ostream&>() << std::declval<T>())>
  std::true_type has_shl_ostream_defined(const T&);
std::false_type has_shl_ostream_defined(...);

template <typename T>
  struct can_show_type {
    static const bool value = decltype(has_shl_ostream_defined(std::declval<T>()))::value;
  };

template <typename ... Ts>
  struct can_show_types { static const bool value = true; };
template <typename T, typename ... Ts>
  struct can_show_types<T, Ts...> { static const bool value = can_show_type<T>::value && can_show_types<Ts...>::value; };

// a standard layout tuple type
constexpr size_t alignTo(size_t x, size_t a) {
  return (x % a == 0) ? x : (a * (1 + x/a));
}
constexpr size_t szmax(size_t x, size_t y) {
  return (x > y) ? x : y;
}

template <size_t index, size_t base, typename ... Fields>
  struct offsetInfo {
    static const size_t offset       = base;
    static const size_t maxAlignment = 1;
    static const size_t size         = 0;
    static const bool   packed       = true; // but technically unit can't be memcopied

    static void defInit (uint8_t*) { }
    static void initFrom(uint8_t*, const Fields&...) { }
    static void copyFrom(uint8_t*, const uint8_t*) { }
    static void destroy (uint8_t*) { }
    static bool eq      (const uint8_t*, const uint8_t*) { return true; } // all units are equal
  };
template <size_t index, size_t base, typename Field, typename ... Fields>
  struct offsetInfo<index, base, Field, Fields...> {
    static const size_t offset = alignTo(base, alignof(Field));

    typedef offsetInfo<index+1, offset+sizeof(Field), Fields...> tail;

    static const size_t maxAlignment = szmax(alignof(Field), tail::maxAlignment);
    static const size_t size         = (offset-base) + sizeof(Field) + tail::size;
    static const bool   packed       = (offset == base) && tail::packed;

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
    static const size_t alignment = offs::maxAlignment;
    static const size_t size      = alignTo(offs::size, offs::maxAlignment);
    static const bool   packed    = (size == offs::size) && offs::packed;
    static const size_t count     = sizeof...(Fields);
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

// show a tuple iff its fields can be shown
template <size_t i, size_t n, typename ... Ts>
  struct ShowTuple {
    static void show(std::ostream& o, const tuple<Ts...>& v) {
      if (i > 0) o << ", ";
      o << v.template at<i>();
      ShowTuple<i+1, n, Ts...>::show(o, v);
    }
  };
template <size_t n, typename ... Ts>
  struct ShowTuple<n, n, Ts...> {
    static void show(std::ostream&, const tuple<Ts...>&) { }
  };

template <typename ... Ts>
  inline typename std::enable_if<can_show_types<Ts...>::value, std::ostream&>::type operator<<(std::ostream& o, const tuple<Ts...>& v) {
    o << "(";
    ShowTuple<0, tuple<Ts...>::count, Ts...>::show(o, v);
    o << ")";
    return o;
  }

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
#define PRIV_HPPF_STRUCT_FIELD_NLOOKUP(t, n) if (o == idx) { return #n; } ++o;
#define PRIV_HPPF_STRUCT_FIELD_SHOW(_, n) ss << ", " << #n << "=" << v.n;

#define DEFINE_STRUCT(T, FIELDS...) \
  struct T { \
    PRIV_HPPF_MAP(PRIV_HPPF_STRUCT_FIELD, FIELDS) /* struct fields */ \
    static const bool is_hmeta_struct = true; /* identify this type as a struct */ \
    typedef typename ::hobbes::tupleTail<int PRIV_HPPF_MAP(PRIV_HPPF_STRUCT_FIELD_TYARGL, FIELDS)>::type as_tuple_type; \
    static std::string _hmeta_struct_type_name() { return #T; } \
    template <typename V> \
      static void meta(V& v) { \
        PRIV_HPPF_MAP(PRIV_HPPF_STRUCT_FIELD_VISIT, FIELDS) \
      } \
    template <size_t idx> \
      static const char* _hmeta_field_name() { \
        size_t o = 0; \
        PRIV_HPPF_MAP(PRIV_HPPF_STRUCT_FIELD_NLOOKUP, FIELDS) \
        return "???"; \
      } \
    template <typename X> \
      inline typename std::enable_if<std::is_base_of<X, T>::value, bool>::type operator==(const X& rhs) const { \
        return true PRIV_HPPF_MAP(PRIV_HPPF_STRUCT_FIELD_EQ, FIELDS); \
      } \
    template <typename X> \
      friend inline typename std::enable_if<std::is_base_of<X,T>::value && ::hobbes::can_show_type<typename T::as_tuple_type>::value,std::ostream&>::type operator<<(std::ostream& o, const X& v) { \
        std::ostringstream ss; \
        PRIV_HPPF_MAP(PRIV_HPPF_STRUCT_FIELD_SHOW, FIELDS); \
        o << "{ " << ss.str().substr(2) << " }"; \
        return o; \
      } \
  }

// reflective enumerations
#define PRIV_HPPF_ENUM_CTOR_DEF(n) n ,
#define PRIV_HPPF_ENUM_CTOR_CTOR(n) static const SelfT n() { return SelfT(SelfT::Enum::n); }
#define PRIV_HPPF_ENUM_META(n) m.push_back(std::pair<std::string,uint32_t>(#n, static_cast<uint32_t>(SelfT::Enum::n)));
#define PRIV_HPPF_ENUM_SHOW(n) case SelfT::Enum::n : return "|" #n "|";
#define PRIV_HPPF_ENUM_CTOR_COUNT(n) +1

#define DEFINE_PACKED_ENUM(T, R, CTORS...) \
  struct T { \
    static const bool is_hmeta_enum = true; \
    static const uint32_t ctorCount = 0 PRIV_HPPF_MAP(PRIV_HPPF_ENUM_CTOR_COUNT, CTORS); \
    typedef R rep_t; \
    enum class Enum : R { \
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
    typedef std::vector<std::pair<std::string,R>> MetaSeq; \
    static MetaSeq meta() { \
      MetaSeq m; \
      PRIV_HPPF_MAP(PRIV_HPPF_ENUM_META, CTORS); \
      return m; \
    } \
    bool operator==(const T& rhs) const { return this->value == rhs.value; } \
    std::string show() const { switch (this->value) { PRIV_HPPF_MAP(PRIV_HPPF_ENUM_SHOW, CTORS); default: return "?"; } } \
    friend inline std::ostream& operator<<(std::ostream& o, const T& v) { o << v.show(); return o; } \
  }

#define DEFINE_ENUM(T, CTORS...) DEFINE_PACKED_ENUM(T, uint32_t, CTORS)

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
    static const size_t count = sizeof...(Ctors);
    static_assert(count > 0, "Empty variants are impossible to construct");

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
    const uint32_t& unsafeTag() const     { return this->tag; }
    uint32_t&       unsafeTag()           { return this->tag; }
    const void*     unsafePayload() const { return this->storage; }
    void*           unsafePayload()       { return this->storage; }
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

// show a variant iff its constructors can be shown
template <size_t, typename T, typename U>
  struct ShowVariantPayload {
    static void fn(T* x, std::ostream* out) {
      *out << *x;
    }
  };

template <typename ... Ts>
  inline typename std::enable_if<0<sizeof...(Ts) && can_show_types<Ts...>::value, std::ostream&>::type operator<<(std::ostream& o, const variant<Ts...>& cv) {
    variant<Ts...>& v = *const_cast<variant<Ts...>*>(&cv);
    o << "|" << v.unsafeTag() << "=";
    variantApp<void, ShowVariantPayload, void, tuple<Ts...>, std::ostream*>::apply(v.unsafeTag(), const_cast<void*>(reinterpret_cast<const void*>(v.unsafePayload())), &o);
    o << "|";
    return o;
  }

// reflective variants
#define PRIV_HPPF_VARIANT_TYARGL(_, t)      , t
#define PRIV_HPPF_VARIANT_CTOR(n, t)        static SelfT n(const t & x) { SelfT r; r.tag = Enum::tag_##n; new (&r.n##_data) t(x); return r; }
#define PRIV_HPPF_VARIANT_PCOPY(n, t)       case Enum::tag_##n: new (&this->n##_data) t(rhs.n##_data); break;
#define PRIV_HPPF_VARIANT_GINIT(n, t)       case Enum::tag_##n: new (&this->n##_data) t(); v.template init<t>(&this->n##_data); break;
#define PRIV_HPPF_VARIANT_PDESTROY(n, t)    case Enum::tag_##n: { typedef t PRIV_DT; reinterpret_cast<PRIV_DT*>(&this->n##_data)->~PRIV_DT(); } break;
#define PRIV_HPPF_VARIANT_CTOR_TAG(n, t)    tag_##n,
#define PRIV_HPPF_VARIANT_CTOR_DATA(n, t)   t n##_data;
#define PRIV_HPPF_VARIANT_CTOR_COUNT(_,__)   +1
#define PRIV_HPPF_VARIANT_EQCASE(n, _)      case Enum::tag_##n: return (this->n##_data == rhs.n##_data);
#define PRIV_HPPF_VARIANT_META(n, t)        v.template ctor<t>(#n, static_cast<uint32_t>(Enum::tag_##n));
#define PRIV_HPPF_VARIANT_VDECL(n, t)       virtual R n(const t & x) const = 0;
#define PRIV_HPPF_VARIANT_VCASE(n, _)       case Enum::tag_##n: return v. n (this->n##_data);
#define PRIV_HPPF_VARIANT_GVCASE(n, t)      case Enum::tag_##n: return v.template visit<t>(#n, static_cast<uint32_t>(Enum::tag_##n), this->n##_data);
#define PRIV_HPPF_VARIANT_SHOW(n, t)        case X::Enum::tag_##n: o << "|" << #n << "=" << v.n##_data << "|"; break;
#define PRIV_HPPF_VARIANT_HMCTOR_NAME(n, _) case Enum::tag_##n: return #n;

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
    template <size_t i> \
      static const char* _hmeta_ctor_name() { \
        static_assert(i < as_variant_type::count, "Invalid constructor index in variant"); \
        switch (static_cast<Enum>(i)) { \
        PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_HMCTOR_NAME, CTORS) \
        default: return "impossible"; \
        } \
      } \
    template <size_t i> \
      static uint32_t _hmeta_ctor_id() { \
        static_assert(i < as_variant_type::count, "Invalid constructor index in variant"); \
        return static_cast<uint32_t>(i); \
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
  public: \
    template <typename X> \
      friend inline typename std::enable_if<std::is_base_of<X,T>::value && ::hobbes::can_show_type<T::as_variant_type>::value,std::ostream&>::type operator<<(std::ostream& o, const X& v) { \
        switch (v.tag) { \
        PRIV_HPPF_MAP(PRIV_HPPF_VARIANT_SHOW, CTORS) \
        default: o << "?"; break; \
        } \
        return o; \
      } \
  }

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
#define PRIV_HPPF_LBLVARIANT_SHOW(n, lbl, t)       case Enum::tag_##n: o << "|" << #lbl << "=" << v.n##_data << "|"; break;

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
    template <typename X> \
      inline typename std::enable_if<std::is_base_of<X, T>::value, bool>::type operator==(const X& rhs) const { \
        if (this->tag != rhs.tag) { \
          return false; \
        } else { \
          switch (this->tag) { \
          PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_EQCASE, CTORS) \
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
  public: \
    template <typename X> \
      friend inline typename std::enable_if<std::is_base_of<X,T>::value && ::hobbes::can_show_type<T::as_variant_type>::value,std::ostream&>::type operator<<(std::ostream& o, const X& v) { \
        switch (v.tag) { \
        PRIV_HPPF_MAP(PRIV_HPPF_LBLVARIANT_SHOW, CTORS) \
        default: o << "?"; break; \
        } \
        return o; \
      } \
  }

// arrays with dynamically-determined length
template <typename T>
  struct array {
    typedef array<T>* ptr;

    size_t size;
    T      data[1]; // unknown length, bytes inline with this structure

    const T& operator[](size_t i) const { return this->data[i]; }
          T& operator[](size_t i)       { return this->data[i]; }
  };
template <typename T> const T* begin(const array<T>* d) { return d->data; }
template <typename T> const T* end  (const array<T>* d) { return d->data + d->size; }
template <typename T>       T* begin(      array<T>* d) { return d->data; }
template <typename T>       T* end  (      array<T>* d) { return d->data + d->size; }

template <typename T> const T* begin(const array<T>& d) { return d.data; }
template <typename T> const T* end  (const array<T>& d) { return d.data + d.size; }
template <typename T>       T* begin(      array<T>& d) { return d.data; }
template <typename T>       T* end  (      array<T>& d) { return d.data + d.size; }

// arrays with dynamically-determined length but with a fixed-capacity
template <typename T, size_t N>
  struct carray {
    size_t size;
    T      data[N];

    const T& operator[](size_t i) const { return this->data[i]; }
          T& operator[](size_t i)       { return this->data[i]; }
  };
template <size_t N>
  struct carray<char,N> {
    size_t size;
    char   data[N];

    const char& operator[](size_t i) const { return this->data[i]; }
          char& operator[](size_t i)       { return this->data[i]; }

    carray<char,N>& operator=(const char* s) {
      size_t n = std::min<size_t>(N, strlen(s));
      memcpy(this->data, s, n);
      this->size = n;
      return *this;
    }
  };

template <typename T, size_t N> const T* begin(const carray<T,N>* d) { return d->data; }
template <typename T, size_t N> const T* end  (const carray<T,N>* d) { return d->data + d->size; }
template <typename T, size_t N>       T* begin(      carray<T,N>* d) { return d->data; }
template <typename T, size_t N>       T* end  (      carray<T,N>* d) { return d->data + d->size; }

template <typename T, size_t N> const T* begin(const carray<T,N>& d) { return d.data; }
template <typename T, size_t N> const T* end  (const carray<T,N>& d) { return d.data + d.size; }
template <typename T, size_t N>       T* begin(      carray<T,N>& d) { return d.data; }
template <typename T, size_t N>       T* end  (      carray<T,N>& d) { return d.data + d.size; }

// define opaque type aliases (inherit from 'alias', add a static 'name()' function)
template <typename RepTy>
  struct alias {
    static const bool is_hmeta_alias = true;
    typedef RepTy type;
    RepTy value;

    alias() : value() { }
    alias(const RepTy& x) : value(x) { }
    constexpr alias(const alias<RepTy>&) = default;
    alias& operator=(const alias<RepTy>&) = default;
  };

#define DEFINE_TYPE_ALIAS_AS(PRIV_ATY, N, PRIV_REPTY) \
  struct PRIV_ATY : public ::hobbes::alias<PRIV_REPTY> { \
    using ::hobbes::alias<PRIV_REPTY>::alias; \
    static const char* name() { return #N; } \
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

template <typename R>
inline desc enumdef(const desc& rep, const std::vector<std::pair<std::string, R>>& eds) {
  Variant::Ctors ctors;
  for (const auto& ed : eds) {
    ctors.push_back(Variant::Ctor(ed.first, ed.second, prim("unit")));
  }
  if (sizeof(R) == sizeof(uint32_t)) {
    return variant(ctors);
  } else {
    return app(prim("penum", fn("r", "v", var("r"))), rep, variant(ctors));
  }
}

inline desc fileRef(const desc& t) {
  return app(prim("fileref", fn("x", prim("long"))), t);
}

// remove file ref types (recursively)
// (this reverses the storage view of all types)
inline desc elimFileRefs(const desc& t) {
  const D*         pd   = t.get();
  const FArr*      pfa  = reinterpret_cast<const FArr*>(pd);
  const Arr*       pa   = reinterpret_cast<const Arr*>(pd);
  const Variant*   pv   = reinterpret_cast<const Variant*>(pd);
  const Struct*    ps   = reinterpret_cast<const Struct*>(pd);
  const Recursive* pr   = reinterpret_cast<const Recursive*>(pd);
  const Fn*        pfn  = reinterpret_cast<const Fn*>(pd);
  const App*       papp = reinterpret_cast<const App*>(pd);

  switch (t->tid) {
  default:
    return t;
  case PRIV_HPPF_TYCTOR_FIXEDARR:
    return array(elimFileRefs(pfa->t), elimFileRefs(pfa->len));
  case PRIV_HPPF_TYCTOR_ARR:
    return array(elimFileRefs(pa->t));
  case PRIV_HPPF_TYCTOR_VARIANT:
    {
      Variant::Ctors ctors;
      for (const auto& ctor : pv->ctors) {
        ctors.push_back(Variant::Ctor(ctor.at<0>(), ctor.at<1>(), elimFileRefs(ctor.at<2>())));
      }
      return variant(ctors);
    }
  case PRIV_HPPF_TYCTOR_STRUCT:
    {
      Struct::Fields fs;
      for (const auto& field : ps->fields) {
        fs.push_back(Struct::Field(field.at<0>(), -1, elimFileRefs(field.at<2>())));
      }
      return record(fs);
    }
  case PRIV_HPPF_TYCTOR_TAPP:
    {
      if (papp->f->tid == PRIV_HPPF_TYCTOR_PRIM) {
        if (reinterpret_cast<const Prim*>(papp->f.get())->n == "fileref") {
          if (papp->args.size() > 0) {
            return elimFileRefs(papp->args[0]);
          }
        }
      }

      // default translation
      App::Args args;
      for (const auto& arg : papp->args) {
        args.push_back(elimFileRefs(arg));
      }
      return appc(elimFileRefs(papp->f), args);
    }
  case PRIV_HPPF_TYCTOR_RECURSIVE:
    return recursive(pr->x, elimFileRefs(pr->t));
  case PRIV_HPPF_TYCTOR_TABS:
    return fnc(pfn->args, elimFileRefs(pfn->t));
  }
}

// encode a type description as a byte array (for storage/transmission)
template <typename T>
  void w(const T& x, bytes* out) {
    out->insert(out->end(), reinterpret_cast<const uint8_t*>(&x), reinterpret_cast<const uint8_t*>(&x) + sizeof(x));
  }
inline void ws(const char* x, bytes* out) {
  size_t n = strlen(x);
  w(n, out);
  out->insert(out->end(), x, x + n);
}
inline void ws(const std::string& x, bytes* out) {
  w(static_cast<size_t>(x.size()), out);
  out->insert(out->end(), x.begin(), x.end());
}
inline void ws(const bytes& x, bytes* out) {
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
inline bool operator!=(const desc& t0, const desc& t1) {
  return !(t0 == t1);
}

// decode a type description from a byte array generated by 'encode'
template <typename T>
  T readValue(const bytes& bs, size_t* i) {
    assert((bs.size() >= (*i + sizeof(T))) && "Invalid type encoding, expected data not available");
    T result = *reinterpret_cast<const T*>(&bs[*i]);
    *i += sizeof(T);
    return result;
  }
inline std::string readString(const bytes& bs, size_t* i) {
  auto sz = readValue<size_t>(bs, i);
  assert((bs.size() >= (*i + sz)) && "Invalid type encoding, expected string data not available");
  const char* s = reinterpret_cast<const char*>(&bs[*i]);
  std::string result(s, s + sz);
  *i += sz;
  return result;
}

inline desc decodeFrom(const bytes& bs, size_t* i) {
  switch (readValue<uint32_t>(bs, i)) {
  case PRIV_HPPF_TYCTOR_PRIM: {
      std::string n = readString(bs, i);
      desc rep;
      if (readValue<bool>(bs, i)) {
        rep = decodeFrom(bs, i);
      }
      return prim(n, rep);
    }

  case PRIV_HPPF_TYCTOR_TVAR: {
      std::string n = readString(bs, i);
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
      size_t n = readValue<size_t>(bs, i);
      Variant::Ctors cs;
      for (size_t k = 0; k < n; ++k) {
        std::string n  = readString(bs, i);
        int         id = readValue<int>(bs, i);
        desc        t  = decodeFrom(bs, i);

        cs.push_back(Variant::Ctor(n, id, t));
      }
      return variant(cs);
    }

  case PRIV_HPPF_TYCTOR_STRUCT: {
      size_t count = readValue<size_t>(bs, i);
      Struct::Fields fs;
      for (size_t k = 0; k < count; ++k) {
        std::string n = readString(bs, i);
        int         o = readValue<int>(bs, i);
        desc        t = decodeFrom(bs, i);

        fs.push_back(Struct::Field(n, o, t));
      }
      return record(fs);
    }

  case PRIV_HPPF_TYCTOR_SIZE: {
      return nat(readValue<size_t>(bs, i));
    }

  case PRIV_HPPF_TYCTOR_TAPP: {
      desc   f = decodeFrom(bs, i);
      size_t n = readValue<size_t>(bs, i);

      App::Args args;
      for (size_t k = 0; k < n; ++k) {
        args.push_back(decodeFrom(bs, i));
      }

      return appc(f, args);
    }

  case PRIV_HPPF_TYCTOR_RECURSIVE: {
      std::string n = readString(bs, i);
      desc        t = decodeFrom(bs, i);

      return recursive(n, t);
    }

  case PRIV_HPPF_TYCTOR_TABS: {
      size_t   n = readValue<size_t>(bs, i);
      Fn::Args args;
      for (size_t k = 0; k < n; ++k) {
        args.push_back(readString(bs, i));
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
      // show as product or record
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

// type description equivalence modulo offset in record types (allows undefined offsets)
inline bool equivModOffset(const desc& t0, const desc& t1) {
  if (t0->tid != t1->tid) {
    return false;
  }

  const D* pd0 = t0.get();
  const D* pd1 = t1.get();

  switch (t0->tid) {
  case PRIV_HPPF_TYCTOR_PRIM:
    return reinterpret_cast<const Prim*>(pd0)->n == reinterpret_cast<const Prim*>(pd1)->n;
  case PRIV_HPPF_TYCTOR_TVAR:
    return reinterpret_cast<const Var*>(pd0)->n == reinterpret_cast<const Var*>(pd1)->n;
  case PRIV_HPPF_TYCTOR_FIXEDARR:
    return equivModOffset(reinterpret_cast<const FArr*>(pd0)->t, reinterpret_cast<const FArr*>(pd1)->t) &&
           equivModOffset(reinterpret_cast<const FArr*>(pd0)->len, reinterpret_cast<const FArr*>(pd1)->len);
  case PRIV_HPPF_TYCTOR_ARR:
    return equivModOffset(reinterpret_cast<const Arr*>(pd0)->t, reinterpret_cast<const Arr*>(pd1)->t);
  case PRIV_HPPF_TYCTOR_VARIANT: {
      const Variant* pv0 = reinterpret_cast<const Variant*>(pd0);
      const Variant* pv1 = reinterpret_cast<const Variant*>(pd1);

      if (pv0->ctors.size() != pv1->ctors.size()) {
        return false;
      } else {
        for (size_t i = 0; i < pv0->ctors.size(); ++i) {
          if (pv0->ctors[i].at<0>() != pv1->ctors[i].at<0>() ||
              pv0->ctors[i].at<1>() != pv1->ctors[i].at<1>() ||
              !equivModOffset(pv0->ctors[i].at<2>(), pv1->ctors[i].at<2>())) {
            return false;
          }
        }
        return true;
      }
    }
  case PRIV_HPPF_TYCTOR_STRUCT: {
      const Struct* pr0 = reinterpret_cast<const Struct*>(pd0);
      const Struct* pr1 = reinterpret_cast<const Struct*>(pd1);

      if (pr0->fields.size() != pr1->fields.size()) {
        return false;
      } else {
        for (size_t i = 0; i < pr0->fields.size(); ++i) {
          if (pr0->fields[i].at<0>() != pr1->fields[i].at<0>() ||
              !equivModOffset(pr0->fields[i].at<2>(), pr1->fields[i].at<2>())) {
            return false;
          }
        }
        return true;
      }
    }
  case PRIV_HPPF_TYCTOR_SIZE:
    return reinterpret_cast<const Nat*>(pd0)->x == reinterpret_cast<const Nat*>(pd1)->x;
  case PRIV_HPPF_TYCTOR_TAPP: {
      const App* papp0 = reinterpret_cast<const App*>(pd0);
      const App* papp1 = reinterpret_cast<const App*>(pd1);

      if (!equivModOffset(papp0->f, papp1->f)) {
        return false;
      } else if (papp0->args.size() != papp1->args.size()) {
        return false;
      } else {
        for (size_t i = 0; i < papp0->args.size(); ++i) {
          if (!equivModOffset(papp0->args[i], papp1->args[i])) {
            return false;
          }
        }
        return true;
      }
    }
  case PRIV_HPPF_TYCTOR_RECURSIVE:
    return equivModOffset(reinterpret_cast<const Recursive*>(pd0)->t, reinterpret_cast<const Recursive*>(pd1)->t);
  case PRIV_HPPF_TYCTOR_TABS: {
      const Fn* fn0 = reinterpret_cast<const Fn*>(pd0);
      const Fn* fn1 = reinterpret_cast<const Fn*>(pd1);
      return fn0->args.size() == fn1->args.size() && equivModOffset(fn0->t, fn1->t);
    }
  default:
    assert(false && "Invalid type description, internal error");
    return false;
  }
}

// standard memory layout
inline size_t alignOf(const desc& t) {
  const D*         pd   = t.get();
  const Prim*      pt   = reinterpret_cast<const Prim*>(pd);
  const Var*       pvn  = reinterpret_cast<const Var*>(pd);
  const FArr*      pfa  = reinterpret_cast<const FArr*>(pd);
  const Variant*   pv   = reinterpret_cast<const Variant*>(pd);
  const Struct*    ps   = reinterpret_cast<const Struct*>(pd);

  switch (t->tid) {
  case PRIV_HPPF_TYCTOR_PRIM:
    if (pt->rep) {
      return alignOf(pt->rep);
    } else {
      static std::map<std::string, size_t> paligns = {
        { "unit",   1 }, { "bool", 1 }, { "char", 1 }, { "byte",  1 },
        { "short",  2 }, { "int",  4 }, { "long", 8 }, { "float", 4 },
        { "double", 8 }
      };
      auto pa = paligns.find(pt->n);
      if (pa != paligns.end()) {
        return pa->second;
      } else {
        throw std::runtime_error("Can't determine alignment of unknown primitive type: " + pt->n);
      }
    }

  case PRIV_HPPF_TYCTOR_TVAR:
    throw std::runtime_error("Can't determine alignment of type variable: " + pvn->n);

  case PRIV_HPPF_TYCTOR_FIXEDARR:
    return alignOf(pfa->t);

  case PRIV_HPPF_TYCTOR_ARR:
    throw std::runtime_error("Can't determine alignment of variable-length array type: " + show(t));

  case PRIV_HPPF_TYCTOR_VARIANT:
    if (pv->ctors.size() == 0) {
      return 1;
    } else {
      size_t maxAlign = 4; // the variant tag is an int
      for (const auto& ctor : pv->ctors) {
        maxAlign = std::max<size_t>(maxAlign, alignOf(ctor.at<2>()));
      }
      return maxAlign;
    }

  case PRIV_HPPF_TYCTOR_STRUCT:
    if (ps->fields.size() == 0) {
      return 1;
    } else {
      size_t maxAlign = 1;
      for (const auto& field : ps->fields) {
        maxAlign = std::max<size_t>(maxAlign, alignOf(field.at<2>()));
      }
      return maxAlign;
    }

  case PRIV_HPPF_TYCTOR_SIZE:
    throw std::runtime_error("Can't determine alignment of type-level number: " + show(t));

  case PRIV_HPPF_TYCTOR_TAPP:
    throw std::runtime_error("nyi, alignment of " + show(t));

  case PRIV_HPPF_TYCTOR_RECURSIVE:
    return alignof(void*);

  case PRIV_HPPF_TYCTOR_TABS:
    throw std::runtime_error("Can't decide alignment of type abstraction: " + show(t));

  default:
    throw std::runtime_error("Invalid type description to alignOf, internal error");
  }
}

inline size_t sizeOf(const desc& t) {
  const D*         pd   = t.get();
  const Prim*      pt   = reinterpret_cast<const Prim*>(pd);
  const Var*       pvn  = reinterpret_cast<const Var*>(pd);
  const FArr*      pfa  = reinterpret_cast<const FArr*>(pd);
  const Variant*   pv   = reinterpret_cast<const Variant*>(pd);
  const Struct*    ps   = reinterpret_cast<const Struct*>(pd);

  switch (t->tid) {
  case PRIV_HPPF_TYCTOR_PRIM:
    if (pt->rep) {
      return sizeOf(pt->rep);
    } else if (pt->n == "unit") {
      return 0;
    } else {
      return alignOf(t);
    }

  case PRIV_HPPF_TYCTOR_TVAR:
    throw std::runtime_error("Can't determine size of type variable: " + pvn->n);

  case PRIV_HPPF_TYCTOR_FIXEDARR:
    if (pfa->len->tid == PRIV_HPPF_TYCTOR_SIZE) {
      return sizeOf(pfa->t) * reinterpret_cast<const Nat*>(pfa->len.get())->x;
    } else {
      throw std::runtime_error("Can't determine size of fixed array: " + show(t));
    }

  case PRIV_HPPF_TYCTOR_ARR:
    throw std::runtime_error("Can't determine size of variable-length array type: " + show(t));

  case PRIV_HPPF_TYCTOR_VARIANT:
    if (pv->ctors.size() == 0) {
      return 4;
    } else {
      size_t maxSize  = 4; // the variant tag is an int
      size_t maxAlign = 4;
      for (const auto& ctor : pv->ctors) {
        maxSize  = std::max<size_t>(maxSize,  sizeOf(ctor.at<2>()));
        maxAlign = std::max<size_t>(maxAlign, alignOf(ctor.at<2>()));
      }
      return alignTo(alignTo(4, maxAlign) + maxSize, maxAlign);
    }

  case PRIV_HPPF_TYCTOR_STRUCT:
    if (ps->fields.size() == 0) {
      return 1;
    } else {
      size_t maxAlign = 1;
      size_t offset   = 0;
      for (const auto& field : ps->fields) {
        maxAlign = std::max<size_t>(maxAlign, alignOf(field.at<2>()));
        offset   = alignTo(offset, alignOf(field.at<2>())) + sizeOf(field.at<2>());
      }
      return alignTo(offset, maxAlign);
    }

  case PRIV_HPPF_TYCTOR_SIZE:
    throw std::runtime_error("Can't determine size of type-level number: " + show(t));

  case PRIV_HPPF_TYCTOR_TAPP:
    throw std::runtime_error("nyi, size of " + show(t));

  case PRIV_HPPF_TYCTOR_RECURSIVE:
    return sizeof(void*);

  case PRIV_HPPF_TYCTOR_TABS:
    throw std::runtime_error("Can't decide size of type abstraction: " + show(t));

  default:
    throw std::runtime_error("Invalid type description for sizeOf, internal error");
  }
}

}
}

#endif

