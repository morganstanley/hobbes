/****
 * variant : yet another POD variant implementation
 ****/

#ifndef HOBBES_UTIL_VARIANT_HPP_INCLUDED
#define HOBBES_UTIL_VARIANT_HPP_INCLUDED

#include <string.h>

namespace hobbes {

// some simple compile-time logic
template <bool f, typename T, typename F>
  struct TIfF { };
template <typename T, typename F>
  struct TIfF<true, T, F> { typedef T type; };
template <typename T, typename F>
  struct TIfF<false, T, F> { typedef F type; };

// sizeof as a type-level function
template <typename T>
  struct TSizeOfF {
    static const size_t value = sizeof(T);
  };

// alignof as a type-level function
template <typename T>
  struct TAlignOfF {
    static const size_t value = alignof(T);
  };

// find a maximum property of a sequence of types
template <template <class> class SzP, typename T0, typename ... Ts>
  struct maximum {
    static const size_t value = SzP<T0>::value;
    typedef T0 type;
  };

template <template <class> class SzP, typename T0, typename T1, typename ... Ts>
  struct maximum<SzP, T0, T1, Ts...> : public maximum<SzP, typename TIfF<SzP<T1>::value < SzP<T0>::value, T0, T1>::type, Ts...> {
  };

// count a number of type arguments
template <typename ... Ts>
  struct CountF {
    static const size_t value = sizeof...(Ts);
  };

// find ctor index by type equality
template <typename T, typename ... Ctors>
  struct CtorIndexOf {
    static const uint32_t value = 0;
  };
template <typename T, typename ... Ctors>
  struct CtorIndexOf<T, T, Ctors...> {
    static const uint32_t value = 0;
  };
template <typename T, typename Ctor, typename ... Ctors>
  struct CtorIndexOf<T, Ctor, Ctors...> {
    static const uint32_t value = 1 + CtorIndexOf<T, Ctors...>::value;
  };

// get the first type in a sequence
template <typename T, typename ... Ts>
  struct First {
    typedef T type;
  };

// a variantSplit-like copy-constructor selector
template <typename ... Ctors>
  struct CCtorAt0 {
    static void invoke(uint32_t,void*,const void*) { } // shouldn't be possible
  };
template <typename Ctor, typename ... Ctors>
  struct CCtorAt0<Ctor, Ctors...> {
    static void invoke(uint32_t t, void* to, const void* from) {
      if (t == 0) {
        new (to) Ctor(*((const Ctor*)from));
      } else {
        CCtorAt0<Ctors...>::invoke(t-1, to, from);
      }
    }
  };

// a variantSplit-like destructor selector
template <typename ... Ctors>
  struct DtorAt0 {
    static void invoke(uint32_t,void*) { } // shouldn't be possible
  };
template <typename Ctor, typename ... Ctors>
  struct DtorAt0<Ctor, Ctors...> {
    static void invoke(uint32_t t, void* p) {
      if (t == 0) {
        ((Ctor*)p)->~Ctor();
      } else {
        DtorAt0<Ctors...>::invoke(t-1, p);
      }
    }
  };

// a simple tagged variant representation
template <typename ... Ctors>
  class variant {
  public:
    static_assert(sizeof...(Ctors) > 0, "Empty variants are impossible to construct");

    variant() : tag(0) {
      new (this->storage) typename First<Ctors...>::type();
    }
    template <typename T>
      variant(const T& t) : tag(CtorIndexOf<T, Ctors...>::value) {
        static_assert(CtorIndexOf<T, Ctors...>::value < sizeof...(Ctors), "Constructor type isn't part of variant");
        new (this->storage) T(t);
      }
    variant(const variant<Ctors...>& rhs) : tag(rhs.tag) {
      CCtorAt0<Ctors...>::invoke(this->tag, this->storage, rhs.storage);
    }

    ~variant() {
      DtorAt0<Ctors...>::invoke(this->tag, this->storage);
    }

    variant<Ctors...>& operator=(const variant<Ctors...>& rhs) {
      if (this != &rhs) {
        DtorAt0<Ctors...>::invoke(this->tag, this->storage);
        this->tag = rhs.tag;
        CCtorAt0<Ctors...>::invoke(this->tag, this->storage, rhs.storage);
      }
      return *this;
    }

    template <typename T>
      T* get() { return findByCtor<T>(); }
    template <typename T>
      const T* get() const { return findByCtor<T>(); }
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
          return (T*)(this->storage);
        } else {
          return 0;
        }
      }
  };

template <typename T, typename ... Ctors>
  T* get(variant<Ctors...>& v) {
    return v.template get<T>();
  }
template <typename T, typename ... Ctors>
  const T* get(const variant<Ctors...>& v) {
    return v.template get<T>();
  }

// the trivially true proposition -- ie: C's "void" with its one value constructible
struct unit { unit() { } };

}

#endif

