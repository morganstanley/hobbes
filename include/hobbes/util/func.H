
#ifndef HOBBES_UTIL_FUNC_HPP_INCLUDED
#define HOBBES_UTIL_FUNC_HPP_INCLUDED

namespace hobbes {

template <typename T>
  struct func {
  };

template <typename R, typename ... Args>
  struct func<R(Args...)> {
    static const std::size_t arity = sizeof...(Args);
    typedef R (*type)(Args...);
    typedef R result_type;
  };

template <typename R, typename ... Args>
  struct func<R(*)(Args...)> {
    static const std::size_t arity = sizeof...(Args);
    typedef R (*type)(Args...);
    typedef R result_type;
  };

template <typename R, typename C, typename ... Args>
  struct func<R(C::*)(Args...)> {
    static const std::size_t arity = sizeof...(Args);
    typedef R (*type)(C*, Args...);
    typedef R result_type;
  };

template <typename T>
  struct mfnTraits {
  };

template <typename R, typename C, typename ... Args>
  struct mfnTraits<R(C::*)(Args...)const> {
    static const int arity = sizeof...(Args);
    typedef R result_type;
    typedef C class_type;
  };

template <typename R, typename C, typename ... Args>
  struct mfnTraits<R(C::*)(Args...)> {
    static const int arity = sizeof...(Args);
    typedef R result_type;
    typedef C class_type;
  };

template <typename X, typename T, T f>
  struct mfnThunk {
  };

template <typename R, typename C, typename ... Args, typename T, T f>
  struct mfnThunk<R(C::*)(Args...), T, f> {
    static R fn(C* c, Args... args) {
      return (c->*f)(args...);
    }
  };
template <typename R, typename C, typename ... Args, typename T, T f>
  struct mfnThunk<R(C::*)(Args...)const, T, f> {
    static R fn(C* c, Args... args) {
      return (c->*f)(args...);
    }
  };
template <typename C, typename ... Args, typename T, T f>
  struct mfnThunk<void(C::*)(Args...), T, f> {
    static void fn(C* c, Args... args) {
      (c->*f)(args...);
    }
  };
template <typename C, typename ... Args, typename T, T f>
  struct mfnThunk<void(C::*)(Args...)const, T, f> {
    static void fn(C* c, Args... args) {
      (c->*f)(args...);
    }
  };

}

#endif
