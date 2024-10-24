
#ifndef HOBBES_UTIL_FUNC_HPP_INCLUDED
#define HOBBES_UTIL_FUNC_HPP_INCLUDED

#include "ptr.H"

namespace hobbes {

template <typename T> struct func {};

template <typename R, typename... Args> struct func<R(Args...)> {
  static const std::size_t arity = sizeof...(Args);
  using type = R (*)(Args...);
  using result_type = R;
};

template <typename R, typename... Args> struct func<R (*)(Args...)> {
  static const std::size_t arity = sizeof...(Args);
  using type = R (*)(Args...);
  using result_type = R;
};

template <typename R, typename C, typename... Args>
struct func<R (C::*)(Args...)> {
  static const std::size_t arity = sizeof...(Args);
  using type = R (*)(C *, Args...);
  using result_type = R;
};

template <typename T> struct mfnTraits {};

template <typename R, typename C, typename... Args>
struct mfnTraits<R (C::*)(Args...) const> {
  static const int arity = sizeof...(Args);
  using result_type = R;
  using class_type = C;
};

template <typename R, typename C, typename... Args>
struct mfnTraits<R (C::*)(Args...)> {
  static const int arity = sizeof...(Args);
  using result_type = R;
  using class_type = C;
};

template <typename T> struct boolSafeFn : public func<T> {
  using Type = typename func<T>::type;
  Type fn = nullptr;
  boolSafeFn(Type fn) : fn(fn) {}
  template <class... Args> auto operator()(Args &&...args) -> decltype(auto) {
    return fn(std::forward<Args>(args)...);
  }
};

// smunix: avoid undefined behavior introduced by llvm-9+
// https://godbolt.org/z/nWWT9jY63
// https://bugs.llvm.org/show_bug.cgi?id=51163

template <typename... Args>
struct boolSafeFn<bool(Args...)> : public func<bool(Args...)> {
  using Type = typename func<bool(Args...)>::type;
  Type fn = nullptr;
  boolSafeFn(Type fn) : fn(fn) {}
  auto operator()(Args &&...args) -> decltype(auto) {
    const bool r = (fn)(std::forward<Args>(args)...);
    return (0x00 | *rcast<std::uint8_t const *>(&r));
  }
};

template <typename... Args>
struct boolSafeFn<bool (*)(Args...)> : public boolSafeFn<bool(Args...)> {
  using Type = typename func<bool(Args...)>::type;
  boolSafeFn(Type fn) : boolSafeFn<bool(Args...)>(fn) {}
};

template <typename X, typename T, T f> struct mfnThunk {};

template <typename R, typename C, typename... Args, typename T, T f>
struct mfnThunk<R (C::*)(Args...), T, f> {
  static R fn(C *c, Args... args) { return (c->*f)(args...); }
};
template <typename R, typename C, typename... Args, typename T, T f>
struct mfnThunk<R (C::*)(Args...) const, T, f> {
  static R fn(C *c, Args... args) { return (c->*f)(args...); }
};
template <typename C, typename... Args, typename T, T f>
struct mfnThunk<void (C::*)(Args...), T, f> {
  static void fn(C *c, Args... args) { (c->*f)(args...); }
};
template <typename C, typename... Args, typename T, T f>
struct mfnThunk<void (C::*)(Args...) const, T, f> {
  static void fn(C *c, Args... args) { (c->*f)(args...); }
};

} // namespace hobbes

#endif
