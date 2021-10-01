
#ifndef HOBBES_UTIL_AUTORELEASE_HPP_INCLUDED
#define HOBBES_UTIL_AUTORELEASE_HPP_INCLUDED

#include <vector>

namespace hobbes {

// simplify automatic destruction within bison parsers
struct AutoreleaseSet {
  using presetfn = void (*)();
  using ResetFns = std::vector<presetfn>;

  static ResetFns& resetFns() {
    thread_local ResetFns rs;
    return rs;
  }

  template <presetfn fn>
    static bool registerClearFn() {
      resetFns().push_back(fn);
      return true;
    }

  static void reset() {
    ResetFns& rfs = resetFns();
    for (ResetFns::const_iterator f = rfs.begin(); f != rfs.end(); ++f) {
      (*f)();
    }
  }
};

template <typename T>
  struct Autorelease {
    using Vals = std::vector<T *>;
    static Vals& vals() {
      thread_local Vals vs;
      return vs;
    }

    static void reset() {
      Vals& vs = vals();
      for (auto i = vs.begin(); i != vs.end(); ++i) {
        delete *i;
      }
      vs.clear();
    }

    static void mark(T* x) {
      thread_local bool saved = false;
      if (!saved) {
        AutoreleaseSet::registerClearFn<&Autorelease<T>::reset>();
        saved = true;
      }
      vals().push_back(x);
    }
  };

template <typename T>
  T* autorelease(T* x) {
    Autorelease<T>::mark(x);
    return x;
  }

}

#endif
