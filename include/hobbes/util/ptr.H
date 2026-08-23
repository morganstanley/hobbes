
#ifndef HOBBES_UTIL_PTR_HPP_INCLUDED
#define HOBBES_UTIL_PTR_HPP_INCLUDED

#include <hobbes/util/hash.H>
#include <memory>
#include <vector>
#include <unordered_map>
#include <map>
#include <iostream>
#include <atomic>
#include <mutex>

namespace hobbes {

template <typename T, typename ... Args>
  class unique_refc_map {
  public:
    using object_type = T;

    // returned by value rather than by reference into the map: compact() can
    // erase an entry from another thread as soon as the lock is released, and
    // a reference handed out here would then be to storage that is gone. A
    // copy is a reference of its own, which also keeps the entry from being
    // the one compact() erases while the caller still holds it.
    std::shared_ptr<T> get(const std::function<T*(Args...)>& mk, const Args&... args) {
      std::lock_guard<std::recursive_mutex> lock(mutex);
      auto k = std::tuple<Args...>(args...);

      auto found = this->values.find(k);
      if (found != this->values.end() && found->second) {
        return found->second;
      }

      // no reference into the map is held across mk(): constructing a type can
      // intern the types it is made of, re-entering this map (the mutex is
      // recursive for that reason), and an insertion there can rehash it. So
      // construct first, then look the key up again, and keep whichever entry
      // is there -- mk() may have interned this very key on the way.
      std::shared_ptr<T> made(mk(args...));
      std::shared_ptr<T>& slot = this->values[k];
      if (!slot) {
        slot = made;
      }
      return slot;
    }

    size_t compact() {
      size_t c = 0;
      std::lock_guard<std::recursive_mutex> lock(mutex);
      for (auto v = this->values.begin(); v != this->values.end();) {
        if (v->second.use_count() == 1) {
          v = this->values.erase(v);
          ++c;
        } else {
          ++v;
        }
      }
      return c;
    }
  private:
    std::recursive_mutex mutex;
    using Values = std::unordered_map<std::tuple<Args...>, std::shared_ptr<T>, genHash<std::tuple<Args...>>>;
    Values values;
  };

template <size_t i, typename ... UniqueRefcMaps>
  inline typename std::enable_if<i == sizeof...(UniqueRefcMaps), size_t>::type compactAll(std::tuple<UniqueRefcMaps...>&) {
    return 0;
  }

template <size_t i, typename ... UniqueRefcMaps>
  inline typename std::enable_if<i < sizeof...(UniqueRefcMaps), size_t>::type compactAll(std::tuple<UniqueRefcMaps...>& ms) {
    return std::get<i>(ms).compact() + compactAll<i+1, UniqueRefcMaps...>(ms);
  }

template <size_t i, typename T, typename ... Ts>
  struct TypeIndex {
    static const size_t value = i;
  };

template <size_t i, typename T, typename ... Ts>
  struct TypeIndex<i, T, T, Ts...> {
    static const size_t value = i;
  };

template <size_t i, typename T, typename U, typename ... Ts>
  struct TypeIndex<i, T, U, Ts...> {
    static const size_t value = TypeIndex<i+1, T, Ts...>::value;
  };

template <typename ... UniqueRefcMaps>
  class unique_refc_maps {
  private:
    using RefcMaps = std::tuple<UniqueRefcMaps...>;
    RefcMaps refcMaps;
  public:
    template <typename T>
      typename std::tuple_element<TypeIndex<0, T, UniqueRefcMaps...>::value, RefcMaps>::type& at() {
        return std::get<TypeIndex<0, T, UniqueRefcMaps...>::value>(this->refcMaps);
      }

    size_t compact() {
      size_t tc = 0;
      while (true) {
        size_t c = compactAll<0>(this->refcMaps);
        tc += c;
        if (c == 0) break;
      }
      return tc;
    }
  };

template <typename V, typename U> V scast (U x)        { return static_cast<V>(x); }
template <typename V, typename U> V rcast (U x)        { return reinterpret_cast<V>(x); }
template <typename V, typename U> V crcast(const U* x) { return reinterpret_cast<V>(const_cast<U*>(x)); }

template <typename T>
  T align(T x, T m) {
    if (m == 0 || (x % m) == 0) {
      return x;
    } else {
      return (1 + (x / m)) * m;
    }
  }

template <typename T, typename S>
  const T* is(const S* s) {
    if (s && s->case_id() == T::type_case_id) {
      return rcast<const T*>(s);
    } else {
      return nullptr;
    }
  }

template <typename T, typename S>
  T* isM(S* s) {
    if (s && s->case_id() == T::type_case_id) {
      return rcast<T*>(s);
    } else {
      return nullptr;
    }
  }

template <typename T, typename S>
  T* is(const std::shared_ptr<S>& s) {
    return isM<T, S>(s.get());
  }

}

#endif
