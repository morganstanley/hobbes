
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
    typedef T object_type;

    const std::shared_ptr<T>& get(const std::function<T*(Args...)>& mk, const Args&... args) {
      std::lock_guard<std::mutex> lock(mutex);
      std::shared_ptr<T>& r = this->values[std::tuple<Args...>(args...)];
      if (!r) {
        r = std::shared_ptr<T>(mk(args...));
      }
      return r;
    }

    size_t compact() {
      size_t c = 0;
      std::lock_guard<std::mutex> lock(mutex);
      for (typename Values::iterator v = this->values.begin(); v != this->values.end();) {
        if (v->second.use_count() == 1) {
          this->values.erase(v++);
          ++c;
        } else {
          ++v;
        }
      }
      return c;
    }
  private:
    std::mutex mutex;
    typedef std::unordered_map<std::tuple<Args...>, std::shared_ptr<T>, genHash<std::tuple<Args...>>> Values;
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
    typedef std::tuple<UniqueRefcMaps...> RefcMaps;
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
      return (const T*)s;
    } else {
      return 0;
    }
  }

template <typename T, typename S>
  T* isM(S* s) {
    if (s && s->case_id() == T::type_case_id) {
      return (T*)s;
    } else {
      return 0;
    }
  }

template <typename T, typename S>
  T* is(const std::shared_ptr<S>& s) {
    return isM<T, S>(s.get());
  }

}

#endif
