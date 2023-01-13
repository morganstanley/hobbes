/*
 * hash : utilities for hashing data
 */

#ifndef HOBBES_UTIL_HASH_H_INCLUDED
#define HOBBES_UTIL_HASH_H_INCLUDED

#include <cstddef>
#include <functional>
#include <unordered_map>
#include <map>
#include <vector>
#include <tuple>

namespace hobbes {

template <typename T>
  struct genHash {
    inline size_t operator()(const T& x) const {
      static std::hash<T> h;
      return h(x);
    }
  };

template <typename T>
  inline void hashAppend(std::size_t& seed, const T& x) {
    genHash<T> hasher;
    seed ^= hasher(x) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  }

// hash tuples
template <std::size_t i, typename ... Ts>
  inline typename std::enable_if<i == sizeof...(Ts), void>::type hashTuple(std::size_t*, const std::tuple<Ts...>&) {
  }
template <std::size_t i, typename ... Ts>
  inline typename std::enable_if<i < sizeof...(Ts), void>::type hashTuple(std::size_t* r, const std::tuple<Ts...>& ms) {
    hashAppend(*r, std::get<i>(ms));
    hashTuple<i+1, Ts...>(r, ms);
  }
template <typename ... Ts>
  struct genHash<std::tuple<Ts...>> {
    inline size_t operator()(const std::tuple<Ts...>& xs) const {
      size_t r = 0;
      hashTuple<0, Ts...>(&r, xs);
      return r;
    }
  };

// hash pairs
template <typename U, typename V>
  struct genHash< std::pair<U, V> > {
    inline size_t operator()(const std::pair<U, V>& x) const {
      size_t r = 0;
      hashAppend<U>(r, x.first);
      hashAppend<V>(r, x.second);
      return r;
    }
  };

// hash vectors
template <typename T>
  struct genHash< std::vector<T> > {
    inline size_t operator()(const std::vector<T>& xs) const {
      size_t r = 0;
      hashAppend<size_t>(r, xs.size());
      for (const auto& x : xs) {
        hashAppend<T>(r, x);
      }
      return r;
    }
  };

}

#endif

