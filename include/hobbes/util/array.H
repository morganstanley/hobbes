
#ifndef HOBBES_UTIL_ARRAY_HPP_INCLUDED
#define HOBBES_UTIL_ARRAY_HPP_INCLUDED

#include <map>
#include <set>
#include <vector>
#include <stdexcept>
#include <algorithm>
#include <functional>
#include <sstream>
#include <string.h>

namespace hobbes {

// a unit value, since C++ doesn't handle 'void' correctly
typedef unsigned char UnitV;
static const UnitV unitv = 0x00;

// byte sequences, a common type of array
typedef std::vector<uint8_t> bytes;

// shorthand for list construction (maybe not necessary with C++ initializer lists)
template <typename T>
  std::vector<T> list() {
    return std::vector<T>();
  }

template <typename T, typename ... Ts>
  std::vector<T> list(const T& x, const Ts& ... xs) {
    std::vector<T> r = {x,xs...};
    return r;
  }

// [i..e]
template <typename T>
  std::vector<T> range(const T& i, const T& e) {
    std::vector<T> result;
    for (T t = i; t < e; ++t) {
      result.push_back(t);
    }
    return result;
  }

// x in (xs :: set T)
template <typename T>
  bool in(T x, const std::set<T>& xs) {
    return xs.find(x) != xs.end();
  }

// x in (xs :: vector T)
template <typename T>
  bool in(T x, const std::vector<T>& xs) {
    for (typename std::vector<T>::const_iterator xi = xs.begin(); xi != xs.end(); ++xi) {
      if (*xi == x) {
        return true;
      }
    }
    return false;
  }

template <typename T>
  int index(const std::vector<T>& xs, T x) {
    for (int i = 0; i < xs.size(); ++i) {
      if (xs[i] == x) {
        return i;
      }
    }

    std::ostringstream ss;
    ss << x << " not in [";
    if (xs.size() > 0) {
      ss << xs[0];
      for (size_t i = 1; i < xs.size(); ++i) {
        ss << ", " << xs[i];
      }
    }
    ss << "]";
    throw std::runtime_error(ss.str());
  }

template <typename T>
  std::vector<int> index(const std::vector<T>& xs, const std::vector<T>& lxs) {
    std::vector<int> result;
    for (typename std::vector<T>::const_iterator lx = lxs.begin(); lx != lxs.end(); ++lx) {
      result.push_back(index<T>(xs, *lx));
    }
    return result;
  }

template <typename T, typename I>
  T select(const std::vector<T>& xs, I i) {
    return xs[i];
  }

template <typename T, typename I>
  std::vector<T> select(const std::vector<T>& xs, I b, I e) {
    std::vector<T> r;
    for (I j = b; j < e; ++j) {
      r.push_back(select(xs, j));
    }
    return r;
  }

template <typename T, typename I>
  std::vector<T> select(const std::vector<T>& xs, const std::vector<I>& is) {
    std::vector<T> r;
    for (typename std::vector<I>::const_iterator i = is.begin(); i != is.end(); ++i) {
      r.push_back(select(xs, *i));
    }
    return r;
  }

template <typename K, typename V>
  std::pair<K, V> select(const std::map<K, V>& m, K k) {
    typename std::map<K,V>::const_iterator mi = m.find(k);

    if (mi == m.end()) {
      throw std::runtime_error("domain out of range error in map lookup");
    } else {
      return *mi;
    }
  }

template <typename K, typename V>
  std::vector< std::pair<K, V> > select(const std::map<K, V>& m, const std::vector<K>& ks) {
    std::vector< std::pair<K, V> > result;
    for (typename std::vector<K>::const_iterator k = ks.begin(); k != ks.end(); ++k) {
      result.push_back(select(m, *k));
    }
    return result;
  }

template <typename K, typename V>
  std::map<K, V> drop(const std::map<K, V>& m, const std::set<K>& ks) {
    std::map<K, V> result;
    for (typename std::map<K,V>::const_iterator p = m.begin(); p != m.end(); ++p) {
      if (ks.find(p->first) == ks.end()) {
        result[p->first] = p->second;
      }
    }
    return result;
  }

template <typename T>
  std::vector<T> toVector(const std::set<T>& xs) {
    return std::vector<T>(xs.begin(), xs.end());
  }

template <typename CT>
  std::set<typename CT::value_type> toSet(const CT& xs) {
    std::set<typename CT::value_type> r;
    for (auto x : xs) {
      r.insert(x);
    }
    return r;
  }

template <typename CT>
  inline CT fromSet(const std::set<typename CT::value_type>& xs) {
    CT r;
    for (auto x : xs) {
      r.insert(r.end(), x);
    }
    return r;
  }

template <typename T>
  std::set<T> setUnion(const std::set<T>& lhs, const std::set<T>& rhs) {
    std::set<T> r;
    std::set_union(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), std::inserter(r, r.begin()));
    return r;
  }

template <typename T>
  std::set<T> setUnion(const std::vector< std::set<T> >& ss) {
    std::set<T> r;
    for (typename std::vector< std::set<T> >::const_iterator s = ss.begin(); s != ss.end(); ++s) {
      r.insert(s->begin(), s->end());
    }
    return r;
  }

template <typename T>
  std::set<T> setDifference(const std::set<T>& lhs, const std::set<T>& rhs) {
    std::set<T> r;
    std::set_difference(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), std::inserter(r, r.begin()));
    return r;
  }

template <typename T>
  std::set<T> setDifference(const std::set<T>& lhs, const T& x) {
    std::set<T> sx;
    sx.insert(x);
    return setDifference(lhs, sx);
  }

template <typename K, typename V>
  std::set<K> keys(const std::map<K, V>& m) {
    std::set<K> r;
    for (typename std::map<K, V>::const_iterator kv = m.begin(); kv != m.end(); ++kv) {
      r.insert(kv->first);
    }
    return r;
  }

template <typename L, typename R>
  std::vector<L> first(const std::vector< std::pair<L, R> >& xs) {
    std::vector<L> result;
    result.reserve(xs.size());
    for (typename std::vector< std::pair<L, R> >::const_iterator x = xs.begin(); x != xs.end(); ++x) {
      result.push_back(x->first);
    }
    return result;
  }

template <typename K, typename V>
  std::vector<V> values(const std::map<K, V>& m) {
    std::vector<V> r;
    r.reserve(m.size());
    for (typename std::map<K, V>::const_iterator kv = m.begin(); kv != m.end(); ++kv) {
      r.push_back(kv->second);
    }
    return r;
  }

template <typename L, typename R>
  std::vector<R> second(const std::vector< std::pair<L, R> >& xs) {
    std::vector<R> result;
    result.reserve(xs.size());
    for (typename std::vector< std::pair<L, R> >::const_iterator x = xs.begin(); x != xs.end(); ++x) {
      result.push_back(x->second);
    }
    return result;
  }

template <typename L, typename R>
  std::pair< std::vector<L>, std::vector<R> > unzip(const std::vector< std::pair<L, R> >& ps) {
    return std::pair< std::vector<L>, std::vector<R> >(first(ps), second(ps));
  }

template <typename L, typename R>
  std::vector< std::pair<L, R> > zip(const std::vector<L>& left, const std::vector<R>& right) {
    std::vector< std::pair<L, R> > r;
    size_t n = std::min<size_t>(left.size(), right.size());
    r.reserve(n);
    for (size_t i = 0; i < n; ++i) {
      r.push_back(std::pair<L, R>(left[i], right[i]));
    }
    return r;
  }

template <typename T>
  std::vector<T> take(const std::vector<T>& xs, size_t n) {
    return std::vector<T>(xs.begin(), xs.begin() + std::min(xs.size(), n));
  }

template <typename T>
  std::vector<T> drop(const std::vector<T>& xs, size_t n) {
    if (n >= xs.size()) {
      return std::vector<T>();
    } else {
      return std::vector<T>(xs.begin() + n, xs.end());
    }
  }

template <typename T>
  std::vector<std::string> show(const std::vector<T>& xs) {
    std::vector<std::string> r;
    for (auto x = xs.begin(); x != xs.end(); ++x) {
      r.push_back(show(*x));
    }
    return r;
  }

template <typename CT, typename CCT>
  CT concat(const CCT& cs) {
    CT r;
    for (const auto& c : cs) {
      r.insert(r.end(), c.begin(), c.end());
    }
    return r;
  }

template <typename T>
  std::vector<T> cons(T h, std::vector<T> t) {
    t.insert(t.begin(), h);
    return t;
  }

template <typename T>
  void append(std::vector<T>* xs, const std::vector<T>& ys) {
    xs->insert(xs->end(), ys.begin(), ys.end());
  }

template <typename T>
  std::vector<T> append(const std::vector<T>& xs, T x) {
    std::vector<T> r(xs);
    r.push_back(x);
    return r;
  }

template <typename T>
  std::vector<T> append(const std::vector<T>& xs, const std::vector<T>& ys) {
    std::vector<T> r;
    r.reserve(xs.size() + ys.size());
    append(&r, xs);
    append(&r, ys);
    return r;
  }


// basic bit-packed 2D bool array (maybe there's already something that does this job?)
class bit_table {
public:
  inline bit_table() : data(0), rowc(0), colc(0) {
  }
  inline bit_table(size_t rowc, size_t colc, bool s) : rowc(rowc), colc(colc) {
    size_t msz = 1+((rowc*colc)/8);
    this->data = new uint8_t[msz];
    memset(this->data, s ? 0xFF : 0, msz);
  }
  inline bit_table(const bit_table& rhs) : rowc(rhs.rowc), colc(rhs.colc) {
    size_t msz = 1+((this->rowc*this->colc)/8);
    this->data = new uint8_t[msz];
    memcpy(this->data, rhs.data, msz);
  }
  inline bit_table& operator=(const bit_table& rhs) {
    if (this != &rhs) {
      delete[] this->data;
      this->rowc = rhs.rowc;
      this->colc = rhs.colc;
      size_t msz = 1+((this->rowc*this->colc)/8);
      this->data = new uint8_t[msz];
      memcpy(this->data, rhs.data, msz);
    }
    return *this;
  }
  inline ~bit_table() {
    delete[] this->data;
  }
  inline bool operator()(size_t r, size_t c) const {
    size_t  i = (r*this->colc)+c;
    size_t  k = i / 8;
    uint8_t b = i % 8;
    return (this->data[k] & (1 << b)) != 0;
  }
  inline void set(size_t r, size_t c, bool f) {
    size_t  i = (r*this->colc)+c;
    size_t  k = i / 8;
    uint8_t b = i % 8;

    if (f) {
      this->data[k] |= 1 << b;
    } else {
      this->data[k] &= ~(1 << b);
    }
  }
  inline size_t rows() const { return this->rowc; }
  inline size_t cols() const { return this->colc; }
private:
  uint8_t* data;
  size_t   rowc, colc;
};
inline std::ostream& operator<<(std::ostream& out, const bit_table& bt) {
  for (size_t r = 0; r != bt.rows(); ++r) {
    for (size_t c = 0; c != bt.cols(); ++c) {
      out << (bt(r,c) ? "1 " : "0 ");
    }
    out << "\n";
  }
  return out;
}

}

#endif
