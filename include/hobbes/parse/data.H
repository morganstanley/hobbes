
#ifndef HOBBES_PARSE_DATAUTIL_HPP_INCLUDED
#define HOBBES_PARSE_DATAUTIL_HPP_INCLUDED

#include <hobbes/util/array.H>
#include <string>
#include <sstream>
#include <vector>
#include <stack>
#include <set>
#include <map>
#include <stdexcept>
#include <typeinfo>
#include <cxxabi.h>

#include <memory>

namespace hobbes {

using nat = unsigned int;
using nats = std::vector<nat>;

template <typename T>
  std::string show(const T* t) {
    std::ostringstream ss;
    t->show(ss);
    return ss.str();
  }

template <typename T>
  std::string show(const std::vector<T*>& ts) {
    std::ostringstream ss;
    ss << "[";
    if (!ts.empty()) {
      ts[0]->show(ss);
      for (unsigned int i = 1; i < ts.size(); ++i) {
        ss << ", ";
        ts[i]->show(ss);
      }
    }
    ss << "]";
    return ss.str();
  }

// a little bit of state to map a sequence index to line numbers
// we just watch characters and record newlines
class linedb {
public:
  using ptr = std::shared_ptr<linedb>;
  using LineCol = std::pair<nat, nat>;

  linedb(nat stype, const std::string& sdesc);

  nat                sourceType() const;
  const std::string& sourceDesc() const;

  void step(char x);
  void reset();

  LineCol pos(nat i) const;
private:
  nat         stype;
  std::string sdesc;
  nat         c;
  nats        lp;    // sequence positions corresponding to line index
};

// load a set of lines around an implicated set from the specified line database
using strings = std::vector<std::string>;
using ldblines = std::pair<nats, strings>;

ldblines load(const linedb::ptr& ldb, const linedb::LineCol& i, const linedb::LineCol& f);

template <typename K, typename V>
  V mapLookup(const std::map<K, V>& m, const K& k) {
    auto i = m.find(k);
    if (i == m.end()) {
      throw std::runtime_error("Unexpected map index undefined");
    } else {
      return i->second;
    }
  }

}

#endif

