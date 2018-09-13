
#ifndef HOBBES_UTIL_STR_HPP_INCLUDED
#define HOBBES_UTIL_STR_HPP_INCLUDED

#include <hobbes/util/array.H>
#include <string>
#include <sstream>
#include <vector>
#include <cxxabi.h>
#include <stdlib.h>
#include <set>
#include <map>

namespace hobbes { namespace str {

std::string env(const std::string& varname);
void env(const std::string& varname, const std::string& value);

typedef std::vector<std::string>  seq;
typedef std::vector<seq>          seqs;
typedef std::vector<unsigned int> lengths;

void repeat(unsigned int n, const std::string& s, seq* out);
seq repeat(unsigned int n, const std::string& s);

template <typename C>
  unsigned int maxSize(unsigned int d, const std::vector<C>& vs) {
    if (vs.size() == 0) {
      return d;
    } else {
      unsigned int r = vs[0].size();
      for (unsigned int i = 1; i < vs.size(); ++i) {
        r = std::max<unsigned int>(r, vs[i].size());
      }
      return r;
    }
  }

template <typename C>
  unsigned int minSize(unsigned int d, const std::vector<C>& vs) {
    if (vs.size() == 0) {
      return d;
    } else {
      unsigned int r = vs[0].size();
      for (unsigned int i = 1; i < vs.size(); ++i) {
        r = std::min<unsigned int>(r, vs[i].size());
      }
      return r;
    }
  }

unsigned int tableCols(const seqs& tbl);
unsigned int tableRows(const seqs& tbl);
unsigned int maxStrLen(const seq&  col);
lengths maxStrLen(const seqs& tbl);
std::string pad(size_t n);
std::string leftAlign(size_t w, const std::string& x);
std::string rightAlign(size_t w, const std::string& x);
seq leftAlign(const seq& col);
seq rightAlign(const seq& col);
seqs leftAlign(const seqs& tbl);
seqs rightAlign(const seqs& tbl);
void printLeftAlignedTable(std::ostream& out, const seqs& tbl);
void printRightAlignedTable(std::ostream& out, const seqs& tbl);
std::string showLeftAlignedTable(const seqs& tbl);
std::string showRightAlignedTable(const seqs& tbl);
void printHeadlessLeftAlignedTable(std::ostream& out, const seqs& tbl);
void printHeadlessRightAlignedTable(std::ostream& out, const seqs& tbl);

template <typename T>
  bool is(const std::string& x) {
    std::istringstream ss(x);
    T dummy;
    ss >> dummy;
    return bool(ss);
  }

template <typename T>
  T to(const std::string& x) {
    std::istringstream ss(x);
    T r = T();
    ss >> r;
    return r;
  }

template <typename T>
  bool to(const std::string& x, T& out) {
    std::istringstream ss(x);
    ss >> out;
    return bool(ss);
  }

template <typename T>
  bool to(const char* b, const char* e, T* out) {
    std::istringstream ss(std::string(b, e));
    ss >> *out;
    return bool(ss);
  }

template <typename T>
  std::string from(const T& x) {
    std::ostringstream ss;
    ss << x;
    return ss.str();
  }

std::string demangle(const char* tn);
std::string demangle(const std::type_info& ti);

template <typename T>
  std::string demangle() {
    return demangle(typeid(T));
  }

template <typename Char>
  std::basic_string<Char> trim(const std::basic_string<Char>& s) {
    if (s.empty()) return s;

    size_t begin = 0;
    size_t end   = 0;

    for (size_t i = 0; i < s.size(); ++i) {
      int c = static_cast<int>(s[i]);
      if (!std::isspace(c) && c != 0) {
        begin = i;
        break;
      }
    }

    // finally, find the last non-whitespace character
    for (size_t i = s.size(); i > 0; --i) {
      int c = static_cast<int>(s[i-1]);
      if (!std::isspace(c) && c != 0) {
        end = i;
        break;
      }
    }

    if (begin == end) {
      return "";
    } else {
      return s.substr(begin, end - begin);
    }
  }

template <typename Char>
  std::pair< std::basic_string<Char>, std::basic_string<Char> > trim(const std::pair< std::basic_string<Char>, std::basic_string<Char> >& p) {
    return std::pair< std::basic_string<Char>, std::basic_string<Char> >(trim<Char>(p.first), trim<Char>(p.second));
  }

template <typename Char>
  std::vector< std::basic_string<Char> > trim(const std::vector< std::basic_string<Char> >& ss) {
    std::vector< std::basic_string<Char> > result;
    for (typename std::vector< std::basic_string<Char> >::const_iterator s = ss.begin(); s != ss.end(); ++s) {
      result.push_back(trim<Char>(*s));
    }
    return result;
  }

template <typename Char>
  std::basic_string<Char> trimq(const std::basic_string<Char>& s, Char q = static_cast<Char>('"')) {
    if (s.size() == 0) {
      return s;
    } else if (s.size() == 1) {
      return (s[0] == q) ? std::basic_string<Char>() : s;
    } else {
      const Char* b = s.c_str();
      const Char* e = b + s.size() - 1;
      if (*b == q) {
        ++b;
      }
      if (*e == q) {
        --e;
      }
      return std::basic_string<Char>(b, e + 1);
    }
  }

// replace [a b c b] [b c] [e] -> [a e b]
template <typename Char>
  std::basic_string<Char> replace(const std::basic_string<Char>& src, const std::basic_string<Char>& old_substr, const std::basic_string<Char>& new_substr) {
    if (old_substr.empty()) {
      return src;
    }

    typedef typename std::basic_string<Char>::size_type SZT;
    std::basic_string<Char> result;
    SZT                     sz     = src.find(old_substr);
    SZT                     lsz    = 0;

    while (sz != std::basic_string<Char>::npos) {
      result += std::string(src.begin() + lsz, src.begin() + sz);
      result += new_substr;

      lsz = sz + old_substr.size();
      sz  = src.find(old_substr, lsz);
    }

    if (lsz != std::basic_string<Char>::npos) {
      result += std::string(src.begin() + lsz, src.end());
    }

    return result;
  }

bool isNyb(char);
char denyb(char);
unsigned char dehex(const std::string&);
std::vector<unsigned char> dehexs(const std::string&);
char nyb(unsigned char x);
std::string hex(unsigned char);
std::string hex(const std::vector<unsigned char>&);
std::string hex(const unsigned char*, size_t);

std::string escape(const std::string&);
std::string unescape(const std::string&);

bool endsWith(const std::string& s, const std::string& sfx);

typedef std::pair<std::string, std::string> pair;

inline pair trim(const pair& p) {
  return pair(trim(p.first), trim(p.second));
}

pair splitAt(const std::string& s, unsigned int i);
pair lsplit(const std::string& s, const std::string& ss);
pair rsplit(const std::string& s, const std::string& ss);
seq csplit(const std::string& s, const std::string& ss);
pair readWhile(bool (*P)(char), const std::string& s);

bool isDigit(char c);
bool isNotDigit(char c);

unsigned int firstFailIndex(bool (*P)(char), const std::string& s);

// reverse csplit
std::string cdelim(const seq& ss, const std::string& d);

// common containers for strings
typedef std::set<std::string> set;
std::string show(const set&);

typedef std::map<std::string, std::string> named_strings;
std::string show(const named_strings&);

// process a string by 'expanding' embedded variables/expressions and unescaping sequences
template <typename T>
  T foldWithFormat(const std::string& str, const T& s, T (*constF)(const T&, const std::string&), T (*expF)(const T&, const std::string&)) {
    T r = s;

    std::ostringstream b;
    int x = 0;
    int bc = 0;

    for (size_t i = 0; i < str.size(); ++i) {
      char c = str[i];

      switch (x) {
      // process unformatted text (decide whether to start variables or unescape)
      case 0: {
          switch (c) {
          case '\\':
            x = 1;
            break;

          case '$':
            r = constF(r, b.str());
            b.str("");
            x = 2;
            break;

          default:
            b << c;
            break;
          }
          break;
        }
      // unescape
      case 1: {
          b << c;
          x = 0;
          break;
        }
      // process variables
      case 2: {
          if (c != '{') {
            b << c;
            x = 3;
          } else {
            bc = 1;
            x = 4;
          }
          break;
        }
      // process variable short-names (may only be alphanumeric)
      case 3: {
          if (std::isalnum(c) != 0 || c == '_') {
            b << c;
          } else {
            --i;
            r = expF(r, b.str());
            b.str("");
            x = 0;
          }
          break;
        }
      // process variable 'expressions' (may be anything delimited by braces)
      case 4: {
          if (c == '}') {
            if (bc == 1) {
              r = expF(r, b.str());
              b.str("");
              x = 0;
            } else {
              --bc;
              b << '}';
            }
          } else {
            if (c == '{') {
              ++bc;
            }
            b << c;
          }
          break;
        }
      }
    }

    if (x < 3) {
      r = constF(r, b.str());
    } else if (x == 3) {
      r = expF(r, b.str());
    }

    return r;
  }

// slurp the entire contents of an input stream into a string
inline std::string slurp(std::istream& in) {
  std::ostringstream ss;
  ss << in.rdbuf();
  return ss.str();
}

// generate?
inline seq strings() { return seq(); }
inline seq strings(const std::string& a0) { seq r; r.push_back(a0); return r; }
inline seq strings(const std::string& a0, const std::string& a1) { seq r; r.push_back(a0); r.push_back(a1); return r; }
inline seq strings(const std::string& a0, const std::string& a1, const std::string& a2) { seq r; r.push_back(a0); r.push_back(a1); r.push_back(a2); return r; }
inline seq strings(const std::string& a0, const std::string& a1, const std::string& a2, const std::string& a3) { seq r; r.push_back(a0); r.push_back(a1); r.push_back(a2); r.push_back(a3); return r; }
inline seq strings(const std::string& a0, const std::string& a1, const std::string& a2, const std::string& a3, const std::string& a4) { seq r; r.push_back(a0); r.push_back(a1); r.push_back(a2); r.push_back(a3); r.push_back(a4); return r; }
inline seq strings(const std::string& a0, const std::string& a1, const std::string& a2, const std::string& a3, const std::string& a4, const std::string& a5) { seq r; r.push_back(a0); r.push_back(a1); r.push_back(a2); r.push_back(a3); r.push_back(a4); r.push_back(a5); return r; }

// read a char definition like 'c' or 'd' or '\0'
char readCharDef(const std::string&);

// convenience functions for expanding environment variable references in strings and paths
std::string expandVars(const std::string&);
std::string expandPath(const std::string&); // same as 'expandVars' but expand '~' to home directory

// display a byte count in typical units
std::string showDataSize(size_t bytes);

// char set utilities
inline std::string charRange(char low, char high) {
  std::ostringstream ss;
  if (low < high) {
    for (char c = low; c <= high; ++c) {
      ss << c;
    }
  } else {
    for (char c = low; c >= high; --c) {
      ss << c;
    }
  }
  return ss.str();
}

inline std::string printableChars() {
  return charRange(0x20, 0x7e);
}

inline std::string difference(const std::string& x, const std::string& y) {
  return fromSet<std::string>(setDifference(toSet(x), toSet(y)));
}

// a set of strings represented as a prefix tree
class ptnode;

class prefix_tree {
public:
  prefix_tree(const seq&);
  prefix_tree(const set&);
  ~prefix_tree();

  std::map<size_t, seq> rankedMatches(const std::string&, size_t maxDist) const;
  seq closestMatches(const std::string&, size_t maxDist) const;
private:
  ptnode* root;
};

// how far is one string from another string?
size_t editDistance(const std::string&, const std::string&);

// find the strings out of a set that are at most the given edit distance away from an input string
seq closestMatches(const std::string&, const set&, size_t maxDist);
seq closestMatches(const std::string&, const seq&, size_t maxDist);

// ensure that a string has a given suffix (add it if it's not there)
std::string mustEndWith(const std::string&, const std::string&);

// get a set of filesystem objects matching a pattern
str::seq paths(const std::string& p);

}}

#endif
