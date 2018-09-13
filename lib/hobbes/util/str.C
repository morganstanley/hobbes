
#include <hobbes/util/str.H>
#include <memory>
#include <wordexp.h>
#include <glob.h>

namespace hobbes { namespace str {

std::string env(const std::string& varname) {
  char* gv = getenv(varname.c_str());
  return gv ? std::string(gv) : std::string("");
}

void env(const std::string& varname, const std::string& value) {
  setenv(varname.c_str(), value.c_str(), 1);
}

void repeat(unsigned int n, const std::string& s, seq* out) {
  for (unsigned int i = 0; i < n; ++i) {
    out->push_back(s);
  }
}

seq repeat(unsigned int n, const std::string& s) {
  seq r;
  repeat(n, s, &r);
  return r;
}

unsigned int tableCols(const seqs& tbl) {
  return tbl.size();
}

unsigned int tableRows(const seqs& tbl) {
  return minSize(0, tbl);
}

unsigned int maxStrLen(const seq& col) {
  return maxSize(0, col);
}

lengths maxStrLen(const seqs& tbl) {
  lengths r;
  for (unsigned int c = 0; c < tbl.size(); ++c) {
    r.push_back(maxStrLen(tbl[c]));
  }
  return r;
}

std::string pad(size_t n) {
  return std::string(n, ' ');
}

std::string leftAlign(size_t w, const std::string& x) {
  if (x.size() >= w) {
    return x;
  } else {
    return x + pad(w - x.size());
  }
}

std::string rightAlign(size_t w, const std::string& x) {
  if (x.size() >= w) {
    return x;
  } else {
    return pad(w - x.size()) + x;
  }
}

seq leftAlign(const seq& col) {
  size_t w = maxStrLen(col);
  seq r;
  r.reserve(col.size());
  for (seq::const_iterator s = col.begin(); s != col.end(); ++s) {
    r.push_back(leftAlign(w, *s));
  }
  return r;
}

seq rightAlign(const seq& col) {
  size_t w = maxStrLen(col);
  seq r;
  r.reserve(col.size());
  for (seq::const_iterator s = col.begin(); s != col.end(); ++s) {
    r.push_back(rightAlign(w, *s));
  }
  return r;
}

seqs leftAlign(const seqs& tbl) {
  seqs r;
  r.reserve(tbl.size());
  for (seqs::const_iterator c = tbl.begin(); c != tbl.end(); ++c) {
    r.push_back(c == tbl.end()-1 ? *c : leftAlign(*c));
  }
  return r;
}

seqs rightAlign(const seqs& tbl) {
  seqs r;
  r.reserve(tbl.size());
  for (seqs::const_iterator c = tbl.begin(); c != tbl.end(); ++c) {
    r.push_back(rightAlign(*c));
  }
  return r;
}

bool validTable(const seqs& tbl) {
  if (tbl.size() == 0) {
    return false;
  } else {
    size_t rsz = tbl[0].size();
    for (size_t c = 1; c < tbl.size(); ++c) {
      if (tbl[c].size() < rsz) {
        return false;
      }
    }
    return true;
  }
}

void printAlignedTable(std::ostream& out, const seqs& tbl) {
  if (validTable(tbl)) {
    // draw the header
    for (seqs::const_iterator c = tbl.begin(); c != tbl.end(); ++c) {
      out << (*c)[0] << " ";
    }
    out << "\n";

    // draw the divider
    for (seqs::const_iterator c = tbl.begin(); c != tbl.end(); ++c) {
      auto N = (c < tbl.end() - 1 ? (*c)[0].size() : maxStrLen(*c));
      out << std::string(N, '-') << " ";
    }
    out << "\n";

    // draw the data
    for (size_t r = 1; r < tbl[0].size(); ++r) {
      for (seqs::const_iterator c = tbl.begin(); c != tbl.end(); ++c) {
        out << (*c)[r] << " ";
      }
      out << "\n";
    }
  }
}

void printLeftAlignedTable(std::ostream& out, const seqs& tbl) {
  printAlignedTable(out, leftAlign(tbl));
}

void printRightAlignedTable(std::ostream& out, const seqs& tbl) {
  printAlignedTable(out, rightAlign(tbl));
}

void printHeadlessAlignedTable(std::ostream& out, const seqs& tbl) {
  if (validTable(tbl)) {
    for (size_t r = 0; r < tbl[0].size(); ++r) {
      if (r > 0) out << "\n";
      for (seqs::const_iterator c = tbl.begin(); c != tbl.end(); ++c) {
        out << (*c)[r] << " ";
      }
    }
  }
}

void printHeadlessLeftAlignedTable(std::ostream& out, const seqs& tbl) {
  printHeadlessAlignedTable(out, leftAlign(tbl));
}

void printHeadlessRightAlignedTable(std::ostream& out, const seqs& tbl) {
  printHeadlessAlignedTable(out, rightAlign(tbl));
}

std::string showLeftAlignedTable(const seqs& tbl) {
  std::ostringstream ss;
  printAlignedTable(ss, leftAlign(tbl));
  return ss.str();
}

std::string showRightAlignedTable(const seqs& tbl) {
  std::ostringstream ss;
  printAlignedTable(ss, rightAlign(tbl));
  return ss.str();
}

std::string demangle(const char* tn) {
  if (tn == 0) {
    return "";
  }

  int   s   = 0;
  char* dmn = abi::__cxa_demangle(tn, 0, 0, &s);

  if (dmn == 0) {
    return std::string(tn);
  } else {
    std::string r(dmn);
    free(dmn);
    return r;
  }
}

std::string demangle(const std::type_info& ti) {
  return demangle(ti.name());
}

pair splitAt(const std::string& s, unsigned int i) {
  if (i >= s.size()) {
    return pair(s, "");
  } else {
    return pair(s.substr(0, i), s.substr(i, s.size()));
  }
}

pair lsplit(const std::string& s, const std::string& ss) {
  size_t p = s.find(ss);
  if (p == std::string::npos) {
    return pair(s, "");
  } else {
    return pair(s.substr(0, p), s.substr(p + ss.size(), s.size()));
  }
}

pair rsplit(const std::string& s, const std::string& ss) {
  size_t p = s.rfind(ss);
  if (p == std::string::npos) {
    return pair("", s);
  } else {
    return pair(s.substr(0, p), s.substr(p + ss.size(), s.size()));
  }
}

seq csplit(const std::string& str, const std::string& pivot) {
  typedef std::string::size_type size_type;

  seq       ret;
  size_type mp = 0;

  while (mp != std::string::npos) {
    size_type nmp = str.find(pivot, mp);

    if (nmp == std::string::npos) {
      ret.push_back(str.substr(mp, str.size() - mp));
      mp = nmp;
    } else {
      ret.push_back(str.substr(mp, nmp - mp));
      mp = nmp + pivot.size();
    }
  }

  return ret;
}

pair readWhile(bool (*P)(char), const std::string& s) {
  return splitAt(s, firstFailIndex(P, s));
}

bool isNyb(char c) {
  return (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || (c >= '0' && c <= '9');
}

char denyb(char c) {
  if (c >= 'a' && c <= 'f') {
    return 10 + (c - 'a');
  } else if (c >= 'A' && c <= 'F') {
    return 10 + (c - 'A');
  } else if (c >= '0' && c <= '9') {
    return (c - '0');
  } else {
    return 0;
  }
}

unsigned char dehex(const std::string& x) {
  if (x.size() == 4 && x[0] == '0' && (x[1] == 'x' || x[1] == 'X')) {
    return (denyb(x[2]) << 4) | denyb(x[3]);
  } else if (x.size() == 2) {
    return (denyb(x[0]) << 4) | denyb(x[1]);
  } else {
    return 0;
  }
}

std::vector<unsigned char> dehexs(const std::string& x) {
  std::vector<unsigned char> r;
  unsigned int i = (x.size() > 2 && x[0] == '0' && (x[1] == 'X' || x[1] == 'x')) ? 2 : 0;
  while (i < x.size()) {
    r.push_back((denyb(x[i]) << 4) | denyb(x[i+1]));
    i += 2;
  }
  return r;
}

char nyb(unsigned char x) {
  static const char cs[] = "0123456789abcdef";
  return cs[x % 16];
}

std::string hex(unsigned char x) {
  const char d[] = {nyb(x >> 4), nyb(x & 0x0F), '\0'};
  return std::string(d);
}

std::string hex(const std::vector<unsigned char>& cs) {
  std::ostringstream ss;
  ss << "0x";
  for (std::vector<unsigned char>::const_iterator c = cs.begin(); c != cs.end(); ++c) {
    ss << hex(*c);
  }
  return ss.str();
}

std::string hex(const unsigned char* b, size_t sz) {
  const unsigned char* e = b + sz;

  std::ostringstream ss;
  ss << "0x";
  for (const unsigned char* c = b; c != e; ++c) {
    ss << hex(*c);
  }
  return ss.str();
}


std::string escape(const std::string& cs) {
  std::ostringstream result;
  for (auto c : cs) {
    switch (c) {
    case '"':
      result << "\\\"";
      break;
    case '\'':
      result << "\\'";
      break;
    case '\n':
      result << "\\n";
      break;
    case '\t':
      result << "\\t";
      break;
    case '\r':
      result << "\\r";
      break;
    case '\\':
      result << "\\\\";
      break;
    case '\0':
      result << "\\0";
      break;
    default:
      result.put(c);
      break;
    }
  }
  return result.str();
}

std::string unescape(const std::string& cs) {
  std::ostringstream result;
  bool esc = false;
  for (auto c = cs.begin(); c != cs.end(); ++c) {
    if (*c == '\\' && !esc) {
      esc = true;
    } else if (esc) {
      switch (*c) {
      case 'n':
        result << "\n";
        break;
      case 't':
        result << "\t";
        break;
      case 'r':
        result << "\r";
        break;
      case '0':
        result.put('\0');
        break;
      case 'x': {
          char b1 = 0; char b2 = 0;
          ++c; if (c != cs.end()) b1 = *c;
          ++c; if (c != cs.end()) b2 = *c;
          result.put((denyb(b1) << 4) + denyb(b2));
          break;
        }
      default:
        result.put(*c);
      }
      esc = false;
    } else {
      result.put(*c);
    }
  }
  return result.str();
}

bool endsWith(const std::string& s, const std::string& sfx) {
  return s.size() >= sfx.size() && s.substr(s.size() - sfx.size(), s.size()) == sfx;
}

std::string show(const set& ss) {
  if (ss.size() == 0) {
    return "{}";
  } else {
    std::ostringstream r;
    r << "{";
    set::const_iterator s = ss.begin();
    r << *s;
    ++s;
    while (s != ss.end()) {
      r << ", ";
      r << *s;
      ++s;
    }
    r << "}";
    return r.str();
  }
}

std::string show(const named_strings& cfg) {
  if (cfg.size() == 0) {
    return "{}";
  } else {
    std::ostringstream r;
    r << "{";
    named_strings::const_iterator f = cfg.begin();
    r << f->first << "=" << f->second;
    ++f;
    while (f != cfg.end()) {
      r << ", ";
      r << f->first << "=" << f->second;
      ++f;
    }
    r << "}";
    return r.str();
  }
}

char readCharDef(const std::string& x) {
  if (x.size() == 0) {
    return 0;
  } else if (x[0] != '\'') {
    return x[0]; // ?
  } else if (x.size() > 2 && x[1] == '\\') {
    switch (x[2]) {
    case '0':  return 0;
    case 'r':  return '\r';
    case 'n':  return '\n';
    case 't':  return '\t';
    case '\\': return '\\';
    default:   return x[2];
    }
  } else {
    return x[1];
  }
}

bool charIn(char c, const char* ps) {
  while (*ps != '\0') {
    if (c == *ps) {
      return true;
    }
    ++ps;
  }
  return false;
}

bool isDigit(char c) {
  return std::isdigit(c) != 0;
}

bool isNotDigit(char c) {
  return !isDigit(c);
}

unsigned int firstFailIndex(bool (*P)(char), const std::string& s) {
  unsigned int i = 0;
  while (i < s.size()) {
    if (!P(s[i])) {
      break;
    }
    ++i;
  }
  return i;
}

std::string cdelim(const seq& ss, const std::string& d) {
  if (ss.size() == 0) {
    return "";
  } else {
    std::ostringstream r;
    r << ss[0];
    for (unsigned int s = 1; s < ss.size(); ++s) {
      r << d << ss[s];
    }
    return r.str();
  }
}

// read and expand environment variable references in strings and paths
std::string epAppend(const std::string& lhs, const std::string& rhs) { return lhs + rhs; }
std::string epLookup(const std::string& lhs, const std::string& var) { return lhs + env(var); }

std::string expandVars(const std::string& x) {
  return
    foldWithFormat(
      x,
      std::string(),
      &epAppend,
      &epLookup
    );
}

std::string expandPath(const std::string& x) {
  wordexp_t we;
  if (wordexp(x.c_str(), &we, 0) == 0) {
    std::string result(we.we_wordv[0]);
    wordfree(&we);
    return result;
  } else {
    return x;
  }
}

// display a byte count in typical units
std::string showDataSize(size_t bytes) {
  size_t kilos = 1024;
  size_t megas = kilos * kilos;
  size_t gigas = kilos * megas;
  size_t teras = kilos * gigas;

  if ((bytes / teras) > 0) {
    return from(static_cast<double>(bytes) / static_cast<double>(teras)) + "TB";
  } else if ((bytes / gigas) > 0) {
    return from(static_cast<double>(bytes) / static_cast<double>(gigas)) + "GB";
  } else if ((bytes / megas) > 0) {
    return from(static_cast<double>(bytes) / static_cast<double>(megas)) + "MB";
  } else if ((bytes / kilos) > 0) {
    return from(static_cast<double>(bytes) / static_cast<double>(kilos)) + "KB";
  } else {
    return from(bytes) + "B";
  }
}

/******
 * represent a set of strings as a tree of common prefixes
 ******/
class ptnode {
public:
  ptnode() : terminal(false) {
  }

  void insert(const std::string& x) {
    ptnode* n = this;
    for (char c : x) {
      auto child = n->children.find(c);
      if (child != n->children.end()) {
        n = child->second.get();
      } else {
        ptnode* nc = new ptnode();
        n->children[c] = std::unique_ptr<ptnode>(nc);
        n = nc;
      }
    }
    n->terminal = true;
  }

  std::map<size_t, seq> rankedMatches(const std::string& x, size_t maxDist) const {
    std::map<size_t, seq> r;
    std::set<const ptnode*> inserted;
    findRankedMatches("", x, 0, 0, maxDist, &r, &inserted);
    return r;
  }

  seq closestMatches(const std::string& x, size_t maxDist) const {
    std::map<size_t, seq> rms = rankedMatches(x, maxDist);

    seq r;
    for (const auto& rm : rms) {
      r.insert(r.end(), rm.second.begin(), rm.second.end());
    }
    return r;
  }
private:
  typedef std::map<char, std::unique_ptr<ptnode>> Children;
  Children children;

  // does this node complete a valid word?
  bool terminal;

  // search for sub-words within the maximum edit distance of an input string
  void findRankedMatches(const std::string& pfx, const std::string& x, size_t xi, size_t edits, size_t maxDist, std::map<size_t, seq>* r, std::set<const ptnode*>* inserted) const {
    if (edits >= maxDist) return;

    // if we've reached a complete string, perhaps add it to the result set
    if (this->terminal) {
      size_t tedits = edits + (x.size() - xi);
      if (tedits < maxDist) {
        if (inserted->find(this) == inserted->end()) {
          (*r)[tedits].push_back(pfx);
          inserted->insert(this);
        }
      }
    }

    // try to modify input chars or insert valid child chars
    for (const auto& child : this->children) {
      std::string   pfxC = pfx + std::string(1, child.first);
      const ptnode* c    = child.second.get();

      if (xi < x.size()) {
        if (x[xi] == child.first) {
          // matched, no edit necessary
          c->findRankedMatches(
            pfxC,
            x,
            xi+1,
            edits,
            maxDist,
            r,
            inserted
          );
        } else {
          // change the current char
          c->findRankedMatches(
            pfxC,
            x,
            xi+1,
            edits + 1,
            maxDist,
            r,
            inserted
          );
        }
      }

      // insert a character
      c->findRankedMatches(
        pfxC,
        x,
        xi,
        edits + 1,
        maxDist,
        r,
        inserted
      );
    }

    // also try to delete an input char
    findRankedMatches(pfx, x, xi+1, edits+1, maxDist, r, inserted);
  }
};

prefix_tree::prefix_tree(const seq& xs) : root(new ptnode()) {
  for (const auto& x : xs) {
    this->root->insert(x);
  }
}

prefix_tree::prefix_tree(const set& xs) : root(new ptnode()) {
  for (const auto& x : xs) {
    this->root->insert(x);
  }
}

prefix_tree::~prefix_tree() {
  delete this->root;
}

std::map<size_t, seq> prefix_tree::rankedMatches(const std::string& x, size_t maxDist) const {
  return this->root->rankedMatches(x, maxDist);
}

seq prefix_tree::closestMatches(const std::string& x, size_t maxDist) const {
  return this->root->closestMatches(x, maxDist);
}

// how far is one string from another string?
size_t editDistance(const std::string& x, const std::string& y) {
  std::map<size_t, seq> r = prefix_tree(strings(y)).rankedMatches(x, std::max<size_t>(x.size(), y.size()));

  if (r.size() > 0) {
    return r.begin()->first;
  } else {
    return std::max<size_t>(x.size(), y.size()); // seems unlikely ...
  }
}

// find the strings out of a set that are at most the given edit distance away from an input string
// (note: this is just a quick and dirty method ... todo: use the obvious prefix tree search)
seq closestMatches(const std::string& x, const set& ss, size_t maxDist) {
  return prefix_tree(ss).closestMatches(x, maxDist);
}

seq closestMatches(const std::string& x, const seq& ss, size_t maxDist) {
  return prefix_tree(ss).closestMatches(x, maxDist);
}

// ensure that a string has a given suffix (add it if it's not there)
std::string mustEndWith(const std::string& x, const std::string& sfx) {
  if (x.substr(x.size() - sfx.size()) == sfx) {
    return x;
  } else {
    return x + sfx;
  }
}

// get a set of filesystem objects matching a pattern
str::seq paths(const std::string& p) {
  glob_t g;
  if (glob(p.c_str(), 0, 0, &g) != 0) {
    return str::seq();
  } else if (g.gl_pathc == 0) {
    globfree(&g);
    return str::seq();
  } else {
    str::seq r;
    for (size_t i = 0; i < g.gl_pathc; ++i) {
      r.push_back(g.gl_pathv[i]);
    }
    globfree(&g);
    return r;
  }
}

}}

