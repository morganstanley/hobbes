
#include <hobbes/parse/data.H>
#include <algorithm>
#include <stdexcept>

namespace hobbes {

const bool unit = true;

// map char sequence positions to line numbers
linedb::linedb(nat stype, const std::string& sdesc) : stype(stype), sdesc(sdesc), c(0) {
}

nat linedb::sourceType() const {
  return this->stype;
}

const std::string& linedb::sourceDesc() const {
  return this->sdesc;
}

void linedb::step(char x) {
  if (x == '\n') {
    this->lp.push_back(this->c);
  }
  ++this->c;
}

void linedb::reset() {
  this->c = 0;
  this->lp.clear();
}

linedb::LineCol linedb::pos(nat i) const {
  nats::const_iterator np = std::lower_bound(this->lp.begin(), this->lp.end(), i);
  nat k = std::distance(this->lp.begin(), np);
  
  if (k == 0) {
    return LineCol(1, i + 1);
  } else if (np == this->lp.end()) {
    return LineCol(this->lp.size() + 1, i - this->lp[this->lp.size() - 1]);
  } else {
    return LineCol(1 + k, i - *(np - 1));
  }
}

// load a set of lines around an implicated set from the specified line database
ldblines load(const linedb::ptr& ldb, const linedb::LineCol& i, const linedb::LineCol&) {
  if (ldb->sourceType() != 0) {
    throw std::runtime_error("I don't know how to load that source text type");
  }

  std::istringstream ss(ldb->sourceDesc());

  ldblines r;
  nat li = 1;;
  std::string ln;
  do {
    std::getline(ss, ln);

    if (li == i.first) {
      r.first.push_back(li);
      r.second.push_back(ln);
      break;
    }

    ++li;
  } while(ss);
  return r;
}

}

