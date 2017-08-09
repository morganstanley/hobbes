/*****************
 * rmap : map key ranges to values
 *****************/

#ifndef HOBBES_UTIL_RMAP_HPP_INCLUDED
#define HOBBES_UTIL_RMAP_HPP_INCLUDED

#include <vector>
#include <set>
#include <map>
#include <functional>
#include <iostream>

namespace hobbes {

template <typename K, typename V, typename Ord>
  class range_map {
  public:
    typedef std::pair<K, K> range;
    typedef std::vector<range> ranges;

    // show the state of this range map
    void show(std::ostream& out) const {
      out << "{";
      if (this->rs.size() != this->vs.size()) {
        out << "INVALID (" << this->rs.size() << " != " << this->vs.size() << ")";
      } else if (this->rs.size() > 0) {
        out << "'" << this->rs[0].first << "-" << this->rs[0].second << "' => " << this->vs[0];
        for (size_t i = 1; i < this->rs.size(); ++i) {
          out << ", '" << this->rs[i].first << "-" << this->rs[i].second << "' => " << this->vs[i];
        }
      }
      out << "}";
    }

    // find the value V such that (k0,k1)->V in this and k0 <= k <= k1
    const V* lookup(const K& k) const {
      size_t i = find(k);
      if (i < this->vs.size()) {
        return &this->vs[i];
      } else {
        return 0;
      }
    }

    // find the value V such that (k0,k1)->V in this and k0 <= kr.0 <= kr.1 <= k1
    const V* lookupRangeSubset(const range& kr) const {
      size_t i = find(kr.first);
      size_t j = find(kr.second);

      if (i == j && i < this->vs.size()) {
        return &this->vs[i];
      } else {
        return 0;
      }
    }

    // insert a range->value mapping
    // overwrite any mappings intersected
    void insert(const range& rr, const V& v) {
      for (size_t i = 0; i < this->rs.size(); ++i) {
        range& lr = this->rs[i];

        switch (classifyIntersection(lr, rr)) {
        case LLRR:
          continue;
        case RRLL:
          insAt(i, rr, v);
          assert(rangesValid());
          return;
        case EE:
          this->vs[i] = v;
          assert(rangesValid());
          return;
        case ELR:
          this->vs[i] = v;
          lr.second = rr.second;
          deleteFrom(i+1, rr);
          assert(rangesValid());
          return;
        case ERL:
          lr.first = Ord::succ(rr.second); //+1
          insAt(i, rr, v);
          assert(rangesValid());
          return;
        case LRE:
          lr.second = Ord::pred(rr.first); //-1
          insAt(i+1, rr, v);
          assert(rangesValid());
          return;
        case RLE:
          lr.first = rr.first;
          this->vs[i] = v;
          assert(rangesValid());
          return;
        case LRRL: {
          range er(Ord::succ(rr.second), lr.second);
          lr.second = Ord::pred(rr.first);

          insAt(i+1, rr, v);
          insAt(i+2, er, this->vs[i]);
          assert(rangesValid());
          return;
        }
        case RLLR:
          lr.first = rr.first;
          lr.second = rr.second;
          this->vs[i] = v;

          deleteFrom(i+1, rr);
          assert(rangesValid());
          return;
        case LRLR:
          lr.second = Ord::pred(rr.first);
          deleteFrom(i+1,rr);
          insAt(i+1, rr, v);
          assert(rangesValid());
          return;
        case RLRL:
          lr.first = Ord::succ(rr.second);
          insAt(i, rr, v);
          assert(rangesValid());
          return;
        }
      }

      // if we get here, the range doesn't intersect anything
      // it must go at the end
      this->rs.push_back(rr);
      this->vs.push_back(v);
    }

    void insert(const K& k0, const K& k1, const V& v) {
      insert(range(k0,k1),v);
    }

    void mergeRange(range rr, const std::function<void(V&)>& f) {
      for (size_t i = 0; i < this->rs.size(); ++i) {
        range& lr = this->rs[i];

        switch (classifyIntersection(lr, rr)) {
        case LLRR:
          continue;
        case RRLL:
          insAt(i, rr, V());
          f(this->vs[i]);
          assert(rangesValid());
          return;
        case EE:
          f(this->vs[i]);
          assert(rangesValid());
          return;
        case ELR:
          f(this->vs[i]);
          rr.first=Ord::succ(lr.second);
          break;
        case ERL: {
          range er(Ord::succ(rr.second), lr.second);
          V     ev = this->vs[i];

          lr.second = rr.second;
          f(this->vs[i]);
          insAt(i+1, er, ev);
          assert(rangesValid());
          return;
        }
        case LRE:
          lr.second = Ord::pred(rr.first);
          insAt(i+1, rr, this->vs[i]);
          f(this->vs[i+1]);
          assert(rangesValid());
          return;
        case RLE:
          insAt(i, range(rr.first, Ord::pred(lr.first)), V());
          f(this->vs[i]);
          f(this->vs[i+1]);
          assert(rangesValid());
          return;
        case LRRL: {
          range er(Ord::succ(rr.second), lr.second);
          lr.second = Ord::pred(rr.first);

          insAt(i+1, rr, this->vs[i]);
          f(this->vs[i+1]);

          insAt(i+2, er, this->vs[i]);
          assert(rangesValid());
          return;
        }
        case RLLR: {
          range nr(Ord::succ(lr.second), rr.second);
          insAt(i, range(rr.first, Ord::pred(lr.first)), V());
          f(this->vs[i]);
          f(this->vs[i+1]);
          rr = nr;
          break;
        }
        case LRLR: {
          range mr(rr.first, lr.second);
          range er(Ord::succ(lr.second), rr.second);
          lr.second = Ord::pred(rr.first);
          insAt(i+1, mr, this->vs[i]);
          f(this->vs[i+1]);
          rr = er;
          break;
        }
        case RLRL: {
          range br(rr.first, Ord::pred(lr.first));
          range er(Ord::succ(rr.second), lr.second);
          V     ev = this->vs[i];

          lr.second = rr.second;
          f(this->vs[i]);

          insAt(i, br, V());
          f(this->vs[i]);
          
          insAt(i+2, er, ev);
          assert(rangesValid());
          return;
        }}
      }

      // if we get here, the range doesn't intersect anything
      // it must go at the end
      this->rs.push_back(rr);
      this->vs.resize(this->vs.size()+1);
      f(this->vs.back());
      assert(rangesValid());
    }

    void mergeRange(const K& k0, const K& k1, const std::function<void(V&)>& f) {
      mergeRange(range(k0,k1),f);
    }

    void keys(std::set<K>* ks) const {
      for (const auto& r : this->rs) {
        Ord::copyRange(r.first, r.second, ks);
      }
    }

    typedef std::vector<std::pair<range, V>> Mapping;
    Mapping mapping() const {
      Mapping r;
      for (size_t i = 0; i < this->rs.size(); ++i) {
        r.push_back(std::make_pair(this->rs[i], this->vs[i]));
      }
      return r;
    }

    ranges disjointRanges(const ranges& trs) const {
      ranges lrs = this->rs;
      ranges rrs = trs;

      ranges r;
      size_t i = 0, j = 0;
      while (i < lrs.size() && j < rrs.size()) {
        range& lr = lrs[i];
        range& rr = rrs[j];

        switch (classifyIntersection(lr, rr)) {
        case LLRR:
          r.push_back(lr);
          ++i;
          break;
        case RRLL:
          r.push_back(rr);
          ++j;
          break;
        case EE:
          r.push_back(lr);
          ++i;
          ++j;
          break;
        case ELR:
          r.push_back(lr);
          ++i;
          rr.first = Ord::succ(lr.second);
          break;
        case ERL:
          r.push_back(rr);
          ++j;
          lr.first = Ord::succ(rr.second);
          break;
        case LRE:
          r.push_back(range(lr.first, Ord::pred(rr.first)));
          r.push_back(range(rr.first, rr.second));
          ++i;
          ++j;
          break;
        case RLE:
          r.push_back(range(rr.first, Ord::pred(lr.first)));
          r.push_back(range(lr.first, lr.second));
          ++i;
          ++j;
          break;
        case LRRL:
          r.push_back(range(lr.first, Ord::pred(rr.first)));
          r.push_back(range(rr.first, rr.second));
          ++j;
          lr.first=Ord::succ(rr.second);
          break;
        case RLLR:
          r.push_back(range(rr.first, Ord::pred(lr.first)));
          r.push_back(range(lr.first, lr.second));
          ++i;
          rr.first=Ord::succ(lr.second);
          break;
        case LRLR:
          r.push_back(range(lr.first, Ord::pred(rr.first)));
          if (rr.first != lr.second) {
            r.push_back(range(rr.first, Ord::pred(lr.second)));
          }
          ++i;
          rr.first = lr.second;
          break;
        case RLRL:
          r.push_back(range(rr.first, Ord::pred(lr.first)));
          if (lr.first != rr.second) {
            r.push_back(range(lr.first, Ord::pred(rr.second)));
          }
          ++j;
          lr.first = rr.second;
          break;
        }
      }

      // copy any remaining ranges
      for (; i < lrs.size(); ++i) r.push_back(lrs[i]);
      for (; j < rrs.size(); ++j) r.push_back(rrs[j]);

      assert(rangesValid(r));
      return r;
    }

    // we can merge contiguous ranges with equivalent map values
    void compact() {
      if (this->rs.size() >= 2) {
        size_t i = 0;
        while (i < this->rs.size()-1) {
          range& tr = this->rs[i];
          range& nr = this->rs[i+1];

          if (tr.second == Ord::pred(nr.first) && this->vs[i] == this->vs[i+1]) {
            tr.second = nr.second;
            this->rs.erase(this->rs.begin()+i+1);
            this->vs.erase(this->vs.begin()+i+1);
          } else {
            ++i;
          }
        }
      }
      assert(rangesValid());
    }
  private:
    typedef std::vector<V> values;
    ranges rs;
    values vs;

    static bool rangesValid(const ranges& rs) {
      for (const auto& r : rs) {
        if (r.first != r.second && !(Ord::lt(r.first, r.second))) {
          return false;
        }
      }
      return true;
    }
    bool rangesValid() const { return rangesValid(this->rs); }

    size_t find(const K& k) const {
      for (size_t i = 0; i < this->rs.size(); ++i) {
        const range& r = this->rs[i];

        if (k == r.first || k == r.second || (Ord::lt(r.first, k) && Ord::lt(k, r.second))) {
          return i;
        } else if (Ord::lt(k, r.first)) {
          break;
        }
      }
      return this->vs.size();
    }

    void insAt(size_t i, const range& r, const V& v) {
      this->rs.insert(this->rs.begin() + i, r);
      this->vs.insert(this->vs.begin() + i, v);
    }

    // there are 11 ways that two ranges (L and R) can be classified for intersection
    // (with the first L meaning first range point for L and second L meaning second range point for L, equiv for R)
    // (E means both L and R are identical)
    enum RangeIntersection {
      LLRR, // disjoint (left first)
      RRLL, // disjoint (right first)

      EE,   // exact overlap
      ELR,  // same start, different ends (right subsumes)
      ERL,  // same start, different ends (left subsumes)
      LRE,  // same end, different starts (left subsumes)
      RLE,  // same end, different starts (right subsumes)

      LRRL, // total overlap (left subsumes)
      RLLR, // total overlap (right subsumes)

      LRLR, // partial overlap (left first)
      RLRL  // partial overlap (right first)
    };

    static RangeIntersection classifyIntersection(const range& lr, const range& rr) {
      if (Ord::lt(lr.second, rr.first)) {
        return LLRR;
      } else if (Ord::lt(rr.second, lr.first)) {
        return RRLL;
      } else if (lr.first == rr.first) {
        if (lr.second == rr.second) {
          return EE;
        } else if (Ord::lt(lr.second, rr.second)) {
          return ELR;
        } else {
          return ERL;
        }
      } else if (lr.second == rr.second) {
        if (Ord::lt(lr.first, rr.first)) {
          return LRE;
        } else {
          return RLE;
        }
      } else if (Ord::lt(lr.first, rr.first) && Ord::lt(rr.second, lr.second)) {
        return LRRL;
      } else if (Ord::lt(rr.first, lr.first) && Ord::lt(lr.second, rr.second)) {
        return RLLR;
      } else if (Ord::lt(rr.first, lr.first) && Ord::lt(rr.second, lr.second)) {
        return RLRL;
      } else {
        return LRLR;
      }
    }

    // truncate/delete ranges clipped by a given range (necessary when inserting large ranges)
    void deleteFrom(size_t i, const range& rr) {
      while (i < this->rs.size()) {
        range& lr = this->rs[i];

        switch (classifyIntersection(lr, rr)) {
        case LLRR:
          ++i;
          break; // ???
        case RRLL:
          return;
        case EE:
          this->rs.erase(this->rs.begin()+i);
          this->vs.erase(this->vs.begin()+i);
          return;
        case ELR:
          this->rs.erase(this->rs.begin()+i);
          this->vs.erase(this->vs.begin()+i);
          break;
        case ERL:
          lr.first = Ord::succ(rr.second);
          return;
        case LRE:
          lr.second = Ord::pred(rr.first);
          return;
        case RLE:
          this->rs.erase(this->rs.begin()+i);
          this->vs.erase(this->vs.begin()+i);
          return;
        case LRRL: {
          range er(Ord::succ(rr.second), lr.second);
          lr.second = Ord::pred(rr.first);
          this->rs.insert(this->rs.begin()+i+1, er);
          this->vs.insert(this->vs.begin()+i+1, this->vs[i]);
          return;
        }
        case RLLR:
          this->rs.erase(this->rs.begin()+i);
          this->vs.erase(this->vs.begin()+i);
          break;
        case LRLR:
          lr.second = Ord::pred(rr.first);
          ++i;
          break;
        case RLRL:
          lr.first = Ord::succ(rr.second);
          return;
        }
      }
    }
  };
}

#endif

