
#ifndef HOBBES_LANG_TYPEMAP_HPP_INCLUDED
#define HOBBES_LANG_TYPEMAP_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/util/trie.H>
#include <hobbes/lang/typeinf.H>
#include <map>

namespace hobbes {

struct MonoTypeLT {
  bool operator()(const MonoTypePtr&, const MonoTypePtr&) const;
};

struct MTyMap {
  template <typename V>
    struct map {
      typedef std::map<MonoTypePtr, V, MonoTypeLT> type;
    };
};

// used to do exact matches on types with a wildcard
typedef variant<unit, MonoTypePtr> MaybePathPoint;
typedef std::vector<MaybePathPoint> MaybePathPoints;

// a map from type sequences to some value
template <typename V>
  class type_map {
  public:
    void insert(const MonoTypes& mts, const V& v) {
      this->d.insert(mts.begin(), mts.end(), v);
    }

    V* lookup(const MonoTypes& mts) const {
      return this->d.lookup(mts.begin(), mts.end());
    }

    std::vector<V> values() const {
      return this->d.values();
    }

    // determines whether at least one type is unifiable with the given type
    bool hasMatch(const TEnvPtr& tenv, const MonoTypes& mts) const {
      MonoTypeUnifier u(tenv);
      return hasMatch(u, this->d.rootPoint(), mts, 0);
    }

    // matches on unifiable (but not necessarily equal) types
    void matches(const TEnvPtr& tenv, const MonoTypes& mts, std::vector<V>* out) const {
      MonoTypeUnifier u(tenv);
      findMatches(u, this->d.rootPoint(), mts, 0, out);
    }

    // matches on bidirectionally unifiable types
    void bidimatches(const TEnvPtr& tenv, const MonoTypes& mts, std::vector<V>* out) const {
      MonoTypeUnifier u(tenv);
      findBidiMatches(u, this->d.rootPoint(), mts, 0, out);
    }

    // exact matches on types with a wildcard
    void find(const MaybePathPoints& path, std::vector<V>* out) const {
      findMatches(this->d.rootPoint(), path, 0, out);
    }
  private:
    typedef prefix_tree<MonoTypePtr, V, MTyMap> tmdata;
    tmdata d;

    bool hasMatch(MonoTypeUnifier& u, typename tmdata::point_t p, const MonoTypes& mts, size_t i) const {
      if (p == 0) {
        return false;
      }

      if (i == mts.size()) {
        return true;
      } else {
        const MonoTypePtr& mty = u.substitute(mts[i]);

        if (!hasFreeVariables(mty)) {
          return hasMatch(u, this->d.moveTo(mty, p), mts, i + 1);
        } else {
          typename tmdata::KeyPointSeq kps;
          this->d.keyPointsAt(&kps, p);
 
          for (typename tmdata::KeyPointSeq::const_iterator kp = kps.begin(); kp != kps.end(); ++kp) {
            try {
              MonoTypeUnifier pu = u;
              mgu(mty, kp->first, &pu);
              if (hasMatch(pu, kp->second, mts, i + 1)) return true;
            } catch (std::exception& ex) {
            }
          }
        }
      }
      return false;
    }

    void findMatches(MonoTypeUnifier& u, typename tmdata::point_t p, const MonoTypes& mts, size_t i, std::vector<V>* out) const {
      if (p == 0) return;

      if (i == mts.size()) {
        if (const V* v = this->d.valueAt(p)) {
          out->push_back(*v);
        }
      } else {
        const MonoTypePtr& mty = u.substitute(mts[i]);

        if (!hasFreeVariables(mty)) {
          findMatches(u, this->d.moveTo(mty, p), mts, i + 1, out);
        } else {
          typename tmdata::KeyPointSeq kps;
          this->d.keyPointsAt(&kps, p);
  
          for (typename tmdata::KeyPointSeq::const_iterator kp = kps.begin(); kp != kps.end(); ++kp) {
            try {
              MonoTypeUnifier pu = u;
              mgu(mty, kp->first, &pu);
              findMatches(pu, kp->second, mts, i + 1, out);
            } catch (std::exception& ex) {
            }
          }
        }
      }
    }

    void findBidiMatches(MonoTypeUnifier& u, typename tmdata::point_t p, const MonoTypes& mts, size_t i, std::vector<V>* out) const {
      if (p == 0) return;

      if (i == mts.size()) {
        if (const V* v = this->d.valueAt(p)) {
          out->push_back(*v);
        }
      } else {
        const MonoTypePtr& mty = u.substitute(mts[i]);

        typename tmdata::KeyPointSeq kps;
        this->d.keyPointsAt(&kps, p);
  
        for (typename tmdata::KeyPointSeq::const_iterator kp = kps.begin(); kp != kps.end(); ++kp) {
          try {
            MonoTypeUnifier pu = u;
            mgu(mty, u.substitute(kp->first), &pu);
            findBidiMatches(pu, kp->second, mts, i + 1, out);
          } catch (std::exception& ex) {
          }
        }
      }
    }

    void findMatches(typename tmdata::point_t p, const MaybePathPoints& path, size_t i, std::vector<V>* out) const {
      if (p == 0) return;

      if (i == path.size()) {
        if (const V* v = this->d.valueAt(p)) {
          out->push_back(*v);
        }
      } else {
        // do we need an exact match or any point here?
        if (const MonoTypePtr* mt = get<MonoTypePtr>(path[i])) {
          findMatches(this->d.moveTo(*mt, p), path, i + 1, out);
        } else {
          typename tmdata::KeyPointSeq kps;
          this->d.keyPointsAt(&kps, p);

          for (auto kp : kps) {
            findMatches(kp.second, path, i + 1, out);
          }
        }
      }
    }
  };

}

#endif

