/****
 * unionfind : a data structure for efficiently performing unification for n terms in effectively O(n) time
 ****/

#ifndef HOBBES_UTIL_UNIONFIND_HPP_INCLUDED
#define HOBBES_UTIL_UNIONFIND_HPP_INCLUDED

#include <unordered_map>
#include <stdexcept>
#include <sstream>
#include <memory>

namespace hobbes {

template <typename K, typename V>
  struct eqsetmem {
    eqsetmem(const K& key, const V& value) : key(key), value(value), rank(0) {
      representative = this;
    }

    K               key;
    V               value;
    size_t          rank;
    eqsetmem<K, V>* representative;
  };

template <typename K, typename V, typename KVLift, typename VPlus>
  class equivalence_mapping {
  public:
    equivalence_mapping() : eqsz(0) {
    }

    // how many equivalence constraints have been recorded?
    size_t size() const {
      return this->eqsz;
    }

    // find the representative element for a set
    V& find(const K& k) {
      return findNode(k)->value;
    }

    // declare two values equal
    void join(const K& k0, const K& k1) {
      node_t* lhs = findNode(k0);
      node_t* rhs = findNode(k1);

      if (lhs->rank < rhs->rank) {
        lhs->representative = rhs;
        rhs->value = VPlus::apply(rhs->value, lhs->value);
      } else if (lhs->rank > rhs->rank) {
        rhs->representative = lhs;
        lhs->value = VPlus::apply(lhs->value, rhs->value);
      } else if (lhs == rhs) {
        // these are the same thing, don't pretend that we've added information
        return;
      } else {
        rhs->representative = lhs;
        lhs->rank += 1;
        lhs->value = VPlus::apply(lhs->value, rhs->value);
      }

      ++this->eqsz;
    }

    // merge another equivalence mapping with this one
    size_t merge(const equivalence_mapping<K, V, KVLift, VPlus>& rhs) {
      // count the number of extra bindings that we add
      size_t c = this->eqsz;

      // add new nodes to this set
      for (const auto& kn : rhs.nodes) {
        if (this->nodes.find(kn.first) == this->nodes.end()) {
          this->nodes[kn.first] = nodep(new node_t(kn.first, KVLift::apply(kn.first)));
        }
      }

      // now for these new nodes, apply the equivalence bindings from the input set
      for (const auto& kn : rhs.nodes) {
        node_t* rrep = findRepresentative(kn.second.get());
        if (kn.first != rrep->key) {
          join(kn.first, rrep->key);
        }
      }

      return this->eqsz - c;
    }

    // get the universe of values
    std::vector<K> values() const {
      std::vector<K> vs;
      vs.reserve(nodes.size());
      for (const auto& kn : this->nodes) {
        vs.push_back(kn.first);
      }
      return vs;
    }
  private:
    size_t eqsz;

    using node_t = eqsetmem<K, V>;
    using nodep = std::unique_ptr<node_t>; // allows multiple incremental extensions of unification sets
    using nodes_t = std::unordered_map<K, nodep>;
    nodes_t nodes;

    static node_t* findRepresentative(node_t* n) {
      // first find the root
      auto* root = n;
      while (root != root->representative) {
        root = root->representative;
      }

      // then update all nodes along the way (they all have the same representative)
      while (n != root) {
        auto* s = n->representative;
        n->representative = root;
        n = s;
      }
      return root;
    }

    node_t* findNode(const K& k) {
      typename nodes_t::const_iterator n = this->nodes.find(k);

      if (n != this->nodes.end()) {
        return findRepresentative(n->second.get());
      } else {
        auto* n = new node_t(k, KVLift::apply(k));
        this->nodes[k] = nodep(n);
        return n;
      }
    }
  };

}

#endif

