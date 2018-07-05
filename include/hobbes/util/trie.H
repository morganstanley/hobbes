/********
 * trie : a basic prefix-tree data structure
 *******/

#ifndef HOBBES_UTIL_TRIE_HPP_INCLUDED
#define HOBBES_UTIL_TRIE_HPP_INCLUDED

#include <vector>
#include <hobbes/reflect.H>

namespace hobbes {

template <typename K, typename V, typename KMap>
  struct prefix_tree_node {
    typedef typename KMap::template map<prefix_tree_node<K, V, KMap>*>::type SubkeyMap;
    typedef variant<unit, V> MaybeV;

    ~prefix_tree_node() {
      for (typename SubkeyMap::const_iterator c = this->children.begin(); c != this->children.end(); ++c) {
        delete c->second;
      }
    }

    SubkeyMap children;
    MaybeV    value;

    void values(std::vector<V>* vs) const {
      if (const V* v = get<V>(this->value)) {
        vs->push_back(*v);
      }

      for (typename SubkeyMap::const_iterator c = this->children.begin(); c != this->children.end(); ++c) {
        c->second->values(vs);
      }
    }
  };

template <typename K, typename V, typename KMap>
  class prefix_tree {
  private:
    typedef prefix_tree_node<K, V, KMap> node_t;
    node_t* root;

    template <typename KIter>
      node_t* makeNode(KIter begin, KIter end) {
        node_t* r = this->root;
        for (KIter i = begin; i != end; ++i) {
          node_t*& nr = r->children[*i];
          if (!nr) {
            nr = new node_t;
          }
          r = nr;
        }
        return r;
      }

    template <typename KIter>
      const node_t* findNode(KIter begin, KIter end) const {
        const node_t* r = this->root;
        for (KIter i = begin; i != end; ++i) {
          typename node_t::SubkeyMap::const_iterator c = r->children.find(*i);
          if (c == r->children.end()) {
            return 0;
          } else {
            r = c->second;
          }
        }
        return r;
      }
  public:
    prefix_tree() : root(new node_t) {
    }

    ~prefix_tree() {
      delete this->root;
    }

    template <typename KIter>
      void insert(KIter begin, KIter end, const V& v) {
        makeNode(begin, end)->value = v;
      }

    template <typename KIter>
      V* lookup(KIter begin, KIter end) const {
        const node_t* n = findNode(begin, end);
        return n ? const_cast<V*>(get<V>(n->value)) : 0;
      }

    void insert(const std::vector<K>& k, const V& v) {
      insert(k.begin(), k.end(), v);
    }

    V* lookup(const std::vector<K>& k) const {
      return lookup(k.begin(), k.end());
    }

    typedef std::vector<V> ValueSeq;
    void values(ValueSeq* vs) const {
      this->root->values(vs);
    }

    ValueSeq values() const {
      ValueSeq vs;
      values(&vs);
      return vs;
    }

    // support incremental search
    typedef void* point_t;
    point_t rootPoint() const { return this->root; }

    point_t moveTo(const K& k, point_t base) const {
      const node_t* n = reinterpret_cast<node_t*>(base);
      typename node_t::SubkeyMap::const_iterator c = n->children.find(k);
      return (c == n->children.end()) ? 0 : c->second;
    }

    typedef std::vector<K> KeySeq;
    void keysAt(KeySeq* ks, point_t base) const {
      const node_t* n = reinterpret_cast<node_t*>(base);

      for (typename node_t::SubkeyMap::const_iterator c = n->children.begin(); c != n->children.end(); ++c) {
        ks->push_back(c->first);
      }
    }

    KeySeq keysAt(point_t base) const {
      KeySeq r;
      keysAt(base, &r);
      return r;
    }

    typedef std::pair<K, point_t> KeyPoint;
    typedef std::vector<KeyPoint> KeyPointSeq;
    void keyPointsAt(KeyPointSeq* kps, point_t base) const {
      const node_t* n = reinterpret_cast<node_t*>(base);

      for (typename node_t::SubkeyMap::const_iterator c = n->children.begin(); c != n->children.end(); ++c) {
        kps->push_back(KeyPoint(c->first, c->second));
      }
    }

    V* valueAt(point_t base) const {
      return get<V>(reinterpret_cast<node_t*>(base)->value);
    }
  };

}

#endif

