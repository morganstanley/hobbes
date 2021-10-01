/*
 * slmap : a skip-list map structure safe for concurrent reading
 *
 *    to place an slmap in a file:
 *      fregion::writer w(...);
 *      slmap<int, double> m("foo", w); // place an slmap as a root variable named "foo"
 *
 *    to add a key/value:
 *      m.insert(k, v);
 *
 *    iterate over map contents:
 *      for (const auto& kv : m) {
 *        F(kv.first, kv.second);
 *      }
 *
 *    lookup a specific key:
 *      auto i = m.find(k);
 *      if (i == m.end()) {
 *        // not found
 *      } else {
 *        // found, use *i, i->first, i->second
 *      }
 *
 *    the structure will be placed with a type description like:
 *      data slmap k v = {count:long, root=^x.{next:(carray x@? 16), key:k, value:v}}
 */

#ifndef HOBBES_SLMAP_H_INCLUDED
#define HOBBES_SLMAP_H_INCLUDED

#include "fregion.H"
#include <stack>

namespace hobbes {

template <typename K, typename V>
  struct slnode {
    using level = uint16_t;
    static const size_t maxLevels = 8*sizeof(level);

    // a fileref pointer to a skip-list node
    using noderef = fregion::fileref<slnode<K, V>>;

    // the tower of 'next' pointers from this node
    // (0 is the "slow lane" with nothing skipped, then "faster" skips going further up)
    carray<noderef, maxLevels> next;

    // the key:value entry for this node
    K key;
    V value;

    // the type description for a skip-list node
    static ty::desc recTy(const ty::desc& elem, const ty::desc& k, const ty::desc& v) {
      return
        ty::rec(
          "next",
            -1,
            ty::app(prim("carray", ty::fn("t", "c", ty::rec("avail", 0, ty::prim("long"), "buffer", sizeof(size_t), ty::array(ty::var("t"), ty::var("c"))))), ty::fileRef(elem), ty::nat(maxLevels)),
          "key",
            -1,
            k,
          "value",
            -1,
            v
        );
    }
    static ty::desc type(const ty::desc& k = fregion::store<K>::storeType(), const ty::desc& v = fregion::store<V>::storeType()) {
      return recTy(ty::recursive("x", recTy(ty::var("x"), k, v)), k, v);
    }

    // allocate a skip-list node out of a file
    static noderef allocNode(fregion::imagefile* f) {
      return noderef(fregion::findSpace(f, fregion::pagetype::data, sizeof(slnode<K,V>), sizeof(size_t)));
    }

    // get the next node at the given level
    // (this assumes that we have a valid link at this level)
    slnode<K,V>* loadNext(fregion::imagefile* f, size_t level) const {
      if (this->next[level].index == 0) {
        return 0;
      } else {
        return this->next[level].load(f);
      }
    }

    // find the list node after this node with the greatest key <= the key we're looking for
    slnode<K,V>* findNextGLEB(fregion::imagefile* f, size_t level, const K& k, const std::function<void(slnode<K,V>*)>& downshiftF) {
      auto* n = this;
      while (n) {
        auto* sn = n->loadNext(f, level);
        if (!sn || k < sn->key) {
          // we've overshot
          if (level > 0) {
            // slow down by moving to the next lower level
            downshiftF(n);
            --level;
          } else {
            // we're at the lowest level,
            // previous node must be the last <= our key
            break;
          }
        } else if (sn->key <= k) {
          // haven't passed our key yet, keep going
          n = sn;
        } else {
          break;
        }
      }
      return n;
    }

    // find the list node after this node with the key we're looking for
    slnode<K,V>* findNext(fregion::imagefile* f, const K& k) {
      if (this->next.size == 0) {
        return 0;
      } else {
        auto* r = findNextGLEB(f, this->next.size-1, k, [](slnode<K,V>*){});
        return (r && r->key == k) ? r : 0;
      }
    }

    // choose an insert level >=1 randomly,
    // but with exponential decay for increasing levels
    //   to do this, it should be sufficient to count the number of trailing 1 bits
    //   in a uniformly distributed random number
    //    (1/2 end in 1b, 1/4 end in 11b, 1/8 end in 111b, etc)
    static level chooseLevel() {
      auto bv = static_cast<level>(rand());
      level r = 1;
      while ((bv&1) != 0) {
        ++r;
        bv>>=1;
      }
      return r;
    }

    // initialize a new level-0 list node at a particular point
    void insertAt(fregion::imagefile* f, slnode<K,V>* n, noderef freshNode, const K& k, const V& v, std::stack<slnode<K,V>*>& glebSpine) {
      // initialize this fresh node and link it to level 0
      auto* fn      = freshNode.load(f);
      fn->key       = k;
      fn->value     = v;
      fn->next.size = 1;
      fn->next[0]   = n->next[0];
      n->next[0]    = freshNode;

      // decide on a level for this node, and incrementally place it after the gleb node at each level
      // if there is no such node, this will be the first node for that level
      level newLevel = chooseLevel();
      for (level p = 1; p < newLevel; ++p) {
        if (glebSpine.empty()) {
          ++fn->next.size;
          this->next[p] = freshNode;
          ++this->next.size;
        } else {
          auto* g = glebSpine.top();
          ++fn->next.size;
          fn->next[p] = g->next[p];
          g->next[p]  = freshNode;
          glebSpine.pop();
        }
      }
    }

    // insert a new key:value or update an existing one
    bool insert(fregion::imagefile* f, const K& k, const V& v) {
      if (this->next.size == 0) {
        this->next.size=1;
      }

      std::stack<slnode<K,V>*> glebSpine;
      if (auto* n = findNextGLEB(f, this->next.size - 1, k, [&](slnode<K,V>* n){ glebSpine.push(n); })) {
        if (n->key < k) {
          // insert after the GLEB (because we don't have this exact key)
          insertAt(f, n, allocNode(f), k, v, glebSpine);
          return true;
        } else {
          // n->key==k
          // we don't need to allocate a new node
          n->value = v;
          return false;
        }
      } else {
        // insert at the root
        insertAt(f, this, allocNode(f), k, v, glebSpine);
        return true;
      }
    }
  };

namespace fregion {
template <typename K, typename V>
  struct store<slnode<K,V>> {
    static const bool can_memcpy = store<K>::can_memcpy && store<V>::can_memcpy;
    static_assert(can_memcpy, "only maps with memcpyable types currently supported");

    static ty::desc storeType() { return slnode<K,V>::type(); }
    static size_t size() { return sizeof(slnode<K,V>); }
    static size_t alignment() { return alignof(slnode<K,V>); }
    static void write(imagefile*, void* p, const slnode<K,V>& x) { memcpy(p, &x, sizeof(x)); }
    static void read(imagefile*, const void* p, slnode<K,V>* x) { memcpy(x, p, sizeof(*x)); }
  };
}

template <typename K, typename V>
  struct sliterator {
    fregion::imagefile* f;
    slnode<K,V>*        n;

    sliterator(fregion::imagefile* f=0, slnode<K,V>* n=0) : f(f), n(n) { }
    bool operator==(const sliterator<K,V>& rhs) const { return this->n==rhs.n; }
    std::pair<K,V>& operator*() { return *reinterpret_cast<std::pair<K,V>*>(&this->n->key); }
    std::pair<K,V>* operator->() { return reinterpret_cast<std::pair<K,V>*>(&this->n->key); }
    operator bool() const { return this->n != 0; }
    
    sliterator<K,V>& operator++() {
      if (this->n->next[0].index==0) {
        this->f=0;
        this->n=0;
      } else {
        this->n=this->n->next[0].load(this->f);
      }
      return *this;
    }

    sliterator<K,V> operator++(int) {
      sliterator<K,V> prev(this->f, this->n);
      ++(*this);
      return prev;
    }
  };

template <typename K, typename V>
  struct slmapdata {
    size_t count;
    slnode<K,V> root;

    static ty::desc type(const ty::desc& kty = fregion::store<K>::storeType(), const ty::desc& vty = fregion::store<V>::storeType()) {
      // slmap k v
      return
        ty::app(
          ty::prim(
            "slmap",
            ty::fn("k", "v",
              ty::rec("count", -1, ty::prim("long"), "root", -1, slnode<K,V>::type(ty::var("k"), ty::var("v")))
            )
          ),
          kty,
          vty
        );
    }

    slnode<K,V>* lookup(fregion::imagefile* f, const K& k) {
      return this->root.findNext(f, k);
    }

    void insert(fregion::imagefile* f, const K& k, const V& v) {
      if (this->root.insert(f, k, v)) {
        ++this->count;
      }
    }
  };

namespace fregion {
template <typename K, typename V>
  struct store<slmapdata<K,V>> {
    static const bool can_memcpy = store<K>::can_memcpy && store<V>::can_memcpy;
    static_assert(can_memcpy, "only maps with memcpyable types currently supported");

    static ty::desc storeType(const ty::desc& kty = store<K>::storeType(), const ty::desc& vty = store<V>::storeType()) { return slmapdata<K,V>::type(kty, vty); }
    static size_t size() { return sizeof(slmapdata<K,V>); }
    static size_t alignment() { return alignof(slmapdata<K,V>); }
    static void write(imagefile*, void* p, const slmapdata<K,V>& x) { memcpy(p, &x, sizeof(x)); }
    static void read(imagefile*, const void* p, slmapdata<K,V>* x) { memcpy(x, p, sizeof(*x)); }
  };
}

template <typename K, typename V>
  class slmap {
  public:
    slmap(const std::string& name, fregion::writer& w) {
      this->f = w.fileData();

      // allocate space for this structure and prepare to write
      ty::desc mty = fregion::store<slmapdata<K,V>>::storeType();

      auto b = this->f->bindings.find(name);
      if (b == this->f->bindings.end()) {
        // this structure is not yet defined, so define it and begin writing to it
        size_t dloc = fregion::findSpace(this->f, fregion::pagetype::data, sizeof(slmapdata<K,V>), alignof(slmapdata<K,V>));
        this->d = reinterpret_cast<slmapdata<K,V>*>(fregion::mapFileData(this->f, dloc, sizeof(slmapdata<K,V>)));
        addBinding(this->f, name, ty::encoding(mty), dloc);
      } else {
        // the structure is already defined, make sure it has the right type def and then resume writing to it
        if (b->second.type != ty::encoding(mty)) {
          throw std::runtime_error("File already defines slmap '" + name + "' with type inconsistent with " + ty::show(mty));
        } else {
          this->d = reinterpret_cast<slmapdata<K,V>*>(fregion::mapFileData(this->f, b->second.offset, sizeof(slmapdata<K,V>)));
        }
      }
    }

    size_t size() const {
      return this->d->count;
    }

    using iterator = sliterator<K, V>;
    iterator end()   { return iterator(); }
    iterator begin() { return (this->d->root.next.size==0 || this->d->root.next[0].index==0) ? end() : iterator(this->f, this->d->root.next[0].load(f)); }

    void insert(const K& k, const V& v) {
      this->d->insert(this->f, k, v);
    }

    iterator find(const K& k) {
      return iterator(this->f, this->d->lookup(this->f, k));
    }
  private:
    fregion::imagefile* f;
    slmapdata<K,V>* d;
  private:
    slmap();
    slmap(const slmap<K,V>&);
    slmap<K,V>& operator=(const slmap<K,V>&);
  };

template <typename K>
  class slrefmap {
  public:
    using ufileref = uint64_t;

    slrefmap(const std::string& name, fregion::imagefile* f, const ty::desc& refType) {
      this->f = f;

      // allocate space for this structure and prepare to write
      ty::desc mty = fregion::store<slmapdata<K,ufileref>>::storeType(fregion::store<K>::storeType(), refType);

      auto b = this->f->bindings.find(name);
      if (b == this->f->bindings.end()) {
        // this structure is not yet defined, so define it and begin writing to it
        size_t dloc = fregion::findSpace(this->f, fregion::pagetype::data, sizeof(slmapdata<K,ufileref>), alignof(slmapdata<K,ufileref>));
        this->d = reinterpret_cast<slmapdata<K,ufileref>*>(fregion::mapFileData(this->f, dloc, sizeof(slmapdata<K,ufileref>)));
        addBinding(this->f, name, ty::encoding(mty), dloc);
      } else {
        // the structure is already defined, make sure it has the right type def and then resume writing to it
        if (b->second.type != ty::encoding(mty)) {
          throw std::runtime_error("File already defines slmap '" + name + "' with type inconsistent with " + ty::show(mty));
        } else {
          this->d = reinterpret_cast<slmapdata<K,ufileref>*>(fregion::mapFileData(this->f, b->second.offset, sizeof(slmapdata<K,ufileref>)));
        }
      }
    }

    size_t size() const {
      return this->d->count;
    }

    using iterator = sliterator<K, ufileref>;
    iterator end()   { return iterator(); }
    iterator begin() { return (this->d->root.next.size==0 || this->d->root.next[0].index==0) ? end() : iterator(this->f, this->d->root.next[0].load(f)); }

    void insert(const K& k, const ufileref& v) {
      this->d->insert(this->f, k, v);
    }

    iterator find(const K& k) {
      return iterator(this->f, this->d->lookup(this->f, k));
    }
  private:
    fregion::imagefile* f;
    slmapdata<K,ufileref>* d;
  private:
    slrefmap();
    slrefmap(const slrefmap<K>&);
    slrefmap<K>& operator=(const slrefmap<K>&);
  };

}

#endif

