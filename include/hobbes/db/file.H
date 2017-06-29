/********************************
 * file : a system for persisting hobbes data
 ********************************/

#ifndef HOBBES_DB_FILE_HPP_INCLUDED
#define HOBBES_DB_FILE_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/lang/tylift.H>
#include <string>

namespace hobbes {

// is this file a db file?
bool isDBFile(const std::string&);

// is this type stored the same way as an array (unpredictable size)?
bool storedAsArray(const MonoTypePtr&);

// a file reference is an index into a file where we can find a value of type T (like a pointer)
template <typename T>
  struct fileref {
    uint64_t index;

    fileref<T>& operator=(const fileref<T>& x) {
      this->index = x.index;
      return *this;
    }

    fileref(uint64_t index = 0) : index(index) {
    }
  };

template <typename T, bool InStruct>
  struct lift< fileref<T>, InStruct > {
    static MonoTypePtr type(typedb& tenv) {
      return MonoTypePtr(TApp::make(MonoTypePtr(Prim::make("fileref")), list(lift<T, false>::type(tenv))));
    }
  };

// a dbseq is a linked list sequence just like 'seq' except that its links are file offsets rather than memory offsets
template <typename T>
  struct dbseq {
    typedef fileref<dbseq<T>*>    link_t;
    typedef std::pair<T, link_t>  cons_t;
    typedef variant<unit, cons_t> rep_t;

    rep_t data;

    dbseq() : data(unit()) { }
    dbseq(const T& x, link_t xs) : data(cons_t(x, xs)) { }

    bool empty() const { return get<unit>(this->data) != 0; }
    const cons_t* head() const { return get<cons_t>(this->data); }
  };

template <typename T, bool InStruct>
  struct lift<dbseq<T>*, InStruct> {
    static MonoTypePtr type(typedb& tenv) {
      // ^x.()+T*x@n
      MonoTypePtr tlink = MonoTypePtr(TApp::make(MonoTypePtr(Prim::make("fileref")), list(tvar("x"))));
      MonoTypePtr tcons = tuple(list(lift<T, true>::type(tenv), tlink));

      return MonoTypePtr(Recursive::make("x", MonoTypePtr(Variant::make(list(Variant::Member(".f0", primty("unit"), 0), Variant::Member(".f1", tcons, 1))))));
    }
  };

// determine representation type (this is necessary to differentiate types represented by value or reference)
template <typename T>
  struct frefty {
    typedef T  type;
    typedef T* ptr;
  };

template <typename T>
  struct frefty< array<T> > {
    typedef array<T>* type;
    typedef array<T>* ptr;
  };

template <typename T>
  struct frefty< array<T>* > {
    typedef array<T>* type;
    typedef array<T>* ptr;
  };

template <typename T>
  struct frefty< dbseq<T> > {
    typedef dbseq<T>* type;
    typedef dbseq<T>* ptr;
  };

template <typename T>
  struct frefty< dbseq<T>* > {
    typedef dbseq<T>* type;
    typedef dbseq<T>* ptr;
  };

template <typename T>
  struct frefty< recursive<T> > {
    typedef recursive<T>* type;
    typedef recursive<T>* ptr;
  };

template <typename T>
  struct frefty< recursive<T>* > {
    typedef recursive<T>* type;
    typedef recursive<T>* ptr;
  };

// common typedefs
typedef fileref<array<char>*> strref;

// what is the size of a type as represented in storage?
size_t storageSizeOf(const MonoTypePtr&);

// the main compiler interface (no need to import its full definition here)
class cc;

// data file details are internally defined
struct imagefile;

// a db reader can only read data from a file
class reader {
public:
  reader(imagefile*);
  reader(const std::string&);
  virtual ~reader();

  const std::string& file() const;
  size_t size() const;

  // get a description of the type signature of this file
  MonoTypeSubst signature() const;

  // find a named variable within this file (and assert that it has a certain type)
  template <typename T>
    typename frefty<T>::ptr lookup(const std::string& vn) const {
      return (typename frefty<T>::ptr)unsafeLookup(vn, lift< typename frefty<T>::type, false >::type(nulltdb));
    }
 
  // determine the file offset of a stored value
  template <typename T>
    fileref< typename frefty<T>::type > offsetOf(const T* x) const {
      return fileref< typename frefty<T>::type >(unsafeOffsetOf(lift< typename frefty<T>::type, false >::type(nulltdb), (void*)x));
    }

  // load an anonymous stored value
  template <typename T>
    typename frefty<T>::ptr load(const fileref<T>& x) {
      return (typename frefty<T>::ptr)unsafeLoad(lift< typename frefty<T>::type, false >::type(nulltdb), x.index);
    }

  // indicate that we're finished with some mapped memory
  template <typename T>
    void unmap(T* x) {
      unsafeUnload(x, sizeof(T));
    }

  template <typename T>
    void unmap(array<T>* xs) {
      unsafeUnloadArray(xs);
    }
private:
  typedef std::vector<uint64_t> StoredOffsets;
  typedef std::vector<size_t>   StoredOffsetSizes;

  StoredOffsets     storedOffsets;
  StoredOffsetSizes storedOffSizes;
public:
  // to speed up dynamic lookups of top-level variables at runtime
  //  (this allows an external user to skip string matching and type structure analysis)
  inline unsigned int pushOffset(uint64_t offset, const MonoTypePtr& ty) {
    unsigned int r = (unsigned int)this->storedOffsets.size();
    this->storedOffsets.push_back(offset);
    this->storedOffSizes.push_back(is<Array>(ty) ? 0 : storageSizeOf(ty));
    return r;
  }

  inline void* unsafeLoadStoredOffset(unsigned int o) const {
    if (this->storedOffSizes[o] > 0) {
      return unsafeLoad(this->storedOffsets[o], this->storedOffSizes[o]);
    } else {
      return unsafeLoadArray(this->storedOffsets[o]);
    }
  }

  inline void storedOffsetDetails(unsigned int o, uint64_t* offset, size_t* sz) const {
    *offset = this->storedOffsets[o];
    *sz     = this->storedOffSizes[o];
  }
public:
  // for debugging, show the state of parts of this file and its data structures
  void showFileSummary(std::ostream&) const;
  void showEnvironment(std::ostream&) const;
  void showMappings(std::ostream&,size_t) const;
  void showPageTable(std::ostream&,size_t) const;
  void show(std::ostream&) const;
public:
  // be careful using these, they require that types are used correctly at runtime
  void* unsafeLookup(const std::string&, const MonoTypePtr&) const;
  uint64_t unsafeLookupOffset(const std::string&, const MonoTypePtr&) const;
  uint64_t unsafeOffsetOf(const MonoTypePtr&, void*) const;
  uint64_t unsafeOffsetOfVal(bool,void*) const;
  void* unsafeLoad(const MonoTypePtr&, uint64_t) const;
  void* unsafeLoad(uint64_t,size_t) const;
  void* unsafeLoadArray(uint64_t) const;
  void unsafeUnload(void*, size_t);
  void unsafeUnloadArray(void*);

  uint64_t unsafeArrayCapacity(uint64_t) const;

  int unsafeGetFD() const;
public:
  // helpful diagnostic functions
  typedef std::pair<uint8_t, short> PageEntry;
  typedef array<PageEntry>          PageEntries;

  PageEntries* pageEntries() const;
protected:
  imagefile* fdata;
};

// a db writer has exclusive access to write data into a file (and may read as well)
class writer : public reader {
public:
  writer(const std::string&);

  // typed top-level allocation out of this file
  template <typename T>
    T* define(const std::string& vn) {
      return (T*)unsafeDefine(vn, lift<T, true>::type(nulltdb));
    }

  template <typename T>
    array<T>* define(const std::string& vn, size_t len) {
      return (array<T>*)unsafeDefine(vn, lift<T, true>::type(nulltdb), len);
    }

  // typed anonymous allocation out of this file
  template <typename T>
    T* store() {
      return (T*)unsafeStore(lift<T, true>::type(nulltdb));
    }

  template <typename T>
    array<T>* store(size_t len) {
      return (array<T>*)unsafeStore(lift<T, true>::type(nulltdb), len);
    }

  // convenience methods for anonymous storage of arrays
  fileref<array<char>*> store(const char*, size_t);
  fileref<array<char>*> store(const char*);
  fileref<array<char>*> store(const array<char>*);
  fileref<array<char>*> store(const std::string&);

  template <typename TIter>
    fileref<array<typename std::iterator_traits<TIter>::value_type>*> store(TIter b, TIter e) {
      typedef typename std::iterator_traits<TIter>::value_type T;
      size_t len = e - b;
      array<T>* r = store<T>(len);
      for (size_t i = 0; i < len; ++i) {
        r->data[i] = *b;
        ++b;
      }
      r->size = len;
      auto result = offsetOf(r);
      unmap(r);
      return result;
    }
public:
  // be careful using these, they require that types are used correctly at runtime
  void* unsafeDefine(const std::string&, const MonoTypePtr&);
  void* unsafeDefine(const std::string&, const MonoTypePtr&, size_t);

  void* unsafeStore(const MonoTypePtr&);
  void* unsafeStore(const MonoTypePtr&, size_t);

  void*    unsafeStore(size_t sz, size_t align);
  uint64_t unsafeStoreToOffset(size_t sz, size_t align);

  void*    unsafeStoreArray(size_t, size_t);
  uint64_t unsafeStoreArrayToOffset(size_t, size_t);

  void signalUpdate();
private:
  void* allocNamed(const std::string&, const MonoTypePtr&, size_t);
  void* allocAnon(size_t sz, size_t align);
};

// a utility function to create all implied directories in a path (used by uniqueFilename, writer)
void ensureDirExists(const std::string&);

// a utility function for safely generating new files
std::string uniqueFilename(const std::string& prefix, const std::string& suffix);

// a utility function for safely moving to new files
std::string moveToUniqueFilename(const std::string& oldpath, const std::string& fprefix, const std::string& fsuffix);

// make it easy to bind files
template <>
  struct liftValue< reader* > {
    static MonoTypePtr type(typedb& tenv, reader* r) {
      MonoTypeSubst fdefs = r->signature();
      Record::Members ms;
      for (const auto& m : fdefs) {
        ms.push_back(Record::Member(m.first, m.second));
        r->pushOffset(r->unsafeLookupOffset(m.first, m.second), m.second);
      }
      return tapp(primty("file"), list(tlong(0), MonoTypePtr(Record::make(ms))));
    }
  };

template <>
  struct liftValue< writer* > {
    static MonoTypePtr type(typedb& tenv, writer* w) {
      MonoTypeSubst fdefs = w->signature();
      Record::Members ms;
      for (const auto& m : fdefs) {
        ms.push_back(Record::Member(m.first, m.second));
        w->pushOffset(w->unsafeLookupOffset(m.first, m.second), m.second);
      }
      return tapp(primty("file"), list(tlong(1), MonoTypePtr(Record::make(ms))));
    }
  };

}

#endif

