/*
 * storage : handle abstract storage into structured log files
 */

#ifndef HOBBES_STORED_SERIES_H_INCLUDED
#define HOBBES_STORED_SERIES_H_INCLUDED

#include <hobbes/hobbes.H>
#include <hobbes/db/file.H>
#include <hobbes/db/cbindings.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/util/time.H>
#include <hobbes/slmap.H>
#include <hobbes/util/perf.H>

namespace hobbes {

using ufileref = uint64_t;

// A -> (A)
MonoTypePtr entuple(const MonoTypePtr&);
// A N -> carray A N
MonoTypePtr carrayty(const MonoTypePtr&, const MonoTypePtr&);
// A -> ^x.(()+(A*x@?))
MonoTypePtr storedListOf(const MonoTypePtr&);
// A N -> fseq A N
MonoTypePtr storedStreamOf(const MonoTypePtr&, size_t);
// A -- StoredAs A B --> B
MonoTypePtr storeAs(cc* c, const MonoTypePtr&);

class RawStoredSeries {
public:
  RawStoredSeries(cc*, writer*, const std::string&, const MonoTypePtr&, size_t);
  RawStoredSeries(cc*, writer*, ufileref, const MonoTypePtr&, size_t);
  ~RawStoredSeries();

  // where has this series been placed?
  ufileref rootRef() const;

  // what would the whole sequence type look like for the given type?
  static MonoTypePtr seriesTypeDesc(cc*, const MonoTypePtr&, size_t);

  // what type is actually being recorded?
  const MonoTypePtr& storageType() const;

  // record a value in this series
  // (assumes that all such values are passed by reference)
  void record(const void*, bool signal = true);

  // bind a function to record data into this series
  // (assumes that this series will live at least as long as the bound function is usable)
  void bindAs(cc*, const std::string&);

  // what is the head write position in this file?
  // (this can be used to make a file reference to recorded values iff recording in raw mode)
  uint64_t writePosition() const;

  // "clear" the data (just reset the root node, ignore old data)
  void clear(bool signal = true);
private:
  ufileref rootLoc;

  writer*     outputFile;
  MonoTypePtr recordType;
  MonoTypePtr storedType;

  size_t      storageSize;
  MonoTypePtr batchType;
  size_t      batchSize;
  size_t      batchStorageSize;

  uint64_t  batchDataRef;
  void*     batchData;
  uint8_t*  batchHead;
  uint64_t  batchNode;
  uint64_t* headNodeRef;

  using StoreFn = void (*)(writer *, const void *, void *);
  StoreFn storeFn;

  void consBatchNode(uint64_t nextPtr);
  void restartFromBatchNode();

  static uint64_t allocBatchNode(writer*);
  static uint64_t allocBatchNode(writer*,uint64_t,uint64_t);
};

class CompressedStoredSeries {
public:
  CompressedStoredSeries(cc*, writer*, ufileref, const MonoTypePtr&, size_t);
  CompressedStoredSeries(cc*, writer*, const std::string&, const MonoTypePtr&, size_t);
  ~CompressedStoredSeries();

  using CWriteFn = void (*)(UCWriter *, void *, const void *);
  using CAllocM = uint8_t *(*)(writer *);
  using CPrepM = void (*)(UCWriter *, uint8_t *);
  using CDeallocM = void (*)(uint8_t *);

  // where has this series been placed?
  ufileref rootRef() const;

  // what would the whole compressed sequence type look like for the given type?
  static MonoTypePtr seriesTypeDesc(cc*, const MonoTypePtr&, size_t);

  // what type is actually being recorded?
  const MonoTypePtr& storageType() const;
  
  // record a value in this series
  // (assumes that all such values are passed by reference)
  void record(const void*, bool signal = true);

  // bind a function to record data into this series
  // (assumes that this series will live at least as long as the bound function is usable)
  void bindAs(cc*, const std::string&);
private:
  using ModelTypes = std::pair<MonoTypePtr, MonoTypePtr>;

  writer*     outputFile;
  MonoTypePtr recordType;
  ModelTypes  modelTypes;
  MonoTypePtr seqType;
  ufileref    rootLoc;
  UCWriter    w;
  uint8_t*    dynModel;
  region      dynModelMem;

  CWriteFn    writeFn;
  CAllocM     allocMFn;
  CPrepM      prepMFn;
  CDeallocM   deallocMFn;
};

class StoredSeries {
public:
  enum StorageMode {
    Raw = 0,
    Compressed
  };
  ~StoredSeries();

  // create a stored series with a top-level variable binding
  StoredSeries(cc*, writer*, const std::string&, const MonoTypePtr&, size_t, StorageMode sm = Raw);

  // create an "anonymous" stored series at a predefined location (or if location==0, create at a new location)
  StoredSeries(cc*, writer*, ufileref, const MonoTypePtr&, size_t, StorageMode sm = Raw);

  // where has this series been placed?
  ufileref rootRef() const;

  // what would the whole sequence type look like if storing the given type?
  static MonoTypePtr seriesTypeDesc(StorageMode, cc*, const MonoTypePtr&, size_t);

  // what type is actually being recorded?
  const MonoTypePtr& storageType() const;

  // record a value in this series
  // (assumes that all such values are passed by reference)
  void record(const void*, bool signal = true);

  // bind a function to record data into this series
  // (assumes that this series will live at least as long as the bound function is usable)
  void bindAs(cc*, const std::string&);

  // what is the head write position in this file?
  // (this can be used to make a file reference to recorded values iff recording in raw mode)
  uint64_t writePosition() const;

  // "clear" the data (just reset the root node, ignore old data)
  void clear(bool signal = true);
private:
  StorageMode sm;
  union {
    alignas(RawStoredSeries) char rss[sizeof(RawStoredSeries)];
    alignas(CompressedStoredSeries) char css[sizeof(CompressedStoredSeries)];
  } storage;
};

template <typename T>
  class series {
  public:
    series(cc* c, writer* db, const std::string& sname, size_t bsize = 10000, StoredSeries::StorageMode sm = StoredSeries::Raw) : storage(c, db, sname, lift<T, true>::type(*c), bsize, sm) {
    }
    void record(const T& x, bool signal = true) {
      this->storage.record(&x, signal);
    }
    void operator()(const T& x, bool signal = true) {
      this->storage.record(&x, signal);
    }
    void clear(bool signal = true) {
      this->storage.clear(signal);
    }
  private:
    StoredSeries storage;
  };

template <typename K>
  class keyseriesdv {
  public:
    keyseriesdv(cc* c, writer* db, const std::string& name, const MonoTypePtr& vty, size_t bsize = 10000, StoredSeries::StorageMode sm = StoredSeries::Raw) :
      c(c), db(db), vty(vty), bsize(bsize), sm(sm),
      pmap(name, db->fileData(), toTD(StoredSeries::seriesTypeDesc(sm, c, vty, bsize)))
    {
      for (const auto& pm : this->pmap) {
        this->dmap[pm.first] = new StoredSeries(this->c, this->db, pm.second, vty, this->bsize, this->sm);
      }
    }

    ~keyseriesdv() {
      for (auto& d : this->dmap) {
        delete d.second;
      }
    }

    void record(const K& k, const void* v, bool signal = true) {
      auto d = this->dmap.find(k);
      if (d != this->dmap.end()) {
        d->second->record(v, signal);
      } else {
        auto* s = new StoredSeries(this->c, this->db, 0, this->vty, this->bsize, this->sm);
        this->pmap.insert(k, s->rootRef());
        this->dmap[k] = s;
        s->record(v, signal);
      }
    }
  private:
    cc*                       c;
    writer*                   db;
    MonoTypePtr               vty;
    size_t                    bsize;
    StoredSeries::StorageMode sm;

    slrefmap<K>                pmap;
    std::map<K, StoredSeries*> dmap;

    static ty::desc toTD(const MonoTypePtr& t) {
      ty::bytes b;
      encode(t, &b);
      return ty::decode(b);
    }
  };

template <typename K, typename V>
  class keyseries : public keyseriesdv<K> {
  public:
    keyseries(cc* c, writer* db, const std::string& name, size_t bsize = 10000, StoredSeries::StorageMode sm = StoredSeries::Raw) : keyseriesdv<K>(c, db, name, lift<V,true>::type(*c), bsize, sm)
    {
    }
    void record(const K& k, const V& v, bool signal = true) {
      keyseriesdv<K>::record(k, reinterpret_cast<const void*>(&v), signal);
    }
  };


}

#endif

