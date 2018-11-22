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
#include <hobbes/util/perf.H>

namespace hobbes {

class RawStoredSeries {
public:
  RawStoredSeries(cc*, writer*, const std::string&, const MonoTypePtr&, size_t);
  ~RawStoredSeries();

  // what type will actually be recorded?
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

  typedef void (*StoreFn)(writer*, const void*, void*);
  StoreFn storeFn;

  void consBatchNode(uint64_t nextPtr);
  void restartFromBatchNode();

  static uint64_t allocBatchNode(writer*);
  static uint64_t allocBatchNode(writer*,uint64_t,uint64_t);
};

class CompressedStoredSeries {
public:
  CompressedStoredSeries(cc*, writer*, const std::string&, const MonoTypePtr&, size_t);
  ~CompressedStoredSeries();

  typedef void (*CWriteFn)(UCWriter*, void*, const void*);
  typedef uint8_t* (*CAllocM)(writer*);
  typedef void (*CPrepM)(UCWriter*,uint8_t*);
  typedef void (*CDeallocM)(uint8_t*);

  // what type will actually be recorded?
  const MonoTypePtr& storageType() const;
  
  // record a value in this series
  // (assumes that all such values are passed by reference)
  void record(const void*, bool signal = true);

  // bind a function to record data into this series
  // (assumes that this series will live at least as long as the bound function is usable)
  void bindAs(cc*, const std::string&);
private:
  typedef std::pair<MonoTypePtr, MonoTypePtr> ModelTypes;

  writer*     outputFile;
  MonoTypePtr recordType;
  ModelTypes  modelTypes;
  MonoTypePtr seqType;
  UCWriter    w;
  uint8_t*    dynModel;
  region      dynModelMem;
  size_t      regionID;

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

  StoredSeries(cc*, writer*, const std::string&, const MonoTypePtr&, size_t, StorageMode sm = Raw);
  ~StoredSeries();

  // what type will actually be recorded?
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
    char rss[sizeof(RawStoredSeries)];
    char css[sizeof(CompressedStoredSeries)];
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

}

#endif

