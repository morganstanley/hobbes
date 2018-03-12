/*
 * storage : handle abstract storage into structured log files
 */

#ifndef HOBBES_STORED_SERIES_H_INCLUDED
#define HOBBES_STORED_SERIES_H_INCLUDED

#include <hobbes/hobbes.H>
#include <hobbes/db/file.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/util/time.H>
#include <hobbes/util/perf.H>

namespace hobbes {

class StoredSeries {
public:
  StoredSeries(cc*, writer*, const std::string&, const MonoTypePtr&, size_t);
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
  // (this can be used to make a file reference to recorded values)
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

template <typename T>
  class series {
  public:
    series(cc* c, writer* db, const std::string& sname, size_t bsize = 10000) : storage(c, db, sname, lift<T, true>::type(*c), bsize) {
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

inline MonoTypePtr filerefty(const MonoTypePtr& t) {
  return tapp(primty("fileref"), list(t));
}

}

#endif

