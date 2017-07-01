/*
 * storage : structured storage of application data
 *
 *   use DEFINE_STORAGE_GROUP(G, C, QoS, T) to create storage group / transaction context
 *   use DECLARE_STORAGE_GROUP(G) to forward-declare the storage group G (suitable for declaration in program headers
 *   use HSTORE(G,N,V0,V1,...) to record the data V0,V1,... with the name N in the group G
 *   use HLOG  (G,N,"text display",V0,V1,...) to record the data V0,V1,... with the name N in the group G (with the display hint "text display" to reconstruct text)
 *
 */

#ifndef HSTORE_H_INCLUDED
#define HSTORE_H_INCLUDED

#include <map>
#include <vector>
#include <string>
#include <string.h>
#include <tuple>
#include <functional>
#include <iostream>
#include <stdexcept>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>

// a few things have to be OS-specific here
//  * waiting in shared memory
#if defined(__APPLE__) && defined(__MACH__)
namespace hobbes { namespace storage {
static inline void waitForUpdate(volatile uint32_t* p, int eqV) {
  while (true) {
    for (size_t c = 0; c < 4096; ++c) {
      if (*p != eqV) {
        return;
      }
    }
    usleep(500);
  }
}
static inline void wakeN(volatile uint32_t* p, int c) {
  // assume the waiter is polling, nothing to do
}
}}
#else
#include <linux/futex.h>
#include <sys/syscall.h>

namespace hobbes { namespace storage {
static inline long sys_futex(volatile uint32_t* p, int op, int v, struct timespec* timeout, void* p2, int v2) {
  return syscall(SYS_futex, p, op, v, timeout, p2, v2);
}
static inline void waitForUpdate(volatile uint32_t* p, int eqV) {
  sys_futex(p, FUTEX_WAIT, eqV, 0, 0, 0);
}
static inline void wakeN(volatile uint32_t* p, int c) {
  sys_futex(p, FUTEX_WAKE, c, 0, 0, 0);
}
}}
#endif

namespace hobbes { namespace storage {

#define HSTORE_VERSION ((uint32_t)0x00010000)

typedef std::vector<uint8_t> bytes;

// write transactions into shared memory
#define _HSTORE_LIKELY(x)   __builtin_expect((x),1)
#define _HSTORE_UNLIKELY(x) __builtin_expect((x),0)

template <typename T>
  T align(T x, T m) {
    if (m == 0 || (x % m) == 0) {
      return x;
    } else {
      return (1 + (x / m)) * m;
    }
  }

static inline void uxchg(volatile uint32_t* px, uint32_t nx) {
  __asm__ __volatile__(
    "xchgl %0,%1"
    :"=r" (nx)
    :"m" (*px), "0" (nx)
    :"memory"
  );
}
#define xchg __sync_lock_test_and_set

// the convention for deriving shared memory region names from group names
inline std::string sharedMemName(const std::string& groupName) {
  return "/hstore." + groupName;
}

// between queue readers and writers, there are only three states of concern:
//   0: both reader and writer are making progress
//   1: the reader is blocked waiting for new values (empty queue)
//   2: the writer is blocked waiting for the reader to catch up (full queue)
#define _HSTORE_STATE_UNBLOCKED      0
#define _HSTORE_STATE_READER_WAITING 1
#define _HSTORE_STATE_WRITER_WAITING 2

struct pqueue_config {
  pqueue_config() :
    valuesz(0), count(0), wstate(0), readerIndex(0), writerIndex(0), data(0) {
  }
  
  pqueue_config(size_t valuesz, size_t count, uint32_t* wstate, uint32_t* ri, uint32_t* wi, uint8_t* data) :
    valuesz(valuesz), count(count), wstate(wstate), readerIndex(ri), writerIndex(wi), data(data)
  {
  }

  size_t             valuesz;     // how large is one "value" or queue element?
  size_t             count;       // how many "values" (of size 'valuesz') are there indexable from 'data'?
  volatile uint32_t* wstate;      // inter-process wait state : 0=no waiting, 1=reader waiting, 2=writer waiting
  volatile uint32_t* readerIndex; // where is the reader in the data sequence?
  volatile uint32_t* writerIndex; // where is the writer in the data sequence?
  uint8_t*           data;        // the actual queue data
};

// shared memory queue data
struct ShQueueHeader {
  uint32_t ready;  // set to 1 when the queue has been fully constructed and is ready to read
  size_t   valsz;  // the size of a single "queue value"
  size_t   count;  // the number of queue values defined in the queue
  size_t   metasz; // the size of the following meta-data section
};

struct ShQueueData {
  uint32_t wstate;
  uint32_t ri;
  uint32_t wi;
  uint32_t unused;
};

// write data into shared memory
class writer {
private:
  std::string   shmname;
  int           shmfd;
  pqueue_config cfg;

  inline volatile uint32_t* waitState()                     const { return this->cfg.wstate; }
  inline volatile uint32_t* readIndex()                     const { return this->cfg.readerIndex; }
  inline volatile uint32_t* writeIndex()                    const { return this->cfg.writerIndex; }
  inline uint8_t*           value(size_t i)                 const { return this->cfg.data + (i*this->cfg.valuesz); }
  inline uint32_t           nextIndex(volatile uint32_t* i) const { return (*i + 1) % this->cfg.count; }
public:
  writer(const bytes& meta, const std::string& shmname, size_t qvalsz, size_t count) {
    shm_unlink(shmname.c_str());

    // sections of shared memory should be aligned to page boundaries
    long pagesz = sysconf(_SC_PAGESIZE);
    if (pagesz == -1) {
      throw std::runtime_error("Failed to query system page size for '" + shmname + "': " + strerror(errno));
    }
  
    // create the shared memory region
    int shfd = shm_open(shmname.c_str(), O_RDWR | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
    if (shfd == -1) {
      throw std::runtime_error("Failed to allocate shared memory for '" + shmname + "': " + strerror(errno));
    }
  
    // our meta-data section comes first up to the first page boundary
    // then our data section comes next
    size_t metaLen = align<size_t>(sizeof(ShQueueHeader) + meta.size(),  pagesz);
    size_t dataLen = align<size_t>(sizeof(ShQueueData)   + qvalsz*count, pagesz);
    size_t memLen  = metaLen + dataLen;
  
    // allocate this much data
    if (ftruncate(shfd, memLen) == -1) {
      throw std::runtime_error("Failed to truncate shared memory for '" + shmname + "': " + strerror(errno));
    }
  
    uint8_t* mem = (uint8_t*)mmap(0, memLen, PROT_READ | PROT_WRITE, MAP_SHARED, shfd, 0);
    if (mem == MAP_FAILED) {
      throw std::runtime_error("Failed to map bytes out of shared memory for '" + shmname + "': " + strerror(errno));
    }
  
    // write meta data
    ShQueueHeader* hdr = (ShQueueHeader*)mem;
    hdr->valsz  = qvalsz;
    hdr->count  = count;
    hdr->metasz = meta.size();
    memcpy(mem + sizeof(ShQueueHeader), &meta[0], meta.size());
  
    // OK, this queue is fully initialized
    uxchg(&hdr->ready, 1);
  
    // now make this pqueue config
    ShQueueData* sqd = (ShQueueData*)(mem + metaLen);
  
    this->shmname = shmname;
    this->shmfd   = shfd;
    this->cfg.valuesz     = qvalsz;
    this->cfg.count       = count;
    this->cfg.wstate      = &sqd->wstate;
    this->cfg.readerIndex = &sqd->ri;
    this->cfg.writerIndex = &sqd->wi;
    this->cfg.data        = mem + metaLen + sizeof(ShQueueData);
  }

  ~writer() {
    shm_unlink(this->shmname.c_str());
  }

  inline const pqueue_config& config() const { return this->cfg; }

  uint8_t* next() {
    uint32_t nwi = nextIndex(writeIndex());
  
    while (_HSTORE_UNLIKELY(*readIndex() == nwi)) {
      // the reader is behind and we've caught up with it, switch into writer-wait mode
      switch (xchg(waitState(), _HSTORE_STATE_WRITER_WAITING)) {
      case _HSTORE_STATE_UNBLOCKED:
        // we previously were unblocked
        // make sure that we still need to block the writer (in case the read index moved while we were getting here)
        // then block while we're in writer-wait state
        if (*readIndex() == nwi) {
          waitForUpdate(waitState(), _HSTORE_STATE_WRITER_WAITING);
        }
        break;
      case _HSTORE_STATE_READER_WAITING:
        // we previously were in reader-wait state (this should practically never happen)
        // since we wait to write anyway, unblock the reader and try again
        uxchg(waitState(), _HSTORE_STATE_UNBLOCKED);
        wakeN(waitState(), 1);
        break;
      }
    }
    return value(*writeIndex());
  }
  
  uint8_t* pollNext() {
    if (_HSTORE_UNLIKELY(*readIndex() == nextIndex(writeIndex()))) {
      return 0;
    } else {
      return value(*writeIndex());
    }
  }

  void push() {
    uxchg(writeIndex(), nextIndex(writeIndex()));
  
    // when the writer advances, the reader can be unblocked
    if (_HSTORE_UNLIKELY(xchg(waitState(), _HSTORE_STATE_UNBLOCKED) == _HSTORE_STATE_READER_WAITING)) {
      wakeN(waitState(), 1);
    }
  }
};

// shared memory pages can be marked as representing four possible conditions:
//   1) page is published but state is undetermined (contingent on being able to acquire another page without blocking)
//   2) page is a continuation of its transaction (intermediate in the total transaction)
//   3) page successfully terminates the transaction (commit)
//   4) page prematurely terminates the transaction (rollback)
#define _HSTORE_PAGE_STATE_TENTATIVE ((uint8_t)0)
#define _HSTORE_PAGE_STATE_CONT      ((uint8_t)1)
#define _HSTORE_PAGE_STATE_COMMIT    ((uint8_t)2)
#define _HSTORE_PAGE_STATE_ROLLBACK  ((uint8_t)3)

// writers may record data 'unreliably' (non-blocking) or 'reliably' (block iff waiting for consumer to catch up)
enum PipeQOS {
  Reliable = 0,
  Unreliable
};

// wpipe : a "write pipe" (to write an 'arbitrary-length' sequence of data) on top of writers
class wpipe {
private:
  writer*         wq;
  uint8_t*        page;
  size_t          pagesz;
  uint32_t        offset;
  PipeQOS         qos;

  void markPage(uint8_t c) {
    *((uint32_t*)(this->page + this->pagesz)) = (((uint32_t)c) << 24) | this->offset;
  }

  inline bool reliable() const {
    return this->qos == Reliable;
  }

  bool stepPage() {
    if (reliable()) {
      markPage(_HSTORE_PAGE_STATE_CONT);
      this->wq->push();

      this->page   = this->wq->next();
      this->offset = 0;
      return true;
    } else {
      markPage(_HSTORE_PAGE_STATE_TENTATIVE);
      this->wq->push();
      uint8_t* npage = this->wq->pollNext();
      markPage(npage ? _HSTORE_PAGE_STATE_CONT : _HSTORE_PAGE_STATE_ROLLBACK);

      this->page   = npage;
      this->offset = 0;
      return npage;
    }
  }
public:
  wpipe(writer* wq, PipeQOS qos = Reliable) : wq(wq), pagesz(wq->config().valuesz - sizeof(uint32_t)), offset(0), qos(qos) {
    if (wq->config().valuesz <= sizeof(uint32_t)) {
      throw std::runtime_error("queue page size too small for use as shared memory pipe");
    }
    this->page = wq->pollNext();
  }

  void commit() {
    if (_HSTORE_LIKELY(this->page != 0)) {
      markPage(_HSTORE_PAGE_STATE_COMMIT);
      this->wq->push();
    }
    this->page   = reliable() ? this->wq->next() : this->wq->pollNext();
    this->offset = 0;
  }

  bool hasSpaceFor(size_t sz) const {
    return this->page && sz < (this->pagesz - this->offset);
  }

  // write a block of bytes within a frame
  bool write(const uint8_t* src, size_t sz) {
    // just for unreliable pipes, we might enter here without a page
    if (_HSTORE_UNLIKELY(!this->page)) {
      return false;
    }

    size_t remsz = this->pagesz - this->offset;

    // most of the time we'll be writing small chunks of data
    if (_HSTORE_LIKELY(sz < remsz)) {
      memcpy(this->page + this->offset, src, sz);
      this->offset += sz;
      return true;
    }

    // now we might cross any number of pages
    // write the src prefix to the remaining page space
    // write all intermediate complete pages
    // write the src suffix to the last page
    memcpy(this->page + this->offset, src, remsz);
    this->offset += remsz;

    size_t so = remsz;
    while (stepPage() && sz - so >= this->pagesz) {
      memcpy(this->page, src + so, this->pagesz);
      so += this->pagesz;
    }
    if (!this->page) {
      return false;
    } else {
      if (so < sz) {
        this->offset = sz - so;
        memcpy(this->page, src + so, this->offset);
      }
      return true;
    }
  }
};

struct QueueConnection {
  int      shfd;
  uint8_t* data;
  size_t   datasz;
  size_t   pagesz;
};

inline QueueConnection consumeGroup(const std::string& gname) {
  std::string shmname = sharedMemName(gname);

  // sections of shared memory are aligned to page boundaries
  long pagesz = sysconf(_SC_PAGESIZE);
  if (pagesz == -1) {
    throw std::runtime_error("Failed to query system page size for '" + gname + "': " + strerror(errno));
  }

  // see if we can open this shared memory region
  int shfd = shm_open(shmname.c_str(), O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  if (shfd == -1) {
    throw std::runtime_error("Failed to open shared memory for '" + gname + "': " + strerror(errno));
  }

  // make sure that the region has been sized
  struct stat msb;
  if (fstat(shfd, &msb) < 0) {
    close(shfd);
    throw std::runtime_error("Failed to stat shared memory for '" + gname + "': " + strerror(errno));
  }
  if (msb.st_size <= 0) {
    close(shfd);
    throw std::runtime_error("Shared memory for '" + gname + "' is not ready");
  }

  // map memory for this data, ensure it is in a good state
  uint8_t* mem = (uint8_t*)mmap(0, msb.st_size, PROT_READ | PROT_WRITE, MAP_SHARED, shfd, 0);
  if (mem == MAP_FAILED) {
    close(shfd);
    throw std::runtime_error("Failed to map bytes out of shared memory for '" + gname + "': " + strerror(errno));
  }

  if (!((ShQueueHeader*)mem)->ready) {
    munmap(mem, msb.st_size);
    close(shfd);
    throw std::runtime_error("Not ready to consume shared memory for '" + gname + "'");
  }

  // ok, if we get here then we can take ownership of the queue (hence unlink it)
  shm_unlink(shmname.c_str());

  QueueConnection c;
  c.shfd   = shfd;
  c.data   = mem;
  c.datasz = msb.st_size;
  c.pagesz = pagesz;
  return c;
}

// read data out of shared memory
class reader {
private:
  int            shfd;
  const uint8_t* metad;
  size_t         metasz;
  pqueue_config  cfg;

  inline volatile uint32_t* waitState()                     const { return this->cfg.wstate; }
  inline volatile uint32_t* readIndex()                     const { return this->cfg.readerIndex; }
  inline volatile uint32_t* writeIndex()                    const { return this->cfg.writerIndex; }
  inline uint8_t*           value(size_t i)                 const { return this->cfg.data + (i*this->cfg.valuesz); }
  inline uint32_t           nextIndex(volatile uint32_t* i) const { return (*i + 1) % this->cfg.count; }
public:
  reader(const QueueConnection& qc) : shfd(qc.shfd) {
    // prepare to read the queue description
    ShQueueHeader* hdr     = (ShQueueHeader*)qc.data;
    size_t         metaLen = align<size_t>(sizeof(ShQueueHeader) + hdr->metasz, qc.pagesz);
    ShQueueData*   sqd     = (ShQueueData*)(qc.data + metaLen);

    // now we should have enough to read out of this queue
    this->metad           = qc.data + sizeof(ShQueueHeader);
    this->metasz          = hdr->metasz;
    this->cfg.valuesz     = hdr->valsz;
    this->cfg.count       = hdr->count;
    this->cfg.wstate      = &sqd->wstate;
    this->cfg.readerIndex = &sqd->ri;
    this->cfg.writerIndex = &sqd->wi;
    this->cfg.data        = qc.data + metaLen + sizeof(ShQueueData);
  }

  reader(const std::string& gname) : reader(consumeGroup(gname)) {
  }

  ~reader() {
    close(this->shfd);
  }

  inline const pqueue_config& config() const { return this->cfg; }

  // access queue init data; (null,0) if no data was specified
  typedef std::pair<const uint8_t*, size_t> MetaData;
  MetaData meta() const {
    return MetaData(this->metad, this->metasz);
  }

  // get the next value in the queue, blocking if necessary
  uint8_t* next() {
    uint32_t ri = *readIndex();
  
    while (_HSTORE_UNLIKELY(*writeIndex() == ri)) {
      // there's nothing to read, switch into reader-wait mode
      switch (xchg(waitState(), _HSTORE_STATE_READER_WAITING)) {
      case _HSTORE_STATE_UNBLOCKED:
        // we previously were unblocked
        // make sure that we still need to block the reader (in case the write index moved while we were getting here)
        // then block while we're in reader-wait state
        if (*writeIndex() == ri) {
          waitForUpdate(waitState(), _HSTORE_STATE_READER_WAITING);
        }
        break;
      case _HSTORE_STATE_WRITER_WAITING:
        // we previously were in writer-wait state (this should practically never happen)
        // since we wait to read anyway, unblock the writer and try again
        uxchg(waitState(), _HSTORE_STATE_UNBLOCKED);
        wakeN(waitState(), 1);
        break;
      }
    }
    return value(*readIndex());
  }

  // get the next value in the queue if one is present, else null
  uint8_t* pollNext() {
    if (_HSTORE_UNLIKELY(*writeIndex() == *readIndex())) {
      return 0;
    } else {
      return value(*readIndex());
    }
  }

  // remove the next value from the queue (increment the read index)
  void pop() {
    uxchg(readIndex(), nextIndex(readIndex()));
  
    // when the reader advances, the writer can be unblocked
    if (_HSTORE_UNLIKELY(xchg(waitState(), _HSTORE_STATE_UNBLOCKED) == _HSTORE_STATE_WRITER_WAITING)) {
      wakeN(waitState(), 1);
    }
  }
};

// rpipe : a "read pipe" on top of readers
class rpipe {
private:
  reader* rq;
  const uint8_t* page;
  size_t         pagesz;
  uint32_t       offset;
public:
  rpipe(reader* rq) : rq(rq), page(0), pagesz(rq->config().valuesz - sizeof(uint32_t)), offset(0) {
    if (rq->config().valuesz <= sizeof(uint32_t)) {
      throw std::runtime_error("queue page size too small for use as shared memory pipe");
    }
  }

  // read a range of bytes out of the 'pipe' into a user-supplied buffer
  size_t read(uint8_t* dst, size_t sz, uint8_t* state) {
    if (!this->page) {
      this->page = this->rq->next();
    }
  
    size_t doff = 0;
  
    while (sz != 0 && this->page) {
      // how much can we copy out of the current page into the dest buffer?
      volatile uint32_t* pend      = (uint32_t*)(this->page + this->pagesz);
      size_t             rpagesz   = *pend & ~(0xFF << 24);
      size_t             availPage = rpagesz - this->offset;
      size_t             csz       = (sz < availPage) ? sz : availPage;
  
      // copy as much as we can
      memcpy(dst + doff, this->page + this->offset, csz);
  
      // step through this segment we've copied
      this->offset += csz;
      doff         += csz;
      sz           -= csz;
  
      // if we've hit the end of the page, mark it done and then try for the next one
      if (this->offset == rpagesz) {
        // wait for the producer to decide what state the page is in
        while ((*pend >> 24) == _HSTORE_PAGE_STATE_TENTATIVE);
        uint8_t ps = *pend >> 24;
        if (state) *state = ps;

        // if this page is just a continuation, continue reading
        // else we've terminated a transaction
        this->rq->pop();
        this->page   = (ps == _HSTORE_PAGE_STATE_CONT) ? this->rq->pollNext() : 0;
        this->offset = 0;
      }
    }
  
    return doff;
  }
};

// type/value serialization (guarded by compile-time predicates)
#define _HSTORE_TYCTOR_PRIM      ((int)0)
#define _HSTORE_TYCTOR_TVAR      ((int)2)
#define _HSTORE_TYCTOR_FIXEDARR  ((int)4)
#define _HSTORE_TYCTOR_ARR       ((int)5)
#define _HSTORE_TYCTOR_VARIANT   ((int)6)
#define _HSTORE_TYCTOR_STRUCT    ((int)7)
#define _HSTORE_TYCTOR_SIZE      ((int)11)
#define _HSTORE_TYCTOR_RECURSIVE ((int)13)

template <typename T, typename P = void>
  struct store {
  };
template <typename T>
  void w(const T& x, bytes* out) {
    out->insert(out->end(), (uint8_t*)&x, ((uint8_t*)&x) + sizeof(x));
  }
inline void ws(const char* x, bytes* out) {
  size_t n = strlen(x);
  w(n, out);
  out->insert(out->end(), x, x + n);
}
inline void ws(const std::string& x, bytes* out) {
  w((size_t)x.size(), out);
  out->insert(out->end(), x.begin(), x.end());
}
inline void ws(const bytes& x, bytes* out) {
  w((size_t)x.size(), out);
  out->insert(out->end(), x.begin(), x.end());
}

inline void encode_primty(const char* tn, bytes* out) {
  w(_HSTORE_TYCTOR_PRIM, out);
  ws(tn, out);
  w((bool)false, out);
}
#define _HSTORE_DEFINE_PRIMTYS(T, n) \
  template <> \
    struct store<T> { \
      typedef void can_memcpy; \
      static void        encode(bytes* out)          { encode_primty(n, out); } \
      static std::string describe()                  { return n; } \
      static size_t      size(const T&)              { return sizeof(T); } \
      static bool        write(wpipe& p, const T& x) { return p.write((const uint8_t*)&x, sizeof(x)); } \
    }

_HSTORE_DEFINE_PRIMTYS(bool,     "bool");
_HSTORE_DEFINE_PRIMTYS(uint8_t,  "byte");
_HSTORE_DEFINE_PRIMTYS(char,     "char");
_HSTORE_DEFINE_PRIMTYS(int16_t,  "short");
_HSTORE_DEFINE_PRIMTYS(uint16_t, "short");
_HSTORE_DEFINE_PRIMTYS(int32_t,  "int");
_HSTORE_DEFINE_PRIMTYS(uint32_t, "int");
_HSTORE_DEFINE_PRIMTYS(int64_t,  "long");
_HSTORE_DEFINE_PRIMTYS(uint64_t, "long");
#ifdef __clang__
_HSTORE_DEFINE_PRIMTYS(size_t, "long");
#endif
_HSTORE_DEFINE_PRIMTYS(float,    "float");
_HSTORE_DEFINE_PRIMTYS(double,   "double");

template <typename T, typename P = void>
  struct cannot_memcpy {
    typedef void type;
  };
template <typename T>
  struct cannot_memcpy<T, typename store<T>::can_memcpy> {
  };

template <typename ... Ts>
  struct store_tuple_types {
    static const size_t count = 0;
    static void        encode(size_t,bytes*)      { }
    static std::string describe()                 { return ""; }
    static size_t      size(const Ts&...)         { return 0; }
    static bool        write(wpipe&,const Ts&...) { return true; }
  };
template <typename T, typename ... Ts>
  struct store_tuple_types<T, Ts...> {
    static const size_t count = 1 + store_tuple_types<Ts...>::count;

    static void encode(size_t f, bytes* out) {
      char fn[32];
      snprintf(fn, sizeof(fn), ".f%lld", (unsigned long long)f);
      ws(fn, out);
      w((int)-1, out);
      store<T>::encode(out);

      store_tuple_types<Ts...>::encode(f+1, out);
    }
    static std::string describe() {
      return store<T>::describe() + "*" + store_tuple_types<Ts...>::describe();
    }
    static size_t size(const T& x, const Ts&... xs) {
      return store<T>::size(x) + store_tuple_types<Ts...>::size(xs...);
    }
    static bool write(wpipe& p, const T& x, const Ts&... xs) {
      return store<T>::write(p, x) && store_tuple_types<Ts...>::write(p, xs...);
    }
  };
template <size_t i, size_t e, typename ... Ts>
  struct storeTuple {
    static size_t size(const std::tuple<Ts...>& t) {
      return store<typename std::tuple_element<i, std::tuple<Ts...>>::type>::size(std::get<i>(t)) +
             storeTuple<i+1, e, Ts...>::size(t);
    }
    static bool write(wpipe& p, const std::tuple<Ts...>& t) {
      return store<typename std::tuple_element<i, std::tuple<Ts...>>::type>::write(p, std::get<i>(t)) &&
             storeTuple<i+1, e, Ts...>::write(p, t);
    }
  };
template <size_t e, typename ... Ts>
  struct storeTuple<e, e, Ts...> {
    static size_t size(const std::tuple<Ts...>&) {
      return 0;
    }
    static bool write(wpipe&, const std::tuple<Ts...>&) {
      return true;
    }
  };
template <typename ... Ts>
  struct store< std::tuple<Ts...> > {
    static const size_t arity = store_tuple_types<Ts...>::count;

    static void encode(bytes* out) {
      size_t localArity = arity; // required because 'arity' doesn't have an address

      if (localArity > 0) {
        w(_HSTORE_TYCTOR_STRUCT, out);
        w(localArity, out);
        store_tuple_types<Ts...>::encode(0, out);
      } else {
        encode_primty("unit", out);
      }
    }
    static std::string describe() {
      std::string x = store_tuple_types<Ts...>::describe();
      return (x.size() > 1) ? x.substr(0,x.size()-1) : x;
    }
    static size_t size(const std::tuple<Ts...>& t) {
      return storeTuple<0, std::tuple_size<std::tuple<Ts...>>::value, Ts...>::size(t);
    }
    static bool write(wpipe& p, const std::tuple<Ts...>& t) {
      return storeTuple<0, std::tuple_size<std::tuple<Ts...>>::value, Ts...>::write(p, t);
    }
  };

// very basic macro metaprogramming
#define _HSTORE_FIRST(a, ...) a
#define _HSTORE_SECOND(a, b, ...) b
#define _HSTORE_JOIN(a,b) a ## b
#define _HSTORE_IS_NEGATE(...) _HSTORE_SECOND(__VA_ARGS__, 0)
#define _HSTORE_NOT(x) _HSTORE_IS_NEGATE(_HSTORE_JOIN(_HSTORE__NOT_, x))
#define _HSTORE__NOT_0 NEGATE, 1
#define _HSTORE_BOOL(x) _HSTORE_NOT(_HSTORE_NOT(x))
#define _HSTORE_IF_ELSE(condition) _HSTORE__IF_ELSE(_HSTORE_BOOL(condition))
#define _HSTORE__IF_ELSE(condition) _HSTORE_JOIN(_HSTORE__IF_, condition)
#define _HSTORE__IF_1(...) __VA_ARGS__ _HSTORE__IF_1_ELSE
#define _HSTORE__IF_0(...)             _HSTORE__IF_0_ELSE
#define _HSTORE__IF_1_ELSE(...)
#define _HSTORE__IF_0_ELSE(...) __VA_ARGS__
#define _HSTORE_EMPTY()
#define _HSTORE_EVAL(...) _HSTORE_EVAL256(__VA_ARGS__)
#define _HSTORE_EVAL256(...) _HSTORE_EVAL128(_HSTORE_EVAL128(__VA_ARGS__))
#define _HSTORE_EVAL128(...) _HSTORE_EVAL64(_HSTORE_EVAL64(__VA_ARGS__))
#define _HSTORE_EVAL64(...) _HSTORE_EVAL32(_HSTORE_EVAL32(__VA_ARGS__))
#define _HSTORE_EVAL32(...) _HSTORE_EVAL16(_HSTORE_EVAL16(__VA_ARGS__))
#define _HSTORE_EVAL16(...) _HSTORE_EVAL8(_HSTORE_EVAL8(__VA_ARGS__))
#define _HSTORE_EVAL8(...) _HSTORE_EVAL4(_HSTORE_EVAL4(__VA_ARGS__))
#define _HSTORE_EVAL4(...) _HSTORE_EVAL2(_HSTORE_EVAL2(__VA_ARGS__))
#define _HSTORE_EVAL2(...) _HSTORE_EVAL1(_HSTORE_EVAL1(__VA_ARGS__))
#define _HSTORE_EVAL1(...) __VA_ARGS__
#define _HSTORE_DEFER2(m) m _HSTORE_EMPTY _HSTORE_EMPTY()()
#define _HSTORE_HAS_PARGS(...) _HSTORE_BOOL(_HSTORE_FIRST(_HSTORE__EOAP_ __VA_ARGS__)())
#define _HSTORE__EOAP_(...) _HSTORE_BOOL(_HSTORE_FIRST(_HSTORE__EOA_ __VA_ARGS__)())
#define _HSTORE__EOA_() 0
#define _HSTORE_MAP(f, VS...) _HSTORE_EVAL(_HSTORE_MAPP(f, VS))
#define _HSTORE_MAPP(f, H, T...)        \
  f H                                 \
  _HSTORE_IF_ELSE(_HSTORE_HAS_PARGS(T))(  \
    _HSTORE_DEFER2(_HSTORE__MAPP)()(f, T) \
  )(                                  \
  )
#define _HSTORE__MAPP() _HSTORE_MAPP

// support storing packed, reflective structs
#define HSTORE_FIELDC_SUCC(t,n) +1
#define HSTORE_FIELD_COUNT(FIELDS...) (0 _HSTORE_MAP(HSTORE_FIELDC_SUCC, FIELDS))

#define HSTORE_FIELDS_SUCC(t,n) +::hobbes::storage::store< t >::size(this-> n)
#define HSTORE_FIELD_SIZE(FIELDS...) (0 _HSTORE_MAP(HSTORE_FIELDS_SUCC, FIELDS))

#define HSTORE_FIELDW_SUCC(t,n) &&::hobbes::storage::store< t >::write(p, this-> n)
#define HSTORE_FIELD_WRITES(FIELDS...) (true _HSTORE_MAP(HSTORE_FIELDW_SUCC, FIELDS))

#define HSTORE_STRUCT_FIELD(t, n) t n;
#define HSTORE_STRUCT_FIELD_ENC(t, n) ::hobbes::storage::ws(#n, out); ::hobbes::storage::w((int)-1, out); ::hobbes::storage::store< t >::encode(out);
#define HSTORE_STRUCT_FIELD_DESC(t, n) + (", " #n " : " + ::hobbes::storage::store< t >::describe())

#define DEFINE_PACKED_HSTORE_STRUCT(T, FIELDS...) \
  struct T { \
    _HSTORE_MAP(HSTORE_STRUCT_FIELD, FIELDS) /* struct fields */ \
    typedef void is_packed_hstore_struct; /* identify this type as a packed struct */ \
    static void encode(::hobbes::storage::bytes* out) { \
      size_t localArity = HSTORE_FIELD_COUNT(FIELDS); \
      if (localArity > 0) { \
        ::hobbes::storage::w(_HSTORE_TYCTOR_STRUCT, out); \
        ::hobbes::storage::w(localArity, out); \
        _HSTORE_MAP(HSTORE_STRUCT_FIELD_ENC, FIELDS) \
      } else { \
        ::hobbes::storage::encode_primty("unit", out); \
      } \
    } \
    static std::string describe() { \
      return "{" + ( std::string("") _HSTORE_MAP(HSTORE_STRUCT_FIELD_DESC, FIELDS) ).substr(2) + "}"; \
    } \
  } __attribute__((packed))

template <typename T>
  struct store<T, typename T::is_packed_hstore_struct> {
    typedef void can_memcpy;
    static void        encode(bytes* out)          { T::encode(out); }
    static std::string describe()                  { return T::describe(); }
    static size_t      size(const T&)              { return sizeof(T); }
    static bool        write(wpipe& p, const T& x) { return p.write((const uint8_t*)&x, sizeof(x)); }
  };

// support storing standard, reflective structs
#define DEFINE_HSTORE_STRUCT(T, FIELDS...) \
  struct T { \
    _HSTORE_MAP(HSTORE_STRUCT_FIELD, FIELDS) /* struct fields */ \
    typedef void is_hstore_struct; /* identify this type as a struct */ \
    static void encode(::hobbes::storage::bytes* out) { \
      size_t localArity = HSTORE_FIELD_COUNT(FIELDS); \
      if (localArity > 0) { \
        ::hobbes::storage::w(_HSTORE_TYCTOR_STRUCT, out); \
        ::hobbes::storage::w(localArity, out); \
        _HSTORE_MAP(HSTORE_STRUCT_FIELD_ENC, FIELDS) \
      } else { \
        ::hobbes::storage::encode_primty("unit", out); \
      } \
    } \
    static std::string describe() { \
      return "{" + ( std::string("") _HSTORE_MAP(HSTORE_STRUCT_FIELD_DESC, FIELDS) ).substr(2) + "}"; \
    } \
    size_t size() const { \
      return HSTORE_FIELD_SIZE(FIELDS); \
    } \
    bool write(::hobbes::storage::wpipe& p) const { \
      return HSTORE_FIELD_WRITES(FIELDS); \
    } \
  }

template <typename T>
  struct store<T, typename T::is_hstore_struct> {
    static void        encode(bytes* out)          { T::encode(out); }
    static std::string describe()                  { return T::describe(); }
    static size_t      size(const T& x)            { return x.size(); }
    static bool        write(wpipe& p, const T& x) { return x.write(p); }
  };

// support storing standard pairs of types
template <typename U, typename V>
  struct store<std::pair<U,V>> {
    static void encode(bytes* out) {
      w(_HSTORE_TYCTOR_STRUCT, out);
      w((size_t)2, out);
      
      ws(".f0", out);
      w((int)-1, out);
      store<U>::encode(out);

      ws(".f1", out);
      w((int)-1, out);
      store<V>::encode(out);
    }
    static std::string describe() {
      return "(" + store<U>::describe() + "*" + store<V>::describe() + ")";
    }
    static size_t size(const std::pair<U, V>& x) {
      return store<U>::size(x.first) + store<V>::size(x.second);
    }
    static bool write(wpipe& p, const std::pair<U, V>& x) {
      return store<U>::write(p, x.first) && store<V>::write(p, x.second);
    }
  };

// support storing reflective enumerations
#define HSTORE_ENUM_CTOR_DEF(n) n ,
#define HSTORE_ENUM_CTOR_CTOR(n) static const SelfT n() { return SelfT(SelfT::Enum::n); }
#define HSTORE_ENUM_CTORC_SUCC(n) +1
#define HSTORE_ENUM_CTOR_STR(n) + "|" #n
#define HSTORE_ENUM_CTOR_ENCODE(n) \
  ::hobbes::storage::ws(#n, out); \
  ::hobbes::storage::w((uint32_t)(Enum :: n), out); \
  ::hobbes::storage::encode_primty("unit", out);

#define DEFINE_HSTORE_ENUM(T, CTORS...) \
  struct T { \
    typedef void is_hstore_enum; \
    enum class Enum : uint32_t { \
      _HSTORE_MAP(HSTORE_ENUM_CTOR_DEF, CTORS) \
      COUNT \
    }; \
    Enum value; \
    T() : value() { } \
    T(Enum v) : value(v) { } \
    T& operator=(Enum v) { this->value = v; return *this; } \
    operator Enum() { return this->value; } \
    typedef T SelfT; \
    _HSTORE_MAP(HSTORE_ENUM_CTOR_CTOR, CTORS) \
    static void encode(::hobbes::storage::bytes* out) { \
      ::hobbes::storage::w(_HSTORE_TYCTOR_VARIANT, out); \
      ::hobbes::storage::w((size_t)(0 _HSTORE_MAP(HSTORE_ENUM_CTORC_SUCC, CTORS)), out); \
      _HSTORE_MAP(HSTORE_ENUM_CTOR_ENCODE, CTORS); \
    } \
    static std::string describe() { \
      return (std::string("") _HSTORE_MAP(HSTORE_ENUM_CTOR_STR, CTORS)).substr(1); \
    } \
  }

template <typename T>
  struct store<T, typename T::is_hstore_enum> {
    typedef void can_memcpy;
    static void        encode(bytes* out)          { T::encode(out); }
    static std::string describe()                  { return T::describe(); }
    static size_t      size(const T&)              { return sizeof(T); }
    static bool        write(wpipe& p, const T& x) { return p.write((const uint8_t*)&x, sizeof(x)); }
  };

// support storing variants (with and without explicit constructor names)
#define DEFINE_HSTORE_VARIANT_GEN(T, VDECL, VCTORS, VCOPY, VDESTROY, CTORCOUNT, VENCODE, VDESC, VSIZE, VWRITE, VVISITCASE, VEQCASE, CTAGS, CDATA) \
  template <typename R> \
    struct T##Visitor { \
      VDECL \
    }; \
  struct T { \
    typedef void is_hstore_variant; \
    typedef T SelfT; \
    T() : tag(Enum::COUNT) { } \
    VCTORS \
    T(const T& rhs) : tag(rhs.tag) { \
      switch (this->tag) { \
      VCOPY \
      default: break; \
      } \
    } \
    ~T() { \
      switch (this->tag) { \
      VDESTROY \
      default: break; \
      } \
    } \
    T& operator=(const T& rhs) { \
      if (this == &rhs) return *this; \
      switch (this->tag) { \
      VDESTROY \
      default: break; \
      } \
      this->tag = rhs.tag; \
      switch (this->tag) { \
      VCOPY \
      default: break; \
      } \
      return *this; \
    } \
    static void encode(::hobbes::storage::bytes* out) { \
      ::hobbes::storage::w(_HSTORE_TYCTOR_VARIANT, out); \
      ::hobbes::storage::w((size_t)(0 CTORCOUNT), out); \
      VENCODE; \
    } \
    static std::string describe() { \
      return "|" + (std::string("") VDESC).substr(1) + "|"; \
    } \
    size_t wireSize() const { \
      switch (this->tag) { \
      VSIZE \
      default: return 0; \
      } \
    } \
    bool write(::hobbes::storage::wpipe& p) const { \
      switch (this->tag) { \
      VWRITE \
      default: return false; \
      } \
    } \
    template <typename R> \
      R visit(const T##Visitor<R>& v) const { \
        switch (this->tag) { \
        VVISITCASE \
        default: throw std::runtime_error("while deconstructing the " #T " variant, cannot decide payload type because tag is invalid"); \
        } \
      } \
    bool operator==(const T& rhs) const { \
      if (this->tag != rhs.tag) { \
        return false; \
      } else { \
        switch (this->tag) { \
        VEQCASE \
        default: return false; \
        } \
      } \
    } \
  private: \
    enum class Enum : uint32_t { \
      CTAGS \
      COUNT \
    }; \
    Enum tag; \
    union { \
      char data[1]; \
      CDATA \
    }; \
  }

// (with implicit ctor names)
#define HSTORE_VARIANT_CTOR(n, t) static SelfT n(const t & x) { SelfT r; r.tag = Enum::tag_##n; new (r.data) t(x); return r; }
#define HSTORE_VARIANT_CTOR_STR(n, t) + "," #n ":" + ::hobbes::storage::store< t >::describe()
#define HSTORE_VARIANT_CTOR_TAG(n, t) tag_##n,
#define HSTORE_VARIANT_SIZE_CASE(n, t)    case Enum::tag_##n: return sizeof(int) + ::hobbes::storage::store< t >::size(this->n##_data);
#define HSTORE_VARIANT_WRITE_CASE(n, t) case Enum::tag_##n: return ::hobbes::storage::store<int>::write(p, (int)this->tag) && ::hobbes::storage::store< t >::write(p, this->n##_data);
#define HSTORE_VARIANT_PCOPY(n, t)      case Enum::tag_##n: new (this->data) t(rhs.n##_data); break;
#define HSTORE_VARIANT_PDESTROY(n, t)   case Enum::tag_##n: { typedef t __DT; ((__DT*)&this->n##_data)->~__DT(); } break;
#define HSTORE_VARIANT_SUCC(n, t) +1
#define HSTORE_VARIANT_CTOR_OPAQUEDATA(n, t) t n##_data;
#define HSTORE_VARIANT_CTOR_ENCODE(n, t) \
  ::hobbes::storage::ws(#n, out); \
  ::hobbes::storage::w((uint32_t)Enum::tag_##n, out); \
  ::hobbes::storage::store< t >::encode(out);

#define HSTORE_VARIANT_VDECL(n, t) virtual R n(const t & x) const = 0;
#define HSTORE_VARIANT_VCASE(n, t) case Enum::tag_##n: return v. n (this->n##_data);
#define HSTORE_VARIANT_EQCASE(n, t) case Enum::tag_##n: return (this->n##_data == rhs.n##_data);

#define DEFINE_HSTORE_VARIANT(T, CTORS...) \
  DEFINE_HSTORE_VARIANT_GEN(T, _HSTORE_MAP(HSTORE_VARIANT_VDECL, CTORS), _HSTORE_MAP(HSTORE_VARIANT_CTOR, CTORS), _HSTORE_MAP(HSTORE_VARIANT_PCOPY, CTORS), _HSTORE_MAP(HSTORE_VARIANT_PDESTROY, CTORS), _HSTORE_MAP(HSTORE_VARIANT_SUCC, CTORS), _HSTORE_MAP(HSTORE_VARIANT_CTOR_ENCODE, CTORS), _HSTORE_MAP(HSTORE_VARIANT_CTOR_STR, CTORS), _HSTORE_MAP(HSTORE_VARIANT_SIZE_CASE, CTORS), _HSTORE_MAP(HSTORE_VARIANT_WRITE_CASE, CTORS), _HSTORE_MAP(HSTORE_VARIANT_VCASE, CTORS), _HSTORE_MAP(HSTORE_VARIANT_EQCASE, CTORS), _HSTORE_MAP(HSTORE_VARIANT_CTOR_TAG, CTORS), _HSTORE_MAP(HSTORE_VARIANT_CTOR_OPAQUEDATA, CTORS))

// (with explicit ctor names)
#define HSTORE_VARIANT_LBL_CTOR(n, lbl, t) static SelfT n(const t & x) { SelfT r; r.tag = Enum::tag_##n; new (r.data) t(x); return r; }
#define HSTORE_VARIANT_LBL_CTOR_STR(n, lbl, t) + "," #n ":" + ::hobbes::storage::store< t >::describe()
#define HSTORE_VARIANT_LBL_CTOR_TAG(n, lbl, t) tag_##n,
#define HSTORE_VARIANT_LBL_SIZE_CASE(n, lbl, t)    case Enum::tag_##n: return sizeof(int) + ::hobbes::storage::store< t >::size(this->n##_data);
#define HSTORE_VARIANT_LBL_WRITE_CASE(n, lbl, t) case Enum::tag_##n: return ::hobbes::storage::store<int>::write(p, (int)this->tag) && ::hobbes::storage::store< t >::write(p, this->n##_data);
#define HSTORE_VARIANT_LBL_PCOPY(n, lbl, t)      case Enum::tag_##n: new (this->data) t(rhs.n##_data); break;
#define HSTORE_VARIANT_LBL_PDESTROY(n, lbl, t)   case Enum::tag_##n: { typedef t __DT; ((__DT*)&this->n##_data)->~__DT(); } break;
#define HSTORE_VARIANT_LBL_SUCC(n, lbl, t) +1
#define HSTORE_VARIANT_LBL_CTOR_OPAQUEDATA(n, lbl, t) t n##_data;
#define HSTORE_VARIANT_LBL_CTOR_ENCODE(n, lbl, t) \
  ::hobbes::storage::ws(#lbl, out); \
  ::hobbes::storage::w((uint32_t)Enum::tag_##n, out); \
  ::hobbes::storage::store< t >::encode(out);

#define HSTORE_VARIANT_LBL_VDECL(n, lbl, t) virtual R n(const t & x) const = 0;
#define HSTORE_VARIANT_LBL_VCASE(n, lbl, t) case Enum::tag_##n: return v. n (this->n##_data);
#define HSTORE_VARIANT_LBL_EQCASE(n, lbl, t) case Enum::tag_##n: return (this->n##_data == rhs.n##_data);

#define DEFINE_HSTORE_VARIANT_WITH_LABELS(T, CTORS...) \
  DEFINE_HSTORE_VARIANT_GEN(T, _HSTORE_MAP(HSTORE_VARIANT_LBL_VDECL, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_CTOR, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_PCOPY, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_PDESTROY, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_SUCC, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_CTOR_ENCODE, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_CTOR_STR, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_SIZE_CASE, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_WRITE_CASE, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_VCASE, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_EQCASE, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_CTOR_TAG, CTORS), _HSTORE_MAP(HSTORE_VARIANT_LBL_CTOR_OPAQUEDATA, CTORS))

template <typename T>
  struct store<T, typename T::is_hstore_variant> {
    static void        encode(bytes* out)          { T::encode(out); }
    static std::string describe()                  { return T::describe(); }
    static size_t      size(const T& x)            { return x.wireSize(); }
    static bool        write(wpipe& p, const T& x) { return x.write(p); }
  };

// store recursive types
struct recursion {
  void* value;
  recursion(void* x) : value(x) { }
};

template <typename T>
  struct recursive {
    typedef T value_type;
    value_type value;
    recursive(const value_type& x) : value(x) { }
  };

template <>
  struct store<recursion> {
    static void encode(bytes* out) {
      w(_HSTORE_TYCTOR_TVAR, out);
      ws("x", out);
    }
    static std::string describe() {
      return "x";
    }

    typedef size_t (*RecSizeF)(const recursion&);
    static RecSizeF& sizeF() {
      thread_local RecSizeF fn = 0;
      return fn;
    }
    static size_t size(const recursion& x) {
      return sizeF()(x);
    }

    typedef bool (*RecWriteF)(wpipe&, const recursion&);
    static RecWriteF& writeF() {
      thread_local RecWriteF fn = 0;
      return fn;
    }
    static bool write(wpipe& p, const recursion& x) {
      return writeF()(p, x);
    }
  };

template <typename T>
  struct store<recursive<T>> {
    static void encode(bytes* out) {
      w(_HSTORE_TYCTOR_RECURSIVE, out);
      ws("x", out);
      store<T>::encode(out);
    }
    static std::string describe() {
      return "^x." + store<T>::describe();
    }
    static size_t recSize(const recursion& x) {
      return size(*((recursive<T>*)x.value));
    }
    static size_t size(const recursive<T>& x) {
      typedef typename store<recursion>::RecSizeF RSF;
      RSF sf = store<recursion>::sizeF();
      store<recursion>::sizeF() = &store<recursive<T>>::recSize;
      size_t r = store<T>::size(x.value);
      store<recursion>::sizeF() = sf;
      return r;
    }

    static bool recWrite(wpipe& p, const recursion& x) {
      return write(p, *((recursive<T>*)x.value));
    }
    static bool write(wpipe& p, const recursive<T>& x) {
      typedef typename store<recursion>::RecWriteF RWF;
      RWF sf = store<recursion>::writeF();
      store<recursion>::writeF() = &store<recursive<T>>::recWrite;
      bool r = store<T>::write(p, x.value);
      store<recursion>::writeF() = sf;
      return r;
    }
  };

// store unit
struct unit {
  unit() { }
  bool operator==(const unit&) const { return true; }
  bool operator< (const unit&) const { return false; }
};

template <>
  struct store<unit> {
    static void encode(bytes* out) { encode_primty("unit", out); }
    static std::string describe() { return "()"; }
    static size_t size(const unit&) { return 0; }
    static bool write(wpipe&, const unit&) { return true; }
  };

// store opaque type aliases
#define DEFINE_HSTORE_TYPE_ALIAS(_ATY, _REPTY) \
  struct _ATY { \
    typedef void is_hstore_alias; \
    typedef _REPTY type; \
    static const char* name() { return #_ATY; } \
    inline operator _REPTY() { return this->value; } \
    _REPTY value; \
    _ATY() : value() { } \
    _ATY(const _REPTY& x) : value(x) { } \
    _ATY(const _ATY& x) : value(x.value) { } \
    _ATY& operator=(const _ATY& x) { this->value = x.value; return *this; } \
  }

template <typename T>
  struct store<T, typename T::is_hstore_alias> {
    static void        encode(bytes* out)          { w(_HSTORE_TYCTOR_PRIM, out); ws(T::name(), out); w((bool)true, out); store<typename T::type>::encode(out); }
    static std::string describe()                  { return T::name(); }
    static size_t      size(const T& x)            { return store<typename T::type>::size(x.value); }
    static bool        write(wpipe& p, const T& x) { return store<typename T::type>::write(p, x.value); }
  };

// store fixed-length arrays
template <typename T, size_t N>
  struct fixedArrTyDesc {
    static void encode(bytes* out) {
      w(_HSTORE_TYCTOR_FIXEDARR, out);
      store<T>::encode(out);
      w(_HSTORE_TYCTOR_SIZE, out);
      w((long)N, out);
    }

    static std::string describe() {
      char nf[32];
      snprintf(nf, sizeof(nf), "%lld", (unsigned long long)N);
      return "[:" + store<T>::describe() + "|" + std::string(nf) + ":]";
    }
  };

template <typename T, size_t N>
  struct fixedArrMemcpyWrite {
    static size_t size(const T (&v)[N]) {
      return sizeof(T)*N;
    }
    static bool write(wpipe& p, const T (&v)[N]) {
      return p.write((const uint8_t*)v, sizeof(T)*N);
    }
  };

template <typename T, size_t N>
  struct fixedArrIterWrite {
    static size_t size(const T (&v)[N]) {
      size_t s = 0;
      for (size_t i = 0; i < N; ++i) {
        s += store<T>::size(v[i]);
      }
      return s;
    }
    static bool write(wpipe& p, const T (&v)[N]) {
      for (size_t i = 0; i < N; ++i) {
        if (!store<T>::write(p, v[i])) {
          return false;
        }
      }
      return true;
    }
  };

template <typename T, size_t N>
  struct store<T[N], typename store<T>::can_memcpy> : public fixedArrTyDesc<T,N>, fixedArrMemcpyWrite<T,N> { };

template <typename T, size_t N>
  struct store<T[N], typename cannot_memcpy<T>::type> : public fixedArrTyDesc<T,N>, fixedArrIterWrite<T,N> { };

// support storage of vectors
template <typename T>
  struct store<std::vector<T>, typename store<T>::can_memcpy> {
    static void encode(bytes* out) { w(_HSTORE_TYCTOR_ARR, out); store<T>::encode(out); }
    static std::string describe() { return "[" + store<T>::describe() + "]"; }
    static size_t size(const std::vector<T>& xs) { return sizeof(size_t) + (xs.size() * sizeof(T)); }
    static bool write(wpipe& p, const std::vector<T>& xs) { size_t n = xs.size(); return store<size_t>::write(p, n) && p.write((const uint8_t*)&xs[0], n * sizeof(T)); }
  };

template <typename T>
  struct store<std::vector<T>, typename cannot_memcpy<T>::type> {
    static void encode(bytes* out) { w(_HSTORE_TYCTOR_ARR, out); store<T>::encode(out); }
    static std::string describe() { return "[" + store<T>::describe() + "]"; }
    static size_t size(const std::vector<T>& xs) {
      size_t t = sizeof(size_t);
      for (const auto& x : xs) {
        t += store<T>::size(x);
      }
      return t;
    }
    static bool write(wpipe& p, const std::vector<T>& xs) {
      size_t n = xs.size();
      if (!store<size_t>::write(p, n)) {
        return false;
      }
      for (const auto& x : xs) {
        if (!store<T>::write(p, x)) {
          return false;
        }
      }
      return true;
    }
  };

// support storage of strings
template <>
  struct store<const char*> {
    static void encode(bytes* out) { w(_HSTORE_TYCTOR_ARR, out); store<char>::encode(out); }
    static std::string describe() { return "[char]"; }
    static size_t size(const char* s) { return sizeof(size_t) + strlen(s); }
    static bool write(wpipe& p, const char* s) { size_t n = strlen(s); return store<size_t>::write(p, n) && p.write((const uint8_t*)s, n); }
  };
template <>
  struct store<std::string> {
    static void encode(bytes* out) { w(_HSTORE_TYCTOR_ARR, out); store<char>::encode(out); }
    static std::string describe() { return "[char]"; }
    static size_t size(const std::string& s) { return sizeof(size_t) + s.size(); }
    static bool write(wpipe& p, const std::string& s) { size_t n = s.size(); return store<size_t>::write(p, n) && p.write((const uint8_t*)s.data(), n); }
  };

// make a function for describing store payload types
template <typename ... Ts>
  struct hstore_payload_types {
    static const size_t count = 0;
    static void encode(size_t,bytes*) {}
    static std::string describe() { return ""; }
  };
template <typename T, typename ... Ts>
  struct hstore_payload_types<T,Ts...> {
    static const size_t count = 1 + hstore_payload_types<Ts...>::count;
    static void encode(size_t f, bytes* out) {
      char fn[32];
      snprintf(fn, sizeof(fn), ".f%lld", (unsigned long long)f);
      ws(fn, out);
      w((int)-1, out);
      store<T>::encode(out);
      hstore_payload_types<Ts...>::encode(f+1, out);
    }
    static std::string describe() {
      return "*" + store<T>::describe() + hstore_payload_types<Ts...>::describe();
    }
  };
template <typename ... Ts>
  hstore_payload_types<Ts...> makePayloadTypes(const Ts&...) {
    return hstore_payload_types<Ts...>();
  }

typedef void (*tydescfn)(bytes*,std::string*);
template <typename TyList>
  void makeTyDescF(bytes* e, std::string* d) {
    if (e) {
      size_t localArity = TyList::count;

      if (localArity > 0) {
        w(_HSTORE_TYCTOR_STRUCT, e);
        w(localArity, e);
        TyList::encode(0, e);
      } else {
        encode_primty("unit", e);
      }
    }

    if (d) {
      *d = TyList::describe().substr(1);
    }
  }

// compile-time strings -- used to uniquely identify log statements
template <size_t N>
  constexpr char at(size_t i, const char (&s)[N]) {
    return (i < N) ? s[i] : '\0';
  }
template <size_t N>
  constexpr size_t at8S(size_t i, size_t k, const char (&s)[N]) {
    return (k==8) ? 0 : ((((size_t)at(i+k,s))<<(8*k))|at8S(i,k+1,s));
  }
template <size_t N>
  constexpr size_t at8(size_t i, const char (&s)[N]) {
    return at8S(i, 0, s);
  }
template <size_t... pcs>
  struct strpack {
    static std::string str() {
      constexpr size_t msg[] = {pcs...};
      static_assert(msg[(sizeof(msg)/sizeof(msg[0]))-1] == 0, "compile-time string larger than internal max limit (this limit can be bumped in storage.H)");
      return std::string((const char*)msg);
    }
  };
#define _HSTORE_TSTR32(i,s)   ::hobbes::storage::at8(i+(8*0),s),::hobbes::storage::at8(i+(8*1),s),::hobbes::storage::at8(i+(8*2),s),::hobbes::storage::at8(i+(8*3),s)
#define _HSTORE_TSTR128(i,s)  _HSTORE_TSTR32(i+(32*0),s),_HSTORE_TSTR32(i+(32*1),s),_HSTORE_TSTR32(i+(32*2),s),_HSTORE_TSTR32(i+(32*3),s)
#define _HSTORE_TSTR512(i,s)  _HSTORE_TSTR128(i+(128*0),s),_HSTORE_TSTR128(i+(128*1),s),_HSTORE_TSTR128(i+(128*2),s),_HSTORE_TSTR128(i+(128*3),s)
#define _HSTORE_TSTR1024(i,s) _HSTORE_TSTR512(i+(512*0),s),_HSTORE_TSTR512(i+(512*1),s)
#define _HSTORE_TSTR(s)       ::hobbes::storage::strpack<_HSTORE_TSTR1024(0,s)>

template <uint32_t* X>
  uint32_t forceRegistration() { return *X; }
template <typename Group, Group* G, typename File, uint32_t Line, typename StmtName, size_t Flags, typename FormatStr, typename ArgTyList>
  struct StorageStatement {
    static uint32_t id;
    StorageStatement() { forceRegistration<&id>(); }
  };
template <typename Group, Group* G, typename File, uint32_t Line, typename StmtName, size_t Flags, typename FormatStr, typename ArgTyList>
  uint32_t StorageStatement<Group, G, File, Line, StmtName, Flags, FormatStr, ArgTyList>::id = G->allocateStorageStatement(File::str(), Line, StmtName::str(), Flags, FormatStr::str(), &makeTyDescF<ArgTyList>);

// allow static registration of storage points
enum CommitMethod {
  AutoCommit = 0,
  ManualCommit
};

template <CommitMethod cm>
struct StorageGroup {
  struct StmtData {
    tydescfn    tdesc;
    uint64_t    flags;
    std::string fmtstr;
    std::string file;
    uint32_t    line;
    uint32_t    id;
  };
  typedef std::map<std::string, StmtData> StorageStatements;

  StorageStatements* statements;
  const char*        name;
  PipeQOS            qos;
  size_t             mempages;
  writer*            pipeData;
  wpipe*             pipe;

  ~StorageGroup() {
    delete this->statements;
    delete this->pipeData;
    delete this->pipe;
  }

  void prepareMeta(bytes* meta) {
    if (!this->statements) return; // if nothing to record, nothing to prepare

    w(HSTORE_VERSION, meta);
    w((int)this->qos, meta);
    w((int)cm, meta);

    // write the count of storage statements, then each statement's static data
    w((uint32_t)this->statements->size(), meta);
    for (auto s : *this->statements) {
      ws(s.first,         meta);
      w (s.second.flags,  meta);
      ws(s.second.fmtstr, meta);
      ws(s.second.file,   meta);
      w (s.second.line,   meta);
      w (s.second.id,     meta);
      
      bytes td;
      s.second.tdesc(&td,0);
      ws(td, meta);
    }
  }

  wpipe& out() {
    assert(this->pipe);
    return *this->pipe;
  }

  inline void init() {
    assert(!this->pipe);

    bytes meta;
    prepareMeta(&meta);
    
    size_t pagesz = sysconf(_SC_PAGESIZE);
    size_t pagec  = 1 + (meta.size() / pagesz) + std::max<size_t>(this->mempages, 10);

    this->pipeData = new writer(meta, sharedMemName(this->name), pagesz, pagec);
    this->pipe     = new wpipe(this->pipeData, this->qos);
  }

  inline void commit() {
    out().commit();
  }

  uint32_t allocateStorageStatement(const std::string& file, uint32_t line, const std::string& name, size_t flags, const std::string& fmtstr, tydescfn tdesc) {
    if (!this->statements) {
      this->statements = new StorageStatements();
    }

    auto s = this->statements->find(name);
    if (s != this->statements->end()) {
      // this statement has already been added
      // make sure that all identifying parameters are consistent
      bytes ety, xty;
      s->second.tdesc(&ety,0);
      tdesc(&xty,0);
      if (ety != xty) {
        std::string etd, xtd;
        s->second.tdesc(0,&etd);
        tdesc(0,&xtd);

        std::cerr
          << "fatal error: incompatible types for store statement '" << name << "' between:\n"
          << "  " << s->second.file << ":" << s->second.line << " (" << xtd << ")\n"
          << "and\n"
          << "  " << file << ":" << line << " (" << etd << ")\n"
          << std::endl;

        exit(-1);
      }

      if (s->second.fmtstr != fmtstr) {
        std::cerr
          << "fatal error: incompatible format strings for store statement '" << name << "' between:\n"
          << "  " << s->second.file << ":" << s->second.line << " (" << s->second.fmtstr << ")\n"
          << "and\n"
          << "  " << file << ":" << line << " (" << fmtstr << ")\n"
          << std::endl;

        exit(-1);
      }

      if (s->second.flags != flags) {
        std::cerr
          << "fatal error: incompatible storage flags for store statement '" << name << "' between:\n"
          << "  " << s->second.file << ":" << s->second.line << " (" << s->second.flags << ")\n"
          << "and\n"
          << "  " << file << ":" << line << " (" << flags << ")\n"
          << std::endl;

        exit(-1);
      }

      return s->second.id;
    }

    // this must be a new statement, give it a new ID and put it in the group list
    StmtData d;
    d.tdesc  = tdesc;
    d.flags  = flags;
    d.fmtstr = fmtstr;
    d.file   = file;
    d.line   = line;
    d.id     = (uint32_t)this->statements->size();

    (*this->statements)[name] = d;
    return d.id;
  }
};

// write a storage statement with payload into a pipe
template <typename ... Ts>
  struct serialize_values {
    static size_t size(const Ts&...) { return 0; }
    static bool write(wpipe&, const Ts&...) { return true; }
  };
template <typename T, typename ... Ts>
  struct serialize_values<T, Ts...> {
    static size_t size(const T& x, const Ts&... xs) {
      return store<T>::size(x) + serialize_values<Ts...>::size(xs...);
    }
    static bool write(wpipe& p, const T& x, const Ts&... xs) {
      return store<T>::write(p, x) && serialize_values<Ts...>::write(p, xs...);
    }
  };

template <typename ... Ts>
  inline bool write(StorageGroup<AutoCommit>* g, uint32_t id, const Ts&... xs) {
    wpipe& p = g->out();
    if (!p.hasSpaceFor(sizeof(uint32_t) + serialize_values<Ts...>::size(xs...))) {
      g->commit();
    }
    return store<uint32_t>::write(p, id) &&
           serialize_values<Ts...>::write(p, xs...);
  }
template <typename ... Ts>
  inline bool write(StorageGroup<ManualCommit>* g, uint32_t id, const Ts&... xs) {
    wpipe& p = g->out();
    return store<uint32_t>::write(p, id) &&
           serialize_values<Ts...>::write(p, xs...);
  }

// validate arity in log format strings (prevent format strings referring to payload variables that don't exist)
template <size_t N>
  static constexpr size_t readInt(const char (&fmt)[N], size_t i, size_t e, size_t n) {
    return (i == e) ? n : readInt(fmt, i+1, e, (n*10)+(fmt[i]-'0'));
  }

static constexpr size_t maxV(size_t x, size_t y) {
  return (x < y) ? y : x;
}

template <size_t N>
  static constexpr size_t maxVarRefS(const char (&fmt)[N], size_t i, size_t s, size_t vri, size_t maxvr) {
    return  (i >= N) ? maxvr
           :(s == 0) ? ((fmt[i] == '\\') ? maxVarRefS(fmt, i+2, 0, 0, maxvr)
                       :(fmt[i] == '$')  ? maxVarRefS(fmt, i+1, 1, i+1, maxvr)
                       :                   maxVarRefS(fmt, i+1, 0, 0,   maxvr))
           :           ((fmt[i] >= '0' && fmt[i] <= '9') ?
                           maxVarRefS(fmt, i+1, 1, vri, maxvr)
                         : maxVarRefS(fmt, i+1, 0, 0,   maxV(maxvr, 1+readInt(fmt, vri, i, 0))));
  }

template <size_t N>
  static constexpr size_t maxVarRef(const char (&fmt)[N]) {
    return maxVarRefS(fmt, 0, 0, 0, 0);
  }

// create statement groups
#define DECLARE_STORAGE_GROUP(NAME, cm)            extern ::hobbes::storage::StorageGroup<cm> NAME
#define DEFINE_STORAGE_GROUP(NAME, pagec, qos, cm) ::hobbes::storage::StorageGroup<cm> NAME = { 0, #NAME, qos, pagec, 0, 0 }

// record some data
#define APPLY_HSTORE_STMT(GROUP, NAME, FLAGS, FMTSTR, ARGS...) \
  ::hobbes::storage::write(&GROUP, ({static_assert(::hobbes::storage::maxVarRef(FMTSTR) <= decltype(::hobbes::storage::makePayloadTypes(ARGS))::count, "Log format string and payload arity mismatch"); ::hobbes::storage::StorageStatement<decltype(GROUP),&GROUP,_HSTORE_TSTR(__FILE__),__LINE__,_HSTORE_TSTR(#NAME),FLAGS,_HSTORE_TSTR(FMTSTR),decltype(::hobbes::storage::makePayloadTypes(ARGS))>::id;}), ## ARGS)

#define HSTORE(GROUP, NAME, ARGS...)       APPLY_HSTORE_STMT(GROUP, NAME, 0,     "", ## ARGS)
#define HLOG(GROUP, NAME, FMTSTR, ARGS...) APPLY_HSTORE_STMT(GROUP, NAME, 1, FMTSTR, ## ARGS)

// run a process to read transaction data
struct statement {
  std::string name;
  uint64_t    flags;
  std::string fmtstr;
  std::string file;
  uint32_t    line;
  uint32_t    id;
  bytes       type;

  inline bool isLog() const { return (this->flags & 1) == 1; }
};
typedef std::vector<statement> statements;

inline size_t rs(const reader::MetaData& md, size_t o, size_t n, uint8_t* b) {
  if (o + n <= md.second) {
    memcpy(b, md.first + o, n);
    return o + n;
  } else {
    return md.second;
  }
}

template <typename T>
  size_t r(const reader::MetaData& md, size_t o, T* t) {
    return rs(md, o, sizeof(T), (uint8_t*)t);
  }

inline size_t rs(const reader::MetaData& md, size_t o, std::string* s) {
  size_t n = 0;
  o = r(md, o, &n);
  s->resize(n);
  return rs(md, o, n, (uint8_t*)s->data());
}

inline size_t rs(const reader::MetaData& md, size_t o, bytes* s) {
  size_t n = 0;
  o = r(md, o, &n);
  s->resize(n);
  return rs(md, o, n, &(*s)[0]);
}

class Transaction {
public:
  Transaction(const uint8_t* data, size_t datasz) : data(data), datasz(datasz), i(0) {
  }

  bool canRead(size_t x) const {
    return (this->i+x) <= this->datasz;
  }

  const uint8_t* ptr() const {
    return this->data + this->i;
  }

  void skip(size_t d) {
    this->i += d;
  }

  template <typename T>
    const T* read() {
      auto p = (const T*)ptr();
      skip(sizeof(T));
      return p;
    }

  size_t size() const {
    return this->datasz;
  }
private:
  const uint8_t* data;
  size_t         datasz;
  size_t         i;
};

inline void runReadProcess(const QueueConnection& qc, const std::function<std::function<void(Transaction&)>(PipeQOS, CommitMethod, const statements&)>& initF) {
  reader rd(qc);
  rpipe  p(&rd);

  // initialize
  reader::MetaData md = rd.meta();

  uint32_t hstoreVersion = 0;
  size_t o = r(md, 0, &hstoreVersion);
  if (hstoreVersion != HSTORE_VERSION) {
    throw std::runtime_error("Can't read storage data from incompatible process");
  }

  // read group flags/settings
  int qos, cm;
  o = r(md, o, &qos);
  o = r(md, o, &cm);

  // read all storage statements
  uint32_t n = 0;
  o = r(md, o, &n);
  statements ss;
  ss.reserve(n);
  for (size_t i = 0; i < n; ++i) {
    statement s;
    o = rs(md, o, &s.name);
    o = r (md, o, &s.flags);
    o = rs(md, o, &s.fmtstr);
    o = rs(md, o, &s.file);
    o = r (md, o, &s.line);
    o = r (md, o, &s.id);
    o = rs(md, o, &s.type);
    ss.push_back(s);
  }

  auto txnF = initF((PipeQOS)qos, (CommitMethod)cm, ss);

  // read transactions and call back into user code
  bytes txn;
  while (true) {
    static const size_t blockSize = 1024;

    size_t i = txn.size();
    txn.resize(txn.size() + blockSize);

    uint8_t txnFlag = 0;
    txn.resize(txn.size() - (blockSize - p.read(&txn[i], blockSize, &txnFlag)));

    switch (txnFlag) {
    case _HSTORE_PAGE_STATE_ROLLBACK:
      txn.clear();
      break;
    case _HSTORE_PAGE_STATE_COMMIT: {
      Transaction txnr(&txn[0], txn.size());
      txnF(txnr);
      txn.clear();
      break;
    }
    default:
      break;
    }
  }
}

inline void runReadProcess(const std::string& qname, const std::function<std::function<void(Transaction&)>(PipeQOS, CommitMethod, const statements&)>& initF) {
  runReadProcess(consumeGroup(qname), initF);
}

}}

#endif
