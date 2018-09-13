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
#include <array>
#include <string>
#include <string.h>
#include <tuple>
#include <functional>
#include <iostream>
#include <sstream>
#include <atomic>
#include <stdexcept>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/un.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>

namespace hobbes { namespace storage { namespace internal {

// a few things have to be OS-specific here
//  * available waiting strategy in shared memory
//  * default waiting strategy
#if defined(__APPLE__) && defined(__MACH__)

// macOS doesn't support this flag to mmap so we can just 0 it out
#ifndef MAP_POPULATE
#define MAP_POPULATE 0
#endif

namespace spin {

#if defined(CLOCK_REALTIME)
inline long poll_tickNS() {
  timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
    return ts.tv_sec * 1000000000L + ts.tv_nsec;
  } else {
    return 0;
  }
}
#else
inline long poll_tickNS() {
  struct timeval t;
  if (gettimeofday(&t, 0) == 0) {
    return ((t.tv_sec*1000000)+t.tv_usec)*1000;
  } else {
    return 0;
  }
}
#endif

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
static inline void waitForUpdate(volatile uint32_t* p, int eqV, size_t timeoutNS, const std::function<void()>& timeoutF) {
  if (timeoutNS == 0) {
    waitForUpdate(p, eqV);
    return;
  }

  long t0 = poll_tickNS();

  while (true) {
    for (size_t c = 0; c < 4096; ++c) {
      if (*p != eqV) {
        return;
      }
    }

    long t1 = poll_tickNS();
    if ((t1-t0) >= timeoutNS) {
      timeoutF();
      t0 = t1;
    }
    usleep(500);
  }
}
static inline void wakeN(volatile uint32_t* p, int c) {
  // assume the waiter is polling, nothing to do
}

} // namespace spin
#else
#include <linux/futex.h>
#include <sys/syscall.h>

namespace spin {

#if defined(CLOCK_REALTIME)
inline long poll_tickNS() {
  timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
    return ts.tv_sec * 1000000000L + ts.tv_nsec;
  } else {
    return 0;
  }
}
#else
inline long poll_tickNS() {
  struct timeval t;
  if (gettimeofday(&t, 0) == 0) {
    return ((t.tv_sec * 1000000) + t.tv_usec) * 1000;
  } else {
    return 0;
  }
}
#endif

static inline void waitForUpdate(volatile uint32_t* p, int eqV) {
  while (true) {
    for (size_t c = 0; c < 4096; ++c) {
      if (*p != uint32_t(eqV)) {
        return;
      }
    }
  }
}
static inline void waitForUpdate(volatile uint32_t* p, int eqV, size_t timeoutNS, const std::function<void()>& timeoutF) {
  if (timeoutNS == 0) {
    waitForUpdate(p, eqV);
    return;
  }

  long t0 = poll_tickNS();

  while (true) {
    for (size_t c = 0; c < 4096; ++c) {
      if (*p != uint32_t(eqV)) {
        return;
      }
    }

    long t1 = poll_tickNS();
    if ((t1-t0) >= long(timeoutNS)) {
      timeoutF();
      t0 = t1;
    }
  }
}
static inline void wakeN(volatile uint32_t* p, int c) {
  // assume the waiter is polling, nothing to do
}

} // namespace spin

namespace futex {

static inline long sys_futex(volatile uint32_t* p, int op, int v, struct timespec* timeout, void* p2, int v2) {
  return syscall(SYS_futex, p, op, v, timeout, p2, v2);
}
static inline void waitForUpdate(volatile uint32_t* p, int eqV) {
  sys_futex(p, FUTEX_WAIT, eqV, 0, 0, 0);
}
static inline void waitForUpdate(volatile uint32_t* p, int eqV, size_t timeoutNS, const std::function<void()>& timeoutF) {
  if (timeoutNS == 0) {
    waitForUpdate(p, eqV);
    return;
  }

  struct timespec ts;
  ts.tv_sec  = timeoutNS / 1000000000L;
  ts.tv_nsec = timeoutNS % 1000000000L;

  bool c = true;
  while (c) {
    int r = sys_futex(p, FUTEX_WAIT, eqV, &ts, 0, 0);
        c = r < 0 && errno == ETIMEDOUT;

    if (c) {
      timeoutF();
    }
  }
}
static inline void wakeN(volatile uint32_t* p, int c) {
  sys_futex(p, FUTEX_WAKE, c, 0, 0, 0);
}

} // namespace futex
#endif

#if defined (__APPLE__) && defined (__MACH__)
namespace platform = spin;
#else
namespace platform = futex;
#endif

}}}

namespace hobbes { namespace storage {

typedef void (*WaitFn)(volatile uint32_t*, int, size_t, const std::function<void()>&);
typedef void (*WakeFn)(volatile uint32_t*, int);

enum WaitPolicy {
  Platform = 0,
  Spin,
};

static inline WaitFn waitFn(const WaitPolicy s) {
  switch (s) {
    case Spin: return &internal::spin::waitForUpdate;
    case Platform:
    default: return &internal::platform::waitForUpdate;
  }
}

static inline WakeFn wakeFn(const WaitPolicy s) {
  switch (s) {
    case Spin: return &internal::spin::wakeN;
    case Platform:
    default: return &internal::platform::wakeN;
  }
}

}}

namespace hobbes { namespace storage {

#define HSTORE_VERSION static_cast<uint32_t>(0x00020000)

typedef std::vector<uint8_t> bytes;

// write transactions into shared memory
#define PRIV_HSTORE_LIKELY(x)   __builtin_expect((x),1)
#define PRIV_HSTORE_UNLIKELY(x) __builtin_expect((x),0)

template <typename T>
  T align(T x, T m) {
    if (m == 0 || (x % m) == 0) {
      return x;
    } else {
      return (1 + (x / m)) * m;
    }
  }

#define PRIV_HSTORE_SPIN_MIN 512
#define PRIV_HSTORE_SPIN_MAX 524288

static inline unsigned spin(unsigned count) {
  for (volatile unsigned i = 0; i != count; ++i);
  return (count << 1);
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

// define a local socket for registering new storage queues
inline void mqwrite(int fd, const uint8_t* x, size_t len) {
  size_t i = 0;
  while (i < len) {
    ssize_t c = write(fd, x + i, len - i);
    if (c < 0) {
      if (errno != EINTR) {
        throw std::runtime_error("Couldn't write to socket: " + std::string(strerror(errno)));
      }
    } else {
      i += c;
    }
  }
}

inline void mqread(int fd, uint8_t* x, size_t len) {
  size_t i = 0;
  while (i < len) {
    auto ret = read(fd , x + i, len - i);
    if (ret < 0 && errno != EINTR) {
      throw std::runtime_error("Couldn't read from socket: " + std::string(strerror(errno)));
    } else {
      i += ret;
    }
  }
}

inline int mqconnect(const std::string& fileName) {
  int r = socket(AF_UNIX, SOCK_STREAM, 0);
  if (r == -1) {
    throw std::runtime_error("Unable to allocate socket: " + std::string(strerror(errno)));
  }
  
  sockaddr_un addr;
  memset(&addr, 0, sizeof(addr));
  addr.sun_family = AF_UNIX;
  snprintf(addr.sun_path, sizeof(addr.sun_path), "%s", fileName.c_str());
 
  sockaddr* saddr = reinterpret_cast<sockaddr*>(&addr);
  size_t    len   = sizeof(addr);
  if (connect(r, saddr, len) == -1) {
    std::string emsg = "Unable to connect socket: " + std::string(strerror(errno));
    close(r);
    throw std::runtime_error(emsg);
  }

  fd_set wd;
  FD_ZERO(&wd);
  FD_SET(r, &wd);
  if (select(r + 1, 0, &wd, 0, 0) == -1) {
    std::string emsg = "Failed to connect socket while waiting for writeability: " + std::string(strerror(errno));
    close(r);
    throw std::runtime_error(emsg);
  }

  uint32_t version = HSTORE_VERSION;
  mqwrite(r, reinterpret_cast<const uint8_t*>(&version), sizeof(version));
  return r;
}

inline int mqlisten(const std::string& fileName) {
  int s = socket(AF_UNIX, SOCK_STREAM, 0);
  if (s == -1) {
    throw std::runtime_error("Unable to allocate socket: " + std::string(strerror(errno)));
  }
  
  sockaddr_un addr;
  memset(&addr, 0, sizeof(addr));
  addr.sun_family = AF_UNIX;
  unlink(fileName.c_str());
  snprintf(addr.sun_path, sizeof(addr.sun_path), "%s", fileName.c_str());

  if (bind(s, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) == -1) {
    std::string emsg = "Unable to bind socket to file: " + fileName + " (" + std::string(strerror(errno)) + ")";
    close(s);
    throw std::runtime_error(emsg);
  }

  // and then start to listen
  if (listen(s, SOMAXCONN) == -1) {
    std::string emsg = "Unable to listen socket on file: " + fileName + " (" + std::string(strerror(errno)) + ")";
    close(s);
    throw std::runtime_error(emsg);
  }

  return s;
}

inline std::string defaultStoreDir() {
  const char* td = 0;
  if ((td = ::getenv("HOBBES_STORE_DIR"))) {
    if (::strlen(td) > 0 && ::access(td, W_OK) == 0) {
      return std::string(td);
    }
  }
  if ((td = ::getenv("TMPDIR"))) {
    if (::strlen(td) > 0 && ::access(td, W_OK) == 0) {
      return std::string(td);
    }
  }
  if ((td = ::getenv("TMP"))) {
    if (::strlen(td) > 0 && ::access(td, W_OK) == 0) {
      return std::string(td);
    }
  }
  if (::access("/var/tmp", W_OK) == 0) {
    return "/var/tmp";
  }
  return "/tmp";
}

inline int connectGroupHost(const std::string& groupName, const std::string& sdir = defaultStoreDir()) {
  auto sp = sdir + "/hstore." + groupName + ".sk";
  try {
    return mqconnect(sp);
  } catch (std::exception& e) {
    throw std::runtime_error("Failed to connect to log consumer for group '" + groupName + "' on socket '" + sp + "' (" + std::string(e.what()) + ")");
  }
}

inline int makeGroupHost(const std::string& groupName, const std::string& sdir = defaultStoreDir()) {
  auto sp = sdir + "/hstore." + groupName + ".sk";
  try {
    return mqlisten(sp);
  } catch (std::exception& e) {
    throw std::runtime_error("Failed to listen for log connections for group '" + groupName + "' on socket '" + sp + "' (" + std::string(e.what()) + ")");
  }
}

// identify this process/thread
typedef std::pair<uint64_t, uint64_t> ProcThread;

inline ProcThread thisProcThread() {
  ProcThread r;
  r.first = static_cast<uint64_t>(getpid());
#if defined(__APPLE__) && defined(__MACH__)
  pthread_threadid_np(0, &r.second);
#else
  r.second = static_cast<uint64_t>(syscall(SYS_gettid));
#endif
  return r;
}

// derive a name for a shared memory region with this group name in this thread/process
inline std::string sharedMemName(const std::string& groupName, const ProcThread& pt) {
  std::ostringstream ss;
  ss << "/" << groupName << "." << pt.first << "." << pt.second;
  return ss.str();
}
inline std::string sharedMemName(const std::string& groupName) {
  return sharedMemName(groupName, thisProcThread());
}

// register the allocation of a shared memory region with this group name in this thread/process
inline void registerSHMAlloc(int* mqserver, const char* groupName, const WaitPolicy wp, bool reconnect = false) {
  if (*mqserver < 0) {
    *mqserver = connectGroupHost(groupName);
  }

  ProcThread pt = thisProcThread();
  uint8_t msg[1+sizeof(ProcThread)] = {0};
  uint8_t& cmd = msg[0];
  cmd |= reconnect ? 1 : 0;
  cmd |= static_cast<uint8_t>(wp) << 1;
  memcpy(msg+1, &pt, sizeof(ProcThread));
  mqwrite(*mqserver, msg, sizeof(msg));
}

inline void reconnectSHM(int* mqserver, const char* groupName, const WaitPolicy wp) {
  // has the other side disconnected?
  if (*mqserver >= 0) {
    fd_set rd;
    FD_ZERO(&rd);
    FD_SET(*mqserver, &rd);

    timeval tmout;
    tmout.tv_sec = 0;
    tmout.tv_usec = 1;

    auto sr = select(*mqserver+1, &rd, 0, 0, &tmout);
    if (sr == 1) {
      char b=0;
      if (recv(*mqserver, &b, 1, MSG_PEEK) == 0) {
        close(*mqserver);
        *mqserver = -1;
      }
    }
  }

  // if we're in a disconnected state, try once to reconnect
  if (*mqserver < 0) {
    try {
      registerSHMAlloc(mqserver, groupName, wp, true);
    } catch (std::exception& ex) {
    }
  }
}

// between queue readers and writers, there are only three states of concern:
//   0: both reader and writer are making progress
//   1: the reader is blocked waiting for new values (empty queue)
//   2: the writer is blocked waiting for the reader to catch up (full queue)
#define PRIV_HSTORE_STATE_UNBLOCKED      0
#define PRIV_HSTORE_STATE_READER_WAITING 1
#define PRIV_HSTORE_STATE_WRITER_WAITING 2

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
  WaitFn        waitFn;
  WakeFn        wakeFn;

  inline volatile uint32_t* waitState()                     const { return this->cfg.wstate; }
  inline volatile uint32_t* readIndex()                     const { return this->cfg.readerIndex; }
  inline volatile uint32_t* writeIndex()                    const { return this->cfg.writerIndex; }
  inline uint8_t*           value(size_t i)                 const { return this->cfg.data + (i*this->cfg.valuesz); }
  inline uint32_t           nextIndex(volatile uint32_t* i) const { return (*i + 1) % this->cfg.count; }
public:
  writer(const bytes& meta, const std::string& shmname, size_t qvalsz, size_t count, const WaitPolicy wp) : waitFn(hobbes::storage::waitFn(wp)), wakeFn(hobbes::storage::wakeFn(wp)) {
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
  
    uint8_t* mem = reinterpret_cast<uint8_t*>(mmap(0, memLen, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_POPULATE, shfd, 0));
    if (mem == MAP_FAILED) {
      throw std::runtime_error("Failed to map bytes out of shared memory for '" + shmname + "': " + strerror(errno));
    }
  
    // write meta data
    ShQueueHeader* hdr = reinterpret_cast<ShQueueHeader*>(mem);
    hdr->valsz  = qvalsz;
    hdr->count  = count;
    hdr->metasz = meta.size();
    memcpy(mem + sizeof(ShQueueHeader), &meta[0], meta.size());
  
    // OK, this queue is fully initialized
    uxchg(&hdr->ready, 1);
  
    // now make this pqueue config
    ShQueueData* sqd = reinterpret_cast<ShQueueData*>(mem + metaLen);
  
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

  uint8_t* next(size_t timeoutNS = 0, const std::function<void()>& timeoutF = [](){}) {
    uint32_t nwi = nextIndex(writeIndex());

    unsigned count = PRIV_HSTORE_SPIN_MIN;
  
    while (PRIV_HSTORE_UNLIKELY(*readIndex() == nwi)) {
      if (count < PRIV_HSTORE_SPIN_MAX) {
        // back-off the writer
        count = spin(count);
      } else {
        // the reader is behind and we've caught up with it, switch into writer-wait mode
        switch (xchg(waitState(), PRIV_HSTORE_STATE_WRITER_WAITING)) {
          case PRIV_HSTORE_STATE_UNBLOCKED:
            // we previously were unblocked
            // make sure that we still need to block the writer (in case the read index moved while we were getting here)
            // then block while we're in writer-wait state
            if (*readIndex() == nwi) {
              (*waitFn)(waitState(), PRIV_HSTORE_STATE_WRITER_WAITING, timeoutNS, timeoutF);
            }
            break;
          case PRIV_HSTORE_STATE_READER_WAITING:
            // we previously were in reader-wait state (this should practically never happen)
            // since we wait to write anyway, unblock the reader and try again
            uxchg(waitState(), PRIV_HSTORE_STATE_UNBLOCKED);
            (*wakeFn)(waitState(), 1);
            break;
        }
      }
    }
    return value(*writeIndex());
  }
  
  uint8_t* pollNext() {
    if (PRIV_HSTORE_UNLIKELY(*readIndex() == nextIndex(writeIndex()))) {
      return 0;
    } else {
      return value(*writeIndex());
    }
  }

  void push() {
    uxchg(writeIndex(), nextIndex(writeIndex()));
  
    // when the writer advances, the reader can be unblocked
    if (PRIV_HSTORE_UNLIKELY(xchg(waitState(), PRIV_HSTORE_STATE_UNBLOCKED) == PRIV_HSTORE_STATE_READER_WAITING)) {
      (*wakeFn)(waitState(), 1);
    }
  }
};

// shared memory pages can be marked as representing four possible conditions:
//   1) page is published but state is undetermined (contingent on being able to acquire another page without blocking)
//   2) page is a continuation of its transaction (intermediate in the total transaction)
//   3) page successfully terminates the transaction (commit)
//   4) page prematurely terminates the transaction (rollback)
#define PRIV_HSTORE_PAGE_STATE_TENTATIVE static_cast<uint8_t>(0)
#define PRIV_HSTORE_PAGE_STATE_CONT      static_cast<uint8_t>(1)
#define PRIV_HSTORE_PAGE_STATE_COMMIT    static_cast<uint8_t>(2)
#define PRIV_HSTORE_PAGE_STATE_ROLLBACK  static_cast<uint8_t>(3)

// writers may record data 'unreliably' (non-blocking) or 'reliably' (block iff waiting for consumer to catch up)
enum PipeQOS {
  Reliable = 0,
  Unreliable
};

// wpipe : a "write pipe" (to write an 'arbitrary-length' sequence of data) on top of writers
class wpipe {
private:
  writer*               wq;
  uint8_t*              page;
  size_t                pagesz;
  uint32_t              offset;
  PipeQOS               qos;
  size_t                timeoutNS;
  std::function<void()> timeoutF;
  size_t                unrtimeNS;

  void markPage(uint8_t c) {
    *reinterpret_cast<uint32_t*>(this->page + this->pagesz) = (static_cast<uint32_t>(c) << 24) | this->offset;
  }

  inline bool reliable() const {
    return this->qos == Reliable;
  }

  bool stepPage() {
    if (reliable()) {
      markPage(PRIV_HSTORE_PAGE_STATE_CONT);
      this->wq->push();

      this->page   = this->wq->next(this->timeoutNS, this->timeoutF);
      this->offset = 0;
      return true;
    } else {
      markPage(PRIV_HSTORE_PAGE_STATE_TENTATIVE);
      this->wq->push();
      uint8_t* npage = this->wq->pollNext();
      markPage(npage ? PRIV_HSTORE_PAGE_STATE_CONT : PRIV_HSTORE_PAGE_STATE_ROLLBACK);

      this->page   = npage;
      this->offset = 0;
      return npage;
    }
  }
public:
  wpipe(writer* wq, PipeQOS qos = Reliable, size_t timeoutNS = 0, const std::function<void()>& timeoutF = [](){}) : wq(wq), pagesz(wq->config().valuesz - sizeof(uint32_t)), offset(0), qos(qos), timeoutNS(timeoutNS), timeoutF(timeoutF), unrtimeNS(0) {
    if (wq->config().valuesz <= sizeof(uint32_t)) {
      throw std::runtime_error("queue page size too small for use as shared memory pipe");
    }
    this->page = wq->pollNext();
  }

  void commit() {
    if (reliable()) {
      markPage(PRIV_HSTORE_PAGE_STATE_COMMIT);
      this->wq->push();
      this->page = this->wq->next(this->timeoutNS, this->timeoutF);
    } else {
      if (PRIV_HSTORE_LIKELY(this->page != 0)) {
        markPage(PRIV_HSTORE_PAGE_STATE_COMMIT);
        this->wq->push();
      }
      this->page = this->wq->pollNext();

      // allow unreliable producers to process timeout events
      if (PRIV_HSTORE_UNLIKELY(this->page == 0)) {
        if (this->unrtimeNS == 0) {
          this->unrtimeNS = internal::spin::poll_tickNS();
        } else if (internal::spin::poll_tickNS() - this->unrtimeNS >= this->timeoutNS) {
          this->timeoutF();
          this->unrtimeNS = 0;
        }
      }
    }
    this->offset = 0;
  }

  void rollback() {
    if (PRIV_HSTORE_LIKELY(this->page != 0)) {
      markPage(PRIV_HSTORE_PAGE_STATE_ROLLBACK);
      this->wq->push();
    }
    this->page   = reliable() ? this->wq->next(this->timeoutNS, this->timeoutF) : this->wq->pollNext();
    this->offset = 0;
  }

  bool hasSpaceFor(size_t sz) const {
    return this->page && sz < (this->pagesz - this->offset);
  }

  // write a block of bytes within a frame
  bool write(const uint8_t* src, size_t sz) {
    // just for unreliable pipes, we might enter here without a page
    if (PRIV_HSTORE_UNLIKELY(!this->page)) {
      return false;
    }

    size_t remsz = this->pagesz - this->offset;

    // most of the time we'll be writing small chunks of data
    if (PRIV_HSTORE_LIKELY(sz < remsz)) {
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
  int         shfd;
  uint8_t*    data;
  size_t      datasz;
  size_t      pagesz;
  std::string shmname;
};

inline QueueConnection consumeQueue(const std::string& shmname) {
  // sections of shared memory are aligned to page boundaries
  long pagesz = sysconf(_SC_PAGESIZE);
  if (pagesz == -1) {
    throw std::runtime_error("Failed to query system page size for '" + shmname + "': " + strerror(errno));
  }

  // see if we can open this shared memory region
  int shfd = shm_open(shmname.c_str(), O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  if (shfd == -1) {
    throw std::runtime_error("Failed to open shared memory for '" + shmname + "': " + strerror(errno));
  }

  // make sure that the region has been sized
  struct stat msb;
  if (fstat(shfd, &msb) < 0) {
    close(shfd);
    throw std::runtime_error("Failed to stat shared memory for '" + shmname + "': " + strerror(errno));
  }
  if (msb.st_size <= 0) {
    close(shfd);
    throw std::runtime_error("Shared memory for '" + shmname + "' is not ready");
  }

  // map memory for this data, ensure it is in a good state
  uint8_t* mem = reinterpret_cast<uint8_t*>(mmap(0, msb.st_size, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_POPULATE, shfd, 0));
  if (mem == MAP_FAILED) {
    close(shfd);
    throw std::runtime_error("Failed to map bytes out of shared memory for '" + shmname + "': " + strerror(errno));
  }

  if (!reinterpret_cast<ShQueueHeader*>(mem)->ready) {
    munmap(mem, msb.st_size);
    close(shfd);
    throw std::runtime_error("Not ready to consume shared memory for '" + shmname + "'");
  }

  QueueConnection c;
  c.shfd    = shfd;
  c.data    = mem;
  c.datasz  = msb.st_size;
  c.pagesz  = pagesz;
  c.shmname = shmname;
  return c;
}

inline QueueConnection consumeGroup(const std::string& gname, const ProcThread& pt) {
  return consumeQueue(sharedMemName(gname, pt));
}

// read data out of shared memory
class reader {
private:
  int            shfd;
  const uint8_t* metad;
  size_t         metasz;
  pqueue_config  cfg;
  WaitFn         waitFn;
  WakeFn         wakeFn;

  inline volatile uint32_t* waitState()                     const { return this->cfg.wstate; }
  inline volatile uint32_t* readIndex()                     const { return this->cfg.readerIndex; }
  inline volatile uint32_t* writeIndex()                    const { return this->cfg.writerIndex; }
  inline uint8_t*           value(size_t i)                 const { return this->cfg.data + (i*this->cfg.valuesz); }
  inline uint32_t           nextIndex(volatile uint32_t* i) const { return (*i + 1) % this->cfg.count; }
public:
  reader(const QueueConnection& qc, const WaitPolicy wp) : shfd(qc.shfd), waitFn(hobbes::storage::waitFn(wp)), wakeFn(hobbes::storage::wakeFn(wp)) {
    // prepare to read the queue description
    ShQueueHeader* hdr     = reinterpret_cast<ShQueueHeader*>(qc.data);
    size_t         metaLen = align<size_t>(sizeof(ShQueueHeader) + hdr->metasz, qc.pagesz);
    ShQueueData*   sqd     = reinterpret_cast<ShQueueData*>(qc.data + metaLen);

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
  uint8_t* next(size_t timeoutNS, const std::function<void()>& timeoutF) {
    uint32_t ri = *readIndex();

    unsigned count = PRIV_HSTORE_SPIN_MIN;
  
    while (PRIV_HSTORE_UNLIKELY(*writeIndex() == ri)) {
      if (count < PRIV_HSTORE_SPIN_MAX) {
        // try back-off the reader
        count = spin(count);
      } else {
        // there's nothing to read, switch into reader-wait mode
        switch (xchg(waitState(), PRIV_HSTORE_STATE_READER_WAITING)) {
          case PRIV_HSTORE_STATE_UNBLOCKED:
            // we previously were unblocked
            // make sure that we still need to block the reader (in case the write index moved while we were getting here)
            // then block while we're in reader-wait state
            if (*writeIndex() == ri) {
              (*waitFn)(waitState(), PRIV_HSTORE_STATE_READER_WAITING, timeoutNS, timeoutF);
            }
            break;
          case PRIV_HSTORE_STATE_WRITER_WAITING:
            // we previously were in writer-wait state (this should practically never happen)
            // since we wait to read anyway, unblock the writer and try again
            uxchg(waitState(), PRIV_HSTORE_STATE_UNBLOCKED);
            (*wakeFn)(waitState(), 1);
            break;
        }
      }
    }
    return value(*readIndex());
  }

  // get the next value in the queue if one is present, else null
  uint8_t* pollNext() {
    if (PRIV_HSTORE_UNLIKELY(*writeIndex() == *readIndex())) {
      return 0;
    } else {
      return value(*readIndex());
    }
  }

  // remove the next value from the queue (increment the read index)
  void pop() {
    uxchg(readIndex(), nextIndex(readIndex()));
  
    // when the reader advances, the writer can be unblocked
    if (PRIV_HSTORE_UNLIKELY(xchg(waitState(), PRIV_HSTORE_STATE_UNBLOCKED) == PRIV_HSTORE_STATE_WRITER_WAITING)) {
      (*wakeFn)(waitState(), 1);
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
  size_t read(uint8_t* dst, size_t sz, uint8_t* state, size_t timeoutNS, const std::function<void()>& timeoutF) {
    if (!this->page) {
      this->page = this->rq->next(timeoutNS, timeoutF);
    }
  
    size_t doff = 0;
  
    while (sz != 0 && this->page) {
      // how much can we copy out of the current page into the dest buffer?
      const volatile uint32_t* pend      = reinterpret_cast<const uint32_t*>(this->page + this->pagesz);
      size_t                   rpagesz   = *pend & ~(0xFF << 24);
      size_t                   availPage = rpagesz - this->offset;
      size_t                   csz       = (sz < availPage) ? sz : availPage;
  
      // copy as much as we can
      memcpy(dst + doff, this->page + this->offset, csz);
  
      // step through this segment we've copied
      this->offset += csz;
      doff         += csz;
      sz           -= csz;
  
      // if we've hit the end of the page, mark it done and then try for the next one
      if (this->offset == rpagesz) {
        // wait for the producer to decide what state the page is in
        while ((*pend >> 24) == PRIV_HSTORE_PAGE_STATE_TENTATIVE);
        uint8_t ps = *pend >> 24;
        if (state) *state = ps;

        // if this page is just a continuation, continue reading
        // else we've terminated a transaction
        this->rq->pop();
        this->page   = (ps == PRIV_HSTORE_PAGE_STATE_CONT) ? this->rq->pollNext() : 0;
        this->offset = 0;
      }
    }
  
    return doff;
  }
};

// basic compile-time strings
template <size_t N>
  constexpr char at(size_t i, const char (&s)[N]) {
    return (i < N) ? s[i] : '\0';
  }
template <size_t N>
  constexpr size_t at8S(size_t i, size_t k, const char (&s)[N]) {
    return (k==8) ? 0 : ((static_cast<size_t>(at(i+k,s))<<(8*k))|at8S(i,k+1,s));
  }
template <size_t N>
  constexpr size_t at8(size_t i, const char (&s)[N]) {
    return at8S(i, 0, s);
  }
template <size_t... pcs>
  struct strpack {
    static const char* str() {
      constexpr size_t msg[] = {pcs...};
      static_assert(msg[(sizeof(msg)/sizeof(msg[0]))-1] == 0, "compile-time string larger than internal max limit (this limit can be bumped in storage.H)");

      static const size_t smsg[] = {pcs...};
      return reinterpret_cast<const char*>(smsg);
    }
  };
#define PRIV_HSTORE_TSTR32(i,s)   ::hobbes::storage::at8(i+(8*0),s),::hobbes::storage::at8(i+(8*1),s),::hobbes::storage::at8(i+(8*2),s),::hobbes::storage::at8(i+(8*3),s)
#define PRIV_HSTORE_TSTR128(i,s)  PRIV_HSTORE_TSTR32(i+(32*0),s),PRIV_HSTORE_TSTR32(i+(32*1),s),PRIV_HSTORE_TSTR32(i+(32*2),s),PRIV_HSTORE_TSTR32(i+(32*3),s)
#define PRIV_HSTORE_TSTR512(i,s)  PRIV_HSTORE_TSTR128(i+(128*0),s),PRIV_HSTORE_TSTR128(i+(128*1),s),PRIV_HSTORE_TSTR128(i+(128*2),s),PRIV_HSTORE_TSTR128(i+(128*3),s)
#define PRIV_HSTORE_TSTR1024(i,s) PRIV_HSTORE_TSTR512(i+(512*0),s),PRIV_HSTORE_TSTR512(i+(512*1),s)
#define PRIV_HSTORE_TSTR(s)       ::hobbes::storage::strpack<PRIV_HSTORE_TSTR1024(0,s)>

// type/value serialization (guarded by compile-time predicates)
#define PRIV_HSTORE_TYCTOR_PRIM      static_cast<int>(0)
#define PRIV_HSTORE_TYCTOR_TVAR      static_cast<int>(2)
#define PRIV_HSTORE_TYCTOR_FIXEDARR  static_cast<int>(4)
#define PRIV_HSTORE_TYCTOR_ARR       static_cast<int>(5)
#define PRIV_HSTORE_TYCTOR_VARIANT   static_cast<int>(6)
#define PRIV_HSTORE_TYCTOR_STRUCT    static_cast<int>(7)
#define PRIV_HSTORE_TYCTOR_SIZE      static_cast<int>(11)
#define PRIV_HSTORE_TYCTOR_TAPP      static_cast<int>(12)
#define PRIV_HSTORE_TYCTOR_RECURSIVE static_cast<int>(13)
#define PRIV_HSTORE_TYCTOR_TABS      static_cast<int>(15)

template <typename T, typename P = void>
  struct store {
  };
template <typename T>
  void w(const T& x, bytes* out) {
    out->insert(out->end(), reinterpret_cast<const uint8_t*>(&x), reinterpret_cast<const uint8_t*>(&x) + sizeof(x));
  }
inline void ws(const char* x, bytes* out) {
  size_t n = strlen(x);
  w(n, out);
  out->insert(out->end(), x, x + n);
}
inline void ws(const std::string& x, bytes* out) {
  w(static_cast<size_t>(x.size()), out);
  out->insert(out->end(), x.begin(), x.end());
}
inline void ws(const bytes& x, bytes* out) {
  w(static_cast<size_t>(x.size()), out);
  out->insert(out->end(), x.begin(), x.end());
}

inline void encode_primty(const char* tn, bytes* out) {
  w(PRIV_HSTORE_TYCTOR_PRIM, out);
  ws(tn, out);
  w(false, out);
}
#define PRIV_HSTORE_DEFINE_PRIMTYS(T, n) \
  template <> \
    struct store<T> { \
      typedef void can_memcpy; \
      static void        encode(bytes* out)          { encode_primty(n, out); } \
      static std::string describe()                  { return n; } \
      static size_t      size(const T&)              { return sizeof(T); } \
      static bool        write(wpipe& p, const T& x) { return p.write(reinterpret_cast<const uint8_t*>(&x), sizeof(x)); } \
    }

PRIV_HSTORE_DEFINE_PRIMTYS(bool,     "bool");
PRIV_HSTORE_DEFINE_PRIMTYS(uint8_t,  "byte");
PRIV_HSTORE_DEFINE_PRIMTYS(char,     "char");
PRIV_HSTORE_DEFINE_PRIMTYS(int16_t,  "short");
PRIV_HSTORE_DEFINE_PRIMTYS(uint16_t, "short");
PRIV_HSTORE_DEFINE_PRIMTYS(int32_t,  "int");
PRIV_HSTORE_DEFINE_PRIMTYS(uint32_t, "int");
PRIV_HSTORE_DEFINE_PRIMTYS(int64_t,  "long");
PRIV_HSTORE_DEFINE_PRIMTYS(uint64_t, "long");
#if defined(__APPLE__) && defined(__MACH__)
PRIV_HSTORE_DEFINE_PRIMTYS(size_t, "long");
#endif
PRIV_HSTORE_DEFINE_PRIMTYS(float,    "float");
PRIV_HSTORE_DEFINE_PRIMTYS(double,   "double");

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
      snprintf(fn, sizeof(fn), ".f%lld", static_cast<unsigned long long>(f));
      ws(fn, out);
      w(static_cast<int>(-1), out);
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
        w(PRIV_HSTORE_TYCTOR_STRUCT, out);
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
#define PRIV_HSTORE_FIRST(a, ...) a
#define PRIV_HSTORE_SECOND(a, b, ...) b
#define PRIV_HSTORE_JOIN(a,b) a ## b
#define PRIV_HSTORE_IS_NEGATE(...) PRIV_HSTORE_SECOND(__VA_ARGS__, 0)
#define PRIV_HSTORE_NOT(x) PRIV_HSTORE_IS_NEGATE(PRIV_HSTORE_JOIN(PRIV_HSTORE_SNOT_, x))
#define PRIV_HSTORE_SNOT_0 NEGATE, 1
#define PRIV_HSTORE_BOOL(x) PRIV_HSTORE_NOT(PRIV_HSTORE_NOT(x))
#define PRIV_HSTORE_IF_ELSE(condition) PRIV_HSTORE_SIF_ELSE(PRIV_HSTORE_BOOL(condition))
#define PRIV_HSTORE_SIF_ELSE(condition) PRIV_HSTORE_JOIN(PRIV_HSTORE_SIF_, condition)
#define PRIV_HSTORE_SIF_1(...) __VA_ARGS__ PRIV_HSTORE_SIF_1_ELSE
#define PRIV_HSTORE_SIF_0(...)             PRIV_HSTORE_SIF_0_ELSE
#define PRIV_HSTORE_SIF_1_ELSE(...)
#define PRIV_HSTORE_SIF_0_ELSE(...) __VA_ARGS__
#define PRIV_HSTORE_EMPTY()
#define PRIV_HSTORE_EVAL(...) PRIV_HSTORE_EVAL256(__VA_ARGS__)
#define PRIV_HSTORE_EVAL256(...) PRIV_HSTORE_EVAL128(PRIV_HSTORE_EVAL128(__VA_ARGS__))
#define PRIV_HSTORE_EVAL128(...) PRIV_HSTORE_EVAL64(PRIV_HSTORE_EVAL64(__VA_ARGS__))
#define PRIV_HSTORE_EVAL64(...) PRIV_HSTORE_EVAL32(PRIV_HSTORE_EVAL32(__VA_ARGS__))
#define PRIV_HSTORE_EVAL32(...) PRIV_HSTORE_EVAL16(PRIV_HSTORE_EVAL16(__VA_ARGS__))
#define PRIV_HSTORE_EVAL16(...) PRIV_HSTORE_EVAL8(PRIV_HSTORE_EVAL8(__VA_ARGS__))
#define PRIV_HSTORE_EVAL8(...) PRIV_HSTORE_EVAL4(PRIV_HSTORE_EVAL4(__VA_ARGS__))
#define PRIV_HSTORE_EVAL4(...) PRIV_HSTORE_EVAL2(PRIV_HSTORE_EVAL2(__VA_ARGS__))
#define PRIV_HSTORE_EVAL2(...) PRIV_HSTORE_EVAL1(PRIV_HSTORE_EVAL1(__VA_ARGS__))
#define PRIV_HSTORE_EVAL1(...) __VA_ARGS__
#define PRIV_HSTORE_DEFER2(m) m PRIV_HSTORE_EMPTY PRIV_HSTORE_EMPTY()()
#define PRIV_HSTORE_HAS_PARGS(...) PRIV_HSTORE_BOOL(PRIV_HSTORE_FIRST(PRIV_HSTORE_SEOAP_ __VA_ARGS__)())
#define PRIV_HSTORE_SEOAP_(...) PRIV_HSTORE_BOOL(PRIV_HSTORE_FIRST(PRIV_HSTORE_SEOA_ __VA_ARGS__)())
#define PRIV_HSTORE_SEOA_() 0
#define PRIV_HSTORE_MAP(f, VS...) PRIV_HSTORE_EVAL(PRIV_HSTORE_MAPP(f, VS))
#define PRIV_HSTORE_MAPP(f, H, T...)        \
  f H                                 \
  PRIV_HSTORE_IF_ELSE(PRIV_HSTORE_HAS_PARGS(T))(  \
    PRIV_HSTORE_DEFER2(PRIV_HSTORE_SMAPP)()(f, T) \
  )(                                  \
  )
#define PRIV_HSTORE_SMAPP() PRIV_HSTORE_MAPP
#define PRIV_HSTORE_MAPL(f, VS...) PRIV_HSTORE_EVAL(PRIV_HSTORE_MAPLP(f, VS))
#define PRIV_HSTORE_MAPLP(f, H, T...)              \
  f(H)                                             \
  PRIV_HSTORE_IF_ELSE(PRIV_HSTORE_HAS_PARGS(T))(   \
    PRIV_HSTORE_DEFER2(PRIV_HSTORE_SMAPLP)()(f, T) \
  )(                                               \
  )
#define PRIV_HSTORE_SMAPLP() PRIV_HSTORE_MAPLP

// support storing packed, reflective structs
#define HSTORE_FIELDC_SUCC(t,n) +1
#define HSTORE_FIELD_COUNT(FIELDS...) (0 PRIV_HSTORE_MAP(HSTORE_FIELDC_SUCC, FIELDS))

#define HSTORE_FIELDS_SUCC(t,n) +::hobbes::storage::store< t >::size(this-> n)
#define HSTORE_FIELD_SIZE(FIELDS...) (0 PRIV_HSTORE_MAP(HSTORE_FIELDS_SUCC, FIELDS))

#define HSTORE_FIELDW_SUCC(t,n) &&::hobbes::storage::store< t >::write(p, this-> n)
#define HSTORE_FIELD_WRITES(FIELDS...) (true PRIV_HSTORE_MAP(HSTORE_FIELDW_SUCC, FIELDS))

#define HSTORE_STRUCT_FIELD(t, n) t n;
#define HSTORE_STRUCT_FIELD_ENC(t, n) ::hobbes::storage::ws(#n, out); ::hobbes::storage::w(static_cast<int>(-1), out); ::hobbes::storage::store< t >::encode(out);
#define HSTORE_STRUCT_FIELD_DESC(t, n) + (", " #n " : " + ::hobbes::storage::store< t >::describe())
#define HSTORE_STRUCT_FIELD_EQ(t, n) && this->n == rhs.n

#define DEFINE_PACKED_HSTORE_STRUCT(T, FIELDS...) \
  struct T { \
    PRIV_HSTORE_MAP(HSTORE_STRUCT_FIELD, FIELDS) /* struct fields */ \
    typedef void is_packed_hstore_struct; /* identify this type as a packed struct */ \
    static void encode(::hobbes::storage::bytes* out) { \
      size_t localArity = HSTORE_FIELD_COUNT(FIELDS); \
      if (localArity > 0) { \
        ::hobbes::storage::w(PRIV_HSTORE_TYCTOR_STRUCT, out); \
        ::hobbes::storage::w(localArity, out); \
        PRIV_HSTORE_MAP(HSTORE_STRUCT_FIELD_ENC, FIELDS) \
      } else { \
        ::hobbes::storage::encode_primty("unit", out); \
      } \
    } \
    static std::string describe() { \
      return "{" + ( std::string("") PRIV_HSTORE_MAP(HSTORE_STRUCT_FIELD_DESC, FIELDS) ).substr(2) + "}"; \
    } \
    bool operator==(const T& rhs) const { \
      return true PRIV_HSTORE_MAP(HSTORE_STRUCT_FIELD_EQ, FIELDS); \
    } \
  } __attribute__((packed))

template <typename T>
  struct store<T, typename T::is_packed_hstore_struct> {
    typedef void can_memcpy;
    static void        encode(bytes* out)          { T::encode(out); }
    static std::string describe()                  { return T::describe(); }
    static size_t      size(const T&)              { return sizeof(T); }
    static bool        write(wpipe& p, const T& x) { return p.write(reinterpret_cast<const uint8_t*>(&x), sizeof(x)); }
  };

// support storing standard, reflective structs
#define DEFINE_HSTORE_STRUCT(T, FIELDS...) \
  struct T { \
    PRIV_HSTORE_MAP(HSTORE_STRUCT_FIELD, FIELDS) /* struct fields */ \
    typedef void is_hstore_struct; /* identify this type as a struct */ \
    static std::string name() { return #T; } \
    static void encode(::hobbes::storage::bytes* out) { \
      size_t localArity = HSTORE_FIELD_COUNT(FIELDS); \
      if (localArity > 0) { \
        ::hobbes::storage::w(PRIV_HSTORE_TYCTOR_STRUCT, out); \
        ::hobbes::storage::w(localArity, out); \
        PRIV_HSTORE_MAP(HSTORE_STRUCT_FIELD_ENC, FIELDS) \
      } else { \
        ::hobbes::storage::encode_primty("unit", out); \
      } \
    } \
    static std::string describe() { \
      return "{" + ( std::string("") PRIV_HSTORE_MAP(HSTORE_STRUCT_FIELD_DESC, FIELDS) ).substr(2) + "}"; \
    } \
    bool operator==(const T& rhs) const { \
      return true PRIV_HSTORE_MAP(HSTORE_STRUCT_FIELD_EQ, FIELDS); \
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
    static std::string storageName()               { return T::name(); }
    static void        encode(bytes* out)          { T::encode(out); }
    static std::string describe()                  { return T::describe(); }
    static size_t      size(const T& x)            { return x.size(); }
    static bool        write(wpipe& p, const T& x) { return x.write(p); }
  };

// support storing standard pairs of types
template <typename U, typename V>
  struct store<std::pair<U,V>> {
    static void encode(bytes* out) {
      w(PRIV_HSTORE_TYCTOR_STRUCT, out);
      w(static_cast<size_t>(2), out);
      
      ws(".f0", out);
      w(static_cast<int>(-1), out);
      store<U>::encode(out);

      ws(".f1", out);
      w(static_cast<int>(-1), out);
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

// support storing "struct views" (select fields / const accessor functions) out of a type
template <typename T, typename F>
  static F T::* resolve(F T::*);
template <typename T, typename R>
  static auto resolve(R (T::*K)() const) -> decltype(K);

template <typename T> struct member;

template <typename T, typename F>
  struct member<F T::*> {
    typedef F type;
    typedef F T::* Acc;
    static const F& read(const T& x, Acc a) { return x.*a; }
  };
template <typename T, typename F>
  struct member<F (T::*)() const> {
    typedef F type;
    typedef F (T::*Acc)() const;
    static F read(const T& x, Acc a) { return (x.*a)(); }
  };

#define PRIV_HSTORE_DSV_T(f)     member<decltype(resolve(&SelfT::f))>::type
#define PRIV_HSTORE_DSV_READ(f)  member<decltype(resolve(&SelfT::f))>::read(x, std::integral_constant<member<decltype(resolve(&SelfT::f))>::Acc, &SelfT::f>::value)
#define PRIV_HSTORE_DSV_COUNT(_) +1
#define PRIV_HSTORE_DSV_ENCF(f)  ws(#f, out); w(int(-1), out); store< PRIV_HSTORE_DSV_T(f) >::encode(out);
#define PRIV_HSTORE_DSV_DESC(f)  ss << ", " << #f << ":" << store< PRIV_HSTORE_DSV_T(f) >::describe();
#define PRIV_HSTORE_DSV_SIZE(f)  + store< PRIV_HSTORE_DSV_T(f) >::size(PRIV_HSTORE_DSV_READ(f))
#define PRIV_HSTORE_DSV_WRITE(f) && store< PRIV_HSTORE_DSV_T(f) >::write(p, PRIV_HSTORE_DSV_READ(f))

#define DEFINE_HSTORE_STRUCT_VIEW(T, FIELDS...) \
  namespace hobbes { namespace storage { \
    template <> \
      struct store<T> { \
        typedef T SelfT; \
        static void encode(bytes* out) { w(PRIV_HSTORE_TYCTOR_STRUCT, out); w(size_t(0 PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_COUNT, FIELDS)), out); PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_ENCF, FIELDS); } \
        static std::string describe() { std::ostringstream ss; PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_DESC, FIELDS); return "{ " + ss.str().substr(2) + " }"; } \
        static size_t size(const T& x) { return 0 PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_SIZE, FIELDS); } \
        static bool write(wpipe& p, const T& x) { return true PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_WRITE, FIELDS); } \
        static std::string storageName() { return #T; } \
      }; \
    template <> \
      struct store<T*> { \
        typedef T SelfT; \
        static void encode(bytes* out) { w(PRIV_HSTORE_TYCTOR_STRUCT, out); w(size_t(0 PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_COUNT, FIELDS)), out); PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_ENCF, FIELDS); } \
        static std::string describe() { std::ostringstream ss; PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_DESC, FIELDS); return "{ " + ss.str().substr(2) + " }"; } \
        static size_t size(const T* px) { const T& x = *px; return 0 PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_SIZE, FIELDS); } \
        static bool write(wpipe& p, const T* px) { const T& x = *px; return true PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_WRITE, FIELDS); } \
        static std::string storageName() { return #T; } \
      }; \
    template <> \
      struct store<const T*> { \
        typedef T SelfT; \
        static void encode(bytes* out) { w(PRIV_HSTORE_TYCTOR_STRUCT, out); w(size_t(0 PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_COUNT, FIELDS)), out); PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_ENCF, FIELDS); } \
        static std::string describe() { std::ostringstream ss; PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_DESC, FIELDS); return "{ " + ss.str().substr(2) + " }"; } \
        static size_t size(const T* px) { const T& x = *px; return 0 PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_SIZE, FIELDS); } \
        static bool write(wpipe& p, const T* px) { const T& x = *px; return true PRIV_HSTORE_MAPL(PRIV_HSTORE_DSV_WRITE, FIELDS); } \
        static std::string storageName() { return #T; } \
      }; \
  }}
#define HSTORE_MAKE_FRIEND template <typename _> friend class hobbes::storage::store;

// support storing reflective enumerations
#define HSTORE_ENUM_CTOR_DEF(n) n ,
#define HSTORE_ENUM_CTOR_CTOR(n) static const SelfT n() { return SelfT(SelfT::Enum::n); }
#define HSTORE_ENUM_CTORC_SUCC(n) +1
#define HSTORE_ENUM_CTOR_STR(n) + "|" #n
#define HSTORE_ENUM_CTOR_ENCODE(n) \
  ::hobbes::storage::ws(#n, out); \
  ::hobbes::storage::w(static_cast<uint32_t>(Enum :: n), out); \
  ::hobbes::storage::encode_primty("unit", out);

#define DEFINE_HSTORE_ENUM(T, CTORS...) \
  struct T { \
    typedef void is_hstore_enum; \
    enum class Enum : uint32_t { \
      PRIV_HSTORE_MAP(HSTORE_ENUM_CTOR_DEF, CTORS) \
      COUNT \
    }; \
    Enum value; \
    T() : value() { } \
    T(Enum v) : value(v) { } \
    T& operator=(Enum v) { this->value = v; return *this; } \
    operator Enum() const { return this->value; } \
    typedef T SelfT; \
    PRIV_HSTORE_MAP(HSTORE_ENUM_CTOR_CTOR, CTORS) \
    static void encode(::hobbes::storage::bytes* out) { \
      ::hobbes::storage::w(PRIV_HSTORE_TYCTOR_VARIANT, out); \
      ::hobbes::storage::w(static_cast<size_t>(0 PRIV_HSTORE_MAP(HSTORE_ENUM_CTORC_SUCC, CTORS)), out); \
      PRIV_HSTORE_MAP(HSTORE_ENUM_CTOR_ENCODE, CTORS); \
    } \
    static std::string describe() { \
      return (std::string("") PRIV_HSTORE_MAP(HSTORE_ENUM_CTOR_STR, CTORS)).substr(1); \
    } \
    bool operator==(const T& rhs) const { return this->value == rhs.value; } \
  }

template <typename T>
  struct store<T, typename T::is_hstore_enum> {
    typedef void can_memcpy;
    static void        encode(bytes* out)          { T::encode(out); }
    static std::string describe()                  { return T::describe(); }
    static size_t      size(const T&)              { return sizeof(T); }
    static bool        write(wpipe& p, const T& x) { return p.write(reinterpret_cast<const uint8_t*>(&x), sizeof(x)); }
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
      ::hobbes::storage::w(PRIV_HSTORE_TYCTOR_VARIANT, out); \
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
#define HSTORE_VARIANT_PDESTROY(n, t)   case Enum::tag_##n: { typedef t PRIV_DT; ((PRIV_DT*)&this->n##_data)->~PRIV_DT(); } break;
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
  DEFINE_HSTORE_VARIANT_GEN(T, PRIV_HSTORE_MAP(HSTORE_VARIANT_VDECL, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_CTOR, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_PCOPY, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_PDESTROY, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_SUCC, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_CTOR_ENCODE, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_CTOR_STR, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_SIZE_CASE, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_WRITE_CASE, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_VCASE, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_EQCASE, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_CTOR_TAG, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_CTOR_OPAQUEDATA, CTORS))

// (with explicit ctor names)
#define HSTORE_VARIANT_LBL_CTOR(n, lbl, t) static SelfT n(const t & x) { SelfT r; r.tag = Enum::tag_##n; new (r.data) t(x); return r; }
#define HSTORE_VARIANT_LBL_CTOR_STR(n, lbl, t) + "," #n ":" + ::hobbes::storage::store< t >::describe()
#define HSTORE_VARIANT_LBL_CTOR_TAG(n, lbl, t) tag_##n,
#define HSTORE_VARIANT_LBL_SIZE_CASE(n, lbl, t)    case Enum::tag_##n: return sizeof(int) + ::hobbes::storage::store< t >::size(this->n##_data);
#define HSTORE_VARIANT_LBL_WRITE_CASE(n, lbl, t) case Enum::tag_##n: return ::hobbes::storage::store<int>::write(p, (int)this->tag) && ::hobbes::storage::store< t >::write(p, this->n##_data);
#define HSTORE_VARIANT_LBL_PCOPY(n, lbl, t)      case Enum::tag_##n: new (this->data) t(rhs.n##_data); break;
#define HSTORE_VARIANT_LBL_PDESTROY(n, lbl, t)   case Enum::tag_##n: { typedef t PRIV_DT; ((PRIV_DT*)&this->n##_data)->~PRIV_DT(); } break;
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
  DEFINE_HSTORE_VARIANT_GEN(T, PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_VDECL, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_CTOR, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_PCOPY, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_PDESTROY, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_SUCC, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_CTOR_ENCODE, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_CTOR_STR, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_SIZE_CASE, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_WRITE_CASE, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_VCASE, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_EQCASE, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_CTOR_TAG, CTORS), PRIV_HSTORE_MAP(HSTORE_VARIANT_LBL_CTOR_OPAQUEDATA, CTORS))

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
      w(PRIV_HSTORE_TYCTOR_TVAR, out);
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
      w(PRIV_HSTORE_TYCTOR_RECURSIVE, out);
      ws("x", out);
      store<T>::encode(out);
    }
    static std::string describe() {
      return "^x." + store<T>::describe();
    }
    static size_t recSize(const recursion& x) {
      return size(*reinterpret_cast<const recursive<T>*>(x.value));
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
      return write(p, *reinterpret_cast<const recursive<T>*>(x.value));
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
#define DEFINE_HSTORE_TYPE_ALIAS(PRIV_ATY, PRIV_REPTY) \
  struct PRIV_ATY { \
    typedef void is_hstore_alias; \
    typedef PRIV_REPTY type; \
    static const char* name() { return #PRIV_ATY; } \
    inline operator PRIV_REPTY() { return this->value; } \
    PRIV_REPTY value; \
    PRIV_ATY() : value() { } \
    PRIV_ATY(const PRIV_REPTY& x) : value(x) { } \
    constexpr PRIV_ATY(const PRIV_ATY& x) = default; \
    PRIV_ATY& operator=(const PRIV_ATY& x) = default; \
    bool operator==(const PRIV_ATY& x) const { return this->value == x.value; } \
  }

template <typename T>
  struct store<T, typename T::is_hstore_alias> {
    static void        encode(bytes* out)          { w(PRIV_HSTORE_TYCTOR_PRIM, out); ws(T::name(), out); w(true, out); store<typename T::type>::encode(out); }
    static std::string describe()                  { return T::name(); }
    static size_t      size(const T& x)            { return store<typename T::type>::size(x.value); }
    static bool        write(wpipe& p, const T& x) { return store<typename T::type>::write(p, x.value); }
  };

// store fixed-length arrays
template <typename T, size_t N>
  struct fixedArrTyDesc {
    static void encode(bytes* out) {
      w(PRIV_HSTORE_TYCTOR_FIXEDARR, out);
      store<T>::encode(out);
      w(PRIV_HSTORE_TYCTOR_SIZE, out);
      w(static_cast<long>(N), out);
    }

    static std::string describe() {
      char nf[32];
      snprintf(nf, sizeof(nf), "%lld", static_cast<unsigned long long>(N));
      return "[:" + store<T>::describe() + "|" + std::string(nf) + ":]";
    }
  };

template <typename T, size_t N>
  struct fixedArrMemcpyWrite {
    static size_t size(const T (&v)[N]) {
      return sizeof(T)*N;
    }
    static bool write(wpipe& p, const T (&v)[N]) {
      return p.write(reinterpret_cast<const uint8_t*>(v), sizeof(T)*N);
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

template <typename T, size_t N>
  struct store<std::array<T, N>, typename store<T>::can_memcpy> : public fixedArrTyDesc<T, N> {
    static size_t size(const std::array<T, N>& x) { return sizeof(T)*N; }
    static bool write(wpipe& p, const std::array<T, N>& x) { return p.write(reinterpret_cast<const uint8_t*>(&x), size(x)); }
  };
template <typename T, size_t N>
  struct store<std::array<T, N>, typename cannot_memcpy<T>::type> : public fixedArrTyDesc<T, N> {
    static size_t size(const std::array<T, N>& x) { size_t n = 0; for (size_t i = 0; i < N; ++i) { n += store<T>::size(x[i]); } return n; }
    static bool write(wpipe& p, const std::array<T, N>& x) { for (size_t i = 0; i < N; ++i) { if (!store<T>::write(p, x[i])) return false; } return true; }
  };

// support storage of vectors
template <typename T>
  struct store<std::vector<T>, typename store<T>::can_memcpy> {
    static void encode(bytes* out) { w(PRIV_HSTORE_TYCTOR_ARR, out); store<T>::encode(out); }
    static std::string describe() { return "[" + store<T>::describe() + "]"; }
    static size_t size(const std::vector<T>& xs) { return sizeof(size_t) + (xs.size() * sizeof(T)); }
    static bool write(wpipe& p, const std::vector<T>& xs) { size_t n = xs.size(); return store<size_t>::write(p, n) && p.write(reinterpret_cast<const uint8_t*>(&xs[0]), n * sizeof(T)); }
  };

template <typename T>
  struct store<std::vector<T>, typename cannot_memcpy<T>::type> {
    static void encode(bytes* out) { w(PRIV_HSTORE_TYCTOR_ARR, out); store<T>::encode(out); }
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
    static void encode(bytes* out) { w(PRIV_HSTORE_TYCTOR_ARR, out); store<char>::encode(out); }
    static std::string describe() { return "[char]"; }
    static size_t size(const char* s) { return sizeof(size_t) + strlen(s); }
    static bool write(wpipe& p, const char* s) { size_t n = strlen(s); return store<size_t>::write(p, n) && p.write(reinterpret_cast<const uint8_t*>(s), n); }
  };
template <>
  struct store<char*> {
    static void encode(bytes* out) { w(PRIV_HSTORE_TYCTOR_ARR, out); store<char>::encode(out); }
    static std::string describe() { return "[char]"; }
    static size_t size(char* s) { return sizeof(size_t) + strlen(s); }
    static bool write(wpipe& p, char* s) { size_t n = strlen(s); return store<size_t>::write(p, n) && p.write(reinterpret_cast<const uint8_t*>(s), n); }
  };
template <>
  struct store<std::string> {
    static void encode(bytes* out) { w(PRIV_HSTORE_TYCTOR_ARR, out); store<char>::encode(out); }
    static std::string describe() { return "[char]"; }
    static size_t size(const std::string& s) { return sizeof(size_t) + s.size(); }
    static bool write(wpipe& p, const std::string& s) { size_t n = s.size(); return store<size_t>::write(p, n) && p.write(reinterpret_cast<const uint8_t*>(s.data()), n); }
  };

// support values carrying field names (easier to record structs without making an explicit struct type)
template <typename F, typename T>
  struct namedValue {
    T value;
    namedValue(const T& v) : value(v) { }
  };

template <typename F, typename T>
  struct store<namedValue<F,T>> {
    static void encode(bytes* out) { store<T>::encode(out); }
    static std::string describe() { return store<T>::describe(); }
    static size_t size(const namedValue<F,T>& x) { return store<T>::size(x.value); }
    static bool write(wpipe& p, const namedValue<F,T>& x) { return store<T>::write(p, x.value); }
  };

#define HNAME(N,E) ::hobbes::storage::namedValue<PRIV_HSTORE_TSTR(#N),decltype(E)>(E)

// allow inference of log statement names from payload types
typedef void (*tynamehints)(std::vector<std::string>*);
template <typename T>
  struct nameAt {
    template <typename U, std::string (*P)()> struct HasStorageName { };
    template <typename U> static std::string is(const HasStorageName<U, &U::storageName>*) { return U::storageName(); }
    template <typename U> static std::string is(...)                                       { return ""; }
  };

template <typename T>
  struct storeNames {
    static void accum(std::vector<std::string>* ns) {
      std::string n = nameAt<T>::template is<store<T>>(0);
      if (n.size() > 0) {
        ns->push_back(n);
      }
    }
  };

// make a function for describing store payload types
template <typename T>
  struct hstore_argl_name {
    static const bool isTuple = true;
    static void nameDesc(const char* prefix, size_t idx, char* buffer, size_t buflen) {
      snprintf(buffer, buflen, prefix, static_cast<unsigned long long>(idx));
    }
  };
template <typename F, typename T>
  struct hstore_argl_name<namedValue<F,T>> {
    static const bool isTuple = false;
    static void nameDesc(const char*, size_t, char* buffer, size_t buflen) {
      strncpy(buffer, F::str(), buflen);
    }
  };

template <typename ... Ts>
  struct hstore_payload_types {
    typedef unit head_type;
    static const size_t count = 0;
    static const bool isTuple = true;
    static void encode(const char*,size_t,bytes*) {}
    static std::string describe(const char*,size_t) { return ""; }
    static void nameHints(std::vector<std::string>*) {}
  };
template <typename T, typename ... Ts>
  struct hstore_payload_types<T,Ts...> {
    typedef T head_type;
    static const size_t count = 1 + hstore_payload_types<Ts...>::count;
    static const bool isTuple = hstore_argl_name<T>::isTuple && hstore_payload_types<Ts...>::isTuple;
    static void encode(const char* fmt, size_t f, bytes* out) {
      char fn[128];
      hstore_argl_name<T>::nameDesc(fmt, f, fn, sizeof(fn));
      ws(fn, out);
      w(static_cast<int>(-1), out);
      store<T>::encode(out);
      hstore_payload_types<Ts...>::encode(fmt, f+1, out);
    }
    static std::string describe(const char* fmt, size_t f) {
      if (fmt) {
        char fn[128];
        hstore_argl_name<T>::nameDesc(fmt, f, fn, sizeof(fn));
        return "," + std::string(fn) + ":" + store<T>::describe() + hstore_payload_types<Ts...>::describe(fmt, f+1);
      } else {
        return "*" + store<T>::describe() + hstore_payload_types<Ts...>::describe(0,0);
      }
    }
    static void nameHints(std::vector<std::string>* ns) {
      storeNames<T>::accum(ns);
      hstore_payload_types<Ts...>::nameHints(ns);
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
      bool   isTuple    = TyList::isTuple;

      if (!isTuple) {
        w(PRIV_HSTORE_TYCTOR_STRUCT, e);
        w(localArity, e);
        TyList::encode("field%lld", 0, e);
      } else {
        // don't bother to entuple log argument lists if there's just one argument
        switch (localArity) {
        case 0:
          encode_primty("unit", e);
          break;
        case 1:
          store<typename TyList::head_type>::encode(e);
          break;
        default:
          w(PRIV_HSTORE_TYCTOR_STRUCT, e);
          w(localArity, e);
          TyList::encode(".f%lld", 0, e);
          break;
        }
      }
    }

    if (d) {
      if (TyList::isTuple) {
        *d = TyList::describe(0,0).substr(1);
      } else {
        *d = "{" + TyList::describe("field%lld",0).substr(1) + "}";
      }
    }
  }

template <uint32_t* X>
  uint32_t forceRegistration() { return *X; }
template <typename Group, Group* G, typename File, uint32_t Line, typename StmtName, size_t Flags, typename FormatStr, typename ArgTyList>
  struct StorageStatement {
    static uint32_t id;
    StorageStatement() { forceRegistration<&id>(); }
  };
template <typename Group, Group* G, typename File, uint32_t Line, typename StmtName, size_t Flags, typename FormatStr, typename ArgTyList>
  uint32_t StorageStatement<Group, G, File, Line, StmtName, Flags, FormatStr, ArgTyList>::id = G->allocateStorageStatement(File::str(), Line, StmtName::str(), Flags, FormatStr::str(), &makeTyDescF<ArgTyList>, &ArgTyList::nameHints);

// allow static registration of storage points
enum CommitMethod {
  AutoCommit = 0,
  ManualCommit
};

template <typename Name, CommitMethod cm>
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
  PipeQOS            qos;
  size_t             mempages;
  int                mqserver;
  WaitPolicy         wp;
  bool               enabled;

  static thread_local wpipe* pipe;

  constexpr StorageGroup(size_t pagec, const PipeQOS qos) : StorageGroup(pagec, qos, Platform) {}
  constexpr StorageGroup(size_t pagec, const PipeQOS qos, const WaitPolicy wp)
    : statements(nullptr), qos(qos), mempages(pagec), mqserver(-1), wp(wp), enabled(true) {}

  ~StorageGroup() {
    delete this->statements;
    delete this->pipe;
  }

  void prepareMeta(bytes* meta) {
    if (!this->statements) return; // if nothing to record, nothing to prepare

    w(HSTORE_VERSION, meta);
    w(static_cast<int>(this->qos), meta);
    w(static_cast<int>(cm), meta);

    // write the count of storage statements, then each statement's static data
    w(static_cast<uint32_t>(this->statements->size()), meta);
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
    if (PRIV_HSTORE_LIKELY(this->pipe != 0)) {
      return *this->pipe;
    }

    // allocate a shared memory queue for this group, register it with the group server
    bytes meta;
    prepareMeta(&meta);
    
    size_t pagesz = sysconf(_SC_PAGESIZE);
    size_t pagec  = 1 + (meta.size() / pagesz) + std::max<size_t>(this->mempages, 10);

    this->pipe = new wpipe(new writer(meta, sharedMemName(Name::str()), pagesz, pagec, wp), this->qos, /*if blocked 1ms*/ 1000000L, /*reconnect if needed*/ [&](){ reconnectSHM(&this->mqserver, Name::str(), wp); });
    try {
      registerSHMAlloc(&this->mqserver, Name::str(), wp);
    } catch (...) {
      // unreliable mode can accept connection failures
      if (this->qos != Unreliable) {
        throw;
      }
    }
    return *this->pipe;
  }

  inline void init() {
    out();
  }

  inline void commit() {
    out().commit();
  }

  inline void rollback() {
    out().rollback();
  }

  inline void enable() {
    this->enabled = true;
  }

  inline void disable() {
    this->enabled = false;
  }

  // use the log statement name provided by the user (if applicable)
  // if the name is auto:N, then use either N or a name inferred from the type being logged
  // for auto-assigned names, append an incrementing counter if necessary to make the name unique
  static bool isAutoName(const std::string& x) {
    return x.substr(0, 5) == "auto:";
  }

  static std::string decideName(const std::string& x, tynamehints tnhints) {
    if (!isAutoName(x)) {
      return x;
    } else {
      std::string n;
      std::vector<std::string> ns;
      tnhints(&ns);
      if (ns.size() == 0) {
        n = x.substr(5);
      } else {
        std::ostringstream nss;
        nss << ns[0];
        for (size_t i = 1; i < ns.size(); ++i) {
          nss << "_" << ns[i];
        }
        n = nss.str();
      }
      return n;
    }
  }

  static std::string incrAutoName(const std::string& n) {
    std::ostringstream fnss;
    static std::map<std::string, std::atomic<size_t>*> ctrs;
    auto i = ctrs.find(n);
    if (i != ctrs.end()) {
      fnss << n << *i->second;
      ++*i->second;
    } else {
      fnss << n << "1";
      ctrs[n] = new std::atomic<size_t>(2);
    }
    return fnss.str();
  }

  uint32_t addStatement(const std::string file, uint32_t line, const std::string& name, size_t flags, const std::string& fmtstr, tydescfn tdesc) {
    if (this->statements->find(name) != this->statements->end()) {
      std::cerr
        << "fatal error: duplicate log name in use (" << name << ")"
        << std::endl;

      exit(-1);
    }

    StmtData d;
    d.tdesc  = tdesc;
    d.flags  = flags;
    d.fmtstr = fmtstr;
    d.file   = file;
    d.line   = line;
    d.id     = static_cast<uint32_t>(this->statements->size());

    (*this->statements)[name] = d;
    return d.id;
  }

  uint32_t allocateStorageStatement(const std::string& file, uint32_t line, const std::string& namet, size_t flags, const std::string& fmtstr, tydescfn tdesc, tynamehints tnhints) {
    if (!this->statements) {
      this->statements = new StorageStatements();
    }
    
    auto name = decideName(namet, tnhints);
    auto s = this->statements->find(name);
    if (s != this->statements->end()) {
      // this statement has already been added
      // make sure that all identifying parameters are consistent
      bytes ety, xty;
      s->second.tdesc(&ety,0);
      tdesc(&xty,0);
      if (ety != xty) {
        if (isAutoName(namet)) {
          return addStatement(file, line, incrAutoName(name), flags, fmtstr, tdesc);
        } else {
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
      }

      if (s->second.fmtstr != fmtstr) {
        if (isAutoName(namet)) {
          return addStatement(file, line, incrAutoName(name), flags, fmtstr, tdesc);
        } else {
          std::cerr
            << "fatal error: incompatible format strings for store statement '" << name << "' between:\n"
            << "  " << s->second.file << ":" << s->second.line << " (" << s->second.fmtstr << ")\n"
            << "and\n"
            << "  " << file << ":" << line << " (" << fmtstr << ")\n"
            << std::endl;

          exit(-1);
        }
      }

      if (s->second.flags != flags) {
        if (isAutoName(namet)) {
          return addStatement(file, line, incrAutoName(name), flags, fmtstr, tdesc);
        } else {
          std::cerr
            << "fatal error: incompatible storage flags for store statement '" << name << "' between:\n"
            << "  " << s->second.file << ":" << s->second.line << " (" << s->second.flags << ")\n"
            << "and\n"
            << "  " << file << ":" << line << " (" << flags << ")\n"
            << std::endl;

          exit(-1);
        }
      }

      return s->second.id;
    }

    // this must be a new statement
    return addStatement(file, line, name, flags, fmtstr, tdesc);
  }
};
template <typename Name, CommitMethod cm>
  thread_local wpipe* StorageGroup<Name, cm>::pipe = 0;

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

template <typename GName, typename ... Ts>
  inline bool write(StorageGroup<GName, AutoCommit>* g, uint32_t id, const Ts&... xs) {
    wpipe& p = g->out();
    if (!g->enabled) {
      return false;
    }
    if (!p.hasSpaceFor(sizeof(uint32_t) + serialize_values<Ts...>::size(xs...))) {
      g->commit();
    }
    return store<uint32_t>::write(p, id) &&
           serialize_values<Ts...>::write(p, xs...);
  }
template <typename GName, typename ... Ts>
  inline bool write(StorageGroup<GName, ManualCommit>* g, uint32_t id, const Ts&... xs) {
    wpipe& p = g->out();
    return g->enabled &&
           store<uint32_t>::write(p, id) &&
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
#define DECLARE_STORAGE_GROUP(NAME, cm)                     extern ::hobbes::storage::StorageGroup<PRIV_HSTORE_TSTR(#NAME),cm> NAME
#define DEFINE_STORAGE_GROUP(NAME, pagec, qos, cm, ARGS...) ::hobbes::storage::StorageGroup<PRIV_HSTORE_TSTR(#NAME),cm> NAME(pagec, qos, ## ARGS)

// record some data
#define APPLY_HSTORE_STMT(GROUP, NAME, FLAGS, FMTSTR, ARGS...) \
  ::hobbes::storage::write(&GROUP, ({static_assert(::hobbes::storage::maxVarRef(FMTSTR) <= decltype(::hobbes::storage::makePayloadTypes(ARGS))::count, "Log format string and payload arity mismatch"); ::hobbes::storage::StorageStatement<decltype(GROUP),&GROUP,PRIV_HSTORE_TSTR(__FILE__),__LINE__,PRIV_HSTORE_TSTR(#NAME),FLAGS,PRIV_HSTORE_TSTR(FMTSTR),decltype(::hobbes::storage::makePayloadTypes(ARGS))>::id;}), ## ARGS)

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

  inline bool operator==(const statement& rhs) const {
    return this->name   == rhs.name   &&
           this->flags  == rhs.flags  &&
           this->fmtstr == rhs.fmtstr &&
           this->file   == rhs.file   &&
           this->line   == rhs.line   &&
           this->id     == rhs.id     &&
           this->type   == rhs.type;
  }
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
    return rs(md, o, sizeof(T), reinterpret_cast<uint8_t*>(t));
  }

inline size_t rs(const reader::MetaData& md, size_t o, std::string* s) {
  size_t n = 0;
  o = r(md, o, &n);
  s->resize(n);
  return rs(md, o, n, reinterpret_cast<uint8_t*>(&(*s)[0]));
}

inline size_t rs(const reader::MetaData& md, size_t o, bytes* s) {
  size_t n = 0;
  o = r(md, o, &n);
  s->resize(n);
  return rs(md, o, n, &(*s)[0]);
}

class Transaction {
public:
  // a persistent transaction to retain log data between consumer sessions (if necessary)
  Transaction(std::string fname) : fd(-1), file_size(0), map_size(0), data(0), data_size(0), wi(0), ri(0) {
    for (size_t i = 0; i < fname.size(); ++i) {
      switch (fname[i]) {
      case '/': fname[i]='_'; break;
      case ':': fname[i]='_'; break;
      case '[': fname[i]='_'; break;
      case ']': fname[i]='_'; break;
      default:                break;
      }
    }
    fname = "." + fname + ".txn";
    this->fd = ::open(fname.c_str(), O_RDWR | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);

    if (this->fd < 0) {
      throw std::runtime_error("Unable to open transaction persistence file: " + fname + " (" + std::string(strerror(errno)) + ")");
    }

    struct stat sb;
    if (::fstat(this->fd, &sb) < 0) {
      close(this->fd);
      throw std::runtime_error("Failed to stat transaction persistence file: " + fname + " (" + std::string(strerror(errno)) + ")");
    }
    truncMapFile(sb.st_size == 0 ? /*1MB*/1048576 : sb.st_size);
  }

  // a non-persistent transaction over constant data (just used by remote consumers)
  Transaction(const uint8_t* d, size_t len) : fd(-1), file_size(len), map_size(len), data(const_cast<uint8_t*>(d)), data_size(0), wi(0), ri(0) {
    this->data_size = &this->map_size;
  }

  bool canRead(size_t x) const {
    return (this->ri+x) <= *this->data_size;
  }

  const uint8_t* ptr() const {
    return this->data + this->ri;
  }

  void skip(size_t d) {
    this->ri += d;
  }

  template <typename T>
    const T* read() {
      auto p = reinterpret_cast<const T*>(ptr());
      skip(sizeof(T));
      return p;
    }

  size_t size() const {
    return *this->data_size;
  }

  void clear() {
    *this->data_size = 0;
    this->wi = 0;
    this->ri = 0;
  }

  bool readToCompletion(rpipe& p, size_t timeoutNS, const std::function<void()>& timeoutF) {
    static const size_t blockSize = 1024;

    if (this->wi + blockSize >= this->file_size) {
      truncMapFile(this->file_size + /*1MB*/1048576);
    }

    uint8_t txnFlag = 0;
    this->wi += p.read(this->data + this->wi, blockSize, &txnFlag, timeoutNS, timeoutF);
    *this->data_size = this->wi;

    switch (txnFlag) {
    case PRIV_HSTORE_PAGE_STATE_ROLLBACK:
      clear();
      return false;
    case PRIV_HSTORE_PAGE_STATE_COMMIT: {
      return true;
    }
    default:
      return false;
    }
  }
private:
  int    fd;
  size_t file_size;
  size_t map_size;

  uint8_t* data;
  size_t*  data_size;
  size_t   wi;
  size_t   ri;

  void truncMapFile(size_t sz) {
    if (::ftruncate(this->fd, sz) == -1) {
      throw std::runtime_error("Failed to expand transaction persistence file");
    }
    this->file_size = sz;

    if (sz > this->map_size) {
      if (this->data) { ::munmap(this->data-sizeof(size_t), this->map_size); }

      this->map_size = align<size_t>(sz, /*10MB*/10485760);
      this->data = reinterpret_cast<uint8_t*>(mmap(0, this->map_size, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_POPULATE, this->fd, 0));
      if (this->data == MAP_FAILED) {
        throw std::runtime_error("Failed to map transaction persistence file");
      }
      this->data_size = reinterpret_cast<size_t*>(this->data);
      this->data      += sizeof(size_t);
    }
  }
};

inline void runReadProcessWithTimeout(const QueueConnection& qc, const WaitPolicy wp, const std::function<std::function<void(Transaction&)>(PipeQOS, CommitMethod, const statements&)>& initF, size_t timeoutNS, const std::function<void()>& timeoutF) {
  Transaction txn(qc.shmname);
  reader      rd(qc, wp);
  rpipe       p(&rd);

  // initialize
  reader::MetaData md = rd.meta();

  uint32_t hstoreVersion = 0;
  size_t o = r(md, 0, &hstoreVersion);
  if (hstoreVersion != HSTORE_VERSION) {
    throw std::runtime_error("Can't read storage data from incompatible process");
  }

  // read group flags/settings
  int qos=0, cm=0;
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

  auto txnF = initF(static_cast<PipeQOS>(qos), static_cast<CommitMethod>(cm), ss);

  // read transactions and call back into user code
  while (true) {
    if (txn.readToCompletion(p, timeoutNS, timeoutF)) {
      txnF(txn);
      txn.clear();
    }
  }
}

inline void runReadProcess(const QueueConnection& qc, const WaitPolicy wp, const std::function<std::function<void(Transaction&)>(PipeQOS, CommitMethod, const statements&)>& initF) {
  runReadProcessWithTimeout(qc, wp, initF, 0, [](){});
}

}}

#endif
