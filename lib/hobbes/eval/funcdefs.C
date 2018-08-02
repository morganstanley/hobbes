#include <hobbes/eval/funcdefs.H>
#include <hobbes/util/region.H>
#include <hobbes/hobbes.H>

#include <hobbes/db/file.H>
#include <hobbes/util/perf.H>
#include <hobbes/util/time.H>
#include <hobbes/util/codec.H>

#include <stack>
#include <iostream>
#include <iomanip>
#include <strings.h>
#include <zlib.h>

namespace hobbes {

static __thread region* threadRegionp = 0;

typedef std::pair<std::string, region*> NamedRegion;
typedef std::vector<NamedRegion> Regions;
static __thread Regions* threadRegionsp = 0;
static __thread size_t   currentRegion = 0;

region& threadRegion() {
  if (threadRegionp == 0) {
    threadRegionp  = new region(32768 /* min page size = 32K */);
    threadRegionsp = new Regions();
    threadRegionsp->push_back(NamedRegion("scratch", threadRegionp));
    currentRegion = 0;
  }
  return *threadRegionp;
}

static Regions& threadRegions() {
  if (threadRegionsp == 0) {
    threadRegion();
  }
  return *threadRegionsp;
}

size_t addThreadRegion(const std::string& n, region* r) {
  Regions& rs = threadRegions();
  rs.push_back(NamedRegion(n, r));
  return rs.size() - 1;
}

size_t findThreadRegion(const std::string& n) {
  const Regions& rs = threadRegions();
  for (size_t i = 0; i < rs.size(); ++i) {
    if (rs[i].first == n) {
      return i;
    }
  }
  return -1;
}

void removeThreadRegion(size_t n) {
  Regions& rs = threadRegions();
  if (n == rs.size()-1) {
    rs.resize(n);
  } else if (n < rs.size()) {
    rs[n].second = 0;
  }
}

size_t setThreadRegion(size_t n) {
  const Regions& rs = threadRegions();
  if (n < rs.size()) {
    threadRegionp = rs[n].second;
  } else {
    throw std::runtime_error("Invalid region : " + str::from(n));
  }

  size_t r = currentRegion;
  currentRegion = n;
  return r;
}

size_t makeMemRegion(const array<char>* n) {
  return addThreadRegion(makeStdString(n), new region(32768));
}

char* memalloc(size_t n) {
  return reinterpret_cast<char*>(threadRegion().malloc(n));
}

char* memallocz(size_t n) {
  char* r = reinterpret_cast<char*>(threadRegion().malloc(n));
  memset(r, 0, n);
  return r;
}

DEFINE_STRUCT(RegionState,
  (const array<char>*, name),
  (size_t,             id),
  (size_t,             allocated),
  (size_t,             used),
  (size_t,             wasted)
);

array<RegionState>* getMemoryPool() {
  const Regions& tr = threadRegions();

  array<RegionState>* rsa = makeArray<RegionState>(tr.size());

  for (size_t i = 0; i < tr.size(); ++i) {
    RegionState& rs = rsa->data[i];
    
    rs.name = makeString(tr[i].first);
    rs.id   = i;

    if (const region* r = tr[i].second) {
      rs.allocated = r->allocated();
      rs.used      = r->used();
      rs.wasted    = r->wasted();
    } else {
      rs.allocated = static_cast<size_t>(-1);
      rs.used      = static_cast<size_t>(-1);
      rs.wasted    = static_cast<size_t>(-1);
    }
  }
  return rsa;
}

std::string showMemoryPool() {
  str::seqs tbl;
  tbl.resize(5);
  tbl[0].push_back("name");
  tbl[1].push_back("id");
  tbl[2].push_back("allocated");
  tbl[3].push_back("used");
  tbl[4].push_back("wasted");
  
  const Regions& rs = threadRegions();
  for (size_t i = 0; i < rs.size(); ++i) {
    tbl[0].push_back(rs[i].first);
    tbl[1].push_back(str::from(i));
    if (const region* r = rs[i].second) {
      tbl[2].push_back(str::showDataSize(r->allocated()));
      tbl[3].push_back(str::showDataSize(r->used()));
      tbl[4].push_back(str::showDataSize(r->wasted()));
    } else {
      tbl[2].push_back("<defunct>");
      tbl[3].push_back("<defunct>");
      tbl[4].push_back("<defunct>");
    }
  }
  return str::showRightAlignedTable(tbl);
}

void printMemoryPool() {
  std::cout << showMemoryPool() << std::flush;
}

void resetMemoryPool() {
  threadRegion().clear();
}

void clearMemoryPool() {
  threadRegion().clear();
}

void abortAtMemUsage(size_t maxsz) {
  threadRegion().abortAtMemCeiling(maxsz);
}

scoped_pool_reset::~scoped_pool_reset() {
  resetMemoryPool();
}

const array<char>* makeString(region& m, const char* s, size_t len) {
  array<char>* r = reinterpret_cast<array<char>*>(m.malloc(sizeof(long) + len));
  r->size = len;
  memcpy(r->data, s, len);
  return r;
}

const array<char>* makeString(region& m, const char* s) {
  return makeString(m, s, strlen(s));
}

const array<char>* makeString(region& m, const std::string& s) {
  return makeString(m, s.data(), s.size());
}

const array<char>* makeString(const char* s, size_t len) {
  return makeString(threadRegion(), s, len);
}

const array<char>* makeString(const std::string& x) {
  return makeString(x.data(), x.size());
}

std::string makeStdString(const array<char>* x) {
  return std::string(x->data, x->size);
}

template <typename T>
  struct maybe {
    typedef variant<unit, T> ty;

    static const maybe<T>::ty* nothing() {
      return new (memalloc(sizeof(ty))) ty(unit());
    }

    static const maybe<T>::ty* just(const T& x) {
      return new (memalloc(sizeof(ty))) ty(x);
    }
  };

const array<char>* showChar(char c) {
  return makeString("'" + str::from(c) + "'");
}

const maybe<char>::ty* readChar(const array<char>* x) {
  if (x->size == 3 && x->data[0] == '\'' && x->data[2] == '\'') {
    return maybe<char>::just(x->data[1]);
  } else {
    return maybe<char>::nothing();
  }
}

const array<char>* showByteV(unsigned char b) {
  return makeString(str::hex(b));
}

const array<char>* showByte(unsigned char b) {
  return makeString("0X" + str::hex(b));
}

const maybe<unsigned char>::ty* readByte(const array<char>* x) {
  if (x->size == 4 && x->data[0] == '0' && x->data[1] == 'X' && str::isNyb(x->data[2]) && str::isNyb(x->data[3])) {
    return maybe<unsigned char>::just(str::denyb(x->data[2]) * 16 + str::denyb(x->data[3]));
  } else {
    return maybe<unsigned char>::nothing();
  }
}

template <typename T>
  const typename maybe<T>::ty* readISV(const array<char>* x) {
    T r;
    if (str::to(&x->data[0], &x->data[0] + x->size, &r)) {
      return maybe<T>::just(r);
    } else {
      return maybe<T>::nothing();
    }
  }

const array<char>* showShort(short s) {
  return makeString(str::from(s) + "S");
}

const maybe<short>::ty* readShort(const array<char>* x) {
  return readISV<short>(x);
}

const array<char>* showInt(int x) {
  return makeString(str::from(x));
}

const maybe<int>::ty* readInt(const array<char>* x) {
  return readISV<int>(x);
}

const array<char>* showLong(long x) {
  return makeString(str::from(x));
}

const maybe<long>::ty* readLong(const array<char>* x) {
  return readISV<long>(x);
}

const array<char>* showFloat(float x, int p) {
  if (p <= 0) {
    return makeString(str::from(x)+"f");
  } else {
    std::ostringstream ss;
    ss.precision(p);
    ss << x;
    return makeString(ss.str()+"f");
  }
}

const maybe<float>::ty* readFloat(const array<char>* x) {
  return readISV<float>(x);
}

const array<char>* showDouble(double x, int p) {
  std::ostringstream ss;
  if (p > 0) {
    ss << std::setiosflags(std::ios::fixed);
    ss.precision(p);
  }
  ss << x;
  return makeString(ss.str());
}

const maybe<double>::ty* readDouble(const array<char>* x) {
  return readISV<double>(x);
}

const array<char>* showString(std::string* x) {
  return makeString("\"" + *x + "\"");
}

const array<char>* showTimespanV(timespanT x) {
  return makeString(showTimespan(x.value));
}

const array<char>* showTimeV(timeT x) {
  return makeString(showTime(x.value));
}

const array<char>* showDateTimeV(datetimeT x) {
  return makeString(showDateTime(x.value));
}

inline std::string showUS(int64_t us) {
  return
   ((us < 10)     ? "00000" :
    (us < 100)    ? "0000"  :
    (us < 1000)   ? "000"   :
    (us < 10000)  ? "00"    :
    (us < 100000) ? "0"     :
                    "")
   + str::from(us);
}

const array<char>* formatTimeV(const array<char>* fmt, long tus) {
  int64_t s   = tus / (1000 * 1000);
  int64_t us  = tus % (1000 * 1000);
  std::string sfmt = str::replace<char>(makeStdString(fmt), "%us", showUS(us));
  static char buf[256];
  strftime(buf, sizeof(buf), sfmt.c_str(), localtime(reinterpret_cast<time_t*>(&s)));
  return makeString(buf);
}

const array<char>* formatDateTime(const array<char>* fmt, datetimeT x) {
  return formatTimeV(fmt, x.value);
}

datetimeT now() {
  return datetimeT(time() / 1000);
}

datetimeT truncDate(datetimeT t) {
  return datetimeT(dateFromDateTime(t.value));
}

timeT truncTime(datetimeT t) {
  return timeT(timeFromDateTime(t.value));
}

datetimeT datetimeAt(datetimeT dt, timeT t) {
  return datetimeT(dateFromDateTime(dt.value) + (timeFromDateTime(t.value) - mkTime(0,0,0,0)));
}

timespanT gmtoffset(datetimeT x) {
  time_t xt = x.value / (1000*1000);
  struct tm xtm;
  localtime_r(&xt, &xtm);
  return xtm.tm_gmtoff * (1000*1000);
}

std::ostringstream& stdoutBuffer() {
  static std::ostringstream ss;
  return ss;
}

void stdoutBufferSwap(std::ostream* os) {
  static std::streambuf* b = std::cout.rdbuf();
  if (os) {
    std::cout.rdbuf(os->rdbuf());
  } else {
    std::cout.rdbuf(b);
  }
}

void captureStdout() {
  stdoutBufferSwap(&stdoutBuffer());
}

const array<char>* releaseStdout() {
  const array<char>* result = makeString(stdoutBuffer().str());
  stdoutBuffer().str("");
  stdoutBufferSwap(0);
  return result;
}

void putStr(array<char>* x) {
  std::cout.write(x->data, x->size);
}

size_t cstrlen(char* x) {
  return strlen(x);
}

char cstrelem(char* x, size_t i) {
  return x[i];
}

long strsize(std::string* s) {
  return s->size();
}

char strelem(std::string* s, long i) {
  return (*s)[i];
}

void stdstringAssign(std::string* lhs, array<char>* rhs) {
  lhs->assign(rhs->data, rhs->size);
}

double random(double low, double high) {
  return low + ((high - low) * (static_cast<double>(rand()) / static_cast<double>(RAND_MAX)));
}

long lrand(long low, long high) { return low + (rand() % (high - low)); }
long lceil(double x) { return ceil(x); }
long lfloor(double x) { return floor(x); }
long truncd(double x) { return x; }

void dbglog(const std::string&);
void failvarmatch(const array<char>* file, size_t line, const array<char>* txt, char* addr) {
  std::ostringstream ss;
  ss << "FATAL ERROR: Unexpected variant match failure on " << reinterpret_cast<void*>(addr) << " at " << makeStdString(file) << ":" << line << " ('" << makeStdString(txt) << "')";
  dbglog(ss.str());
  std::cerr << ss.str() << std::endl;
  throw std::runtime_error(ss.str());
}

void dumpBytes(char* d, long len) {
  for (long i = 0; i < len; ++i) {
    std::cout << str::hex(static_cast<unsigned char>(d[i])) << " ";
  }
  std::cout << std::endl;
}

// support fd reading/writing
//  (mark FDs as bad if there are errors rather than raising an exception and killing the process)
std::set<int>& badFDs() {
  static __thread std::set<int>* bfds = 0;
  if (!bfds) {
    bfds = new std::set<int>();
  }
  return *bfds;
}

void markBadFD(int fd) {
  badFDs().insert(fd);
}

bool unmarkBadFD(int fd) {
  std::set<int>& bfds = badFDs();
  auto bfd = bfds.find(fd);
  if (bfd == bfds.end()) {
    return false;
  } else {
    bfds.erase(bfd);
    return true;
  }
}

void readOrMark(int fd, char* b, size_t sz) {
  try {
    fdread(fd,b,sz);
  } catch (std::exception&) {
    markBadFD(fd);
    memset(b,0,sz);
  }
}

template <typename T>
  T fdRead(int fd) {
    T x = T();
    readOrMark(fd, reinterpret_cast<char*>(&x), sizeof(T));
    return x;
  }

void writeOrMark(int fd, const char* b, size_t sz) {
  try {
    fdwrite(fd, b, sz);
  } catch (std::exception&) {
    markBadFD(fd);
  }
}

template <typename T>
  void fdWrite(int fd, T x) {
    writeOrMark(fd, reinterpret_cast<const char*>(&x), sizeof(T));
  }

void fdWriteChars(int fd, const array<char>* cs) {
  writeOrMark(fd, cs->data, cs->size);
}

void fdWriteBytes(int fd, const array<unsigned char>* bs) {
  writeOrMark(fd, reinterpret_cast<const char*>(bs->data), bs->size);
}

/***********
 * bind to basic compress/decompress logic in zlib
 *   (the extra bit of logic here is to compress the original array length into the compressed output)
 ***********/
uint8_t compressLength(size_t len, uint8_t slen[9]) {
  // can we fit this length in one byte?
  if (len <= 0x3f) {
    slen[0] = static_cast<uint8_t>(len << 2);
    return 1;
  }

  // can we fit this length in two bytes?
  if (len <= 0x3fff) {
    *reinterpret_cast<uint16_t*>(slen) = 1 | static_cast<uint16_t>(len << 2);
    return 2;
  }

  // can we fit this length in four bytes?
  if (len <= 0x3fffffff) {
    *reinterpret_cast<uint32_t*>(slen) = 2 | static_cast<uint32_t>(len << 2);
    return 4;
  }

  // too big, we'll fall back on expanding by a byte
  slen[0] = 3;
  *reinterpret_cast<uint64_t*>(&slen[1]) = len;
  return 9;
}

uint8_t uncompressLength(const uint8_t* data, size_t* len) {
  switch (*data & 3) {
  case 0:
    *len = *data >> 2;
    return 1;
  case 1:
    *len = *reinterpret_cast<const uint16_t*>(data) >> 2;
    return 2;
  case 2:
    *len = *reinterpret_cast<const uint32_t*>(data) >> 2;
    return 4;
  default:
    *len = *reinterpret_cast<const uint64_t*>(data+1);
    return 9;
  }
}

size_t crossZLibCompressBound(size_t t) {
  // zlib's "compressBound" function isn't available in some versions
  return static_cast<size_t>(ceil(static_cast<double>(t) * 1.001)) + 12;
}

const array<uint8_t>* compressBytes(const array<uint8_t>* bs) {
  uint8_t slen[9];
  uint8_t slensz = compressLength(bs->size, slen);

  array<uint8_t>* r = makeArray<uint8_t>(slensz + crossZLibCompressBound(bs->size));
  memcpy(r->data, slen, slensz);

  size_t rlen = r->size - slensz;
  if (compress2(&r->data[slensz], reinterpret_cast<uLongf*>(&rlen), bs->data, static_cast<uLongf>(bs->size), Z_BEST_COMPRESSION) == Z_OK) {
    r->size = slensz + rlen;
  } else {
    r->size = 0;
  }
  return r;
}

const array<uint8_t>* uncompressBytes(const array<uint8_t>* bs) {
  size_t  ulen;
  uint8_t ulensz = uncompressLength(bs->data, &ulen);

  array<uint8_t>* r = makeArray<uint8_t>(ulen);
  if (uncompress(r->data, reinterpret_cast<uLongf*>(&r->size), bs->data + ulensz, bs->size - ulensz) != Z_OK) {
    r->size = 0;
  }
  return r;
}

void runEvery(timespanT dt, bool (*pf)()) {
  addTimer(pf, dt.value/1000);
}

// bindings for all std::vectors
size_t      vectorSize(const std::vector<uint8_t>& xs, size_t esize) { return xs.size()/esize; }
const char* vectorData(const std::vector<uint8_t>& xs) { return reinterpret_cast<const char*>(&xs[0]); }

void initStdFuncDefs(cc& ctx) {
  ctx.bind("malloc",                &memalloc);
  ctx.bind("mallocz",               &memallocz);
  ctx.bind("printMemoryPool",       &printMemoryPool);
  ctx.bind("getMemoryPool",         &getMemoryPool);
  ctx.bind("unsafeSetRegion",       &setThreadRegion);
  ctx.bind("unsafeMakeMemRegion",   &makeMemRegion);
  ctx.bind("unsafeClearMemoryPool", &clearMemoryPool);
  ctx.bind("unsafeAbortAtMemUsage", &abortAtMemUsage);

  ctx.bind("showChar",     &showChar);
  ctx.bind("showByte",     &showByte);
  ctx.bind("showByteV",    &showByteV);
  ctx.bind("showShort",    &showShort);
  ctx.bind("showInt",      &showInt);
  ctx.bind("showLong",     &showLong);
  ctx.bind("showFloat",    &showFloat);
  ctx.bind("showDouble",   &showDouble);
  ctx.bind("showString",   &showString);
  ctx.bind("showTimespan", &showTimespanV);
  ctx.bind("showTime",     &showTimeV);
  ctx.bind("showDateTime", &showDateTimeV);
  ctx.bind("strftime",     &formatTimeV);
  ctx.bind("strfdatetime", &formatDateTime);
  ctx.bind("now",          &now);
  ctx.bind("date",         &truncDate);
  ctx.bind("time",         &truncTime);
  ctx.bind("datetimeAt",   &datetimeAt);
  ctx.bind("gmtoffset",    &gmtoffset);

  ctx.bind("captureStdout", &captureStdout);
  ctx.bind("putStr",        &putStr);
  ctx.bind("releaseStdout", &releaseStdout);

  ctx.bind("readChar",   &readChar);
  ctx.bind("readByte",   &readByte);
  ctx.bind("readShort",  &readShort);
  ctx.bind("readInt",    &readInt);
  ctx.bind("readLong",   &readLong);
  ctx.bind("readFloat",  &readFloat);
  ctx.bind("readDouble", &readDouble);

  // std::string* assignment (dangerous and hidden!)
  ctx.bind("stdstringAssign", &stdstringAssign);

  // a source of randomness (maybe worth revisiting!)
  srand(::time(0));
  ctx.bind("random", &random);
  ctx.bind("lrand",  &lrand);
  ctx.bind("ceil",   &lceil);
  ctx.bind("floor",  &lfloor);
  ctx.bind("truncd", &truncd);

  // this should never be called, it's only here to do something in the event of variant tag match failure
  ctx.bind(".failvarmatch", &failvarmatch);

  // string comparisons
  ctx.bind("cstrlen", &cstrlen);
  ctx.bind("cstrelem", &cstrelem);

  // dump some bytes
  ctx.bind(".dumpBytes", &dumpBytes);

  // block codecs for primitives (maybe just emit reasonable code for these?)
  ctx.bind("fdReadBool",    &fdRead<bool>);
  ctx.bind("fdWriteBool",   &fdWrite<bool>);
  ctx.bind("fdReadByte",    &fdRead<unsigned char>);
  ctx.bind("fdWriteByte",   &fdWrite<unsigned char>);
  ctx.bind("fdReadChar",    &fdRead<char>);
  ctx.bind("fdWriteChar",   &fdWrite<char>);
  ctx.bind("fdReadShort",   &fdRead<short>);
  ctx.bind("fdWriteShort",  &fdWrite<short>);
  ctx.bind("fdReadInt",     &fdRead<int>);
  ctx.bind("fdWriteInt",    &fdWrite<int>);
  ctx.bind("fdReadLong",    &fdRead<long>);
  ctx.bind("fdWriteLong",   &fdWrite<long>);
  ctx.bind("fdReadFloat",   &fdRead<float>);
  ctx.bind("fdWriteFloat",  &fdWrite<float>);
  ctx.bind("fdReadDouble",  &fdRead<double>);
  ctx.bind("fdWriteDouble", &fdWrite<double>);

  ctx.bind("fdWriteChars", &fdWriteChars);
  ctx.bind("fdWriteBytes", &fdWriteBytes);

  // support basic zlib compression on byte arrays (why not?  we're already linking it in)
  ctx.bind("compress",   &compressBytes);
  ctx.bind("decompress", &uncompressBytes);

  ctx.bind("runEvery", &runEvery);

  ctx.bind("vectorSize", &vectorSize);
  ctx.bind("vectorData", &vectorData);
}

}

