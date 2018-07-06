/*
 * fregion : structured data file I/O, interoperable with hobbes structured file programs
 *
 *    to write a file:
 *      writer f("/path/to/file.ext");
 *      auto& s = f.series<T>("yourTableName");
 *      s(T(...)); // write a value of type T
 *
 *    to read a file (receiving values in the order they were written):
 *      reader f("/path/to/file.ext");
 *      auto& s = f.series<T>("yourTableName");
 *      T t;
 *      while (s.next(&t)) {
 *        // do something with t
 *      }
 *
 *    to write a file series recording sequencing of other series (creating the "log" or "transactions" variant of hog):
 *      writer f("/path/to/file.ext");
 *      auto& s = f.series<T>("yourTableName");
 *      auto& r = f.series<U>("yourOtherTableName");
 *      f.recordOrdering("log", s, r); // creates a series named "log" as a variant over references to s or r
 *      // write into s and r as needed
 *
 *    to read a file series sequencing other series (matching just the series of interest, not necessary to match all series nor to match in order):
 *      reader f("/path/to/file.ext");
 *      auto& log = f.ordering("log");
 *      log.match<T>("yourTableName",      [](const T& t) { ... }); // process T values in write order
 *      log.match<U>("yourOtherTableName", [](const U& u) { ... }); // process U values in write order
 *      while (log.next());                                         // read all of them across the file
 *
 *    to signal data availability to all out of process readers waiting on data:
 *      writer f("/path/to/file.ext");
 *      // write to f as needed
 *      f.signal();
 */

#ifndef HOBBES_HFREGION_H_INCLUDED
#define HOBBES_HFREGION_H_INCLUDED

#include <hobbes/reflect.H>

#include <string>
#include <vector>
#include <map>
#include <unordered_map>
#include <functional>
#include <stack>
#include <stdexcept>
#include <sstream>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <cxxabi.h>

namespace hobbes { namespace fregion {

// basic string utilities used here
namespace str {
  template <typename T>
    inline std::string from(const T& x) {
      std::ostringstream ss;
      ss << x;
      return ss.str();
    }

  // pretty-print C++ type names
  template <typename T>
    inline std::string demangle() {
      if (const char* tn = typeid(T).name()) {
        int s = 0;
        if (char* dmn = abi::__cxa_demangle(tn, 0, 0, &s)) {
          std::string r(dmn);
          free(dmn);
          return r;
        } else {
          return std::string(tn);
        }
      } else {
        return "";
      }
    }

  // tokenize strings
  typedef std::vector<std::string> seq;
  inline seq csplit(const std::string& str, const std::string& pivot) {
    typedef std::string::size_type size_type;
  
    seq       ret;
    size_type mp = 0;
  
    while (mp != std::string::npos) {
      size_type nmp = str.find(pivot, mp);
  
      if (nmp == std::string::npos) {
        ret.push_back(str.substr(mp, str.size() - mp));
        mp = nmp;
      } else {
        ret.push_back(str.substr(mp, nmp - mp));
        mp = nmp + pivot.size();
      }
    }
  
    return ret;
  }

  typedef std::pair<std::string, std::string> pair;
  inline pair rsplit(const std::string& s, const std::string& ss) {
    size_t p = s.rfind(ss);
    if (p == std::string::npos) {
      return pair("", s);
    } else {
      return pair(s.substr(0, p), s.substr(p + ss.size(), s.size()));
    }
  }

  inline std::string cdelim(const seq& ss, const std::string& d) {
    if (ss.size() > 0) {
      std::ostringstream x;
      x << ss[0];
      for (size_t i = 1; i < ss.size(); ++i) {
        x << d << ss[i];
      }
      return x.str();
    } else {
      return "";
    }
  }
}

// the file header
struct filehead {
  uint32_t magic;    // trivial sanity check for file-type
  uint16_t pagesize; // how large does this file assume that one memory page is?
  uint16_t version;  // a file format version number (incremented when incompatibilities are introduced)
};
#define HFREGION_CURRENT_FILE_FORMAT_VERSION static_cast<uint16_t>(2)
#define HFREGION_FILE_PREFIX_BYTES           static_cast<uint32_t>(0x10a1db0d)
#define HFREGION_MAX_PAGE_SIZE               static_cast<size_t>((1<<14)-1)
#define HFREGION_MIN_PAGE_SIZE               static_cast<size_t>(256)

// a single system page record
//   this will be stored as two bytes, the upper 2 bits used for storing a page type and the lower 14 used for free size in that page
struct pagetype {
  enum code {
    null        = 0, // a "null" page (used to mark the end of system page data)
    toc         = 1, // a "toc" page (a page of data about other pages)
    environment = 2, // an "environment" page (describing variable bindings)
    data        = 3  // a "data" page (raw data for bound variables)
  };

  static uint8_t encode(code    x) { return static_cast<uint8_t>(x); }
  static code    decode(uint8_t x) { return static_cast<code>   (x); }
};

struct pagedata {
  uint16_t ptfd;

  pagedata(pagetype::code ty = pagetype::toc, uint16_t sz = 0) : ptfd(encode(ty, sz)) { }
  static uint16_t encode(pagetype::code ty, uint16_t sz) { return ((pagetype::encode(ty) & 3) << 14) | (sz & 0x3FFF); }
  pagetype::code type() const { return pagetype::decode(this->ptfd >> 14); }
  uint16_t size() const { return this->ptfd & 0x3FFF; }
  void type(pagetype::code x) { this->ptfd = encode(x, size()); }
  void size(uint16_t x) { this->ptfd = encode(type(), x); }
};
typedef std::vector<pagedata> pagetable;

// the index of a page of data in the underlying file
typedef uint64_t file_pageindex_t;

// convenience for raising errors out of errno
inline void raiseSysError(const std::string& msg, const std::string& fname) {
  std::ostringstream ss;
  ss << fname << ": " << msg << " (" << strerror(errno) << ")" << std::flush;
  throw std::runtime_error(ss.str());
}

typedef std::vector<uint8_t> bytes;

// a stored name/value binding
struct binding {
  binding(const bytes& type = bytes(), size_t offset = 0, size_t boffset = 0) : type(type), offset(offset), boffset(boffset) { }

  bytes  type;    // what type of data is stored here?
  size_t offset;  // where is it stored?
  size_t boffset; // where is this binding data stored in the file?
};

typedef std::map<std::string, binding> bindingset;

// a mem-mapped file region
struct fregion {
  file_pageindex_t base_page; // the base file page for this mapping
  size_t           pages;     // the number of mapped pages
  char*            base;      // the base address of this mapped region
  size_t           used;      // the number of bytes actually used out of this mapped region (when decremented to 0 we can safely unmap)
};

// account for mappings by absolute page
typedef std::map<file_pageindex_t, fregion> fmappings;

// remember which mapped sections correspond to which pages
struct falloc {
  file_pageindex_t page; // the page that this was allocated out of
  size_t           size; // the number of bytes allocated to this thing
};

typedef std::map<char*, falloc> fallocs;

// fast page searches (to avoid linear searches of the entire page table)
typedef std::vector<file_pageindex_t>     pageseq;
typedef std::map<pagetype::code, pageseq> ptyorder;

// an image file, opened either for reading or writing
struct imagefile {
  imagefile() : fd(-1) { }

  // stable open file properties
  std::string path;
  bool        readonly;
  int         fd;
  uint16_t    page_size;
  uint16_t    version;

  // mutable incremental read/write state
  size_t file_size;
  size_t head_toc_pos; // the head position for writing TOC entries
  size_t empty_array;  // empty arrays of all types can be aliased

  // the fewest number of pages to mmap at one time
  // (this prevents us from making too many tiny mmap regions and hitting the OS map limit)
  size_t mmapPageMultiple;

  // toc page -> absolute page
  pageseq tocpages;

  // system, environment, data
  pagetable  pages;
  ptyorder   freespace;
  bindingset bindings;
  fmappings  mappings;
  fallocs    allocs;
};

// how many bytes are remaining in the page for a given index?
inline uint16_t restInPage(const imagefile* f, size_t idx) { return f->page_size - (idx % f->page_size); }

// how many pages are covered by a sequence of bytes?
inline size_t pageCount(const imagefile* f, size_t sz) { return (sz / f->page_size) + ((sz % f->page_size) > 0 ? 1 : 0); }

// basic file I/O primitives
inline void closeFile(imagefile* f) {
  if (f->fd > -1) {
    close(f->fd);
  }
  delete f;
}

inline bool loadFileSize(imagefile* f) {
  struct stat sb;
  if (::fstat(f->fd, &sb) < 0) {
    return false;
  }
  f->file_size = sb.st_size;
  return true;
}

inline void seekAbs(const imagefile* f, size_t pos) {
  if (::lseek(f->fd, pos, SEEK_SET) == -1) {
    raiseSysError("Can't seek to offset=" + str::from(pos) + " in file with size=" + str::from(f->file_size), f->path);
  }
}

inline off_t filePosition(const imagefile* f) {
  off_t r = ::lseek(f->fd, 0, SEEK_CUR);
  if (r == static_cast<off_t>(-1)) {
    raiseSysError("Can't query position in file", f->path);
  }
  return r;
}

inline void allocPages(imagefile* f, size_t pages) {
  size_t nsz = f->file_size + pages * f->page_size;
  if (::ftruncate(f->fd, nsz) == -1) {
    raiseSysError("Can't resize file", f->path);
  }
  f->file_size = nsz;
}

inline void allocPage(imagefile* f) {
  allocPages(f, 1);
}

// trivial read and write to files, assuming type T is POD
inline void fdwrite(imagefile* f, const char* x, size_t len) {
  size_t i = 0;
  while (i < len) {
    ssize_t di = write(f->fd, x + i, len - i);

    if (di < 0) {
      if (errno != EINTR) {
        raiseSysError("Failed to write " + str::from(len) + " bytes to file", f->path);
      }
    } else if (di == 0) {
      raiseSysError("Empty write error", f->path);
    } else {
      i += di;
    }
  }
}
template <typename T>
  inline void write(imagefile* f, const T& x) {
    fdwrite(f, reinterpret_cast<const char*>(&x), sizeof(T));
  }
template <typename TIter>
  inline void writes(imagefile* f, TIter begin, TIter end) {
    fdwrite(f, reinterpret_cast<const char*>(&(*begin)), (end - begin) * sizeof(*begin));
  }
inline void write(imagefile* f, const std::string& x) {
  write(f, x.size());
  writes(f, x.data(), x.data() + x.size());
}
inline void write(imagefile* f, const std::vector<unsigned char>& xs) {
  write(f, xs.size());
  writes(f, xs.begin(), xs.end());
}

inline void fdread(const imagefile* f, char* x, size_t len) {
  size_t i = 0;
  while (i < len) {
    ssize_t di = read(f->fd, x + i, len - i);

    if (di < 0) {
      if (errno != EINTR) {
        raiseSysError("Failed to read " + str::from(len) + " bytes from file", f->path);
      }
    } else if (di == 0) {
      raiseSysError("Empty read error", f->path);
    } else {
      i += di;
    }
  }
}
template <typename T>
  inline void read(const imagefile* f, T* x) {
    fdread(f, reinterpret_cast<char*>(x), sizeof(T));
  }
template <typename T>
  inline void reads(const imagefile* f, size_t sz, T* x) {
    fdread(f, reinterpret_cast<char*>(x), sz * sizeof(T));
  }
inline void read(imagefile* f, std::string* x) {
  size_t sz = 0;
  read(f, &sz);

  x->resize(sz);
  if (sz > 0) {
    reads(f, sz, &((*x)[0]));
  }
}
inline void read(imagefile* f, std::vector<unsigned char>* xs) {
  size_t sz = 0;
  read(f, &sz);

  xs->resize(sz);
  if (sz > 0) {
    reads(f, sz, &((*xs)[0]));
  }
}

// the absolute position of an indexed page
inline size_t pageOffset(const imagefile* f, file_pageindex_t page) { return f->page_size * page; }

// the page index implied by an absolute position
inline file_pageindex_t pageIndex(const imagefile* f, uint64_t fpos) { return fpos / f->page_size; }

// an absolute file position from a page and relative position
inline size_t position(const imagefile* f, file_pageindex_t page, uint16_t offset) { return pageOffset(f, page) + static_cast<uint64_t>(offset); }

// the file page for a given TOC page
inline file_pageindex_t tocPageToFilePage(const imagefile* f, uint64_t tpage) {
  assert(tpage < f->tocpages.size());
  return f->tocpages[tpage];
}

// the absolute position for some TOC data
inline uint64_t tocPosToFilePos(const imagefile* f, uint64_t spos) {
  uint64_t page   = spos / f->page_size;
  uint64_t offset = spos % f->page_size;

  return (tocPageToFilePage(f, page) * f->page_size) + offset;
}

// the absolute file position of a page's entry in the page table
inline size_t pageTOCPosition(const imagefile* f, file_pageindex_t page) {
  // this is how many page entries fit in the first TOC page (which includes the file header)
  const uint64_t firstTPCount = (f->page_size - sizeof(filehead) - sizeof(file_pageindex_t)) / sizeof(pagedata);

  // is the given page in the first TOC page?
  if (page < firstTPCount) {
    // if so, its TOC entry offset is just straight past the header
    return sizeof(filehead) + (page * sizeof(pagedata));
  } else {
    // ok, let's forget about the TOC entries from the first page
    page -= firstTPCount;

    // this is how many page entries fit in every subsequent TOC page
    const uint64_t restTPCount = (f->page_size - sizeof(file_pageindex_t)) / sizeof(pagedata);

    // from this we can find the TOC page that we belong on and the offset within that page
    const uint64_t tpage  = (page / restTPCount) + 1; // +1 because we've covered the first page already
    const uint64_t offset = (page % restTPCount) * sizeof(pagedata);

    // and that's all we need for the file position of this TOC entry
    return (tocPageToFilePage(f, tpage) * f->page_size) + offset;
  }
}

// rearrange our free list to accomodate this page resize
inline void updatePageSizeIndex(imagefile* f, file_pageindex_t page) {
  const pagedata& pd = f->pages[page];
  pageseq& pord = f->freespace[pd.type()];

  for (size_t k = 0; k < pord.size(); ++k) {
    if (pord[k] == page) {
      for (size_t i = k; i < pord.size() - 1; ++i) {
        if (f->pages[pord[i]].size() < f->pages[pord[i+1]].size()) {
          std::swap(pord[i], pord[i+1]);
        } else {
          break;
        }
      }
      break;
    }
  }
}

// insert this page (assumed new) into our size-index
//  (if it's too small to bother with, don't bother remembering it)
inline void insertPageSizeIndex(imagefile* f, file_pageindex_t page) {
  static const size_t minPageSize   = 30;
  static const size_t maxPageMemory = 200;

  const pagedata& pd = f->pages[page];
  if (pd.size() < minPageSize) return;

  // figure out where to put this page
  pageseq& pord = f->freespace[pd.type()];
  size_t k = pord.size();

  for (size_t i = 0; i < pord.size(); ++i) {
    if (pd.size() > f->pages[i].size()) {
      k = i;
      break;
    }
  }

  pord.insert(pord.begin() + k, page);
  if (pord.size() > maxPageMemory) { pord.resize(maxPageMemory); }
}

// try to find a page with as much free space as requested
inline bool findPageWithSpace(imagefile* f, pagetype::code pt, size_t datalen, size_t alignment, file_pageindex_t* idx) {
  const pageseq& pord = f->freespace[pt];
  if (pord.size() == 0) {
    return false;
  }

  const auto& pd  = f->pages[pord[0]];
  size_t      pad = (f->page_size - pd.size()) % alignment;

  if ((datalen + pad) > pd.size()) {
    return false;
  } else {
    *idx = pord[0];
    return true;
  }
}

// update the TOC entry for a given page
inline void updateTOCData(imagefile* f, file_pageindex_t page, const pagedata& pd) {
  // pages never suddenly get free space
  pagedata& opd = f->pages[page];
  assert(pd.size() <= opd.size());

  // update the page table data in memory and on disk
  opd = pd;
  seekAbs(f, pageTOCPosition(f, page));
  write(f, pd);

  // re-evaluate where this page belongs in the ordering of pages with free space
  updatePageSizeIndex(f, page);
}

// append a sequence of TOC entries (representing allocated pages)
inline void appendTOCData(imagefile* f, const pagetable& newpages) {
  // add these new pages to the TOC
  for (const auto& newpage : newpages) {
    f->pages.push_back(newpage);

    // maintain an index of pages by size
    insertPageSizeIndex(f, f->pages.size() - 1);

    // keep track of where system pages are
    if (newpage.type() == pagetype::toc) {
      f->tocpages.push_back(f->pages.size() - 1);
    }
  }

  // we may allocate new TOC entry pages as we go about writing these entries
  pagetable newtocpages;

  // now let's write the new pages to the TOC, allocating new pages for the TOC as necessary
  size_t idx = 0;
  while (idx < newpages.size()) {
    // the write position within this page
    uint16_t relpos = f->head_toc_pos % f->page_size;

    // the number of bytes available to write in this page
    //  (account for the last bit of the page used to link to the next system page)
    size_t bytes_avail = (f->page_size - relpos) - sizeof(file_pageindex_t);

    // the number of pagedata slots available to write in this page
    size_t slots_avail = bytes_avail / sizeof(pagedata);

    // the number of pagedata slots that we have left to write
    size_t slots_left = newpages.size() - idx;

    // the number of pagedata slots that we will fill on this cycle
    size_t slots_write = std::min(slots_avail, slots_left);

    // move to this page offset, and write all of the pagedata entries that we can
    seekAbs(f, f->head_toc_pos);
    writes(f, newpages.begin() + idx, newpages.begin() + idx + slots_write);

    // advance our read heads as far as we've just written
    idx += slots_write;
    f->head_toc_pos += slots_write * sizeof(pagedata);

    // have we written up to the link section of the page?
    // if so, we need to make a new system page and link to it
    file_pageindex_t nextpage = -1;

    if (restInPage(f, f->head_toc_pos) == sizeof(file_pageindex_t)) {
      // make a new TOC page and move the TOC write head to it
      f->head_toc_pos = f->file_size;
      newtocpages.push_back(pagedata(pagetype::toc, 0));
      allocPage(f);

      // link this finished TOC page to the new TOC page
      nextpage = f->pages.size() + newtocpages.size() - 1;
      write(f, nextpage);
    }
  }

  // if we added TOC pages while writing the input TOC entries, then TOC entries for those TOC pages need to be added too
  if (newtocpages.size() > 0) {
    appendTOCData(f, newtocpages);
  }
}

// find a location within pages of a given type where we can put a value with this length/alignment
// (if no such space can be found, allocate new page(s) as necessary)
inline size_t findSpace(imagefile* f, pagetype::code pt, size_t datalen, size_t alignment) {
  assert(datalen > 0 && alignment > 0);

  // can we find an existing page with the space that we need?
  file_pageindex_t fpage = -1;
  if (findPageWithSpace(f, pt, datalen, alignment, &fpage)) {
    pagedata& pd = f->pages[fpage];

    uint64_t offset = f->page_size - pd.size();
    uint64_t pad    = offset % alignment;

    updateTOCData(f, fpage, pagedata(pt, pd.size() - (datalen+pad)));
    return position(f, fpage, offset+pad);
  }

  // if we got here, then we need to allocate one or more contiguous pages for this data
  // all allocated pages will have 0 free size, except the last one which will leave the remainder
  size_t    pages = pageCount(f, datalen);
  size_t    result = f->file_size;
  pagetable tocdata;

  // every intermediate page in this allocation will be fully exhausted
  for (size_t p = 1; p < pages; ++p) {
    tocdata.push_back(pagedata(pt, 0));
  }

  // the last page of the allocation is only fully exhausted if an exact multiple of the page size has been allocated
  // otherwise claim only the remainder as allocated
  uint64_t lpallocd = datalen % f->page_size;

  if (lpallocd == 0) {
    tocdata.push_back(pagedata(pt, 0));
  } else {
    tocdata.push_back(pagedata(pt, f->page_size - lpallocd));
  }
  allocPages(f, pages);

  // keep track of these pages that we've added
  appendTOCData(f, tocdata);

  return result;
}

// add a variable binding (name, type, size, and file location)
inline void addBinding(imagefile* f, const std::string& vname, const bytes& type, size_t offset) {
  // determine how much space we'll need to store it
  size_t bsz = sizeof(size_t)                // the stored data offset
             + sizeof(size_t) + vname.size() // the variable name
             + sizeof(size_t) + type.size()  // the variable's type
             ;

  // find the best place to put this data
  size_t boffset = findSpace(f, pagetype::environment, bsz, 1);

  // now just write the data there
  seekAbs(f, boffset);
  write(f, offset);
  write(f, vname);
  write(f, type);

  // oh and we'll want to keep track of it too
  f->bindings[vname] = binding(type, offset, boffset);
}

// align a value to a boundary
template <typename T>
  inline T align(T x, T m) {
    if (m == 0 || (x % m) == 0) {
      return x;
    } else {
      return (1 + (x / m)) * m;
    }
  }

// mmap a region out of this file
inline fregion& createFileRegionMap(imagefile* f, file_pageindex_t page, size_t pages) {
  // leave no gaps in page mappings
  if (f->mappings.size() > 0) {
    fregion& mr = f->mappings.rbegin()->second;

    size_t pend = mr.base_page + mr.pages;
    if (pend < page) {
      pages += (page - pend);
      page   = pend;
    }
  }

  // adjust our map page count to match the set increment
  pages = align<size_t>(pages, f->mmapPageMultiple);

  // map the specified file region into memory
  char* d = reinterpret_cast<char*>(mmap(0, pages * f->page_size, PROT_READ | (f->readonly ? 0 : PROT_WRITE), MAP_SHARED, f->fd, page * f->page_size));
  if (d == MAP_FAILED) {
    raiseSysError
    (
      "Failed to map " + str::from(pages) +
      " pages from page " + str::from(page) +
      " out of " + str::from(f->file_size) +
      " bytes with a page size of " + str::from(f->page_size) +
      " bytes",
      f->path
    );
  }

  fregion& r = f->mappings[page];
  r.base_page = page;
  r.pages     = pages;
  r.base      = d;
  r.used      = 0;
  return r;
}

// munmap a region out of this file
inline void releaseFileRegionMap(imagefile* f, const fregion& fr) {
  if (munmap(fr.base, fr.pages * f->page_size) != 0) {
    raiseSysError("Failed to unmap page " + str::from(fr.base_page) + " from file", f->path);
  }
}

// the greatest map position <= a point
template <typename K, typename V>
  inline typename std::map<K, V>::iterator gleb(std::map<K, V>& m, const K& x) {
    auto r = m.lower_bound(x);

    if (r == m.end()) {
      if (m.size() > 0) {
        --r;
      }
    } else if (r->first != x) {
      --r;
    }
    return r;
  }

// find the mapping data for a region, or create it if necessary
inline fregion& mappedFileRegion(imagefile* f, file_pageindex_t page, size_t pages) {
  // try to find the nearest possible mapping for this page
  fmappings::iterator fm = gleb(f->mappings, page);

  // if we couldn't find a mapping, then we have to make one
  if (fm == f->mappings.end()) {
    return createFileRegionMap(f, page, pages);
  }
  fregion& r = fm->second;

  // if we found a mapping, and the requested region is in it, ship it!
  if (page >= r.base_page && (page + pages) <= (r.base_page + r.pages)) {
    return r;
  }

  // otherwise, we just need to make a new one
  return createFileRegionMap(f, page, pages);
}

// allocate a region of this file as mapped memory
inline char* mapFileData(imagefile* f, size_t fpos, size_t sz) {
  file_pageindex_t page   = fpos / f->page_size;
  uint16_t         offset = fpos % f->page_size;

  // get the mapped region where this data lives
  // and increment its use count
  fregion& r = mappedFileRegion(f, page, pageCount(f, sz));
  r.used += sz;

  // the result will be offset from the base of the mapped page
  // (plus any intervening pages from the base of the mapping to the page for this data)
  char* result = r.base + (f->page_size * (page - r.base_page)) + offset;

  // remember where this allocated data came from (in case we want to release it later)
  falloc& fa = f->allocs[result];
  fa.page = r.base_page;
  fa.size = sz;

  return result;
}

// deallocate memory mapped out of this file
// (if this means that there are no outstanding references to the mapping, then the mapping itself is released)
inline void unmapFileData(imagefile* f, const void* p, size_t sz) {
  fallocs::iterator fa = f->allocs.find(const_cast<char*>(reinterpret_cast<const char*>(p)));
  if (fa == f->allocs.end()) {
    return;
  }

  // remember what page mapping this allocation was out of, but forget the mapping
  file_pageindex_t dpage = fa->second.page;
  f->allocs.erase(fa);

  // dereference these bytes from the page mapping
  // if the page mapping has no references, we can remove the page mapping too
  fmappings::iterator fm = f->mappings.find(dpage);
  if (fm == f->mappings.end()) {
    throw std::runtime_error("Internal error, inconsistent file mapping state");
  }

  if (fm->second.used > sz) {
    fm->second.used -= sz;
  } else {
    releaseFileRegionMap(f, fm->second);
    f->mappings.erase(fm);
  }
}

// we shouldn't ever work with files that have invalid page sizes
inline uint16_t assertValidPageSize(const imagefile* f, size_t psize) {
  if (psize < HFREGION_MIN_PAGE_SIZE) {
    throw std::runtime_error(f->path + ": System page size too small for db support (" + str::from(psize) + ")");
  } else if (psize > HFREGION_MAX_PAGE_SIZE) {
    throw std::runtime_error(f->path + ": System page size too large for db support (" + str::from(psize) + ")");
  } else if ((psize % sizeof(pagedata)) != 0) {
    throw std::runtime_error(f->path + ": System page size must be a multiple of " + str::from(sizeof(pagedata)) + " (" + str::from(psize) + ")");
  } else if ((sizeof(filehead) % sizeof(pagedata)) != 0) {
    // should be a static assert :T
    throw std::runtime_error("No page size is valid, file format internally inconsistent");
  }
  return static_cast<uint16_t>(psize);
}

// put a new file into a valid empty state
inline void createFile(imagefile* f) {
  if (f->readonly) {
    throw std::runtime_error("Can't initialize empty file for read: " + f->path);
  }
  f->page_size = assertValidPageSize(f, sysconf(_SC_PAGESIZE));
  f->version   = HFREGION_CURRENT_FILE_FORMAT_VERSION;

  // start the first page!
  allocPage(f);
  seekAbs(f, 0);

  // the first page of the file begins with a standard header
  filehead fh;
  memset(&fh, 0, sizeof(fh));
  fh.magic    = HFREGION_FILE_PREFIX_BYTES;
  fh.pagesize = f->page_size;
  fh.version  = f->version;
  write(f, fh);

  // now begin the page table, starting with a description of this page
  f->pages.push_back(pagedata(pagetype::toc, 0));
  write(f, f->pages.back());
  f->tocpages.push_back(0);

  // and point the TOC head here
  f->head_toc_pos = sizeof(filehead) + sizeof(pagedata);
}

// read all of the page data entries possible from the current file position
inline void readPageData(imagefile* f) {
  // we stop reading page data when we hit the 0 page
  // and if we get to the end of a page, there's a link to the next page
  while (true) {
    pagedata pd;
    read(f, &pd);
    if (pd.type() == pagetype::null) {
      break;
    }
    f->pages.push_back(pd);
    f->head_toc_pos += sizeof(pagedata);

    if (restInPage(f, f->head_toc_pos) == sizeof(file_pageindex_t)) {
      file_pageindex_t nextpage = -1;
      read(f, &nextpage);
      f->head_toc_pos = pageOffset(f, nextpage);
      f->tocpages.push_back(nextpage);
      seekAbs(f, f->head_toc_pos);
    }
  }
}

// read a single environment variable definition
inline void readEnvironmentRecord(imagefile* f) {
  size_t      offset;
  std::string vname;
  bytes       type;
  size_t      boffset = filePosition(f);

  read(f, &offset);
  read(f, &vname);
  read(f, &type);

  f->bindings[vname] = binding(type, offset, boffset);
}

// read environment data starting at some page
// (return the number of pages read to avoid erroneously double-reading environment data that might span multiple pages)
inline size_t readEnvironmentPage(imagefile* f, file_pageindex_t p) {
  // remember the offset where we began reading environment data
  size_t initOffset = pageOffset(f, p);

  // go there
  seekAbs(f, initOffset);

  // we're done when we've read as much data as is reported for whatever page we're in
  while (true) {
    // we expect to read at least one environment binding
    readEnvironmentRecord(f);
 
    // exit when we've read to the end of whatever page we're in
    //  NOTE: we have to adjust -/+ 1 byte to account for the case when an environment
    //        page is filled up to and including the last byte (otherwise we'd mistakenly
    //        assume that we have to continue reading from the next page)
    size_t           pos   = filePosition(f) - 1;
    file_pageindex_t tpage = file_pageindex_t(pos / f->page_size);
    uint16_t         rpos  = (pos % f->page_size) + 1;

    if (rpos == (f->page_size - f->pages[tpage].size())) {
      break;
    }
  }

  // now just report on the number of pages we've read
  return pageCount(f, filePosition(f) - initOffset);
}

// read the file
inline void readFile(imagefile* f, uint16_t minVersion, uint16_t maxVersion) {
  // start reading the first page
  seekAbs(f, 0);

  // get the header, make sure it's sound
  filehead fh;
  read(f, &fh);

  if (fh.magic != HFREGION_FILE_PREFIX_BYTES) {
    throw std::runtime_error("Not a valid structured data file: " + f->path);
  } else if (fh.version < minVersion || fh.version > maxVersion) {
    if (minVersion == maxVersion) {
      throw std::runtime_error("Cannot read file format version=" + str::from(fh.version) + " (expected version=" + str::from(maxVersion) + ")");
    } else {
      throw std::runtime_error("Cannot read file format version=" + str::from(fh.version) + " (expected version in [" + str::from(minVersion) + "," + str::from(maxVersion) + "])");
    }
  }
  f->page_size = assertValidPageSize(f, fh.pagesize);
  f->version   = fh.version;

  // load the first page descriptor
  pagedata pd;
  read(f, &pd);
  f->pages.push_back(pd);

  f->tocpages.push_back(0);
  f->head_toc_pos = sizeof(filehead) + sizeof(pagedata);

  // now read all page descriptors
  readPageData(f);

  // and then read in the environment
  for (file_pageindex_t p = 0; p < f->pages.size(); ++p) {
    if (f->pages[p].type() == pagetype::environment) {
      // read environment data starting from this page
      // (skip any contiguous pages we might have read as part of this)
      p += readEnvironmentPage(f, p) - 1;
    }
  }
}

// open a file, or create it if necessary
inline imagefile* openFile(const std::string& fname, bool readonly, uint16_t minVersion = HFREGION_CURRENT_FILE_FORMAT_VERSION, uint16_t maxVersion = HFREGION_CURRENT_FILE_FORMAT_VERSION) {
  imagefile* f        = new imagefile();
  f->path             = fname;
  f->readonly         = readonly;
  f->mmapPageMultiple = 262144;    // map in 1GB increments

  try {
    // open the file
    f->fd = open(fname.c_str(), readonly ? O_RDONLY : (O_RDWR | O_CREAT), S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);

    if (f->fd < 0) {
      raiseSysError("Unable to open for " + std::string(readonly ? "read" : "write"), fname);
    }

    // load the initial file size
    if (!loadFileSize(f)) {
      raiseSysError("Can't stat file", f->path);
    }

    // if we've just created this file, initialize it, else read it
    if (f->file_size == 0) {
      createFile(f);
    } else {
      readFile(f, minVersion, maxVersion);
    }

    // keep a dummy value for writing all 0-length arrays
    if (!readonly) {
      auto za = f->bindings.find(".za");
      if (za != f->bindings.end()) {
        f->empty_array = za->second.offset;
      } else {
        f->empty_array = findSpace(f, pagetype::data, sizeof(size_t), sizeof(size_t));
        addBinding(f, ".za", bytes(), f->empty_array);
      }
    }

    // there, we've loaded this file
    return f;
  } catch (...) {
    closeFile(f);
    throw;
  }
}

// does this file even represent a structured data file?
// we'll say yes if it can be opened, it has the magic number, and its size is a multiple of its page size
inline bool isFRegion(const std::string& fname) {
  imagefile* f = new imagefile();
  f->path      = fname;
  f->readonly  = true;

  try {
    // open it
    f->fd = ::open(fname.c_str(), O_RDONLY, 0);
    if (f->fd < 0) { closeFile(f); return false; }

    // get its size
    if (!loadFileSize(f)) { closeFile(f); return false; }

    // get the header
    seekAbs(f, 0);
    filehead fh;
    read(f, &fh);

    if (fh.magic != HFREGION_FILE_PREFIX_BYTES) { closeFile(f); return false; }
    f->page_size = assertValidPageSize(f, fh.pagesize);

    bool result = (f->page_size > 0) && (f->file_size % f->page_size) == 0;
    closeFile(f);
    return result;
  } catch (...) {
    closeFile(f);
    return false;
  }
}

// create all implied directories in a path
inline void ensureDirExists(const std::string& path) {
  str::seq ps = str::csplit(path, "/");
  std::ostringstream pfx;

  for (const auto& p : ps) {
    pfx << p << "/";
    if (mkdir(pfx.str().c_str(), S_IRWXU | S_IRWXG | S_IRWXO) == -1 && errno != EEXIST && errno != EISDIR) {
      throw std::runtime_error("Failed to make directory '" + pfx.str() + "' with error: " + strerror(errno));
    }
  }
}

// give the first unique filename with some prefix/suffix that satisfies some condition
inline std::string withUniqueFilenameBy(const std::string& fprefix, const std::string& fsuffix, const std::function<bool(const std::string&)>& fileOp) {
  // the directory that this file is in can be created if necessary
  ensureDirExists(str::rsplit(fprefix, "/").first);

  // keep trying for new filenames until we get one that's distinct
  size_t inst = 0;
  while (true) {
    std::ostringstream ss;
    ss << fprefix << "-" << inst << fsuffix;
    if (fileOp(ss.str())) {
      return ss.str();
    }
    ++inst;
  }
}

// generate a new file with a given prefix & suffix
inline std::string uniqueFilename(const std::string& fprefix, const std::string& fsuffix) {
  return withUniqueFilenameBy(fprefix, fsuffix, [](const std::string& newpath) {
    int fd = open(newpath.c_str(), O_CREAT | O_EXCL, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
    if (fd >= 0) {
      close(fd);
      return true;
    } else if (errno == EEXIST) {
      return false;
    } else {
      throw std::runtime_error("Failed to generate a log database file with error: " + std::string(strerror(errno)));
    }
  });
}

// move an existing file to a new file with a given prefix & suffix
inline std::string moveToUniqueFilename(const std::string& oldpath, const std::string& fprefix, const std::string& fsuffix) {
  return withUniqueFilenameBy(fprefix, fsuffix, [&oldpath](const std::string& newpath) {
    int rt = link(oldpath.c_str(), newpath.c_str());
    if (rt == 0) {
      unlink(oldpath.c_str());
      return true;
    } else if (errno == EEXIST) {
      return false;
    } else {
      throw std::runtime_error("Failed to move to a log database file with error: " + std::string(strerror(errno)));
    }
  });
}

/***********************
 *
 * type description utilities for storage types
 *
 ***********************/
inline ty::desc storedListType(const ty::desc& t, size_t tysize) {
  return ty::recursive("x", ty::sum(ty::prim("unit"), ty::tup(0, t, align<size_t>(tysize, sizeof(size_t)), ty::fileRef(ty::var("x")))));
}

inline ty::desc batchSeqType(const ty::desc& t, size_t batchSize) {
  return ty::app(prim("carray", ty::fn("t", "c", ty::rec("avail", 0, ty::prim("long"), "buffer", sizeof(size_t), ty::array(ty::var("t"), ty::var("c"))))), t, ty::nat(batchSize));
}

inline ty::desc storedSeqType(const ty::desc& t, size_t batchSize) {
  return ty::fileRef(storedListType(ty::fileRef(batchSeqType(t, batchSize)), sizeof(size_t)));
}

inline ty::desc maybeDerefFileRef(const ty::desc& t) {
  if (t && t->tid == PRIV_HPPF_TYCTOR_TAPP) {
    const ty::App* ap = reinterpret_cast<const ty::App*>(t.get());
    if (ap->f->tid == PRIV_HPPF_TYCTOR_PRIM && reinterpret_cast<const ty::Prim*>(ap->f.get())->n == "fileref") {
      if (ap->args.size() >= 1) {
        return ap->args[0];
      }
    }
  }
  return ty::desc();
}

inline ty::desc maybeOpenRecursive(const ty::desc& t) {
  if (t && t->tid == PRIV_HPPF_TYCTOR_RECURSIVE) {
    return reinterpret_cast<const ty::Recursive*>(t.get())->t;
  }
  return ty::desc();
}

inline ty::desc maybeNthSum(const ty::desc& t, size_t n) {
  if (t && t->tid == PRIV_HPPF_TYCTOR_VARIANT) {
    const ty::Variant::Ctors& cs = reinterpret_cast<ty::Variant*>(t.get())->ctors;
    if (n < cs.size() && cs[0].at<0>() == ".f0") {
      return cs[n].at<2>();
    }
  }
  return ty::desc();
}

inline ty::desc maybeNthProd(const ty::desc& t, size_t n) {
  if (t && t->tid == PRIV_HPPF_TYCTOR_STRUCT) {
    const ty::Struct::Fields& fs = reinterpret_cast<ty::Struct*>(t.get())->fields;
    if (n < fs.size() && fs[0].at<0>() == ".f0") {
      return fs[n].at<2>();
    }
  }
  return ty::desc();
}

inline size_t inferBatchSize(const bytes& bs) {
  ty::desc dty = ty::decode(bs);

  if (ty::desc t = maybeDerefFileRef(maybeNthProd(maybeNthSum(maybeOpenRecursive(maybeDerefFileRef(dty)), 1), 0))) {
    if (t->tid == PRIV_HPPF_TYCTOR_TAPP) {
      const ty::App* buf = reinterpret_cast<const ty::App*>(t.get());
      if (buf->f->tid == PRIV_HPPF_TYCTOR_PRIM) {
        if (reinterpret_cast<ty::Prim*>(buf->f.get())->n == "carray") {
          if (buf->args.size() >= 2 && buf->args[1]->tid == PRIV_HPPF_TYCTOR_SIZE) {
            return reinterpret_cast<ty::Nat*>(buf->args[1].get())->x;
          }
        }
      }
    }
  }
  throw std::runtime_error("Invalid stored sequence type, can't infer size: " + ty::show(dty));
}

// the main interface for type translation into slog data
template <typename T, typename P = void>
  struct store {
  };

template <typename ... Ts>
  struct all_memcpyable { static const bool value = true; };
template <typename T, typename ... Ts>
  struct all_memcpyable<T, Ts...> { static const bool value = store<T>::can_memcpy && all_memcpyable<Ts...>::value; };

// codecs for built-in types
#define PRIV_HFREGION_DEFINE_PRIMTYS(T, n) \
  template <> \
    struct store<T> { \
      static ty::desc    storeType()                                  { return ty::prim(n); } \
      static const bool  can_memcpy = true; \
      static size_t      size()                                       { return sizeof(T); } \
      static size_t      alignment()                                  { return sizeof(T); } \
      static void        write(imagefile*, void* p,       const T& x) { *(reinterpret_cast<T*>(p)) = x; } \
      static void        read (imagefile*, const void* p, T* x)       { *x = *(reinterpret_cast<const T*>(p)); } \
    }

PRIV_HFREGION_DEFINE_PRIMTYS(bool,     "bool");
PRIV_HFREGION_DEFINE_PRIMTYS(uint8_t,  "byte");
PRIV_HFREGION_DEFINE_PRIMTYS(char,     "char");
PRIV_HFREGION_DEFINE_PRIMTYS(int16_t,  "short");
PRIV_HFREGION_DEFINE_PRIMTYS(uint16_t, "short");
PRIV_HFREGION_DEFINE_PRIMTYS(int32_t,  "int");
PRIV_HFREGION_DEFINE_PRIMTYS(uint32_t, "int");
PRIV_HFREGION_DEFINE_PRIMTYS(int64_t,  "long");
PRIV_HFREGION_DEFINE_PRIMTYS(uint64_t, "long");
#if defined(__APPLE__) && defined(__MACH__)
PRIV_HFREGION_DEFINE_PRIMTYS(size_t, "long");
#endif
PRIV_HFREGION_DEFINE_PRIMTYS(float,    "float");
PRIV_HFREGION_DEFINE_PRIMTYS(double,   "double");

// store fixed-length arrays
template <typename T, size_t N>
  struct storeFixedArrayDef {
    static ty::desc storeType() { return ty::array(store<T>::storeType(), ty::nat(N)); }

    static size_t size()      { return store<T>::size() * N; }
    static size_t alignment() { return store<T>::alignment(); }
  };

template <typename T, size_t N>
  struct store<T[N], typename tbool<store<T>::can_memcpy>::type> : public storeFixedArrayDef<T,N> {
    static const bool can_memcpy = true;
    static void write(imagefile*, void* p,       const T (&x)[N]) { memcpy(p, x, N*sizeof(T)); }
    static void read (imagefile*, const void* p, T       (*x)[N]) { memcpy(x, p, N*sizeof(T)); }
  };

template <typename T, size_t N>
  struct store<T[N], typename tbool<!store<T>::can_memcpy>::type> : public storeFixedArrayDef<T,N> {
    static const bool can_memcpy = false;
    static void write(imagefile* f, void* p, const T (&x)[N]) {
      for (size_t i = 0; i < N; ++i) {
        store<T>::write(f, p, x[i]);
        p = reinterpret_cast<uint8_t*>(p) + store<T>::size();
      }
    }
    static void read (imagefile* f, const void* p, T (*x)[N]) {
      for (size_t i = 0; i < N; ++i) {
        store<T>::read(f, p, &(*x)[i]);
        p = reinterpret_cast<const uint8_t*>(p) + store<T>::size();
      }
    }
  };

// store strings
template <>
  struct store<std::string> {
    static const bool can_memcpy = false;
    static ty::desc storeType() { return ty::fileRef(ty::array(ty::prim("char"))); }
    static size_t size() { return sizeof(size_t); }
    static size_t alignment() { return sizeof(size_t); }

    static void write(imagefile* f, void* p, const std::string& x) {
      if (x.size() > 0) {
        auto bc = sizeof(size_t) + x.size();

        size_t dloc = findSpace(f, pagetype::data, bc, sizeof(size_t));
        uint8_t* d = reinterpret_cast<uint8_t*>(mapFileData(f, dloc, bc));
        *reinterpret_cast<size_t*>(d) = x.size();
        memcpy(d+sizeof(size_t), x.data(), x.size());
        unmapFileData(f, d, bc);

        *reinterpret_cast<size_t*>(p) = dloc;
      } else {
        *reinterpret_cast<size_t*>(p) = f->empty_array;
      }
    }
    static void read (imagefile* f, const void* p, std::string* x) {
      auto     dloc = *reinterpret_cast<const size_t*>(p);
      size_t*  c    = reinterpret_cast<size_t*>(mapFileData(f, dloc, sizeof(size_t)));
      char*    s    = reinterpret_cast<char*>(mapFileData(f, dloc+sizeof(size_t), *c));

      x->assign(s, s + *c);

      unmapFileData(f, s, *c);
      unmapFileData(f, c, sizeof(size_t));
    }
  };

// store vectors
template <typename T>
  struct storeVectorDef {
    static ty::desc storeType() { return ty::fileRef(ty::array(store<T>::storeType())); }
    static size_t size() { return sizeof(size_t); }
    static size_t alignment() { return sizeof(size_t); }
  };
template <typename T>
  struct store<std::vector<T>, typename tbool<store<T>::can_memcpy>::type> : public storeVectorDef<T> {
    static const bool can_memcpy = false;
    static void write(imagefile* f, void* p, const std::vector<T>& x) {
      if (x.size() > 0) {
        auto bc = sizeof(size_t) + sizeof(T)*x.size();

        size_t dloc = findSpace(f, pagetype::data, bc, sizeof(size_t));
        uint8_t* d = reinterpret_cast<uint8_t*>(mapFileData(f, dloc, bc));
        *reinterpret_cast<size_t*>(d) = x.size();
        memcpy(d+sizeof(size_t), &x[0], sizeof(T)*x.size());
        unmapFileData(f, d, bc);

        *reinterpret_cast<size_t*>(p) = dloc;
      } else {
        *reinterpret_cast<size_t*>(p) = f->empty_array;
      }
    }
    static void read(imagefile* f, const void* p, std::vector<T>* x) {
      auto     dloc = *reinterpret_cast<const size_t*>(p);
      size_t*  c    = reinterpret_cast<size_t*>(mapFileData(f, dloc, sizeof(size_t)));
      uint8_t* d    = reinterpret_cast<uint8_t*>(mapFileData(f, dloc+sizeof(size_t), *c * sizeof(T)));

      x->resize(*c);
      memcpy(&(*x)[0], d, *c * sizeof(T));

      unmapFileData(f, d, *c);
      unmapFileData(f, c, sizeof(size_t));
    }
  };
template <typename T>
  struct store<std::vector<T>, typename tbool<!store<T>::can_memcpy>::type> : public storeVectorDef<T> {
    static const bool can_memcpy = false;
    static void write(imagefile* f, void* p, const std::vector<T>& x) {
      size_t tsz = store<T>::size();
      auto   bc  = sizeof(size_t) + tsz*x.size();

      size_t dloc = findSpace(f, pagetype::data, bc, sizeof(size_t));
      uint8_t* d = reinterpret_cast<uint8_t*>(mapFileData(f, dloc, bc));
      *reinterpret_cast<size_t*>(d) = x.size();

      auto dc = d + sizeof(size_t);
      for (const auto& xv : x) {
        store<T>::write(f, dc, xv);
        dc += tsz;
      }
      unmapFileData(f, d, bc);

      *reinterpret_cast<size_t*>(p) = dloc;
    }
    static void read(imagefile* f, const void* p, std::vector<T>* x) {
      size_t   tsz  = store<T>::size();
      auto     dloc = *reinterpret_cast<const size_t*>(p);
      size_t*  c    = reinterpret_cast<size_t*>(mapFileData(f, dloc, sizeof(size_t)));
      uint8_t* d    = reinterpret_cast<uint8_t*>(mapFileData(f, dloc+sizeof(size_t), *c * tsz));
      uint8_t* dc   = d;

      x->resize(*c);
      for (size_t i = 0; i < *c; ++i) {
        store<T>::read(f, dc, &(*x)[i]);
        dc += tsz;
      }

      unmapFileData(f, d, *c);
      unmapFileData(f, c, sizeof(size_t));
    }
  };

// store pairs
template <typename U, typename V>
  struct storePairDef {
    static ty::desc storeType() {
      return ty::tup(0, store<U>::storeType(), align<size_t>(store<U>::size(), store<V>::alignment()), store<V>::storeType());
    }
    static size_t alignment() { return std::max<size_t>(store<U>::alignment(), store<V>::alignment()); }
    static size_t size() { auto usz = store<U>::size(); return align<size_t>(usz + (align<size_t>(usz, store<V>::alignment()) - usz) + store<V>::size(), alignment()); }
  };

template <typename U, typename V>
  struct store<std::pair<U,V>, typename tbool<all_memcpyable<U, V>::value>::type> : public storePairDef<U,V> {
    static const bool can_memcpy = true;
    static void write(imagefile*, void* p,       const std::pair<U,V>& x) { memcpy(p, &x, sizeof(x)); }
    static void read (imagefile*, const void* p, std::pair<U,V>* x)       { memcpy(x, p, sizeof(*x)); }
  };
template <typename U, typename V>
  struct store<std::pair<U,V>, typename tbool<!all_memcpyable<U, V>::value>::type> : public storePairDef<U,V> {
    static const bool can_memcpy = false;
    static void write(imagefile* f, void* p, const std::pair<U,V>& x) {
      store<U>::write(f, p, x.first);
      store<V>::write(f, reinterpret_cast<uint8_t*>(p)+align<size_t>(store<U>::size(), store<V>::alignment()), x.second);
    }
    static void read (imagefile* f, const void* p, std::pair<U,V>* x) {
      store<U>::read(f, p, &x->first);
      store<V>::read(f, reinterpret_cast<const uint8_t*>(p)+align<size_t>(store<U>::size(), store<V>::alignment()), &x->second);
    }
  };

// store tuples
template <size_t i, size_t n, typename ... Ts>
  struct storeTupleDef {
    typedef typename nth<i, Ts...>::type H;
    typedef tuple<Ts...>                 TT;
    typedef typename TT::offs            offs;
    typedef storeTupleDef<i+1, n, Ts...> Recurse;

    static void fieldDefs(ty::Struct::Fields* fs) {
      fs->push_back(ty::Struct::Field(".f" + str::from(i), static_cast<int>(offsetAt<i, offs>::value), store<H>::storeType()));
      Recurse::fieldDefs(fs);
    }
    static ty::desc storeType() { ty::Struct::Fields fs; fieldDefs(&fs); return ty::record(fs); }

    static size_t alignment() { return TT::alignment; }
    static size_t size()      { return TT::size;      }

    static void incrWrite(imagefile* f, void* p, const tuple<Ts...>& x) {
      store<H>::write(f, p, x.template at<i>());
      Recurse::incrWrite(f, p, x);
    }
    static void incrRead(imagefile* f, const void* p, tuple<Ts...>* x) {
      store<H>::read(f, p, &x->template at<i>());
      Recurse::incrRead(f, p, x);
    }
  };
template <size_t n, typename ... Ts>
  struct storeTupleDef<n, n, Ts...> {
    static void fieldDefs(ty::Struct::Fields*) { }
    static ty::desc storeType()   { return ty::prim("unit"); }
    static size_t alignment()     { return  0; }
    static size_t size()          { return  0; }

    static void incrWrite(imagefile*, void*, const tuple<Ts...>&) { }
    static void incrRead (imagefile*, const void*, tuple<Ts...>*) { }
  };

template <typename ... Ts>
  struct store<tuple<Ts...>, typename tbool<all_memcpyable<Ts...>::value>::type> : public storeTupleDef<0, sizeof...(Ts), Ts...> {
    static const bool can_memcpy = true;
    static void write(imagefile*, void* p,       const tuple<Ts...>& x) { memcpy(p, &x, sizeof( x)); }
    static void read (imagefile*, const void* p, tuple<Ts...>* x)       { memcpy(x,  p, sizeof(*x)); }
  };
template <typename ... Ts>
  struct store<tuple<Ts...>, typename tbool<!all_memcpyable<Ts...>::value>::type> : public storeTupleDef<0, sizeof...(Ts), Ts...> {
    static const bool can_memcpy = false;
    typedef storeTupleDef<0, sizeof...(Ts), Ts...> Reflect;

    static void write(imagefile* f, void* p, const tuple<Ts...>& x) {
      Reflect::incrWrite(f, p, x);
    }
    static void read (imagefile* f, const void* p, tuple<Ts...>* x) {
      Reflect::incrRead(f, p, x);
    }
  };

// store reflective structs
struct defStructF {
  ty::Struct::Fields* fs;
  size_t offset;
  defStructF(ty::Struct::Fields* fs) : fs(fs), offset(0) { }

  template <typename T>
    void visit(const char* fname) {
      size_t i = align<size_t>(this->offset, store<T>::alignment());
      this->fs->push_back(ty::Struct::Field(fname, i, store<T>::storeType()));
      this->offset = i + store<T>::size();
    }
};

struct calcSizeF {
  size_t* sz;
  size_t* maxAlign;
  calcSizeF(size_t* sz, size_t* maxAlign) : sz(sz), maxAlign(maxAlign) { *this->sz = 0; }

  template <typename T>
    void visit(const char* fname) {
      *this->sz       = align<size_t>(*this->sz, store<T>::alignment()) + store<T>::size();
      *this->maxAlign = std::max<size_t>(*this->maxAlign, store<T>::alignment());
    }
};

template <typename T>
  struct storeStructDef {
    static ty::desc storeType() {
      ty::Struct::Fields fs;
      defStructF df(&fs);
      T::meta(df);
      return ty::record(fs);
    }

    static size_t size() {
      size_t sz       = 0;
      size_t maxAlign = 1;
      calcSizeF csF(&sz, &maxAlign);
      T::meta(csF);
      return align<size_t>(sz, maxAlign);
    }
    static size_t alignment() {
      size_t sz       = 0;
      size_t maxAlign = 1;
      calcSizeF csF(&sz, &maxAlign);
      T::meta(csF);
      return maxAlign;
    }
  };

template <typename T>
  struct store<T, typename tbool<T::is_hmeta_struct && store<typename T::as_tuple_type>::can_memcpy>::type> : public storeStructDef<T> {
    static const bool can_memcpy = true;
    static void write(imagefile*, void* p,       const T& x) { memcpy(p, &x, sizeof(T)); }
    static void read (imagefile*, const void* p, T*       x) { memcpy(x,  p, sizeof(T)); }
  };

struct writeFieldF {
  imagefile*     f;
  uint8_t*       o;
  const uint8_t* i;
  size_t         ooffset, ioffset;
  writeFieldF(imagefile* f, uint8_t* o, const uint8_t* i) : f(f), o(o), i(i), ooffset(0), ioffset(0) { }

  template <typename T>
    void visit(const char* fname) {
      size_t ooff = align<size_t>(this->ooffset, store<T>::alignment());
      size_t ioff = align<size_t>(this->ioffset, alignof(T));

      store<T>::write(this->f, this->o + ooff, *reinterpret_cast<const T*>(this->i + ioff));

      this->ooffset = ooff + store<T>::size();
      this->ioffset = ioff + sizeof(T);
    }
};

struct readFieldF {
  imagefile*     f;
  const uint8_t* i;
  uint8_t*       o;
  size_t         ooffset, ioffset;
  readFieldF(imagefile* f, const uint8_t* i, uint8_t* o) : f(f), i(i), o(o), ooffset(0), ioffset(0) { }

  template <typename T>
    void visit(const char* fname) {
      size_t ioff = align<size_t>(this->ioffset, store<T>::alignment());
      size_t ooff = align<size_t>(this->ooffset, alignof(T));

      store<T>::read(this->f, this->i + ioff, reinterpret_cast<T*>(this->o + ooff));

      this->ioffset = ioff + store<T>::size();
      this->ooffset = ooff + sizeof(T);
    }
};

template <typename T>
  struct store<T, typename tbool<T::is_hmeta_struct && !store<typename T::as_tuple_type>::can_memcpy>::type> : public storeStructDef<T> {
    static const bool can_memcpy = false;
    static void write(imagefile* f, void* p, const T& x) {
      writeFieldF wfF(f, reinterpret_cast<uint8_t*>(p), reinterpret_cast<const uint8_t*>(&x));
      T::meta(wfF);
    }
    static void read (imagefile* f, const void* p, T* x) {
      readFieldF rfF(f, reinterpret_cast<const uint8_t*>(p), reinterpret_cast<uint8_t*>(x));
      T::meta(rfF);
    }
  };

// store reflective enumerations
template <typename T>
  struct store<T, typename tbool<T::is_hmeta_enum>::type> {
    static const bool can_memcpy = true;
    
    static ty::desc storeType() {
      return ty::enumdef(T::meta());
    }

    static size_t size() { return sizeof(uint32_t); }
    static size_t alignment() { return sizeof(uint32_t); }

    static void write(imagefile*, void* p,       const T& x) { memcpy(p, &x, sizeof(uint32_t)); }
    static void read (imagefile*, const void* p, T*       x) { memcpy(x,  p, sizeof(uint32_t)); }
  };

// store variants
template <size_t i, size_t n, typename ... Ts>
  struct storeVariantDef {
    typedef typename nth<i, Ts...>::type   H;
    typedef variant<Ts...>                 VT;
    typedef storeVariantDef<i+1, n, Ts...> Recurse;

    static void ctorDefs(ty::Variant::Ctors* cs) {
      cs->push_back(ty::Variant::Ctor(".f" + str::from(i), static_cast<int>(i), store<H>::storeType()));
      Recurse::ctorDefs(cs);
    }
    static ty::desc storeType() { ty::Variant::Ctors cs; ctorDefs(&cs); return ty::variant(cs); }

    static size_t maxSize()      { return std::max<size_t>(store<H>::size(),      Recurse::maxSize());      }
    static size_t maxAlignment() { return std::max<size_t>(store<H>::alignment(), Recurse::maxAlignment()); }
    static size_t tagOffset()    { return align<size_t>(sizeof(uint32_t), maxAlignment()); }

    static size_t size()      { return align<size_t>(align<size_t>(sizeof(uint32_t), maxAlignment()) + maxSize(), maxAlignment()); }
    static size_t alignment() { return std::max<size_t>(sizeof(uint32_t), maxAlignment()); }
  };
template <size_t n, typename ... Ts>
  struct storeVariantDef<n, n, Ts...> {
    static void ctorDefs(ty::Variant::Ctors*) { }
    static ty::desc storeType()   { return ty::prim("void"); }
    static size_t maxSize()       { return 0;  }
    static size_t maxAlignment()  { return 1;  }
  };

template <typename ... Ts>
  struct store<variant<Ts...>, typename tbool<all_memcpyable<Ts...>::value>::type> : public storeVariantDef<0, sizeof...(Ts), Ts...> {
    static const bool can_memcpy = true;
    static void write(imagefile*, void* p,       const variant<Ts...>& x) { memcpy(p, &x, sizeof( x)); }
    static void read (imagefile*, const void* p, variant<Ts...>* x)       { memcpy(x,  p, sizeof(*x)); }
  };

template <size_t tag, typename T, typename M>
  struct variantGenWrite {
    static void fn(T* vd, imagefile* f, void* p, size_t tagOffset) {
      *reinterpret_cast<uint32_t*>(p) = tag;
      store<T>::write(f, reinterpret_cast<uint8_t*>(p) + tagOffset, *vd);
    }
  };
template <size_t tag, typename T, typename M>
  struct variantGenRead {
    static void fn(T* vd, imagefile* f, const void* p, size_t tagOffset) {
      new (vd) T();
      store<T>::read(f, reinterpret_cast<const uint8_t*>(p) + tagOffset, vd);
    }
  };
template <typename ... Ts>
  struct store<variant<Ts...>, typename tbool<!all_memcpyable<Ts...>::value>::type> : public storeVariantDef<0, sizeof...(Ts), Ts...> {
    static const bool can_memcpy = false;
    typedef storeVariantDef<0, sizeof...(Ts), Ts...> Reflect;

    static void write(imagefile* f, void* p, const variant<Ts...>& x) {
      x.template apply<void, variantGenWrite, void, imagefile*, void*, size_t>(f, p, Reflect::tagOffset());
    }
    static void read (imagefile* f, const void* p, variant<Ts...>* x) {
      x->unsafeTag() = *reinterpret_cast<const uint32_t*>(p);
      variantApp<void, variantGenRead, void, tuple<Ts...>, imagefile*, const void*, size_t>::apply(x->unsafeTag(), x->unsafePayload(), f, p, Reflect::tagOffset());
    }
  };

// store variants with named constructors
struct printVF {
  std::ostringstream* ss;
  printVF(std::ostringstream* ss) : ss(ss) { }
  template <typename T>
    void visit(const char* n, int, const T& x) {
      *this->ss << n << "=" << x;
    }
};
template <typename T>
  inline std::string showV(const T& x) {
    std::ostringstream ss;
    ss << "|";
    printVF pvf(&ss);
    x.gvisit(pvf);
    ss << "|";
    return ss.str();
  }

struct descVariantF {
  ty::Variant::Ctors* ctors;
  descVariantF(ty::Variant::Ctors* ctors) : ctors(ctors) { }

  template <typename T>
    void ctor(const char* n, int id) {
      this->ctors->push_back(ty::Variant::Ctor(n, id, store<T>::storeType()));
    }
};

struct calcVSizeF {
  size_t* maxSize;
  size_t* maxAlign;
  calcVSizeF(size_t* maxSize, size_t* maxAlign) : maxSize(maxSize), maxAlign(maxAlign) { *this->maxSize = 0; *this->maxAlign = sizeof(uint32_t); }

  template <typename T>
    void ctor(const char* n, int id) {
      *this->maxSize  = std::max<size_t>(*this->maxSize,  store<T>::size());
      *this->maxAlign = std::max<size_t>(*this->maxAlign, store<T>::alignment());
    }
};

template <typename T>
  struct store<T, typename tbool<T::is_hmeta_variant>::type> {
    typedef typename T::as_variant_type VT;
    static const bool can_memcpy = store<VT>::can_memcpy;

    static ty::desc storeType() {
      ty::Variant::Ctors cs;
      descVariantF f(&cs);
      T::meta(f);
      return ty::variant(cs);
    }

    static void write(imagefile* f, void* p,       const T& x) { store<VT>::write(f, p, *reinterpret_cast<const VT*>(&x)); }
    static void read (imagefile* f, const void* p, T*       x) { store<VT>::read (f, p, reinterpret_cast<VT*>(x)); }

    static size_t size() { return store<VT>::size(); }
    static size_t alignment() { return store<VT>::alignment(); }
  };

// store opaque type aliases
template <typename T>
  struct store<T, typename tbool<T::is_hmeta_alias>::type> {
    typedef typename T::type RT;

    static ty::desc storeType() { return ty::prim(T::name(), store<RT>::storeType()); ; }

    static const bool  can_memcpy = store<RT>::can_memcpy;
    static size_t      size()                                         { return store<RT>::size(); }
    static size_t      alignment()                                    { return store<RT>::alignment(); }
    static void        write(imagefile* f, void* p,       const T& x) { store<RT>::write(f, p, x.value); }
    static void        read (imagefile* f, const void* p, T* x)       { store<RT>::read(f, p, &x->value); }
  };

/***********************
 *
 * user API on top of type translation and low-level file access
 *
 ***********************/

struct seriesi {
  virtual ~seriesi() { }
  virtual const ty::desc& typeDef() const = 0;
};

// how much space does a stored array batch use?
template <typename T>
  inline size_t batchByteCount(size_t batchSize) {
    return sizeof(size_t) + align<size_t>(store<T>::size()*batchSize, sizeof(size_t));
  }

// a structured data file opened for output
template <typename T>
  class wseries : public seriesi {
  public:
    wseries(imagefile* f, const std::string& seqname, size_t batchSize, const ty::desc& tdef = store<T>::storeType()) : tdef(tdef), f(f), seqname(seqname), batchSize(batchSize), writeCB([](uint64_t){}) {
      // determine sequence types
      this->stdef = storedSeqType(this->tdef, this->batchSize);

      // allocate space for this sequence and prepare to write
      auto b = this->f->bindings.find(this->seqname);
      if (b == this->f->bindings.end()) {
        // this sequence is not yet defined, so define it and begin writing to it
        size_t dloc = findSpace(this->f, pagetype::data, sizeof(size_t), sizeof(size_t));
        this->headNodeRef = reinterpret_cast<uint64_t*>(mapFileData(this->f, dloc, sizeof(size_t)));
        consBatchNode(allocBatchNode());
        addBinding(this->f, this->seqname, ty::encoding(this->stdef), dloc);
      } else {
        // the sequence is already defined, make sure it has the right type def and then resume writing to it
        if (b->second.type != ty::encoding(this->stdef)) {
          throw std::runtime_error("File already defines series '" + this->seqname + "' with type inconsistent with " + ty::show(store<T>::storeType()));
        } else {
          this->headNodeRef = reinterpret_cast<uint64_t*>(mapFileData(this->f, b->second.offset, sizeof(size_t)));
          restartFromBatchNode();
        }
      }
    }
    ~wseries() {
    }

    const ty::desc&    typeDef()  const { return this->tdef; }
    const std::string& name()     const { return this->seqname; }
    imagefile*         file()     const { return this->f; }

    void operator()(const T& x) {
      store<T>::write(this->f, this->batchHead, x);
      this->writeCB(this->batchDataRef+(this->batchHead-reinterpret_cast<uint8_t*>(this->batchData)));
      this->batchHead += store<T>::size();
      if (++(*reinterpret_cast<size_t*>(this->batchData)) == this->batchSize) {
        void* oldBatchData = this->batchData;
        consBatchNode(this->batchNode);
        unmapFileData(this->f, oldBatchData, batchByteCount<T>(this->batchSize));
      }
    }
  public:
    void setWriteCB(const std::function<void(uint64_t)>& f) { this->writeCB = f; }
  private:
    ty::desc tdef;  // the type for a single sequence value
    ty::desc stdef; // the type for the whole sequence

    imagefile*                    f;          // the file we're writing into
    std::string                   seqname;    // the name of this sequence in the file
    size_t                        batchSize;  // the size of each batch of values within a node
    std::function<void(uint64_t)> writeCB;    // post-write logic (e.g. for sequencing writes across multiple series)

    uint64_t  batchDataRef;
    void*     batchData;
    uint8_t*  batchHead;
    uint64_t  batchNode;
    uint64_t* headNodeRef;

    void consBatchNode(uint64_t nextPtr) {
      auto bsz = batchByteCount<T>(this->batchSize);

      this->batchDataRef = findSpace(this->f, pagetype::data, bsz, sizeof(size_t));
      this->batchData    = mapFileData(this->f, this->batchDataRef, bsz);
      this->batchHead    = reinterpret_cast<uint8_t*>(this->batchData) + sizeof(size_t);
      this->batchNode    = allocBatchNode(this->batchDataRef, nextPtr);

      *this->headNodeRef = this->batchNode;
    }

    void restartFromBatchNode() {
      size_t* d = reinterpret_cast<size_t*>(mapFileData(this->f, *this->headNodeRef, 3*sizeof(size_t)));
      
      // if we somehow get a root node representing the empty list, we're free to start a fresh list
      if (d[0] == 0) {
        consBatchNode(allocBatchNode());
        return;
      }

      this->batchDataRef = d[1];
      this->batchData    = mapFileData(this->f, this->batchDataRef, batchByteCount<T>(this->batchSize));
      this->batchHead    = reinterpret_cast<uint8_t*>(this->batchData) + sizeof(size_t) + ((*reinterpret_cast<size_t*>(this->batchData))*store<T>::size());
      this->batchNode    = *this->headNodeRef;
    }

    uint64_t allocBatchNode() {
      return findSpace(this->f, pagetype::data, sizeof(size_t) + sizeof(size_t) + sizeof(size_t), sizeof(size_t));
    }

    uint64_t allocBatchNode(uint64_t batchOffset, uint64_t nextNodeOffset) {
      size_t  dsize = 3 * sizeof(size_t);
      size_t  dloc  = findSpace(this->f, pagetype::data, dsize, sizeof(size_t));
      size_t* d     = reinterpret_cast<size_t*>(mapFileData(this->f, dloc, dsize));

      d[0] = 1;
      d[1] = batchOffset;
      d[2] = nextNodeOffset;
      unmapFileData(this->f, d, dsize);

      return dloc;
    }
  };
template <typename ... Wss>
  struct seqVar {
    static void stepEnc(ty::Variant::Ctors*, size_t*, const Wss& ...) { }
    static ty::desc storeType(imagefile*, const Wss& ...) { return ty::prim("unit"); }
    static void setWriteCBs(uint32_t, const std::function<void(uint32_t,uint64_t)>&, Wss& ... wss) { }
  };
template <typename Ws, typename ... Wss>
  struct seqVar<Ws, Wss...> {
    static void stepEnc(ty::Variant::Ctors* cs, size_t* c, const Ws& s, const Wss& ... wss) {
      cs->push_back(ty::Variant::Ctor(s.name(), *c, ty::fileRef(s.typeDefinition())));
      ++*c;
      seqVar<Wss...>::stepEnc(cs, c, wss...);
    }
    static ty::desc storeType(imagefile* f, const Ws& s, const Wss& ... wss) {
      if (f != s.file()) {
        throw std::runtime_error("Error, can't sequence series stored in different files");
      }
      ty::Variant::Ctors cs;
      size_t c = 0;
      stepEnc(&cs, &c, s, wss...);
      return ty::variant(cs);
    }
    static void setWriteCBs(uint32_t id, const std::function<void(uint32_t,uint64_t)>& fn, Ws& s, Wss& ... wss) {
      s.setWriteCB([id,fn](uint64_t p) { fn(id, p); });
      seqVar<Wss...>::setWriteCBs(id+1, fn, wss...);
    }
  };
template <typename ... Wss>
  class wsseq : public seriesi {
  public:
    wsseq(imagefile* f, const std::string& n, Wss& ... wss) : log(f, n, 10000, seqVar<Wss...>::storeType(f, wss...)) {
      seqVar<Wss...>::setWriteCBs(0, [this](uint32_t id, uint64_t p){this->log(std::pair<uint32_t,uint64_t>(id,p));}, wss...);
    }
    ~wsseq() {
    }
    const ty::desc& typeDef()  const { return this->log.typeDef(); }
  private:
    wseries<std::pair<uint32_t,size_t>> log;
  };
class writer {
public:
  writer(const std::string& fname) : f(openFile(fname, false)) {
  }
  ~writer() {
    closeFile(this->f);
    for (const auto& s : this->ss) {
      delete s.second;
    }
  }

  template <typename T>
    wseries<T>& series(const std::string& n, size_t batchSize = 10000) {
      auto s = this->ss.find(n);
      if (s != this->ss.end()) {
        if (s->second->typeDef() == store<T>::storeType()) {
          return *reinterpret_cast<wseries<T>*>(s->second);
        } else {
          throw std::runtime_error("Inconsistent usage of '" + n + "' as type " + ty::show(store<T>::storeType()) + " (but declared as type " + ty::show(s->second->typeDef()) + ")");
        }
      } else {
        auto r = new wseries<T>(this->f, n, batchSize);
        this->ss[n] = r;
        return *r;
      }
    }

  template <typename ... Wss>
    void recordOrdering(const std::string& n, Wss& ... wss) {
      auto s = this->ss.find(n);
      if (s != this->ss.end()) {
        ty::desc tdesc = seqVar<Wss...>::storeType(this->f, wss...);

        if (s->second->typeDef() != tdesc) {
          throw std::runtime_error("Inconsistent usage of '" + n + "' as type " + ty::show(tdesc) + " (but declared as type " + ty::show(s->second->typeDef()) + ")");
        }
      } else {
        this->ss[n] = new wsseq<Wss...>(this->f, n, wss...);
      }
    }

  void signal() { 
    seekAbs(this->f, 0);
    write(this->f, static_cast<uint8_t>(0x0d));
  }
private:
  typedef std::map<std::string, seriesi*> wseriess;
  imagefile* f;
  wseriess   ss;
};

// a structured data file opened for input
template <typename T>
  class rseries : public seriesi {
  public:
    rseries(imagefile* f, const std::string& seqname, const ty::desc& tdef, const binding& b) : tdef(tdef), f(f), batchSize(inferBatchSize(b.type)) {
      // determine value and sequence types
      this->stdef = storedSeqType(this->tdef, this->batchSize);

      if (b.type != encoding(this->stdef)) {
        throw std::runtime_error("File defines series '" + seqname + "' with type inconsistent with " + ty::show(store<T>::storeType()));
      }

      // load all nodes, prepare to walk in-order
      auto* n = reinterpret_cast<uint64_t*>(mapFileData(this->f, b.offset, sizeof(size_t)));
      loadReadState(*n);
      unmapFileData(this->f, n, sizeof(size_t));
    }
    rseries(imagefile* f, const std::string& seqname) : rseries(f, seqname, store<T>::storeType(), loadBinding(f, seqname)) {
    }
    ~rseries() {
    }

    const ty::desc& typeDef() const { return this->tdef; }
    imagefile*      file()    const { return this->f; }

    bool next(T* x) {
      if (this->headLen) {
        while (this->headIndex >= *this->headLen) {
          if (!loadNextNode()) {
            return false;
          }
        }

        store<T>::read(this->f, this->head, x);
        this->head += store<T>::size();
        ++this->headIndex;
        return true;
      } else {
        return false;
      }
    }
  private:
    ty::desc tdef;  // the type for a single sequence value
    ty::desc stdef; // the type for the whole sequence

    imagefile* f;
    size_t     batchSize;

    std::stack<uint64_t> batches;
    size_t*              headLen;
    uint8_t*             head;
    size_t               headIndex;

    static const binding& loadBinding(imagefile* f, const std::string& seqname) {
      auto b = f->bindings.find(seqname);
      if (b == f->bindings.end()) {
        throw std::runtime_error("File does not define series '" + seqname + "'");
      }
      return b->second;
    }

    void loadReadState(uint64_t root) {
      while (root != 0) {
        uint64_t* d = reinterpret_cast<uint64_t*>(mapFileData(this->f, root, 3*sizeof(uint64_t)));
        if (d[0] == 0) {
          root = 0;
        } else {
          this->batches.push(d[1]);
          root = d[2];
        }
        unmapFileData(this->f, d, 3*sizeof(size_t));
      }

      this->headLen = 0;
      this->head    = 0;
      loadNextNode();
    }

    bool loadNextNode() {
      auto bsz = batchByteCount<T>(this->batchSize);

      this->headIndex = 0;
      if (this->headLen) {
        unmapFileData(this->f, this->headLen, bsz);
      }

      if (this->batches.size() == 0) {
        this->headLen = 0;
        this->head    = 0;
        return false;
      } else {
        this->headLen = reinterpret_cast<size_t*>(mapFileData(this->f, this->batches.top(), bsz));
        this->head    = reinterpret_cast<uint8_t*>(this->headLen) + sizeof(size_t);
        this->batches.pop();
        return true;
      }
    }
  };
class rordering {
public:
  rordering(imagefile* f, const std::string& seqname) : logDef(loadLogDef(f, seqname)), log(f, seqname, logDef.tdesc, *logDef.b) {
  }
  template <typename T>
    void match(const std::string& n, const std::function<void(const T&)>& cfn) {
      auto v = this->logDef.varDef.find(n);
      if (v == this->logDef.varDef.end()) {
        throw std::runtime_error("Constructor undefined in ordering: " + std::string(n));
      }
      if (ty::encoding(v->second.second) != ty::encoding(store<T>::storeType())) {
        throw std::runtime_error("Constructor '" + n + "' defined in ordering, but not with type " + ty::show(store<T>::storeType()));
      }

      // bind a function to process values out of this ordering
      this->logDef.varBindings[v->second.first] = [cfn](imagefile* f, uint64_t offset) {
        void* d = mapFileData(f, offset, store<T>::size());
        T     t;

        store<T>::read(f, d, &t);
        cfn(t);
        
        unmapFileData(f, d, store<T>::size());
      };
    }
  bool next() {
    std::pair<uint32_t, uint64_t> cp;
    while (this->log.next(&cp)) {
      auto bf = this->logDef.varBindings.find(cp.first);
      if (bf != this->logDef.varBindings.end()) {
        bf->second(this->log.file(), cp.second);
        return true;
      }
    }
    return false;
  }
private:
  typedef std::map<std::string, std::pair<uint32_t, ty::desc>> VarDef;
  typedef std::unordered_map<uint32_t, std::function<void(imagefile*,uint64_t)>> VarCtorBindings;
  struct LogDef {
    // the encoded ordering variant type and seq binding
    ty::desc       tdesc;
    const binding* b;
    
    // the parsed ordering variant definition
    VarDef varDef;

    // bindings to specific cases of the ordering variant
    VarCtorBindings varBindings;
  };
  LogDef logDef;

  static LogDef loadLogDef(imagefile* f, const std::string& seqname) {
    auto b = f->bindings.find(seqname);
    if (b == f->bindings.end()) {
      throw std::runtime_error("File does not define series '" + seqname + "'");
    }
    
    LogDef r;
    r.tdesc = ty::decode(b->second.type);
    r.b     = &b->second;

    if (r.tdesc->tid == PRIV_HPPF_TYCTOR_VARIANT) {
      for (const auto& ctor : reinterpret_cast<ty::Variant*>(r.tdesc.get())->ctors) {
        r.varDef[ctor.at<0>()] = std::pair<uint32_t, ty::desc>(ctor.at<1>(), ctor.at<2>());
      }
    }
    return r;
  }

  rseries<std::pair<uint32_t, uint64_t>> log;
};
class reader {
public:
  reader(const std::string& fname) : f(openFile(fname, true)) {
  }
  ~reader() {
    closeFile(this->f);
    for (const auto& s : this->ss) {
      delete s.second;
    }
  }

  template <typename T>
    rseries<T>& series(const std::string& name) {
      auto s = this->ss.find(name);
      if (s != this->ss.end()) {
        ty::desc tdesc = store<T>::storeType();

        if (s->second->typeDef() == tdesc) {
          return *reinterpret_cast<rseries<T>*>(s->second);
        } else {
          throw std::runtime_error("Inconsistent usage of '" + name + "' as type " + ty::show(tdesc) + " (but declared as type " + ty::show(s->second->typeDef()) + ")");
        }
      } else {
        auto r = new rseries<T>(this->f, name);
        this->ss[name] = r;
        return *r;
      }
    }

    rordering ordering(const std::string& name) {
      return rordering(this->f, name);
    }
private:
  typedef std::map<std::string, seriesi*> rseriess;
  imagefile* f;
  rseriess   ss;
};

}}

#endif

