
#include <hobbes/hobbes.H>
#include <hobbes/db/file.H>
#include <hobbes/db/signals.H>
#include <hobbes/eval/cc.H>
#include <hobbes/util/str.H>
#include <hobbes/util/ptr.H>
#include <stdexcept>
#include <sstream>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

namespace hobbes {

// the file header
struct filehead {
  uint32_t magic;    // trivial sanity check for file-type
  uint16_t pagesize; // how large does this file assume that one memory page is?
  uint16_t version;  // a file format version number (incremented when incompatibilities are introduced)
};
#define CURRENT_FILE_FORMAT_VERSION ((uint16_t)1)

// a single system page record
//   this will be stored as two bytes, the upper 2 bits used for storing a page type and the lower 14 used for free size in that page
struct pagetype {
  enum code {
    null        = 0, // a "null" page (used to mark the end of system page data)
    toc         = 1, // a "toc" page (a page of data about other pages)
    environment = 2, // an "environment" page (describing variable bindings)
    data        = 3  // a "data" page (raw data for bound variables)
  };

  static uint8_t encode(code    x) { return (uint8_t)x; }
  static code    decode(uint8_t x) { return (code)   x; }
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

static const size_t   minPageSize     = 256;
static const size_t   maxPageSize     = ((1 << 14) - 1); // we take the upper 2 bits to store page type
static const uint32_t filePrefixBytes = 0x10a1db0d;
static const size_t   pageDataOffset  = sizeof(filehead); // page data begins just past the header

// the index of a page of data in the underlying file
typedef uint64_t file_pageindex_t;

// convenience for raising errors out of errno
void dbglog(const std::string&);
static void raiseSysError(const std::string& msg, const std::string& fname) {
  std::ostringstream ss;
  ss << fname << ": " << msg << " (" << strerror(errno) << ")" << std::flush;
  dbglog(ss.str());
  throw std::runtime_error(ss.str());
}

// a stored name/value binding
struct binding {
  binding(const MonoTypePtr& type = MonoTypePtr(), size_t offset = 0, size_t boffset = 0) : type(type), offset(offset), boffset(boffset) { } // happy Josh?

  MonoTypePtr type;    // what type of data is stored here?
  size_t      offset;  // where is it stored?
  size_t      boffset; // where is this binding data stored in the file?
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
uint16_t restInPage(const imagefile* f, size_t idx) {
  return f->page_size - (idx % f->page_size);
}

// how many pages are covered by a sequence of bytes?
size_t pageCount(const imagefile* f, size_t sz) {
  return (sz / f->page_size) + ((sz % f->page_size) > 0 ? 1 : 0);
}

static void closeFile(imagefile* f) {
  if (f->fd > -1) {
    close(f->fd);
  }
  delete f;
}

static bool loadFileSize(imagefile* f) {
  struct stat sb;
  if (fstat(f->fd, &sb) < 0) {
    return false;
  }
  f->file_size = sb.st_size;
  return true;
}

static void seekAbs(const imagefile* f, size_t pos) {
  if (lseek(f->fd, pos, SEEK_SET) == -1) {
    raiseSysError("Can't seek to offset=" + str::from(pos) + " in file with size=" + str::from(f->file_size), f->path);
  }
}

static off_t filePosition(const imagefile* f) {
  off_t r = lseek(f->fd, 0, SEEK_CUR);
  if (r == (off_t)-1) {
    raiseSysError("Can't query position in file", f->path);
  }
  return r;
}

static void allocPages(imagefile* f, size_t pages) {
  size_t nsz = f->file_size + pages * f->page_size;
  if (ftruncate(f->fd, nsz) == -1) {
    raiseSysError("Can't resize file", f->path);
  }
  f->file_size = nsz;
}

static void allocPage(imagefile* f) {
  allocPages(f, 1);
}

// trivial read and write to files, assuming type T is POD
template <typename T>
  void write(imagefile* f, const T& x) {
    ssize_t wr = ::write(f->fd, (const void*)&x, sizeof(T));
    if ((wr == (ssize_t)-1) || (wr != sizeof(T))) {
      raiseSysError("Failed to write " + str::demangle<T>() + " to file", f->path);
    }
  }

template <typename TIter>
  void writes(imagefile* f, TIter begin, TIter end) {
    size_t  bsz = sizeof(*begin) * (end - begin);
    ssize_t wr  = ::write(f->fd, (const void*)&(*(begin)), bsz);
    if ((wr == (ssize_t)-1) || (wr != bsz)) {
      raiseSysError("Failed to write " + str::demangle< decltype(*begin) >() + " sequence to file", f->path);
    }
  }

void write(imagefile* f, const std::string& x) {
  write(f, x.size());
  writes(f, x.data(), x.data() + x.size());
}

void write(imagefile* f, const std::vector<unsigned char>& xs) {
  write(f, xs.size());
  writes(f, xs.begin(), xs.end());
}

template <typename T>
  void read(const imagefile* f, T* x) {
    ssize_t rr = ::read(f->fd, (void*)x, sizeof(T));
    if ((rr == (ssize_t)-1) || (rr != sizeof(T))) {
      raiseSysError("Failed to read " + str::demangle<T>() + " from file", f->path);
    }
  }

template <typename T>
  void reads(const imagefile* f, size_t sz, T* x) {
    size_t  bsz = sz * sizeof(T);
    ssize_t rr  = ::read(f->fd, (void*)x, bsz);
    if ((rr == (ssize_t)-1) || (rr != bsz)) {
      raiseSysError("Failed to read " + str::demangle<T>() + " sequence from file", f->path);
    }
  }

void read(imagefile* f, std::string* x) {
  size_t sz = 0;
  read(f, &sz);

  x->resize(sz);
  if (sz > 0) {
    reads(f, sz, &((*x)[0]));
  }
}

void read(imagefile* f, std::vector<unsigned char>* xs) {
  size_t sz = 0;
  read(f, &sz);

  xs->resize(sz);
  if (sz > 0) {
    reads(f, sz, &((*xs)[0]));
  }
}

// the absolute position of an indexed page
inline size_t pageOffset(const imagefile* f, file_pageindex_t page) {
  return f->page_size * page;
}

// the page index implied by an absolute position
inline file_pageindex_t pageIndex(const imagefile* f, uint64_t fpos) {
  return fpos / f->page_size;
}

// an absolute file position from a page and relative position
static size_t position(const imagefile* f, file_pageindex_t page, uint16_t offset) {
  return pageOffset(f, page) + ((uint64_t)offset);
}

// find the file page for a given TOC page
file_pageindex_t tocPageToFilePage(const imagefile* f, uint64_t tpage) {
  assert(tpage < f->tocpages.size());
  return f->tocpages[tpage];
}

// the absolute position for some TOC data
static uint64_t tocPosToFilePos(const imagefile* f, uint64_t spos) {
  uint64_t page   = spos / f->page_size;
  uint64_t offset = spos % f->page_size;

  return (tocPageToFilePage(f, page) * f->page_size) + offset;
}

// the absolute file position of a page's entry in the page table
static size_t pageTOCPosition(const imagefile* f, file_pageindex_t page) {
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
static void updatePageSizeIndex(imagefile* f, file_pageindex_t page) {
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
static void insertPageSizeIndex(imagefile* f, file_pageindex_t page) {
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
static bool findPageWithSpace(imagefile* f, pagetype::code pt, size_t datalen, size_t alignment, file_pageindex_t* idx) {
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
static void updateTOCData(imagefile* f, file_pageindex_t page, const pagedata& pd) {
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
static void appendTOCData(imagefile* f, const pagetable& newpages) {
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

    // the number of bytes written
    size_t bytes_written = slots_write * sizeof(pagedata);

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
size_t findSpace(imagefile* f, pagetype::code pt, size_t datalen, size_t alignment) {
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
static void addBinding(imagefile* f, const std::string& vname, const MonoTypePtr& type, size_t offset) {
  // prepare the binding data to save
  std::vector<unsigned char> etype;
  encode(type, &etype);

  // determine how much space we'll need to store it
  size_t bsz = sizeof(size_t)                // the stored data offset
             + sizeof(size_t) + vname.size() // the variable name
             + sizeof(size_t) + etype.size() // the variable's type
             ;

  // find the best place to put this data
  size_t boffset = findSpace(f, pagetype::environment, bsz, 1);

  // now just write the data there
  seekAbs(f, boffset);
  write(f, offset);
  write(f, vname);
  write(f, etype);

  // oh and we'll want to keep track of it too
  f->bindings[vname] = binding(type, offset, boffset);
}

// mmap a region out of this file
fregion& createFileRegionMap(imagefile* f, file_pageindex_t page, size_t pages) {
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
  char* d = (char*)mmap(0, pages * f->page_size, PROT_READ | (f->readonly ? 0 : PROT_WRITE), MAP_SHARED, f->fd, page * f->page_size);
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
void releaseFileRegionMap(imagefile* f, const fregion& fr) {
  if (munmap(fr.base, fr.pages * f->page_size) != 0) {
    raiseSysError("Failed to unmap page " + str::from(fr.base_page) + " from file", f->path);
  }
}

// the greatest map position <= a point
template <typename K, typename V>
  typename std::map<K, V>::iterator gleb(std::map<K, V>& m, const K& x) {
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
fregion& mappedFileRegion(imagefile* f, file_pageindex_t page, size_t pages) {
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
static char* mapFileData(imagefile* f, size_t fpos, size_t sz) {
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
static void unmapFileData(imagefile* f, void* p, size_t sz) {
  fallocs::iterator fa = f->allocs.find((char*)p);
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
uint16_t assertValidPageSize(const imagefile* f, size_t psize) {
  if (psize < minPageSize) {
    throw std::runtime_error(f->path + ": System page size too small for db support (" + str::from(psize) + ")");
  } else if (psize > maxPageSize) {
    throw std::runtime_error(f->path + ": System page size too large for db support (" + str::from(psize) + ")");
  } else if ((psize % sizeof(pagedata)) != 0) {
    throw std::runtime_error(f->path + ": System page size must be a multiple of " + str::from(sizeof(pagedata)) + " (" + str::from(psize) + ")");
  } else if ((sizeof(filehead) % sizeof(pagedata)) != 0) {
    // should be a static assert :T
    throw std::runtime_error("No page size is valid, file format internally inconsistent");
  }
  return (uint16_t)psize;
}

// put a new file into a valid empty state
static void createFile(imagefile* f) {
  if (f->readonly) {
    throw std::runtime_error("Can't initialize empty file for read: " + f->path);
  }
  f->page_size = assertValidPageSize(f, sysconf(_SC_PAGESIZE));
  f->version   = CURRENT_FILE_FORMAT_VERSION;

  // start the first page!
  allocPage(f);
  seekAbs(f, 0);

  // the first page of the file begins with a standard header
  filehead fh;
  memset(&fh, 0, sizeof(fh));
  fh.magic    = filePrefixBytes;
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
void readPageData(imagefile* f) {
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

// the representation of file reference types changed after v0
struct mapFileRefs : public switchTyFn {
  MonoTypePtr with(const TApp* ap) const {
    if (ap->args().size() == 2) {
      if (const Prim* f = is<Prim>(ap->fn())) {
        if (f->name() == "fileref") {
          return MonoTypePtr(TApp::make(ap->fn(), list(switchOf(ap->args()[1], *this))));
        }
      }
    }
    return MonoTypePtr(TApp::make(switchOf(ap->fn(), *this), switchOf(ap->args(), *this)));
  }
};

// read a single environment variable definition
static void readEnvironmentRecord(imagefile* f) {
  size_t                     offset;
  std::string                vname;
  std::vector<unsigned char> etype;
  size_t                     boffset = filePosition(f);

  // read the variable definition
  read(f, &offset);
  read(f, &vname);
  read(f, &etype);

  MonoTypePtr type = decode(etype);

  // keep track of it -- either by the v0 or v1 convention
  if (f->version == 0) {
    f->bindings[vname] = binding(tapp(primty("fileref"), list(switchOf(type, mapFileRefs()))), boffset, boffset);
  } else {
    f->bindings[vname] = binding(type, offset, boffset);
  }
}

// read environment data starting at some page
// (return the number of pages read to avoid erroneously double-reading environment data that might span multiple pages)
static size_t readEnvironmentPage(imagefile* f, file_pageindex_t p) {
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

  // now just report on the number of pages we've read (probably just 1 or 2)
  return pageCount(f, filePosition(f) - initOffset);
}

// read the file
static void readFile(imagefile* f) {
  // start reading the first page
  seekAbs(f, 0);

  // get the header, make sure it's sound
  filehead fh;
  read(f, &fh);

  if (fh.magic != filePrefixBytes) {
    throw std::runtime_error("Not a valid structured data file: " + f->path);
  } else if (fh.version > CURRENT_FILE_FORMAT_VERSION) {
    throw std::runtime_error("Cannot read newer file format (" + str::from(fh.version) + " > " + str::from(CURRENT_FILE_FORMAT_VERSION) + ")");
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
static imagefile* openFile(const std::string& fname, bool readonly) {
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
      readFile(f);
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
bool isDBFile(const std::string& fname) {
  imagefile* f = new imagefile();
  f->path      = fname;
  f->readonly  = true;

  try {
    // open it
    f->fd = open(fname.c_str(), O_RDONLY, 0);
    if (f->fd < 0) { closeFile(f); return false; }

    // get its size
    if (!loadFileSize(f)) { closeFile(f); return false; }

    // get the header
    seekAbs(f, 0);
    filehead fh;
    read(f, &fh);

    if (fh.magic != filePrefixBytes) { closeFile(f); return false; }
    f->page_size = assertValidPageSize(f, fh.pagesize);

    bool result = (f->page_size > 0) && (f->file_size % f->page_size) == 0;
    closeFile(f);
    return result;
  } catch (...) {
    closeFile(f);
    return false;
  }
}

// for storage, we need to make sure that size is computed slightly differently than for memory
size_t storageSizeOf(const MonoTypePtr& mty) {
  if (const Recursive* rty = is<Recursive>(mty)) {
    return sizeOf(unroll(mty));
  } else {
    return sizeOf(mty);
  }
}

// the base interface for reading data archives
reader::reader(imagefile* f) : fdata(f) {
  // just go ahead and map the whole file in
  mapFileData(this->fdata, 0, this->fdata->file_size);
}

reader::reader(const std::string& path) : fdata(openFile(path, true)) {
  // just go ahead and map the whole file in
  mapFileData(this->fdata, 0, this->fdata->file_size);
}

reader::~reader() {
  closeFile(this->fdata);
}

const std::string& reader::file() const {
  return this->fdata->path;
}

size_t reader::size() const {
  return this->fdata->file_size;
}

MonoTypeSubst reader::signature() const {
  MonoTypeSubst s;
  for (const auto& b : this->fdata->bindings) {
    s[b.first] = b.second.type;
  }
  return s;
}

void* reader::unsafeLookup(const std::string& vn, const MonoTypePtr& ty) const {
  auto b = this->fdata->bindings.find(vn);
  if (b == this->fdata->bindings.end()) {
    throw std::runtime_error("Variable undefined in database: " + vn);
  }

  if (*b->second.type == *ty) {
    return unsafeLoad(b->second.type, b->second.offset);
  } else {
    throw std::runtime_error("Database variable has unexpected type: " + hobbes::show(b->second.type) + " != " + hobbes::show(ty));
  }
}

uint64_t reader::unsafeLookupOffset(const std::string& vn, const MonoTypePtr& ty) const {
  auto b = this->fdata->bindings.find(vn);
  if (b == this->fdata->bindings.end()) {
    throw std::runtime_error("Variable undefined in database: " + vn);
  }

  if (*b->second.type == *ty) {
    return b->second.offset;
  } else {
    throw std::runtime_error("Database variable has unexpected type: " + hobbes::show(b->second.type) + " != " + hobbes::show(ty));
  }
}

uint64_t reader::unsafeOffsetOf(const MonoTypePtr& ty, void* p) const {
  return unsafeOffsetOfVal(is<Array>(ty), p);
}

uint64_t reader::unsafeOffsetOfVal(bool isArr, void* p) const {
  fallocs::const_iterator fa = gleb(this->fdata->allocs, (char*)p);
  if (fa == this->fdata->allocs.end()) {
    throw std::runtime_error("No file offset can be determined for unmapped memory");
  } else {
    fmappings::const_iterator fm = this->fdata->mappings.find(fa->second.page);
    if (fm == this->fdata->mappings.end()) {
      throw std::runtime_error("Internal error, inconsistent file mapping state");
    }

    return pageOffset(this->fdata, fm->second.base_page) + (((char*)p) - fm->second.base) - (isArr ? sizeof(long) : 0);
  }
}

bool storedAsArray(const MonoTypePtr& ty) {
  if (const Recursive* rty = is<Recursive>(ty)) {
    return storedAsArray(rty->recType());
  } else {
    return is<Array>(ty);
  }
}

void* reader::unsafeLoad(const MonoTypePtr& ty, uint64_t pos) const {
  if (pos > this->fdata->file_size) {
    // actually, this will be wrong in general since the writer might have written away from the reader
    throw std::runtime_error("Offset out of range of file");
  } else if (storedAsArray(ty)) {
    return unsafeLoadArray(pos);
  } else {
    return unsafeLoad(pos, storageSizeOf(ty));
  }
}

void* reader::unsafeLoad(uint64_t pos, size_t datasz) const {
  return mapFileData(this->fdata, pos, datasz);
}

uint64_t reader::unsafeArrayCapacity(uint64_t pos) const {
  seekAbs(this->fdata, pos);
  long datasz = 0;
  read(this->fdata, &datasz);
  return (uint64_t)datasz;
}

int reader::unsafeGetFD() const {
  return this->fdata->fd;
}

reader::PageEntries* reader::pageEntries() const {
  PageEntries* r = makeArray<PageEntry>(this->fdata->pages.size());
  for (size_t i = 0; i < r->size; ++i) {
    r->data[i].first  = this->fdata->pages[i].type();
    r->data[i].second = this->fdata->pages[i].size();
  }
  return r;
}

void* reader::unsafeLoadArray(uint64_t pos) const {
  // if we're loading an array, we actually have to read the data size first to know how much to map
  return ((unsigned char*)mapFileData(this->fdata, pos, unsafeArrayCapacity(pos))) + sizeof(long);
}

void reader::unsafeUnload(void* p, size_t sz) {
  unmapFileData(this->fdata, p, sz);
}

void reader::unsafeUnloadArray(void* p) {
  unsigned char* pxd = (((unsigned char*)p) - sizeof(long));
  unmapFileData(this->fdata, pxd, *((long*)pxd));
}

// change any file ref types to point to this reader because they must be out of this file (this is a bit of a hack)
MonoTypePtr mkFR(const MonoTypePtr& t) {
  return tapp(primty("fileref"), list(t));
}

void reader::showFileSummary(std::ostream& out) const {
  out << this->fdata->path << " : " << str::showDataSize(this->fdata->file_size) << std::endl;
}

void reader::showEnvironment(std::ostream& out) const {
  str::seqs table;
  table.resize(3);

  for (const auto& b : this->fdata->bindings) {
    static const size_t maxTypeLen = 100;
    std::string tn = hobbes::show(b.second.type);
    if (tn.size() > 100) { tn = tn.substr(0, 100) + "..."; }

    table[0].push_back(b.first);
    table[1].push_back("::");
    table[2].push_back(tn);
  }

  str::printHeadlessLeftAlignedTable(out, table);
}

void reader::showMappings(std::ostream& out, size_t pageRows) const {
  static const size_t bytesPerRow = 20;
  static const size_t bytesToShow = std::min<size_t>(bytesPerRow * pageRows, this->fdata->page_size);

  for (const auto& m : this->fdata->mappings) {
    out << "map from page " << m.first << " for " << m.second.pages << " page(s) at address " << (void*)m.second.base << std::endl;

    for (size_t i = 0; i < bytesToShow; ++i) {
      out << str::hex(*((unsigned char*)m.second.base + i)) << " ";
      if (((i+1) % bytesPerRow) == 0) {
        out << std::endl;
      }
    }
    if (bytesToShow < this->fdata->page_size) {
      out << "..." << std::endl;
    }
  }
}

std::string showPageDesc(const pagedata& pd) {
  std::ostringstream ss;
  switch (pd.type()) {
  case pagetype::toc:         ss << "T"; break;
  case pagetype::environment: ss << "E"; break;
  case pagetype::data:        ss << "D"; break;
  default:                    ss << "?"; break;
  }

  std::string sz = str::from(pd.size());
  for (size_t p = sz.size(); p < 5; ++p) {
    ss << "0";
  }
  ss << sz;
  return ss.str();
}

void reader::showPageTable(std::ostream& out, size_t maxRows) const {
  static const size_t cellSize = 6;
  static const size_t rowSize  = 10;

  // gather page descriptions
  str::seq pdescs;
  for (size_t i = 0; i < this->fdata->pages.size(); ++i) {
    if (i == rowSize * maxRows) {
      break;
    }
    pdescs.push_back(showPageDesc(this->fdata->pages[i]));
  }

  // show the page table
  out << "page table:" << std::endl;
  for (size_t i = 0; i < pdescs.size(); ++i) {
    out << pdescs[i] << "|";
    if (((i+1) % rowSize) == 0) {
      out << std::endl << std::string(rowSize * (cellSize + 1), '-') << std::endl;
    }
  }
  if (pdescs.size() < this->fdata->pages.size()) {
    out << "..." << std::endl;
  }
  out << std::endl;
}

void reader::show(std::ostream& out) const {
  showEnvironment(out);
}

// the interface for writing data archives
writer::writer(const std::string& path) : reader(openFile(path, false)) {
}

void* writer::unsafeDefine(const std::string& vn, const MonoTypePtr& ty) {
  return allocNamed(vn, ty, storageSizeOf(ty));
}

void* writer::unsafeDefine(const std::string& vn, const MonoTypePtr& ty, size_t len) {
  // allocate an extra bit of space in this array data to store the actual allocated length
  // (this allows storage of arrays with a reported length lower than capacity)
  size_t datasz = sizeof(long) + sizeof(long) + (len * storageSizeOf(ty));
  unsigned char* result = (unsigned char*)allocNamed(vn, arrayty(ty), datasz);
  *((long*)result) = datasz;
  return (void*)(result + sizeof(long));
}

void* writer::allocNamed(const std::string& vn, const MonoTypePtr& ty, size_t datasz) {
  // make sure that we're not redefining a value that's already defined
  if (this->fdata->bindings.find(vn) != this->fdata->bindings.end()) {
    throw std::runtime_error("Variable already defined in database: " + vn);
  }

  // allocate space for the data
  size_t dloc = findSpace(this->fdata, pagetype::data, datasz, alignment(ty));

  // add the environment binding
  addBinding(this->fdata, vn, ty, dloc);

  // get the mapped address of this data
  return mapFileData(this->fdata, dloc, datasz);
}

void* writer::unsafeStore(const MonoTypePtr& ty) {
  return allocAnon(storageSizeOf(ty), alignment(ty));
}

void* writer::unsafeStore(const MonoTypePtr& ty, size_t len) {
  return unsafeStoreArray(storageSizeOf(ty), len);
}

void* writer::unsafeStore(size_t sz, size_t align) {
  return allocAnon(sz, align);
}

uint64_t writer::unsafeStoreToOffset(size_t sz, size_t align) {
  return findSpace(this->fdata, pagetype::data, sz, align);
}

void* writer::unsafeStoreArray(size_t esize, size_t len) {
  // allocate an extra bit of space in this array data to store the actual allocated length
  // (this allows anonymous storage of arrays with a reported length lower than capacity)
  size_t datasz = sizeof(long) + sizeof(long) + (len * esize);
  unsigned char* result = (unsigned char*)allocAnon(datasz, sizeof(size_t));
  *((long*)result) = datasz;
  return (void*)(result + sizeof(long));
}

uint64_t writer::unsafeStoreArrayToOffset(size_t esize, size_t len) {
  void*    p = unsafeStoreArray(esize, len);
  uint64_t r = unsafeOffsetOfVal(true, p);

  unsafeUnloadArray(p);
  return r;
}

void writer::signalUpdate() {
  // write a (safe) dummy byte to the file header to trigger an update signal
  seekAbs(this->fdata, 0);
  write(this->fdata, (uint8_t)0x0d);
}

void* writer::allocAnon(size_t datasz, size_t align) {
  // allocate space for this data
  size_t dloc = findSpace(this->fdata, pagetype::data, datasz, align);

  // get the mapped address of this data
  return mapFileData(this->fdata, dloc, datasz);
}

// convenience storage methods
fileref<array<char>*> writer::store(const char* x, size_t sz) {
  return store(x, x + sz);
}

fileref<array<char>*> writer::store(const char* x) {
  return store(x, strlen(x));
}

fileref<array<char>*> writer::store(const array<char>* x) {
  return store(x->data, x->size);
}

fileref<array<char>*> writer::store(const std::string& x) {
  return store(x.begin(), x.end());
}

// create every directory in a path if it doesn't exist already
//  (equivalent to 'mkdir -p')
void ensureDirExists(const std::string& path) {
  str::seq ps = str::csplit(path, "/");
  std::ostringstream pfx;

  for (const auto& p : ps) {
    pfx << p << "/";
    if (mkdir(pfx.str().c_str(), S_IRWXU | S_IRWXG | S_IRWXO) == -1 && errno != EEXIST) {
      throw std::runtime_error("Failed to make directory '" + pfx.str() + "' with error: " + strerror(errno));
    }
  }
}

std::string withUniqueFilenameBy(const std::string& fprefix, const std::string& fsuffix, const std::function<bool(const std::string&)>& fileOp) {
  // the directory that this file is in can be created if necessary
  ensureDirExists(str::rsplit(fprefix, "/").first);

  // we'll want the current date/time in the name
  time_t now = ::time(0);
  const tm* t = localtime(&now);
  
  // keep trying for new filenames until we get one that's distinct
  size_t inst = 0;
  while (true) {
    std::ostringstream ss;
    ss << fprefix << t->tm_year + 1900 << "." << (t->tm_mon < 9 ? "0" : "") << t->tm_mon + 1 << "." << t->tm_mday << "-" << inst << fsuffix;
    if (fileOp(ss.str())) {
      return ss.str();
    }
    ++inst;
  }
}

// generate a new file with a given prefix & suffix
std::string uniqueFilename(const std::string& fprefix, const std::string& fsuffix) {
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
std::string moveToUniqueFilename(const std::string& oldpath, const std::string& fprefix, const std::string& fsuffix) {
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

}

