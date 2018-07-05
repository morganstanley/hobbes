
#include <hobbes/hobbes.H>
#include <hobbes/fregion.H>
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

using namespace hobbes::fregion;

namespace hobbes {

bool isDBFile(const std::string& f) {
  return isFRegion(f);
}

// does a type represent a 'darray' (the previous storage for all arrays "up to capacity")?
static bool isDArray(const MonoTypePtr& t) {
  if (const TApp* ap = is<TApp>(t)) {
    if (ap->args().size() == 1) {
      if (const Prim* f = is<Prim>(ap->fn())) {
        if (f->name() == "darray") {
          return true;
        }
      }
    }
  }
  return false;
}

MonoTypePtr darrayty(const MonoTypePtr& t) {
  return tapp(primty("darray", tabs(str::strings("x"), arrayty(t))), list(t));
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

// the representation of stored array types changed after v1
struct mapStoredArrays : public switchTyFn {
  MonoTypePtr with(const Array* a) const {
    return darrayty(switchOf(a->type(), *this));
  }
};

// translate a stored type by file version
static MonoTypePtr v0_to_v1(const MonoTypePtr& t) {
  return tapp(primty("fileref"), list(switchOf(t, mapFileRefs())));
}

static MonoTypePtr v1_to_v2(const MonoTypePtr& t) {
  return switchOf(t, mapStoredArrays());
}

static MonoTypePtr decodeBindingByVersion(uint16_t version, const binding& b) {
  switch (version) {
  case 0:  return v1_to_v2(v0_to_v1(decode(b.type)));
  case 1:  return v1_to_v2(decode(b.type));
  default: return decode(b.type);
  }
}

// for storage, we need to make sure that size is computed slightly differently than for memory
size_t storageSizeOf(const MonoTypePtr& mty) {
  if (is<Recursive>(mty)) {
    return sizeOf(unroll(mty));
  } else {
    return sizeOf(mty);
  }
}

// the base interface for reading data archives
reader::reader(imagefile* f) : fdata(f) {
  // just go ahead and map the whole file in
  mapFileData(this->fdata, 0, this->fdata->file_size);

  // and translate bindings
  for (const auto& b : this->fdata->bindings) {
    if (b.first.size() > 0 && b.first[0] != '.') {
      SBinding sb;
      sb.type   = decodeBindingByVersion(this->fdata->version, b.second.type);
      sb.offset = this->fdata->version==0 ? b.second.boffset : b.second.offset;
      this->sbindings[b.first] = sb;
    }
  }
}

reader::reader(const std::string& path) : reader(openFile(path, true, 0, HFREGION_CURRENT_FILE_FORMAT_VERSION)) {
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
  for (const auto& b : this->sbindings) {
    s[b.first] = b.second.type;
  }
  return s;
}

bool reader::isDefined(const std::string& vn) const {
  return this->sbindings.find(vn) != this->sbindings.end();
}

void* reader::unsafeLookup(const std::string& vn, const MonoTypePtr& ty) const {
  return unsafeLoad(ty, unsafeLookupOffset(vn, ty));
}

uint64_t reader::unsafeLookupOffset(const std::string& vn, const MonoTypePtr& ty) const {
  auto b = this->sbindings.find(vn);
  if (b == this->sbindings.end()) {
    throw std::runtime_error("Variable undefined in database: " + vn);
  }

  if (*b->second.type == *ty) {
    return b->second.offset;
  } else {
    throw std::runtime_error("Database variable has unexpected type: " + hobbes::show(b->second.type) + " != " + hobbes::show(ty));
  }
}

uint64_t reader::unsafeOffsetOf(const MonoTypePtr& ty, const void* p) const {
  return unsafeOffsetOfVal(isDArray(ty), p);
}

uint64_t reader::unsafeOffsetOfVal(bool isDArr, const void* p) const {
  fallocs::const_iterator fa = gleb(this->fdata->allocs, crcast<char*>(p));
  if (fa == this->fdata->allocs.end()) {
    throw std::runtime_error("No file offset can be determined for unmapped memory");
  } else {
    fmappings::const_iterator fm = this->fdata->mappings.find(fa->second.page);
    if (fm == this->fdata->mappings.end()) {
      throw std::runtime_error("Internal error, inconsistent file mapping state");
    }

    return pageOffset(this->fdata, fm->second.base_page) + (reinterpret_cast<const char*>(p) - fm->second.base) - (isDArr ? sizeof(long) : 0);
  }
}

const Array* storedAsArray(const MonoTypePtr& ty) {
  if (const Recursive* rty = is<Recursive>(ty)) {
    return storedAsArray(rty->recType());
  } else {
    return is<Array>(ty);
  }
}
bool storedAsDArray(const MonoTypePtr& ty) {
  if (const Recursive* rty = is<Recursive>(ty)) {
    return storedAsDArray(rty->recType());
  } else {
    return isDArray(ty);
  }
}

void* reader::unsafeLoad(const MonoTypePtr& ty, uint64_t pos) const {
  if (pos > this->fdata->file_size) {
    // actually, this will be wrong in general since the writer might have written away from the reader
    throw std::runtime_error("Offset out of range of file");
  } else if (const Array* a = storedAsArray(ty)) {
    return unsafeLoadArray(pos, storageSizeOf(a->type()));
  } else if (storedAsDArray(ty)) {
    return unsafeLoadDArray(pos);
  } else {
    return unsafeLoad(pos, storageSizeOf(ty));
  }
}

void* reader::unsafeLoad(uint64_t pos, size_t datasz) const {
  return mapFileData(this->fdata, pos, datasz);
}

uint64_t reader::unsafeDArrayCapacity(uint64_t pos) const {
  uint64_t* cap    = reinterpret_cast<uint64_t*>(mapFileData(this->fdata, pos, sizeof(uint64_t)));
  uint64_t  result = *cap;
  unmapFileData(this->fdata, cap, sizeof(uint64_t));
  return result;
}
void* reader::unsafeLoadDArray(uint64_t pos) const {
  // if we're loading a darray, we actually have to read the data size first to know how much to map
  uint64_t* cap    = reinterpret_cast<uint64_t*>(mapFileData(this->fdata, pos, sizeof(uint64_t)));
  uint8_t*  result = reinterpret_cast<uint8_t*>(mapFileData(this->fdata, pos+sizeof(uint64_t), *cap));
  unmapFileData(this->fdata, cap, sizeof(uint64_t));
  return result;
}

void* reader::unsafeLoadArray(uint64_t pos, size_t sz) const {
  uint64_t* len    = reinterpret_cast<uint64_t*>(mapFileData(this->fdata, pos, sizeof(uint64_t)));
  uint8_t*  result = reinterpret_cast<uint8_t*>(mapFileData(this->fdata, pos, sizeof(uint64_t) + sz * *len));
  unmapFileData(this->fdata, len, sizeof(uint64_t));
  return result;
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

void reader::addSBinding(const std::string& vn, const MonoTypePtr& t, uint64_t offset) {
  SBinding sb;
  sb.type   = t;
  sb.offset = offset;
  this->sbindings[vn] = sb;
}

void reader::unsafeUnload(void* p, size_t sz) {
  unmapFileData(this->fdata, p, sz);
}

void reader::unsafeUnloadDArray(void* p) {
  unsigned char* pxd = reinterpret_cast<unsigned char*>(p) - sizeof(long);
  unmapFileData(this->fdata, pxd, *reinterpret_cast<long*>(pxd));
}

void reader::unsafeUnloadArray(void* p, size_t sz) {
  unmapFileData(this->fdata, p, sizeof(uint64_t) + sz * *reinterpret_cast<uint64_t*>(p));
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

  for (const auto& b : this->sbindings) {
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
    out << "map from page " << m.first << " for " << m.second.pages << " page(s) at address " << reinterpret_cast<void*>(m.second.base) << std::endl;

    for (size_t i = 0; i < bytesToShow; ++i) {
      out << str::hex(*(reinterpret_cast<unsigned char*>(m.second.base) + i)) << " ";
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
writer::writer(const std::string& path) : reader(openFile(path, false, 0, HFREGION_CURRENT_FILE_FORMAT_VERSION)) {
}

void* writer::unsafeDefine(const std::string& vn, const MonoTypePtr& ty) {
  return allocNamed(vn, ty, storageSizeOf(ty));
}

void* writer::unsafeDefineDArray(const std::string& vn, const MonoTypePtr& ty, size_t len) {
  // allocate an extra bit of space in this array data to store the actual allocated length
  size_t datasz = sizeof(long) + sizeof(long) + (len * storageSizeOf(ty));
  unsigned char* result = reinterpret_cast<unsigned char*>(allocNamed(vn, darrayty(ty), datasz));
  *reinterpret_cast<uint64_t*>(result) = datasz;
  return reinterpret_cast<void*>(result + sizeof(long));
}

void* writer::unsafeDefineArray(const std::string& vn, const MonoTypePtr& ty, size_t len) {
  size_t datasz = sizeof(long) + (len * storageSizeOf(ty));
  unsigned char* result = reinterpret_cast<unsigned char*>(allocNamed(vn, arrayty(ty), datasz));
  *reinterpret_cast<uint64_t*>(result) = len;
  return result;
}

void* writer::allocNamed(const std::string& vn, const MonoTypePtr& ty, size_t datasz) {
  // make sure that we're not redefining a value that's already defined
  if (this->fdata->bindings.find(vn) != this->fdata->bindings.end()) {
    throw std::runtime_error("Variable already defined in database: " + vn);
  }

  // allocate space for the data
  size_t dloc = findSpace(this->fdata, pagetype::data, datasz, alignment(ty));

  // add the environment binding
  bytes ety; encode(ty, &ety);
  addBinding(this->fdata, vn, ety, dloc);
  addSBinding(vn, ty, dloc);

  // get the mapped address of this data
  return mapFileData(this->fdata, dloc, datasz);
}

void* writer::unsafeStore(const MonoTypePtr& ty) {
  return allocAnon(storageSizeOf(ty), alignment(ty));
}

void* writer::unsafeStore(size_t sz, size_t align) {
  return allocAnon(sz, align);
}

uint64_t writer::unsafeStoreToOffset(size_t sz, size_t align) {
  return findSpace(this->fdata, pagetype::data, sz, align);
}

void* writer::unsafeStoreDArray(size_t esize, size_t len) {
  size_t datasz = sizeof(long) + sizeof(long) + (len * esize);
  unsigned char* result = reinterpret_cast<unsigned char*>(allocAnon(datasz, sizeof(size_t)));
  *reinterpret_cast<long*>(result) = datasz;
  return reinterpret_cast<void*>(result + sizeof(long));
}

uint64_t writer::unsafeStoreDArrayToOffset(size_t esize, size_t len) {
  void*    p = unsafeStoreDArray(esize, len);
  uint64_t r = unsafeOffsetOfVal(true, p);

  unsafeUnloadDArray(p);
  return r;
}

void* writer::unsafeStoreArray(size_t esize, size_t len) {
  if (len > 0) {
    unsigned char* result = reinterpret_cast<unsigned char*>(allocAnon(sizeof(long) + (len * esize), sizeof(size_t)));
    *reinterpret_cast<long*>(result) = len;
    return result;
  } else {
    return mapFileData(this->fdata, this->fdata->empty_array, sizeof(size_t));
  }
}

uint64_t writer::unsafeStoreArrayToOffset(size_t esize, size_t len) {
  void*    p = unsafeStoreArray(esize, len);
  uint64_t r = unsafeOffsetOfVal(false, p);

  unsafeUnloadArray(p, esize);
  return r;
}

void writer::signalUpdate() {
  // write a (safe) dummy byte to the file header to trigger an update signal
  seekAbs(this->fdata, 0);
  write(this->fdata, static_cast<uint8_t>(0x0d));
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
    if (mkdir(pfx.str().c_str(), S_IRWXU | S_IRWXG | S_IRWXO) == -1 && errno != EEXIST && errno != EISDIR) {
      throw std::runtime_error("Failed to make directory '" + pfx.str() + "' with error: " + strerror(errno));
    }
  }
}

std::string withUniqueFilenameBy(const std::string& fprefix, const std::string& fsuffix, const std::function<bool(const std::string&)>& fileOp) {
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

