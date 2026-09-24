
#include <algorithm>
#include <stdexcept>
#include <cstdlib>
#include <hobbes/util/ptr.H>
#include <hobbes/util/region.H>
#include <hobbes/util/str.H>

namespace hobbes {

void dbglog(const std::string&);

region::region(size_t minPageSize, size_t initialFreePages, size_t maxPageSize) :
  minPageSize(minPageSize), maxPageSize(maxPageSize), lastAllocPageSize(minPageSize),
  abortOnOOM(false), maxTotalAllocation(0), totalAllocation(0), usedp(nullptr), freep(nullptr)
{
  this->usedp = newpage(nullptr, minPageSize);

  for (size_t i = 0; i < initialFreePages; ++i) {
    this->freep = newpage(this->freep, minPageSize);
  }
}

region::~region() {
  clear();
  freepage(this->usedp);
}

void* region::malloc(size_t sz, size_t asz) {
  // read + sz + asz is the high-water mark this request needs to fit. Computed
  // in size_t with no check, a huge sz wraps that sum to a small value, so the
  // "does it fit" test below passes and a block with almost no usable space is
  // handed back -- which the caller then writes past, rewinding the bump
  // cursor over live objects (STRFR-433992). Reject a size that cannot even be
  // represented before the wrapping arithmetic can run. asz (alignment) is
  // small and read <= size always holds, so this only fires for a genuinely
  // out-of-range sz.
  size_t need = 0;
  if (__builtin_add_overflow(sz, asz, &need) ||
      __builtin_add_overflow(need, this->usedp->read, &need)) {
    throw std::runtime_error("region allocation of " + str::from(sz) + " bytes is too large to represent");
  }

  if (need <= this->usedp->size) {
    // fits in the current page; need <= size and afixup < asz, so no wrap
    uint8_t* uresult = reinterpret_cast<uint8_t*>(this->usedp->base) + this->usedp->read;
    size_t   afixup  = align(reinterpret_cast<size_t>(uresult), asz) - reinterpret_cast<size_t>(uresult);
    uint8_t* result  = uresult + afixup;

    this->usedp->read = (this->usedp->read + sz) + afixup;
    return result;
  } else {
    // sz + asz was checked above (it is the first sum in 'need')
    allocpage(sz + asz);

    auto* uresult = reinterpret_cast<uint8_t*>(this->usedp->base);
    size_t   afixup  = align(reinterpret_cast<size_t>(uresult), asz) - reinterpret_cast<size_t>(uresult);
    uint8_t* result  = uresult + afixup;

    this->usedp->read = sz + afixup;
    return result;
  }
}

void region::clear() {
  freepages(this->freep);
  freepages(this->usedp->succ);

  this->freep       = nullptr;
  this->usedp->read = 0;
  this->usedp->succ = nullptr;

  this->lastAllocPageSize = this->minPageSize;
}

void region::reset() {
  // reset all read pointers in used pages
  // link the final used page to the initial free page
  // finally set free to used, having computed free' = used ++ free
  mempage* p = this->usedp;
  while (p != nullptr) {
    mempage* np = p->succ;

    p->read = 0;
    if (np == nullptr) {
      p->succ = this->freep;
    }
    p = np;
  }
  this->freep = this->usedp->succ;
  this->usedp->succ = nullptr;
}

namespace pattr {
  enum E {
    allocated,
    used,
    wasted
  };
};

size_t sumAttr(const mempage* p, pattr::E a) {
  size_t r = 0;
  switch (a) {
  case pattr::allocated:
    while (p != nullptr) {
      r += p->size;
      p = p->succ;
    }
    break;
  case pattr::used:
    while (p != nullptr) {
      r += p->read;
      p = p->succ;
    }
    break;
  case pattr::wasted:
    p = p != nullptr ? p->succ : p;
    while (p != nullptr) {
      r += p->size - p->read;
      p = p->succ;
    }
    break;
  }
  return r;
}

size_t region::allocated() const {
  return sumAttr(this->usedp, pattr::allocated) + sumAttr(this->freep, pattr::allocated);
}

size_t region::used() const {
  return sumAttr(this->usedp, pattr::used);
}

size_t region::wasted() const {
  return sumAttr(this->usedp, pattr::wasted);
}

std::string showPage(mempage* p) {
  return "{sz=" + str::showDataSize(p->size) + ",read=" + str::showDataSize(p->read) + ",base=" + str::from(reinterpret_cast<void*>(p->base)) + "}";
}

std::string showPages(mempage* ps) {
  if (ps == nullptr) {
    return "[]";
  } else {
    std::string r = showPage(ps);
    ps = ps->succ;
    while (ps != nullptr) {
      r += "; ";
      r += showPage(ps);
      ps = ps->succ;
    }
    return "[" + r + "]";
  }
}

std::string region::show() const {
  return "{used=" + showPages(this->usedp) + ", free=" + showPages(this->freep) + "}";
}

void region::abortAtMemCeiling(size_t maxsz) {
  this->abortOnOOM         = true;
  this->maxTotalAllocation = maxsz;
}

mempage* region::newpage(mempage* succ, size_t sz) {
  size_t psz = 0;
  if (this->lastAllocPageSize < this->maxPageSize) {
    psz = std::max(sz, this->lastAllocPageSize);
    this->lastAllocPageSize *= 2;
  } else {
    psz = std::max(sz, this->maxPageSize);
  }

  this->totalAllocation += psz;
  if (this->abortOnOOM && this->totalAllocation >= this->maxTotalAllocation) {
    // we've gone too far, and we've been asked to abort in this case
    dbglog("aborting on out-of-memory condition");
    abort();
  }

  auto* p = new mempage;
  p->size = psz;
  p->base = ::malloc(p->size);
  p->read = 0;
  p->succ = succ;

  return p;
}

void region::allocpage(size_t sz) {
  if (this->freep != nullptr && sz <= this->freep->size) {
    // used = head free : used
    mempage* usednp = this->freep;
    mempage* freenp = this->freep->succ;

    usednp->succ = this->usedp;

    this->usedp = usednp;

    // free = tail free
    this->freep = freenp;
  } else {
    this->usedp = newpage(this->usedp, sz);
  }
}

void region::freepage(mempage* p) {
  this->totalAllocation -= p->size;
  ::free(p->base);
  delete p;
}

void region::freepages(mempage* p) {
  while (p != nullptr) {
    mempage* np = p->succ;
    freepage(p);
    p = np;
  }
}

}

