
#include <algorithm>
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
  size_t nu = this->usedp->read + sz;
  if (nu + asz <= this->usedp->size) {
    uint8_t* uresult = reinterpret_cast<uint8_t*>(this->usedp->base) + this->usedp->read;
    size_t   afixup  = align(reinterpret_cast<size_t>(uresult), asz) - reinterpret_cast<size_t>(uresult);
    uint8_t* result  = uresult + afixup;

    this->usedp->read = nu + afixup;
    return result;
  } else {
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

