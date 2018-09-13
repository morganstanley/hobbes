
#include <hobbes/util/region.H>
#include <hobbes/util/str.H>
#include <stdlib.h>
#include <algorithm>

namespace hobbes {

void dbglog(const std::string&);

region::region(size_t minPageSize, size_t initialFreePages, size_t maxPageSize) :
  minPageSize(minPageSize), maxPageSize(maxPageSize), lastAllocPageSize(minPageSize),
  abortOnOOM(false), maxTotalAllocation(0), totalAllocation(0), usedp(0), freep(0)
{
  this->usedp = newpage(0, minPageSize);

  for (size_t i = 0; i < initialFreePages; ++i) {
    this->freep = newpage(this->freep, minPageSize);
  }
}

region::~region() {
  clear();
  freepage(this->usedp);
}

void* region::malloc(size_t sz) {
  size_t nr = this->usedp->read + sz;
  if (nr <= this->usedp->size) {
    void* result = reinterpret_cast<unsigned char*>(this->usedp->base) + this->usedp->read;
    this->usedp->read = nr;
    return result;
  } else {
    allocpage(sz);
    this->usedp->read = sz;
    return this->usedp->base;
  }
}

void region::clear() {
  freepages(this->freep);
  freepages(this->usedp->succ);

  this->freep       = 0;
  this->usedp->read = 0;
  this->usedp->succ = 0;

  this->lastAllocPageSize = this->minPageSize;
}

void region::reset() {
  // reset all read pointers in used pages
  // link the final used page to the initial free page
  // finally set free to used, having computed free' = used ++ free
  mempage* p = this->usedp;
  while (p != 0) {
    mempage* np = p->succ;

    p->read = 0;
    if (np == 0) {
      p->succ = this->freep;
    }
    p = np;
  }
  this->freep = this->usedp->succ;
  this->usedp->succ = 0;
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
    while (p) {
      r += p->size;
      p = p->succ;
    }
    break;
  case pattr::used:
    while (p) {
      r += p->read;
      p = p->succ;
    }
    break;
  case pattr::wasted:
    p = p ? p->succ : p;
    while (p) {
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
  if (ps == 0) {
    return "[]";
  } else {
    std::string r = showPage(ps);
    ps = ps->succ;
    while (ps != 0) {
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

  mempage* p = new mempage;
  p->size = psz;
  p->base = ::malloc(p->size);
  p->read = 0;
  p->succ = succ;

  return p;
}

void region::allocpage(size_t sz) {
  if (this->freep != 0 && sz <= this->freep->size) {
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
  while (p != 0) {
    mempage* np = p->succ;
    freepage(p);
    p = np;
  }
}

}

