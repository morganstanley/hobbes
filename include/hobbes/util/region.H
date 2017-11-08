
#ifndef HOBBES_UTIL_REGION_HPP_INCLUDED
#define HOBBES_UTIL_REGION_HPP_INCLUDED

#include <string>

namespace hobbes {

struct mempage {
  void*    base;
  size_t   size;
  size_t   read;
  mempage* succ;
};

class region {
public:
  region(size_t minPageSize, size_t initialFreePages = 1, size_t maxPageSize = /*1GB*/ 1*1024*1024*1024);
  ~region();

  // allocate 'sz' bytes of memory
  void* malloc(size_t sz);

  // deallocate all pages (make sure you're not holding any references after this)
  void clear();

  // reset the page read pointer
  //   (this is logically equivalent to 'clear' but doesn't deallocate pages -- new allocations happen over old ones)
  void reset();

  // inspect the state of this memory region
  size_t allocated() const; // how much memory is allocated by this region in all?
  size_t used()      const; // how much of allocated memory is actually used?
  size_t wasted()    const; // how much of allocated memory is unavailable for use?

  std::string show() const;

  // support catastrophic self-destruct on memory caps
  void abortAtMemCeiling(size_t);
private:
  size_t minPageSize;
  size_t maxPageSize;
  size_t lastAllocPageSize;
  bool   abortOnOOM;
  size_t maxTotalAllocation;
  size_t totalAllocation;

  mempage* usedp;
  mempage* freep;

  mempage* newpage(mempage* succ, size_t sz);
  void allocpage(size_t sz);
  
  void freepage(mempage* p);
  void freepages(mempage* p);
};

}

#endif

