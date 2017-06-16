
#ifndef HOBBES_UTIL_REGION_HPP_INCLUDED
#define HOBBES_UTIL_REGION_HPP_INCLUDED

#include <string>

namespace hobbes {

struct mempage {
  void*        base;
  unsigned int size;
  unsigned int read;
  mempage*     succ;
};

class region {
public:
  region(unsigned int minPageSize, unsigned int initialFreePages = 1, unsigned int maxPageSize = /*1GB*/ 1*1024*1024*1024);
  ~region();

  // allocate 'sz' bytes of memory
  void* malloc(unsigned int sz);

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
  unsigned int minPageSize;
  unsigned int maxPageSize;
  unsigned int lastAllocPageSize;
  bool         abortOnOOM;
  size_t       maxTotalAllocation;
  size_t       totalAllocation;

  mempage* usedp;
  mempage* freep;

  mempage* newpage(mempage* succ, unsigned int sz);
  void allocpage(unsigned int sz);
  
  void freepage(mempage* p);
  void freepages(mempage* p);
};

}

#endif

