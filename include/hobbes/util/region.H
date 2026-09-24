
#ifndef HOBBES_UTIL_REGION_HPP_INCLUDED
#define HOBBES_UTIL_REGION_HPP_INCLUDED

#include <cstddef>
#include <stdexcept>
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

  // allocate 'sz' bytes of memory aligned to 'asz'
  void* malloc(size_t sz, size_t asz = sizeof(size_t));

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

// The byte size of an array header (sizeof(long)) plus n elements of esz bytes,
// refused rather than wrapped if it cannot be represented. The makeArray /
// cc::makeArray / makeString family computes sizeof(long) + sizeof(T) * n
// directly, which wraps to a small size before region::malloc can see it -- a
// tiny buffer allocated while the array's size field claims the full n, so
// element construction and later access run past the end (STRFR-434031).
//
// n is a size_t: the count as it is actually used to size and index the array.
// The signed-length overloads pass their length through here after conversion,
// so a negative one -- what a script passing -1 sign-extends to -- arrives as a
// huge count and is rejected as unrepresentable, the same as any other size
// that will not fit.
inline size_t checkedArrayAllocSize(size_t n, size_t esz) {
  size_t bytes = 0;
  if (__builtin_mul_overflow(n, esz, &bytes) ||
      __builtin_add_overflow(bytes, sizeof(long), &bytes)) {
    throw std::runtime_error("array allocation size is not representable: " +
                             std::to_string(n) + " elements of " + std::to_string(esz) + " bytes");
  }
  return bytes;
}

}

#endif

