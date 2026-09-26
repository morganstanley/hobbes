
#include "test.H"
#include <hobbes/util/region.H>
#include <limits>

// The oversized-allocation test below needs a malloc that fails by returning
// null. AddressSanitizer does not do that: it intercepts a request past its
// maximum supported size and reports allocation-size-too-big instead, so the
// condition cannot be provoked under it (its own hint is to set
// allocator_may_return_null=1, which would relax that reporting for the whole
// test binary). The code path is unchanged there; only the way to reach it is
// unavailable, and the rest of the build matrix still covers it.
#ifndef HOBBES_TEST_SKIP_UNSATISFIABLE_ALLOC
#  if defined(__SANITIZE_ADDRESS__)
#    define HOBBES_TEST_SKIP_UNSATISFIABLE_ALLOC 1
#  elif defined(__has_feature)
#    if __has_feature(address_sanitizer)
#      define HOBBES_TEST_SKIP_UNSATISFIABLE_ALLOC 1
#    endif
#  endif
#endif
#ifndef HOBBES_TEST_SKIP_UNSATISFIABLE_ALLOC
#  define HOBBES_TEST_SKIP_UNSATISFIABLE_ALLOC 0
#endif

using namespace hobbes;

// region::malloc is the bump allocator behind every hobbes runtime
// allocation. It computed read + sz + asz in size_t with no check, so a huge
// sz wrapped the sum small, the "does it fit" test passed, and it returned a
// block with almost no usable space that the caller then wrote past, rewinding
// the cursor over live objects (STRFR-433992). A size that cannot be
// represented is now refused before the arithmetic can wrap.
TEST(Region, mallocRefusesASizeThatWouldOverflow) {
  region r(4096);

  // ordinary allocations still work, and are distinct and usable
  void* a = r.malloc(64);
  void* b = r.malloc(64);
  EXPECT_TRUE(a != nullptr && b != nullptr && a != b);

  // one larger than a page still works (it takes its own page)
  void* big = r.malloc(1024 * 1024);
  EXPECT_TRUE(big != nullptr);

  // the wrap: a size whose sum with alignment cannot be represented is refused
  // rather than wrapping the fit-check and handing back a tiny block
  EXPECT_EXCEPTION(r.malloc(std::numeric_limits<size_t>::max()));
  EXPECT_EXCEPTION(r.malloc(std::numeric_limits<size_t>::max() - 4));

  // a negative length from a caller reaches here as a huge size_t -- same path
  EXPECT_EXCEPTION(r.malloc(static_cast<size_t>(-1)));

  // and the region is still usable after a refused request
  void* c = r.malloc(64);
  EXPECT_TRUE(c != nullptr);
}

// A page size can be arithmetically valid (it does not wrap the fit check
// above) yet far more than malloc can satisfy -- an untrusted array length such
// as 2^60 reaches newpage as the page size and malloc returns null for it.
// newpage used to store that null base and hand the caller a near-null pointer
// to write through, SIGSEGV'ing the process on one oversized message; it now
// raises a recoverable error instead (STRFR-433966).
TEST(Region, newpageRefusesAnUnsatisfiableAllocation) {
  region r(4096);

#if !HOBBES_TEST_SKIP_UNSATISFIABLE_ALLOC
  // representable (no overflow), but a 1 EiB request exceeds the virtual address
  // space a 64-bit process can map (~256 TiB on x86-64), so malloc fails at the
  // reservation regardless of overcommit settings and newpage refuses it
  EXPECT_EXCEPTION(r.malloc(static_cast<size_t>(1) << 60));
#endif

  // the region is still usable after the failed page allocation
  void* a = r.malloc(64);
  void* b = r.malloc(64);
  EXPECT_TRUE(a != nullptr && b != nullptr && a != b);
}
