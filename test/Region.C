
#include "test.H"
#include <hobbes/util/region.H>
#include <limits>

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
